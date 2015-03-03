package flixbackend

import Declarations._

object RuleEvaluator {
  type Rep[T] = T

  sealed abstract class Binding
  case class LeqBinding(v: Rep[Value]) extends Binding
  case class ExactBinding(v: Rep[Value]) extends Binding

  def evalPattern(pattern: Pattern, valuation: PartialFunction[LocalVar, Rep[Value]]): Option[Rep[Value]] = pattern match {
    case Constant(v, _) => Some(v)
    case Variable(l: LocalVar) => valuation.lift(l)
    case TuplePattern(ps) =>
      val pvals = ps.map(evalPattern(_, valuation)).collect{case Some(v) => v}
      if(pvals.size == ps.size) Some(TupleValue(pvals)) else None
    case MapElem(key, value) =>
      for {
        keyVal <- evalPattern(key, valuation)
        valueVal <- evalPattern(value, valuation)
      } yield MapValue(Map(keyVal -> valueVal))
  }

  def evaluateRule(rule: Rule, inputEnv: collection.Map[GlobalVar, Rep[Value]]): Rep[Value] = {
    case class State(val valuation: Map[LocalVar, Binding], val processors: List[State=>Unit]) {
      def accept(newValuation: Map[LocalVar, Binding]): Unit = {
        val newState = State(newValuation, processors.tail)
        processors.head(newState)
      }
      def accept(): Unit = accept(valuation)
      def liftedValuation: PartialFunction[LocalVar, Rep[Value]] = {
        case l if valuation.isDefinedAt(l) => valuation(l) match {
          case LeqBinding(v) => v
          case ExactBinding(v) => v
        }
      }
    }

    def accept(pred: Predicate)(state: State): Unit =
      acceptLeq(pred.pattern)(inputEnv(pred.variable))(state)

    def acceptLeq(pattern: Pattern)(bound: Rep[Value])(state: State): Unit = {
      pattern match {
        case Constant(v, tpe) => if(tpe.leq(v, bound)) state.accept()
        case Variable(l) =>
          state.valuation.get(l) match {
            case None =>
              state.accept(state.valuation + (l -> LeqBinding(bound)))
            case Some(LeqBinding(existing)) =>
              state.accept(state.valuation + (l -> LeqBinding(l.tpe.meet(bound, existing))))
            case Some(ExactBinding(existing)) =>
              if(l.tpe.leq(existing, bound)) state.accept()
          }
        case TuplePattern(ps) =>
          val elems = bound.asInstanceOf[TupleValue].elems
          val newProcessors: List[State=>Unit] = (ps zip elems).map{ case (p, e) => acceptLeq(p)(e)(_) }
          state.copy(processors = newProcessors ++ state.processors).accept()
        case MapElem(keyPattern, valuePattern) =>
          val mapBound = bound.asInstanceOf[MapValue].map
          val keyVal = evalPattern(keyPattern, state.liftedValuation)
          def processKey(key: Rep[Value]) = {
            mapBound.get(key) match {
              case Some(value) =>
                val newProcessors: List[State=>Unit] =
                  (acceptExact(keyPattern)(key)(_)) ::
                    (acceptLeq(valuePattern)(value)(_)) ::
                    state.processors
                state.copy(processors = newProcessors).accept()
              case None =>
            }
          }
          keyVal match {
            case Some(key) => processKey(key)
            case None =>  mapBound.keys.foreach(processKey(_))
          }
      }
    }

    def acceptExact(pattern: Pattern)(other: Rep[Value])(state: State): Unit = {
      pattern match {
        case Constant(v, _) => if(v == other) state.accept()
        case Variable(l) =>
          state.valuation.get(l) match {
            case None =>
              state.accept(state.valuation + (l -> ExactBinding(other)))
            case Some(LeqBinding(existing)) =>
              if(l.tpe.leq(other, existing)) state.accept(state.valuation + (l -> ExactBinding(other)))
            case Some(ExactBinding(existing)) =>
              if(existing == other) state.accept()
          }
        case TuplePattern(ps) =>
          val elems = other.asInstanceOf[TupleValue].elems
          val newProcessors: List[State=>Unit] = (ps zip elems).map{ case (p, e) => acceptExact(p)(e)(_) }
          state.copy(processors = newProcessors ++ state.processors).accept()
        case MapElem(keyPattern, valuePattern) =>
          val mapBound = other.asInstanceOf[MapValue].map
          if(mapBound.size == 1) mapBound.keys.foreach{key =>
            val newProcessors: List[State=>Unit] =
              (acceptExact(keyPattern)(key)(_)) ::
                (acceptExact(valuePattern)(mapBound(key))(_)) ::
                state.processors
            state.copy(processors = newProcessors).accept()
          }
      }
    }

    var outputValue: Rep[Value] = inputEnv(rule.head.variable)

    def updateOutputValue(state: State): Unit = {
      evalPattern(rule.head.pattern, state.liftedValuation) match {
        case None => error(s"rule fails to bind all variables used in head ${rule}")
        case Some(newValue) =>
          outputValue = rule.head.variable.tpe.join(outputValue, newValue)
      }
    }

    val predAccepts: List[State=>Unit] = rule.body.map(pred => accept(pred)(_))
    val outputAccept: State=>Unit = updateOutputValue(_)
    State(Map(), predAccepts :+ outputAccept).accept()

    outputValue
  }
}

