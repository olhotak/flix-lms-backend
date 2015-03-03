package flixbackend

class Error(msg: String) extends RuntimeException(msg)

object Declarations {
  def error(msg: String): Nothing = throw new Error("Error: " + msg)

  /** Types.
    *
    */
  sealed abstract class Type {
    def leq(v1: Value, v2: Value): Boolean
    def meet(v1: Value, v2: Value): Value
    def join(v1: Value, v2: Value): Value
    def bot: Value
  }

  case object BooleanType extends Type {
    def leq(v1: Value, v2: Value) =
      BooleanValue(false) == v1 || BooleanValue(true) == v2
    def meet(v1: Value, v2: Value) =
      BooleanValue(v1.as[BooleanValue].b && v2.as[BooleanValue].b)
    def join(v1: Value, v2: Value) =
      BooleanValue(v1.as[BooleanValue].b || v2.as[BooleanValue].b)
    def bot = BooleanValue(false)
  }

  case object IntType extends Type {
    def leq(v1: Value, v2: Value) = error("leq not defined for integers")
    def meet(v1: Value, v2: Value) = error("meet not defined for integers")
    def join(v1: Value, v2: Value) = error("join not defined for integers")
    def bot = error("bot not defined for integers")
  }
  
  case object StringType extends Type {
    def leq(v1: Value, v2: Value) = error("leq not defined for strings")
    def meet(v1: Value, v2: Value) = error("meet not defined for strings")
    def join(v1: Value, v2: Value) = error("join not defined for strings")
    def bot = error("bot not defined for strings")
  }

  case class TupleType(elems: List[Type]) extends Type {
    def map[A](v1: Value, v2: Value)(f: (Type,Value,Value)=>A): List[A] =
      (elems zip (v1.as[TupleValue].elems zip v2.as[TupleValue].elems)).map {
        case (tpe, (vv1, vv2)) => f(tpe, vv1, vv2)
      }

    def leq(v1: Value, v2: Value) = map(v1, v2){_.leq(_,_)}.forall{_ == true}
    def meet(v1: Value, v2: Value) = TupleValue(map(v1, v2){_.meet(_,_)})
    def join(v1: Value, v2: Value) = TupleValue(map(v1, v2){_.join(_,_)})
    def bot = TupleValue(elems.map(_.bot))
  }
  case class MapType(keyType: Type, valueType: Type) extends Type {
    def leq(v1: Value, v2: Value) = {
      val vv1 = v1.as[MapValue].map
      val vv2 = v2.as[MapValue].map
      val bot = valueType.bot
      (vv1.keySet ++ vv2.keySet).forall{
        case key => valueType.leq(vv1.getOrElse(key, bot), vv2.getOrElse(key, bot))
      }
    }
    def meet(v1: Value, v2: Value) = {
      val vv1 = v1.as[MapValue].map
      val vv2 = v2.as[MapValue].map
      val newMap = (vv1.keySet & vv2.keySet).map{
        case key => (key, valueType.meet(vv1(key), vv2(key)))
      }.toMap
      MapValue(newMap)
    }
    def join(v1: Value, v2: Value) = {
      val vv1 = v1.as[MapValue].map
      val vv2 = v2.as[MapValue].map
      val bot = valueType.bot
      val newMap = (vv1.keySet ++ vv2.keySet).map{
        case key => (key, valueType.join(vv1.getOrElse(key, bot), vv2.getOrElse(key, bot)))
      }.toMap
      MapValue(newMap)
    }
    def bot = MapValue(Map[Value, Value]())
  }

  /** Values.
    *
    */
  sealed abstract class Value {
    def show: String
    def as[T] = asInstanceOf[T]
  }
  case class BooleanValue(val b: Boolean) extends Value {
    def show = b.toString
  }
  case class IntValue(val i: Int) extends Value {
    def show = i.toString
  }
  case class StringValue(val s: String) extends Value {
    def show = '"' + s.toString + '"'
  }
  case class TupleValue(val elems: List[Value]) extends Value {
    def show = elems.map(_.show).mkString("<", ", ", ">")
  }
  case class MapValue(val map: Map[Value, Value]) extends Value {
    def show = {
      def valString(key: Value) = {
        val value = map(key)
        if(value == BooleanValue(true)) "" else " -> " + value.show
      }
      map.keys.map{key => key.show + valString(key)}.mkString("{", ", ", "}")
    }
  }

  /** Vars.
    *
    */
  case class LocalVar(name: String, tpe: Type) {
    def show = s"$name: $tpe"
  }
  case class GlobalVar(name: String, tpe: Type) {
    def show = s"$name: $tpe"
  }

  /** Rule.
    *
    */
  case class Rule(head: Predicate, body: List[Predicate])

  def collectLocalVars(rule: Rule): Set[LocalVar] = (rule.head :: rule.body).flatMap{collectLocalVars(_)}.toSet

  /** Predicates.
    *
    */
  case class Predicate(pattern: Pattern, variable: GlobalVar)

  def collectLocalVars(pred: Predicate): Set[LocalVar] = collectLocalVars(pred.pattern)

  /** Patterns.
    *
    */
  sealed abstract class Pattern(val tpe: Type)
  case class Constant(v: Value, override val tpe: Type) extends Pattern(tpe)
  case class Variable(l: LocalVar) extends Pattern(l.tpe)
  case class TuplePattern(ps: List[Pattern]) extends Pattern(TupleType(ps.map(_.tpe)))
  case class MapElem(key: Pattern, value: Pattern) extends Pattern(MapType(key.tpe, value.tpe))

  def collectLocalVars(pattern: Pattern): Set[LocalVar] = pattern match {
    case pattern: Constant => Set()
    case pattern: Variable => Set(pattern.l)
    case pattern: TuplePattern => pattern.ps.flatMap(collectLocalVars).toSet
    case pattern: MapElem => collectLocalVars(pattern.key) ++ collectLocalVars(pattern.value)
  }

}
