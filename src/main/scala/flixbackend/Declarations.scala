package flixbackend

class Error(msg: String) extends RuntimeException(msg)

object Declarations {
  def errorAtStagingTime(msg: String): Nothing = throw new Error("Error: " + msg)

  /** Types.
    *
    */
  sealed abstract class Type {
    def leq(v1: Value, v2: Value): Boolean
    def meet(v1: Value, v2: Value): Value
    def join(v1: Value, v2: Value): Value
    def bot: Value
    def show(v: Value): String
  }

  case object BooleanType extends Type {
    def leq(v1: Value, v2: Value) = asBoolean(v1) == false || asBoolean(v2) == true
    def meet(v1: Value, v2: Value) = asBoolean(v1) && asBoolean(v2)
    def join(v1: Value, v2: Value) = asBoolean(v1) || asBoolean(v2)
    def bot = Value(false)
    def show(v: Value) = asBoolean(v).toString
  }

  case object IntType extends Type {
    def leq(v1: Value, v2: Value) = errorAtStagingTime("leq not defined for integers")
    def meet(v1: Value, v2: Value) = errorAtStagingTime("meet not defined for integers")
    def join(v1: Value, v2: Value) = errorAtStagingTime("join not defined for integers")
    def bot = errorAtStagingTime("bot not defined for integers")
    def show(v: Value) = asInt(v).toString
  }
  
  case object StringType extends Type {
    def leq(v1: Value, v2: Value) = errorAtStagingTime("leq not defined for strings")
    def meet(v1: Value, v2: Value) = errorAtStagingTime("meet not defined for strings")
    def join(v1: Value, v2: Value) = errorAtStagingTime("join not defined for strings")
    def bot = errorAtStagingTime("bot not defined for strings")
    def show(v: Value) = "\"" + asString(v).toString + "\""
  }

  case class TupleType(elems: Array[Type]) extends Type {
    def map(v1: Value, v2: Value)(f: (Type,Value,Value)=>Value): Array[Value] = {
      val size = elems.size
      val ret = Array.ofDim[Value](size)
      for (i <- 0 until size) {
        ret(i) = f(elems(i), asTupleElem(v1, i), asTupleElem(v2, i))
      }
      ret
    }
    def leq(v1: Value, v2: Value) = map(v1, v2){(t,v1,v2) => Value(t.leq(v1,v2))}.forall{v => asBoolean(v) == true}
    def meet(v1: Value, v2: Value) = TupleValue(map(v1, v2){_.meet(_,_)})
    def join(v1: Value, v2: Value) = TupleValue(map(v1, v2){_.join(_,_)})
    def bot = TupleValue(elems.map(_.bot))
    def show(v: Value) = 
      (for(i <- 0 until elems.size) yield elems(i).show()).mkString("<", ", ", ">")
  }
  
  case class MapType(keyType: Type, valueType: Type) extends Type {
    def leq(v1: Value, v2: Value) = {
      val bot = valueType.bot
      (asMapKeys(v1) ++ asMapKeys(v2)).forall{
        case key => valueType.leq(asMapGetOrElse(v1, key, bot), asMapGetOrElse(v2, key, bot))
      }
    }
    def meet(v1: Value, v2: Value) = {
      val newMap = (asMapKeys(v1) & asMapKeys(v2)).map{
        case key => (key, valueType.meet(asMapGet(v1, key), asMapGet(v2, key)))
      }.toMap
      MapValue(newMap)
    }
    def join(v1: Value, v2: Value) = {
      val bot = valueType.bot
      val newMap = (asMapKeys(v1) ++ asMapKeys(v2)).map{
        case key => (key, valueType.join(asMapGetOrElse(v1, key, bot), asMapGetOrElse(v2, key, bot)))
      }.toMap
      MapValue(newMap)
    }
    def bot = MapValue(Map[Value, Value]())
    def show(map: Value) = {
      def valString(key: Value) = {
        val value = asMapGet(map, key)
        if(asBoolean(value) == true) "" else " -> " + valueType.show(value)
      }
      asMapKeys(map).map{key => keyType.show(key) + valString(key)}.mkString("{", ", ", "}")
    }
  }

  /** Values.
    *
    */
  type Value = Any
  def asBoolean(v: Value) = v.asInstanceOf[Boolean]
  def asInt(v: Value) = v.asInstanceOf[Int]
  def asString(v: Value) = v.asInstanceOf[String]
  def asTupleElem(v: Value, index: Int) = v.asInstanceOf[Array[Value]](index)
  def asMapKeys(map: Value) = map.asInstanceOf[Map[Value, Value]].keySet
  def asMapGet(map: Value, key: Value) = map.asInstanceOf[Map[Value, Value]](key)
  def asMapGetOption(map: Value, key: Value) = map.asInstanceOf[Map[Value, Value]].get(key)
  def asMapGetOrElse(map: Value, key: Value, default: Value) = map.asInstanceOf[Map[Value, Value]].getOrElse(key, default)

  object Value {
    def apply(a: Any): Value = a
  }
  object TupleValue {
    def apply(a: Array[Value]): Value = a
  }
  object MapValue {
    def apply(a: Map[Value, Value]): Value = a
  }

  /*
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
  */

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
  case class TuplePattern(ps: Array[Pattern]) extends Pattern(TupleType(ps.map(_.tpe)))
  case class MapElem(key: Pattern, value: Pattern) extends Pattern(MapType(key.tpe, value.tpe))

  def collectLocalVars(pattern: Pattern): Set[LocalVar] = pattern match {
    case pattern: Constant => Set()
    case pattern: Variable => Set(pattern.l)
    case pattern: TuplePattern => pattern.ps.flatMap(collectLocalVars).toSet
    case pattern: MapElem => collectLocalVars(pattern.key) ++ collectLocalVars(pattern.value)
  }

}
