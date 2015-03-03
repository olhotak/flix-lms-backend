import flixbackend.Declarations._
import flixbackend.Solver

object Test1 {
  def main(args: Array[String]): Unit = {
    val globalVar = GlobalVar("R", TupleType(List(BooleanType, BooleanType, BooleanType)))
    def localVar(name: String) = Variable(LocalVar(name, BooleanType))
    val rules = List(
      Rule(
        Predicate(
          TuplePattern(List(
            Constant(BooleanValue(true), BooleanType),
            Constant(BooleanValue(false), BooleanType),
            Constant(BooleanValue(false), BooleanType)
          )),
          globalVar
        ),
        List()
      ),
      Rule(
        Predicate(
          TuplePattern(List(
            localVar("a"),
            localVar("b"),
            localVar("c")
          )),
          globalVar
        ),
        List(
          Predicate(
            TuplePattern(List(
              localVar("b"),
              localVar("a"),
              localVar("c")
            )),
            globalVar
          )
        )
      )
    )

    val solver = new Solver(rules)
    solver.apply()
    solver.print()
  }
}
