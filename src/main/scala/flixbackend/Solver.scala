package flixbackend

import Declarations._
import scala.collection.mutable

class Solver(rules: List[Rule]) {
  val globalEnv = mutable.Map[GlobalVar, Value]().withDefault(_.tpe.bot)

  def globalsInBody(rule: Rule) = rule.body.map(_.variable).toSet

  val dependences: GlobalVar=>Traversable[Rule] = {
    val pairs: Seq[(GlobalVar, Rule)] = for {
      rule <- rules
      global <- globalsInBody(rule)
    } yield (global, rule)

    pairs.groupBy(_._1).mapValues(_.map(_._2)).withDefaultValue(List.empty)
  }

  val globalVars = rules.map(_.head.variable).toSet

  def apply(): Unit = {
    val worklist = mutable.Set() ++ rules
    val ruleEvals: Rule=>collection.Map[GlobalVar, Value]=>Value = rules.map{
      rule => (rule,
        /* LMS Specialize evaluateRule(rule, _) */
        {(env: collection.Map[GlobalVar, Value]) => RuleEvaluator.evaluateRule(rule, env)})
    }.toMap
    while(!worklist.isEmpty) {
      val rule = worklist.head
      worklist -= rule
      val outputValue: Value =
        /* LMS execute staged evaluateRule(_, globalEnv) */
        ruleEvals(rule).apply(globalEnv)
      if(outputValue != globalEnv(rule.head.variable)) {
        globalEnv(rule.head.variable) = outputValue
        worklist ++= dependences(rule.head.variable)
      }
    }
  }

  def print(): Unit = globalEnv.keys.foreach{key =>
    println(key.show)
    println(key.tpe.show(globalEnv(key)))
    println("=" * 60)
  }
}

