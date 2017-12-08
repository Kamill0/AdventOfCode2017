import java.io.File

import scala.collection.mutable
import scala.io.Source

/**
  * Created by kamil_potoczny on 08.12.17.
  */
object DayEight extends App {
  var values: scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map.empty
  var maxByIteration: mutable.Queue[Int] = mutable.Queue.empty

  val f = new File(getClass.getClassLoader.getResource("dayeight.txt").getPath)
  for (line <- Source.fromFile(f).getLines) {
    val parts = line.split(" ").toList
    val variable_calc = parts(0)
    val variable_cond = parts(4)
    val condition = values.get(variable_cond) match {
      case None => {
        values.put(variable_cond, 0)
        0
      }
      case Some(v) => v
    }

    if (evalIf(condition.toInt, parts(5), parts(6).toInt)) {
      values.get(variable_calc) match {
        case None => values.put(variable_calc, incOrDec(0, parts(1), parts(2).toInt))
        case Some(v) => values.put(variable_calc, incOrDec(v, parts(1), parts(2).toInt))
      }

    }

    maxByIteration.enqueue(values.values.max)

    def incOrDec(value: Int, operator: String, modifyBy: Int): Int = {
      operator match {
        case "inc" => value + modifyBy
        case "dec" => value - modifyBy
      }
    }

    def evalIf(val1: Int, operator: String, val2: Int) = {
      operator match {
        case "==" => val1 == val2
        case "!=" => val1 != val2
        case ">" => val1 > val2
        case "<" => val1 < val2
        case ">=" => val1 >= val2
        case "<=" => val1 <= val2
        case _ => sys.error("Unknown operator, aborting ..")
      }
    }
  }


  println("Result for part one is: " + values.values.max)

  println("Result for part two is: " + maxByIteration.max)

}
