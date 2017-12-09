import java.io.File

import DayEight.getClass

import scala.collection.mutable
import scala.io.Source

object DayNine extends App {
  var skipNext = false
  var groups: mutable.Queue[Char] = mutable.Queue.empty
  var sum = 0
  var insideGarbage = false
  var canceledSum = 0

  val f = new File(getClass.getClassLoader.getResource("daynine.txt").getPath)
  for (line <- Source.fromFile(f).getLines) {
    line.map{
      ch =>
        if (!skipNext) {
          if ((ch == '<') && !insideGarbage) insideGarbage = true
          else if (ch == '>') insideGarbage = false
          else if ((ch == '{') && !insideGarbage) groups.enqueue('{')
          else if ((ch == '}') && !insideGarbage) {
            sum += groups.size
            groups.dequeue
          }
          else if (ch == '!') skipNext = true
          else if (insideGarbage) canceledSum += 1
        }
        else skipNext = false
    }
  }
  println("Result for part one: " + sum)
  println("Result for part two: " + canceledSum)
}