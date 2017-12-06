import scala.collection.mutable.ListBuffer
import util.control.Breaks._


object DaySix extends App{
  //var banks_current_state = List(4,1,15,12,0,9,9,5,5,8,7,3,14,5,12,3)
  var banks_current_state = List(0, 14, 13, 12, 11, 10, 8, 8, 6, 6, 5, 3, 3, 2, 1, 10)

  val len = banks_current_state.length
  val banks: scala.collection.mutable.Map[List[Int], Int] =
    scala.collection.mutable.Map(banks_current_state -> banks_current_state.indexOf(banks_current_state.max))

  def calcIndex(indexOfMax: Int, jumpBy: Int) = if (indexOfMax + jumpBy < len) indexOfMax + jumpBy else (indexOfMax + jumpBy) % len

  breakable {
    while (true) {
      val steps = banks.get(banks_current_state).get
      val max = banks_current_state.max
      val banks_new_state: ListBuffer[Int] = banks_current_state.to[ListBuffer]
      banks_new_state(steps) = 0
      (1 to max).foreach {
        iter =>
           banks_new_state(calcIndex(steps, iter)) += 1
      }

      banks_current_state = banks_new_state.toList

      if (banks.contains(banks_current_state)) {
        println ("Result is: " + banks.size)
        println("State is: " + banks_current_state)
        break
      } else {
        banks(banks_current_state) = banks_current_state.indexOf(banks_current_state.max)
      }
    }
  }
}
