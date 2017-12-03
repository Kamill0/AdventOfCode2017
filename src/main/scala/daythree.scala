import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks
import util.control.Breaks._

object DayThree extends App{
  var current_pos: ((Int, Int), Int) = ((1,0), 1)
  var cellValue: Int = 0

  var steps: scala.collection.mutable.Map[(Int, Int), Int] = scala.collection.mutable.Map((0,0) -> 1, (1, 0) -> 1)

  def tryGoingRight(i: Int, j: Int): Boolean = if (!steps.contains((i,j)) && steps.contains(i-1, j-1)) true else false
  def tryGoingTop(i: Int, j: Int): Boolean = if (!steps.contains((i,j)) && steps.contains(i-1, j+1)) true else false
  def tryGoingLeft(i: Int, j: Int): Boolean = if (!steps.contains((i,j)) && steps.contains(i+1, j+1)) true else false
  def tryGoingBottom(i: Int, j: Int): Boolean = if (!steps.contains((i,j)) && steps.contains(i+1, j-1)) true else false

  def getCellValue(i: Int, j: Int): Int = {
    val list = new ListBuffer[Int]()
    if (steps.contains((i+1, j))) list += steps.get((i+1, j)).get
    if (steps.contains((i+1, j-1))) list += steps.get((i+1, j-1)).get
    if (steps.contains((i, j-1))) list += steps.get((i, j-1)).get
    if (steps.contains((i-1, j-1))) list += steps.get((i-1, j-1)).get
    if (steps.contains((i-1, j))) list += steps.get((i-1,j)).get
    if (steps.contains((i-1, j+1))) list += steps.get((i-1, j+1)).get
    if (steps.contains((i, j+1))) list += steps.get((i, j+1)).get
    if (steps.contains((i+1, j+1))) list += steps.get((i+1, j+1)).get
    list.sum
  }

  val puzzle_input = 361527
  val Inner = new Breaks
  val Outer = new Breaks
  Outer.breakable {
    (3 to 1000).foreach {
      iter =>
        Inner.breakable {
          if (tryGoingRight(current_pos._1._1 + 1, current_pos._1._2)) {
            val posX = current_pos._1._1 + 1
            val posY = current_pos._1._2
            cellValue = getCellValue(posX, posY)
            steps += ((posX, posY) -> cellValue)
            current_pos = ((posX, posY), cellValue)
            Inner.break
          } else if (tryGoingTop(current_pos._1._1, current_pos._1._2 - 1)) {
            val posX = current_pos._1._1
            val posY = current_pos._1._2 - 1
            cellValue = getCellValue(posX, posY)
            steps += ((posX, posY) -> cellValue)
            current_pos = ((posX, posY), cellValue)
            Inner.break
          } else if (tryGoingLeft(current_pos._1._1 - 1, current_pos._1._2)) {
            val posX = current_pos._1._1 - 1
            val posY = current_pos._1._2
            cellValue = getCellValue(posX, posY)
            steps += ((posX, posY) -> cellValue)
            current_pos = ((posX, posY), cellValue)
            Inner.break
          } else if (tryGoingBottom(current_pos._1._1, current_pos._1._2 + 1)) {
            val posX = current_pos._1._1
            val posY = current_pos._1._2 + 1
            cellValue = getCellValue(posX, posY)
            steps += ((posX, posY) -> cellValue)
            current_pos = ((posX, posY), cellValue)
            Inner.break
          } else println("Thats not right, abort")
        }
        if (cellValue > puzzle_input) Outer.break
    }
  }

  println("After last iteration: " + current_pos)
  //println("The answer is: " + (math.abs(current_pos._1._1) + math.abs(current_pos._1._2)))
  println("The answer is: " + cellValue)

}