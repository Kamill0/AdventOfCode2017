import java.io.File

import scala.collection.mutable
import scala.io.Source

import util.control.Breaks._

/**
  * Created by kamil_000 on 2017-12-07.
  */
object DaySeven extends App {
  val nodes: scala.collection.mutable.Map[String, (Int, List[String])] = scala.collection.mutable.Map.empty
  var head_key = ""

  val f = new File(getClass.getClassLoader.getResource("dayseven.txt").getPath)
  for (line <- Source.fromFile(f).getLines) {
    val values = line.split("->").toList
    val tmp = values.head.split(" ").toList
    val key = tmp.head
    val weight = """\(.*\)""".r.findFirstIn(tmp.last).get.drop(1).dropRight(1).toInt
    if (values.length == 1) {
      nodes(key) = (weight, List.empty)
    } else {
      nodes(key) = (weight, values.last.split(",").toList.map(_.trim))
    }
  }

  var isHead = true

  nodes.keys.filter{ f =>
      nodes.get(f).get._2.length > 0
  }.map{
    key =>
      isHead = true
      nodes.map{
        case (k,v)
          =>
            if (v._2.contains(key)) {
              isHead = false
            }
      }
     if (isHead) {
       println("Result is: " + key)
       head_key = key
     }
  }

  def calc_sum(node: String): Int = {
    val node_details = nodes.get(node).get
    val children = node_details._2
    var flag: Boolean = false
    if (children.length > 0) {
      val que: mutable.Queue[Int] = mutable.Queue.empty
      children.foreach {
        child
          => que.enqueue(calc_sum(child))
      }
      val groupBy = que.toSeq.groupBy(identity).mapValues(_.size)
      groupBy.foreach{
        case (k,v)
          =>
            if (v == 1) {
              flag = true
            }
      }
      if (flag) {
        println("Childs: " + groupBy)
        println("Parent node: " + node_details._1)
        println((nodes.get(children(que.indexOf(que.max))).get._1 - (groupBy.keys.max - groupBy.keys.min)))
      }
      que.sum + node_details._1
    } else node_details._1
  }

  calc_sum(head_key)
}
