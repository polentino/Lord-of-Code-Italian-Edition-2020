package it.polentino911.locie.geologicalSorting

import java.io._
import java.math._
import java.security._
import java.text._
import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._
import scala.collection.immutable._
import scala.collection.mutable._
import scala.collection.concurrent._
import scala.concurrent._
import scala.io._
import scala.math._
import scala.sys._
import scala.util.matching._
import scala.reflect._



object Result {

  /*
   * Complete the 'sortIntersect' function below.
   *
   * The function is expected to return an INTEGER_ARRAY.
   * The function accepts following parameters:
   *  1. INTEGER_ARRAY volcanic
   *  2. INTEGER_ARRAY nonVolcanic
   */

  def sortIntersect(volcanic: Array[Int], nonVolcanic: Array[Int]): Array[Int] = {
    // (1) count how many occurrences are there, for each number
    val v = volcanic groupBy(x => x) map { case (k,v) => (k, v.length) }
    val nv = nonVolcanic groupBy(x => x) map { case (k,v) => (k, v.length) }
    // (2) found the occurrences that are shared between the two groups, and sort it
    val sharedKeys = (v.keys.toList intersect nv.keys.toList).sortWith(_ > _).toArray
    // (3) traverse the keys
    sharedKeys flatMap (key => {
      // (4) find the shortest of the two initial arrays
      val min = math.min(v(key), nv(key))
      // (5) produce the array of the correct length, with 'key' content
      Array.tabulate(min)(_ => key)
    })
  }

}

object Solution {
  def main(args: Array[String]) {
    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val volcanicCount = StdIn.readLine.trim.toInt

    val volcanic = Array.ofDim[Int](volcanicCount)

    for (i <- 0 until volcanicCount) {
      val volcanicItem = StdIn.readLine.trim.toInt
      volcanic(i) = volcanicItem
    }

    val nonVolcanicCount = StdIn.readLine.trim.toInt

    val nonVolcanic = Array.ofDim[Int](nonVolcanicCount)

    for (i <- 0 until nonVolcanicCount) {
      val nonVolcanicItem = StdIn.readLine.trim.toInt
      nonVolcanic(i) = nonVolcanicItem
    }

    val result = Result.sortIntersect(volcanic, nonVolcanic)

    printWriter.println(result.mkString("\n"))

    printWriter.close()
  }
}
