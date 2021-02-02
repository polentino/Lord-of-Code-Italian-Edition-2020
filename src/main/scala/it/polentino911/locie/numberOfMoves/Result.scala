package it.polentino911.locie.numberOfMoves

import java.io._
import java.math._
import java.security._
import java.text._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._
import scala.collection.immutable._
import scala.concurrent._
import scala.io._
import scala.math._
import scala.sys._
import scala.util.matching._
import scala.reflect._



object Result {

  /*
   * Complete the 'minMoves' function below.
   *
   * The function is expected to return an INTEGER.
   * The function accepts following parameters:
   *  1. INTEGER n
   *  2. INTEGER startRow
   *  3. INTEGER startCol
   *  4. INTEGER endRow
   *  5. INTEGER endCol
   */

  def minMoves(n: Int, startRow: Int, startCol: Int, endRow: Int, endCol: Int): Int = {
    println(s"n=$n, startRow=$startRow, startCol=$startCol, endRow=$endRow, endCol=$endCol")
    // (1) setup graph, from/to vertices
    val from = V(startRow, startCol)
    val to = V(endRow, endCol)
    val g = buildGraph(n, from, to)
    var min = -1

    def traverse(visitedVertices: List[V]): Unit = {
      // (0) no need to visit a branch that would produce a longer moveset
      if(min != -1 && visitedVertices.length >= min) {
        return
      }
      // (1) get next vertices from the last one we visited, order by min distance, and consider the lowest ones
      val nextVertices = g(visitedVertices.last)
        .sortBy(distance(_, to)) match {
        case nvs if distance(nvs.head,to) > 3 => List(nvs.head)
        case nvs => nvs
      }
      // (2) check whether we do have the end vertex already
      nextVertices.find(_ == to) match {
        // (3) match found => update min value
        case Some(_) => min = if(min == -1) visitedVertices.length else math.min(min, visitedVertices.length)
        // (4) still nothing: recurse for every nextVertices, but:
        case _ =>
          nextVertices.foreach(nv => {
            if(visitedVertices.contains(nv)) {
              // (4a) loop detected => give up this branch
              -1
            } else {
              // (4b) now recurse
              traverse(visitedVertices :+ nv)
            }
          })
      }
    }

    // let's kick the process
    traverse(List(from))
    min
  }

  private def distance(from: V, to: V): Double = {
    math.sqrt(math.pow(from.x - to.x, 2) + math.pow(from.y - to.y, 2))
  }

  def buildGraph(n: Int, from: V, to: V): Map[V, List[V]] = {
    val f = Frame(from, to)
    (for {
      i <- 0 until n
      j <- 0 until n
    } yield {
      // produce all 8 possible moves, then keep only the ones with coordinates (x,y) in [0, n) range
      val moves = List(
        V(i-1, j-2),
        V(i-2, j-1),
        V(i-2, j+1),
        V(i-1, j+2),
        V(i+1, j+2),
        V(i+2, j+1),
        V(i+2, j-1),
        V(i+1, j-2)
      ).filter({
        case V(x,y) => 0 <= x && x < n && 0 <= y && y < n
      })
        // do not consider points way too off the [from,to] rectangle
        .filter(f.isValid)
      V(i, j) -> moves
    }).toMap
  }

  case class V(x: Int, y: Int)

  case class Frame(v1: V, v2:V) {
    private val pMin = V(math.min(v1.x, v2.x) - 2, math.min(v1.y, v2.y) - 2)
    private val pMax = V(math.max(v1.x, v2.x) + 2, math.max(v1.y, v2.y) + 2)

    println(s"pMin=$pMin pMax=$pMax")
    def isValid(v: V): Boolean = {
      v.x >= pMin.x && v.x <= pMax.x && v.y >= pMin.y && v.y <= pMax.y
    }
  }
}

object Solution {
  def main(args: Array[String]) {
    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val n = StdIn.readLine.trim.toInt

    val startRow = StdIn.readLine.trim.toInt

    val startCol = StdIn.readLine.trim.toInt

    val endRow = StdIn.readLine.trim.toInt

    val endCol = StdIn.readLine.trim.toInt

    val result = Result.minMoves(n, startRow, startCol, endRow, endCol)

    printWriter.println(result)

    printWriter.close()
  }
}

