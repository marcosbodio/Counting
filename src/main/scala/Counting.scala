import scala.annotation.tailrec

object Counting {

  /*
  implement code that solves the a counting out problem.
  In these problems, you are in a circle.  From some starting point, X you count k people.
  If you are standing in that kth spot, you are ‘out’.
  The counting continues until there is one person standing.
  A concrete, but morbid, example of this is the Josephus Problem
  (I).

  For this exercise:

  Create an application that takes in 2 parameters: the number of people in the circle (n)
  and the step rate (k).  For example, if k is the step rate, then you skip k-1 people and
  eliminate the kth person.  The output of the program should be the place you need to stand
  in the circle to be the last person left.

  Make sure your tests cover the following scenarios

  Invalid parameters
  What happens if n = k?
  What happens if n = 3 and k = 2?
  What happens if n is very large, but k =2?
  Choose a couple of other cases to test
   */

  def findPlaceOfLastPerson(n: Int, k: Int): Int =
    n match {
      case 1 => 0
      case _ => (k + findPlaceOfLastPerson(n - 1, k)) % n
    }

  def main(args: Array[String]) {
    require(args.length == 2, "usage: Counting <number of people in the circle (n)> <step rate (k)>")

    val n = util.Try(args(0).toInt).getOrElse(-1)
    val k = util.Try(args(1).toInt).getOrElse(-1)

    require(n > 0, "number of people in the circle (n) must be larger than 0")
    require(k > 0, "step rate (k) must be larger than 0")

    val placeOfLastPerson = findPlaceOfLastPerson(n, k)

    println(s"""you need to stand at place $placeOfLastPerson in the circle to be the last person left""")
  }

}

