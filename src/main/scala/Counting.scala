

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

  def findPlaceOfLastPerson(n: Int, k: Int): Int = {
    require(n > 0, "number of people in the circle (n) must be larger than 0")
    require(k > 0, "step rate (k) must be larger than 0")
    (2 to n).foldLeft(0) { case (previousResult, currentSize) => (k + previousResult) % currentSize }
  }

  def main(args: Array[String]) {
    require(args.length == 2, "usage: Counting <number of people in the circle (n)> <step rate (k)>")

    val n = util.Try(args(0).toInt).toOption
    val k = util.Try(args(1).toInt).toOption

    if (n.isEmpty)
      println(s"""${args(0)} is not a valid Int""")
    else if (k.isEmpty)
      println(s"""${args(1)} is not a valid Int""")
    else {
      val placeOfLastPerson = findPlaceOfLastPerson(n.get, k.get)

      println(s"""you need to stand at place $placeOfLastPerson in the circle to be the last person left""")
    }

  }

}

