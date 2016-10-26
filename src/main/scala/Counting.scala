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

  case class Person(position: Int)

  def count(persons: List[Person], step: Int): Option[Int] =
    if (step <= 0) None
    else count(persons, 1, step)

  @tailrec
  def count(persons: List[Person], startIndex: Int, step: Int): Option[Int] =
    persons.size match {
      case 0 => None
      case 1 => Some(persons.head.position)
      case n =>
        val personsWithModularIndexes = assignModularIndex(persons, startIndex, step)
        val nextStartIndex = (personsWithModularIndexes.last._2 + 1) % step
        val survivors = personsWithModularIndexes.filterNot {
          case (person, modularIndex) => modularIndex == 0
        }.unzip._1
        count(survivors, nextStartIndex, step)
    }

  def assignModularIndex(persons: List[Person], startIndex: Int, step: Int): List[(Person, Int)] = {

    @tailrec
    def assign(ps: List[Person], si: Int, result: List[(Person, Int)]): List[(Person, Int)] =
      if (ps.isEmpty)
        result.reverse
      else
        assign(ps.tail, (si + 1) % step, (ps.head, si) :: result)

    assign(persons, startIndex, List.empty)
  }

  def loop(start: Int, end: Int, step: Int): Unit = {
    (start to end) foreach (i => {
      val persons = (1 to i).toList.map(Person(_))
      println(s"""$i : ${count(persons, 1, step)}""")
    })
  }

  def loop(start: Int, end: Int): Unit = {
    (start to end) foreach (i => {
      val persons = (1 to i).toList.map(Person(_))
      println(s"""$i : ${count(persons, 1, i)}""")
    })
  }

  def main(args: Array[String]) {
    loop(2, 100)
  }

}

