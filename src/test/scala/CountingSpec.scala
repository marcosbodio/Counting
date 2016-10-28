import org.specs2.Specification

class CountingSpec extends Specification {
  def is =
    s2"""
This is a specification to check my solution to the 'Counting' problem

The 'Counting' problem should
  return 2 when n = 3 and k = 2                       $e1
  return 0 when n is a power of 2 and k = 2           $e2
  return 2*(n - 2^(floor(log2(n)))) when k = 2        $e3
  not break for large values of n                     $e4
  throw IllegalArgumentException when n is negative   $e5
  throw IllegalArgumentException when k is negative   $e6
  return 3 when n = 7 and k = 3                       $e7
                                                      """

  def e1 = Counting.findPlaceOfLastPerson(3, 2) mustEqual 2

  def e2 = Counting.findPlaceOfLastPerson(1024, 2) mustEqual 0

  def e3 = {
    val n = 4321
    val expected = 2 * (n - Math.pow(2, Math.floor(Math.log(n) / Math.log(2))))
    Counting.findPlaceOfLastPerson(n, 2) mustEqual expected.toInt
  }

  def e4 = Counting.findPlaceOfLastPerson(Int.MaxValue, 123) must beBetween(0, Int.MaxValue)

  def e5 = Counting.findPlaceOfLastPerson(-5, 3) must throwA[IllegalArgumentException]

  def e6 = Counting.findPlaceOfLastPerson(10, -3) must throwA[IllegalArgumentException]

  def e7 = Counting.findPlaceOfLastPerson(7, 3) mustEqual 3

}
