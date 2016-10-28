import org.specs2.Specification
import org.specs2.specification.core.SpecStructure

class CountingSpec extends Specification {
  override def is: SpecStructure =
    s2"""
 This is a specification to check my solution to the 'Counting' problem

 The 'Counting' problem should
  return Some(3) when n = 3 and k = 2                     $e3
  return Some(1) when n is a power of 2 and k = 2         $e4
  return Some(1 + 2*(n - 2^(floor(log2(n)))) when k = 2   $e5
      """

  def e3 = Counting.findPlaceOfLastPerson(3, 2) mustEqual 2

  def e4 = Counting.findPlaceOfLastPerson(1024, 2) mustEqual 0

  def e5 = {
    val n = 4321
    val expected = 2 * (n - Math.pow(2, Math.floor(Math.log(n) / Math.log(2))))
    Counting.findPlaceOfLastPerson(n, 2) mustEqual expected.toInt
  }

}
