import Counting.Person
import org.specs2.Specification
import org.specs2.specification.core.SpecStructure

class CountingSpec extends Specification {
  override def is: SpecStructure =
    s2"""
 This is a specification to check my solution to the 'Counting' problem

 The 'Counting' problem should
  return None when step is equal to 0                     $e1
  return None when step is negative                       $e2
  return Some(3) when n = 3 and k = 2                     $e3
  return Some(1) when n is a power of 2 and k = 2         $e4
  return Some(1 + 2*(n - 2^(floor(log2(n)))) when k = 2   $e5
      """

  def e1 = Counting.findPlaceOfLastPerson(List.empty, 0) mustEqual None

  def e2 = Counting.findPlaceOfLastPerson(List.empty, -1) mustEqual None

  def e3 = Counting.findPlaceOfLastPerson(persons(3), 2) mustEqual Some(3)

  def e4 = Counting.findPlaceOfLastPerson(persons(1024 * 1024), 2) mustEqual Some(1)

  def e5 = {
    val n = 1234567
    val expected = 1 + 2 * (n - Math.pow(2, Math.floor(Math.log(n) / Math.log(2))))
    Counting.findPlaceOfLastPerson(persons(n), 2) mustEqual Some(expected)
  }

  def persons(size: Int): List[Person] =
    (1 to size).toList.map(Person(_))

}
