package typeclass

import org.scalatest._

class `2.2.RecursiveDataType` extends FlatSpec with Matchers {

  behavior of "LinkedList of 2 elements"

  it should "be able to get first" in {
    Pair(1, Pair(2, End()))(0) shouldBe 1
  }

  it should "be able to get second element" in {
    Pair(1, Pair(2, End()))(1) shouldBe 2
  }

  it should "be able to get third element" in {
    Pair(1, Pair(2, Pair(7, End())))(2) shouldBe 7
  }

  it should "throw error Out of Boundary exception if try to get 3rd" in {
    the[Exception] thrownBy Pair(1, Pair(2, End()))(2) should have message "Out of Boundary"
  }

}
