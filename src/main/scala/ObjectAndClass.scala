package classandobject

// * Companion object
class Person(val firstName: String, val lastName: String) {}

object Person {
  def apply(name: String): Person = {
    val names: Array[String] = name.split(" ")
    names match {
      case Array(firstName,lastName) =>
        new Person(firstName, lastName)
      case _ =>
        throw new Exception(
          s"names can only have 2 names, but $names has more than 2 names")
    }

  }
}

// * Pattern Matching
case class Cat(color: String, food: String)

object ChipShop {
  def willServe(c: Cat): Boolean = {
    c.food == "Chips"
  }
}
