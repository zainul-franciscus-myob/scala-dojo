package classandobject

// * Companion object
class Person(val firstName: String, val lastName: String)

object Person {
  def apply(name: String): Either[String, Person] = {
    val names: Array[String] = name.split(" ")
    names match {
      case Array(firstName, lastName) =>
        Right(new Person(firstName, lastName))
      case _ => Left("Could create a person")
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
