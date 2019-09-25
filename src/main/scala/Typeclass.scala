package typeclass

sealed trait TrafficLight {
  def next: TrafficLight = this match {
    case Red    => Green
    case Green  => Yellow
    case Yellow => Red
    case _ =>
      throw new Exception(s"$this is not a valid Traffic Light instance")
  }
}

final case object Red extends TrafficLight
final case object Green extends TrafficLight
final case object Yellow extends TrafficLight

sealed trait LinkedList[A] {
  def apply(index: Int): A = this match {
    case Pair(head, tail) => {
      if (index == 0) head else tail(index - 1)
    }
    case End() => throw new Exception("Out of Boundary")
  }
}

final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]
final case class End[A]() extends LinkedList[A]

sealed trait CoLinkedList[+A] {
  def apply(index: Int): A = ???
}
final case class CoPair[A](head: A, tail: CoLinkedList[A])
    extends CoLinkedList[A]
final case object CoEnd extends CoLinkedList[Nothing]

trait JsonWriter[A] {
  def write(in: A): String
}

object JsonWriter {
  def write[A](input: A)(implicit jsonWriter: JsonWriter[A]) = {
    jsonWriter.write(input)
  }
  implicit class Ops[A](a: A) {
    def writeJson(implicit jsonWriter: JsonWriter[A]): String = {
      jsonWriter.write(a)
    }
  }
}

final case class Person(name: String, email: String)

object Person {
  implicit val jsonWriterForPerson: JsonWriter[Person] =
    new JsonWriter[Person] {
      def write(in: Person): String =
        s"""{"name": "${in.name}", "email": "${in.email}"}"""
    }
  // hint: https://www.scala-lang.org/api/current/scala/math/Ordering$.html#fromLessThan[T](cmp:(T,T)=%3EBoolean):scala.math.Ordering[T]
  implicit val sortablePerson: Ordering[Person] = new Ordering[Person] {
    override def compare(x: Person, y: Person): Int = {
      if (x.name < y.name) 1
      else if (x.name > y.name) -1
      else 0
    }
  }
}

//test

final case class Cat(name: String, food: String)

object Cat {
  implicit val jsonWriterForCat: JsonWriter[Cat] = new JsonWriter[Cat] {
    override def write(in: Cat): String =
      s"""{"name": "${in.name}", "food": "${in.food}"}"""
  }
}

final case class CatPerson(person: Person, cat: Cat)

object CatPerson {
  implicit val jsonWriterForCatPerson: JsonWriter[CatPerson] =
    new JsonWriter[CatPerson] {
      override def write(in: CatPerson): String = {
        s"""{"person":${JsonWriter.write(in.person)},"cat":${JsonWriter.write(
          in.cat)}"""
      }
    }
  implicit class CatPersonOps(cp: CatPerson) {
    def writeJson: String = {
      s"""{"person":${JsonWriter.write(cp.person)},"cat":${JsonWriter.write(
        cp.cat)}"""
    }

  }
}

object CatPersonDemo extends App {
  import JsonWriter.Ops
  println(Person("Philip", "Fry").writeJson)
}
