package lecture.part1as

object AdvancePatternMatching extends App {

  val numbers = List(1)
  val description: Unit = numbers match {
    case head :: Nil => println(s"the only element is $head")
    case _ =>
  }

  /*
  -constants
  -wildcards
  -case classes
  - tuples
  -some special magik live above
   */

  class Person(val name: String, val age: Int)

  object PersonPattern {
    def unapply(person: Person): Option[(String, Int)] = {
      if (person.age < 21) None
      else Some((person.name, person.age))
    }

    def unapply(age: Int): Option[String] =
      Some(if (age < 18) "minor" else "major")
  }

  val bob = new Person("Bob", 25)
  val greeting = bob match {
    case PersonPattern(n, a) => s"Hi, my name is $n, and my age $a old"
  }

  println(greeting)

  val legalStatus = bob.age match {
    case PersonPattern(status) => s"My legal status is $status"
  }

  println(legalStatus)

  object even {
    def unapply(arg: Int): Boolean = arg % 2 == 0
  }

  object singleDigit{
    def unapply(arg: Int): Boolean = arg > -10 && arg < 10
  }

  val n: Int = 12
  val matchProperty = n match {
    case singleDigit() => "single digit"
    case even() => "an even number"
    case _ => "no property"
  }

  println(matchProperty)
}
