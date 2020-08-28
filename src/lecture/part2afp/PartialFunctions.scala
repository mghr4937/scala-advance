package lecture.part2afp

object PartialFunctions extends App {

  val aFunction = (x: Int) => x + 1

  val aFussyFunction = (x: Int) =>
    if (x == 1) 45
    else if (x == 2) 55
    else if (x == 3) 66
    else throw new FunctionNotApplicableException

  class FunctionNotApplicableException extends RuntimeException

  val aNiceFussyFunction = (x: Int) => x match {
    case 1 => 45
    case 2 => 55
    case 3 => 66
  }
  //{1,2,3} => Int

  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 45
    case 2 => 55
    case 3 => 66
  } // partial function value (equivalent with above function)

  println(aPartialFunction(2))

  //PF utilities
  println(aPartialFunction.isDefinedAt(5))

  //lift
  val lifted = aPartialFunction.lift //total function Int => Option[Int]

  println(lifted(2))
  println(lifted(66))

  val pfChain = aPartialFunction.orElse[Int, Int] {
    case 45 => 88
  }

  println(pfChain(45))

  // PF extend normal function
  val aTotalFunction: Int => Int = {
    case 1 => 99
  }

  //HOF accept partial function as well
  val aMappedList = List(1, 2, 3).map {
    case 1 => 68
    case 2 => 33
    case 3 => 89
  }

  println(aMappedList)

  /*
    Note: PF can only have ONE parameter type
   */

  /**
   * excercises
   *
   * 1 - new partial function (anonimous class)
   * 2 - chat bot as PF
   */

    val pFunction = new PartialFunction[Int, Int]  {
      override def apply(x: Int): Int = x match {
        case 1 => 45
        case 2 => 55
        case 3 => 66
      }

      override def isDefinedAt(x: Int): Boolean =
        x == 1 || x == 2 || x == 3
    }

  val aChatBot : PartialFunction[String, String] = {
    case "hello" => "Hi there"
    case "bye" => "goooodbye"
    case "storm" => "thunder"
    case _ => "whaaat!"
  }

//  scala.io.Source.stdin.getLines().foreach(line => println("chatbot: " + aChatBot(line)))

  scala.io.Source.stdin.getLines().map(aChatBot).foreach(println)
}
