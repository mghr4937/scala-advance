package lecture.part1as

import scala.util.Try

object DarkSugars extends App {

  //syntax sugar 1: methods with single param
  def singleArgMethod(arg: Int): String = s"$arg little ducks..."

  val description = singleArgMethod {
    //more code
    22
  }

  println(description)

  val aTryInstance = Try { // java's y{}
    throw new RuntimeException
  }

  List(1, 2, 3).map { x =>
    x + 1
  }

  //syntax sugar 2: single abstract method
  trait Action {
    def act(x: Int): Int
  }

  val anInstance: Action = new Action {
    override def act(x: Int): Int = x + 1
  }

  val aFunkyInstance: Action = (x: Int) => x + 1

  //example: Runnables
  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("Hello scala")
  })

  val aSweeterThread = new Thread(() => println("hello sweet scala"))

  abstract class AnAbstractType {
    def implemented: Int = 23

    def f(a: Int): Unit
  }

  val anAbstractInstance: AnAbstractType = (a:Int) => println("sweet")

  //syntax sugar 3: the :: and #:: methods are special
  val prependedList = 2 :: List(3,4)
  //2.::(List(3,4))
  //List(3,4).::(2)
  //?!

  //scala specifications: last char decides associativity of method
  1 :: 2 :: 3 :: List(3,4)
  List(3,4).::(2).::(1)

  class MyStream[T]{
    def -->:(value: T): MyStream[T] = this //actual implementation here
  }

  val myStream = 1 -->: 2 -->: 3 -->: new MyStream[Int]

  //syntax sugar 4: multi-word method naming
  class TeenGirl(name: String){
    def `and then said`(gossip: String)= println(s"$name said $gossip")
  }

  val lilly = new TeenGirl("Lilly")
  lilly `and then said` "scala is so nice"

  //syntax sugar 5: infix types
  class Composite[A, B]
  val composite: Composite[Int, String] = ???
  val composite2: Int Composite String = ???

  class -->[A, B]
  val towards: Int --> String = ???

  //syntax sugar : update() is very special, much like apply()
  val anArray = Array(1,2,3)
  anArray(2) = 7 //rewriten to anArray.update(2, 7)
  //used in mutable collections
  //remember apply() and update()

  //synatx sugar 7: setters for mutable containers
  class Mutable{
    private var internalMember : Int = 0 //private for OO encapsulation
    def member = internalMember //getter
    def member_= (value: Int): Unit =
      internalMember = value //setter
  }

  val aMutableContainer = new Mutable
  aMutableContainer.member = 42 //rewriten as aMutableContainer.member_=(42)




}
