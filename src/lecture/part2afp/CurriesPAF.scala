package lecture.part2afp

object CurriesPAF extends App {

  //curried functions
  val superAdder: Int => Int => Int =
    x => y => x + y

  val add3 = superAdder(3)
  println(add3(4))
  println(superAdder(2)(5)) //curried functions

  def curriedAdder(x: Int)(y: Int): Int = x + y

  val add4: Int => Int = curriedAdder(4)

  //lifting = ETA-EXPANSION
  //funtions != methods (JVM limitation)
  def inc(x: Int) = x + 1

  List(1, 2, 3, 4).map(x => inc(x))

  //partial functions applications
  val add5 = curriedAdder(5) _ // Int => Int


  //Excercise
  val simpleAddFunction = (x: Int, y: Int) => x + y

  def simpleAddMethod(x: Int, y: Int) = x + y

  def curriedAddMethod(x: Int)(y: Int) = x + y

  val add7 = (x: Int) => simpleAddFunction(7, x)
  val add7_2 = simpleAddFunction.curried(7)
  val add7_6 = simpleAddFunction(7, _: Int)

  val add7_3 = curriedAddMethod(7) _
  val add7_4 = curriedAddMethod(7)(_)

  val add7_5 = simpleAddMethod(7, _: Int) //alternate syntax for turning methods into functions values
  // y => simpleMethod(7, y)

  //underscore
  def concatenator(a: String, b: String, c: String) = a + b + c

  val insertName = concatenator("Hello, I'm ", _: String, " how are you?")
  println(insertName("Roberto"))

  // EXERCISES
  /*
    1.  Process a list of numbers and return their string representations with different formats
        Use the %4.2f, %8.6f and %14.12f with a curried formatter function.
   */
  def curriedFormatter(s: String)(number: Double): String = s.format(number)

  val numbers = List(Math.PI, Math.E, 1, 9.8, 1.3e-12)

  val simpleFormat = curriedFormatter("%4.2f") _ // lift
  val seriousFormat = curriedFormatter("%8.6f") _
  val preciseFormat = curriedFormatter("%14.12f") _

  println(numbers.map(curriedFormatter("%14.12f"))) // compiler does sweet eta-expansion for us


  /*
   2.  difference between
       - functions vs methods
       - parameters: by-name vs 0-lambda
  */
  def byName(n: => Int) = n + 1

  def byFunction(f: () => Int) = f() + 1

  def method: Int = 42

  def parenMethod(): Int = 42

  /*
    calling byName and byFunction
    - int
    - method
    - parenMethod
    - lambda
    - PAF
   */
  byName(23) // ok
  byName(method) // ok
  byName(parenMethod())
  byName(parenMethod) // ok but beware ==> byName(parenMethod())
  //  byName(() => 42) // not ok
  byName((() => 42) ()) // ok
  //  byName(parenMethod _) // not ok

  //  byFunction(45) // not ok
  //  byFunction(method) // not ok!!!!!! does not do ETA-expansion!
  byFunction(parenMethod) // compiler does ETA-expansion
  byFunction(() => 46) // works
  byFunction(parenMethod _) // also works, but warning- unnecessary

}
