package excercises

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean) {

  /*
   Exercise: implement a functional set
   */
  def apply(elem: A): Boolean =
    contains(elem)

  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(elem: MySet[A]): MySet[A]

  def map[B](f: A => B): MySet[B]
  def flatMap[B](g: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit

  /*
  Exercise
  -remove an element
  -intersection with another set
  -difference with another set
   */
  def -(elem: A): MySet[A]
  def --(anotherSet:MySet[A]): MySet[A]
  def &(anotherSet: MySet[A]): MySet[A]

  /*
  excercise implement a unaty_! = NEGATION of a set
   */
  def unary_! : MySet[A]
}

class EmptySet[A] extends MySet[A] {
  def contains(elem: A): Boolean = false
  def +(elem: A): MySet[A] = new NonEmptySet[A](elem, this)
  def ++(elem: MySet[A]): MySet[A] = elem

  def map[B](f: A => B): MySet[B] = new EmptySet[B]
  def flatMap[B](g: A => MySet[B]): MySet[B] = new EmptySet[B]
  def filter(predicate: A => Boolean): MySet[A] = this
  def foreach(f: A => Unit): Unit = ()

  //part 2
  def -(elem: A): MySet[A] = this
  def --(anotherSet:MySet[A]): MySet[A] = this
  def &(anotherSet: MySet[A]): MySet[A] = this

  def unary_! : MySet[A] = new PropertyBasedSet[A](_ => true)
}

//all elements of type A which satisfy a property
class PropertyBasedSet[A](property: A => Boolean) extends MySet[A]{
  def contains(elem: A): Boolean = property(elem)
  def +(elem: A): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || x == elem)

  def ++(anotherSet: MySet[A]): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || anotherSet(x))

  def map[B](f: A => B): MySet[B] = politeltFail
  def flatMap[B](g: A => MySet[B]): MySet[B] = politeltFail

  def filter(predicate: A => Boolean): MySet[A] = new PropertyBasedSet[A](x => property(x) || predicate(x))

  def foreach(f: A => Unit): Unit = politeltFail

  def -(elem: A): MySet[A] = filter(x => x != elem)
  def --(anotherSet:MySet[A]): MySet[A] = filter(!anotherSet)
  def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

  def unary_! : MySet[A] = new PropertyBasedSet[A](x => !property(x))

  def politeltFail = throw new IllegalArgumentException("Really deep rabbit hole!")
}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A]{
  def contains(elem: A): Boolean =
    elem.equals(head) || tail.contains(elem)

  def +(elem: A): MySet[A] =
    if(this.contains(elem)) this
    else  new NonEmptySet[A](elem, this)

  def ++(elem: MySet[A]): MySet[A] =
    tail ++ elem + head

  def map[B](f: A => B): MySet[B] = (tail map f) + f(head)
  def flatMap[B](f: A => MySet[B]): MySet[B] = (tail flatMap f) ++ f(head)

  def filter(predicate: A => Boolean): MySet[A] = {
    val filteredTail = tail filter predicate
    if(predicate(head)) filteredTail + head
    else filteredTail
  }

  def foreach(f: A => Unit): Unit = {
    f(head)
    tail foreach f
  }

  //part 2
  def -(elem: A): MySet[A] =
    if (head == elem) tail
    else tail - elem + head

  def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)

  def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

  //new operator
  def unary_! : MySet[A] = new PropertyBasedSet[A](x => !this.contains(x))
}

object MySet{
  def apply[A] (values: A*): MySet[A] = {
    @tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] =
      if(valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)

    buildSet(values.toSeq, new EmptySet[A])
  }
}

object MySetPlayground extends App {
  val s = MySet(1, 2, 3)
  s + 5 ++ MySet(-1, -2) + 3 flatMap (x => MySet(x, 10 * 5)) filter(_ % 2 == 0) foreach println

  val newgative = !s //s.unary_! = all the naturals not equal 1.2.3.4
  println(newgative(2))
  println(newgative(5))

  val negativeeven = newgative.filter(_ % 2 == 0)
  println(negativeeven(5))
}
