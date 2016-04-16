
// Exercises

/** Things to discuss:
  * Anything marked !!!
  * I've come around to the idea of inner "loop" functions. Allows you to not pass the same variables that don't change (eg. initial ones)
  * Feels like if I were to write this in a text file instead, I'd get frustrated by all the errors / quirks with syntax and the style where we don't declare most types
  * Was a bit strange that most of these functions are one-liners. Seems like if I wrote code like this, it might be hard to follow as you compose more functions
  */
object Chapter3 {
  def main(args: Array[String]) = {
    println(s"For exercise 1, I guessed 3. Answer was: ${exercise1()}")
    println(s"Exercise 8: ${exercise8()}")
    println(s"Exercise 9: ${length(List(1,2,3,4,5))} should be 5")
    println(s"Exercise 12: ${reverse(List(1,2,3,4))} should be ${List(4,3,2,1)}")
    println(s"Exercise 13: ${foldLeftAsRight(List(1,2,3), Nil: List[Int])((acc, h) => Cons(h+1, acc))}} should be ${List(4,3,2)}")
    println(s"Exercise 13: ${foldRightAsLeft(List(1,2,3), Nil: List[Int])((h, acc) => Cons(h+1, acc))} should be ${List(2,3,4)}")
    println(s"Exercise 14: ${append(List(1,2,3), List(4,5))} should be ${List(1,2,3,4,5)}")
    println(s"Exercise 15: ${flatten(List(List(1, 2), List(3, 4, 5), List(6)))} should be ${List(1,2,3,4,5,6)}")
  }

  def exercise1(): Int =
    List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }

  def tail[A](l: List[A]): List[A] =
    l match {
      case Cons(h, t) => t
    }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0)
      l
    else
      l match {
        case Cons(h, t) => drop(t, n - 1)
      }

  // !!! Second param list is a bit awkward. But nothing to be done if we want type inference.
  // Can Intellij maybe do inference better so we don't need it?
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) => if (f(h)) dropWhile(t)(f) else l
    }

  def setHead[A](l: List[A], newHead: A): List[A] =
    l match {
      case Cons(h, t) => Cons(newHead, t)
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  // Exercise 7. No you can't short circuit since you always call fold on the tail, meaning you iterate through the entire list.

  // Exercise 8. I think:
  // This is doing the same thing as apply. In apply's case the initial value is Nil and the function is Cons(_,_)
  def exercise8() =
    foldRight(List(1,2,3), Nil: List[Int])(Cons(_,_)) // !!! Seems odd that I have to give the type of Nil here.

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((h, acc) => acc + 1)

  // !!! Interesting that they don't tell you that foldLeft does work left-to-right and leave you to read docs or infer it
  // Seems like if you had to figure out this property, later questions are much harder (eg. reverse the list)
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def leftSum(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def leftProduct(l: List[Double]): Double =
    foldLeft(l, 1.0)(_ * _)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, h) => Cons(h,acc)) // !!! No "case" needed here!

  // !!! Since all our functions are pure, append is the same as if I wrote its body here
  def foldLeftAsRight[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    val reversed: List[A] = foldRight(l, Nil: List[A])((a,b) => append(b, List(a))) // !!! Can this be improved?
    foldRight(reversed, z)((h, acc) => f(acc, h))
  }

  def foldRightAsLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B = {
    val reversed: List[A] = foldLeft(l, Nil: List[A])((acc, h) => Cons(h, acc))
    foldLeft(reversed, z)((acc, h) => f(h, acc))
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_,_))

  def flatten[A](ls: List[List[A]]): List[A] = {
    foldRight(ls, Nil: List[A])((l, acc) => foldRight(l, acc)(Cons(_,_)))
  }

  // Copied from chapter 3
  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
}

// Code from Chapter 3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val example = Cons(1, Cons(2, Cons(3, Nil)))
  val example2 = List(1,2,3)
  val total = sum(example)

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(l: List[Int]) =
    foldRight(l, 0.0)(_ + _)

  def product2(l: List[Double]) =
    foldRight(l, 1.0)(_ * _)

}
