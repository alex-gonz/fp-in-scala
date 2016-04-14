
object Chapter2 {
  def main(args: Array[String]): Unit = {
    println("Hello, World!")
    (0 to 5).map(fib).foreach(println)
    val sorted = Array(1,2,5)
    val notSorted = Array(1,5,2)
    println(s"isSorted(${sorted.toSeq}, <) = ${isSorted[Int](sorted, (m,n) => m < n)}")
    println(s"isSorted(${notSorted.toSeq}, <) = ${isSorted[Int](notSorted, (m,n) => m < n)}")
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, i0: Int, i1: Int): Int =
      if (n <= 0) i0
      else if (n == 1) i1
      else go(n - 1, i1, i0 + i1)
    go(n, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length) true
      else if (!ordered(as(n - 1), as(n))) false
      else loop(n + 1)
    loop(1)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  
}
