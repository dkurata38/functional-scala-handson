object Main {

  def main(args: Array[String]): Unit = {
    println(length(List(1,3,9,1,5,8)))
  }

  //  f(1) = 1 f(2) = f(1) + 2 f(n) = f(n-1) + 1
  def count(n: Int): Int = n match {
    case 1 => 1
    case _ => count(n - 1) + n
  }
  // f(1) = 1 f(2) = f(1) * 2 f(n) = f(n - 1) * n
  def aaa(n: Int): Int = {
    if (n == 1) 1
    else aaa(n - 1) * n
  }

  // f(1) = 1 f(2) = 1 f(3) = f(1) + f(2)
  def fib(count: Int): Int = count match {
    case 1 => 1
    case 2 => 1
    case _ => fib(count-1) + fib(count - 2)
  }

  /*
      5,2,1,5,Nil
      5 2,1,5,Nil
      5+2 1,5,Nil
      5+2+1 5,Nil
      5+2+1+5 Nil

   */
  def sum(ints: List[Int]) = ints.sum

  def sum1(ints: List[Int]) = ints match {
    case Nil => 0
    case head :: tail => head + sum(tail)
  }

  def product(ints: List[Int]):Option[Int] = ints match {
    case Nil => None
    case head :: tail => product(tail) match {
      case Some(p) => Some(p * head)
      case _ => None
    }
  }

  // 5,2,1,5,Nil -> 大小比較していく
  def max(ints: List[Int]): Option[Int] = ints match {
    case Nil => None
    case head :: tail => max(tail) match {
      case Some(m) => Some(if (head > m) head else m)
      case _ => None
    }
  }

  def reverse(ints: List[Int]): List[Int] = ints match {
    case Nil => Nil
      // :+ リストの末尾に要素を追加する.
    case head :: tail => reverse(tail) :+ head
  }

  def length(ints: List[Int]): Int = ints match {
    case Nil => 0
    case head :: tail => 1 + length(tail)
  }
}
