import scala.annotation.tailrec
import scala.util.Random

class Lists {
  def last[A](list: List[A]): A = list.last

  def penultimate[A](list: List[A]): A = list.reverse.tail.head

  def nth[A](n: Int, list: List[A]): A = list(n)

  def length[A](list: List[A]): Int = list.length

  def reverse[A](list: List[A]): List[A] = list.reverse

  def isPalindrome[A](list: List[A]): Boolean = list == list.reverse

  def flatten(list: List[Any]): List[Any] = list flatMap {
    case xs: List[_] => flatten(xs)
    case x => List(x)
  }

  def compress[A](list: List[A]): List[A] = list.foldRight(List.empty[A]) { (elem, l) => if (l.isEmpty || l.head != elem) elem :: l else l }

  def pack[A](list: List[A]): List[List[A]] = {
    @tailrec
    def checkNext(list: List[A], prev: A, acc: List[List[A]]): List[List[A]] = list match {
      case Nil => acc
      case x :: xs if x == prev => checkNext(xs, x, (x :: acc.head) :: acc.tail)
      case x :: xs => checkNext(xs, x, List(x) :: acc)
    }

    checkNext(list.reverse.tail, list.last, List(List(list.last)))
  }

  def encode[A](list: List[A]): List[(Int, A)] = pack(list).map { l => (l.size, l.head) }

  def encodeModified[A](list: List[A]): List[Any] = encode(list) map {
    case (n, a) if n == 1 => a
    case a => a
  }

  def decode[A](list: List[(Int, A)]): List[A] = list flatMap { a => List.fill(a._1)(a._2) }

  def encodeDirect[A](list: List[A]): List[(Int, A)] =
    if (list.isEmpty) Nil
    else {
      val (acc, next) = list span { _ == list.head }
      (acc.length, acc.head) :: encodeDirect(next)
    }

  def duplicate[A](list: List[A]): List[A] = list flatMap { a => List.fill(2)(a) }

  def duplicateN[A](n: Int, list: List[A]): List[A] = list flatMap { a => List.fill(n)(a) }

  def drop[A](n: Int, list: List[A]): List[A] = list.zipWithIndex filter { x => (x._2 + 1) % n != 0 } map { _._1 }

  def split[A](n: Int, list: List[A]): (List[A], List[A]) = list.splitAt(n)

  def slice[A](i: Int, j: Int, list: List[A]): List[A] = list.slice(i, j)

  def rotate[A](n: Int, list: List[A]): List[A] = {
    val nBounded = if (list.isEmpty) 0 else n % list.length
    if (nBounded < 0) rotate(nBounded + list.length, list)
    else (list drop nBounded) ::: (list take nBounded)
  }

  def removeAt[A](n: Int, list: List[A]): (List[A], A) =
    if (n < 0) throw new NoSuchElementException
    else ((list take n) ::: (list drop n + 1), nth(n, list))

  def insertAt[A](elem: A, n: Int, list: List[A]): List[A] =
    if (n < 0) throw new NoSuchElementException
    else (list take n) ::: List(elem) ::: (list drop n)

  def range(i: Int, j: Int): List[Int] = List.range(i, j + 1)

  def randomSelect[A](n: Int, list: List[A]): List[A] = {
    @tailrec
    def randomSelectRecursive(n: Int, l: List[A], acc: List[A]): List[A] =
      if (n == 0) acc
      else {
        val (rest, elem) = removeAt(Random.nextInt(l.length), l)
        randomSelectRecursive(n - 1, rest, acc :+ elem)
      }

    randomSelectRecursive(n, list, List.empty[A])
  }

  def lotto(n: Int, r: Int): List[Int] = randomSelect(n, List.range(0, r + 1))

  def randomPermute[A](list: List[A]): List[A] = randomSelect(list.length, list)

  def combinations[A](n:Int, list: List[A]): List[List[A]] = list.combinations(n).toList
}
