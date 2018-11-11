import scala.annotation.tailrec

object ex3 extends App {

  val list = List(1, 2, 3)


  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case h :: t => f(h, foldRight(t, z)(f))
  }

  def sumR(ns: List[Int]) =
    foldRight(ns, 0)(_ + _)

  def productR(ns: List[Int]) =
    foldRight(ns, 0)(_ * _)

  def lengthR[A](as: List[A]): Int =
    foldRight(as, 0)((_, t) => 1 + t)

  @tailrec
  def foldLeft[A, B](as: List[A], acc: B)(f: (A, B) => B): B = as match {
    case Nil => acc
    case h :: t => foldLeft(t, f(h, acc))(f)
  }

  def map1[A, B](as: List[A])(f: A => B): List[B] = {
    @tailrec
    def go(list: List[A], acc: List[B]): List[B] =
      list match {
        case Nil => acc
        case h :: t => go(t, acc :+ f(h))
      }

    go(as, Nil: List[B])
  }


  def sumL(as: List[Int]): Int =
    foldLeft(as, 0)(_ + _)

  def lengthL[A](as: List[A]): Int =
    foldLeft(as, 0)((_, t) => 1 + t)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((a: A, b: List[A]) => List(a) ++ b)

  def addOne(as: List[Int]): List[Int] =
    foldLeft(as, List[Int]())((a, b) => b ++ List(a + 1))

  def append[A](as: List[A], newA: A): List[A] =
    foldLeft(List(newA), as)((a: A, b: List[A]) => b ++ List(a))

  def map2[A, B](as: List[A])(f: A => B): List[B] =
    foldLeft(as, Nil: List[B])((a: A, b: List[B]) => b :+ f(a))

  println(list)
  println(append(list, 4))
  println(map1(list)(_ * 2))
  println(map2(list)(_ * 2))

}
