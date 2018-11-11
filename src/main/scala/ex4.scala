import scala.annotation.tailrec

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] =
    this match {
      case Some(a: A) => Some(f(a))
      case None => None
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this match {
      case Some(a: A) => f(a)
      case None => None
    }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(b: B) => b
    case None => default
  }

  def orElse[B >: A](on: => Option[B]): Option[B] = this match {
    case Some(b: B) => Some(b)
    case None => on
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a: A) if f(a) => Some(a)
    case _ => None
  }

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def sequence[A](as: List[Option[A]]): Option[List[A]] = {
    var r = List[A]()
    for (a <- as) {
      a match {
        case None => return None
        case Some(a1: A) => r = r ++ List(a1)
      }
    }
    Some(r)
  }

  def sequence2[A](as: List[Option[A]]): Option[List[A]] = {
    as match {
      case Nil => Some(List())
      case h::t => h flatMap{ a => sequence2(t).map(List(a) ++ _)
      }
    }

  }
}

object ex4 extends App {
  val list = List(Some(1),Some(2),Some(3))
  val listNone = List(Some(1),Some(2),None)
  val listEmpty = List()

  println(Option.sequence(list))
  println(Option.sequence(listNone))
  println(Option.sequence(listEmpty))
  println(Option.sequence2(list))
  println(Option.sequence2(listNone))
  println(Option.sequence2(listEmpty))
}
