
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
    as.foldLeft(Option(List[A]())) { (acc, ao) => ao.flatMap(a => acc.map(_ ++ List(a))) }

  }
}

object ex4 extends App {

}
