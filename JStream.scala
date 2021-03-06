import JStream._
sealed trait JStream[+A] { self =>

  def toList: List[A] = foldLeft(List[A]())(_ :: _).reverse

  def take(n: Int): JStream[A] = self match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => empty
  }

  def drop(n: Int): JStream[A] = self match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => self
  }

  def takeWhile(p: A => Boolean): JStream[A] =
    foldRight(empty[A])((h,t) => if (p(h)) cons(h, t) else t)

  def dropWhile(p: A => Boolean): JStream[A] = self match {
    case Cons(h, t) if p(h()) => t().dropWhile(p)
    case _ => self
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((h,t) => p(h) || t)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h,t) => p(h) && t)

  def foldLeft[B](z: B)(f: (A, => B) => B): B = self match {
    case Cons(h, t) => t().foldLeft(f(h(), z))(f)
    case _ => z
  }

  def foldRight[B](z: B)(f: (A, => B) => B): B = self match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def headOption: Option[A] =
    foldRight(None: Option[A]){ (h,t) =>
      if (h == empty) t
      else Some(h)
    }

  def map[B](f: A => B): JStream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): JStream[A] =
    foldRight(empty[A])((h,t) => if (p(h)) cons(h, t) else t)

  def append[B >: A](a: => JStream[B]): JStream[B] =
    foldRight(a)((h,t) => cons(h, t))

  def flatMap[B](f: A => JStream[B]): JStream[B] =
    foldRight(empty[B])((h,t) => f(h) append t)

}

final case object Empty extends JStream[Nothing]
final case class Cons[+A](h: () => A, t: () => JStream[A]) extends JStream[A] 

object JStream {

  def cons[A](hd: => A, tl: => JStream[A]): JStream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: JStream[A] = Empty

  def apply[A](as: A*): JStream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

}
