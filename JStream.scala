sealed trait JStream[+A] { self =>

  def toList: List[A] = foldLeft(List[A]())(_ :: _).reverse

  def take(n: Int): JStream[A] = self match {
    case Cons(a, t) if n > 0 => JStream.cons(a(), t().take(n - 1))
    case _ => JStream.empty
  }

  def drop(n: Int): JStream[A] = self match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => self
  }

  def takeWhile(p: A => Boolean): JStream[A] =
    foldRight(JStream.empty: JStream[A]){ (a,b) => 
      if (p(a)) JStream.cons(a, b)
      else b
    }

  def dropWhile(p: A => Boolean): JStream[A] = self match {
    case Cons(a, t) if p(a()) => t().dropWhile(p)
    case _ => self
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a,b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)

  def foldLeft[B](z: B)(f: (A, => B) => B): B = self match {
    case Cons(a, t) => t().foldLeft(f(a(), z))(f)
    case _ => z
  }

  def foldRight[B](z: B)(f: (A, => B) => B): B = self match {
    case Cons(a, t) => f(a(), t().foldRight(z)(f))
    case _ => z
  }

  def headOption: Option[A] =
    foldRight(None: Option[A]){ (a,b) =>
      if (a == Empty) b
      else Some(a)
    }

  def map[B](f: A => B): JStream[B] =
    foldRight(JStream.empty: JStream[B]){ (a, b) =>
      JStream.cons(f(a), b)
    }

  def filter(p: A => Boolean): JStream[A] =
    foldRight(JStream.empty: JStream[A]){ (a,b) =>
      if (p(a)) JStream.cons(a, b)
      else b
    }

  def append[AA >: A](a: => AA): JStream[AA] =
    foldRight(JStream.cons(a, JStream.empty)){ (aa,b) => 
      JStream.cons(aa, b)
    }

  // TODO: fix flatMap
  // def flatMap[B](f: A => JStream[B]): JStream[B] =
  //   foldRight(JStream.empty: JStream[B]){ (a,b) =>
  //     f(a)
  //   }
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
