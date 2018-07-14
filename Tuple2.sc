  import scala.util.Try

  /**
   *  Returns an [[Option]] 2-Tuple mapped via the supplied function. If
   *  any of a or b are None, then the result is None
   *
   *  @param a [[Option]] to map over for left-hand or _1 tuple member
   *  @param b [[Option]] to map over for right-hand or _2 tuple member
   *  @param f mapping function
   */
  def map2[A,B](a: Option[A], b: Option[A], f: A => Option[B]): Option[(B,B)] =
    for {
      l <- a
      r <- b
      left <- f(l)
      right <- f(r)
    } yield (left, right)

  /**
   *  Returns an [[Option]] 2-Tuple with the mapped value as member _1
   *  and the derived value as _2
   *  @param fa function to produce left-hand tuple member
   *  @param fb function to derive right-hand tuple member
   *  @param a [[Option]] to map over
   */
  def mapL[A,B](fa: A => Option[B], fb: B => B): Option[A] => Option[(B,B)] = { a =>
    for {
      s <- a
      l <- fa(s)
    } yield (l, fb(l))
  }

  /**
   *  Returns an [[Option]] 2-Tuple with the mapped value as member _2
   *  and the derived value as member _1
   *  @param fb function to derive left-hand tuple member
   *  @param fa function to produce right-hand tuple member
   *  @param a [[Option]] to map over
   */
  def mapR[A,B](fb: B => B, fa: A => Option[B]): Option[A] => Option[(B,B)] = { a =>
    mapL(fa, fb)(a).map(_.swap)
  }

  /**
   *  Returns an [[Option]] from any function that returns a [[Try]] 
   * 
   *  @param f [[Try]] function
   *  @param a input to f
   */
  def curryTry[A,B](f: A => Try[B]): A => Option[B] = { a => f(a).toOption }
