
trait JOption[+A] {
  def map[B](f: A => B): JOption[B]
  def flatMap[B](f: A => JOption[B]): JOption[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](op: => JOption[B]): JOption[B]
  def filter(f: A => Boolean): JOption[A]
}

case class JSome[A](value: A) extends JOption[A] {
  def map[B](f: A => B): JOption[B] = JSome(f(value))
  def flatMap[B](f: A => JOption[B]): JOption[B] = f(value)
  def getOrElse[B >: A](default: => B): B = value
  def orElse[B >: A](op: => JOption[B]): JOption[B] = this
  def filter(f: A => Boolean): JOption[A] = if (f(value)) this else JNone
}

case object JNone extends JOption[Nothing] {
  def map[B](f: Nothing => B): JOption[B] = this
  def flatMap[B](f: Nothing => JOption[B]): JOption[B] = this
  def getOrElse[B >: Nothing](default: => B): B = default
  def orElse[B >: Nothing](op: => JOption[B]): JOption[B] = op
  def filter(f: Nothing => Boolean): JOption[Nothing] = this
}

// Here's another (more proper?) way to implement it i.e entirely in the trait's terms
trait JOption[+A] { self =>
  def map[B](f: A => B): JOption[B] = self match {
    case JSome(a) => JSome(f(a))
    case _ => JNone
  }
  
  def flatMap[B](f: A => JOption[B]): JOption[B] = self match {
    case JSome(a) => f(a)
    case _ => JNone
  }
  
  def getOrElse[B >: A](default: => B): B = self match {
    case JSome(a) => a
    case _ => default
  }
  
  def orElse[B >: A](op: => JOption[B]): JOption[B] = self match {
    case s:JSome[A] => s.asInstanceOf[JSome[B]]
    case _ => op
  }
  
  def filter(f: A => Boolean): JOption[A] = self match {
    case JSome(a) if f(a) => self
    case _ => JNone
  }
}

// The concrete members become basic declarations
case class JSome[A](value: A) extends JOption[A]
case object JNone extends JOption[Nothing]
