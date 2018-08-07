package course

sealed trait ExactlyOne[A]
case class One[A](a: A) extends ExactlyOne[A]

object ExactlyOne {

  type Id[A] = A

  def runExactlyOne[A](fa: ExactlyOne[A]): A =
    fa match {
      case One(x) => x
    }

  def mapExactlyOne[A, B](f: A => B)(fa: ExactlyOne[A]): ExactlyOne[B] =
    fa match {
      case One(x) => One(f(x))
    }

  def bindExactlyOne[A, B](f: A => ExactlyOne[B])(fa: ExactlyOne[A]): ExactlyOne[B] =
    fa match {
      case One(x) => f(x)
    }

}
