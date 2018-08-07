package course

sealed trait Optional[+A]
final case class Full[A](a: A) extends Optional[A]
case object Empty extends Optional[Nothing]

object Optional {

  /**
   * Map the given function on the possible value.
   *
   * >>> mapOptional (+1) Empty
   * Empty
   *
   * >>> mapOptional (+1) (Full 8)
   * Full 9
   */
  def mapOptional[A, B](f: A => B)(fa: Optional[A]): Optional[B] =
    fa match {
      case Full(x) => Full(f(x))
      case Empty   => Empty
    }

  /**
   * Bind the given function on the possible value.
   *
   * >>> bindOptional Full Empty
   * Empty
   *
   * >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 8)
   * Full 7
   *
   * >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 9)
   * Full 10
   */
  def bindOptional[A, B](f: A => Optional[B])(fa: Optional[A]): Optional[B] =
    fa match {
      case Full(x) => f(x)
      case Empty   => Empty
    }

  /**
   * Return the possible value if it exists; otherwise, the second argument.
   *
   * >>> Full 8 ?? 99
   * 8
   *
   * >>> Empty ?? 99
   * 99
   */
  def `??`[A](fa: Optional[A], default: A): A =
    fa match {
      case Full(x) => x
      case Empty   => default
    }

  /**
   * Try the first optional for a value. If it has a value, use it; otherwise, use the second value.
   *
   * >>> Full 8 <+> Empty
   * Full 8
   *
   * >>> Full 8 <+> Full 9
   * Full 8
   *
   * >>> Empty <+> Full 9
   * Full 9
   *
   * >>> Empty <+> Empty
   * Empty
   */
  def `<+>`[A](fa: Optional[A], ga: Optional[A]): Optional[A] =
    fa match {
      case Full(_) => fa
      case Empty   => ga
    }

  /**
   * Replaces the Full and Empty constructors in an optional.
   *
   * >>> optional (+1) 0 (Full 8)
   * 9
   *
   * >>> optional (+1) 0 Empty
   * 0
   */
  def optional[A, B](f: A => B)(default: B, fa: Optional[A]): B =
    fa match {
      case Full(x) => f(x)
      case Empty   => default
    }

  def applyOptional[A, B](f: Optional[A => B])(fa: Optional[A]): Optional[B] =
    bindOptional[A => B, B](g => mapOptional[A, B](g)(fa))(f)

  def twiceOptional[A, B, C](f: A => B => C)(fa: Optional[A])(fb: Optional[B]): Optional[C] =
    applyOptional[B, C](mapOptional[A, B => C](f)(fa))(fb)

  trait Eq[A] {
    def `===`(a1: A, a2: A): Boolean
  }

  object Eq {
    def apply[A](implicit ev: Eq[A]): Eq[A] = ev
  }

  def contains[A: Eq](a: A, fa: Optional[A]): Boolean =
    fa match {
      case Full(x) => Eq[A].`===`(a, x)
      case Empty   => false
    }

}
