package course

import Validation.Err

sealed trait Validation[+A]
final case class Error(error: Err) extends Validation[Nothing]
final case class Value[A](value: A) extends Validation[A]

object Validation {

  type Err = String

  /**
   * Returns whether or not the given validation is an error.
   *
   * >>> isError (Error "message")
   * True
   *
   * >>> isError (Value 7)
   * False
   *
   * prop> \x -> isError x /= isValue x
   */
  def isError[A](fa: Validation[A]): Boolean =
    fa match {
      case Error(_) => true
      case Value(_) => false
    }

  /**
   * Returns whether or not the given validation is a value.
   *
   * >>> isValue (Error "message")
   * False
   *
   * >>> isValue (Value 7)
   * True
   *
   * prop> \x -> isValue x /= isError x
   */
  def isValue[A](fa: Validation[A]): Boolean =
    !isError(fa)

  /**
   * Maps a function on a validation's value side.
   *
   * >>> mapValidation (+10) (Error "message")
   * Error "message"
   *
   * >>> mapValidation (+10) (Value 7)
   * Value 17
   *
   * prop> \x -> mapValidation id x == x
   */
  def mapValidation[A, B](f: A => B)(fa: Validation[A]): Validation[B] =
    fa match {
      case e: Error => e
      case Value(x) => Value(f(x))
    }

  /**
   * Binds a function on a validation's value side to a new validation.
   *
   * >>> bindValidation (\n -> if even n then Value (n + 10) else Error "odd") (Error "message")
   * Error "message"
   *
   * >>> bindValidation (\n -> if even n then Value (n + 10) else Error "odd") (Value 7)
   * Error "odd"
   *
   * >>> bindValidation (\n -> if even n then Value (n + 10) else Error "odd") (Value 8)
   * Value 18
   *
   * prop> \x -> bindValidation Value x == x
   */
   def bindValidation[A, B](f: A => Validation[B])(fa: Validation[A]): Validation[B] =
     fa match {
       case e: Error => e
       case Value(x) => f(x)
     }

  /**
   * Returns a validation's value side or the given default if it is an error.
   *
   * >>> valueOr (Error "message") 3
   * 3
   *
   * >>> valueOr (Value 7) 3
   * 7
   *
   * prop> \x -> isValue x || valueOr x n == n
   */
  def valueOr[A](fa: Validation[A], default: A): A =
    fa match {
      case Error(_) => default
      case Value(x) => x
    }

  /**
   * Returns a validation's error side or the given default if it is a value.
   *
   * >>> errorOr (Error "message") "q"
   * "message"
   *
   * >>> errorOr (Value 7) "q"
   * "q"
   *
   * prop> \x -> isError x || errorOr x e == e
   */
  def errorOr[A](fa: Validation[A], default: Err): Err =
    fa match {
      case Error(e) => e
      case Value(_) => default
    }

  def valueValidation[A](a: A): Validation[A] = Value(a)

}
