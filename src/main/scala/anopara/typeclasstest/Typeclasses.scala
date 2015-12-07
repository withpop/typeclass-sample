package anopara.typeclasstest

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B) : F[B]
}

trait Apply[F[_]] {
  def ap[A, B](fa: F[A])(f : F[A => B]) : F[B]
}

trait Applicative[F[_]] extends Apply[F] with Functor[F] {
  def point[A](a : A): F[A]
}

trait Semigroup[F] {
  def append(f1: F, f2: F): F
}

trait Monoid[F] extends Semigroup[F] {
  def zero: F
}

trait Monad[F[_]] extends Applicative[F] {
  def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
  def >>=[A, B](fa: F[A])(f: A => F[B]): F[B] = bind(fa)(f)
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = bind(fa)(f)
}

