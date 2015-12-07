package anopara.typeclasstest

object AllInstances extends Instances

trait Instances {
  implicit val optionFunctor: Functor[Option] = new Functor[Option]{
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = // fa map f
      fa match {
        case Some(a) => Some(f(a))
        case None => None
      }
  }

  implicit val optionApply: Apply[Option] = new Apply[Option]{
    override def ap[A, B](fa: Option[A])(f: Option[A => B]): Option[B] = f match {
      case Some(f) => fa match {
        case Some(x) => Some(f(x))
        case None => None
      }
      case None => None
    }
  }

  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def point[A](a: A): Option[A] = Some(a)

    override def map[A, B](fa: Option[A])(f: (A) => B): Option[B] = fa match {
      case Some(a) => Some(f(a))
      case None => None
    }

    override def ap[A, B](fa: Option[A])(f: Option[(A) => B]): Option[B] = f match {
      case Some(f) => fa match {
        case Some(x) => Some(f(x))
        case None => None
      }
      case None => None
    }
  }

  implicit val intSemigroup: Semigroup[Int] = new Semigroup[Int] {
    override def append(f1: Int, f2: Int): Int = f1 + f2
  }

  implicit val stringSemigroup: Semigroup[String] = new Semigroup[String] {
    override def append(f1: String, f2: String): String = f1 + f2
  }

  implicit def monoid[A: Semigroup]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def append(f1: Option[A], f2: Option[A]): Option[A]= (f1, f2) match {
      case (Some(a), Some(b)) => Some(implicitly[Semigroup[A]].append(a, b))
      case (Some(a), None) => Some(a)
      case (None, Some(b)) => Some(b)
      case (None, None) => None
    }
    override def zero: Option[A] = None
  }



}