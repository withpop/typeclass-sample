
import org.scalacheck.Prop.forAll
import org.scalatest.FunSuite
import anopara.typeclasstest._
import anopara.typeclasstest.AllInstances._


class TypeclassTest extends FunSuite {
  test("Semigroup[Int] test") {
    forAll {
      (x: Int, y: Int) => implicitly[Semigroup[Int]].append(x, y) == x + y
    }.check
  }

  test("Semigroup[String] test") {
    forAll {
      (x: String, y: String) => implicitly[Semigroup[String]].append(x, y) == x + y
    }.check
  }

  test("Monoid[Option[Int]] test") {
    forAll {
      (x: Option[Int], y: Option [Int]) =>
        val res = (x, y) match {
          case (Some(x), Some(y)) => Some(x + y)
          case (None, Some(y)) => Some(y)
          case (Some(x), None) => Some(x)
          case (None, None) => None
        }
        implicitly[Monoid[Option[Int]]].append(x, y) == res
    }.check
  }

  test("Monoid[Option[String]] test") {
    forAll {
      (x: Option[String], y: Option [String]) =>
        val res = (x, y) match {
          case (Some(x), Some(y)) => Some(x + y)
          case (None, Some(y)) => Some(y)
          case (Some(x), None) => Some(x)
          case (None, None) => None
        }
        implicitly[Monoid[Option[String]]].append(x, y) == res
    }.check
  }

}
