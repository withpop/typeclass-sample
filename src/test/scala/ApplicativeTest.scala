
import org.scalacheck.Prop.forAll
import org.scalatest.FunSuite
import scalaz._
import Scalaz._

class ApplicativeTest extends FunSuite {
  test("x*2 should be equals to x+x") {
    forAll {
      (x: Int) => x * 2 == x + x
    }.check
  }

  test("x*2 should be equals to x+x") {
    forAll {
      (x: Int) => x * 2 == x + x
    }.check
  }

}
