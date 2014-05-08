package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersection contains the overlapping elements") {
    new TestSets {
      var u = union(s1, s2)
      var n = intersect(s1, s2)
      var t1 = intersect(u, s1)
      var t2 = intersect(u, s2)
      var n3 = intersect(u, s3)

      assert(!contains(n, 1), "Intersection of 1 and 2 doesn't contain 1")
      assert(!contains(n, 2), "Intersection of 1 and 2 doesn't contain 2")

      assert(contains(t1, 1),
        "Intersection of (1, 2) and 1 contains 1")
      assert(contains(t2, 2),
        "Intersection of (1, 2) and 2 contains 2")
      assert(!contains(n3, 3),
        "Intersection of (1, 2) and 3 doesn't contain 3")
    }
  }

  test("difference contains elements in s but not in t") {
    new TestSets{
      var u = union(s1, s2)
      assert(!contains(diff(u, s1), 1), "Diff of (1, 2) and 1 doesn't contain 1")
      assert(!contains(diff(u, s2), 2), "Diff of (1, 2) and 2 doesn't contain 2")
      assert(contains(diff(u, s3), 1), "Diff of (1, 2) and 3 contains 1")
      assert(contains(diff(u, s3), 2), "Diff of (1, 2) and 3 contains 2")
      assert(!contains(diff(u, s3), 3), "Diff of (1, 2) and 3 doesn't contain 3")
    }
  }

  test("filter contains elements that satisfies the predicate") {
    new TestSets{
      var u = union(s1, union(s2, s3))
      var p = (x: Int) => x > 1
      var fu = filter(u, p);
      assert(!contains(fu, 1),
        "Elements in (1, 2, 3) that is greater than 1 doesn't contain 1")
      assert(contains(fu, 2),
        "Elements in (1, 2, 3) that is greater than 1 contains 2")
      assert(contains(fu, 3),
        "Elements in (1, 2, 3) that is greater than 1 contains 3")
    }
  }

  trait TestSet2 {
    val elem1 = singletonSet(-500)
    val elem2 = singletonSet(0)
    val elem3 = singletonSet(998)
    val elem4 = singletonSet(999)

    val even_union = union(elem1, union(elem2, elem3));
    val odd_union = union(even_union, elem4);
  }

  test("forall elements that satisfy the predicate") {
    new TestSet2 {
      assert(forall(even_union, (x: Int) => x % 2 == 0), "All elements are even")
      assert(!forall(odd_union, (x: Int) => x % 2 == 0), "Not all elements are even")
    }
  }

  test("there exists element that satisfies the predicate"){
    new TestSet2 {
      assert(!exists(even_union, (x: Int) => x % 2 != 0),
        "There doesn't exist an odd number in the even set")
      assert(exists(odd_union, (x: Int) => x % 2 != 0),
        "There exists an odd number in the odd set")
    }
  }

  test("map a set to another one by a given function") {
    new TestSets {
      val u = union(s1, union(s2, s3))
      var m = map(u, (x: Int) => x + 1)
      assert(!contains(m, 1), "(2, 3, 4) doesn't contain 1")
      assert(contains(m, 2), "(2, 3, 4) contains 2")
      assert(contains(m, 3), "(2, 3, 4) contains 3")
      assert(contains(m, 4), "(2, 3, 4) contains 4")

    }
  }

}
