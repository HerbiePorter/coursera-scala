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
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


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

    val sOne = singletonSet(1)
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
      assert(contains(s1, 1), "Singleton 1")

      assert(contains(s2, 2), "Singleton 2")

      assert(contains(s3, 3), "Singleton 3")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains the elements which are in both sets") {
    new TestSets {
      val s = intersect(s1, sOne)
      assert(contains(s, 1), "Intersect 1 & 1")
      assert(!contains(s, 2), "Intersect 1 & 2")
    }
  }

  test("diff returns the elements which are in s and not t") {
    new TestSets {
      val s = diff(s1, s2)
      assert(contains(s, 1), "Diff 1")
      assert(!contains(s, 2), "Diff 2")
    }
  }


  test("filters returns the elements which are in s and also match the filter") {
    new TestSets {
      val s = filter(s1, (elem: Int) => elem == 1)
      assert(contains(s, 1), "Filter 1")
      assert(!contains(s, 2), "Filter 2")
    }
  }

  test("foreach whether all bounded integers within `s` satisfy `p`") {
    new TestSets {
      assert(forall(s1, (elem: Int) => elem == 1), "ForAll 1")
      assert(!forall(s2, (elem: Int) => elem == 1), "ForAll 1")
      assert(!forall(union(s1,s2), (elem: Int) => elem == 1), "ForAll 1")
      assert(forall(union(s1,s2), (elem: Int) => elem == 1 || elem == 2), "ForAll 1")
    }
  }


  test("exists checks to see if an integer exists in a set matches a predicate") {
    new TestSets {
      assert(exists(s1, (elem: Int) => elem == 1), "Exists 1")
      assert(!exists(s2, (elem: Int) => elem == 1), "Exists 2")
      assert(exists(union(s1,s2), (elem: Int) => elem == 1), "Exists 1")
      assert(!exists(union(s1,s2), (elem: Int) => elem == 3), "Exists 1")
    }
  }

  test("map returns a set transformed by applying `f` to each element of `s`.") {
    new TestSets {
      assert(map(s1, (elem: Int) => elem * 2)(2), "Exists 1")
    }
  }


}
