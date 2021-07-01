package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite:

  import FunSets.*

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

  trait TestSets:
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(1)

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */
  test("singleton set one contains one") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets:
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
  }

  test("union contains all elements of each set") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
  }

  test("intersect contains elements in both sets") {
    new TestSets:
      val s = intersect(s1, s2)
      val t = intersect(s2, s3)
      val u = intersect(s1, s4)
      assert(!contains(s, 1), "Intersect 1")
      assert(!contains(t, 2), "Intersect 2")
      assert(contains(u, 1), "Intersect 3")
  }

  test("diff contains elements that are only in 1 set") {
    new TestSets:
      val s = diff(s1, s2)
      assert(contains(s, 1), "Diff 1")
      assert(!contains(s, 2), "Diff 2")
  }

  test("filter returns subset of s which p holds") {
    new TestSets:
      def p(elem: Int): Int => Boolean = x => x == elem
      val s = filter(s1, p(1))
      assert(contains(s, 1), "Filter 1")

      def p2(elem: Int): Int => Boolean = x => x == elem + 1
      val t = filter(s1, p2(0))
      assert(contains(t, 1), "Filter 2")
  }

  test("forall returns whether all bounded integers within `s` satisfy `p`") {
    new TestSets:
      def p(elem: Int): Int => Boolean = x => x == elem - 1
      val s = forall(s1, p(2))
      assert(s, "ForAll 1")
  }

  test("exists returns whether there exists a bounded integer within `s` that satisfies `p`.") {
    new TestSets:
      def p(elem: Int): Int => Boolean = x => x == elem * 2
      val s = exists(s2, p(1))
      assert(s, "Exists 1")
  }

  test("map returns a set transformed by applying f to all elements in set") {
    new TestSets:
      val s = map(union(s1, s2), x => x * x)
      println(FunSets.toString(s))
      assert(contains(s, 1), "Map 1")
      assert(contains(s, 4), "Map 2")
      assert(!contains(s, 5), "Map 3")
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
