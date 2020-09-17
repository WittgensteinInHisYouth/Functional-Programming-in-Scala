package funsets

/**
 * 2. Purely Functional Sets.
 */
trait FunSets extends FunSetsInterface {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  override type FunSet = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: FunSet, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): FunSet = (x: Int) => x == elem


  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: FunSet, t: FunSet): FunSet = (x: Int) => (contains(s, x) || contains(t, x))

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: FunSet, t: FunSet): FunSet = (x: Int) => (contains(s, x) && contains(t, x))

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: FunSet, t: FunSet): FunSet = (x: Int) => (contains(s, x) && !contains(t,x))

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: FunSet, p: Int => Boolean): FunSet = intersect(s, p)


  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: FunSet, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (contains(diff(s, p), a)) false // if a is in part of s but not p then forall fail
      else if (a == bound) true // if everything works fine a should be able to reach the upper bound
      else iter(a+1)
    }
    iter(-bound) // initialize the loop at the lower bound
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: FunSet, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (contains(intersect(s, p), a)) true // if a is in s and p then exist is true
      else if (a == bound) false // if everything fails a should be able to reach the upper bound
      else iter(a+1)
    }
    iter(-bound) // initialize the loop at the lower bound
  }

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: FunSet, f: Int => Int): FunSet = (y: Int) => exists(s, (x: Int) => f(x)==y) // check whether there exist an element x in s such that f(x)=y

  /**
   * Displays the contents of a set
   */
  def toString(s: FunSet): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: FunSet): Unit = {
    println(toString(s))
  }
}

object FunSets extends FunSets
