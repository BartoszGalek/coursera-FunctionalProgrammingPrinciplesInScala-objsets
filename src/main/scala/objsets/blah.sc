abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  override def isZero: Boolean = true

  override def predecessor: Nat =
    throw new Exception("o.predecessor")

  override def +(that: Nat): Nat =
    that

  override def -(that: Nat): Nat =
    if (that.isZero) this else throw new Exception("o.predecessor")

}
class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false

  override def predecessor: Nat = n

  override def +(that: Nat): Nat = new Succ(n + that)

  override def -(that: Nat): Nat =
    if (that.predecessor.isZero) this else n - that.predecessor
}

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Nil[T] extends List[T] {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

object List {
  def apply[T](): List[T] = new Nil
  def apply[T](x1: T): List[T] = new Cons[T](x1, new Nil)
  def apply[T](x1: T, x2: T): List[T] = new Cons[T](x1, new Cons[T](x2, new Nil))
  def apply[T](x1: T, x2: T, x3: T): List[T] = new Cons[T](x1, new Cons[T](x2, new Cons[T](x3, new Nil)))
}