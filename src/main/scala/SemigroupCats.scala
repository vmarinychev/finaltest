import cats.Semigroup
object SemigroupCats extends App {

  case class Money(dollars: Int, cents: Int)

  implicit val addMoneySemigroup = new Semigroup[Money] {
    override def combine(x: Money, y: Money): Money =
      Money((x.dollars + y.dollars) + (x.cents + y.cents) / 100,
      (x.cents + y.cents) % 100)
  }
  import cats.instances.int._
  import cats.instances.map._

  def add[A: Semigroup](a: A, b: A)(implicit semigroup: Semigroup[A]): A = semigroup.combine(a,b)


  def addMoney(a: Money, b: Money): Money = {
    Money((a.dollars + b.dollars) + (a.cents + b.cents) / 100,
      (a.cents + b.cents) % 100)
  }

  trait Addable[T] {
    def add(a: T, b: T): T
  }

  def addMaps[K, V](balances: Map[K, V], newMap: Map[K, V])(implicit addable: Addable[V]):  Map[K, V] = {
    balances.foldLeft(newMap) {
      case (acc, (k, v)) =>
        acc + (k -> acc.get(k).fold(v)(addable.add(_, v)))
    }
  }

  implicit val addMoneyIn = new Addable[Money] {
    override def add(a: Money, b: Money): Money = addMoney(a, b)
  }

  implicit val addIntIn = new Addable[Int] {
    override def add(a: Int, b: Int): Int = a + b
  }

  println(addMaps(Map("lal"-> Money(1231,321),"231" -> Money(1,23)),Map("lal"-> Money(1231,321),"231" -> Money(1,23))))
  println(add(Map("lal"-> Money(1231,321),"231" -> Money(1,23)),Map("lal"-> Money(1231,321),"231" -> Money(1,23))))
}
