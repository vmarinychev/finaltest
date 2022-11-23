import cats.Monoid
import cats.instances.int._
import cats.instances.map._
import cats.syntax.semigroup._

object MonoidCats extends App{

  case class Order(price: Double, quantity: Int)

  implicit val OrderMonoid = new Monoid[Order] {
    override def empty: Order = Order(0.0, 0)

    override def combine(a: Order, b: Order): Order = {
      Order((a.price |+| b.price), (a.quantity |+| b.quantity))
    }
  }

  def addOrders(list: List[Order])(implicit monoid: Monoid[Order]): Order = {
    list.foldLeft(monoid.empty)(monoid.combine)
  }

  def addWithOutMonoid(list: List[Option[Int]]): Option[Int] = {
    list.foldLeft(Option(0)) {
      case (acc, value) => acc.map(i => value.fold(i)(_ + i))
    }
  }

  def addWithMonoid(list: List[Option[Int]]): Option[Int] = {
    list.foldLeft(Monoid.empty[Option[Int]])(_ |+| _)
  }


  val res = addWithOutMonoid(List(Some(1),Some(1),Some(1),Some(1)))
  val resWithMonoid = addWithMonoid(List(Some(2),Some(2),Some(2),Some(2)))

  val orders = List(Order(21,3),Order(21.10,3),Order(21,3),Order(21,3),Order(21,3),Order(21,3))
  println(addOrders(orders))
  println(res)
  println(resWithMonoid)
}
