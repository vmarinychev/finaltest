import cats.Show

object ShowCat extends App {
  case class Cat(name: String, age: Int, color: String)


  object PrintableInstance {
    implicit val catInstance = new Show[Cat] {
      override def show(value: Cat): String = value.age + value.name + "----entityInstance"
    }
  }

  implicit class ShowClass[T](value: T) {
    def show(implicit show: Show[T]) =println(show.show(value))
  }

  import PrintableInstance._

  Cat("lalal", 123, "kakk").show
}
