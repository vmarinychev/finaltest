import cats.Show
import cats.instances.int._
import cats.instances.string._

object CatsApp {

  val intShow = Show[Int]
////ssdfsdfsdfsdfsdfsdfsdfs
////ssdfsdfsdfsdfsdfsdfsdfs////ssdfsdfsdfsdfsdfsdfsdfs////ssdfsdfsdfsdfsdfsdfsdfs
  ////ssdfsdfsdfsdfsdfsdfsdfs


  case class Entity(name: String, age: String, color: String)


  trait Printable[T] {
    def format(value: T): String
  }

  object PrintableInstance {
    implicit val entityInstance = new Printable[Entity] {
      override def format(value: Entity): String = value.age + value.name + "----entityInstance"
    }
    implicit val intInstance = new Printable[Int] {
      override def format(value: Int): String = value.toString + "----PrintableInstance"
    }

    implicit val stringInstance = new Printable[String] {
      override def format(value: String): String = s"llalal - $value"
    }
  }

  object PrintableInterface {
    def print[T](value: T)(implicit printable: Printable[T]) = println(printable.format(value))
  }


  implicit class PrintClass[T](value: T) {
    def print(implicit printable: Printable[T]) = println(printable.format(value))
  }



  def main(args: Array[String]): Unit = {

    import cats.syntax.all._
    import PrintableInstance._
   // PrintableInterface.print("PrintableInterface")
   // PrintableInterface.print(12312)


    print(intShow.show(13231))
    Entity("Max", "33", "black").print
    "PrintableInterface".print
    123123.print
    print("Knoldus".show)
    //"Knoldus".show
  }
}
