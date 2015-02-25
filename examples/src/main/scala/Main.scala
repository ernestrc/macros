
import com.novus.salat.Grater
import unstable.macros.Macros._
import com.novus.salat.global._

import scala.reflect.ClassTag


sealed trait A {
  val id: Long
}
case class Aa(id: Long) extends A
case class Ae(id: Long) extends A
case class Ai(id: Long) extends A

object Main extends App {

  val amazingMacros = 10
  printExpr(amazingMacros * amazingMacros)

  sealed class TestClass(_name:String, _age:Int){
    private [this] var h = _age
    val name = "\"" + _name + "\""

    def getAge = h
    def setAge(to:Int) = {
      h = to
    }
  }

  val nano = new TestClass("Ernie",25)

  inspectMembers(nano)

//  val graters: PartialFunction[String, Grater[_ <: A]] = grateDescendantsFromContext[A](ctx,classOf[A])

//  graters("Aa")


  val test = missingParamType(1)

}