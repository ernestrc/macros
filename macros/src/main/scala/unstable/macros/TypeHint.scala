package unstable.macros

import scala.reflect.ClassTag

trait TypeHint{

  val hint: String

  override def toString: String = s"TypeHint[$hint]"

  override def equals(obj: scala.Any): Boolean = obj match {
    case t: TypeHint ⇒ t.hint == hint
    case any ⇒ false
  }
}

object TypeHint{

  def apply[T](implicit t: ClassTag[T]):TypeHint =
    new TypeHint {
      override val hint: String = t.runtimeClass.getSimpleName
    }
}

case class InjectedTypeHint(hint: String) extends TypeHint