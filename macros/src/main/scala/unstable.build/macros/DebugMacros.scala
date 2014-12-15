package unstable.build.macros

import language.experimental.macros
import reflect.macros.Context

private[macros] object DebugMacrosImpl {

  /**
   * Prints the expression an its result.
   * Example:
   *    val x = 10
   *    printExpr(x*10) //outputs Main.this.x.*(10) = 100
   *
   * Explanation:
   * show(param.tree) stringifies the tree of the passed expression.
   * We need to pass a splicee Expr[T] to println in order to execute it
   * inside reify. We do this by manually creating a tree and pass it
   * to c.Expr[String].
   */
  def printExpr_impl(c:Context)(param:c.Expr[Any]):c.Expr[Unit] = {
    import c.universe._
    val paramRepTree = Literal(Constant(show(param.tree)))
    val paramRepExpr = c.Expr[String](paramRepTree)
    reify { println(paramRepExpr.splice + " = " + param.splice) }
  }

}

object DebugMacros {
  import DebugMacrosImpl._

  def printExpr(param:Any):Unit = macro printExpr_impl

}
