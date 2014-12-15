package unstable.build.macros

/**
 * Created by ernestrc on 15/12/2014.
 */
object Main extends App{
  import DebugMacros._

  val amazing = 10

  printExpr(amazing * amazing)

}
