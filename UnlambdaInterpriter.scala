/// import ///
import scala.util.matching.Regex
/// abstract ///
abstract class UnlambdaObject{
  def toString(): String
}
/// traits ///
trait Function {
  def call(arg:Function): Function
  def eval(): Function = this
}
trait Node{
  def eval(): Function
}
/// implement of function ///
class SingleArgFunction(name: String) extends UnlambdaObject with Function with Node {
  def call(arg: Function): Function = {
    val re_dot: Regex = """^\.(.)""".r
    name match {
      case "i" => arg
      case "k" => new MultiArgFunction("K2", arg, this) // "this" is dummy
      case "s" => new MultiArgFunction("S2", arg, this) // "this" is dummy
      case "r" => 
        print("\n")
        arg
      case re_dot(x) => 
        print(x)
        arg
      case _ => throw new Exception("SingleArgFunction("+name+") => ERROR")
    }
  }
  override def toString():String = name
}
class MultiArgFunction(name:String, x:Function, y:Function) extends UnlambdaObject with Function with Node {
  def call(arg: Function): Function = {
    name match {
      case "K2" => this.x
      case "S2" => new MultiArgFunction("S3",x,arg)    
      case "S3" => 
        val xx = x.call(arg)
        val yy = y.call(arg)
        xx.call(yy)
      case _ => throw new Exception("MultiArgFunction("+name+") => ERROR")
    }
  }
  override def toString():String = name
}
/// "`" operator ///
class Apply(fun: Node, arg: Node) extends UnlambdaObject with Node{
  def eval(): Function ={
    val ef = fun.eval()
    val ea = arg.eval()
    ef.call(ea)
  }
  override def toString(): String = "(%s %s)".format(fun, arg)
}
/////////////////////////////////////////////////////////
object UnlambdaInterpriter{
  /// tokenizer ///
  def tokenize(src: String, result: List[String] = Nil): List[String] = {
    if(src == "") return result.reverse

    val re_single_char = """^([`skir])(.*)""".r
    val re_dot: Regex = """^(\..)(.*)""".r

    src match {
      case re_single_char(inst,tail) => tokenize(tail,inst::result)
      case re_dot(inst,tail)         => tokenize(tail,inst::result)
      case _                   => return tokenize(src.tail, result)
    }
  }

  /// parse ///
  def make_node(tokens: List[String]): (Node,List[String]) = {
    if(tokens == Nil) throw new Exception("make_node() => EOS")
    val head = tokens.head
    val re_dot: Regex = """^\.(.)""".r
    head match {
      case "`" => 
        val (fun,tail1) = make_node(tokens.tail)
        val (arg,tail2) = make_node(tail1)
        return (new Apply(fun,arg), tail2)
      case "i" | "k" | "s" | "r" =>
        return (new SingleArgFunction(head), tokens.tail)
      case re_dot(x) =>  
        return (new SingleArgFunction(head), tokens.tail)
      case _   => throw new Exception("make_node() => unknown function["+head+"]")
    }
  }

  /// main ///
  /// eval(commandline argument) as unlambda code. ///
  def main(args: Array[String]) :Unit = {
    println("[ARGS] => " + args.mkString("[",",","]"))
    val src = if(args.length != 0 ) args(0) 
              else "`r```````````.H.e.l.l.o. .w.o.r.l.di"

    println("[SOURCE] => " + src)
    val tokens = tokenize(src) 
    println("[TOKENS] => " + tokens.mkString(""," ",""))
    val (node,_) = make_node(tokens)
    println("[NODES] => "+node)
    println("=====eval=====")
    val result = node.eval()
    println("\n=====end=====")
    println("[RESULT] => "+result)
  }
}