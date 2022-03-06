import scala.util.matching.Regex

abstract class UnlambdaObject{
  def toString(): String
}
////////////////////////////////////////////////////////
trait Node{
  def eval(): Function
}

class Apply(fun: Node, arg: Node) extends UnlambdaObject with Node{
  def eval(): Function ={
    val ef = fun.eval()
    val ea = arg.eval()
    ef.call(ea)
  }
  override def toString(): String = "(%s %s)".format(fun, arg)
}
/////////////////////////////////////////////////////////
trait Function {
  def call(arg:Function): Function
  def eval(): Function = this
}

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
/////////////////////////////////////////////////////////
object UnlambdaInterpriter{
  def tokenize(src: String, result: List[String] = Nil): Array[String] = {
    if(src == "") return result.reverse.toArray

    val re_single_char = """^([`skir])(.*)""".r
    val re_dot: Regex = """^(\..)(.*)""".r

    val (inst:String,tail:String) = src match {
      case re_single_char(x,y) => (x,y)
      case re_dot(x,y)         => (x,y)
      case _                   => return tokenize(src.tail, result)
    }
    tokenize(tail, inst::result)
  }

  class TokenReader(val tokens:Array[String], var ptr:Int = -1){
    def next():String = {
      ptr += 1
      tokens(ptr)
    }
    def eos():Boolean = (ptr>=tokens.length)
  }

  def make_node(tr: TokenReader): Node = {
    if(tr.eos()) throw new Exception("make_node() => EOS")
    val t = tr.next()
    val re_dot: Regex = """^\.(.)""".r
    val result:Node = t match {
      case "`" => 
        val fun = make_node(tr)
        val arg = make_node(tr)
        new Apply(fun,arg)
      case "i" | "k" | "s" | "r"=> new SingleArgFunction(t)
      case re_dot(x) =>  new SingleArgFunction(t)
      case _   => throw new Exception("make_node() => unknown function[" + t+"]")
    }
    return result
  }

  def main(args: Array[String]) :Unit = {
    println("[ARGS] => " + args.mkString("[",",","]"))
    val src = if(args.length != 0 ) args(0) 
              else "`r```````````.H.e.l.l.o. .w.o.r.l.di"

    println("[SOURCE] => " + src)
    val tokens = tokenize(src) 
    println("[TOKENS] => " + tokens.mkString(""," ",""))
    val node = make_node(new TokenReader(tokens))
    println("[NODES] => "+node)
    println("=====eval=====")
    val result = node.eval()
    println("\n=====end=====")
    println("[RESULT] => "+result)
  }
}