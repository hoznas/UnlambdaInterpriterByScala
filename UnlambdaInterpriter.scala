import scala.util.matching.Regex

abstract class UnlambdaObject{
  def toString(): String
}
trait Node{
  def eval(): Function

}

class Apply(fun: Node, arg: Node) extends UnlambdaObject with Node{
  override def toString(): String = "(%s %s)".format(fun, arg)
  override def eval(): Function ={
    val ef = fun.eval()
    val ea = arg.eval()
    ef.call(ea)
  }
}


abstract class Function extends UnlambdaObject with Node{
  def call(arg:Function): Function
  def eval(): Function
}
class I(name: String="I") extends Function {
  def call(arg: Function): Function = arg
  override def toString():String = name
  override def eval():Function = this
}
class K(name: String="K") extends Function{
  def call(arg: Function): Function = {
    new K2(arg)
  }
  override def toString():String = name
  override def eval():Function = this
}
class K2(x: Function,name: String="K2") extends Function{
    def call(arg: Function): Function = {
      x
    }
  override def toString():String = name
  override def eval():Function = this
}
class S(name: String="S") extends Function{
  def call(arg: Function): Function = {
    new S2(arg)
  }
  override def toString():String = name
  override def eval():Function = this
}
class S2(x:Function,name: String="S2") extends Function {
  def call(arg: Function): Function = {
    new S3(x,arg)
  } 
  override def toString():String = name
  override def eval():Function = this
}

class S3(x:Function, y:Function,name: String="S3") extends Function{
  def call(arg: Function): Function = {
    x.call(arg).call(y.call(arg))
  }
  override def toString():String = name
  override def eval():Function = this
}

class Dot(name: String) extends Function {
  def call(arg: Function): Function = {
    print(name)
    arg
  }
  override def toString():String = "." + name
  override def eval():Function = this
}
object UnlambdaInterpriter{
  def tokenize(src: String, result: List[String] = Nil): Array[String] = {
    if(src == "") return result.reverse.toArray

    val re_single_char = """^([`skir])(.*)""".r
    val re_dot: Regex = """^(\..)(.*)""".r

    val (inst:String,tail:String) = src match {
      case re_single_char(x,y) =>
        (x,y)
      case re_dot(x,y) =>
        (x,y)
      case _ =>
        return tokenize(src.tail, result)
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
    val result = t match {
      case "`" => 
        val fun = make_node(tr)
        val arg = make_node(tr)
        new Apply(fun,arg)
      case "i" => new I()
      case "k" => new K()
      case "s" => new S()
      case "r" => new Dot("\n")
      case re_dot(x) =>  new Dot(x)
      case _   => throw new Exception("make_node() => unknown function[" + t+"]")
    }
    return result
  }

  def main(args: Array[String]) :Unit = {
    println("[ARGS] => " + args.mkString("[",",","]"))
    val src = if(args.length != 0 ) args(0) 
              else "`r```````````.H.e.l.l.o. .w.o.r.l.di"

    val tokens = tokenize(src) 
    println("[TOKENS] => " + tokens.mkString("","",""))
    val node = make_node(new TokenReader(tokens))
    println("[NODES] => "+node)
    println("=====eval=====")
    val result = node.eval()
    println("\n=====end=====")
    println("[RESULT] => "+result)
  }
}