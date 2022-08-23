import scala.util.parsing.combinator.RegexParsers

sealed trait Type

case class SimpleType(name: String) extends Type

case class TupleType(names: List[Type]) extends Type

case class ArrayType(t: Type) extends Type

case class FixedLengthArrayType(t: Type, size: Int) extends Type

object SignatureParser extends RegexParsers {
  def functionName: Parser[String] = """[a-zA-Z0-9]+""".r
  def simple: Parser[Type] = """[a-z0-9]+""".r ^^ { s => SimpleType(s)}
  def array: Parser[Type] = (simple | tuple) <~ "[]" ^^ { s => ArrayType(s) }
  def fixedArray: Parser[Type] = (simple | tuple) ~ ("[" ~> """[0-9]+""".r <~ "]") ^^ { case s ~ size => FixedLengthArrayType(s, size.toInt) }
  def tuple: Parser[Type] = "(" ~> repsep(anyType, ",") <~ ")" ^^ { l => TupleType(l) }
  def anyType: Parser[Type] = array | fixedArray | tuple | simple
  def params: Parser[List[Type]] = "(" ~> repsep(anyType, ",") <~ ")"
  def signature = functionName ~ params

  def apply(input: String) = parseAll(signature, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }
}