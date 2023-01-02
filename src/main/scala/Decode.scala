import gigahorse.support.okhttp.Gigahorse
import org.web3j.abi.datatypes.Address
import org.web3j.utils.Numeric
import play.api.libs.json.{JsObject, Json}

import java.io.{File, FileWriter}
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.Await
import scala.io.Source

object Decode extends App {

  val fileOrHex = args(0)
  val calldata = if (new File(fileOrHex).exists()) {
    Numeric.cleanHexPrefix(
      Source.fromFile(fileOrHex).getLines().toList.head.replace("\"", "")
    )
  } else {
    Numeric.cleanHexPrefix(fileOrHex)
  }

  val signaturesFile = new File("signatures.dat")

  val cachedSignatures = mutable.Map.empty[String, List[String]]

  def loadSignatures(): Unit = {
    if (signaturesFile.exists()) {
      cachedSignatures ++= Source
        .fromFile(signaturesFile)
        .getLines()
        .toList
        .withFilter(_.nonEmpty)
        .map(_.split('|').toList)
        .groupBy(_ (0))
        .view
        .mapValues(_.map(_ (1)))
        .toMap
    }
  }

  loadSignatures()

  case class Signature(id: Int, text_signature: String)

  implicit val readSignature = Json.reads[Signature]

  implicit val httpClient = Gigahorse.http(Gigahorse.config)
  try {
    val f = decodeFunction(calldata)
    println(f)
  } finally {
    httpClient.close()
  }

  sealed trait Value {
    def format(level: Int): String
  }

  case class SimpleValue(value: Any) extends Value {
    def format(level: Int) = {
      if (value.isInstanceOf[String]) {
        try {
          decodeFunction(value.toString).format(level)
        } catch {
          case e: Throwable =>
            println(e.getMessage)
            value.toString
        }
      } else value.toString
    }

    override def toString = format(0)
  }

  case class TupleValue(values: List[Value]) extends Value {
    def format(level: Int) = values.map(_.format(level)).mkString("(", ", ", ")")

    override def toString = format(0)
  }

  case class ArrayValue(values: List[Value]) extends Value {
    def format(level: Int) = {
      val p = Array.fill(level * 2)(" ").mkString
      val e = Array.fill((level - 1) * 2)(" ").mkString
      values.map(_.format(level)).mkString("[\n" + p, ",\n" + p, "\n" + e + "]")
    }

    override def toString = format(0)
  }

  case class Func(name: String, params: List[Value]) {
    def format(level: Int) = {
      name + params.map(_.format(level + 1)).mkString("(", ", ", ")")
    }

    override def toString = format(0)
  }

  def decodeFunction(data: String): Func = {
    val (function, params) = data.splitAt(8)
    val signature = getFirstSignature(function)
    //println(signature)
    val parsedSignature = SignatureParser(signature)
    //println(parsedSignature)
    val decodedParams = decodeParams(params, parsedSignature._2)
    //println(decodedParams)
    Func(parsedSignature._1, decodedParams)
    //tryDecodeFunction(decodedParams)
  }

  def decodeType(data: String, offset: Int, t: Type): (Value, Int) = {
    t match {
      case SimpleType("address") =>
        (SimpleValue(new Address(data.drop(offset + 24).take(40))), 64)
      case SimpleType("bool") =>
        (SimpleValue(data(offset + 63) == '1'), 64)
      case SimpleType("bytes") =>
        val dynOff = BigInt(data.drop(offset).take(64), 16).toInt
        val len = BigInt(data.drop(dynOff * 2).take(64), 16).toInt
        //println("Offset = " + offset + " DynOff = " + dynOff + " LEN = " + len + " Start = " + data.take(64))
        //sys.exit()
        (SimpleValue(data.drop(dynOff * 2 + 64).take(len * 2)), 64)
      case SimpleType("bytes4") =>
        (SimpleValue(data.take(8)), 64)
      case SimpleType("bytes32") =>
        (SimpleValue(data.take(64)), 64)
      case SimpleType("uint8") =>
        (SimpleValue(BigInt(data.drop(offset).take(64), 16)), 64)
      case SimpleType("uint256") =>
        (SimpleValue(BigInt(data.drop(offset).take(64), 16)), 64)
      case TupleType(types) =>
        val dynOff = BigInt(data.drop(offset).take(64), 16).toInt
        //println("Offset: " + dynOff)
        (TupleValue(decodeParams(data.drop(dynOff * 2), types)), 64)
      case ArrayType(tt) =>
        val dynOff = BigInt(data.drop(offset).take(64), 16).toInt
        //println("Offset: " + dynOff)
        (decodeArray(data.drop(dynOff * 2), tt), 64)
      case FixedLengthArrayType(t, size) =>
        val (values, consumed) = (1 to size).foldLeft((List.empty[Value], 0)) {
          case ((acc, offset), _) =>
            val (v, l) = decodeType(data, offset, t)
            (v :: acc, offset + l)
        }
        (ArrayValue(values.reverse), consumed)
      case _ =>
        sys.error("Unsupported type: " + t)
    }
  }

  def decodeParams(data: String, types: List[Type]): List[Value] = {
    val (values, _) = types.foldLeft((List.empty[Value], 0)) {
      case ((acc, offset), t) =>
        val (decoded, consumed) = decodeType(data, offset, t)
        (decoded :: acc, offset + consumed)
    }
    values.reverse
  }

  def decodeArray(data: String, t: Type) = {
    //println(data.take(64))
    val count = BigInt(data.take(64), 16).intValue
    //println("Count " + count)
    ArrayValue((1 to count).foldLeft((List.empty[Value], 0)) {
      case ((acc, offset), _) =>
        val (v, l) = decodeType(data.drop(64), offset, t)
        (v :: acc, offset + l)
    }._1.reverse)
  }

  def getFirstSignature(
                         hash: String
                       ): String = {
    getSignatures(hash).headOption
      .getOrElse(sys.error("Missing signature: " + hash))
  }

  def getSignatures(
                     hash: String
                   ): List[String] = {
    def fetch(): List[Signature] = {
      require(hash.length == 8, "Invalid function hash")
      val f = httpClient.processFull(
        Gigahorse
          .url("https://www.4byte.directory/api/v1/signatures/")
          .withQueryString("hex_signature" -> ("0x" + hash))
      )
      val r = Await.result(f, 10.seconds)
      r.status match {
        case 200 =>
          (Json.parse(r.bodyAsString).as[JsObject] \ "results")
            .as[List[Signature]]
        case 502 =>
          r.close()
          println("Temporary 4byte gateway error")
          fetch()
        case _ => sys.error("Unexpected 4byte response: " + r.bodyAsString)
      }
    }

    cachedSignatures.getOrElseUpdate(
      hash, {
        val signatures = fetch().sortBy(_.id).map(_.text_signature)
        val out = new FileWriter(signaturesFile, true)
        try {
          signatures.foreach(sig => out.write(s"$hash|$sig\n"))
        } finally {
          out.close()
        }
        signatures
      }
    )
  }

}
