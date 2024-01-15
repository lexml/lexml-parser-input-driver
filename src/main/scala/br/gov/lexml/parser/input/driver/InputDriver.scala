package br.gov.lexml.parser.input.driver

import org.w3c.dom.Document
import zio.*

import java.lang.reflect.InvocationTargetException
import scala.util.matching.Regex
import scala.xml.Elem

type InputDriverClassName = String
type MediaTypeRegex = String
type InputDriverName = String
type MediaType = String
trait InputConversionError:
  def userMessage : String
  def systemMessage : String
  def underlyingException : Option[Throwable] = None

case class UnsuportedMediaType(mediaType : Option[MediaType],driverClass : Class[_ <: InputDriver]) extends InputConversionError:
  override def userMessage: String =
    s"""Erro de sistema. Por favor contate o desenvolvedor. Tipo de
       | documento ($mediaType) não suportado pelo driver de
       | entrada.""".stripMargin

  override def systemMessage: String =
    s"""Tipo de documento ($mediaType) não suportado pelo driver de
    | entrada (${driverClass.getName}).""".stripMargin

case class InvalidDriverRegistryConfiguration(error: Config.Error) extends InputConversionError:
  override def userMessage: String =
    s"""Erro de sistema. Por favor contacte o desenvolvedor e informe o seguinte:
       | Erro na configuração do registro de drivers de entrada: ${error.getMessage()}""".stripMargin

  override def systemMessage: String =
    s"""Erro na configuração do registro de drivers de entrada: ${error.getMessage()}""".stripMargin

  override def underlyingException: Option[Throwable] = Some(error)

case class InvalidMediaTypeForConverter(inputDriver : InputDriver, mediaType : Option[MediaType]) extends InputConversionError:

  def userMessage: String =
    s"""Erro de sistema. Por favor contacte o desenvolvedor e informe o seguinte:
       | Media type inválido usado em converter: $mediaType,
       | driver: ${inputDriver.name}""".stripMargin

  override def systemMessage: String =
    s"""Media type inválido usado em converter: $mediaType,
       | driver: ${inputDriver.name}""".stripMargin

enum InputConversionResult:
  case ScalaXmlResult(bodyElements : Seq[Elem]) extends InputConversionResult
  case ByteArrayResult(result : Array[Byte]) extends InputConversionResult
  case StringResult(result : String) extends InputConversionResult
  case DOMResult(result : Document) extends InputConversionResult

trait InputDriver:
  def name : String
  def description : String
  def acceptMediaTypeRegexps : Set[Regex]
  def converterFor(mimeType : Option[MediaType] = None) : IO[InputConversionError,Option[InputConverter]]

trait InputConverter:
  def convert(input : Array[Byte], mediaType : Option[MediaType] = None) : IO[InputConversionError,InputConversionResult]


final case class InputDriverRegistryConfiguration(
                                                   drivers : Map[InputDriverName,InputDriverClassName] = Map(),
                                                   mediaTypes : List[(MediaTypeRegex,List[InputDriverName])] = List.empty
                                                 )

object InputDriverRegistryConfiguration:
  val config : Config[InputDriverRegistryConfiguration] = (
    Config.table(Config.string) ++
      Config.listOf(Config.string ++ Config.listOf(Config.string))
  ).map(InputDriverRegistryConfiguration.apply.tupled)

trait InputDriverRegistry:
  def drivers : Map[InputDriverName,InputDriver]
  def handlers : List[(Regex,List[InputDriver])]
  def converterFor(mediaType : Option[MediaType] = None) : IO[InputConversionError,Option[InputConverter]]

object InputDriverRegistry:
  class InvalidDriverClass(driverName : String, className : String, reason : String,
                                cause : Option[Throwable] = None) extends InputConversionError:
    override def userMessage =
      s"""Ocorreu um erro de sistema. Favor contactar o desenvolvedor: "A classe do driver de entrada '$driverName é inválida: '$className'."""
    override def systemMessage: String =
      s"""O driver de entrada $driverName está associado a um classe inválida: '$className'. Razão: $reason"""

    override def underlyingException: Option[Throwable] = cause

  class InvalidDriverNameInHandler(handlerMediaTypeRegex : String, invalidDriverNames: Set[String]) extends InputConversionError:
    override def userMessage =
      s"""Ocorreu um erro de sistema. Favor contactar o desenvolvedor: "Nome de drivers de entrada inválidos ($invalidDriverNames) em especificação de Handler"."""

    override def systemMessage: String =
      s"""Foram especificados um ou mais nomes de drivers de entrada
         | inválidos ($invalidDriverNames) para o handler com a
         | expressão '$handlerMediaTypeRegex'""".stripMargin

  def layer : ZLayer[Any,InputConversionError,InputDriverRegistry] =
    val cl = Thread.currentThread().getContextClassLoader
    def loadDriver(driverName : String, className : String) =
      def error(reason : String, cause : Option[Throwable] = None) =
        InvalidDriverClass(driverName,className,reason,cause)
      def loadDriverClass = for {
        clazz <-
          ZIO.attempt[Class[_]](cl.loadClass(className))
            .refineOrDie {
              case _ : ClassNotFoundException => error(s"A classe não foi encontrada.")
            }
        clazz1 <- ZIO.attempt(clazz.asSubclass(classOf[InputDriver]))
          .refineOrDie {
            case _ : ClassCastException => error(s"A classe não implementa a interface ${classOf[InputDriver].getCanonicalName}")
          }
      } yield clazz1
      def loadInstance(clazz : Class[_ <: InputDriver]) = for {
        cons <- ZIO.attempt(clazz.getConstructor()).refineOrDie {
          case _: NoSuchMethodException => error("A classe não possui um construtor sem parâmetros")
          case ex: SecurityException => error(s"O construtor sem parâmetros da classe não é acessível: ${ex.getMessage}",Some(ex))
        }
        inst <- ZIO.attempt(cons.newInstance()).refineOrDie {
          case ex : (InstantiationException | IllegalAccessException
                  | IllegalArgumentException | InvocationTargetException) =>
            error(s"Não foi possível instanciar o driver: ${ex.getMessage}",Some(ex))
        }
      } yield inst
      loadDriverClass.flatMap(loadInstance)
    def loadDrivers(cfg : InputDriverRegistryConfiguration) : ZIO[Any,InputConversionError,Map[InputDriverName,InputDriver]] =
      ZIO.collect(cfg.drivers) { (driverName, driverClass) =>
        loadDriver(driverName, driverClass)
          .mapError(Some(_))
          .map(driver => (driverName, driver))
      }
    def loadHandlers(cfg : InputDriverRegistryConfiguration, driverMap : Map[InputDriverName,InputDriver]) : ZIO[Any,InputConversionError,List[(Regex,List[InputDriver])]] =
      ZIO.collect(cfg.mediaTypes) { (mediaRegexText,driverNames) =>
        val re = mediaRegexText.r
        val invalidDriverNames = driverNames.toSet -- driverMap.keySet
        val validDriverNames = driverNames.filter(driverMap.contains)
        val validDrivers = validDriverNames.flatMap(driverMap.get)
        if invalidDriverNames.isEmpty then
          ZIO.succeed((re,validDrivers))
        else
          ZIO.fail(Some(InvalidDriverNameInHandler(mediaRegexText,invalidDriverNames)))
      }
    ZLayer {
      for {
        cfg <- ZIO.config(InputDriverRegistryConfiguration.config)
          .mapError { err => InvalidDriverRegistryConfiguration(err) }
        loadedDrivers <- loadDrivers(cfg)
        loadedHandlers <- loadHandlers(cfg, loadedDrivers)
      } yield InputDriverRegistryImpl(loadedDrivers, loadedHandlers)
    }
end InputDriverRegistry

class InputDriverRegistryImpl(
       override val drivers : Map[InputDriverName,InputDriver],
       override val handlers : List[(Regex,List[InputDriver])]) extends InputDriverRegistry:
  override def converterFor(mediaType: Option[MediaType] = None): IO[InputConversionError, Option[InputConverter]] =
    val mt = mediaType.getOrElse("")

    def findConverter(nextHandlers: List[(Regex, List[InputDriver])], nextDrivers: List[InputDriver] = List.empty): IO[InputConversionError, Option[InputConverter]] =
      nextDrivers match {
        case Nil => nextHandlers match {
          case Nil => ZIO.succeed(None)
          case (regex, drivers) :: moreHandlers if regex.matches(mt) =>
            findConverter(moreHandlers, drivers)
          case _ => ZIO.succeed(None)
        }
        case driver :: moreDrivers
          if driver.acceptMediaTypeRegexps.exists(_.matches(mt)) =>
          for {
            oConverter <- driver.converterFor(mediaType)
            res <-
              if oConverter.isEmpty then
                findConverter(nextHandlers, moreDrivers)
              else
                ZIO.succeed(oConverter)
          } yield res
        case _ => ZIO.succeed(None)
      }

    findConverter(handlers.to(List), List.empty)
end InputDriverRegistryImpl
