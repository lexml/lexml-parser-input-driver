package br.gov.lexml.parser.input.driver.test

import br.gov.lexml.parser.input.driver.*
import zio.*

import scala.util.matching.Regex

/** This input driver returns a converter that always fails.
 *
 * When asked for a converter, the driver returns a converter that always
 * fails with an error ([[AlwaysFailingInputDriver.Error]]). The media
 * types accepted by the driver can be controlled using the [[acceptMediaTypeRegexps]]
 * parameter.
 *
 * @param acceptMediaTypeRegexps Set o regular expressions that specify the media types
 *                               that should be accepted by this driver.
 */
final class AlwaysFailingInputDriver(
                                 override val acceptMediaTypeRegexps : Set[Regex] = Set(".*".r)
                                 ) extends InputDriver:
  override def name: String = "always-failing-input-driver"

  override def description: String = "A LexML Input Driver that always fails"

  override def converterFor(mediaType: Option[MediaType]): IO[InputConversionError, Option[InputConverter]] =
    if acceptMediaTypeRegexps.exists(_.matches(mediaType.getOrElse(""))) then
      ZIO.succeed(Some(
        (input: Array[Byte], mediaType: Option[MediaType]) => ZIO.fail(AlwaysFailingInputDriver.Error())
      ))
    else
      ZIO.succeed(None)

object AlwaysFailingInputDriver:
  class Error extends InputConversionError:
    override def userMessage: String = "This is the user message for an input conversion error"

    override def systemMessage: String = "This is a system message for an input conversion error"


/** A driver that returns the input document as is.
 *
 * @param acceptMediaTypeRegexps Set o regular expressions that specify the media types
 *                               that should be accepted by this driver.
 */
class IdentityInputDriver(
                           override val acceptMediaTypeRegexps : Set[Regex] = Set(".*".r)
                         ) extends InputDriver:
  override def name: String = "identity-input-driver"

  override def description: String = "A LexML Input Driver that returns the input document as is"

  override def converterFor(mediaType: Option[MediaType]): IO[InputConversionError, Option[InputConverter]] =
    if acceptMediaTypeRegexps.exists(_.matches(mediaType.getOrElse(""))) then
      ZIO.succeed(Some(
        (input: Array[Byte], mediaType: Option[MediaType]) =>
          if acceptMediaTypeRegexps.exists(_.matches(mediaType.getOrElse(""))) then
            ZIO.succeed(InputConversionResult.ByteArrayResult(input))
          else
            ZIO.fail(InvalidMediaTypeForConverter(this,mediaType))
      ))
    else
      ZIO.succeed(None)

/** A driver that always returns a pre-specified result.
 *
 * @param result pre-specified input conversion result
 * @param acceptMediaTypeRegexps Set o regular expressions that specify the media types
 *                               that should be accepted by this driver.
 */
class ConstantInputDriver(
                           val result : InputConversionResult,
                           override val acceptMediaTypeRegexps : Set[Regex] = Set(".*".r)
                         ) extends InputDriver:
  override def name: String = "constant-input-driver"

  override def description: String = "A LexML Input Driver that always returns a pre-specified result"

  override def converterFor(mediaType: Option[MediaType]): IO[InputConversionError, Option[InputConverter]] =
    if acceptMediaTypeRegexps.exists(_.matches(mediaType.getOrElse(""))) then
      ZIO.succeed(Some(
        (_ : Array[Byte], mediaType: Option[MediaType]) =>
          if acceptMediaTypeRegexps.exists(_.matches(mediaType.getOrElse(""))) then
            ZIO.succeed(result)
          else
            ZIO.fail(InvalidMediaTypeForConverter(this,mediaType))
      ))
    else
      ZIO.succeed(None)