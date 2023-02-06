import org.bson.BsonValue

trait Encoder[T]:
  def encode(value: T): BsonValue

object Encoder extends EncoderSyntax with EncoderInstances

trait Decoder[T]:
  def decode(bson: BsonValue): Option[T]

object Decoder extends DecoderSyntax with DecoderInstances
