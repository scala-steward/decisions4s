package decisions4s.persistence.feel

import scala.reflect.ClassTag

/** Converts FEEL evaluation results to Scala types.
  *
  * FEEL uses BigDecimal for all numbers, which is different from CEL. This trait handles the conversion from FEEL's internal representation to the
  * expected Scala types.
  */
trait FromFeel[T] {
  def read: Any => T
}

object FromFeel {

  def unexpected[T: ClassTag](value: Any): T =
    throw new IllegalArgumentException(
      s"Expected ${summon[ClassTag[T]].runtimeClass.getSimpleName} but got: $value (${if (value == null) "null" else value.getClass.getName})",
    )

  given FromFeel[Int] with {
    // FEEL numbers are BigDecimal
    def read: Any => Int = {
      case n: java.math.BigDecimal => n.intValue()
      case n: BigDecimal           => n.toInt
      case n: Number               => n.intValue()
      case other                   => unexpected[Int](other)
    }
  }

  given FromFeel[Long] with {
    def read: Any => Long = {
      case n: java.math.BigDecimal => n.longValue()
      case n: BigDecimal           => n.toLong
      case n: Number               => n.longValue()
      case other                   => unexpected[Long](other)
    }
  }

  given FromFeel[Double] with {
    def read: Any => Double = {
      case n: java.math.BigDecimal => n.doubleValue()
      case n: BigDecimal           => n.toDouble
      case n: Number               => n.doubleValue()
      case other                   => unexpected[Double](other)
    }
  }

  given FromFeel[Float] with {
    def read: Any => Float = {
      case n: java.math.BigDecimal => n.floatValue()
      case n: BigDecimal           => n.toFloat
      case n: Number               => n.floatValue()
      case other                   => unexpected[Float](other)
    }
  }

  given FromFeel[String] with {
    def read: Any => String = {
      case s: String => s
      case other     => unexpected[String](other)
    }
  }

  given FromFeel[Boolean] with {
    def read: Any => Boolean = {
      case b: Boolean => b
      case other      => unexpected[Boolean](other)
    }
  }

}
