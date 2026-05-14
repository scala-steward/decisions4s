package decisions4s.persistence.jsonlogic

import scala.reflect.ClassTag

trait FromJsonLogic[T] {
  def read: Any => T
}

object FromJsonLogic {

  def unexpected[T: ClassTag](value: Any): T =
    throw new IllegalArgumentException(
      s"Expected ${summon[ClassTag[T]].runtimeClass.getSimpleName} but got: $value (${if (value == null) "null" else value.getClass.getName})",
    )

  given FromJsonLogic[Int] with {
    def read: Any => Int = {
      case n: java.lang.Double  => n.intValue()
      case n: java.lang.Integer => n.intValue()
      case n: java.lang.Long    => n.intValue()
      case n: Number            => n.intValue()
      case other                => unexpected[Int](other)
    }
  }

  given FromJsonLogic[Long] with {
    def read: Any => Long = {
      case n: java.lang.Double  => n.longValue()
      case n: java.lang.Integer => n.longValue()
      case n: java.lang.Long    => n.longValue()
      case n: Number            => n.longValue()
      case other                => unexpected[Long](other)
    }
  }

  given FromJsonLogic[Double] with {
    def read: Any => Double = {
      case n: java.lang.Double  => n.doubleValue()
      case n: java.lang.Integer => n.doubleValue()
      case n: java.lang.Long    => n.doubleValue()
      case n: Number            => n.doubleValue()
      case other                => unexpected[Double](other)
    }
  }

  given FromJsonLogic[Float] with {
    def read: Any => Float = {
      case n: java.lang.Double  => n.floatValue()
      case n: java.lang.Integer => n.floatValue()
      case n: java.lang.Long    => n.floatValue()
      case n: Number            => n.floatValue()
      case other                => unexpected[Float](other)
    }
  }

  given FromJsonLogic[String] with {
    def read: Any => String = {
      case s: String => s
      case other     => unexpected[String](other)
    }
  }

  given FromJsonLogic[Boolean] with {
    def read: Any => Boolean = {
      case b: Boolean => b
      case other      => unexpected[Boolean](other)
    }
  }

}
