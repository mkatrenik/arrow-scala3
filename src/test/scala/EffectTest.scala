import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ox.Ox.{fork, forkHold, scoped}

import java.util.concurrent.CompletableFuture

def unreachable[T](args: Any*): Nothing = throw new RuntimeException("unreachable")
def unreachable[T](arg: T): Nothing = throw new RuntimeException("unreachable")

class EffectTest extends AnyWordSpec with Matchers {
  "try/catch - can recover from raise" in {
    effect {
      try raise("boom")
      catch case e: Throwable => "ok"
    }.getOrElse(unreachable) should be("ok")
  }

  "try/catch - finally works" in {
    val promise = CompletableFuture[Int]()
    effect {
      try raise("boom")
      finally promise.complete(1)
    }.fold(identity, identity) should be("boom")
    promise.get() should be(1)
  }

  "try/catch - First raise is ignored and second is returned" in {
    effect {
      try raise("boom")
      catch case e: Throwable => 1
      raise("boom2")
    }.fold(identity, identity) should be("boom2")
  }

  "recover - raise" in {
    effect {
      effect {
        raise("boom")
      }.getOrElse { e =>
        e should be("boom")
        1
      }
    }.getOrElse(unreachable) should be(1)
  }

  "recover - raise and transform error" in {
    effect {
      effect {
        raise("boom")
      }.getOrElse { e =>
        e should be("boom")
        raise("boom2")
      }
    }.fold(identity, identity) should be("boom2")
  }

  "recover - success" in {
    effect {
      effect { 1 }.getOrElse(unreachable)
    }.getOrElse(unreachable) should be(1)
  }

  val boom = new RuntimeException("boom")

  "recover + catch - throw and recover" in {
    effect { throw boom }
      .catchAll { e =>
        e should be(boom)
        1
      }
      .getOrElse(unreachable) should be(1)
  }

  "recover + catch - throw and transform error" in {
    effect { throw boom }
      .catchAll { e =>
        e should be(boom)
        raise("boom2")
      }
      .fold(identity, unreachable) should be("boom2")
  }

  "recover + catch - raise and throw" in {
    effect { raise(5) }
      .recover { e =>
        e should be(5)
        throw boom
        raise("boom2")
      }
      .fold(
        identity,
        _ => unreachable(),
        _ => unreachable()
      ) should be(boom)
  }

  "recover + catch - throw and throw" in {
    val boom2 = ArithmeticException("boom2")
    effect { throw boom }
      .catchAll { e =>
        e should be(boom)
        throw boom2
      }
      .fold(
        identity,
        _ => unreachable(),
        _ => unreachable()
      ) should be(boom2)
  }

//  "catch - throw and transform error"
//  "catch - throw and recover"

  "eagerEffect can be consumed within an Effect computation" in {
    effect {
      val x = eagerEffect(1).bind()
      x + 1
    }.getOrElse(unreachable) should be(2)
  }

  "eagerEffect raise short-circuits effect computation" in {
    effect {
      val x = eagerEffect[String, Int] { raise("boom") }.bind()
      x + 1
    }.fold(identity, unreachable) should be("boom")
  }

  "eagerEffect can be consumed within an Effect computation with bind" in {
    effect {
      val x = eagerEffect(1).bind()
      x + 1
    }.getOrElse(unreachable) should be(2)
  }

//  "eagerEffect raise short-circuits effect computation  with bind"
// "success"
// "short-circuit"

  "Rethrows exceptions" in {
    assertThrows[RuntimeException] {
      effect {
        throw boom
      }.toEither
    }
  }

  "Can short-circuit from nested blocks" in {
    effect {
      effect {
        raise("boom")
      }.bind()
    }.fold(identity, identity) should be("boom")
  }

  "toOption works" in {
    effect { 1 }.toOption should be(Some(1))
  }

  "toOption works with raise" in {
    effect { raise("boom") }.toOption should be(None)
  }

  "toEither works" in {
    effect { 1 }.toEither should be(Right(1))
  }

  "toEither works with raise" in {
    effect { raise("boom") }.toEither should be(Left("boom"))
  }

//  "Can short-circuit immediately after suspending from nested blocks"

  "effect + fork works" in {
    effect {
      fork { 1 }.join()
    }.getOrElse(unreachable) should be(1)
  }

  "fork works with raise" in {
    effect {
      fork { raise("boom") }.join()
    }.fold(identity, identity) should be("boom")
  }

//  flaky
//  "fork throws unhandled exception" in {
//    try
//      effect {
//        fork { throw boom }.join()
//      }.toEither
//    catch
//      case e: InterruptedException => println(s"> $e")
//      case e: Throwable            => println(s">>> $e")
//  }

  "forkHold works with catchAll" in {
    effect {
      forkHold {
        throw boom
      }.join()
    }.catchAll { e =>
      println(e)
      e should be(boom)
      1
    }.getOrElse(unreachable) should be(1)
  }
}
