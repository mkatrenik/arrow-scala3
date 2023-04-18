import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers.should
import scala.concurrent.duration.Duration
import ox.Ox.timeout

class ScheduleTest extends AnyWordSpec with Matchers {
  def checkRepeat[B](schedule: Schedule[Int, List[B]], expected: List[B]): Unit = {
    var i = 0
    val result = schedule.repeat {
      i += 1; i
    }
    result should be(expected)
  }

  def checkRepeat[B](schedule: Schedule[Int, B], expected: B): Unit = {
    var i = 0
    val result = schedule.repeat {
      i += 1; i
    }
    result should be(expected)
  }

  "identity" in {
    val s = Schedule.identity[String]().calculateCont("test", 100)
    s should be((0 to 99).map(_ => ("test", Duration.Zero)))
  }

  "unfold" in {
    val s = Schedule.unfold(0)(_ + 1).calculateCont(0, 100)
    s should be((0 to 99).map((_, Duration.Zero)))
  }

  "forever" in {
    val s = Schedule.forever[String].calculateCont("test", 100)
    s should be((0 to 99).map((_, Duration.Zero)))
  }

  "recurs" in {
    checkRepeat(Schedule.recurs(10), 10)
    checkRepeat(Schedule.recurs(-10), 0)
    checkRepeat(Schedule.recurs(0), 0)
  }

  "recurs with positive number" in {
    val n = 3
    val s = Schedule.recurs(n).calculateSchedule(0, n + 1)
    s should be(
      // all continues but last one
      (0 until n).map(i => s(i)).toList :+ Schedule.Done(n)
    )
  }

  "doWhile repeats until condition holds" in {
    checkRepeat(Schedule.doWhile[Int]((i, _) => i < 5), 5)
  }

  "doUntil repeats until condition holds" in {
    checkRepeat(Schedule.doUntil[Int]((i, _) => i >= 5), 5)
  }

  "collects all doWhile inputs to a list" in {
    checkRepeat(Schedule.doWhile[Int]((i, _) => i < 5).collect(), (1 to 4).toList)
  }

  "collects all doUntil inputs to a list" in {
    checkRepeat(Schedule.doUntil[Int]((i, _) => i > 5).collect(), (1 to 5).toList)
  }

  "repeat scheduled repeat repeats the whole number" in {
    val n = 42
    var count = 0
    Schedule.recurs(1).repeat {
      Schedule.recurs(n).repeat { count += 1 }
    }
    count should be((n + 1) * 2)
  }

  "spaced" in {
    val d = Duration(1, "second")
    val s = Schedule.spaced[String](d).calculateSchedule("test", 100)
    true should be(s.forall {
      case Schedule.Continue(_, delay, _) => delay == d
      case _                              => false
    })
  }

  "fibonacci" in {
    val n = 10
    val s = Schedule.fibonacci[String](Duration(10, "seconds")).calculateDelay("test", n)

    val d = List(
      Duration(10, "seconds"),
      Duration(10, "seconds"),
      Duration(20, "seconds"),
      Duration(30, "seconds"),
      Duration(50, "seconds"),
      Duration(1, "minute") + Duration(20, "seconds"),
      Duration(2, "minutes") + Duration(10, "seconds"),
      Duration(3, "minutes") + Duration(30, "seconds"),
      Duration(5, "minutes") + Duration(40, "seconds"),
      Duration(9, "minutes") + Duration(10, "seconds")
    )
    s should be(d)
  }

  "linear" in {
    val n = 5
    val s = Schedule.linear[String](Duration(10, "seconds")).calculateDelay("test", n)

    val d = List(
      Duration(10, "seconds"),
      Duration(20, "seconds"),
      Duration(30, "seconds"),
      Duration(40, "seconds"),
      Duration(50, "seconds")
    )
    s should be(d)
  }

  "exponential" in {
    val n = 5
    val s = Schedule.exponential[String](Duration(10, "seconds")).calculateDelay("test", n)

    val d = List(
      Duration(10, "seconds"),
      Duration(20, "seconds"),
      Duration(40, "seconds"),
      Duration(80, "seconds"),
      Duration(160, "seconds")
    )
    s should be(d)
  }

  "repeat is stacksafe" in {
    val n = 100000
    val s = Schedule.recurs(n).repeat(0)
    s should be(n)
  }

  "zipRight works" in {
    var i = 0
    val n = 10
    val s = Schedule.recurs(n).zipRight(Schedule.collect()).repeat { i += 1; i - 1 }
    val x = (0 to n).toList
    s should be(x)
  }

  "retry works" in {
    val ex = Exception("test")
    var count = 0
    val n = 10
    val s = effect[Throwable, Any] {
      Schedule.recurs(n).retry {
        count += 1; throw ex
      }
    }.catchAll(raise(_)).toEither
    s should be(Left(ex))
    count should be(n + 1)
  }

  "retryOrElseEither runs the schedule with correct input And Runs OrElse Handler If It Does Not Retry" in {
    val ex = Exception("test")

    val s: Either[Throwable, Any] = Schedule
      .recurs(0)
      .retryOrElseEither(
        { throw ex },
        { (e, _) => e }
      )
    s.fold(e => e should be(ex), unreachable)
  }

  "stops Retrying If First Of More Predicates Is Met" in {
    val ex = Exception("test")

    val s = Schedule
      .exponential[Throwable](Duration(1, "ms"))
      .doUntil((_, o) => o > Duration(50, "ms"))
      .doUntil((i, _) =>
        i match
          case e: IllegalStateException => true
          case _                        => false
      )
    val result = timeout(Duration(1, "s")) {
      s.retryOrElseEither(
        { throw ex },
        { (e, _) => e }
      )
    }
    result.fold(e => e should be(ex), unreachable)
  }

}

extension [I, A](schedule: Schedule[I, A]) {
  def calculateSchedule(input: I, n: Int): List[Schedule.Decision[I, A]] =
    var step = schedule.step
    (0 until n).foldLeft(List.empty[Schedule.Decision[I, A]]) { (acc, _) =>
      val decision = step(input)
      decision match {
        case c: Schedule.Continue[?, ?] =>
          step = c.step
          acc :+ c
        case _ => acc :+ decision
      }
    }

  def calculateCont(input: I, n: Int): List[(A, Duration)] =
    calculateSchedule(input, n)
      .map {
        case c: Schedule.Continue[?, ?] => Some((c.output, c.delay))
        case _                          => None
      }
      .filter(_.isDefined)
      .map(_.get)

  def calculateDelay(input: I, n: Int): List[Duration] =
    calculateSchedule(input, n)
      .map {
        case c: Schedule.Continue[?, ?] => Some(c.delay)
        case _                          => None
      }
      .filter(_.isDefined)
      .map(_.get)
}

@main def test1 = {
  val n = 10
  val s = Schedule.fibonacci[String](Duration(10, "seconds")).calculateDelay("test", n)
  println(s)
}
