package io.github.mkatrenik.arrow

import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.concurrent.duration.Duration

type ScheduleStep[Input, Output] = Input => Schedule.Decision[Input, Output]

class Schedule[-Input, +Output](val step: ScheduleStep[Input, Output]) {
  def repeat(block: => Input): Output = {
    repeatOrElse(block, { (e, _) => throw e })
  }

  def repeatOrElse(block: => Input, orElse: (Throwable, Option[Output]) => Output @uncheckedVariance): Output = {
    repeatOrElseEither(block, orElse).merge
  }

  def repeatOrElseEither[A](block: => Input, orElse: (Throwable, Option[Output]) => A): Either[A, Output] = {
    var step = this.step
    var state: Option[Output] = None

    while true do
      try
        val a = block
        val decision = step(a)
        decision match {
          case d: Schedule.Done[?] => return Right(d.output)
          case c: Schedule.Continue[?, ?] =>
            if c.delay != Duration.Zero then Thread.sleep(c.delay.toMillis)
            state = Some(c.output)
            step = c.step
        }
      catch case e: Throwable => return Left(orElse(e, state))

    Left(orElse(new RuntimeException("Unreachable"), state))
  }

  def doWhile(predicate: (Input @uncheckedVariance, Output @uncheckedVariance) => Boolean): Schedule[Input, Output] = {
    def loop(input: Input, self: ScheduleStep[Input, Output]): Schedule.Decision[Input, Output] =
      val decision = self(input)
      decision match
        case d: Schedule.Done[?] => d
        case c: Schedule.Continue[?, ?] =>
          if predicate(input, c.output) then Schedule.Continue(c.output, c.delay, { i => loop(i, c.step) })
          else Schedule.Done(c.output)

    new Schedule[Input, Output]({ i => loop(i, step) })
  }

  def doUntil(predicate: (Input @uncheckedVariance, Output @uncheckedVariance) => Boolean): Schedule[Input, Output] =
    doWhile((i, o) => !predicate(i, o))

  def collect(): Schedule[Input, List[Output]] =
    fold(List.empty[Output])((acc, o) => acc :+ o)

  def fold[B](b: B)(f: (B, Output) => B): Schedule[Input, B] = {
    def loop(input: Input, b: B, self: ScheduleStep[Input, Output]): Schedule.Decision[Input, B] =
      val decision = self(input)
      decision match
        case _: Schedule.Done[?] => Schedule.Done(b)
        case c: Schedule.Continue[?, ?] =>
          val b2 = f(b, c.output)
          Schedule.Continue(b2, c.delay, { i => loop(i, b2, c.step) })

    new Schedule({ i => loop(i, b, step) })
  }

  def zipRight[B](that: Schedule[Input @uncheckedVariance, B]): Schedule[Input, B] =
    and(that, (_, b) => b)

  def zipLeft[B](that: Schedule[Input @uncheckedVariance, B]): Schedule[Input, Output] =
    and(that, (a, _) => a)

  def and[B, C](
      other: Schedule[Input @uncheckedVariance, B],
      transform: (Output, B) => C,
      combineDuration: (Duration, Duration) => Duration
  ): Schedule[Input, C] =
    Schedule({ i =>
      step(i).and(
        other.step(i),
        transform,
        combineDuration
      )
    })

  def and[B, C](other: Schedule[Input @uncheckedVariance, B], transform: (Output, B) => C): Schedule[Input, C] =
    and(other, transform, (left, right) => left.max(right))
}

object Schedule:
  def identity[Input](): Schedule[Input, Input] =
    def loop(input: Input): Decision[Input, Input] = Continue(input, Duration.Zero, loop)
    new Schedule[Input, Input](loop)

  def collect[Input](): Schedule[Input, List[Input]] =
    identity[Input]().collect()

  def unfold[Input, Output](initial: Output)(next: Output => Output): Schedule[Input, Output] =
    def loop(input: Output): Decision[Input, Output] = Continue(input, Duration.Zero, { _ => loop(next(input)) })
    new Schedule[Input, Output]({ _ => loop(initial) })

  def forever[Input]: Schedule[Input, Int] =
    unfold(0)(_ + 1)

  def recurs[Input](n: Int): Schedule[Input, Int] =
    def loop(i: Int): Decision[Input, Int] =
      if i < n then Continue(i, Duration.Zero, { _ => loop(i + 1) })
      else Done(i)
    new Schedule[Input, Int]({ _ => loop(0) })

  def doWhile[Input](condition: (Input, Input) => Boolean): Schedule[Input, Input] =
    identity[Input]().doWhile(condition)

  def doUntil[Input](condition: (Input, Input) => Boolean): Schedule[Input, Input] =
    identity[Input]().doUntil(condition)

  def spaced[Input](delay: Duration): Schedule[Input, Int] =
    def loop(i: Int): Decision[Input, Int] = Continue(i, delay, { _ => loop(i + 1) })
    new Schedule[Input, Int]({ _ => loop(0) })

  def fibonacci[Input](one: Duration): Schedule[Input, Duration] = {
    def loop(prev: Duration, curr: Duration): Decision[Input, Duration] =
      Continue(curr, curr, { _ => loop(curr, prev + curr) })
    new Schedule({ _ => loop(Duration(0, "ns"), one) })
  }

  def linear[Input](base: Duration): Schedule[Input, Duration] = {
    def loop(i: Int): Decision[Input, Duration] = Continue(base * i, base * i, { _ => loop(i + 1) })
    new Schedule[Input, Duration]({ _ => loop(1) })
  }

  def exponential[Input](base: Duration, factor: Double = 2.0): Schedule[Input, Duration] = {
    def loop(i: Int): Decision[Input, Duration] =
      val next = base * math.pow(factor, i)
      Continue(next, next, { _ => loop(i + 1) })
    new Schedule[Input, Duration]({ _ => loop(0) })
  }

  case class Done[+Output](override val output: Output) extends Decision[Any | Null, Output]
  case class Continue[-Input, +Output](override val output: Output, delay: Duration, step: ScheduleStep[Input, Output])
      extends Decision[Input, Output]

  trait Decision[-Input, +Output] {
    val output: Output

    def recursiveMap(
        transform: Decision[Input, Output] => Decision[Input @uncheckedVariance, Output @uncheckedVariance]
    ): Decision[Input, Output] = {
      val next = transform(this)
      next match {
        case n: Continue[Input, Output] => Continue(n.output, n.delay, { n.step(_) recursiveMap transform })
        case n: Done[Output]            => n
      }
    }

    def delayed(transform: (Output, Duration) => Duration): Decision[Input, Output] = this match {
      case c: Continue[?, ?] => Continue(c.output, transform(c.output, c.delay), c.step)
      case d: Done[?]        => Done(d.output)
    }

    def contramap[A](f: A => Input): Decision[A, Output] = this match {
      case c: Continue[?, ?] => Continue(c.output, c.delay, { i => c.step(f(i)).contramap(f) })
      case d: Done[Output]   => Done[Output](d.output)
    }

    def and[B, C](
        other: Schedule.Decision[Input @uncheckedVariance, B],
        transform: (Output, B) => C,
        combineDuration: (Duration, Duration) => Duration
    ): Schedule.Decision[Input, C] =
      (this, other) match
        case (a: Schedule.Continue[Input, Output], b: Schedule.Continue[Input, B]) =>
          Schedule.Continue(
            transform(a.output, b.output),
            combineDuration(a.delay, b.delay),
            { i =>
              a.step(i).and(b.step(i), transform, combineDuration)
            }
          )
        case _ => Schedule.Done(transform(this.output, other.output))
  }

extension [Input, Output, A](s: Schedule[Throwable, Output])
  def retryOrElseEither(
      action: => Input,
      orElse: (Throwable, Output) => A
  ): Either[A, Input] = {
    @tailrec
    def loop(step: ScheduleStep[Throwable, Output]): Either[A, Input] =
      try Right(action)
      catch
        case e: Throwable =>
          step(e) match
            case Schedule.Done(output) => Left(orElse(e, output))
            case Schedule.Continue(_, delay, next) =>
              if delay != Duration.Zero then Thread.sleep(delay.toMillis)
              loop(next)

    loop(s.step)
  }

extension [Input, Output](s: Schedule[Throwable, Output])
  def retryOrElse(
      action: => Input,
      orElse: (Throwable, Output) => Input
  ): Input =
    s.retryOrElseEither(action, orElse).merge

extension (s: Schedule[Throwable, ?])
  def retry[A](action: => A): A =
    def orElse(e: Throwable, o: Any): A = throw e
    s.retryOrElse(action, orElse)

@main def test = {
  var i = 0
  val schedule = Schedule.doUntil[Int]((i, _) => i == 5).collect()
  val result = schedule.repeat {
    println("Hello")
    i += 1
    i
  }
  println(result)

}
