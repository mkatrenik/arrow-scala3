import org.slf4j.LoggerFactory
import ox.Ox.*
import ox.Ox

import scala.language.postfixOps
import org.slf4j.Logger

private val log: Logger = LoggerFactory.getLogger("Effect").nn

class RaiseCancellationException[E](val err: E) extends InterruptedException

trait Raise[E] {
  def raise(err: E) = throw new RaiseCancellationException(err)
}

trait Fold[E, T] {
  def fold[B](onRaise: E => B, onSuccess: T => B): B

  def fold[B](onCatch: Throwable => B, onRaise: E => B, onSuccess: T => B): B =
    log.trace(s"calling fold3 on $this")
    try fold(onRaise, onSuccess)
    catch case e: Throwable => onCatch(e)

  def toEither: Either[E, T] =
    fold[Either[E, T]](err => Left(err), it => Right(it))

  def toOption: Option[T] =
    fold[Option[T]](_ => None, it => Some(it))

  def getOrElse(recover: E => T): T =
    log.trace(s"calling getOrElse on $this")
    fold(recover, identity)
}

type EffectContext[E, T] = Effect[E, T] ?=> Ox ?=> T

class Effect[E, T](f: EffectContext[E, T]) extends Raise[E], Fold[E, T] {

  log.debug(s"created Effect $this")

  def fold[B](onRaise: E => B, onSuccess: T => B): B =
    log.trace(s"calling fold2 on $this")
    try
      val res = scoped(f(using this))
      log.trace(s"result of effect: $res in $this")
      onSuccess(res)
    catch
      case e: RaiseCancellationException[E] => onRaise(e.err)
      case e: Throwable                     => throw e

  def catchAll(cb: Throwable => EffectContext[E, T]): Effect[E, T] = effect {
    log.trace(s"calling catchAll on $this")

    val res = fold(ex => scoped(cb(ex)), err => raise(err), identity)
    log.trace(s"fold result: $res on $this")
    res
  }

  def recover[E2](f: E => EffectContext[E2, T]): Effect[E2, T] = effect { fold(err => f(err), identity) }

  def bind(): T = fold(err => raise(err), identity)
}

type EagerEffectContext[E, T] = EagerEffect[E, T] ?=> T

class EagerEffect[E, T](f: EagerEffectContext[E, T]) extends Raise[E], Fold[E, T] {

  def fold[B](onRaise: E => B, onSuccess: T => B): B =
    try onSuccess(f(using this))
    catch
      case e: RaiseCancellationException[E] => onRaise(e.err)
      case e: Throwable                     => throw e

  def catchAll(f: Throwable => EagerEffectContext[E, T]): EagerEffect[E, T] = eagerEffect {
    fold(ex => f(ex), err => raise(err), identity)
  }

  def recover[E2](f: E => EagerEffectContext[E2, T]) = eagerEffect { fold(err => f(err), identity) }

  def bind() = fold(err => raise(err), identity)
}

def raise[E](err: E)(using eff: Raise[E]): Nothing =
  eff.raise(err)

def effect[E, T](f: EffectContext[E, T]): Effect[E, T] =
  Effect[E, T](f)

def eagerEffect[E, T](f: EagerEffectContext[E, T]): EagerEffect[E, T] =
  EagerEffect[E, T](f)

extension [E, B](either: Either[E, B])(using eff: Effect[E, B])
  def bind(): B =
    either match
      case Left(err)  => eff.raise(err)
      case Right(res) => res

extension [E, E2, B](either: Either[E, B])(using eff: Effect[E2, B])
  def bind(err: E2): B =
    either.getOrElse(eff.raise(err))

extension [E, T](option: Option[T])(using eff: Effect[E, T])
  def bind(err: E): T =
    option.getOrElse(raise(err))

extension [T](option: Option[T])(using eff: Effect[Option[Nothing], T])
  def bind(): T =
    option.getOrElse(eff.raise(None))
