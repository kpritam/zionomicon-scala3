package zionomicon.ch4

import zio._
import zio.console._

def failWithMessage(string: String) = 
  ZIO.succeed(throw new Error(string))
    .catchAllCause(_ => ZIO.unit)

def recoverFromSomeDefects[R, E, A](zio: ZIO[R, E, A])(f: Throwable => Option[A]): ZIO[R, E, A] = 
  zio.foldCauseM (
    _.defects.map(f).find(_.isDefined).fold(zio)(a => ZIO.succeed(a.get)), 
    _ => zio
  )

def logFailures[R, E, A](zio: ZIO[R, E, A]) = 
  zio.tapError(e =>  putStrLn(e.toString)).tapCause(c => putStrLn(c.prettyPrint))
    
def onAnyFailure[R, E, A](zio: ZIO[R, E, A], handler: ZIO[R, E, Any]): ZIO[R, E, A] = 
  zio.foldCauseM(_ => handler.flatMap(_ => zio), _ => zio)

def ioException[R, A](zio: ZIO[R, Throwable, A]): ZIO[R, java.io.IOException, A] = 
  zio.refineOrDie { case e: java.io.IOException => e }

val parseNumber: ZIO[Any, NumberFormatException, Int] = ZIO.effect("foo".toInt).refineToOrDie[NumberFormatException]

def left[R, E, A, B](zio: ZIO[R, E, Either[A, B]]): ZIO[R, Either[E, B], A] = 
  zio.foldM(e => ZIO.fail(Left(e)), _.fold(a => ZIO.succeed(a), b => ZIO.fail(Right(b))))

def unleft[R, E, A, B](zio: ZIO[R, Either[E, B], A]): ZIO[R, E, Either[A, B]] = 
  zio.foldM(_.fold(e => ZIO.fail(e), b => ZIO.succeed(Right(b))), a => ZIO.succeed(Left(a)))

def catchAllCause[R, E1, E2, A](zio: ZIO[R, E1, A], handler: Cause[E1] => ZIO[R, E2, A] ): ZIO[R, E2, A] = 
  zio.sandbox.foldM(handler, ZIO.succeed)

def catchAllCause2[R, E1, E2, A](zio: ZIO[R, E1, A], handler: Cause[E1] => ZIO[R, E2,A]): ZIO[R, E2, A] = 
  zio.foldCauseM(handler, ZIO.succeed)
  
object Ch4 extends zio.App:

    def run(args: List[String]): zio.URIO[zio.ZEnv, zio.ExitCode] =
        failWithMessage("Boom!").exitCode
        