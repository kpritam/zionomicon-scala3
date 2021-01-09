package zionomicon.ch2

import zio._
import zio.console._

// 1
def readFile(file: String): String = 
    val source = scala.io.Source.fromFile(file)
    try source.getLines.mkString 
    finally source.close() 

def readFileZio(file: String): Task[String] = ZIO.effect(readFile(file))

// 2
def writeFile(file: String, text: String): Unit = 
    import java.io._
    val pw = new PrintWriter(new File(file))
    try pw.write(text) finally pw.close

def writeFileZio(file: String, text: String): Task[Unit] = ZIO.effect(writeFile(file, text))

// 3
def copyFileZio(source: String, dest: String): Task[Unit] =
    for
        content <- readFileZio(source)
        _       <- writeFileZio(dest, content)
    yield ()

object ZioExercises extends zio.App:
    def program123 =   
        for
            content <- copyFileZio("build.sbt", "/tmp/build.sbt.cpy")
            copied  <- readFileZio("/tmp/build.sbt.cpy")
            _       <- putStrLn(copied)
        yield ()

    def run(args: List[String]): zio.URIO[zio.ZEnv, zio.ExitCode] =
        program123.exitCode
        
// 6, 7, 8, 9
object Exercise6:
    final case class ZIO[-R, +E, +A](run: R => Either[E, A])
  
    def zipWith[R, E, A, B, C](self: ZIO[R, E, A], that: ZIO[R, E, B])(f: (A, B) => C): ZIO[R, E, C] =
        ZIO { r =>
            val fst = self.run(r)
            val snd = that.run(r)
            fst.flatMap(a => snd.map(b => f(a, b)))
        }

    def collectAll[R, E, A](in: Iterable[ZIO[R, E, A]]): ZIO[R, E, List[A]] = 
        ZIO { r =>
            def go(effects: List[ZIO[R, E, A]], acc: List[A]): Either[E, List[A]] =
                effects match 
                    case Nil    => Right(acc)
                    case x::xs  => x.run(r).flatMap(v => go(xs, v :: acc))
                    
            go(in.toList, Nil)
        }

    def foreach[R, E, A, B](in: Iterable[A])(f: A => ZIO[R, E, B]): ZIO[R, E, List[B]] =
        collectAll(in.map(f))
    
    def orElse[R, E1, E2, A](self: ZIO[R, E1, A], that: ZIO[R, E2, A]): ZIO[R, E2, A] =
        ZIO { r => self.run(r).left.flatMap(_ => that.run(r)) }

// 10
object Cat extends zio.App:
    def program(fileNames: List[String]) = 
        ZIO.foreach(fileNames)(readFileZio(_).flatMap(putStrLn(_)))

    def run(args: List[String]) = program(args).exitCode

// 11
def eitherToZIO[E, A](either: Either[E, A]): ZIO[Any, E, A] = 
    either match 
        case Left(e)    => ZIO.fail(e)
        case Right(r)   => ZIO.succeed(r)

// 12
def listToZIO[A](list: List[A]): ZIO[Any, None.type, A] = 
    list match
        case Nil    => ZIO.fail(None)
        case x :: _ => ZIO.succeed(x)

// 13
def currentTime(): Long = System.currentTimeMillis()
lazy val currentTimeZIO: ZIO[Any, Nothing, Long] = ZIO.effectTotal(currentTime())

// 14
def getCacheValue(key: String, onSuccess: String => Unit, onFailure: Throwable => Unit): Unit = ???
def getCacheValueZio(key: String): ZIO[Any, Throwable, String] = 
    ZIO.effectAsync[Any, Throwable, String] { cb =>
        getCacheValue(key, r => cb(ZIO.succeed(r)), ex => cb(ZIO.fail(ex)))
    }

// 15
trait User
def saveUserRecord(user: User, onSuccess: () => Unit, onFailure: Throwable => Unit): Unit = ???
def saveUserRecordZio(user: User): ZIO[Any, Throwable, Unit] =
    ZIO.effectAsync { cb =>
        saveUserRecord(user, () => cb(ZIO.succeed(())), ex => cb(ZIO.fail(ex)))
    }

// 16
import scala.concurrent.{ ExecutionContext, Future }
trait Query
trait Result
def doQuery(query: Query)(implicit ec: ExecutionContext): Future[Result] = ???
def doQueryZio(query: Query): ZIO[Any, Throwable, Result] =
    ZIO.fromFuture { implicit ec => doQuery(query) }

// 17
object HelloHuman extends App:
    val program = 
        for
            _       <- putStrLn("What is your name?")
            name    <- getStrLn
            _       <- putStrLn(s"Hello, $name!")
        yield()

    def run(args: List[String]) = program.exitCode

// 18
object NumberGuessing extends zio.App:
    import zio.random._
    val game = 
        for
            rnd     <- nextIntBetween(1, 3)
            _       <- putStrLn("Guess number between 1-3")
            guess   <- getStrLn.flatMap(str => ZIO.effect(str.toInt))
            _       <- if rnd == guess then putStrLn("Congrats!! You have guessed it correctly.")
                       else putStrLn(s"Wrong!! Correct number was $rnd, please try again.")
        yield()

    def run(args: List[String]) = game.exitCode

// 19
import java.io.IOException
def readUntil(acceptInput: String => Boolean): ZIO[Console, IOException, String] =
    for 
        input   <- getStrLn
        res     <- if acceptInput(input) then ZIO.succeed(input)
                   else readUntil(acceptInput)
    yield res

// 20
def doWhile[R, E, A](body: ZIO[R, E, A])(condition: A => Boolean): ZIO[R, E, A] =
  body.flatMap(a => if condition(a) then ZIO.succeed(a) else doWhile(body)(condition))