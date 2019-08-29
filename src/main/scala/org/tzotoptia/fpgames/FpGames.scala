package org.tzotoptia.fpgames

import zio.{IO, RIO, ZIO}
import zio.console.{Console, getStrLn, putStrLn}

object FpGames extends zio.App {
  def checkContinue(): RIO[Console, Boolean] = for {
    _            <- putStrLn("Do you want to continue?")
    choice       <- getStrLn.map(_.toLowerCase)
    willContinue <- choice match {
      case "y" => IO(true)
      case "n" => IO(false)
      case _   => checkContinue()
    }
  } yield willContinue

  def gameLoop(): RIO[Console, Unit] = for {
    willContinue <- checkContinue()
    _            <- if(willContinue) gameLoop() else IO.unit
  } yield ()

  val gameLogic: RIO[Console, Unit] = for {
    _    <- putStrLn("Welcome to the fp games. What's your name?")
    name <- getStrLn
    _    <- putStrLn(s"Hi, $name.")
    _    <- gameLoop()
  } yield ()

  def run(args: List[String]): ZIO[Console, Nothing, Int] =
    gameLogic.fold(_ => 1, _ => 0)
}
