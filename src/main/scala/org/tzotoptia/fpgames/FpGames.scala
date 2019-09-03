package org.tzotoptia.fpgames

import zio.{IO, RIO, Ref, UIO, ZIO}
import zio.console.{Console, getStrLn, putStrLn}

object FpGames extends zio.App {
  sealed trait GameScreen
  final case class Menu() extends GameScreen
  final case class Exit() extends GameScreen

  final class GameState(username: Ref[String])

  object GameState {
    def apply(username: String): UIO[GameState] = {
      for {
        user <- Ref.make(username)
      } yield new GameState(user)
    }
  }

  def checkContinue(): RIO[Console, Boolean] = for {
    _            <- putStrLn("Are you sure you want to exit?")
    choice       <- getStrLn.map(_.toLowerCase)
    willContinue <- choice match {
      case "y" => IO(false)
      case "n" => IO(true)
      case _   => checkContinue()
    }
  } yield willContinue

  def menuSelection(): RIO[Console, GameScreen] = for {
    _             <- putStrLn("Select an option")
    _             <- putStrLn(
      """
        |1. Change name
        |2. Exit
        |""".stripMargin)
    choice        <- getStrLn.map(_.toIntOption)
    menuSelection <- choice.fold(menuSelection())(_ => checkContinue().map(b =>
      if(b) { Menu() }
      else { Exit() }
    ))
  } yield menuSelection

  def gameLoop(gameState: GameState): RIO[Console, Unit] = for {
    selection  <- menuSelection()
    _          <- selection match {
      case Menu() => gameLoop(gameState)
      case Exit() => IO.unit
    }
  } yield ()

  val gameLogic: RIO[Console, Unit] = for {
    _     <- putStrLn("Welcome to the fp games. What's your name?")
    name  <- getStrLn
    _     <- putStrLn(s"Hi, $name.")
    state <- GameState.apply(name)
    _     <- gameLoop(state)
  } yield ()

  def run(args: List[String]): ZIO[Console, Nothing, Int] =
    gameLogic.fold(_ => 1, _ => 0)
}
