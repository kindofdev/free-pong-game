module Main (main) where

import Control.Monad.Extra ( unlessM, whenM )
import CommandSeq
    ( pauseUnpauseAndMore,
      ticksAndQuit,
      player1MoveAndTicks,
      player2MoveAndTicks )
import Data.Either.Combinators ( rightToMaybe )
import System.Console.ANSI ( Color(Yellow, Red) )
import Test.Tasty.HUnit ( testCase, (@?) )
import Test.Tasty ( TestTree, defaultMain, testGroup )

import PongGame.DSL
    ( PongState,
      PongGame,
      Env,
      mkPongState,
      mkEnv,
      printGameSnapshot,
      checkGameTerminated,
      checkPaddleBounce,
      checkWallBounce,
      checkGamePaused,
      waitForUnpauseCommand,
      getCommand,
      handleCommand,
      logEvent )
import PongGame.PureInterpreter ( runPongGamePure, PureResult )
import PongGame.Types
    ( BallPosition,
      BallVel,
      Command,
      Event(Paddle1MovedDown, GameStarted, WallCollision,
            PaddleCollision, BallMoved),
      InputError,
      Player(..),
      PlayerPosition,
      PongGameResult,
      Quitted(Quitted) )
import Utils ( first, second )

main :: IO ()
main = defaultMain tests 

tests :: TestTree
tests = testGroup "Pong Tests" [unitTests]

-- Run multiple test cases in a pure fashion using the 'PureInterpreter' module. 
unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "A player can quit a game" $
      let cmds   = ticksAndQuit 50
          bp     = (50, 15) 
          bv     = (10, 0)
          p1     = 13
          p2     = 13
          result = runGame cmds (inputParams bp bv p1 p2) game
      in isQuitted result @? "" 

  , testCase "A player 1 can win a game" $
      let cmds   = player2MoveAndTicks
          bp     = (50, 15) 
          bv     = (10, 0)
          p1     = 13
          p2     = 13
          result = runGame cmds (inputParams bp bv p1 p2) game
      in Player1 `wins` result @? ""   

  , testCase "A player 2 can win a game" $
      let cmds   = player1MoveAndTicks
          bp     = (50, 15) 
          bv     = (10, 0)
          p1     = 13
          p2     = 13
          result = runGame cmds (inputParams bp bv p1 p2) game
      in Player2 `wins` result @? ""   

  , testCase "A wall collision modify ball velocity in Y" $
      let cmds       = ticksAndQuit 20
          bp         = (50, 15)
          bv         = (1, -10)
          bvExpected = (1, 10) 
          p1         = 13
          p2         = 13
          result     = runGame cmds (inputParams bp bv p1 p2) game
      in wallCollisionModifyVelocity bv bvExpected result @? ""   

  , testCase "A paddle collision on the center only modifies ball velocity in X" $
      let cmds       = ticksAndQuit 50
          bp         = (50, 15)
          bv         = (10, 0)
          bvExpected = (-10, 0) 
          p1         = 13
          p2         = 13
          result     = runGame cmds (inputParams bp bv p1 p2) game
      in paddleCollisionModifyVelocity Player2 bv bvExpected result @? ""   

  , testCase "A paddle collision on the upper side modifies ball velocity in X and Y" $
      let cmds       = ticksAndQuit 50
          bp         = (50, 15)
          bv         = (10, 0)
          bvExpected = (-10, -1) 
          p1         = 13
          p2         = 14
          result     = runGame cmds (inputParams bp bv p1 p2) game
      in paddleCollisionModifyVelocity Player2 bv bvExpected result @? ""     

  , testCase "A paddle collision on the lower side modifies ball velocity in X and Y" $
      let cmds       = ticksAndQuit 50
          bp         = (50, 15)
          bv         = (10, 0)
          bvExpected = (-10, 1) 
          p1         = 13
          p2         = 12
          result     = runGame cmds (inputParams bp bv p1 p2) game
      in paddleCollisionModifyVelocity Player2 bv bvExpected result @? ""

  , testCase "A player can pause a game" $ 
      let cmds   = pauseUnpauseAndMore
          bv     = (10, 0)
          bp     = (50, 15)
          p1     = 13
          p2     = 13
          result = runGame cmds (inputParams bp bv p1 p2) game
      in (pauseStopsBallUntilUnpause 20 result && pauseMakesCommandsIgnored result) @? ""                          
  ]

game :: PongGame ()
game = 
  let go = do 
        unlessM checkGameTerminated $ do
          whenM checkGamePaused $ do waitForUnpauseCommand >>= handleCommand
          checkPaddleBounce
          checkWallBounce
          getCommand >>= handleCommand
          printGameSnapshot
          go
  in logEvent GameStarted >> go

runGame :: [Command] 
        -> Either InputError (Env, PongState) 
        -> PongGame a 
        -> PureResult  
runGame cmds eparams game' = 
  let (env, state) = either (error . show) id eparams
  in runPongGamePure cmds env state game'

inputParams :: BallPosition 
            -> BallVel 
            -> PlayerPosition 
            -> PlayerPosition 
            -> Either InputError (Env, PongState)
inputParams bp bv p1 p2 = do 
  env   <- mkEnv (200, 200) 100 30 5 Red Yellow 10 Nothing 
  state <- mkPongState bp bv p1 p2 env
  return (env, state)

getPongGameResult :: PureResult -> Maybe PongGameResult
getPongGameResult result = first <$> rightToMaybe result

getPongGameEvents :: PureResult -> Maybe [Event]
getPongGameEvents result = second <$> rightToMaybe result

wallCollisionModifyVelocity :: BallVel -> BallVel -> PureResult -> Bool
wallCollisionModifyVelocity bv bv' = 
  any (WallCollision bv bv' `elem`) . getPongGameEvents

paddleCollisionModifyVelocity :: Player -> BallVel -> BallVel -> PureResult -> Bool
paddleCollisionModifyVelocity player bv bv' =
  any (PaddleCollision player bv bv' `elem`) . getPongGameEvents 

pauseStopsBallUntilUnpause :: Int -> PureResult -> Bool  
pauseStopsBallUntilUnpause n result = 
   any (\events -> length [event | event @ (BallMoved _) <- events] == n)
       (getPongGameEvents result)
  
pauseMakesCommandsIgnored :: PureResult -> Bool 
pauseMakesCommandsIgnored result = 
  any (\events -> null [event | event @ (Paddle1MovedDown _ _) <- events])
      (getPongGameEvents result)

isQuitted :: PureResult -> Bool
isQuitted result = getPongGameResult result == Just (Left Quitted)

wins :: Player -> PureResult -> Bool 
wins player result = getPongGameResult result == Just (Right player)
