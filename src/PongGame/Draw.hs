module PongGame.Draw 
  ( printer 
  , restoreTerminal
  ) where

import Control.Concurrent.STM.TChan ( TChan, readTChan )
import Control.Lens ( view )
import Control.Monad ( forM_ )
import Control.Monad.STM (atomically)
import System.Console.ANSI
    ( clearScreen,
      cursorBackward,
      cursorDown,
      hideCursor,
      setCursorPosition,
      setSGR,
      showCursor,
      BlinkSpeed(NoBlink, SlowBlink),
      Color(Green, White),
      ColorIntensity(Vivid),
      ConsoleLayer(Foreground),
      SGR(SetColor, SetBlinkSpeed, Reset) )
import System.IO (hFlush, stdout)

import PongGame.DSL
    ( PongState,
      Env(paddleHeight, windowWidth, windowHeight, paddleColor,
          ballColor),
      ballPos,
      player1,
      player2,
      terminated )

type WindowHeight    = Int
type WallWidth       = Int
type PaddleHeight    = Int
type PaddlePositionX = Int   -- Paddle position in X
type PaddlePositionY = Int   -- Top paddle position in Y
type WallColor       = Color
type GameResult      = String

-- | Draw the 'PongState' snapshots on the terminal.  
-- The snapshots are read from the 'PongState' channel,  
printer :: Env -> TChan PongState -> IO ()
printer env chanState = 
  let paddleHeight' = paddleHeight env
      windowWidth'  = windowWidth  env
      windowHeight' = windowHeight env
      paddleColor'  = paddleColor  env
      go = do
          state <- atomically (readTChan chanState)
          let mresult = view terminated state
              (x, y)  = view ballPos state
              p1      = view player1 state
              p2      = view player2 state
              ballX   = round x
              ballY   = round y

          case mresult of
            Just result -> do
              let msg = case result of 
                          Left _       -> "QUITTED"
                          Right winner -> "WINNER " ++ show winner
              resetScreen 
              drawWall 0             windowWidth' White
              drawWall windowHeight' windowWidth' White
              drawPaddle 0                  p1 paddleHeight' paddleColor'
              drawPaddle (windowWidth' - 3) p2 paddleHeight' paddleColor'
              drawResult (windowWidth' `div` 2 - 4) (windowHeight' `div` 2 - 1) msg
              hFlush stdout
            Nothing     -> do 
              resetScreen
              drawWall 0             windowWidth' White
              drawWall windowHeight' windowWidth' White
              drawPaddle 0                  p1 paddleHeight' paddleColor'
              drawPaddle (windowWidth' - 3) p2 paddleHeight' paddleColor'
              drawBall ballX ballY (ballColor env)
              hFlush stdout
              go
  in hideCursor >> go

-- | Restore the terminal with the default behavior
-- moving the cursor bellow the last snapshot drawn.
restoreTerminal :: WindowHeight -> IO ()
restoreTerminal windowHeight' = 
  setSGR [Reset] >> showCursor >> setCursorPosition (windowHeight' + 2) 0

-- | Draw the game result. 
drawResult :: PaddlePositionX -> PaddlePositionY -> GameResult -> IO ()
drawResult x y result = do
  setSGR [Reset]
  setSGR [SetColor Foreground Vivid Green, SetBlinkSpeed SlowBlink]
  setCursorPosition y x
  let gameOver = "GAME OVER"
  putStr gameOver
  cursorBackward $ length gameOver
  cursorDown 2
  setSGR [SetBlinkSpeed NoBlink]
  putStr result

-- | Reset the screen.
resetScreen :: IO ()
resetScreen = setSGR [Reset] >> clearScreen >> setCursorPosition 0 0

-- | Draw a wall.
drawWall :: PaddlePositionY -> WallWidth -> WallColor -> IO ()
drawWall y w c = do
  setSGR [Reset]
  setCursorPosition y 0
  setSGR [SetColor Foreground Vivid c]
  putStrLn $ replicate (w - 1) '-'

-- | Draw a paddle.
drawPaddle :: PaddlePositionX -> PaddlePositionY -> PaddleHeight -> Color -> IO ()
drawPaddle x y h c = do
  let posyB = y + h - 1 
  setSGR [Reset] 
  setSGR [SetColor Foreground Vivid c]
  forM_ [y .. posyB] $ \posy -> do 
    setCursorPosition posy x
    putStr "::"

-- Draw the ball.
drawBall :: PaddlePositionX -> PaddlePositionY -> Color -> IO ()
drawBall x y c = do
  setSGR [Reset]
  setCursorPosition y x
  setSGR [SetColor Foreground Vivid c]
  putStr "O"
