module PongGame.IOInterpreter 
  ( runPongGameInIO
  , inputHandler
  , ticker
  ) where

import Control.Concurrent ( threadDelay )
import Control.Concurrent.STM
    ( atomically, TChan, readTChan, writeTChan )
import Control.Lens ( view )
import Control.Monad ( forever )
import Control.Monad.Free ( Free(..) )
import System.IO
    ( hSetBuffering, hSetEcho, stdin, BufferMode(NoBuffering) )

import PongGame.DSL
    ( PongState,
      PongGame,
      PongGameF(..),
      Env,
      terminated,
      runPongGame )
import PongGame.Types ( Command(..), Event, PongGameResult )

-- | Run a game in the IO monad returning the game result and the event log.
-- The side effects (IO) consist of:
-- 1. Reading inputs from stdin and writing them on the command channel.    
-- 2. Writing 'PongState' snapshots on the state channel.    
runPongGameInIO :: TChan Command
                -> TChan PongState 
                -> Env
                -> PongState
                -> PongGame a
                -> IO (PongGameResult, [Event])
runPongGameInIO chanInput chanState env initialState game = do
  (finalState, events) <- interpretInMonadIO chanInput chanState $ runPongGame env initialState game
  case view terminated finalState of
    Nothing     -> error "This can never happen"  -- A game always returns a result
    Just result -> return (result, events)

-- | Interpret the 'Free' actions in IO monad.
-- 'GetCommand'        --> Read a character from stdin.
-- 'PrintGameSnapshot' --> Write the state snapshot on a state channel.     
interpretInMonadIO :: TChan Command -> TChan PongState -> Free PongGameF next -> IO next
interpretInMonadIO _chanInput _chanState (Pure r)                             = return r
interpretInMonadIO chanInput  chanState  (Free (GetCommand nextFun))          = do 
  cmd <- readCommandIO chanInput
  interpretInMonadIO chanInput chanState $ nextFun cmd
interpretInMonadIO chanInput  chanState  (Free(PrintGameSnapshot state next)) = do 
  writeGameSnapshotIO chanState state 
  interpretInMonadIO chanInput chanState next

-- | Read characters from stdin and write them on the commands channel.
inputHandler :: TChan Command -> IO ()
inputHandler chanInput = forever $ do
  hSetBuffering stdin NoBuffering -- disable input buffering
  hSetEcho stdin False
  c <- getChar
  case c of 
    'w' -> atomically $ writeTChan chanInput MoveUpP1
    's' -> atomically $ writeTChan chanInput MoveDownP1
    'o' -> atomically $ writeTChan chanInput MoveUpP2
    'l' -> atomically $ writeTChan chanInput MoveDownP2
    'p' -> atomically $ writeTChan chanInput Pause
    'q' -> atomically $ writeTChan chanInput Quit
    _   -> return ()

-- | Write a 'Tick' command on the commands channel periodically
-- given a frame interval interval which is calculated by the caller 
-- depending on FPS param of the game.
-- 'ticker' would be the clock of the PongGame system. 
ticker :: Int -> TChan Command -> IO ()
ticker frameInterval chanInput = 
  let go = do 
        threadDelay frameInterval 
        atomically $ writeTChan chanInput Tick
        go 
  in go 

-- | Write a 'PongState' snapshot on the 'PongState' channel.
writeGameSnapshotIO :: TChan PongState -> PongState -> IO ()
writeGameSnapshotIO chanState = atomically . writeTChan chanState  

-- | Read a 'Command' from the commands channel.
readCommandIO :: TChan Command -> IO Command
readCommandIO chanInput = atomically $ readTChan chanInput
