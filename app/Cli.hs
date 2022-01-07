module Cli ( parseArgsIO ) where

import System.Console.ANSI ( getTerminalSize, Color(Yellow, Red) )
import System.Environment (getArgs)
import System.Random (randomRIO)

import PongGame.DSL ( PongState, Env, mkPongState, mkEnv )
import PongGame.Types
    ( BallPosition,
      BallVel,
      FPS,
      PaddleHeigh,
      PlayerPosition,
      TerminalSize,
      WindowHeight,
      WindowWidth )

-- | Collect args and parse them as 'Env' and 'PongState' (initialState)
parseArgsIO :: IO (Env, PongState)
parseArgsIO = getArgs >>= parseArgs

-- <windowWidth> <windowHeight> <paddleHeight> <fps> <logFile>
parseArgs :: [String] -> IO (Env, PongState)
parseArgs [ww, wh, ph, fps_, file] = do
    let ww'   = read ww
        wh'   = read wh
        ph'   = read ph
        fps'  = read fps_

        velRange = (-15, 15)
        bpx      = fromIntegral ww' / 2  -- Ball starts centered 
        bpy      = fromIntegral wh' / 2
        p1       = 1                     -- Players starts at the top 
        p2       = 1

    Just ts      <- getTerminalSize
    (bvx, bvy)   <- randomVel velRange
    env          <- buildEnv ts ww' wh' ph' Red Yellow fps' (Just file)
    initialState <- buildInitialState (bpx, bpy) (bvx, bvy) p1 p2 env
    return (env, initialState)
parseArgs _ = error "The CLI args could not be parsed"    

-- | Generates a pair of Int given a range.
randomVel :: (Int, Int) -> IO (Int, Int)
randomVel range = (,) <$> randomRIO range <*> randomRIO range

-- | Build an Env inside IO failing if the input params are wrong. 
buildEnv :: TerminalSize 
         -> WindowWidth 
         -> WindowHeight
         -> PaddleHeigh
         -> Color 
         -> Color 
         -> FPS 
         -> Maybe FilePath 
         -> IO Env  
buildEnv ts ww wh ph pc bc fps' mfile = do
  let env = mkEnv ts ww wh ph pc bc fps' mfile 
  case env of 
    Right env' -> return env'
    Left error' -> fail $ show error'     

-- | Build a 'PongState' inside IO failing if the input params are wrong.
buildInitialState :: BallPosition
                  -> BallVel
                  -> PlayerPosition
                  -> PlayerPosition
                  -> Env
                  -> IO PongState 
buildInitialState bp bv p1 p2 env = do
  let state = mkPongState bp bv p1 p2 env
  case state of 
    Right state' -> return state'
    Left error'  -> fail $ show error' 
