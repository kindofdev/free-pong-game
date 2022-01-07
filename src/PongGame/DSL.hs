{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}
module PongGame.DSL 
  ( PongGame -- abstract
  , PongGameF (..) 
  , PongState
  , mkPongState
  , ballPos
  , player1 
  , player2 
  , terminated
  , Env ( windowWidth
        , windowHeight
        , paddleHeight
        , paddleColor
        , ballColor
        , paddleStep
        , fps
        , logFile
        )
  , mkEnv
  , runPongGame
  , printGameSnapshot
  , checkGameTerminated
  , checkGamePaused
  , waitForUnpauseCommand
  , checkPaddleBounce
  , checkWallBounce
  , getCommand
  , handleCommand
  , logEvent
  , logEvents
  ) where

import Control.Lens
    ( Getting, view, over, set, makeLenses, ASetter )
import qualified Data.Sequence as Seq
import Control.Monad        ( unless, when )    
import Control.Monad.Except ( MonadError(throwError) )
import Control.Monad.Extra  ( whenM )
import Control.Monad.Free   ( liftF, Free, MonadFree )
import Control.Monad.RWS
    ( MonadState(get),
      MonadWriter(tell),
      RWST(RWST),
      gets,
      asks,
      modify,
      execRWST )

import qualified Data.Bifunctor as Bi
import Data.Foldable            ( toList )
import Data.Maybe               ( isJust )
import Data.Sequence            ( Seq )
import System.Console.ANSI      ( Color )

import PongGame.Types
    ( GamePaused,
      FPS,
      BallVel,
      BallPosition,
      PlayerPosition,
      PaddleStep,
      PaddleHeigh,
      TerminalSize,
      WindowWidth,
      WindowHeight,
      Player(..),
      Command(..),
      PongGameResult,
      Quitted(Quitted),
      Event(..),
      InputError(..) )

-- | Data describing the state of the pong game.
data PongState = PongState
  { _ballPos :: !BallPosition              -- ^ Pong ball (x, y) location.
  , _ballVel :: !BallVel                   -- ^ Pong ball (vx, vy) velocity given that 
                                           -- vx is pixels per second in X and 
                                           -- vy is pixels per second in Y.
  , _player1 :: !PlayerPosition            -- ^ Upper position in Y for left player paddle.
  , _player2 :: !PlayerPosition            -- ^ Lower position in Y for right player paddle.
  , _paused  :: !GamePaused                -- ^ The game is paused.
  , _terminated :: !(Maybe PongGameResult) -- ^ The game is not terminated (Nothing) or 
                                           -- terminated with a 'Winner' or 'Quitted'.
  } deriving (Eq, Show)

makeLenses ''PongState

-- | Smart constructor for 'PongState'.
-- It fails if it not accomplish the requirements.
mkPongState :: BallPosition
            -> BallVel
            -> PlayerPosition
            -> PlayerPosition
            -> Env
            -> Either InputError PongState 
mkPongState bp@(bpx, bpy) bv@(bvx, bvy) p1 p2 env = 
  let wh = windowHeight env
      ww = windowWidth  env 
      playerRange = [1 .. wh - 1]
      velRange    = [-15 .. 15]

      checkPlayerPositions = p1 `elem` playerRange && p2 `elem` playerRange 

      checkBallPosition =    bpx <= fromIntegral ww - 10 && bpx >= 10
                          && bpy <= fromIntegral wh - 2 && bpy >= 2

      checkBallVelocity =    bvx `elem` velRange
                          && bvy `elem` velRange                  
  in do 
    unless checkPlayerPositions $ throwError PlayerPositionError
    unless checkBallPosition    $ throwError BallPositionError
    unless checkBallVelocity    $ throwError BallVelocityError  
    return PongState 
      { _ballPos = bp
      , _ballVel = bv
      , _player1 = p1
      , _player2 = p2
      , _paused = False
      , _terminated = Nothing
      }

-- | The environment.
data Env = Env 
  { windowWidth        :: !WindowWidth
  , windowHeight       :: !WindowHeight
  , paddleHeight       :: !PaddleHeigh
  , paddleColor        :: !Color 
  , ballColor          :: !Color
  , paddleStep         :: !PaddleStep
  , fps                :: !FPS
  , logFile            :: !(Maybe FilePath)
  } deriving Show

-- | Smart constructor for 'Env'.
-- It fails if it not accomplish the requirements.
mkEnv :: TerminalSize
      -> WindowWidth 
      -> WindowHeight
      -> PaddleHeigh 
      -> Color
      -> Color  
      -> FPS 
      -> Maybe FilePath 
      -> Either InputError Env 
mkEnv ts ww wh ph pc bc fps' mfile = 
  let checkWindowSize = ww < snd ts && wh < fst ts - 2 -- window must be smaller then terminal
      checkPaddleSize = ph <= (20 * wh) `div` 100      -- paddle must be smaller than 20% window height
      checkFPS        = fps' <= 60                     -- fps must be smaller than 60 frames per second
  in do 
    unless checkWindowSize $ throwError WindowSizeError
    unless checkPaddleSize $ throwError PaddleSizeError
    unless checkFPS        $ throwError FPSError
    return Env 
      { windowWidth  = ww
      , windowHeight = wh
      , paddleHeight = ph
      , paddleColor = pc 
      , ballColor = bc 
      , paddleStep = 1
      , fps = fps'
      , logFile = mfile
      }

-- | The functor for our 'Free' monad.
-- This instructions are interpreted by the interpreters (pure or IO interpreter)
data PongGameF next 
    = GetCommand (Command -> next)     -- ^ Gets a 'Command'.
    | PrintGameSnapshot PongState next -- ^ Exposes PongState in order to be "printed" somehow. 
    deriving Functor

-- | The PongGame monad (abstract).
-- Reader for reading the 'Env'.
-- Writer for writing the multiple 'Event's on a log.
-- State for keeping the state of the game 'PongState'.
-- Free for defining actions which need to be interpreted depending on the context 
-- (IO for printing on the terminal or pure for testing)   
newtype PongGame a = PongGame { runPG :: RWST Env (Seq Event) PongState (Free PongGameF) a  }
  deriving 
    ( Functor
    , Applicative
    , Monad
    , MonadFree PongGameF
    )

-- | Run a game given an 'Env' and an initial state 'PongState'.
-- It returns a 'Free' monad with the actions to be interpreted along with 
-- the state, 'PongState', and the event log, '[Event]' as the result of
-- the 'Free' monad.
runPongGame :: Env 
            -> PongState
            -> PongGame a
            -> Free PongGameF (PongState, [Event])
runPongGame env initialState game = 
  Bi.second toList <$> execRWST (runPG game) env initialState

-- | Print a snapshot of the 'PongState'.
-- Notice that lift the abstract action of 'PrintGameSnapshot'.
-- The interpreters will know what to do with this. For example,
-- an IO interpreter would print on the terminal. 
printGameSnapshot :: PongGame ()
printGameSnapshot = do
  state <- PongGame get
  liftF $ PrintGameSnapshot state ()

-- | Check whether the game is terminated.
checkGameTerminated :: PongGame Bool
checkGameTerminated = isJust <$> isGameTerminated

-- | Check whether the game is paused.
checkGamePaused :: PongGame Bool
checkGamePaused = readStateVar paused

-- | Block until an un'Pause' command is received.
-- Note that the pause action is toggled (same command for pausing and unpausing).
-- This action must be used after pausing the game.
-- Commands received after a pause action wil be ignored. Once an unpause ('Pause') command
-- is received commands will be handle normally.
waitForUnpauseCommand :: PongGame Command
waitForUnpauseCommand = do 
  cmd <- getCommand
  case cmd of
    Pause -> return Pause
    _     -> waitForUnpauseCommand

-- | Get a command and it returns it lifting the abstract action
-- 'GetCommand' which will be interpreted by the interpreters. 
getCommand :: PongGame Command
getCommand = liftF $ GetCommand id

-- | Handle a command modifying the game state.
handleCommand :: Command -> PongGame ()
handleCommand cmd = do
  case cmd of 
    MoveUpP1   -> movePaddle1Up
    MoveDownP1 -> movePaddle1Down 
    MoveUpP2   -> movePaddle2Up
    MoveDownP2 -> movePaddle2Down    
    Tick       -> moveBall
    Quit       -> quitGame
    Pause      -> tooglePause 

-- | Move the ball from the ballVel (pixels per second) and FPS.
moveBall :: PongGame ()
moveBall = do
  fps'         <- readEnvVar fps
  (x,  y)      <- readStateVar ballPos
  (vx, vy)     <- readStateVar ballVel
  let frameTime = recip $ fromIntegral fps'
      x'        = x + (fromIntegral vx * frameTime)
      y'        = y + (fromIntegral vy * frameTime)

  setStateVar ballPos (x', y')
  logEvent $ BallMoved (x', y')

-- | Pause or unpause the game.
tooglePause :: PongGame ()
tooglePause = do
  overStateVar paused not
  isPaused <- readStateVar paused
  logEvent $ if isPaused then GamePaused else GameUnPaused

-- | Quit the game.
quitGame :: PongGame ()
quitGame = do 
  setStateVar terminated (Just $ Left Quitted)
  logEvent GameQuitted  

-- | Check whether the game is terminated and return the result. 
-- There are 2 cases:
-- 1. The game has been quitted.
-- 2. The ball crossed one of the two lines, left or right
-- without being intercepted by a paddle.    
isGameTerminated :: PongGame (Maybe PongGameResult)
isGameTerminated = 
  let checkBallPos x vx windowWidth'
        | signum vx == 1 && x >= windowWidth' - 1 = Just Player1
        | x < 1                                   = Just Player2
        | otherwise                               = Nothing
  in do
    isTerminated <- readStateVar terminated
    case isTerminated of 
      Just result -> return $ Just result    
      Nothing     -> do
        (x, _)       <- readStateVar ballPos
        (vx, _)      <- readStateVar ballVel
        windowWidth' <- readEnvVar windowWidth
        let mwinner = checkBallPos x vx (fromIntegral windowWidth')
        case mwinner of 
          Nothing     -> return Nothing
          Just winner -> do 
            let result = Just $ Right winner  
            setStateVar terminated result
            logEvents [PlayerWon winner, GameTerminated]
            return result

-- | Check whether a wall bounce has happened, updating the game 
-- state if so.
checkWallBounce :: PongGame ()
checkWallBounce = 
  let checkCollision y vy windowHeight' 
        | signum vy == 1  && y > windowHeight' - 1 = True 
        | signum vy == -1 && y < 1                 = True
        | otherwise                                = False      
  in do
    (_, y)        <- readStateVar ballPos
    (vx, vy)      <- readStateVar ballVel
    windowHeight' <- readEnvVar windowHeight
    when (checkCollision y vy (fromIntegral windowHeight')) $ do
      let newVel = (vx, -vy)
      setStateVar ballVel newVel
      logEvent $ WallCollision (vx, vy) newVel

-- | Check whether a paddle bounce has happened, updating the game 
-- state if so.
checkPaddleBounce :: PongGame ()
checkPaddleBounce = do
  ballPos'  <- readStateVar ballPos
  p1        <- readStateVar player1
  p2        <- readStateVar player2
  (vx, vy)  <- readStateVar ballVel
  collision <- paddleCollision ballPos' (vx, vy) p1 p2
  case collision of
    Just (player, velCorrection) -> let vy' = vy + velCorrection
                                    in do
                                      let newVel = (-vx, vy')
                                      setStateVar ballVel newVel
                                      logEvent $ PaddleCollision player (vx, vy) newVel
    Nothing                      -> return ()


type XVelCorrection  = Int                      -- Vel correction in X after a paddle collision. 
type PaddleCollision = (Player, XVelCorrection) -- The paddle player causing the collision.  

-- | Check whether a paddle has happened returning the collision if so.
-- A collision at the center does not correct the ball velocity in X (0)
-- A collision at the upper side corrects the ball velocity with factor (-1)
-- A collision at the lower side corrects the ball velocity with factor (+1)
paddleCollision :: BallPosition 
                -> BallVel 
                -> PlayerPosition 
                -> PlayerPosition 
                -> PongGame (Maybe PaddleCollision)
paddleCollision (x, y) (vx, _) p1 p2 = 
  let checkPlayer1CollisionInX              = x < 3 
      checkPlayer2CollisionInX windowWidth' = x > fromIntegral windowWidth' - 5
    
      playerCollisionInY p paddleHeight' = let p' = fromIntegral p 
                                           in y >= p' && y <= p' + fromIntegral paddleHeight'
      
      velCorrection p paddleHeight' = let halfPos = p + paddleHeight' `div` 2 
                                      in case y `compare` fromIntegral halfPos of 
                                        EQ -> 0   -- center collision 
                                        LT -> -1  -- upper side collision
                                        GT -> 1   -- lower side collision
  in do
    windowWidth'  <- readEnvVar windowWidth
    paddleHeight' <- readEnvVar paddleHeight
    if signum vx == 1
      then if checkPlayer2CollisionInX windowWidth' && playerCollisionInY p2 paddleHeight'
             then return $ Just (Player2, velCorrection p2 paddleHeight')
             else return Nothing 
      else if checkPlayer1CollisionInX && playerCollisionInY p1 paddleHeight'
             then return $ Just (Player1, velCorrection p1 paddleHeight')                             
             else return Nothing

-- | Move up the player1 paddle when it is possible. 
-- Check the upper limit of the top wall.      
movePaddle1Up :: PongGame ()
movePaddle1Up = do
  p1 <- readStateVar player1
  movePaddle p1 (set player1) (-) validatePaddleTop (Paddle1MovedUp p1)

-- | Move up the player2 paddle when it is possible. 
-- Check the upper limit of the top wall.
movePaddle2Up :: PongGame ()
movePaddle2Up = do
  p2 <- readStateVar player2
  movePaddle p2 (set player2) (-) validatePaddleTop (Paddle2MovedUp p2)

-- | Move down the player1 paddle when it is possible. 
-- Check the lower limit of the bottom wall.
movePaddle1Down :: PongGame ()
movePaddle1Down = do
  p1 <- readStateVar player1
  movePaddle p1 (set player1) (+) validatePaddleBottom (Paddle1MovedDown p1)

-- | Move down the player2 paddle when it is possible. 
-- Check the lower limit of the bottom wall.
movePaddle2Down :: PongGame ()
movePaddle2Down = do
  p2 <- readStateVar player2
  movePaddle p2 (set player2) (+) validatePaddleBottom (Paddle2MovedDown p2)

type PlayerPosModifier       = PlayerPosition -> PongState -> PongState
type PlayerPositionOp        = PlayerPosition -> PaddleStep -> PlayerPosition
type PlayerPositionValidator = PlayerPosition -> PongGame Bool

-- | Helper function for moving a paddle.
movePaddle :: PlayerPosition 
           -> PlayerPosModifier 
           -> PlayerPositionOp 
           -> PlayerPositionValidator 
           -> (PlayerPosition -> Event) 
           -> PongGame ()
movePaddle oldPos modifier operator validator eventF = do
    paddleStep' <- readEnvVar paddleStep
    let newPos = oldPos `operator` paddleStep' 
    whenM (validator newPos) $ do 
      PongGame $ modify $ modifier newPos
      logEvent (eventF newPos)

-- | Validate a player position at the board top side.
-- Notice that 'PlayerPosition' corresponds with 
-- the upper position in Y of the player paddle.
validatePaddleTop :: PlayerPosition -> PongGame Bool
validatePaddleTop newPos = return $ newPos >= 1

-- | Validate a player position at the board bottom side.
-- Notice that 'PlayerPosition' corresponds with 
-- the upper position in Y of the player paddle.
validatePaddleBottom :: PlayerPosition -> PongGame Bool
validatePaddleBottom newPos = do
  paddleHeight' <- readEnvVar paddleHeight
  windowHeight' <- readEnvVar windowHeight
  return $ newPos + paddleHeight' <= windowHeight'  

-- | Set a value 'a' into 'PongState' given a 'ASetter' lens.
setStateVar :: ASetter PongState PongState a a -> a -> PongGame () 
setStateVar setter = PongGame . modify . set setter 

-- | Read a value 'a' from 'PongState' given a 'Getting' lens.
readStateVar :: Getting a PongState a -> PongGame a
readStateVar = PongGame . gets . view

-- | Apply a function over a 'PongState' value given a 'ASetter' lens.
overStateVar :: ASetter PongState PongState a b -> (a -> b) -> PongGame () 
overStateVar setter = PongGame . modify . over setter

-- | Read a value from the environment.
readEnvVar :: (Env -> a) -> PongGame a
readEnvVar = PongGame . asks 

-- | Write an event on the event log.
logEvent :: Event -> PongGame ()
logEvent = PongGame . tell . Seq.singleton

-- | Write an event list on the event log.
logEvents :: [Event] -> PongGame ()
logEvents = PongGame . tell . Seq.fromList
