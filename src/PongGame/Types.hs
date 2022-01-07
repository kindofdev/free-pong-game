module PongGame.Types where

type WindowHeight = Int
type WindowWidth  = Int

type TerminalSize = (Int, Int)

type PaddleHeigh  = Int
type PaddleStep   = Int

type PlayerPosition       = Int
type OriginPlayerPosition = PlayerPosition
type FinalPlayerPosition  = PlayerPosition

type PositionX     = Float
type PositionY     = Float
type BallPosition  = (PositionX, PositionY)
type VelX          = Int 
type VelY          = Int 
type BallVel       = (VelX, VelY)
type OriginBallVel = BallVel
type FinalBallVel  = BallVel

-- | Frames per second.
type FPS        = Int
type GamePaused = Bool

data Player = Player1  -- Left side player.
            | Player2  -- Right side player.
            deriving (Show, Eq) 

type Winner = Player

data Quitted        = Quitted deriving (Show, Eq)
type PongGameResult = Either Quitted Winner

data Command = MoveDownP1
             | MoveUpP1
             | MoveDownP2
             | MoveUpP2
             | Pause
             | Tick
             | Quit
             deriving (Show, Eq)

data Event = GameStarted
           | BallMoved BallPosition
           | Paddle1MovedUp OriginPlayerPosition FinalPlayerPosition
           | Paddle1MovedDown OriginPlayerPosition FinalPlayerPosition
           | Paddle2MovedUp OriginPlayerPosition FinalPlayerPosition
           | Paddle2MovedDown OriginPlayerPosition FinalPlayerPosition
           | WallCollision OriginBallVel FinalBallVel
           | PaddleCollision Player OriginBallVel FinalBallVel
           | GamePaused
           | GameUnPaused
           | PlayerWon Winner
           | GameTerminated
           | GameQuitted
           deriving (Eq, Show)             

data InputError = WindowSizeError 
                | PaddleSizeError
                | FPSError
                | PlayerPositionError
                | BallPositionError
                | BallVelocityError
                deriving (Eq, Show)