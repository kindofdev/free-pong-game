module CommandSeq where

import PongGame.Types
    ( Command(Quit, MoveUpP2, MoveUpP1, MoveDownP1, Pause, Tick) )

player2MoveAndTicks :: [Command]
player2MoveAndTicks = mconcat
  [ [Tick]
  , [MoveUpP2]
  , [MoveUpP2]
  , [MoveUpP2]
  , [MoveUpP2]
  , [MoveUpP2]
  , replicate 50 Tick
  ]

player1MoveAndTicks :: [Command]
player1MoveAndTicks = mconcat
  [ [Tick]
  , [MoveUpP1]
  , [MoveUpP1]
  , [MoveUpP1]
  , [MoveUpP1]
  , [MoveUpP1]
  , replicate 150 Tick
  ]

ticksAndQuit :: Int -> [Command]
ticksAndQuit n = replicate n Tick <> [Quit]  

pauseUnpauseAndMore :: [Command]
pauseUnpauseAndMore = mconcat 
  [ replicate 10 Tick
  , [Pause]
  , [MoveDownP1] 
  , replicate 10 Tick
  , [Pause]  -- unpause
  , replicate 10 Tick
  , [Quit]
  ] 