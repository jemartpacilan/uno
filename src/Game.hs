module Game where

import Common

initGame :: Int -> State

-- TODO: Implement a method to initialize a new game given n players
initGame n = State { players =  [HPlayer { name = "Player" ++ show xs, hand =  []} | xs <- [1..n] ],
                     deck = Common.fullDeck,
                     d_stack = [ ] }

-- TODO: Implement a method to setup the game
setupGame :: State -> IO State
setupGame gs = return (gs)