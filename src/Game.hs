module Game where

import Common
import Shuffler

initialCardCount :: Int
initialCardCount = 7

initGame :: Int -> State

-- TODO: Implement a method to initialize a new game given n players
initGame n = State { players =  [HPlayer { name = "Player" ++ show xs, hand =  []} | xs <- [1..n] ],
                     deck = Common.fullDeck,
                     d_stack = [ ] }

-- TODO: Implement a method to setup the game

setupGame :: State -> IO State

setupGame gs@State {
          players = ps, deck = d, d_stack = ds }
          = shuffleDeck(State { players = deal d ps,
                        deck = drop 28 d
                  })

deal:: Deck -> [Player] -> [Player]
deal [] _ = []
deal _ [] = []
deal d (p:ps) = p { hand =  (take 7 d) }: deal (drop 7 d) ps
