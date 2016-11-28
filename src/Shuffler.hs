module Shuffler where

import System.Random
import System.Random.Shuffle
import Common


shuffleDeck :: State -> IO State

shuffleDeck state = shuffleM (deck state)
                    >>= (\currentDeck ->
                    return(state{deck = currentDeck}))
