{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe, pending, pendingWith)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import System.Console.ANSI (Color(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White))

import UnoI
import Common
import Shuffler
import Game

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "uno" $ do
  interactiveSpecs
  commonSpecs
  shufflerSpecs
  gameSpecs

interactiveSpecs :: Spec
interactiveSpecs = describe "unoInteractive" $ do
  it "Say 'Hello' if input == 1" $ do
    (unoInteractive 1) `shouldBe` "Hello"
  it "Say 'Hello World' if input == 2" $ do
    (unoInteractive 2) `shouldBe` "Hello World"

commonSpecs :: Spec
commonSpecs = describe "Common" $ do
  it "Card with color Blue is Blue" $ do
    (color blueThree) `shouldBe` Blue
  it "Card with value Three is Three" $ do
    (value blueThree) `shouldBe` Three
  it "Johnny's first card is Blue" $ do
    (color $ head $ hand johnnyCash) `shouldBe` Blue
  it "Johnny's last card is One" $ do
    (value $ last $ hand johnnyCash) `shouldBe` One
  it "Full deck has 108 cards" $ do
    (length fullDeck) `shouldBe` 108

shufflerSpecs :: Spec
shufflerSpecs = describe "Shuffler" $ do
  it "Perform shuffling of cards" $ do
    --pendingWith "Implement shuffleDeck function"
    let gs = State { players = [ ], deck = fullDeck, d_stack = [ ] }
    gs' <- shuffleDeck gs
    (deck gs') `shouldNotBe` (deck gs)

gameSpecs :: Spec
gameSpecs = describe "Game" $ do
  describe "initGame" $ do
    it "should create 4 players" $ do
      --pendingWith "Implement the initGame function"
      let gs = initGame 4
      length (players gs) `shouldBe` 4
    it "should initialize the deck with 108 cards" $ do
      --pendingWith "Implement the initGame function"
      let gs = initGame 4
      length (deck gs) `shouldBe` 108
    it "should initialize discard pile to empty" $ do
      --pendingWith "Implement the initGame function"
      let gs = initGame 4
      length (d_stack gs) `shouldBe` 0
  describe "setupGame" $ do
    it "should shuffle the deck" $ do
      --pendingWith "Implement the setupGame function"
      let gs = initGame 4
      gs' <- shuffleDeck gs
      (deck gs') `shouldNotBe` (deck gs)
    it "should distribute cards to players" $ do
      --pendingWith "Implement the setupGame function"
      let gs = initGame 4
      gs' <- setupGame gs
      and (map (\p -> (length $ hand p) == initialCardCount) $ players gs') `shouldBe` True
    it "should remove cards from deck" $ do
      --pendingWith "Implement the setupGame function"
      let gs = initGame 4
      gs' <- setupGame gs
      length (deck gs') `shouldBe` 80

-- Test data fixtures
--
blueThree = Card { color = Blue, value = Three }
redOne = Card { color = Red, value = One }
johnnyCash = HPlayer { name = "Johnny", hand = [ blueThree, redOne ] }
