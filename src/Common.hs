module Common where
import System.Console.ANSI (Color(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White))

data Card = Card { color::Color ,value::Value } deriving (Read, Eq)

instance Show Card where
  show c = show (color c, value c)

instance Ord Card where
  compare c1 c2 = compare (color c1, value c1) (color c2, value c2)

instance Enum Card where
  toEnum n = let (v,s) = n `divMod` 5
             in Card (toEnum v) (toEnum s)

  fromEnum c = fromEnum (value c) * 5 + fromEnum (color c)

data Value = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Plus2 | Stop | ChDir | Plus4 | ChCol | Dummy
           deriving (Read, Show, Eq, Ord, Enum)

type Hand = [ Card ]
type Deck = [ Card ]
type D_Stack = [ Card ]

data Player = HPlayer { name :: String,
                        hand :: Hand }
            | AiPlayer { name :: String,
                         hand :: Hand }
            deriving (Show, Eq)

data State = State { players :: [ Player ],
                     deck :: Deck,
                     d_stack :: D_Stack }

fullDeck :: Deck
fullDeck = zeroes ++ (nplicate 2 ncards) ++ (nplicate 4 blacks) where
  zeroes = [ Card c Zero | c <- [Red .. Blue] ]
  ncards = [ Card c v | c <- [Red .. Blue], v <- [One .. ChDir] ]
  blacks = [ Card Black v | v <- [Plus4, ChCol] ]


nplicate :: Int -> [a] -> [a]
nplicate n xs = concat $ (take n) $ repeat xs
