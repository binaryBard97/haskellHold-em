{-

Name: Shreya Patel
Time spent on assignment: 10 hours
Collaborators/Acknowledgements:

-}

module CardGames
  ( Rank (..),
    Suit (..),
    Card (..),
    rank,
    suit,
    deck,
    PokerHandKind (..),
    PokerHandClass (..),
    pokerHandClassify,
    pokerHandCompare,
  )
where

import Data.Either qualified as Either
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Ord qualified as Ord

-- ###################################################################
-- ###################################################################

-- Cards

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
  deriving (Eq, Enum, Bounded, Read, Show)

data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Enum, Bounded, Read, Show)

data Card = Rank :@: Suit
  deriving (Eq, Read, Show)

rank :: Card -> Rank
rank (r :@: _) = r

suit :: Card -> Suit
suit (_ :@: s) = s

-- ###################################################################

-- Deck

deck :: [Card]
deck = [r :@: s | r <- [minBound ..], s <- [minBound ..]]

-- ###################################################################

-- Poker

pokerRankSucc :: Rank -> Maybe Rank
pokerRankSucc Ace = Nothing
pokerRankSucc King = Just Ace
pokerRankSucc r = Just (succ r)

pokerRankCompare :: Rank -> Rank -> Ordering
pokerRankCompare Ace Ace = EQ
pokerRankCompare Ace _ = GT
pokerRankCompare _ Ace = LT
pokerRankCompare r1 r2 = fromEnum r1 `compare` fromEnum r2

data PokerHandKind
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | Straight
  | Flush
  | FullHouse
  | FourOfAKind
  | StraightFlush
  deriving (Eq, Enum, Bounded, Ord, Read, Show)

data PokerHandClass = PHC PokerHandKind [Rank]
  deriving (Eq, Show, Read)

instance Ord PokerHandClass where
  PHC k1 rs1 `compare` PHC k2 rs2 =
    case k1 `compare` k2 of
      EQ -> compareBy pokerRankCompare rs1 rs2
      ord -> ord

pokerHandClassify :: (Card, Card, Card, Card, Card) -> PokerHandClass
pokerHandClassify (c1, c2, c3, c4, c5) =
  case (straight, flush, rankCounts) of
    (True, True, (1, Ace) : (1, Five) : _) -> PHC StraightFlush [Five]
    (True, True, (1, rh) : _) -> PHC StraightFlush [rh]
    (_, _, [(4, r4k), (1, rk)]) -> PHC FourOfAKind [r4k, rk]
    (_, _, [(3, r3k), (2, r2k)]) -> PHC FullHouse [r3k, r2k]
    (_, True, _) -> PHC Flush $ map snd rankCounts
    (True, _, (1, Ace) : (1, Five) : _) -> PHC Straight [Five]
    (True, _, (1, rh) : _) -> PHC Straight [rh]
    (_, _, [(3, r3k), (1, rk1), (1, rk2)]) -> PHC ThreeOfAKind [r3k, rk1, rk2]
    (_, _, [(2, r2k1), (2, r2k2), (1, rk)]) -> PHC TwoPair [r2k1, r2k2, rk]
    (_, _, [(2, r2k), (1, rk1), (1, rk2), (1, rk3)]) -> PHC OnePair [r2k, rk1, rk2, rk3]
    _ -> PHC HighCard $ map snd rankCounts
  where
    cs = [c1, c2, c3, c4, c5]
    rs = List.sortBy pokerRankCompare $ map rank cs
    straight = allAdjPairs (\ra rb -> pokerRankSucc ra == Just rb) rs || [Two, Three, Four, Five, Ace] == rs
    flush = allEq (map suit cs)
    rankCounts = List.sortBy cmp $ runLengthEncode rs
      where
        cmp (n1, r1) (n2, r2) =
          case n2 `compare` n1 of
            EQ -> r2 `pokerRankCompare` r1
            ord -> ord

pokerHandCompare :: (Card, Card, Card, Card, Card) -> (Card, Card, Card, Card, Card) -> Maybe Ordering
pokerHandCompare hand1 hand2
  | legal hand1 && legal hand2 = Just $ pokerHandClassify hand1 `compare` pokerHandClassify hand2
  | otherwise = Nothing
  where
    legal (c1, c2, c3, c4, c5) = List.nub cs == cs
      where
        cs = [c1, c2, c3, c4, c5]

-- ###################################################################

allAdjPairs :: (a -> a -> Bool) -> [a] -> Bool
allAdjPairs _ [] = True
allAdjPairs _ [_] = True
allAdjPairs p (x1 : x2 : xs) = x1 `p` x2 && allAdjPairs p (x2 : xs)

allEq :: (Eq a) => [a] -> Bool
allEq = allAdjPairs (==)

runLengthEncode :: (Eq a) => [a] -> [(Int, a)]
runLengthEncode [] = []
runLengthEncode (x : xs) = case zs of
  [] -> [(1, x)]
  (n, y) : ys -> if x == y then (n + 1, y) : ys else (1, x) : zs
  where
    zs = runLengthEncode xs

compareBy :: (a -> a -> Ordering) -> [a] -> [a] -> Ordering
compareBy _ [] [] = EQ
compareBy _ [] _ = LT
compareBy _ _ [] = GT
compareBy cmp (x : xs) (y : ys) = case x `cmp` y of
  EQ -> compareBy cmp xs ys
  ord -> ord
