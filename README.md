- The implementation leverages Haskell's functional programming paradigm, emphasizing immutability and purity.
- Utilizes the Enum and Bounded typeclasses for easy iteration through ranks and suits.
- The deck function generates a standard deck of cards using list comprehensions, showcasing Haskell's concise syntax.
- Utilizes Haskell's powerful type system to handle ordering and comparison of poker hands, enhancing code readability.

## Ranks, Suits, and Cards
- **Ranks:** Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King.
- **Suits:** Clubs, Diamonds, Hearts, Spades.
- **Cards:** A combination of a rank and a suit.

## Deck
- The `deck` function generates a standard deck of cards, covering all possible combinations of ranks and suits.

## Poker
- **Poker Hand Kinds:** HighCard, OnePair, TwoPair, ThreeOfAKind, Straight, Flush, FullHouse, FourOfAKind, StraightFlush.
- **Poker Hand Class:** Represents a poker hand kind along with the relevant ranks.
- **Poker Hand Classification:** The `pokerHandClassify` function takes five cards and classifies them into a Poker Hand Class.
- **Poker Hand Comparison:** The `pokerHandCompare` function compares two poker hands and returns the ordering.

## Utility Functions
- `pokerRankSucc`: Computes the successor rank in poker.
- `pokerRankCompare`: Compares two ranks in the context of poker.
- `allAdjPairs`: Checks if a predicate holds for all adjacent pairs in a list.
- `allEq`: Checks if all elements in a list are equal.
- `runLengthEncode`: Encodes a list into a list of tuples representing run-length encoding.
- `compareBy`: Compares two lists element-wise using a given comparison function.
