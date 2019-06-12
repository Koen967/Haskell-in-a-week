module Poker where

    import Data.List
    import Data.Ord
    import Data.Function

    data Suit = Hearts 
        | Clubs 
        | Spades 
        | Diamonds deriving (Eq, Show)
    
    data Value = Two 
        | Three 
        | Four 
        | Five 
        | Six 
        | Seven 
        | Eight 
        | Nine 
        | Ten 
        | Jack 
        | Queen 
        | King 
        | Ace deriving (Eq, Ord, Bounded, Enum, Show)
    
    compareValues :: Value -> Value -> Bool
    compareValues x y = x < y 

    data Card = Card {value :: Value, suit :: Suit} deriving (Eq, Show)

    data Hand = Hand [Card] deriving (Eq, Show)

    data HandResult = HighCard 
        | OnePair 
        | TwoPairs 
        | ThreeOfAKind 
        | Straight 
        | Flush 
        | FullHouse 
        | FourOfAKind 
        | StraightFlush 
        | RoyalFlush deriving (Eq, Ord, Show)

    getCardsFromHand :: Hand -> [Card]
    getCardsFromHand (Hand cards) = cards

    orderCards :: [Card] -> [Card]
    orderCards = sortBy $ comparing value
    
    orderHand :: Hand -> Hand
    orderHand hand = Hand (orderCards $ getCardsFromHand hand)

    isConsecutive [] = True
    isConsecutive (x:xs) = if null xs 
        then True
        else succ x == (head xs) && isConsecutive xs
    
    isHandStraight :: Hand -> Bool
    isHandStraight hand = isConsecutive $ map value $ getCardsFromHand $ orderHand hand

    isSame [] = True
    isSame (x:xs) = if null xs then True else x == (head xs) && isSame xs

    isHandFlush :: Hand -> Bool
    isHandFlush hand = isSame $ map suit $ getCardsFromHand $ orderHand hand

    isHandStraightFlush :: Hand -> Bool
    isHandStraightFlush hand = isHandStraight hand && isHandFlush hand

    -- Deze functie is gevonden op het internet. Ik heb na veel pogingen deze niet zelfstandig kunnen maken.
    valCounts :: Hand -> [(Value, Int)]
    valCounts (Hand a) =
        sortBy (\x y -> snd y `compare` snd x) -- Put highest counts first 
        $ nubBy sameVal -- Only keep unique vals and their counts in the list
        $ reverse -- Make nub keep only the highest count and put high vals first
        $ scanl1 (\x y -> if sameVal x y then (fst y, snd x + 1) else y) -- inc cnt
            [(value x, 1) | x <- a] -- Ignore suits, initialize counts to 1
        where sameVal x y = fst x == fst y

    getHandType :: Hand -> HandResult
    getHandType hand
        | isHandStraightFlush hand = StraightFlush
        | count 0 == 4 = FourOfAKind
        | count 0 == 3 && count 1 == 2 = FullHouse
        | isHandFlush hand = Flush
        | isHandStraight hand = Straight
        | count 0 == 3 = ThreeOfAKind
        | count 0 == 2 && count 1 == 2 = TwoPairs
        | count 0 == 2 = OnePair
        | otherwise = HighCard
        where count = (map snd (valCounts hand) !!)

    compareHands hand1 hand2
        | getHandType hand1 < getHandType hand2 = "Player 2 wins"
        | getHandType hand1 > getHandType hand2 = "Player 1 wins"
        | otherwise = "Draw"