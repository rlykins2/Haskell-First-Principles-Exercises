import Data.List

-- mostPopularLetter :: String -> Char
-- mostPopularLetter (x:xs) = undefined


-- coolestLtr :: [String] -> Char
-- coolestLtr = ((map (mostPopularLetter)).(map (mostPopularLetter)))


-- coolestWord :: [String] -> String
-- coolestWord = undefined


-- data Maybe = Nothing | Just a


ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n =
    if even n then Just $ n+2 else Nothing

data Example a = Blah | RoofGoats | Woot a

-- what's the difference between type constant and constructor?
    -- type constant takes no arguments, type constructor takes 1 or more
-- The arguments a data constructor can receive are always
    -- values of concrete types
-- What do we apply to type constructors?
    -- types 
-- Lifted and unlifted types
    -- 
-- We can use data constructors anywhere we can use functions 

--1. a is *
--2. a is * and f a is * -> *


-- notThe :: String -> Maybe String
-- notThe = undefined


-- somethang :: a -> Maybe a
--     some a = Just a

notThe :: String -> Maybe String
notThe s = if s == "the" then Just "a" else Nothing



replaceThe :: String -> String
replaceThe =  unwords . 
    (foldr (\x y -> 
            case notThe x of
                Just "a" -> "a": y
                Nothing -> x:y) [])
    . words


vowels = "aeiouAEIOU"


countTheBeforeVowel :: String -> Integer

countTheBeforeVowel' [] = 0
countTheBeforeVowel'  (x:y:xs) = let (y':ys) = y in
    if x == "the" && elem y' vowels 
    then 1 + countTheBeforeVowel' xs
    else countTheBeforeVowel' (y:xs) 
countTheBeforeVowel'  (x:_) = 0

countTheBeforeVowel = (countTheBeforeVowel' . words)


countVowels :: String -> Integer
countVowels = 
    foldr (\x y ->
        case elem x vowels of
            True -> 1 + y
            False -> y) 0

newtype Word' = Word' String deriving (Eq, Show)



mkWord :: String -> Maybe Word' 
mkWord s = if v > c then Nothing else Just $ Word' s
    where
        v = countVowels s
        c = ((fromIntegral .length) s) - v


data Nat = 
    Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat' 0 = Zero
integerToNat' n = Succ $ integerToNat' (n-1) 

integerToNat :: Integer -> Maybe Nat
integerToNat n 
    | n < 0 = Nothing
    | otherwise =  Just $ integerToNat' n


isJust :: Maybe a -> Bool
isJust (Just _) = True 
isJust _ = False   

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False


mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just x) = f x 
mayybee b _ _ = b

fromMaybe :: a -> Maybe a -> a
fromMaybe a = mayybee a (\x -> x)


listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x


maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]

catMaybes [] = []
catMaybes (x:xs) = 
    case x of
        (Just x) -> x : catMaybes xs
        Nothing -> catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Nothing : _) = Nothing
flipMaybe ((Just x):xs) = 
    case flipMaybe xs of
        (Just ys) -> Just (x:ys)
        Nothing -> Nothing

lefts' :: [Either a b] -> [a]
-- lefts' [] = []
-- lefts' ((Left x):xs) = x : lefts' xs
-- lefts' ((Right x): xs) = lefts' xs

lefts' = 
    foldr (\x y -> 
        case x of 
            (Right _) -> y
            (Left a) -> a:y) []

rights' :: [Either a b] -> [b]


rights' = 
    foldr (\x y -> 
            case x of 
                (Right a) -> a:y
                (Left _) -> y) []


partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' s = (lefts' s, rights' s) 

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just $ f b 


either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) =  g b


eitherMaybe'' f = either' (\_ -> Nothing) (\x -> Just $ f x)



myIterate :: (a -> a) -> a -> [a]
myIterate f prev = prev : myIterate f (f prev)

myUnfoldr :: (b -> Maybe (a,b)) -> b -> [a]
myUnfoldr f prev = 
    case f prev of
        Just (a,b) -> a : myUnfoldr f b
        Nothing -> []


betterIterate f prev = myUnfoldr (\x -> Just $ (x, f x)) prev



data BinaryTree a = 
    Leaf
    | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f prev = 
    case f prev of
        Just (left, b, right) -> Node (unfold f left) (b) (unfold f right)
        Nothing -> Leaf


treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\x -> if x >= n then Nothing else Just (x+1, x, x+1)) 0
