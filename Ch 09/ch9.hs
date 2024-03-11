-- data [] a = [] | a : [a]
import Data.Bool
import Data.Char

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:[]) = Nothing
safeTail (_:xs) = Just xs


safeHead :: [a] -> Maybe a


safeHead [] = Nothing
safeHead (x:_) = Just x


eftBool :: Bool -> Bool -> [Bool]
eftBool True True = [True]
eftBool True False = []
eftBool False True = [False, True]
eftBool False False = [False]



-- eftOrd :: (Ordering a) => a -> a -> [a]
-- 

eft :: (Enum a, Ord a) => a -> a -> [a]
eft x y 
    | x > y = []
    | x == y = [x]
    | x < y = (x:) $ eft (succ x) y


-- takeWhile is a Higher order function that instead of providing a
-- an index like take, provides a predicate that must be true to continue
-- taking


-- myWords :: String -> [String]
-- myWords "" = []
-- myWords s = (takeWhile (/= ' ') s ) : (myWords . drop 1 . dropWhile (/= ' ') $ s)
-- myLines :: String -> [String]
-- myLines "" = []
-- myLines s = (takeWhile (/='\n') s) : (myLines . drop 1. dropWhile(/='\n') $ s)


split :: Char -> String ->  [String]
split _ "" = []
split c s = (takeWhile (/=c) s ) : ((split c). drop 1 . dropWhile (/=c) $ s)

myWords = split ' '
myLines = split '\n'

acro :: String -> String
acro s = [x | x <- s, not(elem x ['A'..'Z'])]


-- evaluation of expressions in the list proceeds from left to right (first element down to last)
-- construction of the list proceeds from right to left (last element up to the first)

length1 :: [a] -> Integer
length1 [] = 0
length1 (_:xs) = 1 + length1 xs

-- weak head normal form allows unevaluated expressions so long as the outermost part is a data constructor
-- normal form means nothing is unevaluated 
someFun = map (\x -> bool x (-x) (x == 3))


myFilter :: String -> [String]
myFilter s = filter (\x -> not $ elem x ["the", "a", "an"]) $ myWords s

myZip :: [a] -> [b] -> [(a,b)]
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys


myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f _ [] = []
myZipWith f [] _ = []
myZipWith f (x:xs) (y:ys) = (f x y) : myZipWith f xs ys
myZip2 = myZipWith (,) 


capitalize :: String -> String
capitalize (x:xs) = toUpper x : xs


toUpperString :: String -> String
toUpperString "" = ""
toUpperString (x:xs) = toUpper x : toUpperString xs


firstLetterUp = toUpper . head 



myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x == True then True else myOr xs


myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = if (f x) then True else myAny f xs


myElem :: Eq a => a -> [a] -> Bool

myElem _ [] = False
myElem x (y:ys) = if x == y then True else myElem x ys


myElem2 x = myAny (==x)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x] 

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs


squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = (f x) ++ squishMap f xs  


squishAgain = squishMap (\x -> x)


myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy f (x:y:xs) =
    case f x y of
        GT -> myMaximumBy f (x:xs)
        _ ->  myMaximumBy f (y:xs)
-- Can you write a flip for the function that compares myMaximum by?
-- myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
-- myMinimumBy _ [x] = x
-- myMinimumBy f (x:y:xs) = 
--     case f x y of
--         LT -> myMinimumBy f (x:xs)
--         _  -> myMinimumBy f (y:xs)

myMinimumBy f = myMaximumBy (flip f)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy (compare)

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy (compare)

