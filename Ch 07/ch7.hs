addOne :: Integer -> Integer
addOne x = x + 1

-- as we apply values to functions we bind the parameters to that value
-- a function will only reduce once all of its parameters are bounded

-- lets allow us to declare and bind variables to values and then write those
-- variables in an expression

-- let bindings are only valid for the expression after in
-- where bindings are valid for the entire function declaration


-- bindExp:: Integer -> String
-- bindExp x = 
--     let z = x in
--         let y = 5 in  -- the reason this is true is because this y is
--         -- only valid for this scope not the let scope above it.
--             "the integer was: "
--             ++ show x ++ " and y was: "
--             ++ show y ++ " and z was: "
--             ++ show z

    

-- addoneIfOdd = \n -> case odd n of
--     True -> f n
--     False -> n
--     where f n = n + 1

-- addFive = \x -> \y -> (if x > y then y else x) + 5



k (x,y) = x


f :: (a,b,c) -> (d,e,f) -> ((a,d), (c,f))
f (a,_,c) (d,_,f') =  ((a,d), (c,f'))



funcZ x = 
    case x+1 == 1 of 
        True -> "Awesome"
        False -> "wut"
pal x =
    case x == reverse x of
        True -> "yes"
        False -> "no"

functionC x y = 
    case x > y of
        True -> y
        False -> y

ifEvenAdd2 n = 
    case even n of 
        True -> (n+2)
        False -> n
nums x = 
    case compare x 0 of
        LT -> -1
        GT -> 1
        EQ -> 0


data Employee = Coder | Manager | Veep | CEO deriving (Eq, Ord, Show)


reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $ show e ++ " is the boss of " ++ show e'


employeeRank :: (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO()

employeeRank f e e' = 
    case f e e' of
        GT -> reportBoss e e'
        EQ -> putStrLn "Neither employee is the boss"
        LT -> (flip reportBoss) e e'


codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' = compare e e'



dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

myAbs x 
    | x < 0 = (-x)
    | otherwise = x

bloodNa :: Integer -> String
bloodNa x
    | x < 135 = "too low"
    | x > 145 = "too high"
    | otherwise = "just right"

isRight :: (Num a, Eq a) => a -> a -> a -> String

isRight a b c 
    | a^2 + b^2 == c^2 = "Right On"
    | otherwise        = "not right"

-- To some degree it seems as if indentation determines scope?


avgGrade :: (Fractional a, Ord a) => a -> Char

avgGrade x 
    | y >= 0.9 = 'A'
    | y >= 0.8 = 'B'
    | y >= 0.7 = 'C'
    | y >= 0.59 = 'D'
    | y < 0.59 = 'F'
    where y  = x / 100


tensDigit :: Integral a => a -> a
tensDigit x = snd $ divMod x 10


hunsD :: Integral a => a -> a
hunsD x = tensDigit $ x `div` 100



foldBool :: a -> a -> Bool -> a

foldBool x y b 
    | b == True  = y
    | b == False = x

foldBool' x y b = 
    case b of
        True -> y
        False -> x

g :: (a -> b) -> (a,c) -> (b,c)
g f (a, c) = (f a, c)


       