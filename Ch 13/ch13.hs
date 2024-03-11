module Chap 
() 
where



import Control.Monad (forever)
import System.Exit (exitSuccess)
import Data.Char(toLower)
-- a package is made modules + dependencies



palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    let line2 = removeTrash line1  
    case (line2 == reverse line2) of
        True -> putStrLn "It's a me mario!"
        False -> exitSuccess

removeTrash [] = []
removeTrash (x:xs)
    | elem x ['A'..'Z'] = toLower x : removeTrash xs
    | elem x ['a'..'z'] = x : removeTrash xs
    | otherwise = removeTrash xs

type Name = String 
type Age = Integer


data Person = Person Name Age deriving Show

data PersonInvalid = 
    NameEmpty
    | AgeTooLow
    | PersonInvalidUnknown String
    deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person

mkPerson name age
    | name /= "" && age > 0 = 
        Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise = 
        Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
    putStrLn "Give a name:"
    name <- getLine
    putStrLn "Give an age:"
    ageInputted <- getLine
    let age = (read ageInputted)::Integer
    case (mkPerson name age) of
        (Right p) -> 
            putStrLn ("Yay! succesfful: " ++ show p)
        (Left error) -> 
            putStrLn ("An error occured: " ++ show error)