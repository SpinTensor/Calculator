import Data.List

nums :: [Char]
nums = ['0'..'9']

letts :: [Char]
letts = ['a'..'z']++['A'..'Z']

separateOperators :: [Char] -> [Char]
separateOperators [] = []
separateOperators (a:b:c:as)
   | elem a nums && elem b ['e','E'] && elem c nums = a:b:c:(separateOperators as)
separateOperators (a:b:as)
   | elem a letts && elem b nums = a:' ':(separateOperators $ b:as)
separateOperators (a:as)
   | elem a "+-*/^()"  = ' ':a:' ':(separateOperators as)
   | otherwise = a:(separateOperators as)

evalexpr :: [[Char]] -> Double
evalexpr a
   | elem "+"    a = (substr1 "+" a) +  (substr2 "+" a)
   | elem "-"    a = (substr1 "-" a) -  (substr2 "-" a)
   | elem "*"    a = (substr1 "*" a) *  (substr2 "*" a)
   | elem "/"    a = (substr1 "/" a) /  (substr2 "/" a)
   | elem "^"    a = (substr1 "^" a) ** (substr2 "^" a)
   | elem "ln"   a = log                (substr2 "ln" a)
   | elem "log"  a = logBase 10.0       (substr2 "log" a)
   | elem "sin"  a = sin                (substr2 "sin" a)
   | elem "cos"  a = cos                (substr2 "cos" a)
   | elem "tan"  a = tan                (substr2 "tan" a)
   | elem "asin" a = asin               (substr2 "asin" a)
   | elem "acos" a = acos               (substr2 "acos" a)
   | elem "atan" a = atan               (substr2 "atan" a)
   | elem "exp"  a = exp                (substr2 "exp" a)
   | elem "sqrt" a = sqrt               (substr2 "sqrt" a)
   | elem "pi"   a = pi
   | elem "e"    a = exp 1.0
   | length      a == 1 = read (a!!0) :: Double
   | otherwise = (0/0)
      where 
         idx :: [Char] -> [[Char]] -> Int
         idx op str = last $ elemIndices op str
         substr1 :: [Char] -> [[Char]] -> Double
         substr1 op str = evalexpr $ take ((idx op str)+0) str
         substr2 :: [Char] -> [[Char]] -> Double
         substr2 op str = evalexpr $ drop ((idx op str)+1) str

evalPar :: [[Char]] -> [[Char]]
evalPar a
   | count "(" a == 0 = a
   | otherwise = evalPar $ leftrest ++ replacePar ++ rightrest
      where
         count :: [Char] -> [[Char]] -> Int
         count x list = length $ filter (==x) list
         replacePar :: [[Char]]
         replacePar = (:[]) $ show $ evalexpr $ drop ((leftidx a) + 1) $ take (rightidx a) a
         leftrest :: [[Char]]
         leftrest = take (leftidx a) a
         rightrest :: [[Char]]
         rightrest = drop ((rightidx a) + 1) a
         rightidx :: [[Char]] -> Int
         rightidx [] = -1
         rightidx x = (!!0) $ elemIndices ")" x
         leftidx :: [[Char]] -> Int
         leftidx [] = -1
         leftidx x = last $ takeWhile (<rightidx x ) $ elemIndices "(" x

main :: IO ()
main =
   do
      input <- getContents
      putStr $ 
         unlines $ 
         map head $
         map evalPar $ 
         map words $ 
         map separateOperators $ 
         map ('(':) $
         map (++")") $ 
         lines input
