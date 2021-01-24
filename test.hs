module Tutorial9 where

-- Sudoku solver
-- Based on Bird, "Thinking Functionally with Haskell"

import Data.List (sort,nub,(\\),transpose,genericLength)
import Data.String (lines,unlines)
import Test.QuickCheck


type Row a    = [a]
type Matrix a = [Row a]
type Digit    = Char

digits :: [Digit]
digits = ['1'..'9']

blank :: Digit -> Bool
blank d = d == ' '

-- 1.
group :: [a] -> [[a]]
group = groupBy 3

groupBy :: Int -> [a] -> [[a]]
groupBy s xs = [ take s (drop (t*s) xs) | t <- [0..s-1]]

-- 2.
intersperse :: a -> [a] -> [a]
intersperse d = foldr (\x xs -> d:x:xs) [d]

-- 3.
showRow :: String -> String
showRow = concat . intersperse "|" . group

-- 4.
showGrid :: Matrix Digit -> [String]
showGrid = concat . intersperse ["-------------"] . group . map showRow

-- 5.
put :: Matrix Digit -> IO ()
put = putStrLn . unlines . showGrid

-- 6.
showMat :: Matrix Digit -> String
showMat = map (\c -> if c == ' ' then '.' else c) . concat

readMat :: String -> Matrix Digit
readMat = groupBy 9 . map (\c -> if c == '.' then ' ' else c)

-- 7.
choices :: Matrix Digit -> Matrix [Digit]
choices = map (map (\c -> if c == ' ' then ['1'..'9'] else [c]))

-- 8.
cp :: [[a]] -> [[a]]
cp []       = [[]]
cp (xs:xss) = [ x:ys | x <- xs, ys <- cp xss ]

prop_cp :: [[a]] -> Bool
prop_cp xss = length (cp xss) == product (map length xss)
-- slow: use `quickCheckWith (stdArgs {maxSize = 5})`

expand :: Matrix [Digit] -> [Matrix Digit]
expand = cp . map cp

-- 9.
prop_expand :: Matrix [Digit] -> Bool
prop_expand m = length (expand m) == product (map (product . map length) m)

-- 10.
easySols :: Integer
easySols = fromIntegral $ product (map (fromIntegral . product . map length) (choices easy))

timeSolve :: Double
timeSolve = (fromIntegral easySols) / (10^12 * 3600 * 24 * 365 * (13.7 * 10^9))

-- 11, 12, 13.
rows, cols, boxs :: Matrix a -> Matrix a
rows = id
cols = transpose
boxs = map ungroup . ungroup . map cols . group . map group
  where
    ungroup :: Matrix a -> [a]
    ungroup = concat

-- 14.
distinct :: Eq a => [a] -> Bool
distinct xs = xs == nub xs

-- 15.
valid :: Matrix Digit -> Bool
valid g = and $ map (\f -> all distinct (f g)) [rows, cols, boxs]

-- 16.
-- this is not a viable method as shown by the number of easySols
simple :: Matrix Digit -> [Matrix Digit]
simple = filter valid . expand . choices

-- 17.
the :: [Digit] -> Digit
the [d] = d

pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = foldr (\d r -> map (\p -> if [d] == p then p else filter (/=d) p) r) row ds
    where 
        ds = [the p | p <- row, length p == 1]
        rm p d = if [d] == p then p else filter (/=d) p
{-
rewrite this map through each set of possiblilitys and remove all digits
pruneRow = map (\r -> map rm ds)
    where 
        ds = [the p | p <- row, length p == 1]
        rm p d = if [d] == p then p else filter (/=d) p
not . elem d ds
-}

-- 18.
pruneBy :: (Matrix [Digit] -> Matrix [Digit]) -> (Matrix [Digit] -> Matrix [Digit])
pruneBy f = f . map pruneRow . f

prune :: Matrix [Digit] -> Matrix [Digit]
prune = pruneBy rows . pruneBy cols . pruneBy boxs

-- 19.
many :: Eq a => (a -> a) -> a -> a
many f g = if g == f g then g else many f (f g)

-- 20.
extract :: Matrix [Digit] -> Matrix Digit
extract = map (map the)

-- 21.
-- solves everything but hard
solve :: Matrix Digit -> Matrix Digit
solve = extract . many prune . choices


-- ** Optional Material

-- 22.
failed :: Matrix [Digit] -> Bool
failed = any (any null)

-- 23.
solved :: Matrix [Digit] -> Bool
solved = all (all ((1==) . length))

-- 24.
shortest :: Matrix [Digit] -> Int
shortest = mini . map (mini . map length)
    where mini xs = if all (<=1) xs then 1 else minimum (filter (>1) xs)

-- 25.
expand1 :: Matrix [Digit] -> [Matrix [Digit]]
expand1 mat = map (\d -> preMat ++ [preRow ++ [[d]] ++ postRow] ++ postMat) ds
    where
        s = shortest mat
        (preMat, row:postMat) = break (any ((s==) . length)) mat
        (preRow, ds:postRow) = break ((s==) . length) row

-- 26.
search :: Matrix Digit -> [Matrix Digit]
-- search = failed, solved, extract, many, prune, expand1
search = map extract . solver . many prune . choices
    where
        solver :: Matrix [Digit] -> [Matrix [Digit]]
        solver mat | solved mat = [mat]
                   | failed mat = []
                   | otherwise  = concat (map (solver . many prune) (expand1 mat))

book :: Matrix Digit
book = ["  4  57  ",
        "     94  ",
        "36      8",
        "72  6    ",
        "   4 2   ",
        "    8  93",
        "4      56",
        "  53     ",
        "  61  9  "]

-- Examples from websudoku.com

easy :: Matrix Digit
easy = ["    345  ",
        "  89   3 ",
        "3    2789",
        "2 4  6815",
        "    4    ",
        "8765  4 2",
        "7523    6",
        " 1   79  ",
        "  942    "]

medium :: Matrix Digit
medium = ["   4 6 9 ",
          "     3  5",
          "45     86",
          "6 2 74  1",
          "    9    ",
          "9  56 7 8",
          "71     64",
          "3  6     ",
          " 6 9 2   "]

hard :: Matrix Digit
hard = ["9 3  42  ",
        "4 65     ",
        "  28     ",
        "     5  4",
        " 67 4 92 ",
        "1  9     ",
        "     87  ",
        "     94 3",
        "  83  6 1"]

evil :: Matrix Digit
evil = ["  9      ",
        "384   5  ",
        "    4 3  ",
        "   1  27 ",
        "2  3 4  5",
        " 48  6   ",
        "  6 1    ",
        "  7   629",
        "     5   "]

puts :: [Matrix Digit] -> IO ()
puts = sequence_ . map put

puzzle :: Matrix Digit -> IO ()
puzzle g = put g >> puts (search g) >> putStrLn "***"
       
main :: IO ()
main = puzzle easy >>
       puzzle medium >>
       puzzle hard >>
       puzzle evil