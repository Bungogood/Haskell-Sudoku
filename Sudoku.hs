import Data.Char

type Sudoku = [Int]
type SubIndex = Int
type Index = Int
type Value = Int
type RC = (Int, Int)

blank :: Sudoku
blank =
    [ 0,0,0,0,0,0,0,0,0
    , 0,0,0,0,0,0,0,0,0
    , 0,0,0,0,0,0,0,0,0
    , 0,0,0,0,0,0,0,0,0
    , 0,0,0,0,0,0,0,0,0
    , 0,0,0,0,0,0,0,0,0
    , 0,0,0,0,0,0,0,0,0
    , 0,0,0,0,0,0,0,0,0
    , 0,0,0,0,0,0,0,0,0
    ]

test1 = 
    [ 1,2,3,4,5,6,7,8,9
    , 4,5,6,7,8,9,1,2,3
    , 7,8,9,1,2,3,4,5,6
    , 2,1,4,3,6,5,8,9,7
    , 3,6,5,8,9,7,2,1,4
    , 8,9,7,2,1,4,3,6,5
    , 5,3,1,6,4,2,9,7,8
    , 6,4,2,9,7,8,5,3,1
    , 9,7,8,5,3,1,6,4,2
    ]

test2 = 
    [ 1,2,3,4,5,6,7,8,9
    , 4,5,6,7,8,9,1,2,3
    , 7,8,9,1,2,3,4,5,6
    , 2,1,4,3,6,5,8,9,7
    , 3,6,5,8,9,7,2,1,4
    , 8,9,7,2,1,4,3,6,5
    , 5,3,1,6,4,2,9,7,8
    , 6,4,2,9,7,8,5,3,1
    , 9,7,8,5,3,1,6,4,2
    ]

input :: -> Sudoku
r str | length str /= 9 = row r getline
      | and [isInt c | c <- str] = 
          where 

toSudoku ::  -> Sudoku


output :: Sudoku -> IO ()
output s = do
    putStrLn (row 0)
    where
    row :: Index -> String
    row r | r == 9 = "+-------+-------+-------+"
          | mod r 3 == 0 = "+-------+-------+-------+\n" ++ col 0 ++ row (r+1)
          | otherwise = col 0 ++ row (r+1)
        where
        col :: Index -> String
        col c | c == 9 = "|\n"
              | mod c 3 == 0 = "| " ++ (intToDigit (s !! (rctoi (r,c))) : ' ' : col (c+1))
              | otherwise = intToDigit (s !! (rctoi (r,c))) : ' ' : col (c+1)

itorc :: Index -> RC
itorc i = (div i 9, mod i 9)

rctoi :: RC -> Index
rctoi (r, c) = r * 9 + c

toSub :: Index -> SubIndex
toSub n  = 3 * (div n 3)

checkRow :: Sudoku -> RC -> Value -> Bool
checkRow s (r, c) v = and [ s !! rctoi (r, i) /= v | i <- [0..8] ]

checkCol :: Sudoku -> RC -> Value -> Bool
checkCol s (r, c) v = and [ s !! rctoi (i, c) /= v | i <- [0..8] ]

checkSub :: Sudoku -> RC -> Value -> Bool
checkSub s (r, c) v = and [ s !! rctoi (toSub r + div i 3, toSub c + mod i 3) /= v | i <- [0..8] ]

valid :: Sudoku -> Index -> Value -> Bool
valid s i v = and [check s rc v | check <- [checkRow, checkCol, checkSub] ]
    where rc = itorc i

set :: Sudoku -> Index -> Value -> Sudoku
set s i v = (take i s) ++ [v] ++ (drop (i+1) s)

process :: Sudoku -> Index -> Maybe Sudoku
process s i | i    == 81 = Just s
            | s!!i /=  0 = process s (i+1)
            | otherwise  = checker s i 0  
            where 
            checker :: Sudoku -> Index -> Value -> Maybe Sudoku
            checker s i v | v == 10 = Nothing
                          | valid s i v = setter s i v
                          | otherwise   = checker s i (v+1)
            setter :: Sudoku -> Index -> Value -> Maybe Sudoku
            setter s i v | new /= Nothing = new
                         | otherwise = checker s i (v+1)
                         where new = process (set s i v) (i+1)

solve :: Sudoku -> Maybe Sudoku
solve s = process s 0

main = do  
    sudoku <- input sudoku
    sudoku <- solve sudoku
    output sudoku