import Data.List
import Data.Char(intToDigit)

-- [ The following lines code our language of forms ]
-- https://en.wikipedia.org/wiki/DPLL_algorithm
--printAllSolutions sudokuProblem

data Literal atom = P atom | N atom
                    deriving (Eq, Show)

data Clause atom = Or [Literal atom]
                   deriving (Eq, Show)

data Form atom = And [Clause atom]
                 deriving (Eq, Show)

neg :: Literal atom -> Literal atom
neg (P a) = N a
neg (N a) = P a                          

(<&&>) :: Form a -> Form a -> Form a
And xs <&&> And ys = And ( xs ++ ys )

(<<) :: Eq atom => [Clause atom] -> Literal atom -> [Clause atom]
cs << l = [ Or (delete (neg l) ls)
               | Or ls <- cs, not (l `elem` ls) ]

dpll :: Eq atom => Form atom -> [[Literal atom]]
dpll f =
    case prioritise f of
      [] -> [[]] -- the trivial solution
      Or [] : cs -> [] -- no solution
      Or (l:ls) : cs ->
          [ l : ls | ls <- dpll (And (cs << l)) ]
          ++
          [ neg l : ls | ls <- dpll (And (Or ls : cs << neg l)) ]

prioritise :: Form atom -> [Clause atom]
prioritise (And cs) = sortOn (\(Or ls) -> length ls) cs

{-
sudoku = allFilled <&&> noneFilledTwice
         <&&> rowsComplete <&&> columnsComplete <&&> squaresComplete
         <&&> rowsNoRepetition <&&> columnsNoRepetition <&&> squaresNoRepetition
the following constraints were found to be the fastest set of constraints
-}

sudoku :: Form (Int, Int, Int)
sudoku = noneFilledTwice <&&> rowsComplete <&&> columnsComplete
         <&&> rowsNoRepetition <&&> columnsNoRepetition <&&> squaresNoRepetition

allFilled :: Form (Int,Int,Int)
allFilled = And [ Or [ P (i,j,n) | n <- [1..9] ]
                | i <- [1..9], j <- [1..9] ]

noneFilledTwice :: Form (Int,Int,Int)
noneFilledTwice = And [ Or [ N (i, j, n), N (i, j, n') ]
                      | i <- [1..9], j <- [1..9],
                        n <- [1..9], n' <- [1..(n-1)]]

rowsComplete :: Form (Int,Int,Int)
rowsComplete = And [ Or [ P (i, j, n) | j <- [1..9] ]
                   | i <- [1..9], n <- [1..9] ]

columnsComplete :: Form (Int,Int,Int)
columnsComplete = And [ Or [ P (i, j, n) | i <- [1..9] ]
                   | j <- [1..9], n <- [1..9] ]

squaresComplete :: Form (Int,Int,Int)
squaresComplete = And [ Or [ P (3*si+i, 3*sj+j, n) | i <- [1..3], j <- [1..3] ]
                   | si <- [0..2], sj <- [0..2], n <- [1..9] ]

rowsNoRepetition :: Form (Int,Int,Int)
rowsNoRepetition = And [ Or [ N (i, j, n), N (i, j', n) ]
                       | i <- [1..9], n <- [1..9],
                         j <- [1..9], j' <- [1..(j-1)] ]

columnsNoRepetition :: Form (Int,Int,Int)
columnsNoRepetition = And [ Or [ N (i, j, n), N (i', j, n) ]
                       | i <- [1..9], n <- [1..9],
                         j <- [1..9], i' <- [1..(i-1)] ]
                      
squaresNoRepetition :: Form (Int,Int,Int)
squaresNoRepetition = And [ Or [ N (3*si+i, 3*sj+j, n), N (3*si+i', 3*sj+j', n) ]
                   | i <- [1..3], j <- [1..3], 
                   i' <- [1..3], j' <- [1..3], 
                   si <- [0..2], sj <- [0..2], n <- [1..9],
                   i /= i' && j /= j']

solutions :: Form (Int, Int, Int) -> [[Literal (Int, Int, Int)]]
solutions problem = dpll (sudoku <&&> problem)

sudokuProblem :: Form (Int, Int, Int)
sudokuProblem = And [ Or [P (1,8,8)], Or [P (1,9,2)], Or [P (2,1,6)]
                    , Or [P (2,4,4)], Or [P (4,1,4)], Or [P (4,5,7)]
                    , Or [P (4,6,2)], Or [P (5,1,5)], Or [P (5,7,4)]
                    , Or [P (5,8,3)], Or [P (6,5,1)], Or [P (7,4,8)]
                    , Or [P (7,7,6)], Or [P (8,2,8)], Or [P (8,3,1)]
                    , Or [P (9,2,2)], Or [P (9,9,7)]]

toLiterals :: Form atom -> [Literal atom]
toLiterals (And clauses) = concat $ map unpack clauses
    where unpack (Or literals) = literals
                                 
showSquares :: [Literal (Int,Int,Int)] -> String
showSquares lits =
  let pos = [ a | P a <- lits ]
  in
   [ (intToDigit.last) [ k | k <-[0..9]
                       , (i, j, k)`elem`pos || k == 0 ]
   | i <- [1..9], j <- [1..9] ]

pretty :: String -> String
pretty = ((tl++dsh++dn++dsh++dn++dsh++tr++"\n"++vt++" ")++)
         . (++(" "++vt++" \n"++bl++dsh++up++dsh++up++dsh++br))
         . intercalate (" "++vt++"\n"++vl++dsh++pl++dsh++pl++dsh++vr++" \n"++vt++" ")
         . map (intercalate (" "++vt++"\n"++vt++" ")) . byThree
         . map (intercalate (" "++vt++" ")). byThree
         . map (intersperse ' ')  . byThree
         . map (\d -> if d == '0' then '\x005F' else d)
  where
    byThree :: [a] -> [[a]]
    byThree (a : b : c : xs) = [a,b,c] : byThree xs
    byThree [] = []
    tl = "\x250F" -- topleft
    tr = "\x2513" -- topright
    bl = "\x2517" -- botleft
    br = "\x251B" -- botright
    dn = "\x2533"
    up = "\x253B"
    vl = "\x2523" -- vertleft
    vr = "\x252B" -- vertright
    vt = "\x2503" -- vertical
    pl = "\x254B" -- plus
    dsh = take 7 $ repeat '\x2501'
  
printProblem :: Form (Int, Int, Int) -> IO ()
printProblem = putStrLn . pretty . showSquares . toLiterals

printSolution :: [Literal (Int, Int, Int)] -> IO ()
printSolution = putStrLn . pretty . showSquares

printAllSolutions :: Form (Int, Int, Int) -> IO ()
printAllSolutions = mapM_ printSolution . solutions

main = do
  printAllSolutions sudokuProblem