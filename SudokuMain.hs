import Sudoku
import SudokuSolver

import qualified Data.Attoparsec.Text as A 
import Data.Text (pack)

main = do
          runSolve (A.parseOnly boardParser (pack test))
          return ()

test = "7 _ _ _ 9 5 6 _ _ \n" ++ 
       "_ _ 9 _ _ _ _ 1 7 \n" ++
       "_ 8 6 2 _ _ 4 _ _ \n" ++
       "_ _ _ 6 _ 9 _ _ 2 \n" ++
       "5 _ 7 _ 1 _ _ _ _ \n" ++
       "_ _ _ _ 5 4 _ _ _ \n" ++
       "1 _ 2 _ _ 7 _ _ 4 \n" ++
       "_ _ _ _ _ 8 _ _ _ \n" ++
       "_ _ _ _ 2 _ _ 5 _ \n" 


runSolve :: Either String (Board Value) -> IO ()
runSolve (Right board) = do
      let solvedBoard = solve $ toPossibleBoard board 
      print solvedBoard
      return ()
runSolve (Left string) = do 
      putStrLn string
      return ()
