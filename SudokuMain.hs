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

showGroups board = do
      showRow 1 board
      showRow 2 board
      showRow 3 board
      showRow 4 board
      showRow 5 board
      showRow 6 board
      showRow 7 board
      showRow 8 board
      showRow 9 board
      showCol 1 board
      showCol 2 board
      showCol 3 board
      showCol 4 board
      showCol 5 board
      showCol 6 board
      showCol 7 board
      showCol 8 board
      showCol 9 board
      showSqr 1 board
      showSqr 2 board
      showSqr 3 board
      showSqr 4 board
      showSqr 5 board
      showSqr 6 board
      showSqr 7 board
      showSqr 8 board
      showSqr 9 board
      putStrLn ""
      putStrLn ""
      putStrLn ""
      putStrLn ""
      putStrLn ""
      putStrLn ""
      putStrLn ""
      putStrLn ""
      let (r1,c1,s1) = getGroups 80 board
      putStrLn $ show r1
      putStrLn $ show c1
      putStrLn $ show s1
      return ()


runSolve :: Either String (Board Value) -> IO ()
runSolve (Right board) = do
      let solvedBoard = solve $ toPossibleBoard board 
      print solvedBoard
      return ()
runSolve (Left string) = do 
      putStrLn string
      return ()

showRow :: GroupId -> Board Value -> IO ()
showRow n board = do 
      putStr ("Row " ++ (show n) ++ ":\t")
      print (row n board)
      return ()

showCol :: GroupId -> Board Value -> IO ()
showCol n board = do 
      putStr ("Col " ++ (show n) ++ ":\t")
      print (column n board)
      return ()

showSqr :: GroupId -> Board Value -> IO ()
showSqr n board = do
      putStr ("Sqr " ++ (show n) ++ ":\t")
      print (square n board)
      return ()

afterParse :: Either String (Board Value) -> IO ()
afterParse (Left string) = do 
                      return ()
afterParse (Right board) = do
                      showGroups board
                      return ()