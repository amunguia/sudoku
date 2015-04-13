module SudokuSolver where

import Data.List
import Sudoku

-------------------------------------------------------------------------------
-- PossibleValues
--
-- PossibleValues are used in solving a Sudoku puzzle.

data PossibleValues = Only Value 
                    | Guess Value
                    | OneOf [Value]

instance Show PossibleValues where
    show (Only v)   = show v
    show (Guess v)  = show v
    show (OneOf vs) = show vs

instance Eq PossibleValues where
    Only v == Only v1     = v == v1
    Guess v == Guess v2   = v == v2
    OneOf vs == OneOf v2s = vs == v2s
    _ == _                = False

calculatePossibles :: Int -> Board Value -> PossibleValues
calculatePossibles position board
    | length possibles == 1 = Only (head possibles)
    | otherwise             = OneOf possibles
    where (r,c,s) = getGroups position board
          unionOf = r `union` c `union` s
          possibles = enumerateValues \\ unionOf

deduceValue :: PossibleValues -> PossibleValues
deduceValue (OneOf (v:[])) = Only v 
deduceValue  v             = v


fromPossibleBoard :: Board PossibleValues -> Board Value
fromPossibleBoard = map (map toValue)

possibleValues :: Int -> Board Value -> PossibleValues
possibleValues position board = case getPosition position board of 
                                    v@(Value _) -> Only v
                                    Empty       -> calculatePossibles position board

toPossibleBoard :: Board Value -> Board PossibleValues
toPossibleBoard origBoard = makeRows $ map (\i -> 
                                   possibleValues i origBoard) [1..81]

toValue :: PossibleValues -> Value 
toValue (Only v)  = v 
toValue (Guess v) = v
toValue _         = Empty

-------------------------------------------------------------------------------
-- Algorithm
--

anUnsolvedPosition :: Board PossibleValues -> Maybe Int
anUnsolvedPosition board = findUnsolved board [1..81]
    where findUnsolved board []     = Nothing
          findUnsolved board (p:ps) = if hasOnlyOneValue (getPosition p board) 
                                      then findUnsolved board ps
                                      else Just p

checkAllValues :: Int -> PossibleValues -> Board PossibleValues -> Maybe (Board Value)
checkAllValues pos (OneOf []) _ = Nothing
checkAllValues pos (OneOf (p:pvs)) board =
     case solve $ improve (setPosition pos (Guess p) board) of 
        Just solved -> Just solved
        Nothing     -> checkAllValues pos (OneOf pvs) board

improve :: Board PossibleValues -> Board PossibleValues
improve board = toPossibleBoard $ fromPossibleBoard board                  

hasOnlyOneValue :: PossibleValues -> Bool
hasOnlyOneValue (Only _)  = True
hasOnlyOneValue (Guess _) = True
hasOnlyOneValue _         = False

isSolved :: Board PossibleValues -> Bool
isSolved board = (validGroups "col" board) &&
                 (validGroups "row" board) &&
                 (validGroups "square" board)


solve :: Board PossibleValues -> Maybe (Board Value)
solve board = case anUnsolvedPosition board of 
                  Just int -> checkAllValues int (getPosition int board) board
                  Nothing  -> if isSolved board 
                              then Just (fromPossibleBoard board)
                              else Nothing 

removeDups :: Eq a => [a] -> [a]
removeDups list = foldl' (\acc x -> if x `elem ` acc
                               then acc
                               else x:acc) [] list

validGroupTestFunc :: Eq a => [[a]] -> Bool
validGroupTestFunc list = and $ foldl' (\acc x -> ((length $ removeDups x) == 9):acc) [] list

validGroups :: Eq a => String -> Board a -> Bool
validGroups "col" board = validGroupTestFunc $ map (\x -> column x board) [1..9]
validGroups "row" board = validGroupTestFunc $ map (\x -> row x board) [1..9]
validGroups _ board     = validGroupTestFunc $ map (\x -> square x board) [1..9]







