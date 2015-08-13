{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Sudoku where

import Control.Applicative((<$>), (*>), (<*), (<|>), pure, (<*>))
import Data.List(foldl')

import qualified Data.Attoparsec.Text as A 
import qualified Data.Attoparsec.Combinator as AC
import Data.Attoparsec.Text (Parser)
import Data.Text (Text, pack)

-------------------------------------------------------------------------------
-- Value
--
-- Values are for board IO.  PossibleValues (defined in SudokuSolver.sh) are used 
--in the actual Sudoku solving algorithm.

data Value = Empty
           | Value Int

instance Show Value where
    show Empty     = "_"
    show (Value v) = (show v)

instance Eq Value where
    Empty == Empty = True
    Empty == _     = False
    _ == Empty     = False
    (Value v1) == (Value v2) = v1 == v2  

enumerateValues :: [Value]
enumerateValues = [Value 1, Value 2, Value 3, Value 4, Value 5, Value 6, Value 7, Value 8, Value 9]

------------------------------------------------------------------------------
-- GroupId (used to specify a row, column or square)

type GroupId = Int

groupIdFor :: Int -> Maybe GroupId
groupIdFor x
    | x > 0 && x < 10 = Just x
    | otherwise       = Nothing

-------------------------------------------------------------------------------
-- Board

type Group  a = [a]
type Board  a = [Group a] -- Default group is in rows

row :: GroupId -> Board a -> Group a
row = pick

column :: GroupId -> Board a -> Group a
column colId = map (\r -> pick colId r)

square :: GroupId -> Board a -> Group a
square sqrId board = let
                       sRow = startingRow sqrId
                       sCol = startingColumn sqrId
                     in concat $ map(\r -> pickN 3 sCol r) (pickN 3 sRow board)
    where startingRow x 
              | x == 1 || x == 4 || x == 7 = 1
              | x == 2 || x == 5 || x == 8 = 4
              | otherwise                  = 7
          startingColumn x
              | x == 1 || x == 2 || x == 3 = 1
              | x == 4 || x == 5 || x == 6 = 4
              | otherwise                  = 7

getGroups :: Int -> Board a -> (Group a,Group a,Group a) -- (Row,Column,Square)
getGroups position b = (row r b, column c b, square s b)
    where r = ((position-1) `div` 9) + 1
          c = ((position-1) `mod` 9) + 1
          s = ((r-1) `div` 3) + 3 * ((c-1) `div` 3) + 1 

makeRows :: [a] -> Board a
makeRows []  = []
makeRows pvs = (take 9 pvs) : (makeRows (drop 9 pvs))

pick :: GroupId -> [a] -> a
pick groupId list = head $ drop (groupId-1) list

pickN :: Int -> GroupId -> [a] -> [a]
pickN n groupId list = take n $ drop (groupId-1) list

getPosition :: Int -> Board a -> a
getPosition pos board = pick c $ pick r board
    where r = (pos - 1) `div` 9 + 1
          c = (pos - 1) `mod` 9 + 1

setPosition :: Int -> a -> Board a -> Board a
setPosition pos value board = makeRows $ foldr (\val acc -> if length acc == index 
                                                then value:acc 
                                                else val:acc) [] $ concat board
    where index = 81 - pos


-------------------------------------------------------------------------------
-- Sudoku Parser
-- 
-- Expect format:  1 _ _ _ 4 _ _ 2 _ \n
--                 _ _ 3 _ 3 _ _ _ _ \n
--                 ...

valueParser :: Parser Value
valueParser = empty  <|>  value
        where empty = A.char '_' *> pure Empty 
              value = (Value <$> A.decimal)


groupParser :: Parser (Group Value)
groupParser =  A.many' $ valueParser <* (A.char ' ')

boardParser :: Parser (Board Value)
boardParser = A.many' $ groupParser <* (A.char '\n')



