{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Coder (compress) where

import qualified Data.ByteString.Internal as BS (c2w)
import Data.List ( sortBy )

data HTree a = HLeaf a | HNode (HTree a) (HTree a) deriving (Show, Eq)

createArray :: (Ord t, Num t, Num a) => t -> [a]
createArray n | n < 255 = 0 : createArray (n + 1)
              | otherwise = [0]

incArr :: Num a => [a] -> Integer -> [a]
incArr arr i = take (fromIntegral i) arr ++ (arr!! fromIntegral i + 1) : drop (fromIntegral i + 1) arr

count :: (Foldable t, Integral a1, Num a2) => t a1 -> [a2] -> [a2]
count xs arr = foldl (\ arr x -> incArr arr (fromIntegral x)) arr xs

makeTuples :: [b] -> [(HTree Integer, b)]
makeTuples xs = makeTuples' xs 0
makeTuples' :: [b] -> Int -> [(HTree Integer, b)]
makeTuples' _ 256 = []
makeTuples' arr i = (HLeaf (fromIntegral i), arr!! fromIntegral i) : makeTuples' arr (i + 1)

clearNull :: (Eq b, Num b) => [(HTree a, b)] -> [(HTree a, b)]
clearNull [] = []
clearNull ((HLeaf a, b):xs) | b == 0 = clearNull xs
                            | otherwise = (HLeaf a, b):xs

sumNode :: Num b => (HTree a, b) -> (HTree a, b) -> (HTree a, b)
sumNode (a, c1) (b, c2) = (HNode a b, c1+c2)

getTree :: (a, b) -> a
getTree (a, b) = a

sortBy' :: [(a, Integer)] -> [(a, Integer)]
sortBy' = sortBy (\(_, a) (_, b) -> compare a b)

createTree :: [(HTree a, Integer)] -> HTree a
createTree xs = getTree $ head $ createTree' xs
createTree' :: [(HTree a, Integer)] -> [(HTree a, Integer)]
createTree' (f:s:xs) = createTree' $ sortBy' (sumNode f s : xs)
createTree' a = a

createTable :: Num a1 => HTree a2 -> [a1] -> [(a2, [a1])]
createTable (HLeaf a) num = [(a,num)]
createTable (HNode a b) num = createTable a (num ++ [0]) ++ createTable b (num ++ [1])

sortBy2 :: [(Integer, b)] -> [(Integer, b)]
sortBy2 = sortBy (\(a, _) (b, _) -> compare a b)

addNulls :: (Eq a1, Num a1) => [(a1, [a2])] -> [[a2]]
addNulls table = addNulls' table 0
addNulls' :: (Eq a1, Num a1) => [(a1, [a2])] -> a1 -> [[a2]]
addNulls' [] _ = []
addNulls' ((a,b):table) i  | i == a = b:addNulls' table (i + 1)
                           | otherwise = [] :addNulls' ((a,b):table) (i + 1)

coding :: Integral a1 => [a1] -> [[a2]] -> [a2]
coding [] _ = []
coding (x:xs) table = (table !! fromIntegral x) ++ coding xs table

toBytes :: Num a => [a] -> [a]
toBytes [] = []
toBytes xs = toByte (take 8 xs) 7 : toBytes (drop 8 xs)

toByte :: (Integral t, Num p) => [p] -> t -> p
toByte [] _ = 0
toByte (x:xs) i = x * (2^i) + toByte xs (i-1)


doTable :: (Show a1, Integral a2) => [(a1, [a2])] -> [Char]
doTable [] = []
doTable ((a, code):xs) = show a ++ " " ++ show (toByte (map fromIntegral code) (length code - 1)) ++ "\n" ++ doTable xs

compress :: (Num b, Integral a1) => [a1] -> [b]
compress inp = do
    let tree = createTree $ clearNull $ sortBy' $ makeTuples $ count inp (createArray 0) -- построение дерева
    let binTable = createTable tree [] -- таблица символ - двоичный код
    let bitList = coding inp (addNulls $ sortBy2 binTable) -- данные в список битов
    let crc = fromIntegral $ length bitList `mod` 8 -- контрольная сумма по модулю 8 - нужна для отброса последних битов
    let bytesData = toBytes bitList -- разбиение битов на байты
    let newTable = map BS.c2w $ doTable binTable -- Создание таблицы для раскодировки
    map fromIntegral (newTable ++ [0,0,0] ++ [crc] ++ bytesData) -- таблица для раскодировки, затем нули (конец таблицы) и сжатые данные