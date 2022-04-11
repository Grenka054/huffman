{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Decoder (decompress) where

import qualified Data.ByteString.Internal as BS (w2c)
import Data.Maybe ( fromJust, isJust )
import Data.Char ( digitToInt )
import qualified Data.Word as GHC.Word

data HTree a = Null | HLeaf a | HNode (HTree a) (HTree a) deriving (Show, Eq)

getTable :: [GHC.Word.Word8] -> [Char]
getTable (a:b:xs) | a + b + head xs /= 0 = BS.w2c a : getTable (b:xs)
                    | otherwise = []
getTable [] = []

numBForSkip :: (Eq a, Num a, Num t) => [a] -> t
numBForSkip xs = numBForSkip' xs 0

numBForSkip' :: (Eq a, Num a, Num t) => [a] -> t -> t
numBForSkip' (a:b:xs) n | a /= 0 && b /= 0 && head xs /= 0 = numBForSkip' (b:xs) (n + 1)
                         | otherwise = n + 2
numBForSkip' [] n = n

codeToBits :: (Integral a1, Num a2) => [a1] -> Int -> [a2]
codeToBits [c] 0 = toBin c
codeToBits [c] crc = take crc $ toBin c
codeToBits (c:code) crc = toBin c ++ codeToBits code crc

toBin 0 = replicate 8 0
toBin n = replicate (8 - length a) 0 ++ a where
    a = toBin' n

toBin' :: (Integral a1, Num a2) => a1 -> [a2]
toBin' 0 = []
toBin' n | n `mod` 2 == 1 = toBin' (n `div` 2) ++ [1]
         | otherwise = toBin' (n `div` 2) ++ [0]

toByte :: (Integral t, Num p) => [p] -> t -> p
toByte [] _ = 0
toByte (x:xs) i = x * (2^i) + toByte xs (i-1)

makeTable :: (Integral a1, Num a2) => [(a3, a1)] -> [(a3, [a2])]
makeTable [] = []
makeTable ((a,b):table) = (a,toBin2 b): makeTable table
toBin2 :: (Num a2, Integral a1) => a1 -> [a2]
toBin2 0 = [0]
toBin2 n = toBin' n

addToTree :: (Eq a1, Num a1) => a2 -> [a1] -> HTree a2 -> HTree a2
addToTree a (x:xs) (HNode b c)  | x == 0 = HNode (addToTree a xs b) c
                                | otherwise = HNode b (addToTree a xs c)
addToTree a [] Null = HLeaf a
addToTree a xs Null = addToTree a xs (HNode Null Null)

createTree :: (Eq a1, Num a1) => [(a, [a1])] -> HTree a -> HTree a
createTree [] tree = tree
createTree ((a,b):xs) tree = createTree xs (createTree' a b tree)
createTree' :: (Eq a1, Num a1) => a -> [a1] -> HTree a -> HTree a
createTree' a xs tree   | check xs tree = addToTree a xs tree
                        | otherwise = addToTree a xs (HNode tree Null)

check :: (Eq a1, Num a1) => [a1] -> HTree a2 -> Bool
check (x:xs) (HNode a b) | x == 0 = check xs a
                         | otherwise = check xs b
check _ Null = True
check _ _ = False

decode :: Integral a1 => [a1] -> [a1] -> HTree a1 -> [a1]
decode [] s table = [fromJust (findInTree s table)]
decode (x:xs) s table | isJust a = fromJust a : decode (x:xs) [] table
                      | otherwise = decode xs (s ++ [x]) table where
    a = findInTree s table


findInTree :: Integral a1 => [a1] -> HTree a2 -> Maybe a2
findInTree [] (HLeaf a) = Just a
findInTree (x:xs) (HNode a b) | fromIntegral x == 0 = findInTree xs a
                              | otherwise = findInTree xs b
findInTree (x:xs) (HLeaf a) = Nothing
findInTree _ _ = Nothing

stringToInt :: [Char] -> Int
stringToInt [] = 0
stringToInt x = foldl (\ a b -> a * 10 + b) 0 (map digitToInt x)


encodeSeries :: (Num a, Num b) => [Char] -> [(a, b)]
encodeSeries [] = []
encodeSeries str = (fromIntegral $ stringToInt a, fromIntegral $ stringToInt b) : encodeSeries (drop (length a + length b + 2) str) where
    a = takeNum str
    b = takeNum $ drop (length a + 1) str

takeNum :: String -> String
takeNum (s:xs)  | s == ' ' || s == '\n' = []
                | otherwise = s : takeNum xs

isNeed :: (Eq a, Num a) => [a] -> Bool
isNeed (a:b:c:_) = not (a == 0 && c == 0)

decompress :: Integral a => [GHC.Word.Word8] -> [a]
decompress inp = do
    if isNeed inp then do
        let tableZ = encodeSeries $ getTable inp -- сжатая таблица
        let table = makeTable tableZ -- сделать таблицу
        let tree = createTree table Null -- построить дерево по таблице
        let numSkip = numBForSkip inp -- посчитать сколько байт пропустить
        let inp2 = drop numSkip inp -- Убрать таблицу из данных
        let crc = fromIntegral (inp2 !! 3) --контрольная сумма
        let code = drop 4 inp2 -- убрать crc и 0-и
        let bits = codeToBits code crc --разбить код на биты
        decode (tail bits) [head bits] tree -- декодировать
    else map fromIntegral (drop 3 inp) -- оставить как есть