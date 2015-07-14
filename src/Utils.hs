module Utils where

import Data.Char (ord, chr)
import Data.Foldable (foldMap)
import Data.List (isPrefixOf)
import Data.Monoid

idToHash :: Int -> String
idToHash i | i <= 0 = error "invalid id"
idToHash i = let numberOfChars = length ['a'..'z']
                 firstCharNum = ord 'a'
           in map (chr.(+ firstCharNum).(`mod` numberOfChars))
           $ reverse 
           $ takeWhile (/= 0) 
           $ iterate (`div` numberOfChars) i

hashToId :: String -> Int
hashToId str | str == "" = error "empty string"
hashToId str = let numberOfChars = length ['a'..'z']
                   firstCharNum = ord 'a'
                   arrOfExps = reverse $ take (length str) $ iterate (* numberOfChars) 1
             in sum $ zipWith (*) arrOfExps $ map ((+ negate firstCharNum) . ord) str

prefixHttp :: String -> String
prefixHttp url = let hasPrefix = getAny $ foldMap (Any.(`isPrefixOf` url)) ["http://", "https://", "//"]
               in if not hasPrefix then "http://" <> url else url
