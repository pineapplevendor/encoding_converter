module Conversions
(
 hexToBinary,
 binaryToHex,
 decimalToBinary,
 binaryToDecimal,
 asciiToBinary,
 binaryToAscii,
 binaryToBase64,
 base64ToBinary,
) where

import Numeric
import Data.Char
import Data.List
import qualified Data.ByteString.Base64 as B64 (encode, decodeLenient)
import qualified Data.ByteString.Char8 as Char8 (pack, unpack)

{-
testing:

hexToBinary $ binaryToHex $ decimalToBinary $ binaryToDecimal $ asciiToBinary $ binaryToAscii $ base64ToBinary $ binaryToBase64 "10101010"
-}

--hex related
hexToBinary :: [Char] -> [Char]
hexToBinary xs = decimalToBinary $ hexToDecimal xs

binaryToHex :: [Char] -> [Char]
binaryToHex xs = decimalToHex $ binaryToDecimal xs

hexToDecimal :: [Char] -> Int
hexToDecimal xs =  fst $ head $ readHex xs

decimalToHex :: Int -> [Char]
decimalToHex x = showHex x ""

--decimal related
decimalToBinary :: Int -> [Char]
decimalToBinary x = showIntAtBase 2 intToDigit x ""

binaryToDecimal :: [Char] -> Int
binaryToDecimal xs = fst $ head $ readInt 2 isValidBinaryDigit digitToInt xs

isValidBinaryDigit :: Char -> Bool
isValidBinaryDigit x
    | x == '0' = True
    | x == '1' = True
    | otherwise = False

{-
ascii related
output binary is separated by spaces to represent separate ascii chars
input is expected to come as binary numbers separated by spaces
-}
asciiToBinary :: [Char] -> [Char]
asciiToBinary xs = unwords $ map (decimalToBinary . fromEnum) xs 

binaryToAscii :: [Char] -> [Char]
binaryToAscii xs = map toEnum (map (binaryToDecimal) (words xs))

--base64 related
base64ToBinary :: [Char] -> [Char]
base64ToBinary xs = asciiToBinary $ base64ToAscii xs

binaryToBase64 :: [Char] -> [Char]
binaryToBase64 xs = asciiToBase64 $ binaryToAscii xs

asciiToBase64 :: [Char] -> [Char]
asciiToBase64 xs = Char8.unpack $ B64.encode $ Char8.pack xs

base64ToAscii :: [Char] -> [Char]
base64ToAscii xs = Char8.unpack $ B64.decodeLenient $ Char8.pack $ xs

