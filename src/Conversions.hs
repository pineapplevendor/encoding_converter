{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Conversions
(
 hexToBinary,
 decimalToBinary,
 base64ToBinary,
 asciiToBinary,
 toEncodings,
) where

import Numeric
import Data.Char
import Data.List
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Base64 as B64 (encode, decodeLenient)
import qualified Data.ByteString.Char8 as Char8 (pack, unpack)
import qualified Data.ByteString.Lazy.Internal as LazyBS (ByteString)
import qualified Data.ByteString.Internal as StrictBS (ByteString)
import qualified Data.ByteString.Lazy as LazyBS (toStrict)

data Encodings = Encodings {
    binary :: String,
    hex :: String,
    decimal :: String,
    base64 :: String,
    ascii :: String
} deriving (Generic, Show)

instance ToJSON Encodings where toEncoding = genericToEncoding defaultOptions

binaryToEncodings :: [Char] -> StrictBS.ByteString
binaryToEncodings binary = LazyBS.toStrict $ encode $ encodings
    where encodings = Encodings {binary = binary,
                                 hex = binaryToHex binary,
                                 decimal = binaryToDecimal binary,
                                 base64 = binaryToBase64 binary,
                                 ascii = binaryToAscii binary}

toEncodings :: ([Char] -> [Char]) -> [Char] -> StrictBS.ByteString
toEncodings toBinary input = binaryToEncodings $ toBinary input

--hex related
hexToBinary :: [Char] -> [Char]
hexToBinary xs = decimalToBinary $ unwords $ map hexToDecimal (words xs)
    where hexToDecimal ys = show $ fst $ head $ readHex ys

binaryToHex :: [Char] -> [Char]
binaryToHex xs = unwords $ map (decimalToHex . read) (words $ binaryToDecimal xs)
    where decimalToHex x = showHex x ""

--decimal related
decimalToBinary :: [Char] -> [Char]
decimalToBinary xs = unwords $ map numToBinary (words xs)
    where numToBinary str = showIntAtBase 2 intToDigit (read str) "" 

binaryToDecimal :: [Char] -> [Char]
binaryToDecimal xs = unwords $ map (show . binaryNumToDecimal) (words xs)
    where binaryNumToDecimal ys = fst $ head $ readInt 2 isValidBinaryDigit digitToInt ys

isValidBinaryDigit :: Char -> Bool
isValidBinaryDigit x
    | x == '0' = True
    | x == '1' = True
    | otherwise = False

--ascii related
asciiToBinary :: [Char] -> [Char]
asciiToBinary xs = decimalToBinary $ unwords $ map (show . fromEnum) xs

binaryToAscii :: [Char] -> [Char]
binaryToAscii xs = map (toEnum . read) (words $ binaryToDecimal xs)

--base64 related
base64ToBinary :: [Char] -> [Char]
base64ToBinary xs = asciiToBinary $ base64ToAscii xs
    where base64ToAscii ys = Char8.unpack $ B64.decodeLenient $ Char8.pack ys

binaryToBase64 :: [Char] -> [Char]
binaryToBase64 xs = asciiToBase64 $ binaryToAscii xs
    where asciiToBase64 ys = Char8.unpack $ B64.encode $ Char8.pack ys

