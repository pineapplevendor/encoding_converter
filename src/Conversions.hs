{-# LANGUAGE OverloadedStrings #-}

module Conversions
(
 binaryToEncodings, 
 hexToEncodings,
 decimalToEncodings,
 base64ToEncodings,
 asciiToEncodings,
) where

import Numeric
import Data.Char
import Data.Aeson
import Data.Aeson
import Data.List
import Data.Aeson
import qualified Data.ByteString.Base64 as B64 (encode, decodeLenient)
import qualified Data.ByteString.Char8 as Char8 (pack, unpack)
import qualified Data.ByteString.Lazy.Internal as LazyBS (ByteString)
import qualified Data.ByteString.Internal as StrictBS (ByteString)
import qualified Data.ByteString.Lazy as LazyBS (toStrict)

data Encodings = Encodings {
    binary :: String,
    hex :: String,
    decimal :: [Int],
    base64 :: String,
    ascii :: String
} deriving Show

instance ToJSON Encodings where 
    toJSON (Encodings binary hex decimal base64 ascii) = object ["binary" .= binary, "hex" .= hex, 
        "decimal" .= decimal, "base64" .= base64, "ascii" .= ascii]

binaryToEncodings :: [Char] -> StrictBS.ByteString
binaryToEncodings binary =
    let hex = binaryToHex binary
        decimal = map binaryToDecimal (words binary)
        base64 = binaryToBase64 binary
        ascii = binaryToAscii binary
        encodings = (Encodings {binary = binary, hex = hex, decimal = decimal, base64 = base64, ascii = ascii })
    in LazyBS.toStrict $ encode $ encodings

hexToEncodings :: [Char] -> StrictBS.ByteString
hexToEncodings hex = binaryToEncodings $ hexToBinary hex

decimalToEncodings :: Int -> StrictBS.ByteString
decimalToEncodings decimal = binaryToEncodings $ decimalToBinary decimal

base64ToEncodings :: [Char] -> StrictBS.ByteString
base64ToEncodings base64 = binaryToEncodings $ base64ToBinary base64

asciiToEncodings :: [Char] -> StrictBS.ByteString 
asciiToEncodings ascii = binaryToEncodings $ asciiToBinary ascii

--hex related
hexToBinary :: [Char] -> [Char]
hexToBinary xs = unwords $ map (decimalToBinary . hexToDecimal) (words xs)

binaryToHex :: [Char] -> [Char]
binaryToHex xs = unwords $ map (decimalToHex . binaryToDecimal) (words xs)

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

--ascii related
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

