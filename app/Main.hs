{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Conversions
import qualified Data.ByteString.Char8 as Char8 (pack, unpack)

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "Please visit /binary/, /hex/, /decimal/, /ascii/, or /base64/ followed by your value") <|>
    route [ ("binary/:binaryValue", binaryHandler)
            , ("hex/:hexValue", hexHandler)
            , ("decimal/:decimalValue", decimalHandler)
            , ("ascii/:asciiValue", asciiHandler)
            , ("base64/:base64Value", base64Handler)
    ]

binaryHandler :: Snap ()
binaryHandler = do
    param <- getParam "binaryValue"
    case param of
        Nothing -> writeBS $ "Given binary input couldn't be parsed"
        Just binaryParam -> writeBS $ binaryToEncodings $ Char8.unpack $ binaryParam 

hexHandler :: Snap ()
hexHandler = do
    param <- getParam "hexValue"
    case param of
        Nothing -> writeBS $ "Given hex input couldn't be parsed"
        Just binaryParam -> writeBS $ hexToEncodings $ Char8.unpack $ binaryParam 

decimalHandler :: Snap ()
decimalHandler = do
    param <- getParam "decimalValue"
    case param of
        Nothing -> writeBS $ "Given decimal input couldn't be parsed"
        Just binaryParam -> writeBS $ decimalToEncodings $ read $ Char8.unpack $ binaryParam 

base64Handler :: Snap ()
base64Handler = do
    param <- getParam "base64Value"
    case param of
        Nothing -> writeBS $ "Given base64 input couldn't be parsed"
        Just binaryParam -> writeBS $ base64ToEncodings $ Char8.unpack $ binaryParam 

asciiHandler :: Snap ()
asciiHandler = do
    param <- getParam "asciiValue"
    case param of
        Nothing -> writeBS $ "Given ascii input couldn't be parsed"
        Just binaryParam -> writeBS $ asciiToEncodings $ Char8.unpack $ binaryParam 
