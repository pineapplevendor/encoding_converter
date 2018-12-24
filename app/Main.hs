{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Conversions
import qualified Data.ByteString.Char8 as Char8 (pack, unpack)
import qualified Data.ByteString.Internal as StrictBS (ByteString)

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "Please visit /binary/, /hex/, /decimal/, /ascii/, or /base64/ followed by your value") <|>
    route [ ("binary/:value", handler (toEncodings (\x -> x)))
            , ("hex/:value", handler (toEncodings hexToBinary))
            , ("decimal/:value", handler (toEncodings decimalToBinary))
            , ("ascii/:value", handler (toEncodings asciiToBinary))
            , ("base64/:value", handler (toEncodings base64ToBinary))
    ]

handler :: ([Char] -> StrictBS.ByteString) -> Snap ()
handler toEncodings = do
    param <- getParam "value"
    case param of
        Nothing -> writeBS $ "Given input couldn't be parsed"
        Just inputParam -> writeBS $ toEncodings $ Char8.unpack $ inputParam 

