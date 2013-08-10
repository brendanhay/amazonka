{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.AWS.Internal.String
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.String where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.List
import           Data.Maybe

dropPrefix :: String -> String -> String
dropPrefix pre s = fromMaybe s $ pre `stripPrefix` s

dropSuffix :: String -> String -> String
dropSuffix suf s
    | suf `isSuffixOf` s = take (length s - length suf) s
    | otherwise          = s

dropLower :: String -> String
dropLower = dropWhile isLower

lowerFirst :: String -> String
lowerFirst (x:xs) = toLower x : xs
lowerFirst []     = []

lowerAll :: String -> String
lowerAll = map toLower

upperAll :: String -> String
upperAll = map toUpper

underscore :: String -> String
underscore (x:xs) | isUpper x = toLower x : underscore xs
underscore xs                 = concatMap f xs
  where
    f x = ['_' | isUpper x] ++ [toLower x]

strip :: Char -> ByteString -> ByteString
strip c bstr
    | BS.cons c "" == bstr = ""
    | otherwise = ($ bstr) $ case (BS.head bstr == c, BS.last bstr == c) of
        (True,  True)  -> BS.tail . BS.init
        (False, True)  -> BS.init
        (True,  False) -> BS.tail
        _              -> id
