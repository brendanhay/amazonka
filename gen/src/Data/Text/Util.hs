{-# LANGUAGE OverloadedStrings #-}

-- Module      : Data.Text.Util
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Data.Text.Util where

import           Data.Char
import           Data.Maybe
import           Data.Monoid
import           Data.String.CaseConversion
import           Data.Text                  (Text)
import qualified Data.Text                  as Text

accessor :: Text -> Text
accessor t
    | "_" `Text.isPrefixOf` t = t
    | otherwise               = "_" <> t

firstAcronym :: Text -> Maybe Text
firstAcronym t
    | Text.length x > 1 = Just x
    | otherwise         = Nothing
  where
    x = Text.init . Text.toLower $ Text.takeWhile isUpper t

firstWord :: Text -> Text
firstWord = Text.toLower . Text.pack . head . splitBy isUpper . Text.unpack

casedChars :: Text -> Text
casedChars = Text.toLower . Text.filter isUpper

numericSuffix :: Text -> Text
numericSuffix t
    | Text.null t                 = Text.singleton '1'
    | x <- Text.last t, isDigit x = Text.init t `Text.snoc` succ x
    | otherwise                   = t `Text.snoc` '1'

indent :: Int -> Text -> Text
indent n = Text.intercalate "\n"
    . map (Text.replicate n " " <>)
    . filter (not . Text.null)
    . Text.lines

pad :: Int -> Text -> Text
pad n = (<> Text.replicate n " ")

strip :: Text -> Text -> Text
strip delim = f Text.stripSuffix . f Text.stripPrefix
  where
    f g x = fromMaybe x $ g delim x

normalise :: Int -> Text -> [Text]
normalise n = wrapLines n
    . f
    . Text.strip
    . Text.unwords
    . map (stripTags . Text.strip)
    . Text.lines
  where
    f ""  = ""
    f " " = ""
    f t   = endWith "." t

wrapLines :: Int -> Text -> [Text]
wrapLines n = map (Text.pack . unwords) . go 0 [] . words . Text.unpack
    where
      go :: Int -> [String] -> [String] -> [[String]]
      go _ acc [] = [reverse acc]
      go k acc ws@(w : rest)
          | l >= n     = reverse acc : [w] : go 0 [] rest
          | k + l >= n = reverse acc       : go 0 [] ws
          | otherwise  = go (k + l + 1) (w : acc) rest
        where
          l = length w

endWith :: Text -> Text -> Text
endWith x y
    | x `Text.isSuffixOf` y = y
    | otherwise             = y <> x

stripTags :: Text -> Text
stripTags t
    | Text.null t = Text.empty
    | otherwise   =
        case Text.head t of
            '<' -> stripTags . Text.drop 1 . Text.dropWhile (/= '>') $ Text.tail t
            _   -> Text.cons (Text.head t) . stripTags $ Text.tail t
