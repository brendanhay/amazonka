{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Gen.Documentation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.Documentation where

import           Data.Foldable      (foldMap)
import           Data.Monoid
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Text.HTML.TagSoup

highlightType :: Text -> Text
highlightType = Text.unwords . map start . Text.words
  where
    start t
        | Text.null t                     = t
        | Just ('[', xs) <- Text.uncons t = "['" <> end xs
        | Just ('(', xs) <- Text.uncons t = "('" <> end xs
        | otherwise                       = "'"  <> end t

    end t
        | Text.null t        = t
        | ']' <- Text.last t = Text.init t <> "']"
        | ')' <- Text.last t = Text.init t <> "')"
        | otherwise          = t <> "'"

wrapDescription :: Text -> Text
wrapDescription = Text.intercalate "\n" . map (indent <>) . wrapLines 72
  where
    indent = Text.replicate 4 (Text.singleton ' ')

wrapHaddock :: Text -> Text -> Int -> Int -> Text
wrapHaddock start t n i
    | x:xs <- wrapped = Text.intercalate "\n" . map (indent <>) $ start <> x:xs
    | otherwise       = ""
  where
    indent = Text.replicate i (Text.singleton ' ') <> "-- "

    wrapped = wrapLines n
        . f
        . Text.strip
        . Text.unwords
        . Text.lines
        $ formatTags t

    f ""  = ""
    f " " = ""
    f x   = endWith "." x

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

formatTags :: Text -> Text
formatTags = foldMap parse . parseTags
  where
    parse :: Tag Text -> Text
    parse = \case
        TagOpen  "p" _             -> mempty
        TagClose "p"               -> "\n"
        TagOpen  "a" [("href", a)] -> "<" <> a <> " "
        TagClose "a"               -> ">"
        TagOpen  "i" _             -> "/"
        TagClose "i"               -> "/"
        TagText  t                 -> t
        _                          -> mempty
