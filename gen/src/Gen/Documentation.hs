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

import           Data.Monoid
import           Data.Text   (Text)
import qualified Data.Text   as Text

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
        . map (stripTags . Text.strip)
        $ Text.lines t

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

stripTags :: Text -> Text
stripTags t
    | Text.null t = Text.empty
    | otherwise   =
        case Text.head t of
            '<' -> stripTags . Text.drop 1 . Text.dropWhile (/= '>') $ Text.tail t
            _   -> Text.cons (Text.head t) . stripTags $ Text.tail t


-- formatTags :: Text -> Text

  -- <p>Acquires an Elastic IP address.</p>

  -- <p>An Elastic IP address is for use either in the EC2-Classic platform or in
  -- a VPC. For more information,
  -- see <a href=\"http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html\">Elastic
  -- IP Addresses</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>"

