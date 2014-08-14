-- Module      : Data.String.CaseConversion
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Data.String.CaseConversion
    (
      Case (..)
    , recase
    , lowered
    ) where

import Data.Char
import Data.List

data Case
    = Camel
    | Under
    | Hyphen

recase :: Case -> Case -> String -> String
recase x y = g y . f x
  where
    f Camel  = splitBy isUpper
    f Under  = uncased '_'
    f Hyphen = uncased '-'

    g Camel  = camelise
    g Under  = cased '_'
    g Hyphen = cased '-'

lowered :: String -> String
lowered = map toLower

camelise :: [String] -> String
camelise (x : xs) = concat (x : map (\(y : ys) -> toUpper y : lowered ys) xs)
camelise []       = []

cased :: Char -> [String] -> String
cased x xs = concat $ intersperse [x] (map lowered xs)

uncased :: Char -> String -> [String]
uncased x xs = y ++ map tail ys
  where
  (y, ys) = splitAt 1 (splitBy (== x) xs)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p = groupBy (const (not . p))
