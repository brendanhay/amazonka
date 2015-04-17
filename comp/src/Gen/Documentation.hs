{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Gen.Documentation
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.Documentation
    ( Above (..)
    , Below (..)
    , Blank (..)
    , Doc
    , append
    , format
    ) where

import           Control.Applicative
import qualified Data.Aeson          as Aeson
import           Data.Default.Class
import           Data.Foldable       (foldMap)
import           Data.Jason
import           Data.Monoid
import           Data.String
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Gen.Text
import           Text.Pandoc

data Above a = Above Int a
data Below a = Below Int a
data Blank a = Blank Int a

data Doc = Doc Pandoc [Text]
    deriving (Eq)

instance Show Doc where
    show = Text.unpack . haddock 255

instance IsString Doc where
    fromString = Doc mempty . (:[]) . fromString

instance FromJSON Doc where
    parseJSON = withText "doc" (pure . format)

instance Aeson.ToJSON (Above Doc) where
    toJSON (Above n d) = Aeson.String (layout n "-- | " "-- " d)

instance Aeson.ToJSON (Below Doc) where
    toJSON (Below n d) = Aeson.String (layout n "-- ^ " "-- " d)

instance Aeson.ToJSON (Blank Doc) where
    toJSON (Blank n d) = Aeson.String (layout n mempty mempty d)

layout :: Int -> Text -> Text -> Doc -> Text
layout n start com = pref . Text.lines . haddock col
  where
    pref []     = mempty
    pref (x:xs) = begin <> x <> foldMap (mappend sep) xs

    begin = indent <> start
    sep   = "\n" <> indent <> com

    indent = Text.replicate pad (Text.singleton ' ')

    pad = abs n
    col = abs (80 - pad - 4)

append :: Doc -> Text -> Doc
append (Doc p xs) x = Doc p (xs ++ [x])

format :: Text -> Doc
format = (`Doc` []) . readHtml def . Text.unpack

haddock :: Int -> Doc -> Text
haddock col (Doc p xs) = Text.pack h <> sep <> Text.intercalate "\n" xs
  where
    sep | [] <- xs    = mempty
        | p == mempty = mempty
        | otherwise   = "\n"

    h = writeHaddock (def { writerColumns = col, writerWrapText = True }) p

