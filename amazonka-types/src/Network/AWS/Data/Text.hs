{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Data.Text
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Text
    (
    -- * Classes
    -- ** FromText
      FromText  (..)
    , fromText
    , readText

    -- ** ToText
    , ToText    (..)
    , showText
    ) where

import           Control.Applicative
import           Data.Attoparsec.Text             (Parser)
import qualified Data.Attoparsec.Text             as AText
import           Data.Int
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import qualified Data.Text.Lazy                   as LText
import           Data.Text.Lazy.Builder           (Builder)
import qualified Data.Text.Lazy.Builder           as Build
import qualified Data.Text.Lazy.Builder.Int       as Build
import qualified Data.Text.Lazy.Builder.RealFloat as Build

fromText :: FromText a => Text -> Either String a
fromText = AText.parseOnly parser

readText :: FromText a => ReadS a
readText = either (const []) (\x -> [(x, "")]) . fromText . Text.pack

class FromText a where
    parser :: Parser a

instance FromText Text    where parser = AText.takeText
instance FromText Int     where parser = AText.decimal
instance FromText Integer where parser = AText.decimal
instance FromText Double  where parser = AText.rational

instance FromText Bool where
    parser = f "false" False <|> f "true" True
      where
        f x y = AText.asciiCI x >> return y

showText :: ToText a => a -> String
showText = Text.unpack . toText

class ToText a where
    toText :: a -> Text

instance ToText Text    where toText = id
instance ToText Int     where toText = buildText
instance ToText Int64   where toText = buildText
instance ToText Integer where toText = buildText
instance ToText Double  where toText = buildText

buildText :: ToBuilder a => a -> Text
buildText = LText.toStrict . Build.toLazyText . build

buildShortText :: ToBuilder a => a -> Text
buildShortText = LText.toStrict . Build.toLazyTextWith 128 . build

class ToBuilder a where
    build :: a -> Builder

    default build :: ToText a => a -> Builder
    build = build . toText

instance ToBuilder Builder where build = id
instance ToBuilder Text    where build = Build.fromText
instance ToBuilder Char    where build = Build.singleton
instance ToBuilder [Char]  where build = Build.fromString
instance ToBuilder Int     where build = Build.decimal
instance ToBuilder Int64   where build = Build.decimal
instance ToBuilder Integer where build = Build.decimal
instance ToBuilder Double  where build = Build.realFloat
