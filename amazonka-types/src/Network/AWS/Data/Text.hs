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
      FromText (..)
    , ToText   (..)

    -- * Instance helpers
    -- ** FromText
    , fromText
    , readText

    -- ** ToText
    , showText
    , integral
    , float
    , fromBuilder
    ) where

import           Control.Applicative
import           Data.Attoparsec.Text             (Parser)
import qualified Data.Attoparsec.Text             as AText
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import qualified Data.Text.Lazy                   as LText
import qualified Data.Text.Lazy.Builder           as LText
import qualified Data.Text.Lazy.Builder.Int       as LText
import qualified Data.Text.Lazy.Builder.RealFloat as LText

fromText :: FromText a => Text -> Either String a
fromText = AText.parseOnly parser

readText :: FromText a => ReadS a
readText = either (const []) (\x -> [(x, "")]) . fromText . Text.pack

class FromText a where
    parser :: Parser a

instance FromText Text    where parser = AText.takeText
instance FromText Int     where parser = AText.decimal
instance FromText Integer where parser = AText.decimal
instance FromText Float   where parser = AText.rational
instance FromText Double  where parser = AText.rational

instance FromText Bool where
    parser = f "false" False <|> f "true" True
      where
        f x y = AText.asciiCI x >> return y

showText :: ToText a => a -> String
showText = Text.unpack . toText

integral :: Integral a => a -> Text
integral = fromBuilder . LText.decimal

float :: RealFloat a => a -> Text
float = fromBuilder . LText.realFloat

fromBuilder :: LText.Builder -> Text
fromBuilder = LText.toStrict . LText.toLazyText

class ToText a where
    toText :: a -> Text

instance ToText Text    where toText = id
instance ToText Int     where toText = integral
instance ToText Integer where toText = integral
instance ToText Float   where toText = float
instance ToText Double  where toText = float
