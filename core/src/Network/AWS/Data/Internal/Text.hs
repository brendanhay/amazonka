{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Data.Internal.Text
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Internal.Text
    ( FromText (..)
    , fromText
    , takeLowerText
    , matchCI

    , ToText   (..)
    , showText
    ) where

import           Control.Applicative
import           Crypto.Hash
import           Data.Attoparsec.Text              (Parser)
import qualified Data.Attoparsec.Text              as AText
import           Data.ByteString                   (ByteString)
import           Data.CaseInsensitive              (CI)
import qualified Data.CaseInsensitive              as CI
import           Data.Int
import           Data.Monoid
import           Data.Scientific
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import qualified Data.Text.Encoding                as Text
import qualified Data.Text.Lazy                    as LText
import           Data.Text.Lazy.Builder            (Builder)
import qualified Data.Text.Lazy.Builder            as Build
import qualified Data.Text.Lazy.Builder.Int        as Build
import qualified Data.Text.Lazy.Builder.RealFloat  as Build
import qualified Data.Text.Lazy.Builder.Scientific as Build
import           Network.HTTP.Client
import           Network.HTTP.Types
import           Numeric.Natural

fromText :: FromText a => Text -> Either String a
fromText = AText.parseOnly parser

takeLowerText :: Parser Text
takeLowerText = Text.toLower <$> AText.takeText

matchCI :: Text -> a -> Parser a
matchCI x y = AText.asciiCI x <* AText.endOfInput >> return y

class FromText a where
    parser :: Parser a

instance FromText Text       where parser = AText.takeText
instance FromText ByteString where parser = Text.encodeUtf8 <$> AText.takeText
instance FromText Int        where parser = AText.signed AText.decimal
instance FromText Integer    where parser = AText.signed AText.decimal
instance FromText Scientific where parser = AText.signed AText.scientific
instance FromText Natural    where parser = AText.decimal
instance FromText Double     where parser = AText.signed AText.rational

instance FromText Bool where
    parser = matchCI "false" False <|> matchCI "true" True

showText :: ToText a => a -> String
showText = Text.unpack . toText

class ToText a where
    toText :: a -> Text

instance (ToText a, ToText b) => ToText (a, b) where
    toText (a, b) = "(" <> toText a <> ", " <> toText b <> ")"

instance ToText a => ToText [a] where
    toText xs = "[" <> Text.intercalate ", " (map toText xs) <> "]"

instance ToText a => ToText (CI a) where
    toText = toText . CI.original

instance ToText (Response a) where
    toText rs = Text.pack . show $ rs { responseBody = () }

instance ToText Text       where toText = id
instance ToText ByteString where toText = Text.decodeUtf8
instance ToText Int        where toText = shortText . Build.decimal
instance ToText Int64      where toText = shortText . Build.decimal
instance ToText Integer    where toText = shortText . Build.decimal
instance ToText Natural    where toText = shortText . Build.decimal
instance ToText Scientific where toText = shortText . Build.scientificBuilder
instance ToText Double     where toText = shortText . Build.realFloat
instance ToText StdMethod  where toText = toText . renderStdMethod
instance ToText (Digest a) where toText = toText . digestToHexByteString

instance ToText Bool where
    toText True  = "true"
    toText False = "false"

shortText :: Builder -> Text
shortText = LText.toStrict . Build.toLazyTextWith 32
