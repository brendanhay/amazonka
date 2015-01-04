{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Data.Internal.Text
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
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
    , takeCI
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
{-# INLINE fromText #-}

takeCI :: Parser (CI Text)
takeCI = CI.mk <$> AText.takeText
{-# INLINE takeCI #-}

matchCI :: Text -> a -> Parser a
matchCI x y = AText.asciiCI x <* AText.endOfInput >> return y
{-# INLINE matchCI #-}

class FromText a where
    parser :: Parser a

instance FromText Text       where parser = AText.takeText
instance FromText ByteString where parser = Text.encodeUtf8 <$> AText.takeText
instance FromText Int        where parser = AText.decimal
instance FromText Integer    where parser = AText.decimal
instance FromText Scientific where parser = AText.scientific
instance FromText Natural    where parser = AText.decimal
instance FromText Double     where parser = AText.rational

instance FromText Bool where
    parser = matchCI "false" False <|> matchCI "true" True
    {-# INLINE parser #-}

showText :: ToText a => a -> String
showText = Text.unpack . toText
{-# INLINE showText #-}

class ToText a where
    toText :: a -> Text

instance (ToText a, ToText b) => ToText (a, b) where
    toText (a, b) = "(" <> toText a <> ", " <> toText b <> ")"
    {-# INLINE toText #-}

instance ToText a => ToText [a] where
    toText xs = "[" <> Text.intercalate ", " (map toText xs) <> "]"
    {-# INLINE toText #-}

instance ToText a => ToText (CI a) where
    toText = toText . CI.original
    {-# INLINE toText #-}

instance ToText (Response a) where
    toText rs = Text.pack . show $ rs { responseBody = () }
    {-# INLINE toText #-}

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
    {-# INLINE toText #-}

shortText :: Builder -> Text
shortText = LText.toStrict . Build.toLazyTextWith 32
{-# INLINE shortText #-}
