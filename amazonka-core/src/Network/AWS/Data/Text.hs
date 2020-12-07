{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.AWS.Data.Text
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Data.Text
  ( Text,

    -- * Deserialisation
    FromText (..),
    parseText,

    -- * Serialisation
    ToText (..),
    showText,
  )
where

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Int
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LText
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Build
import qualified Data.Text.Lazy.Builder.Int as Build
import qualified Data.Text.Lazy.Builder.Scientific as Build
import Network.AWS.Data.Crypto
import Network.HTTP.Types
import Numeric
import Numeric.Natural

parseText :: FromText a => Parser a -> Text -> Either String a
parseText parser = A.parseOnly (parser <* A.endOfInput)

class FromText a where
  fromText :: Text -> Either String a

instance FromText Text where
  fromText = pure

instance FromText String where
  fromText = pure . Text.unpack

instance FromText ByteString where
  fromText = pure . Text.encodeUtf8

instance FromText Char where
  fromText = parseText A.anyChar

instance FromText Int where
  fromText = parseText (A.signed A.decimal)

instance FromText Int64 where
  fromText = parseText (A.signed A.decimal)

instance FromText Integer where
  fromText = parseText (A.signed A.decimal)

instance FromText Scientific where
  fromText = parseText (A.signed A.scientific)

instance FromText Natural where
  fromText = parseText A.decimal

instance FromText Double where
  fromText = parseText (A.signed A.rational)

instance FromText Bool where
  fromText = \case
    "true" -> pure True
    "false" -> pure False
    e -> Left ("Failure parsing Bool from " ++ show e)

instance FromText StdMethod where
  fromText =
    parseText $ do
      bs <- Text.encodeUtf8 <$> A.takeText
      either (fail . BS8.unpack) pure (parseMethod bs)

showText :: ToText a => a -> String
showText = Text.unpack . toText

class ToText a where
  toText :: a -> Text

instance ToText Text where
  toText = id

instance ToText ByteString where
  toText = Text.decodeUtf8

instance ToText Char where
  toText = Text.singleton

instance ToText String where
  toText = Text.pack

instance ToText Int where
  toText = shortText . Build.decimal

instance ToText Int64 where
  toText = shortText . Build.decimal

instance ToText Integer where
  toText = shortText . Build.decimal

instance ToText Natural where
  toText = shortText . Build.decimal

instance ToText Scientific where
  toText = shortText . Build.scientificBuilder

instance ToText Double where
  toText = toText . ($ "") . showFFloat Nothing

instance ToText StdMethod where
  toText = toText . renderStdMethod

instance ToText (Digest a) where
  toText = toText . digestToBase Base16

instance ToText Bool where
  toText = \case
    True -> "true"
    False -> "false"

shortText :: Builder -> Text
shortText = LText.toStrict . Build.toLazyTextWith 32
