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
  ( -- * Serialisation
    ToText (..),
    toUTF8,

    -- * Deserialisation
    FromText (..),
    parseUTF8,
  )
where

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder.Int
import qualified Data.Text.Lazy.Builder.RealFloat as Builder.RealFloat
import qualified Data.Text.Lazy.Builder.Scientific as Builder.Scientific
import Data.Time (Day (..), UTCTime (..))
import qualified Network.AWS.Data.Time as AWS.Time
import Network.AWS.Prelude
import qualified Network.HTTP.Types as HTTP.Types
import qualified Numeric

-- 1. have a Network.AWS.Prelude (qualify as "Prelude")
-- 2. have Network.AWS.Data (qualify as "Data")
-- 3. have Network.AWS.Lens (qualify as "Lens")

-- parseText :: Parser a -> Text -> Either Text a
-- parseText parser = A.parseOnly (parser <* A.endOfInput)

class ToText a where
  toText :: a -> Text

-- unpackText :: ToText a => a -> String
-- unpackText = Text.unpack . toText
-- {-# INLINEABLE unpackText #-}

toUTF8 :: ToText a => a -> ByteString
toUTF8 = Text.Encoding.encodeUtf8 . toText
{-# INLINEABLE toUTF8 #-}

instance ToText Char where
  toText = Text.singleton
  {-# INLINEABLE toText #-}

instance ToText Text where
  toText = id
  {-# INLINE toText #-}

instance ToText Bool where
  toText = \case
    True -> "true"
    False -> "false"
  {-# INLINEABLE toText #-}

instance ToText Int where
  toText = buildText . Builder.Int.decimal
  {-# INLINEABLE toText #-}

instance ToText Integer where
  toText = buildText . Builder.Int.decimal
  {-# INLINEABLE toText #-}

instance ToText Natural where
  toText = buildText . Builder.Int.decimal
  {-# INLINEABLE toText #-}

instance ToText Scientific where
  toText = buildText . Builder.Scientific.scientificBuilder
  {-# INLINEABLE toText #-}

instance ToText Double where
  toText = buildText . Builder.RealFloat.realFloat
  {-# INLINEABLE toText #-}

instance ToText UTCTime where
  toText = AWS.Time.formatDateTime AWS.Time.iso8601Format
  {-# INLINEABLE toText #-}

instance ToText NominalDiffTime where
  toText = toText . AWS.Time.formatTimestamp
  {-# INLINEABLE toText #-}

buildText :: TextBuilder -> Text
buildText = Text.Lazy.toStrict . Builder.toLazyText

class FromText a where
  parseText :: Text -> Either Text a

parseUTF8 :: FromText a => ByteString -> Either Text a
parseUTF8 bytes = do
  text <- Bifunctor.first (Text.pack . show) (Text.Encoding.decodeUtf8' bytes)
  parseText text
{-# INLINEABLE parseUTF8 #-}

instance FromText Text where
  parseText = Right
  {-# INLINEABLE parseText #-}

-- instance FromText ByteString where
--   parseText = Right . Text.Encoding.encodeUtf8
--   {-# INLINEABLE parseText #-}

-- instance FromText Text where
--   fromText = pure

-- instance FromText String where
--   fromText = pure . Text.unpack

-- instance FromText ByteString where
--   fromText = pure . Text.encodeUtf8

-- instance FromText Char where
--   fromText = parseText A.anyChar

-- instance FromText Int where
--   fromText = parseText (A.signed A.decimal)

-- instance FromText Int64 where
--   fromText = parseText (A.signed A.decimal)

-- instance FromText Integer where
--   fromText = parseText (A.signed A.decimal)

-- instance FromText Scientific where
--   fromText = parseText (A.signed A.scientific)

-- instance FromText Natural where
--   fromText = parseText A.decimal

-- instance FromText Double where
--   fromText = parseText (A.signed A.rational)

-- instance FromText UTCTime where
--   fromText = parseDateTime iso8601Format . Text.unpack

-- instance FromText Bool where
--   fromText = \case
--     "true" -> pure True
--     "false" -> pure False
--     e -> Left ("failure parsing Bool from " ++ show e)

-- instance FromText StdMethod where
--   fromText =
--     parseText $ do
--       bs <- Text.encodeUtf8 <$> A.takeText
--       either (fail . BS8.unpack) pure (parseMethod bs)

-- instance ToText ByteString where
--   toText = Text.decodeUtf8

-- instance ToText Char where
--   toText = Text.singleton

-- instance ToText String where
--   toText = Text.pack

-- instance ToText Int where
--   toText = shortText . Build.decimal

-- instance ToText Int64 where
--   toText = shortText . Build.decimal

-- instance ToText Integer where
--   toText = shortText . Build.decimal

-- instance ToText Natural where
--   toText = shortText . Build.decimal

-- instance ToText StdMethod where
--   toText = toText . renderStdMethod

-- instance ToText (Digest a) where
--   toText = toText . digestToBase Base16

-- instance ToText UTCTime where
--   toText = Text.pack . formatDateTime iso8601Format

-- instance ToText Bool where
--   toText = \case
--     True -> "true"
--     False -> "false"
