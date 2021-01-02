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
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder.Int
import qualified Data.Text.Lazy.Builder.RealFloat as Builder.RealFloat
-- import qualified Data.Text.Lazy.Builder.Scientific as Builder.Scientific
import qualified Network.AWS.Data.Time as AWS.Time
import Network.AWS.Prelude

-- FIXME:
-- 1. have a Network.AWS.Prelude (qualify as "Prelude")
-- 2. have Network.AWS.Data (qualify as "Data")
-- 3. have Network.AWS.Lens (qualify as "Lens")

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

instance ToText Double where
  toText = buildText . Builder.RealFloat.realFloat
  {-# INLINEABLE toText #-}

instance ToText UTCTime where
  toText = AWS.Time.formatDateTime AWS.Time.iso8601Format
  {-# INLINEABLE toText #-}

instance ToText NominalDiffTime where
  toText = toText . AWS.Time.formatTimestamp
  {-# INLINEABLE toText #-}

-- instance ToText StdMethod where
--   toText = toText . renderStdMethod

-- instance ToText (Digest a) where
--   toText = toText . digestToBase Base16

buildText :: TextBuilder -> Text
buildText = Text.Lazy.toStrict . Builder.toLazyText

class FromText a where
  parseText :: Text -> Either Text a

parseUTF8 :: FromText a => ByteString -> Either Text a
parseUTF8 bytes = do
  text <- Bifunctor.first (Text.pack . show) (Text.Encoding.decodeUtf8' bytes)
  parseText text
{-# INLINEABLE parseUTF8 #-}

instance FromText Char where
  parseText = runTextParser "Char" Atto.anyChar
  {-# INLINEABLE parseText #-}

instance FromText Text where
  parseText = Right
  {-# INLINE parseText #-}

instance FromText Bool where
  parseText =
    annotate "Bool" . \case
      "true" -> Right True
      "false" -> Right False
      other -> Left ("unrecognised boolean " <> other)
  {-# INLINEABLE parseText #-}

-- instance FromText ByteString where
--   parseText = Right . Text.Encoding.encodeUtf8
--   {-# INLINEABLE parseText #-}

instance FromText Int where
  parseText = runTextParser "Int" (Atto.signed Atto.decimal)
  {-# INLINEABLE parseText #-}

instance FromText Integer where
  parseText = runTextParser "Integer" (Atto.signed Atto.decimal)
  {-# INLINEABLE parseText #-}

instance FromText Natural where
  parseText = runTextParser "Natural" Atto.decimal
  {-# INLINEABLE parseText #-}

instance FromText Scientific where
  parseText = runTextParser "Scientific" (Atto.signed Atto.scientific)
  {-# INLINEABLE parseText #-}

instance FromText Double where
  parseText = runTextParser "Double" (Atto.signed Atto.rational)
  {-# INLINEABLE parseText #-}

instance FromText UTCTime where
  parseText = AWS.Time.parseDateTime AWS.Time.iso8601Format . Text.unpack
  {-# INLINEABLE parseText #-}

instance FromText NominalDiffTime where
  parseText = AWS.Time.parseTimestamp
  {-# INLINEABLE parseText #-}

-- instance FromText StdMethod where
--   parseText =
--     parseText $ do
--       bs <- Text.encodeUtf8 <$> Atto.takeText
--       either (fail . BS8.unpack) pure (parseMethod bs)

runTextParser :: Text -> Atto.Parser a -> Text -> Either Text a
runTextParser ann parser =
  annotate ann
    . Bifunctor.first Text.pack
    . Atto.parseOnly (parser <* Atto.endOfInput)

annotate :: Text -> Either Text a -> Either Text a
annotate ann =
  Bifunctor.first $ \err ->
    "parseText." <> ann <> ": " <> err
