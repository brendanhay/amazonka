{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

-- |
-- Module      : Network.AWS.Data.Text
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Data.Text
    ( Text

    -- * Deserialisation
    , FromText (..)
    , fromText
    , fromTextError
    , takeLowerText
    , matchCI

    -- * Serialisation
    , ToText   (..)
    , showText
    ) where

import           Control.Applicative
import           "cryptohash" Crypto.Hash
import           Data.Attoparsec.Text              (Parser)
import qualified Data.Attoparsec.Text              as A
import           Data.ByteString                   (ByteString)
import           Data.CaseInsensitive              (CI)
import qualified Data.CaseInsensitive              as CI
import           Data.Double.Conversion.Text       (toShortest)
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
import qualified Data.Text.Lazy.Builder.Scientific as Build
import           Network.HTTP.Client
import           Network.HTTP.Types
import           Numeric.Natural

-- | Fail parsing with a 'Text' error.
--
-- Constrained to the actual attoparsec monad to avoid
-- exposing 'fail' usage directly.
fromTextError :: Text -> Parser a
fromTextError = fail . Text.unpack

fromText :: FromText a => Text -> Either String a
fromText = A.parseOnly parser

takeLowerText :: Parser Text
takeLowerText = Text.toLower <$> A.takeText

matchCI :: Text -> a -> Parser a
matchCI x y = A.asciiCI x <* A.endOfInput >> return y

class FromText a where
    parser :: Parser a

instance FromText Text where
    parser = A.takeText

instance FromText ByteString where
    parser = Text.encodeUtf8 <$> A.takeText

instance FromText Char where
    parser = A.anyChar <* A.endOfInput

instance FromText Int where
    parser = A.signed A.decimal <* A.endOfInput

instance FromText Integer where
    parser = A.signed A.decimal <* A.endOfInput

instance FromText Scientific where
    parser = A.signed A.scientific <* A.endOfInput

instance FromText Natural where
    parser = A.decimal <* A.endOfInput

instance FromText Double where
    parser = A.signed A.rational <* A.endOfInput

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
instance ToText Char       where toText = Text.singleton
instance ToText Int        where toText = shortText . Build.decimal
instance ToText Int64      where toText = shortText . Build.decimal
instance ToText Integer    where toText = shortText . Build.decimal
instance ToText Natural    where toText = shortText . Build.decimal
instance ToText Scientific where toText = shortText . Build.scientificBuilder
instance ToText Double     where toText = toShortest
instance ToText StdMethod  where toText = toText . renderStdMethod
instance ToText (Digest a) where toText = toText . digestToHexByteString

instance ToText Bool where
    toText True  = "true"
    toText False = "false"

shortText :: Builder -> Text
shortText = LText.toStrict . Build.toLazyTextWith 32
