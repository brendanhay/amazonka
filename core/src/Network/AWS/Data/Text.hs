{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

-- |
-- Module      : Network.AWS.Data.Text
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Data.Text
    ( Text

    -- * Deserialisation
    , FromText (..)
    , fromText
    , fromTextError
    , takeLowerText
    , takeText

    -- * Serialisation
    , ToText   (..)
    , toTextCI
    , showText
    ) where

import           Data.Attoparsec.Text              (Parser)
import qualified Data.Attoparsec.Text              as A
import           Data.ByteString                   (ByteString)
import qualified Data.ByteString.Char8             as BS8
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
import qualified Data.Text.Lazy.Builder.Scientific as Build
import           Network.AWS.Data.Crypto
import           Network.HTTP.Types
import           Numeric
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

takeText :: Parser Text
takeText = A.takeText

class FromText a where
    parser :: Parser a

instance FromText Text where
    parser = A.takeText

instance FromText String where
    parser = Text.unpack <$> A.takeText

instance FromText ByteString where
    parser = Text.encodeUtf8 <$> A.takeText

instance (CI.FoldCase a, FromText a) => FromText (CI a) where
    parser = CI.mk <$> parser

instance FromText Char where
    parser = A.anyChar <* A.endOfInput

instance FromText Int where
    parser = A.signed A.decimal <* A.endOfInput

instance FromText Int64 where
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
    parser = takeLowerText >>= \case
        "true"  -> pure True
        "false" -> pure False
        e       -> fromTextError $ "Failure parsing Bool from '" <> e <> "'."

instance FromText StdMethod where
    parser = do
        bs <- Text.encodeUtf8 <$> A.takeText
        either (fail . BS8.unpack) pure (parseMethod bs)

showText :: ToText a => a -> String
showText = Text.unpack . toText

class ToText a where
    toText :: a -> Text

instance ToText a => ToText (CI a) where
    toText = toText . CI.original

instance ToText Text       where toText = id
instance ToText ByteString where toText = Text.decodeUtf8
instance ToText Char       where toText = Text.singleton
instance ToText String     where toText = Text.pack
instance ToText Int        where toText = shortText . Build.decimal
instance ToText Int64      where toText = shortText . Build.decimal
instance ToText Integer    where toText = shortText . Build.decimal
instance ToText Natural    where toText = shortText . Build.decimal
instance ToText Scientific where toText = shortText . Build.scientificBuilder
instance ToText Double     where toText = toText . ($ "") . showFFloat Nothing
instance ToText StdMethod  where toText = toText . renderStdMethod
instance ToText (Digest a) where toText = toText . digestToBase Base16

instance ToText Bool where
    toText True  = "true"
    toText False = "false"

shortText :: Builder -> Text
shortText = LText.toStrict . Build.toLazyTextWith 32

toTextCI :: ToText a => a -> CI Text
toTextCI = CI.mk . toText
