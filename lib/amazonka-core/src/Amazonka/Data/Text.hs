-- |
-- Module      : Amazonka.Data.Text
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.Data.Text
  ( Text,

    -- * Deserialisation
    FromText (..),

    -- * Serialisation
    ToText (..),
    toTextCI,
    showText,
  )
where

import qualified Amazonka.Bytes as Bytes
import qualified Amazonka.Crypto as Crypto
import Amazonka.Prelude
import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as Build
import qualified Data.Text.Lazy.Builder.Int as Build
import qualified Data.Text.Lazy.Builder.Scientific as Build
import qualified Network.HTTP.Types as HTTP
import qualified Numeric

class FromText a where
  fromText :: Text -> Either String a

instance FromText Text where
  fromText = pure

instance FromText String where
  fromText = pure . Text.unpack

instance FromText ByteString where
  fromText = pure . Text.encodeUtf8

instance (CI.FoldCase a, FromText a) => FromText (CI a) where
  fromText = fmap CI.mk . fromText

instance FromText Char where
  fromText = A.parseOnly (A.anyChar <* A.endOfInput)

instance FromText Int where
  fromText = A.parseOnly (A.signed A.decimal <* A.endOfInput)

instance FromText Int64 where
  fromText = A.parseOnly (A.signed A.decimal <* A.endOfInput)

instance FromText Integer where
  fromText = A.parseOnly (A.signed A.decimal <* A.endOfInput)

instance FromText Scientific where
  fromText = A.parseOnly (A.signed A.scientific <* A.endOfInput)

instance FromText Natural where
  fromText = A.parseOnly (A.decimal <* A.endOfInput)

instance FromText Double where
  fromText = A.parseOnly (A.signed A.rational <* A.endOfInput)

instance FromText Bool where
  fromText text =
    case CI.mk text of
      "true" -> pure True
      "false" -> pure False
      other -> Left ("Failure parsing Bool from " ++ show other ++ ".")

instance FromText HTTP.StdMethod where
  fromText text =
    case HTTP.parseMethod (Text.encodeUtf8 text) of
      Left err -> Left (BS8.unpack err)
      Right ok -> pure ok

showText :: ToText a => a -> String
showText = Text.unpack . toText

class ToText a where
  toText :: a -> Text

instance ToText a => ToText (CI a) where
  toText = toText . CI.original

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
  toText = toText . ($ "") . Numeric.showFFloat Nothing

instance ToText HTTP.StdMethod where
  toText = toText . HTTP.renderStdMethod

instance ToText (Crypto.Digest a) where
  toText = toText . Bytes.encodeBase16

instance ToText Bool where
  toText True = "true"
  toText False = "false"

shortText :: TextBuilder -> Text
shortText = LText.toStrict . Build.toLazyTextWith 32

toTextCI :: ToText a => a -> CI Text
toTextCI = CI.mk . toText
