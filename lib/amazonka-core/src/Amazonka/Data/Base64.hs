-- |
-- Module      : Amazonka.Data.Base64
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.Data.Base64
  ( Base64 (..),
    _Base64,
  )
where

import qualified Amazonka.Bytes as Bytes
import Amazonka.Core.Lens.Internal (iso)
import Amazonka.Data.Body
import Amazonka.Data.ByteString
import Amazonka.Data.JSON
import Amazonka.Data.Query
import Amazonka.Data.Text
import Amazonka.Data.XML
import Amazonka.Prelude
import qualified Data.Text.Encoding as Text

-- | Base64 encoded binary data.
--
-- Encoding\/decoding is automatically deferred to serialisation and deserialisation
-- respectively.
newtype Base64 = Base64 {unBase64 :: ByteString}
  deriving stock (Eq, Read, Ord, Generic)

instance Hashable Base64

instance NFData Base64

_Base64 :: Iso' Base64 ByteString
_Base64 = iso unBase64 Base64

-- FIXME: a mistake to wrap a ByteString since the underlying serialisers
-- (JSON, XML) use Text internally.
instance FromText Base64 where
  fromText = fmap Base64 . Bytes.decodeBase64 . Text.encodeUtf8

instance ToByteString Base64 where
  toBS = Bytes.encodeBase64 . unBase64

instance Show Base64 where
  show = show . toBS

instance ToText Base64 where
  toText = Text.decodeUtf8 . toBS

instance ToQuery Base64 where
  toQuery = toQuery . toBS

instance FromXML Base64 where
  parseXML = parseXMLText "Base64"

instance ToXML Base64 where
  toXML = toXMLText

instance FromJSON Base64 where
  parseJSON = parseJSONText "Base64"

instance ToJSON Base64 where
  toJSON = toJSONText

instance ToHashedBody Base64 where
  toHashed = toHashed . toBS

instance ToBody Base64
