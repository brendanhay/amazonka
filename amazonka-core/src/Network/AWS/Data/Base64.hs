-- |
-- Module      : Network.AWS.Data.Base64
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Data.Base64
  ( Base64 (..),
  )
where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import Network.AWS.Data.Body (ToBody (..), ToHashedBody (..))
import Network.AWS.Data.JSON (FromJSON, ToJSON)
import qualified Network.AWS.Data.JSON as AWS.JSON
import Network.AWS.Data.Query (ToQuery (..))
import Network.AWS.Data.Text (FromText (..), ToText (..))
import Network.AWS.Data.XML (FromXML, ToXML)
import qualified Network.AWS.Data.XML as AWS.XML
import Network.AWS.Prelude
import qualified Network.AWS.Bytes as Bytes

-- | Base64 encoded binary data.
--
-- Encoding\/decoding is automatically deferred to serialisation and deserialisation
-- respectively.
newtype Base64 = Base64 {fromBase64 :: ByteString}
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving newtype
    ( Hashable,
      NFData,
      Semigroup,
      Monoid
    )

instance ToText Base64 where
  toText =
    Text.Encoding.decodeUtf8
      . Bytes.encodeBase64
      . fromBase64
  {-# INLINEABLE toText #-}

instance FromText Base64 where
  parseText =
    Bifunctor.bimap Text.pack Base64
      . Bytes.decodeBase64 
      . Text.Encoding.encodeUtf8
  {-# INLINEABLE parseText #-}

instance ToXML Base64 where
  toXML = AWS.XML.toXML . toText
  {-# INLINEABLE toXML #-}

instance FromXML Base64 where
  parseXML = AWS.XML.withXMLText "Base64"
  {-# INLINEABLE parseXML #-}

instance ToJSON Base64 where
  toJSON = AWS.JSON.toJSON . toText
  {-# INLINEABLE toJSON #-}
  toEncoding = AWS.JSON.toEncoding . toText
  {-# INLINEABLE toEncoding #-}

instance FromJSON Base64 where
  parseJSON = AWS.JSON.withJSONText "Base64"
  {-# INLINEABLE parseJSON #-}

instance ToQuery Base64 where
  toQuery = toQuery . toText
  {-# INLINEABLE toQuery #-}

instance ToHashedBody Base64 where
  toHashedBody = toHashedBody . toText
  {-# INLINEABLE toHashedBody #-}

instance ToBody Base64 where
  toBody = toBody . toText
  {-# INLINEABLE toBody #-}
