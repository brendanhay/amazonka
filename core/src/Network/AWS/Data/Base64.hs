{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Network.AWS.Data.Base64
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Data.Base64
    ( Base64
    ) where

import           Data.Aeson.Types
import qualified Data.Attoparsec.Text        as AText
import qualified Data.ByteArray.Encoding     as BA
import           Data.Data                   (Data, Typeable)
import qualified Data.Text.Encoding          as Text
import           GHC.Generics                (Generic)
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.JSON
import           Network.AWS.Data.Query
import           Network.AWS.Data.Text
import           Network.AWS.Data.XML

-- FIXME: this needs to be revisited.

-- | Base64 encoded binary data.
--
-- Encoding/decoding is deferred to serialisation/deserialisation respectively.
newtype Base64 = Base64 { unBase64 :: ByteString }
    deriving (Eq, Read, Ord, Data, Typeable, Generic)

-- FIXME: probably a mistake to wrap a ByteString since
-- the underlying serialisers (JSON, XML) use Text internally.
instance FromText Base64 where
    parser = AText.takeText >>=
        either fail (return . Base64)
            . BA.convertFromBase BA.Base64
            . Text.encodeUtf8

instance Show         Base64 where show      = show . toBS
instance ToText       Base64 where toText    = Text.decodeUtf8 . toBS
instance ToByteString Base64 where toBS      = BA.convertToBase BA.Base64 . unBase64
instance ToQuery      Base64 where toQuery   = toQuery . toText
instance FromXML      Base64 where parseXML  = parseXMLText "Base64"
instance ToXML        Base64 where toXML     = toXMLText
instance FromJSON     Base64 where parseJSON = parseJSONText "Base64"
instance ToJSON       Base64 where toJSON    = toJSONText
