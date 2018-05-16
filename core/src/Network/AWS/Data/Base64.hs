{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Network.AWS.Data.Base64
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Data.Base64
    ( Base64 (..)
    , _Base64
    ) where

import           Control.Applicative         (pure)
import           Control.DeepSeq
import           Data.Aeson.Types
import qualified Data.Attoparsec.Text        as AText
import qualified Data.ByteArray.Encoding     as BA
import           Data.Data                   (Data, Typeable)
import           Data.Hashable
import qualified Data.Text.Encoding          as Text
import           GHC.Generics                (Generic)
import           Network.AWS.Data.Body
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.JSON
import           Network.AWS.Data.Query
import           Network.AWS.Data.Text
import           Network.AWS.Data.XML
import           Network.AWS.Lens            (Iso', iso)

-- | Base64 encoded binary data.
--
-- Encoding\/decoding is automatically deferred to serialisation and deserialisation
-- respectively.
newtype Base64 = Base64 { unBase64 :: ByteString }
    deriving (Eq, Read, Ord, Data, Typeable, Generic)

instance Hashable Base64
instance NFData   Base64

_Base64 :: Iso' Base64 ByteString
_Base64 = iso unBase64 Base64

-- FIXME: probably a mistake to wrap a ByteString since
-- the underlying serialisers (JSON, XML) use Text internally.
instance FromText Base64 where
    parser = AText.takeText >>=
        either fail (pure . Base64)
            . BA.convertFromBase BA.Base64
            . Text.encodeUtf8

instance ToByteString Base64 where
    toBS = BA.convertToBase BA.Base64 . unBase64

instance Show         Base64 where show      = show . toBS
instance ToText       Base64 where toText    = Text.decodeUtf8 . toBS
instance ToQuery      Base64 where toQuery   = toQuery . toBS
instance FromXML      Base64 where parseXML  = parseXMLText "Base64"
instance ToXML        Base64 where toXML     = toXMLText
instance FromJSON     Base64 where parseJSON = parseJSONText "Base64"
instance ToJSON       Base64 where toJSON    = toJSONText
instance ToHashedBody Base64 where toHashed  = toHashed . toBS
instance ToBody       Base64
