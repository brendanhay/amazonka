{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Module      : Network.AWS.Data.Internal.Base64
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Internal.Base64
    ( Base64
    ) where

import           Data.Aeson.Types
import qualified Data.Attoparsec.Text                 as AText
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Base64               as Base64
import qualified Data.Text.Encoding                   as Text
import           GHC.Generics
import           Network.AWS.Data.Internal.ByteString
import           Network.AWS.Data.Internal.JSON
import           Network.AWS.Data.Internal.Query
import           Network.AWS.Data.Internal.Text
import           Network.AWS.Data.Internal.XML

-- | Base64 encoded binary data.
newtype Base64 = Base64 { unBase64 :: ByteString }
    deriving (Eq, Read, Ord, Generic)

instance FromText Base64 where
    parser = AText.takeText >>=
        either fail (return . Base64) . Base64.decode . Text.encodeUtf8

instance Show         Base64 where show      = show . toBS
instance ToText       Base64 where toText    = Text.decodeUtf8 . toBS
instance ToByteString Base64 where toBS      = Base64.encode . unBase64
instance ToQuery      Base64 where toQuery   = toQuery . toText
instance FromXML      Base64 where parseXML  = parseXMLText "Base64"
instance ToXML        Base64 where toXML     = toXMLText
instance FromJSON     Base64 where parseJSON = parseJSONText "Base64"
instance ToJSON       Base64 where toJSON    = toJSONText
