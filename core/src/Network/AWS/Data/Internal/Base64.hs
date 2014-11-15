{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

-- Module      : Network.AWS.Data.Internal.Base64
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
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

import           Control.Monad
import           Data.Aeson.Types
import qualified Data.Attoparsec.Text                 as AText
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Base64               as Base64
import           Data.Tagged
import qualified Data.Text.Encoding                   as Text
import           GHC.Generics
import           Network.AWS.Data.Internal.ByteString
import           Network.AWS.Data.Internal.Query
import           Network.AWS.Data.Internal.Text
import           Network.AWS.Data.Internal.XML

-- | Base64 encoded binary data.
newtype Base64 = Base64 { unBase64 :: ByteString }
    deriving (Eq, Ord, Generic)

instance Show Base64 where
    show = show . toBS

instance ToByteString Base64 where
    toBS = Base64.encode . unBase64

instance FromText Base64 where
    parser = AText.takeText >>=
        either fail (return . Base64) . Base64.decode . Text.encodeUtf8

instance ToText Base64 where
    toText = Text.decodeUtf8 . toBS

instance FromXML Base64 where
    fromXML o = either Left (return . Base64)
        . join
        . fmap Base64.decode
        . fromXML (retag o)

instance ToXML Base64 where
    toXML o = toXML (retag o) . toText

instance ToQuery Base64 where
    toQuery = toQuery . toBS

instance FromJSON Base64 where
    parseJSON = withText "base64" (either fail (return . Base64) . fromText)

instance ToJSON Base64 where
    toJSON = String . toText
