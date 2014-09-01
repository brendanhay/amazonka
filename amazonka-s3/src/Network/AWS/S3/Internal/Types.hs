{-# LANGUAGE DeriveGeneric #-}

-- Module      : Network.AWS.S3.Internal.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.S3.Internal.Types where

import Data.String
import GHC.Generics
import Network.AWS.Prelude

newtype BucketName = BucketName Text
    deriving (Eq, Show, Generic, IsString)

instance FromText     BucketName where parser = BucketName <$> takeText
instance ToText       BucketName where toText (BucketName b) = b
instance ToByteString BucketName
instance FromXML      BucketName
instance ToXML        BucketName
instance ToQuery      BucketName where toQuery = toQuery . toBS

newtype ObjectKey = ObjectKey Text
    deriving (Eq, Show, Generic, IsString)

instance FromText     ObjectKey where parser = ObjectKey <$> takeText
instance ToText       ObjectKey where toText (ObjectKey k) = k
instance ToByteString ObjectKey
instance FromXML      ObjectKey
instance ToXML        ObjectKey
instance ToQuery      ObjectKey where toQuery = toQuery . toBS

newtype ObjectVersionId = ObjectVersionId Text
    deriving (Eq, Show, Generic, IsString)

instance FromText     ObjectVersionId where parser = ObjectVersionId <$> takeText
instance ToText       ObjectVersionId where toText (ObjectVersionId v) = v
instance ToByteString ObjectVersionId
instance FromXML      ObjectVersionId
instance ToXML        ObjectVersionId
instance ToQuery      ObjectVersionId where toQuery = toQuery . toBS

newtype ETag = ETag Text
    deriving (Eq, Show, Generic, IsString)

instance FromText     ETag where parser = ETag <$> takeText
instance ToText       ETag where toText (ETag t) = t
instance ToByteString ETag
instance FromXML      ETag
instance ToXML        ETag
instance ToQuery      ETag where toQuery = toQuery . toBS
