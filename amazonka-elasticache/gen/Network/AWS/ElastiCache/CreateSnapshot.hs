{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.CreateSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreateSnapshot operation creates a copy of an entire cache cluster at a
-- specific moment in time. https://elasticache.us-east-1.amazonaws.com/
-- ?Action=CreateSnapshot &CacheClusterId=my-redis-primary
-- &SnapshotName=my-manual-snapshot &Version=2014-03-24 &SignatureVersion=4
-- &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z &X-Amz-Credential=
-- my-redis-primary 6379 cache.m1.small default.redis2.8 redis us-east-1d
-- 2014-04-01T18:46:57.972Z 2.8.6 manual true wed:09:00-wed:10:00
-- my-manual-snapshot 5 2014-04-01T18:46:57.972Z 0001 creating 1 07:30-08:30
-- faf5a232-b9ce-11e3-8a16-7978bb24ffdf.
module Network.AWS.ElastiCache.CreateSnapshot
    (
    -- * Request
      CreateSnapshot
    -- ** Request constructor
    , createSnapshot
    -- ** Request lenses
    , cs1CacheClusterId
    , cs1SnapshotName

    -- * Response
    , CreateSnapshotResponse
    -- ** Response constructor
    , createSnapshotResponse
    -- ** Response lenses
    , csrrSnapshot
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import Network.AWS.Prelude

-- | Represents the input of a CreateSnapshotMessage operation.
data CreateSnapshot = CreateSnapshot
    { _cs1CacheClusterId :: Text
    , _cs1SnapshotName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateSnapshot' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheClusterId ::@ @Text@
--
-- * @SnapshotName ::@ @Text@
--
createSnapshot :: Text -- ^ 'cs1CacheClusterId'
               -> Text -- ^ 'cs1SnapshotName'
               -> CreateSnapshot
createSnapshot p1 p2 = CreateSnapshot
    { _cs1CacheClusterId = p1
    , _cs1SnapshotName = p2
    }

-- | The identifier of an existing cache cluster. The snapshot will be created
-- from this cache cluster.
cs1CacheClusterId :: Lens' CreateSnapshot Text
cs1CacheClusterId =
    lens _cs1CacheClusterId (\s a -> s { _cs1CacheClusterId = a })

-- | A name for the snapshot being created.
cs1SnapshotName :: Lens' CreateSnapshot Text
cs1SnapshotName = lens _cs1SnapshotName (\s a -> s { _cs1SnapshotName = a })

instance ToQuery CreateSnapshot where
    toQuery = genericQuery def

newtype CreateSnapshotResponse = CreateSnapshotResponse
    { _csrrSnapshot :: Maybe Snapshot
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateSnapshotResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Snapshot ::@ @Maybe Snapshot@
--
createSnapshotResponse :: CreateSnapshotResponse
createSnapshotResponse = CreateSnapshotResponse
    { _csrrSnapshot = Nothing
    }

-- | Represents a copy of an entire cache cluster as of the time when the
-- snapshot was taken.
csrrSnapshot :: Lens' CreateSnapshotResponse (Maybe Snapshot)
csrrSnapshot = lens _csrrSnapshot (\s a -> s { _csrrSnapshot = a })

instance FromXML CreateSnapshotResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateSnapshot where
    type Sv CreateSnapshot = ElastiCache
    type Rs CreateSnapshot = CreateSnapshotResponse

    request = post "CreateSnapshot"
    response _ = xmlResponse
