{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.CreateSnapshot
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
module Network.AWS.ElastiCache.V2014_07_15.CreateSnapshot
    (
    -- * Request
      CreateSnapshot
    -- ** Request constructor
    , createSnapshot
    -- ** Request lenses
    , csnCacheClusterId
    , csnSnapshotName

    -- * Response
    , CreateSnapshotResponse
    -- ** Response lenses
    , ssssssrSnapshot
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateSnapshot' request.
createSnapshot :: Text -- ^ 'csnCacheClusterId'
               -> Text -- ^ 'csnSnapshotName'
               -> CreateSnapshot
createSnapshot p1 p2 = CreateSnapshot
    { _csnCacheClusterId = p1
    , _csnSnapshotName = p2
    }

data CreateSnapshot = CreateSnapshot
    { _csnCacheClusterId :: Text
      -- ^ The identifier of an existing cache cluster. The snapshot will be
      -- created from this cache cluster.
    , _csnSnapshotName :: Text
      -- ^ A name for the snapshot being created.
    } deriving (Show, Generic)

-- | The identifier of an existing cache cluster. The snapshot will be created
-- from this cache cluster.
csnCacheClusterId
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateSnapshot
    -> f CreateSnapshot
csnCacheClusterId f x =
    (\y -> x { _csnCacheClusterId = y })
       <$> f (_csnCacheClusterId x)
{-# INLINE csnCacheClusterId #-}

-- | A name for the snapshot being created.
csnSnapshotName
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateSnapshot
    -> f CreateSnapshot
csnSnapshotName f x =
    (\y -> x { _csnSnapshotName = y })
       <$> f (_csnSnapshotName x)
{-# INLINE csnSnapshotName #-}

instance ToQuery CreateSnapshot where
    toQuery = genericQuery def

data CreateSnapshotResponse = CreateSnapshotResponse
    { _ssssssrSnapshot :: Maybe Snapshot
      -- ^ Represents a copy of an entire cache cluster as of the time when
      -- the snapshot was taken.
    } deriving (Show, Generic)

-- | Represents a copy of an entire cache cluster as of the time when the
-- snapshot was taken.
ssssssrSnapshot
    :: Functor f
    => (Maybe Snapshot
    -> f (Maybe Snapshot))
    -> CreateSnapshotResponse
    -> f CreateSnapshotResponse
ssssssrSnapshot f x =
    (\y -> x { _ssssssrSnapshot = y })
       <$> f (_ssssssrSnapshot x)
{-# INLINE ssssssrSnapshot #-}

instance FromXML CreateSnapshotResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateSnapshot where
    type Sv CreateSnapshot = ElastiCache
    type Rs CreateSnapshot = CreateSnapshotResponse

    request = post "CreateSnapshot"
    response _ = xmlResponse
