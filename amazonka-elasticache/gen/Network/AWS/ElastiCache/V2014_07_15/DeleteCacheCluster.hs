{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.DeleteCacheCluster
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeleteCacheCluster operation deletes a previously provisioned cache
-- cluster. DeleteCacheCluster deletes all associated cache nodes, node
-- endpoints and the cache cluster itself. When you receive a successful
-- response from this operation, Amazon ElastiCache immediately begins
-- deleting the cache cluster; you cannot cancel or revert this operation.
-- https://elasticache.us-east-1.amazonaws.com/ ?Action=DeleteCacheCluster
-- &CacheClusterId=simcoprod43&Version=2014-03-24 &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= in-sync default.memcached1.4 simcoprod43 deleting 11211
-- simcoprod43.m2st2p.cfg.cache.amazonaws.com cache.m1.large memcached
-- us-east-1b 2014-03-27T02:18:26.497Z 1.4.5 true mon:05:00-mon:05:30 default
-- active 3 ab84aa7e-b7fa-11e0-9b0b-a9261be2b354.
module Network.AWS.ElastiCache.V2014_07_15.DeleteCacheCluster
    (
    -- * Request
      DeleteCacheCluster
    -- ** Request constructor
    , mkDeleteCacheCluster
    -- ** Request lenses
    , dccCacheClusterId
    , dccFinalSnapshotIdentifier

    -- * Response
    , DeleteCacheClusterResponse
    -- ** Response lenses
    , dccrsCacheCluster
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Represents the input of a DeleteCacheCluster operation.
data DeleteCacheCluster = DeleteCacheCluster
    { _dccCacheClusterId :: Text
    , _dccFinalSnapshotIdentifier :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteCacheCluster' request.
mkDeleteCacheCluster :: Text -- ^ 'dccCacheClusterId'
                     -> DeleteCacheCluster
mkDeleteCacheCluster p1 = DeleteCacheCluster
    { _dccCacheClusterId = p1
    , _dccFinalSnapshotIdentifier = Nothing
    }

-- | The cache cluster identifier for the cluster to be deleted. This parameter
-- is not case sensitive.
dccCacheClusterId :: Lens' DeleteCacheCluster Text
dccCacheClusterId =
    lens _dccCacheClusterId (\s a -> s { _dccCacheClusterId = a })

-- | The name of a final cache cluster snapshot. ElastiCache creates the
-- snapshot, and then deletes the cache cluster immediately afterward.
dccFinalSnapshotIdentifier :: Lens' DeleteCacheCluster (Maybe Text)
dccFinalSnapshotIdentifier =
    lens _dccFinalSnapshotIdentifier
         (\s a -> s { _dccFinalSnapshotIdentifier = a })

instance ToQuery DeleteCacheCluster where
    toQuery = genericQuery def

newtype DeleteCacheClusterResponse = DeleteCacheClusterResponse
    { _dccrsCacheCluster :: Maybe CacheCluster
    } deriving (Show, Generic)

-- | Contains all of the attributes of a specific cache cluster.
dccrsCacheCluster :: Lens' DeleteCacheClusterResponse (Maybe CacheCluster)
dccrsCacheCluster =
    lens _dccrsCacheCluster (\s a -> s { _dccrsCacheCluster = a })

instance FromXML DeleteCacheClusterResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteCacheCluster where
    type Sv DeleteCacheCluster = ElastiCache
    type Rs DeleteCacheCluster = DeleteCacheClusterResponse

    request = post "DeleteCacheCluster"
    response _ = xmlResponse
