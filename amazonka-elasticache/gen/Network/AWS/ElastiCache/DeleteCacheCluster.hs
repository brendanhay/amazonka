{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.DeleteCacheCluster
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
-- This API cannot be used to delete a cache cluster that is the last read
-- replica of a replication group that has automatic failover mode enabled.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DeleteCacheCluster.html>
module Network.AWS.ElastiCache.DeleteCacheCluster
    (
    -- * Request
      DeleteCacheCluster
    -- ** Request constructor
    , deleteCacheCluster
    -- ** Request lenses
    , dccCacheClusterId
    , dccFinalSnapshotIdentifier

    -- * Response
    , DeleteCacheClusterResponse
    -- ** Response constructor
    , deleteCacheClusterResponse
    -- ** Response lenses
    , dccrCacheCluster
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data DeleteCacheCluster = DeleteCacheCluster
    { _dccCacheClusterId          :: Text
    , _dccFinalSnapshotIdentifier :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteCacheCluster' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dccCacheClusterId' @::@ 'Text'
--
-- * 'dccFinalSnapshotIdentifier' @::@ 'Maybe' 'Text'
--
deleteCacheCluster :: Text -- ^ 'dccCacheClusterId'
                   -> DeleteCacheCluster
deleteCacheCluster p1 = DeleteCacheCluster
    { _dccCacheClusterId          = p1
    , _dccFinalSnapshotIdentifier = Nothing
    }

-- | The cache cluster identifier for the cluster to be deleted. This
-- parameter is not case sensitive.
dccCacheClusterId :: Lens' DeleteCacheCluster Text
dccCacheClusterId =
    lens _dccCacheClusterId (\s a -> s { _dccCacheClusterId = a })

-- | The user-supplied name of a final cache cluster snapshot. This is the
-- unique name that identifies the snapshot. ElastiCache creates the
-- snapshot, and then deletes the cache cluster immediately afterward.
dccFinalSnapshotIdentifier :: Lens' DeleteCacheCluster (Maybe Text)
dccFinalSnapshotIdentifier =
    lens _dccFinalSnapshotIdentifier
        (\s a -> s { _dccFinalSnapshotIdentifier = a })

newtype DeleteCacheClusterResponse = DeleteCacheClusterResponse
    { _dccrCacheCluster :: Maybe CacheCluster
    } deriving (Eq, Show, Generic)

-- | 'DeleteCacheClusterResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dccrCacheCluster' @::@ 'Maybe' 'CacheCluster'
--
deleteCacheClusterResponse :: DeleteCacheClusterResponse
deleteCacheClusterResponse = DeleteCacheClusterResponse
    { _dccrCacheCluster = Nothing
    }

dccrCacheCluster :: Lens' DeleteCacheClusterResponse (Maybe CacheCluster)
dccrCacheCluster = lens _dccrCacheCluster (\s a -> s { _dccrCacheCluster = a })

instance ToPath DeleteCacheCluster where
    toPath = const "/"

instance ToQuery DeleteCacheCluster

instance ToHeaders DeleteCacheCluster

instance AWSRequest DeleteCacheCluster where
    type Sv DeleteCacheCluster = ElastiCache
    type Rs DeleteCacheCluster = DeleteCacheClusterResponse

    request  = post "DeleteCacheCluster"
    response = xmlResponse

instance FromXML DeleteCacheClusterResponse where
    parseXML x = DeleteCacheClusterResponse
        <$> x .@? "CacheCluster"
