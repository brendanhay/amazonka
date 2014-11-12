{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.ElastiCache.DeleteCacheCluster
    (
    -- * Request
      DeleteCacheClusterMessage
    -- ** Request constructor
    , deleteCacheCluster
    -- ** Request lenses
    , dccmCacheClusterId
    , dccmFinalSnapshotIdentifier

    -- * Response
    , DeleteCacheClusterResult
    -- ** Response constructor
    , deleteCacheClusterResponse
    -- ** Response lenses
    , dccrCacheCluster
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data DeleteCacheClusterMessage = DeleteCacheClusterMessage
    { _dccmCacheClusterId          :: Text
    , _dccmFinalSnapshotIdentifier :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteCacheClusterMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dccmCacheClusterId' @::@ 'Text'
--
-- * 'dccmFinalSnapshotIdentifier' @::@ 'Maybe' 'Text'
--
deleteCacheCluster :: Text -- ^ 'dccmCacheClusterId'
                   -> DeleteCacheClusterMessage
deleteCacheCluster p1 = DeleteCacheClusterMessage
    { _dccmCacheClusterId          = p1
    , _dccmFinalSnapshotIdentifier = Nothing
    }

-- | The cache cluster identifier for the cluster to be deleted. This
-- parameter is not case sensitive.
dccmCacheClusterId :: Lens' DeleteCacheClusterMessage Text
dccmCacheClusterId =
    lens _dccmCacheClusterId (\s a -> s { _dccmCacheClusterId = a })

-- | The user-supplied name of a final cache cluster snapshot. This is the
-- unique name that identifies the snapshot. ElastiCache creates the
-- snapshot, and then deletes the cache cluster immediately afterward.
dccmFinalSnapshotIdentifier :: Lens' DeleteCacheClusterMessage (Maybe Text)
dccmFinalSnapshotIdentifier =
    lens _dccmFinalSnapshotIdentifier
        (\s a -> s { _dccmFinalSnapshotIdentifier = a })

instance ToQuery DeleteCacheClusterMessage

instance ToPath DeleteCacheClusterMessage where
    toPath = const "/"

newtype DeleteCacheClusterResult = DeleteCacheClusterResult
    { _dccrCacheCluster :: Maybe CacheCluster
    } deriving (Eq, Show, Generic)

-- | 'DeleteCacheClusterResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dccrCacheCluster' @::@ 'Maybe' 'CacheCluster'
--
deleteCacheClusterResponse :: DeleteCacheClusterResult
deleteCacheClusterResponse = DeleteCacheClusterResult
    { _dccrCacheCluster = Nothing
    }

dccrCacheCluster :: Lens' DeleteCacheClusterResult (Maybe CacheCluster)
dccrCacheCluster = lens _dccrCacheCluster (\s a -> s { _dccrCacheCluster = a })

instance FromXML DeleteCacheClusterResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteCacheClusterResult"

instance AWSRequest DeleteCacheClusterMessage where
    type Sv DeleteCacheClusterMessage = ElastiCache
    type Rs DeleteCacheClusterMessage = DeleteCacheClusterResult

    request  = post "DeleteCacheCluster"
    response = xmlResponse $ \h x -> DeleteCacheClusterResult
        <$> x %| "CacheCluster"
