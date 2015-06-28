{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElastiCache.DeleteCacheCluster
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | The /DeleteCacheCluster/ action deletes a previously provisioned cache
-- cluster. /DeleteCacheCluster/ deletes all associated cache nodes, node
-- endpoints and the cache cluster itself. When you receive a successful
-- response from this action, Amazon ElastiCache immediately begins
-- deleting the cache cluster; you cannot cancel or revert this action.
--
-- This API cannot be used to delete a cache cluster that is the last read
-- replica of a replication group that has Multi-AZ mode enabled.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DeleteCacheCluster.html>
module Network.AWS.ElastiCache.DeleteCacheCluster
    (
    -- * Request
      DeleteCacheCluster
    -- ** Request constructor
    , deleteCacheCluster
    -- ** Request lenses
    , dccFinalSnapshotIdentifier
    , dccCacheClusterId

    -- * Response
    , DeleteCacheClusterResponse
    -- ** Response constructor
    , deleteCacheClusterResponse
    -- ** Response lenses
    , dccrCacheCluster
    , dccrStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /DeleteCacheCluster/ action.
--
-- /See:/ 'deleteCacheCluster' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dccFinalSnapshotIdentifier'
--
-- * 'dccCacheClusterId'
data DeleteCacheCluster = DeleteCacheCluster'
    { _dccFinalSnapshotIdentifier :: !(Maybe Text)
    , _dccCacheClusterId          :: !Text
    } deriving (Eq,Read,Show)

-- | 'DeleteCacheCluster' smart constructor.
deleteCacheCluster :: Text -> DeleteCacheCluster
deleteCacheCluster pCacheClusterId =
    DeleteCacheCluster'
    { _dccFinalSnapshotIdentifier = Nothing
    , _dccCacheClusterId = pCacheClusterId
    }

-- | The user-supplied name of a final cache cluster snapshot. This is the
-- unique name that identifies the snapshot. ElastiCache creates the
-- snapshot, and then deletes the cache cluster immediately afterward.
dccFinalSnapshotIdentifier :: Lens' DeleteCacheCluster (Maybe Text)
dccFinalSnapshotIdentifier = lens _dccFinalSnapshotIdentifier (\ s a -> s{_dccFinalSnapshotIdentifier = a});

-- | The cache cluster identifier for the cluster to be deleted. This
-- parameter is not case sensitive.
dccCacheClusterId :: Lens' DeleteCacheCluster Text
dccCacheClusterId = lens _dccCacheClusterId (\ s a -> s{_dccCacheClusterId = a});

instance AWSRequest DeleteCacheCluster where
        type Sv DeleteCacheCluster = ElastiCache
        type Rs DeleteCacheCluster =
             DeleteCacheClusterResponse
        request = post
        response
          = receiveXMLWrapper "DeleteCacheClusterResult"
              (\ s h x ->
                 DeleteCacheClusterResponse' <$>
                   (x .@? "CacheCluster") <*> (pure s))

instance ToHeaders DeleteCacheCluster where
        toHeaders = const mempty

instance ToPath DeleteCacheCluster where
        toPath = const "/"

instance ToQuery DeleteCacheCluster where
        toQuery DeleteCacheCluster'{..}
          = mconcat
              ["Action" =: ("DeleteCacheCluster" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "FinalSnapshotIdentifier" =:
                 _dccFinalSnapshotIdentifier,
               "CacheClusterId" =: _dccCacheClusterId]

-- | /See:/ 'deleteCacheClusterResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dccrCacheCluster'
--
-- * 'dccrStatus'
data DeleteCacheClusterResponse = DeleteCacheClusterResponse'
    { _dccrCacheCluster :: !(Maybe CacheCluster)
    , _dccrStatus       :: !Status
    } deriving (Eq,Show)

-- | 'DeleteCacheClusterResponse' smart constructor.
deleteCacheClusterResponse :: Status -> DeleteCacheClusterResponse
deleteCacheClusterResponse pStatus =
    DeleteCacheClusterResponse'
    { _dccrCacheCluster = Nothing
    , _dccrStatus = pStatus
    }

-- | FIXME: Undocumented member.
dccrCacheCluster :: Lens' DeleteCacheClusterResponse (Maybe CacheCluster)
dccrCacheCluster = lens _dccrCacheCluster (\ s a -> s{_dccrCacheCluster = a});

-- | FIXME: Undocumented member.
dccrStatus :: Lens' DeleteCacheClusterResponse Status
dccrStatus = lens _dccrStatus (\ s a -> s{_dccrStatus = a});
