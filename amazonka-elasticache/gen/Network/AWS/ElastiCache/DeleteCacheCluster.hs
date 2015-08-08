{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DeleteCacheCluster
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /DeleteCacheCluster/ action deletes a previously provisioned cache
-- cluster. /DeleteCacheCluster/ deletes all associated cache nodes, node
-- endpoints and the cache cluster itself. When you receive a successful
-- response from this action, Amazon ElastiCache immediately begins
-- deleting the cache cluster; you cannot cancel or revert this action.
--
-- This API cannot be used to delete a cache cluster that is the last read
-- replica of a replication group that has Multi-AZ mode enabled.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DeleteCacheCluster.html AWS API Reference> for DeleteCacheCluster.
module Network.AWS.ElastiCache.DeleteCacheCluster
    (
    -- * Creating a Request
      DeleteCacheCluster
    , deleteCacheCluster
    -- * Request Lenses
    , dccFinalSnapshotIdentifier
    , dccCacheClusterId

    -- * Destructuring the Response
    , DeleteCacheClusterResponse
    , deleteCacheClusterResponse
    -- * Response Lenses
    , dccrsCacheCluster
    , dccrsStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteCacheCluster' smart constructor.
deleteCacheCluster :: Text -> DeleteCacheCluster
deleteCacheCluster pCacheClusterId_ =
    DeleteCacheCluster'
    { _dccFinalSnapshotIdentifier = Nothing
    , _dccCacheClusterId = pCacheClusterId_
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
        request = postQuery
        response
          = receiveXMLWrapper "DeleteCacheClusterResult"
              (\ s h x ->
                 DeleteCacheClusterResponse' <$>
                   (x .@? "CacheCluster") <*> (pure (fromEnum s)))

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
-- * 'dccrsCacheCluster'
--
-- * 'dccrsStatus'
data DeleteCacheClusterResponse = DeleteCacheClusterResponse'
    { _dccrsCacheCluster :: !(Maybe CacheCluster)
    , _dccrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteCacheClusterResponse' smart constructor.
deleteCacheClusterResponse :: Int -> DeleteCacheClusterResponse
deleteCacheClusterResponse pStatus_ =
    DeleteCacheClusterResponse'
    { _dccrsCacheCluster = Nothing
    , _dccrsStatus = pStatus_
    }

-- | Undocumented member.
dccrsCacheCluster :: Lens' DeleteCacheClusterResponse (Maybe CacheCluster)
dccrsCacheCluster = lens _dccrsCacheCluster (\ s a -> s{_dccrsCacheCluster = a});

-- | Undocumented member.
dccrsStatus :: Lens' DeleteCacheClusterResponse Int
dccrsStatus = lens _dccrsStatus (\ s a -> s{_dccrsStatus = a});
