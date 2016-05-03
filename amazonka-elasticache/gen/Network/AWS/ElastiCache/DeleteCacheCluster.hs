{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DeleteCacheCluster
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
module Network.AWS.ElastiCache.DeleteCacheCluster
    (
    -- * Creating a Request
      deleteCacheCluster
    , DeleteCacheCluster
    -- * Request Lenses
    , dccFinalSnapshotIdentifier
    , dccCacheClusterId

    -- * Destructuring the Response
    , deleteCacheClusterResponse
    , DeleteCacheClusterResponse
    -- * Response Lenses
    , dccrsCacheCluster
    , dccrsResponseStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.ElastiCache.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /DeleteCacheCluster/ action.
--
-- /See:/ 'deleteCacheCluster' smart constructor.
data DeleteCacheCluster = DeleteCacheCluster'
    { _dccFinalSnapshotIdentifier :: !(Maybe Text)
    , _dccCacheClusterId          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteCacheCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dccFinalSnapshotIdentifier'
--
-- * 'dccCacheClusterId'
deleteCacheCluster
    :: Text -- ^ 'dccCacheClusterId'
    -> DeleteCacheCluster
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
        type Rs DeleteCacheCluster =
             DeleteCacheClusterResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper "DeleteCacheClusterResult"
              (\ s h x ->
                 DeleteCacheClusterResponse' <$>
                   (x .@? "CacheCluster") <*> (pure (fromEnum s)))

instance Hashable DeleteCacheCluster

instance NFData DeleteCacheCluster

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
data DeleteCacheClusterResponse = DeleteCacheClusterResponse'
    { _dccrsCacheCluster   :: !(Maybe CacheCluster)
    , _dccrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteCacheClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dccrsCacheCluster'
--
-- * 'dccrsResponseStatus'
deleteCacheClusterResponse
    :: Int -- ^ 'dccrsResponseStatus'
    -> DeleteCacheClusterResponse
deleteCacheClusterResponse pResponseStatus_ =
    DeleteCacheClusterResponse'
    { _dccrsCacheCluster = Nothing
    , _dccrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
dccrsCacheCluster :: Lens' DeleteCacheClusterResponse (Maybe CacheCluster)
dccrsCacheCluster = lens _dccrsCacheCluster (\ s a -> s{_dccrsCacheCluster = a});

-- | The response status code.
dccrsResponseStatus :: Lens' DeleteCacheClusterResponse Int
dccrsResponseStatus = lens _dccrsResponseStatus (\ s a -> s{_dccrsResponseStatus = a});

instance NFData DeleteCacheClusterResponse
