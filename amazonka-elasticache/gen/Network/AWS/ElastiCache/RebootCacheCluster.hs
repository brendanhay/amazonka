{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.RebootCacheCluster
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /RebootCacheCluster/ action reboots some, or all, of the cache nodes
-- within a provisioned cache cluster. This API will apply any modified
-- cache parameter groups to the cache cluster. The reboot action takes
-- place as soon as possible, and results in a momentary outage to the
-- cache cluster. During the reboot, the cache cluster status is set to
-- REBOOTING.
--
-- The reboot causes the contents of the cache (for each cache node being
-- rebooted) to be lost.
--
-- When the reboot is complete, a cache cluster event is created.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_RebootCacheCluster.html>
module Network.AWS.ElastiCache.RebootCacheCluster
    (
    -- * Request
      RebootCacheCluster
    -- ** Request constructor
    , rebootCacheCluster
    -- ** Request lenses
    , rccCacheClusterId
    , rccCacheNodeIdsToReboot

    -- * Response
    , RebootCacheClusterResponse
    -- ** Response constructor
    , rebootCacheClusterResponse
    -- ** Response lenses
    , rccrsCacheCluster
    , rccrsStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /RebootCacheCluster/ action.
--
-- /See:/ 'rebootCacheCluster' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rccCacheClusterId'
--
-- * 'rccCacheNodeIdsToReboot'
data RebootCacheCluster = RebootCacheCluster'
    { _rccCacheClusterId       :: !Text
    , _rccCacheNodeIdsToReboot :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RebootCacheCluster' smart constructor.
rebootCacheCluster :: Text -> RebootCacheCluster
rebootCacheCluster pCacheClusterId_ =
    RebootCacheCluster'
    { _rccCacheClusterId = pCacheClusterId_
    , _rccCacheNodeIdsToReboot = mempty
    }

-- | The cache cluster identifier. This parameter is stored as a lowercase
-- string.
rccCacheClusterId :: Lens' RebootCacheCluster Text
rccCacheClusterId = lens _rccCacheClusterId (\ s a -> s{_rccCacheClusterId = a});

-- | A list of cache node IDs to reboot. A node ID is a numeric identifier
-- (0001, 0002, etc.). To reboot an entire cache cluster, specify all of
-- the cache node IDs.
rccCacheNodeIdsToReboot :: Lens' RebootCacheCluster [Text]
rccCacheNodeIdsToReboot = lens _rccCacheNodeIdsToReboot (\ s a -> s{_rccCacheNodeIdsToReboot = a}) . _Coerce;

instance AWSRequest RebootCacheCluster where
        type Sv RebootCacheCluster = ElastiCache
        type Rs RebootCacheCluster =
             RebootCacheClusterResponse
        request = postQuery
        response
          = receiveXMLWrapper "RebootCacheClusterResult"
              (\ s h x ->
                 RebootCacheClusterResponse' <$>
                   (x .@? "CacheCluster") <*> (pure (fromEnum s)))

instance ToHeaders RebootCacheCluster where
        toHeaders = const mempty

instance ToPath RebootCacheCluster where
        toPath = const mempty

instance ToQuery RebootCacheCluster where
        toQuery RebootCacheCluster'{..}
          = mconcat
              ["Action" =: ("RebootCacheCluster" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheClusterId" =: _rccCacheClusterId,
               "CacheNodeIdsToReboot" =:
                 toQueryList "CacheNodeId" _rccCacheNodeIdsToReboot]

-- | /See:/ 'rebootCacheClusterResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rccrsCacheCluster'
--
-- * 'rccrsStatus'
data RebootCacheClusterResponse = RebootCacheClusterResponse'
    { _rccrsCacheCluster :: !(Maybe CacheCluster)
    , _rccrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RebootCacheClusterResponse' smart constructor.
rebootCacheClusterResponse :: Int -> RebootCacheClusterResponse
rebootCacheClusterResponse pStatus_ =
    RebootCacheClusterResponse'
    { _rccrsCacheCluster = Nothing
    , _rccrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
rccrsCacheCluster :: Lens' RebootCacheClusterResponse (Maybe CacheCluster)
rccrsCacheCluster = lens _rccrsCacheCluster (\ s a -> s{_rccrsCacheCluster = a});

-- | FIXME: Undocumented member.
rccrsStatus :: Lens' RebootCacheClusterResponse Int
rccrsStatus = lens _rccrsStatus (\ s a -> s{_rccrsStatus = a});
