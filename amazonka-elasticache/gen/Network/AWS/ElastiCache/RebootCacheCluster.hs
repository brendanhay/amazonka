{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElastiCache.RebootCacheCluster
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

-- | The /RebootCacheCluster/ action reboots some, or all, of the cache nodes
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
    , rccrCacheCluster
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'rebootCacheCluster' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rccCacheClusterId'
--
-- * 'rccCacheNodeIdsToReboot'
data RebootCacheCluster = RebootCacheCluster'{_rccCacheClusterId :: Text, _rccCacheNodeIdsToReboot :: [Text]} deriving (Eq, Read, Show)

-- | 'RebootCacheCluster' smart constructor.
rebootCacheCluster :: Text -> RebootCacheCluster
rebootCacheCluster pCacheClusterId = RebootCacheCluster'{_rccCacheClusterId = pCacheClusterId, _rccCacheNodeIdsToReboot = mempty};

-- | The cache cluster identifier. This parameter is stored as a lowercase
-- string.
rccCacheClusterId :: Lens' RebootCacheCluster Text
rccCacheClusterId = lens _rccCacheClusterId (\ s a -> s{_rccCacheClusterId = a});

-- | A list of cache node IDs to reboot. A node ID is a numeric identifier
-- (0001, 0002, etc.). To reboot an entire cache cluster, specify all of
-- the cache node IDs.
rccCacheNodeIdsToReboot :: Lens' RebootCacheCluster [Text]
rccCacheNodeIdsToReboot = lens _rccCacheNodeIdsToReboot (\ s a -> s{_rccCacheNodeIdsToReboot = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest RebootCacheCluster where
        type Sv RebootCacheCluster = ElastiCache
        type Rs RebootCacheCluster =
             RebootCacheClusterResponse
        request = post
        response
          = receiveXMLWrapper "RebootCacheClusterResult"
              (\ s h x ->
                 RebootCacheClusterResponse' <$>
                   (x .@? "CacheCluster"))

instance ToHeaders RebootCacheCluster where
        toHeaders = const mempty

instance ToPath RebootCacheCluster where
        toPath = const "/"

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
-- * 'rccrCacheCluster'
newtype RebootCacheClusterResponse = RebootCacheClusterResponse'{_rccrCacheCluster :: Maybe CacheCluster} deriving (Eq, Read, Show)

-- | 'RebootCacheClusterResponse' smart constructor.
rebootCacheClusterResponse :: RebootCacheClusterResponse
rebootCacheClusterResponse = RebootCacheClusterResponse'{_rccrCacheCluster = Nothing};

-- | FIXME: Undocumented member.
rccrCacheCluster :: Lens' RebootCacheClusterResponse (Maybe CacheCluster)
rccrCacheCluster = lens _rccrCacheCluster (\ s a -> s{_rccrCacheCluster = a});
