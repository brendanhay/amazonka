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
-- Module      : Network.AWS.ElastiCache.RebootCacheCluster
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots some, or all, of the cache nodes within a provisioned cache cluster. This operation applies any modified cache parameter groups to the cache cluster. The reboot operation takes place as soon as possible, and results in a momentary outage to the cache cluster. During the reboot, the cache cluster status is set to REBOOTING.
--
--
-- The reboot causes the contents of the cache (for each cache node being rebooted) to be lost.
--
-- When the reboot is complete, a cache cluster event is created.
--
module Network.AWS.ElastiCache.RebootCacheCluster
    (
    -- * Creating a Request
      rebootCacheCluster
    , RebootCacheCluster
    -- * Request Lenses
    , rccCacheClusterId
    , rccCacheNodeIdsToReboot

    -- * Destructuring the Response
    , rebootCacheClusterResponse
    , RebootCacheClusterResponse
    -- * Response Lenses
    , rccrsCacheCluster
    , rccrsResponseStatus
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @RebootCacheCluster@ operation.
--
--
--
-- /See:/ 'rebootCacheCluster' smart constructor.
data RebootCacheCluster = RebootCacheCluster'
  { _rccCacheClusterId       :: {-# NOUNPACK #-}!Text
  , _rccCacheNodeIdsToReboot :: {-# NOUNPACK #-}![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RebootCacheCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rccCacheClusterId' - The cache cluster identifier. This parameter is stored as a lowercase string.
--
-- * 'rccCacheNodeIdsToReboot' - A list of cache node IDs to reboot. A node ID is a numeric identifier (0001, 0002, etc.). To reboot an entire cache cluster, specify all of the cache node IDs.
rebootCacheCluster
    :: Text -- ^ 'rccCacheClusterId'
    -> RebootCacheCluster
rebootCacheCluster pCacheClusterId_ =
  RebootCacheCluster'
  {_rccCacheClusterId = pCacheClusterId_, _rccCacheNodeIdsToReboot = mempty}


-- | The cache cluster identifier. This parameter is stored as a lowercase string.
rccCacheClusterId :: Lens' RebootCacheCluster Text
rccCacheClusterId = lens _rccCacheClusterId (\ s a -> s{_rccCacheClusterId = a});

-- | A list of cache node IDs to reboot. A node ID is a numeric identifier (0001, 0002, etc.). To reboot an entire cache cluster, specify all of the cache node IDs.
rccCacheNodeIdsToReboot :: Lens' RebootCacheCluster [Text]
rccCacheNodeIdsToReboot = lens _rccCacheNodeIdsToReboot (\ s a -> s{_rccCacheNodeIdsToReboot = a}) . _Coerce;

instance AWSRequest RebootCacheCluster where
        type Rs RebootCacheCluster =
             RebootCacheClusterResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper "RebootCacheClusterResult"
              (\ s h x ->
                 RebootCacheClusterResponse' <$>
                   (x .@? "CacheCluster") <*> (pure (fromEnum s)))

instance Hashable RebootCacheCluster where

instance NFData RebootCacheCluster where

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
data RebootCacheClusterResponse = RebootCacheClusterResponse'
  { _rccrsCacheCluster   :: {-# NOUNPACK #-}!(Maybe CacheCluster)
  , _rccrsResponseStatus :: {-# NOUNPACK #-}!Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RebootCacheClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rccrsCacheCluster' - Undocumented member.
--
-- * 'rccrsResponseStatus' - -- | The response status code.
rebootCacheClusterResponse
    :: Int -- ^ 'rccrsResponseStatus'
    -> RebootCacheClusterResponse
rebootCacheClusterResponse pResponseStatus_ =
  RebootCacheClusterResponse'
  {_rccrsCacheCluster = Nothing, _rccrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
rccrsCacheCluster :: Lens' RebootCacheClusterResponse (Maybe CacheCluster)
rccrsCacheCluster = lens _rccrsCacheCluster (\ s a -> s{_rccrsCacheCluster = a});

-- | -- | The response status code.
rccrsResponseStatus :: Lens' RebootCacheClusterResponse Int
rccrsResponseStatus = lens _rccrsResponseStatus (\ s a -> s{_rccrsResponseStatus = a});

instance NFData RebootCacheClusterResponse where
