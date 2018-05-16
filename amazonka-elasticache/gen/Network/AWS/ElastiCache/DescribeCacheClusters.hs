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
-- Module      : Network.AWS.ElastiCache.DescribeCacheClusters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all provisioned clusters if no cluster identifier is specified, or about a specific cache cluster if a cluster identifier is supplied.
--
--
-- By default, abbreviated information about the clusters is returned. You can use the optional /ShowCacheNodeInfo/ flag to retrieve detailed information about the cache nodes associated with the clusters. These details include the DNS address and port for the cache node endpoint.
--
-- If the cluster is in the /creating/ state, only cluster-level information is displayed until all of the nodes are successfully provisioned.
--
-- If the cluster is in the /deleting/ state, only cluster-level information is displayed.
--
-- If cache nodes are currently being added to the cluster, node endpoint information and creation time for the additional nodes are not displayed until they are completely provisioned. When the cluster state is /available/ , the cluster is ready for use.
--
-- If cache nodes are currently being removed from the cluster, no endpoint information for the removed nodes is displayed.
--
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeCacheClusters
    (
    -- * Creating a Request
      describeCacheClusters
    , DescribeCacheClusters
    -- * Request Lenses
    , dShowCacheClustersNotInReplicationGroups
    , dCacheClusterId
    , dMarker
    , dMaxRecords
    , dShowCacheNodeInfo

    -- * Destructuring the Response
    , describeCacheClustersResponse
    , DescribeCacheClustersResponse
    -- * Response Lenses
    , drsCacheClusters
    , drsMarker
    , drsResponseStatus
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @DescribeCacheClusters@ operation.
--
--
--
-- /See:/ 'describeCacheClusters' smart constructor.
data DescribeCacheClusters = DescribeCacheClusters'
  { _dShowCacheClustersNotInReplicationGroups :: !(Maybe Bool)
  , _dCacheClusterId                          :: !(Maybe Text)
  , _dMarker                                  :: !(Maybe Text)
  , _dMaxRecords                              :: !(Maybe Int)
  , _dShowCacheNodeInfo                       :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCacheClusters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dShowCacheClustersNotInReplicationGroups' - An optional flag that can be included in the @DescribeCacheCluster@ request to show only nodes (API/CLI: clusters) that are not members of a replication group. In practice, this mean Memcached and single node Redis clusters.
--
-- * 'dCacheClusterId' - The user-supplied cluster identifier. If this parameter is specified, only information about that specific cluster is returned. This parameter isn't case sensitive.
--
-- * 'dMarker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved. Default: 100 Constraints: minimum 20; maximum 100.
--
-- * 'dShowCacheNodeInfo' - An optional flag that can be included in the @DescribeCacheCluster@ request to retrieve information about the individual cache nodes.
describeCacheClusters
    :: DescribeCacheClusters
describeCacheClusters =
  DescribeCacheClusters'
    { _dShowCacheClustersNotInReplicationGroups = Nothing
    , _dCacheClusterId = Nothing
    , _dMarker = Nothing
    , _dMaxRecords = Nothing
    , _dShowCacheNodeInfo = Nothing
    }


-- | An optional flag that can be included in the @DescribeCacheCluster@ request to show only nodes (API/CLI: clusters) that are not members of a replication group. In practice, this mean Memcached and single node Redis clusters.
dShowCacheClustersNotInReplicationGroups :: Lens' DescribeCacheClusters (Maybe Bool)
dShowCacheClustersNotInReplicationGroups = lens _dShowCacheClustersNotInReplicationGroups (\ s a -> s{_dShowCacheClustersNotInReplicationGroups = a})

-- | The user-supplied cluster identifier. If this parameter is specified, only information about that specific cluster is returned. This parameter isn't case sensitive.
dCacheClusterId :: Lens' DescribeCacheClusters (Maybe Text)
dCacheClusterId = lens _dCacheClusterId (\ s a -> s{_dCacheClusterId = a})

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dMarker :: Lens' DescribeCacheClusters (Maybe Text)
dMarker = lens _dMarker (\ s a -> s{_dMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved. Default: 100 Constraints: minimum 20; maximum 100.
dMaxRecords :: Lens' DescribeCacheClusters (Maybe Int)
dMaxRecords = lens _dMaxRecords (\ s a -> s{_dMaxRecords = a})

-- | An optional flag that can be included in the @DescribeCacheCluster@ request to retrieve information about the individual cache nodes.
dShowCacheNodeInfo :: Lens' DescribeCacheClusters (Maybe Bool)
dShowCacheNodeInfo = lens _dShowCacheNodeInfo (\ s a -> s{_dShowCacheNodeInfo = a})

instance AWSPager DescribeCacheClusters where
        page rq rs
          | stop (rs ^. drsMarker) = Nothing
          | stop (rs ^. drsCacheClusters) = Nothing
          | otherwise = Just $ rq & dMarker .~ rs ^. drsMarker

instance AWSRequest DescribeCacheClusters where
        type Rs DescribeCacheClusters =
             DescribeCacheClustersResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper "DescribeCacheClustersResult"
              (\ s h x ->
                 DescribeCacheClustersResponse' <$>
                   (x .@? "CacheClusters" .!@ mempty >>=
                      may (parseXMLList "CacheCluster"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeCacheClusters where

instance NFData DescribeCacheClusters where

instance ToHeaders DescribeCacheClusters where
        toHeaders = const mempty

instance ToPath DescribeCacheClusters where
        toPath = const "/"

instance ToQuery DescribeCacheClusters where
        toQuery DescribeCacheClusters'{..}
          = mconcat
              ["Action" =: ("DescribeCacheClusters" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "ShowCacheClustersNotInReplicationGroups" =:
                 _dShowCacheClustersNotInReplicationGroups,
               "CacheClusterId" =: _dCacheClusterId,
               "Marker" =: _dMarker, "MaxRecords" =: _dMaxRecords,
               "ShowCacheNodeInfo" =: _dShowCacheNodeInfo]

-- | Represents the output of a @DescribeCacheClusters@ operation.
--
--
--
-- /See:/ 'describeCacheClustersResponse' smart constructor.
data DescribeCacheClustersResponse = DescribeCacheClustersResponse'
  { _drsCacheClusters  :: !(Maybe [CacheCluster])
  , _drsMarker         :: !(Maybe Text)
  , _drsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCacheClustersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsCacheClusters' - A list of clusters. Each item in the list contains detailed information about one cluster.
--
-- * 'drsMarker' - Provides an identifier to allow retrieval of paginated results.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeCacheClustersResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeCacheClustersResponse
describeCacheClustersResponse pResponseStatus_ =
  DescribeCacheClustersResponse'
    { _drsCacheClusters = Nothing
    , _drsMarker = Nothing
    , _drsResponseStatus = pResponseStatus_
    }


-- | A list of clusters. Each item in the list contains detailed information about one cluster.
drsCacheClusters :: Lens' DescribeCacheClustersResponse [CacheCluster]
drsCacheClusters = lens _drsCacheClusters (\ s a -> s{_drsCacheClusters = a}) . _Default . _Coerce

-- | Provides an identifier to allow retrieval of paginated results.
drsMarker :: Lens' DescribeCacheClustersResponse (Maybe Text)
drsMarker = lens _drsMarker (\ s a -> s{_drsMarker = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeCacheClustersResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DescribeCacheClustersResponse where
