{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElastiCache.DescribeCacheClusters
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

-- | The /DescribeCacheClusters/ action returns information about all
-- provisioned cache clusters if no cache cluster identifier is specified,
-- or about a specific cache cluster if a cache cluster identifier is
-- supplied.
--
-- By default, abbreviated information about the cache clusters(s) will be
-- returned. You can use the optional /ShowDetails/ flag to retrieve
-- detailed information about the cache nodes associated with the cache
-- clusters. These details include the DNS address and port for the cache
-- node endpoint.
--
-- If the cluster is in the CREATING state, only cluster level information
-- will be displayed until all of the nodes are successfully provisioned.
--
-- If the cluster is in the DELETING state, only cluster level information
-- will be displayed.
--
-- If cache nodes are currently being added to the cache cluster, node
-- endpoint information and creation time for the additional nodes will not
-- be displayed until they are completely provisioned. When the cache
-- cluster state is /available/, the cluster is ready for use.
--
-- If cache nodes are currently being removed from the cache cluster, no
-- endpoint information for the removed nodes is displayed.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeCacheClusters.html>
module Network.AWS.ElastiCache.DescribeCacheClusters
    (
    -- * Request
      DescribeCacheClusters
    -- ** Request constructor
    , describeCacheClusters
    -- ** Request lenses
    , desCacheClusterId
    , desMaxRecords
    , desMarker
    , desShowCacheNodeInfo

    -- * Response
    , DescribeCacheClustersResponse
    -- ** Response constructor
    , describeCacheClustersResponse
    -- ** Response lenses
    , dCacheClusters
    , dMarker
    , dStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /DescribeCacheClusters/ action.
--
-- /See:/ 'describeCacheClusters' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desCacheClusterId'
--
-- * 'desMaxRecords'
--
-- * 'desMarker'
--
-- * 'desShowCacheNodeInfo'
data DescribeCacheClusters = DescribeCacheClusters'
    { _desCacheClusterId    :: !(Maybe Text)
    , _desMaxRecords        :: !(Maybe Int)
    , _desMarker            :: !(Maybe Text)
    , _desShowCacheNodeInfo :: !(Maybe Bool)
    } deriving (Eq,Read,Show)

-- | 'DescribeCacheClusters' smart constructor.
describeCacheClusters :: DescribeCacheClusters
describeCacheClusters =
    DescribeCacheClusters'
    { _desCacheClusterId = Nothing
    , _desMaxRecords = Nothing
    , _desMarker = Nothing
    , _desShowCacheNodeInfo = Nothing
    }

-- | The user-supplied cluster identifier. If this parameter is specified,
-- only information about that specific cache cluster is returned. This
-- parameter isn\'t case sensitive.
desCacheClusterId :: Lens' DescribeCacheClusters (Maybe Text)
desCacheClusterId = lens _desCacheClusterId (\ s a -> s{_desCacheClusterId = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
desMaxRecords :: Lens' DescribeCacheClusters (Maybe Int)
desMaxRecords = lens _desMaxRecords (\ s a -> s{_desMaxRecords = a});

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by /MaxRecords/.
desMarker :: Lens' DescribeCacheClusters (Maybe Text)
desMarker = lens _desMarker (\ s a -> s{_desMarker = a});

-- | An optional flag that can be included in the DescribeCacheCluster
-- request to retrieve information about the individual cache nodes.
desShowCacheNodeInfo :: Lens' DescribeCacheClusters (Maybe Bool)
desShowCacheNodeInfo = lens _desShowCacheNodeInfo (\ s a -> s{_desShowCacheNodeInfo = a});

instance AWSPager DescribeCacheClusters where
        page rq rs
          | stop (rs ^. dMarker) = Nothing
          | stop (rs ^. dCacheClusters) = Nothing
          | otherwise = Just $ rq & desMarker .~ rs ^. dMarker

instance AWSRequest DescribeCacheClusters where
        type Sv DescribeCacheClusters = ElastiCache
        type Rs DescribeCacheClusters =
             DescribeCacheClustersResponse
        request = post
        response
          = receiveXMLWrapper "DescribeCacheClustersResult"
              (\ s h x ->
                 DescribeCacheClustersResponse' <$>
                   (x .@? "CacheClusters" .!@ mempty >>=
                      may (parseXMLList "CacheCluster"))
                     <*> (x .@? "Marker")
                     <*> (pure s))

instance ToHeaders DescribeCacheClusters where
        toHeaders = const mempty

instance ToPath DescribeCacheClusters where
        toPath = const "/"

instance ToQuery DescribeCacheClusters where
        toQuery DescribeCacheClusters'{..}
          = mconcat
              ["Action" =: ("DescribeCacheClusters" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheClusterId" =: _desCacheClusterId,
               "MaxRecords" =: _desMaxRecords,
               "Marker" =: _desMarker,
               "ShowCacheNodeInfo" =: _desShowCacheNodeInfo]

-- | Represents the output of a /DescribeCacheClusters/ action.
--
-- /See:/ 'describeCacheClustersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dCacheClusters'
--
-- * 'dMarker'
--
-- * 'dStatus'
data DescribeCacheClustersResponse = DescribeCacheClustersResponse'
    { _dCacheClusters :: !(Maybe [CacheCluster])
    , _dMarker        :: !(Maybe Text)
    , _dStatus        :: !Status
    } deriving (Eq,Show)

-- | 'DescribeCacheClustersResponse' smart constructor.
describeCacheClustersResponse :: Status -> DescribeCacheClustersResponse
describeCacheClustersResponse pStatus =
    DescribeCacheClustersResponse'
    { _dCacheClusters = Nothing
    , _dMarker = Nothing
    , _dStatus = pStatus
    }

-- | A list of cache clusters. Each item in the list contains detailed
-- information about one cache cluster.
dCacheClusters :: Lens' DescribeCacheClustersResponse [CacheCluster]
dCacheClusters = lens _dCacheClusters (\ s a -> s{_dCacheClusters = a}) . _Default;

-- | Provides an identifier to allow retrieval of paginated results.
dMarker :: Lens' DescribeCacheClustersResponse (Maybe Text)
dMarker = lens _dMarker (\ s a -> s{_dMarker = a});

-- | FIXME: Undocumented member.
dStatus :: Lens' DescribeCacheClustersResponse Status
dStatus = lens _dStatus (\ s a -> s{_dStatus = a});
