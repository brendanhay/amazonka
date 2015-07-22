{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeCacheClusters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /DescribeCacheClusters/ action returns information about all
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
    , drqCacheClusterId
    , drqMaxRecords
    , drqMarker
    , drqShowCacheNodeInfo

    -- * Response
    , DescribeCacheClustersResponse
    -- ** Response constructor
    , describeCacheClustersResponse
    -- ** Response lenses
    , drsCacheClusters
    , drsMarker
    , drsStatus
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
-- * 'drqCacheClusterId'
--
-- * 'drqMaxRecords'
--
-- * 'drqMarker'
--
-- * 'drqShowCacheNodeInfo'
data DescribeCacheClusters = DescribeCacheClusters'
    { _drqCacheClusterId    :: !(Maybe Text)
    , _drqMaxRecords        :: !(Maybe Int)
    , _drqMarker            :: !(Maybe Text)
    , _drqShowCacheNodeInfo :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeCacheClusters' smart constructor.
describeCacheClusters :: DescribeCacheClusters
describeCacheClusters =
    DescribeCacheClusters'
    { _drqCacheClusterId = Nothing
    , _drqMaxRecords = Nothing
    , _drqMarker = Nothing
    , _drqShowCacheNodeInfo = Nothing
    }

-- | The user-supplied cluster identifier. If this parameter is specified,
-- only information about that specific cache cluster is returned. This
-- parameter isn\'t case sensitive.
drqCacheClusterId :: Lens' DescribeCacheClusters (Maybe Text)
drqCacheClusterId = lens _drqCacheClusterId (\ s a -> s{_drqCacheClusterId = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
drqMaxRecords :: Lens' DescribeCacheClusters (Maybe Int)
drqMaxRecords = lens _drqMaxRecords (\ s a -> s{_drqMaxRecords = a});

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by /MaxRecords/.
drqMarker :: Lens' DescribeCacheClusters (Maybe Text)
drqMarker = lens _drqMarker (\ s a -> s{_drqMarker = a});

-- | An optional flag that can be included in the DescribeCacheCluster
-- request to retrieve information about the individual cache nodes.
drqShowCacheNodeInfo :: Lens' DescribeCacheClusters (Maybe Bool)
drqShowCacheNodeInfo = lens _drqShowCacheNodeInfo (\ s a -> s{_drqShowCacheNodeInfo = a});

instance AWSPager DescribeCacheClusters where
        page rq rs
          | stop (rs ^. drsMarker) = Nothing
          | stop (rs ^. drsCacheClusters) = Nothing
          | otherwise =
            Just $ rq & drqMarker .~ rs ^. drsMarker

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
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeCacheClusters where
        toHeaders = const mempty

instance ToPath DescribeCacheClusters where
        toPath = const "/"

instance ToQuery DescribeCacheClusters where
        toQuery DescribeCacheClusters'{..}
          = mconcat
              ["Action" =: ("DescribeCacheClusters" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheClusterId" =: _drqCacheClusterId,
               "MaxRecords" =: _drqMaxRecords,
               "Marker" =: _drqMarker,
               "ShowCacheNodeInfo" =: _drqShowCacheNodeInfo]

-- | Represents the output of a /DescribeCacheClusters/ action.
--
-- /See:/ 'describeCacheClustersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drsCacheClusters'
--
-- * 'drsMarker'
--
-- * 'drsStatus'
data DescribeCacheClustersResponse = DescribeCacheClustersResponse'
    { _drsCacheClusters :: !(Maybe [CacheCluster])
    , _drsMarker        :: !(Maybe Text)
    , _drsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeCacheClustersResponse' smart constructor.
describeCacheClustersResponse :: Int -> DescribeCacheClustersResponse
describeCacheClustersResponse pStatus_ =
    DescribeCacheClustersResponse'
    { _drsCacheClusters = Nothing
    , _drsMarker = Nothing
    , _drsStatus = pStatus_
    }

-- | A list of cache clusters. Each item in the list contains detailed
-- information about one cache cluster.
drsCacheClusters :: Lens' DescribeCacheClustersResponse [CacheCluster]
drsCacheClusters = lens _drsCacheClusters (\ s a -> s{_drsCacheClusters = a}) . _Default;

-- | Provides an identifier to allow retrieval of paginated results.
drsMarker :: Lens' DescribeCacheClustersResponse (Maybe Text)
drsMarker = lens _drsMarker (\ s a -> s{_drsMarker = a});

-- | FIXME: Undocumented member.
drsStatus :: Lens' DescribeCacheClustersResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
