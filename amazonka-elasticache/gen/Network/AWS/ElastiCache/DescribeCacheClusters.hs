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

-- Module      : Network.AWS.ElastiCache.DescribeCacheClusters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeCacheClusters operation returns information about all
-- provisioned cache clusters if no cache cluster identifier is specified, or
-- about a specific cache cluster if a cache cluster identifier is supplied.
-- By default, abbreviated information about the cache clusters(s) will be
-- returned. You can use the optional ShowDetails flag to retrieve detailed
-- information about the cache nodes associated with the cache clusters. These
-- details include the DNS address and port for the cache node endpoint. If
-- the cluster is in the CREATING state, only cluster level information will
-- be displayed until all of the nodes are successfully provisioned. If the
-- cluster is in the DELETING state, only cluster level information will be
-- displayed. If cache nodes are currently being added to the cache cluster,
-- node endpoint information and creation time for the additional nodes will
-- not be displayed until they are completely provisioned. When the cache
-- cluster state is available, the cluster is ready for use. If cache nodes
-- are currently being removed from the cache cluster, no endpoint information
-- for the removed nodes is displayed.
module Network.AWS.ElastiCache.DescribeCacheClusters
    (
    -- * Request
      DescribeCacheClustersMessage
    -- ** Request constructor
    , describeCacheClusters
    -- ** Request lenses
    , dccm1CacheClusterId
    , dccm1Marker
    , dccm1MaxRecords
    , dccm1ShowCacheNodeInfo

    -- * Response
    , CacheClusterMessage
    -- ** Response constructor
    , describeCacheClustersResponse
    -- ** Response lenses
    , ccmCacheClusters
    , ccmMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data DescribeCacheClustersMessage = DescribeCacheClustersMessage
    { _dccm1CacheClusterId    :: Maybe Text
    , _dccm1Marker            :: Maybe Text
    , _dccm1MaxRecords        :: Maybe Int
    , _dccm1ShowCacheNodeInfo :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeCacheClustersMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dccm1CacheClusterId' @::@ 'Maybe' 'Text'
--
-- * 'dccm1Marker' @::@ 'Maybe' 'Text'
--
-- * 'dccm1MaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dccm1ShowCacheNodeInfo' @::@ 'Maybe' 'Bool'
--
describeCacheClusters :: DescribeCacheClustersMessage
describeCacheClusters = DescribeCacheClustersMessage
    { _dccm1CacheClusterId    = Nothing
    , _dccm1MaxRecords        = Nothing
    , _dccm1Marker            = Nothing
    , _dccm1ShowCacheNodeInfo = Nothing
    }

-- | The user-supplied cluster identifier. If this parameter is specified,
-- only information about that specific cache cluster is returned. This
-- parameter isn't case sensitive.
dccm1CacheClusterId :: Lens' DescribeCacheClustersMessage (Maybe Text)
dccm1CacheClusterId =
    lens _dccm1CacheClusterId (\s a -> s { _dccm1CacheClusterId = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
dccm1Marker :: Lens' DescribeCacheClustersMessage (Maybe Text)
dccm1Marker = lens _dccm1Marker (\s a -> s { _dccm1Marker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
dccm1MaxRecords :: Lens' DescribeCacheClustersMessage (Maybe Int)
dccm1MaxRecords = lens _dccm1MaxRecords (\s a -> s { _dccm1MaxRecords = a })

-- | An optional flag that can be included in the DescribeCacheCluster request
-- to retrieve information about the individual cache nodes.
dccm1ShowCacheNodeInfo :: Lens' DescribeCacheClustersMessage (Maybe Bool)
dccm1ShowCacheNodeInfo =
    lens _dccm1ShowCacheNodeInfo (\s a -> s { _dccm1ShowCacheNodeInfo = a })

instance ToPath DescribeCacheClustersMessage where
    toPath = const "/"

instance ToQuery DescribeCacheClustersMessage

data CacheClusterMessage = CacheClusterMessage
    { _ccmCacheClusters :: [CacheCluster]
    , _ccmMarker        :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'CacheClusterMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccmCacheClusters' @::@ ['CacheCluster']
--
-- * 'ccmMarker' @::@ 'Maybe' 'Text'
--
describeCacheClustersResponse :: CacheClusterMessage
describeCacheClustersResponse = CacheClusterMessage
    { _ccmMarker        = Nothing
    , _ccmCacheClusters = mempty
    }

-- | A list of cache clusters. Each item in the list contains detailed
-- information about one cache cluster.
ccmCacheClusters :: Lens' CacheClusterMessage [CacheCluster]
ccmCacheClusters = lens _ccmCacheClusters (\s a -> s { _ccmCacheClusters = a })

-- | Provides an identifier to allow retrieval of paginated results.
ccmMarker :: Lens' CacheClusterMessage (Maybe Text)
ccmMarker = lens _ccmMarker (\s a -> s { _ccmMarker = a })

instance AWSRequest DescribeCacheClustersMessage where
    type Sv DescribeCacheClustersMessage = ElastiCache
    type Rs DescribeCacheClustersMessage = CacheClusterMessage

    request  = post "DescribeCacheClusters"
    response = xmlResponse $ \h x -> CacheClusterMessage
        <$> x %| "CacheClusters"
        <*> x %| "Marker"
