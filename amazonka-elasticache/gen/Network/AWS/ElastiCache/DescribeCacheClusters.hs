{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeCacheClusters.html>
module Network.AWS.ElastiCache.DescribeCacheClusters
    (
    -- * Request
      DescribeCacheClusters
    -- ** Request constructor
    , describeCacheClusters
    -- ** Request lenses
    , dcc1CacheClusterId
    , dcc1Marker
    , dcc1MaxRecords
    , dcc1ShowCacheNodeInfo

    -- * Response
    , DescribeCacheClustersResponse
    -- ** Response constructor
    , describeCacheClustersResponse
    -- ** Response lenses
    , dccrCacheClusters
    , dccrMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data DescribeCacheClusters = DescribeCacheClusters
    { _dcc1CacheClusterId    :: Maybe Text
    , _dcc1Marker            :: Maybe Text
    , _dcc1MaxRecords        :: Maybe Int
    , _dcc1ShowCacheNodeInfo :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeCacheClusters' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcc1CacheClusterId' @::@ 'Maybe' 'Text'
--
-- * 'dcc1Marker' @::@ 'Maybe' 'Text'
--
-- * 'dcc1MaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dcc1ShowCacheNodeInfo' @::@ 'Maybe' 'Bool'
--
describeCacheClusters :: DescribeCacheClusters
describeCacheClusters = DescribeCacheClusters
    { _dcc1CacheClusterId    = Nothing
    , _dcc1MaxRecords        = Nothing
    , _dcc1Marker            = Nothing
    , _dcc1ShowCacheNodeInfo = Nothing
    }

-- | The user-supplied cluster identifier. If this parameter is specified,
-- only information about that specific cache cluster is returned. This
-- parameter isn't case sensitive.
dcc1CacheClusterId :: Lens' DescribeCacheClusters (Maybe Text)
dcc1CacheClusterId =
    lens _dcc1CacheClusterId (\s a -> s { _dcc1CacheClusterId = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
dcc1Marker :: Lens' DescribeCacheClusters (Maybe Text)
dcc1Marker = lens _dcc1Marker (\s a -> s { _dcc1Marker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
dcc1MaxRecords :: Lens' DescribeCacheClusters (Maybe Int)
dcc1MaxRecords = lens _dcc1MaxRecords (\s a -> s { _dcc1MaxRecords = a })

-- | An optional flag that can be included in the DescribeCacheCluster request
-- to retrieve information about the individual cache nodes.
dcc1ShowCacheNodeInfo :: Lens' DescribeCacheClusters (Maybe Bool)
dcc1ShowCacheNodeInfo =
    lens _dcc1ShowCacheNodeInfo (\s a -> s { _dcc1ShowCacheNodeInfo = a })

data DescribeCacheClustersResponse = DescribeCacheClustersResponse
    { _dccrCacheClusters :: [CacheCluster]
    , _dccrMarker        :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeCacheClustersResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dccrCacheClusters' @::@ ['CacheCluster']
--
-- * 'dccrMarker' @::@ 'Maybe' 'Text'
--
describeCacheClustersResponse :: DescribeCacheClustersResponse
describeCacheClustersResponse = DescribeCacheClustersResponse
    { _dccrMarker        = Nothing
    , _dccrCacheClusters = mempty
    }

-- | A list of cache clusters. Each item in the list contains detailed
-- information about one cache cluster.
dccrCacheClusters :: Lens' DescribeCacheClustersResponse [CacheCluster]
dccrCacheClusters =
    lens _dccrCacheClusters (\s a -> s { _dccrCacheClusters = a })

-- | Provides an identifier to allow retrieval of paginated results.
dccrMarker :: Lens' DescribeCacheClustersResponse (Maybe Text)
dccrMarker = lens _dccrMarker (\s a -> s { _dccrMarker = a })

instance ToPath DescribeCacheClusters where
    toPath = const "/"

instance ToQuery DescribeCacheClusters

instance ToHeaders DescribeCacheClusters

instance AWSRequest DescribeCacheClusters where
    type Sv DescribeCacheClusters = ElastiCache
    type Rs DescribeCacheClusters = DescribeCacheClustersResponse

    request  = post "DescribeCacheClusters"
    response = xmlResponse

instance FromXML DescribeCacheClustersResponse where
    parseXML c = DescribeCacheClustersResponse
        <$> c .: "CacheClusters"
        <*> c .:? "Marker"
