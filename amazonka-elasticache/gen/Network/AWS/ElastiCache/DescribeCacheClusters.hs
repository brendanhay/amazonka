{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache
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
-- https://elasticache.us-east-1.amazonaws.com/ ?Action=DescribeCacheClusters
-- &MaxRecords=100 &ShowCacheNodeInfo=false &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= in-sync default.memcached1.4 simcoprod42 available 11211
-- simcoprod42.m2st2p.cfg.cache.amazonaws.com
-- https://console.aws.amazon.com/elasticache/home#client-download:
-- cache.m1.large memcached us-east-1d 2014-03-26T01:21:46.607Z 1.4.5 true
-- fri:08:30-fri:09:00 default active active
-- arn:aws:sns:us-east-1:123456789012:ElastiCacheNotifications 6
-- f270d58f-b7fb-11e0-9326-b7275b9d4a6c.
module Network.AWS.ElastiCache
    (
    -- * Request
      DescribeCacheClusters
    -- ** Request constructor
    , mkDescribeCacheClusters
    -- ** Request lenses
    , dcc1CacheClusterId
    , dcc1MaxRecords
    , dcc1Marker
    , dcc1ShowCacheNodeInfo

    -- * Response
    , DescribeCacheClustersResponse
    -- ** Response constructor
    , mkDescribeCacheClustersResponse
    -- ** Response lenses
    , dccrrMarker
    , dccrrCacheClusters
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import Network.AWS.Prelude

-- | Represents the input of a DescribeCacheClusters operation.
data DescribeCacheClusters = DescribeCacheClusters
    { _dcc1CacheClusterId :: Maybe Text
    , _dcc1MaxRecords :: Maybe Integer
    , _dcc1Marker :: Maybe Text
    , _dcc1ShowCacheNodeInfo :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeCacheClusters' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheClusterId ::@ @Maybe Text@
--
-- * @MaxRecords ::@ @Maybe Integer@
--
-- * @Marker ::@ @Maybe Text@
--
-- * @ShowCacheNodeInfo ::@ @Maybe Bool@
--
mkDescribeCacheClusters :: DescribeCacheClusters
mkDescribeCacheClusters = DescribeCacheClusters
    { _dcc1CacheClusterId = Nothing
    , _dcc1MaxRecords = Nothing
    , _dcc1Marker = Nothing
    , _dcc1ShowCacheNodeInfo = Nothing
    }

-- | The user-supplied cluster identifier. If this parameter is specified, only
-- information about that specific cache cluster is returned. This parameter
-- isn't case sensitive.
dcc1CacheClusterId :: Lens' DescribeCacheClusters (Maybe Text)
dcc1CacheClusterId =
    lens _dcc1CacheClusterId (\s a -> s { _dcc1CacheClusterId = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
dcc1MaxRecords :: Lens' DescribeCacheClusters (Maybe Integer)
dcc1MaxRecords = lens _dcc1MaxRecords (\s a -> s { _dcc1MaxRecords = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
dcc1Marker :: Lens' DescribeCacheClusters (Maybe Text)
dcc1Marker = lens _dcc1Marker (\s a -> s { _dcc1Marker = a })

-- | An optional flag that can be included in the DescribeCacheCluster request
-- to retrieve information about the individual cache nodes.
dcc1ShowCacheNodeInfo :: Lens' DescribeCacheClusters (Maybe Bool)
dcc1ShowCacheNodeInfo =
    lens _dcc1ShowCacheNodeInfo (\s a -> s { _dcc1ShowCacheNodeInfo = a })

instance ToQuery DescribeCacheClusters where
    toQuery = genericQuery def

-- | Represents the output of a DescribeCacheClusters operation.
data DescribeCacheClustersResponse = DescribeCacheClustersResponse
    { _dccrrMarker :: Maybe Text
    , _dccrrCacheClusters :: [CacheCluster]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeCacheClustersResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Marker ::@ @Maybe Text@
--
-- * @CacheClusters ::@ @[CacheCluster]@
--
mkDescribeCacheClustersResponse :: DescribeCacheClustersResponse
mkDescribeCacheClustersResponse = DescribeCacheClustersResponse
    { _dccrrMarker = Nothing
    , _dccrrCacheClusters = mempty
    }

-- | Provides an identifier to allow retrieval of paginated results.
dccrrMarker :: Lens' DescribeCacheClustersResponse (Maybe Text)
dccrrMarker = lens _dccrrMarker (\s a -> s { _dccrrMarker = a })

-- | A list of cache clusters. Each item in the list contains detailed
-- information about one cache cluster.
dccrrCacheClusters :: Lens' DescribeCacheClustersResponse [CacheCluster]
dccrrCacheClusters =
    lens _dccrrCacheClusters (\s a -> s { _dccrrCacheClusters = a })

instance FromXML DescribeCacheClustersResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeCacheClusters where
    type Sv DescribeCacheClusters = ElastiCache
    type Rs DescribeCacheClusters = DescribeCacheClustersResponse

    request = post "DescribeCacheClusters"
    response _ = xmlResponse

instance AWSPager DescribeCacheClusters where
    next rq rs = (\x -> rq & dcc1Marker ?~ x)
        <$> (rs ^. dccrrMarker)
