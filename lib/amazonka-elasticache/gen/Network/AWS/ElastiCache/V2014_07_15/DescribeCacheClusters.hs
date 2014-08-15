{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.DescribeCacheClusters
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
module Network.AWS.ElastiCache.V2014_07_15.DescribeCacheClusters where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeCacheClusters' request.
describeCacheClusters :: DescribeCacheClusters
describeCacheClusters = DescribeCacheClusters
    { _dccnShowCacheNodeInfo = Nothing
    , _dccnMaxRecords = Nothing
    , _dccnCacheClusterId = Nothing
    , _dccnMarker = Nothing
    }

data DescribeCacheClusters = DescribeCacheClusters
    { _dccnShowCacheNodeInfo :: Maybe Bool
      -- ^ An optional flag that can be included in the DescribeCacheCluster
      -- request to retrieve information about the individual cache nodes.
    , _dccnMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a marker is
      -- included in the response so that the remaining results can be
      -- retrieved. Default: 100 Constraints: minimum 20; maximum 100.
    , _dccnCacheClusterId :: Maybe Text
      -- ^ The user-supplied cluster identifier. If this parameter is
      -- specified, only information about that specific cache cluster is
      -- returned. This parameter isn't case sensitive.
    , _dccnMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker
      -- for pagination of results from this operation. If this parameter
      -- is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

makeLenses ''DescribeCacheClusters

instance ToQuery DescribeCacheClusters where
    toQuery = genericQuery def

data DescribeCacheClustersResponse = DescribeCacheClustersResponse
    { _ccmCacheClusters :: [CacheCluster]
      -- ^ A list of cache clusters. Each item in the list contains detailed
      -- information about one cache cluster.
    , _ccmMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    } deriving (Show, Generic)

makeLenses ''DescribeCacheClustersResponse

instance FromXML DescribeCacheClustersResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeCacheClusters where
    type Sv DescribeCacheClusters = ElastiCache
    type Rs DescribeCacheClusters = DescribeCacheClustersResponse

    request = post "DescribeCacheClusters"
    response _ = xmlResponse

instance AWSPager DescribeCacheClusters where
    next rq rs = (\x -> rq { _dccnMarker = Just x })
        <$> (_ccmMarker rs)
