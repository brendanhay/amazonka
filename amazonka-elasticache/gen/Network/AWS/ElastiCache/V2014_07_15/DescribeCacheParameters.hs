{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.DescribeCacheParameters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeCacheParameters operation returns the detailed parameter list
-- for a particular cache parameter group. Some of the output has been omitted
-- for brevity. https://elasticache.us-east-1.amazonaws.com/
-- ?Action=DescribeCacheParameters
-- &CacheParameterGroupName=default.memcached1.4 &MaxRecords=100
-- &Version=2014-03-24 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20140401T192317Z &X-Amz-Credential= 1-07-15/"> cache.c1.xlarge
-- 6000 (...output omitted...) integer system false The maximum configurable
-- amount of memory to use to store items, in megabytes. 1-100000
-- max_cache_memory 1.4.5 (...output omitted...) 1024 integer system false The
-- backlog queue limit. 1-10000 backlog_queue_limit 1.4.5 (...output
-- omitted...) 0c507368-b7fe-11e0-9326-b7275b9d4a6c.
module Network.AWS.ElastiCache.V2014_07_15.DescribeCacheParameters
    (
    -- * Request
      DescribeCacheParameters
    -- ** Request constructor
    , mkDescribeCacheParameters
    -- ** Request lenses
    , dcpCacheParameterGroupName
    , dcpSource
    , dcpMaxRecords
    , dcpMarker

    -- * Response
    , DescribeCacheParametersResponse
    -- ** Response lenses
    , dcprMarker
    , dcprParameters
    , dcprCacheNodeTypeSpecificParameters
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Represents the input of a DescribeCacheParameters operation.
data DescribeCacheParameters = DescribeCacheParameters
    { _dcpCacheParameterGroupName :: Text
    , _dcpSource :: Maybe Text
    , _dcpMaxRecords :: Maybe Integer
    , _dcpMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeCacheParameters' request.
mkDescribeCacheParameters :: Text -- ^ 'dcpCacheParameterGroupName'
                          -> DescribeCacheParameters
mkDescribeCacheParameters p1 = DescribeCacheParameters
    { _dcpCacheParameterGroupName = p1
    , _dcpSource = Nothing
    , _dcpMaxRecords = Nothing
    , _dcpMarker = Nothing
    }

-- | The name of a specific cache parameter group to return details for.
dcpCacheParameterGroupName :: Lens' DescribeCacheParameters Text
dcpCacheParameterGroupName =
    lens _dcpCacheParameterGroupName
         (\s a -> s { _dcpCacheParameterGroupName = a })

-- | The parameter types to return. Valid values: user | system |
-- engine-default.
dcpSource :: Lens' DescribeCacheParameters (Maybe Text)
dcpSource = lens _dcpSource (\s a -> s { _dcpSource = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
dcpMaxRecords :: Lens' DescribeCacheParameters (Maybe Integer)
dcpMaxRecords = lens _dcpMaxRecords (\s a -> s { _dcpMaxRecords = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
dcpMarker :: Lens' DescribeCacheParameters (Maybe Text)
dcpMarker = lens _dcpMarker (\s a -> s { _dcpMarker = a })

instance ToQuery DescribeCacheParameters where
    toQuery = genericQuery def

-- | Represents the output of a DescribeCacheParameters operation.
data DescribeCacheParametersResponse = DescribeCacheParametersResponse
    { _dcprMarker :: Maybe Text
    , _dcprParameters :: [Parameter]
    , _dcprCacheNodeTypeSpecificParameters :: [CacheNodeTypeSpecificParameter]
    } deriving (Show, Generic)

-- | Provides an identifier to allow retrieval of paginated results.
dcprMarker :: Lens' DescribeCacheParametersResponse (Maybe Text)
dcprMarker = lens _dcprMarker (\s a -> s { _dcprMarker = a })

-- | A list of Parameter instances.
dcprParameters :: Lens' DescribeCacheParametersResponse [Parameter]
dcprParameters = lens _dcprParameters (\s a -> s { _dcprParameters = a })

-- | A list of parameters specific to a particular cache node type. Each element
-- in the list contains detailed information about one parameter.
dcprCacheNodeTypeSpecificParameters :: Lens' DescribeCacheParametersResponse [CacheNodeTypeSpecificParameter]
dcprCacheNodeTypeSpecificParameters =
    lens _dcprCacheNodeTypeSpecificParameters
         (\s a -> s { _dcprCacheNodeTypeSpecificParameters = a })

instance FromXML DescribeCacheParametersResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeCacheParameters where
    type Sv DescribeCacheParameters = ElastiCache
    type Rs DescribeCacheParameters = DescribeCacheParametersResponse

    request = post "DescribeCacheParameters"
    response _ = xmlResponse

instance AWSPager DescribeCacheParameters where
    next rq rs = (\x -> rq & dcpMarker ?~ x)
        <$> (rs ^. dcprMarker)
