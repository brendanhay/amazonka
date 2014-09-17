{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.DescribeEngineDefaultParameters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeEngineDefaultParameters operation returns the default engine
-- and system parameter information for the specified cache engine. Some of
-- the output has been omitted for brevity.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=DescribeEngineDefaultParameters
-- &CacheParameterGroupFamily=memcached1.4 &MaxRecords=100 &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= memcached1.4 1024 integer system false The backlog queue
-- limit. 1-10000 backlog_queue_limit 1.4.5 (...output omitted...)
-- cache.c1.xlarge 6000 (...output omitted...) integer system false The
-- maximum configurable amount of memory to use to store items, in megabytes.
-- 1-100000 max_cache_memory 1.4.5 (...output omitted...)
-- 061282fe-b7fd-11e0-9326-b7275b9d4a6c.
module Network.AWS.ElastiCache.DescribeEngineDefaultParameters
    (
    -- * Request
      DescribeEngineDefaultParameters
    -- ** Request constructor
    , mkDescribeEngineDefaultParameters
    -- ** Request lenses
    , dedpCacheParameterGroupFamily
    , dedpMaxRecords
    , dedpMarker

    -- * Response
    , DescribeEngineDefaultParametersResponse
    -- ** Response constructor
    , mkDescribeEngineDefaultParametersResponse
    -- ** Response lenses
    , dedprEngineDefaults
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import Network.AWS.Prelude

-- | Represents the input of a DescribeEngineDefaultParameters operation.
data DescribeEngineDefaultParameters = DescribeEngineDefaultParameters
    { _dedpCacheParameterGroupFamily :: Text
    , _dedpMaxRecords :: Maybe Integer
    , _dedpMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeEngineDefaultParameters' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheParameterGroupFamily ::@ @Text@
--
-- * @MaxRecords ::@ @Maybe Integer@
--
-- * @Marker ::@ @Maybe Text@
--
mkDescribeEngineDefaultParameters :: Text -- ^ 'dedpCacheParameterGroupFamily'
                                  -> DescribeEngineDefaultParameters
mkDescribeEngineDefaultParameters p1 = DescribeEngineDefaultParameters
    { _dedpCacheParameterGroupFamily = p1
    , _dedpMaxRecords = Nothing
    , _dedpMarker = Nothing
    }

-- | The name of the cache parameter group family. Valid values are:
-- memcached1.4 | redis2.6 | redis2.8.
dedpCacheParameterGroupFamily :: Lens' DescribeEngineDefaultParameters Text
dedpCacheParameterGroupFamily =
    lens _dedpCacheParameterGroupFamily
         (\s a -> s { _dedpCacheParameterGroupFamily = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
dedpMaxRecords :: Lens' DescribeEngineDefaultParameters (Maybe Integer)
dedpMaxRecords = lens _dedpMaxRecords (\s a -> s { _dedpMaxRecords = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
dedpMarker :: Lens' DescribeEngineDefaultParameters (Maybe Text)
dedpMarker = lens _dedpMarker (\s a -> s { _dedpMarker = a })

instance ToQuery DescribeEngineDefaultParameters where
    toQuery = genericQuery def

newtype DescribeEngineDefaultParametersResponse = DescribeEngineDefaultParametersResponse
    { _dedprEngineDefaults :: EngineDefaults
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeEngineDefaultParametersResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @EngineDefaults ::@ @EngineDefaults@
--
mkDescribeEngineDefaultParametersResponse :: EngineDefaults -- ^ 'dedprEngineDefaults'
                                          -> DescribeEngineDefaultParametersResponse
mkDescribeEngineDefaultParametersResponse p1 = DescribeEngineDefaultParametersResponse
    { _dedprEngineDefaults = p1
    }

-- | Represents the output of a DescribeEngineDefaultParameters operation.
dedprEngineDefaults :: Lens' DescribeEngineDefaultParametersResponse EngineDefaults
dedprEngineDefaults =
    lens _dedprEngineDefaults (\s a -> s { _dedprEngineDefaults = a })

instance FromXML DescribeEngineDefaultParametersResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeEngineDefaultParameters where
    type Sv DescribeEngineDefaultParameters = ElastiCache
    type Rs DescribeEngineDefaultParameters = DescribeEngineDefaultParametersResponse

    request = post "DescribeEngineDefaultParameters"
    response _ = xmlResponse

instance AWSPager DescribeEngineDefaultParameters where
    next rq rs = (\x -> rq & dedpMarker ?~ x)
        <$> (rs ^. dedprEngineDefaults . edMarker)
