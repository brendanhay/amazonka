{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.DescribeEngineDefaultParameters
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
module Network.AWS.ElastiCache.V2014_07_15.DescribeEngineDefaultParameters
    (
    -- * Request
      DescribeEngineDefaultParameters
    -- ** Request constructor
    , describeEngineDefaultParameters
    -- ** Request lenses
    , dedpmCacheParameterGroupFamily
    , dedpmMaxRecords
    , dedpmMarker

    -- * Response
    , DescribeEngineDefaultParametersResponse
    -- ** Response lenses
    , edwEngineDefaults
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeEngineDefaultParameters' request.
describeEngineDefaultParameters :: Text -- ^ 'dedpmCacheParameterGroupFamily'
                                -> DescribeEngineDefaultParameters
describeEngineDefaultParameters p1 = DescribeEngineDefaultParameters
    { _dedpmCacheParameterGroupFamily = p1
    , _dedpmMaxRecords = Nothing
    , _dedpmMarker = Nothing
    }
{-# INLINE describeEngineDefaultParameters #-}

data DescribeEngineDefaultParameters = DescribeEngineDefaultParameters
    { _dedpmCacheParameterGroupFamily :: Text
      -- ^ The name of the cache parameter group family. Valid values are:
      -- memcached1.4 | redis2.6 | redis2.8.
    , _dedpmMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a marker is
      -- included in the response so that the remaining results can be
      -- retrieved. Default: 100 Constraints: minimum 20; maximum 100.
    , _dedpmMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker
      -- for pagination of results from this operation. If this parameter
      -- is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

-- | The name of the cache parameter group family. Valid values are:
-- memcached1.4 | redis2.6 | redis2.8.
dedpmCacheParameterGroupFamily :: Lens' DescribeEngineDefaultParameters (Text)
dedpmCacheParameterGroupFamily f x =
    f (_dedpmCacheParameterGroupFamily x)
        <&> \y -> x { _dedpmCacheParameterGroupFamily = y }
{-# INLINE dedpmCacheParameterGroupFamily #-}

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
dedpmMaxRecords :: Lens' DescribeEngineDefaultParameters (Maybe Integer)
dedpmMaxRecords f x =
    f (_dedpmMaxRecords x)
        <&> \y -> x { _dedpmMaxRecords = y }
{-# INLINE dedpmMaxRecords #-}

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
dedpmMarker :: Lens' DescribeEngineDefaultParameters (Maybe Text)
dedpmMarker f x =
    f (_dedpmMarker x)
        <&> \y -> x { _dedpmMarker = y }
{-# INLINE dedpmMarker #-}

instance ToQuery DescribeEngineDefaultParameters where
    toQuery = genericQuery def

data DescribeEngineDefaultParametersResponse = DescribeEngineDefaultParametersResponse
    { _edwEngineDefaults :: EngineDefaults
      -- ^ Represents the output of a DescribeEngineDefaultParameters
      -- operation.
    } deriving (Show, Generic)

-- | Represents the output of a DescribeEngineDefaultParameters operation.
edwEngineDefaults :: Lens' DescribeEngineDefaultParametersResponse (EngineDefaults)
edwEngineDefaults f x =
    f (_edwEngineDefaults x)
        <&> \y -> x { _edwEngineDefaults = y }
{-# INLINE edwEngineDefaults #-}

instance FromXML DescribeEngineDefaultParametersResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeEngineDefaultParameters where
    type Sv DescribeEngineDefaultParameters = ElastiCache
    type Rs DescribeEngineDefaultParameters = DescribeEngineDefaultParametersResponse

    request = post "DescribeEngineDefaultParameters"
    response _ = xmlResponse

instance AWSPager DescribeEngineDefaultParameters where
    next rq rs = (\x -> rq { _dedpmMarker = Just x })
        <$> (_edMarker $ _edwEngineDefaults rs)
