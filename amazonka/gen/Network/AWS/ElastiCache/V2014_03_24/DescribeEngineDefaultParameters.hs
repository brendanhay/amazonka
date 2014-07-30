{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_03_24.DescribeEngineDefaultParameters
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
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2014-03-27T01%3A34%3A31.045Z &AWSAccessKeyId=YOUR-ACCESS-KEY
-- &Signature=YOUR-SIGNATURE memcached1.4 1024 integer system false The
-- backlog queue limit. 1-10000 backlog_queue_limit 1.4.5 (...output
-- omitted...) cache.c1.xlarge 6000 (...output omitted...) integer system
-- false The maximum configurable amount of memory to use to store items, in
-- megabytes. 1-100000 max_cache_memory 1.4.5 (...output omitted...)
-- 061282fe-b7fd-11e0-9326-b7275b9d4a6c.
module Network.AWS.ElastiCache.V2014_03_24.DescribeEngineDefaultParameters where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.ElastiCache.V2014_03_24.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'DescribeEngineDefaultParameters' request.
describeEngineDefaultParameters :: Text -- ^ '_dedpmCacheParameterGroupFamily'
                                -> DescribeEngineDefaultParameters
describeEngineDefaultParameters p1 = DescribeEngineDefaultParameters
    { _dedpmCacheParameterGroupFamily = p1
    , _dedpmMaxRecords = Nothing
    , _dedpmMarker = Nothing
    }

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
    } deriving (Generic)

instance ToQuery DescribeEngineDefaultParameters where
    toQuery = genericToQuery def

instance AWSRequest DescribeEngineDefaultParameters where
    type Sv DescribeEngineDefaultParameters = ElastiCache
    type Rs DescribeEngineDefaultParameters = DescribeEngineDefaultParametersResponse

    request = post "DescribeEngineDefaultParameters"
    response _ = xmlResponse

instance AWSPager DescribeEngineDefaultParameters where
    next rq rs = (\x -> rq { _dedpmMarker = Just x })
        <$> _edwEngineDefaults.Marker rs

data DescribeEngineDefaultParametersResponse = DescribeEngineDefaultParametersResponse
    { _edwEngineDefaults :: Maybe EngineDefaults
      -- ^ Represents the output of a DescribeEngineDefaultParameters
      -- operation.
    } deriving (Generic)

instance FromXML DescribeEngineDefaultParametersResponse where
    fromXMLOptions = xmlOptions
