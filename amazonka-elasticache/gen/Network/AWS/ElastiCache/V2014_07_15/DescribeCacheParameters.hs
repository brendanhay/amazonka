{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.ElastiCache.V2014_07_15.DescribeCacheParameters where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeCacheParameters' request.
describeCacheParameters :: Text -- ^ '_dcpmCacheParameterGroupName'
                        -> DescribeCacheParameters
describeCacheParameters p1 = DescribeCacheParameters
    { _dcpmCacheParameterGroupName = p1
    , _dcpmMaxRecords = Nothing
    , _dcpmSource = Nothing
    , _dcpmMarker = Nothing
    }

data DescribeCacheParameters = DescribeCacheParameters
    { _dcpmCacheParameterGroupName :: Text
      -- ^ The name of a specific cache parameter group to return details
      -- for.
    , _dcpmMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a marker is
      -- included in the response so that the remaining results can be
      -- retrieved. Default: 100 Constraints: minimum 20; maximum 100.
    , _dcpmSource :: Maybe Text
      -- ^ The parameter types to return. Valid values: user | system |
      -- engine-default.
    , _dcpmMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker
      -- for pagination of results from this operation. If this parameter
      -- is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

makeLenses ''DescribeCacheParameters

instance ToQuery DescribeCacheParameters where
    toQuery = genericQuery def

data DescribeCacheParametersResponse = DescribeCacheParametersResponse
    { _cpgdCacheNodeTypeSpecificParameters :: [CacheNodeTypeSpecificParameter]
      -- ^ A list of parameters specific to a particular cache node type.
      -- Each element in the list contains detailed information about one
      -- parameter.
    , _cpgdParameters :: [Parameter]
      -- ^ A list of Parameter instances.
    , _cpgdMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    } deriving (Show, Generic)

makeLenses ''DescribeCacheParametersResponse

instance FromXML DescribeCacheParametersResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeCacheParameters where
    type Sv DescribeCacheParameters = ElastiCache
    type Rs DescribeCacheParameters = DescribeCacheParametersResponse

    request = post "DescribeCacheParameters"
    response _ = xmlResponse

instance AWSPager DescribeCacheParameters where
    next rq rs = (\x -> rq { _dcpmMarker = Just x })
        <$> (_cpgdMarker rs)
