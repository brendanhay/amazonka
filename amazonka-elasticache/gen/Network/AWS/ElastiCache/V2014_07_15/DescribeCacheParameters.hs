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
    , mkDescribeCacheParametersMessage
    -- ** Request lenses
    , dcpmCacheParameterGroupName
    , dcpmSource
    , dcpmMaxRecords
    , dcpmMarker

    -- * Response
    , DescribeCacheParametersResponse
    -- ** Response lenses
    , cpgdMarker
    , cpgdParameters
    , cpgdCacheNodeTypeSpecificParameters
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeCacheParameters' request.
mkDescribeCacheParametersMessage :: Text -- ^ 'dcpmCacheParameterGroupName'
                                 -> DescribeCacheParameters
mkDescribeCacheParametersMessage p1 = DescribeCacheParameters
    { _dcpmCacheParameterGroupName = p1
    , _dcpmSource = Nothing
    , _dcpmMaxRecords = Nothing
    , _dcpmMarker = Nothing
    }
{-# INLINE mkDescribeCacheParametersMessage #-}

data DescribeCacheParameters = DescribeCacheParameters
    { _dcpmCacheParameterGroupName :: Text
      -- ^ The name of a specific cache parameter group to return details
      -- for.
    , _dcpmSource :: Maybe Text
      -- ^ The parameter types to return. Valid values: user | system |
      -- engine-default.
    , _dcpmMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a marker is
      -- included in the response so that the remaining results can be
      -- retrieved. Default: 100 Constraints: minimum 20; maximum 100.
    , _dcpmMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker
      -- for pagination of results from this operation. If this parameter
      -- is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

-- | The name of a specific cache parameter group to return details for.
dcpmCacheParameterGroupName :: Lens' DescribeCacheParameters (Text)
dcpmCacheParameterGroupName = lens _dcpmCacheParameterGroupName (\s a -> s { _dcpmCacheParameterGroupName = a })
{-# INLINE dcpmCacheParameterGroupName #-}

-- | The parameter types to return. Valid values: user | system |
-- engine-default.
dcpmSource :: Lens' DescribeCacheParameters (Maybe Text)
dcpmSource = lens _dcpmSource (\s a -> s { _dcpmSource = a })
{-# INLINE dcpmSource #-}

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
dcpmMaxRecords :: Lens' DescribeCacheParameters (Maybe Integer)
dcpmMaxRecords = lens _dcpmMaxRecords (\s a -> s { _dcpmMaxRecords = a })
{-# INLINE dcpmMaxRecords #-}

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
dcpmMarker :: Lens' DescribeCacheParameters (Maybe Text)
dcpmMarker = lens _dcpmMarker (\s a -> s { _dcpmMarker = a })
{-# INLINE dcpmMarker #-}

instance ToQuery DescribeCacheParameters where
    toQuery = genericQuery def

data DescribeCacheParametersResponse = DescribeCacheParametersResponse
    { _cpgdMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    , _cpgdParameters :: [Parameter]
      -- ^ A list of Parameter instances.
    , _cpgdCacheNodeTypeSpecificParameters :: [CacheNodeTypeSpecificParameter]
      -- ^ A list of parameters specific to a particular cache node type.
      -- Each element in the list contains detailed information about one
      -- parameter.
    } deriving (Show, Generic)

-- | Provides an identifier to allow retrieval of paginated results.
cpgdMarker :: Lens' DescribeCacheParametersResponse (Maybe Text)
cpgdMarker = lens _cpgdMarker (\s a -> s { _cpgdMarker = a })
{-# INLINE cpgdMarker #-}

-- | A list of Parameter instances.
cpgdParameters :: Lens' DescribeCacheParametersResponse ([Parameter])
cpgdParameters = lens _cpgdParameters (\s a -> s { _cpgdParameters = a })
{-# INLINE cpgdParameters #-}

-- | A list of parameters specific to a particular cache node type. Each element
-- in the list contains detailed information about one parameter.
cpgdCacheNodeTypeSpecificParameters :: Lens' DescribeCacheParametersResponse ([CacheNodeTypeSpecificParameter])
cpgdCacheNodeTypeSpecificParameters = lens _cpgdCacheNodeTypeSpecificParameters (\s a -> s { _cpgdCacheNodeTypeSpecificParameters = a })
{-# INLINE cpgdCacheNodeTypeSpecificParameters #-}

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
