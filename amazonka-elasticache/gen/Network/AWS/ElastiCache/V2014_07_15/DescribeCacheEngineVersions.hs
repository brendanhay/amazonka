{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.DescribeCacheEngineVersions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeCacheEngineVersions operation returns a list of the available
-- cache engines and their versions.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=DescribeCacheEngineVersions &MaxRecords=100 &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= memcached1.4 memcached memcached version 1.4.14
-- memcached 1.4.14 memcached1.4 memcached memcached version 1.4.5 memcached
-- 1.4.5 a6ac9ad2-f8a4-11e1-a4d1-a345e5370093.
module Network.AWS.ElastiCache.V2014_07_15.DescribeCacheEngineVersions
    (
    -- * Request
      DescribeCacheEngineVersions
    -- ** Request constructor
    , mkDescribeCacheEngineVersions
    -- ** Request lenses
    , dcevEngine
    , dcevEngineVersion
    , dcevCacheParameterGroupFamily
    , dcevMaxRecords
    , dcevMarker
    , dcevDefaultOnly

    -- * Response
    , DescribeCacheEngineVersionsResponse
    -- ** Response constructor
    , mkDescribeCacheEngineVersionsResponse
    -- ** Response lenses
    , dcevrMarker
    , dcevrCacheEngineVersions
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Represents the input of a DescribeCacheEngineVersions operation.
data DescribeCacheEngineVersions = DescribeCacheEngineVersions
    { _dcevEngine :: Maybe Text
    , _dcevEngineVersion :: Maybe Text
    , _dcevCacheParameterGroupFamily :: Maybe Text
    , _dcevMaxRecords :: Maybe Integer
    , _dcevMarker :: Maybe Text
    , _dcevDefaultOnly :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeCacheEngineVersions' request.
mkDescribeCacheEngineVersions :: DescribeCacheEngineVersions
mkDescribeCacheEngineVersions = DescribeCacheEngineVersions
    { _dcevEngine = Nothing
    , _dcevEngineVersion = Nothing
    , _dcevCacheParameterGroupFamily = Nothing
    , _dcevMaxRecords = Nothing
    , _dcevMarker = Nothing
    , _dcevDefaultOnly = Nothing
    }

-- | The cache engine to return. Valid values: memcached | redis.
dcevEngine :: Lens' DescribeCacheEngineVersions (Maybe Text)
dcevEngine = lens _dcevEngine (\s a -> s { _dcevEngine = a })

-- | The cache engine version to return. Example: 1.4.14.
dcevEngineVersion :: Lens' DescribeCacheEngineVersions (Maybe Text)
dcevEngineVersion =
    lens _dcevEngineVersion (\s a -> s { _dcevEngineVersion = a })

-- | The name of a specific cache parameter group family to return details for.
-- Constraints: Must be 1 to 255 alphanumeric characters First character must
-- be a letter Cannot end with a hyphen or contain two consecutive hyphens.
dcevCacheParameterGroupFamily :: Lens' DescribeCacheEngineVersions (Maybe Text)
dcevCacheParameterGroupFamily =
    lens _dcevCacheParameterGroupFamily
         (\s a -> s { _dcevCacheParameterGroupFamily = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
dcevMaxRecords :: Lens' DescribeCacheEngineVersions (Maybe Integer)
dcevMaxRecords = lens _dcevMaxRecords (\s a -> s { _dcevMaxRecords = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
dcevMarker :: Lens' DescribeCacheEngineVersions (Maybe Text)
dcevMarker = lens _dcevMarker (\s a -> s { _dcevMarker = a })

-- | If true, specifies that only the default version of the specified engine or
-- engine and major version combination is to be returned.
dcevDefaultOnly :: Lens' DescribeCacheEngineVersions (Maybe Bool)
dcevDefaultOnly = lens _dcevDefaultOnly (\s a -> s { _dcevDefaultOnly = a })

instance ToQuery DescribeCacheEngineVersions where
    toQuery = genericQuery def

-- | Represents the output of a DescribeCacheEngineVersions operation.
data DescribeCacheEngineVersionsResponse = DescribeCacheEngineVersionsResponse
    { _dcevrMarker :: Maybe Text
    , _dcevrCacheEngineVersions :: [CacheEngineVersion]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeCacheEngineVersionsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDescribeCacheEngineVersionsResponse :: DescribeCacheEngineVersionsResponse
mkDescribeCacheEngineVersionsResponse = DescribeCacheEngineVersionsResponse
    { _dcevrMarker = Nothing
    , _dcevrCacheEngineVersions = mempty
    }

-- | Provides an identifier to allow retrieval of paginated results.
dcevrMarker :: Lens' DescribeCacheEngineVersionsResponse (Maybe Text)
dcevrMarker = lens _dcevrMarker (\s a -> s { _dcevrMarker = a })

-- | A list of cache engine version details. Each element in the list contains
-- detailed information about once cache engine version.
dcevrCacheEngineVersions :: Lens' DescribeCacheEngineVersionsResponse [CacheEngineVersion]
dcevrCacheEngineVersions =
    lens _dcevrCacheEngineVersions
         (\s a -> s { _dcevrCacheEngineVersions = a })

instance FromXML DescribeCacheEngineVersionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeCacheEngineVersions where
    type Sv DescribeCacheEngineVersions = ElastiCache
    type Rs DescribeCacheEngineVersions = DescribeCacheEngineVersionsResponse

    request = post "DescribeCacheEngineVersions"
    response _ = xmlResponse

instance AWSPager DescribeCacheEngineVersions where
    next rq rs = (\x -> rq & dcevMarker ?~ x)
        <$> (rs ^. dcevrMarker)
