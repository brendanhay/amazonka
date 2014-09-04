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
    , describeCacheEngineVersions
    -- ** Request lenses
    , dcevmDefaultOnly
    , dcevmMaxRecords
    , dcevmEngine
    , dcevmEngineVersion
    , dcevmCacheParameterGroupFamily
    , dcevmMarker

    -- * Response
    , DescribeCacheEngineVersionsResponse
    -- ** Response lenses
    , cevmCacheEngineVersions
    , cevmMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeCacheEngineVersions' request.
describeCacheEngineVersions :: DescribeCacheEngineVersions
describeCacheEngineVersions = DescribeCacheEngineVersions
    { _dcevmDefaultOnly = Nothing
    , _dcevmMaxRecords = Nothing
    , _dcevmEngine = Nothing
    , _dcevmEngineVersion = Nothing
    , _dcevmCacheParameterGroupFamily = Nothing
    , _dcevmMarker = Nothing
    }
{-# INLINE describeCacheEngineVersions #-}

data DescribeCacheEngineVersions = DescribeCacheEngineVersions
    { _dcevmDefaultOnly :: Maybe Bool
      -- ^ If true, specifies that only the default version of the specified
      -- engine or engine and major version combination is to be returned.
    , _dcevmMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a marker is
      -- included in the response so that the remaining results can be
      -- retrieved. Default: 100 Constraints: minimum 20; maximum 100.
    , _dcevmEngine :: Maybe Text
      -- ^ The cache engine to return. Valid values: memcached | redis.
    , _dcevmEngineVersion :: Maybe Text
      -- ^ The cache engine version to return. Example: 1.4.14.
    , _dcevmCacheParameterGroupFamily :: Maybe Text
      -- ^ The name of a specific cache parameter group family to return
      -- details for. Constraints: Must be 1 to 255 alphanumeric
      -- characters First character must be a letter Cannot end with a
      -- hyphen or contain two consecutive hyphens.
    , _dcevmMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker
      -- for pagination of results from this operation. If this parameter
      -- is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

-- | If true, specifies that only the default version of the specified engine or
-- engine and major version combination is to be returned.
dcevmDefaultOnly :: Lens' DescribeCacheEngineVersions (Maybe Bool)
dcevmDefaultOnly f x =
    f (_dcevmDefaultOnly x)
        <&> \y -> x { _dcevmDefaultOnly = y }
{-# INLINE dcevmDefaultOnly #-}

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
dcevmMaxRecords :: Lens' DescribeCacheEngineVersions (Maybe Integer)
dcevmMaxRecords f x =
    f (_dcevmMaxRecords x)
        <&> \y -> x { _dcevmMaxRecords = y }
{-# INLINE dcevmMaxRecords #-}

-- | The cache engine to return. Valid values: memcached | redis.
dcevmEngine :: Lens' DescribeCacheEngineVersions (Maybe Text)
dcevmEngine f x =
    f (_dcevmEngine x)
        <&> \y -> x { _dcevmEngine = y }
{-# INLINE dcevmEngine #-}

-- | The cache engine version to return. Example: 1.4.14.
dcevmEngineVersion :: Lens' DescribeCacheEngineVersions (Maybe Text)
dcevmEngineVersion f x =
    f (_dcevmEngineVersion x)
        <&> \y -> x { _dcevmEngineVersion = y }
{-# INLINE dcevmEngineVersion #-}

-- | The name of a specific cache parameter group family to return details for.
-- Constraints: Must be 1 to 255 alphanumeric characters First character must
-- be a letter Cannot end with a hyphen or contain two consecutive hyphens.
dcevmCacheParameterGroupFamily :: Lens' DescribeCacheEngineVersions (Maybe Text)
dcevmCacheParameterGroupFamily f x =
    f (_dcevmCacheParameterGroupFamily x)
        <&> \y -> x { _dcevmCacheParameterGroupFamily = y }
{-# INLINE dcevmCacheParameterGroupFamily #-}

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
dcevmMarker :: Lens' DescribeCacheEngineVersions (Maybe Text)
dcevmMarker f x =
    f (_dcevmMarker x)
        <&> \y -> x { _dcevmMarker = y }
{-# INLINE dcevmMarker #-}

instance ToQuery DescribeCacheEngineVersions where
    toQuery = genericQuery def

data DescribeCacheEngineVersionsResponse = DescribeCacheEngineVersionsResponse
    { _cevmCacheEngineVersions :: [CacheEngineVersion]
      -- ^ A list of cache engine version details. Each element in the list
      -- contains detailed information about once cache engine version.
    , _cevmMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    } deriving (Show, Generic)

-- | A list of cache engine version details. Each element in the list contains
-- detailed information about once cache engine version.
cevmCacheEngineVersions :: Lens' DescribeCacheEngineVersionsResponse ([CacheEngineVersion])
cevmCacheEngineVersions f x =
    f (_cevmCacheEngineVersions x)
        <&> \y -> x { _cevmCacheEngineVersions = y }
{-# INLINE cevmCacheEngineVersions #-}

-- | Provides an identifier to allow retrieval of paginated results.
cevmMarker :: Lens' DescribeCacheEngineVersionsResponse (Maybe Text)
cevmMarker f x =
    f (_cevmMarker x)
        <&> \y -> x { _cevmMarker = y }
{-# INLINE cevmMarker #-}

instance FromXML DescribeCacheEngineVersionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeCacheEngineVersions where
    type Sv DescribeCacheEngineVersions = ElastiCache
    type Rs DescribeCacheEngineVersions = DescribeCacheEngineVersionsResponse

    request = post "DescribeCacheEngineVersions"
    response _ = xmlResponse

instance AWSPager DescribeCacheEngineVersions where
    next rq rs = (\x -> rq { _dcevmMarker = Just x })
        <$> (_cevmMarker rs)
