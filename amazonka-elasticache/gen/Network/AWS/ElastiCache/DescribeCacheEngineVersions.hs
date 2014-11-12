{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.ElastiCache.DescribeCacheEngineVersions
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
module Network.AWS.ElastiCache.DescribeCacheEngineVersions
    (
    -- * Request
      DescribeCacheEngineVersionsMessage
    -- ** Request constructor
    , describeCacheEngineVersionsMessage
    -- ** Request lenses
    , dcevmCacheParameterGroupFamily
    , dcevmDefaultOnly
    , dcevmEngine
    , dcevmEngineVersion
    , dcevmMarker
    , dcevmMaxRecords

    -- * Response
    , CacheEngineVersionMessage
    -- ** Response constructor
    , cacheEngineVersionMessage
    -- ** Response lenses
    , cevmCacheEngineVersions
    , cevmMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data DescribeCacheEngineVersionsMessage = DescribeCacheEngineVersionsMessage
    { _dcevmCacheParameterGroupFamily :: Maybe Text
    , _dcevmDefaultOnly               :: Maybe Bool
    , _dcevmEngine                    :: Maybe Text
    , _dcevmEngineVersion             :: Maybe Text
    , _dcevmMarker                    :: Maybe Text
    , _dcevmMaxRecords                :: Maybe Int
    } (Eq, Ord, Show, Generic)

-- | 'DescribeCacheEngineVersionsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcevmCacheParameterGroupFamily' @::@ 'Maybe' 'Text'
--
-- * 'dcevmDefaultOnly' @::@ 'Maybe' 'Bool'
--
-- * 'dcevmEngine' @::@ 'Maybe' 'Text'
--
-- * 'dcevmEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'dcevmMarker' @::@ 'Maybe' 'Text'
--
-- * 'dcevmMaxRecords' @::@ 'Maybe' 'Int'
--
describeCacheEngineVersionsMessage :: DescribeCacheEngineVersionsMessage
describeCacheEngineVersionsMessage = DescribeCacheEngineVersionsMessage
    { _dcevmEngine                    = Nothing
    , _dcevmEngineVersion             = Nothing
    , _dcevmCacheParameterGroupFamily = Nothing
    , _dcevmMaxRecords                = Nothing
    , _dcevmMarker                    = Nothing
    , _dcevmDefaultOnly               = Nothing
    }

-- | The name of a specific cache parameter group family to return details
-- for. Constraints: Must be 1 to 255 alphanumeric characters First
-- character must be a letter Cannot end with a hyphen or contain two
-- consecutive hyphens.
dcevmCacheParameterGroupFamily :: Lens' DescribeCacheEngineVersionsMessage (Maybe Text)
dcevmCacheParameterGroupFamily =
    lens _dcevmCacheParameterGroupFamily
        (\s a -> s { _dcevmCacheParameterGroupFamily = a })

-- | If true, specifies that only the default version of the specified engine
-- or engine and major version combination is to be returned.
dcevmDefaultOnly :: Lens' DescribeCacheEngineVersionsMessage (Maybe Bool)
dcevmDefaultOnly = lens _dcevmDefaultOnly (\s a -> s { _dcevmDefaultOnly = a })

-- | The cache engine to return. Valid values: memcached | redis.
dcevmEngine :: Lens' DescribeCacheEngineVersionsMessage (Maybe Text)
dcevmEngine = lens _dcevmEngine (\s a -> s { _dcevmEngine = a })

-- | The cache engine version to return. Example: 1.4.14.
dcevmEngineVersion :: Lens' DescribeCacheEngineVersionsMessage (Maybe Text)
dcevmEngineVersion =
    lens _dcevmEngineVersion (\s a -> s { _dcevmEngineVersion = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
dcevmMarker :: Lens' DescribeCacheEngineVersionsMessage (Maybe Text)
dcevmMarker = lens _dcevmMarker (\s a -> s { _dcevmMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
dcevmMaxRecords :: Lens' DescribeCacheEngineVersionsMessage (Maybe Int)
dcevmMaxRecords = lens _dcevmMaxRecords (\s a -> s { _dcevmMaxRecords = a })
instance ToQuery DescribeCacheEngineVersionsMessage

instance ToPath DescribeCacheEngineVersionsMessage where
    toPath = const "/"

data CacheEngineVersionMessage = CacheEngineVersionMessage
    { _cevmCacheEngineVersions :: [CacheEngineVersion]
    , _cevmMarker              :: Maybe Text
    } (Eq, Show, Generic)

-- | 'CacheEngineVersionMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cevmCacheEngineVersions' @::@ ['CacheEngineVersion']
--
-- * 'cevmMarker' @::@ 'Maybe' 'Text'
--
cacheEngineVersionMessage :: CacheEngineVersionMessage
cacheEngineVersionMessage = CacheEngineVersionMessage
    { _cevmMarker              = Nothing
    , _cevmCacheEngineVersions = mempty
    }

-- | A list of cache engine version details. Each element in the list contains
-- detailed information about one cache engine version.
cevmCacheEngineVersions :: Lens' CacheEngineVersionMessage [CacheEngineVersion]
cevmCacheEngineVersions =
    lens _cevmCacheEngineVersions (\s a -> s { _cevmCacheEngineVersions = a })

-- | Provides an identifier to allow retrieval of paginated results.
cevmMarker :: Lens' CacheEngineVersionMessage (Maybe Text)
cevmMarker = lens _cevmMarker (\s a -> s { _cevmMarker = a })

instance FromXML CacheEngineVersionMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheEngineVersionMessage"

instance AWSRequest DescribeCacheEngineVersionsMessage where
    type Sv DescribeCacheEngineVersionsMessage = ElastiCache
    type Rs DescribeCacheEngineVersionsMessage = CacheEngineVersionMessage

    request  = post "DescribeCacheEngineVersions"
    response = xmlResponse $ \h x -> CacheEngineVersionMessage
        <$> x %| "CacheEngineVersions"
        <*> x %| "Marker"
