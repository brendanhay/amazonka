{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.DescribeCacheEngineVersions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The /DescribeCacheEngineVersions/ operation returns a list of the available
-- cache engines and their versions.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeCacheEngineVersions.html>
module Network.AWS.ElastiCache.DescribeCacheEngineVersions
    (
    -- * Request
      DescribeCacheEngineVersions
    -- ** Request constructor
    , describeCacheEngineVersions
    -- ** Request lenses
    , dcevCacheParameterGroupFamily
    , dcevDefaultOnly
    , dcevEngine
    , dcevEngineVersion
    , dcevMarker
    , dcevMaxRecords

    -- * Response
    , DescribeCacheEngineVersionsResponse
    -- ** Response constructor
    , describeCacheEngineVersionsResponse
    -- ** Response lenses
    , dcevrCacheEngineVersions
    , dcevrMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data DescribeCacheEngineVersions = DescribeCacheEngineVersions
    { _dcevCacheParameterGroupFamily :: Maybe Text
    , _dcevDefaultOnly               :: Maybe Bool
    , _dcevEngine                    :: Maybe Text
    , _dcevEngineVersion             :: Maybe Text
    , _dcevMarker                    :: Maybe Text
    , _dcevMaxRecords                :: Maybe Int
    } deriving (Eq, Ord, Show)

-- | 'DescribeCacheEngineVersions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcevCacheParameterGroupFamily' @::@ 'Maybe' 'Text'
--
-- * 'dcevDefaultOnly' @::@ 'Maybe' 'Bool'
--
-- * 'dcevEngine' @::@ 'Maybe' 'Text'
--
-- * 'dcevEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'dcevMarker' @::@ 'Maybe' 'Text'
--
-- * 'dcevMaxRecords' @::@ 'Maybe' 'Int'
--
describeCacheEngineVersions :: DescribeCacheEngineVersions
describeCacheEngineVersions = DescribeCacheEngineVersions
    { _dcevEngine                    = Nothing
    , _dcevEngineVersion             = Nothing
    , _dcevCacheParameterGroupFamily = Nothing
    , _dcevMaxRecords                = Nothing
    , _dcevMarker                    = Nothing
    , _dcevDefaultOnly               = Nothing
    }

-- | The name of a specific cache parameter group family to return details
-- for. Constraints: Must be 1 to 255 alphanumeric characters First
-- character must be a letter Cannot end with a hyphen or contain two
-- consecutive hyphens.
dcevCacheParameterGroupFamily :: Lens' DescribeCacheEngineVersions (Maybe Text)
dcevCacheParameterGroupFamily =
    lens _dcevCacheParameterGroupFamily
        (\s a -> s { _dcevCacheParameterGroupFamily = a })

-- | If /true/, specifies that only the default version of the specified
-- engine or engine and major version combination is to be returned.
dcevDefaultOnly :: Lens' DescribeCacheEngineVersions (Maybe Bool)
dcevDefaultOnly = lens _dcevDefaultOnly (\s a -> s { _dcevDefaultOnly = a })

-- | The cache engine to return. Valid values: @memcached@ | @redis@.
dcevEngine :: Lens' DescribeCacheEngineVersions (Maybe Text)
dcevEngine = lens _dcevEngine (\s a -> s { _dcevEngine = a })

-- | The cache engine version to return. Example: @1.4.14@.
dcevEngineVersion :: Lens' DescribeCacheEngineVersions (Maybe Text)
dcevEngineVersion =
    lens _dcevEngineVersion (\s a -> s { _dcevEngineVersion = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by /MaxRecords/.
dcevMarker :: Lens' DescribeCacheEngineVersions (Maybe Text)
dcevMarker = lens _dcevMarker (\s a -> s { _dcevMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified @MaxRecords@ value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
dcevMaxRecords :: Lens' DescribeCacheEngineVersions (Maybe Int)
dcevMaxRecords = lens _dcevMaxRecords (\s a -> s { _dcevMaxRecords = a })

data DescribeCacheEngineVersionsResponse = DescribeCacheEngineVersionsResponse
    { _dcevrCacheEngineVersions :: List "CacheEngineVersion" CacheEngineVersion
    , _dcevrMarker              :: Maybe Text
    } deriving (Eq, Show)

-- | 'DescribeCacheEngineVersionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcevrCacheEngineVersions' @::@ ['CacheEngineVersion']
--
-- * 'dcevrMarker' @::@ 'Maybe' 'Text'
--
describeCacheEngineVersionsResponse :: DescribeCacheEngineVersionsResponse
describeCacheEngineVersionsResponse = DescribeCacheEngineVersionsResponse
    { _dcevrMarker              = Nothing
    , _dcevrCacheEngineVersions = mempty
    }

-- | A list of cache engine version details. Each element in the list contains
-- detailed information about one cache engine version.
dcevrCacheEngineVersions :: Lens' DescribeCacheEngineVersionsResponse [CacheEngineVersion]
dcevrCacheEngineVersions =
    lens _dcevrCacheEngineVersions
        (\s a -> s { _dcevrCacheEngineVersions = a })
            . _List

-- | Provides an identifier to allow retrieval of paginated results.
dcevrMarker :: Lens' DescribeCacheEngineVersionsResponse (Maybe Text)
dcevrMarker = lens _dcevrMarker (\s a -> s { _dcevrMarker = a })

instance ToPath DescribeCacheEngineVersions where
    toPath = const "/"

instance ToQuery DescribeCacheEngineVersions where
    toQuery DescribeCacheEngineVersions{..} = mconcat
        [ "CacheParameterGroupFamily" =? _dcevCacheParameterGroupFamily
        , "DefaultOnly"               =? _dcevDefaultOnly
        , "Engine"                    =? _dcevEngine
        , "EngineVersion"             =? _dcevEngineVersion
        , "Marker"                    =? _dcevMarker
        , "MaxRecords"                =? _dcevMaxRecords
        ]

instance ToHeaders DescribeCacheEngineVersions

instance AWSRequest DescribeCacheEngineVersions where
    type Sv DescribeCacheEngineVersions = ElastiCache
    type Rs DescribeCacheEngineVersions = DescribeCacheEngineVersionsResponse

    request  = post "DescribeCacheEngineVersions"
    response = xmlResponse

instance FromXML DescribeCacheEngineVersionsResponse where
    parseXML = withElement "DescribeCacheEngineVersionsResult" $ \x -> DescribeCacheEngineVersionsResponse
        <$> x .@  "CacheEngineVersions"
        <*> x .@? "Marker"

instance AWSPager DescribeCacheEngineVersions where
    page rq rs
        | stop (rq ^. dcevMarker) = Nothing
        | otherwise = (\x -> rq & dcevMarker ?~ x)
            <$> (rs ^. dcevrMarker)
