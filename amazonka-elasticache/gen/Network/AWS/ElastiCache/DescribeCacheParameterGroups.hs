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

-- Module      : Network.AWS.ElastiCache.DescribeCacheParameterGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | The /DescribeCacheParameterGroups/ operation returns a list of cache parameter
-- group descriptions. If a cache parameter group name is specified, the list
-- will contain only the descriptions for that group.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeCacheParameterGroups.html>
module Network.AWS.ElastiCache.DescribeCacheParameterGroups
    (
    -- * Request
      DescribeCacheParameterGroups
    -- ** Request constructor
    , describeCacheParameterGroups
    -- ** Request lenses
    , dcpgCacheParameterGroupName
    , dcpgMarker
    , dcpgMaxRecords

    -- * Response
    , DescribeCacheParameterGroupsResponse
    -- ** Response constructor
    , describeCacheParameterGroupsResponse
    -- ** Response lenses
    , dcpgrCacheParameterGroups
    , dcpgrMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data DescribeCacheParameterGroups = DescribeCacheParameterGroups
    { _dcpgCacheParameterGroupName :: Maybe Text
    , _dcpgMarker                  :: Maybe Text
    , _dcpgMaxRecords              :: Maybe Int
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeCacheParameterGroups' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcpgCacheParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'dcpgMarker' @::@ 'Maybe' 'Text'
--
-- * 'dcpgMaxRecords' @::@ 'Maybe' 'Int'
--
describeCacheParameterGroups :: DescribeCacheParameterGroups
describeCacheParameterGroups = DescribeCacheParameterGroups
    { _dcpgCacheParameterGroupName = Nothing
    , _dcpgMaxRecords              = Nothing
    , _dcpgMarker                  = Nothing
    }

-- | The name of a specific cache parameter group to return details for.
dcpgCacheParameterGroupName :: Lens' DescribeCacheParameterGroups (Maybe Text)
dcpgCacheParameterGroupName =
    lens _dcpgCacheParameterGroupName
        (\s a -> s { _dcpgCacheParameterGroupName = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by /MaxRecords/.
dcpgMarker :: Lens' DescribeCacheParameterGroups (Maybe Text)
dcpgMarker = lens _dcpgMarker (\s a -> s { _dcpgMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified 'MaxRecords' value, a marker is included in the
-- response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
dcpgMaxRecords :: Lens' DescribeCacheParameterGroups (Maybe Int)
dcpgMaxRecords = lens _dcpgMaxRecords (\s a -> s { _dcpgMaxRecords = a })

data DescribeCacheParameterGroupsResponse = DescribeCacheParameterGroupsResponse
    { _dcpgrCacheParameterGroups :: List "member" CacheParameterGroup
    , _dcpgrMarker               :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'DescribeCacheParameterGroupsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcpgrCacheParameterGroups' @::@ ['CacheParameterGroup']
--
-- * 'dcpgrMarker' @::@ 'Maybe' 'Text'
--
describeCacheParameterGroupsResponse :: DescribeCacheParameterGroupsResponse
describeCacheParameterGroupsResponse = DescribeCacheParameterGroupsResponse
    { _dcpgrMarker               = Nothing
    , _dcpgrCacheParameterGroups = mempty
    }

-- | A list of cache parameter groups. Each element in the list contains detailed
-- information about one cache parameter group.
dcpgrCacheParameterGroups :: Lens' DescribeCacheParameterGroupsResponse [CacheParameterGroup]
dcpgrCacheParameterGroups =
    lens _dcpgrCacheParameterGroups
        (\s a -> s { _dcpgrCacheParameterGroups = a })
            . _List

-- | Provides an identifier to allow retrieval of paginated results.
dcpgrMarker :: Lens' DescribeCacheParameterGroupsResponse (Maybe Text)
dcpgrMarker = lens _dcpgrMarker (\s a -> s { _dcpgrMarker = a })

instance ToPath DescribeCacheParameterGroups where
    toPath = const "/"

instance ToQuery DescribeCacheParameterGroups where
    toQuery DescribeCacheParameterGroups{..} = mconcat
        [ "CacheParameterGroupName" =? _dcpgCacheParameterGroupName
        , "Marker"                  =? _dcpgMarker
        , "MaxRecords"              =? _dcpgMaxRecords
        ]

instance ToHeaders DescribeCacheParameterGroups

instance AWSRequest DescribeCacheParameterGroups where
    type Sv DescribeCacheParameterGroups = ElastiCache
    type Rs DescribeCacheParameterGroups = DescribeCacheParameterGroupsResponse

    request  = post "DescribeCacheParameterGroups"
    response = xmlResponse

instance FromXML DescribeCacheParameterGroupsResponse where
    parseXML = withElement "DescribeCacheParameterGroupsResult" $ \x -> DescribeCacheParameterGroupsResponse
        <$> x .@? "CacheParameterGroups" .!@ mempty
        <*> x .@? "Marker"

instance AWSPager DescribeCacheParameterGroups where
    page rq rs
        | stop (rq ^. dcpgMarker) = Nothing
        | otherwise = (\x -> rq & dcpgMarker ?~ x)
            <$> (rs ^. dcpgrMarker)
