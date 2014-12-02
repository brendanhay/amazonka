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

-- Module      : Network.AWS.ElastiCache.DescribeCacheSecurityGroups
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

-- | The /DescribeCacheSecurityGroups/ operation returns a list of cache security
-- group descriptions. If a cache security group name is specified, the list
-- will contain only the description of that group.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeCacheSecurityGroups.html>
module Network.AWS.ElastiCache.DescribeCacheSecurityGroups
    (
    -- * Request
      DescribeCacheSecurityGroups
    -- ** Request constructor
    , describeCacheSecurityGroups
    -- ** Request lenses
    , dcsg1CacheSecurityGroupName
    , dcsg1Marker
    , dcsg1MaxRecords

    -- * Response
    , DescribeCacheSecurityGroupsResponse
    -- ** Response constructor
    , describeCacheSecurityGroupsResponse
    -- ** Response lenses
    , dcsgr1CacheSecurityGroups
    , dcsgr1Marker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data DescribeCacheSecurityGroups = DescribeCacheSecurityGroups
    { _dcsg1CacheSecurityGroupName :: Maybe Text
    , _dcsg1Marker                 :: Maybe Text
    , _dcsg1MaxRecords             :: Maybe Int
    } deriving (Eq, Ord, Show)

-- | 'DescribeCacheSecurityGroups' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsg1CacheSecurityGroupName' @::@ 'Maybe' 'Text'
--
-- * 'dcsg1Marker' @::@ 'Maybe' 'Text'
--
-- * 'dcsg1MaxRecords' @::@ 'Maybe' 'Int'
--
describeCacheSecurityGroups :: DescribeCacheSecurityGroups
describeCacheSecurityGroups = DescribeCacheSecurityGroups
    { _dcsg1CacheSecurityGroupName = Nothing
    , _dcsg1MaxRecords             = Nothing
    , _dcsg1Marker                 = Nothing
    }

-- | The name of the cache security group to return details for.
dcsg1CacheSecurityGroupName :: Lens' DescribeCacheSecurityGroups (Maybe Text)
dcsg1CacheSecurityGroupName =
    lens _dcsg1CacheSecurityGroupName
        (\s a -> s { _dcsg1CacheSecurityGroupName = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by /MaxRecords/.
dcsg1Marker :: Lens' DescribeCacheSecurityGroups (Maybe Text)
dcsg1Marker = lens _dcsg1Marker (\s a -> s { _dcsg1Marker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified 'MaxRecords' value, a marker is included in the
-- response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
dcsg1MaxRecords :: Lens' DescribeCacheSecurityGroups (Maybe Int)
dcsg1MaxRecords = lens _dcsg1MaxRecords (\s a -> s { _dcsg1MaxRecords = a })

data DescribeCacheSecurityGroupsResponse = DescribeCacheSecurityGroupsResponse
    { _dcsgr1CacheSecurityGroups :: List "member" CacheSecurityGroup
    , _dcsgr1Marker              :: Maybe Text
    } deriving (Eq, Show)

-- | 'DescribeCacheSecurityGroupsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsgr1CacheSecurityGroups' @::@ ['CacheSecurityGroup']
--
-- * 'dcsgr1Marker' @::@ 'Maybe' 'Text'
--
describeCacheSecurityGroupsResponse :: DescribeCacheSecurityGroupsResponse
describeCacheSecurityGroupsResponse = DescribeCacheSecurityGroupsResponse
    { _dcsgr1Marker              = Nothing
    , _dcsgr1CacheSecurityGroups = mempty
    }

-- | A list of cache security groups. Each element in the list contains detailed
-- information about one group.
dcsgr1CacheSecurityGroups :: Lens' DescribeCacheSecurityGroupsResponse [CacheSecurityGroup]
dcsgr1CacheSecurityGroups =
    lens _dcsgr1CacheSecurityGroups
        (\s a -> s { _dcsgr1CacheSecurityGroups = a })
            . _List

-- | Provides an identifier to allow retrieval of paginated results.
dcsgr1Marker :: Lens' DescribeCacheSecurityGroupsResponse (Maybe Text)
dcsgr1Marker = lens _dcsgr1Marker (\s a -> s { _dcsgr1Marker = a })

instance ToPath DescribeCacheSecurityGroups where
    toPath = const "/"

instance ToQuery DescribeCacheSecurityGroups where
    toQuery DescribeCacheSecurityGroups{..} = mconcat
        [ "CacheSecurityGroupName" =? _dcsg1CacheSecurityGroupName
        , "Marker"                 =? _dcsg1Marker
        , "MaxRecords"             =? _dcsg1MaxRecords
        ]

instance ToHeaders DescribeCacheSecurityGroups

instance AWSRequest DescribeCacheSecurityGroups where
    type Sv DescribeCacheSecurityGroups = ElastiCache
    type Rs DescribeCacheSecurityGroups = DescribeCacheSecurityGroupsResponse

    request  = post "DescribeCacheSecurityGroups"
    response = xmlResponse

instance FromXML DescribeCacheSecurityGroupsResponse where
    parseXML = withElement "DescribeCacheSecurityGroupsResult" $ \x -> DescribeCacheSecurityGroupsResponse
        <$> x .@? "CacheSecurityGroups" .!@ mempty
        <*> x .@? "Marker"

instance AWSPager DescribeCacheSecurityGroups where
    page rq rs
        | stop (rq ^. dcsg1Marker) = Nothing
        | otherwise = (\x -> rq & dcsg1Marker ?~ x)
            <$> (rs ^. dcsgr1Marker)
