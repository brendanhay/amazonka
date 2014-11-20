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

-- Module      : Network.AWS.ElastiCache.DescribeCacheSubnetGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeCacheSubnetGroups operation returns a list of cache subnet
-- group descriptions. If a subnet group name is specified, the list will
-- contain only the description of that group.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeCacheSubnetGroups.html>
module Network.AWS.ElastiCache.DescribeCacheSubnetGroups
    (
    -- * Request
      DescribeCacheSubnetGroups
    -- ** Request constructor
    , describeCacheSubnetGroups
    -- ** Request lenses
    , dcsgCacheSubnetGroupName
    , dcsgMarker
    , dcsgMaxRecords

    -- * Response
    , DescribeCacheSubnetGroupsResponse
    -- ** Response constructor
    , describeCacheSubnetGroupsResponse
    -- ** Response lenses
    , dcsgrCacheSubnetGroups
    , dcsgrMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data DescribeCacheSubnetGroups = DescribeCacheSubnetGroups
    { _dcsgCacheSubnetGroupName :: Maybe Text
    , _dcsgMarker               :: Maybe Text
    , _dcsgMaxRecords           :: Maybe Int
    } deriving (Eq, Ord, Show)

-- | 'DescribeCacheSubnetGroups' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsgCacheSubnetGroupName' @::@ 'Maybe' 'Text'
--
-- * 'dcsgMarker' @::@ 'Maybe' 'Text'
--
-- * 'dcsgMaxRecords' @::@ 'Maybe' 'Int'
--
describeCacheSubnetGroups :: DescribeCacheSubnetGroups
describeCacheSubnetGroups = DescribeCacheSubnetGroups
    { _dcsgCacheSubnetGroupName = Nothing
    , _dcsgMaxRecords           = Nothing
    , _dcsgMarker               = Nothing
    }

-- | The name of the cache subnet group to return details for.
dcsgCacheSubnetGroupName :: Lens' DescribeCacheSubnetGroups (Maybe Text)
dcsgCacheSubnetGroupName =
    lens _dcsgCacheSubnetGroupName
        (\s a -> s { _dcsgCacheSubnetGroupName = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
dcsgMarker :: Lens' DescribeCacheSubnetGroups (Maybe Text)
dcsgMarker = lens _dcsgMarker (\s a -> s { _dcsgMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
dcsgMaxRecords :: Lens' DescribeCacheSubnetGroups (Maybe Int)
dcsgMaxRecords = lens _dcsgMaxRecords (\s a -> s { _dcsgMaxRecords = a })

data DescribeCacheSubnetGroupsResponse = DescribeCacheSubnetGroupsResponse
    { _dcsgrCacheSubnetGroups :: List "CacheSubnetGroup" CacheSubnetGroup
    , _dcsgrMarker            :: Maybe Text
    } deriving (Eq, Show)

-- | 'DescribeCacheSubnetGroupsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsgrCacheSubnetGroups' @::@ ['CacheSubnetGroup']
--
-- * 'dcsgrMarker' @::@ 'Maybe' 'Text'
--
describeCacheSubnetGroupsResponse :: DescribeCacheSubnetGroupsResponse
describeCacheSubnetGroupsResponse = DescribeCacheSubnetGroupsResponse
    { _dcsgrMarker            = Nothing
    , _dcsgrCacheSubnetGroups = mempty
    }

-- | A list of cache subnet groups. Each element in the list contains detailed
-- information about one group.
dcsgrCacheSubnetGroups :: Lens' DescribeCacheSubnetGroupsResponse [CacheSubnetGroup]
dcsgrCacheSubnetGroups =
    lens _dcsgrCacheSubnetGroups (\s a -> s { _dcsgrCacheSubnetGroups = a })
        . _List

-- | Provides an identifier to allow retrieval of paginated results.
dcsgrMarker :: Lens' DescribeCacheSubnetGroupsResponse (Maybe Text)
dcsgrMarker = lens _dcsgrMarker (\s a -> s { _dcsgrMarker = a })

instance ToPath DescribeCacheSubnetGroups where
    toPath = const "/"

instance ToQuery DescribeCacheSubnetGroups where
    toQuery DescribeCacheSubnetGroups{..} = mconcat
        [ "CacheSubnetGroupName" =? _dcsgCacheSubnetGroupName
        , "Marker"               =? _dcsgMarker
        , "MaxRecords"           =? _dcsgMaxRecords
        ]

instance ToHeaders DescribeCacheSubnetGroups

instance AWSRequest DescribeCacheSubnetGroups where
    type Sv DescribeCacheSubnetGroups = ElastiCache
    type Rs DescribeCacheSubnetGroups = DescribeCacheSubnetGroupsResponse

    request  = post "DescribeCacheSubnetGroups"
    response = xmlResponse

instance FromXML DescribeCacheSubnetGroupsResponse where
    parseXML = withElement "DescribeCacheSubnetGroupsResult" $ \x -> DescribeCacheSubnetGroupsResponse
        <$> x .@  "CacheSubnetGroups"
        <*> x .@? "Marker"

instance AWSPager DescribeCacheSubnetGroups where
  next rq rs

  = (\x -> rq & dcsgMarker ?~ x)
  <$> (rs ^. dcsgMarker)

    


Some kind of operator / class to check the types whether to continue?
