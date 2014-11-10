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

-- Module      : Network.AWS.ElastiCache.DescribeCacheParameterGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeCacheParameterGroups operation returns a list of cache
-- parameter group descriptions. If a cache parameter group name is specified,
-- the list will contain only the descriptions for that group.
module Network.AWS.ElastiCache.DescribeCacheParameterGroups
    (
    -- * Request
      DescribeCacheParameterGroupsMessage
    -- ** Request constructor
    , describeCacheParameterGroups
    -- ** Request lenses
    , dcpgmCacheParameterGroupName
    , dcpgmMarker
    , dcpgmMaxRecords

    -- * Response
    , CacheParameterGroupsMessage
    -- ** Response constructor
    , describeCacheParameterGroupsResponse
    -- ** Response lenses
    , cpgmCacheParameterGroups
    , cpgmMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data DescribeCacheParameterGroupsMessage = DescribeCacheParameterGroupsMessage
    { _dcpgmCacheParameterGroupName :: Maybe Text
    , _dcpgmMarker                  :: Maybe Text
    , _dcpgmMaxRecords              :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeCacheParameterGroupsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcpgmCacheParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'dcpgmMarker' @::@ 'Maybe' 'Text'
--
-- * 'dcpgmMaxRecords' @::@ 'Maybe' 'Int'
--
describeCacheParameterGroups :: DescribeCacheParameterGroupsMessage
describeCacheParameterGroups = DescribeCacheParameterGroupsMessage
    { _dcpgmCacheParameterGroupName = Nothing
    , _dcpgmMaxRecords              = Nothing
    , _dcpgmMarker                  = Nothing
    }

-- | The name of a specific cache parameter group to return details for.
dcpgmCacheParameterGroupName :: Lens' DescribeCacheParameterGroupsMessage (Maybe Text)
dcpgmCacheParameterGroupName =
    lens _dcpgmCacheParameterGroupName
        (\s a -> s { _dcpgmCacheParameterGroupName = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
dcpgmMarker :: Lens' DescribeCacheParameterGroupsMessage (Maybe Text)
dcpgmMarker = lens _dcpgmMarker (\s a -> s { _dcpgmMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
dcpgmMaxRecords :: Lens' DescribeCacheParameterGroupsMessage (Maybe Int)
dcpgmMaxRecords = lens _dcpgmMaxRecords (\s a -> s { _dcpgmMaxRecords = a })

instance ToPath DescribeCacheParameterGroupsMessage where
    toPath = const "/"

instance ToQuery DescribeCacheParameterGroupsMessage

data CacheParameterGroupsMessage = CacheParameterGroupsMessage
    { _cpgmCacheParameterGroups :: [CacheParameterGroup]
    , _cpgmMarker               :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'CacheParameterGroupsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpgmCacheParameterGroups' @::@ ['CacheParameterGroup']
--
-- * 'cpgmMarker' @::@ 'Maybe' 'Text'
--
describeCacheParameterGroupsResponse :: CacheParameterGroupsMessage
describeCacheParameterGroupsResponse = CacheParameterGroupsMessage
    { _cpgmMarker               = Nothing
    , _cpgmCacheParameterGroups = mempty
    }

-- | A list of cache parameter groups. Each element in the list contains
-- detailed information about one cache parameter group.
cpgmCacheParameterGroups :: Lens' CacheParameterGroupsMessage [CacheParameterGroup]
cpgmCacheParameterGroups =
    lens _cpgmCacheParameterGroups
        (\s a -> s { _cpgmCacheParameterGroups = a })

-- | Provides an identifier to allow retrieval of paginated results.
cpgmMarker :: Lens' CacheParameterGroupsMessage (Maybe Text)
cpgmMarker = lens _cpgmMarker (\s a -> s { _cpgmMarker = a })

instance AWSRequest DescribeCacheParameterGroupsMessage where
    type Sv DescribeCacheParameterGroupsMessage = ElastiCache
    type Rs DescribeCacheParameterGroupsMessage = CacheParameterGroupsMessage

    request  = post "DescribeCacheParameterGroups"
    response = xmlResponse $ \h x -> CacheParameterGroupsMessage
        <$> x %| "CacheParameterGroups"
        <*> x %| "Marker"
