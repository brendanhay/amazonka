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

-- Module      : Network.AWS.ElastiCache.DescribeCacheSecurityGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeCacheSecurityGroups operation returns a list of cache security
-- group descriptions. If a cache security group name is specified, the list
-- will contain only the description of that group.
module Network.AWS.ElastiCache.DescribeCacheSecurityGroups
    (
    -- * Request
      DescribeCacheSecurityGroupsMessage
    -- ** Request constructor
    , describeCacheSecurityGroupsMessage
    -- ** Request lenses
    , dcsgm1CacheSecurityGroupName
    , dcsgm1Marker
    , dcsgm1MaxRecords

    -- * Response
    , CacheSecurityGroupMessage
    -- ** Response constructor
    , cacheSecurityGroupMessage
    -- ** Response lenses
    , csgmCacheSecurityGroups
    , csgmMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data DescribeCacheSecurityGroupsMessage = DescribeCacheSecurityGroupsMessage
    { _dcsgm1CacheSecurityGroupName :: Maybe Text
    , _dcsgm1Marker                 :: Maybe Text
    , _dcsgm1MaxRecords             :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeCacheSecurityGroupsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsgm1CacheSecurityGroupName' @::@ 'Maybe' 'Text'
--
-- * 'dcsgm1Marker' @::@ 'Maybe' 'Text'
--
-- * 'dcsgm1MaxRecords' @::@ 'Maybe' 'Int'
--
describeCacheSecurityGroupsMessage :: DescribeCacheSecurityGroupsMessage
describeCacheSecurityGroupsMessage = DescribeCacheSecurityGroupsMessage
    { _dcsgm1CacheSecurityGroupName = Nothing
    , _dcsgm1MaxRecords             = Nothing
    , _dcsgm1Marker                 = Nothing
    }

-- | The name of the cache security group to return details for.
dcsgm1CacheSecurityGroupName :: Lens' DescribeCacheSecurityGroupsMessage (Maybe Text)
dcsgm1CacheSecurityGroupName =
    lens _dcsgm1CacheSecurityGroupName
        (\s a -> s { _dcsgm1CacheSecurityGroupName = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
dcsgm1Marker :: Lens' DescribeCacheSecurityGroupsMessage (Maybe Text)
dcsgm1Marker = lens _dcsgm1Marker (\s a -> s { _dcsgm1Marker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
dcsgm1MaxRecords :: Lens' DescribeCacheSecurityGroupsMessage (Maybe Int)
dcsgm1MaxRecords = lens _dcsgm1MaxRecords (\s a -> s { _dcsgm1MaxRecords = a })
instance ToQuery DescribeCacheSecurityGroupsMessage

instance ToPath DescribeCacheSecurityGroupsMessage where
    toPath = const "/"

data CacheSecurityGroupMessage = CacheSecurityGroupMessage
    { _csgmCacheSecurityGroups :: [CacheSecurityGroup]
    , _csgmMarker              :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'CacheSecurityGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csgmCacheSecurityGroups' @::@ ['CacheSecurityGroup']
--
-- * 'csgmMarker' @::@ 'Maybe' 'Text'
--
cacheSecurityGroupMessage :: CacheSecurityGroupMessage
cacheSecurityGroupMessage = CacheSecurityGroupMessage
    { _csgmMarker              = Nothing
    , _csgmCacheSecurityGroups = mempty
    }

-- | A list of cache security groups. Each element in the list contains
-- detailed information about one group.
csgmCacheSecurityGroups :: Lens' CacheSecurityGroupMessage [CacheSecurityGroup]
csgmCacheSecurityGroups =
    lens _csgmCacheSecurityGroups (\s a -> s { _csgmCacheSecurityGroups = a })

-- | Provides an identifier to allow retrieval of paginated results.
csgmMarker :: Lens' CacheSecurityGroupMessage (Maybe Text)
csgmMarker = lens _csgmMarker (\s a -> s { _csgmMarker = a })
instance FromXML CacheSecurityGroupMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheSecurityGroupMessage"

instance AWSRequest DescribeCacheSecurityGroupsMessage where
    type Sv DescribeCacheSecurityGroupsMessage = ElastiCache
    type Rs DescribeCacheSecurityGroupsMessage = CacheSecurityGroupMessage

    request  = post "DescribeCacheSecurityGroups"
    response = xmlResponse $ \h x -> CacheSecurityGroupMessage
        <$> x %| "CacheSecurityGroups"
        <*> x %| "Marker"
