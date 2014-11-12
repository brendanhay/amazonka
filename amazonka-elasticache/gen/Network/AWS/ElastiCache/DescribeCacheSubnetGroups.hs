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
module Network.AWS.ElastiCache.DescribeCacheSubnetGroups
    (
    -- * Request
      DescribeCacheSubnetGroupsMessage
    -- ** Request constructor
    , describeCacheSubnetGroupsMessage
    -- ** Request lenses
    , dcsgmCacheSubnetGroupName
    , dcsgmMarker
    , dcsgmMaxRecords

    -- * Response
    , CacheSubnetGroupMessage
    -- ** Response constructor
    , cacheSubnetGroupMessage
    -- ** Response lenses
    , csgm1CacheSubnetGroups
    , csgm1Marker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data DescribeCacheSubnetGroupsMessage = DescribeCacheSubnetGroupsMessage
    { _dcsgmCacheSubnetGroupName :: Maybe Text
    , _dcsgmMarker               :: Maybe Text
    , _dcsgmMaxRecords           :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeCacheSubnetGroupsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsgmCacheSubnetGroupName' @::@ 'Maybe' 'Text'
--
-- * 'dcsgmMarker' @::@ 'Maybe' 'Text'
--
-- * 'dcsgmMaxRecords' @::@ 'Maybe' 'Int'
--
describeCacheSubnetGroupsMessage :: DescribeCacheSubnetGroupsMessage
describeCacheSubnetGroupsMessage = DescribeCacheSubnetGroupsMessage
    { _dcsgmCacheSubnetGroupName = Nothing
    , _dcsgmMaxRecords           = Nothing
    , _dcsgmMarker               = Nothing
    }

-- | The name of the cache subnet group to return details for.
dcsgmCacheSubnetGroupName :: Lens' DescribeCacheSubnetGroupsMessage (Maybe Text)
dcsgmCacheSubnetGroupName =
    lens _dcsgmCacheSubnetGroupName
        (\s a -> s { _dcsgmCacheSubnetGroupName = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
dcsgmMarker :: Lens' DescribeCacheSubnetGroupsMessage (Maybe Text)
dcsgmMarker = lens _dcsgmMarker (\s a -> s { _dcsgmMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
dcsgmMaxRecords :: Lens' DescribeCacheSubnetGroupsMessage (Maybe Int)
dcsgmMaxRecords = lens _dcsgmMaxRecords (\s a -> s { _dcsgmMaxRecords = a })

instance ToQuery DescribeCacheSubnetGroupsMessage

instance ToPath DescribeCacheSubnetGroupsMessage where
    toPath = const "/"

data CacheSubnetGroupMessage = CacheSubnetGroupMessage
    { _csgm1CacheSubnetGroups :: [CacheSubnetGroup]
    , _csgm1Marker            :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'CacheSubnetGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csgm1CacheSubnetGroups' @::@ ['CacheSubnetGroup']
--
-- * 'csgm1Marker' @::@ 'Maybe' 'Text'
--
cacheSubnetGroupMessage :: CacheSubnetGroupMessage
cacheSubnetGroupMessage = CacheSubnetGroupMessage
    { _csgm1Marker            = Nothing
    , _csgm1CacheSubnetGroups = mempty
    }

-- | A list of cache subnet groups. Each element in the list contains detailed
-- information about one group.
csgm1CacheSubnetGroups :: Lens' CacheSubnetGroupMessage [CacheSubnetGroup]
csgm1CacheSubnetGroups =
    lens _csgm1CacheSubnetGroups (\s a -> s { _csgm1CacheSubnetGroups = a })

-- | Provides an identifier to allow retrieval of paginated results.
csgm1Marker :: Lens' CacheSubnetGroupMessage (Maybe Text)
csgm1Marker = lens _csgm1Marker (\s a -> s { _csgm1Marker = a })

instance FromXML CacheSubnetGroupMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheSubnetGroupMessage"

instance AWSRequest DescribeCacheSubnetGroupsMessage where
    type Sv DescribeCacheSubnetGroupsMessage = ElastiCache
    type Rs DescribeCacheSubnetGroupsMessage = CacheSubnetGroupMessage

    request  = post "DescribeCacheSubnetGroups"
    response = xmlResponse $ \h x -> CacheSubnetGroupMessage
        <$> x %| "CacheSubnetGroups"
        <*> x %| "Marker"
