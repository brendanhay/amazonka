{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.DescribeReservedCacheNodesOfferings
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeReservedCacheNodesOfferings operation lists available reserved
-- cache node offerings.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeReservedCacheNodesOfferings.html>
module Network.AWS.ElastiCache.DescribeReservedCacheNodesOfferings
    (
    -- * Request
      DescribeReservedCacheNodesOfferings
    -- ** Request constructor
    , describeReservedCacheNodesOfferings
    -- ** Request lenses
    , drcnoCacheNodeType
    , drcnoDuration
    , drcnoMarker
    , drcnoMaxRecords
    , drcnoOfferingType
    , drcnoProductDescription
    , drcnoReservedCacheNodesOfferingId

    -- * Response
    , DescribeReservedCacheNodesOfferingsResponse
    -- ** Response constructor
    , describeReservedCacheNodesOfferingsResponse
    -- ** Response lenses
    , drcnorMarker
    , drcnorReservedCacheNodesOfferings
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data DescribeReservedCacheNodesOfferings = DescribeReservedCacheNodesOfferings
    { _drcnoCacheNodeType                :: Maybe Text
    , _drcnoDuration                     :: Maybe Text
    , _drcnoMarker                       :: Maybe Text
    , _drcnoMaxRecords                   :: Maybe Int
    , _drcnoOfferingType                 :: Maybe Text
    , _drcnoProductDescription           :: Maybe Text
    , _drcnoReservedCacheNodesOfferingId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeReservedCacheNodesOfferings' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drcnoCacheNodeType' @::@ 'Maybe' 'Text'
--
-- * 'drcnoDuration' @::@ 'Maybe' 'Text'
--
-- * 'drcnoMarker' @::@ 'Maybe' 'Text'
--
-- * 'drcnoMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'drcnoOfferingType' @::@ 'Maybe' 'Text'
--
-- * 'drcnoProductDescription' @::@ 'Maybe' 'Text'
--
-- * 'drcnoReservedCacheNodesOfferingId' @::@ 'Maybe' 'Text'
--
describeReservedCacheNodesOfferings :: DescribeReservedCacheNodesOfferings
describeReservedCacheNodesOfferings = DescribeReservedCacheNodesOfferings
    { _drcnoReservedCacheNodesOfferingId = Nothing
    , _drcnoCacheNodeType                = Nothing
    , _drcnoDuration                     = Nothing
    , _drcnoProductDescription           = Nothing
    , _drcnoOfferingType                 = Nothing
    , _drcnoMaxRecords                   = Nothing
    , _drcnoMarker                       = Nothing
    }

-- | The cache node type filter value. Use this parameter to show only the
-- available offerings matching the specified cache node type.
drcnoCacheNodeType :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnoCacheNodeType =
    lens _drcnoCacheNodeType (\s a -> s { _drcnoCacheNodeType = a })

-- | Duration filter value, specified in years or seconds. Use this parameter
-- to show only reservations for a given duration. Valid Values: 1 | 3 |
-- 31536000 | 94608000.
drcnoDuration :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnoDuration = lens _drcnoDuration (\s a -> s { _drcnoDuration = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
drcnoMarker :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnoMarker = lens _drcnoMarker (\s a -> s { _drcnoMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
drcnoMaxRecords :: Lens' DescribeReservedCacheNodesOfferings (Maybe Int)
drcnoMaxRecords = lens _drcnoMaxRecords (\s a -> s { _drcnoMaxRecords = a })

-- | The offering type filter value. Use this parameter to show only the
-- available offerings matching the specified offering type. Valid Values:
-- "Light Utilization" | "Medium Utilization" | "Heavy Utilization".
drcnoOfferingType :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnoOfferingType =
    lens _drcnoOfferingType (\s a -> s { _drcnoOfferingType = a })

-- | The product description filter value. Use this parameter to show only the
-- available offerings matching the specified product description.
drcnoProductDescription :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnoProductDescription =
    lens _drcnoProductDescription (\s a -> s { _drcnoProductDescription = a })

-- | The offering identifier filter value. Use this parameter to show only the
-- available offering that matches the specified reservation identifier.
-- Example: 438012d3-4052-4cc7-b2e3-8d3372e0e706.
drcnoReservedCacheNodesOfferingId :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnoReservedCacheNodesOfferingId =
    lens _drcnoReservedCacheNodesOfferingId
        (\s a -> s { _drcnoReservedCacheNodesOfferingId = a })

data DescribeReservedCacheNodesOfferingsResponse = DescribeReservedCacheNodesOfferingsResponse
    { _drcnorMarker                      :: Maybe Text
    , _drcnorReservedCacheNodesOfferings :: [ReservedCacheNodesOffering]
    } deriving (Eq, Show, Generic)

-- | 'DescribeReservedCacheNodesOfferingsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drcnorMarker' @::@ 'Maybe' 'Text'
--
-- * 'drcnorReservedCacheNodesOfferings' @::@ ['ReservedCacheNodesOffering']
--
describeReservedCacheNodesOfferingsResponse :: DescribeReservedCacheNodesOfferingsResponse
describeReservedCacheNodesOfferingsResponse = DescribeReservedCacheNodesOfferingsResponse
    { _drcnorMarker                      = Nothing
    , _drcnorReservedCacheNodesOfferings = mempty
    }

-- | Provides an identifier to allow retrieval of paginated results.
drcnorMarker :: Lens' DescribeReservedCacheNodesOfferingsResponse (Maybe Text)
drcnorMarker = lens _drcnorMarker (\s a -> s { _drcnorMarker = a })

-- | A list of reserved cache node offerings. Each element in the list
-- contains detailed information about one offering.
drcnorReservedCacheNodesOfferings :: Lens' DescribeReservedCacheNodesOfferingsResponse [ReservedCacheNodesOffering]
drcnorReservedCacheNodesOfferings =
    lens _drcnorReservedCacheNodesOfferings
        (\s a -> s { _drcnorReservedCacheNodesOfferings = a })

instance ToPath DescribeReservedCacheNodesOfferings where
    toPath = const "/"

instance ToQuery DescribeReservedCacheNodesOfferings

instance ToHeaders DescribeReservedCacheNodesOfferings

instance AWSRequest DescribeReservedCacheNodesOfferings where
    type Sv DescribeReservedCacheNodesOfferings = ElastiCache
    type Rs DescribeReservedCacheNodesOfferings = DescribeReservedCacheNodesOfferingsResponse

    request  = post "DescribeReservedCacheNodesOfferings"
    response = xmlResponse

instance FromXML DescribeReservedCacheNodesOfferingsResponse where
    parseXML c = DescribeReservedCacheNodesOfferingsResponse
        <$> c .:? "Marker"
        <*> c .: "ReservedCacheNodesOfferings"
