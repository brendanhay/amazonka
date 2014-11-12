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
module Network.AWS.ElastiCache.DescribeReservedCacheNodesOfferings
    (
    -- * Request
      DescribeReservedCacheNodesOfferingsMessage
    -- ** Request constructor
    , describeReservedCacheNodesOfferings
    -- ** Request lenses
    , drcnomCacheNodeType
    , drcnomDuration
    , drcnomMarker
    , drcnomMaxRecords
    , drcnomOfferingType
    , drcnomProductDescription
    , drcnomReservedCacheNodesOfferingId

    -- * Response
    , ReservedCacheNodesOfferingMessage
    -- ** Response constructor
    , describeReservedCacheNodesOfferingsResponse
    -- ** Response lenses
    , rcnomMarker
    , rcnomReservedCacheNodesOfferings
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data DescribeReservedCacheNodesOfferingsMessage = DescribeReservedCacheNodesOfferingsMessage
    { _drcnomCacheNodeType                :: Maybe Text
    , _drcnomDuration                     :: Maybe Text
    , _drcnomMarker                       :: Maybe Text
    , _drcnomMaxRecords                   :: Maybe Int
    , _drcnomOfferingType                 :: Maybe Text
    , _drcnomProductDescription           :: Maybe Text
    , _drcnomReservedCacheNodesOfferingId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeReservedCacheNodesOfferingsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drcnomCacheNodeType' @::@ 'Maybe' 'Text'
--
-- * 'drcnomDuration' @::@ 'Maybe' 'Text'
--
-- * 'drcnomMarker' @::@ 'Maybe' 'Text'
--
-- * 'drcnomMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'drcnomOfferingType' @::@ 'Maybe' 'Text'
--
-- * 'drcnomProductDescription' @::@ 'Maybe' 'Text'
--
-- * 'drcnomReservedCacheNodesOfferingId' @::@ 'Maybe' 'Text'
--
describeReservedCacheNodesOfferings :: DescribeReservedCacheNodesOfferingsMessage
describeReservedCacheNodesOfferings = DescribeReservedCacheNodesOfferingsMessage
    { _drcnomReservedCacheNodesOfferingId = Nothing
    , _drcnomCacheNodeType                = Nothing
    , _drcnomDuration                     = Nothing
    , _drcnomProductDescription           = Nothing
    , _drcnomOfferingType                 = Nothing
    , _drcnomMaxRecords                   = Nothing
    , _drcnomMarker                       = Nothing
    }

-- | The cache node type filter value. Use this parameter to show only the
-- available offerings matching the specified cache node type.
drcnomCacheNodeType :: Lens' DescribeReservedCacheNodesOfferingsMessage (Maybe Text)
drcnomCacheNodeType =
    lens _drcnomCacheNodeType (\s a -> s { _drcnomCacheNodeType = a })

-- | Duration filter value, specified in years or seconds. Use this parameter
-- to show only reservations for a given duration. Valid Values: 1 | 3 |
-- 31536000 | 94608000.
drcnomDuration :: Lens' DescribeReservedCacheNodesOfferingsMessage (Maybe Text)
drcnomDuration = lens _drcnomDuration (\s a -> s { _drcnomDuration = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
drcnomMarker :: Lens' DescribeReservedCacheNodesOfferingsMessage (Maybe Text)
drcnomMarker = lens _drcnomMarker (\s a -> s { _drcnomMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
drcnomMaxRecords :: Lens' DescribeReservedCacheNodesOfferingsMessage (Maybe Int)
drcnomMaxRecords = lens _drcnomMaxRecords (\s a -> s { _drcnomMaxRecords = a })

-- | The offering type filter value. Use this parameter to show only the
-- available offerings matching the specified offering type. Valid Values:
-- "Light Utilization" | "Medium Utilization" | "Heavy Utilization".
drcnomOfferingType :: Lens' DescribeReservedCacheNodesOfferingsMessage (Maybe Text)
drcnomOfferingType =
    lens _drcnomOfferingType (\s a -> s { _drcnomOfferingType = a })

-- | The product description filter value. Use this parameter to show only the
-- available offerings matching the specified product description.
drcnomProductDescription :: Lens' DescribeReservedCacheNodesOfferingsMessage (Maybe Text)
drcnomProductDescription =
    lens _drcnomProductDescription
        (\s a -> s { _drcnomProductDescription = a })

-- | The offering identifier filter value. Use this parameter to show only the
-- available offering that matches the specified reservation identifier.
-- Example: 438012d3-4052-4cc7-b2e3-8d3372e0e706.
drcnomReservedCacheNodesOfferingId :: Lens' DescribeReservedCacheNodesOfferingsMessage (Maybe Text)
drcnomReservedCacheNodesOfferingId =
    lens _drcnomReservedCacheNodesOfferingId
        (\s a -> s { _drcnomReservedCacheNodesOfferingId = a })

instance ToQuery DescribeReservedCacheNodesOfferingsMessage

instance ToPath DescribeReservedCacheNodesOfferingsMessage where
    toPath = const "/"

data ReservedCacheNodesOfferingMessage = ReservedCacheNodesOfferingMessage
    { _rcnomMarker                      :: Maybe Text
    , _rcnomReservedCacheNodesOfferings :: [ReservedCacheNodesOffering]
    } deriving (Eq, Show, Generic)

-- | 'ReservedCacheNodesOfferingMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcnomMarker' @::@ 'Maybe' 'Text'
--
-- * 'rcnomReservedCacheNodesOfferings' @::@ ['ReservedCacheNodesOffering']
--
describeReservedCacheNodesOfferingsResponse :: ReservedCacheNodesOfferingMessage
describeReservedCacheNodesOfferingsResponse = ReservedCacheNodesOfferingMessage
    { _rcnomMarker                      = Nothing
    , _rcnomReservedCacheNodesOfferings = mempty
    }

-- | Provides an identifier to allow retrieval of paginated results.
rcnomMarker :: Lens' ReservedCacheNodesOfferingMessage (Maybe Text)
rcnomMarker = lens _rcnomMarker (\s a -> s { _rcnomMarker = a })

-- | A list of reserved cache node offerings. Each element in the list
-- contains detailed information about one offering.
rcnomReservedCacheNodesOfferings :: Lens' ReservedCacheNodesOfferingMessage [ReservedCacheNodesOffering]
rcnomReservedCacheNodesOfferings =
    lens _rcnomReservedCacheNodesOfferings
        (\s a -> s { _rcnomReservedCacheNodesOfferings = a })

instance FromXML ReservedCacheNodesOfferingMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedCacheNodesOfferingMessage"

instance AWSRequest DescribeReservedCacheNodesOfferingsMessage where
    type Sv DescribeReservedCacheNodesOfferingsMessage = ElastiCache
    type Rs DescribeReservedCacheNodesOfferingsMessage = ReservedCacheNodesOfferingMessage

    request  = post "DescribeReservedCacheNodesOfferings"
    response = xmlResponse $ \h x -> ReservedCacheNodesOfferingMessage
        <$> x %| "Marker"
        <*> x %| "ReservedCacheNodesOfferings"
