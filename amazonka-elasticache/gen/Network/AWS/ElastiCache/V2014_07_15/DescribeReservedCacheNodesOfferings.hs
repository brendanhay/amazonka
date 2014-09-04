{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.DescribeReservedCacheNodesOfferings
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeReservedCacheNodesOfferings operation lists available reserved
-- cache node offerings. https://elasticache.amazonaws.com/
-- ?Action=DescribeReservedCacheNodesOfferings
-- &ReservedCacheNodesOfferingId=438012d3-4052-4cc7-b2e3-8d3372e0e706
-- &Version=2014-03-24 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20140401T192317Z &X-Amz-Credential= 31536000 Heavy Utilization
-- Hourly 0.123 162.0 memcached 0.0 SampleOfferingId cache.m1.small
-- 521b420a-2961-11e1-bd06-6fe008f046c3.
module Network.AWS.ElastiCache.V2014_07_15.DescribeReservedCacheNodesOfferings
    (
    -- * Request
      DescribeReservedCacheNodesOfferings
    -- ** Request constructor
    , describeReservedCacheNodesOfferings
    -- ** Request lenses
    , drcnomMaxRecords
    , drcnomReservedCacheNodesOfferingId
    , drcnomCacheNodeType
    , drcnomDuration
    , drcnomProductDescription
    , drcnomOfferingType
    , drcnomMarker

    -- * Response
    , DescribeReservedCacheNodesOfferingsResponse
    -- ** Response lenses
    , rcnomReservedCacheNodesOfferings
    , rcnomMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeReservedCacheNodesOfferings' request.
describeReservedCacheNodesOfferings :: DescribeReservedCacheNodesOfferings
describeReservedCacheNodesOfferings = DescribeReservedCacheNodesOfferings
    { _drcnomMaxRecords = Nothing
    , _drcnomReservedCacheNodesOfferingId = Nothing
    , _drcnomCacheNodeType = Nothing
    , _drcnomDuration = Nothing
    , _drcnomProductDescription = Nothing
    , _drcnomOfferingType = Nothing
    , _drcnomMarker = Nothing
    }
{-# INLINE describeReservedCacheNodesOfferings #-}

data DescribeReservedCacheNodesOfferings = DescribeReservedCacheNodesOfferings
    { _drcnomMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a marker is
      -- included in the response so that the remaining results can be
      -- retrieved. Default: 100 Constraints: minimum 20; maximum 100.
    , _drcnomReservedCacheNodesOfferingId :: Maybe Text
      -- ^ The offering identifier filter value. Use this parameter to show
      -- only the available offering that matches the specified
      -- reservation identifier. Example:
      -- 438012d3-4052-4cc7-b2e3-8d3372e0e706.
    , _drcnomCacheNodeType :: Maybe Text
      -- ^ The cache node type filter value. Use this parameter to show only
      -- the available offerings matching the specified cache node type.
    , _drcnomDuration :: Maybe Text
      -- ^ Duration filter value, specified in years or seconds. Use this
      -- parameter to show only reservations for a given duration. Valid
      -- Values: 1 | 3 | 31536000 | 94608000.
    , _drcnomProductDescription :: Maybe Text
      -- ^ The product description filter value. Use this parameter to show
      -- only the available offerings matching the specified product
      -- description.
    , _drcnomOfferingType :: Maybe Text
      -- ^ The offering type filter value. Use this parameter to show only
      -- the available offerings matching the specified offering type.
      -- Valid Values: "Light Utilization" | "Medium Utilization" | "Heavy
      -- Utilization".
    , _drcnomMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker
      -- for pagination of results from this operation. If this parameter
      -- is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
drcnomMaxRecords :: Lens' DescribeReservedCacheNodesOfferings (Maybe Integer)
drcnomMaxRecords f x =
    f (_drcnomMaxRecords x)
        <&> \y -> x { _drcnomMaxRecords = y }
{-# INLINE drcnomMaxRecords #-}

-- | The offering identifier filter value. Use this parameter to show only the
-- available offering that matches the specified reservation identifier.
-- Example: 438012d3-4052-4cc7-b2e3-8d3372e0e706.
drcnomReservedCacheNodesOfferingId :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnomReservedCacheNodesOfferingId f x =
    f (_drcnomReservedCacheNodesOfferingId x)
        <&> \y -> x { _drcnomReservedCacheNodesOfferingId = y }
{-# INLINE drcnomReservedCacheNodesOfferingId #-}

-- | The cache node type filter value. Use this parameter to show only the
-- available offerings matching the specified cache node type.
drcnomCacheNodeType :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnomCacheNodeType f x =
    f (_drcnomCacheNodeType x)
        <&> \y -> x { _drcnomCacheNodeType = y }
{-# INLINE drcnomCacheNodeType #-}

-- | Duration filter value, specified in years or seconds. Use this parameter to
-- show only reservations for a given duration. Valid Values: 1 | 3 | 31536000
-- | 94608000.
drcnomDuration :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnomDuration f x =
    f (_drcnomDuration x)
        <&> \y -> x { _drcnomDuration = y }
{-# INLINE drcnomDuration #-}

-- | The product description filter value. Use this parameter to show only the
-- available offerings matching the specified product description.
drcnomProductDescription :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnomProductDescription f x =
    f (_drcnomProductDescription x)
        <&> \y -> x { _drcnomProductDescription = y }
{-# INLINE drcnomProductDescription #-}

-- | The offering type filter value. Use this parameter to show only the
-- available offerings matching the specified offering type. Valid Values:
-- "Light Utilization" | "Medium Utilization" | "Heavy Utilization".
drcnomOfferingType :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnomOfferingType f x =
    f (_drcnomOfferingType x)
        <&> \y -> x { _drcnomOfferingType = y }
{-# INLINE drcnomOfferingType #-}

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
drcnomMarker :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnomMarker f x =
    f (_drcnomMarker x)
        <&> \y -> x { _drcnomMarker = y }
{-# INLINE drcnomMarker #-}

instance ToQuery DescribeReservedCacheNodesOfferings where
    toQuery = genericQuery def

data DescribeReservedCacheNodesOfferingsResponse = DescribeReservedCacheNodesOfferingsResponse
    { _rcnomReservedCacheNodesOfferings :: [ReservedCacheNodesOffering]
      -- ^ A list of reserved cache node offerings. Each element in the list
      -- contains detailed information about one offering.
    , _rcnomMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    } deriving (Show, Generic)

-- | A list of reserved cache node offerings. Each element in the list contains
-- detailed information about one offering.
rcnomReservedCacheNodesOfferings :: Lens' DescribeReservedCacheNodesOfferingsResponse ([ReservedCacheNodesOffering])
rcnomReservedCacheNodesOfferings f x =
    f (_rcnomReservedCacheNodesOfferings x)
        <&> \y -> x { _rcnomReservedCacheNodesOfferings = y }
{-# INLINE rcnomReservedCacheNodesOfferings #-}

-- | Provides an identifier to allow retrieval of paginated results.
rcnomMarker :: Lens' DescribeReservedCacheNodesOfferingsResponse (Maybe Text)
rcnomMarker f x =
    f (_rcnomMarker x)
        <&> \y -> x { _rcnomMarker = y }
{-# INLINE rcnomMarker #-}

instance FromXML DescribeReservedCacheNodesOfferingsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeReservedCacheNodesOfferings where
    type Sv DescribeReservedCacheNodesOfferings = ElastiCache
    type Rs DescribeReservedCacheNodesOfferings = DescribeReservedCacheNodesOfferingsResponse

    request = post "DescribeReservedCacheNodesOfferings"
    response _ = xmlResponse

instance AWSPager DescribeReservedCacheNodesOfferings where
    next rq rs = (\x -> rq { _drcnomMarker = Just x })
        <$> (_rcnomMarker rs)
