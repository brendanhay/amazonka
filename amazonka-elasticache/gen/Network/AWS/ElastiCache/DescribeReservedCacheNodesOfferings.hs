{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache
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
module Network.AWS.ElastiCache
    (
    -- * Request
      DescribeReservedCacheNodesOfferings
    -- ** Request constructor
    , mkDescribeReservedCacheNodesOfferings
    -- ** Request lenses
    , drcnoReservedCacheNodesOfferingId
    , drcnoCacheNodeType
    , drcnoDuration
    , drcnoProductDescription
    , drcnoOfferingType
    , drcnoMaxRecords
    , drcnoMarker

    -- * Response
    , DescribeReservedCacheNodesOfferingsResponse
    -- ** Response constructor
    , mkDescribeReservedCacheNodesOfferingsResponse
    -- ** Response lenses
    , drcnorMarker
    , drcnorReservedCacheNodesOfferings
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import Network.AWS.Prelude

-- | Represents the input of a DescribeReservedCacheNodesOfferings operation.
data DescribeReservedCacheNodesOfferings = DescribeReservedCacheNodesOfferings
    { _drcnoReservedCacheNodesOfferingId :: !(Maybe Text)
    , _drcnoCacheNodeType :: !(Maybe Text)
    , _drcnoDuration :: !(Maybe Text)
    , _drcnoProductDescription :: !(Maybe Text)
    , _drcnoOfferingType :: !(Maybe Text)
    , _drcnoMaxRecords :: !(Maybe Integer)
    , _drcnoMarker :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeReservedCacheNodesOfferings' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReservedCacheNodesOfferingId ::@ @Maybe Text@
--
-- * @CacheNodeType ::@ @Maybe Text@
--
-- * @Duration ::@ @Maybe Text@
--
-- * @ProductDescription ::@ @Maybe Text@
--
-- * @OfferingType ::@ @Maybe Text@
--
-- * @MaxRecords ::@ @Maybe Integer@
--
-- * @Marker ::@ @Maybe Text@
--
mkDescribeReservedCacheNodesOfferings :: DescribeReservedCacheNodesOfferings
mkDescribeReservedCacheNodesOfferings = DescribeReservedCacheNodesOfferings
    { _drcnoReservedCacheNodesOfferingId = Nothing
    , _drcnoCacheNodeType = Nothing
    , _drcnoDuration = Nothing
    , _drcnoProductDescription = Nothing
    , _drcnoOfferingType = Nothing
    , _drcnoMaxRecords = Nothing
    , _drcnoMarker = Nothing
    }

-- | The offering identifier filter value. Use this parameter to show only the
-- available offering that matches the specified reservation identifier.
-- Example: 438012d3-4052-4cc7-b2e3-8d3372e0e706.
drcnoReservedCacheNodesOfferingId :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnoReservedCacheNodesOfferingId =
    lens _drcnoReservedCacheNodesOfferingId
         (\s a -> s { _drcnoReservedCacheNodesOfferingId = a })

-- | The cache node type filter value. Use this parameter to show only the
-- available offerings matching the specified cache node type.
drcnoCacheNodeType :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnoCacheNodeType =
    lens _drcnoCacheNodeType (\s a -> s { _drcnoCacheNodeType = a })

-- | Duration filter value, specified in years or seconds. Use this parameter to
-- show only reservations for a given duration. Valid Values: 1 | 3 | 31536000
-- | 94608000.
drcnoDuration :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnoDuration = lens _drcnoDuration (\s a -> s { _drcnoDuration = a })

-- | The product description filter value. Use this parameter to show only the
-- available offerings matching the specified product description.
drcnoProductDescription :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnoProductDescription =
    lens _drcnoProductDescription
         (\s a -> s { _drcnoProductDescription = a })

-- | The offering type filter value. Use this parameter to show only the
-- available offerings matching the specified offering type. Valid Values:
-- "Light Utilization" | "Medium Utilization" | "Heavy Utilization".
drcnoOfferingType :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnoOfferingType =
    lens _drcnoOfferingType (\s a -> s { _drcnoOfferingType = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
drcnoMaxRecords :: Lens' DescribeReservedCacheNodesOfferings (Maybe Integer)
drcnoMaxRecords = lens _drcnoMaxRecords (\s a -> s { _drcnoMaxRecords = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
drcnoMarker :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnoMarker = lens _drcnoMarker (\s a -> s { _drcnoMarker = a })

instance ToQuery DescribeReservedCacheNodesOfferings where
    toQuery = genericQuery def

-- | Represents the output of a DescribeReservedCacheNodesOfferings operation.
data DescribeReservedCacheNodesOfferingsResponse = DescribeReservedCacheNodesOfferingsResponse
    { _drcnorMarker :: !(Maybe Text)
    , _drcnorReservedCacheNodesOfferings :: [ReservedCacheNodesOffering]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeReservedCacheNodesOfferingsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Marker ::@ @Maybe Text@
--
-- * @ReservedCacheNodesOfferings ::@ @[ReservedCacheNodesOffering]@
--
mkDescribeReservedCacheNodesOfferingsResponse :: DescribeReservedCacheNodesOfferingsResponse
mkDescribeReservedCacheNodesOfferingsResponse = DescribeReservedCacheNodesOfferingsResponse
    { _drcnorMarker = Nothing
    , _drcnorReservedCacheNodesOfferings = mempty
    }

-- | Provides an identifier to allow retrieval of paginated results.
drcnorMarker :: Lens' DescribeReservedCacheNodesOfferingsResponse (Maybe Text)
drcnorMarker = lens _drcnorMarker (\s a -> s { _drcnorMarker = a })

-- | A list of reserved cache node offerings. Each element in the list contains
-- detailed information about one offering.
drcnorReservedCacheNodesOfferings :: Lens' DescribeReservedCacheNodesOfferingsResponse [ReservedCacheNodesOffering]
drcnorReservedCacheNodesOfferings =
    lens _drcnorReservedCacheNodesOfferings
         (\s a -> s { _drcnorReservedCacheNodesOfferings = a })

instance FromXML DescribeReservedCacheNodesOfferingsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeReservedCacheNodesOfferings where
    type Sv DescribeReservedCacheNodesOfferings = ElastiCache
    type Rs DescribeReservedCacheNodesOfferings = DescribeReservedCacheNodesOfferingsResponse

    request = post "DescribeReservedCacheNodesOfferings"
    response _ = xmlResponse

instance AWSPager DescribeReservedCacheNodesOfferings where
    next rq rs = (\x -> rq & drcnoMarker ?~ x)
        <$> (rs ^. drcnorMarker)
