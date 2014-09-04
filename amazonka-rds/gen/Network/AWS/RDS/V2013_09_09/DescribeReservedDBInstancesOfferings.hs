{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.DescribeReservedDBInstancesOfferings
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists available reserved DB instance offerings. https://rds.amazonaws.com/
-- ?Action=DescribeReservedDBInstancesOfferings
-- &ReservedDBInstancesOfferingId=438012d3-4052-4cc7-b2e3-8d3372e0e706
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-12-18T18%3A31%3A36.118Z &AWSAccessKeyId= &Signature=
-- c/2012-04-02/"> 31536000 Heavy Utilization USD Hourly 0.123 162.0 mysql 0.0
-- false SampleOfferingId db.m1.small 521b420a-2961-11e1-bd06-6fe008f046c3.
module Network.AWS.RDS.V2013_09_09.DescribeReservedDBInstancesOfferings
    (
    -- * Request
      DescribeReservedDBInstancesOfferings
    -- ** Request constructor
    , describeReservedDBInstancesOfferings
    -- ** Request lenses
    , drdbiomMultiAZ
    , drdbiomMaxRecords
    , drdbiomReservedDBInstancesOfferingId
    , drdbiomDBInstanceClass
    , drdbiomDuration
    , drdbiomProductDescription
    , drdbiomOfferingType
    , drdbiomMarker

    -- * Response
    , DescribeReservedDBInstancesOfferingsResponse
    -- ** Response lenses
    , rdbiomReservedDBInstancesOfferings
    , rdbiomMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeReservedDBInstancesOfferings' request.
describeReservedDBInstancesOfferings :: DescribeReservedDBInstancesOfferings
describeReservedDBInstancesOfferings = DescribeReservedDBInstancesOfferings
    { _drdbiomMultiAZ = Nothing
    , _drdbiomMaxRecords = Nothing
    , _drdbiomReservedDBInstancesOfferingId = Nothing
    , _drdbiomDBInstanceClass = Nothing
    , _drdbiomDuration = Nothing
    , _drdbiomProductDescription = Nothing
    , _drdbiomOfferingType = Nothing
    , _drdbiomMarker = Nothing
    }
{-# INLINE describeReservedDBInstancesOfferings #-}

data DescribeReservedDBInstancesOfferings = DescribeReservedDBInstancesOfferings
    { _drdbiomMultiAZ :: Maybe Bool
      -- ^ The Multi-AZ filter value. Specify this parameter to show only
      -- the available offerings matching the specified Multi-AZ
      -- parameter.
    , _drdbiomMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- than the MaxRecords value is available, a pagination token called
      -- a marker is included in the response so that the following
      -- results can be retrieved. Default: 100 Constraints: minimum 20,
      -- maximum 100.
    , _drdbiomReservedDBInstancesOfferingId :: Maybe Text
      -- ^ The offering identifier filter value. Specify this parameter to
      -- show only the available offering that matches the specified
      -- reservation identifier. Example:
      -- 438012d3-4052-4cc7-b2e3-8d3372e0e706.
    , _drdbiomDBInstanceClass :: Maybe Text
      -- ^ The DB instance class filter value. Specify this parameter to
      -- show only the available offerings matching the specified DB
      -- instance class.
    , _drdbiomDuration :: Maybe Text
      -- ^ Duration filter value, specified in years or seconds. Specify
      -- this parameter to show only reservations for this duration. Valid
      -- Values: 1 | 3 | 31536000 | 94608000.
    , _drdbiomProductDescription :: Maybe Text
      -- ^ Product description filter value. Specify this parameter to show
      -- only the available offerings matching the specified product
      -- description.
    , _drdbiomOfferingType :: Maybe Text
      -- ^ The offering type filter value. Specify this parameter to show
      -- only the available offerings matching the specified offering
      -- type. Valid Values: "Light Utilization" | "Medium Utilization" |
      -- "Heavy Utilization".
    , _drdbiomMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If
      -- this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

-- | The Multi-AZ filter value. Specify this parameter to show only the
-- available offerings matching the specified Multi-AZ parameter.
drdbiomMultiAZ :: Lens' DescribeReservedDBInstancesOfferings (Maybe Bool)
drdbiomMultiAZ f x =
    f (_drdbiomMultiAZ x)
        <&> \y -> x { _drdbiomMultiAZ = y }
{-# INLINE drdbiomMultiAZ #-}

-- | The maximum number of records to include in the response. If more than the
-- MaxRecords value is available, a pagination token called a marker is
-- included in the response so that the following results can be retrieved.
-- Default: 100 Constraints: minimum 20, maximum 100.
drdbiomMaxRecords :: Lens' DescribeReservedDBInstancesOfferings (Maybe Integer)
drdbiomMaxRecords f x =
    f (_drdbiomMaxRecords x)
        <&> \y -> x { _drdbiomMaxRecords = y }
{-# INLINE drdbiomMaxRecords #-}

-- | The offering identifier filter value. Specify this parameter to show only
-- the available offering that matches the specified reservation identifier.
-- Example: 438012d3-4052-4cc7-b2e3-8d3372e0e706.
drdbiomReservedDBInstancesOfferingId :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdbiomReservedDBInstancesOfferingId f x =
    f (_drdbiomReservedDBInstancesOfferingId x)
        <&> \y -> x { _drdbiomReservedDBInstancesOfferingId = y }
{-# INLINE drdbiomReservedDBInstancesOfferingId #-}

-- | The DB instance class filter value. Specify this parameter to show only the
-- available offerings matching the specified DB instance class.
drdbiomDBInstanceClass :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdbiomDBInstanceClass f x =
    f (_drdbiomDBInstanceClass x)
        <&> \y -> x { _drdbiomDBInstanceClass = y }
{-# INLINE drdbiomDBInstanceClass #-}

-- | Duration filter value, specified in years or seconds. Specify this
-- parameter to show only reservations for this duration. Valid Values: 1 | 3
-- | 31536000 | 94608000.
drdbiomDuration :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdbiomDuration f x =
    f (_drdbiomDuration x)
        <&> \y -> x { _drdbiomDuration = y }
{-# INLINE drdbiomDuration #-}

-- | Product description filter value. Specify this parameter to show only the
-- available offerings matching the specified product description.
drdbiomProductDescription :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdbiomProductDescription f x =
    f (_drdbiomProductDescription x)
        <&> \y -> x { _drdbiomProductDescription = y }
{-# INLINE drdbiomProductDescription #-}

-- | The offering type filter value. Specify this parameter to show only the
-- available offerings matching the specified offering type. Valid Values:
-- "Light Utilization" | "Medium Utilization" | "Heavy Utilization".
drdbiomOfferingType :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdbiomOfferingType f x =
    f (_drdbiomOfferingType x)
        <&> \y -> x { _drdbiomOfferingType = y }
{-# INLINE drdbiomOfferingType #-}

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
drdbiomMarker :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdbiomMarker f x =
    f (_drdbiomMarker x)
        <&> \y -> x { _drdbiomMarker = y }
{-# INLINE drdbiomMarker #-}

instance ToQuery DescribeReservedDBInstancesOfferings where
    toQuery = genericQuery def

data DescribeReservedDBInstancesOfferingsResponse = DescribeReservedDBInstancesOfferingsResponse
    { _rdbiomReservedDBInstancesOfferings :: [ReservedDBInstancesOffering]
      -- ^ A list of reserved DB instance offerings.
    , _rdbiomMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If
      -- this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

-- | A list of reserved DB instance offerings.
rdbiomReservedDBInstancesOfferings :: Lens' DescribeReservedDBInstancesOfferingsResponse ([ReservedDBInstancesOffering])
rdbiomReservedDBInstancesOfferings f x =
    f (_rdbiomReservedDBInstancesOfferings x)
        <&> \y -> x { _rdbiomReservedDBInstancesOfferings = y }
{-# INLINE rdbiomReservedDBInstancesOfferings #-}

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
rdbiomMarker :: Lens' DescribeReservedDBInstancesOfferingsResponse (Maybe Text)
rdbiomMarker f x =
    f (_rdbiomMarker x)
        <&> \y -> x { _rdbiomMarker = y }
{-# INLINE rdbiomMarker #-}

instance FromXML DescribeReservedDBInstancesOfferingsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeReservedDBInstancesOfferings where
    type Sv DescribeReservedDBInstancesOfferings = RDS
    type Rs DescribeReservedDBInstancesOfferings = DescribeReservedDBInstancesOfferingsResponse

    request = post "DescribeReservedDBInstancesOfferings"
    response _ = xmlResponse

instance AWSPager DescribeReservedDBInstancesOfferings where
    next rq rs = (\x -> rq { _drdbiomMarker = Just x })
        <$> (_rdbiomMarker rs)
