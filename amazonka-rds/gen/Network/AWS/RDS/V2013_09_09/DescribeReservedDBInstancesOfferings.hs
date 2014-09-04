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
    , mkDescribeReservedDBInstancesOfferingsMessage
    -- ** Request lenses
    , drdbiomReservedDBInstancesOfferingId
    , drdbiomDBInstanceClass
    , drdbiomDuration
    , drdbiomProductDescription
    , drdbiomOfferingType
    , drdbiomMultiAZ
    , drdbiomMaxRecords
    , drdbiomMarker

    -- * Response
    , DescribeReservedDBInstancesOfferingsResponse
    -- ** Response lenses
    , rdbiomMarker
    , rdbiomReservedDBInstancesOfferings
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeReservedDBInstancesOfferings' request.
mkDescribeReservedDBInstancesOfferingsMessage :: DescribeReservedDBInstancesOfferings
mkDescribeReservedDBInstancesOfferingsMessage = DescribeReservedDBInstancesOfferings
    { _drdbiomReservedDBInstancesOfferingId = Nothing
    , _drdbiomDBInstanceClass = Nothing
    , _drdbiomDuration = Nothing
    , _drdbiomProductDescription = Nothing
    , _drdbiomOfferingType = Nothing
    , _drdbiomMultiAZ = Nothing
    , _drdbiomMaxRecords = Nothing
    , _drdbiomMarker = Nothing
    }
{-# INLINE mkDescribeReservedDBInstancesOfferingsMessage #-}

data DescribeReservedDBInstancesOfferings = DescribeReservedDBInstancesOfferings
    { _drdbiomReservedDBInstancesOfferingId :: Maybe Text
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
    , _drdbiomMultiAZ :: Maybe Bool
      -- ^ The Multi-AZ filter value. Specify this parameter to show only
      -- the available offerings matching the specified Multi-AZ
      -- parameter.
    , _drdbiomMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- than the MaxRecords value is available, a pagination token called
      -- a marker is included in the response so that the following
      -- results can be retrieved. Default: 100 Constraints: minimum 20,
      -- maximum 100.
    , _drdbiomMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If
      -- this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

-- | The offering identifier filter value. Specify this parameter to show only
-- the available offering that matches the specified reservation identifier.
-- Example: 438012d3-4052-4cc7-b2e3-8d3372e0e706.
drdbiomReservedDBInstancesOfferingId :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdbiomReservedDBInstancesOfferingId = lens _drdbiomReservedDBInstancesOfferingId (\s a -> s { _drdbiomReservedDBInstancesOfferingId = a })
{-# INLINE drdbiomReservedDBInstancesOfferingId #-}

-- | The DB instance class filter value. Specify this parameter to show only the
-- available offerings matching the specified DB instance class.
drdbiomDBInstanceClass :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdbiomDBInstanceClass = lens _drdbiomDBInstanceClass (\s a -> s { _drdbiomDBInstanceClass = a })
{-# INLINE drdbiomDBInstanceClass #-}

-- | Duration filter value, specified in years or seconds. Specify this
-- parameter to show only reservations for this duration. Valid Values: 1 | 3
-- | 31536000 | 94608000.
drdbiomDuration :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdbiomDuration = lens _drdbiomDuration (\s a -> s { _drdbiomDuration = a })
{-# INLINE drdbiomDuration #-}

-- | Product description filter value. Specify this parameter to show only the
-- available offerings matching the specified product description.
drdbiomProductDescription :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdbiomProductDescription = lens _drdbiomProductDescription (\s a -> s { _drdbiomProductDescription = a })
{-# INLINE drdbiomProductDescription #-}

-- | The offering type filter value. Specify this parameter to show only the
-- available offerings matching the specified offering type. Valid Values:
-- "Light Utilization" | "Medium Utilization" | "Heavy Utilization".
drdbiomOfferingType :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdbiomOfferingType = lens _drdbiomOfferingType (\s a -> s { _drdbiomOfferingType = a })
{-# INLINE drdbiomOfferingType #-}

-- | The Multi-AZ filter value. Specify this parameter to show only the
-- available offerings matching the specified Multi-AZ parameter.
drdbiomMultiAZ :: Lens' DescribeReservedDBInstancesOfferings (Maybe Bool)
drdbiomMultiAZ = lens _drdbiomMultiAZ (\s a -> s { _drdbiomMultiAZ = a })
{-# INLINE drdbiomMultiAZ #-}

-- | The maximum number of records to include in the response. If more than the
-- MaxRecords value is available, a pagination token called a marker is
-- included in the response so that the following results can be retrieved.
-- Default: 100 Constraints: minimum 20, maximum 100.
drdbiomMaxRecords :: Lens' DescribeReservedDBInstancesOfferings (Maybe Integer)
drdbiomMaxRecords = lens _drdbiomMaxRecords (\s a -> s { _drdbiomMaxRecords = a })
{-# INLINE drdbiomMaxRecords #-}

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
drdbiomMarker :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdbiomMarker = lens _drdbiomMarker (\s a -> s { _drdbiomMarker = a })
{-# INLINE drdbiomMarker #-}

instance ToQuery DescribeReservedDBInstancesOfferings where
    toQuery = genericQuery def

data DescribeReservedDBInstancesOfferingsResponse = DescribeReservedDBInstancesOfferingsResponse
    { _rdbiomMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If
      -- this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords.
    , _rdbiomReservedDBInstancesOfferings :: [ReservedDBInstancesOffering]
      -- ^ A list of reserved DB instance offerings.
    } deriving (Show, Generic)

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
rdbiomMarker :: Lens' DescribeReservedDBInstancesOfferingsResponse (Maybe Text)
rdbiomMarker = lens _rdbiomMarker (\s a -> s { _rdbiomMarker = a })
{-# INLINE rdbiomMarker #-}

-- | A list of reserved DB instance offerings.
rdbiomReservedDBInstancesOfferings :: Lens' DescribeReservedDBInstancesOfferingsResponse ([ReservedDBInstancesOffering])
rdbiomReservedDBInstancesOfferings = lens _rdbiomReservedDBInstancesOfferings (\s a -> s { _rdbiomReservedDBInstancesOfferings = a })
{-# INLINE rdbiomReservedDBInstancesOfferings #-}

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
