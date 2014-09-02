{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.RDS.V2013_09_09.DescribeReservedDBInstancesOfferings where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeReservedDBInstancesOfferings' request.
describeReservedDBInstancesOfferings :: DescribeReservedDBInstancesOfferings
describeReservedDBInstancesOfferings = DescribeReservedDBInstancesOfferings
    { _drdbiomMultiAZ = Nothing
    , _drdbiomMaxRecords = Nothing
    , _drdbiomDBInstanceClass = Nothing
    , _drdbiomDuration = Nothing
    , _drdbiomMarker = Nothing
    , _drdbiomOfferingType = Nothing
    , _drdbiomProductDescription = Nothing
    , _drdbiomReservedDBInstancesOfferingId = Nothing
    }

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
    , _drdbiomDBInstanceClass :: Maybe Text
      -- ^ The DB instance class filter value. Specify this parameter to
      -- show only the available offerings matching the specified DB
      -- instance class.
    , _drdbiomDuration :: Maybe Text
      -- ^ Duration filter value, specified in years or seconds. Specify
      -- this parameter to show only reservations for this duration. Valid
      -- Values: 1 | 3 | 31536000 | 94608000.
    , _drdbiomMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If
      -- this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords.
    , _drdbiomOfferingType :: Maybe Text
      -- ^ The offering type filter value. Specify this parameter to show
      -- only the available offerings matching the specified offering
      -- type. Valid Values: "Light Utilization" | "Medium Utilization" |
      -- "Heavy Utilization".
    , _drdbiomProductDescription :: Maybe Text
      -- ^ Product description filter value. Specify this parameter to show
      -- only the available offerings matching the specified product
      -- description.
    , _drdbiomReservedDBInstancesOfferingId :: Maybe Text
      -- ^ The offering identifier filter value. Specify this parameter to
      -- show only the available offering that matches the specified
      -- reservation identifier. Example:
      -- 438012d3-4052-4cc7-b2e3-8d3372e0e706.
    } deriving (Show, Generic)

makeLenses ''DescribeReservedDBInstancesOfferings

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

makeLenses ''DescribeReservedDBInstancesOfferingsResponse

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
