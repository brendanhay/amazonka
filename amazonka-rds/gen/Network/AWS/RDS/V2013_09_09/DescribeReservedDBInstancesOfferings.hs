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
    , mkDescribeReservedDBInstancesOfferings
    -- ** Request lenses
    , drdbioReservedDBInstancesOfferingId
    , drdbioDBInstanceClass
    , drdbioDuration
    , drdbioProductDescription
    , drdbioOfferingType
    , drdbioMultiAZ
    , drdbioMaxRecords
    , drdbioMarker

    -- * Response
    , DescribeReservedDBInstancesOfferingsResponse
    -- ** Response lenses
    , drdbiorMarker
    , drdbiorReservedDBInstancesOfferings
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | 
data DescribeReservedDBInstancesOfferings = DescribeReservedDBInstancesOfferings
    { _drdbioReservedDBInstancesOfferingId :: Maybe Text
    , _drdbioDBInstanceClass :: Maybe Text
    , _drdbioDuration :: Maybe Text
    , _drdbioProductDescription :: Maybe Text
    , _drdbioOfferingType :: Maybe Text
    , _drdbioMultiAZ :: Maybe Bool
    , _drdbioMaxRecords :: Maybe Integer
    , _drdbioMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeReservedDBInstancesOfferings' request.
mkDescribeReservedDBInstancesOfferings :: DescribeReservedDBInstancesOfferings
mkDescribeReservedDBInstancesOfferings = DescribeReservedDBInstancesOfferings
    { _drdbioReservedDBInstancesOfferingId = Nothing
    , _drdbioDBInstanceClass = Nothing
    , _drdbioDuration = Nothing
    , _drdbioProductDescription = Nothing
    , _drdbioOfferingType = Nothing
    , _drdbioMultiAZ = Nothing
    , _drdbioMaxRecords = Nothing
    , _drdbioMarker = Nothing
    }

-- | The offering identifier filter value. Specify this parameter to show only
-- the available offering that matches the specified reservation identifier.
-- Example: 438012d3-4052-4cc7-b2e3-8d3372e0e706.
drdbioReservedDBInstancesOfferingId :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdbioReservedDBInstancesOfferingId =
    lens _drdbioReservedDBInstancesOfferingId
         (\s a -> s { _drdbioReservedDBInstancesOfferingId = a })

-- | The DB instance class filter value. Specify this parameter to show only the
-- available offerings matching the specified DB instance class.
drdbioDBInstanceClass :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdbioDBInstanceClass =
    lens _drdbioDBInstanceClass (\s a -> s { _drdbioDBInstanceClass = a })

-- | Duration filter value, specified in years or seconds. Specify this
-- parameter to show only reservations for this duration. Valid Values: 1 | 3
-- | 31536000 | 94608000.
drdbioDuration :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdbioDuration = lens _drdbioDuration (\s a -> s { _drdbioDuration = a })

-- | Product description filter value. Specify this parameter to show only the
-- available offerings matching the specified product description.
drdbioProductDescription :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdbioProductDescription =
    lens _drdbioProductDescription
         (\s a -> s { _drdbioProductDescription = a })

-- | The offering type filter value. Specify this parameter to show only the
-- available offerings matching the specified offering type. Valid Values:
-- "Light Utilization" | "Medium Utilization" | "Heavy Utilization".
drdbioOfferingType :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdbioOfferingType =
    lens _drdbioOfferingType (\s a -> s { _drdbioOfferingType = a })

-- | The Multi-AZ filter value. Specify this parameter to show only the
-- available offerings matching the specified Multi-AZ parameter.
drdbioMultiAZ :: Lens' DescribeReservedDBInstancesOfferings (Maybe Bool)
drdbioMultiAZ = lens _drdbioMultiAZ (\s a -> s { _drdbioMultiAZ = a })

-- | The maximum number of records to include in the response. If more than the
-- MaxRecords value is available, a pagination token called a marker is
-- included in the response so that the following results can be retrieved.
-- Default: 100 Constraints: minimum 20, maximum 100.
drdbioMaxRecords :: Lens' DescribeReservedDBInstancesOfferings (Maybe Integer)
drdbioMaxRecords =
    lens _drdbioMaxRecords (\s a -> s { _drdbioMaxRecords = a })

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
drdbioMarker :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdbioMarker = lens _drdbioMarker (\s a -> s { _drdbioMarker = a })

instance ToQuery DescribeReservedDBInstancesOfferings where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the
-- DescribeReservedDBInstancesOfferings action.
data DescribeReservedDBInstancesOfferingsResponse = DescribeReservedDBInstancesOfferingsResponse
    { _drdbiorMarker :: Maybe Text
    , _drdbiorReservedDBInstancesOfferings :: [ReservedDBInstancesOffering]
    } deriving (Show, Generic)

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
drdbiorMarker :: Lens' DescribeReservedDBInstancesOfferingsResponse (Maybe Text)
drdbiorMarker = lens _drdbiorMarker (\s a -> s { _drdbiorMarker = a })

-- | A list of reserved DB instance offerings.
drdbiorReservedDBInstancesOfferings :: Lens' DescribeReservedDBInstancesOfferingsResponse [ReservedDBInstancesOffering]
drdbiorReservedDBInstancesOfferings =
    lens _drdbiorReservedDBInstancesOfferings
         (\s a -> s { _drdbiorReservedDBInstancesOfferings = a })

instance FromXML DescribeReservedDBInstancesOfferingsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeReservedDBInstancesOfferings where
    type Sv DescribeReservedDBInstancesOfferings = RDS
    type Rs DescribeReservedDBInstancesOfferings = DescribeReservedDBInstancesOfferingsResponse

    request = post "DescribeReservedDBInstancesOfferings"
    response _ = xmlResponse

instance AWSPager DescribeReservedDBInstancesOfferings where
    next rq rs = (\x -> rq & drdbioMarker ?~ x)
        <$> (rs ^. drdbiorMarker)
