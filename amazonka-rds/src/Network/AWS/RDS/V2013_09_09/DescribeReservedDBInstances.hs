{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.RDS.V2013_09_09.DescribeReservedDBInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about reserved DB instances for this account, or about
-- a specified reserved DB instance. https://rds.amazonaws.com/
-- ?Action=DescribeReservedDBInstances
-- &ReservedDBInstanceId=customerSpecifiedID &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2012-12-18T18%3A31%3A36.118Z
-- &AWSAccessKeyId= &Signature= Medium Utilization USD mysql
-- 649fd0c8-cf6d-47a0-bfa6-060f8e75e95f false active myreservationid 1
-- 2010-12-15T00:25:14.131Z 31536000 227.5 0.046 db.m1.small
-- c695119b-2961-11e1-bd06-6fe008f046c3.
module Network.AWS.RDS.V2013_09_09.DescribeReservedDBInstances where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeReservedDBInstances' request.
describeReservedDBInstances :: DescribeReservedDBInstances
describeReservedDBInstances = DescribeReservedDBInstances
    { _drdbimMultiAZ = Nothing
    , _drdbimMaxRecords = Nothing
    , _drdbimProductDescription = Nothing
    , _drdbimReservedDBInstanceId = Nothing
    , _drdbimDBInstanceClass = Nothing
    , _drdbimMarker = Nothing
    , _drdbimReservedDBInstancesOfferingId = Nothing
    , _drdbimOfferingType = Nothing
    , _drdbimDuration = Nothing
    }

data DescribeReservedDBInstances = DescribeReservedDBInstances
    { _drdbimMultiAZ :: Maybe Bool
      -- ^ The Multi-AZ filter value. Specify this parameter to show only
      -- those reservations matching the specified Multi-AZ parameter.
    , _drdbimMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- than the MaxRecords value is available, a pagination token called
      -- a marker is included in the response so that the following
      -- results can be retrieved. Default: 100 Constraints: minimum 20,
      -- maximum 100.
    , _drdbimProductDescription :: Maybe Text
      -- ^ The product description filter value. Specify this parameter to
      -- show only those reservations matching the specified product
      -- description.
    , _drdbimReservedDBInstanceId :: Maybe Text
      -- ^ The reserved DB instance identifier filter value. Specify this
      -- parameter to show only the reservation that matches the specified
      -- reservation ID.
    , _drdbimDBInstanceClass :: Maybe Text
      -- ^ The DB instance class filter value. Specify this parameter to
      -- show only those reservations matching the specified DB instances
      -- class.
    , _drdbimMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If
      -- this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords.
    , _drdbimReservedDBInstancesOfferingId :: Maybe Text
      -- ^ The offering identifier filter value. Specify this parameter to
      -- show only purchased reservations matching the specified offering
      -- identifier.
    , _drdbimOfferingType :: Maybe Text
      -- ^ The offering type filter value. Specify this parameter to show
      -- only the available offerings matching the specified offering
      -- type. Valid Values: "Light Utilization" | "Medium Utilization" |
      -- "Heavy Utilization".
    , _drdbimDuration :: Maybe Text
      -- ^ The duration filter value, specified in years or seconds. Specify
      -- this parameter to show only reservations for this duration. Valid
      -- Values: 1 | 3 | 31536000 | 94608000.
    } deriving (Show, Generic)

makeLenses ''DescribeReservedDBInstances

instance ToQuery DescribeReservedDBInstances where
    toQuery = genericQuery def

data DescribeReservedDBInstancesResponse = DescribeReservedDBInstancesResponse
    { _rdbimReservedDBInstances :: [ReservedDBInstance]
      -- ^ A list of reserved DB instances.
    , _rdbimMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If
      -- this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

makeLenses ''DescribeReservedDBInstancesResponse

instance FromXML DescribeReservedDBInstancesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeReservedDBInstances where
    type Sv DescribeReservedDBInstances = RDS
    type Rs DescribeReservedDBInstances = DescribeReservedDBInstancesResponse

    request = post "DescribeReservedDBInstances"
    response _ = xmlResponse

instance AWSPager DescribeReservedDBInstances where
    next rq rs = (\x -> rq { _drdbimMarker = Just x })
        <$> (_rdbimMarker rs)
