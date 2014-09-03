{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.RDS.V2013_09_09.DescribeReservedDBInstances
    (
    -- * Request
      DescribeReservedDBInstances
    -- ** Request constructor
    , describeReservedDBInstances
    -- ** Request lenses
    , drdbimMultiAZ
    , drdbimMaxRecords
    , drdbimReservedDBInstanceId
    , drdbimReservedDBInstancesOfferingId
    , drdbimDBInstanceClass
    , drdbimDuration
    , drdbimProductDescription
    , drdbimOfferingType
    , drdbimMarker

    -- * Response
    , DescribeReservedDBInstancesResponse
    -- ** Response lenses
    , rdbimReservedDBInstances
    , rdbimMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeReservedDBInstances' request.
describeReservedDBInstances :: DescribeReservedDBInstances
describeReservedDBInstances = DescribeReservedDBInstances
    { _drdbimMultiAZ = Nothing
    , _drdbimMaxRecords = Nothing
    , _drdbimReservedDBInstanceId = Nothing
    , _drdbimReservedDBInstancesOfferingId = Nothing
    , _drdbimDBInstanceClass = Nothing
    , _drdbimDuration = Nothing
    , _drdbimProductDescription = Nothing
    , _drdbimOfferingType = Nothing
    , _drdbimMarker = Nothing
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
    , _drdbimReservedDBInstanceId :: Maybe Text
      -- ^ The reserved DB instance identifier filter value. Specify this
      -- parameter to show only the reservation that matches the specified
      -- reservation ID.
    , _drdbimReservedDBInstancesOfferingId :: Maybe Text
      -- ^ The offering identifier filter value. Specify this parameter to
      -- show only purchased reservations matching the specified offering
      -- identifier.
    , _drdbimDBInstanceClass :: Maybe Text
      -- ^ The DB instance class filter value. Specify this parameter to
      -- show only those reservations matching the specified DB instances
      -- class.
    , _drdbimDuration :: Maybe Text
      -- ^ The duration filter value, specified in years or seconds. Specify
      -- this parameter to show only reservations for this duration. Valid
      -- Values: 1 | 3 | 31536000 | 94608000.
    , _drdbimProductDescription :: Maybe Text
      -- ^ The product description filter value. Specify this parameter to
      -- show only those reservations matching the specified product
      -- description.
    , _drdbimOfferingType :: Maybe Text
      -- ^ The offering type filter value. Specify this parameter to show
      -- only the available offerings matching the specified offering
      -- type. Valid Values: "Light Utilization" | "Medium Utilization" |
      -- "Heavy Utilization".
    , _drdbimMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If
      -- this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

-- | The Multi-AZ filter value. Specify this parameter to show only those
-- reservations matching the specified Multi-AZ parameter.
drdbimMultiAZ
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DescribeReservedDBInstances
    -> f DescribeReservedDBInstances
drdbimMultiAZ f x =
    (\y -> x { _drdbimMultiAZ = y })
       <$> f (_drdbimMultiAZ x)
{-# INLINE drdbimMultiAZ #-}

-- | The maximum number of records to include in the response. If more than the
-- MaxRecords value is available, a pagination token called a marker is
-- included in the response so that the following results can be retrieved.
-- Default: 100 Constraints: minimum 20, maximum 100.
drdbimMaxRecords
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeReservedDBInstances
    -> f DescribeReservedDBInstances
drdbimMaxRecords f x =
    (\y -> x { _drdbimMaxRecords = y })
       <$> f (_drdbimMaxRecords x)
{-# INLINE drdbimMaxRecords #-}

-- | The reserved DB instance identifier filter value. Specify this parameter to
-- show only the reservation that matches the specified reservation ID.
drdbimReservedDBInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeReservedDBInstances
    -> f DescribeReservedDBInstances
drdbimReservedDBInstanceId f x =
    (\y -> x { _drdbimReservedDBInstanceId = y })
       <$> f (_drdbimReservedDBInstanceId x)
{-# INLINE drdbimReservedDBInstanceId #-}

-- | The offering identifier filter value. Specify this parameter to show only
-- purchased reservations matching the specified offering identifier.
drdbimReservedDBInstancesOfferingId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeReservedDBInstances
    -> f DescribeReservedDBInstances
drdbimReservedDBInstancesOfferingId f x =
    (\y -> x { _drdbimReservedDBInstancesOfferingId = y })
       <$> f (_drdbimReservedDBInstancesOfferingId x)
{-# INLINE drdbimReservedDBInstancesOfferingId #-}

-- | The DB instance class filter value. Specify this parameter to show only
-- those reservations matching the specified DB instances class.
drdbimDBInstanceClass
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeReservedDBInstances
    -> f DescribeReservedDBInstances
drdbimDBInstanceClass f x =
    (\y -> x { _drdbimDBInstanceClass = y })
       <$> f (_drdbimDBInstanceClass x)
{-# INLINE drdbimDBInstanceClass #-}

-- | The duration filter value, specified in years or seconds. Specify this
-- parameter to show only reservations for this duration. Valid Values: 1 | 3
-- | 31536000 | 94608000.
drdbimDuration
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeReservedDBInstances
    -> f DescribeReservedDBInstances
drdbimDuration f x =
    (\y -> x { _drdbimDuration = y })
       <$> f (_drdbimDuration x)
{-# INLINE drdbimDuration #-}

-- | The product description filter value. Specify this parameter to show only
-- those reservations matching the specified product description.
drdbimProductDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeReservedDBInstances
    -> f DescribeReservedDBInstances
drdbimProductDescription f x =
    (\y -> x { _drdbimProductDescription = y })
       <$> f (_drdbimProductDescription x)
{-# INLINE drdbimProductDescription #-}

-- | The offering type filter value. Specify this parameter to show only the
-- available offerings matching the specified offering type. Valid Values:
-- "Light Utilization" | "Medium Utilization" | "Heavy Utilization".
drdbimOfferingType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeReservedDBInstances
    -> f DescribeReservedDBInstances
drdbimOfferingType f x =
    (\y -> x { _drdbimOfferingType = y })
       <$> f (_drdbimOfferingType x)
{-# INLINE drdbimOfferingType #-}

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
drdbimMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeReservedDBInstances
    -> f DescribeReservedDBInstances
drdbimMarker f x =
    (\y -> x { _drdbimMarker = y })
       <$> f (_drdbimMarker x)
{-# INLINE drdbimMarker #-}

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

-- | A list of reserved DB instances.
rdbimReservedDBInstances
    :: Functor f
    => ([ReservedDBInstance]
    -> f ([ReservedDBInstance]))
    -> DescribeReservedDBInstancesResponse
    -> f DescribeReservedDBInstancesResponse
rdbimReservedDBInstances f x =
    (\y -> x { _rdbimReservedDBInstances = y })
       <$> f (_rdbimReservedDBInstances x)
{-# INLINE rdbimReservedDBInstances #-}

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
rdbimMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeReservedDBInstancesResponse
    -> f DescribeReservedDBInstancesResponse
rdbimMarker f x =
    (\y -> x { _rdbimMarker = y })
       <$> f (_rdbimMarker x)
{-# INLINE rdbimMarker #-}

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
