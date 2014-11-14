{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.RDS.DescribeReservedDBInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about reserved DB instances for this account, or about
-- a specified reserved DB instance.
module Network.AWS.RDS.DescribeReservedDBInstances
    (
    -- * Request
      DescribeReservedDBInstances
    -- ** Request constructor
    , describeReservedDBInstances
    -- ** Request lenses
    , drdbiDBInstanceClass
    , drdbiDuration
    , drdbiFilters
    , drdbiMarker
    , drdbiMaxRecords
    , drdbiMultiAZ
    , drdbiOfferingType
    , drdbiProductDescription
    , drdbiReservedDBInstanceId
    , drdbiReservedDBInstancesOfferingId

    -- * Response
    , DescribeReservedDBInstancesResponse
    -- ** Response constructor
    , describeReservedDBInstancesResponse
    -- ** Response lenses
    , drdbirMarker
    , drdbirReservedDBInstances
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data DescribeReservedDBInstances = DescribeReservedDBInstances
    { _drdbiDBInstanceClass               :: Maybe Text
    , _drdbiDuration                      :: Maybe Text
    , _drdbiFilters                       :: [Filter]
    , _drdbiMarker                        :: Maybe Text
    , _drdbiMaxRecords                    :: Maybe Int
    , _drdbiMultiAZ                       :: Maybe Bool
    , _drdbiOfferingType                  :: Maybe Text
    , _drdbiProductDescription            :: Maybe Text
    , _drdbiReservedDBInstanceId          :: Maybe Text
    , _drdbiReservedDBInstancesOfferingId :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeReservedDBInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drdbiDBInstanceClass' @::@ 'Maybe' 'Text'
--
-- * 'drdbiDuration' @::@ 'Maybe' 'Text'
--
-- * 'drdbiFilters' @::@ ['Filter']
--
-- * 'drdbiMarker' @::@ 'Maybe' 'Text'
--
-- * 'drdbiMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'drdbiMultiAZ' @::@ 'Maybe' 'Bool'
--
-- * 'drdbiOfferingType' @::@ 'Maybe' 'Text'
--
-- * 'drdbiProductDescription' @::@ 'Maybe' 'Text'
--
-- * 'drdbiReservedDBInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'drdbiReservedDBInstancesOfferingId' @::@ 'Maybe' 'Text'
--
describeReservedDBInstances :: DescribeReservedDBInstances
describeReservedDBInstances = DescribeReservedDBInstances
    { _drdbiReservedDBInstanceId          = Nothing
    , _drdbiReservedDBInstancesOfferingId = Nothing
    , _drdbiDBInstanceClass               = Nothing
    , _drdbiDuration                      = Nothing
    , _drdbiProductDescription            = Nothing
    , _drdbiOfferingType                  = Nothing
    , _drdbiMultiAZ                       = Nothing
    , _drdbiFilters                       = mempty
    , _drdbiMaxRecords                    = Nothing
    , _drdbiMarker                        = Nothing
    }

-- | The DB instance class filter value. Specify this parameter to show only
-- those reservations matching the specified DB instances class.
drdbiDBInstanceClass :: Lens' DescribeReservedDBInstances (Maybe Text)
drdbiDBInstanceClass =
    lens _drdbiDBInstanceClass (\s a -> s { _drdbiDBInstanceClass = a })

-- | The duration filter value, specified in years or seconds. Specify this
-- parameter to show only reservations for this duration. Valid Values: 1 |
-- 3 | 31536000 | 94608000.
drdbiDuration :: Lens' DescribeReservedDBInstances (Maybe Text)
drdbiDuration = lens _drdbiDuration (\s a -> s { _drdbiDuration = a })

-- | This parameter is not currently supported.
drdbiFilters :: Lens' DescribeReservedDBInstances [Filter]
drdbiFilters = lens _drdbiFilters (\s a -> s { _drdbiFilters = a })

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
drdbiMarker :: Lens' DescribeReservedDBInstances (Maybe Text)
drdbiMarker = lens _drdbiMarker (\s a -> s { _drdbiMarker = a })

-- | The maximum number of records to include in the response. If more than
-- the MaxRecords value is available, a pagination token called a marker is
-- included in the response so that the following results can be retrieved.
-- Default: 100 Constraints: minimum 20, maximum 100.
drdbiMaxRecords :: Lens' DescribeReservedDBInstances (Maybe Int)
drdbiMaxRecords = lens _drdbiMaxRecords (\s a -> s { _drdbiMaxRecords = a })

-- | The Multi-AZ filter value. Specify this parameter to show only those
-- reservations matching the specified Multi-AZ parameter.
drdbiMultiAZ :: Lens' DescribeReservedDBInstances (Maybe Bool)
drdbiMultiAZ = lens _drdbiMultiAZ (\s a -> s { _drdbiMultiAZ = a })

-- | The offering type filter value. Specify this parameter to show only the
-- available offerings matching the specified offering type. Valid Values:
-- "Light Utilization" | "Medium Utilization" | "Heavy Utilization".
drdbiOfferingType :: Lens' DescribeReservedDBInstances (Maybe Text)
drdbiOfferingType =
    lens _drdbiOfferingType (\s a -> s { _drdbiOfferingType = a })

-- | The product description filter value. Specify this parameter to show only
-- those reservations matching the specified product description.
drdbiProductDescription :: Lens' DescribeReservedDBInstances (Maybe Text)
drdbiProductDescription =
    lens _drdbiProductDescription (\s a -> s { _drdbiProductDescription = a })

-- | The reserved DB instance identifier filter value. Specify this parameter
-- to show only the reservation that matches the specified reservation ID.
drdbiReservedDBInstanceId :: Lens' DescribeReservedDBInstances (Maybe Text)
drdbiReservedDBInstanceId =
    lens _drdbiReservedDBInstanceId
        (\s a -> s { _drdbiReservedDBInstanceId = a })

-- | The offering identifier filter value. Specify this parameter to show only
-- purchased reservations matching the specified offering identifier.
drdbiReservedDBInstancesOfferingId :: Lens' DescribeReservedDBInstances (Maybe Text)
drdbiReservedDBInstancesOfferingId =
    lens _drdbiReservedDBInstancesOfferingId
        (\s a -> s { _drdbiReservedDBInstancesOfferingId = a })

instance ToQuery DescribeReservedDBInstances

instance ToPath DescribeReservedDBInstances where
    toPath = const "/"

data DescribeReservedDBInstancesResponse = DescribeReservedDBInstancesResponse
    { _drdbirMarker              :: Maybe Text
    , _drdbirReservedDBInstances :: [ReservedDBInstance]
    } deriving (Eq, Show, Generic)

-- | 'DescribeReservedDBInstancesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drdbirMarker' @::@ 'Maybe' 'Text'
--
-- * 'drdbirReservedDBInstances' @::@ ['ReservedDBInstance']
--
describeReservedDBInstancesResponse :: DescribeReservedDBInstancesResponse
describeReservedDBInstancesResponse = DescribeReservedDBInstancesResponse
    { _drdbirMarker              = Nothing
    , _drdbirReservedDBInstances = mempty
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
drdbirMarker :: Lens' DescribeReservedDBInstancesResponse (Maybe Text)
drdbirMarker = lens _drdbirMarker (\s a -> s { _drdbirMarker = a })

-- | A list of reserved DB instances.
drdbirReservedDBInstances :: Lens' DescribeReservedDBInstancesResponse [ReservedDBInstance]
drdbirReservedDBInstances =
    lens _drdbirReservedDBInstances
        (\s a -> s { _drdbirReservedDBInstances = a })

instance AWSRequest DescribeReservedDBInstances where
    type Sv DescribeReservedDBInstances = RDS
    type Rs DescribeReservedDBInstances = DescribeReservedDBInstancesResponse

    request  = post "DescribeReservedDBInstances"
    response = xmlResponse $ \h x -> DescribeReservedDBInstancesResponse
        <$> x %| "Marker"
        <*> x %| "ReservedDBInstances"
