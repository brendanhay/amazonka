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
      DescribeReservedDBInstancesMessage
    -- ** Request constructor
    , describeReservedDBInstancesMessage
    -- ** Request lenses
    , drdbimDBInstanceClass
    , drdbimDuration
    , drdbimFilters
    , drdbimMarker
    , drdbimMaxRecords
    , drdbimMultiAZ
    , drdbimOfferingType
    , drdbimProductDescription
    , drdbimReservedDBInstanceId
    , drdbimReservedDBInstancesOfferingId

    -- * Response
    , ReservedDBInstanceMessage
    -- ** Response constructor
    , reservedDBInstanceMessage
    -- ** Response lenses
    , rdbimMarker
    , rdbimReservedDBInstances
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data DescribeReservedDBInstancesMessage = DescribeReservedDBInstancesMessage
    { _drdbimDBInstanceClass               :: Maybe Text
    , _drdbimDuration                      :: Maybe Text
    , _drdbimFilters                       :: [Filter]
    , _drdbimMarker                        :: Maybe Text
    , _drdbimMaxRecords                    :: Maybe Int
    , _drdbimMultiAZ                       :: Maybe Bool
    , _drdbimOfferingType                  :: Maybe Text
    , _drdbimProductDescription            :: Maybe Text
    , _drdbimReservedDBInstanceId          :: Maybe Text
    , _drdbimReservedDBInstancesOfferingId :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeReservedDBInstancesMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drdbimDBInstanceClass' @::@ 'Maybe' 'Text'
--
-- * 'drdbimDuration' @::@ 'Maybe' 'Text'
--
-- * 'drdbimFilters' @::@ ['Filter']
--
-- * 'drdbimMarker' @::@ 'Maybe' 'Text'
--
-- * 'drdbimMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'drdbimMultiAZ' @::@ 'Maybe' 'Bool'
--
-- * 'drdbimOfferingType' @::@ 'Maybe' 'Text'
--
-- * 'drdbimProductDescription' @::@ 'Maybe' 'Text'
--
-- * 'drdbimReservedDBInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'drdbimReservedDBInstancesOfferingId' @::@ 'Maybe' 'Text'
--
describeReservedDBInstancesMessage :: DescribeReservedDBInstancesMessage
describeReservedDBInstancesMessage = DescribeReservedDBInstancesMessage
    { _drdbimReservedDBInstanceId          = Nothing
    , _drdbimReservedDBInstancesOfferingId = Nothing
    , _drdbimDBInstanceClass               = Nothing
    , _drdbimDuration                      = Nothing
    , _drdbimProductDescription            = Nothing
    , _drdbimOfferingType                  = Nothing
    , _drdbimMultiAZ                       = Nothing
    , _drdbimFilters                       = mempty
    , _drdbimMaxRecords                    = Nothing
    , _drdbimMarker                        = Nothing
    }

-- | The DB instance class filter value. Specify this parameter to show only
-- those reservations matching the specified DB instances class.
drdbimDBInstanceClass :: Lens' DescribeReservedDBInstancesMessage (Maybe Text)
drdbimDBInstanceClass =
    lens _drdbimDBInstanceClass (\s a -> s { _drdbimDBInstanceClass = a })

-- | The duration filter value, specified in years or seconds. Specify this
-- parameter to show only reservations for this duration. Valid Values: 1 |
-- 3 | 31536000 | 94608000.
drdbimDuration :: Lens' DescribeReservedDBInstancesMessage (Maybe Text)
drdbimDuration = lens _drdbimDuration (\s a -> s { _drdbimDuration = a })

-- | This parameter is not currently supported.
drdbimFilters :: Lens' DescribeReservedDBInstancesMessage [Filter]
drdbimFilters = lens _drdbimFilters (\s a -> s { _drdbimFilters = a })

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
drdbimMarker :: Lens' DescribeReservedDBInstancesMessage (Maybe Text)
drdbimMarker = lens _drdbimMarker (\s a -> s { _drdbimMarker = a })

-- | The maximum number of records to include in the response. If more than
-- the MaxRecords value is available, a pagination token called a marker is
-- included in the response so that the following results can be retrieved.
-- Default: 100 Constraints: minimum 20, maximum 100.
drdbimMaxRecords :: Lens' DescribeReservedDBInstancesMessage (Maybe Int)
drdbimMaxRecords = lens _drdbimMaxRecords (\s a -> s { _drdbimMaxRecords = a })

-- | The Multi-AZ filter value. Specify this parameter to show only those
-- reservations matching the specified Multi-AZ parameter.
drdbimMultiAZ :: Lens' DescribeReservedDBInstancesMessage (Maybe Bool)
drdbimMultiAZ = lens _drdbimMultiAZ (\s a -> s { _drdbimMultiAZ = a })

-- | The offering type filter value. Specify this parameter to show only the
-- available offerings matching the specified offering type. Valid Values:
-- "Light Utilization" | "Medium Utilization" | "Heavy Utilization".
drdbimOfferingType :: Lens' DescribeReservedDBInstancesMessage (Maybe Text)
drdbimOfferingType =
    lens _drdbimOfferingType (\s a -> s { _drdbimOfferingType = a })

-- | The product description filter value. Specify this parameter to show only
-- those reservations matching the specified product description.
drdbimProductDescription :: Lens' DescribeReservedDBInstancesMessage (Maybe Text)
drdbimProductDescription =
    lens _drdbimProductDescription
        (\s a -> s { _drdbimProductDescription = a })

-- | The reserved DB instance identifier filter value. Specify this parameter
-- to show only the reservation that matches the specified reservation ID.
drdbimReservedDBInstanceId :: Lens' DescribeReservedDBInstancesMessage (Maybe Text)
drdbimReservedDBInstanceId =
    lens _drdbimReservedDBInstanceId
        (\s a -> s { _drdbimReservedDBInstanceId = a })

-- | The offering identifier filter value. Specify this parameter to show only
-- purchased reservations matching the specified offering identifier.
drdbimReservedDBInstancesOfferingId :: Lens' DescribeReservedDBInstancesMessage (Maybe Text)
drdbimReservedDBInstancesOfferingId =
    lens _drdbimReservedDBInstancesOfferingId
        (\s a -> s { _drdbimReservedDBInstancesOfferingId = a })

instance ToQuery DescribeReservedDBInstancesMessage

instance ToPath DescribeReservedDBInstancesMessage where
    toPath = const "/"

data ReservedDBInstanceMessage = ReservedDBInstanceMessage
    { _rdbimMarker              :: Maybe Text
    , _rdbimReservedDBInstances :: [ReservedDBInstance]
    } deriving (Eq, Show, Generic)

-- | 'ReservedDBInstanceMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdbimMarker' @::@ 'Maybe' 'Text'
--
-- * 'rdbimReservedDBInstances' @::@ ['ReservedDBInstance']
--
reservedDBInstanceMessage :: ReservedDBInstanceMessage
reservedDBInstanceMessage = ReservedDBInstanceMessage
    { _rdbimMarker              = Nothing
    , _rdbimReservedDBInstances = mempty
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
rdbimMarker :: Lens' ReservedDBInstanceMessage (Maybe Text)
rdbimMarker = lens _rdbimMarker (\s a -> s { _rdbimMarker = a })

-- | A list of reserved DB instances.
rdbimReservedDBInstances :: Lens' ReservedDBInstanceMessage [ReservedDBInstance]
rdbimReservedDBInstances =
    lens _rdbimReservedDBInstances
        (\s a -> s { _rdbimReservedDBInstances = a })

instance FromXML ReservedDBInstanceMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedDBInstanceMessage"

instance AWSRequest DescribeReservedDBInstancesMessage where
    type Sv DescribeReservedDBInstancesMessage = RDS
    type Rs DescribeReservedDBInstancesMessage = ReservedDBInstanceMessage

    request  = post "DescribeReservedDBInstances"
    response = xmlResponse $ \h x -> ReservedDBInstanceMessage
        <$> x %| "Marker"
        <*> x %| "ReservedDBInstances"
