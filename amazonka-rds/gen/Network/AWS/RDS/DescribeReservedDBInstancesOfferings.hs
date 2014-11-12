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

-- Module      : Network.AWS.RDS.DescribeReservedDBInstancesOfferings
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists available reserved DB instance offerings.
module Network.AWS.RDS.DescribeReservedDBInstancesOfferings
    (
    -- * Request
      DescribeReservedDBInstancesOfferingsMessage
    -- ** Request constructor
    , describeReservedDBInstancesOfferingsMessage
    -- ** Request lenses
    , drdbiomDBInstanceClass
    , drdbiomDuration
    , drdbiomFilters
    , drdbiomMarker
    , drdbiomMaxRecords
    , drdbiomMultiAZ
    , drdbiomOfferingType
    , drdbiomProductDescription
    , drdbiomReservedDBInstancesOfferingId

    -- * Response
    , ReservedDBInstancesOfferingMessage
    -- ** Response constructor
    , reservedDBInstancesOfferingMessage
    -- ** Response lenses
    , rdbiomMarker
    , rdbiomReservedDBInstancesOfferings
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data DescribeReservedDBInstancesOfferingsMessage = DescribeReservedDBInstancesOfferingsMessage
    { _drdbiomDBInstanceClass               :: Maybe Text
    , _drdbiomDuration                      :: Maybe Text
    , _drdbiomFilters                       :: [Filter]
    , _drdbiomMarker                        :: Maybe Text
    , _drdbiomMaxRecords                    :: Maybe Int
    , _drdbiomMultiAZ                       :: Maybe Bool
    , _drdbiomOfferingType                  :: Maybe Text
    , _drdbiomProductDescription            :: Maybe Text
    , _drdbiomReservedDBInstancesOfferingId :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeReservedDBInstancesOfferingsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drdbiomDBInstanceClass' @::@ 'Maybe' 'Text'
--
-- * 'drdbiomDuration' @::@ 'Maybe' 'Text'
--
-- * 'drdbiomFilters' @::@ ['Filter']
--
-- * 'drdbiomMarker' @::@ 'Maybe' 'Text'
--
-- * 'drdbiomMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'drdbiomMultiAZ' @::@ 'Maybe' 'Bool'
--
-- * 'drdbiomOfferingType' @::@ 'Maybe' 'Text'
--
-- * 'drdbiomProductDescription' @::@ 'Maybe' 'Text'
--
-- * 'drdbiomReservedDBInstancesOfferingId' @::@ 'Maybe' 'Text'
--
describeReservedDBInstancesOfferingsMessage :: DescribeReservedDBInstancesOfferingsMessage
describeReservedDBInstancesOfferingsMessage = DescribeReservedDBInstancesOfferingsMessage
    { _drdbiomReservedDBInstancesOfferingId = Nothing
    , _drdbiomDBInstanceClass               = Nothing
    , _drdbiomDuration                      = Nothing
    , _drdbiomProductDescription            = Nothing
    , _drdbiomOfferingType                  = Nothing
    , _drdbiomMultiAZ                       = Nothing
    , _drdbiomFilters                       = mempty
    , _drdbiomMaxRecords                    = Nothing
    , _drdbiomMarker                        = Nothing
    }

-- | The DB instance class filter value. Specify this parameter to show only
-- the available offerings matching the specified DB instance class.
drdbiomDBInstanceClass :: Lens' DescribeReservedDBInstancesOfferingsMessage (Maybe Text)
drdbiomDBInstanceClass =
    lens _drdbiomDBInstanceClass (\s a -> s { _drdbiomDBInstanceClass = a })

-- | Duration filter value, specified in years or seconds. Specify this
-- parameter to show only reservations for this duration. Valid Values: 1 |
-- 3 | 31536000 | 94608000.
drdbiomDuration :: Lens' DescribeReservedDBInstancesOfferingsMessage (Maybe Text)
drdbiomDuration = lens _drdbiomDuration (\s a -> s { _drdbiomDuration = a })

-- | This parameter is not currently supported.
drdbiomFilters :: Lens' DescribeReservedDBInstancesOfferingsMessage [Filter]
drdbiomFilters = lens _drdbiomFilters (\s a -> s { _drdbiomFilters = a })

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
drdbiomMarker :: Lens' DescribeReservedDBInstancesOfferingsMessage (Maybe Text)
drdbiomMarker = lens _drdbiomMarker (\s a -> s { _drdbiomMarker = a })

-- | The maximum number of records to include in the response. If more than
-- the MaxRecords value is available, a pagination token called a marker is
-- included in the response so that the following results can be retrieved.
-- Default: 100 Constraints: minimum 20, maximum 100.
drdbiomMaxRecords :: Lens' DescribeReservedDBInstancesOfferingsMessage (Maybe Int)
drdbiomMaxRecords =
    lens _drdbiomMaxRecords (\s a -> s { _drdbiomMaxRecords = a })

-- | The Multi-AZ filter value. Specify this parameter to show only the
-- available offerings matching the specified Multi-AZ parameter.
drdbiomMultiAZ :: Lens' DescribeReservedDBInstancesOfferingsMessage (Maybe Bool)
drdbiomMultiAZ = lens _drdbiomMultiAZ (\s a -> s { _drdbiomMultiAZ = a })

-- | The offering type filter value. Specify this parameter to show only the
-- available offerings matching the specified offering type. Valid Values:
-- "Light Utilization" | "Medium Utilization" | "Heavy Utilization".
drdbiomOfferingType :: Lens' DescribeReservedDBInstancesOfferingsMessage (Maybe Text)
drdbiomOfferingType =
    lens _drdbiomOfferingType (\s a -> s { _drdbiomOfferingType = a })

-- | Product description filter value. Specify this parameter to show only the
-- available offerings matching the specified product description.
drdbiomProductDescription :: Lens' DescribeReservedDBInstancesOfferingsMessage (Maybe Text)
drdbiomProductDescription =
    lens _drdbiomProductDescription
        (\s a -> s { _drdbiomProductDescription = a })

-- | The offering identifier filter value. Specify this parameter to show only
-- the available offering that matches the specified reservation identifier.
-- Example: 438012d3-4052-4cc7-b2e3-8d3372e0e706.
drdbiomReservedDBInstancesOfferingId :: Lens' DescribeReservedDBInstancesOfferingsMessage (Maybe Text)
drdbiomReservedDBInstancesOfferingId =
    lens _drdbiomReservedDBInstancesOfferingId
        (\s a -> s { _drdbiomReservedDBInstancesOfferingId = a })

instance ToQuery DescribeReservedDBInstancesOfferingsMessage

instance ToPath DescribeReservedDBInstancesOfferingsMessage where
    toPath = const "/"

data ReservedDBInstancesOfferingMessage = ReservedDBInstancesOfferingMessage
    { _rdbiomMarker                       :: Maybe Text
    , _rdbiomReservedDBInstancesOfferings :: [ReservedDBInstancesOffering]
    } deriving (Eq, Show, Generic)

-- | 'ReservedDBInstancesOfferingMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdbiomMarker' @::@ 'Maybe' 'Text'
--
-- * 'rdbiomReservedDBInstancesOfferings' @::@ ['ReservedDBInstancesOffering']
--
reservedDBInstancesOfferingMessage :: ReservedDBInstancesOfferingMessage
reservedDBInstancesOfferingMessage = ReservedDBInstancesOfferingMessage
    { _rdbiomMarker                       = Nothing
    , _rdbiomReservedDBInstancesOfferings = mempty
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
rdbiomMarker :: Lens' ReservedDBInstancesOfferingMessage (Maybe Text)
rdbiomMarker = lens _rdbiomMarker (\s a -> s { _rdbiomMarker = a })

-- | A list of reserved DB instance offerings.
rdbiomReservedDBInstancesOfferings :: Lens' ReservedDBInstancesOfferingMessage [ReservedDBInstancesOffering]
rdbiomReservedDBInstancesOfferings =
    lens _rdbiomReservedDBInstancesOfferings
        (\s a -> s { _rdbiomReservedDBInstancesOfferings = a })

instance FromXML ReservedDBInstancesOfferingMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedDBInstancesOfferingMessage"

instance AWSRequest DescribeReservedDBInstancesOfferingsMessage where
    type Sv DescribeReservedDBInstancesOfferingsMessage = RDS
    type Rs DescribeReservedDBInstancesOfferingsMessage = ReservedDBInstancesOfferingMessage

    request  = post "DescribeReservedDBInstancesOfferings"
    response = xmlResponse $ \h x -> ReservedDBInstancesOfferingMessage
        <$> x %| "Marker"
        <*> x %| "ReservedDBInstancesOfferings"
