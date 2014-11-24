{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeReservedDBInstancesOfferings.html>
module Network.AWS.RDS.DescribeReservedDBInstancesOfferings
    (
    -- * Request
      DescribeReservedDBInstancesOfferings
    -- ** Request constructor
    , describeReservedDBInstancesOfferings
    -- ** Request lenses
    , drdbioDBInstanceClass
    , drdbioDuration
    , drdbioFilters
    , drdbioMarker
    , drdbioMaxRecords
    , drdbioMultiAZ
    , drdbioOfferingType
    , drdbioProductDescription
    , drdbioReservedDBInstancesOfferingId

    -- * Response
    , DescribeReservedDBInstancesOfferingsResponse
    -- ** Response constructor
    , describeReservedDBInstancesOfferingsResponse
    -- ** Response lenses
    , drdbiorMarker
    , drdbiorReservedDBInstancesOfferings
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data DescribeReservedDBInstancesOfferings = DescribeReservedDBInstancesOfferings
    { _drdbioDBInstanceClass               :: Maybe Text
    , _drdbioDuration                      :: Maybe Text
    , _drdbioFilters                       :: List "Filter" Filter
    , _drdbioMarker                        :: Maybe Text
    , _drdbioMaxRecords                    :: Maybe Int
    , _drdbioMultiAZ                       :: Maybe Bool
    , _drdbioOfferingType                  :: Maybe Text
    , _drdbioProductDescription            :: Maybe Text
    , _drdbioReservedDBInstancesOfferingId :: Maybe Text
    } deriving (Eq, Show)

-- | 'DescribeReservedDBInstancesOfferings' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drdbioDBInstanceClass' @::@ 'Maybe' 'Text'
--
-- * 'drdbioDuration' @::@ 'Maybe' 'Text'
--
-- * 'drdbioFilters' @::@ ['Filter']
--
-- * 'drdbioMarker' @::@ 'Maybe' 'Text'
--
-- * 'drdbioMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'drdbioMultiAZ' @::@ 'Maybe' 'Bool'
--
-- * 'drdbioOfferingType' @::@ 'Maybe' 'Text'
--
-- * 'drdbioProductDescription' @::@ 'Maybe' 'Text'
--
-- * 'drdbioReservedDBInstancesOfferingId' @::@ 'Maybe' 'Text'
--
describeReservedDBInstancesOfferings :: DescribeReservedDBInstancesOfferings
describeReservedDBInstancesOfferings = DescribeReservedDBInstancesOfferings
    { _drdbioReservedDBInstancesOfferingId = Nothing
    , _drdbioDBInstanceClass               = Nothing
    , _drdbioDuration                      = Nothing
    , _drdbioProductDescription            = Nothing
    , _drdbioOfferingType                  = Nothing
    , _drdbioMultiAZ                       = Nothing
    , _drdbioFilters                       = mempty
    , _drdbioMaxRecords                    = Nothing
    , _drdbioMarker                        = Nothing
    }

-- | The DB instance class filter value. Specify this parameter to show only
-- the available offerings matching the specified DB instance class.
drdbioDBInstanceClass :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdbioDBInstanceClass =
    lens _drdbioDBInstanceClass (\s a -> s { _drdbioDBInstanceClass = a })

-- | Duration filter value, specified in years or seconds. Specify this
-- parameter to show only reservations for this duration. Valid Values: '1 |
-- 3 | 31536000 | 94608000'.
drdbioDuration :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdbioDuration = lens _drdbioDuration (\s a -> s { _drdbioDuration = a })

-- | This parameter is not currently supported.
drdbioFilters :: Lens' DescribeReservedDBInstancesOfferings [Filter]
drdbioFilters = lens _drdbioFilters (\s a -> s { _drdbioFilters = a }) . _List

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by 'MaxRecords'.
drdbioMarker :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdbioMarker = lens _drdbioMarker (\s a -> s { _drdbioMarker = a })

-- | The maximum number of records to include in the response. If more than
-- the 'MaxRecords' value is available, a pagination token called a marker
-- is included in the response so that the following results can be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
drdbioMaxRecords :: Lens' DescribeReservedDBInstancesOfferings (Maybe Int)
drdbioMaxRecords = lens _drdbioMaxRecords (\s a -> s { _drdbioMaxRecords = a })

-- | The Multi-AZ filter value. Specify this parameter to show only the
-- available offerings matching the specified Multi-AZ parameter.
drdbioMultiAZ :: Lens' DescribeReservedDBInstancesOfferings (Maybe Bool)
drdbioMultiAZ = lens _drdbioMultiAZ (\s a -> s { _drdbioMultiAZ = a })

-- | The offering type filter value. Specify this parameter to show only the
-- available offerings matching the specified offering type. Valid Values:
-- '"Light Utilization" | "Medium Utilization" | "Heavy Utilization" '.
drdbioOfferingType :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdbioOfferingType =
    lens _drdbioOfferingType (\s a -> s { _drdbioOfferingType = a })

-- | Product description filter value. Specify this parameter to show only the
-- available offerings matching the specified product description.
drdbioProductDescription :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdbioProductDescription =
    lens _drdbioProductDescription
        (\s a -> s { _drdbioProductDescription = a })

-- | The offering identifier filter value. Specify this parameter to show only
-- the available offering that matches the specified reservation identifier.
-- Example: '438012d3-4052-4cc7-b2e3-8d3372e0e706'.
drdbioReservedDBInstancesOfferingId :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdbioReservedDBInstancesOfferingId =
    lens _drdbioReservedDBInstancesOfferingId
        (\s a -> s { _drdbioReservedDBInstancesOfferingId = a })

data DescribeReservedDBInstancesOfferingsResponse = DescribeReservedDBInstancesOfferingsResponse
    { _drdbiorMarker                       :: Maybe Text
    , _drdbiorReservedDBInstancesOfferings :: List "ReservedDBInstancesOffering" ReservedDBInstancesOffering
    } deriving (Eq, Show)

-- | 'DescribeReservedDBInstancesOfferingsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drdbiorMarker' @::@ 'Maybe' 'Text'
--
-- * 'drdbiorReservedDBInstancesOfferings' @::@ ['ReservedDBInstancesOffering']
--
describeReservedDBInstancesOfferingsResponse :: DescribeReservedDBInstancesOfferingsResponse
describeReservedDBInstancesOfferingsResponse = DescribeReservedDBInstancesOfferingsResponse
    { _drdbiorMarker                       = Nothing
    , _drdbiorReservedDBInstancesOfferings = mempty
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by 'MaxRecords'.
drdbiorMarker :: Lens' DescribeReservedDBInstancesOfferingsResponse (Maybe Text)
drdbiorMarker = lens _drdbiorMarker (\s a -> s { _drdbiorMarker = a })

-- | A list of reserved DB instance offerings.
drdbiorReservedDBInstancesOfferings :: Lens' DescribeReservedDBInstancesOfferingsResponse [ReservedDBInstancesOffering]
drdbiorReservedDBInstancesOfferings =
    lens _drdbiorReservedDBInstancesOfferings
        (\s a -> s { _drdbiorReservedDBInstancesOfferings = a })
            . _List

instance ToPath DescribeReservedDBInstancesOfferings where
    toPath = const "/"

instance ToQuery DescribeReservedDBInstancesOfferings where
    toQuery DescribeReservedDBInstancesOfferings{..} = mconcat
        [ "DBInstanceClass"               =? _drdbioDBInstanceClass
        , "Duration"                      =? _drdbioDuration
        , "Filters"                       =? _drdbioFilters
        , "Marker"                        =? _drdbioMarker
        , "MaxRecords"                    =? _drdbioMaxRecords
        , "MultiAZ"                       =? _drdbioMultiAZ
        , "OfferingType"                  =? _drdbioOfferingType
        , "ProductDescription"            =? _drdbioProductDescription
        , "ReservedDBInstancesOfferingId" =? _drdbioReservedDBInstancesOfferingId
        ]

instance ToHeaders DescribeReservedDBInstancesOfferings

instance AWSRequest DescribeReservedDBInstancesOfferings where
    type Sv DescribeReservedDBInstancesOfferings = RDS
    type Rs DescribeReservedDBInstancesOfferings = DescribeReservedDBInstancesOfferingsResponse

    request  = post "DescribeReservedDBInstancesOfferings"
    response = xmlResponse

instance FromXML DescribeReservedDBInstancesOfferingsResponse where
    parseXML = withElement "DescribeReservedDBInstancesOfferingsResult" $ \x -> DescribeReservedDBInstancesOfferingsResponse
        <$> x .@? "Marker"
        <*> x .@  "ReservedDBInstancesOfferings"

instance AWSPager DescribeReservedDBInstancesOfferings where
    page rq rs
        | stop (rq ^. drdbioMarker) = Nothing
        | otherwise = (\x -> rq & drdbioMarker ?~ x)
            <$> (rs ^. drdbiorMarker)
