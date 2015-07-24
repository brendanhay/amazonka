{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeReservedDBInstancesOfferings
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists available reserved DB instance offerings.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeReservedDBInstancesOfferings.html>
module Network.AWS.RDS.DescribeReservedDBInstancesOfferings
    (
    -- * Request
      DescribeReservedDBInstancesOfferings
    -- ** Request constructor
    , describeReservedDBInstancesOfferings
    -- ** Request lenses
    , drdioProductDescription
    , drdioFilters
    , drdioDBInstanceClass
    , drdioMaxRecords
    , drdioMultiAZ
    , drdioMarker
    , drdioReservedDBInstancesOfferingId
    , drdioOfferingType
    , drdioDuration

    -- * Response
    , DescribeReservedDBInstancesOfferingsResponse
    -- ** Response constructor
    , describeReservedDBInstancesOfferingsResponse
    -- ** Response lenses
    , drdiorsMarker
    , drdiorsReservedDBInstancesOfferings
    , drdiorsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeReservedDBInstancesOfferings' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drdioProductDescription'
--
-- * 'drdioFilters'
--
-- * 'drdioDBInstanceClass'
--
-- * 'drdioMaxRecords'
--
-- * 'drdioMultiAZ'
--
-- * 'drdioMarker'
--
-- * 'drdioReservedDBInstancesOfferingId'
--
-- * 'drdioOfferingType'
--
-- * 'drdioDuration'
data DescribeReservedDBInstancesOfferings = DescribeReservedDBInstancesOfferings'
    { _drdioProductDescription            :: !(Maybe Text)
    , _drdioFilters                       :: !(Maybe [Filter])
    , _drdioDBInstanceClass               :: !(Maybe Text)
    , _drdioMaxRecords                    :: !(Maybe Int)
    , _drdioMultiAZ                       :: !(Maybe Bool)
    , _drdioMarker                        :: !(Maybe Text)
    , _drdioReservedDBInstancesOfferingId :: !(Maybe Text)
    , _drdioOfferingType                  :: !(Maybe Text)
    , _drdioDuration                      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeReservedDBInstancesOfferings' smart constructor.
describeReservedDBInstancesOfferings :: DescribeReservedDBInstancesOfferings
describeReservedDBInstancesOfferings =
    DescribeReservedDBInstancesOfferings'
    { _drdioProductDescription = Nothing
    , _drdioFilters = Nothing
    , _drdioDBInstanceClass = Nothing
    , _drdioMaxRecords = Nothing
    , _drdioMultiAZ = Nothing
    , _drdioMarker = Nothing
    , _drdioReservedDBInstancesOfferingId = Nothing
    , _drdioOfferingType = Nothing
    , _drdioDuration = Nothing
    }

-- | Product description filter value. Specify this parameter to show only
-- the available offerings matching the specified product description.
drdioProductDescription :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdioProductDescription = lens _drdioProductDescription (\ s a -> s{_drdioProductDescription = a});

-- | This parameter is not currently supported.
drdioFilters :: Lens' DescribeReservedDBInstancesOfferings [Filter]
drdioFilters = lens _drdioFilters (\ s a -> s{_drdioFilters = a}) . _Default;

-- | The DB instance class filter value. Specify this parameter to show only
-- the available offerings matching the specified DB instance class.
drdioDBInstanceClass :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdioDBInstanceClass = lens _drdioDBInstanceClass (\ s a -> s{_drdioDBInstanceClass = a});

-- | The maximum number of records to include in the response. If more than
-- the @MaxRecords@ value is available, a pagination token called a marker
-- is included in the response so that the following results can be
-- retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
drdioMaxRecords :: Lens' DescribeReservedDBInstancesOfferings (Maybe Int)
drdioMaxRecords = lens _drdioMaxRecords (\ s a -> s{_drdioMaxRecords = a});

-- | The Multi-AZ filter value. Specify this parameter to show only the
-- available offerings matching the specified Multi-AZ parameter.
drdioMultiAZ :: Lens' DescribeReservedDBInstancesOfferings (Maybe Bool)
drdioMultiAZ = lens _drdioMultiAZ (\ s a -> s{_drdioMultiAZ = a});

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
drdioMarker :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdioMarker = lens _drdioMarker (\ s a -> s{_drdioMarker = a});

-- | The offering identifier filter value. Specify this parameter to show
-- only the available offering that matches the specified reservation
-- identifier.
--
-- Example: @438012d3-4052-4cc7-b2e3-8d3372e0e706@
drdioReservedDBInstancesOfferingId :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdioReservedDBInstancesOfferingId = lens _drdioReservedDBInstancesOfferingId (\ s a -> s{_drdioReservedDBInstancesOfferingId = a});

-- | The offering type filter value. Specify this parameter to show only the
-- available offerings matching the specified offering type.
--
-- Valid Values:
-- @\"Light Utilization\" | \"Medium Utilization\" | \"Heavy Utilization\" @
drdioOfferingType :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdioOfferingType = lens _drdioOfferingType (\ s a -> s{_drdioOfferingType = a});

-- | Duration filter value, specified in years or seconds. Specify this
-- parameter to show only reservations for this duration.
--
-- Valid Values: @1 | 3 | 31536000 | 94608000@
drdioDuration :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdioDuration = lens _drdioDuration (\ s a -> s{_drdioDuration = a});

instance AWSPager
         DescribeReservedDBInstancesOfferings where
        page rq rs
          | stop (rs ^. drdiorsMarker) = Nothing
          | stop (rs ^. drdiorsReservedDBInstancesOfferings) =
            Nothing
          | otherwise =
            Just $ rq & drdioMarker .~ rs ^. drdiorsMarker

instance AWSRequest
         DescribeReservedDBInstancesOfferings where
        type Sv DescribeReservedDBInstancesOfferings = RDS
        type Rs DescribeReservedDBInstancesOfferings =
             DescribeReservedDBInstancesOfferingsResponse
        request = postQuery
        response
          = receiveXMLWrapper
              "DescribeReservedDBInstancesOfferingsResult"
              (\ s h x ->
                 DescribeReservedDBInstancesOfferingsResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "ReservedDBInstancesOfferings" .!@ mempty >>=
                        may (parseXMLList "ReservedDBInstancesOffering"))
                     <*> (pure (fromEnum s)))

instance ToHeaders
         DescribeReservedDBInstancesOfferings where
        toHeaders = const mempty

instance ToPath DescribeReservedDBInstancesOfferings
         where
        toPath = const "/"

instance ToQuery DescribeReservedDBInstancesOfferings
         where
        toQuery DescribeReservedDBInstancesOfferings'{..}
          = mconcat
              ["Action" =:
                 ("DescribeReservedDBInstancesOfferings" ::
                    ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "ProductDescription" =: _drdioProductDescription,
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _drdioFilters),
               "DBInstanceClass" =: _drdioDBInstanceClass,
               "MaxRecords" =: _drdioMaxRecords,
               "MultiAZ" =: _drdioMultiAZ, "Marker" =: _drdioMarker,
               "ReservedDBInstancesOfferingId" =:
                 _drdioReservedDBInstancesOfferingId,
               "OfferingType" =: _drdioOfferingType,
               "Duration" =: _drdioDuration]

-- | Contains the result of a successful invocation of the
-- DescribeReservedDBInstancesOfferings action.
--
-- /See:/ 'describeReservedDBInstancesOfferingsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drdiorsMarker'
--
-- * 'drdiorsReservedDBInstancesOfferings'
--
-- * 'drdiorsStatus'
data DescribeReservedDBInstancesOfferingsResponse = DescribeReservedDBInstancesOfferingsResponse'
    { _drdiorsMarker                       :: !(Maybe Text)
    , _drdiorsReservedDBInstancesOfferings :: !(Maybe [ReservedDBInstancesOffering])
    , _drdiorsStatus                       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeReservedDBInstancesOfferingsResponse' smart constructor.
describeReservedDBInstancesOfferingsResponse :: Int -> DescribeReservedDBInstancesOfferingsResponse
describeReservedDBInstancesOfferingsResponse pStatus_ =
    DescribeReservedDBInstancesOfferingsResponse'
    { _drdiorsMarker = Nothing
    , _drdiorsReservedDBInstancesOfferings = Nothing
    , _drdiorsStatus = pStatus_
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
drdiorsMarker :: Lens' DescribeReservedDBInstancesOfferingsResponse (Maybe Text)
drdiorsMarker = lens _drdiorsMarker (\ s a -> s{_drdiorsMarker = a});

-- | A list of reserved DB instance offerings.
drdiorsReservedDBInstancesOfferings :: Lens' DescribeReservedDBInstancesOfferingsResponse [ReservedDBInstancesOffering]
drdiorsReservedDBInstancesOfferings = lens _drdiorsReservedDBInstancesOfferings (\ s a -> s{_drdiorsReservedDBInstancesOfferings = a}) . _Default;

-- | FIXME: Undocumented member.
drdiorsStatus :: Lens' DescribeReservedDBInstancesOfferingsResponse Int
drdiorsStatus = lens _drdiorsStatus (\ s a -> s{_drdiorsStatus = a});
