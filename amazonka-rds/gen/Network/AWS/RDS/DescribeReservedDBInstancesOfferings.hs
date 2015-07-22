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
    , drdiorqProductDescription
    , drdiorqFilters
    , drdiorqDBInstanceClass
    , drdiorqMaxRecords
    , drdiorqMultiAZ
    , drdiorqMarker
    , drdiorqReservedDBInstancesOfferingId
    , drdiorqOfferingType
    , drdiorqDuration

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
-- * 'drdiorqProductDescription'
--
-- * 'drdiorqFilters'
--
-- * 'drdiorqDBInstanceClass'
--
-- * 'drdiorqMaxRecords'
--
-- * 'drdiorqMultiAZ'
--
-- * 'drdiorqMarker'
--
-- * 'drdiorqReservedDBInstancesOfferingId'
--
-- * 'drdiorqOfferingType'
--
-- * 'drdiorqDuration'
data DescribeReservedDBInstancesOfferings = DescribeReservedDBInstancesOfferings'
    { _drdiorqProductDescription            :: !(Maybe Text)
    , _drdiorqFilters                       :: !(Maybe [Filter])
    , _drdiorqDBInstanceClass               :: !(Maybe Text)
    , _drdiorqMaxRecords                    :: !(Maybe Int)
    , _drdiorqMultiAZ                       :: !(Maybe Bool)
    , _drdiorqMarker                        :: !(Maybe Text)
    , _drdiorqReservedDBInstancesOfferingId :: !(Maybe Text)
    , _drdiorqOfferingType                  :: !(Maybe Text)
    , _drdiorqDuration                      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeReservedDBInstancesOfferings' smart constructor.
describeReservedDBInstancesOfferings :: DescribeReservedDBInstancesOfferings
describeReservedDBInstancesOfferings =
    DescribeReservedDBInstancesOfferings'
    { _drdiorqProductDescription = Nothing
    , _drdiorqFilters = Nothing
    , _drdiorqDBInstanceClass = Nothing
    , _drdiorqMaxRecords = Nothing
    , _drdiorqMultiAZ = Nothing
    , _drdiorqMarker = Nothing
    , _drdiorqReservedDBInstancesOfferingId = Nothing
    , _drdiorqOfferingType = Nothing
    , _drdiorqDuration = Nothing
    }

-- | Product description filter value. Specify this parameter to show only
-- the available offerings matching the specified product description.
drdiorqProductDescription :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdiorqProductDescription = lens _drdiorqProductDescription (\ s a -> s{_drdiorqProductDescription = a});

-- | This parameter is not currently supported.
drdiorqFilters :: Lens' DescribeReservedDBInstancesOfferings [Filter]
drdiorqFilters = lens _drdiorqFilters (\ s a -> s{_drdiorqFilters = a}) . _Default;

-- | The DB instance class filter value. Specify this parameter to show only
-- the available offerings matching the specified DB instance class.
drdiorqDBInstanceClass :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdiorqDBInstanceClass = lens _drdiorqDBInstanceClass (\ s a -> s{_drdiorqDBInstanceClass = a});

-- | The maximum number of records to include in the response. If more than
-- the @MaxRecords@ value is available, a pagination token called a marker
-- is included in the response so that the following results can be
-- retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
drdiorqMaxRecords :: Lens' DescribeReservedDBInstancesOfferings (Maybe Int)
drdiorqMaxRecords = lens _drdiorqMaxRecords (\ s a -> s{_drdiorqMaxRecords = a});

-- | The Multi-AZ filter value. Specify this parameter to show only the
-- available offerings matching the specified Multi-AZ parameter.
drdiorqMultiAZ :: Lens' DescribeReservedDBInstancesOfferings (Maybe Bool)
drdiorqMultiAZ = lens _drdiorqMultiAZ (\ s a -> s{_drdiorqMultiAZ = a});

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
drdiorqMarker :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdiorqMarker = lens _drdiorqMarker (\ s a -> s{_drdiorqMarker = a});

-- | The offering identifier filter value. Specify this parameter to show
-- only the available offering that matches the specified reservation
-- identifier.
--
-- Example: @438012d3-4052-4cc7-b2e3-8d3372e0e706@
drdiorqReservedDBInstancesOfferingId :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdiorqReservedDBInstancesOfferingId = lens _drdiorqReservedDBInstancesOfferingId (\ s a -> s{_drdiorqReservedDBInstancesOfferingId = a});

-- | The offering type filter value. Specify this parameter to show only the
-- available offerings matching the specified offering type.
--
-- Valid Values:
-- @\"Light Utilization\" | \"Medium Utilization\" | \"Heavy Utilization\" @
drdiorqOfferingType :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdiorqOfferingType = lens _drdiorqOfferingType (\ s a -> s{_drdiorqOfferingType = a});

-- | Duration filter value, specified in years or seconds. Specify this
-- parameter to show only reservations for this duration.
--
-- Valid Values: @1 | 3 | 31536000 | 94608000@
drdiorqDuration :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdiorqDuration = lens _drdiorqDuration (\ s a -> s{_drdiorqDuration = a});

instance AWSPager
         DescribeReservedDBInstancesOfferings where
        page rq rs
          | stop (rs ^. drdiorsMarker) = Nothing
          | stop (rs ^. drdiorsReservedDBInstancesOfferings) =
            Nothing
          | otherwise =
            Just $ rq & drdiorqMarker .~ rs ^. drdiorsMarker

instance AWSRequest
         DescribeReservedDBInstancesOfferings where
        type Sv DescribeReservedDBInstancesOfferings = RDS
        type Rs DescribeReservedDBInstancesOfferings =
             DescribeReservedDBInstancesOfferingsResponse
        request = post
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
               "ProductDescription" =: _drdiorqProductDescription,
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _drdiorqFilters),
               "DBInstanceClass" =: _drdiorqDBInstanceClass,
               "MaxRecords" =: _drdiorqMaxRecords,
               "MultiAZ" =: _drdiorqMultiAZ,
               "Marker" =: _drdiorqMarker,
               "ReservedDBInstancesOfferingId" =:
                 _drdiorqReservedDBInstancesOfferingId,
               "OfferingType" =: _drdiorqOfferingType,
               "Duration" =: _drdiorqDuration]

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
