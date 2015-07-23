{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeReservedDBInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns information about reserved DB instances for this account, or
-- about a specified reserved DB instance.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeReservedDBInstances.html>
module Network.AWS.RDS.DescribeReservedDBInstances
    (
    -- * Request
      DescribeReservedDBInstances
    -- ** Request constructor
    , describeReservedDBInstances
    -- ** Request lenses
    , drdirqProductDescription
    , drdirqFilters
    , drdirqReservedDBInstanceId
    , drdirqDBInstanceClass
    , drdirqMaxRecords
    , drdirqMultiAZ
    , drdirqMarker
    , drdirqReservedDBInstancesOfferingId
    , drdirqOfferingType
    , drdirqDuration

    -- * Response
    , DescribeReservedDBInstancesResponse
    -- ** Response constructor
    , describeReservedDBInstancesResponse
    -- ** Response lenses
    , drdirsReservedDBInstances
    , drdirsMarker
    , drdirsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeReservedDBInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drdirqProductDescription'
--
-- * 'drdirqFilters'
--
-- * 'drdirqReservedDBInstanceId'
--
-- * 'drdirqDBInstanceClass'
--
-- * 'drdirqMaxRecords'
--
-- * 'drdirqMultiAZ'
--
-- * 'drdirqMarker'
--
-- * 'drdirqReservedDBInstancesOfferingId'
--
-- * 'drdirqOfferingType'
--
-- * 'drdirqDuration'
data DescribeReservedDBInstances = DescribeReservedDBInstances'
    { _drdirqProductDescription            :: !(Maybe Text)
    , _drdirqFilters                       :: !(Maybe [Filter])
    , _drdirqReservedDBInstanceId          :: !(Maybe Text)
    , _drdirqDBInstanceClass               :: !(Maybe Text)
    , _drdirqMaxRecords                    :: !(Maybe Int)
    , _drdirqMultiAZ                       :: !(Maybe Bool)
    , _drdirqMarker                        :: !(Maybe Text)
    , _drdirqReservedDBInstancesOfferingId :: !(Maybe Text)
    , _drdirqOfferingType                  :: !(Maybe Text)
    , _drdirqDuration                      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeReservedDBInstances' smart constructor.
describeReservedDBInstances :: DescribeReservedDBInstances
describeReservedDBInstances =
    DescribeReservedDBInstances'
    { _drdirqProductDescription = Nothing
    , _drdirqFilters = Nothing
    , _drdirqReservedDBInstanceId = Nothing
    , _drdirqDBInstanceClass = Nothing
    , _drdirqMaxRecords = Nothing
    , _drdirqMultiAZ = Nothing
    , _drdirqMarker = Nothing
    , _drdirqReservedDBInstancesOfferingId = Nothing
    , _drdirqOfferingType = Nothing
    , _drdirqDuration = Nothing
    }

-- | The product description filter value. Specify this parameter to show
-- only those reservations matching the specified product description.
drdirqProductDescription :: Lens' DescribeReservedDBInstances (Maybe Text)
drdirqProductDescription = lens _drdirqProductDescription (\ s a -> s{_drdirqProductDescription = a});

-- | This parameter is not currently supported.
drdirqFilters :: Lens' DescribeReservedDBInstances [Filter]
drdirqFilters = lens _drdirqFilters (\ s a -> s{_drdirqFilters = a}) . _Default;

-- | The reserved DB instance identifier filter value. Specify this parameter
-- to show only the reservation that matches the specified reservation ID.
drdirqReservedDBInstanceId :: Lens' DescribeReservedDBInstances (Maybe Text)
drdirqReservedDBInstanceId = lens _drdirqReservedDBInstanceId (\ s a -> s{_drdirqReservedDBInstanceId = a});

-- | The DB instance class filter value. Specify this parameter to show only
-- those reservations matching the specified DB instances class.
drdirqDBInstanceClass :: Lens' DescribeReservedDBInstances (Maybe Text)
drdirqDBInstanceClass = lens _drdirqDBInstanceClass (\ s a -> s{_drdirqDBInstanceClass = a});

-- | The maximum number of records to include in the response. If more than
-- the @MaxRecords@ value is available, a pagination token called a marker
-- is included in the response so that the following results can be
-- retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
drdirqMaxRecords :: Lens' DescribeReservedDBInstances (Maybe Int)
drdirqMaxRecords = lens _drdirqMaxRecords (\ s a -> s{_drdirqMaxRecords = a});

-- | The Multi-AZ filter value. Specify this parameter to show only those
-- reservations matching the specified Multi-AZ parameter.
drdirqMultiAZ :: Lens' DescribeReservedDBInstances (Maybe Bool)
drdirqMultiAZ = lens _drdirqMultiAZ (\ s a -> s{_drdirqMultiAZ = a});

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
drdirqMarker :: Lens' DescribeReservedDBInstances (Maybe Text)
drdirqMarker = lens _drdirqMarker (\ s a -> s{_drdirqMarker = a});

-- | The offering identifier filter value. Specify this parameter to show
-- only purchased reservations matching the specified offering identifier.
drdirqReservedDBInstancesOfferingId :: Lens' DescribeReservedDBInstances (Maybe Text)
drdirqReservedDBInstancesOfferingId = lens _drdirqReservedDBInstancesOfferingId (\ s a -> s{_drdirqReservedDBInstancesOfferingId = a});

-- | The offering type filter value. Specify this parameter to show only the
-- available offerings matching the specified offering type.
--
-- Valid Values:
-- @\"Light Utilization\" | \"Medium Utilization\" | \"Heavy Utilization\" @
drdirqOfferingType :: Lens' DescribeReservedDBInstances (Maybe Text)
drdirqOfferingType = lens _drdirqOfferingType (\ s a -> s{_drdirqOfferingType = a});

-- | The duration filter value, specified in years or seconds. Specify this
-- parameter to show only reservations for this duration.
--
-- Valid Values: @1 | 3 | 31536000 | 94608000@
drdirqDuration :: Lens' DescribeReservedDBInstances (Maybe Text)
drdirqDuration = lens _drdirqDuration (\ s a -> s{_drdirqDuration = a});

instance AWSPager DescribeReservedDBInstances where
        page rq rs
          | stop (rs ^. drdirsMarker) = Nothing
          | stop (rs ^. drdirsReservedDBInstances) = Nothing
          | otherwise =
            Just $ rq & drdirqMarker .~ rs ^. drdirsMarker

instance AWSRequest DescribeReservedDBInstances where
        type Sv DescribeReservedDBInstances = RDS
        type Rs DescribeReservedDBInstances =
             DescribeReservedDBInstancesResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeReservedDBInstancesResult"
              (\ s h x ->
                 DescribeReservedDBInstancesResponse' <$>
                   (x .@? "ReservedDBInstances" .!@ mempty >>=
                      may (parseXMLList "ReservedDBInstance"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeReservedDBInstances where
        toHeaders = const mempty

instance ToPath DescribeReservedDBInstances where
        toPath = const "/"

instance ToQuery DescribeReservedDBInstances where
        toQuery DescribeReservedDBInstances'{..}
          = mconcat
              ["Action" =:
                 ("DescribeReservedDBInstances" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "ProductDescription" =: _drdirqProductDescription,
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _drdirqFilters),
               "ReservedDBInstanceId" =:
                 _drdirqReservedDBInstanceId,
               "DBInstanceClass" =: _drdirqDBInstanceClass,
               "MaxRecords" =: _drdirqMaxRecords,
               "MultiAZ" =: _drdirqMultiAZ,
               "Marker" =: _drdirqMarker,
               "ReservedDBInstancesOfferingId" =:
                 _drdirqReservedDBInstancesOfferingId,
               "OfferingType" =: _drdirqOfferingType,
               "Duration" =: _drdirqDuration]

-- | Contains the result of a successful invocation of the
-- DescribeReservedDBInstances action.
--
-- /See:/ 'describeReservedDBInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drdirsReservedDBInstances'
--
-- * 'drdirsMarker'
--
-- * 'drdirsStatus'
data DescribeReservedDBInstancesResponse = DescribeReservedDBInstancesResponse'
    { _drdirsReservedDBInstances :: !(Maybe [ReservedDBInstance])
    , _drdirsMarker              :: !(Maybe Text)
    , _drdirsStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeReservedDBInstancesResponse' smart constructor.
describeReservedDBInstancesResponse :: Int -> DescribeReservedDBInstancesResponse
describeReservedDBInstancesResponse pStatus_ =
    DescribeReservedDBInstancesResponse'
    { _drdirsReservedDBInstances = Nothing
    , _drdirsMarker = Nothing
    , _drdirsStatus = pStatus_
    }

-- | A list of reserved DB instances.
drdirsReservedDBInstances :: Lens' DescribeReservedDBInstancesResponse [ReservedDBInstance]
drdirsReservedDBInstances = lens _drdirsReservedDBInstances (\ s a -> s{_drdirsReservedDBInstances = a}) . _Default;

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
drdirsMarker :: Lens' DescribeReservedDBInstancesResponse (Maybe Text)
drdirsMarker = lens _drdirsMarker (\ s a -> s{_drdirsMarker = a});

-- | FIXME: Undocumented member.
drdirsStatus :: Lens' DescribeReservedDBInstancesResponse Int
drdirsStatus = lens _drdirsStatus (\ s a -> s{_drdirsStatus = a});
