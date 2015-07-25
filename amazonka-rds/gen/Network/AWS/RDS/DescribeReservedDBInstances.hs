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
    , drdiProductDescription
    , drdiFilters
    , drdiReservedDBInstanceId
    , drdiDBInstanceClass
    , drdiMaxRecords
    , drdiMultiAZ
    , drdiMarker
    , drdiReservedDBInstancesOfferingId
    , drdiOfferingType
    , drdiDuration

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
-- * 'drdiProductDescription'
--
-- * 'drdiFilters'
--
-- * 'drdiReservedDBInstanceId'
--
-- * 'drdiDBInstanceClass'
--
-- * 'drdiMaxRecords'
--
-- * 'drdiMultiAZ'
--
-- * 'drdiMarker'
--
-- * 'drdiReservedDBInstancesOfferingId'
--
-- * 'drdiOfferingType'
--
-- * 'drdiDuration'
data DescribeReservedDBInstances = DescribeReservedDBInstances'
    { _drdiProductDescription            :: !(Maybe Text)
    , _drdiFilters                       :: !(Maybe [Filter])
    , _drdiReservedDBInstanceId          :: !(Maybe Text)
    , _drdiDBInstanceClass               :: !(Maybe Text)
    , _drdiMaxRecords                    :: !(Maybe Int)
    , _drdiMultiAZ                       :: !(Maybe Bool)
    , _drdiMarker                        :: !(Maybe Text)
    , _drdiReservedDBInstancesOfferingId :: !(Maybe Text)
    , _drdiOfferingType                  :: !(Maybe Text)
    , _drdiDuration                      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeReservedDBInstances' smart constructor.
describeReservedDBInstances :: DescribeReservedDBInstances
describeReservedDBInstances =
    DescribeReservedDBInstances'
    { _drdiProductDescription = Nothing
    , _drdiFilters = Nothing
    , _drdiReservedDBInstanceId = Nothing
    , _drdiDBInstanceClass = Nothing
    , _drdiMaxRecords = Nothing
    , _drdiMultiAZ = Nothing
    , _drdiMarker = Nothing
    , _drdiReservedDBInstancesOfferingId = Nothing
    , _drdiOfferingType = Nothing
    , _drdiDuration = Nothing
    }

-- | The product description filter value. Specify this parameter to show
-- only those reservations matching the specified product description.
drdiProductDescription :: Lens' DescribeReservedDBInstances (Maybe Text)
drdiProductDescription = lens _drdiProductDescription (\ s a -> s{_drdiProductDescription = a});

-- | This parameter is not currently supported.
drdiFilters :: Lens' DescribeReservedDBInstances [Filter]
drdiFilters = lens _drdiFilters (\ s a -> s{_drdiFilters = a}) . _Default . _Coerce;

-- | The reserved DB instance identifier filter value. Specify this parameter
-- to show only the reservation that matches the specified reservation ID.
drdiReservedDBInstanceId :: Lens' DescribeReservedDBInstances (Maybe Text)
drdiReservedDBInstanceId = lens _drdiReservedDBInstanceId (\ s a -> s{_drdiReservedDBInstanceId = a});

-- | The DB instance class filter value. Specify this parameter to show only
-- those reservations matching the specified DB instances class.
drdiDBInstanceClass :: Lens' DescribeReservedDBInstances (Maybe Text)
drdiDBInstanceClass = lens _drdiDBInstanceClass (\ s a -> s{_drdiDBInstanceClass = a});

-- | The maximum number of records to include in the response. If more than
-- the @MaxRecords@ value is available, a pagination token called a marker
-- is included in the response so that the following results can be
-- retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
drdiMaxRecords :: Lens' DescribeReservedDBInstances (Maybe Int)
drdiMaxRecords = lens _drdiMaxRecords (\ s a -> s{_drdiMaxRecords = a});

-- | The Multi-AZ filter value. Specify this parameter to show only those
-- reservations matching the specified Multi-AZ parameter.
drdiMultiAZ :: Lens' DescribeReservedDBInstances (Maybe Bool)
drdiMultiAZ = lens _drdiMultiAZ (\ s a -> s{_drdiMultiAZ = a});

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
drdiMarker :: Lens' DescribeReservedDBInstances (Maybe Text)
drdiMarker = lens _drdiMarker (\ s a -> s{_drdiMarker = a});

-- | The offering identifier filter value. Specify this parameter to show
-- only purchased reservations matching the specified offering identifier.
drdiReservedDBInstancesOfferingId :: Lens' DescribeReservedDBInstances (Maybe Text)
drdiReservedDBInstancesOfferingId = lens _drdiReservedDBInstancesOfferingId (\ s a -> s{_drdiReservedDBInstancesOfferingId = a});

-- | The offering type filter value. Specify this parameter to show only the
-- available offerings matching the specified offering type.
--
-- Valid Values:
-- @\"Light Utilization\" | \"Medium Utilization\" | \"Heavy Utilization\" @
drdiOfferingType :: Lens' DescribeReservedDBInstances (Maybe Text)
drdiOfferingType = lens _drdiOfferingType (\ s a -> s{_drdiOfferingType = a});

-- | The duration filter value, specified in years or seconds. Specify this
-- parameter to show only reservations for this duration.
--
-- Valid Values: @1 | 3 | 31536000 | 94608000@
drdiDuration :: Lens' DescribeReservedDBInstances (Maybe Text)
drdiDuration = lens _drdiDuration (\ s a -> s{_drdiDuration = a});

instance AWSPager DescribeReservedDBInstances where
        page rq rs
          | stop (rs ^. drdirsMarker) = Nothing
          | stop (rs ^. drdirsReservedDBInstances) = Nothing
          | otherwise =
            Just $ rq & drdiMarker .~ rs ^. drdirsMarker

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
               "ProductDescription" =: _drdiProductDescription,
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _drdiFilters),
               "ReservedDBInstanceId" =: _drdiReservedDBInstanceId,
               "DBInstanceClass" =: _drdiDBInstanceClass,
               "MaxRecords" =: _drdiMaxRecords,
               "MultiAZ" =: _drdiMultiAZ, "Marker" =: _drdiMarker,
               "ReservedDBInstancesOfferingId" =:
                 _drdiReservedDBInstancesOfferingId,
               "OfferingType" =: _drdiOfferingType,
               "Duration" =: _drdiDuration]

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
drdirsReservedDBInstances = lens _drdirsReservedDBInstances (\ s a -> s{_drdirsReservedDBInstances = a}) . _Default . _Coerce;

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
drdirsMarker :: Lens' DescribeReservedDBInstancesResponse (Maybe Text)
drdirsMarker = lens _drdirsMarker (\ s a -> s{_drdirsMarker = a});

-- | FIXME: Undocumented member.
drdirsStatus :: Lens' DescribeReservedDBInstancesResponse Int
drdirsStatus = lens _drdirsStatus (\ s a -> s{_drdirsStatus = a});
