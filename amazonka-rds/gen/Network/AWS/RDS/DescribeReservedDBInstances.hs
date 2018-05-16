{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeReservedDBInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about reserved DB instances for this account, or about a specified reserved DB instance.
--
--
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeReservedDBInstances
    (
    -- * Creating a Request
      describeReservedDBInstances
    , DescribeReservedDBInstances
    -- * Request Lenses
    , drdiProductDescription
    , drdiFilters
    , drdiReservedDBInstanceId
    , drdiDBInstanceClass
    , drdiMarker
    , drdiMaxRecords
    , drdiMultiAZ
    , drdiReservedDBInstancesOfferingId
    , drdiOfferingType
    , drdiDuration

    -- * Destructuring the Response
    , describeReservedDBInstancesResponse
    , DescribeReservedDBInstancesResponse
    -- * Response Lenses
    , drdirsReservedDBInstances
    , drdirsMarker
    , drdirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeReservedDBInstances' smart constructor.
data DescribeReservedDBInstances = DescribeReservedDBInstances'
  { _drdiProductDescription            :: !(Maybe Text)
  , _drdiFilters                       :: !(Maybe [Filter])
  , _drdiReservedDBInstanceId          :: !(Maybe Text)
  , _drdiDBInstanceClass               :: !(Maybe Text)
  , _drdiMarker                        :: !(Maybe Text)
  , _drdiMaxRecords                    :: !(Maybe Int)
  , _drdiMultiAZ                       :: !(Maybe Bool)
  , _drdiReservedDBInstancesOfferingId :: !(Maybe Text)
  , _drdiOfferingType                  :: !(Maybe Text)
  , _drdiDuration                      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeReservedDBInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drdiProductDescription' - The product description filter value. Specify this parameter to show only those reservations matching the specified product description.
--
-- * 'drdiFilters' - This parameter is not currently supported.
--
-- * 'drdiReservedDBInstanceId' - The reserved DB instance identifier filter value. Specify this parameter to show only the reservation that matches the specified reservation ID.
--
-- * 'drdiDBInstanceClass' - The DB instance class filter value. Specify this parameter to show only those reservations matching the specified DB instances class.
--
-- * 'drdiMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'drdiMaxRecords' - The maximum number of records to include in the response. If more than the @MaxRecords@ value is available, a pagination token called a marker is included in the response so that the following results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
--
-- * 'drdiMultiAZ' - The Multi-AZ filter value. Specify this parameter to show only those reservations matching the specified Multi-AZ parameter.
--
-- * 'drdiReservedDBInstancesOfferingId' - The offering identifier filter value. Specify this parameter to show only purchased reservations matching the specified offering identifier.
--
-- * 'drdiOfferingType' - The offering type filter value. Specify this parameter to show only the available offerings matching the specified offering type. Valid Values: @"Partial Upfront" | "All Upfront" | "No Upfront" @
--
-- * 'drdiDuration' - The duration filter value, specified in years or seconds. Specify this parameter to show only reservations for this duration. Valid Values: @1 | 3 | 31536000 | 94608000@
describeReservedDBInstances
    :: DescribeReservedDBInstances
describeReservedDBInstances =
  DescribeReservedDBInstances'
    { _drdiProductDescription = Nothing
    , _drdiFilters = Nothing
    , _drdiReservedDBInstanceId = Nothing
    , _drdiDBInstanceClass = Nothing
    , _drdiMarker = Nothing
    , _drdiMaxRecords = Nothing
    , _drdiMultiAZ = Nothing
    , _drdiReservedDBInstancesOfferingId = Nothing
    , _drdiOfferingType = Nothing
    , _drdiDuration = Nothing
    }


-- | The product description filter value. Specify this parameter to show only those reservations matching the specified product description.
drdiProductDescription :: Lens' DescribeReservedDBInstances (Maybe Text)
drdiProductDescription = lens _drdiProductDescription (\ s a -> s{_drdiProductDescription = a})

-- | This parameter is not currently supported.
drdiFilters :: Lens' DescribeReservedDBInstances [Filter]
drdiFilters = lens _drdiFilters (\ s a -> s{_drdiFilters = a}) . _Default . _Coerce

-- | The reserved DB instance identifier filter value. Specify this parameter to show only the reservation that matches the specified reservation ID.
drdiReservedDBInstanceId :: Lens' DescribeReservedDBInstances (Maybe Text)
drdiReservedDBInstanceId = lens _drdiReservedDBInstanceId (\ s a -> s{_drdiReservedDBInstanceId = a})

-- | The DB instance class filter value. Specify this parameter to show only those reservations matching the specified DB instances class.
drdiDBInstanceClass :: Lens' DescribeReservedDBInstances (Maybe Text)
drdiDBInstanceClass = lens _drdiDBInstanceClass (\ s a -> s{_drdiDBInstanceClass = a})

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
drdiMarker :: Lens' DescribeReservedDBInstances (Maybe Text)
drdiMarker = lens _drdiMarker (\ s a -> s{_drdiMarker = a})

-- | The maximum number of records to include in the response. If more than the @MaxRecords@ value is available, a pagination token called a marker is included in the response so that the following results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
drdiMaxRecords :: Lens' DescribeReservedDBInstances (Maybe Int)
drdiMaxRecords = lens _drdiMaxRecords (\ s a -> s{_drdiMaxRecords = a})

-- | The Multi-AZ filter value. Specify this parameter to show only those reservations matching the specified Multi-AZ parameter.
drdiMultiAZ :: Lens' DescribeReservedDBInstances (Maybe Bool)
drdiMultiAZ = lens _drdiMultiAZ (\ s a -> s{_drdiMultiAZ = a})

-- | The offering identifier filter value. Specify this parameter to show only purchased reservations matching the specified offering identifier.
drdiReservedDBInstancesOfferingId :: Lens' DescribeReservedDBInstances (Maybe Text)
drdiReservedDBInstancesOfferingId = lens _drdiReservedDBInstancesOfferingId (\ s a -> s{_drdiReservedDBInstancesOfferingId = a})

-- | The offering type filter value. Specify this parameter to show only the available offerings matching the specified offering type. Valid Values: @"Partial Upfront" | "All Upfront" | "No Upfront" @
drdiOfferingType :: Lens' DescribeReservedDBInstances (Maybe Text)
drdiOfferingType = lens _drdiOfferingType (\ s a -> s{_drdiOfferingType = a})

-- | The duration filter value, specified in years or seconds. Specify this parameter to show only reservations for this duration. Valid Values: @1 | 3 | 31536000 | 94608000@
drdiDuration :: Lens' DescribeReservedDBInstances (Maybe Text)
drdiDuration = lens _drdiDuration (\ s a -> s{_drdiDuration = a})

instance AWSPager DescribeReservedDBInstances where
        page rq rs
          | stop (rs ^. drdirsMarker) = Nothing
          | stop (rs ^. drdirsReservedDBInstances) = Nothing
          | otherwise =
            Just $ rq & drdiMarker .~ rs ^. drdirsMarker

instance AWSRequest DescribeReservedDBInstances where
        type Rs DescribeReservedDBInstances =
             DescribeReservedDBInstancesResponse
        request = postQuery rds
        response
          = receiveXMLWrapper
              "DescribeReservedDBInstancesResult"
              (\ s h x ->
                 DescribeReservedDBInstancesResponse' <$>
                   (x .@? "ReservedDBInstances" .!@ mempty >>=
                      may (parseXMLList "ReservedDBInstance"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeReservedDBInstances where

instance NFData DescribeReservedDBInstances where

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
               "Marker" =: _drdiMarker,
               "MaxRecords" =: _drdiMaxRecords,
               "MultiAZ" =: _drdiMultiAZ,
               "ReservedDBInstancesOfferingId" =:
                 _drdiReservedDBInstancesOfferingId,
               "OfferingType" =: _drdiOfferingType,
               "Duration" =: _drdiDuration]

-- | Contains the result of a successful invocation of the 'DescribeReservedDBInstances' action.
--
--
--
-- /See:/ 'describeReservedDBInstancesResponse' smart constructor.
data DescribeReservedDBInstancesResponse = DescribeReservedDBInstancesResponse'
  { _drdirsReservedDBInstances :: !(Maybe [ReservedDBInstance])
  , _drdirsMarker              :: !(Maybe Text)
  , _drdirsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeReservedDBInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drdirsReservedDBInstances' - A list of reserved DB instances.
--
-- * 'drdirsMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'drdirsResponseStatus' - -- | The response status code.
describeReservedDBInstancesResponse
    :: Int -- ^ 'drdirsResponseStatus'
    -> DescribeReservedDBInstancesResponse
describeReservedDBInstancesResponse pResponseStatus_ =
  DescribeReservedDBInstancesResponse'
    { _drdirsReservedDBInstances = Nothing
    , _drdirsMarker = Nothing
    , _drdirsResponseStatus = pResponseStatus_
    }


-- | A list of reserved DB instances.
drdirsReservedDBInstances :: Lens' DescribeReservedDBInstancesResponse [ReservedDBInstance]
drdirsReservedDBInstances = lens _drdirsReservedDBInstances (\ s a -> s{_drdirsReservedDBInstances = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
drdirsMarker :: Lens' DescribeReservedDBInstancesResponse (Maybe Text)
drdirsMarker = lens _drdirsMarker (\ s a -> s{_drdirsMarker = a})

-- | -- | The response status code.
drdirsResponseStatus :: Lens' DescribeReservedDBInstancesResponse Int
drdirsResponseStatus = lens _drdirsResponseStatus (\ s a -> s{_drdirsResponseStatus = a})

instance NFData DescribeReservedDBInstancesResponse
         where
