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
-- Module      : Network.AWS.RDS.DescribeDBInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about provisioned RDS instances. This API supports pagination.
--
--
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBInstances
    (
    -- * Creating a Request
      describeDBInstances
    , DescribeDBInstances
    -- * Request Lenses
    , ddbiFilters
    , ddbiDBInstanceIdentifier
    , ddbiMarker
    , ddbiMaxRecords

    -- * Destructuring the Response
    , describeDBInstancesResponse
    , DescribeDBInstancesResponse
    -- * Response Lenses
    , ddbirsDBInstances
    , ddbirsMarker
    , ddbirsResponseStatus
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
-- /See:/ 'describeDBInstances' smart constructor.
data DescribeDBInstances = DescribeDBInstances'
  { _ddbiFilters              :: !(Maybe [Filter])
  , _ddbiDBInstanceIdentifier :: !(Maybe Text)
  , _ddbiMarker               :: !(Maybe Text)
  , _ddbiMaxRecords           :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDBInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddbiFilters' - A filter that specifies one or more DB instances to describe. Supported filters:     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list will only include information about the DB instances associated with the DB clusters identified by these ARNs.     * @db-instance-id@ - Accepts DB instance identifiers and DB instance Amazon Resource Names (ARNs). The results list will only include information about the DB instances identified by these ARNs.
--
-- * 'ddbiDBInstanceIdentifier' - The user-supplied instance identifier. If this parameter is specified, information from only the specific DB instance is returned. This parameter isn't case-sensitive. Constraints:     * If supplied, must match the identifier of an existing DBInstance.
--
-- * 'ddbiMarker' - An optional pagination token provided by a previous @DescribeDBInstances@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddbiMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
describeDBInstances
    :: DescribeDBInstances
describeDBInstances =
  DescribeDBInstances'
    { _ddbiFilters = Nothing
    , _ddbiDBInstanceIdentifier = Nothing
    , _ddbiMarker = Nothing
    , _ddbiMaxRecords = Nothing
    }


-- | A filter that specifies one or more DB instances to describe. Supported filters:     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list will only include information about the DB instances associated with the DB clusters identified by these ARNs.     * @db-instance-id@ - Accepts DB instance identifiers and DB instance Amazon Resource Names (ARNs). The results list will only include information about the DB instances identified by these ARNs.
ddbiFilters :: Lens' DescribeDBInstances [Filter]
ddbiFilters = lens _ddbiFilters (\ s a -> s{_ddbiFilters = a}) . _Default . _Coerce

-- | The user-supplied instance identifier. If this parameter is specified, information from only the specific DB instance is returned. This parameter isn't case-sensitive. Constraints:     * If supplied, must match the identifier of an existing DBInstance.
ddbiDBInstanceIdentifier :: Lens' DescribeDBInstances (Maybe Text)
ddbiDBInstanceIdentifier = lens _ddbiDBInstanceIdentifier (\ s a -> s{_ddbiDBInstanceIdentifier = a})

-- | An optional pagination token provided by a previous @DescribeDBInstances@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddbiMarker :: Lens' DescribeDBInstances (Maybe Text)
ddbiMarker = lens _ddbiMarker (\ s a -> s{_ddbiMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
ddbiMaxRecords :: Lens' DescribeDBInstances (Maybe Int)
ddbiMaxRecords = lens _ddbiMaxRecords (\ s a -> s{_ddbiMaxRecords = a})

instance AWSPager DescribeDBInstances where
        page rq rs
          | stop (rs ^. ddbirsMarker) = Nothing
          | stop (rs ^. ddbirsDBInstances) = Nothing
          | otherwise =
            Just $ rq & ddbiMarker .~ rs ^. ddbirsMarker

instance AWSRequest DescribeDBInstances where
        type Rs DescribeDBInstances =
             DescribeDBInstancesResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "DescribeDBInstancesResult"
              (\ s h x ->
                 DescribeDBInstancesResponse' <$>
                   (x .@? "DBInstances" .!@ mempty >>=
                      may (parseXMLList "DBInstance"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeDBInstances where

instance NFData DescribeDBInstances where

instance ToHeaders DescribeDBInstances where
        toHeaders = const mempty

instance ToPath DescribeDBInstances where
        toPath = const "/"

instance ToQuery DescribeDBInstances where
        toQuery DescribeDBInstances'{..}
          = mconcat
              ["Action" =: ("DescribeDBInstances" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _ddbiFilters),
               "DBInstanceIdentifier" =: _ddbiDBInstanceIdentifier,
               "Marker" =: _ddbiMarker,
               "MaxRecords" =: _ddbiMaxRecords]

-- | Contains the result of a successful invocation of the 'DescribeDBInstances' action.
--
--
--
-- /See:/ 'describeDBInstancesResponse' smart constructor.
data DescribeDBInstancesResponse = DescribeDBInstancesResponse'
  { _ddbirsDBInstances    :: !(Maybe [DBInstance])
  , _ddbirsMarker         :: !(Maybe Text)
  , _ddbirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDBInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddbirsDBInstances' - A list of 'DBInstance' instances.
--
-- * 'ddbirsMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddbirsResponseStatus' - -- | The response status code.
describeDBInstancesResponse
    :: Int -- ^ 'ddbirsResponseStatus'
    -> DescribeDBInstancesResponse
describeDBInstancesResponse pResponseStatus_ =
  DescribeDBInstancesResponse'
    { _ddbirsDBInstances = Nothing
    , _ddbirsMarker = Nothing
    , _ddbirsResponseStatus = pResponseStatus_
    }


-- | A list of 'DBInstance' instances.
ddbirsDBInstances :: Lens' DescribeDBInstancesResponse [DBInstance]
ddbirsDBInstances = lens _ddbirsDBInstances (\ s a -> s{_ddbirsDBInstances = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddbirsMarker :: Lens' DescribeDBInstancesResponse (Maybe Text)
ddbirsMarker = lens _ddbirsMarker (\ s a -> s{_ddbirsMarker = a})

-- | -- | The response status code.
ddbirsResponseStatus :: Lens' DescribeDBInstancesResponse Int
ddbirsResponseStatus = lens _ddbirsResponseStatus (\ s a -> s{_ddbirsResponseStatus = a})

instance NFData DescribeDBInstancesResponse where
