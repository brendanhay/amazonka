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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about provisioned RDS instances. This API supports
-- pagination.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeDBInstances.html AWS API Reference> for DescribeDBInstances.
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
    , ddbiMaxRecords
    , ddbiMarker

    -- * Destructuring the Response
    , describeDBInstancesResponse
    , DescribeDBInstancesResponse
    -- * Response Lenses
    , ddbirsDBInstances
    , ddbirsMarker
    , ddbirsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeDBInstances' smart constructor.
data DescribeDBInstances = DescribeDBInstances'
    { _ddbiFilters              :: !(Maybe [Filter])
    , _ddbiDBInstanceIdentifier :: !(Maybe Text)
    , _ddbiMaxRecords           :: !(Maybe Int)
    , _ddbiMarker               :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeDBInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddbiFilters'
--
-- * 'ddbiDBInstanceIdentifier'
--
-- * 'ddbiMaxRecords'
--
-- * 'ddbiMarker'
describeDBInstances
    :: DescribeDBInstances
describeDBInstances =
    DescribeDBInstances'
    { _ddbiFilters = Nothing
    , _ddbiDBInstanceIdentifier = Nothing
    , _ddbiMaxRecords = Nothing
    , _ddbiMarker = Nothing
    }

-- | This parameter is not currently supported.
ddbiFilters :: Lens' DescribeDBInstances [Filter]
ddbiFilters = lens _ddbiFilters (\ s a -> s{_ddbiFilters = a}) . _Default . _Coerce;

-- | The user-supplied instance identifier. If this parameter is specified,
-- information from only the specific DB instance is returned. This
-- parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
ddbiDBInstanceIdentifier :: Lens' DescribeDBInstances (Maybe Text)
ddbiDBInstanceIdentifier = lens _ddbiDBInstanceIdentifier (\ s a -> s{_ddbiDBInstanceIdentifier = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified 'MaxRecords' value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
ddbiMaxRecords :: Lens' DescribeDBInstances (Maybe Int)
ddbiMaxRecords = lens _ddbiMaxRecords (\ s a -> s{_ddbiMaxRecords = a});

-- | An optional pagination token provided by a previous
-- 'DescribeDBInstances' request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by 'MaxRecords'.
ddbiMarker :: Lens' DescribeDBInstances (Maybe Text)
ddbiMarker = lens _ddbiMarker (\ s a -> s{_ddbiMarker = a});

instance AWSPager DescribeDBInstances where
        page rq rs
          | stop (rs ^. ddbirsMarker) = Nothing
          | stop (rs ^. ddbirsDBInstances) = Nothing
          | otherwise =
            Just $ rq & ddbiMarker .~ rs ^. ddbirsMarker

instance AWSRequest DescribeDBInstances where
        type Rs DescribeDBInstances =
             DescribeDBInstancesResponse
        request = postQuery rDS
        response
          = receiveXMLWrapper "DescribeDBInstancesResult"
              (\ s h x ->
                 DescribeDBInstancesResponse' <$>
                   (x .@? "DBInstances" .!@ mempty >>=
                      may (parseXMLList "DBInstance"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

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
               "MaxRecords" =: _ddbiMaxRecords,
               "Marker" =: _ddbiMarker]

-- | Contains the result of a successful invocation of the
-- DescribeDBInstances action.
--
-- /See:/ 'describeDBInstancesResponse' smart constructor.
data DescribeDBInstancesResponse = DescribeDBInstancesResponse'
    { _ddbirsDBInstances :: !(Maybe [DBInstance])
    , _ddbirsMarker      :: !(Maybe Text)
    , _ddbirsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeDBInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddbirsDBInstances'
--
-- * 'ddbirsMarker'
--
-- * 'ddbirsStatus'
describeDBInstancesResponse
    :: Int -- ^ 'ddbirsStatus'
    -> DescribeDBInstancesResponse
describeDBInstancesResponse pStatus_ =
    DescribeDBInstancesResponse'
    { _ddbirsDBInstances = Nothing
    , _ddbirsMarker = Nothing
    , _ddbirsStatus = pStatus_
    }

-- | A list of DBInstance instances.
ddbirsDBInstances :: Lens' DescribeDBInstancesResponse [DBInstance]
ddbirsDBInstances = lens _ddbirsDBInstances (\ s a -> s{_ddbirsDBInstances = a}) . _Default . _Coerce;

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by 'MaxRecords' .
ddbirsMarker :: Lens' DescribeDBInstancesResponse (Maybe Text)
ddbirsMarker = lens _ddbirsMarker (\ s a -> s{_ddbirsMarker = a});

-- | The response status code.
ddbirsStatus :: Lens' DescribeDBInstancesResponse Int
ddbirsStatus = lens _ddbirsStatus (\ s a -> s{_ddbirsStatus = a});
