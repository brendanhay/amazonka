{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns information about provisioned RDS instances. This API supports
-- pagination.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeDBInstances.html>
module Network.AWS.RDS.DescribeDBInstances
    (
    -- * Request
      DescribeDBInstances
    -- ** Request constructor
    , describeDBInstances
    -- ** Request lenses
    , ddbirqFilters
    , ddbirqDBInstanceIdentifier
    , ddbirqMaxRecords
    , ddbirqMarker

    -- * Response
    , DescribeDBInstancesResponse
    -- ** Response constructor
    , describeDBInstancesResponse
    -- ** Response lenses
    , ddbirsDBInstances
    , ddbirsMarker
    , ddbirsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeDBInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbirqFilters'
--
-- * 'ddbirqDBInstanceIdentifier'
--
-- * 'ddbirqMaxRecords'
--
-- * 'ddbirqMarker'
data DescribeDBInstances = DescribeDBInstances'
    { _ddbirqFilters              :: !(Maybe [Filter])
    , _ddbirqDBInstanceIdentifier :: !(Maybe Text)
    , _ddbirqMaxRecords           :: !(Maybe Int)
    , _ddbirqMarker               :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDBInstances' smart constructor.
describeDBInstances :: DescribeDBInstances
describeDBInstances =
    DescribeDBInstances'
    { _ddbirqFilters = Nothing
    , _ddbirqDBInstanceIdentifier = Nothing
    , _ddbirqMaxRecords = Nothing
    , _ddbirqMarker = Nothing
    }

-- | This parameter is not currently supported.
ddbirqFilters :: Lens' DescribeDBInstances [Filter]
ddbirqFilters = lens _ddbirqFilters (\ s a -> s{_ddbirqFilters = a}) . _Default;

-- | The user-supplied instance identifier. If this parameter is specified,
-- information from only the specific DB instance is returned. This
-- parameter isn\'t case sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
ddbirqDBInstanceIdentifier :: Lens' DescribeDBInstances (Maybe Text)
ddbirqDBInstanceIdentifier = lens _ddbirqDBInstanceIdentifier (\ s a -> s{_ddbirqDBInstanceIdentifier = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results may be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
ddbirqMaxRecords :: Lens' DescribeDBInstances (Maybe Int)
ddbirqMaxRecords = lens _ddbirqMaxRecords (\ s a -> s{_ddbirqMaxRecords = a});

-- | An optional pagination token provided by a previous DescribeDBInstances
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by @MaxRecords@ .
ddbirqMarker :: Lens' DescribeDBInstances (Maybe Text)
ddbirqMarker = lens _ddbirqMarker (\ s a -> s{_ddbirqMarker = a});

instance AWSPager DescribeDBInstances where
        page rq rs
          | stop (rs ^. ddbirsMarker) = Nothing
          | stop (rs ^. ddbirsDBInstances) = Nothing
          | otherwise =
            Just $ rq & ddbirqMarker .~ rs ^. ddbirsMarker

instance AWSRequest DescribeDBInstances where
        type Sv DescribeDBInstances = RDS
        type Rs DescribeDBInstances =
             DescribeDBInstancesResponse
        request = post
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
                 toQuery (toQueryList "Filter" <$> _ddbirqFilters),
               "DBInstanceIdentifier" =:
                 _ddbirqDBInstanceIdentifier,
               "MaxRecords" =: _ddbirqMaxRecords,
               "Marker" =: _ddbirqMarker]

-- | Contains the result of a successful invocation of the
-- DescribeDBInstances action.
--
-- /See:/ 'describeDBInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbirsDBInstances'
--
-- * 'ddbirsMarker'
--
-- * 'ddbirsStatus'
data DescribeDBInstancesResponse = DescribeDBInstancesResponse'
    { _ddbirsDBInstances :: !(Maybe [DBInstance])
    , _ddbirsMarker      :: !(Maybe Text)
    , _ddbirsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDBInstancesResponse' smart constructor.
describeDBInstancesResponse :: Int -> DescribeDBInstancesResponse
describeDBInstancesResponse pStatus_ =
    DescribeDBInstancesResponse'
    { _ddbirsDBInstances = Nothing
    , _ddbirsMarker = Nothing
    , _ddbirsStatus = pStatus_
    }

-- | A list of DBInstance instances.
ddbirsDBInstances :: Lens' DescribeDBInstancesResponse [DBInstance]
ddbirsDBInstances = lens _ddbirsDBInstances (\ s a -> s{_ddbirsDBInstances = a}) . _Default;

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@ .
ddbirsMarker :: Lens' DescribeDBInstancesResponse (Maybe Text)
ddbirsMarker = lens _ddbirsMarker (\ s a -> s{_ddbirsMarker = a});

-- | FIXME: Undocumented member.
ddbirsStatus :: Lens' DescribeDBInstancesResponse Int
ddbirsStatus = lens _ddbirsStatus (\ s a -> s{_ddbirsStatus = a});
