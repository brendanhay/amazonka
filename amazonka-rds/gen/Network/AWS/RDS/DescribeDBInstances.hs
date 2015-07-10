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
    , ddbiFilters
    , ddbiDBInstanceIdentifier
    , ddbiMaxRecords
    , ddbiMarker

    -- * Response
    , DescribeDBInstancesResponse
    -- ** Response constructor
    , describeDBInstancesResponse
    -- ** Response lenses
    , desDBInstances
    , desMarker
    , desStatus
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
-- * 'ddbiFilters'
--
-- * 'ddbiDBInstanceIdentifier'
--
-- * 'ddbiMaxRecords'
--
-- * 'ddbiMarker'
data DescribeDBInstances = DescribeDBInstances'
    { _ddbiFilters              :: !(Maybe [Filter])
    , _ddbiDBInstanceIdentifier :: !(Maybe Text)
    , _ddbiMaxRecords           :: !(Maybe Int)
    , _ddbiMarker               :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDBInstances' smart constructor.
describeDBInstances :: DescribeDBInstances
describeDBInstances =
    DescribeDBInstances'
    { _ddbiFilters = Nothing
    , _ddbiDBInstanceIdentifier = Nothing
    , _ddbiMaxRecords = Nothing
    , _ddbiMarker = Nothing
    }

-- | This parameter is not currently supported.
ddbiFilters :: Lens' DescribeDBInstances [Filter]
ddbiFilters = lens _ddbiFilters (\ s a -> s{_ddbiFilters = a}) . _Default;

-- | The user-supplied instance identifier. If this parameter is specified,
-- information from only the specific DB instance is returned. This
-- parameter isn\'t case sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
ddbiDBInstanceIdentifier :: Lens' DescribeDBInstances (Maybe Text)
ddbiDBInstanceIdentifier = lens _ddbiDBInstanceIdentifier (\ s a -> s{_ddbiDBInstanceIdentifier = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results may be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
ddbiMaxRecords :: Lens' DescribeDBInstances (Maybe Int)
ddbiMaxRecords = lens _ddbiMaxRecords (\ s a -> s{_ddbiMaxRecords = a});

-- | An optional pagination token provided by a previous DescribeDBInstances
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by @MaxRecords@ .
ddbiMarker :: Lens' DescribeDBInstances (Maybe Text)
ddbiMarker = lens _ddbiMarker (\ s a -> s{_ddbiMarker = a});

instance AWSPager DescribeDBInstances where
        page rq rs
          | stop (rs ^. desMarker) = Nothing
          | stop (rs ^. desDBInstances) = Nothing
          | otherwise =
            Just $ rq & ddbiMarker .~ rs ^. desMarker

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
                 toQuery (toQueryList "Filter" <$> _ddbiFilters),
               "DBInstanceIdentifier" =: _ddbiDBInstanceIdentifier,
               "MaxRecords" =: _ddbiMaxRecords,
               "Marker" =: _ddbiMarker]

-- | Contains the result of a successful invocation of the
-- DescribeDBInstances action.
--
-- /See:/ 'describeDBInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desDBInstances'
--
-- * 'desMarker'
--
-- * 'desStatus'
data DescribeDBInstancesResponse = DescribeDBInstancesResponse'
    { _desDBInstances :: !(Maybe [DBInstance])
    , _desMarker      :: !(Maybe Text)
    , _desStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDBInstancesResponse' smart constructor.
describeDBInstancesResponse :: Int -> DescribeDBInstancesResponse
describeDBInstancesResponse pStatus =
    DescribeDBInstancesResponse'
    { _desDBInstances = Nothing
    , _desMarker = Nothing
    , _desStatus = pStatus
    }

-- | A list of DBInstance instances.
desDBInstances :: Lens' DescribeDBInstancesResponse [DBInstance]
desDBInstances = lens _desDBInstances (\ s a -> s{_desDBInstances = a}) . _Default;

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@ .
desMarker :: Lens' DescribeDBInstancesResponse (Maybe Text)
desMarker = lens _desMarker (\ s a -> s{_desMarker = a});

-- | FIXME: Undocumented member.
desStatus :: Lens' DescribeDBInstancesResponse Int
desStatus = lens _desStatus (\ s a -> s{_desStatus = a});
