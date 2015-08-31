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
-- Module      : Network.AWS.RDS.DescribeDBSnapshots
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about DB snapshots. This API supports pagination.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeDBSnapshots.html AWS API Reference> for DescribeDBSnapshots.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBSnapshots
    (
    -- * Creating a Request
      describeDBSnapshots
    , DescribeDBSnapshots
    -- * Request Lenses
    , ddsFilters
    , ddsDBSnapshotIdentifier
    , ddsSnapshotType
    , ddsDBInstanceIdentifier
    , ddsMarker
    , ddsMaxRecords

    -- * Destructuring the Response
    , describeDBSnapshotsResponse
    , DescribeDBSnapshotsResponse
    -- * Response Lenses
    , ddsrsMarker
    , ddsrsDBSnapshots
    , ddsrsResponseStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeDBSnapshots' smart constructor.
data DescribeDBSnapshots = DescribeDBSnapshots'
    { _ddsFilters              :: !(Maybe [Filter])
    , _ddsDBSnapshotIdentifier :: !(Maybe Text)
    , _ddsSnapshotType         :: !(Maybe Text)
    , _ddsDBInstanceIdentifier :: !(Maybe Text)
    , _ddsMarker               :: !(Maybe Text)
    , _ddsMaxRecords           :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeDBSnapshots' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddsFilters'
--
-- * 'ddsDBSnapshotIdentifier'
--
-- * 'ddsSnapshotType'
--
-- * 'ddsDBInstanceIdentifier'
--
-- * 'ddsMarker'
--
-- * 'ddsMaxRecords'
describeDBSnapshots
    :: DescribeDBSnapshots
describeDBSnapshots =
    DescribeDBSnapshots'
    { _ddsFilters = Nothing
    , _ddsDBSnapshotIdentifier = Nothing
    , _ddsSnapshotType = Nothing
    , _ddsDBInstanceIdentifier = Nothing
    , _ddsMarker = Nothing
    , _ddsMaxRecords = Nothing
    }

-- | This parameter is not currently supported.
ddsFilters :: Lens' DescribeDBSnapshots [Filter]
ddsFilters = lens _ddsFilters (\ s a -> s{_ddsFilters = a}) . _Default . _Coerce;

-- | A specific DB snapshot identifier to describe. This parameter cannot be
-- used in conjunction with 'DBInstanceIdentifier'. This value is stored as
-- a lowercase string.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters.
-- -   First character must be a letter.
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
-- -   If this is the identifier of an automated snapshot, the
--     'SnapshotType' parameter must also be specified.
ddsDBSnapshotIdentifier :: Lens' DescribeDBSnapshots (Maybe Text)
ddsDBSnapshotIdentifier = lens _ddsDBSnapshotIdentifier (\ s a -> s{_ddsDBSnapshotIdentifier = a});

-- | The type of snapshots that will be returned. Values can be \"automated\"
-- or \"manual.\" If not specified, the returned results will include all
-- snapshots types.
ddsSnapshotType :: Lens' DescribeDBSnapshots (Maybe Text)
ddsSnapshotType = lens _ddsSnapshotType (\ s a -> s{_ddsSnapshotType = a});

-- | A DB instance identifier to retrieve the list of DB snapshots for. This
-- parameter cannot be used in conjunction with 'DBSnapshotIdentifier'.
-- This parameter is not case-sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
ddsDBInstanceIdentifier :: Lens' DescribeDBSnapshots (Maybe Text)
ddsDBInstanceIdentifier = lens _ddsDBInstanceIdentifier (\ s a -> s{_ddsDBInstanceIdentifier = a});

-- | An optional pagination token provided by a previous
-- 'DescribeDBSnapshots' request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by 'MaxRecords'.
ddsMarker :: Lens' DescribeDBSnapshots (Maybe Text)
ddsMarker = lens _ddsMarker (\ s a -> s{_ddsMarker = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified 'MaxRecords' value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
ddsMaxRecords :: Lens' DescribeDBSnapshots (Maybe Int)
ddsMaxRecords = lens _ddsMaxRecords (\ s a -> s{_ddsMaxRecords = a});

instance AWSPager DescribeDBSnapshots where
        page rq rs
          | stop (rs ^. ddsrsMarker) = Nothing
          | stop (rs ^. ddsrsDBSnapshots) = Nothing
          | otherwise =
            Just $ rq & ddsMarker .~ rs ^. ddsrsMarker

instance AWSRequest DescribeDBSnapshots where
        type Rs DescribeDBSnapshots =
             DescribeDBSnapshotsResponse
        request = postQuery rDS
        response
          = receiveXMLWrapper "DescribeDBSnapshotsResult"
              (\ s h x ->
                 DescribeDBSnapshotsResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "DBSnapshots" .!@ mempty >>=
                        may (parseXMLList "DBSnapshot"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeDBSnapshots where
        toHeaders = const mempty

instance ToPath DescribeDBSnapshots where
        toPath = const "/"

instance ToQuery DescribeDBSnapshots where
        toQuery DescribeDBSnapshots'{..}
          = mconcat
              ["Action" =: ("DescribeDBSnapshots" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _ddsFilters),
               "DBSnapshotIdentifier" =: _ddsDBSnapshotIdentifier,
               "SnapshotType" =: _ddsSnapshotType,
               "DBInstanceIdentifier" =: _ddsDBInstanceIdentifier,
               "Marker" =: _ddsMarker,
               "MaxRecords" =: _ddsMaxRecords]

-- | Contains the result of a successful invocation of the
-- DescribeDBSnapshots action.
--
-- /See:/ 'describeDBSnapshotsResponse' smart constructor.
data DescribeDBSnapshotsResponse = DescribeDBSnapshotsResponse'
    { _ddsrsMarker         :: !(Maybe Text)
    , _ddsrsDBSnapshots    :: !(Maybe [DBSnapshot])
    , _ddsrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeDBSnapshotsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddsrsMarker'
--
-- * 'ddsrsDBSnapshots'
--
-- * 'ddsrsResponseStatus'
describeDBSnapshotsResponse
    :: Int -- ^ 'ddsrsResponseStatus'
    -> DescribeDBSnapshotsResponse
describeDBSnapshotsResponse pResponseStatus_ =
    DescribeDBSnapshotsResponse'
    { _ddsrsMarker = Nothing
    , _ddsrsDBSnapshots = Nothing
    , _ddsrsResponseStatus = pResponseStatus_
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by 'MaxRecords'.
ddsrsMarker :: Lens' DescribeDBSnapshotsResponse (Maybe Text)
ddsrsMarker = lens _ddsrsMarker (\ s a -> s{_ddsrsMarker = a});

-- | A list of DBSnapshot instances.
ddsrsDBSnapshots :: Lens' DescribeDBSnapshotsResponse [DBSnapshot]
ddsrsDBSnapshots = lens _ddsrsDBSnapshots (\ s a -> s{_ddsrsDBSnapshots = a}) . _Default . _Coerce;

-- | The response status code.
ddsrsResponseStatus :: Lens' DescribeDBSnapshotsResponse Int
ddsrsResponseStatus = lens _ddsrsResponseStatus (\ s a -> s{_ddsrsResponseStatus = a});
