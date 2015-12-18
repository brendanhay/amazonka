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
-- Module      : Network.AWS.RDS.DescribeDBClusterSnapshots
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about DB cluster snapshots. This API supports
-- pagination.
--
-- For more information on Amazon Aurora, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Aurora.html Aurora on Amazon RDS>
-- in the /Amazon RDS User Guide./
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeDBClusterSnapshots.html AWS API Reference> for DescribeDBClusterSnapshots.
module Network.AWS.RDS.DescribeDBClusterSnapshots
    (
    -- * Creating a Request
      describeDBClusterSnapshots
    , DescribeDBClusterSnapshots
    -- * Request Lenses
    , ddbcsDBClusterIdentifier
    , ddbcsDBClusterSnapshotIdentifier
    , ddbcsFilters
    , ddbcsSnapshotType
    , ddbcsMarker
    , ddbcsMaxRecords

    -- * Destructuring the Response
    , describeDBClusterSnapshotsResponse
    , DescribeDBClusterSnapshotsResponse
    -- * Response Lenses
    , ddbcsrsMarker
    , ddbcsrsDBClusterSnapshots
    , ddbcsrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeDBClusterSnapshots' smart constructor.
data DescribeDBClusterSnapshots = DescribeDBClusterSnapshots'
    { _ddbcsDBClusterIdentifier         :: !(Maybe Text)
    , _ddbcsDBClusterSnapshotIdentifier :: !(Maybe Text)
    , _ddbcsFilters                     :: !(Maybe [Filter])
    , _ddbcsSnapshotType                :: !(Maybe Text)
    , _ddbcsMarker                      :: !(Maybe Text)
    , _ddbcsMaxRecords                  :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeDBClusterSnapshots' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddbcsDBClusterIdentifier'
--
-- * 'ddbcsDBClusterSnapshotIdentifier'
--
-- * 'ddbcsFilters'
--
-- * 'ddbcsSnapshotType'
--
-- * 'ddbcsMarker'
--
-- * 'ddbcsMaxRecords'
describeDBClusterSnapshots
    :: DescribeDBClusterSnapshots
describeDBClusterSnapshots =
    DescribeDBClusterSnapshots'
    { _ddbcsDBClusterIdentifier = Nothing
    , _ddbcsDBClusterSnapshotIdentifier = Nothing
    , _ddbcsFilters = Nothing
    , _ddbcsSnapshotType = Nothing
    , _ddbcsMarker = Nothing
    , _ddbcsMaxRecords = Nothing
    }

-- | A DB cluster identifier to retrieve the list of DB cluster snapshots
-- for. This parameter cannot be used in conjunction with the
-- 'DBClusterSnapshotIdentifier' parameter. This parameter is not
-- case-sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
ddbcsDBClusterIdentifier :: Lens' DescribeDBClusterSnapshots (Maybe Text)
ddbcsDBClusterIdentifier = lens _ddbcsDBClusterIdentifier (\ s a -> s{_ddbcsDBClusterIdentifier = a});

-- | A specific DB cluster snapshot identifier to describe. This parameter
-- cannot be used in conjunction with the 'DBClusterIdentifier' parameter.
-- This value is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
-- -   If this is the identifier of an automated snapshot, the
--     'SnapshotType' parameter must also be specified.
ddbcsDBClusterSnapshotIdentifier :: Lens' DescribeDBClusterSnapshots (Maybe Text)
ddbcsDBClusterSnapshotIdentifier = lens _ddbcsDBClusterSnapshotIdentifier (\ s a -> s{_ddbcsDBClusterSnapshotIdentifier = a});

-- | This parameter is not currently supported.
ddbcsFilters :: Lens' DescribeDBClusterSnapshots [Filter]
ddbcsFilters = lens _ddbcsFilters (\ s a -> s{_ddbcsFilters = a}) . _Default . _Coerce;

-- | The type of DB cluster snapshots that will be returned. Values can be
-- 'automated' or 'manual'. If this parameter is not specified, the
-- returned results will include all snapshot types.
ddbcsSnapshotType :: Lens' DescribeDBClusterSnapshots (Maybe Text)
ddbcsSnapshotType = lens _ddbcsSnapshotType (\ s a -> s{_ddbcsSnapshotType = a});

-- | An optional pagination token provided by a previous
-- 'DescribeDBClusterSnapshots' request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by 'MaxRecords'.
ddbcsMarker :: Lens' DescribeDBClusterSnapshots (Maybe Text)
ddbcsMarker = lens _ddbcsMarker (\ s a -> s{_ddbcsMarker = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified 'MaxRecords' value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
ddbcsMaxRecords :: Lens' DescribeDBClusterSnapshots (Maybe Int)
ddbcsMaxRecords = lens _ddbcsMaxRecords (\ s a -> s{_ddbcsMaxRecords = a});

instance AWSRequest DescribeDBClusterSnapshots where
        type Rs DescribeDBClusterSnapshots =
             DescribeDBClusterSnapshotsResponse
        request = postQuery rDS
        response
          = receiveXMLWrapper
              "DescribeDBClusterSnapshotsResult"
              (\ s h x ->
                 DescribeDBClusterSnapshotsResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "DBClusterSnapshots" .!@ mempty >>=
                        may (parseXMLList "DBClusterSnapshot"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeDBClusterSnapshots where
        toHeaders = const mempty

instance ToPath DescribeDBClusterSnapshots where
        toPath = const "/"

instance ToQuery DescribeDBClusterSnapshots where
        toQuery DescribeDBClusterSnapshots'{..}
          = mconcat
              ["Action" =:
                 ("DescribeDBClusterSnapshots" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBClusterIdentifier" =: _ddbcsDBClusterIdentifier,
               "DBClusterSnapshotIdentifier" =:
                 _ddbcsDBClusterSnapshotIdentifier,
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _ddbcsFilters),
               "SnapshotType" =: _ddbcsSnapshotType,
               "Marker" =: _ddbcsMarker,
               "MaxRecords" =: _ddbcsMaxRecords]

-- | Provides a list of DB cluster snapshots for the user as the result of a
-- call to the DescribeDBClusterSnapshots action.
--
-- /See:/ 'describeDBClusterSnapshotsResponse' smart constructor.
data DescribeDBClusterSnapshotsResponse = DescribeDBClusterSnapshotsResponse'
    { _ddbcsrsMarker             :: !(Maybe Text)
    , _ddbcsrsDBClusterSnapshots :: !(Maybe [DBClusterSnapshot])
    , _ddbcsrsResponseStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeDBClusterSnapshotsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddbcsrsMarker'
--
-- * 'ddbcsrsDBClusterSnapshots'
--
-- * 'ddbcsrsResponseStatus'
describeDBClusterSnapshotsResponse
    :: Int -- ^ 'ddbcsrsResponseStatus'
    -> DescribeDBClusterSnapshotsResponse
describeDBClusterSnapshotsResponse pResponseStatus_ =
    DescribeDBClusterSnapshotsResponse'
    { _ddbcsrsMarker = Nothing
    , _ddbcsrsDBClusterSnapshots = Nothing
    , _ddbcsrsResponseStatus = pResponseStatus_
    }

-- | An optional pagination token provided by a previous
-- DescribeDBClusterSnapshots request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by 'MaxRecords'.
ddbcsrsMarker :: Lens' DescribeDBClusterSnapshotsResponse (Maybe Text)
ddbcsrsMarker = lens _ddbcsrsMarker (\ s a -> s{_ddbcsrsMarker = a});

-- | Provides a list of DB cluster snapshots for the user.
ddbcsrsDBClusterSnapshots :: Lens' DescribeDBClusterSnapshotsResponse [DBClusterSnapshot]
ddbcsrsDBClusterSnapshots = lens _ddbcsrsDBClusterSnapshots (\ s a -> s{_ddbcsrsDBClusterSnapshots = a}) . _Default . _Coerce;

-- | The response status code.
ddbcsrsResponseStatus :: Lens' DescribeDBClusterSnapshotsResponse Int
ddbcsrsResponseStatus = lens _ddbcsrsResponseStatus (\ s a -> s{_ddbcsrsResponseStatus = a});
