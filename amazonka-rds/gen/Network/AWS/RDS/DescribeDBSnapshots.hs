{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.RDS.DescribeDBSnapshots
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns information about DB snapshots. This API supports pagination.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeDBSnapshots.html>
module Network.AWS.RDS.DescribeDBSnapshots
    (
    -- * Request
      DescribeDBSnapshots
    -- ** Request constructor
    , describeDBSnapshots
    -- ** Request lenses
    , ddsFilters
    , ddsDBSnapshotIdentifier
    , ddsSnapshotType
    , ddsDBInstanceIdentifier
    , ddsMaxRecords
    , ddsMarker

    -- * Response
    , DescribeDBSnapshotsResponse
    -- ** Response constructor
    , describeDBSnapshotsResponse
    -- ** Response lenses
    , ddsrMarker
    , ddsrDBSnapshots
    ) where

import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDBSnapshots' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddsFilters'
--
-- * 'ddsDBSnapshotIdentifier'
--
-- * 'ddsSnapshotType'
--
-- * 'ddsDBInstanceIdentifier'
--
-- * 'ddsMaxRecords'
--
-- * 'ddsMarker'
data DescribeDBSnapshots = DescribeDBSnapshots'{_ddsFilters :: Maybe [Filter], _ddsDBSnapshotIdentifier :: Maybe Text, _ddsSnapshotType :: Maybe Text, _ddsDBInstanceIdentifier :: Maybe Text, _ddsMaxRecords :: Maybe Int, _ddsMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeDBSnapshots' smart constructor.
describeDBSnapshots :: DescribeDBSnapshots
describeDBSnapshots = DescribeDBSnapshots'{_ddsFilters = Nothing, _ddsDBSnapshotIdentifier = Nothing, _ddsSnapshotType = Nothing, _ddsDBInstanceIdentifier = Nothing, _ddsMaxRecords = Nothing, _ddsMarker = Nothing};

-- | This parameter is not currently supported.
ddsFilters :: Lens' DescribeDBSnapshots [Filter]
ddsFilters = lens _ddsFilters (\ s a -> s{_ddsFilters = a}) . _Default;

-- | A specific DB snapshot identifier to describe. Cannot be used in
-- conjunction with @DBInstanceIdentifier@. This value is stored as a
-- lowercase string.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
-- -   If this is the identifier of an automated snapshot, the
--     @SnapshotType@ parameter must also be specified.
ddsDBSnapshotIdentifier :: Lens' DescribeDBSnapshots (Maybe Text)
ddsDBSnapshotIdentifier = lens _ddsDBSnapshotIdentifier (\ s a -> s{_ddsDBSnapshotIdentifier = a});

-- | The type of snapshots that will be returned. Values can be \"automated\"
-- or \"manual.\" If not specified, the returned results will include all
-- snapshots types.
ddsSnapshotType :: Lens' DescribeDBSnapshots (Maybe Text)
ddsSnapshotType = lens _ddsSnapshotType (\ s a -> s{_ddsSnapshotType = a});

-- | A DB instance identifier to retrieve the list of DB snapshots for.
-- Cannot be used in conjunction with @DBSnapshotIdentifier@. This
-- parameter is not case sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
ddsDBInstanceIdentifier :: Lens' DescribeDBSnapshots (Maybe Text)
ddsDBInstanceIdentifier = lens _ddsDBInstanceIdentifier (\ s a -> s{_ddsDBInstanceIdentifier = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results may be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
ddsMaxRecords :: Lens' DescribeDBSnapshots (Maybe Int)
ddsMaxRecords = lens _ddsMaxRecords (\ s a -> s{_ddsMaxRecords = a});

-- | An optional pagination token provided by a previous
-- @DescribeDBSnapshots@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
ddsMarker :: Lens' DescribeDBSnapshots (Maybe Text)
ddsMarker = lens _ddsMarker (\ s a -> s{_ddsMarker = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DescribeDBSnapshots where
        type Sv DescribeDBSnapshots = RDS
        type Rs DescribeDBSnapshots =
             DescribeDBSnapshotsResponse
        request = post
        response
          = receiveXMLWrapper "DescribeDBSnapshotsResult"
              (\ s h x ->
                 DescribeDBSnapshotsResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "DBSnapshots" .!@ mempty >>=
                        may (parseXMLList "DBSnapshot")))

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
               "MaxRecords" =: _ddsMaxRecords,
               "Marker" =: _ddsMarker]

-- | /See:/ 'describeDBSnapshotsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddsrMarker'
--
-- * 'ddsrDBSnapshots'
data DescribeDBSnapshotsResponse = DescribeDBSnapshotsResponse'{_ddsrMarker :: Maybe Text, _ddsrDBSnapshots :: Maybe [DBSnapshot]} deriving (Eq, Read, Show)

-- | 'DescribeDBSnapshotsResponse' smart constructor.
describeDBSnapshotsResponse :: DescribeDBSnapshotsResponse
describeDBSnapshotsResponse = DescribeDBSnapshotsResponse'{_ddsrMarker = Nothing, _ddsrDBSnapshots = Nothing};

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
ddsrMarker :: Lens' DescribeDBSnapshotsResponse (Maybe Text)
ddsrMarker = lens _ddsrMarker (\ s a -> s{_ddsrMarker = a});

-- | A list of DBSnapshot instances.
ddsrDBSnapshots :: Lens' DescribeDBSnapshotsResponse [DBSnapshot]
ddsrDBSnapshots = lens _ddsrDBSnapshots (\ s a -> s{_ddsrDBSnapshots = a}) . _Default;
