{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBSnapshots
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns information about DB snapshots. This API supports pagination.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeDBSnapshots.html>
module Network.AWS.RDS.DescribeDBSnapshots
    (
    -- * Request
      DescribeDBSnapshots
    -- ** Request constructor
    , describeDBSnapshots
    -- ** Request lenses
    , ddsrqFilters
    , ddsrqDBSnapshotIdentifier
    , ddsrqSnapshotType
    , ddsrqDBInstanceIdentifier
    , ddsrqMaxRecords
    , ddsrqMarker

    -- * Response
    , DescribeDBSnapshotsResponse
    -- ** Response constructor
    , describeDBSnapshotsResponse
    -- ** Response lenses
    , ddsrsMarker
    , ddsrsDBSnapshots
    , ddsrsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeDBSnapshots' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddsrqFilters'
--
-- * 'ddsrqDBSnapshotIdentifier'
--
-- * 'ddsrqSnapshotType'
--
-- * 'ddsrqDBInstanceIdentifier'
--
-- * 'ddsrqMaxRecords'
--
-- * 'ddsrqMarker'
data DescribeDBSnapshots = DescribeDBSnapshots'
    { _ddsrqFilters              :: !(Maybe [Filter])
    , _ddsrqDBSnapshotIdentifier :: !(Maybe Text)
    , _ddsrqSnapshotType         :: !(Maybe Text)
    , _ddsrqDBInstanceIdentifier :: !(Maybe Text)
    , _ddsrqMaxRecords           :: !(Maybe Int)
    , _ddsrqMarker               :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDBSnapshots' smart constructor.
describeDBSnapshots :: DescribeDBSnapshots
describeDBSnapshots =
    DescribeDBSnapshots'
    { _ddsrqFilters = Nothing
    , _ddsrqDBSnapshotIdentifier = Nothing
    , _ddsrqSnapshotType = Nothing
    , _ddsrqDBInstanceIdentifier = Nothing
    , _ddsrqMaxRecords = Nothing
    , _ddsrqMarker = Nothing
    }

-- | This parameter is not currently supported.
ddsrqFilters :: Lens' DescribeDBSnapshots [Filter]
ddsrqFilters = lens _ddsrqFilters (\ s a -> s{_ddsrqFilters = a}) . _Default;

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
ddsrqDBSnapshotIdentifier :: Lens' DescribeDBSnapshots (Maybe Text)
ddsrqDBSnapshotIdentifier = lens _ddsrqDBSnapshotIdentifier (\ s a -> s{_ddsrqDBSnapshotIdentifier = a});

-- | The type of snapshots that will be returned. Values can be \"automated\"
-- or \"manual.\" If not specified, the returned results will include all
-- snapshots types.
ddsrqSnapshotType :: Lens' DescribeDBSnapshots (Maybe Text)
ddsrqSnapshotType = lens _ddsrqSnapshotType (\ s a -> s{_ddsrqSnapshotType = a});

-- | A DB instance identifier to retrieve the list of DB snapshots for.
-- Cannot be used in conjunction with @DBSnapshotIdentifier@. This
-- parameter is not case sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
ddsrqDBInstanceIdentifier :: Lens' DescribeDBSnapshots (Maybe Text)
ddsrqDBInstanceIdentifier = lens _ddsrqDBInstanceIdentifier (\ s a -> s{_ddsrqDBInstanceIdentifier = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results may be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
ddsrqMaxRecords :: Lens' DescribeDBSnapshots (Maybe Int)
ddsrqMaxRecords = lens _ddsrqMaxRecords (\ s a -> s{_ddsrqMaxRecords = a});

-- | An optional pagination token provided by a previous
-- @DescribeDBSnapshots@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
ddsrqMarker :: Lens' DescribeDBSnapshots (Maybe Text)
ddsrqMarker = lens _ddsrqMarker (\ s a -> s{_ddsrqMarker = a});

instance AWSPager DescribeDBSnapshots where
        page rq rs
          | stop (rs ^. ddsrsMarker) = Nothing
          | stop (rs ^. ddsrsDBSnapshots) = Nothing
          | otherwise =
            Just $ rq & ddsrqMarker .~ rs ^. ddsrsMarker

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
                 toQuery (toQueryList "Filter" <$> _ddsrqFilters),
               "DBSnapshotIdentifier" =: _ddsrqDBSnapshotIdentifier,
               "SnapshotType" =: _ddsrqSnapshotType,
               "DBInstanceIdentifier" =: _ddsrqDBInstanceIdentifier,
               "MaxRecords" =: _ddsrqMaxRecords,
               "Marker" =: _ddsrqMarker]

-- | Contains the result of a successful invocation of the
-- DescribeDBSnapshots action.
--
-- /See:/ 'describeDBSnapshotsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddsrsMarker'
--
-- * 'ddsrsDBSnapshots'
--
-- * 'ddsrsStatus'
data DescribeDBSnapshotsResponse = DescribeDBSnapshotsResponse'
    { _ddsrsMarker      :: !(Maybe Text)
    , _ddsrsDBSnapshots :: !(Maybe [DBSnapshot])
    , _ddsrsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDBSnapshotsResponse' smart constructor.
describeDBSnapshotsResponse :: Int -> DescribeDBSnapshotsResponse
describeDBSnapshotsResponse pStatus =
    DescribeDBSnapshotsResponse'
    { _ddsrsMarker = Nothing
    , _ddsrsDBSnapshots = Nothing
    , _ddsrsStatus = pStatus
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
ddsrsMarker :: Lens' DescribeDBSnapshotsResponse (Maybe Text)
ddsrsMarker = lens _ddsrsMarker (\ s a -> s{_ddsrsMarker = a});

-- | A list of DBSnapshot instances.
ddsrsDBSnapshots :: Lens' DescribeDBSnapshotsResponse [DBSnapshot]
ddsrsDBSnapshots = lens _ddsrsDBSnapshots (\ s a -> s{_ddsrsDBSnapshots = a}) . _Default;

-- | FIXME: Undocumented member.
ddsrsStatus :: Lens' DescribeDBSnapshotsResponse Int
ddsrsStatus = lens _ddsrsStatus (\ s a -> s{_ddsrsStatus = a});
