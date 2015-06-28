{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Redshift.DescribeClusterSnapshots
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

-- | Returns one or more snapshot objects, which contain metadata about your
-- cluster snapshots. By default, this operation returns information about
-- all snapshots of all clusters that are owned by you AWS customer
-- account. No information is returned for snapshots owned by inactive AWS
-- customer accounts.
--
-- If you specify both tag keys and tag values in the same request, Amazon
-- Redshift returns all snapshots that match any combination of the
-- specified keys and values. For example, if you have @owner@ and
-- @environment@ for tag keys, and @admin@ and @test@ for tag values, all
-- snapshots that have any combination of those values are returned. Only
-- snapshots that you own are returned in the response; shared snapshots
-- are not returned with the tag key and tag value request parameters.
--
-- If both tag keys and values are omitted from the request, snapshots are
-- returned regardless of whether they have tag keys or values associated
-- with them.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeClusterSnapshots.html>
module Network.AWS.Redshift.DescribeClusterSnapshots
    (
    -- * Request
      DescribeClusterSnapshots
    -- ** Request constructor
    , describeClusterSnapshots
    -- ** Request lenses
    , desSnapshotIdentifier
    , desTagValues
    , desStartTime
    , desTagKeys
    , desClusterIdentifier
    , desSnapshotType
    , desMaxRecords
    , desEndTime
    , desMarker
    , desOwnerAccount

    -- * Response
    , DescribeClusterSnapshotsResponse
    -- ** Response constructor
    , describeClusterSnapshotsResponse
    -- ** Response lenses
    , descSnapshots
    , descMarker
    , descStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeClusterSnapshots' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desSnapshotIdentifier'
--
-- * 'desTagValues'
--
-- * 'desStartTime'
--
-- * 'desTagKeys'
--
-- * 'desClusterIdentifier'
--
-- * 'desSnapshotType'
--
-- * 'desMaxRecords'
--
-- * 'desEndTime'
--
-- * 'desMarker'
--
-- * 'desOwnerAccount'
data DescribeClusterSnapshots = DescribeClusterSnapshots'
    { _desSnapshotIdentifier :: !(Maybe Text)
    , _desTagValues          :: !(Maybe [Text])
    , _desStartTime          :: !(Maybe ISO8601)
    , _desTagKeys            :: !(Maybe [Text])
    , _desClusterIdentifier  :: !(Maybe Text)
    , _desSnapshotType       :: !(Maybe Text)
    , _desMaxRecords         :: !(Maybe Int)
    , _desEndTime            :: !(Maybe ISO8601)
    , _desMarker             :: !(Maybe Text)
    , _desOwnerAccount       :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'DescribeClusterSnapshots' smart constructor.
describeClusterSnapshots :: DescribeClusterSnapshots
describeClusterSnapshots =
    DescribeClusterSnapshots'
    { _desSnapshotIdentifier = Nothing
    , _desTagValues = Nothing
    , _desStartTime = Nothing
    , _desTagKeys = Nothing
    , _desClusterIdentifier = Nothing
    , _desSnapshotType = Nothing
    , _desMaxRecords = Nothing
    , _desEndTime = Nothing
    , _desMarker = Nothing
    , _desOwnerAccount = Nothing
    }

-- | The snapshot identifier of the snapshot about which to return
-- information.
desSnapshotIdentifier :: Lens' DescribeClusterSnapshots (Maybe Text)
desSnapshotIdentifier = lens _desSnapshotIdentifier (\ s a -> s{_desSnapshotIdentifier = a});

-- | A tag value or values for which you want to return all matching cluster
-- snapshots that are associated with the specified tag value or values.
-- For example, suppose that you have snapshots that are tagged with values
-- called @admin@ and @test@. If you specify both of these tag values in
-- the request, Amazon Redshift returns a response with the snapshots that
-- have either or both of these tag values associated with them.
desTagValues :: Lens' DescribeClusterSnapshots [Text]
desTagValues = lens _desTagValues (\ s a -> s{_desTagValues = a}) . _Default;

-- | A value that requests only snapshots created at or after the specified
-- time. The time value is specified in ISO 8601 format. For more
-- information about ISO 8601, go to the
-- <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: @2012-07-16T18:00:00Z@
desStartTime :: Lens' DescribeClusterSnapshots (Maybe UTCTime)
desStartTime = lens _desStartTime (\ s a -> s{_desStartTime = a}) . mapping _Time;

-- | A tag key or keys for which you want to return all matching cluster
-- snapshots that are associated with the specified key or keys. For
-- example, suppose that you have snapshots that are tagged with keys
-- called @owner@ and @environment@. If you specify both of these tag keys
-- in the request, Amazon Redshift returns a response with the snapshots
-- that have either or both of these tag keys associated with them.
desTagKeys :: Lens' DescribeClusterSnapshots [Text]
desTagKeys = lens _desTagKeys (\ s a -> s{_desTagKeys = a}) . _Default;

-- | The identifier of the cluster for which information about snapshots is
-- requested.
desClusterIdentifier :: Lens' DescribeClusterSnapshots (Maybe Text)
desClusterIdentifier = lens _desClusterIdentifier (\ s a -> s{_desClusterIdentifier = a});

-- | The type of snapshots for which you are requesting information. By
-- default, snapshots of all types are returned.
--
-- Valid Values: @automated@ | @manual@
desSnapshotType :: Lens' DescribeClusterSnapshots (Maybe Text)
desSnapshotType = lens _desSnapshotType (\ s a -> s{_desSnapshotType = a});

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
desMaxRecords :: Lens' DescribeClusterSnapshots (Maybe Int)
desMaxRecords = lens _desMaxRecords (\ s a -> s{_desMaxRecords = a});

-- | A time value that requests only snapshots created at or before the
-- specified time. The time value is specified in ISO 8601 format. For more
-- information about ISO 8601, go to the
-- <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: @2012-07-16T18:00:00Z@
desEndTime :: Lens' DescribeClusterSnapshots (Maybe UTCTime)
desEndTime = lens _desEndTime (\ s a -> s{_desEndTime = a}) . mapping _Time;

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterSnapshots
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
desMarker :: Lens' DescribeClusterSnapshots (Maybe Text)
desMarker = lens _desMarker (\ s a -> s{_desMarker = a});

-- | The AWS customer account used to create or copy the snapshot. Use this
-- field to filter the results to snapshots owned by a particular account.
-- To describe snapshots you own, either specify your AWS customer account,
-- or do not specify the parameter.
desOwnerAccount :: Lens' DescribeClusterSnapshots (Maybe Text)
desOwnerAccount = lens _desOwnerAccount (\ s a -> s{_desOwnerAccount = a});

instance AWSPager DescribeClusterSnapshots where
        page rq rs
          | stop (rs ^. descMarker) = Nothing
          | stop (rs ^. descSnapshots) = Nothing
          | otherwise =
            Just $ rq & desMarker .~ rs ^. descMarker

instance AWSRequest DescribeClusterSnapshots where
        type Sv DescribeClusterSnapshots = Redshift
        type Rs DescribeClusterSnapshots =
             DescribeClusterSnapshotsResponse
        request = post
        response
          = receiveXMLWrapper "DescribeClusterSnapshotsResult"
              (\ s h x ->
                 DescribeClusterSnapshotsResponse' <$>
                   (x .@? "Snapshots" .!@ mempty >>=
                      may (parseXMLList "Snapshot"))
                     <*> (x .@? "Marker")
                     <*> (pure s))

instance ToHeaders DescribeClusterSnapshots where
        toHeaders = const mempty

instance ToPath DescribeClusterSnapshots where
        toPath = const "/"

instance ToQuery DescribeClusterSnapshots where
        toQuery DescribeClusterSnapshots'{..}
          = mconcat
              ["Action" =:
                 ("DescribeClusterSnapshots" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "SnapshotIdentifier" =: _desSnapshotIdentifier,
               "TagValues" =:
                 toQuery (toQueryList "TagValue" <$> _desTagValues),
               "StartTime" =: _desStartTime,
               "TagKeys" =:
                 toQuery (toQueryList "TagKey" <$> _desTagKeys),
               "ClusterIdentifier" =: _desClusterIdentifier,
               "SnapshotType" =: _desSnapshotType,
               "MaxRecords" =: _desMaxRecords,
               "EndTime" =: _desEndTime, "Marker" =: _desMarker,
               "OwnerAccount" =: _desOwnerAccount]

-- | Contains the output from the DescribeClusterSnapshots action.
--
-- /See:/ 'describeClusterSnapshotsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'descSnapshots'
--
-- * 'descMarker'
--
-- * 'descStatus'
data DescribeClusterSnapshotsResponse = DescribeClusterSnapshotsResponse'
    { _descSnapshots :: !(Maybe [Snapshot])
    , _descMarker    :: !(Maybe Text)
    , _descStatus    :: !Status
    } deriving (Eq,Read,Show)

-- | 'DescribeClusterSnapshotsResponse' smart constructor.
describeClusterSnapshotsResponse :: Status -> DescribeClusterSnapshotsResponse
describeClusterSnapshotsResponse pStatus =
    DescribeClusterSnapshotsResponse'
    { _descSnapshots = Nothing
    , _descMarker = Nothing
    , _descStatus = pStatus
    }

-- | A list of Snapshot instances.
descSnapshots :: Lens' DescribeClusterSnapshotsResponse [Snapshot]
descSnapshots = lens _descSnapshots (\ s a -> s{_descSnapshots = a}) . _Default;

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
descMarker :: Lens' DescribeClusterSnapshotsResponse (Maybe Text)
descMarker = lens _descMarker (\ s a -> s{_descMarker = a});

-- | FIXME: Undocumented member.
descStatus :: Lens' DescribeClusterSnapshotsResponse Status
descStatus = lens _descStatus (\ s a -> s{_descStatus = a});
