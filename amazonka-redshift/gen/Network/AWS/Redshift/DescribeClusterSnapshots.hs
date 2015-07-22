{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeClusterSnapshots
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns one or more snapshot objects, which contain metadata about your
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
    , dcssrqSnapshotIdentifier
    , dcssrqTagValues
    , dcssrqStartTime
    , dcssrqTagKeys
    , dcssrqClusterIdentifier
    , dcssrqSnapshotType
    , dcssrqMaxRecords
    , dcssrqEndTime
    , dcssrqMarker
    , dcssrqOwnerAccount

    -- * Response
    , DescribeClusterSnapshotsResponse
    -- ** Response constructor
    , describeClusterSnapshotsResponse
    -- ** Response lenses
    , dcssrsSnapshots
    , dcssrsMarker
    , dcssrsStatus
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
-- * 'dcssrqSnapshotIdentifier'
--
-- * 'dcssrqTagValues'
--
-- * 'dcssrqStartTime'
--
-- * 'dcssrqTagKeys'
--
-- * 'dcssrqClusterIdentifier'
--
-- * 'dcssrqSnapshotType'
--
-- * 'dcssrqMaxRecords'
--
-- * 'dcssrqEndTime'
--
-- * 'dcssrqMarker'
--
-- * 'dcssrqOwnerAccount'
data DescribeClusterSnapshots = DescribeClusterSnapshots'
    { _dcssrqSnapshotIdentifier :: !(Maybe Text)
    , _dcssrqTagValues          :: !(Maybe [Text])
    , _dcssrqStartTime          :: !(Maybe ISO8601)
    , _dcssrqTagKeys            :: !(Maybe [Text])
    , _dcssrqClusterIdentifier  :: !(Maybe Text)
    , _dcssrqSnapshotType       :: !(Maybe Text)
    , _dcssrqMaxRecords         :: !(Maybe Int)
    , _dcssrqEndTime            :: !(Maybe ISO8601)
    , _dcssrqMarker             :: !(Maybe Text)
    , _dcssrqOwnerAccount       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeClusterSnapshots' smart constructor.
describeClusterSnapshots :: DescribeClusterSnapshots
describeClusterSnapshots =
    DescribeClusterSnapshots'
    { _dcssrqSnapshotIdentifier = Nothing
    , _dcssrqTagValues = Nothing
    , _dcssrqStartTime = Nothing
    , _dcssrqTagKeys = Nothing
    , _dcssrqClusterIdentifier = Nothing
    , _dcssrqSnapshotType = Nothing
    , _dcssrqMaxRecords = Nothing
    , _dcssrqEndTime = Nothing
    , _dcssrqMarker = Nothing
    , _dcssrqOwnerAccount = Nothing
    }

-- | The snapshot identifier of the snapshot about which to return
-- information.
dcssrqSnapshotIdentifier :: Lens' DescribeClusterSnapshots (Maybe Text)
dcssrqSnapshotIdentifier = lens _dcssrqSnapshotIdentifier (\ s a -> s{_dcssrqSnapshotIdentifier = a});

-- | A tag value or values for which you want to return all matching cluster
-- snapshots that are associated with the specified tag value or values.
-- For example, suppose that you have snapshots that are tagged with values
-- called @admin@ and @test@. If you specify both of these tag values in
-- the request, Amazon Redshift returns a response with the snapshots that
-- have either or both of these tag values associated with them.
dcssrqTagValues :: Lens' DescribeClusterSnapshots [Text]
dcssrqTagValues = lens _dcssrqTagValues (\ s a -> s{_dcssrqTagValues = a}) . _Default;

-- | A value that requests only snapshots created at or after the specified
-- time. The time value is specified in ISO 8601 format. For more
-- information about ISO 8601, go to the
-- <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: @2012-07-16T18:00:00Z@
dcssrqStartTime :: Lens' DescribeClusterSnapshots (Maybe UTCTime)
dcssrqStartTime = lens _dcssrqStartTime (\ s a -> s{_dcssrqStartTime = a}) . mapping _Time;

-- | A tag key or keys for which you want to return all matching cluster
-- snapshots that are associated with the specified key or keys. For
-- example, suppose that you have snapshots that are tagged with keys
-- called @owner@ and @environment@. If you specify both of these tag keys
-- in the request, Amazon Redshift returns a response with the snapshots
-- that have either or both of these tag keys associated with them.
dcssrqTagKeys :: Lens' DescribeClusterSnapshots [Text]
dcssrqTagKeys = lens _dcssrqTagKeys (\ s a -> s{_dcssrqTagKeys = a}) . _Default;

-- | The identifier of the cluster for which information about snapshots is
-- requested.
dcssrqClusterIdentifier :: Lens' DescribeClusterSnapshots (Maybe Text)
dcssrqClusterIdentifier = lens _dcssrqClusterIdentifier (\ s a -> s{_dcssrqClusterIdentifier = a});

-- | The type of snapshots for which you are requesting information. By
-- default, snapshots of all types are returned.
--
-- Valid Values: @automated@ | @manual@
dcssrqSnapshotType :: Lens' DescribeClusterSnapshots (Maybe Text)
dcssrqSnapshotType = lens _dcssrqSnapshotType (\ s a -> s{_dcssrqSnapshotType = a});

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
dcssrqMaxRecords :: Lens' DescribeClusterSnapshots (Maybe Int)
dcssrqMaxRecords = lens _dcssrqMaxRecords (\ s a -> s{_dcssrqMaxRecords = a});

-- | A time value that requests only snapshots created at or before the
-- specified time. The time value is specified in ISO 8601 format. For more
-- information about ISO 8601, go to the
-- <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: @2012-07-16T18:00:00Z@
dcssrqEndTime :: Lens' DescribeClusterSnapshots (Maybe UTCTime)
dcssrqEndTime = lens _dcssrqEndTime (\ s a -> s{_dcssrqEndTime = a}) . mapping _Time;

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterSnapshots
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
dcssrqMarker :: Lens' DescribeClusterSnapshots (Maybe Text)
dcssrqMarker = lens _dcssrqMarker (\ s a -> s{_dcssrqMarker = a});

-- | The AWS customer account used to create or copy the snapshot. Use this
-- field to filter the results to snapshots owned by a particular account.
-- To describe snapshots you own, either specify your AWS customer account,
-- or do not specify the parameter.
dcssrqOwnerAccount :: Lens' DescribeClusterSnapshots (Maybe Text)
dcssrqOwnerAccount = lens _dcssrqOwnerAccount (\ s a -> s{_dcssrqOwnerAccount = a});

instance AWSPager DescribeClusterSnapshots where
        page rq rs
          | stop (rs ^. dcssrsMarker) = Nothing
          | stop (rs ^. dcssrsSnapshots) = Nothing
          | otherwise =
            Just $ rq & dcssrqMarker .~ rs ^. dcssrsMarker

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
                     <*> (pure (fromEnum s)))

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
               "SnapshotIdentifier" =: _dcssrqSnapshotIdentifier,
               "TagValues" =:
                 toQuery
                   (toQueryList "TagValue" <$> _dcssrqTagValues),
               "StartTime" =: _dcssrqStartTime,
               "TagKeys" =:
                 toQuery (toQueryList "TagKey" <$> _dcssrqTagKeys),
               "ClusterIdentifier" =: _dcssrqClusterIdentifier,
               "SnapshotType" =: _dcssrqSnapshotType,
               "MaxRecords" =: _dcssrqMaxRecords,
               "EndTime" =: _dcssrqEndTime,
               "Marker" =: _dcssrqMarker,
               "OwnerAccount" =: _dcssrqOwnerAccount]

-- | Contains the output from the DescribeClusterSnapshots action.
--
-- /See:/ 'describeClusterSnapshotsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcssrsSnapshots'
--
-- * 'dcssrsMarker'
--
-- * 'dcssrsStatus'
data DescribeClusterSnapshotsResponse = DescribeClusterSnapshotsResponse'
    { _dcssrsSnapshots :: !(Maybe [Snapshot])
    , _dcssrsMarker    :: !(Maybe Text)
    , _dcssrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeClusterSnapshotsResponse' smart constructor.
describeClusterSnapshotsResponse :: Int -> DescribeClusterSnapshotsResponse
describeClusterSnapshotsResponse pStatus =
    DescribeClusterSnapshotsResponse'
    { _dcssrsSnapshots = Nothing
    , _dcssrsMarker = Nothing
    , _dcssrsStatus = pStatus
    }

-- | A list of Snapshot instances.
dcssrsSnapshots :: Lens' DescribeClusterSnapshotsResponse [Snapshot]
dcssrsSnapshots = lens _dcssrsSnapshots (\ s a -> s{_dcssrsSnapshots = a}) . _Default;

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
dcssrsMarker :: Lens' DescribeClusterSnapshotsResponse (Maybe Text)
dcssrsMarker = lens _dcssrsMarker (\ s a -> s{_dcssrsMarker = a});

-- | FIXME: Undocumented member.
dcssrsStatus :: Lens' DescribeClusterSnapshotsResponse Int
dcssrsStatus = lens _dcssrsStatus (\ s a -> s{_dcssrsStatus = a});
