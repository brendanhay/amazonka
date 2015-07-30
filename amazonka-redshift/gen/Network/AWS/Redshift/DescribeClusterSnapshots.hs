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
    , dcssSnapshotIdentifier
    , dcssTagValues
    , dcssStartTime
    , dcssTagKeys
    , dcssClusterIdentifier
    , dcssSnapshotType
    , dcssMaxRecords
    , dcssEndTime
    , dcssMarker
    , dcssOwnerAccount

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
-- * 'dcssSnapshotIdentifier'
--
-- * 'dcssTagValues'
--
-- * 'dcssStartTime'
--
-- * 'dcssTagKeys'
--
-- * 'dcssClusterIdentifier'
--
-- * 'dcssSnapshotType'
--
-- * 'dcssMaxRecords'
--
-- * 'dcssEndTime'
--
-- * 'dcssMarker'
--
-- * 'dcssOwnerAccount'
data DescribeClusterSnapshots = DescribeClusterSnapshots'
    { _dcssSnapshotIdentifier :: !(Maybe Text)
    , _dcssTagValues          :: !(Maybe [Text])
    , _dcssStartTime          :: !(Maybe ISO8601)
    , _dcssTagKeys            :: !(Maybe [Text])
    , _dcssClusterIdentifier  :: !(Maybe Text)
    , _dcssSnapshotType       :: !(Maybe Text)
    , _dcssMaxRecords         :: !(Maybe Int)
    , _dcssEndTime            :: !(Maybe ISO8601)
    , _dcssMarker             :: !(Maybe Text)
    , _dcssOwnerAccount       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeClusterSnapshots' smart constructor.
describeClusterSnapshots :: DescribeClusterSnapshots
describeClusterSnapshots =
    DescribeClusterSnapshots'
    { _dcssSnapshotIdentifier = Nothing
    , _dcssTagValues = Nothing
    , _dcssStartTime = Nothing
    , _dcssTagKeys = Nothing
    , _dcssClusterIdentifier = Nothing
    , _dcssSnapshotType = Nothing
    , _dcssMaxRecords = Nothing
    , _dcssEndTime = Nothing
    , _dcssMarker = Nothing
    , _dcssOwnerAccount = Nothing
    }

-- | The snapshot identifier of the snapshot about which to return
-- information.
dcssSnapshotIdentifier :: Lens' DescribeClusterSnapshots (Maybe Text)
dcssSnapshotIdentifier = lens _dcssSnapshotIdentifier (\ s a -> s{_dcssSnapshotIdentifier = a});

-- | A tag value or values for which you want to return all matching cluster
-- snapshots that are associated with the specified tag value or values.
-- For example, suppose that you have snapshots that are tagged with values
-- called @admin@ and @test@. If you specify both of these tag values in
-- the request, Amazon Redshift returns a response with the snapshots that
-- have either or both of these tag values associated with them.
dcssTagValues :: Lens' DescribeClusterSnapshots [Text]
dcssTagValues = lens _dcssTagValues (\ s a -> s{_dcssTagValues = a}) . _Default . _Coerce;

-- | A value that requests only snapshots created at or after the specified
-- time. The time value is specified in ISO 8601 format. For more
-- information about ISO 8601, go to the
-- <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: @2012-07-16T18:00:00Z@
dcssStartTime :: Lens' DescribeClusterSnapshots (Maybe UTCTime)
dcssStartTime = lens _dcssStartTime (\ s a -> s{_dcssStartTime = a}) . mapping _Time;

-- | A tag key or keys for which you want to return all matching cluster
-- snapshots that are associated with the specified key or keys. For
-- example, suppose that you have snapshots that are tagged with keys
-- called @owner@ and @environment@. If you specify both of these tag keys
-- in the request, Amazon Redshift returns a response with the snapshots
-- that have either or both of these tag keys associated with them.
dcssTagKeys :: Lens' DescribeClusterSnapshots [Text]
dcssTagKeys = lens _dcssTagKeys (\ s a -> s{_dcssTagKeys = a}) . _Default . _Coerce;

-- | The identifier of the cluster for which information about snapshots is
-- requested.
dcssClusterIdentifier :: Lens' DescribeClusterSnapshots (Maybe Text)
dcssClusterIdentifier = lens _dcssClusterIdentifier (\ s a -> s{_dcssClusterIdentifier = a});

-- | The type of snapshots for which you are requesting information. By
-- default, snapshots of all types are returned.
--
-- Valid Values: @automated@ | @manual@
dcssSnapshotType :: Lens' DescribeClusterSnapshots (Maybe Text)
dcssSnapshotType = lens _dcssSnapshotType (\ s a -> s{_dcssSnapshotType = a});

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
dcssMaxRecords :: Lens' DescribeClusterSnapshots (Maybe Int)
dcssMaxRecords = lens _dcssMaxRecords (\ s a -> s{_dcssMaxRecords = a});

-- | A time value that requests only snapshots created at or before the
-- specified time. The time value is specified in ISO 8601 format. For more
-- information about ISO 8601, go to the
-- <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: @2012-07-16T18:00:00Z@
dcssEndTime :: Lens' DescribeClusterSnapshots (Maybe UTCTime)
dcssEndTime = lens _dcssEndTime (\ s a -> s{_dcssEndTime = a}) . mapping _Time;

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterSnapshots
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
dcssMarker :: Lens' DescribeClusterSnapshots (Maybe Text)
dcssMarker = lens _dcssMarker (\ s a -> s{_dcssMarker = a});

-- | The AWS customer account used to create or copy the snapshot. Use this
-- field to filter the results to snapshots owned by a particular account.
-- To describe snapshots you own, either specify your AWS customer account,
-- or do not specify the parameter.
dcssOwnerAccount :: Lens' DescribeClusterSnapshots (Maybe Text)
dcssOwnerAccount = lens _dcssOwnerAccount (\ s a -> s{_dcssOwnerAccount = a});

instance AWSPager DescribeClusterSnapshots where
        page rq rs
          | stop (rs ^. dcssrsMarker) = Nothing
          | stop (rs ^. dcssrsSnapshots) = Nothing
          | otherwise =
            Just $ rq & dcssMarker .~ rs ^. dcssrsMarker

instance AWSRequest DescribeClusterSnapshots where
        type Sv DescribeClusterSnapshots = Redshift
        type Rs DescribeClusterSnapshots =
             DescribeClusterSnapshotsResponse
        request = postQuery
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
        toPath = const mempty

instance ToQuery DescribeClusterSnapshots where
        toQuery DescribeClusterSnapshots'{..}
          = mconcat
              ["Action" =:
                 ("DescribeClusterSnapshots" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "SnapshotIdentifier" =: _dcssSnapshotIdentifier,
               "TagValues" =:
                 toQuery (toQueryList "TagValue" <$> _dcssTagValues),
               "StartTime" =: _dcssStartTime,
               "TagKeys" =:
                 toQuery (toQueryList "TagKey" <$> _dcssTagKeys),
               "ClusterIdentifier" =: _dcssClusterIdentifier,
               "SnapshotType" =: _dcssSnapshotType,
               "MaxRecords" =: _dcssMaxRecords,
               "EndTime" =: _dcssEndTime, "Marker" =: _dcssMarker,
               "OwnerAccount" =: _dcssOwnerAccount]

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
describeClusterSnapshotsResponse pStatus_ =
    DescribeClusterSnapshotsResponse'
    { _dcssrsSnapshots = Nothing
    , _dcssrsMarker = Nothing
    , _dcssrsStatus = pStatus_
    }

-- | A list of Snapshot instances.
dcssrsSnapshots :: Lens' DescribeClusterSnapshotsResponse [Snapshot]
dcssrsSnapshots = lens _dcssrsSnapshots (\ s a -> s{_dcssrsSnapshots = a}) . _Default . _Coerce;

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
