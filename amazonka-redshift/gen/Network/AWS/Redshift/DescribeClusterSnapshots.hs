{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DescribeClusterSnapshots
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns one or more snapshot objects, which contain metadata about your
-- cluster snapshots. By default, this operation returns information about all
-- snapshots of all clusters that are owned by you AWS customer account. No
-- information is returned for snapshots owned by inactive AWS customer
-- accounts. If you specify both tag keys and tag values in the same request,
-- Amazon Redshift returns all snapshots that match any combination of the
-- specified keys and values. For example, if you have @owner@ and
-- @environment@ for tag keys, and @admin@ and @test@ for tag values, all
-- snapshots that have any combination of those values are returned. Only
-- snapshots that you own are returned in the response; shared snapshots are
-- not returned with the tag key and tag value request parameters. If both tag
-- keys and values are omitted from the request, snapshots are returned
-- regardless of whether they have tag keys or values associated with them.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeClusterSnapshots.html>
module Network.AWS.Redshift.DescribeClusterSnapshots
    (
    -- * Request
      DescribeClusterSnapshots
    -- ** Request constructor
    , describeClusterSnapshots
    -- ** Request lenses
    , dcs1ClusterIdentifier
    , dcs1EndTime
    , dcs1Marker
    , dcs1MaxRecords
    , dcs1OwnerAccount
    , dcs1SnapshotIdentifier
    , dcs1SnapshotType
    , dcs1StartTime
    , dcs1TagKeys
    , dcs1TagValues

    -- * Response
    , DescribeClusterSnapshotsResponse
    -- ** Response constructor
    , describeClusterSnapshotsResponse
    -- ** Response lenses
    , dcsrMarker
    , dcsrSnapshots
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data DescribeClusterSnapshots = DescribeClusterSnapshots
    { _dcs1ClusterIdentifier  :: Maybe Text
    , _dcs1EndTime            :: Maybe RFC822
    , _dcs1Marker             :: Maybe Text
    , _dcs1MaxRecords         :: Maybe Int
    , _dcs1OwnerAccount       :: Maybe Text
    , _dcs1SnapshotIdentifier :: Maybe Text
    , _dcs1SnapshotType       :: Maybe Text
    , _dcs1StartTime          :: Maybe RFC822
    , _dcs1TagKeys            :: List "TagKey" Text
    , _dcs1TagValues          :: List "TagValue" Text
    } deriving (Eq, Ord, Show)

-- | 'DescribeClusterSnapshots' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcs1ClusterIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'dcs1EndTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'dcs1Marker' @::@ 'Maybe' 'Text'
--
-- * 'dcs1MaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dcs1OwnerAccount' @::@ 'Maybe' 'Text'
--
-- * 'dcs1SnapshotIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'dcs1SnapshotType' @::@ 'Maybe' 'Text'
--
-- * 'dcs1StartTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'dcs1TagKeys' @::@ ['Text']
--
-- * 'dcs1TagValues' @::@ ['Text']
--
describeClusterSnapshots :: DescribeClusterSnapshots
describeClusterSnapshots = DescribeClusterSnapshots
    { _dcs1ClusterIdentifier  = Nothing
    , _dcs1SnapshotIdentifier = Nothing
    , _dcs1SnapshotType       = Nothing
    , _dcs1StartTime          = Nothing
    , _dcs1EndTime            = Nothing
    , _dcs1MaxRecords         = Nothing
    , _dcs1Marker             = Nothing
    , _dcs1OwnerAccount       = Nothing
    , _dcs1TagKeys            = mempty
    , _dcs1TagValues          = mempty
    }

-- | The identifier of the cluster for which information about snapshots is
-- requested.
dcs1ClusterIdentifier :: Lens' DescribeClusterSnapshots (Maybe Text)
dcs1ClusterIdentifier =
    lens _dcs1ClusterIdentifier (\s a -> s { _dcs1ClusterIdentifier = a })

-- | A time value that requests only snapshots created at or before the
-- specified time. The time value is specified in ISO 8601 format. For more
-- information about ISO 8601, go to the
-- <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.> Example:
-- @2012-07-16T18:00:00Z@.
dcs1EndTime :: Lens' DescribeClusterSnapshots (Maybe UTCTime)
dcs1EndTime = lens _dcs1EndTime (\s a -> s { _dcs1EndTime = a }) . mapping _Time

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a 'DescribeClusterSnapshots'
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
dcs1Marker :: Lens' DescribeClusterSnapshots (Maybe Text)
dcs1Marker = lens _dcs1Marker (\s a -> s { _dcs1Marker = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value. Default: @100@ Constraints: minimum 20, maximum
-- 100.
dcs1MaxRecords :: Lens' DescribeClusterSnapshots (Maybe Int)
dcs1MaxRecords = lens _dcs1MaxRecords (\s a -> s { _dcs1MaxRecords = a })

-- | The AWS customer account used to create or copy the snapshot. Use this
-- field to filter the results to snapshots owned by a particular account.
-- To describe snapshots you own, either specify your AWS customer account,
-- or do not specify the parameter.
dcs1OwnerAccount :: Lens' DescribeClusterSnapshots (Maybe Text)
dcs1OwnerAccount = lens _dcs1OwnerAccount (\s a -> s { _dcs1OwnerAccount = a })

-- | The snapshot identifier of the snapshot about which to return
-- information.
dcs1SnapshotIdentifier :: Lens' DescribeClusterSnapshots (Maybe Text)
dcs1SnapshotIdentifier =
    lens _dcs1SnapshotIdentifier (\s a -> s { _dcs1SnapshotIdentifier = a })

-- | The type of snapshots for which you are requesting information. By
-- default, snapshots of all types are returned. Valid Values: @automated@ |
-- @manual@.
dcs1SnapshotType :: Lens' DescribeClusterSnapshots (Maybe Text)
dcs1SnapshotType = lens _dcs1SnapshotType (\s a -> s { _dcs1SnapshotType = a })

-- | A value that requests only snapshots created at or after the specified
-- time. The time value is specified in ISO 8601 format. For more
-- information about ISO 8601, go to the
-- <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.> Example:
-- @2012-07-16T18:00:00Z@.
dcs1StartTime :: Lens' DescribeClusterSnapshots (Maybe UTCTime)
dcs1StartTime = lens _dcs1StartTime (\s a -> s { _dcs1StartTime = a }) . mapping _Time

-- | A tag key or keys for which you want to return all matching cluster
-- snapshots that are associated with the specified key or keys. For
-- example, suppose that you have snapshots that are tagged with keys called
-- @owner@ and @environment@. If you specify both of these tag keys in the
-- request, Amazon Redshift returns a response with the snapshots that have
-- either or both of these tag keys associated with them.
dcs1TagKeys :: Lens' DescribeClusterSnapshots [Text]
dcs1TagKeys = lens _dcs1TagKeys (\s a -> s { _dcs1TagKeys = a }) . _List

-- | A tag value or values for which you want to return all matching cluster
-- snapshots that are associated with the specified tag value or values. For
-- example, suppose that you have snapshots that are tagged with values
-- called @admin@ and @test@. If you specify both of these tag values in the
-- request, Amazon Redshift returns a response with the snapshots that have
-- either or both of these tag values associated with them.
dcs1TagValues :: Lens' DescribeClusterSnapshots [Text]
dcs1TagValues = lens _dcs1TagValues (\s a -> s { _dcs1TagValues = a }) . _List

data DescribeClusterSnapshotsResponse = DescribeClusterSnapshotsResponse
    { _dcsrMarker    :: Maybe Text
    , _dcsrSnapshots :: List "Snapshot" Snapshot
    } deriving (Eq, Show)

-- | 'DescribeClusterSnapshotsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsrMarker' @::@ 'Maybe' 'Text'
--
-- * 'dcsrSnapshots' @::@ ['Snapshot']
--
describeClusterSnapshotsResponse :: DescribeClusterSnapshotsResponse
describeClusterSnapshotsResponse = DescribeClusterSnapshotsResponse
    { _dcsrMarker    = Nothing
    , _dcsrSnapshots = mempty
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for the
-- request.
dcsrMarker :: Lens' DescribeClusterSnapshotsResponse (Maybe Text)
dcsrMarker = lens _dcsrMarker (\s a -> s { _dcsrMarker = a })

-- | A list of 'Snapshot' instances.
dcsrSnapshots :: Lens' DescribeClusterSnapshotsResponse [Snapshot]
dcsrSnapshots = lens _dcsrSnapshots (\s a -> s { _dcsrSnapshots = a }) . _List

instance ToPath DescribeClusterSnapshots where
    toPath = const "/"

instance ToQuery DescribeClusterSnapshots where
    toQuery DescribeClusterSnapshots{..} = mconcat
        [ "ClusterIdentifier"  =? _dcs1ClusterIdentifier
        , "EndTime"            =? _dcs1EndTime
        , "Marker"             =? _dcs1Marker
        , "MaxRecords"         =? _dcs1MaxRecords
        , "OwnerAccount"       =? _dcs1OwnerAccount
        , "SnapshotIdentifier" =? _dcs1SnapshotIdentifier
        , "SnapshotType"       =? _dcs1SnapshotType
        , "StartTime"          =? _dcs1StartTime
        , "TagKeys"            =? _dcs1TagKeys
        , "TagValues"          =? _dcs1TagValues
        ]

instance ToHeaders DescribeClusterSnapshots

instance AWSRequest DescribeClusterSnapshots where
    type Sv DescribeClusterSnapshots = Redshift
    type Rs DescribeClusterSnapshots = DescribeClusterSnapshotsResponse

    request  = post "DescribeClusterSnapshots"
    response = xmlResponse

instance FromXML DescribeClusterSnapshotsResponse where
    parseXML = withElement "DescribeClusterSnapshotsResult" $ \x -> DescribeClusterSnapshotsResponse
        <$> x .@? "Marker"
        <*> x .@  "Snapshots"

instance AWSPager DescribeClusterSnapshots where
    page rq rs
        | stop (rq ^. dcs1Marker) = Nothing
        | otherwise = (\x -> rq & dcs1Marker ?~ x)
            <$> (rs ^. dcsrMarker)
