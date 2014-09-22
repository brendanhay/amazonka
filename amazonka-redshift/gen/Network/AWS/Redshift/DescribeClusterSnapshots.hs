{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
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
-- accounts. https://redshift.us-east-1.amazonaws.com/
-- ?Action=DescribeClusterSnapshots &ClusterIdentifier=examplecluster
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T011512Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 5439
-- cm:examplecluster-2013-01-22-19-27-58 available automated 1.0
-- 2013-01-22T19:27:58.931Z 2 dev 2013-01-22T19:23:59.368Z us-east-1c
-- dw1.xlarge examplecluster adminuser 5439 my-snapshot-123 available manual
-- 1.0 2013-01-23T01:09:03.149Z 2 dev 2013-01-22T19:23:59.368Z us-east-1c
-- dw1.xlarge examplecluster adminuser 56a9daf4-64fa-11e2-a8da-655adc216806.
module Network.AWS.Redshift.DescribeClusterSnapshots
    (
    -- * Request
      DescribeClusterSnapshots
    -- ** Request constructor
    , describeClusterSnapshots
    -- ** Request lenses
    , dcs1ClusterIdentifier
    , dcs1SnapshotIdentifier
    , dcs1SnapshotType
    , dcs1StartTime
    , dcs1EndTime
    , dcs1MaxRecords
    , dcs1Marker
    , dcs1OwnerAccount

    -- * Response
    , DescribeClusterSnapshotsResponse
    -- ** Response constructor
    , describeClusterSnapshotsResponse
    -- ** Response lenses
    , dcsrrMarker
    , dcsrrSnapshot
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import Network.AWS.Prelude

-- | 
data DescribeClusterSnapshots = DescribeClusterSnapshots
    { _dcs1ClusterIdentifier :: Maybe Text
    , _dcs1SnapshotIdentifier :: Maybe Text
    , _dcs1SnapshotType :: Maybe Text
    , _dcs1StartTime :: Maybe ISO8601
    , _dcs1EndTime :: Maybe ISO8601
    , _dcs1MaxRecords :: Maybe Integer
    , _dcs1Marker :: Maybe Text
    , _dcs1OwnerAccount :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeClusterSnapshots' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ClusterIdentifier ::@ @Maybe Text@
--
-- * @SnapshotIdentifier ::@ @Maybe Text@
--
-- * @SnapshotType ::@ @Maybe Text@
--
-- * @StartTime ::@ @Maybe ISO8601@
--
-- * @EndTime ::@ @Maybe ISO8601@
--
-- * @MaxRecords ::@ @Maybe Integer@
--
-- * @Marker ::@ @Maybe Text@
--
-- * @OwnerAccount ::@ @Maybe Text@
--
describeClusterSnapshots :: DescribeClusterSnapshots
describeClusterSnapshots = DescribeClusterSnapshots
    { _dcs1ClusterIdentifier = Nothing
    , _dcs1SnapshotIdentifier = Nothing
    , _dcs1SnapshotType = Nothing
    , _dcs1StartTime = Nothing
    , _dcs1EndTime = Nothing
    , _dcs1MaxRecords = Nothing
    , _dcs1Marker = Nothing
    , _dcs1OwnerAccount = Nothing
    }

-- | The identifier of the cluster for which information about snapshots is
-- requested.
dcs1ClusterIdentifier :: Lens' DescribeClusterSnapshots (Maybe Text)
dcs1ClusterIdentifier =
    lens _dcs1ClusterIdentifier (\s a -> s { _dcs1ClusterIdentifier = a })

-- | The snapshot identifier of the snapshot about which to return information.
dcs1SnapshotIdentifier :: Lens' DescribeClusterSnapshots (Maybe Text)
dcs1SnapshotIdentifier =
    lens _dcs1SnapshotIdentifier (\s a -> s { _dcs1SnapshotIdentifier = a })

-- | The type of snapshots for which you are requesting information. By default,
-- snapshots of all types are returned. Valid Values: automated | manual.
dcs1SnapshotType :: Lens' DescribeClusterSnapshots (Maybe Text)
dcs1SnapshotType =
    lens _dcs1SnapshotType (\s a -> s { _dcs1SnapshotType = a })

-- | A value that requests only snapshots created at or after the specified
-- time. The time value is specified in ISO 8601 format. For more information
-- about ISO 8601, go to the ISO8601 Wikipedia page. Example:
-- 2012-07-16T18:00:00Z.
dcs1StartTime :: Lens' DescribeClusterSnapshots (Maybe ISO8601)
dcs1StartTime = lens _dcs1StartTime (\s a -> s { _dcs1StartTime = a })

-- | A time value that requests only snapshots created at or before the
-- specified time. The time value is specified in ISO 8601 format. For more
-- information about ISO 8601, go to the ISO8601 Wikipedia page. Example:
-- 2012-07-16T18:00:00Z.
dcs1EndTime :: Lens' DescribeClusterSnapshots (Maybe ISO8601)
dcs1EndTime = lens _dcs1EndTime (\s a -> s { _dcs1EndTime = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the returned
-- marker value. Default: 100 Constraints: minimum 20, maximum 100.
dcs1MaxRecords :: Lens' DescribeClusterSnapshots (Maybe Integer)
dcs1MaxRecords = lens _dcs1MaxRecords (\s a -> s { _dcs1MaxRecords = a })

-- | An optional parameter that specifies the starting point to return a set of
-- response records. When the results of a DescribeClusterSnapshots request
-- exceed the value specified in MaxRecords, AWS returns a value in the Marker
-- field of the response. You can retrieve the next set of response records by
-- providing the returned marker value in the Marker parameter and retrying
-- the request.
dcs1Marker :: Lens' DescribeClusterSnapshots (Maybe Text)
dcs1Marker = lens _dcs1Marker (\s a -> s { _dcs1Marker = a })

-- | The AWS customer account used to create or copy the snapshot. Use this
-- field to filter the results to snapshots owned by a particular account. To
-- describe snapshots you own, either specify your AWS customer account, or do
-- not specify the parameter.
dcs1OwnerAccount :: Lens' DescribeClusterSnapshots (Maybe Text)
dcs1OwnerAccount =
    lens _dcs1OwnerAccount (\s a -> s { _dcs1OwnerAccount = a })

instance ToQuery DescribeClusterSnapshots where
    toQuery = genericQuery def

-- | Contains the output from the DescribeClusterSnapshots action.
data DescribeClusterSnapshotsResponse = DescribeClusterSnapshotsResponse
    { _dcsrrMarker :: Maybe Text
    , _dcsrrSnapshot :: [Snapshot]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeClusterSnapshotsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Marker ::@ @Maybe Text@
--
-- * @Snapshot ::@ @[Snapshot]@
--
describeClusterSnapshotsResponse :: DescribeClusterSnapshotsResponse
describeClusterSnapshotsResponse = DescribeClusterSnapshotsResponse
    { _dcsrrMarker = Nothing
    , _dcsrrSnapshot = mempty
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker
-- value in the Marker parameter and retrying the command. If the Marker field
-- is empty, all response records have been retrieved for the request.
dcsrrMarker :: Lens' DescribeClusterSnapshotsResponse (Maybe Text)
dcsrrMarker = lens _dcsrrMarker (\s a -> s { _dcsrrMarker = a })

-- | A list of Snapshot instances.
dcsrrSnapshot :: Lens' DescribeClusterSnapshotsResponse [Snapshot]
dcsrrSnapshot = lens _dcsrrSnapshot (\s a -> s { _dcsrrSnapshot = a })

instance FromXML DescribeClusterSnapshotsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeClusterSnapshots where
    type Sv DescribeClusterSnapshots = Redshift
    type Rs DescribeClusterSnapshots = DescribeClusterSnapshotsResponse

    request = post "DescribeClusterSnapshots"
    response _ = xmlResponse

instance AWSPager DescribeClusterSnapshots where
    next rq rs = (\x -> rq & dcs1Marker ?~ x)
        <$> (rs ^. dcsrrMarker)
