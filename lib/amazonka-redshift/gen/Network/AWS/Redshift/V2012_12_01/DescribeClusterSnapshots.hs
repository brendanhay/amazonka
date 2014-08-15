{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DescribeClusterSnapshots
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
module Network.AWS.Redshift.V2012_12_01.DescribeClusterSnapshots where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeClusterSnapshots' request.
describeClusterSnapshots :: DescribeClusterSnapshots
describeClusterSnapshots = DescribeClusterSnapshots
    { _dcsnMaxRecords = Nothing
    , _dcsnSnapshotIdentifier = Nothing
    , _dcsnClusterIdentifier = Nothing
    , _dcsnSnapshotType = Nothing
    , _dcsnMarker = Nothing
    , _dcsnOwnerAccount = Nothing
    , _dcsnStartTime = Nothing
    , _dcsnEndTime = Nothing
    }

data DescribeClusterSnapshots = DescribeClusterSnapshots
    { _dcsnMaxRecords :: Maybe Integer
      -- ^ The maximum number of response records to return in each call. If
      -- the number of remaining response records exceeds the specified
      -- MaxRecords value, a value is returned in a marker field of the
      -- response. You can retrieve the next set of records by retrying
      -- the command with the returned marker value. Default: 100
      -- Constraints: minimum 20, maximum 100.
    , _dcsnSnapshotIdentifier :: Maybe Text
      -- ^ The snapshot identifier of the snapshot about which to return
      -- information.
    , _dcsnClusterIdentifier :: Maybe Text
      -- ^ The identifier of the cluster for which information about
      -- snapshots is requested.
    , _dcsnSnapshotType :: Maybe Text
      -- ^ The type of snapshots for which you are requesting information.
      -- By default, snapshots of all types are returned. Valid Values:
      -- automated | manual.
    , _dcsnMarker :: Maybe Text
      -- ^ An optional parameter that specifies the starting point to return
      -- a set of response records. When the results of a
      -- DescribeClusterSnapshots request exceed the value specified in
      -- MaxRecords, AWS returns a value in the Marker field of the
      -- response. You can retrieve the next set of response records by
      -- providing the returned marker value in the Marker parameter and
      -- retrying the request.
    , _dcsnOwnerAccount :: Maybe Text
      -- ^ The AWS customer account used to create or copy the snapshot. Use
      -- this field to filter the results to snapshots owned by a
      -- particular account. To describe snapshots you own, either specify
      -- your AWS customer account, or do not specify the parameter.
    , _dcsnStartTime :: Maybe ISO8601
      -- ^ A value that requests only snapshots created at or after the
      -- specified time. The time value is specified in ISO 8601 format.
      -- For more information about ISO 8601, go to the ISO8601 Wikipedia
      -- page. Example: 2012-07-16T18:00:00Z.
    , _dcsnEndTime :: Maybe ISO8601
      -- ^ A time value that requests only snapshots created at or before
      -- the specified time. The time value is specified in ISO 8601
      -- format. For more information about ISO 8601, go to the ISO8601
      -- Wikipedia page. Example: 2012-07-16T18:00:00Z.
    } deriving (Show, Generic)

makeLenses ''DescribeClusterSnapshots

instance ToQuery DescribeClusterSnapshots where
    toQuery = genericQuery def

data DescribeClusterSnapshotsResponse = DescribeClusterSnapshotsResponse
    { _ssseSnapshots :: [Snapshot]
      -- ^ A list of Snapshot instances.
    , _ssseMarker :: Maybe Text
      -- ^ A value that indicates the starting point for the next set of
      -- response records in a subsequent request. If a value is returned
      -- in a response, you can retrieve the next set of records by
      -- providing this returned marker value in the Marker parameter and
      -- retrying the command. If the Marker field is empty, all response
      -- records have been retrieved for the request.
    } deriving (Show, Generic)

makeLenses ''DescribeClusterSnapshotsResponse

instance FromXML DescribeClusterSnapshotsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeClusterSnapshots where
    type Sv DescribeClusterSnapshots = Redshift
    type Rs DescribeClusterSnapshots = DescribeClusterSnapshotsResponse

    request = post "DescribeClusterSnapshots"
    response _ = xmlResponse

instance AWSPager DescribeClusterSnapshots where
    next rq rs = (\x -> rq { _dcsnMarker = Just x })
        <$> (_ssseMarker rs)
