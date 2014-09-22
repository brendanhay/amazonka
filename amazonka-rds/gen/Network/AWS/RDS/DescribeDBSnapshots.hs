{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DescribeDBSnapshots
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about DB snapshots. This API supports pagination.
-- https://rds.amazon.com/ ?Action=DescribeDBSnapshots &MaxRecords=100
-- &Version=2013-05-15 &Timestamp=2011-05-23T06%3A27%3A42.551Z
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256 &AWSAccessKeyId=
-- &Signature= 3306 2011-05-23T06:29:03.483Z mysql available us-east-1a
-- general-public-license 2011-05-23T06:06:43.110Z 10 simcoprod01 5.1.50
-- mydbsnapshot manual master myoptiongroupname 3306 2011-03-11T07:20:24.082Z
-- mysql available us-east-1a general-public-license 2010-08-04T23:27:36.420Z
-- 50 mydbinstance 5.1.49 mysnapshot1 manual sa myoptiongroupname 3306
-- 2012-04-02T00:01:24.082Z mysql available us-east-1d general-public-license
-- 2010-07-16T00:06:59.107Z 60 simcoprod01 5.1.47
-- rds:simcoprod01-2012-04-02-00-01 automated master myoptiongroupname
-- c4191173-8506-11e0-90aa-eb648410240d.
module Network.AWS.RDS.DescribeDBSnapshots
    (
    -- * Request
      DescribeDBSnapshots
    -- ** Request constructor
    , describeDBSnapshots
    -- ** Request lenses
    , ddbs1DBInstanceIdentifier
    , ddbs1DBSnapshotIdentifier
    , ddbs1SnapshotType
    , ddbs1MaxRecords
    , ddbs1Marker

    -- * Response
    , DescribeDBSnapshotsResponse
    -- ** Response constructor
    , describeDBSnapshotsResponse
    -- ** Response lenses
    , ddbsrrMarker
    , ddbsrrDBSnapshot
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import Network.AWS.Prelude

-- | 
data DescribeDBSnapshots = DescribeDBSnapshots
    { _ddbs1DBInstanceIdentifier :: Maybe Text
    , _ddbs1DBSnapshotIdentifier :: Maybe Text
    , _ddbs1SnapshotType :: Maybe Text
    , _ddbs1MaxRecords :: Maybe Integer
    , _ddbs1Marker :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeDBSnapshots' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBInstanceIdentifier ::@ @Maybe Text@
--
-- * @DBSnapshotIdentifier ::@ @Maybe Text@
--
-- * @SnapshotType ::@ @Maybe Text@
--
-- * @MaxRecords ::@ @Maybe Integer@
--
-- * @Marker ::@ @Maybe Text@
--
describeDBSnapshots :: DescribeDBSnapshots
describeDBSnapshots = DescribeDBSnapshots
    { _ddbs1DBInstanceIdentifier = Nothing
    , _ddbs1DBSnapshotIdentifier = Nothing
    , _ddbs1SnapshotType = Nothing
    , _ddbs1MaxRecords = Nothing
    , _ddbs1Marker = Nothing
    }

-- | A DB instance identifier to retrieve the list of DB snapshots for. Cannot
-- be used in conjunction with DBSnapshotIdentifier. This parameter is not
-- case sensitive. Constraints: Must contain from 1 to 63 alphanumeric
-- characters or hyphens First character must be a letter Cannot end with a
-- hyphen or contain two consecutive hyphens.
ddbs1DBInstanceIdentifier :: Lens' DescribeDBSnapshots (Maybe Text)
ddbs1DBInstanceIdentifier =
    lens _ddbs1DBInstanceIdentifier
         (\s a -> s { _ddbs1DBInstanceIdentifier = a })

-- | A specific DB snapshot identifier to describe. Cannot be used in
-- conjunction with DBInstanceIdentifier. This value is stored as a lowercase
-- string. Constraints: Must be 1 to 255 alphanumeric characters First
-- character must be a letter Cannot end with a hyphen or contain two
-- consecutive hyphens If this is the identifier of an automated snapshot, the
-- SnapshotType parameter must also be specified.
ddbs1DBSnapshotIdentifier :: Lens' DescribeDBSnapshots (Maybe Text)
ddbs1DBSnapshotIdentifier =
    lens _ddbs1DBSnapshotIdentifier
         (\s a -> s { _ddbs1DBSnapshotIdentifier = a })

-- | The type of snapshots that will be returned. Values can be "automated" or
-- "manual." If not specified, the returned results will include all snapshots
-- types.
ddbs1SnapshotType :: Lens' DescribeDBSnapshots (Maybe Text)
ddbs1SnapshotType =
    lens _ddbs1SnapshotType (\s a -> s { _ddbs1SnapshotType = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results may be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
ddbs1MaxRecords :: Lens' DescribeDBSnapshots (Maybe Integer)
ddbs1MaxRecords = lens _ddbs1MaxRecords (\s a -> s { _ddbs1MaxRecords = a })

-- | An optional pagination token provided by a previous DescribeDBSnapshots
-- request. If this parameter is specified, the response includes only records
-- beyond the marker, up to the value specified by MaxRecords.
ddbs1Marker :: Lens' DescribeDBSnapshots (Maybe Text)
ddbs1Marker = lens _ddbs1Marker (\s a -> s { _ddbs1Marker = a })

instance ToQuery DescribeDBSnapshots where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the DescribeDBSnapshots
-- action.
data DescribeDBSnapshotsResponse = DescribeDBSnapshotsResponse
    { _ddbsrrMarker :: Maybe Text
    , _ddbsrrDBSnapshot :: [DBSnapshot]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeDBSnapshotsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Marker ::@ @Maybe Text@
--
-- * @DBSnapshot ::@ @[DBSnapshot]@
--
describeDBSnapshotsResponse :: DescribeDBSnapshotsResponse
describeDBSnapshotsResponse = DescribeDBSnapshotsResponse
    { _ddbsrrMarker = Nothing
    , _ddbsrrDBSnapshot = mempty
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
ddbsrrMarker :: Lens' DescribeDBSnapshotsResponse (Maybe Text)
ddbsrrMarker = lens _ddbsrrMarker (\s a -> s { _ddbsrrMarker = a })

-- | A list of DBSnapshot instances.
ddbsrrDBSnapshot :: Lens' DescribeDBSnapshotsResponse [DBSnapshot]
ddbsrrDBSnapshot =
    lens _ddbsrrDBSnapshot (\s a -> s { _ddbsrrDBSnapshot = a })

instance FromXML DescribeDBSnapshotsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeDBSnapshots where
    type Sv DescribeDBSnapshots = RDS
    type Rs DescribeDBSnapshots = DescribeDBSnapshotsResponse

    request = post "DescribeDBSnapshots"
    response _ = xmlResponse

instance AWSPager DescribeDBSnapshots where
    next rq rs = (\x -> rq & ddbs1Marker ?~ x)
        <$> (rs ^. ddbsrrMarker)
