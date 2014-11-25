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
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeDBSnapshots.html>
module Network.AWS.RDS.DescribeDBSnapshots
    (
    -- * Request
      DescribeDBSnapshots
    -- ** Request constructor
    , describeDBSnapshots
    -- ** Request lenses
    , ddbsDBInstanceIdentifier
    , ddbsDBSnapshotIdentifier
    , ddbsFilters
    , ddbsMarker
    , ddbsMaxRecords
    , ddbsSnapshotType

    -- * Response
    , DescribeDBSnapshotsResponse
    -- ** Response constructor
    , describeDBSnapshotsResponse
    -- ** Response lenses
    , ddbsrDBSnapshots
    , ddbsrMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data DescribeDBSnapshots = DescribeDBSnapshots
    { _ddbsDBInstanceIdentifier :: Maybe Text
    , _ddbsDBSnapshotIdentifier :: Maybe Text
    , _ddbsFilters              :: List "Filter" Filter
    , _ddbsMarker               :: Maybe Text
    , _ddbsMaxRecords           :: Maybe Int
    , _ddbsSnapshotType         :: Maybe Text
    } deriving (Eq, Show)

-- | 'DescribeDBSnapshots' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbsDBInstanceIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'ddbsDBSnapshotIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'ddbsFilters' @::@ ['Filter']
--
-- * 'ddbsMarker' @::@ 'Maybe' 'Text'
--
-- * 'ddbsMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'ddbsSnapshotType' @::@ 'Maybe' 'Text'
--
describeDBSnapshots :: DescribeDBSnapshots
describeDBSnapshots = DescribeDBSnapshots
    { _ddbsDBInstanceIdentifier = Nothing
    , _ddbsDBSnapshotIdentifier = Nothing
    , _ddbsSnapshotType         = Nothing
    , _ddbsFilters              = mempty
    , _ddbsMaxRecords           = Nothing
    , _ddbsMarker               = Nothing
    }

-- | A DB instance identifier to retrieve the list of DB snapshots for. Cannot be
-- used in conjunction with 'DBSnapshotIdentifier'. This parameter is not case
-- sensitive.
--
-- Constraints:
--
-- Must contain from 1 to 63 alphanumeric characters or hyphens First
-- character must be a letter Cannot end with a hyphen or contain two
-- consecutive hyphens
ddbsDBInstanceIdentifier :: Lens' DescribeDBSnapshots (Maybe Text)
ddbsDBInstanceIdentifier =
    lens _ddbsDBInstanceIdentifier
        (\s a -> s { _ddbsDBInstanceIdentifier = a })

-- | A specific DB snapshot identifier to describe. Cannot be used in conjunction
-- with 'DBInstanceIdentifier'. This value is stored as a lowercase string.
--
-- Constraints:
--
-- Must be 1 to 255 alphanumeric characters First character must be a letter Cannot end with a hyphen or contain two consecutive hyphens
-- If this is the identifier of an automated snapshot, the 'SnapshotType'
-- parameter must also be specified.
ddbsDBSnapshotIdentifier :: Lens' DescribeDBSnapshots (Maybe Text)
ddbsDBSnapshotIdentifier =
    lens _ddbsDBSnapshotIdentifier
        (\s a -> s { _ddbsDBSnapshotIdentifier = a })

-- | This parameter is not currently supported.
--
ddbsFilters :: Lens' DescribeDBSnapshots [Filter]
ddbsFilters = lens _ddbsFilters (\s a -> s { _ddbsFilters = a }) . _List

-- | An optional pagination token provided by a previous 'DescribeDBSnapshots'
-- request. If this parameter is specified, the response includes only records
-- beyond the marker, up to the value specified by 'MaxRecords'.
--
ddbsMarker :: Lens' DescribeDBSnapshots (Maybe Text)
ddbsMarker = lens _ddbsMarker (\s a -> s { _ddbsMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified 'MaxRecords' value, a pagination token called a marker
-- is included in the response so that the remaining results may be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
--
ddbsMaxRecords :: Lens' DescribeDBSnapshots (Maybe Int)
ddbsMaxRecords = lens _ddbsMaxRecords (\s a -> s { _ddbsMaxRecords = a })

-- | The type of snapshots that will be returned. Values can be "automated" or
-- "manual." If not specified, the returned results will include all snapshots
-- types.
--
ddbsSnapshotType :: Lens' DescribeDBSnapshots (Maybe Text)
ddbsSnapshotType = lens _ddbsSnapshotType (\s a -> s { _ddbsSnapshotType = a })

data DescribeDBSnapshotsResponse = DescribeDBSnapshotsResponse
    { _ddbsrDBSnapshots :: List "DBSnapshot" DBSnapshot
    , _ddbsrMarker      :: Maybe Text
    } deriving (Eq, Show)

-- | 'DescribeDBSnapshotsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbsrDBSnapshots' @::@ ['DBSnapshot']
--
-- * 'ddbsrMarker' @::@ 'Maybe' 'Text'
--
describeDBSnapshotsResponse :: DescribeDBSnapshotsResponse
describeDBSnapshotsResponse = DescribeDBSnapshotsResponse
    { _ddbsrMarker      = Nothing
    , _ddbsrDBSnapshots = mempty
    }

-- | A list of 'DBSnapshot' instances.
--
ddbsrDBSnapshots :: Lens' DescribeDBSnapshotsResponse [DBSnapshot]
ddbsrDBSnapshots = lens _ddbsrDBSnapshots (\s a -> s { _ddbsrDBSnapshots = a }) . _List

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the marker,
-- up to the value specified by 'MaxRecords'.
--
ddbsrMarker :: Lens' DescribeDBSnapshotsResponse (Maybe Text)
ddbsrMarker = lens _ddbsrMarker (\s a -> s { _ddbsrMarker = a })

instance ToPath DescribeDBSnapshots where
    toPath = const "/"

instance ToQuery DescribeDBSnapshots where
    toQuery DescribeDBSnapshots{..} = mconcat
        [ "DBInstanceIdentifier" =? _ddbsDBInstanceIdentifier
        , "DBSnapshotIdentifier" =? _ddbsDBSnapshotIdentifier
        , "Filters"              =? _ddbsFilters
        , "Marker"               =? _ddbsMarker
        , "MaxRecords"           =? _ddbsMaxRecords
        , "SnapshotType"         =? _ddbsSnapshotType
        ]

instance ToHeaders DescribeDBSnapshots

instance AWSRequest DescribeDBSnapshots where
    type Sv DescribeDBSnapshots = RDS
    type Rs DescribeDBSnapshots = DescribeDBSnapshotsResponse

    request  = post "DescribeDBSnapshots"
    response = xmlResponse

instance FromXML DescribeDBSnapshotsResponse where
    parseXML = withElement "DescribeDBSnapshotsResult" $ \x -> DescribeDBSnapshotsResponse
        <$> x .@  "DBSnapshots"
        <*> x .@? "Marker"

instance AWSPager DescribeDBSnapshots where
    page rq rs
        | stop (rq ^. ddbsMarker) = Nothing
        | otherwise = (\x -> rq & ddbsMarker ?~ x)
            <$> (rs ^. ddbsrMarker)
