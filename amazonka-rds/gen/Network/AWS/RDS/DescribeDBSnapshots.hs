{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.RDS.DescribeDBSnapshots
    (
    -- * Request
      DescribeDBSnapshotsMessage
    -- ** Request constructor
    , describeDBSnapshotsMessage
    -- ** Request lenses
    , ddbsmDBInstanceIdentifier
    , ddbsmDBSnapshotIdentifier
    , ddbsmFilters
    , ddbsmMarker
    , ddbsmMaxRecords
    , ddbsmSnapshotType

    -- * Response
    , DBSnapshotMessage
    -- ** Response constructor
    , dbsnapshotMessage
    -- ** Response lenses
    , dbsmDBSnapshots
    , dbsmMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data DescribeDBSnapshotsMessage = DescribeDBSnapshotsMessage
    { _ddbsmDBInstanceIdentifier :: Maybe Text
    , _ddbsmDBSnapshotIdentifier :: Maybe Text
    , _ddbsmFilters              :: [Filter]
    , _ddbsmMarker               :: Maybe Text
    , _ddbsmMaxRecords           :: Maybe Int
    , _ddbsmSnapshotType         :: Maybe Text
    } (Eq, Show, Generic)

-- | 'DescribeDBSnapshotsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbsmDBInstanceIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'ddbsmDBSnapshotIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'ddbsmFilters' @::@ ['Filter']
--
-- * 'ddbsmMarker' @::@ 'Maybe' 'Text'
--
-- * 'ddbsmMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'ddbsmSnapshotType' @::@ 'Maybe' 'Text'
--
describeDBSnapshotsMessage :: DescribeDBSnapshotsMessage
describeDBSnapshotsMessage = DescribeDBSnapshotsMessage
    { _ddbsmDBInstanceIdentifier = Nothing
    , _ddbsmDBSnapshotIdentifier = Nothing
    , _ddbsmSnapshotType         = Nothing
    , _ddbsmFilters              = mempty
    , _ddbsmMaxRecords           = Nothing
    , _ddbsmMarker               = Nothing
    }

-- | A DB instance identifier to retrieve the list of DB snapshots for. Cannot
-- be used in conjunction with DBSnapshotIdentifier. This parameter is not
-- case sensitive. Constraints: Must contain from 1 to 63 alphanumeric
-- characters or hyphens First character must be a letter Cannot end with a
-- hyphen or contain two consecutive hyphens.
ddbsmDBInstanceIdentifier :: Lens' DescribeDBSnapshotsMessage (Maybe Text)
ddbsmDBInstanceIdentifier =
    lens _ddbsmDBInstanceIdentifier
        (\s a -> s { _ddbsmDBInstanceIdentifier = a })

-- | A specific DB snapshot identifier to describe. Cannot be used in
-- conjunction with DBInstanceIdentifier. This value is stored as a
-- lowercase string. Constraints: Must be 1 to 255 alphanumeric characters
-- First character must be a letter Cannot end with a hyphen or contain two
-- consecutive hyphens If this is the identifier of an automated snapshot,
-- the SnapshotType parameter must also be specified.
ddbsmDBSnapshotIdentifier :: Lens' DescribeDBSnapshotsMessage (Maybe Text)
ddbsmDBSnapshotIdentifier =
    lens _ddbsmDBSnapshotIdentifier
        (\s a -> s { _ddbsmDBSnapshotIdentifier = a })

-- | This parameter is not currently supported.
ddbsmFilters :: Lens' DescribeDBSnapshotsMessage [Filter]
ddbsmFilters = lens _ddbsmFilters (\s a -> s { _ddbsmFilters = a })

-- | An optional pagination token provided by a previous DescribeDBSnapshots
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by MaxRecords.
ddbsmMarker :: Lens' DescribeDBSnapshotsMessage (Maybe Text)
ddbsmMarker = lens _ddbsmMarker (\s a -> s { _ddbsmMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results may be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
ddbsmMaxRecords :: Lens' DescribeDBSnapshotsMessage (Maybe Int)
ddbsmMaxRecords = lens _ddbsmMaxRecords (\s a -> s { _ddbsmMaxRecords = a })

-- | The type of snapshots that will be returned. Values can be "automated" or
-- "manual." If not specified, the returned results will include all
-- snapshots types.
ddbsmSnapshotType :: Lens' DescribeDBSnapshotsMessage (Maybe Text)
ddbsmSnapshotType =
    lens _ddbsmSnapshotType (\s a -> s { _ddbsmSnapshotType = a })
instance ToQuery DescribeDBSnapshotsMessage

instance ToPath DescribeDBSnapshotsMessage where
    toPath = const "/"

data DBSnapshotMessage = DBSnapshotMessage
    { _dbsmDBSnapshots :: [DBSnapshot]
    , _dbsmMarker      :: Maybe Text
    } (Eq, Show, Generic)

-- | 'DBSnapshotMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbsmDBSnapshots' @::@ ['DBSnapshot']
--
-- * 'dbsmMarker' @::@ 'Maybe' 'Text'
--
dbsnapshotMessage :: DBSnapshotMessage
dbsnapshotMessage = DBSnapshotMessage
    { _dbsmMarker      = Nothing
    , _dbsmDBSnapshots = mempty
    }

-- | A list of DBSnapshot instances.
dbsmDBSnapshots :: Lens' DBSnapshotMessage [DBSnapshot]
dbsmDBSnapshots = lens _dbsmDBSnapshots (\s a -> s { _dbsmDBSnapshots = a })

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
dbsmMarker :: Lens' DBSnapshotMessage (Maybe Text)
dbsmMarker = lens _dbsmMarker (\s a -> s { _dbsmMarker = a })

instance FromXML DBSnapshotMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBSnapshotMessage"

instance AWSRequest DescribeDBSnapshotsMessage where
    type Sv DescribeDBSnapshotsMessage = RDS
    type Rs DescribeDBSnapshotsMessage = DBSnapshotMessage

    request  = post "DescribeDBSnapshots"
    response = xmlResponse $ \h x -> DBSnapshotMessage
        <$> x %| "DBSnapshots"
        <*> x %| "Marker"
