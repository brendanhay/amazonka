{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.DescribeDBSnapshots
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
module Network.AWS.RDS.V2013_09_09.DescribeDBSnapshots
    (
    -- * Request
      DescribeDBSnapshots
    -- ** Request constructor
    , describeDBSnapshots
    -- ** Request lenses
    , ddbsnMaxRecords
    , ddbsnDBInstanceIdentifier
    , ddbsnDBSnapshotIdentifier
    , ddbsnSnapshotType
    , ddbsnMarker

    -- * Response
    , DescribeDBSnapshotsResponse
    -- ** Response lenses
    , dbspDBSnapshots
    , dbspMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeDBSnapshots' request.
describeDBSnapshots :: DescribeDBSnapshots
describeDBSnapshots = DescribeDBSnapshots
    { _ddbsnMaxRecords = Nothing
    , _ddbsnDBInstanceIdentifier = Nothing
    , _ddbsnDBSnapshotIdentifier = Nothing
    , _ddbsnSnapshotType = Nothing
    , _ddbsnMarker = Nothing
    }

data DescribeDBSnapshots = DescribeDBSnapshots
    { _ddbsnMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a pagination
      -- token called a marker is included in the response so that the
      -- remaining results may be retrieved. Default: 100 Constraints:
      -- minimum 20, maximum 100.
    , _ddbsnDBInstanceIdentifier :: Maybe Text
      -- ^ A DB instance identifier to retrieve the list of DB snapshots
      -- for. Cannot be used in conjunction with DBSnapshotIdentifier.
      -- This parameter is not case sensitive. Constraints: Must contain
      -- from 1 to 63 alphanumeric characters or hyphens First character
      -- must be a letter Cannot end with a hyphen or contain two
      -- consecutive hyphens.
    , _ddbsnDBSnapshotIdentifier :: Maybe Text
      -- ^ A specific DB snapshot identifier to describe. Cannot be used in
      -- conjunction with DBInstanceIdentifier. This value is stored as a
      -- lowercase string. Constraints: Must be 1 to 255 alphanumeric
      -- characters First character must be a letter Cannot end with a
      -- hyphen or contain two consecutive hyphens If this is the
      -- identifier of an automated snapshot, the SnapshotType parameter
      -- must also be specified.
    , _ddbsnSnapshotType :: Maybe Text
      -- ^ The type of snapshots that will be returned. Values can be
      -- "automated" or "manual." If not specified, the returned results
      -- will include all snapshots types.
    , _ddbsnMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DescribeDBSnapshots request. If this parameter is specified, the
      -- response includes only records beyond the marker, up to the value
      -- specified by MaxRecords.
    } deriving (Show, Generic)

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results may be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
ddbsnMaxRecords
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeDBSnapshots
    -> f DescribeDBSnapshots
ddbsnMaxRecords f x =
    (\y -> x { _ddbsnMaxRecords = y })
       <$> f (_ddbsnMaxRecords x)
{-# INLINE ddbsnMaxRecords #-}

-- | A DB instance identifier to retrieve the list of DB snapshots for. Cannot
-- be used in conjunction with DBSnapshotIdentifier. This parameter is not
-- case sensitive. Constraints: Must contain from 1 to 63 alphanumeric
-- characters or hyphens First character must be a letter Cannot end with a
-- hyphen or contain two consecutive hyphens.
ddbsnDBInstanceIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeDBSnapshots
    -> f DescribeDBSnapshots
ddbsnDBInstanceIdentifier f x =
    (\y -> x { _ddbsnDBInstanceIdentifier = y })
       <$> f (_ddbsnDBInstanceIdentifier x)
{-# INLINE ddbsnDBInstanceIdentifier #-}

-- | A specific DB snapshot identifier to describe. Cannot be used in
-- conjunction with DBInstanceIdentifier. This value is stored as a lowercase
-- string. Constraints: Must be 1 to 255 alphanumeric characters First
-- character must be a letter Cannot end with a hyphen or contain two
-- consecutive hyphens If this is the identifier of an automated snapshot, the
-- SnapshotType parameter must also be specified.
ddbsnDBSnapshotIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeDBSnapshots
    -> f DescribeDBSnapshots
ddbsnDBSnapshotIdentifier f x =
    (\y -> x { _ddbsnDBSnapshotIdentifier = y })
       <$> f (_ddbsnDBSnapshotIdentifier x)
{-# INLINE ddbsnDBSnapshotIdentifier #-}

-- | The type of snapshots that will be returned. Values can be "automated" or
-- "manual." If not specified, the returned results will include all snapshots
-- types.
ddbsnSnapshotType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeDBSnapshots
    -> f DescribeDBSnapshots
ddbsnSnapshotType f x =
    (\y -> x { _ddbsnSnapshotType = y })
       <$> f (_ddbsnSnapshotType x)
{-# INLINE ddbsnSnapshotType #-}

-- | An optional pagination token provided by a previous DescribeDBSnapshots
-- request. If this parameter is specified, the response includes only records
-- beyond the marker, up to the value specified by MaxRecords.
ddbsnMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeDBSnapshots
    -> f DescribeDBSnapshots
ddbsnMarker f x =
    (\y -> x { _ddbsnMarker = y })
       <$> f (_ddbsnMarker x)
{-# INLINE ddbsnMarker #-}

instance ToQuery DescribeDBSnapshots where
    toQuery = genericQuery def

data DescribeDBSnapshotsResponse = DescribeDBSnapshotsResponse
    { _dbspDBSnapshots :: [DBSnapshot]
      -- ^ A list of DBSnapshot instances.
    , _dbspMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If
      -- this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

-- | A list of DBSnapshot instances.
dbspDBSnapshots
    :: Functor f
    => ([DBSnapshot]
    -> f ([DBSnapshot]))
    -> DescribeDBSnapshotsResponse
    -> f DescribeDBSnapshotsResponse
dbspDBSnapshots f x =
    (\y -> x { _dbspDBSnapshots = y })
       <$> f (_dbspDBSnapshots x)
{-# INLINE dbspDBSnapshots #-}

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
dbspMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeDBSnapshotsResponse
    -> f DescribeDBSnapshotsResponse
dbspMarker f x =
    (\y -> x { _dbspMarker = y })
       <$> f (_dbspMarker x)
{-# INLINE dbspMarker #-}

instance FromXML DescribeDBSnapshotsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeDBSnapshots where
    type Sv DescribeDBSnapshots = RDS
    type Rs DescribeDBSnapshots = DescribeDBSnapshotsResponse

    request = post "DescribeDBSnapshots"
    response _ = xmlResponse

instance AWSPager DescribeDBSnapshots where
    next rq rs = (\x -> rq { _ddbsnMarker = Just x })
        <$> (_dbspMarker rs)
