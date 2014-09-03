{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.DescribeSnapshots
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeSnapshots operation returns information about cache cluster
-- snapshots. By default, DescribeSnapshots lists all of your snapshots; it
-- can optionally describe a single snapshot, or just the snapshots associated
-- with a particular cache cluster.
-- https://elasticache.us-east-1.amazonaws.com/ ?Action=DescribeSnapshots
-- &MaxRecords=50 &Version=2014-03-24 &SignatureVersion=4
-- &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z &X-Amz-Credential=
-- my-redis-primary 6379 cache.m1.small default.redis2.8 redis us-east-1d
-- 2014-04-01T18:46:57.972Z 2.8.6 manual true wed:09:00-wed:10:00
-- my-manual-snapshot 5 2014-04-01T18:54:12Z 2014-04-01T18:46:57.972Z 0001 3
-- MB creating 1 07:30-08:30 51b0b25e-b9cf-11e3-8a16-7978bb24ffdf.
module Network.AWS.ElastiCache.V2014_07_15.DescribeSnapshots
    (
    -- * Request
      DescribeSnapshots
    -- ** Request constructor
    , describeSnapshots
    -- ** Request lenses
    , dsnMaxRecords
    , dsnCacheClusterId
    , dsnSnapshotName
    , dsnSnapshotSource
    , dsnMarker

    -- * Response
    , DescribeSnapshotsResponse
    -- ** Response lenses
    , dslmSnapshots
    , dslmMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeSnapshots' request.
describeSnapshots :: DescribeSnapshots
describeSnapshots = DescribeSnapshots
    { _dsnMaxRecords = Nothing
    , _dsnCacheClusterId = Nothing
    , _dsnSnapshotName = Nothing
    , _dsnSnapshotSource = Nothing
    , _dsnMarker = Nothing
    }

data DescribeSnapshots = DescribeSnapshots
    { _dsnMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a marker is
      -- included in the response so that the remaining results can be
      -- retrieved. Default: 50 Constraints: minimum 20; maximum 50.
    , _dsnCacheClusterId :: Maybe Text
      -- ^ A user-supplied cluster identifier. If this parameter is
      -- specified, only snapshots associated with that specific cache
      -- cluster will be described.
    , _dsnSnapshotName :: Maybe Text
      -- ^ A user-supplied name of the snapshot. If this parameter is
      -- specified, only this snapshot will be described.
    , _dsnSnapshotSource :: Maybe Text
      -- ^ If set to system, the output shows snapshots that were
      -- automatically created by ElastiCache. If set to user the output
      -- shows snapshots that were manually created. If omitted, the
      -- output shows both automatically and manually created snapshots.
    , _dsnMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker
      -- for pagination of results from this operation. If this parameter
      -- is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 50
-- Constraints: minimum 20; maximum 50.
dsnMaxRecords
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeSnapshots
    -> f DescribeSnapshots
dsnMaxRecords f x =
    (\y -> x { _dsnMaxRecords = y })
       <$> f (_dsnMaxRecords x)
{-# INLINE dsnMaxRecords #-}

-- | A user-supplied cluster identifier. If this parameter is specified, only
-- snapshots associated with that specific cache cluster will be described.
dsnCacheClusterId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeSnapshots
    -> f DescribeSnapshots
dsnCacheClusterId f x =
    (\y -> x { _dsnCacheClusterId = y })
       <$> f (_dsnCacheClusterId x)
{-# INLINE dsnCacheClusterId #-}

-- | A user-supplied name of the snapshot. If this parameter is specified, only
-- this snapshot will be described.
dsnSnapshotName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeSnapshots
    -> f DescribeSnapshots
dsnSnapshotName f x =
    (\y -> x { _dsnSnapshotName = y })
       <$> f (_dsnSnapshotName x)
{-# INLINE dsnSnapshotName #-}

-- | If set to system, the output shows snapshots that were automatically
-- created by ElastiCache. If set to user the output shows snapshots that were
-- manually created. If omitted, the output shows both automatically and
-- manually created snapshots.
dsnSnapshotSource
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeSnapshots
    -> f DescribeSnapshots
dsnSnapshotSource f x =
    (\y -> x { _dsnSnapshotSource = y })
       <$> f (_dsnSnapshotSource x)
{-# INLINE dsnSnapshotSource #-}

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
dsnMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeSnapshots
    -> f DescribeSnapshots
dsnMarker f x =
    (\y -> x { _dsnMarker = y })
       <$> f (_dsnMarker x)
{-# INLINE dsnMarker #-}

instance ToQuery DescribeSnapshots where
    toQuery = genericQuery def

data DescribeSnapshotsResponse = DescribeSnapshotsResponse
    { _dslmSnapshots :: [Snapshot]
      -- ^ A list of snapshots. Each item in the list contains detailed
      -- information about one snapshot.
    , _dslmMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker
      -- for pagination of results from this operation. If this parameter
      -- is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

-- | A list of snapshots. Each item in the list contains detailed information
-- about one snapshot.
dslmSnapshots
    :: Functor f
    => ([Snapshot]
    -> f ([Snapshot]))
    -> DescribeSnapshotsResponse
    -> f DescribeSnapshotsResponse
dslmSnapshots f x =
    (\y -> x { _dslmSnapshots = y })
       <$> f (_dslmSnapshots x)
{-# INLINE dslmSnapshots #-}

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
dslmMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeSnapshotsResponse
    -> f DescribeSnapshotsResponse
dslmMarker f x =
    (\y -> x { _dslmMarker = y })
       <$> f (_dslmMarker x)
{-# INLINE dslmMarker #-}

instance FromXML DescribeSnapshotsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeSnapshots where
    type Sv DescribeSnapshots = ElastiCache
    type Rs DescribeSnapshots = DescribeSnapshotsResponse

    request = post "DescribeSnapshots"
    response _ = xmlResponse
