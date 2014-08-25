{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.ElastiCache.V2014_07_15.DescribeSnapshots where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeSnapshots' request.
describeSnapshots :: DescribeSnapshots
describeSnapshots = DescribeSnapshots
    { _dsmMaxRecords = Nothing
    , _dsmCacheClusterId = Nothing
    , _dsmMarker = Nothing
    , _dsmSnapshotName = Nothing
    , _dsmSnapshotSource = Nothing
    }

data DescribeSnapshots = DescribeSnapshots
    { _dsmMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a marker is
      -- included in the response so that the remaining results can be
      -- retrieved. Default: 50 Constraints: minimum 20; maximum 50.
    , _dsmCacheClusterId :: Maybe Text
      -- ^ A user-supplied cluster identifier. If this parameter is
      -- specified, only snapshots associated with that specific cache
      -- cluster will be described.
    , _dsmMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker
      -- for pagination of results from this operation. If this parameter
      -- is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    , _dsmSnapshotName :: Maybe Text
      -- ^ A user-supplied name of the snapshot. If this parameter is
      -- specified, only this snapshot will be described.
    , _dsmSnapshotSource :: Maybe Text
      -- ^ If set to system, the output shows snapshots that were
      -- automatically created by ElastiCache. If set to user the output
      -- shows snapshots that were manually created. If omitted, the
      -- output shows both automatically and manually created snapshots.
    } deriving (Show, Generic)

makeLenses ''DescribeSnapshots

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

makeLenses ''DescribeSnapshotsResponse

instance FromXML DescribeSnapshotsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeSnapshots where
    type Sv DescribeSnapshots = ElastiCache
    type Rs DescribeSnapshots = DescribeSnapshotsResponse

    request = post "DescribeSnapshots"
    response _ = xmlResponse
