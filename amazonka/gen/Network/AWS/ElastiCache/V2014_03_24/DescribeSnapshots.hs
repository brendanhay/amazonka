{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElastiCache.V2014_03_24.DescribeSnapshots
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
module Network.AWS.ElastiCache.V2014_03_24.DescribeSnapshots where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_03_24.Types
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
      -- ^ Indicates whether the snapshot is from an automatic backup
      -- (automated) or was created manually (manual).
    } deriving (Generic)

makeLenses ''DescribeSnapshots

instance ToQuery DescribeSnapshots where
    toQuery = genericToQuery def

data DescribeSnapshotsResponse = DescribeSnapshotsResponse
    { _dslmSnapshots :: [Snapshot]
      -- ^ A list of snapshots. Each item in the list contains detailed
      -- information about one snapshot.
    , _dslmMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker
      -- for pagination of results from this operation. If this parameter
      -- is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    } deriving (Generic)

makeLenses ''DescribeSnapshotsResponse

instance FromXML DescribeSnapshotsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeSnapshots where
    type Sv DescribeSnapshots = ElastiCache
    type Rs DescribeSnapshots = DescribeSnapshotsResponse

    request = post "DescribeSnapshots"
    response _ = xmlResponse
