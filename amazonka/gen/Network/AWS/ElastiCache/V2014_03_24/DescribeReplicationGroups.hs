{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElastiCache.V2014_03_24.DescribeReplicationGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeReplicationGroups operation returns information about a
-- particular replication group. If no identifier is specified,
-- DescribeReplicationGroups returns information about all replication groups.
module Network.AWS.ElastiCache.V2014_03_24.DescribeReplicationGroups where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_03_24.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeReplicationGroups' request.
describeReplicationGroups :: DescribeReplicationGroups
describeReplicationGroups = DescribeReplicationGroups
    { _drgnMaxRecords = Nothing
    , _drgnMarker = Nothing
    , _drgnReplicationGroupId = Nothing
    }

data DescribeReplicationGroups = DescribeReplicationGroups
    { _drgnMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a marker is
      -- included in the response so that the remaining results can be
      -- retrieved. Default: 100 Constraints: minimum 20; maximum 100.
    , _drgnMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker
      -- for pagination of results from this operation. If this parameter
      -- is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    , _drgnReplicationGroupId :: Maybe Text
      -- ^ The identifier for the replication group to be described. This
      -- parameter is not case sensitive. If you do not specify this
      -- parameter, information about all replication groups is returned.
    } deriving (Generic)

makeLenses ''DescribeReplicationGroups

instance ToQuery DescribeReplicationGroups where
    toQuery = genericToQuery def

data DescribeReplicationGroupsResponse = DescribeReplicationGroupsResponse
    { _rgmReplicationGroups :: [ReplicationGroup]
      -- ^ A list of replication groups. Each item in the list contains
      -- detailed information about one replication group.
    , _rgmMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    } deriving (Generic)

makeLenses ''DescribeReplicationGroupsResponse

instance FromXML DescribeReplicationGroupsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeReplicationGroups where
    type Sv DescribeReplicationGroups = ElastiCache
    type Rs DescribeReplicationGroups = DescribeReplicationGroupsResponse

    request = post "DescribeReplicationGroups"
    response _ = xmlResponse

instance AWSPager DescribeReplicationGroups where
    next rq rs = (\x -> rq { _drgnMarker = Just x })
        <$> (_rgmMarker rs)
