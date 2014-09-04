{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.DescribeReplicationGroups
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
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=DescribeReplicationGroups &MaxRecords=100 &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= my-redis-primary my-redis-primary 0001 6379
-- my-repgroup.q68zge.ng.0001.use1devo.elmo-dev.amazonaws.com available
-- my-redis-primary 6379
-- my-redis-primary.q68zge.0001.use1devo.elmo-dev.amazonaws.com us-east-1d
-- 0001 primary my-repgroup available My replication group
-- 144745b0-b9d3-11e3-8a16-7978bb24ffdf.
module Network.AWS.ElastiCache.V2014_07_15.DescribeReplicationGroups
    (
    -- * Request
      DescribeReplicationGroups
    -- ** Request constructor
    , mkDescribeReplicationGroupsMessage
    -- ** Request lenses
    , drgnReplicationGroupId
    , drgnMaxRecords
    , drgnMarker

    -- * Response
    , DescribeReplicationGroupsResponse
    -- ** Response lenses
    , rgmMarker
    , rgmReplicationGroups
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeReplicationGroups' request.
mkDescribeReplicationGroupsMessage :: DescribeReplicationGroups
mkDescribeReplicationGroupsMessage = DescribeReplicationGroups
    { _drgnReplicationGroupId = Nothing
    , _drgnMaxRecords = Nothing
    , _drgnMarker = Nothing
    }
{-# INLINE mkDescribeReplicationGroupsMessage #-}

data DescribeReplicationGroups = DescribeReplicationGroups
    { _drgnReplicationGroupId :: Maybe Text
      -- ^ The identifier for the replication group to be described. This
      -- parameter is not case sensitive. If you do not specify this
      -- parameter, information about all replication groups is returned.
    , _drgnMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a marker is
      -- included in the response so that the remaining results can be
      -- retrieved. Default: 100 Constraints: minimum 20; maximum 100.
    , _drgnMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker
      -- for pagination of results from this operation. If this parameter
      -- is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

-- | The identifier for the replication group to be described. This parameter is
-- not case sensitive. If you do not specify this parameter, information about
-- all replication groups is returned.
drgnReplicationGroupId :: Lens' DescribeReplicationGroups (Maybe Text)
drgnReplicationGroupId = lens _drgnReplicationGroupId (\s a -> s { _drgnReplicationGroupId = a })
{-# INLINE drgnReplicationGroupId #-}

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
drgnMaxRecords :: Lens' DescribeReplicationGroups (Maybe Integer)
drgnMaxRecords = lens _drgnMaxRecords (\s a -> s { _drgnMaxRecords = a })
{-# INLINE drgnMaxRecords #-}

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
drgnMarker :: Lens' DescribeReplicationGroups (Maybe Text)
drgnMarker = lens _drgnMarker (\s a -> s { _drgnMarker = a })
{-# INLINE drgnMarker #-}

instance ToQuery DescribeReplicationGroups where
    toQuery = genericQuery def

data DescribeReplicationGroupsResponse = DescribeReplicationGroupsResponse
    { _rgmMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    , _rgmReplicationGroups :: [ReplicationGroup]
      -- ^ A list of replication groups. Each item in the list contains
      -- detailed information about one replication group.
    } deriving (Show, Generic)

-- | Provides an identifier to allow retrieval of paginated results.
rgmMarker :: Lens' DescribeReplicationGroupsResponse (Maybe Text)
rgmMarker = lens _rgmMarker (\s a -> s { _rgmMarker = a })
{-# INLINE rgmMarker #-}

-- | A list of replication groups. Each item in the list contains detailed
-- information about one replication group.
rgmReplicationGroups :: Lens' DescribeReplicationGroupsResponse ([ReplicationGroup])
rgmReplicationGroups = lens _rgmReplicationGroups (\s a -> s { _rgmReplicationGroups = a })
{-# INLINE rgmReplicationGroups #-}

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
