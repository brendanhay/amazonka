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
    , describeReplicationGroups
    -- ** Request lenses
    , drgnMaxRecords
    , drgnReplicationGroupId
    , drgnMarker

    -- * Response
    , DescribeReplicationGroupsResponse
    -- ** Response lenses
    , rgmReplicationGroups
    , rgmMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeReplicationGroups' request.
describeReplicationGroups :: DescribeReplicationGroups
describeReplicationGroups = DescribeReplicationGroups
    { _drgnMaxRecords = Nothing
    , _drgnReplicationGroupId = Nothing
    , _drgnMarker = Nothing
    }

data DescribeReplicationGroups = DescribeReplicationGroups
    { _drgnMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a marker is
      -- included in the response so that the remaining results can be
      -- retrieved. Default: 100 Constraints: minimum 20; maximum 100.
    , _drgnReplicationGroupId :: Maybe Text
      -- ^ The identifier for the replication group to be described. This
      -- parameter is not case sensitive. If you do not specify this
      -- parameter, information about all replication groups is returned.
    , _drgnMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker
      -- for pagination of results from this operation. If this parameter
      -- is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
drgnMaxRecords
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeReplicationGroups
    -> f DescribeReplicationGroups
drgnMaxRecords f x =
    (\y -> x { _drgnMaxRecords = y })
       <$> f (_drgnMaxRecords x)
{-# INLINE drgnMaxRecords #-}

-- | The identifier for the replication group to be described. This parameter is
-- not case sensitive. If you do not specify this parameter, information about
-- all replication groups is returned.
drgnReplicationGroupId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeReplicationGroups
    -> f DescribeReplicationGroups
drgnReplicationGroupId f x =
    (\y -> x { _drgnReplicationGroupId = y })
       <$> f (_drgnReplicationGroupId x)
{-# INLINE drgnReplicationGroupId #-}

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
drgnMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeReplicationGroups
    -> f DescribeReplicationGroups
drgnMarker f x =
    (\y -> x { _drgnMarker = y })
       <$> f (_drgnMarker x)
{-# INLINE drgnMarker #-}

instance ToQuery DescribeReplicationGroups where
    toQuery = genericQuery def

data DescribeReplicationGroupsResponse = DescribeReplicationGroupsResponse
    { _rgmReplicationGroups :: [ReplicationGroup]
      -- ^ A list of replication groups. Each item in the list contains
      -- detailed information about one replication group.
    , _rgmMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    } deriving (Show, Generic)

-- | A list of replication groups. Each item in the list contains detailed
-- information about one replication group.
rgmReplicationGroups
    :: Functor f
    => ([ReplicationGroup]
    -> f ([ReplicationGroup]))
    -> DescribeReplicationGroupsResponse
    -> f DescribeReplicationGroupsResponse
rgmReplicationGroups f x =
    (\y -> x { _rgmReplicationGroups = y })
       <$> f (_rgmReplicationGroups x)
{-# INLINE rgmReplicationGroups #-}

-- | Provides an identifier to allow retrieval of paginated results.
rgmMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeReplicationGroupsResponse
    -> f DescribeReplicationGroupsResponse
rgmMarker f x =
    (\y -> x { _rgmMarker = y })
       <$> f (_rgmMarker x)
{-# INLINE rgmMarker #-}

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
