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
    , mkDescribeReplicationGroups
    -- ** Request lenses
    , drg1ReplicationGroupId
    , drg1MaxRecords
    , drg1Marker

    -- * Response
    , DescribeReplicationGroupsResponse
    -- ** Response constructor
    , mkDescribeReplicationGroupsResponse
    -- ** Response lenses
    , drgrrMarker
    , drgrrReplicationGroups
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Represents the input of a DescribeReplicationGroups operation.
data DescribeReplicationGroups = DescribeReplicationGroups
    { _drg1ReplicationGroupId :: Maybe Text
    , _drg1MaxRecords :: Maybe Integer
    , _drg1Marker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeReplicationGroups' request.
mkDescribeReplicationGroups :: DescribeReplicationGroups
mkDescribeReplicationGroups = DescribeReplicationGroups
    { _drg1ReplicationGroupId = Nothing
    , _drg1MaxRecords = Nothing
    , _drg1Marker = Nothing
    }

-- | The identifier for the replication group to be described. This parameter is
-- not case sensitive. If you do not specify this parameter, information about
-- all replication groups is returned.
drg1ReplicationGroupId :: Lens' DescribeReplicationGroups (Maybe Text)
drg1ReplicationGroupId =
    lens _drg1ReplicationGroupId (\s a -> s { _drg1ReplicationGroupId = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
drg1MaxRecords :: Lens' DescribeReplicationGroups (Maybe Integer)
drg1MaxRecords = lens _drg1MaxRecords (\s a -> s { _drg1MaxRecords = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
drg1Marker :: Lens' DescribeReplicationGroups (Maybe Text)
drg1Marker = lens _drg1Marker (\s a -> s { _drg1Marker = a })

instance ToQuery DescribeReplicationGroups where
    toQuery = genericQuery def

-- | Represents the output of a DescribeReplicationGroups operation.
data DescribeReplicationGroupsResponse = DescribeReplicationGroupsResponse
    { _drgrrMarker :: Maybe Text
    , _drgrrReplicationGroups :: [ReplicationGroup]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeReplicationGroupsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDescribeReplicationGroupsResponse :: DescribeReplicationGroupsResponse
mkDescribeReplicationGroupsResponse = DescribeReplicationGroupsResponse
    { _drgrrMarker = Nothing
    , _drgrrReplicationGroups = mempty
    }

-- | Provides an identifier to allow retrieval of paginated results.
drgrrMarker :: Lens' DescribeReplicationGroupsResponse (Maybe Text)
drgrrMarker = lens _drgrrMarker (\s a -> s { _drgrrMarker = a })

-- | A list of replication groups. Each item in the list contains detailed
-- information about one replication group.
drgrrReplicationGroups :: Lens' DescribeReplicationGroupsResponse [ReplicationGroup]
drgrrReplicationGroups =
    lens _drgrrReplicationGroups (\s a -> s { _drgrrReplicationGroups = a })

instance FromXML DescribeReplicationGroupsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeReplicationGroups where
    type Sv DescribeReplicationGroups = ElastiCache
    type Rs DescribeReplicationGroups = DescribeReplicationGroupsResponse

    request = post "DescribeReplicationGroups"
    response _ = xmlResponse

instance AWSPager DescribeReplicationGroups where
    next rq rs = (\x -> rq & drg1Marker ?~ x)
        <$> (rs ^. drgrrMarker)
