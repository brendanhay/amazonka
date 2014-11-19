{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.DescribeReplicationGroups
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
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeReplicationGroups.html>
module Network.AWS.ElastiCache.DescribeReplicationGroups
    (
    -- * Request
      DescribeReplicationGroups
    -- ** Request constructor
    , describeReplicationGroups
    -- ** Request lenses
    , drg1Marker
    , drg1MaxRecords
    , drg1ReplicationGroupId

    -- * Response
    , DescribeReplicationGroupsResponse
    -- ** Response constructor
    , describeReplicationGroupsResponse
    -- ** Response lenses
    , drgrMarker
    , drgrReplicationGroups
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data DescribeReplicationGroups = DescribeReplicationGroups
    { _drg1Marker             :: Maybe Text
    , _drg1MaxRecords         :: Maybe Int
    , _drg1ReplicationGroupId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeReplicationGroups' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drg1Marker' @::@ 'Maybe' 'Text'
--
-- * 'drg1MaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'drg1ReplicationGroupId' @::@ 'Maybe' 'Text'
--
describeReplicationGroups :: DescribeReplicationGroups
describeReplicationGroups = DescribeReplicationGroups
    { _drg1ReplicationGroupId = Nothing
    , _drg1MaxRecords         = Nothing
    , _drg1Marker             = Nothing
    }

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
drg1Marker :: Lens' DescribeReplicationGroups (Maybe Text)
drg1Marker = lens _drg1Marker (\s a -> s { _drg1Marker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
drg1MaxRecords :: Lens' DescribeReplicationGroups (Maybe Int)
drg1MaxRecords = lens _drg1MaxRecords (\s a -> s { _drg1MaxRecords = a })

-- | The identifier for the replication group to be described. This parameter
-- is not case sensitive. If you do not specify this parameter, information
-- about all replication groups is returned.
drg1ReplicationGroupId :: Lens' DescribeReplicationGroups (Maybe Text)
drg1ReplicationGroupId =
    lens _drg1ReplicationGroupId (\s a -> s { _drg1ReplicationGroupId = a })

data DescribeReplicationGroupsResponse = DescribeReplicationGroupsResponse
    { _drgrMarker            :: Maybe Text
    , _drgrReplicationGroups :: [ReplicationGroup]
    } deriving (Eq, Show, Generic)

-- | 'DescribeReplicationGroupsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drgrMarker' @::@ 'Maybe' 'Text'
--
-- * 'drgrReplicationGroups' @::@ ['ReplicationGroup']
--
describeReplicationGroupsResponse :: DescribeReplicationGroupsResponse
describeReplicationGroupsResponse = DescribeReplicationGroupsResponse
    { _drgrMarker            = Nothing
    , _drgrReplicationGroups = mempty
    }

-- | Provides an identifier to allow retrieval of paginated results.
drgrMarker :: Lens' DescribeReplicationGroupsResponse (Maybe Text)
drgrMarker = lens _drgrMarker (\s a -> s { _drgrMarker = a })

-- | A list of replication groups. Each item in the list contains detailed
-- information about one replication group.
drgrReplicationGroups :: Lens' DescribeReplicationGroupsResponse [ReplicationGroup]
drgrReplicationGroups =
    lens _drgrReplicationGroups (\s a -> s { _drgrReplicationGroups = a })

instance ToPath DescribeReplicationGroups where
    toPath = const "/"

instance ToQuery DescribeReplicationGroups

instance ToHeaders DescribeReplicationGroups

instance AWSRequest DescribeReplicationGroups where
    type Sv DescribeReplicationGroups = ElastiCache
    type Rs DescribeReplicationGroups = DescribeReplicationGroupsResponse

    request  = post "DescribeReplicationGroups"
    response = xmlResponse

instance FromXML DescribeReplicationGroupsResponse where
    parseXML = withElement "DescribeReplicationGroupsResult" $ \x ->
        DescribeReplicationGroupsResponse
            <$> x .@? "Marker"
            <*> x .@ "ReplicationGroups"
