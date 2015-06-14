{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElastiCache.DescribeReplicationGroups
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | The /DescribeReplicationGroups/ action returns information about a
-- particular replication group. If no identifier is specified,
-- /DescribeReplicationGroups/ returns information about all replication
-- groups.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeReplicationGroups.html>
module Network.AWS.ElastiCache.DescribeReplicationGroups
    (
    -- * Request
      DescribeReplicationGroups
    -- ** Request constructor
    , describeReplicationGroups
    -- ** Request lenses
    , dMaxRecords
    , dMarker
    , dReplicationGroupId

    -- * Response
    , DescribeReplicationGroupsResponse
    -- ** Response constructor
    , describeReplicationGroupsResponse
    -- ** Response lenses
    , drgrMarker
    , drgrReplicationGroups
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ElastiCache.Types

-- | /See:/ 'describeReplicationGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dMaxRecords'
--
-- * 'dMarker'
--
-- * 'dReplicationGroupId'
data DescribeReplicationGroups = DescribeReplicationGroups'{_dMaxRecords :: Maybe Int, _dMarker :: Maybe Text, _dReplicationGroupId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeReplicationGroups' smart constructor.
describeReplicationGroups :: DescribeReplicationGroups
describeReplicationGroups = DescribeReplicationGroups'{_dMaxRecords = Nothing, _dMarker = Nothing, _dReplicationGroupId = Nothing};

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
dMaxRecords :: Lens' DescribeReplicationGroups (Maybe Int)
dMaxRecords = lens _dMaxRecords (\ s a -> s{_dMaxRecords = a});

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by /MaxRecords/.
dMarker :: Lens' DescribeReplicationGroups (Maybe Text)
dMarker = lens _dMarker (\ s a -> s{_dMarker = a});

-- | The identifier for the replication group to be described. This parameter
-- is not case sensitive.
--
-- If you do not specify this parameter, information about all replication
-- groups is returned.
dReplicationGroupId :: Lens' DescribeReplicationGroups (Maybe Text)
dReplicationGroupId = lens _dReplicationGroupId (\ s a -> s{_dReplicationGroupId = a});

instance AWSRequest DescribeReplicationGroups where
        type Sv DescribeReplicationGroups = ElastiCache
        type Rs DescribeReplicationGroups =
             DescribeReplicationGroupsResponse
        request = post
        response
          = receiveXMLWrapper "DescribeReplicationGroupsResult"
              (\ s h x ->
                 DescribeReplicationGroupsResponse' <$>
                   x .@? "Marker" <*>
                     (x .@? "ReplicationGroups" .!@ mempty >>=
                        parseXMLList "ReplicationGroup"))

instance ToHeaders DescribeReplicationGroups where
        toHeaders = const mempty

instance ToPath DescribeReplicationGroups where
        toPath = const "/"

instance ToQuery DescribeReplicationGroups where
        toQuery DescribeReplicationGroups'{..}
          = mconcat
              ["Action" =:
                 ("DescribeReplicationGroups" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "MaxRecords" =: _dMaxRecords, "Marker" =: _dMarker,
               "ReplicationGroupId" =: _dReplicationGroupId]

-- | /See:/ 'describeReplicationGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drgrMarker'
--
-- * 'drgrReplicationGroups'
data DescribeReplicationGroupsResponse = DescribeReplicationGroupsResponse'{_drgrMarker :: Maybe Text, _drgrReplicationGroups :: [ReplicationGroup]} deriving (Eq, Read, Show)

-- | 'DescribeReplicationGroupsResponse' smart constructor.
describeReplicationGroupsResponse :: DescribeReplicationGroupsResponse
describeReplicationGroupsResponse = DescribeReplicationGroupsResponse'{_drgrMarker = Nothing, _drgrReplicationGroups = mempty};

-- | Provides an identifier to allow retrieval of paginated results.
drgrMarker :: Lens' DescribeReplicationGroupsResponse (Maybe Text)
drgrMarker = lens _drgrMarker (\ s a -> s{_drgrMarker = a});

-- | A list of replication groups. Each item in the list contains detailed
-- information about one replication group.
drgrReplicationGroups :: Lens' DescribeReplicationGroupsResponse [ReplicationGroup]
drgrReplicationGroups = lens _drgrReplicationGroups (\ s a -> s{_drgrReplicationGroups = a});
