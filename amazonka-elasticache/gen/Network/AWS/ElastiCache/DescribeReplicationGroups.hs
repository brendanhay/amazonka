{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeReplicationGroups
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /DescribeReplicationGroups/ action returns information about a
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
    , drgsrqMaxRecords
    , drgsrqMarker
    , drgsrqReplicationGroupId

    -- * Response
    , DescribeReplicationGroupsResponse
    -- ** Response constructor
    , describeReplicationGroupsResponse
    -- ** Response lenses
    , drgsrsMarker
    , drgsrsReplicationGroups
    , drgsrsStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /DescribeReplicationGroups/ action.
--
-- /See:/ 'describeReplicationGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drgsrqMaxRecords'
--
-- * 'drgsrqMarker'
--
-- * 'drgsrqReplicationGroupId'
data DescribeReplicationGroups = DescribeReplicationGroups'
    { _drgsrqMaxRecords         :: !(Maybe Int)
    , _drgsrqMarker             :: !(Maybe Text)
    , _drgsrqReplicationGroupId :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeReplicationGroups' smart constructor.
describeReplicationGroups :: DescribeReplicationGroups
describeReplicationGroups =
    DescribeReplicationGroups'
    { _drgsrqMaxRecords = Nothing
    , _drgsrqMarker = Nothing
    , _drgsrqReplicationGroupId = Nothing
    }

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
drgsrqMaxRecords :: Lens' DescribeReplicationGroups (Maybe Int)
drgsrqMaxRecords = lens _drgsrqMaxRecords (\ s a -> s{_drgsrqMaxRecords = a});

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by /MaxRecords/.
drgsrqMarker :: Lens' DescribeReplicationGroups (Maybe Text)
drgsrqMarker = lens _drgsrqMarker (\ s a -> s{_drgsrqMarker = a});

-- | The identifier for the replication group to be described. This parameter
-- is not case sensitive.
--
-- If you do not specify this parameter, information about all replication
-- groups is returned.
drgsrqReplicationGroupId :: Lens' DescribeReplicationGroups (Maybe Text)
drgsrqReplicationGroupId = lens _drgsrqReplicationGroupId (\ s a -> s{_drgsrqReplicationGroupId = a});

instance AWSPager DescribeReplicationGroups where
        page rq rs
          | stop (rs ^. drgsrsMarker) = Nothing
          | stop (rs ^. drgsrsReplicationGroups) = Nothing
          | otherwise =
            Just $ rq & drgsrqMarker .~ rs ^. drgsrsMarker

instance AWSRequest DescribeReplicationGroups where
        type Sv DescribeReplicationGroups = ElastiCache
        type Rs DescribeReplicationGroups =
             DescribeReplicationGroupsResponse
        request = post
        response
          = receiveXMLWrapper "DescribeReplicationGroupsResult"
              (\ s h x ->
                 DescribeReplicationGroupsResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "ReplicationGroups" .!@ mempty >>=
                        may (parseXMLList "ReplicationGroup"))
                     <*> (pure (fromEnum s)))

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
               "MaxRecords" =: _drgsrqMaxRecords,
               "Marker" =: _drgsrqMarker,
               "ReplicationGroupId" =: _drgsrqReplicationGroupId]

-- | Represents the output of a /DescribeReplicationGroups/ action.
--
-- /See:/ 'describeReplicationGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drgsrsMarker'
--
-- * 'drgsrsReplicationGroups'
--
-- * 'drgsrsStatus'
data DescribeReplicationGroupsResponse = DescribeReplicationGroupsResponse'
    { _drgsrsMarker            :: !(Maybe Text)
    , _drgsrsReplicationGroups :: !(Maybe [ReplicationGroup])
    , _drgsrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeReplicationGroupsResponse' smart constructor.
describeReplicationGroupsResponse :: Int -> DescribeReplicationGroupsResponse
describeReplicationGroupsResponse pStatus =
    DescribeReplicationGroupsResponse'
    { _drgsrsMarker = Nothing
    , _drgsrsReplicationGroups = Nothing
    , _drgsrsStatus = pStatus
    }

-- | Provides an identifier to allow retrieval of paginated results.
drgsrsMarker :: Lens' DescribeReplicationGroupsResponse (Maybe Text)
drgsrsMarker = lens _drgsrsMarker (\ s a -> s{_drgsrsMarker = a});

-- | A list of replication groups. Each item in the list contains detailed
-- information about one replication group.
drgsrsReplicationGroups :: Lens' DescribeReplicationGroupsResponse [ReplicationGroup]
drgsrsReplicationGroups = lens _drgsrsReplicationGroups (\ s a -> s{_drgsrsReplicationGroups = a}) . _Default;

-- | FIXME: Undocumented member.
drgsrsStatus :: Lens' DescribeReplicationGroupsResponse Int
drgsrsStatus = lens _drgsrsStatus (\ s a -> s{_drgsrsStatus = a});
