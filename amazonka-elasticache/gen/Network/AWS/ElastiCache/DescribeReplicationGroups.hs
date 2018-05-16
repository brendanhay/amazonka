{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeReplicationGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a particular replication group. If no identifier is specified, @DescribeReplicationGroups@ returns information about all replication groups.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeReplicationGroups
    (
    -- * Creating a Request
      describeReplicationGroups
    , DescribeReplicationGroups
    -- * Request Lenses
    , drgsMarker
    , drgsMaxRecords
    , drgsReplicationGroupId

    -- * Destructuring the Response
    , describeReplicationGroupsResponse
    , DescribeReplicationGroupsResponse
    -- * Response Lenses
    , drgrsMarker
    , drgrsReplicationGroups
    , drgrsResponseStatus
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @DescribeReplicationGroups@ operation.
--
--
--
-- /See:/ 'describeReplicationGroups' smart constructor.
data DescribeReplicationGroups = DescribeReplicationGroups'
  { _drgsMarker             :: !(Maybe Text)
  , _drgsMaxRecords         :: !(Maybe Int)
  , _drgsReplicationGroupId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeReplicationGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drgsMarker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'drgsMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved. Default: 100 Constraints: minimum 20; maximum 100.
--
-- * 'drgsReplicationGroupId' - The identifier for the replication group to be described. This parameter is not case sensitive. If you do not specify this parameter, information about all replication groups is returned.
describeReplicationGroups
    :: DescribeReplicationGroups
describeReplicationGroups =
  DescribeReplicationGroups'
    { _drgsMarker = Nothing
    , _drgsMaxRecords = Nothing
    , _drgsReplicationGroupId = Nothing
    }


-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
drgsMarker :: Lens' DescribeReplicationGroups (Maybe Text)
drgsMarker = lens _drgsMarker (\ s a -> s{_drgsMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved. Default: 100 Constraints: minimum 20; maximum 100.
drgsMaxRecords :: Lens' DescribeReplicationGroups (Maybe Int)
drgsMaxRecords = lens _drgsMaxRecords (\ s a -> s{_drgsMaxRecords = a})

-- | The identifier for the replication group to be described. This parameter is not case sensitive. If you do not specify this parameter, information about all replication groups is returned.
drgsReplicationGroupId :: Lens' DescribeReplicationGroups (Maybe Text)
drgsReplicationGroupId = lens _drgsReplicationGroupId (\ s a -> s{_drgsReplicationGroupId = a})

instance AWSPager DescribeReplicationGroups where
        page rq rs
          | stop (rs ^. drgrsMarker) = Nothing
          | stop (rs ^. drgrsReplicationGroups) = Nothing
          | otherwise =
            Just $ rq & drgsMarker .~ rs ^. drgrsMarker

instance AWSRequest DescribeReplicationGroups where
        type Rs DescribeReplicationGroups =
             DescribeReplicationGroupsResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper "DescribeReplicationGroupsResult"
              (\ s h x ->
                 DescribeReplicationGroupsResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "ReplicationGroups" .!@ mempty >>=
                        may (parseXMLList "ReplicationGroup"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeReplicationGroups where

instance NFData DescribeReplicationGroups where

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
               "Marker" =: _drgsMarker,
               "MaxRecords" =: _drgsMaxRecords,
               "ReplicationGroupId" =: _drgsReplicationGroupId]

-- | Represents the output of a @DescribeReplicationGroups@ operation.
--
--
--
-- /See:/ 'describeReplicationGroupsResponse' smart constructor.
data DescribeReplicationGroupsResponse = DescribeReplicationGroupsResponse'
  { _drgrsMarker            :: !(Maybe Text)
  , _drgrsReplicationGroups :: !(Maybe [ReplicationGroup])
  , _drgrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeReplicationGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drgrsMarker' - Provides an identifier to allow retrieval of paginated results.
--
-- * 'drgrsReplicationGroups' - A list of replication groups. Each item in the list contains detailed information about one replication group.
--
-- * 'drgrsResponseStatus' - -- | The response status code.
describeReplicationGroupsResponse
    :: Int -- ^ 'drgrsResponseStatus'
    -> DescribeReplicationGroupsResponse
describeReplicationGroupsResponse pResponseStatus_ =
  DescribeReplicationGroupsResponse'
    { _drgrsMarker = Nothing
    , _drgrsReplicationGroups = Nothing
    , _drgrsResponseStatus = pResponseStatus_
    }


-- | Provides an identifier to allow retrieval of paginated results.
drgrsMarker :: Lens' DescribeReplicationGroupsResponse (Maybe Text)
drgrsMarker = lens _drgrsMarker (\ s a -> s{_drgrsMarker = a})

-- | A list of replication groups. Each item in the list contains detailed information about one replication group.
drgrsReplicationGroups :: Lens' DescribeReplicationGroupsResponse [ReplicationGroup]
drgrsReplicationGroups = lens _drgrsReplicationGroups (\ s a -> s{_drgrsReplicationGroups = a}) . _Default . _Coerce

-- | -- | The response status code.
drgrsResponseStatus :: Lens' DescribeReplicationGroupsResponse Int
drgrsResponseStatus = lens _drgrsResponseStatus (\ s a -> s{_drgrsResponseStatus = a})

instance NFData DescribeReplicationGroupsResponse
         where
