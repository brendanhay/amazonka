{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeGlobalReplicationGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a particular global replication group. If no identifier is specified, returns information about all Global Datastores.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeGlobalReplicationGroups
  ( -- * Creating a Request
    describeGlobalReplicationGroups,
    DescribeGlobalReplicationGroups,

    -- * Request Lenses
    dgrgsShowMemberInfo,
    dgrgsMarker,
    dgrgsMaxRecords,
    dgrgsGlobalReplicationGroupId,

    -- * Destructuring the Response
    describeGlobalReplicationGroupsResponse,
    DescribeGlobalReplicationGroupsResponse,

    -- * Response Lenses
    dgrgsrsMarker,
    dgrgsrsGlobalReplicationGroups,
    dgrgsrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeGlobalReplicationGroups' smart constructor.
data DescribeGlobalReplicationGroups = DescribeGlobalReplicationGroups'
  { _dgrgsShowMemberInfo ::
      !(Maybe Bool),
    _dgrgsMarker ::
      !(Maybe Text),
    _dgrgsMaxRecords ::
      !(Maybe Int),
    _dgrgsGlobalReplicationGroupId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeGlobalReplicationGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgrgsShowMemberInfo' - Returns the list of members that comprise the Global Datastore.
--
-- * 'dgrgsMarker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dgrgsMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a marker is included in the response so that the remaining results can be retrieved.
--
-- * 'dgrgsGlobalReplicationGroupId' - The name of the Global Datastore
describeGlobalReplicationGroups ::
  DescribeGlobalReplicationGroups
describeGlobalReplicationGroups =
  DescribeGlobalReplicationGroups'
    { _dgrgsShowMemberInfo = Nothing,
      _dgrgsMarker = Nothing,
      _dgrgsMaxRecords = Nothing,
      _dgrgsGlobalReplicationGroupId = Nothing
    }

-- | Returns the list of members that comprise the Global Datastore.
dgrgsShowMemberInfo :: Lens' DescribeGlobalReplicationGroups (Maybe Bool)
dgrgsShowMemberInfo = lens _dgrgsShowMemberInfo (\s a -> s {_dgrgsShowMemberInfo = a})

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dgrgsMarker :: Lens' DescribeGlobalReplicationGroups (Maybe Text)
dgrgsMarker = lens _dgrgsMarker (\s a -> s {_dgrgsMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a marker is included in the response so that the remaining results can be retrieved.
dgrgsMaxRecords :: Lens' DescribeGlobalReplicationGroups (Maybe Int)
dgrgsMaxRecords = lens _dgrgsMaxRecords (\s a -> s {_dgrgsMaxRecords = a})

-- | The name of the Global Datastore
dgrgsGlobalReplicationGroupId :: Lens' DescribeGlobalReplicationGroups (Maybe Text)
dgrgsGlobalReplicationGroupId = lens _dgrgsGlobalReplicationGroupId (\s a -> s {_dgrgsGlobalReplicationGroupId = a})

instance AWSPager DescribeGlobalReplicationGroups where
  page rq rs
    | stop (rs ^. dgrgsrsMarker) = Nothing
    | stop (rs ^. dgrgsrsGlobalReplicationGroups) = Nothing
    | otherwise = Just $ rq & dgrgsMarker .~ rs ^. dgrgsrsMarker

instance AWSRequest DescribeGlobalReplicationGroups where
  type
    Rs DescribeGlobalReplicationGroups =
      DescribeGlobalReplicationGroupsResponse
  request = postQuery elastiCache
  response =
    receiveXMLWrapper
      "DescribeGlobalReplicationGroupsResult"
      ( \s h x ->
          DescribeGlobalReplicationGroupsResponse'
            <$> (x .@? "Marker")
            <*> ( x .@? "GlobalReplicationGroups" .!@ mempty
                    >>= may (parseXMLList "GlobalReplicationGroup")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeGlobalReplicationGroups

instance NFData DescribeGlobalReplicationGroups

instance ToHeaders DescribeGlobalReplicationGroups where
  toHeaders = const mempty

instance ToPath DescribeGlobalReplicationGroups where
  toPath = const "/"

instance ToQuery DescribeGlobalReplicationGroups where
  toQuery DescribeGlobalReplicationGroups' {..} =
    mconcat
      [ "Action" =: ("DescribeGlobalReplicationGroups" :: ByteString),
        "Version" =: ("2015-02-02" :: ByteString),
        "ShowMemberInfo" =: _dgrgsShowMemberInfo,
        "Marker" =: _dgrgsMarker,
        "MaxRecords" =: _dgrgsMaxRecords,
        "GlobalReplicationGroupId" =: _dgrgsGlobalReplicationGroupId
      ]

-- | /See:/ 'describeGlobalReplicationGroupsResponse' smart constructor.
data DescribeGlobalReplicationGroupsResponse = DescribeGlobalReplicationGroupsResponse'
  { _dgrgsrsMarker ::
      !( Maybe
           Text
       ),
    _dgrgsrsGlobalReplicationGroups ::
      !( Maybe
           [GlobalReplicationGroup]
       ),
    _dgrgsrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeGlobalReplicationGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgrgsrsMarker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
--
-- * 'dgrgsrsGlobalReplicationGroups' - Indicates the slot configuration and global identifier for each slice group.
--
-- * 'dgrgsrsResponseStatus' - -- | The response status code.
describeGlobalReplicationGroupsResponse ::
  -- | 'dgrgsrsResponseStatus'
  Int ->
  DescribeGlobalReplicationGroupsResponse
describeGlobalReplicationGroupsResponse pResponseStatus_ =
  DescribeGlobalReplicationGroupsResponse'
    { _dgrgsrsMarker =
        Nothing,
      _dgrgsrsGlobalReplicationGroups = Nothing,
      _dgrgsrsResponseStatus = pResponseStatus_
    }

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
dgrgsrsMarker :: Lens' DescribeGlobalReplicationGroupsResponse (Maybe Text)
dgrgsrsMarker = lens _dgrgsrsMarker (\s a -> s {_dgrgsrsMarker = a})

-- | Indicates the slot configuration and global identifier for each slice group.
dgrgsrsGlobalReplicationGroups :: Lens' DescribeGlobalReplicationGroupsResponse [GlobalReplicationGroup]
dgrgsrsGlobalReplicationGroups = lens _dgrgsrsGlobalReplicationGroups (\s a -> s {_dgrgsrsGlobalReplicationGroups = a}) . _Default . _Coerce

-- | -- | The response status code.
dgrgsrsResponseStatus :: Lens' DescribeGlobalReplicationGroupsResponse Int
dgrgsrsResponseStatus = lens _dgrgsrsResponseStatus (\s a -> s {_dgrgsrsResponseStatus = a})

instance NFData DescribeGlobalReplicationGroupsResponse
