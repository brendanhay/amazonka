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
-- Module      : Network.AWS.ElastiCache.DescribeUpdateActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details of the update actions
--
--
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeUpdateActions
  ( -- * Creating a Request
    describeUpdateActions,
    DescribeUpdateActions,

    -- * Request Lenses
    duaCacheClusterIds,
    duaServiceUpdateName,
    duaUpdateActionStatus,
    duaEngine,
    duaServiceUpdateTimeRange,
    duaMarker,
    duaMaxRecords,
    duaReplicationGroupIds,
    duaShowNodeLevelUpdateStatus,
    duaServiceUpdateStatus,

    -- * Destructuring the Response
    describeUpdateActionsResponse,
    DescribeUpdateActionsResponse,

    -- * Response Lenses
    duarsUpdateActions,
    duarsMarker,
    duarsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeUpdateActions' smart constructor.
data DescribeUpdateActions = DescribeUpdateActions'
  { _duaCacheClusterIds ::
      !(Maybe [Text]),
    _duaServiceUpdateName :: !(Maybe Text),
    _duaUpdateActionStatus ::
      !(Maybe [UpdateActionStatus]),
    _duaEngine :: !(Maybe Text),
    _duaServiceUpdateTimeRange ::
      !(Maybe TimeRangeFilter),
    _duaMarker :: !(Maybe Text),
    _duaMaxRecords :: !(Maybe Int),
    _duaReplicationGroupIds :: !(Maybe [Text]),
    _duaShowNodeLevelUpdateStatus :: !(Maybe Bool),
    _duaServiceUpdateStatus ::
      !(Maybe [ServiceUpdateStatus])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeUpdateActions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duaCacheClusterIds' - The cache cluster IDs
--
-- * 'duaServiceUpdateName' - The unique ID of the service update
--
-- * 'duaUpdateActionStatus' - The status of the update action.
--
-- * 'duaEngine' - The Elasticache engine to which the update applies. Either Redis or Memcached
--
-- * 'duaServiceUpdateTimeRange' - The range of time specified to search for service updates that are in available status
--
-- * 'duaMarker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'duaMaxRecords' - The maximum number of records to include in the response
--
-- * 'duaReplicationGroupIds' - The replication group IDs
--
-- * 'duaShowNodeLevelUpdateStatus' - Dictates whether to include node level update status in the response
--
-- * 'duaServiceUpdateStatus' - The status of the service update
describeUpdateActions ::
  DescribeUpdateActions
describeUpdateActions =
  DescribeUpdateActions'
    { _duaCacheClusterIds = Nothing,
      _duaServiceUpdateName = Nothing,
      _duaUpdateActionStatus = Nothing,
      _duaEngine = Nothing,
      _duaServiceUpdateTimeRange = Nothing,
      _duaMarker = Nothing,
      _duaMaxRecords = Nothing,
      _duaReplicationGroupIds = Nothing,
      _duaShowNodeLevelUpdateStatus = Nothing,
      _duaServiceUpdateStatus = Nothing
    }

-- | The cache cluster IDs
duaCacheClusterIds :: Lens' DescribeUpdateActions [Text]
duaCacheClusterIds = lens _duaCacheClusterIds (\s a -> s {_duaCacheClusterIds = a}) . _Default . _Coerce

-- | The unique ID of the service update
duaServiceUpdateName :: Lens' DescribeUpdateActions (Maybe Text)
duaServiceUpdateName = lens _duaServiceUpdateName (\s a -> s {_duaServiceUpdateName = a})

-- | The status of the update action.
duaUpdateActionStatus :: Lens' DescribeUpdateActions [UpdateActionStatus]
duaUpdateActionStatus = lens _duaUpdateActionStatus (\s a -> s {_duaUpdateActionStatus = a}) . _Default . _Coerce

-- | The Elasticache engine to which the update applies. Either Redis or Memcached
duaEngine :: Lens' DescribeUpdateActions (Maybe Text)
duaEngine = lens _duaEngine (\s a -> s {_duaEngine = a})

-- | The range of time specified to search for service updates that are in available status
duaServiceUpdateTimeRange :: Lens' DescribeUpdateActions (Maybe TimeRangeFilter)
duaServiceUpdateTimeRange = lens _duaServiceUpdateTimeRange (\s a -> s {_duaServiceUpdateTimeRange = a})

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
duaMarker :: Lens' DescribeUpdateActions (Maybe Text)
duaMarker = lens _duaMarker (\s a -> s {_duaMarker = a})

-- | The maximum number of records to include in the response
duaMaxRecords :: Lens' DescribeUpdateActions (Maybe Int)
duaMaxRecords = lens _duaMaxRecords (\s a -> s {_duaMaxRecords = a})

-- | The replication group IDs
duaReplicationGroupIds :: Lens' DescribeUpdateActions [Text]
duaReplicationGroupIds = lens _duaReplicationGroupIds (\s a -> s {_duaReplicationGroupIds = a}) . _Default . _Coerce

-- | Dictates whether to include node level update status in the response
duaShowNodeLevelUpdateStatus :: Lens' DescribeUpdateActions (Maybe Bool)
duaShowNodeLevelUpdateStatus = lens _duaShowNodeLevelUpdateStatus (\s a -> s {_duaShowNodeLevelUpdateStatus = a})

-- | The status of the service update
duaServiceUpdateStatus :: Lens' DescribeUpdateActions [ServiceUpdateStatus]
duaServiceUpdateStatus = lens _duaServiceUpdateStatus (\s a -> s {_duaServiceUpdateStatus = a}) . _Default . _Coerce

instance AWSPager DescribeUpdateActions where
  page rq rs
    | stop (rs ^. duarsMarker) = Nothing
    | stop (rs ^. duarsUpdateActions) = Nothing
    | otherwise = Just $ rq & duaMarker .~ rs ^. duarsMarker

instance AWSRequest DescribeUpdateActions where
  type Rs DescribeUpdateActions = DescribeUpdateActionsResponse
  request = postQuery elastiCache
  response =
    receiveXMLWrapper
      "DescribeUpdateActionsResult"
      ( \s h x ->
          DescribeUpdateActionsResponse'
            <$> ( x .@? "UpdateActions" .!@ mempty
                    >>= may (parseXMLList "UpdateAction")
                )
            <*> (x .@? "Marker")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeUpdateActions

instance NFData DescribeUpdateActions

instance ToHeaders DescribeUpdateActions where
  toHeaders = const mempty

instance ToPath DescribeUpdateActions where
  toPath = const "/"

instance ToQuery DescribeUpdateActions where
  toQuery DescribeUpdateActions' {..} =
    mconcat
      [ "Action" =: ("DescribeUpdateActions" :: ByteString),
        "Version" =: ("2015-02-02" :: ByteString),
        "CacheClusterIds"
          =: toQuery (toQueryList "member" <$> _duaCacheClusterIds),
        "ServiceUpdateName" =: _duaServiceUpdateName,
        "UpdateActionStatus"
          =: toQuery (toQueryList "member" <$> _duaUpdateActionStatus),
        "Engine" =: _duaEngine,
        "ServiceUpdateTimeRange" =: _duaServiceUpdateTimeRange,
        "Marker" =: _duaMarker,
        "MaxRecords" =: _duaMaxRecords,
        "ReplicationGroupIds"
          =: toQuery (toQueryList "member" <$> _duaReplicationGroupIds),
        "ShowNodeLevelUpdateStatus" =: _duaShowNodeLevelUpdateStatus,
        "ServiceUpdateStatus"
          =: toQuery (toQueryList "member" <$> _duaServiceUpdateStatus)
      ]

-- | /See:/ 'describeUpdateActionsResponse' smart constructor.
data DescribeUpdateActionsResponse = DescribeUpdateActionsResponse'
  { _duarsUpdateActions ::
      !(Maybe [UpdateAction]),
    _duarsMarker :: !(Maybe Text),
    _duarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeUpdateActionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duarsUpdateActions' - Returns a list of update actions
--
-- * 'duarsMarker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'duarsResponseStatus' - -- | The response status code.
describeUpdateActionsResponse ::
  -- | 'duarsResponseStatus'
  Int ->
  DescribeUpdateActionsResponse
describeUpdateActionsResponse pResponseStatus_ =
  DescribeUpdateActionsResponse'
    { _duarsUpdateActions = Nothing,
      _duarsMarker = Nothing,
      _duarsResponseStatus = pResponseStatus_
    }

-- | Returns a list of update actions
duarsUpdateActions :: Lens' DescribeUpdateActionsResponse [UpdateAction]
duarsUpdateActions = lens _duarsUpdateActions (\s a -> s {_duarsUpdateActions = a}) . _Default . _Coerce

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
duarsMarker :: Lens' DescribeUpdateActionsResponse (Maybe Text)
duarsMarker = lens _duarsMarker (\s a -> s {_duarsMarker = a})

-- | -- | The response status code.
duarsResponseStatus :: Lens' DescribeUpdateActionsResponse Int
duarsResponseStatus = lens _duarsResponseStatus (\s a -> s {_duarsResponseStatus = a})

instance NFData DescribeUpdateActionsResponse
