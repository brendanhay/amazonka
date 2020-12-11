{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeUpdateActions
  ( -- * Creating a request
    DescribeUpdateActions (..),
    mkDescribeUpdateActions,

    -- ** Request lenses
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

    -- * Destructuring the response
    DescribeUpdateActionsResponse (..),
    mkDescribeUpdateActionsResponse,

    -- ** Response lenses
    duarsUpdateActions,
    duarsMarker,
    duarsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeUpdateActions' smart constructor.
data DescribeUpdateActions = DescribeUpdateActions'
  { cacheClusterIds ::
      Lude.Maybe [Lude.Text],
    serviceUpdateName :: Lude.Maybe Lude.Text,
    updateActionStatus ::
      Lude.Maybe [UpdateActionStatus],
    engine :: Lude.Maybe Lude.Text,
    serviceUpdateTimeRange ::
      Lude.Maybe TimeRangeFilter,
    marker :: Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Int,
    replicationGroupIds :: Lude.Maybe [Lude.Text],
    showNodeLevelUpdateStatus ::
      Lude.Maybe Lude.Bool,
    serviceUpdateStatus ::
      Lude.Maybe [ServiceUpdateStatus]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUpdateActions' with the minimum fields required to make a request.
--
-- * 'cacheClusterIds' - The cache cluster IDs
-- * 'engine' - The Elasticache engine to which the update applies. Either Redis or Memcached
-- * 'marker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response
-- * 'replicationGroupIds' - The replication group IDs
-- * 'serviceUpdateName' - The unique ID of the service update
-- * 'serviceUpdateStatus' - The status of the service update
-- * 'serviceUpdateTimeRange' - The range of time specified to search for service updates that are in available status
-- * 'showNodeLevelUpdateStatus' - Dictates whether to include node level update status in the response
-- * 'updateActionStatus' - The status of the update action.
mkDescribeUpdateActions ::
  DescribeUpdateActions
mkDescribeUpdateActions =
  DescribeUpdateActions'
    { cacheClusterIds = Lude.Nothing,
      serviceUpdateName = Lude.Nothing,
      updateActionStatus = Lude.Nothing,
      engine = Lude.Nothing,
      serviceUpdateTimeRange = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      replicationGroupIds = Lude.Nothing,
      showNodeLevelUpdateStatus = Lude.Nothing,
      serviceUpdateStatus = Lude.Nothing
    }

-- | The cache cluster IDs
--
-- /Note:/ Consider using 'cacheClusterIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaCacheClusterIds :: Lens.Lens' DescribeUpdateActions (Lude.Maybe [Lude.Text])
duaCacheClusterIds = Lens.lens (cacheClusterIds :: DescribeUpdateActions -> Lude.Maybe [Lude.Text]) (\s a -> s {cacheClusterIds = a} :: DescribeUpdateActions)
{-# DEPRECATED duaCacheClusterIds "Use generic-lens or generic-optics with 'cacheClusterIds' instead." #-}

-- | The unique ID of the service update
--
-- /Note:/ Consider using 'serviceUpdateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaServiceUpdateName :: Lens.Lens' DescribeUpdateActions (Lude.Maybe Lude.Text)
duaServiceUpdateName = Lens.lens (serviceUpdateName :: DescribeUpdateActions -> Lude.Maybe Lude.Text) (\s a -> s {serviceUpdateName = a} :: DescribeUpdateActions)
{-# DEPRECATED duaServiceUpdateName "Use generic-lens or generic-optics with 'serviceUpdateName' instead." #-}

-- | The status of the update action.
--
-- /Note:/ Consider using 'updateActionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaUpdateActionStatus :: Lens.Lens' DescribeUpdateActions (Lude.Maybe [UpdateActionStatus])
duaUpdateActionStatus = Lens.lens (updateActionStatus :: DescribeUpdateActions -> Lude.Maybe [UpdateActionStatus]) (\s a -> s {updateActionStatus = a} :: DescribeUpdateActions)
{-# DEPRECATED duaUpdateActionStatus "Use generic-lens or generic-optics with 'updateActionStatus' instead." #-}

-- | The Elasticache engine to which the update applies. Either Redis or Memcached
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaEngine :: Lens.Lens' DescribeUpdateActions (Lude.Maybe Lude.Text)
duaEngine = Lens.lens (engine :: DescribeUpdateActions -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: DescribeUpdateActions)
{-# DEPRECATED duaEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The range of time specified to search for service updates that are in available status
--
-- /Note:/ Consider using 'serviceUpdateTimeRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaServiceUpdateTimeRange :: Lens.Lens' DescribeUpdateActions (Lude.Maybe TimeRangeFilter)
duaServiceUpdateTimeRange = Lens.lens (serviceUpdateTimeRange :: DescribeUpdateActions -> Lude.Maybe TimeRangeFilter) (\s a -> s {serviceUpdateTimeRange = a} :: DescribeUpdateActions)
{-# DEPRECATED duaServiceUpdateTimeRange "Use generic-lens or generic-optics with 'serviceUpdateTimeRange' instead." #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaMarker :: Lens.Lens' DescribeUpdateActions (Lude.Maybe Lude.Text)
duaMarker = Lens.lens (marker :: DescribeUpdateActions -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeUpdateActions)
{-# DEPRECATED duaMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaMaxRecords :: Lens.Lens' DescribeUpdateActions (Lude.Maybe Lude.Int)
duaMaxRecords = Lens.lens (maxRecords :: DescribeUpdateActions -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeUpdateActions)
{-# DEPRECATED duaMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The replication group IDs
--
-- /Note:/ Consider using 'replicationGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaReplicationGroupIds :: Lens.Lens' DescribeUpdateActions (Lude.Maybe [Lude.Text])
duaReplicationGroupIds = Lens.lens (replicationGroupIds :: DescribeUpdateActions -> Lude.Maybe [Lude.Text]) (\s a -> s {replicationGroupIds = a} :: DescribeUpdateActions)
{-# DEPRECATED duaReplicationGroupIds "Use generic-lens or generic-optics with 'replicationGroupIds' instead." #-}

-- | Dictates whether to include node level update status in the response
--
-- /Note:/ Consider using 'showNodeLevelUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaShowNodeLevelUpdateStatus :: Lens.Lens' DescribeUpdateActions (Lude.Maybe Lude.Bool)
duaShowNodeLevelUpdateStatus = Lens.lens (showNodeLevelUpdateStatus :: DescribeUpdateActions -> Lude.Maybe Lude.Bool) (\s a -> s {showNodeLevelUpdateStatus = a} :: DescribeUpdateActions)
{-# DEPRECATED duaShowNodeLevelUpdateStatus "Use generic-lens or generic-optics with 'showNodeLevelUpdateStatus' instead." #-}

-- | The status of the service update
--
-- /Note:/ Consider using 'serviceUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaServiceUpdateStatus :: Lens.Lens' DescribeUpdateActions (Lude.Maybe [ServiceUpdateStatus])
duaServiceUpdateStatus = Lens.lens (serviceUpdateStatus :: DescribeUpdateActions -> Lude.Maybe [ServiceUpdateStatus]) (\s a -> s {serviceUpdateStatus = a} :: DescribeUpdateActions)
{-# DEPRECATED duaServiceUpdateStatus "Use generic-lens or generic-optics with 'serviceUpdateStatus' instead." #-}

instance Page.AWSPager DescribeUpdateActions where
  page rq rs
    | Page.stop (rs Lens.^. duarsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. duarsUpdateActions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& duaMarker Lens..~ rs Lens.^. duarsMarker

instance Lude.AWSRequest DescribeUpdateActions where
  type Rs DescribeUpdateActions = DescribeUpdateActionsResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "DescribeUpdateActionsResult"
      ( \s h x ->
          DescribeUpdateActionsResponse'
            Lude.<$> ( x Lude..@? "UpdateActions" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "UpdateAction")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeUpdateActions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeUpdateActions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeUpdateActions where
  toQuery DescribeUpdateActions' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeUpdateActions" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "CacheClusterIds"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> cacheClusterIds),
        "ServiceUpdateName" Lude.=: serviceUpdateName,
        "UpdateActionStatus"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> updateActionStatus),
        "Engine" Lude.=: engine,
        "ServiceUpdateTimeRange" Lude.=: serviceUpdateTimeRange,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "ReplicationGroupIds"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> replicationGroupIds),
        "ShowNodeLevelUpdateStatus" Lude.=: showNodeLevelUpdateStatus,
        "ServiceUpdateStatus"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> serviceUpdateStatus)
      ]

-- | /See:/ 'mkDescribeUpdateActionsResponse' smart constructor.
data DescribeUpdateActionsResponse = DescribeUpdateActionsResponse'
  { updateActions ::
      Lude.Maybe [UpdateAction],
    marker :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUpdateActionsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'responseStatus' - The response status code.
-- * 'updateActions' - Returns a list of update actions
mkDescribeUpdateActionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeUpdateActionsResponse
mkDescribeUpdateActionsResponse pResponseStatus_ =
  DescribeUpdateActionsResponse'
    { updateActions = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns a list of update actions
--
-- /Note:/ Consider using 'updateActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duarsUpdateActions :: Lens.Lens' DescribeUpdateActionsResponse (Lude.Maybe [UpdateAction])
duarsUpdateActions = Lens.lens (updateActions :: DescribeUpdateActionsResponse -> Lude.Maybe [UpdateAction]) (\s a -> s {updateActions = a} :: DescribeUpdateActionsResponse)
{-# DEPRECATED duarsUpdateActions "Use generic-lens or generic-optics with 'updateActions' instead." #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duarsMarker :: Lens.Lens' DescribeUpdateActionsResponse (Lude.Maybe Lude.Text)
duarsMarker = Lens.lens (marker :: DescribeUpdateActionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeUpdateActionsResponse)
{-# DEPRECATED duarsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duarsResponseStatus :: Lens.Lens' DescribeUpdateActionsResponse Lude.Int
duarsResponseStatus = Lens.lens (responseStatus :: DescribeUpdateActionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeUpdateActionsResponse)
{-# DEPRECATED duarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
