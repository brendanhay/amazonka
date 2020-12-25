{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DeleteCacheCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a previously provisioned cluster. @DeleteCacheCluster@ deletes all associated cache nodes, node endpoints and the cluster itself. When you receive a successful response from this operation, Amazon ElastiCache immediately begins deleting the cluster; you cannot cancel or revert this operation.
--
-- This operation is not valid for:
--
--     * Redis (cluster mode enabled) clusters
--
--
--     * A cluster that is the last read replica of a replication group
--
--
--     * A node group (shard) that has Multi-AZ mode enabled
--
--
--     * A cluster from a Redis (cluster mode enabled) replication group
--
--
--     * A cluster that is not in the @available@ state
module Network.AWS.ElastiCache.DeleteCacheCluster
  ( -- * Creating a request
    DeleteCacheCluster (..),
    mkDeleteCacheCluster,

    -- ** Request lenses
    dccCacheClusterId,
    dccFinalSnapshotIdentifier,

    -- * Destructuring the response
    DeleteCacheClusterResponse (..),
    mkDeleteCacheClusterResponse,

    -- ** Response lenses
    dccrrsCacheCluster,
    dccrrsResponseStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteCacheCluster@ operation.
--
-- /See:/ 'mkDeleteCacheCluster' smart constructor.
data DeleteCacheCluster = DeleteCacheCluster'
  { -- | The cluster identifier for the cluster to be deleted. This parameter is not case sensitive.
    cacheClusterId :: Types.CacheClusterId,
    -- | The user-supplied name of a final cluster snapshot. This is the unique name that identifies the snapshot. ElastiCache creates the snapshot, and then deletes the cluster immediately afterward.
    finalSnapshotIdentifier :: Core.Maybe Types.FinalSnapshotIdentifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCacheCluster' value with any optional fields omitted.
mkDeleteCacheCluster ::
  -- | 'cacheClusterId'
  Types.CacheClusterId ->
  DeleteCacheCluster
mkDeleteCacheCluster cacheClusterId =
  DeleteCacheCluster'
    { cacheClusterId,
      finalSnapshotIdentifier = Core.Nothing
    }

-- | The cluster identifier for the cluster to be deleted. This parameter is not case sensitive.
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccCacheClusterId :: Lens.Lens' DeleteCacheCluster Types.CacheClusterId
dccCacheClusterId = Lens.field @"cacheClusterId"
{-# DEPRECATED dccCacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead." #-}

-- | The user-supplied name of a final cluster snapshot. This is the unique name that identifies the snapshot. ElastiCache creates the snapshot, and then deletes the cluster immediately afterward.
--
-- /Note:/ Consider using 'finalSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccFinalSnapshotIdentifier :: Lens.Lens' DeleteCacheCluster (Core.Maybe Types.FinalSnapshotIdentifier)
dccFinalSnapshotIdentifier = Lens.field @"finalSnapshotIdentifier"
{-# DEPRECATED dccFinalSnapshotIdentifier "Use generic-lens or generic-optics with 'finalSnapshotIdentifier' instead." #-}

instance Core.AWSRequest DeleteCacheCluster where
  type Rs DeleteCacheCluster = DeleteCacheClusterResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DeleteCacheCluster")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> (Core.toQueryValue "CacheClusterId" cacheClusterId)
                Core.<> ( Core.toQueryValue "FinalSnapshotIdentifier"
                            Core.<$> finalSnapshotIdentifier
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteCacheClusterResult"
      ( \s h x ->
          DeleteCacheClusterResponse'
            Core.<$> (x Core..@? "CacheCluster") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteCacheClusterResponse' smart constructor.
data DeleteCacheClusterResponse = DeleteCacheClusterResponse'
  { cacheCluster :: Core.Maybe Types.CacheCluster,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteCacheClusterResponse' value with any optional fields omitted.
mkDeleteCacheClusterResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteCacheClusterResponse
mkDeleteCacheClusterResponse responseStatus =
  DeleteCacheClusterResponse'
    { cacheCluster = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cacheCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccrrsCacheCluster :: Lens.Lens' DeleteCacheClusterResponse (Core.Maybe Types.CacheCluster)
dccrrsCacheCluster = Lens.field @"cacheCluster"
{-# DEPRECATED dccrrsCacheCluster "Use generic-lens or generic-optics with 'cacheCluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccrrsResponseStatus :: Lens.Lens' DeleteCacheClusterResponse Core.Int
dccrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dccrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
