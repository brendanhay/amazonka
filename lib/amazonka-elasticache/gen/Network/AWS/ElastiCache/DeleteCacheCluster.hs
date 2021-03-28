{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
--
module Network.AWS.ElastiCache.DeleteCacheCluster
    (
    -- * Creating a request
      DeleteCacheCluster (..)
    , mkDeleteCacheCluster
    -- ** Request lenses
    , dccCacheClusterId
    , dccFinalSnapshotIdentifier

    -- * Destructuring the response
    , DeleteCacheClusterResponse (..)
    , mkDeleteCacheClusterResponse
    -- ** Response lenses
    , dccrrsCacheCluster
    , dccrrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteCacheCluster@ operation.
--
-- /See:/ 'mkDeleteCacheCluster' smart constructor.
data DeleteCacheCluster = DeleteCacheCluster'
  { cacheClusterId :: Core.Text
    -- ^ The cluster identifier for the cluster to be deleted. This parameter is not case sensitive.
  , finalSnapshotIdentifier :: Core.Maybe Core.Text
    -- ^ The user-supplied name of a final cluster snapshot. This is the unique name that identifies the snapshot. ElastiCache creates the snapshot, and then deletes the cluster immediately afterward.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCacheCluster' value with any optional fields omitted.
mkDeleteCacheCluster
    :: Core.Text -- ^ 'cacheClusterId'
    -> DeleteCacheCluster
mkDeleteCacheCluster cacheClusterId
  = DeleteCacheCluster'{cacheClusterId,
                        finalSnapshotIdentifier = Core.Nothing}

-- | The cluster identifier for the cluster to be deleted. This parameter is not case sensitive.
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccCacheClusterId :: Lens.Lens' DeleteCacheCluster Core.Text
dccCacheClusterId = Lens.field @"cacheClusterId"
{-# INLINEABLE dccCacheClusterId #-}
{-# DEPRECATED cacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead"  #-}

-- | The user-supplied name of a final cluster snapshot. This is the unique name that identifies the snapshot. ElastiCache creates the snapshot, and then deletes the cluster immediately afterward.
--
-- /Note:/ Consider using 'finalSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccFinalSnapshotIdentifier :: Lens.Lens' DeleteCacheCluster (Core.Maybe Core.Text)
dccFinalSnapshotIdentifier = Lens.field @"finalSnapshotIdentifier"
{-# INLINEABLE dccFinalSnapshotIdentifier #-}
{-# DEPRECATED finalSnapshotIdentifier "Use generic-lens or generic-optics with 'finalSnapshotIdentifier' instead"  #-}

instance Core.ToQuery DeleteCacheCluster where
        toQuery DeleteCacheCluster{..}
          = Core.toQueryPair "Action" ("DeleteCacheCluster" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<> Core.toQueryPair "CacheClusterId" cacheClusterId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "FinalSnapshotIdentifier")
                finalSnapshotIdentifier

instance Core.ToHeaders DeleteCacheCluster where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteCacheCluster where
        type Rs DeleteCacheCluster = DeleteCacheClusterResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DeleteCacheClusterResult"
              (\ s h x ->
                 DeleteCacheClusterResponse' Core.<$>
                   (x Core..@? "CacheCluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteCacheClusterResponse' smart constructor.
data DeleteCacheClusterResponse = DeleteCacheClusterResponse'
  { cacheCluster :: Core.Maybe Types.CacheCluster
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteCacheClusterResponse' value with any optional fields omitted.
mkDeleteCacheClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteCacheClusterResponse
mkDeleteCacheClusterResponse responseStatus
  = DeleteCacheClusterResponse'{cacheCluster = Core.Nothing,
                                responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cacheCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccrrsCacheCluster :: Lens.Lens' DeleteCacheClusterResponse (Core.Maybe Types.CacheCluster)
dccrrsCacheCluster = Lens.field @"cacheCluster"
{-# INLINEABLE dccrrsCacheCluster #-}
{-# DEPRECATED cacheCluster "Use generic-lens or generic-optics with 'cacheCluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccrrsResponseStatus :: Lens.Lens' DeleteCacheClusterResponse Core.Int
dccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
