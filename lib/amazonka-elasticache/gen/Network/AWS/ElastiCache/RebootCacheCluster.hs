{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.RebootCacheCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots some, or all, of the cache nodes within a provisioned cluster. This operation applies any modified cache parameter groups to the cluster. The reboot operation takes place as soon as possible, and results in a momentary outage to the cluster. During the reboot, the cluster status is set to REBOOTING.
--
-- The reboot causes the contents of the cache (for each cache node being rebooted) to be lost.
-- When the reboot is complete, a cluster event is created.
-- Rebooting a cluster is currently supported on Memcached and Redis (cluster mode disabled) clusters. Rebooting is not supported on Redis (cluster mode enabled) clusters.
-- If you make changes to parameters that require a Redis (cluster mode enabled) cluster reboot for the changes to be applied, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.Rebooting.html Rebooting a Cluster> for an alternate process.
module Network.AWS.ElastiCache.RebootCacheCluster
    (
    -- * Creating a request
      RebootCacheCluster (..)
    , mkRebootCacheCluster
    -- ** Request lenses
    , rccCacheClusterId
    , rccCacheNodeIdsToReboot

    -- * Destructuring the response
    , RebootCacheClusterResponse (..)
    , mkRebootCacheClusterResponse
    -- ** Response lenses
    , rccrrsCacheCluster
    , rccrrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @RebootCacheCluster@ operation.
--
-- /See:/ 'mkRebootCacheCluster' smart constructor.
data RebootCacheCluster = RebootCacheCluster'
  { cacheClusterId :: Core.Text
    -- ^ The cluster identifier. This parameter is stored as a lowercase string.
  , cacheNodeIdsToReboot :: [Core.Text]
    -- ^ A list of cache node IDs to reboot. A node ID is a numeric identifier (0001, 0002, etc.). To reboot an entire cluster, specify all of the cache node IDs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RebootCacheCluster' value with any optional fields omitted.
mkRebootCacheCluster
    :: Core.Text -- ^ 'cacheClusterId'
    -> RebootCacheCluster
mkRebootCacheCluster cacheClusterId
  = RebootCacheCluster'{cacheClusterId,
                        cacheNodeIdsToReboot = Core.mempty}

-- | The cluster identifier. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rccCacheClusterId :: Lens.Lens' RebootCacheCluster Core.Text
rccCacheClusterId = Lens.field @"cacheClusterId"
{-# INLINEABLE rccCacheClusterId #-}
{-# DEPRECATED cacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead"  #-}

-- | A list of cache node IDs to reboot. A node ID is a numeric identifier (0001, 0002, etc.). To reboot an entire cluster, specify all of the cache node IDs.
--
-- /Note:/ Consider using 'cacheNodeIdsToReboot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rccCacheNodeIdsToReboot :: Lens.Lens' RebootCacheCluster [Core.Text]
rccCacheNodeIdsToReboot = Lens.field @"cacheNodeIdsToReboot"
{-# INLINEABLE rccCacheNodeIdsToReboot #-}
{-# DEPRECATED cacheNodeIdsToReboot "Use generic-lens or generic-optics with 'cacheNodeIdsToReboot' instead"  #-}

instance Core.ToQuery RebootCacheCluster where
        toQuery RebootCacheCluster{..}
          = Core.toQueryPair "Action" ("RebootCacheCluster" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<> Core.toQueryPair "CacheClusterId" cacheClusterId
              Core.<>
              Core.toQueryPair "CacheNodeIdsToReboot"
                (Core.toQueryList "CacheNodeId" cacheNodeIdsToReboot)

instance Core.ToHeaders RebootCacheCluster where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RebootCacheCluster where
        type Rs RebootCacheCluster = RebootCacheClusterResponse
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
          = Response.receiveXMLWrapper "RebootCacheClusterResult"
              (\ s h x ->
                 RebootCacheClusterResponse' Core.<$>
                   (x Core..@? "CacheCluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRebootCacheClusterResponse' smart constructor.
data RebootCacheClusterResponse = RebootCacheClusterResponse'
  { cacheCluster :: Core.Maybe Types.CacheCluster
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RebootCacheClusterResponse' value with any optional fields omitted.
mkRebootCacheClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RebootCacheClusterResponse
mkRebootCacheClusterResponse responseStatus
  = RebootCacheClusterResponse'{cacheCluster = Core.Nothing,
                                responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cacheCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rccrrsCacheCluster :: Lens.Lens' RebootCacheClusterResponse (Core.Maybe Types.CacheCluster)
rccrrsCacheCluster = Lens.field @"cacheCluster"
{-# INLINEABLE rccrrsCacheCluster #-}
{-# DEPRECATED cacheCluster "Use generic-lens or generic-optics with 'cacheCluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rccrrsResponseStatus :: Lens.Lens' RebootCacheClusterResponse Core.Int
rccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
