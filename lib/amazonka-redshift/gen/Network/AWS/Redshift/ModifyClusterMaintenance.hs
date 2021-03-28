{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifyClusterMaintenance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the maintenance settings of a cluster.
module Network.AWS.Redshift.ModifyClusterMaintenance
    (
    -- * Creating a request
      ModifyClusterMaintenance (..)
    , mkModifyClusterMaintenance
    -- ** Request lenses
    , mcmClusterIdentifier
    , mcmDeferMaintenance
    , mcmDeferMaintenanceDuration
    , mcmDeferMaintenanceEndTime
    , mcmDeferMaintenanceIdentifier
    , mcmDeferMaintenanceStartTime

    -- * Destructuring the response
    , ModifyClusterMaintenanceResponse (..)
    , mkModifyClusterMaintenanceResponse
    -- ** Response lenses
    , mcmrrsCluster
    , mcmrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyClusterMaintenance' smart constructor.
data ModifyClusterMaintenance = ModifyClusterMaintenance'
  { clusterIdentifier :: Core.Text
    -- ^ A unique identifier for the cluster.
  , deferMaintenance :: Core.Maybe Core.Bool
    -- ^ A boolean indicating whether to enable the deferred maintenance window. 
  , deferMaintenanceDuration :: Core.Maybe Core.Int
    -- ^ An integer indicating the duration of the maintenance window in days. If you specify a duration, you can't specify an end time. The duration must be 45 days or less.
  , deferMaintenanceEndTime :: Core.Maybe Core.UTCTime
    -- ^ A timestamp indicating end time for the deferred maintenance window. If you specify an end time, you can't specify a duration.
  , deferMaintenanceIdentifier :: Core.Maybe Core.Text
    -- ^ A unique identifier for the deferred maintenance window.
  , deferMaintenanceStartTime :: Core.Maybe Core.UTCTime
    -- ^ A timestamp indicating the start time for the deferred maintenance window.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ModifyClusterMaintenance' value with any optional fields omitted.
mkModifyClusterMaintenance
    :: Core.Text -- ^ 'clusterIdentifier'
    -> ModifyClusterMaintenance
mkModifyClusterMaintenance clusterIdentifier
  = ModifyClusterMaintenance'{clusterIdentifier,
                              deferMaintenance = Core.Nothing,
                              deferMaintenanceDuration = Core.Nothing,
                              deferMaintenanceEndTime = Core.Nothing,
                              deferMaintenanceIdentifier = Core.Nothing,
                              deferMaintenanceStartTime = Core.Nothing}

-- | A unique identifier for the cluster.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcmClusterIdentifier :: Lens.Lens' ModifyClusterMaintenance Core.Text
mcmClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE mcmClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

-- | A boolean indicating whether to enable the deferred maintenance window. 
--
-- /Note:/ Consider using 'deferMaintenance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcmDeferMaintenance :: Lens.Lens' ModifyClusterMaintenance (Core.Maybe Core.Bool)
mcmDeferMaintenance = Lens.field @"deferMaintenance"
{-# INLINEABLE mcmDeferMaintenance #-}
{-# DEPRECATED deferMaintenance "Use generic-lens or generic-optics with 'deferMaintenance' instead"  #-}

-- | An integer indicating the duration of the maintenance window in days. If you specify a duration, you can't specify an end time. The duration must be 45 days or less.
--
-- /Note:/ Consider using 'deferMaintenanceDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcmDeferMaintenanceDuration :: Lens.Lens' ModifyClusterMaintenance (Core.Maybe Core.Int)
mcmDeferMaintenanceDuration = Lens.field @"deferMaintenanceDuration"
{-# INLINEABLE mcmDeferMaintenanceDuration #-}
{-# DEPRECATED deferMaintenanceDuration "Use generic-lens or generic-optics with 'deferMaintenanceDuration' instead"  #-}

-- | A timestamp indicating end time for the deferred maintenance window. If you specify an end time, you can't specify a duration.
--
-- /Note:/ Consider using 'deferMaintenanceEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcmDeferMaintenanceEndTime :: Lens.Lens' ModifyClusterMaintenance (Core.Maybe Core.UTCTime)
mcmDeferMaintenanceEndTime = Lens.field @"deferMaintenanceEndTime"
{-# INLINEABLE mcmDeferMaintenanceEndTime #-}
{-# DEPRECATED deferMaintenanceEndTime "Use generic-lens or generic-optics with 'deferMaintenanceEndTime' instead"  #-}

-- | A unique identifier for the deferred maintenance window.
--
-- /Note:/ Consider using 'deferMaintenanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcmDeferMaintenanceIdentifier :: Lens.Lens' ModifyClusterMaintenance (Core.Maybe Core.Text)
mcmDeferMaintenanceIdentifier = Lens.field @"deferMaintenanceIdentifier"
{-# INLINEABLE mcmDeferMaintenanceIdentifier #-}
{-# DEPRECATED deferMaintenanceIdentifier "Use generic-lens or generic-optics with 'deferMaintenanceIdentifier' instead"  #-}

-- | A timestamp indicating the start time for the deferred maintenance window.
--
-- /Note:/ Consider using 'deferMaintenanceStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcmDeferMaintenanceStartTime :: Lens.Lens' ModifyClusterMaintenance (Core.Maybe Core.UTCTime)
mcmDeferMaintenanceStartTime = Lens.field @"deferMaintenanceStartTime"
{-# INLINEABLE mcmDeferMaintenanceStartTime #-}
{-# DEPRECATED deferMaintenanceStartTime "Use generic-lens or generic-optics with 'deferMaintenanceStartTime' instead"  #-}

instance Core.ToQuery ModifyClusterMaintenance where
        toQuery ModifyClusterMaintenance{..}
          = Core.toQueryPair "Action"
              ("ModifyClusterMaintenance" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ClusterIdentifier" clusterIdentifier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DeferMaintenance")
                deferMaintenance
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "DeferMaintenanceDuration")
                deferMaintenanceDuration
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DeferMaintenanceEndTime")
                deferMaintenanceEndTime
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "DeferMaintenanceIdentifier")
                deferMaintenanceIdentifier
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "DeferMaintenanceStartTime")
                deferMaintenanceStartTime

instance Core.ToHeaders ModifyClusterMaintenance where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyClusterMaintenance where
        type Rs ModifyClusterMaintenance = ModifyClusterMaintenanceResponse
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
          = Response.receiveXMLWrapper "ModifyClusterMaintenanceResult"
              (\ s h x ->
                 ModifyClusterMaintenanceResponse' Core.<$>
                   (x Core..@? "Cluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyClusterMaintenanceResponse' smart constructor.
data ModifyClusterMaintenanceResponse = ModifyClusterMaintenanceResponse'
  { cluster :: Core.Maybe Types.Cluster
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ModifyClusterMaintenanceResponse' value with any optional fields omitted.
mkModifyClusterMaintenanceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyClusterMaintenanceResponse
mkModifyClusterMaintenanceResponse responseStatus
  = ModifyClusterMaintenanceResponse'{cluster = Core.Nothing,
                                      responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcmrrsCluster :: Lens.Lens' ModifyClusterMaintenanceResponse (Core.Maybe Types.Cluster)
mcmrrsCluster = Lens.field @"cluster"
{-# INLINEABLE mcmrrsCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcmrrsResponseStatus :: Lens.Lens' ModifyClusterMaintenanceResponse Core.Int
mcmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mcmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
