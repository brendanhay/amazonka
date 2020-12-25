{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ModifyClusterMaintenance (..),
    mkModifyClusterMaintenance,

    -- ** Request lenses
    mcmClusterIdentifier,
    mcmDeferMaintenance,
    mcmDeferMaintenanceDuration,
    mcmDeferMaintenanceEndTime,
    mcmDeferMaintenanceIdentifier,
    mcmDeferMaintenanceStartTime,

    -- * Destructuring the response
    ModifyClusterMaintenanceResponse (..),
    mkModifyClusterMaintenanceResponse,

    -- ** Response lenses
    mcmrrsCluster,
    mcmrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyClusterMaintenance' smart constructor.
data ModifyClusterMaintenance = ModifyClusterMaintenance'
  { -- | A unique identifier for the cluster.
    clusterIdentifier :: Types.String,
    -- | A boolean indicating whether to enable the deferred maintenance window.
    deferMaintenance :: Core.Maybe Core.Bool,
    -- | An integer indicating the duration of the maintenance window in days. If you specify a duration, you can't specify an end time. The duration must be 45 days or less.
    deferMaintenanceDuration :: Core.Maybe Core.Int,
    -- | A timestamp indicating end time for the deferred maintenance window. If you specify an end time, you can't specify a duration.
    deferMaintenanceEndTime :: Core.Maybe Core.UTCTime,
    -- | A unique identifier for the deferred maintenance window.
    deferMaintenanceIdentifier :: Core.Maybe Types.String,
    -- | A timestamp indicating the start time for the deferred maintenance window.
    deferMaintenanceStartTime :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ModifyClusterMaintenance' value with any optional fields omitted.
mkModifyClusterMaintenance ::
  -- | 'clusterIdentifier'
  Types.String ->
  ModifyClusterMaintenance
mkModifyClusterMaintenance clusterIdentifier =
  ModifyClusterMaintenance'
    { clusterIdentifier,
      deferMaintenance = Core.Nothing,
      deferMaintenanceDuration = Core.Nothing,
      deferMaintenanceEndTime = Core.Nothing,
      deferMaintenanceIdentifier = Core.Nothing,
      deferMaintenanceStartTime = Core.Nothing
    }

-- | A unique identifier for the cluster.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcmClusterIdentifier :: Lens.Lens' ModifyClusterMaintenance Types.String
mcmClusterIdentifier = Lens.field @"clusterIdentifier"
{-# DEPRECATED mcmClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | A boolean indicating whether to enable the deferred maintenance window.
--
-- /Note:/ Consider using 'deferMaintenance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcmDeferMaintenance :: Lens.Lens' ModifyClusterMaintenance (Core.Maybe Core.Bool)
mcmDeferMaintenance = Lens.field @"deferMaintenance"
{-# DEPRECATED mcmDeferMaintenance "Use generic-lens or generic-optics with 'deferMaintenance' instead." #-}

-- | An integer indicating the duration of the maintenance window in days. If you specify a duration, you can't specify an end time. The duration must be 45 days or less.
--
-- /Note:/ Consider using 'deferMaintenanceDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcmDeferMaintenanceDuration :: Lens.Lens' ModifyClusterMaintenance (Core.Maybe Core.Int)
mcmDeferMaintenanceDuration = Lens.field @"deferMaintenanceDuration"
{-# DEPRECATED mcmDeferMaintenanceDuration "Use generic-lens or generic-optics with 'deferMaintenanceDuration' instead." #-}

-- | A timestamp indicating end time for the deferred maintenance window. If you specify an end time, you can't specify a duration.
--
-- /Note:/ Consider using 'deferMaintenanceEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcmDeferMaintenanceEndTime :: Lens.Lens' ModifyClusterMaintenance (Core.Maybe Core.UTCTime)
mcmDeferMaintenanceEndTime = Lens.field @"deferMaintenanceEndTime"
{-# DEPRECATED mcmDeferMaintenanceEndTime "Use generic-lens or generic-optics with 'deferMaintenanceEndTime' instead." #-}

-- | A unique identifier for the deferred maintenance window.
--
-- /Note:/ Consider using 'deferMaintenanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcmDeferMaintenanceIdentifier :: Lens.Lens' ModifyClusterMaintenance (Core.Maybe Types.String)
mcmDeferMaintenanceIdentifier = Lens.field @"deferMaintenanceIdentifier"
{-# DEPRECATED mcmDeferMaintenanceIdentifier "Use generic-lens or generic-optics with 'deferMaintenanceIdentifier' instead." #-}

-- | A timestamp indicating the start time for the deferred maintenance window.
--
-- /Note:/ Consider using 'deferMaintenanceStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcmDeferMaintenanceStartTime :: Lens.Lens' ModifyClusterMaintenance (Core.Maybe Core.UTCTime)
mcmDeferMaintenanceStartTime = Lens.field @"deferMaintenanceStartTime"
{-# DEPRECATED mcmDeferMaintenanceStartTime "Use generic-lens or generic-optics with 'deferMaintenanceStartTime' instead." #-}

instance Core.AWSRequest ModifyClusterMaintenance where
  type Rs ModifyClusterMaintenance = ModifyClusterMaintenanceResponse
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
            ( Core.pure ("Action", "ModifyClusterMaintenance")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ClusterIdentifier" clusterIdentifier)
                Core.<> (Core.toQueryValue "DeferMaintenance" Core.<$> deferMaintenance)
                Core.<> ( Core.toQueryValue "DeferMaintenanceDuration"
                            Core.<$> deferMaintenanceDuration
                        )
                Core.<> ( Core.toQueryValue "DeferMaintenanceEndTime"
                            Core.<$> deferMaintenanceEndTime
                        )
                Core.<> ( Core.toQueryValue "DeferMaintenanceIdentifier"
                            Core.<$> deferMaintenanceIdentifier
                        )
                Core.<> ( Core.toQueryValue "DeferMaintenanceStartTime"
                            Core.<$> deferMaintenanceStartTime
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ModifyClusterMaintenanceResult"
      ( \s h x ->
          ModifyClusterMaintenanceResponse'
            Core.<$> (x Core..@? "Cluster") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyClusterMaintenanceResponse' smart constructor.
data ModifyClusterMaintenanceResponse = ModifyClusterMaintenanceResponse'
  { cluster :: Core.Maybe Types.Cluster,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ModifyClusterMaintenanceResponse' value with any optional fields omitted.
mkModifyClusterMaintenanceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyClusterMaintenanceResponse
mkModifyClusterMaintenanceResponse responseStatus =
  ModifyClusterMaintenanceResponse'
    { cluster = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcmrrsCluster :: Lens.Lens' ModifyClusterMaintenanceResponse (Core.Maybe Types.Cluster)
mcmrrsCluster = Lens.field @"cluster"
{-# DEPRECATED mcmrrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcmrrsResponseStatus :: Lens.Lens' ModifyClusterMaintenanceResponse Core.Int
mcmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mcmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
