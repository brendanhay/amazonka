{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifySnapshotCopyRetentionPeriod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the number of days to retain snapshots in the destination AWS Region after they are copied from the source AWS Region. By default, this operation only changes the retention period of copied automated snapshots. The retention periods for both new and existing copied automated snapshots are updated with the new retention period. You can set the manual option to change only the retention periods of copied manual snapshots. If you set this option, only newly copied manual snapshots have the new retention period.
module Network.AWS.Redshift.ModifySnapshotCopyRetentionPeriod
  ( -- * Creating a request
    ModifySnapshotCopyRetentionPeriod (..),
    mkModifySnapshotCopyRetentionPeriod,

    -- ** Request lenses
    mscrpClusterIdentifier,
    mscrpRetentionPeriod,
    mscrpManual,

    -- * Destructuring the response
    ModifySnapshotCopyRetentionPeriodResponse (..),
    mkModifySnapshotCopyRetentionPeriodResponse,

    -- ** Response lenses
    mscrprrsCluster,
    mscrprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkModifySnapshotCopyRetentionPeriod' smart constructor.
data ModifySnapshotCopyRetentionPeriod = ModifySnapshotCopyRetentionPeriod'
  { -- | The unique identifier of the cluster for which you want to change the retention period for either automated or manual snapshots that are copied to a destination AWS Region.
    --
    -- Constraints: Must be the valid name of an existing cluster that has cross-region snapshot copy enabled.
    clusterIdentifier :: Types.String,
    -- | The number of days to retain automated snapshots in the destination AWS Region after they are copied from the source AWS Region.
    --
    -- By default, this only changes the retention period of copied automated snapshots.
    -- If you decrease the retention period for automated snapshots that are copied to a destination AWS Region, Amazon Redshift deletes any existing automated snapshots that were copied to the destination AWS Region and that fall outside of the new retention period.
    -- Constraints: Must be at least 1 and no more than 35 for automated snapshots.
    -- If you specify the @manual@ option, only newly copied manual snapshots will have the new retention period.
    -- If you specify the value of -1 newly copied manual snapshots are retained indefinitely.
    -- Constraints: The number of days must be either -1 or an integer between 1 and 3,653 for manual snapshots.
    retentionPeriod :: Core.Int,
    -- | Indicates whether to apply the snapshot retention period to newly copied manual snapshots instead of automated snapshots.
    manual :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifySnapshotCopyRetentionPeriod' value with any optional fields omitted.
mkModifySnapshotCopyRetentionPeriod ::
  -- | 'clusterIdentifier'
  Types.String ->
  -- | 'retentionPeriod'
  Core.Int ->
  ModifySnapshotCopyRetentionPeriod
mkModifySnapshotCopyRetentionPeriod
  clusterIdentifier
  retentionPeriod =
    ModifySnapshotCopyRetentionPeriod'
      { clusterIdentifier,
        retentionPeriod,
        manual = Core.Nothing
      }

-- | The unique identifier of the cluster for which you want to change the retention period for either automated or manual snapshots that are copied to a destination AWS Region.
--
-- Constraints: Must be the valid name of an existing cluster that has cross-region snapshot copy enabled.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mscrpClusterIdentifier :: Lens.Lens' ModifySnapshotCopyRetentionPeriod Types.String
mscrpClusterIdentifier = Lens.field @"clusterIdentifier"
{-# DEPRECATED mscrpClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | The number of days to retain automated snapshots in the destination AWS Region after they are copied from the source AWS Region.
--
-- By default, this only changes the retention period of copied automated snapshots.
-- If you decrease the retention period for automated snapshots that are copied to a destination AWS Region, Amazon Redshift deletes any existing automated snapshots that were copied to the destination AWS Region and that fall outside of the new retention period.
-- Constraints: Must be at least 1 and no more than 35 for automated snapshots.
-- If you specify the @manual@ option, only newly copied manual snapshots will have the new retention period.
-- If you specify the value of -1 newly copied manual snapshots are retained indefinitely.
-- Constraints: The number of days must be either -1 or an integer between 1 and 3,653 for manual snapshots.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mscrpRetentionPeriod :: Lens.Lens' ModifySnapshotCopyRetentionPeriod Core.Int
mscrpRetentionPeriod = Lens.field @"retentionPeriod"
{-# DEPRECATED mscrpRetentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead." #-}

-- | Indicates whether to apply the snapshot retention period to newly copied manual snapshots instead of automated snapshots.
--
-- /Note:/ Consider using 'manual' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mscrpManual :: Lens.Lens' ModifySnapshotCopyRetentionPeriod (Core.Maybe Core.Bool)
mscrpManual = Lens.field @"manual"
{-# DEPRECATED mscrpManual "Use generic-lens or generic-optics with 'manual' instead." #-}

instance Core.AWSRequest ModifySnapshotCopyRetentionPeriod where
  type
    Rs ModifySnapshotCopyRetentionPeriod =
      ModifySnapshotCopyRetentionPeriodResponse
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
            ( Core.pure ("Action", "ModifySnapshotCopyRetentionPeriod")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ClusterIdentifier" clusterIdentifier)
                Core.<> (Core.toQueryValue "RetentionPeriod" retentionPeriod)
                Core.<> (Core.toQueryValue "Manual" Core.<$> manual)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ModifySnapshotCopyRetentionPeriodResult"
      ( \s h x ->
          ModifySnapshotCopyRetentionPeriodResponse'
            Core.<$> (x Core..@? "Cluster") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifySnapshotCopyRetentionPeriodResponse' smart constructor.
data ModifySnapshotCopyRetentionPeriodResponse = ModifySnapshotCopyRetentionPeriodResponse'
  { cluster :: Core.Maybe Types.Cluster,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ModifySnapshotCopyRetentionPeriodResponse' value with any optional fields omitted.
mkModifySnapshotCopyRetentionPeriodResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifySnapshotCopyRetentionPeriodResponse
mkModifySnapshotCopyRetentionPeriodResponse responseStatus =
  ModifySnapshotCopyRetentionPeriodResponse'
    { cluster =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mscrprrsCluster :: Lens.Lens' ModifySnapshotCopyRetentionPeriodResponse (Core.Maybe Types.Cluster)
mscrprrsCluster = Lens.field @"cluster"
{-# DEPRECATED mscrprrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mscrprrsResponseStatus :: Lens.Lens' ModifySnapshotCopyRetentionPeriodResponse Core.Int
mscrprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mscrprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
