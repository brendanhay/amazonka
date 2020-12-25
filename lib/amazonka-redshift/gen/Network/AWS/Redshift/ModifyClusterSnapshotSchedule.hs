{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifyClusterSnapshotSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a snapshot schedule for a cluster.
module Network.AWS.Redshift.ModifyClusterSnapshotSchedule
  ( -- * Creating a request
    ModifyClusterSnapshotSchedule (..),
    mkModifyClusterSnapshotSchedule,

    -- ** Request lenses
    mcssClusterIdentifier,
    mcssDisassociateSchedule,
    mcssScheduleIdentifier,

    -- * Destructuring the response
    ModifyClusterSnapshotScheduleResponse (..),
    mkModifyClusterSnapshotScheduleResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyClusterSnapshotSchedule' smart constructor.
data ModifyClusterSnapshotSchedule = ModifyClusterSnapshotSchedule'
  { -- | A unique identifier for the cluster whose snapshot schedule you want to modify.
    clusterIdentifier :: Types.String,
    -- | A boolean to indicate whether to remove the assoiciation between the cluster and the schedule.
    disassociateSchedule :: Core.Maybe Core.Bool,
    -- | A unique alphanumeric identifier for the schedule that you want to associate with the cluster.
    scheduleIdentifier :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyClusterSnapshotSchedule' value with any optional fields omitted.
mkModifyClusterSnapshotSchedule ::
  -- | 'clusterIdentifier'
  Types.String ->
  ModifyClusterSnapshotSchedule
mkModifyClusterSnapshotSchedule clusterIdentifier =
  ModifyClusterSnapshotSchedule'
    { clusterIdentifier,
      disassociateSchedule = Core.Nothing,
      scheduleIdentifier = Core.Nothing
    }

-- | A unique identifier for the cluster whose snapshot schedule you want to modify.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcssClusterIdentifier :: Lens.Lens' ModifyClusterSnapshotSchedule Types.String
mcssClusterIdentifier = Lens.field @"clusterIdentifier"
{-# DEPRECATED mcssClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | A boolean to indicate whether to remove the assoiciation between the cluster and the schedule.
--
-- /Note:/ Consider using 'disassociateSchedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcssDisassociateSchedule :: Lens.Lens' ModifyClusterSnapshotSchedule (Core.Maybe Core.Bool)
mcssDisassociateSchedule = Lens.field @"disassociateSchedule"
{-# DEPRECATED mcssDisassociateSchedule "Use generic-lens or generic-optics with 'disassociateSchedule' instead." #-}

-- | A unique alphanumeric identifier for the schedule that you want to associate with the cluster.
--
-- /Note:/ Consider using 'scheduleIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcssScheduleIdentifier :: Lens.Lens' ModifyClusterSnapshotSchedule (Core.Maybe Types.String)
mcssScheduleIdentifier = Lens.field @"scheduleIdentifier"
{-# DEPRECATED mcssScheduleIdentifier "Use generic-lens or generic-optics with 'scheduleIdentifier' instead." #-}

instance Core.AWSRequest ModifyClusterSnapshotSchedule where
  type
    Rs ModifyClusterSnapshotSchedule =
      ModifyClusterSnapshotScheduleResponse
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
            ( Core.pure ("Action", "ModifyClusterSnapshotSchedule")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ClusterIdentifier" clusterIdentifier)
                Core.<> ( Core.toQueryValue "DisassociateSchedule"
                            Core.<$> disassociateSchedule
                        )
                Core.<> ( Core.toQueryValue "ScheduleIdentifier"
                            Core.<$> scheduleIdentifier
                        )
            )
      }
  response =
    Response.receiveNull ModifyClusterSnapshotScheduleResponse'

-- | /See:/ 'mkModifyClusterSnapshotScheduleResponse' smart constructor.
data ModifyClusterSnapshotScheduleResponse = ModifyClusterSnapshotScheduleResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyClusterSnapshotScheduleResponse' value with any optional fields omitted.
mkModifyClusterSnapshotScheduleResponse ::
  ModifyClusterSnapshotScheduleResponse
mkModifyClusterSnapshotScheduleResponse =
  ModifyClusterSnapshotScheduleResponse'
