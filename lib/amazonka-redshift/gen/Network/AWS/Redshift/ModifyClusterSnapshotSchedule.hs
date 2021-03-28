{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ModifyClusterSnapshotSchedule (..)
    , mkModifyClusterSnapshotSchedule
    -- ** Request lenses
    , mcssClusterIdentifier
    , mcssDisassociateSchedule
    , mcssScheduleIdentifier

    -- * Destructuring the response
    , ModifyClusterSnapshotScheduleResponse (..)
    , mkModifyClusterSnapshotScheduleResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyClusterSnapshotSchedule' smart constructor.
data ModifyClusterSnapshotSchedule = ModifyClusterSnapshotSchedule'
  { clusterIdentifier :: Core.Text
    -- ^ A unique identifier for the cluster whose snapshot schedule you want to modify. 
  , disassociateSchedule :: Core.Maybe Core.Bool
    -- ^ A boolean to indicate whether to remove the assoiciation between the cluster and the schedule.
  , scheduleIdentifier :: Core.Maybe Core.Text
    -- ^ A unique alphanumeric identifier for the schedule that you want to associate with the cluster.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyClusterSnapshotSchedule' value with any optional fields omitted.
mkModifyClusterSnapshotSchedule
    :: Core.Text -- ^ 'clusterIdentifier'
    -> ModifyClusterSnapshotSchedule
mkModifyClusterSnapshotSchedule clusterIdentifier
  = ModifyClusterSnapshotSchedule'{clusterIdentifier,
                                   disassociateSchedule = Core.Nothing,
                                   scheduleIdentifier = Core.Nothing}

-- | A unique identifier for the cluster whose snapshot schedule you want to modify. 
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcssClusterIdentifier :: Lens.Lens' ModifyClusterSnapshotSchedule Core.Text
mcssClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE mcssClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

-- | A boolean to indicate whether to remove the assoiciation between the cluster and the schedule.
--
-- /Note:/ Consider using 'disassociateSchedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcssDisassociateSchedule :: Lens.Lens' ModifyClusterSnapshotSchedule (Core.Maybe Core.Bool)
mcssDisassociateSchedule = Lens.field @"disassociateSchedule"
{-# INLINEABLE mcssDisassociateSchedule #-}
{-# DEPRECATED disassociateSchedule "Use generic-lens or generic-optics with 'disassociateSchedule' instead"  #-}

-- | A unique alphanumeric identifier for the schedule that you want to associate with the cluster.
--
-- /Note:/ Consider using 'scheduleIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcssScheduleIdentifier :: Lens.Lens' ModifyClusterSnapshotSchedule (Core.Maybe Core.Text)
mcssScheduleIdentifier = Lens.field @"scheduleIdentifier"
{-# INLINEABLE mcssScheduleIdentifier #-}
{-# DEPRECATED scheduleIdentifier "Use generic-lens or generic-optics with 'scheduleIdentifier' instead"  #-}

instance Core.ToQuery ModifyClusterSnapshotSchedule where
        toQuery ModifyClusterSnapshotSchedule{..}
          = Core.toQueryPair "Action"
              ("ModifyClusterSnapshotSchedule" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ClusterIdentifier" clusterIdentifier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DisassociateSchedule")
                disassociateSchedule
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ScheduleIdentifier")
                scheduleIdentifier

instance Core.ToHeaders ModifyClusterSnapshotSchedule where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyClusterSnapshotSchedule where
        type Rs ModifyClusterSnapshotSchedule =
             ModifyClusterSnapshotScheduleResponse
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
          = Response.receiveNull ModifyClusterSnapshotScheduleResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyClusterSnapshotScheduleResponse' smart constructor.
data ModifyClusterSnapshotScheduleResponse = ModifyClusterSnapshotScheduleResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyClusterSnapshotScheduleResponse' value with any optional fields omitted.
mkModifyClusterSnapshotScheduleResponse
    :: ModifyClusterSnapshotScheduleResponse
mkModifyClusterSnapshotScheduleResponse
  = ModifyClusterSnapshotScheduleResponse'
