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
    mcssDisassociateSchedule,
    mcssClusterIdentifier,
    mcssScheduleIdentifier,

    -- * Destructuring the response
    ModifyClusterSnapshotScheduleResponse (..),
    mkModifyClusterSnapshotScheduleResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyClusterSnapshotSchedule' smart constructor.
data ModifyClusterSnapshotSchedule = ModifyClusterSnapshotSchedule'
  { -- | A boolean to indicate whether to remove the assoiciation between the cluster and the schedule.
    disassociateSchedule :: Lude.Maybe Lude.Bool,
    -- | A unique identifier for the cluster whose snapshot schedule you want to modify.
    clusterIdentifier :: Lude.Text,
    -- | A unique alphanumeric identifier for the schedule that you want to associate with the cluster.
    scheduleIdentifier :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyClusterSnapshotSchedule' with the minimum fields required to make a request.
--
-- * 'disassociateSchedule' - A boolean to indicate whether to remove the assoiciation between the cluster and the schedule.
-- * 'clusterIdentifier' - A unique identifier for the cluster whose snapshot schedule you want to modify.
-- * 'scheduleIdentifier' - A unique alphanumeric identifier for the schedule that you want to associate with the cluster.
mkModifyClusterSnapshotSchedule ::
  -- | 'clusterIdentifier'
  Lude.Text ->
  ModifyClusterSnapshotSchedule
mkModifyClusterSnapshotSchedule pClusterIdentifier_ =
  ModifyClusterSnapshotSchedule'
    { disassociateSchedule =
        Lude.Nothing,
      clusterIdentifier = pClusterIdentifier_,
      scheduleIdentifier = Lude.Nothing
    }

-- | A boolean to indicate whether to remove the assoiciation between the cluster and the schedule.
--
-- /Note:/ Consider using 'disassociateSchedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcssDisassociateSchedule :: Lens.Lens' ModifyClusterSnapshotSchedule (Lude.Maybe Lude.Bool)
mcssDisassociateSchedule = Lens.lens (disassociateSchedule :: ModifyClusterSnapshotSchedule -> Lude.Maybe Lude.Bool) (\s a -> s {disassociateSchedule = a} :: ModifyClusterSnapshotSchedule)
{-# DEPRECATED mcssDisassociateSchedule "Use generic-lens or generic-optics with 'disassociateSchedule' instead." #-}

-- | A unique identifier for the cluster whose snapshot schedule you want to modify.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcssClusterIdentifier :: Lens.Lens' ModifyClusterSnapshotSchedule Lude.Text
mcssClusterIdentifier = Lens.lens (clusterIdentifier :: ModifyClusterSnapshotSchedule -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: ModifyClusterSnapshotSchedule)
{-# DEPRECATED mcssClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | A unique alphanumeric identifier for the schedule that you want to associate with the cluster.
--
-- /Note:/ Consider using 'scheduleIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcssScheduleIdentifier :: Lens.Lens' ModifyClusterSnapshotSchedule (Lude.Maybe Lude.Text)
mcssScheduleIdentifier = Lens.lens (scheduleIdentifier :: ModifyClusterSnapshotSchedule -> Lude.Maybe Lude.Text) (\s a -> s {scheduleIdentifier = a} :: ModifyClusterSnapshotSchedule)
{-# DEPRECATED mcssScheduleIdentifier "Use generic-lens or generic-optics with 'scheduleIdentifier' instead." #-}

instance Lude.AWSRequest ModifyClusterSnapshotSchedule where
  type
    Rs ModifyClusterSnapshotSchedule =
      ModifyClusterSnapshotScheduleResponse
  request = Req.postQuery redshiftService
  response = Res.receiveNull ModifyClusterSnapshotScheduleResponse'

instance Lude.ToHeaders ModifyClusterSnapshotSchedule where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyClusterSnapshotSchedule where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyClusterSnapshotSchedule where
  toQuery ModifyClusterSnapshotSchedule' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ModifyClusterSnapshotSchedule" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "DisassociateSchedule" Lude.=: disassociateSchedule,
        "ClusterIdentifier" Lude.=: clusterIdentifier,
        "ScheduleIdentifier" Lude.=: scheduleIdentifier
      ]

-- | /See:/ 'mkModifyClusterSnapshotScheduleResponse' smart constructor.
data ModifyClusterSnapshotScheduleResponse = ModifyClusterSnapshotScheduleResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyClusterSnapshotScheduleResponse' with the minimum fields required to make a request.
mkModifyClusterSnapshotScheduleResponse ::
  ModifyClusterSnapshotScheduleResponse
mkModifyClusterSnapshotScheduleResponse =
  ModifyClusterSnapshotScheduleResponse'
