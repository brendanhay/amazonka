{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteSnapshotSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a snapshot schedule.
module Network.AWS.Redshift.DeleteSnapshotSchedule
  ( -- * Creating a request
    DeleteSnapshotSchedule (..),
    mkDeleteSnapshotSchedule,

    -- ** Request lenses
    dScheduleIdentifier,

    -- * Destructuring the response
    DeleteSnapshotScheduleResponse (..),
    mkDeleteSnapshotScheduleResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteSnapshotSchedule' smart constructor.
newtype DeleteSnapshotSchedule = DeleteSnapshotSchedule'
  { scheduleIdentifier ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSnapshotSchedule' with the minimum fields required to make a request.
--
-- * 'scheduleIdentifier' - A unique identifier of the snapshot schedule to delete.
mkDeleteSnapshotSchedule ::
  -- | 'scheduleIdentifier'
  Lude.Text ->
  DeleteSnapshotSchedule
mkDeleteSnapshotSchedule pScheduleIdentifier_ =
  DeleteSnapshotSchedule'
    { scheduleIdentifier =
        pScheduleIdentifier_
    }

-- | A unique identifier of the snapshot schedule to delete.
--
-- /Note:/ Consider using 'scheduleIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dScheduleIdentifier :: Lens.Lens' DeleteSnapshotSchedule Lude.Text
dScheduleIdentifier = Lens.lens (scheduleIdentifier :: DeleteSnapshotSchedule -> Lude.Text) (\s a -> s {scheduleIdentifier = a} :: DeleteSnapshotSchedule)
{-# DEPRECATED dScheduleIdentifier "Use generic-lens or generic-optics with 'scheduleIdentifier' instead." #-}

instance Lude.AWSRequest DeleteSnapshotSchedule where
  type Rs DeleteSnapshotSchedule = DeleteSnapshotScheduleResponse
  request = Req.postQuery redshiftService
  response = Res.receiveNull DeleteSnapshotScheduleResponse'

instance Lude.ToHeaders DeleteSnapshotSchedule where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteSnapshotSchedule where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteSnapshotSchedule where
  toQuery DeleteSnapshotSchedule' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteSnapshotSchedule" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ScheduleIdentifier" Lude.=: scheduleIdentifier
      ]

-- | /See:/ 'mkDeleteSnapshotScheduleResponse' smart constructor.
data DeleteSnapshotScheduleResponse = DeleteSnapshotScheduleResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSnapshotScheduleResponse' with the minimum fields required to make a request.
mkDeleteSnapshotScheduleResponse ::
  DeleteSnapshotScheduleResponse
mkDeleteSnapshotScheduleResponse = DeleteSnapshotScheduleResponse'
