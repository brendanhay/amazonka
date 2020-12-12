{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.Task
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.Task
  ( Task (..),

    -- * Smart constructor
    mkTask,

    -- * Lenses
    tProgressPercent,
    tStatusDetail,
    tStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types.MigrationStatus
import qualified Network.AWS.Prelude as Lude

-- | Task object encapsulating task information.
--
-- /See:/ 'mkTask' smart constructor.
data Task = Task'
  { progressPercent :: Lude.Maybe Lude.Natural,
    statusDetail :: Lude.Maybe Lude.Text,
    status :: MigrationStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Task' with the minimum fields required to make a request.
--
-- * 'progressPercent' - Indication of the percentage completion of the task.
-- * 'status' - Status of the task - Not Started, In-Progress, Complete.
-- * 'statusDetail' - Details of task status as notified by a migration tool. A tool might use this field to provide clarifying information about the status that is unique to that tool or that explains an error state.
mkTask ::
  -- | 'status'
  MigrationStatus ->
  Task
mkTask pStatus_ =
  Task'
    { progressPercent = Lude.Nothing,
      statusDetail = Lude.Nothing,
      status = pStatus_
    }

-- | Indication of the percentage completion of the task.
--
-- /Note:/ Consider using 'progressPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tProgressPercent :: Lens.Lens' Task (Lude.Maybe Lude.Natural)
tProgressPercent = Lens.lens (progressPercent :: Task -> Lude.Maybe Lude.Natural) (\s a -> s {progressPercent = a} :: Task)
{-# DEPRECATED tProgressPercent "Use generic-lens or generic-optics with 'progressPercent' instead." #-}

-- | Details of task status as notified by a migration tool. A tool might use this field to provide clarifying information about the status that is unique to that tool or that explains an error state.
--
-- /Note:/ Consider using 'statusDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStatusDetail :: Lens.Lens' Task (Lude.Maybe Lude.Text)
tStatusDetail = Lens.lens (statusDetail :: Task -> Lude.Maybe Lude.Text) (\s a -> s {statusDetail = a} :: Task)
{-# DEPRECATED tStatusDetail "Use generic-lens or generic-optics with 'statusDetail' instead." #-}

-- | Status of the task - Not Started, In-Progress, Complete.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStatus :: Lens.Lens' Task MigrationStatus
tStatus = Lens.lens (status :: Task -> MigrationStatus) (\s a -> s {status = a} :: Task)
{-# DEPRECATED tStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.FromJSON Task where
  parseJSON =
    Lude.withObject
      "Task"
      ( \x ->
          Task'
            Lude.<$> (x Lude..:? "ProgressPercent")
            Lude.<*> (x Lude..:? "StatusDetail")
            Lude.<*> (x Lude..: "Status")
      )

instance Lude.ToJSON Task where
  toJSON Task' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ProgressPercent" Lude..=) Lude.<$> progressPercent,
            ("StatusDetail" Lude..=) Lude.<$> statusDetail,
            Lude.Just ("Status" Lude..= status)
          ]
      )
