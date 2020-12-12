{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditMitigationActionsTaskMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditMitigationActionsTaskMetadata
  ( AuditMitigationActionsTaskMetadata (..),

    -- * Smart constructor
    mkAuditMitigationActionsTaskMetadata,

    -- * Lenses
    amatmStartTime,
    amatmTaskId,
    amatmTaskStatus,
  )
where

import Network.AWS.IoT.Types.AuditMitigationActionsTaskStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an audit mitigation actions task that is returned by @ListAuditMitigationActionsTasks@ .
--
-- /See:/ 'mkAuditMitigationActionsTaskMetadata' smart constructor.
data AuditMitigationActionsTaskMetadata = AuditMitigationActionsTaskMetadata'
  { startTime ::
      Lude.Maybe
        Lude.Timestamp,
    taskId ::
      Lude.Maybe Lude.Text,
    taskStatus ::
      Lude.Maybe
        AuditMitigationActionsTaskStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuditMitigationActionsTaskMetadata' with the minimum fields required to make a request.
--
-- * 'startTime' - The time at which the audit mitigation actions task was started.
-- * 'taskId' - The unique identifier for the task.
-- * 'taskStatus' - The current state of the audit mitigation actions task.
mkAuditMitigationActionsTaskMetadata ::
  AuditMitigationActionsTaskMetadata
mkAuditMitigationActionsTaskMetadata =
  AuditMitigationActionsTaskMetadata'
    { startTime = Lude.Nothing,
      taskId = Lude.Nothing,
      taskStatus = Lude.Nothing
    }

-- | The time at which the audit mitigation actions task was started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amatmStartTime :: Lens.Lens' AuditMitigationActionsTaskMetadata (Lude.Maybe Lude.Timestamp)
amatmStartTime = Lens.lens (startTime :: AuditMitigationActionsTaskMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: AuditMitigationActionsTaskMetadata)
{-# DEPRECATED amatmStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The unique identifier for the task.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amatmTaskId :: Lens.Lens' AuditMitigationActionsTaskMetadata (Lude.Maybe Lude.Text)
amatmTaskId = Lens.lens (taskId :: AuditMitigationActionsTaskMetadata -> Lude.Maybe Lude.Text) (\s a -> s {taskId = a} :: AuditMitigationActionsTaskMetadata)
{-# DEPRECATED amatmTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | The current state of the audit mitigation actions task.
--
-- /Note:/ Consider using 'taskStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amatmTaskStatus :: Lens.Lens' AuditMitigationActionsTaskMetadata (Lude.Maybe AuditMitigationActionsTaskStatus)
amatmTaskStatus = Lens.lens (taskStatus :: AuditMitigationActionsTaskMetadata -> Lude.Maybe AuditMitigationActionsTaskStatus) (\s a -> s {taskStatus = a} :: AuditMitigationActionsTaskMetadata)
{-# DEPRECATED amatmTaskStatus "Use generic-lens or generic-optics with 'taskStatus' instead." #-}

instance Lude.FromJSON AuditMitigationActionsTaskMetadata where
  parseJSON =
    Lude.withObject
      "AuditMitigationActionsTaskMetadata"
      ( \x ->
          AuditMitigationActionsTaskMetadata'
            Lude.<$> (x Lude..:? "startTime")
            Lude.<*> (x Lude..:? "taskId")
            Lude.<*> (x Lude..:? "taskStatus")
      )
