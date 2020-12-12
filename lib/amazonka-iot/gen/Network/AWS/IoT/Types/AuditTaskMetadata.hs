{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditTaskMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditTaskMetadata
  ( AuditTaskMetadata (..),

    -- * Smart constructor
    mkAuditTaskMetadata,

    -- * Lenses
    atmTaskType,
    atmTaskId,
    atmTaskStatus,
  )
where

import Network.AWS.IoT.Types.AuditTaskStatus
import Network.AWS.IoT.Types.AuditTaskType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The audits that were performed.
--
-- /See:/ 'mkAuditTaskMetadata' smart constructor.
data AuditTaskMetadata = AuditTaskMetadata'
  { taskType ::
      Lude.Maybe AuditTaskType,
    taskId :: Lude.Maybe Lude.Text,
    taskStatus :: Lude.Maybe AuditTaskStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuditTaskMetadata' with the minimum fields required to make a request.
--
-- * 'taskId' - The ID of this audit.
-- * 'taskStatus' - The status of this audit. One of "IN_PROGRESS", "COMPLETED", "FAILED", or "CANCELED".
-- * 'taskType' - The type of this audit. One of "ON_DEMAND_AUDIT_TASK" or "SCHEDULED_AUDIT_TASK".
mkAuditTaskMetadata ::
  AuditTaskMetadata
mkAuditTaskMetadata =
  AuditTaskMetadata'
    { taskType = Lude.Nothing,
      taskId = Lude.Nothing,
      taskStatus = Lude.Nothing
    }

-- | The type of this audit. One of "ON_DEMAND_AUDIT_TASK" or "SCHEDULED_AUDIT_TASK".
--
-- /Note:/ Consider using 'taskType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atmTaskType :: Lens.Lens' AuditTaskMetadata (Lude.Maybe AuditTaskType)
atmTaskType = Lens.lens (taskType :: AuditTaskMetadata -> Lude.Maybe AuditTaskType) (\s a -> s {taskType = a} :: AuditTaskMetadata)
{-# DEPRECATED atmTaskType "Use generic-lens or generic-optics with 'taskType' instead." #-}

-- | The ID of this audit.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atmTaskId :: Lens.Lens' AuditTaskMetadata (Lude.Maybe Lude.Text)
atmTaskId = Lens.lens (taskId :: AuditTaskMetadata -> Lude.Maybe Lude.Text) (\s a -> s {taskId = a} :: AuditTaskMetadata)
{-# DEPRECATED atmTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | The status of this audit. One of "IN_PROGRESS", "COMPLETED", "FAILED", or "CANCELED".
--
-- /Note:/ Consider using 'taskStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atmTaskStatus :: Lens.Lens' AuditTaskMetadata (Lude.Maybe AuditTaskStatus)
atmTaskStatus = Lens.lens (taskStatus :: AuditTaskMetadata -> Lude.Maybe AuditTaskStatus) (\s a -> s {taskStatus = a} :: AuditTaskMetadata)
{-# DEPRECATED atmTaskStatus "Use generic-lens or generic-optics with 'taskStatus' instead." #-}

instance Lude.FromJSON AuditTaskMetadata where
  parseJSON =
    Lude.withObject
      "AuditTaskMetadata"
      ( \x ->
          AuditTaskMetadata'
            Lude.<$> (x Lude..:? "taskType")
            Lude.<*> (x Lude..:? "taskId")
            Lude.<*> (x Lude..:? "taskStatus")
      )
