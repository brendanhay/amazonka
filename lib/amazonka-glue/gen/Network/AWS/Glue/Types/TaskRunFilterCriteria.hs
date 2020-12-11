-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TaskRunFilterCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TaskRunFilterCriteria
  ( TaskRunFilterCriteria (..),

    -- * Smart constructor
    mkTaskRunFilterCriteria,

    -- * Lenses
    trfcStatus,
    trfcStartedAfter,
    trfcStartedBefore,
    trfcTaskRunType,
  )
where

import Network.AWS.Glue.Types.TaskStatusType
import Network.AWS.Glue.Types.TaskType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The criteria that are used to filter the task runs for the machine learning transform.
--
-- /See:/ 'mkTaskRunFilterCriteria' smart constructor.
data TaskRunFilterCriteria = TaskRunFilterCriteria'
  { status ::
      Lude.Maybe TaskStatusType,
    startedAfter :: Lude.Maybe Lude.Timestamp,
    startedBefore :: Lude.Maybe Lude.Timestamp,
    taskRunType :: Lude.Maybe TaskType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TaskRunFilterCriteria' with the minimum fields required to make a request.
--
-- * 'startedAfter' - Filter on task runs started after this date.
-- * 'startedBefore' - Filter on task runs started before this date.
-- * 'status' - The current status of the task run.
-- * 'taskRunType' - The type of task run.
mkTaskRunFilterCriteria ::
  TaskRunFilterCriteria
mkTaskRunFilterCriteria =
  TaskRunFilterCriteria'
    { status = Lude.Nothing,
      startedAfter = Lude.Nothing,
      startedBefore = Lude.Nothing,
      taskRunType = Lude.Nothing
    }

-- | The current status of the task run.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trfcStatus :: Lens.Lens' TaskRunFilterCriteria (Lude.Maybe TaskStatusType)
trfcStatus = Lens.lens (status :: TaskRunFilterCriteria -> Lude.Maybe TaskStatusType) (\s a -> s {status = a} :: TaskRunFilterCriteria)
{-# DEPRECATED trfcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Filter on task runs started after this date.
--
-- /Note:/ Consider using 'startedAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trfcStartedAfter :: Lens.Lens' TaskRunFilterCriteria (Lude.Maybe Lude.Timestamp)
trfcStartedAfter = Lens.lens (startedAfter :: TaskRunFilterCriteria -> Lude.Maybe Lude.Timestamp) (\s a -> s {startedAfter = a} :: TaskRunFilterCriteria)
{-# DEPRECATED trfcStartedAfter "Use generic-lens or generic-optics with 'startedAfter' instead." #-}

-- | Filter on task runs started before this date.
--
-- /Note:/ Consider using 'startedBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trfcStartedBefore :: Lens.Lens' TaskRunFilterCriteria (Lude.Maybe Lude.Timestamp)
trfcStartedBefore = Lens.lens (startedBefore :: TaskRunFilterCriteria -> Lude.Maybe Lude.Timestamp) (\s a -> s {startedBefore = a} :: TaskRunFilterCriteria)
{-# DEPRECATED trfcStartedBefore "Use generic-lens or generic-optics with 'startedBefore' instead." #-}

-- | The type of task run.
--
-- /Note:/ Consider using 'taskRunType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trfcTaskRunType :: Lens.Lens' TaskRunFilterCriteria (Lude.Maybe TaskType)
trfcTaskRunType = Lens.lens (taskRunType :: TaskRunFilterCriteria -> Lude.Maybe TaskType) (\s a -> s {taskRunType = a} :: TaskRunFilterCriteria)
{-# DEPRECATED trfcTaskRunType "Use generic-lens or generic-optics with 'taskRunType' instead." #-}

instance Lude.ToJSON TaskRunFilterCriteria where
  toJSON TaskRunFilterCriteria' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Status" Lude..=) Lude.<$> status,
            ("StartedAfter" Lude..=) Lude.<$> startedAfter,
            ("StartedBefore" Lude..=) Lude.<$> startedBefore,
            ("TaskRunType" Lude..=) Lude.<$> taskRunType
          ]
      )
