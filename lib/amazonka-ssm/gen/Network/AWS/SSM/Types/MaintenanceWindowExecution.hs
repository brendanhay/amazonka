{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowExecution
  ( MaintenanceWindowExecution (..),

    -- * Smart constructor
    mkMaintenanceWindowExecution,

    -- * Lenses
    mweStatus,
    mweStartTime,
    mweWindowExecutionId,
    mweStatusDetails,
    mweEndTime,
    mweWindowId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.MaintenanceWindowExecutionStatus

-- | Describes the information about an execution of a maintenance window.
--
-- /See:/ 'mkMaintenanceWindowExecution' smart constructor.
data MaintenanceWindowExecution = MaintenanceWindowExecution'
  { -- | The status of the execution.
    status :: Lude.Maybe MaintenanceWindowExecutionStatus,
    -- | The time the execution started.
    startTime :: Lude.Maybe Lude.Timestamp,
    -- | The ID of the maintenance window execution.
    windowExecutionId :: Lude.Maybe Lude.Text,
    -- | The details explaining the Status. Only available for certain status values.
    statusDetails :: Lude.Maybe Lude.Text,
    -- | The time the execution finished.
    endTime :: Lude.Maybe Lude.Timestamp,
    -- | The ID of the maintenance window.
    windowId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MaintenanceWindowExecution' with the minimum fields required to make a request.
--
-- * 'status' - The status of the execution.
-- * 'startTime' - The time the execution started.
-- * 'windowExecutionId' - The ID of the maintenance window execution.
-- * 'statusDetails' - The details explaining the Status. Only available for certain status values.
-- * 'endTime' - The time the execution finished.
-- * 'windowId' - The ID of the maintenance window.
mkMaintenanceWindowExecution ::
  MaintenanceWindowExecution
mkMaintenanceWindowExecution =
  MaintenanceWindowExecution'
    { status = Lude.Nothing,
      startTime = Lude.Nothing,
      windowExecutionId = Lude.Nothing,
      statusDetails = Lude.Nothing,
      endTime = Lude.Nothing,
      windowId = Lude.Nothing
    }

-- | The status of the execution.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mweStatus :: Lens.Lens' MaintenanceWindowExecution (Lude.Maybe MaintenanceWindowExecutionStatus)
mweStatus = Lens.lens (status :: MaintenanceWindowExecution -> Lude.Maybe MaintenanceWindowExecutionStatus) (\s a -> s {status = a} :: MaintenanceWindowExecution)
{-# DEPRECATED mweStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The time the execution started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mweStartTime :: Lens.Lens' MaintenanceWindowExecution (Lude.Maybe Lude.Timestamp)
mweStartTime = Lens.lens (startTime :: MaintenanceWindowExecution -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: MaintenanceWindowExecution)
{-# DEPRECATED mweStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The ID of the maintenance window execution.
--
-- /Note:/ Consider using 'windowExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mweWindowExecutionId :: Lens.Lens' MaintenanceWindowExecution (Lude.Maybe Lude.Text)
mweWindowExecutionId = Lens.lens (windowExecutionId :: MaintenanceWindowExecution -> Lude.Maybe Lude.Text) (\s a -> s {windowExecutionId = a} :: MaintenanceWindowExecution)
{-# DEPRECATED mweWindowExecutionId "Use generic-lens or generic-optics with 'windowExecutionId' instead." #-}

-- | The details explaining the Status. Only available for certain status values.
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mweStatusDetails :: Lens.Lens' MaintenanceWindowExecution (Lude.Maybe Lude.Text)
mweStatusDetails = Lens.lens (statusDetails :: MaintenanceWindowExecution -> Lude.Maybe Lude.Text) (\s a -> s {statusDetails = a} :: MaintenanceWindowExecution)
{-# DEPRECATED mweStatusDetails "Use generic-lens or generic-optics with 'statusDetails' instead." #-}

-- | The time the execution finished.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mweEndTime :: Lens.Lens' MaintenanceWindowExecution (Lude.Maybe Lude.Timestamp)
mweEndTime = Lens.lens (endTime :: MaintenanceWindowExecution -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: MaintenanceWindowExecution)
{-# DEPRECATED mweEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The ID of the maintenance window.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mweWindowId :: Lens.Lens' MaintenanceWindowExecution (Lude.Maybe Lude.Text)
mweWindowId = Lens.lens (windowId :: MaintenanceWindowExecution -> Lude.Maybe Lude.Text) (\s a -> s {windowId = a} :: MaintenanceWindowExecution)
{-# DEPRECATED mweWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

instance Lude.FromJSON MaintenanceWindowExecution where
  parseJSON =
    Lude.withObject
      "MaintenanceWindowExecution"
      ( \x ->
          MaintenanceWindowExecution'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "StartTime")
            Lude.<*> (x Lude..:? "WindowExecutionId")
            Lude.<*> (x Lude..:? "StatusDetails")
            Lude.<*> (x Lude..:? "EndTime")
            Lude.<*> (x Lude..:? "WindowId")
      )
