{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowExecutionTaskInvocationIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowExecutionTaskInvocationIdentity
  ( MaintenanceWindowExecutionTaskInvocationIdentity (..),

    -- * Smart constructor
    mkMaintenanceWindowExecutionTaskInvocationIdentity,

    -- * Lenses
    mwetiiStatus,
    mwetiiExecutionId,
    mwetiiTaskExecutionId,
    mwetiiStartTime,
    mwetiiInvocationId,
    mwetiiOwnerInformation,
    mwetiiTaskType,
    mwetiiWindowTargetId,
    mwetiiWindowExecutionId,
    mwetiiStatusDetails,
    mwetiiEndTime,
    mwetiiParameters,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.MaintenanceWindowExecutionStatus
import Network.AWS.SSM.Types.MaintenanceWindowTaskType

-- | Describes the information about a task invocation for a particular target as part of a task execution performed as part of a maintenance window execution.
--
-- /See:/ 'mkMaintenanceWindowExecutionTaskInvocationIdentity' smart constructor.
data MaintenanceWindowExecutionTaskInvocationIdentity = MaintenanceWindowExecutionTaskInvocationIdentity'
  { -- | The status of the task invocation.
    status :: Lude.Maybe MaintenanceWindowExecutionStatus,
    -- | The ID of the action performed in the service that actually handled the task invocation. If the task type is RUN_COMMAND, this value is the command ID.
    executionId :: Lude.Maybe Lude.Text,
    -- | The ID of the specific task execution in the maintenance window execution.
    taskExecutionId :: Lude.Maybe Lude.Text,
    -- | The time the invocation started.
    startTime :: Lude.Maybe Lude.Timestamp,
    -- | The ID of the task invocation.
    invocationId :: Lude.Maybe Lude.Text,
    -- | User-provided value that was specified when the target was registered with the maintenance window. This was also included in any CloudWatch events raised during the task invocation.
    ownerInformation :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The task type.
    taskType :: Lude.Maybe MaintenanceWindowTaskType,
    -- | The ID of the target definition in this maintenance window the invocation was performed for.
    windowTargetId :: Lude.Maybe Lude.Text,
    -- | The ID of the maintenance window execution that ran the task.
    windowExecutionId :: Lude.Maybe Lude.Text,
    -- | The details explaining the status of the task invocation. Only available for certain Status values.
    statusDetails :: Lude.Maybe Lude.Text,
    -- | The time the invocation finished.
    endTime :: Lude.Maybe Lude.Timestamp,
    -- | The parameters that were provided for the invocation when it was run.
    parameters :: Lude.Maybe (Lude.Sensitive Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MaintenanceWindowExecutionTaskInvocationIdentity' with the minimum fields required to make a request.
--
-- * 'status' - The status of the task invocation.
-- * 'executionId' - The ID of the action performed in the service that actually handled the task invocation. If the task type is RUN_COMMAND, this value is the command ID.
-- * 'taskExecutionId' - The ID of the specific task execution in the maintenance window execution.
-- * 'startTime' - The time the invocation started.
-- * 'invocationId' - The ID of the task invocation.
-- * 'ownerInformation' - User-provided value that was specified when the target was registered with the maintenance window. This was also included in any CloudWatch events raised during the task invocation.
-- * 'taskType' - The task type.
-- * 'windowTargetId' - The ID of the target definition in this maintenance window the invocation was performed for.
-- * 'windowExecutionId' - The ID of the maintenance window execution that ran the task.
-- * 'statusDetails' - The details explaining the status of the task invocation. Only available for certain Status values.
-- * 'endTime' - The time the invocation finished.
-- * 'parameters' - The parameters that were provided for the invocation when it was run.
mkMaintenanceWindowExecutionTaskInvocationIdentity ::
  MaintenanceWindowExecutionTaskInvocationIdentity
mkMaintenanceWindowExecutionTaskInvocationIdentity =
  MaintenanceWindowExecutionTaskInvocationIdentity'
    { status =
        Lude.Nothing,
      executionId = Lude.Nothing,
      taskExecutionId = Lude.Nothing,
      startTime = Lude.Nothing,
      invocationId = Lude.Nothing,
      ownerInformation = Lude.Nothing,
      taskType = Lude.Nothing,
      windowTargetId = Lude.Nothing,
      windowExecutionId = Lude.Nothing,
      statusDetails = Lude.Nothing,
      endTime = Lude.Nothing,
      parameters = Lude.Nothing
    }

-- | The status of the task invocation.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiiStatus :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Lude.Maybe MaintenanceWindowExecutionStatus)
mwetiiStatus = Lens.lens (status :: MaintenanceWindowExecutionTaskInvocationIdentity -> Lude.Maybe MaintenanceWindowExecutionStatus) (\s a -> s {status = a} :: MaintenanceWindowExecutionTaskInvocationIdentity)
{-# DEPRECATED mwetiiStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ID of the action performed in the service that actually handled the task invocation. If the task type is RUN_COMMAND, this value is the command ID.
--
-- /Note:/ Consider using 'executionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiiExecutionId :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Lude.Maybe Lude.Text)
mwetiiExecutionId = Lens.lens (executionId :: MaintenanceWindowExecutionTaskInvocationIdentity -> Lude.Maybe Lude.Text) (\s a -> s {executionId = a} :: MaintenanceWindowExecutionTaskInvocationIdentity)
{-# DEPRECATED mwetiiExecutionId "Use generic-lens or generic-optics with 'executionId' instead." #-}

-- | The ID of the specific task execution in the maintenance window execution.
--
-- /Note:/ Consider using 'taskExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiiTaskExecutionId :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Lude.Maybe Lude.Text)
mwetiiTaskExecutionId = Lens.lens (taskExecutionId :: MaintenanceWindowExecutionTaskInvocationIdentity -> Lude.Maybe Lude.Text) (\s a -> s {taskExecutionId = a} :: MaintenanceWindowExecutionTaskInvocationIdentity)
{-# DEPRECATED mwetiiTaskExecutionId "Use generic-lens or generic-optics with 'taskExecutionId' instead." #-}

-- | The time the invocation started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiiStartTime :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Lude.Maybe Lude.Timestamp)
mwetiiStartTime = Lens.lens (startTime :: MaintenanceWindowExecutionTaskInvocationIdentity -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: MaintenanceWindowExecutionTaskInvocationIdentity)
{-# DEPRECATED mwetiiStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The ID of the task invocation.
--
-- /Note:/ Consider using 'invocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiiInvocationId :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Lude.Maybe Lude.Text)
mwetiiInvocationId = Lens.lens (invocationId :: MaintenanceWindowExecutionTaskInvocationIdentity -> Lude.Maybe Lude.Text) (\s a -> s {invocationId = a} :: MaintenanceWindowExecutionTaskInvocationIdentity)
{-# DEPRECATED mwetiiInvocationId "Use generic-lens or generic-optics with 'invocationId' instead." #-}

-- | User-provided value that was specified when the target was registered with the maintenance window. This was also included in any CloudWatch events raised during the task invocation.
--
-- /Note:/ Consider using 'ownerInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiiOwnerInformation :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Lude.Maybe (Lude.Sensitive Lude.Text))
mwetiiOwnerInformation = Lens.lens (ownerInformation :: MaintenanceWindowExecutionTaskInvocationIdentity -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {ownerInformation = a} :: MaintenanceWindowExecutionTaskInvocationIdentity)
{-# DEPRECATED mwetiiOwnerInformation "Use generic-lens or generic-optics with 'ownerInformation' instead." #-}

-- | The task type.
--
-- /Note:/ Consider using 'taskType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiiTaskType :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Lude.Maybe MaintenanceWindowTaskType)
mwetiiTaskType = Lens.lens (taskType :: MaintenanceWindowExecutionTaskInvocationIdentity -> Lude.Maybe MaintenanceWindowTaskType) (\s a -> s {taskType = a} :: MaintenanceWindowExecutionTaskInvocationIdentity)
{-# DEPRECATED mwetiiTaskType "Use generic-lens or generic-optics with 'taskType' instead." #-}

-- | The ID of the target definition in this maintenance window the invocation was performed for.
--
-- /Note:/ Consider using 'windowTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiiWindowTargetId :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Lude.Maybe Lude.Text)
mwetiiWindowTargetId = Lens.lens (windowTargetId :: MaintenanceWindowExecutionTaskInvocationIdentity -> Lude.Maybe Lude.Text) (\s a -> s {windowTargetId = a} :: MaintenanceWindowExecutionTaskInvocationIdentity)
{-# DEPRECATED mwetiiWindowTargetId "Use generic-lens or generic-optics with 'windowTargetId' instead." #-}

-- | The ID of the maintenance window execution that ran the task.
--
-- /Note:/ Consider using 'windowExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiiWindowExecutionId :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Lude.Maybe Lude.Text)
mwetiiWindowExecutionId = Lens.lens (windowExecutionId :: MaintenanceWindowExecutionTaskInvocationIdentity -> Lude.Maybe Lude.Text) (\s a -> s {windowExecutionId = a} :: MaintenanceWindowExecutionTaskInvocationIdentity)
{-# DEPRECATED mwetiiWindowExecutionId "Use generic-lens or generic-optics with 'windowExecutionId' instead." #-}

-- | The details explaining the status of the task invocation. Only available for certain Status values.
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiiStatusDetails :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Lude.Maybe Lude.Text)
mwetiiStatusDetails = Lens.lens (statusDetails :: MaintenanceWindowExecutionTaskInvocationIdentity -> Lude.Maybe Lude.Text) (\s a -> s {statusDetails = a} :: MaintenanceWindowExecutionTaskInvocationIdentity)
{-# DEPRECATED mwetiiStatusDetails "Use generic-lens or generic-optics with 'statusDetails' instead." #-}

-- | The time the invocation finished.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiiEndTime :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Lude.Maybe Lude.Timestamp)
mwetiiEndTime = Lens.lens (endTime :: MaintenanceWindowExecutionTaskInvocationIdentity -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: MaintenanceWindowExecutionTaskInvocationIdentity)
{-# DEPRECATED mwetiiEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The parameters that were provided for the invocation when it was run.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiiParameters :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Lude.Maybe (Lude.Sensitive Lude.Text))
mwetiiParameters = Lens.lens (parameters :: MaintenanceWindowExecutionTaskInvocationIdentity -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {parameters = a} :: MaintenanceWindowExecutionTaskInvocationIdentity)
{-# DEPRECATED mwetiiParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

instance
  Lude.FromJSON
    MaintenanceWindowExecutionTaskInvocationIdentity
  where
  parseJSON =
    Lude.withObject
      "MaintenanceWindowExecutionTaskInvocationIdentity"
      ( \x ->
          MaintenanceWindowExecutionTaskInvocationIdentity'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "ExecutionId")
            Lude.<*> (x Lude..:? "TaskExecutionId")
            Lude.<*> (x Lude..:? "StartTime")
            Lude.<*> (x Lude..:? "InvocationId")
            Lude.<*> (x Lude..:? "OwnerInformation")
            Lude.<*> (x Lude..:? "TaskType")
            Lude.<*> (x Lude..:? "WindowTargetId")
            Lude.<*> (x Lude..:? "WindowExecutionId")
            Lude.<*> (x Lude..:? "StatusDetails")
            Lude.<*> (x Lude..:? "EndTime")
            Lude.<*> (x Lude..:? "Parameters")
      )
