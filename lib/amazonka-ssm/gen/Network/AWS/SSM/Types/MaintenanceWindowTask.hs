{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowTask
  ( MaintenanceWindowTask (..),

    -- * Smart constructor
    mkMaintenanceWindowTask,

    -- * Lenses
    mwtServiceRoleARN,
    mwtWindowTaskId,
    mwtTaskParameters,
    mwtPriority,
    mwtTaskARN,
    mwtMaxErrors,
    mwtName,
    mwtTargets,
    mwtLoggingInfo,
    mwtType,
    mwtDescription,
    mwtMaxConcurrency,
    mwtWindowId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.LoggingInfo
import Network.AWS.SSM.Types.MaintenanceWindowTaskParameterValueExpression
import Network.AWS.SSM.Types.MaintenanceWindowTaskType
import Network.AWS.SSM.Types.Target

-- | Information about a task defined for a maintenance window.
--
-- /See:/ 'mkMaintenanceWindowTask' smart constructor.
data MaintenanceWindowTask = MaintenanceWindowTask'
  { -- | The ARN of the IAM service role to use to publish Amazon Simple Notification Service (Amazon SNS) notifications for maintenance window Run Command tasks.
    serviceRoleARN :: Lude.Maybe Lude.Text,
    -- | The task ID.
    windowTaskId :: Lude.Maybe Lude.Text,
    -- | The parameters that should be passed to the task when it is run.
    taskParameters :: Lude.Maybe (Lude.HashMap Lude.Text (MaintenanceWindowTaskParameterValueExpression)),
    -- | The priority of the task in the maintenance window. The lower the number, the higher the priority. Tasks that have the same priority are scheduled in parallel.
    priority :: Lude.Maybe Lude.Natural,
    -- | The resource that the task uses during execution. For RUN_COMMAND and AUTOMATION task types, @TaskArn@ is the Systems Manager document name or ARN. For LAMBDA tasks, it's the function name or ARN. For STEP_FUNCTIONS tasks, it's the state machine ARN.
    taskARN :: Lude.Maybe Lude.Text,
    -- | The maximum number of errors allowed before this task stops being scheduled.
    maxErrors :: Lude.Maybe Lude.Text,
    -- | The task name.
    name :: Lude.Maybe Lude.Text,
    -- | The targets (either instances or tags). Instances are specified using Key=instanceids,Values=<instanceid1>,<instanceid2>. Tags are specified using Key=<tag name>,Values=<tag value>.
    targets :: Lude.Maybe [Target],
    -- | Information about an S3 bucket to write task-level logs to.
    loggingInfo :: Lude.Maybe LoggingInfo,
    -- | The type of task. The type can be one of the following: RUN_COMMAND, AUTOMATION, LAMBDA, or STEP_FUNCTIONS.
    type' :: Lude.Maybe MaintenanceWindowTaskType,
    -- | A description of the task.
    description :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The maximum number of targets this task can be run for, in parallel.
    maxConcurrency :: Lude.Maybe Lude.Text,
    -- | The ID of the maintenance window where the task is registered.
    windowId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MaintenanceWindowTask' with the minimum fields required to make a request.
--
-- * 'serviceRoleARN' - The ARN of the IAM service role to use to publish Amazon Simple Notification Service (Amazon SNS) notifications for maintenance window Run Command tasks.
-- * 'windowTaskId' - The task ID.
-- * 'taskParameters' - The parameters that should be passed to the task when it is run.
-- * 'priority' - The priority of the task in the maintenance window. The lower the number, the higher the priority. Tasks that have the same priority are scheduled in parallel.
-- * 'taskARN' - The resource that the task uses during execution. For RUN_COMMAND and AUTOMATION task types, @TaskArn@ is the Systems Manager document name or ARN. For LAMBDA tasks, it's the function name or ARN. For STEP_FUNCTIONS tasks, it's the state machine ARN.
-- * 'maxErrors' - The maximum number of errors allowed before this task stops being scheduled.
-- * 'name' - The task name.
-- * 'targets' - The targets (either instances or tags). Instances are specified using Key=instanceids,Values=<instanceid1>,<instanceid2>. Tags are specified using Key=<tag name>,Values=<tag value>.
-- * 'loggingInfo' - Information about an S3 bucket to write task-level logs to.
-- * 'type'' - The type of task. The type can be one of the following: RUN_COMMAND, AUTOMATION, LAMBDA, or STEP_FUNCTIONS.
-- * 'description' - A description of the task.
-- * 'maxConcurrency' - The maximum number of targets this task can be run for, in parallel.
-- * 'windowId' - The ID of the maintenance window where the task is registered.
mkMaintenanceWindowTask ::
  MaintenanceWindowTask
mkMaintenanceWindowTask =
  MaintenanceWindowTask'
    { serviceRoleARN = Lude.Nothing,
      windowTaskId = Lude.Nothing,
      taskParameters = Lude.Nothing,
      priority = Lude.Nothing,
      taskARN = Lude.Nothing,
      maxErrors = Lude.Nothing,
      name = Lude.Nothing,
      targets = Lude.Nothing,
      loggingInfo = Lude.Nothing,
      type' = Lude.Nothing,
      description = Lude.Nothing,
      maxConcurrency = Lude.Nothing,
      windowId = Lude.Nothing
    }

-- | The ARN of the IAM service role to use to publish Amazon Simple Notification Service (Amazon SNS) notifications for maintenance window Run Command tasks.
--
-- /Note:/ Consider using 'serviceRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwtServiceRoleARN :: Lens.Lens' MaintenanceWindowTask (Lude.Maybe Lude.Text)
mwtServiceRoleARN = Lens.lens (serviceRoleARN :: MaintenanceWindowTask -> Lude.Maybe Lude.Text) (\s a -> s {serviceRoleARN = a} :: MaintenanceWindowTask)
{-# DEPRECATED mwtServiceRoleARN "Use generic-lens or generic-optics with 'serviceRoleARN' instead." #-}

-- | The task ID.
--
-- /Note:/ Consider using 'windowTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwtWindowTaskId :: Lens.Lens' MaintenanceWindowTask (Lude.Maybe Lude.Text)
mwtWindowTaskId = Lens.lens (windowTaskId :: MaintenanceWindowTask -> Lude.Maybe Lude.Text) (\s a -> s {windowTaskId = a} :: MaintenanceWindowTask)
{-# DEPRECATED mwtWindowTaskId "Use generic-lens or generic-optics with 'windowTaskId' instead." #-}

-- | The parameters that should be passed to the task when it is run.
--
-- /Note:/ Consider using 'taskParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwtTaskParameters :: Lens.Lens' MaintenanceWindowTask (Lude.Maybe (Lude.HashMap Lude.Text (MaintenanceWindowTaskParameterValueExpression)))
mwtTaskParameters = Lens.lens (taskParameters :: MaintenanceWindowTask -> Lude.Maybe (Lude.HashMap Lude.Text (MaintenanceWindowTaskParameterValueExpression))) (\s a -> s {taskParameters = a} :: MaintenanceWindowTask)
{-# DEPRECATED mwtTaskParameters "Use generic-lens or generic-optics with 'taskParameters' instead." #-}

-- | The priority of the task in the maintenance window. The lower the number, the higher the priority. Tasks that have the same priority are scheduled in parallel.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwtPriority :: Lens.Lens' MaintenanceWindowTask (Lude.Maybe Lude.Natural)
mwtPriority = Lens.lens (priority :: MaintenanceWindowTask -> Lude.Maybe Lude.Natural) (\s a -> s {priority = a} :: MaintenanceWindowTask)
{-# DEPRECATED mwtPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The resource that the task uses during execution. For RUN_COMMAND and AUTOMATION task types, @TaskArn@ is the Systems Manager document name or ARN. For LAMBDA tasks, it's the function name or ARN. For STEP_FUNCTIONS tasks, it's the state machine ARN.
--
-- /Note:/ Consider using 'taskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwtTaskARN :: Lens.Lens' MaintenanceWindowTask (Lude.Maybe Lude.Text)
mwtTaskARN = Lens.lens (taskARN :: MaintenanceWindowTask -> Lude.Maybe Lude.Text) (\s a -> s {taskARN = a} :: MaintenanceWindowTask)
{-# DEPRECATED mwtTaskARN "Use generic-lens or generic-optics with 'taskARN' instead." #-}

-- | The maximum number of errors allowed before this task stops being scheduled.
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwtMaxErrors :: Lens.Lens' MaintenanceWindowTask (Lude.Maybe Lude.Text)
mwtMaxErrors = Lens.lens (maxErrors :: MaintenanceWindowTask -> Lude.Maybe Lude.Text) (\s a -> s {maxErrors = a} :: MaintenanceWindowTask)
{-# DEPRECATED mwtMaxErrors "Use generic-lens or generic-optics with 'maxErrors' instead." #-}

-- | The task name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwtName :: Lens.Lens' MaintenanceWindowTask (Lude.Maybe Lude.Text)
mwtName = Lens.lens (name :: MaintenanceWindowTask -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: MaintenanceWindowTask)
{-# DEPRECATED mwtName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The targets (either instances or tags). Instances are specified using Key=instanceids,Values=<instanceid1>,<instanceid2>. Tags are specified using Key=<tag name>,Values=<tag value>.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwtTargets :: Lens.Lens' MaintenanceWindowTask (Lude.Maybe [Target])
mwtTargets = Lens.lens (targets :: MaintenanceWindowTask -> Lude.Maybe [Target]) (\s a -> s {targets = a} :: MaintenanceWindowTask)
{-# DEPRECATED mwtTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | Information about an S3 bucket to write task-level logs to.
--
-- /Note:/ Consider using 'loggingInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwtLoggingInfo :: Lens.Lens' MaintenanceWindowTask (Lude.Maybe LoggingInfo)
mwtLoggingInfo = Lens.lens (loggingInfo :: MaintenanceWindowTask -> Lude.Maybe LoggingInfo) (\s a -> s {loggingInfo = a} :: MaintenanceWindowTask)
{-# DEPRECATED mwtLoggingInfo "Use generic-lens or generic-optics with 'loggingInfo' instead." #-}

-- | The type of task. The type can be one of the following: RUN_COMMAND, AUTOMATION, LAMBDA, or STEP_FUNCTIONS.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwtType :: Lens.Lens' MaintenanceWindowTask (Lude.Maybe MaintenanceWindowTaskType)
mwtType = Lens.lens (type' :: MaintenanceWindowTask -> Lude.Maybe MaintenanceWindowTaskType) (\s a -> s {type' = a} :: MaintenanceWindowTask)
{-# DEPRECATED mwtType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A description of the task.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwtDescription :: Lens.Lens' MaintenanceWindowTask (Lude.Maybe (Lude.Sensitive Lude.Text))
mwtDescription = Lens.lens (description :: MaintenanceWindowTask -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {description = a} :: MaintenanceWindowTask)
{-# DEPRECATED mwtDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The maximum number of targets this task can be run for, in parallel.
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwtMaxConcurrency :: Lens.Lens' MaintenanceWindowTask (Lude.Maybe Lude.Text)
mwtMaxConcurrency = Lens.lens (maxConcurrency :: MaintenanceWindowTask -> Lude.Maybe Lude.Text) (\s a -> s {maxConcurrency = a} :: MaintenanceWindowTask)
{-# DEPRECATED mwtMaxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead." #-}

-- | The ID of the maintenance window where the task is registered.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwtWindowId :: Lens.Lens' MaintenanceWindowTask (Lude.Maybe Lude.Text)
mwtWindowId = Lens.lens (windowId :: MaintenanceWindowTask -> Lude.Maybe Lude.Text) (\s a -> s {windowId = a} :: MaintenanceWindowTask)
{-# DEPRECATED mwtWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

instance Lude.FromJSON MaintenanceWindowTask where
  parseJSON =
    Lude.withObject
      "MaintenanceWindowTask"
      ( \x ->
          MaintenanceWindowTask'
            Lude.<$> (x Lude..:? "ServiceRoleArn")
            Lude.<*> (x Lude..:? "WindowTaskId")
            Lude.<*> (x Lude..:? "TaskParameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Priority")
            Lude.<*> (x Lude..:? "TaskArn")
            Lude.<*> (x Lude..:? "MaxErrors")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Targets" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "LoggingInfo")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "MaxConcurrency")
            Lude.<*> (x Lude..:? "WindowId")
      )
