{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowTask where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.LoggingInfo
import Network.AWS.SSM.Types.MaintenanceWindowTaskParameterValueExpression
import Network.AWS.SSM.Types.MaintenanceWindowTaskType
import Network.AWS.SSM.Types.Target

-- | Information about a task defined for a maintenance window.
--
-- /See:/ 'newMaintenanceWindowTask' smart constructor.
data MaintenanceWindowTask = MaintenanceWindowTask'
  { -- | The maximum number of errors allowed before this task stops being
    -- scheduled.
    maxErrors :: Core.Maybe Core.Text,
    -- | The parameters that should be passed to the task when it is run.
    --
    -- @TaskParameters@ has been deprecated. To specify parameters to pass to a
    -- task when it runs, instead use the @Parameters@ option in the
    -- @TaskInvocationParameters@ structure. For information about how Systems
    -- Manager handles these options for the supported maintenance window task
    -- types, see MaintenanceWindowTaskInvocationParameters.
    taskParameters :: Core.Maybe (Core.Sensitive (Core.HashMap Core.Text (Core.Sensitive MaintenanceWindowTaskParameterValueExpression))),
    -- | The task ID.
    windowTaskId :: Core.Maybe Core.Text,
    -- | The ARN of the IAM service role to use to publish Amazon Simple
    -- Notification Service (Amazon SNS) notifications for maintenance window
    -- Run Command tasks.
    serviceRoleArn :: Core.Maybe Core.Text,
    -- | The priority of the task in the maintenance window. The lower the
    -- number, the higher the priority. Tasks that have the same priority are
    -- scheduled in parallel.
    priority :: Core.Maybe Core.Natural,
    -- | The targets (either instances or tags). Instances are specified using
    -- Key=instanceids,Values=\<instanceid1>,\<instanceid2>. Tags are specified
    -- using Key=\<tag name>,Values=\<tag value>.
    targets :: Core.Maybe [Target],
    -- | The task name.
    name :: Core.Maybe Core.Text,
    -- | The maximum number of targets this task can be run for, in parallel.
    maxConcurrency :: Core.Maybe Core.Text,
    -- | The ID of the maintenance window where the task is registered.
    windowId :: Core.Maybe Core.Text,
    -- | A description of the task.
    description :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The type of task. The type can be one of the following: RUN_COMMAND,
    -- AUTOMATION, LAMBDA, or STEP_FUNCTIONS.
    type' :: Core.Maybe MaintenanceWindowTaskType,
    -- | The resource that the task uses during execution. For RUN_COMMAND and
    -- AUTOMATION task types, @TaskArn@ is the Systems Manager document name or
    -- ARN. For LAMBDA tasks, it\'s the function name or ARN. For
    -- STEP_FUNCTIONS tasks, it\'s the state machine ARN.
    taskArn :: Core.Maybe Core.Text,
    -- | Information about an S3 bucket to write task-level logs to.
    --
    -- @LoggingInfo@ has been deprecated. To specify an S3 bucket to contain
    -- logs, instead use the @OutputS3BucketName@ and @OutputS3KeyPrefix@
    -- options in the @TaskInvocationParameters@ structure. For information
    -- about how Systems Manager handles these options for the supported
    -- maintenance window task types, see
    -- MaintenanceWindowTaskInvocationParameters.
    loggingInfo :: Core.Maybe LoggingInfo
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'MaintenanceWindowTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxErrors', 'maintenanceWindowTask_maxErrors' - The maximum number of errors allowed before this task stops being
-- scheduled.
--
-- 'taskParameters', 'maintenanceWindowTask_taskParameters' - The parameters that should be passed to the task when it is run.
--
-- @TaskParameters@ has been deprecated. To specify parameters to pass to a
-- task when it runs, instead use the @Parameters@ option in the
-- @TaskInvocationParameters@ structure. For information about how Systems
-- Manager handles these options for the supported maintenance window task
-- types, see MaintenanceWindowTaskInvocationParameters.
--
-- 'windowTaskId', 'maintenanceWindowTask_windowTaskId' - The task ID.
--
-- 'serviceRoleArn', 'maintenanceWindowTask_serviceRoleArn' - The ARN of the IAM service role to use to publish Amazon Simple
-- Notification Service (Amazon SNS) notifications for maintenance window
-- Run Command tasks.
--
-- 'priority', 'maintenanceWindowTask_priority' - The priority of the task in the maintenance window. The lower the
-- number, the higher the priority. Tasks that have the same priority are
-- scheduled in parallel.
--
-- 'targets', 'maintenanceWindowTask_targets' - The targets (either instances or tags). Instances are specified using
-- Key=instanceids,Values=\<instanceid1>,\<instanceid2>. Tags are specified
-- using Key=\<tag name>,Values=\<tag value>.
--
-- 'name', 'maintenanceWindowTask_name' - The task name.
--
-- 'maxConcurrency', 'maintenanceWindowTask_maxConcurrency' - The maximum number of targets this task can be run for, in parallel.
--
-- 'windowId', 'maintenanceWindowTask_windowId' - The ID of the maintenance window where the task is registered.
--
-- 'description', 'maintenanceWindowTask_description' - A description of the task.
--
-- 'type'', 'maintenanceWindowTask_type' - The type of task. The type can be one of the following: RUN_COMMAND,
-- AUTOMATION, LAMBDA, or STEP_FUNCTIONS.
--
-- 'taskArn', 'maintenanceWindowTask_taskArn' - The resource that the task uses during execution. For RUN_COMMAND and
-- AUTOMATION task types, @TaskArn@ is the Systems Manager document name or
-- ARN. For LAMBDA tasks, it\'s the function name or ARN. For
-- STEP_FUNCTIONS tasks, it\'s the state machine ARN.
--
-- 'loggingInfo', 'maintenanceWindowTask_loggingInfo' - Information about an S3 bucket to write task-level logs to.
--
-- @LoggingInfo@ has been deprecated. To specify an S3 bucket to contain
-- logs, instead use the @OutputS3BucketName@ and @OutputS3KeyPrefix@
-- options in the @TaskInvocationParameters@ structure. For information
-- about how Systems Manager handles these options for the supported
-- maintenance window task types, see
-- MaintenanceWindowTaskInvocationParameters.
newMaintenanceWindowTask ::
  MaintenanceWindowTask
newMaintenanceWindowTask =
  MaintenanceWindowTask'
    { maxErrors = Core.Nothing,
      taskParameters = Core.Nothing,
      windowTaskId = Core.Nothing,
      serviceRoleArn = Core.Nothing,
      priority = Core.Nothing,
      targets = Core.Nothing,
      name = Core.Nothing,
      maxConcurrency = Core.Nothing,
      windowId = Core.Nothing,
      description = Core.Nothing,
      type' = Core.Nothing,
      taskArn = Core.Nothing,
      loggingInfo = Core.Nothing
    }

-- | The maximum number of errors allowed before this task stops being
-- scheduled.
maintenanceWindowTask_maxErrors :: Lens.Lens' MaintenanceWindowTask (Core.Maybe Core.Text)
maintenanceWindowTask_maxErrors = Lens.lens (\MaintenanceWindowTask' {maxErrors} -> maxErrors) (\s@MaintenanceWindowTask' {} a -> s {maxErrors = a} :: MaintenanceWindowTask)

-- | The parameters that should be passed to the task when it is run.
--
-- @TaskParameters@ has been deprecated. To specify parameters to pass to a
-- task when it runs, instead use the @Parameters@ option in the
-- @TaskInvocationParameters@ structure. For information about how Systems
-- Manager handles these options for the supported maintenance window task
-- types, see MaintenanceWindowTaskInvocationParameters.
maintenanceWindowTask_taskParameters :: Lens.Lens' MaintenanceWindowTask (Core.Maybe (Core.HashMap Core.Text MaintenanceWindowTaskParameterValueExpression))
maintenanceWindowTask_taskParameters = Lens.lens (\MaintenanceWindowTask' {taskParameters} -> taskParameters) (\s@MaintenanceWindowTask' {} a -> s {taskParameters = a} :: MaintenanceWindowTask) Core.. Lens.mapping (Core._Sensitive Core.. Lens._Coerce)

-- | The task ID.
maintenanceWindowTask_windowTaskId :: Lens.Lens' MaintenanceWindowTask (Core.Maybe Core.Text)
maintenanceWindowTask_windowTaskId = Lens.lens (\MaintenanceWindowTask' {windowTaskId} -> windowTaskId) (\s@MaintenanceWindowTask' {} a -> s {windowTaskId = a} :: MaintenanceWindowTask)

-- | The ARN of the IAM service role to use to publish Amazon Simple
-- Notification Service (Amazon SNS) notifications for maintenance window
-- Run Command tasks.
maintenanceWindowTask_serviceRoleArn :: Lens.Lens' MaintenanceWindowTask (Core.Maybe Core.Text)
maintenanceWindowTask_serviceRoleArn = Lens.lens (\MaintenanceWindowTask' {serviceRoleArn} -> serviceRoleArn) (\s@MaintenanceWindowTask' {} a -> s {serviceRoleArn = a} :: MaintenanceWindowTask)

-- | The priority of the task in the maintenance window. The lower the
-- number, the higher the priority. Tasks that have the same priority are
-- scheduled in parallel.
maintenanceWindowTask_priority :: Lens.Lens' MaintenanceWindowTask (Core.Maybe Core.Natural)
maintenanceWindowTask_priority = Lens.lens (\MaintenanceWindowTask' {priority} -> priority) (\s@MaintenanceWindowTask' {} a -> s {priority = a} :: MaintenanceWindowTask)

-- | The targets (either instances or tags). Instances are specified using
-- Key=instanceids,Values=\<instanceid1>,\<instanceid2>. Tags are specified
-- using Key=\<tag name>,Values=\<tag value>.
maintenanceWindowTask_targets :: Lens.Lens' MaintenanceWindowTask (Core.Maybe [Target])
maintenanceWindowTask_targets = Lens.lens (\MaintenanceWindowTask' {targets} -> targets) (\s@MaintenanceWindowTask' {} a -> s {targets = a} :: MaintenanceWindowTask) Core.. Lens.mapping Lens._Coerce

-- | The task name.
maintenanceWindowTask_name :: Lens.Lens' MaintenanceWindowTask (Core.Maybe Core.Text)
maintenanceWindowTask_name = Lens.lens (\MaintenanceWindowTask' {name} -> name) (\s@MaintenanceWindowTask' {} a -> s {name = a} :: MaintenanceWindowTask)

-- | The maximum number of targets this task can be run for, in parallel.
maintenanceWindowTask_maxConcurrency :: Lens.Lens' MaintenanceWindowTask (Core.Maybe Core.Text)
maintenanceWindowTask_maxConcurrency = Lens.lens (\MaintenanceWindowTask' {maxConcurrency} -> maxConcurrency) (\s@MaintenanceWindowTask' {} a -> s {maxConcurrency = a} :: MaintenanceWindowTask)

-- | The ID of the maintenance window where the task is registered.
maintenanceWindowTask_windowId :: Lens.Lens' MaintenanceWindowTask (Core.Maybe Core.Text)
maintenanceWindowTask_windowId = Lens.lens (\MaintenanceWindowTask' {windowId} -> windowId) (\s@MaintenanceWindowTask' {} a -> s {windowId = a} :: MaintenanceWindowTask)

-- | A description of the task.
maintenanceWindowTask_description :: Lens.Lens' MaintenanceWindowTask (Core.Maybe Core.Text)
maintenanceWindowTask_description = Lens.lens (\MaintenanceWindowTask' {description} -> description) (\s@MaintenanceWindowTask' {} a -> s {description = a} :: MaintenanceWindowTask) Core.. Lens.mapping Core._Sensitive

-- | The type of task. The type can be one of the following: RUN_COMMAND,
-- AUTOMATION, LAMBDA, or STEP_FUNCTIONS.
maintenanceWindowTask_type :: Lens.Lens' MaintenanceWindowTask (Core.Maybe MaintenanceWindowTaskType)
maintenanceWindowTask_type = Lens.lens (\MaintenanceWindowTask' {type'} -> type') (\s@MaintenanceWindowTask' {} a -> s {type' = a} :: MaintenanceWindowTask)

-- | The resource that the task uses during execution. For RUN_COMMAND and
-- AUTOMATION task types, @TaskArn@ is the Systems Manager document name or
-- ARN. For LAMBDA tasks, it\'s the function name or ARN. For
-- STEP_FUNCTIONS tasks, it\'s the state machine ARN.
maintenanceWindowTask_taskArn :: Lens.Lens' MaintenanceWindowTask (Core.Maybe Core.Text)
maintenanceWindowTask_taskArn = Lens.lens (\MaintenanceWindowTask' {taskArn} -> taskArn) (\s@MaintenanceWindowTask' {} a -> s {taskArn = a} :: MaintenanceWindowTask)

-- | Information about an S3 bucket to write task-level logs to.
--
-- @LoggingInfo@ has been deprecated. To specify an S3 bucket to contain
-- logs, instead use the @OutputS3BucketName@ and @OutputS3KeyPrefix@
-- options in the @TaskInvocationParameters@ structure. For information
-- about how Systems Manager handles these options for the supported
-- maintenance window task types, see
-- MaintenanceWindowTaskInvocationParameters.
maintenanceWindowTask_loggingInfo :: Lens.Lens' MaintenanceWindowTask (Core.Maybe LoggingInfo)
maintenanceWindowTask_loggingInfo = Lens.lens (\MaintenanceWindowTask' {loggingInfo} -> loggingInfo) (\s@MaintenanceWindowTask' {} a -> s {loggingInfo = a} :: MaintenanceWindowTask)

instance Core.FromJSON MaintenanceWindowTask where
  parseJSON =
    Core.withObject
      "MaintenanceWindowTask"
      ( \x ->
          MaintenanceWindowTask'
            Core.<$> (x Core..:? "MaxErrors")
            Core.<*> (x Core..:? "TaskParameters" Core..!= Core.mempty)
            Core.<*> (x Core..:? "WindowTaskId")
            Core.<*> (x Core..:? "ServiceRoleArn")
            Core.<*> (x Core..:? "Priority")
            Core.<*> (x Core..:? "Targets" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "MaxConcurrency")
            Core.<*> (x Core..:? "WindowId")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "Type")
            Core.<*> (x Core..:? "TaskArn")
            Core.<*> (x Core..:? "LoggingInfo")
      )

instance Core.Hashable MaintenanceWindowTask

instance Core.NFData MaintenanceWindowTask
