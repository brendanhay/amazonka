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
-- Module      : Amazonka.SSM.Types.MaintenanceWindowTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.MaintenanceWindowTask where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.LoggingInfo
import Amazonka.SSM.Types.MaintenanceWindowTaskCutoffBehavior
import Amazonka.SSM.Types.MaintenanceWindowTaskParameterValueExpression
import Amazonka.SSM.Types.MaintenanceWindowTaskType
import Amazonka.SSM.Types.Target

-- | Information about a task defined for a maintenance window.
--
-- /See:/ 'newMaintenanceWindowTask' smart constructor.
data MaintenanceWindowTask = MaintenanceWindowTask'
  { -- | The Amazon Resource Name (ARN) of the Identity and Access Management
    -- (IAM) service role to use to publish Amazon Simple Notification Service
    -- (Amazon SNS) notifications for maintenance window Run Command tasks.
    serviceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The task ID.
    windowTaskId :: Prelude.Maybe Prelude.Text,
    -- | The parameters that should be passed to the task when it is run.
    --
    -- @TaskParameters@ has been deprecated. To specify parameters to pass to a
    -- task when it runs, instead use the @Parameters@ option in the
    -- @TaskInvocationParameters@ structure. For information about how Systems
    -- Manager handles these options for the supported maintenance window task
    -- types, see MaintenanceWindowTaskInvocationParameters.
    taskParameters :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text (Core.Sensitive MaintenanceWindowTaskParameterValueExpression))),
    -- | The priority of the task in the maintenance window. The lower the
    -- number, the higher the priority. Tasks that have the same priority are
    -- scheduled in parallel.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The resource that the task uses during execution. For @RUN_COMMAND@ and
    -- @AUTOMATION@ task types, @TaskArn@ is the Amazon Web Services Systems
    -- Manager (SSM document) name or ARN. For @LAMBDA@ tasks, it\'s the
    -- function name or ARN. For @STEP_FUNCTIONS@ tasks, it\'s the state
    -- machine ARN.
    taskArn :: Prelude.Maybe Prelude.Text,
    -- | The specification for whether tasks should continue to run after the
    -- cutoff time specified in the maintenance windows is reached.
    cutoffBehavior :: Prelude.Maybe MaintenanceWindowTaskCutoffBehavior,
    -- | The maximum number of errors allowed before this task stops being
    -- scheduled.
    maxErrors :: Prelude.Maybe Prelude.Text,
    -- | The task name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The targets (either instances or tags). Instances are specified using
    -- @Key=instanceids,Values=\<instanceid1>,\<instanceid2>@. Tags are
    -- specified using @Key=\<tag name>,Values=\<tag value>@.
    targets :: Prelude.Maybe [Target],
    -- | Information about an S3 bucket to write task-level logs to.
    --
    -- @LoggingInfo@ has been deprecated. To specify an Amazon Simple Storage
    -- Service (Amazon S3) bucket to contain logs, instead use the
    -- @OutputS3BucketName@ and @OutputS3KeyPrefix@ options in the
    -- @TaskInvocationParameters@ structure. For information about how Amazon
    -- Web Services Systems Manager handles these options for the supported
    -- maintenance window task types, see
    -- MaintenanceWindowTaskInvocationParameters.
    loggingInfo :: Prelude.Maybe LoggingInfo,
    -- | The type of task.
    type' :: Prelude.Maybe MaintenanceWindowTaskType,
    -- | A description of the task.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The maximum number of targets this task can be run for, in parallel.
    maxConcurrency :: Prelude.Maybe Prelude.Text,
    -- | The ID of the maintenance window where the task is registered.
    windowId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MaintenanceWindowTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceRoleArn', 'maintenanceWindowTask_serviceRoleArn' - The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) service role to use to publish Amazon Simple Notification Service
-- (Amazon SNS) notifications for maintenance window Run Command tasks.
--
-- 'windowTaskId', 'maintenanceWindowTask_windowTaskId' - The task ID.
--
-- 'taskParameters', 'maintenanceWindowTask_taskParameters' - The parameters that should be passed to the task when it is run.
--
-- @TaskParameters@ has been deprecated. To specify parameters to pass to a
-- task when it runs, instead use the @Parameters@ option in the
-- @TaskInvocationParameters@ structure. For information about how Systems
-- Manager handles these options for the supported maintenance window task
-- types, see MaintenanceWindowTaskInvocationParameters.
--
-- 'priority', 'maintenanceWindowTask_priority' - The priority of the task in the maintenance window. The lower the
-- number, the higher the priority. Tasks that have the same priority are
-- scheduled in parallel.
--
-- 'taskArn', 'maintenanceWindowTask_taskArn' - The resource that the task uses during execution. For @RUN_COMMAND@ and
-- @AUTOMATION@ task types, @TaskArn@ is the Amazon Web Services Systems
-- Manager (SSM document) name or ARN. For @LAMBDA@ tasks, it\'s the
-- function name or ARN. For @STEP_FUNCTIONS@ tasks, it\'s the state
-- machine ARN.
--
-- 'cutoffBehavior', 'maintenanceWindowTask_cutoffBehavior' - The specification for whether tasks should continue to run after the
-- cutoff time specified in the maintenance windows is reached.
--
-- 'maxErrors', 'maintenanceWindowTask_maxErrors' - The maximum number of errors allowed before this task stops being
-- scheduled.
--
-- 'name', 'maintenanceWindowTask_name' - The task name.
--
-- 'targets', 'maintenanceWindowTask_targets' - The targets (either instances or tags). Instances are specified using
-- @Key=instanceids,Values=\<instanceid1>,\<instanceid2>@. Tags are
-- specified using @Key=\<tag name>,Values=\<tag value>@.
--
-- 'loggingInfo', 'maintenanceWindowTask_loggingInfo' - Information about an S3 bucket to write task-level logs to.
--
-- @LoggingInfo@ has been deprecated. To specify an Amazon Simple Storage
-- Service (Amazon S3) bucket to contain logs, instead use the
-- @OutputS3BucketName@ and @OutputS3KeyPrefix@ options in the
-- @TaskInvocationParameters@ structure. For information about how Amazon
-- Web Services Systems Manager handles these options for the supported
-- maintenance window task types, see
-- MaintenanceWindowTaskInvocationParameters.
--
-- 'type'', 'maintenanceWindowTask_type' - The type of task.
--
-- 'description', 'maintenanceWindowTask_description' - A description of the task.
--
-- 'maxConcurrency', 'maintenanceWindowTask_maxConcurrency' - The maximum number of targets this task can be run for, in parallel.
--
-- 'windowId', 'maintenanceWindowTask_windowId' - The ID of the maintenance window where the task is registered.
newMaintenanceWindowTask ::
  MaintenanceWindowTask
newMaintenanceWindowTask =
  MaintenanceWindowTask'
    { serviceRoleArn =
        Prelude.Nothing,
      windowTaskId = Prelude.Nothing,
      taskParameters = Prelude.Nothing,
      priority = Prelude.Nothing,
      taskArn = Prelude.Nothing,
      cutoffBehavior = Prelude.Nothing,
      maxErrors = Prelude.Nothing,
      name = Prelude.Nothing,
      targets = Prelude.Nothing,
      loggingInfo = Prelude.Nothing,
      type' = Prelude.Nothing,
      description = Prelude.Nothing,
      maxConcurrency = Prelude.Nothing,
      windowId = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) service role to use to publish Amazon Simple Notification Service
-- (Amazon SNS) notifications for maintenance window Run Command tasks.
maintenanceWindowTask_serviceRoleArn :: Lens.Lens' MaintenanceWindowTask (Prelude.Maybe Prelude.Text)
maintenanceWindowTask_serviceRoleArn = Lens.lens (\MaintenanceWindowTask' {serviceRoleArn} -> serviceRoleArn) (\s@MaintenanceWindowTask' {} a -> s {serviceRoleArn = a} :: MaintenanceWindowTask)

-- | The task ID.
maintenanceWindowTask_windowTaskId :: Lens.Lens' MaintenanceWindowTask (Prelude.Maybe Prelude.Text)
maintenanceWindowTask_windowTaskId = Lens.lens (\MaintenanceWindowTask' {windowTaskId} -> windowTaskId) (\s@MaintenanceWindowTask' {} a -> s {windowTaskId = a} :: MaintenanceWindowTask)

-- | The parameters that should be passed to the task when it is run.
--
-- @TaskParameters@ has been deprecated. To specify parameters to pass to a
-- task when it runs, instead use the @Parameters@ option in the
-- @TaskInvocationParameters@ structure. For information about how Systems
-- Manager handles these options for the supported maintenance window task
-- types, see MaintenanceWindowTaskInvocationParameters.
maintenanceWindowTask_taskParameters :: Lens.Lens' MaintenanceWindowTask (Prelude.Maybe (Prelude.HashMap Prelude.Text MaintenanceWindowTaskParameterValueExpression))
maintenanceWindowTask_taskParameters = Lens.lens (\MaintenanceWindowTask' {taskParameters} -> taskParameters) (\s@MaintenanceWindowTask' {} a -> s {taskParameters = a} :: MaintenanceWindowTask) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | The priority of the task in the maintenance window. The lower the
-- number, the higher the priority. Tasks that have the same priority are
-- scheduled in parallel.
maintenanceWindowTask_priority :: Lens.Lens' MaintenanceWindowTask (Prelude.Maybe Prelude.Natural)
maintenanceWindowTask_priority = Lens.lens (\MaintenanceWindowTask' {priority} -> priority) (\s@MaintenanceWindowTask' {} a -> s {priority = a} :: MaintenanceWindowTask)

-- | The resource that the task uses during execution. For @RUN_COMMAND@ and
-- @AUTOMATION@ task types, @TaskArn@ is the Amazon Web Services Systems
-- Manager (SSM document) name or ARN. For @LAMBDA@ tasks, it\'s the
-- function name or ARN. For @STEP_FUNCTIONS@ tasks, it\'s the state
-- machine ARN.
maintenanceWindowTask_taskArn :: Lens.Lens' MaintenanceWindowTask (Prelude.Maybe Prelude.Text)
maintenanceWindowTask_taskArn = Lens.lens (\MaintenanceWindowTask' {taskArn} -> taskArn) (\s@MaintenanceWindowTask' {} a -> s {taskArn = a} :: MaintenanceWindowTask)

-- | The specification for whether tasks should continue to run after the
-- cutoff time specified in the maintenance windows is reached.
maintenanceWindowTask_cutoffBehavior :: Lens.Lens' MaintenanceWindowTask (Prelude.Maybe MaintenanceWindowTaskCutoffBehavior)
maintenanceWindowTask_cutoffBehavior = Lens.lens (\MaintenanceWindowTask' {cutoffBehavior} -> cutoffBehavior) (\s@MaintenanceWindowTask' {} a -> s {cutoffBehavior = a} :: MaintenanceWindowTask)

-- | The maximum number of errors allowed before this task stops being
-- scheduled.
maintenanceWindowTask_maxErrors :: Lens.Lens' MaintenanceWindowTask (Prelude.Maybe Prelude.Text)
maintenanceWindowTask_maxErrors = Lens.lens (\MaintenanceWindowTask' {maxErrors} -> maxErrors) (\s@MaintenanceWindowTask' {} a -> s {maxErrors = a} :: MaintenanceWindowTask)

-- | The task name.
maintenanceWindowTask_name :: Lens.Lens' MaintenanceWindowTask (Prelude.Maybe Prelude.Text)
maintenanceWindowTask_name = Lens.lens (\MaintenanceWindowTask' {name} -> name) (\s@MaintenanceWindowTask' {} a -> s {name = a} :: MaintenanceWindowTask)

-- | The targets (either instances or tags). Instances are specified using
-- @Key=instanceids,Values=\<instanceid1>,\<instanceid2>@. Tags are
-- specified using @Key=\<tag name>,Values=\<tag value>@.
maintenanceWindowTask_targets :: Lens.Lens' MaintenanceWindowTask (Prelude.Maybe [Target])
maintenanceWindowTask_targets = Lens.lens (\MaintenanceWindowTask' {targets} -> targets) (\s@MaintenanceWindowTask' {} a -> s {targets = a} :: MaintenanceWindowTask) Prelude.. Lens.mapping Lens.coerced

-- | Information about an S3 bucket to write task-level logs to.
--
-- @LoggingInfo@ has been deprecated. To specify an Amazon Simple Storage
-- Service (Amazon S3) bucket to contain logs, instead use the
-- @OutputS3BucketName@ and @OutputS3KeyPrefix@ options in the
-- @TaskInvocationParameters@ structure. For information about how Amazon
-- Web Services Systems Manager handles these options for the supported
-- maintenance window task types, see
-- MaintenanceWindowTaskInvocationParameters.
maintenanceWindowTask_loggingInfo :: Lens.Lens' MaintenanceWindowTask (Prelude.Maybe LoggingInfo)
maintenanceWindowTask_loggingInfo = Lens.lens (\MaintenanceWindowTask' {loggingInfo} -> loggingInfo) (\s@MaintenanceWindowTask' {} a -> s {loggingInfo = a} :: MaintenanceWindowTask)

-- | The type of task.
maintenanceWindowTask_type :: Lens.Lens' MaintenanceWindowTask (Prelude.Maybe MaintenanceWindowTaskType)
maintenanceWindowTask_type = Lens.lens (\MaintenanceWindowTask' {type'} -> type') (\s@MaintenanceWindowTask' {} a -> s {type' = a} :: MaintenanceWindowTask)

-- | A description of the task.
maintenanceWindowTask_description :: Lens.Lens' MaintenanceWindowTask (Prelude.Maybe Prelude.Text)
maintenanceWindowTask_description = Lens.lens (\MaintenanceWindowTask' {description} -> description) (\s@MaintenanceWindowTask' {} a -> s {description = a} :: MaintenanceWindowTask) Prelude.. Lens.mapping Core._Sensitive

-- | The maximum number of targets this task can be run for, in parallel.
maintenanceWindowTask_maxConcurrency :: Lens.Lens' MaintenanceWindowTask (Prelude.Maybe Prelude.Text)
maintenanceWindowTask_maxConcurrency = Lens.lens (\MaintenanceWindowTask' {maxConcurrency} -> maxConcurrency) (\s@MaintenanceWindowTask' {} a -> s {maxConcurrency = a} :: MaintenanceWindowTask)

-- | The ID of the maintenance window where the task is registered.
maintenanceWindowTask_windowId :: Lens.Lens' MaintenanceWindowTask (Prelude.Maybe Prelude.Text)
maintenanceWindowTask_windowId = Lens.lens (\MaintenanceWindowTask' {windowId} -> windowId) (\s@MaintenanceWindowTask' {} a -> s {windowId = a} :: MaintenanceWindowTask)

instance Core.FromJSON MaintenanceWindowTask where
  parseJSON =
    Core.withObject
      "MaintenanceWindowTask"
      ( \x ->
          MaintenanceWindowTask'
            Prelude.<$> (x Core..:? "ServiceRoleArn")
            Prelude.<*> (x Core..:? "WindowTaskId")
            Prelude.<*> (x Core..:? "TaskParameters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Priority")
            Prelude.<*> (x Core..:? "TaskArn")
            Prelude.<*> (x Core..:? "CutoffBehavior")
            Prelude.<*> (x Core..:? "MaxErrors")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Targets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "LoggingInfo")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "MaxConcurrency")
            Prelude.<*> (x Core..:? "WindowId")
      )

instance Prelude.Hashable MaintenanceWindowTask

instance Prelude.NFData MaintenanceWindowTask
