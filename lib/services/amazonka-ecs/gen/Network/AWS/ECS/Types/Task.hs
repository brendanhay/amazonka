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
-- Module      : Network.AWS.ECS.Types.Task
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Task where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types.Attachment
import Network.AWS.ECS.Types.Attribute
import Network.AWS.ECS.Types.Connectivity
import Network.AWS.ECS.Types.Container
import Network.AWS.ECS.Types.EphemeralStorage
import Network.AWS.ECS.Types.HealthStatus
import Network.AWS.ECS.Types.InferenceAccelerator
import Network.AWS.ECS.Types.LaunchType
import Network.AWS.ECS.Types.Tag
import Network.AWS.ECS.Types.TaskOverride
import Network.AWS.ECS.Types.TaskStopCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details on a task in a cluster.
--
-- /See:/ 'newTask' smart constructor.
data Task = Task'
  { -- | The Unix timestamp for when the task was stopped (the task transitioned
    -- from the @RUNNING@ state to the @STOPPED@ state).
    stoppedAt :: Prelude.Maybe Core.POSIX,
    -- | The desired status of the task. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-lifecycle.html Task Lifecycle>.
    desiredStatus :: Prelude.Maybe Prelude.Text,
    -- | One or more container overrides.
    overrides :: Prelude.Maybe TaskOverride,
    -- | The Elastic Inference accelerator associated with the task.
    inferenceAccelerators :: Prelude.Maybe [InferenceAccelerator],
    -- | The ARN of the cluster that hosts the task.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the task group associated with the task.
    group' :: Prelude.Maybe Prelude.Text,
    -- | The Elastic Network Adapter associated with the task if the task uses
    -- the @awsvpc@ network mode.
    attachments :: Prelude.Maybe [Attachment],
    -- | The Unix timestamp for when the task was created (the task entered the
    -- @PENDING@ state).
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The stop code indicating why a task was stopped. The @stoppedReason@ may
    -- contain additional details.
    stopCode :: Prelude.Maybe TaskStopCode,
    -- | The platform version on which your task is running. A platform version
    -- is only specified for tasks using the Fargate launch type. If one is not
    -- specified, the @LATEST@ platform version is used by default. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate Platform Versions>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    platformVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the task.
    taskArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the container instances that host the task.
    containerInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The Unix timestamp for when the task execution stopped.
    executionStoppedAt :: Prelude.Maybe Core.POSIX,
    -- | The ephemeral storage settings for the task.
    ephemeralStorage :: Prelude.Maybe EphemeralStorage,
    -- | The last known status of the task. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-lifecycle.html Task Lifecycle>.
    lastStatus :: Prelude.Maybe Prelude.Text,
    -- | The amount of memory (in MiB) used by the task as expressed in a task
    -- definition. It can be expressed as an integer using MiB, for example
    -- @1024@. It can also be expressed as a string using GB, for example @1GB@
    -- or @1 GB@. String values are converted to an integer indicating the MiB
    -- when the task definition is registered.
    --
    -- If you are using the EC2 launch type, this field is optional.
    --
    -- If you are using the Fargate launch type, this field is required and you
    -- must use one of the following values, which determines your range of
    -- supported values for the @cpu@ parameter:
    --
    -- -   512 (0.5 GB), 1024 (1 GB), 2048 (2 GB) - Available @cpu@ values: 256
    --     (.25 vCPU)
    --
    -- -   1024 (1 GB), 2048 (2 GB), 3072 (3 GB), 4096 (4 GB) - Available @cpu@
    --     values: 512 (.5 vCPU)
    --
    -- -   2048 (2 GB), 3072 (3 GB), 4096 (4 GB), 5120 (5 GB), 6144 (6 GB),
    --     7168 (7 GB), 8192 (8 GB) - Available @cpu@ values: 1024 (1 vCPU)
    --
    -- -   Between 4096 (4 GB) and 16384 (16 GB) in increments of 1024 (1 GB) -
    --     Available @cpu@ values: 2048 (2 vCPU)
    --
    -- -   Between 8192 (8 GB) and 30720 (30 GB) in increments of 1024 (1 GB) -
    --     Available @cpu@ values: 4096 (4 vCPU)
    memory :: Prelude.Maybe Prelude.Text,
    -- | The Unix timestamp for when the container image pull completed.
    pullStoppedAt :: Prelude.Maybe Core.POSIX,
    -- | The containers associated with the task.
    containers :: Prelude.Maybe [Container],
    -- | The Unix timestamp for when the task started (the task transitioned from
    -- the @PENDING@ state to the @RUNNING@ state).
    startedAt :: Prelude.Maybe Core.POSIX,
    -- | The availability zone of the task.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The attributes of the task
    attributes :: Prelude.Maybe [Attribute],
    -- | The version counter for the task. Every time a task experiences a change
    -- that triggers a CloudWatch event, the version counter is incremented. If
    -- you are replicating your Amazon ECS task state with CloudWatch Events,
    -- you can compare the version of a task reported by the Amazon ECS API
    -- actions with the version reported in CloudWatch Events for the task
    -- (inside the @detail@ object) to verify that the version in your event
    -- stream is current.
    version :: Prelude.Maybe Prelude.Integer,
    -- | The capacity provider associated with the task.
    capacityProviderName :: Prelude.Maybe Prelude.Text,
    -- | The tag specified when a task is started. If the task is started by an
    -- Amazon ECS service, then the @startedBy@ parameter contains the
    -- deployment ID of the service that starts it.
    startedBy :: Prelude.Maybe Prelude.Text,
    -- | The reason that the task was stopped.
    stoppedReason :: Prelude.Maybe Prelude.Text,
    -- | The connectivity status of a task.
    connectivity :: Prelude.Maybe Connectivity,
    -- | The Unix timestamp for when the task stops (transitions from the
    -- @RUNNING@ state to @STOPPED@).
    stoppingAt :: Prelude.Maybe Core.POSIX,
    -- | The infrastructure on which your task is running. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS launch types>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    launchType :: Prelude.Maybe LaunchType,
    -- | The ARN of the task definition that creates the task.
    taskDefinitionArn :: Prelude.Maybe Prelude.Text,
    -- | The health status for the task, which is determined by the health of the
    -- essential containers in the task. If all essential containers in the
    -- task are reporting as @HEALTHY@, then the task status also reports as
    -- @HEALTHY@. If any essential containers in the task are reporting as
    -- @UNHEALTHY@ or @UNKNOWN@, then the task status also reports as
    -- @UNHEALTHY@ or @UNKNOWN@, accordingly.
    --
    -- The Amazon ECS container agent does not monitor or report on Docker
    -- health checks that are embedded in a container image (such as those
    -- specified in a parent image or from the image\'s Dockerfile) and not
    -- specified in the container definition. Health check parameters that are
    -- specified in a container definition override any Docker health checks
    -- that exist in the container image.
    healthStatus :: Prelude.Maybe HealthStatus,
    -- | The Unix timestamp for when the task last went into @CONNECTED@ status.
    connectivityAt :: Prelude.Maybe Core.POSIX,
    -- | The number of CPU units used by the task as expressed in a task
    -- definition. It can be expressed as an integer using CPU units, for
    -- example @1024@. It can also be expressed as a string using vCPUs, for
    -- example @1 vCPU@ or @1 vcpu@. String values are converted to an integer
    -- indicating the CPU units when the task definition is registered.
    --
    -- If you are using the EC2 launch type, this field is optional. Supported
    -- values are between @128@ CPU units (@0.125@ vCPUs) and @10240@ CPU units
    -- (@10@ vCPUs).
    --
    -- If you are using the Fargate launch type, this field is required and you
    -- must use one of the following values, which determines your range of
    -- supported values for the @memory@ parameter:
    --
    -- -   256 (.25 vCPU) - Available @memory@ values: 512 (0.5 GB), 1024 (1
    --     GB), 2048 (2 GB)
    --
    -- -   512 (.5 vCPU) - Available @memory@ values: 1024 (1 GB), 2048 (2 GB),
    --     3072 (3 GB), 4096 (4 GB)
    --
    -- -   1024 (1 vCPU) - Available @memory@ values: 2048 (2 GB), 3072 (3 GB),
    --     4096 (4 GB), 5120 (5 GB), 6144 (6 GB), 7168 (7 GB), 8192 (8 GB)
    --
    -- -   2048 (2 vCPU) - Available @memory@ values: Between 4096 (4 GB) and
    --     16384 (16 GB) in increments of 1024 (1 GB)
    --
    -- -   4096 (4 vCPU) - Available @memory@ values: Between 8192 (8 GB) and
    --     30720 (30 GB) in increments of 1024 (1 GB)
    cpu :: Prelude.Maybe Prelude.Text,
    -- | Whether or not execute command functionality is enabled for this task.
    -- If @true@, this enables execute command functionality on all containers
    -- in the task.
    enableExecuteCommand :: Prelude.Maybe Prelude.Bool,
    -- | The Unix timestamp for when the container image pull began.
    pullStartedAt :: Prelude.Maybe Core.POSIX,
    -- | The metadata that you apply to the task to help you categorize and
    -- organize them. Each tag consists of a key and an optional value, both of
    -- which you define.
    --
    -- The following basic restrictions apply to tags:
    --
    -- -   Maximum number of tags per resource - 50
    --
    -- -   For each resource, each tag key must be unique, and each tag key can
    --     have only one value.
    --
    -- -   Maximum key length - 128 Unicode characters in UTF-8
    --
    -- -   Maximum value length - 256 Unicode characters in UTF-8
    --
    -- -   If your tagging schema is used across multiple services and
    --     resources, remember that other services may have restrictions on
    --     allowed characters. Generally allowed characters are: letters,
    --     numbers, and spaces representable in UTF-8, and the following
    --     characters: + - = . _ : \/ \@.
    --
    -- -   Tag keys and values are case-sensitive.
    --
    -- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
    --     such as a prefix for either keys or values as it is reserved for
    --     Amazon Web Services use. You cannot edit or delete tag keys or
    --     values with this prefix. Tags with this prefix do not count against
    --     your tags per resource limit.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Task' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stoppedAt', 'task_stoppedAt' - The Unix timestamp for when the task was stopped (the task transitioned
-- from the @RUNNING@ state to the @STOPPED@ state).
--
-- 'desiredStatus', 'task_desiredStatus' - The desired status of the task. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-lifecycle.html Task Lifecycle>.
--
-- 'overrides', 'task_overrides' - One or more container overrides.
--
-- 'inferenceAccelerators', 'task_inferenceAccelerators' - The Elastic Inference accelerator associated with the task.
--
-- 'clusterArn', 'task_clusterArn' - The ARN of the cluster that hosts the task.
--
-- 'group'', 'task_group' - The name of the task group associated with the task.
--
-- 'attachments', 'task_attachments' - The Elastic Network Adapter associated with the task if the task uses
-- the @awsvpc@ network mode.
--
-- 'createdAt', 'task_createdAt' - The Unix timestamp for when the task was created (the task entered the
-- @PENDING@ state).
--
-- 'stopCode', 'task_stopCode' - The stop code indicating why a task was stopped. The @stoppedReason@ may
-- contain additional details.
--
-- 'platformVersion', 'task_platformVersion' - The platform version on which your task is running. A platform version
-- is only specified for tasks using the Fargate launch type. If one is not
-- specified, the @LATEST@ platform version is used by default. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate Platform Versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'taskArn', 'task_taskArn' - The Amazon Resource Name (ARN) of the task.
--
-- 'containerInstanceArn', 'task_containerInstanceArn' - The ARN of the container instances that host the task.
--
-- 'executionStoppedAt', 'task_executionStoppedAt' - The Unix timestamp for when the task execution stopped.
--
-- 'ephemeralStorage', 'task_ephemeralStorage' - The ephemeral storage settings for the task.
--
-- 'lastStatus', 'task_lastStatus' - The last known status of the task. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-lifecycle.html Task Lifecycle>.
--
-- 'memory', 'task_memory' - The amount of memory (in MiB) used by the task as expressed in a task
-- definition. It can be expressed as an integer using MiB, for example
-- @1024@. It can also be expressed as a string using GB, for example @1GB@
-- or @1 GB@. String values are converted to an integer indicating the MiB
-- when the task definition is registered.
--
-- If you are using the EC2 launch type, this field is optional.
--
-- If you are using the Fargate launch type, this field is required and you
-- must use one of the following values, which determines your range of
-- supported values for the @cpu@ parameter:
--
-- -   512 (0.5 GB), 1024 (1 GB), 2048 (2 GB) - Available @cpu@ values: 256
--     (.25 vCPU)
--
-- -   1024 (1 GB), 2048 (2 GB), 3072 (3 GB), 4096 (4 GB) - Available @cpu@
--     values: 512 (.5 vCPU)
--
-- -   2048 (2 GB), 3072 (3 GB), 4096 (4 GB), 5120 (5 GB), 6144 (6 GB),
--     7168 (7 GB), 8192 (8 GB) - Available @cpu@ values: 1024 (1 vCPU)
--
-- -   Between 4096 (4 GB) and 16384 (16 GB) in increments of 1024 (1 GB) -
--     Available @cpu@ values: 2048 (2 vCPU)
--
-- -   Between 8192 (8 GB) and 30720 (30 GB) in increments of 1024 (1 GB) -
--     Available @cpu@ values: 4096 (4 vCPU)
--
-- 'pullStoppedAt', 'task_pullStoppedAt' - The Unix timestamp for when the container image pull completed.
--
-- 'containers', 'task_containers' - The containers associated with the task.
--
-- 'startedAt', 'task_startedAt' - The Unix timestamp for when the task started (the task transitioned from
-- the @PENDING@ state to the @RUNNING@ state).
--
-- 'availabilityZone', 'task_availabilityZone' - The availability zone of the task.
--
-- 'attributes', 'task_attributes' - The attributes of the task
--
-- 'version', 'task_version' - The version counter for the task. Every time a task experiences a change
-- that triggers a CloudWatch event, the version counter is incremented. If
-- you are replicating your Amazon ECS task state with CloudWatch Events,
-- you can compare the version of a task reported by the Amazon ECS API
-- actions with the version reported in CloudWatch Events for the task
-- (inside the @detail@ object) to verify that the version in your event
-- stream is current.
--
-- 'capacityProviderName', 'task_capacityProviderName' - The capacity provider associated with the task.
--
-- 'startedBy', 'task_startedBy' - The tag specified when a task is started. If the task is started by an
-- Amazon ECS service, then the @startedBy@ parameter contains the
-- deployment ID of the service that starts it.
--
-- 'stoppedReason', 'task_stoppedReason' - The reason that the task was stopped.
--
-- 'connectivity', 'task_connectivity' - The connectivity status of a task.
--
-- 'stoppingAt', 'task_stoppingAt' - The Unix timestamp for when the task stops (transitions from the
-- @RUNNING@ state to @STOPPED@).
--
-- 'launchType', 'task_launchType' - The infrastructure on which your task is running. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS launch types>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'taskDefinitionArn', 'task_taskDefinitionArn' - The ARN of the task definition that creates the task.
--
-- 'healthStatus', 'task_healthStatus' - The health status for the task, which is determined by the health of the
-- essential containers in the task. If all essential containers in the
-- task are reporting as @HEALTHY@, then the task status also reports as
-- @HEALTHY@. If any essential containers in the task are reporting as
-- @UNHEALTHY@ or @UNKNOWN@, then the task status also reports as
-- @UNHEALTHY@ or @UNKNOWN@, accordingly.
--
-- The Amazon ECS container agent does not monitor or report on Docker
-- health checks that are embedded in a container image (such as those
-- specified in a parent image or from the image\'s Dockerfile) and not
-- specified in the container definition. Health check parameters that are
-- specified in a container definition override any Docker health checks
-- that exist in the container image.
--
-- 'connectivityAt', 'task_connectivityAt' - The Unix timestamp for when the task last went into @CONNECTED@ status.
--
-- 'cpu', 'task_cpu' - The number of CPU units used by the task as expressed in a task
-- definition. It can be expressed as an integer using CPU units, for
-- example @1024@. It can also be expressed as a string using vCPUs, for
-- example @1 vCPU@ or @1 vcpu@. String values are converted to an integer
-- indicating the CPU units when the task definition is registered.
--
-- If you are using the EC2 launch type, this field is optional. Supported
-- values are between @128@ CPU units (@0.125@ vCPUs) and @10240@ CPU units
-- (@10@ vCPUs).
--
-- If you are using the Fargate launch type, this field is required and you
-- must use one of the following values, which determines your range of
-- supported values for the @memory@ parameter:
--
-- -   256 (.25 vCPU) - Available @memory@ values: 512 (0.5 GB), 1024 (1
--     GB), 2048 (2 GB)
--
-- -   512 (.5 vCPU) - Available @memory@ values: 1024 (1 GB), 2048 (2 GB),
--     3072 (3 GB), 4096 (4 GB)
--
-- -   1024 (1 vCPU) - Available @memory@ values: 2048 (2 GB), 3072 (3 GB),
--     4096 (4 GB), 5120 (5 GB), 6144 (6 GB), 7168 (7 GB), 8192 (8 GB)
--
-- -   2048 (2 vCPU) - Available @memory@ values: Between 4096 (4 GB) and
--     16384 (16 GB) in increments of 1024 (1 GB)
--
-- -   4096 (4 vCPU) - Available @memory@ values: Between 8192 (8 GB) and
--     30720 (30 GB) in increments of 1024 (1 GB)
--
-- 'enableExecuteCommand', 'task_enableExecuteCommand' - Whether or not execute command functionality is enabled for this task.
-- If @true@, this enables execute command functionality on all containers
-- in the task.
--
-- 'pullStartedAt', 'task_pullStartedAt' - The Unix timestamp for when the container image pull began.
--
-- 'tags', 'task_tags' - The metadata that you apply to the task to help you categorize and
-- organize them. Each tag consists of a key and an optional value, both of
-- which you define.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per resource - 50
--
-- -   For each resource, each tag key must be unique, and each tag key can
--     have only one value.
--
-- -   Maximum key length - 128 Unicode characters in UTF-8
--
-- -   Maximum value length - 256 Unicode characters in UTF-8
--
-- -   If your tagging schema is used across multiple services and
--     resources, remember that other services may have restrictions on
--     allowed characters. Generally allowed characters are: letters,
--     numbers, and spaces representable in UTF-8, and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Tag keys and values are case-sensitive.
--
-- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
--     such as a prefix for either keys or values as it is reserved for
--     Amazon Web Services use. You cannot edit or delete tag keys or
--     values with this prefix. Tags with this prefix do not count against
--     your tags per resource limit.
newTask ::
  Task
newTask =
  Task'
    { stoppedAt = Prelude.Nothing,
      desiredStatus = Prelude.Nothing,
      overrides = Prelude.Nothing,
      inferenceAccelerators = Prelude.Nothing,
      clusterArn = Prelude.Nothing,
      group' = Prelude.Nothing,
      attachments = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      stopCode = Prelude.Nothing,
      platformVersion = Prelude.Nothing,
      taskArn = Prelude.Nothing,
      containerInstanceArn = Prelude.Nothing,
      executionStoppedAt = Prelude.Nothing,
      ephemeralStorage = Prelude.Nothing,
      lastStatus = Prelude.Nothing,
      memory = Prelude.Nothing,
      pullStoppedAt = Prelude.Nothing,
      containers = Prelude.Nothing,
      startedAt = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      attributes = Prelude.Nothing,
      version = Prelude.Nothing,
      capacityProviderName = Prelude.Nothing,
      startedBy = Prelude.Nothing,
      stoppedReason = Prelude.Nothing,
      connectivity = Prelude.Nothing,
      stoppingAt = Prelude.Nothing,
      launchType = Prelude.Nothing,
      taskDefinitionArn = Prelude.Nothing,
      healthStatus = Prelude.Nothing,
      connectivityAt = Prelude.Nothing,
      cpu = Prelude.Nothing,
      enableExecuteCommand = Prelude.Nothing,
      pullStartedAt = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The Unix timestamp for when the task was stopped (the task transitioned
-- from the @RUNNING@ state to the @STOPPED@ state).
task_stoppedAt :: Lens.Lens' Task (Prelude.Maybe Prelude.UTCTime)
task_stoppedAt = Lens.lens (\Task' {stoppedAt} -> stoppedAt) (\s@Task' {} a -> s {stoppedAt = a} :: Task) Prelude.. Lens.mapping Core._Time

-- | The desired status of the task. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-lifecycle.html Task Lifecycle>.
task_desiredStatus :: Lens.Lens' Task (Prelude.Maybe Prelude.Text)
task_desiredStatus = Lens.lens (\Task' {desiredStatus} -> desiredStatus) (\s@Task' {} a -> s {desiredStatus = a} :: Task)

-- | One or more container overrides.
task_overrides :: Lens.Lens' Task (Prelude.Maybe TaskOverride)
task_overrides = Lens.lens (\Task' {overrides} -> overrides) (\s@Task' {} a -> s {overrides = a} :: Task)

-- | The Elastic Inference accelerator associated with the task.
task_inferenceAccelerators :: Lens.Lens' Task (Prelude.Maybe [InferenceAccelerator])
task_inferenceAccelerators = Lens.lens (\Task' {inferenceAccelerators} -> inferenceAccelerators) (\s@Task' {} a -> s {inferenceAccelerators = a} :: Task) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the cluster that hosts the task.
task_clusterArn :: Lens.Lens' Task (Prelude.Maybe Prelude.Text)
task_clusterArn = Lens.lens (\Task' {clusterArn} -> clusterArn) (\s@Task' {} a -> s {clusterArn = a} :: Task)

-- | The name of the task group associated with the task.
task_group :: Lens.Lens' Task (Prelude.Maybe Prelude.Text)
task_group = Lens.lens (\Task' {group'} -> group') (\s@Task' {} a -> s {group' = a} :: Task)

-- | The Elastic Network Adapter associated with the task if the task uses
-- the @awsvpc@ network mode.
task_attachments :: Lens.Lens' Task (Prelude.Maybe [Attachment])
task_attachments = Lens.lens (\Task' {attachments} -> attachments) (\s@Task' {} a -> s {attachments = a} :: Task) Prelude.. Lens.mapping Lens.coerced

-- | The Unix timestamp for when the task was created (the task entered the
-- @PENDING@ state).
task_createdAt :: Lens.Lens' Task (Prelude.Maybe Prelude.UTCTime)
task_createdAt = Lens.lens (\Task' {createdAt} -> createdAt) (\s@Task' {} a -> s {createdAt = a} :: Task) Prelude.. Lens.mapping Core._Time

-- | The stop code indicating why a task was stopped. The @stoppedReason@ may
-- contain additional details.
task_stopCode :: Lens.Lens' Task (Prelude.Maybe TaskStopCode)
task_stopCode = Lens.lens (\Task' {stopCode} -> stopCode) (\s@Task' {} a -> s {stopCode = a} :: Task)

-- | The platform version on which your task is running. A platform version
-- is only specified for tasks using the Fargate launch type. If one is not
-- specified, the @LATEST@ platform version is used by default. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate Platform Versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
task_platformVersion :: Lens.Lens' Task (Prelude.Maybe Prelude.Text)
task_platformVersion = Lens.lens (\Task' {platformVersion} -> platformVersion) (\s@Task' {} a -> s {platformVersion = a} :: Task)

-- | The Amazon Resource Name (ARN) of the task.
task_taskArn :: Lens.Lens' Task (Prelude.Maybe Prelude.Text)
task_taskArn = Lens.lens (\Task' {taskArn} -> taskArn) (\s@Task' {} a -> s {taskArn = a} :: Task)

-- | The ARN of the container instances that host the task.
task_containerInstanceArn :: Lens.Lens' Task (Prelude.Maybe Prelude.Text)
task_containerInstanceArn = Lens.lens (\Task' {containerInstanceArn} -> containerInstanceArn) (\s@Task' {} a -> s {containerInstanceArn = a} :: Task)

-- | The Unix timestamp for when the task execution stopped.
task_executionStoppedAt :: Lens.Lens' Task (Prelude.Maybe Prelude.UTCTime)
task_executionStoppedAt = Lens.lens (\Task' {executionStoppedAt} -> executionStoppedAt) (\s@Task' {} a -> s {executionStoppedAt = a} :: Task) Prelude.. Lens.mapping Core._Time

-- | The ephemeral storage settings for the task.
task_ephemeralStorage :: Lens.Lens' Task (Prelude.Maybe EphemeralStorage)
task_ephemeralStorage = Lens.lens (\Task' {ephemeralStorage} -> ephemeralStorage) (\s@Task' {} a -> s {ephemeralStorage = a} :: Task)

-- | The last known status of the task. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-lifecycle.html Task Lifecycle>.
task_lastStatus :: Lens.Lens' Task (Prelude.Maybe Prelude.Text)
task_lastStatus = Lens.lens (\Task' {lastStatus} -> lastStatus) (\s@Task' {} a -> s {lastStatus = a} :: Task)

-- | The amount of memory (in MiB) used by the task as expressed in a task
-- definition. It can be expressed as an integer using MiB, for example
-- @1024@. It can also be expressed as a string using GB, for example @1GB@
-- or @1 GB@. String values are converted to an integer indicating the MiB
-- when the task definition is registered.
--
-- If you are using the EC2 launch type, this field is optional.
--
-- If you are using the Fargate launch type, this field is required and you
-- must use one of the following values, which determines your range of
-- supported values for the @cpu@ parameter:
--
-- -   512 (0.5 GB), 1024 (1 GB), 2048 (2 GB) - Available @cpu@ values: 256
--     (.25 vCPU)
--
-- -   1024 (1 GB), 2048 (2 GB), 3072 (3 GB), 4096 (4 GB) - Available @cpu@
--     values: 512 (.5 vCPU)
--
-- -   2048 (2 GB), 3072 (3 GB), 4096 (4 GB), 5120 (5 GB), 6144 (6 GB),
--     7168 (7 GB), 8192 (8 GB) - Available @cpu@ values: 1024 (1 vCPU)
--
-- -   Between 4096 (4 GB) and 16384 (16 GB) in increments of 1024 (1 GB) -
--     Available @cpu@ values: 2048 (2 vCPU)
--
-- -   Between 8192 (8 GB) and 30720 (30 GB) in increments of 1024 (1 GB) -
--     Available @cpu@ values: 4096 (4 vCPU)
task_memory :: Lens.Lens' Task (Prelude.Maybe Prelude.Text)
task_memory = Lens.lens (\Task' {memory} -> memory) (\s@Task' {} a -> s {memory = a} :: Task)

-- | The Unix timestamp for when the container image pull completed.
task_pullStoppedAt :: Lens.Lens' Task (Prelude.Maybe Prelude.UTCTime)
task_pullStoppedAt = Lens.lens (\Task' {pullStoppedAt} -> pullStoppedAt) (\s@Task' {} a -> s {pullStoppedAt = a} :: Task) Prelude.. Lens.mapping Core._Time

-- | The containers associated with the task.
task_containers :: Lens.Lens' Task (Prelude.Maybe [Container])
task_containers = Lens.lens (\Task' {containers} -> containers) (\s@Task' {} a -> s {containers = a} :: Task) Prelude.. Lens.mapping Lens.coerced

-- | The Unix timestamp for when the task started (the task transitioned from
-- the @PENDING@ state to the @RUNNING@ state).
task_startedAt :: Lens.Lens' Task (Prelude.Maybe Prelude.UTCTime)
task_startedAt = Lens.lens (\Task' {startedAt} -> startedAt) (\s@Task' {} a -> s {startedAt = a} :: Task) Prelude.. Lens.mapping Core._Time

-- | The availability zone of the task.
task_availabilityZone :: Lens.Lens' Task (Prelude.Maybe Prelude.Text)
task_availabilityZone = Lens.lens (\Task' {availabilityZone} -> availabilityZone) (\s@Task' {} a -> s {availabilityZone = a} :: Task)

-- | The attributes of the task
task_attributes :: Lens.Lens' Task (Prelude.Maybe [Attribute])
task_attributes = Lens.lens (\Task' {attributes} -> attributes) (\s@Task' {} a -> s {attributes = a} :: Task) Prelude.. Lens.mapping Lens.coerced

-- | The version counter for the task. Every time a task experiences a change
-- that triggers a CloudWatch event, the version counter is incremented. If
-- you are replicating your Amazon ECS task state with CloudWatch Events,
-- you can compare the version of a task reported by the Amazon ECS API
-- actions with the version reported in CloudWatch Events for the task
-- (inside the @detail@ object) to verify that the version in your event
-- stream is current.
task_version :: Lens.Lens' Task (Prelude.Maybe Prelude.Integer)
task_version = Lens.lens (\Task' {version} -> version) (\s@Task' {} a -> s {version = a} :: Task)

-- | The capacity provider associated with the task.
task_capacityProviderName :: Lens.Lens' Task (Prelude.Maybe Prelude.Text)
task_capacityProviderName = Lens.lens (\Task' {capacityProviderName} -> capacityProviderName) (\s@Task' {} a -> s {capacityProviderName = a} :: Task)

-- | The tag specified when a task is started. If the task is started by an
-- Amazon ECS service, then the @startedBy@ parameter contains the
-- deployment ID of the service that starts it.
task_startedBy :: Lens.Lens' Task (Prelude.Maybe Prelude.Text)
task_startedBy = Lens.lens (\Task' {startedBy} -> startedBy) (\s@Task' {} a -> s {startedBy = a} :: Task)

-- | The reason that the task was stopped.
task_stoppedReason :: Lens.Lens' Task (Prelude.Maybe Prelude.Text)
task_stoppedReason = Lens.lens (\Task' {stoppedReason} -> stoppedReason) (\s@Task' {} a -> s {stoppedReason = a} :: Task)

-- | The connectivity status of a task.
task_connectivity :: Lens.Lens' Task (Prelude.Maybe Connectivity)
task_connectivity = Lens.lens (\Task' {connectivity} -> connectivity) (\s@Task' {} a -> s {connectivity = a} :: Task)

-- | The Unix timestamp for when the task stops (transitions from the
-- @RUNNING@ state to @STOPPED@).
task_stoppingAt :: Lens.Lens' Task (Prelude.Maybe Prelude.UTCTime)
task_stoppingAt = Lens.lens (\Task' {stoppingAt} -> stoppingAt) (\s@Task' {} a -> s {stoppingAt = a} :: Task) Prelude.. Lens.mapping Core._Time

-- | The infrastructure on which your task is running. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS launch types>
-- in the /Amazon Elastic Container Service Developer Guide/.
task_launchType :: Lens.Lens' Task (Prelude.Maybe LaunchType)
task_launchType = Lens.lens (\Task' {launchType} -> launchType) (\s@Task' {} a -> s {launchType = a} :: Task)

-- | The ARN of the task definition that creates the task.
task_taskDefinitionArn :: Lens.Lens' Task (Prelude.Maybe Prelude.Text)
task_taskDefinitionArn = Lens.lens (\Task' {taskDefinitionArn} -> taskDefinitionArn) (\s@Task' {} a -> s {taskDefinitionArn = a} :: Task)

-- | The health status for the task, which is determined by the health of the
-- essential containers in the task. If all essential containers in the
-- task are reporting as @HEALTHY@, then the task status also reports as
-- @HEALTHY@. If any essential containers in the task are reporting as
-- @UNHEALTHY@ or @UNKNOWN@, then the task status also reports as
-- @UNHEALTHY@ or @UNKNOWN@, accordingly.
--
-- The Amazon ECS container agent does not monitor or report on Docker
-- health checks that are embedded in a container image (such as those
-- specified in a parent image or from the image\'s Dockerfile) and not
-- specified in the container definition. Health check parameters that are
-- specified in a container definition override any Docker health checks
-- that exist in the container image.
task_healthStatus :: Lens.Lens' Task (Prelude.Maybe HealthStatus)
task_healthStatus = Lens.lens (\Task' {healthStatus} -> healthStatus) (\s@Task' {} a -> s {healthStatus = a} :: Task)

-- | The Unix timestamp for when the task last went into @CONNECTED@ status.
task_connectivityAt :: Lens.Lens' Task (Prelude.Maybe Prelude.UTCTime)
task_connectivityAt = Lens.lens (\Task' {connectivityAt} -> connectivityAt) (\s@Task' {} a -> s {connectivityAt = a} :: Task) Prelude.. Lens.mapping Core._Time

-- | The number of CPU units used by the task as expressed in a task
-- definition. It can be expressed as an integer using CPU units, for
-- example @1024@. It can also be expressed as a string using vCPUs, for
-- example @1 vCPU@ or @1 vcpu@. String values are converted to an integer
-- indicating the CPU units when the task definition is registered.
--
-- If you are using the EC2 launch type, this field is optional. Supported
-- values are between @128@ CPU units (@0.125@ vCPUs) and @10240@ CPU units
-- (@10@ vCPUs).
--
-- If you are using the Fargate launch type, this field is required and you
-- must use one of the following values, which determines your range of
-- supported values for the @memory@ parameter:
--
-- -   256 (.25 vCPU) - Available @memory@ values: 512 (0.5 GB), 1024 (1
--     GB), 2048 (2 GB)
--
-- -   512 (.5 vCPU) - Available @memory@ values: 1024 (1 GB), 2048 (2 GB),
--     3072 (3 GB), 4096 (4 GB)
--
-- -   1024 (1 vCPU) - Available @memory@ values: 2048 (2 GB), 3072 (3 GB),
--     4096 (4 GB), 5120 (5 GB), 6144 (6 GB), 7168 (7 GB), 8192 (8 GB)
--
-- -   2048 (2 vCPU) - Available @memory@ values: Between 4096 (4 GB) and
--     16384 (16 GB) in increments of 1024 (1 GB)
--
-- -   4096 (4 vCPU) - Available @memory@ values: Between 8192 (8 GB) and
--     30720 (30 GB) in increments of 1024 (1 GB)
task_cpu :: Lens.Lens' Task (Prelude.Maybe Prelude.Text)
task_cpu = Lens.lens (\Task' {cpu} -> cpu) (\s@Task' {} a -> s {cpu = a} :: Task)

-- | Whether or not execute command functionality is enabled for this task.
-- If @true@, this enables execute command functionality on all containers
-- in the task.
task_enableExecuteCommand :: Lens.Lens' Task (Prelude.Maybe Prelude.Bool)
task_enableExecuteCommand = Lens.lens (\Task' {enableExecuteCommand} -> enableExecuteCommand) (\s@Task' {} a -> s {enableExecuteCommand = a} :: Task)

-- | The Unix timestamp for when the container image pull began.
task_pullStartedAt :: Lens.Lens' Task (Prelude.Maybe Prelude.UTCTime)
task_pullStartedAt = Lens.lens (\Task' {pullStartedAt} -> pullStartedAt) (\s@Task' {} a -> s {pullStartedAt = a} :: Task) Prelude.. Lens.mapping Core._Time

-- | The metadata that you apply to the task to help you categorize and
-- organize them. Each tag consists of a key and an optional value, both of
-- which you define.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per resource - 50
--
-- -   For each resource, each tag key must be unique, and each tag key can
--     have only one value.
--
-- -   Maximum key length - 128 Unicode characters in UTF-8
--
-- -   Maximum value length - 256 Unicode characters in UTF-8
--
-- -   If your tagging schema is used across multiple services and
--     resources, remember that other services may have restrictions on
--     allowed characters. Generally allowed characters are: letters,
--     numbers, and spaces representable in UTF-8, and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Tag keys and values are case-sensitive.
--
-- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
--     such as a prefix for either keys or values as it is reserved for
--     Amazon Web Services use. You cannot edit or delete tag keys or
--     values with this prefix. Tags with this prefix do not count against
--     your tags per resource limit.
task_tags :: Lens.Lens' Task (Prelude.Maybe [Tag])
task_tags = Lens.lens (\Task' {tags} -> tags) (\s@Task' {} a -> s {tags = a} :: Task) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Task where
  parseJSON =
    Core.withObject
      "Task"
      ( \x ->
          Task'
            Prelude.<$> (x Core..:? "stoppedAt")
            Prelude.<*> (x Core..:? "desiredStatus")
            Prelude.<*> (x Core..:? "overrides")
            Prelude.<*> ( x Core..:? "inferenceAccelerators"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "clusterArn")
            Prelude.<*> (x Core..:? "group")
            Prelude.<*> (x Core..:? "attachments" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "stopCode")
            Prelude.<*> (x Core..:? "platformVersion")
            Prelude.<*> (x Core..:? "taskArn")
            Prelude.<*> (x Core..:? "containerInstanceArn")
            Prelude.<*> (x Core..:? "executionStoppedAt")
            Prelude.<*> (x Core..:? "ephemeralStorage")
            Prelude.<*> (x Core..:? "lastStatus")
            Prelude.<*> (x Core..:? "memory")
            Prelude.<*> (x Core..:? "pullStoppedAt")
            Prelude.<*> (x Core..:? "containers" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "startedAt")
            Prelude.<*> (x Core..:? "availabilityZone")
            Prelude.<*> (x Core..:? "attributes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "version")
            Prelude.<*> (x Core..:? "capacityProviderName")
            Prelude.<*> (x Core..:? "startedBy")
            Prelude.<*> (x Core..:? "stoppedReason")
            Prelude.<*> (x Core..:? "connectivity")
            Prelude.<*> (x Core..:? "stoppingAt")
            Prelude.<*> (x Core..:? "launchType")
            Prelude.<*> (x Core..:? "taskDefinitionArn")
            Prelude.<*> (x Core..:? "healthStatus")
            Prelude.<*> (x Core..:? "connectivityAt")
            Prelude.<*> (x Core..:? "cpu")
            Prelude.<*> (x Core..:? "enableExecuteCommand")
            Prelude.<*> (x Core..:? "pullStartedAt")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Task

instance Prelude.NFData Task
