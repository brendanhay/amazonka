{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Task
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Task
  ( Task (..),

    -- * Smart constructor
    mkTask,

    -- * Lenses
    tAttachments,
    tAttributes,
    tAvailabilityZone,
    tCapacityProviderName,
    tClusterArn,
    tConnectivity,
    tConnectivityAt,
    tContainerInstanceArn,
    tContainers,
    tCpu,
    tCreatedAt,
    tDesiredStatus,
    tExecutionStoppedAt,
    tGroup,
    tHealthStatus,
    tInferenceAccelerators,
    tLastStatus,
    tLaunchType,
    tMemory,
    tOverrides,
    tPlatformVersion,
    tPullStartedAt,
    tPullStoppedAt,
    tStartedAt,
    tStartedBy,
    tStopCode,
    tStoppedAt,
    tStoppedReason,
    tStoppingAt,
    tTags,
    tTaskArn,
    tTaskDefinitionArn,
    tVersion,
  )
where

import qualified Network.AWS.ECS.Types.Attachment as Types
import qualified Network.AWS.ECS.Types.Attribute as Types
import qualified Network.AWS.ECS.Types.Connectivity as Types
import qualified Network.AWS.ECS.Types.Container as Types
import qualified Network.AWS.ECS.Types.HealthStatus as Types
import qualified Network.AWS.ECS.Types.InferenceAccelerator as Types
import qualified Network.AWS.ECS.Types.LaunchType as Types
import qualified Network.AWS.ECS.Types.String as Types
import qualified Network.AWS.ECS.Types.Tag as Types
import qualified Network.AWS.ECS.Types.TaskOverride as Types
import qualified Network.AWS.ECS.Types.TaskStopCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details on a task in a cluster.
--
-- /See:/ 'mkTask' smart constructor.
data Task = Task'
  { -- | The Elastic Network Adapter associated with the task if the task uses the @awsvpc@ network mode.
    attachments :: Core.Maybe [Types.Attachment],
    -- | The attributes of the task
    attributes :: Core.Maybe [Types.Attribute],
    -- | The availability zone of the task.
    availabilityZone :: Core.Maybe Types.String,
    -- | The capacity provider associated with the task.
    capacityProviderName :: Core.Maybe Types.String,
    -- | The ARN of the cluster that hosts the task.
    clusterArn :: Core.Maybe Types.String,
    -- | The connectivity status of a task.
    connectivity :: Core.Maybe Types.Connectivity,
    -- | The Unix timestamp for when the task last went into @CONNECTED@ status.
    connectivityAt :: Core.Maybe Core.NominalDiffTime,
    -- | The ARN of the container instances that host the task.
    containerInstanceArn :: Core.Maybe Types.String,
    -- | The containers associated with the task.
    containers :: Core.Maybe [Types.Container],
    -- | The number of CPU units used by the task as expressed in a task definition. It can be expressed as an integer using CPU units, for example @1024@ . It can also be expressed as a string using vCPUs, for example @1 vCPU@ or @1 vcpu@ . String values are converted to an integer indicating the CPU units when the task definition is registered.
    --
    -- If you are using the EC2 launch type, this field is optional. Supported values are between @128@ CPU units (@0.125@ vCPUs) and @10240@ CPU units (@10@ vCPUs).
    -- If you are using the Fargate launch type, this field is required and you must use one of the following values, which determines your range of supported values for the @memory@ parameter:
    --
    --     * 256 (.25 vCPU) - Available @memory@ values: 512 (0.5 GB), 1024 (1 GB), 2048 (2 GB)
    --
    --
    --     * 512 (.5 vCPU) - Available @memory@ values: 1024 (1 GB), 2048 (2 GB), 3072 (3 GB), 4096 (4 GB)
    --
    --
    --     * 1024 (1 vCPU) - Available @memory@ values: 2048 (2 GB), 3072 (3 GB), 4096 (4 GB), 5120 (5 GB), 6144 (6 GB), 7168 (7 GB), 8192 (8 GB)
    --
    --
    --     * 2048 (2 vCPU) - Available @memory@ values: Between 4096 (4 GB) and 16384 (16 GB) in increments of 1024 (1 GB)
    --
    --
    --     * 4096 (4 vCPU) - Available @memory@ values: Between 8192 (8 GB) and 30720 (30 GB) in increments of 1024 (1 GB)
    cpu :: Core.Maybe Types.String,
    -- | The Unix timestamp for when the task was created (the task entered the @PENDING@ state).
    createdAt :: Core.Maybe Core.NominalDiffTime,
    -- | The desired status of the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-lifecycle.html Task Lifecycle> .
    desiredStatus :: Core.Maybe Types.String,
    -- | The Unix timestamp for when the task execution stopped.
    executionStoppedAt :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the task group associated with the task.
    group :: Core.Maybe Types.String,
    -- | The health status for the task, which is determined by the health of the essential containers in the task. If all essential containers in the task are reporting as @HEALTHY@ , then the task status also reports as @HEALTHY@ . If any essential containers in the task are reporting as @UNHEALTHY@ or @UNKNOWN@ , then the task status also reports as @UNHEALTHY@ or @UNKNOWN@ , accordingly.
    healthStatus :: Core.Maybe Types.HealthStatus,
    -- | The Elastic Inference accelerator associated with the task.
    inferenceAccelerators :: Core.Maybe [Types.InferenceAccelerator],
    -- | The last known status of the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-lifecycle.html Task Lifecycle> .
    lastStatus :: Core.Maybe Types.String,
    -- | The launch type on which your task is running. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
    launchType :: Core.Maybe Types.LaunchType,
    -- | The amount of memory (in MiB) used by the task as expressed in a task definition. It can be expressed as an integer using MiB, for example @1024@ . It can also be expressed as a string using GB, for example @1GB@ or @1 GB@ . String values are converted to an integer indicating the MiB when the task definition is registered.
    --
    -- If you are using the EC2 launch type, this field is optional.
    -- If you are using the Fargate launch type, this field is required and you must use one of the following values, which determines your range of supported values for the @cpu@ parameter:
    --
    --     * 512 (0.5 GB), 1024 (1 GB), 2048 (2 GB) - Available @cpu@ values: 256 (.25 vCPU)
    --
    --
    --     * 1024 (1 GB), 2048 (2 GB), 3072 (3 GB), 4096 (4 GB) - Available @cpu@ values: 512 (.5 vCPU)
    --
    --
    --     * 2048 (2 GB), 3072 (3 GB), 4096 (4 GB), 5120 (5 GB), 6144 (6 GB), 7168 (7 GB), 8192 (8 GB) - Available @cpu@ values: 1024 (1 vCPU)
    --
    --
    --     * Between 4096 (4 GB) and 16384 (16 GB) in increments of 1024 (1 GB) - Available @cpu@ values: 2048 (2 vCPU)
    --
    --
    --     * Between 8192 (8 GB) and 30720 (30 GB) in increments of 1024 (1 GB) - Available @cpu@ values: 4096 (4 vCPU)
    memory :: Core.Maybe Types.String,
    -- | One or more container overrides.
    overrides :: Core.Maybe Types.TaskOverride,
    -- | The platform version on which your task is running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
    platformVersion :: Core.Maybe Types.String,
    -- | The Unix timestamp for when the container image pull began.
    pullStartedAt :: Core.Maybe Core.NominalDiffTime,
    -- | The Unix timestamp for when the container image pull completed.
    pullStoppedAt :: Core.Maybe Core.NominalDiffTime,
    -- | The Unix timestamp for when the task started (the task transitioned from the @PENDING@ state to the @RUNNING@ state).
    startedAt :: Core.Maybe Core.NominalDiffTime,
    -- | The tag specified when a task is started. If the task is started by an Amazon ECS service, then the @startedBy@ parameter contains the deployment ID of the service that starts it.
    startedBy :: Core.Maybe Types.String,
    -- | The stop code indicating why a task was stopped. The @stoppedReason@ may contain additional details.
    stopCode :: Core.Maybe Types.TaskStopCode,
    -- | The Unix timestamp for when the task was stopped (the task transitioned from the @RUNNING@ state to the @STOPPED@ state).
    stoppedAt :: Core.Maybe Core.NominalDiffTime,
    -- | The reason that the task was stopped.
    stoppedReason :: Core.Maybe Types.String,
    -- | The Unix timestamp for when the task stops (transitions from the @RUNNING@ state to @STOPPED@ ).
    stoppingAt :: Core.Maybe Core.NominalDiffTime,
    -- | The metadata that you apply to the task to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
    --
    -- The following basic restrictions apply to tags:
    --
    --     * Maximum number of tags per resource - 50
    --
    --
    --     * For each resource, each tag key must be unique, and each tag key can have only one value.
    --
    --
    --     * Maximum key length - 128 Unicode characters in UTF-8
    --
    --
    --     * Maximum value length - 256 Unicode characters in UTF-8
    --
    --
    --     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.
    --
    --
    --     * Tag keys and values are case-sensitive.
    --
    --
    --     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
    tags :: Core.Maybe [Types.Tag],
    -- | The Amazon Resource Name (ARN) of the task.
    taskArn :: Core.Maybe Types.String,
    -- | The ARN of the task definition that creates the task.
    taskDefinitionArn :: Core.Maybe Types.String,
    -- | The version counter for the task. Every time a task experiences a change that triggers a CloudWatch event, the version counter is incremented. If you are replicating your Amazon ECS task state with CloudWatch Events, you can compare the version of a task reported by the Amazon ECS API actions with the version reported in CloudWatch Events for the task (inside the @detail@ object) to verify that the version in your event stream is current.
    version :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Task' value with any optional fields omitted.
mkTask ::
  Task
mkTask =
  Task'
    { attachments = Core.Nothing,
      attributes = Core.Nothing,
      availabilityZone = Core.Nothing,
      capacityProviderName = Core.Nothing,
      clusterArn = Core.Nothing,
      connectivity = Core.Nothing,
      connectivityAt = Core.Nothing,
      containerInstanceArn = Core.Nothing,
      containers = Core.Nothing,
      cpu = Core.Nothing,
      createdAt = Core.Nothing,
      desiredStatus = Core.Nothing,
      executionStoppedAt = Core.Nothing,
      group = Core.Nothing,
      healthStatus = Core.Nothing,
      inferenceAccelerators = Core.Nothing,
      lastStatus = Core.Nothing,
      launchType = Core.Nothing,
      memory = Core.Nothing,
      overrides = Core.Nothing,
      platformVersion = Core.Nothing,
      pullStartedAt = Core.Nothing,
      pullStoppedAt = Core.Nothing,
      startedAt = Core.Nothing,
      startedBy = Core.Nothing,
      stopCode = Core.Nothing,
      stoppedAt = Core.Nothing,
      stoppedReason = Core.Nothing,
      stoppingAt = Core.Nothing,
      tags = Core.Nothing,
      taskArn = Core.Nothing,
      taskDefinitionArn = Core.Nothing,
      version = Core.Nothing
    }

-- | The Elastic Network Adapter associated with the task if the task uses the @awsvpc@ network mode.
--
-- /Note:/ Consider using 'attachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tAttachments :: Lens.Lens' Task (Core.Maybe [Types.Attachment])
tAttachments = Lens.field @"attachments"
{-# DEPRECATED tAttachments "Use generic-lens or generic-optics with 'attachments' instead." #-}

-- | The attributes of the task
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tAttributes :: Lens.Lens' Task (Core.Maybe [Types.Attribute])
tAttributes = Lens.field @"attributes"
{-# DEPRECATED tAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The availability zone of the task.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tAvailabilityZone :: Lens.Lens' Task (Core.Maybe Types.String)
tAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED tAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The capacity provider associated with the task.
--
-- /Note:/ Consider using 'capacityProviderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCapacityProviderName :: Lens.Lens' Task (Core.Maybe Types.String)
tCapacityProviderName = Lens.field @"capacityProviderName"
{-# DEPRECATED tCapacityProviderName "Use generic-lens or generic-optics with 'capacityProviderName' instead." #-}

-- | The ARN of the cluster that hosts the task.
--
-- /Note:/ Consider using 'clusterArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tClusterArn :: Lens.Lens' Task (Core.Maybe Types.String)
tClusterArn = Lens.field @"clusterArn"
{-# DEPRECATED tClusterArn "Use generic-lens or generic-optics with 'clusterArn' instead." #-}

-- | The connectivity status of a task.
--
-- /Note:/ Consider using 'connectivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tConnectivity :: Lens.Lens' Task (Core.Maybe Types.Connectivity)
tConnectivity = Lens.field @"connectivity"
{-# DEPRECATED tConnectivity "Use generic-lens or generic-optics with 'connectivity' instead." #-}

-- | The Unix timestamp for when the task last went into @CONNECTED@ status.
--
-- /Note:/ Consider using 'connectivityAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tConnectivityAt :: Lens.Lens' Task (Core.Maybe Core.NominalDiffTime)
tConnectivityAt = Lens.field @"connectivityAt"
{-# DEPRECATED tConnectivityAt "Use generic-lens or generic-optics with 'connectivityAt' instead." #-}

-- | The ARN of the container instances that host the task.
--
-- /Note:/ Consider using 'containerInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tContainerInstanceArn :: Lens.Lens' Task (Core.Maybe Types.String)
tContainerInstanceArn = Lens.field @"containerInstanceArn"
{-# DEPRECATED tContainerInstanceArn "Use generic-lens or generic-optics with 'containerInstanceArn' instead." #-}

-- | The containers associated with the task.
--
-- /Note:/ Consider using 'containers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tContainers :: Lens.Lens' Task (Core.Maybe [Types.Container])
tContainers = Lens.field @"containers"
{-# DEPRECATED tContainers "Use generic-lens or generic-optics with 'containers' instead." #-}

-- | The number of CPU units used by the task as expressed in a task definition. It can be expressed as an integer using CPU units, for example @1024@ . It can also be expressed as a string using vCPUs, for example @1 vCPU@ or @1 vcpu@ . String values are converted to an integer indicating the CPU units when the task definition is registered.
--
-- If you are using the EC2 launch type, this field is optional. Supported values are between @128@ CPU units (@0.125@ vCPUs) and @10240@ CPU units (@10@ vCPUs).
-- If you are using the Fargate launch type, this field is required and you must use one of the following values, which determines your range of supported values for the @memory@ parameter:
--
--     * 256 (.25 vCPU) - Available @memory@ values: 512 (0.5 GB), 1024 (1 GB), 2048 (2 GB)
--
--
--     * 512 (.5 vCPU) - Available @memory@ values: 1024 (1 GB), 2048 (2 GB), 3072 (3 GB), 4096 (4 GB)
--
--
--     * 1024 (1 vCPU) - Available @memory@ values: 2048 (2 GB), 3072 (3 GB), 4096 (4 GB), 5120 (5 GB), 6144 (6 GB), 7168 (7 GB), 8192 (8 GB)
--
--
--     * 2048 (2 vCPU) - Available @memory@ values: Between 4096 (4 GB) and 16384 (16 GB) in increments of 1024 (1 GB)
--
--
--     * 4096 (4 vCPU) - Available @memory@ values: Between 8192 (8 GB) and 30720 (30 GB) in increments of 1024 (1 GB)
--
--
--
-- /Note:/ Consider using 'cpu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCpu :: Lens.Lens' Task (Core.Maybe Types.String)
tCpu = Lens.field @"cpu"
{-# DEPRECATED tCpu "Use generic-lens or generic-optics with 'cpu' instead." #-}

-- | The Unix timestamp for when the task was created (the task entered the @PENDING@ state).
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCreatedAt :: Lens.Lens' Task (Core.Maybe Core.NominalDiffTime)
tCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED tCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The desired status of the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-lifecycle.html Task Lifecycle> .
--
-- /Note:/ Consider using 'desiredStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tDesiredStatus :: Lens.Lens' Task (Core.Maybe Types.String)
tDesiredStatus = Lens.field @"desiredStatus"
{-# DEPRECATED tDesiredStatus "Use generic-lens or generic-optics with 'desiredStatus' instead." #-}

-- | The Unix timestamp for when the task execution stopped.
--
-- /Note:/ Consider using 'executionStoppedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tExecutionStoppedAt :: Lens.Lens' Task (Core.Maybe Core.NominalDiffTime)
tExecutionStoppedAt = Lens.field @"executionStoppedAt"
{-# DEPRECATED tExecutionStoppedAt "Use generic-lens or generic-optics with 'executionStoppedAt' instead." #-}

-- | The name of the task group associated with the task.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tGroup :: Lens.Lens' Task (Core.Maybe Types.String)
tGroup = Lens.field @"group"
{-# DEPRECATED tGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The health status for the task, which is determined by the health of the essential containers in the task. If all essential containers in the task are reporting as @HEALTHY@ , then the task status also reports as @HEALTHY@ . If any essential containers in the task are reporting as @UNHEALTHY@ or @UNKNOWN@ , then the task status also reports as @UNHEALTHY@ or @UNKNOWN@ , accordingly.
--
-- /Note:/ Consider using 'healthStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tHealthStatus :: Lens.Lens' Task (Core.Maybe Types.HealthStatus)
tHealthStatus = Lens.field @"healthStatus"
{-# DEPRECATED tHealthStatus "Use generic-lens or generic-optics with 'healthStatus' instead." #-}

-- | The Elastic Inference accelerator associated with the task.
--
-- /Note:/ Consider using 'inferenceAccelerators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tInferenceAccelerators :: Lens.Lens' Task (Core.Maybe [Types.InferenceAccelerator])
tInferenceAccelerators = Lens.field @"inferenceAccelerators"
{-# DEPRECATED tInferenceAccelerators "Use generic-lens or generic-optics with 'inferenceAccelerators' instead." #-}

-- | The last known status of the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-lifecycle.html Task Lifecycle> .
--
-- /Note:/ Consider using 'lastStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tLastStatus :: Lens.Lens' Task (Core.Maybe Types.String)
tLastStatus = Lens.field @"lastStatus"
{-# DEPRECATED tLastStatus "Use generic-lens or generic-optics with 'lastStatus' instead." #-}

-- | The launch type on which your task is running. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'launchType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tLaunchType :: Lens.Lens' Task (Core.Maybe Types.LaunchType)
tLaunchType = Lens.field @"launchType"
{-# DEPRECATED tLaunchType "Use generic-lens or generic-optics with 'launchType' instead." #-}

-- | The amount of memory (in MiB) used by the task as expressed in a task definition. It can be expressed as an integer using MiB, for example @1024@ . It can also be expressed as a string using GB, for example @1GB@ or @1 GB@ . String values are converted to an integer indicating the MiB when the task definition is registered.
--
-- If you are using the EC2 launch type, this field is optional.
-- If you are using the Fargate launch type, this field is required and you must use one of the following values, which determines your range of supported values for the @cpu@ parameter:
--
--     * 512 (0.5 GB), 1024 (1 GB), 2048 (2 GB) - Available @cpu@ values: 256 (.25 vCPU)
--
--
--     * 1024 (1 GB), 2048 (2 GB), 3072 (3 GB), 4096 (4 GB) - Available @cpu@ values: 512 (.5 vCPU)
--
--
--     * 2048 (2 GB), 3072 (3 GB), 4096 (4 GB), 5120 (5 GB), 6144 (6 GB), 7168 (7 GB), 8192 (8 GB) - Available @cpu@ values: 1024 (1 vCPU)
--
--
--     * Between 4096 (4 GB) and 16384 (16 GB) in increments of 1024 (1 GB) - Available @cpu@ values: 2048 (2 vCPU)
--
--
--     * Between 8192 (8 GB) and 30720 (30 GB) in increments of 1024 (1 GB) - Available @cpu@ values: 4096 (4 vCPU)
--
--
--
-- /Note:/ Consider using 'memory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tMemory :: Lens.Lens' Task (Core.Maybe Types.String)
tMemory = Lens.field @"memory"
{-# DEPRECATED tMemory "Use generic-lens or generic-optics with 'memory' instead." #-}

-- | One or more container overrides.
--
-- /Note:/ Consider using 'overrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tOverrides :: Lens.Lens' Task (Core.Maybe Types.TaskOverride)
tOverrides = Lens.field @"overrides"
{-# DEPRECATED tOverrides "Use generic-lens or generic-optics with 'overrides' instead." #-}

-- | The platform version on which your task is running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'platformVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tPlatformVersion :: Lens.Lens' Task (Core.Maybe Types.String)
tPlatformVersion = Lens.field @"platformVersion"
{-# DEPRECATED tPlatformVersion "Use generic-lens or generic-optics with 'platformVersion' instead." #-}

-- | The Unix timestamp for when the container image pull began.
--
-- /Note:/ Consider using 'pullStartedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tPullStartedAt :: Lens.Lens' Task (Core.Maybe Core.NominalDiffTime)
tPullStartedAt = Lens.field @"pullStartedAt"
{-# DEPRECATED tPullStartedAt "Use generic-lens or generic-optics with 'pullStartedAt' instead." #-}

-- | The Unix timestamp for when the container image pull completed.
--
-- /Note:/ Consider using 'pullStoppedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tPullStoppedAt :: Lens.Lens' Task (Core.Maybe Core.NominalDiffTime)
tPullStoppedAt = Lens.field @"pullStoppedAt"
{-# DEPRECATED tPullStoppedAt "Use generic-lens or generic-optics with 'pullStoppedAt' instead." #-}

-- | The Unix timestamp for when the task started (the task transitioned from the @PENDING@ state to the @RUNNING@ state).
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStartedAt :: Lens.Lens' Task (Core.Maybe Core.NominalDiffTime)
tStartedAt = Lens.field @"startedAt"
{-# DEPRECATED tStartedAt "Use generic-lens or generic-optics with 'startedAt' instead." #-}

-- | The tag specified when a task is started. If the task is started by an Amazon ECS service, then the @startedBy@ parameter contains the deployment ID of the service that starts it.
--
-- /Note:/ Consider using 'startedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStartedBy :: Lens.Lens' Task (Core.Maybe Types.String)
tStartedBy = Lens.field @"startedBy"
{-# DEPRECATED tStartedBy "Use generic-lens or generic-optics with 'startedBy' instead." #-}

-- | The stop code indicating why a task was stopped. The @stoppedReason@ may contain additional details.
--
-- /Note:/ Consider using 'stopCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStopCode :: Lens.Lens' Task (Core.Maybe Types.TaskStopCode)
tStopCode = Lens.field @"stopCode"
{-# DEPRECATED tStopCode "Use generic-lens or generic-optics with 'stopCode' instead." #-}

-- | The Unix timestamp for when the task was stopped (the task transitioned from the @RUNNING@ state to the @STOPPED@ state).
--
-- /Note:/ Consider using 'stoppedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStoppedAt :: Lens.Lens' Task (Core.Maybe Core.NominalDiffTime)
tStoppedAt = Lens.field @"stoppedAt"
{-# DEPRECATED tStoppedAt "Use generic-lens or generic-optics with 'stoppedAt' instead." #-}

-- | The reason that the task was stopped.
--
-- /Note:/ Consider using 'stoppedReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStoppedReason :: Lens.Lens' Task (Core.Maybe Types.String)
tStoppedReason = Lens.field @"stoppedReason"
{-# DEPRECATED tStoppedReason "Use generic-lens or generic-optics with 'stoppedReason' instead." #-}

-- | The Unix timestamp for when the task stops (transitions from the @RUNNING@ state to @STOPPED@ ).
--
-- /Note:/ Consider using 'stoppingAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStoppingAt :: Lens.Lens' Task (Core.Maybe Core.NominalDiffTime)
tStoppingAt = Lens.field @"stoppingAt"
{-# DEPRECATED tStoppingAt "Use generic-lens or generic-optics with 'stoppingAt' instead." #-}

-- | The metadata that you apply to the task to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
--
-- The following basic restrictions apply to tags:
--
--     * Maximum number of tags per resource - 50
--
--
--     * For each resource, each tag key must be unique, and each tag key can have only one value.
--
--
--     * Maximum key length - 128 Unicode characters in UTF-8
--
--
--     * Maximum value length - 256 Unicode characters in UTF-8
--
--
--     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.
--
--
--     * Tag keys and values are case-sensitive.
--
--
--     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
--
--
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTags :: Lens.Lens' Task (Core.Maybe [Types.Tag])
tTags = Lens.field @"tags"
{-# DEPRECATED tTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The Amazon Resource Name (ARN) of the task.
--
-- /Note:/ Consider using 'taskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTaskArn :: Lens.Lens' Task (Core.Maybe Types.String)
tTaskArn = Lens.field @"taskArn"
{-# DEPRECATED tTaskArn "Use generic-lens or generic-optics with 'taskArn' instead." #-}

-- | The ARN of the task definition that creates the task.
--
-- /Note:/ Consider using 'taskDefinitionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTaskDefinitionArn :: Lens.Lens' Task (Core.Maybe Types.String)
tTaskDefinitionArn = Lens.field @"taskDefinitionArn"
{-# DEPRECATED tTaskDefinitionArn "Use generic-lens or generic-optics with 'taskDefinitionArn' instead." #-}

-- | The version counter for the task. Every time a task experiences a change that triggers a CloudWatch event, the version counter is incremented. If you are replicating your Amazon ECS task state with CloudWatch Events, you can compare the version of a task reported by the Amazon ECS API actions with the version reported in CloudWatch Events for the task (inside the @detail@ object) to verify that the version in your event stream is current.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tVersion :: Lens.Lens' Task (Core.Maybe Core.Integer)
tVersion = Lens.field @"version"
{-# DEPRECATED tVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON Task where
  parseJSON =
    Core.withObject "Task" Core.$
      \x ->
        Task'
          Core.<$> (x Core..:? "attachments")
          Core.<*> (x Core..:? "attributes")
          Core.<*> (x Core..:? "availabilityZone")
          Core.<*> (x Core..:? "capacityProviderName")
          Core.<*> (x Core..:? "clusterArn")
          Core.<*> (x Core..:? "connectivity")
          Core.<*> (x Core..:? "connectivityAt")
          Core.<*> (x Core..:? "containerInstanceArn")
          Core.<*> (x Core..:? "containers")
          Core.<*> (x Core..:? "cpu")
          Core.<*> (x Core..:? "createdAt")
          Core.<*> (x Core..:? "desiredStatus")
          Core.<*> (x Core..:? "executionStoppedAt")
          Core.<*> (x Core..:? "group")
          Core.<*> (x Core..:? "healthStatus")
          Core.<*> (x Core..:? "inferenceAccelerators")
          Core.<*> (x Core..:? "lastStatus")
          Core.<*> (x Core..:? "launchType")
          Core.<*> (x Core..:? "memory")
          Core.<*> (x Core..:? "overrides")
          Core.<*> (x Core..:? "platformVersion")
          Core.<*> (x Core..:? "pullStartedAt")
          Core.<*> (x Core..:? "pullStoppedAt")
          Core.<*> (x Core..:? "startedAt")
          Core.<*> (x Core..:? "startedBy")
          Core.<*> (x Core..:? "stopCode")
          Core.<*> (x Core..:? "stoppedAt")
          Core.<*> (x Core..:? "stoppedReason")
          Core.<*> (x Core..:? "stoppingAt")
          Core.<*> (x Core..:? "tags")
          Core.<*> (x Core..:? "taskArn")
          Core.<*> (x Core..:? "taskDefinitionArn")
          Core.<*> (x Core..:? "version")
