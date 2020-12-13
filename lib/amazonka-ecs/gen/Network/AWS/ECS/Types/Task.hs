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
    tStoppedAt,
    tDesiredStatus,
    tOverrides,
    tInferenceAccelerators,
    tClusterARN,
    tGroup,
    tAttachments,
    tCreatedAt,
    tStopCode,
    tPlatformVersion,
    tTaskARN,
    tContainerInstanceARN,
    tExecutionStoppedAt,
    tLastStatus,
    tMemory,
    tPullStoppedAt,
    tContainers,
    tStartedAt,
    tAvailabilityZone,
    tAttributes,
    tVersion,
    tCapacityProviderName,
    tStartedBy,
    tStoppedReason,
    tConnectivity,
    tStoppingAt,
    tLaunchType,
    tTaskDefinitionARN,
    tHealthStatus,
    tConnectivityAt,
    tCpu,
    tPullStartedAt,
    tTags,
  )
where

import Network.AWS.ECS.Types.Attachment
import Network.AWS.ECS.Types.Attribute
import Network.AWS.ECS.Types.Connectivity
import Network.AWS.ECS.Types.Container
import Network.AWS.ECS.Types.HealthStatus
import Network.AWS.ECS.Types.InferenceAccelerator
import Network.AWS.ECS.Types.LaunchType
import Network.AWS.ECS.Types.Tag
import Network.AWS.ECS.Types.TaskOverride
import Network.AWS.ECS.Types.TaskStopCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details on a task in a cluster.
--
-- /See:/ 'mkTask' smart constructor.
data Task = Task'
  { -- | The Unix timestamp for when the task was stopped (the task transitioned from the @RUNNING@ state to the @STOPPED@ state).
    stoppedAt :: Lude.Maybe Lude.Timestamp,
    -- | The desired status of the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-lifecycle.html Task Lifecycle> .
    desiredStatus :: Lude.Maybe Lude.Text,
    -- | One or more container overrides.
    overrides :: Lude.Maybe TaskOverride,
    -- | The Elastic Inference accelerator associated with the task.
    inferenceAccelerators :: Lude.Maybe [InferenceAccelerator],
    -- | The ARN of the cluster that hosts the task.
    clusterARN :: Lude.Maybe Lude.Text,
    -- | The name of the task group associated with the task.
    group :: Lude.Maybe Lude.Text,
    -- | The Elastic Network Adapter associated with the task if the task uses the @awsvpc@ network mode.
    attachments :: Lude.Maybe [Attachment],
    -- | The Unix timestamp for when the task was created (the task entered the @PENDING@ state).
    createdAt :: Lude.Maybe Lude.Timestamp,
    -- | The stop code indicating why a task was stopped. The @stoppedReason@ may contain additional details.
    stopCode :: Lude.Maybe TaskStopCode,
    -- | The platform version on which your task is running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
    platformVersion :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the task.
    taskARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the container instances that host the task.
    containerInstanceARN :: Lude.Maybe Lude.Text,
    -- | The Unix timestamp for when the task execution stopped.
    executionStoppedAt :: Lude.Maybe Lude.Timestamp,
    -- | The last known status of the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-lifecycle.html Task Lifecycle> .
    lastStatus :: Lude.Maybe Lude.Text,
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
    memory :: Lude.Maybe Lude.Text,
    -- | The Unix timestamp for when the container image pull completed.
    pullStoppedAt :: Lude.Maybe Lude.Timestamp,
    -- | The containers associated with the task.
    containers :: Lude.Maybe [Container],
    -- | The Unix timestamp for when the task started (the task transitioned from the @PENDING@ state to the @RUNNING@ state).
    startedAt :: Lude.Maybe Lude.Timestamp,
    -- | The availability zone of the task.
    availabilityZone :: Lude.Maybe Lude.Text,
    -- | The attributes of the task
    attributes :: Lude.Maybe [Attribute],
    -- | The version counter for the task. Every time a task experiences a change that triggers a CloudWatch event, the version counter is incremented. If you are replicating your Amazon ECS task state with CloudWatch Events, you can compare the version of a task reported by the Amazon ECS API actions with the version reported in CloudWatch Events for the task (inside the @detail@ object) to verify that the version in your event stream is current.
    version :: Lude.Maybe Lude.Integer,
    -- | The capacity provider associated with the task.
    capacityProviderName :: Lude.Maybe Lude.Text,
    -- | The tag specified when a task is started. If the task is started by an Amazon ECS service, then the @startedBy@ parameter contains the deployment ID of the service that starts it.
    startedBy :: Lude.Maybe Lude.Text,
    -- | The reason that the task was stopped.
    stoppedReason :: Lude.Maybe Lude.Text,
    -- | The connectivity status of a task.
    connectivity :: Lude.Maybe Connectivity,
    -- | The Unix timestamp for when the task stops (transitions from the @RUNNING@ state to @STOPPED@ ).
    stoppingAt :: Lude.Maybe Lude.Timestamp,
    -- | The launch type on which your task is running. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
    launchType :: Lude.Maybe LaunchType,
    -- | The ARN of the task definition that creates the task.
    taskDefinitionARN :: Lude.Maybe Lude.Text,
    -- | The health status for the task, which is determined by the health of the essential containers in the task. If all essential containers in the task are reporting as @HEALTHY@ , then the task status also reports as @HEALTHY@ . If any essential containers in the task are reporting as @UNHEALTHY@ or @UNKNOWN@ , then the task status also reports as @UNHEALTHY@ or @UNKNOWN@ , accordingly.
    healthStatus :: Lude.Maybe HealthStatus,
    -- | The Unix timestamp for when the task last went into @CONNECTED@ status.
    connectivityAt :: Lude.Maybe Lude.Timestamp,
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
    cpu :: Lude.Maybe Lude.Text,
    -- | The Unix timestamp for when the container image pull began.
    pullStartedAt :: Lude.Maybe Lude.Timestamp,
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
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Task' with the minimum fields required to make a request.
--
-- * 'stoppedAt' - The Unix timestamp for when the task was stopped (the task transitioned from the @RUNNING@ state to the @STOPPED@ state).
-- * 'desiredStatus' - The desired status of the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-lifecycle.html Task Lifecycle> .
-- * 'overrides' - One or more container overrides.
-- * 'inferenceAccelerators' - The Elastic Inference accelerator associated with the task.
-- * 'clusterARN' - The ARN of the cluster that hosts the task.
-- * 'group' - The name of the task group associated with the task.
-- * 'attachments' - The Elastic Network Adapter associated with the task if the task uses the @awsvpc@ network mode.
-- * 'createdAt' - The Unix timestamp for when the task was created (the task entered the @PENDING@ state).
-- * 'stopCode' - The stop code indicating why a task was stopped. The @stoppedReason@ may contain additional details.
-- * 'platformVersion' - The platform version on which your task is running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'taskARN' - The Amazon Resource Name (ARN) of the task.
-- * 'containerInstanceARN' - The ARN of the container instances that host the task.
-- * 'executionStoppedAt' - The Unix timestamp for when the task execution stopped.
-- * 'lastStatus' - The last known status of the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-lifecycle.html Task Lifecycle> .
-- * 'memory' - The amount of memory (in MiB) used by the task as expressed in a task definition. It can be expressed as an integer using MiB, for example @1024@ . It can also be expressed as a string using GB, for example @1GB@ or @1 GB@ . String values are converted to an integer indicating the MiB when the task definition is registered.
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
-- * 'pullStoppedAt' - The Unix timestamp for when the container image pull completed.
-- * 'containers' - The containers associated with the task.
-- * 'startedAt' - The Unix timestamp for when the task started (the task transitioned from the @PENDING@ state to the @RUNNING@ state).
-- * 'availabilityZone' - The availability zone of the task.
-- * 'attributes' - The attributes of the task
-- * 'version' - The version counter for the task. Every time a task experiences a change that triggers a CloudWatch event, the version counter is incremented. If you are replicating your Amazon ECS task state with CloudWatch Events, you can compare the version of a task reported by the Amazon ECS API actions with the version reported in CloudWatch Events for the task (inside the @detail@ object) to verify that the version in your event stream is current.
-- * 'capacityProviderName' - The capacity provider associated with the task.
-- * 'startedBy' - The tag specified when a task is started. If the task is started by an Amazon ECS service, then the @startedBy@ parameter contains the deployment ID of the service that starts it.
-- * 'stoppedReason' - The reason that the task was stopped.
-- * 'connectivity' - The connectivity status of a task.
-- * 'stoppingAt' - The Unix timestamp for when the task stops (transitions from the @RUNNING@ state to @STOPPED@ ).
-- * 'launchType' - The launch type on which your task is running. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'taskDefinitionARN' - The ARN of the task definition that creates the task.
-- * 'healthStatus' - The health status for the task, which is determined by the health of the essential containers in the task. If all essential containers in the task are reporting as @HEALTHY@ , then the task status also reports as @HEALTHY@ . If any essential containers in the task are reporting as @UNHEALTHY@ or @UNKNOWN@ , then the task status also reports as @UNHEALTHY@ or @UNKNOWN@ , accordingly.
-- * 'connectivityAt' - The Unix timestamp for when the task last went into @CONNECTED@ status.
-- * 'cpu' - The number of CPU units used by the task as expressed in a task definition. It can be expressed as an integer using CPU units, for example @1024@ . It can also be expressed as a string using vCPUs, for example @1 vCPU@ or @1 vcpu@ . String values are converted to an integer indicating the CPU units when the task definition is registered.
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
-- * 'pullStartedAt' - The Unix timestamp for when the container image pull began.
-- * 'tags' - The metadata that you apply to the task to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
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
mkTask ::
  Task
mkTask =
  Task'
    { stoppedAt = Lude.Nothing,
      desiredStatus = Lude.Nothing,
      overrides = Lude.Nothing,
      inferenceAccelerators = Lude.Nothing,
      clusterARN = Lude.Nothing,
      group = Lude.Nothing,
      attachments = Lude.Nothing,
      createdAt = Lude.Nothing,
      stopCode = Lude.Nothing,
      platformVersion = Lude.Nothing,
      taskARN = Lude.Nothing,
      containerInstanceARN = Lude.Nothing,
      executionStoppedAt = Lude.Nothing,
      lastStatus = Lude.Nothing,
      memory = Lude.Nothing,
      pullStoppedAt = Lude.Nothing,
      containers = Lude.Nothing,
      startedAt = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      attributes = Lude.Nothing,
      version = Lude.Nothing,
      capacityProviderName = Lude.Nothing,
      startedBy = Lude.Nothing,
      stoppedReason = Lude.Nothing,
      connectivity = Lude.Nothing,
      stoppingAt = Lude.Nothing,
      launchType = Lude.Nothing,
      taskDefinitionARN = Lude.Nothing,
      healthStatus = Lude.Nothing,
      connectivityAt = Lude.Nothing,
      cpu = Lude.Nothing,
      pullStartedAt = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The Unix timestamp for when the task was stopped (the task transitioned from the @RUNNING@ state to the @STOPPED@ state).
--
-- /Note:/ Consider using 'stoppedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStoppedAt :: Lens.Lens' Task (Lude.Maybe Lude.Timestamp)
tStoppedAt = Lens.lens (stoppedAt :: Task -> Lude.Maybe Lude.Timestamp) (\s a -> s {stoppedAt = a} :: Task)
{-# DEPRECATED tStoppedAt "Use generic-lens or generic-optics with 'stoppedAt' instead." #-}

-- | The desired status of the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-lifecycle.html Task Lifecycle> .
--
-- /Note:/ Consider using 'desiredStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tDesiredStatus :: Lens.Lens' Task (Lude.Maybe Lude.Text)
tDesiredStatus = Lens.lens (desiredStatus :: Task -> Lude.Maybe Lude.Text) (\s a -> s {desiredStatus = a} :: Task)
{-# DEPRECATED tDesiredStatus "Use generic-lens or generic-optics with 'desiredStatus' instead." #-}

-- | One or more container overrides.
--
-- /Note:/ Consider using 'overrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tOverrides :: Lens.Lens' Task (Lude.Maybe TaskOverride)
tOverrides = Lens.lens (overrides :: Task -> Lude.Maybe TaskOverride) (\s a -> s {overrides = a} :: Task)
{-# DEPRECATED tOverrides "Use generic-lens or generic-optics with 'overrides' instead." #-}

-- | The Elastic Inference accelerator associated with the task.
--
-- /Note:/ Consider using 'inferenceAccelerators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tInferenceAccelerators :: Lens.Lens' Task (Lude.Maybe [InferenceAccelerator])
tInferenceAccelerators = Lens.lens (inferenceAccelerators :: Task -> Lude.Maybe [InferenceAccelerator]) (\s a -> s {inferenceAccelerators = a} :: Task)
{-# DEPRECATED tInferenceAccelerators "Use generic-lens or generic-optics with 'inferenceAccelerators' instead." #-}

-- | The ARN of the cluster that hosts the task.
--
-- /Note:/ Consider using 'clusterARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tClusterARN :: Lens.Lens' Task (Lude.Maybe Lude.Text)
tClusterARN = Lens.lens (clusterARN :: Task -> Lude.Maybe Lude.Text) (\s a -> s {clusterARN = a} :: Task)
{-# DEPRECATED tClusterARN "Use generic-lens or generic-optics with 'clusterARN' instead." #-}

-- | The name of the task group associated with the task.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tGroup :: Lens.Lens' Task (Lude.Maybe Lude.Text)
tGroup = Lens.lens (group :: Task -> Lude.Maybe Lude.Text) (\s a -> s {group = a} :: Task)
{-# DEPRECATED tGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The Elastic Network Adapter associated with the task if the task uses the @awsvpc@ network mode.
--
-- /Note:/ Consider using 'attachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tAttachments :: Lens.Lens' Task (Lude.Maybe [Attachment])
tAttachments = Lens.lens (attachments :: Task -> Lude.Maybe [Attachment]) (\s a -> s {attachments = a} :: Task)
{-# DEPRECATED tAttachments "Use generic-lens or generic-optics with 'attachments' instead." #-}

-- | The Unix timestamp for when the task was created (the task entered the @PENDING@ state).
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCreatedAt :: Lens.Lens' Task (Lude.Maybe Lude.Timestamp)
tCreatedAt = Lens.lens (createdAt :: Task -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: Task)
{-# DEPRECATED tCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The stop code indicating why a task was stopped. The @stoppedReason@ may contain additional details.
--
-- /Note:/ Consider using 'stopCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStopCode :: Lens.Lens' Task (Lude.Maybe TaskStopCode)
tStopCode = Lens.lens (stopCode :: Task -> Lude.Maybe TaskStopCode) (\s a -> s {stopCode = a} :: Task)
{-# DEPRECATED tStopCode "Use generic-lens or generic-optics with 'stopCode' instead." #-}

-- | The platform version on which your task is running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'platformVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tPlatformVersion :: Lens.Lens' Task (Lude.Maybe Lude.Text)
tPlatformVersion = Lens.lens (platformVersion :: Task -> Lude.Maybe Lude.Text) (\s a -> s {platformVersion = a} :: Task)
{-# DEPRECATED tPlatformVersion "Use generic-lens or generic-optics with 'platformVersion' instead." #-}

-- | The Amazon Resource Name (ARN) of the task.
--
-- /Note:/ Consider using 'taskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTaskARN :: Lens.Lens' Task (Lude.Maybe Lude.Text)
tTaskARN = Lens.lens (taskARN :: Task -> Lude.Maybe Lude.Text) (\s a -> s {taskARN = a} :: Task)
{-# DEPRECATED tTaskARN "Use generic-lens or generic-optics with 'taskARN' instead." #-}

-- | The ARN of the container instances that host the task.
--
-- /Note:/ Consider using 'containerInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tContainerInstanceARN :: Lens.Lens' Task (Lude.Maybe Lude.Text)
tContainerInstanceARN = Lens.lens (containerInstanceARN :: Task -> Lude.Maybe Lude.Text) (\s a -> s {containerInstanceARN = a} :: Task)
{-# DEPRECATED tContainerInstanceARN "Use generic-lens or generic-optics with 'containerInstanceARN' instead." #-}

-- | The Unix timestamp for when the task execution stopped.
--
-- /Note:/ Consider using 'executionStoppedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tExecutionStoppedAt :: Lens.Lens' Task (Lude.Maybe Lude.Timestamp)
tExecutionStoppedAt = Lens.lens (executionStoppedAt :: Task -> Lude.Maybe Lude.Timestamp) (\s a -> s {executionStoppedAt = a} :: Task)
{-# DEPRECATED tExecutionStoppedAt "Use generic-lens or generic-optics with 'executionStoppedAt' instead." #-}

-- | The last known status of the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-lifecycle.html Task Lifecycle> .
--
-- /Note:/ Consider using 'lastStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tLastStatus :: Lens.Lens' Task (Lude.Maybe Lude.Text)
tLastStatus = Lens.lens (lastStatus :: Task -> Lude.Maybe Lude.Text) (\s a -> s {lastStatus = a} :: Task)
{-# DEPRECATED tLastStatus "Use generic-lens or generic-optics with 'lastStatus' instead." #-}

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
tMemory :: Lens.Lens' Task (Lude.Maybe Lude.Text)
tMemory = Lens.lens (memory :: Task -> Lude.Maybe Lude.Text) (\s a -> s {memory = a} :: Task)
{-# DEPRECATED tMemory "Use generic-lens or generic-optics with 'memory' instead." #-}

-- | The Unix timestamp for when the container image pull completed.
--
-- /Note:/ Consider using 'pullStoppedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tPullStoppedAt :: Lens.Lens' Task (Lude.Maybe Lude.Timestamp)
tPullStoppedAt = Lens.lens (pullStoppedAt :: Task -> Lude.Maybe Lude.Timestamp) (\s a -> s {pullStoppedAt = a} :: Task)
{-# DEPRECATED tPullStoppedAt "Use generic-lens or generic-optics with 'pullStoppedAt' instead." #-}

-- | The containers associated with the task.
--
-- /Note:/ Consider using 'containers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tContainers :: Lens.Lens' Task (Lude.Maybe [Container])
tContainers = Lens.lens (containers :: Task -> Lude.Maybe [Container]) (\s a -> s {containers = a} :: Task)
{-# DEPRECATED tContainers "Use generic-lens or generic-optics with 'containers' instead." #-}

-- | The Unix timestamp for when the task started (the task transitioned from the @PENDING@ state to the @RUNNING@ state).
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStartedAt :: Lens.Lens' Task (Lude.Maybe Lude.Timestamp)
tStartedAt = Lens.lens (startedAt :: Task -> Lude.Maybe Lude.Timestamp) (\s a -> s {startedAt = a} :: Task)
{-# DEPRECATED tStartedAt "Use generic-lens or generic-optics with 'startedAt' instead." #-}

-- | The availability zone of the task.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tAvailabilityZone :: Lens.Lens' Task (Lude.Maybe Lude.Text)
tAvailabilityZone = Lens.lens (availabilityZone :: Task -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: Task)
{-# DEPRECATED tAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The attributes of the task
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tAttributes :: Lens.Lens' Task (Lude.Maybe [Attribute])
tAttributes = Lens.lens (attributes :: Task -> Lude.Maybe [Attribute]) (\s a -> s {attributes = a} :: Task)
{-# DEPRECATED tAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The version counter for the task. Every time a task experiences a change that triggers a CloudWatch event, the version counter is incremented. If you are replicating your Amazon ECS task state with CloudWatch Events, you can compare the version of a task reported by the Amazon ECS API actions with the version reported in CloudWatch Events for the task (inside the @detail@ object) to verify that the version in your event stream is current.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tVersion :: Lens.Lens' Task (Lude.Maybe Lude.Integer)
tVersion = Lens.lens (version :: Task -> Lude.Maybe Lude.Integer) (\s a -> s {version = a} :: Task)
{-# DEPRECATED tVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The capacity provider associated with the task.
--
-- /Note:/ Consider using 'capacityProviderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCapacityProviderName :: Lens.Lens' Task (Lude.Maybe Lude.Text)
tCapacityProviderName = Lens.lens (capacityProviderName :: Task -> Lude.Maybe Lude.Text) (\s a -> s {capacityProviderName = a} :: Task)
{-# DEPRECATED tCapacityProviderName "Use generic-lens or generic-optics with 'capacityProviderName' instead." #-}

-- | The tag specified when a task is started. If the task is started by an Amazon ECS service, then the @startedBy@ parameter contains the deployment ID of the service that starts it.
--
-- /Note:/ Consider using 'startedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStartedBy :: Lens.Lens' Task (Lude.Maybe Lude.Text)
tStartedBy = Lens.lens (startedBy :: Task -> Lude.Maybe Lude.Text) (\s a -> s {startedBy = a} :: Task)
{-# DEPRECATED tStartedBy "Use generic-lens or generic-optics with 'startedBy' instead." #-}

-- | The reason that the task was stopped.
--
-- /Note:/ Consider using 'stoppedReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStoppedReason :: Lens.Lens' Task (Lude.Maybe Lude.Text)
tStoppedReason = Lens.lens (stoppedReason :: Task -> Lude.Maybe Lude.Text) (\s a -> s {stoppedReason = a} :: Task)
{-# DEPRECATED tStoppedReason "Use generic-lens or generic-optics with 'stoppedReason' instead." #-}

-- | The connectivity status of a task.
--
-- /Note:/ Consider using 'connectivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tConnectivity :: Lens.Lens' Task (Lude.Maybe Connectivity)
tConnectivity = Lens.lens (connectivity :: Task -> Lude.Maybe Connectivity) (\s a -> s {connectivity = a} :: Task)
{-# DEPRECATED tConnectivity "Use generic-lens or generic-optics with 'connectivity' instead." #-}

-- | The Unix timestamp for when the task stops (transitions from the @RUNNING@ state to @STOPPED@ ).
--
-- /Note:/ Consider using 'stoppingAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStoppingAt :: Lens.Lens' Task (Lude.Maybe Lude.Timestamp)
tStoppingAt = Lens.lens (stoppingAt :: Task -> Lude.Maybe Lude.Timestamp) (\s a -> s {stoppingAt = a} :: Task)
{-# DEPRECATED tStoppingAt "Use generic-lens or generic-optics with 'stoppingAt' instead." #-}

-- | The launch type on which your task is running. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'launchType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tLaunchType :: Lens.Lens' Task (Lude.Maybe LaunchType)
tLaunchType = Lens.lens (launchType :: Task -> Lude.Maybe LaunchType) (\s a -> s {launchType = a} :: Task)
{-# DEPRECATED tLaunchType "Use generic-lens or generic-optics with 'launchType' instead." #-}

-- | The ARN of the task definition that creates the task.
--
-- /Note:/ Consider using 'taskDefinitionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTaskDefinitionARN :: Lens.Lens' Task (Lude.Maybe Lude.Text)
tTaskDefinitionARN = Lens.lens (taskDefinitionARN :: Task -> Lude.Maybe Lude.Text) (\s a -> s {taskDefinitionARN = a} :: Task)
{-# DEPRECATED tTaskDefinitionARN "Use generic-lens or generic-optics with 'taskDefinitionARN' instead." #-}

-- | The health status for the task, which is determined by the health of the essential containers in the task. If all essential containers in the task are reporting as @HEALTHY@ , then the task status also reports as @HEALTHY@ . If any essential containers in the task are reporting as @UNHEALTHY@ or @UNKNOWN@ , then the task status also reports as @UNHEALTHY@ or @UNKNOWN@ , accordingly.
--
-- /Note:/ Consider using 'healthStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tHealthStatus :: Lens.Lens' Task (Lude.Maybe HealthStatus)
tHealthStatus = Lens.lens (healthStatus :: Task -> Lude.Maybe HealthStatus) (\s a -> s {healthStatus = a} :: Task)
{-# DEPRECATED tHealthStatus "Use generic-lens or generic-optics with 'healthStatus' instead." #-}

-- | The Unix timestamp for when the task last went into @CONNECTED@ status.
--
-- /Note:/ Consider using 'connectivityAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tConnectivityAt :: Lens.Lens' Task (Lude.Maybe Lude.Timestamp)
tConnectivityAt = Lens.lens (connectivityAt :: Task -> Lude.Maybe Lude.Timestamp) (\s a -> s {connectivityAt = a} :: Task)
{-# DEPRECATED tConnectivityAt "Use generic-lens or generic-optics with 'connectivityAt' instead." #-}

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
tCpu :: Lens.Lens' Task (Lude.Maybe Lude.Text)
tCpu = Lens.lens (cpu :: Task -> Lude.Maybe Lude.Text) (\s a -> s {cpu = a} :: Task)
{-# DEPRECATED tCpu "Use generic-lens or generic-optics with 'cpu' instead." #-}

-- | The Unix timestamp for when the container image pull began.
--
-- /Note:/ Consider using 'pullStartedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tPullStartedAt :: Lens.Lens' Task (Lude.Maybe Lude.Timestamp)
tPullStartedAt = Lens.lens (pullStartedAt :: Task -> Lude.Maybe Lude.Timestamp) (\s a -> s {pullStartedAt = a} :: Task)
{-# DEPRECATED tPullStartedAt "Use generic-lens or generic-optics with 'pullStartedAt' instead." #-}

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
tTags :: Lens.Lens' Task (Lude.Maybe [Tag])
tTags = Lens.lens (tags :: Task -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: Task)
{-# DEPRECATED tTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON Task where
  parseJSON =
    Lude.withObject
      "Task"
      ( \x ->
          Task'
            Lude.<$> (x Lude..:? "stoppedAt")
            Lude.<*> (x Lude..:? "desiredStatus")
            Lude.<*> (x Lude..:? "overrides")
            Lude.<*> (x Lude..:? "inferenceAccelerators" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "clusterArn")
            Lude.<*> (x Lude..:? "group")
            Lude.<*> (x Lude..:? "attachments" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "stopCode")
            Lude.<*> (x Lude..:? "platformVersion")
            Lude.<*> (x Lude..:? "taskArn")
            Lude.<*> (x Lude..:? "containerInstanceArn")
            Lude.<*> (x Lude..:? "executionStoppedAt")
            Lude.<*> (x Lude..:? "lastStatus")
            Lude.<*> (x Lude..:? "memory")
            Lude.<*> (x Lude..:? "pullStoppedAt")
            Lude.<*> (x Lude..:? "containers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "startedAt")
            Lude.<*> (x Lude..:? "availabilityZone")
            Lude.<*> (x Lude..:? "attributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "version")
            Lude.<*> (x Lude..:? "capacityProviderName")
            Lude.<*> (x Lude..:? "startedBy")
            Lude.<*> (x Lude..:? "stoppedReason")
            Lude.<*> (x Lude..:? "connectivity")
            Lude.<*> (x Lude..:? "stoppingAt")
            Lude.<*> (x Lude..:? "launchType")
            Lude.<*> (x Lude..:? "taskDefinitionArn")
            Lude.<*> (x Lude..:? "healthStatus")
            Lude.<*> (x Lude..:? "connectivityAt")
            Lude.<*> (x Lude..:? "cpu")
            Lude.<*> (x Lude..:? "pullStartedAt")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
