{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.RegisterTaskDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a new task definition from the supplied @family@ and @containerDefinitions@ . Optionally, you can add data volumes to your containers with the @volumes@ parameter. For more information about task definition parameters and defaults, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Amazon ECS Task Definitions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- You can specify an IAM role for your task with the @taskRoleArn@ parameter. When you specify an IAM role for a task, its containers can then use the latest versions of the AWS CLI or SDKs to make API requests to the AWS services that are specified in the IAM policy associated with the role. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks> in the /Amazon Elastic Container Service Developer Guide/ .
-- You can specify a Docker networking mode for the containers in your task definition with the @networkMode@ parameter. The available network modes correspond to those described in <https://docs.docker.com/engine/reference/run/#/network-settings Network settings> in the Docker run reference. If you specify the @awsvpc@ network mode, the task is allocated an elastic network interface, and you must specify a 'NetworkConfiguration' when you create a service or run a task with the task definition. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking> in the /Amazon Elastic Container Service Developer Guide/ .
module Network.AWS.ECS.RegisterTaskDefinition
  ( -- * Creating a request
    RegisterTaskDefinition (..),
    mkRegisterTaskDefinition,

    -- ** Request lenses
    rtdInferenceAccelerators,
    rtdExecutionRoleARN,
    rtdRequiresCompatibilities,
    rtdPidMode,
    rtdIpcMode,
    rtdMemory,
    rtdProxyConfiguration,
    rtdTaskRoleARN,
    rtdPlacementConstraints,
    rtdNetworkMode,
    rtdVolumes,
    rtdCpu,
    rtdTags,
    rtdFamily,
    rtdContainerDefinitions,

    -- * Destructuring the response
    RegisterTaskDefinitionResponse (..),
    mkRegisterTaskDefinitionResponse,

    -- ** Response lenses
    rtdrsTaskDefinition,
    rtdrsTags,
    rtdrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRegisterTaskDefinition' smart constructor.
data RegisterTaskDefinition = RegisterTaskDefinition'
  { inferenceAccelerators ::
      Lude.Maybe [InferenceAccelerator],
    executionRoleARN :: Lude.Maybe Lude.Text,
    requiresCompatibilities ::
      Lude.Maybe [Compatibility],
    pidMode :: Lude.Maybe PidMode,
    ipcMode :: Lude.Maybe IPcMode,
    memory :: Lude.Maybe Lude.Text,
    proxyConfiguration ::
      Lude.Maybe ProxyConfiguration,
    taskRoleARN :: Lude.Maybe Lude.Text,
    placementConstraints ::
      Lude.Maybe
        [TaskDefinitionPlacementConstraint],
    networkMode :: Lude.Maybe NetworkMode,
    volumes :: Lude.Maybe [Volume],
    cpu :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    family :: Lude.Text,
    containerDefinitions :: [ContainerDefinition]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterTaskDefinition' with the minimum fields required to make a request.
--
-- * 'containerDefinitions' - A list of container definitions in JSON format that describe the different containers that make up your task.
-- * 'cpu' - The number of CPU units used by the task. It can be expressed as an integer using CPU units, for example @1024@ , or as a string using vCPUs, for example @1 vCPU@ or @1 vcpu@ , in a task definition. String values are converted to an integer indicating the CPU units when the task definition is registered.
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
-- * 'executionRoleARN' - The Amazon Resource Name (ARN) of the task execution role that grants the Amazon ECS container agent permission to make AWS API calls on your behalf. The task execution IAM role is required depending on the requirements of your task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_execution_IAM_role.html Amazon ECS task execution IAM role> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'family' - You must specify a @family@ for a task definition, which allows you to track multiple versions of the same task definition. The @family@ is used as a name for your task definition. Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed.
-- * 'inferenceAccelerators' - The Elastic Inference accelerators to use for the containers in the task.
-- * 'ipcMode' - The IPC resource namespace to use for the containers in the task. The valid values are @host@ , @task@ , or @none@ . If @host@ is specified, then all containers within the tasks that specified the @host@ IPC mode on the same container instance share the same IPC resources with the host Amazon EC2 instance. If @task@ is specified, all containers within the specified task share the same IPC resources. If @none@ is specified, then IPC resources within the containers of a task are private and not shared with other containers in a task or on the container instance. If no value is specified, then the IPC resource namespace sharing depends on the Docker daemon setting on the container instance. For more information, see <https://docs.docker.com/engine/reference/run/#ipc-settings---ipc IPC settings> in the /Docker run reference/ .
--
-- If the @host@ IPC mode is used, be aware that there is a heightened risk of undesired IPC namespace expose. For more information, see <https://docs.docker.com/engine/security/security/ Docker security> .
-- If you are setting namespaced kernel parameters using @systemControls@ for the containers in the task, the following will apply to your IPC resource namespace. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_definition_parameters.html System Controls> in the /Amazon Elastic Container Service Developer Guide/ .
--
--     * For tasks that use the @host@ IPC mode, IPC namespace related @systemControls@ are not supported.
--
--
--     * For tasks that use the @task@ IPC mode, IPC namespace related @systemControls@ will apply to all containers within a task.
--
--
-- * 'memory' - The amount of memory (in MiB) used by the task. It can be expressed as an integer using MiB, for example @1024@ , or as a string using GB, for example @1GB@ or @1 GB@ , in a task definition. String values are converted to an integer indicating the MiB when the task definition is registered.
--
-- If using the EC2 launch type, this field is optional.
-- If using the Fargate launch type, this field is required and you must use one of the following values, which determines your range of supported values for the @cpu@ parameter:
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
-- * 'networkMode' - The Docker networking mode to use for the containers in the task. The valid values are @none@ , @bridge@ , @awsvpc@ , and @host@ . If no network mode is specified, the default is @bridge@ .
--
-- For Amazon ECS tasks on Fargate, the @awsvpc@ network mode is required. For Amazon ECS tasks on Amazon EC2 instances, any network mode can be used. If the network mode is set to @none@ , you cannot specify port mappings in your container definitions, and the tasks containers do not have external connectivity. The @host@ and @awsvpc@ network modes offer the highest networking performance for containers because they use the EC2 network stack instead of the virtualized network stack provided by the @bridge@ mode.
-- With the @host@ and @awsvpc@ network modes, exposed container ports are mapped directly to the corresponding host port (for the @host@ network mode) or the attached elastic network interface port (for the @awsvpc@ network mode), so you cannot take advantage of dynamic host port mappings.
-- /Important:/ When using the @host@ network mode, you should not run containers using the root user (UID 0). It is considered best practice to use a non-root user.
-- If the network mode is @awsvpc@ , the task is allocated an elastic network interface, and you must specify a 'NetworkConfiguration' value when you create a service or run a task with the task definition. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking> in the /Amazon Elastic Container Service Developer Guide/ .
-- If the network mode is @host@ , you cannot run multiple instantiations of the same task on a single container instance when port mappings are used.
-- Docker for Windows uses different network modes than Docker for Linux. When you register a task definition with Windows containers, you must not specify a network mode. If you use the console to register a task definition with Windows containers, you must choose the @<default>@ network mode object.
-- For more information, see <https://docs.docker.com/engine/reference/run/#network-settings Network settings> in the /Docker run reference/ .
-- * 'pidMode' - The process namespace to use for the containers in the task. The valid values are @host@ or @task@ . If @host@ is specified, then all containers within the tasks that specified the @host@ PID mode on the same container instance share the same process namespace with the host Amazon EC2 instance. If @task@ is specified, all containers within the specified task share the same process namespace. If no value is specified, the default is a private namespace. For more information, see <https://docs.docker.com/engine/reference/run/#pid-settings---pid PID settings> in the /Docker run reference/ .
--
-- If the @host@ PID mode is used, be aware that there is a heightened risk of undesired process namespace expose. For more information, see <https://docs.docker.com/engine/security/security/ Docker security> .
-- * 'placementConstraints' - An array of placement constraint objects to use for the task. You can specify a maximum of 10 constraints per task (this limit includes constraints in the task definition and those specified at runtime).
-- * 'proxyConfiguration' - Undocumented field.
-- * 'requiresCompatibilities' - The task launch type that Amazon ECS should validate the task definition against. This ensures that the task definition parameters are compatible with the specified launch type. If no value is specified, it defaults to @EC2@ .
-- * 'tags' - The metadata that you apply to the task definition to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
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
-- * 'taskRoleARN' - The short name or full Amazon Resource Name (ARN) of the IAM role that containers in this task can assume. All containers in this task are granted the permissions that are specified in this role. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'volumes' - A list of volume definitions in JSON format that containers in your task may use.
mkRegisterTaskDefinition ::
  -- | 'family'
  Lude.Text ->
  RegisterTaskDefinition
mkRegisterTaskDefinition pFamily_ =
  RegisterTaskDefinition'
    { inferenceAccelerators = Lude.Nothing,
      executionRoleARN = Lude.Nothing,
      requiresCompatibilities = Lude.Nothing,
      pidMode = Lude.Nothing,
      ipcMode = Lude.Nothing,
      memory = Lude.Nothing,
      proxyConfiguration = Lude.Nothing,
      taskRoleARN = Lude.Nothing,
      placementConstraints = Lude.Nothing,
      networkMode = Lude.Nothing,
      volumes = Lude.Nothing,
      cpu = Lude.Nothing,
      tags = Lude.Nothing,
      family = pFamily_,
      containerDefinitions = Lude.mempty
    }

-- | The Elastic Inference accelerators to use for the containers in the task.
--
-- /Note:/ Consider using 'inferenceAccelerators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdInferenceAccelerators :: Lens.Lens' RegisterTaskDefinition (Lude.Maybe [InferenceAccelerator])
rtdInferenceAccelerators = Lens.lens (inferenceAccelerators :: RegisterTaskDefinition -> Lude.Maybe [InferenceAccelerator]) (\s a -> s {inferenceAccelerators = a} :: RegisterTaskDefinition)
{-# DEPRECATED rtdInferenceAccelerators "Use generic-lens or generic-optics with 'inferenceAccelerators' instead." #-}

-- | The Amazon Resource Name (ARN) of the task execution role that grants the Amazon ECS container agent permission to make AWS API calls on your behalf. The task execution IAM role is required depending on the requirements of your task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_execution_IAM_role.html Amazon ECS task execution IAM role> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'executionRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdExecutionRoleARN :: Lens.Lens' RegisterTaskDefinition (Lude.Maybe Lude.Text)
rtdExecutionRoleARN = Lens.lens (executionRoleARN :: RegisterTaskDefinition -> Lude.Maybe Lude.Text) (\s a -> s {executionRoleARN = a} :: RegisterTaskDefinition)
{-# DEPRECATED rtdExecutionRoleARN "Use generic-lens or generic-optics with 'executionRoleARN' instead." #-}

-- | The task launch type that Amazon ECS should validate the task definition against. This ensures that the task definition parameters are compatible with the specified launch type. If no value is specified, it defaults to @EC2@ .
--
-- /Note:/ Consider using 'requiresCompatibilities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdRequiresCompatibilities :: Lens.Lens' RegisterTaskDefinition (Lude.Maybe [Compatibility])
rtdRequiresCompatibilities = Lens.lens (requiresCompatibilities :: RegisterTaskDefinition -> Lude.Maybe [Compatibility]) (\s a -> s {requiresCompatibilities = a} :: RegisterTaskDefinition)
{-# DEPRECATED rtdRequiresCompatibilities "Use generic-lens or generic-optics with 'requiresCompatibilities' instead." #-}

-- | The process namespace to use for the containers in the task. The valid values are @host@ or @task@ . If @host@ is specified, then all containers within the tasks that specified the @host@ PID mode on the same container instance share the same process namespace with the host Amazon EC2 instance. If @task@ is specified, all containers within the specified task share the same process namespace. If no value is specified, the default is a private namespace. For more information, see <https://docs.docker.com/engine/reference/run/#pid-settings---pid PID settings> in the /Docker run reference/ .
--
-- If the @host@ PID mode is used, be aware that there is a heightened risk of undesired process namespace expose. For more information, see <https://docs.docker.com/engine/security/security/ Docker security> .
--
-- /Note:/ Consider using 'pidMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdPidMode :: Lens.Lens' RegisterTaskDefinition (Lude.Maybe PidMode)
rtdPidMode = Lens.lens (pidMode :: RegisterTaskDefinition -> Lude.Maybe PidMode) (\s a -> s {pidMode = a} :: RegisterTaskDefinition)
{-# DEPRECATED rtdPidMode "Use generic-lens or generic-optics with 'pidMode' instead." #-}

-- | The IPC resource namespace to use for the containers in the task. The valid values are @host@ , @task@ , or @none@ . If @host@ is specified, then all containers within the tasks that specified the @host@ IPC mode on the same container instance share the same IPC resources with the host Amazon EC2 instance. If @task@ is specified, all containers within the specified task share the same IPC resources. If @none@ is specified, then IPC resources within the containers of a task are private and not shared with other containers in a task or on the container instance. If no value is specified, then the IPC resource namespace sharing depends on the Docker daemon setting on the container instance. For more information, see <https://docs.docker.com/engine/reference/run/#ipc-settings---ipc IPC settings> in the /Docker run reference/ .
--
-- If the @host@ IPC mode is used, be aware that there is a heightened risk of undesired IPC namespace expose. For more information, see <https://docs.docker.com/engine/security/security/ Docker security> .
-- If you are setting namespaced kernel parameters using @systemControls@ for the containers in the task, the following will apply to your IPC resource namespace. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_definition_parameters.html System Controls> in the /Amazon Elastic Container Service Developer Guide/ .
--
--     * For tasks that use the @host@ IPC mode, IPC namespace related @systemControls@ are not supported.
--
--
--     * For tasks that use the @task@ IPC mode, IPC namespace related @systemControls@ will apply to all containers within a task.
--
--
--
-- /Note:/ Consider using 'ipcMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdIpcMode :: Lens.Lens' RegisterTaskDefinition (Lude.Maybe IPcMode)
rtdIpcMode = Lens.lens (ipcMode :: RegisterTaskDefinition -> Lude.Maybe IPcMode) (\s a -> s {ipcMode = a} :: RegisterTaskDefinition)
{-# DEPRECATED rtdIpcMode "Use generic-lens or generic-optics with 'ipcMode' instead." #-}

-- | The amount of memory (in MiB) used by the task. It can be expressed as an integer using MiB, for example @1024@ , or as a string using GB, for example @1GB@ or @1 GB@ , in a task definition. String values are converted to an integer indicating the MiB when the task definition is registered.
--
-- If using the EC2 launch type, this field is optional.
-- If using the Fargate launch type, this field is required and you must use one of the following values, which determines your range of supported values for the @cpu@ parameter:
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
rtdMemory :: Lens.Lens' RegisterTaskDefinition (Lude.Maybe Lude.Text)
rtdMemory = Lens.lens (memory :: RegisterTaskDefinition -> Lude.Maybe Lude.Text) (\s a -> s {memory = a} :: RegisterTaskDefinition)
{-# DEPRECATED rtdMemory "Use generic-lens or generic-optics with 'memory' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'proxyConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdProxyConfiguration :: Lens.Lens' RegisterTaskDefinition (Lude.Maybe ProxyConfiguration)
rtdProxyConfiguration = Lens.lens (proxyConfiguration :: RegisterTaskDefinition -> Lude.Maybe ProxyConfiguration) (\s a -> s {proxyConfiguration = a} :: RegisterTaskDefinition)
{-# DEPRECATED rtdProxyConfiguration "Use generic-lens or generic-optics with 'proxyConfiguration' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the IAM role that containers in this task can assume. All containers in this task are granted the permissions that are specified in this role. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'taskRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdTaskRoleARN :: Lens.Lens' RegisterTaskDefinition (Lude.Maybe Lude.Text)
rtdTaskRoleARN = Lens.lens (taskRoleARN :: RegisterTaskDefinition -> Lude.Maybe Lude.Text) (\s a -> s {taskRoleARN = a} :: RegisterTaskDefinition)
{-# DEPRECATED rtdTaskRoleARN "Use generic-lens or generic-optics with 'taskRoleARN' instead." #-}

-- | An array of placement constraint objects to use for the task. You can specify a maximum of 10 constraints per task (this limit includes constraints in the task definition and those specified at runtime).
--
-- /Note:/ Consider using 'placementConstraints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdPlacementConstraints :: Lens.Lens' RegisterTaskDefinition (Lude.Maybe [TaskDefinitionPlacementConstraint])
rtdPlacementConstraints = Lens.lens (placementConstraints :: RegisterTaskDefinition -> Lude.Maybe [TaskDefinitionPlacementConstraint]) (\s a -> s {placementConstraints = a} :: RegisterTaskDefinition)
{-# DEPRECATED rtdPlacementConstraints "Use generic-lens or generic-optics with 'placementConstraints' instead." #-}

-- | The Docker networking mode to use for the containers in the task. The valid values are @none@ , @bridge@ , @awsvpc@ , and @host@ . If no network mode is specified, the default is @bridge@ .
--
-- For Amazon ECS tasks on Fargate, the @awsvpc@ network mode is required. For Amazon ECS tasks on Amazon EC2 instances, any network mode can be used. If the network mode is set to @none@ , you cannot specify port mappings in your container definitions, and the tasks containers do not have external connectivity. The @host@ and @awsvpc@ network modes offer the highest networking performance for containers because they use the EC2 network stack instead of the virtualized network stack provided by the @bridge@ mode.
-- With the @host@ and @awsvpc@ network modes, exposed container ports are mapped directly to the corresponding host port (for the @host@ network mode) or the attached elastic network interface port (for the @awsvpc@ network mode), so you cannot take advantage of dynamic host port mappings.
-- /Important:/ When using the @host@ network mode, you should not run containers using the root user (UID 0). It is considered best practice to use a non-root user.
-- If the network mode is @awsvpc@ , the task is allocated an elastic network interface, and you must specify a 'NetworkConfiguration' value when you create a service or run a task with the task definition. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking> in the /Amazon Elastic Container Service Developer Guide/ .
-- If the network mode is @host@ , you cannot run multiple instantiations of the same task on a single container instance when port mappings are used.
-- Docker for Windows uses different network modes than Docker for Linux. When you register a task definition with Windows containers, you must not specify a network mode. If you use the console to register a task definition with Windows containers, you must choose the @<default>@ network mode object.
-- For more information, see <https://docs.docker.com/engine/reference/run/#network-settings Network settings> in the /Docker run reference/ .
--
-- /Note:/ Consider using 'networkMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdNetworkMode :: Lens.Lens' RegisterTaskDefinition (Lude.Maybe NetworkMode)
rtdNetworkMode = Lens.lens (networkMode :: RegisterTaskDefinition -> Lude.Maybe NetworkMode) (\s a -> s {networkMode = a} :: RegisterTaskDefinition)
{-# DEPRECATED rtdNetworkMode "Use generic-lens or generic-optics with 'networkMode' instead." #-}

-- | A list of volume definitions in JSON format that containers in your task may use.
--
-- /Note:/ Consider using 'volumes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdVolumes :: Lens.Lens' RegisterTaskDefinition (Lude.Maybe [Volume])
rtdVolumes = Lens.lens (volumes :: RegisterTaskDefinition -> Lude.Maybe [Volume]) (\s a -> s {volumes = a} :: RegisterTaskDefinition)
{-# DEPRECATED rtdVolumes "Use generic-lens or generic-optics with 'volumes' instead." #-}

-- | The number of CPU units used by the task. It can be expressed as an integer using CPU units, for example @1024@ , or as a string using vCPUs, for example @1 vCPU@ or @1 vcpu@ , in a task definition. String values are converted to an integer indicating the CPU units when the task definition is registered.
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
rtdCpu :: Lens.Lens' RegisterTaskDefinition (Lude.Maybe Lude.Text)
rtdCpu = Lens.lens (cpu :: RegisterTaskDefinition -> Lude.Maybe Lude.Text) (\s a -> s {cpu = a} :: RegisterTaskDefinition)
{-# DEPRECATED rtdCpu "Use generic-lens or generic-optics with 'cpu' instead." #-}

-- | The metadata that you apply to the task definition to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
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
rtdTags :: Lens.Lens' RegisterTaskDefinition (Lude.Maybe [Tag])
rtdTags = Lens.lens (tags :: RegisterTaskDefinition -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: RegisterTaskDefinition)
{-# DEPRECATED rtdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | You must specify a @family@ for a task definition, which allows you to track multiple versions of the same task definition. The @family@ is used as a name for your task definition. Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed.
--
-- /Note:/ Consider using 'family' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdFamily :: Lens.Lens' RegisterTaskDefinition Lude.Text
rtdFamily = Lens.lens (family :: RegisterTaskDefinition -> Lude.Text) (\s a -> s {family = a} :: RegisterTaskDefinition)
{-# DEPRECATED rtdFamily "Use generic-lens or generic-optics with 'family' instead." #-}

-- | A list of container definitions in JSON format that describe the different containers that make up your task.
--
-- /Note:/ Consider using 'containerDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdContainerDefinitions :: Lens.Lens' RegisterTaskDefinition [ContainerDefinition]
rtdContainerDefinitions = Lens.lens (containerDefinitions :: RegisterTaskDefinition -> [ContainerDefinition]) (\s a -> s {containerDefinitions = a} :: RegisterTaskDefinition)
{-# DEPRECATED rtdContainerDefinitions "Use generic-lens or generic-optics with 'containerDefinitions' instead." #-}

instance Lude.AWSRequest RegisterTaskDefinition where
  type Rs RegisterTaskDefinition = RegisterTaskDefinitionResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          RegisterTaskDefinitionResponse'
            Lude.<$> (x Lude..?> "taskDefinition")
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterTaskDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.RegisterTaskDefinition" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterTaskDefinition where
  toJSON RegisterTaskDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("inferenceAccelerators" Lude..=) Lude.<$> inferenceAccelerators,
            ("executionRoleArn" Lude..=) Lude.<$> executionRoleARN,
            ("requiresCompatibilities" Lude..=)
              Lude.<$> requiresCompatibilities,
            ("pidMode" Lude..=) Lude.<$> pidMode,
            ("ipcMode" Lude..=) Lude.<$> ipcMode,
            ("memory" Lude..=) Lude.<$> memory,
            ("proxyConfiguration" Lude..=) Lude.<$> proxyConfiguration,
            ("taskRoleArn" Lude..=) Lude.<$> taskRoleARN,
            ("placementConstraints" Lude..=) Lude.<$> placementConstraints,
            ("networkMode" Lude..=) Lude.<$> networkMode,
            ("volumes" Lude..=) Lude.<$> volumes,
            ("cpu" Lude..=) Lude.<$> cpu,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("family" Lude..= family),
            Lude.Just ("containerDefinitions" Lude..= containerDefinitions)
          ]
      )

instance Lude.ToPath RegisterTaskDefinition where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterTaskDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRegisterTaskDefinitionResponse' smart constructor.
data RegisterTaskDefinitionResponse = RegisterTaskDefinitionResponse'
  { taskDefinition ::
      Lude.Maybe TaskDefinition,
    tags :: Lude.Maybe [Tag],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterTaskDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'tags' - The list of tags associated with the task definition.
-- * 'taskDefinition' - The full description of the registered task definition.
mkRegisterTaskDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterTaskDefinitionResponse
mkRegisterTaskDefinitionResponse pResponseStatus_ =
  RegisterTaskDefinitionResponse'
    { taskDefinition = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The full description of the registered task definition.
--
-- /Note:/ Consider using 'taskDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdrsTaskDefinition :: Lens.Lens' RegisterTaskDefinitionResponse (Lude.Maybe TaskDefinition)
rtdrsTaskDefinition = Lens.lens (taskDefinition :: RegisterTaskDefinitionResponse -> Lude.Maybe TaskDefinition) (\s a -> s {taskDefinition = a} :: RegisterTaskDefinitionResponse)
{-# DEPRECATED rtdrsTaskDefinition "Use generic-lens or generic-optics with 'taskDefinition' instead." #-}

-- | The list of tags associated with the task definition.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdrsTags :: Lens.Lens' RegisterTaskDefinitionResponse (Lude.Maybe [Tag])
rtdrsTags = Lens.lens (tags :: RegisterTaskDefinitionResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: RegisterTaskDefinitionResponse)
{-# DEPRECATED rtdrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdrsResponseStatus :: Lens.Lens' RegisterTaskDefinitionResponse Lude.Int
rtdrsResponseStatus = Lens.lens (responseStatus :: RegisterTaskDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterTaskDefinitionResponse)
{-# DEPRECATED rtdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
