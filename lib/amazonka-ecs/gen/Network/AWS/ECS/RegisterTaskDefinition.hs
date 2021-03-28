{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      RegisterTaskDefinition (..)
    , mkRegisterTaskDefinition
    -- ** Request lenses
    , rtdFamily
    , rtdContainerDefinitions
    , rtdCpu
    , rtdExecutionRoleArn
    , rtdInferenceAccelerators
    , rtdIpcMode
    , rtdMemory
    , rtdNetworkMode
    , rtdPidMode
    , rtdPlacementConstraints
    , rtdProxyConfiguration
    , rtdRequiresCompatibilities
    , rtdTags
    , rtdTaskRoleArn
    , rtdVolumes

    -- * Destructuring the response
    , RegisterTaskDefinitionResponse (..)
    , mkRegisterTaskDefinitionResponse
    -- ** Response lenses
    , rtdrrsTags
    , rtdrrsTaskDefinition
    , rtdrrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterTaskDefinition' smart constructor.
data RegisterTaskDefinition = RegisterTaskDefinition'
  { family :: Core.Text
    -- ^ You must specify a @family@ for a task definition, which allows you to track multiple versions of the same task definition. The @family@ is used as a name for your task definition. Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed.
  , containerDefinitions :: [Types.ContainerDefinition]
    -- ^ A list of container definitions in JSON format that describe the different containers that make up your task.
  , cpu :: Core.Maybe Core.Text
    -- ^ The number of CPU units used by the task. It can be expressed as an integer using CPU units, for example @1024@ , or as a string using vCPUs, for example @1 vCPU@ or @1 vcpu@ , in a task definition. String values are converted to an integer indicating the CPU units when the task definition is registered.
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
  , executionRoleArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the task execution role that grants the Amazon ECS container agent permission to make AWS API calls on your behalf. The task execution IAM role is required depending on the requirements of your task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_execution_IAM_role.html Amazon ECS task execution IAM role> in the /Amazon Elastic Container Service Developer Guide/ .
  , inferenceAccelerators :: Core.Maybe [Types.InferenceAccelerator]
    -- ^ The Elastic Inference accelerators to use for the containers in the task.
  , ipcMode :: Core.Maybe Types.IpcMode
    -- ^ The IPC resource namespace to use for the containers in the task. The valid values are @host@ , @task@ , or @none@ . If @host@ is specified, then all containers within the tasks that specified the @host@ IPC mode on the same container instance share the same IPC resources with the host Amazon EC2 instance. If @task@ is specified, all containers within the specified task share the same IPC resources. If @none@ is specified, then IPC resources within the containers of a task are private and not shared with other containers in a task or on the container instance. If no value is specified, then the IPC resource namespace sharing depends on the Docker daemon setting on the container instance. For more information, see <https://docs.docker.com/engine/reference/run/#ipc-settings---ipc IPC settings> in the /Docker run reference/ .
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
  , memory :: Core.Maybe Core.Text
    -- ^ The amount of memory (in MiB) used by the task. It can be expressed as an integer using MiB, for example @1024@ , or as a string using GB, for example @1GB@ or @1 GB@ , in a task definition. String values are converted to an integer indicating the MiB when the task definition is registered.
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
  , networkMode :: Core.Maybe Types.NetworkMode
    -- ^ The Docker networking mode to use for the containers in the task. The valid values are @none@ , @bridge@ , @awsvpc@ , and @host@ . If no network mode is specified, the default is @bridge@ .
--
-- For Amazon ECS tasks on Fargate, the @awsvpc@ network mode is required. For Amazon ECS tasks on Amazon EC2 instances, any network mode can be used. If the network mode is set to @none@ , you cannot specify port mappings in your container definitions, and the tasks containers do not have external connectivity. The @host@ and @awsvpc@ network modes offer the highest networking performance for containers because they use the EC2 network stack instead of the virtualized network stack provided by the @bridge@ mode.
-- With the @host@ and @awsvpc@ network modes, exposed container ports are mapped directly to the corresponding host port (for the @host@ network mode) or the attached elastic network interface port (for the @awsvpc@ network mode), so you cannot take advantage of dynamic host port mappings. 
-- /Important:/ When using the @host@ network mode, you should not run containers using the root user (UID 0). It is considered best practice to use a non-root user.
-- If the network mode is @awsvpc@ , the task is allocated an elastic network interface, and you must specify a 'NetworkConfiguration' value when you create a service or run a task with the task definition. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking> in the /Amazon Elastic Container Service Developer Guide/ .
-- If the network mode is @host@ , you cannot run multiple instantiations of the same task on a single container instance when port mappings are used.
-- Docker for Windows uses different network modes than Docker for Linux. When you register a task definition with Windows containers, you must not specify a network mode. If you use the console to register a task definition with Windows containers, you must choose the @<default>@ network mode object. 
-- For more information, see <https://docs.docker.com/engine/reference/run/#network-settings Network settings> in the /Docker run reference/ .
  , pidMode :: Core.Maybe Types.PidMode
    -- ^ The process namespace to use for the containers in the task. The valid values are @host@ or @task@ . If @host@ is specified, then all containers within the tasks that specified the @host@ PID mode on the same container instance share the same process namespace with the host Amazon EC2 instance. If @task@ is specified, all containers within the specified task share the same process namespace. If no value is specified, the default is a private namespace. For more information, see <https://docs.docker.com/engine/reference/run/#pid-settings---pid PID settings> in the /Docker run reference/ .
--
-- If the @host@ PID mode is used, be aware that there is a heightened risk of undesired process namespace expose. For more information, see <https://docs.docker.com/engine/security/security/ Docker security> .
  , placementConstraints :: Core.Maybe [Types.TaskDefinitionPlacementConstraint]
    -- ^ An array of placement constraint objects to use for the task. You can specify a maximum of 10 constraints per task (this limit includes constraints in the task definition and those specified at runtime).
  , proxyConfiguration :: Core.Maybe Types.ProxyConfiguration
  , requiresCompatibilities :: Core.Maybe [Types.Compatibility]
    -- ^ The task launch type that Amazon ECS should validate the task definition against. This ensures that the task definition parameters are compatible with the specified launch type. If no value is specified, it defaults to @EC2@ .
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The metadata that you apply to the task definition to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
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
  , taskRoleArn :: Core.Maybe Core.Text
    -- ^ The short name or full Amazon Resource Name (ARN) of the IAM role that containers in this task can assume. All containers in this task are granted the permissions that are specified in this role. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks> in the /Amazon Elastic Container Service Developer Guide/ .
  , volumes :: Core.Maybe [Types.Volume]
    -- ^ A list of volume definitions in JSON format that containers in your task may use.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterTaskDefinition' value with any optional fields omitted.
mkRegisterTaskDefinition
    :: Core.Text -- ^ 'family'
    -> RegisterTaskDefinition
mkRegisterTaskDefinition family
  = RegisterTaskDefinition'{family,
                            containerDefinitions = Core.mempty, cpu = Core.Nothing,
                            executionRoleArn = Core.Nothing,
                            inferenceAccelerators = Core.Nothing, ipcMode = Core.Nothing,
                            memory = Core.Nothing, networkMode = Core.Nothing,
                            pidMode = Core.Nothing, placementConstraints = Core.Nothing,
                            proxyConfiguration = Core.Nothing,
                            requiresCompatibilities = Core.Nothing, tags = Core.Nothing,
                            taskRoleArn = Core.Nothing, volumes = Core.Nothing}

-- | You must specify a @family@ for a task definition, which allows you to track multiple versions of the same task definition. The @family@ is used as a name for your task definition. Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed.
--
-- /Note:/ Consider using 'family' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdFamily :: Lens.Lens' RegisterTaskDefinition Core.Text
rtdFamily = Lens.field @"family"
{-# INLINEABLE rtdFamily #-}
{-# DEPRECATED family "Use generic-lens or generic-optics with 'family' instead"  #-}

-- | A list of container definitions in JSON format that describe the different containers that make up your task.
--
-- /Note:/ Consider using 'containerDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdContainerDefinitions :: Lens.Lens' RegisterTaskDefinition [Types.ContainerDefinition]
rtdContainerDefinitions = Lens.field @"containerDefinitions"
{-# INLINEABLE rtdContainerDefinitions #-}
{-# DEPRECATED containerDefinitions "Use generic-lens or generic-optics with 'containerDefinitions' instead"  #-}

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
rtdCpu :: Lens.Lens' RegisterTaskDefinition (Core.Maybe Core.Text)
rtdCpu = Lens.field @"cpu"
{-# INLINEABLE rtdCpu #-}
{-# DEPRECATED cpu "Use generic-lens or generic-optics with 'cpu' instead"  #-}

-- | The Amazon Resource Name (ARN) of the task execution role that grants the Amazon ECS container agent permission to make AWS API calls on your behalf. The task execution IAM role is required depending on the requirements of your task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_execution_IAM_role.html Amazon ECS task execution IAM role> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'executionRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdExecutionRoleArn :: Lens.Lens' RegisterTaskDefinition (Core.Maybe Core.Text)
rtdExecutionRoleArn = Lens.field @"executionRoleArn"
{-# INLINEABLE rtdExecutionRoleArn #-}
{-# DEPRECATED executionRoleArn "Use generic-lens or generic-optics with 'executionRoleArn' instead"  #-}

-- | The Elastic Inference accelerators to use for the containers in the task.
--
-- /Note:/ Consider using 'inferenceAccelerators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdInferenceAccelerators :: Lens.Lens' RegisterTaskDefinition (Core.Maybe [Types.InferenceAccelerator])
rtdInferenceAccelerators = Lens.field @"inferenceAccelerators"
{-# INLINEABLE rtdInferenceAccelerators #-}
{-# DEPRECATED inferenceAccelerators "Use generic-lens or generic-optics with 'inferenceAccelerators' instead"  #-}

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
rtdIpcMode :: Lens.Lens' RegisterTaskDefinition (Core.Maybe Types.IpcMode)
rtdIpcMode = Lens.field @"ipcMode"
{-# INLINEABLE rtdIpcMode #-}
{-# DEPRECATED ipcMode "Use generic-lens or generic-optics with 'ipcMode' instead"  #-}

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
rtdMemory :: Lens.Lens' RegisterTaskDefinition (Core.Maybe Core.Text)
rtdMemory = Lens.field @"memory"
{-# INLINEABLE rtdMemory #-}
{-# DEPRECATED memory "Use generic-lens or generic-optics with 'memory' instead"  #-}

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
rtdNetworkMode :: Lens.Lens' RegisterTaskDefinition (Core.Maybe Types.NetworkMode)
rtdNetworkMode = Lens.field @"networkMode"
{-# INLINEABLE rtdNetworkMode #-}
{-# DEPRECATED networkMode "Use generic-lens or generic-optics with 'networkMode' instead"  #-}

-- | The process namespace to use for the containers in the task. The valid values are @host@ or @task@ . If @host@ is specified, then all containers within the tasks that specified the @host@ PID mode on the same container instance share the same process namespace with the host Amazon EC2 instance. If @task@ is specified, all containers within the specified task share the same process namespace. If no value is specified, the default is a private namespace. For more information, see <https://docs.docker.com/engine/reference/run/#pid-settings---pid PID settings> in the /Docker run reference/ .
--
-- If the @host@ PID mode is used, be aware that there is a heightened risk of undesired process namespace expose. For more information, see <https://docs.docker.com/engine/security/security/ Docker security> .
--
-- /Note:/ Consider using 'pidMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdPidMode :: Lens.Lens' RegisterTaskDefinition (Core.Maybe Types.PidMode)
rtdPidMode = Lens.field @"pidMode"
{-# INLINEABLE rtdPidMode #-}
{-# DEPRECATED pidMode "Use generic-lens or generic-optics with 'pidMode' instead"  #-}

-- | An array of placement constraint objects to use for the task. You can specify a maximum of 10 constraints per task (this limit includes constraints in the task definition and those specified at runtime).
--
-- /Note:/ Consider using 'placementConstraints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdPlacementConstraints :: Lens.Lens' RegisterTaskDefinition (Core.Maybe [Types.TaskDefinitionPlacementConstraint])
rtdPlacementConstraints = Lens.field @"placementConstraints"
{-# INLINEABLE rtdPlacementConstraints #-}
{-# DEPRECATED placementConstraints "Use generic-lens or generic-optics with 'placementConstraints' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'proxyConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdProxyConfiguration :: Lens.Lens' RegisterTaskDefinition (Core.Maybe Types.ProxyConfiguration)
rtdProxyConfiguration = Lens.field @"proxyConfiguration"
{-# INLINEABLE rtdProxyConfiguration #-}
{-# DEPRECATED proxyConfiguration "Use generic-lens or generic-optics with 'proxyConfiguration' instead"  #-}

-- | The task launch type that Amazon ECS should validate the task definition against. This ensures that the task definition parameters are compatible with the specified launch type. If no value is specified, it defaults to @EC2@ .
--
-- /Note:/ Consider using 'requiresCompatibilities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdRequiresCompatibilities :: Lens.Lens' RegisterTaskDefinition (Core.Maybe [Types.Compatibility])
rtdRequiresCompatibilities = Lens.field @"requiresCompatibilities"
{-# INLINEABLE rtdRequiresCompatibilities #-}
{-# DEPRECATED requiresCompatibilities "Use generic-lens or generic-optics with 'requiresCompatibilities' instead"  #-}

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
rtdTags :: Lens.Lens' RegisterTaskDefinition (Core.Maybe [Types.Tag])
rtdTags = Lens.field @"tags"
{-# INLINEABLE rtdTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The short name or full Amazon Resource Name (ARN) of the IAM role that containers in this task can assume. All containers in this task are granted the permissions that are specified in this role. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'taskRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdTaskRoleArn :: Lens.Lens' RegisterTaskDefinition (Core.Maybe Core.Text)
rtdTaskRoleArn = Lens.field @"taskRoleArn"
{-# INLINEABLE rtdTaskRoleArn #-}
{-# DEPRECATED taskRoleArn "Use generic-lens or generic-optics with 'taskRoleArn' instead"  #-}

-- | A list of volume definitions in JSON format that containers in your task may use.
--
-- /Note:/ Consider using 'volumes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdVolumes :: Lens.Lens' RegisterTaskDefinition (Core.Maybe [Types.Volume])
rtdVolumes = Lens.field @"volumes"
{-# INLINEABLE rtdVolumes #-}
{-# DEPRECATED volumes "Use generic-lens or generic-optics with 'volumes' instead"  #-}

instance Core.ToQuery RegisterTaskDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RegisterTaskDefinition where
        toHeaders RegisterTaskDefinition{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerServiceV20141113.RegisterTaskDefinition")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RegisterTaskDefinition where
        toJSON RegisterTaskDefinition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("family" Core..= family),
                  Core.Just ("containerDefinitions" Core..= containerDefinitions),
                  ("cpu" Core..=) Core.<$> cpu,
                  ("executionRoleArn" Core..=) Core.<$> executionRoleArn,
                  ("inferenceAccelerators" Core..=) Core.<$> inferenceAccelerators,
                  ("ipcMode" Core..=) Core.<$> ipcMode,
                  ("memory" Core..=) Core.<$> memory,
                  ("networkMode" Core..=) Core.<$> networkMode,
                  ("pidMode" Core..=) Core.<$> pidMode,
                  ("placementConstraints" Core..=) Core.<$> placementConstraints,
                  ("proxyConfiguration" Core..=) Core.<$> proxyConfiguration,
                  ("requiresCompatibilities" Core..=) Core.<$>
                    requiresCompatibilities,
                  ("tags" Core..=) Core.<$> tags,
                  ("taskRoleArn" Core..=) Core.<$> taskRoleArn,
                  ("volumes" Core..=) Core.<$> volumes])

instance Core.AWSRequest RegisterTaskDefinition where
        type Rs RegisterTaskDefinition = RegisterTaskDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RegisterTaskDefinitionResponse' Core.<$>
                   (x Core..:? "tags") Core.<*> x Core..:? "taskDefinition" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRegisterTaskDefinitionResponse' smart constructor.
data RegisterTaskDefinitionResponse = RegisterTaskDefinitionResponse'
  { tags :: Core.Maybe [Types.Tag]
    -- ^ The list of tags associated with the task definition.
  , taskDefinition :: Core.Maybe Types.TaskDefinition
    -- ^ The full description of the registered task definition.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterTaskDefinitionResponse' value with any optional fields omitted.
mkRegisterTaskDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RegisterTaskDefinitionResponse
mkRegisterTaskDefinitionResponse responseStatus
  = RegisterTaskDefinitionResponse'{tags = Core.Nothing,
                                    taskDefinition = Core.Nothing, responseStatus}

-- | The list of tags associated with the task definition.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdrrsTags :: Lens.Lens' RegisterTaskDefinitionResponse (Core.Maybe [Types.Tag])
rtdrrsTags = Lens.field @"tags"
{-# INLINEABLE rtdrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The full description of the registered task definition.
--
-- /Note:/ Consider using 'taskDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdrrsTaskDefinition :: Lens.Lens' RegisterTaskDefinitionResponse (Core.Maybe Types.TaskDefinition)
rtdrrsTaskDefinition = Lens.field @"taskDefinition"
{-# INLINEABLE rtdrrsTaskDefinition #-}
{-# DEPRECATED taskDefinition "Use generic-lens or generic-optics with 'taskDefinition' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdrrsResponseStatus :: Lens.Lens' RegisterTaskDefinitionResponse Core.Int
rtdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rtdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
