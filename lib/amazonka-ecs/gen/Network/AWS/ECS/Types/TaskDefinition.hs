{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.TaskDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.TaskDefinition
  ( TaskDefinition (..)
  -- * Smart constructor
  , mkTaskDefinition
  -- * Lenses
  , tdCompatibilities
  , tdContainerDefinitions
  , tdCpu
  , tdExecutionRoleArn
  , tdFamily
  , tdInferenceAccelerators
  , tdIpcMode
  , tdMemory
  , tdNetworkMode
  , tdPidMode
  , tdPlacementConstraints
  , tdProxyConfiguration
  , tdRequiresAttributes
  , tdRequiresCompatibilities
  , tdRevision
  , tdStatus
  , tdTaskDefinitionArn
  , tdTaskRoleArn
  , tdVolumes
  ) where

import qualified Network.AWS.ECS.Types.Attribute as Types
import qualified Network.AWS.ECS.Types.Compatibility as Types
import qualified Network.AWS.ECS.Types.ContainerDefinition as Types
import qualified Network.AWS.ECS.Types.InferenceAccelerator as Types
import qualified Network.AWS.ECS.Types.IpcMode as Types
import qualified Network.AWS.ECS.Types.NetworkMode as Types
import qualified Network.AWS.ECS.Types.PidMode as Types
import qualified Network.AWS.ECS.Types.ProxyConfiguration as Types
import qualified Network.AWS.ECS.Types.TaskDefinitionPlacementConstraint as Types
import qualified Network.AWS.ECS.Types.TaskDefinitionStatus as Types
import qualified Network.AWS.ECS.Types.Volume as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details of a task definition which describes the container and volume definitions of an Amazon Elastic Container Service task. You can specify which Docker images to use, the required resources, and other configurations related to launching the task definition through an Amazon ECS service or task.
--
-- /See:/ 'mkTaskDefinition' smart constructor.
data TaskDefinition = TaskDefinition'
  { compatibilities :: Core.Maybe [Types.Compatibility]
    -- ^ The launch type to use with your task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
  , containerDefinitions :: Core.Maybe [Types.ContainerDefinition]
    -- ^ A list of container definitions in JSON format that describe the different containers that make up your task. For more information about container definition parameters and defaults, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Amazon ECS Task Definitions> in the /Amazon Elastic Container Service Developer Guide/ .
  , cpu :: Core.Maybe Core.Text
    -- ^ The number of @cpu@ units used by the task. If you are using the EC2 launch type, this field is optional and any value can be used. If you are using the Fargate launch type, this field is required and you must use one of the following values, which determines your range of valid values for the @memory@ parameter:
--
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
  , family :: Core.Maybe Core.Text
    -- ^ The name of a family that this task definition is registered to. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
--
-- A family groups multiple versions of a task definition. Amazon ECS gives the first task definition that you registered to a family a revision number of 1. Amazon ECS gives sequential revision numbers to each task definition that you add.
  , inferenceAccelerators :: Core.Maybe [Types.InferenceAccelerator]
    -- ^ The Elastic Inference accelerator associated with the task.
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
    -- ^ The amount (in MiB) of memory used by the task.
--
-- If using the EC2 launch type, you must specify either a task-level memory value or a container-level memory value. This field is optional and any value can be used. If a task-level memory value is specified then the container-level memory value is optional. For more information regarding container-level memory and memory reservation, see <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ContainerDefinition.html ContainerDefinition> .
-- If using the Fargate launch type, this field is required and you must use one of the following values, which determines your range of valid values for the @cpu@ parameter:
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
    -- ^ An array of placement constraint objects to use for tasks. This field is not valid if you are using the Fargate launch type for your task.
  , proxyConfiguration :: Core.Maybe Types.ProxyConfiguration
    -- ^ The configuration details for the App Mesh proxy.
--
-- Your Amazon ECS container instances require at least version 1.26.0 of the container agent and at least version 1.26.0-1 of the @ecs-init@ package to enable a proxy configuration. If your container instances are launched from the Amazon ECS-optimized AMI version @20190301@ or later, then they contain the required versions of the container agent and @ecs-init@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI> in the /Amazon Elastic Container Service Developer Guide/ .
  , requiresAttributes :: Core.Maybe [Types.Attribute]
    -- ^ The container instance attributes required by your task. This field is not valid if you are using the Fargate launch type for your task.
  , requiresCompatibilities :: Core.Maybe [Types.Compatibility]
    -- ^ The launch type the task requires. If no value is specified, it will default to @EC2@ . Valid values include @EC2@ and @FARGATE@ .
  , revision :: Core.Maybe Core.Int
    -- ^ The revision of the task in a particular family. The revision is a version number of a task definition in a family. When you register a task definition for the first time, the revision is @1@ . Each time that you register a new revision of a task definition in the same family, the revision value always increases by one, even if you have deregistered previous revisions in this family.
  , status :: Core.Maybe Types.TaskDefinitionStatus
    -- ^ The status of the task definition.
  , taskDefinitionArn :: Core.Maybe Core.Text
    -- ^ The full Amazon Resource Name (ARN) of the task definition.
  , taskRoleArn :: Core.Maybe Core.Text
    -- ^ The short name or full Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants containers in the task permission to call AWS APIs on your behalf. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html Amazon ECS Task Role> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- IAM roles for tasks on Windows require that the @-EnableTaskIAMRole@ option is set when you launch the Amazon ECS-optimized Windows AMI. Your containers must also run some configuration code in order to take advantage of the feature. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/windows_task_IAM_roles.html Windows IAM Roles for Tasks> in the /Amazon Elastic Container Service Developer Guide/ .
  , volumes :: Core.Maybe [Types.Volume]
    -- ^ The list of volume definitions for the task.
--
-- If your tasks are using the Fargate launch type, the @host@ and @sourcePath@ parameters are not supported.
-- For more information about volume definition parameters and defaults, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_definitions.html Amazon ECS Task Definitions> in the /Amazon Elastic Container Service Developer Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TaskDefinition' value with any optional fields omitted.
mkTaskDefinition
    :: TaskDefinition
mkTaskDefinition
  = TaskDefinition'{compatibilities = Core.Nothing,
                    containerDefinitions = Core.Nothing, cpu = Core.Nothing,
                    executionRoleArn = Core.Nothing, family = Core.Nothing,
                    inferenceAccelerators = Core.Nothing, ipcMode = Core.Nothing,
                    memory = Core.Nothing, networkMode = Core.Nothing,
                    pidMode = Core.Nothing, placementConstraints = Core.Nothing,
                    proxyConfiguration = Core.Nothing,
                    requiresAttributes = Core.Nothing,
                    requiresCompatibilities = Core.Nothing, revision = Core.Nothing,
                    status = Core.Nothing, taskDefinitionArn = Core.Nothing,
                    taskRoleArn = Core.Nothing, volumes = Core.Nothing}

-- | The launch type to use with your task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'compatibilities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdCompatibilities :: Lens.Lens' TaskDefinition (Core.Maybe [Types.Compatibility])
tdCompatibilities = Lens.field @"compatibilities"
{-# INLINEABLE tdCompatibilities #-}
{-# DEPRECATED compatibilities "Use generic-lens or generic-optics with 'compatibilities' instead"  #-}

-- | A list of container definitions in JSON format that describe the different containers that make up your task. For more information about container definition parameters and defaults, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Amazon ECS Task Definitions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'containerDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdContainerDefinitions :: Lens.Lens' TaskDefinition (Core.Maybe [Types.ContainerDefinition])
tdContainerDefinitions = Lens.field @"containerDefinitions"
{-# INLINEABLE tdContainerDefinitions #-}
{-# DEPRECATED containerDefinitions "Use generic-lens or generic-optics with 'containerDefinitions' instead"  #-}

-- | The number of @cpu@ units used by the task. If you are using the EC2 launch type, this field is optional and any value can be used. If you are using the Fargate launch type, this field is required and you must use one of the following values, which determines your range of valid values for the @memory@ parameter:
--
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
tdCpu :: Lens.Lens' TaskDefinition (Core.Maybe Core.Text)
tdCpu = Lens.field @"cpu"
{-# INLINEABLE tdCpu #-}
{-# DEPRECATED cpu "Use generic-lens or generic-optics with 'cpu' instead"  #-}

-- | The Amazon Resource Name (ARN) of the task execution role that grants the Amazon ECS container agent permission to make AWS API calls on your behalf. The task execution IAM role is required depending on the requirements of your task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_execution_IAM_role.html Amazon ECS task execution IAM role> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'executionRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdExecutionRoleArn :: Lens.Lens' TaskDefinition (Core.Maybe Core.Text)
tdExecutionRoleArn = Lens.field @"executionRoleArn"
{-# INLINEABLE tdExecutionRoleArn #-}
{-# DEPRECATED executionRoleArn "Use generic-lens or generic-optics with 'executionRoleArn' instead"  #-}

-- | The name of a family that this task definition is registered to. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
--
-- A family groups multiple versions of a task definition. Amazon ECS gives the first task definition that you registered to a family a revision number of 1. Amazon ECS gives sequential revision numbers to each task definition that you add.
--
-- /Note:/ Consider using 'family' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdFamily :: Lens.Lens' TaskDefinition (Core.Maybe Core.Text)
tdFamily = Lens.field @"family"
{-# INLINEABLE tdFamily #-}
{-# DEPRECATED family "Use generic-lens or generic-optics with 'family' instead"  #-}

-- | The Elastic Inference accelerator associated with the task.
--
-- /Note:/ Consider using 'inferenceAccelerators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdInferenceAccelerators :: Lens.Lens' TaskDefinition (Core.Maybe [Types.InferenceAccelerator])
tdInferenceAccelerators = Lens.field @"inferenceAccelerators"
{-# INLINEABLE tdInferenceAccelerators #-}
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
tdIpcMode :: Lens.Lens' TaskDefinition (Core.Maybe Types.IpcMode)
tdIpcMode = Lens.field @"ipcMode"
{-# INLINEABLE tdIpcMode #-}
{-# DEPRECATED ipcMode "Use generic-lens or generic-optics with 'ipcMode' instead"  #-}

-- | The amount (in MiB) of memory used by the task.
--
-- If using the EC2 launch type, you must specify either a task-level memory value or a container-level memory value. This field is optional and any value can be used. If a task-level memory value is specified then the container-level memory value is optional. For more information regarding container-level memory and memory reservation, see <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ContainerDefinition.html ContainerDefinition> .
-- If using the Fargate launch type, this field is required and you must use one of the following values, which determines your range of valid values for the @cpu@ parameter:
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
tdMemory :: Lens.Lens' TaskDefinition (Core.Maybe Core.Text)
tdMemory = Lens.field @"memory"
{-# INLINEABLE tdMemory #-}
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
tdNetworkMode :: Lens.Lens' TaskDefinition (Core.Maybe Types.NetworkMode)
tdNetworkMode = Lens.field @"networkMode"
{-# INLINEABLE tdNetworkMode #-}
{-# DEPRECATED networkMode "Use generic-lens or generic-optics with 'networkMode' instead"  #-}

-- | The process namespace to use for the containers in the task. The valid values are @host@ or @task@ . If @host@ is specified, then all containers within the tasks that specified the @host@ PID mode on the same container instance share the same process namespace with the host Amazon EC2 instance. If @task@ is specified, all containers within the specified task share the same process namespace. If no value is specified, the default is a private namespace. For more information, see <https://docs.docker.com/engine/reference/run/#pid-settings---pid PID settings> in the /Docker run reference/ .
--
-- If the @host@ PID mode is used, be aware that there is a heightened risk of undesired process namespace expose. For more information, see <https://docs.docker.com/engine/security/security/ Docker security> .
--
-- /Note:/ Consider using 'pidMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdPidMode :: Lens.Lens' TaskDefinition (Core.Maybe Types.PidMode)
tdPidMode = Lens.field @"pidMode"
{-# INLINEABLE tdPidMode #-}
{-# DEPRECATED pidMode "Use generic-lens or generic-optics with 'pidMode' instead"  #-}

-- | An array of placement constraint objects to use for tasks. This field is not valid if you are using the Fargate launch type for your task.
--
-- /Note:/ Consider using 'placementConstraints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdPlacementConstraints :: Lens.Lens' TaskDefinition (Core.Maybe [Types.TaskDefinitionPlacementConstraint])
tdPlacementConstraints = Lens.field @"placementConstraints"
{-# INLINEABLE tdPlacementConstraints #-}
{-# DEPRECATED placementConstraints "Use generic-lens or generic-optics with 'placementConstraints' instead"  #-}

-- | The configuration details for the App Mesh proxy.
--
-- Your Amazon ECS container instances require at least version 1.26.0 of the container agent and at least version 1.26.0-1 of the @ecs-init@ package to enable a proxy configuration. If your container instances are launched from the Amazon ECS-optimized AMI version @20190301@ or later, then they contain the required versions of the container agent and @ecs-init@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'proxyConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdProxyConfiguration :: Lens.Lens' TaskDefinition (Core.Maybe Types.ProxyConfiguration)
tdProxyConfiguration = Lens.field @"proxyConfiguration"
{-# INLINEABLE tdProxyConfiguration #-}
{-# DEPRECATED proxyConfiguration "Use generic-lens or generic-optics with 'proxyConfiguration' instead"  #-}

-- | The container instance attributes required by your task. This field is not valid if you are using the Fargate launch type for your task.
--
-- /Note:/ Consider using 'requiresAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdRequiresAttributes :: Lens.Lens' TaskDefinition (Core.Maybe [Types.Attribute])
tdRequiresAttributes = Lens.field @"requiresAttributes"
{-# INLINEABLE tdRequiresAttributes #-}
{-# DEPRECATED requiresAttributes "Use generic-lens or generic-optics with 'requiresAttributes' instead"  #-}

-- | The launch type the task requires. If no value is specified, it will default to @EC2@ . Valid values include @EC2@ and @FARGATE@ .
--
-- /Note:/ Consider using 'requiresCompatibilities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdRequiresCompatibilities :: Lens.Lens' TaskDefinition (Core.Maybe [Types.Compatibility])
tdRequiresCompatibilities = Lens.field @"requiresCompatibilities"
{-# INLINEABLE tdRequiresCompatibilities #-}
{-# DEPRECATED requiresCompatibilities "Use generic-lens or generic-optics with 'requiresCompatibilities' instead"  #-}

-- | The revision of the task in a particular family. The revision is a version number of a task definition in a family. When you register a task definition for the first time, the revision is @1@ . Each time that you register a new revision of a task definition in the same family, the revision value always increases by one, even if you have deregistered previous revisions in this family.
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdRevision :: Lens.Lens' TaskDefinition (Core.Maybe Core.Int)
tdRevision = Lens.field @"revision"
{-# INLINEABLE tdRevision #-}
{-# DEPRECATED revision "Use generic-lens or generic-optics with 'revision' instead"  #-}

-- | The status of the task definition.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdStatus :: Lens.Lens' TaskDefinition (Core.Maybe Types.TaskDefinitionStatus)
tdStatus = Lens.field @"status"
{-# INLINEABLE tdStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The full Amazon Resource Name (ARN) of the task definition.
--
-- /Note:/ Consider using 'taskDefinitionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdTaskDefinitionArn :: Lens.Lens' TaskDefinition (Core.Maybe Core.Text)
tdTaskDefinitionArn = Lens.field @"taskDefinitionArn"
{-# INLINEABLE tdTaskDefinitionArn #-}
{-# DEPRECATED taskDefinitionArn "Use generic-lens or generic-optics with 'taskDefinitionArn' instead"  #-}

-- | The short name or full Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants containers in the task permission to call AWS APIs on your behalf. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html Amazon ECS Task Role> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- IAM roles for tasks on Windows require that the @-EnableTaskIAMRole@ option is set when you launch the Amazon ECS-optimized Windows AMI. Your containers must also run some configuration code in order to take advantage of the feature. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/windows_task_IAM_roles.html Windows IAM Roles for Tasks> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'taskRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdTaskRoleArn :: Lens.Lens' TaskDefinition (Core.Maybe Core.Text)
tdTaskRoleArn = Lens.field @"taskRoleArn"
{-# INLINEABLE tdTaskRoleArn #-}
{-# DEPRECATED taskRoleArn "Use generic-lens or generic-optics with 'taskRoleArn' instead"  #-}

-- | The list of volume definitions for the task.
--
-- If your tasks are using the Fargate launch type, the @host@ and @sourcePath@ parameters are not supported.
-- For more information about volume definition parameters and defaults, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_definitions.html Amazon ECS Task Definitions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'volumes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdVolumes :: Lens.Lens' TaskDefinition (Core.Maybe [Types.Volume])
tdVolumes = Lens.field @"volumes"
{-# INLINEABLE tdVolumes #-}
{-# DEPRECATED volumes "Use generic-lens or generic-optics with 'volumes' instead"  #-}

instance Core.FromJSON TaskDefinition where
        parseJSON
          = Core.withObject "TaskDefinition" Core.$
              \ x ->
                TaskDefinition' Core.<$>
                  (x Core..:? "compatibilities") Core.<*>
                    x Core..:? "containerDefinitions"
                    Core.<*> x Core..:? "cpu"
                    Core.<*> x Core..:? "executionRoleArn"
                    Core.<*> x Core..:? "family"
                    Core.<*> x Core..:? "inferenceAccelerators"
                    Core.<*> x Core..:? "ipcMode"
                    Core.<*> x Core..:? "memory"
                    Core.<*> x Core..:? "networkMode"
                    Core.<*> x Core..:? "pidMode"
                    Core.<*> x Core..:? "placementConstraints"
                    Core.<*> x Core..:? "proxyConfiguration"
                    Core.<*> x Core..:? "requiresAttributes"
                    Core.<*> x Core..:? "requiresCompatibilities"
                    Core.<*> x Core..:? "revision"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "taskDefinitionArn"
                    Core.<*> x Core..:? "taskRoleArn"
                    Core.<*> x Core..:? "volumes"
