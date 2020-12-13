{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.TaskDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.TaskDefinition
  ( TaskDefinition (..),

    -- * Smart constructor
    mkTaskDefinition,

    -- * Lenses
    tdStatus,
    tdInferenceAccelerators,
    tdExecutionRoleARN,
    tdRequiresCompatibilities,
    tdPidMode,
    tdFamily,
    tdIpcMode,
    tdContainerDefinitions,
    tdMemory,
    tdProxyConfiguration,
    tdTaskRoleARN,
    tdPlacementConstraints,
    tdNetworkMode,
    tdTaskDefinitionARN,
    tdCompatibilities,
    tdRevision,
    tdVolumes,
    tdCpu,
    tdRequiresAttributes,
  )
where

import Network.AWS.ECS.Types.Attribute
import Network.AWS.ECS.Types.Compatibility
import Network.AWS.ECS.Types.ContainerDefinition
import Network.AWS.ECS.Types.IPcMode
import Network.AWS.ECS.Types.InferenceAccelerator
import Network.AWS.ECS.Types.NetworkMode
import Network.AWS.ECS.Types.PidMode
import Network.AWS.ECS.Types.ProxyConfiguration
import Network.AWS.ECS.Types.TaskDefinitionPlacementConstraint
import Network.AWS.ECS.Types.TaskDefinitionStatus
import Network.AWS.ECS.Types.Volume
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details of a task definition which describes the container and volume definitions of an Amazon Elastic Container Service task. You can specify which Docker images to use, the required resources, and other configurations related to launching the task definition through an Amazon ECS service or task.
--
-- /See:/ 'mkTaskDefinition' smart constructor.
data TaskDefinition = TaskDefinition'
  { -- | The status of the task definition.
    status :: Lude.Maybe TaskDefinitionStatus,
    -- | The Elastic Inference accelerator associated with the task.
    inferenceAccelerators :: Lude.Maybe [InferenceAccelerator],
    -- | The Amazon Resource Name (ARN) of the task execution role that grants the Amazon ECS container agent permission to make AWS API calls on your behalf. The task execution IAM role is required depending on the requirements of your task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_execution_IAM_role.html Amazon ECS task execution IAM role> in the /Amazon Elastic Container Service Developer Guide/ .
    executionRoleARN :: Lude.Maybe Lude.Text,
    -- | The launch type the task requires. If no value is specified, it will default to @EC2@ . Valid values include @EC2@ and @FARGATE@ .
    requiresCompatibilities :: Lude.Maybe [Compatibility],
    -- | The process namespace to use for the containers in the task. The valid values are @host@ or @task@ . If @host@ is specified, then all containers within the tasks that specified the @host@ PID mode on the same container instance share the same process namespace with the host Amazon EC2 instance. If @task@ is specified, all containers within the specified task share the same process namespace. If no value is specified, the default is a private namespace. For more information, see <https://docs.docker.com/engine/reference/run/#pid-settings---pid PID settings> in the /Docker run reference/ .
    --
    -- If the @host@ PID mode is used, be aware that there is a heightened risk of undesired process namespace expose. For more information, see <https://docs.docker.com/engine/security/security/ Docker security> .
    pidMode :: Lude.Maybe PidMode,
    -- | The name of a family that this task definition is registered to. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
    --
    -- A family groups multiple versions of a task definition. Amazon ECS gives the first task definition that you registered to a family a revision number of 1. Amazon ECS gives sequential revision numbers to each task definition that you add.
    family :: Lude.Maybe Lude.Text,
    -- | The IPC resource namespace to use for the containers in the task. The valid values are @host@ , @task@ , or @none@ . If @host@ is specified, then all containers within the tasks that specified the @host@ IPC mode on the same container instance share the same IPC resources with the host Amazon EC2 instance. If @task@ is specified, all containers within the specified task share the same IPC resources. If @none@ is specified, then IPC resources within the containers of a task are private and not shared with other containers in a task or on the container instance. If no value is specified, then the IPC resource namespace sharing depends on the Docker daemon setting on the container instance. For more information, see <https://docs.docker.com/engine/reference/run/#ipc-settings---ipc IPC settings> in the /Docker run reference/ .
    --
    -- If the @host@ IPC mode is used, be aware that there is a heightened risk of undesired IPC namespace expose. For more information, see <https://docs.docker.com/engine/security/security/ Docker security> .
    -- If you are setting namespaced kernel parameters using @systemControls@ for the containers in the task, the following will apply to your IPC resource namespace. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_definition_parameters.html System Controls> in the /Amazon Elastic Container Service Developer Guide/ .
    --
    --     * For tasks that use the @host@ IPC mode, IPC namespace related @systemControls@ are not supported.
    --
    --
    --     * For tasks that use the @task@ IPC mode, IPC namespace related @systemControls@ will apply to all containers within a task.
    ipcMode :: Lude.Maybe IPcMode,
    -- | A list of container definitions in JSON format that describe the different containers that make up your task. For more information about container definition parameters and defaults, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Amazon ECS Task Definitions> in the /Amazon Elastic Container Service Developer Guide/ .
    containerDefinitions :: Lude.Maybe [ContainerDefinition],
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
    memory :: Lude.Maybe Lude.Text,
    -- | The configuration details for the App Mesh proxy.
    --
    -- Your Amazon ECS container instances require at least version 1.26.0 of the container agent and at least version 1.26.0-1 of the @ecs-init@ package to enable a proxy configuration. If your container instances are launched from the Amazon ECS-optimized AMI version @20190301@ or later, then they contain the required versions of the container agent and @ecs-init@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI> in the /Amazon Elastic Container Service Developer Guide/ .
    proxyConfiguration :: Lude.Maybe ProxyConfiguration,
    -- | The short name or full Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants containers in the task permission to call AWS APIs on your behalf. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html Amazon ECS Task Role> in the /Amazon Elastic Container Service Developer Guide/ .
    --
    -- IAM roles for tasks on Windows require that the @-EnableTaskIAMRole@ option is set when you launch the Amazon ECS-optimized Windows AMI. Your containers must also run some configuration code in order to take advantage of the feature. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/windows_task_IAM_roles.html Windows IAM Roles for Tasks> in the /Amazon Elastic Container Service Developer Guide/ .
    taskRoleARN :: Lude.Maybe Lude.Text,
    -- | An array of placement constraint objects to use for tasks. This field is not valid if you are using the Fargate launch type for your task.
    placementConstraints :: Lude.Maybe [TaskDefinitionPlacementConstraint],
    -- | The Docker networking mode to use for the containers in the task. The valid values are @none@ , @bridge@ , @awsvpc@ , and @host@ . If no network mode is specified, the default is @bridge@ .
    --
    -- For Amazon ECS tasks on Fargate, the @awsvpc@ network mode is required. For Amazon ECS tasks on Amazon EC2 instances, any network mode can be used. If the network mode is set to @none@ , you cannot specify port mappings in your container definitions, and the tasks containers do not have external connectivity. The @host@ and @awsvpc@ network modes offer the highest networking performance for containers because they use the EC2 network stack instead of the virtualized network stack provided by the @bridge@ mode.
    -- With the @host@ and @awsvpc@ network modes, exposed container ports are mapped directly to the corresponding host port (for the @host@ network mode) or the attached elastic network interface port (for the @awsvpc@ network mode), so you cannot take advantage of dynamic host port mappings.
    -- /Important:/ When using the @host@ network mode, you should not run containers using the root user (UID 0). It is considered best practice to use a non-root user.
    -- If the network mode is @awsvpc@ , the task is allocated an elastic network interface, and you must specify a 'NetworkConfiguration' value when you create a service or run a task with the task definition. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking> in the /Amazon Elastic Container Service Developer Guide/ .
    -- If the network mode is @host@ , you cannot run multiple instantiations of the same task on a single container instance when port mappings are used.
    -- Docker for Windows uses different network modes than Docker for Linux. When you register a task definition with Windows containers, you must not specify a network mode. If you use the console to register a task definition with Windows containers, you must choose the @<default>@ network mode object.
    -- For more information, see <https://docs.docker.com/engine/reference/run/#network-settings Network settings> in the /Docker run reference/ .
    networkMode :: Lude.Maybe NetworkMode,
    -- | The full Amazon Resource Name (ARN) of the task definition.
    taskDefinitionARN :: Lude.Maybe Lude.Text,
    -- | The launch type to use with your task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
    compatibilities :: Lude.Maybe [Compatibility],
    -- | The revision of the task in a particular family. The revision is a version number of a task definition in a family. When you register a task definition for the first time, the revision is @1@ . Each time that you register a new revision of a task definition in the same family, the revision value always increases by one, even if you have deregistered previous revisions in this family.
    revision :: Lude.Maybe Lude.Int,
    -- | The list of volume definitions for the task.
    --
    -- If your tasks are using the Fargate launch type, the @host@ and @sourcePath@ parameters are not supported.
    -- For more information about volume definition parameters and defaults, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_definitions.html Amazon ECS Task Definitions> in the /Amazon Elastic Container Service Developer Guide/ .
    volumes :: Lude.Maybe [Volume],
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
    cpu :: Lude.Maybe Lude.Text,
    -- | The container instance attributes required by your task. This field is not valid if you are using the Fargate launch type for your task.
    requiresAttributes :: Lude.Maybe [Attribute]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TaskDefinition' with the minimum fields required to make a request.
--
-- * 'status' - The status of the task definition.
-- * 'inferenceAccelerators' - The Elastic Inference accelerator associated with the task.
-- * 'executionRoleARN' - The Amazon Resource Name (ARN) of the task execution role that grants the Amazon ECS container agent permission to make AWS API calls on your behalf. The task execution IAM role is required depending on the requirements of your task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_execution_IAM_role.html Amazon ECS task execution IAM role> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'requiresCompatibilities' - The launch type the task requires. If no value is specified, it will default to @EC2@ . Valid values include @EC2@ and @FARGATE@ .
-- * 'pidMode' - The process namespace to use for the containers in the task. The valid values are @host@ or @task@ . If @host@ is specified, then all containers within the tasks that specified the @host@ PID mode on the same container instance share the same process namespace with the host Amazon EC2 instance. If @task@ is specified, all containers within the specified task share the same process namespace. If no value is specified, the default is a private namespace. For more information, see <https://docs.docker.com/engine/reference/run/#pid-settings---pid PID settings> in the /Docker run reference/ .
--
-- If the @host@ PID mode is used, be aware that there is a heightened risk of undesired process namespace expose. For more information, see <https://docs.docker.com/engine/security/security/ Docker security> .
-- * 'family' - The name of a family that this task definition is registered to. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
--
-- A family groups multiple versions of a task definition. Amazon ECS gives the first task definition that you registered to a family a revision number of 1. Amazon ECS gives sequential revision numbers to each task definition that you add.
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
-- * 'containerDefinitions' - A list of container definitions in JSON format that describe the different containers that make up your task. For more information about container definition parameters and defaults, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Amazon ECS Task Definitions> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'memory' - The amount (in MiB) of memory used by the task.
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
-- * 'proxyConfiguration' - The configuration details for the App Mesh proxy.
--
-- Your Amazon ECS container instances require at least version 1.26.0 of the container agent and at least version 1.26.0-1 of the @ecs-init@ package to enable a proxy configuration. If your container instances are launched from the Amazon ECS-optimized AMI version @20190301@ or later, then they contain the required versions of the container agent and @ecs-init@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'taskRoleARN' - The short name or full Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants containers in the task permission to call AWS APIs on your behalf. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html Amazon ECS Task Role> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- IAM roles for tasks on Windows require that the @-EnableTaskIAMRole@ option is set when you launch the Amazon ECS-optimized Windows AMI. Your containers must also run some configuration code in order to take advantage of the feature. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/windows_task_IAM_roles.html Windows IAM Roles for Tasks> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'placementConstraints' - An array of placement constraint objects to use for tasks. This field is not valid if you are using the Fargate launch type for your task.
-- * 'networkMode' - The Docker networking mode to use for the containers in the task. The valid values are @none@ , @bridge@ , @awsvpc@ , and @host@ . If no network mode is specified, the default is @bridge@ .
--
-- For Amazon ECS tasks on Fargate, the @awsvpc@ network mode is required. For Amazon ECS tasks on Amazon EC2 instances, any network mode can be used. If the network mode is set to @none@ , you cannot specify port mappings in your container definitions, and the tasks containers do not have external connectivity. The @host@ and @awsvpc@ network modes offer the highest networking performance for containers because they use the EC2 network stack instead of the virtualized network stack provided by the @bridge@ mode.
-- With the @host@ and @awsvpc@ network modes, exposed container ports are mapped directly to the corresponding host port (for the @host@ network mode) or the attached elastic network interface port (for the @awsvpc@ network mode), so you cannot take advantage of dynamic host port mappings.
-- /Important:/ When using the @host@ network mode, you should not run containers using the root user (UID 0). It is considered best practice to use a non-root user.
-- If the network mode is @awsvpc@ , the task is allocated an elastic network interface, and you must specify a 'NetworkConfiguration' value when you create a service or run a task with the task definition. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking> in the /Amazon Elastic Container Service Developer Guide/ .
-- If the network mode is @host@ , you cannot run multiple instantiations of the same task on a single container instance when port mappings are used.
-- Docker for Windows uses different network modes than Docker for Linux. When you register a task definition with Windows containers, you must not specify a network mode. If you use the console to register a task definition with Windows containers, you must choose the @<default>@ network mode object.
-- For more information, see <https://docs.docker.com/engine/reference/run/#network-settings Network settings> in the /Docker run reference/ .
-- * 'taskDefinitionARN' - The full Amazon Resource Name (ARN) of the task definition.
-- * 'compatibilities' - The launch type to use with your task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'revision' - The revision of the task in a particular family. The revision is a version number of a task definition in a family. When you register a task definition for the first time, the revision is @1@ . Each time that you register a new revision of a task definition in the same family, the revision value always increases by one, even if you have deregistered previous revisions in this family.
-- * 'volumes' - The list of volume definitions for the task.
--
-- If your tasks are using the Fargate launch type, the @host@ and @sourcePath@ parameters are not supported.
-- For more information about volume definition parameters and defaults, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_definitions.html Amazon ECS Task Definitions> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'cpu' - The number of @cpu@ units used by the task. If you are using the EC2 launch type, this field is optional and any value can be used. If you are using the Fargate launch type, this field is required and you must use one of the following values, which determines your range of valid values for the @memory@ parameter:
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
-- * 'requiresAttributes' - The container instance attributes required by your task. This field is not valid if you are using the Fargate launch type for your task.
mkTaskDefinition ::
  TaskDefinition
mkTaskDefinition =
  TaskDefinition'
    { status = Lude.Nothing,
      inferenceAccelerators = Lude.Nothing,
      executionRoleARN = Lude.Nothing,
      requiresCompatibilities = Lude.Nothing,
      pidMode = Lude.Nothing,
      family = Lude.Nothing,
      ipcMode = Lude.Nothing,
      containerDefinitions = Lude.Nothing,
      memory = Lude.Nothing,
      proxyConfiguration = Lude.Nothing,
      taskRoleARN = Lude.Nothing,
      placementConstraints = Lude.Nothing,
      networkMode = Lude.Nothing,
      taskDefinitionARN = Lude.Nothing,
      compatibilities = Lude.Nothing,
      revision = Lude.Nothing,
      volumes = Lude.Nothing,
      cpu = Lude.Nothing,
      requiresAttributes = Lude.Nothing
    }

-- | The status of the task definition.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdStatus :: Lens.Lens' TaskDefinition (Lude.Maybe TaskDefinitionStatus)
tdStatus = Lens.lens (status :: TaskDefinition -> Lude.Maybe TaskDefinitionStatus) (\s a -> s {status = a} :: TaskDefinition)
{-# DEPRECATED tdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Elastic Inference accelerator associated with the task.
--
-- /Note:/ Consider using 'inferenceAccelerators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdInferenceAccelerators :: Lens.Lens' TaskDefinition (Lude.Maybe [InferenceAccelerator])
tdInferenceAccelerators = Lens.lens (inferenceAccelerators :: TaskDefinition -> Lude.Maybe [InferenceAccelerator]) (\s a -> s {inferenceAccelerators = a} :: TaskDefinition)
{-# DEPRECATED tdInferenceAccelerators "Use generic-lens or generic-optics with 'inferenceAccelerators' instead." #-}

-- | The Amazon Resource Name (ARN) of the task execution role that grants the Amazon ECS container agent permission to make AWS API calls on your behalf. The task execution IAM role is required depending on the requirements of your task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_execution_IAM_role.html Amazon ECS task execution IAM role> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'executionRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdExecutionRoleARN :: Lens.Lens' TaskDefinition (Lude.Maybe Lude.Text)
tdExecutionRoleARN = Lens.lens (executionRoleARN :: TaskDefinition -> Lude.Maybe Lude.Text) (\s a -> s {executionRoleARN = a} :: TaskDefinition)
{-# DEPRECATED tdExecutionRoleARN "Use generic-lens or generic-optics with 'executionRoleARN' instead." #-}

-- | The launch type the task requires. If no value is specified, it will default to @EC2@ . Valid values include @EC2@ and @FARGATE@ .
--
-- /Note:/ Consider using 'requiresCompatibilities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdRequiresCompatibilities :: Lens.Lens' TaskDefinition (Lude.Maybe [Compatibility])
tdRequiresCompatibilities = Lens.lens (requiresCompatibilities :: TaskDefinition -> Lude.Maybe [Compatibility]) (\s a -> s {requiresCompatibilities = a} :: TaskDefinition)
{-# DEPRECATED tdRequiresCompatibilities "Use generic-lens or generic-optics with 'requiresCompatibilities' instead." #-}

-- | The process namespace to use for the containers in the task. The valid values are @host@ or @task@ . If @host@ is specified, then all containers within the tasks that specified the @host@ PID mode on the same container instance share the same process namespace with the host Amazon EC2 instance. If @task@ is specified, all containers within the specified task share the same process namespace. If no value is specified, the default is a private namespace. For more information, see <https://docs.docker.com/engine/reference/run/#pid-settings---pid PID settings> in the /Docker run reference/ .
--
-- If the @host@ PID mode is used, be aware that there is a heightened risk of undesired process namespace expose. For more information, see <https://docs.docker.com/engine/security/security/ Docker security> .
--
-- /Note:/ Consider using 'pidMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdPidMode :: Lens.Lens' TaskDefinition (Lude.Maybe PidMode)
tdPidMode = Lens.lens (pidMode :: TaskDefinition -> Lude.Maybe PidMode) (\s a -> s {pidMode = a} :: TaskDefinition)
{-# DEPRECATED tdPidMode "Use generic-lens or generic-optics with 'pidMode' instead." #-}

-- | The name of a family that this task definition is registered to. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
--
-- A family groups multiple versions of a task definition. Amazon ECS gives the first task definition that you registered to a family a revision number of 1. Amazon ECS gives sequential revision numbers to each task definition that you add.
--
-- /Note:/ Consider using 'family' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdFamily :: Lens.Lens' TaskDefinition (Lude.Maybe Lude.Text)
tdFamily = Lens.lens (family :: TaskDefinition -> Lude.Maybe Lude.Text) (\s a -> s {family = a} :: TaskDefinition)
{-# DEPRECATED tdFamily "Use generic-lens or generic-optics with 'family' instead." #-}

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
tdIpcMode :: Lens.Lens' TaskDefinition (Lude.Maybe IPcMode)
tdIpcMode = Lens.lens (ipcMode :: TaskDefinition -> Lude.Maybe IPcMode) (\s a -> s {ipcMode = a} :: TaskDefinition)
{-# DEPRECATED tdIpcMode "Use generic-lens or generic-optics with 'ipcMode' instead." #-}

-- | A list of container definitions in JSON format that describe the different containers that make up your task. For more information about container definition parameters and defaults, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Amazon ECS Task Definitions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'containerDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdContainerDefinitions :: Lens.Lens' TaskDefinition (Lude.Maybe [ContainerDefinition])
tdContainerDefinitions = Lens.lens (containerDefinitions :: TaskDefinition -> Lude.Maybe [ContainerDefinition]) (\s a -> s {containerDefinitions = a} :: TaskDefinition)
{-# DEPRECATED tdContainerDefinitions "Use generic-lens or generic-optics with 'containerDefinitions' instead." #-}

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
tdMemory :: Lens.Lens' TaskDefinition (Lude.Maybe Lude.Text)
tdMemory = Lens.lens (memory :: TaskDefinition -> Lude.Maybe Lude.Text) (\s a -> s {memory = a} :: TaskDefinition)
{-# DEPRECATED tdMemory "Use generic-lens or generic-optics with 'memory' instead." #-}

-- | The configuration details for the App Mesh proxy.
--
-- Your Amazon ECS container instances require at least version 1.26.0 of the container agent and at least version 1.26.0-1 of the @ecs-init@ package to enable a proxy configuration. If your container instances are launched from the Amazon ECS-optimized AMI version @20190301@ or later, then they contain the required versions of the container agent and @ecs-init@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'proxyConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdProxyConfiguration :: Lens.Lens' TaskDefinition (Lude.Maybe ProxyConfiguration)
tdProxyConfiguration = Lens.lens (proxyConfiguration :: TaskDefinition -> Lude.Maybe ProxyConfiguration) (\s a -> s {proxyConfiguration = a} :: TaskDefinition)
{-# DEPRECATED tdProxyConfiguration "Use generic-lens or generic-optics with 'proxyConfiguration' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants containers in the task permission to call AWS APIs on your behalf. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html Amazon ECS Task Role> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- IAM roles for tasks on Windows require that the @-EnableTaskIAMRole@ option is set when you launch the Amazon ECS-optimized Windows AMI. Your containers must also run some configuration code in order to take advantage of the feature. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/windows_task_IAM_roles.html Windows IAM Roles for Tasks> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'taskRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdTaskRoleARN :: Lens.Lens' TaskDefinition (Lude.Maybe Lude.Text)
tdTaskRoleARN = Lens.lens (taskRoleARN :: TaskDefinition -> Lude.Maybe Lude.Text) (\s a -> s {taskRoleARN = a} :: TaskDefinition)
{-# DEPRECATED tdTaskRoleARN "Use generic-lens or generic-optics with 'taskRoleARN' instead." #-}

-- | An array of placement constraint objects to use for tasks. This field is not valid if you are using the Fargate launch type for your task.
--
-- /Note:/ Consider using 'placementConstraints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdPlacementConstraints :: Lens.Lens' TaskDefinition (Lude.Maybe [TaskDefinitionPlacementConstraint])
tdPlacementConstraints = Lens.lens (placementConstraints :: TaskDefinition -> Lude.Maybe [TaskDefinitionPlacementConstraint]) (\s a -> s {placementConstraints = a} :: TaskDefinition)
{-# DEPRECATED tdPlacementConstraints "Use generic-lens or generic-optics with 'placementConstraints' instead." #-}

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
tdNetworkMode :: Lens.Lens' TaskDefinition (Lude.Maybe NetworkMode)
tdNetworkMode = Lens.lens (networkMode :: TaskDefinition -> Lude.Maybe NetworkMode) (\s a -> s {networkMode = a} :: TaskDefinition)
{-# DEPRECATED tdNetworkMode "Use generic-lens or generic-optics with 'networkMode' instead." #-}

-- | The full Amazon Resource Name (ARN) of the task definition.
--
-- /Note:/ Consider using 'taskDefinitionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdTaskDefinitionARN :: Lens.Lens' TaskDefinition (Lude.Maybe Lude.Text)
tdTaskDefinitionARN = Lens.lens (taskDefinitionARN :: TaskDefinition -> Lude.Maybe Lude.Text) (\s a -> s {taskDefinitionARN = a} :: TaskDefinition)
{-# DEPRECATED tdTaskDefinitionARN "Use generic-lens or generic-optics with 'taskDefinitionARN' instead." #-}

-- | The launch type to use with your task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'compatibilities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdCompatibilities :: Lens.Lens' TaskDefinition (Lude.Maybe [Compatibility])
tdCompatibilities = Lens.lens (compatibilities :: TaskDefinition -> Lude.Maybe [Compatibility]) (\s a -> s {compatibilities = a} :: TaskDefinition)
{-# DEPRECATED tdCompatibilities "Use generic-lens or generic-optics with 'compatibilities' instead." #-}

-- | The revision of the task in a particular family. The revision is a version number of a task definition in a family. When you register a task definition for the first time, the revision is @1@ . Each time that you register a new revision of a task definition in the same family, the revision value always increases by one, even if you have deregistered previous revisions in this family.
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdRevision :: Lens.Lens' TaskDefinition (Lude.Maybe Lude.Int)
tdRevision = Lens.lens (revision :: TaskDefinition -> Lude.Maybe Lude.Int) (\s a -> s {revision = a} :: TaskDefinition)
{-# DEPRECATED tdRevision "Use generic-lens or generic-optics with 'revision' instead." #-}

-- | The list of volume definitions for the task.
--
-- If your tasks are using the Fargate launch type, the @host@ and @sourcePath@ parameters are not supported.
-- For more information about volume definition parameters and defaults, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_definitions.html Amazon ECS Task Definitions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'volumes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdVolumes :: Lens.Lens' TaskDefinition (Lude.Maybe [Volume])
tdVolumes = Lens.lens (volumes :: TaskDefinition -> Lude.Maybe [Volume]) (\s a -> s {volumes = a} :: TaskDefinition)
{-# DEPRECATED tdVolumes "Use generic-lens or generic-optics with 'volumes' instead." #-}

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
tdCpu :: Lens.Lens' TaskDefinition (Lude.Maybe Lude.Text)
tdCpu = Lens.lens (cpu :: TaskDefinition -> Lude.Maybe Lude.Text) (\s a -> s {cpu = a} :: TaskDefinition)
{-# DEPRECATED tdCpu "Use generic-lens or generic-optics with 'cpu' instead." #-}

-- | The container instance attributes required by your task. This field is not valid if you are using the Fargate launch type for your task.
--
-- /Note:/ Consider using 'requiresAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdRequiresAttributes :: Lens.Lens' TaskDefinition (Lude.Maybe [Attribute])
tdRequiresAttributes = Lens.lens (requiresAttributes :: TaskDefinition -> Lude.Maybe [Attribute]) (\s a -> s {requiresAttributes = a} :: TaskDefinition)
{-# DEPRECATED tdRequiresAttributes "Use generic-lens or generic-optics with 'requiresAttributes' instead." #-}

instance Lude.FromJSON TaskDefinition where
  parseJSON =
    Lude.withObject
      "TaskDefinition"
      ( \x ->
          TaskDefinition'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "inferenceAccelerators" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "executionRoleArn")
            Lude.<*> (x Lude..:? "requiresCompatibilities" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "pidMode")
            Lude.<*> (x Lude..:? "family")
            Lude.<*> (x Lude..:? "ipcMode")
            Lude.<*> (x Lude..:? "containerDefinitions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "memory")
            Lude.<*> (x Lude..:? "proxyConfiguration")
            Lude.<*> (x Lude..:? "taskRoleArn")
            Lude.<*> (x Lude..:? "placementConstraints" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "networkMode")
            Lude.<*> (x Lude..:? "taskDefinitionArn")
            Lude.<*> (x Lude..:? "compatibilities" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "revision")
            Lude.<*> (x Lude..:? "volumes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "cpu")
            Lude.<*> (x Lude..:? "requiresAttributes" Lude..!= Lude.mempty)
      )
