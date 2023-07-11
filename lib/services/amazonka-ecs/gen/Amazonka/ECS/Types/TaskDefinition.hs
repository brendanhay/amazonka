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
-- Module      : Amazonka.ECS.Types.TaskDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.TaskDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.Attribute
import Amazonka.ECS.Types.Compatibility
import Amazonka.ECS.Types.ContainerDefinition
import Amazonka.ECS.Types.EphemeralStorage
import Amazonka.ECS.Types.InferenceAccelerator
import Amazonka.ECS.Types.IpcMode
import Amazonka.ECS.Types.NetworkMode
import Amazonka.ECS.Types.PidMode
import Amazonka.ECS.Types.ProxyConfiguration
import Amazonka.ECS.Types.RuntimePlatform
import Amazonka.ECS.Types.TaskDefinitionPlacementConstraint
import Amazonka.ECS.Types.TaskDefinitionStatus
import Amazonka.ECS.Types.Volume
import qualified Amazonka.Prelude as Prelude

-- | The details of a task definition which describes the container and
-- volume definitions of an Amazon Elastic Container Service task. You can
-- specify which Docker images to use, the required resources, and other
-- configurations related to launching the task definition through an
-- Amazon ECS service or task.
--
-- /See:/ 'newTaskDefinition' smart constructor.
data TaskDefinition = TaskDefinition'
  { -- | The task launch types the task definition validated against during task
    -- definition registration. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS launch types>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    compatibilities :: Prelude.Maybe [Compatibility],
    -- | A list of container definitions in JSON format that describe the
    -- different containers that make up your task. For more information about
    -- container definition parameters and defaults, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Amazon ECS Task Definitions>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    containerDefinitions :: Prelude.Maybe [ContainerDefinition],
    -- | The number of @cpu@ units used by the task. If you use the EC2 launch
    -- type, this field is optional. Any value can be used. If you use the
    -- Fargate launch type, this field is required. You must use one of the
    -- following values. The value that you choose determines your range of
    -- valid values for the @memory@ parameter.
    --
    -- The CPU units cannot be less than 1 vCPU when you use Windows containers
    -- on Fargate.
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
    -- -   2048 (2 vCPU) - Available @memory@ values: 4096 (4 GB) and 16384 (16
    --     GB) in increments of 1024 (1 GB)
    --
    -- -   4096 (4 vCPU) - Available @memory@ values: 8192 (8 GB) and 30720 (30
    --     GB) in increments of 1024 (1 GB)
    --
    -- -   8192 (8 vCPU) - Available @memory@ values: 16 GB and 60 GB in 4 GB
    --     increments
    --
    --     This option requires Linux platform @1.4.0@ or later.
    --
    -- -   16384 (16vCPU) - Available @memory@ values: 32GB and 120 GB in 8 GB
    --     increments
    --
    --     This option requires Linux platform @1.4.0@ or later.
    cpu :: Prelude.Maybe Prelude.Text,
    -- | The Unix timestamp for the time when the task definition was
    -- deregistered.
    deregisteredAt :: Prelude.Maybe Data.POSIX,
    -- | The ephemeral storage settings to use for tasks run with the task
    -- definition.
    ephemeralStorage :: Prelude.Maybe EphemeralStorage,
    -- | The Amazon Resource Name (ARN) of the task execution role that grants
    -- the Amazon ECS container agent permission to make Amazon Web Services
    -- API calls on your behalf. The task execution IAM role is required
    -- depending on the requirements of your task. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_execution_IAM_role.html Amazon ECS task execution IAM role>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    executionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of a family that this task definition is registered to. Up to
    -- 255 characters are allowed. Letters (both uppercase and lowercase
    -- letters), numbers, hyphens (-), and underscores (_) are allowed.
    --
    -- A family groups multiple versions of a task definition. Amazon ECS gives
    -- the first task definition that you registered to a family a revision
    -- number of 1. Amazon ECS gives sequential revision numbers to each task
    -- definition that you add.
    family :: Prelude.Maybe Prelude.Text,
    -- | The Elastic Inference accelerator that\'s associated with the task.
    inferenceAccelerators :: Prelude.Maybe [InferenceAccelerator],
    -- | The IPC resource namespace to use for the containers in the task. The
    -- valid values are @host@, @task@, or @none@. If @host@ is specified, then
    -- all containers within the tasks that specified the @host@ IPC mode on
    -- the same container instance share the same IPC resources with the host
    -- Amazon EC2 instance. If @task@ is specified, all containers within the
    -- specified task share the same IPC resources. If @none@ is specified,
    -- then IPC resources within the containers of a task are private and not
    -- shared with other containers in a task or on the container instance. If
    -- no value is specified, then the IPC resource namespace sharing depends
    -- on the Docker daemon setting on the container instance. For more
    -- information, see
    -- <https://docs.docker.com/engine/reference/run/#ipc-settings---ipc IPC settings>
    -- in the /Docker run reference/.
    --
    -- If the @host@ IPC mode is used, be aware that there is a heightened risk
    -- of undesired IPC namespace expose. For more information, see
    -- <https://docs.docker.com/engine/security/security/ Docker security>.
    --
    -- If you are setting namespaced kernel parameters using @systemControls@
    -- for the containers in the task, the following will apply to your IPC
    -- resource namespace. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_definition_parameters.html System Controls>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    --
    -- -   For tasks that use the @host@ IPC mode, IPC namespace related
    --     @systemControls@ are not supported.
    --
    -- -   For tasks that use the @task@ IPC mode, IPC namespace related
    --     @systemControls@ will apply to all containers within a task.
    --
    -- This parameter is not supported for Windows containers or tasks run on
    -- Fargate.
    ipcMode :: Prelude.Maybe IpcMode,
    -- | The amount (in MiB) of memory used by the task.
    --
    -- If your tasks runs on Amazon EC2 instances, you must specify either a
    -- task-level memory value or a container-level memory value. This field is
    -- optional and any value can be used. If a task-level memory value is
    -- specified, the container-level memory value is optional. For more
    -- information regarding container-level memory and memory reservation, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ContainerDefinition.html ContainerDefinition>.
    --
    -- If your tasks runs on Fargate, this field is required. You must use one
    -- of the following values. The value you choose determines your range of
    -- valid values for the @cpu@ parameter.
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
    -- -   Between 16 GB and 60 GB in 4 GB increments - Available @cpu@ values:
    --     8192 (8 vCPU)
    --
    --     This option requires Linux platform @1.4.0@ or later.
    --
    -- -   Between 32GB and 120 GB in 8 GB increments - Available @cpu@ values:
    --     16384 (16 vCPU)
    --
    --     This option requires Linux platform @1.4.0@ or later.
    memory :: Prelude.Maybe Prelude.Text,
    -- | The Docker networking mode to use for the containers in the task. The
    -- valid values are @none@, @bridge@, @awsvpc@, and @host@. If no network
    -- mode is specified, the default is @bridge@.
    --
    -- For Amazon ECS tasks on Fargate, the @awsvpc@ network mode is required.
    -- For Amazon ECS tasks on Amazon EC2 Linux instances, any network mode can
    -- be used. For Amazon ECS tasks on Amazon EC2 Windows instances,
    -- @\<default>@ or @awsvpc@ can be used. If the network mode is set to
    -- @none@, you cannot specify port mappings in your container definitions,
    -- and the tasks containers do not have external connectivity. The @host@
    -- and @awsvpc@ network modes offer the highest networking performance for
    -- containers because they use the EC2 network stack instead of the
    -- virtualized network stack provided by the @bridge@ mode.
    --
    -- With the @host@ and @awsvpc@ network modes, exposed container ports are
    -- mapped directly to the corresponding host port (for the @host@ network
    -- mode) or the attached elastic network interface port (for the @awsvpc@
    -- network mode), so you cannot take advantage of dynamic host port
    -- mappings.
    --
    -- When using the @host@ network mode, you should not run containers using
    -- the root user (UID 0). It is considered best practice to use a non-root
    -- user.
    --
    -- If the network mode is @awsvpc@, the task is allocated an elastic
    -- network interface, and you must specify a NetworkConfiguration value
    -- when you create a service or run a task with the task definition. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    --
    -- If the network mode is @host@, you cannot run multiple instantiations of
    -- the same task on a single container instance when port mappings are
    -- used.
    --
    -- For more information, see
    -- <https://docs.docker.com/engine/reference/run/#network-settings Network settings>
    -- in the /Docker run reference/.
    networkMode :: Prelude.Maybe NetworkMode,
    -- | The process namespace to use for the containers in the task. The valid
    -- values are @host@ or @task@. If @host@ is specified, then all containers
    -- within the tasks that specified the @host@ PID mode on the same
    -- container instance share the same process namespace with the host Amazon
    -- EC2 instance. If @task@ is specified, all containers within the
    -- specified task share the same process namespace. If no value is
    -- specified, the default is a private namespace. For more information, see
    -- <https://docs.docker.com/engine/reference/run/#pid-settings---pid PID settings>
    -- in the /Docker run reference/.
    --
    -- If the @host@ PID mode is used, be aware that there is a heightened risk
    -- of undesired process namespace expose. For more information, see
    -- <https://docs.docker.com/engine/security/security/ Docker security>.
    --
    -- This parameter is not supported for Windows containers or tasks run on
    -- Fargate.
    pidMode :: Prelude.Maybe PidMode,
    -- | An array of placement constraint objects to use for tasks.
    --
    -- This parameter isn\'t supported for tasks run on Fargate.
    placementConstraints :: Prelude.Maybe [TaskDefinitionPlacementConstraint],
    -- | The configuration details for the App Mesh proxy.
    --
    -- Your Amazon ECS container instances require at least version 1.26.0 of
    -- the container agent and at least version 1.26.0-1 of the @ecs-init@
    -- package to use a proxy configuration. If your container instances are
    -- launched from the Amazon ECS optimized AMI version @20190301@ or later,
    -- they contain the required versions of the container agent and
    -- @ecs-init@. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    proxyConfiguration :: Prelude.Maybe ProxyConfiguration,
    -- | The Unix timestamp for the time when the task definition was registered.
    registeredAt :: Prelude.Maybe Data.POSIX,
    -- | The principal that registered the task definition.
    registeredBy :: Prelude.Maybe Prelude.Text,
    -- | The container instance attributes required by your task. When an Amazon
    -- EC2 instance is registered to your cluster, the Amazon ECS container
    -- agent assigns some standard attributes to the instance. You can apply
    -- custom attributes. These are specified as key-value pairs using the
    -- Amazon ECS console or the PutAttributes API. These attributes are used
    -- when determining task placement for tasks hosted on Amazon EC2
    -- instances. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html#attributes Attributes>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    --
    -- This parameter isn\'t supported for tasks run on Fargate.
    requiresAttributes :: Prelude.Maybe [Attribute],
    -- | The task launch types the task definition was validated against. To
    -- determine which task launch types the task definition is validated for,
    -- see the TaskDefinition$compatibilities parameter.
    requiresCompatibilities :: Prelude.Maybe [Compatibility],
    -- | The revision of the task in a particular family. The revision is a
    -- version number of a task definition in a family. When you register a
    -- task definition for the first time, the revision is @1@. Each time that
    -- you register a new revision of a task definition in the same family, the
    -- revision value always increases by one. This is even if you deregistered
    -- previous revisions in this family.
    revision :: Prelude.Maybe Prelude.Int,
    -- | The operating system that your task definitions are running on. A
    -- platform family is specified only for tasks using the Fargate launch
    -- type.
    --
    -- When you specify a task in a service, this value must match the
    -- @runtimePlatform@ value of the service.
    runtimePlatform :: Prelude.Maybe RuntimePlatform,
    -- | The status of the task definition.
    status :: Prelude.Maybe TaskDefinitionStatus,
    -- | The full Amazon Resource Name (ARN) of the task definition.
    taskDefinitionArn :: Prelude.Maybe Prelude.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the Identity and
    -- Access Management role that grants containers in the task permission to
    -- call Amazon Web Services APIs on your behalf. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html Amazon ECS Task Role>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    --
    -- IAM roles for tasks on Windows require that the @-EnableTaskIAMRole@
    -- option is set when you launch the Amazon ECS-optimized Windows AMI. Your
    -- containers must also run some configuration code to use the feature. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/windows_task_IAM_roles.html Windows IAM roles for tasks>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    taskRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The list of data volume definitions for the task. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_data_volumes.html Using data volumes in tasks>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    --
    -- The @host@ and @sourcePath@ parameters aren\'t supported for tasks run
    -- on Fargate.
    volumes :: Prelude.Maybe [Volume]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TaskDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compatibilities', 'taskDefinition_compatibilities' - The task launch types the task definition validated against during task
-- definition registration. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS launch types>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'containerDefinitions', 'taskDefinition_containerDefinitions' - A list of container definitions in JSON format that describe the
-- different containers that make up your task. For more information about
-- container definition parameters and defaults, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Amazon ECS Task Definitions>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'cpu', 'taskDefinition_cpu' - The number of @cpu@ units used by the task. If you use the EC2 launch
-- type, this field is optional. Any value can be used. If you use the
-- Fargate launch type, this field is required. You must use one of the
-- following values. The value that you choose determines your range of
-- valid values for the @memory@ parameter.
--
-- The CPU units cannot be less than 1 vCPU when you use Windows containers
-- on Fargate.
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
-- -   2048 (2 vCPU) - Available @memory@ values: 4096 (4 GB) and 16384 (16
--     GB) in increments of 1024 (1 GB)
--
-- -   4096 (4 vCPU) - Available @memory@ values: 8192 (8 GB) and 30720 (30
--     GB) in increments of 1024 (1 GB)
--
-- -   8192 (8 vCPU) - Available @memory@ values: 16 GB and 60 GB in 4 GB
--     increments
--
--     This option requires Linux platform @1.4.0@ or later.
--
-- -   16384 (16vCPU) - Available @memory@ values: 32GB and 120 GB in 8 GB
--     increments
--
--     This option requires Linux platform @1.4.0@ or later.
--
-- 'deregisteredAt', 'taskDefinition_deregisteredAt' - The Unix timestamp for the time when the task definition was
-- deregistered.
--
-- 'ephemeralStorage', 'taskDefinition_ephemeralStorage' - The ephemeral storage settings to use for tasks run with the task
-- definition.
--
-- 'executionRoleArn', 'taskDefinition_executionRoleArn' - The Amazon Resource Name (ARN) of the task execution role that grants
-- the Amazon ECS container agent permission to make Amazon Web Services
-- API calls on your behalf. The task execution IAM role is required
-- depending on the requirements of your task. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_execution_IAM_role.html Amazon ECS task execution IAM role>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'family', 'taskDefinition_family' - The name of a family that this task definition is registered to. Up to
-- 255 characters are allowed. Letters (both uppercase and lowercase
-- letters), numbers, hyphens (-), and underscores (_) are allowed.
--
-- A family groups multiple versions of a task definition. Amazon ECS gives
-- the first task definition that you registered to a family a revision
-- number of 1. Amazon ECS gives sequential revision numbers to each task
-- definition that you add.
--
-- 'inferenceAccelerators', 'taskDefinition_inferenceAccelerators' - The Elastic Inference accelerator that\'s associated with the task.
--
-- 'ipcMode', 'taskDefinition_ipcMode' - The IPC resource namespace to use for the containers in the task. The
-- valid values are @host@, @task@, or @none@. If @host@ is specified, then
-- all containers within the tasks that specified the @host@ IPC mode on
-- the same container instance share the same IPC resources with the host
-- Amazon EC2 instance. If @task@ is specified, all containers within the
-- specified task share the same IPC resources. If @none@ is specified,
-- then IPC resources within the containers of a task are private and not
-- shared with other containers in a task or on the container instance. If
-- no value is specified, then the IPC resource namespace sharing depends
-- on the Docker daemon setting on the container instance. For more
-- information, see
-- <https://docs.docker.com/engine/reference/run/#ipc-settings---ipc IPC settings>
-- in the /Docker run reference/.
--
-- If the @host@ IPC mode is used, be aware that there is a heightened risk
-- of undesired IPC namespace expose. For more information, see
-- <https://docs.docker.com/engine/security/security/ Docker security>.
--
-- If you are setting namespaced kernel parameters using @systemControls@
-- for the containers in the task, the following will apply to your IPC
-- resource namespace. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_definition_parameters.html System Controls>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- -   For tasks that use the @host@ IPC mode, IPC namespace related
--     @systemControls@ are not supported.
--
-- -   For tasks that use the @task@ IPC mode, IPC namespace related
--     @systemControls@ will apply to all containers within a task.
--
-- This parameter is not supported for Windows containers or tasks run on
-- Fargate.
--
-- 'memory', 'taskDefinition_memory' - The amount (in MiB) of memory used by the task.
--
-- If your tasks runs on Amazon EC2 instances, you must specify either a
-- task-level memory value or a container-level memory value. This field is
-- optional and any value can be used. If a task-level memory value is
-- specified, the container-level memory value is optional. For more
-- information regarding container-level memory and memory reservation, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ContainerDefinition.html ContainerDefinition>.
--
-- If your tasks runs on Fargate, this field is required. You must use one
-- of the following values. The value you choose determines your range of
-- valid values for the @cpu@ parameter.
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
-- -   Between 16 GB and 60 GB in 4 GB increments - Available @cpu@ values:
--     8192 (8 vCPU)
--
--     This option requires Linux platform @1.4.0@ or later.
--
-- -   Between 32GB and 120 GB in 8 GB increments - Available @cpu@ values:
--     16384 (16 vCPU)
--
--     This option requires Linux platform @1.4.0@ or later.
--
-- 'networkMode', 'taskDefinition_networkMode' - The Docker networking mode to use for the containers in the task. The
-- valid values are @none@, @bridge@, @awsvpc@, and @host@. If no network
-- mode is specified, the default is @bridge@.
--
-- For Amazon ECS tasks on Fargate, the @awsvpc@ network mode is required.
-- For Amazon ECS tasks on Amazon EC2 Linux instances, any network mode can
-- be used. For Amazon ECS tasks on Amazon EC2 Windows instances,
-- @\<default>@ or @awsvpc@ can be used. If the network mode is set to
-- @none@, you cannot specify port mappings in your container definitions,
-- and the tasks containers do not have external connectivity. The @host@
-- and @awsvpc@ network modes offer the highest networking performance for
-- containers because they use the EC2 network stack instead of the
-- virtualized network stack provided by the @bridge@ mode.
--
-- With the @host@ and @awsvpc@ network modes, exposed container ports are
-- mapped directly to the corresponding host port (for the @host@ network
-- mode) or the attached elastic network interface port (for the @awsvpc@
-- network mode), so you cannot take advantage of dynamic host port
-- mappings.
--
-- When using the @host@ network mode, you should not run containers using
-- the root user (UID 0). It is considered best practice to use a non-root
-- user.
--
-- If the network mode is @awsvpc@, the task is allocated an elastic
-- network interface, and you must specify a NetworkConfiguration value
-- when you create a service or run a task with the task definition. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If the network mode is @host@, you cannot run multiple instantiations of
-- the same task on a single container instance when port mappings are
-- used.
--
-- For more information, see
-- <https://docs.docker.com/engine/reference/run/#network-settings Network settings>
-- in the /Docker run reference/.
--
-- 'pidMode', 'taskDefinition_pidMode' - The process namespace to use for the containers in the task. The valid
-- values are @host@ or @task@. If @host@ is specified, then all containers
-- within the tasks that specified the @host@ PID mode on the same
-- container instance share the same process namespace with the host Amazon
-- EC2 instance. If @task@ is specified, all containers within the
-- specified task share the same process namespace. If no value is
-- specified, the default is a private namespace. For more information, see
-- <https://docs.docker.com/engine/reference/run/#pid-settings---pid PID settings>
-- in the /Docker run reference/.
--
-- If the @host@ PID mode is used, be aware that there is a heightened risk
-- of undesired process namespace expose. For more information, see
-- <https://docs.docker.com/engine/security/security/ Docker security>.
--
-- This parameter is not supported for Windows containers or tasks run on
-- Fargate.
--
-- 'placementConstraints', 'taskDefinition_placementConstraints' - An array of placement constraint objects to use for tasks.
--
-- This parameter isn\'t supported for tasks run on Fargate.
--
-- 'proxyConfiguration', 'taskDefinition_proxyConfiguration' - The configuration details for the App Mesh proxy.
--
-- Your Amazon ECS container instances require at least version 1.26.0 of
-- the container agent and at least version 1.26.0-1 of the @ecs-init@
-- package to use a proxy configuration. If your container instances are
-- launched from the Amazon ECS optimized AMI version @20190301@ or later,
-- they contain the required versions of the container agent and
-- @ecs-init@. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'registeredAt', 'taskDefinition_registeredAt' - The Unix timestamp for the time when the task definition was registered.
--
-- 'registeredBy', 'taskDefinition_registeredBy' - The principal that registered the task definition.
--
-- 'requiresAttributes', 'taskDefinition_requiresAttributes' - The container instance attributes required by your task. When an Amazon
-- EC2 instance is registered to your cluster, the Amazon ECS container
-- agent assigns some standard attributes to the instance. You can apply
-- custom attributes. These are specified as key-value pairs using the
-- Amazon ECS console or the PutAttributes API. These attributes are used
-- when determining task placement for tasks hosted on Amazon EC2
-- instances. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html#attributes Attributes>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- This parameter isn\'t supported for tasks run on Fargate.
--
-- 'requiresCompatibilities', 'taskDefinition_requiresCompatibilities' - The task launch types the task definition was validated against. To
-- determine which task launch types the task definition is validated for,
-- see the TaskDefinition$compatibilities parameter.
--
-- 'revision', 'taskDefinition_revision' - The revision of the task in a particular family. The revision is a
-- version number of a task definition in a family. When you register a
-- task definition for the first time, the revision is @1@. Each time that
-- you register a new revision of a task definition in the same family, the
-- revision value always increases by one. This is even if you deregistered
-- previous revisions in this family.
--
-- 'runtimePlatform', 'taskDefinition_runtimePlatform' - The operating system that your task definitions are running on. A
-- platform family is specified only for tasks using the Fargate launch
-- type.
--
-- When you specify a task in a service, this value must match the
-- @runtimePlatform@ value of the service.
--
-- 'status', 'taskDefinition_status' - The status of the task definition.
--
-- 'taskDefinitionArn', 'taskDefinition_taskDefinitionArn' - The full Amazon Resource Name (ARN) of the task definition.
--
-- 'taskRoleArn', 'taskDefinition_taskRoleArn' - The short name or full Amazon Resource Name (ARN) of the Identity and
-- Access Management role that grants containers in the task permission to
-- call Amazon Web Services APIs on your behalf. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html Amazon ECS Task Role>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- IAM roles for tasks on Windows require that the @-EnableTaskIAMRole@
-- option is set when you launch the Amazon ECS-optimized Windows AMI. Your
-- containers must also run some configuration code to use the feature. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/windows_task_IAM_roles.html Windows IAM roles for tasks>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'volumes', 'taskDefinition_volumes' - The list of data volume definitions for the task. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_data_volumes.html Using data volumes in tasks>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- The @host@ and @sourcePath@ parameters aren\'t supported for tasks run
-- on Fargate.
newTaskDefinition ::
  TaskDefinition
newTaskDefinition =
  TaskDefinition'
    { compatibilities = Prelude.Nothing,
      containerDefinitions = Prelude.Nothing,
      cpu = Prelude.Nothing,
      deregisteredAt = Prelude.Nothing,
      ephemeralStorage = Prelude.Nothing,
      executionRoleArn = Prelude.Nothing,
      family = Prelude.Nothing,
      inferenceAccelerators = Prelude.Nothing,
      ipcMode = Prelude.Nothing,
      memory = Prelude.Nothing,
      networkMode = Prelude.Nothing,
      pidMode = Prelude.Nothing,
      placementConstraints = Prelude.Nothing,
      proxyConfiguration = Prelude.Nothing,
      registeredAt = Prelude.Nothing,
      registeredBy = Prelude.Nothing,
      requiresAttributes = Prelude.Nothing,
      requiresCompatibilities = Prelude.Nothing,
      revision = Prelude.Nothing,
      runtimePlatform = Prelude.Nothing,
      status = Prelude.Nothing,
      taskDefinitionArn = Prelude.Nothing,
      taskRoleArn = Prelude.Nothing,
      volumes = Prelude.Nothing
    }

-- | The task launch types the task definition validated against during task
-- definition registration. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS launch types>
-- in the /Amazon Elastic Container Service Developer Guide/.
taskDefinition_compatibilities :: Lens.Lens' TaskDefinition (Prelude.Maybe [Compatibility])
taskDefinition_compatibilities = Lens.lens (\TaskDefinition' {compatibilities} -> compatibilities) (\s@TaskDefinition' {} a -> s {compatibilities = a} :: TaskDefinition) Prelude.. Lens.mapping Lens.coerced

-- | A list of container definitions in JSON format that describe the
-- different containers that make up your task. For more information about
-- container definition parameters and defaults, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Amazon ECS Task Definitions>
-- in the /Amazon Elastic Container Service Developer Guide/.
taskDefinition_containerDefinitions :: Lens.Lens' TaskDefinition (Prelude.Maybe [ContainerDefinition])
taskDefinition_containerDefinitions = Lens.lens (\TaskDefinition' {containerDefinitions} -> containerDefinitions) (\s@TaskDefinition' {} a -> s {containerDefinitions = a} :: TaskDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The number of @cpu@ units used by the task. If you use the EC2 launch
-- type, this field is optional. Any value can be used. If you use the
-- Fargate launch type, this field is required. You must use one of the
-- following values. The value that you choose determines your range of
-- valid values for the @memory@ parameter.
--
-- The CPU units cannot be less than 1 vCPU when you use Windows containers
-- on Fargate.
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
-- -   2048 (2 vCPU) - Available @memory@ values: 4096 (4 GB) and 16384 (16
--     GB) in increments of 1024 (1 GB)
--
-- -   4096 (4 vCPU) - Available @memory@ values: 8192 (8 GB) and 30720 (30
--     GB) in increments of 1024 (1 GB)
--
-- -   8192 (8 vCPU) - Available @memory@ values: 16 GB and 60 GB in 4 GB
--     increments
--
--     This option requires Linux platform @1.4.0@ or later.
--
-- -   16384 (16vCPU) - Available @memory@ values: 32GB and 120 GB in 8 GB
--     increments
--
--     This option requires Linux platform @1.4.0@ or later.
taskDefinition_cpu :: Lens.Lens' TaskDefinition (Prelude.Maybe Prelude.Text)
taskDefinition_cpu = Lens.lens (\TaskDefinition' {cpu} -> cpu) (\s@TaskDefinition' {} a -> s {cpu = a} :: TaskDefinition)

-- | The Unix timestamp for the time when the task definition was
-- deregistered.
taskDefinition_deregisteredAt :: Lens.Lens' TaskDefinition (Prelude.Maybe Prelude.UTCTime)
taskDefinition_deregisteredAt = Lens.lens (\TaskDefinition' {deregisteredAt} -> deregisteredAt) (\s@TaskDefinition' {} a -> s {deregisteredAt = a} :: TaskDefinition) Prelude.. Lens.mapping Data._Time

-- | The ephemeral storage settings to use for tasks run with the task
-- definition.
taskDefinition_ephemeralStorage :: Lens.Lens' TaskDefinition (Prelude.Maybe EphemeralStorage)
taskDefinition_ephemeralStorage = Lens.lens (\TaskDefinition' {ephemeralStorage} -> ephemeralStorage) (\s@TaskDefinition' {} a -> s {ephemeralStorage = a} :: TaskDefinition)

-- | The Amazon Resource Name (ARN) of the task execution role that grants
-- the Amazon ECS container agent permission to make Amazon Web Services
-- API calls on your behalf. The task execution IAM role is required
-- depending on the requirements of your task. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_execution_IAM_role.html Amazon ECS task execution IAM role>
-- in the /Amazon Elastic Container Service Developer Guide/.
taskDefinition_executionRoleArn :: Lens.Lens' TaskDefinition (Prelude.Maybe Prelude.Text)
taskDefinition_executionRoleArn = Lens.lens (\TaskDefinition' {executionRoleArn} -> executionRoleArn) (\s@TaskDefinition' {} a -> s {executionRoleArn = a} :: TaskDefinition)

-- | The name of a family that this task definition is registered to. Up to
-- 255 characters are allowed. Letters (both uppercase and lowercase
-- letters), numbers, hyphens (-), and underscores (_) are allowed.
--
-- A family groups multiple versions of a task definition. Amazon ECS gives
-- the first task definition that you registered to a family a revision
-- number of 1. Amazon ECS gives sequential revision numbers to each task
-- definition that you add.
taskDefinition_family :: Lens.Lens' TaskDefinition (Prelude.Maybe Prelude.Text)
taskDefinition_family = Lens.lens (\TaskDefinition' {family} -> family) (\s@TaskDefinition' {} a -> s {family = a} :: TaskDefinition)

-- | The Elastic Inference accelerator that\'s associated with the task.
taskDefinition_inferenceAccelerators :: Lens.Lens' TaskDefinition (Prelude.Maybe [InferenceAccelerator])
taskDefinition_inferenceAccelerators = Lens.lens (\TaskDefinition' {inferenceAccelerators} -> inferenceAccelerators) (\s@TaskDefinition' {} a -> s {inferenceAccelerators = a} :: TaskDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The IPC resource namespace to use for the containers in the task. The
-- valid values are @host@, @task@, or @none@. If @host@ is specified, then
-- all containers within the tasks that specified the @host@ IPC mode on
-- the same container instance share the same IPC resources with the host
-- Amazon EC2 instance. If @task@ is specified, all containers within the
-- specified task share the same IPC resources. If @none@ is specified,
-- then IPC resources within the containers of a task are private and not
-- shared with other containers in a task or on the container instance. If
-- no value is specified, then the IPC resource namespace sharing depends
-- on the Docker daemon setting on the container instance. For more
-- information, see
-- <https://docs.docker.com/engine/reference/run/#ipc-settings---ipc IPC settings>
-- in the /Docker run reference/.
--
-- If the @host@ IPC mode is used, be aware that there is a heightened risk
-- of undesired IPC namespace expose. For more information, see
-- <https://docs.docker.com/engine/security/security/ Docker security>.
--
-- If you are setting namespaced kernel parameters using @systemControls@
-- for the containers in the task, the following will apply to your IPC
-- resource namespace. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_definition_parameters.html System Controls>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- -   For tasks that use the @host@ IPC mode, IPC namespace related
--     @systemControls@ are not supported.
--
-- -   For tasks that use the @task@ IPC mode, IPC namespace related
--     @systemControls@ will apply to all containers within a task.
--
-- This parameter is not supported for Windows containers or tasks run on
-- Fargate.
taskDefinition_ipcMode :: Lens.Lens' TaskDefinition (Prelude.Maybe IpcMode)
taskDefinition_ipcMode = Lens.lens (\TaskDefinition' {ipcMode} -> ipcMode) (\s@TaskDefinition' {} a -> s {ipcMode = a} :: TaskDefinition)

-- | The amount (in MiB) of memory used by the task.
--
-- If your tasks runs on Amazon EC2 instances, you must specify either a
-- task-level memory value or a container-level memory value. This field is
-- optional and any value can be used. If a task-level memory value is
-- specified, the container-level memory value is optional. For more
-- information regarding container-level memory and memory reservation, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ContainerDefinition.html ContainerDefinition>.
--
-- If your tasks runs on Fargate, this field is required. You must use one
-- of the following values. The value you choose determines your range of
-- valid values for the @cpu@ parameter.
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
-- -   Between 16 GB and 60 GB in 4 GB increments - Available @cpu@ values:
--     8192 (8 vCPU)
--
--     This option requires Linux platform @1.4.0@ or later.
--
-- -   Between 32GB and 120 GB in 8 GB increments - Available @cpu@ values:
--     16384 (16 vCPU)
--
--     This option requires Linux platform @1.4.0@ or later.
taskDefinition_memory :: Lens.Lens' TaskDefinition (Prelude.Maybe Prelude.Text)
taskDefinition_memory = Lens.lens (\TaskDefinition' {memory} -> memory) (\s@TaskDefinition' {} a -> s {memory = a} :: TaskDefinition)

-- | The Docker networking mode to use for the containers in the task. The
-- valid values are @none@, @bridge@, @awsvpc@, and @host@. If no network
-- mode is specified, the default is @bridge@.
--
-- For Amazon ECS tasks on Fargate, the @awsvpc@ network mode is required.
-- For Amazon ECS tasks on Amazon EC2 Linux instances, any network mode can
-- be used. For Amazon ECS tasks on Amazon EC2 Windows instances,
-- @\<default>@ or @awsvpc@ can be used. If the network mode is set to
-- @none@, you cannot specify port mappings in your container definitions,
-- and the tasks containers do not have external connectivity. The @host@
-- and @awsvpc@ network modes offer the highest networking performance for
-- containers because they use the EC2 network stack instead of the
-- virtualized network stack provided by the @bridge@ mode.
--
-- With the @host@ and @awsvpc@ network modes, exposed container ports are
-- mapped directly to the corresponding host port (for the @host@ network
-- mode) or the attached elastic network interface port (for the @awsvpc@
-- network mode), so you cannot take advantage of dynamic host port
-- mappings.
--
-- When using the @host@ network mode, you should not run containers using
-- the root user (UID 0). It is considered best practice to use a non-root
-- user.
--
-- If the network mode is @awsvpc@, the task is allocated an elastic
-- network interface, and you must specify a NetworkConfiguration value
-- when you create a service or run a task with the task definition. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If the network mode is @host@, you cannot run multiple instantiations of
-- the same task on a single container instance when port mappings are
-- used.
--
-- For more information, see
-- <https://docs.docker.com/engine/reference/run/#network-settings Network settings>
-- in the /Docker run reference/.
taskDefinition_networkMode :: Lens.Lens' TaskDefinition (Prelude.Maybe NetworkMode)
taskDefinition_networkMode = Lens.lens (\TaskDefinition' {networkMode} -> networkMode) (\s@TaskDefinition' {} a -> s {networkMode = a} :: TaskDefinition)

-- | The process namespace to use for the containers in the task. The valid
-- values are @host@ or @task@. If @host@ is specified, then all containers
-- within the tasks that specified the @host@ PID mode on the same
-- container instance share the same process namespace with the host Amazon
-- EC2 instance. If @task@ is specified, all containers within the
-- specified task share the same process namespace. If no value is
-- specified, the default is a private namespace. For more information, see
-- <https://docs.docker.com/engine/reference/run/#pid-settings---pid PID settings>
-- in the /Docker run reference/.
--
-- If the @host@ PID mode is used, be aware that there is a heightened risk
-- of undesired process namespace expose. For more information, see
-- <https://docs.docker.com/engine/security/security/ Docker security>.
--
-- This parameter is not supported for Windows containers or tasks run on
-- Fargate.
taskDefinition_pidMode :: Lens.Lens' TaskDefinition (Prelude.Maybe PidMode)
taskDefinition_pidMode = Lens.lens (\TaskDefinition' {pidMode} -> pidMode) (\s@TaskDefinition' {} a -> s {pidMode = a} :: TaskDefinition)

-- | An array of placement constraint objects to use for tasks.
--
-- This parameter isn\'t supported for tasks run on Fargate.
taskDefinition_placementConstraints :: Lens.Lens' TaskDefinition (Prelude.Maybe [TaskDefinitionPlacementConstraint])
taskDefinition_placementConstraints = Lens.lens (\TaskDefinition' {placementConstraints} -> placementConstraints) (\s@TaskDefinition' {} a -> s {placementConstraints = a} :: TaskDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The configuration details for the App Mesh proxy.
--
-- Your Amazon ECS container instances require at least version 1.26.0 of
-- the container agent and at least version 1.26.0-1 of the @ecs-init@
-- package to use a proxy configuration. If your container instances are
-- launched from the Amazon ECS optimized AMI version @20190301@ or later,
-- they contain the required versions of the container agent and
-- @ecs-init@. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI>
-- in the /Amazon Elastic Container Service Developer Guide/.
taskDefinition_proxyConfiguration :: Lens.Lens' TaskDefinition (Prelude.Maybe ProxyConfiguration)
taskDefinition_proxyConfiguration = Lens.lens (\TaskDefinition' {proxyConfiguration} -> proxyConfiguration) (\s@TaskDefinition' {} a -> s {proxyConfiguration = a} :: TaskDefinition)

-- | The Unix timestamp for the time when the task definition was registered.
taskDefinition_registeredAt :: Lens.Lens' TaskDefinition (Prelude.Maybe Prelude.UTCTime)
taskDefinition_registeredAt = Lens.lens (\TaskDefinition' {registeredAt} -> registeredAt) (\s@TaskDefinition' {} a -> s {registeredAt = a} :: TaskDefinition) Prelude.. Lens.mapping Data._Time

-- | The principal that registered the task definition.
taskDefinition_registeredBy :: Lens.Lens' TaskDefinition (Prelude.Maybe Prelude.Text)
taskDefinition_registeredBy = Lens.lens (\TaskDefinition' {registeredBy} -> registeredBy) (\s@TaskDefinition' {} a -> s {registeredBy = a} :: TaskDefinition)

-- | The container instance attributes required by your task. When an Amazon
-- EC2 instance is registered to your cluster, the Amazon ECS container
-- agent assigns some standard attributes to the instance. You can apply
-- custom attributes. These are specified as key-value pairs using the
-- Amazon ECS console or the PutAttributes API. These attributes are used
-- when determining task placement for tasks hosted on Amazon EC2
-- instances. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html#attributes Attributes>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- This parameter isn\'t supported for tasks run on Fargate.
taskDefinition_requiresAttributes :: Lens.Lens' TaskDefinition (Prelude.Maybe [Attribute])
taskDefinition_requiresAttributes = Lens.lens (\TaskDefinition' {requiresAttributes} -> requiresAttributes) (\s@TaskDefinition' {} a -> s {requiresAttributes = a} :: TaskDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The task launch types the task definition was validated against. To
-- determine which task launch types the task definition is validated for,
-- see the TaskDefinition$compatibilities parameter.
taskDefinition_requiresCompatibilities :: Lens.Lens' TaskDefinition (Prelude.Maybe [Compatibility])
taskDefinition_requiresCompatibilities = Lens.lens (\TaskDefinition' {requiresCompatibilities} -> requiresCompatibilities) (\s@TaskDefinition' {} a -> s {requiresCompatibilities = a} :: TaskDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The revision of the task in a particular family. The revision is a
-- version number of a task definition in a family. When you register a
-- task definition for the first time, the revision is @1@. Each time that
-- you register a new revision of a task definition in the same family, the
-- revision value always increases by one. This is even if you deregistered
-- previous revisions in this family.
taskDefinition_revision :: Lens.Lens' TaskDefinition (Prelude.Maybe Prelude.Int)
taskDefinition_revision = Lens.lens (\TaskDefinition' {revision} -> revision) (\s@TaskDefinition' {} a -> s {revision = a} :: TaskDefinition)

-- | The operating system that your task definitions are running on. A
-- platform family is specified only for tasks using the Fargate launch
-- type.
--
-- When you specify a task in a service, this value must match the
-- @runtimePlatform@ value of the service.
taskDefinition_runtimePlatform :: Lens.Lens' TaskDefinition (Prelude.Maybe RuntimePlatform)
taskDefinition_runtimePlatform = Lens.lens (\TaskDefinition' {runtimePlatform} -> runtimePlatform) (\s@TaskDefinition' {} a -> s {runtimePlatform = a} :: TaskDefinition)

-- | The status of the task definition.
taskDefinition_status :: Lens.Lens' TaskDefinition (Prelude.Maybe TaskDefinitionStatus)
taskDefinition_status = Lens.lens (\TaskDefinition' {status} -> status) (\s@TaskDefinition' {} a -> s {status = a} :: TaskDefinition)

-- | The full Amazon Resource Name (ARN) of the task definition.
taskDefinition_taskDefinitionArn :: Lens.Lens' TaskDefinition (Prelude.Maybe Prelude.Text)
taskDefinition_taskDefinitionArn = Lens.lens (\TaskDefinition' {taskDefinitionArn} -> taskDefinitionArn) (\s@TaskDefinition' {} a -> s {taskDefinitionArn = a} :: TaskDefinition)

-- | The short name or full Amazon Resource Name (ARN) of the Identity and
-- Access Management role that grants containers in the task permission to
-- call Amazon Web Services APIs on your behalf. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html Amazon ECS Task Role>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- IAM roles for tasks on Windows require that the @-EnableTaskIAMRole@
-- option is set when you launch the Amazon ECS-optimized Windows AMI. Your
-- containers must also run some configuration code to use the feature. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/windows_task_IAM_roles.html Windows IAM roles for tasks>
-- in the /Amazon Elastic Container Service Developer Guide/.
taskDefinition_taskRoleArn :: Lens.Lens' TaskDefinition (Prelude.Maybe Prelude.Text)
taskDefinition_taskRoleArn = Lens.lens (\TaskDefinition' {taskRoleArn} -> taskRoleArn) (\s@TaskDefinition' {} a -> s {taskRoleArn = a} :: TaskDefinition)

-- | The list of data volume definitions for the task. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_data_volumes.html Using data volumes in tasks>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- The @host@ and @sourcePath@ parameters aren\'t supported for tasks run
-- on Fargate.
taskDefinition_volumes :: Lens.Lens' TaskDefinition (Prelude.Maybe [Volume])
taskDefinition_volumes = Lens.lens (\TaskDefinition' {volumes} -> volumes) (\s@TaskDefinition' {} a -> s {volumes = a} :: TaskDefinition) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TaskDefinition where
  parseJSON =
    Data.withObject
      "TaskDefinition"
      ( \x ->
          TaskDefinition'
            Prelude.<$> ( x
                            Data..:? "compatibilities"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "containerDefinitions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "cpu")
            Prelude.<*> (x Data..:? "deregisteredAt")
            Prelude.<*> (x Data..:? "ephemeralStorage")
            Prelude.<*> (x Data..:? "executionRoleArn")
            Prelude.<*> (x Data..:? "family")
            Prelude.<*> ( x
                            Data..:? "inferenceAccelerators"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ipcMode")
            Prelude.<*> (x Data..:? "memory")
            Prelude.<*> (x Data..:? "networkMode")
            Prelude.<*> (x Data..:? "pidMode")
            Prelude.<*> ( x
                            Data..:? "placementConstraints"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "proxyConfiguration")
            Prelude.<*> (x Data..:? "registeredAt")
            Prelude.<*> (x Data..:? "registeredBy")
            Prelude.<*> ( x
                            Data..:? "requiresAttributes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "requiresCompatibilities"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "revision")
            Prelude.<*> (x Data..:? "runtimePlatform")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "taskDefinitionArn")
            Prelude.<*> (x Data..:? "taskRoleArn")
            Prelude.<*> (x Data..:? "volumes" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable TaskDefinition where
  hashWithSalt _salt TaskDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` compatibilities
      `Prelude.hashWithSalt` containerDefinitions
      `Prelude.hashWithSalt` cpu
      `Prelude.hashWithSalt` deregisteredAt
      `Prelude.hashWithSalt` ephemeralStorage
      `Prelude.hashWithSalt` executionRoleArn
      `Prelude.hashWithSalt` family
      `Prelude.hashWithSalt` inferenceAccelerators
      `Prelude.hashWithSalt` ipcMode
      `Prelude.hashWithSalt` memory
      `Prelude.hashWithSalt` networkMode
      `Prelude.hashWithSalt` pidMode
      `Prelude.hashWithSalt` placementConstraints
      `Prelude.hashWithSalt` proxyConfiguration
      `Prelude.hashWithSalt` registeredAt
      `Prelude.hashWithSalt` registeredBy
      `Prelude.hashWithSalt` requiresAttributes
      `Prelude.hashWithSalt` requiresCompatibilities
      `Prelude.hashWithSalt` revision
      `Prelude.hashWithSalt` runtimePlatform
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` taskDefinitionArn
      `Prelude.hashWithSalt` taskRoleArn
      `Prelude.hashWithSalt` volumes

instance Prelude.NFData TaskDefinition where
  rnf TaskDefinition' {..} =
    Prelude.rnf compatibilities
      `Prelude.seq` Prelude.rnf containerDefinitions
      `Prelude.seq` Prelude.rnf cpu
      `Prelude.seq` Prelude.rnf deregisteredAt
      `Prelude.seq` Prelude.rnf ephemeralStorage
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf family
      `Prelude.seq` Prelude.rnf inferenceAccelerators
      `Prelude.seq` Prelude.rnf ipcMode
      `Prelude.seq` Prelude.rnf memory
      `Prelude.seq` Prelude.rnf networkMode
      `Prelude.seq` Prelude.rnf pidMode
      `Prelude.seq` Prelude.rnf placementConstraints
      `Prelude.seq` Prelude.rnf proxyConfiguration
      `Prelude.seq` Prelude.rnf registeredAt
      `Prelude.seq` Prelude.rnf registeredBy
      `Prelude.seq` Prelude.rnf requiresAttributes
      `Prelude.seq` Prelude.rnf
        requiresCompatibilities
      `Prelude.seq` Prelude.rnf revision
      `Prelude.seq` Prelude.rnf runtimePlatform
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf
        taskDefinitionArn
      `Prelude.seq` Prelude.rnf taskRoleArn
      `Prelude.seq` Prelude.rnf volumes
