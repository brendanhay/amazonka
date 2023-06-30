{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ECS.RegisterTaskDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a new task definition from the supplied @family@ and
-- @containerDefinitions@. Optionally, you can add data volumes to your
-- containers with the @volumes@ parameter. For more information about task
-- definition parameters and defaults, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Amazon ECS Task Definitions>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- You can specify an IAM role for your task with the @taskRoleArn@
-- parameter. When you specify an IAM role for a task, its containers can
-- then use the latest versions of the CLI or SDKs to make API requests to
-- the Amazon Web Services services that are specified in the IAM policy
-- that\'s associated with the role. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- You can specify a Docker networking mode for the containers in your task
-- definition with the @networkMode@ parameter. The available network modes
-- correspond to those described in
-- <https://docs.docker.com/engine/reference/run/#/network-settings Network settings>
-- in the Docker run reference. If you specify the @awsvpc@ network mode,
-- the task is allocated an elastic network interface, and you must specify
-- a NetworkConfiguration when you create a service or run a task with the
-- task definition. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking>
-- in the /Amazon Elastic Container Service Developer Guide/.
module Amazonka.ECS.RegisterTaskDefinition
  ( -- * Creating a Request
    RegisterTaskDefinition (..),
    newRegisterTaskDefinition,

    -- * Request Lenses
    registerTaskDefinition_cpu,
    registerTaskDefinition_ephemeralStorage,
    registerTaskDefinition_executionRoleArn,
    registerTaskDefinition_inferenceAccelerators,
    registerTaskDefinition_ipcMode,
    registerTaskDefinition_memory,
    registerTaskDefinition_networkMode,
    registerTaskDefinition_pidMode,
    registerTaskDefinition_placementConstraints,
    registerTaskDefinition_proxyConfiguration,
    registerTaskDefinition_requiresCompatibilities,
    registerTaskDefinition_runtimePlatform,
    registerTaskDefinition_tags,
    registerTaskDefinition_taskRoleArn,
    registerTaskDefinition_volumes,
    registerTaskDefinition_family,
    registerTaskDefinition_containerDefinitions,

    -- * Destructuring the Response
    RegisterTaskDefinitionResponse (..),
    newRegisterTaskDefinitionResponse,

    -- * Response Lenses
    registerTaskDefinitionResponse_tags,
    registerTaskDefinitionResponse_taskDefinition,
    registerTaskDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterTaskDefinition' smart constructor.
data RegisterTaskDefinition = RegisterTaskDefinition'
  { -- | The number of CPU units used by the task. It can be expressed as an
    -- integer using CPU units (for example, @1024@) or as a string using vCPUs
    -- (for example, @1 vCPU@ or @1 vcpu@) in a task definition. String values
    -- are converted to an integer indicating the CPU units when the task
    -- definition is registered.
    --
    -- Task-level CPU and memory parameters are ignored for Windows containers.
    -- We recommend specifying container-level resources for Windows
    -- containers.
    --
    -- If you\'re using the EC2 launch type, this field is optional. Supported
    -- values are between @128@ CPU units (@0.125@ vCPUs) and @10240@ CPU units
    -- (@10@ vCPUs). If you do not specify a value, the parameter is ignored.
    --
    -- If you\'re using the Fargate launch type, this field is required and you
    -- must use one of the following values, which determines your range of
    -- supported values for the @memory@ parameter:
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
    -- | The amount of ephemeral storage to allocate for the task. This parameter
    -- is used to expand the total amount of ephemeral storage available,
    -- beyond the default amount, for tasks hosted on Fargate. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/userguide/using_data_volumes.html Fargate task storage>
    -- in the /Amazon ECS User Guide for Fargate/.
    --
    -- This parameter is only supported for tasks hosted on Fargate using the
    -- following platform versions:
    --
    -- -   Linux platform version @1.4.0@ or later.
    ephemeralStorage :: Prelude.Maybe EphemeralStorage,
    -- | The Amazon Resource Name (ARN) of the task execution role that grants
    -- the Amazon ECS container agent permission to make Amazon Web Services
    -- API calls on your behalf. The task execution IAM role is required
    -- depending on the requirements of your task. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_execution_IAM_role.html Amazon ECS task execution IAM role>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    executionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The Elastic Inference accelerators to use for the containers in the
    -- task.
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
    -- | The amount of memory (in MiB) used by the task. It can be expressed as
    -- an integer using MiB (for example ,@1024@) or as a string using GB (for
    -- example, @1GB@ or @1 GB@) in a task definition. String values are
    -- converted to an integer indicating the MiB when the task definition is
    -- registered.
    --
    -- Task-level CPU and memory parameters are ignored for Windows containers.
    -- We recommend specifying container-level resources for Windows
    -- containers.
    --
    -- If using the EC2 launch type, this field is optional.
    --
    -- If using the Fargate launch type, this field is required and you must
    -- use one of the following values. This determines your range of supported
    -- values for the @cpu@ parameter.
    --
    -- The CPU units cannot be less than 1 vCPU when you use Windows containers
    -- on Fargate.
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
    -- | An array of placement constraint objects to use for the task. You can
    -- specify a maximum of 10 constraints for each task. This limit includes
    -- constraints in the task definition and those specified at runtime.
    placementConstraints :: Prelude.Maybe [TaskDefinitionPlacementConstraint],
    -- | The configuration details for the App Mesh proxy.
    --
    -- For tasks hosted on Amazon EC2 instances, the container instances
    -- require at least version @1.26.0@ of the container agent and at least
    -- version @1.26.0-1@ of the @ecs-init@ package to use a proxy
    -- configuration. If your container instances are launched from the Amazon
    -- ECS-optimized AMI version @20190301@ or later, then they contain the
    -- required versions of the container agent and @ecs-init@. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-ami-versions.html Amazon ECS-optimized AMI versions>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    proxyConfiguration :: Prelude.Maybe ProxyConfiguration,
    -- | The task launch type that Amazon ECS validates the task definition
    -- against. A client exception is returned if the task definition doesn\'t
    -- validate against the compatibilities specified. If no value is
    -- specified, the parameter is omitted from the response.
    requiresCompatibilities :: Prelude.Maybe [Compatibility],
    -- | The operating system that your tasks definitions run on. A platform
    -- family is specified only for tasks using the Fargate launch type.
    --
    -- When you specify a task definition in a service, this value must match
    -- the @runtimePlatform@ value of the service.
    runtimePlatform :: Prelude.Maybe RuntimePlatform,
    -- | The metadata that you apply to the task definition to help you
    -- categorize and organize them. Each tag consists of a key and an optional
    -- value. You define both of them.
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
    tags :: Prelude.Maybe [Tag],
    -- | The short name or full Amazon Resource Name (ARN) of the IAM role that
    -- containers in this task can assume. All containers in this task are
    -- granted the permissions that are specified in this role. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    taskRoleArn :: Prelude.Maybe Prelude.Text,
    -- | A list of volume definitions in JSON format that containers in your task
    -- might use.
    volumes :: Prelude.Maybe [Volume],
    -- | You must specify a @family@ for a task definition. You can use it track
    -- multiple versions of the same task definition. The @family@ is used as a
    -- name for your task definition. Up to 255 letters (uppercase and
    -- lowercase), numbers, underscores, and hyphens are allowed.
    family :: Prelude.Text,
    -- | A list of container definitions in JSON format that describe the
    -- different containers that make up your task.
    containerDefinitions :: [ContainerDefinition]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterTaskDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cpu', 'registerTaskDefinition_cpu' - The number of CPU units used by the task. It can be expressed as an
-- integer using CPU units (for example, @1024@) or as a string using vCPUs
-- (for example, @1 vCPU@ or @1 vcpu@) in a task definition. String values
-- are converted to an integer indicating the CPU units when the task
-- definition is registered.
--
-- Task-level CPU and memory parameters are ignored for Windows containers.
-- We recommend specifying container-level resources for Windows
-- containers.
--
-- If you\'re using the EC2 launch type, this field is optional. Supported
-- values are between @128@ CPU units (@0.125@ vCPUs) and @10240@ CPU units
-- (@10@ vCPUs). If you do not specify a value, the parameter is ignored.
--
-- If you\'re using the Fargate launch type, this field is required and you
-- must use one of the following values, which determines your range of
-- supported values for the @memory@ parameter:
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
-- 'ephemeralStorage', 'registerTaskDefinition_ephemeralStorage' - The amount of ephemeral storage to allocate for the task. This parameter
-- is used to expand the total amount of ephemeral storage available,
-- beyond the default amount, for tasks hosted on Fargate. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/userguide/using_data_volumes.html Fargate task storage>
-- in the /Amazon ECS User Guide for Fargate/.
--
-- This parameter is only supported for tasks hosted on Fargate using the
-- following platform versions:
--
-- -   Linux platform version @1.4.0@ or later.
--
-- 'executionRoleArn', 'registerTaskDefinition_executionRoleArn' - The Amazon Resource Name (ARN) of the task execution role that grants
-- the Amazon ECS container agent permission to make Amazon Web Services
-- API calls on your behalf. The task execution IAM role is required
-- depending on the requirements of your task. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_execution_IAM_role.html Amazon ECS task execution IAM role>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'inferenceAccelerators', 'registerTaskDefinition_inferenceAccelerators' - The Elastic Inference accelerators to use for the containers in the
-- task.
--
-- 'ipcMode', 'registerTaskDefinition_ipcMode' - The IPC resource namespace to use for the containers in the task. The
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
-- 'memory', 'registerTaskDefinition_memory' - The amount of memory (in MiB) used by the task. It can be expressed as
-- an integer using MiB (for example ,@1024@) or as a string using GB (for
-- example, @1GB@ or @1 GB@) in a task definition. String values are
-- converted to an integer indicating the MiB when the task definition is
-- registered.
--
-- Task-level CPU and memory parameters are ignored for Windows containers.
-- We recommend specifying container-level resources for Windows
-- containers.
--
-- If using the EC2 launch type, this field is optional.
--
-- If using the Fargate launch type, this field is required and you must
-- use one of the following values. This determines your range of supported
-- values for the @cpu@ parameter.
--
-- The CPU units cannot be less than 1 vCPU when you use Windows containers
-- on Fargate.
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
-- 'networkMode', 'registerTaskDefinition_networkMode' - The Docker networking mode to use for the containers in the task. The
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
-- 'pidMode', 'registerTaskDefinition_pidMode' - The process namespace to use for the containers in the task. The valid
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
-- 'placementConstraints', 'registerTaskDefinition_placementConstraints' - An array of placement constraint objects to use for the task. You can
-- specify a maximum of 10 constraints for each task. This limit includes
-- constraints in the task definition and those specified at runtime.
--
-- 'proxyConfiguration', 'registerTaskDefinition_proxyConfiguration' - The configuration details for the App Mesh proxy.
--
-- For tasks hosted on Amazon EC2 instances, the container instances
-- require at least version @1.26.0@ of the container agent and at least
-- version @1.26.0-1@ of the @ecs-init@ package to use a proxy
-- configuration. If your container instances are launched from the Amazon
-- ECS-optimized AMI version @20190301@ or later, then they contain the
-- required versions of the container agent and @ecs-init@. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-ami-versions.html Amazon ECS-optimized AMI versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'requiresCompatibilities', 'registerTaskDefinition_requiresCompatibilities' - The task launch type that Amazon ECS validates the task definition
-- against. A client exception is returned if the task definition doesn\'t
-- validate against the compatibilities specified. If no value is
-- specified, the parameter is omitted from the response.
--
-- 'runtimePlatform', 'registerTaskDefinition_runtimePlatform' - The operating system that your tasks definitions run on. A platform
-- family is specified only for tasks using the Fargate launch type.
--
-- When you specify a task definition in a service, this value must match
-- the @runtimePlatform@ value of the service.
--
-- 'tags', 'registerTaskDefinition_tags' - The metadata that you apply to the task definition to help you
-- categorize and organize them. Each tag consists of a key and an optional
-- value. You define both of them.
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
--
-- 'taskRoleArn', 'registerTaskDefinition_taskRoleArn' - The short name or full Amazon Resource Name (ARN) of the IAM role that
-- containers in this task can assume. All containers in this task are
-- granted the permissions that are specified in this role. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'volumes', 'registerTaskDefinition_volumes' - A list of volume definitions in JSON format that containers in your task
-- might use.
--
-- 'family', 'registerTaskDefinition_family' - You must specify a @family@ for a task definition. You can use it track
-- multiple versions of the same task definition. The @family@ is used as a
-- name for your task definition. Up to 255 letters (uppercase and
-- lowercase), numbers, underscores, and hyphens are allowed.
--
-- 'containerDefinitions', 'registerTaskDefinition_containerDefinitions' - A list of container definitions in JSON format that describe the
-- different containers that make up your task.
newRegisterTaskDefinition ::
  -- | 'family'
  Prelude.Text ->
  RegisterTaskDefinition
newRegisterTaskDefinition pFamily_ =
  RegisterTaskDefinition'
    { cpu = Prelude.Nothing,
      ephemeralStorage = Prelude.Nothing,
      executionRoleArn = Prelude.Nothing,
      inferenceAccelerators = Prelude.Nothing,
      ipcMode = Prelude.Nothing,
      memory = Prelude.Nothing,
      networkMode = Prelude.Nothing,
      pidMode = Prelude.Nothing,
      placementConstraints = Prelude.Nothing,
      proxyConfiguration = Prelude.Nothing,
      requiresCompatibilities = Prelude.Nothing,
      runtimePlatform = Prelude.Nothing,
      tags = Prelude.Nothing,
      taskRoleArn = Prelude.Nothing,
      volumes = Prelude.Nothing,
      family = pFamily_,
      containerDefinitions = Prelude.mempty
    }

-- | The number of CPU units used by the task. It can be expressed as an
-- integer using CPU units (for example, @1024@) or as a string using vCPUs
-- (for example, @1 vCPU@ or @1 vcpu@) in a task definition. String values
-- are converted to an integer indicating the CPU units when the task
-- definition is registered.
--
-- Task-level CPU and memory parameters are ignored for Windows containers.
-- We recommend specifying container-level resources for Windows
-- containers.
--
-- If you\'re using the EC2 launch type, this field is optional. Supported
-- values are between @128@ CPU units (@0.125@ vCPUs) and @10240@ CPU units
-- (@10@ vCPUs). If you do not specify a value, the parameter is ignored.
--
-- If you\'re using the Fargate launch type, this field is required and you
-- must use one of the following values, which determines your range of
-- supported values for the @memory@ parameter:
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
registerTaskDefinition_cpu :: Lens.Lens' RegisterTaskDefinition (Prelude.Maybe Prelude.Text)
registerTaskDefinition_cpu = Lens.lens (\RegisterTaskDefinition' {cpu} -> cpu) (\s@RegisterTaskDefinition' {} a -> s {cpu = a} :: RegisterTaskDefinition)

-- | The amount of ephemeral storage to allocate for the task. This parameter
-- is used to expand the total amount of ephemeral storage available,
-- beyond the default amount, for tasks hosted on Fargate. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/userguide/using_data_volumes.html Fargate task storage>
-- in the /Amazon ECS User Guide for Fargate/.
--
-- This parameter is only supported for tasks hosted on Fargate using the
-- following platform versions:
--
-- -   Linux platform version @1.4.0@ or later.
registerTaskDefinition_ephemeralStorage :: Lens.Lens' RegisterTaskDefinition (Prelude.Maybe EphemeralStorage)
registerTaskDefinition_ephemeralStorage = Lens.lens (\RegisterTaskDefinition' {ephemeralStorage} -> ephemeralStorage) (\s@RegisterTaskDefinition' {} a -> s {ephemeralStorage = a} :: RegisterTaskDefinition)

-- | The Amazon Resource Name (ARN) of the task execution role that grants
-- the Amazon ECS container agent permission to make Amazon Web Services
-- API calls on your behalf. The task execution IAM role is required
-- depending on the requirements of your task. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_execution_IAM_role.html Amazon ECS task execution IAM role>
-- in the /Amazon Elastic Container Service Developer Guide/.
registerTaskDefinition_executionRoleArn :: Lens.Lens' RegisterTaskDefinition (Prelude.Maybe Prelude.Text)
registerTaskDefinition_executionRoleArn = Lens.lens (\RegisterTaskDefinition' {executionRoleArn} -> executionRoleArn) (\s@RegisterTaskDefinition' {} a -> s {executionRoleArn = a} :: RegisterTaskDefinition)

-- | The Elastic Inference accelerators to use for the containers in the
-- task.
registerTaskDefinition_inferenceAccelerators :: Lens.Lens' RegisterTaskDefinition (Prelude.Maybe [InferenceAccelerator])
registerTaskDefinition_inferenceAccelerators = Lens.lens (\RegisterTaskDefinition' {inferenceAccelerators} -> inferenceAccelerators) (\s@RegisterTaskDefinition' {} a -> s {inferenceAccelerators = a} :: RegisterTaskDefinition) Prelude.. Lens.mapping Lens.coerced

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
registerTaskDefinition_ipcMode :: Lens.Lens' RegisterTaskDefinition (Prelude.Maybe IpcMode)
registerTaskDefinition_ipcMode = Lens.lens (\RegisterTaskDefinition' {ipcMode} -> ipcMode) (\s@RegisterTaskDefinition' {} a -> s {ipcMode = a} :: RegisterTaskDefinition)

-- | The amount of memory (in MiB) used by the task. It can be expressed as
-- an integer using MiB (for example ,@1024@) or as a string using GB (for
-- example, @1GB@ or @1 GB@) in a task definition. String values are
-- converted to an integer indicating the MiB when the task definition is
-- registered.
--
-- Task-level CPU and memory parameters are ignored for Windows containers.
-- We recommend specifying container-level resources for Windows
-- containers.
--
-- If using the EC2 launch type, this field is optional.
--
-- If using the Fargate launch type, this field is required and you must
-- use one of the following values. This determines your range of supported
-- values for the @cpu@ parameter.
--
-- The CPU units cannot be less than 1 vCPU when you use Windows containers
-- on Fargate.
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
registerTaskDefinition_memory :: Lens.Lens' RegisterTaskDefinition (Prelude.Maybe Prelude.Text)
registerTaskDefinition_memory = Lens.lens (\RegisterTaskDefinition' {memory} -> memory) (\s@RegisterTaskDefinition' {} a -> s {memory = a} :: RegisterTaskDefinition)

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
registerTaskDefinition_networkMode :: Lens.Lens' RegisterTaskDefinition (Prelude.Maybe NetworkMode)
registerTaskDefinition_networkMode = Lens.lens (\RegisterTaskDefinition' {networkMode} -> networkMode) (\s@RegisterTaskDefinition' {} a -> s {networkMode = a} :: RegisterTaskDefinition)

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
registerTaskDefinition_pidMode :: Lens.Lens' RegisterTaskDefinition (Prelude.Maybe PidMode)
registerTaskDefinition_pidMode = Lens.lens (\RegisterTaskDefinition' {pidMode} -> pidMode) (\s@RegisterTaskDefinition' {} a -> s {pidMode = a} :: RegisterTaskDefinition)

-- | An array of placement constraint objects to use for the task. You can
-- specify a maximum of 10 constraints for each task. This limit includes
-- constraints in the task definition and those specified at runtime.
registerTaskDefinition_placementConstraints :: Lens.Lens' RegisterTaskDefinition (Prelude.Maybe [TaskDefinitionPlacementConstraint])
registerTaskDefinition_placementConstraints = Lens.lens (\RegisterTaskDefinition' {placementConstraints} -> placementConstraints) (\s@RegisterTaskDefinition' {} a -> s {placementConstraints = a} :: RegisterTaskDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The configuration details for the App Mesh proxy.
--
-- For tasks hosted on Amazon EC2 instances, the container instances
-- require at least version @1.26.0@ of the container agent and at least
-- version @1.26.0-1@ of the @ecs-init@ package to use a proxy
-- configuration. If your container instances are launched from the Amazon
-- ECS-optimized AMI version @20190301@ or later, then they contain the
-- required versions of the container agent and @ecs-init@. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-ami-versions.html Amazon ECS-optimized AMI versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
registerTaskDefinition_proxyConfiguration :: Lens.Lens' RegisterTaskDefinition (Prelude.Maybe ProxyConfiguration)
registerTaskDefinition_proxyConfiguration = Lens.lens (\RegisterTaskDefinition' {proxyConfiguration} -> proxyConfiguration) (\s@RegisterTaskDefinition' {} a -> s {proxyConfiguration = a} :: RegisterTaskDefinition)

-- | The task launch type that Amazon ECS validates the task definition
-- against. A client exception is returned if the task definition doesn\'t
-- validate against the compatibilities specified. If no value is
-- specified, the parameter is omitted from the response.
registerTaskDefinition_requiresCompatibilities :: Lens.Lens' RegisterTaskDefinition (Prelude.Maybe [Compatibility])
registerTaskDefinition_requiresCompatibilities = Lens.lens (\RegisterTaskDefinition' {requiresCompatibilities} -> requiresCompatibilities) (\s@RegisterTaskDefinition' {} a -> s {requiresCompatibilities = a} :: RegisterTaskDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The operating system that your tasks definitions run on. A platform
-- family is specified only for tasks using the Fargate launch type.
--
-- When you specify a task definition in a service, this value must match
-- the @runtimePlatform@ value of the service.
registerTaskDefinition_runtimePlatform :: Lens.Lens' RegisterTaskDefinition (Prelude.Maybe RuntimePlatform)
registerTaskDefinition_runtimePlatform = Lens.lens (\RegisterTaskDefinition' {runtimePlatform} -> runtimePlatform) (\s@RegisterTaskDefinition' {} a -> s {runtimePlatform = a} :: RegisterTaskDefinition)

-- | The metadata that you apply to the task definition to help you
-- categorize and organize them. Each tag consists of a key and an optional
-- value. You define both of them.
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
registerTaskDefinition_tags :: Lens.Lens' RegisterTaskDefinition (Prelude.Maybe [Tag])
registerTaskDefinition_tags = Lens.lens (\RegisterTaskDefinition' {tags} -> tags) (\s@RegisterTaskDefinition' {} a -> s {tags = a} :: RegisterTaskDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The short name or full Amazon Resource Name (ARN) of the IAM role that
-- containers in this task can assume. All containers in this task are
-- granted the permissions that are specified in this role. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks>
-- in the /Amazon Elastic Container Service Developer Guide/.
registerTaskDefinition_taskRoleArn :: Lens.Lens' RegisterTaskDefinition (Prelude.Maybe Prelude.Text)
registerTaskDefinition_taskRoleArn = Lens.lens (\RegisterTaskDefinition' {taskRoleArn} -> taskRoleArn) (\s@RegisterTaskDefinition' {} a -> s {taskRoleArn = a} :: RegisterTaskDefinition)

-- | A list of volume definitions in JSON format that containers in your task
-- might use.
registerTaskDefinition_volumes :: Lens.Lens' RegisterTaskDefinition (Prelude.Maybe [Volume])
registerTaskDefinition_volumes = Lens.lens (\RegisterTaskDefinition' {volumes} -> volumes) (\s@RegisterTaskDefinition' {} a -> s {volumes = a} :: RegisterTaskDefinition) Prelude.. Lens.mapping Lens.coerced

-- | You must specify a @family@ for a task definition. You can use it track
-- multiple versions of the same task definition. The @family@ is used as a
-- name for your task definition. Up to 255 letters (uppercase and
-- lowercase), numbers, underscores, and hyphens are allowed.
registerTaskDefinition_family :: Lens.Lens' RegisterTaskDefinition Prelude.Text
registerTaskDefinition_family = Lens.lens (\RegisterTaskDefinition' {family} -> family) (\s@RegisterTaskDefinition' {} a -> s {family = a} :: RegisterTaskDefinition)

-- | A list of container definitions in JSON format that describe the
-- different containers that make up your task.
registerTaskDefinition_containerDefinitions :: Lens.Lens' RegisterTaskDefinition [ContainerDefinition]
registerTaskDefinition_containerDefinitions = Lens.lens (\RegisterTaskDefinition' {containerDefinitions} -> containerDefinitions) (\s@RegisterTaskDefinition' {} a -> s {containerDefinitions = a} :: RegisterTaskDefinition) Prelude.. Lens.coerced

instance Core.AWSRequest RegisterTaskDefinition where
  type
    AWSResponse RegisterTaskDefinition =
      RegisterTaskDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterTaskDefinitionResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "taskDefinition")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterTaskDefinition where
  hashWithSalt _salt RegisterTaskDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` cpu
      `Prelude.hashWithSalt` ephemeralStorage
      `Prelude.hashWithSalt` executionRoleArn
      `Prelude.hashWithSalt` inferenceAccelerators
      `Prelude.hashWithSalt` ipcMode
      `Prelude.hashWithSalt` memory
      `Prelude.hashWithSalt` networkMode
      `Prelude.hashWithSalt` pidMode
      `Prelude.hashWithSalt` placementConstraints
      `Prelude.hashWithSalt` proxyConfiguration
      `Prelude.hashWithSalt` requiresCompatibilities
      `Prelude.hashWithSalt` runtimePlatform
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` taskRoleArn
      `Prelude.hashWithSalt` volumes
      `Prelude.hashWithSalt` family
      `Prelude.hashWithSalt` containerDefinitions

instance Prelude.NFData RegisterTaskDefinition where
  rnf RegisterTaskDefinition' {..} =
    Prelude.rnf cpu
      `Prelude.seq` Prelude.rnf ephemeralStorage
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf inferenceAccelerators
      `Prelude.seq` Prelude.rnf ipcMode
      `Prelude.seq` Prelude.rnf memory
      `Prelude.seq` Prelude.rnf networkMode
      `Prelude.seq` Prelude.rnf pidMode
      `Prelude.seq` Prelude.rnf placementConstraints
      `Prelude.seq` Prelude.rnf proxyConfiguration
      `Prelude.seq` Prelude.rnf requiresCompatibilities
      `Prelude.seq` Prelude.rnf runtimePlatform
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf taskRoleArn
      `Prelude.seq` Prelude.rnf volumes
      `Prelude.seq` Prelude.rnf family
      `Prelude.seq` Prelude.rnf containerDefinitions

instance Data.ToHeaders RegisterTaskDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.RegisterTaskDefinition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RegisterTaskDefinition where
  toJSON RegisterTaskDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cpu" Data..=) Prelude.<$> cpu,
            ("ephemeralStorage" Data..=)
              Prelude.<$> ephemeralStorage,
            ("executionRoleArn" Data..=)
              Prelude.<$> executionRoleArn,
            ("inferenceAccelerators" Data..=)
              Prelude.<$> inferenceAccelerators,
            ("ipcMode" Data..=) Prelude.<$> ipcMode,
            ("memory" Data..=) Prelude.<$> memory,
            ("networkMode" Data..=) Prelude.<$> networkMode,
            ("pidMode" Data..=) Prelude.<$> pidMode,
            ("placementConstraints" Data..=)
              Prelude.<$> placementConstraints,
            ("proxyConfiguration" Data..=)
              Prelude.<$> proxyConfiguration,
            ("requiresCompatibilities" Data..=)
              Prelude.<$> requiresCompatibilities,
            ("runtimePlatform" Data..=)
              Prelude.<$> runtimePlatform,
            ("tags" Data..=) Prelude.<$> tags,
            ("taskRoleArn" Data..=) Prelude.<$> taskRoleArn,
            ("volumes" Data..=) Prelude.<$> volumes,
            Prelude.Just ("family" Data..= family),
            Prelude.Just
              ( "containerDefinitions"
                  Data..= containerDefinitions
              )
          ]
      )

instance Data.ToPath RegisterTaskDefinition where
  toPath = Prelude.const "/"

instance Data.ToQuery RegisterTaskDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterTaskDefinitionResponse' smart constructor.
data RegisterTaskDefinitionResponse = RegisterTaskDefinitionResponse'
  { -- | The list of tags associated with the task definition.
    tags :: Prelude.Maybe [Tag],
    -- | The full description of the registered task definition.
    taskDefinition :: Prelude.Maybe TaskDefinition,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterTaskDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'registerTaskDefinitionResponse_tags' - The list of tags associated with the task definition.
--
-- 'taskDefinition', 'registerTaskDefinitionResponse_taskDefinition' - The full description of the registered task definition.
--
-- 'httpStatus', 'registerTaskDefinitionResponse_httpStatus' - The response's http status code.
newRegisterTaskDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterTaskDefinitionResponse
newRegisterTaskDefinitionResponse pHttpStatus_ =
  RegisterTaskDefinitionResponse'
    { tags =
        Prelude.Nothing,
      taskDefinition = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of tags associated with the task definition.
registerTaskDefinitionResponse_tags :: Lens.Lens' RegisterTaskDefinitionResponse (Prelude.Maybe [Tag])
registerTaskDefinitionResponse_tags = Lens.lens (\RegisterTaskDefinitionResponse' {tags} -> tags) (\s@RegisterTaskDefinitionResponse' {} a -> s {tags = a} :: RegisterTaskDefinitionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The full description of the registered task definition.
registerTaskDefinitionResponse_taskDefinition :: Lens.Lens' RegisterTaskDefinitionResponse (Prelude.Maybe TaskDefinition)
registerTaskDefinitionResponse_taskDefinition = Lens.lens (\RegisterTaskDefinitionResponse' {taskDefinition} -> taskDefinition) (\s@RegisterTaskDefinitionResponse' {} a -> s {taskDefinition = a} :: RegisterTaskDefinitionResponse)

-- | The response's http status code.
registerTaskDefinitionResponse_httpStatus :: Lens.Lens' RegisterTaskDefinitionResponse Prelude.Int
registerTaskDefinitionResponse_httpStatus = Lens.lens (\RegisterTaskDefinitionResponse' {httpStatus} -> httpStatus) (\s@RegisterTaskDefinitionResponse' {} a -> s {httpStatus = a} :: RegisterTaskDefinitionResponse)

instance
  Prelude.NFData
    RegisterTaskDefinitionResponse
  where
  rnf RegisterTaskDefinitionResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf taskDefinition
      `Prelude.seq` Prelude.rnf httpStatus
