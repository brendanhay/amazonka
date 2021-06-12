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
-- Module      : Network.AWS.ECS.RegisterTaskDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- then use the latest versions of the AWS CLI or SDKs to make API requests
-- to the AWS services that are specified in the IAM policy associated with
-- the role. For more information, see
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
module Network.AWS.ECS.RegisterTaskDefinition
  ( -- * Creating a Request
    RegisterTaskDefinition (..),
    newRegisterTaskDefinition,

    -- * Request Lenses
    registerTaskDefinition_taskRoleArn,
    registerTaskDefinition_memory,
    registerTaskDefinition_pidMode,
    registerTaskDefinition_requiresCompatibilities,
    registerTaskDefinition_executionRoleArn,
    registerTaskDefinition_volumes,
    registerTaskDefinition_inferenceAccelerators,
    registerTaskDefinition_placementConstraints,
    registerTaskDefinition_proxyConfiguration,
    registerTaskDefinition_ipcMode,
    registerTaskDefinition_tags,
    registerTaskDefinition_cpu,
    registerTaskDefinition_networkMode,
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

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRegisterTaskDefinition' smart constructor.
data RegisterTaskDefinition = RegisterTaskDefinition'
  { -- | The short name or full Amazon Resource Name (ARN) of the IAM role that
    -- containers in this task can assume. All containers in this task are
    -- granted the permissions that are specified in this role. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    taskRoleArn :: Core.Maybe Core.Text,
    -- | The amount of memory (in MiB) used by the task. It can be expressed as
    -- an integer using MiB, for example @1024@, or as a string using GB, for
    -- example @1GB@ or @1 GB@, in a task definition. String values are
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
    -- use one of the following values, which determines your range of
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
    memory :: Core.Maybe Core.Text,
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
    -- This parameter is not supported for Windows containers or tasks using
    -- the Fargate launch type.
    pidMode :: Core.Maybe PidMode,
    -- | The task launch type that Amazon ECS should validate the task definition
    -- against. This ensures that the task definition parameters are compatible
    -- with the specified launch type. If no value is specified, it defaults to
    -- @EC2@.
    requiresCompatibilities :: Core.Maybe [Compatibility],
    -- | The Amazon Resource Name (ARN) of the task execution role that grants
    -- the Amazon ECS container agent permission to make AWS API calls on your
    -- behalf. The task execution IAM role is required depending on the
    -- requirements of your task. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_execution_IAM_role.html Amazon ECS task execution IAM role>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    executionRoleArn :: Core.Maybe Core.Text,
    -- | A list of volume definitions in JSON format that containers in your task
    -- may use.
    volumes :: Core.Maybe [Volume],
    -- | The Elastic Inference accelerators to use for the containers in the
    -- task.
    inferenceAccelerators :: Core.Maybe [InferenceAccelerator],
    -- | An array of placement constraint objects to use for the task. You can
    -- specify a maximum of 10 constraints per task (this limit includes
    -- constraints in the task definition and those specified at runtime).
    placementConstraints :: Core.Maybe [TaskDefinitionPlacementConstraint],
    proxyConfiguration :: Core.Maybe ProxyConfiguration,
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
    -- This parameter is not supported for Windows containers or tasks using
    -- the Fargate launch type.
    ipcMode :: Core.Maybe IpcMode,
    -- | The metadata that you apply to the task definition to help you
    -- categorize and organize them. Each tag consists of a key and an optional
    -- value, both of which you define.
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
    --     such as a prefix for either keys or values as it is reserved for AWS
    --     use. You cannot edit or delete tag keys or values with this prefix.
    --     Tags with this prefix do not count against your tags per resource
    --     limit.
    tags :: Core.Maybe [Tag],
    -- | The number of CPU units used by the task. It can be expressed as an
    -- integer using CPU units, for example @1024@, or as a string using vCPUs,
    -- for example @1 vCPU@ or @1 vcpu@, in a task definition. String values
    -- are converted to an integer indicating the CPU units when the task
    -- definition is registered.
    --
    -- Task-level CPU and memory parameters are ignored for Windows containers.
    -- We recommend specifying container-level resources for Windows
    -- containers.
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
    cpu :: Core.Maybe Core.Text,
    -- | The Docker networking mode to use for the containers in the task. The
    -- valid values are @none@, @bridge@, @awsvpc@, and @host@. If no network
    -- mode is specified, the default is @bridge@.
    --
    -- For Amazon ECS tasks on Fargate, the @awsvpc@ network mode is required.
    -- For Amazon ECS tasks on Amazon EC2 instances, any network mode can be
    -- used. If the network mode is set to @none@, you cannot specify port
    -- mappings in your container definitions, and the tasks containers do not
    -- have external connectivity. The @host@ and @awsvpc@ network modes offer
    -- the highest networking performance for containers because they use the
    -- EC2 network stack instead of the virtualized network stack provided by
    -- the @bridge@ mode.
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
    -- Currently, only Amazon ECS-optimized AMIs, other Amazon Linux variants
    -- with the @ecs-init@ package, or AWS Fargate infrastructure support the
    -- @awsvpc@ network mode.
    --
    -- If the network mode is @host@, you cannot run multiple instantiations of
    -- the same task on a single container instance when port mappings are
    -- used.
    --
    -- Docker for Windows uses different network modes than Docker for Linux.
    -- When you register a task definition with Windows containers, you must
    -- not specify a network mode. If you use the console to register a task
    -- definition with Windows containers, you must choose the @\<default>@
    -- network mode object.
    --
    -- For more information, see
    -- <https://docs.docker.com/engine/reference/run/#network-settings Network settings>
    -- in the /Docker run reference/.
    networkMode :: Core.Maybe NetworkMode,
    -- | You must specify a @family@ for a task definition, which allows you to
    -- track multiple versions of the same task definition. The @family@ is
    -- used as a name for your task definition. Up to 255 letters (uppercase
    -- and lowercase), numbers, and hyphens are allowed.
    family :: Core.Text,
    -- | A list of container definitions in JSON format that describe the
    -- different containers that make up your task.
    containerDefinitions :: [ContainerDefinition]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterTaskDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskRoleArn', 'registerTaskDefinition_taskRoleArn' - The short name or full Amazon Resource Name (ARN) of the IAM role that
-- containers in this task can assume. All containers in this task are
-- granted the permissions that are specified in this role. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'memory', 'registerTaskDefinition_memory' - The amount of memory (in MiB) used by the task. It can be expressed as
-- an integer using MiB, for example @1024@, or as a string using GB, for
-- example @1GB@ or @1 GB@, in a task definition. String values are
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
-- use one of the following values, which determines your range of
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
-- This parameter is not supported for Windows containers or tasks using
-- the Fargate launch type.
--
-- 'requiresCompatibilities', 'registerTaskDefinition_requiresCompatibilities' - The task launch type that Amazon ECS should validate the task definition
-- against. This ensures that the task definition parameters are compatible
-- with the specified launch type. If no value is specified, it defaults to
-- @EC2@.
--
-- 'executionRoleArn', 'registerTaskDefinition_executionRoleArn' - The Amazon Resource Name (ARN) of the task execution role that grants
-- the Amazon ECS container agent permission to make AWS API calls on your
-- behalf. The task execution IAM role is required depending on the
-- requirements of your task. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_execution_IAM_role.html Amazon ECS task execution IAM role>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'volumes', 'registerTaskDefinition_volumes' - A list of volume definitions in JSON format that containers in your task
-- may use.
--
-- 'inferenceAccelerators', 'registerTaskDefinition_inferenceAccelerators' - The Elastic Inference accelerators to use for the containers in the
-- task.
--
-- 'placementConstraints', 'registerTaskDefinition_placementConstraints' - An array of placement constraint objects to use for the task. You can
-- specify a maximum of 10 constraints per task (this limit includes
-- constraints in the task definition and those specified at runtime).
--
-- 'proxyConfiguration', 'registerTaskDefinition_proxyConfiguration' - Undocumented member.
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
-- This parameter is not supported for Windows containers or tasks using
-- the Fargate launch type.
--
-- 'tags', 'registerTaskDefinition_tags' - The metadata that you apply to the task definition to help you
-- categorize and organize them. Each tag consists of a key and an optional
-- value, both of which you define.
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
--     such as a prefix for either keys or values as it is reserved for AWS
--     use. You cannot edit or delete tag keys or values with this prefix.
--     Tags with this prefix do not count against your tags per resource
--     limit.
--
-- 'cpu', 'registerTaskDefinition_cpu' - The number of CPU units used by the task. It can be expressed as an
-- integer using CPU units, for example @1024@, or as a string using vCPUs,
-- for example @1 vCPU@ or @1 vcpu@, in a task definition. String values
-- are converted to an integer indicating the CPU units when the task
-- definition is registered.
--
-- Task-level CPU and memory parameters are ignored for Windows containers.
-- We recommend specifying container-level resources for Windows
-- containers.
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
-- 'networkMode', 'registerTaskDefinition_networkMode' - The Docker networking mode to use for the containers in the task. The
-- valid values are @none@, @bridge@, @awsvpc@, and @host@. If no network
-- mode is specified, the default is @bridge@.
--
-- For Amazon ECS tasks on Fargate, the @awsvpc@ network mode is required.
-- For Amazon ECS tasks on Amazon EC2 instances, any network mode can be
-- used. If the network mode is set to @none@, you cannot specify port
-- mappings in your container definitions, and the tasks containers do not
-- have external connectivity. The @host@ and @awsvpc@ network modes offer
-- the highest networking performance for containers because they use the
-- EC2 network stack instead of the virtualized network stack provided by
-- the @bridge@ mode.
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
-- Currently, only Amazon ECS-optimized AMIs, other Amazon Linux variants
-- with the @ecs-init@ package, or AWS Fargate infrastructure support the
-- @awsvpc@ network mode.
--
-- If the network mode is @host@, you cannot run multiple instantiations of
-- the same task on a single container instance when port mappings are
-- used.
--
-- Docker for Windows uses different network modes than Docker for Linux.
-- When you register a task definition with Windows containers, you must
-- not specify a network mode. If you use the console to register a task
-- definition with Windows containers, you must choose the @\<default>@
-- network mode object.
--
-- For more information, see
-- <https://docs.docker.com/engine/reference/run/#network-settings Network settings>
-- in the /Docker run reference/.
--
-- 'family', 'registerTaskDefinition_family' - You must specify a @family@ for a task definition, which allows you to
-- track multiple versions of the same task definition. The @family@ is
-- used as a name for your task definition. Up to 255 letters (uppercase
-- and lowercase), numbers, and hyphens are allowed.
--
-- 'containerDefinitions', 'registerTaskDefinition_containerDefinitions' - A list of container definitions in JSON format that describe the
-- different containers that make up your task.
newRegisterTaskDefinition ::
  -- | 'family'
  Core.Text ->
  RegisterTaskDefinition
newRegisterTaskDefinition pFamily_ =
  RegisterTaskDefinition'
    { taskRoleArn = Core.Nothing,
      memory = Core.Nothing,
      pidMode = Core.Nothing,
      requiresCompatibilities = Core.Nothing,
      executionRoleArn = Core.Nothing,
      volumes = Core.Nothing,
      inferenceAccelerators = Core.Nothing,
      placementConstraints = Core.Nothing,
      proxyConfiguration = Core.Nothing,
      ipcMode = Core.Nothing,
      tags = Core.Nothing,
      cpu = Core.Nothing,
      networkMode = Core.Nothing,
      family = pFamily_,
      containerDefinitions = Core.mempty
    }

-- | The short name or full Amazon Resource Name (ARN) of the IAM role that
-- containers in this task can assume. All containers in this task are
-- granted the permissions that are specified in this role. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks>
-- in the /Amazon Elastic Container Service Developer Guide/.
registerTaskDefinition_taskRoleArn :: Lens.Lens' RegisterTaskDefinition (Core.Maybe Core.Text)
registerTaskDefinition_taskRoleArn = Lens.lens (\RegisterTaskDefinition' {taskRoleArn} -> taskRoleArn) (\s@RegisterTaskDefinition' {} a -> s {taskRoleArn = a} :: RegisterTaskDefinition)

-- | The amount of memory (in MiB) used by the task. It can be expressed as
-- an integer using MiB, for example @1024@, or as a string using GB, for
-- example @1GB@ or @1 GB@, in a task definition. String values are
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
-- use one of the following values, which determines your range of
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
registerTaskDefinition_memory :: Lens.Lens' RegisterTaskDefinition (Core.Maybe Core.Text)
registerTaskDefinition_memory = Lens.lens (\RegisterTaskDefinition' {memory} -> memory) (\s@RegisterTaskDefinition' {} a -> s {memory = a} :: RegisterTaskDefinition)

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
-- This parameter is not supported for Windows containers or tasks using
-- the Fargate launch type.
registerTaskDefinition_pidMode :: Lens.Lens' RegisterTaskDefinition (Core.Maybe PidMode)
registerTaskDefinition_pidMode = Lens.lens (\RegisterTaskDefinition' {pidMode} -> pidMode) (\s@RegisterTaskDefinition' {} a -> s {pidMode = a} :: RegisterTaskDefinition)

-- | The task launch type that Amazon ECS should validate the task definition
-- against. This ensures that the task definition parameters are compatible
-- with the specified launch type. If no value is specified, it defaults to
-- @EC2@.
registerTaskDefinition_requiresCompatibilities :: Lens.Lens' RegisterTaskDefinition (Core.Maybe [Compatibility])
registerTaskDefinition_requiresCompatibilities = Lens.lens (\RegisterTaskDefinition' {requiresCompatibilities} -> requiresCompatibilities) (\s@RegisterTaskDefinition' {} a -> s {requiresCompatibilities = a} :: RegisterTaskDefinition) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the task execution role that grants
-- the Amazon ECS container agent permission to make AWS API calls on your
-- behalf. The task execution IAM role is required depending on the
-- requirements of your task. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_execution_IAM_role.html Amazon ECS task execution IAM role>
-- in the /Amazon Elastic Container Service Developer Guide/.
registerTaskDefinition_executionRoleArn :: Lens.Lens' RegisterTaskDefinition (Core.Maybe Core.Text)
registerTaskDefinition_executionRoleArn = Lens.lens (\RegisterTaskDefinition' {executionRoleArn} -> executionRoleArn) (\s@RegisterTaskDefinition' {} a -> s {executionRoleArn = a} :: RegisterTaskDefinition)

-- | A list of volume definitions in JSON format that containers in your task
-- may use.
registerTaskDefinition_volumes :: Lens.Lens' RegisterTaskDefinition (Core.Maybe [Volume])
registerTaskDefinition_volumes = Lens.lens (\RegisterTaskDefinition' {volumes} -> volumes) (\s@RegisterTaskDefinition' {} a -> s {volumes = a} :: RegisterTaskDefinition) Core.. Lens.mapping Lens._Coerce

-- | The Elastic Inference accelerators to use for the containers in the
-- task.
registerTaskDefinition_inferenceAccelerators :: Lens.Lens' RegisterTaskDefinition (Core.Maybe [InferenceAccelerator])
registerTaskDefinition_inferenceAccelerators = Lens.lens (\RegisterTaskDefinition' {inferenceAccelerators} -> inferenceAccelerators) (\s@RegisterTaskDefinition' {} a -> s {inferenceAccelerators = a} :: RegisterTaskDefinition) Core.. Lens.mapping Lens._Coerce

-- | An array of placement constraint objects to use for the task. You can
-- specify a maximum of 10 constraints per task (this limit includes
-- constraints in the task definition and those specified at runtime).
registerTaskDefinition_placementConstraints :: Lens.Lens' RegisterTaskDefinition (Core.Maybe [TaskDefinitionPlacementConstraint])
registerTaskDefinition_placementConstraints = Lens.lens (\RegisterTaskDefinition' {placementConstraints} -> placementConstraints) (\s@RegisterTaskDefinition' {} a -> s {placementConstraints = a} :: RegisterTaskDefinition) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
registerTaskDefinition_proxyConfiguration :: Lens.Lens' RegisterTaskDefinition (Core.Maybe ProxyConfiguration)
registerTaskDefinition_proxyConfiguration = Lens.lens (\RegisterTaskDefinition' {proxyConfiguration} -> proxyConfiguration) (\s@RegisterTaskDefinition' {} a -> s {proxyConfiguration = a} :: RegisterTaskDefinition)

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
-- This parameter is not supported for Windows containers or tasks using
-- the Fargate launch type.
registerTaskDefinition_ipcMode :: Lens.Lens' RegisterTaskDefinition (Core.Maybe IpcMode)
registerTaskDefinition_ipcMode = Lens.lens (\RegisterTaskDefinition' {ipcMode} -> ipcMode) (\s@RegisterTaskDefinition' {} a -> s {ipcMode = a} :: RegisterTaskDefinition)

-- | The metadata that you apply to the task definition to help you
-- categorize and organize them. Each tag consists of a key and an optional
-- value, both of which you define.
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
--     such as a prefix for either keys or values as it is reserved for AWS
--     use. You cannot edit or delete tag keys or values with this prefix.
--     Tags with this prefix do not count against your tags per resource
--     limit.
registerTaskDefinition_tags :: Lens.Lens' RegisterTaskDefinition (Core.Maybe [Tag])
registerTaskDefinition_tags = Lens.lens (\RegisterTaskDefinition' {tags} -> tags) (\s@RegisterTaskDefinition' {} a -> s {tags = a} :: RegisterTaskDefinition) Core.. Lens.mapping Lens._Coerce

-- | The number of CPU units used by the task. It can be expressed as an
-- integer using CPU units, for example @1024@, or as a string using vCPUs,
-- for example @1 vCPU@ or @1 vcpu@, in a task definition. String values
-- are converted to an integer indicating the CPU units when the task
-- definition is registered.
--
-- Task-level CPU and memory parameters are ignored for Windows containers.
-- We recommend specifying container-level resources for Windows
-- containers.
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
registerTaskDefinition_cpu :: Lens.Lens' RegisterTaskDefinition (Core.Maybe Core.Text)
registerTaskDefinition_cpu = Lens.lens (\RegisterTaskDefinition' {cpu} -> cpu) (\s@RegisterTaskDefinition' {} a -> s {cpu = a} :: RegisterTaskDefinition)

-- | The Docker networking mode to use for the containers in the task. The
-- valid values are @none@, @bridge@, @awsvpc@, and @host@. If no network
-- mode is specified, the default is @bridge@.
--
-- For Amazon ECS tasks on Fargate, the @awsvpc@ network mode is required.
-- For Amazon ECS tasks on Amazon EC2 instances, any network mode can be
-- used. If the network mode is set to @none@, you cannot specify port
-- mappings in your container definitions, and the tasks containers do not
-- have external connectivity. The @host@ and @awsvpc@ network modes offer
-- the highest networking performance for containers because they use the
-- EC2 network stack instead of the virtualized network stack provided by
-- the @bridge@ mode.
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
-- Currently, only Amazon ECS-optimized AMIs, other Amazon Linux variants
-- with the @ecs-init@ package, or AWS Fargate infrastructure support the
-- @awsvpc@ network mode.
--
-- If the network mode is @host@, you cannot run multiple instantiations of
-- the same task on a single container instance when port mappings are
-- used.
--
-- Docker for Windows uses different network modes than Docker for Linux.
-- When you register a task definition with Windows containers, you must
-- not specify a network mode. If you use the console to register a task
-- definition with Windows containers, you must choose the @\<default>@
-- network mode object.
--
-- For more information, see
-- <https://docs.docker.com/engine/reference/run/#network-settings Network settings>
-- in the /Docker run reference/.
registerTaskDefinition_networkMode :: Lens.Lens' RegisterTaskDefinition (Core.Maybe NetworkMode)
registerTaskDefinition_networkMode = Lens.lens (\RegisterTaskDefinition' {networkMode} -> networkMode) (\s@RegisterTaskDefinition' {} a -> s {networkMode = a} :: RegisterTaskDefinition)

-- | You must specify a @family@ for a task definition, which allows you to
-- track multiple versions of the same task definition. The @family@ is
-- used as a name for your task definition. Up to 255 letters (uppercase
-- and lowercase), numbers, and hyphens are allowed.
registerTaskDefinition_family :: Lens.Lens' RegisterTaskDefinition Core.Text
registerTaskDefinition_family = Lens.lens (\RegisterTaskDefinition' {family} -> family) (\s@RegisterTaskDefinition' {} a -> s {family = a} :: RegisterTaskDefinition)

-- | A list of container definitions in JSON format that describe the
-- different containers that make up your task.
registerTaskDefinition_containerDefinitions :: Lens.Lens' RegisterTaskDefinition [ContainerDefinition]
registerTaskDefinition_containerDefinitions = Lens.lens (\RegisterTaskDefinition' {containerDefinitions} -> containerDefinitions) (\s@RegisterTaskDefinition' {} a -> s {containerDefinitions = a} :: RegisterTaskDefinition) Core.. Lens._Coerce

instance Core.AWSRequest RegisterTaskDefinition where
  type
    AWSResponse RegisterTaskDefinition =
      RegisterTaskDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterTaskDefinitionResponse'
            Core.<$> (x Core..?> "tags" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "taskDefinition")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RegisterTaskDefinition

instance Core.NFData RegisterTaskDefinition

instance Core.ToHeaders RegisterTaskDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.RegisterTaskDefinition" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RegisterTaskDefinition where
  toJSON RegisterTaskDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("taskRoleArn" Core..=) Core.<$> taskRoleArn,
            ("memory" Core..=) Core.<$> memory,
            ("pidMode" Core..=) Core.<$> pidMode,
            ("requiresCompatibilities" Core..=)
              Core.<$> requiresCompatibilities,
            ("executionRoleArn" Core..=)
              Core.<$> executionRoleArn,
            ("volumes" Core..=) Core.<$> volumes,
            ("inferenceAccelerators" Core..=)
              Core.<$> inferenceAccelerators,
            ("placementConstraints" Core..=)
              Core.<$> placementConstraints,
            ("proxyConfiguration" Core..=)
              Core.<$> proxyConfiguration,
            ("ipcMode" Core..=) Core.<$> ipcMode,
            ("tags" Core..=) Core.<$> tags,
            ("cpu" Core..=) Core.<$> cpu,
            ("networkMode" Core..=) Core.<$> networkMode,
            Core.Just ("family" Core..= family),
            Core.Just
              ( "containerDefinitions"
                  Core..= containerDefinitions
              )
          ]
      )

instance Core.ToPath RegisterTaskDefinition where
  toPath = Core.const "/"

instance Core.ToQuery RegisterTaskDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRegisterTaskDefinitionResponse' smart constructor.
data RegisterTaskDefinitionResponse = RegisterTaskDefinitionResponse'
  { -- | The list of tags associated with the task definition.
    tags :: Core.Maybe [Tag],
    -- | The full description of the registered task definition.
    taskDefinition :: Core.Maybe TaskDefinition,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  RegisterTaskDefinitionResponse
newRegisterTaskDefinitionResponse pHttpStatus_ =
  RegisterTaskDefinitionResponse'
    { tags =
        Core.Nothing,
      taskDefinition = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of tags associated with the task definition.
registerTaskDefinitionResponse_tags :: Lens.Lens' RegisterTaskDefinitionResponse (Core.Maybe [Tag])
registerTaskDefinitionResponse_tags = Lens.lens (\RegisterTaskDefinitionResponse' {tags} -> tags) (\s@RegisterTaskDefinitionResponse' {} a -> s {tags = a} :: RegisterTaskDefinitionResponse) Core.. Lens.mapping Lens._Coerce

-- | The full description of the registered task definition.
registerTaskDefinitionResponse_taskDefinition :: Lens.Lens' RegisterTaskDefinitionResponse (Core.Maybe TaskDefinition)
registerTaskDefinitionResponse_taskDefinition = Lens.lens (\RegisterTaskDefinitionResponse' {taskDefinition} -> taskDefinition) (\s@RegisterTaskDefinitionResponse' {} a -> s {taskDefinition = a} :: RegisterTaskDefinitionResponse)

-- | The response's http status code.
registerTaskDefinitionResponse_httpStatus :: Lens.Lens' RegisterTaskDefinitionResponse Core.Int
registerTaskDefinitionResponse_httpStatus = Lens.lens (\RegisterTaskDefinitionResponse' {httpStatus} -> httpStatus) (\s@RegisterTaskDefinitionResponse' {} a -> s {httpStatus = a} :: RegisterTaskDefinitionResponse)

instance Core.NFData RegisterTaskDefinitionResponse
