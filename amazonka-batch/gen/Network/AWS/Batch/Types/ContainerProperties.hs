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
-- Module      : Network.AWS.Batch.Types.ContainerProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ContainerProperties where

import Network.AWS.Batch.Types.FargatePlatformConfiguration
import Network.AWS.Batch.Types.KeyValuePair
import Network.AWS.Batch.Types.LinuxParameters
import Network.AWS.Batch.Types.LogConfiguration
import Network.AWS.Batch.Types.MountPoint
import Network.AWS.Batch.Types.NetworkConfiguration
import Network.AWS.Batch.Types.ResourceRequirement
import Network.AWS.Batch.Types.Secret
import Network.AWS.Batch.Types.Ulimit
import Network.AWS.Batch.Types.Volume
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Container properties are used in job definitions to describe the
-- container that\'s launched as part of a job.
--
-- /See:/ 'newContainerProperties' smart constructor.
data ContainerProperties = ContainerProperties'
  { -- | Linux-specific modifications that are applied to the container, such as
    -- details for device mappings.
    linuxParameters :: Core.Maybe LinuxParameters,
    -- | This parameter is deprecated and not supported for jobs run on Fargate
    -- resources, use @ResourceRequirement@. For jobs run on EC2 resources can
    -- specify the memory requirement using the @ResourceRequirement@
    -- structure. The hard limit (in MiB) of memory to present to the
    -- container. If your container attempts to exceed the memory specified
    -- here, the container is killed. This parameter maps to @Memory@ in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--memory@ option to
    -- <https://docs.docker.com/engine/reference/run/ docker run>. You must
    -- specify at least 4 MiB of memory for a job. This is required but can be
    -- specified in several places; it must be specified for each node at least
    -- once.
    --
    -- If you\'re trying to maximize your resource utilization by providing
    -- your jobs as much memory as possible for a particular instance type, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/memory-management.html Memory Management>
    -- in the /AWS Batch User Guide/.
    memory :: Core.Maybe Core.Int,
    -- | The user name to use inside the container. This parameter maps to @User@
    -- in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--user@ option to
    -- <https://docs.docker.com/engine/reference/run/ docker run>.
    user :: Core.Maybe Core.Text,
    -- | The instance type to use for a multi-node parallel job. All node groups
    -- in a multi-node parallel job must use the same instance type.
    --
    -- This parameter isn\'t applicable to single-node container jobs or for
    -- jobs running on Fargate resources and shouldn\'t be provided.
    instanceType :: Core.Maybe Core.Text,
    -- | The network configuration for jobs running on Fargate resources. Jobs
    -- running on EC2 resources must not specify this parameter.
    networkConfiguration :: Core.Maybe NetworkConfiguration,
    -- | The Amazon Resource Name (ARN) of the execution role that AWS Batch can
    -- assume. Jobs running on Fargate resources must provide an execution
    -- role. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/execution-IAM-role.html AWS Batch execution IAM role>
    -- in the /AWS Batch User Guide/.
    executionRoleArn :: Core.Maybe Core.Text,
    -- | When this parameter is true, the container is given elevated permissions
    -- on the host container instance (similar to the @root@ user). This
    -- parameter maps to @Privileged@ in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--privileged@ option to
    -- <https://docs.docker.com/engine/reference/run/ docker run>. The default
    -- value is false.
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources
    -- and shouldn\'t be provided, or specified as false.
    privileged :: Core.Maybe Core.Bool,
    -- | This parameter is deprecated and not supported for jobs run on Fargate
    -- resources, see @resourceRequirement@. The number of vCPUs reserved for
    -- the container. Jobs running on EC2 resources can specify the vCPU
    -- requirement for the job using @resourceRequirements@ but the vCPU
    -- requirements can\'t be specified both here and in the
    -- @resourceRequirement@ structure. This parameter maps to @CpuShares@ in
    -- the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--cpu-shares@ option to
    -- <https://docs.docker.com/engine/reference/run/ docker run>. Each vCPU is
    -- equivalent to 1,024 CPU shares. You must specify at least one vCPU. This
    -- is required but can be specified in several places. It must be specified
    -- for each node at least once.
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources
    -- and shouldn\'t be provided. Jobs running on Fargate resources must
    -- specify the vCPU requirement for the job using @resourceRequirements@.
    vcpus :: Core.Maybe Core.Int,
    -- | A list of data volumes used in a job.
    volumes :: Core.Maybe [Volume],
    -- | The environment variables to pass to a container. This parameter maps to
    -- @Env@ in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--env@ option to
    -- <https://docs.docker.com/engine/reference/run/ docker run>.
    --
    -- We don\'t recommend using plaintext environment variables for sensitive
    -- information, such as credential data.
    --
    -- Environment variables must not start with @AWS_BATCH@; this naming
    -- convention is reserved for variables that are set by the AWS Batch
    -- service.
    environment :: Core.Maybe [KeyValuePair],
    -- | The platform configuration for jobs running on Fargate resources. Jobs
    -- running on EC2 resources must not specify this parameter.
    fargatePlatformConfiguration :: Core.Maybe FargatePlatformConfiguration,
    -- | The secrets for the container. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/specifying-sensitive-data.html Specifying sensitive data>
    -- in the /AWS Batch User Guide/.
    secrets :: Core.Maybe [Secret],
    -- | The mount points for data volumes in your container. This parameter maps
    -- to @Volumes@ in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--volume@ option to
    -- <https://docs.docker.com/engine/reference/run/ docker run>.
    mountPoints :: Core.Maybe [MountPoint],
    -- | The image used to start a container. This string is passed directly to
    -- the Docker daemon. Images in the Docker Hub registry are available by
    -- default. Other repositories are specified with
    -- @ repository-url\/image:tag @. Up to 255 letters (uppercase and
    -- lowercase), numbers, hyphens, underscores, colons, periods, forward
    -- slashes, and number signs are allowed. This parameter maps to @Image@ in
    -- the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @IMAGE@ parameter of
    -- <https://docs.docker.com/engine/reference/run/ docker run>.
    --
    -- Docker image architecture must match the processor architecture of the
    -- compute resources that they\'re scheduled on. For example, ARM-based
    -- Docker images can only run on ARM-based compute resources.
    --
    -- -   Images in Amazon ECR repositories use the full registry and
    --     repository URI (for example,
    --     @012345678910.dkr.ecr.\<region-name>.amazonaws.com\/\<repository-name>@).
    --
    -- -   Images in official repositories on Docker Hub use a single name (for
    --     example, @ubuntu@ or @mongo@).
    --
    -- -   Images in other repositories on Docker Hub are qualified with an
    --     organization name (for example, @amazon\/amazon-ecs-agent@).
    --
    -- -   Images in other online repositories are qualified further by a
    --     domain name (for example, @quay.io\/assemblyline\/ubuntu@).
    image :: Core.Maybe Core.Text,
    -- | The command that\'s passed to the container. This parameter maps to
    -- @Cmd@ in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @COMMAND@ parameter to
    -- <https://docs.docker.com/engine/reference/run/ docker run>. For more
    -- information, see
    -- <https://docs.docker.com/engine/reference/builder/#cmd>.
    command :: Core.Maybe [Core.Text],
    -- | The log configuration specification for the container.
    --
    -- This parameter maps to @LogConfig@ in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--log-driver@ option to
    -- <https://docs.docker.com/engine/reference/run/ docker run>. By default,
    -- containers use the same logging driver that the Docker daemon uses.
    -- However the container might use a different logging driver than the
    -- Docker daemon by specifying a log driver with this parameter in the
    -- container definition. To use a different logging driver for a container,
    -- the log system must be configured properly on the container instance (or
    -- on a different log server for remote logging options). For more
    -- information on the options for different supported log drivers, see
    -- <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers>
    -- in the Docker documentation.
    --
    -- AWS Batch currently supports a subset of the logging drivers available
    -- to the Docker daemon (shown in the LogConfiguration data type).
    --
    -- This parameter requires version 1.18 of the Docker Remote API or greater
    -- on your container instance. To check the Docker Remote API version on
    -- your container instance, log into your container instance and run the
    -- following command: @sudo docker version | grep \"Server API version\"@
    --
    -- The Amazon ECS container agent running on a container instance must
    -- register the logging drivers available on that instance with the
    -- @ECS_AVAILABLE_LOGGING_DRIVERS@ environment variable before containers
    -- placed on that instance can use these log configuration options. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-config.html Amazon ECS Container Agent Configuration>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    logConfiguration :: Core.Maybe LogConfiguration,
    -- | The type and amount of resources to assign to a container. The supported
    -- resources include @GPU@, @MEMORY@, and @VCPU@.
    resourceRequirements :: Core.Maybe [ResourceRequirement],
    -- | The Amazon Resource Name (ARN) of the IAM role that the container can
    -- assume for AWS permissions. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    jobRoleArn :: Core.Maybe Core.Text,
    -- | When this parameter is true, the container is given read-only access to
    -- its root file system. This parameter maps to @ReadonlyRootfs@ in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--read-only@ option to @docker run@.
    readonlyRootFilesystem :: Core.Maybe Core.Bool,
    -- | A list of @ulimits@ to set in the container. This parameter maps to
    -- @Ulimits@ in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--ulimit@ option to
    -- <https://docs.docker.com/engine/reference/run/ docker run>.
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources
    -- and shouldn\'t be provided.
    ulimits :: Core.Maybe [Ulimit]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ContainerProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'linuxParameters', 'containerProperties_linuxParameters' - Linux-specific modifications that are applied to the container, such as
-- details for device mappings.
--
-- 'memory', 'containerProperties_memory' - This parameter is deprecated and not supported for jobs run on Fargate
-- resources, use @ResourceRequirement@. For jobs run on EC2 resources can
-- specify the memory requirement using the @ResourceRequirement@
-- structure. The hard limit (in MiB) of memory to present to the
-- container. If your container attempts to exceed the memory specified
-- here, the container is killed. This parameter maps to @Memory@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--memory@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>. You must
-- specify at least 4 MiB of memory for a job. This is required but can be
-- specified in several places; it must be specified for each node at least
-- once.
--
-- If you\'re trying to maximize your resource utilization by providing
-- your jobs as much memory as possible for a particular instance type, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/memory-management.html Memory Management>
-- in the /AWS Batch User Guide/.
--
-- 'user', 'containerProperties_user' - The user name to use inside the container. This parameter maps to @User@
-- in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--user@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- 'instanceType', 'containerProperties_instanceType' - The instance type to use for a multi-node parallel job. All node groups
-- in a multi-node parallel job must use the same instance type.
--
-- This parameter isn\'t applicable to single-node container jobs or for
-- jobs running on Fargate resources and shouldn\'t be provided.
--
-- 'networkConfiguration', 'containerProperties_networkConfiguration' - The network configuration for jobs running on Fargate resources. Jobs
-- running on EC2 resources must not specify this parameter.
--
-- 'executionRoleArn', 'containerProperties_executionRoleArn' - The Amazon Resource Name (ARN) of the execution role that AWS Batch can
-- assume. Jobs running on Fargate resources must provide an execution
-- role. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/execution-IAM-role.html AWS Batch execution IAM role>
-- in the /AWS Batch User Guide/.
--
-- 'privileged', 'containerProperties_privileged' - When this parameter is true, the container is given elevated permissions
-- on the host container instance (similar to the @root@ user). This
-- parameter maps to @Privileged@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--privileged@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>. The default
-- value is false.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources
-- and shouldn\'t be provided, or specified as false.
--
-- 'vcpus', 'containerProperties_vcpus' - This parameter is deprecated and not supported for jobs run on Fargate
-- resources, see @resourceRequirement@. The number of vCPUs reserved for
-- the container. Jobs running on EC2 resources can specify the vCPU
-- requirement for the job using @resourceRequirements@ but the vCPU
-- requirements can\'t be specified both here and in the
-- @resourceRequirement@ structure. This parameter maps to @CpuShares@ in
-- the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--cpu-shares@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>. Each vCPU is
-- equivalent to 1,024 CPU shares. You must specify at least one vCPU. This
-- is required but can be specified in several places. It must be specified
-- for each node at least once.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources
-- and shouldn\'t be provided. Jobs running on Fargate resources must
-- specify the vCPU requirement for the job using @resourceRequirements@.
--
-- 'volumes', 'containerProperties_volumes' - A list of data volumes used in a job.
--
-- 'environment', 'containerProperties_environment' - The environment variables to pass to a container. This parameter maps to
-- @Env@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--env@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- We don\'t recommend using plaintext environment variables for sensitive
-- information, such as credential data.
--
-- Environment variables must not start with @AWS_BATCH@; this naming
-- convention is reserved for variables that are set by the AWS Batch
-- service.
--
-- 'fargatePlatformConfiguration', 'containerProperties_fargatePlatformConfiguration' - The platform configuration for jobs running on Fargate resources. Jobs
-- running on EC2 resources must not specify this parameter.
--
-- 'secrets', 'containerProperties_secrets' - The secrets for the container. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/specifying-sensitive-data.html Specifying sensitive data>
-- in the /AWS Batch User Guide/.
--
-- 'mountPoints', 'containerProperties_mountPoints' - The mount points for data volumes in your container. This parameter maps
-- to @Volumes@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--volume@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- 'image', 'containerProperties_image' - The image used to start a container. This string is passed directly to
-- the Docker daemon. Images in the Docker Hub registry are available by
-- default. Other repositories are specified with
-- @ repository-url\/image:tag @. Up to 255 letters (uppercase and
-- lowercase), numbers, hyphens, underscores, colons, periods, forward
-- slashes, and number signs are allowed. This parameter maps to @Image@ in
-- the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @IMAGE@ parameter of
-- <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- Docker image architecture must match the processor architecture of the
-- compute resources that they\'re scheduled on. For example, ARM-based
-- Docker images can only run on ARM-based compute resources.
--
-- -   Images in Amazon ECR repositories use the full registry and
--     repository URI (for example,
--     @012345678910.dkr.ecr.\<region-name>.amazonaws.com\/\<repository-name>@).
--
-- -   Images in official repositories on Docker Hub use a single name (for
--     example, @ubuntu@ or @mongo@).
--
-- -   Images in other repositories on Docker Hub are qualified with an
--     organization name (for example, @amazon\/amazon-ecs-agent@).
--
-- -   Images in other online repositories are qualified further by a
--     domain name (for example, @quay.io\/assemblyline\/ubuntu@).
--
-- 'command', 'containerProperties_command' - The command that\'s passed to the container. This parameter maps to
-- @Cmd@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @COMMAND@ parameter to
-- <https://docs.docker.com/engine/reference/run/ docker run>. For more
-- information, see
-- <https://docs.docker.com/engine/reference/builder/#cmd>.
--
-- 'logConfiguration', 'containerProperties_logConfiguration' - The log configuration specification for the container.
--
-- This parameter maps to @LogConfig@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--log-driver@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>. By default,
-- containers use the same logging driver that the Docker daemon uses.
-- However the container might use a different logging driver than the
-- Docker daemon by specifying a log driver with this parameter in the
-- container definition. To use a different logging driver for a container,
-- the log system must be configured properly on the container instance (or
-- on a different log server for remote logging options). For more
-- information on the options for different supported log drivers, see
-- <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers>
-- in the Docker documentation.
--
-- AWS Batch currently supports a subset of the logging drivers available
-- to the Docker daemon (shown in the LogConfiguration data type).
--
-- This parameter requires version 1.18 of the Docker Remote API or greater
-- on your container instance. To check the Docker Remote API version on
-- your container instance, log into your container instance and run the
-- following command: @sudo docker version | grep \"Server API version\"@
--
-- The Amazon ECS container agent running on a container instance must
-- register the logging drivers available on that instance with the
-- @ECS_AVAILABLE_LOGGING_DRIVERS@ environment variable before containers
-- placed on that instance can use these log configuration options. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-config.html Amazon ECS Container Agent Configuration>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'resourceRequirements', 'containerProperties_resourceRequirements' - The type and amount of resources to assign to a container. The supported
-- resources include @GPU@, @MEMORY@, and @VCPU@.
--
-- 'jobRoleArn', 'containerProperties_jobRoleArn' - The Amazon Resource Name (ARN) of the IAM role that the container can
-- assume for AWS permissions. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'readonlyRootFilesystem', 'containerProperties_readonlyRootFilesystem' - When this parameter is true, the container is given read-only access to
-- its root file system. This parameter maps to @ReadonlyRootfs@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--read-only@ option to @docker run@.
--
-- 'ulimits', 'containerProperties_ulimits' - A list of @ulimits@ to set in the container. This parameter maps to
-- @Ulimits@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--ulimit@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources
-- and shouldn\'t be provided.
newContainerProperties ::
  ContainerProperties
newContainerProperties =
  ContainerProperties'
    { linuxParameters =
        Core.Nothing,
      memory = Core.Nothing,
      user = Core.Nothing,
      instanceType = Core.Nothing,
      networkConfiguration = Core.Nothing,
      executionRoleArn = Core.Nothing,
      privileged = Core.Nothing,
      vcpus = Core.Nothing,
      volumes = Core.Nothing,
      environment = Core.Nothing,
      fargatePlatformConfiguration = Core.Nothing,
      secrets = Core.Nothing,
      mountPoints = Core.Nothing,
      image = Core.Nothing,
      command = Core.Nothing,
      logConfiguration = Core.Nothing,
      resourceRequirements = Core.Nothing,
      jobRoleArn = Core.Nothing,
      readonlyRootFilesystem = Core.Nothing,
      ulimits = Core.Nothing
    }

-- | Linux-specific modifications that are applied to the container, such as
-- details for device mappings.
containerProperties_linuxParameters :: Lens.Lens' ContainerProperties (Core.Maybe LinuxParameters)
containerProperties_linuxParameters = Lens.lens (\ContainerProperties' {linuxParameters} -> linuxParameters) (\s@ContainerProperties' {} a -> s {linuxParameters = a} :: ContainerProperties)

-- | This parameter is deprecated and not supported for jobs run on Fargate
-- resources, use @ResourceRequirement@. For jobs run on EC2 resources can
-- specify the memory requirement using the @ResourceRequirement@
-- structure. The hard limit (in MiB) of memory to present to the
-- container. If your container attempts to exceed the memory specified
-- here, the container is killed. This parameter maps to @Memory@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--memory@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>. You must
-- specify at least 4 MiB of memory for a job. This is required but can be
-- specified in several places; it must be specified for each node at least
-- once.
--
-- If you\'re trying to maximize your resource utilization by providing
-- your jobs as much memory as possible for a particular instance type, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/memory-management.html Memory Management>
-- in the /AWS Batch User Guide/.
containerProperties_memory :: Lens.Lens' ContainerProperties (Core.Maybe Core.Int)
containerProperties_memory = Lens.lens (\ContainerProperties' {memory} -> memory) (\s@ContainerProperties' {} a -> s {memory = a} :: ContainerProperties)

-- | The user name to use inside the container. This parameter maps to @User@
-- in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--user@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
containerProperties_user :: Lens.Lens' ContainerProperties (Core.Maybe Core.Text)
containerProperties_user = Lens.lens (\ContainerProperties' {user} -> user) (\s@ContainerProperties' {} a -> s {user = a} :: ContainerProperties)

-- | The instance type to use for a multi-node parallel job. All node groups
-- in a multi-node parallel job must use the same instance type.
--
-- This parameter isn\'t applicable to single-node container jobs or for
-- jobs running on Fargate resources and shouldn\'t be provided.
containerProperties_instanceType :: Lens.Lens' ContainerProperties (Core.Maybe Core.Text)
containerProperties_instanceType = Lens.lens (\ContainerProperties' {instanceType} -> instanceType) (\s@ContainerProperties' {} a -> s {instanceType = a} :: ContainerProperties)

-- | The network configuration for jobs running on Fargate resources. Jobs
-- running on EC2 resources must not specify this parameter.
containerProperties_networkConfiguration :: Lens.Lens' ContainerProperties (Core.Maybe NetworkConfiguration)
containerProperties_networkConfiguration = Lens.lens (\ContainerProperties' {networkConfiguration} -> networkConfiguration) (\s@ContainerProperties' {} a -> s {networkConfiguration = a} :: ContainerProperties)

-- | The Amazon Resource Name (ARN) of the execution role that AWS Batch can
-- assume. Jobs running on Fargate resources must provide an execution
-- role. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/execution-IAM-role.html AWS Batch execution IAM role>
-- in the /AWS Batch User Guide/.
containerProperties_executionRoleArn :: Lens.Lens' ContainerProperties (Core.Maybe Core.Text)
containerProperties_executionRoleArn = Lens.lens (\ContainerProperties' {executionRoleArn} -> executionRoleArn) (\s@ContainerProperties' {} a -> s {executionRoleArn = a} :: ContainerProperties)

-- | When this parameter is true, the container is given elevated permissions
-- on the host container instance (similar to the @root@ user). This
-- parameter maps to @Privileged@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--privileged@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>. The default
-- value is false.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources
-- and shouldn\'t be provided, or specified as false.
containerProperties_privileged :: Lens.Lens' ContainerProperties (Core.Maybe Core.Bool)
containerProperties_privileged = Lens.lens (\ContainerProperties' {privileged} -> privileged) (\s@ContainerProperties' {} a -> s {privileged = a} :: ContainerProperties)

-- | This parameter is deprecated and not supported for jobs run on Fargate
-- resources, see @resourceRequirement@. The number of vCPUs reserved for
-- the container. Jobs running on EC2 resources can specify the vCPU
-- requirement for the job using @resourceRequirements@ but the vCPU
-- requirements can\'t be specified both here and in the
-- @resourceRequirement@ structure. This parameter maps to @CpuShares@ in
-- the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--cpu-shares@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>. Each vCPU is
-- equivalent to 1,024 CPU shares. You must specify at least one vCPU. This
-- is required but can be specified in several places. It must be specified
-- for each node at least once.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources
-- and shouldn\'t be provided. Jobs running on Fargate resources must
-- specify the vCPU requirement for the job using @resourceRequirements@.
containerProperties_vcpus :: Lens.Lens' ContainerProperties (Core.Maybe Core.Int)
containerProperties_vcpus = Lens.lens (\ContainerProperties' {vcpus} -> vcpus) (\s@ContainerProperties' {} a -> s {vcpus = a} :: ContainerProperties)

-- | A list of data volumes used in a job.
containerProperties_volumes :: Lens.Lens' ContainerProperties (Core.Maybe [Volume])
containerProperties_volumes = Lens.lens (\ContainerProperties' {volumes} -> volumes) (\s@ContainerProperties' {} a -> s {volumes = a} :: ContainerProperties) Core.. Lens.mapping Lens._Coerce

-- | The environment variables to pass to a container. This parameter maps to
-- @Env@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--env@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- We don\'t recommend using plaintext environment variables for sensitive
-- information, such as credential data.
--
-- Environment variables must not start with @AWS_BATCH@; this naming
-- convention is reserved for variables that are set by the AWS Batch
-- service.
containerProperties_environment :: Lens.Lens' ContainerProperties (Core.Maybe [KeyValuePair])
containerProperties_environment = Lens.lens (\ContainerProperties' {environment} -> environment) (\s@ContainerProperties' {} a -> s {environment = a} :: ContainerProperties) Core.. Lens.mapping Lens._Coerce

-- | The platform configuration for jobs running on Fargate resources. Jobs
-- running on EC2 resources must not specify this parameter.
containerProperties_fargatePlatformConfiguration :: Lens.Lens' ContainerProperties (Core.Maybe FargatePlatformConfiguration)
containerProperties_fargatePlatformConfiguration = Lens.lens (\ContainerProperties' {fargatePlatformConfiguration} -> fargatePlatformConfiguration) (\s@ContainerProperties' {} a -> s {fargatePlatformConfiguration = a} :: ContainerProperties)

-- | The secrets for the container. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/specifying-sensitive-data.html Specifying sensitive data>
-- in the /AWS Batch User Guide/.
containerProperties_secrets :: Lens.Lens' ContainerProperties (Core.Maybe [Secret])
containerProperties_secrets = Lens.lens (\ContainerProperties' {secrets} -> secrets) (\s@ContainerProperties' {} a -> s {secrets = a} :: ContainerProperties) Core.. Lens.mapping Lens._Coerce

-- | The mount points for data volumes in your container. This parameter maps
-- to @Volumes@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--volume@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
containerProperties_mountPoints :: Lens.Lens' ContainerProperties (Core.Maybe [MountPoint])
containerProperties_mountPoints = Lens.lens (\ContainerProperties' {mountPoints} -> mountPoints) (\s@ContainerProperties' {} a -> s {mountPoints = a} :: ContainerProperties) Core.. Lens.mapping Lens._Coerce

-- | The image used to start a container. This string is passed directly to
-- the Docker daemon. Images in the Docker Hub registry are available by
-- default. Other repositories are specified with
-- @ repository-url\/image:tag @. Up to 255 letters (uppercase and
-- lowercase), numbers, hyphens, underscores, colons, periods, forward
-- slashes, and number signs are allowed. This parameter maps to @Image@ in
-- the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @IMAGE@ parameter of
-- <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- Docker image architecture must match the processor architecture of the
-- compute resources that they\'re scheduled on. For example, ARM-based
-- Docker images can only run on ARM-based compute resources.
--
-- -   Images in Amazon ECR repositories use the full registry and
--     repository URI (for example,
--     @012345678910.dkr.ecr.\<region-name>.amazonaws.com\/\<repository-name>@).
--
-- -   Images in official repositories on Docker Hub use a single name (for
--     example, @ubuntu@ or @mongo@).
--
-- -   Images in other repositories on Docker Hub are qualified with an
--     organization name (for example, @amazon\/amazon-ecs-agent@).
--
-- -   Images in other online repositories are qualified further by a
--     domain name (for example, @quay.io\/assemblyline\/ubuntu@).
containerProperties_image :: Lens.Lens' ContainerProperties (Core.Maybe Core.Text)
containerProperties_image = Lens.lens (\ContainerProperties' {image} -> image) (\s@ContainerProperties' {} a -> s {image = a} :: ContainerProperties)

-- | The command that\'s passed to the container. This parameter maps to
-- @Cmd@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @COMMAND@ parameter to
-- <https://docs.docker.com/engine/reference/run/ docker run>. For more
-- information, see
-- <https://docs.docker.com/engine/reference/builder/#cmd>.
containerProperties_command :: Lens.Lens' ContainerProperties (Core.Maybe [Core.Text])
containerProperties_command = Lens.lens (\ContainerProperties' {command} -> command) (\s@ContainerProperties' {} a -> s {command = a} :: ContainerProperties) Core.. Lens.mapping Lens._Coerce

-- | The log configuration specification for the container.
--
-- This parameter maps to @LogConfig@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--log-driver@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>. By default,
-- containers use the same logging driver that the Docker daemon uses.
-- However the container might use a different logging driver than the
-- Docker daemon by specifying a log driver with this parameter in the
-- container definition. To use a different logging driver for a container,
-- the log system must be configured properly on the container instance (or
-- on a different log server for remote logging options). For more
-- information on the options for different supported log drivers, see
-- <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers>
-- in the Docker documentation.
--
-- AWS Batch currently supports a subset of the logging drivers available
-- to the Docker daemon (shown in the LogConfiguration data type).
--
-- This parameter requires version 1.18 of the Docker Remote API or greater
-- on your container instance. To check the Docker Remote API version on
-- your container instance, log into your container instance and run the
-- following command: @sudo docker version | grep \"Server API version\"@
--
-- The Amazon ECS container agent running on a container instance must
-- register the logging drivers available on that instance with the
-- @ECS_AVAILABLE_LOGGING_DRIVERS@ environment variable before containers
-- placed on that instance can use these log configuration options. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-config.html Amazon ECS Container Agent Configuration>
-- in the /Amazon Elastic Container Service Developer Guide/.
containerProperties_logConfiguration :: Lens.Lens' ContainerProperties (Core.Maybe LogConfiguration)
containerProperties_logConfiguration = Lens.lens (\ContainerProperties' {logConfiguration} -> logConfiguration) (\s@ContainerProperties' {} a -> s {logConfiguration = a} :: ContainerProperties)

-- | The type and amount of resources to assign to a container. The supported
-- resources include @GPU@, @MEMORY@, and @VCPU@.
containerProperties_resourceRequirements :: Lens.Lens' ContainerProperties (Core.Maybe [ResourceRequirement])
containerProperties_resourceRequirements = Lens.lens (\ContainerProperties' {resourceRequirements} -> resourceRequirements) (\s@ContainerProperties' {} a -> s {resourceRequirements = a} :: ContainerProperties) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the IAM role that the container can
-- assume for AWS permissions. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks>
-- in the /Amazon Elastic Container Service Developer Guide/.
containerProperties_jobRoleArn :: Lens.Lens' ContainerProperties (Core.Maybe Core.Text)
containerProperties_jobRoleArn = Lens.lens (\ContainerProperties' {jobRoleArn} -> jobRoleArn) (\s@ContainerProperties' {} a -> s {jobRoleArn = a} :: ContainerProperties)

-- | When this parameter is true, the container is given read-only access to
-- its root file system. This parameter maps to @ReadonlyRootfs@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--read-only@ option to @docker run@.
containerProperties_readonlyRootFilesystem :: Lens.Lens' ContainerProperties (Core.Maybe Core.Bool)
containerProperties_readonlyRootFilesystem = Lens.lens (\ContainerProperties' {readonlyRootFilesystem} -> readonlyRootFilesystem) (\s@ContainerProperties' {} a -> s {readonlyRootFilesystem = a} :: ContainerProperties)

-- | A list of @ulimits@ to set in the container. This parameter maps to
-- @Ulimits@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--ulimit@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources
-- and shouldn\'t be provided.
containerProperties_ulimits :: Lens.Lens' ContainerProperties (Core.Maybe [Ulimit])
containerProperties_ulimits = Lens.lens (\ContainerProperties' {ulimits} -> ulimits) (\s@ContainerProperties' {} a -> s {ulimits = a} :: ContainerProperties) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON ContainerProperties where
  parseJSON =
    Core.withObject
      "ContainerProperties"
      ( \x ->
          ContainerProperties'
            Core.<$> (x Core..:? "linuxParameters")
            Core.<*> (x Core..:? "memory")
            Core.<*> (x Core..:? "user")
            Core.<*> (x Core..:? "instanceType")
            Core.<*> (x Core..:? "networkConfiguration")
            Core.<*> (x Core..:? "executionRoleArn")
            Core.<*> (x Core..:? "privileged")
            Core.<*> (x Core..:? "vcpus")
            Core.<*> (x Core..:? "volumes" Core..!= Core.mempty)
            Core.<*> (x Core..:? "environment" Core..!= Core.mempty)
            Core.<*> (x Core..:? "fargatePlatformConfiguration")
            Core.<*> (x Core..:? "secrets" Core..!= Core.mempty)
            Core.<*> (x Core..:? "mountPoints" Core..!= Core.mempty)
            Core.<*> (x Core..:? "image")
            Core.<*> (x Core..:? "command" Core..!= Core.mempty)
            Core.<*> (x Core..:? "logConfiguration")
            Core.<*> ( x Core..:? "resourceRequirements"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "jobRoleArn")
            Core.<*> (x Core..:? "readonlyRootFilesystem")
            Core.<*> (x Core..:? "ulimits" Core..!= Core.mempty)
      )

instance Core.Hashable ContainerProperties

instance Core.NFData ContainerProperties

instance Core.ToJSON ContainerProperties where
  toJSON ContainerProperties' {..} =
    Core.object
      ( Core.catMaybes
          [ ("linuxParameters" Core..=)
              Core.<$> linuxParameters,
            ("memory" Core..=) Core.<$> memory,
            ("user" Core..=) Core.<$> user,
            ("instanceType" Core..=) Core.<$> instanceType,
            ("networkConfiguration" Core..=)
              Core.<$> networkConfiguration,
            ("executionRoleArn" Core..=)
              Core.<$> executionRoleArn,
            ("privileged" Core..=) Core.<$> privileged,
            ("vcpus" Core..=) Core.<$> vcpus,
            ("volumes" Core..=) Core.<$> volumes,
            ("environment" Core..=) Core.<$> environment,
            ("fargatePlatformConfiguration" Core..=)
              Core.<$> fargatePlatformConfiguration,
            ("secrets" Core..=) Core.<$> secrets,
            ("mountPoints" Core..=) Core.<$> mountPoints,
            ("image" Core..=) Core.<$> image,
            ("command" Core..=) Core.<$> command,
            ("logConfiguration" Core..=)
              Core.<$> logConfiguration,
            ("resourceRequirements" Core..=)
              Core.<$> resourceRequirements,
            ("jobRoleArn" Core..=) Core.<$> jobRoleArn,
            ("readonlyRootFilesystem" Core..=)
              Core.<$> readonlyRootFilesystem,
            ("ulimits" Core..=) Core.<$> ulimits
          ]
      )
