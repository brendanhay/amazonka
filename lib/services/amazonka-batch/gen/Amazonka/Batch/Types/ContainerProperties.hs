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
-- Module      : Amazonka.Batch.Types.ContainerProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.ContainerProperties where

import Amazonka.Batch.Types.FargatePlatformConfiguration
import Amazonka.Batch.Types.KeyValuePair
import Amazonka.Batch.Types.LinuxParameters
import Amazonka.Batch.Types.LogConfiguration
import Amazonka.Batch.Types.MountPoint
import Amazonka.Batch.Types.NetworkConfiguration
import Amazonka.Batch.Types.ResourceRequirement
import Amazonka.Batch.Types.Secret
import Amazonka.Batch.Types.Ulimit
import Amazonka.Batch.Types.Volume
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Container properties are used in job definitions to describe the
-- container that\'s launched as part of a job.
--
-- /See:/ 'newContainerProperties' smart constructor.
data ContainerProperties = ContainerProperties'
  { -- | The image used to start a container. This string is passed directly to
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
    image :: Prelude.Maybe Prelude.Text,
    -- | The command that\'s passed to the container. This parameter maps to
    -- @Cmd@ in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @COMMAND@ parameter to
    -- <https://docs.docker.com/engine/reference/run/ docker run>. For more
    -- information, see
    -- <https://docs.docker.com/engine/reference/builder/#cmd>.
    command :: Prelude.Maybe [Prelude.Text],
    -- | The secrets for the container. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/specifying-sensitive-data.html Specifying sensitive data>
    -- in the /Batch User Guide/.
    secrets :: Prelude.Maybe [Secret],
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
    -- convention is reserved for variables that are set by the Batch service.
    environment :: Prelude.Maybe [KeyValuePair],
    -- | A list of @ulimits@ to set in the container. This parameter maps to
    -- @Ulimits@ in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--ulimit@ option to
    -- <https://docs.docker.com/engine/reference/run/ docker run>.
    --
    -- This parameter isn\'t applicable to jobs that are running on Fargate
    -- resources and shouldn\'t be provided.
    ulimits :: Prelude.Maybe [Ulimit],
    -- | The Amazon Resource Name (ARN) of the execution role that Batch can
    -- assume. For jobs that run on Fargate resources, you must provide an
    -- execution role. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/execution-IAM-role.html Batch execution IAM role>
    -- in the /Batch User Guide/.
    executionRoleArn :: Prelude.Maybe Prelude.Text,
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
    -- This parameter isn\'t applicable to jobs that are running on Fargate
    -- resources and shouldn\'t be provided, or specified as false.
    privileged :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the IAM role that the container can
    -- assume for Amazon Web Services permissions. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    jobRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The type and amount of resources to assign to a container. The supported
    -- resources include @GPU@, @MEMORY@, and @VCPU@.
    resourceRequirements :: Prelude.Maybe [ResourceRequirement],
    -- | The instance type to use for a multi-node parallel job. All node groups
    -- in a multi-node parallel job must use the same instance type.
    --
    -- This parameter isn\'t applicable to single-node container jobs or jobs
    -- that run on Fargate resources, and shouldn\'t be provided.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | This parameter indicates the memory hard limit (in MiB) for a container.
    -- If your container attempts to exceed the specified number, it\'s
    -- terminated. You must specify at least 4 MiB of memory for a job using
    -- this parameter. The memory hard limit can be specified in several
    -- places. It must be specified for each node at least once.
    --
    -- This parameter maps to @Memory@ in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--memory@ option to
    -- <https://docs.docker.com/engine/reference/run/ docker run>.
    --
    -- This parameter is supported on EC2 resources but isn\'t supported on
    -- Fargate resources. For Fargate resources, you should specify the memory
    -- requirement using @resourceRequirement@. You can also do this for EC2
    -- resources.
    --
    -- If you\'re trying to maximize your resource utilization by providing
    -- your jobs as much memory as possible for a particular instance type, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/memory-management.html Memory Management>
    -- in the /Batch User Guide/.
    memory :: Prelude.Maybe Prelude.Int,
    -- | The user name to use inside the container. This parameter maps to @User@
    -- in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--user@ option to
    -- <https://docs.docker.com/engine/reference/run/ docker run>.
    user :: Prelude.Maybe Prelude.Text,
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
    -- Batch currently supports a subset of the logging drivers available to
    -- the Docker daemon (shown in the LogConfiguration data type).
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
    logConfiguration :: Prelude.Maybe LogConfiguration,
    -- | Linux-specific modifications that are applied to the container, such as
    -- details for device mappings.
    linuxParameters :: Prelude.Maybe LinuxParameters,
    -- | The mount points for data volumes in your container. This parameter maps
    -- to @Volumes@ in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--volume@ option to
    -- <https://docs.docker.com/engine/reference/run/ docker run>.
    mountPoints :: Prelude.Maybe [MountPoint],
    -- | The platform configuration for jobs that are running on Fargate
    -- resources. Jobs that are running on EC2 resources must not specify this
    -- parameter.
    fargatePlatformConfiguration :: Prelude.Maybe FargatePlatformConfiguration,
    -- | The number of vCPUs reserved for the job. Each vCPU is equivalent to
    -- 1,024 CPU shares. This parameter maps to @CpuShares@ in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--cpu-shares@ option to
    -- <https://docs.docker.com/engine/reference/run/ docker run>. The number
    -- of vCPUs must be specified but can be specified in several places. You
    -- must specify it at least once for each node.
    --
    -- This parameter is supported on EC2 resources but isn\'t supported for
    -- jobs that run on Fargate resources. For these resources, use
    -- @resourceRequirement@ instead. You can use this parameter or
    -- @resourceRequirements@ structure but not both.
    --
    -- This parameter isn\'t applicable to jobs that are running on Fargate
    -- resources and shouldn\'t be provided. For jobs that run on Fargate
    -- resources, you must specify the vCPU requirement for the job using
    -- @resourceRequirements@.
    vcpus :: Prelude.Maybe Prelude.Int,
    -- | When this parameter is true, the container is given read-only access to
    -- its root file system. This parameter maps to @ReadonlyRootfs@ in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--read-only@ option to @docker run@.
    readonlyRootFilesystem :: Prelude.Maybe Prelude.Bool,
    -- | A list of data volumes used in a job.
    volumes :: Prelude.Maybe [Volume],
    -- | The network configuration for jobs that are running on Fargate
    -- resources. Jobs that are running on EC2 resources must not specify this
    -- parameter.
    networkConfiguration :: Prelude.Maybe NetworkConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'secrets', 'containerProperties_secrets' - The secrets for the container. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/specifying-sensitive-data.html Specifying sensitive data>
-- in the /Batch User Guide/.
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
-- convention is reserved for variables that are set by the Batch service.
--
-- 'ulimits', 'containerProperties_ulimits' - A list of @ulimits@ to set in the container. This parameter maps to
-- @Ulimits@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--ulimit@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources and shouldn\'t be provided.
--
-- 'executionRoleArn', 'containerProperties_executionRoleArn' - The Amazon Resource Name (ARN) of the execution role that Batch can
-- assume. For jobs that run on Fargate resources, you must provide an
-- execution role. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/execution-IAM-role.html Batch execution IAM role>
-- in the /Batch User Guide/.
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
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources and shouldn\'t be provided, or specified as false.
--
-- 'jobRoleArn', 'containerProperties_jobRoleArn' - The Amazon Resource Name (ARN) of the IAM role that the container can
-- assume for Amazon Web Services permissions. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'resourceRequirements', 'containerProperties_resourceRequirements' - The type and amount of resources to assign to a container. The supported
-- resources include @GPU@, @MEMORY@, and @VCPU@.
--
-- 'instanceType', 'containerProperties_instanceType' - The instance type to use for a multi-node parallel job. All node groups
-- in a multi-node parallel job must use the same instance type.
--
-- This parameter isn\'t applicable to single-node container jobs or jobs
-- that run on Fargate resources, and shouldn\'t be provided.
--
-- 'memory', 'containerProperties_memory' - This parameter indicates the memory hard limit (in MiB) for a container.
-- If your container attempts to exceed the specified number, it\'s
-- terminated. You must specify at least 4 MiB of memory for a job using
-- this parameter. The memory hard limit can be specified in several
-- places. It must be specified for each node at least once.
--
-- This parameter maps to @Memory@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--memory@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- This parameter is supported on EC2 resources but isn\'t supported on
-- Fargate resources. For Fargate resources, you should specify the memory
-- requirement using @resourceRequirement@. You can also do this for EC2
-- resources.
--
-- If you\'re trying to maximize your resource utilization by providing
-- your jobs as much memory as possible for a particular instance type, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/memory-management.html Memory Management>
-- in the /Batch User Guide/.
--
-- 'user', 'containerProperties_user' - The user name to use inside the container. This parameter maps to @User@
-- in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--user@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
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
-- Batch currently supports a subset of the logging drivers available to
-- the Docker daemon (shown in the LogConfiguration data type).
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
-- 'linuxParameters', 'containerProperties_linuxParameters' - Linux-specific modifications that are applied to the container, such as
-- details for device mappings.
--
-- 'mountPoints', 'containerProperties_mountPoints' - The mount points for data volumes in your container. This parameter maps
-- to @Volumes@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--volume@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- 'fargatePlatformConfiguration', 'containerProperties_fargatePlatformConfiguration' - The platform configuration for jobs that are running on Fargate
-- resources. Jobs that are running on EC2 resources must not specify this
-- parameter.
--
-- 'vcpus', 'containerProperties_vcpus' - The number of vCPUs reserved for the job. Each vCPU is equivalent to
-- 1,024 CPU shares. This parameter maps to @CpuShares@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--cpu-shares@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>. The number
-- of vCPUs must be specified but can be specified in several places. You
-- must specify it at least once for each node.
--
-- This parameter is supported on EC2 resources but isn\'t supported for
-- jobs that run on Fargate resources. For these resources, use
-- @resourceRequirement@ instead. You can use this parameter or
-- @resourceRequirements@ structure but not both.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources and shouldn\'t be provided. For jobs that run on Fargate
-- resources, you must specify the vCPU requirement for the job using
-- @resourceRequirements@.
--
-- 'readonlyRootFilesystem', 'containerProperties_readonlyRootFilesystem' - When this parameter is true, the container is given read-only access to
-- its root file system. This parameter maps to @ReadonlyRootfs@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--read-only@ option to @docker run@.
--
-- 'volumes', 'containerProperties_volumes' - A list of data volumes used in a job.
--
-- 'networkConfiguration', 'containerProperties_networkConfiguration' - The network configuration for jobs that are running on Fargate
-- resources. Jobs that are running on EC2 resources must not specify this
-- parameter.
newContainerProperties ::
  ContainerProperties
newContainerProperties =
  ContainerProperties'
    { image = Prelude.Nothing,
      command = Prelude.Nothing,
      secrets = Prelude.Nothing,
      environment = Prelude.Nothing,
      ulimits = Prelude.Nothing,
      executionRoleArn = Prelude.Nothing,
      privileged = Prelude.Nothing,
      jobRoleArn = Prelude.Nothing,
      resourceRequirements = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      memory = Prelude.Nothing,
      user = Prelude.Nothing,
      logConfiguration = Prelude.Nothing,
      linuxParameters = Prelude.Nothing,
      mountPoints = Prelude.Nothing,
      fargatePlatformConfiguration = Prelude.Nothing,
      vcpus = Prelude.Nothing,
      readonlyRootFilesystem = Prelude.Nothing,
      volumes = Prelude.Nothing,
      networkConfiguration = Prelude.Nothing
    }

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
containerProperties_image :: Lens.Lens' ContainerProperties (Prelude.Maybe Prelude.Text)
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
containerProperties_command :: Lens.Lens' ContainerProperties (Prelude.Maybe [Prelude.Text])
containerProperties_command = Lens.lens (\ContainerProperties' {command} -> command) (\s@ContainerProperties' {} a -> s {command = a} :: ContainerProperties) Prelude.. Lens.mapping Lens.coerced

-- | The secrets for the container. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/specifying-sensitive-data.html Specifying sensitive data>
-- in the /Batch User Guide/.
containerProperties_secrets :: Lens.Lens' ContainerProperties (Prelude.Maybe [Secret])
containerProperties_secrets = Lens.lens (\ContainerProperties' {secrets} -> secrets) (\s@ContainerProperties' {} a -> s {secrets = a} :: ContainerProperties) Prelude.. Lens.mapping Lens.coerced

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
-- convention is reserved for variables that are set by the Batch service.
containerProperties_environment :: Lens.Lens' ContainerProperties (Prelude.Maybe [KeyValuePair])
containerProperties_environment = Lens.lens (\ContainerProperties' {environment} -> environment) (\s@ContainerProperties' {} a -> s {environment = a} :: ContainerProperties) Prelude.. Lens.mapping Lens.coerced

-- | A list of @ulimits@ to set in the container. This parameter maps to
-- @Ulimits@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--ulimit@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources and shouldn\'t be provided.
containerProperties_ulimits :: Lens.Lens' ContainerProperties (Prelude.Maybe [Ulimit])
containerProperties_ulimits = Lens.lens (\ContainerProperties' {ulimits} -> ulimits) (\s@ContainerProperties' {} a -> s {ulimits = a} :: ContainerProperties) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the execution role that Batch can
-- assume. For jobs that run on Fargate resources, you must provide an
-- execution role. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/execution-IAM-role.html Batch execution IAM role>
-- in the /Batch User Guide/.
containerProperties_executionRoleArn :: Lens.Lens' ContainerProperties (Prelude.Maybe Prelude.Text)
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
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources and shouldn\'t be provided, or specified as false.
containerProperties_privileged :: Lens.Lens' ContainerProperties (Prelude.Maybe Prelude.Bool)
containerProperties_privileged = Lens.lens (\ContainerProperties' {privileged} -> privileged) (\s@ContainerProperties' {} a -> s {privileged = a} :: ContainerProperties)

-- | The Amazon Resource Name (ARN) of the IAM role that the container can
-- assume for Amazon Web Services permissions. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks>
-- in the /Amazon Elastic Container Service Developer Guide/.
containerProperties_jobRoleArn :: Lens.Lens' ContainerProperties (Prelude.Maybe Prelude.Text)
containerProperties_jobRoleArn = Lens.lens (\ContainerProperties' {jobRoleArn} -> jobRoleArn) (\s@ContainerProperties' {} a -> s {jobRoleArn = a} :: ContainerProperties)

-- | The type and amount of resources to assign to a container. The supported
-- resources include @GPU@, @MEMORY@, and @VCPU@.
containerProperties_resourceRequirements :: Lens.Lens' ContainerProperties (Prelude.Maybe [ResourceRequirement])
containerProperties_resourceRequirements = Lens.lens (\ContainerProperties' {resourceRequirements} -> resourceRequirements) (\s@ContainerProperties' {} a -> s {resourceRequirements = a} :: ContainerProperties) Prelude.. Lens.mapping Lens.coerced

-- | The instance type to use for a multi-node parallel job. All node groups
-- in a multi-node parallel job must use the same instance type.
--
-- This parameter isn\'t applicable to single-node container jobs or jobs
-- that run on Fargate resources, and shouldn\'t be provided.
containerProperties_instanceType :: Lens.Lens' ContainerProperties (Prelude.Maybe Prelude.Text)
containerProperties_instanceType = Lens.lens (\ContainerProperties' {instanceType} -> instanceType) (\s@ContainerProperties' {} a -> s {instanceType = a} :: ContainerProperties)

-- | This parameter indicates the memory hard limit (in MiB) for a container.
-- If your container attempts to exceed the specified number, it\'s
-- terminated. You must specify at least 4 MiB of memory for a job using
-- this parameter. The memory hard limit can be specified in several
-- places. It must be specified for each node at least once.
--
-- This parameter maps to @Memory@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--memory@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- This parameter is supported on EC2 resources but isn\'t supported on
-- Fargate resources. For Fargate resources, you should specify the memory
-- requirement using @resourceRequirement@. You can also do this for EC2
-- resources.
--
-- If you\'re trying to maximize your resource utilization by providing
-- your jobs as much memory as possible for a particular instance type, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/memory-management.html Memory Management>
-- in the /Batch User Guide/.
containerProperties_memory :: Lens.Lens' ContainerProperties (Prelude.Maybe Prelude.Int)
containerProperties_memory = Lens.lens (\ContainerProperties' {memory} -> memory) (\s@ContainerProperties' {} a -> s {memory = a} :: ContainerProperties)

-- | The user name to use inside the container. This parameter maps to @User@
-- in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--user@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
containerProperties_user :: Lens.Lens' ContainerProperties (Prelude.Maybe Prelude.Text)
containerProperties_user = Lens.lens (\ContainerProperties' {user} -> user) (\s@ContainerProperties' {} a -> s {user = a} :: ContainerProperties)

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
-- Batch currently supports a subset of the logging drivers available to
-- the Docker daemon (shown in the LogConfiguration data type).
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
containerProperties_logConfiguration :: Lens.Lens' ContainerProperties (Prelude.Maybe LogConfiguration)
containerProperties_logConfiguration = Lens.lens (\ContainerProperties' {logConfiguration} -> logConfiguration) (\s@ContainerProperties' {} a -> s {logConfiguration = a} :: ContainerProperties)

-- | Linux-specific modifications that are applied to the container, such as
-- details for device mappings.
containerProperties_linuxParameters :: Lens.Lens' ContainerProperties (Prelude.Maybe LinuxParameters)
containerProperties_linuxParameters = Lens.lens (\ContainerProperties' {linuxParameters} -> linuxParameters) (\s@ContainerProperties' {} a -> s {linuxParameters = a} :: ContainerProperties)

-- | The mount points for data volumes in your container. This parameter maps
-- to @Volumes@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--volume@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
containerProperties_mountPoints :: Lens.Lens' ContainerProperties (Prelude.Maybe [MountPoint])
containerProperties_mountPoints = Lens.lens (\ContainerProperties' {mountPoints} -> mountPoints) (\s@ContainerProperties' {} a -> s {mountPoints = a} :: ContainerProperties) Prelude.. Lens.mapping Lens.coerced

-- | The platform configuration for jobs that are running on Fargate
-- resources. Jobs that are running on EC2 resources must not specify this
-- parameter.
containerProperties_fargatePlatformConfiguration :: Lens.Lens' ContainerProperties (Prelude.Maybe FargatePlatformConfiguration)
containerProperties_fargatePlatformConfiguration = Lens.lens (\ContainerProperties' {fargatePlatformConfiguration} -> fargatePlatformConfiguration) (\s@ContainerProperties' {} a -> s {fargatePlatformConfiguration = a} :: ContainerProperties)

-- | The number of vCPUs reserved for the job. Each vCPU is equivalent to
-- 1,024 CPU shares. This parameter maps to @CpuShares@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--cpu-shares@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>. The number
-- of vCPUs must be specified but can be specified in several places. You
-- must specify it at least once for each node.
--
-- This parameter is supported on EC2 resources but isn\'t supported for
-- jobs that run on Fargate resources. For these resources, use
-- @resourceRequirement@ instead. You can use this parameter or
-- @resourceRequirements@ structure but not both.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources and shouldn\'t be provided. For jobs that run on Fargate
-- resources, you must specify the vCPU requirement for the job using
-- @resourceRequirements@.
containerProperties_vcpus :: Lens.Lens' ContainerProperties (Prelude.Maybe Prelude.Int)
containerProperties_vcpus = Lens.lens (\ContainerProperties' {vcpus} -> vcpus) (\s@ContainerProperties' {} a -> s {vcpus = a} :: ContainerProperties)

-- | When this parameter is true, the container is given read-only access to
-- its root file system. This parameter maps to @ReadonlyRootfs@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--read-only@ option to @docker run@.
containerProperties_readonlyRootFilesystem :: Lens.Lens' ContainerProperties (Prelude.Maybe Prelude.Bool)
containerProperties_readonlyRootFilesystem = Lens.lens (\ContainerProperties' {readonlyRootFilesystem} -> readonlyRootFilesystem) (\s@ContainerProperties' {} a -> s {readonlyRootFilesystem = a} :: ContainerProperties)

-- | A list of data volumes used in a job.
containerProperties_volumes :: Lens.Lens' ContainerProperties (Prelude.Maybe [Volume])
containerProperties_volumes = Lens.lens (\ContainerProperties' {volumes} -> volumes) (\s@ContainerProperties' {} a -> s {volumes = a} :: ContainerProperties) Prelude.. Lens.mapping Lens.coerced

-- | The network configuration for jobs that are running on Fargate
-- resources. Jobs that are running on EC2 resources must not specify this
-- parameter.
containerProperties_networkConfiguration :: Lens.Lens' ContainerProperties (Prelude.Maybe NetworkConfiguration)
containerProperties_networkConfiguration = Lens.lens (\ContainerProperties' {networkConfiguration} -> networkConfiguration) (\s@ContainerProperties' {} a -> s {networkConfiguration = a} :: ContainerProperties)

instance Core.FromJSON ContainerProperties where
  parseJSON =
    Core.withObject
      "ContainerProperties"
      ( \x ->
          ContainerProperties'
            Prelude.<$> (x Core..:? "image")
            Prelude.<*> (x Core..:? "command" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "secrets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "environment" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ulimits" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "executionRoleArn")
            Prelude.<*> (x Core..:? "privileged")
            Prelude.<*> (x Core..:? "jobRoleArn")
            Prelude.<*> ( x Core..:? "resourceRequirements"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "instanceType")
            Prelude.<*> (x Core..:? "memory")
            Prelude.<*> (x Core..:? "user")
            Prelude.<*> (x Core..:? "logConfiguration")
            Prelude.<*> (x Core..:? "linuxParameters")
            Prelude.<*> (x Core..:? "mountPoints" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "fargatePlatformConfiguration")
            Prelude.<*> (x Core..:? "vcpus")
            Prelude.<*> (x Core..:? "readonlyRootFilesystem")
            Prelude.<*> (x Core..:? "volumes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "networkConfiguration")
      )

instance Prelude.Hashable ContainerProperties where
  hashWithSalt _salt ContainerProperties' {..} =
    _salt `Prelude.hashWithSalt` image
      `Prelude.hashWithSalt` command
      `Prelude.hashWithSalt` secrets
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` ulimits
      `Prelude.hashWithSalt` executionRoleArn
      `Prelude.hashWithSalt` privileged
      `Prelude.hashWithSalt` jobRoleArn
      `Prelude.hashWithSalt` resourceRequirements
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` memory
      `Prelude.hashWithSalt` user
      `Prelude.hashWithSalt` logConfiguration
      `Prelude.hashWithSalt` linuxParameters
      `Prelude.hashWithSalt` mountPoints
      `Prelude.hashWithSalt` fargatePlatformConfiguration
      `Prelude.hashWithSalt` vcpus
      `Prelude.hashWithSalt` readonlyRootFilesystem
      `Prelude.hashWithSalt` volumes
      `Prelude.hashWithSalt` networkConfiguration

instance Prelude.NFData ContainerProperties where
  rnf ContainerProperties' {..} =
    Prelude.rnf image
      `Prelude.seq` Prelude.rnf command
      `Prelude.seq` Prelude.rnf secrets
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf ulimits
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf privileged
      `Prelude.seq` Prelude.rnf jobRoleArn
      `Prelude.seq` Prelude.rnf resourceRequirements
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf memory
      `Prelude.seq` Prelude.rnf user
      `Prelude.seq` Prelude.rnf logConfiguration
      `Prelude.seq` Prelude.rnf linuxParameters
      `Prelude.seq` Prelude.rnf mountPoints
      `Prelude.seq` Prelude.rnf
        fargatePlatformConfiguration
      `Prelude.seq` Prelude.rnf vcpus
      `Prelude.seq` Prelude.rnf readonlyRootFilesystem
      `Prelude.seq` Prelude.rnf volumes
      `Prelude.seq` Prelude.rnf
        networkConfiguration

instance Core.ToJSON ContainerProperties where
  toJSON ContainerProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("image" Core..=) Prelude.<$> image,
            ("command" Core..=) Prelude.<$> command,
            ("secrets" Core..=) Prelude.<$> secrets,
            ("environment" Core..=) Prelude.<$> environment,
            ("ulimits" Core..=) Prelude.<$> ulimits,
            ("executionRoleArn" Core..=)
              Prelude.<$> executionRoleArn,
            ("privileged" Core..=) Prelude.<$> privileged,
            ("jobRoleArn" Core..=) Prelude.<$> jobRoleArn,
            ("resourceRequirements" Core..=)
              Prelude.<$> resourceRequirements,
            ("instanceType" Core..=) Prelude.<$> instanceType,
            ("memory" Core..=) Prelude.<$> memory,
            ("user" Core..=) Prelude.<$> user,
            ("logConfiguration" Core..=)
              Prelude.<$> logConfiguration,
            ("linuxParameters" Core..=)
              Prelude.<$> linuxParameters,
            ("mountPoints" Core..=) Prelude.<$> mountPoints,
            ("fargatePlatformConfiguration" Core..=)
              Prelude.<$> fargatePlatformConfiguration,
            ("vcpus" Core..=) Prelude.<$> vcpus,
            ("readonlyRootFilesystem" Core..=)
              Prelude.<$> readonlyRootFilesystem,
            ("volumes" Core..=) Prelude.<$> volumes,
            ("networkConfiguration" Core..=)
              Prelude.<$> networkConfiguration
          ]
      )
