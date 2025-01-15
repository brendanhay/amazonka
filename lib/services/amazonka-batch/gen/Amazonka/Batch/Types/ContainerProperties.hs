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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Container properties are used for Amazon ECS based job definitions.
-- These properties to describe the container that\'s launched as part of a
-- job.
--
-- /See:/ 'newContainerProperties' smart constructor.
data ContainerProperties = ContainerProperties'
  { -- | The command that\'s passed to the container. This parameter maps to
    -- @Cmd@ in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @COMMAND@ parameter to
    -- <https://docs.docker.com/engine/reference/run/ docker run>. For more
    -- information, see
    -- <https://docs.docker.com/engine/reference/builder/#cmd>.
    command :: Prelude.Maybe [Prelude.Text],
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
    -- Environment variables cannot start with \"@AWS_BATCH@\". This naming
    -- convention is reserved for variables that Batch sets.
    environment :: Prelude.Maybe [KeyValuePair],
    -- | The Amazon Resource Name (ARN) of the execution role that Batch can
    -- assume. For jobs that run on Fargate resources, you must provide an
    -- execution role. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/execution-IAM-role.html Batch execution IAM role>
    -- in the /Batch User Guide/.
    executionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The platform configuration for jobs that are running on Fargate
    -- resources. Jobs that are running on EC2 resources must not specify this
    -- parameter.
    fargatePlatformConfiguration :: Prelude.Maybe FargatePlatformConfiguration,
    -- | The image used to start a container. This string is passed directly to
    -- the Docker daemon. Images in the Docker Hub registry are available by
    -- default. Other repositories are specified with
    -- @ @/@repository-url@/@\/@/@image@/@:@/@tag@/@ @. It can be 255
    -- characters long. It can contain uppercase and lowercase letters,
    -- numbers, hyphens (-), underscores (_), colons (:), periods (.), forward
    -- slashes (\/), and number signs (#). This parameter maps to @Image@ in
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
    -- -   Images in Amazon ECR Public repositories use the full
    --     @registry\/repository[:tag]@ or @registry\/repository[\@digest]@
    --     naming conventions. For example,
    --     @public.ecr.aws\/@/@registry_alias@/@\/@/@my-web-app@/@:@/@latest@/@ @.
    --
    -- -   Images in Amazon ECR repositories use the full registry and
    --     repository URI (for example,
    --     @123456789012.dkr.ecr.\<region-name>.amazonaws.com\/\<repository-name>@).
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
    -- | The instance type to use for a multi-node parallel job. All node groups
    -- in a multi-node parallel job must use the same instance type.
    --
    -- This parameter isn\'t applicable to single-node container jobs or jobs
    -- that run on Fargate resources, and shouldn\'t be provided.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role that the container can
    -- assume for Amazon Web Services permissions. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM roles for tasks>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    jobRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Linux-specific modifications that are applied to the container, such as
    -- details for device mappings.
    linuxParameters :: Prelude.Maybe LinuxParameters,
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
    -- your container instance, log in to your container instance and run the
    -- following command: @sudo docker version | grep \"Server API version\"@
    --
    -- The Amazon ECS container agent running on a container instance must
    -- register the logging drivers available on that instance with the
    -- @ECS_AVAILABLE_LOGGING_DRIVERS@ environment variable before containers
    -- placed on that instance can use these log configuration options. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-config.html Amazon ECS container agent configuration>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    logConfiguration :: Prelude.Maybe LogConfiguration,
    -- | This parameter is deprecated, use @resourceRequirements@ to specify the
    -- memory requirements for the job definition. It\'s not supported for jobs
    -- running on Fargate resources. For jobs that run on EC2 resources, it
    -- specifies the memory hard limit (in MiB) for a container. If your
    -- container attempts to exceed the specified number, it\'s terminated. You
    -- must specify at least 4 MiB of memory for a job using this parameter.
    -- The memory hard limit can be specified in several places. It must be
    -- specified for each node at least once.
    memory :: Prelude.Maybe Prelude.Int,
    -- | The mount points for data volumes in your container. This parameter maps
    -- to @Volumes@ in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--volume@ option to
    -- <https://docs.docker.com/engine/reference/run/ docker run>.
    mountPoints :: Prelude.Maybe [MountPoint],
    -- | The network configuration for jobs that are running on Fargate
    -- resources. Jobs that are running on EC2 resources must not specify this
    -- parameter.
    networkConfiguration :: Prelude.Maybe NetworkConfiguration,
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
    -- | When this parameter is true, the container is given read-only access to
    -- its root file system. This parameter maps to @ReadonlyRootfs@ in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--read-only@ option to @docker run@.
    readonlyRootFilesystem :: Prelude.Maybe Prelude.Bool,
    -- | The type and amount of resources to assign to a container. The supported
    -- resources include @GPU@, @MEMORY@, and @VCPU@.
    resourceRequirements :: Prelude.Maybe [ResourceRequirement],
    -- | The secrets for the container. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/specifying-sensitive-data.html Specifying sensitive data>
    -- in the /Batch User Guide/.
    secrets :: Prelude.Maybe [Secret],
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
    -- | The user name to use inside the container. This parameter maps to @User@
    -- in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--user@ option to
    -- <https://docs.docker.com/engine/reference/run/ docker run>.
    user :: Prelude.Maybe Prelude.Text,
    -- | This parameter is deprecated, use @resourceRequirements@ to specify the
    -- vCPU requirements for the job definition. It\'s not supported for jobs
    -- running on Fargate resources. For jobs running on EC2 resources, it
    -- specifies the number of vCPUs reserved for the job.
    --
    -- Each vCPU is equivalent to 1,024 CPU shares. This parameter maps to
    -- @CpuShares@ in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--cpu-shares@ option to
    -- <https://docs.docker.com/engine/reference/run/ docker run>. The number
    -- of vCPUs must be specified but can be specified in several places. You
    -- must specify it at least once for each node.
    vcpus :: Prelude.Maybe Prelude.Int,
    -- | A list of data volumes used in a job.
    volumes :: Prelude.Maybe [Volume]
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
-- Environment variables cannot start with \"@AWS_BATCH@\". This naming
-- convention is reserved for variables that Batch sets.
--
-- 'executionRoleArn', 'containerProperties_executionRoleArn' - The Amazon Resource Name (ARN) of the execution role that Batch can
-- assume. For jobs that run on Fargate resources, you must provide an
-- execution role. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/execution-IAM-role.html Batch execution IAM role>
-- in the /Batch User Guide/.
--
-- 'fargatePlatformConfiguration', 'containerProperties_fargatePlatformConfiguration' - The platform configuration for jobs that are running on Fargate
-- resources. Jobs that are running on EC2 resources must not specify this
-- parameter.
--
-- 'image', 'containerProperties_image' - The image used to start a container. This string is passed directly to
-- the Docker daemon. Images in the Docker Hub registry are available by
-- default. Other repositories are specified with
-- @ @/@repository-url@/@\/@/@image@/@:@/@tag@/@ @. It can be 255
-- characters long. It can contain uppercase and lowercase letters,
-- numbers, hyphens (-), underscores (_), colons (:), periods (.), forward
-- slashes (\/), and number signs (#). This parameter maps to @Image@ in
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
-- -   Images in Amazon ECR Public repositories use the full
--     @registry\/repository[:tag]@ or @registry\/repository[\@digest]@
--     naming conventions. For example,
--     @public.ecr.aws\/@/@registry_alias@/@\/@/@my-web-app@/@:@/@latest@/@ @.
--
-- -   Images in Amazon ECR repositories use the full registry and
--     repository URI (for example,
--     @123456789012.dkr.ecr.\<region-name>.amazonaws.com\/\<repository-name>@).
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
-- 'instanceType', 'containerProperties_instanceType' - The instance type to use for a multi-node parallel job. All node groups
-- in a multi-node parallel job must use the same instance type.
--
-- This parameter isn\'t applicable to single-node container jobs or jobs
-- that run on Fargate resources, and shouldn\'t be provided.
--
-- 'jobRoleArn', 'containerProperties_jobRoleArn' - The Amazon Resource Name (ARN) of the IAM role that the container can
-- assume for Amazon Web Services permissions. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM roles for tasks>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'linuxParameters', 'containerProperties_linuxParameters' - Linux-specific modifications that are applied to the container, such as
-- details for device mappings.
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
-- your container instance, log in to your container instance and run the
-- following command: @sudo docker version | grep \"Server API version\"@
--
-- The Amazon ECS container agent running on a container instance must
-- register the logging drivers available on that instance with the
-- @ECS_AVAILABLE_LOGGING_DRIVERS@ environment variable before containers
-- placed on that instance can use these log configuration options. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-config.html Amazon ECS container agent configuration>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'memory', 'containerProperties_memory' - This parameter is deprecated, use @resourceRequirements@ to specify the
-- memory requirements for the job definition. It\'s not supported for jobs
-- running on Fargate resources. For jobs that run on EC2 resources, it
-- specifies the memory hard limit (in MiB) for a container. If your
-- container attempts to exceed the specified number, it\'s terminated. You
-- must specify at least 4 MiB of memory for a job using this parameter.
-- The memory hard limit can be specified in several places. It must be
-- specified for each node at least once.
--
-- 'mountPoints', 'containerProperties_mountPoints' - The mount points for data volumes in your container. This parameter maps
-- to @Volumes@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--volume@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- 'networkConfiguration', 'containerProperties_networkConfiguration' - The network configuration for jobs that are running on Fargate
-- resources. Jobs that are running on EC2 resources must not specify this
-- parameter.
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
-- 'readonlyRootFilesystem', 'containerProperties_readonlyRootFilesystem' - When this parameter is true, the container is given read-only access to
-- its root file system. This parameter maps to @ReadonlyRootfs@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--read-only@ option to @docker run@.
--
-- 'resourceRequirements', 'containerProperties_resourceRequirements' - The type and amount of resources to assign to a container. The supported
-- resources include @GPU@, @MEMORY@, and @VCPU@.
--
-- 'secrets', 'containerProperties_secrets' - The secrets for the container. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/specifying-sensitive-data.html Specifying sensitive data>
-- in the /Batch User Guide/.
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
-- 'user', 'containerProperties_user' - The user name to use inside the container. This parameter maps to @User@
-- in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--user@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- 'vcpus', 'containerProperties_vcpus' - This parameter is deprecated, use @resourceRequirements@ to specify the
-- vCPU requirements for the job definition. It\'s not supported for jobs
-- running on Fargate resources. For jobs running on EC2 resources, it
-- specifies the number of vCPUs reserved for the job.
--
-- Each vCPU is equivalent to 1,024 CPU shares. This parameter maps to
-- @CpuShares@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--cpu-shares@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>. The number
-- of vCPUs must be specified but can be specified in several places. You
-- must specify it at least once for each node.
--
-- 'volumes', 'containerProperties_volumes' - A list of data volumes used in a job.
newContainerProperties ::
  ContainerProperties
newContainerProperties =
  ContainerProperties'
    { command = Prelude.Nothing,
      environment = Prelude.Nothing,
      executionRoleArn = Prelude.Nothing,
      fargatePlatformConfiguration = Prelude.Nothing,
      image = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      jobRoleArn = Prelude.Nothing,
      linuxParameters = Prelude.Nothing,
      logConfiguration = Prelude.Nothing,
      memory = Prelude.Nothing,
      mountPoints = Prelude.Nothing,
      networkConfiguration = Prelude.Nothing,
      privileged = Prelude.Nothing,
      readonlyRootFilesystem = Prelude.Nothing,
      resourceRequirements = Prelude.Nothing,
      secrets = Prelude.Nothing,
      ulimits = Prelude.Nothing,
      user = Prelude.Nothing,
      vcpus = Prelude.Nothing,
      volumes = Prelude.Nothing
    }

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
-- Environment variables cannot start with \"@AWS_BATCH@\". This naming
-- convention is reserved for variables that Batch sets.
containerProperties_environment :: Lens.Lens' ContainerProperties (Prelude.Maybe [KeyValuePair])
containerProperties_environment = Lens.lens (\ContainerProperties' {environment} -> environment) (\s@ContainerProperties' {} a -> s {environment = a} :: ContainerProperties) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the execution role that Batch can
-- assume. For jobs that run on Fargate resources, you must provide an
-- execution role. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/execution-IAM-role.html Batch execution IAM role>
-- in the /Batch User Guide/.
containerProperties_executionRoleArn :: Lens.Lens' ContainerProperties (Prelude.Maybe Prelude.Text)
containerProperties_executionRoleArn = Lens.lens (\ContainerProperties' {executionRoleArn} -> executionRoleArn) (\s@ContainerProperties' {} a -> s {executionRoleArn = a} :: ContainerProperties)

-- | The platform configuration for jobs that are running on Fargate
-- resources. Jobs that are running on EC2 resources must not specify this
-- parameter.
containerProperties_fargatePlatformConfiguration :: Lens.Lens' ContainerProperties (Prelude.Maybe FargatePlatformConfiguration)
containerProperties_fargatePlatformConfiguration = Lens.lens (\ContainerProperties' {fargatePlatformConfiguration} -> fargatePlatformConfiguration) (\s@ContainerProperties' {} a -> s {fargatePlatformConfiguration = a} :: ContainerProperties)

-- | The image used to start a container. This string is passed directly to
-- the Docker daemon. Images in the Docker Hub registry are available by
-- default. Other repositories are specified with
-- @ @/@repository-url@/@\/@/@image@/@:@/@tag@/@ @. It can be 255
-- characters long. It can contain uppercase and lowercase letters,
-- numbers, hyphens (-), underscores (_), colons (:), periods (.), forward
-- slashes (\/), and number signs (#). This parameter maps to @Image@ in
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
-- -   Images in Amazon ECR Public repositories use the full
--     @registry\/repository[:tag]@ or @registry\/repository[\@digest]@
--     naming conventions. For example,
--     @public.ecr.aws\/@/@registry_alias@/@\/@/@my-web-app@/@:@/@latest@/@ @.
--
-- -   Images in Amazon ECR repositories use the full registry and
--     repository URI (for example,
--     @123456789012.dkr.ecr.\<region-name>.amazonaws.com\/\<repository-name>@).
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

-- | The instance type to use for a multi-node parallel job. All node groups
-- in a multi-node parallel job must use the same instance type.
--
-- This parameter isn\'t applicable to single-node container jobs or jobs
-- that run on Fargate resources, and shouldn\'t be provided.
containerProperties_instanceType :: Lens.Lens' ContainerProperties (Prelude.Maybe Prelude.Text)
containerProperties_instanceType = Lens.lens (\ContainerProperties' {instanceType} -> instanceType) (\s@ContainerProperties' {} a -> s {instanceType = a} :: ContainerProperties)

-- | The Amazon Resource Name (ARN) of the IAM role that the container can
-- assume for Amazon Web Services permissions. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM roles for tasks>
-- in the /Amazon Elastic Container Service Developer Guide/.
containerProperties_jobRoleArn :: Lens.Lens' ContainerProperties (Prelude.Maybe Prelude.Text)
containerProperties_jobRoleArn = Lens.lens (\ContainerProperties' {jobRoleArn} -> jobRoleArn) (\s@ContainerProperties' {} a -> s {jobRoleArn = a} :: ContainerProperties)

-- | Linux-specific modifications that are applied to the container, such as
-- details for device mappings.
containerProperties_linuxParameters :: Lens.Lens' ContainerProperties (Prelude.Maybe LinuxParameters)
containerProperties_linuxParameters = Lens.lens (\ContainerProperties' {linuxParameters} -> linuxParameters) (\s@ContainerProperties' {} a -> s {linuxParameters = a} :: ContainerProperties)

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
-- your container instance, log in to your container instance and run the
-- following command: @sudo docker version | grep \"Server API version\"@
--
-- The Amazon ECS container agent running on a container instance must
-- register the logging drivers available on that instance with the
-- @ECS_AVAILABLE_LOGGING_DRIVERS@ environment variable before containers
-- placed on that instance can use these log configuration options. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-config.html Amazon ECS container agent configuration>
-- in the /Amazon Elastic Container Service Developer Guide/.
containerProperties_logConfiguration :: Lens.Lens' ContainerProperties (Prelude.Maybe LogConfiguration)
containerProperties_logConfiguration = Lens.lens (\ContainerProperties' {logConfiguration} -> logConfiguration) (\s@ContainerProperties' {} a -> s {logConfiguration = a} :: ContainerProperties)

-- | This parameter is deprecated, use @resourceRequirements@ to specify the
-- memory requirements for the job definition. It\'s not supported for jobs
-- running on Fargate resources. For jobs that run on EC2 resources, it
-- specifies the memory hard limit (in MiB) for a container. If your
-- container attempts to exceed the specified number, it\'s terminated. You
-- must specify at least 4 MiB of memory for a job using this parameter.
-- The memory hard limit can be specified in several places. It must be
-- specified for each node at least once.
containerProperties_memory :: Lens.Lens' ContainerProperties (Prelude.Maybe Prelude.Int)
containerProperties_memory = Lens.lens (\ContainerProperties' {memory} -> memory) (\s@ContainerProperties' {} a -> s {memory = a} :: ContainerProperties)

-- | The mount points for data volumes in your container. This parameter maps
-- to @Volumes@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--volume@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
containerProperties_mountPoints :: Lens.Lens' ContainerProperties (Prelude.Maybe [MountPoint])
containerProperties_mountPoints = Lens.lens (\ContainerProperties' {mountPoints} -> mountPoints) (\s@ContainerProperties' {} a -> s {mountPoints = a} :: ContainerProperties) Prelude.. Lens.mapping Lens.coerced

-- | The network configuration for jobs that are running on Fargate
-- resources. Jobs that are running on EC2 resources must not specify this
-- parameter.
containerProperties_networkConfiguration :: Lens.Lens' ContainerProperties (Prelude.Maybe NetworkConfiguration)
containerProperties_networkConfiguration = Lens.lens (\ContainerProperties' {networkConfiguration} -> networkConfiguration) (\s@ContainerProperties' {} a -> s {networkConfiguration = a} :: ContainerProperties)

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

-- | When this parameter is true, the container is given read-only access to
-- its root file system. This parameter maps to @ReadonlyRootfs@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--read-only@ option to @docker run@.
containerProperties_readonlyRootFilesystem :: Lens.Lens' ContainerProperties (Prelude.Maybe Prelude.Bool)
containerProperties_readonlyRootFilesystem = Lens.lens (\ContainerProperties' {readonlyRootFilesystem} -> readonlyRootFilesystem) (\s@ContainerProperties' {} a -> s {readonlyRootFilesystem = a} :: ContainerProperties)

-- | The type and amount of resources to assign to a container. The supported
-- resources include @GPU@, @MEMORY@, and @VCPU@.
containerProperties_resourceRequirements :: Lens.Lens' ContainerProperties (Prelude.Maybe [ResourceRequirement])
containerProperties_resourceRequirements = Lens.lens (\ContainerProperties' {resourceRequirements} -> resourceRequirements) (\s@ContainerProperties' {} a -> s {resourceRequirements = a} :: ContainerProperties) Prelude.. Lens.mapping Lens.coerced

-- | The secrets for the container. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/specifying-sensitive-data.html Specifying sensitive data>
-- in the /Batch User Guide/.
containerProperties_secrets :: Lens.Lens' ContainerProperties (Prelude.Maybe [Secret])
containerProperties_secrets = Lens.lens (\ContainerProperties' {secrets} -> secrets) (\s@ContainerProperties' {} a -> s {secrets = a} :: ContainerProperties) Prelude.. Lens.mapping Lens.coerced

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

-- | The user name to use inside the container. This parameter maps to @User@
-- in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--user@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
containerProperties_user :: Lens.Lens' ContainerProperties (Prelude.Maybe Prelude.Text)
containerProperties_user = Lens.lens (\ContainerProperties' {user} -> user) (\s@ContainerProperties' {} a -> s {user = a} :: ContainerProperties)

-- | This parameter is deprecated, use @resourceRequirements@ to specify the
-- vCPU requirements for the job definition. It\'s not supported for jobs
-- running on Fargate resources. For jobs running on EC2 resources, it
-- specifies the number of vCPUs reserved for the job.
--
-- Each vCPU is equivalent to 1,024 CPU shares. This parameter maps to
-- @CpuShares@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--cpu-shares@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>. The number
-- of vCPUs must be specified but can be specified in several places. You
-- must specify it at least once for each node.
containerProperties_vcpus :: Lens.Lens' ContainerProperties (Prelude.Maybe Prelude.Int)
containerProperties_vcpus = Lens.lens (\ContainerProperties' {vcpus} -> vcpus) (\s@ContainerProperties' {} a -> s {vcpus = a} :: ContainerProperties)

-- | A list of data volumes used in a job.
containerProperties_volumes :: Lens.Lens' ContainerProperties (Prelude.Maybe [Volume])
containerProperties_volumes = Lens.lens (\ContainerProperties' {volumes} -> volumes) (\s@ContainerProperties' {} a -> s {volumes = a} :: ContainerProperties) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ContainerProperties where
  parseJSON =
    Data.withObject
      "ContainerProperties"
      ( \x ->
          ContainerProperties'
            Prelude.<$> (x Data..:? "command" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "environment" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "executionRoleArn")
            Prelude.<*> (x Data..:? "fargatePlatformConfiguration")
            Prelude.<*> (x Data..:? "image")
            Prelude.<*> (x Data..:? "instanceType")
            Prelude.<*> (x Data..:? "jobRoleArn")
            Prelude.<*> (x Data..:? "linuxParameters")
            Prelude.<*> (x Data..:? "logConfiguration")
            Prelude.<*> (x Data..:? "memory")
            Prelude.<*> (x Data..:? "mountPoints" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "networkConfiguration")
            Prelude.<*> (x Data..:? "privileged")
            Prelude.<*> (x Data..:? "readonlyRootFilesystem")
            Prelude.<*> ( x
                            Data..:? "resourceRequirements"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "secrets" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ulimits" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "user")
            Prelude.<*> (x Data..:? "vcpus")
            Prelude.<*> (x Data..:? "volumes" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ContainerProperties where
  hashWithSalt _salt ContainerProperties' {..} =
    _salt
      `Prelude.hashWithSalt` command
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` executionRoleArn
      `Prelude.hashWithSalt` fargatePlatformConfiguration
      `Prelude.hashWithSalt` image
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` jobRoleArn
      `Prelude.hashWithSalt` linuxParameters
      `Prelude.hashWithSalt` logConfiguration
      `Prelude.hashWithSalt` memory
      `Prelude.hashWithSalt` mountPoints
      `Prelude.hashWithSalt` networkConfiguration
      `Prelude.hashWithSalt` privileged
      `Prelude.hashWithSalt` readonlyRootFilesystem
      `Prelude.hashWithSalt` resourceRequirements
      `Prelude.hashWithSalt` secrets
      `Prelude.hashWithSalt` ulimits
      `Prelude.hashWithSalt` user
      `Prelude.hashWithSalt` vcpus
      `Prelude.hashWithSalt` volumes

instance Prelude.NFData ContainerProperties where
  rnf ContainerProperties' {..} =
    Prelude.rnf command `Prelude.seq`
      Prelude.rnf environment `Prelude.seq`
        Prelude.rnf executionRoleArn `Prelude.seq`
          Prelude.rnf fargatePlatformConfiguration `Prelude.seq`
            Prelude.rnf image `Prelude.seq`
              Prelude.rnf instanceType `Prelude.seq`
                Prelude.rnf jobRoleArn `Prelude.seq`
                  Prelude.rnf linuxParameters `Prelude.seq`
                    Prelude.rnf logConfiguration `Prelude.seq`
                      Prelude.rnf memory `Prelude.seq`
                        Prelude.rnf mountPoints `Prelude.seq`
                          Prelude.rnf networkConfiguration `Prelude.seq`
                            Prelude.rnf privileged `Prelude.seq`
                              Prelude.rnf readonlyRootFilesystem `Prelude.seq`
                                Prelude.rnf resourceRequirements `Prelude.seq`
                                  Prelude.rnf secrets `Prelude.seq`
                                    Prelude.rnf ulimits `Prelude.seq`
                                      Prelude.rnf user `Prelude.seq`
                                        Prelude.rnf vcpus `Prelude.seq`
                                          Prelude.rnf volumes

instance Data.ToJSON ContainerProperties where
  toJSON ContainerProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("command" Data..=) Prelude.<$> command,
            ("environment" Data..=) Prelude.<$> environment,
            ("executionRoleArn" Data..=)
              Prelude.<$> executionRoleArn,
            ("fargatePlatformConfiguration" Data..=)
              Prelude.<$> fargatePlatformConfiguration,
            ("image" Data..=) Prelude.<$> image,
            ("instanceType" Data..=) Prelude.<$> instanceType,
            ("jobRoleArn" Data..=) Prelude.<$> jobRoleArn,
            ("linuxParameters" Data..=)
              Prelude.<$> linuxParameters,
            ("logConfiguration" Data..=)
              Prelude.<$> logConfiguration,
            ("memory" Data..=) Prelude.<$> memory,
            ("mountPoints" Data..=) Prelude.<$> mountPoints,
            ("networkConfiguration" Data..=)
              Prelude.<$> networkConfiguration,
            ("privileged" Data..=) Prelude.<$> privileged,
            ("readonlyRootFilesystem" Data..=)
              Prelude.<$> readonlyRootFilesystem,
            ("resourceRequirements" Data..=)
              Prelude.<$> resourceRequirements,
            ("secrets" Data..=) Prelude.<$> secrets,
            ("ulimits" Data..=) Prelude.<$> ulimits,
            ("user" Data..=) Prelude.<$> user,
            ("vcpus" Data..=) Prelude.<$> vcpus,
            ("volumes" Data..=) Prelude.<$> volumes
          ]
      )
