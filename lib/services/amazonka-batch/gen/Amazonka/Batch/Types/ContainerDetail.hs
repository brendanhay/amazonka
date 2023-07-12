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
-- Module      : Amazonka.Batch.Types.ContainerDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.ContainerDetail where

import Amazonka.Batch.Types.FargatePlatformConfiguration
import Amazonka.Batch.Types.KeyValuePair
import Amazonka.Batch.Types.LinuxParameters
import Amazonka.Batch.Types.LogConfiguration
import Amazonka.Batch.Types.MountPoint
import Amazonka.Batch.Types.NetworkConfiguration
import Amazonka.Batch.Types.NetworkInterface
import Amazonka.Batch.Types.ResourceRequirement
import Amazonka.Batch.Types.Secret
import Amazonka.Batch.Types.Ulimit
import Amazonka.Batch.Types.Volume
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the details of a container that\'s part of a
-- job.
--
-- /See:/ 'newContainerDetail' smart constructor.
data ContainerDetail = ContainerDetail'
  { -- | The command that\'s passed to the container.
    command :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the container instance that the
    -- container is running on.
    containerInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The environment variables to pass to a container.
    --
    -- Environment variables cannot start with \"@AWS_BATCH@\". This naming
    -- convention is reserved for variables that Batch sets.
    environment :: Prelude.Maybe [KeyValuePair],
    -- | The Amazon Resource Name (ARN) of the execution role that Batch can
    -- assume. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/execution-IAM-role.html Batch execution IAM role>
    -- in the /Batch User Guide/.
    executionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The exit code to return upon completion.
    exitCode :: Prelude.Maybe Prelude.Int,
    -- | The platform configuration for jobs that are running on Fargate
    -- resources. Jobs that are running on EC2 resources must not specify this
    -- parameter.
    fargatePlatformConfiguration :: Prelude.Maybe FargatePlatformConfiguration,
    -- | The image used to start the container.
    image :: Prelude.Maybe Prelude.Text,
    -- | The instance type of the underlying host infrastructure of a multi-node
    -- parallel job.
    --
    -- This parameter isn\'t applicable to jobs that are running on Fargate
    -- resources.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that\'s associated with the job when run.
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
    -- However, the container might use a different logging driver than the
    -- Docker daemon by specifying a log driver with this parameter in the
    -- container definition. To use a different logging driver for a container,
    -- the log system must be configured properly on the container instance.
    -- Or, alternatively, it must be configured on a different log server for
    -- remote logging options. For more information on the options for
    -- different supported log drivers, see
    -- <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers>
    -- in the Docker documentation.
    --
    -- Batch currently supports a subset of the logging drivers available to
    -- the Docker daemon (shown in the LogConfiguration data type). Additional
    -- log drivers might be available in future releases of the Amazon ECS
    -- container agent.
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
    -- | The name of the Amazon CloudWatch Logs log stream that\'s associated
    -- with the container. The log group for Batch jobs is @\/aws\/batch\/job@.
    -- Each container attempt receives a log stream name when they reach the
    -- @RUNNING@ status.
    logStreamName :: Prelude.Maybe Prelude.Text,
    -- | For jobs running on EC2 resources that didn\'t specify memory
    -- requirements using @resourceRequirements@, the number of MiB of memory
    -- reserved for the job. For other jobs, including all run on Fargate
    -- resources, see @resourceRequirements@.
    memory :: Prelude.Maybe Prelude.Int,
    -- | The mount points for data volumes in your container.
    mountPoints :: Prelude.Maybe [MountPoint],
    -- | The network configuration for jobs that are running on Fargate
    -- resources. Jobs that are running on EC2 resources must not specify this
    -- parameter.
    networkConfiguration :: Prelude.Maybe NetworkConfiguration,
    -- | The network interfaces that are associated with the job.
    networkInterfaces :: Prelude.Maybe [NetworkInterface],
    -- | When this parameter is true, the container is given elevated permissions
    -- on the host container instance (similar to the @root@ user). The default
    -- value is @false@.
    --
    -- This parameter isn\'t applicable to jobs that are running on Fargate
    -- resources and shouldn\'t be provided, or specified as @false@.
    privileged :: Prelude.Maybe Prelude.Bool,
    -- | When this parameter is true, the container is given read-only access to
    -- its root file system. This parameter maps to @ReadonlyRootfs@ in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--read-only@ option to
    -- <https://docs.docker.com/engine/reference/commandline/run/ docker run> .
    readonlyRootFilesystem :: Prelude.Maybe Prelude.Bool,
    -- | A short (255 max characters) human-readable string to provide additional
    -- details for a running or stopped container.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The type and amount of resources to assign to a container. The supported
    -- resources include @GPU@, @MEMORY@, and @VCPU@.
    resourceRequirements :: Prelude.Maybe [ResourceRequirement],
    -- | The secrets to pass to the container. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/specifying-sensitive-data.html Specifying sensitive data>
    -- in the /Batch User Guide/.
    secrets :: Prelude.Maybe [Secret],
    -- | The Amazon Resource Name (ARN) of the Amazon ECS task that\'s associated
    -- with the container job. Each container attempt receives a task ARN when
    -- they reach the @STARTING@ status.
    taskArn :: Prelude.Maybe Prelude.Text,
    -- | A list of @ulimit@ values to set in the container. This parameter maps
    -- to @Ulimits@ in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--ulimit@ option to
    -- <https://docs.docker.com/engine/reference/run/ docker run>.
    --
    -- This parameter isn\'t applicable to jobs that are running on Fargate
    -- resources.
    ulimits :: Prelude.Maybe [Ulimit],
    -- | The user name to use inside the container. This parameter maps to @User@
    -- in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--user@ option to
    -- <https://docs.docker.com/engine/reference/run/ docker run>.
    user :: Prelude.Maybe Prelude.Text,
    -- | The number of vCPUs reserved for the container. For jobs that run on EC2
    -- resources, you can specify the vCPU requirement for the job using
    -- @resourceRequirements@, but you can\'t specify the vCPU requirements in
    -- both the @vcpus@ and @resourceRequirements@ object. This parameter maps
    -- to @CpuShares@ in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--cpu-shares@ option to
    -- <https://docs.docker.com/engine/reference/run/ docker run>. Each vCPU is
    -- equivalent to 1,024 CPU shares. You must specify at least one vCPU. This
    -- is required but can be specified in several places. It must be specified
    -- for each node at least once.
    --
    -- This parameter isn\'t applicable to jobs that run on Fargate resources.
    -- For jobs that run on Fargate resources, you must specify the vCPU
    -- requirement for the job using @resourceRequirements@.
    vcpus :: Prelude.Maybe Prelude.Int,
    -- | A list of volumes that are associated with the job.
    volumes :: Prelude.Maybe [Volume]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'command', 'containerDetail_command' - The command that\'s passed to the container.
--
-- 'containerInstanceArn', 'containerDetail_containerInstanceArn' - The Amazon Resource Name (ARN) of the container instance that the
-- container is running on.
--
-- 'environment', 'containerDetail_environment' - The environment variables to pass to a container.
--
-- Environment variables cannot start with \"@AWS_BATCH@\". This naming
-- convention is reserved for variables that Batch sets.
--
-- 'executionRoleArn', 'containerDetail_executionRoleArn' - The Amazon Resource Name (ARN) of the execution role that Batch can
-- assume. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/execution-IAM-role.html Batch execution IAM role>
-- in the /Batch User Guide/.
--
-- 'exitCode', 'containerDetail_exitCode' - The exit code to return upon completion.
--
-- 'fargatePlatformConfiguration', 'containerDetail_fargatePlatformConfiguration' - The platform configuration for jobs that are running on Fargate
-- resources. Jobs that are running on EC2 resources must not specify this
-- parameter.
--
-- 'image', 'containerDetail_image' - The image used to start the container.
--
-- 'instanceType', 'containerDetail_instanceType' - The instance type of the underlying host infrastructure of a multi-node
-- parallel job.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources.
--
-- 'jobRoleArn', 'containerDetail_jobRoleArn' - The Amazon Resource Name (ARN) that\'s associated with the job when run.
--
-- 'linuxParameters', 'containerDetail_linuxParameters' - Linux-specific modifications that are applied to the container, such as
-- details for device mappings.
--
-- 'logConfiguration', 'containerDetail_logConfiguration' - The log configuration specification for the container.
--
-- This parameter maps to @LogConfig@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--log-driver@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>. By default,
-- containers use the same logging driver that the Docker daemon uses.
-- However, the container might use a different logging driver than the
-- Docker daemon by specifying a log driver with this parameter in the
-- container definition. To use a different logging driver for a container,
-- the log system must be configured properly on the container instance.
-- Or, alternatively, it must be configured on a different log server for
-- remote logging options. For more information on the options for
-- different supported log drivers, see
-- <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers>
-- in the Docker documentation.
--
-- Batch currently supports a subset of the logging drivers available to
-- the Docker daemon (shown in the LogConfiguration data type). Additional
-- log drivers might be available in future releases of the Amazon ECS
-- container agent.
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
-- 'logStreamName', 'containerDetail_logStreamName' - The name of the Amazon CloudWatch Logs log stream that\'s associated
-- with the container. The log group for Batch jobs is @\/aws\/batch\/job@.
-- Each container attempt receives a log stream name when they reach the
-- @RUNNING@ status.
--
-- 'memory', 'containerDetail_memory' - For jobs running on EC2 resources that didn\'t specify memory
-- requirements using @resourceRequirements@, the number of MiB of memory
-- reserved for the job. For other jobs, including all run on Fargate
-- resources, see @resourceRequirements@.
--
-- 'mountPoints', 'containerDetail_mountPoints' - The mount points for data volumes in your container.
--
-- 'networkConfiguration', 'containerDetail_networkConfiguration' - The network configuration for jobs that are running on Fargate
-- resources. Jobs that are running on EC2 resources must not specify this
-- parameter.
--
-- 'networkInterfaces', 'containerDetail_networkInterfaces' - The network interfaces that are associated with the job.
--
-- 'privileged', 'containerDetail_privileged' - When this parameter is true, the container is given elevated permissions
-- on the host container instance (similar to the @root@ user). The default
-- value is @false@.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources and shouldn\'t be provided, or specified as @false@.
--
-- 'readonlyRootFilesystem', 'containerDetail_readonlyRootFilesystem' - When this parameter is true, the container is given read-only access to
-- its root file system. This parameter maps to @ReadonlyRootfs@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--read-only@ option to
-- <https://docs.docker.com/engine/reference/commandline/run/ docker run> .
--
-- 'reason', 'containerDetail_reason' - A short (255 max characters) human-readable string to provide additional
-- details for a running or stopped container.
--
-- 'resourceRequirements', 'containerDetail_resourceRequirements' - The type and amount of resources to assign to a container. The supported
-- resources include @GPU@, @MEMORY@, and @VCPU@.
--
-- 'secrets', 'containerDetail_secrets' - The secrets to pass to the container. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/specifying-sensitive-data.html Specifying sensitive data>
-- in the /Batch User Guide/.
--
-- 'taskArn', 'containerDetail_taskArn' - The Amazon Resource Name (ARN) of the Amazon ECS task that\'s associated
-- with the container job. Each container attempt receives a task ARN when
-- they reach the @STARTING@ status.
--
-- 'ulimits', 'containerDetail_ulimits' - A list of @ulimit@ values to set in the container. This parameter maps
-- to @Ulimits@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--ulimit@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources.
--
-- 'user', 'containerDetail_user' - The user name to use inside the container. This parameter maps to @User@
-- in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--user@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- 'vcpus', 'containerDetail_vcpus' - The number of vCPUs reserved for the container. For jobs that run on EC2
-- resources, you can specify the vCPU requirement for the job using
-- @resourceRequirements@, but you can\'t specify the vCPU requirements in
-- both the @vcpus@ and @resourceRequirements@ object. This parameter maps
-- to @CpuShares@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--cpu-shares@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>. Each vCPU is
-- equivalent to 1,024 CPU shares. You must specify at least one vCPU. This
-- is required but can be specified in several places. It must be specified
-- for each node at least once.
--
-- This parameter isn\'t applicable to jobs that run on Fargate resources.
-- For jobs that run on Fargate resources, you must specify the vCPU
-- requirement for the job using @resourceRequirements@.
--
-- 'volumes', 'containerDetail_volumes' - A list of volumes that are associated with the job.
newContainerDetail ::
  ContainerDetail
newContainerDetail =
  ContainerDetail'
    { command = Prelude.Nothing,
      containerInstanceArn = Prelude.Nothing,
      environment = Prelude.Nothing,
      executionRoleArn = Prelude.Nothing,
      exitCode = Prelude.Nothing,
      fargatePlatformConfiguration = Prelude.Nothing,
      image = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      jobRoleArn = Prelude.Nothing,
      linuxParameters = Prelude.Nothing,
      logConfiguration = Prelude.Nothing,
      logStreamName = Prelude.Nothing,
      memory = Prelude.Nothing,
      mountPoints = Prelude.Nothing,
      networkConfiguration = Prelude.Nothing,
      networkInterfaces = Prelude.Nothing,
      privileged = Prelude.Nothing,
      readonlyRootFilesystem = Prelude.Nothing,
      reason = Prelude.Nothing,
      resourceRequirements = Prelude.Nothing,
      secrets = Prelude.Nothing,
      taskArn = Prelude.Nothing,
      ulimits = Prelude.Nothing,
      user = Prelude.Nothing,
      vcpus = Prelude.Nothing,
      volumes = Prelude.Nothing
    }

-- | The command that\'s passed to the container.
containerDetail_command :: Lens.Lens' ContainerDetail (Prelude.Maybe [Prelude.Text])
containerDetail_command = Lens.lens (\ContainerDetail' {command} -> command) (\s@ContainerDetail' {} a -> s {command = a} :: ContainerDetail) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the container instance that the
-- container is running on.
containerDetail_containerInstanceArn :: Lens.Lens' ContainerDetail (Prelude.Maybe Prelude.Text)
containerDetail_containerInstanceArn = Lens.lens (\ContainerDetail' {containerInstanceArn} -> containerInstanceArn) (\s@ContainerDetail' {} a -> s {containerInstanceArn = a} :: ContainerDetail)

-- | The environment variables to pass to a container.
--
-- Environment variables cannot start with \"@AWS_BATCH@\". This naming
-- convention is reserved for variables that Batch sets.
containerDetail_environment :: Lens.Lens' ContainerDetail (Prelude.Maybe [KeyValuePair])
containerDetail_environment = Lens.lens (\ContainerDetail' {environment} -> environment) (\s@ContainerDetail' {} a -> s {environment = a} :: ContainerDetail) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the execution role that Batch can
-- assume. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/execution-IAM-role.html Batch execution IAM role>
-- in the /Batch User Guide/.
containerDetail_executionRoleArn :: Lens.Lens' ContainerDetail (Prelude.Maybe Prelude.Text)
containerDetail_executionRoleArn = Lens.lens (\ContainerDetail' {executionRoleArn} -> executionRoleArn) (\s@ContainerDetail' {} a -> s {executionRoleArn = a} :: ContainerDetail)

-- | The exit code to return upon completion.
containerDetail_exitCode :: Lens.Lens' ContainerDetail (Prelude.Maybe Prelude.Int)
containerDetail_exitCode = Lens.lens (\ContainerDetail' {exitCode} -> exitCode) (\s@ContainerDetail' {} a -> s {exitCode = a} :: ContainerDetail)

-- | The platform configuration for jobs that are running on Fargate
-- resources. Jobs that are running on EC2 resources must not specify this
-- parameter.
containerDetail_fargatePlatformConfiguration :: Lens.Lens' ContainerDetail (Prelude.Maybe FargatePlatformConfiguration)
containerDetail_fargatePlatformConfiguration = Lens.lens (\ContainerDetail' {fargatePlatformConfiguration} -> fargatePlatformConfiguration) (\s@ContainerDetail' {} a -> s {fargatePlatformConfiguration = a} :: ContainerDetail)

-- | The image used to start the container.
containerDetail_image :: Lens.Lens' ContainerDetail (Prelude.Maybe Prelude.Text)
containerDetail_image = Lens.lens (\ContainerDetail' {image} -> image) (\s@ContainerDetail' {} a -> s {image = a} :: ContainerDetail)

-- | The instance type of the underlying host infrastructure of a multi-node
-- parallel job.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources.
containerDetail_instanceType :: Lens.Lens' ContainerDetail (Prelude.Maybe Prelude.Text)
containerDetail_instanceType = Lens.lens (\ContainerDetail' {instanceType} -> instanceType) (\s@ContainerDetail' {} a -> s {instanceType = a} :: ContainerDetail)

-- | The Amazon Resource Name (ARN) that\'s associated with the job when run.
containerDetail_jobRoleArn :: Lens.Lens' ContainerDetail (Prelude.Maybe Prelude.Text)
containerDetail_jobRoleArn = Lens.lens (\ContainerDetail' {jobRoleArn} -> jobRoleArn) (\s@ContainerDetail' {} a -> s {jobRoleArn = a} :: ContainerDetail)

-- | Linux-specific modifications that are applied to the container, such as
-- details for device mappings.
containerDetail_linuxParameters :: Lens.Lens' ContainerDetail (Prelude.Maybe LinuxParameters)
containerDetail_linuxParameters = Lens.lens (\ContainerDetail' {linuxParameters} -> linuxParameters) (\s@ContainerDetail' {} a -> s {linuxParameters = a} :: ContainerDetail)

-- | The log configuration specification for the container.
--
-- This parameter maps to @LogConfig@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--log-driver@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>. By default,
-- containers use the same logging driver that the Docker daemon uses.
-- However, the container might use a different logging driver than the
-- Docker daemon by specifying a log driver with this parameter in the
-- container definition. To use a different logging driver for a container,
-- the log system must be configured properly on the container instance.
-- Or, alternatively, it must be configured on a different log server for
-- remote logging options. For more information on the options for
-- different supported log drivers, see
-- <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers>
-- in the Docker documentation.
--
-- Batch currently supports a subset of the logging drivers available to
-- the Docker daemon (shown in the LogConfiguration data type). Additional
-- log drivers might be available in future releases of the Amazon ECS
-- container agent.
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
containerDetail_logConfiguration :: Lens.Lens' ContainerDetail (Prelude.Maybe LogConfiguration)
containerDetail_logConfiguration = Lens.lens (\ContainerDetail' {logConfiguration} -> logConfiguration) (\s@ContainerDetail' {} a -> s {logConfiguration = a} :: ContainerDetail)

-- | The name of the Amazon CloudWatch Logs log stream that\'s associated
-- with the container. The log group for Batch jobs is @\/aws\/batch\/job@.
-- Each container attempt receives a log stream name when they reach the
-- @RUNNING@ status.
containerDetail_logStreamName :: Lens.Lens' ContainerDetail (Prelude.Maybe Prelude.Text)
containerDetail_logStreamName = Lens.lens (\ContainerDetail' {logStreamName} -> logStreamName) (\s@ContainerDetail' {} a -> s {logStreamName = a} :: ContainerDetail)

-- | For jobs running on EC2 resources that didn\'t specify memory
-- requirements using @resourceRequirements@, the number of MiB of memory
-- reserved for the job. For other jobs, including all run on Fargate
-- resources, see @resourceRequirements@.
containerDetail_memory :: Lens.Lens' ContainerDetail (Prelude.Maybe Prelude.Int)
containerDetail_memory = Lens.lens (\ContainerDetail' {memory} -> memory) (\s@ContainerDetail' {} a -> s {memory = a} :: ContainerDetail)

-- | The mount points for data volumes in your container.
containerDetail_mountPoints :: Lens.Lens' ContainerDetail (Prelude.Maybe [MountPoint])
containerDetail_mountPoints = Lens.lens (\ContainerDetail' {mountPoints} -> mountPoints) (\s@ContainerDetail' {} a -> s {mountPoints = a} :: ContainerDetail) Prelude.. Lens.mapping Lens.coerced

-- | The network configuration for jobs that are running on Fargate
-- resources. Jobs that are running on EC2 resources must not specify this
-- parameter.
containerDetail_networkConfiguration :: Lens.Lens' ContainerDetail (Prelude.Maybe NetworkConfiguration)
containerDetail_networkConfiguration = Lens.lens (\ContainerDetail' {networkConfiguration} -> networkConfiguration) (\s@ContainerDetail' {} a -> s {networkConfiguration = a} :: ContainerDetail)

-- | The network interfaces that are associated with the job.
containerDetail_networkInterfaces :: Lens.Lens' ContainerDetail (Prelude.Maybe [NetworkInterface])
containerDetail_networkInterfaces = Lens.lens (\ContainerDetail' {networkInterfaces} -> networkInterfaces) (\s@ContainerDetail' {} a -> s {networkInterfaces = a} :: ContainerDetail) Prelude.. Lens.mapping Lens.coerced

-- | When this parameter is true, the container is given elevated permissions
-- on the host container instance (similar to the @root@ user). The default
-- value is @false@.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources and shouldn\'t be provided, or specified as @false@.
containerDetail_privileged :: Lens.Lens' ContainerDetail (Prelude.Maybe Prelude.Bool)
containerDetail_privileged = Lens.lens (\ContainerDetail' {privileged} -> privileged) (\s@ContainerDetail' {} a -> s {privileged = a} :: ContainerDetail)

-- | When this parameter is true, the container is given read-only access to
-- its root file system. This parameter maps to @ReadonlyRootfs@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--read-only@ option to
-- <https://docs.docker.com/engine/reference/commandline/run/ docker run> .
containerDetail_readonlyRootFilesystem :: Lens.Lens' ContainerDetail (Prelude.Maybe Prelude.Bool)
containerDetail_readonlyRootFilesystem = Lens.lens (\ContainerDetail' {readonlyRootFilesystem} -> readonlyRootFilesystem) (\s@ContainerDetail' {} a -> s {readonlyRootFilesystem = a} :: ContainerDetail)

-- | A short (255 max characters) human-readable string to provide additional
-- details for a running or stopped container.
containerDetail_reason :: Lens.Lens' ContainerDetail (Prelude.Maybe Prelude.Text)
containerDetail_reason = Lens.lens (\ContainerDetail' {reason} -> reason) (\s@ContainerDetail' {} a -> s {reason = a} :: ContainerDetail)

-- | The type and amount of resources to assign to a container. The supported
-- resources include @GPU@, @MEMORY@, and @VCPU@.
containerDetail_resourceRequirements :: Lens.Lens' ContainerDetail (Prelude.Maybe [ResourceRequirement])
containerDetail_resourceRequirements = Lens.lens (\ContainerDetail' {resourceRequirements} -> resourceRequirements) (\s@ContainerDetail' {} a -> s {resourceRequirements = a} :: ContainerDetail) Prelude.. Lens.mapping Lens.coerced

-- | The secrets to pass to the container. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/specifying-sensitive-data.html Specifying sensitive data>
-- in the /Batch User Guide/.
containerDetail_secrets :: Lens.Lens' ContainerDetail (Prelude.Maybe [Secret])
containerDetail_secrets = Lens.lens (\ContainerDetail' {secrets} -> secrets) (\s@ContainerDetail' {} a -> s {secrets = a} :: ContainerDetail) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the Amazon ECS task that\'s associated
-- with the container job. Each container attempt receives a task ARN when
-- they reach the @STARTING@ status.
containerDetail_taskArn :: Lens.Lens' ContainerDetail (Prelude.Maybe Prelude.Text)
containerDetail_taskArn = Lens.lens (\ContainerDetail' {taskArn} -> taskArn) (\s@ContainerDetail' {} a -> s {taskArn = a} :: ContainerDetail)

-- | A list of @ulimit@ values to set in the container. This parameter maps
-- to @Ulimits@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--ulimit@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources.
containerDetail_ulimits :: Lens.Lens' ContainerDetail (Prelude.Maybe [Ulimit])
containerDetail_ulimits = Lens.lens (\ContainerDetail' {ulimits} -> ulimits) (\s@ContainerDetail' {} a -> s {ulimits = a} :: ContainerDetail) Prelude.. Lens.mapping Lens.coerced

-- | The user name to use inside the container. This parameter maps to @User@
-- in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--user@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
containerDetail_user :: Lens.Lens' ContainerDetail (Prelude.Maybe Prelude.Text)
containerDetail_user = Lens.lens (\ContainerDetail' {user} -> user) (\s@ContainerDetail' {} a -> s {user = a} :: ContainerDetail)

-- | The number of vCPUs reserved for the container. For jobs that run on EC2
-- resources, you can specify the vCPU requirement for the job using
-- @resourceRequirements@, but you can\'t specify the vCPU requirements in
-- both the @vcpus@ and @resourceRequirements@ object. This parameter maps
-- to @CpuShares@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--cpu-shares@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>. Each vCPU is
-- equivalent to 1,024 CPU shares. You must specify at least one vCPU. This
-- is required but can be specified in several places. It must be specified
-- for each node at least once.
--
-- This parameter isn\'t applicable to jobs that run on Fargate resources.
-- For jobs that run on Fargate resources, you must specify the vCPU
-- requirement for the job using @resourceRequirements@.
containerDetail_vcpus :: Lens.Lens' ContainerDetail (Prelude.Maybe Prelude.Int)
containerDetail_vcpus = Lens.lens (\ContainerDetail' {vcpus} -> vcpus) (\s@ContainerDetail' {} a -> s {vcpus = a} :: ContainerDetail)

-- | A list of volumes that are associated with the job.
containerDetail_volumes :: Lens.Lens' ContainerDetail (Prelude.Maybe [Volume])
containerDetail_volumes = Lens.lens (\ContainerDetail' {volumes} -> volumes) (\s@ContainerDetail' {} a -> s {volumes = a} :: ContainerDetail) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ContainerDetail where
  parseJSON =
    Data.withObject
      "ContainerDetail"
      ( \x ->
          ContainerDetail'
            Prelude.<$> (x Data..:? "command" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "containerInstanceArn")
            Prelude.<*> (x Data..:? "environment" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "executionRoleArn")
            Prelude.<*> (x Data..:? "exitCode")
            Prelude.<*> (x Data..:? "fargatePlatformConfiguration")
            Prelude.<*> (x Data..:? "image")
            Prelude.<*> (x Data..:? "instanceType")
            Prelude.<*> (x Data..:? "jobRoleArn")
            Prelude.<*> (x Data..:? "linuxParameters")
            Prelude.<*> (x Data..:? "logConfiguration")
            Prelude.<*> (x Data..:? "logStreamName")
            Prelude.<*> (x Data..:? "memory")
            Prelude.<*> (x Data..:? "mountPoints" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "networkConfiguration")
            Prelude.<*> ( x
                            Data..:? "networkInterfaces"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "privileged")
            Prelude.<*> (x Data..:? "readonlyRootFilesystem")
            Prelude.<*> (x Data..:? "reason")
            Prelude.<*> ( x
                            Data..:? "resourceRequirements"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "secrets" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "taskArn")
            Prelude.<*> (x Data..:? "ulimits" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "user")
            Prelude.<*> (x Data..:? "vcpus")
            Prelude.<*> (x Data..:? "volumes" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ContainerDetail where
  hashWithSalt _salt ContainerDetail' {..} =
    _salt
      `Prelude.hashWithSalt` command
      `Prelude.hashWithSalt` containerInstanceArn
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` executionRoleArn
      `Prelude.hashWithSalt` exitCode
      `Prelude.hashWithSalt` fargatePlatformConfiguration
      `Prelude.hashWithSalt` image
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` jobRoleArn
      `Prelude.hashWithSalt` linuxParameters
      `Prelude.hashWithSalt` logConfiguration
      `Prelude.hashWithSalt` logStreamName
      `Prelude.hashWithSalt` memory
      `Prelude.hashWithSalt` mountPoints
      `Prelude.hashWithSalt` networkConfiguration
      `Prelude.hashWithSalt` networkInterfaces
      `Prelude.hashWithSalt` privileged
      `Prelude.hashWithSalt` readonlyRootFilesystem
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` resourceRequirements
      `Prelude.hashWithSalt` secrets
      `Prelude.hashWithSalt` taskArn
      `Prelude.hashWithSalt` ulimits
      `Prelude.hashWithSalt` user
      `Prelude.hashWithSalt` vcpus
      `Prelude.hashWithSalt` volumes

instance Prelude.NFData ContainerDetail where
  rnf ContainerDetail' {..} =
    Prelude.rnf command
      `Prelude.seq` Prelude.rnf containerInstanceArn
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf exitCode
      `Prelude.seq` Prelude.rnf fargatePlatformConfiguration
      `Prelude.seq` Prelude.rnf image
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf jobRoleArn
      `Prelude.seq` Prelude.rnf linuxParameters
      `Prelude.seq` Prelude.rnf logConfiguration
      `Prelude.seq` Prelude.rnf logStreamName
      `Prelude.seq` Prelude.rnf memory
      `Prelude.seq` Prelude.rnf mountPoints
      `Prelude.seq` Prelude.rnf networkConfiguration
      `Prelude.seq` Prelude.rnf networkInterfaces
      `Prelude.seq` Prelude.rnf privileged
      `Prelude.seq` Prelude.rnf readonlyRootFilesystem
      `Prelude.seq` Prelude.rnf reason
      `Prelude.seq` Prelude.rnf
        resourceRequirements
      `Prelude.seq` Prelude.rnf secrets
      `Prelude.seq` Prelude.rnf taskArn
      `Prelude.seq` Prelude.rnf ulimits
      `Prelude.seq` Prelude.rnf user
      `Prelude.seq` Prelude.rnf vcpus
      `Prelude.seq` Prelude.rnf
        volumes
