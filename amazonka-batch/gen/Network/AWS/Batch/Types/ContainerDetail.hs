{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Batch.Types.ContainerDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ContainerDetail where

import Network.AWS.Batch.Types.FargatePlatformConfiguration
import Network.AWS.Batch.Types.KeyValuePair
import Network.AWS.Batch.Types.LinuxParameters
import Network.AWS.Batch.Types.LogConfiguration
import Network.AWS.Batch.Types.MountPoint
import Network.AWS.Batch.Types.NetworkConfiguration
import Network.AWS.Batch.Types.NetworkInterface
import Network.AWS.Batch.Types.ResourceRequirement
import Network.AWS.Batch.Types.Secret
import Network.AWS.Batch.Types.Ulimit
import Network.AWS.Batch.Types.Volume
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing the details of a container that\'s part of a job.
--
-- /See:/ 'newContainerDetail' smart constructor.
data ContainerDetail = ContainerDetail'
  { -- | The name of the CloudWatch Logs log stream associated with the
    -- container. The log group for AWS Batch jobs is @\/aws\/batch\/job@. Each
    -- container attempt receives a log stream name when they reach the
    -- @RUNNING@ status.
    logStreamName :: Prelude.Maybe Prelude.Text,
    -- | Linux-specific modifications that are applied to the container, such as
    -- details for device mappings.
    linuxParameters :: Prelude.Maybe LinuxParameters,
    -- | For jobs run on EC2 resources that didn\'t specify memory requirements
    -- using @ResourceRequirement@, the number of MiB of memory reserved for
    -- the job. For other jobs, including all run on Fargate resources, see
    -- @resourceRequirements@.
    memory :: Prelude.Maybe Prelude.Int,
    -- | The user name to use inside the container. This parameter maps to @User@
    -- in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--user@ option to
    -- <https://docs.docker.com/engine/reference/run/ docker run>.
    user :: Prelude.Maybe Prelude.Text,
    -- | The instance type of the underlying host infrastructure of a multi-node
    -- parallel job.
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The network configuration for jobs running on Fargate resources. Jobs
    -- running on EC2 resources must not specify this parameter.
    networkConfiguration :: Prelude.Maybe NetworkConfiguration,
    -- | The Amazon Resource Name (ARN) of the execution role that AWS Batch can
    -- assume. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/execution-IAM-role.html AWS Batch execution IAM role>
    -- in the /AWS Batch User Guide/.
    executionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | When this parameter is true, the container is given elevated permissions
    -- on the host container instance (similar to the @root@ user). The default
    -- value is false.
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources
    -- and shouldn\'t be provided, or specified as false.
    privileged :: Prelude.Maybe Prelude.Bool,
    -- | The number of vCPUs reserved for the container. Jobs running on EC2
    -- resources can specify the vCPU requirement for the job using
    -- @resourceRequirements@ but the vCPU requirements can\'t be specified
    -- both here and in the @resourceRequirement@ object. This parameter maps
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
    -- This parameter isn\'t applicable to jobs running on Fargate resources.
    -- Jobs running on Fargate resources must specify the vCPU requirement for
    -- the job using @resourceRequirements@.
    vcpus :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the container instance that the
    -- container is running on.
    containerInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | A list of volumes associated with the job.
    volumes :: Prelude.Maybe [Volume],
    -- | The environment variables to pass to a container.
    --
    -- Environment variables must not start with @AWS_BATCH@; this naming
    -- convention is reserved for variables that are set by the AWS Batch
    -- service.
    environment :: Prelude.Maybe [KeyValuePair],
    -- | The platform configuration for jobs running on Fargate resources. Jobs
    -- running on EC2 resources must not specify this parameter.
    fargatePlatformConfiguration :: Prelude.Maybe FargatePlatformConfiguration,
    -- | The exit code to return upon completion.
    exitCode :: Prelude.Maybe Prelude.Int,
    -- | The secrets to pass to the container. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/specifying-sensitive-data.html Specifying sensitive data>
    -- in the /AWS Batch User Guide/.
    secrets :: Prelude.Maybe [Secret],
    -- | The mount points for data volumes in your container.
    mountPoints :: Prelude.Maybe [MountPoint],
    -- | The image used to start the container.
    image :: Prelude.Maybe Prelude.Text,
    -- | The command that\'s passed to the container.
    command :: Prelude.Maybe [Prelude.Text],
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
    -- the log system must be configured properly on the container instance.
    -- Or, alternatively, it must be configured on a different log server for
    -- remote logging options. For more information on the options for
    -- different supported log drivers, see
    -- <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers>
    -- in the Docker documentation.
    --
    -- AWS Batch currently supports a subset of the logging drivers available
    -- to the Docker daemon (shown in the LogConfiguration data type).
    -- Additional log drivers might be available in future releases of the
    -- Amazon ECS container agent.
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
    -- | A short (255 max characters) human-readable string to provide additional
    -- details about a running or stopped container.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The type and amount of resources to assign to a container. The supported
    -- resources include @GPU@, @MEMORY@, and @VCPU@.
    resourceRequirements :: Prelude.Maybe [ResourceRequirement],
    -- | The Amazon Resource Name (ARN) associated with the job upon execution.
    jobRoleArn :: Prelude.Maybe Prelude.Text,
    -- | When this parameter is true, the container is given read-only access to
    -- its root file system. This parameter maps to @ReadonlyRootfs@ in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--read-only@ option to
    -- <https://docs.docker.com/engine/reference/commandline/run/ docker run> .
    readonlyRootFilesystem :: Prelude.Maybe Prelude.Bool,
    -- | A list of @ulimit@ values to set in the container. This parameter maps
    -- to @Ulimits@ in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--ulimit@ option to
    -- <https://docs.docker.com/engine/reference/run/ docker run>.
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources.
    ulimits :: Prelude.Maybe [Ulimit],
    -- | The Amazon Resource Name (ARN) of the Amazon ECS task that\'s associated
    -- with the container job. Each container attempt receives a task ARN when
    -- they reach the @STARTING@ status.
    taskArn :: Prelude.Maybe Prelude.Text,
    -- | The network interfaces associated with the job.
    networkInterfaces :: Prelude.Maybe [NetworkInterface]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ContainerDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logStreamName', 'containerDetail_logStreamName' - The name of the CloudWatch Logs log stream associated with the
-- container. The log group for AWS Batch jobs is @\/aws\/batch\/job@. Each
-- container attempt receives a log stream name when they reach the
-- @RUNNING@ status.
--
-- 'linuxParameters', 'containerDetail_linuxParameters' - Linux-specific modifications that are applied to the container, such as
-- details for device mappings.
--
-- 'memory', 'containerDetail_memory' - For jobs run on EC2 resources that didn\'t specify memory requirements
-- using @ResourceRequirement@, the number of MiB of memory reserved for
-- the job. For other jobs, including all run on Fargate resources, see
-- @resourceRequirements@.
--
-- 'user', 'containerDetail_user' - The user name to use inside the container. This parameter maps to @User@
-- in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--user@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- 'instanceType', 'containerDetail_instanceType' - The instance type of the underlying host infrastructure of a multi-node
-- parallel job.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources.
--
-- 'networkConfiguration', 'containerDetail_networkConfiguration' - The network configuration for jobs running on Fargate resources. Jobs
-- running on EC2 resources must not specify this parameter.
--
-- 'executionRoleArn', 'containerDetail_executionRoleArn' - The Amazon Resource Name (ARN) of the execution role that AWS Batch can
-- assume. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/execution-IAM-role.html AWS Batch execution IAM role>
-- in the /AWS Batch User Guide/.
--
-- 'privileged', 'containerDetail_privileged' - When this parameter is true, the container is given elevated permissions
-- on the host container instance (similar to the @root@ user). The default
-- value is false.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources
-- and shouldn\'t be provided, or specified as false.
--
-- 'vcpus', 'containerDetail_vcpus' - The number of vCPUs reserved for the container. Jobs running on EC2
-- resources can specify the vCPU requirement for the job using
-- @resourceRequirements@ but the vCPU requirements can\'t be specified
-- both here and in the @resourceRequirement@ object. This parameter maps
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
-- This parameter isn\'t applicable to jobs running on Fargate resources.
-- Jobs running on Fargate resources must specify the vCPU requirement for
-- the job using @resourceRequirements@.
--
-- 'containerInstanceArn', 'containerDetail_containerInstanceArn' - The Amazon Resource Name (ARN) of the container instance that the
-- container is running on.
--
-- 'volumes', 'containerDetail_volumes' - A list of volumes associated with the job.
--
-- 'environment', 'containerDetail_environment' - The environment variables to pass to a container.
--
-- Environment variables must not start with @AWS_BATCH@; this naming
-- convention is reserved for variables that are set by the AWS Batch
-- service.
--
-- 'fargatePlatformConfiguration', 'containerDetail_fargatePlatformConfiguration' - The platform configuration for jobs running on Fargate resources. Jobs
-- running on EC2 resources must not specify this parameter.
--
-- 'exitCode', 'containerDetail_exitCode' - The exit code to return upon completion.
--
-- 'secrets', 'containerDetail_secrets' - The secrets to pass to the container. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/specifying-sensitive-data.html Specifying sensitive data>
-- in the /AWS Batch User Guide/.
--
-- 'mountPoints', 'containerDetail_mountPoints' - The mount points for data volumes in your container.
--
-- 'image', 'containerDetail_image' - The image used to start the container.
--
-- 'command', 'containerDetail_command' - The command that\'s passed to the container.
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
-- However the container might use a different logging driver than the
-- Docker daemon by specifying a log driver with this parameter in the
-- container definition. To use a different logging driver for a container,
-- the log system must be configured properly on the container instance.
-- Or, alternatively, it must be configured on a different log server for
-- remote logging options. For more information on the options for
-- different supported log drivers, see
-- <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers>
-- in the Docker documentation.
--
-- AWS Batch currently supports a subset of the logging drivers available
-- to the Docker daemon (shown in the LogConfiguration data type).
-- Additional log drivers might be available in future releases of the
-- Amazon ECS container agent.
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
-- 'reason', 'containerDetail_reason' - A short (255 max characters) human-readable string to provide additional
-- details about a running or stopped container.
--
-- 'resourceRequirements', 'containerDetail_resourceRequirements' - The type and amount of resources to assign to a container. The supported
-- resources include @GPU@, @MEMORY@, and @VCPU@.
--
-- 'jobRoleArn', 'containerDetail_jobRoleArn' - The Amazon Resource Name (ARN) associated with the job upon execution.
--
-- 'readonlyRootFilesystem', 'containerDetail_readonlyRootFilesystem' - When this parameter is true, the container is given read-only access to
-- its root file system. This parameter maps to @ReadonlyRootfs@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--read-only@ option to
-- <https://docs.docker.com/engine/reference/commandline/run/ docker run> .
--
-- 'ulimits', 'containerDetail_ulimits' - A list of @ulimit@ values to set in the container. This parameter maps
-- to @Ulimits@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--ulimit@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources.
--
-- 'taskArn', 'containerDetail_taskArn' - The Amazon Resource Name (ARN) of the Amazon ECS task that\'s associated
-- with the container job. Each container attempt receives a task ARN when
-- they reach the @STARTING@ status.
--
-- 'networkInterfaces', 'containerDetail_networkInterfaces' - The network interfaces associated with the job.
newContainerDetail ::
  ContainerDetail
newContainerDetail =
  ContainerDetail'
    { logStreamName = Prelude.Nothing,
      linuxParameters = Prelude.Nothing,
      memory = Prelude.Nothing,
      user = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      networkConfiguration = Prelude.Nothing,
      executionRoleArn = Prelude.Nothing,
      privileged = Prelude.Nothing,
      vcpus = Prelude.Nothing,
      containerInstanceArn = Prelude.Nothing,
      volumes = Prelude.Nothing,
      environment = Prelude.Nothing,
      fargatePlatformConfiguration = Prelude.Nothing,
      exitCode = Prelude.Nothing,
      secrets = Prelude.Nothing,
      mountPoints = Prelude.Nothing,
      image = Prelude.Nothing,
      command = Prelude.Nothing,
      logConfiguration = Prelude.Nothing,
      reason = Prelude.Nothing,
      resourceRequirements = Prelude.Nothing,
      jobRoleArn = Prelude.Nothing,
      readonlyRootFilesystem = Prelude.Nothing,
      ulimits = Prelude.Nothing,
      taskArn = Prelude.Nothing,
      networkInterfaces = Prelude.Nothing
    }

-- | The name of the CloudWatch Logs log stream associated with the
-- container. The log group for AWS Batch jobs is @\/aws\/batch\/job@. Each
-- container attempt receives a log stream name when they reach the
-- @RUNNING@ status.
containerDetail_logStreamName :: Lens.Lens' ContainerDetail (Prelude.Maybe Prelude.Text)
containerDetail_logStreamName = Lens.lens (\ContainerDetail' {logStreamName} -> logStreamName) (\s@ContainerDetail' {} a -> s {logStreamName = a} :: ContainerDetail)

-- | Linux-specific modifications that are applied to the container, such as
-- details for device mappings.
containerDetail_linuxParameters :: Lens.Lens' ContainerDetail (Prelude.Maybe LinuxParameters)
containerDetail_linuxParameters = Lens.lens (\ContainerDetail' {linuxParameters} -> linuxParameters) (\s@ContainerDetail' {} a -> s {linuxParameters = a} :: ContainerDetail)

-- | For jobs run on EC2 resources that didn\'t specify memory requirements
-- using @ResourceRequirement@, the number of MiB of memory reserved for
-- the job. For other jobs, including all run on Fargate resources, see
-- @resourceRequirements@.
containerDetail_memory :: Lens.Lens' ContainerDetail (Prelude.Maybe Prelude.Int)
containerDetail_memory = Lens.lens (\ContainerDetail' {memory} -> memory) (\s@ContainerDetail' {} a -> s {memory = a} :: ContainerDetail)

-- | The user name to use inside the container. This parameter maps to @User@
-- in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--user@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
containerDetail_user :: Lens.Lens' ContainerDetail (Prelude.Maybe Prelude.Text)
containerDetail_user = Lens.lens (\ContainerDetail' {user} -> user) (\s@ContainerDetail' {} a -> s {user = a} :: ContainerDetail)

-- | The instance type of the underlying host infrastructure of a multi-node
-- parallel job.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources.
containerDetail_instanceType :: Lens.Lens' ContainerDetail (Prelude.Maybe Prelude.Text)
containerDetail_instanceType = Lens.lens (\ContainerDetail' {instanceType} -> instanceType) (\s@ContainerDetail' {} a -> s {instanceType = a} :: ContainerDetail)

-- | The network configuration for jobs running on Fargate resources. Jobs
-- running on EC2 resources must not specify this parameter.
containerDetail_networkConfiguration :: Lens.Lens' ContainerDetail (Prelude.Maybe NetworkConfiguration)
containerDetail_networkConfiguration = Lens.lens (\ContainerDetail' {networkConfiguration} -> networkConfiguration) (\s@ContainerDetail' {} a -> s {networkConfiguration = a} :: ContainerDetail)

-- | The Amazon Resource Name (ARN) of the execution role that AWS Batch can
-- assume. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/execution-IAM-role.html AWS Batch execution IAM role>
-- in the /AWS Batch User Guide/.
containerDetail_executionRoleArn :: Lens.Lens' ContainerDetail (Prelude.Maybe Prelude.Text)
containerDetail_executionRoleArn = Lens.lens (\ContainerDetail' {executionRoleArn} -> executionRoleArn) (\s@ContainerDetail' {} a -> s {executionRoleArn = a} :: ContainerDetail)

-- | When this parameter is true, the container is given elevated permissions
-- on the host container instance (similar to the @root@ user). The default
-- value is false.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources
-- and shouldn\'t be provided, or specified as false.
containerDetail_privileged :: Lens.Lens' ContainerDetail (Prelude.Maybe Prelude.Bool)
containerDetail_privileged = Lens.lens (\ContainerDetail' {privileged} -> privileged) (\s@ContainerDetail' {} a -> s {privileged = a} :: ContainerDetail)

-- | The number of vCPUs reserved for the container. Jobs running on EC2
-- resources can specify the vCPU requirement for the job using
-- @resourceRequirements@ but the vCPU requirements can\'t be specified
-- both here and in the @resourceRequirement@ object. This parameter maps
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
-- This parameter isn\'t applicable to jobs running on Fargate resources.
-- Jobs running on Fargate resources must specify the vCPU requirement for
-- the job using @resourceRequirements@.
containerDetail_vcpus :: Lens.Lens' ContainerDetail (Prelude.Maybe Prelude.Int)
containerDetail_vcpus = Lens.lens (\ContainerDetail' {vcpus} -> vcpus) (\s@ContainerDetail' {} a -> s {vcpus = a} :: ContainerDetail)

-- | The Amazon Resource Name (ARN) of the container instance that the
-- container is running on.
containerDetail_containerInstanceArn :: Lens.Lens' ContainerDetail (Prelude.Maybe Prelude.Text)
containerDetail_containerInstanceArn = Lens.lens (\ContainerDetail' {containerInstanceArn} -> containerInstanceArn) (\s@ContainerDetail' {} a -> s {containerInstanceArn = a} :: ContainerDetail)

-- | A list of volumes associated with the job.
containerDetail_volumes :: Lens.Lens' ContainerDetail (Prelude.Maybe [Volume])
containerDetail_volumes = Lens.lens (\ContainerDetail' {volumes} -> volumes) (\s@ContainerDetail' {} a -> s {volumes = a} :: ContainerDetail) Prelude.. Lens.mapping Prelude._Coerce

-- | The environment variables to pass to a container.
--
-- Environment variables must not start with @AWS_BATCH@; this naming
-- convention is reserved for variables that are set by the AWS Batch
-- service.
containerDetail_environment :: Lens.Lens' ContainerDetail (Prelude.Maybe [KeyValuePair])
containerDetail_environment = Lens.lens (\ContainerDetail' {environment} -> environment) (\s@ContainerDetail' {} a -> s {environment = a} :: ContainerDetail) Prelude.. Lens.mapping Prelude._Coerce

-- | The platform configuration for jobs running on Fargate resources. Jobs
-- running on EC2 resources must not specify this parameter.
containerDetail_fargatePlatformConfiguration :: Lens.Lens' ContainerDetail (Prelude.Maybe FargatePlatformConfiguration)
containerDetail_fargatePlatformConfiguration = Lens.lens (\ContainerDetail' {fargatePlatformConfiguration} -> fargatePlatformConfiguration) (\s@ContainerDetail' {} a -> s {fargatePlatformConfiguration = a} :: ContainerDetail)

-- | The exit code to return upon completion.
containerDetail_exitCode :: Lens.Lens' ContainerDetail (Prelude.Maybe Prelude.Int)
containerDetail_exitCode = Lens.lens (\ContainerDetail' {exitCode} -> exitCode) (\s@ContainerDetail' {} a -> s {exitCode = a} :: ContainerDetail)

-- | The secrets to pass to the container. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/specifying-sensitive-data.html Specifying sensitive data>
-- in the /AWS Batch User Guide/.
containerDetail_secrets :: Lens.Lens' ContainerDetail (Prelude.Maybe [Secret])
containerDetail_secrets = Lens.lens (\ContainerDetail' {secrets} -> secrets) (\s@ContainerDetail' {} a -> s {secrets = a} :: ContainerDetail) Prelude.. Lens.mapping Prelude._Coerce

-- | The mount points for data volumes in your container.
containerDetail_mountPoints :: Lens.Lens' ContainerDetail (Prelude.Maybe [MountPoint])
containerDetail_mountPoints = Lens.lens (\ContainerDetail' {mountPoints} -> mountPoints) (\s@ContainerDetail' {} a -> s {mountPoints = a} :: ContainerDetail) Prelude.. Lens.mapping Prelude._Coerce

-- | The image used to start the container.
containerDetail_image :: Lens.Lens' ContainerDetail (Prelude.Maybe Prelude.Text)
containerDetail_image = Lens.lens (\ContainerDetail' {image} -> image) (\s@ContainerDetail' {} a -> s {image = a} :: ContainerDetail)

-- | The command that\'s passed to the container.
containerDetail_command :: Lens.Lens' ContainerDetail (Prelude.Maybe [Prelude.Text])
containerDetail_command = Lens.lens (\ContainerDetail' {command} -> command) (\s@ContainerDetail' {} a -> s {command = a} :: ContainerDetail) Prelude.. Lens.mapping Prelude._Coerce

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
-- the log system must be configured properly on the container instance.
-- Or, alternatively, it must be configured on a different log server for
-- remote logging options. For more information on the options for
-- different supported log drivers, see
-- <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers>
-- in the Docker documentation.
--
-- AWS Batch currently supports a subset of the logging drivers available
-- to the Docker daemon (shown in the LogConfiguration data type).
-- Additional log drivers might be available in future releases of the
-- Amazon ECS container agent.
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
containerDetail_logConfiguration :: Lens.Lens' ContainerDetail (Prelude.Maybe LogConfiguration)
containerDetail_logConfiguration = Lens.lens (\ContainerDetail' {logConfiguration} -> logConfiguration) (\s@ContainerDetail' {} a -> s {logConfiguration = a} :: ContainerDetail)

-- | A short (255 max characters) human-readable string to provide additional
-- details about a running or stopped container.
containerDetail_reason :: Lens.Lens' ContainerDetail (Prelude.Maybe Prelude.Text)
containerDetail_reason = Lens.lens (\ContainerDetail' {reason} -> reason) (\s@ContainerDetail' {} a -> s {reason = a} :: ContainerDetail)

-- | The type and amount of resources to assign to a container. The supported
-- resources include @GPU@, @MEMORY@, and @VCPU@.
containerDetail_resourceRequirements :: Lens.Lens' ContainerDetail (Prelude.Maybe [ResourceRequirement])
containerDetail_resourceRequirements = Lens.lens (\ContainerDetail' {resourceRequirements} -> resourceRequirements) (\s@ContainerDetail' {} a -> s {resourceRequirements = a} :: ContainerDetail) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon Resource Name (ARN) associated with the job upon execution.
containerDetail_jobRoleArn :: Lens.Lens' ContainerDetail (Prelude.Maybe Prelude.Text)
containerDetail_jobRoleArn = Lens.lens (\ContainerDetail' {jobRoleArn} -> jobRoleArn) (\s@ContainerDetail' {} a -> s {jobRoleArn = a} :: ContainerDetail)

-- | When this parameter is true, the container is given read-only access to
-- its root file system. This parameter maps to @ReadonlyRootfs@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--read-only@ option to
-- <https://docs.docker.com/engine/reference/commandline/run/ docker run> .
containerDetail_readonlyRootFilesystem :: Lens.Lens' ContainerDetail (Prelude.Maybe Prelude.Bool)
containerDetail_readonlyRootFilesystem = Lens.lens (\ContainerDetail' {readonlyRootFilesystem} -> readonlyRootFilesystem) (\s@ContainerDetail' {} a -> s {readonlyRootFilesystem = a} :: ContainerDetail)

-- | A list of @ulimit@ values to set in the container. This parameter maps
-- to @Ulimits@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--ulimit@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources.
containerDetail_ulimits :: Lens.Lens' ContainerDetail (Prelude.Maybe [Ulimit])
containerDetail_ulimits = Lens.lens (\ContainerDetail' {ulimits} -> ulimits) (\s@ContainerDetail' {} a -> s {ulimits = a} :: ContainerDetail) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon Resource Name (ARN) of the Amazon ECS task that\'s associated
-- with the container job. Each container attempt receives a task ARN when
-- they reach the @STARTING@ status.
containerDetail_taskArn :: Lens.Lens' ContainerDetail (Prelude.Maybe Prelude.Text)
containerDetail_taskArn = Lens.lens (\ContainerDetail' {taskArn} -> taskArn) (\s@ContainerDetail' {} a -> s {taskArn = a} :: ContainerDetail)

-- | The network interfaces associated with the job.
containerDetail_networkInterfaces :: Lens.Lens' ContainerDetail (Prelude.Maybe [NetworkInterface])
containerDetail_networkInterfaces = Lens.lens (\ContainerDetail' {networkInterfaces} -> networkInterfaces) (\s@ContainerDetail' {} a -> s {networkInterfaces = a} :: ContainerDetail) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON ContainerDetail where
  parseJSON =
    Prelude.withObject
      "ContainerDetail"
      ( \x ->
          ContainerDetail'
            Prelude.<$> (x Prelude..:? "logStreamName")
            Prelude.<*> (x Prelude..:? "linuxParameters")
            Prelude.<*> (x Prelude..:? "memory")
            Prelude.<*> (x Prelude..:? "user")
            Prelude.<*> (x Prelude..:? "instanceType")
            Prelude.<*> (x Prelude..:? "networkConfiguration")
            Prelude.<*> (x Prelude..:? "executionRoleArn")
            Prelude.<*> (x Prelude..:? "privileged")
            Prelude.<*> (x Prelude..:? "vcpus")
            Prelude.<*> (x Prelude..:? "containerInstanceArn")
            Prelude.<*> (x Prelude..:? "volumes" Prelude..!= Prelude.mempty)
            Prelude.<*> ( x Prelude..:? "environment"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "fargatePlatformConfiguration")
            Prelude.<*> (x Prelude..:? "exitCode")
            Prelude.<*> (x Prelude..:? "secrets" Prelude..!= Prelude.mempty)
            Prelude.<*> ( x Prelude..:? "mountPoints"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "image")
            Prelude.<*> (x Prelude..:? "command" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "logConfiguration")
            Prelude.<*> (x Prelude..:? "reason")
            Prelude.<*> ( x Prelude..:? "resourceRequirements"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "jobRoleArn")
            Prelude.<*> (x Prelude..:? "readonlyRootFilesystem")
            Prelude.<*> (x Prelude..:? "ulimits" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "taskArn")
            Prelude.<*> ( x Prelude..:? "networkInterfaces"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ContainerDetail

instance Prelude.NFData ContainerDetail
