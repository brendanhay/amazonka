{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ContainerProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ContainerProperties
  ( ContainerProperties (..),

    -- * Smart constructor
    mkContainerProperties,

    -- * Lenses
    cpImage,
    cpCommand,
    cpSecrets,
    cpEnvironment,
    cpUlimits,
    cpExecutionRoleARN,
    cpPrivileged,
    cpJobRoleARN,
    cpResourceRequirements,
    cpInstanceType,
    cpMemory,
    cpUser,
    cpLogConfiguration,
    cpLinuxParameters,
    cpMountPoints,
    cpVcpus,
    cpReadonlyRootFilesystem,
    cpVolumes,
  )
where

import Network.AWS.Batch.Types.KeyValuePair
import Network.AWS.Batch.Types.LinuxParameters
import Network.AWS.Batch.Types.LogConfiguration
import Network.AWS.Batch.Types.MountPoint
import Network.AWS.Batch.Types.ResourceRequirement
import Network.AWS.Batch.Types.Secret
import Network.AWS.Batch.Types.Ulimit
import Network.AWS.Batch.Types.Volume
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Container properties are used in job definitions to describe the container that is launched as part of a job.
--
-- /See:/ 'mkContainerProperties' smart constructor.
data ContainerProperties = ContainerProperties'
  { image ::
      Lude.Maybe Lude.Text,
    command :: Lude.Maybe [Lude.Text],
    secrets :: Lude.Maybe [Secret],
    environment :: Lude.Maybe [KeyValuePair],
    ulimits :: Lude.Maybe [Ulimit],
    executionRoleARN :: Lude.Maybe Lude.Text,
    privileged :: Lude.Maybe Lude.Bool,
    jobRoleARN :: Lude.Maybe Lude.Text,
    resourceRequirements ::
      Lude.Maybe [ResourceRequirement],
    instanceType :: Lude.Maybe Lude.Text,
    memory :: Lude.Maybe Lude.Int,
    user :: Lude.Maybe Lude.Text,
    logConfiguration :: Lude.Maybe LogConfiguration,
    linuxParameters :: Lude.Maybe LinuxParameters,
    mountPoints :: Lude.Maybe [MountPoint],
    vcpus :: Lude.Maybe Lude.Int,
    readonlyRootFilesystem :: Lude.Maybe Lude.Bool,
    volumes :: Lude.Maybe [Volume]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContainerProperties' with the minimum fields required to make a request.
--
-- * 'command' - The command that is passed to the container. This parameter maps to @Cmd@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @COMMAND@ parameter to <https://docs.docker.com/engine/reference/run/ docker run> . For more information, see <https://docs.docker.com/engine/reference/builder/#cmd https://docs.docker.com/engine/reference/builder/#cmd> .
-- * 'environment' - The environment variables to pass to a container. This parameter maps to @Env@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--env@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- /Important:/ We do not recommend using plaintext environment variables for sensitive information, such as credential data.
-- * 'executionRoleARN' - The Amazon Resource Name (ARN) of the execution role that AWS Batch can assume. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/execution-IAM-role.html AWS Batch execution IAM role> .
-- * 'image' - The image used to start a container. This string is passed directly to the Docker daemon. Images in the Docker Hub registry are available by default. Other repositories are specified with @/repository-url/ //image/ :/tag/ @ . Up to 255 letters (uppercase and lowercase), numbers, hyphens, underscores, colons, periods, forward slashes, and number signs are allowed. This parameter maps to @Image@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @IMAGE@ parameter of <https://docs.docker.com/engine/reference/run/ docker run> .
--
--
--     * Images in Amazon ECR repositories use the full registry and repository URI (for example, @012345678910.dkr.ecr.<region-name>.amazonaws.com/<repository-name>@ ).
--
--
--     * Images in official repositories on Docker Hub use a single name (for example, @ubuntu@ or @mongo@ ).
--
--
--     * Images in other repositories on Docker Hub are qualified with an organization name (for example, @amazon/amazon-ecs-agent@ ).
--
--
--     * Images in other online repositories are qualified further by a domain name (for example, @quay.io/assemblyline/ubuntu@ ).
--
--
-- * 'instanceType' - The instance type to use for a multi-node parallel job. Currently all node groups in a multi-node parallel job must use the same instance type. This parameter is not valid for single-node container jobs.
-- * 'jobRoleARN' - The Amazon Resource Name (ARN) of the IAM role that the container can assume for AWS permissions.
-- * 'linuxParameters' - Linux-specific modifications that are applied to the container, such as details for device mappings.
-- * 'logConfiguration' - The log configuration specification for the container.
--
-- This parameter maps to @LogConfig@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--log-driver@ option to <https://docs.docker.com/engine/reference/run/ docker run> . By default, containers use the same logging driver that the Docker daemon uses. However the container may use a different logging driver than the Docker daemon by specifying a log driver with this parameter in the container definition. To use a different logging driver for a container, the log system must be configured properly on the container instance (or on a different log server for remote logging options). For more information on the options for different supported log drivers, see <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers> in the Docker documentation.
-- This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@
-- * 'memory' - The hard limit (in MiB) of memory to present to the container. If your container attempts to exceed the memory specified here, the container is killed. This parameter maps to @Memory@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--memory@ option to <https://docs.docker.com/engine/reference/run/ docker run> . You must specify at least 4 MiB of memory for a job. This is required but can be specified in several places for multi-node parallel (MNP) jobs; it must be specified for each node at least once.
-- * 'mountPoints' - The mount points for data volumes in your container. This parameter maps to @Volumes@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--volume@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
-- * 'privileged' - When this parameter is true, the container is given elevated privileges on the host container instance (similar to the @root@ user). This parameter maps to @Privileged@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--privileged@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
-- * 'readonlyRootFilesystem' - When this parameter is true, the container is given read-only access to its root file system. This parameter maps to @ReadonlyRootfs@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--read-only@ option to @docker run@ .
-- * 'resourceRequirements' - The type and amount of a resource to assign to a container. Currently, the only supported resource is @GPU@ .
-- * 'secrets' - The secrets for the container. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html Specifying Sensitive Data> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'ulimits' - A list of @ulimits@ to set in the container. This parameter maps to @Ulimits@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--ulimit@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
-- * 'user' - The user name to use inside the container. This parameter maps to @User@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--user@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
-- * 'vcpus' - The number of vCPUs reserved for the container. This parameter maps to @CpuShares@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--cpu-shares@ option to <https://docs.docker.com/engine/reference/run/ docker run> . Each vCPU is equivalent to 1,024 CPU shares. You must specify at least one vCPU. This is required but can be specified in several places for multi-node parallel (MNP) jobs; it must be specified for each node at least once.
-- * 'volumes' - A list of data volumes used in a job.
mkContainerProperties ::
  ContainerProperties
mkContainerProperties =
  ContainerProperties'
    { image = Lude.Nothing,
      command = Lude.Nothing,
      secrets = Lude.Nothing,
      environment = Lude.Nothing,
      ulimits = Lude.Nothing,
      executionRoleARN = Lude.Nothing,
      privileged = Lude.Nothing,
      jobRoleARN = Lude.Nothing,
      resourceRequirements = Lude.Nothing,
      instanceType = Lude.Nothing,
      memory = Lude.Nothing,
      user = Lude.Nothing,
      logConfiguration = Lude.Nothing,
      linuxParameters = Lude.Nothing,
      mountPoints = Lude.Nothing,
      vcpus = Lude.Nothing,
      readonlyRootFilesystem = Lude.Nothing,
      volumes = Lude.Nothing
    }

-- | The image used to start a container. This string is passed directly to the Docker daemon. Images in the Docker Hub registry are available by default. Other repositories are specified with @/repository-url/ //image/ :/tag/ @ . Up to 255 letters (uppercase and lowercase), numbers, hyphens, underscores, colons, periods, forward slashes, and number signs are allowed. This parameter maps to @Image@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @IMAGE@ parameter of <https://docs.docker.com/engine/reference/run/ docker run> .
--
--
--     * Images in Amazon ECR repositories use the full registry and repository URI (for example, @012345678910.dkr.ecr.<region-name>.amazonaws.com/<repository-name>@ ).
--
--
--     * Images in official repositories on Docker Hub use a single name (for example, @ubuntu@ or @mongo@ ).
--
--
--     * Images in other repositories on Docker Hub are qualified with an organization name (for example, @amazon/amazon-ecs-agent@ ).
--
--
--     * Images in other online repositories are qualified further by a domain name (for example, @quay.io/assemblyline/ubuntu@ ).
--
--
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpImage :: Lens.Lens' ContainerProperties (Lude.Maybe Lude.Text)
cpImage = Lens.lens (image :: ContainerProperties -> Lude.Maybe Lude.Text) (\s a -> s {image = a} :: ContainerProperties)
{-# DEPRECATED cpImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | The command that is passed to the container. This parameter maps to @Cmd@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @COMMAND@ parameter to <https://docs.docker.com/engine/reference/run/ docker run> . For more information, see <https://docs.docker.com/engine/reference/builder/#cmd https://docs.docker.com/engine/reference/builder/#cmd> .
--
-- /Note:/ Consider using 'command' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpCommand :: Lens.Lens' ContainerProperties (Lude.Maybe [Lude.Text])
cpCommand = Lens.lens (command :: ContainerProperties -> Lude.Maybe [Lude.Text]) (\s a -> s {command = a} :: ContainerProperties)
{-# DEPRECATED cpCommand "Use generic-lens or generic-optics with 'command' instead." #-}

-- | The secrets for the container. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html Specifying Sensitive Data> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'secrets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSecrets :: Lens.Lens' ContainerProperties (Lude.Maybe [Secret])
cpSecrets = Lens.lens (secrets :: ContainerProperties -> Lude.Maybe [Secret]) (\s a -> s {secrets = a} :: ContainerProperties)
{-# DEPRECATED cpSecrets "Use generic-lens or generic-optics with 'secrets' instead." #-}

-- | The environment variables to pass to a container. This parameter maps to @Env@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--env@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- /Important:/ We do not recommend using plaintext environment variables for sensitive information, such as credential data.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpEnvironment :: Lens.Lens' ContainerProperties (Lude.Maybe [KeyValuePair])
cpEnvironment = Lens.lens (environment :: ContainerProperties -> Lude.Maybe [KeyValuePair]) (\s a -> s {environment = a} :: ContainerProperties)
{-# DEPRECATED cpEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | A list of @ulimits@ to set in the container. This parameter maps to @Ulimits@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--ulimit@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- /Note:/ Consider using 'ulimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpUlimits :: Lens.Lens' ContainerProperties (Lude.Maybe [Ulimit])
cpUlimits = Lens.lens (ulimits :: ContainerProperties -> Lude.Maybe [Ulimit]) (\s a -> s {ulimits = a} :: ContainerProperties)
{-# DEPRECATED cpUlimits "Use generic-lens or generic-optics with 'ulimits' instead." #-}

-- | The Amazon Resource Name (ARN) of the execution role that AWS Batch can assume. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/execution-IAM-role.html AWS Batch execution IAM role> .
--
-- /Note:/ Consider using 'executionRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpExecutionRoleARN :: Lens.Lens' ContainerProperties (Lude.Maybe Lude.Text)
cpExecutionRoleARN = Lens.lens (executionRoleARN :: ContainerProperties -> Lude.Maybe Lude.Text) (\s a -> s {executionRoleARN = a} :: ContainerProperties)
{-# DEPRECATED cpExecutionRoleARN "Use generic-lens or generic-optics with 'executionRoleARN' instead." #-}

-- | When this parameter is true, the container is given elevated privileges on the host container instance (similar to the @root@ user). This parameter maps to @Privileged@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--privileged@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- /Note:/ Consider using 'privileged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPrivileged :: Lens.Lens' ContainerProperties (Lude.Maybe Lude.Bool)
cpPrivileged = Lens.lens (privileged :: ContainerProperties -> Lude.Maybe Lude.Bool) (\s a -> s {privileged = a} :: ContainerProperties)
{-# DEPRECATED cpPrivileged "Use generic-lens or generic-optics with 'privileged' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role that the container can assume for AWS permissions.
--
-- /Note:/ Consider using 'jobRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpJobRoleARN :: Lens.Lens' ContainerProperties (Lude.Maybe Lude.Text)
cpJobRoleARN = Lens.lens (jobRoleARN :: ContainerProperties -> Lude.Maybe Lude.Text) (\s a -> s {jobRoleARN = a} :: ContainerProperties)
{-# DEPRECATED cpJobRoleARN "Use generic-lens or generic-optics with 'jobRoleARN' instead." #-}

-- | The type and amount of a resource to assign to a container. Currently, the only supported resource is @GPU@ .
--
-- /Note:/ Consider using 'resourceRequirements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpResourceRequirements :: Lens.Lens' ContainerProperties (Lude.Maybe [ResourceRequirement])
cpResourceRequirements = Lens.lens (resourceRequirements :: ContainerProperties -> Lude.Maybe [ResourceRequirement]) (\s a -> s {resourceRequirements = a} :: ContainerProperties)
{-# DEPRECATED cpResourceRequirements "Use generic-lens or generic-optics with 'resourceRequirements' instead." #-}

-- | The instance type to use for a multi-node parallel job. Currently all node groups in a multi-node parallel job must use the same instance type. This parameter is not valid for single-node container jobs.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpInstanceType :: Lens.Lens' ContainerProperties (Lude.Maybe Lude.Text)
cpInstanceType = Lens.lens (instanceType :: ContainerProperties -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: ContainerProperties)
{-# DEPRECATED cpInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The hard limit (in MiB) of memory to present to the container. If your container attempts to exceed the memory specified here, the container is killed. This parameter maps to @Memory@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--memory@ option to <https://docs.docker.com/engine/reference/run/ docker run> . You must specify at least 4 MiB of memory for a job. This is required but can be specified in several places for multi-node parallel (MNP) jobs; it must be specified for each node at least once.
--
-- /Note:/ Consider using 'memory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpMemory :: Lens.Lens' ContainerProperties (Lude.Maybe Lude.Int)
cpMemory = Lens.lens (memory :: ContainerProperties -> Lude.Maybe Lude.Int) (\s a -> s {memory = a} :: ContainerProperties)
{-# DEPRECATED cpMemory "Use generic-lens or generic-optics with 'memory' instead." #-}

-- | The user name to use inside the container. This parameter maps to @User@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--user@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpUser :: Lens.Lens' ContainerProperties (Lude.Maybe Lude.Text)
cpUser = Lens.lens (user :: ContainerProperties -> Lude.Maybe Lude.Text) (\s a -> s {user = a} :: ContainerProperties)
{-# DEPRECATED cpUser "Use generic-lens or generic-optics with 'user' instead." #-}

-- | The log configuration specification for the container.
--
-- This parameter maps to @LogConfig@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--log-driver@ option to <https://docs.docker.com/engine/reference/run/ docker run> . By default, containers use the same logging driver that the Docker daemon uses. However the container may use a different logging driver than the Docker daemon by specifying a log driver with this parameter in the container definition. To use a different logging driver for a container, the log system must be configured properly on the container instance (or on a different log server for remote logging options). For more information on the options for different supported log drivers, see <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers> in the Docker documentation.
-- This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@
--
-- /Note:/ Consider using 'logConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpLogConfiguration :: Lens.Lens' ContainerProperties (Lude.Maybe LogConfiguration)
cpLogConfiguration = Lens.lens (logConfiguration :: ContainerProperties -> Lude.Maybe LogConfiguration) (\s a -> s {logConfiguration = a} :: ContainerProperties)
{-# DEPRECATED cpLogConfiguration "Use generic-lens or generic-optics with 'logConfiguration' instead." #-}

-- | Linux-specific modifications that are applied to the container, such as details for device mappings.
--
-- /Note:/ Consider using 'linuxParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpLinuxParameters :: Lens.Lens' ContainerProperties (Lude.Maybe LinuxParameters)
cpLinuxParameters = Lens.lens (linuxParameters :: ContainerProperties -> Lude.Maybe LinuxParameters) (\s a -> s {linuxParameters = a} :: ContainerProperties)
{-# DEPRECATED cpLinuxParameters "Use generic-lens or generic-optics with 'linuxParameters' instead." #-}

-- | The mount points for data volumes in your container. This parameter maps to @Volumes@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--volume@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- /Note:/ Consider using 'mountPoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpMountPoints :: Lens.Lens' ContainerProperties (Lude.Maybe [MountPoint])
cpMountPoints = Lens.lens (mountPoints :: ContainerProperties -> Lude.Maybe [MountPoint]) (\s a -> s {mountPoints = a} :: ContainerProperties)
{-# DEPRECATED cpMountPoints "Use generic-lens or generic-optics with 'mountPoints' instead." #-}

-- | The number of vCPUs reserved for the container. This parameter maps to @CpuShares@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--cpu-shares@ option to <https://docs.docker.com/engine/reference/run/ docker run> . Each vCPU is equivalent to 1,024 CPU shares. You must specify at least one vCPU. This is required but can be specified in several places for multi-node parallel (MNP) jobs; it must be specified for each node at least once.
--
-- /Note:/ Consider using 'vcpus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpVcpus :: Lens.Lens' ContainerProperties (Lude.Maybe Lude.Int)
cpVcpus = Lens.lens (vcpus :: ContainerProperties -> Lude.Maybe Lude.Int) (\s a -> s {vcpus = a} :: ContainerProperties)
{-# DEPRECATED cpVcpus "Use generic-lens or generic-optics with 'vcpus' instead." #-}

-- | When this parameter is true, the container is given read-only access to its root file system. This parameter maps to @ReadonlyRootfs@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--read-only@ option to @docker run@ .
--
-- /Note:/ Consider using 'readonlyRootFilesystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpReadonlyRootFilesystem :: Lens.Lens' ContainerProperties (Lude.Maybe Lude.Bool)
cpReadonlyRootFilesystem = Lens.lens (readonlyRootFilesystem :: ContainerProperties -> Lude.Maybe Lude.Bool) (\s a -> s {readonlyRootFilesystem = a} :: ContainerProperties)
{-# DEPRECATED cpReadonlyRootFilesystem "Use generic-lens or generic-optics with 'readonlyRootFilesystem' instead." #-}

-- | A list of data volumes used in a job.
--
-- /Note:/ Consider using 'volumes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpVolumes :: Lens.Lens' ContainerProperties (Lude.Maybe [Volume])
cpVolumes = Lens.lens (volumes :: ContainerProperties -> Lude.Maybe [Volume]) (\s a -> s {volumes = a} :: ContainerProperties)
{-# DEPRECATED cpVolumes "Use generic-lens or generic-optics with 'volumes' instead." #-}

instance Lude.FromJSON ContainerProperties where
  parseJSON =
    Lude.withObject
      "ContainerProperties"
      ( \x ->
          ContainerProperties'
            Lude.<$> (x Lude..:? "image")
            Lude.<*> (x Lude..:? "command" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "secrets" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "environment" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ulimits" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "executionRoleArn")
            Lude.<*> (x Lude..:? "privileged")
            Lude.<*> (x Lude..:? "jobRoleArn")
            Lude.<*> (x Lude..:? "resourceRequirements" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "instanceType")
            Lude.<*> (x Lude..:? "memory")
            Lude.<*> (x Lude..:? "user")
            Lude.<*> (x Lude..:? "logConfiguration")
            Lude.<*> (x Lude..:? "linuxParameters")
            Lude.<*> (x Lude..:? "mountPoints" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "vcpus")
            Lude.<*> (x Lude..:? "readonlyRootFilesystem")
            Lude.<*> (x Lude..:? "volumes" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON ContainerProperties where
  toJSON ContainerProperties' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("image" Lude..=) Lude.<$> image,
            ("command" Lude..=) Lude.<$> command,
            ("secrets" Lude..=) Lude.<$> secrets,
            ("environment" Lude..=) Lude.<$> environment,
            ("ulimits" Lude..=) Lude.<$> ulimits,
            ("executionRoleArn" Lude..=) Lude.<$> executionRoleARN,
            ("privileged" Lude..=) Lude.<$> privileged,
            ("jobRoleArn" Lude..=) Lude.<$> jobRoleARN,
            ("resourceRequirements" Lude..=) Lude.<$> resourceRequirements,
            ("instanceType" Lude..=) Lude.<$> instanceType,
            ("memory" Lude..=) Lude.<$> memory,
            ("user" Lude..=) Lude.<$> user,
            ("logConfiguration" Lude..=) Lude.<$> logConfiguration,
            ("linuxParameters" Lude..=) Lude.<$> linuxParameters,
            ("mountPoints" Lude..=) Lude.<$> mountPoints,
            ("vcpus" Lude..=) Lude.<$> vcpus,
            ("readonlyRootFilesystem" Lude..=) Lude.<$> readonlyRootFilesystem,
            ("volumes" Lude..=) Lude.<$> volumes
          ]
      )
