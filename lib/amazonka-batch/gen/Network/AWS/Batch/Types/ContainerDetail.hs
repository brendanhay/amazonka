-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ContainerDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ContainerDetail
  ( ContainerDetail (..),

    -- * Smart constructor
    mkContainerDetail,

    -- * Lenses
    cdImage,
    cdCommand,
    cdSecrets,
    cdEnvironment,
    cdNetworkInterfaces,
    cdTaskARN,
    cdUlimits,
    cdContainerInstanceARN,
    cdExecutionRoleARN,
    cdPrivileged,
    cdJobRoleARN,
    cdResourceRequirements,
    cdInstanceType,
    cdMemory,
    cdUser,
    cdLogConfiguration,
    cdLinuxParameters,
    cdReason,
    cdLogStreamName,
    cdMountPoints,
    cdExitCode,
    cdVcpus,
    cdReadonlyRootFilesystem,
    cdVolumes,
  )
where

import Network.AWS.Batch.Types.KeyValuePair
import Network.AWS.Batch.Types.LinuxParameters
import Network.AWS.Batch.Types.LogConfiguration
import Network.AWS.Batch.Types.MountPoint
import Network.AWS.Batch.Types.NetworkInterface
import Network.AWS.Batch.Types.ResourceRequirement
import Network.AWS.Batch.Types.Secret
import Network.AWS.Batch.Types.Ulimit
import Network.AWS.Batch.Types.Volume
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing the details of a container that is part of a job.
--
-- /See:/ 'mkContainerDetail' smart constructor.
data ContainerDetail = ContainerDetail'
  { image ::
      Lude.Maybe Lude.Text,
    command :: Lude.Maybe [Lude.Text],
    secrets :: Lude.Maybe [Secret],
    environment :: Lude.Maybe [KeyValuePair],
    networkInterfaces :: Lude.Maybe [NetworkInterface],
    taskARN :: Lude.Maybe Lude.Text,
    ulimits :: Lude.Maybe [Ulimit],
    containerInstanceARN :: Lude.Maybe Lude.Text,
    executionRoleARN :: Lude.Maybe Lude.Text,
    privileged :: Lude.Maybe Lude.Bool,
    jobRoleARN :: Lude.Maybe Lude.Text,
    resourceRequirements :: Lude.Maybe [ResourceRequirement],
    instanceType :: Lude.Maybe Lude.Text,
    memory :: Lude.Maybe Lude.Int,
    user :: Lude.Maybe Lude.Text,
    logConfiguration :: Lude.Maybe LogConfiguration,
    linuxParameters :: Lude.Maybe LinuxParameters,
    reason :: Lude.Maybe Lude.Text,
    logStreamName :: Lude.Maybe Lude.Text,
    mountPoints :: Lude.Maybe [MountPoint],
    exitCode :: Lude.Maybe Lude.Int,
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

-- | Creates a value of 'ContainerDetail' with the minimum fields required to make a request.
--
-- * 'command' - The command that is passed to the container.
-- * 'containerInstanceARN' - The Amazon Resource Name (ARN) of the container instance on which the container is running.
-- * 'environment' - The environment variables to pass to a container.
-- * 'executionRoleARN' - The Amazon Resource Name (ARN) of the execution role that AWS Batch can assume. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/execution-IAM-role.html AWS Batch execution IAM role> .
-- * 'exitCode' - The exit code to return upon completion.
-- * 'image' - The image used to start the container.
-- * 'instanceType' - The instance type of the underlying host infrastructure of a multi-node parallel job.
-- * 'jobRoleARN' - The Amazon Resource Name (ARN) associated with the job upon execution.
-- * 'linuxParameters' - Linux-specific modifications that are applied to the container, such as details for device mappings.
-- * 'logConfiguration' - The log configuration specification for the container.
--
-- This parameter maps to @LogConfig@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--log-driver@ option to <https://docs.docker.com/engine/reference/run/ docker run> . By default, containers use the same logging driver that the Docker daemon uses. However the container may use a different logging driver than the Docker daemon by specifying a log driver with this parameter in the container definition. To use a different logging driver for a container, the log system must be configured properly on the container instance (or on a different log server for remote logging options). For more information on the options for different supported log drivers, see <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers> in the Docker documentation.
-- This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@
-- * 'logStreamName' - The name of the CloudWatch Logs log stream associated with the container. The log group for AWS Batch jobs is @/aws/batch/job@ . Each container attempt receives a log stream name when they reach the @RUNNING@ status.
-- * 'memory' - The number of MiB of memory reserved for the job. This is a required parameter.
-- * 'mountPoints' - The mount points for data volumes in your container.
-- * 'networkInterfaces' - The network interfaces associated with the job.
-- * 'privileged' - When this parameter is true, the container is given elevated privileges on the host container instance (similar to the @root@ user).
-- * 'readonlyRootFilesystem' - When this parameter is true, the container is given read-only access to its root file system.
-- * 'reason' - A short (255 max characters) human-readable string to provide additional details about a running or stopped container.
-- * 'resourceRequirements' - The type and amount of a resource to assign to a container. Currently, the only supported resource is @GPU@ .
-- * 'secrets' - The secrets to pass to the container. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html Specifying Sensitive Data> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'taskARN' - The Amazon Resource Name (ARN) of the Amazon ECS task that is associated with the container job. Each container attempt receives a task ARN when they reach the @STARTING@ status.
-- * 'ulimits' - A list of @ulimit@ values to set in the container.
-- * 'user' - The user name to use inside the container.
-- * 'vcpus' - The number of VCPUs allocated for the job. This is a required parameter.
-- * 'volumes' - A list of volumes associated with the job.
mkContainerDetail ::
  ContainerDetail
mkContainerDetail =
  ContainerDetail'
    { image = Lude.Nothing,
      command = Lude.Nothing,
      secrets = Lude.Nothing,
      environment = Lude.Nothing,
      networkInterfaces = Lude.Nothing,
      taskARN = Lude.Nothing,
      ulimits = Lude.Nothing,
      containerInstanceARN = Lude.Nothing,
      executionRoleARN = Lude.Nothing,
      privileged = Lude.Nothing,
      jobRoleARN = Lude.Nothing,
      resourceRequirements = Lude.Nothing,
      instanceType = Lude.Nothing,
      memory = Lude.Nothing,
      user = Lude.Nothing,
      logConfiguration = Lude.Nothing,
      linuxParameters = Lude.Nothing,
      reason = Lude.Nothing,
      logStreamName = Lude.Nothing,
      mountPoints = Lude.Nothing,
      exitCode = Lude.Nothing,
      vcpus = Lude.Nothing,
      readonlyRootFilesystem = Lude.Nothing,
      volumes = Lude.Nothing
    }

-- | The image used to start the container.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdImage :: Lens.Lens' ContainerDetail (Lude.Maybe Lude.Text)
cdImage = Lens.lens (image :: ContainerDetail -> Lude.Maybe Lude.Text) (\s a -> s {image = a} :: ContainerDetail)
{-# DEPRECATED cdImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | The command that is passed to the container.
--
-- /Note:/ Consider using 'command' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCommand :: Lens.Lens' ContainerDetail (Lude.Maybe [Lude.Text])
cdCommand = Lens.lens (command :: ContainerDetail -> Lude.Maybe [Lude.Text]) (\s a -> s {command = a} :: ContainerDetail)
{-# DEPRECATED cdCommand "Use generic-lens or generic-optics with 'command' instead." #-}

-- | The secrets to pass to the container. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html Specifying Sensitive Data> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'secrets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdSecrets :: Lens.Lens' ContainerDetail (Lude.Maybe [Secret])
cdSecrets = Lens.lens (secrets :: ContainerDetail -> Lude.Maybe [Secret]) (\s a -> s {secrets = a} :: ContainerDetail)
{-# DEPRECATED cdSecrets "Use generic-lens or generic-optics with 'secrets' instead." #-}

-- | The environment variables to pass to a container.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdEnvironment :: Lens.Lens' ContainerDetail (Lude.Maybe [KeyValuePair])
cdEnvironment = Lens.lens (environment :: ContainerDetail -> Lude.Maybe [KeyValuePair]) (\s a -> s {environment = a} :: ContainerDetail)
{-# DEPRECATED cdEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | The network interfaces associated with the job.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdNetworkInterfaces :: Lens.Lens' ContainerDetail (Lude.Maybe [NetworkInterface])
cdNetworkInterfaces = Lens.lens (networkInterfaces :: ContainerDetail -> Lude.Maybe [NetworkInterface]) (\s a -> s {networkInterfaces = a} :: ContainerDetail)
{-# DEPRECATED cdNetworkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon ECS task that is associated with the container job. Each container attempt receives a task ARN when they reach the @STARTING@ status.
--
-- /Note:/ Consider using 'taskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdTaskARN :: Lens.Lens' ContainerDetail (Lude.Maybe Lude.Text)
cdTaskARN = Lens.lens (taskARN :: ContainerDetail -> Lude.Maybe Lude.Text) (\s a -> s {taskARN = a} :: ContainerDetail)
{-# DEPRECATED cdTaskARN "Use generic-lens or generic-optics with 'taskARN' instead." #-}

-- | A list of @ulimit@ values to set in the container.
--
-- /Note:/ Consider using 'ulimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdUlimits :: Lens.Lens' ContainerDetail (Lude.Maybe [Ulimit])
cdUlimits = Lens.lens (ulimits :: ContainerDetail -> Lude.Maybe [Ulimit]) (\s a -> s {ulimits = a} :: ContainerDetail)
{-# DEPRECATED cdUlimits "Use generic-lens or generic-optics with 'ulimits' instead." #-}

-- | The Amazon Resource Name (ARN) of the container instance on which the container is running.
--
-- /Note:/ Consider using 'containerInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdContainerInstanceARN :: Lens.Lens' ContainerDetail (Lude.Maybe Lude.Text)
cdContainerInstanceARN = Lens.lens (containerInstanceARN :: ContainerDetail -> Lude.Maybe Lude.Text) (\s a -> s {containerInstanceARN = a} :: ContainerDetail)
{-# DEPRECATED cdContainerInstanceARN "Use generic-lens or generic-optics with 'containerInstanceARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the execution role that AWS Batch can assume. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/execution-IAM-role.html AWS Batch execution IAM role> .
--
-- /Note:/ Consider using 'executionRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdExecutionRoleARN :: Lens.Lens' ContainerDetail (Lude.Maybe Lude.Text)
cdExecutionRoleARN = Lens.lens (executionRoleARN :: ContainerDetail -> Lude.Maybe Lude.Text) (\s a -> s {executionRoleARN = a} :: ContainerDetail)
{-# DEPRECATED cdExecutionRoleARN "Use generic-lens or generic-optics with 'executionRoleARN' instead." #-}

-- | When this parameter is true, the container is given elevated privileges on the host container instance (similar to the @root@ user).
--
-- /Note:/ Consider using 'privileged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdPrivileged :: Lens.Lens' ContainerDetail (Lude.Maybe Lude.Bool)
cdPrivileged = Lens.lens (privileged :: ContainerDetail -> Lude.Maybe Lude.Bool) (\s a -> s {privileged = a} :: ContainerDetail)
{-# DEPRECATED cdPrivileged "Use generic-lens or generic-optics with 'privileged' instead." #-}

-- | The Amazon Resource Name (ARN) associated with the job upon execution.
--
-- /Note:/ Consider using 'jobRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdJobRoleARN :: Lens.Lens' ContainerDetail (Lude.Maybe Lude.Text)
cdJobRoleARN = Lens.lens (jobRoleARN :: ContainerDetail -> Lude.Maybe Lude.Text) (\s a -> s {jobRoleARN = a} :: ContainerDetail)
{-# DEPRECATED cdJobRoleARN "Use generic-lens or generic-optics with 'jobRoleARN' instead." #-}

-- | The type and amount of a resource to assign to a container. Currently, the only supported resource is @GPU@ .
--
-- /Note:/ Consider using 'resourceRequirements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdResourceRequirements :: Lens.Lens' ContainerDetail (Lude.Maybe [ResourceRequirement])
cdResourceRequirements = Lens.lens (resourceRequirements :: ContainerDetail -> Lude.Maybe [ResourceRequirement]) (\s a -> s {resourceRequirements = a} :: ContainerDetail)
{-# DEPRECATED cdResourceRequirements "Use generic-lens or generic-optics with 'resourceRequirements' instead." #-}

-- | The instance type of the underlying host infrastructure of a multi-node parallel job.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdInstanceType :: Lens.Lens' ContainerDetail (Lude.Maybe Lude.Text)
cdInstanceType = Lens.lens (instanceType :: ContainerDetail -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: ContainerDetail)
{-# DEPRECATED cdInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The number of MiB of memory reserved for the job. This is a required parameter.
--
-- /Note:/ Consider using 'memory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdMemory :: Lens.Lens' ContainerDetail (Lude.Maybe Lude.Int)
cdMemory = Lens.lens (memory :: ContainerDetail -> Lude.Maybe Lude.Int) (\s a -> s {memory = a} :: ContainerDetail)
{-# DEPRECATED cdMemory "Use generic-lens or generic-optics with 'memory' instead." #-}

-- | The user name to use inside the container.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdUser :: Lens.Lens' ContainerDetail (Lude.Maybe Lude.Text)
cdUser = Lens.lens (user :: ContainerDetail -> Lude.Maybe Lude.Text) (\s a -> s {user = a} :: ContainerDetail)
{-# DEPRECATED cdUser "Use generic-lens or generic-optics with 'user' instead." #-}

-- | The log configuration specification for the container.
--
-- This parameter maps to @LogConfig@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--log-driver@ option to <https://docs.docker.com/engine/reference/run/ docker run> . By default, containers use the same logging driver that the Docker daemon uses. However the container may use a different logging driver than the Docker daemon by specifying a log driver with this parameter in the container definition. To use a different logging driver for a container, the log system must be configured properly on the container instance (or on a different log server for remote logging options). For more information on the options for different supported log drivers, see <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers> in the Docker documentation.
-- This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@
--
-- /Note:/ Consider using 'logConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdLogConfiguration :: Lens.Lens' ContainerDetail (Lude.Maybe LogConfiguration)
cdLogConfiguration = Lens.lens (logConfiguration :: ContainerDetail -> Lude.Maybe LogConfiguration) (\s a -> s {logConfiguration = a} :: ContainerDetail)
{-# DEPRECATED cdLogConfiguration "Use generic-lens or generic-optics with 'logConfiguration' instead." #-}

-- | Linux-specific modifications that are applied to the container, such as details for device mappings.
--
-- /Note:/ Consider using 'linuxParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdLinuxParameters :: Lens.Lens' ContainerDetail (Lude.Maybe LinuxParameters)
cdLinuxParameters = Lens.lens (linuxParameters :: ContainerDetail -> Lude.Maybe LinuxParameters) (\s a -> s {linuxParameters = a} :: ContainerDetail)
{-# DEPRECATED cdLinuxParameters "Use generic-lens or generic-optics with 'linuxParameters' instead." #-}

-- | A short (255 max characters) human-readable string to provide additional details about a running or stopped container.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdReason :: Lens.Lens' ContainerDetail (Lude.Maybe Lude.Text)
cdReason = Lens.lens (reason :: ContainerDetail -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: ContainerDetail)
{-# DEPRECATED cdReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The name of the CloudWatch Logs log stream associated with the container. The log group for AWS Batch jobs is @/aws/batch/job@ . Each container attempt receives a log stream name when they reach the @RUNNING@ status.
--
-- /Note:/ Consider using 'logStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdLogStreamName :: Lens.Lens' ContainerDetail (Lude.Maybe Lude.Text)
cdLogStreamName = Lens.lens (logStreamName :: ContainerDetail -> Lude.Maybe Lude.Text) (\s a -> s {logStreamName = a} :: ContainerDetail)
{-# DEPRECATED cdLogStreamName "Use generic-lens or generic-optics with 'logStreamName' instead." #-}

-- | The mount points for data volumes in your container.
--
-- /Note:/ Consider using 'mountPoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdMountPoints :: Lens.Lens' ContainerDetail (Lude.Maybe [MountPoint])
cdMountPoints = Lens.lens (mountPoints :: ContainerDetail -> Lude.Maybe [MountPoint]) (\s a -> s {mountPoints = a} :: ContainerDetail)
{-# DEPRECATED cdMountPoints "Use generic-lens or generic-optics with 'mountPoints' instead." #-}

-- | The exit code to return upon completion.
--
-- /Note:/ Consider using 'exitCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdExitCode :: Lens.Lens' ContainerDetail (Lude.Maybe Lude.Int)
cdExitCode = Lens.lens (exitCode :: ContainerDetail -> Lude.Maybe Lude.Int) (\s a -> s {exitCode = a} :: ContainerDetail)
{-# DEPRECATED cdExitCode "Use generic-lens or generic-optics with 'exitCode' instead." #-}

-- | The number of VCPUs allocated for the job. This is a required parameter.
--
-- /Note:/ Consider using 'vcpus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdVcpus :: Lens.Lens' ContainerDetail (Lude.Maybe Lude.Int)
cdVcpus = Lens.lens (vcpus :: ContainerDetail -> Lude.Maybe Lude.Int) (\s a -> s {vcpus = a} :: ContainerDetail)
{-# DEPRECATED cdVcpus "Use generic-lens or generic-optics with 'vcpus' instead." #-}

-- | When this parameter is true, the container is given read-only access to its root file system.
--
-- /Note:/ Consider using 'readonlyRootFilesystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdReadonlyRootFilesystem :: Lens.Lens' ContainerDetail (Lude.Maybe Lude.Bool)
cdReadonlyRootFilesystem = Lens.lens (readonlyRootFilesystem :: ContainerDetail -> Lude.Maybe Lude.Bool) (\s a -> s {readonlyRootFilesystem = a} :: ContainerDetail)
{-# DEPRECATED cdReadonlyRootFilesystem "Use generic-lens or generic-optics with 'readonlyRootFilesystem' instead." #-}

-- | A list of volumes associated with the job.
--
-- /Note:/ Consider using 'volumes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdVolumes :: Lens.Lens' ContainerDetail (Lude.Maybe [Volume])
cdVolumes = Lens.lens (volumes :: ContainerDetail -> Lude.Maybe [Volume]) (\s a -> s {volumes = a} :: ContainerDetail)
{-# DEPRECATED cdVolumes "Use generic-lens or generic-optics with 'volumes' instead." #-}

instance Lude.FromJSON ContainerDetail where
  parseJSON =
    Lude.withObject
      "ContainerDetail"
      ( \x ->
          ContainerDetail'
            Lude.<$> (x Lude..:? "image")
            Lude.<*> (x Lude..:? "command" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "secrets" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "environment" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "networkInterfaces" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "taskArn")
            Lude.<*> (x Lude..:? "ulimits" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "containerInstanceArn")
            Lude.<*> (x Lude..:? "executionRoleArn")
            Lude.<*> (x Lude..:? "privileged")
            Lude.<*> (x Lude..:? "jobRoleArn")
            Lude.<*> (x Lude..:? "resourceRequirements" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "instanceType")
            Lude.<*> (x Lude..:? "memory")
            Lude.<*> (x Lude..:? "user")
            Lude.<*> (x Lude..:? "logConfiguration")
            Lude.<*> (x Lude..:? "linuxParameters")
            Lude.<*> (x Lude..:? "reason")
            Lude.<*> (x Lude..:? "logStreamName")
            Lude.<*> (x Lude..:? "mountPoints" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "exitCode")
            Lude.<*> (x Lude..:? "vcpus")
            Lude.<*> (x Lude..:? "readonlyRootFilesystem")
            Lude.<*> (x Lude..:? "volumes" Lude..!= Lude.mempty)
      )
