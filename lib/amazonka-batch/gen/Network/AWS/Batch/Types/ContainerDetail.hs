{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    cdCommand,
    cdContainerInstanceArn,
    cdEnvironment,
    cdExecutionRoleArn,
    cdExitCode,
    cdImage,
    cdInstanceType,
    cdJobRoleArn,
    cdLinuxParameters,
    cdLogConfiguration,
    cdLogStreamName,
    cdMemory,
    cdMountPoints,
    cdNetworkInterfaces,
    cdPrivileged,
    cdReadonlyRootFilesystem,
    cdReason,
    cdResourceRequirements,
    cdSecrets,
    cdTaskArn,
    cdUlimits,
    cdUser,
    cdVcpus,
    cdVolumes,
  )
where

import qualified Network.AWS.Batch.Types.KeyValuePair as Types
import qualified Network.AWS.Batch.Types.LinuxParameters as Types
import qualified Network.AWS.Batch.Types.LogConfiguration as Types
import qualified Network.AWS.Batch.Types.MountPoint as Types
import qualified Network.AWS.Batch.Types.NetworkInterface as Types
import qualified Network.AWS.Batch.Types.ResourceRequirement as Types
import qualified Network.AWS.Batch.Types.Secret as Types
import qualified Network.AWS.Batch.Types.String as Types
import qualified Network.AWS.Batch.Types.Ulimit as Types
import qualified Network.AWS.Batch.Types.Volume as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing the details of a container that is part of a job.
--
-- /See:/ 'mkContainerDetail' smart constructor.
data ContainerDetail = ContainerDetail'
  { -- | The command that is passed to the container.
    command :: Core.Maybe [Types.String],
    -- | The Amazon Resource Name (ARN) of the container instance on which the container is running.
    containerInstanceArn :: Core.Maybe Types.String,
    -- | The environment variables to pass to a container.
    environment :: Core.Maybe [Types.KeyValuePair],
    -- | The Amazon Resource Name (ARN) of the execution role that AWS Batch can assume. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/execution-IAM-role.html AWS Batch execution IAM role> .
    executionRoleArn :: Core.Maybe Types.String,
    -- | The exit code to return upon completion.
    exitCode :: Core.Maybe Core.Int,
    -- | The image used to start the container.
    image :: Core.Maybe Types.String,
    -- | The instance type of the underlying host infrastructure of a multi-node parallel job.
    instanceType :: Core.Maybe Types.String,
    -- | The Amazon Resource Name (ARN) associated with the job upon execution.
    jobRoleArn :: Core.Maybe Types.String,
    -- | Linux-specific modifications that are applied to the container, such as details for device mappings.
    linuxParameters :: Core.Maybe Types.LinuxParameters,
    -- | The log configuration specification for the container.
    --
    -- This parameter maps to @LogConfig@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--log-driver@ option to <https://docs.docker.com/engine/reference/run/ docker run> . By default, containers use the same logging driver that the Docker daemon uses. However the container may use a different logging driver than the Docker daemon by specifying a log driver with this parameter in the container definition. To use a different logging driver for a container, the log system must be configured properly on the container instance (or on a different log server for remote logging options). For more information on the options for different supported log drivers, see <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers> in the Docker documentation.
    -- This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@
    logConfiguration :: Core.Maybe Types.LogConfiguration,
    -- | The name of the CloudWatch Logs log stream associated with the container. The log group for AWS Batch jobs is @/aws/batch/job@ . Each container attempt receives a log stream name when they reach the @RUNNING@ status.
    logStreamName :: Core.Maybe Types.String,
    -- | The number of MiB of memory reserved for the job. This is a required parameter.
    memory :: Core.Maybe Core.Int,
    -- | The mount points for data volumes in your container.
    mountPoints :: Core.Maybe [Types.MountPoint],
    -- | The network interfaces associated with the job.
    networkInterfaces :: Core.Maybe [Types.NetworkInterface],
    -- | When this parameter is true, the container is given elevated privileges on the host container instance (similar to the @root@ user).
    privileged :: Core.Maybe Core.Bool,
    -- | When this parameter is true, the container is given read-only access to its root file system.
    readonlyRootFilesystem :: Core.Maybe Core.Bool,
    -- | A short (255 max characters) human-readable string to provide additional details about a running or stopped container.
    reason :: Core.Maybe Types.String,
    -- | The type and amount of a resource to assign to a container. Currently, the only supported resource is @GPU@ .
    resourceRequirements :: Core.Maybe [Types.ResourceRequirement],
    -- | The secrets to pass to the container. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html Specifying Sensitive Data> in the /Amazon Elastic Container Service Developer Guide/ .
    secrets :: Core.Maybe [Types.Secret],
    -- | The Amazon Resource Name (ARN) of the Amazon ECS task that is associated with the container job. Each container attempt receives a task ARN when they reach the @STARTING@ status.
    taskArn :: Core.Maybe Types.String,
    -- | A list of @ulimit@ values to set in the container.
    ulimits :: Core.Maybe [Types.Ulimit],
    -- | The user name to use inside the container.
    user :: Core.Maybe Types.String,
    -- | The number of VCPUs allocated for the job. This is a required parameter.
    vcpus :: Core.Maybe Core.Int,
    -- | A list of volumes associated with the job.
    volumes :: Core.Maybe [Types.Volume]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContainerDetail' value with any optional fields omitted.
mkContainerDetail ::
  ContainerDetail
mkContainerDetail =
  ContainerDetail'
    { command = Core.Nothing,
      containerInstanceArn = Core.Nothing,
      environment = Core.Nothing,
      executionRoleArn = Core.Nothing,
      exitCode = Core.Nothing,
      image = Core.Nothing,
      instanceType = Core.Nothing,
      jobRoleArn = Core.Nothing,
      linuxParameters = Core.Nothing,
      logConfiguration = Core.Nothing,
      logStreamName = Core.Nothing,
      memory = Core.Nothing,
      mountPoints = Core.Nothing,
      networkInterfaces = Core.Nothing,
      privileged = Core.Nothing,
      readonlyRootFilesystem = Core.Nothing,
      reason = Core.Nothing,
      resourceRequirements = Core.Nothing,
      secrets = Core.Nothing,
      taskArn = Core.Nothing,
      ulimits = Core.Nothing,
      user = Core.Nothing,
      vcpus = Core.Nothing,
      volumes = Core.Nothing
    }

-- | The command that is passed to the container.
--
-- /Note:/ Consider using 'command' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCommand :: Lens.Lens' ContainerDetail (Core.Maybe [Types.String])
cdCommand = Lens.field @"command"
{-# DEPRECATED cdCommand "Use generic-lens or generic-optics with 'command' instead." #-}

-- | The Amazon Resource Name (ARN) of the container instance on which the container is running.
--
-- /Note:/ Consider using 'containerInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdContainerInstanceArn :: Lens.Lens' ContainerDetail (Core.Maybe Types.String)
cdContainerInstanceArn = Lens.field @"containerInstanceArn"
{-# DEPRECATED cdContainerInstanceArn "Use generic-lens or generic-optics with 'containerInstanceArn' instead." #-}

-- | The environment variables to pass to a container.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdEnvironment :: Lens.Lens' ContainerDetail (Core.Maybe [Types.KeyValuePair])
cdEnvironment = Lens.field @"environment"
{-# DEPRECATED cdEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | The Amazon Resource Name (ARN) of the execution role that AWS Batch can assume. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/execution-IAM-role.html AWS Batch execution IAM role> .
--
-- /Note:/ Consider using 'executionRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdExecutionRoleArn :: Lens.Lens' ContainerDetail (Core.Maybe Types.String)
cdExecutionRoleArn = Lens.field @"executionRoleArn"
{-# DEPRECATED cdExecutionRoleArn "Use generic-lens or generic-optics with 'executionRoleArn' instead." #-}

-- | The exit code to return upon completion.
--
-- /Note:/ Consider using 'exitCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdExitCode :: Lens.Lens' ContainerDetail (Core.Maybe Core.Int)
cdExitCode = Lens.field @"exitCode"
{-# DEPRECATED cdExitCode "Use generic-lens or generic-optics with 'exitCode' instead." #-}

-- | The image used to start the container.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdImage :: Lens.Lens' ContainerDetail (Core.Maybe Types.String)
cdImage = Lens.field @"image"
{-# DEPRECATED cdImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | The instance type of the underlying host infrastructure of a multi-node parallel job.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdInstanceType :: Lens.Lens' ContainerDetail (Core.Maybe Types.String)
cdInstanceType = Lens.field @"instanceType"
{-# DEPRECATED cdInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The Amazon Resource Name (ARN) associated with the job upon execution.
--
-- /Note:/ Consider using 'jobRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdJobRoleArn :: Lens.Lens' ContainerDetail (Core.Maybe Types.String)
cdJobRoleArn = Lens.field @"jobRoleArn"
{-# DEPRECATED cdJobRoleArn "Use generic-lens or generic-optics with 'jobRoleArn' instead." #-}

-- | Linux-specific modifications that are applied to the container, such as details for device mappings.
--
-- /Note:/ Consider using 'linuxParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdLinuxParameters :: Lens.Lens' ContainerDetail (Core.Maybe Types.LinuxParameters)
cdLinuxParameters = Lens.field @"linuxParameters"
{-# DEPRECATED cdLinuxParameters "Use generic-lens or generic-optics with 'linuxParameters' instead." #-}

-- | The log configuration specification for the container.
--
-- This parameter maps to @LogConfig@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--log-driver@ option to <https://docs.docker.com/engine/reference/run/ docker run> . By default, containers use the same logging driver that the Docker daemon uses. However the container may use a different logging driver than the Docker daemon by specifying a log driver with this parameter in the container definition. To use a different logging driver for a container, the log system must be configured properly on the container instance (or on a different log server for remote logging options). For more information on the options for different supported log drivers, see <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers> in the Docker documentation.
-- This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@
--
-- /Note:/ Consider using 'logConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdLogConfiguration :: Lens.Lens' ContainerDetail (Core.Maybe Types.LogConfiguration)
cdLogConfiguration = Lens.field @"logConfiguration"
{-# DEPRECATED cdLogConfiguration "Use generic-lens or generic-optics with 'logConfiguration' instead." #-}

-- | The name of the CloudWatch Logs log stream associated with the container. The log group for AWS Batch jobs is @/aws/batch/job@ . Each container attempt receives a log stream name when they reach the @RUNNING@ status.
--
-- /Note:/ Consider using 'logStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdLogStreamName :: Lens.Lens' ContainerDetail (Core.Maybe Types.String)
cdLogStreamName = Lens.field @"logStreamName"
{-# DEPRECATED cdLogStreamName "Use generic-lens or generic-optics with 'logStreamName' instead." #-}

-- | The number of MiB of memory reserved for the job. This is a required parameter.
--
-- /Note:/ Consider using 'memory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdMemory :: Lens.Lens' ContainerDetail (Core.Maybe Core.Int)
cdMemory = Lens.field @"memory"
{-# DEPRECATED cdMemory "Use generic-lens or generic-optics with 'memory' instead." #-}

-- | The mount points for data volumes in your container.
--
-- /Note:/ Consider using 'mountPoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdMountPoints :: Lens.Lens' ContainerDetail (Core.Maybe [Types.MountPoint])
cdMountPoints = Lens.field @"mountPoints"
{-# DEPRECATED cdMountPoints "Use generic-lens or generic-optics with 'mountPoints' instead." #-}

-- | The network interfaces associated with the job.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdNetworkInterfaces :: Lens.Lens' ContainerDetail (Core.Maybe [Types.NetworkInterface])
cdNetworkInterfaces = Lens.field @"networkInterfaces"
{-# DEPRECATED cdNetworkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead." #-}

-- | When this parameter is true, the container is given elevated privileges on the host container instance (similar to the @root@ user).
--
-- /Note:/ Consider using 'privileged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdPrivileged :: Lens.Lens' ContainerDetail (Core.Maybe Core.Bool)
cdPrivileged = Lens.field @"privileged"
{-# DEPRECATED cdPrivileged "Use generic-lens or generic-optics with 'privileged' instead." #-}

-- | When this parameter is true, the container is given read-only access to its root file system.
--
-- /Note:/ Consider using 'readonlyRootFilesystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdReadonlyRootFilesystem :: Lens.Lens' ContainerDetail (Core.Maybe Core.Bool)
cdReadonlyRootFilesystem = Lens.field @"readonlyRootFilesystem"
{-# DEPRECATED cdReadonlyRootFilesystem "Use generic-lens or generic-optics with 'readonlyRootFilesystem' instead." #-}

-- | A short (255 max characters) human-readable string to provide additional details about a running or stopped container.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdReason :: Lens.Lens' ContainerDetail (Core.Maybe Types.String)
cdReason = Lens.field @"reason"
{-# DEPRECATED cdReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The type and amount of a resource to assign to a container. Currently, the only supported resource is @GPU@ .
--
-- /Note:/ Consider using 'resourceRequirements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdResourceRequirements :: Lens.Lens' ContainerDetail (Core.Maybe [Types.ResourceRequirement])
cdResourceRequirements = Lens.field @"resourceRequirements"
{-# DEPRECATED cdResourceRequirements "Use generic-lens or generic-optics with 'resourceRequirements' instead." #-}

-- | The secrets to pass to the container. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html Specifying Sensitive Data> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'secrets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdSecrets :: Lens.Lens' ContainerDetail (Core.Maybe [Types.Secret])
cdSecrets = Lens.field @"secrets"
{-# DEPRECATED cdSecrets "Use generic-lens or generic-optics with 'secrets' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon ECS task that is associated with the container job. Each container attempt receives a task ARN when they reach the @STARTING@ status.
--
-- /Note:/ Consider using 'taskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdTaskArn :: Lens.Lens' ContainerDetail (Core.Maybe Types.String)
cdTaskArn = Lens.field @"taskArn"
{-# DEPRECATED cdTaskArn "Use generic-lens or generic-optics with 'taskArn' instead." #-}

-- | A list of @ulimit@ values to set in the container.
--
-- /Note:/ Consider using 'ulimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdUlimits :: Lens.Lens' ContainerDetail (Core.Maybe [Types.Ulimit])
cdUlimits = Lens.field @"ulimits"
{-# DEPRECATED cdUlimits "Use generic-lens or generic-optics with 'ulimits' instead." #-}

-- | The user name to use inside the container.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdUser :: Lens.Lens' ContainerDetail (Core.Maybe Types.String)
cdUser = Lens.field @"user"
{-# DEPRECATED cdUser "Use generic-lens or generic-optics with 'user' instead." #-}

-- | The number of VCPUs allocated for the job. This is a required parameter.
--
-- /Note:/ Consider using 'vcpus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdVcpus :: Lens.Lens' ContainerDetail (Core.Maybe Core.Int)
cdVcpus = Lens.field @"vcpus"
{-# DEPRECATED cdVcpus "Use generic-lens or generic-optics with 'vcpus' instead." #-}

-- | A list of volumes associated with the job.
--
-- /Note:/ Consider using 'volumes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdVolumes :: Lens.Lens' ContainerDetail (Core.Maybe [Types.Volume])
cdVolumes = Lens.field @"volumes"
{-# DEPRECATED cdVolumes "Use generic-lens or generic-optics with 'volumes' instead." #-}

instance Core.FromJSON ContainerDetail where
  parseJSON =
    Core.withObject "ContainerDetail" Core.$
      \x ->
        ContainerDetail'
          Core.<$> (x Core..:? "command")
          Core.<*> (x Core..:? "containerInstanceArn")
          Core.<*> (x Core..:? "environment")
          Core.<*> (x Core..:? "executionRoleArn")
          Core.<*> (x Core..:? "exitCode")
          Core.<*> (x Core..:? "image")
          Core.<*> (x Core..:? "instanceType")
          Core.<*> (x Core..:? "jobRoleArn")
          Core.<*> (x Core..:? "linuxParameters")
          Core.<*> (x Core..:? "logConfiguration")
          Core.<*> (x Core..:? "logStreamName")
          Core.<*> (x Core..:? "memory")
          Core.<*> (x Core..:? "mountPoints")
          Core.<*> (x Core..:? "networkInterfaces")
          Core.<*> (x Core..:? "privileged")
          Core.<*> (x Core..:? "readonlyRootFilesystem")
          Core.<*> (x Core..:? "reason")
          Core.<*> (x Core..:? "resourceRequirements")
          Core.<*> (x Core..:? "secrets")
          Core.<*> (x Core..:? "taskArn")
          Core.<*> (x Core..:? "ulimits")
          Core.<*> (x Core..:? "user")
          Core.<*> (x Core..:? "vcpus")
          Core.<*> (x Core..:? "volumes")
