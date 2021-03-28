{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ContainerProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.ContainerProperties
  ( ContainerProperties (..)
  -- * Smart constructor
  , mkContainerProperties
  -- * Lenses
  , cpCommand
  , cpEnvironment
  , cpExecutionRoleArn
  , cpImage
  , cpInstanceType
  , cpJobRoleArn
  , cpLinuxParameters
  , cpLogConfiguration
  , cpMemory
  , cpMountPoints
  , cpPrivileged
  , cpReadonlyRootFilesystem
  , cpResourceRequirements
  , cpSecrets
  , cpUlimits
  , cpUser
  , cpVcpus
  , cpVolumes
  ) where

import qualified Network.AWS.Batch.Types.KeyValuePair as Types
import qualified Network.AWS.Batch.Types.LinuxParameters as Types
import qualified Network.AWS.Batch.Types.LogConfiguration as Types
import qualified Network.AWS.Batch.Types.MountPoint as Types
import qualified Network.AWS.Batch.Types.ResourceRequirement as Types
import qualified Network.AWS.Batch.Types.Secret as Types
import qualified Network.AWS.Batch.Types.Ulimit as Types
import qualified Network.AWS.Batch.Types.Volume as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Container properties are used in job definitions to describe the container that is launched as part of a job.
--
-- /See:/ 'mkContainerProperties' smart constructor.
data ContainerProperties = ContainerProperties'
  { command :: Core.Maybe [Core.Text]
    -- ^ The command that is passed to the container. This parameter maps to @Cmd@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @COMMAND@ parameter to <https://docs.docker.com/engine/reference/run/ docker run> . For more information, see <https://docs.docker.com/engine/reference/builder/#cmd https://docs.docker.com/engine/reference/builder/#cmd> .
  , environment :: Core.Maybe [Types.KeyValuePair]
    -- ^ The environment variables to pass to a container. This parameter maps to @Env@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--env@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- /Important:/ We do not recommend using plaintext environment variables for sensitive information, such as credential data.
  , executionRoleArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the execution role that AWS Batch can assume. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/execution-IAM-role.html AWS Batch execution IAM role> .
  , image :: Core.Maybe Core.Text
    -- ^ The image used to start a container. This string is passed directly to the Docker daemon. Images in the Docker Hub registry are available by default. Other repositories are specified with @/repository-url/ //image/ :/tag/ @ . Up to 255 letters (uppercase and lowercase), numbers, hyphens, underscores, colons, periods, forward slashes, and number signs are allowed. This parameter maps to @Image@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @IMAGE@ parameter of <https://docs.docker.com/engine/reference/run/ docker run> .
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
  , instanceType :: Core.Maybe Core.Text
    -- ^ The instance type to use for a multi-node parallel job. Currently all node groups in a multi-node parallel job must use the same instance type. This parameter is not valid for single-node container jobs.
  , jobRoleArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the IAM role that the container can assume for AWS permissions.
  , linuxParameters :: Core.Maybe Types.LinuxParameters
    -- ^ Linux-specific modifications that are applied to the container, such as details for device mappings.
  , logConfiguration :: Core.Maybe Types.LogConfiguration
    -- ^ The log configuration specification for the container.
--
-- This parameter maps to @LogConfig@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--log-driver@ option to <https://docs.docker.com/engine/reference/run/ docker run> . By default, containers use the same logging driver that the Docker daemon uses. However the container may use a different logging driver than the Docker daemon by specifying a log driver with this parameter in the container definition. To use a different logging driver for a container, the log system must be configured properly on the container instance (or on a different log server for remote logging options). For more information on the options for different supported log drivers, see <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers> in the Docker documentation.
-- This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@ 
  , memory :: Core.Maybe Core.Int
    -- ^ The hard limit (in MiB) of memory to present to the container. If your container attempts to exceed the memory specified here, the container is killed. This parameter maps to @Memory@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--memory@ option to <https://docs.docker.com/engine/reference/run/ docker run> . You must specify at least 4 MiB of memory for a job. This is required but can be specified in several places for multi-node parallel (MNP) jobs; it must be specified for each node at least once.
  , mountPoints :: Core.Maybe [Types.MountPoint]
    -- ^ The mount points for data volumes in your container. This parameter maps to @Volumes@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--volume@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
  , privileged :: Core.Maybe Core.Bool
    -- ^ When this parameter is true, the container is given elevated privileges on the host container instance (similar to the @root@ user). This parameter maps to @Privileged@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--privileged@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
  , readonlyRootFilesystem :: Core.Maybe Core.Bool
    -- ^ When this parameter is true, the container is given read-only access to its root file system. This parameter maps to @ReadonlyRootfs@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--read-only@ option to @docker run@ .
  , resourceRequirements :: Core.Maybe [Types.ResourceRequirement]
    -- ^ The type and amount of a resource to assign to a container. Currently, the only supported resource is @GPU@ .
  , secrets :: Core.Maybe [Types.Secret]
    -- ^ The secrets for the container. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html Specifying Sensitive Data> in the /Amazon Elastic Container Service Developer Guide/ .
  , ulimits :: Core.Maybe [Types.Ulimit]
    -- ^ A list of @ulimits@ to set in the container. This parameter maps to @Ulimits@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--ulimit@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
  , user :: Core.Maybe Core.Text
    -- ^ The user name to use inside the container. This parameter maps to @User@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--user@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
  , vcpus :: Core.Maybe Core.Int
    -- ^ The number of vCPUs reserved for the container. This parameter maps to @CpuShares@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--cpu-shares@ option to <https://docs.docker.com/engine/reference/run/ docker run> . Each vCPU is equivalent to 1,024 CPU shares. You must specify at least one vCPU. This is required but can be specified in several places for multi-node parallel (MNP) jobs; it must be specified for each node at least once.
  , volumes :: Core.Maybe [Types.Volume]
    -- ^ A list of data volumes used in a job.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContainerProperties' value with any optional fields omitted.
mkContainerProperties
    :: ContainerProperties
mkContainerProperties
  = ContainerProperties'{command = Core.Nothing,
                         environment = Core.Nothing, executionRoleArn = Core.Nothing,
                         image = Core.Nothing, instanceType = Core.Nothing,
                         jobRoleArn = Core.Nothing, linuxParameters = Core.Nothing,
                         logConfiguration = Core.Nothing, memory = Core.Nothing,
                         mountPoints = Core.Nothing, privileged = Core.Nothing,
                         readonlyRootFilesystem = Core.Nothing,
                         resourceRequirements = Core.Nothing, secrets = Core.Nothing,
                         ulimits = Core.Nothing, user = Core.Nothing, vcpus = Core.Nothing,
                         volumes = Core.Nothing}

-- | The command that is passed to the container. This parameter maps to @Cmd@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @COMMAND@ parameter to <https://docs.docker.com/engine/reference/run/ docker run> . For more information, see <https://docs.docker.com/engine/reference/builder/#cmd https://docs.docker.com/engine/reference/builder/#cmd> .
--
-- /Note:/ Consider using 'command' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpCommand :: Lens.Lens' ContainerProperties (Core.Maybe [Core.Text])
cpCommand = Lens.field @"command"
{-# INLINEABLE cpCommand #-}
{-# DEPRECATED command "Use generic-lens or generic-optics with 'command' instead"  #-}

-- | The environment variables to pass to a container. This parameter maps to @Env@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--env@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- /Important:/ We do not recommend using plaintext environment variables for sensitive information, such as credential data.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpEnvironment :: Lens.Lens' ContainerProperties (Core.Maybe [Types.KeyValuePair])
cpEnvironment = Lens.field @"environment"
{-# INLINEABLE cpEnvironment #-}
{-# DEPRECATED environment "Use generic-lens or generic-optics with 'environment' instead"  #-}

-- | The Amazon Resource Name (ARN) of the execution role that AWS Batch can assume. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/execution-IAM-role.html AWS Batch execution IAM role> .
--
-- /Note:/ Consider using 'executionRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpExecutionRoleArn :: Lens.Lens' ContainerProperties (Core.Maybe Core.Text)
cpExecutionRoleArn = Lens.field @"executionRoleArn"
{-# INLINEABLE cpExecutionRoleArn #-}
{-# DEPRECATED executionRoleArn "Use generic-lens or generic-optics with 'executionRoleArn' instead"  #-}

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
cpImage :: Lens.Lens' ContainerProperties (Core.Maybe Core.Text)
cpImage = Lens.field @"image"
{-# INLINEABLE cpImage #-}
{-# DEPRECATED image "Use generic-lens or generic-optics with 'image' instead"  #-}

-- | The instance type to use for a multi-node parallel job. Currently all node groups in a multi-node parallel job must use the same instance type. This parameter is not valid for single-node container jobs.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpInstanceType :: Lens.Lens' ContainerProperties (Core.Maybe Core.Text)
cpInstanceType = Lens.field @"instanceType"
{-# INLINEABLE cpInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The Amazon Resource Name (ARN) of the IAM role that the container can assume for AWS permissions.
--
-- /Note:/ Consider using 'jobRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpJobRoleArn :: Lens.Lens' ContainerProperties (Core.Maybe Core.Text)
cpJobRoleArn = Lens.field @"jobRoleArn"
{-# INLINEABLE cpJobRoleArn #-}
{-# DEPRECATED jobRoleArn "Use generic-lens or generic-optics with 'jobRoleArn' instead"  #-}

-- | Linux-specific modifications that are applied to the container, such as details for device mappings.
--
-- /Note:/ Consider using 'linuxParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpLinuxParameters :: Lens.Lens' ContainerProperties (Core.Maybe Types.LinuxParameters)
cpLinuxParameters = Lens.field @"linuxParameters"
{-# INLINEABLE cpLinuxParameters #-}
{-# DEPRECATED linuxParameters "Use generic-lens or generic-optics with 'linuxParameters' instead"  #-}

-- | The log configuration specification for the container.
--
-- This parameter maps to @LogConfig@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--log-driver@ option to <https://docs.docker.com/engine/reference/run/ docker run> . By default, containers use the same logging driver that the Docker daemon uses. However the container may use a different logging driver than the Docker daemon by specifying a log driver with this parameter in the container definition. To use a different logging driver for a container, the log system must be configured properly on the container instance (or on a different log server for remote logging options). For more information on the options for different supported log drivers, see <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers> in the Docker documentation.
-- This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@ 
--
-- /Note:/ Consider using 'logConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpLogConfiguration :: Lens.Lens' ContainerProperties (Core.Maybe Types.LogConfiguration)
cpLogConfiguration = Lens.field @"logConfiguration"
{-# INLINEABLE cpLogConfiguration #-}
{-# DEPRECATED logConfiguration "Use generic-lens or generic-optics with 'logConfiguration' instead"  #-}

-- | The hard limit (in MiB) of memory to present to the container. If your container attempts to exceed the memory specified here, the container is killed. This parameter maps to @Memory@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--memory@ option to <https://docs.docker.com/engine/reference/run/ docker run> . You must specify at least 4 MiB of memory for a job. This is required but can be specified in several places for multi-node parallel (MNP) jobs; it must be specified for each node at least once.
--
-- /Note:/ Consider using 'memory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpMemory :: Lens.Lens' ContainerProperties (Core.Maybe Core.Int)
cpMemory = Lens.field @"memory"
{-# INLINEABLE cpMemory #-}
{-# DEPRECATED memory "Use generic-lens or generic-optics with 'memory' instead"  #-}

-- | The mount points for data volumes in your container. This parameter maps to @Volumes@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--volume@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- /Note:/ Consider using 'mountPoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpMountPoints :: Lens.Lens' ContainerProperties (Core.Maybe [Types.MountPoint])
cpMountPoints = Lens.field @"mountPoints"
{-# INLINEABLE cpMountPoints #-}
{-# DEPRECATED mountPoints "Use generic-lens or generic-optics with 'mountPoints' instead"  #-}

-- | When this parameter is true, the container is given elevated privileges on the host container instance (similar to the @root@ user). This parameter maps to @Privileged@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--privileged@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- /Note:/ Consider using 'privileged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPrivileged :: Lens.Lens' ContainerProperties (Core.Maybe Core.Bool)
cpPrivileged = Lens.field @"privileged"
{-# INLINEABLE cpPrivileged #-}
{-# DEPRECATED privileged "Use generic-lens or generic-optics with 'privileged' instead"  #-}

-- | When this parameter is true, the container is given read-only access to its root file system. This parameter maps to @ReadonlyRootfs@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--read-only@ option to @docker run@ .
--
-- /Note:/ Consider using 'readonlyRootFilesystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpReadonlyRootFilesystem :: Lens.Lens' ContainerProperties (Core.Maybe Core.Bool)
cpReadonlyRootFilesystem = Lens.field @"readonlyRootFilesystem"
{-# INLINEABLE cpReadonlyRootFilesystem #-}
{-# DEPRECATED readonlyRootFilesystem "Use generic-lens or generic-optics with 'readonlyRootFilesystem' instead"  #-}

-- | The type and amount of a resource to assign to a container. Currently, the only supported resource is @GPU@ .
--
-- /Note:/ Consider using 'resourceRequirements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpResourceRequirements :: Lens.Lens' ContainerProperties (Core.Maybe [Types.ResourceRequirement])
cpResourceRequirements = Lens.field @"resourceRequirements"
{-# INLINEABLE cpResourceRequirements #-}
{-# DEPRECATED resourceRequirements "Use generic-lens or generic-optics with 'resourceRequirements' instead"  #-}

-- | The secrets for the container. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html Specifying Sensitive Data> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'secrets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSecrets :: Lens.Lens' ContainerProperties (Core.Maybe [Types.Secret])
cpSecrets = Lens.field @"secrets"
{-# INLINEABLE cpSecrets #-}
{-# DEPRECATED secrets "Use generic-lens or generic-optics with 'secrets' instead"  #-}

-- | A list of @ulimits@ to set in the container. This parameter maps to @Ulimits@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--ulimit@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- /Note:/ Consider using 'ulimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpUlimits :: Lens.Lens' ContainerProperties (Core.Maybe [Types.Ulimit])
cpUlimits = Lens.field @"ulimits"
{-# INLINEABLE cpUlimits #-}
{-# DEPRECATED ulimits "Use generic-lens or generic-optics with 'ulimits' instead"  #-}

-- | The user name to use inside the container. This parameter maps to @User@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--user@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpUser :: Lens.Lens' ContainerProperties (Core.Maybe Core.Text)
cpUser = Lens.field @"user"
{-# INLINEABLE cpUser #-}
{-# DEPRECATED user "Use generic-lens or generic-optics with 'user' instead"  #-}

-- | The number of vCPUs reserved for the container. This parameter maps to @CpuShares@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--cpu-shares@ option to <https://docs.docker.com/engine/reference/run/ docker run> . Each vCPU is equivalent to 1,024 CPU shares. You must specify at least one vCPU. This is required but can be specified in several places for multi-node parallel (MNP) jobs; it must be specified for each node at least once.
--
-- /Note:/ Consider using 'vcpus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpVcpus :: Lens.Lens' ContainerProperties (Core.Maybe Core.Int)
cpVcpus = Lens.field @"vcpus"
{-# INLINEABLE cpVcpus #-}
{-# DEPRECATED vcpus "Use generic-lens or generic-optics with 'vcpus' instead"  #-}

-- | A list of data volumes used in a job.
--
-- /Note:/ Consider using 'volumes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpVolumes :: Lens.Lens' ContainerProperties (Core.Maybe [Types.Volume])
cpVolumes = Lens.field @"volumes"
{-# INLINEABLE cpVolumes #-}
{-# DEPRECATED volumes "Use generic-lens or generic-optics with 'volumes' instead"  #-}

instance Core.FromJSON ContainerProperties where
        toJSON ContainerProperties{..}
          = Core.object
              (Core.catMaybes
                 [("command" Core..=) Core.<$> command,
                  ("environment" Core..=) Core.<$> environment,
                  ("executionRoleArn" Core..=) Core.<$> executionRoleArn,
                  ("image" Core..=) Core.<$> image,
                  ("instanceType" Core..=) Core.<$> instanceType,
                  ("jobRoleArn" Core..=) Core.<$> jobRoleArn,
                  ("linuxParameters" Core..=) Core.<$> linuxParameters,
                  ("logConfiguration" Core..=) Core.<$> logConfiguration,
                  ("memory" Core..=) Core.<$> memory,
                  ("mountPoints" Core..=) Core.<$> mountPoints,
                  ("privileged" Core..=) Core.<$> privileged,
                  ("readonlyRootFilesystem" Core..=) Core.<$> readonlyRootFilesystem,
                  ("resourceRequirements" Core..=) Core.<$> resourceRequirements,
                  ("secrets" Core..=) Core.<$> secrets,
                  ("ulimits" Core..=) Core.<$> ulimits,
                  ("user" Core..=) Core.<$> user, ("vcpus" Core..=) Core.<$> vcpus,
                  ("volumes" Core..=) Core.<$> volumes])

instance Core.FromJSON ContainerProperties where
        parseJSON
          = Core.withObject "ContainerProperties" Core.$
              \ x ->
                ContainerProperties' Core.<$>
                  (x Core..:? "command") Core.<*> x Core..:? "environment" Core.<*>
                    x Core..:? "executionRoleArn"
                    Core.<*> x Core..:? "image"
                    Core.<*> x Core..:? "instanceType"
                    Core.<*> x Core..:? "jobRoleArn"
                    Core.<*> x Core..:? "linuxParameters"
                    Core.<*> x Core..:? "logConfiguration"
                    Core.<*> x Core..:? "memory"
                    Core.<*> x Core..:? "mountPoints"
                    Core.<*> x Core..:? "privileged"
                    Core.<*> x Core..:? "readonlyRootFilesystem"
                    Core.<*> x Core..:? "resourceRequirements"
                    Core.<*> x Core..:? "secrets"
                    Core.<*> x Core..:? "ulimits"
                    Core.<*> x Core..:? "user"
                    Core.<*> x Core..:? "vcpus"
                    Core.<*> x Core..:? "volumes"
