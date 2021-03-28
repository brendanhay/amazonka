{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ContainerDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.ContainerDefinition
  ( ContainerDefinition (..)
  -- * Smart constructor
  , mkContainerDefinition
  -- * Lenses
  , cdCommand
  , cdCpu
  , cdDependsOn
  , cdDisableNetworking
  , cdDnsSearchDomains
  , cdDnsServers
  , cdDockerLabels
  , cdDockerSecurityOptions
  , cdEntryPoint
  , cdEnvironment
  , cdEnvironmentFiles
  , cdEssential
  , cdExtraHosts
  , cdFirelensConfiguration
  , cdHealthCheck
  , cdHostname
  , cdImage
  , cdInteractive
  , cdLinks
  , cdLinuxParameters
  , cdLogConfiguration
  , cdMemory
  , cdMemoryReservation
  , cdMountPoints
  , cdName
  , cdPortMappings
  , cdPrivileged
  , cdPseudoTerminal
  , cdReadonlyRootFilesystem
  , cdRepositoryCredentials
  , cdResourceRequirements
  , cdSecrets
  , cdStartTimeout
  , cdStopTimeout
  , cdSystemControls
  , cdUlimits
  , cdUser
  , cdVolumesFrom
  , cdWorkingDirectory
  ) where

import qualified Network.AWS.ECS.Types.ContainerDependency as Types
import qualified Network.AWS.ECS.Types.EnvironmentFile as Types
import qualified Network.AWS.ECS.Types.FirelensConfiguration as Types
import qualified Network.AWS.ECS.Types.HealthCheck as Types
import qualified Network.AWS.ECS.Types.HostEntry as Types
import qualified Network.AWS.ECS.Types.KeyValuePair as Types
import qualified Network.AWS.ECS.Types.LinuxParameters as Types
import qualified Network.AWS.ECS.Types.LogConfiguration as Types
import qualified Network.AWS.ECS.Types.MountPoint as Types
import qualified Network.AWS.ECS.Types.PortMapping as Types
import qualified Network.AWS.ECS.Types.RepositoryCredentials as Types
import qualified Network.AWS.ECS.Types.ResourceRequirement as Types
import qualified Network.AWS.ECS.Types.Secret as Types
import qualified Network.AWS.ECS.Types.SystemControl as Types
import qualified Network.AWS.ECS.Types.Ulimit as Types
import qualified Network.AWS.ECS.Types.VolumeFrom as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Container definitions are used in task definitions to describe the different containers that are launched as part of a task.
--
-- /See:/ 'mkContainerDefinition' smart constructor.
data ContainerDefinition = ContainerDefinition'
  { command :: Core.Maybe [Core.Text]
    -- ^ The command that is passed to the container. This parameter maps to @Cmd@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @COMMAND@ parameter to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . For more information, see <https://docs.docker.com/engine/reference/builder/#cmd https://docs.docker.com/engine/reference/builder/#cmd> . If there are multiple arguments, each argument should be a separated string in the array.
  , cpu :: Core.Maybe Core.Int
    -- ^ The number of @cpu@ units reserved for the container. This parameter maps to @CpuShares@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--cpu-shares@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- This field is optional for tasks using the Fargate launch type, and the only requirement is that the total amount of CPU reserved for all containers within a task be lower than the task-level @cpu@ value.
-- Linux containers share unallocated CPU units with other containers on the container instance with the same ratio as their allocated amount. For example, if you run a single-container task on a single-core instance type with 512 CPU units specified for that container, and that is the only task running on the container instance, that container could use the full 1,024 CPU unit share at any given time. However, if you launched another copy of the same task on that container instance, each task would be guaranteed a minimum of 512 CPU units when needed, and each container could float to higher CPU usage if the other container was not using it, but if both tasks were 100% active all of the time, they would be limited to 512 CPU units.
-- On Linux container instances, the Docker daemon on the container instance uses the CPU value to calculate the relative CPU share ratios for running containers. For more information, see <https://docs.docker.com/engine/reference/run/#cpu-share-constraint CPU share constraint> in the Docker documentation. The minimum valid CPU share value that the Linux kernel allows is 2. However, the CPU parameter is not required, and you can use CPU values below 2 in your container definitions. For CPU values below 2 (including null), the behavior varies based on your Amazon ECS container agent version:
--
--     * __Agent versions less than or equal to 1.1.0:__ Null and zero CPU values are passed to Docker as 0, which Docker then converts to 1,024 CPU shares. CPU values of 1 are passed to Docker as 1, which the Linux kernel converts to two CPU shares.
--
--
--     * __Agent versions greater than or equal to 1.2.0:__ Null, zero, and CPU values of 1 are passed to Docker as 2.
--
--
-- On Windows container instances, the CPU limit is enforced as an absolute limit, or a quota. Windows containers only have access to the specified amount of CPU that is described in the task definition. A null or zero CPU value is passed to Docker as @0@ , which Windows interprets as 1% of one CPU.
  , dependsOn :: Core.Maybe [Types.ContainerDependency]
    -- ^ The dependencies defined for container startup and shutdown. A container can contain multiple dependencies. When a dependency is defined for container startup, for container shutdown it is reversed.
--
-- For tasks using the EC2 launch type, the container instances require at least version 1.26.0 of the container agent to enable container dependencies. However, we recommend using the latest container agent version. For information about checking your agent version and updating to the latest version, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent> in the /Amazon Elastic Container Service Developer Guide/ . If you are using an Amazon ECS-optimized Linux AMI, your instance needs at least version 1.26.0-1 of the @ecs-init@ package. If your container instances are launched from version @20190301@ or later, then they contain the required versions of the container agent and @ecs-init@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI> in the /Amazon Elastic Container Service Developer Guide/ .
-- For tasks using the Fargate launch type, the task or service requires platform version @1.3.0@ or later.
  , disableNetworking :: Core.Maybe Core.Bool
    -- ^ When this parameter is true, networking is disabled within the container. This parameter maps to @NetworkDisabled@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> .
  , dnsSearchDomains :: Core.Maybe [Core.Text]
    -- ^ A list of DNS search domains that are presented to the container. This parameter maps to @DnsSearch@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--dns-search@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
  , dnsServers :: Core.Maybe [Core.Text]
    -- ^ A list of DNS servers that are presented to the container. This parameter maps to @Dns@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--dns@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
  , dockerLabels :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A key/value map of labels to add to the container. This parameter maps to @Labels@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--label@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@ 
  , dockerSecurityOptions :: Core.Maybe [Core.Text]
    -- ^ A list of strings to provide custom labels for SELinux and AppArmor multi-level security systems. This field is not valid for containers in tasks using the Fargate launch type.
--
-- With Windows containers, this parameter can be used to reference a credential spec file when configuring a container for Active Directory authentication. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/windows-gmsa.html Using gMSAs for Windows Containers> in the /Amazon Elastic Container Service Developer Guide/ .
-- This parameter maps to @SecurityOpt@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--security-opt@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
-- For more information about valid values, see <https://docs.docker.com/engine/reference/run/#security-configuration Docker Run Security Configuration> . 
-- Valid values: "no-new-privileges" | "apparmor:PROFILE" | "label:value" | "credentialspec:CredentialSpecFilePath"
  , entryPoint :: Core.Maybe [Core.Text]
    -- ^ /Important:/ Early versions of the Amazon ECS container agent do not properly handle @entryPoint@ parameters. If you have problems using @entryPoint@ , update your container agent or enter your commands and arguments as @command@ array items instead.
--
-- The entry point that is passed to the container. This parameter maps to @Entrypoint@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--entrypoint@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . For more information, see <https://docs.docker.com/engine/reference/builder/#entrypoint https://docs.docker.com/engine/reference/builder/#entrypoint> .
  , environment :: Core.Maybe [Types.KeyValuePair]
    -- ^ The environment variables to pass to a container. This parameter maps to @Env@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--env@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- /Important:/ We do not recommend using plaintext environment variables for sensitive information, such as credential data.
  , environmentFiles :: Core.Maybe [Types.EnvironmentFile]
    -- ^ A list of files containing the environment variables to pass to a container. This parameter maps to the @--env-file@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- You can specify up to ten environment files. The file must have a @.env@ file extension. Each line in an environment file should contain an environment variable in @VARIABLE=VALUE@ format. Lines beginning with @#@ are treated as comments and are ignored. For more information on the environment variable file syntax, see <https://docs.docker.com/compose/env-file/ Declare default environment variables in file> .
-- If there are environment variables specified using the @environment@ parameter in a container definition, they take precedence over the variables contained within an environment file. If multiple environment files are specified that contain the same variable, they are processed from the top down. It is recommended to use unique variable names. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/taskdef-envfiles.html Specifying Environment Variables> in the /Amazon Elastic Container Service Developer Guide/ .
-- This field is not valid for containers in tasks using the Fargate launch type.
  , essential :: Core.Maybe Core.Bool
    -- ^ If the @essential@ parameter of a container is marked as @true@ , and that container fails or stops for any reason, all other containers that are part of the task are stopped. If the @essential@ parameter of a container is marked as @false@ , then its failure does not affect the rest of the containers in a task. If this parameter is omitted, a container is assumed to be essential.
--
-- All tasks must have at least one essential container. If you have an application that is composed of multiple containers, you should group containers that are used for a common purpose into components, and separate the different components into multiple task definitions. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/application_architecture.html Application Architecture> in the /Amazon Elastic Container Service Developer Guide/ .
  , extraHosts :: Core.Maybe [Types.HostEntry]
    -- ^ A list of hostnames and IP address mappings to append to the @/etc/hosts@ file on the container. This parameter maps to @ExtraHosts@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--add-host@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
  , firelensConfiguration :: Core.Maybe Types.FirelensConfiguration
    -- ^ The FireLens configuration for the container. This is used to specify and configure a log router for container logs. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html Custom Log Routing> in the /Amazon Elastic Container Service Developer Guide/ .
  , healthCheck :: Core.Maybe Types.HealthCheck
    -- ^ The container health check command and associated configuration parameters for the container. This parameter maps to @HealthCheck@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @HEALTHCHECK@ parameter of <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
  , hostname :: Core.Maybe Core.Text
    -- ^ The hostname to use for your container. This parameter maps to @Hostname@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--hostname@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
  , image :: Core.Maybe Core.Text
    -- ^ The image used to start a container. This string is passed directly to the Docker daemon. Images in the Docker Hub registry are available by default. Other repositories are specified with either @/repository-url/ //image/ :/tag/ @ or @/repository-url/ //image/ @/digest/ @ . Up to 255 letters (uppercase and lowercase), numbers, hyphens, underscores, colons, periods, forward slashes, and number signs are allowed. This parameter maps to @Image@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @IMAGE@ parameter of <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
--
--     * When a new task starts, the Amazon ECS container agent pulls the latest version of the specified image and tag for the container to use. However, subsequent updates to a repository image are not propagated to already running tasks.
--
--
--     * Images in Amazon ECR repositories can be specified by either using the full @registry/repository:tag@ or @registry/repository@digest@ . For example, @012345678910.dkr.ecr.<region-name>.amazonaws.com/<repository-name>:latest@ or @012345678910.dkr.ecr.<region-name>.amazonaws.com/<repository-name>@sha256:94afd1f2e64d908bc90dbca0035a5b567EXAMPLE@ . 
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
  , interactive :: Core.Maybe Core.Bool
    -- ^ When this parameter is @true@ , this allows you to deploy containerized applications that require @stdin@ or a @tty@ to be allocated. This parameter maps to @OpenStdin@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--interactive@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
  , links :: Core.Maybe [Core.Text]
    -- ^ The @links@ parameter allows containers to communicate with each other without the need for port mappings. This parameter is only supported if the network mode of a task definition is @bridge@ . The @name:internalName@ construct is analogous to @name:alias@ in Docker links. Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed. For more information about linking Docker containers, go to <https://docs.docker.com/network/links/ Legacy container links> in the Docker documentation. This parameter maps to @Links@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--link@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- /Important:/ Containers that are collocated on a single container instance may be able to communicate with each other without requiring links or host port mappings. Network isolation is achieved on the container instance using security groups and VPC settings.
  , linuxParameters :: Core.Maybe Types.LinuxParameters
    -- ^ Linux-specific modifications that are applied to the container, such as Linux kernel capabilities. For more information see 'KernelCapabilities' .
  , logConfiguration :: Core.Maybe Types.LogConfiguration
    -- ^ The log configuration specification for the container.
--
-- This parameter maps to @LogConfig@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--log-driver@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . By default, containers use the same logging driver that the Docker daemon uses. However the container may use a different logging driver than the Docker daemon by specifying a log driver with this parameter in the container definition. To use a different logging driver for a container, the log system must be configured properly on the container instance (or on a different log server for remote logging options). For more information on the options for different supported log drivers, see <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers> in the Docker documentation.
-- This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@ 
  , memory :: Core.Maybe Core.Int
    -- ^ The amount (in MiB) of memory to present to the container. If your container attempts to exceed the memory specified here, the container is killed. The total amount of memory reserved for all containers within a task must be lower than the task @memory@ value, if one is specified. This parameter maps to @Memory@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--memory@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- If using the Fargate launch type, this parameter is optional.
-- If using the EC2 launch type, you must specify either a task-level memory value or a container-level memory value. If you specify both a container-level @memory@ and @memoryReservation@ value, @memory@ must be greater than @memoryReservation@ . If you specify @memoryReservation@ , then that value is subtracted from the available memory resources for the container instance on which the container is placed. Otherwise, the value of @memory@ is used.
-- The Docker daemon reserves a minimum of 4 MiB of memory for a container, so you should not specify fewer than 4 MiB of memory for your containers.
  , memoryReservation :: Core.Maybe Core.Int
    -- ^ The soft limit (in MiB) of memory to reserve for the container. When system memory is under heavy contention, Docker attempts to keep the container memory to this soft limit. However, your container can consume more memory when it needs to, up to either the hard limit specified with the @memory@ parameter (if applicable), or all of the available memory on the container instance, whichever comes first. This parameter maps to @MemoryReservation@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--memory-reservation@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- If a task-level memory value is not specified, you must specify a non-zero integer for one or both of @memory@ or @memoryReservation@ in a container definition. If you specify both, @memory@ must be greater than @memoryReservation@ . If you specify @memoryReservation@ , then that value is subtracted from the available memory resources for the container instance on which the container is placed. Otherwise, the value of @memory@ is used.
-- For example, if your container normally uses 128 MiB of memory, but occasionally bursts to 256 MiB of memory for short periods of time, you can set a @memoryReservation@ of 128 MiB, and a @memory@ hard limit of 300 MiB. This configuration would allow the container to only reserve 128 MiB of memory from the remaining resources on the container instance, but also allow the container to consume more memory resources when needed.
-- The Docker daemon reserves a minimum of 4 MiB of memory for a container, so you should not specify fewer than 4 MiB of memory for your containers. 
  , mountPoints :: Core.Maybe [Types.MountPoint]
    -- ^ The mount points for data volumes in your container.
--
-- This parameter maps to @Volumes@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--volume@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
-- Windows containers can mount whole directories on the same drive as @> env:ProgramData@ . Windows containers cannot mount directories on a different drive, and mount point cannot be across drives.
  , name :: Core.Maybe Core.Text
    -- ^ The name of a container. If you are linking multiple containers together in a task definition, the @name@ of one container can be entered in the @links@ of another container to connect the containers. Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed. This parameter maps to @name@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--name@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . 
  , portMappings :: Core.Maybe [Types.PortMapping]
    -- ^ The list of port mappings for the container. Port mappings allow containers to access ports on the host container instance to send or receive traffic.
--
-- For task definitions that use the @awsvpc@ network mode, you should only specify the @containerPort@ . The @hostPort@ can be left blank or it must be the same value as the @containerPort@ .
-- Port mappings on Windows use the @NetNAT@ gateway address rather than @localhost@ . There is no loopback for port mappings on Windows, so you cannot access a container's mapped port from the host itself. 
-- This parameter maps to @PortBindings@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--publish@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . If the network mode of a task definition is set to @none@ , then you can't specify port mappings. If the network mode of a task definition is set to @host@ , then host ports must either be undefined or they must match the container port in the port mapping.
  , privileged :: Core.Maybe Core.Bool
    -- ^ When this parameter is true, the container is given elevated privileges on the host container instance (similar to the @root@ user). This parameter maps to @Privileged@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--privileged@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
  , pseudoTerminal :: Core.Maybe Core.Bool
    -- ^ When this parameter is @true@ , a TTY is allocated. This parameter maps to @Tty@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--tty@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
  , readonlyRootFilesystem :: Core.Maybe Core.Bool
    -- ^ When this parameter is true, the container is given read-only access to its root file system. This parameter maps to @ReadonlyRootfs@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--read-only@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
  , repositoryCredentials :: Core.Maybe Types.RepositoryCredentials
    -- ^ The private repository authentication credentials to use.
  , resourceRequirements :: Core.Maybe [Types.ResourceRequirement]
    -- ^ The type and amount of a resource to assign to a container. The only supported resource is a GPU.
  , secrets :: Core.Maybe [Types.Secret]
    -- ^ The secrets to pass to the container. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html Specifying Sensitive Data> in the /Amazon Elastic Container Service Developer Guide/ .
  , startTimeout :: Core.Maybe Core.Int
    -- ^ Time duration (in seconds) to wait before giving up on resolving dependencies for a container. For example, you specify two containers in a task definition with containerA having a dependency on containerB reaching a @COMPLETE@ , @SUCCESS@ , or @HEALTHY@ status. If a @startTimeout@ value is specified for containerB and it does not reach the desired status within that time then containerA will give up and not start. This results in the task transitioning to a @STOPPED@ state.
--
-- For tasks using the Fargate launch type, this parameter requires that the task or service uses platform version 1.3.0 or later.
-- For tasks using the EC2 launch type, your container instances require at least version @1.26.0@ of the container agent to enable a container start timeout value. However, we recommend using the latest container agent version. For information about checking your agent version and updating to the latest version, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent> in the /Amazon Elastic Container Service Developer Guide/ . If you are using an Amazon ECS-optimized Linux AMI, your instance needs at least version @1.26.0-1@ of the @ecs-init@ package. If your container instances are launched from version @20190301@ or later, then they contain the required versions of the container agent and @ecs-init@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI> in the /Amazon Elastic Container Service Developer Guide/ .
  , stopTimeout :: Core.Maybe Core.Int
    -- ^ Time duration (in seconds) to wait before the container is forcefully killed if it doesn't exit normally on its own.
--
-- For tasks using the Fargate launch type, the task or service requires platform version 1.3.0 or later. The max stop timeout value is 120 seconds and if the parameter is not specified, the default value of 30 seconds is used.
-- For tasks using the EC2 launch type, if the @stopTimeout@ parameter is not specified, the value set for the Amazon ECS container agent configuration variable @ECS_CONTAINER_STOP_TIMEOUT@ is used by default. If neither the @stopTimeout@ parameter or the @ECS_CONTAINER_STOP_TIMEOUT@ agent configuration variable are set, then the default values of 30 seconds for Linux containers and 30 seconds on Windows containers are used. Your container instances require at least version 1.26.0 of the container agent to enable a container stop timeout value. However, we recommend using the latest container agent version. For information about checking your agent version and updating to the latest version, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent> in the /Amazon Elastic Container Service Developer Guide/ . If you are using an Amazon ECS-optimized Linux AMI, your instance needs at least version 1.26.0-1 of the @ecs-init@ package. If your container instances are launched from version @20190301@ or later, then they contain the required versions of the container agent and @ecs-init@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI> in the /Amazon Elastic Container Service Developer Guide/ .
  , systemControls :: Core.Maybe [Types.SystemControl]
    -- ^ A list of namespaced kernel parameters to set in the container. This parameter maps to @Sysctls@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--sysctl@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
  , ulimits :: Core.Maybe [Types.Ulimit]
    -- ^ A list of @ulimits@ to set in the container. If a ulimit value is specified in a task definition, it will override the default values set by Docker. This parameter maps to @Ulimits@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--ulimit@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . Valid naming values are displayed in the 'Ulimit' data type. This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@ 
  , user :: Core.Maybe Core.Text
    -- ^ The user to use inside the container. This parameter maps to @User@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--user@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- /Important:/ When running tasks using the @host@ network mode, you should not run containers using the root user (UID 0). It is considered best practice to use a non-root user.
-- You can specify the @user@ using the following formats. If specifying a UID or GID, you must specify it as a positive integer.
--
--     * @user@ 
--
--
--     * @user:group@ 
--
--
--     * @uid@ 
--
--
--     * @uid:gid@ 
--
--
--     * @user:gid@ 
--
--
--     * @uid:group@ 
--
--
  , volumesFrom :: Core.Maybe [Types.VolumeFrom]
    -- ^ Data volumes to mount from another container. This parameter maps to @VolumesFrom@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--volumes-from@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
  , workingDirectory :: Core.Maybe Core.Text
    -- ^ The working directory in which to run commands inside the container. This parameter maps to @WorkingDir@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--workdir@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContainerDefinition' value with any optional fields omitted.
mkContainerDefinition
    :: ContainerDefinition
mkContainerDefinition
  = ContainerDefinition'{command = Core.Nothing, cpu = Core.Nothing,
                         dependsOn = Core.Nothing, disableNetworking = Core.Nothing,
                         dnsSearchDomains = Core.Nothing, dnsServers = Core.Nothing,
                         dockerLabels = Core.Nothing, dockerSecurityOptions = Core.Nothing,
                         entryPoint = Core.Nothing, environment = Core.Nothing,
                         environmentFiles = Core.Nothing, essential = Core.Nothing,
                         extraHosts = Core.Nothing, firelensConfiguration = Core.Nothing,
                         healthCheck = Core.Nothing, hostname = Core.Nothing,
                         image = Core.Nothing, interactive = Core.Nothing,
                         links = Core.Nothing, linuxParameters = Core.Nothing,
                         logConfiguration = Core.Nothing, memory = Core.Nothing,
                         memoryReservation = Core.Nothing, mountPoints = Core.Nothing,
                         name = Core.Nothing, portMappings = Core.Nothing,
                         privileged = Core.Nothing, pseudoTerminal = Core.Nothing,
                         readonlyRootFilesystem = Core.Nothing,
                         repositoryCredentials = Core.Nothing,
                         resourceRequirements = Core.Nothing, secrets = Core.Nothing,
                         startTimeout = Core.Nothing, stopTimeout = Core.Nothing,
                         systemControls = Core.Nothing, ulimits = Core.Nothing,
                         user = Core.Nothing, volumesFrom = Core.Nothing,
                         workingDirectory = Core.Nothing}

-- | The command that is passed to the container. This parameter maps to @Cmd@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @COMMAND@ parameter to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . For more information, see <https://docs.docker.com/engine/reference/builder/#cmd https://docs.docker.com/engine/reference/builder/#cmd> . If there are multiple arguments, each argument should be a separated string in the array.
--
-- /Note:/ Consider using 'command' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCommand :: Lens.Lens' ContainerDefinition (Core.Maybe [Core.Text])
cdCommand = Lens.field @"command"
{-# INLINEABLE cdCommand #-}
{-# DEPRECATED command "Use generic-lens or generic-optics with 'command' instead"  #-}

-- | The number of @cpu@ units reserved for the container. This parameter maps to @CpuShares@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--cpu-shares@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- This field is optional for tasks using the Fargate launch type, and the only requirement is that the total amount of CPU reserved for all containers within a task be lower than the task-level @cpu@ value.
-- Linux containers share unallocated CPU units with other containers on the container instance with the same ratio as their allocated amount. For example, if you run a single-container task on a single-core instance type with 512 CPU units specified for that container, and that is the only task running on the container instance, that container could use the full 1,024 CPU unit share at any given time. However, if you launched another copy of the same task on that container instance, each task would be guaranteed a minimum of 512 CPU units when needed, and each container could float to higher CPU usage if the other container was not using it, but if both tasks were 100% active all of the time, they would be limited to 512 CPU units.
-- On Linux container instances, the Docker daemon on the container instance uses the CPU value to calculate the relative CPU share ratios for running containers. For more information, see <https://docs.docker.com/engine/reference/run/#cpu-share-constraint CPU share constraint> in the Docker documentation. The minimum valid CPU share value that the Linux kernel allows is 2. However, the CPU parameter is not required, and you can use CPU values below 2 in your container definitions. For CPU values below 2 (including null), the behavior varies based on your Amazon ECS container agent version:
--
--     * __Agent versions less than or equal to 1.1.0:__ Null and zero CPU values are passed to Docker as 0, which Docker then converts to 1,024 CPU shares. CPU values of 1 are passed to Docker as 1, which the Linux kernel converts to two CPU shares.
--
--
--     * __Agent versions greater than or equal to 1.2.0:__ Null, zero, and CPU values of 1 are passed to Docker as 2.
--
--
-- On Windows container instances, the CPU limit is enforced as an absolute limit, or a quota. Windows containers only have access to the specified amount of CPU that is described in the task definition. A null or zero CPU value is passed to Docker as @0@ , which Windows interprets as 1% of one CPU.
--
-- /Note:/ Consider using 'cpu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCpu :: Lens.Lens' ContainerDefinition (Core.Maybe Core.Int)
cdCpu = Lens.field @"cpu"
{-# INLINEABLE cdCpu #-}
{-# DEPRECATED cpu "Use generic-lens or generic-optics with 'cpu' instead"  #-}

-- | The dependencies defined for container startup and shutdown. A container can contain multiple dependencies. When a dependency is defined for container startup, for container shutdown it is reversed.
--
-- For tasks using the EC2 launch type, the container instances require at least version 1.26.0 of the container agent to enable container dependencies. However, we recommend using the latest container agent version. For information about checking your agent version and updating to the latest version, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent> in the /Amazon Elastic Container Service Developer Guide/ . If you are using an Amazon ECS-optimized Linux AMI, your instance needs at least version 1.26.0-1 of the @ecs-init@ package. If your container instances are launched from version @20190301@ or later, then they contain the required versions of the container agent and @ecs-init@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI> in the /Amazon Elastic Container Service Developer Guide/ .
-- For tasks using the Fargate launch type, the task or service requires platform version @1.3.0@ or later.
--
-- /Note:/ Consider using 'dependsOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDependsOn :: Lens.Lens' ContainerDefinition (Core.Maybe [Types.ContainerDependency])
cdDependsOn = Lens.field @"dependsOn"
{-# INLINEABLE cdDependsOn #-}
{-# DEPRECATED dependsOn "Use generic-lens or generic-optics with 'dependsOn' instead"  #-}

-- | When this parameter is true, networking is disabled within the container. This parameter maps to @NetworkDisabled@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> .
--
-- /Note:/ Consider using 'disableNetworking' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDisableNetworking :: Lens.Lens' ContainerDefinition (Core.Maybe Core.Bool)
cdDisableNetworking = Lens.field @"disableNetworking"
{-# INLINEABLE cdDisableNetworking #-}
{-# DEPRECATED disableNetworking "Use generic-lens or generic-optics with 'disableNetworking' instead"  #-}

-- | A list of DNS search domains that are presented to the container. This parameter maps to @DnsSearch@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--dns-search@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- /Note:/ Consider using 'dnsSearchDomains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDnsSearchDomains :: Lens.Lens' ContainerDefinition (Core.Maybe [Core.Text])
cdDnsSearchDomains = Lens.field @"dnsSearchDomains"
{-# INLINEABLE cdDnsSearchDomains #-}
{-# DEPRECATED dnsSearchDomains "Use generic-lens or generic-optics with 'dnsSearchDomains' instead"  #-}

-- | A list of DNS servers that are presented to the container. This parameter maps to @Dns@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--dns@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- /Note:/ Consider using 'dnsServers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDnsServers :: Lens.Lens' ContainerDefinition (Core.Maybe [Core.Text])
cdDnsServers = Lens.field @"dnsServers"
{-# INLINEABLE cdDnsServers #-}
{-# DEPRECATED dnsServers "Use generic-lens or generic-optics with 'dnsServers' instead"  #-}

-- | A key/value map of labels to add to the container. This parameter maps to @Labels@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--label@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@ 
--
-- /Note:/ Consider using 'dockerLabels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDockerLabels :: Lens.Lens' ContainerDefinition (Core.Maybe (Core.HashMap Core.Text Core.Text))
cdDockerLabels = Lens.field @"dockerLabels"
{-# INLINEABLE cdDockerLabels #-}
{-# DEPRECATED dockerLabels "Use generic-lens or generic-optics with 'dockerLabels' instead"  #-}

-- | A list of strings to provide custom labels for SELinux and AppArmor multi-level security systems. This field is not valid for containers in tasks using the Fargate launch type.
--
-- With Windows containers, this parameter can be used to reference a credential spec file when configuring a container for Active Directory authentication. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/windows-gmsa.html Using gMSAs for Windows Containers> in the /Amazon Elastic Container Service Developer Guide/ .
-- This parameter maps to @SecurityOpt@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--security-opt@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
-- For more information about valid values, see <https://docs.docker.com/engine/reference/run/#security-configuration Docker Run Security Configuration> . 
-- Valid values: "no-new-privileges" | "apparmor:PROFILE" | "label:value" | "credentialspec:CredentialSpecFilePath"
--
-- /Note:/ Consider using 'dockerSecurityOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDockerSecurityOptions :: Lens.Lens' ContainerDefinition (Core.Maybe [Core.Text])
cdDockerSecurityOptions = Lens.field @"dockerSecurityOptions"
{-# INLINEABLE cdDockerSecurityOptions #-}
{-# DEPRECATED dockerSecurityOptions "Use generic-lens or generic-optics with 'dockerSecurityOptions' instead"  #-}

-- | /Important:/ Early versions of the Amazon ECS container agent do not properly handle @entryPoint@ parameters. If you have problems using @entryPoint@ , update your container agent or enter your commands and arguments as @command@ array items instead.
--
-- The entry point that is passed to the container. This parameter maps to @Entrypoint@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--entrypoint@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . For more information, see <https://docs.docker.com/engine/reference/builder/#entrypoint https://docs.docker.com/engine/reference/builder/#entrypoint> .
--
-- /Note:/ Consider using 'entryPoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdEntryPoint :: Lens.Lens' ContainerDefinition (Core.Maybe [Core.Text])
cdEntryPoint = Lens.field @"entryPoint"
{-# INLINEABLE cdEntryPoint #-}
{-# DEPRECATED entryPoint "Use generic-lens or generic-optics with 'entryPoint' instead"  #-}

-- | The environment variables to pass to a container. This parameter maps to @Env@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--env@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- /Important:/ We do not recommend using plaintext environment variables for sensitive information, such as credential data.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdEnvironment :: Lens.Lens' ContainerDefinition (Core.Maybe [Types.KeyValuePair])
cdEnvironment = Lens.field @"environment"
{-# INLINEABLE cdEnvironment #-}
{-# DEPRECATED environment "Use generic-lens or generic-optics with 'environment' instead"  #-}

-- | A list of files containing the environment variables to pass to a container. This parameter maps to the @--env-file@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- You can specify up to ten environment files. The file must have a @.env@ file extension. Each line in an environment file should contain an environment variable in @VARIABLE=VALUE@ format. Lines beginning with @#@ are treated as comments and are ignored. For more information on the environment variable file syntax, see <https://docs.docker.com/compose/env-file/ Declare default environment variables in file> .
-- If there are environment variables specified using the @environment@ parameter in a container definition, they take precedence over the variables contained within an environment file. If multiple environment files are specified that contain the same variable, they are processed from the top down. It is recommended to use unique variable names. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/taskdef-envfiles.html Specifying Environment Variables> in the /Amazon Elastic Container Service Developer Guide/ .
-- This field is not valid for containers in tasks using the Fargate launch type.
--
-- /Note:/ Consider using 'environmentFiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdEnvironmentFiles :: Lens.Lens' ContainerDefinition (Core.Maybe [Types.EnvironmentFile])
cdEnvironmentFiles = Lens.field @"environmentFiles"
{-# INLINEABLE cdEnvironmentFiles #-}
{-# DEPRECATED environmentFiles "Use generic-lens or generic-optics with 'environmentFiles' instead"  #-}

-- | If the @essential@ parameter of a container is marked as @true@ , and that container fails or stops for any reason, all other containers that are part of the task are stopped. If the @essential@ parameter of a container is marked as @false@ , then its failure does not affect the rest of the containers in a task. If this parameter is omitted, a container is assumed to be essential.
--
-- All tasks must have at least one essential container. If you have an application that is composed of multiple containers, you should group containers that are used for a common purpose into components, and separate the different components into multiple task definitions. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/application_architecture.html Application Architecture> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'essential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdEssential :: Lens.Lens' ContainerDefinition (Core.Maybe Core.Bool)
cdEssential = Lens.field @"essential"
{-# INLINEABLE cdEssential #-}
{-# DEPRECATED essential "Use generic-lens or generic-optics with 'essential' instead"  #-}

-- | A list of hostnames and IP address mappings to append to the @/etc/hosts@ file on the container. This parameter maps to @ExtraHosts@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--add-host@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- /Note:/ Consider using 'extraHosts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdExtraHosts :: Lens.Lens' ContainerDefinition (Core.Maybe [Types.HostEntry])
cdExtraHosts = Lens.field @"extraHosts"
{-# INLINEABLE cdExtraHosts #-}
{-# DEPRECATED extraHosts "Use generic-lens or generic-optics with 'extraHosts' instead"  #-}

-- | The FireLens configuration for the container. This is used to specify and configure a log router for container logs. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html Custom Log Routing> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'firelensConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdFirelensConfiguration :: Lens.Lens' ContainerDefinition (Core.Maybe Types.FirelensConfiguration)
cdFirelensConfiguration = Lens.field @"firelensConfiguration"
{-# INLINEABLE cdFirelensConfiguration #-}
{-# DEPRECATED firelensConfiguration "Use generic-lens or generic-optics with 'firelensConfiguration' instead"  #-}

-- | The container health check command and associated configuration parameters for the container. This parameter maps to @HealthCheck@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @HEALTHCHECK@ parameter of <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- /Note:/ Consider using 'healthCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdHealthCheck :: Lens.Lens' ContainerDefinition (Core.Maybe Types.HealthCheck)
cdHealthCheck = Lens.field @"healthCheck"
{-# INLINEABLE cdHealthCheck #-}
{-# DEPRECATED healthCheck "Use generic-lens or generic-optics with 'healthCheck' instead"  #-}

-- | The hostname to use for your container. This parameter maps to @Hostname@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--hostname@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- /Note:/ Consider using 'hostname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdHostname :: Lens.Lens' ContainerDefinition (Core.Maybe Core.Text)
cdHostname = Lens.field @"hostname"
{-# INLINEABLE cdHostname #-}
{-# DEPRECATED hostname "Use generic-lens or generic-optics with 'hostname' instead"  #-}

-- | The image used to start a container. This string is passed directly to the Docker daemon. Images in the Docker Hub registry are available by default. Other repositories are specified with either @/repository-url/ //image/ :/tag/ @ or @/repository-url/ //image/ @/digest/ @ . Up to 255 letters (uppercase and lowercase), numbers, hyphens, underscores, colons, periods, forward slashes, and number signs are allowed. This parameter maps to @Image@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @IMAGE@ parameter of <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
--
--     * When a new task starts, the Amazon ECS container agent pulls the latest version of the specified image and tag for the container to use. However, subsequent updates to a repository image are not propagated to already running tasks.
--
--
--     * Images in Amazon ECR repositories can be specified by either using the full @registry/repository:tag@ or @registry/repository@digest@ . For example, @012345678910.dkr.ecr.<region-name>.amazonaws.com/<repository-name>:latest@ or @012345678910.dkr.ecr.<region-name>.amazonaws.com/<repository-name>@sha256:94afd1f2e64d908bc90dbca0035a5b567EXAMPLE@ . 
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
cdImage :: Lens.Lens' ContainerDefinition (Core.Maybe Core.Text)
cdImage = Lens.field @"image"
{-# INLINEABLE cdImage #-}
{-# DEPRECATED image "Use generic-lens or generic-optics with 'image' instead"  #-}

-- | When this parameter is @true@ , this allows you to deploy containerized applications that require @stdin@ or a @tty@ to be allocated. This parameter maps to @OpenStdin@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--interactive@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- /Note:/ Consider using 'interactive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdInteractive :: Lens.Lens' ContainerDefinition (Core.Maybe Core.Bool)
cdInteractive = Lens.field @"interactive"
{-# INLINEABLE cdInteractive #-}
{-# DEPRECATED interactive "Use generic-lens or generic-optics with 'interactive' instead"  #-}

-- | The @links@ parameter allows containers to communicate with each other without the need for port mappings. This parameter is only supported if the network mode of a task definition is @bridge@ . The @name:internalName@ construct is analogous to @name:alias@ in Docker links. Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed. For more information about linking Docker containers, go to <https://docs.docker.com/network/links/ Legacy container links> in the Docker documentation. This parameter maps to @Links@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--link@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- /Important:/ Containers that are collocated on a single container instance may be able to communicate with each other without requiring links or host port mappings. Network isolation is achieved on the container instance using security groups and VPC settings.
--
-- /Note:/ Consider using 'links' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdLinks :: Lens.Lens' ContainerDefinition (Core.Maybe [Core.Text])
cdLinks = Lens.field @"links"
{-# INLINEABLE cdLinks #-}
{-# DEPRECATED links "Use generic-lens or generic-optics with 'links' instead"  #-}

-- | Linux-specific modifications that are applied to the container, such as Linux kernel capabilities. For more information see 'KernelCapabilities' .
--
-- /Note:/ Consider using 'linuxParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdLinuxParameters :: Lens.Lens' ContainerDefinition (Core.Maybe Types.LinuxParameters)
cdLinuxParameters = Lens.field @"linuxParameters"
{-# INLINEABLE cdLinuxParameters #-}
{-# DEPRECATED linuxParameters "Use generic-lens or generic-optics with 'linuxParameters' instead"  #-}

-- | The log configuration specification for the container.
--
-- This parameter maps to @LogConfig@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--log-driver@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . By default, containers use the same logging driver that the Docker daemon uses. However the container may use a different logging driver than the Docker daemon by specifying a log driver with this parameter in the container definition. To use a different logging driver for a container, the log system must be configured properly on the container instance (or on a different log server for remote logging options). For more information on the options for different supported log drivers, see <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers> in the Docker documentation.
-- This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@ 
--
-- /Note:/ Consider using 'logConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdLogConfiguration :: Lens.Lens' ContainerDefinition (Core.Maybe Types.LogConfiguration)
cdLogConfiguration = Lens.field @"logConfiguration"
{-# INLINEABLE cdLogConfiguration #-}
{-# DEPRECATED logConfiguration "Use generic-lens or generic-optics with 'logConfiguration' instead"  #-}

-- | The amount (in MiB) of memory to present to the container. If your container attempts to exceed the memory specified here, the container is killed. The total amount of memory reserved for all containers within a task must be lower than the task @memory@ value, if one is specified. This parameter maps to @Memory@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--memory@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- If using the Fargate launch type, this parameter is optional.
-- If using the EC2 launch type, you must specify either a task-level memory value or a container-level memory value. If you specify both a container-level @memory@ and @memoryReservation@ value, @memory@ must be greater than @memoryReservation@ . If you specify @memoryReservation@ , then that value is subtracted from the available memory resources for the container instance on which the container is placed. Otherwise, the value of @memory@ is used.
-- The Docker daemon reserves a minimum of 4 MiB of memory for a container, so you should not specify fewer than 4 MiB of memory for your containers.
--
-- /Note:/ Consider using 'memory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdMemory :: Lens.Lens' ContainerDefinition (Core.Maybe Core.Int)
cdMemory = Lens.field @"memory"
{-# INLINEABLE cdMemory #-}
{-# DEPRECATED memory "Use generic-lens or generic-optics with 'memory' instead"  #-}

-- | The soft limit (in MiB) of memory to reserve for the container. When system memory is under heavy contention, Docker attempts to keep the container memory to this soft limit. However, your container can consume more memory when it needs to, up to either the hard limit specified with the @memory@ parameter (if applicable), or all of the available memory on the container instance, whichever comes first. This parameter maps to @MemoryReservation@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--memory-reservation@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- If a task-level memory value is not specified, you must specify a non-zero integer for one or both of @memory@ or @memoryReservation@ in a container definition. If you specify both, @memory@ must be greater than @memoryReservation@ . If you specify @memoryReservation@ , then that value is subtracted from the available memory resources for the container instance on which the container is placed. Otherwise, the value of @memory@ is used.
-- For example, if your container normally uses 128 MiB of memory, but occasionally bursts to 256 MiB of memory for short periods of time, you can set a @memoryReservation@ of 128 MiB, and a @memory@ hard limit of 300 MiB. This configuration would allow the container to only reserve 128 MiB of memory from the remaining resources on the container instance, but also allow the container to consume more memory resources when needed.
-- The Docker daemon reserves a minimum of 4 MiB of memory for a container, so you should not specify fewer than 4 MiB of memory for your containers. 
--
-- /Note:/ Consider using 'memoryReservation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdMemoryReservation :: Lens.Lens' ContainerDefinition (Core.Maybe Core.Int)
cdMemoryReservation = Lens.field @"memoryReservation"
{-# INLINEABLE cdMemoryReservation #-}
{-# DEPRECATED memoryReservation "Use generic-lens or generic-optics with 'memoryReservation' instead"  #-}

-- | The mount points for data volumes in your container.
--
-- This parameter maps to @Volumes@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--volume@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
-- Windows containers can mount whole directories on the same drive as @> env:ProgramData@ . Windows containers cannot mount directories on a different drive, and mount point cannot be across drives.
--
-- /Note:/ Consider using 'mountPoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdMountPoints :: Lens.Lens' ContainerDefinition (Core.Maybe [Types.MountPoint])
cdMountPoints = Lens.field @"mountPoints"
{-# INLINEABLE cdMountPoints #-}
{-# DEPRECATED mountPoints "Use generic-lens or generic-optics with 'mountPoints' instead"  #-}

-- | The name of a container. If you are linking multiple containers together in a task definition, the @name@ of one container can be entered in the @links@ of another container to connect the containers. Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed. This parameter maps to @name@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--name@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdName :: Lens.Lens' ContainerDefinition (Core.Maybe Core.Text)
cdName = Lens.field @"name"
{-# INLINEABLE cdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The list of port mappings for the container. Port mappings allow containers to access ports on the host container instance to send or receive traffic.
--
-- For task definitions that use the @awsvpc@ network mode, you should only specify the @containerPort@ . The @hostPort@ can be left blank or it must be the same value as the @containerPort@ .
-- Port mappings on Windows use the @NetNAT@ gateway address rather than @localhost@ . There is no loopback for port mappings on Windows, so you cannot access a container's mapped port from the host itself. 
-- This parameter maps to @PortBindings@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--publish@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . If the network mode of a task definition is set to @none@ , then you can't specify port mappings. If the network mode of a task definition is set to @host@ , then host ports must either be undefined or they must match the container port in the port mapping.
--
-- /Note:/ Consider using 'portMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdPortMappings :: Lens.Lens' ContainerDefinition (Core.Maybe [Types.PortMapping])
cdPortMappings = Lens.field @"portMappings"
{-# INLINEABLE cdPortMappings #-}
{-# DEPRECATED portMappings "Use generic-lens or generic-optics with 'portMappings' instead"  #-}

-- | When this parameter is true, the container is given elevated privileges on the host container instance (similar to the @root@ user). This parameter maps to @Privileged@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--privileged@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- /Note:/ Consider using 'privileged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdPrivileged :: Lens.Lens' ContainerDefinition (Core.Maybe Core.Bool)
cdPrivileged = Lens.field @"privileged"
{-# INLINEABLE cdPrivileged #-}
{-# DEPRECATED privileged "Use generic-lens or generic-optics with 'privileged' instead"  #-}

-- | When this parameter is @true@ , a TTY is allocated. This parameter maps to @Tty@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--tty@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- /Note:/ Consider using 'pseudoTerminal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdPseudoTerminal :: Lens.Lens' ContainerDefinition (Core.Maybe Core.Bool)
cdPseudoTerminal = Lens.field @"pseudoTerminal"
{-# INLINEABLE cdPseudoTerminal #-}
{-# DEPRECATED pseudoTerminal "Use generic-lens or generic-optics with 'pseudoTerminal' instead"  #-}

-- | When this parameter is true, the container is given read-only access to its root file system. This parameter maps to @ReadonlyRootfs@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--read-only@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- /Note:/ Consider using 'readonlyRootFilesystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdReadonlyRootFilesystem :: Lens.Lens' ContainerDefinition (Core.Maybe Core.Bool)
cdReadonlyRootFilesystem = Lens.field @"readonlyRootFilesystem"
{-# INLINEABLE cdReadonlyRootFilesystem #-}
{-# DEPRECATED readonlyRootFilesystem "Use generic-lens or generic-optics with 'readonlyRootFilesystem' instead"  #-}

-- | The private repository authentication credentials to use.
--
-- /Note:/ Consider using 'repositoryCredentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdRepositoryCredentials :: Lens.Lens' ContainerDefinition (Core.Maybe Types.RepositoryCredentials)
cdRepositoryCredentials = Lens.field @"repositoryCredentials"
{-# INLINEABLE cdRepositoryCredentials #-}
{-# DEPRECATED repositoryCredentials "Use generic-lens or generic-optics with 'repositoryCredentials' instead"  #-}

-- | The type and amount of a resource to assign to a container. The only supported resource is a GPU.
--
-- /Note:/ Consider using 'resourceRequirements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdResourceRequirements :: Lens.Lens' ContainerDefinition (Core.Maybe [Types.ResourceRequirement])
cdResourceRequirements = Lens.field @"resourceRequirements"
{-# INLINEABLE cdResourceRequirements #-}
{-# DEPRECATED resourceRequirements "Use generic-lens or generic-optics with 'resourceRequirements' instead"  #-}

-- | The secrets to pass to the container. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html Specifying Sensitive Data> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'secrets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdSecrets :: Lens.Lens' ContainerDefinition (Core.Maybe [Types.Secret])
cdSecrets = Lens.field @"secrets"
{-# INLINEABLE cdSecrets #-}
{-# DEPRECATED secrets "Use generic-lens or generic-optics with 'secrets' instead"  #-}

-- | Time duration (in seconds) to wait before giving up on resolving dependencies for a container. For example, you specify two containers in a task definition with containerA having a dependency on containerB reaching a @COMPLETE@ , @SUCCESS@ , or @HEALTHY@ status. If a @startTimeout@ value is specified for containerB and it does not reach the desired status within that time then containerA will give up and not start. This results in the task transitioning to a @STOPPED@ state.
--
-- For tasks using the Fargate launch type, this parameter requires that the task or service uses platform version 1.3.0 or later.
-- For tasks using the EC2 launch type, your container instances require at least version @1.26.0@ of the container agent to enable a container start timeout value. However, we recommend using the latest container agent version. For information about checking your agent version and updating to the latest version, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent> in the /Amazon Elastic Container Service Developer Guide/ . If you are using an Amazon ECS-optimized Linux AMI, your instance needs at least version @1.26.0-1@ of the @ecs-init@ package. If your container instances are launched from version @20190301@ or later, then they contain the required versions of the container agent and @ecs-init@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'startTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdStartTimeout :: Lens.Lens' ContainerDefinition (Core.Maybe Core.Int)
cdStartTimeout = Lens.field @"startTimeout"
{-# INLINEABLE cdStartTimeout #-}
{-# DEPRECATED startTimeout "Use generic-lens or generic-optics with 'startTimeout' instead"  #-}

-- | Time duration (in seconds) to wait before the container is forcefully killed if it doesn't exit normally on its own.
--
-- For tasks using the Fargate launch type, the task or service requires platform version 1.3.0 or later. The max stop timeout value is 120 seconds and if the parameter is not specified, the default value of 30 seconds is used.
-- For tasks using the EC2 launch type, if the @stopTimeout@ parameter is not specified, the value set for the Amazon ECS container agent configuration variable @ECS_CONTAINER_STOP_TIMEOUT@ is used by default. If neither the @stopTimeout@ parameter or the @ECS_CONTAINER_STOP_TIMEOUT@ agent configuration variable are set, then the default values of 30 seconds for Linux containers and 30 seconds on Windows containers are used. Your container instances require at least version 1.26.0 of the container agent to enable a container stop timeout value. However, we recommend using the latest container agent version. For information about checking your agent version and updating to the latest version, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent> in the /Amazon Elastic Container Service Developer Guide/ . If you are using an Amazon ECS-optimized Linux AMI, your instance needs at least version 1.26.0-1 of the @ecs-init@ package. If your container instances are launched from version @20190301@ or later, then they contain the required versions of the container agent and @ecs-init@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'stopTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdStopTimeout :: Lens.Lens' ContainerDefinition (Core.Maybe Core.Int)
cdStopTimeout = Lens.field @"stopTimeout"
{-# INLINEABLE cdStopTimeout #-}
{-# DEPRECATED stopTimeout "Use generic-lens or generic-optics with 'stopTimeout' instead"  #-}

-- | A list of namespaced kernel parameters to set in the container. This parameter maps to @Sysctls@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--sysctl@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- /Note:/ Consider using 'systemControls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdSystemControls :: Lens.Lens' ContainerDefinition (Core.Maybe [Types.SystemControl])
cdSystemControls = Lens.field @"systemControls"
{-# INLINEABLE cdSystemControls #-}
{-# DEPRECATED systemControls "Use generic-lens or generic-optics with 'systemControls' instead"  #-}

-- | A list of @ulimits@ to set in the container. If a ulimit value is specified in a task definition, it will override the default values set by Docker. This parameter maps to @Ulimits@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--ulimit@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . Valid naming values are displayed in the 'Ulimit' data type. This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@ 
--
-- /Note:/ Consider using 'ulimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdUlimits :: Lens.Lens' ContainerDefinition (Core.Maybe [Types.Ulimit])
cdUlimits = Lens.field @"ulimits"
{-# INLINEABLE cdUlimits #-}
{-# DEPRECATED ulimits "Use generic-lens or generic-optics with 'ulimits' instead"  #-}

-- | The user to use inside the container. This parameter maps to @User@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--user@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- /Important:/ When running tasks using the @host@ network mode, you should not run containers using the root user (UID 0). It is considered best practice to use a non-root user.
-- You can specify the @user@ using the following formats. If specifying a UID or GID, you must specify it as a positive integer.
--
--     * @user@ 
--
--
--     * @user:group@ 
--
--
--     * @uid@ 
--
--
--     * @uid:gid@ 
--
--
--     * @user:gid@ 
--
--
--     * @uid:group@ 
--
--
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdUser :: Lens.Lens' ContainerDefinition (Core.Maybe Core.Text)
cdUser = Lens.field @"user"
{-# INLINEABLE cdUser #-}
{-# DEPRECATED user "Use generic-lens or generic-optics with 'user' instead"  #-}

-- | Data volumes to mount from another container. This parameter maps to @VolumesFrom@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--volumes-from@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- /Note:/ Consider using 'volumesFrom' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdVolumesFrom :: Lens.Lens' ContainerDefinition (Core.Maybe [Types.VolumeFrom])
cdVolumesFrom = Lens.field @"volumesFrom"
{-# INLINEABLE cdVolumesFrom #-}
{-# DEPRECATED volumesFrom "Use generic-lens or generic-optics with 'volumesFrom' instead"  #-}

-- | The working directory in which to run commands inside the container. This parameter maps to @WorkingDir@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--workdir@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- /Note:/ Consider using 'workingDirectory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdWorkingDirectory :: Lens.Lens' ContainerDefinition (Core.Maybe Core.Text)
cdWorkingDirectory = Lens.field @"workingDirectory"
{-# INLINEABLE cdWorkingDirectory #-}
{-# DEPRECATED workingDirectory "Use generic-lens or generic-optics with 'workingDirectory' instead"  #-}

instance Core.FromJSON ContainerDefinition where
        toJSON ContainerDefinition{..}
          = Core.object
              (Core.catMaybes
                 [("command" Core..=) Core.<$> command,
                  ("cpu" Core..=) Core.<$> cpu,
                  ("dependsOn" Core..=) Core.<$> dependsOn,
                  ("disableNetworking" Core..=) Core.<$> disableNetworking,
                  ("dnsSearchDomains" Core..=) Core.<$> dnsSearchDomains,
                  ("dnsServers" Core..=) Core.<$> dnsServers,
                  ("dockerLabels" Core..=) Core.<$> dockerLabels,
                  ("dockerSecurityOptions" Core..=) Core.<$> dockerSecurityOptions,
                  ("entryPoint" Core..=) Core.<$> entryPoint,
                  ("environment" Core..=) Core.<$> environment,
                  ("environmentFiles" Core..=) Core.<$> environmentFiles,
                  ("essential" Core..=) Core.<$> essential,
                  ("extraHosts" Core..=) Core.<$> extraHosts,
                  ("firelensConfiguration" Core..=) Core.<$> firelensConfiguration,
                  ("healthCheck" Core..=) Core.<$> healthCheck,
                  ("hostname" Core..=) Core.<$> hostname,
                  ("image" Core..=) Core.<$> image,
                  ("interactive" Core..=) Core.<$> interactive,
                  ("links" Core..=) Core.<$> links,
                  ("linuxParameters" Core..=) Core.<$> linuxParameters,
                  ("logConfiguration" Core..=) Core.<$> logConfiguration,
                  ("memory" Core..=) Core.<$> memory,
                  ("memoryReservation" Core..=) Core.<$> memoryReservation,
                  ("mountPoints" Core..=) Core.<$> mountPoints,
                  ("name" Core..=) Core.<$> name,
                  ("portMappings" Core..=) Core.<$> portMappings,
                  ("privileged" Core..=) Core.<$> privileged,
                  ("pseudoTerminal" Core..=) Core.<$> pseudoTerminal,
                  ("readonlyRootFilesystem" Core..=) Core.<$> readonlyRootFilesystem,
                  ("repositoryCredentials" Core..=) Core.<$> repositoryCredentials,
                  ("resourceRequirements" Core..=) Core.<$> resourceRequirements,
                  ("secrets" Core..=) Core.<$> secrets,
                  ("startTimeout" Core..=) Core.<$> startTimeout,
                  ("stopTimeout" Core..=) Core.<$> stopTimeout,
                  ("systemControls" Core..=) Core.<$> systemControls,
                  ("ulimits" Core..=) Core.<$> ulimits,
                  ("user" Core..=) Core.<$> user,
                  ("volumesFrom" Core..=) Core.<$> volumesFrom,
                  ("workingDirectory" Core..=) Core.<$> workingDirectory])

instance Core.FromJSON ContainerDefinition where
        parseJSON
          = Core.withObject "ContainerDefinition" Core.$
              \ x ->
                ContainerDefinition' Core.<$>
                  (x Core..:? "command") Core.<*> x Core..:? "cpu" Core.<*>
                    x Core..:? "dependsOn"
                    Core.<*> x Core..:? "disableNetworking"
                    Core.<*> x Core..:? "dnsSearchDomains"
                    Core.<*> x Core..:? "dnsServers"
                    Core.<*> x Core..:? "dockerLabels"
                    Core.<*> x Core..:? "dockerSecurityOptions"
                    Core.<*> x Core..:? "entryPoint"
                    Core.<*> x Core..:? "environment"
                    Core.<*> x Core..:? "environmentFiles"
                    Core.<*> x Core..:? "essential"
                    Core.<*> x Core..:? "extraHosts"
                    Core.<*> x Core..:? "firelensConfiguration"
                    Core.<*> x Core..:? "healthCheck"
                    Core.<*> x Core..:? "hostname"
                    Core.<*> x Core..:? "image"
                    Core.<*> x Core..:? "interactive"
                    Core.<*> x Core..:? "links"
                    Core.<*> x Core..:? "linuxParameters"
                    Core.<*> x Core..:? "logConfiguration"
                    Core.<*> x Core..:? "memory"
                    Core.<*> x Core..:? "memoryReservation"
                    Core.<*> x Core..:? "mountPoints"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "portMappings"
                    Core.<*> x Core..:? "privileged"
                    Core.<*> x Core..:? "pseudoTerminal"
                    Core.<*> x Core..:? "readonlyRootFilesystem"
                    Core.<*> x Core..:? "repositoryCredentials"
                    Core.<*> x Core..:? "resourceRequirements"
                    Core.<*> x Core..:? "secrets"
                    Core.<*> x Core..:? "startTimeout"
                    Core.<*> x Core..:? "stopTimeout"
                    Core.<*> x Core..:? "systemControls"
                    Core.<*> x Core..:? "ulimits"
                    Core.<*> x Core..:? "user"
                    Core.<*> x Core..:? "volumesFrom"
                    Core.<*> x Core..:? "workingDirectory"
