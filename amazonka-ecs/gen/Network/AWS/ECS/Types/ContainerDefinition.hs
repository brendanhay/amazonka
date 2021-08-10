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
-- Module      : Network.AWS.ECS.Types.ContainerDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ContainerDefinition where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types.ContainerDependency
import Network.AWS.ECS.Types.EnvironmentFile
import Network.AWS.ECS.Types.FirelensConfiguration
import Network.AWS.ECS.Types.HealthCheck
import Network.AWS.ECS.Types.HostEntry
import Network.AWS.ECS.Types.KeyValuePair
import Network.AWS.ECS.Types.LinuxParameters
import Network.AWS.ECS.Types.LogConfiguration
import Network.AWS.ECS.Types.MountPoint
import Network.AWS.ECS.Types.PortMapping
import Network.AWS.ECS.Types.RepositoryCredentials
import Network.AWS.ECS.Types.ResourceRequirement
import Network.AWS.ECS.Types.Secret
import Network.AWS.ECS.Types.SystemControl
import Network.AWS.ECS.Types.Ulimit
import Network.AWS.ECS.Types.VolumeFrom
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Container definitions are used in task definitions to describe the
-- different containers that are launched as part of a task.
--
-- /See:/ 'newContainerDefinition' smart constructor.
data ContainerDefinition = ContainerDefinition'
  { -- | The hostname to use for your container. This parameter maps to
    -- @Hostname@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @--hostname@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    --
    -- The @hostname@ parameter is not supported if you are using the @awsvpc@
    -- network mode.
    hostname :: Prelude.Maybe Prelude.Text,
    -- | The dependencies defined for container startup and shutdown. A container
    -- can contain multiple dependencies. When a dependency is defined for
    -- container startup, for container shutdown it is reversed.
    --
    -- For tasks using the EC2 launch type, the container instances require at
    -- least version 1.26.0 of the container agent to enable container
    -- dependencies. However, we recommend using the latest container agent
    -- version. For information about checking your agent version and updating
    -- to the latest version, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent>
    -- in the /Amazon Elastic Container Service Developer Guide/. If you are
    -- using an Amazon ECS-optimized Linux AMI, your instance needs at least
    -- version 1.26.0-1 of the @ecs-init@ package. If your container instances
    -- are launched from version @20190301@ or later, then they contain the
    -- required versions of the container agent and @ecs-init@. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    --
    -- For tasks using the Fargate launch type, the task or service requires
    -- platform version @1.3.0@ or later.
    dependsOn :: Prelude.Maybe [ContainerDependency],
    -- | Linux-specific modifications that are applied to the container, such as
    -- Linux kernel capabilities. For more information see KernelCapabilities.
    --
    -- This parameter is not supported for Windows containers.
    linuxParameters :: Prelude.Maybe LinuxParameters,
    -- | The FireLens configuration for the container. This is used to specify
    -- and configure a log router for container logs. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html Custom Log Routing>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    firelensConfiguration :: Prelude.Maybe FirelensConfiguration,
    -- | The amount (in MiB) of memory to present to the container. If your
    -- container attempts to exceed the memory specified here, the container is
    -- killed. The total amount of memory reserved for all containers within a
    -- task must be lower than the task @memory@ value, if one is specified.
    -- This parameter maps to @Memory@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @--memory@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    --
    -- If using the Fargate launch type, this parameter is optional.
    --
    -- If using the EC2 launch type, you must specify either a task-level
    -- memory value or a container-level memory value. If you specify both a
    -- container-level @memory@ and @memoryReservation@ value, @memory@ must be
    -- greater than @memoryReservation@. If you specify @memoryReservation@,
    -- then that value is subtracted from the available memory resources for
    -- the container instance on which the container is placed. Otherwise, the
    -- value of @memory@ is used.
    --
    -- The Docker daemon reserves a minimum of 4 MiB of memory for a container,
    -- so you should not specify fewer than 4 MiB of memory for your
    -- containers.
    memory :: Prelude.Maybe Prelude.Int,
    -- | The user to use inside the container. This parameter maps to @User@ in
    -- the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @--user@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    --
    -- When running tasks using the @host@ network mode, you should not run
    -- containers using the root user (UID 0). It is considered best practice
    -- to use a non-root user.
    --
    -- You can specify the @user@ using the following formats. If specifying a
    -- UID or GID, you must specify it as a positive integer.
    --
    -- -   @user@
    --
    -- -   @user:group@
    --
    -- -   @uid@
    --
    -- -   @uid:gid@
    --
    -- -   @user:gid@
    --
    -- -   @uid:group@
    --
    -- This parameter is not supported for Windows containers or tasks that use
    -- the awsvpc network mode.
    user :: Prelude.Maybe Prelude.Text,
    -- | The soft limit (in MiB) of memory to reserve for the container. When
    -- system memory is under heavy contention, Docker attempts to keep the
    -- container memory to this soft limit. However, your container can consume
    -- more memory when it needs to, up to either the hard limit specified with
    -- the @memory@ parameter (if applicable), or all of the available memory
    -- on the container instance, whichever comes first. This parameter maps to
    -- @MemoryReservation@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @--memory-reservation@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    --
    -- If a task-level memory value is not specified, you must specify a
    -- non-zero integer for one or both of @memory@ or @memoryReservation@ in a
    -- container definition. If you specify both, @memory@ must be greater than
    -- @memoryReservation@. If you specify @memoryReservation@, then that value
    -- is subtracted from the available memory resources for the container
    -- instance on which the container is placed. Otherwise, the value of
    -- @memory@ is used.
    --
    -- For example, if your container normally uses 128 MiB of memory, but
    -- occasionally bursts to 256 MiB of memory for short periods of time, you
    -- can set a @memoryReservation@ of 128 MiB, and a @memory@ hard limit of
    -- 300 MiB. This configuration would allow the container to only reserve
    -- 128 MiB of memory from the remaining resources on the container
    -- instance, but also allow the container to consume more memory resources
    -- when needed.
    --
    -- The Docker daemon reserves a minimum of 4 MiB of memory for a container,
    -- so you should not specify fewer than 4 MiB of memory for your
    -- containers.
    memoryReservation :: Prelude.Maybe Prelude.Int,
    -- | A key\/value map of labels to add to the container. This parameter maps
    -- to @Labels@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @--label@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    -- This parameter requires version 1.18 of the Docker Remote API or greater
    -- on your container instance. To check the Docker Remote API version on
    -- your container instance, log in to your container instance and run the
    -- following command:
    -- @sudo docker version --format \'{{.Server.APIVersion}}\'@
    dockerLabels :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of hostnames and IP address mappings to append to the
    -- @\/etc\/hosts@ file on the container. This parameter maps to
    -- @ExtraHosts@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @--add-host@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    --
    -- This parameter is not supported for Windows containers or tasks that use
    -- the @awsvpc@ network mode.
    extraHosts :: Prelude.Maybe [HostEntry],
    -- | A list of namespaced kernel parameters to set in the container. This
    -- parameter maps to @Sysctls@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @--sysctl@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    --
    -- It is not recommended that you specify network-related @systemControls@
    -- parameters for multiple containers in a single task that also uses
    -- either the @awsvpc@ or @host@ network modes. For tasks that use the
    -- @awsvpc@ network mode, the container that is started last determines
    -- which @systemControls@ parameters take effect. For tasks that use the
    -- @host@ network mode, it changes the container instance\'s namespaced
    -- kernel parameters as well as the containers.
    systemControls :: Prelude.Maybe [SystemControl],
    -- | When this parameter is true, the container is given elevated privileges
    -- on the host container instance (similar to the @root@ user). This
    -- parameter maps to @Privileged@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @--privileged@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    --
    -- This parameter is not supported for Windows containers or tasks using
    -- the Fargate launch type.
    privileged :: Prelude.Maybe Prelude.Bool,
    -- | The @links@ parameter allows containers to communicate with each other
    -- without the need for port mappings. This parameter is only supported if
    -- the network mode of a task definition is @bridge@. The
    -- @name:internalName@ construct is analogous to @name:alias@ in Docker
    -- links. Up to 255 letters (uppercase and lowercase), numbers, and hyphens
    -- are allowed. For more information about linking Docker containers, go to
    -- <https://docs.docker.com/network/links/ Legacy container links> in the
    -- Docker documentation. This parameter maps to @Links@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @--link@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    --
    -- This parameter is not supported for Windows containers or tasks that use
    -- the awsvpc network mode.
    --
    -- Containers that are collocated on a single container instance may be
    -- able to communicate with each other without requiring links or host port
    -- mappings. Network isolation is achieved on the container instance using
    -- security groups and VPC settings.
    links :: Prelude.Maybe [Prelude.Text],
    -- | A list of files containing the environment variables to pass to a
    -- container. This parameter maps to the @--env-file@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    --
    -- You can specify up to ten environment files. The file must have a @.env@
    -- file extension. Each line in an environment file should contain an
    -- environment variable in @VARIABLE=VALUE@ format. Lines beginning with
    -- @#@ are treated as comments and are ignored. For more information on the
    -- environment variable file syntax, see
    -- <https://docs.docker.com/compose/env-file/ Declare default environment variables in file>.
    --
    -- If there are environment variables specified using the @environment@
    -- parameter in a container definition, they take precedence over the
    -- variables contained within an environment file. If multiple environment
    -- files are specified that contain the same variable, they are processed
    -- from the top down. It is recommended to use unique variable names. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/taskdef-envfiles.html Specifying Environment Variables>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    environmentFiles :: Prelude.Maybe [EnvironmentFile],
    -- | When this parameter is @true@, this allows you to deploy containerized
    -- applications that require @stdin@ or a @tty@ to be allocated. This
    -- parameter maps to @OpenStdin@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @--interactive@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    interactive :: Prelude.Maybe Prelude.Bool,
    -- | The working directory in which to run commands inside the container.
    -- This parameter maps to @WorkingDir@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @--workdir@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    workingDirectory :: Prelude.Maybe Prelude.Text,
    -- | Early versions of the Amazon ECS container agent do not properly handle
    -- @entryPoint@ parameters. If you have problems using @entryPoint@, update
    -- your container agent or enter your commands and arguments as @command@
    -- array items instead.
    --
    -- The entry point that is passed to the container. This parameter maps to
    -- @Entrypoint@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @--entrypoint@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    -- For more information, see
    -- <https://docs.docker.com/engine/reference/builder/#entrypoint>.
    entryPoint :: Prelude.Maybe [Prelude.Text],
    -- | The environment variables to pass to a container. This parameter maps to
    -- @Env@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @--env@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    --
    -- We do not recommend using plaintext environment variables for sensitive
    -- information, such as credential data.
    environment :: Prelude.Maybe [KeyValuePair],
    -- | Data volumes to mount from another container. This parameter maps to
    -- @VolumesFrom@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @--volumes-from@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    volumesFrom :: Prelude.Maybe [VolumeFrom],
    -- | The secrets to pass to the container. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html Specifying Sensitive Data>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    secrets :: Prelude.Maybe [Secret],
    -- | The mount points for data volumes in your container.
    --
    -- This parameter maps to @Volumes@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @--volume@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    --
    -- Windows containers can mount whole directories on the same drive as
    -- @$env:ProgramData@. Windows containers cannot mount directories on a
    -- different drive, and mount point cannot be across drives.
    mountPoints :: Prelude.Maybe [MountPoint],
    -- | A list of DNS servers that are presented to the container. This
    -- parameter maps to @Dns@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @--dns@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    --
    -- This parameter is not supported for Windows containers or tasks that use
    -- the awsvpc network mode.
    dnsServers :: Prelude.Maybe [Prelude.Text],
    -- | The name of a container. If you are linking multiple containers together
    -- in a task definition, the @name@ of one container can be entered in the
    -- @links@ of another container to connect the containers. Up to 255
    -- letters (uppercase and lowercase), numbers, and hyphens are allowed.
    -- This parameter maps to @name@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @--name@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    name :: Prelude.Maybe Prelude.Text,
    -- | The image used to start a container. This string is passed directly to
    -- the Docker daemon. Images in the Docker Hub registry are available by
    -- default. Other repositories are specified with either
    -- @ repository-url\/image:tag @ or @ repository-url\/image\@digest @. Up
    -- to 255 letters (uppercase and lowercase), numbers, hyphens, underscores,
    -- colons, periods, forward slashes, and number signs are allowed. This
    -- parameter maps to @Image@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @IMAGE@ parameter of
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    --
    -- -   When a new task starts, the Amazon ECS container agent pulls the
    --     latest version of the specified image and tag for the container to
    --     use. However, subsequent updates to a repository image are not
    --     propagated to already running tasks.
    --
    -- -   Images in Amazon ECR repositories can be specified by either using
    --     the full @registry\/repository:tag@ or
    --     @registry\/repository\@digest@. For example,
    --     @012345678910.dkr.ecr.\<region-name>.amazonaws.com\/\<repository-name>:latest@
    --     or
    --     @012345678910.dkr.ecr.\<region-name>.amazonaws.com\/\<repository-name>\@sha256:94afd1f2e64d908bc90dbca0035a5b567EXAMPLE@.
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
    -- | The command that is passed to the container. This parameter maps to
    -- @Cmd@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @COMMAND@ parameter to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    -- For more information, see
    -- <https://docs.docker.com/engine/reference/builder/#cmd>. If there are
    -- multiple arguments, each argument should be a separated string in the
    -- array.
    command :: Prelude.Maybe [Prelude.Text],
    -- | The log configuration specification for the container.
    --
    -- This parameter maps to @LogConfig@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @--log-driver@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    -- By default, containers use the same logging driver that the Docker
    -- daemon uses. However the container may use a different logging driver
    -- than the Docker daemon by specifying a log driver with this parameter in
    -- the container definition. To use a different logging driver for a
    -- container, the log system must be configured properly on the container
    -- instance (or on a different log server for remote logging options). For
    -- more information on the options for different supported log drivers, see
    -- <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers>
    -- in the Docker documentation.
    --
    -- Amazon ECS currently supports a subset of the logging drivers available
    -- to the Docker daemon (shown in the LogConfiguration data type).
    -- Additional log drivers may be available in future releases of the Amazon
    -- ECS container agent.
    --
    -- This parameter requires version 1.18 of the Docker Remote API or greater
    -- on your container instance. To check the Docker Remote API version on
    -- your container instance, log in to your container instance and run the
    -- following command:
    -- @sudo docker version --format \'{{.Server.APIVersion}}\'@
    --
    -- The Amazon ECS container agent running on a container instance must
    -- register the logging drivers available on that instance with the
    -- @ECS_AVAILABLE_LOGGING_DRIVERS@ environment variable before containers
    -- placed on that instance can use these log configuration options. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-config.html Amazon ECS Container Agent Configuration>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    logConfiguration :: Prelude.Maybe LogConfiguration,
    -- | When this parameter is @true@, a TTY is allocated. This parameter maps
    -- to @Tty@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @--tty@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    pseudoTerminal :: Prelude.Maybe Prelude.Bool,
    -- | A list of DNS search domains that are presented to the container. This
    -- parameter maps to @DnsSearch@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @--dns-search@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    --
    -- This parameter is not supported for Windows containers or tasks that use
    -- the awsvpc network mode.
    dnsSearchDomains :: Prelude.Maybe [Prelude.Text],
    -- | If the @essential@ parameter of a container is marked as @true@, and
    -- that container fails or stops for any reason, all other containers that
    -- are part of the task are stopped. If the @essential@ parameter of a
    -- container is marked as @false@, then its failure does not affect the
    -- rest of the containers in a task. If this parameter is omitted, a
    -- container is assumed to be essential.
    --
    -- All tasks must have at least one essential container. If you have an
    -- application that is composed of multiple containers, you should group
    -- containers that are used for a common purpose into components, and
    -- separate the different components into multiple task definitions. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/application_architecture.html Application Architecture>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    essential :: Prelude.Maybe Prelude.Bool,
    -- | The list of port mappings for the container. Port mappings allow
    -- containers to access ports on the host container instance to send or
    -- receive traffic.
    --
    -- For task definitions that use the @awsvpc@ network mode, you should only
    -- specify the @containerPort@. The @hostPort@ can be left blank or it must
    -- be the same value as the @containerPort@.
    --
    -- Port mappings on Windows use the @NetNAT@ gateway address rather than
    -- @localhost@. There is no loopback for port mappings on Windows, so you
    -- cannot access a container\'s mapped port from the host itself.
    --
    -- This parameter maps to @PortBindings@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @--publish@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    -- If the network mode of a task definition is set to @none@, then you
    -- can\'t specify port mappings. If the network mode of a task definition
    -- is set to @host@, then host ports must either be undefined or they must
    -- match the container port in the port mapping.
    --
    -- After a task reaches the @RUNNING@ status, manual and automatic host and
    -- container port assignments are visible in the __Network Bindings__
    -- section of a container description for a selected task in the Amazon ECS
    -- console. The assignments are also visible in the @networkBindings@
    -- section DescribeTasks responses.
    portMappings :: Prelude.Maybe [PortMapping],
    -- | The number of @cpu@ units reserved for the container. This parameter
    -- maps to @CpuShares@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @--cpu-shares@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    --
    -- This field is optional for tasks using the Fargate launch type, and the
    -- only requirement is that the total amount of CPU reserved for all
    -- containers within a task be lower than the task-level @cpu@ value.
    --
    -- You can determine the number of CPU units that are available per EC2
    -- instance type by multiplying the vCPUs listed for that instance type on
    -- the <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instances>
    -- detail page by 1,024.
    --
    -- Linux containers share unallocated CPU units with other containers on
    -- the container instance with the same ratio as their allocated amount.
    -- For example, if you run a single-container task on a single-core
    -- instance type with 512 CPU units specified for that container, and that
    -- is the only task running on the container instance, that container could
    -- use the full 1,024 CPU unit share at any given time. However, if you
    -- launched another copy of the same task on that container instance, each
    -- task would be guaranteed a minimum of 512 CPU units when needed, and
    -- each container could float to higher CPU usage if the other container
    -- was not using it, but if both tasks were 100% active all of the time,
    -- they would be limited to 512 CPU units.
    --
    -- On Linux container instances, the Docker daemon on the container
    -- instance uses the CPU value to calculate the relative CPU share ratios
    -- for running containers. For more information, see
    -- <https://docs.docker.com/engine/reference/run/#cpu-share-constraint CPU share constraint>
    -- in the Docker documentation. The minimum valid CPU share value that the
    -- Linux kernel allows is 2. However, the CPU parameter is not required,
    -- and you can use CPU values below 2 in your container definitions. For
    -- CPU values below 2 (including null), the behavior varies based on your
    -- Amazon ECS container agent version:
    --
    -- -   __Agent versions less than or equal to 1.1.0:__ Null and zero CPU
    --     values are passed to Docker as 0, which Docker then converts to
    --     1,024 CPU shares. CPU values of 1 are passed to Docker as 1, which
    --     the Linux kernel converts to two CPU shares.
    --
    -- -   __Agent versions greater than or equal to 1.2.0:__ Null, zero, and
    --     CPU values of 1 are passed to Docker as 2.
    --
    -- On Windows container instances, the CPU limit is enforced as an absolute
    -- limit, or a quota. Windows containers only have access to the specified
    -- amount of CPU that is described in the task definition. A null or zero
    -- CPU value is passed to Docker as @0@, which Windows interprets as 1% of
    -- one CPU.
    cpu :: Prelude.Maybe Prelude.Int,
    -- | The type and amount of a resource to assign to a container. The only
    -- supported resource is a GPU.
    resourceRequirements :: Prelude.Maybe [ResourceRequirement],
    -- | Time duration (in seconds) to wait before giving up on resolving
    -- dependencies for a container. For example, you specify two containers in
    -- a task definition with containerA having a dependency on containerB
    -- reaching a @COMPLETE@, @SUCCESS@, or @HEALTHY@ status. If a
    -- @startTimeout@ value is specified for containerB and it does not reach
    -- the desired status within that time then containerA will give up and not
    -- start. This results in the task transitioning to a @STOPPED@ state.
    --
    -- When the @ECS_CONTAINER_START_TIMEOUT@ container agent configuration
    -- variable is used, it is enforced indendently from this start timeout
    -- value.
    --
    -- For tasks using the Fargate launch type, this parameter requires that
    -- the task or service uses platform version 1.3.0 or later.
    --
    -- For tasks using the EC2 launch type, your container instances require at
    -- least version @1.26.0@ of the container agent to enable a container
    -- start timeout value. However, we recommend using the latest container
    -- agent version. For information about checking your agent version and
    -- updating to the latest version, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent>
    -- in the /Amazon Elastic Container Service Developer Guide/. If you are
    -- using an Amazon ECS-optimized Linux AMI, your instance needs at least
    -- version @1.26.0-1@ of the @ecs-init@ package. If your container
    -- instances are launched from version @20190301@ or later, then they
    -- contain the required versions of the container agent and @ecs-init@. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    startTimeout :: Prelude.Maybe Prelude.Int,
    -- | When this parameter is true, the container is given read-only access to
    -- its root file system. This parameter maps to @ReadonlyRootfs@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @--read-only@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    --
    -- This parameter is not supported for Windows containers or tasks that use
    -- the awsvpc network mode.
    readonlyRootFilesystem :: Prelude.Maybe Prelude.Bool,
    -- | A list of @ulimits@ to set in the container. If a ulimit value is
    -- specified in a task definition, it will override the default values set
    -- by Docker. This parameter maps to @Ulimits@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @--ulimit@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    -- Valid naming values are displayed in the Ulimit data type. This
    -- parameter requires version 1.18 of the Docker Remote API or greater on
    -- your container instance. To check the Docker Remote API version on your
    -- container instance, log in to your container instance and run the
    -- following command:
    -- @sudo docker version --format \'{{.Server.APIVersion}}\'@
    --
    -- This parameter is not supported for Windows containers or tasks that use
    -- the awsvpc network mode.
    ulimits :: Prelude.Maybe [Ulimit],
    -- | Time duration (in seconds) to wait before the container is forcefully
    -- killed if it doesn\'t exit normally on its own.
    --
    -- For tasks using the Fargate launch type, the task or service requires
    -- platform version 1.3.0 or later. The max stop timeout value is 120
    -- seconds and if the parameter is not specified, the default value of 30
    -- seconds is used.
    --
    -- For tasks using the EC2 launch type, if the @stopTimeout@ parameter is
    -- not specified, the value set for the Amazon ECS container agent
    -- configuration variable @ECS_CONTAINER_STOP_TIMEOUT@ is used by default.
    -- If neither the @stopTimeout@ parameter or the
    -- @ECS_CONTAINER_STOP_TIMEOUT@ agent configuration variable are set, then
    -- the default values of 30 seconds for Linux containers and 30 seconds on
    -- Windows containers are used. Your container instances require at least
    -- version 1.26.0 of the container agent to enable a container stop timeout
    -- value. However, we recommend using the latest container agent version.
    -- For information about checking your agent version and updating to the
    -- latest version, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent>
    -- in the /Amazon Elastic Container Service Developer Guide/. If you are
    -- using an Amazon ECS-optimized Linux AMI, your instance needs at least
    -- version 1.26.0-1 of the @ecs-init@ package. If your container instances
    -- are launched from version @20190301@ or later, then they contain the
    -- required versions of the container agent and @ecs-init@. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    stopTimeout :: Prelude.Maybe Prelude.Int,
    -- | A list of strings to provide custom labels for SELinux and AppArmor
    -- multi-level security systems. This field is not valid for containers in
    -- tasks using the Fargate launch type.
    --
    -- With Windows containers, this parameter can be used to reference a
    -- credential spec file when configuring a container for Active Directory
    -- authentication. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/windows-gmsa.html Using gMSAs for Windows Containers>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    --
    -- This parameter maps to @SecurityOpt@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @--security-opt@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    --
    -- The Amazon ECS container agent running on a container instance must
    -- register with the @ECS_SELINUX_CAPABLE=true@ or
    -- @ECS_APPARMOR_CAPABLE=true@ environment variables before containers
    -- placed on that instance can use these security options. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-config.html Amazon ECS Container Agent Configuration>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    --
    -- For more information about valid values, see
    -- <https://docs.docker.com/engine/reference/run/#security-configuration Docker Run Security Configuration>.
    --
    -- Valid values: \"no-new-privileges\" | \"apparmor:PROFILE\" |
    -- \"label:value\" | \"credentialspec:CredentialSpecFilePath\"
    dockerSecurityOptions :: Prelude.Maybe [Prelude.Text],
    -- | When this parameter is true, networking is disabled within the
    -- container. This parameter maps to @NetworkDisabled@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API>.
    --
    -- This parameter is not supported for Windows containers or tasks that use
    -- the awsvpc network mode.
    disableNetworking :: Prelude.Maybe Prelude.Bool,
    -- | The container health check command and associated configuration
    -- parameters for the container. This parameter maps to @HealthCheck@ in
    -- the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @HEALTHCHECK@ parameter of
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    healthCheck :: Prelude.Maybe HealthCheck,
    -- | The private repository authentication credentials to use.
    repositoryCredentials :: Prelude.Maybe RepositoryCredentials
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostname', 'containerDefinition_hostname' - The hostname to use for your container. This parameter maps to
-- @Hostname@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--hostname@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- The @hostname@ parameter is not supported if you are using the @awsvpc@
-- network mode.
--
-- 'dependsOn', 'containerDefinition_dependsOn' - The dependencies defined for container startup and shutdown. A container
-- can contain multiple dependencies. When a dependency is defined for
-- container startup, for container shutdown it is reversed.
--
-- For tasks using the EC2 launch type, the container instances require at
-- least version 1.26.0 of the container agent to enable container
-- dependencies. However, we recommend using the latest container agent
-- version. For information about checking your agent version and updating
-- to the latest version, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent>
-- in the /Amazon Elastic Container Service Developer Guide/. If you are
-- using an Amazon ECS-optimized Linux AMI, your instance needs at least
-- version 1.26.0-1 of the @ecs-init@ package. If your container instances
-- are launched from version @20190301@ or later, then they contain the
-- required versions of the container agent and @ecs-init@. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- For tasks using the Fargate launch type, the task or service requires
-- platform version @1.3.0@ or later.
--
-- 'linuxParameters', 'containerDefinition_linuxParameters' - Linux-specific modifications that are applied to the container, such as
-- Linux kernel capabilities. For more information see KernelCapabilities.
--
-- This parameter is not supported for Windows containers.
--
-- 'firelensConfiguration', 'containerDefinition_firelensConfiguration' - The FireLens configuration for the container. This is used to specify
-- and configure a log router for container logs. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html Custom Log Routing>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'memory', 'containerDefinition_memory' - The amount (in MiB) of memory to present to the container. If your
-- container attempts to exceed the memory specified here, the container is
-- killed. The total amount of memory reserved for all containers within a
-- task must be lower than the task @memory@ value, if one is specified.
-- This parameter maps to @Memory@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--memory@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- If using the Fargate launch type, this parameter is optional.
--
-- If using the EC2 launch type, you must specify either a task-level
-- memory value or a container-level memory value. If you specify both a
-- container-level @memory@ and @memoryReservation@ value, @memory@ must be
-- greater than @memoryReservation@. If you specify @memoryReservation@,
-- then that value is subtracted from the available memory resources for
-- the container instance on which the container is placed. Otherwise, the
-- value of @memory@ is used.
--
-- The Docker daemon reserves a minimum of 4 MiB of memory for a container,
-- so you should not specify fewer than 4 MiB of memory for your
-- containers.
--
-- 'user', 'containerDefinition_user' - The user to use inside the container. This parameter maps to @User@ in
-- the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--user@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- When running tasks using the @host@ network mode, you should not run
-- containers using the root user (UID 0). It is considered best practice
-- to use a non-root user.
--
-- You can specify the @user@ using the following formats. If specifying a
-- UID or GID, you must specify it as a positive integer.
--
-- -   @user@
--
-- -   @user:group@
--
-- -   @uid@
--
-- -   @uid:gid@
--
-- -   @user:gid@
--
-- -   @uid:group@
--
-- This parameter is not supported for Windows containers or tasks that use
-- the awsvpc network mode.
--
-- 'memoryReservation', 'containerDefinition_memoryReservation' - The soft limit (in MiB) of memory to reserve for the container. When
-- system memory is under heavy contention, Docker attempts to keep the
-- container memory to this soft limit. However, your container can consume
-- more memory when it needs to, up to either the hard limit specified with
-- the @memory@ parameter (if applicable), or all of the available memory
-- on the container instance, whichever comes first. This parameter maps to
-- @MemoryReservation@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--memory-reservation@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- If a task-level memory value is not specified, you must specify a
-- non-zero integer for one or both of @memory@ or @memoryReservation@ in a
-- container definition. If you specify both, @memory@ must be greater than
-- @memoryReservation@. If you specify @memoryReservation@, then that value
-- is subtracted from the available memory resources for the container
-- instance on which the container is placed. Otherwise, the value of
-- @memory@ is used.
--
-- For example, if your container normally uses 128 MiB of memory, but
-- occasionally bursts to 256 MiB of memory for short periods of time, you
-- can set a @memoryReservation@ of 128 MiB, and a @memory@ hard limit of
-- 300 MiB. This configuration would allow the container to only reserve
-- 128 MiB of memory from the remaining resources on the container
-- instance, but also allow the container to consume more memory resources
-- when needed.
--
-- The Docker daemon reserves a minimum of 4 MiB of memory for a container,
-- so you should not specify fewer than 4 MiB of memory for your
-- containers.
--
-- 'dockerLabels', 'containerDefinition_dockerLabels' - A key\/value map of labels to add to the container. This parameter maps
-- to @Labels@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--label@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
-- This parameter requires version 1.18 of the Docker Remote API or greater
-- on your container instance. To check the Docker Remote API version on
-- your container instance, log in to your container instance and run the
-- following command:
-- @sudo docker version --format \'{{.Server.APIVersion}}\'@
--
-- 'extraHosts', 'containerDefinition_extraHosts' - A list of hostnames and IP address mappings to append to the
-- @\/etc\/hosts@ file on the container. This parameter maps to
-- @ExtraHosts@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--add-host@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- This parameter is not supported for Windows containers or tasks that use
-- the @awsvpc@ network mode.
--
-- 'systemControls', 'containerDefinition_systemControls' - A list of namespaced kernel parameters to set in the container. This
-- parameter maps to @Sysctls@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--sysctl@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- It is not recommended that you specify network-related @systemControls@
-- parameters for multiple containers in a single task that also uses
-- either the @awsvpc@ or @host@ network modes. For tasks that use the
-- @awsvpc@ network mode, the container that is started last determines
-- which @systemControls@ parameters take effect. For tasks that use the
-- @host@ network mode, it changes the container instance\'s namespaced
-- kernel parameters as well as the containers.
--
-- 'privileged', 'containerDefinition_privileged' - When this parameter is true, the container is given elevated privileges
-- on the host container instance (similar to the @root@ user). This
-- parameter maps to @Privileged@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--privileged@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- This parameter is not supported for Windows containers or tasks using
-- the Fargate launch type.
--
-- 'links', 'containerDefinition_links' - The @links@ parameter allows containers to communicate with each other
-- without the need for port mappings. This parameter is only supported if
-- the network mode of a task definition is @bridge@. The
-- @name:internalName@ construct is analogous to @name:alias@ in Docker
-- links. Up to 255 letters (uppercase and lowercase), numbers, and hyphens
-- are allowed. For more information about linking Docker containers, go to
-- <https://docs.docker.com/network/links/ Legacy container links> in the
-- Docker documentation. This parameter maps to @Links@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--link@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- This parameter is not supported for Windows containers or tasks that use
-- the awsvpc network mode.
--
-- Containers that are collocated on a single container instance may be
-- able to communicate with each other without requiring links or host port
-- mappings. Network isolation is achieved on the container instance using
-- security groups and VPC settings.
--
-- 'environmentFiles', 'containerDefinition_environmentFiles' - A list of files containing the environment variables to pass to a
-- container. This parameter maps to the @--env-file@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- You can specify up to ten environment files. The file must have a @.env@
-- file extension. Each line in an environment file should contain an
-- environment variable in @VARIABLE=VALUE@ format. Lines beginning with
-- @#@ are treated as comments and are ignored. For more information on the
-- environment variable file syntax, see
-- <https://docs.docker.com/compose/env-file/ Declare default environment variables in file>.
--
-- If there are environment variables specified using the @environment@
-- parameter in a container definition, they take precedence over the
-- variables contained within an environment file. If multiple environment
-- files are specified that contain the same variable, they are processed
-- from the top down. It is recommended to use unique variable names. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/taskdef-envfiles.html Specifying Environment Variables>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'interactive', 'containerDefinition_interactive' - When this parameter is @true@, this allows you to deploy containerized
-- applications that require @stdin@ or a @tty@ to be allocated. This
-- parameter maps to @OpenStdin@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--interactive@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- 'workingDirectory', 'containerDefinition_workingDirectory' - The working directory in which to run commands inside the container.
-- This parameter maps to @WorkingDir@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--workdir@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- 'entryPoint', 'containerDefinition_entryPoint' - Early versions of the Amazon ECS container agent do not properly handle
-- @entryPoint@ parameters. If you have problems using @entryPoint@, update
-- your container agent or enter your commands and arguments as @command@
-- array items instead.
--
-- The entry point that is passed to the container. This parameter maps to
-- @Entrypoint@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--entrypoint@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
-- For more information, see
-- <https://docs.docker.com/engine/reference/builder/#entrypoint>.
--
-- 'environment', 'containerDefinition_environment' - The environment variables to pass to a container. This parameter maps to
-- @Env@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--env@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- We do not recommend using plaintext environment variables for sensitive
-- information, such as credential data.
--
-- 'volumesFrom', 'containerDefinition_volumesFrom' - Data volumes to mount from another container. This parameter maps to
-- @VolumesFrom@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--volumes-from@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- 'secrets', 'containerDefinition_secrets' - The secrets to pass to the container. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html Specifying Sensitive Data>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'mountPoints', 'containerDefinition_mountPoints' - The mount points for data volumes in your container.
--
-- This parameter maps to @Volumes@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--volume@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- Windows containers can mount whole directories on the same drive as
-- @$env:ProgramData@. Windows containers cannot mount directories on a
-- different drive, and mount point cannot be across drives.
--
-- 'dnsServers', 'containerDefinition_dnsServers' - A list of DNS servers that are presented to the container. This
-- parameter maps to @Dns@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--dns@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- This parameter is not supported for Windows containers or tasks that use
-- the awsvpc network mode.
--
-- 'name', 'containerDefinition_name' - The name of a container. If you are linking multiple containers together
-- in a task definition, the @name@ of one container can be entered in the
-- @links@ of another container to connect the containers. Up to 255
-- letters (uppercase and lowercase), numbers, and hyphens are allowed.
-- This parameter maps to @name@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--name@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- 'image', 'containerDefinition_image' - The image used to start a container. This string is passed directly to
-- the Docker daemon. Images in the Docker Hub registry are available by
-- default. Other repositories are specified with either
-- @ repository-url\/image:tag @ or @ repository-url\/image\@digest @. Up
-- to 255 letters (uppercase and lowercase), numbers, hyphens, underscores,
-- colons, periods, forward slashes, and number signs are allowed. This
-- parameter maps to @Image@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @IMAGE@ parameter of
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- -   When a new task starts, the Amazon ECS container agent pulls the
--     latest version of the specified image and tag for the container to
--     use. However, subsequent updates to a repository image are not
--     propagated to already running tasks.
--
-- -   Images in Amazon ECR repositories can be specified by either using
--     the full @registry\/repository:tag@ or
--     @registry\/repository\@digest@. For example,
--     @012345678910.dkr.ecr.\<region-name>.amazonaws.com\/\<repository-name>:latest@
--     or
--     @012345678910.dkr.ecr.\<region-name>.amazonaws.com\/\<repository-name>\@sha256:94afd1f2e64d908bc90dbca0035a5b567EXAMPLE@.
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
-- 'command', 'containerDefinition_command' - The command that is passed to the container. This parameter maps to
-- @Cmd@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @COMMAND@ parameter to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
-- For more information, see
-- <https://docs.docker.com/engine/reference/builder/#cmd>. If there are
-- multiple arguments, each argument should be a separated string in the
-- array.
--
-- 'logConfiguration', 'containerDefinition_logConfiguration' - The log configuration specification for the container.
--
-- This parameter maps to @LogConfig@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--log-driver@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
-- By default, containers use the same logging driver that the Docker
-- daemon uses. However the container may use a different logging driver
-- than the Docker daemon by specifying a log driver with this parameter in
-- the container definition. To use a different logging driver for a
-- container, the log system must be configured properly on the container
-- instance (or on a different log server for remote logging options). For
-- more information on the options for different supported log drivers, see
-- <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers>
-- in the Docker documentation.
--
-- Amazon ECS currently supports a subset of the logging drivers available
-- to the Docker daemon (shown in the LogConfiguration data type).
-- Additional log drivers may be available in future releases of the Amazon
-- ECS container agent.
--
-- This parameter requires version 1.18 of the Docker Remote API or greater
-- on your container instance. To check the Docker Remote API version on
-- your container instance, log in to your container instance and run the
-- following command:
-- @sudo docker version --format \'{{.Server.APIVersion}}\'@
--
-- The Amazon ECS container agent running on a container instance must
-- register the logging drivers available on that instance with the
-- @ECS_AVAILABLE_LOGGING_DRIVERS@ environment variable before containers
-- placed on that instance can use these log configuration options. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-config.html Amazon ECS Container Agent Configuration>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'pseudoTerminal', 'containerDefinition_pseudoTerminal' - When this parameter is @true@, a TTY is allocated. This parameter maps
-- to @Tty@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--tty@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- 'dnsSearchDomains', 'containerDefinition_dnsSearchDomains' - A list of DNS search domains that are presented to the container. This
-- parameter maps to @DnsSearch@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--dns-search@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- This parameter is not supported for Windows containers or tasks that use
-- the awsvpc network mode.
--
-- 'essential', 'containerDefinition_essential' - If the @essential@ parameter of a container is marked as @true@, and
-- that container fails or stops for any reason, all other containers that
-- are part of the task are stopped. If the @essential@ parameter of a
-- container is marked as @false@, then its failure does not affect the
-- rest of the containers in a task. If this parameter is omitted, a
-- container is assumed to be essential.
--
-- All tasks must have at least one essential container. If you have an
-- application that is composed of multiple containers, you should group
-- containers that are used for a common purpose into components, and
-- separate the different components into multiple task definitions. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/application_architecture.html Application Architecture>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'portMappings', 'containerDefinition_portMappings' - The list of port mappings for the container. Port mappings allow
-- containers to access ports on the host container instance to send or
-- receive traffic.
--
-- For task definitions that use the @awsvpc@ network mode, you should only
-- specify the @containerPort@. The @hostPort@ can be left blank or it must
-- be the same value as the @containerPort@.
--
-- Port mappings on Windows use the @NetNAT@ gateway address rather than
-- @localhost@. There is no loopback for port mappings on Windows, so you
-- cannot access a container\'s mapped port from the host itself.
--
-- This parameter maps to @PortBindings@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--publish@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
-- If the network mode of a task definition is set to @none@, then you
-- can\'t specify port mappings. If the network mode of a task definition
-- is set to @host@, then host ports must either be undefined or they must
-- match the container port in the port mapping.
--
-- After a task reaches the @RUNNING@ status, manual and automatic host and
-- container port assignments are visible in the __Network Bindings__
-- section of a container description for a selected task in the Amazon ECS
-- console. The assignments are also visible in the @networkBindings@
-- section DescribeTasks responses.
--
-- 'cpu', 'containerDefinition_cpu' - The number of @cpu@ units reserved for the container. This parameter
-- maps to @CpuShares@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--cpu-shares@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- This field is optional for tasks using the Fargate launch type, and the
-- only requirement is that the total amount of CPU reserved for all
-- containers within a task be lower than the task-level @cpu@ value.
--
-- You can determine the number of CPU units that are available per EC2
-- instance type by multiplying the vCPUs listed for that instance type on
-- the <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instances>
-- detail page by 1,024.
--
-- Linux containers share unallocated CPU units with other containers on
-- the container instance with the same ratio as their allocated amount.
-- For example, if you run a single-container task on a single-core
-- instance type with 512 CPU units specified for that container, and that
-- is the only task running on the container instance, that container could
-- use the full 1,024 CPU unit share at any given time. However, if you
-- launched another copy of the same task on that container instance, each
-- task would be guaranteed a minimum of 512 CPU units when needed, and
-- each container could float to higher CPU usage if the other container
-- was not using it, but if both tasks were 100% active all of the time,
-- they would be limited to 512 CPU units.
--
-- On Linux container instances, the Docker daemon on the container
-- instance uses the CPU value to calculate the relative CPU share ratios
-- for running containers. For more information, see
-- <https://docs.docker.com/engine/reference/run/#cpu-share-constraint CPU share constraint>
-- in the Docker documentation. The minimum valid CPU share value that the
-- Linux kernel allows is 2. However, the CPU parameter is not required,
-- and you can use CPU values below 2 in your container definitions. For
-- CPU values below 2 (including null), the behavior varies based on your
-- Amazon ECS container agent version:
--
-- -   __Agent versions less than or equal to 1.1.0:__ Null and zero CPU
--     values are passed to Docker as 0, which Docker then converts to
--     1,024 CPU shares. CPU values of 1 are passed to Docker as 1, which
--     the Linux kernel converts to two CPU shares.
--
-- -   __Agent versions greater than or equal to 1.2.0:__ Null, zero, and
--     CPU values of 1 are passed to Docker as 2.
--
-- On Windows container instances, the CPU limit is enforced as an absolute
-- limit, or a quota. Windows containers only have access to the specified
-- amount of CPU that is described in the task definition. A null or zero
-- CPU value is passed to Docker as @0@, which Windows interprets as 1% of
-- one CPU.
--
-- 'resourceRequirements', 'containerDefinition_resourceRequirements' - The type and amount of a resource to assign to a container. The only
-- supported resource is a GPU.
--
-- 'startTimeout', 'containerDefinition_startTimeout' - Time duration (in seconds) to wait before giving up on resolving
-- dependencies for a container. For example, you specify two containers in
-- a task definition with containerA having a dependency on containerB
-- reaching a @COMPLETE@, @SUCCESS@, or @HEALTHY@ status. If a
-- @startTimeout@ value is specified for containerB and it does not reach
-- the desired status within that time then containerA will give up and not
-- start. This results in the task transitioning to a @STOPPED@ state.
--
-- When the @ECS_CONTAINER_START_TIMEOUT@ container agent configuration
-- variable is used, it is enforced indendently from this start timeout
-- value.
--
-- For tasks using the Fargate launch type, this parameter requires that
-- the task or service uses platform version 1.3.0 or later.
--
-- For tasks using the EC2 launch type, your container instances require at
-- least version @1.26.0@ of the container agent to enable a container
-- start timeout value. However, we recommend using the latest container
-- agent version. For information about checking your agent version and
-- updating to the latest version, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent>
-- in the /Amazon Elastic Container Service Developer Guide/. If you are
-- using an Amazon ECS-optimized Linux AMI, your instance needs at least
-- version @1.26.0-1@ of the @ecs-init@ package. If your container
-- instances are launched from version @20190301@ or later, then they
-- contain the required versions of the container agent and @ecs-init@. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'readonlyRootFilesystem', 'containerDefinition_readonlyRootFilesystem' - When this parameter is true, the container is given read-only access to
-- its root file system. This parameter maps to @ReadonlyRootfs@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--read-only@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- This parameter is not supported for Windows containers or tasks that use
-- the awsvpc network mode.
--
-- 'ulimits', 'containerDefinition_ulimits' - A list of @ulimits@ to set in the container. If a ulimit value is
-- specified in a task definition, it will override the default values set
-- by Docker. This parameter maps to @Ulimits@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--ulimit@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
-- Valid naming values are displayed in the Ulimit data type. This
-- parameter requires version 1.18 of the Docker Remote API or greater on
-- your container instance. To check the Docker Remote API version on your
-- container instance, log in to your container instance and run the
-- following command:
-- @sudo docker version --format \'{{.Server.APIVersion}}\'@
--
-- This parameter is not supported for Windows containers or tasks that use
-- the awsvpc network mode.
--
-- 'stopTimeout', 'containerDefinition_stopTimeout' - Time duration (in seconds) to wait before the container is forcefully
-- killed if it doesn\'t exit normally on its own.
--
-- For tasks using the Fargate launch type, the task or service requires
-- platform version 1.3.0 or later. The max stop timeout value is 120
-- seconds and if the parameter is not specified, the default value of 30
-- seconds is used.
--
-- For tasks using the EC2 launch type, if the @stopTimeout@ parameter is
-- not specified, the value set for the Amazon ECS container agent
-- configuration variable @ECS_CONTAINER_STOP_TIMEOUT@ is used by default.
-- If neither the @stopTimeout@ parameter or the
-- @ECS_CONTAINER_STOP_TIMEOUT@ agent configuration variable are set, then
-- the default values of 30 seconds for Linux containers and 30 seconds on
-- Windows containers are used. Your container instances require at least
-- version 1.26.0 of the container agent to enable a container stop timeout
-- value. However, we recommend using the latest container agent version.
-- For information about checking your agent version and updating to the
-- latest version, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent>
-- in the /Amazon Elastic Container Service Developer Guide/. If you are
-- using an Amazon ECS-optimized Linux AMI, your instance needs at least
-- version 1.26.0-1 of the @ecs-init@ package. If your container instances
-- are launched from version @20190301@ or later, then they contain the
-- required versions of the container agent and @ecs-init@. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'dockerSecurityOptions', 'containerDefinition_dockerSecurityOptions' - A list of strings to provide custom labels for SELinux and AppArmor
-- multi-level security systems. This field is not valid for containers in
-- tasks using the Fargate launch type.
--
-- With Windows containers, this parameter can be used to reference a
-- credential spec file when configuring a container for Active Directory
-- authentication. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/windows-gmsa.html Using gMSAs for Windows Containers>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- This parameter maps to @SecurityOpt@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--security-opt@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- The Amazon ECS container agent running on a container instance must
-- register with the @ECS_SELINUX_CAPABLE=true@ or
-- @ECS_APPARMOR_CAPABLE=true@ environment variables before containers
-- placed on that instance can use these security options. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-config.html Amazon ECS Container Agent Configuration>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- For more information about valid values, see
-- <https://docs.docker.com/engine/reference/run/#security-configuration Docker Run Security Configuration>.
--
-- Valid values: \"no-new-privileges\" | \"apparmor:PROFILE\" |
-- \"label:value\" | \"credentialspec:CredentialSpecFilePath\"
--
-- 'disableNetworking', 'containerDefinition_disableNetworking' - When this parameter is true, networking is disabled within the
-- container. This parameter maps to @NetworkDisabled@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API>.
--
-- This parameter is not supported for Windows containers or tasks that use
-- the awsvpc network mode.
--
-- 'healthCheck', 'containerDefinition_healthCheck' - The container health check command and associated configuration
-- parameters for the container. This parameter maps to @HealthCheck@ in
-- the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @HEALTHCHECK@ parameter of
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- 'repositoryCredentials', 'containerDefinition_repositoryCredentials' - The private repository authentication credentials to use.
newContainerDefinition ::
  ContainerDefinition
newContainerDefinition =
  ContainerDefinition'
    { hostname = Prelude.Nothing,
      dependsOn = Prelude.Nothing,
      linuxParameters = Prelude.Nothing,
      firelensConfiguration = Prelude.Nothing,
      memory = Prelude.Nothing,
      user = Prelude.Nothing,
      memoryReservation = Prelude.Nothing,
      dockerLabels = Prelude.Nothing,
      extraHosts = Prelude.Nothing,
      systemControls = Prelude.Nothing,
      privileged = Prelude.Nothing,
      links = Prelude.Nothing,
      environmentFiles = Prelude.Nothing,
      interactive = Prelude.Nothing,
      workingDirectory = Prelude.Nothing,
      entryPoint = Prelude.Nothing,
      environment = Prelude.Nothing,
      volumesFrom = Prelude.Nothing,
      secrets = Prelude.Nothing,
      mountPoints = Prelude.Nothing,
      dnsServers = Prelude.Nothing,
      name = Prelude.Nothing,
      image = Prelude.Nothing,
      command = Prelude.Nothing,
      logConfiguration = Prelude.Nothing,
      pseudoTerminal = Prelude.Nothing,
      dnsSearchDomains = Prelude.Nothing,
      essential = Prelude.Nothing,
      portMappings = Prelude.Nothing,
      cpu = Prelude.Nothing,
      resourceRequirements = Prelude.Nothing,
      startTimeout = Prelude.Nothing,
      readonlyRootFilesystem = Prelude.Nothing,
      ulimits = Prelude.Nothing,
      stopTimeout = Prelude.Nothing,
      dockerSecurityOptions = Prelude.Nothing,
      disableNetworking = Prelude.Nothing,
      healthCheck = Prelude.Nothing,
      repositoryCredentials = Prelude.Nothing
    }

-- | The hostname to use for your container. This parameter maps to
-- @Hostname@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--hostname@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- The @hostname@ parameter is not supported if you are using the @awsvpc@
-- network mode.
containerDefinition_hostname :: Lens.Lens' ContainerDefinition (Prelude.Maybe Prelude.Text)
containerDefinition_hostname = Lens.lens (\ContainerDefinition' {hostname} -> hostname) (\s@ContainerDefinition' {} a -> s {hostname = a} :: ContainerDefinition)

-- | The dependencies defined for container startup and shutdown. A container
-- can contain multiple dependencies. When a dependency is defined for
-- container startup, for container shutdown it is reversed.
--
-- For tasks using the EC2 launch type, the container instances require at
-- least version 1.26.0 of the container agent to enable container
-- dependencies. However, we recommend using the latest container agent
-- version. For information about checking your agent version and updating
-- to the latest version, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent>
-- in the /Amazon Elastic Container Service Developer Guide/. If you are
-- using an Amazon ECS-optimized Linux AMI, your instance needs at least
-- version 1.26.0-1 of the @ecs-init@ package. If your container instances
-- are launched from version @20190301@ or later, then they contain the
-- required versions of the container agent and @ecs-init@. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- For tasks using the Fargate launch type, the task or service requires
-- platform version @1.3.0@ or later.
containerDefinition_dependsOn :: Lens.Lens' ContainerDefinition (Prelude.Maybe [ContainerDependency])
containerDefinition_dependsOn = Lens.lens (\ContainerDefinition' {dependsOn} -> dependsOn) (\s@ContainerDefinition' {} a -> s {dependsOn = a} :: ContainerDefinition) Prelude.. Lens.mapping Lens._Coerce

-- | Linux-specific modifications that are applied to the container, such as
-- Linux kernel capabilities. For more information see KernelCapabilities.
--
-- This parameter is not supported for Windows containers.
containerDefinition_linuxParameters :: Lens.Lens' ContainerDefinition (Prelude.Maybe LinuxParameters)
containerDefinition_linuxParameters = Lens.lens (\ContainerDefinition' {linuxParameters} -> linuxParameters) (\s@ContainerDefinition' {} a -> s {linuxParameters = a} :: ContainerDefinition)

-- | The FireLens configuration for the container. This is used to specify
-- and configure a log router for container logs. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html Custom Log Routing>
-- in the /Amazon Elastic Container Service Developer Guide/.
containerDefinition_firelensConfiguration :: Lens.Lens' ContainerDefinition (Prelude.Maybe FirelensConfiguration)
containerDefinition_firelensConfiguration = Lens.lens (\ContainerDefinition' {firelensConfiguration} -> firelensConfiguration) (\s@ContainerDefinition' {} a -> s {firelensConfiguration = a} :: ContainerDefinition)

-- | The amount (in MiB) of memory to present to the container. If your
-- container attempts to exceed the memory specified here, the container is
-- killed. The total amount of memory reserved for all containers within a
-- task must be lower than the task @memory@ value, if one is specified.
-- This parameter maps to @Memory@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--memory@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- If using the Fargate launch type, this parameter is optional.
--
-- If using the EC2 launch type, you must specify either a task-level
-- memory value or a container-level memory value. If you specify both a
-- container-level @memory@ and @memoryReservation@ value, @memory@ must be
-- greater than @memoryReservation@. If you specify @memoryReservation@,
-- then that value is subtracted from the available memory resources for
-- the container instance on which the container is placed. Otherwise, the
-- value of @memory@ is used.
--
-- The Docker daemon reserves a minimum of 4 MiB of memory for a container,
-- so you should not specify fewer than 4 MiB of memory for your
-- containers.
containerDefinition_memory :: Lens.Lens' ContainerDefinition (Prelude.Maybe Prelude.Int)
containerDefinition_memory = Lens.lens (\ContainerDefinition' {memory} -> memory) (\s@ContainerDefinition' {} a -> s {memory = a} :: ContainerDefinition)

-- | The user to use inside the container. This parameter maps to @User@ in
-- the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--user@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- When running tasks using the @host@ network mode, you should not run
-- containers using the root user (UID 0). It is considered best practice
-- to use a non-root user.
--
-- You can specify the @user@ using the following formats. If specifying a
-- UID or GID, you must specify it as a positive integer.
--
-- -   @user@
--
-- -   @user:group@
--
-- -   @uid@
--
-- -   @uid:gid@
--
-- -   @user:gid@
--
-- -   @uid:group@
--
-- This parameter is not supported for Windows containers or tasks that use
-- the awsvpc network mode.
containerDefinition_user :: Lens.Lens' ContainerDefinition (Prelude.Maybe Prelude.Text)
containerDefinition_user = Lens.lens (\ContainerDefinition' {user} -> user) (\s@ContainerDefinition' {} a -> s {user = a} :: ContainerDefinition)

-- | The soft limit (in MiB) of memory to reserve for the container. When
-- system memory is under heavy contention, Docker attempts to keep the
-- container memory to this soft limit. However, your container can consume
-- more memory when it needs to, up to either the hard limit specified with
-- the @memory@ parameter (if applicable), or all of the available memory
-- on the container instance, whichever comes first. This parameter maps to
-- @MemoryReservation@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--memory-reservation@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- If a task-level memory value is not specified, you must specify a
-- non-zero integer for one or both of @memory@ or @memoryReservation@ in a
-- container definition. If you specify both, @memory@ must be greater than
-- @memoryReservation@. If you specify @memoryReservation@, then that value
-- is subtracted from the available memory resources for the container
-- instance on which the container is placed. Otherwise, the value of
-- @memory@ is used.
--
-- For example, if your container normally uses 128 MiB of memory, but
-- occasionally bursts to 256 MiB of memory for short periods of time, you
-- can set a @memoryReservation@ of 128 MiB, and a @memory@ hard limit of
-- 300 MiB. This configuration would allow the container to only reserve
-- 128 MiB of memory from the remaining resources on the container
-- instance, but also allow the container to consume more memory resources
-- when needed.
--
-- The Docker daemon reserves a minimum of 4 MiB of memory for a container,
-- so you should not specify fewer than 4 MiB of memory for your
-- containers.
containerDefinition_memoryReservation :: Lens.Lens' ContainerDefinition (Prelude.Maybe Prelude.Int)
containerDefinition_memoryReservation = Lens.lens (\ContainerDefinition' {memoryReservation} -> memoryReservation) (\s@ContainerDefinition' {} a -> s {memoryReservation = a} :: ContainerDefinition)

-- | A key\/value map of labels to add to the container. This parameter maps
-- to @Labels@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--label@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
-- This parameter requires version 1.18 of the Docker Remote API or greater
-- on your container instance. To check the Docker Remote API version on
-- your container instance, log in to your container instance and run the
-- following command:
-- @sudo docker version --format \'{{.Server.APIVersion}}\'@
containerDefinition_dockerLabels :: Lens.Lens' ContainerDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
containerDefinition_dockerLabels = Lens.lens (\ContainerDefinition' {dockerLabels} -> dockerLabels) (\s@ContainerDefinition' {} a -> s {dockerLabels = a} :: ContainerDefinition) Prelude.. Lens.mapping Lens._Coerce

-- | A list of hostnames and IP address mappings to append to the
-- @\/etc\/hosts@ file on the container. This parameter maps to
-- @ExtraHosts@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--add-host@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- This parameter is not supported for Windows containers or tasks that use
-- the @awsvpc@ network mode.
containerDefinition_extraHosts :: Lens.Lens' ContainerDefinition (Prelude.Maybe [HostEntry])
containerDefinition_extraHosts = Lens.lens (\ContainerDefinition' {extraHosts} -> extraHosts) (\s@ContainerDefinition' {} a -> s {extraHosts = a} :: ContainerDefinition) Prelude.. Lens.mapping Lens._Coerce

-- | A list of namespaced kernel parameters to set in the container. This
-- parameter maps to @Sysctls@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--sysctl@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- It is not recommended that you specify network-related @systemControls@
-- parameters for multiple containers in a single task that also uses
-- either the @awsvpc@ or @host@ network modes. For tasks that use the
-- @awsvpc@ network mode, the container that is started last determines
-- which @systemControls@ parameters take effect. For tasks that use the
-- @host@ network mode, it changes the container instance\'s namespaced
-- kernel parameters as well as the containers.
containerDefinition_systemControls :: Lens.Lens' ContainerDefinition (Prelude.Maybe [SystemControl])
containerDefinition_systemControls = Lens.lens (\ContainerDefinition' {systemControls} -> systemControls) (\s@ContainerDefinition' {} a -> s {systemControls = a} :: ContainerDefinition) Prelude.. Lens.mapping Lens._Coerce

-- | When this parameter is true, the container is given elevated privileges
-- on the host container instance (similar to the @root@ user). This
-- parameter maps to @Privileged@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--privileged@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- This parameter is not supported for Windows containers or tasks using
-- the Fargate launch type.
containerDefinition_privileged :: Lens.Lens' ContainerDefinition (Prelude.Maybe Prelude.Bool)
containerDefinition_privileged = Lens.lens (\ContainerDefinition' {privileged} -> privileged) (\s@ContainerDefinition' {} a -> s {privileged = a} :: ContainerDefinition)

-- | The @links@ parameter allows containers to communicate with each other
-- without the need for port mappings. This parameter is only supported if
-- the network mode of a task definition is @bridge@. The
-- @name:internalName@ construct is analogous to @name:alias@ in Docker
-- links. Up to 255 letters (uppercase and lowercase), numbers, and hyphens
-- are allowed. For more information about linking Docker containers, go to
-- <https://docs.docker.com/network/links/ Legacy container links> in the
-- Docker documentation. This parameter maps to @Links@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--link@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- This parameter is not supported for Windows containers or tasks that use
-- the awsvpc network mode.
--
-- Containers that are collocated on a single container instance may be
-- able to communicate with each other without requiring links or host port
-- mappings. Network isolation is achieved on the container instance using
-- security groups and VPC settings.
containerDefinition_links :: Lens.Lens' ContainerDefinition (Prelude.Maybe [Prelude.Text])
containerDefinition_links = Lens.lens (\ContainerDefinition' {links} -> links) (\s@ContainerDefinition' {} a -> s {links = a} :: ContainerDefinition) Prelude.. Lens.mapping Lens._Coerce

-- | A list of files containing the environment variables to pass to a
-- container. This parameter maps to the @--env-file@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- You can specify up to ten environment files. The file must have a @.env@
-- file extension. Each line in an environment file should contain an
-- environment variable in @VARIABLE=VALUE@ format. Lines beginning with
-- @#@ are treated as comments and are ignored. For more information on the
-- environment variable file syntax, see
-- <https://docs.docker.com/compose/env-file/ Declare default environment variables in file>.
--
-- If there are environment variables specified using the @environment@
-- parameter in a container definition, they take precedence over the
-- variables contained within an environment file. If multiple environment
-- files are specified that contain the same variable, they are processed
-- from the top down. It is recommended to use unique variable names. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/taskdef-envfiles.html Specifying Environment Variables>
-- in the /Amazon Elastic Container Service Developer Guide/.
containerDefinition_environmentFiles :: Lens.Lens' ContainerDefinition (Prelude.Maybe [EnvironmentFile])
containerDefinition_environmentFiles = Lens.lens (\ContainerDefinition' {environmentFiles} -> environmentFiles) (\s@ContainerDefinition' {} a -> s {environmentFiles = a} :: ContainerDefinition) Prelude.. Lens.mapping Lens._Coerce

-- | When this parameter is @true@, this allows you to deploy containerized
-- applications that require @stdin@ or a @tty@ to be allocated. This
-- parameter maps to @OpenStdin@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--interactive@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
containerDefinition_interactive :: Lens.Lens' ContainerDefinition (Prelude.Maybe Prelude.Bool)
containerDefinition_interactive = Lens.lens (\ContainerDefinition' {interactive} -> interactive) (\s@ContainerDefinition' {} a -> s {interactive = a} :: ContainerDefinition)

-- | The working directory in which to run commands inside the container.
-- This parameter maps to @WorkingDir@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--workdir@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
containerDefinition_workingDirectory :: Lens.Lens' ContainerDefinition (Prelude.Maybe Prelude.Text)
containerDefinition_workingDirectory = Lens.lens (\ContainerDefinition' {workingDirectory} -> workingDirectory) (\s@ContainerDefinition' {} a -> s {workingDirectory = a} :: ContainerDefinition)

-- | Early versions of the Amazon ECS container agent do not properly handle
-- @entryPoint@ parameters. If you have problems using @entryPoint@, update
-- your container agent or enter your commands and arguments as @command@
-- array items instead.
--
-- The entry point that is passed to the container. This parameter maps to
-- @Entrypoint@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--entrypoint@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
-- For more information, see
-- <https://docs.docker.com/engine/reference/builder/#entrypoint>.
containerDefinition_entryPoint :: Lens.Lens' ContainerDefinition (Prelude.Maybe [Prelude.Text])
containerDefinition_entryPoint = Lens.lens (\ContainerDefinition' {entryPoint} -> entryPoint) (\s@ContainerDefinition' {} a -> s {entryPoint = a} :: ContainerDefinition) Prelude.. Lens.mapping Lens._Coerce

-- | The environment variables to pass to a container. This parameter maps to
-- @Env@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--env@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- We do not recommend using plaintext environment variables for sensitive
-- information, such as credential data.
containerDefinition_environment :: Lens.Lens' ContainerDefinition (Prelude.Maybe [KeyValuePair])
containerDefinition_environment = Lens.lens (\ContainerDefinition' {environment} -> environment) (\s@ContainerDefinition' {} a -> s {environment = a} :: ContainerDefinition) Prelude.. Lens.mapping Lens._Coerce

-- | Data volumes to mount from another container. This parameter maps to
-- @VolumesFrom@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--volumes-from@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
containerDefinition_volumesFrom :: Lens.Lens' ContainerDefinition (Prelude.Maybe [VolumeFrom])
containerDefinition_volumesFrom = Lens.lens (\ContainerDefinition' {volumesFrom} -> volumesFrom) (\s@ContainerDefinition' {} a -> s {volumesFrom = a} :: ContainerDefinition) Prelude.. Lens.mapping Lens._Coerce

-- | The secrets to pass to the container. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html Specifying Sensitive Data>
-- in the /Amazon Elastic Container Service Developer Guide/.
containerDefinition_secrets :: Lens.Lens' ContainerDefinition (Prelude.Maybe [Secret])
containerDefinition_secrets = Lens.lens (\ContainerDefinition' {secrets} -> secrets) (\s@ContainerDefinition' {} a -> s {secrets = a} :: ContainerDefinition) Prelude.. Lens.mapping Lens._Coerce

-- | The mount points for data volumes in your container.
--
-- This parameter maps to @Volumes@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--volume@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- Windows containers can mount whole directories on the same drive as
-- @$env:ProgramData@. Windows containers cannot mount directories on a
-- different drive, and mount point cannot be across drives.
containerDefinition_mountPoints :: Lens.Lens' ContainerDefinition (Prelude.Maybe [MountPoint])
containerDefinition_mountPoints = Lens.lens (\ContainerDefinition' {mountPoints} -> mountPoints) (\s@ContainerDefinition' {} a -> s {mountPoints = a} :: ContainerDefinition) Prelude.. Lens.mapping Lens._Coerce

-- | A list of DNS servers that are presented to the container. This
-- parameter maps to @Dns@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--dns@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- This parameter is not supported for Windows containers or tasks that use
-- the awsvpc network mode.
containerDefinition_dnsServers :: Lens.Lens' ContainerDefinition (Prelude.Maybe [Prelude.Text])
containerDefinition_dnsServers = Lens.lens (\ContainerDefinition' {dnsServers} -> dnsServers) (\s@ContainerDefinition' {} a -> s {dnsServers = a} :: ContainerDefinition) Prelude.. Lens.mapping Lens._Coerce

-- | The name of a container. If you are linking multiple containers together
-- in a task definition, the @name@ of one container can be entered in the
-- @links@ of another container to connect the containers. Up to 255
-- letters (uppercase and lowercase), numbers, and hyphens are allowed.
-- This parameter maps to @name@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--name@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
containerDefinition_name :: Lens.Lens' ContainerDefinition (Prelude.Maybe Prelude.Text)
containerDefinition_name = Lens.lens (\ContainerDefinition' {name} -> name) (\s@ContainerDefinition' {} a -> s {name = a} :: ContainerDefinition)

-- | The image used to start a container. This string is passed directly to
-- the Docker daemon. Images in the Docker Hub registry are available by
-- default. Other repositories are specified with either
-- @ repository-url\/image:tag @ or @ repository-url\/image\@digest @. Up
-- to 255 letters (uppercase and lowercase), numbers, hyphens, underscores,
-- colons, periods, forward slashes, and number signs are allowed. This
-- parameter maps to @Image@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @IMAGE@ parameter of
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- -   When a new task starts, the Amazon ECS container agent pulls the
--     latest version of the specified image and tag for the container to
--     use. However, subsequent updates to a repository image are not
--     propagated to already running tasks.
--
-- -   Images in Amazon ECR repositories can be specified by either using
--     the full @registry\/repository:tag@ or
--     @registry\/repository\@digest@. For example,
--     @012345678910.dkr.ecr.\<region-name>.amazonaws.com\/\<repository-name>:latest@
--     or
--     @012345678910.dkr.ecr.\<region-name>.amazonaws.com\/\<repository-name>\@sha256:94afd1f2e64d908bc90dbca0035a5b567EXAMPLE@.
--
-- -   Images in official repositories on Docker Hub use a single name (for
--     example, @ubuntu@ or @mongo@).
--
-- -   Images in other repositories on Docker Hub are qualified with an
--     organization name (for example, @amazon\/amazon-ecs-agent@).
--
-- -   Images in other online repositories are qualified further by a
--     domain name (for example, @quay.io\/assemblyline\/ubuntu@).
containerDefinition_image :: Lens.Lens' ContainerDefinition (Prelude.Maybe Prelude.Text)
containerDefinition_image = Lens.lens (\ContainerDefinition' {image} -> image) (\s@ContainerDefinition' {} a -> s {image = a} :: ContainerDefinition)

-- | The command that is passed to the container. This parameter maps to
-- @Cmd@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @COMMAND@ parameter to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
-- For more information, see
-- <https://docs.docker.com/engine/reference/builder/#cmd>. If there are
-- multiple arguments, each argument should be a separated string in the
-- array.
containerDefinition_command :: Lens.Lens' ContainerDefinition (Prelude.Maybe [Prelude.Text])
containerDefinition_command = Lens.lens (\ContainerDefinition' {command} -> command) (\s@ContainerDefinition' {} a -> s {command = a} :: ContainerDefinition) Prelude.. Lens.mapping Lens._Coerce

-- | The log configuration specification for the container.
--
-- This parameter maps to @LogConfig@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--log-driver@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
-- By default, containers use the same logging driver that the Docker
-- daemon uses. However the container may use a different logging driver
-- than the Docker daemon by specifying a log driver with this parameter in
-- the container definition. To use a different logging driver for a
-- container, the log system must be configured properly on the container
-- instance (or on a different log server for remote logging options). For
-- more information on the options for different supported log drivers, see
-- <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers>
-- in the Docker documentation.
--
-- Amazon ECS currently supports a subset of the logging drivers available
-- to the Docker daemon (shown in the LogConfiguration data type).
-- Additional log drivers may be available in future releases of the Amazon
-- ECS container agent.
--
-- This parameter requires version 1.18 of the Docker Remote API or greater
-- on your container instance. To check the Docker Remote API version on
-- your container instance, log in to your container instance and run the
-- following command:
-- @sudo docker version --format \'{{.Server.APIVersion}}\'@
--
-- The Amazon ECS container agent running on a container instance must
-- register the logging drivers available on that instance with the
-- @ECS_AVAILABLE_LOGGING_DRIVERS@ environment variable before containers
-- placed on that instance can use these log configuration options. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-config.html Amazon ECS Container Agent Configuration>
-- in the /Amazon Elastic Container Service Developer Guide/.
containerDefinition_logConfiguration :: Lens.Lens' ContainerDefinition (Prelude.Maybe LogConfiguration)
containerDefinition_logConfiguration = Lens.lens (\ContainerDefinition' {logConfiguration} -> logConfiguration) (\s@ContainerDefinition' {} a -> s {logConfiguration = a} :: ContainerDefinition)

-- | When this parameter is @true@, a TTY is allocated. This parameter maps
-- to @Tty@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--tty@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
containerDefinition_pseudoTerminal :: Lens.Lens' ContainerDefinition (Prelude.Maybe Prelude.Bool)
containerDefinition_pseudoTerminal = Lens.lens (\ContainerDefinition' {pseudoTerminal} -> pseudoTerminal) (\s@ContainerDefinition' {} a -> s {pseudoTerminal = a} :: ContainerDefinition)

-- | A list of DNS search domains that are presented to the container. This
-- parameter maps to @DnsSearch@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--dns-search@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- This parameter is not supported for Windows containers or tasks that use
-- the awsvpc network mode.
containerDefinition_dnsSearchDomains :: Lens.Lens' ContainerDefinition (Prelude.Maybe [Prelude.Text])
containerDefinition_dnsSearchDomains = Lens.lens (\ContainerDefinition' {dnsSearchDomains} -> dnsSearchDomains) (\s@ContainerDefinition' {} a -> s {dnsSearchDomains = a} :: ContainerDefinition) Prelude.. Lens.mapping Lens._Coerce

-- | If the @essential@ parameter of a container is marked as @true@, and
-- that container fails or stops for any reason, all other containers that
-- are part of the task are stopped. If the @essential@ parameter of a
-- container is marked as @false@, then its failure does not affect the
-- rest of the containers in a task. If this parameter is omitted, a
-- container is assumed to be essential.
--
-- All tasks must have at least one essential container. If you have an
-- application that is composed of multiple containers, you should group
-- containers that are used for a common purpose into components, and
-- separate the different components into multiple task definitions. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/application_architecture.html Application Architecture>
-- in the /Amazon Elastic Container Service Developer Guide/.
containerDefinition_essential :: Lens.Lens' ContainerDefinition (Prelude.Maybe Prelude.Bool)
containerDefinition_essential = Lens.lens (\ContainerDefinition' {essential} -> essential) (\s@ContainerDefinition' {} a -> s {essential = a} :: ContainerDefinition)

-- | The list of port mappings for the container. Port mappings allow
-- containers to access ports on the host container instance to send or
-- receive traffic.
--
-- For task definitions that use the @awsvpc@ network mode, you should only
-- specify the @containerPort@. The @hostPort@ can be left blank or it must
-- be the same value as the @containerPort@.
--
-- Port mappings on Windows use the @NetNAT@ gateway address rather than
-- @localhost@. There is no loopback for port mappings on Windows, so you
-- cannot access a container\'s mapped port from the host itself.
--
-- This parameter maps to @PortBindings@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--publish@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
-- If the network mode of a task definition is set to @none@, then you
-- can\'t specify port mappings. If the network mode of a task definition
-- is set to @host@, then host ports must either be undefined or they must
-- match the container port in the port mapping.
--
-- After a task reaches the @RUNNING@ status, manual and automatic host and
-- container port assignments are visible in the __Network Bindings__
-- section of a container description for a selected task in the Amazon ECS
-- console. The assignments are also visible in the @networkBindings@
-- section DescribeTasks responses.
containerDefinition_portMappings :: Lens.Lens' ContainerDefinition (Prelude.Maybe [PortMapping])
containerDefinition_portMappings = Lens.lens (\ContainerDefinition' {portMappings} -> portMappings) (\s@ContainerDefinition' {} a -> s {portMappings = a} :: ContainerDefinition) Prelude.. Lens.mapping Lens._Coerce

-- | The number of @cpu@ units reserved for the container. This parameter
-- maps to @CpuShares@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--cpu-shares@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- This field is optional for tasks using the Fargate launch type, and the
-- only requirement is that the total amount of CPU reserved for all
-- containers within a task be lower than the task-level @cpu@ value.
--
-- You can determine the number of CPU units that are available per EC2
-- instance type by multiplying the vCPUs listed for that instance type on
-- the <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instances>
-- detail page by 1,024.
--
-- Linux containers share unallocated CPU units with other containers on
-- the container instance with the same ratio as their allocated amount.
-- For example, if you run a single-container task on a single-core
-- instance type with 512 CPU units specified for that container, and that
-- is the only task running on the container instance, that container could
-- use the full 1,024 CPU unit share at any given time. However, if you
-- launched another copy of the same task on that container instance, each
-- task would be guaranteed a minimum of 512 CPU units when needed, and
-- each container could float to higher CPU usage if the other container
-- was not using it, but if both tasks were 100% active all of the time,
-- they would be limited to 512 CPU units.
--
-- On Linux container instances, the Docker daemon on the container
-- instance uses the CPU value to calculate the relative CPU share ratios
-- for running containers. For more information, see
-- <https://docs.docker.com/engine/reference/run/#cpu-share-constraint CPU share constraint>
-- in the Docker documentation. The minimum valid CPU share value that the
-- Linux kernel allows is 2. However, the CPU parameter is not required,
-- and you can use CPU values below 2 in your container definitions. For
-- CPU values below 2 (including null), the behavior varies based on your
-- Amazon ECS container agent version:
--
-- -   __Agent versions less than or equal to 1.1.0:__ Null and zero CPU
--     values are passed to Docker as 0, which Docker then converts to
--     1,024 CPU shares. CPU values of 1 are passed to Docker as 1, which
--     the Linux kernel converts to two CPU shares.
--
-- -   __Agent versions greater than or equal to 1.2.0:__ Null, zero, and
--     CPU values of 1 are passed to Docker as 2.
--
-- On Windows container instances, the CPU limit is enforced as an absolute
-- limit, or a quota. Windows containers only have access to the specified
-- amount of CPU that is described in the task definition. A null or zero
-- CPU value is passed to Docker as @0@, which Windows interprets as 1% of
-- one CPU.
containerDefinition_cpu :: Lens.Lens' ContainerDefinition (Prelude.Maybe Prelude.Int)
containerDefinition_cpu = Lens.lens (\ContainerDefinition' {cpu} -> cpu) (\s@ContainerDefinition' {} a -> s {cpu = a} :: ContainerDefinition)

-- | The type and amount of a resource to assign to a container. The only
-- supported resource is a GPU.
containerDefinition_resourceRequirements :: Lens.Lens' ContainerDefinition (Prelude.Maybe [ResourceRequirement])
containerDefinition_resourceRequirements = Lens.lens (\ContainerDefinition' {resourceRequirements} -> resourceRequirements) (\s@ContainerDefinition' {} a -> s {resourceRequirements = a} :: ContainerDefinition) Prelude.. Lens.mapping Lens._Coerce

-- | Time duration (in seconds) to wait before giving up on resolving
-- dependencies for a container. For example, you specify two containers in
-- a task definition with containerA having a dependency on containerB
-- reaching a @COMPLETE@, @SUCCESS@, or @HEALTHY@ status. If a
-- @startTimeout@ value is specified for containerB and it does not reach
-- the desired status within that time then containerA will give up and not
-- start. This results in the task transitioning to a @STOPPED@ state.
--
-- When the @ECS_CONTAINER_START_TIMEOUT@ container agent configuration
-- variable is used, it is enforced indendently from this start timeout
-- value.
--
-- For tasks using the Fargate launch type, this parameter requires that
-- the task or service uses platform version 1.3.0 or later.
--
-- For tasks using the EC2 launch type, your container instances require at
-- least version @1.26.0@ of the container agent to enable a container
-- start timeout value. However, we recommend using the latest container
-- agent version. For information about checking your agent version and
-- updating to the latest version, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent>
-- in the /Amazon Elastic Container Service Developer Guide/. If you are
-- using an Amazon ECS-optimized Linux AMI, your instance needs at least
-- version @1.26.0-1@ of the @ecs-init@ package. If your container
-- instances are launched from version @20190301@ or later, then they
-- contain the required versions of the container agent and @ecs-init@. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI>
-- in the /Amazon Elastic Container Service Developer Guide/.
containerDefinition_startTimeout :: Lens.Lens' ContainerDefinition (Prelude.Maybe Prelude.Int)
containerDefinition_startTimeout = Lens.lens (\ContainerDefinition' {startTimeout} -> startTimeout) (\s@ContainerDefinition' {} a -> s {startTimeout = a} :: ContainerDefinition)

-- | When this parameter is true, the container is given read-only access to
-- its root file system. This parameter maps to @ReadonlyRootfs@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--read-only@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- This parameter is not supported for Windows containers or tasks that use
-- the awsvpc network mode.
containerDefinition_readonlyRootFilesystem :: Lens.Lens' ContainerDefinition (Prelude.Maybe Prelude.Bool)
containerDefinition_readonlyRootFilesystem = Lens.lens (\ContainerDefinition' {readonlyRootFilesystem} -> readonlyRootFilesystem) (\s@ContainerDefinition' {} a -> s {readonlyRootFilesystem = a} :: ContainerDefinition)

-- | A list of @ulimits@ to set in the container. If a ulimit value is
-- specified in a task definition, it will override the default values set
-- by Docker. This parameter maps to @Ulimits@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--ulimit@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
-- Valid naming values are displayed in the Ulimit data type. This
-- parameter requires version 1.18 of the Docker Remote API or greater on
-- your container instance. To check the Docker Remote API version on your
-- container instance, log in to your container instance and run the
-- following command:
-- @sudo docker version --format \'{{.Server.APIVersion}}\'@
--
-- This parameter is not supported for Windows containers or tasks that use
-- the awsvpc network mode.
containerDefinition_ulimits :: Lens.Lens' ContainerDefinition (Prelude.Maybe [Ulimit])
containerDefinition_ulimits = Lens.lens (\ContainerDefinition' {ulimits} -> ulimits) (\s@ContainerDefinition' {} a -> s {ulimits = a} :: ContainerDefinition) Prelude.. Lens.mapping Lens._Coerce

-- | Time duration (in seconds) to wait before the container is forcefully
-- killed if it doesn\'t exit normally on its own.
--
-- For tasks using the Fargate launch type, the task or service requires
-- platform version 1.3.0 or later. The max stop timeout value is 120
-- seconds and if the parameter is not specified, the default value of 30
-- seconds is used.
--
-- For tasks using the EC2 launch type, if the @stopTimeout@ parameter is
-- not specified, the value set for the Amazon ECS container agent
-- configuration variable @ECS_CONTAINER_STOP_TIMEOUT@ is used by default.
-- If neither the @stopTimeout@ parameter or the
-- @ECS_CONTAINER_STOP_TIMEOUT@ agent configuration variable are set, then
-- the default values of 30 seconds for Linux containers and 30 seconds on
-- Windows containers are used. Your container instances require at least
-- version 1.26.0 of the container agent to enable a container stop timeout
-- value. However, we recommend using the latest container agent version.
-- For information about checking your agent version and updating to the
-- latest version, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent>
-- in the /Amazon Elastic Container Service Developer Guide/. If you are
-- using an Amazon ECS-optimized Linux AMI, your instance needs at least
-- version 1.26.0-1 of the @ecs-init@ package. If your container instances
-- are launched from version @20190301@ or later, then they contain the
-- required versions of the container agent and @ecs-init@. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI>
-- in the /Amazon Elastic Container Service Developer Guide/.
containerDefinition_stopTimeout :: Lens.Lens' ContainerDefinition (Prelude.Maybe Prelude.Int)
containerDefinition_stopTimeout = Lens.lens (\ContainerDefinition' {stopTimeout} -> stopTimeout) (\s@ContainerDefinition' {} a -> s {stopTimeout = a} :: ContainerDefinition)

-- | A list of strings to provide custom labels for SELinux and AppArmor
-- multi-level security systems. This field is not valid for containers in
-- tasks using the Fargate launch type.
--
-- With Windows containers, this parameter can be used to reference a
-- credential spec file when configuring a container for Active Directory
-- authentication. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/windows-gmsa.html Using gMSAs for Windows Containers>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- This parameter maps to @SecurityOpt@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--security-opt@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- The Amazon ECS container agent running on a container instance must
-- register with the @ECS_SELINUX_CAPABLE=true@ or
-- @ECS_APPARMOR_CAPABLE=true@ environment variables before containers
-- placed on that instance can use these security options. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-config.html Amazon ECS Container Agent Configuration>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- For more information about valid values, see
-- <https://docs.docker.com/engine/reference/run/#security-configuration Docker Run Security Configuration>.
--
-- Valid values: \"no-new-privileges\" | \"apparmor:PROFILE\" |
-- \"label:value\" | \"credentialspec:CredentialSpecFilePath\"
containerDefinition_dockerSecurityOptions :: Lens.Lens' ContainerDefinition (Prelude.Maybe [Prelude.Text])
containerDefinition_dockerSecurityOptions = Lens.lens (\ContainerDefinition' {dockerSecurityOptions} -> dockerSecurityOptions) (\s@ContainerDefinition' {} a -> s {dockerSecurityOptions = a} :: ContainerDefinition) Prelude.. Lens.mapping Lens._Coerce

-- | When this parameter is true, networking is disabled within the
-- container. This parameter maps to @NetworkDisabled@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API>.
--
-- This parameter is not supported for Windows containers or tasks that use
-- the awsvpc network mode.
containerDefinition_disableNetworking :: Lens.Lens' ContainerDefinition (Prelude.Maybe Prelude.Bool)
containerDefinition_disableNetworking = Lens.lens (\ContainerDefinition' {disableNetworking} -> disableNetworking) (\s@ContainerDefinition' {} a -> s {disableNetworking = a} :: ContainerDefinition)

-- | The container health check command and associated configuration
-- parameters for the container. This parameter maps to @HealthCheck@ in
-- the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @HEALTHCHECK@ parameter of
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
containerDefinition_healthCheck :: Lens.Lens' ContainerDefinition (Prelude.Maybe HealthCheck)
containerDefinition_healthCheck = Lens.lens (\ContainerDefinition' {healthCheck} -> healthCheck) (\s@ContainerDefinition' {} a -> s {healthCheck = a} :: ContainerDefinition)

-- | The private repository authentication credentials to use.
containerDefinition_repositoryCredentials :: Lens.Lens' ContainerDefinition (Prelude.Maybe RepositoryCredentials)
containerDefinition_repositoryCredentials = Lens.lens (\ContainerDefinition' {repositoryCredentials} -> repositoryCredentials) (\s@ContainerDefinition' {} a -> s {repositoryCredentials = a} :: ContainerDefinition)

instance Core.FromJSON ContainerDefinition where
  parseJSON =
    Core.withObject
      "ContainerDefinition"
      ( \x ->
          ContainerDefinition'
            Prelude.<$> (x Core..:? "hostname")
            Prelude.<*> (x Core..:? "dependsOn" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "linuxParameters")
            Prelude.<*> (x Core..:? "firelensConfiguration")
            Prelude.<*> (x Core..:? "memory")
            Prelude.<*> (x Core..:? "user")
            Prelude.<*> (x Core..:? "memoryReservation")
            Prelude.<*> (x Core..:? "dockerLabels" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "extraHosts" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "systemControls" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "privileged")
            Prelude.<*> (x Core..:? "links" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "environmentFiles"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "interactive")
            Prelude.<*> (x Core..:? "workingDirectory")
            Prelude.<*> (x Core..:? "entryPoint" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "environment" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "volumesFrom" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "secrets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "mountPoints" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "dnsServers" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "image")
            Prelude.<*> (x Core..:? "command" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "logConfiguration")
            Prelude.<*> (x Core..:? "pseudoTerminal")
            Prelude.<*> ( x Core..:? "dnsSearchDomains"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "essential")
            Prelude.<*> (x Core..:? "portMappings" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "cpu")
            Prelude.<*> ( x Core..:? "resourceRequirements"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "startTimeout")
            Prelude.<*> (x Core..:? "readonlyRootFilesystem")
            Prelude.<*> (x Core..:? "ulimits" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "stopTimeout")
            Prelude.<*> ( x Core..:? "dockerSecurityOptions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "disableNetworking")
            Prelude.<*> (x Core..:? "healthCheck")
            Prelude.<*> (x Core..:? "repositoryCredentials")
      )

instance Prelude.Hashable ContainerDefinition

instance Prelude.NFData ContainerDefinition

instance Core.ToJSON ContainerDefinition where
  toJSON ContainerDefinition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("hostname" Core..=) Prelude.<$> hostname,
            ("dependsOn" Core..=) Prelude.<$> dependsOn,
            ("linuxParameters" Core..=)
              Prelude.<$> linuxParameters,
            ("firelensConfiguration" Core..=)
              Prelude.<$> firelensConfiguration,
            ("memory" Core..=) Prelude.<$> memory,
            ("user" Core..=) Prelude.<$> user,
            ("memoryReservation" Core..=)
              Prelude.<$> memoryReservation,
            ("dockerLabels" Core..=) Prelude.<$> dockerLabels,
            ("extraHosts" Core..=) Prelude.<$> extraHosts,
            ("systemControls" Core..=)
              Prelude.<$> systemControls,
            ("privileged" Core..=) Prelude.<$> privileged,
            ("links" Core..=) Prelude.<$> links,
            ("environmentFiles" Core..=)
              Prelude.<$> environmentFiles,
            ("interactive" Core..=) Prelude.<$> interactive,
            ("workingDirectory" Core..=)
              Prelude.<$> workingDirectory,
            ("entryPoint" Core..=) Prelude.<$> entryPoint,
            ("environment" Core..=) Prelude.<$> environment,
            ("volumesFrom" Core..=) Prelude.<$> volumesFrom,
            ("secrets" Core..=) Prelude.<$> secrets,
            ("mountPoints" Core..=) Prelude.<$> mountPoints,
            ("dnsServers" Core..=) Prelude.<$> dnsServers,
            ("name" Core..=) Prelude.<$> name,
            ("image" Core..=) Prelude.<$> image,
            ("command" Core..=) Prelude.<$> command,
            ("logConfiguration" Core..=)
              Prelude.<$> logConfiguration,
            ("pseudoTerminal" Core..=)
              Prelude.<$> pseudoTerminal,
            ("dnsSearchDomains" Core..=)
              Prelude.<$> dnsSearchDomains,
            ("essential" Core..=) Prelude.<$> essential,
            ("portMappings" Core..=) Prelude.<$> portMappings,
            ("cpu" Core..=) Prelude.<$> cpu,
            ("resourceRequirements" Core..=)
              Prelude.<$> resourceRequirements,
            ("startTimeout" Core..=) Prelude.<$> startTimeout,
            ("readonlyRootFilesystem" Core..=)
              Prelude.<$> readonlyRootFilesystem,
            ("ulimits" Core..=) Prelude.<$> ulimits,
            ("stopTimeout" Core..=) Prelude.<$> stopTimeout,
            ("dockerSecurityOptions" Core..=)
              Prelude.<$> dockerSecurityOptions,
            ("disableNetworking" Core..=)
              Prelude.<$> disableNetworking,
            ("healthCheck" Core..=) Prelude.<$> healthCheck,
            ("repositoryCredentials" Core..=)
              Prelude.<$> repositoryCredentials
          ]
      )
