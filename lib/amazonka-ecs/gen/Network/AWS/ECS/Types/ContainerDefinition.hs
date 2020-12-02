{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ContainerDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ContainerDefinition where

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
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Container definitions are used in task definitions to describe the different containers that are launched as part of a task.
--
--
--
-- /See:/ 'containerDefinition' smart constructor.
data ContainerDefinition = ContainerDefinition'
  { _cdImage ::
      !(Maybe Text),
    _cdCommand :: !(Maybe [Text]),
    _cdHostname :: !(Maybe Text),
    _cdRepositoryCredentials ::
      !(Maybe RepositoryCredentials),
    _cdDockerSecurityOptions :: !(Maybe [Text]),
    _cdHealthCheck :: !(Maybe HealthCheck),
    _cdDisableNetworking :: !(Maybe Bool),
    _cdSecrets :: !(Maybe [Secret]),
    _cdVolumesFrom :: !(Maybe [VolumeFrom]),
    _cdEnvironment :: !(Maybe [KeyValuePair]),
    _cdEnvironmentFiles :: !(Maybe [EnvironmentFile]),
    _cdEntryPoint :: !(Maybe [Text]),
    _cdWorkingDirectory :: !(Maybe Text),
    _cdUlimits :: !(Maybe [Ulimit]),
    _cdStopTimeout :: !(Maybe Int),
    _cdPrivileged :: !(Maybe Bool),
    _cdPortMappings :: !(Maybe [PortMapping]),
    _cdResourceRequirements ::
      !(Maybe [ResourceRequirement]),
    _cdDockerLabels :: !(Maybe (Map Text (Text))),
    _cdExtraHosts :: !(Maybe [HostEntry]),
    _cdMemory :: !(Maybe Int),
    _cdSystemControls :: !(Maybe [SystemControl]),
    _cdUser :: !(Maybe Text),
    _cdFirelensConfiguration ::
      !(Maybe FirelensConfiguration),
    _cdDnsSearchDomains :: !(Maybe [Text]),
    _cdLogConfiguration :: !(Maybe LogConfiguration),
    _cdLinuxParameters :: !(Maybe LinuxParameters),
    _cdPseudoTerminal :: !(Maybe Bool),
    _cdDependsOn :: !(Maybe [ContainerDependency]),
    _cdName :: !(Maybe Text),
    _cdDnsServers :: !(Maybe [Text]),
    _cdMountPoints :: !(Maybe [MountPoint]),
    _cdInteractive :: !(Maybe Bool),
    _cdStartTimeout :: !(Maybe Int),
    _cdLinks :: !(Maybe [Text]),
    _cdReadonlyRootFilesystem :: !(Maybe Bool),
    _cdEssential :: !(Maybe Bool),
    _cdCpu :: !(Maybe Int),
    _cdMemoryReservation :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContainerDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdImage' - The image used to start a container. This string is passed directly to the Docker daemon. Images in the Docker Hub registry are available by default. Other repositories are specified with either @/repository-url/ //image/ :/tag/ @ or @/repository-url/ //image/ @/digest/ @ . Up to 255 letters (uppercase and lowercase), numbers, hyphens, underscores, colons, periods, forward slashes, and number signs are allowed. This parameter maps to @Image@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @IMAGE@ parameter of <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .     * When a new task starts, the Amazon ECS container agent pulls the latest version of the specified image and tag for the container to use. However, subsequent updates to a repository image are not propagated to already running tasks.     * Images in Amazon ECR repositories can be specified by either using the full @registry/repository:tag@ or @registry/repository@digest@ . For example, @012345678910.dkr.ecr.<region-name>.amazonaws.com/<repository-name>:latest@ or @012345678910.dkr.ecr.<region-name>.amazonaws.com/<repository-name>@sha256:94afd1f2e64d908bc90dbca0035a5b567EXAMPLE@ .      * Images in official repositories on Docker Hub use a single name (for example, @ubuntu@ or @mongo@ ).     * Images in other repositories on Docker Hub are qualified with an organization name (for example, @amazon/amazon-ecs-agent@ ).     * Images in other online repositories are qualified further by a domain name (for example, @quay.io/assemblyline/ubuntu@ ).
--
-- * 'cdCommand' - The command that is passed to the container. This parameter maps to @Cmd@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @COMMAND@ parameter to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . For more information, see <https://docs.docker.com/engine/reference/builder/#cmd https://docs.docker.com/engine/reference/builder/#cmd> . If there are multiple arguments, each argument should be a separated string in the array.
--
-- * 'cdHostname' - The hostname to use for your container. This parameter maps to @Hostname@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--hostname@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- * 'cdRepositoryCredentials' - The private repository authentication credentials to use.
--
-- * 'cdDockerSecurityOptions' - A list of strings to provide custom labels for SELinux and AppArmor multi-level security systems. This field is not valid for containers in tasks using the Fargate launch type. With Windows containers, this parameter can be used to reference a credential spec file when configuring a container for Active Directory authentication. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/windows-gmsa.html Using gMSAs for Windows Containers> in the /Amazon Elastic Container Service Developer Guide/ . This parameter maps to @SecurityOpt@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--security-opt@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . For more information about valid values, see <https://docs.docker.com/engine/reference/run/#security-configuration Docker Run Security Configuration> .  Valid values: "no-new-privileges" | "apparmor:PROFILE" | "label:value" | "credentialspec:CredentialSpecFilePath"
--
-- * 'cdHealthCheck' - The container health check command and associated configuration parameters for the container. This parameter maps to @HealthCheck@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @HEALTHCHECK@ parameter of <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- * 'cdDisableNetworking' - When this parameter is true, networking is disabled within the container. This parameter maps to @NetworkDisabled@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> .
--
-- * 'cdSecrets' - The secrets to pass to the container. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html Specifying Sensitive Data> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'cdVolumesFrom' - Data volumes to mount from another container. This parameter maps to @VolumesFrom@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--volumes-from@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- * 'cdEnvironment' - The environment variables to pass to a container. This parameter maps to @Env@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--env@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . /Important:/ We do not recommend using plaintext environment variables for sensitive information, such as credential data.
--
-- * 'cdEnvironmentFiles' - A list of files containing the environment variables to pass to a container. This parameter maps to the @--env-file@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . You can specify up to ten environment files. The file must have a @.env@ file extension. Each line in an environment file should contain an environment variable in @VARIABLE=VALUE@ format. Lines beginning with @#@ are treated as comments and are ignored. For more information on the environment variable file syntax, see <https://docs.docker.com/compose/env-file/ Declare default environment variables in file> . If there are environment variables specified using the @environment@ parameter in a container definition, they take precedence over the variables contained within an environment file. If multiple environment files are specified that contain the same variable, they are processed from the top down. It is recommended to use unique variable names. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/taskdef-envfiles.html Specifying Environment Variables> in the /Amazon Elastic Container Service Developer Guide/ . This field is not valid for containers in tasks using the Fargate launch type.
--
-- * 'cdEntryPoint' - /Important:/ Early versions of the Amazon ECS container agent do not properly handle @entryPoint@ parameters. If you have problems using @entryPoint@ , update your container agent or enter your commands and arguments as @command@ array items instead. The entry point that is passed to the container. This parameter maps to @Entrypoint@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--entrypoint@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . For more information, see <https://docs.docker.com/engine/reference/builder/#entrypoint https://docs.docker.com/engine/reference/builder/#entrypoint> .
--
-- * 'cdWorkingDirectory' - The working directory in which to run commands inside the container. This parameter maps to @WorkingDir@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--workdir@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- * 'cdUlimits' - A list of @ulimits@ to set in the container. If a ulimit value is specified in a task definition, it will override the default values set by Docker. This parameter maps to @Ulimits@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--ulimit@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . Valid naming values are displayed in the 'Ulimit' data type. This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@
--
-- * 'cdStopTimeout' - Time duration (in seconds) to wait before the container is forcefully killed if it doesn't exit normally on its own. For tasks using the Fargate launch type, the task or service requires platform version 1.3.0 or later. The max stop timeout value is 120 seconds and if the parameter is not specified, the default value of 30 seconds is used. For tasks using the EC2 launch type, if the @stopTimeout@ parameter is not specified, the value set for the Amazon ECS container agent configuration variable @ECS_CONTAINER_STOP_TIMEOUT@ is used by default. If neither the @stopTimeout@ parameter or the @ECS_CONTAINER_STOP_TIMEOUT@ agent configuration variable are set, then the default values of 30 seconds for Linux containers and 30 seconds on Windows containers are used. Your container instances require at least version 1.26.0 of the container agent to enable a container stop timeout value. However, we recommend using the latest container agent version. For information about checking your agent version and updating to the latest version, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent> in the /Amazon Elastic Container Service Developer Guide/ . If you are using an Amazon ECS-optimized Linux AMI, your instance needs at least version 1.26.0-1 of the @ecs-init@ package. If your container instances are launched from version @20190301@ or later, then they contain the required versions of the container agent and @ecs-init@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'cdPrivileged' - When this parameter is true, the container is given elevated privileges on the host container instance (similar to the @root@ user). This parameter maps to @Privileged@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--privileged@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- * 'cdPortMappings' - The list of port mappings for the container. Port mappings allow containers to access ports on the host container instance to send or receive traffic. For task definitions that use the @awsvpc@ network mode, you should only specify the @containerPort@ . The @hostPort@ can be left blank or it must be the same value as the @containerPort@ . Port mappings on Windows use the @NetNAT@ gateway address rather than @localhost@ . There is no loopback for port mappings on Windows, so you cannot access a container's mapped port from the host itself.  This parameter maps to @PortBindings@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--publish@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . If the network mode of a task definition is set to @none@ , then you can't specify port mappings. If the network mode of a task definition is set to @host@ , then host ports must either be undefined or they must match the container port in the port mapping.
--
-- * 'cdResourceRequirements' - The type and amount of a resource to assign to a container. The only supported resource is a GPU.
--
-- * 'cdDockerLabels' - A key/value map of labels to add to the container. This parameter maps to @Labels@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--label@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@
--
-- * 'cdExtraHosts' - A list of hostnames and IP address mappings to append to the @/etc/hosts@ file on the container. This parameter maps to @ExtraHosts@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--add-host@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- * 'cdMemory' - The amount (in MiB) of memory to present to the container. If your container attempts to exceed the memory specified here, the container is killed. The total amount of memory reserved for all containers within a task must be lower than the task @memory@ value, if one is specified. This parameter maps to @Memory@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--memory@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . If using the Fargate launch type, this parameter is optional. If using the EC2 launch type, you must specify either a task-level memory value or a container-level memory value. If you specify both a container-level @memory@ and @memoryReservation@ value, @memory@ must be greater than @memoryReservation@ . If you specify @memoryReservation@ , then that value is subtracted from the available memory resources for the container instance on which the container is placed. Otherwise, the value of @memory@ is used. The Docker daemon reserves a minimum of 4 MiB of memory for a container, so you should not specify fewer than 4 MiB of memory for your containers.
--
-- * 'cdSystemControls' - A list of namespaced kernel parameters to set in the container. This parameter maps to @Sysctls@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--sysctl@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- * 'cdUser' - The user to use inside the container. This parameter maps to @User@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--user@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . /Important:/ When running tasks using the @host@ network mode, you should not run containers using the root user (UID 0). It is considered best practice to use a non-root user. You can specify the @user@ using the following formats. If specifying a UID or GID, you must specify it as a positive integer.     * @user@      * @user:group@      * @uid@      * @uid:gid@      * @user:gid@      * @uid:group@
--
-- * 'cdFirelensConfiguration' - The FireLens configuration for the container. This is used to specify and configure a log router for container logs. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html Custom Log Routing> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'cdDnsSearchDomains' - A list of DNS search domains that are presented to the container. This parameter maps to @DnsSearch@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--dns-search@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- * 'cdLogConfiguration' - The log configuration specification for the container. This parameter maps to @LogConfig@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--log-driver@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . By default, containers use the same logging driver that the Docker daemon uses. However the container may use a different logging driver than the Docker daemon by specifying a log driver with this parameter in the container definition. To use a different logging driver for a container, the log system must be configured properly on the container instance (or on a different log server for remote logging options). For more information on the options for different supported log drivers, see <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers> in the Docker documentation. This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@
--
-- * 'cdLinuxParameters' - Linux-specific modifications that are applied to the container, such as Linux kernel capabilities. For more information see 'KernelCapabilities' .
--
-- * 'cdPseudoTerminal' - When this parameter is @true@ , a TTY is allocated. This parameter maps to @Tty@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--tty@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- * 'cdDependsOn' - The dependencies defined for container startup and shutdown. A container can contain multiple dependencies. When a dependency is defined for container startup, for container shutdown it is reversed. For tasks using the EC2 launch type, the container instances require at least version 1.26.0 of the container agent to enable container dependencies. However, we recommend using the latest container agent version. For information about checking your agent version and updating to the latest version, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent> in the /Amazon Elastic Container Service Developer Guide/ . If you are using an Amazon ECS-optimized Linux AMI, your instance needs at least version 1.26.0-1 of the @ecs-init@ package. If your container instances are launched from version @20190301@ or later, then they contain the required versions of the container agent and @ecs-init@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI> in the /Amazon Elastic Container Service Developer Guide/ . For tasks using the Fargate launch type, the task or service requires platform version @1.3.0@ or later.
--
-- * 'cdName' - The name of a container. If you are linking multiple containers together in a task definition, the @name@ of one container can be entered in the @links@ of another container to connect the containers. Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed. This parameter maps to @name@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--name@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- * 'cdDnsServers' - A list of DNS servers that are presented to the container. This parameter maps to @Dns@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--dns@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- * 'cdMountPoints' - The mount points for data volumes in your container. This parameter maps to @Volumes@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--volume@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . Windows containers can mount whole directories on the same drive as @> env:ProgramData@ . Windows containers cannot mount directories on a different drive, and mount point cannot be across drives.
--
-- * 'cdInteractive' - When this parameter is @true@ , this allows you to deploy containerized applications that require @stdin@ or a @tty@ to be allocated. This parameter maps to @OpenStdin@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--interactive@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- * 'cdStartTimeout' - Time duration (in seconds) to wait before giving up on resolving dependencies for a container. For example, you specify two containers in a task definition with containerA having a dependency on containerB reaching a @COMPLETE@ , @SUCCESS@ , or @HEALTHY@ status. If a @startTimeout@ value is specified for containerB and it does not reach the desired status within that time then containerA will give up and not start. This results in the task transitioning to a @STOPPED@ state. For tasks using the Fargate launch type, this parameter requires that the task or service uses platform version 1.3.0 or later. For tasks using the EC2 launch type, your container instances require at least version @1.26.0@ of the container agent to enable a container start timeout value. However, we recommend using the latest container agent version. For information about checking your agent version and updating to the latest version, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent> in the /Amazon Elastic Container Service Developer Guide/ . If you are using an Amazon ECS-optimized Linux AMI, your instance needs at least version @1.26.0-1@ of the @ecs-init@ package. If your container instances are launched from version @20190301@ or later, then they contain the required versions of the container agent and @ecs-init@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'cdLinks' - The @links@ parameter allows containers to communicate with each other without the need for port mappings. This parameter is only supported if the network mode of a task definition is @bridge@ . The @name:internalName@ construct is analogous to @name:alias@ in Docker links. Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed. For more information about linking Docker containers, go to <https://docs.docker.com/network/links/ Legacy container links> in the Docker documentation. This parameter maps to @Links@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--link@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . /Important:/ Containers that are collocated on a single container instance may be able to communicate with each other without requiring links or host port mappings. Network isolation is achieved on the container instance using security groups and VPC settings.
--
-- * 'cdReadonlyRootFilesystem' - When this parameter is true, the container is given read-only access to its root file system. This parameter maps to @ReadonlyRootfs@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--read-only@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- * 'cdEssential' - If the @essential@ parameter of a container is marked as @true@ , and that container fails or stops for any reason, all other containers that are part of the task are stopped. If the @essential@ parameter of a container is marked as @false@ , then its failure does not affect the rest of the containers in a task. If this parameter is omitted, a container is assumed to be essential. All tasks must have at least one essential container. If you have an application that is composed of multiple containers, you should group containers that are used for a common purpose into components, and separate the different components into multiple task definitions. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/application_architecture.html Application Architecture> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'cdCpu' - The number of @cpu@ units reserved for the container. This parameter maps to @CpuShares@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--cpu-shares@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . This field is optional for tasks using the Fargate launch type, and the only requirement is that the total amount of CPU reserved for all containers within a task be lower than the task-level @cpu@ value. Linux containers share unallocated CPU units with other containers on the container instance with the same ratio as their allocated amount. For example, if you run a single-container task on a single-core instance type with 512 CPU units specified for that container, and that is the only task running on the container instance, that container could use the full 1,024 CPU unit share at any given time. However, if you launched another copy of the same task on that container instance, each task would be guaranteed a minimum of 512 CPU units when needed, and each container could float to higher CPU usage if the other container was not using it, but if both tasks were 100% active all of the time, they would be limited to 512 CPU units. On Linux container instances, the Docker daemon on the container instance uses the CPU value to calculate the relative CPU share ratios for running containers. For more information, see <https://docs.docker.com/engine/reference/run/#cpu-share-constraint CPU share constraint> in the Docker documentation. The minimum valid CPU share value that the Linux kernel allows is 2. However, the CPU parameter is not required, and you can use CPU values below 2 in your container definitions. For CPU values below 2 (including null), the behavior varies based on your Amazon ECS container agent version:     * __Agent versions less than or equal to 1.1.0:__ Null and zero CPU values are passed to Docker as 0, which Docker then converts to 1,024 CPU shares. CPU values of 1 are passed to Docker as 1, which the Linux kernel converts to two CPU shares.     * __Agent versions greater than or equal to 1.2.0:__ Null, zero, and CPU values of 1 are passed to Docker as 2. On Windows container instances, the CPU limit is enforced as an absolute limit, or a quota. Windows containers only have access to the specified amount of CPU that is described in the task definition. A null or zero CPU value is passed to Docker as @0@ , which Windows interprets as 1% of one CPU.
--
-- * 'cdMemoryReservation' - The soft limit (in MiB) of memory to reserve for the container. When system memory is under heavy contention, Docker attempts to keep the container memory to this soft limit. However, your container can consume more memory when it needs to, up to either the hard limit specified with the @memory@ parameter (if applicable), or all of the available memory on the container instance, whichever comes first. This parameter maps to @MemoryReservation@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--memory-reservation@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . If a task-level memory value is not specified, you must specify a non-zero integer for one or both of @memory@ or @memoryReservation@ in a container definition. If you specify both, @memory@ must be greater than @memoryReservation@ . If you specify @memoryReservation@ , then that value is subtracted from the available memory resources for the container instance on which the container is placed. Otherwise, the value of @memory@ is used. For example, if your container normally uses 128 MiB of memory, but occasionally bursts to 256 MiB of memory for short periods of time, you can set a @memoryReservation@ of 128 MiB, and a @memory@ hard limit of 300 MiB. This configuration would allow the container to only reserve 128 MiB of memory from the remaining resources on the container instance, but also allow the container to consume more memory resources when needed. The Docker daemon reserves a minimum of 4 MiB of memory for a container, so you should not specify fewer than 4 MiB of memory for your containers.
containerDefinition ::
  ContainerDefinition
containerDefinition =
  ContainerDefinition'
    { _cdImage = Nothing,
      _cdCommand = Nothing,
      _cdHostname = Nothing,
      _cdRepositoryCredentials = Nothing,
      _cdDockerSecurityOptions = Nothing,
      _cdHealthCheck = Nothing,
      _cdDisableNetworking = Nothing,
      _cdSecrets = Nothing,
      _cdVolumesFrom = Nothing,
      _cdEnvironment = Nothing,
      _cdEnvironmentFiles = Nothing,
      _cdEntryPoint = Nothing,
      _cdWorkingDirectory = Nothing,
      _cdUlimits = Nothing,
      _cdStopTimeout = Nothing,
      _cdPrivileged = Nothing,
      _cdPortMappings = Nothing,
      _cdResourceRequirements = Nothing,
      _cdDockerLabels = Nothing,
      _cdExtraHosts = Nothing,
      _cdMemory = Nothing,
      _cdSystemControls = Nothing,
      _cdUser = Nothing,
      _cdFirelensConfiguration = Nothing,
      _cdDnsSearchDomains = Nothing,
      _cdLogConfiguration = Nothing,
      _cdLinuxParameters = Nothing,
      _cdPseudoTerminal = Nothing,
      _cdDependsOn = Nothing,
      _cdName = Nothing,
      _cdDnsServers = Nothing,
      _cdMountPoints = Nothing,
      _cdInteractive = Nothing,
      _cdStartTimeout = Nothing,
      _cdLinks = Nothing,
      _cdReadonlyRootFilesystem = Nothing,
      _cdEssential = Nothing,
      _cdCpu = Nothing,
      _cdMemoryReservation = Nothing
    }

-- | The image used to start a container. This string is passed directly to the Docker daemon. Images in the Docker Hub registry are available by default. Other repositories are specified with either @/repository-url/ //image/ :/tag/ @ or @/repository-url/ //image/ @/digest/ @ . Up to 255 letters (uppercase and lowercase), numbers, hyphens, underscores, colons, periods, forward slashes, and number signs are allowed. This parameter maps to @Image@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @IMAGE@ parameter of <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .     * When a new task starts, the Amazon ECS container agent pulls the latest version of the specified image and tag for the container to use. However, subsequent updates to a repository image are not propagated to already running tasks.     * Images in Amazon ECR repositories can be specified by either using the full @registry/repository:tag@ or @registry/repository@digest@ . For example, @012345678910.dkr.ecr.<region-name>.amazonaws.com/<repository-name>:latest@ or @012345678910.dkr.ecr.<region-name>.amazonaws.com/<repository-name>@sha256:94afd1f2e64d908bc90dbca0035a5b567EXAMPLE@ .      * Images in official repositories on Docker Hub use a single name (for example, @ubuntu@ or @mongo@ ).     * Images in other repositories on Docker Hub are qualified with an organization name (for example, @amazon/amazon-ecs-agent@ ).     * Images in other online repositories are qualified further by a domain name (for example, @quay.io/assemblyline/ubuntu@ ).
cdImage :: Lens' ContainerDefinition (Maybe Text)
cdImage = lens _cdImage (\s a -> s {_cdImage = a})

-- | The command that is passed to the container. This parameter maps to @Cmd@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @COMMAND@ parameter to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . For more information, see <https://docs.docker.com/engine/reference/builder/#cmd https://docs.docker.com/engine/reference/builder/#cmd> . If there are multiple arguments, each argument should be a separated string in the array.
cdCommand :: Lens' ContainerDefinition [Text]
cdCommand = lens _cdCommand (\s a -> s {_cdCommand = a}) . _Default . _Coerce

-- | The hostname to use for your container. This parameter maps to @Hostname@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--hostname@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
cdHostname :: Lens' ContainerDefinition (Maybe Text)
cdHostname = lens _cdHostname (\s a -> s {_cdHostname = a})

-- | The private repository authentication credentials to use.
cdRepositoryCredentials :: Lens' ContainerDefinition (Maybe RepositoryCredentials)
cdRepositoryCredentials = lens _cdRepositoryCredentials (\s a -> s {_cdRepositoryCredentials = a})

-- | A list of strings to provide custom labels for SELinux and AppArmor multi-level security systems. This field is not valid for containers in tasks using the Fargate launch type. With Windows containers, this parameter can be used to reference a credential spec file when configuring a container for Active Directory authentication. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/windows-gmsa.html Using gMSAs for Windows Containers> in the /Amazon Elastic Container Service Developer Guide/ . This parameter maps to @SecurityOpt@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--security-opt@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . For more information about valid values, see <https://docs.docker.com/engine/reference/run/#security-configuration Docker Run Security Configuration> .  Valid values: "no-new-privileges" | "apparmor:PROFILE" | "label:value" | "credentialspec:CredentialSpecFilePath"
cdDockerSecurityOptions :: Lens' ContainerDefinition [Text]
cdDockerSecurityOptions = lens _cdDockerSecurityOptions (\s a -> s {_cdDockerSecurityOptions = a}) . _Default . _Coerce

-- | The container health check command and associated configuration parameters for the container. This parameter maps to @HealthCheck@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @HEALTHCHECK@ parameter of <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
cdHealthCheck :: Lens' ContainerDefinition (Maybe HealthCheck)
cdHealthCheck = lens _cdHealthCheck (\s a -> s {_cdHealthCheck = a})

-- | When this parameter is true, networking is disabled within the container. This parameter maps to @NetworkDisabled@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> .
cdDisableNetworking :: Lens' ContainerDefinition (Maybe Bool)
cdDisableNetworking = lens _cdDisableNetworking (\s a -> s {_cdDisableNetworking = a})

-- | The secrets to pass to the container. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html Specifying Sensitive Data> in the /Amazon Elastic Container Service Developer Guide/ .
cdSecrets :: Lens' ContainerDefinition [Secret]
cdSecrets = lens _cdSecrets (\s a -> s {_cdSecrets = a}) . _Default . _Coerce

-- | Data volumes to mount from another container. This parameter maps to @VolumesFrom@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--volumes-from@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
cdVolumesFrom :: Lens' ContainerDefinition [VolumeFrom]
cdVolumesFrom = lens _cdVolumesFrom (\s a -> s {_cdVolumesFrom = a}) . _Default . _Coerce

-- | The environment variables to pass to a container. This parameter maps to @Env@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--env@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . /Important:/ We do not recommend using plaintext environment variables for sensitive information, such as credential data.
cdEnvironment :: Lens' ContainerDefinition [KeyValuePair]
cdEnvironment = lens _cdEnvironment (\s a -> s {_cdEnvironment = a}) . _Default . _Coerce

-- | A list of files containing the environment variables to pass to a container. This parameter maps to the @--env-file@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . You can specify up to ten environment files. The file must have a @.env@ file extension. Each line in an environment file should contain an environment variable in @VARIABLE=VALUE@ format. Lines beginning with @#@ are treated as comments and are ignored. For more information on the environment variable file syntax, see <https://docs.docker.com/compose/env-file/ Declare default environment variables in file> . If there are environment variables specified using the @environment@ parameter in a container definition, they take precedence over the variables contained within an environment file. If multiple environment files are specified that contain the same variable, they are processed from the top down. It is recommended to use unique variable names. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/taskdef-envfiles.html Specifying Environment Variables> in the /Amazon Elastic Container Service Developer Guide/ . This field is not valid for containers in tasks using the Fargate launch type.
cdEnvironmentFiles :: Lens' ContainerDefinition [EnvironmentFile]
cdEnvironmentFiles = lens _cdEnvironmentFiles (\s a -> s {_cdEnvironmentFiles = a}) . _Default . _Coerce

-- | /Important:/ Early versions of the Amazon ECS container agent do not properly handle @entryPoint@ parameters. If you have problems using @entryPoint@ , update your container agent or enter your commands and arguments as @command@ array items instead. The entry point that is passed to the container. This parameter maps to @Entrypoint@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--entrypoint@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . For more information, see <https://docs.docker.com/engine/reference/builder/#entrypoint https://docs.docker.com/engine/reference/builder/#entrypoint> .
cdEntryPoint :: Lens' ContainerDefinition [Text]
cdEntryPoint = lens _cdEntryPoint (\s a -> s {_cdEntryPoint = a}) . _Default . _Coerce

-- | The working directory in which to run commands inside the container. This parameter maps to @WorkingDir@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--workdir@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
cdWorkingDirectory :: Lens' ContainerDefinition (Maybe Text)
cdWorkingDirectory = lens _cdWorkingDirectory (\s a -> s {_cdWorkingDirectory = a})

-- | A list of @ulimits@ to set in the container. If a ulimit value is specified in a task definition, it will override the default values set by Docker. This parameter maps to @Ulimits@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--ulimit@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . Valid naming values are displayed in the 'Ulimit' data type. This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@
cdUlimits :: Lens' ContainerDefinition [Ulimit]
cdUlimits = lens _cdUlimits (\s a -> s {_cdUlimits = a}) . _Default . _Coerce

-- | Time duration (in seconds) to wait before the container is forcefully killed if it doesn't exit normally on its own. For tasks using the Fargate launch type, the task or service requires platform version 1.3.0 or later. The max stop timeout value is 120 seconds and if the parameter is not specified, the default value of 30 seconds is used. For tasks using the EC2 launch type, if the @stopTimeout@ parameter is not specified, the value set for the Amazon ECS container agent configuration variable @ECS_CONTAINER_STOP_TIMEOUT@ is used by default. If neither the @stopTimeout@ parameter or the @ECS_CONTAINER_STOP_TIMEOUT@ agent configuration variable are set, then the default values of 30 seconds for Linux containers and 30 seconds on Windows containers are used. Your container instances require at least version 1.26.0 of the container agent to enable a container stop timeout value. However, we recommend using the latest container agent version. For information about checking your agent version and updating to the latest version, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent> in the /Amazon Elastic Container Service Developer Guide/ . If you are using an Amazon ECS-optimized Linux AMI, your instance needs at least version 1.26.0-1 of the @ecs-init@ package. If your container instances are launched from version @20190301@ or later, then they contain the required versions of the container agent and @ecs-init@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI> in the /Amazon Elastic Container Service Developer Guide/ .
cdStopTimeout :: Lens' ContainerDefinition (Maybe Int)
cdStopTimeout = lens _cdStopTimeout (\s a -> s {_cdStopTimeout = a})

-- | When this parameter is true, the container is given elevated privileges on the host container instance (similar to the @root@ user). This parameter maps to @Privileged@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--privileged@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
cdPrivileged :: Lens' ContainerDefinition (Maybe Bool)
cdPrivileged = lens _cdPrivileged (\s a -> s {_cdPrivileged = a})

-- | The list of port mappings for the container. Port mappings allow containers to access ports on the host container instance to send or receive traffic. For task definitions that use the @awsvpc@ network mode, you should only specify the @containerPort@ . The @hostPort@ can be left blank or it must be the same value as the @containerPort@ . Port mappings on Windows use the @NetNAT@ gateway address rather than @localhost@ . There is no loopback for port mappings on Windows, so you cannot access a container's mapped port from the host itself.  This parameter maps to @PortBindings@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--publish@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . If the network mode of a task definition is set to @none@ , then you can't specify port mappings. If the network mode of a task definition is set to @host@ , then host ports must either be undefined or they must match the container port in the port mapping.
cdPortMappings :: Lens' ContainerDefinition [PortMapping]
cdPortMappings = lens _cdPortMappings (\s a -> s {_cdPortMappings = a}) . _Default . _Coerce

-- | The type and amount of a resource to assign to a container. The only supported resource is a GPU.
cdResourceRequirements :: Lens' ContainerDefinition [ResourceRequirement]
cdResourceRequirements = lens _cdResourceRequirements (\s a -> s {_cdResourceRequirements = a}) . _Default . _Coerce

-- | A key/value map of labels to add to the container. This parameter maps to @Labels@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--label@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@
cdDockerLabels :: Lens' ContainerDefinition (HashMap Text (Text))
cdDockerLabels = lens _cdDockerLabels (\s a -> s {_cdDockerLabels = a}) . _Default . _Map

-- | A list of hostnames and IP address mappings to append to the @/etc/hosts@ file on the container. This parameter maps to @ExtraHosts@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--add-host@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
cdExtraHosts :: Lens' ContainerDefinition [HostEntry]
cdExtraHosts = lens _cdExtraHosts (\s a -> s {_cdExtraHosts = a}) . _Default . _Coerce

-- | The amount (in MiB) of memory to present to the container. If your container attempts to exceed the memory specified here, the container is killed. The total amount of memory reserved for all containers within a task must be lower than the task @memory@ value, if one is specified. This parameter maps to @Memory@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--memory@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . If using the Fargate launch type, this parameter is optional. If using the EC2 launch type, you must specify either a task-level memory value or a container-level memory value. If you specify both a container-level @memory@ and @memoryReservation@ value, @memory@ must be greater than @memoryReservation@ . If you specify @memoryReservation@ , then that value is subtracted from the available memory resources for the container instance on which the container is placed. Otherwise, the value of @memory@ is used. The Docker daemon reserves a minimum of 4 MiB of memory for a container, so you should not specify fewer than 4 MiB of memory for your containers.
cdMemory :: Lens' ContainerDefinition (Maybe Int)
cdMemory = lens _cdMemory (\s a -> s {_cdMemory = a})

-- | A list of namespaced kernel parameters to set in the container. This parameter maps to @Sysctls@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--sysctl@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
cdSystemControls :: Lens' ContainerDefinition [SystemControl]
cdSystemControls = lens _cdSystemControls (\s a -> s {_cdSystemControls = a}) . _Default . _Coerce

-- | The user to use inside the container. This parameter maps to @User@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--user@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . /Important:/ When running tasks using the @host@ network mode, you should not run containers using the root user (UID 0). It is considered best practice to use a non-root user. You can specify the @user@ using the following formats. If specifying a UID or GID, you must specify it as a positive integer.     * @user@      * @user:group@      * @uid@      * @uid:gid@      * @user:gid@      * @uid:group@
cdUser :: Lens' ContainerDefinition (Maybe Text)
cdUser = lens _cdUser (\s a -> s {_cdUser = a})

-- | The FireLens configuration for the container. This is used to specify and configure a log router for container logs. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html Custom Log Routing> in the /Amazon Elastic Container Service Developer Guide/ .
cdFirelensConfiguration :: Lens' ContainerDefinition (Maybe FirelensConfiguration)
cdFirelensConfiguration = lens _cdFirelensConfiguration (\s a -> s {_cdFirelensConfiguration = a})

-- | A list of DNS search domains that are presented to the container. This parameter maps to @DnsSearch@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--dns-search@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
cdDnsSearchDomains :: Lens' ContainerDefinition [Text]
cdDnsSearchDomains = lens _cdDnsSearchDomains (\s a -> s {_cdDnsSearchDomains = a}) . _Default . _Coerce

-- | The log configuration specification for the container. This parameter maps to @LogConfig@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--log-driver@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . By default, containers use the same logging driver that the Docker daemon uses. However the container may use a different logging driver than the Docker daemon by specifying a log driver with this parameter in the container definition. To use a different logging driver for a container, the log system must be configured properly on the container instance (or on a different log server for remote logging options). For more information on the options for different supported log drivers, see <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers> in the Docker documentation. This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@
cdLogConfiguration :: Lens' ContainerDefinition (Maybe LogConfiguration)
cdLogConfiguration = lens _cdLogConfiguration (\s a -> s {_cdLogConfiguration = a})

-- | Linux-specific modifications that are applied to the container, such as Linux kernel capabilities. For more information see 'KernelCapabilities' .
cdLinuxParameters :: Lens' ContainerDefinition (Maybe LinuxParameters)
cdLinuxParameters = lens _cdLinuxParameters (\s a -> s {_cdLinuxParameters = a})

-- | When this parameter is @true@ , a TTY is allocated. This parameter maps to @Tty@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--tty@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
cdPseudoTerminal :: Lens' ContainerDefinition (Maybe Bool)
cdPseudoTerminal = lens _cdPseudoTerminal (\s a -> s {_cdPseudoTerminal = a})

-- | The dependencies defined for container startup and shutdown. A container can contain multiple dependencies. When a dependency is defined for container startup, for container shutdown it is reversed. For tasks using the EC2 launch type, the container instances require at least version 1.26.0 of the container agent to enable container dependencies. However, we recommend using the latest container agent version. For information about checking your agent version and updating to the latest version, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent> in the /Amazon Elastic Container Service Developer Guide/ . If you are using an Amazon ECS-optimized Linux AMI, your instance needs at least version 1.26.0-1 of the @ecs-init@ package. If your container instances are launched from version @20190301@ or later, then they contain the required versions of the container agent and @ecs-init@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI> in the /Amazon Elastic Container Service Developer Guide/ . For tasks using the Fargate launch type, the task or service requires platform version @1.3.0@ or later.
cdDependsOn :: Lens' ContainerDefinition [ContainerDependency]
cdDependsOn = lens _cdDependsOn (\s a -> s {_cdDependsOn = a}) . _Default . _Coerce

-- | The name of a container. If you are linking multiple containers together in a task definition, the @name@ of one container can be entered in the @links@ of another container to connect the containers. Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed. This parameter maps to @name@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--name@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
cdName :: Lens' ContainerDefinition (Maybe Text)
cdName = lens _cdName (\s a -> s {_cdName = a})

-- | A list of DNS servers that are presented to the container. This parameter maps to @Dns@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--dns@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
cdDnsServers :: Lens' ContainerDefinition [Text]
cdDnsServers = lens _cdDnsServers (\s a -> s {_cdDnsServers = a}) . _Default . _Coerce

-- | The mount points for data volumes in your container. This parameter maps to @Volumes@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--volume@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . Windows containers can mount whole directories on the same drive as @> env:ProgramData@ . Windows containers cannot mount directories on a different drive, and mount point cannot be across drives.
cdMountPoints :: Lens' ContainerDefinition [MountPoint]
cdMountPoints = lens _cdMountPoints (\s a -> s {_cdMountPoints = a}) . _Default . _Coerce

-- | When this parameter is @true@ , this allows you to deploy containerized applications that require @stdin@ or a @tty@ to be allocated. This parameter maps to @OpenStdin@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--interactive@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
cdInteractive :: Lens' ContainerDefinition (Maybe Bool)
cdInteractive = lens _cdInteractive (\s a -> s {_cdInteractive = a})

-- | Time duration (in seconds) to wait before giving up on resolving dependencies for a container. For example, you specify two containers in a task definition with containerA having a dependency on containerB reaching a @COMPLETE@ , @SUCCESS@ , or @HEALTHY@ status. If a @startTimeout@ value is specified for containerB and it does not reach the desired status within that time then containerA will give up and not start. This results in the task transitioning to a @STOPPED@ state. For tasks using the Fargate launch type, this parameter requires that the task or service uses platform version 1.3.0 or later. For tasks using the EC2 launch type, your container instances require at least version @1.26.0@ of the container agent to enable a container start timeout value. However, we recommend using the latest container agent version. For information about checking your agent version and updating to the latest version, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent> in the /Amazon Elastic Container Service Developer Guide/ . If you are using an Amazon ECS-optimized Linux AMI, your instance needs at least version @1.26.0-1@ of the @ecs-init@ package. If your container instances are launched from version @20190301@ or later, then they contain the required versions of the container agent and @ecs-init@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI> in the /Amazon Elastic Container Service Developer Guide/ .
cdStartTimeout :: Lens' ContainerDefinition (Maybe Int)
cdStartTimeout = lens _cdStartTimeout (\s a -> s {_cdStartTimeout = a})

-- | The @links@ parameter allows containers to communicate with each other without the need for port mappings. This parameter is only supported if the network mode of a task definition is @bridge@ . The @name:internalName@ construct is analogous to @name:alias@ in Docker links. Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed. For more information about linking Docker containers, go to <https://docs.docker.com/network/links/ Legacy container links> in the Docker documentation. This parameter maps to @Links@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--link@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . /Important:/ Containers that are collocated on a single container instance may be able to communicate with each other without requiring links or host port mappings. Network isolation is achieved on the container instance using security groups and VPC settings.
cdLinks :: Lens' ContainerDefinition [Text]
cdLinks = lens _cdLinks (\s a -> s {_cdLinks = a}) . _Default . _Coerce

-- | When this parameter is true, the container is given read-only access to its root file system. This parameter maps to @ReadonlyRootfs@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--read-only@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
cdReadonlyRootFilesystem :: Lens' ContainerDefinition (Maybe Bool)
cdReadonlyRootFilesystem = lens _cdReadonlyRootFilesystem (\s a -> s {_cdReadonlyRootFilesystem = a})

-- | If the @essential@ parameter of a container is marked as @true@ , and that container fails or stops for any reason, all other containers that are part of the task are stopped. If the @essential@ parameter of a container is marked as @false@ , then its failure does not affect the rest of the containers in a task. If this parameter is omitted, a container is assumed to be essential. All tasks must have at least one essential container. If you have an application that is composed of multiple containers, you should group containers that are used for a common purpose into components, and separate the different components into multiple task definitions. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/application_architecture.html Application Architecture> in the /Amazon Elastic Container Service Developer Guide/ .
cdEssential :: Lens' ContainerDefinition (Maybe Bool)
cdEssential = lens _cdEssential (\s a -> s {_cdEssential = a})

-- | The number of @cpu@ units reserved for the container. This parameter maps to @CpuShares@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--cpu-shares@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . This field is optional for tasks using the Fargate launch type, and the only requirement is that the total amount of CPU reserved for all containers within a task be lower than the task-level @cpu@ value. Linux containers share unallocated CPU units with other containers on the container instance with the same ratio as their allocated amount. For example, if you run a single-container task on a single-core instance type with 512 CPU units specified for that container, and that is the only task running on the container instance, that container could use the full 1,024 CPU unit share at any given time. However, if you launched another copy of the same task on that container instance, each task would be guaranteed a minimum of 512 CPU units when needed, and each container could float to higher CPU usage if the other container was not using it, but if both tasks were 100% active all of the time, they would be limited to 512 CPU units. On Linux container instances, the Docker daemon on the container instance uses the CPU value to calculate the relative CPU share ratios for running containers. For more information, see <https://docs.docker.com/engine/reference/run/#cpu-share-constraint CPU share constraint> in the Docker documentation. The minimum valid CPU share value that the Linux kernel allows is 2. However, the CPU parameter is not required, and you can use CPU values below 2 in your container definitions. For CPU values below 2 (including null), the behavior varies based on your Amazon ECS container agent version:     * __Agent versions less than or equal to 1.1.0:__ Null and zero CPU values are passed to Docker as 0, which Docker then converts to 1,024 CPU shares. CPU values of 1 are passed to Docker as 1, which the Linux kernel converts to two CPU shares.     * __Agent versions greater than or equal to 1.2.0:__ Null, zero, and CPU values of 1 are passed to Docker as 2. On Windows container instances, the CPU limit is enforced as an absolute limit, or a quota. Windows containers only have access to the specified amount of CPU that is described in the task definition. A null or zero CPU value is passed to Docker as @0@ , which Windows interprets as 1% of one CPU.
cdCpu :: Lens' ContainerDefinition (Maybe Int)
cdCpu = lens _cdCpu (\s a -> s {_cdCpu = a})

-- | The soft limit (in MiB) of memory to reserve for the container. When system memory is under heavy contention, Docker attempts to keep the container memory to this soft limit. However, your container can consume more memory when it needs to, up to either the hard limit specified with the @memory@ parameter (if applicable), or all of the available memory on the container instance, whichever comes first. This parameter maps to @MemoryReservation@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--memory-reservation@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . If a task-level memory value is not specified, you must specify a non-zero integer for one or both of @memory@ or @memoryReservation@ in a container definition. If you specify both, @memory@ must be greater than @memoryReservation@ . If you specify @memoryReservation@ , then that value is subtracted from the available memory resources for the container instance on which the container is placed. Otherwise, the value of @memory@ is used. For example, if your container normally uses 128 MiB of memory, but occasionally bursts to 256 MiB of memory for short periods of time, you can set a @memoryReservation@ of 128 MiB, and a @memory@ hard limit of 300 MiB. This configuration would allow the container to only reserve 128 MiB of memory from the remaining resources on the container instance, but also allow the container to consume more memory resources when needed. The Docker daemon reserves a minimum of 4 MiB of memory for a container, so you should not specify fewer than 4 MiB of memory for your containers.
cdMemoryReservation :: Lens' ContainerDefinition (Maybe Int)
cdMemoryReservation = lens _cdMemoryReservation (\s a -> s {_cdMemoryReservation = a})

instance FromJSON ContainerDefinition where
  parseJSON =
    withObject
      "ContainerDefinition"
      ( \x ->
          ContainerDefinition'
            <$> (x .:? "image")
            <*> (x .:? "command" .!= mempty)
            <*> (x .:? "hostname")
            <*> (x .:? "repositoryCredentials")
            <*> (x .:? "dockerSecurityOptions" .!= mempty)
            <*> (x .:? "healthCheck")
            <*> (x .:? "disableNetworking")
            <*> (x .:? "secrets" .!= mempty)
            <*> (x .:? "volumesFrom" .!= mempty)
            <*> (x .:? "environment" .!= mempty)
            <*> (x .:? "environmentFiles" .!= mempty)
            <*> (x .:? "entryPoint" .!= mempty)
            <*> (x .:? "workingDirectory")
            <*> (x .:? "ulimits" .!= mempty)
            <*> (x .:? "stopTimeout")
            <*> (x .:? "privileged")
            <*> (x .:? "portMappings" .!= mempty)
            <*> (x .:? "resourceRequirements" .!= mempty)
            <*> (x .:? "dockerLabels" .!= mempty)
            <*> (x .:? "extraHosts" .!= mempty)
            <*> (x .:? "memory")
            <*> (x .:? "systemControls" .!= mempty)
            <*> (x .:? "user")
            <*> (x .:? "firelensConfiguration")
            <*> (x .:? "dnsSearchDomains" .!= mempty)
            <*> (x .:? "logConfiguration")
            <*> (x .:? "linuxParameters")
            <*> (x .:? "pseudoTerminal")
            <*> (x .:? "dependsOn" .!= mempty)
            <*> (x .:? "name")
            <*> (x .:? "dnsServers" .!= mempty)
            <*> (x .:? "mountPoints" .!= mempty)
            <*> (x .:? "interactive")
            <*> (x .:? "startTimeout")
            <*> (x .:? "links" .!= mempty)
            <*> (x .:? "readonlyRootFilesystem")
            <*> (x .:? "essential")
            <*> (x .:? "cpu")
            <*> (x .:? "memoryReservation")
      )

instance Hashable ContainerDefinition

instance NFData ContainerDefinition

instance ToJSON ContainerDefinition where
  toJSON ContainerDefinition' {..} =
    object
      ( catMaybes
          [ ("image" .=) <$> _cdImage,
            ("command" .=) <$> _cdCommand,
            ("hostname" .=) <$> _cdHostname,
            ("repositoryCredentials" .=) <$> _cdRepositoryCredentials,
            ("dockerSecurityOptions" .=) <$> _cdDockerSecurityOptions,
            ("healthCheck" .=) <$> _cdHealthCheck,
            ("disableNetworking" .=) <$> _cdDisableNetworking,
            ("secrets" .=) <$> _cdSecrets,
            ("volumesFrom" .=) <$> _cdVolumesFrom,
            ("environment" .=) <$> _cdEnvironment,
            ("environmentFiles" .=) <$> _cdEnvironmentFiles,
            ("entryPoint" .=) <$> _cdEntryPoint,
            ("workingDirectory" .=) <$> _cdWorkingDirectory,
            ("ulimits" .=) <$> _cdUlimits,
            ("stopTimeout" .=) <$> _cdStopTimeout,
            ("privileged" .=) <$> _cdPrivileged,
            ("portMappings" .=) <$> _cdPortMappings,
            ("resourceRequirements" .=) <$> _cdResourceRequirements,
            ("dockerLabels" .=) <$> _cdDockerLabels,
            ("extraHosts" .=) <$> _cdExtraHosts,
            ("memory" .=) <$> _cdMemory,
            ("systemControls" .=) <$> _cdSystemControls,
            ("user" .=) <$> _cdUser,
            ("firelensConfiguration" .=) <$> _cdFirelensConfiguration,
            ("dnsSearchDomains" .=) <$> _cdDnsSearchDomains,
            ("logConfiguration" .=) <$> _cdLogConfiguration,
            ("linuxParameters" .=) <$> _cdLinuxParameters,
            ("pseudoTerminal" .=) <$> _cdPseudoTerminal,
            ("dependsOn" .=) <$> _cdDependsOn,
            ("name" .=) <$> _cdName,
            ("dnsServers" .=) <$> _cdDnsServers,
            ("mountPoints" .=) <$> _cdMountPoints,
            ("interactive" .=) <$> _cdInteractive,
            ("startTimeout" .=) <$> _cdStartTimeout,
            ("links" .=) <$> _cdLinks,
            ("readonlyRootFilesystem" .=) <$> _cdReadonlyRootFilesystem,
            ("essential" .=) <$> _cdEssential,
            ("cpu" .=) <$> _cdCpu,
            ("memoryReservation" .=) <$> _cdMemoryReservation
          ]
      )
