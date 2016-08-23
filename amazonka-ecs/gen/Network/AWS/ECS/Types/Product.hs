{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- /Note:/ This module is auto-generated and exported for convenience, but should
-- not be considered stable as the internal representations and naming is subject
-- to change per release.
module Network.AWS.ECS.Types.Product where

import           Network.AWS.ECS.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | The attributes applicable to a container instance when it is registered.
--
-- /See:/ 'attribute' smart constructor.
data Attribute = Attribute'
    { _aValue :: !(Maybe Text)
    , _aName  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Attribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aValue'
--
-- * 'aName'
attribute
    :: Text -- ^ 'aName'
    -> Attribute
attribute pName_ =
    Attribute'
    { _aValue = Nothing
    , _aName = pName_
    }

-- | The value of the container instance attribute (at this time, the value here is 'Null', but this could change in future revisions for expandability).
aValue :: Lens' Attribute (Maybe Text)
aValue = lens _aValue (\ s a -> s{_aValue = a});

-- | The name of the container instance attribute.
aName :: Lens' Attribute Text
aName = lens _aName (\ s a -> s{_aName = a});

instance FromJSON Attribute where
        parseJSON
          = withObject "Attribute"
              (\ x ->
                 Attribute' <$> (x .:? "value") <*> (x .: "name"))

instance Hashable Attribute

instance NFData Attribute

instance ToJSON Attribute where
        toJSON Attribute'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _aValue, Just ("name" .= _aName)])

-- | A regional grouping of one or more container instances on which you can run task requests. Each account receives a default cluster the first time you use the Amazon ECS service, but you may also create other clusters. Clusters may contain more than one instance type simultaneously.
--
-- /See:/ 'cluster' smart constructor.
data Cluster = Cluster'
    { _cStatus                            :: !(Maybe Text)
    , _cClusterARN                        :: !(Maybe Text)
    , _cRunningTasksCount                 :: !(Maybe Int)
    , _cRegisteredContainerInstancesCount :: !(Maybe Int)
    , _cPendingTasksCount                 :: !(Maybe Int)
    , _cClusterName                       :: !(Maybe Text)
    , _cActiveServicesCount               :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Cluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cStatus'
--
-- * 'cClusterARN'
--
-- * 'cRunningTasksCount'
--
-- * 'cRegisteredContainerInstancesCount'
--
-- * 'cPendingTasksCount'
--
-- * 'cClusterName'
--
-- * 'cActiveServicesCount'
cluster
    :: Cluster
cluster =
    Cluster'
    { _cStatus = Nothing
    , _cClusterARN = Nothing
    , _cRunningTasksCount = Nothing
    , _cRegisteredContainerInstancesCount = Nothing
    , _cPendingTasksCount = Nothing
    , _cClusterName = Nothing
    , _cActiveServicesCount = Nothing
    }

-- | The status of the cluster. The valid values are 'ACTIVE' or 'INACTIVE'. 'ACTIVE' indicates that you can register container instances with the cluster and the associated instances can accept tasks.
cStatus :: Lens' Cluster (Maybe Text)
cStatus = lens _cStatus (\ s a -> s{_cStatus = a});

-- | The Amazon Resource Name (ARN) that identifies the cluster. The ARN contains the 'arn:aws:ecs' namespace, followed by the region of the cluster, the AWS account ID of the cluster owner, the 'cluster' namespace, and then the cluster name. For example, 'arn:aws:ecs:region:012345678910:cluster\/test '..
cClusterARN :: Lens' Cluster (Maybe Text)
cClusterARN = lens _cClusterARN (\ s a -> s{_cClusterARN = a});

-- | The number of tasks in the cluster that are in the 'RUNNING' state.
cRunningTasksCount :: Lens' Cluster (Maybe Int)
cRunningTasksCount = lens _cRunningTasksCount (\ s a -> s{_cRunningTasksCount = a});

-- | The number of container instances registered into the cluster.
cRegisteredContainerInstancesCount :: Lens' Cluster (Maybe Int)
cRegisteredContainerInstancesCount = lens _cRegisteredContainerInstancesCount (\ s a -> s{_cRegisteredContainerInstancesCount = a});

-- | The number of tasks in the cluster that are in the 'PENDING' state.
cPendingTasksCount :: Lens' Cluster (Maybe Int)
cPendingTasksCount = lens _cPendingTasksCount (\ s a -> s{_cPendingTasksCount = a});

-- | A user-generated string that you use to identify your cluster.
cClusterName :: Lens' Cluster (Maybe Text)
cClusterName = lens _cClusterName (\ s a -> s{_cClusterName = a});

-- | The number of services that are running on the cluster in an 'ACTIVE' state. You can view these services with < ListServices>.
cActiveServicesCount :: Lens' Cluster (Maybe Int)
cActiveServicesCount = lens _cActiveServicesCount (\ s a -> s{_cActiveServicesCount = a});

instance FromJSON Cluster where
        parseJSON
          = withObject "Cluster"
              (\ x ->
                 Cluster' <$>
                   (x .:? "status") <*> (x .:? "clusterArn") <*>
                     (x .:? "runningTasksCount")
                     <*> (x .:? "registeredContainerInstancesCount")
                     <*> (x .:? "pendingTasksCount")
                     <*> (x .:? "clusterName")
                     <*> (x .:? "activeServicesCount"))

instance Hashable Cluster

instance NFData Cluster

-- | A Docker container that is part of a task.
--
-- /See:/ 'container' smart constructor.
data Container = Container'
    { _cNetworkBindings :: !(Maybe [NetworkBinding])
    , _cContainerARN    :: !(Maybe Text)
    , _cTaskARN         :: !(Maybe Text)
    , _cLastStatus      :: !(Maybe Text)
    , _cReason          :: !(Maybe Text)
    , _cName            :: !(Maybe Text)
    , _cExitCode        :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Container' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cNetworkBindings'
--
-- * 'cContainerARN'
--
-- * 'cTaskARN'
--
-- * 'cLastStatus'
--
-- * 'cReason'
--
-- * 'cName'
--
-- * 'cExitCode'
container
    :: Container
container =
    Container'
    { _cNetworkBindings = Nothing
    , _cContainerARN = Nothing
    , _cTaskARN = Nothing
    , _cLastStatus = Nothing
    , _cReason = Nothing
    , _cName = Nothing
    , _cExitCode = Nothing
    }

-- | The network bindings associated with the container.
cNetworkBindings :: Lens' Container [NetworkBinding]
cNetworkBindings = lens _cNetworkBindings (\ s a -> s{_cNetworkBindings = a}) . _Default . _Coerce;

-- | The Amazon Resource Name (ARN) of the container.
cContainerARN :: Lens' Container (Maybe Text)
cContainerARN = lens _cContainerARN (\ s a -> s{_cContainerARN = a});

-- | The Amazon Resource Name (ARN) of the task.
cTaskARN :: Lens' Container (Maybe Text)
cTaskARN = lens _cTaskARN (\ s a -> s{_cTaskARN = a});

-- | The last known status of the container.
cLastStatus :: Lens' Container (Maybe Text)
cLastStatus = lens _cLastStatus (\ s a -> s{_cLastStatus = a});

-- | A short (255 max characters) human-readable string to provide additional detail about a running or stopped container.
cReason :: Lens' Container (Maybe Text)
cReason = lens _cReason (\ s a -> s{_cReason = a});

-- | The name of the container.
cName :: Lens' Container (Maybe Text)
cName = lens _cName (\ s a -> s{_cName = a});

-- | The exit code returned from the container.
cExitCode :: Lens' Container (Maybe Int)
cExitCode = lens _cExitCode (\ s a -> s{_cExitCode = a});

instance FromJSON Container where
        parseJSON
          = withObject "Container"
              (\ x ->
                 Container' <$>
                   (x .:? "networkBindings" .!= mempty) <*>
                     (x .:? "containerArn")
                     <*> (x .:? "taskArn")
                     <*> (x .:? "lastStatus")
                     <*> (x .:? "reason")
                     <*> (x .:? "name")
                     <*> (x .:? "exitCode"))

instance Hashable Container

instance NFData Container

-- | Container definitions are used in task definitions to describe the different containers that are launched as part of a task.
--
-- /See:/ 'containerDefinition' smart constructor.
data ContainerDefinition = ContainerDefinition'
    { _cdImage                  :: !(Maybe Text)
    , _cdCommand                :: !(Maybe [Text])
    , _cdHostname               :: !(Maybe Text)
    , _cdDockerSecurityOptions  :: !(Maybe [Text])
    , _cdDisableNetworking      :: !(Maybe Bool)
    , _cdVolumesFrom            :: !(Maybe [VolumeFrom])
    , _cdEnvironment            :: !(Maybe [KeyValuePair])
    , _cdEntryPoint             :: !(Maybe [Text])
    , _cdWorkingDirectory       :: !(Maybe Text)
    , _cdUlimits                :: !(Maybe [Ulimit])
    , _cdPrivileged             :: !(Maybe Bool)
    , _cdPortMappings           :: !(Maybe [PortMapping])
    , _cdDockerLabels           :: !(Maybe (Map Text Text))
    , _cdExtraHosts             :: !(Maybe [HostEntry])
    , _cdMemory                 :: !(Maybe Int)
    , _cdUser                   :: !(Maybe Text)
    , _cdDnsSearchDomains       :: !(Maybe [Text])
    , _cdLogConfiguration       :: !(Maybe LogConfiguration)
    , _cdName                   :: !(Maybe Text)
    , _cdDnsServers             :: !(Maybe [Text])
    , _cdMountPoints            :: !(Maybe [MountPoint])
    , _cdLinks                  :: !(Maybe [Text])
    , _cdReadonlyRootFilesystem :: !(Maybe Bool)
    , _cdEssential              :: !(Maybe Bool)
    , _cdCpu                    :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ContainerDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdImage'
--
-- * 'cdCommand'
--
-- * 'cdHostname'
--
-- * 'cdDockerSecurityOptions'
--
-- * 'cdDisableNetworking'
--
-- * 'cdVolumesFrom'
--
-- * 'cdEnvironment'
--
-- * 'cdEntryPoint'
--
-- * 'cdWorkingDirectory'
--
-- * 'cdUlimits'
--
-- * 'cdPrivileged'
--
-- * 'cdPortMappings'
--
-- * 'cdDockerLabels'
--
-- * 'cdExtraHosts'
--
-- * 'cdMemory'
--
-- * 'cdUser'
--
-- * 'cdDnsSearchDomains'
--
-- * 'cdLogConfiguration'
--
-- * 'cdName'
--
-- * 'cdDnsServers'
--
-- * 'cdMountPoints'
--
-- * 'cdLinks'
--
-- * 'cdReadonlyRootFilesystem'
--
-- * 'cdEssential'
--
-- * 'cdCpu'
containerDefinition
    :: ContainerDefinition
containerDefinition =
    ContainerDefinition'
    { _cdImage = Nothing
    , _cdCommand = Nothing
    , _cdHostname = Nothing
    , _cdDockerSecurityOptions = Nothing
    , _cdDisableNetworking = Nothing
    , _cdVolumesFrom = Nothing
    , _cdEnvironment = Nothing
    , _cdEntryPoint = Nothing
    , _cdWorkingDirectory = Nothing
    , _cdUlimits = Nothing
    , _cdPrivileged = Nothing
    , _cdPortMappings = Nothing
    , _cdDockerLabels = Nothing
    , _cdExtraHosts = Nothing
    , _cdMemory = Nothing
    , _cdUser = Nothing
    , _cdDnsSearchDomains = Nothing
    , _cdLogConfiguration = Nothing
    , _cdName = Nothing
    , _cdDnsServers = Nothing
    , _cdMountPoints = Nothing
    , _cdLinks = Nothing
    , _cdReadonlyRootFilesystem = Nothing
    , _cdEssential = Nothing
    , _cdCpu = Nothing
    }

-- | The image used to start a container. This string is passed directly to the Docker daemon. Images in the Docker Hub registry are available by default. Other repositories are specified with ' repository-url\/image:tag '. Up to 255 letters (uppercase and lowercase), numbers, hyphens, underscores, colons, periods, forward slashes, and number signs are allowed. This parameter maps to 'Image' in the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/#create-a-container Create a container> section of the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/ Docker Remote API> and the 'IMAGE' parameter of <https://docs.docker.com/reference/commandline/run/ docker run>.
--
-- -   Images in official repositories on Docker Hub use a single name (for example, 'ubuntu' or 'mongo').
--
-- -   Images in other repositories on Docker Hub are qualified with an organization name (for example, 'amazon\/amazon-ecs-agent').
--
-- -   Images in other online repositories are qualified further by a domain name (for example, 'quay.io\/assemblyline\/ubuntu').
--
cdImage :: Lens' ContainerDefinition (Maybe Text)
cdImage = lens _cdImage (\ s a -> s{_cdImage = a});

-- | The command that is passed to the container. This parameter maps to 'Cmd' in the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/#create-a-container Create a container> section of the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/ Docker Remote API> and the 'COMMAND' parameter to <https://docs.docker.com/reference/commandline/run/ docker run>. For more information, see <https://docs.docker.com/reference/builder/#cmd>.
cdCommand :: Lens' ContainerDefinition [Text]
cdCommand = lens _cdCommand (\ s a -> s{_cdCommand = a}) . _Default . _Coerce;

-- | The hostname to use for your container. This parameter maps to 'Hostname' in the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/#create-a-container Create a container> section of the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/ Docker Remote API> and the '--hostname' option to <https://docs.docker.com/reference/commandline/run/ docker run>.
cdHostname :: Lens' ContainerDefinition (Maybe Text)
cdHostname = lens _cdHostname (\ s a -> s{_cdHostname = a});

-- | A list of strings to provide custom labels for SELinux and AppArmor multi-level security systems. This parameter maps to 'SecurityOpt' in the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/#create-a-container Create a container> section of the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/ Docker Remote API> and the '--security-opt' option to <https://docs.docker.com/reference/commandline/run/ docker run>.
--
-- The Amazon ECS container agent running on a container instance must register with the 'ECS_SELINUX_CAPABLE=true' or 'ECS_APPARMOR_CAPABLE=true' environment variables before containers placed on that instance can use these security options. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-config.html Amazon ECS Container Agent Configuration> in the /Amazon EC2 Container Service Developer Guide/.
cdDockerSecurityOptions :: Lens' ContainerDefinition [Text]
cdDockerSecurityOptions = lens _cdDockerSecurityOptions (\ s a -> s{_cdDockerSecurityOptions = a}) . _Default . _Coerce;

-- | When this parameter is true, networking is disabled within the container. This parameter maps to 'NetworkDisabled' in the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/#create-a-container Create a container> section of the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/ Docker Remote API>.
cdDisableNetworking :: Lens' ContainerDefinition (Maybe Bool)
cdDisableNetworking = lens _cdDisableNetworking (\ s a -> s{_cdDisableNetworking = a});

-- | Data volumes to mount from another container. This parameter maps to 'VolumesFrom' in the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/#create-a-container Create a container> section of the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/ Docker Remote API> and the '--volumes-from' option to <https://docs.docker.com/reference/commandline/run/ docker run>.
cdVolumesFrom :: Lens' ContainerDefinition [VolumeFrom]
cdVolumesFrom = lens _cdVolumesFrom (\ s a -> s{_cdVolumesFrom = a}) . _Default . _Coerce;

-- | The environment variables to pass to a container. This parameter maps to 'Env' in the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/#create-a-container Create a container> section of the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/ Docker Remote API> and the '--env' option to <https://docs.docker.com/reference/commandline/run/ docker run>.
--
-- We do not recommend using plain text environment variables for sensitive information, such as credential data.
cdEnvironment :: Lens' ContainerDefinition [KeyValuePair]
cdEnvironment = lens _cdEnvironment (\ s a -> s{_cdEnvironment = a}) . _Default . _Coerce;

-- | Early versions of the Amazon ECS container agent do not properly handle 'entryPoint' parameters. If you have problems using 'entryPoint', update your container agent or enter your commands and arguments as 'command' array items instead.
--
-- The entry point that is passed to the container. This parameter maps to 'Entrypoint' in the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/#create-a-container Create a container> section of the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/ Docker Remote API> and the '--entrypoint' option to <https://docs.docker.com/reference/commandline/run/ docker run>. For more information, see <https://docs.docker.com/reference/builder/#entrypoint>.
cdEntryPoint :: Lens' ContainerDefinition [Text]
cdEntryPoint = lens _cdEntryPoint (\ s a -> s{_cdEntryPoint = a}) . _Default . _Coerce;

-- | The working directory in which to run commands inside the container. This parameter maps to 'WorkingDir' in the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/#create-a-container Create a container> section of the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/ Docker Remote API> and the '--workdir' option to <https://docs.docker.com/reference/commandline/run/ docker run>.
cdWorkingDirectory :: Lens' ContainerDefinition (Maybe Text)
cdWorkingDirectory = lens _cdWorkingDirectory (\ s a -> s{_cdWorkingDirectory = a});

-- | A list of 'ulimits' to set in the container. This parameter maps to 'Ulimits' in the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/#create-a-container Create a container> section of the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/ Docker Remote API> and the '--ulimit' option to <https://docs.docker.com/reference/commandline/run/ docker run>. Valid naming values are displayed in the < Ulimit> data type. This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: 'sudo docker version | grep \"Server API version\"'
cdUlimits :: Lens' ContainerDefinition [Ulimit]
cdUlimits = lens _cdUlimits (\ s a -> s{_cdUlimits = a}) . _Default . _Coerce;

-- | When this parameter is true, the container is given elevated privileges on the host container instance (similar to the 'root' user). This parameter maps to 'Privileged' in the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/#create-a-container Create a container> section of the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/ Docker Remote API> and the '--privileged' option to <https://docs.docker.com/reference/commandline/run/ docker run>.
cdPrivileged :: Lens' ContainerDefinition (Maybe Bool)
cdPrivileged = lens _cdPrivileged (\ s a -> s{_cdPrivileged = a});

-- | The list of port mappings for the container. Port mappings allow containers to access ports on the host container instance to send or receive traffic. This parameter maps to 'PortBindings' in the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/#create-a-container Create a container> section of the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/ Docker Remote API> and the '--publish' option to <https://docs.docker.com/reference/commandline/run/ docker run>.
--
-- After a task reaches the 'RUNNING' status, manual and automatic host and container port assignments are visible in the __Network Bindings__ section of a container description of a selected task in the Amazon ECS console, or the 'networkBindings' section < DescribeTasks> responses.
cdPortMappings :: Lens' ContainerDefinition [PortMapping]
cdPortMappings = lens _cdPortMappings (\ s a -> s{_cdPortMappings = a}) . _Default . _Coerce;

-- | A key\/value map of labels to add to the container. This parameter maps to 'Labels' in the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/#create-a-container Create a container> section of the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/ Docker Remote API> and the '--label' option to <https://docs.docker.com/reference/commandline/run/ docker run>. This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: 'sudo docker version | grep \"Server API version\"'
cdDockerLabels :: Lens' ContainerDefinition (HashMap Text Text)
cdDockerLabels = lens _cdDockerLabels (\ s a -> s{_cdDockerLabels = a}) . _Default . _Map;

-- | A list of hostnames and IP address mappings to append to the '\/etc\/hosts' file on the container. This parameter maps to 'ExtraHosts' in the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/#create-a-container Create a container> section of the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/ Docker Remote API> and the '--add-host' option to <https://docs.docker.com/reference/commandline/run/ docker run>.
cdExtraHosts :: Lens' ContainerDefinition [HostEntry]
cdExtraHosts = lens _cdExtraHosts (\ s a -> s{_cdExtraHosts = a}) . _Default . _Coerce;

-- | The number of MiB of memory to reserve for the container. You must specify a non-zero integer for this parameter; the Docker daemon reserves a minimum of 4 MiB of memory for a container, so you should not specify fewer than 4 MiB of memory for your containers. If your container attempts to exceed the memory allocated here, the container is killed. This parameter maps to 'Memory' in the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/#create-a-container Create a container> section of the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/ Docker Remote API> and the '--memory' option to <https://docs.docker.com/reference/commandline/run/ docker run>.
cdMemory :: Lens' ContainerDefinition (Maybe Int)
cdMemory = lens _cdMemory (\ s a -> s{_cdMemory = a});

-- | The user name to use inside the container. This parameter maps to 'User' in the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/#create-a-container Create a container> section of the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/ Docker Remote API> and the '--user' option to <https://docs.docker.com/reference/commandline/run/ docker run>.
cdUser :: Lens' ContainerDefinition (Maybe Text)
cdUser = lens _cdUser (\ s a -> s{_cdUser = a});

-- | A list of DNS search domains that are presented to the container. This parameter maps to 'DnsSearch' in the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/#create-a-container Create a container> section of the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/ Docker Remote API> and the '--dns-search' option to <https://docs.docker.com/reference/commandline/run/ docker run>.
cdDnsSearchDomains :: Lens' ContainerDefinition [Text]
cdDnsSearchDomains = lens _cdDnsSearchDomains (\ s a -> s{_cdDnsSearchDomains = a}) . _Default . _Coerce;

-- | The log configuration specification for the container. This parameter maps to 'LogConfig' in the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/#create-a-container Create a container> section of the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/ Docker Remote API> and the '--log-driver' option to <https://docs.docker.com/reference/commandline/run/ docker run>. By default, containers use the same logging driver that the Docker daemon uses; however the container may use a different logging driver than the Docker daemon by specifying a log driver with this parameter in the container definition. To use a different logging driver for a container, the log system must be configured properly on the container instance (or on a different log server for remote logging options). For more information on the options for different supported log drivers, see <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers> in the Docker documentation.
--
-- Amazon ECS currently supports a subset of the logging drivers available to the Docker daemon (shown in the < LogConfiguration> data type). Currently unsupported log drivers may be available in future releases of the Amazon ECS container agent.
--
-- This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: 'sudo docker version | grep \"Server API version\"'
--
-- The Amazon ECS container agent running on a container instance must register the logging drivers available on that instance with the 'ECS_AVAILABLE_LOGGING_DRIVERS' environment variable before containers placed on that instance can use these log configuration options. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-config.html Amazon ECS Container Agent Configuration> in the /Amazon EC2 Container Service Developer Guide/.
cdLogConfiguration :: Lens' ContainerDefinition (Maybe LogConfiguration)
cdLogConfiguration = lens _cdLogConfiguration (\ s a -> s{_cdLogConfiguration = a});

-- | The name of a container. If you are linking multiple containers together in a task definition, the 'name' of one container can be entered in the 'links' of another container to connect the containers. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. This parameter maps to 'name' in the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/#create-a-container Create a container> section of the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/ Docker Remote API> and the '--name' option to <https://docs.docker.com/reference/commandline/run/ docker run>.
cdName :: Lens' ContainerDefinition (Maybe Text)
cdName = lens _cdName (\ s a -> s{_cdName = a});

-- | A list of DNS servers that are presented to the container. This parameter maps to 'Dns' in the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/#create-a-container Create a container> section of the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/ Docker Remote API> and the '--dns' option to <https://docs.docker.com/reference/commandline/run/ docker run>.
cdDnsServers :: Lens' ContainerDefinition [Text]
cdDnsServers = lens _cdDnsServers (\ s a -> s{_cdDnsServers = a}) . _Default . _Coerce;

-- | The mount points for data volumes in your container. This parameter maps to 'Volumes' in the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/#create-a-container Create a container> section of the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/ Docker Remote API> and the '--volume' option to <https://docs.docker.com/reference/commandline/run/ docker run>.
cdMountPoints :: Lens' ContainerDefinition [MountPoint]
cdMountPoints = lens _cdMountPoints (\ s a -> s{_cdMountPoints = a}) . _Default . _Coerce;

-- | The 'link' parameter allows containers to communicate with each other without the need for port mappings, using the 'name' parameter and optionally, an 'alias' for the link. This construct is analogous to 'name:alias' in Docker links. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed for each 'name' and 'alias'. For more information on linking Docker containers, see <https://docs.docker.com/userguide/dockerlinks/>. This parameter maps to 'Links' in the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/#create-a-container Create a container> section of the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/ Docker Remote API> and the '--link' option to <https://docs.docker.com/reference/commandline/run/ docker run>.
--
-- Containers that are collocated on a single container instance may be able to communicate with each other without requiring links or host port mappings. Network isolation is achieved on the container instance using security groups and VPC settings.
cdLinks :: Lens' ContainerDefinition [Text]
cdLinks = lens _cdLinks (\ s a -> s{_cdLinks = a}) . _Default . _Coerce;

-- | When this parameter is true, the container is given read-only access to its root file system. This parameter maps to 'ReadonlyRootfs' in the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/#create-a-container Create a container> section of the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/ Docker Remote API> and the '--read-only' option to 'docker run'.
cdReadonlyRootFilesystem :: Lens' ContainerDefinition (Maybe Bool)
cdReadonlyRootFilesystem = lens _cdReadonlyRootFilesystem (\ s a -> s{_cdReadonlyRootFilesystem = a});

-- | If the 'essential' parameter of a container is marked as 'true', and that container fails or stops for any reason, all other containers that are part of the task are stopped. If the 'essential' parameter of a container is marked as 'false', then its failure does not affect the rest of the containers in a task. If this parameter is omitted, a container is assumed to be essential.
--
-- All tasks must have at least one essential container. If you have an application that is composed of multiple containers, you should group containers that are used for a common purpose into components, and separate the different components into multiple task definitions. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/application_architecture.html Application Architecture> in the /Amazon EC2 Container Service Developer Guide/.
cdEssential :: Lens' ContainerDefinition (Maybe Bool)
cdEssential = lens _cdEssential (\ s a -> s{_cdEssential = a});

-- | The number of 'cpu' units reserved for the container. A container instance has 1,024 'cpu' units for every CPU core. This parameter specifies the minimum amount of CPU to reserve for a container, and containers share unallocated CPU units with other containers on the instance with the same ratio as their allocated amount. This parameter maps to 'CpuShares' in the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/#create-a-container Create a container> section of the <https://docs.docker.com/reference/api/docker_remote_api_v1.19/ Docker Remote API> and the '--cpu-shares' option to <https://docs.docker.com/reference/commandline/run/ docker run>.
--
-- You can determine the number of CPU units that are available per EC2 instance type by multiplying the vCPUs listed for that instance type on the <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instances> detail page by 1,024.
--
-- For example, if you run a single-container task on a single-core instance type with 512 CPU units specified for that container, and that is the only task running on the container instance, that container could use the full 1,024 CPU unit share at any given time. However, if you launched another copy of the same task on that container instance, each task would be guaranteed a minimum of 512 CPU units when needed, and each container could float to higher CPU usage if the other container was not using it, but if both tasks were 100% active all of the time, they would be limited to 512 CPU units.
--
-- The Docker daemon on the container instance uses the CPU value to calculate the relative CPU share ratios for running containers. For more information, see <https://docs.docker.com/reference/run/#cpu-share-constraint CPU share constraint> in the Docker documentation. The minimum valid CPU share value that the Linux kernel allows is 2; however, the CPU parameter is not required, and you can use CPU values below 2 in your container definitions. For CPU values below 2 (including null), the behavior varies based on your Amazon ECS container agent version:
--
-- -   __Agent versions less than or equal to 1.1.0:__ Null and zero CPU values are passed to Docker as 0, which Docker then converts to 1,024 CPU shares. CPU values of 1 are passed to Docker as 1, which the Linux kernel converts to 2 CPU shares.
--
-- -   __Agent versions greater than or equal to 1.2.0:__ Null, zero, and CPU values of 1 are passed to Docker as 2.
--
cdCpu :: Lens' ContainerDefinition (Maybe Int)
cdCpu = lens _cdCpu (\ s a -> s{_cdCpu = a});

instance FromJSON ContainerDefinition where
        parseJSON
          = withObject "ContainerDefinition"
              (\ x ->
                 ContainerDefinition' <$>
                   (x .:? "image") <*> (x .:? "command" .!= mempty) <*>
                     (x .:? "hostname")
                     <*> (x .:? "dockerSecurityOptions" .!= mempty)
                     <*> (x .:? "disableNetworking")
                     <*> (x .:? "volumesFrom" .!= mempty)
                     <*> (x .:? "environment" .!= mempty)
                     <*> (x .:? "entryPoint" .!= mempty)
                     <*> (x .:? "workingDirectory")
                     <*> (x .:? "ulimits" .!= mempty)
                     <*> (x .:? "privileged")
                     <*> (x .:? "portMappings" .!= mempty)
                     <*> (x .:? "dockerLabels" .!= mempty)
                     <*> (x .:? "extraHosts" .!= mempty)
                     <*> (x .:? "memory")
                     <*> (x .:? "user")
                     <*> (x .:? "dnsSearchDomains" .!= mempty)
                     <*> (x .:? "logConfiguration")
                     <*> (x .:? "name")
                     <*> (x .:? "dnsServers" .!= mempty)
                     <*> (x .:? "mountPoints" .!= mempty)
                     <*> (x .:? "links" .!= mempty)
                     <*> (x .:? "readonlyRootFilesystem")
                     <*> (x .:? "essential")
                     <*> (x .:? "cpu"))

instance Hashable ContainerDefinition

instance NFData ContainerDefinition

instance ToJSON ContainerDefinition where
        toJSON ContainerDefinition'{..}
          = object
              (catMaybes
                 [("image" .=) <$> _cdImage,
                  ("command" .=) <$> _cdCommand,
                  ("hostname" .=) <$> _cdHostname,
                  ("dockerSecurityOptions" .=) <$>
                    _cdDockerSecurityOptions,
                  ("disableNetworking" .=) <$> _cdDisableNetworking,
                  ("volumesFrom" .=) <$> _cdVolumesFrom,
                  ("environment" .=) <$> _cdEnvironment,
                  ("entryPoint" .=) <$> _cdEntryPoint,
                  ("workingDirectory" .=) <$> _cdWorkingDirectory,
                  ("ulimits" .=) <$> _cdUlimits,
                  ("privileged" .=) <$> _cdPrivileged,
                  ("portMappings" .=) <$> _cdPortMappings,
                  ("dockerLabels" .=) <$> _cdDockerLabels,
                  ("extraHosts" .=) <$> _cdExtraHosts,
                  ("memory" .=) <$> _cdMemory, ("user" .=) <$> _cdUser,
                  ("dnsSearchDomains" .=) <$> _cdDnsSearchDomains,
                  ("logConfiguration" .=) <$> _cdLogConfiguration,
                  ("name" .=) <$> _cdName,
                  ("dnsServers" .=) <$> _cdDnsServers,
                  ("mountPoints" .=) <$> _cdMountPoints,
                  ("links" .=) <$> _cdLinks,
                  ("readonlyRootFilesystem" .=) <$>
                    _cdReadonlyRootFilesystem,
                  ("essential" .=) <$> _cdEssential,
                  ("cpu" .=) <$> _cdCpu])

-- | An EC2 instance that is running the Amazon ECS agent and has been registered with a cluster.
--
-- /See:/ 'containerInstance' smart constructor.
data ContainerInstance = ContainerInstance'
    { _ciStatus               :: !(Maybe Text)
    , _ciRunningTasksCount    :: !(Maybe Int)
    , _ciRemainingResources   :: !(Maybe [Resource])
    , _ciEc2InstanceId        :: !(Maybe Text)
    , _ciContainerInstanceARN :: !(Maybe Text)
    , _ciAgentConnected       :: !(Maybe Bool)
    , _ciVersionInfo          :: !(Maybe VersionInfo)
    , _ciAgentUpdateStatus    :: !(Maybe AgentUpdateStatus)
    , _ciAttributes           :: !(Maybe [Attribute])
    , _ciPendingTasksCount    :: !(Maybe Int)
    , _ciRegisteredResources  :: !(Maybe [Resource])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ContainerInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciStatus'
--
-- * 'ciRunningTasksCount'
--
-- * 'ciRemainingResources'
--
-- * 'ciEc2InstanceId'
--
-- * 'ciContainerInstanceARN'
--
-- * 'ciAgentConnected'
--
-- * 'ciVersionInfo'
--
-- * 'ciAgentUpdateStatus'
--
-- * 'ciAttributes'
--
-- * 'ciPendingTasksCount'
--
-- * 'ciRegisteredResources'
containerInstance
    :: ContainerInstance
containerInstance =
    ContainerInstance'
    { _ciStatus = Nothing
    , _ciRunningTasksCount = Nothing
    , _ciRemainingResources = Nothing
    , _ciEc2InstanceId = Nothing
    , _ciContainerInstanceARN = Nothing
    , _ciAgentConnected = Nothing
    , _ciVersionInfo = Nothing
    , _ciAgentUpdateStatus = Nothing
    , _ciAttributes = Nothing
    , _ciPendingTasksCount = Nothing
    , _ciRegisteredResources = Nothing
    }

-- | The status of the container instance. The valid values are 'ACTIVE' or 'INACTIVE'. 'ACTIVE' indicates that the container instance can accept tasks.
ciStatus :: Lens' ContainerInstance (Maybe Text)
ciStatus = lens _ciStatus (\ s a -> s{_ciStatus = a});

-- | The number of tasks on the container instance that are in the 'RUNNING' status.
ciRunningTasksCount :: Lens' ContainerInstance (Maybe Int)
ciRunningTasksCount = lens _ciRunningTasksCount (\ s a -> s{_ciRunningTasksCount = a});

-- | The remaining resources of the container instance that are available for new tasks.
ciRemainingResources :: Lens' ContainerInstance [Resource]
ciRemainingResources = lens _ciRemainingResources (\ s a -> s{_ciRemainingResources = a}) . _Default . _Coerce;

-- | The EC2 instance ID of the container instance.
ciEc2InstanceId :: Lens' ContainerInstance (Maybe Text)
ciEc2InstanceId = lens _ciEc2InstanceId (\ s a -> s{_ciEc2InstanceId = a});

-- | The Amazon Resource Name (ARN) of the container instance. The ARN contains the 'arn:aws:ecs' namespace, followed by the region of the container instance, the AWS account ID of the container instance owner, the 'container-instance' namespace, and then the container instance ID. For example, 'arn:aws:ecs:region:aws_account_id:container-instance\/container_instance_ID '.
ciContainerInstanceARN :: Lens' ContainerInstance (Maybe Text)
ciContainerInstanceARN = lens _ciContainerInstanceARN (\ s a -> s{_ciContainerInstanceARN = a});

-- | This parameter returns 'true' if the agent is actually connected to Amazon ECS. Registered instances with an agent that may be unhealthy or stopped return 'false', and instances without a connected agent cannot accept placement requests.
ciAgentConnected :: Lens' ContainerInstance (Maybe Bool)
ciAgentConnected = lens _ciAgentConnected (\ s a -> s{_ciAgentConnected = a});

-- | The version information for the Amazon ECS container agent and Docker daemon running on the container instance.
ciVersionInfo :: Lens' ContainerInstance (Maybe VersionInfo)
ciVersionInfo = lens _ciVersionInfo (\ s a -> s{_ciVersionInfo = a});

-- | The status of the most recent agent update. If an update has never been requested, this value is 'NULL'.
ciAgentUpdateStatus :: Lens' ContainerInstance (Maybe AgentUpdateStatus)
ciAgentUpdateStatus = lens _ciAgentUpdateStatus (\ s a -> s{_ciAgentUpdateStatus = a});

-- | The attributes set for the container instance by the Amazon ECS container agent at instance registration.
ciAttributes :: Lens' ContainerInstance [Attribute]
ciAttributes = lens _ciAttributes (\ s a -> s{_ciAttributes = a}) . _Default . _Coerce;

-- | The number of tasks on the container instance that are in the 'PENDING' status.
ciPendingTasksCount :: Lens' ContainerInstance (Maybe Int)
ciPendingTasksCount = lens _ciPendingTasksCount (\ s a -> s{_ciPendingTasksCount = a});

-- | The registered resources on the container instance that are in use by current tasks.
ciRegisteredResources :: Lens' ContainerInstance [Resource]
ciRegisteredResources = lens _ciRegisteredResources (\ s a -> s{_ciRegisteredResources = a}) . _Default . _Coerce;

instance FromJSON ContainerInstance where
        parseJSON
          = withObject "ContainerInstance"
              (\ x ->
                 ContainerInstance' <$>
                   (x .:? "status") <*> (x .:? "runningTasksCount") <*>
                     (x .:? "remainingResources" .!= mempty)
                     <*> (x .:? "ec2InstanceId")
                     <*> (x .:? "containerInstanceArn")
                     <*> (x .:? "agentConnected")
                     <*> (x .:? "versionInfo")
                     <*> (x .:? "agentUpdateStatus")
                     <*> (x .:? "attributes" .!= mempty)
                     <*> (x .:? "pendingTasksCount")
                     <*> (x .:? "registeredResources" .!= mempty))

instance Hashable ContainerInstance

instance NFData ContainerInstance

-- | The overrides that should be sent to a container.
--
-- /See:/ 'containerOverride' smart constructor.
data ContainerOverride = ContainerOverride'
    { _coCommand     :: !(Maybe [Text])
    , _coEnvironment :: !(Maybe [KeyValuePair])
    , _coName        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ContainerOverride' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coCommand'
--
-- * 'coEnvironment'
--
-- * 'coName'
containerOverride
    :: ContainerOverride
containerOverride =
    ContainerOverride'
    { _coCommand = Nothing
    , _coEnvironment = Nothing
    , _coName = Nothing
    }

-- | The command to send to the container that overrides the default command from the Docker image or the task definition.
coCommand :: Lens' ContainerOverride [Text]
coCommand = lens _coCommand (\ s a -> s{_coCommand = a}) . _Default . _Coerce;

-- | The environment variables to send to the container. You can add new environment variables, which are added to the container at launch, or you can override the existing environment variables from the Docker image or the task definition.
coEnvironment :: Lens' ContainerOverride [KeyValuePair]
coEnvironment = lens _coEnvironment (\ s a -> s{_coEnvironment = a}) . _Default . _Coerce;

-- | The name of the container that receives the override.
coName :: Lens' ContainerOverride (Maybe Text)
coName = lens _coName (\ s a -> s{_coName = a});

instance FromJSON ContainerOverride where
        parseJSON
          = withObject "ContainerOverride"
              (\ x ->
                 ContainerOverride' <$>
                   (x .:? "command" .!= mempty) <*>
                     (x .:? "environment" .!= mempty)
                     <*> (x .:? "name"))

instance Hashable ContainerOverride

instance NFData ContainerOverride

instance ToJSON ContainerOverride where
        toJSON ContainerOverride'{..}
          = object
              (catMaybes
                 [("command" .=) <$> _coCommand,
                  ("environment" .=) <$> _coEnvironment,
                  ("name" .=) <$> _coName])

-- | Details on a service within a cluster
--
-- /See:/ 'containerService' smart constructor.
data ContainerService = ContainerService'
    { _csRunningCount            :: !(Maybe Int)
    , _csStatus                  :: !(Maybe Text)
    , _csClusterARN              :: !(Maybe Text)
    , _csCreatedAt               :: !(Maybe POSIX)
    , _csDesiredCount            :: !(Maybe Int)
    , _csLoadBalancers           :: !(Maybe [LoadBalancer])
    , _csPendingCount            :: !(Maybe Int)
    , _csEvents                  :: !(Maybe [ServiceEvent])
    , _csDeployments             :: !(Maybe [Deployment])
    , _csServiceName             :: !(Maybe Text)
    , _csServiceARN              :: !(Maybe Text)
    , _csTaskDefinition          :: !(Maybe Text)
    , _csRoleARN                 :: !(Maybe Text)
    , _csDeploymentConfiguration :: !(Maybe DeploymentConfiguration)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ContainerService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csRunningCount'
--
-- * 'csStatus'
--
-- * 'csClusterARN'
--
-- * 'csCreatedAt'
--
-- * 'csDesiredCount'
--
-- * 'csLoadBalancers'
--
-- * 'csPendingCount'
--
-- * 'csEvents'
--
-- * 'csDeployments'
--
-- * 'csServiceName'
--
-- * 'csServiceARN'
--
-- * 'csTaskDefinition'
--
-- * 'csRoleARN'
--
-- * 'csDeploymentConfiguration'
containerService
    :: ContainerService
containerService =
    ContainerService'
    { _csRunningCount = Nothing
    , _csStatus = Nothing
    , _csClusterARN = Nothing
    , _csCreatedAt = Nothing
    , _csDesiredCount = Nothing
    , _csLoadBalancers = Nothing
    , _csPendingCount = Nothing
    , _csEvents = Nothing
    , _csDeployments = Nothing
    , _csServiceName = Nothing
    , _csServiceARN = Nothing
    , _csTaskDefinition = Nothing
    , _csRoleARN = Nothing
    , _csDeploymentConfiguration = Nothing
    }

-- | The number of tasks in the cluster that are in the 'RUNNING' state.
csRunningCount :: Lens' ContainerService (Maybe Int)
csRunningCount = lens _csRunningCount (\ s a -> s{_csRunningCount = a});

-- | The status of the service. The valid values are 'ACTIVE', 'DRAINING', or 'INACTIVE'.
csStatus :: Lens' ContainerService (Maybe Text)
csStatus = lens _csStatus (\ s a -> s{_csStatus = a});

-- | The Amazon Resource Name (ARN) of the cluster that hosts the service.
csClusterARN :: Lens' ContainerService (Maybe Text)
csClusterARN = lens _csClusterARN (\ s a -> s{_csClusterARN = a});

-- | The Unix timestamp for when the service was created.
csCreatedAt :: Lens' ContainerService (Maybe UTCTime)
csCreatedAt = lens _csCreatedAt (\ s a -> s{_csCreatedAt = a}) . mapping _Time;

-- | The desired number of instantiations of the task definition to keep running on the service. This value is specified when the service is created with < CreateService>, and it can be modified with < UpdateService>.
csDesiredCount :: Lens' ContainerService (Maybe Int)
csDesiredCount = lens _csDesiredCount (\ s a -> s{_csDesiredCount = a});

-- | A list of Elastic Load Balancing load balancer objects, containing the load balancer name, the container name (as it appears in a container definition), and the container port to access from the load balancer.
csLoadBalancers :: Lens' ContainerService [LoadBalancer]
csLoadBalancers = lens _csLoadBalancers (\ s a -> s{_csLoadBalancers = a}) . _Default . _Coerce;

-- | The number of tasks in the cluster that are in the 'PENDING' state.
csPendingCount :: Lens' ContainerService (Maybe Int)
csPendingCount = lens _csPendingCount (\ s a -> s{_csPendingCount = a});

-- | The event stream for your service. A maximum of 100 of the latest events are displayed.
csEvents :: Lens' ContainerService [ServiceEvent]
csEvents = lens _csEvents (\ s a -> s{_csEvents = a}) . _Default . _Coerce;

-- | The current state of deployments for the service.
csDeployments :: Lens' ContainerService [Deployment]
csDeployments = lens _csDeployments (\ s a -> s{_csDeployments = a}) . _Default . _Coerce;

-- | The name of your service. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. Service names must be unique within a cluster, but you can have similarly named services in multiple clusters within a region or across multiple regions.
csServiceName :: Lens' ContainerService (Maybe Text)
csServiceName = lens _csServiceName (\ s a -> s{_csServiceName = a});

-- | The Amazon Resource Name (ARN) that identifies the service. The ARN contains the 'arn:aws:ecs' namespace, followed by the region of the service, the AWS account ID of the service owner, the 'service' namespace, and then the service name. For example, 'arn:aws:ecs:region:012345678910:service\/my-service '.
csServiceARN :: Lens' ContainerService (Maybe Text)
csServiceARN = lens _csServiceARN (\ s a -> s{_csServiceARN = a});

-- | The task definition to use for tasks in the service. This value is specified when the service is created with < CreateService>, and it can be modified with < UpdateService>.
csTaskDefinition :: Lens' ContainerService (Maybe Text)
csTaskDefinition = lens _csTaskDefinition (\ s a -> s{_csTaskDefinition = a});

-- | The Amazon Resource Name (ARN) of the IAM role associated with the service that allows the Amazon ECS container agent to register container instances with an Elastic Load Balancing load balancer.
csRoleARN :: Lens' ContainerService (Maybe Text)
csRoleARN = lens _csRoleARN (\ s a -> s{_csRoleARN = a});

-- | Optional deployment parameters that control how many tasks run during the deployment and the ordering of stopping and starting tasks.
csDeploymentConfiguration :: Lens' ContainerService (Maybe DeploymentConfiguration)
csDeploymentConfiguration = lens _csDeploymentConfiguration (\ s a -> s{_csDeploymentConfiguration = a});

instance FromJSON ContainerService where
        parseJSON
          = withObject "ContainerService"
              (\ x ->
                 ContainerService' <$>
                   (x .:? "runningCount") <*> (x .:? "status") <*>
                     (x .:? "clusterArn")
                     <*> (x .:? "createdAt")
                     <*> (x .:? "desiredCount")
                     <*> (x .:? "loadBalancers" .!= mempty)
                     <*> (x .:? "pendingCount")
                     <*> (x .:? "events" .!= mempty)
                     <*> (x .:? "deployments" .!= mempty)
                     <*> (x .:? "serviceName")
                     <*> (x .:? "serviceArn")
                     <*> (x .:? "taskDefinition")
                     <*> (x .:? "roleArn")
                     <*> (x .:? "deploymentConfiguration"))

instance Hashable ContainerService

instance NFData ContainerService

-- | The details of an Amazon ECS service deployment.
--
-- /See:/ 'deployment' smart constructor.
data Deployment = Deployment'
    { _dRunningCount   :: !(Maybe Int)
    , _dStatus         :: !(Maybe Text)
    , _dCreatedAt      :: !(Maybe POSIX)
    , _dDesiredCount   :: !(Maybe Int)
    , _dPendingCount   :: !(Maybe Int)
    , _dId             :: !(Maybe Text)
    , _dUpdatedAt      :: !(Maybe POSIX)
    , _dTaskDefinition :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Deployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dRunningCount'
--
-- * 'dStatus'
--
-- * 'dCreatedAt'
--
-- * 'dDesiredCount'
--
-- * 'dPendingCount'
--
-- * 'dId'
--
-- * 'dUpdatedAt'
--
-- * 'dTaskDefinition'
deployment
    :: Deployment
deployment =
    Deployment'
    { _dRunningCount = Nothing
    , _dStatus = Nothing
    , _dCreatedAt = Nothing
    , _dDesiredCount = Nothing
    , _dPendingCount = Nothing
    , _dId = Nothing
    , _dUpdatedAt = Nothing
    , _dTaskDefinition = Nothing
    }

-- | The number of tasks in the deployment that are in the 'RUNNING' status.
dRunningCount :: Lens' Deployment (Maybe Int)
dRunningCount = lens _dRunningCount (\ s a -> s{_dRunningCount = a});

-- | The status of the deployment. Valid values are 'PRIMARY' (for the most recent deployment), 'ACTIVE' (for previous deployments that still have tasks running, but are being replaced with the 'PRIMARY' deployment), and 'INACTIVE' (for deployments that have been completely replaced).
dStatus :: Lens' Deployment (Maybe Text)
dStatus = lens _dStatus (\ s a -> s{_dStatus = a});

-- | The Unix timestamp for when the service was created.
dCreatedAt :: Lens' Deployment (Maybe UTCTime)
dCreatedAt = lens _dCreatedAt (\ s a -> s{_dCreatedAt = a}) . mapping _Time;

-- | The most recent desired count of tasks that was specified for the service to deploy or maintain.
dDesiredCount :: Lens' Deployment (Maybe Int)
dDesiredCount = lens _dDesiredCount (\ s a -> s{_dDesiredCount = a});

-- | The number of tasks in the deployment that are in the 'PENDING' status.
dPendingCount :: Lens' Deployment (Maybe Int)
dPendingCount = lens _dPendingCount (\ s a -> s{_dPendingCount = a});

-- | The ID of the deployment.
dId :: Lens' Deployment (Maybe Text)
dId = lens _dId (\ s a -> s{_dId = a});

-- | The Unix timestamp for when the service was last updated.
dUpdatedAt :: Lens' Deployment (Maybe UTCTime)
dUpdatedAt = lens _dUpdatedAt (\ s a -> s{_dUpdatedAt = a}) . mapping _Time;

-- | The most recent task definition that was specified for the service to use.
dTaskDefinition :: Lens' Deployment (Maybe Text)
dTaskDefinition = lens _dTaskDefinition (\ s a -> s{_dTaskDefinition = a});

instance FromJSON Deployment where
        parseJSON
          = withObject "Deployment"
              (\ x ->
                 Deployment' <$>
                   (x .:? "runningCount") <*> (x .:? "status") <*>
                     (x .:? "createdAt")
                     <*> (x .:? "desiredCount")
                     <*> (x .:? "pendingCount")
                     <*> (x .:? "id")
                     <*> (x .:? "updatedAt")
                     <*> (x .:? "taskDefinition"))

instance Hashable Deployment

instance NFData Deployment

-- | Optional deployment parameters that control how many tasks run during the deployment and the ordering of stopping and starting tasks.
--
-- /See:/ 'deploymentConfiguration' smart constructor.
data DeploymentConfiguration = DeploymentConfiguration'
    { _dcMinimumHealthyPercent :: !(Maybe Int)
    , _dcMaximumPercent        :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeploymentConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcMinimumHealthyPercent'
--
-- * 'dcMaximumPercent'
deploymentConfiguration
    :: DeploymentConfiguration
deploymentConfiguration =
    DeploymentConfiguration'
    { _dcMinimumHealthyPercent = Nothing
    , _dcMaximumPercent = Nothing
    }

-- | The lower limit (as a percentage of the service\'s 'desiredCount') of the number of running tasks that must remain running and healthy in a service during a deployment. The minimum healthy tasks during a deployment is the 'desiredCount' multiplied by the 'minimumHealthyPercent'\/100, rounded up to the nearest integer value.
dcMinimumHealthyPercent :: Lens' DeploymentConfiguration (Maybe Int)
dcMinimumHealthyPercent = lens _dcMinimumHealthyPercent (\ s a -> s{_dcMinimumHealthyPercent = a});

-- | The upper limit (as a percentage of the service\'s 'desiredCount') of the number of running tasks that can be running in a service during a deployment. The maximum number of tasks during a deployment is the 'desiredCount' multiplied by the 'maximumPercent'\/100, rounded down to the nearest integer value.
dcMaximumPercent :: Lens' DeploymentConfiguration (Maybe Int)
dcMaximumPercent = lens _dcMaximumPercent (\ s a -> s{_dcMaximumPercent = a});

instance FromJSON DeploymentConfiguration where
        parseJSON
          = withObject "DeploymentConfiguration"
              (\ x ->
                 DeploymentConfiguration' <$>
                   (x .:? "minimumHealthyPercent") <*>
                     (x .:? "maximumPercent"))

instance Hashable DeploymentConfiguration

instance NFData DeploymentConfiguration

instance ToJSON DeploymentConfiguration where
        toJSON DeploymentConfiguration'{..}
          = object
              (catMaybes
                 [("minimumHealthyPercent" .=) <$>
                    _dcMinimumHealthyPercent,
                  ("maximumPercent" .=) <$> _dcMaximumPercent])

-- | A failed resource.
--
-- /See:/ 'failure' smart constructor.
data Failure = Failure'
    { _fArn    :: !(Maybe Text)
    , _fReason :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Failure' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fArn'
--
-- * 'fReason'
failure
    :: Failure
failure =
    Failure'
    { _fArn = Nothing
    , _fReason = Nothing
    }

-- | The Amazon Resource Name (ARN) of the failed resource.
fArn :: Lens' Failure (Maybe Text)
fArn = lens _fArn (\ s a -> s{_fArn = a});

-- | The reason for the failure.
fReason :: Lens' Failure (Maybe Text)
fReason = lens _fReason (\ s a -> s{_fReason = a});

instance FromJSON Failure where
        parseJSON
          = withObject "Failure"
              (\ x ->
                 Failure' <$> (x .:? "arn") <*> (x .:? "reason"))

instance Hashable Failure

instance NFData Failure

-- | Hostnames and IP address entries that are added to the '\/etc\/hosts' file of a container via the 'extraHosts' parameter of its < ContainerDefinition>.
--
-- /See:/ 'hostEntry' smart constructor.
data HostEntry = HostEntry'
    { _heHostname  :: !Text
    , _heIpAddress :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'HostEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'heHostname'
--
-- * 'heIpAddress'
hostEntry
    :: Text -- ^ 'heHostname'
    -> Text -- ^ 'heIpAddress'
    -> HostEntry
hostEntry pHostname_ pIpAddress_ =
    HostEntry'
    { _heHostname = pHostname_
    , _heIpAddress = pIpAddress_
    }

-- | The hostname to use in the '\/etc\/hosts' entry.
heHostname :: Lens' HostEntry Text
heHostname = lens _heHostname (\ s a -> s{_heHostname = a});

-- | The IP address to use in the '\/etc\/hosts' entry.
heIpAddress :: Lens' HostEntry Text
heIpAddress = lens _heIpAddress (\ s a -> s{_heIpAddress = a});

instance FromJSON HostEntry where
        parseJSON
          = withObject "HostEntry"
              (\ x ->
                 HostEntry' <$>
                   (x .: "hostname") <*> (x .: "ipAddress"))

instance Hashable HostEntry

instance NFData HostEntry

instance ToJSON HostEntry where
        toJSON HostEntry'{..}
          = object
              (catMaybes
                 [Just ("hostname" .= _heHostname),
                  Just ("ipAddress" .= _heIpAddress)])

-- | Details on a container instance host volume.
--
-- /See:/ 'hostVolumeProperties' smart constructor.
newtype HostVolumeProperties = HostVolumeProperties'
    { _hvpSourcePath :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'HostVolumeProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hvpSourcePath'
hostVolumeProperties
    :: HostVolumeProperties
hostVolumeProperties =
    HostVolumeProperties'
    { _hvpSourcePath = Nothing
    }

-- | The path on the host container instance that is presented to the container. If this parameter is empty, then the Docker daemon has assigned a host path for you. If the 'host' parameter contains a 'sourcePath' file location, then the data volume persists at the specified location on the host container instance until you delete it manually. If the 'sourcePath' value does not exist on the host container instance, the Docker daemon creates it. If the location does exist, the contents of the source path folder are exported.
hvpSourcePath :: Lens' HostVolumeProperties (Maybe Text)
hvpSourcePath = lens _hvpSourcePath (\ s a -> s{_hvpSourcePath = a});

instance FromJSON HostVolumeProperties where
        parseJSON
          = withObject "HostVolumeProperties"
              (\ x ->
                 HostVolumeProperties' <$> (x .:? "sourcePath"))

instance Hashable HostVolumeProperties

instance NFData HostVolumeProperties

instance ToJSON HostVolumeProperties where
        toJSON HostVolumeProperties'{..}
          = object
              (catMaybes [("sourcePath" .=) <$> _hvpSourcePath])

-- | A key and value pair object.
--
-- /See:/ 'keyValuePair' smart constructor.
data KeyValuePair = KeyValuePair'
    { _kvpValue :: !(Maybe Text)
    , _kvpName  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'KeyValuePair' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kvpValue'
--
-- * 'kvpName'
keyValuePair
    :: KeyValuePair
keyValuePair =
    KeyValuePair'
    { _kvpValue = Nothing
    , _kvpName = Nothing
    }

-- | The value of the key value pair. For environment variables, this is the value of the environment variable.
kvpValue :: Lens' KeyValuePair (Maybe Text)
kvpValue = lens _kvpValue (\ s a -> s{_kvpValue = a});

-- | The name of the key value pair. For environment variables, this is the name of the environment variable.
kvpName :: Lens' KeyValuePair (Maybe Text)
kvpName = lens _kvpName (\ s a -> s{_kvpName = a});

instance FromJSON KeyValuePair where
        parseJSON
          = withObject "KeyValuePair"
              (\ x ->
                 KeyValuePair' <$> (x .:? "value") <*> (x .:? "name"))

instance Hashable KeyValuePair

instance NFData KeyValuePair

instance ToJSON KeyValuePair where
        toJSON KeyValuePair'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _kvpValue,
                  ("name" .=) <$> _kvpName])

-- | Details on a load balancer that is used with a service.
--
-- /See:/ 'loadBalancer' smart constructor.
data LoadBalancer = LoadBalancer'
    { _lbLoadBalancerName :: !(Maybe Text)
    , _lbContainerName    :: !(Maybe Text)
    , _lbTargetGroupARN   :: !(Maybe Text)
    , _lbContainerPort    :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LoadBalancer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbLoadBalancerName'
--
-- * 'lbContainerName'
--
-- * 'lbTargetGroupARN'
--
-- * 'lbContainerPort'
loadBalancer
    :: LoadBalancer
loadBalancer =
    LoadBalancer'
    { _lbLoadBalancerName = Nothing
    , _lbContainerName = Nothing
    , _lbTargetGroupARN = Nothing
    , _lbContainerPort = Nothing
    }

-- | The name of the load balancer.
lbLoadBalancerName :: Lens' LoadBalancer (Maybe Text)
lbLoadBalancerName = lens _lbLoadBalancerName (\ s a -> s{_lbLoadBalancerName = a});

-- | The name of the container (as it appears in a container definition) to associate with the load balancer.
lbContainerName :: Lens' LoadBalancer (Maybe Text)
lbContainerName = lens _lbContainerName (\ s a -> s{_lbContainerName = a});

-- | The full Amazon Resource Name (ARN) of the Elastic Load Balancing target group associated with a service.
lbTargetGroupARN :: Lens' LoadBalancer (Maybe Text)
lbTargetGroupARN = lens _lbTargetGroupARN (\ s a -> s{_lbTargetGroupARN = a});

-- | The port on the container to associate with the load balancer. This port must correspond to a 'containerPort' in the service\'s task definition. Your container instances must allow ingress traffic on the 'hostPort' of the port mapping.
lbContainerPort :: Lens' LoadBalancer (Maybe Int)
lbContainerPort = lens _lbContainerPort (\ s a -> s{_lbContainerPort = a});

instance FromJSON LoadBalancer where
        parseJSON
          = withObject "LoadBalancer"
              (\ x ->
                 LoadBalancer' <$>
                   (x .:? "loadBalancerName") <*>
                     (x .:? "containerName")
                     <*> (x .:? "targetGroupArn")
                     <*> (x .:? "containerPort"))

instance Hashable LoadBalancer

instance NFData LoadBalancer

instance ToJSON LoadBalancer where
        toJSON LoadBalancer'{..}
          = object
              (catMaybes
                 [("loadBalancerName" .=) <$> _lbLoadBalancerName,
                  ("containerName" .=) <$> _lbContainerName,
                  ("targetGroupArn" .=) <$> _lbTargetGroupARN,
                  ("containerPort" .=) <$> _lbContainerPort])

-- | Log configuration options to send to a custom log driver for the container.
--
-- /See:/ 'logConfiguration' smart constructor.
data LogConfiguration = LogConfiguration'
    { _lcOptions   :: !(Maybe (Map Text Text))
    , _lcLogDriver :: !LogDriver
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LogConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcOptions'
--
-- * 'lcLogDriver'
logConfiguration
    :: LogDriver -- ^ 'lcLogDriver'
    -> LogConfiguration
logConfiguration pLogDriver_ =
    LogConfiguration'
    { _lcOptions = Nothing
    , _lcLogDriver = pLogDriver_
    }

-- | The configuration options to send to the log driver. This parameter requires version 1.19 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: 'sudo docker version | grep \"Server API version\"'
lcOptions :: Lens' LogConfiguration (HashMap Text Text)
lcOptions = lens _lcOptions (\ s a -> s{_lcOptions = a}) . _Default . _Map;

-- | The log driver to use for the container. The valid values listed for this parameter are log drivers that the Amazon ECS container agent can communicate with by default.
--
-- If you have a custom driver that is not listed above that you would like to work with the Amazon ECS container agent, you can fork the Amazon ECS container agent project that is <https://github.com/aws/amazon-ecs-agent available on GitHub> and customize it to work with that driver. We encourage you to submit pull requests for changes that you would like to have included. However, Amazon Web Services does not currently provide support for running modified copies of this software.
--
-- This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: 'sudo docker version | grep \"Server API version\"'
lcLogDriver :: Lens' LogConfiguration LogDriver
lcLogDriver = lens _lcLogDriver (\ s a -> s{_lcLogDriver = a});

instance FromJSON LogConfiguration where
        parseJSON
          = withObject "LogConfiguration"
              (\ x ->
                 LogConfiguration' <$>
                   (x .:? "options" .!= mempty) <*> (x .: "logDriver"))

instance Hashable LogConfiguration

instance NFData LogConfiguration

instance ToJSON LogConfiguration where
        toJSON LogConfiguration'{..}
          = object
              (catMaybes
                 [("options" .=) <$> _lcOptions,
                  Just ("logDriver" .= _lcLogDriver)])

-- | Details on a volume mount point that is used in a container definition.
--
-- /See:/ 'mountPoint' smart constructor.
data MountPoint = MountPoint'
    { _mpContainerPath :: !(Maybe Text)
    , _mpSourceVolume  :: !(Maybe Text)
    , _mpReadOnly      :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MountPoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpContainerPath'
--
-- * 'mpSourceVolume'
--
-- * 'mpReadOnly'
mountPoint
    :: MountPoint
mountPoint =
    MountPoint'
    { _mpContainerPath = Nothing
    , _mpSourceVolume = Nothing
    , _mpReadOnly = Nothing
    }

-- | The path on the container to mount the host volume at.
mpContainerPath :: Lens' MountPoint (Maybe Text)
mpContainerPath = lens _mpContainerPath (\ s a -> s{_mpContainerPath = a});

-- | The name of the volume to mount.
mpSourceVolume :: Lens' MountPoint (Maybe Text)
mpSourceVolume = lens _mpSourceVolume (\ s a -> s{_mpSourceVolume = a});

-- | If this value is 'true', the container has read-only access to the volume. If this value is 'false', then the container can write to the volume. The default value is 'false'.
mpReadOnly :: Lens' MountPoint (Maybe Bool)
mpReadOnly = lens _mpReadOnly (\ s a -> s{_mpReadOnly = a});

instance FromJSON MountPoint where
        parseJSON
          = withObject "MountPoint"
              (\ x ->
                 MountPoint' <$>
                   (x .:? "containerPath") <*> (x .:? "sourceVolume")
                     <*> (x .:? "readOnly"))

instance Hashable MountPoint

instance NFData MountPoint

instance ToJSON MountPoint where
        toJSON MountPoint'{..}
          = object
              (catMaybes
                 [("containerPath" .=) <$> _mpContainerPath,
                  ("sourceVolume" .=) <$> _mpSourceVolume,
                  ("readOnly" .=) <$> _mpReadOnly])

-- | Details on the network bindings between a container and its host container instance. After a task reaches the 'RUNNING' status, manual and automatic host and container port assignments are visible in the 'networkBindings' section of < DescribeTasks> API responses.
--
-- /See:/ 'networkBinding' smart constructor.
data NetworkBinding = NetworkBinding'
    { _nbBindIP        :: !(Maybe Text)
    , _nbProtocol      :: !(Maybe TransportProtocol)
    , _nbHostPort      :: !(Maybe Int)
    , _nbContainerPort :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'NetworkBinding' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nbBindIP'
--
-- * 'nbProtocol'
--
-- * 'nbHostPort'
--
-- * 'nbContainerPort'
networkBinding
    :: NetworkBinding
networkBinding =
    NetworkBinding'
    { _nbBindIP = Nothing
    , _nbProtocol = Nothing
    , _nbHostPort = Nothing
    , _nbContainerPort = Nothing
    }

-- | The IP address that the container is bound to on the container instance.
nbBindIP :: Lens' NetworkBinding (Maybe Text)
nbBindIP = lens _nbBindIP (\ s a -> s{_nbBindIP = a});

-- | The protocol used for the network binding.
nbProtocol :: Lens' NetworkBinding (Maybe TransportProtocol)
nbProtocol = lens _nbProtocol (\ s a -> s{_nbProtocol = a});

-- | The port number on the host that is used with the network binding.
nbHostPort :: Lens' NetworkBinding (Maybe Int)
nbHostPort = lens _nbHostPort (\ s a -> s{_nbHostPort = a});

-- | The port number on the container that is be used with the network binding.
nbContainerPort :: Lens' NetworkBinding (Maybe Int)
nbContainerPort = lens _nbContainerPort (\ s a -> s{_nbContainerPort = a});

instance FromJSON NetworkBinding where
        parseJSON
          = withObject "NetworkBinding"
              (\ x ->
                 NetworkBinding' <$>
                   (x .:? "bindIP") <*> (x .:? "protocol") <*>
                     (x .:? "hostPort")
                     <*> (x .:? "containerPort"))

instance Hashable NetworkBinding

instance NFData NetworkBinding

instance ToJSON NetworkBinding where
        toJSON NetworkBinding'{..}
          = object
              (catMaybes
                 [("bindIP" .=) <$> _nbBindIP,
                  ("protocol" .=) <$> _nbProtocol,
                  ("hostPort" .=) <$> _nbHostPort,
                  ("containerPort" .=) <$> _nbContainerPort])

-- | Port mappings allow containers to access ports on the host container instance to send or receive traffic. Port mappings are specified as part of the container definition. After a task reaches the 'RUNNING' status, manual and automatic host and container port assignments are visible in the 'networkBindings' section of < DescribeTasks> API responses.
--
-- /See:/ 'portMapping' smart constructor.
data PortMapping = PortMapping'
    { _pmProtocol      :: !(Maybe TransportProtocol)
    , _pmHostPort      :: !(Maybe Int)
    , _pmContainerPort :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PortMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmProtocol'
--
-- * 'pmHostPort'
--
-- * 'pmContainerPort'
portMapping
    :: PortMapping
portMapping =
    PortMapping'
    { _pmProtocol = Nothing
    , _pmHostPort = Nothing
    , _pmContainerPort = Nothing
    }

-- | The protocol used for the port mapping. Valid values are 'tcp' and 'udp'. The default is 'tcp'.
pmProtocol :: Lens' PortMapping (Maybe TransportProtocol)
pmProtocol = lens _pmProtocol (\ s a -> s{_pmProtocol = a});

-- | The port number on the container instance to reserve for your container. You can specify a non-reserved host port for your container port mapping, or you can omit the 'hostPort' (or set it to '0') while specifying a 'containerPort' and your container automatically receives a port in the ephemeral port range for your container instance operating system and Docker version.
--
-- The default ephemeral port range is 49153 to 65535, and this range is used for Docker versions prior to 1.6.0. For Docker version 1.6.0 and later, the Docker daemon tries to read the ephemeral port range from '\/proc\/sys\/net\/ipv4\/ip_local_port_range'; if this kernel parameter is unavailable, the default ephemeral port range is used. You should not attempt to specify a host port in the ephemeral port range, because these are reserved for automatic assignment. In general, ports below 32768 are outside of the ephemeral port range.
--
-- The default reserved ports are 22 for SSH, the Docker ports 2375 and 2376, and the Amazon ECS container agent port 51678. Any host port that was previously specified in a running task is also reserved while the task is running (after a task stops, the host port is released).The current reserved ports are displayed in the 'remainingResources' of < DescribeContainerInstances> output, and a container instance may have up to 100 reserved ports at a time, including the default reserved ports (automatically assigned ports do not count toward the 100 reserved ports limit).
pmHostPort :: Lens' PortMapping (Maybe Int)
pmHostPort = lens _pmHostPort (\ s a -> s{_pmHostPort = a});

-- | The port number on the container that is bound to the user-specified or automatically assigned host port. If you specify a container port and not a host port, your container automatically receives a host port in the ephemeral port range (for more information, see 'hostPort'). Port mappings that are automatically assigned in this way do not count toward the 100 reserved ports limit of a container instance.
pmContainerPort :: Lens' PortMapping (Maybe Int)
pmContainerPort = lens _pmContainerPort (\ s a -> s{_pmContainerPort = a});

instance FromJSON PortMapping where
        parseJSON
          = withObject "PortMapping"
              (\ x ->
                 PortMapping' <$>
                   (x .:? "protocol") <*> (x .:? "hostPort") <*>
                     (x .:? "containerPort"))

instance Hashable PortMapping

instance NFData PortMapping

instance ToJSON PortMapping where
        toJSON PortMapping'{..}
          = object
              (catMaybes
                 [("protocol" .=) <$> _pmProtocol,
                  ("hostPort" .=) <$> _pmHostPort,
                  ("containerPort" .=) <$> _pmContainerPort])

-- | Describes the resources available for a container instance.
--
-- /See:/ 'resource' smart constructor.
data Resource = Resource'
    { _rStringSetValue :: !(Maybe [Text])
    , _rIntegerValue   :: !(Maybe Int)
    , _rDoubleValue    :: !(Maybe Double)
    , _rLongValue      :: !(Maybe Integer)
    , _rName           :: !(Maybe Text)
    , _rType           :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Resource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rStringSetValue'
--
-- * 'rIntegerValue'
--
-- * 'rDoubleValue'
--
-- * 'rLongValue'
--
-- * 'rName'
--
-- * 'rType'
resource
    :: Resource
resource =
    Resource'
    { _rStringSetValue = Nothing
    , _rIntegerValue = Nothing
    , _rDoubleValue = Nothing
    , _rLongValue = Nothing
    , _rName = Nothing
    , _rType = Nothing
    }

-- | When the 'stringSetValue' type is set, the value of the resource must be a string type.
rStringSetValue :: Lens' Resource [Text]
rStringSetValue = lens _rStringSetValue (\ s a -> s{_rStringSetValue = a}) . _Default . _Coerce;

-- | When the 'integerValue' type is set, the value of the resource must be an integer.
rIntegerValue :: Lens' Resource (Maybe Int)
rIntegerValue = lens _rIntegerValue (\ s a -> s{_rIntegerValue = a});

-- | When the 'doubleValue' type is set, the value of the resource must be a double precision floating-point type.
rDoubleValue :: Lens' Resource (Maybe Double)
rDoubleValue = lens _rDoubleValue (\ s a -> s{_rDoubleValue = a});

-- | When the 'longValue' type is set, the value of the resource must be an extended precision floating-point type.
rLongValue :: Lens' Resource (Maybe Integer)
rLongValue = lens _rLongValue (\ s a -> s{_rLongValue = a});

-- | The name of the resource, such as 'CPU', 'MEMORY', 'PORTS', or a user-defined resource.
rName :: Lens' Resource (Maybe Text)
rName = lens _rName (\ s a -> s{_rName = a});

-- | The type of the resource, such as 'INTEGER', 'DOUBLE', 'LONG', or 'STRINGSET'.
rType :: Lens' Resource (Maybe Text)
rType = lens _rType (\ s a -> s{_rType = a});

instance FromJSON Resource where
        parseJSON
          = withObject "Resource"
              (\ x ->
                 Resource' <$>
                   (x .:? "stringSetValue" .!= mempty) <*>
                     (x .:? "integerValue")
                     <*> (x .:? "doubleValue")
                     <*> (x .:? "longValue")
                     <*> (x .:? "name")
                     <*> (x .:? "type"))

instance Hashable Resource

instance NFData Resource

instance ToJSON Resource where
        toJSON Resource'{..}
          = object
              (catMaybes
                 [("stringSetValue" .=) <$> _rStringSetValue,
                  ("integerValue" .=) <$> _rIntegerValue,
                  ("doubleValue" .=) <$> _rDoubleValue,
                  ("longValue" .=) <$> _rLongValue,
                  ("name" .=) <$> _rName, ("type" .=) <$> _rType])

-- | Details on an event associated with a service.
--
-- /See:/ 'serviceEvent' smart constructor.
data ServiceEvent = ServiceEvent'
    { _seCreatedAt :: !(Maybe POSIX)
    , _seId        :: !(Maybe Text)
    , _seMessage   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ServiceEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seCreatedAt'
--
-- * 'seId'
--
-- * 'seMessage'
serviceEvent
    :: ServiceEvent
serviceEvent =
    ServiceEvent'
    { _seCreatedAt = Nothing
    , _seId = Nothing
    , _seMessage = Nothing
    }

-- | The Unix timestamp for when the event was triggered.
seCreatedAt :: Lens' ServiceEvent (Maybe UTCTime)
seCreatedAt = lens _seCreatedAt (\ s a -> s{_seCreatedAt = a}) . mapping _Time;

-- | The ID string of the event.
seId :: Lens' ServiceEvent (Maybe Text)
seId = lens _seId (\ s a -> s{_seId = a});

-- | The event message.
seMessage :: Lens' ServiceEvent (Maybe Text)
seMessage = lens _seMessage (\ s a -> s{_seMessage = a});

instance FromJSON ServiceEvent where
        parseJSON
          = withObject "ServiceEvent"
              (\ x ->
                 ServiceEvent' <$>
                   (x .:? "createdAt") <*> (x .:? "id") <*>
                     (x .:? "message"))

instance Hashable ServiceEvent

instance NFData ServiceEvent

-- | Details on a task in a cluster.
--
-- /See:/ 'task' smart constructor.
data Task = Task'
    { _tStoppedAt            :: !(Maybe POSIX)
    , _tDesiredStatus        :: !(Maybe Text)
    , _tOverrides            :: !(Maybe TaskOverride)
    , _tClusterARN           :: !(Maybe Text)
    , _tCreatedAt            :: !(Maybe POSIX)
    , _tTaskARN              :: !(Maybe Text)
    , _tContainerInstanceARN :: !(Maybe Text)
    , _tLastStatus           :: !(Maybe Text)
    , _tContainers           :: !(Maybe [Container])
    , _tStartedAt            :: !(Maybe POSIX)
    , _tStartedBy            :: !(Maybe Text)
    , _tStoppedReason        :: !(Maybe Text)
    , _tTaskDefinitionARN    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Task' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tStoppedAt'
--
-- * 'tDesiredStatus'
--
-- * 'tOverrides'
--
-- * 'tClusterARN'
--
-- * 'tCreatedAt'
--
-- * 'tTaskARN'
--
-- * 'tContainerInstanceARN'
--
-- * 'tLastStatus'
--
-- * 'tContainers'
--
-- * 'tStartedAt'
--
-- * 'tStartedBy'
--
-- * 'tStoppedReason'
--
-- * 'tTaskDefinitionARN'
task
    :: Task
task =
    Task'
    { _tStoppedAt = Nothing
    , _tDesiredStatus = Nothing
    , _tOverrides = Nothing
    , _tClusterARN = Nothing
    , _tCreatedAt = Nothing
    , _tTaskARN = Nothing
    , _tContainerInstanceARN = Nothing
    , _tLastStatus = Nothing
    , _tContainers = Nothing
    , _tStartedAt = Nothing
    , _tStartedBy = Nothing
    , _tStoppedReason = Nothing
    , _tTaskDefinitionARN = Nothing
    }

-- | The Unix timestamp for when the task was stopped (the task transitioned from the 'RUNNING' state to the 'STOPPED' state).
tStoppedAt :: Lens' Task (Maybe UTCTime)
tStoppedAt = lens _tStoppedAt (\ s a -> s{_tStoppedAt = a}) . mapping _Time;

-- | The desired status of the task.
tDesiredStatus :: Lens' Task (Maybe Text)
tDesiredStatus = lens _tDesiredStatus (\ s a -> s{_tDesiredStatus = a});

-- | One or more container overrides.
tOverrides :: Lens' Task (Maybe TaskOverride)
tOverrides = lens _tOverrides (\ s a -> s{_tOverrides = a});

-- | The Amazon Resource Name (ARN) of the cluster that hosts the task.
tClusterARN :: Lens' Task (Maybe Text)
tClusterARN = lens _tClusterARN (\ s a -> s{_tClusterARN = a});

-- | The Unix timestamp for when the task was created (the task entered the 'PENDING' state).
tCreatedAt :: Lens' Task (Maybe UTCTime)
tCreatedAt = lens _tCreatedAt (\ s a -> s{_tCreatedAt = a}) . mapping _Time;

-- | The Amazon Resource Name (ARN) of the task.
tTaskARN :: Lens' Task (Maybe Text)
tTaskARN = lens _tTaskARN (\ s a -> s{_tTaskARN = a});

-- | The Amazon Resource Name (ARN) of the container instances that host the task.
tContainerInstanceARN :: Lens' Task (Maybe Text)
tContainerInstanceARN = lens _tContainerInstanceARN (\ s a -> s{_tContainerInstanceARN = a});

-- | The last known status of the task.
tLastStatus :: Lens' Task (Maybe Text)
tLastStatus = lens _tLastStatus (\ s a -> s{_tLastStatus = a});

-- | The containers associated with the task.
tContainers :: Lens' Task [Container]
tContainers = lens _tContainers (\ s a -> s{_tContainers = a}) . _Default . _Coerce;

-- | The Unix timestamp for when the task was started (the task transitioned from the 'PENDING' state to the 'RUNNING' state).
tStartedAt :: Lens' Task (Maybe UTCTime)
tStartedAt = lens _tStartedAt (\ s a -> s{_tStartedAt = a}) . mapping _Time;

-- | The tag specified when a task is started. If the task is started by an Amazon ECS service, then the 'startedBy' parameter contains the deployment ID of the service that starts it.
tStartedBy :: Lens' Task (Maybe Text)
tStartedBy = lens _tStartedBy (\ s a -> s{_tStartedBy = a});

-- | The reason the task was stopped.
tStoppedReason :: Lens' Task (Maybe Text)
tStoppedReason = lens _tStoppedReason (\ s a -> s{_tStoppedReason = a});

-- | The Amazon Resource Name (ARN) of the task definition that creates the task.
tTaskDefinitionARN :: Lens' Task (Maybe Text)
tTaskDefinitionARN = lens _tTaskDefinitionARN (\ s a -> s{_tTaskDefinitionARN = a});

instance FromJSON Task where
        parseJSON
          = withObject "Task"
              (\ x ->
                 Task' <$>
                   (x .:? "stoppedAt") <*> (x .:? "desiredStatus") <*>
                     (x .:? "overrides")
                     <*> (x .:? "clusterArn")
                     <*> (x .:? "createdAt")
                     <*> (x .:? "taskArn")
                     <*> (x .:? "containerInstanceArn")
                     <*> (x .:? "lastStatus")
                     <*> (x .:? "containers" .!= mempty)
                     <*> (x .:? "startedAt")
                     <*> (x .:? "startedBy")
                     <*> (x .:? "stoppedReason")
                     <*> (x .:? "taskDefinitionArn"))

instance Hashable Task

instance NFData Task

-- | Details of a task definition.
--
-- /See:/ 'taskDefinition' smart constructor.
data TaskDefinition = TaskDefinition'
    { _tdStatus               :: !(Maybe TaskDefinitionStatus)
    , _tdFamily               :: !(Maybe Text)
    , _tdContainerDefinitions :: !(Maybe [ContainerDefinition])
    , _tdTaskRoleARN          :: !(Maybe Text)
    , _tdTaskDefinitionARN    :: !(Maybe Text)
    , _tdRevision             :: !(Maybe Int)
    , _tdVolumes              :: !(Maybe [Volume])
    , _tdRequiresAttributes   :: !(Maybe [Attribute])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TaskDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdStatus'
--
-- * 'tdFamily'
--
-- * 'tdContainerDefinitions'
--
-- * 'tdTaskRoleARN'
--
-- * 'tdTaskDefinitionARN'
--
-- * 'tdRevision'
--
-- * 'tdVolumes'
--
-- * 'tdRequiresAttributes'
taskDefinition
    :: TaskDefinition
taskDefinition =
    TaskDefinition'
    { _tdStatus = Nothing
    , _tdFamily = Nothing
    , _tdContainerDefinitions = Nothing
    , _tdTaskRoleARN = Nothing
    , _tdTaskDefinitionARN = Nothing
    , _tdRevision = Nothing
    , _tdVolumes = Nothing
    , _tdRequiresAttributes = Nothing
    }

-- | The status of the task definition.
tdStatus :: Lens' TaskDefinition (Maybe TaskDefinitionStatus)
tdStatus = lens _tdStatus (\ s a -> s{_tdStatus = a});

-- | The family of your task definition, used as the definition name.
tdFamily :: Lens' TaskDefinition (Maybe Text)
tdFamily = lens _tdFamily (\ s a -> s{_tdFamily = a});

-- | A list of container definitions in JSON format that describe the different containers that make up your task. For more information about container definition parameters and defaults, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Amazon ECS Task Definitions> in the /Amazon EC2 Container Service Developer Guide/.
tdContainerDefinitions :: Lens' TaskDefinition [ContainerDefinition]
tdContainerDefinitions = lens _tdContainerDefinitions (\ s a -> s{_tdContainerDefinitions = a}) . _Default . _Coerce;

-- | The Amazon Resource Name (ARN) of the IAM role that containers in this task can assume. All containers in this task are granted the permissions that are specified in this role.
tdTaskRoleARN :: Lens' TaskDefinition (Maybe Text)
tdTaskRoleARN = lens _tdTaskRoleARN (\ s a -> s{_tdTaskRoleARN = a});

-- | The full Amazon Resource Name (ARN) of the task definition.
tdTaskDefinitionARN :: Lens' TaskDefinition (Maybe Text)
tdTaskDefinitionARN = lens _tdTaskDefinitionARN (\ s a -> s{_tdTaskDefinitionARN = a});

-- | The revision of the task in a particular family. The revision is a version number of a task definition in a family. When you register a task definition for the first time, the revision is '1'; each time you register a new revision of a task definition in the same family, the revision value always increases by one (even if you have deregistered previous revisions in this family).
tdRevision :: Lens' TaskDefinition (Maybe Int)
tdRevision = lens _tdRevision (\ s a -> s{_tdRevision = a});

-- | The list of volumes in a task. For more information about volume definition parameters and defaults, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Amazon ECS Task Definitions> in the /Amazon EC2 Container Service Developer Guide/.
tdVolumes :: Lens' TaskDefinition [Volume]
tdVolumes = lens _tdVolumes (\ s a -> s{_tdVolumes = a}) . _Default . _Coerce;

-- | The container instance attributes required by your task.
tdRequiresAttributes :: Lens' TaskDefinition [Attribute]
tdRequiresAttributes = lens _tdRequiresAttributes (\ s a -> s{_tdRequiresAttributes = a}) . _Default . _Coerce;

instance FromJSON TaskDefinition where
        parseJSON
          = withObject "TaskDefinition"
              (\ x ->
                 TaskDefinition' <$>
                   (x .:? "status") <*> (x .:? "family") <*>
                     (x .:? "containerDefinitions" .!= mempty)
                     <*> (x .:? "taskRoleArn")
                     <*> (x .:? "taskDefinitionArn")
                     <*> (x .:? "revision")
                     <*> (x .:? "volumes" .!= mempty)
                     <*> (x .:? "requiresAttributes" .!= mempty))

instance Hashable TaskDefinition

instance NFData TaskDefinition

-- | The overrides associated with a task.
--
-- /See:/ 'taskOverride' smart constructor.
data TaskOverride = TaskOverride'
    { _toContainerOverrides :: !(Maybe [ContainerOverride])
    , _toTaskRoleARN        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TaskOverride' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'toContainerOverrides'
--
-- * 'toTaskRoleARN'
taskOverride
    :: TaskOverride
taskOverride =
    TaskOverride'
    { _toContainerOverrides = Nothing
    , _toTaskRoleARN = Nothing
    }

-- | One or more container overrides sent to a task.
toContainerOverrides :: Lens' TaskOverride [ContainerOverride]
toContainerOverrides = lens _toContainerOverrides (\ s a -> s{_toContainerOverrides = a}) . _Default . _Coerce;

-- | The Amazon Resource Name (ARN) of the IAM role that containers in this task can assume. All containers in this task are granted the permissions that are specified in this role.
toTaskRoleARN :: Lens' TaskOverride (Maybe Text)
toTaskRoleARN = lens _toTaskRoleARN (\ s a -> s{_toTaskRoleARN = a});

instance FromJSON TaskOverride where
        parseJSON
          = withObject "TaskOverride"
              (\ x ->
                 TaskOverride' <$>
                   (x .:? "containerOverrides" .!= mempty) <*>
                     (x .:? "taskRoleArn"))

instance Hashable TaskOverride

instance NFData TaskOverride

instance ToJSON TaskOverride where
        toJSON TaskOverride'{..}
          = object
              (catMaybes
                 [("containerOverrides" .=) <$> _toContainerOverrides,
                  ("taskRoleArn" .=) <$> _toTaskRoleARN])

-- | The 'ulimit' settings to pass to the container.
--
-- /See:/ 'ulimit' smart constructor.
data Ulimit = Ulimit'
    { _uName      :: !UlimitName
    , _uSoftLimit :: !Int
    , _uHardLimit :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Ulimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uName'
--
-- * 'uSoftLimit'
--
-- * 'uHardLimit'
ulimit
    :: UlimitName -- ^ 'uName'
    -> Int -- ^ 'uSoftLimit'
    -> Int -- ^ 'uHardLimit'
    -> Ulimit
ulimit pName_ pSoftLimit_ pHardLimit_ =
    Ulimit'
    { _uName = pName_
    , _uSoftLimit = pSoftLimit_
    , _uHardLimit = pHardLimit_
    }

-- | The 'type' of the 'ulimit'.
uName :: Lens' Ulimit UlimitName
uName = lens _uName (\ s a -> s{_uName = a});

-- | The soft limit for the ulimit type.
uSoftLimit :: Lens' Ulimit Int
uSoftLimit = lens _uSoftLimit (\ s a -> s{_uSoftLimit = a});

-- | The hard limit for the ulimit type.
uHardLimit :: Lens' Ulimit Int
uHardLimit = lens _uHardLimit (\ s a -> s{_uHardLimit = a});

instance FromJSON Ulimit where
        parseJSON
          = withObject "Ulimit"
              (\ x ->
                 Ulimit' <$>
                   (x .: "name") <*> (x .: "softLimit") <*>
                     (x .: "hardLimit"))

instance Hashable Ulimit

instance NFData Ulimit

instance ToJSON Ulimit where
        toJSON Ulimit'{..}
          = object
              (catMaybes
                 [Just ("name" .= _uName),
                  Just ("softLimit" .= _uSoftLimit),
                  Just ("hardLimit" .= _uHardLimit)])

-- | The Docker and Amazon ECS container agent version information about a container instance.
--
-- /See:/ 'versionInfo' smart constructor.
data VersionInfo = VersionInfo'
    { _viAgentHash     :: !(Maybe Text)
    , _viAgentVersion  :: !(Maybe Text)
    , _viDockerVersion :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'VersionInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'viAgentHash'
--
-- * 'viAgentVersion'
--
-- * 'viDockerVersion'
versionInfo
    :: VersionInfo
versionInfo =
    VersionInfo'
    { _viAgentHash = Nothing
    , _viAgentVersion = Nothing
    , _viDockerVersion = Nothing
    }

-- | The Git commit hash for the Amazon ECS container agent build on the <https://github.com/aws/amazon-ecs-agent/commits/master amazon-ecs-agent> GitHub repository.
viAgentHash :: Lens' VersionInfo (Maybe Text)
viAgentHash = lens _viAgentHash (\ s a -> s{_viAgentHash = a});

-- | The version number of the Amazon ECS container agent.
viAgentVersion :: Lens' VersionInfo (Maybe Text)
viAgentVersion = lens _viAgentVersion (\ s a -> s{_viAgentVersion = a});

-- | The Docker version running on the container instance.
viDockerVersion :: Lens' VersionInfo (Maybe Text)
viDockerVersion = lens _viDockerVersion (\ s a -> s{_viDockerVersion = a});

instance FromJSON VersionInfo where
        parseJSON
          = withObject "VersionInfo"
              (\ x ->
                 VersionInfo' <$>
                   (x .:? "agentHash") <*> (x .:? "agentVersion") <*>
                     (x .:? "dockerVersion"))

instance Hashable VersionInfo

instance NFData VersionInfo

instance ToJSON VersionInfo where
        toJSON VersionInfo'{..}
          = object
              (catMaybes
                 [("agentHash" .=) <$> _viAgentHash,
                  ("agentVersion" .=) <$> _viAgentVersion,
                  ("dockerVersion" .=) <$> _viDockerVersion])

-- | A data volume used in a task definition.
--
-- /See:/ 'volume' smart constructor.
data Volume = Volume'
    { _vName :: !(Maybe Text)
    , _vHost :: !(Maybe HostVolumeProperties)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Volume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vName'
--
-- * 'vHost'
volume
    :: Volume
volume =
    Volume'
    { _vName = Nothing
    , _vHost = Nothing
    }

-- | The name of the volume. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. This name is referenced in the 'sourceVolume' parameter of container definition 'mountPoints'.
vName :: Lens' Volume (Maybe Text)
vName = lens _vName (\ s a -> s{_vName = a});

-- | The contents of the 'host' parameter determine whether your data volume persists on the host container instance and where it is stored. If the host parameter is empty, then the Docker daemon assigns a host path for your data volume, but the data is not guaranteed to persist after the containers associated with it stop running.
vHost :: Lens' Volume (Maybe HostVolumeProperties)
vHost = lens _vHost (\ s a -> s{_vHost = a});

instance FromJSON Volume where
        parseJSON
          = withObject "Volume"
              (\ x ->
                 Volume' <$> (x .:? "name") <*> (x .:? "host"))

instance Hashable Volume

instance NFData Volume

instance ToJSON Volume where
        toJSON Volume'{..}
          = object
              (catMaybes
                 [("name" .=) <$> _vName, ("host" .=) <$> _vHost])

-- | Details on a data volume from another container.
--
-- /See:/ 'volumeFrom' smart constructor.
data VolumeFrom = VolumeFrom'
    { _vfSourceContainer :: !(Maybe Text)
    , _vfReadOnly        :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'VolumeFrom' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vfSourceContainer'
--
-- * 'vfReadOnly'
volumeFrom
    :: VolumeFrom
volumeFrom =
    VolumeFrom'
    { _vfSourceContainer = Nothing
    , _vfReadOnly = Nothing
    }

-- | The name of the container to mount volumes from.
vfSourceContainer :: Lens' VolumeFrom (Maybe Text)
vfSourceContainer = lens _vfSourceContainer (\ s a -> s{_vfSourceContainer = a});

-- | If this value is 'true', the container has read-only access to the volume. If this value is 'false', then the container can write to the volume. The default value is 'false'.
vfReadOnly :: Lens' VolumeFrom (Maybe Bool)
vfReadOnly = lens _vfReadOnly (\ s a -> s{_vfReadOnly = a});

instance FromJSON VolumeFrom where
        parseJSON
          = withObject "VolumeFrom"
              (\ x ->
                 VolumeFrom' <$>
                   (x .:? "sourceContainer") <*> (x .:? "readOnly"))

instance Hashable VolumeFrom

instance NFData VolumeFrom

instance ToJSON VolumeFrom where
        toJSON VolumeFrom'{..}
          = object
              (catMaybes
                 [("sourceContainer" .=) <$> _vfSourceContainer,
                  ("readOnly" .=) <$> _vfReadOnly])
