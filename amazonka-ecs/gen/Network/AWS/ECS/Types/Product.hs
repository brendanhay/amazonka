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
module Network.AWS.ECS.Types.Product where

import           Network.AWS.ECS.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | An attribute is a name-value pair associated with an Amazon ECS object. Attributes enable you to extend the Amazon ECS data model by adding custom metadata to your resources. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html#attributes Attributes> in the /Amazon EC2 Container Service Developer Guide/ .
--
--
--
-- /See:/ 'attribute' smart constructor.
data Attribute = Attribute'
    { _aTargetId   :: !(Maybe Text)
    , _aValue      :: !(Maybe Text)
    , _aTargetType :: !(Maybe TargetType)
    , _aName       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Attribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aTargetId' - The ID of the target. You can specify the short form ID for a resource or the full Amazon Resource Name (ARN).
--
-- * 'aValue' - The value of the attribute. Up to 128 letters (uppercase and lowercase), numbers, hyphens, underscores, periods, at signs (@), forward slashes, colons, and spaces are allowed.
--
-- * 'aTargetType' - The type of the target with which to attach the attribute. This parameter is required if you use the short form ID for a resource instead of the full Amazon Resource Name (ARN).
--
-- * 'aName' - The name of the attribute. Up to 128 letters (uppercase and lowercase), numbers, hyphens, underscores, and periods are allowed.
attribute
    :: Text -- ^ 'aName'
    -> Attribute
attribute pName_ =
    Attribute'
    { _aTargetId = Nothing
    , _aValue = Nothing
    , _aTargetType = Nothing
    , _aName = pName_
    }

-- | The ID of the target. You can specify the short form ID for a resource or the full Amazon Resource Name (ARN).
aTargetId :: Lens' Attribute (Maybe Text)
aTargetId = lens _aTargetId (\ s a -> s{_aTargetId = a});

-- | The value of the attribute. Up to 128 letters (uppercase and lowercase), numbers, hyphens, underscores, periods, at signs (@), forward slashes, colons, and spaces are allowed.
aValue :: Lens' Attribute (Maybe Text)
aValue = lens _aValue (\ s a -> s{_aValue = a});

-- | The type of the target with which to attach the attribute. This parameter is required if you use the short form ID for a resource instead of the full Amazon Resource Name (ARN).
aTargetType :: Lens' Attribute (Maybe TargetType)
aTargetType = lens _aTargetType (\ s a -> s{_aTargetType = a});

-- | The name of the attribute. Up to 128 letters (uppercase and lowercase), numbers, hyphens, underscores, and periods are allowed.
aName :: Lens' Attribute Text
aName = lens _aName (\ s a -> s{_aName = a});

instance FromJSON Attribute where
        parseJSON
          = withObject "Attribute"
              (\ x ->
                 Attribute' <$>
                   (x .:? "targetId") <*> (x .:? "value") <*>
                     (x .:? "targetType")
                     <*> (x .: "name"))

instance Hashable Attribute

instance NFData Attribute

instance ToJSON Attribute where
        toJSON Attribute'{..}
          = object
              (catMaybes
                 [("targetId" .=) <$> _aTargetId,
                  ("value" .=) <$> _aValue,
                  ("targetType" .=) <$> _aTargetType,
                  Just ("name" .= _aName)])

-- | A regional grouping of one or more container instances on which you can run task requests. Each account receives a default cluster the first time you use the Amazon ECS service, but you may also create other clusters. Clusters may contain more than one instance type simultaneously.
--
--
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
-- * 'cStatus' - The status of the cluster. The valid values are @ACTIVE@ or @INACTIVE@ . @ACTIVE@ indicates that you can register container instances with the cluster and the associated instances can accept tasks.
--
-- * 'cClusterARN' - The Amazon Resource Name (ARN) that identifies the cluster. The ARN contains the @arn:aws:ecs@ namespace, followed by the region of the cluster, the AWS account ID of the cluster owner, the @cluster@ namespace, and then the cluster name. For example, @arn:aws:ecs:/region/ :/012345678910/ :cluster//test/ @ ..
--
-- * 'cRunningTasksCount' - The number of tasks in the cluster that are in the @RUNNING@ state.
--
-- * 'cRegisteredContainerInstancesCount' - The number of container instances registered into the cluster.
--
-- * 'cPendingTasksCount' - The number of tasks in the cluster that are in the @PENDING@ state.
--
-- * 'cClusterName' - A user-generated string that you use to identify your cluster.
--
-- * 'cActiveServicesCount' - The number of services that are running on the cluster in an @ACTIVE@ state. You can view these services with 'ListServices' .
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

-- | The status of the cluster. The valid values are @ACTIVE@ or @INACTIVE@ . @ACTIVE@ indicates that you can register container instances with the cluster and the associated instances can accept tasks.
cStatus :: Lens' Cluster (Maybe Text)
cStatus = lens _cStatus (\ s a -> s{_cStatus = a});

-- | The Amazon Resource Name (ARN) that identifies the cluster. The ARN contains the @arn:aws:ecs@ namespace, followed by the region of the cluster, the AWS account ID of the cluster owner, the @cluster@ namespace, and then the cluster name. For example, @arn:aws:ecs:/region/ :/012345678910/ :cluster//test/ @ ..
cClusterARN :: Lens' Cluster (Maybe Text)
cClusterARN = lens _cClusterARN (\ s a -> s{_cClusterARN = a});

-- | The number of tasks in the cluster that are in the @RUNNING@ state.
cRunningTasksCount :: Lens' Cluster (Maybe Int)
cRunningTasksCount = lens _cRunningTasksCount (\ s a -> s{_cRunningTasksCount = a});

-- | The number of container instances registered into the cluster.
cRegisteredContainerInstancesCount :: Lens' Cluster (Maybe Int)
cRegisteredContainerInstancesCount = lens _cRegisteredContainerInstancesCount (\ s a -> s{_cRegisteredContainerInstancesCount = a});

-- | The number of tasks in the cluster that are in the @PENDING@ state.
cPendingTasksCount :: Lens' Cluster (Maybe Int)
cPendingTasksCount = lens _cPendingTasksCount (\ s a -> s{_cPendingTasksCount = a});

-- | A user-generated string that you use to identify your cluster.
cClusterName :: Lens' Cluster (Maybe Text)
cClusterName = lens _cClusterName (\ s a -> s{_cClusterName = a});

-- | The number of services that are running on the cluster in an @ACTIVE@ state. You can view these services with 'ListServices' .
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
--
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
-- * 'cNetworkBindings' - The network bindings associated with the container.
--
-- * 'cContainerARN' - The Amazon Resource Name (ARN) of the container.
--
-- * 'cTaskARN' - The Amazon Resource Name (ARN) of the task.
--
-- * 'cLastStatus' - The last known status of the container.
--
-- * 'cReason' - A short (255 max characters) human-readable string to provide additional details about a running or stopped container.
--
-- * 'cName' - The name of the container.
--
-- * 'cExitCode' - The exit code returned from the container.
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

-- | A short (255 max characters) human-readable string to provide additional details about a running or stopped container.
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
--
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
    , _cdMemoryReservation      :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ContainerDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdImage' - The image used to start a container. This string is passed directly to the Docker daemon. Images in the Docker Hub registry are available by default. Other repositories are specified with @/repository-url/ //image/ :/tag/ @ . Up to 255 letters (uppercase and lowercase), numbers, hyphens, underscores, colons, periods, forward slashes, and number signs are allowed. This parameter maps to @Image@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @IMAGE@ parameter of <https://docs.docker.com/engine/reference/run/ docker run> .     * Images in Amazon ECR repositories use the full registry and repository URI (for example, @012345678910.dkr.ecr.<region-name>.amazonaws.com/<repository-name>@ ).      * Images in official repositories on Docker Hub use a single name (for example, @ubuntu@ or @mongo@ ).     * Images in other repositories on Docker Hub are qualified with an organization name (for example, @amazon/amazon-ecs-agent@ ).     * Images in other online repositories are qualified further by a domain name (for example, @quay.io/assemblyline/ubuntu@ ).
--
-- * 'cdCommand' - The command that is passed to the container. This parameter maps to @Cmd@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @COMMAND@ parameter to <https://docs.docker.com/engine/reference/run/ docker run> . For more information, see <https://docs.docker.com/engine/reference/builder/#cmd https://docs.docker.com/engine/reference/builder/#cmd> .
--
-- * 'cdHostname' - The hostname to use for your container. This parameter maps to @Hostname@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--hostname@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cdDockerSecurityOptions' - A list of strings to provide custom labels for SELinux and AppArmor multi-level security systems. This parameter maps to @SecurityOpt@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--security-opt@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cdDisableNetworking' - When this parameter is true, networking is disabled within the container. This parameter maps to @NetworkDisabled@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> .
--
-- * 'cdVolumesFrom' - Data volumes to mount from another container. This parameter maps to @VolumesFrom@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--volumes-from@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cdEnvironment' - The environment variables to pass to a container. This parameter maps to @Env@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--env@ option to <https://docs.docker.com/engine/reference/run/ docker run> . /Important:/ We do not recommend using plain text environment variables for sensitive information, such as credential data.
--
-- * 'cdEntryPoint' - /Important:/ Early versions of the Amazon ECS container agent do not properly handle @entryPoint@ parameters. If you have problems using @entryPoint@ , update your container agent or enter your commands and arguments as @command@ array items instead. The entry point that is passed to the container. This parameter maps to @Entrypoint@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--entrypoint@ option to <https://docs.docker.com/engine/reference/run/ docker run> . For more information, see <https://docs.docker.com/engine/reference/builder/#entrypoint https://docs.docker.com/engine/reference/builder/#entrypoint> .
--
-- * 'cdWorkingDirectory' - The working directory in which to run commands inside the container. This parameter maps to @WorkingDir@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--workdir@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cdUlimits' - A list of @ulimits@ to set in the container. This parameter maps to @Ulimits@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--ulimit@ option to <https://docs.docker.com/engine/reference/run/ docker run> . Valid naming values are displayed in the 'Ulimit' data type. This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@
--
-- * 'cdPrivileged' - When this parameter is true, the container is given elevated privileges on the host container instance (similar to the @root@ user). This parameter maps to @Privileged@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--privileged@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cdPortMappings' - The list of port mappings for the container. Port mappings allow containers to access ports on the host container instance to send or receive traffic. This parameter maps to @PortBindings@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--publish@ option to <https://docs.docker.com/engine/reference/run/ docker run> . If the network mode of a task definition is set to @none@ , then you cannot specify port mappings. If the network mode of a task definition is set to @host@ , then host ports must either be undefined or they must match the container port in the port mapping.
--
-- * 'cdDockerLabels' - A key/value map of labels to add to the container. This parameter maps to @Labels@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--label@ option to <https://docs.docker.com/engine/reference/run/ docker run> . This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@
--
-- * 'cdExtraHosts' - A list of hostnames and IP address mappings to append to the @/etc/hosts@ file on the container. This parameter maps to @ExtraHosts@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--add-host@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cdMemory' - The hard limit (in MiB) of memory to present to the container. If your container attempts to exceed the memory specified here, the container is killed. This parameter maps to @Memory@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--memory@ option to <https://docs.docker.com/engine/reference/run/ docker run> . You must specify a non-zero integer for one or both of @memory@ or @memoryReservation@ in container definitions. If you specify both, @memory@ must be greater than @memoryReservation@ . If you specify @memoryReservation@ , then that value is subtracted from the available memory resources for the container instance on which the container is placed; otherwise, the value of @memory@ is used. The Docker daemon reserves a minimum of 4 MiB of memory for a container, so you should not specify fewer than 4 MiB of memory for your containers.
--
-- * 'cdUser' - The user name to use inside the container. This parameter maps to @User@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--user@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cdDnsSearchDomains' - A list of DNS search domains that are presented to the container. This parameter maps to @DnsSearch@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--dns-search@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cdLogConfiguration' - The log configuration specification for the container. This parameter maps to @LogConfig@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--log-driver@ option to <https://docs.docker.com/engine/reference/run/ docker run> . By default, containers use the same logging driver that the Docker daemon uses; however the container may use a different logging driver than the Docker daemon by specifying a log driver with this parameter in the container definition. To use a different logging driver for a container, the log system must be configured properly on the container instance (or on a different log server for remote logging options). For more information on the options for different supported log drivers, see <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers> in the Docker documentation. This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@
--
-- * 'cdName' - The name of a container. If you are linking multiple containers together in a task definition, the @name@ of one container can be entered in the @links@ of another container to connect the containers. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. This parameter maps to @name@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--name@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cdDnsServers' - A list of DNS servers that are presented to the container. This parameter maps to @Dns@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--dns@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cdMountPoints' - The mount points for data volumes in your container. This parameter maps to @Volumes@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--volume@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cdLinks' - The @link@ parameter allows containers to communicate with each other without the need for port mappings, using the @name@ parameter and optionally, an @alias@ for the link. This construct is analogous to @name:alias@ in Docker links. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed for each @name@ and @alias@ . For more information on linking Docker containers, see <https://docs.docker.com/engine/userguide/networking/default_network/dockerlinks/ https://docs.docker.com/engine/userguide/networking/default_network/dockerlinks/> . This parameter maps to @Links@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--link@ option to <https://docs.docker.com/engine/reference/run/ docker run> . /Important:/ Containers that are collocated on a single container instance may be able to communicate with each other without requiring links or host port mappings. Network isolation is achieved on the container instance using security groups and VPC settings.
--
-- * 'cdReadonlyRootFilesystem' - When this parameter is true, the container is given read-only access to its root file system. This parameter maps to @ReadonlyRootfs@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--read-only@ option to @docker run@ .
--
-- * 'cdEssential' - If the @essential@ parameter of a container is marked as @true@ , and that container fails or stops for any reason, all other containers that are part of the task are stopped. If the @essential@ parameter of a container is marked as @false@ , then its failure does not affect the rest of the containers in a task. If this parameter is omitted, a container is assumed to be essential. All tasks must have at least one essential container. If you have an application that is composed of multiple containers, you should group containers that are used for a common purpose into components, and separate the different components into multiple task definitions. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/application_architecture.html Application Architecture> in the /Amazon EC2 Container Service Developer Guide/ .
--
-- * 'cdCpu' - The number of @cpu@ units reserved for the container. A container instance has 1,024 @cpu@ units for every CPU core. This parameter specifies the minimum amount of CPU to reserve for a container, and containers share unallocated CPU units with other containers on the instance with the same ratio as their allocated amount. This parameter maps to @CpuShares@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--cpu-shares@ option to <https://docs.docker.com/engine/reference/run/ docker run> . For example, if you run a single-container task on a single-core instance type with 512 CPU units specified for that container, and that is the only task running on the container instance, that container could use the full 1,024 CPU unit share at any given time. However, if you launched another copy of the same task on that container instance, each task would be guaranteed a minimum of 512 CPU units when needed, and each container could float to higher CPU usage if the other container was not using it, but if both tasks were 100% active all of the time, they would be limited to 512 CPU units. The Docker daemon on the container instance uses the CPU value to calculate the relative CPU share ratios for running containers. For more information, see <https://docs.docker.com/engine/reference/run/#cpu-share-constraint CPU share constraint> in the Docker documentation. The minimum valid CPU share value that the Linux kernel allows is 2; however, the CPU parameter is not required, and you can use CPU values below 2 in your container definitions. For CPU values below 2 (including null), the behavior varies based on your Amazon ECS container agent version:     * __Agent versions less than or equal to 1.1.0:__ Null and zero CPU values are passed to Docker as 0, which Docker then converts to 1,024 CPU shares. CPU values of 1 are passed to Docker as 1, which the Linux kernel converts to 2 CPU shares.     * __Agent versions greater than or equal to 1.2.0:__ Null, zero, and CPU values of 1 are passed to Docker as 2.
--
-- * 'cdMemoryReservation' - The soft limit (in MiB) of memory to reserve for the container. When system memory is under heavy contention, Docker attempts to keep the container memory to this soft limit; however, your container can consume more memory when it needs to, up to either the hard limit specified with the @memory@ parameter (if applicable), or all of the available memory on the container instance, whichever comes first. This parameter maps to @MemoryReservation@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--memory-reservation@ option to <https://docs.docker.com/engine/reference/run/ docker run> . You must specify a non-zero integer for one or both of @memory@ or @memoryReservation@ in container definitions. If you specify both, @memory@ must be greater than @memoryReservation@ . If you specify @memoryReservation@ , then that value is subtracted from the available memory resources for the container instance on which the container is placed; otherwise, the value of @memory@ is used. For example, if your container normally uses 128 MiB of memory, but occasionally bursts to 256 MiB of memory for short periods of time, you can set a @memoryReservation@ of 128 MiB, and a @memory@ hard limit of 300 MiB. This configuration would allow the container to only reserve 128 MiB of memory from the remaining resources on the container instance, but also allow the container to consume more memory resources when needed.
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
    , _cdMemoryReservation = Nothing
    }

-- | The image used to start a container. This string is passed directly to the Docker daemon. Images in the Docker Hub registry are available by default. Other repositories are specified with @/repository-url/ //image/ :/tag/ @ . Up to 255 letters (uppercase and lowercase), numbers, hyphens, underscores, colons, periods, forward slashes, and number signs are allowed. This parameter maps to @Image@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @IMAGE@ parameter of <https://docs.docker.com/engine/reference/run/ docker run> .     * Images in Amazon ECR repositories use the full registry and repository URI (for example, @012345678910.dkr.ecr.<region-name>.amazonaws.com/<repository-name>@ ).      * Images in official repositories on Docker Hub use a single name (for example, @ubuntu@ or @mongo@ ).     * Images in other repositories on Docker Hub are qualified with an organization name (for example, @amazon/amazon-ecs-agent@ ).     * Images in other online repositories are qualified further by a domain name (for example, @quay.io/assemblyline/ubuntu@ ).
cdImage :: Lens' ContainerDefinition (Maybe Text)
cdImage = lens _cdImage (\ s a -> s{_cdImage = a});

-- | The command that is passed to the container. This parameter maps to @Cmd@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @COMMAND@ parameter to <https://docs.docker.com/engine/reference/run/ docker run> . For more information, see <https://docs.docker.com/engine/reference/builder/#cmd https://docs.docker.com/engine/reference/builder/#cmd> .
cdCommand :: Lens' ContainerDefinition [Text]
cdCommand = lens _cdCommand (\ s a -> s{_cdCommand = a}) . _Default . _Coerce;

-- | The hostname to use for your container. This parameter maps to @Hostname@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--hostname@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cdHostname :: Lens' ContainerDefinition (Maybe Text)
cdHostname = lens _cdHostname (\ s a -> s{_cdHostname = a});

-- | A list of strings to provide custom labels for SELinux and AppArmor multi-level security systems. This parameter maps to @SecurityOpt@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--security-opt@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cdDockerSecurityOptions :: Lens' ContainerDefinition [Text]
cdDockerSecurityOptions = lens _cdDockerSecurityOptions (\ s a -> s{_cdDockerSecurityOptions = a}) . _Default . _Coerce;

-- | When this parameter is true, networking is disabled within the container. This parameter maps to @NetworkDisabled@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> .
cdDisableNetworking :: Lens' ContainerDefinition (Maybe Bool)
cdDisableNetworking = lens _cdDisableNetworking (\ s a -> s{_cdDisableNetworking = a});

-- | Data volumes to mount from another container. This parameter maps to @VolumesFrom@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--volumes-from@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cdVolumesFrom :: Lens' ContainerDefinition [VolumeFrom]
cdVolumesFrom = lens _cdVolumesFrom (\ s a -> s{_cdVolumesFrom = a}) . _Default . _Coerce;

-- | The environment variables to pass to a container. This parameter maps to @Env@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--env@ option to <https://docs.docker.com/engine/reference/run/ docker run> . /Important:/ We do not recommend using plain text environment variables for sensitive information, such as credential data.
cdEnvironment :: Lens' ContainerDefinition [KeyValuePair]
cdEnvironment = lens _cdEnvironment (\ s a -> s{_cdEnvironment = a}) . _Default . _Coerce;

-- | /Important:/ Early versions of the Amazon ECS container agent do not properly handle @entryPoint@ parameters. If you have problems using @entryPoint@ , update your container agent or enter your commands and arguments as @command@ array items instead. The entry point that is passed to the container. This parameter maps to @Entrypoint@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--entrypoint@ option to <https://docs.docker.com/engine/reference/run/ docker run> . For more information, see <https://docs.docker.com/engine/reference/builder/#entrypoint https://docs.docker.com/engine/reference/builder/#entrypoint> .
cdEntryPoint :: Lens' ContainerDefinition [Text]
cdEntryPoint = lens _cdEntryPoint (\ s a -> s{_cdEntryPoint = a}) . _Default . _Coerce;

-- | The working directory in which to run commands inside the container. This parameter maps to @WorkingDir@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--workdir@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cdWorkingDirectory :: Lens' ContainerDefinition (Maybe Text)
cdWorkingDirectory = lens _cdWorkingDirectory (\ s a -> s{_cdWorkingDirectory = a});

-- | A list of @ulimits@ to set in the container. This parameter maps to @Ulimits@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--ulimit@ option to <https://docs.docker.com/engine/reference/run/ docker run> . Valid naming values are displayed in the 'Ulimit' data type. This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@
cdUlimits :: Lens' ContainerDefinition [Ulimit]
cdUlimits = lens _cdUlimits (\ s a -> s{_cdUlimits = a}) . _Default . _Coerce;

-- | When this parameter is true, the container is given elevated privileges on the host container instance (similar to the @root@ user). This parameter maps to @Privileged@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--privileged@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cdPrivileged :: Lens' ContainerDefinition (Maybe Bool)
cdPrivileged = lens _cdPrivileged (\ s a -> s{_cdPrivileged = a});

-- | The list of port mappings for the container. Port mappings allow containers to access ports on the host container instance to send or receive traffic. This parameter maps to @PortBindings@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--publish@ option to <https://docs.docker.com/engine/reference/run/ docker run> . If the network mode of a task definition is set to @none@ , then you cannot specify port mappings. If the network mode of a task definition is set to @host@ , then host ports must either be undefined or they must match the container port in the port mapping.
cdPortMappings :: Lens' ContainerDefinition [PortMapping]
cdPortMappings = lens _cdPortMappings (\ s a -> s{_cdPortMappings = a}) . _Default . _Coerce;

-- | A key/value map of labels to add to the container. This parameter maps to @Labels@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--label@ option to <https://docs.docker.com/engine/reference/run/ docker run> . This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@
cdDockerLabels :: Lens' ContainerDefinition (HashMap Text Text)
cdDockerLabels = lens _cdDockerLabels (\ s a -> s{_cdDockerLabels = a}) . _Default . _Map;

-- | A list of hostnames and IP address mappings to append to the @/etc/hosts@ file on the container. This parameter maps to @ExtraHosts@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--add-host@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cdExtraHosts :: Lens' ContainerDefinition [HostEntry]
cdExtraHosts = lens _cdExtraHosts (\ s a -> s{_cdExtraHosts = a}) . _Default . _Coerce;

-- | The hard limit (in MiB) of memory to present to the container. If your container attempts to exceed the memory specified here, the container is killed. This parameter maps to @Memory@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--memory@ option to <https://docs.docker.com/engine/reference/run/ docker run> . You must specify a non-zero integer for one or both of @memory@ or @memoryReservation@ in container definitions. If you specify both, @memory@ must be greater than @memoryReservation@ . If you specify @memoryReservation@ , then that value is subtracted from the available memory resources for the container instance on which the container is placed; otherwise, the value of @memory@ is used. The Docker daemon reserves a minimum of 4 MiB of memory for a container, so you should not specify fewer than 4 MiB of memory for your containers.
cdMemory :: Lens' ContainerDefinition (Maybe Int)
cdMemory = lens _cdMemory (\ s a -> s{_cdMemory = a});

-- | The user name to use inside the container. This parameter maps to @User@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--user@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cdUser :: Lens' ContainerDefinition (Maybe Text)
cdUser = lens _cdUser (\ s a -> s{_cdUser = a});

-- | A list of DNS search domains that are presented to the container. This parameter maps to @DnsSearch@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--dns-search@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cdDnsSearchDomains :: Lens' ContainerDefinition [Text]
cdDnsSearchDomains = lens _cdDnsSearchDomains (\ s a -> s{_cdDnsSearchDomains = a}) . _Default . _Coerce;

-- | The log configuration specification for the container. This parameter maps to @LogConfig@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--log-driver@ option to <https://docs.docker.com/engine/reference/run/ docker run> . By default, containers use the same logging driver that the Docker daemon uses; however the container may use a different logging driver than the Docker daemon by specifying a log driver with this parameter in the container definition. To use a different logging driver for a container, the log system must be configured properly on the container instance (or on a different log server for remote logging options). For more information on the options for different supported log drivers, see <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers> in the Docker documentation. This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@
cdLogConfiguration :: Lens' ContainerDefinition (Maybe LogConfiguration)
cdLogConfiguration = lens _cdLogConfiguration (\ s a -> s{_cdLogConfiguration = a});

-- | The name of a container. If you are linking multiple containers together in a task definition, the @name@ of one container can be entered in the @links@ of another container to connect the containers. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. This parameter maps to @name@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--name@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cdName :: Lens' ContainerDefinition (Maybe Text)
cdName = lens _cdName (\ s a -> s{_cdName = a});

-- | A list of DNS servers that are presented to the container. This parameter maps to @Dns@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--dns@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cdDnsServers :: Lens' ContainerDefinition [Text]
cdDnsServers = lens _cdDnsServers (\ s a -> s{_cdDnsServers = a}) . _Default . _Coerce;

-- | The mount points for data volumes in your container. This parameter maps to @Volumes@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--volume@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cdMountPoints :: Lens' ContainerDefinition [MountPoint]
cdMountPoints = lens _cdMountPoints (\ s a -> s{_cdMountPoints = a}) . _Default . _Coerce;

-- | The @link@ parameter allows containers to communicate with each other without the need for port mappings, using the @name@ parameter and optionally, an @alias@ for the link. This construct is analogous to @name:alias@ in Docker links. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed for each @name@ and @alias@ . For more information on linking Docker containers, see <https://docs.docker.com/engine/userguide/networking/default_network/dockerlinks/ https://docs.docker.com/engine/userguide/networking/default_network/dockerlinks/> . This parameter maps to @Links@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--link@ option to <https://docs.docker.com/engine/reference/run/ docker run> . /Important:/ Containers that are collocated on a single container instance may be able to communicate with each other without requiring links or host port mappings. Network isolation is achieved on the container instance using security groups and VPC settings.
cdLinks :: Lens' ContainerDefinition [Text]
cdLinks = lens _cdLinks (\ s a -> s{_cdLinks = a}) . _Default . _Coerce;

-- | When this parameter is true, the container is given read-only access to its root file system. This parameter maps to @ReadonlyRootfs@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--read-only@ option to @docker run@ .
cdReadonlyRootFilesystem :: Lens' ContainerDefinition (Maybe Bool)
cdReadonlyRootFilesystem = lens _cdReadonlyRootFilesystem (\ s a -> s{_cdReadonlyRootFilesystem = a});

-- | If the @essential@ parameter of a container is marked as @true@ , and that container fails or stops for any reason, all other containers that are part of the task are stopped. If the @essential@ parameter of a container is marked as @false@ , then its failure does not affect the rest of the containers in a task. If this parameter is omitted, a container is assumed to be essential. All tasks must have at least one essential container. If you have an application that is composed of multiple containers, you should group containers that are used for a common purpose into components, and separate the different components into multiple task definitions. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/application_architecture.html Application Architecture> in the /Amazon EC2 Container Service Developer Guide/ .
cdEssential :: Lens' ContainerDefinition (Maybe Bool)
cdEssential = lens _cdEssential (\ s a -> s{_cdEssential = a});

-- | The number of @cpu@ units reserved for the container. A container instance has 1,024 @cpu@ units for every CPU core. This parameter specifies the minimum amount of CPU to reserve for a container, and containers share unallocated CPU units with other containers on the instance with the same ratio as their allocated amount. This parameter maps to @CpuShares@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--cpu-shares@ option to <https://docs.docker.com/engine/reference/run/ docker run> . For example, if you run a single-container task on a single-core instance type with 512 CPU units specified for that container, and that is the only task running on the container instance, that container could use the full 1,024 CPU unit share at any given time. However, if you launched another copy of the same task on that container instance, each task would be guaranteed a minimum of 512 CPU units when needed, and each container could float to higher CPU usage if the other container was not using it, but if both tasks were 100% active all of the time, they would be limited to 512 CPU units. The Docker daemon on the container instance uses the CPU value to calculate the relative CPU share ratios for running containers. For more information, see <https://docs.docker.com/engine/reference/run/#cpu-share-constraint CPU share constraint> in the Docker documentation. The minimum valid CPU share value that the Linux kernel allows is 2; however, the CPU parameter is not required, and you can use CPU values below 2 in your container definitions. For CPU values below 2 (including null), the behavior varies based on your Amazon ECS container agent version:     * __Agent versions less than or equal to 1.1.0:__ Null and zero CPU values are passed to Docker as 0, which Docker then converts to 1,024 CPU shares. CPU values of 1 are passed to Docker as 1, which the Linux kernel converts to 2 CPU shares.     * __Agent versions greater than or equal to 1.2.0:__ Null, zero, and CPU values of 1 are passed to Docker as 2.
cdCpu :: Lens' ContainerDefinition (Maybe Int)
cdCpu = lens _cdCpu (\ s a -> s{_cdCpu = a});

-- | The soft limit (in MiB) of memory to reserve for the container. When system memory is under heavy contention, Docker attempts to keep the container memory to this soft limit; however, your container can consume more memory when it needs to, up to either the hard limit specified with the @memory@ parameter (if applicable), or all of the available memory on the container instance, whichever comes first. This parameter maps to @MemoryReservation@ in the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.23/ Docker Remote API> and the @--memory-reservation@ option to <https://docs.docker.com/engine/reference/run/ docker run> . You must specify a non-zero integer for one or both of @memory@ or @memoryReservation@ in container definitions. If you specify both, @memory@ must be greater than @memoryReservation@ . If you specify @memoryReservation@ , then that value is subtracted from the available memory resources for the container instance on which the container is placed; otherwise, the value of @memory@ is used. For example, if your container normally uses 128 MiB of memory, but occasionally bursts to 256 MiB of memory for short periods of time, you can set a @memoryReservation@ of 128 MiB, and a @memory@ hard limit of 300 MiB. This configuration would allow the container to only reserve 128 MiB of memory from the remaining resources on the container instance, but also allow the container to consume more memory resources when needed.
cdMemoryReservation :: Lens' ContainerDefinition (Maybe Int)
cdMemoryReservation = lens _cdMemoryReservation (\ s a -> s{_cdMemoryReservation = a});

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
                     <*> (x .:? "cpu")
                     <*> (x .:? "memoryReservation"))

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
                  ("cpu" .=) <$> _cdCpu,
                  ("memoryReservation" .=) <$> _cdMemoryReservation])

-- | An EC2 instance that is running the Amazon ECS agent and has been registered with a cluster.
--
--
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
    , _ciVersion              :: !(Maybe Integer)
    , _ciPendingTasksCount    :: !(Maybe Int)
    , _ciRegisteredAt         :: !(Maybe POSIX)
    , _ciRegisteredResources  :: !(Maybe [Resource])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ContainerInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciStatus' - The status of the container instance. The valid values are @ACTIVE@ , @INACTIVE@ , or @DRAINING@ . @ACTIVE@ indicates that the container instance can accept tasks. @DRAINING@ indicates that new tasks are not placed on the container instance and any service tasks running on the container instance are removed if possible. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/container-instance-draining.html Container Instance Draining> in the /Amazon EC2 Container Service Developer Guide/ .
--
-- * 'ciRunningTasksCount' - The number of tasks on the container instance that are in the @RUNNING@ status.
--
-- * 'ciRemainingResources' - For most resource types, this parameter describes the remaining resources of the container instance that are available for new tasks. For port resource types, this parameter describes the ports that are reserved by the Amazon ECS container agent and any containers that have reserved port mappings; any port that is not specified here is available for new tasks.
--
-- * 'ciEc2InstanceId' - The EC2 instance ID of the container instance.
--
-- * 'ciContainerInstanceARN' - The Amazon Resource Name (ARN) of the container instance. The ARN contains the @arn:aws:ecs@ namespace, followed by the region of the container instance, the AWS account ID of the container instance owner, the @container-instance@ namespace, and then the container instance ID. For example, @arn:aws:ecs:/region/ :/aws_account_id/ :container-instance//container_instance_ID/ @ .
--
-- * 'ciAgentConnected' - This parameter returns @true@ if the agent is actually connected to Amazon ECS. Registered instances with an agent that may be unhealthy or stopped return @false@ , and instances without a connected agent cannot accept placement requests.
--
-- * 'ciVersionInfo' - The version information for the Amazon ECS container agent and Docker daemon running on the container instance.
--
-- * 'ciAgentUpdateStatus' - The status of the most recent agent update. If an update has never been requested, this value is @NULL@ .
--
-- * 'ciAttributes' - The attributes set for the container instance, either by the Amazon ECS container agent at instance registration or manually with the 'PutAttributes' operation.
--
-- * 'ciVersion' - The version counter for the container instance. Every time a container instance experiences a change that triggers a CloudWatch event, the version counter is incremented. If you are replicating your Amazon ECS container instance state with CloudWatch events, you can compare the version of a container instance reported by the Amazon ECS APIs with the version reported in CloudWatch events for the container instance (inside the @detail@ object) to verify that the version in your event stream is current.
--
-- * 'ciPendingTasksCount' - The number of tasks on the container instance that are in the @PENDING@ status.
--
-- * 'ciRegisteredAt' - The Unix timestamp for when the container instance was registered.
--
-- * 'ciRegisteredResources' - For most resource types, this parameter describes the registered resources on the container instance that are in use by current tasks. For port resource types, this parameter describes the ports that were reserved by the Amazon ECS container agent when it registered the container instance with Amazon ECS.
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
    , _ciVersion = Nothing
    , _ciPendingTasksCount = Nothing
    , _ciRegisteredAt = Nothing
    , _ciRegisteredResources = Nothing
    }

-- | The status of the container instance. The valid values are @ACTIVE@ , @INACTIVE@ , or @DRAINING@ . @ACTIVE@ indicates that the container instance can accept tasks. @DRAINING@ indicates that new tasks are not placed on the container instance and any service tasks running on the container instance are removed if possible. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/container-instance-draining.html Container Instance Draining> in the /Amazon EC2 Container Service Developer Guide/ .
ciStatus :: Lens' ContainerInstance (Maybe Text)
ciStatus = lens _ciStatus (\ s a -> s{_ciStatus = a});

-- | The number of tasks on the container instance that are in the @RUNNING@ status.
ciRunningTasksCount :: Lens' ContainerInstance (Maybe Int)
ciRunningTasksCount = lens _ciRunningTasksCount (\ s a -> s{_ciRunningTasksCount = a});

-- | For most resource types, this parameter describes the remaining resources of the container instance that are available for new tasks. For port resource types, this parameter describes the ports that are reserved by the Amazon ECS container agent and any containers that have reserved port mappings; any port that is not specified here is available for new tasks.
ciRemainingResources :: Lens' ContainerInstance [Resource]
ciRemainingResources = lens _ciRemainingResources (\ s a -> s{_ciRemainingResources = a}) . _Default . _Coerce;

-- | The EC2 instance ID of the container instance.
ciEc2InstanceId :: Lens' ContainerInstance (Maybe Text)
ciEc2InstanceId = lens _ciEc2InstanceId (\ s a -> s{_ciEc2InstanceId = a});

-- | The Amazon Resource Name (ARN) of the container instance. The ARN contains the @arn:aws:ecs@ namespace, followed by the region of the container instance, the AWS account ID of the container instance owner, the @container-instance@ namespace, and then the container instance ID. For example, @arn:aws:ecs:/region/ :/aws_account_id/ :container-instance//container_instance_ID/ @ .
ciContainerInstanceARN :: Lens' ContainerInstance (Maybe Text)
ciContainerInstanceARN = lens _ciContainerInstanceARN (\ s a -> s{_ciContainerInstanceARN = a});

-- | This parameter returns @true@ if the agent is actually connected to Amazon ECS. Registered instances with an agent that may be unhealthy or stopped return @false@ , and instances without a connected agent cannot accept placement requests.
ciAgentConnected :: Lens' ContainerInstance (Maybe Bool)
ciAgentConnected = lens _ciAgentConnected (\ s a -> s{_ciAgentConnected = a});

-- | The version information for the Amazon ECS container agent and Docker daemon running on the container instance.
ciVersionInfo :: Lens' ContainerInstance (Maybe VersionInfo)
ciVersionInfo = lens _ciVersionInfo (\ s a -> s{_ciVersionInfo = a});

-- | The status of the most recent agent update. If an update has never been requested, this value is @NULL@ .
ciAgentUpdateStatus :: Lens' ContainerInstance (Maybe AgentUpdateStatus)
ciAgentUpdateStatus = lens _ciAgentUpdateStatus (\ s a -> s{_ciAgentUpdateStatus = a});

-- | The attributes set for the container instance, either by the Amazon ECS container agent at instance registration or manually with the 'PutAttributes' operation.
ciAttributes :: Lens' ContainerInstance [Attribute]
ciAttributes = lens _ciAttributes (\ s a -> s{_ciAttributes = a}) . _Default . _Coerce;

-- | The version counter for the container instance. Every time a container instance experiences a change that triggers a CloudWatch event, the version counter is incremented. If you are replicating your Amazon ECS container instance state with CloudWatch events, you can compare the version of a container instance reported by the Amazon ECS APIs with the version reported in CloudWatch events for the container instance (inside the @detail@ object) to verify that the version in your event stream is current.
ciVersion :: Lens' ContainerInstance (Maybe Integer)
ciVersion = lens _ciVersion (\ s a -> s{_ciVersion = a});

-- | The number of tasks on the container instance that are in the @PENDING@ status.
ciPendingTasksCount :: Lens' ContainerInstance (Maybe Int)
ciPendingTasksCount = lens _ciPendingTasksCount (\ s a -> s{_ciPendingTasksCount = a});

-- | The Unix timestamp for when the container instance was registered.
ciRegisteredAt :: Lens' ContainerInstance (Maybe UTCTime)
ciRegisteredAt = lens _ciRegisteredAt (\ s a -> s{_ciRegisteredAt = a}) . mapping _Time;

-- | For most resource types, this parameter describes the registered resources on the container instance that are in use by current tasks. For port resource types, this parameter describes the ports that were reserved by the Amazon ECS container agent when it registered the container instance with Amazon ECS.
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
                     <*> (x .:? "version")
                     <*> (x .:? "pendingTasksCount")
                     <*> (x .:? "registeredAt")
                     <*> (x .:? "registeredResources" .!= mempty))

instance Hashable ContainerInstance

instance NFData ContainerInstance

-- | The overrides that should be sent to a container.
--
--
--
-- /See:/ 'containerOverride' smart constructor.
data ContainerOverride = ContainerOverride'
    { _coCommand           :: !(Maybe [Text])
    , _coEnvironment       :: !(Maybe [KeyValuePair])
    , _coMemory            :: !(Maybe Int)
    , _coName              :: !(Maybe Text)
    , _coCpu               :: !(Maybe Int)
    , _coMemoryReservation :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ContainerOverride' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coCommand' - The command to send to the container that overrides the default command from the Docker image or the task definition. You must also specify a container name.
--
-- * 'coEnvironment' - The environment variables to send to the container. You can add new environment variables, which are added to the container at launch, or you can override the existing environment variables from the Docker image or the task definition. You must also specify a container name.
--
-- * 'coMemory' - The hard limit (in MiB) of memory to present to the container, instead of the default value from the task definition. If your container attempts to exceed the memory specified here, the container is killed. You must also specify a container name.
--
-- * 'coName' - The name of the container that receives the override. This parameter is required if any override is specified.
--
-- * 'coCpu' - The number of @cpu@ units reserved for the container, instead of the default value from the task definition. You must also specify a container name.
--
-- * 'coMemoryReservation' - The soft limit (in MiB) of memory to reserve for the container, instead of the default value from the task definition. You must also specify a container name.
containerOverride
    :: ContainerOverride
containerOverride =
    ContainerOverride'
    { _coCommand = Nothing
    , _coEnvironment = Nothing
    , _coMemory = Nothing
    , _coName = Nothing
    , _coCpu = Nothing
    , _coMemoryReservation = Nothing
    }

-- | The command to send to the container that overrides the default command from the Docker image or the task definition. You must also specify a container name.
coCommand :: Lens' ContainerOverride [Text]
coCommand = lens _coCommand (\ s a -> s{_coCommand = a}) . _Default . _Coerce;

-- | The environment variables to send to the container. You can add new environment variables, which are added to the container at launch, or you can override the existing environment variables from the Docker image or the task definition. You must also specify a container name.
coEnvironment :: Lens' ContainerOverride [KeyValuePair]
coEnvironment = lens _coEnvironment (\ s a -> s{_coEnvironment = a}) . _Default . _Coerce;

-- | The hard limit (in MiB) of memory to present to the container, instead of the default value from the task definition. If your container attempts to exceed the memory specified here, the container is killed. You must also specify a container name.
coMemory :: Lens' ContainerOverride (Maybe Int)
coMemory = lens _coMemory (\ s a -> s{_coMemory = a});

-- | The name of the container that receives the override. This parameter is required if any override is specified.
coName :: Lens' ContainerOverride (Maybe Text)
coName = lens _coName (\ s a -> s{_coName = a});

-- | The number of @cpu@ units reserved for the container, instead of the default value from the task definition. You must also specify a container name.
coCpu :: Lens' ContainerOverride (Maybe Int)
coCpu = lens _coCpu (\ s a -> s{_coCpu = a});

-- | The soft limit (in MiB) of memory to reserve for the container, instead of the default value from the task definition. You must also specify a container name.
coMemoryReservation :: Lens' ContainerOverride (Maybe Int)
coMemoryReservation = lens _coMemoryReservation (\ s a -> s{_coMemoryReservation = a});

instance FromJSON ContainerOverride where
        parseJSON
          = withObject "ContainerOverride"
              (\ x ->
                 ContainerOverride' <$>
                   (x .:? "command" .!= mempty) <*>
                     (x .:? "environment" .!= mempty)
                     <*> (x .:? "memory")
                     <*> (x .:? "name")
                     <*> (x .:? "cpu")
                     <*> (x .:? "memoryReservation"))

instance Hashable ContainerOverride

instance NFData ContainerOverride

instance ToJSON ContainerOverride where
        toJSON ContainerOverride'{..}
          = object
              (catMaybes
                 [("command" .=) <$> _coCommand,
                  ("environment" .=) <$> _coEnvironment,
                  ("memory" .=) <$> _coMemory, ("name" .=) <$> _coName,
                  ("cpu" .=) <$> _coCpu,
                  ("memoryReservation" .=) <$> _coMemoryReservation])

-- | Details on a service within a cluster
--
--
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
    , _csPlacementConstraints    :: !(Maybe [PlacementConstraint])
    , _csEvents                  :: !(Maybe [ServiceEvent])
    , _csPlacementStrategy       :: !(Maybe [PlacementStrategy])
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
-- * 'csRunningCount' - The number of tasks in the cluster that are in the @RUNNING@ state.
--
-- * 'csStatus' - The status of the service. The valid values are @ACTIVE@ , @DRAINING@ , or @INACTIVE@ .
--
-- * 'csClusterARN' - The Amazon Resource Name (ARN) of the cluster that hosts the service.
--
-- * 'csCreatedAt' - The Unix timestamp for when the service was created.
--
-- * 'csDesiredCount' - The desired number of instantiations of the task definition to keep running on the service. This value is specified when the service is created with 'CreateService' , and it can be modified with 'UpdateService' .
--
-- * 'csLoadBalancers' - A list of Elastic Load Balancing load balancer objects, containing the load balancer name, the container name (as it appears in a container definition), and the container port to access from the load balancer.
--
-- * 'csPendingCount' - The number of tasks in the cluster that are in the @PENDING@ state.
--
-- * 'csPlacementConstraints' - The placement constraints for the tasks in the service.
--
-- * 'csEvents' - The event stream for your service. A maximum of 100 of the latest events are displayed.
--
-- * 'csPlacementStrategy' - The placement strategy that determines how tasks for the service are placed.
--
-- * 'csDeployments' - The current state of deployments for the service.
--
-- * 'csServiceName' - The name of your service. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. Service names must be unique within a cluster, but you can have similarly named services in multiple clusters within a region or across multiple regions.
--
-- * 'csServiceARN' - The Amazon Resource Name (ARN) that identifies the service. The ARN contains the @arn:aws:ecs@ namespace, followed by the region of the service, the AWS account ID of the service owner, the @service@ namespace, and then the service name. For example, @arn:aws:ecs:/region/ :/012345678910/ :service//my-service/ @ .
--
-- * 'csTaskDefinition' - The task definition to use for tasks in the service. This value is specified when the service is created with 'CreateService' , and it can be modified with 'UpdateService' .
--
-- * 'csRoleARN' - The Amazon Resource Name (ARN) of the IAM role associated with the service that allows the Amazon ECS container agent to register container instances with an Elastic Load Balancing load balancer.
--
-- * 'csDeploymentConfiguration' - Optional deployment parameters that control how many tasks run during the deployment and the ordering of stopping and starting tasks.
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
    , _csPlacementConstraints = Nothing
    , _csEvents = Nothing
    , _csPlacementStrategy = Nothing
    , _csDeployments = Nothing
    , _csServiceName = Nothing
    , _csServiceARN = Nothing
    , _csTaskDefinition = Nothing
    , _csRoleARN = Nothing
    , _csDeploymentConfiguration = Nothing
    }

-- | The number of tasks in the cluster that are in the @RUNNING@ state.
csRunningCount :: Lens' ContainerService (Maybe Int)
csRunningCount = lens _csRunningCount (\ s a -> s{_csRunningCount = a});

-- | The status of the service. The valid values are @ACTIVE@ , @DRAINING@ , or @INACTIVE@ .
csStatus :: Lens' ContainerService (Maybe Text)
csStatus = lens _csStatus (\ s a -> s{_csStatus = a});

-- | The Amazon Resource Name (ARN) of the cluster that hosts the service.
csClusterARN :: Lens' ContainerService (Maybe Text)
csClusterARN = lens _csClusterARN (\ s a -> s{_csClusterARN = a});

-- | The Unix timestamp for when the service was created.
csCreatedAt :: Lens' ContainerService (Maybe UTCTime)
csCreatedAt = lens _csCreatedAt (\ s a -> s{_csCreatedAt = a}) . mapping _Time;

-- | The desired number of instantiations of the task definition to keep running on the service. This value is specified when the service is created with 'CreateService' , and it can be modified with 'UpdateService' .
csDesiredCount :: Lens' ContainerService (Maybe Int)
csDesiredCount = lens _csDesiredCount (\ s a -> s{_csDesiredCount = a});

-- | A list of Elastic Load Balancing load balancer objects, containing the load balancer name, the container name (as it appears in a container definition), and the container port to access from the load balancer.
csLoadBalancers :: Lens' ContainerService [LoadBalancer]
csLoadBalancers = lens _csLoadBalancers (\ s a -> s{_csLoadBalancers = a}) . _Default . _Coerce;

-- | The number of tasks in the cluster that are in the @PENDING@ state.
csPendingCount :: Lens' ContainerService (Maybe Int)
csPendingCount = lens _csPendingCount (\ s a -> s{_csPendingCount = a});

-- | The placement constraints for the tasks in the service.
csPlacementConstraints :: Lens' ContainerService [PlacementConstraint]
csPlacementConstraints = lens _csPlacementConstraints (\ s a -> s{_csPlacementConstraints = a}) . _Default . _Coerce;

-- | The event stream for your service. A maximum of 100 of the latest events are displayed.
csEvents :: Lens' ContainerService [ServiceEvent]
csEvents = lens _csEvents (\ s a -> s{_csEvents = a}) . _Default . _Coerce;

-- | The placement strategy that determines how tasks for the service are placed.
csPlacementStrategy :: Lens' ContainerService [PlacementStrategy]
csPlacementStrategy = lens _csPlacementStrategy (\ s a -> s{_csPlacementStrategy = a}) . _Default . _Coerce;

-- | The current state of deployments for the service.
csDeployments :: Lens' ContainerService [Deployment]
csDeployments = lens _csDeployments (\ s a -> s{_csDeployments = a}) . _Default . _Coerce;

-- | The name of your service. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. Service names must be unique within a cluster, but you can have similarly named services in multiple clusters within a region or across multiple regions.
csServiceName :: Lens' ContainerService (Maybe Text)
csServiceName = lens _csServiceName (\ s a -> s{_csServiceName = a});

-- | The Amazon Resource Name (ARN) that identifies the service. The ARN contains the @arn:aws:ecs@ namespace, followed by the region of the service, the AWS account ID of the service owner, the @service@ namespace, and then the service name. For example, @arn:aws:ecs:/region/ :/012345678910/ :service//my-service/ @ .
csServiceARN :: Lens' ContainerService (Maybe Text)
csServiceARN = lens _csServiceARN (\ s a -> s{_csServiceARN = a});

-- | The task definition to use for tasks in the service. This value is specified when the service is created with 'CreateService' , and it can be modified with 'UpdateService' .
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
                     <*> (x .:? "placementConstraints" .!= mempty)
                     <*> (x .:? "events" .!= mempty)
                     <*> (x .:? "placementStrategy" .!= mempty)
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
--
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
-- * 'dRunningCount' - The number of tasks in the deployment that are in the @RUNNING@ status.
--
-- * 'dStatus' - The status of the deployment. Valid values are @PRIMARY@ (for the most recent deployment), @ACTIVE@ (for previous deployments that still have tasks running, but are being replaced with the @PRIMARY@ deployment), and @INACTIVE@ (for deployments that have been completely replaced).
--
-- * 'dCreatedAt' - The Unix timestamp for when the service was created.
--
-- * 'dDesiredCount' - The most recent desired count of tasks that was specified for the service to deploy or maintain.
--
-- * 'dPendingCount' - The number of tasks in the deployment that are in the @PENDING@ status.
--
-- * 'dId' - The ID of the deployment.
--
-- * 'dUpdatedAt' - The Unix timestamp for when the service was last updated.
--
-- * 'dTaskDefinition' - The most recent task definition that was specified for the service to use.
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

-- | The number of tasks in the deployment that are in the @RUNNING@ status.
dRunningCount :: Lens' Deployment (Maybe Int)
dRunningCount = lens _dRunningCount (\ s a -> s{_dRunningCount = a});

-- | The status of the deployment. Valid values are @PRIMARY@ (for the most recent deployment), @ACTIVE@ (for previous deployments that still have tasks running, but are being replaced with the @PRIMARY@ deployment), and @INACTIVE@ (for deployments that have been completely replaced).
dStatus :: Lens' Deployment (Maybe Text)
dStatus = lens _dStatus (\ s a -> s{_dStatus = a});

-- | The Unix timestamp for when the service was created.
dCreatedAt :: Lens' Deployment (Maybe UTCTime)
dCreatedAt = lens _dCreatedAt (\ s a -> s{_dCreatedAt = a}) . mapping _Time;

-- | The most recent desired count of tasks that was specified for the service to deploy or maintain.
dDesiredCount :: Lens' Deployment (Maybe Int)
dDesiredCount = lens _dDesiredCount (\ s a -> s{_dDesiredCount = a});

-- | The number of tasks in the deployment that are in the @PENDING@ status.
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
--
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
-- * 'dcMinimumHealthyPercent' - The lower limit (as a percentage of the service's @desiredCount@ ) of the number of running tasks that must remain in the @RUNNING@ state in a service during a deployment. The minimum healthy tasks during a deployment is the @desiredCount@ multiplied by @minimumHealthyPercent@ /100, rounded up to the nearest integer value.
--
-- * 'dcMaximumPercent' - The upper limit (as a percentage of the service's @desiredCount@ ) of the number of tasks that are allowed in the @RUNNING@ or @PENDING@ state in a service during a deployment. The maximum number of tasks during a deployment is the @desiredCount@ multiplied by @maximumPercent@ /100, rounded down to the nearest integer value.
deploymentConfiguration
    :: DeploymentConfiguration
deploymentConfiguration =
    DeploymentConfiguration'
    { _dcMinimumHealthyPercent = Nothing
    , _dcMaximumPercent = Nothing
    }

-- | The lower limit (as a percentage of the service's @desiredCount@ ) of the number of running tasks that must remain in the @RUNNING@ state in a service during a deployment. The minimum healthy tasks during a deployment is the @desiredCount@ multiplied by @minimumHealthyPercent@ /100, rounded up to the nearest integer value.
dcMinimumHealthyPercent :: Lens' DeploymentConfiguration (Maybe Int)
dcMinimumHealthyPercent = lens _dcMinimumHealthyPercent (\ s a -> s{_dcMinimumHealthyPercent = a});

-- | The upper limit (as a percentage of the service's @desiredCount@ ) of the number of tasks that are allowed in the @RUNNING@ or @PENDING@ state in a service during a deployment. The maximum number of tasks during a deployment is the @desiredCount@ multiplied by @maximumPercent@ /100, rounded down to the nearest integer value.
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
--
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
-- * 'fArn' - The Amazon Resource Name (ARN) of the failed resource.
--
-- * 'fReason' - The reason for the failure.
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

-- | Hostnames and IP address entries that are added to the @/etc/hosts@ file of a container via the @extraHosts@ parameter of its 'ContainerDefinition' .
--
--
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
-- * 'heHostname' - The hostname to use in the @/etc/hosts@ entry.
--
-- * 'heIpAddress' - The IP address to use in the @/etc/hosts@ entry.
hostEntry
    :: Text -- ^ 'heHostname'
    -> Text -- ^ 'heIpAddress'
    -> HostEntry
hostEntry pHostname_ pIpAddress_ =
    HostEntry'
    { _heHostname = pHostname_
    , _heIpAddress = pIpAddress_
    }

-- | The hostname to use in the @/etc/hosts@ entry.
heHostname :: Lens' HostEntry Text
heHostname = lens _heHostname (\ s a -> s{_heHostname = a});

-- | The IP address to use in the @/etc/hosts@ entry.
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
--
--
-- /See:/ 'hostVolumeProperties' smart constructor.
newtype HostVolumeProperties = HostVolumeProperties'
    { _hvpSourcePath :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'HostVolumeProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hvpSourcePath' - The path on the host container instance that is presented to the container. If this parameter is empty, then the Docker daemon has assigned a host path for you. If the @host@ parameter contains a @sourcePath@ file location, then the data volume persists at the specified location on the host container instance until you delete it manually. If the @sourcePath@ value does not exist on the host container instance, the Docker daemon creates it. If the location does exist, the contents of the source path folder are exported.
hostVolumeProperties
    :: HostVolumeProperties
hostVolumeProperties =
    HostVolumeProperties'
    { _hvpSourcePath = Nothing
    }

-- | The path on the host container instance that is presented to the container. If this parameter is empty, then the Docker daemon has assigned a host path for you. If the @host@ parameter contains a @sourcePath@ file location, then the data volume persists at the specified location on the host container instance until you delete it manually. If the @sourcePath@ value does not exist on the host container instance, the Docker daemon creates it. If the location does exist, the contents of the source path folder are exported.
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
--
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
-- * 'kvpValue' - The value of the key value pair. For environment variables, this is the value of the environment variable.
--
-- * 'kvpName' - The name of the key value pair. For environment variables, this is the name of the environment variable.
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
--
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
-- * 'lbLoadBalancerName' - The name of a Classic load balancer.
--
-- * 'lbContainerName' - The name of the container (as it appears in a container definition) to associate with the load balancer.
--
-- * 'lbTargetGroupARN' - The full Amazon Resource Name (ARN) of the Elastic Load Balancing target group associated with a service.
--
-- * 'lbContainerPort' - The port on the container to associate with the load balancer. This port must correspond to a @containerPort@ in the service's task definition. Your container instances must allow ingress traffic on the @hostPort@ of the port mapping.
loadBalancer
    :: LoadBalancer
loadBalancer =
    LoadBalancer'
    { _lbLoadBalancerName = Nothing
    , _lbContainerName = Nothing
    , _lbTargetGroupARN = Nothing
    , _lbContainerPort = Nothing
    }

-- | The name of a Classic load balancer.
lbLoadBalancerName :: Lens' LoadBalancer (Maybe Text)
lbLoadBalancerName = lens _lbLoadBalancerName (\ s a -> s{_lbLoadBalancerName = a});

-- | The name of the container (as it appears in a container definition) to associate with the load balancer.
lbContainerName :: Lens' LoadBalancer (Maybe Text)
lbContainerName = lens _lbContainerName (\ s a -> s{_lbContainerName = a});

-- | The full Amazon Resource Name (ARN) of the Elastic Load Balancing target group associated with a service.
lbTargetGroupARN :: Lens' LoadBalancer (Maybe Text)
lbTargetGroupARN = lens _lbTargetGroupARN (\ s a -> s{_lbTargetGroupARN = a});

-- | The port on the container to associate with the load balancer. This port must correspond to a @containerPort@ in the service's task definition. Your container instances must allow ingress traffic on the @hostPort@ of the port mapping.
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
--
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
-- * 'lcOptions' - The configuration options to send to the log driver. This parameter requires version 1.19 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@
--
-- * 'lcLogDriver' - The log driver to use for the container. The valid values listed for this parameter are log drivers that the Amazon ECS container agent can communicate with by default.  This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@
logConfiguration
    :: LogDriver -- ^ 'lcLogDriver'
    -> LogConfiguration
logConfiguration pLogDriver_ =
    LogConfiguration'
    { _lcOptions = Nothing
    , _lcLogDriver = pLogDriver_
    }

-- | The configuration options to send to the log driver. This parameter requires version 1.19 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@
lcOptions :: Lens' LogConfiguration (HashMap Text Text)
lcOptions = lens _lcOptions (\ s a -> s{_lcOptions = a}) . _Default . _Map;

-- | The log driver to use for the container. The valid values listed for this parameter are log drivers that the Amazon ECS container agent can communicate with by default.  This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@
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
--
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
-- * 'mpContainerPath' - The path on the container to mount the host volume at.
--
-- * 'mpSourceVolume' - The name of the volume to mount.
--
-- * 'mpReadOnly' - If this value is @true@ , the container has read-only access to the volume. If this value is @false@ , then the container can write to the volume. The default value is @false@ .
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

-- | If this value is @true@ , the container has read-only access to the volume. If this value is @false@ , then the container can write to the volume. The default value is @false@ .
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

-- | Details on the network bindings between a container and its host container instance. After a task reaches the @RUNNING@ status, manual and automatic host and container port assignments are visible in the @networkBindings@ section of 'DescribeTasks' API responses.
--
--
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
-- * 'nbBindIP' - The IP address that the container is bound to on the container instance.
--
-- * 'nbProtocol' - The protocol used for the network binding.
--
-- * 'nbHostPort' - The port number on the host that is used with the network binding.
--
-- * 'nbContainerPort' - The port number on the container that is be used with the network binding.
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

-- | An object representing a constraint on task placement. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html Task Placement Constraints> in the /Amazon EC2 Container Service Developer Guide/ .
--
--
--
-- /See:/ 'placementConstraint' smart constructor.
data PlacementConstraint = PlacementConstraint'
    { _pcExpression :: !(Maybe Text)
    , _pcType       :: !(Maybe PlacementConstraintType)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PlacementConstraint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcExpression' - A cluster query language expression to apply to the constraint. Note you cannot specify an expression if the constraint type is @distinctInstance@ . For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language> in the /Amazon EC2 Container Service Developer Guide/ .
--
-- * 'pcType' - The type of constraint. Use @distinctInstance@ to ensure that each task in a particular group is running on a different container instance. Use @memberOf@ to restrict selection to a group of valid candidates. Note that @distinctInstance@ is not supported in task definitions.
placementConstraint
    :: PlacementConstraint
placementConstraint =
    PlacementConstraint'
    { _pcExpression = Nothing
    , _pcType = Nothing
    }

-- | A cluster query language expression to apply to the constraint. Note you cannot specify an expression if the constraint type is @distinctInstance@ . For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language> in the /Amazon EC2 Container Service Developer Guide/ .
pcExpression :: Lens' PlacementConstraint (Maybe Text)
pcExpression = lens _pcExpression (\ s a -> s{_pcExpression = a});

-- | The type of constraint. Use @distinctInstance@ to ensure that each task in a particular group is running on a different container instance. Use @memberOf@ to restrict selection to a group of valid candidates. Note that @distinctInstance@ is not supported in task definitions.
pcType :: Lens' PlacementConstraint (Maybe PlacementConstraintType)
pcType = lens _pcType (\ s a -> s{_pcType = a});

instance FromJSON PlacementConstraint where
        parseJSON
          = withObject "PlacementConstraint"
              (\ x ->
                 PlacementConstraint' <$>
                   (x .:? "expression") <*> (x .:? "type"))

instance Hashable PlacementConstraint

instance NFData PlacementConstraint

instance ToJSON PlacementConstraint where
        toJSON PlacementConstraint'{..}
          = object
              (catMaybes
                 [("expression" .=) <$> _pcExpression,
                  ("type" .=) <$> _pcType])

-- | The task placement strategy for a task or service. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-strategies.html Task Placement Strategies> in the /Amazon EC2 Container Service Developer Guide/ .
--
--
--
-- /See:/ 'placementStrategy' smart constructor.
data PlacementStrategy = PlacementStrategy'
    { _psField :: !(Maybe Text)
    , _psType  :: !(Maybe PlacementStrategyType)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PlacementStrategy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psField' - The field to apply the placement strategy against. For the @spread@ placement strategy, valid values are @instanceId@ (or @host@ , which has the same effect), or any platform or custom attribute that is applied to a container instance, such as @attribute:ecs.availability-zone@ . For the @binpack@ placement strategy, valid values are @cpu@ and @memory@ . For the @random@ placement strategy, this field is not used.
--
-- * 'psType' - The type of placement strategy. The @random@ placement strategy randomly places tasks on available candidates. The @spread@ placement strategy spreads placement across available candidates evenly based on the @field@ parameter. The @binpack@ strategy places tasks on available candidates that have the least available amount of the resource that is specified with the @field@ parameter. For example, if you binpack on memory, a task is placed on the instance with the least amount of remaining memory (but still enough to run the task).
placementStrategy
    :: PlacementStrategy
placementStrategy =
    PlacementStrategy'
    { _psField = Nothing
    , _psType = Nothing
    }

-- | The field to apply the placement strategy against. For the @spread@ placement strategy, valid values are @instanceId@ (or @host@ , which has the same effect), or any platform or custom attribute that is applied to a container instance, such as @attribute:ecs.availability-zone@ . For the @binpack@ placement strategy, valid values are @cpu@ and @memory@ . For the @random@ placement strategy, this field is not used.
psField :: Lens' PlacementStrategy (Maybe Text)
psField = lens _psField (\ s a -> s{_psField = a});

-- | The type of placement strategy. The @random@ placement strategy randomly places tasks on available candidates. The @spread@ placement strategy spreads placement across available candidates evenly based on the @field@ parameter. The @binpack@ strategy places tasks on available candidates that have the least available amount of the resource that is specified with the @field@ parameter. For example, if you binpack on memory, a task is placed on the instance with the least amount of remaining memory (but still enough to run the task).
psType :: Lens' PlacementStrategy (Maybe PlacementStrategyType)
psType = lens _psType (\ s a -> s{_psType = a});

instance FromJSON PlacementStrategy where
        parseJSON
          = withObject "PlacementStrategy"
              (\ x ->
                 PlacementStrategy' <$>
                   (x .:? "field") <*> (x .:? "type"))

instance Hashable PlacementStrategy

instance NFData PlacementStrategy

instance ToJSON PlacementStrategy where
        toJSON PlacementStrategy'{..}
          = object
              (catMaybes
                 [("field" .=) <$> _psField, ("type" .=) <$> _psType])

-- | Port mappings allow containers to access ports on the host container instance to send or receive traffic. Port mappings are specified as part of the container definition. After a task reaches the @RUNNING@ status, manual and automatic host and container port assignments are visible in the @networkBindings@ section of 'DescribeTasks' API responses.
--
--
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
-- * 'pmProtocol' - The protocol used for the port mapping. Valid values are @tcp@ and @udp@ . The default is @tcp@ .
--
-- * 'pmHostPort' - The port number on the container instance to reserve for your container. You can specify a non-reserved host port for your container port mapping, or you can omit the @hostPort@ (or set it to @0@ ) while specifying a @containerPort@ and your container automatically receives a port in the ephemeral port range for your container instance operating system and Docker version. The default ephemeral port range for Docker version 1.6.0 and later is listed on the instance under @/proc/sys/net/ipv4/ip_local_port_range@ ; if this kernel parameter is unavailable, the default ephemeral port range of 49153 to 65535 is used. You should not attempt to specify a host port in the ephemeral port range as these are reserved for automatic assignment. In general, ports below 32768 are outside of the ephemeral port range. The default reserved ports are 22 for SSH, the Docker ports 2375 and 2376, and the Amazon ECS container agent ports 51678 and 51679. Any host port that was previously specified in a running task is also reserved while the task is running (after a task stops, the host port is released).The current reserved ports are displayed in the @remainingResources@ of 'DescribeContainerInstances' output, and a container instance may have up to 100 reserved ports at a time, including the default reserved ports (automatically assigned ports do not count toward the 100 reserved ports limit).
--
-- * 'pmContainerPort' - The port number on the container that is bound to the user-specified or automatically assigned host port. If you specify a container port and not a host port, your container automatically receives a host port in the ephemeral port range (for more information, see @hostPort@ ). Port mappings that are automatically assigned in this way do not count toward the 100 reserved ports limit of a container instance.
portMapping
    :: PortMapping
portMapping =
    PortMapping'
    { _pmProtocol = Nothing
    , _pmHostPort = Nothing
    , _pmContainerPort = Nothing
    }

-- | The protocol used for the port mapping. Valid values are @tcp@ and @udp@ . The default is @tcp@ .
pmProtocol :: Lens' PortMapping (Maybe TransportProtocol)
pmProtocol = lens _pmProtocol (\ s a -> s{_pmProtocol = a});

-- | The port number on the container instance to reserve for your container. You can specify a non-reserved host port for your container port mapping, or you can omit the @hostPort@ (or set it to @0@ ) while specifying a @containerPort@ and your container automatically receives a port in the ephemeral port range for your container instance operating system and Docker version. The default ephemeral port range for Docker version 1.6.0 and later is listed on the instance under @/proc/sys/net/ipv4/ip_local_port_range@ ; if this kernel parameter is unavailable, the default ephemeral port range of 49153 to 65535 is used. You should not attempt to specify a host port in the ephemeral port range as these are reserved for automatic assignment. In general, ports below 32768 are outside of the ephemeral port range. The default reserved ports are 22 for SSH, the Docker ports 2375 and 2376, and the Amazon ECS container agent ports 51678 and 51679. Any host port that was previously specified in a running task is also reserved while the task is running (after a task stops, the host port is released).The current reserved ports are displayed in the @remainingResources@ of 'DescribeContainerInstances' output, and a container instance may have up to 100 reserved ports at a time, including the default reserved ports (automatically assigned ports do not count toward the 100 reserved ports limit).
pmHostPort :: Lens' PortMapping (Maybe Int)
pmHostPort = lens _pmHostPort (\ s a -> s{_pmHostPort = a});

-- | The port number on the container that is bound to the user-specified or automatically assigned host port. If you specify a container port and not a host port, your container automatically receives a host port in the ephemeral port range (for more information, see @hostPort@ ). Port mappings that are automatically assigned in this way do not count toward the 100 reserved ports limit of a container instance.
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
--
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
-- * 'rStringSetValue' - When the @stringSetValue@ type is set, the value of the resource must be a string type.
--
-- * 'rIntegerValue' - When the @integerValue@ type is set, the value of the resource must be an integer.
--
-- * 'rDoubleValue' - When the @doubleValue@ type is set, the value of the resource must be a double precision floating-point type.
--
-- * 'rLongValue' - When the @longValue@ type is set, the value of the resource must be an extended precision floating-point type.
--
-- * 'rName' - The name of the resource, such as @cpu@ , @memory@ , @ports@ , or a user-defined resource.
--
-- * 'rType' - The type of the resource, such as @INTEGER@ , @DOUBLE@ , @LONG@ , or @STRINGSET@ .
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

-- | When the @stringSetValue@ type is set, the value of the resource must be a string type.
rStringSetValue :: Lens' Resource [Text]
rStringSetValue = lens _rStringSetValue (\ s a -> s{_rStringSetValue = a}) . _Default . _Coerce;

-- | When the @integerValue@ type is set, the value of the resource must be an integer.
rIntegerValue :: Lens' Resource (Maybe Int)
rIntegerValue = lens _rIntegerValue (\ s a -> s{_rIntegerValue = a});

-- | When the @doubleValue@ type is set, the value of the resource must be a double precision floating-point type.
rDoubleValue :: Lens' Resource (Maybe Double)
rDoubleValue = lens _rDoubleValue (\ s a -> s{_rDoubleValue = a});

-- | When the @longValue@ type is set, the value of the resource must be an extended precision floating-point type.
rLongValue :: Lens' Resource (Maybe Integer)
rLongValue = lens _rLongValue (\ s a -> s{_rLongValue = a});

-- | The name of the resource, such as @cpu@ , @memory@ , @ports@ , or a user-defined resource.
rName :: Lens' Resource (Maybe Text)
rName = lens _rName (\ s a -> s{_rName = a});

-- | The type of the resource, such as @INTEGER@ , @DOUBLE@ , @LONG@ , or @STRINGSET@ .
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
--
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
-- * 'seCreatedAt' - The Unix timestamp for when the event was triggered.
--
-- * 'seId' - The ID string of the event.
--
-- * 'seMessage' - The event message.
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
--
--
-- /See:/ 'task' smart constructor.
data Task = Task'
    { _tStoppedAt            :: !(Maybe POSIX)
    , _tDesiredStatus        :: !(Maybe Text)
    , _tOverrides            :: !(Maybe TaskOverride)
    , _tClusterARN           :: !(Maybe Text)
    , _tGroup                :: !(Maybe Text)
    , _tCreatedAt            :: !(Maybe POSIX)
    , _tTaskARN              :: !(Maybe Text)
    , _tContainerInstanceARN :: !(Maybe Text)
    , _tLastStatus           :: !(Maybe Text)
    , _tContainers           :: !(Maybe [Container])
    , _tStartedAt            :: !(Maybe POSIX)
    , _tVersion              :: !(Maybe Integer)
    , _tStartedBy            :: !(Maybe Text)
    , _tStoppedReason        :: !(Maybe Text)
    , _tTaskDefinitionARN    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Task' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tStoppedAt' - The Unix timestamp for when the task was stopped (the task transitioned from the @RUNNING@ state to the @STOPPED@ state).
--
-- * 'tDesiredStatus' - The desired status of the task.
--
-- * 'tOverrides' - One or more container overrides.
--
-- * 'tClusterARN' - The Amazon Resource Name (ARN) of the cluster that hosts the task.
--
-- * 'tGroup' - The name of the task group associated with the task.
--
-- * 'tCreatedAt' - The Unix timestamp for when the task was created (the task entered the @PENDING@ state).
--
-- * 'tTaskARN' - The Amazon Resource Name (ARN) of the task.
--
-- * 'tContainerInstanceARN' - The Amazon Resource Name (ARN) of the container instances that host the task.
--
-- * 'tLastStatus' - The last known status of the task.
--
-- * 'tContainers' - The containers associated with the task.
--
-- * 'tStartedAt' - The Unix timestamp for when the task was started (the task transitioned from the @PENDING@ state to the @RUNNING@ state).
--
-- * 'tVersion' - The version counter for the task. Every time a task experiences a change that triggers a CloudWatch event, the version counter is incremented. If you are replicating your Amazon ECS task state with CloudWatch events, you can compare the version of a task reported by the Amazon ECS APIs with the version reported in CloudWatch events for the task (inside the @detail@ object) to verify that the version in your event stream is current.
--
-- * 'tStartedBy' - The tag specified when a task is started. If the task is started by an Amazon ECS service, then the @startedBy@ parameter contains the deployment ID of the service that starts it.
--
-- * 'tStoppedReason' - The reason the task was stopped.
--
-- * 'tTaskDefinitionARN' - The Amazon Resource Name (ARN) of the task definition that creates the task.
task
    :: Task
task =
    Task'
    { _tStoppedAt = Nothing
    , _tDesiredStatus = Nothing
    , _tOverrides = Nothing
    , _tClusterARN = Nothing
    , _tGroup = Nothing
    , _tCreatedAt = Nothing
    , _tTaskARN = Nothing
    , _tContainerInstanceARN = Nothing
    , _tLastStatus = Nothing
    , _tContainers = Nothing
    , _tStartedAt = Nothing
    , _tVersion = Nothing
    , _tStartedBy = Nothing
    , _tStoppedReason = Nothing
    , _tTaskDefinitionARN = Nothing
    }

-- | The Unix timestamp for when the task was stopped (the task transitioned from the @RUNNING@ state to the @STOPPED@ state).
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

-- | The name of the task group associated with the task.
tGroup :: Lens' Task (Maybe Text)
tGroup = lens _tGroup (\ s a -> s{_tGroup = a});

-- | The Unix timestamp for when the task was created (the task entered the @PENDING@ state).
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

-- | The Unix timestamp for when the task was started (the task transitioned from the @PENDING@ state to the @RUNNING@ state).
tStartedAt :: Lens' Task (Maybe UTCTime)
tStartedAt = lens _tStartedAt (\ s a -> s{_tStartedAt = a}) . mapping _Time;

-- | The version counter for the task. Every time a task experiences a change that triggers a CloudWatch event, the version counter is incremented. If you are replicating your Amazon ECS task state with CloudWatch events, you can compare the version of a task reported by the Amazon ECS APIs with the version reported in CloudWatch events for the task (inside the @detail@ object) to verify that the version in your event stream is current.
tVersion :: Lens' Task (Maybe Integer)
tVersion = lens _tVersion (\ s a -> s{_tVersion = a});

-- | The tag specified when a task is started. If the task is started by an Amazon ECS service, then the @startedBy@ parameter contains the deployment ID of the service that starts it.
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
                     <*> (x .:? "group")
                     <*> (x .:? "createdAt")
                     <*> (x .:? "taskArn")
                     <*> (x .:? "containerInstanceArn")
                     <*> (x .:? "lastStatus")
                     <*> (x .:? "containers" .!= mempty)
                     <*> (x .:? "startedAt")
                     <*> (x .:? "version")
                     <*> (x .:? "startedBy")
                     <*> (x .:? "stoppedReason")
                     <*> (x .:? "taskDefinitionArn"))

instance Hashable Task

instance NFData Task

-- | Details of a task definition.
--
--
--
-- /See:/ 'taskDefinition' smart constructor.
data TaskDefinition = TaskDefinition'
    { _tdStatus               :: !(Maybe TaskDefinitionStatus)
    , _tdFamily               :: !(Maybe Text)
    , _tdContainerDefinitions :: !(Maybe [ContainerDefinition])
    , _tdTaskRoleARN          :: !(Maybe Text)
    , _tdPlacementConstraints :: !(Maybe [TaskDefinitionPlacementConstraint])
    , _tdNetworkMode          :: !(Maybe NetworkMode)
    , _tdTaskDefinitionARN    :: !(Maybe Text)
    , _tdRevision             :: !(Maybe Int)
    , _tdVolumes              :: !(Maybe [Volume])
    , _tdRequiresAttributes   :: !(Maybe [Attribute])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TaskDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdStatus' - The status of the task definition.
--
-- * 'tdFamily' - The family of your task definition, used as the definition name.
--
-- * 'tdContainerDefinitions' - A list of container definitions in JSON format that describe the different containers that make up your task. For more information about container definition parameters and defaults, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Amazon ECS Task Definitions> in the /Amazon EC2 Container Service Developer Guide/ .
--
-- * 'tdTaskRoleARN' - The Amazon Resource Name (ARN) of the IAM role that containers in this task can assume. All containers in this task are granted the permissions that are specified in this role.
--
-- * 'tdPlacementConstraints' - An array of placement constraint objects to use for tasks.
--
-- * 'tdNetworkMode' - The Docker networking mode to use for the containers in the task. The valid values are @none@ , @bridge@ , and @host@ .  If the network mode is @none@ , the containers do not have external connectivity. The default Docker network mode is @bridge@ . The @host@ network mode offers the highest networking performance for containers because it uses the host network stack instead of the virtualized network stack provided by the @bridge@ mode. For more information, see <https://docs.docker.com/engine/reference/run/#network-settings Network settings> in the /Docker run reference/ .
--
-- * 'tdTaskDefinitionARN' - The full Amazon Resource Name (ARN) of the task definition.
--
-- * 'tdRevision' - The revision of the task in a particular family. The revision is a version number of a task definition in a family. When you register a task definition for the first time, the revision is @1@ ; each time you register a new revision of a task definition in the same family, the revision value always increases by one (even if you have deregistered previous revisions in this family).
--
-- * 'tdVolumes' - The list of volumes in a task. For more information about volume definition parameters and defaults, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Amazon ECS Task Definitions> in the /Amazon EC2 Container Service Developer Guide/ .
--
-- * 'tdRequiresAttributes' - The container instance attributes required by your task.
taskDefinition
    :: TaskDefinition
taskDefinition =
    TaskDefinition'
    { _tdStatus = Nothing
    , _tdFamily = Nothing
    , _tdContainerDefinitions = Nothing
    , _tdTaskRoleARN = Nothing
    , _tdPlacementConstraints = Nothing
    , _tdNetworkMode = Nothing
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

-- | A list of container definitions in JSON format that describe the different containers that make up your task. For more information about container definition parameters and defaults, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Amazon ECS Task Definitions> in the /Amazon EC2 Container Service Developer Guide/ .
tdContainerDefinitions :: Lens' TaskDefinition [ContainerDefinition]
tdContainerDefinitions = lens _tdContainerDefinitions (\ s a -> s{_tdContainerDefinitions = a}) . _Default . _Coerce;

-- | The Amazon Resource Name (ARN) of the IAM role that containers in this task can assume. All containers in this task are granted the permissions that are specified in this role.
tdTaskRoleARN :: Lens' TaskDefinition (Maybe Text)
tdTaskRoleARN = lens _tdTaskRoleARN (\ s a -> s{_tdTaskRoleARN = a});

-- | An array of placement constraint objects to use for tasks.
tdPlacementConstraints :: Lens' TaskDefinition [TaskDefinitionPlacementConstraint]
tdPlacementConstraints = lens _tdPlacementConstraints (\ s a -> s{_tdPlacementConstraints = a}) . _Default . _Coerce;

-- | The Docker networking mode to use for the containers in the task. The valid values are @none@ , @bridge@ , and @host@ .  If the network mode is @none@ , the containers do not have external connectivity. The default Docker network mode is @bridge@ . The @host@ network mode offers the highest networking performance for containers because it uses the host network stack instead of the virtualized network stack provided by the @bridge@ mode. For more information, see <https://docs.docker.com/engine/reference/run/#network-settings Network settings> in the /Docker run reference/ .
tdNetworkMode :: Lens' TaskDefinition (Maybe NetworkMode)
tdNetworkMode = lens _tdNetworkMode (\ s a -> s{_tdNetworkMode = a});

-- | The full Amazon Resource Name (ARN) of the task definition.
tdTaskDefinitionARN :: Lens' TaskDefinition (Maybe Text)
tdTaskDefinitionARN = lens _tdTaskDefinitionARN (\ s a -> s{_tdTaskDefinitionARN = a});

-- | The revision of the task in a particular family. The revision is a version number of a task definition in a family. When you register a task definition for the first time, the revision is @1@ ; each time you register a new revision of a task definition in the same family, the revision value always increases by one (even if you have deregistered previous revisions in this family).
tdRevision :: Lens' TaskDefinition (Maybe Int)
tdRevision = lens _tdRevision (\ s a -> s{_tdRevision = a});

-- | The list of volumes in a task. For more information about volume definition parameters and defaults, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Amazon ECS Task Definitions> in the /Amazon EC2 Container Service Developer Guide/ .
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
                     <*> (x .:? "placementConstraints" .!= mempty)
                     <*> (x .:? "networkMode")
                     <*> (x .:? "taskDefinitionArn")
                     <*> (x .:? "revision")
                     <*> (x .:? "volumes" .!= mempty)
                     <*> (x .:? "requiresAttributes" .!= mempty))

instance Hashable TaskDefinition

instance NFData TaskDefinition

-- | An object representing a constraint on task placement in the task definition. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html Task Placement Constraints> in the /Amazon EC2 Container Service Developer Guide/ .
--
--
--
-- /See:/ 'taskDefinitionPlacementConstraint' smart constructor.
data TaskDefinitionPlacementConstraint = TaskDefinitionPlacementConstraint'
    { _tdpcExpression :: !(Maybe Text)
    , _tdpcType       :: !(Maybe TaskDefinitionPlacementConstraintType)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TaskDefinitionPlacementConstraint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdpcExpression' - A cluster query language expression to apply to the constraint. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language> in the /Amazon EC2 Container Service Developer Guide/ .
--
-- * 'tdpcType' - The type of constraint. The @DistinctInstance@ constraint ensures that each task in a particular group is running on a different container instance. The @MemberOf@ constraint restricts selection to be from a group of valid candidates.
taskDefinitionPlacementConstraint
    :: TaskDefinitionPlacementConstraint
taskDefinitionPlacementConstraint =
    TaskDefinitionPlacementConstraint'
    { _tdpcExpression = Nothing
    , _tdpcType = Nothing
    }

-- | A cluster query language expression to apply to the constraint. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language> in the /Amazon EC2 Container Service Developer Guide/ .
tdpcExpression :: Lens' TaskDefinitionPlacementConstraint (Maybe Text)
tdpcExpression = lens _tdpcExpression (\ s a -> s{_tdpcExpression = a});

-- | The type of constraint. The @DistinctInstance@ constraint ensures that each task in a particular group is running on a different container instance. The @MemberOf@ constraint restricts selection to be from a group of valid candidates.
tdpcType :: Lens' TaskDefinitionPlacementConstraint (Maybe TaskDefinitionPlacementConstraintType)
tdpcType = lens _tdpcType (\ s a -> s{_tdpcType = a});

instance FromJSON TaskDefinitionPlacementConstraint
         where
        parseJSON
          = withObject "TaskDefinitionPlacementConstraint"
              (\ x ->
                 TaskDefinitionPlacementConstraint' <$>
                   (x .:? "expression") <*> (x .:? "type"))

instance Hashable TaskDefinitionPlacementConstraint

instance NFData TaskDefinitionPlacementConstraint

instance ToJSON TaskDefinitionPlacementConstraint
         where
        toJSON TaskDefinitionPlacementConstraint'{..}
          = object
              (catMaybes
                 [("expression" .=) <$> _tdpcExpression,
                  ("type" .=) <$> _tdpcType])

-- | The overrides associated with a task.
--
--
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
-- * 'toContainerOverrides' - One or more container overrides sent to a task.
--
-- * 'toTaskRoleARN' - The Amazon Resource Name (ARN) of the IAM role that containers in this task can assume. All containers in this task are granted the permissions that are specified in this role.
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

-- | The @ulimit@ settings to pass to the container.
--
--
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
-- * 'uName' - The @type@ of the @ulimit@ .
--
-- * 'uSoftLimit' - The soft limit for the ulimit type.
--
-- * 'uHardLimit' - The hard limit for the ulimit type.
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

-- | The @type@ of the @ulimit@ .
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
--
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
-- * 'viAgentHash' - The Git commit hash for the Amazon ECS container agent build on the <https://github.com/aws/amazon-ecs-agent/commits/master amazon-ecs-agent > GitHub repository.
--
-- * 'viAgentVersion' - The version number of the Amazon ECS container agent.
--
-- * 'viDockerVersion' - The Docker version running on the container instance.
versionInfo
    :: VersionInfo
versionInfo =
    VersionInfo'
    { _viAgentHash = Nothing
    , _viAgentVersion = Nothing
    , _viDockerVersion = Nothing
    }

-- | The Git commit hash for the Amazon ECS container agent build on the <https://github.com/aws/amazon-ecs-agent/commits/master amazon-ecs-agent > GitHub repository.
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
--
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
-- * 'vName' - The name of the volume. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. This name is referenced in the @sourceVolume@ parameter of container definition @mountPoints@ .
--
-- * 'vHost' - The contents of the @host@ parameter determine whether your data volume persists on the host container instance and where it is stored. If the host parameter is empty, then the Docker daemon assigns a host path for your data volume, but the data is not guaranteed to persist after the containers associated with it stop running.
volume
    :: Volume
volume =
    Volume'
    { _vName = Nothing
    , _vHost = Nothing
    }

-- | The name of the volume. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. This name is referenced in the @sourceVolume@ parameter of container definition @mountPoints@ .
vName :: Lens' Volume (Maybe Text)
vName = lens _vName (\ s a -> s{_vName = a});

-- | The contents of the @host@ parameter determine whether your data volume persists on the host container instance and where it is stored. If the host parameter is empty, then the Docker daemon assigns a host path for your data volume, but the data is not guaranteed to persist after the containers associated with it stop running.
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

-- | Details on a data volume from another container in the same task definition.
--
--
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
-- * 'vfSourceContainer' - The name of another container within the same task definition to mount volumes from.
--
-- * 'vfReadOnly' - If this value is @true@ , the container has read-only access to the volume. If this value is @false@ , then the container can write to the volume. The default value is @false@ .
volumeFrom
    :: VolumeFrom
volumeFrom =
    VolumeFrom'
    { _vfSourceContainer = Nothing
    , _vfReadOnly = Nothing
    }

-- | The name of another container within the same task definition to mount volumes from.
vfSourceContainer :: Lens' VolumeFrom (Maybe Text)
vfSourceContainer = lens _vfSourceContainer (\ s a -> s{_vfSourceContainer = a});

-- | If this value is @true@ , the container has read-only access to the volume. If this value is @false@ , then the container can write to the volume. The default value is @false@ .
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
