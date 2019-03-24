{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.Product where

import Network.AWS.ECS.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing the networking details for a task or service.
--
--
--
-- /See:/ 'awsVPCConfiguration' smart constructor.
data AWSVPCConfiguration = AWSVPCConfiguration'
  { _avcSecurityGroups :: !(Maybe [Text])
  , _avcAssignPublicIP :: !(Maybe AssignPublicIP)
  , _avcSubnets        :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AWSVPCConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avcSecurityGroups' - The security groups associated with the task or service. If you do not specify a security group, the default security group for the VPC is used. There is a limit of 5 security groups that can be specified per @AwsVpcConfiguration@ .
--
-- * 'avcAssignPublicIP' - Whether the task's elastic network interface receives a public IP address. The default value is @DISABLED@ .
--
-- * 'avcSubnets' - The subnets associated with the task or service. There is a limit of 16 subnets that can be specified per @AwsVpcConfiguration@ .
awsVPCConfiguration
    :: AWSVPCConfiguration
awsVPCConfiguration =
  AWSVPCConfiguration'
    { _avcSecurityGroups = Nothing
    , _avcAssignPublicIP = Nothing
    , _avcSubnets = mempty
    }


-- | The security groups associated with the task or service. If you do not specify a security group, the default security group for the VPC is used. There is a limit of 5 security groups that can be specified per @AwsVpcConfiguration@ .
avcSecurityGroups :: Lens' AWSVPCConfiguration [Text]
avcSecurityGroups = lens _avcSecurityGroups (\ s a -> s{_avcSecurityGroups = a}) . _Default . _Coerce

-- | Whether the task's elastic network interface receives a public IP address. The default value is @DISABLED@ .
avcAssignPublicIP :: Lens' AWSVPCConfiguration (Maybe AssignPublicIP)
avcAssignPublicIP = lens _avcAssignPublicIP (\ s a -> s{_avcAssignPublicIP = a})

-- | The subnets associated with the task or service. There is a limit of 16 subnets that can be specified per @AwsVpcConfiguration@ .
avcSubnets :: Lens' AWSVPCConfiguration [Text]
avcSubnets = lens _avcSubnets (\ s a -> s{_avcSubnets = a}) . _Coerce

instance FromJSON AWSVPCConfiguration where
        parseJSON
          = withObject "AWSVPCConfiguration"
              (\ x ->
                 AWSVPCConfiguration' <$>
                   (x .:? "securityGroups" .!= mempty) <*>
                     (x .:? "assignPublicIp")
                     <*> (x .:? "subnets" .!= mempty))

instance Hashable AWSVPCConfiguration where

instance NFData AWSVPCConfiguration where

instance ToJSON AWSVPCConfiguration where
        toJSON AWSVPCConfiguration'{..}
          = object
              (catMaybes
                 [("securityGroups" .=) <$> _avcSecurityGroups,
                  ("assignPublicIp" .=) <$> _avcAssignPublicIP,
                  Just ("subnets" .= _avcSubnets)])

-- | An object representing a container instance or task attachment.
--
--
--
-- /See:/ 'attachment' smart constructor.
data Attachment = Attachment'
  { _aStatus  :: !(Maybe Text)
  , _aDetails :: !(Maybe [KeyValuePair])
  , _aId      :: !(Maybe Text)
  , _aType    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Attachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aStatus' - The status of the attachment. Valid values are @PRECREATED@ , @CREATED@ , @ATTACHING@ , @ATTACHED@ , @DETACHING@ , @DETACHED@ , and @DELETED@ .
--
-- * 'aDetails' - Details of the attachment. For elastic network interfaces, this includes the network interface ID, the MAC address, the subnet ID, and the private IPv4 address.
--
-- * 'aId' - The unique identifier for the attachment.
--
-- * 'aType' - The type of the attachment, such as @ElasticNetworkInterface@ .
attachment
    :: Attachment
attachment =
  Attachment'
    {_aStatus = Nothing, _aDetails = Nothing, _aId = Nothing, _aType = Nothing}


-- | The status of the attachment. Valid values are @PRECREATED@ , @CREATED@ , @ATTACHING@ , @ATTACHED@ , @DETACHING@ , @DETACHED@ , and @DELETED@ .
aStatus :: Lens' Attachment (Maybe Text)
aStatus = lens _aStatus (\ s a -> s{_aStatus = a})

-- | Details of the attachment. For elastic network interfaces, this includes the network interface ID, the MAC address, the subnet ID, and the private IPv4 address.
aDetails :: Lens' Attachment [KeyValuePair]
aDetails = lens _aDetails (\ s a -> s{_aDetails = a}) . _Default . _Coerce

-- | The unique identifier for the attachment.
aId :: Lens' Attachment (Maybe Text)
aId = lens _aId (\ s a -> s{_aId = a})

-- | The type of the attachment, such as @ElasticNetworkInterface@ .
aType :: Lens' Attachment (Maybe Text)
aType = lens _aType (\ s a -> s{_aType = a})

instance FromJSON Attachment where
        parseJSON
          = withObject "Attachment"
              (\ x ->
                 Attachment' <$>
                   (x .:? "status") <*> (x .:? "details" .!= mempty) <*>
                     (x .:? "id")
                     <*> (x .:? "type"))

instance Hashable Attachment where

instance NFData Attachment where

-- | An object representing a change in state for a task attachment.
--
--
--
-- /See:/ 'attachmentStateChange' smart constructor.
data AttachmentStateChange = AttachmentStateChange'
  { _ascAttachmentARN :: !Text
  , _ascStatus        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachmentStateChange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ascAttachmentARN' - The Amazon Resource Name (ARN) of the attachment.
--
-- * 'ascStatus' - The status of the attachment.
attachmentStateChange
    :: Text -- ^ 'ascAttachmentARN'
    -> Text -- ^ 'ascStatus'
    -> AttachmentStateChange
attachmentStateChange pAttachmentARN_ pStatus_ =
  AttachmentStateChange'
    {_ascAttachmentARN = pAttachmentARN_, _ascStatus = pStatus_}


-- | The Amazon Resource Name (ARN) of the attachment.
ascAttachmentARN :: Lens' AttachmentStateChange Text
ascAttachmentARN = lens _ascAttachmentARN (\ s a -> s{_ascAttachmentARN = a})

-- | The status of the attachment.
ascStatus :: Lens' AttachmentStateChange Text
ascStatus = lens _ascStatus (\ s a -> s{_ascStatus = a})

instance Hashable AttachmentStateChange where

instance NFData AttachmentStateChange where

instance ToJSON AttachmentStateChange where
        toJSON AttachmentStateChange'{..}
          = object
              (catMaybes
                 [Just ("attachmentArn" .= _ascAttachmentARN),
                  Just ("status" .= _ascStatus)])

-- | An attribute is a name-value pair associated with an Amazon ECS object. Attributes enable you to extend the Amazon ECS data model by adding custom metadata to your resources. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html#attributes Attributes> in the /Amazon Elastic Container Service Developer Guide/ .
--
--
--
-- /See:/ 'attribute' smart constructor.
data Attribute = Attribute'
  { _aTargetId   :: !(Maybe Text)
  , _aValue      :: !(Maybe Text)
  , _aTargetType :: !(Maybe TargetType)
  , _aName       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Attribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aTargetId' - The ID of the target. You can specify the short form ID for a resource or the full Amazon Resource Name (ARN).
--
-- * 'aValue' - The value of the attribute. Up to 128 letters (uppercase and lowercase), numbers, hyphens, underscores, periods, at signs (@), forward slashes, colons, and spaces are allowed.
--
-- * 'aTargetType' - The type of the target with which to attach the attribute. This parameter is required if you use the short form ID for a resource instead of the full ARN.
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
aTargetId = lens _aTargetId (\ s a -> s{_aTargetId = a})

-- | The value of the attribute. Up to 128 letters (uppercase and lowercase), numbers, hyphens, underscores, periods, at signs (@), forward slashes, colons, and spaces are allowed.
aValue :: Lens' Attribute (Maybe Text)
aValue = lens _aValue (\ s a -> s{_aValue = a})

-- | The type of the target with which to attach the attribute. This parameter is required if you use the short form ID for a resource instead of the full ARN.
aTargetType :: Lens' Attribute (Maybe TargetType)
aTargetType = lens _aTargetType (\ s a -> s{_aTargetType = a})

-- | The name of the attribute. Up to 128 letters (uppercase and lowercase), numbers, hyphens, underscores, and periods are allowed.
aName :: Lens' Attribute Text
aName = lens _aName (\ s a -> s{_aName = a})

instance FromJSON Attribute where
        parseJSON
          = withObject "Attribute"
              (\ x ->
                 Attribute' <$>
                   (x .:? "targetId") <*> (x .:? "value") <*>
                     (x .:? "targetType")
                     <*> (x .: "name"))

instance Hashable Attribute where

instance NFData Attribute where

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
  , _cStatistics                        :: !(Maybe [KeyValuePair])
  , _cActiveServicesCount               :: !(Maybe Int)
  , _cTags                              :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Cluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cStatus' - The status of the cluster. The valid values are @ACTIVE@ or @INACTIVE@ . @ACTIVE@ indicates that you can register container instances with the cluster and the associated instances can accept tasks.
--
-- * 'cClusterARN' - The Amazon Resource Name (ARN) that identifies the cluster. The ARN contains the @arn:aws:ecs@ namespace, followed by the Region of the cluster, the AWS account ID of the cluster owner, the @cluster@ namespace, and then the cluster name. For example, @arn:aws:ecs:/region/ :/012345678910/ :cluster//test/ @ ..
--
-- * 'cRunningTasksCount' - The number of tasks in the cluster that are in the @RUNNING@ state.
--
-- * 'cRegisteredContainerInstancesCount' - The number of container instances registered into the cluster. This includes container instances in both @ACTIVE@ and @DRAINING@ status.
--
-- * 'cPendingTasksCount' - The number of tasks in the cluster that are in the @PENDING@ state.
--
-- * 'cClusterName' - A user-generated string that you use to identify your cluster.
--
-- * 'cStatistics' - Additional information about your clusters that are separated by launch type, including:     * runningEC2TasksCount     * RunningFargateTasksCount     * pendingEC2TasksCount     * pendingFargateTasksCount     * activeEC2ServiceCount     * activeFargateServiceCount     * drainingEC2ServiceCount     * drainingFargateServiceCount
--
-- * 'cActiveServicesCount' - The number of services that are running on the cluster in an @ACTIVE@ state. You can view these services with 'ListServices' .
--
-- * 'cTags' - The metadata that you apply to the cluster to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
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
    , _cStatistics = Nothing
    , _cActiveServicesCount = Nothing
    , _cTags = Nothing
    }


-- | The status of the cluster. The valid values are @ACTIVE@ or @INACTIVE@ . @ACTIVE@ indicates that you can register container instances with the cluster and the associated instances can accept tasks.
cStatus :: Lens' Cluster (Maybe Text)
cStatus = lens _cStatus (\ s a -> s{_cStatus = a})

-- | The Amazon Resource Name (ARN) that identifies the cluster. The ARN contains the @arn:aws:ecs@ namespace, followed by the Region of the cluster, the AWS account ID of the cluster owner, the @cluster@ namespace, and then the cluster name. For example, @arn:aws:ecs:/region/ :/012345678910/ :cluster//test/ @ ..
cClusterARN :: Lens' Cluster (Maybe Text)
cClusterARN = lens _cClusterARN (\ s a -> s{_cClusterARN = a})

-- | The number of tasks in the cluster that are in the @RUNNING@ state.
cRunningTasksCount :: Lens' Cluster (Maybe Int)
cRunningTasksCount = lens _cRunningTasksCount (\ s a -> s{_cRunningTasksCount = a})

-- | The number of container instances registered into the cluster. This includes container instances in both @ACTIVE@ and @DRAINING@ status.
cRegisteredContainerInstancesCount :: Lens' Cluster (Maybe Int)
cRegisteredContainerInstancesCount = lens _cRegisteredContainerInstancesCount (\ s a -> s{_cRegisteredContainerInstancesCount = a})

-- | The number of tasks in the cluster that are in the @PENDING@ state.
cPendingTasksCount :: Lens' Cluster (Maybe Int)
cPendingTasksCount = lens _cPendingTasksCount (\ s a -> s{_cPendingTasksCount = a})

-- | A user-generated string that you use to identify your cluster.
cClusterName :: Lens' Cluster (Maybe Text)
cClusterName = lens _cClusterName (\ s a -> s{_cClusterName = a})

-- | Additional information about your clusters that are separated by launch type, including:     * runningEC2TasksCount     * RunningFargateTasksCount     * pendingEC2TasksCount     * pendingFargateTasksCount     * activeEC2ServiceCount     * activeFargateServiceCount     * drainingEC2ServiceCount     * drainingFargateServiceCount
cStatistics :: Lens' Cluster [KeyValuePair]
cStatistics = lens _cStatistics (\ s a -> s{_cStatistics = a}) . _Default . _Coerce

-- | The number of services that are running on the cluster in an @ACTIVE@ state. You can view these services with 'ListServices' .
cActiveServicesCount :: Lens' Cluster (Maybe Int)
cActiveServicesCount = lens _cActiveServicesCount (\ s a -> s{_cActiveServicesCount = a})

-- | The metadata that you apply to the cluster to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
cTags :: Lens' Cluster [Tag]
cTags = lens _cTags (\ s a -> s{_cTags = a}) . _Default . _Coerce

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
                     <*> (x .:? "statistics" .!= mempty)
                     <*> (x .:? "activeServicesCount")
                     <*> (x .:? "tags" .!= mempty))

instance Hashable Cluster where

instance NFData Cluster where

-- | A Docker container that is part of a task.
--
--
--
-- /See:/ 'container' smart constructor.
data Container = Container'
  { _cGpuIds            :: !(Maybe [Text])
  , _cNetworkBindings   :: !(Maybe [NetworkBinding])
  , _cContainerARN      :: !(Maybe Text)
  , _cNetworkInterfaces :: !(Maybe [NetworkInterface])
  , _cTaskARN           :: !(Maybe Text)
  , _cLastStatus        :: !(Maybe Text)
  , _cMemory            :: !(Maybe Text)
  , _cReason            :: !(Maybe Text)
  , _cName              :: !(Maybe Text)
  , _cExitCode          :: !(Maybe Int)
  , _cHealthStatus      :: !(Maybe HealthStatus)
  , _cCpu               :: !(Maybe Text)
  , _cMemoryReservation :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Container' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cGpuIds' - The IDs of each GPU assigned to the container.
--
-- * 'cNetworkBindings' - The network bindings associated with the container.
--
-- * 'cContainerARN' - The Amazon Resource Name (ARN) of the container.
--
-- * 'cNetworkInterfaces' - The network interfaces associated with the container.
--
-- * 'cTaskARN' - The ARN of the task.
--
-- * 'cLastStatus' - The last known status of the container.
--
-- * 'cMemory' - The hard limit (in MiB) of memory set for the container.
--
-- * 'cReason' - A short (255 max characters) human-readable string to provide additional details about a running or stopped container.
--
-- * 'cName' - The name of the container.
--
-- * 'cExitCode' - The exit code returned from the container.
--
-- * 'cHealthStatus' - The health status of the container. If health checks are not configured for this container in its task definition, then it reports the health status as @UNKNOWN@ .
--
-- * 'cCpu' - The number of CPU units set for the container. The value will be @0@ if no value was specified in the container definition when the task definition was registered.
--
-- * 'cMemoryReservation' - The soft limit (in MiB) of memory set for the container.
container
    :: Container
container =
  Container'
    { _cGpuIds = Nothing
    , _cNetworkBindings = Nothing
    , _cContainerARN = Nothing
    , _cNetworkInterfaces = Nothing
    , _cTaskARN = Nothing
    , _cLastStatus = Nothing
    , _cMemory = Nothing
    , _cReason = Nothing
    , _cName = Nothing
    , _cExitCode = Nothing
    , _cHealthStatus = Nothing
    , _cCpu = Nothing
    , _cMemoryReservation = Nothing
    }


-- | The IDs of each GPU assigned to the container.
cGpuIds :: Lens' Container [Text]
cGpuIds = lens _cGpuIds (\ s a -> s{_cGpuIds = a}) . _Default . _Coerce

-- | The network bindings associated with the container.
cNetworkBindings :: Lens' Container [NetworkBinding]
cNetworkBindings = lens _cNetworkBindings (\ s a -> s{_cNetworkBindings = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the container.
cContainerARN :: Lens' Container (Maybe Text)
cContainerARN = lens _cContainerARN (\ s a -> s{_cContainerARN = a})

-- | The network interfaces associated with the container.
cNetworkInterfaces :: Lens' Container [NetworkInterface]
cNetworkInterfaces = lens _cNetworkInterfaces (\ s a -> s{_cNetworkInterfaces = a}) . _Default . _Coerce

-- | The ARN of the task.
cTaskARN :: Lens' Container (Maybe Text)
cTaskARN = lens _cTaskARN (\ s a -> s{_cTaskARN = a})

-- | The last known status of the container.
cLastStatus :: Lens' Container (Maybe Text)
cLastStatus = lens _cLastStatus (\ s a -> s{_cLastStatus = a})

-- | The hard limit (in MiB) of memory set for the container.
cMemory :: Lens' Container (Maybe Text)
cMemory = lens _cMemory (\ s a -> s{_cMemory = a})

-- | A short (255 max characters) human-readable string to provide additional details about a running or stopped container.
cReason :: Lens' Container (Maybe Text)
cReason = lens _cReason (\ s a -> s{_cReason = a})

-- | The name of the container.
cName :: Lens' Container (Maybe Text)
cName = lens _cName (\ s a -> s{_cName = a})

-- | The exit code returned from the container.
cExitCode :: Lens' Container (Maybe Int)
cExitCode = lens _cExitCode (\ s a -> s{_cExitCode = a})

-- | The health status of the container. If health checks are not configured for this container in its task definition, then it reports the health status as @UNKNOWN@ .
cHealthStatus :: Lens' Container (Maybe HealthStatus)
cHealthStatus = lens _cHealthStatus (\ s a -> s{_cHealthStatus = a})

-- | The number of CPU units set for the container. The value will be @0@ if no value was specified in the container definition when the task definition was registered.
cCpu :: Lens' Container (Maybe Text)
cCpu = lens _cCpu (\ s a -> s{_cCpu = a})

-- | The soft limit (in MiB) of memory set for the container.
cMemoryReservation :: Lens' Container (Maybe Text)
cMemoryReservation = lens _cMemoryReservation (\ s a -> s{_cMemoryReservation = a})

instance FromJSON Container where
        parseJSON
          = withObject "Container"
              (\ x ->
                 Container' <$>
                   (x .:? "gpuIds" .!= mempty) <*>
                     (x .:? "networkBindings" .!= mempty)
                     <*> (x .:? "containerArn")
                     <*> (x .:? "networkInterfaces" .!= mempty)
                     <*> (x .:? "taskArn")
                     <*> (x .:? "lastStatus")
                     <*> (x .:? "memory")
                     <*> (x .:? "reason")
                     <*> (x .:? "name")
                     <*> (x .:? "exitCode")
                     <*> (x .:? "healthStatus")
                     <*> (x .:? "cpu")
                     <*> (x .:? "memoryReservation"))

instance Hashable Container where

instance NFData Container where

-- | Container definitions are used in task definitions to describe the different containers that are launched as part of a task.
--
--
--
-- /See:/ 'containerDefinition' smart constructor.
data ContainerDefinition = ContainerDefinition'
  { _cdImage                  :: !(Maybe Text)
  , _cdCommand                :: !(Maybe [Text])
  , _cdHostname               :: !(Maybe Text)
  , _cdRepositoryCredentials  :: !(Maybe RepositoryCredentials)
  , _cdDockerSecurityOptions  :: !(Maybe [Text])
  , _cdHealthCheck            :: !(Maybe HealthCheck)
  , _cdDisableNetworking      :: !(Maybe Bool)
  , _cdSecrets                :: !(Maybe [Secret])
  , _cdVolumesFrom            :: !(Maybe [VolumeFrom])
  , _cdEnvironment            :: !(Maybe [KeyValuePair])
  , _cdEntryPoint             :: !(Maybe [Text])
  , _cdWorkingDirectory       :: !(Maybe Text)
  , _cdUlimits                :: !(Maybe [Ulimit])
  , _cdStopTimeout            :: !(Maybe Int)
  , _cdPrivileged             :: !(Maybe Bool)
  , _cdPortMappings           :: !(Maybe [PortMapping])
  , _cdResourceRequirements   :: !(Maybe [ResourceRequirement])
  , _cdDockerLabels           :: !(Maybe (Map Text Text))
  , _cdExtraHosts             :: !(Maybe [HostEntry])
  , _cdMemory                 :: !(Maybe Int)
  , _cdSystemControls         :: !(Maybe [SystemControl])
  , _cdUser                   :: !(Maybe Text)
  , _cdDnsSearchDomains       :: !(Maybe [Text])
  , _cdLogConfiguration       :: !(Maybe LogConfiguration)
  , _cdLinuxParameters        :: !(Maybe LinuxParameters)
  , _cdPseudoTerminal         :: !(Maybe Bool)
  , _cdDependsOn              :: !(Maybe [ContainerDependency])
  , _cdName                   :: !(Maybe Text)
  , _cdDnsServers             :: !(Maybe [Text])
  , _cdMountPoints            :: !(Maybe [MountPoint])
  , _cdInteractive            :: !(Maybe Bool)
  , _cdStartTimeout           :: !(Maybe Int)
  , _cdLinks                  :: !(Maybe [Text])
  , _cdReadonlyRootFilesystem :: !(Maybe Bool)
  , _cdEssential              :: !(Maybe Bool)
  , _cdCpu                    :: !(Maybe Int)
  , _cdMemoryReservation      :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ContainerDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdImage' - The image used to start a container. This string is passed directly to the Docker daemon. Images in the Docker Hub registry are available by default. Other repositories are specified with either @/repository-url/ //image/ :/tag/ @ or @/repository-url/ //image/ @/digest/ @ . Up to 255 letters (uppercase and lowercase), numbers, hyphens, underscores, colons, periods, forward slashes, and number signs are allowed. This parameter maps to @Image@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @IMAGE@ parameter of <https://docs.docker.com/engine/reference/run/ docker run> .     * When a new task starts, the Amazon ECS container agent pulls the latest version of the specified image and tag for the container to use. However, subsequent updates to a repository image are not propagated to already running tasks.     * Images in Amazon ECR repositories can be specified by either using the full @registry/repository:tag@ or @registry/repository@digest@ . For example, @012345678910.dkr.ecr.<region-name>.amazonaws.com/<repository-name>:latest@ or @012345678910.dkr.ecr.<region-name>.amazonaws.com/<repository-name>@sha256:94afd1f2e64d908bc90dbca0035a5b567EXAMPLE@ .      * Images in official repositories on Docker Hub use a single name (for example, @ubuntu@ or @mongo@ ).     * Images in other repositories on Docker Hub are qualified with an organization name (for example, @amazon/amazon-ecs-agent@ ).     * Images in other online repositories are qualified further by a domain name (for example, @quay.io/assemblyline/ubuntu@ ).
--
-- * 'cdCommand' - The command that is passed to the container. This parameter maps to @Cmd@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @COMMAND@ parameter to <https://docs.docker.com/engine/reference/run/ docker run> . For more information, see <https://docs.docker.com/engine/reference/builder/#cmd https://docs.docker.com/engine/reference/builder/#cmd> . If there are multiple arguments, each argument should be a separated string in the array.
--
-- * 'cdHostname' - The hostname to use for your container. This parameter maps to @Hostname@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--hostname@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cdRepositoryCredentials' - The private repository authentication credentials to use.
--
-- * 'cdDockerSecurityOptions' - A list of strings to provide custom labels for SELinux and AppArmor multi-level security systems. This field is not valid for containers in tasks using the Fargate launch type. This parameter maps to @SecurityOpt@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--security-opt@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cdHealthCheck' - The health check command and associated configuration parameters for the container. This parameter maps to @HealthCheck@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @HEALTHCHECK@ parameter of <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cdDisableNetworking' - When this parameter is true, networking is disabled within the container. This parameter maps to @NetworkDisabled@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> .
--
-- * 'cdSecrets' - The secrets to pass to the container. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html Specifying Sensitive Data> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'cdVolumesFrom' - Data volumes to mount from another container. This parameter maps to @VolumesFrom@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--volumes-from@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cdEnvironment' - The environment variables to pass to a container. This parameter maps to @Env@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--env@ option to <https://docs.docker.com/engine/reference/run/ docker run> . /Important:/ We do not recommend using plaintext environment variables for sensitive information, such as credential data.
--
-- * 'cdEntryPoint' - /Important:/ Early versions of the Amazon ECS container agent do not properly handle @entryPoint@ parameters. If you have problems using @entryPoint@ , update your container agent or enter your commands and arguments as @command@ array items instead. The entry point that is passed to the container. This parameter maps to @Entrypoint@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--entrypoint@ option to <https://docs.docker.com/engine/reference/run/ docker run> . For more information, see <https://docs.docker.com/engine/reference/builder/#entrypoint https://docs.docker.com/engine/reference/builder/#entrypoint> .
--
-- * 'cdWorkingDirectory' - The working directory in which to run commands inside the container. This parameter maps to @WorkingDir@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--workdir@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cdUlimits' - A list of @ulimits@ to set in the container. This parameter maps to @Ulimits@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--ulimit@ option to <https://docs.docker.com/engine/reference/run/ docker run> . Valid naming values are displayed in the 'Ulimit' data type. This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@
--
-- * 'cdStopTimeout' - Time duration to wait before the container is forcefully killed if it does not exit normally on its own.
--
-- * 'cdPrivileged' - When this parameter is true, the container is given elevated privileges on the host container instance (similar to the @root@ user). This parameter maps to @Privileged@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--privileged@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cdPortMappings' - The list of port mappings for the container. Port mappings allow containers to access ports on the host container instance to send or receive traffic. For task definitions that use the @awsvpc@ network mode, you should only specify the @containerPort@ . The @hostPort@ can be left blank or it must be the same value as the @containerPort@ . Port mappings on Windows use the @NetNAT@ gateway address rather than @localhost@ . There is no loopback for port mappings on Windows, so you cannot access a container's mapped port from the host itself.  This parameter maps to @PortBindings@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--publish@ option to <https://docs.docker.com/engine/reference/run/ docker run> . If the network mode of a task definition is set to @none@ , then you can't specify port mappings. If the network mode of a task definition is set to @host@ , then host ports must either be undefined or they must match the container port in the port mapping.
--
-- * 'cdResourceRequirements' - The type and amount of a resource to assign to a container. The only supported resource is a GPU.
--
-- * 'cdDockerLabels' - A key/value map of labels to add to the container. This parameter maps to @Labels@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--label@ option to <https://docs.docker.com/engine/reference/run/ docker run> . This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@
--
-- * 'cdExtraHosts' - A list of hostnames and IP address mappings to append to the @/etc/hosts@ file on the container. This parameter maps to @ExtraHosts@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--add-host@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cdMemory' - The hard limit (in MiB) of memory to present to the container. If your container attempts to exceed the memory specified here, the container is killed. This parameter maps to @Memory@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--memory@ option to <https://docs.docker.com/engine/reference/run/ docker run> . If your containers are part of a task using the Fargate launch type, this field is optional and the only requirement is that the total amount of memory reserved for all containers within a task be lower than the task @memory@ value. For containers that are part of a task using the EC2 launch type, you must specify a non-zero integer for one or both of @memory@ or @memoryReservation@ in container definitions. If you specify both, @memory@ must be greater than @memoryReservation@ . If you specify @memoryReservation@ , then that value is subtracted from the available memory resources for the container instance on which the container is placed. Otherwise, the value of @memory@ is used. The Docker daemon reserves a minimum of 4 MiB of memory for a container, so you should not specify fewer than 4 MiB of memory for your containers.
--
-- * 'cdSystemControls' - A list of namespaced kernel parameters to set in the container. This parameter maps to @Sysctls@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--sysctl@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cdUser' - The username to use inside the container. This parameter maps to @User@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--user@ option to <https://docs.docker.com/engine/reference/run/ docker run> . This following formats can be used. If specifying a UID or GID, it must be specified as a positive integer.     * @user@      * @user:group@      * @uid@      * @uid:gid@      * @user:gid@      * @uid:group@
--
-- * 'cdDnsSearchDomains' - A list of DNS search domains that are presented to the container. This parameter maps to @DnsSearch@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--dns-search@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cdLogConfiguration' - The log configuration specification for the container. If you are using the Fargate launch type, the only supported value is @awslogs@ . This parameter maps to @LogConfig@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--log-driver@ option to <https://docs.docker.com/engine/reference/run/ docker run> . By default, containers use the same logging driver that the Docker daemon uses. However the container may use a different logging driver than the Docker daemon by specifying a log driver with this parameter in the container definition. To use a different logging driver for a container, the log system must be configured properly on the container instance (or on a different log server for remote logging options). For more information on the options for different supported log drivers, see <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers> in the Docker documentation. This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@
--
-- * 'cdLinuxParameters' - Linux-specific modifications that are applied to the container, such as Linux 'KernelCapabilities' .
--
-- * 'cdPseudoTerminal' - When this parameter is @true@ , a TTY is allocated. This parameter maps to @Tty@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--tty@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cdDependsOn' - The dependencies defined for container startup. A container can contain multiple dependencies.
--
-- * 'cdName' - The name of a container. If you are linking multiple containers together in a task definition, the @name@ of one container can be entered in the @links@ of another container to connect the containers. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. This parameter maps to @name@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--name@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cdDnsServers' - A list of DNS servers that are presented to the container. This parameter maps to @Dns@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--dns@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cdMountPoints' - The mount points for data volumes in your container. This parameter maps to @Volumes@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--volume@ option to <https://docs.docker.com/engine/reference/run/ docker run> . Windows containers can mount whole directories on the same drive as @> env:ProgramData@ . Windows containers cannot mount directories on a different drive, and mount point cannot be across drives.
--
-- * 'cdInteractive' - When this parameter is @true@ , this allows you to deploy containerized applications that require @stdin@ or a @tty@ to be allocated. This parameter maps to @OpenStdin@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--interactive@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cdStartTimeout' - Time duration to wait before giving up on starting the container.
--
-- * 'cdLinks' - The @link@ parameter allows containers to communicate with each other without the need for port mappings. Only supported if the network mode of a task definition is set to @bridge@ . The @name:internalName@ construct is analogous to @name:alias@ in Docker links. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. For more information about linking Docker containers, go to <https://docs.docker.com/engine/userguide/networking/default_network/dockerlinks/ https://docs.docker.com/engine/userguide/networking/default_network/dockerlinks/> . This parameter maps to @Links@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--link@ option to <https://docs.docker.com/engine/reference/commandline/run/ @docker run@ > . /Important:/ Containers that are collocated on a single container instance may be able to communicate with each other without requiring links or host port mappings. Network isolation is achieved on the container instance using security groups and VPC settings.
--
-- * 'cdReadonlyRootFilesystem' - When this parameter is true, the container is given read-only access to its root file system. This parameter maps to @ReadonlyRootfs@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--read-only@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cdEssential' - If the @essential@ parameter of a container is marked as @true@ , and that container fails or stops for any reason, all other containers that are part of the task are stopped. If the @essential@ parameter of a container is marked as @false@ , then its failure does not affect the rest of the containers in a task. If this parameter is omitted, a container is assumed to be essential. All tasks must have at least one essential container. If you have an application that is composed of multiple containers, you should group containers that are used for a common purpose into components, and separate the different components into multiple task definitions. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/application_architecture.html Application Architecture> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'cdCpu' - The number of @cpu@ units reserved for the container. This parameter maps to @CpuShares@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--cpu-shares@ option to <https://docs.docker.com/engine/reference/run/ docker run> . This field is optional for tasks using the Fargate launch type, and the only requirement is that the total amount of CPU reserved for all containers within a task be lower than the task-level @cpu@ value. For example, if you run a single-container task on a single-core instance type with 512 CPU units specified for that container, and that is the only task running on the container instance, that container could use the full 1,024 CPU unit share at any given time. However, if you launched another copy of the same task on that container instance, each task would be guaranteed a minimum of 512 CPU units when needed, and each container could float to higher CPU usage if the other container was not using it, but if both tasks were 100% active all of the time, they would be limited to 512 CPU units. Linux containers share unallocated CPU units with other containers on the container instance with the same ratio as their allocated amount. For example, if you run a single-container task on a single-core instance type with 512 CPU units specified for that container, and that is the only task running on the container instance, that container could use the full 1,024 CPU unit share at any given time. However, if you launched another copy of the same task on that container instance, each task would be guaranteed a minimum of 512 CPU units when needed, and each container could float to higher CPU usage if the other container was not using it, but if both tasks were 100% active all of the time, they would be limited to 512 CPU units. On Linux container instances, the Docker daemon on the container instance uses the CPU value to calculate the relative CPU share ratios for running containers. For more information, see <https://docs.docker.com/engine/reference/run/#cpu-share-constraint CPU share constraint> in the Docker documentation. The minimum valid CPU share value that the Linux kernel allows is 2. However, the CPU parameter is not required, and you can use CPU values below 2 in your container definitions. For CPU values below 2 (including null), the behavior varies based on your Amazon ECS container agent version:     * __Agent versions less than or equal to 1.1.0:__ Null and zero CPU values are passed to Docker as 0, which Docker then converts to 1,024 CPU shares. CPU values of 1 are passed to Docker as 1, which the Linux kernel converts to two CPU shares.     * __Agent versions greater than or equal to 1.2.0:__ Null, zero, and CPU values of 1 are passed to Docker as 2. On Windows container instances, the CPU limit is enforced as an absolute limit, or a quota. Windows containers only have access to the specified amount of CPU that is described in the task definition.
--
-- * 'cdMemoryReservation' - The soft limit (in MiB) of memory to reserve for the container. When system memory is under heavy contention, Docker attempts to keep the container memory to this soft limit. However, your container can consume more memory when it needs to, up to either the hard limit specified with the @memory@ parameter (if applicable), or all of the available memory on the container instance, whichever comes first. This parameter maps to @MemoryReservation@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--memory-reservation@ option to <https://docs.docker.com/engine/reference/run/ docker run> . You must specify a non-zero integer for one or both of @memory@ or @memoryReservation@ in container definitions. If you specify both, @memory@ must be greater than @memoryReservation@ . If you specify @memoryReservation@ , then that value is subtracted from the available memory resources for the container instance on which the container is placed. Otherwise, the value of @memory@ is used. For example, if your container normally uses 128 MiB of memory, but occasionally bursts to 256 MiB of memory for short periods of time, you can set a @memoryReservation@ of 128 MiB, and a @memory@ hard limit of 300 MiB. This configuration would allow the container to only reserve 128 MiB of memory from the remaining resources on the container instance, but also allow the container to consume more memory resources when needed. The Docker daemon reserves a minimum of 4 MiB of memory for a container, so you should not specify fewer than 4 MiB of memory for your containers.
containerDefinition
    :: ContainerDefinition
containerDefinition =
  ContainerDefinition'
    { _cdImage = Nothing
    , _cdCommand = Nothing
    , _cdHostname = Nothing
    , _cdRepositoryCredentials = Nothing
    , _cdDockerSecurityOptions = Nothing
    , _cdHealthCheck = Nothing
    , _cdDisableNetworking = Nothing
    , _cdSecrets = Nothing
    , _cdVolumesFrom = Nothing
    , _cdEnvironment = Nothing
    , _cdEntryPoint = Nothing
    , _cdWorkingDirectory = Nothing
    , _cdUlimits = Nothing
    , _cdStopTimeout = Nothing
    , _cdPrivileged = Nothing
    , _cdPortMappings = Nothing
    , _cdResourceRequirements = Nothing
    , _cdDockerLabels = Nothing
    , _cdExtraHosts = Nothing
    , _cdMemory = Nothing
    , _cdSystemControls = Nothing
    , _cdUser = Nothing
    , _cdDnsSearchDomains = Nothing
    , _cdLogConfiguration = Nothing
    , _cdLinuxParameters = Nothing
    , _cdPseudoTerminal = Nothing
    , _cdDependsOn = Nothing
    , _cdName = Nothing
    , _cdDnsServers = Nothing
    , _cdMountPoints = Nothing
    , _cdInteractive = Nothing
    , _cdStartTimeout = Nothing
    , _cdLinks = Nothing
    , _cdReadonlyRootFilesystem = Nothing
    , _cdEssential = Nothing
    , _cdCpu = Nothing
    , _cdMemoryReservation = Nothing
    }


-- | The image used to start a container. This string is passed directly to the Docker daemon. Images in the Docker Hub registry are available by default. Other repositories are specified with either @/repository-url/ //image/ :/tag/ @ or @/repository-url/ //image/ @/digest/ @ . Up to 255 letters (uppercase and lowercase), numbers, hyphens, underscores, colons, periods, forward slashes, and number signs are allowed. This parameter maps to @Image@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @IMAGE@ parameter of <https://docs.docker.com/engine/reference/run/ docker run> .     * When a new task starts, the Amazon ECS container agent pulls the latest version of the specified image and tag for the container to use. However, subsequent updates to a repository image are not propagated to already running tasks.     * Images in Amazon ECR repositories can be specified by either using the full @registry/repository:tag@ or @registry/repository@digest@ . For example, @012345678910.dkr.ecr.<region-name>.amazonaws.com/<repository-name>:latest@ or @012345678910.dkr.ecr.<region-name>.amazonaws.com/<repository-name>@sha256:94afd1f2e64d908bc90dbca0035a5b567EXAMPLE@ .      * Images in official repositories on Docker Hub use a single name (for example, @ubuntu@ or @mongo@ ).     * Images in other repositories on Docker Hub are qualified with an organization name (for example, @amazon/amazon-ecs-agent@ ).     * Images in other online repositories are qualified further by a domain name (for example, @quay.io/assemblyline/ubuntu@ ).
cdImage :: Lens' ContainerDefinition (Maybe Text)
cdImage = lens _cdImage (\ s a -> s{_cdImage = a})

-- | The command that is passed to the container. This parameter maps to @Cmd@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @COMMAND@ parameter to <https://docs.docker.com/engine/reference/run/ docker run> . For more information, see <https://docs.docker.com/engine/reference/builder/#cmd https://docs.docker.com/engine/reference/builder/#cmd> . If there are multiple arguments, each argument should be a separated string in the array.
cdCommand :: Lens' ContainerDefinition [Text]
cdCommand = lens _cdCommand (\ s a -> s{_cdCommand = a}) . _Default . _Coerce

-- | The hostname to use for your container. This parameter maps to @Hostname@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--hostname@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cdHostname :: Lens' ContainerDefinition (Maybe Text)
cdHostname = lens _cdHostname (\ s a -> s{_cdHostname = a})

-- | The private repository authentication credentials to use.
cdRepositoryCredentials :: Lens' ContainerDefinition (Maybe RepositoryCredentials)
cdRepositoryCredentials = lens _cdRepositoryCredentials (\ s a -> s{_cdRepositoryCredentials = a})

-- | A list of strings to provide custom labels for SELinux and AppArmor multi-level security systems. This field is not valid for containers in tasks using the Fargate launch type. This parameter maps to @SecurityOpt@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--security-opt@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cdDockerSecurityOptions :: Lens' ContainerDefinition [Text]
cdDockerSecurityOptions = lens _cdDockerSecurityOptions (\ s a -> s{_cdDockerSecurityOptions = a}) . _Default . _Coerce

-- | The health check command and associated configuration parameters for the container. This parameter maps to @HealthCheck@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @HEALTHCHECK@ parameter of <https://docs.docker.com/engine/reference/run/ docker run> .
cdHealthCheck :: Lens' ContainerDefinition (Maybe HealthCheck)
cdHealthCheck = lens _cdHealthCheck (\ s a -> s{_cdHealthCheck = a})

-- | When this parameter is true, networking is disabled within the container. This parameter maps to @NetworkDisabled@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> .
cdDisableNetworking :: Lens' ContainerDefinition (Maybe Bool)
cdDisableNetworking = lens _cdDisableNetworking (\ s a -> s{_cdDisableNetworking = a})

-- | The secrets to pass to the container. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html Specifying Sensitive Data> in the /Amazon Elastic Container Service Developer Guide/ .
cdSecrets :: Lens' ContainerDefinition [Secret]
cdSecrets = lens _cdSecrets (\ s a -> s{_cdSecrets = a}) . _Default . _Coerce

-- | Data volumes to mount from another container. This parameter maps to @VolumesFrom@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--volumes-from@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cdVolumesFrom :: Lens' ContainerDefinition [VolumeFrom]
cdVolumesFrom = lens _cdVolumesFrom (\ s a -> s{_cdVolumesFrom = a}) . _Default . _Coerce

-- | The environment variables to pass to a container. This parameter maps to @Env@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--env@ option to <https://docs.docker.com/engine/reference/run/ docker run> . /Important:/ We do not recommend using plaintext environment variables for sensitive information, such as credential data.
cdEnvironment :: Lens' ContainerDefinition [KeyValuePair]
cdEnvironment = lens _cdEnvironment (\ s a -> s{_cdEnvironment = a}) . _Default . _Coerce

-- | /Important:/ Early versions of the Amazon ECS container agent do not properly handle @entryPoint@ parameters. If you have problems using @entryPoint@ , update your container agent or enter your commands and arguments as @command@ array items instead. The entry point that is passed to the container. This parameter maps to @Entrypoint@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--entrypoint@ option to <https://docs.docker.com/engine/reference/run/ docker run> . For more information, see <https://docs.docker.com/engine/reference/builder/#entrypoint https://docs.docker.com/engine/reference/builder/#entrypoint> .
cdEntryPoint :: Lens' ContainerDefinition [Text]
cdEntryPoint = lens _cdEntryPoint (\ s a -> s{_cdEntryPoint = a}) . _Default . _Coerce

-- | The working directory in which to run commands inside the container. This parameter maps to @WorkingDir@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--workdir@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cdWorkingDirectory :: Lens' ContainerDefinition (Maybe Text)
cdWorkingDirectory = lens _cdWorkingDirectory (\ s a -> s{_cdWorkingDirectory = a})

-- | A list of @ulimits@ to set in the container. This parameter maps to @Ulimits@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--ulimit@ option to <https://docs.docker.com/engine/reference/run/ docker run> . Valid naming values are displayed in the 'Ulimit' data type. This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@
cdUlimits :: Lens' ContainerDefinition [Ulimit]
cdUlimits = lens _cdUlimits (\ s a -> s{_cdUlimits = a}) . _Default . _Coerce

-- | Time duration to wait before the container is forcefully killed if it does not exit normally on its own.
cdStopTimeout :: Lens' ContainerDefinition (Maybe Int)
cdStopTimeout = lens _cdStopTimeout (\ s a -> s{_cdStopTimeout = a})

-- | When this parameter is true, the container is given elevated privileges on the host container instance (similar to the @root@ user). This parameter maps to @Privileged@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--privileged@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cdPrivileged :: Lens' ContainerDefinition (Maybe Bool)
cdPrivileged = lens _cdPrivileged (\ s a -> s{_cdPrivileged = a})

-- | The list of port mappings for the container. Port mappings allow containers to access ports on the host container instance to send or receive traffic. For task definitions that use the @awsvpc@ network mode, you should only specify the @containerPort@ . The @hostPort@ can be left blank or it must be the same value as the @containerPort@ . Port mappings on Windows use the @NetNAT@ gateway address rather than @localhost@ . There is no loopback for port mappings on Windows, so you cannot access a container's mapped port from the host itself.  This parameter maps to @PortBindings@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--publish@ option to <https://docs.docker.com/engine/reference/run/ docker run> . If the network mode of a task definition is set to @none@ , then you can't specify port mappings. If the network mode of a task definition is set to @host@ , then host ports must either be undefined or they must match the container port in the port mapping.
cdPortMappings :: Lens' ContainerDefinition [PortMapping]
cdPortMappings = lens _cdPortMappings (\ s a -> s{_cdPortMappings = a}) . _Default . _Coerce

-- | The type and amount of a resource to assign to a container. The only supported resource is a GPU.
cdResourceRequirements :: Lens' ContainerDefinition [ResourceRequirement]
cdResourceRequirements = lens _cdResourceRequirements (\ s a -> s{_cdResourceRequirements = a}) . _Default . _Coerce

-- | A key/value map of labels to add to the container. This parameter maps to @Labels@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--label@ option to <https://docs.docker.com/engine/reference/run/ docker run> . This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@
cdDockerLabels :: Lens' ContainerDefinition (HashMap Text Text)
cdDockerLabels = lens _cdDockerLabels (\ s a -> s{_cdDockerLabels = a}) . _Default . _Map

-- | A list of hostnames and IP address mappings to append to the @/etc/hosts@ file on the container. This parameter maps to @ExtraHosts@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--add-host@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cdExtraHosts :: Lens' ContainerDefinition [HostEntry]
cdExtraHosts = lens _cdExtraHosts (\ s a -> s{_cdExtraHosts = a}) . _Default . _Coerce

-- | The hard limit (in MiB) of memory to present to the container. If your container attempts to exceed the memory specified here, the container is killed. This parameter maps to @Memory@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--memory@ option to <https://docs.docker.com/engine/reference/run/ docker run> . If your containers are part of a task using the Fargate launch type, this field is optional and the only requirement is that the total amount of memory reserved for all containers within a task be lower than the task @memory@ value. For containers that are part of a task using the EC2 launch type, you must specify a non-zero integer for one or both of @memory@ or @memoryReservation@ in container definitions. If you specify both, @memory@ must be greater than @memoryReservation@ . If you specify @memoryReservation@ , then that value is subtracted from the available memory resources for the container instance on which the container is placed. Otherwise, the value of @memory@ is used. The Docker daemon reserves a minimum of 4 MiB of memory for a container, so you should not specify fewer than 4 MiB of memory for your containers.
cdMemory :: Lens' ContainerDefinition (Maybe Int)
cdMemory = lens _cdMemory (\ s a -> s{_cdMemory = a})

-- | A list of namespaced kernel parameters to set in the container. This parameter maps to @Sysctls@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--sysctl@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cdSystemControls :: Lens' ContainerDefinition [SystemControl]
cdSystemControls = lens _cdSystemControls (\ s a -> s{_cdSystemControls = a}) . _Default . _Coerce

-- | The username to use inside the container. This parameter maps to @User@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--user@ option to <https://docs.docker.com/engine/reference/run/ docker run> . This following formats can be used. If specifying a UID or GID, it must be specified as a positive integer.     * @user@      * @user:group@      * @uid@      * @uid:gid@      * @user:gid@      * @uid:group@
cdUser :: Lens' ContainerDefinition (Maybe Text)
cdUser = lens _cdUser (\ s a -> s{_cdUser = a})

-- | A list of DNS search domains that are presented to the container. This parameter maps to @DnsSearch@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--dns-search@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cdDnsSearchDomains :: Lens' ContainerDefinition [Text]
cdDnsSearchDomains = lens _cdDnsSearchDomains (\ s a -> s{_cdDnsSearchDomains = a}) . _Default . _Coerce

-- | The log configuration specification for the container. If you are using the Fargate launch type, the only supported value is @awslogs@ . This parameter maps to @LogConfig@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--log-driver@ option to <https://docs.docker.com/engine/reference/run/ docker run> . By default, containers use the same logging driver that the Docker daemon uses. However the container may use a different logging driver than the Docker daemon by specifying a log driver with this parameter in the container definition. To use a different logging driver for a container, the log system must be configured properly on the container instance (or on a different log server for remote logging options). For more information on the options for different supported log drivers, see <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers> in the Docker documentation. This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@
cdLogConfiguration :: Lens' ContainerDefinition (Maybe LogConfiguration)
cdLogConfiguration = lens _cdLogConfiguration (\ s a -> s{_cdLogConfiguration = a})

-- | Linux-specific modifications that are applied to the container, such as Linux 'KernelCapabilities' .
cdLinuxParameters :: Lens' ContainerDefinition (Maybe LinuxParameters)
cdLinuxParameters = lens _cdLinuxParameters (\ s a -> s{_cdLinuxParameters = a})

-- | When this parameter is @true@ , a TTY is allocated. This parameter maps to @Tty@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--tty@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cdPseudoTerminal :: Lens' ContainerDefinition (Maybe Bool)
cdPseudoTerminal = lens _cdPseudoTerminal (\ s a -> s{_cdPseudoTerminal = a})

-- | The dependencies defined for container startup. A container can contain multiple dependencies.
cdDependsOn :: Lens' ContainerDefinition [ContainerDependency]
cdDependsOn = lens _cdDependsOn (\ s a -> s{_cdDependsOn = a}) . _Default . _Coerce

-- | The name of a container. If you are linking multiple containers together in a task definition, the @name@ of one container can be entered in the @links@ of another container to connect the containers. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. This parameter maps to @name@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--name@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cdName :: Lens' ContainerDefinition (Maybe Text)
cdName = lens _cdName (\ s a -> s{_cdName = a})

-- | A list of DNS servers that are presented to the container. This parameter maps to @Dns@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--dns@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cdDnsServers :: Lens' ContainerDefinition [Text]
cdDnsServers = lens _cdDnsServers (\ s a -> s{_cdDnsServers = a}) . _Default . _Coerce

-- | The mount points for data volumes in your container. This parameter maps to @Volumes@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--volume@ option to <https://docs.docker.com/engine/reference/run/ docker run> . Windows containers can mount whole directories on the same drive as @> env:ProgramData@ . Windows containers cannot mount directories on a different drive, and mount point cannot be across drives.
cdMountPoints :: Lens' ContainerDefinition [MountPoint]
cdMountPoints = lens _cdMountPoints (\ s a -> s{_cdMountPoints = a}) . _Default . _Coerce

-- | When this parameter is @true@ , this allows you to deploy containerized applications that require @stdin@ or a @tty@ to be allocated. This parameter maps to @OpenStdin@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--interactive@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cdInteractive :: Lens' ContainerDefinition (Maybe Bool)
cdInteractive = lens _cdInteractive (\ s a -> s{_cdInteractive = a})

-- | Time duration to wait before giving up on starting the container.
cdStartTimeout :: Lens' ContainerDefinition (Maybe Int)
cdStartTimeout = lens _cdStartTimeout (\ s a -> s{_cdStartTimeout = a})

-- | The @link@ parameter allows containers to communicate with each other without the need for port mappings. Only supported if the network mode of a task definition is set to @bridge@ . The @name:internalName@ construct is analogous to @name:alias@ in Docker links. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. For more information about linking Docker containers, go to <https://docs.docker.com/engine/userguide/networking/default_network/dockerlinks/ https://docs.docker.com/engine/userguide/networking/default_network/dockerlinks/> . This parameter maps to @Links@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--link@ option to <https://docs.docker.com/engine/reference/commandline/run/ @docker run@ > . /Important:/ Containers that are collocated on a single container instance may be able to communicate with each other without requiring links or host port mappings. Network isolation is achieved on the container instance using security groups and VPC settings.
cdLinks :: Lens' ContainerDefinition [Text]
cdLinks = lens _cdLinks (\ s a -> s{_cdLinks = a}) . _Default . _Coerce

-- | When this parameter is true, the container is given read-only access to its root file system. This parameter maps to @ReadonlyRootfs@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--read-only@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cdReadonlyRootFilesystem :: Lens' ContainerDefinition (Maybe Bool)
cdReadonlyRootFilesystem = lens _cdReadonlyRootFilesystem (\ s a -> s{_cdReadonlyRootFilesystem = a})

-- | If the @essential@ parameter of a container is marked as @true@ , and that container fails or stops for any reason, all other containers that are part of the task are stopped. If the @essential@ parameter of a container is marked as @false@ , then its failure does not affect the rest of the containers in a task. If this parameter is omitted, a container is assumed to be essential. All tasks must have at least one essential container. If you have an application that is composed of multiple containers, you should group containers that are used for a common purpose into components, and separate the different components into multiple task definitions. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/application_architecture.html Application Architecture> in the /Amazon Elastic Container Service Developer Guide/ .
cdEssential :: Lens' ContainerDefinition (Maybe Bool)
cdEssential = lens _cdEssential (\ s a -> s{_cdEssential = a})

-- | The number of @cpu@ units reserved for the container. This parameter maps to @CpuShares@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--cpu-shares@ option to <https://docs.docker.com/engine/reference/run/ docker run> . This field is optional for tasks using the Fargate launch type, and the only requirement is that the total amount of CPU reserved for all containers within a task be lower than the task-level @cpu@ value. For example, if you run a single-container task on a single-core instance type with 512 CPU units specified for that container, and that is the only task running on the container instance, that container could use the full 1,024 CPU unit share at any given time. However, if you launched another copy of the same task on that container instance, each task would be guaranteed a minimum of 512 CPU units when needed, and each container could float to higher CPU usage if the other container was not using it, but if both tasks were 100% active all of the time, they would be limited to 512 CPU units. Linux containers share unallocated CPU units with other containers on the container instance with the same ratio as their allocated amount. For example, if you run a single-container task on a single-core instance type with 512 CPU units specified for that container, and that is the only task running on the container instance, that container could use the full 1,024 CPU unit share at any given time. However, if you launched another copy of the same task on that container instance, each task would be guaranteed a minimum of 512 CPU units when needed, and each container could float to higher CPU usage if the other container was not using it, but if both tasks were 100% active all of the time, they would be limited to 512 CPU units. On Linux container instances, the Docker daemon on the container instance uses the CPU value to calculate the relative CPU share ratios for running containers. For more information, see <https://docs.docker.com/engine/reference/run/#cpu-share-constraint CPU share constraint> in the Docker documentation. The minimum valid CPU share value that the Linux kernel allows is 2. However, the CPU parameter is not required, and you can use CPU values below 2 in your container definitions. For CPU values below 2 (including null), the behavior varies based on your Amazon ECS container agent version:     * __Agent versions less than or equal to 1.1.0:__ Null and zero CPU values are passed to Docker as 0, which Docker then converts to 1,024 CPU shares. CPU values of 1 are passed to Docker as 1, which the Linux kernel converts to two CPU shares.     * __Agent versions greater than or equal to 1.2.0:__ Null, zero, and CPU values of 1 are passed to Docker as 2. On Windows container instances, the CPU limit is enforced as an absolute limit, or a quota. Windows containers only have access to the specified amount of CPU that is described in the task definition.
cdCpu :: Lens' ContainerDefinition (Maybe Int)
cdCpu = lens _cdCpu (\ s a -> s{_cdCpu = a})

-- | The soft limit (in MiB) of memory to reserve for the container. When system memory is under heavy contention, Docker attempts to keep the container memory to this soft limit. However, your container can consume more memory when it needs to, up to either the hard limit specified with the @memory@ parameter (if applicable), or all of the available memory on the container instance, whichever comes first. This parameter maps to @MemoryReservation@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--memory-reservation@ option to <https://docs.docker.com/engine/reference/run/ docker run> . You must specify a non-zero integer for one or both of @memory@ or @memoryReservation@ in container definitions. If you specify both, @memory@ must be greater than @memoryReservation@ . If you specify @memoryReservation@ , then that value is subtracted from the available memory resources for the container instance on which the container is placed. Otherwise, the value of @memory@ is used. For example, if your container normally uses 128 MiB of memory, but occasionally bursts to 256 MiB of memory for short periods of time, you can set a @memoryReservation@ of 128 MiB, and a @memory@ hard limit of 300 MiB. This configuration would allow the container to only reserve 128 MiB of memory from the remaining resources on the container instance, but also allow the container to consume more memory resources when needed. The Docker daemon reserves a minimum of 4 MiB of memory for a container, so you should not specify fewer than 4 MiB of memory for your containers.
cdMemoryReservation :: Lens' ContainerDefinition (Maybe Int)
cdMemoryReservation = lens _cdMemoryReservation (\ s a -> s{_cdMemoryReservation = a})

instance FromJSON ContainerDefinition where
        parseJSON
          = withObject "ContainerDefinition"
              (\ x ->
                 ContainerDefinition' <$>
                   (x .:? "image") <*> (x .:? "command" .!= mempty) <*>
                     (x .:? "hostname")
                     <*> (x .:? "repositoryCredentials")
                     <*> (x .:? "dockerSecurityOptions" .!= mempty)
                     <*> (x .:? "healthCheck")
                     <*> (x .:? "disableNetworking")
                     <*> (x .:? "secrets" .!= mempty)
                     <*> (x .:? "volumesFrom" .!= mempty)
                     <*> (x .:? "environment" .!= mempty)
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
                     <*> (x .:? "memoryReservation"))

instance Hashable ContainerDefinition where

instance NFData ContainerDefinition where

instance ToJSON ContainerDefinition where
        toJSON ContainerDefinition'{..}
          = object
              (catMaybes
                 [("image" .=) <$> _cdImage,
                  ("command" .=) <$> _cdCommand,
                  ("hostname" .=) <$> _cdHostname,
                  ("repositoryCredentials" .=) <$>
                    _cdRepositoryCredentials,
                  ("dockerSecurityOptions" .=) <$>
                    _cdDockerSecurityOptions,
                  ("healthCheck" .=) <$> _cdHealthCheck,
                  ("disableNetworking" .=) <$> _cdDisableNetworking,
                  ("secrets" .=) <$> _cdSecrets,
                  ("volumesFrom" .=) <$> _cdVolumesFrom,
                  ("environment" .=) <$> _cdEnvironment,
                  ("entryPoint" .=) <$> _cdEntryPoint,
                  ("workingDirectory" .=) <$> _cdWorkingDirectory,
                  ("ulimits" .=) <$> _cdUlimits,
                  ("stopTimeout" .=) <$> _cdStopTimeout,
                  ("privileged" .=) <$> _cdPrivileged,
                  ("portMappings" .=) <$> _cdPortMappings,
                  ("resourceRequirements" .=) <$>
                    _cdResourceRequirements,
                  ("dockerLabels" .=) <$> _cdDockerLabels,
                  ("extraHosts" .=) <$> _cdExtraHosts,
                  ("memory" .=) <$> _cdMemory,
                  ("systemControls" .=) <$> _cdSystemControls,
                  ("user" .=) <$> _cdUser,
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
                  ("readonlyRootFilesystem" .=) <$>
                    _cdReadonlyRootFilesystem,
                  ("essential" .=) <$> _cdEssential,
                  ("cpu" .=) <$> _cdCpu,
                  ("memoryReservation" .=) <$> _cdMemoryReservation])

-- | The dependencies defined for container startup. A container can contain multiple dependencies.
--
--
--
-- /See:/ 'containerDependency' smart constructor.
data ContainerDependency = ContainerDependency'
  { _cdContainerName :: !Text
  , _cdCondition     :: !ContainerCondition
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ContainerDependency' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdContainerName' - The name of a container.
--
-- * 'cdCondition' - The dependency condition of the container. The following are the available conditions and their behavior:     * @START@ - This condition emulates the behavior of links and volumes today. It validates that a dependent container is started before permitting other containers to start.     * @COMPLETE@ - This condition validates that a dependent container runs to completion (exits) before permitting other containers to start. This can be useful for non-essential containers that run a script and then subsequently exit.     * @SUCCESS@ - This condition is the same as @COMPLETE@ , but it will also require that the container exits with a @zero@ status.     * @HEALTHY@ - This condition validates that the dependent container passes its Docker health check before permitting other containers to start. This requires that the dependent container has health checks configured. This condition will only be confirmed at task startup.
containerDependency
    :: Text -- ^ 'cdContainerName'
    -> ContainerCondition -- ^ 'cdCondition'
    -> ContainerDependency
containerDependency pContainerName_ pCondition_ =
  ContainerDependency'
    {_cdContainerName = pContainerName_, _cdCondition = pCondition_}


-- | The name of a container.
cdContainerName :: Lens' ContainerDependency Text
cdContainerName = lens _cdContainerName (\ s a -> s{_cdContainerName = a})

-- | The dependency condition of the container. The following are the available conditions and their behavior:     * @START@ - This condition emulates the behavior of links and volumes today. It validates that a dependent container is started before permitting other containers to start.     * @COMPLETE@ - This condition validates that a dependent container runs to completion (exits) before permitting other containers to start. This can be useful for non-essential containers that run a script and then subsequently exit.     * @SUCCESS@ - This condition is the same as @COMPLETE@ , but it will also require that the container exits with a @zero@ status.     * @HEALTHY@ - This condition validates that the dependent container passes its Docker health check before permitting other containers to start. This requires that the dependent container has health checks configured. This condition will only be confirmed at task startup.
cdCondition :: Lens' ContainerDependency ContainerCondition
cdCondition = lens _cdCondition (\ s a -> s{_cdCondition = a})

instance FromJSON ContainerDependency where
        parseJSON
          = withObject "ContainerDependency"
              (\ x ->
                 ContainerDependency' <$>
                   (x .: "containerName") <*> (x .: "condition"))

instance Hashable ContainerDependency where

instance NFData ContainerDependency where

instance ToJSON ContainerDependency where
        toJSON ContainerDependency'{..}
          = object
              (catMaybes
                 [Just ("containerName" .= _cdContainerName),
                  Just ("condition" .= _cdCondition)])

-- | An EC2 instance that is running the Amazon ECS agent and has been registered with a cluster.
--
--
--
-- /See:/ 'containerInstance' smart constructor.
data ContainerInstance = ContainerInstance'
  { _ciStatus               :: !(Maybe Text)
  , _ciAttachments          :: !(Maybe [Attachment])
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
  , _ciTags                 :: !(Maybe [Tag])
  , _ciRegisteredResources  :: !(Maybe [Resource])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ContainerInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciStatus' - The status of the container instance. The valid values are @ACTIVE@ , @INACTIVE@ , or @DRAINING@ . @ACTIVE@ indicates that the container instance can accept tasks. @DRAINING@ indicates that new tasks are not placed on the container instance and any service tasks running on the container instance are removed if possible. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/container-instance-draining.html Container Instance Draining> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'ciAttachments' - The elastic network interfaces associated with the container instance.
--
-- * 'ciRunningTasksCount' - The number of tasks on the container instance that are in the @RUNNING@ status.
--
-- * 'ciRemainingResources' - For CPU and memory resource types, this parameter describes the remaining CPU and memory that has not already been allocated to tasks and is therefore available for new tasks. For port resource types, this parameter describes the ports that were reserved by the Amazon ECS container agent (at instance registration time) and any task containers that have reserved port mappings on the host (with the @host@ or @bridge@ network mode). Any port that is not specified here is available for new tasks.
--
-- * 'ciEc2InstanceId' - The EC2 instance ID of the container instance.
--
-- * 'ciContainerInstanceARN' - The Amazon Resource Name (ARN) of the container instance. The ARN contains the @arn:aws:ecs@ namespace, followed by the Region of the container instance, the AWS account ID of the container instance owner, the @container-instance@ namespace, and then the container instance ID. For example, @arn:aws:ecs:/region/ :/aws_account_id/ :container-instance//container_instance_ID/ @ .
--
-- * 'ciAgentConnected' - This parameter returns @true@ if the agent is connected to Amazon ECS. Registered instances with an agent that may be unhealthy or stopped return @false@ . Only instances connected to an agent can accept placement requests.
--
-- * 'ciVersionInfo' - The version information for the Amazon ECS container agent and Docker daemon running on the container instance.
--
-- * 'ciAgentUpdateStatus' - The status of the most recent agent update. If an update has never been requested, this value is @NULL@ .
--
-- * 'ciAttributes' - The attributes set for the container instance, either by the Amazon ECS container agent at instance registration or manually with the 'PutAttributes' operation.
--
-- * 'ciVersion' - The version counter for the container instance. Every time a container instance experiences a change that triggers a CloudWatch event, the version counter is incremented. If you are replicating your Amazon ECS container instance state with CloudWatch Events, you can compare the version of a container instance reported by the Amazon ECS APIs with the version reported in CloudWatch Events for the container instance (inside the @detail@ object) to verify that the version in your event stream is current.
--
-- * 'ciPendingTasksCount' - The number of tasks on the container instance that are in the @PENDING@ status.
--
-- * 'ciRegisteredAt' - The Unix timestamp for when the container instance was registered.
--
-- * 'ciTags' - The metadata that you apply to the container instance to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
--
-- * 'ciRegisteredResources' - For CPU and memory resource types, this parameter describes the amount of each resource that was available on the container instance when the container agent registered it with Amazon ECS. This value represents the total amount of CPU and memory that can be allocated on this container instance to tasks. For port resource types, this parameter describes the ports that were reserved by the Amazon ECS container agent when it registered the container instance with Amazon ECS.
containerInstance
    :: ContainerInstance
containerInstance =
  ContainerInstance'
    { _ciStatus = Nothing
    , _ciAttachments = Nothing
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
    , _ciTags = Nothing
    , _ciRegisteredResources = Nothing
    }


-- | The status of the container instance. The valid values are @ACTIVE@ , @INACTIVE@ , or @DRAINING@ . @ACTIVE@ indicates that the container instance can accept tasks. @DRAINING@ indicates that new tasks are not placed on the container instance and any service tasks running on the container instance are removed if possible. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/container-instance-draining.html Container Instance Draining> in the /Amazon Elastic Container Service Developer Guide/ .
ciStatus :: Lens' ContainerInstance (Maybe Text)
ciStatus = lens _ciStatus (\ s a -> s{_ciStatus = a})

-- | The elastic network interfaces associated with the container instance.
ciAttachments :: Lens' ContainerInstance [Attachment]
ciAttachments = lens _ciAttachments (\ s a -> s{_ciAttachments = a}) . _Default . _Coerce

-- | The number of tasks on the container instance that are in the @RUNNING@ status.
ciRunningTasksCount :: Lens' ContainerInstance (Maybe Int)
ciRunningTasksCount = lens _ciRunningTasksCount (\ s a -> s{_ciRunningTasksCount = a})

-- | For CPU and memory resource types, this parameter describes the remaining CPU and memory that has not already been allocated to tasks and is therefore available for new tasks. For port resource types, this parameter describes the ports that were reserved by the Amazon ECS container agent (at instance registration time) and any task containers that have reserved port mappings on the host (with the @host@ or @bridge@ network mode). Any port that is not specified here is available for new tasks.
ciRemainingResources :: Lens' ContainerInstance [Resource]
ciRemainingResources = lens _ciRemainingResources (\ s a -> s{_ciRemainingResources = a}) . _Default . _Coerce

-- | The EC2 instance ID of the container instance.
ciEc2InstanceId :: Lens' ContainerInstance (Maybe Text)
ciEc2InstanceId = lens _ciEc2InstanceId (\ s a -> s{_ciEc2InstanceId = a})

-- | The Amazon Resource Name (ARN) of the container instance. The ARN contains the @arn:aws:ecs@ namespace, followed by the Region of the container instance, the AWS account ID of the container instance owner, the @container-instance@ namespace, and then the container instance ID. For example, @arn:aws:ecs:/region/ :/aws_account_id/ :container-instance//container_instance_ID/ @ .
ciContainerInstanceARN :: Lens' ContainerInstance (Maybe Text)
ciContainerInstanceARN = lens _ciContainerInstanceARN (\ s a -> s{_ciContainerInstanceARN = a})

-- | This parameter returns @true@ if the agent is connected to Amazon ECS. Registered instances with an agent that may be unhealthy or stopped return @false@ . Only instances connected to an agent can accept placement requests.
ciAgentConnected :: Lens' ContainerInstance (Maybe Bool)
ciAgentConnected = lens _ciAgentConnected (\ s a -> s{_ciAgentConnected = a})

-- | The version information for the Amazon ECS container agent and Docker daemon running on the container instance.
ciVersionInfo :: Lens' ContainerInstance (Maybe VersionInfo)
ciVersionInfo = lens _ciVersionInfo (\ s a -> s{_ciVersionInfo = a})

-- | The status of the most recent agent update. If an update has never been requested, this value is @NULL@ .
ciAgentUpdateStatus :: Lens' ContainerInstance (Maybe AgentUpdateStatus)
ciAgentUpdateStatus = lens _ciAgentUpdateStatus (\ s a -> s{_ciAgentUpdateStatus = a})

-- | The attributes set for the container instance, either by the Amazon ECS container agent at instance registration or manually with the 'PutAttributes' operation.
ciAttributes :: Lens' ContainerInstance [Attribute]
ciAttributes = lens _ciAttributes (\ s a -> s{_ciAttributes = a}) . _Default . _Coerce

-- | The version counter for the container instance. Every time a container instance experiences a change that triggers a CloudWatch event, the version counter is incremented. If you are replicating your Amazon ECS container instance state with CloudWatch Events, you can compare the version of a container instance reported by the Amazon ECS APIs with the version reported in CloudWatch Events for the container instance (inside the @detail@ object) to verify that the version in your event stream is current.
ciVersion :: Lens' ContainerInstance (Maybe Integer)
ciVersion = lens _ciVersion (\ s a -> s{_ciVersion = a})

-- | The number of tasks on the container instance that are in the @PENDING@ status.
ciPendingTasksCount :: Lens' ContainerInstance (Maybe Int)
ciPendingTasksCount = lens _ciPendingTasksCount (\ s a -> s{_ciPendingTasksCount = a})

-- | The Unix timestamp for when the container instance was registered.
ciRegisteredAt :: Lens' ContainerInstance (Maybe UTCTime)
ciRegisteredAt = lens _ciRegisteredAt (\ s a -> s{_ciRegisteredAt = a}) . mapping _Time

-- | The metadata that you apply to the container instance to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
ciTags :: Lens' ContainerInstance [Tag]
ciTags = lens _ciTags (\ s a -> s{_ciTags = a}) . _Default . _Coerce

-- | For CPU and memory resource types, this parameter describes the amount of each resource that was available on the container instance when the container agent registered it with Amazon ECS. This value represents the total amount of CPU and memory that can be allocated on this container instance to tasks. For port resource types, this parameter describes the ports that were reserved by the Amazon ECS container agent when it registered the container instance with Amazon ECS.
ciRegisteredResources :: Lens' ContainerInstance [Resource]
ciRegisteredResources = lens _ciRegisteredResources (\ s a -> s{_ciRegisteredResources = a}) . _Default . _Coerce

instance FromJSON ContainerInstance where
        parseJSON
          = withObject "ContainerInstance"
              (\ x ->
                 ContainerInstance' <$>
                   (x .:? "status") <*> (x .:? "attachments" .!= mempty)
                     <*> (x .:? "runningTasksCount")
                     <*> (x .:? "remainingResources" .!= mempty)
                     <*> (x .:? "ec2InstanceId")
                     <*> (x .:? "containerInstanceArn")
                     <*> (x .:? "agentConnected")
                     <*> (x .:? "versionInfo")
                     <*> (x .:? "agentUpdateStatus")
                     <*> (x .:? "attributes" .!= mempty)
                     <*> (x .:? "version")
                     <*> (x .:? "pendingTasksCount")
                     <*> (x .:? "registeredAt")
                     <*> (x .:? "tags" .!= mempty)
                     <*> (x .:? "registeredResources" .!= mempty))

instance Hashable ContainerInstance where

instance NFData ContainerInstance where

-- | The overrides that should be sent to a container.
--
--
--
-- /See:/ 'containerOverride' smart constructor.
data ContainerOverride = ContainerOverride'
  { _coCommand              :: !(Maybe [Text])
  , _coEnvironment          :: !(Maybe [KeyValuePair])
  , _coResourceRequirements :: !(Maybe [ResourceRequirement])
  , _coMemory               :: !(Maybe Int)
  , _coName                 :: !(Maybe Text)
  , _coCpu                  :: !(Maybe Int)
  , _coMemoryReservation    :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ContainerOverride' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coCommand' - The command to send to the container that overrides the default command from the Docker image or the task definition. You must also specify a container name.
--
-- * 'coEnvironment' - The environment variables to send to the container. You can add new environment variables, which are added to the container at launch, or you can override the existing environment variables from the Docker image or the task definition. You must also specify a container name.
--
-- * 'coResourceRequirements' - The type and amount of a resource to assign to a container, instead of the default value from the task definition. The only supported resource is a GPU.
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
    , _coResourceRequirements = Nothing
    , _coMemory = Nothing
    , _coName = Nothing
    , _coCpu = Nothing
    , _coMemoryReservation = Nothing
    }


-- | The command to send to the container that overrides the default command from the Docker image or the task definition. You must also specify a container name.
coCommand :: Lens' ContainerOverride [Text]
coCommand = lens _coCommand (\ s a -> s{_coCommand = a}) . _Default . _Coerce

-- | The environment variables to send to the container. You can add new environment variables, which are added to the container at launch, or you can override the existing environment variables from the Docker image or the task definition. You must also specify a container name.
coEnvironment :: Lens' ContainerOverride [KeyValuePair]
coEnvironment = lens _coEnvironment (\ s a -> s{_coEnvironment = a}) . _Default . _Coerce

-- | The type and amount of a resource to assign to a container, instead of the default value from the task definition. The only supported resource is a GPU.
coResourceRequirements :: Lens' ContainerOverride [ResourceRequirement]
coResourceRequirements = lens _coResourceRequirements (\ s a -> s{_coResourceRequirements = a}) . _Default . _Coerce

-- | The hard limit (in MiB) of memory to present to the container, instead of the default value from the task definition. If your container attempts to exceed the memory specified here, the container is killed. You must also specify a container name.
coMemory :: Lens' ContainerOverride (Maybe Int)
coMemory = lens _coMemory (\ s a -> s{_coMemory = a})

-- | The name of the container that receives the override. This parameter is required if any override is specified.
coName :: Lens' ContainerOverride (Maybe Text)
coName = lens _coName (\ s a -> s{_coName = a})

-- | The number of @cpu@ units reserved for the container, instead of the default value from the task definition. You must also specify a container name.
coCpu :: Lens' ContainerOverride (Maybe Int)
coCpu = lens _coCpu (\ s a -> s{_coCpu = a})

-- | The soft limit (in MiB) of memory to reserve for the container, instead of the default value from the task definition. You must also specify a container name.
coMemoryReservation :: Lens' ContainerOverride (Maybe Int)
coMemoryReservation = lens _coMemoryReservation (\ s a -> s{_coMemoryReservation = a})

instance FromJSON ContainerOverride where
        parseJSON
          = withObject "ContainerOverride"
              (\ x ->
                 ContainerOverride' <$>
                   (x .:? "command" .!= mempty) <*>
                     (x .:? "environment" .!= mempty)
                     <*> (x .:? "resourceRequirements" .!= mempty)
                     <*> (x .:? "memory")
                     <*> (x .:? "name")
                     <*> (x .:? "cpu")
                     <*> (x .:? "memoryReservation"))

instance Hashable ContainerOverride where

instance NFData ContainerOverride where

instance ToJSON ContainerOverride where
        toJSON ContainerOverride'{..}
          = object
              (catMaybes
                 [("command" .=) <$> _coCommand,
                  ("environment" .=) <$> _coEnvironment,
                  ("resourceRequirements" .=) <$>
                    _coResourceRequirements,
                  ("memory" .=) <$> _coMemory, ("name" .=) <$> _coName,
                  ("cpu" .=) <$> _coCpu,
                  ("memoryReservation" .=) <$> _coMemoryReservation])

-- | Details on a service within a cluster
--
--
--
-- /See:/ 'containerService' smart constructor.
data ContainerService = ContainerService'
  { _csTaskSets                      :: !(Maybe [TaskSet])
  , _csRunningCount                  :: !(Maybe Int)
  , _csStatus                        :: !(Maybe Text)
  , _csClusterARN                    :: !(Maybe Text)
  , _csPropagateTags                 :: !(Maybe PropagateTags)
  , _csCreatedAt                     :: !(Maybe POSIX)
  , _csPlatformVersion               :: !(Maybe Text)
  , _csEnableECSManagedTags          :: !(Maybe Bool)
  , _csCreatedBy                     :: !(Maybe Text)
  , _csDesiredCount                  :: !(Maybe Int)
  , _csLoadBalancers                 :: !(Maybe [LoadBalancer])
  , _csPendingCount                  :: !(Maybe Int)
  , _csPlacementConstraints          :: !(Maybe [PlacementConstraint])
  , _csEvents                        :: !(Maybe [ServiceEvent])
  , _csPlacementStrategy             :: !(Maybe [PlacementStrategy])
  , _csDeployments                   :: !(Maybe [Deployment])
  , _csServiceName                   :: !(Maybe Text)
  , _csDeploymentController          :: !(Maybe DeploymentController)
  , _csLaunchType                    :: !(Maybe LaunchType)
  , _csServiceARN                    :: !(Maybe Text)
  , _csTaskDefinition                :: !(Maybe Text)
  , _csSchedulingStrategy            :: !(Maybe SchedulingStrategy)
  , _csHealthCheckGracePeriodSeconds :: !(Maybe Int)
  , _csNetworkConfiguration          :: !(Maybe NetworkConfiguration)
  , _csServiceRegistries             :: !(Maybe [ServiceRegistry])
  , _csTags                          :: !(Maybe [Tag])
  , _csRoleARN                       :: !(Maybe Text)
  , _csDeploymentConfiguration       :: !(Maybe DeploymentConfiguration)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ContainerService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csTaskSets' - Information about a set of Amazon ECS tasks in an AWS CodeDeploy deployment. An Amazon ECS task set includes details such as the desired number of tasks, how many tasks are running, and whether the task set serves production traffic.
--
-- * 'csRunningCount' - The number of tasks in the cluster that are in the @RUNNING@ state.
--
-- * 'csStatus' - The status of the service. The valid values are @ACTIVE@ , @DRAINING@ , or @INACTIVE@ .
--
-- * 'csClusterARN' - The Amazon Resource Name (ARN) of the cluster that hosts the service.
--
-- * 'csPropagateTags' - Specifies whether to propagate the tags from the task definition or the service to the task. If no value is specified, the tags are not propagated.
--
-- * 'csCreatedAt' - The Unix timestamp for when the service was created.
--
-- * 'csPlatformVersion' - The platform version on which your tasks in the service are running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'csEnableECSManagedTags' - Specifies whether to enable Amazon ECS managed tags for the tasks in the service. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'csCreatedBy' - The principal that created the service.
--
-- * 'csDesiredCount' - The desired number of instantiations of the task definition to keep running on the service. This value is specified when the service is created with 'CreateService' , and it can be modified with 'UpdateService' .
--
-- * 'csLoadBalancers' - A list of Elastic Load Balancing load balancer objects, containing the load balancer name, the container name (as it appears in a container definition), and the container port to access from the load balancer. Services with tasks that use the @awsvpc@ network mode (for example, those with the Fargate launch type) only support Application Load Balancers and Network Load Balancers. Classic Load Balancers are not supported. Also, when you create any target groups for these services, you must choose @ip@ as the target type, not @instance@ . Tasks that use the @awsvpc@ network mode are associated with an elastic network interface, not an Amazon EC2 instance.
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
-- * 'csServiceName' - The name of your service. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. Service names must be unique within a cluster, but you can have similarly named services in multiple clusters within a Region or across multiple Regions.
--
-- * 'csDeploymentController' - The deployment controller type the service is using.
--
-- * 'csLaunchType' - The launch type on which your service is running. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'csServiceARN' - The ARN that identifies the service. The ARN contains the @arn:aws:ecs@ namespace, followed by the Region of the service, the AWS account ID of the service owner, the @service@ namespace, and then the service name. For example, @arn:aws:ecs:/region/ :/012345678910/ :service//my-service/ @ .
--
-- * 'csTaskDefinition' - The task definition to use for tasks in the service. This value is specified when the service is created with 'CreateService' , and it can be modified with 'UpdateService' .
--
-- * 'csSchedulingStrategy' - The scheduling strategy to use for the service. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs_services.html Services> . There are two service scheduler strategies available:     * @REPLICA@ -The replica scheduling strategy places and maintains the desired number of tasks across your cluster. By default, the service scheduler spreads tasks across Availability Zones. You can use task placement strategies and constraints to customize task placement decisions.     * @DAEMON@ -The daemon scheduling strategy deploys exactly one task on each container instance in your cluster. When you are using this strategy, do not specify a desired number of tasks or any task placement strategies.
--
-- * 'csHealthCheckGracePeriodSeconds' - The period of time, in seconds, that the Amazon ECS service scheduler ignores unhealthy Elastic Load Balancing target health checks after a task has first started.
--
-- * 'csNetworkConfiguration' - The VPC subnet and security group configuration for tasks that receive their own elastic network interface by using the @awsvpc@ networking mode.
--
-- * 'csServiceRegistries' -
--
-- * 'csTags' - The metadata that you apply to the service to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
--
-- * 'csRoleARN' - The ARN of the IAM role associated with the service that allows the Amazon ECS container agent to register container instances with an Elastic Load Balancing load balancer.
--
-- * 'csDeploymentConfiguration' - Optional deployment parameters that control how many tasks run during the deployment and the ordering of stopping and starting tasks.
containerService
    :: ContainerService
containerService =
  ContainerService'
    { _csTaskSets = Nothing
    , _csRunningCount = Nothing
    , _csStatus = Nothing
    , _csClusterARN = Nothing
    , _csPropagateTags = Nothing
    , _csCreatedAt = Nothing
    , _csPlatformVersion = Nothing
    , _csEnableECSManagedTags = Nothing
    , _csCreatedBy = Nothing
    , _csDesiredCount = Nothing
    , _csLoadBalancers = Nothing
    , _csPendingCount = Nothing
    , _csPlacementConstraints = Nothing
    , _csEvents = Nothing
    , _csPlacementStrategy = Nothing
    , _csDeployments = Nothing
    , _csServiceName = Nothing
    , _csDeploymentController = Nothing
    , _csLaunchType = Nothing
    , _csServiceARN = Nothing
    , _csTaskDefinition = Nothing
    , _csSchedulingStrategy = Nothing
    , _csHealthCheckGracePeriodSeconds = Nothing
    , _csNetworkConfiguration = Nothing
    , _csServiceRegistries = Nothing
    , _csTags = Nothing
    , _csRoleARN = Nothing
    , _csDeploymentConfiguration = Nothing
    }


-- | Information about a set of Amazon ECS tasks in an AWS CodeDeploy deployment. An Amazon ECS task set includes details such as the desired number of tasks, how many tasks are running, and whether the task set serves production traffic.
csTaskSets :: Lens' ContainerService [TaskSet]
csTaskSets = lens _csTaskSets (\ s a -> s{_csTaskSets = a}) . _Default . _Coerce

-- | The number of tasks in the cluster that are in the @RUNNING@ state.
csRunningCount :: Lens' ContainerService (Maybe Int)
csRunningCount = lens _csRunningCount (\ s a -> s{_csRunningCount = a})

-- | The status of the service. The valid values are @ACTIVE@ , @DRAINING@ , or @INACTIVE@ .
csStatus :: Lens' ContainerService (Maybe Text)
csStatus = lens _csStatus (\ s a -> s{_csStatus = a})

-- | The Amazon Resource Name (ARN) of the cluster that hosts the service.
csClusterARN :: Lens' ContainerService (Maybe Text)
csClusterARN = lens _csClusterARN (\ s a -> s{_csClusterARN = a})

-- | Specifies whether to propagate the tags from the task definition or the service to the task. If no value is specified, the tags are not propagated.
csPropagateTags :: Lens' ContainerService (Maybe PropagateTags)
csPropagateTags = lens _csPropagateTags (\ s a -> s{_csPropagateTags = a})

-- | The Unix timestamp for when the service was created.
csCreatedAt :: Lens' ContainerService (Maybe UTCTime)
csCreatedAt = lens _csCreatedAt (\ s a -> s{_csCreatedAt = a}) . mapping _Time

-- | The platform version on which your tasks in the service are running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
csPlatformVersion :: Lens' ContainerService (Maybe Text)
csPlatformVersion = lens _csPlatformVersion (\ s a -> s{_csPlatformVersion = a})

-- | Specifies whether to enable Amazon ECS managed tags for the tasks in the service. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources> in the /Amazon Elastic Container Service Developer Guide/ .
csEnableECSManagedTags :: Lens' ContainerService (Maybe Bool)
csEnableECSManagedTags = lens _csEnableECSManagedTags (\ s a -> s{_csEnableECSManagedTags = a})

-- | The principal that created the service.
csCreatedBy :: Lens' ContainerService (Maybe Text)
csCreatedBy = lens _csCreatedBy (\ s a -> s{_csCreatedBy = a})

-- | The desired number of instantiations of the task definition to keep running on the service. This value is specified when the service is created with 'CreateService' , and it can be modified with 'UpdateService' .
csDesiredCount :: Lens' ContainerService (Maybe Int)
csDesiredCount = lens _csDesiredCount (\ s a -> s{_csDesiredCount = a})

-- | A list of Elastic Load Balancing load balancer objects, containing the load balancer name, the container name (as it appears in a container definition), and the container port to access from the load balancer. Services with tasks that use the @awsvpc@ network mode (for example, those with the Fargate launch type) only support Application Load Balancers and Network Load Balancers. Classic Load Balancers are not supported. Also, when you create any target groups for these services, you must choose @ip@ as the target type, not @instance@ . Tasks that use the @awsvpc@ network mode are associated with an elastic network interface, not an Amazon EC2 instance.
csLoadBalancers :: Lens' ContainerService [LoadBalancer]
csLoadBalancers = lens _csLoadBalancers (\ s a -> s{_csLoadBalancers = a}) . _Default . _Coerce

-- | The number of tasks in the cluster that are in the @PENDING@ state.
csPendingCount :: Lens' ContainerService (Maybe Int)
csPendingCount = lens _csPendingCount (\ s a -> s{_csPendingCount = a})

-- | The placement constraints for the tasks in the service.
csPlacementConstraints :: Lens' ContainerService [PlacementConstraint]
csPlacementConstraints = lens _csPlacementConstraints (\ s a -> s{_csPlacementConstraints = a}) . _Default . _Coerce

-- | The event stream for your service. A maximum of 100 of the latest events are displayed.
csEvents :: Lens' ContainerService [ServiceEvent]
csEvents = lens _csEvents (\ s a -> s{_csEvents = a}) . _Default . _Coerce

-- | The placement strategy that determines how tasks for the service are placed.
csPlacementStrategy :: Lens' ContainerService [PlacementStrategy]
csPlacementStrategy = lens _csPlacementStrategy (\ s a -> s{_csPlacementStrategy = a}) . _Default . _Coerce

-- | The current state of deployments for the service.
csDeployments :: Lens' ContainerService [Deployment]
csDeployments = lens _csDeployments (\ s a -> s{_csDeployments = a}) . _Default . _Coerce

-- | The name of your service. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. Service names must be unique within a cluster, but you can have similarly named services in multiple clusters within a Region or across multiple Regions.
csServiceName :: Lens' ContainerService (Maybe Text)
csServiceName = lens _csServiceName (\ s a -> s{_csServiceName = a})

-- | The deployment controller type the service is using.
csDeploymentController :: Lens' ContainerService (Maybe DeploymentController)
csDeploymentController = lens _csDeploymentController (\ s a -> s{_csDeploymentController = a})

-- | The launch type on which your service is running. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
csLaunchType :: Lens' ContainerService (Maybe LaunchType)
csLaunchType = lens _csLaunchType (\ s a -> s{_csLaunchType = a})

-- | The ARN that identifies the service. The ARN contains the @arn:aws:ecs@ namespace, followed by the Region of the service, the AWS account ID of the service owner, the @service@ namespace, and then the service name. For example, @arn:aws:ecs:/region/ :/012345678910/ :service//my-service/ @ .
csServiceARN :: Lens' ContainerService (Maybe Text)
csServiceARN = lens _csServiceARN (\ s a -> s{_csServiceARN = a})

-- | The task definition to use for tasks in the service. This value is specified when the service is created with 'CreateService' , and it can be modified with 'UpdateService' .
csTaskDefinition :: Lens' ContainerService (Maybe Text)
csTaskDefinition = lens _csTaskDefinition (\ s a -> s{_csTaskDefinition = a})

-- | The scheduling strategy to use for the service. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs_services.html Services> . There are two service scheduler strategies available:     * @REPLICA@ -The replica scheduling strategy places and maintains the desired number of tasks across your cluster. By default, the service scheduler spreads tasks across Availability Zones. You can use task placement strategies and constraints to customize task placement decisions.     * @DAEMON@ -The daemon scheduling strategy deploys exactly one task on each container instance in your cluster. When you are using this strategy, do not specify a desired number of tasks or any task placement strategies.
csSchedulingStrategy :: Lens' ContainerService (Maybe SchedulingStrategy)
csSchedulingStrategy = lens _csSchedulingStrategy (\ s a -> s{_csSchedulingStrategy = a})

-- | The period of time, in seconds, that the Amazon ECS service scheduler ignores unhealthy Elastic Load Balancing target health checks after a task has first started.
csHealthCheckGracePeriodSeconds :: Lens' ContainerService (Maybe Int)
csHealthCheckGracePeriodSeconds = lens _csHealthCheckGracePeriodSeconds (\ s a -> s{_csHealthCheckGracePeriodSeconds = a})

-- | The VPC subnet and security group configuration for tasks that receive their own elastic network interface by using the @awsvpc@ networking mode.
csNetworkConfiguration :: Lens' ContainerService (Maybe NetworkConfiguration)
csNetworkConfiguration = lens _csNetworkConfiguration (\ s a -> s{_csNetworkConfiguration = a})

-- |
csServiceRegistries :: Lens' ContainerService [ServiceRegistry]
csServiceRegistries = lens _csServiceRegistries (\ s a -> s{_csServiceRegistries = a}) . _Default . _Coerce

-- | The metadata that you apply to the service to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
csTags :: Lens' ContainerService [Tag]
csTags = lens _csTags (\ s a -> s{_csTags = a}) . _Default . _Coerce

-- | The ARN of the IAM role associated with the service that allows the Amazon ECS container agent to register container instances with an Elastic Load Balancing load balancer.
csRoleARN :: Lens' ContainerService (Maybe Text)
csRoleARN = lens _csRoleARN (\ s a -> s{_csRoleARN = a})

-- | Optional deployment parameters that control how many tasks run during the deployment and the ordering of stopping and starting tasks.
csDeploymentConfiguration :: Lens' ContainerService (Maybe DeploymentConfiguration)
csDeploymentConfiguration = lens _csDeploymentConfiguration (\ s a -> s{_csDeploymentConfiguration = a})

instance FromJSON ContainerService where
        parseJSON
          = withObject "ContainerService"
              (\ x ->
                 ContainerService' <$>
                   (x .:? "taskSets" .!= mempty) <*>
                     (x .:? "runningCount")
                     <*> (x .:? "status")
                     <*> (x .:? "clusterArn")
                     <*> (x .:? "propagateTags")
                     <*> (x .:? "createdAt")
                     <*> (x .:? "platformVersion")
                     <*> (x .:? "enableECSManagedTags")
                     <*> (x .:? "createdBy")
                     <*> (x .:? "desiredCount")
                     <*> (x .:? "loadBalancers" .!= mempty)
                     <*> (x .:? "pendingCount")
                     <*> (x .:? "placementConstraints" .!= mempty)
                     <*> (x .:? "events" .!= mempty)
                     <*> (x .:? "placementStrategy" .!= mempty)
                     <*> (x .:? "deployments" .!= mempty)
                     <*> (x .:? "serviceName")
                     <*> (x .:? "deploymentController")
                     <*> (x .:? "launchType")
                     <*> (x .:? "serviceArn")
                     <*> (x .:? "taskDefinition")
                     <*> (x .:? "schedulingStrategy")
                     <*> (x .:? "healthCheckGracePeriodSeconds")
                     <*> (x .:? "networkConfiguration")
                     <*> (x .:? "serviceRegistries" .!= mempty)
                     <*> (x .:? "tags" .!= mempty)
                     <*> (x .:? "roleArn")
                     <*> (x .:? "deploymentConfiguration"))

instance Hashable ContainerService where

instance NFData ContainerService where

-- | An object representing a change in state for a container.
--
--
--
-- /See:/ 'containerStateChange' smart constructor.
data ContainerStateChange = ContainerStateChange'
  { _cscNetworkBindings :: !(Maybe [NetworkBinding])
  , _cscStatus          :: !(Maybe Text)
  , _cscContainerName   :: !(Maybe Text)
  , _cscReason          :: !(Maybe Text)
  , _cscExitCode        :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ContainerStateChange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cscNetworkBindings' - Any network bindings associated with the container.
--
-- * 'cscStatus' - The status of the container.
--
-- * 'cscContainerName' - The name of the container.
--
-- * 'cscReason' - The reason for the state change.
--
-- * 'cscExitCode' - The exit code for the container, if the state change is a result of the container exiting.
containerStateChange
    :: ContainerStateChange
containerStateChange =
  ContainerStateChange'
    { _cscNetworkBindings = Nothing
    , _cscStatus = Nothing
    , _cscContainerName = Nothing
    , _cscReason = Nothing
    , _cscExitCode = Nothing
    }


-- | Any network bindings associated with the container.
cscNetworkBindings :: Lens' ContainerStateChange [NetworkBinding]
cscNetworkBindings = lens _cscNetworkBindings (\ s a -> s{_cscNetworkBindings = a}) . _Default . _Coerce

-- | The status of the container.
cscStatus :: Lens' ContainerStateChange (Maybe Text)
cscStatus = lens _cscStatus (\ s a -> s{_cscStatus = a})

-- | The name of the container.
cscContainerName :: Lens' ContainerStateChange (Maybe Text)
cscContainerName = lens _cscContainerName (\ s a -> s{_cscContainerName = a})

-- | The reason for the state change.
cscReason :: Lens' ContainerStateChange (Maybe Text)
cscReason = lens _cscReason (\ s a -> s{_cscReason = a})

-- | The exit code for the container, if the state change is a result of the container exiting.
cscExitCode :: Lens' ContainerStateChange (Maybe Int)
cscExitCode = lens _cscExitCode (\ s a -> s{_cscExitCode = a})

instance Hashable ContainerStateChange where

instance NFData ContainerStateChange where

instance ToJSON ContainerStateChange where
        toJSON ContainerStateChange'{..}
          = object
              (catMaybes
                 [("networkBindings" .=) <$> _cscNetworkBindings,
                  ("status" .=) <$> _cscStatus,
                  ("containerName" .=) <$> _cscContainerName,
                  ("reason" .=) <$> _cscReason,
                  ("exitCode" .=) <$> _cscExitCode])

-- | The details of an Amazon ECS service deployment. This is used when a service uses the @CODE_DEPLOY@ deployment controller type.
--
--
--
-- /See:/ 'deployment' smart constructor.
data Deployment = Deployment'
  { _dRunningCount         :: !(Maybe Int)
  , _dStatus               :: !(Maybe Text)
  , _dCreatedAt            :: !(Maybe POSIX)
  , _dPlatformVersion      :: !(Maybe Text)
  , _dDesiredCount         :: !(Maybe Int)
  , _dPendingCount         :: !(Maybe Int)
  , _dId                   :: !(Maybe Text)
  , _dLaunchType           :: !(Maybe LaunchType)
  , _dUpdatedAt            :: !(Maybe POSIX)
  , _dTaskDefinition       :: !(Maybe Text)
  , _dNetworkConfiguration :: !(Maybe NetworkConfiguration)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Deployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dRunningCount' - The number of tasks in the deployment that are in the @RUNNING@ status.
--
-- * 'dStatus' - The status of the deployment. The following describes each state:     * PRIMARY    * The most recent deployment of a service.     * ACTIVE    * A service deployment that still has running tasks, but are in the process of being replaced with a new @PRIMARY@ deployment.     * INACTIVE    * A deployment that has been completely replaced.
--
-- * 'dCreatedAt' - The Unix timestamp for when the service deployment was created.
--
-- * 'dPlatformVersion' - The platform version on which your tasks in the service are running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'dDesiredCount' - The most recent desired count of tasks that was specified for the service to deploy or maintain.
--
-- * 'dPendingCount' - The number of tasks in the deployment that are in the @PENDING@ status.
--
-- * 'dId' - The ID of the deployment.
--
-- * 'dLaunchType' - The launch type the tasks in the service are using. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'dUpdatedAt' - The Unix timestamp for when the service deployment was last updated.
--
-- * 'dTaskDefinition' - The most recent task definition that was specified for the tasks in the service to use.
--
-- * 'dNetworkConfiguration' - The VPC subnet and security group configuration for tasks that receive their own elastic network interface by using the @awsvpc@ networking mode.
deployment
    :: Deployment
deployment =
  Deployment'
    { _dRunningCount = Nothing
    , _dStatus = Nothing
    , _dCreatedAt = Nothing
    , _dPlatformVersion = Nothing
    , _dDesiredCount = Nothing
    , _dPendingCount = Nothing
    , _dId = Nothing
    , _dLaunchType = Nothing
    , _dUpdatedAt = Nothing
    , _dTaskDefinition = Nothing
    , _dNetworkConfiguration = Nothing
    }


-- | The number of tasks in the deployment that are in the @RUNNING@ status.
dRunningCount :: Lens' Deployment (Maybe Int)
dRunningCount = lens _dRunningCount (\ s a -> s{_dRunningCount = a})

-- | The status of the deployment. The following describes each state:     * PRIMARY    * The most recent deployment of a service.     * ACTIVE    * A service deployment that still has running tasks, but are in the process of being replaced with a new @PRIMARY@ deployment.     * INACTIVE    * A deployment that has been completely replaced.
dStatus :: Lens' Deployment (Maybe Text)
dStatus = lens _dStatus (\ s a -> s{_dStatus = a})

-- | The Unix timestamp for when the service deployment was created.
dCreatedAt :: Lens' Deployment (Maybe UTCTime)
dCreatedAt = lens _dCreatedAt (\ s a -> s{_dCreatedAt = a}) . mapping _Time

-- | The platform version on which your tasks in the service are running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
dPlatformVersion :: Lens' Deployment (Maybe Text)
dPlatformVersion = lens _dPlatformVersion (\ s a -> s{_dPlatformVersion = a})

-- | The most recent desired count of tasks that was specified for the service to deploy or maintain.
dDesiredCount :: Lens' Deployment (Maybe Int)
dDesiredCount = lens _dDesiredCount (\ s a -> s{_dDesiredCount = a})

-- | The number of tasks in the deployment that are in the @PENDING@ status.
dPendingCount :: Lens' Deployment (Maybe Int)
dPendingCount = lens _dPendingCount (\ s a -> s{_dPendingCount = a})

-- | The ID of the deployment.
dId :: Lens' Deployment (Maybe Text)
dId = lens _dId (\ s a -> s{_dId = a})

-- | The launch type the tasks in the service are using. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
dLaunchType :: Lens' Deployment (Maybe LaunchType)
dLaunchType = lens _dLaunchType (\ s a -> s{_dLaunchType = a})

-- | The Unix timestamp for when the service deployment was last updated.
dUpdatedAt :: Lens' Deployment (Maybe UTCTime)
dUpdatedAt = lens _dUpdatedAt (\ s a -> s{_dUpdatedAt = a}) . mapping _Time

-- | The most recent task definition that was specified for the tasks in the service to use.
dTaskDefinition :: Lens' Deployment (Maybe Text)
dTaskDefinition = lens _dTaskDefinition (\ s a -> s{_dTaskDefinition = a})

-- | The VPC subnet and security group configuration for tasks that receive their own elastic network interface by using the @awsvpc@ networking mode.
dNetworkConfiguration :: Lens' Deployment (Maybe NetworkConfiguration)
dNetworkConfiguration = lens _dNetworkConfiguration (\ s a -> s{_dNetworkConfiguration = a})

instance FromJSON Deployment where
        parseJSON
          = withObject "Deployment"
              (\ x ->
                 Deployment' <$>
                   (x .:? "runningCount") <*> (x .:? "status") <*>
                     (x .:? "createdAt")
                     <*> (x .:? "platformVersion")
                     <*> (x .:? "desiredCount")
                     <*> (x .:? "pendingCount")
                     <*> (x .:? "id")
                     <*> (x .:? "launchType")
                     <*> (x .:? "updatedAt")
                     <*> (x .:? "taskDefinition")
                     <*> (x .:? "networkConfiguration"))

instance Hashable Deployment where

instance NFData Deployment where

-- | Optional deployment parameters that control how many tasks run during the deployment and the ordering of stopping and starting tasks.
--
--
--
-- /See:/ 'deploymentConfiguration' smart constructor.
data DeploymentConfiguration = DeploymentConfiguration'
  { _dcMinimumHealthyPercent :: !(Maybe Int)
  , _dcMaximumPercent        :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeploymentConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcMinimumHealthyPercent' - If a service is using the rolling update (@ECS@ ) deployment type, the __minimum healthy percent__ represents a lower limit on the number of tasks in a service that must remain in the @RUNNING@ state during a deployment, as a percentage of the desired number of tasks (rounded up to the nearest integer), and while any container instances are in the @DRAINING@ state if the service contains tasks using the EC2 launch type. This parameter enables you to deploy without using additional cluster capacity. For example, if your service has a desired number of four tasks and a minimum healthy percent of 50%, the scheduler may stop two existing tasks to free up cluster capacity before starting two new tasks. Tasks for services that /do not/ use a load balancer are considered healthy if they are in the @RUNNING@ state; tasks for services that /do/ use a load balancer are considered healthy if they are in the @RUNNING@ state and they are reported as healthy by the load balancer. The default value for minimum healthy percent is 100%. If a service is using the blue/green (@CODE_DEPLOY@ ) deployment type and tasks that use the EC2 launch type, the __minimum healthy percent__ value is set to the default value and is used to define the lower limit on the number of the tasks in the service that remain in the @RUNNING@ state while the container instances are in the @DRAINING@ state. If the tasks in the service use the Fargate launch type, the minimum healthy percent value is not used, although it is returned when describing your service.
--
-- * 'dcMaximumPercent' - If a service is using the rolling update (@ECS@ ) deployment type, the __maximum percent__ parameter represents an upper limit on the number of tasks in a service that are allowed in the @RUNNING@ or @PENDING@ state during a deployment, as a percentage of the desired number of tasks (rounded down to the nearest integer), and while any container instances are in the @DRAINING@ state if the service contains tasks using the EC2 launch type. This parameter enables you to define the deployment batch size. For example, if your service has a desired number of four tasks and a maximum percent value of 200%, the scheduler may start four new tasks before stopping the four older tasks (provided that the cluster resources required to do this are available). The default value for maximum percent is 200%. If a service is using the blue/green (@CODE_DEPLOY@ ) deployment type and tasks that use the EC2 launch type, the __maximum percent__ value is set to the default value and is used to define the upper limit on the number of the tasks in the service that remain in the @RUNNING@ state while the container instances are in the @DRAINING@ state. If the tasks in the service use the Fargate launch type, the maximum percent value is not used, although it is returned when describing your service.
deploymentConfiguration
    :: DeploymentConfiguration
deploymentConfiguration =
  DeploymentConfiguration'
    {_dcMinimumHealthyPercent = Nothing, _dcMaximumPercent = Nothing}


-- | If a service is using the rolling update (@ECS@ ) deployment type, the __minimum healthy percent__ represents a lower limit on the number of tasks in a service that must remain in the @RUNNING@ state during a deployment, as a percentage of the desired number of tasks (rounded up to the nearest integer), and while any container instances are in the @DRAINING@ state if the service contains tasks using the EC2 launch type. This parameter enables you to deploy without using additional cluster capacity. For example, if your service has a desired number of four tasks and a minimum healthy percent of 50%, the scheduler may stop two existing tasks to free up cluster capacity before starting two new tasks. Tasks for services that /do not/ use a load balancer are considered healthy if they are in the @RUNNING@ state; tasks for services that /do/ use a load balancer are considered healthy if they are in the @RUNNING@ state and they are reported as healthy by the load balancer. The default value for minimum healthy percent is 100%. If a service is using the blue/green (@CODE_DEPLOY@ ) deployment type and tasks that use the EC2 launch type, the __minimum healthy percent__ value is set to the default value and is used to define the lower limit on the number of the tasks in the service that remain in the @RUNNING@ state while the container instances are in the @DRAINING@ state. If the tasks in the service use the Fargate launch type, the minimum healthy percent value is not used, although it is returned when describing your service.
dcMinimumHealthyPercent :: Lens' DeploymentConfiguration (Maybe Int)
dcMinimumHealthyPercent = lens _dcMinimumHealthyPercent (\ s a -> s{_dcMinimumHealthyPercent = a})

-- | If a service is using the rolling update (@ECS@ ) deployment type, the __maximum percent__ parameter represents an upper limit on the number of tasks in a service that are allowed in the @RUNNING@ or @PENDING@ state during a deployment, as a percentage of the desired number of tasks (rounded down to the nearest integer), and while any container instances are in the @DRAINING@ state if the service contains tasks using the EC2 launch type. This parameter enables you to define the deployment batch size. For example, if your service has a desired number of four tasks and a maximum percent value of 200%, the scheduler may start four new tasks before stopping the four older tasks (provided that the cluster resources required to do this are available). The default value for maximum percent is 200%. If a service is using the blue/green (@CODE_DEPLOY@ ) deployment type and tasks that use the EC2 launch type, the __maximum percent__ value is set to the default value and is used to define the upper limit on the number of the tasks in the service that remain in the @RUNNING@ state while the container instances are in the @DRAINING@ state. If the tasks in the service use the Fargate launch type, the maximum percent value is not used, although it is returned when describing your service.
dcMaximumPercent :: Lens' DeploymentConfiguration (Maybe Int)
dcMaximumPercent = lens _dcMaximumPercent (\ s a -> s{_dcMaximumPercent = a})

instance FromJSON DeploymentConfiguration where
        parseJSON
          = withObject "DeploymentConfiguration"
              (\ x ->
                 DeploymentConfiguration' <$>
                   (x .:? "minimumHealthyPercent") <*>
                     (x .:? "maximumPercent"))

instance Hashable DeploymentConfiguration where

instance NFData DeploymentConfiguration where

instance ToJSON DeploymentConfiguration where
        toJSON DeploymentConfiguration'{..}
          = object
              (catMaybes
                 [("minimumHealthyPercent" .=) <$>
                    _dcMinimumHealthyPercent,
                  ("maximumPercent" .=) <$> _dcMaximumPercent])

-- | The deployment controller to use for the service. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
--
--
-- /See:/ 'deploymentController' smart constructor.
newtype DeploymentController = DeploymentController'
  { _dcType :: DeploymentControllerType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeploymentController' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcType' - The deployment controller type to use. There are two deployment controller types available:     * ECS    * The rolling update (@ECS@ ) deployment type involves replacing the current running version of the container with the latest version. The number of containers Amazon ECS adds or removes from the service during a rolling update is controlled by adjusting the minimum and maximum number of healthy tasks allowed during a service deployment, as specified in the 'DeploymentConfiguration' .     * CODE_DEPLOY    * The blue/green (@CODE_DEPLOY@ ) deployment type uses the blue/green deployment model powered by AWS CodeDeploy, which allows you to verify a new deployment of a service before sending production traffic to it. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types> in the /Amazon Elastic Container Service Developer Guide/ .
deploymentController
    :: DeploymentControllerType -- ^ 'dcType'
    -> DeploymentController
deploymentController pType_ = DeploymentController' {_dcType = pType_}


-- | The deployment controller type to use. There are two deployment controller types available:     * ECS    * The rolling update (@ECS@ ) deployment type involves replacing the current running version of the container with the latest version. The number of containers Amazon ECS adds or removes from the service during a rolling update is controlled by adjusting the minimum and maximum number of healthy tasks allowed during a service deployment, as specified in the 'DeploymentConfiguration' .     * CODE_DEPLOY    * The blue/green (@CODE_DEPLOY@ ) deployment type uses the blue/green deployment model powered by AWS CodeDeploy, which allows you to verify a new deployment of a service before sending production traffic to it. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types> in the /Amazon Elastic Container Service Developer Guide/ .
dcType :: Lens' DeploymentController DeploymentControllerType
dcType = lens _dcType (\ s a -> s{_dcType = a})

instance FromJSON DeploymentController where
        parseJSON
          = withObject "DeploymentController"
              (\ x -> DeploymentController' <$> (x .: "type"))

instance Hashable DeploymentController where

instance NFData DeploymentController where

instance ToJSON DeploymentController where
        toJSON DeploymentController'{..}
          = object (catMaybes [Just ("type" .= _dcType)])

-- | An object representing a container instance host device.
--
--
--
-- /See:/ 'device' smart constructor.
data Device = Device'
  { _dContainerPath :: !(Maybe Text)
  , _dPermissions   :: !(Maybe [DeviceCgroupPermission])
  , _dHostPath      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Device' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dContainerPath' - The path inside the container at which to expose the host device.
--
-- * 'dPermissions' - The explicit permissions to provide to the container for the device. By default, the container has permissions for @read@ , @write@ , and @mknod@ for the device.
--
-- * 'dHostPath' - The path for the device on the host container instance.
device
    :: Text -- ^ 'dHostPath'
    -> Device
device pHostPath_ =
  Device'
    { _dContainerPath = Nothing
    , _dPermissions = Nothing
    , _dHostPath = pHostPath_
    }


-- | The path inside the container at which to expose the host device.
dContainerPath :: Lens' Device (Maybe Text)
dContainerPath = lens _dContainerPath (\ s a -> s{_dContainerPath = a})

-- | The explicit permissions to provide to the container for the device. By default, the container has permissions for @read@ , @write@ , and @mknod@ for the device.
dPermissions :: Lens' Device [DeviceCgroupPermission]
dPermissions = lens _dPermissions (\ s a -> s{_dPermissions = a}) . _Default . _Coerce

-- | The path for the device on the host container instance.
dHostPath :: Lens' Device Text
dHostPath = lens _dHostPath (\ s a -> s{_dHostPath = a})

instance FromJSON Device where
        parseJSON
          = withObject "Device"
              (\ x ->
                 Device' <$>
                   (x .:? "containerPath") <*>
                     (x .:? "permissions" .!= mempty)
                     <*> (x .: "hostPath"))

instance Hashable Device where

instance NFData Device where

instance ToJSON Device where
        toJSON Device'{..}
          = object
              (catMaybes
                 [("containerPath" .=) <$> _dContainerPath,
                  ("permissions" .=) <$> _dPermissions,
                  Just ("hostPath" .= _dHostPath)])

-- | This parameter is specified when you are using Docker volumes. Docker volumes are only supported when you are using the EC2 launch type. Windows containers only support the use of the @local@ driver. To use bind mounts, specify a @host@ instead.
--
--
--
-- /See:/ 'dockerVolumeConfiguration' smart constructor.
data DockerVolumeConfiguration = DockerVolumeConfiguration'
  { _dvcDriverOpts    :: !(Maybe (Map Text Text))
  , _dvcDriver        :: !(Maybe Text)
  , _dvcScope         :: !(Maybe Scope)
  , _dvcLabels        :: !(Maybe (Map Text Text))
  , _dvcAutoprovision :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DockerVolumeConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvcDriverOpts' - A map of Docker driver-specific options passed through. This parameter maps to @DriverOpts@ in the <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @xxopt@ option to <https://docs.docker.com/engine/reference/commandline/volume_create/ @docker volume create@ > .
--
-- * 'dvcDriver' - The Docker volume driver to use. The driver value must match the driver name provided by Docker because it is used for task placement. If the driver was installed using the Docker plugin CLI, use @docker plugin ls@ to retrieve the driver name from your container instance. If the driver was installed using another method, use Docker plugin discovery to retrieve the driver name. For more information, see <https://docs.docker.com/engine/extend/plugin_api/#plugin-discovery Docker plugin discovery> . This parameter maps to @Driver@ in the <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @xxdriver@ option to <https://docs.docker.com/engine/reference/commandline/volume_create/ @docker volume create@ > .
--
-- * 'dvcScope' - The scope for the Docker volume that determines its lifecycle. Docker volumes that are scoped to a @task@ are automatically provisioned when the task starts and destroyed when the task stops. Docker volumes that are scoped as @shared@ persist after the task stops.
--
-- * 'dvcLabels' - Custom metadata to add to your Docker volume. This parameter maps to @Labels@ in the <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @xxlabel@ option to <https://docs.docker.com/engine/reference/commandline/volume_create/ @docker volume create@ > .
--
-- * 'dvcAutoprovision' - If this value is @true@ , the Docker volume is created if it does not already exist.
dockerVolumeConfiguration
    :: DockerVolumeConfiguration
dockerVolumeConfiguration =
  DockerVolumeConfiguration'
    { _dvcDriverOpts = Nothing
    , _dvcDriver = Nothing
    , _dvcScope = Nothing
    , _dvcLabels = Nothing
    , _dvcAutoprovision = Nothing
    }


-- | A map of Docker driver-specific options passed through. This parameter maps to @DriverOpts@ in the <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @xxopt@ option to <https://docs.docker.com/engine/reference/commandline/volume_create/ @docker volume create@ > .
dvcDriverOpts :: Lens' DockerVolumeConfiguration (HashMap Text Text)
dvcDriverOpts = lens _dvcDriverOpts (\ s a -> s{_dvcDriverOpts = a}) . _Default . _Map

-- | The Docker volume driver to use. The driver value must match the driver name provided by Docker because it is used for task placement. If the driver was installed using the Docker plugin CLI, use @docker plugin ls@ to retrieve the driver name from your container instance. If the driver was installed using another method, use Docker plugin discovery to retrieve the driver name. For more information, see <https://docs.docker.com/engine/extend/plugin_api/#plugin-discovery Docker plugin discovery> . This parameter maps to @Driver@ in the <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @xxdriver@ option to <https://docs.docker.com/engine/reference/commandline/volume_create/ @docker volume create@ > .
dvcDriver :: Lens' DockerVolumeConfiguration (Maybe Text)
dvcDriver = lens _dvcDriver (\ s a -> s{_dvcDriver = a})

-- | The scope for the Docker volume that determines its lifecycle. Docker volumes that are scoped to a @task@ are automatically provisioned when the task starts and destroyed when the task stops. Docker volumes that are scoped as @shared@ persist after the task stops.
dvcScope :: Lens' DockerVolumeConfiguration (Maybe Scope)
dvcScope = lens _dvcScope (\ s a -> s{_dvcScope = a})

-- | Custom metadata to add to your Docker volume. This parameter maps to @Labels@ in the <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @xxlabel@ option to <https://docs.docker.com/engine/reference/commandline/volume_create/ @docker volume create@ > .
dvcLabels :: Lens' DockerVolumeConfiguration (HashMap Text Text)
dvcLabels = lens _dvcLabels (\ s a -> s{_dvcLabels = a}) . _Default . _Map

-- | If this value is @true@ , the Docker volume is created if it does not already exist.
dvcAutoprovision :: Lens' DockerVolumeConfiguration (Maybe Bool)
dvcAutoprovision = lens _dvcAutoprovision (\ s a -> s{_dvcAutoprovision = a})

instance FromJSON DockerVolumeConfiguration where
        parseJSON
          = withObject "DockerVolumeConfiguration"
              (\ x ->
                 DockerVolumeConfiguration' <$>
                   (x .:? "driverOpts" .!= mempty) <*> (x .:? "driver")
                     <*> (x .:? "scope")
                     <*> (x .:? "labels" .!= mempty)
                     <*> (x .:? "autoprovision"))

instance Hashable DockerVolumeConfiguration where

instance NFData DockerVolumeConfiguration where

instance ToJSON DockerVolumeConfiguration where
        toJSON DockerVolumeConfiguration'{..}
          = object
              (catMaybes
                 [("driverOpts" .=) <$> _dvcDriverOpts,
                  ("driver" .=) <$> _dvcDriver,
                  ("scope" .=) <$> _dvcScope,
                  ("labels" .=) <$> _dvcLabels,
                  ("autoprovision" .=) <$> _dvcAutoprovision])

-- | A failed resource.
--
--
--
-- /See:/ 'failure' smart constructor.
data Failure = Failure'
  { _fArn    :: !(Maybe Text)
  , _fReason :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Failure' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fArn' - The Amazon Resource Name (ARN) of the failed resource.
--
-- * 'fReason' - The reason for the failure.
failure
    :: Failure
failure = Failure' {_fArn = Nothing, _fReason = Nothing}


-- | The Amazon Resource Name (ARN) of the failed resource.
fArn :: Lens' Failure (Maybe Text)
fArn = lens _fArn (\ s a -> s{_fArn = a})

-- | The reason for the failure.
fReason :: Lens' Failure (Maybe Text)
fReason = lens _fReason (\ s a -> s{_fReason = a})

instance FromJSON Failure where
        parseJSON
          = withObject "Failure"
              (\ x ->
                 Failure' <$> (x .:? "arn") <*> (x .:? "reason"))

instance Hashable Failure where

instance NFData Failure where

-- | An object representing a container health check. Health check parameters that are specified in a container definition override any Docker health checks that exist in the container image (such as those specified in a parent image or from the image's Dockerfile).
--
--
-- The following are notes about container health check support:
--
--     * Container health checks require version 1.17.0 or greater of the Amazon ECS container agent. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent> .
--
--     * Container health checks are supported for Fargate tasks if you are using platform version 1.1.0 or greater. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> .
--
--     * Container health checks are not supported for tasks that are part of a service that is configured to use a Classic Load Balancer.
--
--
--
--
-- /See:/ 'healthCheck' smart constructor.
data HealthCheck = HealthCheck'
  { _hcStartPeriod :: !(Maybe Int)
  , _hcRetries     :: !(Maybe Int)
  , _hcInterval    :: !(Maybe Int)
  , _hcTimeout     :: !(Maybe Int)
  , _hcCommand     :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HealthCheck' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hcStartPeriod' - The optional grace period within which to provide containers time to bootstrap before failed health checks count towards the maximum number of retries. You may specify between 0 and 300 seconds. The @startPeriod@ is disabled by default.
--
-- * 'hcRetries' - The number of times to retry a failed health check before the container is considered unhealthy. You may specify between 1 and 10 retries. The default value is 3.
--
-- * 'hcInterval' - The time period in seconds between each health check execution. You may specify between 5 and 300 seconds. The default value is 30 seconds.
--
-- * 'hcTimeout' - The time period in seconds to wait for a health check to succeed before it is considered a failure. You may specify between 2 and 60 seconds. The default value is 5.
--
-- * 'hcCommand' - A string array representing the command that the container runs to determine if it is healthy. The string array must start with @CMD@ to execute the command arguments directly, or @CMD-SHELL@ to run the command with the container's default shell. For example: @[ "CMD-SHELL", "curl -f http://localhost/ || exit 1" ]@  An exit code of 0 indicates success, and non-zero exit code indicates failure. For more information, see @HealthCheck@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> .
healthCheck
    :: HealthCheck
healthCheck =
  HealthCheck'
    { _hcStartPeriod = Nothing
    , _hcRetries = Nothing
    , _hcInterval = Nothing
    , _hcTimeout = Nothing
    , _hcCommand = mempty
    }


-- | The optional grace period within which to provide containers time to bootstrap before failed health checks count towards the maximum number of retries. You may specify between 0 and 300 seconds. The @startPeriod@ is disabled by default.
hcStartPeriod :: Lens' HealthCheck (Maybe Int)
hcStartPeriod = lens _hcStartPeriod (\ s a -> s{_hcStartPeriod = a})

-- | The number of times to retry a failed health check before the container is considered unhealthy. You may specify between 1 and 10 retries. The default value is 3.
hcRetries :: Lens' HealthCheck (Maybe Int)
hcRetries = lens _hcRetries (\ s a -> s{_hcRetries = a})

-- | The time period in seconds between each health check execution. You may specify between 5 and 300 seconds. The default value is 30 seconds.
hcInterval :: Lens' HealthCheck (Maybe Int)
hcInterval = lens _hcInterval (\ s a -> s{_hcInterval = a})

-- | The time period in seconds to wait for a health check to succeed before it is considered a failure. You may specify between 2 and 60 seconds. The default value is 5.
hcTimeout :: Lens' HealthCheck (Maybe Int)
hcTimeout = lens _hcTimeout (\ s a -> s{_hcTimeout = a})

-- | A string array representing the command that the container runs to determine if it is healthy. The string array must start with @CMD@ to execute the command arguments directly, or @CMD-SHELL@ to run the command with the container's default shell. For example: @[ "CMD-SHELL", "curl -f http://localhost/ || exit 1" ]@  An exit code of 0 indicates success, and non-zero exit code indicates failure. For more information, see @HealthCheck@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> .
hcCommand :: Lens' HealthCheck [Text]
hcCommand = lens _hcCommand (\ s a -> s{_hcCommand = a}) . _Coerce

instance FromJSON HealthCheck where
        parseJSON
          = withObject "HealthCheck"
              (\ x ->
                 HealthCheck' <$>
                   (x .:? "startPeriod") <*> (x .:? "retries") <*>
                     (x .:? "interval")
                     <*> (x .:? "timeout")
                     <*> (x .:? "command" .!= mempty))

instance Hashable HealthCheck where

instance NFData HealthCheck where

instance ToJSON HealthCheck where
        toJSON HealthCheck'{..}
          = object
              (catMaybes
                 [("startPeriod" .=) <$> _hcStartPeriod,
                  ("retries" .=) <$> _hcRetries,
                  ("interval" .=) <$> _hcInterval,
                  ("timeout" .=) <$> _hcTimeout,
                  Just ("command" .= _hcCommand)])

-- | Hostnames and IP address entries that are added to the @/etc/hosts@ file of a container via the @extraHosts@ parameter of its 'ContainerDefinition' .
--
--
--
-- /See:/ 'hostEntry' smart constructor.
data HostEntry = HostEntry'
  { _heHostname  :: !Text
  , _heIpAddress :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
  HostEntry' {_heHostname = pHostname_, _heIpAddress = pIpAddress_}


-- | The hostname to use in the @/etc/hosts@ entry.
heHostname :: Lens' HostEntry Text
heHostname = lens _heHostname (\ s a -> s{_heHostname = a})

-- | The IP address to use in the @/etc/hosts@ entry.
heIpAddress :: Lens' HostEntry Text
heIpAddress = lens _heIpAddress (\ s a -> s{_heIpAddress = a})

instance FromJSON HostEntry where
        parseJSON
          = withObject "HostEntry"
              (\ x ->
                 HostEntry' <$>
                   (x .: "hostname") <*> (x .: "ipAddress"))

instance Hashable HostEntry where

instance NFData HostEntry where

instance ToJSON HostEntry where
        toJSON HostEntry'{..}
          = object
              (catMaybes
                 [Just ("hostname" .= _heHostname),
                  Just ("ipAddress" .= _heIpAddress)])

-- | Details on a container instance bind mount host volume.
--
--
--
-- /See:/ 'hostVolumeProperties' smart constructor.
newtype HostVolumeProperties = HostVolumeProperties'
  { _hvpSourcePath :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HostVolumeProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hvpSourcePath' - When the @host@ parameter is used, specify a @sourcePath@ to declare the path on the host container instance that is presented to the container. If this parameter is empty, then the Docker daemon has assigned a host path for you. If the @host@ parameter contains a @sourcePath@ file location, then the data volume persists at the specified location on the host container instance until you delete it manually. If the @sourcePath@ value does not exist on the host container instance, the Docker daemon creates it. If the location does exist, the contents of the source path folder are exported. If you are using the Fargate launch type, the @sourcePath@ parameter is not supported.
hostVolumeProperties
    :: HostVolumeProperties
hostVolumeProperties = HostVolumeProperties' {_hvpSourcePath = Nothing}


-- | When the @host@ parameter is used, specify a @sourcePath@ to declare the path on the host container instance that is presented to the container. If this parameter is empty, then the Docker daemon has assigned a host path for you. If the @host@ parameter contains a @sourcePath@ file location, then the data volume persists at the specified location on the host container instance until you delete it manually. If the @sourcePath@ value does not exist on the host container instance, the Docker daemon creates it. If the location does exist, the contents of the source path folder are exported. If you are using the Fargate launch type, the @sourcePath@ parameter is not supported.
hvpSourcePath :: Lens' HostVolumeProperties (Maybe Text)
hvpSourcePath = lens _hvpSourcePath (\ s a -> s{_hvpSourcePath = a})

instance FromJSON HostVolumeProperties where
        parseJSON
          = withObject "HostVolumeProperties"
              (\ x ->
                 HostVolumeProperties' <$> (x .:? "sourcePath"))

instance Hashable HostVolumeProperties where

instance NFData HostVolumeProperties where

instance ToJSON HostVolumeProperties where
        toJSON HostVolumeProperties'{..}
          = object
              (catMaybes [("sourcePath" .=) <$> _hvpSourcePath])

-- | The Linux capabilities for the container that are added to or dropped from the default configuration provided by Docker. For more information on the default capabilities and the non-default available capabilities, see <https://docs.docker.com/engine/reference/run/#runtime-privilege-and-linux-capabilities Runtime privilege and Linux capabilities> in the /Docker run reference/ . For more detailed information on these Linux capabilities, see the <http://man7.org/linux/man-pages/man7/capabilities.7.html capabilities(7)> Linux manual page.
--
--
--
-- /See:/ 'kernelCapabilities' smart constructor.
data KernelCapabilities = KernelCapabilities'
  { _kcDrop :: !(Maybe [Text])
  , _kcAdd  :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KernelCapabilities' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kcDrop' - The Linux capabilities for the container that have been removed from the default configuration provided by Docker. This parameter maps to @CapDrop@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--cap-drop@ option to <https://docs.docker.com/engine/reference/run/ docker run> . Valid values: @"ALL" | "AUDIT_CONTROL" | "AUDIT_WRITE" | "BLOCK_SUSPEND" | "CHOWN" | "DAC_OVERRIDE" | "DAC_READ_SEARCH" | "FOWNER" | "FSETID" | "IPC_LOCK" | "IPC_OWNER" | "KILL" | "LEASE" | "LINUX_IMMUTABLE" | "MAC_ADMIN" | "MAC_OVERRIDE" | "MKNOD" | "NET_ADMIN" | "NET_BIND_SERVICE" | "NET_BROADCAST" | "NET_RAW" | "SETFCAP" | "SETGID" | "SETPCAP" | "SETUID" | "SYS_ADMIN" | "SYS_BOOT" | "SYS_CHROOT" | "SYS_MODULE" | "SYS_NICE" | "SYS_PACCT" | "SYS_PTRACE" | "SYS_RAWIO" | "SYS_RESOURCE" | "SYS_TIME" | "SYS_TTY_CONFIG" | "SYSLOG" | "WAKE_ALARM"@
--
-- * 'kcAdd' - The Linux capabilities for the container that have been added to the default configuration provided by Docker. This parameter maps to @CapAdd@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--cap-add@ option to <https://docs.docker.com/engine/reference/run/ docker run> . Valid values: @"ALL" | "AUDIT_CONTROL" | "AUDIT_WRITE" | "BLOCK_SUSPEND" | "CHOWN" | "DAC_OVERRIDE" | "DAC_READ_SEARCH" | "FOWNER" | "FSETID" | "IPC_LOCK" | "IPC_OWNER" | "KILL" | "LEASE" | "LINUX_IMMUTABLE" | "MAC_ADMIN" | "MAC_OVERRIDE" | "MKNOD" | "NET_ADMIN" | "NET_BIND_SERVICE" | "NET_BROADCAST" | "NET_RAW" | "SETFCAP" | "SETGID" | "SETPCAP" | "SETUID" | "SYS_ADMIN" | "SYS_BOOT" | "SYS_CHROOT" | "SYS_MODULE" | "SYS_NICE" | "SYS_PACCT" | "SYS_PTRACE" | "SYS_RAWIO" | "SYS_RESOURCE" | "SYS_TIME" | "SYS_TTY_CONFIG" | "SYSLOG" | "WAKE_ALARM"@
kernelCapabilities
    :: KernelCapabilities
kernelCapabilities = KernelCapabilities' {_kcDrop = Nothing, _kcAdd = Nothing}


-- | The Linux capabilities for the container that have been removed from the default configuration provided by Docker. This parameter maps to @CapDrop@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--cap-drop@ option to <https://docs.docker.com/engine/reference/run/ docker run> . Valid values: @"ALL" | "AUDIT_CONTROL" | "AUDIT_WRITE" | "BLOCK_SUSPEND" | "CHOWN" | "DAC_OVERRIDE" | "DAC_READ_SEARCH" | "FOWNER" | "FSETID" | "IPC_LOCK" | "IPC_OWNER" | "KILL" | "LEASE" | "LINUX_IMMUTABLE" | "MAC_ADMIN" | "MAC_OVERRIDE" | "MKNOD" | "NET_ADMIN" | "NET_BIND_SERVICE" | "NET_BROADCAST" | "NET_RAW" | "SETFCAP" | "SETGID" | "SETPCAP" | "SETUID" | "SYS_ADMIN" | "SYS_BOOT" | "SYS_CHROOT" | "SYS_MODULE" | "SYS_NICE" | "SYS_PACCT" | "SYS_PTRACE" | "SYS_RAWIO" | "SYS_RESOURCE" | "SYS_TIME" | "SYS_TTY_CONFIG" | "SYSLOG" | "WAKE_ALARM"@
kcDrop :: Lens' KernelCapabilities [Text]
kcDrop = lens _kcDrop (\ s a -> s{_kcDrop = a}) . _Default . _Coerce

-- | The Linux capabilities for the container that have been added to the default configuration provided by Docker. This parameter maps to @CapAdd@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--cap-add@ option to <https://docs.docker.com/engine/reference/run/ docker run> . Valid values: @"ALL" | "AUDIT_CONTROL" | "AUDIT_WRITE" | "BLOCK_SUSPEND" | "CHOWN" | "DAC_OVERRIDE" | "DAC_READ_SEARCH" | "FOWNER" | "FSETID" | "IPC_LOCK" | "IPC_OWNER" | "KILL" | "LEASE" | "LINUX_IMMUTABLE" | "MAC_ADMIN" | "MAC_OVERRIDE" | "MKNOD" | "NET_ADMIN" | "NET_BIND_SERVICE" | "NET_BROADCAST" | "NET_RAW" | "SETFCAP" | "SETGID" | "SETPCAP" | "SETUID" | "SYS_ADMIN" | "SYS_BOOT" | "SYS_CHROOT" | "SYS_MODULE" | "SYS_NICE" | "SYS_PACCT" | "SYS_PTRACE" | "SYS_RAWIO" | "SYS_RESOURCE" | "SYS_TIME" | "SYS_TTY_CONFIG" | "SYSLOG" | "WAKE_ALARM"@
kcAdd :: Lens' KernelCapabilities [Text]
kcAdd = lens _kcAdd (\ s a -> s{_kcAdd = a}) . _Default . _Coerce

instance FromJSON KernelCapabilities where
        parseJSON
          = withObject "KernelCapabilities"
              (\ x ->
                 KernelCapabilities' <$>
                   (x .:? "drop" .!= mempty) <*>
                     (x .:? "add" .!= mempty))

instance Hashable KernelCapabilities where

instance NFData KernelCapabilities where

instance ToJSON KernelCapabilities where
        toJSON KernelCapabilities'{..}
          = object
              (catMaybes
                 [("drop" .=) <$> _kcDrop, ("add" .=) <$> _kcAdd])

-- | A key-value pair object.
--
--
--
-- /See:/ 'keyValuePair' smart constructor.
data KeyValuePair = KeyValuePair'
  { _kvpValue :: !(Maybe Text)
  , _kvpName  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KeyValuePair' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kvpValue' - The value of the key-value pair. For environment variables, this is the value of the environment variable.
--
-- * 'kvpName' - The name of the key-value pair. For environment variables, this is the name of the environment variable.
keyValuePair
    :: KeyValuePair
keyValuePair = KeyValuePair' {_kvpValue = Nothing, _kvpName = Nothing}


-- | The value of the key-value pair. For environment variables, this is the value of the environment variable.
kvpValue :: Lens' KeyValuePair (Maybe Text)
kvpValue = lens _kvpValue (\ s a -> s{_kvpValue = a})

-- | The name of the key-value pair. For environment variables, this is the name of the environment variable.
kvpName :: Lens' KeyValuePair (Maybe Text)
kvpName = lens _kvpName (\ s a -> s{_kvpName = a})

instance FromJSON KeyValuePair where
        parseJSON
          = withObject "KeyValuePair"
              (\ x ->
                 KeyValuePair' <$> (x .:? "value") <*> (x .:? "name"))

instance Hashable KeyValuePair where

instance NFData KeyValuePair where

instance ToJSON KeyValuePair where
        toJSON KeyValuePair'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _kvpValue,
                  ("name" .=) <$> _kvpName])

-- | Linux-specific options that are applied to the container, such as Linux 'KernelCapabilities' .
--
--
--
-- /See:/ 'linuxParameters' smart constructor.
data LinuxParameters = LinuxParameters'
  { _lpSharedMemorySize   :: !(Maybe Int)
  , _lpInitProcessEnabled :: !(Maybe Bool)
  , _lpTmpfs              :: !(Maybe [Tmpfs])
  , _lpDevices            :: !(Maybe [Device])
  , _lpCapabilities       :: !(Maybe KernelCapabilities)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LinuxParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpSharedMemorySize' - The value for the size (in MiB) of the @/dev/shm@ volume. This parameter maps to the @--shm-size@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'lpInitProcessEnabled' - Run an @init@ process inside the container that forwards signals and reaps processes. This parameter maps to the @--init@ option to <https://docs.docker.com/engine/reference/run/ docker run> . This parameter requires version 1.25 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@
--
-- * 'lpTmpfs' - The container path, mount options, and size (in MiB) of the tmpfs mount. This parameter maps to the @--tmpfs@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'lpDevices' - Any host devices to expose to the container. This parameter maps to @Devices@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--device@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'lpCapabilities' - The Linux capabilities for the container that are added to or dropped from the default configuration provided by Docker.
linuxParameters
    :: LinuxParameters
linuxParameters =
  LinuxParameters'
    { _lpSharedMemorySize = Nothing
    , _lpInitProcessEnabled = Nothing
    , _lpTmpfs = Nothing
    , _lpDevices = Nothing
    , _lpCapabilities = Nothing
    }


-- | The value for the size (in MiB) of the @/dev/shm@ volume. This parameter maps to the @--shm-size@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
lpSharedMemorySize :: Lens' LinuxParameters (Maybe Int)
lpSharedMemorySize = lens _lpSharedMemorySize (\ s a -> s{_lpSharedMemorySize = a})

-- | Run an @init@ process inside the container that forwards signals and reaps processes. This parameter maps to the @--init@ option to <https://docs.docker.com/engine/reference/run/ docker run> . This parameter requires version 1.25 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@
lpInitProcessEnabled :: Lens' LinuxParameters (Maybe Bool)
lpInitProcessEnabled = lens _lpInitProcessEnabled (\ s a -> s{_lpInitProcessEnabled = a})

-- | The container path, mount options, and size (in MiB) of the tmpfs mount. This parameter maps to the @--tmpfs@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
lpTmpfs :: Lens' LinuxParameters [Tmpfs]
lpTmpfs = lens _lpTmpfs (\ s a -> s{_lpTmpfs = a}) . _Default . _Coerce

-- | Any host devices to expose to the container. This parameter maps to @Devices@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--device@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
lpDevices :: Lens' LinuxParameters [Device]
lpDevices = lens _lpDevices (\ s a -> s{_lpDevices = a}) . _Default . _Coerce

-- | The Linux capabilities for the container that are added to or dropped from the default configuration provided by Docker.
lpCapabilities :: Lens' LinuxParameters (Maybe KernelCapabilities)
lpCapabilities = lens _lpCapabilities (\ s a -> s{_lpCapabilities = a})

instance FromJSON LinuxParameters where
        parseJSON
          = withObject "LinuxParameters"
              (\ x ->
                 LinuxParameters' <$>
                   (x .:? "sharedMemorySize") <*>
                     (x .:? "initProcessEnabled")
                     <*> (x .:? "tmpfs" .!= mempty)
                     <*> (x .:? "devices" .!= mempty)
                     <*> (x .:? "capabilities"))

instance Hashable LinuxParameters where

instance NFData LinuxParameters where

instance ToJSON LinuxParameters where
        toJSON LinuxParameters'{..}
          = object
              (catMaybes
                 [("sharedMemorySize" .=) <$> _lpSharedMemorySize,
                  ("initProcessEnabled" .=) <$> _lpInitProcessEnabled,
                  ("tmpfs" .=) <$> _lpTmpfs,
                  ("devices" .=) <$> _lpDevices,
                  ("capabilities" .=) <$> _lpCapabilities])

-- | Details on a load balancer that is used with a service.
--
--
-- If the service is using the @ECS@ deployment controller, you are limited to one load balancer or target group.
--
-- If the service is using the @CODE_DEPLOY@ deployment controller, the service is required to use either an Application Load Balancer or Network Load Balancer. When you are creating an AWS CodeDeploy deployment group, you specify two target groups (referred to as a @targetGroupPair@ ). Each target group binds to a separate task set in the deployment. The load balancer can also have up to two listeners, a required listener for production traffic and an optional listener that allows you to test new revisions of the service before routing production traffic to it.
--
-- Services with tasks that use the @awsvpc@ network mode (for example, those with the Fargate launch type) only support Application Load Balancers and Network Load Balancers. Classic Load Balancers are not supported. Also, when you create any target groups for these services, you must choose @ip@ as the target type, not @instance@ . Tasks that use the @awsvpc@ network mode are associated with an elastic network interface, not an Amazon EC2 instance.
--
--
-- /See:/ 'loadBalancer' smart constructor.
data LoadBalancer = LoadBalancer'
  { _lbLoadBalancerName :: !(Maybe Text)
  , _lbContainerName    :: !(Maybe Text)
  , _lbTargetGroupARN   :: !(Maybe Text)
  , _lbContainerPort    :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoadBalancer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbLoadBalancerName' - The name of a load balancer.
--
-- * 'lbContainerName' - The name of the container (as it appears in a container definition) to associate with the load balancer.
--
-- * 'lbTargetGroupARN' - The full Amazon Resource Name (ARN) of the Elastic Load Balancing target group or groups associated with a service. For services using the @ECS@ deployment controller, you are limited to one target group. For services using the @CODE_DEPLOY@ deployment controller, you are required to define two target groups for the load balancer. /Important:/ If your service's task definition uses the @awsvpc@ network mode (which is required for the Fargate launch type), you must choose @ip@ as the target type, not @instance@ , because tasks that use the @awsvpc@ network mode are associated with an elastic network interface, not an Amazon EC2 instance.
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


-- | The name of a load balancer.
lbLoadBalancerName :: Lens' LoadBalancer (Maybe Text)
lbLoadBalancerName = lens _lbLoadBalancerName (\ s a -> s{_lbLoadBalancerName = a})

-- | The name of the container (as it appears in a container definition) to associate with the load balancer.
lbContainerName :: Lens' LoadBalancer (Maybe Text)
lbContainerName = lens _lbContainerName (\ s a -> s{_lbContainerName = a})

-- | The full Amazon Resource Name (ARN) of the Elastic Load Balancing target group or groups associated with a service. For services using the @ECS@ deployment controller, you are limited to one target group. For services using the @CODE_DEPLOY@ deployment controller, you are required to define two target groups for the load balancer. /Important:/ If your service's task definition uses the @awsvpc@ network mode (which is required for the Fargate launch type), you must choose @ip@ as the target type, not @instance@ , because tasks that use the @awsvpc@ network mode are associated with an elastic network interface, not an Amazon EC2 instance.
lbTargetGroupARN :: Lens' LoadBalancer (Maybe Text)
lbTargetGroupARN = lens _lbTargetGroupARN (\ s a -> s{_lbTargetGroupARN = a})

-- | The port on the container to associate with the load balancer. This port must correspond to a @containerPort@ in the service's task definition. Your container instances must allow ingress traffic on the @hostPort@ of the port mapping.
lbContainerPort :: Lens' LoadBalancer (Maybe Int)
lbContainerPort = lens _lbContainerPort (\ s a -> s{_lbContainerPort = a})

instance FromJSON LoadBalancer where
        parseJSON
          = withObject "LoadBalancer"
              (\ x ->
                 LoadBalancer' <$>
                   (x .:? "loadBalancerName") <*>
                     (x .:? "containerName")
                     <*> (x .:? "targetGroupArn")
                     <*> (x .:? "containerPort"))

instance Hashable LoadBalancer where

instance NFData LoadBalancer where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LogConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcOptions' - The configuration options to send to the log driver. This parameter requires version 1.19 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@
--
-- * 'lcLogDriver' - The log driver to use for the container. The valid values listed for this parameter are log drivers that the Amazon ECS container agent can communicate with by default. If you are using the Fargate launch type, the only supported value is @awslogs@ . For more information about using the @awslogs@ driver, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_awslogs.html Using the awslogs Log Driver> in the /Amazon Elastic Container Service Developer Guide/ . This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@
logConfiguration
    :: LogDriver -- ^ 'lcLogDriver'
    -> LogConfiguration
logConfiguration pLogDriver_ =
  LogConfiguration' {_lcOptions = Nothing, _lcLogDriver = pLogDriver_}


-- | The configuration options to send to the log driver. This parameter requires version 1.19 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@
lcOptions :: Lens' LogConfiguration (HashMap Text Text)
lcOptions = lens _lcOptions (\ s a -> s{_lcOptions = a}) . _Default . _Map

-- | The log driver to use for the container. The valid values listed for this parameter are log drivers that the Amazon ECS container agent can communicate with by default. If you are using the Fargate launch type, the only supported value is @awslogs@ . For more information about using the @awslogs@ driver, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_awslogs.html Using the awslogs Log Driver> in the /Amazon Elastic Container Service Developer Guide/ . This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@
lcLogDriver :: Lens' LogConfiguration LogDriver
lcLogDriver = lens _lcLogDriver (\ s a -> s{_lcLogDriver = a})

instance FromJSON LogConfiguration where
        parseJSON
          = withObject "LogConfiguration"
              (\ x ->
                 LogConfiguration' <$>
                   (x .:? "options" .!= mempty) <*> (x .: "logDriver"))

instance Hashable LogConfiguration where

instance NFData LogConfiguration where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MountPoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpContainerPath' - The path on the container to mount the host volume at.
--
-- * 'mpSourceVolume' - The name of the volume to mount. Must be a volume name referenced in the @name@ parameter of task definition @volume@ .
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
mpContainerPath = lens _mpContainerPath (\ s a -> s{_mpContainerPath = a})

-- | The name of the volume to mount. Must be a volume name referenced in the @name@ parameter of task definition @volume@ .
mpSourceVolume :: Lens' MountPoint (Maybe Text)
mpSourceVolume = lens _mpSourceVolume (\ s a -> s{_mpSourceVolume = a})

-- | If this value is @true@ , the container has read-only access to the volume. If this value is @false@ , then the container can write to the volume. The default value is @false@ .
mpReadOnly :: Lens' MountPoint (Maybe Bool)
mpReadOnly = lens _mpReadOnly (\ s a -> s{_mpReadOnly = a})

instance FromJSON MountPoint where
        parseJSON
          = withObject "MountPoint"
              (\ x ->
                 MountPoint' <$>
                   (x .:? "containerPath") <*> (x .:? "sourceVolume")
                     <*> (x .:? "readOnly"))

instance Hashable MountPoint where

instance NFData MountPoint where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
-- * 'nbContainerPort' - The port number on the container that is used with the network binding.
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
nbBindIP = lens _nbBindIP (\ s a -> s{_nbBindIP = a})

-- | The protocol used for the network binding.
nbProtocol :: Lens' NetworkBinding (Maybe TransportProtocol)
nbProtocol = lens _nbProtocol (\ s a -> s{_nbProtocol = a})

-- | The port number on the host that is used with the network binding.
nbHostPort :: Lens' NetworkBinding (Maybe Int)
nbHostPort = lens _nbHostPort (\ s a -> s{_nbHostPort = a})

-- | The port number on the container that is used with the network binding.
nbContainerPort :: Lens' NetworkBinding (Maybe Int)
nbContainerPort = lens _nbContainerPort (\ s a -> s{_nbContainerPort = a})

instance FromJSON NetworkBinding where
        parseJSON
          = withObject "NetworkBinding"
              (\ x ->
                 NetworkBinding' <$>
                   (x .:? "bindIP") <*> (x .:? "protocol") <*>
                     (x .:? "hostPort")
                     <*> (x .:? "containerPort"))

instance Hashable NetworkBinding where

instance NFData NetworkBinding where

instance ToJSON NetworkBinding where
        toJSON NetworkBinding'{..}
          = object
              (catMaybes
                 [("bindIP" .=) <$> _nbBindIP,
                  ("protocol" .=) <$> _nbProtocol,
                  ("hostPort" .=) <$> _nbHostPort,
                  ("containerPort" .=) <$> _nbContainerPort])

-- | An object representing the network configuration for a task or service.
--
--
--
-- /See:/ 'networkConfiguration' smart constructor.
newtype NetworkConfiguration = NetworkConfiguration'
  { _ncAwsvpcConfiguration :: Maybe AWSVPCConfiguration
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NetworkConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncAwsvpcConfiguration' - The VPC subnets and security groups associated with a task.
networkConfiguration
    :: NetworkConfiguration
networkConfiguration = NetworkConfiguration' {_ncAwsvpcConfiguration = Nothing}


-- | The VPC subnets and security groups associated with a task.
ncAwsvpcConfiguration :: Lens' NetworkConfiguration (Maybe AWSVPCConfiguration)
ncAwsvpcConfiguration = lens _ncAwsvpcConfiguration (\ s a -> s{_ncAwsvpcConfiguration = a})

instance FromJSON NetworkConfiguration where
        parseJSON
          = withObject "NetworkConfiguration"
              (\ x ->
                 NetworkConfiguration' <$>
                   (x .:? "awsvpcConfiguration"))

instance Hashable NetworkConfiguration where

instance NFData NetworkConfiguration where

instance ToJSON NetworkConfiguration where
        toJSON NetworkConfiguration'{..}
          = object
              (catMaybes
                 [("awsvpcConfiguration" .=) <$>
                    _ncAwsvpcConfiguration])

-- | An object representing the elastic network interface for tasks that use the @awsvpc@ network mode.
--
--
--
-- /See:/ 'networkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { _niIpv6Address        :: !(Maybe Text)
  , _niPrivateIPv4Address :: !(Maybe Text)
  , _niAttachmentId       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NetworkInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'niIpv6Address' - The private IPv6 address for the network interface.
--
-- * 'niPrivateIPv4Address' - The private IPv4 address for the network interface.
--
-- * 'niAttachmentId' - The attachment ID for the network interface.
networkInterface
    :: NetworkInterface
networkInterface =
  NetworkInterface'
    { _niIpv6Address = Nothing
    , _niPrivateIPv4Address = Nothing
    , _niAttachmentId = Nothing
    }


-- | The private IPv6 address for the network interface.
niIpv6Address :: Lens' NetworkInterface (Maybe Text)
niIpv6Address = lens _niIpv6Address (\ s a -> s{_niIpv6Address = a})

-- | The private IPv4 address for the network interface.
niPrivateIPv4Address :: Lens' NetworkInterface (Maybe Text)
niPrivateIPv4Address = lens _niPrivateIPv4Address (\ s a -> s{_niPrivateIPv4Address = a})

-- | The attachment ID for the network interface.
niAttachmentId :: Lens' NetworkInterface (Maybe Text)
niAttachmentId = lens _niAttachmentId (\ s a -> s{_niAttachmentId = a})

instance FromJSON NetworkInterface where
        parseJSON
          = withObject "NetworkInterface"
              (\ x ->
                 NetworkInterface' <$>
                   (x .:? "ipv6Address") <*>
                     (x .:? "privateIpv4Address")
                     <*> (x .:? "attachmentId"))

instance Hashable NetworkInterface where

instance NFData NetworkInterface where

-- | An object representing a constraint on task placement. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html Task Placement Constraints> in the /Amazon Elastic Container Service Developer Guide/ .
--
--
--
-- /See:/ 'placementConstraint' smart constructor.
data PlacementConstraint = PlacementConstraint'
  { _pcExpression :: !(Maybe Text)
  , _pcType       :: !(Maybe PlacementConstraintType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PlacementConstraint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcExpression' - A cluster query language expression to apply to the constraint. You cannot specify an expression if the constraint type is @distinctInstance@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'pcType' - The type of constraint. Use @distinctInstance@ to ensure that each task in a particular group is running on a different container instance. Use @memberOf@ to restrict the selection to a group of valid candidates. The value @distinctInstance@ is not supported in task definitions.
placementConstraint
    :: PlacementConstraint
placementConstraint =
  PlacementConstraint' {_pcExpression = Nothing, _pcType = Nothing}


-- | A cluster query language expression to apply to the constraint. You cannot specify an expression if the constraint type is @distinctInstance@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language> in the /Amazon Elastic Container Service Developer Guide/ .
pcExpression :: Lens' PlacementConstraint (Maybe Text)
pcExpression = lens _pcExpression (\ s a -> s{_pcExpression = a})

-- | The type of constraint. Use @distinctInstance@ to ensure that each task in a particular group is running on a different container instance. Use @memberOf@ to restrict the selection to a group of valid candidates. The value @distinctInstance@ is not supported in task definitions.
pcType :: Lens' PlacementConstraint (Maybe PlacementConstraintType)
pcType = lens _pcType (\ s a -> s{_pcType = a})

instance FromJSON PlacementConstraint where
        parseJSON
          = withObject "PlacementConstraint"
              (\ x ->
                 PlacementConstraint' <$>
                   (x .:? "expression") <*> (x .:? "type"))

instance Hashable PlacementConstraint where

instance NFData PlacementConstraint where

instance ToJSON PlacementConstraint where
        toJSON PlacementConstraint'{..}
          = object
              (catMaybes
                 [("expression" .=) <$> _pcExpression,
                  ("type" .=) <$> _pcType])

-- | The task placement strategy for a task or service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-strategies.html Task Placement Strategies> in the /Amazon Elastic Container Service Developer Guide/ .
--
--
--
-- /See:/ 'placementStrategy' smart constructor.
data PlacementStrategy = PlacementStrategy'
  { _psField :: !(Maybe Text)
  , _psType  :: !(Maybe PlacementStrategyType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PlacementStrategy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psField' - The field to apply the placement strategy against. For the @spread@ placement strategy, valid values are @instanceId@ (or @host@ , which has the same effect), or any platform or custom attribute that is applied to a container instance, such as @attribute:ecs.availability-zone@ . For the @binpack@ placement strategy, valid values are @cpu@ and @memory@ . For the @random@ placement strategy, this field is not used.
--
-- * 'psType' - The type of placement strategy. The @random@ placement strategy randomly places tasks on available candidates. The @spread@ placement strategy spreads placement across available candidates evenly based on the @field@ parameter. The @binpack@ strategy places tasks on available candidates that have the least available amount of the resource that is specified with the @field@ parameter. For example, if you binpack on memory, a task is placed on the instance with the least amount of remaining memory (but still enough to run the task).
placementStrategy
    :: PlacementStrategy
placementStrategy = PlacementStrategy' {_psField = Nothing, _psType = Nothing}


-- | The field to apply the placement strategy against. For the @spread@ placement strategy, valid values are @instanceId@ (or @host@ , which has the same effect), or any platform or custom attribute that is applied to a container instance, such as @attribute:ecs.availability-zone@ . For the @binpack@ placement strategy, valid values are @cpu@ and @memory@ . For the @random@ placement strategy, this field is not used.
psField :: Lens' PlacementStrategy (Maybe Text)
psField = lens _psField (\ s a -> s{_psField = a})

-- | The type of placement strategy. The @random@ placement strategy randomly places tasks on available candidates. The @spread@ placement strategy spreads placement across available candidates evenly based on the @field@ parameter. The @binpack@ strategy places tasks on available candidates that have the least available amount of the resource that is specified with the @field@ parameter. For example, if you binpack on memory, a task is placed on the instance with the least amount of remaining memory (but still enough to run the task).
psType :: Lens' PlacementStrategy (Maybe PlacementStrategyType)
psType = lens _psType (\ s a -> s{_psType = a})

instance FromJSON PlacementStrategy where
        parseJSON
          = withObject "PlacementStrategy"
              (\ x ->
                 PlacementStrategy' <$>
                   (x .:? "field") <*> (x .:? "type"))

instance Hashable PlacementStrategy where

instance NFData PlacementStrategy where

instance ToJSON PlacementStrategy where
        toJSON PlacementStrategy'{..}
          = object
              (catMaybes
                 [("field" .=) <$> _psField, ("type" .=) <$> _psType])

-- | The devices that are available on the container instance. The only supported device type is a GPU.
--
--
--
-- /See:/ 'platformDevice' smart constructor.
data PlatformDevice = PlatformDevice'
  { _pdId   :: !Text
  , _pdType :: !PlatformDeviceType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PlatformDevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdId' - The ID for the GPU(s) on the container instance. The available GPU IDs can also be obtained on the container instance in the @/var/lib/ecs/gpu/nvidia_gpu_info.json@ file.
--
-- * 'pdType' - The type of device that is available on the container instance. The only supported value is @GPU@ .
platformDevice
    :: Text -- ^ 'pdId'
    -> PlatformDeviceType -- ^ 'pdType'
    -> PlatformDevice
platformDevice pId_ pType_ = PlatformDevice' {_pdId = pId_, _pdType = pType_}


-- | The ID for the GPU(s) on the container instance. The available GPU IDs can also be obtained on the container instance in the @/var/lib/ecs/gpu/nvidia_gpu_info.json@ file.
pdId :: Lens' PlatformDevice Text
pdId = lens _pdId (\ s a -> s{_pdId = a})

-- | The type of device that is available on the container instance. The only supported value is @GPU@ .
pdType :: Lens' PlatformDevice PlatformDeviceType
pdType = lens _pdType (\ s a -> s{_pdType = a})

instance Hashable PlatformDevice where

instance NFData PlatformDevice where

instance ToJSON PlatformDevice where
        toJSON PlatformDevice'{..}
          = object
              (catMaybes
                 [Just ("id" .= _pdId), Just ("type" .= _pdType)])

-- | Port mappings allow containers to access ports on the host container instance to send or receive traffic. Port mappings are specified as part of the container definition.
--
--
-- If you are using containers in a task with the @awsvpc@ or @host@ network mode, exposed ports should be specified using @containerPort@ . The @hostPort@ can be left blank or it must be the same value as the @containerPort@ .
--
-- After a task reaches the @RUNNING@ status, manual and automatic host and container port assignments are visible in the @networkBindings@ section of 'DescribeTasks' API responses.
--
--
-- /See:/ 'portMapping' smart constructor.
data PortMapping = PortMapping'
  { _pmProtocol      :: !(Maybe TransportProtocol)
  , _pmHostPort      :: !(Maybe Int)
  , _pmContainerPort :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PortMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmProtocol' - The protocol used for the port mapping. Valid values are @tcp@ and @udp@ . The default is @tcp@ .
--
-- * 'pmHostPort' - The port number on the container instance to reserve for your container. If you are using containers in a task with the @awsvpc@ or @host@ network mode, the @hostPort@ can either be left blank or set to the same value as the @containerPort@ . If you are using containers in a task with the @bridge@ network mode, you can specify a non-reserved host port for your container port mapping, or you can omit the @hostPort@ (or set it to @0@ ) while specifying a @containerPort@ and your container automatically receives a port in the ephemeral port range for your container instance operating system and Docker version. The default ephemeral port range for Docker version 1.6.0 and later is listed on the instance under @/proc/sys/net/ipv4/ip_local_port_range@ . If this kernel parameter is unavailable, the default ephemeral port range from 49153 through 65535 is used. Do not attempt to specify a host port in the ephemeral port range as these are reserved for automatic assignment. In general, ports below 32768 are outside of the ephemeral port range. The default reserved ports are 22 for SSH, the Docker ports 2375 and 2376, and the Amazon ECS container agent ports 51678-51680. Any host port that was previously specified in a running task is also reserved while the task is running (after a task stops, the host port is released). The current reserved ports are displayed in the @remainingResources@ of 'DescribeContainerInstances' output. A container instance can have up to 100 reserved ports at a time, including the default reserved ports. Automatically assigned ports don't count toward the 100 reserved ports limit.
--
-- * 'pmContainerPort' - The port number on the container that is bound to the user-specified or automatically assigned host port. If you are using containers in a task with the @awsvpc@ or @host@ network mode, exposed ports should be specified using @containerPort@ . If you are using containers in a task with the @bridge@ network mode and you specify a container port and not a host port, your container automatically receives a host port in the ephemeral port range. For more information, see @hostPort@ . Port mappings that are automatically assigned in this way do not count toward the 100 reserved ports limit of a container instance.
portMapping
    :: PortMapping
portMapping =
  PortMapping'
    {_pmProtocol = Nothing, _pmHostPort = Nothing, _pmContainerPort = Nothing}


-- | The protocol used for the port mapping. Valid values are @tcp@ and @udp@ . The default is @tcp@ .
pmProtocol :: Lens' PortMapping (Maybe TransportProtocol)
pmProtocol = lens _pmProtocol (\ s a -> s{_pmProtocol = a})

-- | The port number on the container instance to reserve for your container. If you are using containers in a task with the @awsvpc@ or @host@ network mode, the @hostPort@ can either be left blank or set to the same value as the @containerPort@ . If you are using containers in a task with the @bridge@ network mode, you can specify a non-reserved host port for your container port mapping, or you can omit the @hostPort@ (or set it to @0@ ) while specifying a @containerPort@ and your container automatically receives a port in the ephemeral port range for your container instance operating system and Docker version. The default ephemeral port range for Docker version 1.6.0 and later is listed on the instance under @/proc/sys/net/ipv4/ip_local_port_range@ . If this kernel parameter is unavailable, the default ephemeral port range from 49153 through 65535 is used. Do not attempt to specify a host port in the ephemeral port range as these are reserved for automatic assignment. In general, ports below 32768 are outside of the ephemeral port range. The default reserved ports are 22 for SSH, the Docker ports 2375 and 2376, and the Amazon ECS container agent ports 51678-51680. Any host port that was previously specified in a running task is also reserved while the task is running (after a task stops, the host port is released). The current reserved ports are displayed in the @remainingResources@ of 'DescribeContainerInstances' output. A container instance can have up to 100 reserved ports at a time, including the default reserved ports. Automatically assigned ports don't count toward the 100 reserved ports limit.
pmHostPort :: Lens' PortMapping (Maybe Int)
pmHostPort = lens _pmHostPort (\ s a -> s{_pmHostPort = a})

-- | The port number on the container that is bound to the user-specified or automatically assigned host port. If you are using containers in a task with the @awsvpc@ or @host@ network mode, exposed ports should be specified using @containerPort@ . If you are using containers in a task with the @bridge@ network mode and you specify a container port and not a host port, your container automatically receives a host port in the ephemeral port range. For more information, see @hostPort@ . Port mappings that are automatically assigned in this way do not count toward the 100 reserved ports limit of a container instance.
pmContainerPort :: Lens' PortMapping (Maybe Int)
pmContainerPort = lens _pmContainerPort (\ s a -> s{_pmContainerPort = a})

instance FromJSON PortMapping where
        parseJSON
          = withObject "PortMapping"
              (\ x ->
                 PortMapping' <$>
                   (x .:? "protocol") <*> (x .:? "hostPort") <*>
                     (x .:? "containerPort"))

instance Hashable PortMapping where

instance NFData PortMapping where

instance ToJSON PortMapping where
        toJSON PortMapping'{..}
          = object
              (catMaybes
                 [("protocol" .=) <$> _pmProtocol,
                  ("hostPort" .=) <$> _pmHostPort,
                  ("containerPort" .=) <$> _pmContainerPort])

-- | The configuration details for the App Mesh proxy.
--
--
--
-- /See:/ 'proxyConfiguration' smart constructor.
data ProxyConfiguration = ProxyConfiguration'
  { _pType          :: !(Maybe ProxyConfigurationType)
  , _pProperties    :: !(Maybe [KeyValuePair])
  , _pContainerName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProxyConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pType' - The proxy type. The only supported value is @APPMESH@ .
--
-- * 'pProperties' - The set of network configuration parameters to provide the Container Network Interface (CNI) plugin, specified as key-value pairs.     * @IgnoredUID@ - (Required) The user ID (UID) of the proxy container as defined by the @user@ parameter in a container definition. This is used to ensure the proxy ignores its own traffic. If @IgnoredGID@ is specified, this field can be empty.     * @IgnoredGID@ - (Required) The group ID (GID) of the proxy container as defined by the @user@ parameter in a container definition. This is used to ensure the proxy ignores its own traffic. If @IgnoredGID@ is specified, this field can be empty.     * @AppPorts@ - (Required) The list of ports that the application uses. Network traffic to these ports will be forwarded to the @ProxyIngressPort@ and @ProxyEgressPort@ .     * @ProxyIngressPort@ - (Required) Specifies the port that incoming traffic to the @AppPorts@ is directed to.     * @ProxyEgressPort@ - (Required) Specifies the port that outgoing traffic from the @AppPorts@ is directed to.     * @EgressIgnoredPorts@ - (Required) The egress traffic going to these specified ports will be ignored and not redirected to the @ProxyEgressPort@ . It can be empty list.     * @EgressIgnoredIPs@ - (Required) The egress traffic going to these specified IP addresses will be ignored and not redirected to the @ProxyEgressPort@ . It can be empty list.
--
-- * 'pContainerName' - The name of the container that will serve as the App Mesh proxy.
proxyConfiguration
    :: Text -- ^ 'pContainerName'
    -> ProxyConfiguration
proxyConfiguration pContainerName_ =
  ProxyConfiguration'
    { _pType = Nothing
    , _pProperties = Nothing
    , _pContainerName = pContainerName_
    }


-- | The proxy type. The only supported value is @APPMESH@ .
pType :: Lens' ProxyConfiguration (Maybe ProxyConfigurationType)
pType = lens _pType (\ s a -> s{_pType = a})

-- | The set of network configuration parameters to provide the Container Network Interface (CNI) plugin, specified as key-value pairs.     * @IgnoredUID@ - (Required) The user ID (UID) of the proxy container as defined by the @user@ parameter in a container definition. This is used to ensure the proxy ignores its own traffic. If @IgnoredGID@ is specified, this field can be empty.     * @IgnoredGID@ - (Required) The group ID (GID) of the proxy container as defined by the @user@ parameter in a container definition. This is used to ensure the proxy ignores its own traffic. If @IgnoredGID@ is specified, this field can be empty.     * @AppPorts@ - (Required) The list of ports that the application uses. Network traffic to these ports will be forwarded to the @ProxyIngressPort@ and @ProxyEgressPort@ .     * @ProxyIngressPort@ - (Required) Specifies the port that incoming traffic to the @AppPorts@ is directed to.     * @ProxyEgressPort@ - (Required) Specifies the port that outgoing traffic from the @AppPorts@ is directed to.     * @EgressIgnoredPorts@ - (Required) The egress traffic going to these specified ports will be ignored and not redirected to the @ProxyEgressPort@ . It can be empty list.     * @EgressIgnoredIPs@ - (Required) The egress traffic going to these specified IP addresses will be ignored and not redirected to the @ProxyEgressPort@ . It can be empty list.
pProperties :: Lens' ProxyConfiguration [KeyValuePair]
pProperties = lens _pProperties (\ s a -> s{_pProperties = a}) . _Default . _Coerce

-- | The name of the container that will serve as the App Mesh proxy.
pContainerName :: Lens' ProxyConfiguration Text
pContainerName = lens _pContainerName (\ s a -> s{_pContainerName = a})

instance FromJSON ProxyConfiguration where
        parseJSON
          = withObject "ProxyConfiguration"
              (\ x ->
                 ProxyConfiguration' <$>
                   (x .:? "type") <*> (x .:? "properties" .!= mempty)
                     <*> (x .: "containerName"))

instance Hashable ProxyConfiguration where

instance NFData ProxyConfiguration where

instance ToJSON ProxyConfiguration where
        toJSON ProxyConfiguration'{..}
          = object
              (catMaybes
                 [("type" .=) <$> _pType,
                  ("properties" .=) <$> _pProperties,
                  Just ("containerName" .= _pContainerName)])

-- | The repository credentials for private registry authentication.
--
--
--
-- /See:/ 'repositoryCredentials' smart constructor.
newtype RepositoryCredentials = RepositoryCredentials'
  { _rcCredentialsParameter :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RepositoryCredentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcCredentialsParameter' - The Amazon Resource Name (ARN) of the secret containing the private repository credentials.
repositoryCredentials
    :: Text -- ^ 'rcCredentialsParameter'
    -> RepositoryCredentials
repositoryCredentials pCredentialsParameter_ =
  RepositoryCredentials' {_rcCredentialsParameter = pCredentialsParameter_}


-- | The Amazon Resource Name (ARN) of the secret containing the private repository credentials.
rcCredentialsParameter :: Lens' RepositoryCredentials Text
rcCredentialsParameter = lens _rcCredentialsParameter (\ s a -> s{_rcCredentialsParameter = a})

instance FromJSON RepositoryCredentials where
        parseJSON
          = withObject "RepositoryCredentials"
              (\ x ->
                 RepositoryCredentials' <$>
                   (x .: "credentialsParameter"))

instance Hashable RepositoryCredentials where

instance NFData RepositoryCredentials where

instance ToJSON RepositoryCredentials where
        toJSON RepositoryCredentials'{..}
          = object
              (catMaybes
                 [Just
                    ("credentialsParameter" .= _rcCredentialsParameter)])

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
-- * 'rName' - The name of the resource, such as @CPU@ , @MEMORY@ , @PORTS@ , @PORTS_UDP@ , or a user-defined resource.
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
rStringSetValue = lens _rStringSetValue (\ s a -> s{_rStringSetValue = a}) . _Default . _Coerce

-- | When the @integerValue@ type is set, the value of the resource must be an integer.
rIntegerValue :: Lens' Resource (Maybe Int)
rIntegerValue = lens _rIntegerValue (\ s a -> s{_rIntegerValue = a})

-- | When the @doubleValue@ type is set, the value of the resource must be a double precision floating-point type.
rDoubleValue :: Lens' Resource (Maybe Double)
rDoubleValue = lens _rDoubleValue (\ s a -> s{_rDoubleValue = a})

-- | When the @longValue@ type is set, the value of the resource must be an extended precision floating-point type.
rLongValue :: Lens' Resource (Maybe Integer)
rLongValue = lens _rLongValue (\ s a -> s{_rLongValue = a})

-- | The name of the resource, such as @CPU@ , @MEMORY@ , @PORTS@ , @PORTS_UDP@ , or a user-defined resource.
rName :: Lens' Resource (Maybe Text)
rName = lens _rName (\ s a -> s{_rName = a})

-- | The type of the resource, such as @INTEGER@ , @DOUBLE@ , @LONG@ , or @STRINGSET@ .
rType :: Lens' Resource (Maybe Text)
rType = lens _rType (\ s a -> s{_rType = a})

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

instance Hashable Resource where

instance NFData Resource where

instance ToJSON Resource where
        toJSON Resource'{..}
          = object
              (catMaybes
                 [("stringSetValue" .=) <$> _rStringSetValue,
                  ("integerValue" .=) <$> _rIntegerValue,
                  ("doubleValue" .=) <$> _rDoubleValue,
                  ("longValue" .=) <$> _rLongValue,
                  ("name" .=) <$> _rName, ("type" .=) <$> _rType])

-- | The type and amount of a resource to assign to a container. The only supported resource is a GPU. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-gpu.html Working with GPUs on Amazon ECS> in the /Amazon Elastic Container Service Developer Guide/
--
--
--
-- /See:/ 'resourceRequirement' smart constructor.
data ResourceRequirement = ResourceRequirement'
  { _rrValue :: !Text
  , _rrType  :: !ResourceType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceRequirement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrValue' - The number of physical @GPUs@ the Amazon ECS container agent will reserve for the container. The number of GPUs reserved for all containers in a task should not exceed the number of available GPUs on the container instance the task is launched on.
--
-- * 'rrType' - The type of resource to assign to a container. The only supported value is @GPU@ .
resourceRequirement
    :: Text -- ^ 'rrValue'
    -> ResourceType -- ^ 'rrType'
    -> ResourceRequirement
resourceRequirement pValue_ pType_ =
  ResourceRequirement' {_rrValue = pValue_, _rrType = pType_}


-- | The number of physical @GPUs@ the Amazon ECS container agent will reserve for the container. The number of GPUs reserved for all containers in a task should not exceed the number of available GPUs on the container instance the task is launched on.
rrValue :: Lens' ResourceRequirement Text
rrValue = lens _rrValue (\ s a -> s{_rrValue = a})

-- | The type of resource to assign to a container. The only supported value is @GPU@ .
rrType :: Lens' ResourceRequirement ResourceType
rrType = lens _rrType (\ s a -> s{_rrType = a})

instance FromJSON ResourceRequirement where
        parseJSON
          = withObject "ResourceRequirement"
              (\ x ->
                 ResourceRequirement' <$>
                   (x .: "value") <*> (x .: "type"))

instance Hashable ResourceRequirement where

instance NFData ResourceRequirement where

instance ToJSON ResourceRequirement where
        toJSON ResourceRequirement'{..}
          = object
              (catMaybes
                 [Just ("value" .= _rrValue),
                  Just ("type" .= _rrType)])

-- | A floating-point percentage of the desired number of tasks to place and keep running in the service. This is used when a service uses the @CODE_DEPLOY@ deployment controller type.
--
--
--
-- /See:/ 'scale' smart constructor.
data Scale = Scale'
  { _sValue :: !(Maybe Double)
  , _sUnit  :: !(Maybe ScaleUnit)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Scale' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sValue' - The value, specified as a percent total of a service's @desiredCount@ , to scale the task set.
--
-- * 'sUnit' - The unit of measure for the scale value.
scale
    :: Scale
scale = Scale' {_sValue = Nothing, _sUnit = Nothing}


-- | The value, specified as a percent total of a service's @desiredCount@ , to scale the task set.
sValue :: Lens' Scale (Maybe Double)
sValue = lens _sValue (\ s a -> s{_sValue = a})

-- | The unit of measure for the scale value.
sUnit :: Lens' Scale (Maybe ScaleUnit)
sUnit = lens _sUnit (\ s a -> s{_sUnit = a})

instance FromJSON Scale where
        parseJSON
          = withObject "Scale"
              (\ x ->
                 Scale' <$> (x .:? "value") <*> (x .:? "unit"))

instance Hashable Scale where

instance NFData Scale where

-- | An object representing the secret to expose to your container. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html Specifying Sensitive Data> in the /Amazon Elastic Container Service Developer Guide/ .
--
--
--
-- /See:/ 'secret' smart constructor.
data Secret = Secret'
  { _sName      :: !Text
  , _sValueFrom :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Secret' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sName' - The value to set as the environment variable on the container.
--
-- * 'sValueFrom' - The secret to expose to the container. If your task is using the EC2 launch type, then supported values are either the full ARN of the AWS Secrets Manager secret or the full ARN of the parameter in the AWS Systems Manager Parameter Store. If your task is using the Fargate launch type, then the only supported value is the full ARN of the parameter in the AWS Systems Manager Parameter Store.
secret
    :: Text -- ^ 'sName'
    -> Text -- ^ 'sValueFrom'
    -> Secret
secret pName_ pValueFrom_ = Secret' {_sName = pName_, _sValueFrom = pValueFrom_}


-- | The value to set as the environment variable on the container.
sName :: Lens' Secret Text
sName = lens _sName (\ s a -> s{_sName = a})

-- | The secret to expose to the container. If your task is using the EC2 launch type, then supported values are either the full ARN of the AWS Secrets Manager secret or the full ARN of the parameter in the AWS Systems Manager Parameter Store. If your task is using the Fargate launch type, then the only supported value is the full ARN of the parameter in the AWS Systems Manager Parameter Store.
sValueFrom :: Lens' Secret Text
sValueFrom = lens _sValueFrom (\ s a -> s{_sValueFrom = a})

instance FromJSON Secret where
        parseJSON
          = withObject "Secret"
              (\ x ->
                 Secret' <$> (x .: "name") <*> (x .: "valueFrom"))

instance Hashable Secret where

instance NFData Secret where

instance ToJSON Secret where
        toJSON Secret'{..}
          = object
              (catMaybes
                 [Just ("name" .= _sName),
                  Just ("valueFrom" .= _sValueFrom)])

-- | Details on an event associated with a service.
--
--
--
-- /See:/ 'serviceEvent' smart constructor.
data ServiceEvent = ServiceEvent'
  { _seCreatedAt :: !(Maybe POSIX)
  , _seId        :: !(Maybe Text)
  , _seMessage   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
  ServiceEvent' {_seCreatedAt = Nothing, _seId = Nothing, _seMessage = Nothing}


-- | The Unix timestamp for when the event was triggered.
seCreatedAt :: Lens' ServiceEvent (Maybe UTCTime)
seCreatedAt = lens _seCreatedAt (\ s a -> s{_seCreatedAt = a}) . mapping _Time

-- | The ID string of the event.
seId :: Lens' ServiceEvent (Maybe Text)
seId = lens _seId (\ s a -> s{_seId = a})

-- | The event message.
seMessage :: Lens' ServiceEvent (Maybe Text)
seMessage = lens _seMessage (\ s a -> s{_seMessage = a})

instance FromJSON ServiceEvent where
        parseJSON
          = withObject "ServiceEvent"
              (\ x ->
                 ServiceEvent' <$>
                   (x .:? "createdAt") <*> (x .:? "id") <*>
                     (x .:? "message"))

instance Hashable ServiceEvent where

instance NFData ServiceEvent where

-- | Details of the service registry.
--
--
--
-- /See:/ 'serviceRegistry' smart constructor.
data ServiceRegistry = ServiceRegistry'
  { _srRegistryARN   :: !(Maybe Text)
  , _srContainerName :: !(Maybe Text)
  , _srContainerPort :: !(Maybe Int)
  , _srPort          :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServiceRegistry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srRegistryARN' - The Amazon Resource Name (ARN) of the service registry. The currently supported service registry is Amazon Route 53 Auto Naming. For more information, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_autonaming_Service.html Service> .
--
-- * 'srContainerName' - The container name value, already specified in the task definition, to be used for your service discovery service. If the task definition that your service task specifies uses the @bridge@ or @host@ network mode, you must specify a @containerName@ and @containerPort@ combination from the task definition. If the task definition that your service task specifies uses the @awsvpc@ network mode and a type SRV DNS record is used, you must specify either a @containerName@ and @containerPort@ combination or a @port@ value, but not both.
--
-- * 'srContainerPort' - The port value, already specified in the task definition, to be used for your service discovery service. If the task definition your service task specifies uses the @bridge@ or @host@ network mode, you must specify a @containerName@ and @containerPort@ combination from the task definition. If the task definition your service task specifies uses the @awsvpc@ network mode and a type SRV DNS record is used, you must specify either a @containerName@ and @containerPort@ combination or a @port@ value, but not both.
--
-- * 'srPort' - The port value used if your service discovery service specified an SRV record. This field may be used if both the @awsvpc@ network mode and SRV records are used.
serviceRegistry
    :: ServiceRegistry
serviceRegistry =
  ServiceRegistry'
    { _srRegistryARN = Nothing
    , _srContainerName = Nothing
    , _srContainerPort = Nothing
    , _srPort = Nothing
    }


-- | The Amazon Resource Name (ARN) of the service registry. The currently supported service registry is Amazon Route 53 Auto Naming. For more information, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_autonaming_Service.html Service> .
srRegistryARN :: Lens' ServiceRegistry (Maybe Text)
srRegistryARN = lens _srRegistryARN (\ s a -> s{_srRegistryARN = a})

-- | The container name value, already specified in the task definition, to be used for your service discovery service. If the task definition that your service task specifies uses the @bridge@ or @host@ network mode, you must specify a @containerName@ and @containerPort@ combination from the task definition. If the task definition that your service task specifies uses the @awsvpc@ network mode and a type SRV DNS record is used, you must specify either a @containerName@ and @containerPort@ combination or a @port@ value, but not both.
srContainerName :: Lens' ServiceRegistry (Maybe Text)
srContainerName = lens _srContainerName (\ s a -> s{_srContainerName = a})

-- | The port value, already specified in the task definition, to be used for your service discovery service. If the task definition your service task specifies uses the @bridge@ or @host@ network mode, you must specify a @containerName@ and @containerPort@ combination from the task definition. If the task definition your service task specifies uses the @awsvpc@ network mode and a type SRV DNS record is used, you must specify either a @containerName@ and @containerPort@ combination or a @port@ value, but not both.
srContainerPort :: Lens' ServiceRegistry (Maybe Int)
srContainerPort = lens _srContainerPort (\ s a -> s{_srContainerPort = a})

-- | The port value used if your service discovery service specified an SRV record. This field may be used if both the @awsvpc@ network mode and SRV records are used.
srPort :: Lens' ServiceRegistry (Maybe Int)
srPort = lens _srPort (\ s a -> s{_srPort = a})

instance FromJSON ServiceRegistry where
        parseJSON
          = withObject "ServiceRegistry"
              (\ x ->
                 ServiceRegistry' <$>
                   (x .:? "registryArn") <*> (x .:? "containerName") <*>
                     (x .:? "containerPort")
                     <*> (x .:? "port"))

instance Hashable ServiceRegistry where

instance NFData ServiceRegistry where

instance ToJSON ServiceRegistry where
        toJSON ServiceRegistry'{..}
          = object
              (catMaybes
                 [("registryArn" .=) <$> _srRegistryARN,
                  ("containerName" .=) <$> _srContainerName,
                  ("containerPort" .=) <$> _srContainerPort,
                  ("port" .=) <$> _srPort])

-- | The current account setting for a resource.
--
--
--
-- /See:/ 'setting' smart constructor.
data Setting = Setting'
  { _setValue        :: !(Maybe Text)
  , _setName         :: !(Maybe SettingName)
  , _setPrincipalARN :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Setting' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'setValue' - The current account setting for the resource name. If @enabled@ , then the resource will receive the new Amazon Resource Name (ARN) and resource identifier (ID) format. If @disabled@ , then the resource will receive the old Amazon Resource Name (ARN) and resource identifier (ID) format.
--
-- * 'setName' - The account resource name.
--
-- * 'setPrincipalARN' - The ARN of the principal, which can be an IAM user, IAM role, or the root user. If this field is omitted, the authenticated user is assumed.
setting
    :: Setting
setting =
  Setting' {_setValue = Nothing, _setName = Nothing, _setPrincipalARN = Nothing}


-- | The current account setting for the resource name. If @enabled@ , then the resource will receive the new Amazon Resource Name (ARN) and resource identifier (ID) format. If @disabled@ , then the resource will receive the old Amazon Resource Name (ARN) and resource identifier (ID) format.
setValue :: Lens' Setting (Maybe Text)
setValue = lens _setValue (\ s a -> s{_setValue = a})

-- | The account resource name.
setName :: Lens' Setting (Maybe SettingName)
setName = lens _setName (\ s a -> s{_setName = a})

-- | The ARN of the principal, which can be an IAM user, IAM role, or the root user. If this field is omitted, the authenticated user is assumed.
setPrincipalARN :: Lens' Setting (Maybe Text)
setPrincipalARN = lens _setPrincipalARN (\ s a -> s{_setPrincipalARN = a})

instance FromJSON Setting where
        parseJSON
          = withObject "Setting"
              (\ x ->
                 Setting' <$>
                   (x .:? "value") <*> (x .:? "name") <*>
                     (x .:? "principalArn"))

instance Hashable Setting where

instance NFData Setting where

-- | A list of namespaced kernel parameters to set in the container. This parameter maps to @Sysctls@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--sysctl@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
--
-- It is not recommended that you specify network-related @systemControls@ parameters for multiple containers in a single task that also uses either the @awsvpc@ or @host@ network mode for the following reasons:
--
--     * For tasks that use the @awsvpc@ network mode, if you set @systemControls@ for any container, it applies to all containers in the task. If you set different @systemControls@ for multiple containers in a single task, the container that is started last determines which @systemControls@ take effect.
--
--     * For tasks that use the @host@ network mode, the @systemControls@ parameter applies to the container instance's kernel parameter as well as that of all containers of any tasks running on that container instance.
--
--
--
--
-- /See:/ 'systemControl' smart constructor.
data SystemControl = SystemControl'
  { _scValue     :: !(Maybe Text)
  , _scNamespace :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SystemControl' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scValue' - The value for the namespaced kernel parameter specified in @namespace@ .
--
-- * 'scNamespace' - The namespaced kernel parameter for which to set a @value@ .
systemControl
    :: SystemControl
systemControl = SystemControl' {_scValue = Nothing, _scNamespace = Nothing}


-- | The value for the namespaced kernel parameter specified in @namespace@ .
scValue :: Lens' SystemControl (Maybe Text)
scValue = lens _scValue (\ s a -> s{_scValue = a})

-- | The namespaced kernel parameter for which to set a @value@ .
scNamespace :: Lens' SystemControl (Maybe Text)
scNamespace = lens _scNamespace (\ s a -> s{_scNamespace = a})

instance FromJSON SystemControl where
        parseJSON
          = withObject "SystemControl"
              (\ x ->
                 SystemControl' <$>
                   (x .:? "value") <*> (x .:? "namespace"))

instance Hashable SystemControl where

instance NFData SystemControl where

instance ToJSON SystemControl where
        toJSON SystemControl'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _scValue,
                  ("namespace" .=) <$> _scNamespace])

-- | The metadata that you apply to a resource to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagValue :: !(Maybe Text)
  , _tagKey   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - The optional part of a key-value pair that make up a tag. A @value@ acts as a descriptor within a tag category (key).
--
-- * 'tagKey' - One part of a key-value pair that make up a tag. A @key@ is a general label that acts like a category for more specific tag values.
tag
    :: Tag
tag = Tag' {_tagValue = Nothing, _tagKey = Nothing}


-- | The optional part of a key-value pair that make up a tag. A @value@ acts as a descriptor within a tag category (key).
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | One part of a key-value pair that make up a tag. A @key@ is a general label that acts like a category for more specific tag values.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .:? "value") <*> (x .:? "key"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _tagValue, ("key" .=) <$> _tagKey])

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
  , _tAttachments          :: !(Maybe [Attachment])
  , _tCreatedAt            :: !(Maybe POSIX)
  , _tStopCode             :: !(Maybe TaskStopCode)
  , _tPlatformVersion      :: !(Maybe Text)
  , _tTaskARN              :: !(Maybe Text)
  , _tContainerInstanceARN :: !(Maybe Text)
  , _tExecutionStoppedAt   :: !(Maybe POSIX)
  , _tLastStatus           :: !(Maybe Text)
  , _tMemory               :: !(Maybe Text)
  , _tPullStoppedAt        :: !(Maybe POSIX)
  , _tContainers           :: !(Maybe [Container])
  , _tStartedAt            :: !(Maybe POSIX)
  , _tVersion              :: !(Maybe Integer)
  , _tStartedBy            :: !(Maybe Text)
  , _tStoppedReason        :: !(Maybe Text)
  , _tConnectivity         :: !(Maybe Connectivity)
  , _tStoppingAt           :: !(Maybe POSIX)
  , _tLaunchType           :: !(Maybe LaunchType)
  , _tTaskDefinitionARN    :: !(Maybe Text)
  , _tHealthStatus         :: !(Maybe HealthStatus)
  , _tConnectivityAt       :: !(Maybe POSIX)
  , _tCpu                  :: !(Maybe Text)
  , _tPullStartedAt        :: !(Maybe POSIX)
  , _tTags                 :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Task' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tStoppedAt' - The Unix timestamp for when the task was stopped (the task transitioned from the @RUNNING@ state to the @STOPPED@ state).
--
-- * 'tDesiredStatus' - The desired status of the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_life_cycle.html Task Lifecycle> .
--
-- * 'tOverrides' - One or more container overrides.
--
-- * 'tClusterARN' - The ARN of the cluster that hosts the task.
--
-- * 'tGroup' - The name of the task group associated with the task.
--
-- * 'tAttachments' - The Elastic Network Adapter associated with the task if the task uses the @awsvpc@ network mode.
--
-- * 'tCreatedAt' - The Unix timestamp for when the task was created (the task entered the @PENDING@ state).
--
-- * 'tStopCode' - The stop code indicating why a task was stopped. The @stoppedReason@ may contain additional details.
--
-- * 'tPlatformVersion' - The platform version on which your task is running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'tTaskARN' - The Amazon Resource Name (ARN) of the task.
--
-- * 'tContainerInstanceARN' - The ARN of the container instances that host the task.
--
-- * 'tExecutionStoppedAt' - The Unix timestamp for when the task execution stopped.
--
-- * 'tLastStatus' - The last known status of the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_life_cycle.html Task Lifecycle> .
--
-- * 'tMemory' - The amount of memory (in MiB) used by the task as expressed in a task definition. It can be expressed as an integer using MiB, for example @1024@ . It can also be expressed as a string using GB, for example @1GB@ or @1 GB@ . String values are converted to an integer indicating the MiB when the task definition is registered. If you are using the EC2 launch type, this field is optional. If you are using the Fargate launch type, this field is required and you must use one of the following values, which determines your range of supported values for the @cpu@ parameter:     * 512 (0.5 GB), 1024 (1 GB), 2048 (2 GB) - Available @cpu@ values: 256 (.25 vCPU)     * 1024 (1 GB), 2048 (2 GB), 3072 (3 GB), 4096 (4 GB) - Available @cpu@ values: 512 (.5 vCPU)     * 2048 (2 GB), 3072 (3 GB), 4096 (4 GB), 5120 (5 GB), 6144 (6 GB), 7168 (7 GB), 8192 (8 GB) - Available @cpu@ values: 1024 (1 vCPU)     * Between 4096 (4 GB) and 16384 (16 GB) in increments of 1024 (1 GB) - Available @cpu@ values: 2048 (2 vCPU)     * Between 8192 (8 GB) and 30720 (30 GB) in increments of 1024 (1 GB) - Available @cpu@ values: 4096 (4 vCPU)
--
-- * 'tPullStoppedAt' - The Unix timestamp for when the container image pull completed.
--
-- * 'tContainers' - The containers associated with the task.
--
-- * 'tStartedAt' - The Unix timestamp for when the task started (the task transitioned from the @PENDING@ state to the @RUNNING@ state).
--
-- * 'tVersion' - The version counter for the task. Every time a task experiences a change that triggers a CloudWatch event, the version counter is incremented. If you are replicating your Amazon ECS task state with CloudWatch Events, you can compare the version of a task reported by the Amazon ECS API actionss with the version reported in CloudWatch Events for the task (inside the @detail@ object) to verify that the version in your event stream is current.
--
-- * 'tStartedBy' - The tag specified when a task is started. If the task is started by an Amazon ECS service, then the @startedBy@ parameter contains the deployment ID of the service that starts it.
--
-- * 'tStoppedReason' - The reason that the task was stopped.
--
-- * 'tConnectivity' - The connectivity status of a task.
--
-- * 'tStoppingAt' - The Unix timestamp for when the task stops (transitions from the @RUNNING@ state to @STOPPED@ ).
--
-- * 'tLaunchType' - The launch type on which your task is running. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'tTaskDefinitionARN' - The ARN of the task definition that creates the task.
--
-- * 'tHealthStatus' - The health status for the task, which is determined by the health of the essential containers in the task. If all essential containers in the task are reporting as @HEALTHY@ , then the task status also reports as @HEALTHY@ . If any essential containers in the task are reporting as @UNHEALTHY@ or @UNKNOWN@ , then the task status also reports as @UNHEALTHY@ or @UNKNOWN@ , accordingly.
--
-- * 'tConnectivityAt' - The Unix timestamp for when the task last went into @CONNECTED@ status.
--
-- * 'tCpu' - The number of CPU units used by the task as expressed in a task definition. It can be expressed as an integer using CPU units, for example @1024@ . It can also be expressed as a string using vCPUs, for example @1 vCPU@ or @1 vcpu@ . String values are converted to an integer indicating the CPU units when the task definition is registered. If you are using the EC2 launch type, this field is optional. Supported values are between @128@ CPU units (@0.125@ vCPUs) and @10240@ CPU units (@10@ vCPUs). If you are using the Fargate launch type, this field is required and you must use one of the following values, which determines your range of supported values for the @memory@ parameter:     * 256 (.25 vCPU) - Available @memory@ values: 512 (0.5 GB), 1024 (1 GB), 2048 (2 GB)     * 512 (.5 vCPU) - Available @memory@ values: 1024 (1 GB), 2048 (2 GB), 3072 (3 GB), 4096 (4 GB)     * 1024 (1 vCPU) - Available @memory@ values: 2048 (2 GB), 3072 (3 GB), 4096 (4 GB), 5120 (5 GB), 6144 (6 GB), 7168 (7 GB), 8192 (8 GB)     * 2048 (2 vCPU) - Available @memory@ values: Between 4096 (4 GB) and 16384 (16 GB) in increments of 1024 (1 GB)     * 4096 (4 vCPU) - Available @memory@ values: Between 8192 (8 GB) and 30720 (30 GB) in increments of 1024 (1 GB)
--
-- * 'tPullStartedAt' - The Unix timestamp for when the container image pull began.
--
-- * 'tTags' - The metadata that you apply to the task to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
task
    :: Task
task =
  Task'
    { _tStoppedAt = Nothing
    , _tDesiredStatus = Nothing
    , _tOverrides = Nothing
    , _tClusterARN = Nothing
    , _tGroup = Nothing
    , _tAttachments = Nothing
    , _tCreatedAt = Nothing
    , _tStopCode = Nothing
    , _tPlatformVersion = Nothing
    , _tTaskARN = Nothing
    , _tContainerInstanceARN = Nothing
    , _tExecutionStoppedAt = Nothing
    , _tLastStatus = Nothing
    , _tMemory = Nothing
    , _tPullStoppedAt = Nothing
    , _tContainers = Nothing
    , _tStartedAt = Nothing
    , _tVersion = Nothing
    , _tStartedBy = Nothing
    , _tStoppedReason = Nothing
    , _tConnectivity = Nothing
    , _tStoppingAt = Nothing
    , _tLaunchType = Nothing
    , _tTaskDefinitionARN = Nothing
    , _tHealthStatus = Nothing
    , _tConnectivityAt = Nothing
    , _tCpu = Nothing
    , _tPullStartedAt = Nothing
    , _tTags = Nothing
    }


-- | The Unix timestamp for when the task was stopped (the task transitioned from the @RUNNING@ state to the @STOPPED@ state).
tStoppedAt :: Lens' Task (Maybe UTCTime)
tStoppedAt = lens _tStoppedAt (\ s a -> s{_tStoppedAt = a}) . mapping _Time

-- | The desired status of the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_life_cycle.html Task Lifecycle> .
tDesiredStatus :: Lens' Task (Maybe Text)
tDesiredStatus = lens _tDesiredStatus (\ s a -> s{_tDesiredStatus = a})

-- | One or more container overrides.
tOverrides :: Lens' Task (Maybe TaskOverride)
tOverrides = lens _tOverrides (\ s a -> s{_tOverrides = a})

-- | The ARN of the cluster that hosts the task.
tClusterARN :: Lens' Task (Maybe Text)
tClusterARN = lens _tClusterARN (\ s a -> s{_tClusterARN = a})

-- | The name of the task group associated with the task.
tGroup :: Lens' Task (Maybe Text)
tGroup = lens _tGroup (\ s a -> s{_tGroup = a})

-- | The Elastic Network Adapter associated with the task if the task uses the @awsvpc@ network mode.
tAttachments :: Lens' Task [Attachment]
tAttachments = lens _tAttachments (\ s a -> s{_tAttachments = a}) . _Default . _Coerce

-- | The Unix timestamp for when the task was created (the task entered the @PENDING@ state).
tCreatedAt :: Lens' Task (Maybe UTCTime)
tCreatedAt = lens _tCreatedAt (\ s a -> s{_tCreatedAt = a}) . mapping _Time

-- | The stop code indicating why a task was stopped. The @stoppedReason@ may contain additional details.
tStopCode :: Lens' Task (Maybe TaskStopCode)
tStopCode = lens _tStopCode (\ s a -> s{_tStopCode = a})

-- | The platform version on which your task is running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
tPlatformVersion :: Lens' Task (Maybe Text)
tPlatformVersion = lens _tPlatformVersion (\ s a -> s{_tPlatformVersion = a})

-- | The Amazon Resource Name (ARN) of the task.
tTaskARN :: Lens' Task (Maybe Text)
tTaskARN = lens _tTaskARN (\ s a -> s{_tTaskARN = a})

-- | The ARN of the container instances that host the task.
tContainerInstanceARN :: Lens' Task (Maybe Text)
tContainerInstanceARN = lens _tContainerInstanceARN (\ s a -> s{_tContainerInstanceARN = a})

-- | The Unix timestamp for when the task execution stopped.
tExecutionStoppedAt :: Lens' Task (Maybe UTCTime)
tExecutionStoppedAt = lens _tExecutionStoppedAt (\ s a -> s{_tExecutionStoppedAt = a}) . mapping _Time

-- | The last known status of the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_life_cycle.html Task Lifecycle> .
tLastStatus :: Lens' Task (Maybe Text)
tLastStatus = lens _tLastStatus (\ s a -> s{_tLastStatus = a})

-- | The amount of memory (in MiB) used by the task as expressed in a task definition. It can be expressed as an integer using MiB, for example @1024@ . It can also be expressed as a string using GB, for example @1GB@ or @1 GB@ . String values are converted to an integer indicating the MiB when the task definition is registered. If you are using the EC2 launch type, this field is optional. If you are using the Fargate launch type, this field is required and you must use one of the following values, which determines your range of supported values for the @cpu@ parameter:     * 512 (0.5 GB), 1024 (1 GB), 2048 (2 GB) - Available @cpu@ values: 256 (.25 vCPU)     * 1024 (1 GB), 2048 (2 GB), 3072 (3 GB), 4096 (4 GB) - Available @cpu@ values: 512 (.5 vCPU)     * 2048 (2 GB), 3072 (3 GB), 4096 (4 GB), 5120 (5 GB), 6144 (6 GB), 7168 (7 GB), 8192 (8 GB) - Available @cpu@ values: 1024 (1 vCPU)     * Between 4096 (4 GB) and 16384 (16 GB) in increments of 1024 (1 GB) - Available @cpu@ values: 2048 (2 vCPU)     * Between 8192 (8 GB) and 30720 (30 GB) in increments of 1024 (1 GB) - Available @cpu@ values: 4096 (4 vCPU)
tMemory :: Lens' Task (Maybe Text)
tMemory = lens _tMemory (\ s a -> s{_tMemory = a})

-- | The Unix timestamp for when the container image pull completed.
tPullStoppedAt :: Lens' Task (Maybe UTCTime)
tPullStoppedAt = lens _tPullStoppedAt (\ s a -> s{_tPullStoppedAt = a}) . mapping _Time

-- | The containers associated with the task.
tContainers :: Lens' Task [Container]
tContainers = lens _tContainers (\ s a -> s{_tContainers = a}) . _Default . _Coerce

-- | The Unix timestamp for when the task started (the task transitioned from the @PENDING@ state to the @RUNNING@ state).
tStartedAt :: Lens' Task (Maybe UTCTime)
tStartedAt = lens _tStartedAt (\ s a -> s{_tStartedAt = a}) . mapping _Time

-- | The version counter for the task. Every time a task experiences a change that triggers a CloudWatch event, the version counter is incremented. If you are replicating your Amazon ECS task state with CloudWatch Events, you can compare the version of a task reported by the Amazon ECS API actionss with the version reported in CloudWatch Events for the task (inside the @detail@ object) to verify that the version in your event stream is current.
tVersion :: Lens' Task (Maybe Integer)
tVersion = lens _tVersion (\ s a -> s{_tVersion = a})

-- | The tag specified when a task is started. If the task is started by an Amazon ECS service, then the @startedBy@ parameter contains the deployment ID of the service that starts it.
tStartedBy :: Lens' Task (Maybe Text)
tStartedBy = lens _tStartedBy (\ s a -> s{_tStartedBy = a})

-- | The reason that the task was stopped.
tStoppedReason :: Lens' Task (Maybe Text)
tStoppedReason = lens _tStoppedReason (\ s a -> s{_tStoppedReason = a})

-- | The connectivity status of a task.
tConnectivity :: Lens' Task (Maybe Connectivity)
tConnectivity = lens _tConnectivity (\ s a -> s{_tConnectivity = a})

-- | The Unix timestamp for when the task stops (transitions from the @RUNNING@ state to @STOPPED@ ).
tStoppingAt :: Lens' Task (Maybe UTCTime)
tStoppingAt = lens _tStoppingAt (\ s a -> s{_tStoppingAt = a}) . mapping _Time

-- | The launch type on which your task is running. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
tLaunchType :: Lens' Task (Maybe LaunchType)
tLaunchType = lens _tLaunchType (\ s a -> s{_tLaunchType = a})

-- | The ARN of the task definition that creates the task.
tTaskDefinitionARN :: Lens' Task (Maybe Text)
tTaskDefinitionARN = lens _tTaskDefinitionARN (\ s a -> s{_tTaskDefinitionARN = a})

-- | The health status for the task, which is determined by the health of the essential containers in the task. If all essential containers in the task are reporting as @HEALTHY@ , then the task status also reports as @HEALTHY@ . If any essential containers in the task are reporting as @UNHEALTHY@ or @UNKNOWN@ , then the task status also reports as @UNHEALTHY@ or @UNKNOWN@ , accordingly.
tHealthStatus :: Lens' Task (Maybe HealthStatus)
tHealthStatus = lens _tHealthStatus (\ s a -> s{_tHealthStatus = a})

-- | The Unix timestamp for when the task last went into @CONNECTED@ status.
tConnectivityAt :: Lens' Task (Maybe UTCTime)
tConnectivityAt = lens _tConnectivityAt (\ s a -> s{_tConnectivityAt = a}) . mapping _Time

-- | The number of CPU units used by the task as expressed in a task definition. It can be expressed as an integer using CPU units, for example @1024@ . It can also be expressed as a string using vCPUs, for example @1 vCPU@ or @1 vcpu@ . String values are converted to an integer indicating the CPU units when the task definition is registered. If you are using the EC2 launch type, this field is optional. Supported values are between @128@ CPU units (@0.125@ vCPUs) and @10240@ CPU units (@10@ vCPUs). If you are using the Fargate launch type, this field is required and you must use one of the following values, which determines your range of supported values for the @memory@ parameter:     * 256 (.25 vCPU) - Available @memory@ values: 512 (0.5 GB), 1024 (1 GB), 2048 (2 GB)     * 512 (.5 vCPU) - Available @memory@ values: 1024 (1 GB), 2048 (2 GB), 3072 (3 GB), 4096 (4 GB)     * 1024 (1 vCPU) - Available @memory@ values: 2048 (2 GB), 3072 (3 GB), 4096 (4 GB), 5120 (5 GB), 6144 (6 GB), 7168 (7 GB), 8192 (8 GB)     * 2048 (2 vCPU) - Available @memory@ values: Between 4096 (4 GB) and 16384 (16 GB) in increments of 1024 (1 GB)     * 4096 (4 vCPU) - Available @memory@ values: Between 8192 (8 GB) and 30720 (30 GB) in increments of 1024 (1 GB)
tCpu :: Lens' Task (Maybe Text)
tCpu = lens _tCpu (\ s a -> s{_tCpu = a})

-- | The Unix timestamp for when the container image pull began.
tPullStartedAt :: Lens' Task (Maybe UTCTime)
tPullStartedAt = lens _tPullStartedAt (\ s a -> s{_tPullStartedAt = a}) . mapping _Time

-- | The metadata that you apply to the task to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
tTags :: Lens' Task [Tag]
tTags = lens _tTags (\ s a -> s{_tTags = a}) . _Default . _Coerce

instance FromJSON Task where
        parseJSON
          = withObject "Task"
              (\ x ->
                 Task' <$>
                   (x .:? "stoppedAt") <*> (x .:? "desiredStatus") <*>
                     (x .:? "overrides")
                     <*> (x .:? "clusterArn")
                     <*> (x .:? "group")
                     <*> (x .:? "attachments" .!= mempty)
                     <*> (x .:? "createdAt")
                     <*> (x .:? "stopCode")
                     <*> (x .:? "platformVersion")
                     <*> (x .:? "taskArn")
                     <*> (x .:? "containerInstanceArn")
                     <*> (x .:? "executionStoppedAt")
                     <*> (x .:? "lastStatus")
                     <*> (x .:? "memory")
                     <*> (x .:? "pullStoppedAt")
                     <*> (x .:? "containers" .!= mempty)
                     <*> (x .:? "startedAt")
                     <*> (x .:? "version")
                     <*> (x .:? "startedBy")
                     <*> (x .:? "stoppedReason")
                     <*> (x .:? "connectivity")
                     <*> (x .:? "stoppingAt")
                     <*> (x .:? "launchType")
                     <*> (x .:? "taskDefinitionArn")
                     <*> (x .:? "healthStatus")
                     <*> (x .:? "connectivityAt")
                     <*> (x .:? "cpu")
                     <*> (x .:? "pullStartedAt")
                     <*> (x .:? "tags" .!= mempty))

instance Hashable Task where

instance NFData Task where

-- | Details of a task definition.
--
--
--
-- /See:/ 'taskDefinition' smart constructor.
data TaskDefinition = TaskDefinition'
  { _tdStatus                  :: !(Maybe TaskDefinitionStatus)
  , _tdExecutionRoleARN        :: !(Maybe Text)
  , _tdRequiresCompatibilities :: !(Maybe [Compatibility])
  , _tdPidMode                 :: !(Maybe PidMode)
  , _tdFamily                  :: !(Maybe Text)
  , _tdIpcMode                 :: !(Maybe IPcMode)
  , _tdContainerDefinitions    :: !(Maybe [ContainerDefinition])
  , _tdMemory                  :: !(Maybe Text)
  , _tdProxyConfiguration      :: !(Maybe ProxyConfiguration)
  , _tdTaskRoleARN             :: !(Maybe Text)
  , _tdPlacementConstraints    :: !(Maybe [TaskDefinitionPlacementConstraint])
  , _tdNetworkMode             :: !(Maybe NetworkMode)
  , _tdTaskDefinitionARN       :: !(Maybe Text)
  , _tdCompatibilities         :: !(Maybe [Compatibility])
  , _tdRevision                :: !(Maybe Int)
  , _tdVolumes                 :: !(Maybe [Volume])
  , _tdCpu                     :: !(Maybe Text)
  , _tdRequiresAttributes      :: !(Maybe [Attribute])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TaskDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdStatus' - The status of the task definition.
--
-- * 'tdExecutionRoleARN' - The Amazon Resource Name (ARN) of the task execution role that the Amazon ECS container agent and the Docker daemon can assume.
--
-- * 'tdRequiresCompatibilities' - The launch type that the task is using.
--
-- * 'tdPidMode' - The process namespace to use for the containers in the task. The valid values are @host@ or @task@ . If @host@ is specified, then all containers within the tasks that specified the @host@ PID mode on the same container instance share the same IPC resources with the host Amazon EC2 instance. If @task@ is specified, all containers within the specified task share the same process namespace. If no value is specified, the default is a private namespace. For more information, see <https://docs.docker.com/engine/reference/run/#pid-settings---pid PID settings> in the /Docker run reference/ . If the @host@ PID mode is used, be aware that there is a heightened risk of undesired process namespace expose. For more information, see <https://docs.docker.com/engine/security/security/ Docker security> .
--
-- * 'tdFamily' - The family of your task definition, used as the definition name.
--
-- * 'tdIpcMode' - The IPC resource namespace to use for the containers in the task. The valid values are @host@ , @task@ , or @none@ . If @host@ is specified, then all containers within the tasks that specified the @host@ IPC mode on the same container instance share the same IPC resources with the host Amazon EC2 instance. If @task@ is specified, all containers within the specified task share the same IPC resources. If @none@ is specified, then IPC resources within the containers of a task are private and not shared with other containers in a task or on the container instance. If no value is specified, then the IPC resource namespace sharing depends on the Docker daemon setting on the container instance. For more information, see <https://docs.docker.com/engine/reference/run/#ipc-settings---ipc IPC settings> in the /Docker run reference/ . If the @host@ IPC mode is used, be aware that there is a heightened risk of undesired IPC namespace expose. For more information, see <https://docs.docker.com/engine/security/security/ Docker security> . If you are setting namespaced kernel parameters using @systemControls@ for the containers in the task, the following will apply to your IPC resource namespace. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_definition_parameters.html System Controls> in the /Amazon Elastic Container Service Developer Guide/ .     * For tasks that use the @host@ IPC mode, IPC namespace related @systemControls@ are not supported.     * For tasks that use the @task@ IPC mode, IPC namespace related @systemControls@ will apply to all containers within a task.
--
-- * 'tdContainerDefinitions' - A list of container definitions in JSON format that describe the different containers that make up your task. For more information about container definition parameters and defaults, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Amazon ECS Task Definitions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'tdMemory' - The amount (in MiB) of memory used by the task. If using the EC2 launch type, this field is optional and any value can be used. If using the Fargate launch type, this field is required and you must use one of the following values, which determines your range of valid values for the @cpu@ parameter:     * 512 (0.5 GB), 1024 (1 GB), 2048 (2 GB) - Available @cpu@ values: 256 (.25 vCPU)     * 1024 (1 GB), 2048 (2 GB), 3072 (3 GB), 4096 (4 GB) - Available @cpu@ values: 512 (.5 vCPU)     * 2048 (2 GB), 3072 (3 GB), 4096 (4 GB), 5120 (5 GB), 6144 (6 GB), 7168 (7 GB), 8192 (8 GB) - Available @cpu@ values: 1024 (1 vCPU)     * Between 4096 (4 GB) and 16384 (16 GB) in increments of 1024 (1 GB) - Available @cpu@ values: 2048 (2 vCPU)     * Between 8192 (8 GB) and 30720 (30 GB) in increments of 1024 (1 GB) - Available @cpu@ values: 4096 (4 vCPU)
--
-- * 'tdProxyConfiguration' -
--
-- * 'tdTaskRoleARN' - The ARN of the IAM role that containers in this task can assume. All containers in this task are granted the permissions that are specified in this role. IAM roles for tasks on Windows require that the @-EnableTaskIAMRole@ option is set when you launch the Amazon ECS-optimized Windows AMI. Your containers must also run some configuration code in order to take advantage of the feature. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/windows_task_IAM_roles.html Windows IAM Roles for Tasks> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'tdPlacementConstraints' - An array of placement constraint objects to use for tasks. This field is not valid if you are using the Fargate launch type for your task.
--
-- * 'tdNetworkMode' - The Docker networking mode to use for the containers in the task. The valid values are @none@ , @bridge@ , @awsvpc@ , and @host@ . The default Docker network mode is @bridge@ . If you are using the Fargate launch type, the @awsvpc@ network mode is required. If you are using the EC2 launch type, any network mode can be used. If the network mode is set to @none@ , you cannot specify port mappings in your container definitions, and the tasks containers do not have external connectivity. The @host@ and @awsvpc@ network modes offer the highest networking performance for containers because they use the EC2 network stack instead of the virtualized network stack provided by the @bridge@ mode. With the @host@ and @awsvpc@ network modes, exposed container ports are mapped directly to the corresponding host port (for the @host@ network mode) or the attached elastic network interface port (for the @awsvpc@ network mode), so you cannot take advantage of dynamic host port mappings.  If the network mode is @awsvpc@ , the task is allocated an elastic network interface, and you must specify a 'NetworkConfiguration' value when you create a service or run a task with the task definition. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking> in the /Amazon Elastic Container Service Developer Guide/ . If the network mode is @host@ , you cannot run multiple instantiations of the same task on a single container instance when port mappings are used. Docker for Windows uses different network modes than Docker for Linux. When you register a task definition with Windows containers, you must not specify a network mode. If you use the console to register a task definition with Windows containers, you must choose the @<default>@ network mode object.  For more information, see <https://docs.docker.com/engine/reference/run/#network-settings Network settings> in the /Docker run reference/ .
--
-- * 'tdTaskDefinitionARN' - The full Amazon Resource Name (ARN) of the task definition.
--
-- * 'tdCompatibilities' - The launch type to use with your task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'tdRevision' - The revision of the task in a particular family. The revision is a version number of a task definition in a family. When you register a task definition for the first time, the revision is @1@ . Each time that you register a new revision of a task definition in the same family, the revision value always increases by one, even if you have deregistered previous revisions in this family.
--
-- * 'tdVolumes' - The list of volumes in a task. If you are using the Fargate launch type, the @host@ and @sourcePath@ parameters are not supported. For more information about volume definition parameters and defaults, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_definitions.html Amazon ECS Task Definitions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'tdCpu' - The number of @cpu@ units used by the task. If you are using the EC2 launch type, this field is optional and any value can be used. If you are using the Fargate launch type, this field is required and you must use one of the following values, which determines your range of valid values for the @memory@ parameter:     * 256 (.25 vCPU) - Available @memory@ values: 512 (0.5 GB), 1024 (1 GB), 2048 (2 GB)     * 512 (.5 vCPU) - Available @memory@ values: 1024 (1 GB), 2048 (2 GB), 3072 (3 GB), 4096 (4 GB)     * 1024 (1 vCPU) - Available @memory@ values: 2048 (2 GB), 3072 (3 GB), 4096 (4 GB), 5120 (5 GB), 6144 (6 GB), 7168 (7 GB), 8192 (8 GB)     * 2048 (2 vCPU) - Available @memory@ values: Between 4096 (4 GB) and 16384 (16 GB) in increments of 1024 (1 GB)     * 4096 (4 vCPU) - Available @memory@ values: Between 8192 (8 GB) and 30720 (30 GB) in increments of 1024 (1 GB)
--
-- * 'tdRequiresAttributes' - The container instance attributes required by your task. This field is not valid if you are using the Fargate launch type for your task.
taskDefinition
    :: TaskDefinition
taskDefinition =
  TaskDefinition'
    { _tdStatus = Nothing
    , _tdExecutionRoleARN = Nothing
    , _tdRequiresCompatibilities = Nothing
    , _tdPidMode = Nothing
    , _tdFamily = Nothing
    , _tdIpcMode = Nothing
    , _tdContainerDefinitions = Nothing
    , _tdMemory = Nothing
    , _tdProxyConfiguration = Nothing
    , _tdTaskRoleARN = Nothing
    , _tdPlacementConstraints = Nothing
    , _tdNetworkMode = Nothing
    , _tdTaskDefinitionARN = Nothing
    , _tdCompatibilities = Nothing
    , _tdRevision = Nothing
    , _tdVolumes = Nothing
    , _tdCpu = Nothing
    , _tdRequiresAttributes = Nothing
    }


-- | The status of the task definition.
tdStatus :: Lens' TaskDefinition (Maybe TaskDefinitionStatus)
tdStatus = lens _tdStatus (\ s a -> s{_tdStatus = a})

-- | The Amazon Resource Name (ARN) of the task execution role that the Amazon ECS container agent and the Docker daemon can assume.
tdExecutionRoleARN :: Lens' TaskDefinition (Maybe Text)
tdExecutionRoleARN = lens _tdExecutionRoleARN (\ s a -> s{_tdExecutionRoleARN = a})

-- | The launch type that the task is using.
tdRequiresCompatibilities :: Lens' TaskDefinition [Compatibility]
tdRequiresCompatibilities = lens _tdRequiresCompatibilities (\ s a -> s{_tdRequiresCompatibilities = a}) . _Default . _Coerce

-- | The process namespace to use for the containers in the task. The valid values are @host@ or @task@ . If @host@ is specified, then all containers within the tasks that specified the @host@ PID mode on the same container instance share the same IPC resources with the host Amazon EC2 instance. If @task@ is specified, all containers within the specified task share the same process namespace. If no value is specified, the default is a private namespace. For more information, see <https://docs.docker.com/engine/reference/run/#pid-settings---pid PID settings> in the /Docker run reference/ . If the @host@ PID mode is used, be aware that there is a heightened risk of undesired process namespace expose. For more information, see <https://docs.docker.com/engine/security/security/ Docker security> .
tdPidMode :: Lens' TaskDefinition (Maybe PidMode)
tdPidMode = lens _tdPidMode (\ s a -> s{_tdPidMode = a})

-- | The family of your task definition, used as the definition name.
tdFamily :: Lens' TaskDefinition (Maybe Text)
tdFamily = lens _tdFamily (\ s a -> s{_tdFamily = a})

-- | The IPC resource namespace to use for the containers in the task. The valid values are @host@ , @task@ , or @none@ . If @host@ is specified, then all containers within the tasks that specified the @host@ IPC mode on the same container instance share the same IPC resources with the host Amazon EC2 instance. If @task@ is specified, all containers within the specified task share the same IPC resources. If @none@ is specified, then IPC resources within the containers of a task are private and not shared with other containers in a task or on the container instance. If no value is specified, then the IPC resource namespace sharing depends on the Docker daemon setting on the container instance. For more information, see <https://docs.docker.com/engine/reference/run/#ipc-settings---ipc IPC settings> in the /Docker run reference/ . If the @host@ IPC mode is used, be aware that there is a heightened risk of undesired IPC namespace expose. For more information, see <https://docs.docker.com/engine/security/security/ Docker security> . If you are setting namespaced kernel parameters using @systemControls@ for the containers in the task, the following will apply to your IPC resource namespace. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_definition_parameters.html System Controls> in the /Amazon Elastic Container Service Developer Guide/ .     * For tasks that use the @host@ IPC mode, IPC namespace related @systemControls@ are not supported.     * For tasks that use the @task@ IPC mode, IPC namespace related @systemControls@ will apply to all containers within a task.
tdIpcMode :: Lens' TaskDefinition (Maybe IPcMode)
tdIpcMode = lens _tdIpcMode (\ s a -> s{_tdIpcMode = a})

-- | A list of container definitions in JSON format that describe the different containers that make up your task. For more information about container definition parameters and defaults, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Amazon ECS Task Definitions> in the /Amazon Elastic Container Service Developer Guide/ .
tdContainerDefinitions :: Lens' TaskDefinition [ContainerDefinition]
tdContainerDefinitions = lens _tdContainerDefinitions (\ s a -> s{_tdContainerDefinitions = a}) . _Default . _Coerce

-- | The amount (in MiB) of memory used by the task. If using the EC2 launch type, this field is optional and any value can be used. If using the Fargate launch type, this field is required and you must use one of the following values, which determines your range of valid values for the @cpu@ parameter:     * 512 (0.5 GB), 1024 (1 GB), 2048 (2 GB) - Available @cpu@ values: 256 (.25 vCPU)     * 1024 (1 GB), 2048 (2 GB), 3072 (3 GB), 4096 (4 GB) - Available @cpu@ values: 512 (.5 vCPU)     * 2048 (2 GB), 3072 (3 GB), 4096 (4 GB), 5120 (5 GB), 6144 (6 GB), 7168 (7 GB), 8192 (8 GB) - Available @cpu@ values: 1024 (1 vCPU)     * Between 4096 (4 GB) and 16384 (16 GB) in increments of 1024 (1 GB) - Available @cpu@ values: 2048 (2 vCPU)     * Between 8192 (8 GB) and 30720 (30 GB) in increments of 1024 (1 GB) - Available @cpu@ values: 4096 (4 vCPU)
tdMemory :: Lens' TaskDefinition (Maybe Text)
tdMemory = lens _tdMemory (\ s a -> s{_tdMemory = a})

-- |
tdProxyConfiguration :: Lens' TaskDefinition (Maybe ProxyConfiguration)
tdProxyConfiguration = lens _tdProxyConfiguration (\ s a -> s{_tdProxyConfiguration = a})

-- | The ARN of the IAM role that containers in this task can assume. All containers in this task are granted the permissions that are specified in this role. IAM roles for tasks on Windows require that the @-EnableTaskIAMRole@ option is set when you launch the Amazon ECS-optimized Windows AMI. Your containers must also run some configuration code in order to take advantage of the feature. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/windows_task_IAM_roles.html Windows IAM Roles for Tasks> in the /Amazon Elastic Container Service Developer Guide/ .
tdTaskRoleARN :: Lens' TaskDefinition (Maybe Text)
tdTaskRoleARN = lens _tdTaskRoleARN (\ s a -> s{_tdTaskRoleARN = a})

-- | An array of placement constraint objects to use for tasks. This field is not valid if you are using the Fargate launch type for your task.
tdPlacementConstraints :: Lens' TaskDefinition [TaskDefinitionPlacementConstraint]
tdPlacementConstraints = lens _tdPlacementConstraints (\ s a -> s{_tdPlacementConstraints = a}) . _Default . _Coerce

-- | The Docker networking mode to use for the containers in the task. The valid values are @none@ , @bridge@ , @awsvpc@ , and @host@ . The default Docker network mode is @bridge@ . If you are using the Fargate launch type, the @awsvpc@ network mode is required. If you are using the EC2 launch type, any network mode can be used. If the network mode is set to @none@ , you cannot specify port mappings in your container definitions, and the tasks containers do not have external connectivity. The @host@ and @awsvpc@ network modes offer the highest networking performance for containers because they use the EC2 network stack instead of the virtualized network stack provided by the @bridge@ mode. With the @host@ and @awsvpc@ network modes, exposed container ports are mapped directly to the corresponding host port (for the @host@ network mode) or the attached elastic network interface port (for the @awsvpc@ network mode), so you cannot take advantage of dynamic host port mappings.  If the network mode is @awsvpc@ , the task is allocated an elastic network interface, and you must specify a 'NetworkConfiguration' value when you create a service or run a task with the task definition. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking> in the /Amazon Elastic Container Service Developer Guide/ . If the network mode is @host@ , you cannot run multiple instantiations of the same task on a single container instance when port mappings are used. Docker for Windows uses different network modes than Docker for Linux. When you register a task definition with Windows containers, you must not specify a network mode. If you use the console to register a task definition with Windows containers, you must choose the @<default>@ network mode object.  For more information, see <https://docs.docker.com/engine/reference/run/#network-settings Network settings> in the /Docker run reference/ .
tdNetworkMode :: Lens' TaskDefinition (Maybe NetworkMode)
tdNetworkMode = lens _tdNetworkMode (\ s a -> s{_tdNetworkMode = a})

-- | The full Amazon Resource Name (ARN) of the task definition.
tdTaskDefinitionARN :: Lens' TaskDefinition (Maybe Text)
tdTaskDefinitionARN = lens _tdTaskDefinitionARN (\ s a -> s{_tdTaskDefinitionARN = a})

-- | The launch type to use with your task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
tdCompatibilities :: Lens' TaskDefinition [Compatibility]
tdCompatibilities = lens _tdCompatibilities (\ s a -> s{_tdCompatibilities = a}) . _Default . _Coerce

-- | The revision of the task in a particular family. The revision is a version number of a task definition in a family. When you register a task definition for the first time, the revision is @1@ . Each time that you register a new revision of a task definition in the same family, the revision value always increases by one, even if you have deregistered previous revisions in this family.
tdRevision :: Lens' TaskDefinition (Maybe Int)
tdRevision = lens _tdRevision (\ s a -> s{_tdRevision = a})

-- | The list of volumes in a task. If you are using the Fargate launch type, the @host@ and @sourcePath@ parameters are not supported. For more information about volume definition parameters and defaults, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_definitions.html Amazon ECS Task Definitions> in the /Amazon Elastic Container Service Developer Guide/ .
tdVolumes :: Lens' TaskDefinition [Volume]
tdVolumes = lens _tdVolumes (\ s a -> s{_tdVolumes = a}) . _Default . _Coerce

-- | The number of @cpu@ units used by the task. If you are using the EC2 launch type, this field is optional and any value can be used. If you are using the Fargate launch type, this field is required and you must use one of the following values, which determines your range of valid values for the @memory@ parameter:     * 256 (.25 vCPU) - Available @memory@ values: 512 (0.5 GB), 1024 (1 GB), 2048 (2 GB)     * 512 (.5 vCPU) - Available @memory@ values: 1024 (1 GB), 2048 (2 GB), 3072 (3 GB), 4096 (4 GB)     * 1024 (1 vCPU) - Available @memory@ values: 2048 (2 GB), 3072 (3 GB), 4096 (4 GB), 5120 (5 GB), 6144 (6 GB), 7168 (7 GB), 8192 (8 GB)     * 2048 (2 vCPU) - Available @memory@ values: Between 4096 (4 GB) and 16384 (16 GB) in increments of 1024 (1 GB)     * 4096 (4 vCPU) - Available @memory@ values: Between 8192 (8 GB) and 30720 (30 GB) in increments of 1024 (1 GB)
tdCpu :: Lens' TaskDefinition (Maybe Text)
tdCpu = lens _tdCpu (\ s a -> s{_tdCpu = a})

-- | The container instance attributes required by your task. This field is not valid if you are using the Fargate launch type for your task.
tdRequiresAttributes :: Lens' TaskDefinition [Attribute]
tdRequiresAttributes = lens _tdRequiresAttributes (\ s a -> s{_tdRequiresAttributes = a}) . _Default . _Coerce

instance FromJSON TaskDefinition where
        parseJSON
          = withObject "TaskDefinition"
              (\ x ->
                 TaskDefinition' <$>
                   (x .:? "status") <*> (x .:? "executionRoleArn") <*>
                     (x .:? "requiresCompatibilities" .!= mempty)
                     <*> (x .:? "pidMode")
                     <*> (x .:? "family")
                     <*> (x .:? "ipcMode")
                     <*> (x .:? "containerDefinitions" .!= mempty)
                     <*> (x .:? "memory")
                     <*> (x .:? "proxyConfiguration")
                     <*> (x .:? "taskRoleArn")
                     <*> (x .:? "placementConstraints" .!= mempty)
                     <*> (x .:? "networkMode")
                     <*> (x .:? "taskDefinitionArn")
                     <*> (x .:? "compatibilities" .!= mempty)
                     <*> (x .:? "revision")
                     <*> (x .:? "volumes" .!= mempty)
                     <*> (x .:? "cpu")
                     <*> (x .:? "requiresAttributes" .!= mempty))

instance Hashable TaskDefinition where

instance NFData TaskDefinition where

-- | An object representing a constraint on task placement in the task definition.
--
--
-- If you are using the Fargate launch type, task placement constraints are not supported.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html Task Placement Constraints> in the /Amazon Elastic Container Service Developer Guide/ .
--
--
-- /See:/ 'taskDefinitionPlacementConstraint' smart constructor.
data TaskDefinitionPlacementConstraint = TaskDefinitionPlacementConstraint'
  { _tdpcExpression :: !(Maybe Text)
  , _tdpcType       :: !(Maybe TaskDefinitionPlacementConstraintType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TaskDefinitionPlacementConstraint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdpcExpression' - A cluster query language expression to apply to the constraint. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'tdpcType' - The type of constraint. The @DistinctInstance@ constraint ensures that each task in a particular group is running on a different container instance. The @MemberOf@ constraint restricts selection to be from a group of valid candidates.
taskDefinitionPlacementConstraint
    :: TaskDefinitionPlacementConstraint
taskDefinitionPlacementConstraint =
  TaskDefinitionPlacementConstraint'
    {_tdpcExpression = Nothing, _tdpcType = Nothing}


-- | A cluster query language expression to apply to the constraint. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language> in the /Amazon Elastic Container Service Developer Guide/ .
tdpcExpression :: Lens' TaskDefinitionPlacementConstraint (Maybe Text)
tdpcExpression = lens _tdpcExpression (\ s a -> s{_tdpcExpression = a})

-- | The type of constraint. The @DistinctInstance@ constraint ensures that each task in a particular group is running on a different container instance. The @MemberOf@ constraint restricts selection to be from a group of valid candidates.
tdpcType :: Lens' TaskDefinitionPlacementConstraint (Maybe TaskDefinitionPlacementConstraintType)
tdpcType = lens _tdpcType (\ s a -> s{_tdpcType = a})

instance FromJSON TaskDefinitionPlacementConstraint
         where
        parseJSON
          = withObject "TaskDefinitionPlacementConstraint"
              (\ x ->
                 TaskDefinitionPlacementConstraint' <$>
                   (x .:? "expression") <*> (x .:? "type"))

instance Hashable TaskDefinitionPlacementConstraint
         where

instance NFData TaskDefinitionPlacementConstraint
         where

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
  , _toExecutionRoleARN   :: !(Maybe Text)
  , _toTaskRoleARN        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TaskOverride' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'toContainerOverrides' - One or more container overrides sent to a task.
--
-- * 'toExecutionRoleARN' - The Amazon Resource Name (ARN) of the task execution role that the Amazon ECS container agent and the Docker daemon can assume.
--
-- * 'toTaskRoleARN' - The Amazon Resource Name (ARN) of the IAM role that containers in this task can assume. All containers in this task are granted the permissions that are specified in this role.
taskOverride
    :: TaskOverride
taskOverride =
  TaskOverride'
    { _toContainerOverrides = Nothing
    , _toExecutionRoleARN = Nothing
    , _toTaskRoleARN = Nothing
    }


-- | One or more container overrides sent to a task.
toContainerOverrides :: Lens' TaskOverride [ContainerOverride]
toContainerOverrides = lens _toContainerOverrides (\ s a -> s{_toContainerOverrides = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the task execution role that the Amazon ECS container agent and the Docker daemon can assume.
toExecutionRoleARN :: Lens' TaskOverride (Maybe Text)
toExecutionRoleARN = lens _toExecutionRoleARN (\ s a -> s{_toExecutionRoleARN = a})

-- | The Amazon Resource Name (ARN) of the IAM role that containers in this task can assume. All containers in this task are granted the permissions that are specified in this role.
toTaskRoleARN :: Lens' TaskOverride (Maybe Text)
toTaskRoleARN = lens _toTaskRoleARN (\ s a -> s{_toTaskRoleARN = a})

instance FromJSON TaskOverride where
        parseJSON
          = withObject "TaskOverride"
              (\ x ->
                 TaskOverride' <$>
                   (x .:? "containerOverrides" .!= mempty) <*>
                     (x .:? "executionRoleArn")
                     <*> (x .:? "taskRoleArn"))

instance Hashable TaskOverride where

instance NFData TaskOverride where

instance ToJSON TaskOverride where
        toJSON TaskOverride'{..}
          = object
              (catMaybes
                 [("containerOverrides" .=) <$> _toContainerOverrides,
                  ("executionRoleArn" .=) <$> _toExecutionRoleARN,
                  ("taskRoleArn" .=) <$> _toTaskRoleARN])

-- | Information about a set of Amazon ECS tasks in an AWS CodeDeploy deployment. An Amazon ECS task set includes details such as the desired number of tasks, how many tasks are running, and whether the task set serves production traffic.
--
--
--
-- /See:/ 'taskSet' smart constructor.
data TaskSet = TaskSet'
  { _tsRunningCount         :: !(Maybe Int)
  , _tsStatus               :: !(Maybe Text)
  , _tsComputedDesiredCount :: !(Maybe Int)
  , _tsCreatedAt            :: !(Maybe POSIX)
  , _tsPlatformVersion      :: !(Maybe Text)
  , _tsScale                :: !(Maybe Scale)
  , _tsLoadBalancers        :: !(Maybe [LoadBalancer])
  , _tsStabilityStatusAt    :: !(Maybe POSIX)
  , _tsPendingCount         :: !(Maybe Int)
  , _tsTaskSetARN           :: !(Maybe Text)
  , _tsStartedBy            :: !(Maybe Text)
  , _tsId                   :: !(Maybe Text)
  , _tsLaunchType           :: !(Maybe LaunchType)
  , _tsUpdatedAt            :: !(Maybe POSIX)
  , _tsTaskDefinition       :: !(Maybe Text)
  , _tsExternalId           :: !(Maybe Text)
  , _tsNetworkConfiguration :: !(Maybe NetworkConfiguration)
  , _tsStabilityStatus      :: !(Maybe StabilityStatus)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TaskSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsRunningCount' - The number of tasks in the task set that are in the @RUNNING@ status during a deployment. A task in the @RUNNING@ state is running and ready for use.
--
-- * 'tsStatus' - The status of the task set. The following describes each state:     * PRIMARY    * The task set is serving production traffic.     * ACTIVE    * The task set is not serving production traffic.     * DRAINING    * The tasks in the task set are being stopped and their corresponding targets are being deregistered from their target group.
--
-- * 'tsComputedDesiredCount' - The computed desired count for the task set. This is calculated by multiplying the service's @desiredCount@ by the task set's @scale@ percentage.
--
-- * 'tsCreatedAt' - The Unix timestamp for when the task set was created.
--
-- * 'tsPlatformVersion' - The platform version on which the tasks in the task set are running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'tsScale' - A floating-point percentage of the desired number of tasks to place and keep running in the service.
--
-- * 'tsLoadBalancers' - Details on a load balancer that is used with a task set.
--
-- * 'tsStabilityStatusAt' - The Unix timestamp for when the task set stability status was retrieved.
--
-- * 'tsPendingCount' - The number of tasks in the task set that are in the @PENDING@ status during a deployment. A task in the @PENDING@ state is preparing to enter the @RUNNING@ state. A task set enters the @PENDING@ status when it launches for the first time, or when it is restarted after being in the @STOPPED@ state.
--
-- * 'tsTaskSetARN' - The Amazon Resource Name (ARN) of the task set.
--
-- * 'tsStartedBy' - The tag specified when a task set is started. If the task is started by an AWS CodeDeploy deployment, then the @startedBy@ parameter is @CODE_DEPLOY@ .
--
-- * 'tsId' - The ID of the task set.
--
-- * 'tsLaunchType' - The launch type the tasks in the task set are using. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'tsUpdatedAt' - The Unix timestamp for when the task set was last updated.
--
-- * 'tsTaskDefinition' - The task definition the task set is using.
--
-- * 'tsExternalId' - The deployment ID of the AWS CodeDeploy deployment.
--
-- * 'tsNetworkConfiguration' - The network configuration for the task set.
--
-- * 'tsStabilityStatus' - The stability status, which indicates whether the task set has reached a steady state. If the following conditions are met, the task set will be in @STEADY_STATE@ :     * The task @runningCount@ is equal to the @computedDesiredCount@ .     * The @pendingCount@ is @0@ .     * There are no tasks running on container instances in the @DRAINING@ status.     * All tasks are reporting a healthy status from the load balancers, service discovery, and container health checks. If any of those conditions are not met, the stability status returns @STABILIZING@ .
taskSet
    :: TaskSet
taskSet =
  TaskSet'
    { _tsRunningCount = Nothing
    , _tsStatus = Nothing
    , _tsComputedDesiredCount = Nothing
    , _tsCreatedAt = Nothing
    , _tsPlatformVersion = Nothing
    , _tsScale = Nothing
    , _tsLoadBalancers = Nothing
    , _tsStabilityStatusAt = Nothing
    , _tsPendingCount = Nothing
    , _tsTaskSetARN = Nothing
    , _tsStartedBy = Nothing
    , _tsId = Nothing
    , _tsLaunchType = Nothing
    , _tsUpdatedAt = Nothing
    , _tsTaskDefinition = Nothing
    , _tsExternalId = Nothing
    , _tsNetworkConfiguration = Nothing
    , _tsStabilityStatus = Nothing
    }


-- | The number of tasks in the task set that are in the @RUNNING@ status during a deployment. A task in the @RUNNING@ state is running and ready for use.
tsRunningCount :: Lens' TaskSet (Maybe Int)
tsRunningCount = lens _tsRunningCount (\ s a -> s{_tsRunningCount = a})

-- | The status of the task set. The following describes each state:     * PRIMARY    * The task set is serving production traffic.     * ACTIVE    * The task set is not serving production traffic.     * DRAINING    * The tasks in the task set are being stopped and their corresponding targets are being deregistered from their target group.
tsStatus :: Lens' TaskSet (Maybe Text)
tsStatus = lens _tsStatus (\ s a -> s{_tsStatus = a})

-- | The computed desired count for the task set. This is calculated by multiplying the service's @desiredCount@ by the task set's @scale@ percentage.
tsComputedDesiredCount :: Lens' TaskSet (Maybe Int)
tsComputedDesiredCount = lens _tsComputedDesiredCount (\ s a -> s{_tsComputedDesiredCount = a})

-- | The Unix timestamp for when the task set was created.
tsCreatedAt :: Lens' TaskSet (Maybe UTCTime)
tsCreatedAt = lens _tsCreatedAt (\ s a -> s{_tsCreatedAt = a}) . mapping _Time

-- | The platform version on which the tasks in the task set are running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
tsPlatformVersion :: Lens' TaskSet (Maybe Text)
tsPlatformVersion = lens _tsPlatformVersion (\ s a -> s{_tsPlatformVersion = a})

-- | A floating-point percentage of the desired number of tasks to place and keep running in the service.
tsScale :: Lens' TaskSet (Maybe Scale)
tsScale = lens _tsScale (\ s a -> s{_tsScale = a})

-- | Details on a load balancer that is used with a task set.
tsLoadBalancers :: Lens' TaskSet [LoadBalancer]
tsLoadBalancers = lens _tsLoadBalancers (\ s a -> s{_tsLoadBalancers = a}) . _Default . _Coerce

-- | The Unix timestamp for when the task set stability status was retrieved.
tsStabilityStatusAt :: Lens' TaskSet (Maybe UTCTime)
tsStabilityStatusAt = lens _tsStabilityStatusAt (\ s a -> s{_tsStabilityStatusAt = a}) . mapping _Time

-- | The number of tasks in the task set that are in the @PENDING@ status during a deployment. A task in the @PENDING@ state is preparing to enter the @RUNNING@ state. A task set enters the @PENDING@ status when it launches for the first time, or when it is restarted after being in the @STOPPED@ state.
tsPendingCount :: Lens' TaskSet (Maybe Int)
tsPendingCount = lens _tsPendingCount (\ s a -> s{_tsPendingCount = a})

-- | The Amazon Resource Name (ARN) of the task set.
tsTaskSetARN :: Lens' TaskSet (Maybe Text)
tsTaskSetARN = lens _tsTaskSetARN (\ s a -> s{_tsTaskSetARN = a})

-- | The tag specified when a task set is started. If the task is started by an AWS CodeDeploy deployment, then the @startedBy@ parameter is @CODE_DEPLOY@ .
tsStartedBy :: Lens' TaskSet (Maybe Text)
tsStartedBy = lens _tsStartedBy (\ s a -> s{_tsStartedBy = a})

-- | The ID of the task set.
tsId :: Lens' TaskSet (Maybe Text)
tsId = lens _tsId (\ s a -> s{_tsId = a})

-- | The launch type the tasks in the task set are using. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
tsLaunchType :: Lens' TaskSet (Maybe LaunchType)
tsLaunchType = lens _tsLaunchType (\ s a -> s{_tsLaunchType = a})

-- | The Unix timestamp for when the task set was last updated.
tsUpdatedAt :: Lens' TaskSet (Maybe UTCTime)
tsUpdatedAt = lens _tsUpdatedAt (\ s a -> s{_tsUpdatedAt = a}) . mapping _Time

-- | The task definition the task set is using.
tsTaskDefinition :: Lens' TaskSet (Maybe Text)
tsTaskDefinition = lens _tsTaskDefinition (\ s a -> s{_tsTaskDefinition = a})

-- | The deployment ID of the AWS CodeDeploy deployment.
tsExternalId :: Lens' TaskSet (Maybe Text)
tsExternalId = lens _tsExternalId (\ s a -> s{_tsExternalId = a})

-- | The network configuration for the task set.
tsNetworkConfiguration :: Lens' TaskSet (Maybe NetworkConfiguration)
tsNetworkConfiguration = lens _tsNetworkConfiguration (\ s a -> s{_tsNetworkConfiguration = a})

-- | The stability status, which indicates whether the task set has reached a steady state. If the following conditions are met, the task set will be in @STEADY_STATE@ :     * The task @runningCount@ is equal to the @computedDesiredCount@ .     * The @pendingCount@ is @0@ .     * There are no tasks running on container instances in the @DRAINING@ status.     * All tasks are reporting a healthy status from the load balancers, service discovery, and container health checks. If any of those conditions are not met, the stability status returns @STABILIZING@ .
tsStabilityStatus :: Lens' TaskSet (Maybe StabilityStatus)
tsStabilityStatus = lens _tsStabilityStatus (\ s a -> s{_tsStabilityStatus = a})

instance FromJSON TaskSet where
        parseJSON
          = withObject "TaskSet"
              (\ x ->
                 TaskSet' <$>
                   (x .:? "runningCount") <*> (x .:? "status") <*>
                     (x .:? "computedDesiredCount")
                     <*> (x .:? "createdAt")
                     <*> (x .:? "platformVersion")
                     <*> (x .:? "scale")
                     <*> (x .:? "loadBalancers" .!= mempty)
                     <*> (x .:? "stabilityStatusAt")
                     <*> (x .:? "pendingCount")
                     <*> (x .:? "taskSetArn")
                     <*> (x .:? "startedBy")
                     <*> (x .:? "id")
                     <*> (x .:? "launchType")
                     <*> (x .:? "updatedAt")
                     <*> (x .:? "taskDefinition")
                     <*> (x .:? "externalId")
                     <*> (x .:? "networkConfiguration")
                     <*> (x .:? "stabilityStatus"))

instance Hashable TaskSet where

instance NFData TaskSet where

-- | The container path, mount options, and size of the tmpfs mount.
--
--
--
-- /See:/ 'tmpfs' smart constructor.
data Tmpfs = Tmpfs'
  { _tMountOptions  :: !(Maybe [Text])
  , _tContainerPath :: !Text
  , _tSize          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tmpfs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tMountOptions' - The list of tmpfs volume mount options. Valid values: @"defaults" | "ro" | "rw" | "suid" | "nosuid" | "dev" | "nodev" | "exec" | "noexec" | "sync" | "async" | "dirsync" | "remount" | "mand" | "nomand" | "atime" | "noatime" | "diratime" | "nodiratime" | "bind" | "rbind" | "unbindable" | "runbindable" | "private" | "rprivate" | "shared" | "rshared" | "slave" | "rslave" | "relatime" | "norelatime" | "strictatime" | "nostrictatime" | "mode" | "uid" | "gid" | "nr_inodes" | "nr_blocks" | "mpol"@
--
-- * 'tContainerPath' - The absolute file path where the tmpfs volume is to be mounted.
--
-- * 'tSize' - The size (in MiB) of the tmpfs volume.
tmpfs
    :: Text -- ^ 'tContainerPath'
    -> Int -- ^ 'tSize'
    -> Tmpfs
tmpfs pContainerPath_ pSize_ =
  Tmpfs'
    { _tMountOptions = Nothing
    , _tContainerPath = pContainerPath_
    , _tSize = pSize_
    }


-- | The list of tmpfs volume mount options. Valid values: @"defaults" | "ro" | "rw" | "suid" | "nosuid" | "dev" | "nodev" | "exec" | "noexec" | "sync" | "async" | "dirsync" | "remount" | "mand" | "nomand" | "atime" | "noatime" | "diratime" | "nodiratime" | "bind" | "rbind" | "unbindable" | "runbindable" | "private" | "rprivate" | "shared" | "rshared" | "slave" | "rslave" | "relatime" | "norelatime" | "strictatime" | "nostrictatime" | "mode" | "uid" | "gid" | "nr_inodes" | "nr_blocks" | "mpol"@
tMountOptions :: Lens' Tmpfs [Text]
tMountOptions = lens _tMountOptions (\ s a -> s{_tMountOptions = a}) . _Default . _Coerce

-- | The absolute file path where the tmpfs volume is to be mounted.
tContainerPath :: Lens' Tmpfs Text
tContainerPath = lens _tContainerPath (\ s a -> s{_tContainerPath = a})

-- | The size (in MiB) of the tmpfs volume.
tSize :: Lens' Tmpfs Int
tSize = lens _tSize (\ s a -> s{_tSize = a})

instance FromJSON Tmpfs where
        parseJSON
          = withObject "Tmpfs"
              (\ x ->
                 Tmpfs' <$>
                   (x .:? "mountOptions" .!= mempty) <*>
                     (x .: "containerPath")
                     <*> (x .: "size"))

instance Hashable Tmpfs where

instance NFData Tmpfs where

instance ToJSON Tmpfs where
        toJSON Tmpfs'{..}
          = object
              (catMaybes
                 [("mountOptions" .=) <$> _tMountOptions,
                  Just ("containerPath" .= _tContainerPath),
                  Just ("size" .= _tSize)])

-- | The @ulimit@ settings to pass to the container.
--
--
--
-- /See:/ 'ulimit' smart constructor.
data Ulimit = Ulimit'
  { _uName      :: !UlimitName
  , _uSoftLimit :: !Int
  , _uHardLimit :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
    {_uName = pName_, _uSoftLimit = pSoftLimit_, _uHardLimit = pHardLimit_}


-- | The @type@ of the @ulimit@ .
uName :: Lens' Ulimit UlimitName
uName = lens _uName (\ s a -> s{_uName = a})

-- | The soft limit for the ulimit type.
uSoftLimit :: Lens' Ulimit Int
uSoftLimit = lens _uSoftLimit (\ s a -> s{_uSoftLimit = a})

-- | The hard limit for the ulimit type.
uHardLimit :: Lens' Ulimit Int
uHardLimit = lens _uHardLimit (\ s a -> s{_uHardLimit = a})

instance FromJSON Ulimit where
        parseJSON
          = withObject "Ulimit"
              (\ x ->
                 Ulimit' <$>
                   (x .: "name") <*> (x .: "softLimit") <*>
                     (x .: "hardLimit"))

instance Hashable Ulimit where

instance NFData Ulimit where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
viAgentHash = lens _viAgentHash (\ s a -> s{_viAgentHash = a})

-- | The version number of the Amazon ECS container agent.
viAgentVersion :: Lens' VersionInfo (Maybe Text)
viAgentVersion = lens _viAgentVersion (\ s a -> s{_viAgentVersion = a})

-- | The Docker version running on the container instance.
viDockerVersion :: Lens' VersionInfo (Maybe Text)
viDockerVersion = lens _viDockerVersion (\ s a -> s{_viDockerVersion = a})

instance FromJSON VersionInfo where
        parseJSON
          = withObject "VersionInfo"
              (\ x ->
                 VersionInfo' <$>
                   (x .:? "agentHash") <*> (x .:? "agentVersion") <*>
                     (x .:? "dockerVersion"))

instance Hashable VersionInfo where

instance NFData VersionInfo where

instance ToJSON VersionInfo where
        toJSON VersionInfo'{..}
          = object
              (catMaybes
                 [("agentHash" .=) <$> _viAgentHash,
                  ("agentVersion" .=) <$> _viAgentVersion,
                  ("dockerVersion" .=) <$> _viDockerVersion])

-- | A data volume used in a task definition. For tasks that use a Docker volume, specify a @DockerVolumeConfiguration@ . For tasks that use a bind mount host volume, specify a @host@ and optional @sourcePath@ . For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_data_volumes.html Using Data Volumes in Tasks> .
--
--
--
-- /See:/ 'volume' smart constructor.
data Volume = Volume'
  { _vDockerVolumeConfiguration :: !(Maybe DockerVolumeConfiguration)
  , _vName                      :: !(Maybe Text)
  , _vHost                      :: !(Maybe HostVolumeProperties)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Volume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vDockerVolumeConfiguration' - This parameter is specified when you are using Docker volumes. Docker volumes are only supported when you are using the EC2 launch type. Windows containers only support the use of the @local@ driver. To use bind mounts, specify a @host@ instead.
--
-- * 'vName' - The name of the volume. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. This name is referenced in the @sourceVolume@ parameter of container definition @mountPoints@ .
--
-- * 'vHost' - This parameter is specified when you are using bind mount host volumes. Bind mount host volumes are supported when you are using either the EC2 or Fargate launch types. The contents of the @host@ parameter determine whether your bind mount host volume persists on the host container instance and where it is stored. If the @host@ parameter is empty, then the Docker daemon assigns a host path for your data volume. However, the data is not guaranteed to persist after the containers associated with it stop running. Windows containers can mount whole directories on the same drive as @> env:ProgramData@ . Windows containers cannot mount directories on a different drive, and mount point cannot be across drives. For example, you can mount @C:\my\path:C:\my\path@ and @D:\:D:\@ , but not @D:\my\path:C:\my\path@ or @D:\:C:\my\path@ .
volume
    :: Volume
volume =
  Volume'
    {_vDockerVolumeConfiguration = Nothing, _vName = Nothing, _vHost = Nothing}


-- | This parameter is specified when you are using Docker volumes. Docker volumes are only supported when you are using the EC2 launch type. Windows containers only support the use of the @local@ driver. To use bind mounts, specify a @host@ instead.
vDockerVolumeConfiguration :: Lens' Volume (Maybe DockerVolumeConfiguration)
vDockerVolumeConfiguration = lens _vDockerVolumeConfiguration (\ s a -> s{_vDockerVolumeConfiguration = a})

-- | The name of the volume. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. This name is referenced in the @sourceVolume@ parameter of container definition @mountPoints@ .
vName :: Lens' Volume (Maybe Text)
vName = lens _vName (\ s a -> s{_vName = a})

-- | This parameter is specified when you are using bind mount host volumes. Bind mount host volumes are supported when you are using either the EC2 or Fargate launch types. The contents of the @host@ parameter determine whether your bind mount host volume persists on the host container instance and where it is stored. If the @host@ parameter is empty, then the Docker daemon assigns a host path for your data volume. However, the data is not guaranteed to persist after the containers associated with it stop running. Windows containers can mount whole directories on the same drive as @> env:ProgramData@ . Windows containers cannot mount directories on a different drive, and mount point cannot be across drives. For example, you can mount @C:\my\path:C:\my\path@ and @D:\:D:\@ , but not @D:\my\path:C:\my\path@ or @D:\:C:\my\path@ .
vHost :: Lens' Volume (Maybe HostVolumeProperties)
vHost = lens _vHost (\ s a -> s{_vHost = a})

instance FromJSON Volume where
        parseJSON
          = withObject "Volume"
              (\ x ->
                 Volume' <$>
                   (x .:? "dockerVolumeConfiguration") <*>
                     (x .:? "name")
                     <*> (x .:? "host"))

instance Hashable Volume where

instance NFData Volume where

instance ToJSON Volume where
        toJSON Volume'{..}
          = object
              (catMaybes
                 [("dockerVolumeConfiguration" .=) <$>
                    _vDockerVolumeConfiguration,
                  ("name" .=) <$> _vName, ("host" .=) <$> _vHost])

-- | Details on a data volume from another container in the same task definition.
--
--
--
-- /See:/ 'volumeFrom' smart constructor.
data VolumeFrom = VolumeFrom'
  { _vfSourceContainer :: !(Maybe Text)
  , _vfReadOnly        :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VolumeFrom' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vfSourceContainer' - The name of another container within the same task definition from which to mount volumes.
--
-- * 'vfReadOnly' - If this value is @true@ , the container has read-only access to the volume. If this value is @false@ , then the container can write to the volume. The default value is @false@ .
volumeFrom
    :: VolumeFrom
volumeFrom = VolumeFrom' {_vfSourceContainer = Nothing, _vfReadOnly = Nothing}


-- | The name of another container within the same task definition from which to mount volumes.
vfSourceContainer :: Lens' VolumeFrom (Maybe Text)
vfSourceContainer = lens _vfSourceContainer (\ s a -> s{_vfSourceContainer = a})

-- | If this value is @true@ , the container has read-only access to the volume. If this value is @false@ , then the container can write to the volume. The default value is @false@ .
vfReadOnly :: Lens' VolumeFrom (Maybe Bool)
vfReadOnly = lens _vfReadOnly (\ s a -> s{_vfReadOnly = a})

instance FromJSON VolumeFrom where
        parseJSON
          = withObject "VolumeFrom"
              (\ x ->
                 VolumeFrom' <$>
                   (x .:? "sourceContainer") <*> (x .:? "readOnly"))

instance Hashable VolumeFrom where

instance NFData VolumeFrom where

instance ToJSON VolumeFrom where
        toJSON VolumeFrom'{..}
          = object
              (catMaybes
                 [("sourceContainer" .=) <$> _vfSourceContainer,
                  ("readOnly" .=) <$> _vfReadOnly])
