{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.ECS.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.ECS.Types
    (
    -- * Service
      ECS
    -- ** Errors
    , JSONError

    -- * Cluster
    , Cluster
    , cluster
    , cluStatus
    , cluClusterARN
    , cluRunningTasksCount
    , cluRegisteredContainerInstancesCount
    , cluPendingTasksCount
    , cluClusterName

    -- * Container
    , Container
    , container
    , conNetworkBindings
    , conContainerARN
    , conTaskARN
    , conLastStatus
    , conReason
    , conName
    , conExitCode

    -- * ContainerDefinition
    , ContainerDefinition
    , containerDefinition
    , cdImage
    , cdCommand
    , cdVolumesFrom
    , cdEnvironment
    , cdEntryPoint
    , cdPortMappings
    , cdMemory
    , cdName
    , cdMountPoints
    , cdLinks
    , cdEssential
    , cdCpu

    -- * ContainerInstance
    , ContainerInstance
    , containerInstance
    , ciStatus
    , ciRunningTasksCount
    , ciRemainingResources
    , ciEc2InstanceId
    , ciContainerInstanceARN
    , ciAgentConnected
    , ciPendingTasksCount
    , ciRegisteredResources

    -- * ContainerOverride
    , ContainerOverride
    , containerOverride
    , coCommand
    , coName

    -- * ContainerService
    , ContainerService
    , containerService
    , csStatus
    , csRunningCount
    , csClusterARN
    , csDesiredCount
    , csLoadBalancers
    , csPendingCount
    , csEvents
    , csServiceName
    , csDeployments
    , csTaskDefinition
    , csServiceARN
    , csRoleARN

    -- * Deployment
    , Deployment
    , deployment
    , depStatus
    , depRunningCount
    , depCreatedAt
    , depDesiredCount
    , depPendingCount
    , depId
    , depTaskDefinition
    , depUpdatedAt

    -- * Failure
    , Failure
    , failure
    , faiArn
    , faiReason

    -- * HostVolumeProperties
    , HostVolumeProperties
    , hostVolumeProperties
    , hvpSourcePath

    -- * KeyValuePair
    , KeyValuePair
    , keyValuePair
    , kvpValue
    , kvpName

    -- * LoadBalancer
    , LoadBalancer
    , loadBalancer
    , lbLoadBalancerName
    , lbContainerName
    , lbContainerPort

    -- * MountPoint
    , MountPoint
    , mountPoint
    , mpContainerPath
    , mpSourceVolume
    , mpReadOnly

    -- * NetworkBinding
    , NetworkBinding
    , networkBinding
    , nbBindIP
    , nbHostPort
    , nbContainerPort

    -- * PortMapping
    , PortMapping
    , portMapping
    , pmHostPort
    , pmContainerPort

    -- * Resource
    , Resource
    , resource
    , resStringSetValue
    , resIntegerValue
    , resDoubleValue
    , resLongValue
    , resName
    , resType

    -- * ServiceEvent
    , ServiceEvent
    , serviceEvent
    , seCreatedAt
    , seId
    , seMessage

    -- * Task
    , Task
    , task
    , tasDesiredStatus
    , tasClusterARN
    , tasOverrides
    , tasTaskARN
    , tasContainerInstanceARN
    , tasLastStatus
    , tasContainers
    , tasStartedBy
    , tasTaskDefinitionARN

    -- * TaskDefinition
    , TaskDefinition
    , taskDefinition
    , tdFamily
    , tdContainerDefinitions
    , tdTaskDefinitionARN
    , tdRevision
    , tdVolumes

    -- * TaskOverride
    , TaskOverride
    , taskOverride
    , toContainerOverrides

    -- * VersionInfo
    , VersionInfo
    , versionInfo
    , viAgentVersion
    , viAgentHash
    , viDockerVersion

    -- * Volume
    , Volume
    , volume
    , volName
    , volHost

    -- * VolumeFrom
    , VolumeFrom
    , volumeFrom
    , vfSourceContainer
    , vfReadOnly
    ) where

import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | Version @2014-11-13@ of the Amazon EC2 Container Service SDK.
data ECS

instance AWSService ECS where
    type Sg ECS = V4
    type Er ECS = JSONError

    service = service'
      where
        service' :: Service ECS
        service' = Service
            { _svcAbbrev  = "ECS"
            , _svcPrefix  = "ecs"
            , _svcVersion = "2014-11-13"
            , _svcHandle  = handle
            , _svcRetry   = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError JSONError)
        handle = jsonError statusSuccess service'

        retry :: Retry ECS
        retry = undefined

        check :: Status
              -> JSONError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e) = undefined

-- | /See:/ 'cluster' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cluStatus'
--
-- * 'cluClusterARN'
--
-- * 'cluRunningTasksCount'
--
-- * 'cluRegisteredContainerInstancesCount'
--
-- * 'cluPendingTasksCount'
--
-- * 'cluClusterName'
data Cluster = Cluster'{_cluStatus :: Maybe Text, _cluClusterARN :: Maybe Text, _cluRunningTasksCount :: Maybe Int, _cluRegisteredContainerInstancesCount :: Maybe Int, _cluPendingTasksCount :: Maybe Int, _cluClusterName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Cluster' smart constructor.
cluster :: Cluster
cluster = Cluster'{_cluStatus = Nothing, _cluClusterARN = Nothing, _cluRunningTasksCount = Nothing, _cluRegisteredContainerInstancesCount = Nothing, _cluPendingTasksCount = Nothing, _cluClusterName = Nothing};

-- | The status of the cluster. The valid values are @ACTIVE@ or @INACTIVE@.
-- @ACTIVE@ indicates that you can register container instances with the
-- cluster and the associated instances can accept tasks.
cluStatus :: Lens' Cluster (Maybe Text)
cluStatus = lens _cluStatus (\ s a -> s{_cluStatus = a});

-- | The Amazon Resource Name (ARN) that identifies the cluster. The ARN
-- contains the @arn:aws:ecs@ namespace, followed by the region of the
-- cluster, the AWS account ID of the cluster owner, the @cluster@
-- namespace, and then the cluster name. For example,
-- arn:aws:ecs:/region/:/012345678910/:cluster\//test/.
cluClusterARN :: Lens' Cluster (Maybe Text)
cluClusterARN = lens _cluClusterARN (\ s a -> s{_cluClusterARN = a});

-- | The number of tasks in the cluster that are in the @RUNNING@ state.
cluRunningTasksCount :: Lens' Cluster (Maybe Int)
cluRunningTasksCount = lens _cluRunningTasksCount (\ s a -> s{_cluRunningTasksCount = a});

-- | The number of container instances registered into the cluster.
cluRegisteredContainerInstancesCount :: Lens' Cluster (Maybe Int)
cluRegisteredContainerInstancesCount = lens _cluRegisteredContainerInstancesCount (\ s a -> s{_cluRegisteredContainerInstancesCount = a});

-- | The number of tasks in the cluster that are in the @PENDING@ state.
cluPendingTasksCount :: Lens' Cluster (Maybe Int)
cluPendingTasksCount = lens _cluPendingTasksCount (\ s a -> s{_cluPendingTasksCount = a});

-- | A user-generated string that you can use to identify your cluster.
cluClusterName :: Lens' Cluster (Maybe Text)
cluClusterName = lens _cluClusterName (\ s a -> s{_cluClusterName = a});

instance FromJSON Cluster where
        parseJSON
          = withObject "Cluster"
              (\ x ->
                 Cluster' <$>
                   (x .:? "status") <*> (x .:? "clusterArn") <*>
                     (x .:? "runningTasksCount")
                     <*> (x .:? "registeredContainerInstancesCount")
                     <*> (x .:? "pendingTasksCount")
                     <*> (x .:? "clusterName"))

-- | /See:/ 'container' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'conNetworkBindings'
--
-- * 'conContainerARN'
--
-- * 'conTaskARN'
--
-- * 'conLastStatus'
--
-- * 'conReason'
--
-- * 'conName'
--
-- * 'conExitCode'
data Container = Container'{_conNetworkBindings :: Maybe [NetworkBinding], _conContainerARN :: Maybe Text, _conTaskARN :: Maybe Text, _conLastStatus :: Maybe Text, _conReason :: Maybe Text, _conName :: Maybe Text, _conExitCode :: Maybe Int} deriving (Eq, Read, Show)

-- | 'Container' smart constructor.
container :: Container
container = Container'{_conNetworkBindings = Nothing, _conContainerARN = Nothing, _conTaskARN = Nothing, _conLastStatus = Nothing, _conReason = Nothing, _conName = Nothing, _conExitCode = Nothing};

-- | FIXME: Undocumented member.
conNetworkBindings :: Lens' Container [NetworkBinding]
conNetworkBindings = lens _conNetworkBindings (\ s a -> s{_conNetworkBindings = a}) . _Default;

-- | The Amazon Resource Name (ARN) of the container.
conContainerARN :: Lens' Container (Maybe Text)
conContainerARN = lens _conContainerARN (\ s a -> s{_conContainerARN = a});

-- | The Amazon Resource Name (ARN) of the task.
conTaskARN :: Lens' Container (Maybe Text)
conTaskARN = lens _conTaskARN (\ s a -> s{_conTaskARN = a});

-- | The last known status of the container.
conLastStatus :: Lens' Container (Maybe Text)
conLastStatus = lens _conLastStatus (\ s a -> s{_conLastStatus = a});

-- | A short (255 max characters) human-readable string to provide additional
-- detail about a running or stopped container.
conReason :: Lens' Container (Maybe Text)
conReason = lens _conReason (\ s a -> s{_conReason = a});

-- | The name of the container.
conName :: Lens' Container (Maybe Text)
conName = lens _conName (\ s a -> s{_conName = a});

-- | The exit code returned from the container.
conExitCode :: Lens' Container (Maybe Int)
conExitCode = lens _conExitCode (\ s a -> s{_conExitCode = a});

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

-- | /See:/ 'containerDefinition' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdImage'
--
-- * 'cdCommand'
--
-- * 'cdVolumesFrom'
--
-- * 'cdEnvironment'
--
-- * 'cdEntryPoint'
--
-- * 'cdPortMappings'
--
-- * 'cdMemory'
--
-- * 'cdName'
--
-- * 'cdMountPoints'
--
-- * 'cdLinks'
--
-- * 'cdEssential'
--
-- * 'cdCpu'
data ContainerDefinition = ContainerDefinition'{_cdImage :: Maybe Text, _cdCommand :: Maybe [Text], _cdVolumesFrom :: Maybe [VolumeFrom], _cdEnvironment :: Maybe [KeyValuePair], _cdEntryPoint :: Maybe [Text], _cdPortMappings :: Maybe [PortMapping], _cdMemory :: Maybe Int, _cdName :: Maybe Text, _cdMountPoints :: Maybe [MountPoint], _cdLinks :: Maybe [Text], _cdEssential :: Maybe Bool, _cdCpu :: Maybe Int} deriving (Eq, Read, Show)

-- | 'ContainerDefinition' smart constructor.
containerDefinition :: ContainerDefinition
containerDefinition = ContainerDefinition'{_cdImage = Nothing, _cdCommand = Nothing, _cdVolumesFrom = Nothing, _cdEnvironment = Nothing, _cdEntryPoint = Nothing, _cdPortMappings = Nothing, _cdMemory = Nothing, _cdName = Nothing, _cdMountPoints = Nothing, _cdLinks = Nothing, _cdEssential = Nothing, _cdCpu = Nothing};

-- | The image used to start a container. This string is passed directly to
-- the Docker daemon. Images in the Docker Hub registry are available by
-- default. Other repositories are specified with
-- @repository-url\/image:tag@.
cdImage :: Lens' ContainerDefinition (Maybe Text)
cdImage = lens _cdImage (\ s a -> s{_cdImage = a});

-- | The @CMD@ that is passed to the container. For more information on the
-- Docker @CMD@ parameter, see
-- <https://docs.docker.com/reference/builder/#cmd>.
cdCommand :: Lens' ContainerDefinition [Text]
cdCommand = lens _cdCommand (\ s a -> s{_cdCommand = a}) . _Default;

-- | Data volumes to mount from another container.
cdVolumesFrom :: Lens' ContainerDefinition [VolumeFrom]
cdVolumesFrom = lens _cdVolumesFrom (\ s a -> s{_cdVolumesFrom = a}) . _Default;

-- | The environment variables to pass to a container.
cdEnvironment :: Lens' ContainerDefinition [KeyValuePair]
cdEnvironment = lens _cdEnvironment (\ s a -> s{_cdEnvironment = a}) . _Default;

-- | Early versions of the Amazon ECS container agent do not properly handle
-- @entryPoint@ parameters. If you have problems using @entryPoint@, update
-- your container agent or enter your commands and arguments as @command@
-- array items instead.
--
-- The @ENTRYPOINT@ that is passed to the container. For more information
-- on the Docker @ENTRYPOINT@ parameter, see
-- <https://docs.docker.com/reference/builder/#entrypoint>.
cdEntryPoint :: Lens' ContainerDefinition [Text]
cdEntryPoint = lens _cdEntryPoint (\ s a -> s{_cdEntryPoint = a}) . _Default;

-- | The list of port mappings for the container.
cdPortMappings :: Lens' ContainerDefinition [PortMapping]
cdPortMappings = lens _cdPortMappings (\ s a -> s{_cdPortMappings = a}) . _Default;

-- | The number of MiB of memory reserved for the container. Docker will
-- allocate a minimum of 4 MiB of memory to a container.
cdMemory :: Lens' ContainerDefinition (Maybe Int)
cdMemory = lens _cdMemory (\ s a -> s{_cdMemory = a});

-- | The name of a container. If you are linking multiple containers together
-- in a task definition, the @name@ of one container can be entered in the
-- @links@ of another container to connect the containers.
cdName :: Lens' ContainerDefinition (Maybe Text)
cdName = lens _cdName (\ s a -> s{_cdName = a});

-- | The mount points for data volumes in your container.
cdMountPoints :: Lens' ContainerDefinition [MountPoint]
cdMountPoints = lens _cdMountPoints (\ s a -> s{_cdMountPoints = a}) . _Default;

-- | The @link@ parameter allows containers to communicate with each other
-- without the need for port mappings, using the @name@ parameter. For more
-- information on linking Docker containers, see
-- <https://docs.docker.com/userguide/dockerlinks/>.
cdLinks :: Lens' ContainerDefinition [Text]
cdLinks = lens _cdLinks (\ s a -> s{_cdLinks = a}) . _Default;

-- | If the @essential@ parameter of a container is marked as @true@, the
-- failure of that container will stop the task. If the @essential@
-- parameter of a container is marked as @false@, then its failure will not
-- affect the rest of the containers in a task.
cdEssential :: Lens' ContainerDefinition (Maybe Bool)
cdEssential = lens _cdEssential (\ s a -> s{_cdEssential = a});

-- | The number of @cpu@ units reserved for the container. A container
-- instance has 1,024 @cpu@ units for every CPU core.
cdCpu :: Lens' ContainerDefinition (Maybe Int)
cdCpu = lens _cdCpu (\ s a -> s{_cdCpu = a});

instance FromJSON ContainerDefinition where
        parseJSON
          = withObject "ContainerDefinition"
              (\ x ->
                 ContainerDefinition' <$>
                   (x .:? "image") <*> (x .:? "command" .!= mempty) <*>
                     (x .:? "volumesFrom" .!= mempty)
                     <*> (x .:? "environment" .!= mempty)
                     <*> (x .:? "entryPoint" .!= mempty)
                     <*> (x .:? "portMappings" .!= mempty)
                     <*> (x .:? "memory")
                     <*> (x .:? "name")
                     <*> (x .:? "mountPoints" .!= mempty)
                     <*> (x .:? "links" .!= mempty)
                     <*> (x .:? "essential")
                     <*> (x .:? "cpu"))

instance ToJSON ContainerDefinition where
        toJSON ContainerDefinition'{..}
          = object
              ["image" .= _cdImage, "command" .= _cdCommand,
               "volumesFrom" .= _cdVolumesFrom,
               "environment" .= _cdEnvironment,
               "entryPoint" .= _cdEntryPoint,
               "portMappings" .= _cdPortMappings,
               "memory" .= _cdMemory, "name" .= _cdName,
               "mountPoints" .= _cdMountPoints, "links" .= _cdLinks,
               "essential" .= _cdEssential, "cpu" .= _cdCpu]

-- | /See:/ 'containerInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
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
-- * 'ciPendingTasksCount'
--
-- * 'ciRegisteredResources'
data ContainerInstance = ContainerInstance'{_ciStatus :: Maybe Text, _ciRunningTasksCount :: Maybe Int, _ciRemainingResources :: Maybe [Resource], _ciEc2InstanceId :: Maybe Text, _ciContainerInstanceARN :: Maybe Text, _ciAgentConnected :: Maybe Bool, _ciPendingTasksCount :: Maybe Int, _ciRegisteredResources :: Maybe [Resource]} deriving (Eq, Read, Show)

-- | 'ContainerInstance' smart constructor.
containerInstance :: ContainerInstance
containerInstance = ContainerInstance'{_ciStatus = Nothing, _ciRunningTasksCount = Nothing, _ciRemainingResources = Nothing, _ciEc2InstanceId = Nothing, _ciContainerInstanceARN = Nothing, _ciAgentConnected = Nothing, _ciPendingTasksCount = Nothing, _ciRegisteredResources = Nothing};

-- | The status of the container instance. The valid values are @ACTIVE@ or
-- @INACTIVE@. @ACTIVE@ indicates that the container instance can accept
-- tasks.
ciStatus :: Lens' ContainerInstance (Maybe Text)
ciStatus = lens _ciStatus (\ s a -> s{_ciStatus = a});

-- | The number of tasks on the container instance that are in the @RUNNING@
-- status.
ciRunningTasksCount :: Lens' ContainerInstance (Maybe Int)
ciRunningTasksCount = lens _ciRunningTasksCount (\ s a -> s{_ciRunningTasksCount = a});

-- | The remaining resources of the container instance that are available for
-- new tasks.
ciRemainingResources :: Lens' ContainerInstance [Resource]
ciRemainingResources = lens _ciRemainingResources (\ s a -> s{_ciRemainingResources = a}) . _Default;

-- | The Amazon EC2 instance ID of the container instance.
ciEc2InstanceId :: Lens' ContainerInstance (Maybe Text)
ciEc2InstanceId = lens _ciEc2InstanceId (\ s a -> s{_ciEc2InstanceId = a});

-- | The Amazon Resource Name (ARN) of the container instance. The ARN
-- contains the @arn:aws:ecs@ namespace, followed by the region of the
-- container instance, the AWS account ID of the container instance owner,
-- the @container-instance@ namespace, and then the container instance
-- UUID. For example,
-- arn:aws:ecs:/region/:/aws_account_id/:container-instance\//container_instance_UUID/.
ciContainerInstanceARN :: Lens' ContainerInstance (Maybe Text)
ciContainerInstanceARN = lens _ciContainerInstanceARN (\ s a -> s{_ciContainerInstanceARN = a});

-- | This parameter returns @true@ if the agent is actually connected to
-- Amazon ECS. Registered instances with an agent that may be unhealthy or
-- stopped will return @false@, and instances without a connected agent
-- cannot accept placement request.
ciAgentConnected :: Lens' ContainerInstance (Maybe Bool)
ciAgentConnected = lens _ciAgentConnected (\ s a -> s{_ciAgentConnected = a});

-- | The number of tasks on the container instance that are in the @PENDING@
-- status.
ciPendingTasksCount :: Lens' ContainerInstance (Maybe Int)
ciPendingTasksCount = lens _ciPendingTasksCount (\ s a -> s{_ciPendingTasksCount = a});

-- | The registered resources on the container instance that are in use by
-- current tasks.
ciRegisteredResources :: Lens' ContainerInstance [Resource]
ciRegisteredResources = lens _ciRegisteredResources (\ s a -> s{_ciRegisteredResources = a}) . _Default;

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
                     <*> (x .:? "pendingTasksCount")
                     <*> (x .:? "registeredResources" .!= mempty))

-- | /See:/ 'containerOverride' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'coCommand'
--
-- * 'coName'
data ContainerOverride = ContainerOverride'{_coCommand :: Maybe [Text], _coName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ContainerOverride' smart constructor.
containerOverride :: ContainerOverride
containerOverride = ContainerOverride'{_coCommand = Nothing, _coName = Nothing};

-- | The command to send to the container that overrides the default command
-- from the Docker image or the task definition.
coCommand :: Lens' ContainerOverride [Text]
coCommand = lens _coCommand (\ s a -> s{_coCommand = a}) . _Default;

-- | The name of the container that receives the override.
coName :: Lens' ContainerOverride (Maybe Text)
coName = lens _coName (\ s a -> s{_coName = a});

instance FromJSON ContainerOverride where
        parseJSON
          = withObject "ContainerOverride"
              (\ x ->
                 ContainerOverride' <$>
                   (x .:? "command" .!= mempty) <*> (x .:? "name"))

instance ToJSON ContainerOverride where
        toJSON ContainerOverride'{..}
          = object ["command" .= _coCommand, "name" .= _coName]

-- | /See:/ 'containerService' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csStatus'
--
-- * 'csRunningCount'
--
-- * 'csClusterARN'
--
-- * 'csDesiredCount'
--
-- * 'csLoadBalancers'
--
-- * 'csPendingCount'
--
-- * 'csEvents'
--
-- * 'csServiceName'
--
-- * 'csDeployments'
--
-- * 'csTaskDefinition'
--
-- * 'csServiceARN'
--
-- * 'csRoleARN'
data ContainerService = ContainerService'{_csStatus :: Maybe Text, _csRunningCount :: Maybe Int, _csClusterARN :: Maybe Text, _csDesiredCount :: Maybe Int, _csLoadBalancers :: Maybe [LoadBalancer], _csPendingCount :: Maybe Int, _csEvents :: Maybe [ServiceEvent], _csServiceName :: Maybe Text, _csDeployments :: Maybe [Deployment], _csTaskDefinition :: Maybe Text, _csServiceARN :: Maybe Text, _csRoleARN :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ContainerService' smart constructor.
containerService :: ContainerService
containerService = ContainerService'{_csStatus = Nothing, _csRunningCount = Nothing, _csClusterARN = Nothing, _csDesiredCount = Nothing, _csLoadBalancers = Nothing, _csPendingCount = Nothing, _csEvents = Nothing, _csServiceName = Nothing, _csDeployments = Nothing, _csTaskDefinition = Nothing, _csServiceARN = Nothing, _csRoleARN = Nothing};

-- | The status of the service. The valid values are @ACTIVE@, @DRAINING@, or
-- @INACTIVE@.
csStatus :: Lens' ContainerService (Maybe Text)
csStatus = lens _csStatus (\ s a -> s{_csStatus = a});

-- | The number of tasks in the cluster that are in the @RUNNING@ state.
csRunningCount :: Lens' ContainerService (Maybe Int)
csRunningCount = lens _csRunningCount (\ s a -> s{_csRunningCount = a});

-- | The Amazon Resource Name (ARN) of the of the cluster that hosts the
-- service.
csClusterARN :: Lens' ContainerService (Maybe Text)
csClusterARN = lens _csClusterARN (\ s a -> s{_csClusterARN = a});

-- | The desired number of instantiations of the task definition to keep
-- running on the service. This value is specified when the service is
-- created with CreateService, and it can be modified with UpdateService.
csDesiredCount :: Lens' ContainerService (Maybe Int)
csDesiredCount = lens _csDesiredCount (\ s a -> s{_csDesiredCount = a});

-- | A list of load balancer objects, containing the load balancer name, the
-- container name (as it appears in a container definition), and the
-- container port to access from the load balancer.
csLoadBalancers :: Lens' ContainerService [LoadBalancer]
csLoadBalancers = lens _csLoadBalancers (\ s a -> s{_csLoadBalancers = a}) . _Default;

-- | The number of tasks in the cluster that are in the @PENDING@ state.
csPendingCount :: Lens' ContainerService (Maybe Int)
csPendingCount = lens _csPendingCount (\ s a -> s{_csPendingCount = a});

-- | The event stream for your service. A maximum of 100 of the latest events
-- are displayed.
csEvents :: Lens' ContainerService [ServiceEvent]
csEvents = lens _csEvents (\ s a -> s{_csEvents = a}) . _Default;

-- | A user-generated string that you can use to identify your service.
csServiceName :: Lens' ContainerService (Maybe Text)
csServiceName = lens _csServiceName (\ s a -> s{_csServiceName = a});

-- | The current state of deployments for the service.
csDeployments :: Lens' ContainerService [Deployment]
csDeployments = lens _csDeployments (\ s a -> s{_csDeployments = a}) . _Default;

-- | The task definition to use for tasks in the service. This value is
-- specified when the service is created with CreateService, and it can be
-- modified with UpdateService.
csTaskDefinition :: Lens' ContainerService (Maybe Text)
csTaskDefinition = lens _csTaskDefinition (\ s a -> s{_csTaskDefinition = a});

-- | The Amazon Resource Name (ARN) that identifies the service. The ARN
-- contains the @arn:aws:ecs@ namespace, followed by the region of the
-- service, the AWS account ID of the service owner, the @service@
-- namespace, and then the service name. For example,
-- arn:aws:ecs:/region/:/012345678910/:service\//my-service/.
csServiceARN :: Lens' ContainerService (Maybe Text)
csServiceARN = lens _csServiceARN (\ s a -> s{_csServiceARN = a});

-- | The Amazon Resource Name (ARN) of the IAM role associated with the
-- service that allows the Amazon ECS container agent to register container
-- instances with a load balancer.
csRoleARN :: Lens' ContainerService (Maybe Text)
csRoleARN = lens _csRoleARN (\ s a -> s{_csRoleARN = a});

instance FromJSON ContainerService where
        parseJSON
          = withObject "ContainerService"
              (\ x ->
                 ContainerService' <$>
                   (x .:? "status") <*> (x .:? "runningCount") <*>
                     (x .:? "clusterArn")
                     <*> (x .:? "desiredCount")
                     <*> (x .:? "loadBalancers" .!= mempty)
                     <*> (x .:? "pendingCount")
                     <*> (x .:? "events" .!= mempty)
                     <*> (x .:? "serviceName")
                     <*> (x .:? "deployments" .!= mempty)
                     <*> (x .:? "taskDefinition")
                     <*> (x .:? "serviceArn")
                     <*> (x .:? "roleArn"))

-- | /See:/ 'deployment' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'depStatus'
--
-- * 'depRunningCount'
--
-- * 'depCreatedAt'
--
-- * 'depDesiredCount'
--
-- * 'depPendingCount'
--
-- * 'depId'
--
-- * 'depTaskDefinition'
--
-- * 'depUpdatedAt'
data Deployment = Deployment'{_depStatus :: Maybe Text, _depRunningCount :: Maybe Int, _depCreatedAt :: Maybe POSIX, _depDesiredCount :: Maybe Int, _depPendingCount :: Maybe Int, _depId :: Maybe Text, _depTaskDefinition :: Maybe Text, _depUpdatedAt :: Maybe POSIX} deriving (Eq, Read, Show)

-- | 'Deployment' smart constructor.
deployment :: Deployment
deployment = Deployment'{_depStatus = Nothing, _depRunningCount = Nothing, _depCreatedAt = Nothing, _depDesiredCount = Nothing, _depPendingCount = Nothing, _depId = Nothing, _depTaskDefinition = Nothing, _depUpdatedAt = Nothing};

-- | The status of the deployment. Valid values are @PRIMARY@ (for the most
-- recent deployment), @ACTIVE@ (for previous deployments that still have
-- tasks running, but are being replaced with the @PRIMARY@ deployment),
-- and @INACTIVE@ (for deployments that have been completely replaced).
depStatus :: Lens' Deployment (Maybe Text)
depStatus = lens _depStatus (\ s a -> s{_depStatus = a});

-- | The number of tasks in the deployment that are in the @RUNNING@ status.
depRunningCount :: Lens' Deployment (Maybe Int)
depRunningCount = lens _depRunningCount (\ s a -> s{_depRunningCount = a});

-- | The Unix time in seconds and milliseconds when the service was created.
depCreatedAt :: Lens' Deployment (Maybe UTCTime)
depCreatedAt = lens _depCreatedAt (\ s a -> s{_depCreatedAt = a}) . mapping _Time;

-- | The most recent desired count of tasks that was specified for the
-- service to deploy and\/or maintain.
depDesiredCount :: Lens' Deployment (Maybe Int)
depDesiredCount = lens _depDesiredCount (\ s a -> s{_depDesiredCount = a});

-- | The number of tasks in the deployment that are in the @PENDING@ status.
depPendingCount :: Lens' Deployment (Maybe Int)
depPendingCount = lens _depPendingCount (\ s a -> s{_depPendingCount = a});

-- | The ID of the deployment.
depId :: Lens' Deployment (Maybe Text)
depId = lens _depId (\ s a -> s{_depId = a});

-- | The most recent task definition that was specified for the service to
-- use.
depTaskDefinition :: Lens' Deployment (Maybe Text)
depTaskDefinition = lens _depTaskDefinition (\ s a -> s{_depTaskDefinition = a});

-- | The Unix time in seconds and milliseconds when the service was last
-- updated.
depUpdatedAt :: Lens' Deployment (Maybe UTCTime)
depUpdatedAt = lens _depUpdatedAt (\ s a -> s{_depUpdatedAt = a}) . mapping _Time;

instance FromJSON Deployment where
        parseJSON
          = withObject "Deployment"
              (\ x ->
                 Deployment' <$>
                   (x .:? "status") <*> (x .:? "runningCount") <*>
                     (x .:? "createdAt")
                     <*> (x .:? "desiredCount")
                     <*> (x .:? "pendingCount")
                     <*> (x .:? "id")
                     <*> (x .:? "taskDefinition")
                     <*> (x .:? "updatedAt"))

-- | /See:/ 'failure' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'faiArn'
--
-- * 'faiReason'
data Failure = Failure'{_faiArn :: Maybe Text, _faiReason :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Failure' smart constructor.
failure :: Failure
failure = Failure'{_faiArn = Nothing, _faiReason = Nothing};

-- | The Amazon Resource Name (ARN) of the failed resource.
faiArn :: Lens' Failure (Maybe Text)
faiArn = lens _faiArn (\ s a -> s{_faiArn = a});

-- | The reason for the failure.
faiReason :: Lens' Failure (Maybe Text)
faiReason = lens _faiReason (\ s a -> s{_faiReason = a});

instance FromJSON Failure where
        parseJSON
          = withObject "Failure"
              (\ x ->
                 Failure' <$> (x .:? "arn") <*> (x .:? "reason"))

-- | /See:/ 'hostVolumeProperties' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hvpSourcePath'
newtype HostVolumeProperties = HostVolumeProperties'{_hvpSourcePath :: Maybe Text} deriving (Eq, Read, Show)

-- | 'HostVolumeProperties' smart constructor.
hostVolumeProperties :: HostVolumeProperties
hostVolumeProperties = HostVolumeProperties'{_hvpSourcePath = Nothing};

-- | The path on the host container instance that is presented to the
-- container. If this parameter is empty, then the Docker daemon has
-- assigned a host path for you.
hvpSourcePath :: Lens' HostVolumeProperties (Maybe Text)
hvpSourcePath = lens _hvpSourcePath (\ s a -> s{_hvpSourcePath = a});

instance FromJSON HostVolumeProperties where
        parseJSON
          = withObject "HostVolumeProperties"
              (\ x ->
                 HostVolumeProperties' <$> (x .:? "sourcePath"))

instance ToJSON HostVolumeProperties where
        toJSON HostVolumeProperties'{..}
          = object ["sourcePath" .= _hvpSourcePath]

-- | /See:/ 'keyValuePair' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'kvpValue'
--
-- * 'kvpName'
data KeyValuePair = KeyValuePair'{_kvpValue :: Maybe Text, _kvpName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'KeyValuePair' smart constructor.
keyValuePair :: KeyValuePair
keyValuePair = KeyValuePair'{_kvpValue = Nothing, _kvpName = Nothing};

-- | The value of the key value pair.
kvpValue :: Lens' KeyValuePair (Maybe Text)
kvpValue = lens _kvpValue (\ s a -> s{_kvpValue = a});

-- | The name of the key value pair.
kvpName :: Lens' KeyValuePair (Maybe Text)
kvpName = lens _kvpName (\ s a -> s{_kvpName = a});

instance FromJSON KeyValuePair where
        parseJSON
          = withObject "KeyValuePair"
              (\ x ->
                 KeyValuePair' <$> (x .:? "value") <*> (x .:? "name"))

instance ToJSON KeyValuePair where
        toJSON KeyValuePair'{..}
          = object ["value" .= _kvpValue, "name" .= _kvpName]

-- | /See:/ 'loadBalancer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lbLoadBalancerName'
--
-- * 'lbContainerName'
--
-- * 'lbContainerPort'
data LoadBalancer = LoadBalancer'{_lbLoadBalancerName :: Maybe Text, _lbContainerName :: Maybe Text, _lbContainerPort :: Maybe Int} deriving (Eq, Read, Show)

-- | 'LoadBalancer' smart constructor.
loadBalancer :: LoadBalancer
loadBalancer = LoadBalancer'{_lbLoadBalancerName = Nothing, _lbContainerName = Nothing, _lbContainerPort = Nothing};

-- | The name of the load balancer.
lbLoadBalancerName :: Lens' LoadBalancer (Maybe Text)
lbLoadBalancerName = lens _lbLoadBalancerName (\ s a -> s{_lbLoadBalancerName = a});

-- | The name of the container to associate with the load balancer.
lbContainerName :: Lens' LoadBalancer (Maybe Text)
lbContainerName = lens _lbContainerName (\ s a -> s{_lbContainerName = a});

-- | The port on the container to associate with the load balancer.
lbContainerPort :: Lens' LoadBalancer (Maybe Int)
lbContainerPort = lens _lbContainerPort (\ s a -> s{_lbContainerPort = a});

instance FromJSON LoadBalancer where
        parseJSON
          = withObject "LoadBalancer"
              (\ x ->
                 LoadBalancer' <$>
                   (x .:? "loadBalancerName") <*>
                     (x .:? "containerName")
                     <*> (x .:? "containerPort"))

instance ToJSON LoadBalancer where
        toJSON LoadBalancer'{..}
          = object
              ["loadBalancerName" .= _lbLoadBalancerName,
               "containerName" .= _lbContainerName,
               "containerPort" .= _lbContainerPort]

-- | /See:/ 'mountPoint' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mpContainerPath'
--
-- * 'mpSourceVolume'
--
-- * 'mpReadOnly'
data MountPoint = MountPoint'{_mpContainerPath :: Maybe Text, _mpSourceVolume :: Maybe Text, _mpReadOnly :: Maybe Bool} deriving (Eq, Read, Show)

-- | 'MountPoint' smart constructor.
mountPoint :: MountPoint
mountPoint = MountPoint'{_mpContainerPath = Nothing, _mpSourceVolume = Nothing, _mpReadOnly = Nothing};

-- | The path on the container to mount the host volume at.
mpContainerPath :: Lens' MountPoint (Maybe Text)
mpContainerPath = lens _mpContainerPath (\ s a -> s{_mpContainerPath = a});

-- | The name of the volume to mount.
mpSourceVolume :: Lens' MountPoint (Maybe Text)
mpSourceVolume = lens _mpSourceVolume (\ s a -> s{_mpSourceVolume = a});

-- | If this value is @true@, the container has read-only access to the
-- volume. If this value is @false@, then the container can write to the
-- volume. The default value is @false@.
mpReadOnly :: Lens' MountPoint (Maybe Bool)
mpReadOnly = lens _mpReadOnly (\ s a -> s{_mpReadOnly = a});

instance FromJSON MountPoint where
        parseJSON
          = withObject "MountPoint"
              (\ x ->
                 MountPoint' <$>
                   (x .:? "containerPath") <*> (x .:? "sourceVolume")
                     <*> (x .:? "readOnly"))

instance ToJSON MountPoint where
        toJSON MountPoint'{..}
          = object
              ["containerPath" .= _mpContainerPath,
               "sourceVolume" .= _mpSourceVolume,
               "readOnly" .= _mpReadOnly]

-- | /See:/ 'networkBinding' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'nbBindIP'
--
-- * 'nbHostPort'
--
-- * 'nbContainerPort'
data NetworkBinding = NetworkBinding'{_nbBindIP :: Maybe Text, _nbHostPort :: Maybe Int, _nbContainerPort :: Maybe Int} deriving (Eq, Read, Show)

-- | 'NetworkBinding' smart constructor.
networkBinding :: NetworkBinding
networkBinding = NetworkBinding'{_nbBindIP = Nothing, _nbHostPort = Nothing, _nbContainerPort = Nothing};

-- | The IP address that the container is bound to on the container instance.
nbBindIP :: Lens' NetworkBinding (Maybe Text)
nbBindIP = lens _nbBindIP (\ s a -> s{_nbBindIP = a});

-- | The port number on the host that is used with the network binding.
nbHostPort :: Lens' NetworkBinding (Maybe Int)
nbHostPort = lens _nbHostPort (\ s a -> s{_nbHostPort = a});

-- | The port number on the container that is be used with the network
-- binding.
nbContainerPort :: Lens' NetworkBinding (Maybe Int)
nbContainerPort = lens _nbContainerPort (\ s a -> s{_nbContainerPort = a});

instance FromJSON NetworkBinding where
        parseJSON
          = withObject "NetworkBinding"
              (\ x ->
                 NetworkBinding' <$>
                   (x .:? "bindIP") <*> (x .:? "hostPort") <*>
                     (x .:? "containerPort"))

instance ToJSON NetworkBinding where
        toJSON NetworkBinding'{..}
          = object
              ["bindIP" .= _nbBindIP, "hostPort" .= _nbHostPort,
               "containerPort" .= _nbContainerPort]

-- | /See:/ 'portMapping' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pmHostPort'
--
-- * 'pmContainerPort'
data PortMapping = PortMapping'{_pmHostPort :: Maybe Int, _pmContainerPort :: Maybe Int} deriving (Eq, Read, Show)

-- | 'PortMapping' smart constructor.
portMapping :: PortMapping
portMapping = PortMapping'{_pmHostPort = Nothing, _pmContainerPort = Nothing};

-- | The port number on the container instance to reserve for your container.
-- You can specify a non-reserved host port for your container port
-- mapping, or you can omit the @hostPort@ while specifying a
-- @containerPort@ and your container will automatically receive a port in
-- the 49153 to 65535 port range. You should not attempt to specify a host
-- port in the 49153 to 65535 port range, since these are reserved for
-- automatic assignment.
--
-- The default reserved ports are 22 for SSH, the Docker ports 2375 and
-- 2376, and the Amazon ECS Container Agent port 51678. Any host port that
-- was previously specified in a running task is also reserved while the
-- task is running (once a task stops, the host port is released).The
-- current reserved ports are displayed in the @remainingResources@ of
-- DescribeContainerInstances output, and a container instance may have up
-- to 50 reserved ports at a time, including the default reserved ports
-- (automatically assigned ports do not count toward this limit).
pmHostPort :: Lens' PortMapping (Maybe Int)
pmHostPort = lens _pmHostPort (\ s a -> s{_pmHostPort = a});

-- | The port number on the container that is bound to the user-specified or
-- automatically assigned host port. If you specify a container port and
-- not a host port, your container will automatically receive a host port
-- in the 49153 to 65535 port range.
pmContainerPort :: Lens' PortMapping (Maybe Int)
pmContainerPort = lens _pmContainerPort (\ s a -> s{_pmContainerPort = a});

instance FromJSON PortMapping where
        parseJSON
          = withObject "PortMapping"
              (\ x ->
                 PortMapping' <$>
                   (x .:? "hostPort") <*> (x .:? "containerPort"))

instance ToJSON PortMapping where
        toJSON PortMapping'{..}
          = object
              ["hostPort" .= _pmHostPort,
               "containerPort" .= _pmContainerPort]

-- | /See:/ 'resource' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'resStringSetValue'
--
-- * 'resIntegerValue'
--
-- * 'resDoubleValue'
--
-- * 'resLongValue'
--
-- * 'resName'
--
-- * 'resType'
data Resource = Resource'{_resStringSetValue :: Maybe [Text], _resIntegerValue :: Maybe Int, _resDoubleValue :: Maybe Double, _resLongValue :: Maybe Integer, _resName :: Maybe Text, _resType :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Resource' smart constructor.
resource :: Resource
resource = Resource'{_resStringSetValue = Nothing, _resIntegerValue = Nothing, _resDoubleValue = Nothing, _resLongValue = Nothing, _resName = Nothing, _resType = Nothing};

-- | When the @stringSetValue@ type is set, the value of the resource must be
-- a string type.
resStringSetValue :: Lens' Resource [Text]
resStringSetValue = lens _resStringSetValue (\ s a -> s{_resStringSetValue = a}) . _Default;

-- | When the @integerValue@ type is set, the value of the resource must be
-- an integer.
resIntegerValue :: Lens' Resource (Maybe Int)
resIntegerValue = lens _resIntegerValue (\ s a -> s{_resIntegerValue = a});

-- | When the @doubleValue@ type is set, the value of the resource must be a
-- double precision floating-point type.
resDoubleValue :: Lens' Resource (Maybe Double)
resDoubleValue = lens _resDoubleValue (\ s a -> s{_resDoubleValue = a});

-- | When the @longValue@ type is set, the value of the resource must be an
-- extended precision floating-point type.
resLongValue :: Lens' Resource (Maybe Integer)
resLongValue = lens _resLongValue (\ s a -> s{_resLongValue = a});

-- | The name of the resource, such as @CPU@, @MEMORY@, @PORTS@, or a
-- user-defined resource.
resName :: Lens' Resource (Maybe Text)
resName = lens _resName (\ s a -> s{_resName = a});

-- | The type of the resource, such as @INTEGER@, @DOUBLE@, @LONG@, or
-- @STRINGSET@.
resType :: Lens' Resource (Maybe Text)
resType = lens _resType (\ s a -> s{_resType = a});

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

instance ToJSON Resource where
        toJSON Resource'{..}
          = object
              ["stringSetValue" .= _resStringSetValue,
               "integerValue" .= _resIntegerValue,
               "doubleValue" .= _resDoubleValue,
               "longValue" .= _resLongValue, "name" .= _resName,
               "type" .= _resType]

-- | /See:/ 'serviceEvent' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'seCreatedAt'
--
-- * 'seId'
--
-- * 'seMessage'
data ServiceEvent = ServiceEvent'{_seCreatedAt :: Maybe POSIX, _seId :: Maybe Text, _seMessage :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ServiceEvent' smart constructor.
serviceEvent :: ServiceEvent
serviceEvent = ServiceEvent'{_seCreatedAt = Nothing, _seId = Nothing, _seMessage = Nothing};

-- | The Unix time in seconds and milliseconds when the event was triggered.
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

-- | /See:/ 'task' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tasDesiredStatus'
--
-- * 'tasClusterARN'
--
-- * 'tasOverrides'
--
-- * 'tasTaskARN'
--
-- * 'tasContainerInstanceARN'
--
-- * 'tasLastStatus'
--
-- * 'tasContainers'
--
-- * 'tasStartedBy'
--
-- * 'tasTaskDefinitionARN'
data Task = Task'{_tasDesiredStatus :: Maybe Text, _tasClusterARN :: Maybe Text, _tasOverrides :: Maybe TaskOverride, _tasTaskARN :: Maybe Text, _tasContainerInstanceARN :: Maybe Text, _tasLastStatus :: Maybe Text, _tasContainers :: Maybe [Container], _tasStartedBy :: Maybe Text, _tasTaskDefinitionARN :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Task' smart constructor.
task :: Task
task = Task'{_tasDesiredStatus = Nothing, _tasClusterARN = Nothing, _tasOverrides = Nothing, _tasTaskARN = Nothing, _tasContainerInstanceARN = Nothing, _tasLastStatus = Nothing, _tasContainers = Nothing, _tasStartedBy = Nothing, _tasTaskDefinitionARN = Nothing};

-- | The desired status of the task.
tasDesiredStatus :: Lens' Task (Maybe Text)
tasDesiredStatus = lens _tasDesiredStatus (\ s a -> s{_tasDesiredStatus = a});

-- | The Amazon Resource Name (ARN) of the of the cluster that hosts the
-- task.
tasClusterARN :: Lens' Task (Maybe Text)
tasClusterARN = lens _tasClusterARN (\ s a -> s{_tasClusterARN = a});

-- | One or more container overrides.
tasOverrides :: Lens' Task (Maybe TaskOverride)
tasOverrides = lens _tasOverrides (\ s a -> s{_tasOverrides = a});

-- | The Amazon Resource Name (ARN) of the task.
tasTaskARN :: Lens' Task (Maybe Text)
tasTaskARN = lens _tasTaskARN (\ s a -> s{_tasTaskARN = a});

-- | The Amazon Resource Name (ARN) of the container instances that host the
-- task.
tasContainerInstanceARN :: Lens' Task (Maybe Text)
tasContainerInstanceARN = lens _tasContainerInstanceARN (\ s a -> s{_tasContainerInstanceARN = a});

-- | The last known status of the task.
tasLastStatus :: Lens' Task (Maybe Text)
tasLastStatus = lens _tasLastStatus (\ s a -> s{_tasLastStatus = a});

-- | The containers associated with the task.
tasContainers :: Lens' Task [Container]
tasContainers = lens _tasContainers (\ s a -> s{_tasContainers = a}) . _Default;

-- | The tag specified when a task is started. If the task is started by an
-- Amazon ECS service, then the @startedBy@ parameter contains the
-- deployment ID of the service that starts it.
tasStartedBy :: Lens' Task (Maybe Text)
tasStartedBy = lens _tasStartedBy (\ s a -> s{_tasStartedBy = a});

-- | The Amazon Resource Name (ARN) of the of the task definition that
-- creates the task.
tasTaskDefinitionARN :: Lens' Task (Maybe Text)
tasTaskDefinitionARN = lens _tasTaskDefinitionARN (\ s a -> s{_tasTaskDefinitionARN = a});

instance FromJSON Task where
        parseJSON
          = withObject "Task"
              (\ x ->
                 Task' <$>
                   (x .:? "desiredStatus") <*> (x .:? "clusterArn") <*>
                     (x .:? "overrides")
                     <*> (x .:? "taskArn")
                     <*> (x .:? "containerInstanceArn")
                     <*> (x .:? "lastStatus")
                     <*> (x .:? "containers" .!= mempty)
                     <*> (x .:? "startedBy")
                     <*> (x .:? "taskDefinitionArn"))

-- | /See:/ 'taskDefinition' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tdFamily'
--
-- * 'tdContainerDefinitions'
--
-- * 'tdTaskDefinitionARN'
--
-- * 'tdRevision'
--
-- * 'tdVolumes'
data TaskDefinition = TaskDefinition'{_tdFamily :: Maybe Text, _tdContainerDefinitions :: Maybe [ContainerDefinition], _tdTaskDefinitionARN :: Maybe Text, _tdRevision :: Maybe Int, _tdVolumes :: Maybe [Volume]} deriving (Eq, Read, Show)

-- | 'TaskDefinition' smart constructor.
taskDefinition :: TaskDefinition
taskDefinition = TaskDefinition'{_tdFamily = Nothing, _tdContainerDefinitions = Nothing, _tdTaskDefinitionARN = Nothing, _tdRevision = Nothing, _tdVolumes = Nothing};

-- | The family of your task definition. You can think of the @family@ as the
-- name of your task definition.
tdFamily :: Lens' TaskDefinition (Maybe Text)
tdFamily = lens _tdFamily (\ s a -> s{_tdFamily = a});

-- | A list of container definitions in JSON format that describe the
-- different containers that make up your task. For more information on
-- container definition parameters and defaults, see
-- <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Amazon ECS Task Definitions>
-- in the /Amazon EC2 Container Service Developer Guide/.
tdContainerDefinitions :: Lens' TaskDefinition [ContainerDefinition]
tdContainerDefinitions = lens _tdContainerDefinitions (\ s a -> s{_tdContainerDefinitions = a}) . _Default;

-- | The full Amazon Resource Name (ARN) of the of the task definition.
tdTaskDefinitionARN :: Lens' TaskDefinition (Maybe Text)
tdTaskDefinitionARN = lens _tdTaskDefinitionARN (\ s a -> s{_tdTaskDefinitionARN = a});

-- | The revision of the task in a particular family. You can think of the
-- revision as a version number of a task definition in a family. When you
-- register a task definition for the first time, the revision is @1@, and
-- each time you register a task definition in the same family, the
-- revision value increases by one.
tdRevision :: Lens' TaskDefinition (Maybe Int)
tdRevision = lens _tdRevision (\ s a -> s{_tdRevision = a});

-- | The list of volumes in a task. For more information on volume definition
-- parameters and defaults, see
-- <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Amazon ECS Task Definitions>
-- in the /Amazon EC2 Container Service Developer Guide/.
tdVolumes :: Lens' TaskDefinition [Volume]
tdVolumes = lens _tdVolumes (\ s a -> s{_tdVolumes = a}) . _Default;

instance FromJSON TaskDefinition where
        parseJSON
          = withObject "TaskDefinition"
              (\ x ->
                 TaskDefinition' <$>
                   (x .:? "family") <*>
                     (x .:? "containerDefinitions" .!= mempty)
                     <*> (x .:? "taskDefinitionArn")
                     <*> (x .:? "revision")
                     <*> (x .:? "volumes" .!= mempty))

-- | /See:/ 'taskOverride' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'toContainerOverrides'
newtype TaskOverride = TaskOverride'{_toContainerOverrides :: Maybe [ContainerOverride]} deriving (Eq, Read, Show)

-- | 'TaskOverride' smart constructor.
taskOverride :: TaskOverride
taskOverride = TaskOverride'{_toContainerOverrides = Nothing};

-- | One or more container overrides sent to a task.
toContainerOverrides :: Lens' TaskOverride [ContainerOverride]
toContainerOverrides = lens _toContainerOverrides (\ s a -> s{_toContainerOverrides = a}) . _Default;

instance FromJSON TaskOverride where
        parseJSON
          = withObject "TaskOverride"
              (\ x ->
                 TaskOverride' <$>
                   (x .:? "containerOverrides" .!= mempty))

instance ToJSON TaskOverride where
        toJSON TaskOverride'{..}
          = object
              ["containerOverrides" .= _toContainerOverrides]

-- | /See:/ 'versionInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'viAgentVersion'
--
-- * 'viAgentHash'
--
-- * 'viDockerVersion'
data VersionInfo = VersionInfo'{_viAgentVersion :: Maybe Text, _viAgentHash :: Maybe Text, _viDockerVersion :: Maybe Text} deriving (Eq, Read, Show)

-- | 'VersionInfo' smart constructor.
versionInfo :: VersionInfo
versionInfo = VersionInfo'{_viAgentVersion = Nothing, _viAgentHash = Nothing, _viDockerVersion = Nothing};

-- | The version number of the Amazon ECS container agent.
viAgentVersion :: Lens' VersionInfo (Maybe Text)
viAgentVersion = lens _viAgentVersion (\ s a -> s{_viAgentVersion = a});

-- | The Git commit hash for the Amazon ECS container agent build on the
-- <https://github.com/aws/amazon-ecs-agent/commits/master amazon-ecs-agent>
-- GitHub repository.
viAgentHash :: Lens' VersionInfo (Maybe Text)
viAgentHash = lens _viAgentHash (\ s a -> s{_viAgentHash = a});

-- | The Docker version running on the container instance.
viDockerVersion :: Lens' VersionInfo (Maybe Text)
viDockerVersion = lens _viDockerVersion (\ s a -> s{_viDockerVersion = a});

instance ToJSON VersionInfo where
        toJSON VersionInfo'{..}
          = object
              ["agentVersion" .= _viAgentVersion,
               "agentHash" .= _viAgentHash,
               "dockerVersion" .= _viDockerVersion]

-- | /See:/ 'volume' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'volName'
--
-- * 'volHost'
data Volume = Volume'{_volName :: Maybe Text, _volHost :: Maybe HostVolumeProperties} deriving (Eq, Read, Show)

-- | 'Volume' smart constructor.
volume :: Volume
volume = Volume'{_volName = Nothing, _volHost = Nothing};

-- | The name of the volume. This name is referenced in the @sourceVolume@
-- parameter of container definition @mountPoints@.
volName :: Lens' Volume (Maybe Text)
volName = lens _volName (\ s a -> s{_volName = a});

-- | The path on the host container instance that is presented to the
-- containers which access the volume. If this parameter is empty, then the
-- Docker daemon assigns a host path for you.
volHost :: Lens' Volume (Maybe HostVolumeProperties)
volHost = lens _volHost (\ s a -> s{_volHost = a});

instance FromJSON Volume where
        parseJSON
          = withObject "Volume"
              (\ x ->
                 Volume' <$> (x .:? "name") <*> (x .:? "host"))

instance ToJSON Volume where
        toJSON Volume'{..}
          = object ["name" .= _volName, "host" .= _volHost]

-- | /See:/ 'volumeFrom' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vfSourceContainer'
--
-- * 'vfReadOnly'
data VolumeFrom = VolumeFrom'{_vfSourceContainer :: Maybe Text, _vfReadOnly :: Maybe Bool} deriving (Eq, Read, Show)

-- | 'VolumeFrom' smart constructor.
volumeFrom :: VolumeFrom
volumeFrom = VolumeFrom'{_vfSourceContainer = Nothing, _vfReadOnly = Nothing};

-- | The name of the container to mount volumes from.
vfSourceContainer :: Lens' VolumeFrom (Maybe Text)
vfSourceContainer = lens _vfSourceContainer (\ s a -> s{_vfSourceContainer = a});

-- | If this value is @true@, the container has read-only access to the
-- volume. If this value is @false@, then the container can write to the
-- volume. The default value is @false@.
vfReadOnly :: Lens' VolumeFrom (Maybe Bool)
vfReadOnly = lens _vfReadOnly (\ s a -> s{_vfReadOnly = a});

instance FromJSON VolumeFrom where
        parseJSON
          = withObject "VolumeFrom"
              (\ x ->
                 VolumeFrom' <$>
                   (x .:? "sourceContainer") <*> (x .:? "readOnly"))

instance ToJSON VolumeFrom where
        toJSON VolumeFrom'{..}
          = object
              ["sourceContainer" .= _vfSourceContainer,
               "readOnly" .= _vfReadOnly]
