{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.Product where

import Network.AWS.ECS.Types.Sum
import Network.AWS.Prelude

-- | A regional grouping of one or more container instances on which you can
-- run task requests. Each account receives a default cluster the first
-- time you use the Amazon ECS service, but you may also create other
-- clusters. Clusters may contain more than one instance type
-- simultaneously.
--
-- /See:/ 'cluster' smart constructor.
--
-- The fields accessible through corresponding lenses are:
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
data Cluster = Cluster'
    { _cStatus :: !(Maybe Text)
    , _cClusterARN :: !(Maybe Text)
    , _cRunningTasksCount :: !(Maybe Int)
    , _cRegisteredContainerInstancesCount :: !(Maybe Int)
    , _cPendingTasksCount :: !(Maybe Int)
    , _cClusterName :: !(Maybe Text)
    , _cActiveServicesCount :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Cluster' smart constructor.
cluster :: Cluster
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

-- | The status of the cluster. The valid values are @ACTIVE@ or @INACTIVE@.
-- @ACTIVE@ indicates that you can register container instances with the
-- cluster and the associated instances can accept tasks.
cStatus :: Lens' Cluster (Maybe Text)
cStatus = lens _cStatus (\ s a -> s{_cStatus = a});

-- | The Amazon Resource Name (ARN) that identifies the cluster. The ARN
-- contains the @arn:aws:ecs@ namespace, followed by the region of the
-- cluster, the AWS account ID of the cluster owner, the @cluster@
-- namespace, and then the cluster name. For example,
-- arn:aws:ecs:/region/:/012345678910/:cluster\//test/.
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

-- | A user-generated string that you can use to identify your cluster.
cClusterName :: Lens' Cluster (Maybe Text)
cClusterName = lens _cClusterName (\ s a -> s{_cClusterName = a});

-- | The number of services that are running on the cluster in an @ACTIVE@
-- state. You can view these services with ListServices.
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

-- | A docker container that is part of a task.
--
-- /See:/ 'container' smart constructor.
--
-- The fields accessible through corresponding lenses are:
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
data Container = Container'
    { _cNetworkBindings :: !(Maybe [NetworkBinding])
    , _cContainerARN :: !(Maybe Text)
    , _cTaskARN :: !(Maybe Text)
    , _cLastStatus :: !(Maybe Text)
    , _cReason :: !(Maybe Text)
    , _cName :: !(Maybe Text)
    , _cExitCode :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Container' smart constructor.
container :: Container
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

-- | A short (255 max characters) human-readable string to provide additional
-- detail about a running or stopped container.
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

-- | Container definitions are used in task definitions to describe the
-- different containers that are launched as part of a task.
--
-- /See:/ 'containerDefinition' smart constructor.
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
data ContainerDefinition = ContainerDefinition'
    { _cdImage :: !(Maybe Text)
    , _cdCommand :: !(Maybe [Text])
    , _cdVolumesFrom :: !(Maybe [VolumeFrom])
    , _cdEnvironment :: !(Maybe [KeyValuePair])
    , _cdEntryPoint :: !(Maybe [Text])
    , _cdPortMappings :: !(Maybe [PortMapping])
    , _cdMemory :: !(Maybe Int)
    , _cdName :: !(Maybe Text)
    , _cdMountPoints :: !(Maybe [MountPoint])
    , _cdLinks :: !(Maybe [Text])
    , _cdEssential :: !(Maybe Bool)
    , _cdCpu :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ContainerDefinition' smart constructor.
containerDefinition :: ContainerDefinition
containerDefinition = 
    ContainerDefinition'
    { _cdImage = Nothing
    , _cdCommand = Nothing
    , _cdVolumesFrom = Nothing
    , _cdEnvironment = Nothing
    , _cdEntryPoint = Nothing
    , _cdPortMappings = Nothing
    , _cdMemory = Nothing
    , _cdName = Nothing
    , _cdMountPoints = Nothing
    , _cdLinks = Nothing
    , _cdEssential = Nothing
    , _cdCpu = Nothing
    }

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
cdCommand = lens _cdCommand (\ s a -> s{_cdCommand = a}) . _Default . _Coerce;

-- | Data volumes to mount from another container.
cdVolumesFrom :: Lens' ContainerDefinition [VolumeFrom]
cdVolumesFrom = lens _cdVolumesFrom (\ s a -> s{_cdVolumesFrom = a}) . _Default . _Coerce;

-- | The environment variables to pass to a container.
cdEnvironment :: Lens' ContainerDefinition [KeyValuePair]
cdEnvironment = lens _cdEnvironment (\ s a -> s{_cdEnvironment = a}) . _Default . _Coerce;

-- | Early versions of the Amazon ECS container agent do not properly handle
-- @entryPoint@ parameters. If you have problems using @entryPoint@, update
-- your container agent or enter your commands and arguments as @command@
-- array items instead.
--
-- The @ENTRYPOINT@ that is passed to the container. For more information
-- on the Docker @ENTRYPOINT@ parameter, see
-- <https://docs.docker.com/reference/builder/#entrypoint>.
cdEntryPoint :: Lens' ContainerDefinition [Text]
cdEntryPoint = lens _cdEntryPoint (\ s a -> s{_cdEntryPoint = a}) . _Default . _Coerce;

-- | The list of port mappings for the container.
cdPortMappings :: Lens' ContainerDefinition [PortMapping]
cdPortMappings = lens _cdPortMappings (\ s a -> s{_cdPortMappings = a}) . _Default . _Coerce;

-- | The number of MiB of memory reserved for the container. If your
-- container attempts to exceed the memory allocated here, the container is
-- killed.
cdMemory :: Lens' ContainerDefinition (Maybe Int)
cdMemory = lens _cdMemory (\ s a -> s{_cdMemory = a});

-- | The name of a container. If you are linking multiple containers together
-- in a task definition, the @name@ of one container can be entered in the
-- @links@ of another container to connect the containers.
cdName :: Lens' ContainerDefinition (Maybe Text)
cdName = lens _cdName (\ s a -> s{_cdName = a});

-- | The mount points for data volumes in your container.
cdMountPoints :: Lens' ContainerDefinition [MountPoint]
cdMountPoints = lens _cdMountPoints (\ s a -> s{_cdMountPoints = a}) . _Default . _Coerce;

-- | The @link@ parameter allows containers to communicate with each other
-- without the need for port mappings, using the @name@ parameter. The
-- @name:internalName@ construct is analogous to @name:alias@ in Docker
-- links. For more information on linking Docker containers, see
-- <https://docs.docker.com/userguide/dockerlinks/>.
--
-- Containers that are collocated on a single container instance may be
-- able to communicate with each other without requiring links or host port
-- mappings. Network isolation is achieved on the container instance using
-- security groups and VPC settings.
cdLinks :: Lens' ContainerDefinition [Text]
cdLinks = lens _cdLinks (\ s a -> s{_cdLinks = a}) . _Default . _Coerce;

-- | If the @essential@ parameter of a container is marked as @true@, the
-- failure of that container will stop the task. If the @essential@
-- parameter of a container is marked as @false@, then its failure will not
-- affect the rest of the containers in a task. If this parameter is
-- omitted, a container is assumed to be essential.
--
-- All tasks must have at least one essential container.
cdEssential :: Lens' ContainerDefinition (Maybe Bool)
cdEssential = lens _cdEssential (\ s a -> s{_cdEssential = a});

-- | The number of @cpu@ units reserved for the container. A container
-- instance has 1,024 @cpu@ units for every CPU core. This parameter
-- specifies the minimum amount of CPU to reserve for a container, and
-- containers share unallocated CPU units with other containers on the
-- instance with the same ratio as their allocated amount.
--
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
-- The Docker daemon on the container instance uses the CPU value to
-- calculate the relative CPU share ratios for running containers. For more
-- information, see
-- <https://docs.docker.com/reference/run/#cpu-share-constraint CPU share constraint>
-- in the Docker documentation. The minimum valid CPU share value that the
-- Linux kernel will allow is 2; however, the CPU parameter is not
-- required, and you can use CPU values below 2 in your container
-- definitions. For CPU values below 2 (including null), the behavior
-- varies based on your Amazon ECS container agent version:
--
-- -   __Agent versions less than or equal to 1.1.0:__ Null and zero CPU
--     values are passed to Docker as 0, which Docker then converts to
--     1,024 CPU shares. CPU values of 1 are passed to Docker as 1, which
--     the Linux kernel converts to 2 CPU shares.
-- -   __Agent versions greater than or equal to 1.2.0:__ Null, zero, and
--     CPU values of 1 are passed to Docker as 2.
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

-- | An Amazon EC2 instance that is running the Amazon ECS agent and has been
-- registered with a cluster.
--
-- /See:/ 'containerInstance' smart constructor.
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
-- * 'ciVersionInfo'
--
-- * 'ciAgentUpdateStatus'
--
-- * 'ciPendingTasksCount'
--
-- * 'ciRegisteredResources'
data ContainerInstance = ContainerInstance'
    { _ciStatus :: !(Maybe Text)
    , _ciRunningTasksCount :: !(Maybe Int)
    , _ciRemainingResources :: !(Maybe [Resource])
    , _ciEc2InstanceId :: !(Maybe Text)
    , _ciContainerInstanceARN :: !(Maybe Text)
    , _ciAgentConnected :: !(Maybe Bool)
    , _ciVersionInfo :: !(Maybe VersionInfo)
    , _ciAgentUpdateStatus :: !(Maybe AgentUpdateStatus)
    , _ciPendingTasksCount :: !(Maybe Int)
    , _ciRegisteredResources :: !(Maybe [Resource])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ContainerInstance' smart constructor.
containerInstance :: ContainerInstance
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
    , _ciPendingTasksCount = Nothing
    , _ciRegisteredResources = Nothing
    }

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
ciRemainingResources = lens _ciRemainingResources (\ s a -> s{_ciRemainingResources = a}) . _Default . _Coerce;

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

-- | The version information for the Amazon ECS container agent and Docker
-- daemon running on the container instance.
ciVersionInfo :: Lens' ContainerInstance (Maybe VersionInfo)
ciVersionInfo = lens _ciVersionInfo (\ s a -> s{_ciVersionInfo = a});

-- | The status of the most recent agent update. If an update has never been
-- requested, this value is @NULL@.
ciAgentUpdateStatus :: Lens' ContainerInstance (Maybe AgentUpdateStatus)
ciAgentUpdateStatus = lens _ciAgentUpdateStatus (\ s a -> s{_ciAgentUpdateStatus = a});

-- | The number of tasks on the container instance that are in the @PENDING@
-- status.
ciPendingTasksCount :: Lens' ContainerInstance (Maybe Int)
ciPendingTasksCount = lens _ciPendingTasksCount (\ s a -> s{_ciPendingTasksCount = a});

-- | The registered resources on the container instance that are in use by
-- current tasks.
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
                     <*> (x .:? "pendingTasksCount")
                     <*> (x .:? "registeredResources" .!= mempty))

-- | The overrides that should be sent to a container.
--
-- /See:/ 'containerOverride' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'coCommand'
--
-- * 'coEnvironment'
--
-- * 'coName'
data ContainerOverride = ContainerOverride'
    { _coCommand :: !(Maybe [Text])
    , _coEnvironment :: !(Maybe [KeyValuePair])
    , _coName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ContainerOverride' smart constructor.
containerOverride :: ContainerOverride
containerOverride = 
    ContainerOverride'
    { _coCommand = Nothing
    , _coEnvironment = Nothing
    , _coName = Nothing
    }

-- | The command to send to the container that overrides the default command
-- from the Docker image or the task definition.
coCommand :: Lens' ContainerOverride [Text]
coCommand = lens _coCommand (\ s a -> s{_coCommand = a}) . _Default . _Coerce;

-- | The environment variables to send to the container. You can add new
-- environment variables, which are added to the container at launch, or
-- you can override the existing environment variables from the Docker
-- image or the task definition.
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

instance ToJSON ContainerOverride where
        toJSON ContainerOverride'{..}
          = object
              ["command" .= _coCommand,
               "environment" .= _coEnvironment, "name" .= _coName]

-- | Details on a service within a cluster
--
-- /See:/ 'containerService' smart constructor.
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
data ContainerService = ContainerService'
    { _csStatus :: !(Maybe Text)
    , _csRunningCount :: !(Maybe Int)
    , _csClusterARN :: !(Maybe Text)
    , _csDesiredCount :: !(Maybe Int)
    , _csLoadBalancers :: !(Maybe [LoadBalancer])
    , _csPendingCount :: !(Maybe Int)
    , _csEvents :: !(Maybe [ServiceEvent])
    , _csServiceName :: !(Maybe Text)
    , _csDeployments :: !(Maybe [Deployment])
    , _csTaskDefinition :: !(Maybe Text)
    , _csServiceARN :: !(Maybe Text)
    , _csRoleARN :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ContainerService' smart constructor.
containerService :: ContainerService
containerService = 
    ContainerService'
    { _csStatus = Nothing
    , _csRunningCount = Nothing
    , _csClusterARN = Nothing
    , _csDesiredCount = Nothing
    , _csLoadBalancers = Nothing
    , _csPendingCount = Nothing
    , _csEvents = Nothing
    , _csServiceName = Nothing
    , _csDeployments = Nothing
    , _csTaskDefinition = Nothing
    , _csServiceARN = Nothing
    , _csRoleARN = Nothing
    }

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
csLoadBalancers = lens _csLoadBalancers (\ s a -> s{_csLoadBalancers = a}) . _Default . _Coerce;

-- | The number of tasks in the cluster that are in the @PENDING@ state.
csPendingCount :: Lens' ContainerService (Maybe Int)
csPendingCount = lens _csPendingCount (\ s a -> s{_csPendingCount = a});

-- | The event stream for your service. A maximum of 100 of the latest events
-- are displayed.
csEvents :: Lens' ContainerService [ServiceEvent]
csEvents = lens _csEvents (\ s a -> s{_csEvents = a}) . _Default . _Coerce;

-- | A user-generated string that you can use to identify your service.
csServiceName :: Lens' ContainerService (Maybe Text)
csServiceName = lens _csServiceName (\ s a -> s{_csServiceName = a});

-- | The current state of deployments for the service.
csDeployments :: Lens' ContainerService [Deployment]
csDeployments = lens _csDeployments (\ s a -> s{_csDeployments = a}) . _Default . _Coerce;

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

-- | The details of an Amazon ECS service deployment.
--
-- /See:/ 'deployment' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dStatus'
--
-- * 'dRunningCount'
--
-- * 'dCreatedAt'
--
-- * 'dDesiredCount'
--
-- * 'dPendingCount'
--
-- * 'dId'
--
-- * 'dTaskDefinition'
--
-- * 'dUpdatedAt'
data Deployment = Deployment'
    { _dStatus :: !(Maybe Text)
    , _dRunningCount :: !(Maybe Int)
    , _dCreatedAt :: !(Maybe POSIX)
    , _dDesiredCount :: !(Maybe Int)
    , _dPendingCount :: !(Maybe Int)
    , _dId :: !(Maybe Text)
    , _dTaskDefinition :: !(Maybe Text)
    , _dUpdatedAt :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Deployment' smart constructor.
deployment :: Deployment
deployment = 
    Deployment'
    { _dStatus = Nothing
    , _dRunningCount = Nothing
    , _dCreatedAt = Nothing
    , _dDesiredCount = Nothing
    , _dPendingCount = Nothing
    , _dId = Nothing
    , _dTaskDefinition = Nothing
    , _dUpdatedAt = Nothing
    }

-- | The status of the deployment. Valid values are @PRIMARY@ (for the most
-- recent deployment), @ACTIVE@ (for previous deployments that still have
-- tasks running, but are being replaced with the @PRIMARY@ deployment),
-- and @INACTIVE@ (for deployments that have been completely replaced).
dStatus :: Lens' Deployment (Maybe Text)
dStatus = lens _dStatus (\ s a -> s{_dStatus = a});

-- | The number of tasks in the deployment that are in the @RUNNING@ status.
dRunningCount :: Lens' Deployment (Maybe Int)
dRunningCount = lens _dRunningCount (\ s a -> s{_dRunningCount = a});

-- | The Unix time in seconds and milliseconds when the service was created.
dCreatedAt :: Lens' Deployment (Maybe UTCTime)
dCreatedAt = lens _dCreatedAt (\ s a -> s{_dCreatedAt = a}) . mapping _Time;

-- | The most recent desired count of tasks that was specified for the
-- service to deploy and\/or maintain.
dDesiredCount :: Lens' Deployment (Maybe Int)
dDesiredCount = lens _dDesiredCount (\ s a -> s{_dDesiredCount = a});

-- | The number of tasks in the deployment that are in the @PENDING@ status.
dPendingCount :: Lens' Deployment (Maybe Int)
dPendingCount = lens _dPendingCount (\ s a -> s{_dPendingCount = a});

-- | The ID of the deployment.
dId :: Lens' Deployment (Maybe Text)
dId = lens _dId (\ s a -> s{_dId = a});

-- | The most recent task definition that was specified for the service to
-- use.
dTaskDefinition :: Lens' Deployment (Maybe Text)
dTaskDefinition = lens _dTaskDefinition (\ s a -> s{_dTaskDefinition = a});

-- | The Unix time in seconds and milliseconds when the service was last
-- updated.
dUpdatedAt :: Lens' Deployment (Maybe UTCTime)
dUpdatedAt = lens _dUpdatedAt (\ s a -> s{_dUpdatedAt = a}) . mapping _Time;

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

-- | A failed resource.
--
-- /See:/ 'failure' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fArn'
--
-- * 'fReason'
data Failure = Failure'
    { _fArn :: !(Maybe Text)
    , _fReason :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Failure' smart constructor.
failure :: Failure
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

-- | Details on a container instance host volume.
--
-- /See:/ 'hostVolumeProperties' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hvpSourcePath'
newtype HostVolumeProperties = HostVolumeProperties'
    { _hvpSourcePath :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'HostVolumeProperties' smart constructor.
hostVolumeProperties :: HostVolumeProperties
hostVolumeProperties = 
    HostVolumeProperties'
    { _hvpSourcePath = Nothing
    }

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

-- | A key and value pair object.
--
-- /See:/ 'keyValuePair' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'kvpValue'
--
-- * 'kvpName'
data KeyValuePair = KeyValuePair'
    { _kvpValue :: !(Maybe Text)
    , _kvpName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'KeyValuePair' smart constructor.
keyValuePair :: KeyValuePair
keyValuePair = 
    KeyValuePair'
    { _kvpValue = Nothing
    , _kvpName = Nothing
    }

-- | The value of the key value pair. For environment variables, this is the
-- value of the environment variable.
kvpValue :: Lens' KeyValuePair (Maybe Text)
kvpValue = lens _kvpValue (\ s a -> s{_kvpValue = a});

-- | The name of the key value pair. For environment variables, this is the
-- name of the environment variable.
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

-- | Details on a load balancer that is used with a service.
--
-- /See:/ 'loadBalancer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lbLoadBalancerName'
--
-- * 'lbContainerName'
--
-- * 'lbContainerPort'
data LoadBalancer = LoadBalancer'
    { _lbLoadBalancerName :: !(Maybe Text)
    , _lbContainerName :: !(Maybe Text)
    , _lbContainerPort :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'LoadBalancer' smart constructor.
loadBalancer :: LoadBalancer
loadBalancer = 
    LoadBalancer'
    { _lbLoadBalancerName = Nothing
    , _lbContainerName = Nothing
    , _lbContainerPort = Nothing
    }

-- | The name of the load balancer.
lbLoadBalancerName :: Lens' LoadBalancer (Maybe Text)
lbLoadBalancerName = lens _lbLoadBalancerName (\ s a -> s{_lbLoadBalancerName = a});

-- | The name of the container to associate with the load balancer.
lbContainerName :: Lens' LoadBalancer (Maybe Text)
lbContainerName = lens _lbContainerName (\ s a -> s{_lbContainerName = a});

-- | The port on the container to associate with the load balancer. This port
-- must correspond to a @containerPort@ in the service\'s task definition.
-- Your container instances must allow ingress traffic on the @hostPort@ of
-- the port mapping.
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

-- | Details on a volume mount point that is used in a container definition.
--
-- /See:/ 'mountPoint' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mpContainerPath'
--
-- * 'mpSourceVolume'
--
-- * 'mpReadOnly'
data MountPoint = MountPoint'
    { _mpContainerPath :: !(Maybe Text)
    , _mpSourceVolume :: !(Maybe Text)
    , _mpReadOnly :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'MountPoint' smart constructor.
mountPoint :: MountPoint
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

-- | Details on the network bindings between a container and its host
-- container instance.
--
-- /See:/ 'networkBinding' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'nbBindIP'
--
-- * 'nbProtocol'
--
-- * 'nbHostPort'
--
-- * 'nbContainerPort'
data NetworkBinding = NetworkBinding'
    { _nbBindIP :: !(Maybe Text)
    , _nbProtocol :: !(Maybe TransportProtocol)
    , _nbHostPort :: !(Maybe Int)
    , _nbContainerPort :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'NetworkBinding' smart constructor.
networkBinding :: NetworkBinding
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

-- | The port number on the container that is be used with the network
-- binding.
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

instance ToJSON NetworkBinding where
        toJSON NetworkBinding'{..}
          = object
              ["bindIP" .= _nbBindIP, "protocol" .= _nbProtocol,
               "hostPort" .= _nbHostPort,
               "containerPort" .= _nbContainerPort]

-- | Port mappings allow containers to access ports on the host container
-- instance to send or receive traffic. Port mappings are specified as part
-- of the container definition.
--
-- /See:/ 'portMapping' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pmProtocol'
--
-- * 'pmHostPort'
--
-- * 'pmContainerPort'
data PortMapping = PortMapping'
    { _pmProtocol :: !(Maybe TransportProtocol)
    , _pmHostPort :: !(Maybe Int)
    , _pmContainerPort :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PortMapping' smart constructor.
portMapping :: PortMapping
portMapping = 
    PortMapping'
    { _pmProtocol = Nothing
    , _pmHostPort = Nothing
    , _pmContainerPort = Nothing
    }

-- | The protocol used for the port mapping. Valid values are @tcp@ and
-- @udp@. The default is @tcp@.
pmProtocol :: Lens' PortMapping (Maybe TransportProtocol)
pmProtocol = lens _pmProtocol (\ s a -> s{_pmProtocol = a});

-- | The port number on the container instance to reserve for your container.
-- You can specify a non-reserved host port for your container port
-- mapping, or you can omit the @hostPort@ (or set it to @0@) while
-- specifying a @containerPort@ and your container will automatically
-- receive a port in the ephemeral port range for your container instance
-- operating system and Docker version.
--
-- The default ephemeral port range is 49153 to 65535, and this range is
-- used for Docker versions prior to 1.6.0. For Docker version 1.6.0 and
-- later, the Docker daemon tries to read the ephemeral port range from
-- @\/proc\/sys\/net\/ipv4\/ip_local_port_range@; if this kernel parameter
-- is unavailable, the default ephemeral port range is used. You should not
-- attempt to specify a host port in the ephemeral port range, since these
-- are reserved for automatic assignment. In general, ports below 32768 are
-- outside of the ephemeral port range.
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
-- in the ephemeral port range (for more information, see @hostPort@).
pmContainerPort :: Lens' PortMapping (Maybe Int)
pmContainerPort = lens _pmContainerPort (\ s a -> s{_pmContainerPort = a});

instance FromJSON PortMapping where
        parseJSON
          = withObject "PortMapping"
              (\ x ->
                 PortMapping' <$>
                   (x .:? "protocol") <*> (x .:? "hostPort") <*>
                     (x .:? "containerPort"))

instance ToJSON PortMapping where
        toJSON PortMapping'{..}
          = object
              ["protocol" .= _pmProtocol,
               "hostPort" .= _pmHostPort,
               "containerPort" .= _pmContainerPort]

-- | Describes the resources available for a container instance.
--
-- /See:/ 'resource' smart constructor.
--
-- The fields accessible through corresponding lenses are:
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
data Resource = Resource'
    { _rStringSetValue :: !(Maybe [Text])
    , _rIntegerValue :: !(Maybe Int)
    , _rDoubleValue :: !(Maybe Double)
    , _rLongValue :: !(Maybe Integer)
    , _rName :: !(Maybe Text)
    , _rType :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Resource' smart constructor.
resource :: Resource
resource = 
    Resource'
    { _rStringSetValue = Nothing
    , _rIntegerValue = Nothing
    , _rDoubleValue = Nothing
    , _rLongValue = Nothing
    , _rName = Nothing
    , _rType = Nothing
    }

-- | When the @stringSetValue@ type is set, the value of the resource must be
-- a string type.
rStringSetValue :: Lens' Resource [Text]
rStringSetValue = lens _rStringSetValue (\ s a -> s{_rStringSetValue = a}) . _Default . _Coerce;

-- | When the @integerValue@ type is set, the value of the resource must be
-- an integer.
rIntegerValue :: Lens' Resource (Maybe Int)
rIntegerValue = lens _rIntegerValue (\ s a -> s{_rIntegerValue = a});

-- | When the @doubleValue@ type is set, the value of the resource must be a
-- double precision floating-point type.
rDoubleValue :: Lens' Resource (Maybe Double)
rDoubleValue = lens _rDoubleValue (\ s a -> s{_rDoubleValue = a});

-- | When the @longValue@ type is set, the value of the resource must be an
-- extended precision floating-point type.
rLongValue :: Lens' Resource (Maybe Integer)
rLongValue = lens _rLongValue (\ s a -> s{_rLongValue = a});

-- | The name of the resource, such as @CPU@, @MEMORY@, @PORTS@, or a
-- user-defined resource.
rName :: Lens' Resource (Maybe Text)
rName = lens _rName (\ s a -> s{_rName = a});

-- | The type of the resource, such as @INTEGER@, @DOUBLE@, @LONG@, or
-- @STRINGSET@.
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

instance ToJSON Resource where
        toJSON Resource'{..}
          = object
              ["stringSetValue" .= _rStringSetValue,
               "integerValue" .= _rIntegerValue,
               "doubleValue" .= _rDoubleValue,
               "longValue" .= _rLongValue, "name" .= _rName,
               "type" .= _rType]

-- | Details on an event associated with a service.
--
-- /See:/ 'serviceEvent' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'seCreatedAt'
--
-- * 'seId'
--
-- * 'seMessage'
data ServiceEvent = ServiceEvent'
    { _seCreatedAt :: !(Maybe POSIX)
    , _seId :: !(Maybe Text)
    , _seMessage :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ServiceEvent' smart constructor.
serviceEvent :: ServiceEvent
serviceEvent = 
    ServiceEvent'
    { _seCreatedAt = Nothing
    , _seId = Nothing
    , _seMessage = Nothing
    }

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

-- | Details on a task in a cluster.
--
-- /See:/ 'task' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tDesiredStatus'
--
-- * 'tClusterARN'
--
-- * 'tOverrides'
--
-- * 'tTaskARN'
--
-- * 'tContainerInstanceARN'
--
-- * 'tLastStatus'
--
-- * 'tContainers'
--
-- * 'tStartedBy'
--
-- * 'tTaskDefinitionARN'
data Task = Task'
    { _tDesiredStatus :: !(Maybe Text)
    , _tClusterARN :: !(Maybe Text)
    , _tOverrides :: !(Maybe TaskOverride)
    , _tTaskARN :: !(Maybe Text)
    , _tContainerInstanceARN :: !(Maybe Text)
    , _tLastStatus :: !(Maybe Text)
    , _tContainers :: !(Maybe [Container])
    , _tStartedBy :: !(Maybe Text)
    , _tTaskDefinitionARN :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Task' smart constructor.
task :: Task
task = 
    Task'
    { _tDesiredStatus = Nothing
    , _tClusterARN = Nothing
    , _tOverrides = Nothing
    , _tTaskARN = Nothing
    , _tContainerInstanceARN = Nothing
    , _tLastStatus = Nothing
    , _tContainers = Nothing
    , _tStartedBy = Nothing
    , _tTaskDefinitionARN = Nothing
    }

-- | The desired status of the task.
tDesiredStatus :: Lens' Task (Maybe Text)
tDesiredStatus = lens _tDesiredStatus (\ s a -> s{_tDesiredStatus = a});

-- | The Amazon Resource Name (ARN) of the of the cluster that hosts the
-- task.
tClusterARN :: Lens' Task (Maybe Text)
tClusterARN = lens _tClusterARN (\ s a -> s{_tClusterARN = a});

-- | One or more container overrides.
tOverrides :: Lens' Task (Maybe TaskOverride)
tOverrides = lens _tOverrides (\ s a -> s{_tOverrides = a});

-- | The Amazon Resource Name (ARN) of the task.
tTaskARN :: Lens' Task (Maybe Text)
tTaskARN = lens _tTaskARN (\ s a -> s{_tTaskARN = a});

-- | The Amazon Resource Name (ARN) of the container instances that host the
-- task.
tContainerInstanceARN :: Lens' Task (Maybe Text)
tContainerInstanceARN = lens _tContainerInstanceARN (\ s a -> s{_tContainerInstanceARN = a});

-- | The last known status of the task.
tLastStatus :: Lens' Task (Maybe Text)
tLastStatus = lens _tLastStatus (\ s a -> s{_tLastStatus = a});

-- | The containers associated with the task.
tContainers :: Lens' Task [Container]
tContainers = lens _tContainers (\ s a -> s{_tContainers = a}) . _Default . _Coerce;

-- | The tag specified when a task is started. If the task is started by an
-- Amazon ECS service, then the @startedBy@ parameter contains the
-- deployment ID of the service that starts it.
tStartedBy :: Lens' Task (Maybe Text)
tStartedBy = lens _tStartedBy (\ s a -> s{_tStartedBy = a});

-- | The Amazon Resource Name (ARN) of the of the task definition that
-- creates the task.
tTaskDefinitionARN :: Lens' Task (Maybe Text)
tTaskDefinitionARN = lens _tTaskDefinitionARN (\ s a -> s{_tTaskDefinitionARN = a});

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

-- | Details of a task definition.
--
-- /See:/ 'taskDefinition' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tdStatus'
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
data TaskDefinition = TaskDefinition'
    { _tdStatus :: !(Maybe TaskDefinitionStatus)
    , _tdFamily :: !(Maybe Text)
    , _tdContainerDefinitions :: !(Maybe [ContainerDefinition])
    , _tdTaskDefinitionARN :: !(Maybe Text)
    , _tdRevision :: !(Maybe Int)
    , _tdVolumes :: !(Maybe [Volume])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TaskDefinition' smart constructor.
taskDefinition :: TaskDefinition
taskDefinition = 
    TaskDefinition'
    { _tdStatus = Nothing
    , _tdFamily = Nothing
    , _tdContainerDefinitions = Nothing
    , _tdTaskDefinitionARN = Nothing
    , _tdRevision = Nothing
    , _tdVolumes = Nothing
    }

-- | The status of the task definition.
tdStatus :: Lens' TaskDefinition (Maybe TaskDefinitionStatus)
tdStatus = lens _tdStatus (\ s a -> s{_tdStatus = a});

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
tdContainerDefinitions = lens _tdContainerDefinitions (\ s a -> s{_tdContainerDefinitions = a}) . _Default . _Coerce;

-- | The full Amazon Resource Name (ARN) of the of the task definition.
tdTaskDefinitionARN :: Lens' TaskDefinition (Maybe Text)
tdTaskDefinitionARN = lens _tdTaskDefinitionARN (\ s a -> s{_tdTaskDefinitionARN = a});

-- | The revision of the task in a particular family. You can think of the
-- revision as a version number of a task definition in a family. When you
-- register a task definition for the first time, the revision is @1@, and
-- each time you register a new revision of a task definition in the same
-- family, the revision value always increases by one (even if you have
-- deregistered previous revisions in this family).
tdRevision :: Lens' TaskDefinition (Maybe Int)
tdRevision = lens _tdRevision (\ s a -> s{_tdRevision = a});

-- | The list of volumes in a task. For more information on volume definition
-- parameters and defaults, see
-- <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Amazon ECS Task Definitions>
-- in the /Amazon EC2 Container Service Developer Guide/.
tdVolumes :: Lens' TaskDefinition [Volume]
tdVolumes = lens _tdVolumes (\ s a -> s{_tdVolumes = a}) . _Default . _Coerce;

instance FromJSON TaskDefinition where
        parseJSON
          = withObject "TaskDefinition"
              (\ x ->
                 TaskDefinition' <$>
                   (x .:? "status") <*> (x .:? "family") <*>
                     (x .:? "containerDefinitions" .!= mempty)
                     <*> (x .:? "taskDefinitionArn")
                     <*> (x .:? "revision")
                     <*> (x .:? "volumes" .!= mempty))

-- | The overrides associated with a task.
--
-- /See:/ 'taskOverride' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'toContainerOverrides'
newtype TaskOverride = TaskOverride'
    { _toContainerOverrides :: Maybe [ContainerOverride]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TaskOverride' smart constructor.
taskOverride :: TaskOverride
taskOverride = 
    TaskOverride'
    { _toContainerOverrides = Nothing
    }

-- | One or more container overrides sent to a task.
toContainerOverrides :: Lens' TaskOverride [ContainerOverride]
toContainerOverrides = lens _toContainerOverrides (\ s a -> s{_toContainerOverrides = a}) . _Default . _Coerce;

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

-- | The Docker and Amazon ECS container agent version information on a
-- container instance.
--
-- /See:/ 'versionInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'viAgentVersion'
--
-- * 'viAgentHash'
--
-- * 'viDockerVersion'
data VersionInfo = VersionInfo'
    { _viAgentVersion :: !(Maybe Text)
    , _viAgentHash :: !(Maybe Text)
    , _viDockerVersion :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'VersionInfo' smart constructor.
versionInfo :: VersionInfo
versionInfo = 
    VersionInfo'
    { _viAgentVersion = Nothing
    , _viAgentHash = Nothing
    , _viDockerVersion = Nothing
    }

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

instance FromJSON VersionInfo where
        parseJSON
          = withObject "VersionInfo"
              (\ x ->
                 VersionInfo' <$>
                   (x .:? "agentVersion") <*> (x .:? "agentHash") <*>
                     (x .:? "dockerVersion"))

instance ToJSON VersionInfo where
        toJSON VersionInfo'{..}
          = object
              ["agentVersion" .= _viAgentVersion,
               "agentHash" .= _viAgentHash,
               "dockerVersion" .= _viDockerVersion]

-- | A data volume used in a task definition.
--
-- /See:/ 'volume' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vName'
--
-- * 'vHost'
data Volume = Volume'
    { _vName :: !(Maybe Text)
    , _vHost :: !(Maybe HostVolumeProperties)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Volume' smart constructor.
volume :: Volume
volume = 
    Volume'
    { _vName = Nothing
    , _vHost = Nothing
    }

-- | The name of the volume. This name is referenced in the @sourceVolume@
-- parameter of container definition @mountPoints@.
vName :: Lens' Volume (Maybe Text)
vName = lens _vName (\ s a -> s{_vName = a});

-- | The path on the host container instance that is presented to the
-- containers which access the volume. If this parameter is empty, then the
-- Docker daemon assigns a host path for you.
vHost :: Lens' Volume (Maybe HostVolumeProperties)
vHost = lens _vHost (\ s a -> s{_vHost = a});

instance FromJSON Volume where
        parseJSON
          = withObject "Volume"
              (\ x ->
                 Volume' <$> (x .:? "name") <*> (x .:? "host"))

instance ToJSON Volume where
        toJSON Volume'{..}
          = object ["name" .= _vName, "host" .= _vHost]

-- | Details on a data volume from another container.
--
-- /See:/ 'volumeFrom' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vfSourceContainer'
--
-- * 'vfReadOnly'
data VolumeFrom = VolumeFrom'
    { _vfSourceContainer :: !(Maybe Text)
    , _vfReadOnly :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'VolumeFrom' smart constructor.
volumeFrom :: VolumeFrom
volumeFrom = 
    VolumeFrom'
    { _vfSourceContainer = Nothing
    , _vfReadOnly = Nothing
    }

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
