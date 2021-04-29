{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Elastic Container Service
--
-- Amazon Elastic Container Service (Amazon ECS) is a highly scalable,
-- fast, container management service that makes it easy to run, stop, and
-- manage Docker containers on a cluster. You can host your cluster on a
-- serverless infrastructure that is managed by Amazon ECS by launching
-- your services or tasks using the Fargate launch type. For more control,
-- you can host your tasks on a cluster of Amazon Elastic Compute Cloud
-- (Amazon EC2) instances that you manage by using the EC2 launch type. For
-- more information about launch types, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types>.
--
-- Amazon ECS lets you launch and stop container-based applications with
-- simple API calls, allows you to get the state of your cluster from a
-- centralized service, and gives you access to many familiar Amazon EC2
-- features.
--
-- You can use Amazon ECS to schedule the placement of containers across
-- your cluster based on your resource needs, isolation policies, and
-- availability requirements. Amazon ECS eliminates the need for you to
-- operate your own cluster management and configuration management systems
-- or worry about scaling your management infrastructure.
module Network.AWS.ECS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** UpdateInProgressException
    _UpdateInProgressException,

    -- ** ServiceNotFoundException
    _ServiceNotFoundException,

    -- ** PlatformTaskDefinitionIncompatibilityException
    _PlatformTaskDefinitionIncompatibilityException,

    -- ** UnsupportedFeatureException
    _UnsupportedFeatureException,

    -- ** ClusterContainsContainerInstancesException
    _ClusterContainsContainerInstancesException,

    -- ** TaskSetNotFoundException
    _TaskSetNotFoundException,

    -- ** ClusterContainsServicesException
    _ClusterContainsServicesException,

    -- ** PlatformUnknownException
    _PlatformUnknownException,

    -- ** BlockedException
    _BlockedException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** MissingVersionException
    _MissingVersionException,

    -- ** ClientException
    _ClientException,

    -- ** ClusterNotFoundException
    _ClusterNotFoundException,

    -- ** NoUpdateAvailableException
    _NoUpdateAvailableException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceNotActiveException
    _ServiceNotActiveException,

    -- ** ClusterContainsTasksException
    _ClusterContainsTasksException,

    -- ** TargetNotFoundException
    _TargetNotFoundException,

    -- ** AttributeLimitExceededException
    _AttributeLimitExceededException,

    -- ** ServerException
    _ServerException,

    -- * Waiters
    -- $waiters

    -- ** TasksRunning
    newTasksRunning,

    -- ** TasksStopped
    newTasksStopped,

    -- ** ServicesInactive
    newServicesInactive,

    -- * Operations
    -- $operations

    -- ** SubmitAttachmentStateChanges
    SubmitAttachmentStateChanges (SubmitAttachmentStateChanges'),
    newSubmitAttachmentStateChanges,
    SubmitAttachmentStateChangesResponse (SubmitAttachmentStateChangesResponse'),
    newSubmitAttachmentStateChangesResponse,

    -- ** RegisterContainerInstance
    RegisterContainerInstance (RegisterContainerInstance'),
    newRegisterContainerInstance,
    RegisterContainerInstanceResponse (RegisterContainerInstanceResponse'),
    newRegisterContainerInstanceResponse,

    -- ** DiscoverPollEndpoint
    DiscoverPollEndpoint (DiscoverPollEndpoint'),
    newDiscoverPollEndpoint,
    DiscoverPollEndpointResponse (DiscoverPollEndpointResponse'),
    newDiscoverPollEndpointResponse,

    -- ** UpdateServicePrimaryTaskSet
    UpdateServicePrimaryTaskSet (UpdateServicePrimaryTaskSet'),
    newUpdateServicePrimaryTaskSet,
    UpdateServicePrimaryTaskSetResponse (UpdateServicePrimaryTaskSetResponse'),
    newUpdateServicePrimaryTaskSetResponse,

    -- ** DescribeClusters
    DescribeClusters (DescribeClusters'),
    newDescribeClusters,
    DescribeClustersResponse (DescribeClustersResponse'),
    newDescribeClustersResponse,

    -- ** ListServices (Paginated)
    ListServices (ListServices'),
    newListServices,
    ListServicesResponse (ListServicesResponse'),
    newListServicesResponse,

    -- ** RunTask
    RunTask (RunTask'),
    newRunTask,
    RunTaskResponse (RunTaskResponse'),
    newRunTaskResponse,

    -- ** ListTasks (Paginated)
    ListTasks (ListTasks'),
    newListTasks,
    ListTasksResponse (ListTasksResponse'),
    newListTasksResponse,

    -- ** CreateService
    CreateService (CreateService'),
    newCreateService,
    CreateServiceResponse (CreateServiceResponse'),
    newCreateServiceResponse,

    -- ** PutAccountSetting
    PutAccountSetting (PutAccountSetting'),
    newPutAccountSetting,
    PutAccountSettingResponse (PutAccountSettingResponse'),
    newPutAccountSettingResponse,

    -- ** DeleteAttributes
    DeleteAttributes (DeleteAttributes'),
    newDeleteAttributes,
    DeleteAttributesResponse (DeleteAttributesResponse'),
    newDeleteAttributesResponse,

    -- ** CreateCluster
    CreateCluster (CreateCluster'),
    newCreateCluster,
    CreateClusterResponse (CreateClusterResponse'),
    newCreateClusterResponse,

    -- ** CreateTaskSet
    CreateTaskSet (CreateTaskSet'),
    newCreateTaskSet,
    CreateTaskSetResponse (CreateTaskSetResponse'),
    newCreateTaskSetResponse,

    -- ** DescribeTasks
    DescribeTasks (DescribeTasks'),
    newDescribeTasks,
    DescribeTasksResponse (DescribeTasksResponse'),
    newDescribeTasksResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DescribeContainerInstances
    DescribeContainerInstances (DescribeContainerInstances'),
    newDescribeContainerInstances,
    DescribeContainerInstancesResponse (DescribeContainerInstancesResponse'),
    newDescribeContainerInstancesResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** PutAccountSettingDefault
    PutAccountSettingDefault (PutAccountSettingDefault'),
    newPutAccountSettingDefault,
    PutAccountSettingDefaultResponse (PutAccountSettingDefaultResponse'),
    newPutAccountSettingDefaultResponse,

    -- ** ListAttributes (Paginated)
    ListAttributes (ListAttributes'),
    newListAttributes,
    ListAttributesResponse (ListAttributesResponse'),
    newListAttributesResponse,

    -- ** SubmitContainerStateChange
    SubmitContainerStateChange (SubmitContainerStateChange'),
    newSubmitContainerStateChange,
    SubmitContainerStateChangeResponse (SubmitContainerStateChangeResponse'),
    newSubmitContainerStateChangeResponse,

    -- ** ListContainerInstances (Paginated)
    ListContainerInstances (ListContainerInstances'),
    newListContainerInstances,
    ListContainerInstancesResponse (ListContainerInstancesResponse'),
    newListContainerInstancesResponse,

    -- ** UpdateContainerAgent
    UpdateContainerAgent (UpdateContainerAgent'),
    newUpdateContainerAgent,
    UpdateContainerAgentResponse (UpdateContainerAgentResponse'),
    newUpdateContainerAgentResponse,

    -- ** UpdateCapacityProvider
    UpdateCapacityProvider (UpdateCapacityProvider'),
    newUpdateCapacityProvider,
    UpdateCapacityProviderResponse (UpdateCapacityProviderResponse'),
    newUpdateCapacityProviderResponse,

    -- ** DeleteService
    DeleteService (DeleteService'),
    newDeleteService,
    DeleteServiceResponse (DeleteServiceResponse'),
    newDeleteServiceResponse,

    -- ** UpdateService
    UpdateService (UpdateService'),
    newUpdateService,
    UpdateServiceResponse (UpdateServiceResponse'),
    newUpdateServiceResponse,

    -- ** DeleteCapacityProvider
    DeleteCapacityProvider (DeleteCapacityProvider'),
    newDeleteCapacityProvider,
    DeleteCapacityProviderResponse (DeleteCapacityProviderResponse'),
    newDeleteCapacityProviderResponse,

    -- ** DescribeTaskSets
    DescribeTaskSets (DescribeTaskSets'),
    newDescribeTaskSets,
    DescribeTaskSetsResponse (DescribeTaskSetsResponse'),
    newDescribeTaskSetsResponse,

    -- ** ListTaskDefinitions (Paginated)
    ListTaskDefinitions (ListTaskDefinitions'),
    newListTaskDefinitions,
    ListTaskDefinitionsResponse (ListTaskDefinitionsResponse'),
    newListTaskDefinitionsResponse,

    -- ** CreateCapacityProvider
    CreateCapacityProvider (CreateCapacityProvider'),
    newCreateCapacityProvider,
    CreateCapacityProviderResponse (CreateCapacityProviderResponse'),
    newCreateCapacityProviderResponse,

    -- ** RegisterTaskDefinition
    RegisterTaskDefinition (RegisterTaskDefinition'),
    newRegisterTaskDefinition,
    RegisterTaskDefinitionResponse (RegisterTaskDefinitionResponse'),
    newRegisterTaskDefinitionResponse,

    -- ** DeleteTaskSet
    DeleteTaskSet (DeleteTaskSet'),
    newDeleteTaskSet,
    DeleteTaskSetResponse (DeleteTaskSetResponse'),
    newDeleteTaskSetResponse,

    -- ** UpdateClusterSettings
    UpdateClusterSettings (UpdateClusterSettings'),
    newUpdateClusterSettings,
    UpdateClusterSettingsResponse (UpdateClusterSettingsResponse'),
    newUpdateClusterSettingsResponse,

    -- ** UpdateTaskSet
    UpdateTaskSet (UpdateTaskSet'),
    newUpdateTaskSet,
    UpdateTaskSetResponse (UpdateTaskSetResponse'),
    newUpdateTaskSetResponse,

    -- ** DeregisterContainerInstance
    DeregisterContainerInstance (DeregisterContainerInstance'),
    newDeregisterContainerInstance,
    DeregisterContainerInstanceResponse (DeregisterContainerInstanceResponse'),
    newDeregisterContainerInstanceResponse,

    -- ** PutAttributes
    PutAttributes (PutAttributes'),
    newPutAttributes,
    PutAttributesResponse (PutAttributesResponse'),
    newPutAttributesResponse,

    -- ** DeleteAccountSetting
    DeleteAccountSetting (DeleteAccountSetting'),
    newDeleteAccountSetting,
    DeleteAccountSettingResponse (DeleteAccountSettingResponse'),
    newDeleteAccountSettingResponse,

    -- ** ListAccountSettings (Paginated)
    ListAccountSettings (ListAccountSettings'),
    newListAccountSettings,
    ListAccountSettingsResponse (ListAccountSettingsResponse'),
    newListAccountSettingsResponse,

    -- ** DescribeServices
    DescribeServices (DescribeServices'),
    newDescribeServices,
    DescribeServicesResponse (DescribeServicesResponse'),
    newDescribeServicesResponse,

    -- ** DescribeCapacityProviders
    DescribeCapacityProviders (DescribeCapacityProviders'),
    newDescribeCapacityProviders,
    DescribeCapacityProvidersResponse (DescribeCapacityProvidersResponse'),
    newDescribeCapacityProvidersResponse,

    -- ** DeleteCluster
    DeleteCluster (DeleteCluster'),
    newDeleteCluster,
    DeleteClusterResponse (DeleteClusterResponse'),
    newDeleteClusterResponse,

    -- ** ListClusters (Paginated)
    ListClusters (ListClusters'),
    newListClusters,
    ListClustersResponse (ListClustersResponse'),
    newListClustersResponse,

    -- ** UpdateContainerInstancesState
    UpdateContainerInstancesState (UpdateContainerInstancesState'),
    newUpdateContainerInstancesState,
    UpdateContainerInstancesStateResponse (UpdateContainerInstancesStateResponse'),
    newUpdateContainerInstancesStateResponse,

    -- ** SubmitTaskStateChange
    SubmitTaskStateChange (SubmitTaskStateChange'),
    newSubmitTaskStateChange,
    SubmitTaskStateChangeResponse (SubmitTaskStateChangeResponse'),
    newSubmitTaskStateChangeResponse,

    -- ** DeregisterTaskDefinition
    DeregisterTaskDefinition (DeregisterTaskDefinition'),
    newDeregisterTaskDefinition,
    DeregisterTaskDefinitionResponse (DeregisterTaskDefinitionResponse'),
    newDeregisterTaskDefinitionResponse,

    -- ** StopTask
    StopTask (StopTask'),
    newStopTask,
    StopTaskResponse (StopTaskResponse'),
    newStopTaskResponse,

    -- ** PutClusterCapacityProviders
    PutClusterCapacityProviders (PutClusterCapacityProviders'),
    newPutClusterCapacityProviders,
    PutClusterCapacityProvidersResponse (PutClusterCapacityProvidersResponse'),
    newPutClusterCapacityProvidersResponse,

    -- ** DescribeTaskDefinition
    DescribeTaskDefinition (DescribeTaskDefinition'),
    newDescribeTaskDefinition,
    DescribeTaskDefinitionResponse (DescribeTaskDefinitionResponse'),
    newDescribeTaskDefinitionResponse,

    -- ** StartTask
    StartTask (StartTask'),
    newStartTask,
    StartTaskResponse (StartTaskResponse'),
    newStartTaskResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTaskDefinitionFamilies (Paginated)
    ListTaskDefinitionFamilies (ListTaskDefinitionFamilies'),
    newListTaskDefinitionFamilies,
    ListTaskDefinitionFamiliesResponse (ListTaskDefinitionFamiliesResponse'),
    newListTaskDefinitionFamiliesResponse,

    -- * Types

    -- ** AgentUpdateStatus
    AgentUpdateStatus (..),

    -- ** AssignPublicIp
    AssignPublicIp (..),

    -- ** CapacityProviderField
    CapacityProviderField (..),

    -- ** CapacityProviderStatus
    CapacityProviderStatus (..),

    -- ** CapacityProviderUpdateStatus
    CapacityProviderUpdateStatus (..),

    -- ** ClusterField
    ClusterField (..),

    -- ** ClusterSettingName
    ClusterSettingName (..),

    -- ** Compatibility
    Compatibility (..),

    -- ** Connectivity
    Connectivity (..),

    -- ** ContainerCondition
    ContainerCondition (..),

    -- ** ContainerInstanceField
    ContainerInstanceField (..),

    -- ** ContainerInstanceStatus
    ContainerInstanceStatus (..),

    -- ** DeploymentControllerType
    DeploymentControllerType (..),

    -- ** DeploymentRolloutState
    DeploymentRolloutState (..),

    -- ** DesiredStatus
    DesiredStatus (..),

    -- ** DeviceCgroupPermission
    DeviceCgroupPermission (..),

    -- ** EFSAuthorizationConfigIAM
    EFSAuthorizationConfigIAM (..),

    -- ** EFSTransitEncryption
    EFSTransitEncryption (..),

    -- ** EnvironmentFileType
    EnvironmentFileType (..),

    -- ** FirelensConfigurationType
    FirelensConfigurationType (..),

    -- ** HealthStatus
    HealthStatus (..),

    -- ** IpcMode
    IpcMode (..),

    -- ** LaunchType
    LaunchType (..),

    -- ** LogDriver
    LogDriver (..),

    -- ** ManagedScalingStatus
    ManagedScalingStatus (..),

    -- ** ManagedTerminationProtection
    ManagedTerminationProtection (..),

    -- ** NetworkMode
    NetworkMode (..),

    -- ** PidMode
    PidMode (..),

    -- ** PlacementConstraintType
    PlacementConstraintType (..),

    -- ** PlacementStrategyType
    PlacementStrategyType (..),

    -- ** PlatformDeviceType
    PlatformDeviceType (..),

    -- ** PropagateTags
    PropagateTags (..),

    -- ** ProxyConfigurationType
    ProxyConfigurationType (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** ScaleUnit
    ScaleUnit (..),

    -- ** SchedulingStrategy
    SchedulingStrategy (..),

    -- ** Scope
    Scope (..),

    -- ** ServiceField
    ServiceField (..),

    -- ** SettingName
    SettingName (..),

    -- ** SortOrder
    SortOrder (..),

    -- ** StabilityStatus
    StabilityStatus (..),

    -- ** TargetType
    TargetType (..),

    -- ** TaskDefinitionFamilyStatus
    TaskDefinitionFamilyStatus (..),

    -- ** TaskDefinitionField
    TaskDefinitionField (..),

    -- ** TaskDefinitionPlacementConstraintType
    TaskDefinitionPlacementConstraintType (..),

    -- ** TaskDefinitionStatus
    TaskDefinitionStatus (..),

    -- ** TaskField
    TaskField (..),

    -- ** TaskSetField
    TaskSetField (..),

    -- ** TaskStopCode
    TaskStopCode (..),

    -- ** TransportProtocol
    TransportProtocol (..),

    -- ** UlimitName
    UlimitName (..),

    -- ** Attachment
    Attachment (Attachment'),
    newAttachment,

    -- ** AttachmentStateChange
    AttachmentStateChange (AttachmentStateChange'),
    newAttachmentStateChange,

    -- ** Attribute
    Attribute (Attribute'),
    newAttribute,

    -- ** AutoScalingGroupProvider
    AutoScalingGroupProvider (AutoScalingGroupProvider'),
    newAutoScalingGroupProvider,

    -- ** AutoScalingGroupProviderUpdate
    AutoScalingGroupProviderUpdate (AutoScalingGroupProviderUpdate'),
    newAutoScalingGroupProviderUpdate,

    -- ** AwsVpcConfiguration
    AwsVpcConfiguration (AwsVpcConfiguration'),
    newAwsVpcConfiguration,

    -- ** CapacityProvider
    CapacityProvider (CapacityProvider'),
    newCapacityProvider,

    -- ** CapacityProviderStrategyItem
    CapacityProviderStrategyItem (CapacityProviderStrategyItem'),
    newCapacityProviderStrategyItem,

    -- ** Cluster
    Cluster (Cluster'),
    newCluster,

    -- ** ClusterSetting
    ClusterSetting (ClusterSetting'),
    newClusterSetting,

    -- ** Container
    Container (Container'),
    newContainer,

    -- ** ContainerDefinition
    ContainerDefinition (ContainerDefinition'),
    newContainerDefinition,

    -- ** ContainerDependency
    ContainerDependency (ContainerDependency'),
    newContainerDependency,

    -- ** ContainerInstance
    ContainerInstance (ContainerInstance'),
    newContainerInstance,

    -- ** ContainerOverride
    ContainerOverride (ContainerOverride'),
    newContainerOverride,

    -- ** ContainerService
    ContainerService (ContainerService'),
    newContainerService,

    -- ** ContainerStateChange
    ContainerStateChange (ContainerStateChange'),
    newContainerStateChange,

    -- ** Deployment
    Deployment (Deployment'),
    newDeployment,

    -- ** DeploymentCircuitBreaker
    DeploymentCircuitBreaker (DeploymentCircuitBreaker'),
    newDeploymentCircuitBreaker,

    -- ** DeploymentConfiguration
    DeploymentConfiguration (DeploymentConfiguration'),
    newDeploymentConfiguration,

    -- ** DeploymentController
    DeploymentController (DeploymentController'),
    newDeploymentController,

    -- ** Device
    Device (Device'),
    newDevice,

    -- ** DockerVolumeConfiguration
    DockerVolumeConfiguration (DockerVolumeConfiguration'),
    newDockerVolumeConfiguration,

    -- ** EFSAuthorizationConfig
    EFSAuthorizationConfig (EFSAuthorizationConfig'),
    newEFSAuthorizationConfig,

    -- ** EFSVolumeConfiguration
    EFSVolumeConfiguration (EFSVolumeConfiguration'),
    newEFSVolumeConfiguration,

    -- ** EnvironmentFile
    EnvironmentFile (EnvironmentFile'),
    newEnvironmentFile,

    -- ** FSxWindowsFileServerAuthorizationConfig
    FSxWindowsFileServerAuthorizationConfig (FSxWindowsFileServerAuthorizationConfig'),
    newFSxWindowsFileServerAuthorizationConfig,

    -- ** FSxWindowsFileServerVolumeConfiguration
    FSxWindowsFileServerVolumeConfiguration (FSxWindowsFileServerVolumeConfiguration'),
    newFSxWindowsFileServerVolumeConfiguration,

    -- ** Failure
    Failure (Failure'),
    newFailure,

    -- ** FirelensConfiguration
    FirelensConfiguration (FirelensConfiguration'),
    newFirelensConfiguration,

    -- ** HealthCheck
    HealthCheck (HealthCheck'),
    newHealthCheck,

    -- ** HostEntry
    HostEntry (HostEntry'),
    newHostEntry,

    -- ** HostVolumeProperties
    HostVolumeProperties (HostVolumeProperties'),
    newHostVolumeProperties,

    -- ** InferenceAccelerator
    InferenceAccelerator (InferenceAccelerator'),
    newInferenceAccelerator,

    -- ** InferenceAcceleratorOverride
    InferenceAcceleratorOverride (InferenceAcceleratorOverride'),
    newInferenceAcceleratorOverride,

    -- ** KernelCapabilities
    KernelCapabilities (KernelCapabilities'),
    newKernelCapabilities,

    -- ** KeyValuePair
    KeyValuePair (KeyValuePair'),
    newKeyValuePair,

    -- ** LinuxParameters
    LinuxParameters (LinuxParameters'),
    newLinuxParameters,

    -- ** LoadBalancer
    LoadBalancer (LoadBalancer'),
    newLoadBalancer,

    -- ** LogConfiguration
    LogConfiguration (LogConfiguration'),
    newLogConfiguration,

    -- ** ManagedScaling
    ManagedScaling (ManagedScaling'),
    newManagedScaling,

    -- ** MountPoint
    MountPoint (MountPoint'),
    newMountPoint,

    -- ** NetworkBinding
    NetworkBinding (NetworkBinding'),
    newNetworkBinding,

    -- ** NetworkConfiguration
    NetworkConfiguration (NetworkConfiguration'),
    newNetworkConfiguration,

    -- ** NetworkInterface
    NetworkInterface (NetworkInterface'),
    newNetworkInterface,

    -- ** PlacementConstraint
    PlacementConstraint (PlacementConstraint'),
    newPlacementConstraint,

    -- ** PlacementStrategy
    PlacementStrategy (PlacementStrategy'),
    newPlacementStrategy,

    -- ** PlatformDevice
    PlatformDevice (PlatformDevice'),
    newPlatformDevice,

    -- ** PortMapping
    PortMapping (PortMapping'),
    newPortMapping,

    -- ** ProxyConfiguration
    ProxyConfiguration (ProxyConfiguration'),
    newProxyConfiguration,

    -- ** RepositoryCredentials
    RepositoryCredentials (RepositoryCredentials'),
    newRepositoryCredentials,

    -- ** Resource
    Resource (Resource'),
    newResource,

    -- ** ResourceRequirement
    ResourceRequirement (ResourceRequirement'),
    newResourceRequirement,

    -- ** Scale
    Scale (Scale'),
    newScale,

    -- ** Secret
    Secret (Secret'),
    newSecret,

    -- ** ServiceEvent
    ServiceEvent (ServiceEvent'),
    newServiceEvent,

    -- ** ServiceRegistry
    ServiceRegistry (ServiceRegistry'),
    newServiceRegistry,

    -- ** Setting
    Setting (Setting'),
    newSetting,

    -- ** SystemControl
    SystemControl (SystemControl'),
    newSystemControl,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** Task
    Task (Task'),
    newTask,

    -- ** TaskDefinition
    TaskDefinition (TaskDefinition'),
    newTaskDefinition,

    -- ** TaskDefinitionPlacementConstraint
    TaskDefinitionPlacementConstraint (TaskDefinitionPlacementConstraint'),
    newTaskDefinitionPlacementConstraint,

    -- ** TaskOverride
    TaskOverride (TaskOverride'),
    newTaskOverride,

    -- ** TaskSet
    TaskSet (TaskSet'),
    newTaskSet,

    -- ** Tmpfs
    Tmpfs (Tmpfs'),
    newTmpfs,

    -- ** Ulimit
    Ulimit (Ulimit'),
    newUlimit,

    -- ** VersionInfo
    VersionInfo (VersionInfo'),
    newVersionInfo,

    -- ** Volume
    Volume (Volume'),
    newVolume,

    -- ** VolumeFrom
    VolumeFrom (VolumeFrom'),
    newVolumeFrom,
  )
where

import Network.AWS.ECS.CreateCapacityProvider
import Network.AWS.ECS.CreateCluster
import Network.AWS.ECS.CreateService
import Network.AWS.ECS.CreateTaskSet
import Network.AWS.ECS.DeleteAccountSetting
import Network.AWS.ECS.DeleteAttributes
import Network.AWS.ECS.DeleteCapacityProvider
import Network.AWS.ECS.DeleteCluster
import Network.AWS.ECS.DeleteService
import Network.AWS.ECS.DeleteTaskSet
import Network.AWS.ECS.DeregisterContainerInstance
import Network.AWS.ECS.DeregisterTaskDefinition
import Network.AWS.ECS.DescribeCapacityProviders
import Network.AWS.ECS.DescribeClusters
import Network.AWS.ECS.DescribeContainerInstances
import Network.AWS.ECS.DescribeServices
import Network.AWS.ECS.DescribeTaskDefinition
import Network.AWS.ECS.DescribeTaskSets
import Network.AWS.ECS.DescribeTasks
import Network.AWS.ECS.DiscoverPollEndpoint
import Network.AWS.ECS.Lens
import Network.AWS.ECS.ListAccountSettings
import Network.AWS.ECS.ListAttributes
import Network.AWS.ECS.ListClusters
import Network.AWS.ECS.ListContainerInstances
import Network.AWS.ECS.ListServices
import Network.AWS.ECS.ListTagsForResource
import Network.AWS.ECS.ListTaskDefinitionFamilies
import Network.AWS.ECS.ListTaskDefinitions
import Network.AWS.ECS.ListTasks
import Network.AWS.ECS.PutAccountSetting
import Network.AWS.ECS.PutAccountSettingDefault
import Network.AWS.ECS.PutAttributes
import Network.AWS.ECS.PutClusterCapacityProviders
import Network.AWS.ECS.RegisterContainerInstance
import Network.AWS.ECS.RegisterTaskDefinition
import Network.AWS.ECS.RunTask
import Network.AWS.ECS.StartTask
import Network.AWS.ECS.StopTask
import Network.AWS.ECS.SubmitAttachmentStateChanges
import Network.AWS.ECS.SubmitContainerStateChange
import Network.AWS.ECS.SubmitTaskStateChange
import Network.AWS.ECS.TagResource
import Network.AWS.ECS.Types
import Network.AWS.ECS.UntagResource
import Network.AWS.ECS.UpdateCapacityProvider
import Network.AWS.ECS.UpdateClusterSettings
import Network.AWS.ECS.UpdateContainerAgent
import Network.AWS.ECS.UpdateContainerInstancesState
import Network.AWS.ECS.UpdateService
import Network.AWS.ECS.UpdateServicePrimaryTaskSet
import Network.AWS.ECS.UpdateTaskSet
import Network.AWS.ECS.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ECS'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
