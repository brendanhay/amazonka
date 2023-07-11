{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ECS
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2014-11-13@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Elastic Container Service
--
-- Amazon Elastic Container Service (Amazon ECS) is a highly scalable,
-- fast, container management service. It makes it easy to run, stop, and
-- manage Docker containers. You can host your cluster on a serverless
-- infrastructure that\'s managed by Amazon ECS by launching your services
-- or tasks on Fargate. For more control, you can host your tasks on a
-- cluster of Amazon Elastic Compute Cloud (Amazon EC2) or External
-- (on-premises) instances that you manage.
--
-- Amazon ECS makes it easy to launch and stop container-based applications
-- with simple API calls. This makes it easy to get the state of your
-- cluster from a centralized service, and gives you access to many
-- familiar Amazon EC2 features.
--
-- You can use Amazon ECS to schedule the placement of containers across
-- your cluster based on your resource needs, isolation policies, and
-- availability requirements. With Amazon ECS, you don\'t need to operate
-- your own cluster management and configuration management systems. You
-- also don\'t need to worry about scaling your management infrastructure.
module Amazonka.ECS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** AttributeLimitExceededException
    _AttributeLimitExceededException,

    -- ** BlockedException
    _BlockedException,

    -- ** ClientException
    _ClientException,

    -- ** ClusterContainsContainerInstancesException
    _ClusterContainsContainerInstancesException,

    -- ** ClusterContainsServicesException
    _ClusterContainsServicesException,

    -- ** ClusterContainsTasksException
    _ClusterContainsTasksException,

    -- ** ClusterNotFoundException
    _ClusterNotFoundException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** MissingVersionException
    _MissingVersionException,

    -- ** NamespaceNotFoundException
    _NamespaceNotFoundException,

    -- ** NoUpdateAvailableException
    _NoUpdateAvailableException,

    -- ** PlatformTaskDefinitionIncompatibilityException
    _PlatformTaskDefinitionIncompatibilityException,

    -- ** PlatformUnknownException
    _PlatformUnknownException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServerException
    _ServerException,

    -- ** ServiceNotActiveException
    _ServiceNotActiveException,

    -- ** ServiceNotFoundException
    _ServiceNotFoundException,

    -- ** TargetNotConnectedException
    _TargetNotConnectedException,

    -- ** TargetNotFoundException
    _TargetNotFoundException,

    -- ** TaskSetNotFoundException
    _TaskSetNotFoundException,

    -- ** UnsupportedFeatureException
    _UnsupportedFeatureException,

    -- ** UpdateInProgressException
    _UpdateInProgressException,

    -- * Waiters
    -- $waiters

    -- ** ServicesInactive
    newServicesInactive,

    -- ** TasksRunning
    newTasksRunning,

    -- ** TasksStopped
    newTasksStopped,

    -- * Operations
    -- $operations

    -- ** CreateCapacityProvider
    CreateCapacityProvider (CreateCapacityProvider'),
    newCreateCapacityProvider,
    CreateCapacityProviderResponse (CreateCapacityProviderResponse'),
    newCreateCapacityProviderResponse,

    -- ** CreateCluster
    CreateCluster (CreateCluster'),
    newCreateCluster,
    CreateClusterResponse (CreateClusterResponse'),
    newCreateClusterResponse,

    -- ** CreateService
    CreateService (CreateService'),
    newCreateService,
    CreateServiceResponse (CreateServiceResponse'),
    newCreateServiceResponse,

    -- ** CreateTaskSet
    CreateTaskSet (CreateTaskSet'),
    newCreateTaskSet,
    CreateTaskSetResponse (CreateTaskSetResponse'),
    newCreateTaskSetResponse,

    -- ** DeleteAccountSetting
    DeleteAccountSetting (DeleteAccountSetting'),
    newDeleteAccountSetting,
    DeleteAccountSettingResponse (DeleteAccountSettingResponse'),
    newDeleteAccountSettingResponse,

    -- ** DeleteAttributes
    DeleteAttributes (DeleteAttributes'),
    newDeleteAttributes,
    DeleteAttributesResponse (DeleteAttributesResponse'),
    newDeleteAttributesResponse,

    -- ** DeleteCapacityProvider
    DeleteCapacityProvider (DeleteCapacityProvider'),
    newDeleteCapacityProvider,
    DeleteCapacityProviderResponse (DeleteCapacityProviderResponse'),
    newDeleteCapacityProviderResponse,

    -- ** DeleteCluster
    DeleteCluster (DeleteCluster'),
    newDeleteCluster,
    DeleteClusterResponse (DeleteClusterResponse'),
    newDeleteClusterResponse,

    -- ** DeleteService
    DeleteService (DeleteService'),
    newDeleteService,
    DeleteServiceResponse (DeleteServiceResponse'),
    newDeleteServiceResponse,

    -- ** DeleteTaskSet
    DeleteTaskSet (DeleteTaskSet'),
    newDeleteTaskSet,
    DeleteTaskSetResponse (DeleteTaskSetResponse'),
    newDeleteTaskSetResponse,

    -- ** DeregisterContainerInstance
    DeregisterContainerInstance (DeregisterContainerInstance'),
    newDeregisterContainerInstance,
    DeregisterContainerInstanceResponse (DeregisterContainerInstanceResponse'),
    newDeregisterContainerInstanceResponse,

    -- ** DeregisterTaskDefinition
    DeregisterTaskDefinition (DeregisterTaskDefinition'),
    newDeregisterTaskDefinition,
    DeregisterTaskDefinitionResponse (DeregisterTaskDefinitionResponse'),
    newDeregisterTaskDefinitionResponse,

    -- ** DescribeCapacityProviders
    DescribeCapacityProviders (DescribeCapacityProviders'),
    newDescribeCapacityProviders,
    DescribeCapacityProvidersResponse (DescribeCapacityProvidersResponse'),
    newDescribeCapacityProvidersResponse,

    -- ** DescribeClusters
    DescribeClusters (DescribeClusters'),
    newDescribeClusters,
    DescribeClustersResponse (DescribeClustersResponse'),
    newDescribeClustersResponse,

    -- ** DescribeContainerInstances
    DescribeContainerInstances (DescribeContainerInstances'),
    newDescribeContainerInstances,
    DescribeContainerInstancesResponse (DescribeContainerInstancesResponse'),
    newDescribeContainerInstancesResponse,

    -- ** DescribeServices
    DescribeServices (DescribeServices'),
    newDescribeServices,
    DescribeServicesResponse (DescribeServicesResponse'),
    newDescribeServicesResponse,

    -- ** DescribeTaskDefinition
    DescribeTaskDefinition (DescribeTaskDefinition'),
    newDescribeTaskDefinition,
    DescribeTaskDefinitionResponse (DescribeTaskDefinitionResponse'),
    newDescribeTaskDefinitionResponse,

    -- ** DescribeTaskSets
    DescribeTaskSets (DescribeTaskSets'),
    newDescribeTaskSets,
    DescribeTaskSetsResponse (DescribeTaskSetsResponse'),
    newDescribeTaskSetsResponse,

    -- ** DescribeTasks
    DescribeTasks (DescribeTasks'),
    newDescribeTasks,
    DescribeTasksResponse (DescribeTasksResponse'),
    newDescribeTasksResponse,

    -- ** DiscoverPollEndpoint
    DiscoverPollEndpoint (DiscoverPollEndpoint'),
    newDiscoverPollEndpoint,
    DiscoverPollEndpointResponse (DiscoverPollEndpointResponse'),
    newDiscoverPollEndpointResponse,

    -- ** ExecuteCommand
    ExecuteCommand (ExecuteCommand'),
    newExecuteCommand,
    ExecuteCommandResponse (ExecuteCommandResponse'),
    newExecuteCommandResponse,

    -- ** GetTaskProtection
    GetTaskProtection (GetTaskProtection'),
    newGetTaskProtection,
    GetTaskProtectionResponse (GetTaskProtectionResponse'),
    newGetTaskProtectionResponse,

    -- ** ListAccountSettings (Paginated)
    ListAccountSettings (ListAccountSettings'),
    newListAccountSettings,
    ListAccountSettingsResponse (ListAccountSettingsResponse'),
    newListAccountSettingsResponse,

    -- ** ListAttributes (Paginated)
    ListAttributes (ListAttributes'),
    newListAttributes,
    ListAttributesResponse (ListAttributesResponse'),
    newListAttributesResponse,

    -- ** ListClusters (Paginated)
    ListClusters (ListClusters'),
    newListClusters,
    ListClustersResponse (ListClustersResponse'),
    newListClustersResponse,

    -- ** ListContainerInstances (Paginated)
    ListContainerInstances (ListContainerInstances'),
    newListContainerInstances,
    ListContainerInstancesResponse (ListContainerInstancesResponse'),
    newListContainerInstancesResponse,

    -- ** ListServices (Paginated)
    ListServices (ListServices'),
    newListServices,
    ListServicesResponse (ListServicesResponse'),
    newListServicesResponse,

    -- ** ListServicesByNamespace (Paginated)
    ListServicesByNamespace (ListServicesByNamespace'),
    newListServicesByNamespace,
    ListServicesByNamespaceResponse (ListServicesByNamespaceResponse'),
    newListServicesByNamespaceResponse,

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

    -- ** ListTaskDefinitions (Paginated)
    ListTaskDefinitions (ListTaskDefinitions'),
    newListTaskDefinitions,
    ListTaskDefinitionsResponse (ListTaskDefinitionsResponse'),
    newListTaskDefinitionsResponse,

    -- ** ListTasks (Paginated)
    ListTasks (ListTasks'),
    newListTasks,
    ListTasksResponse (ListTasksResponse'),
    newListTasksResponse,

    -- ** PutAccountSetting
    PutAccountSetting (PutAccountSetting'),
    newPutAccountSetting,
    PutAccountSettingResponse (PutAccountSettingResponse'),
    newPutAccountSettingResponse,

    -- ** PutAccountSettingDefault
    PutAccountSettingDefault (PutAccountSettingDefault'),
    newPutAccountSettingDefault,
    PutAccountSettingDefaultResponse (PutAccountSettingDefaultResponse'),
    newPutAccountSettingDefaultResponse,

    -- ** PutAttributes
    PutAttributes (PutAttributes'),
    newPutAttributes,
    PutAttributesResponse (PutAttributesResponse'),
    newPutAttributesResponse,

    -- ** PutClusterCapacityProviders
    PutClusterCapacityProviders (PutClusterCapacityProviders'),
    newPutClusterCapacityProviders,
    PutClusterCapacityProvidersResponse (PutClusterCapacityProvidersResponse'),
    newPutClusterCapacityProvidersResponse,

    -- ** RegisterContainerInstance
    RegisterContainerInstance (RegisterContainerInstance'),
    newRegisterContainerInstance,
    RegisterContainerInstanceResponse (RegisterContainerInstanceResponse'),
    newRegisterContainerInstanceResponse,

    -- ** RegisterTaskDefinition
    RegisterTaskDefinition (RegisterTaskDefinition'),
    newRegisterTaskDefinition,
    RegisterTaskDefinitionResponse (RegisterTaskDefinitionResponse'),
    newRegisterTaskDefinitionResponse,

    -- ** RunTask
    RunTask (RunTask'),
    newRunTask,
    RunTaskResponse (RunTaskResponse'),
    newRunTaskResponse,

    -- ** StartTask
    StartTask (StartTask'),
    newStartTask,
    StartTaskResponse (StartTaskResponse'),
    newStartTaskResponse,

    -- ** StopTask
    StopTask (StopTask'),
    newStopTask,
    StopTaskResponse (StopTaskResponse'),
    newStopTaskResponse,

    -- ** SubmitAttachmentStateChanges
    SubmitAttachmentStateChanges (SubmitAttachmentStateChanges'),
    newSubmitAttachmentStateChanges,
    SubmitAttachmentStateChangesResponse (SubmitAttachmentStateChangesResponse'),
    newSubmitAttachmentStateChangesResponse,

    -- ** SubmitContainerStateChange
    SubmitContainerStateChange (SubmitContainerStateChange'),
    newSubmitContainerStateChange,
    SubmitContainerStateChangeResponse (SubmitContainerStateChangeResponse'),
    newSubmitContainerStateChangeResponse,

    -- ** SubmitTaskStateChange
    SubmitTaskStateChange (SubmitTaskStateChange'),
    newSubmitTaskStateChange,
    SubmitTaskStateChangeResponse (SubmitTaskStateChangeResponse'),
    newSubmitTaskStateChangeResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateCapacityProvider
    UpdateCapacityProvider (UpdateCapacityProvider'),
    newUpdateCapacityProvider,
    UpdateCapacityProviderResponse (UpdateCapacityProviderResponse'),
    newUpdateCapacityProviderResponse,

    -- ** UpdateCluster
    UpdateCluster (UpdateCluster'),
    newUpdateCluster,
    UpdateClusterResponse (UpdateClusterResponse'),
    newUpdateClusterResponse,

    -- ** UpdateClusterSettings
    UpdateClusterSettings (UpdateClusterSettings'),
    newUpdateClusterSettings,
    UpdateClusterSettingsResponse (UpdateClusterSettingsResponse'),
    newUpdateClusterSettingsResponse,

    -- ** UpdateContainerAgent
    UpdateContainerAgent (UpdateContainerAgent'),
    newUpdateContainerAgent,
    UpdateContainerAgentResponse (UpdateContainerAgentResponse'),
    newUpdateContainerAgentResponse,

    -- ** UpdateContainerInstancesState
    UpdateContainerInstancesState (UpdateContainerInstancesState'),
    newUpdateContainerInstancesState,
    UpdateContainerInstancesStateResponse (UpdateContainerInstancesStateResponse'),
    newUpdateContainerInstancesStateResponse,

    -- ** UpdateService
    UpdateService (UpdateService'),
    newUpdateService,
    UpdateServiceResponse (UpdateServiceResponse'),
    newUpdateServiceResponse,

    -- ** UpdateServicePrimaryTaskSet
    UpdateServicePrimaryTaskSet (UpdateServicePrimaryTaskSet'),
    newUpdateServicePrimaryTaskSet,
    UpdateServicePrimaryTaskSetResponse (UpdateServicePrimaryTaskSetResponse'),
    newUpdateServicePrimaryTaskSetResponse,

    -- ** UpdateTaskProtection
    UpdateTaskProtection (UpdateTaskProtection'),
    newUpdateTaskProtection,
    UpdateTaskProtectionResponse (UpdateTaskProtectionResponse'),
    newUpdateTaskProtectionResponse,

    -- ** UpdateTaskSet
    UpdateTaskSet (UpdateTaskSet'),
    newUpdateTaskSet,
    UpdateTaskSetResponse (UpdateTaskSetResponse'),
    newUpdateTaskSetResponse,

    -- * Types

    -- ** AgentUpdateStatus
    AgentUpdateStatus (..),

    -- ** ApplicationProtocol
    ApplicationProtocol (..),

    -- ** AssignPublicIp
    AssignPublicIp (..),

    -- ** CPUArchitecture
    CPUArchitecture (..),

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

    -- ** ExecuteCommandLogging
    ExecuteCommandLogging (..),

    -- ** FirelensConfigurationType
    FirelensConfigurationType (..),

    -- ** HealthStatus
    HealthStatus (..),

    -- ** InstanceHealthCheckState
    InstanceHealthCheckState (..),

    -- ** InstanceHealthCheckType
    InstanceHealthCheckType (..),

    -- ** IpcMode
    IpcMode (..),

    -- ** LaunchType
    LaunchType (..),

    -- ** LogDriver
    LogDriver (..),

    -- ** ManagedAgentName
    ManagedAgentName (..),

    -- ** ManagedScalingStatus
    ManagedScalingStatus (..),

    -- ** ManagedTerminationProtection
    ManagedTerminationProtection (..),

    -- ** NetworkMode
    NetworkMode (..),

    -- ** OSFamily
    OSFamily (..),

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

    -- ** ClusterConfiguration
    ClusterConfiguration (ClusterConfiguration'),
    newClusterConfiguration,

    -- ** ClusterServiceConnectDefaults
    ClusterServiceConnectDefaults (ClusterServiceConnectDefaults'),
    newClusterServiceConnectDefaults,

    -- ** ClusterServiceConnectDefaultsRequest
    ClusterServiceConnectDefaultsRequest (ClusterServiceConnectDefaultsRequest'),
    newClusterServiceConnectDefaultsRequest,

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

    -- ** ContainerInstanceHealthStatus
    ContainerInstanceHealthStatus (ContainerInstanceHealthStatus'),
    newContainerInstanceHealthStatus,

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

    -- ** DeploymentAlarms
    DeploymentAlarms (DeploymentAlarms'),
    newDeploymentAlarms,

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

    -- ** EphemeralStorage
    EphemeralStorage (EphemeralStorage'),
    newEphemeralStorage,

    -- ** ExecuteCommandConfiguration
    ExecuteCommandConfiguration (ExecuteCommandConfiguration'),
    newExecuteCommandConfiguration,

    -- ** ExecuteCommandLogConfiguration
    ExecuteCommandLogConfiguration (ExecuteCommandLogConfiguration'),
    newExecuteCommandLogConfiguration,

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

    -- ** InstanceHealthCheckResult
    InstanceHealthCheckResult (InstanceHealthCheckResult'),
    newInstanceHealthCheckResult,

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

    -- ** ManagedAgent
    ManagedAgent (ManagedAgent'),
    newManagedAgent,

    -- ** ManagedAgentStateChange
    ManagedAgentStateChange (ManagedAgentStateChange'),
    newManagedAgentStateChange,

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

    -- ** ProtectedTask
    ProtectedTask (ProtectedTask'),
    newProtectedTask,

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

    -- ** RuntimePlatform
    RuntimePlatform (RuntimePlatform'),
    newRuntimePlatform,

    -- ** Scale
    Scale (Scale'),
    newScale,

    -- ** Secret
    Secret (Secret'),
    newSecret,

    -- ** ServiceConnectClientAlias
    ServiceConnectClientAlias (ServiceConnectClientAlias'),
    newServiceConnectClientAlias,

    -- ** ServiceConnectConfiguration
    ServiceConnectConfiguration (ServiceConnectConfiguration'),
    newServiceConnectConfiguration,

    -- ** ServiceConnectService
    ServiceConnectService (ServiceConnectService'),
    newServiceConnectService,

    -- ** ServiceConnectServiceResource
    ServiceConnectServiceResource (ServiceConnectServiceResource'),
    newServiceConnectServiceResource,

    -- ** ServiceEvent
    ServiceEvent (ServiceEvent'),
    newServiceEvent,

    -- ** ServiceRegistry
    ServiceRegistry (ServiceRegistry'),
    newServiceRegistry,

    -- ** Session
    Session (Session'),
    newSession,

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

import Amazonka.ECS.CreateCapacityProvider
import Amazonka.ECS.CreateCluster
import Amazonka.ECS.CreateService
import Amazonka.ECS.CreateTaskSet
import Amazonka.ECS.DeleteAccountSetting
import Amazonka.ECS.DeleteAttributes
import Amazonka.ECS.DeleteCapacityProvider
import Amazonka.ECS.DeleteCluster
import Amazonka.ECS.DeleteService
import Amazonka.ECS.DeleteTaskSet
import Amazonka.ECS.DeregisterContainerInstance
import Amazonka.ECS.DeregisterTaskDefinition
import Amazonka.ECS.DescribeCapacityProviders
import Amazonka.ECS.DescribeClusters
import Amazonka.ECS.DescribeContainerInstances
import Amazonka.ECS.DescribeServices
import Amazonka.ECS.DescribeTaskDefinition
import Amazonka.ECS.DescribeTaskSets
import Amazonka.ECS.DescribeTasks
import Amazonka.ECS.DiscoverPollEndpoint
import Amazonka.ECS.ExecuteCommand
import Amazonka.ECS.GetTaskProtection
import Amazonka.ECS.Lens
import Amazonka.ECS.ListAccountSettings
import Amazonka.ECS.ListAttributes
import Amazonka.ECS.ListClusters
import Amazonka.ECS.ListContainerInstances
import Amazonka.ECS.ListServices
import Amazonka.ECS.ListServicesByNamespace
import Amazonka.ECS.ListTagsForResource
import Amazonka.ECS.ListTaskDefinitionFamilies
import Amazonka.ECS.ListTaskDefinitions
import Amazonka.ECS.ListTasks
import Amazonka.ECS.PutAccountSetting
import Amazonka.ECS.PutAccountSettingDefault
import Amazonka.ECS.PutAttributes
import Amazonka.ECS.PutClusterCapacityProviders
import Amazonka.ECS.RegisterContainerInstance
import Amazonka.ECS.RegisterTaskDefinition
import Amazonka.ECS.RunTask
import Amazonka.ECS.StartTask
import Amazonka.ECS.StopTask
import Amazonka.ECS.SubmitAttachmentStateChanges
import Amazonka.ECS.SubmitContainerStateChange
import Amazonka.ECS.SubmitTaskStateChange
import Amazonka.ECS.TagResource
import Amazonka.ECS.Types
import Amazonka.ECS.UntagResource
import Amazonka.ECS.UpdateCapacityProvider
import Amazonka.ECS.UpdateCluster
import Amazonka.ECS.UpdateClusterSettings
import Amazonka.ECS.UpdateContainerAgent
import Amazonka.ECS.UpdateContainerInstancesState
import Amazonka.ECS.UpdateService
import Amazonka.ECS.UpdateServicePrimaryTaskSet
import Amazonka.ECS.UpdateTaskProtection
import Amazonka.ECS.UpdateTaskSet
import Amazonka.ECS.Waiters

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
