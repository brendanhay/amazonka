{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Elastic Container Service__
--
-- Amazon Elastic Container Service (Amazon ECS) is a highly scalable, fast, container management service that makes it easy to run, stop, and manage Docker containers on a cluster. You can host your cluster on a serverless infrastructure that is managed by Amazon ECS by launching your services or tasks using the Fargate launch type. For more control, you can host your tasks on a cluster of Amazon Elastic Compute Cloud (Amazon EC2) instances that you manage by using the EC2 launch type. For more information about launch types, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> .
--
-- Amazon ECS lets you launch and stop container-based applications with simple API calls, allows you to get the state of your cluster from a centralized service, and gives you access to many familiar Amazon EC2 features.
--
-- You can use Amazon ECS to schedule the placement of containers across your cluster based on your resource needs, isolation policies, and availability requirements. Amazon ECS eliminates the need for you to operate your own cluster management and configuration management systems or worry about scaling your management infrastructure.
module Network.AWS.ECS
  ( -- * Service Configuration
    ecs,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- ** ServicesInactive
    servicesInactive,

    -- ** TasksRunning
    tasksRunning,

    -- ** TasksStopped
    tasksStopped,

    -- * Operations
    -- $operations

    -- ** ListServices (Paginated)
    module Network.AWS.ECS.ListServices,

    -- ** DescribeClusters
    module Network.AWS.ECS.DescribeClusters,

    -- ** DeleteService
    module Network.AWS.ECS.DeleteService,

    -- ** UpdateService
    module Network.AWS.ECS.UpdateService,

    -- ** DiscoverPollEndpoint
    module Network.AWS.ECS.DiscoverPollEndpoint,

    -- ** SubmitAttachmentStateChanges
    module Network.AWS.ECS.SubmitAttachmentStateChanges,

    -- ** SubmitContainerStateChange
    module Network.AWS.ECS.SubmitContainerStateChange,

    -- ** ListTagsForResource
    module Network.AWS.ECS.ListTagsForResource,

    -- ** StopTask
    module Network.AWS.ECS.StopTask,

    -- ** DescribeTaskDefinition
    module Network.AWS.ECS.DescribeTaskDefinition,

    -- ** SubmitTaskStateChange
    module Network.AWS.ECS.SubmitTaskStateChange,

    -- ** DescribeContainerInstances
    module Network.AWS.ECS.DescribeContainerInstances,

    -- ** DescribeCapacityProviders
    module Network.AWS.ECS.DescribeCapacityProviders,

    -- ** UpdateContainerInstancesState
    module Network.AWS.ECS.UpdateContainerInstancesState,

    -- ** DeleteCluster
    module Network.AWS.ECS.DeleteCluster,

    -- ** CreateCluster
    module Network.AWS.ECS.CreateCluster,

    -- ** PutAccountSetting
    module Network.AWS.ECS.PutAccountSetting,

    -- ** DeleteAccountSetting
    module Network.AWS.ECS.DeleteAccountSetting,

    -- ** ListTaskDefinitions (Paginated)
    module Network.AWS.ECS.ListTaskDefinitions,

    -- ** RunTask
    module Network.AWS.ECS.RunTask,

    -- ** DeleteCapacityProvider
    module Network.AWS.ECS.DeleteCapacityProvider,

    -- ** ListTasks (Paginated)
    module Network.AWS.ECS.ListTasks,

    -- ** UpdateCapacityProvider
    module Network.AWS.ECS.UpdateCapacityProvider,

    -- ** RegisterContainerInstance
    module Network.AWS.ECS.RegisterContainerInstance,

    -- ** UpdateContainerAgent
    module Network.AWS.ECS.UpdateContainerAgent,

    -- ** ListContainerInstances (Paginated)
    module Network.AWS.ECS.ListContainerInstances,

    -- ** UpdateServicePrimaryTaskSet
    module Network.AWS.ECS.UpdateServicePrimaryTaskSet,

    -- ** ListTaskDefinitionFamilies (Paginated)
    module Network.AWS.ECS.ListTaskDefinitionFamilies,

    -- ** StartTask
    module Network.AWS.ECS.StartTask,

    -- ** PutClusterCapacityProviders
    module Network.AWS.ECS.PutClusterCapacityProviders,

    -- ** PutAccountSettingDefault
    module Network.AWS.ECS.PutAccountSettingDefault,

    -- ** ListAttributes (Paginated)
    module Network.AWS.ECS.ListAttributes,

    -- ** DeregisterTaskDefinition
    module Network.AWS.ECS.DeregisterTaskDefinition,

    -- ** TagResource
    module Network.AWS.ECS.TagResource,

    -- ** CreateTaskSet
    module Network.AWS.ECS.CreateTaskSet,

    -- ** DescribeTasks
    module Network.AWS.ECS.DescribeTasks,

    -- ** ListClusters (Paginated)
    module Network.AWS.ECS.ListClusters,

    -- ** UntagResource
    module Network.AWS.ECS.UntagResource,

    -- ** DescribeServices
    module Network.AWS.ECS.DescribeServices,

    -- ** DeregisterContainerInstance
    module Network.AWS.ECS.DeregisterContainerInstance,

    -- ** UpdateClusterSettings
    module Network.AWS.ECS.UpdateClusterSettings,

    -- ** DeleteAttributes
    module Network.AWS.ECS.DeleteAttributes,

    -- ** PutAttributes
    module Network.AWS.ECS.PutAttributes,

    -- ** ListAccountSettings (Paginated)
    module Network.AWS.ECS.ListAccountSettings,

    -- ** DeleteTaskSet
    module Network.AWS.ECS.DeleteTaskSet,

    -- ** UpdateTaskSet
    module Network.AWS.ECS.UpdateTaskSet,

    -- ** CreateCapacityProvider
    module Network.AWS.ECS.CreateCapacityProvider,

    -- ** DescribeTaskSets
    module Network.AWS.ECS.DescribeTaskSets,

    -- ** RegisterTaskDefinition
    module Network.AWS.ECS.RegisterTaskDefinition,

    -- ** CreateService
    module Network.AWS.ECS.CreateService,

    -- * Types

    -- ** AgentUpdateStatus
    AgentUpdateStatus (..),

    -- ** AssignPublicIP
    AssignPublicIP (..),

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

    -- ** IPcMode
    IPcMode (..),

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

    -- ** AWSVPCConfiguration
    AWSVPCConfiguration,
    awsVPCConfiguration,
    avcSecurityGroups,
    avcAssignPublicIP,
    avcSubnets,

    -- ** Attachment
    Attachment,
    attachment,
    aStatus,
    aDetails,
    aId,
    aType,

    -- ** AttachmentStateChange
    AttachmentStateChange,
    attachmentStateChange,
    ascAttachmentARN,
    ascStatus,

    -- ** Attribute
    Attribute,
    attribute,
    aTargetId,
    aValue,
    aTargetType,
    aName,

    -- ** AutoScalingGroupProvider
    AutoScalingGroupProvider,
    autoScalingGroupProvider,
    asgpManagedScaling,
    asgpManagedTerminationProtection,
    asgpAutoScalingGroupARN,

    -- ** AutoScalingGroupProviderUpdate
    AutoScalingGroupProviderUpdate,
    autoScalingGroupProviderUpdate,
    asgpuManagedScaling,
    asgpuManagedTerminationProtection,

    -- ** CapacityProvider
    CapacityProvider,
    capacityProvider,
    cpStatus,
    cpUpdateStatusReason,
    cpAutoScalingGroupProvider,
    cpName,
    cpUpdateStatus,
    cpCapacityProviderARN,
    cpTags,

    -- ** CapacityProviderStrategyItem
    CapacityProviderStrategyItem,
    capacityProviderStrategyItem,
    cpsiBase,
    cpsiWeight,
    cpsiCapacityProvider,

    -- ** Cluster
    Cluster,
    cluster,
    cStatus,
    cClusterARN,
    cAttachments,
    cRunningTasksCount,
    cDefaultCapacityProviderStrategy,
    cSettings,
    cRegisteredContainerInstancesCount,
    cPendingTasksCount,
    cClusterName,
    cStatistics,
    cAttachmentsStatus,
    cCapacityProviders,
    cActiveServicesCount,
    cTags,

    -- ** ClusterSetting
    ClusterSetting,
    clusterSetting,
    csValue,
    csName,

    -- ** Container
    Container,
    container,
    cGpuIds,
    cNetworkBindings,
    cImage,
    cContainerARN,
    cNetworkInterfaces,
    cTaskARN,
    cLastStatus,
    cMemory,
    cReason,
    cName,
    cImageDigest,
    cExitCode,
    cHealthStatus,
    cCpu,
    cRuntimeId,
    cMemoryReservation,

    -- ** ContainerDefinition
    ContainerDefinition,
    containerDefinition,
    cdImage,
    cdCommand,
    cdHostname,
    cdRepositoryCredentials,
    cdDockerSecurityOptions,
    cdHealthCheck,
    cdDisableNetworking,
    cdSecrets,
    cdVolumesFrom,
    cdEnvironment,
    cdEnvironmentFiles,
    cdEntryPoint,
    cdWorkingDirectory,
    cdUlimits,
    cdStopTimeout,
    cdPrivileged,
    cdPortMappings,
    cdResourceRequirements,
    cdDockerLabels,
    cdExtraHosts,
    cdMemory,
    cdSystemControls,
    cdUser,
    cdFirelensConfiguration,
    cdDnsSearchDomains,
    cdLogConfiguration,
    cdLinuxParameters,
    cdPseudoTerminal,
    cdDependsOn,
    cdName,
    cdDnsServers,
    cdMountPoints,
    cdInteractive,
    cdStartTimeout,
    cdLinks,
    cdReadonlyRootFilesystem,
    cdEssential,
    cdCpu,
    cdMemoryReservation,

    -- ** ContainerDependency
    ContainerDependency,
    containerDependency,
    cdContainerName,
    cdCondition,

    -- ** ContainerInstance
    ContainerInstance,
    containerInstance,
    ciStatus,
    ciAttachments,
    ciRunningTasksCount,
    ciRemainingResources,
    ciEc2InstanceId,
    ciContainerInstanceARN,
    ciAgentConnected,
    ciVersionInfo,
    ciAgentUpdateStatus,
    ciAttributes,
    ciVersion,
    ciPendingTasksCount,
    ciCapacityProviderName,
    ciRegisteredAt,
    ciStatusReason,
    ciTags,
    ciRegisteredResources,

    -- ** ContainerOverride
    ContainerOverride,
    containerOverride,
    coCommand,
    coEnvironment,
    coEnvironmentFiles,
    coResourceRequirements,
    coMemory,
    coName,
    coCpu,
    coMemoryReservation,

    -- ** ContainerService
    ContainerService,
    containerService,
    csTaskSets,
    csRunningCount,
    csStatus,
    csClusterARN,
    csPropagateTags,
    csCreatedAt,
    csPlatformVersion,
    csEnableECSManagedTags,
    csCreatedBy,
    csDesiredCount,
    csLoadBalancers,
    csPendingCount,
    csPlacementConstraints,
    csEvents,
    csPlacementStrategy,
    csDeployments,
    csServiceName,
    csDeploymentController,
    csLaunchType,
    csServiceARN,
    csTaskDefinition,
    csSchedulingStrategy,
    csHealthCheckGracePeriodSeconds,
    csNetworkConfiguration,
    csServiceRegistries,
    csCapacityProviderStrategy,
    csTags,
    csRoleARN,
    csDeploymentConfiguration,

    -- ** ContainerStateChange
    ContainerStateChange,
    containerStateChange,
    cscNetworkBindings,
    cscStatus,
    cscContainerName,
    cscReason,
    cscImageDigest,
    cscExitCode,
    cscRuntimeId,

    -- ** Deployment
    Deployment,
    deployment,
    dRolloutState,
    dRunningCount,
    dStatus,
    dCreatedAt,
    dPlatformVersion,
    dDesiredCount,
    dPendingCount,
    dId,
    dFailedTasks,
    dLaunchType,
    dUpdatedAt,
    dTaskDefinition,
    dRolloutStateReason,
    dNetworkConfiguration,
    dCapacityProviderStrategy,

    -- ** DeploymentCircuitBreaker
    DeploymentCircuitBreaker,
    deploymentCircuitBreaker,
    dcbEnable,
    dcbRollback,

    -- ** DeploymentConfiguration
    DeploymentConfiguration,
    deploymentConfiguration,
    dcMinimumHealthyPercent,
    dcMaximumPercent,
    dcDeploymentCircuitBreaker,

    -- ** DeploymentController
    DeploymentController,
    deploymentController,
    dcType,

    -- ** Device
    Device,
    device,
    dContainerPath,
    dPermissions,
    dHostPath,

    -- ** DockerVolumeConfiguration
    DockerVolumeConfiguration,
    dockerVolumeConfiguration,
    dvcDriverOpts,
    dvcDriver,
    dvcScope,
    dvcLabels,
    dvcAutoprovision,

    -- ** EFSAuthorizationConfig
    EFSAuthorizationConfig,
    eFSAuthorizationConfig,
    efsacAccessPointId,
    efsacIam,

    -- ** EFSVolumeConfiguration
    EFSVolumeConfiguration,
    eFSVolumeConfiguration,
    efsvcRootDirectory,
    efsvcTransitEncryption,
    efsvcAuthorizationConfig,
    efsvcTransitEncryptionPort,
    efsvcFileSystemId,

    -- ** EnvironmentFile
    EnvironmentFile,
    environmentFile,
    efValue,
    efType,

    -- ** FSxWindowsFileServerAuthorizationConfig
    FSxWindowsFileServerAuthorizationConfig,
    fSxWindowsFileServerAuthorizationConfig,
    fswfsacCredentialsParameter,
    fswfsacDomain,

    -- ** FSxWindowsFileServerVolumeConfiguration
    FSxWindowsFileServerVolumeConfiguration,
    fSxWindowsFileServerVolumeConfiguration,
    fswfsvcFileSystemId,
    fswfsvcRootDirectory,
    fswfsvcAuthorizationConfig,

    -- ** Failure
    Failure,
    failure,
    fArn,
    fReason,
    fDetail,

    -- ** FirelensConfiguration
    FirelensConfiguration,
    firelensConfiguration,
    fcOptions,
    fcType,

    -- ** HealthCheck
    HealthCheck,
    healthCheck,
    hcStartPeriod,
    hcRetries,
    hcInterval,
    hcTimeout,
    hcCommand,

    -- ** HostEntry
    HostEntry,
    hostEntry,
    heHostname,
    heIpAddress,

    -- ** HostVolumeProperties
    HostVolumeProperties,
    hostVolumeProperties,
    hvpSourcePath,

    -- ** InferenceAccelerator
    InferenceAccelerator,
    inferenceAccelerator,
    iaDeviceName,
    iaDeviceType,

    -- ** InferenceAcceleratorOverride
    InferenceAcceleratorOverride,
    inferenceAcceleratorOverride,
    iaoDeviceName,
    iaoDeviceType,

    -- ** KernelCapabilities
    KernelCapabilities,
    kernelCapabilities,
    kcDrop,
    kcAdd,

    -- ** KeyValuePair
    KeyValuePair,
    keyValuePair,
    kvpValue,
    kvpName,

    -- ** LinuxParameters
    LinuxParameters,
    linuxParameters,
    lpSharedMemorySize,
    lpInitProcessEnabled,
    lpTmpfs,
    lpSwappiness,
    lpDevices,
    lpCapabilities,
    lpMaxSwap,

    -- ** LoadBalancer
    LoadBalancer,
    loadBalancer,
    lbLoadBalancerName,
    lbContainerName,
    lbTargetGroupARN,
    lbContainerPort,

    -- ** LogConfiguration
    LogConfiguration,
    logConfiguration,
    lcOptions,
    lcSecretOptions,
    lcLogDriver,

    -- ** ManagedScaling
    ManagedScaling,
    managedScaling,
    msStatus,
    msMaximumScalingStepSize,
    msTargetCapacity,
    msMinimumScalingStepSize,
    msInstanceWarmupPeriod,

    -- ** MountPoint
    MountPoint,
    mountPoint,
    mpContainerPath,
    mpSourceVolume,
    mpReadOnly,

    -- ** NetworkBinding
    NetworkBinding,
    networkBinding,
    nbBindIP,
    nbProtocol,
    nbHostPort,
    nbContainerPort,

    -- ** NetworkConfiguration
    NetworkConfiguration,
    networkConfiguration,
    ncAwsvpcConfiguration,

    -- ** NetworkInterface
    NetworkInterface,
    networkInterface,
    niIpv6Address,
    niPrivateIPv4Address,
    niAttachmentId,

    -- ** PlacementConstraint
    PlacementConstraint,
    placementConstraint,
    pcExpression,
    pcType,

    -- ** PlacementStrategy
    PlacementStrategy,
    placementStrategy,
    psField,
    psType,

    -- ** PlatformDevice
    PlatformDevice,
    platformDevice,
    pdId,
    pdType,

    -- ** PortMapping
    PortMapping,
    portMapping,
    pmProtocol,
    pmHostPort,
    pmContainerPort,

    -- ** ProxyConfiguration
    ProxyConfiguration,
    proxyConfiguration,
    pType,
    pProperties,
    pContainerName,

    -- ** RepositoryCredentials
    RepositoryCredentials,
    repositoryCredentials,
    rcCredentialsParameter,

    -- ** Resource
    Resource,
    resource,
    rStringSetValue,
    rIntegerValue,
    rDoubleValue,
    rLongValue,
    rName,
    rType,

    -- ** ResourceRequirement
    ResourceRequirement,
    resourceRequirement,
    rrValue,
    rrType,

    -- ** Scale
    Scale,
    scale,
    sValue,
    sUnit,

    -- ** Secret
    Secret,
    secret,
    sName,
    sValueFrom,

    -- ** ServiceEvent
    ServiceEvent,
    serviceEvent,
    seCreatedAt,
    seId,
    seMessage,

    -- ** ServiceRegistry
    ServiceRegistry,
    serviceRegistry,
    srRegistryARN,
    srContainerName,
    srContainerPort,
    srPort,

    -- ** Setting
    Setting,
    setting,
    setValue,
    setName,
    setPrincipalARN,

    -- ** SystemControl
    SystemControl,
    systemControl,
    scValue,
    scNamespace,

    -- ** Tag
    Tag,
    tag,
    tagValue,
    tagKey,

    -- ** Task
    Task,
    task,
    tStoppedAt,
    tDesiredStatus,
    tOverrides,
    tInferenceAccelerators,
    tClusterARN,
    tGroup,
    tAttachments,
    tCreatedAt,
    tStopCode,
    tPlatformVersion,
    tTaskARN,
    tContainerInstanceARN,
    tExecutionStoppedAt,
    tLastStatus,
    tMemory,
    tPullStoppedAt,
    tContainers,
    tStartedAt,
    tAvailabilityZone,
    tAttributes,
    tVersion,
    tCapacityProviderName,
    tStartedBy,
    tStoppedReason,
    tConnectivity,
    tStoppingAt,
    tLaunchType,
    tTaskDefinitionARN,
    tHealthStatus,
    tConnectivityAt,
    tCpu,
    tPullStartedAt,
    tTags,

    -- ** TaskDefinition
    TaskDefinition,
    taskDefinition,
    tdStatus,
    tdInferenceAccelerators,
    tdExecutionRoleARN,
    tdRequiresCompatibilities,
    tdPidMode,
    tdFamily,
    tdIpcMode,
    tdContainerDefinitions,
    tdMemory,
    tdProxyConfiguration,
    tdTaskRoleARN,
    tdPlacementConstraints,
    tdNetworkMode,
    tdTaskDefinitionARN,
    tdCompatibilities,
    tdRevision,
    tdVolumes,
    tdCpu,
    tdRequiresAttributes,

    -- ** TaskDefinitionPlacementConstraint
    TaskDefinitionPlacementConstraint,
    taskDefinitionPlacementConstraint,
    tdpcExpression,
    tdpcType,

    -- ** TaskOverride
    TaskOverride,
    taskOverride,
    toContainerOverrides,
    toExecutionRoleARN,
    toMemory,
    toTaskRoleARN,
    toInferenceAcceleratorOverrides,
    toCpu,

    -- ** TaskSet
    TaskSet,
    taskSet,
    tsRunningCount,
    tsStatus,
    tsClusterARN,
    tsComputedDesiredCount,
    tsCreatedAt,
    tsPlatformVersion,
    tsScale,
    tsLoadBalancers,
    tsStabilityStatusAt,
    tsPendingCount,
    tsTaskSetARN,
    tsStartedBy,
    tsId,
    tsLaunchType,
    tsUpdatedAt,
    tsServiceARN,
    tsTaskDefinition,
    tsExternalId,
    tsNetworkConfiguration,
    tsServiceRegistries,
    tsCapacityProviderStrategy,
    tsStabilityStatus,
    tsTags,

    -- ** Tmpfs
    Tmpfs,
    tmpfs,
    tMountOptions,
    tContainerPath,
    tSize,

    -- ** Ulimit
    Ulimit,
    ulimit,
    uName,
    uSoftLimit,
    uHardLimit,

    -- ** VersionInfo
    VersionInfo,
    versionInfo,
    viAgentHash,
    viAgentVersion,
    viDockerVersion,

    -- ** Volume
    Volume,
    volume,
    vDockerVolumeConfiguration,
    vFsxWindowsFileServerVolumeConfiguration,
    vName,
    vEfsVolumeConfiguration,
    vHost,

    -- ** VolumeFrom
    VolumeFrom,
    volumeFrom,
    vfSourceContainer,
    vfReadOnly,
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
