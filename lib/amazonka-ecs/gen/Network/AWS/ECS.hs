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
-- Amazon ECS lets you launch and stop container-based applications with simple API calls, allows you to get the state of your cluster from a centralized service, and gives you access to many familiar Amazon EC2 features.
-- You can use Amazon ECS to schedule the placement of containers across your cluster based on your resource needs, isolation policies, and availability requirements. Amazon ECS eliminates the need for you to operate your own cluster management and configuration management systems or worry about scaling your management infrastructure.
module Network.AWS.ECS
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** ServerException
    _ServerException,

    -- ** ClusterContainsTasksException
    _ClusterContainsTasksException,

    -- ** PlatformUnknownException
    _PlatformUnknownException,

    -- ** ClusterContainsServicesException
    _ClusterContainsServicesException,

    -- ** TaskSetNotFoundException
    _TaskSetNotFoundException,

    -- ** ClusterContainsContainerInstancesException
    _ClusterContainsContainerInstancesException,

    -- ** ServiceNotActiveException
    _ServiceNotActiveException,

    -- ** ClusterNotFoundException
    _ClusterNotFoundException,

    -- ** NoUpdateAvailableException
    _NoUpdateAvailableException,

    -- ** UnsupportedFeatureException
    _UnsupportedFeatureException,

    -- ** ServiceNotFoundException
    _ServiceNotFoundException,

    -- ** PlatformTaskDefinitionIncompatibilityException
    _PlatformTaskDefinitionIncompatibilityException,

    -- ** MissingVersionException
    _MissingVersionException,

    -- ** UpdateInProgressException
    _UpdateInProgressException,

    -- ** BlockedException
    _BlockedException,

    -- ** TargetNotFoundException
    _TargetNotFoundException,

    -- ** AttributeLimitExceededException
    _AttributeLimitExceededException,

    -- ** ClientException
    _ClientException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- * Waiters
    -- $waiters

    -- ** ServicesInactive
    mkServicesInactive,

    -- ** TasksRunning
    mkTasksRunning,

    -- ** TasksStopped
    mkTasksStopped,

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

    -- ** DesiredStatus
    DesiredStatus (..),

    -- ** TaskDefinitionPlacementConstraintType
    TaskDefinitionPlacementConstraintType (..),

    -- ** DockerVolumeConfiguration
    DockerVolumeConfiguration (..),
    mkDockerVolumeConfiguration,
    dvcAutoprovision,
    dvcDriver,
    dvcDriverOpts,
    dvcLabels,
    dvcScope,

    -- ** ProxyConfigurationType
    ProxyConfigurationType (..),

    -- ** Attribute
    Attribute (..),
    mkAttribute,
    aName,
    aTargetId,
    aTargetType,
    aValue,

    -- ** HostEntry
    HostEntry (..),
    mkHostEntry,
    heHostname,
    heIpAddress,

    -- ** ManagedScalingStatus
    ManagedScalingStatus (..),

    -- ** ContainerInstanceField
    ContainerInstanceField (..),

    -- ** PlacementConstraint
    PlacementConstraint (..),
    mkPlacementConstraint,
    pcExpression,
    pcType,

    -- ** FirelensConfigurationType
    FirelensConfigurationType (..),

    -- ** TaskField
    TaskField (..),

    -- ** UlimitName
    UlimitName (..),

    -- ** TaskSet
    TaskSet (..),
    mkTaskSet,
    tsCapacityProviderStrategy,
    tsClusterArn,
    tsComputedDesiredCount,
    tsCreatedAt,
    tsExternalId,
    tsId,
    tsLaunchType,
    tsLoadBalancers,
    tsNetworkConfiguration,
    tsPendingCount,
    tsPlatformVersion,
    tsRunningCount,
    tsScale,
    tsServiceArn,
    tsServiceRegistries,
    tsStabilityStatus,
    tsStabilityStatusAt,
    tsStartedBy,
    tsStatus,
    tsTags,
    tsTaskDefinition,
    tsTaskSetArn,
    tsUpdatedAt,

    -- ** NetworkBinding
    NetworkBinding (..),
    mkNetworkBinding,
    nbBindIP,
    nbContainerPort,
    nbHostPort,
    nbProtocol,

    -- ** PlatformDevice
    PlatformDevice (..),
    mkPlatformDevice,
    pdId,
    pdType,

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** RepositoryCredentials
    RepositoryCredentials (..),
    mkRepositoryCredentials,
    rcCredentialsParameter,

    -- ** Cluster
    Cluster (..),
    mkCluster,
    cActiveServicesCount,
    cAttachments,
    cAttachmentsStatus,
    cCapacityProviders,
    cClusterArn,
    cClusterName,
    cDefaultCapacityProviderStrategy,
    cPendingTasksCount,
    cRegisteredContainerInstancesCount,
    cRunningTasksCount,
    cSettings,
    cStatistics,
    cStatus,
    cTags,

    -- ** PlacementStrategyType
    PlacementStrategyType (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** ContainerInstanceStatus
    ContainerInstanceStatus (..),

    -- ** HealthCheck
    HealthCheck (..),
    mkHealthCheck,
    hcCommand,
    hcInterval,
    hcRetries,
    hcStartPeriod,
    hcTimeout,

    -- ** PropagateTags
    PropagateTags (..),

    -- ** TaskDefinitionFamilyStatus
    TaskDefinitionFamilyStatus (..),

    -- ** KernelCapabilities
    KernelCapabilities (..),
    mkKernelCapabilities,
    kcAdd,
    kcDrop,

    -- ** String
    String (..),

    -- ** TransportProtocol
    TransportProtocol (..),

    -- ** ContainerService
    ContainerService (..),
    mkContainerService,
    csCapacityProviderStrategy,
    csClusterArn,
    csCreatedAt,
    csCreatedBy,
    csDeploymentConfiguration,
    csDeploymentController,
    csDeployments,
    csDesiredCount,
    csEnableECSManagedTags,
    csEvents,
    csHealthCheckGracePeriodSeconds,
    csLaunchType,
    csLoadBalancers,
    csNetworkConfiguration,
    csPendingCount,
    csPlacementConstraints,
    csPlacementStrategy,
    csPlatformVersion,
    csPropagateTags,
    csRoleArn,
    csRunningCount,
    csSchedulingStrategy,
    csServiceArn,
    csServiceName,
    csServiceRegistries,
    csStatus,
    csTags,
    csTaskDefinition,
    csTaskSets,

    -- ** DeploymentControllerType
    DeploymentControllerType (..),

    -- ** AutoScalingGroupProviderUpdate
    AutoScalingGroupProviderUpdate (..),
    mkAutoScalingGroupProviderUpdate,
    asgpuManagedScaling,
    asgpuManagedTerminationProtection,

    -- ** CapacityProvider
    CapacityProvider (..),
    mkCapacityProvider,
    cpAutoScalingGroupProvider,
    cpCapacityProviderArn,
    cpName,
    cpStatus,
    cpTags,
    cpUpdateStatus,
    cpUpdateStatusReason,

    -- ** EFSTransitEncryption
    EFSTransitEncryption (..),

    -- ** ClusterField
    ClusterField (..),

    -- ** ContainerDependency
    ContainerDependency (..),
    mkContainerDependency,
    cdContainerName,
    cdCondition,

    -- ** Device
    Device (..),
    mkDevice,
    dHostPath,
    dContainerPath,
    dPermissions,

    -- ** Scale
    Scale (..),
    mkScale,
    sUnit,
    sValue,

    -- ** Volume
    Volume (..),
    mkVolume,
    vDockerVolumeConfiguration,
    vEfsVolumeConfiguration,
    vFsxWindowsFileServerVolumeConfiguration,
    vHost,
    vName,

    -- ** AttachmentStateChange
    AttachmentStateChange (..),
    mkAttachmentStateChange,
    ascAttachmentArn,
    ascStatus,

    -- ** InferenceAcceleratorOverride
    InferenceAcceleratorOverride (..),
    mkInferenceAcceleratorOverride,
    iaoDeviceName,
    iaoDeviceType,

    -- ** ManagedScaling
    ManagedScaling (..),
    mkManagedScaling,
    msInstanceWarmupPeriod,
    msMaximumScalingStepSize,
    msMinimumScalingStepSize,
    msStatus,
    msTargetCapacity,

    -- ** ContainerOverride
    ContainerOverride (..),
    mkContainerOverride,
    coCommand,
    coCpu,
    coEnvironment,
    coEnvironmentFiles,
    coMemory,
    coMemoryReservation,
    coName,
    coResourceRequirements,

    -- ** NetworkInterface
    NetworkInterface (..),
    mkNetworkInterface,
    niAttachmentId,
    niIpv6Address,
    niPrivateIpv4Address,

    -- ** EnvironmentFile
    EnvironmentFile (..),
    mkEnvironmentFile,
    efValue,
    efType,

    -- ** ClusterSettingName
    ClusterSettingName (..),

    -- ** PidMode
    PidMode (..),

    -- ** KeyValuePair
    KeyValuePair (..),
    mkKeyValuePair,
    kvpName,
    kvpValue,

    -- ** Secret
    Secret (..),
    mkSecret,
    sName,
    sValueFrom,

    -- ** ContainerStateChange
    ContainerStateChange (..),
    mkContainerStateChange,
    cscContainerName,
    cscExitCode,
    cscImageDigest,
    cscNetworkBindings,
    cscReason,
    cscRuntimeId,
    cscStatus,

    -- ** VolumeFrom
    VolumeFrom (..),
    mkVolumeFrom,
    vfReadOnly,
    vfSourceContainer,

    -- ** Setting
    Setting (..),
    mkSetting,
    sfName,
    sfPrincipalArn,
    sfValue,

    -- ** FSxWindowsFileServerVolumeConfiguration
    FSxWindowsFileServerVolumeConfiguration (..),
    mkFSxWindowsFileServerVolumeConfiguration,
    fswfsvcFileSystemId,
    fswfsvcRootDirectory,
    fswfsvcAuthorizationConfig,

    -- ** EFSAuthorizationConfig
    EFSAuthorizationConfig (..),
    mkEFSAuthorizationConfig,
    efsacAccessPointId,
    efsacIam,

    -- ** AssignPublicIp
    AssignPublicIp (..),

    -- ** TaskOverride
    TaskOverride (..),
    mkTaskOverride,
    toContainerOverrides,
    toCpu,
    toExecutionRoleArn,
    toInferenceAcceleratorOverrides,
    toMemory,
    toTaskRoleArn,

    -- ** HostVolumeProperties
    HostVolumeProperties (..),
    mkHostVolumeProperties,
    hvpSourcePath,

    -- ** CapacityProviderStatus
    CapacityProviderStatus (..),

    -- ** TargetType
    TargetType (..),

    -- ** IpcMode
    IpcMode (..),

    -- ** SettingName
    SettingName (..),

    -- ** Attachment
    Attachment (..),
    mkAttachment,
    aDetails,
    aId,
    aStatus,
    aType,

    -- ** TaskDefinitionPlacementConstraint
    TaskDefinitionPlacementConstraint (..),
    mkTaskDefinitionPlacementConstraint,
    tdpcExpression,
    tdpcType,

    -- ** LinuxParameters
    LinuxParameters (..),
    mkLinuxParameters,
    lpCapabilities,
    lpDevices,
    lpInitProcessEnabled,
    lpMaxSwap,
    lpSharedMemorySize,
    lpSwappiness,
    lpTmpfs,

    -- ** Tmpfs
    Tmpfs (..),
    mkTmpfs,
    tContainerPath,
    tSize,
    tMountOptions,

    -- ** InferenceAccelerator
    InferenceAccelerator (..),
    mkInferenceAccelerator,
    iaDeviceName,
    iaDeviceType,

    -- ** LogConfiguration
    LogConfiguration (..),
    mkLogConfiguration,
    lcLogDriver,
    lcOptions,
    lcSecretOptions,

    -- ** EFSAuthorizationConfigIAM
    EFSAuthorizationConfigIAM (..),

    -- ** FirelensConfiguration
    FirelensConfiguration (..),
    mkFirelensConfiguration,
    fcType,
    fcOptions,

    -- ** ProxyConfiguration
    ProxyConfiguration (..),
    mkProxyConfiguration,
    pContainerName,
    pProperties,
    pType,

    -- ** PlacementConstraintType
    PlacementConstraintType (..),

    -- ** TaskDefinitionStatus
    TaskDefinitionStatus (..),

    -- ** ContainerCondition
    ContainerCondition (..),

    -- ** ClusterSetting
    ClusterSetting (..),
    mkClusterSetting,
    csName,
    csValue,

    -- ** SortOrder
    SortOrder (..),

    -- ** AutoScalingGroupProvider
    AutoScalingGroupProvider (..),
    mkAutoScalingGroupProvider,
    asgpAutoScalingGroupArn,
    asgpManagedScaling,
    asgpManagedTerminationProtection,

    -- ** AgentUpdateStatus
    AgentUpdateStatus (..),

    -- ** VersionInfo
    VersionInfo (..),
    mkVersionInfo,
    viAgentHash,
    viAgentVersion,
    viDockerVersion,

    -- ** ServiceField
    ServiceField (..),

    -- ** CapacityProviderField
    CapacityProviderField (..),

    -- ** Container
    Container (..),
    mkContainer,
    cContainerArn,
    cCpu,
    cExitCode,
    cGpuIds,
    cHealthStatus,
    cImage,
    cImageDigest,
    cLastStatus,
    cMemory,
    cMemoryReservation,
    cName,
    cNetworkBindings,
    cNetworkInterfaces,
    cReason,
    cRuntimeId,
    cTaskArn,

    -- ** PlatformDeviceType
    PlatformDeviceType (..),

    -- ** DeviceCgroupPermission
    DeviceCgroupPermission (..),

    -- ** LoadBalancer
    LoadBalancer (..),
    mkLoadBalancer,
    lbContainerName,
    lbContainerPort,
    lbLoadBalancerName,
    lbTargetGroupArn,

    -- ** TaskDefinitionField
    TaskDefinitionField (..),

    -- ** LogDriver
    LogDriver (..),

    -- ** ManagedTerminationProtection
    ManagedTerminationProtection (..),

    -- ** DeploymentCircuitBreaker
    DeploymentCircuitBreaker (..),
    mkDeploymentCircuitBreaker,
    dcbEnable,
    dcbRollback,

    -- ** SystemControl
    SystemControl (..),
    mkSystemControl,
    scNamespace,
    scValue,

    -- ** Scope
    Scope (..),

    -- ** ContainerDefinition
    ContainerDefinition (..),
    mkContainerDefinition,
    cdCommand,
    cdCpu,
    cdDependsOn,
    cdDisableNetworking,
    cdDnsSearchDomains,
    cdDnsServers,
    cdDockerLabels,
    cdDockerSecurityOptions,
    cdEntryPoint,
    cdEnvironment,
    cdEnvironmentFiles,
    cdEssential,
    cdExtraHosts,
    cdFirelensConfiguration,
    cdHealthCheck,
    cdHostname,
    cdImage,
    cdInteractive,
    cdLinks,
    cdLinuxParameters,
    cdLogConfiguration,
    cdMemory,
    cdMemoryReservation,
    cdMountPoints,
    cdName,
    cdPortMappings,
    cdPrivileged,
    cdPseudoTerminal,
    cdReadonlyRootFilesystem,
    cdRepositoryCredentials,
    cdResourceRequirements,
    cdSecrets,
    cdStartTimeout,
    cdStopTimeout,
    cdSystemControls,
    cdUlimits,
    cdUser,
    cdVolumesFrom,
    cdWorkingDirectory,

    -- ** PlacementStrategy
    PlacementStrategy (..),
    mkPlacementStrategy,
    psField,
    psType,

    -- ** Resource
    Resource (..),
    mkResource,
    rDoubleValue,
    rIntegerValue,
    rLongValue,
    rName,
    rStringSetValue,
    rType,

    -- ** DeploymentController
    DeploymentController (..),
    mkDeploymentController,
    dcType,

    -- ** Connectivity
    Connectivity (..),

    -- ** DeploymentRolloutState
    DeploymentRolloutState (..),

    -- ** TaskSetField
    TaskSetField (..),

    -- ** Task
    Task (..),
    mkTask,
    tAttachments,
    tAttributes,
    tAvailabilityZone,
    tCapacityProviderName,
    tClusterArn,
    tConnectivity,
    tConnectivityAt,
    tContainerInstanceArn,
    tContainers,
    tCpu,
    tCreatedAt,
    tDesiredStatus,
    tExecutionStoppedAt,
    tGroup,
    tHealthStatus,
    tInferenceAccelerators,
    tLastStatus,
    tLaunchType,
    tMemory,
    tOverrides,
    tPlatformVersion,
    tPullStartedAt,
    tPullStoppedAt,
    tStartedAt,
    tStartedBy,
    tStopCode,
    tStoppedAt,
    tStoppedReason,
    tStoppingAt,
    tTags,
    tTaskArn,
    tTaskDefinitionArn,
    tVersion,

    -- ** ResourceRequirement
    ResourceRequirement (..),
    mkResourceRequirement,
    rrValue,
    rrType,

    -- ** PortMapping
    PortMapping (..),
    mkPortMapping,
    pmContainerPort,
    pmHostPort,
    pmProtocol,

    -- ** LaunchType
    LaunchType (..),

    -- ** NetworkMode
    NetworkMode (..),

    -- ** TagKey
    TagKey (..),

    -- ** TaskDefinition
    TaskDefinition (..),
    mkTaskDefinition,
    tdCompatibilities,
    tdContainerDefinitions,
    tdCpu,
    tdExecutionRoleArn,
    tdFamily,
    tdInferenceAccelerators,
    tdIpcMode,
    tdMemory,
    tdNetworkMode,
    tdPidMode,
    tdPlacementConstraints,
    tdProxyConfiguration,
    tdRequiresAttributes,
    tdRequiresCompatibilities,
    tdRevision,
    tdStatus,
    tdTaskDefinitionArn,
    tdTaskRoleArn,
    tdVolumes,

    -- ** Ulimit
    Ulimit (..),
    mkUlimit,
    uName,
    uSoftLimit,
    uHardLimit,

    -- ** ServiceRegistry
    ServiceRegistry (..),
    mkServiceRegistry,
    srContainerName,
    srContainerPort,
    srPort,
    srRegistryArn,

    -- ** CapacityProviderUpdateStatus
    CapacityProviderUpdateStatus (..),

    -- ** EFSVolumeConfiguration
    EFSVolumeConfiguration (..),
    mkEFSVolumeConfiguration,
    efsvcFileSystemId,
    efsvcAuthorizationConfig,
    efsvcRootDirectory,
    efsvcTransitEncryption,
    efsvcTransitEncryptionPort,

    -- ** HealthStatus
    HealthStatus (..),

    -- ** FSxWindowsFileServerAuthorizationConfig
    FSxWindowsFileServerAuthorizationConfig (..),
    mkFSxWindowsFileServerAuthorizationConfig,
    fswfsacCredentialsParameter,
    fswfsacDomain,

    -- ** TaskStopCode
    TaskStopCode (..),

    -- ** AwsVpcConfiguration
    AwsVpcConfiguration (..),
    mkAwsVpcConfiguration,
    avcSubnets,
    avcAssignPublicIp,
    avcSecurityGroups,

    -- ** Failure
    Failure (..),
    mkFailure,
    fArn,
    fDetail,
    fReason,

    -- ** CapacityProviderStrategyItem
    CapacityProviderStrategyItem (..),
    mkCapacityProviderStrategyItem,
    cpsiCapacityProvider,
    cpsiBase,
    cpsiWeight,

    -- ** EnvironmentFileType
    EnvironmentFileType (..),

    -- ** SchedulingStrategy
    SchedulingStrategy (..),

    -- ** ContainerInstance
    ContainerInstance (..),
    mkContainerInstance,
    ciAgentConnected,
    ciAgentUpdateStatus,
    ciAttachments,
    ciAttributes,
    ciCapacityProviderName,
    ciContainerInstanceArn,
    ciEc2InstanceId,
    ciPendingTasksCount,
    ciRegisteredAt,
    ciRegisteredResources,
    ciRemainingResources,
    ciRunningTasksCount,
    ciStatus,
    ciStatusReason,
    ciTags,
    ciVersion,
    ciVersionInfo,

    -- ** ScaleUnit
    ScaleUnit (..),

    -- ** NetworkConfiguration
    NetworkConfiguration (..),
    mkNetworkConfiguration,
    ncAwsvpcConfiguration,

    -- ** Compatibility
    Compatibility (..),

    -- ** StabilityStatus
    StabilityStatus (..),

    -- ** ServiceEvent
    ServiceEvent (..),
    mkServiceEvent,
    seCreatedAt,
    seId,
    seMessage,

    -- ** DeploymentConfiguration
    DeploymentConfiguration (..),
    mkDeploymentConfiguration,
    dcDeploymentCircuitBreaker,
    dcMaximumPercent,
    dcMinimumHealthyPercent,

    -- ** Deployment
    Deployment (..),
    mkDeployment,
    dCapacityProviderStrategy,
    dCreatedAt,
    dDesiredCount,
    dFailedTasks,
    dId,
    dLaunchType,
    dNetworkConfiguration,
    dPendingCount,
    dPlatformVersion,
    dRolloutState,
    dRolloutStateReason,
    dRunningCount,
    dStatus,
    dTaskDefinition,
    dUpdatedAt,

    -- ** MountPoint
    MountPoint (..),
    mkMountPoint,
    mpContainerPath,
    mpReadOnly,
    mpSourceVolume,

    -- ** Acknowledgment
    Acknowledgment (..),

    -- ** Reason
    Reason (..),

    -- ** Endpoint
    Endpoint (..),

    -- ** TelemetryEndpoint
    TelemetryEndpoint (..),

    -- ** Driver
    Driver (..),

    -- ** Name
    Name (..),

    -- ** TargetId
    TargetId (..),

    -- ** Value
    Value (..),

    -- ** Hostname
    Hostname (..),

    -- ** IpAddress
    IpAddress (..),

    -- ** Expression
    Expression (..),

    -- ** ClusterArn
    ClusterArn (..),

    -- ** ExternalId
    ExternalId (..),

    -- ** Id
    Id (..),

    -- ** PlatformVersion
    PlatformVersion (..),

    -- ** ServiceArn
    ServiceArn (..),

    -- ** StartedBy
    StartedBy (..),

    -- ** Status
    Status (..),

    -- ** TaskSetArn
    TaskSetArn (..),

    -- ** PrincipalArn
    PrincipalArn (..),

    -- ** NextToken
    NextToken (..),

    -- ** ClusterName
    ClusterName (..),

    -- ** BindIP
    BindIP (..),

    -- ** Key
    Key (..),

    -- ** CredentialsParameter
    CredentialsParameter (..),

    -- ** AttachmentsStatus
    AttachmentsStatus (..),

    -- ** Family
    Family (..),

    -- ** ServiceName
    ServiceName (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
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
import qualified Network.AWS.Prelude as Lude

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
