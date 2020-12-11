-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types
  ( -- * Service configuration
    ecsService,

    -- * Errors

    -- * AgentUpdateStatus
    AgentUpdateStatus (..),

    -- * AssignPublicIP
    AssignPublicIP (..),

    -- * CapacityProviderField
    CapacityProviderField (..),

    -- * CapacityProviderStatus
    CapacityProviderStatus (..),

    -- * CapacityProviderUpdateStatus
    CapacityProviderUpdateStatus (..),

    -- * ClusterField
    ClusterField (..),

    -- * ClusterSettingName
    ClusterSettingName (..),

    -- * Compatibility
    Compatibility (..),

    -- * Connectivity
    Connectivity (..),

    -- * ContainerCondition
    ContainerCondition (..),

    -- * ContainerInstanceField
    ContainerInstanceField (..),

    -- * ContainerInstanceStatus
    ContainerInstanceStatus (..),

    -- * DeploymentControllerType
    DeploymentControllerType (..),

    -- * DeploymentRolloutState
    DeploymentRolloutState (..),

    -- * DesiredStatus
    DesiredStatus (..),

    -- * DeviceCgroupPermission
    DeviceCgroupPermission (..),

    -- * EFSAuthorizationConfigIAM
    EFSAuthorizationConfigIAM (..),

    -- * EFSTransitEncryption
    EFSTransitEncryption (..),

    -- * EnvironmentFileType
    EnvironmentFileType (..),

    -- * FirelensConfigurationType
    FirelensConfigurationType (..),

    -- * HealthStatus
    HealthStatus (..),

    -- * IPcMode
    IPcMode (..),

    -- * LaunchType
    LaunchType (..),

    -- * LogDriver
    LogDriver (..),

    -- * ManagedScalingStatus
    ManagedScalingStatus (..),

    -- * ManagedTerminationProtection
    ManagedTerminationProtection (..),

    -- * NetworkMode
    NetworkMode (..),

    -- * PidMode
    PidMode (..),

    -- * PlacementConstraintType
    PlacementConstraintType (..),

    -- * PlacementStrategyType
    PlacementStrategyType (..),

    -- * PlatformDeviceType
    PlatformDeviceType (..),

    -- * PropagateTags
    PropagateTags (..),

    -- * ProxyConfigurationType
    ProxyConfigurationType (..),

    -- * ResourceType
    ResourceType (..),

    -- * ScaleUnit
    ScaleUnit (..),

    -- * SchedulingStrategy
    SchedulingStrategy (..),

    -- * Scope
    Scope (..),

    -- * ServiceField
    ServiceField (..),

    -- * SettingName
    SettingName (..),

    -- * SortOrder
    SortOrder (..),

    -- * StabilityStatus
    StabilityStatus (..),

    -- * TargetType
    TargetType (..),

    -- * TaskDefinitionFamilyStatus
    TaskDefinitionFamilyStatus (..),

    -- * TaskDefinitionField
    TaskDefinitionField (..),

    -- * TaskDefinitionPlacementConstraintType
    TaskDefinitionPlacementConstraintType (..),

    -- * TaskDefinitionStatus
    TaskDefinitionStatus (..),

    -- * TaskField
    TaskField (..),

    -- * TaskSetField
    TaskSetField (..),

    -- * TaskStopCode
    TaskStopCode (..),

    -- * TransportProtocol
    TransportProtocol (..),

    -- * UlimitName
    UlimitName (..),

    -- * AWSVPCConfiguration
    AWSVPCConfiguration (..),
    mkAWSVPCConfiguration,
    avcSecurityGroups,
    avcAssignPublicIP,
    avcSubnets,

    -- * Attachment
    Attachment (..),
    mkAttachment,
    aStatus,
    aDetails,
    aId,
    aType,

    -- * AttachmentStateChange
    AttachmentStateChange (..),
    mkAttachmentStateChange,
    ascAttachmentARN,
    ascStatus,

    -- * Attribute
    Attribute (..),
    mkAttribute,
    aTargetId,
    aValue,
    aTargetType,
    aName,

    -- * AutoScalingGroupProvider
    AutoScalingGroupProvider (..),
    mkAutoScalingGroupProvider,
    asgpManagedScaling,
    asgpManagedTerminationProtection,
    asgpAutoScalingGroupARN,

    -- * AutoScalingGroupProviderUpdate
    AutoScalingGroupProviderUpdate (..),
    mkAutoScalingGroupProviderUpdate,
    asgpuManagedScaling,
    asgpuManagedTerminationProtection,

    -- * CapacityProvider
    CapacityProvider (..),
    mkCapacityProvider,
    cpStatus,
    cpUpdateStatusReason,
    cpAutoScalingGroupProvider,
    cpName,
    cpUpdateStatus,
    cpCapacityProviderARN,
    cpTags,

    -- * CapacityProviderStrategyItem
    CapacityProviderStrategyItem (..),
    mkCapacityProviderStrategyItem,
    cpsiBase,
    cpsiWeight,
    cpsiCapacityProvider,

    -- * Cluster
    Cluster (..),
    mkCluster,
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

    -- * ClusterSetting
    ClusterSetting (..),
    mkClusterSetting,
    csValue,
    csName,

    -- * Container
    Container (..),
    mkContainer,
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

    -- * ContainerDefinition
    ContainerDefinition (..),
    mkContainerDefinition,
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

    -- * ContainerDependency
    ContainerDependency (..),
    mkContainerDependency,
    cdContainerName,
    cdCondition,

    -- * ContainerInstance
    ContainerInstance (..),
    mkContainerInstance,
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

    -- * ContainerOverride
    ContainerOverride (..),
    mkContainerOverride,
    coCommand,
    coEnvironment,
    coEnvironmentFiles,
    coResourceRequirements,
    coMemory,
    coName,
    coCpu,
    coMemoryReservation,

    -- * ContainerService
    ContainerService (..),
    mkContainerService,
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

    -- * ContainerStateChange
    ContainerStateChange (..),
    mkContainerStateChange,
    cscNetworkBindings,
    cscStatus,
    cscContainerName,
    cscReason,
    cscImageDigest,
    cscExitCode,
    cscRuntimeId,

    -- * Deployment
    Deployment (..),
    mkDeployment,
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

    -- * DeploymentCircuitBreaker
    DeploymentCircuitBreaker (..),
    mkDeploymentCircuitBreaker,
    dcbEnable,
    dcbRollback,

    -- * DeploymentConfiguration
    DeploymentConfiguration (..),
    mkDeploymentConfiguration,
    dcMinimumHealthyPercent,
    dcMaximumPercent,
    dcDeploymentCircuitBreaker,

    -- * DeploymentController
    DeploymentController (..),
    mkDeploymentController,
    dcType,

    -- * Device
    Device (..),
    mkDevice,
    dContainerPath,
    dPermissions,
    dHostPath,

    -- * DockerVolumeConfiguration
    DockerVolumeConfiguration (..),
    mkDockerVolumeConfiguration,
    dvcDriverOpts,
    dvcDriver,
    dvcScope,
    dvcLabels,
    dvcAutoprovision,

    -- * EFSAuthorizationConfig
    EFSAuthorizationConfig (..),
    mkEFSAuthorizationConfig,
    efsacAccessPointId,
    efsacIam,

    -- * EFSVolumeConfiguration
    EFSVolumeConfiguration (..),
    mkEFSVolumeConfiguration,
    efsvcRootDirectory,
    efsvcTransitEncryption,
    efsvcAuthorizationConfig,
    efsvcTransitEncryptionPort,
    efsvcFileSystemId,

    -- * EnvironmentFile
    EnvironmentFile (..),
    mkEnvironmentFile,
    efValue,
    efType,

    -- * FSxWindowsFileServerAuthorizationConfig
    FSxWindowsFileServerAuthorizationConfig (..),
    mkFSxWindowsFileServerAuthorizationConfig,
    fswfsacCredentialsParameter,
    fswfsacDomain,

    -- * FSxWindowsFileServerVolumeConfiguration
    FSxWindowsFileServerVolumeConfiguration (..),
    mkFSxWindowsFileServerVolumeConfiguration,
    fswfsvcFileSystemId,
    fswfsvcRootDirectory,
    fswfsvcAuthorizationConfig,

    -- * Failure
    Failure (..),
    mkFailure,
    fArn,
    fReason,
    fDetail,

    -- * FirelensConfiguration
    FirelensConfiguration (..),
    mkFirelensConfiguration,
    fcOptions,
    fcType,

    -- * HealthCheck
    HealthCheck (..),
    mkHealthCheck,
    hcStartPeriod,
    hcRetries,
    hcInterval,
    hcTimeout,
    hcCommand,

    -- * HostEntry
    HostEntry (..),
    mkHostEntry,
    heHostname,
    heIpAddress,

    -- * HostVolumeProperties
    HostVolumeProperties (..),
    mkHostVolumeProperties,
    hvpSourcePath,

    -- * InferenceAccelerator
    InferenceAccelerator (..),
    mkInferenceAccelerator,
    iaDeviceName,
    iaDeviceType,

    -- * InferenceAcceleratorOverride
    InferenceAcceleratorOverride (..),
    mkInferenceAcceleratorOverride,
    iaoDeviceName,
    iaoDeviceType,

    -- * KernelCapabilities
    KernelCapabilities (..),
    mkKernelCapabilities,
    kcDrop,
    kcAdd,

    -- * KeyValuePair
    KeyValuePair (..),
    mkKeyValuePair,
    kvpValue,
    kvpName,

    -- * LinuxParameters
    LinuxParameters (..),
    mkLinuxParameters,
    lpSharedMemorySize,
    lpInitProcessEnabled,
    lpTmpfs,
    lpSwappiness,
    lpDevices,
    lpCapabilities,
    lpMaxSwap,

    -- * LoadBalancer
    LoadBalancer (..),
    mkLoadBalancer,
    lbLoadBalancerName,
    lbContainerName,
    lbTargetGroupARN,
    lbContainerPort,

    -- * LogConfiguration
    LogConfiguration (..),
    mkLogConfiguration,
    lcOptions,
    lcSecretOptions,
    lcLogDriver,

    -- * ManagedScaling
    ManagedScaling (..),
    mkManagedScaling,
    msStatus,
    msMaximumScalingStepSize,
    msTargetCapacity,
    msMinimumScalingStepSize,
    msInstanceWarmupPeriod,

    -- * MountPoint
    MountPoint (..),
    mkMountPoint,
    mpContainerPath,
    mpSourceVolume,
    mpReadOnly,

    -- * NetworkBinding
    NetworkBinding (..),
    mkNetworkBinding,
    nbBindIP,
    nbProtocol,
    nbHostPort,
    nbContainerPort,

    -- * NetworkConfiguration
    NetworkConfiguration (..),
    mkNetworkConfiguration,
    ncAwsvpcConfiguration,

    -- * NetworkInterface
    NetworkInterface (..),
    mkNetworkInterface,
    niIpv6Address,
    niPrivateIPv4Address,
    niAttachmentId,

    -- * PlacementConstraint
    PlacementConstraint (..),
    mkPlacementConstraint,
    pcExpression,
    pcType,

    -- * PlacementStrategy
    PlacementStrategy (..),
    mkPlacementStrategy,
    psField,
    psType,

    -- * PlatformDevice
    PlatformDevice (..),
    mkPlatformDevice,
    pdId,
    pdType,

    -- * PortMapping
    PortMapping (..),
    mkPortMapping,
    pmProtocol,
    pmHostPort,
    pmContainerPort,

    -- * ProxyConfiguration
    ProxyConfiguration (..),
    mkProxyConfiguration,
    pType,
    pProperties,
    pContainerName,

    -- * RepositoryCredentials
    RepositoryCredentials (..),
    mkRepositoryCredentials,
    rcCredentialsParameter,

    -- * Resource
    Resource (..),
    mkResource,
    rStringSetValue,
    rIntegerValue,
    rDoubleValue,
    rLongValue,
    rName,
    rType,

    -- * ResourceRequirement
    ResourceRequirement (..),
    mkResourceRequirement,
    rrValue,
    rrType,

    -- * Scale
    Scale (..),
    mkScale,
    sValue,
    sUnit,

    -- * Secret
    Secret (..),
    mkSecret,
    sName,
    sValueFrom,

    -- * ServiceEvent
    ServiceEvent (..),
    mkServiceEvent,
    seCreatedAt,
    seId,
    seMessage,

    -- * ServiceRegistry
    ServiceRegistry (..),
    mkServiceRegistry,
    srRegistryARN,
    srContainerName,
    srContainerPort,
    srPort,

    -- * Setting
    Setting (..),
    mkSetting,
    setValue,
    setName,
    setPrincipalARN,

    -- * SystemControl
    SystemControl (..),
    mkSystemControl,
    scValue,
    scNamespace,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * Task
    Task (..),
    mkTask,
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

    -- * TaskDefinition
    TaskDefinition (..),
    mkTaskDefinition,
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

    -- * TaskDefinitionPlacementConstraint
    TaskDefinitionPlacementConstraint (..),
    mkTaskDefinitionPlacementConstraint,
    tdpcExpression,
    tdpcType,

    -- * TaskOverride
    TaskOverride (..),
    mkTaskOverride,
    toContainerOverrides,
    toExecutionRoleARN,
    toMemory,
    toTaskRoleARN,
    toInferenceAcceleratorOverrides,
    toCpu,

    -- * TaskSet
    TaskSet (..),
    mkTaskSet,
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

    -- * Tmpfs
    Tmpfs (..),
    mkTmpfs,
    tMountOptions,
    tContainerPath,
    tSize,

    -- * Ulimit
    Ulimit (..),
    mkUlimit,
    uName,
    uSoftLimit,
    uHardLimit,

    -- * VersionInfo
    VersionInfo (..),
    mkVersionInfo,
    viAgentHash,
    viAgentVersion,
    viDockerVersion,

    -- * Volume
    Volume (..),
    mkVolume,
    vDockerVolumeConfiguration,
    vFsxWindowsFileServerVolumeConfiguration,
    vName,
    vEfsVolumeConfiguration,
    vHost,

    -- * VolumeFrom
    VolumeFrom (..),
    mkVolumeFrom,
    vfSourceContainer,
    vfReadOnly,
  )
where

import Network.AWS.ECS.Types.AWSVPCConfiguration
import Network.AWS.ECS.Types.AgentUpdateStatus
import Network.AWS.ECS.Types.AssignPublicIP
import Network.AWS.ECS.Types.Attachment
import Network.AWS.ECS.Types.AttachmentStateChange
import Network.AWS.ECS.Types.Attribute
import Network.AWS.ECS.Types.AutoScalingGroupProvider
import Network.AWS.ECS.Types.AutoScalingGroupProviderUpdate
import Network.AWS.ECS.Types.CapacityProvider
import Network.AWS.ECS.Types.CapacityProviderField
import Network.AWS.ECS.Types.CapacityProviderStatus
import Network.AWS.ECS.Types.CapacityProviderStrategyItem
import Network.AWS.ECS.Types.CapacityProviderUpdateStatus
import Network.AWS.ECS.Types.Cluster
import Network.AWS.ECS.Types.ClusterField
import Network.AWS.ECS.Types.ClusterSetting
import Network.AWS.ECS.Types.ClusterSettingName
import Network.AWS.ECS.Types.Compatibility
import Network.AWS.ECS.Types.Connectivity
import Network.AWS.ECS.Types.Container
import Network.AWS.ECS.Types.ContainerCondition
import Network.AWS.ECS.Types.ContainerDefinition
import Network.AWS.ECS.Types.ContainerDependency
import Network.AWS.ECS.Types.ContainerInstance
import Network.AWS.ECS.Types.ContainerInstanceField
import Network.AWS.ECS.Types.ContainerInstanceStatus
import Network.AWS.ECS.Types.ContainerOverride
import Network.AWS.ECS.Types.ContainerService
import Network.AWS.ECS.Types.ContainerStateChange
import Network.AWS.ECS.Types.Deployment
import Network.AWS.ECS.Types.DeploymentCircuitBreaker
import Network.AWS.ECS.Types.DeploymentConfiguration
import Network.AWS.ECS.Types.DeploymentController
import Network.AWS.ECS.Types.DeploymentControllerType
import Network.AWS.ECS.Types.DeploymentRolloutState
import Network.AWS.ECS.Types.DesiredStatus
import Network.AWS.ECS.Types.Device
import Network.AWS.ECS.Types.DeviceCgroupPermission
import Network.AWS.ECS.Types.DockerVolumeConfiguration
import Network.AWS.ECS.Types.EFSAuthorizationConfig
import Network.AWS.ECS.Types.EFSAuthorizationConfigIAM
import Network.AWS.ECS.Types.EFSTransitEncryption
import Network.AWS.ECS.Types.EFSVolumeConfiguration
import Network.AWS.ECS.Types.EnvironmentFile
import Network.AWS.ECS.Types.EnvironmentFileType
import Network.AWS.ECS.Types.FSxWindowsFileServerAuthorizationConfig
import Network.AWS.ECS.Types.FSxWindowsFileServerVolumeConfiguration
import Network.AWS.ECS.Types.Failure
import Network.AWS.ECS.Types.FirelensConfiguration
import Network.AWS.ECS.Types.FirelensConfigurationType
import Network.AWS.ECS.Types.HealthCheck
import Network.AWS.ECS.Types.HealthStatus
import Network.AWS.ECS.Types.HostEntry
import Network.AWS.ECS.Types.HostVolumeProperties
import Network.AWS.ECS.Types.IPcMode
import Network.AWS.ECS.Types.InferenceAccelerator
import Network.AWS.ECS.Types.InferenceAcceleratorOverride
import Network.AWS.ECS.Types.KernelCapabilities
import Network.AWS.ECS.Types.KeyValuePair
import Network.AWS.ECS.Types.LaunchType
import Network.AWS.ECS.Types.LinuxParameters
import Network.AWS.ECS.Types.LoadBalancer
import Network.AWS.ECS.Types.LogConfiguration
import Network.AWS.ECS.Types.LogDriver
import Network.AWS.ECS.Types.ManagedScaling
import Network.AWS.ECS.Types.ManagedScalingStatus
import Network.AWS.ECS.Types.ManagedTerminationProtection
import Network.AWS.ECS.Types.MountPoint
import Network.AWS.ECS.Types.NetworkBinding
import Network.AWS.ECS.Types.NetworkConfiguration
import Network.AWS.ECS.Types.NetworkInterface
import Network.AWS.ECS.Types.NetworkMode
import Network.AWS.ECS.Types.PidMode
import Network.AWS.ECS.Types.PlacementConstraint
import Network.AWS.ECS.Types.PlacementConstraintType
import Network.AWS.ECS.Types.PlacementStrategy
import Network.AWS.ECS.Types.PlacementStrategyType
import Network.AWS.ECS.Types.PlatformDevice
import Network.AWS.ECS.Types.PlatformDeviceType
import Network.AWS.ECS.Types.PortMapping
import Network.AWS.ECS.Types.PropagateTags
import Network.AWS.ECS.Types.ProxyConfiguration
import Network.AWS.ECS.Types.ProxyConfigurationType
import Network.AWS.ECS.Types.RepositoryCredentials
import Network.AWS.ECS.Types.Resource
import Network.AWS.ECS.Types.ResourceRequirement
import Network.AWS.ECS.Types.ResourceType
import Network.AWS.ECS.Types.Scale
import Network.AWS.ECS.Types.ScaleUnit
import Network.AWS.ECS.Types.SchedulingStrategy
import Network.AWS.ECS.Types.Scope
import Network.AWS.ECS.Types.Secret
import Network.AWS.ECS.Types.ServiceEvent
import Network.AWS.ECS.Types.ServiceField
import Network.AWS.ECS.Types.ServiceRegistry
import Network.AWS.ECS.Types.Setting
import Network.AWS.ECS.Types.SettingName
import Network.AWS.ECS.Types.SortOrder
import Network.AWS.ECS.Types.StabilityStatus
import Network.AWS.ECS.Types.SystemControl
import Network.AWS.ECS.Types.Tag
import Network.AWS.ECS.Types.TargetType
import Network.AWS.ECS.Types.Task
import Network.AWS.ECS.Types.TaskDefinition
import Network.AWS.ECS.Types.TaskDefinitionFamilyStatus
import Network.AWS.ECS.Types.TaskDefinitionField
import Network.AWS.ECS.Types.TaskDefinitionPlacementConstraint
import Network.AWS.ECS.Types.TaskDefinitionPlacementConstraintType
import Network.AWS.ECS.Types.TaskDefinitionStatus
import Network.AWS.ECS.Types.TaskField
import Network.AWS.ECS.Types.TaskOverride
import Network.AWS.ECS.Types.TaskSet
import Network.AWS.ECS.Types.TaskSetField
import Network.AWS.ECS.Types.TaskStopCode
import Network.AWS.ECS.Types.Tmpfs
import Network.AWS.ECS.Types.TransportProtocol
import Network.AWS.ECS.Types.Ulimit
import Network.AWS.ECS.Types.UlimitName
import Network.AWS.ECS.Types.VersionInfo
import Network.AWS.ECS.Types.Volume
import Network.AWS.ECS.Types.VolumeFrom
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-11-13@ of the Amazon EC2 Container Service SDK configuration.
ecsService :: Lude.Service
ecsService =
  Lude.Service
    { Lude._svcAbbrev = "ECS",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "ecs",
      Lude._svcVersion = "2014-11-13",
      Lude._svcEndpoint = Lude.defaultEndpoint ecsService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "ECS",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
