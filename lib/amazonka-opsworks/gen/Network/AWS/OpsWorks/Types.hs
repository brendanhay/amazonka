-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _ValidationException,
    _ResourceNotFoundException,

    -- * SslConfiguration
    SslConfiguration (..),
    mkSslConfiguration,
    scCertificate,
    scPrivateKey,
    scChain,

    -- * VirtualizationType
    VirtualizationType (..),

    -- * Command
    Command (..),
    mkCommand,
    cAcknowledgedAt,
    cCommandId,
    cCompletedAt,
    cCreatedAt,
    cDeploymentId,
    cExitCode,
    cInstanceId,
    cLogUrl,
    cStatus,
    cType,

    -- * RaidArray
    RaidArray (..),
    mkRaidArray,
    raAvailabilityZone,
    raCreatedAt,
    raDevice,
    raInstanceId,
    raIops,
    raMountPoint,
    raName,
    raNumberOfDisks,
    raRaidArrayId,
    raRaidLevel,
    raSize,
    raStackId,
    raVolumeType,

    -- * ElasticLoadBalancer
    ElasticLoadBalancer (..),
    mkElasticLoadBalancer,
    elbAvailabilityZones,
    elbDnsName,
    elbEc2InstanceIds,
    elbElasticLoadBalancerName,
    elbLayerId,
    elbRegion,
    elbStackId,
    elbSubnetIds,
    elbVpcId,

    -- * CloudWatchLogsConfiguration
    CloudWatchLogsConfiguration (..),
    mkCloudWatchLogsConfiguration,
    cwlcEnabled,
    cwlcLogStreams,

    -- * Switch
    Switch (..),

    -- * LifecycleEventConfiguration
    LifecycleEventConfiguration (..),
    mkLifecycleEventConfiguration,
    lecShutdown,

    -- * RdsDbInstance
    RdsDbInstance (..),
    mkRdsDbInstance,
    rdiAddress,
    rdiDbInstanceIdentifier,
    rdiDbPassword,
    rdiDbUser,
    rdiEngine,
    rdiMissingOnRds,
    rdiRdsDbInstanceArn,
    rdiRegion,
    rdiStackId,

    -- * AppAttributesKeys
    AppAttributesKeys (..),

    -- * StackSummary
    StackSummary (..),
    mkStackSummary,
    ssAppsCount,
    ssArn,
    ssInstancesCount,
    ssLayersCount,
    ssName,
    ssStackId,

    -- * BlockDeviceMapping
    BlockDeviceMapping (..),
    mkBlockDeviceMapping,
    bdmDeviceName,
    bdmEbs,
    bdmNoDevice,
    bdmVirtualName,

    -- * StackAttributesKeys
    StackAttributesKeys (..),

    -- * LoadBasedAutoScalingConfiguration
    LoadBasedAutoScalingConfiguration (..),
    mkLoadBasedAutoScalingConfiguration,
    lbascDownScaling,
    lbascEnable,
    lbascLayerId,
    lbascUpScaling,

    -- * String
    String (..),

    -- * SourceType
    SourceType (..),

    -- * OperatingSystem
    OperatingSystem (..),
    mkOperatingSystem,
    osConfigurationManagers,
    osId,
    osName,
    osReportedName,
    osReportedVersion,
    osSupported,
    osType,

    -- * Volume
    Volume (..),
    mkVolume,
    vAvailabilityZone,
    vDevice,
    vEc2VolumeId,
    vEncrypted,
    vInstanceId,
    vIops,
    vMountPoint,
    vName,
    vRaidArrayId,
    vRegion,
    vSize,
    vStatus,
    vVolumeId,
    vVolumeType,

    -- * CloudWatchLogsInitialPosition
    CloudWatchLogsInitialPosition (..),

    -- * ChefConfiguration
    ChefConfiguration (..),
    mkChefConfiguration,
    ccBerkshelfVersion,
    ccManageBerkshelf,

    -- * AgentVersion
    AgentVersion (..),
    mkAgentVersion,
    avConfigurationManager,
    avVersion,

    -- * LayerType
    LayerType (..),

    -- * AutoScalingThresholds
    AutoScalingThresholds (..),
    mkAutoScalingThresholds,
    astAlarms,
    astCpuThreshold,
    astIgnoreMetricsTime,
    astInstanceCount,
    astLoadThreshold,
    astMemoryThreshold,
    astThresholdsWaitTime,

    -- * TagValue
    TagValue (..),

    -- * App
    App (..),
    mkApp,
    aAppId,
    aAppSource,
    aAttributes,
    aCreatedAt,
    aDataSources,
    aDescription,
    aDomains,
    aEnableSsl,
    aEnvironment,
    aName,
    aShortname,
    aSslConfiguration,
    aStackId,
    aType,

    -- * ElasticIp
    ElasticIp (..),
    mkElasticIp,
    eiDomain,
    eiInstanceId,
    eiIp,
    eiName,
    eiRegion,

    -- * CloudWatchLogsEncoding
    CloudWatchLogsEncoding (..),

    -- * NextToken
    NextToken (..),

    -- * ShutdownEventConfiguration
    ShutdownEventConfiguration (..),
    mkShutdownEventConfiguration,
    secDelayUntilElbConnectionsDrained,
    secExecutionTimeout,

    -- * CloudWatchLogsTimeZone
    CloudWatchLogsTimeZone (..),

    -- * CloudWatchLogsLogStream
    CloudWatchLogsLogStream (..),
    mkCloudWatchLogsLogStream,
    cwllsBatchCount,
    cwllsBatchSize,
    cwllsBufferDuration,
    cwllsDatetimeFormat,
    cwllsEncoding,
    cwllsFile,
    cwllsFileFingerprintLines,
    cwllsInitialPosition,
    cwllsLogGroupName,
    cwllsMultiLineStartPattern,
    cwllsTimeZone,

    -- * ResourceArn
    ResourceArn (..),

    -- * EcsCluster
    EcsCluster (..),
    mkEcsCluster,
    ecEcsClusterArn,
    ecEcsClusterName,
    ecRegisteredAt,
    ecStackId,

    -- * InstanceIdentity
    InstanceIdentity (..),
    mkInstanceIdentity,
    iiDocument,
    iiSignature,

    -- * UserProfile
    UserProfile (..),
    mkUserProfile,
    upAllowSelfManagement,
    upIamUserArn,
    upName,
    upSshPublicKey,
    upSshUsername,

    -- * AutoScalingType
    AutoScalingType (..),

    -- * Source
    Source (..),
    mkSource,
    sPassword,
    sRevision,
    sSshKey,
    sType,
    sUrl,
    sUsername,

    -- * DataSource
    DataSource (..),
    mkDataSource,
    dsArn,
    dsDatabaseName,
    dsType,

    -- * Architecture
    Architecture (..),

    -- * StackConfigurationManager
    StackConfigurationManager (..),
    mkStackConfigurationManager,
    scmName,
    scmVersion,

    -- * EbsBlockDevice
    EbsBlockDevice (..),
    mkEbsBlockDevice,
    ebdDeleteOnTermination,
    ebdIops,
    ebdSnapshotId,
    ebdVolumeSize,
    ebdVolumeType,

    -- * ServiceError'
    ServiceError' (..),
    mkServiceError',
    seCreatedAt,
    seInstanceId,
    seMessage,
    seServiceErrorId,
    seStackId,
    seType,

    -- * TagKey
    TagKey (..),

    -- * Hour
    Hour (..),

    -- * LayerAttributesKeys
    LayerAttributesKeys (..),

    -- * TemporaryCredential
    TemporaryCredential (..),
    mkTemporaryCredential,
    tcInstanceId,
    tcPassword,
    tcUsername,
    tcValidForInMinutes,

    -- * VolumeConfiguration
    VolumeConfiguration (..),
    mkVolumeConfiguration,
    vcMountPoint,
    vcNumberOfDisks,
    vcSize,
    vcEncrypted,
    vcIops,
    vcRaidLevel,
    vcVolumeType,

    -- * VolumeType
    VolumeType (..),

    -- * ReportedOs
    ReportedOs (..),
    mkReportedOs,
    roFamily,
    roName,
    roVersion,

    -- * Permission
    Permission (..),
    mkPermission,
    pAllowSsh,
    pAllowSudo,
    pIamUserArn,
    pLevel,
    pStackId,

    -- * EnvironmentVariable
    EnvironmentVariable (..),
    mkEnvironmentVariable,
    evKey,
    evValue,
    evSecure,

    -- * OperatingSystemConfigurationManager
    OperatingSystemConfigurationManager (..),
    mkOperatingSystemConfigurationManager,
    oscmName,
    oscmVersion,

    -- * Layer
    Layer (..),
    mkLayer,
    lArn,
    lAttributes,
    lAutoAssignElasticIps,
    lAutoAssignPublicIps,
    lCloudWatchLogsConfiguration,
    lCreatedAt,
    lCustomInstanceProfileArn,
    lCustomJson,
    lCustomRecipes,
    lCustomSecurityGroupIds,
    lDefaultRecipes,
    lDefaultSecurityGroupNames,
    lEnableAutoHealing,
    lInstallUpdatesOnBoot,
    lLayerId,
    lLifecycleEventConfiguration,
    lName,
    lPackages,
    lShortname,
    lStackId,
    lType,
    lUseEbsOptimizedInstances,
    lVolumeConfigurations,

    -- * Recipes
    Recipes (..),
    mkRecipes,
    rConfigure,
    rDeploy,
    rSetup,
    rShutdown,
    rUndeploy,

    -- * TimeBasedAutoScalingConfiguration
    TimeBasedAutoScalingConfiguration (..),
    mkTimeBasedAutoScalingConfiguration,
    tbascAutoScalingSchedule,
    tbascInstanceId,

    -- * SelfUserProfile
    SelfUserProfile (..),
    mkSelfUserProfile,
    supIamUserArn,
    supName,
    supSshPublicKey,
    supSshUsername,

    -- * RootDeviceType
    RootDeviceType (..),

    -- * Stack
    Stack (..),
    mkStack,
    sAgentVersion,
    sArn,
    sAttributes,
    sChefConfiguration,
    sConfigurationManager,
    sCreatedAt,
    sCustomCookbooksSource,
    sCustomJson,
    sDefaultAvailabilityZone,
    sDefaultInstanceProfileArn,
    sDefaultOs,
    sDefaultRootDeviceType,
    sDefaultSshKeyName,
    sDefaultSubnetId,
    sHostnameTheme,
    sName,
    sRegion,
    sServiceRoleArn,
    sStackId,
    sUseCustomCookbooks,
    sUseOpsworksSecurityGroups,
    sVpcId,

    -- * DeploymentCommand
    DeploymentCommand (..),
    mkDeploymentCommand,
    dcName,
    dcArgs,

    -- * WeeklyAutoScalingSchedule
    WeeklyAutoScalingSchedule (..),
    mkWeeklyAutoScalingSchedule,
    wassFriday,
    wassMonday,
    wassSaturday,
    wassSunday,
    wassThursday,
    wassTuesday,
    wassWednesday,

    -- * DeploymentCommandName
    DeploymentCommandName (..),

    -- * DateTime
    DateTime (..),

    -- * Instance
    Instance (..),
    mkInstance,
    iAgentVersion,
    iAmiId,
    iArchitecture,
    iArn,
    iAutoScalingType,
    iAvailabilityZone,
    iBlockDeviceMappings,
    iCreatedAt,
    iEbsOptimized,
    iEc2InstanceId,
    iEcsClusterArn,
    iEcsContainerInstanceArn,
    iElasticIp,
    iHostname,
    iInfrastructureClass,
    iInstallUpdatesOnBoot,
    iInstanceId,
    iInstanceProfileArn,
    iInstanceType,
    iLastServiceErrorId,
    iLayerIds,
    iOs,
    iPlatform,
    iPrivateDns,
    iPrivateIp,
    iPublicDns,
    iPublicIp,
    iRegisteredBy,
    iReportedAgentVersion,
    iReportedOs,
    iRootDeviceType,
    iRootDeviceVolumeId,
    iSecurityGroupIds,
    iSshHostDsaKeyFingerprint,
    iSshHostRsaKeyFingerprint,
    iSshKeyName,
    iStackId,
    iStatus,
    iSubnetId,
    iTenancy,
    iVirtualizationType,

    -- * Deployment
    Deployment (..),
    mkDeployment,
    dAppId,
    dCommand,
    dComment,
    dCompletedAt,
    dCreatedAt,
    dCustomJson,
    dDeploymentId,
    dDuration,
    dIamUserArn,
    dInstanceIds,
    dStackId,
    dStatus,

    -- * InstancesCount
    InstancesCount (..),
    mkInstancesCount,
    icAssigning,
    icBooting,
    icConnectionLost,
    icDeregistering,
    icOnline,
    icPending,
    icRebooting,
    icRegistered,
    icRegistering,
    icRequested,
    icRunningSetup,
    icSetupFailed,
    icShuttingDown,
    icStartFailed,
    icStopFailed,
    icStopped,
    icStopping,
    icTerminated,
    icTerminating,
    icUnassigning,

    -- * AppType
    AppType (..),

    -- * StackId
    StackId (..),

    -- * DeploymentId
    DeploymentId (..),

    -- * InstanceId
    InstanceId (..),

    -- * AppId
    AppId (..),

    -- * Comment
    Comment (..),

    -- * CustomJson
    CustomJson (..),

    -- * Certificate
    Certificate (..),

    -- * PrivateKey
    PrivateKey (..),

    -- * Chain
    Chain (..),

    -- * InstanceType
    InstanceType (..),

    -- * AmiId
    AmiId (..),

    -- * AvailabilityZone
    AvailabilityZone (..),

    -- * Hostname
    Hostname (..),

    -- * Os
    Os (..),

    -- * SshKeyName
    SshKeyName (..),

    -- * SubnetId
    SubnetId (..),

    -- * Tenancy
    Tenancy (..),

    -- * AcknowledgedAt
    AcknowledgedAt (..),

    -- * CommandId
    CommandId (..),

    -- * CompletedAt
    CompletedAt (..),

    -- * CreatedAt
    CreatedAt (..),

    -- * LogUrl
    LogUrl (..),

    -- * Status
    Status (..),

    -- * Type
    Type (..),

    -- * RaidArrayId
    RaidArrayId (..),

    -- * Device
    Device (..),

    -- * MountPoint
    MountPoint (..),

    -- * Name
    Name (..),

    -- * IamUserArn
    IamUserArn (..),

    -- * DnsName
    DnsName (..),

    -- * ElasticLoadBalancerName
    ElasticLoadBalancerName (..),

    -- * LayerId
    LayerId (..),

    -- * Region
    Region (..),

    -- * VpcId
    VpcId (..),

    -- * EcsClusterArn
    EcsClusterArn (..),

    -- * RdsDbInstanceArn
    RdsDbInstanceArn (..),

    -- * DbPassword
    DbPassword (..),

    -- * DbUser
    DbUser (..),

    -- * Address
    Address (..),

    -- * DbInstanceIdentifier
    DbInstanceIdentifier (..),

    -- * Engine
    Engine (..),

    -- * Arn
    Arn (..),

    -- * DeviceName
    DeviceName (..),

    -- * NoDevice
    NoDevice (..),

    -- * VirtualName
    VirtualName (..),

    -- * RegisteredAt
    RegisteredAt (..),
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.AcknowledgedAt
import Network.AWS.OpsWorks.Types.Address
import Network.AWS.OpsWorks.Types.AgentVersion
import Network.AWS.OpsWorks.Types.AmiId
import Network.AWS.OpsWorks.Types.App
import Network.AWS.OpsWorks.Types.AppAttributesKeys
import Network.AWS.OpsWorks.Types.AppId
import Network.AWS.OpsWorks.Types.AppType
import Network.AWS.OpsWorks.Types.Architecture
import Network.AWS.OpsWorks.Types.Arn
import Network.AWS.OpsWorks.Types.AutoScalingThresholds
import Network.AWS.OpsWorks.Types.AutoScalingType
import Network.AWS.OpsWorks.Types.AvailabilityZone
import Network.AWS.OpsWorks.Types.BlockDeviceMapping
import Network.AWS.OpsWorks.Types.Certificate
import Network.AWS.OpsWorks.Types.Chain
import Network.AWS.OpsWorks.Types.ChefConfiguration
import Network.AWS.OpsWorks.Types.CloudWatchLogsConfiguration
import Network.AWS.OpsWorks.Types.CloudWatchLogsEncoding
import Network.AWS.OpsWorks.Types.CloudWatchLogsInitialPosition
import Network.AWS.OpsWorks.Types.CloudWatchLogsLogStream
import Network.AWS.OpsWorks.Types.CloudWatchLogsTimeZone
import Network.AWS.OpsWorks.Types.Command
import Network.AWS.OpsWorks.Types.CommandId
import Network.AWS.OpsWorks.Types.Comment
import Network.AWS.OpsWorks.Types.CompletedAt
import Network.AWS.OpsWorks.Types.CreatedAt
import Network.AWS.OpsWorks.Types.CustomJson
import Network.AWS.OpsWorks.Types.DataSource
import Network.AWS.OpsWorks.Types.DateTime
import Network.AWS.OpsWorks.Types.DbInstanceIdentifier
import Network.AWS.OpsWorks.Types.DbPassword
import Network.AWS.OpsWorks.Types.DbUser
import Network.AWS.OpsWorks.Types.Deployment
import Network.AWS.OpsWorks.Types.DeploymentCommand
import Network.AWS.OpsWorks.Types.DeploymentCommandName
import Network.AWS.OpsWorks.Types.DeploymentId
import Network.AWS.OpsWorks.Types.Device
import Network.AWS.OpsWorks.Types.DeviceName
import Network.AWS.OpsWorks.Types.DnsName
import Network.AWS.OpsWorks.Types.EbsBlockDevice
import Network.AWS.OpsWorks.Types.EcsCluster
import Network.AWS.OpsWorks.Types.EcsClusterArn
import Network.AWS.OpsWorks.Types.ElasticIp
import Network.AWS.OpsWorks.Types.ElasticLoadBalancer
import Network.AWS.OpsWorks.Types.ElasticLoadBalancerName
import Network.AWS.OpsWorks.Types.Engine
import Network.AWS.OpsWorks.Types.EnvironmentVariable
import Network.AWS.OpsWorks.Types.Hostname
import Network.AWS.OpsWorks.Types.Hour
import Network.AWS.OpsWorks.Types.IamUserArn
import Network.AWS.OpsWorks.Types.Instance
import Network.AWS.OpsWorks.Types.InstanceId
import Network.AWS.OpsWorks.Types.InstanceIdentity
import Network.AWS.OpsWorks.Types.InstanceType
import Network.AWS.OpsWorks.Types.InstancesCount
import Network.AWS.OpsWorks.Types.Layer
import Network.AWS.OpsWorks.Types.LayerAttributesKeys
import Network.AWS.OpsWorks.Types.LayerId
import Network.AWS.OpsWorks.Types.LayerType
import Network.AWS.OpsWorks.Types.LifecycleEventConfiguration
import Network.AWS.OpsWorks.Types.LoadBasedAutoScalingConfiguration
import Network.AWS.OpsWorks.Types.LogUrl
import Network.AWS.OpsWorks.Types.MountPoint
import Network.AWS.OpsWorks.Types.Name
import Network.AWS.OpsWorks.Types.NextToken
import Network.AWS.OpsWorks.Types.NoDevice
import Network.AWS.OpsWorks.Types.OperatingSystem
import Network.AWS.OpsWorks.Types.OperatingSystemConfigurationManager
import Network.AWS.OpsWorks.Types.Os
import Network.AWS.OpsWorks.Types.Permission
import Network.AWS.OpsWorks.Types.PrivateKey
import Network.AWS.OpsWorks.Types.RaidArray
import Network.AWS.OpsWorks.Types.RaidArrayId
import Network.AWS.OpsWorks.Types.RdsDbInstance
import Network.AWS.OpsWorks.Types.RdsDbInstanceArn
import Network.AWS.OpsWorks.Types.Recipes
import Network.AWS.OpsWorks.Types.Region
import Network.AWS.OpsWorks.Types.RegisteredAt
import Network.AWS.OpsWorks.Types.ReportedOs
import Network.AWS.OpsWorks.Types.ResourceArn
import Network.AWS.OpsWorks.Types.RootDeviceType
import Network.AWS.OpsWorks.Types.SelfUserProfile
import Network.AWS.OpsWorks.Types.ServiceError'
import Network.AWS.OpsWorks.Types.ShutdownEventConfiguration
import Network.AWS.OpsWorks.Types.Source
import Network.AWS.OpsWorks.Types.SourceType
import Network.AWS.OpsWorks.Types.SshKeyName
import Network.AWS.OpsWorks.Types.SslConfiguration
import Network.AWS.OpsWorks.Types.Stack
import Network.AWS.OpsWorks.Types.StackAttributesKeys
import Network.AWS.OpsWorks.Types.StackConfigurationManager
import Network.AWS.OpsWorks.Types.StackId
import Network.AWS.OpsWorks.Types.StackSummary
import Network.AWS.OpsWorks.Types.Status
import Network.AWS.OpsWorks.Types.String
import Network.AWS.OpsWorks.Types.SubnetId
import Network.AWS.OpsWorks.Types.Switch
import Network.AWS.OpsWorks.Types.TagKey
import Network.AWS.OpsWorks.Types.TagValue
import Network.AWS.OpsWorks.Types.TemporaryCredential
import Network.AWS.OpsWorks.Types.Tenancy
import Network.AWS.OpsWorks.Types.TimeBasedAutoScalingConfiguration
import Network.AWS.OpsWorks.Types.Type
import Network.AWS.OpsWorks.Types.UserProfile
import Network.AWS.OpsWorks.Types.VirtualName
import Network.AWS.OpsWorks.Types.VirtualizationType
import Network.AWS.OpsWorks.Types.Volume
import Network.AWS.OpsWorks.Types.VolumeConfiguration
import Network.AWS.OpsWorks.Types.VolumeType
import Network.AWS.OpsWorks.Types.VpcId
import Network.AWS.OpsWorks.Types.WeeklyAutoScalingSchedule
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2013-02-18@ of the Amazon OpsWorks SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "OpsWorks",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "opsworks",
      Core._svcVersion = "2013-02-18",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "OpsWorks",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | Indicates that a request was not valid.
_ValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError mkServiceConfig "ValidationException"
{-# DEPRECATED _ValidationException "Use generic-lens or generic-optics instead." #-}

-- | Indicates that a resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceNotFoundException"
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead." #-}
