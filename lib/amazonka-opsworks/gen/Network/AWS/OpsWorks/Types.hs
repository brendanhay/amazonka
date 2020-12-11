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
    opsWorksService,

    -- * Errors

    -- * AppAttributesKeys
    AppAttributesKeys (..),

    -- * AppType
    AppType (..),

    -- * Architecture
    Architecture (..),

    -- * AutoScalingType
    AutoScalingType (..),

    -- * CloudWatchLogsEncoding
    CloudWatchLogsEncoding (..),

    -- * CloudWatchLogsInitialPosition
    CloudWatchLogsInitialPosition (..),

    -- * CloudWatchLogsTimeZone
    CloudWatchLogsTimeZone (..),

    -- * DeploymentCommandName
    DeploymentCommandName (..),

    -- * LayerAttributesKeys
    LayerAttributesKeys (..),

    -- * LayerType
    LayerType (..),

    -- * RootDeviceType
    RootDeviceType (..),

    -- * SourceType
    SourceType (..),

    -- * StackAttributesKeys
    StackAttributesKeys (..),

    -- * VirtualizationType
    VirtualizationType (..),

    -- * VolumeType
    VolumeType (..),

    -- * AgentVersion
    AgentVersion (..),
    mkAgentVersion,
    avVersion,
    avConfigurationManager,

    -- * App
    App (..),
    mkApp,
    aSSLConfiguration,
    aEnvironment,
    aEnableSSL,
    aCreatedAt,
    aShortname,
    aDataSources,
    aAppSource,
    aAppId,
    aAttributes,
    aName,
    aType,
    aStackId,
    aDomains,
    aDescription,

    -- * AutoScalingThresholds
    AutoScalingThresholds (..),
    mkAutoScalingThresholds,
    astInstanceCount,
    astIgnoreMetricsTime,
    astLoadThreshold,
    astThresholdsWaitTime,
    astAlarms,
    astMemoryThreshold,
    astCPUThreshold,

    -- * BlockDeviceMapping
    BlockDeviceMapping (..),
    mkBlockDeviceMapping,
    bdmVirtualName,
    bdmNoDevice,
    bdmEBS,
    bdmDeviceName,

    -- * ChefConfiguration
    ChefConfiguration (..),
    mkChefConfiguration,
    ccBerkshelfVersion,
    ccManageBerkshelf,

    -- * CloudWatchLogsConfiguration
    CloudWatchLogsConfiguration (..),
    mkCloudWatchLogsConfiguration,
    cwlcEnabled,
    cwlcLogStreams,

    -- * CloudWatchLogsLogStream
    CloudWatchLogsLogStream (..),
    mkCloudWatchLogsLogStream,
    cwllsBatchCount,
    cwllsFileFingerprintLines,
    cwllsBufferDuration,
    cwllsBatchSize,
    cwllsLogGroupName,
    cwllsMultiLineStartPattern,
    cwllsInitialPosition,
    cwllsDatetimeFormat,
    cwllsEncoding,
    cwllsTimeZone,
    cwllsFile,

    -- * Command
    Command (..),
    mkCommand,
    cDeploymentId,
    cInstanceId,
    cStatus,
    cLogURL,
    cCreatedAt,
    cCommandId,
    cExitCode,
    cType,
    cCompletedAt,
    cAcknowledgedAt,

    -- * DataSource
    DataSource (..),
    mkDataSource,
    dsARN,
    dsDatabaseName,
    dsType,

    -- * Deployment
    Deployment (..),
    mkDeployment,
    dDeploymentId,
    dStatus,
    dCommand,
    dCreatedAt,
    dCustomJSON,
    dIAMUserARN,
    dAppId,
    dInstanceIds,
    dCompletedAt,
    dStackId,
    dComment,
    dDuration,

    -- * DeploymentCommand
    DeploymentCommand (..),
    mkDeploymentCommand,
    dcArgs,
    dcName,

    -- * EBSBlockDevice
    EBSBlockDevice (..),
    mkEBSBlockDevice,
    ebdDeleteOnTermination,
    ebdVolumeSize,
    ebdIOPS,
    ebdVolumeType,
    ebdSnapshotId,

    -- * EcsCluster
    EcsCluster (..),
    mkEcsCluster,
    ecEcsClusterARN,
    ecEcsClusterName,
    ecRegisteredAt,
    ecStackId,

    -- * ElasticIP
    ElasticIP (..),
    mkElasticIP,
    eiInstanceId,
    eiDomain,
    eiIP,
    eiName,
    eiRegion,

    -- * ElasticLoadBalancer
    ElasticLoadBalancer (..),
    mkElasticLoadBalancer,
    elbSubnetIds,
    elbVPCId,
    elbAvailabilityZones,
    elbRegion,
    elbElasticLoadBalancerName,
    elbStackId,
    elbEC2InstanceIds,
    elbLayerId,
    elbDNSName,

    -- * EnvironmentVariable
    EnvironmentVariable (..),
    mkEnvironmentVariable,
    evSecure,
    evKey,
    evValue,

    -- * Instance
    Instance (..),
    mkInstance,
    iPrivateDNS,
    iReportedAgentVersion,
    iInstanceId,
    iStatus,
    iPrivateIP,
    iInstallUpdatesOnBoot,
    iVirtualizationType,
    iInstanceProfileARN,
    iPlatform,
    iHostname,
    iSSHHostRsaKeyFingerprint,
    iSecurityGroupIds,
    iEcsClusterARN,
    iARN,
    iCreatedAt,
    iEC2InstanceId,
    iSSHKeyName,
    iAgentVersion,
    iRootDeviceVolumeId,
    iSubnetId,
    iInfrastructureClass,
    iSSHHostDsaKeyFingerprint,
    iInstanceType,
    iEBSOptimized,
    iElasticIP,
    iOS,
    iAvailabilityZone,
    iLastServiceErrorId,
    iTenancy,
    iAutoScalingType,
    iLayerIds,
    iArchitecture,
    iPublicDNS,
    iAMIId,
    iPublicIP,
    iReportedOS,
    iRegisteredBy,
    iStackId,
    iRootDeviceType,
    iEcsContainerInstanceARN,
    iBlockDeviceMappings,

    -- * InstanceIdentity
    InstanceIdentity (..),
    mkInstanceIdentity,
    iiSignature,
    iiDocument,

    -- * InstancesCount
    InstancesCount (..),
    mkInstancesCount,
    icTerminating,
    icPending,
    icOnline,
    icUnassigning,
    icDeregistering,
    icRunningSetup,
    icRequested,
    icStopFailed,
    icBooting,
    icStopped,
    icRebooting,
    icAssigning,
    icShuttingDown,
    icSetupFailed,
    icConnectionLost,
    icTerminated,
    icStopping,
    icRegistered,
    icStartFailed,
    icRegistering,

    -- * Layer
    Layer (..),
    mkLayer,
    lCustomInstanceProfileARN,
    lCustomSecurityGroupIds,
    lInstallUpdatesOnBoot,
    lCloudWatchLogsConfiguration,
    lLifecycleEventConfiguration,
    lARN,
    lCreatedAt,
    lShortname,
    lDefaultRecipes,
    lCustomRecipes,
    lCustomJSON,
    lVolumeConfigurations,
    lEnableAutoHealing,
    lPackages,
    lAttributes,
    lName,
    lAutoAssignPublicIPs,
    lType,
    lUseEBSOptimizedInstances,
    lStackId,
    lLayerId,
    lDefaultSecurityGroupNames,
    lAutoAssignElasticIPs,

    -- * LifecycleEventConfiguration
    LifecycleEventConfiguration (..),
    mkLifecycleEventConfiguration,
    lecShutdown,

    -- * LoadBasedAutoScalingConfiguration
    LoadBasedAutoScalingConfiguration (..),
    mkLoadBasedAutoScalingConfiguration,
    lbascUpScaling,
    lbascEnable,
    lbascDownScaling,
    lbascLayerId,

    -- * OperatingSystem
    OperatingSystem (..),
    mkOperatingSystem,
    osReportedVersion,
    osSupported,
    osName,
    osId,
    osConfigurationManagers,
    osType,
    osReportedName,

    -- * OperatingSystemConfigurationManager
    OperatingSystemConfigurationManager (..),
    mkOperatingSystemConfigurationManager,
    oscmName,
    oscmVersion,

    -- * Permission
    Permission (..),
    mkPermission,
    pIAMUserARN,
    pAllowSudo,
    pStackId,
    pLevel,
    pAllowSSH,

    -- * RAIDArray
    RAIDArray (..),
    mkRAIDArray,
    raiaInstanceId,
    raiaSize,
    raiaIOPS,
    raiaCreatedAt,
    raiaRAIDLevel,
    raiaDevice,
    raiaNumberOfDisks,
    raiaAvailabilityZone,
    raiaName,
    raiaRAIDArrayId,
    raiaVolumeType,
    raiaStackId,
    raiaMountPoint,

    -- * RDSDBInstance
    RDSDBInstance (..),
    mkRDSDBInstance,
    rdiRDSDBInstanceARN,
    rdiDBUser,
    rdiMissingOnRDS,
    rdiEngine,
    rdiAddress,
    rdiDBInstanceIdentifier,
    rdiRegion,
    rdiStackId,
    rdiDBPassword,

    -- * Recipes
    Recipes (..),
    mkRecipes,
    rSetup,
    rShutdown,
    rUndeploy,
    rConfigure,
    rDeploy,

    -- * ReportedOS
    ReportedOS (..),
    mkReportedOS,
    roFamily,
    roName,
    roVersion,

    -- * SSLConfiguration
    SSLConfiguration (..),
    mkSSLConfiguration,
    scPrivateKey,
    scCertificate,
    scChain,

    -- * SelfUserProfile
    SelfUserProfile (..),
    mkSelfUserProfile,
    supSSHPublicKey,
    supSSHUsername,
    supIAMUserARN,
    supName,

    -- * ServiceError'
    ServiceError' (..),
    mkServiceError',
    seInstanceId,
    seCreatedAt,
    seServiceErrorId,
    seType,
    seStackId,
    seMessage,

    -- * ShutdownEventConfiguration
    ShutdownEventConfiguration (..),
    mkShutdownEventConfiguration,
    secExecutionTimeout,
    secDelayUntilElbConnectionsDrained,

    -- * Source
    Source (..),
    mkSource,
    sURL,
    sUsername,
    sSSHKey,
    sPassword,
    sType,
    sRevision,

    -- * Stack
    Stack (..),
    mkStack,
    sDefaultInstanceProfileARN,
    sServiceRoleARN,
    sDefaultRootDeviceType,
    sARN,
    sCreatedAt,
    sVPCId,
    sChefConfiguration,
    sAgentVersion,
    sDefaultSSHKeyName,
    sCustomJSON,
    sCustomCookbooksSource,
    sDefaultAvailabilityZone,
    sAttributes,
    sName,
    sDefaultOS,
    sUseOpsworksSecurityGroups,
    sUseCustomCookbooks,
    sDefaultSubnetId,
    sRegion,
    sConfigurationManager,
    sStackId,
    sHostnameTheme,

    -- * StackConfigurationManager
    StackConfigurationManager (..),
    mkStackConfigurationManager,
    scmName,
    scmVersion,

    -- * StackSummary
    StackSummary (..),
    mkStackSummary,
    ssARN,
    ssAppsCount,
    ssName,
    ssStackId,
    ssLayersCount,
    ssInstancesCount,

    -- * TemporaryCredential
    TemporaryCredential (..),
    mkTemporaryCredential,
    tcInstanceId,
    tcUsername,
    tcPassword,
    tcValidForInMinutes,

    -- * TimeBasedAutoScalingConfiguration
    TimeBasedAutoScalingConfiguration (..),
    mkTimeBasedAutoScalingConfiguration,
    tbascInstanceId,
    tbascAutoScalingSchedule,

    -- * UserProfile
    UserProfile (..),
    mkUserProfile,
    upAllowSelfManagement,
    upSSHPublicKey,
    upSSHUsername,
    upIAMUserARN,
    upName,

    -- * Volume
    Volume (..),
    mkVolume,
    vInstanceId,
    vStatus,
    vSize,
    vIOPS,
    vDevice,
    vEncrypted,
    vAvailabilityZone,
    vName,
    vRAIDArrayId,
    vVolumeId,
    vRegion,
    vVolumeType,
    vEC2VolumeId,
    vMountPoint,

    -- * VolumeConfiguration
    VolumeConfiguration (..),
    mkVolumeConfiguration,
    vcIOPS,
    vcRAIDLevel,
    vcEncrypted,
    vcVolumeType,
    vcMountPoint,
    vcNumberOfDisks,
    vcSize,

    -- * WeeklyAutoScalingSchedule
    WeeklyAutoScalingSchedule (..),
    mkWeeklyAutoScalingSchedule,
    wassThursday,
    wassWednesday,
    wassSaturday,
    wassMonday,
    wassFriday,
    wassSunday,
    wassTuesday,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.AgentVersion
import Network.AWS.OpsWorks.Types.App
import Network.AWS.OpsWorks.Types.AppAttributesKeys
import Network.AWS.OpsWorks.Types.AppType
import Network.AWS.OpsWorks.Types.Architecture
import Network.AWS.OpsWorks.Types.AutoScalingThresholds
import Network.AWS.OpsWorks.Types.AutoScalingType
import Network.AWS.OpsWorks.Types.BlockDeviceMapping
import Network.AWS.OpsWorks.Types.ChefConfiguration
import Network.AWS.OpsWorks.Types.CloudWatchLogsConfiguration
import Network.AWS.OpsWorks.Types.CloudWatchLogsEncoding
import Network.AWS.OpsWorks.Types.CloudWatchLogsInitialPosition
import Network.AWS.OpsWorks.Types.CloudWatchLogsLogStream
import Network.AWS.OpsWorks.Types.CloudWatchLogsTimeZone
import Network.AWS.OpsWorks.Types.Command
import Network.AWS.OpsWorks.Types.DataSource
import Network.AWS.OpsWorks.Types.Deployment
import Network.AWS.OpsWorks.Types.DeploymentCommand
import Network.AWS.OpsWorks.Types.DeploymentCommandName
import Network.AWS.OpsWorks.Types.EBSBlockDevice
import Network.AWS.OpsWorks.Types.EcsCluster
import Network.AWS.OpsWorks.Types.ElasticIP
import Network.AWS.OpsWorks.Types.ElasticLoadBalancer
import Network.AWS.OpsWorks.Types.EnvironmentVariable
import Network.AWS.OpsWorks.Types.Instance
import Network.AWS.OpsWorks.Types.InstanceIdentity
import Network.AWS.OpsWorks.Types.InstancesCount
import Network.AWS.OpsWorks.Types.Layer
import Network.AWS.OpsWorks.Types.LayerAttributesKeys
import Network.AWS.OpsWorks.Types.LayerType
import Network.AWS.OpsWorks.Types.LifecycleEventConfiguration
import Network.AWS.OpsWorks.Types.LoadBasedAutoScalingConfiguration
import Network.AWS.OpsWorks.Types.OperatingSystem
import Network.AWS.OpsWorks.Types.OperatingSystemConfigurationManager
import Network.AWS.OpsWorks.Types.Permission
import Network.AWS.OpsWorks.Types.RAIDArray
import Network.AWS.OpsWorks.Types.RDSDBInstance
import Network.AWS.OpsWorks.Types.Recipes
import Network.AWS.OpsWorks.Types.ReportedOS
import Network.AWS.OpsWorks.Types.RootDeviceType
import Network.AWS.OpsWorks.Types.SSLConfiguration
import Network.AWS.OpsWorks.Types.SelfUserProfile
import Network.AWS.OpsWorks.Types.ServiceError'
import Network.AWS.OpsWorks.Types.ShutdownEventConfiguration
import Network.AWS.OpsWorks.Types.Source
import Network.AWS.OpsWorks.Types.SourceType
import Network.AWS.OpsWorks.Types.Stack
import Network.AWS.OpsWorks.Types.StackAttributesKeys
import Network.AWS.OpsWorks.Types.StackConfigurationManager
import Network.AWS.OpsWorks.Types.StackSummary
import Network.AWS.OpsWorks.Types.TemporaryCredential
import Network.AWS.OpsWorks.Types.TimeBasedAutoScalingConfiguration
import Network.AWS.OpsWorks.Types.UserProfile
import Network.AWS.OpsWorks.Types.VirtualizationType
import Network.AWS.OpsWorks.Types.Volume
import Network.AWS.OpsWorks.Types.VolumeConfiguration
import Network.AWS.OpsWorks.Types.VolumeType
import Network.AWS.OpsWorks.Types.WeeklyAutoScalingSchedule
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2013-02-18@ of the Amazon OpsWorks SDK configuration.
opsWorksService :: Lude.Service
opsWorksService =
  Lude.Service
    { Lude._svcAbbrev = "OpsWorks",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "opsworks",
      Lude._svcVersion = "2013-02-18",
      Lude._svcEndpoint = Lude.defaultEndpoint opsWorksService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "OpsWorks",
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
