{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types
  ( -- * Service Configuration
    opsWorks,

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
    AgentVersion,
    agentVersion,
    avVersion,
    avConfigurationManager,

    -- * App
    App,
    app,
    appSSLConfiguration,
    appEnvironment,
    appEnableSSL,
    appCreatedAt,
    appShortname,
    appDataSources,
    appAppSource,
    appAppId,
    appAttributes,
    appName,
    appType,
    appStackId,
    appDomains,
    appDescription,

    -- * AutoScalingThresholds
    AutoScalingThresholds,
    autoScalingThresholds,
    astInstanceCount,
    astIgnoreMetricsTime,
    astLoadThreshold,
    astThresholdsWaitTime,
    astAlarms,
    astMemoryThreshold,
    astCPUThreshold,

    -- * BlockDeviceMapping
    BlockDeviceMapping,
    blockDeviceMapping,
    bdmVirtualName,
    bdmNoDevice,
    bdmEBS,
    bdmDeviceName,

    -- * ChefConfiguration
    ChefConfiguration,
    chefConfiguration,
    ccBerkshelfVersion,
    ccManageBerkshelf,

    -- * CloudWatchLogsConfiguration
    CloudWatchLogsConfiguration,
    cloudWatchLogsConfiguration,
    cwlcEnabled,
    cwlcLogStreams,

    -- * CloudWatchLogsLogStream
    CloudWatchLogsLogStream,
    cloudWatchLogsLogStream,
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
    Command,
    command,
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
    DataSource,
    dataSource,
    dsARN,
    dsDatabaseName,
    dsType,

    -- * Deployment
    Deployment,
    deployment,
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
    DeploymentCommand,
    deploymentCommand,
    dcArgs,
    dcName,

    -- * EBSBlockDevice
    EBSBlockDevice,
    ebsBlockDevice,
    ebdDeleteOnTermination,
    ebdVolumeSize,
    ebdIOPS,
    ebdVolumeType,
    ebdSnapshotId,

    -- * EcsCluster
    EcsCluster,
    ecsCluster,
    ecEcsClusterARN,
    ecEcsClusterName,
    ecRegisteredAt,
    ecStackId,

    -- * ElasticIP
    ElasticIP,
    elasticIP,
    eiInstanceId,
    eiDomain,
    eiIP,
    eiName,
    eiRegion,

    -- * ElasticLoadBalancer
    ElasticLoadBalancer,
    elasticLoadBalancer,
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
    EnvironmentVariable,
    environmentVariable,
    evSecure,
    evKey,
    evValue,

    -- * Instance
    Instance,
    instance',
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
    InstanceIdentity,
    instanceIdentity,
    iiSignature,
    iiDocument,

    -- * InstancesCount
    InstancesCount,
    instancesCount,
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
    Layer,
    layer,
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
    LifecycleEventConfiguration,
    lifecycleEventConfiguration,
    lecShutdown,

    -- * LoadBasedAutoScalingConfiguration
    LoadBasedAutoScalingConfiguration,
    loadBasedAutoScalingConfiguration,
    lbascUpScaling,
    lbascEnable,
    lbascDownScaling,
    lbascLayerId,

    -- * OperatingSystem
    OperatingSystem,
    operatingSystem,
    osReportedVersion,
    osSupported,
    osName,
    osId,
    osConfigurationManagers,
    osType,
    osReportedName,

    -- * OperatingSystemConfigurationManager
    OperatingSystemConfigurationManager,
    operatingSystemConfigurationManager,
    oscmName,
    oscmVersion,

    -- * Permission
    Permission,
    permission,
    pIAMUserARN,
    pAllowSudo,
    pStackId,
    pLevel,
    pAllowSSH,

    -- * RAIdArray
    RAIdArray,
    rAIdArray,
    raiaInstanceId,
    raiaSize,
    raiaIOPS,
    raiaCreatedAt,
    raiaRAIdLevel,
    raiaDevice,
    raiaNumberOfDisks,
    raiaAvailabilityZone,
    raiaName,
    raiaRAIdArrayId,
    raiaVolumeType,
    raiaStackId,
    raiaMountPoint,

    -- * RDSDBInstance
    RDSDBInstance,
    rdsDBInstance,
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
    Recipes,
    recipes,
    rSetup,
    rShutdown,
    rUndeploy,
    rConfigure,
    rDeploy,

    -- * ReportedOS
    ReportedOS,
    reportedOS,
    roFamily,
    roName,
    roVersion,

    -- * SSLConfiguration
    SSLConfiguration,
    sslConfiguration,
    scPrivateKey,
    scCertificate,
    scChain,

    -- * SelfUserProfile
    SelfUserProfile,
    selfUserProfile,
    supSSHPublicKey,
    supSSHUsername,
    supIAMUserARN,
    supName,

    -- * ServiceError'
    ServiceError',
    serviceError',
    seInstanceId,
    seCreatedAt,
    seServiceErrorId,
    seType,
    seStackId,
    seMessage,

    -- * ShutdownEventConfiguration
    ShutdownEventConfiguration,
    shutdownEventConfiguration,
    secExecutionTimeout,
    secDelayUntilElbConnectionsDrained,

    -- * Source
    Source,
    source,
    sURL,
    sUsername,
    sSSHKey,
    sPassword,
    sType,
    sRevision,

    -- * Stack
    Stack,
    stack,
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
    StackConfigurationManager,
    stackConfigurationManager,
    scmName,
    scmVersion,

    -- * StackSummary
    StackSummary,
    stackSummary,
    ssARN,
    ssAppsCount,
    ssName,
    ssStackId,
    ssLayersCount,
    ssInstancesCount,

    -- * TemporaryCredential
    TemporaryCredential,
    temporaryCredential,
    tcInstanceId,
    tcUsername,
    tcPassword,
    tcValidForInMinutes,

    -- * TimeBasedAutoScalingConfiguration
    TimeBasedAutoScalingConfiguration,
    timeBasedAutoScalingConfiguration,
    tbascInstanceId,
    tbascAutoScalingSchedule,

    -- * UserProfile
    UserProfile,
    userProfile,
    upAllowSelfManagement,
    upSSHPublicKey,
    upSSHUsername,
    upIAMUserARN,
    upName,

    -- * Volume
    Volume,
    volume,
    vInstanceId,
    vStatus,
    vSize,
    vIOPS,
    vDevice,
    vEncrypted,
    vAvailabilityZone,
    vName,
    vRAIdArrayId,
    vVolumeId,
    vRegion,
    vVolumeType,
    vEC2VolumeId,
    vMountPoint,

    -- * VolumeConfiguration
    VolumeConfiguration,
    volumeConfiguration,
    vcIOPS,
    vcRAIdLevel,
    vcEncrypted,
    vcVolumeType,
    vcMountPoint,
    vcNumberOfDisks,
    vcSize,

    -- * WeeklyAutoScalingSchedule
    WeeklyAutoScalingSchedule,
    weeklyAutoScalingSchedule,
    wassThursday,
    wassWednesday,
    wassSaturday,
    wassMonday,
    wassFriday,
    wassSunday,
    wassTuesday,
  )
where

import Network.AWS.Lens
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
import Network.AWS.OpsWorks.Types.RAIdArray
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
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2013-02-18@ of the Amazon OpsWorks SDK configuration.
opsWorks :: Service
opsWorks =
  Service
    { _svcAbbrev = "OpsWorks",
      _svcSigner = v4,
      _svcPrefix = "opsworks",
      _svcVersion = "2013-02-18",
      _svcEndpoint = defaultEndpoint opsWorks,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "OpsWorks",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
