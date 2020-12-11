-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types
  ( -- * Service configuration
    elasticBeanstalkService,

    -- * Errors

    -- * ActionHistoryStatus
    ActionHistoryStatus (..),

    -- * ActionStatus
    ActionStatus (..),

    -- * ActionType
    ActionType (..),

    -- * ApplicationVersionStatus
    ApplicationVersionStatus (..),

    -- * ComputeType
    ComputeType (..),

    -- * ConfigurationDeploymentStatus
    ConfigurationDeploymentStatus (..),

    -- * ConfigurationOptionValueType
    ConfigurationOptionValueType (..),

    -- * EnvironmentHealth
    EnvironmentHealth (..),

    -- * EnvironmentHealthAttribute
    EnvironmentHealthAttribute (..),

    -- * EnvironmentHealthStatus
    EnvironmentHealthStatus (..),

    -- * EnvironmentInfoType
    EnvironmentInfoType (..),

    -- * EnvironmentStatus
    EnvironmentStatus (..),

    -- * EventSeverity
    EventSeverity (..),

    -- * FailureType
    FailureType (..),

    -- * InstancesHealthAttribute
    InstancesHealthAttribute (..),

    -- * PlatformStatus
    PlatformStatus (..),

    -- * SourceRepository
    SourceRepository (..),

    -- * SourceType
    SourceType (..),

    -- * ValidationSeverity
    ValidationSeverity (..),

    -- * ApplicationDescription
    ApplicationDescription (..),
    mkApplicationDescription,
    adApplicationARN,
    adVersions,
    adDateUpdated,
    adDateCreated,
    adApplicationName,
    adConfigurationTemplates,
    adResourceLifecycleConfig,
    adDescription,

    -- * ApplicationDescriptionMessage
    ApplicationDescriptionMessage (..),
    mkApplicationDescriptionMessage,
    admApplication,

    -- * ApplicationMetrics
    ApplicationMetrics (..),
    mkApplicationMetrics,
    amRequestCount,
    amLatency,
    amStatusCodes,
    amDuration,

    -- * ApplicationResourceLifecycleConfig
    ApplicationResourceLifecycleConfig (..),
    mkApplicationResourceLifecycleConfig,
    arlcVersionLifecycleConfig,
    arlcServiceRole,

    -- * ApplicationVersionDescription
    ApplicationVersionDescription (..),
    mkApplicationVersionDescription,
    avdStatus,
    avdSourceBundle,
    avdDateUpdated,
    avdDateCreated,
    avdVersionLabel,
    avdSourceBuildInformation,
    avdApplicationName,
    avdApplicationVersionARN,
    avdBuildARN,
    avdDescription,

    -- * ApplicationVersionDescriptionMessage
    ApplicationVersionDescriptionMessage (..),
    mkApplicationVersionDescriptionMessage,
    avdmApplicationVersion,

    -- * ApplicationVersionLifecycleConfig
    ApplicationVersionLifecycleConfig (..),
    mkApplicationVersionLifecycleConfig,
    avlcMaxAgeRule,
    avlcMaxCountRule,

    -- * AutoScalingGroup
    AutoScalingGroup (..),
    mkAutoScalingGroup,
    asgName,

    -- * BuildConfiguration
    BuildConfiguration (..),
    mkBuildConfiguration,
    bcArtifactName,
    bcComputeType,
    bcTimeoutInMinutes,
    bcCodeBuildServiceRole,
    bcImage,

    -- * Builder
    Builder (..),
    mkBuilder,
    bARN,

    -- * CPUUtilization
    CPUUtilization (..),
    mkCPUUtilization,
    cuSoftIRQ,
    cuIdle,
    cuIRQ,
    cuSystem,
    cuPrivileged,
    cuUser,
    cuIOWait,
    cuNice,

    -- * ConfigurationOptionDescription
    ConfigurationOptionDescription (..),
    mkConfigurationOptionDescription,
    codMaxValue,
    codRegex,
    codMaxLength,
    codUserDefined,
    codNamespace,
    codValueOptions,
    codName,
    codChangeSeverity,
    codDefaultValue,
    codValueType,
    codMinValue,

    -- * ConfigurationOptionSetting
    ConfigurationOptionSetting (..),
    mkConfigurationOptionSetting,
    cosOptionName,
    cosResourceName,
    cosNamespace,
    cosValue,

    -- * ConfigurationSettingsDescription
    ConfigurationSettingsDescription (..),
    mkConfigurationSettingsDescription,
    csdTemplateName,
    csdOptionSettings,
    csdDateUpdated,
    csdDateCreated,
    csdPlatformARN,
    csdEnvironmentName,
    csdApplicationName,
    csdDeploymentStatus,
    csdSolutionStackName,
    csdDescription,

    -- * CustomAMI
    CustomAMI (..),
    mkCustomAMI,
    caVirtualizationType,
    caImageId,

    -- * Deployment
    Deployment (..),
    mkDeployment,
    dDeploymentId,
    dStatus,
    dDeploymentTime,
    dVersionLabel,

    -- * EnvironmentDescription
    EnvironmentDescription (..),
    mkEnvironmentDescription,
    eStatus,
    eCNAME,
    eTemplateName,
    eAbortableOperationInProgress,
    eEndpointURL,
    eResources,
    eDateUpdated,
    eDateCreated,
    eHealth,
    eVersionLabel,
    eOperationsRole,
    ePlatformARN,
    eTier,
    eEnvironmentName,
    eApplicationName,
    eEnvironmentARN,
    eSolutionStackName,
    eEnvironmentId,
    eHealthStatus,
    eEnvironmentLinks,
    eDescription,

    -- * EnvironmentDescriptionsMessage
    EnvironmentDescriptionsMessage (..),
    mkEnvironmentDescriptionsMessage,
    edmNextToken,
    edmEnvironments,

    -- * EnvironmentInfoDescription
    EnvironmentInfoDescription (..),
    mkEnvironmentInfoDescription,
    eidSampleTimestamp,
    eidEC2InstanceId,
    eidInfoType,
    eidMessage,

    -- * EnvironmentLink
    EnvironmentLink (..),
    mkEnvironmentLink,
    elLinkName,
    elEnvironmentName,

    -- * EnvironmentResourceDescription
    EnvironmentResourceDescription (..),
    mkEnvironmentResourceDescription,
    erdQueues,
    erdTriggers,
    erdLaunchTemplates,
    erdLoadBalancers,
    erdEnvironmentName,
    erdInstances,
    erdLaunchConfigurations,
    erdAutoScalingGroups,

    -- * EnvironmentResourcesDescription
    EnvironmentResourcesDescription (..),
    mkEnvironmentResourcesDescription,
    erdLoadBalancer,

    -- * EnvironmentTier
    EnvironmentTier (..),
    mkEnvironmentTier,
    etName,
    etVersion,
    etType,

    -- * EventDescription
    EventDescription (..),
    mkEventDescription,
    edRequestId,
    edTemplateName,
    edSeverity,
    edVersionLabel,
    edPlatformARN,
    edEnvironmentName,
    edApplicationName,
    edEventDate,
    edMessage,

    -- * Instance
    Instance (..),
    mkInstance,
    iId,

    -- * InstanceHealthSummary
    InstanceHealthSummary (..),
    mkInstanceHealthSummary,
    ihsOK,
    ihsPending,
    ihsSevere,
    ihsUnknown,
    ihsNoData,
    ihsWarning,
    ihsDegraded,
    ihsInfo,

    -- * Latency
    Latency (..),
    mkLatency,
    lP75,
    lP50,
    lP85,
    lP999,
    lP90,
    lP95,
    lP99,
    lP10,

    -- * LaunchConfiguration
    LaunchConfiguration (..),
    mkLaunchConfiguration,
    lcName,

    -- * LaunchTemplate
    LaunchTemplate (..),
    mkLaunchTemplate,
    ltId,

    -- * Listener
    Listener (..),
    mkListener,
    lProtocol,
    lPort,

    -- * LoadBalancer
    LoadBalancer (..),
    mkLoadBalancer,
    lbName,

    -- * LoadBalancerDescription
    LoadBalancerDescription (..),
    mkLoadBalancerDescription,
    lbdLoadBalancerName,
    lbdDomain,
    lbdListeners,

    -- * ManagedAction
    ManagedAction (..),
    mkManagedAction,
    maStatus,
    maActionId,
    maWindowStartTime,
    maActionDescription,
    maActionType,

    -- * ManagedActionHistoryItem
    ManagedActionHistoryItem (..),
    mkManagedActionHistoryItem,
    mahiStatus,
    mahiFailureType,
    mahiActionId,
    mahiFailureDescription,
    mahiFinishedTime,
    mahiActionDescription,
    mahiExecutedTime,
    mahiActionType,

    -- * MaxAgeRule
    MaxAgeRule (..),
    mkMaxAgeRule,
    marDeleteSourceFromS3,
    marMaxAgeInDays,
    marEnabled,

    -- * MaxCountRule
    MaxCountRule (..),
    mkMaxCountRule,
    mcrMaxCount,
    mcrDeleteSourceFromS3,
    mcrEnabled,

    -- * OptionRestrictionRegex
    OptionRestrictionRegex (..),
    mkOptionRestrictionRegex,
    orrPattern,
    orrLabel,

    -- * OptionSpecification
    OptionSpecification (..),
    mkOptionSpecification,
    osOptionName,
    osResourceName,
    osNamespace,

    -- * PlatformBranchSummary
    PlatformBranchSummary (..),
    mkPlatformBranchSummary,
    pbsBranchName,
    pbsBranchOrder,
    pbsPlatformName,
    pbsSupportedTierList,
    pbsLifecycleState,

    -- * PlatformDescription
    PlatformDescription (..),
    mkPlatformDescription,
    pdPlatformBranchName,
    pdSupportedAddonList,
    pdPlatformCategory,
    pdPlatformBranchLifecycleState,
    pdPlatformVersion,
    pdPlatformStatus,
    pdMaintainer,
    pdPlatformLifecycleState,
    pdPlatformOwner,
    pdDateUpdated,
    pdCustomAMIList,
    pdDateCreated,
    pdOperatingSystemName,
    pdFrameworks,
    pdPlatformARN,
    pdOperatingSystemVersion,
    pdProgrammingLanguages,
    pdSolutionStackName,
    pdPlatformName,
    pdDescription,
    pdSupportedTierList,

    -- * PlatformFilter
    PlatformFilter (..),
    mkPlatformFilter,
    pfValues,
    pfOperator,
    pfType,

    -- * PlatformFramework
    PlatformFramework (..),
    mkPlatformFramework,
    pfName,
    pfVersion,

    -- * PlatformProgrammingLanguage
    PlatformProgrammingLanguage (..),
    mkPlatformProgrammingLanguage,
    pplName,
    pplVersion,

    -- * PlatformSummary
    PlatformSummary (..),
    mkPlatformSummary,
    psPlatformBranchName,
    psSupportedAddonList,
    psPlatformCategory,
    psPlatformBranchLifecycleState,
    psPlatformVersion,
    psPlatformStatus,
    psPlatformLifecycleState,
    psPlatformOwner,
    psOperatingSystemName,
    psPlatformARN,
    psOperatingSystemVersion,
    psSupportedTierList,

    -- * Queue
    Queue (..),
    mkQueue,
    qURL,
    qName,

    -- * ResourceQuota
    ResourceQuota (..),
    mkResourceQuota,
    rqMaximum,

    -- * ResourceQuotas
    ResourceQuotas (..),
    mkResourceQuotas,
    rqApplicationQuota,
    rqCustomPlatformQuota,
    rqApplicationVersionQuota,
    rqEnvironmentQuota,
    rqConfigurationTemplateQuota,

    -- * S3Location
    S3Location (..),
    mkS3Location,
    slS3Key,
    slS3Bucket,

    -- * SearchFilter
    SearchFilter (..),
    mkSearchFilter,
    sfAttribute,
    sfValues,
    sfOperator,

    -- * SingleInstanceHealth
    SingleInstanceHealth (..),
    mkSingleInstanceHealth,
    sihInstanceId,
    sihCauses,
    sihSystem,
    sihApplicationMetrics,
    sihColor,
    sihInstanceType,
    sihAvailabilityZone,
    sihHealthStatus,
    sihDeployment,
    sihLaunchedAt,

    -- * SolutionStackDescription
    SolutionStackDescription (..),
    mkSolutionStackDescription,
    ssdPermittedFileTypes,
    ssdSolutionStackName,

    -- * SourceBuildInformation
    SourceBuildInformation (..),
    mkSourceBuildInformation,
    sbiSourceType,
    sbiSourceRepository,
    sbiSourceLocation,

    -- * SourceConfiguration
    SourceConfiguration (..),
    mkSourceConfiguration,
    scTemplateName,
    scApplicationName,

    -- * StatusCodes
    StatusCodes (..),
    mkStatusCodes,
    scStatus2xx,
    scStatus3xx,
    scStatus4xx,
    scStatus5xx,

    -- * SystemStatus
    SystemStatus (..),
    mkSystemStatus,
    ssCPUUtilization,
    ssLoadAverage,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * Trigger
    Trigger (..),
    mkTrigger,
    tName,

    -- * ValidationMessage
    ValidationMessage (..),
    mkValidationMessage,
    vmOptionName,
    vmSeverity,
    vmNamespace,
    vmMessage,
  )
where

import Network.AWS.ElasticBeanstalk.Types.ActionHistoryStatus
import Network.AWS.ElasticBeanstalk.Types.ActionStatus
import Network.AWS.ElasticBeanstalk.Types.ActionType
import Network.AWS.ElasticBeanstalk.Types.ApplicationDescription
import Network.AWS.ElasticBeanstalk.Types.ApplicationDescriptionMessage
import Network.AWS.ElasticBeanstalk.Types.ApplicationMetrics
import Network.AWS.ElasticBeanstalk.Types.ApplicationResourceLifecycleConfig
import Network.AWS.ElasticBeanstalk.Types.ApplicationVersionDescription
import Network.AWS.ElasticBeanstalk.Types.ApplicationVersionDescriptionMessage
import Network.AWS.ElasticBeanstalk.Types.ApplicationVersionLifecycleConfig
import Network.AWS.ElasticBeanstalk.Types.ApplicationVersionStatus
import Network.AWS.ElasticBeanstalk.Types.AutoScalingGroup
import Network.AWS.ElasticBeanstalk.Types.BuildConfiguration
import Network.AWS.ElasticBeanstalk.Types.Builder
import Network.AWS.ElasticBeanstalk.Types.CPUUtilization
import Network.AWS.ElasticBeanstalk.Types.ComputeType
import Network.AWS.ElasticBeanstalk.Types.ConfigurationDeploymentStatus
import Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionDescription
import Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionSetting
import Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionValueType
import Network.AWS.ElasticBeanstalk.Types.ConfigurationSettingsDescription
import Network.AWS.ElasticBeanstalk.Types.CustomAMI
import Network.AWS.ElasticBeanstalk.Types.Deployment
import Network.AWS.ElasticBeanstalk.Types.EnvironmentDescription
import Network.AWS.ElasticBeanstalk.Types.EnvironmentDescriptionsMessage
import Network.AWS.ElasticBeanstalk.Types.EnvironmentHealth
import Network.AWS.ElasticBeanstalk.Types.EnvironmentHealthAttribute
import Network.AWS.ElasticBeanstalk.Types.EnvironmentHealthStatus
import Network.AWS.ElasticBeanstalk.Types.EnvironmentInfoDescription
import Network.AWS.ElasticBeanstalk.Types.EnvironmentInfoType
import Network.AWS.ElasticBeanstalk.Types.EnvironmentLink
import Network.AWS.ElasticBeanstalk.Types.EnvironmentResourceDescription
import Network.AWS.ElasticBeanstalk.Types.EnvironmentResourcesDescription
import Network.AWS.ElasticBeanstalk.Types.EnvironmentStatus
import Network.AWS.ElasticBeanstalk.Types.EnvironmentTier
import Network.AWS.ElasticBeanstalk.Types.EventDescription
import Network.AWS.ElasticBeanstalk.Types.EventSeverity
import Network.AWS.ElasticBeanstalk.Types.FailureType
import Network.AWS.ElasticBeanstalk.Types.Instance
import Network.AWS.ElasticBeanstalk.Types.InstanceHealthSummary
import Network.AWS.ElasticBeanstalk.Types.InstancesHealthAttribute
import Network.AWS.ElasticBeanstalk.Types.Latency
import Network.AWS.ElasticBeanstalk.Types.LaunchConfiguration
import Network.AWS.ElasticBeanstalk.Types.LaunchTemplate
import Network.AWS.ElasticBeanstalk.Types.Listener
import Network.AWS.ElasticBeanstalk.Types.LoadBalancer
import Network.AWS.ElasticBeanstalk.Types.LoadBalancerDescription
import Network.AWS.ElasticBeanstalk.Types.ManagedAction
import Network.AWS.ElasticBeanstalk.Types.ManagedActionHistoryItem
import Network.AWS.ElasticBeanstalk.Types.MaxAgeRule
import Network.AWS.ElasticBeanstalk.Types.MaxCountRule
import Network.AWS.ElasticBeanstalk.Types.OptionRestrictionRegex
import Network.AWS.ElasticBeanstalk.Types.OptionSpecification
import Network.AWS.ElasticBeanstalk.Types.PlatformBranchSummary
import Network.AWS.ElasticBeanstalk.Types.PlatformDescription
import Network.AWS.ElasticBeanstalk.Types.PlatformFilter
import Network.AWS.ElasticBeanstalk.Types.PlatformFramework
import Network.AWS.ElasticBeanstalk.Types.PlatformProgrammingLanguage
import Network.AWS.ElasticBeanstalk.Types.PlatformStatus
import Network.AWS.ElasticBeanstalk.Types.PlatformSummary
import Network.AWS.ElasticBeanstalk.Types.Queue
import Network.AWS.ElasticBeanstalk.Types.ResourceQuota
import Network.AWS.ElasticBeanstalk.Types.ResourceQuotas
import Network.AWS.ElasticBeanstalk.Types.S3Location
import Network.AWS.ElasticBeanstalk.Types.SearchFilter
import Network.AWS.ElasticBeanstalk.Types.SingleInstanceHealth
import Network.AWS.ElasticBeanstalk.Types.SolutionStackDescription
import Network.AWS.ElasticBeanstalk.Types.SourceBuildInformation
import Network.AWS.ElasticBeanstalk.Types.SourceConfiguration
import Network.AWS.ElasticBeanstalk.Types.SourceRepository
import Network.AWS.ElasticBeanstalk.Types.SourceType
import Network.AWS.ElasticBeanstalk.Types.StatusCodes
import Network.AWS.ElasticBeanstalk.Types.SystemStatus
import Network.AWS.ElasticBeanstalk.Types.Tag
import Network.AWS.ElasticBeanstalk.Types.Trigger
import Network.AWS.ElasticBeanstalk.Types.ValidationMessage
import Network.AWS.ElasticBeanstalk.Types.ValidationSeverity
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2010-12-01@ of the Amazon Elastic Beanstalk SDK configuration.
elasticBeanstalkService :: Lude.Service
elasticBeanstalkService =
  Lude.Service
    { Lude._svcAbbrev = "ElasticBeanstalk",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "elasticbeanstalk",
      Lude._svcVersion = "2010-12-01",
      Lude._svcEndpoint = Lude.defaultEndpoint elasticBeanstalkService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseXMLError "ElasticBeanstalk",
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
