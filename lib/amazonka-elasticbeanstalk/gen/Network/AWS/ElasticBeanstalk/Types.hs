{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types
  ( -- * Service Configuration
    elasticBeanstalk,

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
    ApplicationDescription,
    applicationDescription,
    adApplicationARN,
    adVersions,
    adDateUpdated,
    adDateCreated,
    adApplicationName,
    adConfigurationTemplates,
    adResourceLifecycleConfig,
    adDescription,

    -- * ApplicationDescriptionMessage
    ApplicationDescriptionMessage,
    applicationDescriptionMessage,
    admApplication,

    -- * ApplicationMetrics
    ApplicationMetrics,
    applicationMetrics,
    amRequestCount,
    amLatency,
    amStatusCodes,
    amDuration,

    -- * ApplicationResourceLifecycleConfig
    ApplicationResourceLifecycleConfig,
    applicationResourceLifecycleConfig,
    arlcVersionLifecycleConfig,
    arlcServiceRole,

    -- * ApplicationVersionDescription
    ApplicationVersionDescription,
    applicationVersionDescription,
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
    ApplicationVersionDescriptionMessage,
    applicationVersionDescriptionMessage,
    avdmApplicationVersion,

    -- * ApplicationVersionLifecycleConfig
    ApplicationVersionLifecycleConfig,
    applicationVersionLifecycleConfig,
    avlcMaxAgeRule,
    avlcMaxCountRule,

    -- * AutoScalingGroup
    AutoScalingGroup,
    autoScalingGroup,
    asgName,

    -- * BuildConfiguration
    BuildConfiguration,
    buildConfiguration,
    bcArtifactName,
    bcComputeType,
    bcTimeoutInMinutes,
    bcCodeBuildServiceRole,
    bcImage,

    -- * Builder
    Builder,
    builder,
    bARN,

    -- * CPUUtilization
    CPUUtilization,
    cpuUtilization,
    cuSoftIRQ,
    cuIdle,
    cuIRQ,
    cuSystem,
    cuPrivileged,
    cuUser,
    cuIOWait,
    cuNice,

    -- * ConfigurationOptionDescription
    ConfigurationOptionDescription,
    configurationOptionDescription,
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
    ConfigurationOptionSetting,
    configurationOptionSetting,
    cosOptionName,
    cosResourceName,
    cosNamespace,
    cosValue,

    -- * ConfigurationSettingsDescription
    ConfigurationSettingsDescription,
    configurationSettingsDescription,
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
    CustomAMI,
    customAMI,
    caVirtualizationType,
    caImageId,

    -- * Deployment
    Deployment,
    deployment,
    dDeploymentId,
    dStatus,
    dDeploymentTime,
    dVersionLabel,

    -- * EnvironmentDescription
    EnvironmentDescription,
    environmentDescription,
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
    EnvironmentDescriptionsMessage,
    environmentDescriptionsMessage,
    edmNextToken,
    edmEnvironments,

    -- * EnvironmentInfoDescription
    EnvironmentInfoDescription,
    environmentInfoDescription,
    eidSampleTimestamp,
    eidEC2InstanceId,
    eidInfoType,
    eidMessage,

    -- * EnvironmentLink
    EnvironmentLink,
    environmentLink,
    elLinkName,
    elEnvironmentName,

    -- * EnvironmentResourceDescription
    EnvironmentResourceDescription,
    environmentResourceDescription,
    erdQueues,
    erdTriggers,
    erdLaunchTemplates,
    erdLoadBalancers,
    erdEnvironmentName,
    erdInstances,
    erdLaunchConfigurations,
    erdAutoScalingGroups,

    -- * EnvironmentResourcesDescription
    EnvironmentResourcesDescription,
    environmentResourcesDescription,
    erdLoadBalancer,

    -- * EnvironmentTier
    EnvironmentTier,
    environmentTier,
    etName,
    etVersion,
    etType,

    -- * EventDescription
    EventDescription,
    eventDescription,
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
    Instance,
    instance',
    iId,

    -- * InstanceHealthSummary
    InstanceHealthSummary,
    instanceHealthSummary,
    ihsOK,
    ihsPending,
    ihsSevere,
    ihsUnknown,
    ihsNoData,
    ihsWarning,
    ihsDegraded,
    ihsInfo,

    -- * Latency
    Latency,
    latency,
    lP75,
    lP50,
    lP85,
    lP999,
    lP90,
    lP95,
    lP99,
    lP10,

    -- * LaunchConfiguration
    LaunchConfiguration,
    launchConfiguration,
    lcName,

    -- * LaunchTemplate
    LaunchTemplate,
    launchTemplate,
    ltId,

    -- * Listener
    Listener,
    listener,
    lProtocol,
    lPort,

    -- * LoadBalancer
    LoadBalancer,
    loadBalancer,
    lbName,

    -- * LoadBalancerDescription
    LoadBalancerDescription,
    loadBalancerDescription,
    lbdLoadBalancerName,
    lbdDomain,
    lbdListeners,

    -- * ManagedAction
    ManagedAction,
    managedAction,
    maStatus,
    maActionId,
    maWindowStartTime,
    maActionDescription,
    maActionType,

    -- * ManagedActionHistoryItem
    ManagedActionHistoryItem,
    managedActionHistoryItem,
    mahiStatus,
    mahiFailureType,
    mahiActionId,
    mahiFailureDescription,
    mahiFinishedTime,
    mahiActionDescription,
    mahiExecutedTime,
    mahiActionType,

    -- * MaxAgeRule
    MaxAgeRule,
    maxAgeRule,
    marDeleteSourceFromS3,
    marMaxAgeInDays,
    marEnabled,

    -- * MaxCountRule
    MaxCountRule,
    maxCountRule,
    mcrMaxCount,
    mcrDeleteSourceFromS3,
    mcrEnabled,

    -- * OptionRestrictionRegex
    OptionRestrictionRegex,
    optionRestrictionRegex,
    orrPattern,
    orrLabel,

    -- * OptionSpecification
    OptionSpecification,
    optionSpecification,
    osOptionName,
    osResourceName,
    osNamespace,

    -- * PlatformBranchSummary
    PlatformBranchSummary,
    platformBranchSummary,
    pbsBranchName,
    pbsBranchOrder,
    pbsPlatformName,
    pbsSupportedTierList,
    pbsLifecycleState,

    -- * PlatformDescription
    PlatformDescription,
    platformDescription,
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
    PlatformFilter,
    platformFilter,
    pfValues,
    pfOperator,
    pfType,

    -- * PlatformFramework
    PlatformFramework,
    platformFramework,
    pfName,
    pfVersion,

    -- * PlatformProgrammingLanguage
    PlatformProgrammingLanguage,
    platformProgrammingLanguage,
    pplName,
    pplVersion,

    -- * PlatformSummary
    PlatformSummary,
    platformSummary,
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
    Queue,
    queue,
    qURL,
    qName,

    -- * ResourceQuota
    ResourceQuota,
    resourceQuota,
    rqMaximum,

    -- * ResourceQuotas
    ResourceQuotas,
    resourceQuotas,
    rqApplicationQuota,
    rqCustomPlatformQuota,
    rqApplicationVersionQuota,
    rqEnvironmentQuota,
    rqConfigurationTemplateQuota,

    -- * S3Location
    S3Location,
    s3Location,
    slS3Key,
    slS3Bucket,

    -- * SearchFilter
    SearchFilter,
    searchFilter,
    sfAttribute,
    sfValues,
    sfOperator,

    -- * SingleInstanceHealth
    SingleInstanceHealth,
    singleInstanceHealth,
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
    SolutionStackDescription,
    solutionStackDescription,
    ssdPermittedFileTypes,
    ssdSolutionStackName,

    -- * SourceBuildInformation
    SourceBuildInformation,
    sourceBuildInformation,
    sbiSourceType,
    sbiSourceRepository,
    sbiSourceLocation,

    -- * SourceConfiguration
    SourceConfiguration,
    sourceConfiguration,
    scTemplateName,
    scApplicationName,

    -- * StatusCodes
    StatusCodes,
    statusCodes,
    scStatus2xx,
    scStatus3xx,
    scStatus4xx,
    scStatus5xx,

    -- * SystemStatus
    SystemStatus,
    systemStatus,
    ssCPUUtilization,
    ssLoadAverage,

    -- * Tag
    Tag,
    tag,
    tagValue,
    tagKey,

    -- * Trigger
    Trigger,
    trigger,
    tName,

    -- * ValidationMessage
    ValidationMessage,
    validationMessage,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2010-12-01@ of the Amazon Elastic Beanstalk SDK configuration.
elasticBeanstalk :: Service
elasticBeanstalk =
  Service
    { _svcAbbrev = "ElasticBeanstalk",
      _svcSigner = v4,
      _svcPrefix = "elasticbeanstalk",
      _svcVersion = "2010-12-01",
      _svcEndpoint = defaultEndpoint elasticBeanstalk,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseXMLError "ElasticBeanstalk",
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
