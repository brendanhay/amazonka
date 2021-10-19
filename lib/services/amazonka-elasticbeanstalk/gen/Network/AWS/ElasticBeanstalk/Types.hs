{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidRequestException,
    _TooManyBucketsException,
    _S3SubscriptionRequiredException,
    _OperationInProgressException,
    _PlatformVersionStillReferencedException,
    _TooManyApplicationVersionsException,
    _TooManyConfigurationTemplatesException,
    _ResourceTypeNotSupportedException,
    _InsufficientPrivilegesException,
    _ElasticBeanstalkServiceException,
    _TooManyTagsException,
    _TooManyApplicationsException,
    _TooManyPlatformsException,
    _ManagedActionInvalidStateException,
    _SourceBundleDeletionException,
    _S3LocationNotInServiceRegionException,
    _CodeBuildNotInServiceRegionException,
    _TooManyEnvironmentsException,
    _ResourceNotFoundException,

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
    newApplicationDescription,
    applicationDescription_applicationArn,
    applicationDescription_versions,
    applicationDescription_dateUpdated,
    applicationDescription_dateCreated,
    applicationDescription_applicationName,
    applicationDescription_configurationTemplates,
    applicationDescription_resourceLifecycleConfig,
    applicationDescription_description,

    -- * ApplicationDescriptionMessage
    ApplicationDescriptionMessage (..),
    newApplicationDescriptionMessage,
    applicationDescriptionMessage_application,

    -- * ApplicationMetrics
    ApplicationMetrics (..),
    newApplicationMetrics,
    applicationMetrics_requestCount,
    applicationMetrics_latency,
    applicationMetrics_statusCodes,
    applicationMetrics_duration,

    -- * ApplicationResourceLifecycleConfig
    ApplicationResourceLifecycleConfig (..),
    newApplicationResourceLifecycleConfig,
    applicationResourceLifecycleConfig_versionLifecycleConfig,
    applicationResourceLifecycleConfig_serviceRole,

    -- * ApplicationVersionDescription
    ApplicationVersionDescription (..),
    newApplicationVersionDescription,
    applicationVersionDescription_status,
    applicationVersionDescription_sourceBundle,
    applicationVersionDescription_dateUpdated,
    applicationVersionDescription_dateCreated,
    applicationVersionDescription_versionLabel,
    applicationVersionDescription_sourceBuildInformation,
    applicationVersionDescription_applicationName,
    applicationVersionDescription_applicationVersionArn,
    applicationVersionDescription_buildArn,
    applicationVersionDescription_description,

    -- * ApplicationVersionDescriptionMessage
    ApplicationVersionDescriptionMessage (..),
    newApplicationVersionDescriptionMessage,
    applicationVersionDescriptionMessage_applicationVersion,

    -- * ApplicationVersionLifecycleConfig
    ApplicationVersionLifecycleConfig (..),
    newApplicationVersionLifecycleConfig,
    applicationVersionLifecycleConfig_maxAgeRule,
    applicationVersionLifecycleConfig_maxCountRule,

    -- * AutoScalingGroup
    AutoScalingGroup (..),
    newAutoScalingGroup,
    autoScalingGroup_name,

    -- * BuildConfiguration
    BuildConfiguration (..),
    newBuildConfiguration,
    buildConfiguration_artifactName,
    buildConfiguration_computeType,
    buildConfiguration_timeoutInMinutes,
    buildConfiguration_codeBuildServiceRole,
    buildConfiguration_image,

    -- * Builder
    Builder (..),
    newBuilder,
    builder_arn,

    -- * CPUUtilization
    CPUUtilization (..),
    newCPUUtilization,
    cPUUtilization_softIRQ,
    cPUUtilization_idle,
    cPUUtilization_irq,
    cPUUtilization_system,
    cPUUtilization_privileged,
    cPUUtilization_user,
    cPUUtilization_iOWait,
    cPUUtilization_nice,

    -- * ConfigurationOptionDescription
    ConfigurationOptionDescription (..),
    newConfigurationOptionDescription,
    configurationOptionDescription_maxValue,
    configurationOptionDescription_regex,
    configurationOptionDescription_maxLength,
    configurationOptionDescription_userDefined,
    configurationOptionDescription_namespace,
    configurationOptionDescription_valueOptions,
    configurationOptionDescription_name,
    configurationOptionDescription_changeSeverity,
    configurationOptionDescription_defaultValue,
    configurationOptionDescription_valueType,
    configurationOptionDescription_minValue,

    -- * ConfigurationOptionSetting
    ConfigurationOptionSetting (..),
    newConfigurationOptionSetting,
    configurationOptionSetting_optionName,
    configurationOptionSetting_resourceName,
    configurationOptionSetting_namespace,
    configurationOptionSetting_value,

    -- * ConfigurationSettingsDescription
    ConfigurationSettingsDescription (..),
    newConfigurationSettingsDescription,
    configurationSettingsDescription_templateName,
    configurationSettingsDescription_optionSettings,
    configurationSettingsDescription_dateUpdated,
    configurationSettingsDescription_dateCreated,
    configurationSettingsDescription_platformArn,
    configurationSettingsDescription_environmentName,
    configurationSettingsDescription_applicationName,
    configurationSettingsDescription_deploymentStatus,
    configurationSettingsDescription_solutionStackName,
    configurationSettingsDescription_description,

    -- * CustomAmi
    CustomAmi (..),
    newCustomAmi,
    customAmi_virtualizationType,
    customAmi_imageId,

    -- * Deployment
    Deployment (..),
    newDeployment,
    deployment_deploymentId,
    deployment_status,
    deployment_deploymentTime,
    deployment_versionLabel,

    -- * EnvironmentDescription
    EnvironmentDescription (..),
    newEnvironmentDescription,
    environmentDescription_status,
    environmentDescription_cname,
    environmentDescription_templateName,
    environmentDescription_abortableOperationInProgress,
    environmentDescription_endpointURL,
    environmentDescription_resources,
    environmentDescription_dateUpdated,
    environmentDescription_dateCreated,
    environmentDescription_health,
    environmentDescription_versionLabel,
    environmentDescription_operationsRole,
    environmentDescription_platformArn,
    environmentDescription_tier,
    environmentDescription_environmentName,
    environmentDescription_applicationName,
    environmentDescription_environmentArn,
    environmentDescription_solutionStackName,
    environmentDescription_environmentId,
    environmentDescription_healthStatus,
    environmentDescription_environmentLinks,
    environmentDescription_description,

    -- * EnvironmentDescriptionsMessage
    EnvironmentDescriptionsMessage (..),
    newEnvironmentDescriptionsMessage,
    environmentDescriptionsMessage_nextToken,
    environmentDescriptionsMessage_environments,

    -- * EnvironmentInfoDescription
    EnvironmentInfoDescription (..),
    newEnvironmentInfoDescription,
    environmentInfoDescription_sampleTimestamp,
    environmentInfoDescription_ec2InstanceId,
    environmentInfoDescription_infoType,
    environmentInfoDescription_message,

    -- * EnvironmentLink
    EnvironmentLink (..),
    newEnvironmentLink,
    environmentLink_linkName,
    environmentLink_environmentName,

    -- * EnvironmentResourceDescription
    EnvironmentResourceDescription (..),
    newEnvironmentResourceDescription,
    environmentResourceDescription_queues,
    environmentResourceDescription_triggers,
    environmentResourceDescription_launchTemplates,
    environmentResourceDescription_loadBalancers,
    environmentResourceDescription_environmentName,
    environmentResourceDescription_instances,
    environmentResourceDescription_launchConfigurations,
    environmentResourceDescription_autoScalingGroups,

    -- * EnvironmentResourcesDescription
    EnvironmentResourcesDescription (..),
    newEnvironmentResourcesDescription,
    environmentResourcesDescription_loadBalancer,

    -- * EnvironmentTier
    EnvironmentTier (..),
    newEnvironmentTier,
    environmentTier_name,
    environmentTier_version,
    environmentTier_type,

    -- * EventDescription
    EventDescription (..),
    newEventDescription,
    eventDescription_requestId,
    eventDescription_templateName,
    eventDescription_severity,
    eventDescription_versionLabel,
    eventDescription_platformArn,
    eventDescription_environmentName,
    eventDescription_applicationName,
    eventDescription_eventDate,
    eventDescription_message,

    -- * Instance
    Instance (..),
    newInstance,
    instance_id,

    -- * InstanceHealthSummary
    InstanceHealthSummary (..),
    newInstanceHealthSummary,
    instanceHealthSummary_ok,
    instanceHealthSummary_pending,
    instanceHealthSummary_severe,
    instanceHealthSummary_unknown,
    instanceHealthSummary_noData,
    instanceHealthSummary_warning,
    instanceHealthSummary_degraded,
    instanceHealthSummary_info,

    -- * Latency
    Latency (..),
    newLatency,
    latency_p75,
    latency_p50,
    latency_p85,
    latency_p999,
    latency_p90,
    latency_p95,
    latency_p99,
    latency_p10,

    -- * LaunchConfiguration
    LaunchConfiguration (..),
    newLaunchConfiguration,
    launchConfiguration_name,

    -- * LaunchTemplate
    LaunchTemplate (..),
    newLaunchTemplate,
    launchTemplate_id,

    -- * Listener
    Listener (..),
    newListener,
    listener_protocol,
    listener_port,

    -- * LoadBalancer
    LoadBalancer (..),
    newLoadBalancer,
    loadBalancer_name,

    -- * LoadBalancerDescription
    LoadBalancerDescription (..),
    newLoadBalancerDescription,
    loadBalancerDescription_loadBalancerName,
    loadBalancerDescription_domain,
    loadBalancerDescription_listeners,

    -- * ManagedAction
    ManagedAction (..),
    newManagedAction,
    managedAction_status,
    managedAction_actionId,
    managedAction_windowStartTime,
    managedAction_actionDescription,
    managedAction_actionType,

    -- * ManagedActionHistoryItem
    ManagedActionHistoryItem (..),
    newManagedActionHistoryItem,
    managedActionHistoryItem_status,
    managedActionHistoryItem_failureType,
    managedActionHistoryItem_actionId,
    managedActionHistoryItem_failureDescription,
    managedActionHistoryItem_finishedTime,
    managedActionHistoryItem_actionDescription,
    managedActionHistoryItem_executedTime,
    managedActionHistoryItem_actionType,

    -- * MaxAgeRule
    MaxAgeRule (..),
    newMaxAgeRule,
    maxAgeRule_deleteSourceFromS3,
    maxAgeRule_maxAgeInDays,
    maxAgeRule_enabled,

    -- * MaxCountRule
    MaxCountRule (..),
    newMaxCountRule,
    maxCountRule_maxCount,
    maxCountRule_deleteSourceFromS3,
    maxCountRule_enabled,

    -- * OptionRestrictionRegex
    OptionRestrictionRegex (..),
    newOptionRestrictionRegex,
    optionRestrictionRegex_pattern,
    optionRestrictionRegex_label,

    -- * OptionSpecification
    OptionSpecification (..),
    newOptionSpecification,
    optionSpecification_optionName,
    optionSpecification_resourceName,
    optionSpecification_namespace,

    -- * PlatformBranchSummary
    PlatformBranchSummary (..),
    newPlatformBranchSummary,
    platformBranchSummary_branchName,
    platformBranchSummary_branchOrder,
    platformBranchSummary_platformName,
    platformBranchSummary_supportedTierList,
    platformBranchSummary_lifecycleState,

    -- * PlatformDescription
    PlatformDescription (..),
    newPlatformDescription,
    platformDescription_platformBranchName,
    platformDescription_supportedAddonList,
    platformDescription_platformCategory,
    platformDescription_platformBranchLifecycleState,
    platformDescription_platformVersion,
    platformDescription_platformStatus,
    platformDescription_maintainer,
    platformDescription_platformLifecycleState,
    platformDescription_platformOwner,
    platformDescription_dateUpdated,
    platformDescription_customAmiList,
    platformDescription_dateCreated,
    platformDescription_operatingSystemName,
    platformDescription_frameworks,
    platformDescription_platformArn,
    platformDescription_operatingSystemVersion,
    platformDescription_programmingLanguages,
    platformDescription_solutionStackName,
    platformDescription_platformName,
    platformDescription_description,
    platformDescription_supportedTierList,

    -- * PlatformFilter
    PlatformFilter (..),
    newPlatformFilter,
    platformFilter_values,
    platformFilter_operator,
    platformFilter_type,

    -- * PlatformFramework
    PlatformFramework (..),
    newPlatformFramework,
    platformFramework_name,
    platformFramework_version,

    -- * PlatformProgrammingLanguage
    PlatformProgrammingLanguage (..),
    newPlatformProgrammingLanguage,
    platformProgrammingLanguage_name,
    platformProgrammingLanguage_version,

    -- * PlatformSummary
    PlatformSummary (..),
    newPlatformSummary,
    platformSummary_platformBranchName,
    platformSummary_supportedAddonList,
    platformSummary_platformCategory,
    platformSummary_platformBranchLifecycleState,
    platformSummary_platformVersion,
    platformSummary_platformStatus,
    platformSummary_platformLifecycleState,
    platformSummary_platformOwner,
    platformSummary_operatingSystemName,
    platformSummary_platformArn,
    platformSummary_operatingSystemVersion,
    platformSummary_supportedTierList,

    -- * Queue
    Queue (..),
    newQueue,
    queue_url,
    queue_name,

    -- * ResourceQuota
    ResourceQuota (..),
    newResourceQuota,
    resourceQuota_maximum,

    -- * ResourceQuotas
    ResourceQuotas (..),
    newResourceQuotas,
    resourceQuotas_applicationQuota,
    resourceQuotas_customPlatformQuota,
    resourceQuotas_applicationVersionQuota,
    resourceQuotas_environmentQuota,
    resourceQuotas_configurationTemplateQuota,

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_s3Key,
    s3Location_s3Bucket,

    -- * SearchFilter
    SearchFilter (..),
    newSearchFilter,
    searchFilter_attribute,
    searchFilter_values,
    searchFilter_operator,

    -- * SingleInstanceHealth
    SingleInstanceHealth (..),
    newSingleInstanceHealth,
    singleInstanceHealth_instanceId,
    singleInstanceHealth_causes,
    singleInstanceHealth_system,
    singleInstanceHealth_applicationMetrics,
    singleInstanceHealth_color,
    singleInstanceHealth_instanceType,
    singleInstanceHealth_availabilityZone,
    singleInstanceHealth_healthStatus,
    singleInstanceHealth_deployment,
    singleInstanceHealth_launchedAt,

    -- * SolutionStackDescription
    SolutionStackDescription (..),
    newSolutionStackDescription,
    solutionStackDescription_permittedFileTypes,
    solutionStackDescription_solutionStackName,

    -- * SourceBuildInformation
    SourceBuildInformation (..),
    newSourceBuildInformation,
    sourceBuildInformation_sourceType,
    sourceBuildInformation_sourceRepository,
    sourceBuildInformation_sourceLocation,

    -- * SourceConfiguration
    SourceConfiguration (..),
    newSourceConfiguration,
    sourceConfiguration_templateName,
    sourceConfiguration_applicationName,

    -- * StatusCodes
    StatusCodes (..),
    newStatusCodes,
    statusCodes_status2xx,
    statusCodes_status3xx,
    statusCodes_status4xx,
    statusCodes_status5xx,

    -- * SystemStatus
    SystemStatus (..),
    newSystemStatus,
    systemStatus_cPUUtilization,
    systemStatus_loadAverage,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * Trigger
    Trigger (..),
    newTrigger,
    trigger_name,

    -- * ValidationMessage
    ValidationMessage (..),
    newValidationMessage,
    validationMessage_optionName,
    validationMessage_severity,
    validationMessage_namespace,
    validationMessage_message,
  )
where

import qualified Network.AWS.Core as Core
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
import Network.AWS.ElasticBeanstalk.Types.CustomAmi
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2010-12-01@ of the Amazon Elastic Beanstalk SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "ElasticBeanstalk",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "elasticbeanstalk",
      Core._serviceSigningName = "elasticbeanstalk",
      Core._serviceVersion = "2010-12-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseXMLError "ElasticBeanstalk",
      Core._serviceRetry = retry
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
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | One or more input parameters is not valid. Please correct the input
-- parameters and try the operation again.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- | The specified account has reached its limit of Amazon S3 buckets.
_TooManyBucketsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyBucketsException =
  Core._MatchServiceError
    defaultService
    "TooManyBucketsException"
    Prelude.. Core.hasStatus 400

-- | The specified account does not have a subscription to Amazon S3.
_S3SubscriptionRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_S3SubscriptionRequiredException =
  Core._MatchServiceError
    defaultService
    "S3SubscriptionRequiredException"
    Prelude.. Core.hasStatus 400

-- | Unable to perform the specified operation because another operation that
-- effects an element in this activity is already in progress.
_OperationInProgressException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationInProgressException =
  Core._MatchServiceError
    defaultService
    "OperationInProgressFailure"
    Prelude.. Core.hasStatus 400

-- | You cannot delete the platform version because there are still
-- environments running on it.
_PlatformVersionStillReferencedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PlatformVersionStillReferencedException =
  Core._MatchServiceError
    defaultService
    "PlatformVersionStillReferencedException"
    Prelude.. Core.hasStatus 400

-- | The specified account has reached its limit of application versions.
_TooManyApplicationVersionsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyApplicationVersionsException =
  Core._MatchServiceError
    defaultService
    "TooManyApplicationVersionsException"

-- | The specified account has reached its limit of configuration templates.
_TooManyConfigurationTemplatesException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyConfigurationTemplatesException =
  Core._MatchServiceError
    defaultService
    "TooManyConfigurationTemplatesException"
    Prelude.. Core.hasStatus 400

-- | The type of the specified Amazon Resource Name (ARN) isn\'t supported
-- for this operation.
_ResourceTypeNotSupportedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceTypeNotSupportedException =
  Core._MatchServiceError
    defaultService
    "ResourceTypeNotSupportedException"
    Prelude.. Core.hasStatus 400

-- | The specified account does not have sufficient privileges for one or
-- more AWS services.
_InsufficientPrivilegesException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientPrivilegesException =
  Core._MatchServiceError
    defaultService
    "InsufficientPrivilegesException"
    Prelude.. Core.hasStatus 403

-- | A generic service exception has occurred.
_ElasticBeanstalkServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ElasticBeanstalkServiceException =
  Core._MatchServiceError
    defaultService
    "ElasticBeanstalkServiceException"

-- | The number of tags in the resource would exceed the number of tags that
-- each resource can have.
--
-- To calculate this, the operation considers both the number of tags the
-- resource already has and the tags this operation would add if it
-- succeeded.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"
    Prelude.. Core.hasStatus 400

-- | The specified account has reached its limit of applications.
_TooManyApplicationsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyApplicationsException =
  Core._MatchServiceError
    defaultService
    "TooManyApplicationsException"
    Prelude.. Core.hasStatus 400

-- | You have exceeded the maximum number of allowed platforms associated
-- with the account.
_TooManyPlatformsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyPlatformsException =
  Core._MatchServiceError
    defaultService
    "TooManyPlatformsException"
    Prelude.. Core.hasStatus 400

-- | Cannot modify the managed action in its current state.
_ManagedActionInvalidStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ManagedActionInvalidStateException =
  Core._MatchServiceError
    defaultService
    "ManagedActionInvalidStateException"
    Prelude.. Core.hasStatus 400

-- | Unable to delete the Amazon S3 source bundle associated with the
-- application version. The application version was deleted successfully.
_SourceBundleDeletionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SourceBundleDeletionException =
  Core._MatchServiceError
    defaultService
    "SourceBundleDeletionFailure"
    Prelude.. Core.hasStatus 400

-- | The specified S3 bucket does not belong to the S3 region in which the
-- service is running. The following regions are supported:
--
-- -   IAD\/us-east-1
--
-- -   PDX\/us-west-2
--
-- -   DUB\/eu-west-1
_S3LocationNotInServiceRegionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_S3LocationNotInServiceRegionException =
  Core._MatchServiceError
    defaultService
    "S3LocationNotInServiceRegionException"
    Prelude.. Core.hasStatus 400

-- | AWS CodeBuild is not available in the specified region.
_CodeBuildNotInServiceRegionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CodeBuildNotInServiceRegionException =
  Core._MatchServiceError
    defaultService
    "CodeBuildNotInServiceRegionException"
    Prelude.. Core.hasStatus 400

-- | The specified account has reached its limit of environments.
_TooManyEnvironmentsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyEnvironmentsException =
  Core._MatchServiceError
    defaultService
    "TooManyEnvironmentsException"
    Prelude.. Core.hasStatus 400

-- | A resource doesn\'t exist for the specified Amazon Resource Name (ARN).
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 400
