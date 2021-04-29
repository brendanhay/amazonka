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
    _TooManyPlatformsException,
    _TooManyTagsException,
    _TooManyApplicationsException,
    _ResourceTypeNotSupportedException,
    _TooManyEnvironmentsException,
    _InsufficientPrivilegesException,
    _TooManyConfigurationTemplatesException,
    _OperationInProgressException,
    _TooManyBucketsException,
    _S3SubscriptionRequiredException,
    _SourceBundleDeletionException,
    _InvalidRequestException,
    _ElasticBeanstalkServiceException,
    _ResourceNotFoundException,
    _CodeBuildNotInServiceRegionException,
    _PlatformVersionStillReferencedException,
    _TooManyApplicationVersionsException,
    _S3LocationNotInServiceRegionException,
    _ManagedActionInvalidStateException,

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
    applicationDescription_dateCreated,
    applicationDescription_versions,
    applicationDescription_dateUpdated,
    applicationDescription_resourceLifecycleConfig,
    applicationDescription_description,
    applicationDescription_configurationTemplates,
    applicationDescription_applicationName,

    -- * ApplicationDescriptionMessage
    ApplicationDescriptionMessage (..),
    newApplicationDescriptionMessage,
    applicationDescriptionMessage_application,

    -- * ApplicationMetrics
    ApplicationMetrics (..),
    newApplicationMetrics,
    applicationMetrics_duration,
    applicationMetrics_statusCodes,
    applicationMetrics_requestCount,
    applicationMetrics_latency,

    -- * ApplicationResourceLifecycleConfig
    ApplicationResourceLifecycleConfig (..),
    newApplicationResourceLifecycleConfig,
    applicationResourceLifecycleConfig_serviceRole,
    applicationResourceLifecycleConfig_versionLifecycleConfig,

    -- * ApplicationVersionDescription
    ApplicationVersionDescription (..),
    newApplicationVersionDescription,
    applicationVersionDescription_status,
    applicationVersionDescription_dateCreated,
    applicationVersionDescription_sourceBundle,
    applicationVersionDescription_sourceBuildInformation,
    applicationVersionDescription_versionLabel,
    applicationVersionDescription_dateUpdated,
    applicationVersionDescription_description,
    applicationVersionDescription_buildArn,
    applicationVersionDescription_applicationVersionArn,
    applicationVersionDescription_applicationName,

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
    buildConfiguration_timeoutInMinutes,
    buildConfiguration_computeType,
    buildConfiguration_codeBuildServiceRole,
    buildConfiguration_image,

    -- * Builder
    Builder (..),
    newBuilder,
    builder_arn,

    -- * CPUUtilization
    CPUUtilization (..),
    newCPUUtilization,
    cPUUtilization_idle,
    cPUUtilization_user,
    cPUUtilization_privileged,
    cPUUtilization_iOWait,
    cPUUtilization_softIRQ,
    cPUUtilization_nice,
    cPUUtilization_system,
    cPUUtilization_irq,

    -- * ConfigurationOptionDescription
    ConfigurationOptionDescription (..),
    newConfigurationOptionDescription,
    configurationOptionDescription_maxValue,
    configurationOptionDescription_valueOptions,
    configurationOptionDescription_valueType,
    configurationOptionDescription_changeSeverity,
    configurationOptionDescription_regex,
    configurationOptionDescription_name,
    configurationOptionDescription_minValue,
    configurationOptionDescription_namespace,
    configurationOptionDescription_userDefined,
    configurationOptionDescription_maxLength,
    configurationOptionDescription_defaultValue,

    -- * ConfigurationOptionSetting
    ConfigurationOptionSetting (..),
    newConfigurationOptionSetting,
    configurationOptionSetting_optionName,
    configurationOptionSetting_value,
    configurationOptionSetting_namespace,
    configurationOptionSetting_resourceName,

    -- * ConfigurationSettingsDescription
    ConfigurationSettingsDescription (..),
    newConfigurationSettingsDescription,
    configurationSettingsDescription_templateName,
    configurationSettingsDescription_dateCreated,
    configurationSettingsDescription_solutionStackName,
    configurationSettingsDescription_deploymentStatus,
    configurationSettingsDescription_environmentName,
    configurationSettingsDescription_platformArn,
    configurationSettingsDescription_dateUpdated,
    configurationSettingsDescription_optionSettings,
    configurationSettingsDescription_description,
    configurationSettingsDescription_applicationName,

    -- * CustomAmi
    CustomAmi (..),
    newCustomAmi,
    customAmi_virtualizationType,
    customAmi_imageId,

    -- * Deployment
    Deployment (..),
    newDeployment,
    deployment_status,
    deployment_deploymentId,
    deployment_versionLabel,
    deployment_deploymentTime,

    -- * EnvironmentDescription
    EnvironmentDescription (..),
    newEnvironmentDescription,
    environmentDescription_abortableOperationInProgress,
    environmentDescription_templateName,
    environmentDescription_status,
    environmentDescription_dateCreated,
    environmentDescription_environmentLinks,
    environmentDescription_solutionStackName,
    environmentDescription_environmentId,
    environmentDescription_environmentName,
    environmentDescription_platformArn,
    environmentDescription_versionLabel,
    environmentDescription_health,
    environmentDescription_cname,
    environmentDescription_resources,
    environmentDescription_dateUpdated,
    environmentDescription_description,
    environmentDescription_healthStatus,
    environmentDescription_environmentArn,
    environmentDescription_endpointURL,
    environmentDescription_applicationName,
    environmentDescription_tier,
    environmentDescription_operationsRole,

    -- * EnvironmentDescriptionsMessage
    EnvironmentDescriptionsMessage (..),
    newEnvironmentDescriptionsMessage,
    environmentDescriptionsMessage_nextToken,
    environmentDescriptionsMessage_environments,

    -- * EnvironmentInfoDescription
    EnvironmentInfoDescription (..),
    newEnvironmentInfoDescription,
    environmentInfoDescription_message,
    environmentInfoDescription_infoType,
    environmentInfoDescription_ec2InstanceId,
    environmentInfoDescription_sampleTimestamp,

    -- * EnvironmentLink
    EnvironmentLink (..),
    newEnvironmentLink,
    environmentLink_environmentName,
    environmentLink_linkName,

    -- * EnvironmentResourceDescription
    EnvironmentResourceDescription (..),
    newEnvironmentResourceDescription,
    environmentResourceDescription_launchConfigurations,
    environmentResourceDescription_launchTemplates,
    environmentResourceDescription_triggers,
    environmentResourceDescription_instances,
    environmentResourceDescription_environmentName,
    environmentResourceDescription_queues,
    environmentResourceDescription_loadBalancers,
    environmentResourceDescription_autoScalingGroups,

    -- * EnvironmentResourcesDescription
    EnvironmentResourcesDescription (..),
    newEnvironmentResourcesDescription,
    environmentResourcesDescription_loadBalancer,

    -- * EnvironmentTier
    EnvironmentTier (..),
    newEnvironmentTier,
    environmentTier_version,
    environmentTier_name,
    environmentTier_type,

    -- * EventDescription
    EventDescription (..),
    newEventDescription,
    eventDescription_templateName,
    eventDescription_severity,
    eventDescription_message,
    eventDescription_eventDate,
    eventDescription_environmentName,
    eventDescription_platformArn,
    eventDescription_versionLabel,
    eventDescription_requestId,
    eventDescription_applicationName,

    -- * Instance
    Instance (..),
    newInstance,
    instance_id,

    -- * InstanceHealthSummary
    InstanceHealthSummary (..),
    newInstanceHealthSummary,
    instanceHealthSummary_ok,
    instanceHealthSummary_noData,
    instanceHealthSummary_info,
    instanceHealthSummary_severe,
    instanceHealthSummary_warning,
    instanceHealthSummary_pending,
    instanceHealthSummary_degraded,
    instanceHealthSummary_unknown,

    -- * Latency
    Latency (..),
    newLatency,
    latency_p95,
    latency_p10,
    latency_p999,
    latency_p99,
    latency_p85,
    latency_p50,
    latency_p90,
    latency_p75,

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
    listener_port,
    listener_protocol,

    -- * LoadBalancer
    LoadBalancer (..),
    newLoadBalancer,
    loadBalancer_name,

    -- * LoadBalancerDescription
    LoadBalancerDescription (..),
    newLoadBalancerDescription,
    loadBalancerDescription_domain,
    loadBalancerDescription_listeners,
    loadBalancerDescription_loadBalancerName,

    -- * ManagedAction
    ManagedAction (..),
    newManagedAction,
    managedAction_status,
    managedAction_actionType,
    managedAction_actionId,
    managedAction_actionDescription,
    managedAction_windowStartTime,

    -- * ManagedActionHistoryItem
    ManagedActionHistoryItem (..),
    newManagedActionHistoryItem,
    managedActionHistoryItem_status,
    managedActionHistoryItem_actionType,
    managedActionHistoryItem_executedTime,
    managedActionHistoryItem_actionId,
    managedActionHistoryItem_actionDescription,
    managedActionHistoryItem_finishedTime,
    managedActionHistoryItem_failureDescription,
    managedActionHistoryItem_failureType,

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
    optionRestrictionRegex_label,
    optionRestrictionRegex_pattern,

    -- * OptionSpecification
    OptionSpecification (..),
    newOptionSpecification,
    optionSpecification_optionName,
    optionSpecification_namespace,
    optionSpecification_resourceName,

    -- * PlatformBranchSummary
    PlatformBranchSummary (..),
    newPlatformBranchSummary,
    platformBranchSummary_branchName,
    platformBranchSummary_branchOrder,
    platformBranchSummary_lifecycleState,
    platformBranchSummary_supportedTierList,
    platformBranchSummary_platformName,

    -- * PlatformDescription
    PlatformDescription (..),
    newPlatformDescription,
    platformDescription_platformCategory,
    platformDescription_operatingSystemName,
    platformDescription_platformBranchName,
    platformDescription_supportedAddonList,
    platformDescription_dateCreated,
    platformDescription_customAmiList,
    platformDescription_platformOwner,
    platformDescription_platformStatus,
    platformDescription_solutionStackName,
    platformDescription_platformVersion,
    platformDescription_platformBranchLifecycleState,
    platformDescription_platformArn,
    platformDescription_frameworks,
    platformDescription_dateUpdated,
    platformDescription_supportedTierList,
    platformDescription_platformLifecycleState,
    platformDescription_maintainer,
    platformDescription_description,
    platformDescription_platformName,
    platformDescription_programmingLanguages,
    platformDescription_operatingSystemVersion,

    -- * PlatformFilter
    PlatformFilter (..),
    newPlatformFilter,
    platformFilter_values,
    platformFilter_operator,
    platformFilter_type,

    -- * PlatformFramework
    PlatformFramework (..),
    newPlatformFramework,
    platformFramework_version,
    platformFramework_name,

    -- * PlatformProgrammingLanguage
    PlatformProgrammingLanguage (..),
    newPlatformProgrammingLanguage,
    platformProgrammingLanguage_version,
    platformProgrammingLanguage_name,

    -- * PlatformSummary
    PlatformSummary (..),
    newPlatformSummary,
    platformSummary_platformCategory,
    platformSummary_operatingSystemName,
    platformSummary_platformBranchName,
    platformSummary_supportedAddonList,
    platformSummary_platformOwner,
    platformSummary_platformStatus,
    platformSummary_platformVersion,
    platformSummary_platformBranchLifecycleState,
    platformSummary_platformArn,
    platformSummary_supportedTierList,
    platformSummary_platformLifecycleState,
    platformSummary_operatingSystemVersion,

    -- * Queue
    Queue (..),
    newQueue,
    queue_name,
    queue_url,

    -- * ResourceQuota
    ResourceQuota (..),
    newResourceQuota,
    resourceQuota_maximum,

    -- * ResourceQuotas
    ResourceQuotas (..),
    newResourceQuotas,
    resourceQuotas_applicationQuota,
    resourceQuotas_configurationTemplateQuota,
    resourceQuotas_applicationVersionQuota,
    resourceQuotas_environmentQuota,
    resourceQuotas_customPlatformQuota,

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_s3Bucket,
    s3Location_s3Key,

    -- * SearchFilter
    SearchFilter (..),
    newSearchFilter,
    searchFilter_values,
    searchFilter_operator,
    searchFilter_attribute,

    -- * SingleInstanceHealth
    SingleInstanceHealth (..),
    newSingleInstanceHealth,
    singleInstanceHealth_instanceId,
    singleInstanceHealth_instanceType,
    singleInstanceHealth_color,
    singleInstanceHealth_causes,
    singleInstanceHealth_availabilityZone,
    singleInstanceHealth_deployment,
    singleInstanceHealth_launchedAt,
    singleInstanceHealth_healthStatus,
    singleInstanceHealth_system,
    singleInstanceHealth_applicationMetrics,

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
    statusCodes_status3xx,
    statusCodes_status5xx,
    statusCodes_status2xx,
    statusCodes_status4xx,

    -- * SystemStatus
    SystemStatus (..),
    newSystemStatus,
    systemStatus_cPUUtilization,
    systemStatus_loadAverage,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * Trigger
    Trigger (..),
    newTrigger,
    trigger_name,

    -- * ValidationMessage
    ValidationMessage (..),
    newValidationMessage,
    validationMessage_optionName,
    validationMessage_severity,
    validationMessage_message,
    validationMessage_namespace,
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
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev =
        "ElasticBeanstalk",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "elasticbeanstalk",
      Prelude._svcVersion = "2010-12-01",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseXMLError "ElasticBeanstalk",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | You have exceeded the maximum number of allowed platforms associated
-- with the account.
_TooManyPlatformsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyPlatformsException =
  Prelude._MatchServiceError
    defaultService
    "TooManyPlatformsException"
    Prelude.. Prelude.hasStatus 400

-- | The number of tags in the resource would exceed the number of tags that
-- each resource can have.
--
-- To calculate this, the operation considers both the number of tags the
-- resource already has and the tags this operation would add if it
-- succeeded.
_TooManyTagsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyTagsException =
  Prelude._MatchServiceError
    defaultService
    "TooManyTagsException"
    Prelude.. Prelude.hasStatus 400

-- | The specified account has reached its limit of applications.
_TooManyApplicationsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyApplicationsException =
  Prelude._MatchServiceError
    defaultService
    "TooManyApplicationsException"
    Prelude.. Prelude.hasStatus 400

-- | The type of the specified Amazon Resource Name (ARN) isn\'t supported
-- for this operation.
_ResourceTypeNotSupportedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceTypeNotSupportedException =
  Prelude._MatchServiceError
    defaultService
    "ResourceTypeNotSupportedException"
    Prelude.. Prelude.hasStatus 400

-- | The specified account has reached its limit of environments.
_TooManyEnvironmentsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyEnvironmentsException =
  Prelude._MatchServiceError
    defaultService
    "TooManyEnvironmentsException"
    Prelude.. Prelude.hasStatus 400

-- | The specified account does not have sufficient privileges for one or
-- more AWS services.
_InsufficientPrivilegesException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InsufficientPrivilegesException =
  Prelude._MatchServiceError
    defaultService
    "InsufficientPrivilegesException"
    Prelude.. Prelude.hasStatus 403

-- | The specified account has reached its limit of configuration templates.
_TooManyConfigurationTemplatesException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyConfigurationTemplatesException =
  Prelude._MatchServiceError
    defaultService
    "TooManyConfigurationTemplatesException"
    Prelude.. Prelude.hasStatus 400

-- | Unable to perform the specified operation because another operation that
-- effects an element in this activity is already in progress.
_OperationInProgressException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_OperationInProgressException =
  Prelude._MatchServiceError
    defaultService
    "OperationInProgressFailure"
    Prelude.. Prelude.hasStatus 400

-- | The specified account has reached its limit of Amazon S3 buckets.
_TooManyBucketsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyBucketsException =
  Prelude._MatchServiceError
    defaultService
    "TooManyBucketsException"
    Prelude.. Prelude.hasStatus 400

-- | The specified account does not have a subscription to Amazon S3.
_S3SubscriptionRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_S3SubscriptionRequiredException =
  Prelude._MatchServiceError
    defaultService
    "S3SubscriptionRequiredException"
    Prelude.. Prelude.hasStatus 400

-- | Unable to delete the Amazon S3 source bundle associated with the
-- application version. The application version was deleted successfully.
_SourceBundleDeletionException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SourceBundleDeletionException =
  Prelude._MatchServiceError
    defaultService
    "SourceBundleDeletionFailure"
    Prelude.. Prelude.hasStatus 400

-- | One or more input parameters is not valid. Please correct the input
-- parameters and try the operation again.
_InvalidRequestException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRequestException =
  Prelude._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Prelude.hasStatus 400

-- | A generic service exception has occurred.
_ElasticBeanstalkServiceException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ElasticBeanstalkServiceException =
  Prelude._MatchServiceError
    defaultService
    "ElasticBeanstalkServiceException"

-- | A resource doesn\'t exist for the specified Amazon Resource Name (ARN).
_ResourceNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Prelude.hasStatus 400

-- | AWS CodeBuild is not available in the specified region.
_CodeBuildNotInServiceRegionException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CodeBuildNotInServiceRegionException =
  Prelude._MatchServiceError
    defaultService
    "CodeBuildNotInServiceRegionException"
    Prelude.. Prelude.hasStatus 400

-- | You cannot delete the platform version because there are still
-- environments running on it.
_PlatformVersionStillReferencedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PlatformVersionStillReferencedException =
  Prelude._MatchServiceError
    defaultService
    "PlatformVersionStillReferencedException"
    Prelude.. Prelude.hasStatus 400

-- | The specified account has reached its limit of application versions.
_TooManyApplicationVersionsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyApplicationVersionsException =
  Prelude._MatchServiceError
    defaultService
    "TooManyApplicationVersionsException"

-- | The specified S3 bucket does not belong to the S3 region in which the
-- service is running. The following regions are supported:
--
-- -   IAD\/us-east-1
--
-- -   PDX\/us-west-2
--
-- -   DUB\/eu-west-1
_S3LocationNotInServiceRegionException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_S3LocationNotInServiceRegionException =
  Prelude._MatchServiceError
    defaultService
    "S3LocationNotInServiceRegionException"
    Prelude.. Prelude.hasStatus 400

-- | Cannot modify the managed action in its current state.
_ManagedActionInvalidStateException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ManagedActionInvalidStateException =
  Prelude._MatchServiceError
    defaultService
    "ManagedActionInvalidStateException"
    Prelude.. Prelude.hasStatus 400
