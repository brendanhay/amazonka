{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ElasticBeanstalk.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _CodeBuildNotInServiceRegionException,
    _ElasticBeanstalkServiceException,
    _InsufficientPrivilegesException,
    _InvalidRequestException,
    _ManagedActionInvalidStateException,
    _OperationInProgressException,
    _PlatformVersionStillReferencedException,
    _ResourceNotFoundException,
    _ResourceTypeNotSupportedException,
    _S3LocationNotInServiceRegionException,
    _S3SubscriptionRequiredException,
    _SourceBundleDeletionException,
    _TooManyApplicationVersionsException,
    _TooManyApplicationsException,
    _TooManyBucketsException,
    _TooManyConfigurationTemplatesException,
    _TooManyEnvironmentsException,
    _TooManyPlatformsException,
    _TooManyTagsException,

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
    applicationDescription_applicationName,
    applicationDescription_configurationTemplates,
    applicationDescription_dateCreated,
    applicationDescription_dateUpdated,
    applicationDescription_description,
    applicationDescription_resourceLifecycleConfig,
    applicationDescription_versions,

    -- * ApplicationDescriptionMessage
    ApplicationDescriptionMessage (..),
    newApplicationDescriptionMessage,
    applicationDescriptionMessage_application,

    -- * ApplicationMetrics
    ApplicationMetrics (..),
    newApplicationMetrics,
    applicationMetrics_duration,
    applicationMetrics_latency,
    applicationMetrics_requestCount,
    applicationMetrics_statusCodes,

    -- * ApplicationResourceLifecycleConfig
    ApplicationResourceLifecycleConfig (..),
    newApplicationResourceLifecycleConfig,
    applicationResourceLifecycleConfig_serviceRole,
    applicationResourceLifecycleConfig_versionLifecycleConfig,

    -- * ApplicationVersionDescription
    ApplicationVersionDescription (..),
    newApplicationVersionDescription,
    applicationVersionDescription_applicationName,
    applicationVersionDescription_applicationVersionArn,
    applicationVersionDescription_buildArn,
    applicationVersionDescription_dateCreated,
    applicationVersionDescription_dateUpdated,
    applicationVersionDescription_description,
    applicationVersionDescription_sourceBuildInformation,
    applicationVersionDescription_sourceBundle,
    applicationVersionDescription_status,
    applicationVersionDescription_versionLabel,

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
    cPUUtilization_iOWait,
    cPUUtilization_irq,
    cPUUtilization_idle,
    cPUUtilization_nice,
    cPUUtilization_privileged,
    cPUUtilization_softIRQ,
    cPUUtilization_system,
    cPUUtilization_user,

    -- * ConfigurationOptionDescription
    ConfigurationOptionDescription (..),
    newConfigurationOptionDescription,
    configurationOptionDescription_changeSeverity,
    configurationOptionDescription_defaultValue,
    configurationOptionDescription_maxLength,
    configurationOptionDescription_maxValue,
    configurationOptionDescription_minValue,
    configurationOptionDescription_name,
    configurationOptionDescription_namespace,
    configurationOptionDescription_regex,
    configurationOptionDescription_userDefined,
    configurationOptionDescription_valueOptions,
    configurationOptionDescription_valueType,

    -- * ConfigurationOptionSetting
    ConfigurationOptionSetting (..),
    newConfigurationOptionSetting,
    configurationOptionSetting_namespace,
    configurationOptionSetting_optionName,
    configurationOptionSetting_resourceName,
    configurationOptionSetting_value,

    -- * ConfigurationSettingsDescription
    ConfigurationSettingsDescription (..),
    newConfigurationSettingsDescription,
    configurationSettingsDescription_applicationName,
    configurationSettingsDescription_dateCreated,
    configurationSettingsDescription_dateUpdated,
    configurationSettingsDescription_deploymentStatus,
    configurationSettingsDescription_description,
    configurationSettingsDescription_environmentName,
    configurationSettingsDescription_optionSettings,
    configurationSettingsDescription_platformArn,
    configurationSettingsDescription_solutionStackName,
    configurationSettingsDescription_templateName,

    -- * CustomAmi
    CustomAmi (..),
    newCustomAmi,
    customAmi_imageId,
    customAmi_virtualizationType,

    -- * Deployment
    Deployment (..),
    newDeployment,
    deployment_deploymentId,
    deployment_deploymentTime,
    deployment_status,
    deployment_versionLabel,

    -- * EnvironmentDescription
    EnvironmentDescription (..),
    newEnvironmentDescription,
    environmentDescription_abortableOperationInProgress,
    environmentDescription_applicationName,
    environmentDescription_cname,
    environmentDescription_dateCreated,
    environmentDescription_dateUpdated,
    environmentDescription_description,
    environmentDescription_endpointURL,
    environmentDescription_environmentArn,
    environmentDescription_environmentId,
    environmentDescription_environmentLinks,
    environmentDescription_environmentName,
    environmentDescription_health,
    environmentDescription_healthStatus,
    environmentDescription_operationsRole,
    environmentDescription_platformArn,
    environmentDescription_resources,
    environmentDescription_solutionStackName,
    environmentDescription_status,
    environmentDescription_templateName,
    environmentDescription_tier,
    environmentDescription_versionLabel,

    -- * EnvironmentDescriptionsMessage
    EnvironmentDescriptionsMessage (..),
    newEnvironmentDescriptionsMessage,
    environmentDescriptionsMessage_environments,
    environmentDescriptionsMessage_nextToken,

    -- * EnvironmentInfoDescription
    EnvironmentInfoDescription (..),
    newEnvironmentInfoDescription,
    environmentInfoDescription_ec2InstanceId,
    environmentInfoDescription_infoType,
    environmentInfoDescription_message,
    environmentInfoDescription_sampleTimestamp,

    -- * EnvironmentLink
    EnvironmentLink (..),
    newEnvironmentLink,
    environmentLink_environmentName,
    environmentLink_linkName,

    -- * EnvironmentResourceDescription
    EnvironmentResourceDescription (..),
    newEnvironmentResourceDescription,
    environmentResourceDescription_autoScalingGroups,
    environmentResourceDescription_environmentName,
    environmentResourceDescription_instances,
    environmentResourceDescription_launchConfigurations,
    environmentResourceDescription_launchTemplates,
    environmentResourceDescription_loadBalancers,
    environmentResourceDescription_queues,
    environmentResourceDescription_triggers,

    -- * EnvironmentResourcesDescription
    EnvironmentResourcesDescription (..),
    newEnvironmentResourcesDescription,
    environmentResourcesDescription_loadBalancer,

    -- * EnvironmentTier
    EnvironmentTier (..),
    newEnvironmentTier,
    environmentTier_name,
    environmentTier_type,
    environmentTier_version,

    -- * EventDescription
    EventDescription (..),
    newEventDescription,
    eventDescription_applicationName,
    eventDescription_environmentName,
    eventDescription_eventDate,
    eventDescription_message,
    eventDescription_platformArn,
    eventDescription_requestId,
    eventDescription_severity,
    eventDescription_templateName,
    eventDescription_versionLabel,

    -- * Instance
    Instance (..),
    newInstance,
    instance_id,

    -- * InstanceHealthSummary
    InstanceHealthSummary (..),
    newInstanceHealthSummary,
    instanceHealthSummary_degraded,
    instanceHealthSummary_info,
    instanceHealthSummary_noData,
    instanceHealthSummary_ok,
    instanceHealthSummary_pending,
    instanceHealthSummary_severe,
    instanceHealthSummary_unknown,
    instanceHealthSummary_warning,

    -- * Latency
    Latency (..),
    newLatency,
    latency_p10,
    latency_p50,
    latency_p75,
    latency_p85,
    latency_p90,
    latency_p95,
    latency_p99,
    latency_p999,

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
    managedAction_actionDescription,
    managedAction_actionId,
    managedAction_actionType,
    managedAction_status,
    managedAction_windowStartTime,

    -- * ManagedActionHistoryItem
    ManagedActionHistoryItem (..),
    newManagedActionHistoryItem,
    managedActionHistoryItem_actionDescription,
    managedActionHistoryItem_actionId,
    managedActionHistoryItem_actionType,
    managedActionHistoryItem_executedTime,
    managedActionHistoryItem_failureDescription,
    managedActionHistoryItem_failureType,
    managedActionHistoryItem_finishedTime,
    managedActionHistoryItem_status,

    -- * MaxAgeRule
    MaxAgeRule (..),
    newMaxAgeRule,
    maxAgeRule_deleteSourceFromS3,
    maxAgeRule_maxAgeInDays,
    maxAgeRule_enabled,

    -- * MaxCountRule
    MaxCountRule (..),
    newMaxCountRule,
    maxCountRule_deleteSourceFromS3,
    maxCountRule_maxCount,
    maxCountRule_enabled,

    -- * OptionRestrictionRegex
    OptionRestrictionRegex (..),
    newOptionRestrictionRegex,
    optionRestrictionRegex_label,
    optionRestrictionRegex_pattern,

    -- * OptionSpecification
    OptionSpecification (..),
    newOptionSpecification,
    optionSpecification_namespace,
    optionSpecification_optionName,
    optionSpecification_resourceName,

    -- * PlatformBranchSummary
    PlatformBranchSummary (..),
    newPlatformBranchSummary,
    platformBranchSummary_branchName,
    platformBranchSummary_branchOrder,
    platformBranchSummary_lifecycleState,
    platformBranchSummary_platformName,
    platformBranchSummary_supportedTierList,

    -- * PlatformDescription
    PlatformDescription (..),
    newPlatformDescription,
    platformDescription_customAmiList,
    platformDescription_dateCreated,
    platformDescription_dateUpdated,
    platformDescription_description,
    platformDescription_frameworks,
    platformDescription_maintainer,
    platformDescription_operatingSystemName,
    platformDescription_operatingSystemVersion,
    platformDescription_platformArn,
    platformDescription_platformBranchLifecycleState,
    platformDescription_platformBranchName,
    platformDescription_platformCategory,
    platformDescription_platformLifecycleState,
    platformDescription_platformName,
    platformDescription_platformOwner,
    platformDescription_platformStatus,
    platformDescription_platformVersion,
    platformDescription_programmingLanguages,
    platformDescription_solutionStackName,
    platformDescription_supportedAddonList,
    platformDescription_supportedTierList,

    -- * PlatformFilter
    PlatformFilter (..),
    newPlatformFilter,
    platformFilter_operator,
    platformFilter_type,
    platformFilter_values,

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
    platformSummary_operatingSystemName,
    platformSummary_operatingSystemVersion,
    platformSummary_platformArn,
    platformSummary_platformBranchLifecycleState,
    platformSummary_platformBranchName,
    platformSummary_platformCategory,
    platformSummary_platformLifecycleState,
    platformSummary_platformOwner,
    platformSummary_platformStatus,
    platformSummary_platformVersion,
    platformSummary_supportedAddonList,
    platformSummary_supportedTierList,

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
    resourceQuotas_applicationVersionQuota,
    resourceQuotas_configurationTemplateQuota,
    resourceQuotas_customPlatformQuota,
    resourceQuotas_environmentQuota,

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_s3Bucket,
    s3Location_s3Key,

    -- * SearchFilter
    SearchFilter (..),
    newSearchFilter,
    searchFilter_attribute,
    searchFilter_operator,
    searchFilter_values,

    -- * SingleInstanceHealth
    SingleInstanceHealth (..),
    newSingleInstanceHealth,
    singleInstanceHealth_applicationMetrics,
    singleInstanceHealth_availabilityZone,
    singleInstanceHealth_causes,
    singleInstanceHealth_color,
    singleInstanceHealth_deployment,
    singleInstanceHealth_healthStatus,
    singleInstanceHealth_instanceId,
    singleInstanceHealth_instanceType,
    singleInstanceHealth_launchedAt,
    singleInstanceHealth_system,

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
    sourceConfiguration_applicationName,
    sourceConfiguration_templateName,

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
    tag_key,
    tag_value,

    -- * Trigger
    Trigger (..),
    newTrigger,
    trigger_name,

    -- * ValidationMessage
    ValidationMessage (..),
    newValidationMessage,
    validationMessage_message,
    validationMessage_namespace,
    validationMessage_optionName,
    validationMessage_severity,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElasticBeanstalk.Types.ActionHistoryStatus
import Amazonka.ElasticBeanstalk.Types.ActionStatus
import Amazonka.ElasticBeanstalk.Types.ActionType
import Amazonka.ElasticBeanstalk.Types.ApplicationDescription
import Amazonka.ElasticBeanstalk.Types.ApplicationDescriptionMessage
import Amazonka.ElasticBeanstalk.Types.ApplicationMetrics
import Amazonka.ElasticBeanstalk.Types.ApplicationResourceLifecycleConfig
import Amazonka.ElasticBeanstalk.Types.ApplicationVersionDescription
import Amazonka.ElasticBeanstalk.Types.ApplicationVersionDescriptionMessage
import Amazonka.ElasticBeanstalk.Types.ApplicationVersionLifecycleConfig
import Amazonka.ElasticBeanstalk.Types.ApplicationVersionStatus
import Amazonka.ElasticBeanstalk.Types.AutoScalingGroup
import Amazonka.ElasticBeanstalk.Types.BuildConfiguration
import Amazonka.ElasticBeanstalk.Types.Builder
import Amazonka.ElasticBeanstalk.Types.CPUUtilization
import Amazonka.ElasticBeanstalk.Types.ComputeType
import Amazonka.ElasticBeanstalk.Types.ConfigurationDeploymentStatus
import Amazonka.ElasticBeanstalk.Types.ConfigurationOptionDescription
import Amazonka.ElasticBeanstalk.Types.ConfigurationOptionSetting
import Amazonka.ElasticBeanstalk.Types.ConfigurationOptionValueType
import Amazonka.ElasticBeanstalk.Types.ConfigurationSettingsDescription
import Amazonka.ElasticBeanstalk.Types.CustomAmi
import Amazonka.ElasticBeanstalk.Types.Deployment
import Amazonka.ElasticBeanstalk.Types.EnvironmentDescription
import Amazonka.ElasticBeanstalk.Types.EnvironmentDescriptionsMessage
import Amazonka.ElasticBeanstalk.Types.EnvironmentHealth
import Amazonka.ElasticBeanstalk.Types.EnvironmentHealthAttribute
import Amazonka.ElasticBeanstalk.Types.EnvironmentHealthStatus
import Amazonka.ElasticBeanstalk.Types.EnvironmentInfoDescription
import Amazonka.ElasticBeanstalk.Types.EnvironmentInfoType
import Amazonka.ElasticBeanstalk.Types.EnvironmentLink
import Amazonka.ElasticBeanstalk.Types.EnvironmentResourceDescription
import Amazonka.ElasticBeanstalk.Types.EnvironmentResourcesDescription
import Amazonka.ElasticBeanstalk.Types.EnvironmentStatus
import Amazonka.ElasticBeanstalk.Types.EnvironmentTier
import Amazonka.ElasticBeanstalk.Types.EventDescription
import Amazonka.ElasticBeanstalk.Types.EventSeverity
import Amazonka.ElasticBeanstalk.Types.FailureType
import Amazonka.ElasticBeanstalk.Types.Instance
import Amazonka.ElasticBeanstalk.Types.InstanceHealthSummary
import Amazonka.ElasticBeanstalk.Types.InstancesHealthAttribute
import Amazonka.ElasticBeanstalk.Types.Latency
import Amazonka.ElasticBeanstalk.Types.LaunchConfiguration
import Amazonka.ElasticBeanstalk.Types.LaunchTemplate
import Amazonka.ElasticBeanstalk.Types.Listener
import Amazonka.ElasticBeanstalk.Types.LoadBalancer
import Amazonka.ElasticBeanstalk.Types.LoadBalancerDescription
import Amazonka.ElasticBeanstalk.Types.ManagedAction
import Amazonka.ElasticBeanstalk.Types.ManagedActionHistoryItem
import Amazonka.ElasticBeanstalk.Types.MaxAgeRule
import Amazonka.ElasticBeanstalk.Types.MaxCountRule
import Amazonka.ElasticBeanstalk.Types.OptionRestrictionRegex
import Amazonka.ElasticBeanstalk.Types.OptionSpecification
import Amazonka.ElasticBeanstalk.Types.PlatformBranchSummary
import Amazonka.ElasticBeanstalk.Types.PlatformDescription
import Amazonka.ElasticBeanstalk.Types.PlatformFilter
import Amazonka.ElasticBeanstalk.Types.PlatformFramework
import Amazonka.ElasticBeanstalk.Types.PlatformProgrammingLanguage
import Amazonka.ElasticBeanstalk.Types.PlatformStatus
import Amazonka.ElasticBeanstalk.Types.PlatformSummary
import Amazonka.ElasticBeanstalk.Types.Queue
import Amazonka.ElasticBeanstalk.Types.ResourceQuota
import Amazonka.ElasticBeanstalk.Types.ResourceQuotas
import Amazonka.ElasticBeanstalk.Types.S3Location
import Amazonka.ElasticBeanstalk.Types.SearchFilter
import Amazonka.ElasticBeanstalk.Types.SingleInstanceHealth
import Amazonka.ElasticBeanstalk.Types.SolutionStackDescription
import Amazonka.ElasticBeanstalk.Types.SourceBuildInformation
import Amazonka.ElasticBeanstalk.Types.SourceConfiguration
import Amazonka.ElasticBeanstalk.Types.SourceRepository
import Amazonka.ElasticBeanstalk.Types.SourceType
import Amazonka.ElasticBeanstalk.Types.StatusCodes
import Amazonka.ElasticBeanstalk.Types.SystemStatus
import Amazonka.ElasticBeanstalk.Types.Tag
import Amazonka.ElasticBeanstalk.Types.Trigger
import Amazonka.ElasticBeanstalk.Types.ValidationMessage
import Amazonka.ElasticBeanstalk.Types.ValidationSeverity
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2010-12-01@ of the Amazon Elastic Beanstalk SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "ElasticBeanstalk",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "elasticbeanstalk",
      Core.signingName = "elasticbeanstalk",
      Core.version = "2010-12-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseXMLError "ElasticBeanstalk",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | AWS CodeBuild is not available in the specified region.
_CodeBuildNotInServiceRegionException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CodeBuildNotInServiceRegionException =
  Core._MatchServiceError
    defaultService
    "CodeBuildNotInServiceRegionException"
    Prelude.. Core.hasStatus 400

-- | A generic service exception has occurred.
_ElasticBeanstalkServiceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ElasticBeanstalkServiceException =
  Core._MatchServiceError
    defaultService
    "ElasticBeanstalkServiceException"

-- | The specified account does not have sufficient privileges for one or
-- more AWS services.
_InsufficientPrivilegesException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InsufficientPrivilegesException =
  Core._MatchServiceError
    defaultService
    "InsufficientPrivilegesException"
    Prelude.. Core.hasStatus 403

-- | One or more input parameters is not valid. Please correct the input
-- parameters and try the operation again.
_InvalidRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- | Cannot modify the managed action in its current state.
_ManagedActionInvalidStateException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ManagedActionInvalidStateException =
  Core._MatchServiceError
    defaultService
    "ManagedActionInvalidStateException"
    Prelude.. Core.hasStatus 400

-- | Unable to perform the specified operation because another operation that
-- effects an element in this activity is already in progress.
_OperationInProgressException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OperationInProgressException =
  Core._MatchServiceError
    defaultService
    "OperationInProgressFailure"
    Prelude.. Core.hasStatus 400

-- | You cannot delete the platform version because there are still
-- environments running on it.
_PlatformVersionStillReferencedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PlatformVersionStillReferencedException =
  Core._MatchServiceError
    defaultService
    "PlatformVersionStillReferencedException"
    Prelude.. Core.hasStatus 400

-- | A resource doesn\'t exist for the specified Amazon Resource Name (ARN).
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 400

-- | The type of the specified Amazon Resource Name (ARN) isn\'t supported
-- for this operation.
_ResourceTypeNotSupportedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceTypeNotSupportedException =
  Core._MatchServiceError
    defaultService
    "ResourceTypeNotSupportedException"
    Prelude.. Core.hasStatus 400

-- | The specified S3 bucket does not belong to the S3 region in which the
-- service is running. The following regions are supported:
--
-- -   IAD\/us-east-1
--
-- -   PDX\/us-west-2
--
-- -   DUB\/eu-west-1
_S3LocationNotInServiceRegionException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_S3LocationNotInServiceRegionException =
  Core._MatchServiceError
    defaultService
    "S3LocationNotInServiceRegionException"
    Prelude.. Core.hasStatus 400

-- | The specified account does not have a subscription to Amazon S3.
_S3SubscriptionRequiredException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_S3SubscriptionRequiredException =
  Core._MatchServiceError
    defaultService
    "S3SubscriptionRequiredException"
    Prelude.. Core.hasStatus 400

-- | Unable to delete the Amazon S3 source bundle associated with the
-- application version. The application version was deleted successfully.
_SourceBundleDeletionException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SourceBundleDeletionException =
  Core._MatchServiceError
    defaultService
    "SourceBundleDeletionFailure"
    Prelude.. Core.hasStatus 400

-- | The specified account has reached its limit of application versions.
_TooManyApplicationVersionsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyApplicationVersionsException =
  Core._MatchServiceError
    defaultService
    "TooManyApplicationVersionsException"

-- | The specified account has reached its limit of applications.
_TooManyApplicationsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyApplicationsException =
  Core._MatchServiceError
    defaultService
    "TooManyApplicationsException"
    Prelude.. Core.hasStatus 400

-- | The specified account has reached its limit of Amazon S3 buckets.
_TooManyBucketsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyBucketsException =
  Core._MatchServiceError
    defaultService
    "TooManyBucketsException"
    Prelude.. Core.hasStatus 400

-- | The specified account has reached its limit of configuration templates.
_TooManyConfigurationTemplatesException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyConfigurationTemplatesException =
  Core._MatchServiceError
    defaultService
    "TooManyConfigurationTemplatesException"
    Prelude.. Core.hasStatus 400

-- | The specified account has reached its limit of environments.
_TooManyEnvironmentsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyEnvironmentsException =
  Core._MatchServiceError
    defaultService
    "TooManyEnvironmentsException"
    Prelude.. Core.hasStatus 400

-- | You have exceeded the maximum number of allowed platforms associated
-- with the account.
_TooManyPlatformsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyPlatformsException =
  Core._MatchServiceError
    defaultService
    "TooManyPlatformsException"
    Prelude.. Core.hasStatus 400

-- | The number of tags in the resource would exceed the number of tags that
-- each resource can have.
--
-- To calculate this, the operation considers both the number of tags the
-- resource already has and the tags this operation would add if it
-- succeeded.
_TooManyTagsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"
    Prelude.. Core.hasStatus 400
