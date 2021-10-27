{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GreengrassV2.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GreengrassV2.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _ConflictException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _RequestAlreadyInProgressException,
    _InternalServerException,
    _ResourceNotFoundException,

    -- * CloudComponentState
    CloudComponentState (..),

    -- * ComponentDependencyType
    ComponentDependencyType (..),

    -- * ComponentVisibilityScope
    ComponentVisibilityScope (..),

    -- * CoreDeviceStatus
    CoreDeviceStatus (..),

    -- * DeploymentComponentUpdatePolicyAction
    DeploymentComponentUpdatePolicyAction (..),

    -- * DeploymentFailureHandlingPolicy
    DeploymentFailureHandlingPolicy (..),

    -- * DeploymentHistoryFilter
    DeploymentHistoryFilter (..),

    -- * DeploymentStatus
    DeploymentStatus (..),

    -- * EffectiveDeploymentExecutionStatus
    EffectiveDeploymentExecutionStatus (..),

    -- * InstalledComponentLifecycleState
    InstalledComponentLifecycleState (..),

    -- * IoTJobAbortAction
    IoTJobAbortAction (..),

    -- * IoTJobExecutionFailureType
    IoTJobExecutionFailureType (..),

    -- * LambdaEventSourceType
    LambdaEventSourceType (..),

    -- * LambdaFilesystemPermission
    LambdaFilesystemPermission (..),

    -- * LambdaInputPayloadEncodingType
    LambdaInputPayloadEncodingType (..),

    -- * LambdaIsolationMode
    LambdaIsolationMode (..),

    -- * RecipeOutputFormat
    RecipeOutputFormat (..),

    -- * AssociateClientDeviceWithCoreDeviceEntry
    AssociateClientDeviceWithCoreDeviceEntry (..),
    newAssociateClientDeviceWithCoreDeviceEntry,
    associateClientDeviceWithCoreDeviceEntry_thingName,

    -- * AssociateClientDeviceWithCoreDeviceErrorEntry
    AssociateClientDeviceWithCoreDeviceErrorEntry (..),
    newAssociateClientDeviceWithCoreDeviceErrorEntry,
    associateClientDeviceWithCoreDeviceErrorEntry_code,
    associateClientDeviceWithCoreDeviceErrorEntry_message,
    associateClientDeviceWithCoreDeviceErrorEntry_thingName,

    -- * AssociatedClientDevice
    AssociatedClientDevice (..),
    newAssociatedClientDevice,
    associatedClientDevice_associationTimestamp,
    associatedClientDevice_thingName,

    -- * CloudComponentStatus
    CloudComponentStatus (..),
    newCloudComponentStatus,
    cloudComponentStatus_componentState,
    cloudComponentStatus_message,
    cloudComponentStatus_errors,

    -- * Component
    Component (..),
    newComponent,
    component_arn,
    component_componentName,
    component_latestVersion,

    -- * ComponentCandidate
    ComponentCandidate (..),
    newComponentCandidate,
    componentCandidate_componentVersion,
    componentCandidate_versionRequirements,
    componentCandidate_componentName,

    -- * ComponentConfigurationUpdate
    ComponentConfigurationUpdate (..),
    newComponentConfigurationUpdate,
    componentConfigurationUpdate_reset,
    componentConfigurationUpdate_merge,

    -- * ComponentDependencyRequirement
    ComponentDependencyRequirement (..),
    newComponentDependencyRequirement,
    componentDependencyRequirement_dependencyType,
    componentDependencyRequirement_versionRequirement,

    -- * ComponentDeploymentSpecification
    ComponentDeploymentSpecification (..),
    newComponentDeploymentSpecification,
    componentDeploymentSpecification_componentVersion,
    componentDeploymentSpecification_runWith,
    componentDeploymentSpecification_configurationUpdate,

    -- * ComponentLatestVersion
    ComponentLatestVersion (..),
    newComponentLatestVersion,
    componentLatestVersion_platforms,
    componentLatestVersion_arn,
    componentLatestVersion_componentVersion,
    componentLatestVersion_creationTimestamp,
    componentLatestVersion_publisher,
    componentLatestVersion_description,

    -- * ComponentPlatform
    ComponentPlatform (..),
    newComponentPlatform,
    componentPlatform_name,
    componentPlatform_attributes,

    -- * ComponentRunWith
    ComponentRunWith (..),
    newComponentRunWith,
    componentRunWith_posixUser,
    componentRunWith_systemResourceLimits,

    -- * ComponentVersionListItem
    ComponentVersionListItem (..),
    newComponentVersionListItem,
    componentVersionListItem_arn,
    componentVersionListItem_componentVersion,
    componentVersionListItem_componentName,

    -- * CoreDevice
    CoreDevice (..),
    newCoreDevice,
    coreDevice_status,
    coreDevice_coreDeviceThingName,
    coreDevice_lastStatusUpdateTimestamp,

    -- * Deployment
    Deployment (..),
    newDeployment,
    deployment_targetArn,
    deployment_deploymentId,
    deployment_creationTimestamp,
    deployment_deploymentStatus,
    deployment_isLatestForTarget,
    deployment_revisionId,
    deployment_deploymentName,

    -- * DeploymentComponentUpdatePolicy
    DeploymentComponentUpdatePolicy (..),
    newDeploymentComponentUpdatePolicy,
    deploymentComponentUpdatePolicy_action,
    deploymentComponentUpdatePolicy_timeoutInSeconds,

    -- * DeploymentConfigurationValidationPolicy
    DeploymentConfigurationValidationPolicy (..),
    newDeploymentConfigurationValidationPolicy,
    deploymentConfigurationValidationPolicy_timeoutInSeconds,

    -- * DeploymentIoTJobConfiguration
    DeploymentIoTJobConfiguration (..),
    newDeploymentIoTJobConfiguration,
    deploymentIoTJobConfiguration_jobExecutionsRolloutConfig,
    deploymentIoTJobConfiguration_abortConfig,
    deploymentIoTJobConfiguration_timeoutConfig,

    -- * DeploymentPolicies
    DeploymentPolicies (..),
    newDeploymentPolicies,
    deploymentPolicies_failureHandlingPolicy,
    deploymentPolicies_configurationValidationPolicy,
    deploymentPolicies_componentUpdatePolicy,

    -- * DisassociateClientDeviceFromCoreDeviceEntry
    DisassociateClientDeviceFromCoreDeviceEntry (..),
    newDisassociateClientDeviceFromCoreDeviceEntry,
    disassociateClientDeviceFromCoreDeviceEntry_thingName,

    -- * DisassociateClientDeviceFromCoreDeviceErrorEntry
    DisassociateClientDeviceFromCoreDeviceErrorEntry (..),
    newDisassociateClientDeviceFromCoreDeviceErrorEntry,
    disassociateClientDeviceFromCoreDeviceErrorEntry_code,
    disassociateClientDeviceFromCoreDeviceErrorEntry_message,
    disassociateClientDeviceFromCoreDeviceErrorEntry_thingName,

    -- * EffectiveDeployment
    EffectiveDeployment (..),
    newEffectiveDeployment,
    effectiveDeployment_iotJobId,
    effectiveDeployment_iotJobArn,
    effectiveDeployment_reason,
    effectiveDeployment_description,
    effectiveDeployment_deploymentId,
    effectiveDeployment_deploymentName,
    effectiveDeployment_targetArn,
    effectiveDeployment_coreDeviceExecutionStatus,
    effectiveDeployment_creationTimestamp,
    effectiveDeployment_modifiedTimestamp,

    -- * InstalledComponent
    InstalledComponent (..),
    newInstalledComponent,
    installedComponent_isRoot,
    installedComponent_componentVersion,
    installedComponent_componentName,
    installedComponent_lifecycleStateDetails,
    installedComponent_lifecycleState,

    -- * IoTJobAbortConfig
    IoTJobAbortConfig (..),
    newIoTJobAbortConfig,
    ioTJobAbortConfig_criteriaList,

    -- * IoTJobAbortCriteria
    IoTJobAbortCriteria (..),
    newIoTJobAbortCriteria,
    ioTJobAbortCriteria_failureType,
    ioTJobAbortCriteria_action,
    ioTJobAbortCriteria_thresholdPercentage,
    ioTJobAbortCriteria_minNumberOfExecutedThings,

    -- * IoTJobExecutionsRolloutConfig
    IoTJobExecutionsRolloutConfig (..),
    newIoTJobExecutionsRolloutConfig,
    ioTJobExecutionsRolloutConfig_exponentialRate,
    ioTJobExecutionsRolloutConfig_maximumPerMinute,

    -- * IoTJobExponentialRolloutRate
    IoTJobExponentialRolloutRate (..),
    newIoTJobExponentialRolloutRate,
    ioTJobExponentialRolloutRate_baseRatePerMinute,
    ioTJobExponentialRolloutRate_incrementFactor,
    ioTJobExponentialRolloutRate_rateIncreaseCriteria,

    -- * IoTJobRateIncreaseCriteria
    IoTJobRateIncreaseCriteria (..),
    newIoTJobRateIncreaseCriteria,
    ioTJobRateIncreaseCriteria_numberOfNotifiedThings,
    ioTJobRateIncreaseCriteria_numberOfSucceededThings,

    -- * IoTJobTimeoutConfig
    IoTJobTimeoutConfig (..),
    newIoTJobTimeoutConfig,
    ioTJobTimeoutConfig_inProgressTimeoutInMinutes,

    -- * LambdaContainerParams
    LambdaContainerParams (..),
    newLambdaContainerParams,
    lambdaContainerParams_mountROSysfs,
    lambdaContainerParams_memorySizeInKB,
    lambdaContainerParams_devices,
    lambdaContainerParams_volumes,

    -- * LambdaDeviceMount
    LambdaDeviceMount (..),
    newLambdaDeviceMount,
    lambdaDeviceMount_addGroupOwner,
    lambdaDeviceMount_permission,
    lambdaDeviceMount_path,

    -- * LambdaEventSource
    LambdaEventSource (..),
    newLambdaEventSource,
    lambdaEventSource_topic,
    lambdaEventSource_type,

    -- * LambdaExecutionParameters
    LambdaExecutionParameters (..),
    newLambdaExecutionParameters,
    lambdaExecutionParameters_execArgs,
    lambdaExecutionParameters_maxQueueSize,
    lambdaExecutionParameters_pinned,
    lambdaExecutionParameters_inputPayloadEncodingType,
    lambdaExecutionParameters_maxIdleTimeInSeconds,
    lambdaExecutionParameters_timeoutInSeconds,
    lambdaExecutionParameters_eventSources,
    lambdaExecutionParameters_maxInstancesCount,
    lambdaExecutionParameters_environmentVariables,
    lambdaExecutionParameters_statusTimeoutInSeconds,
    lambdaExecutionParameters_linuxProcessParams,

    -- * LambdaFunctionRecipeSource
    LambdaFunctionRecipeSource (..),
    newLambdaFunctionRecipeSource,
    lambdaFunctionRecipeSource_componentLambdaParameters,
    lambdaFunctionRecipeSource_componentVersion,
    lambdaFunctionRecipeSource_componentName,
    lambdaFunctionRecipeSource_componentPlatforms,
    lambdaFunctionRecipeSource_componentDependencies,
    lambdaFunctionRecipeSource_lambdaArn,

    -- * LambdaLinuxProcessParams
    LambdaLinuxProcessParams (..),
    newLambdaLinuxProcessParams,
    lambdaLinuxProcessParams_containerParams,
    lambdaLinuxProcessParams_isolationMode,

    -- * LambdaVolumeMount
    LambdaVolumeMount (..),
    newLambdaVolumeMount,
    lambdaVolumeMount_addGroupOwner,
    lambdaVolumeMount_permission,
    lambdaVolumeMount_sourcePath,
    lambdaVolumeMount_destinationPath,

    -- * ResolvedComponentVersion
    ResolvedComponentVersion (..),
    newResolvedComponentVersion,
    resolvedComponentVersion_arn,
    resolvedComponentVersion_componentVersion,
    resolvedComponentVersion_recipe,
    resolvedComponentVersion_componentName,

    -- * SystemResourceLimits
    SystemResourceLimits (..),
    newSystemResourceLimits,
    systemResourceLimits_memory,
    systemResourceLimits_cpus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GreengrassV2.Types.AssociateClientDeviceWithCoreDeviceEntry
import Network.AWS.GreengrassV2.Types.AssociateClientDeviceWithCoreDeviceErrorEntry
import Network.AWS.GreengrassV2.Types.AssociatedClientDevice
import Network.AWS.GreengrassV2.Types.CloudComponentState
import Network.AWS.GreengrassV2.Types.CloudComponentStatus
import Network.AWS.GreengrassV2.Types.Component
import Network.AWS.GreengrassV2.Types.ComponentCandidate
import Network.AWS.GreengrassV2.Types.ComponentConfigurationUpdate
import Network.AWS.GreengrassV2.Types.ComponentDependencyRequirement
import Network.AWS.GreengrassV2.Types.ComponentDependencyType
import Network.AWS.GreengrassV2.Types.ComponentDeploymentSpecification
import Network.AWS.GreengrassV2.Types.ComponentLatestVersion
import Network.AWS.GreengrassV2.Types.ComponentPlatform
import Network.AWS.GreengrassV2.Types.ComponentRunWith
import Network.AWS.GreengrassV2.Types.ComponentVersionListItem
import Network.AWS.GreengrassV2.Types.ComponentVisibilityScope
import Network.AWS.GreengrassV2.Types.CoreDevice
import Network.AWS.GreengrassV2.Types.CoreDeviceStatus
import Network.AWS.GreengrassV2.Types.Deployment
import Network.AWS.GreengrassV2.Types.DeploymentComponentUpdatePolicy
import Network.AWS.GreengrassV2.Types.DeploymentComponentUpdatePolicyAction
import Network.AWS.GreengrassV2.Types.DeploymentConfigurationValidationPolicy
import Network.AWS.GreengrassV2.Types.DeploymentFailureHandlingPolicy
import Network.AWS.GreengrassV2.Types.DeploymentHistoryFilter
import Network.AWS.GreengrassV2.Types.DeploymentIoTJobConfiguration
import Network.AWS.GreengrassV2.Types.DeploymentPolicies
import Network.AWS.GreengrassV2.Types.DeploymentStatus
import Network.AWS.GreengrassV2.Types.DisassociateClientDeviceFromCoreDeviceEntry
import Network.AWS.GreengrassV2.Types.DisassociateClientDeviceFromCoreDeviceErrorEntry
import Network.AWS.GreengrassV2.Types.EffectiveDeployment
import Network.AWS.GreengrassV2.Types.EffectiveDeploymentExecutionStatus
import Network.AWS.GreengrassV2.Types.InstalledComponent
import Network.AWS.GreengrassV2.Types.InstalledComponentLifecycleState
import Network.AWS.GreengrassV2.Types.IoTJobAbortAction
import Network.AWS.GreengrassV2.Types.IoTJobAbortConfig
import Network.AWS.GreengrassV2.Types.IoTJobAbortCriteria
import Network.AWS.GreengrassV2.Types.IoTJobExecutionFailureType
import Network.AWS.GreengrassV2.Types.IoTJobExecutionsRolloutConfig
import Network.AWS.GreengrassV2.Types.IoTJobExponentialRolloutRate
import Network.AWS.GreengrassV2.Types.IoTJobRateIncreaseCriteria
import Network.AWS.GreengrassV2.Types.IoTJobTimeoutConfig
import Network.AWS.GreengrassV2.Types.LambdaContainerParams
import Network.AWS.GreengrassV2.Types.LambdaDeviceMount
import Network.AWS.GreengrassV2.Types.LambdaEventSource
import Network.AWS.GreengrassV2.Types.LambdaEventSourceType
import Network.AWS.GreengrassV2.Types.LambdaExecutionParameters
import Network.AWS.GreengrassV2.Types.LambdaFilesystemPermission
import Network.AWS.GreengrassV2.Types.LambdaFunctionRecipeSource
import Network.AWS.GreengrassV2.Types.LambdaInputPayloadEncodingType
import Network.AWS.GreengrassV2.Types.LambdaIsolationMode
import Network.AWS.GreengrassV2.Types.LambdaLinuxProcessParams
import Network.AWS.GreengrassV2.Types.LambdaVolumeMount
import Network.AWS.GreengrassV2.Types.RecipeOutputFormat
import Network.AWS.GreengrassV2.Types.ResolvedComponentVersion
import Network.AWS.GreengrassV2.Types.SystemResourceLimits
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2020-11-30@ of the Amazon IoT Greengrass V2 SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "GreengrassV2",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "greengrass",
      Core._serviceSigningName = "greengrass",
      Core._serviceVersion = "2020-11-30",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "GreengrassV2",
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

-- | The request isn\'t valid. This can occur if your request contains
-- malformed JSON or unsupported characters.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | You don\'t have permission to perform the action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | Your request has conflicting operations. This can occur if you\'re
-- trying to perform more than one operation on the same resource at the
-- same time.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Your request exceeds a service quota. For example, you might have the
-- maximum number of components that you can create.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | Your request exceeded a request rate quota. For example, you might have
-- exceeded the amount of times that you can retrieve device or deployment
-- status per second.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The request is already in progress. This exception occurs when you use a
-- client token for multiple requests while IoT Greengrass is still
-- processing an earlier request that uses the same client token.
_RequestAlreadyInProgressException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RequestAlreadyInProgressException =
  Core._MatchServiceError
    defaultService
    "RequestAlreadyInProgressException"
    Prelude.. Core.hasStatus 400

-- | IoT Greengrass can\'t process your request right now. Try again later.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The requested resource can\'t be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
