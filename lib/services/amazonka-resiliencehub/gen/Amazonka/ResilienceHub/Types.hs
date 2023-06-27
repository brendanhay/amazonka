{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ResilienceHub.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * AlarmType
    AlarmType (..),

    -- * AppAssessmentScheduleType
    AppAssessmentScheduleType (..),

    -- * AppComplianceStatusType
    AppComplianceStatusType (..),

    -- * AppStatusType
    AppStatusType (..),

    -- * AssessmentInvoker
    AssessmentInvoker (..),

    -- * AssessmentStatus
    AssessmentStatus (..),

    -- * ComplianceStatus
    ComplianceStatus (..),

    -- * ConfigRecommendationOptimizationType
    ConfigRecommendationOptimizationType (..),

    -- * CostFrequency
    CostFrequency (..),

    -- * DataLocationConstraint
    DataLocationConstraint (..),

    -- * DisruptionType
    DisruptionType (..),

    -- * EstimatedCostTier
    EstimatedCostTier (..),

    -- * HaArchitecture
    HaArchitecture (..),

    -- * PhysicalIdentifierType
    PhysicalIdentifierType (..),

    -- * RecommendationComplianceStatus
    RecommendationComplianceStatus (..),

    -- * RecommendationTemplateStatus
    RecommendationTemplateStatus (..),

    -- * RenderRecommendationType
    RenderRecommendationType (..),

    -- * ResiliencyPolicyTier
    ResiliencyPolicyTier (..),

    -- * ResourceImportStatusType
    ResourceImportStatusType (..),

    -- * ResourceImportStrategyType
    ResourceImportStrategyType (..),

    -- * ResourceMappingType
    ResourceMappingType (..),

    -- * ResourceResolutionStatusType
    ResourceResolutionStatusType (..),

    -- * ResourceSourceType
    ResourceSourceType (..),

    -- * SopServiceType
    SopServiceType (..),

    -- * TemplateFormat
    TemplateFormat (..),

    -- * TestRisk
    TestRisk (..),

    -- * TestType
    TestType (..),

    -- * AlarmRecommendation
    AlarmRecommendation (..),
    newAlarmRecommendation,
    alarmRecommendation_appComponentName,
    alarmRecommendation_description,
    alarmRecommendation_items,
    alarmRecommendation_prerequisite,
    alarmRecommendation_name,
    alarmRecommendation_recommendationId,
    alarmRecommendation_referenceId,
    alarmRecommendation_type,

    -- * App
    App (..),
    newApp,
    app_assessmentSchedule,
    app_complianceStatus,
    app_description,
    app_lastAppComplianceEvaluationTime,
    app_lastResiliencyScoreEvaluationTime,
    app_policyArn,
    app_resiliencyScore,
    app_status,
    app_tags,
    app_appArn,
    app_creationTime,
    app_name,

    -- * AppAssessment
    AppAssessment (..),
    newAppAssessment,
    appAssessment_appArn,
    appAssessment_appVersion,
    appAssessment_assessmentName,
    appAssessment_compliance,
    appAssessment_complianceStatus,
    appAssessment_cost,
    appAssessment_endTime,
    appAssessment_message,
    appAssessment_policy,
    appAssessment_resiliencyScore,
    appAssessment_resourceErrorsDetails,
    appAssessment_startTime,
    appAssessment_tags,
    appAssessment_assessmentArn,
    appAssessment_assessmentStatus,
    appAssessment_invoker,

    -- * AppAssessmentSummary
    AppAssessmentSummary (..),
    newAppAssessmentSummary,
    appAssessmentSummary_appArn,
    appAssessmentSummary_appVersion,
    appAssessmentSummary_assessmentName,
    appAssessmentSummary_complianceStatus,
    appAssessmentSummary_cost,
    appAssessmentSummary_endTime,
    appAssessmentSummary_invoker,
    appAssessmentSummary_message,
    appAssessmentSummary_resiliencyScore,
    appAssessmentSummary_startTime,
    appAssessmentSummary_assessmentArn,
    appAssessmentSummary_assessmentStatus,

    -- * AppComponent
    AppComponent (..),
    newAppComponent,
    appComponent_additionalInfo,
    appComponent_id,
    appComponent_name,
    appComponent_type,

    -- * AppComponentCompliance
    AppComponentCompliance (..),
    newAppComponentCompliance,
    appComponentCompliance_appComponentName,
    appComponentCompliance_compliance,
    appComponentCompliance_cost,
    appComponentCompliance_message,
    appComponentCompliance_resiliencyScore,
    appComponentCompliance_status,

    -- * AppInputSource
    AppInputSource (..),
    newAppInputSource,
    appInputSource_eksSourceClusterNamespace,
    appInputSource_resourceCount,
    appInputSource_sourceArn,
    appInputSource_sourceName,
    appInputSource_terraformSource,
    appInputSource_importType,

    -- * AppSummary
    AppSummary (..),
    newAppSummary,
    appSummary_assessmentSchedule,
    appSummary_complianceStatus,
    appSummary_description,
    appSummary_resiliencyScore,
    appSummary_status,
    appSummary_appArn,
    appSummary_creationTime,
    appSummary_name,

    -- * AppVersionSummary
    AppVersionSummary (..),
    newAppVersionSummary,
    appVersionSummary_appVersion,

    -- * ComponentRecommendation
    ComponentRecommendation (..),
    newComponentRecommendation,
    componentRecommendation_appComponentName,
    componentRecommendation_configRecommendations,
    componentRecommendation_recommendationStatus,

    -- * ConfigRecommendation
    ConfigRecommendation (..),
    newConfigRecommendation,
    configRecommendation_appComponentName,
    configRecommendation_compliance,
    configRecommendation_cost,
    configRecommendation_description,
    configRecommendation_haArchitecture,
    configRecommendation_recommendationCompliance,
    configRecommendation_suggestedChanges,
    configRecommendation_name,
    configRecommendation_optimizationType,
    configRecommendation_referenceId,

    -- * Cost
    Cost (..),
    newCost,
    cost_amount,
    cost_currency,
    cost_frequency,

    -- * DisruptionCompliance
    DisruptionCompliance (..),
    newDisruptionCompliance,
    disruptionCompliance_achievableRpoInSecs,
    disruptionCompliance_achievableRtoInSecs,
    disruptionCompliance_currentRpoInSecs,
    disruptionCompliance_currentRtoInSecs,
    disruptionCompliance_message,
    disruptionCompliance_rpoDescription,
    disruptionCompliance_rpoReferenceId,
    disruptionCompliance_rtoDescription,
    disruptionCompliance_rtoReferenceId,
    disruptionCompliance_complianceStatus,

    -- * EksSource
    EksSource (..),
    newEksSource,
    eksSource_eksClusterArn,
    eksSource_namespaces,

    -- * EksSourceClusterNamespace
    EksSourceClusterNamespace (..),
    newEksSourceClusterNamespace,
    eksSourceClusterNamespace_eksClusterArn,
    eksSourceClusterNamespace_namespace,

    -- * FailurePolicy
    FailurePolicy (..),
    newFailurePolicy,
    failurePolicy_rpoInSecs,
    failurePolicy_rtoInSecs,

    -- * LogicalResourceId
    LogicalResourceId (..),
    newLogicalResourceId,
    logicalResourceId_eksSourceName,
    logicalResourceId_logicalStackName,
    logicalResourceId_resourceGroupName,
    logicalResourceId_terraformSourceName,
    logicalResourceId_identifier,

    -- * PhysicalResource
    PhysicalResource (..),
    newPhysicalResource,
    physicalResource_additionalInfo,
    physicalResource_appComponents,
    physicalResource_excluded,
    physicalResource_parentResourceName,
    physicalResource_resourceName,
    physicalResource_sourceType,
    physicalResource_logicalResourceId,
    physicalResource_physicalResourceId,
    physicalResource_resourceType,

    -- * PhysicalResourceId
    PhysicalResourceId (..),
    newPhysicalResourceId,
    physicalResourceId_awsAccountId,
    physicalResourceId_awsRegion,
    physicalResourceId_identifier,
    physicalResourceId_type,

    -- * RecommendationDisruptionCompliance
    RecommendationDisruptionCompliance (..),
    newRecommendationDisruptionCompliance,
    recommendationDisruptionCompliance_expectedRpoDescription,
    recommendationDisruptionCompliance_expectedRpoInSecs,
    recommendationDisruptionCompliance_expectedRtoDescription,
    recommendationDisruptionCompliance_expectedRtoInSecs,
    recommendationDisruptionCompliance_expectedComplianceStatus,

    -- * RecommendationItem
    RecommendationItem (..),
    newRecommendationItem,
    recommendationItem_alreadyImplemented,
    recommendationItem_resourceId,
    recommendationItem_targetAccountId,
    recommendationItem_targetRegion,

    -- * RecommendationTemplate
    RecommendationTemplate (..),
    newRecommendationTemplate,
    recommendationTemplate_appArn,
    recommendationTemplate_endTime,
    recommendationTemplate_message,
    recommendationTemplate_needsReplacements,
    recommendationTemplate_recommendationIds,
    recommendationTemplate_startTime,
    recommendationTemplate_tags,
    recommendationTemplate_templatesLocation,
    recommendationTemplate_assessmentArn,
    recommendationTemplate_format,
    recommendationTemplate_name,
    recommendationTemplate_recommendationTemplateArn,
    recommendationTemplate_recommendationTypes,
    recommendationTemplate_status,

    -- * ResiliencyPolicy
    ResiliencyPolicy (..),
    newResiliencyPolicy,
    resiliencyPolicy_creationTime,
    resiliencyPolicy_dataLocationConstraint,
    resiliencyPolicy_estimatedCostTier,
    resiliencyPolicy_policy,
    resiliencyPolicy_policyArn,
    resiliencyPolicy_policyDescription,
    resiliencyPolicy_policyName,
    resiliencyPolicy_tags,
    resiliencyPolicy_tier,

    -- * ResiliencyScore
    ResiliencyScore (..),
    newResiliencyScore,
    resiliencyScore_disruptionScore,
    resiliencyScore_score,

    -- * ResourceError
    ResourceError (..),
    newResourceError,
    resourceError_logicalResourceId,
    resourceError_physicalResourceId,
    resourceError_reason,

    -- * ResourceErrorsDetails
    ResourceErrorsDetails (..),
    newResourceErrorsDetails,
    resourceErrorsDetails_hasMoreErrors,
    resourceErrorsDetails_resourceErrors,

    -- * ResourceMapping
    ResourceMapping (..),
    newResourceMapping,
    resourceMapping_appRegistryAppName,
    resourceMapping_eksSourceName,
    resourceMapping_logicalStackName,
    resourceMapping_resourceGroupName,
    resourceMapping_resourceName,
    resourceMapping_terraformSourceName,
    resourceMapping_mappingType,
    resourceMapping_physicalResourceId,

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_bucket,
    s3Location_prefix,

    -- * SopRecommendation
    SopRecommendation (..),
    newSopRecommendation,
    sopRecommendation_appComponentName,
    sopRecommendation_description,
    sopRecommendation_items,
    sopRecommendation_name,
    sopRecommendation_prerequisite,
    sopRecommendation_recommendationId,
    sopRecommendation_referenceId,
    sopRecommendation_serviceType,

    -- * TerraformSource
    TerraformSource (..),
    newTerraformSource,
    terraformSource_s3StateFileUrl,

    -- * TestRecommendation
    TestRecommendation (..),
    newTestRecommendation,
    testRecommendation_appComponentName,
    testRecommendation_dependsOnAlarms,
    testRecommendation_description,
    testRecommendation_intent,
    testRecommendation_items,
    testRecommendation_name,
    testRecommendation_prerequisite,
    testRecommendation_recommendationId,
    testRecommendation_risk,
    testRecommendation_type,
    testRecommendation_referenceId,

    -- * UnsupportedResource
    UnsupportedResource (..),
    newUnsupportedResource,
    unsupportedResource_unsupportedResourceStatus,
    unsupportedResource_logicalResourceId,
    unsupportedResource_physicalResourceId,
    unsupportedResource_resourceType,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResilienceHub.Types.AlarmRecommendation
import Amazonka.ResilienceHub.Types.AlarmType
import Amazonka.ResilienceHub.Types.App
import Amazonka.ResilienceHub.Types.AppAssessment
import Amazonka.ResilienceHub.Types.AppAssessmentScheduleType
import Amazonka.ResilienceHub.Types.AppAssessmentSummary
import Amazonka.ResilienceHub.Types.AppComplianceStatusType
import Amazonka.ResilienceHub.Types.AppComponent
import Amazonka.ResilienceHub.Types.AppComponentCompliance
import Amazonka.ResilienceHub.Types.AppInputSource
import Amazonka.ResilienceHub.Types.AppStatusType
import Amazonka.ResilienceHub.Types.AppSummary
import Amazonka.ResilienceHub.Types.AppVersionSummary
import Amazonka.ResilienceHub.Types.AssessmentInvoker
import Amazonka.ResilienceHub.Types.AssessmentStatus
import Amazonka.ResilienceHub.Types.ComplianceStatus
import Amazonka.ResilienceHub.Types.ComponentRecommendation
import Amazonka.ResilienceHub.Types.ConfigRecommendation
import Amazonka.ResilienceHub.Types.ConfigRecommendationOptimizationType
import Amazonka.ResilienceHub.Types.Cost
import Amazonka.ResilienceHub.Types.CostFrequency
import Amazonka.ResilienceHub.Types.DataLocationConstraint
import Amazonka.ResilienceHub.Types.DisruptionCompliance
import Amazonka.ResilienceHub.Types.DisruptionType
import Amazonka.ResilienceHub.Types.EksSource
import Amazonka.ResilienceHub.Types.EksSourceClusterNamespace
import Amazonka.ResilienceHub.Types.EstimatedCostTier
import Amazonka.ResilienceHub.Types.FailurePolicy
import Amazonka.ResilienceHub.Types.HaArchitecture
import Amazonka.ResilienceHub.Types.LogicalResourceId
import Amazonka.ResilienceHub.Types.PhysicalIdentifierType
import Amazonka.ResilienceHub.Types.PhysicalResource
import Amazonka.ResilienceHub.Types.PhysicalResourceId
import Amazonka.ResilienceHub.Types.RecommendationComplianceStatus
import Amazonka.ResilienceHub.Types.RecommendationDisruptionCompliance
import Amazonka.ResilienceHub.Types.RecommendationItem
import Amazonka.ResilienceHub.Types.RecommendationTemplate
import Amazonka.ResilienceHub.Types.RecommendationTemplateStatus
import Amazonka.ResilienceHub.Types.RenderRecommendationType
import Amazonka.ResilienceHub.Types.ResiliencyPolicy
import Amazonka.ResilienceHub.Types.ResiliencyPolicyTier
import Amazonka.ResilienceHub.Types.ResiliencyScore
import Amazonka.ResilienceHub.Types.ResourceError
import Amazonka.ResilienceHub.Types.ResourceErrorsDetails
import Amazonka.ResilienceHub.Types.ResourceImportStatusType
import Amazonka.ResilienceHub.Types.ResourceImportStrategyType
import Amazonka.ResilienceHub.Types.ResourceMapping
import Amazonka.ResilienceHub.Types.ResourceMappingType
import Amazonka.ResilienceHub.Types.ResourceResolutionStatusType
import Amazonka.ResilienceHub.Types.ResourceSourceType
import Amazonka.ResilienceHub.Types.S3Location
import Amazonka.ResilienceHub.Types.SopRecommendation
import Amazonka.ResilienceHub.Types.SopServiceType
import Amazonka.ResilienceHub.Types.TemplateFormat
import Amazonka.ResilienceHub.Types.TerraformSource
import Amazonka.ResilienceHub.Types.TestRecommendation
import Amazonka.ResilienceHub.Types.TestRisk
import Amazonka.ResilienceHub.Types.TestType
import Amazonka.ResilienceHub.Types.UnsupportedResource
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-04-30@ of the Amazon Resilience Hub SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "ResilienceHub",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "resiliencehub",
      Core.signingName = "resiliencehub",
      Core.version = "2020-04-30",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "ResilienceHub",
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

-- | You don\'t have permissions to perform the requested operation. The user
-- or role that is making the request must have at least one IAM
-- permissions policy attached that grants the required permissions.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | This exception occurs when a conflict with a previous successful write
-- is detected. This generally occurs when the previous write did not have
-- time to propagate to the host serving the current request. A retry (with
-- appropriate backoff logic) is the recommended response to this
-- exception.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | This exception occurs when there is an internal failure in the
-- Resilience Hub service.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | This exception occurs when the specified resource could not be found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | This exception occurs when you have exceeded your service quota. To
-- perform the requested action, remove some of the relevant resources, or
-- use Service Quotas to request a service quota increase.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | This exception occurs when you have exceeded the limit on the number of
-- requests per second.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | This exception occurs when a request is not valid.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
