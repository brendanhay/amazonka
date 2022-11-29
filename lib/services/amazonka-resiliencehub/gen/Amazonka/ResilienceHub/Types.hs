{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ResilienceHub.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InternalServerException,
    _ServiceQuotaExceededException,
    _ResourceNotFoundException,
    _ConflictException,
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

    -- * ResourceMappingType
    ResourceMappingType (..),

    -- * ResourceResolutionStatusType
    ResourceResolutionStatusType (..),

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
    alarmRecommendation_items,
    alarmRecommendation_prerequisite,
    alarmRecommendation_appComponentName,
    alarmRecommendation_description,
    alarmRecommendation_name,
    alarmRecommendation_recommendationId,
    alarmRecommendation_referenceId,
    alarmRecommendation_type,

    -- * App
    App (..),
    newApp,
    app_tags,
    app_resiliencyScore,
    app_complianceStatus,
    app_status,
    app_description,
    app_lastAppComplianceEvaluationTime,
    app_policyArn,
    app_assessmentSchedule,
    app_lastResiliencyScoreEvaluationTime,
    app_appArn,
    app_creationTime,
    app_name,

    -- * AppAssessment
    AppAssessment (..),
    newAppAssessment,
    appAssessment_tags,
    appAssessment_policy,
    appAssessment_message,
    appAssessment_assessmentName,
    appAssessment_resiliencyScore,
    appAssessment_complianceStatus,
    appAssessment_endTime,
    appAssessment_appVersion,
    appAssessment_appArn,
    appAssessment_cost,
    appAssessment_startTime,
    appAssessment_compliance,
    appAssessment_resourceErrorsDetails,
    appAssessment_assessmentArn,
    appAssessment_assessmentStatus,
    appAssessment_invoker,

    -- * AppAssessmentSummary
    AppAssessmentSummary (..),
    newAppAssessmentSummary,
    appAssessmentSummary_message,
    appAssessmentSummary_assessmentName,
    appAssessmentSummary_resiliencyScore,
    appAssessmentSummary_complianceStatus,
    appAssessmentSummary_invoker,
    appAssessmentSummary_endTime,
    appAssessmentSummary_appVersion,
    appAssessmentSummary_appArn,
    appAssessmentSummary_cost,
    appAssessmentSummary_startTime,
    appAssessmentSummary_assessmentArn,
    appAssessmentSummary_assessmentStatus,

    -- * AppComponent
    AppComponent (..),
    newAppComponent,
    appComponent_name,
    appComponent_type,

    -- * AppComponentCompliance
    AppComponentCompliance (..),
    newAppComponentCompliance,
    appComponentCompliance_message,
    appComponentCompliance_resiliencyScore,
    appComponentCompliance_appComponentName,
    appComponentCompliance_status,
    appComponentCompliance_cost,
    appComponentCompliance_compliance,

    -- * AppSummary
    AppSummary (..),
    newAppSummary,
    appSummary_resiliencyScore,
    appSummary_complianceStatus,
    appSummary_status,
    appSummary_description,
    appSummary_assessmentSchedule,
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
    configRecommendation_haArchitecture,
    configRecommendation_recommendationCompliance,
    configRecommendation_appComponentName,
    configRecommendation_description,
    configRecommendation_cost,
    configRecommendation_compliance,
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
    disruptionCompliance_rpoReferenceId,
    disruptionCompliance_message,
    disruptionCompliance_achievableRtoInSecs,
    disruptionCompliance_rtoDescription,
    disruptionCompliance_currentRtoInSecs,
    disruptionCompliance_rtoReferenceId,
    disruptionCompliance_achievableRpoInSecs,
    disruptionCompliance_rpoDescription,
    disruptionCompliance_currentRpoInSecs,
    disruptionCompliance_complianceStatus,

    -- * FailurePolicy
    FailurePolicy (..),
    newFailurePolicy,
    failurePolicy_rpoInSecs,
    failurePolicy_rtoInSecs,

    -- * LogicalResourceId
    LogicalResourceId (..),
    newLogicalResourceId,
    logicalResourceId_terraformSourceName,
    logicalResourceId_logicalStackName,
    logicalResourceId_resourceGroupName,
    logicalResourceId_identifier,

    -- * PhysicalResource
    PhysicalResource (..),
    newPhysicalResource,
    physicalResource_appComponents,
    physicalResource_resourceName,
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
    recommendationDisruptionCompliance_expectedRtoInSecs,
    recommendationDisruptionCompliance_expectedRtoDescription,
    recommendationDisruptionCompliance_expectedComplianceStatus,

    -- * RecommendationItem
    RecommendationItem (..),
    newRecommendationItem,
    recommendationItem_resourceId,
    recommendationItem_alreadyImplemented,
    recommendationItem_targetAccountId,
    recommendationItem_targetRegion,

    -- * RecommendationTemplate
    RecommendationTemplate (..),
    newRecommendationTemplate,
    recommendationTemplate_tags,
    recommendationTemplate_message,
    recommendationTemplate_needsReplacements,
    recommendationTemplate_endTime,
    recommendationTemplate_recommendationIds,
    recommendationTemplate_templatesLocation,
    recommendationTemplate_appArn,
    recommendationTemplate_startTime,
    recommendationTemplate_assessmentArn,
    recommendationTemplate_format,
    recommendationTemplate_name,
    recommendationTemplate_recommendationTemplateArn,
    recommendationTemplate_recommendationTypes,
    recommendationTemplate_status,

    -- * ResiliencyPolicy
    ResiliencyPolicy (..),
    newResiliencyPolicy,
    resiliencyPolicy_tags,
    resiliencyPolicy_policyName,
    resiliencyPolicy_policy,
    resiliencyPolicy_dataLocationConstraint,
    resiliencyPolicy_estimatedCostTier,
    resiliencyPolicy_tier,
    resiliencyPolicy_policyArn,
    resiliencyPolicy_creationTime,
    resiliencyPolicy_policyDescription,

    -- * ResiliencyScore
    ResiliencyScore (..),
    newResiliencyScore,
    resiliencyScore_disruptionScore,
    resiliencyScore_score,

    -- * ResourceError
    ResourceError (..),
    newResourceError,
    resourceError_logicalResourceId,
    resourceError_reason,
    resourceError_physicalResourceId,

    -- * ResourceErrorsDetails
    ResourceErrorsDetails (..),
    newResourceErrorsDetails,
    resourceErrorsDetails_hasMoreErrors,
    resourceErrorsDetails_resourceErrors,

    -- * ResourceMapping
    ResourceMapping (..),
    newResourceMapping,
    resourceMapping_terraformSourceName,
    resourceMapping_resourceName,
    resourceMapping_logicalStackName,
    resourceMapping_resourceGroupName,
    resourceMapping_appRegistryAppName,
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
    sopRecommendation_items,
    sopRecommendation_name,
    sopRecommendation_prerequisite,
    sopRecommendation_appComponentName,
    sopRecommendation_description,
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
    testRecommendation_items,
    testRecommendation_name,
    testRecommendation_type,
    testRecommendation_dependsOnAlarms,
    testRecommendation_prerequisite,
    testRecommendation_risk,
    testRecommendation_recommendationId,
    testRecommendation_appComponentName,
    testRecommendation_description,
    testRecommendation_intent,
    testRecommendation_referenceId,

    -- * UnsupportedResource
    UnsupportedResource (..),
    newUnsupportedResource,
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
import Amazonka.ResilienceHub.Types.ResourceMapping
import Amazonka.ResilienceHub.Types.ResourceMappingType
import Amazonka.ResilienceHub.Types.ResourceResolutionStatusType
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | You don\'t have permissions to perform the requested operation. The user
-- or role that is making the request must have at least one IAM
-- permissions policy attached that grants the required permissions.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | This exception occurs when there is an internal failure in the AWS
-- Resilience Hub service.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | You have exceeded your service quota. To perform the requested action,
-- remove some of the relevant resources, or use Service Quotas to request
-- a service quota increase.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The specified resource could not be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Occurs when a conflict with a previous successful write is detected.
-- This generally occurs when the previous write did not have time to
-- propagate to the host serving the current request. A retry (with
-- appropriate backoff logic) is the recommended response to this
-- exception.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The limit on the number of requests per second was exceeded.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | Indicates that a request was not valid.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
