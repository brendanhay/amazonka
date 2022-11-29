{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WellArchitected.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types
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

    -- * AdditionalResourceType
    AdditionalResourceType (..),

    -- * AnswerReason
    AnswerReason (..),

    -- * CheckFailureReason
    CheckFailureReason (..),

    -- * CheckProvider
    CheckProvider (..),

    -- * CheckStatus
    CheckStatus (..),

    -- * ChoiceReason
    ChoiceReason (..),

    -- * ChoiceStatus
    ChoiceStatus (..),

    -- * DifferenceStatus
    DifferenceStatus (..),

    -- * ImportLensStatus
    ImportLensStatus (..),

    -- * LensStatus
    LensStatus (..),

    -- * LensStatusType
    LensStatusType (..),

    -- * LensType
    LensType (..),

    -- * NotificationType
    NotificationType (..),

    -- * OrganizationSharingStatus
    OrganizationSharingStatus (..),

    -- * PermissionType
    PermissionType (..),

    -- * Risk
    Risk (..),

    -- * ShareInvitationAction
    ShareInvitationAction (..),

    -- * ShareResourceType
    ShareResourceType (..),

    -- * ShareStatus
    ShareStatus (..),

    -- * TrustedAdvisorIntegrationStatus
    TrustedAdvisorIntegrationStatus (..),

    -- * WorkloadEnvironment
    WorkloadEnvironment (..),

    -- * WorkloadImprovementStatus
    WorkloadImprovementStatus (..),

    -- * AdditionalResources
    AdditionalResources (..),
    newAdditionalResources,
    additionalResources_type,
    additionalResources_content,

    -- * Answer
    Answer (..),
    newAnswer,
    answer_choices,
    answer_selectedChoices,
    answer_helpfulResourceDisplayText,
    answer_risk,
    answer_questionId,
    answer_questionDescription,
    answer_improvementPlanUrl,
    answer_isApplicable,
    answer_reason,
    answer_notes,
    answer_questionTitle,
    answer_helpfulResourceUrl,
    answer_pillarId,
    answer_choiceAnswers,

    -- * AnswerSummary
    AnswerSummary (..),
    newAnswerSummary,
    answerSummary_choices,
    answerSummary_selectedChoices,
    answerSummary_risk,
    answerSummary_choiceAnswerSummaries,
    answerSummary_questionId,
    answerSummary_isApplicable,
    answerSummary_reason,
    answerSummary_questionTitle,
    answerSummary_pillarId,

    -- * CheckDetail
    CheckDetail (..),
    newCheckDetail,
    checkDetail_name,
    checkDetail_lensArn,
    checkDetail_provider,
    checkDetail_questionId,
    checkDetail_status,
    checkDetail_id,
    checkDetail_description,
    checkDetail_choiceId,
    checkDetail_accountId,
    checkDetail_reason,
    checkDetail_flaggedResources,
    checkDetail_pillarId,
    checkDetail_updatedAt,

    -- * CheckSummary
    CheckSummary (..),
    newCheckSummary,
    checkSummary_name,
    checkSummary_lensArn,
    checkSummary_accountSummary,
    checkSummary_provider,
    checkSummary_questionId,
    checkSummary_status,
    checkSummary_id,
    checkSummary_description,
    checkSummary_choiceId,
    checkSummary_pillarId,
    checkSummary_updatedAt,

    -- * Choice
    Choice (..),
    newChoice,
    choice_description,
    choice_choiceId,
    choice_title,
    choice_helpfulResource,
    choice_additionalResources,
    choice_improvementPlan,

    -- * ChoiceAnswer
    ChoiceAnswer (..),
    newChoiceAnswer,
    choiceAnswer_status,
    choiceAnswer_choiceId,
    choiceAnswer_reason,
    choiceAnswer_notes,

    -- * ChoiceAnswerSummary
    ChoiceAnswerSummary (..),
    newChoiceAnswerSummary,
    choiceAnswerSummary_status,
    choiceAnswerSummary_choiceId,
    choiceAnswerSummary_reason,

    -- * ChoiceContent
    ChoiceContent (..),
    newChoiceContent,
    choiceContent_displayText,
    choiceContent_url,

    -- * ChoiceImprovementPlan
    ChoiceImprovementPlan (..),
    newChoiceImprovementPlan,
    choiceImprovementPlan_displayText,
    choiceImprovementPlan_improvementPlanUrl,
    choiceImprovementPlan_choiceId,

    -- * ChoiceUpdate
    ChoiceUpdate (..),
    newChoiceUpdate,
    choiceUpdate_reason,
    choiceUpdate_notes,
    choiceUpdate_status,

    -- * ImprovementSummary
    ImprovementSummary (..),
    newImprovementSummary,
    improvementSummary_risk,
    improvementSummary_questionId,
    improvementSummary_improvementPlanUrl,
    improvementSummary_improvementPlans,
    improvementSummary_questionTitle,
    improvementSummary_pillarId,

    -- * Lens
    Lens (..),
    newLens,
    lens_tags,
    lens_name,
    lens_lensArn,
    lens_shareInvitationId,
    lens_owner,
    lens_lensVersion,
    lens_description,

    -- * LensReview
    LensReview (..),
    newLensReview,
    lensReview_nextToken,
    lensReview_lensArn,
    lensReview_riskCounts,
    lensReview_lensAlias,
    lensReview_lensVersion,
    lensReview_lensName,
    lensReview_pillarReviewSummaries,
    lensReview_notes,
    lensReview_lensStatus,
    lensReview_updatedAt,

    -- * LensReviewReport
    LensReviewReport (..),
    newLensReviewReport,
    lensReviewReport_lensArn,
    lensReviewReport_lensAlias,
    lensReviewReport_base64String,

    -- * LensReviewSummary
    LensReviewSummary (..),
    newLensReviewSummary,
    lensReviewSummary_lensArn,
    lensReviewSummary_riskCounts,
    lensReviewSummary_lensAlias,
    lensReviewSummary_lensVersion,
    lensReviewSummary_lensName,
    lensReviewSummary_lensStatus,
    lensReviewSummary_updatedAt,

    -- * LensShareSummary
    LensShareSummary (..),
    newLensShareSummary,
    lensShareSummary_sharedWith,
    lensShareSummary_status,
    lensShareSummary_shareId,
    lensShareSummary_statusMessage,

    -- * LensSummary
    LensSummary (..),
    newLensSummary,
    lensSummary_lensArn,
    lensSummary_lensAlias,
    lensSummary_owner,
    lensSummary_lensVersion,
    lensSummary_description,
    lensSummary_lensName,
    lensSummary_lensType,
    lensSummary_lensStatus,
    lensSummary_createdAt,
    lensSummary_updatedAt,

    -- * LensUpgradeSummary
    LensUpgradeSummary (..),
    newLensUpgradeSummary,
    lensUpgradeSummary_currentLensVersion,
    lensUpgradeSummary_lensArn,
    lensUpgradeSummary_lensAlias,
    lensUpgradeSummary_workloadName,
    lensUpgradeSummary_latestLensVersion,
    lensUpgradeSummary_workloadId,

    -- * Milestone
    Milestone (..),
    newMilestone,
    milestone_recordedAt,
    milestone_milestoneName,
    milestone_milestoneNumber,
    milestone_workload,

    -- * MilestoneSummary
    MilestoneSummary (..),
    newMilestoneSummary,
    milestoneSummary_recordedAt,
    milestoneSummary_milestoneName,
    milestoneSummary_workloadSummary,
    milestoneSummary_milestoneNumber,

    -- * NotificationSummary
    NotificationSummary (..),
    newNotificationSummary,
    notificationSummary_type,
    notificationSummary_lensUpgradeSummary,

    -- * PillarDifference
    PillarDifference (..),
    newPillarDifference,
    pillarDifference_differenceStatus,
    pillarDifference_questionDifferences,
    pillarDifference_pillarId,
    pillarDifference_pillarName,

    -- * PillarReviewSummary
    PillarReviewSummary (..),
    newPillarReviewSummary,
    pillarReviewSummary_riskCounts,
    pillarReviewSummary_notes,
    pillarReviewSummary_pillarId,
    pillarReviewSummary_pillarName,

    -- * QuestionDifference
    QuestionDifference (..),
    newQuestionDifference,
    questionDifference_questionId,
    questionDifference_differenceStatus,
    questionDifference_questionTitle,

    -- * ShareInvitation
    ShareInvitation (..),
    newShareInvitation,
    shareInvitation_lensArn,
    shareInvitation_lensAlias,
    shareInvitation_shareInvitationId,
    shareInvitation_shareResourceType,
    shareInvitation_workloadId,

    -- * ShareInvitationSummary
    ShareInvitationSummary (..),
    newShareInvitationSummary,
    shareInvitationSummary_permissionType,
    shareInvitationSummary_lensArn,
    shareInvitationSummary_sharedWith,
    shareInvitationSummary_shareInvitationId,
    shareInvitationSummary_workloadName,
    shareInvitationSummary_lensName,
    shareInvitationSummary_sharedBy,
    shareInvitationSummary_shareResourceType,
    shareInvitationSummary_workloadId,

    -- * VersionDifferences
    VersionDifferences (..),
    newVersionDifferences,
    versionDifferences_pillarDifferences,

    -- * Workload
    Workload (..),
    newWorkload,
    workload_discoveryConfig,
    workload_tags,
    workload_accountIds,
    workload_environment,
    workload_riskCounts,
    workload_isReviewOwnerUpdateAcknowledged,
    workload_industry,
    workload_applications,
    workload_shareInvitationId,
    workload_workloadArn,
    workload_awsRegions,
    workload_workloadName,
    workload_reviewOwner,
    workload_owner,
    workload_nonAwsRegions,
    workload_description,
    workload_notes,
    workload_industryType,
    workload_architecturalDesign,
    workload_pillarPriorities,
    workload_improvementStatus,
    workload_lenses,
    workload_reviewRestrictionDate,
    workload_updatedAt,
    workload_workloadId,

    -- * WorkloadDiscoveryConfig
    WorkloadDiscoveryConfig (..),
    newWorkloadDiscoveryConfig,
    workloadDiscoveryConfig_trustedAdvisorIntegrationStatus,

    -- * WorkloadShare
    WorkloadShare (..),
    newWorkloadShare,
    workloadShare_permissionType,
    workloadShare_sharedWith,
    workloadShare_workloadName,
    workloadShare_status,
    workloadShare_shareId,
    workloadShare_sharedBy,
    workloadShare_workloadId,

    -- * WorkloadShareSummary
    WorkloadShareSummary (..),
    newWorkloadShareSummary,
    workloadShareSummary_permissionType,
    workloadShareSummary_sharedWith,
    workloadShareSummary_status,
    workloadShareSummary_shareId,
    workloadShareSummary_statusMessage,

    -- * WorkloadSummary
    WorkloadSummary (..),
    newWorkloadSummary,
    workloadSummary_riskCounts,
    workloadSummary_workloadArn,
    workloadSummary_workloadName,
    workloadSummary_owner,
    workloadSummary_improvementStatus,
    workloadSummary_lenses,
    workloadSummary_updatedAt,
    workloadSummary_workloadId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.WellArchitected.Types.AdditionalResourceType
import Amazonka.WellArchitected.Types.AdditionalResources
import Amazonka.WellArchitected.Types.Answer
import Amazonka.WellArchitected.Types.AnswerReason
import Amazonka.WellArchitected.Types.AnswerSummary
import Amazonka.WellArchitected.Types.CheckDetail
import Amazonka.WellArchitected.Types.CheckFailureReason
import Amazonka.WellArchitected.Types.CheckProvider
import Amazonka.WellArchitected.Types.CheckStatus
import Amazonka.WellArchitected.Types.CheckSummary
import Amazonka.WellArchitected.Types.Choice
import Amazonka.WellArchitected.Types.ChoiceAnswer
import Amazonka.WellArchitected.Types.ChoiceAnswerSummary
import Amazonka.WellArchitected.Types.ChoiceContent
import Amazonka.WellArchitected.Types.ChoiceImprovementPlan
import Amazonka.WellArchitected.Types.ChoiceReason
import Amazonka.WellArchitected.Types.ChoiceStatus
import Amazonka.WellArchitected.Types.ChoiceUpdate
import Amazonka.WellArchitected.Types.DifferenceStatus
import Amazonka.WellArchitected.Types.ImportLensStatus
import Amazonka.WellArchitected.Types.ImprovementSummary
import Amazonka.WellArchitected.Types.Lens
import Amazonka.WellArchitected.Types.LensReview
import Amazonka.WellArchitected.Types.LensReviewReport
import Amazonka.WellArchitected.Types.LensReviewSummary
import Amazonka.WellArchitected.Types.LensShareSummary
import Amazonka.WellArchitected.Types.LensStatus
import Amazonka.WellArchitected.Types.LensStatusType
import Amazonka.WellArchitected.Types.LensSummary
import Amazonka.WellArchitected.Types.LensType
import Amazonka.WellArchitected.Types.LensUpgradeSummary
import Amazonka.WellArchitected.Types.Milestone
import Amazonka.WellArchitected.Types.MilestoneSummary
import Amazonka.WellArchitected.Types.NotificationSummary
import Amazonka.WellArchitected.Types.NotificationType
import Amazonka.WellArchitected.Types.OrganizationSharingStatus
import Amazonka.WellArchitected.Types.PermissionType
import Amazonka.WellArchitected.Types.PillarDifference
import Amazonka.WellArchitected.Types.PillarReviewSummary
import Amazonka.WellArchitected.Types.QuestionDifference
import Amazonka.WellArchitected.Types.Risk
import Amazonka.WellArchitected.Types.ShareInvitation
import Amazonka.WellArchitected.Types.ShareInvitationAction
import Amazonka.WellArchitected.Types.ShareInvitationSummary
import Amazonka.WellArchitected.Types.ShareResourceType
import Amazonka.WellArchitected.Types.ShareStatus
import Amazonka.WellArchitected.Types.TrustedAdvisorIntegrationStatus
import Amazonka.WellArchitected.Types.VersionDifferences
import Amazonka.WellArchitected.Types.Workload
import Amazonka.WellArchitected.Types.WorkloadDiscoveryConfig
import Amazonka.WellArchitected.Types.WorkloadEnvironment
import Amazonka.WellArchitected.Types.WorkloadImprovementStatus
import Amazonka.WellArchitected.Types.WorkloadShare
import Amazonka.WellArchitected.Types.WorkloadShareSummary
import Amazonka.WellArchitected.Types.WorkloadSummary

-- | API version @2020-03-31@ of the Amazon Well-Architected Tool SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "WellArchitected",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "wellarchitected",
      Core.signingName = "wellarchitected",
      Core.version = "2020-03-31",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "WellArchitected",
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

-- | User does not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | There is a problem with the Well-Architected Tool API service.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The user has reached their resource quota.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The requested resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The resource already exists.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The user input is not valid.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
