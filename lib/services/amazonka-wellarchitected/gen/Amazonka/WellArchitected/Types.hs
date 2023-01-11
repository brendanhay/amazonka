{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WellArchitected.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types
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
    additionalResources_content,
    additionalResources_type,

    -- * Answer
    Answer (..),
    newAnswer,
    answer_choiceAnswers,
    answer_choices,
    answer_helpfulResourceDisplayText,
    answer_helpfulResourceUrl,
    answer_improvementPlanUrl,
    answer_isApplicable,
    answer_notes,
    answer_pillarId,
    answer_questionDescription,
    answer_questionId,
    answer_questionTitle,
    answer_reason,
    answer_risk,
    answer_selectedChoices,

    -- * AnswerSummary
    AnswerSummary (..),
    newAnswerSummary,
    answerSummary_choiceAnswerSummaries,
    answerSummary_choices,
    answerSummary_isApplicable,
    answerSummary_pillarId,
    answerSummary_questionId,
    answerSummary_questionTitle,
    answerSummary_reason,
    answerSummary_risk,
    answerSummary_selectedChoices,

    -- * CheckDetail
    CheckDetail (..),
    newCheckDetail,
    checkDetail_accountId,
    checkDetail_choiceId,
    checkDetail_description,
    checkDetail_flaggedResources,
    checkDetail_id,
    checkDetail_lensArn,
    checkDetail_name,
    checkDetail_pillarId,
    checkDetail_provider,
    checkDetail_questionId,
    checkDetail_reason,
    checkDetail_status,
    checkDetail_updatedAt,

    -- * CheckSummary
    CheckSummary (..),
    newCheckSummary,
    checkSummary_accountSummary,
    checkSummary_choiceId,
    checkSummary_description,
    checkSummary_id,
    checkSummary_lensArn,
    checkSummary_name,
    checkSummary_pillarId,
    checkSummary_provider,
    checkSummary_questionId,
    checkSummary_status,
    checkSummary_updatedAt,

    -- * Choice
    Choice (..),
    newChoice,
    choice_additionalResources,
    choice_choiceId,
    choice_description,
    choice_helpfulResource,
    choice_improvementPlan,
    choice_title,

    -- * ChoiceAnswer
    ChoiceAnswer (..),
    newChoiceAnswer,
    choiceAnswer_choiceId,
    choiceAnswer_notes,
    choiceAnswer_reason,
    choiceAnswer_status,

    -- * ChoiceAnswerSummary
    ChoiceAnswerSummary (..),
    newChoiceAnswerSummary,
    choiceAnswerSummary_choiceId,
    choiceAnswerSummary_reason,
    choiceAnswerSummary_status,

    -- * ChoiceContent
    ChoiceContent (..),
    newChoiceContent,
    choiceContent_displayText,
    choiceContent_url,

    -- * ChoiceImprovementPlan
    ChoiceImprovementPlan (..),
    newChoiceImprovementPlan,
    choiceImprovementPlan_choiceId,
    choiceImprovementPlan_displayText,
    choiceImprovementPlan_improvementPlanUrl,

    -- * ChoiceUpdate
    ChoiceUpdate (..),
    newChoiceUpdate,
    choiceUpdate_notes,
    choiceUpdate_reason,
    choiceUpdate_status,

    -- * ImprovementSummary
    ImprovementSummary (..),
    newImprovementSummary,
    improvementSummary_improvementPlanUrl,
    improvementSummary_improvementPlans,
    improvementSummary_pillarId,
    improvementSummary_questionId,
    improvementSummary_questionTitle,
    improvementSummary_risk,

    -- * Lens
    Lens (..),
    newLens,
    lens_description,
    lens_lensArn,
    lens_lensVersion,
    lens_name,
    lens_owner,
    lens_shareInvitationId,
    lens_tags,

    -- * LensReview
    LensReview (..),
    newLensReview,
    lensReview_lensAlias,
    lensReview_lensArn,
    lensReview_lensName,
    lensReview_lensStatus,
    lensReview_lensVersion,
    lensReview_nextToken,
    lensReview_notes,
    lensReview_pillarReviewSummaries,
    lensReview_riskCounts,
    lensReview_updatedAt,

    -- * LensReviewReport
    LensReviewReport (..),
    newLensReviewReport,
    lensReviewReport_base64String,
    lensReviewReport_lensAlias,
    lensReviewReport_lensArn,

    -- * LensReviewSummary
    LensReviewSummary (..),
    newLensReviewSummary,
    lensReviewSummary_lensAlias,
    lensReviewSummary_lensArn,
    lensReviewSummary_lensName,
    lensReviewSummary_lensStatus,
    lensReviewSummary_lensVersion,
    lensReviewSummary_riskCounts,
    lensReviewSummary_updatedAt,

    -- * LensShareSummary
    LensShareSummary (..),
    newLensShareSummary,
    lensShareSummary_shareId,
    lensShareSummary_sharedWith,
    lensShareSummary_status,
    lensShareSummary_statusMessage,

    -- * LensSummary
    LensSummary (..),
    newLensSummary,
    lensSummary_createdAt,
    lensSummary_description,
    lensSummary_lensAlias,
    lensSummary_lensArn,
    lensSummary_lensName,
    lensSummary_lensStatus,
    lensSummary_lensType,
    lensSummary_lensVersion,
    lensSummary_owner,
    lensSummary_updatedAt,

    -- * LensUpgradeSummary
    LensUpgradeSummary (..),
    newLensUpgradeSummary,
    lensUpgradeSummary_currentLensVersion,
    lensUpgradeSummary_latestLensVersion,
    lensUpgradeSummary_lensAlias,
    lensUpgradeSummary_lensArn,
    lensUpgradeSummary_workloadId,
    lensUpgradeSummary_workloadName,

    -- * Milestone
    Milestone (..),
    newMilestone,
    milestone_milestoneName,
    milestone_milestoneNumber,
    milestone_recordedAt,
    milestone_workload,

    -- * MilestoneSummary
    MilestoneSummary (..),
    newMilestoneSummary,
    milestoneSummary_milestoneName,
    milestoneSummary_milestoneNumber,
    milestoneSummary_recordedAt,
    milestoneSummary_workloadSummary,

    -- * NotificationSummary
    NotificationSummary (..),
    newNotificationSummary,
    notificationSummary_lensUpgradeSummary,
    notificationSummary_type,

    -- * PillarDifference
    PillarDifference (..),
    newPillarDifference,
    pillarDifference_differenceStatus,
    pillarDifference_pillarId,
    pillarDifference_pillarName,
    pillarDifference_questionDifferences,

    -- * PillarReviewSummary
    PillarReviewSummary (..),
    newPillarReviewSummary,
    pillarReviewSummary_notes,
    pillarReviewSummary_pillarId,
    pillarReviewSummary_pillarName,
    pillarReviewSummary_riskCounts,

    -- * QuestionDifference
    QuestionDifference (..),
    newQuestionDifference,
    questionDifference_differenceStatus,
    questionDifference_questionId,
    questionDifference_questionTitle,

    -- * ShareInvitation
    ShareInvitation (..),
    newShareInvitation,
    shareInvitation_lensAlias,
    shareInvitation_lensArn,
    shareInvitation_shareInvitationId,
    shareInvitation_shareResourceType,
    shareInvitation_workloadId,

    -- * ShareInvitationSummary
    ShareInvitationSummary (..),
    newShareInvitationSummary,
    shareInvitationSummary_lensArn,
    shareInvitationSummary_lensName,
    shareInvitationSummary_permissionType,
    shareInvitationSummary_shareInvitationId,
    shareInvitationSummary_shareResourceType,
    shareInvitationSummary_sharedBy,
    shareInvitationSummary_sharedWith,
    shareInvitationSummary_workloadId,
    shareInvitationSummary_workloadName,

    -- * VersionDifferences
    VersionDifferences (..),
    newVersionDifferences,
    versionDifferences_pillarDifferences,

    -- * Workload
    Workload (..),
    newWorkload,
    workload_accountIds,
    workload_applications,
    workload_architecturalDesign,
    workload_awsRegions,
    workload_description,
    workload_discoveryConfig,
    workload_environment,
    workload_improvementStatus,
    workload_industry,
    workload_industryType,
    workload_isReviewOwnerUpdateAcknowledged,
    workload_lenses,
    workload_nonAwsRegions,
    workload_notes,
    workload_owner,
    workload_pillarPriorities,
    workload_reviewOwner,
    workload_reviewRestrictionDate,
    workload_riskCounts,
    workload_shareInvitationId,
    workload_tags,
    workload_updatedAt,
    workload_workloadArn,
    workload_workloadId,
    workload_workloadName,

    -- * WorkloadDiscoveryConfig
    WorkloadDiscoveryConfig (..),
    newWorkloadDiscoveryConfig,
    workloadDiscoveryConfig_trustedAdvisorIntegrationStatus,

    -- * WorkloadShare
    WorkloadShare (..),
    newWorkloadShare,
    workloadShare_permissionType,
    workloadShare_shareId,
    workloadShare_sharedBy,
    workloadShare_sharedWith,
    workloadShare_status,
    workloadShare_workloadId,
    workloadShare_workloadName,

    -- * WorkloadShareSummary
    WorkloadShareSummary (..),
    newWorkloadShareSummary,
    workloadShareSummary_permissionType,
    workloadShareSummary_shareId,
    workloadShareSummary_sharedWith,
    workloadShareSummary_status,
    workloadShareSummary_statusMessage,

    -- * WorkloadSummary
    WorkloadSummary (..),
    newWorkloadSummary,
    workloadSummary_improvementStatus,
    workloadSummary_lenses,
    workloadSummary_owner,
    workloadSummary_riskCounts,
    workloadSummary_updatedAt,
    workloadSummary_workloadArn,
    workloadSummary_workloadId,
    workloadSummary_workloadName,
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

-- | User does not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The resource already exists.
_ConflictException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | There is a problem with the Well-Architected Tool API service.
_InternalServerException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The requested resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The user has reached their resource quota.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | Request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The user input is not valid.
_ValidationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
