{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WellArchitected.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _ConflictException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _InternalServerException,
    _ResourceNotFoundException,

    -- * AnswerReason
    AnswerReason (..),

    -- * ChoiceReason
    ChoiceReason (..),

    -- * ChoiceStatus
    ChoiceStatus (..),

    -- * DifferenceStatus
    DifferenceStatus (..),

    -- * LensStatus
    LensStatus (..),

    -- * NotificationType
    NotificationType (..),

    -- * PermissionType
    PermissionType (..),

    -- * Risk
    Risk (..),

    -- * ShareInvitationAction
    ShareInvitationAction (..),

    -- * ShareStatus
    ShareStatus (..),

    -- * WorkloadEnvironment
    WorkloadEnvironment (..),

    -- * WorkloadImprovementStatus
    WorkloadImprovementStatus (..),

    -- * Answer
    Answer (..),
    newAnswer,
    answer_choiceAnswers,
    answer_helpfulResourceUrl,
    answer_isApplicable,
    answer_pillarId,
    answer_improvementPlanUrl,
    answer_questionDescription,
    answer_risk,
    answer_questionTitle,
    answer_selectedChoices,
    answer_reason,
    answer_choices,
    answer_questionId,
    answer_notes,

    -- * AnswerSummary
    AnswerSummary (..),
    newAnswerSummary,
    answerSummary_isApplicable,
    answerSummary_pillarId,
    answerSummary_choiceAnswerSummaries,
    answerSummary_risk,
    answerSummary_questionTitle,
    answerSummary_selectedChoices,
    answerSummary_reason,
    answerSummary_choices,
    answerSummary_questionId,

    -- * Choice
    Choice (..),
    newChoice,
    choice_title,
    choice_description,
    choice_choiceId,

    -- * ChoiceAnswer
    ChoiceAnswer (..),
    newChoiceAnswer,
    choiceAnswer_status,
    choiceAnswer_reason,
    choiceAnswer_notes,
    choiceAnswer_choiceId,

    -- * ChoiceAnswerSummary
    ChoiceAnswerSummary (..),
    newChoiceAnswerSummary,
    choiceAnswerSummary_status,
    choiceAnswerSummary_reason,
    choiceAnswerSummary_choiceId,

    -- * ChoiceUpdate
    ChoiceUpdate (..),
    newChoiceUpdate,
    choiceUpdate_reason,
    choiceUpdate_notes,
    choiceUpdate_status,

    -- * ImprovementSummary
    ImprovementSummary (..),
    newImprovementSummary,
    improvementSummary_pillarId,
    improvementSummary_improvementPlanUrl,
    improvementSummary_risk,
    improvementSummary_questionTitle,
    improvementSummary_questionId,

    -- * LensReview
    LensReview (..),
    newLensReview,
    lensReview_lensAlias,
    lensReview_riskCounts,
    lensReview_lensName,
    lensReview_nextToken,
    lensReview_pillarReviewSummaries,
    lensReview_updatedAt,
    lensReview_lensStatus,
    lensReview_notes,
    lensReview_lensVersion,

    -- * LensReviewReport
    LensReviewReport (..),
    newLensReviewReport,
    lensReviewReport_lensAlias,
    lensReviewReport_base64String,

    -- * LensReviewSummary
    LensReviewSummary (..),
    newLensReviewSummary,
    lensReviewSummary_lensAlias,
    lensReviewSummary_riskCounts,
    lensReviewSummary_lensName,
    lensReviewSummary_updatedAt,
    lensReviewSummary_lensStatus,
    lensReviewSummary_lensVersion,

    -- * LensSummary
    LensSummary (..),
    newLensSummary,
    lensSummary_lensAlias,
    lensSummary_lensName,
    lensSummary_lensVersion,
    lensSummary_description,

    -- * LensUpgradeSummary
    LensUpgradeSummary (..),
    newLensUpgradeSummary,
    lensUpgradeSummary_lensAlias,
    lensUpgradeSummary_latestLensVersion,
    lensUpgradeSummary_currentLensVersion,
    lensUpgradeSummary_workloadId,
    lensUpgradeSummary_workloadName,

    -- * Milestone
    Milestone (..),
    newMilestone,
    milestone_workload,
    milestone_milestoneNumber,
    milestone_milestoneName,
    milestone_recordedAt,

    -- * MilestoneSummary
    MilestoneSummary (..),
    newMilestoneSummary,
    milestoneSummary_milestoneNumber,
    milestoneSummary_milestoneName,
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
    pillarDifference_pillarId,
    pillarDifference_questionDifferences,
    pillarDifference_differenceStatus,

    -- * PillarReviewSummary
    PillarReviewSummary (..),
    newPillarReviewSummary,
    pillarReviewSummary_pillarId,
    pillarReviewSummary_pillarName,
    pillarReviewSummary_riskCounts,
    pillarReviewSummary_notes,

    -- * QuestionDifference
    QuestionDifference (..),
    newQuestionDifference,
    questionDifference_questionTitle,
    questionDifference_differenceStatus,
    questionDifference_questionId,

    -- * ShareInvitation
    ShareInvitation (..),
    newShareInvitation,
    shareInvitation_workloadId,
    shareInvitation_shareInvitationId,

    -- * ShareInvitationSummary
    ShareInvitationSummary (..),
    newShareInvitationSummary,
    shareInvitationSummary_sharedBy,
    shareInvitationSummary_sharedWith,
    shareInvitationSummary_permissionType,
    shareInvitationSummary_workloadId,
    shareInvitationSummary_workloadName,
    shareInvitationSummary_shareInvitationId,

    -- * VersionDifferences
    VersionDifferences (..),
    newVersionDifferences,
    versionDifferences_pillarDifferences,

    -- * Workload
    Workload (..),
    newWorkload,
    workload_isReviewOwnerUpdateAcknowledged,
    workload_architecturalDesign,
    workload_accountIds,
    workload_lenses,
    workload_reviewRestrictionDate,
    workload_industry,
    workload_environment,
    workload_riskCounts,
    workload_awsRegions,
    workload_owner,
    workload_improvementStatus,
    workload_workloadArn,
    workload_industryType,
    workload_workloadId,
    workload_workloadName,
    workload_updatedAt,
    workload_notes,
    workload_reviewOwner,
    workload_description,
    workload_pillarPriorities,
    workload_shareInvitationId,
    workload_nonAwsRegions,
    workload_tags,

    -- * WorkloadShare
    WorkloadShare (..),
    newWorkloadShare,
    workloadShare_status,
    workloadShare_sharedBy,
    workloadShare_sharedWith,
    workloadShare_permissionType,
    workloadShare_workloadId,
    workloadShare_workloadName,
    workloadShare_shareId,

    -- * WorkloadShareSummary
    WorkloadShareSummary (..),
    newWorkloadShareSummary,
    workloadShareSummary_status,
    workloadShareSummary_sharedWith,
    workloadShareSummary_permissionType,
    workloadShareSummary_shareId,

    -- * WorkloadSummary
    WorkloadSummary (..),
    newWorkloadSummary,
    workloadSummary_lenses,
    workloadSummary_riskCounts,
    workloadSummary_owner,
    workloadSummary_improvementStatus,
    workloadSummary_workloadArn,
    workloadSummary_workloadId,
    workloadSummary_workloadName,
    workloadSummary_updatedAt,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.WellArchitected.Types.Answer
import Amazonka.WellArchitected.Types.AnswerReason
import Amazonka.WellArchitected.Types.AnswerSummary
import Amazonka.WellArchitected.Types.Choice
import Amazonka.WellArchitected.Types.ChoiceAnswer
import Amazonka.WellArchitected.Types.ChoiceAnswerSummary
import Amazonka.WellArchitected.Types.ChoiceReason
import Amazonka.WellArchitected.Types.ChoiceStatus
import Amazonka.WellArchitected.Types.ChoiceUpdate
import Amazonka.WellArchitected.Types.DifferenceStatus
import Amazonka.WellArchitected.Types.ImprovementSummary
import Amazonka.WellArchitected.Types.LensReview
import Amazonka.WellArchitected.Types.LensReviewReport
import Amazonka.WellArchitected.Types.LensReviewSummary
import Amazonka.WellArchitected.Types.LensStatus
import Amazonka.WellArchitected.Types.LensSummary
import Amazonka.WellArchitected.Types.LensUpgradeSummary
import Amazonka.WellArchitected.Types.Milestone
import Amazonka.WellArchitected.Types.MilestoneSummary
import Amazonka.WellArchitected.Types.NotificationSummary
import Amazonka.WellArchitected.Types.NotificationType
import Amazonka.WellArchitected.Types.PermissionType
import Amazonka.WellArchitected.Types.PillarDifference
import Amazonka.WellArchitected.Types.PillarReviewSummary
import Amazonka.WellArchitected.Types.QuestionDifference
import Amazonka.WellArchitected.Types.Risk
import Amazonka.WellArchitected.Types.ShareInvitation
import Amazonka.WellArchitected.Types.ShareInvitationAction
import Amazonka.WellArchitected.Types.ShareInvitationSummary
import Amazonka.WellArchitected.Types.ShareStatus
import Amazonka.WellArchitected.Types.VersionDifferences
import Amazonka.WellArchitected.Types.Workload
import Amazonka.WellArchitected.Types.WorkloadEnvironment
import Amazonka.WellArchitected.Types.WorkloadImprovementStatus
import Amazonka.WellArchitected.Types.WorkloadShare
import Amazonka.WellArchitected.Types.WorkloadShareSummary
import Amazonka.WellArchitected.Types.WorkloadSummary

-- | API version @2020-03-31@ of the Amazon Well-Architected Tool SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "WellArchitected",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "wellarchitected",
      Core._serviceSigningName = "wellarchitected",
      Core._serviceVersion = "2020-03-31",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "WellArchitected",
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

-- | The user input is not valid.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | User does not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The resource already exists.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The user has reached their resource quota.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | Request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | There is a problem with the AWS Well-Architected Tool API service.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The requested resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
