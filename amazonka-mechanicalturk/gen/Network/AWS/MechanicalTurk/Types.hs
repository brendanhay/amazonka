{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ServiceFault,
    _RequestError,

    -- * AssignmentStatus
    AssignmentStatus (..),

    -- * Comparator
    Comparator (..),

    -- * EventType
    EventType (..),

    -- * HITAccessActions
    HITAccessActions (..),

    -- * HITReviewStatus
    HITReviewStatus (..),

    -- * HITStatus
    HITStatus (..),

    -- * NotificationTransport
    NotificationTransport (..),

    -- * NotifyWorkersFailureCode
    NotifyWorkersFailureCode (..),

    -- * QualificationStatus
    QualificationStatus (..),

    -- * QualificationTypeStatus
    QualificationTypeStatus (..),

    -- * ReviewActionStatus
    ReviewActionStatus (..),

    -- * ReviewPolicyLevel
    ReviewPolicyLevel (..),

    -- * ReviewableHITStatus
    ReviewableHITStatus (..),

    -- * Assignment
    Assignment (..),
    newAssignment,
    assignment_requesterFeedback,
    assignment_assignmentId,
    assignment_hITId,
    assignment_autoApprovalTime,
    assignment_rejectionTime,
    assignment_assignmentStatus,
    assignment_answer,
    assignment_submitTime,
    assignment_workerId,
    assignment_acceptTime,
    assignment_approvalTime,
    assignment_deadline,

    -- * BonusPayment
    BonusPayment (..),
    newBonusPayment,
    bonusPayment_bonusAmount,
    bonusPayment_assignmentId,
    bonusPayment_grantTime,
    bonusPayment_reason,
    bonusPayment_workerId,

    -- * HIT
    HIT (..),
    newHIT,
    hit_hITGroupId,
    hit_creationTime,
    hit_assignmentDurationInSeconds,
    hit_autoApprovalDelayInSeconds,
    hit_question,
    hit_expiration,
    hit_hITReviewStatus,
    hit_title,
    hit_hITId,
    hit_hITLayoutId,
    hit_numberOfAssignmentsCompleted,
    hit_reward,
    hit_maxAssignments,
    hit_hITStatus,
    hit_requesterAnnotation,
    hit_description,
    hit_numberOfAssignmentsAvailable,
    hit_hITTypeId,
    hit_qualificationRequirements,
    hit_numberOfAssignmentsPending,
    hit_keywords,

    -- * HITLayoutParameter
    HITLayoutParameter (..),
    newHITLayoutParameter,
    hITLayoutParameter_name,
    hITLayoutParameter_value,

    -- * Locale
    Locale (..),
    newLocale,
    locale_subdivision,
    locale_country,

    -- * NotificationSpecification
    NotificationSpecification (..),
    newNotificationSpecification,
    notificationSpecification_destination,
    notificationSpecification_transport,
    notificationSpecification_version,
    notificationSpecification_eventTypes,

    -- * NotifyWorkersFailureStatus
    NotifyWorkersFailureStatus (..),
    newNotifyWorkersFailureStatus,
    notifyWorkersFailureStatus_workerId,
    notifyWorkersFailureStatus_notifyWorkersFailureCode,
    notifyWorkersFailureStatus_notifyWorkersFailureMessage,

    -- * ParameterMapEntry
    ParameterMapEntry (..),
    newParameterMapEntry,
    parameterMapEntry_key,
    parameterMapEntry_values,

    -- * PolicyParameter
    PolicyParameter (..),
    newPolicyParameter,
    policyParameter_key,
    policyParameter_values,
    policyParameter_mapEntries,

    -- * Qualification
    Qualification (..),
    newQualification,
    qualification_qualificationTypeId,
    qualification_status,
    qualification_grantTime,
    qualification_workerId,
    qualification_localeValue,
    qualification_integerValue,

    -- * QualificationRequest
    QualificationRequest (..),
    newQualificationRequest,
    qualificationRequest_qualificationTypeId,
    qualificationRequest_answer,
    qualificationRequest_submitTime,
    qualificationRequest_test,
    qualificationRequest_workerId,
    qualificationRequest_qualificationRequestId,

    -- * QualificationRequirement
    QualificationRequirement (..),
    newQualificationRequirement,
    qualificationRequirement_actionsGuarded,
    qualificationRequirement_localeValues,
    qualificationRequirement_requiredToPreview,
    qualificationRequirement_integerValues,
    qualificationRequirement_qualificationTypeId,
    qualificationRequirement_comparator,

    -- * QualificationType
    QualificationType (..),
    newQualificationType,
    qualificationType_qualificationTypeId,
    qualificationType_creationTime,
    qualificationType_isRequestable,
    qualificationType_retryDelayInSeconds,
    qualificationType_autoGranted,
    qualificationType_qualificationTypeStatus,
    qualificationType_name,
    qualificationType_testDurationInSeconds,
    qualificationType_description,
    qualificationType_test,
    qualificationType_answerKey,
    qualificationType_autoGrantedValue,
    qualificationType_keywords,

    -- * ReviewActionDetail
    ReviewActionDetail (..),
    newReviewActionDetail,
    reviewActionDetail_status,
    reviewActionDetail_targetId,
    reviewActionDetail_actionName,
    reviewActionDetail_targetType,
    reviewActionDetail_result,
    reviewActionDetail_actionId,
    reviewActionDetail_completeTime,
    reviewActionDetail_errorCode,

    -- * ReviewPolicy
    ReviewPolicy (..),
    newReviewPolicy,
    reviewPolicy_parameters,
    reviewPolicy_policyName,

    -- * ReviewReport
    ReviewReport (..),
    newReviewReport,
    reviewReport_reviewActions,
    reviewReport_reviewResults,

    -- * ReviewResultDetail
    ReviewResultDetail (..),
    newReviewResultDetail,
    reviewResultDetail_key,
    reviewResultDetail_subjectType,
    reviewResultDetail_subjectId,
    reviewResultDetail_actionId,
    reviewResultDetail_value,
    reviewResultDetail_questionId,

    -- * WorkerBlock
    WorkerBlock (..),
    newWorkerBlock,
    workerBlock_reason,
    workerBlock_workerId,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types.Assignment
import Network.AWS.MechanicalTurk.Types.AssignmentStatus
import Network.AWS.MechanicalTurk.Types.BonusPayment
import Network.AWS.MechanicalTurk.Types.Comparator
import Network.AWS.MechanicalTurk.Types.EventType
import Network.AWS.MechanicalTurk.Types.HIT
import Network.AWS.MechanicalTurk.Types.HITAccessActions
import Network.AWS.MechanicalTurk.Types.HITLayoutParameter
import Network.AWS.MechanicalTurk.Types.HITReviewStatus
import Network.AWS.MechanicalTurk.Types.HITStatus
import Network.AWS.MechanicalTurk.Types.Locale
import Network.AWS.MechanicalTurk.Types.NotificationSpecification
import Network.AWS.MechanicalTurk.Types.NotificationTransport
import Network.AWS.MechanicalTurk.Types.NotifyWorkersFailureCode
import Network.AWS.MechanicalTurk.Types.NotifyWorkersFailureStatus
import Network.AWS.MechanicalTurk.Types.ParameterMapEntry
import Network.AWS.MechanicalTurk.Types.PolicyParameter
import Network.AWS.MechanicalTurk.Types.Qualification
import Network.AWS.MechanicalTurk.Types.QualificationRequest
import Network.AWS.MechanicalTurk.Types.QualificationRequirement
import Network.AWS.MechanicalTurk.Types.QualificationStatus
import Network.AWS.MechanicalTurk.Types.QualificationType
import Network.AWS.MechanicalTurk.Types.QualificationTypeStatus
import Network.AWS.MechanicalTurk.Types.ReviewActionDetail
import Network.AWS.MechanicalTurk.Types.ReviewActionStatus
import Network.AWS.MechanicalTurk.Types.ReviewPolicy
import Network.AWS.MechanicalTurk.Types.ReviewPolicyLevel
import Network.AWS.MechanicalTurk.Types.ReviewReport
import Network.AWS.MechanicalTurk.Types.ReviewResultDetail
import Network.AWS.MechanicalTurk.Types.ReviewableHITStatus
import Network.AWS.MechanicalTurk.Types.WorkerBlock
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-01-17@ of the Amazon Mechanical Turk SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "MechanicalTurk",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "mturk-requester",
      Core._serviceSigningName = "mturk-requester",
      Core._serviceVersion = "2017-01-17",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "MechanicalTurk",
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
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | Amazon Mechanical Turk is temporarily unable to process your request.
-- Try your call again.
_ServiceFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceFault =
  Core._MatchServiceError
    defaultService
    "ServiceFault"

-- | Your request is invalid.
_RequestError :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RequestError =
  Core._MatchServiceError
    defaultService
    "RequestError"
