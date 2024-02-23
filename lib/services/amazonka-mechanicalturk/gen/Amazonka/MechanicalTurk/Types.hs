{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MechanicalTurk.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MechanicalTurk.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _RequestError,
    _ServiceFault,

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
    assignment_acceptTime,
    assignment_answer,
    assignment_approvalTime,
    assignment_assignmentId,
    assignment_assignmentStatus,
    assignment_autoApprovalTime,
    assignment_deadline,
    assignment_hITId,
    assignment_rejectionTime,
    assignment_requesterFeedback,
    assignment_submitTime,
    assignment_workerId,

    -- * BonusPayment
    BonusPayment (..),
    newBonusPayment,
    bonusPayment_assignmentId,
    bonusPayment_bonusAmount,
    bonusPayment_grantTime,
    bonusPayment_reason,
    bonusPayment_workerId,

    -- * HIT
    HIT (..),
    newHIT,
    hit_assignmentDurationInSeconds,
    hit_autoApprovalDelayInSeconds,
    hit_creationTime,
    hit_description,
    hit_expiration,
    hit_hITGroupId,
    hit_hITId,
    hit_hITLayoutId,
    hit_hITReviewStatus,
    hit_hITStatus,
    hit_hITTypeId,
    hit_keywords,
    hit_maxAssignments,
    hit_numberOfAssignmentsAvailable,
    hit_numberOfAssignmentsCompleted,
    hit_numberOfAssignmentsPending,
    hit_qualificationRequirements,
    hit_question,
    hit_requesterAnnotation,
    hit_reward,
    hit_title,

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
    notifyWorkersFailureStatus_notifyWorkersFailureCode,
    notifyWorkersFailureStatus_notifyWorkersFailureMessage,
    notifyWorkersFailureStatus_workerId,

    -- * ParameterMapEntry
    ParameterMapEntry (..),
    newParameterMapEntry,
    parameterMapEntry_key,
    parameterMapEntry_values,

    -- * PolicyParameter
    PolicyParameter (..),
    newPolicyParameter,
    policyParameter_key,
    policyParameter_mapEntries,
    policyParameter_values,

    -- * Qualification
    Qualification (..),
    newQualification,
    qualification_grantTime,
    qualification_integerValue,
    qualification_localeValue,
    qualification_qualificationTypeId,
    qualification_status,
    qualification_workerId,

    -- * QualificationRequest
    QualificationRequest (..),
    newQualificationRequest,
    qualificationRequest_answer,
    qualificationRequest_qualificationRequestId,
    qualificationRequest_qualificationTypeId,
    qualificationRequest_submitTime,
    qualificationRequest_test,
    qualificationRequest_workerId,

    -- * QualificationRequirement
    QualificationRequirement (..),
    newQualificationRequirement,
    qualificationRequirement_actionsGuarded,
    qualificationRequirement_integerValues,
    qualificationRequirement_localeValues,
    qualificationRequirement_requiredToPreview,
    qualificationRequirement_qualificationTypeId,
    qualificationRequirement_comparator,

    -- * QualificationType
    QualificationType (..),
    newQualificationType,
    qualificationType_answerKey,
    qualificationType_autoGranted,
    qualificationType_autoGrantedValue,
    qualificationType_creationTime,
    qualificationType_description,
    qualificationType_isRequestable,
    qualificationType_keywords,
    qualificationType_name,
    qualificationType_qualificationTypeId,
    qualificationType_qualificationTypeStatus,
    qualificationType_retryDelayInSeconds,
    qualificationType_test,
    qualificationType_testDurationInSeconds,

    -- * ReviewActionDetail
    ReviewActionDetail (..),
    newReviewActionDetail,
    reviewActionDetail_actionId,
    reviewActionDetail_actionName,
    reviewActionDetail_completeTime,
    reviewActionDetail_errorCode,
    reviewActionDetail_result,
    reviewActionDetail_status,
    reviewActionDetail_targetId,
    reviewActionDetail_targetType,

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
    reviewResultDetail_actionId,
    reviewResultDetail_key,
    reviewResultDetail_questionId,
    reviewResultDetail_subjectId,
    reviewResultDetail_subjectType,
    reviewResultDetail_value,

    -- * WorkerBlock
    WorkerBlock (..),
    newWorkerBlock,
    workerBlock_reason,
    workerBlock_workerId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MechanicalTurk.Types.Assignment
import Amazonka.MechanicalTurk.Types.AssignmentStatus
import Amazonka.MechanicalTurk.Types.BonusPayment
import Amazonka.MechanicalTurk.Types.Comparator
import Amazonka.MechanicalTurk.Types.EventType
import Amazonka.MechanicalTurk.Types.HIT
import Amazonka.MechanicalTurk.Types.HITAccessActions
import Amazonka.MechanicalTurk.Types.HITLayoutParameter
import Amazonka.MechanicalTurk.Types.HITReviewStatus
import Amazonka.MechanicalTurk.Types.HITStatus
import Amazonka.MechanicalTurk.Types.Locale
import Amazonka.MechanicalTurk.Types.NotificationSpecification
import Amazonka.MechanicalTurk.Types.NotificationTransport
import Amazonka.MechanicalTurk.Types.NotifyWorkersFailureCode
import Amazonka.MechanicalTurk.Types.NotifyWorkersFailureStatus
import Amazonka.MechanicalTurk.Types.ParameterMapEntry
import Amazonka.MechanicalTurk.Types.PolicyParameter
import Amazonka.MechanicalTurk.Types.Qualification
import Amazonka.MechanicalTurk.Types.QualificationRequest
import Amazonka.MechanicalTurk.Types.QualificationRequirement
import Amazonka.MechanicalTurk.Types.QualificationStatus
import Amazonka.MechanicalTurk.Types.QualificationType
import Amazonka.MechanicalTurk.Types.QualificationTypeStatus
import Amazonka.MechanicalTurk.Types.ReviewActionDetail
import Amazonka.MechanicalTurk.Types.ReviewActionStatus
import Amazonka.MechanicalTurk.Types.ReviewPolicy
import Amazonka.MechanicalTurk.Types.ReviewPolicyLevel
import Amazonka.MechanicalTurk.Types.ReviewReport
import Amazonka.MechanicalTurk.Types.ReviewResultDetail
import Amazonka.MechanicalTurk.Types.ReviewableHITStatus
import Amazonka.MechanicalTurk.Types.WorkerBlock
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-01-17@ of the Amazon Mechanical Turk SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "MechanicalTurk",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "mturk-requester",
      Core.signingName = "mturk-requester",
      Core.version = "2017-01-17",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "MechanicalTurk",
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

-- | Your request is invalid.
_RequestError :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_RequestError =
  Core._MatchServiceError
    defaultService
    "RequestError"

-- | Amazon Mechanical Turk is temporarily unable to process your request.
-- Try your call again.
_ServiceFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceFault =
  Core._MatchServiceError
    defaultService
    "ServiceFault"
