-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _ServiceFault,
    _RequestError,

    -- * ReviewReport
    ReviewReport (..),
    mkReviewReport,
    rrReviewActions,
    rrReviewResults,

    -- * IdempotencyToken
    IdempotencyToken (..),

    -- * ReviewPolicyLevel
    ReviewPolicyLevel (..),

    -- * PaginationToken
    PaginationToken (..),

    -- * WorkerBlock
    WorkerBlock (..),
    mkWorkerBlock,
    wbReason,
    wbWorkerId,

    -- * BonusPayment
    BonusPayment (..),
    mkBonusPayment,
    bpAssignmentId,
    bpBonusAmount,
    bpGrantTime,
    bpReason,
    bpWorkerId,

    -- * ReviewResultDetail
    ReviewResultDetail (..),
    mkReviewResultDetail,
    rrdActionId,
    rrdKey,
    rrdQuestionId,
    rrdSubjectId,
    rrdSubjectType,
    rrdValue,

    -- * ReviewPolicy
    ReviewPolicy (..),
    mkReviewPolicy,
    rpPolicyName,
    rpParameters,

    -- * NotifyWorkersFailureStatus
    NotifyWorkersFailureStatus (..),
    mkNotifyWorkersFailureStatus,
    nwfsNotifyWorkersFailureCode,
    nwfsNotifyWorkersFailureMessage,
    nwfsWorkerId,

    -- * QualificationTypeStatus
    QualificationTypeStatus (..),

    -- * String
    String (..),

    -- * HIT
    HIT (..),
    mkHIT,
    hitAssignmentDurationInSeconds,
    hitAutoApprovalDelayInSeconds,
    hitCreationTime,
    hitDescription,
    hitExpiration,
    hitHITGroupId,
    hitHITId,
    hitHITLayoutId,
    hitHITReviewStatus,
    hitHITStatus,
    hitHITTypeId,
    hitKeywords,
    hitMaxAssignments,
    hitNumberOfAssignmentsAvailable,
    hitNumberOfAssignmentsCompleted,
    hitNumberOfAssignmentsPending,
    hitQualificationRequirements,
    hitQuestion,
    hitRequesterAnnotation,
    hitReward,
    hitTitle,

    -- * NotificationSpecification
    NotificationSpecification (..),
    mkNotificationSpecification,
    nsDestination,
    nsTransport,
    nsVersion,
    nsEventTypes,

    -- * Locale
    Locale (..),
    mkLocale,
    lCountry,
    lSubdivision,

    -- * ReviewActionStatus
    ReviewActionStatus (..),

    -- * CustomerId
    CustomerId (..),

    -- * QualificationType
    QualificationType (..),
    mkQualificationType,
    qtAnswerKey,
    qtAutoGranted,
    qtAutoGrantedValue,
    qtCreationTime,
    qtDescription,
    qtIsRequestable,
    qtKeywords,
    qtName,
    qtQualificationTypeId,
    qtQualificationTypeStatus,
    qtRetryDelayInSeconds,
    qtTest,
    qtTestDurationInSeconds,

    -- * PolicyParameter
    PolicyParameter (..),
    mkPolicyParameter,
    ppKey,
    ppMapEntries,
    ppValues,

    -- * HITStatus
    HITStatus (..),

    -- * ReviewableHITStatus
    ReviewableHITStatus (..),

    -- * HITAccessActions
    HITAccessActions (..),

    -- * EventType
    EventType (..),

    -- * AssignmentStatus
    AssignmentStatus (..),

    -- * Comparator
    Comparator (..),

    -- * QualificationStatus
    QualificationStatus (..),

    -- * ReviewActionDetail
    ReviewActionDetail (..),
    mkReviewActionDetail,
    radActionId,
    radActionName,
    radCompleteTime,
    radErrorCode,
    radResult,
    radStatus,
    radTargetId,
    radTargetType,

    -- * QualificationRequest
    QualificationRequest (..),
    mkQualificationRequest,
    qrfAnswer,
    qrfQualificationRequestId,
    qrfQualificationTypeId,
    qrfSubmitTime,
    qrfTest,
    qrfWorkerId,

    -- * NotifyWorkersFailureCode
    NotifyWorkersFailureCode (..),

    -- * HITReviewStatus
    HITReviewStatus (..),

    -- * Assignment
    Assignment (..),
    mkAssignment,
    aAcceptTime,
    aAnswer,
    aApprovalTime,
    aAssignmentId,
    aAssignmentStatus,
    aAutoApprovalTime,
    aDeadline,
    aHITId,
    aRejectionTime,
    aRequesterFeedback,
    aSubmitTime,
    aWorkerId,

    -- * QualificationRequirement
    QualificationRequirement (..),
    mkQualificationRequirement,
    qrQualificationTypeId,
    qrComparator,
    qrActionsGuarded,
    qrIntegerValues,
    qrLocaleValues,
    qrRequiredToPreview,

    -- * EntityId
    EntityId (..),

    -- * HITLayoutParameter
    HITLayoutParameter (..),
    mkHITLayoutParameter,
    hitlpName,
    hitlpValue,

    -- * NotificationTransport
    NotificationTransport (..),

    -- * ParameterMapEntry
    ParameterMapEntry (..),
    mkParameterMapEntry,
    pmeKey,
    pmeValues,

    -- * Qualification
    Qualification (..),
    mkQualification,
    qGrantTime,
    qIntegerValue,
    qLocaleValue,
    qQualificationTypeId,
    qStatus,
    qWorkerId,

    -- * AssignmentId
    AssignmentId (..),

    -- * HITId
    HITId (..),

    -- * Reason
    Reason (..),

    -- * WorkerId
    WorkerId (..),

    -- * Subject
    Subject (..),

    -- * MessageText
    MessageText (..),

    -- * HITTypeId
    HITTypeId (..),

    -- * HITLayoutId
    HITLayoutId (..),

    -- * Question
    Question (..),

    -- * RequesterAnnotation
    RequesterAnnotation (..),

    -- * BonusAmount
    BonusAmount (..),

    -- * ActionId
    ActionId (..),

    -- * Key
    Key (..),

    -- * QuestionId
    QuestionId (..),

    -- * SubjectId
    SubjectId (..),

    -- * SubjectType
    SubjectType (..),

    -- * Value
    Value (..),

    -- * PolicyName
    PolicyName (..),

    -- * QualificationTypeId
    QualificationTypeId (..),

    -- * NotifyWorkersFailureMessage
    NotifyWorkersFailureMessage (..),

    -- * HITGroupId
    HITGroupId (..),

    -- * Reward
    Reward (..),

    -- * Country
    Country (..),

    -- * Subdivision
    Subdivision (..),

    -- * AvailableBalance
    AvailableBalance (..),

    -- * OnHoldBalance
    OnHoldBalance (..),

    -- * TargetId
    TargetId (..),
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types.ActionId
import Network.AWS.MechanicalTurk.Types.Assignment
import Network.AWS.MechanicalTurk.Types.AssignmentId
import Network.AWS.MechanicalTurk.Types.AssignmentStatus
import Network.AWS.MechanicalTurk.Types.AvailableBalance
import Network.AWS.MechanicalTurk.Types.BonusAmount
import Network.AWS.MechanicalTurk.Types.BonusPayment
import Network.AWS.MechanicalTurk.Types.Comparator
import Network.AWS.MechanicalTurk.Types.Country
import Network.AWS.MechanicalTurk.Types.CustomerId
import Network.AWS.MechanicalTurk.Types.EntityId
import Network.AWS.MechanicalTurk.Types.EventType
import Network.AWS.MechanicalTurk.Types.HIT
import Network.AWS.MechanicalTurk.Types.HITAccessActions
import Network.AWS.MechanicalTurk.Types.HITGroupId
import Network.AWS.MechanicalTurk.Types.HITId
import Network.AWS.MechanicalTurk.Types.HITLayoutId
import Network.AWS.MechanicalTurk.Types.HITLayoutParameter
import Network.AWS.MechanicalTurk.Types.HITReviewStatus
import Network.AWS.MechanicalTurk.Types.HITStatus
import Network.AWS.MechanicalTurk.Types.HITTypeId
import Network.AWS.MechanicalTurk.Types.IdempotencyToken
import Network.AWS.MechanicalTurk.Types.Key
import Network.AWS.MechanicalTurk.Types.Locale
import Network.AWS.MechanicalTurk.Types.MessageText
import Network.AWS.MechanicalTurk.Types.NotificationSpecification
import Network.AWS.MechanicalTurk.Types.NotificationTransport
import Network.AWS.MechanicalTurk.Types.NotifyWorkersFailureCode
import Network.AWS.MechanicalTurk.Types.NotifyWorkersFailureMessage
import Network.AWS.MechanicalTurk.Types.NotifyWorkersFailureStatus
import Network.AWS.MechanicalTurk.Types.OnHoldBalance
import Network.AWS.MechanicalTurk.Types.PaginationToken
import Network.AWS.MechanicalTurk.Types.ParameterMapEntry
import Network.AWS.MechanicalTurk.Types.PolicyName
import Network.AWS.MechanicalTurk.Types.PolicyParameter
import Network.AWS.MechanicalTurk.Types.Qualification
import Network.AWS.MechanicalTurk.Types.QualificationRequest
import Network.AWS.MechanicalTurk.Types.QualificationRequirement
import Network.AWS.MechanicalTurk.Types.QualificationStatus
import Network.AWS.MechanicalTurk.Types.QualificationType
import Network.AWS.MechanicalTurk.Types.QualificationTypeId
import Network.AWS.MechanicalTurk.Types.QualificationTypeStatus
import Network.AWS.MechanicalTurk.Types.Question
import Network.AWS.MechanicalTurk.Types.QuestionId
import Network.AWS.MechanicalTurk.Types.Reason
import Network.AWS.MechanicalTurk.Types.RequesterAnnotation
import Network.AWS.MechanicalTurk.Types.ReviewActionDetail
import Network.AWS.MechanicalTurk.Types.ReviewActionStatus
import Network.AWS.MechanicalTurk.Types.ReviewPolicy
import Network.AWS.MechanicalTurk.Types.ReviewPolicyLevel
import Network.AWS.MechanicalTurk.Types.ReviewReport
import Network.AWS.MechanicalTurk.Types.ReviewResultDetail
import Network.AWS.MechanicalTurk.Types.ReviewableHITStatus
import Network.AWS.MechanicalTurk.Types.Reward
import Network.AWS.MechanicalTurk.Types.String
import Network.AWS.MechanicalTurk.Types.Subdivision
import Network.AWS.MechanicalTurk.Types.Subject
import Network.AWS.MechanicalTurk.Types.SubjectId
import Network.AWS.MechanicalTurk.Types.SubjectType
import Network.AWS.MechanicalTurk.Types.TargetId
import Network.AWS.MechanicalTurk.Types.Value
import Network.AWS.MechanicalTurk.Types.WorkerBlock
import Network.AWS.MechanicalTurk.Types.WorkerId
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-01-17@ of the Amazon Mechanical Turk SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "MechanicalTurk",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "mturk-requester",
      Core._svcVersion = "2017-01-17",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "MechanicalTurk",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | Amazon Mechanical Turk is temporarily unable to process your request. Try your call again.
_ServiceFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceFault =
  Core._MatchServiceError mkServiceConfig "ServiceFault"
{-# DEPRECATED _ServiceFault "Use generic-lens or generic-optics instead." #-}

-- | Your request is invalid.
_RequestError :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RequestError =
  Core._MatchServiceError mkServiceConfig "RequestError"
{-# DEPRECATED _RequestError "Use generic-lens or generic-optics instead." #-}
