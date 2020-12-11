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
    mechanicalTurkService,

    -- * Errors

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
    mkAssignment,
    aAcceptTime,
    aAnswer,
    aAssignmentStatus,
    aRequesterFeedback,
    aDeadline,
    aApprovalTime,
    aRejectionTime,
    aAutoApprovalTime,
    aHITId,
    aWorkerId,
    aAssignmentId,
    aSubmitTime,

    -- * BonusPayment
    BonusPayment (..),
    mkBonusPayment,
    bpReason,
    bpGrantTime,
    bpWorkerId,
    bpAssignmentId,
    bpBonusAmount,

    -- * HIT
    HIT (..),
    mkHIT,
    hitCreationTime,
    hitHITGroupId,
    hitNumberOfAssignmentsPending,
    hitHITTypeId,
    hitExpiration,
    hitAutoApprovalDelayInSeconds,
    hitRequesterAnnotation,
    hitHITStatus,
    hitMaxAssignments,
    hitNumberOfAssignmentsCompleted,
    hitReward,
    hitKeywords,
    hitHITLayoutId,
    hitQualificationRequirements,
    hitTitle,
    hitHITId,
    hitHITReviewStatus,
    hitNumberOfAssignmentsAvailable,
    hitDescription,
    hitQuestion,
    hitAssignmentDurationInSeconds,

    -- * HITLayoutParameter
    HITLayoutParameter (..),
    mkHITLayoutParameter,
    hitlpName,
    hitlpValue,

    -- * Locale
    Locale (..),
    mkLocale,
    lSubdivision,
    lCountry,

    -- * NotificationSpecification
    NotificationSpecification (..),
    mkNotificationSpecification,
    nsDestination,
    nsTransport,
    nsVersion,
    nsEventTypes,

    -- * NotifyWorkersFailureStatus
    NotifyWorkersFailureStatus (..),
    mkNotifyWorkersFailureStatus,
    nwfsNotifyWorkersFailureMessage,
    nwfsNotifyWorkersFailureCode,
    nwfsWorkerId,

    -- * ParameterMapEntry
    ParameterMapEntry (..),
    mkParameterMapEntry,
    pmeValues,
    pmeKey,

    -- * PolicyParameter
    PolicyParameter (..),
    mkPolicyParameter,
    ppValues,
    ppMapEntries,
    ppKey,

    -- * Qualification
    Qualification (..),
    mkQualification,
    qStatus,
    qIntegerValue,
    qLocaleValue,
    qQualificationTypeId,
    qGrantTime,
    qWorkerId,

    -- * QualificationRequest
    QualificationRequest (..),
    mkQualificationRequest,
    quaQualificationRequestId,
    quaTest,
    quaQualificationTypeId,
    quaAnswer,
    quaWorkerId,
    quaSubmitTime,

    -- * QualificationRequirement
    QualificationRequirement (..),
    mkQualificationRequirement,
    qrLocaleValues,
    qrActionsGuarded,
    qrRequiredToPreview,
    qrIntegerValues,
    qrQualificationTypeId,
    qrComparator,

    -- * QualificationType
    QualificationType (..),
    mkQualificationType,
    qtCreationTime,
    qtTestDurationInSeconds,
    qtQualificationTypeStatus,
    qtAnswerKey,
    qtTest,
    qtQualificationTypeId,
    qtName,
    qtKeywords,
    qtAutoGranted,
    qtAutoGrantedValue,
    qtDescription,
    qtIsRequestable,
    qtRetryDelayInSeconds,

    -- * ReviewActionDetail
    ReviewActionDetail (..),
    mkReviewActionDetail,
    radStatus,
    radTargetId,
    radActionId,
    radTargetType,
    radResult,
    radActionName,
    radCompleteTime,
    radErrorCode,

    -- * ReviewPolicy
    ReviewPolicy (..),
    mkReviewPolicy,
    rpParameters,
    rpPolicyName,

    -- * ReviewReport
    ReviewReport (..),
    mkReviewReport,
    rrReviewActions,
    rrReviewResults,

    -- * ReviewResultDetail
    ReviewResultDetail (..),
    mkReviewResultDetail,
    rrdValue,
    rrdActionId,
    rrdSubjectType,
    rrdKey,
    rrdQuestionId,
    rrdSubjectId,

    -- * WorkerBlock
    WorkerBlock (..),
    mkWorkerBlock,
    wbReason,
    wbWorkerId,
  )
where

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
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-01-17@ of the Amazon Mechanical Turk SDK configuration.
mechanicalTurkService :: Lude.Service
mechanicalTurkService =
  Lude.Service
    { Lude._svcAbbrev = "MechanicalTurk",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "mturk-requester",
      Lude._svcVersion = "2017-01-17",
      Lude._svcEndpoint = Lude.defaultEndpoint mechanicalTurkService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "MechanicalTurk",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
