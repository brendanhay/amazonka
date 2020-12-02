{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MechanicalTurk.Types
    (
    -- * Service Configuration
      mechanicalTurk

    -- * Errors
    , _ServiceFault
    , _RequestError

    -- * AssignmentStatus
    , AssignmentStatus (..)

    -- * Comparator
    , Comparator (..)

    -- * EventType
    , EventType (..)

    -- * HITAccessActions
    , HITAccessActions (..)

    -- * HITReviewStatus
    , HITReviewStatus (..)

    -- * HITStatus
    , HITStatus (..)

    -- * NotificationTransport
    , NotificationTransport (..)

    -- * NotifyWorkersFailureCode
    , NotifyWorkersFailureCode (..)

    -- * QualificationStatus
    , QualificationStatus (..)

    -- * QualificationTypeStatus
    , QualificationTypeStatus (..)

    -- * ReviewActionStatus
    , ReviewActionStatus (..)

    -- * ReviewPolicyLevel
    , ReviewPolicyLevel (..)

    -- * ReviewableHITStatus
    , ReviewableHITStatus (..)

    -- * Assignment
    , Assignment
    , assignment
    , aAcceptTime
    , aAnswer
    , aAssignmentStatus
    , aRequesterFeedback
    , aDeadline
    , aApprovalTime
    , aRejectionTime
    , aAutoApprovalTime
    , aHITId
    , aWorkerId
    , aAssignmentId
    , aSubmitTime

    -- * BonusPayment
    , BonusPayment
    , bonusPayment
    , bpReason
    , bpGrantTime
    , bpWorkerId
    , bpAssignmentId
    , bpBonusAmount

    -- * HIT
    , HIT
    , hIT
    , hitCreationTime
    , hitHITGroupId
    , hitNumberOfAssignmentsPending
    , hitHITTypeId
    , hitExpiration
    , hitAutoApprovalDelayInSeconds
    , hitRequesterAnnotation
    , hitHITStatus
    , hitMaxAssignments
    , hitNumberOfAssignmentsCompleted
    , hitReward
    , hitKeywords
    , hitHITLayoutId
    , hitQualificationRequirements
    , hitTitle
    , hitHITId
    , hitHITReviewStatus
    , hitNumberOfAssignmentsAvailable
    , hitDescription
    , hitQuestion
    , hitAssignmentDurationInSeconds

    -- * HITLayoutParameter
    , HITLayoutParameter
    , hITLayoutParameter
    , hitlpName
    , hitlpValue

    -- * Locale
    , Locale
    , locale
    , lSubdivision
    , lCountry

    -- * NotificationSpecification
    , NotificationSpecification
    , notificationSpecification
    , nsDestination
    , nsTransport
    , nsVersion
    , nsEventTypes

    -- * NotifyWorkersFailureStatus
    , NotifyWorkersFailureStatus
    , notifyWorkersFailureStatus
    , nwfsNotifyWorkersFailureMessage
    , nwfsNotifyWorkersFailureCode
    , nwfsWorkerId

    -- * ParameterMapEntry
    , ParameterMapEntry
    , parameterMapEntry
    , pmeValues
    , pmeKey

    -- * PolicyParameter
    , PolicyParameter
    , policyParameter
    , ppValues
    , ppMapEntries
    , ppKey

    -- * Qualification
    , Qualification
    , qualification
    , qStatus
    , qIntegerValue
    , qLocaleValue
    , qQualificationTypeId
    , qGrantTime
    , qWorkerId

    -- * QualificationRequest
    , QualificationRequest
    , qualificationRequest
    , quaQualificationRequestId
    , quaTest
    , quaQualificationTypeId
    , quaAnswer
    , quaWorkerId
    , quaSubmitTime

    -- * QualificationRequirement
    , QualificationRequirement
    , qualificationRequirement
    , qrLocaleValues
    , qrActionsGuarded
    , qrRequiredToPreview
    , qrIntegerValues
    , qrQualificationTypeId
    , qrComparator

    -- * QualificationType
    , QualificationType
    , qualificationType
    , qtCreationTime
    , qtTestDurationInSeconds
    , qtQualificationTypeStatus
    , qtAnswerKey
    , qtTest
    , qtQualificationTypeId
    , qtName
    , qtKeywords
    , qtAutoGranted
    , qtAutoGrantedValue
    , qtDescription
    , qtIsRequestable
    , qtRetryDelayInSeconds

    -- * ReviewActionDetail
    , ReviewActionDetail
    , reviewActionDetail
    , radStatus
    , radTargetId
    , radActionId
    , radTargetType
    , radResult
    , radActionName
    , radCompleteTime
    , radErrorCode

    -- * ReviewPolicy
    , ReviewPolicy
    , reviewPolicy
    , rpParameters
    , rpPolicyName

    -- * ReviewReport
    , ReviewReport
    , reviewReport
    , rrReviewActions
    , rrReviewResults

    -- * ReviewResultDetail
    , ReviewResultDetail
    , reviewResultDetail
    , rrdValue
    , rrdActionId
    , rrdSubjectType
    , rrdKey
    , rrdQuestionId
    , rrdSubjectId

    -- * WorkerBlock
    , WorkerBlock
    , workerBlock
    , wbReason
    , wbWorkerId
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.MechanicalTurk.Types.Sum
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-01-17@ of the Amazon Mechanical Turk SDK configuration.
mechanicalTurk :: Service
mechanicalTurk =
  Service
    { _svcAbbrev = "MechanicalTurk"
    , _svcSigner = v4
    , _svcPrefix = "mturk-requester"
    , _svcVersion = "2017-01-17"
    , _svcEndpoint = defaultEndpoint mechanicalTurk
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "MechanicalTurk"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | Amazon Mechanical Turk is temporarily unable to process your request. Try your call again.
--
--
_ServiceFault :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceFault = _MatchServiceError mechanicalTurk "ServiceFault"


-- | Your request is invalid.
--
--
_RequestError :: AsError a => Getting (First ServiceError) a ServiceError
_RequestError = _MatchServiceError mechanicalTurk "RequestError"

