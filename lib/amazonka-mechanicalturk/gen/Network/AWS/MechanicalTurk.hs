{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Mechanical Turk API Reference__
module Network.AWS.MechanicalTurk
    (
    -- * Service Configuration
      mechanicalTurk

    -- * Errors
    -- $errors

    -- ** ServiceFault
    , _ServiceFault

    -- ** RequestError
    , _RequestError

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ApproveAssignment
    , module Network.AWS.MechanicalTurk.ApproveAssignment

    -- ** ListReviewPolicyResultsForHIT
    , module Network.AWS.MechanicalTurk.ListReviewPolicyResultsForHIT

    -- ** ListHITs (Paginated)
    , module Network.AWS.MechanicalTurk.ListHITs

    -- ** ListWorkersWithQualificationType (Paginated)
    , module Network.AWS.MechanicalTurk.ListWorkersWithQualificationType

    -- ** DeleteHIT
    , module Network.AWS.MechanicalTurk.DeleteHIT

    -- ** ListReviewableHITs (Paginated)
    , module Network.AWS.MechanicalTurk.ListReviewableHITs

    -- ** GetAssignment
    , module Network.AWS.MechanicalTurk.GetAssignment

    -- ** DeleteQualificationType
    , module Network.AWS.MechanicalTurk.DeleteQualificationType

    -- ** UpdateQualificationType
    , module Network.AWS.MechanicalTurk.UpdateQualificationType

    -- ** ListQualificationTypes (Paginated)
    , module Network.AWS.MechanicalTurk.ListQualificationTypes

    -- ** UpdateHITTypeOfHIT
    , module Network.AWS.MechanicalTurk.UpdateHITTypeOfHIT

    -- ** DisassociateQualificationFromWorker
    , module Network.AWS.MechanicalTurk.DisassociateQualificationFromWorker

    -- ** SendTestEventNotification
    , module Network.AWS.MechanicalTurk.SendTestEventNotification

    -- ** NotifyWorkers
    , module Network.AWS.MechanicalTurk.NotifyWorkers

    -- ** CreateHITWithHITType
    , module Network.AWS.MechanicalTurk.CreateHITWithHITType

    -- ** CreateHITType
    , module Network.AWS.MechanicalTurk.CreateHITType

    -- ** SendBonus
    , module Network.AWS.MechanicalTurk.SendBonus

    -- ** ListQualificationRequests (Paginated)
    , module Network.AWS.MechanicalTurk.ListQualificationRequests

    -- ** UpdateExpirationForHIT
    , module Network.AWS.MechanicalTurk.UpdateExpirationForHIT

    -- ** RejectAssignment
    , module Network.AWS.MechanicalTurk.RejectAssignment

    -- ** ListAssignmentsForHIT (Paginated)
    , module Network.AWS.MechanicalTurk.ListAssignmentsForHIT

    -- ** RejectQualificationRequest
    , module Network.AWS.MechanicalTurk.RejectQualificationRequest

    -- ** GetQualificationScore
    , module Network.AWS.MechanicalTurk.GetQualificationScore

    -- ** GetQualificationType
    , module Network.AWS.MechanicalTurk.GetQualificationType

    -- ** UpdateHITReviewStatus
    , module Network.AWS.MechanicalTurk.UpdateHITReviewStatus

    -- ** CreateQualificationType
    , module Network.AWS.MechanicalTurk.CreateQualificationType

    -- ** AcceptQualificationRequest
    , module Network.AWS.MechanicalTurk.AcceptQualificationRequest

    -- ** GetFileUploadURL
    , module Network.AWS.MechanicalTurk.GetFileUploadURL

    -- ** CreateAdditionalAssignmentsForHIT
    , module Network.AWS.MechanicalTurk.CreateAdditionalAssignmentsForHIT

    -- ** GetHIT
    , module Network.AWS.MechanicalTurk.GetHIT

    -- ** CreateWorkerBlock
    , module Network.AWS.MechanicalTurk.CreateWorkerBlock

    -- ** ListHITsForQualificationType (Paginated)
    , module Network.AWS.MechanicalTurk.ListHITsForQualificationType

    -- ** ListBonusPayments (Paginated)
    , module Network.AWS.MechanicalTurk.ListBonusPayments

    -- ** ListWorkerBlocks (Paginated)
    , module Network.AWS.MechanicalTurk.ListWorkerBlocks

    -- ** DeleteWorkerBlock
    , module Network.AWS.MechanicalTurk.DeleteWorkerBlock

    -- ** UpdateNotificationSettings
    , module Network.AWS.MechanicalTurk.UpdateNotificationSettings

    -- ** AssociateQualificationWithWorker
    , module Network.AWS.MechanicalTurk.AssociateQualificationWithWorker

    -- ** CreateHIT
    , module Network.AWS.MechanicalTurk.CreateHIT

    -- ** GetAccountBalance
    , module Network.AWS.MechanicalTurk.GetAccountBalance

    -- * Types

    -- ** AssignmentStatus
    , AssignmentStatus (..)

    -- ** Comparator
    , Comparator (..)

    -- ** EventType
    , EventType (..)

    -- ** HITAccessActions
    , HITAccessActions (..)

    -- ** HITReviewStatus
    , HITReviewStatus (..)

    -- ** HITStatus
    , HITStatus (..)

    -- ** NotificationTransport
    , NotificationTransport (..)

    -- ** NotifyWorkersFailureCode
    , NotifyWorkersFailureCode (..)

    -- ** QualificationStatus
    , QualificationStatus (..)

    -- ** QualificationTypeStatus
    , QualificationTypeStatus (..)

    -- ** ReviewActionStatus
    , ReviewActionStatus (..)

    -- ** ReviewPolicyLevel
    , ReviewPolicyLevel (..)

    -- ** ReviewableHITStatus
    , ReviewableHITStatus (..)

    -- ** Assignment
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

    -- ** BonusPayment
    , BonusPayment
    , bonusPayment
    , bpReason
    , bpGrantTime
    , bpWorkerId
    , bpAssignmentId
    , bpBonusAmount

    -- ** HIT
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

    -- ** HITLayoutParameter
    , HITLayoutParameter
    , hITLayoutParameter
    , hitlpName
    , hitlpValue

    -- ** Locale
    , Locale
    , locale
    , lSubdivision
    , lCountry

    -- ** NotificationSpecification
    , NotificationSpecification
    , notificationSpecification
    , nsDestination
    , nsTransport
    , nsVersion
    , nsEventTypes

    -- ** NotifyWorkersFailureStatus
    , NotifyWorkersFailureStatus
    , notifyWorkersFailureStatus
    , nwfsNotifyWorkersFailureMessage
    , nwfsNotifyWorkersFailureCode
    , nwfsWorkerId

    -- ** ParameterMapEntry
    , ParameterMapEntry
    , parameterMapEntry
    , pmeValues
    , pmeKey

    -- ** PolicyParameter
    , PolicyParameter
    , policyParameter
    , ppValues
    , ppMapEntries
    , ppKey

    -- ** Qualification
    , Qualification
    , qualification
    , qStatus
    , qIntegerValue
    , qLocaleValue
    , qQualificationTypeId
    , qGrantTime
    , qWorkerId

    -- ** QualificationRequest
    , QualificationRequest
    , qualificationRequest
    , quaQualificationRequestId
    , quaTest
    , quaQualificationTypeId
    , quaAnswer
    , quaWorkerId
    , quaSubmitTime

    -- ** QualificationRequirement
    , QualificationRequirement
    , qualificationRequirement
    , qrLocaleValues
    , qrActionsGuarded
    , qrRequiredToPreview
    , qrIntegerValues
    , qrQualificationTypeId
    , qrComparator

    -- ** QualificationType
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

    -- ** ReviewActionDetail
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

    -- ** ReviewPolicy
    , ReviewPolicy
    , reviewPolicy
    , rpParameters
    , rpPolicyName

    -- ** ReviewReport
    , ReviewReport
    , reviewReport
    , rrReviewActions
    , rrReviewResults

    -- ** ReviewResultDetail
    , ReviewResultDetail
    , reviewResultDetail
    , rrdValue
    , rrdActionId
    , rrdSubjectType
    , rrdKey
    , rrdQuestionId
    , rrdSubjectId

    -- ** WorkerBlock
    , WorkerBlock
    , workerBlock
    , wbReason
    , wbWorkerId
    ) where

import Network.AWS.MechanicalTurk.AcceptQualificationRequest
import Network.AWS.MechanicalTurk.ApproveAssignment
import Network.AWS.MechanicalTurk.AssociateQualificationWithWorker
import Network.AWS.MechanicalTurk.CreateAdditionalAssignmentsForHIT
import Network.AWS.MechanicalTurk.CreateHIT
import Network.AWS.MechanicalTurk.CreateHITType
import Network.AWS.MechanicalTurk.CreateHITWithHITType
import Network.AWS.MechanicalTurk.CreateQualificationType
import Network.AWS.MechanicalTurk.CreateWorkerBlock
import Network.AWS.MechanicalTurk.DeleteHIT
import Network.AWS.MechanicalTurk.DeleteQualificationType
import Network.AWS.MechanicalTurk.DeleteWorkerBlock
import Network.AWS.MechanicalTurk.DisassociateQualificationFromWorker
import Network.AWS.MechanicalTurk.GetAccountBalance
import Network.AWS.MechanicalTurk.GetAssignment
import Network.AWS.MechanicalTurk.GetFileUploadURL
import Network.AWS.MechanicalTurk.GetHIT
import Network.AWS.MechanicalTurk.GetQualificationScore
import Network.AWS.MechanicalTurk.GetQualificationType
import Network.AWS.MechanicalTurk.ListAssignmentsForHIT
import Network.AWS.MechanicalTurk.ListBonusPayments
import Network.AWS.MechanicalTurk.ListHITs
import Network.AWS.MechanicalTurk.ListHITsForQualificationType
import Network.AWS.MechanicalTurk.ListQualificationRequests
import Network.AWS.MechanicalTurk.ListQualificationTypes
import Network.AWS.MechanicalTurk.ListReviewableHITs
import Network.AWS.MechanicalTurk.ListReviewPolicyResultsForHIT
import Network.AWS.MechanicalTurk.ListWorkerBlocks
import Network.AWS.MechanicalTurk.ListWorkersWithQualificationType
import Network.AWS.MechanicalTurk.NotifyWorkers
import Network.AWS.MechanicalTurk.RejectAssignment
import Network.AWS.MechanicalTurk.RejectQualificationRequest
import Network.AWS.MechanicalTurk.SendBonus
import Network.AWS.MechanicalTurk.SendTestEventNotification
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.UpdateExpirationForHIT
import Network.AWS.MechanicalTurk.UpdateHITReviewStatus
import Network.AWS.MechanicalTurk.UpdateHITTypeOfHIT
import Network.AWS.MechanicalTurk.UpdateNotificationSettings
import Network.AWS.MechanicalTurk.UpdateQualificationType
import Network.AWS.MechanicalTurk.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'MechanicalTurk'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
