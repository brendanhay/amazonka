{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Mechanical Turk API Reference
module Network.AWS.MechanicalTurk
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ServiceFault
    _ServiceFault,

    -- ** RequestError
    _RequestError,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetQualificationScore
    GetQualificationScore (GetQualificationScore'),
    newGetQualificationScore,
    GetQualificationScoreResponse (GetQualificationScoreResponse'),
    newGetQualificationScoreResponse,

    -- ** ListReviewableHITs (Paginated)
    ListReviewableHITs (ListReviewableHITs'),
    newListReviewableHITs,
    ListReviewableHITsResponse (ListReviewableHITsResponse'),
    newListReviewableHITsResponse,

    -- ** ListHITs (Paginated)
    ListHITs (ListHITs'),
    newListHITs,
    ListHITsResponse (ListHITsResponse'),
    newListHITsResponse,

    -- ** ListAssignmentsForHIT (Paginated)
    ListAssignmentsForHIT (ListAssignmentsForHIT'),
    newListAssignmentsForHIT,
    ListAssignmentsForHITResponse (ListAssignmentsForHITResponse'),
    newListAssignmentsForHITResponse,

    -- ** ListWorkersWithQualificationType (Paginated)
    ListWorkersWithQualificationType (ListWorkersWithQualificationType'),
    newListWorkersWithQualificationType,
    ListWorkersWithQualificationTypeResponse (ListWorkersWithQualificationTypeResponse'),
    newListWorkersWithQualificationTypeResponse,

    -- ** GetAccountBalance
    GetAccountBalance (GetAccountBalance'),
    newGetAccountBalance,
    GetAccountBalanceResponse (GetAccountBalanceResponse'),
    newGetAccountBalanceResponse,

    -- ** CreateHIT
    CreateHIT (CreateHIT'),
    newCreateHIT,
    CreateHITResponse (CreateHITResponse'),
    newCreateHITResponse,

    -- ** NotifyWorkers
    NotifyWorkers (NotifyWorkers'),
    newNotifyWorkers,
    NotifyWorkersResponse (NotifyWorkersResponse'),
    newNotifyWorkersResponse,

    -- ** ListWorkerBlocks (Paginated)
    ListWorkerBlocks (ListWorkerBlocks'),
    newListWorkerBlocks,
    ListWorkerBlocksResponse (ListWorkerBlocksResponse'),
    newListWorkerBlocksResponse,

    -- ** ListHITsForQualificationType (Paginated)
    ListHITsForQualificationType (ListHITsForQualificationType'),
    newListHITsForQualificationType,
    ListHITsForQualificationTypeResponse (ListHITsForQualificationTypeResponse'),
    newListHITsForQualificationTypeResponse,

    -- ** CreateWorkerBlock
    CreateWorkerBlock (CreateWorkerBlock'),
    newCreateWorkerBlock,
    CreateWorkerBlockResponse (CreateWorkerBlockResponse'),
    newCreateWorkerBlockResponse,

    -- ** CreateAdditionalAssignmentsForHIT
    CreateAdditionalAssignmentsForHIT (CreateAdditionalAssignmentsForHIT'),
    newCreateAdditionalAssignmentsForHIT,
    CreateAdditionalAssignmentsForHITResponse (CreateAdditionalAssignmentsForHITResponse'),
    newCreateAdditionalAssignmentsForHITResponse,

    -- ** ListQualificationTypes (Paginated)
    ListQualificationTypes (ListQualificationTypes'),
    newListQualificationTypes,
    ListQualificationTypesResponse (ListQualificationTypesResponse'),
    newListQualificationTypesResponse,

    -- ** UpdateHITTypeOfHIT
    UpdateHITTypeOfHIT (UpdateHITTypeOfHIT'),
    newUpdateHITTypeOfHIT,
    UpdateHITTypeOfHITResponse (UpdateHITTypeOfHITResponse'),
    newUpdateHITTypeOfHITResponse,

    -- ** GetAssignment
    GetAssignment (GetAssignment'),
    newGetAssignment,
    GetAssignmentResponse (GetAssignmentResponse'),
    newGetAssignmentResponse,

    -- ** UpdateHITReviewStatus
    UpdateHITReviewStatus (UpdateHITReviewStatus'),
    newUpdateHITReviewStatus,
    UpdateHITReviewStatusResponse (UpdateHITReviewStatusResponse'),
    newUpdateHITReviewStatusResponse,

    -- ** RejectQualificationRequest
    RejectQualificationRequest (RejectQualificationRequest'),
    newRejectQualificationRequest,
    RejectQualificationRequestResponse (RejectQualificationRequestResponse'),
    newRejectQualificationRequestResponse,

    -- ** GetQualificationType
    GetQualificationType (GetQualificationType'),
    newGetQualificationType,
    GetQualificationTypeResponse (GetQualificationTypeResponse'),
    newGetQualificationTypeResponse,

    -- ** RejectAssignment
    RejectAssignment (RejectAssignment'),
    newRejectAssignment,
    RejectAssignmentResponse (RejectAssignmentResponse'),
    newRejectAssignmentResponse,

    -- ** UpdateExpirationForHIT
    UpdateExpirationForHIT (UpdateExpirationForHIT'),
    newUpdateExpirationForHIT,
    UpdateExpirationForHITResponse (UpdateExpirationForHITResponse'),
    newUpdateExpirationForHITResponse,

    -- ** ApproveAssignment
    ApproveAssignment (ApproveAssignment'),
    newApproveAssignment,
    ApproveAssignmentResponse (ApproveAssignmentResponse'),
    newApproveAssignmentResponse,

    -- ** DeleteHIT
    DeleteHIT (DeleteHIT'),
    newDeleteHIT,
    DeleteHITResponse (DeleteHITResponse'),
    newDeleteHITResponse,

    -- ** ListReviewPolicyResultsForHIT
    ListReviewPolicyResultsForHIT (ListReviewPolicyResultsForHIT'),
    newListReviewPolicyResultsForHIT,
    ListReviewPolicyResultsForHITResponse (ListReviewPolicyResultsForHITResponse'),
    newListReviewPolicyResultsForHITResponse,

    -- ** CreateHITType
    CreateHITType (CreateHITType'),
    newCreateHITType,
    CreateHITTypeResponse (CreateHITTypeResponse'),
    newCreateHITTypeResponse,

    -- ** UpdateNotificationSettings
    UpdateNotificationSettings (UpdateNotificationSettings'),
    newUpdateNotificationSettings,
    UpdateNotificationSettingsResponse (UpdateNotificationSettingsResponse'),
    newUpdateNotificationSettingsResponse,

    -- ** SendBonus
    SendBonus (SendBonus'),
    newSendBonus,
    SendBonusResponse (SendBonusResponse'),
    newSendBonusResponse,

    -- ** ListQualificationRequests (Paginated)
    ListQualificationRequests (ListQualificationRequests'),
    newListQualificationRequests,
    ListQualificationRequestsResponse (ListQualificationRequestsResponse'),
    newListQualificationRequestsResponse,

    -- ** AssociateQualificationWithWorker
    AssociateQualificationWithWorker (AssociateQualificationWithWorker'),
    newAssociateQualificationWithWorker,
    AssociateQualificationWithWorkerResponse (AssociateQualificationWithWorkerResponse'),
    newAssociateQualificationWithWorkerResponse,

    -- ** CreateHITWithHITType
    CreateHITWithHITType (CreateHITWithHITType'),
    newCreateHITWithHITType,
    CreateHITWithHITTypeResponse (CreateHITWithHITTypeResponse'),
    newCreateHITWithHITTypeResponse,

    -- ** DeleteWorkerBlock
    DeleteWorkerBlock (DeleteWorkerBlock'),
    newDeleteWorkerBlock,
    DeleteWorkerBlockResponse (DeleteWorkerBlockResponse'),
    newDeleteWorkerBlockResponse,

    -- ** ListBonusPayments (Paginated)
    ListBonusPayments (ListBonusPayments'),
    newListBonusPayments,
    ListBonusPaymentsResponse (ListBonusPaymentsResponse'),
    newListBonusPaymentsResponse,

    -- ** DisassociateQualificationFromWorker
    DisassociateQualificationFromWorker (DisassociateQualificationFromWorker'),
    newDisassociateQualificationFromWorker,
    DisassociateQualificationFromWorkerResponse (DisassociateQualificationFromWorkerResponse'),
    newDisassociateQualificationFromWorkerResponse,

    -- ** SendTestEventNotification
    SendTestEventNotification (SendTestEventNotification'),
    newSendTestEventNotification,
    SendTestEventNotificationResponse (SendTestEventNotificationResponse'),
    newSendTestEventNotificationResponse,

    -- ** GetHIT
    GetHIT (GetHIT'),
    newGetHIT,
    GetHITResponse (GetHITResponse'),
    newGetHITResponse,

    -- ** UpdateQualificationType
    UpdateQualificationType (UpdateQualificationType'),
    newUpdateQualificationType,
    UpdateQualificationTypeResponse (UpdateQualificationTypeResponse'),
    newUpdateQualificationTypeResponse,

    -- ** AcceptQualificationRequest
    AcceptQualificationRequest (AcceptQualificationRequest'),
    newAcceptQualificationRequest,
    AcceptQualificationRequestResponse (AcceptQualificationRequestResponse'),
    newAcceptQualificationRequestResponse,

    -- ** DeleteQualificationType
    DeleteQualificationType (DeleteQualificationType'),
    newDeleteQualificationType,
    DeleteQualificationTypeResponse (DeleteQualificationTypeResponse'),
    newDeleteQualificationTypeResponse,

    -- ** GetFileUploadURL
    GetFileUploadURL (GetFileUploadURL'),
    newGetFileUploadURL,
    GetFileUploadURLResponse (GetFileUploadURLResponse'),
    newGetFileUploadURLResponse,

    -- ** CreateQualificationType
    CreateQualificationType (CreateQualificationType'),
    newCreateQualificationType,
    CreateQualificationTypeResponse (CreateQualificationTypeResponse'),
    newCreateQualificationTypeResponse,

    -- * Types

    -- ** AssignmentStatus
    AssignmentStatus (..),

    -- ** Comparator
    Comparator (..),

    -- ** EventType
    EventType (..),

    -- ** HITAccessActions
    HITAccessActions (..),

    -- ** HITReviewStatus
    HITReviewStatus (..),

    -- ** HITStatus
    HITStatus (..),

    -- ** NotificationTransport
    NotificationTransport (..),

    -- ** NotifyWorkersFailureCode
    NotifyWorkersFailureCode (..),

    -- ** QualificationStatus
    QualificationStatus (..),

    -- ** QualificationTypeStatus
    QualificationTypeStatus (..),

    -- ** ReviewActionStatus
    ReviewActionStatus (..),

    -- ** ReviewPolicyLevel
    ReviewPolicyLevel (..),

    -- ** ReviewableHITStatus
    ReviewableHITStatus (..),

    -- ** Assignment
    Assignment (Assignment'),
    newAssignment,

    -- ** BonusPayment
    BonusPayment (BonusPayment'),
    newBonusPayment,

    -- ** HIT
    HIT (HIT'),
    newHIT,

    -- ** HITLayoutParameter
    HITLayoutParameter (HITLayoutParameter'),
    newHITLayoutParameter,

    -- ** Locale
    Locale (Locale'),
    newLocale,

    -- ** NotificationSpecification
    NotificationSpecification (NotificationSpecification'),
    newNotificationSpecification,

    -- ** NotifyWorkersFailureStatus
    NotifyWorkersFailureStatus (NotifyWorkersFailureStatus'),
    newNotifyWorkersFailureStatus,

    -- ** ParameterMapEntry
    ParameterMapEntry (ParameterMapEntry'),
    newParameterMapEntry,

    -- ** PolicyParameter
    PolicyParameter (PolicyParameter'),
    newPolicyParameter,

    -- ** Qualification
    Qualification (Qualification'),
    newQualification,

    -- ** QualificationRequest
    QualificationRequest (QualificationRequest'),
    newQualificationRequest,

    -- ** QualificationRequirement
    QualificationRequirement (QualificationRequirement'),
    newQualificationRequirement,

    -- ** QualificationType
    QualificationType (QualificationType'),
    newQualificationType,

    -- ** ReviewActionDetail
    ReviewActionDetail (ReviewActionDetail'),
    newReviewActionDetail,

    -- ** ReviewPolicy
    ReviewPolicy (ReviewPolicy'),
    newReviewPolicy,

    -- ** ReviewReport
    ReviewReport (ReviewReport'),
    newReviewReport,

    -- ** ReviewResultDetail
    ReviewResultDetail (ReviewResultDetail'),
    newReviewResultDetail,

    -- ** WorkerBlock
    WorkerBlock (WorkerBlock'),
    newWorkerBlock,
  )
where

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
import Network.AWS.MechanicalTurk.Lens
import Network.AWS.MechanicalTurk.ListAssignmentsForHIT
import Network.AWS.MechanicalTurk.ListBonusPayments
import Network.AWS.MechanicalTurk.ListHITs
import Network.AWS.MechanicalTurk.ListHITsForQualificationType
import Network.AWS.MechanicalTurk.ListQualificationRequests
import Network.AWS.MechanicalTurk.ListQualificationTypes
import Network.AWS.MechanicalTurk.ListReviewPolicyResultsForHIT
import Network.AWS.MechanicalTurk.ListReviewableHITs
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

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'MechanicalTurk'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
