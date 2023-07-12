{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.MechanicalTurk
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-01-17@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Mechanical Turk API Reference
module Amazonka.MechanicalTurk
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** RequestError
    _RequestError,

    -- ** ServiceFault
    _ServiceFault,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AcceptQualificationRequest
    AcceptQualificationRequest (AcceptQualificationRequest'),
    newAcceptQualificationRequest,
    AcceptQualificationRequestResponse (AcceptQualificationRequestResponse'),
    newAcceptQualificationRequestResponse,

    -- ** ApproveAssignment
    ApproveAssignment (ApproveAssignment'),
    newApproveAssignment,
    ApproveAssignmentResponse (ApproveAssignmentResponse'),
    newApproveAssignmentResponse,

    -- ** AssociateQualificationWithWorker
    AssociateQualificationWithWorker (AssociateQualificationWithWorker'),
    newAssociateQualificationWithWorker,
    AssociateQualificationWithWorkerResponse (AssociateQualificationWithWorkerResponse'),
    newAssociateQualificationWithWorkerResponse,

    -- ** CreateAdditionalAssignmentsForHIT
    CreateAdditionalAssignmentsForHIT (CreateAdditionalAssignmentsForHIT'),
    newCreateAdditionalAssignmentsForHIT,
    CreateAdditionalAssignmentsForHITResponse (CreateAdditionalAssignmentsForHITResponse'),
    newCreateAdditionalAssignmentsForHITResponse,

    -- ** CreateHIT
    CreateHIT (CreateHIT'),
    newCreateHIT,
    CreateHITResponse (CreateHITResponse'),
    newCreateHITResponse,

    -- ** CreateHITType
    CreateHITType (CreateHITType'),
    newCreateHITType,
    CreateHITTypeResponse (CreateHITTypeResponse'),
    newCreateHITTypeResponse,

    -- ** CreateHITWithHITType
    CreateHITWithHITType (CreateHITWithHITType'),
    newCreateHITWithHITType,
    CreateHITWithHITTypeResponse (CreateHITWithHITTypeResponse'),
    newCreateHITWithHITTypeResponse,

    -- ** CreateQualificationType
    CreateQualificationType (CreateQualificationType'),
    newCreateQualificationType,
    CreateQualificationTypeResponse (CreateQualificationTypeResponse'),
    newCreateQualificationTypeResponse,

    -- ** CreateWorkerBlock
    CreateWorkerBlock (CreateWorkerBlock'),
    newCreateWorkerBlock,
    CreateWorkerBlockResponse (CreateWorkerBlockResponse'),
    newCreateWorkerBlockResponse,

    -- ** DeleteHIT
    DeleteHIT (DeleteHIT'),
    newDeleteHIT,
    DeleteHITResponse (DeleteHITResponse'),
    newDeleteHITResponse,

    -- ** DeleteQualificationType
    DeleteQualificationType (DeleteQualificationType'),
    newDeleteQualificationType,
    DeleteQualificationTypeResponse (DeleteQualificationTypeResponse'),
    newDeleteQualificationTypeResponse,

    -- ** DeleteWorkerBlock
    DeleteWorkerBlock (DeleteWorkerBlock'),
    newDeleteWorkerBlock,
    DeleteWorkerBlockResponse (DeleteWorkerBlockResponse'),
    newDeleteWorkerBlockResponse,

    -- ** DisassociateQualificationFromWorker
    DisassociateQualificationFromWorker (DisassociateQualificationFromWorker'),
    newDisassociateQualificationFromWorker,
    DisassociateQualificationFromWorkerResponse (DisassociateQualificationFromWorkerResponse'),
    newDisassociateQualificationFromWorkerResponse,

    -- ** GetAccountBalance
    GetAccountBalance (GetAccountBalance'),
    newGetAccountBalance,
    GetAccountBalanceResponse (GetAccountBalanceResponse'),
    newGetAccountBalanceResponse,

    -- ** GetAssignment
    GetAssignment (GetAssignment'),
    newGetAssignment,
    GetAssignmentResponse (GetAssignmentResponse'),
    newGetAssignmentResponse,

    -- ** GetFileUploadURL
    GetFileUploadURL (GetFileUploadURL'),
    newGetFileUploadURL,
    GetFileUploadURLResponse (GetFileUploadURLResponse'),
    newGetFileUploadURLResponse,

    -- ** GetHIT
    GetHIT (GetHIT'),
    newGetHIT,
    GetHITResponse (GetHITResponse'),
    newGetHITResponse,

    -- ** GetQualificationScore
    GetQualificationScore (GetQualificationScore'),
    newGetQualificationScore,
    GetQualificationScoreResponse (GetQualificationScoreResponse'),
    newGetQualificationScoreResponse,

    -- ** GetQualificationType
    GetQualificationType (GetQualificationType'),
    newGetQualificationType,
    GetQualificationTypeResponse (GetQualificationTypeResponse'),
    newGetQualificationTypeResponse,

    -- ** ListAssignmentsForHIT (Paginated)
    ListAssignmentsForHIT (ListAssignmentsForHIT'),
    newListAssignmentsForHIT,
    ListAssignmentsForHITResponse (ListAssignmentsForHITResponse'),
    newListAssignmentsForHITResponse,

    -- ** ListBonusPayments (Paginated)
    ListBonusPayments (ListBonusPayments'),
    newListBonusPayments,
    ListBonusPaymentsResponse (ListBonusPaymentsResponse'),
    newListBonusPaymentsResponse,

    -- ** ListHITs (Paginated)
    ListHITs (ListHITs'),
    newListHITs,
    ListHITsResponse (ListHITsResponse'),
    newListHITsResponse,

    -- ** ListHITsForQualificationType (Paginated)
    ListHITsForQualificationType (ListHITsForQualificationType'),
    newListHITsForQualificationType,
    ListHITsForQualificationTypeResponse (ListHITsForQualificationTypeResponse'),
    newListHITsForQualificationTypeResponse,

    -- ** ListQualificationRequests (Paginated)
    ListQualificationRequests (ListQualificationRequests'),
    newListQualificationRequests,
    ListQualificationRequestsResponse (ListQualificationRequestsResponse'),
    newListQualificationRequestsResponse,

    -- ** ListQualificationTypes (Paginated)
    ListQualificationTypes (ListQualificationTypes'),
    newListQualificationTypes,
    ListQualificationTypesResponse (ListQualificationTypesResponse'),
    newListQualificationTypesResponse,

    -- ** ListReviewPolicyResultsForHIT
    ListReviewPolicyResultsForHIT (ListReviewPolicyResultsForHIT'),
    newListReviewPolicyResultsForHIT,
    ListReviewPolicyResultsForHITResponse (ListReviewPolicyResultsForHITResponse'),
    newListReviewPolicyResultsForHITResponse,

    -- ** ListReviewableHITs (Paginated)
    ListReviewableHITs (ListReviewableHITs'),
    newListReviewableHITs,
    ListReviewableHITsResponse (ListReviewableHITsResponse'),
    newListReviewableHITsResponse,

    -- ** ListWorkerBlocks (Paginated)
    ListWorkerBlocks (ListWorkerBlocks'),
    newListWorkerBlocks,
    ListWorkerBlocksResponse (ListWorkerBlocksResponse'),
    newListWorkerBlocksResponse,

    -- ** ListWorkersWithQualificationType (Paginated)
    ListWorkersWithQualificationType (ListWorkersWithQualificationType'),
    newListWorkersWithQualificationType,
    ListWorkersWithQualificationTypeResponse (ListWorkersWithQualificationTypeResponse'),
    newListWorkersWithQualificationTypeResponse,

    -- ** NotifyWorkers
    NotifyWorkers (NotifyWorkers'),
    newNotifyWorkers,
    NotifyWorkersResponse (NotifyWorkersResponse'),
    newNotifyWorkersResponse,

    -- ** RejectAssignment
    RejectAssignment (RejectAssignment'),
    newRejectAssignment,
    RejectAssignmentResponse (RejectAssignmentResponse'),
    newRejectAssignmentResponse,

    -- ** RejectQualificationRequest
    RejectQualificationRequest (RejectQualificationRequest'),
    newRejectQualificationRequest,
    RejectQualificationRequestResponse (RejectQualificationRequestResponse'),
    newRejectQualificationRequestResponse,

    -- ** SendBonus
    SendBonus (SendBonus'),
    newSendBonus,
    SendBonusResponse (SendBonusResponse'),
    newSendBonusResponse,

    -- ** SendTestEventNotification
    SendTestEventNotification (SendTestEventNotification'),
    newSendTestEventNotification,
    SendTestEventNotificationResponse (SendTestEventNotificationResponse'),
    newSendTestEventNotificationResponse,

    -- ** UpdateExpirationForHIT
    UpdateExpirationForHIT (UpdateExpirationForHIT'),
    newUpdateExpirationForHIT,
    UpdateExpirationForHITResponse (UpdateExpirationForHITResponse'),
    newUpdateExpirationForHITResponse,

    -- ** UpdateHITReviewStatus
    UpdateHITReviewStatus (UpdateHITReviewStatus'),
    newUpdateHITReviewStatus,
    UpdateHITReviewStatusResponse (UpdateHITReviewStatusResponse'),
    newUpdateHITReviewStatusResponse,

    -- ** UpdateHITTypeOfHIT
    UpdateHITTypeOfHIT (UpdateHITTypeOfHIT'),
    newUpdateHITTypeOfHIT,
    UpdateHITTypeOfHITResponse (UpdateHITTypeOfHITResponse'),
    newUpdateHITTypeOfHITResponse,

    -- ** UpdateNotificationSettings
    UpdateNotificationSettings (UpdateNotificationSettings'),
    newUpdateNotificationSettings,
    UpdateNotificationSettingsResponse (UpdateNotificationSettingsResponse'),
    newUpdateNotificationSettingsResponse,

    -- ** UpdateQualificationType
    UpdateQualificationType (UpdateQualificationType'),
    newUpdateQualificationType,
    UpdateQualificationTypeResponse (UpdateQualificationTypeResponse'),
    newUpdateQualificationTypeResponse,

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

import Amazonka.MechanicalTurk.AcceptQualificationRequest
import Amazonka.MechanicalTurk.ApproveAssignment
import Amazonka.MechanicalTurk.AssociateQualificationWithWorker
import Amazonka.MechanicalTurk.CreateAdditionalAssignmentsForHIT
import Amazonka.MechanicalTurk.CreateHIT
import Amazonka.MechanicalTurk.CreateHITType
import Amazonka.MechanicalTurk.CreateHITWithHITType
import Amazonka.MechanicalTurk.CreateQualificationType
import Amazonka.MechanicalTurk.CreateWorkerBlock
import Amazonka.MechanicalTurk.DeleteHIT
import Amazonka.MechanicalTurk.DeleteQualificationType
import Amazonka.MechanicalTurk.DeleteWorkerBlock
import Amazonka.MechanicalTurk.DisassociateQualificationFromWorker
import Amazonka.MechanicalTurk.GetAccountBalance
import Amazonka.MechanicalTurk.GetAssignment
import Amazonka.MechanicalTurk.GetFileUploadURL
import Amazonka.MechanicalTurk.GetHIT
import Amazonka.MechanicalTurk.GetQualificationScore
import Amazonka.MechanicalTurk.GetQualificationType
import Amazonka.MechanicalTurk.Lens
import Amazonka.MechanicalTurk.ListAssignmentsForHIT
import Amazonka.MechanicalTurk.ListBonusPayments
import Amazonka.MechanicalTurk.ListHITs
import Amazonka.MechanicalTurk.ListHITsForQualificationType
import Amazonka.MechanicalTurk.ListQualificationRequests
import Amazonka.MechanicalTurk.ListQualificationTypes
import Amazonka.MechanicalTurk.ListReviewPolicyResultsForHIT
import Amazonka.MechanicalTurk.ListReviewableHITs
import Amazonka.MechanicalTurk.ListWorkerBlocks
import Amazonka.MechanicalTurk.ListWorkersWithQualificationType
import Amazonka.MechanicalTurk.NotifyWorkers
import Amazonka.MechanicalTurk.RejectAssignment
import Amazonka.MechanicalTurk.RejectQualificationRequest
import Amazonka.MechanicalTurk.SendBonus
import Amazonka.MechanicalTurk.SendTestEventNotification
import Amazonka.MechanicalTurk.Types
import Amazonka.MechanicalTurk.UpdateExpirationForHIT
import Amazonka.MechanicalTurk.UpdateHITReviewStatus
import Amazonka.MechanicalTurk.UpdateHITTypeOfHIT
import Amazonka.MechanicalTurk.UpdateNotificationSettings
import Amazonka.MechanicalTurk.UpdateQualificationType
import Amazonka.MechanicalTurk.Waiters

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
