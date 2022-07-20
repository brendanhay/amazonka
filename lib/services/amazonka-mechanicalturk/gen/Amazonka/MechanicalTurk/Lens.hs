{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MechanicalTurk.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MechanicalTurk.Lens
  ( -- * Operations

    -- ** AcceptQualificationRequest
    acceptQualificationRequest_integerValue,
    acceptQualificationRequest_qualificationRequestId,
    acceptQualificationRequestResponse_httpStatus,

    -- ** ApproveAssignment
    approveAssignment_overrideRejection,
    approveAssignment_requesterFeedback,
    approveAssignment_assignmentId,
    approveAssignmentResponse_httpStatus,

    -- ** AssociateQualificationWithWorker
    associateQualificationWithWorker_integerValue,
    associateQualificationWithWorker_sendNotification,
    associateQualificationWithWorker_qualificationTypeId,
    associateQualificationWithWorker_workerId,
    associateQualificationWithWorkerResponse_httpStatus,

    -- ** CreateAdditionalAssignmentsForHIT
    createAdditionalAssignmentsForHIT_uniqueRequestToken,
    createAdditionalAssignmentsForHIT_hITId,
    createAdditionalAssignmentsForHIT_numberOfAdditionalAssignments,
    createAdditionalAssignmentsForHITResponse_httpStatus,

    -- ** CreateHIT
    createHIT_hITLayoutParameters,
    createHIT_hITReviewPolicy,
    createHIT_maxAssignments,
    createHIT_keywords,
    createHIT_question,
    createHIT_autoApprovalDelayInSeconds,
    createHIT_qualificationRequirements,
    createHIT_uniqueRequestToken,
    createHIT_hITLayoutId,
    createHIT_assignmentReviewPolicy,
    createHIT_requesterAnnotation,
    createHIT_lifetimeInSeconds,
    createHIT_assignmentDurationInSeconds,
    createHIT_reward,
    createHIT_title,
    createHIT_description,
    createHITResponse_hit,
    createHITResponse_httpStatus,

    -- ** CreateHITType
    createHITType_keywords,
    createHITType_autoApprovalDelayInSeconds,
    createHITType_qualificationRequirements,
    createHITType_assignmentDurationInSeconds,
    createHITType_reward,
    createHITType_title,
    createHITType_description,
    createHITTypeResponse_hITTypeId,
    createHITTypeResponse_httpStatus,

    -- ** CreateHITWithHITType
    createHITWithHITType_hITLayoutParameters,
    createHITWithHITType_hITReviewPolicy,
    createHITWithHITType_maxAssignments,
    createHITWithHITType_question,
    createHITWithHITType_uniqueRequestToken,
    createHITWithHITType_hITLayoutId,
    createHITWithHITType_assignmentReviewPolicy,
    createHITWithHITType_requesterAnnotation,
    createHITWithHITType_hITTypeId,
    createHITWithHITType_lifetimeInSeconds,
    createHITWithHITTypeResponse_hit,
    createHITWithHITTypeResponse_httpStatus,

    -- ** CreateQualificationType
    createQualificationType_autoGrantedValue,
    createQualificationType_testDurationInSeconds,
    createQualificationType_keywords,
    createQualificationType_test,
    createQualificationType_retryDelayInSeconds,
    createQualificationType_answerKey,
    createQualificationType_autoGranted,
    createQualificationType_name,
    createQualificationType_description,
    createQualificationType_qualificationTypeStatus,
    createQualificationTypeResponse_qualificationType,
    createQualificationTypeResponse_httpStatus,

    -- ** CreateWorkerBlock
    createWorkerBlock_workerId,
    createWorkerBlock_reason,
    createWorkerBlockResponse_httpStatus,

    -- ** DeleteHIT
    deleteHIT_hITId,
    deleteHITResponse_httpStatus,

    -- ** DeleteQualificationType
    deleteQualificationType_qualificationTypeId,
    deleteQualificationTypeResponse_httpStatus,

    -- ** DeleteWorkerBlock
    deleteWorkerBlock_reason,
    deleteWorkerBlock_workerId,
    deleteWorkerBlockResponse_httpStatus,

    -- ** DisassociateQualificationFromWorker
    disassociateQualificationFromWorker_reason,
    disassociateQualificationFromWorker_workerId,
    disassociateQualificationFromWorker_qualificationTypeId,
    disassociateQualificationFromWorkerResponse_httpStatus,

    -- ** GetAccountBalance
    getAccountBalanceResponse_availableBalance,
    getAccountBalanceResponse_onHoldBalance,
    getAccountBalanceResponse_httpStatus,

    -- ** GetAssignment
    getAssignment_assignmentId,
    getAssignmentResponse_hit,
    getAssignmentResponse_assignment,
    getAssignmentResponse_httpStatus,

    -- ** GetFileUploadURL
    getFileUploadURL_assignmentId,
    getFileUploadURL_questionIdentifier,
    getFileUploadURLResponse_fileUploadURL,
    getFileUploadURLResponse_httpStatus,

    -- ** GetHIT
    getHIT_hITId,
    getHITResponse_hit,
    getHITResponse_httpStatus,

    -- ** GetQualificationScore
    getQualificationScore_qualificationTypeId,
    getQualificationScore_workerId,
    getQualificationScoreResponse_qualification,
    getQualificationScoreResponse_httpStatus,

    -- ** GetQualificationType
    getQualificationType_qualificationTypeId,
    getQualificationTypeResponse_qualificationType,
    getQualificationTypeResponse_httpStatus,

    -- ** ListAssignmentsForHIT
    listAssignmentsForHIT_nextToken,
    listAssignmentsForHIT_assignmentStatuses,
    listAssignmentsForHIT_maxResults,
    listAssignmentsForHIT_hITId,
    listAssignmentsForHITResponse_nextToken,
    listAssignmentsForHITResponse_numResults,
    listAssignmentsForHITResponse_assignments,
    listAssignmentsForHITResponse_httpStatus,

    -- ** ListBonusPayments
    listBonusPayments_nextToken,
    listBonusPayments_hITId,
    listBonusPayments_maxResults,
    listBonusPayments_assignmentId,
    listBonusPaymentsResponse_nextToken,
    listBonusPaymentsResponse_numResults,
    listBonusPaymentsResponse_bonusPayments,
    listBonusPaymentsResponse_httpStatus,

    -- ** ListHITs
    listHITs_nextToken,
    listHITs_maxResults,
    listHITsResponse_nextToken,
    listHITsResponse_numResults,
    listHITsResponse_hITs,
    listHITsResponse_httpStatus,

    -- ** ListHITsForQualificationType
    listHITsForQualificationType_nextToken,
    listHITsForQualificationType_maxResults,
    listHITsForQualificationType_qualificationTypeId,
    listHITsForQualificationTypeResponse_nextToken,
    listHITsForQualificationTypeResponse_numResults,
    listHITsForQualificationTypeResponse_hITs,
    listHITsForQualificationTypeResponse_httpStatus,

    -- ** ListQualificationRequests
    listQualificationRequests_qualificationTypeId,
    listQualificationRequests_nextToken,
    listQualificationRequests_maxResults,
    listQualificationRequestsResponse_nextToken,
    listQualificationRequestsResponse_numResults,
    listQualificationRequestsResponse_qualificationRequests,
    listQualificationRequestsResponse_httpStatus,

    -- ** ListQualificationTypes
    listQualificationTypes_nextToken,
    listQualificationTypes_mustBeOwnedByCaller,
    listQualificationTypes_maxResults,
    listQualificationTypes_query,
    listQualificationTypes_mustBeRequestable,
    listQualificationTypesResponse_nextToken,
    listQualificationTypesResponse_numResults,
    listQualificationTypesResponse_qualificationTypes,
    listQualificationTypesResponse_httpStatus,

    -- ** ListReviewPolicyResultsForHIT
    listReviewPolicyResultsForHIT_nextToken,
    listReviewPolicyResultsForHIT_policyLevels,
    listReviewPolicyResultsForHIT_retrieveActions,
    listReviewPolicyResultsForHIT_maxResults,
    listReviewPolicyResultsForHIT_retrieveResults,
    listReviewPolicyResultsForHIT_hITId,
    listReviewPolicyResultsForHITResponse_nextToken,
    listReviewPolicyResultsForHITResponse_hITReviewPolicy,
    listReviewPolicyResultsForHITResponse_hITId,
    listReviewPolicyResultsForHITResponse_hITReviewReport,
    listReviewPolicyResultsForHITResponse_assignmentReviewReport,
    listReviewPolicyResultsForHITResponse_assignmentReviewPolicy,
    listReviewPolicyResultsForHITResponse_httpStatus,

    -- ** ListReviewableHITs
    listReviewableHITs_nextToken,
    listReviewableHITs_status,
    listReviewableHITs_maxResults,
    listReviewableHITs_hITTypeId,
    listReviewableHITsResponse_nextToken,
    listReviewableHITsResponse_numResults,
    listReviewableHITsResponse_hITs,
    listReviewableHITsResponse_httpStatus,

    -- ** ListWorkerBlocks
    listWorkerBlocks_nextToken,
    listWorkerBlocks_maxResults,
    listWorkerBlocksResponse_nextToken,
    listWorkerBlocksResponse_numResults,
    listWorkerBlocksResponse_workerBlocks,
    listWorkerBlocksResponse_httpStatus,

    -- ** ListWorkersWithQualificationType
    listWorkersWithQualificationType_nextToken,
    listWorkersWithQualificationType_status,
    listWorkersWithQualificationType_maxResults,
    listWorkersWithQualificationType_qualificationTypeId,
    listWorkersWithQualificationTypeResponse_nextToken,
    listWorkersWithQualificationTypeResponse_numResults,
    listWorkersWithQualificationTypeResponse_qualifications,
    listWorkersWithQualificationTypeResponse_httpStatus,

    -- ** NotifyWorkers
    notifyWorkers_subject,
    notifyWorkers_messageText,
    notifyWorkers_workerIds,
    notifyWorkersResponse_notifyWorkersFailureStatuses,
    notifyWorkersResponse_httpStatus,

    -- ** RejectAssignment
    rejectAssignment_assignmentId,
    rejectAssignment_requesterFeedback,
    rejectAssignmentResponse_httpStatus,

    -- ** RejectQualificationRequest
    rejectQualificationRequest_reason,
    rejectQualificationRequest_qualificationRequestId,
    rejectQualificationRequestResponse_httpStatus,

    -- ** SendBonus
    sendBonus_uniqueRequestToken,
    sendBonus_workerId,
    sendBonus_bonusAmount,
    sendBonus_assignmentId,
    sendBonus_reason,
    sendBonusResponse_httpStatus,

    -- ** SendTestEventNotification
    sendTestEventNotification_notification,
    sendTestEventNotification_testEventType,
    sendTestEventNotificationResponse_httpStatus,

    -- ** UpdateExpirationForHIT
    updateExpirationForHIT_hITId,
    updateExpirationForHIT_expireAt,
    updateExpirationForHITResponse_httpStatus,

    -- ** UpdateHITReviewStatus
    updateHITReviewStatus_revert,
    updateHITReviewStatus_hITId,
    updateHITReviewStatusResponse_httpStatus,

    -- ** UpdateHITTypeOfHIT
    updateHITTypeOfHIT_hITId,
    updateHITTypeOfHIT_hITTypeId,
    updateHITTypeOfHITResponse_httpStatus,

    -- ** UpdateNotificationSettings
    updateNotificationSettings_active,
    updateNotificationSettings_notification,
    updateNotificationSettings_hITTypeId,
    updateNotificationSettingsResponse_httpStatus,

    -- ** UpdateQualificationType
    updateQualificationType_autoGrantedValue,
    updateQualificationType_testDurationInSeconds,
    updateQualificationType_description,
    updateQualificationType_test,
    updateQualificationType_retryDelayInSeconds,
    updateQualificationType_answerKey,
    updateQualificationType_autoGranted,
    updateQualificationType_qualificationTypeStatus,
    updateQualificationType_qualificationTypeId,
    updateQualificationTypeResponse_qualificationType,
    updateQualificationTypeResponse_httpStatus,

    -- * Types

    -- ** Assignment
    assignment_approvalTime,
    assignment_submitTime,
    assignment_workerId,
    assignment_deadline,
    assignment_requesterFeedback,
    assignment_autoApprovalTime,
    assignment_hITId,
    assignment_answer,
    assignment_assignmentId,
    assignment_rejectionTime,
    assignment_assignmentStatus,
    assignment_acceptTime,

    -- ** BonusPayment
    bonusPayment_workerId,
    bonusPayment_grantTime,
    bonusPayment_reason,
    bonusPayment_assignmentId,
    bonusPayment_bonusAmount,

    -- ** HIT
    hit_hITGroupId,
    hit_expiration,
    hit_numberOfAssignmentsAvailable,
    hit_hITStatus,
    hit_maxAssignments,
    hit_keywords,
    hit_numberOfAssignmentsPending,
    hit_description,
    hit_assignmentDurationInSeconds,
    hit_question,
    hit_title,
    hit_hITId,
    hit_autoApprovalDelayInSeconds,
    hit_reward,
    hit_qualificationRequirements,
    hit_hITTypeId,
    hit_creationTime,
    hit_hITReviewStatus,
    hit_hITLayoutId,
    hit_numberOfAssignmentsCompleted,
    hit_requesterAnnotation,

    -- ** HITLayoutParameter
    hITLayoutParameter_name,
    hITLayoutParameter_value,

    -- ** Locale
    locale_subdivision,
    locale_country,

    -- ** NotificationSpecification
    notificationSpecification_destination,
    notificationSpecification_transport,
    notificationSpecification_version,
    notificationSpecification_eventTypes,

    -- ** NotifyWorkersFailureStatus
    notifyWorkersFailureStatus_workerId,
    notifyWorkersFailureStatus_notifyWorkersFailureCode,
    notifyWorkersFailureStatus_notifyWorkersFailureMessage,

    -- ** ParameterMapEntry
    parameterMapEntry_key,
    parameterMapEntry_values,

    -- ** PolicyParameter
    policyParameter_key,
    policyParameter_mapEntries,
    policyParameter_values,

    -- ** Qualification
    qualification_qualificationTypeId,
    qualification_integerValue,
    qualification_workerId,
    qualification_status,
    qualification_grantTime,
    qualification_localeValue,

    -- ** QualificationRequest
    qualificationRequest_qualificationTypeId,
    qualificationRequest_submitTime,
    qualificationRequest_workerId,
    qualificationRequest_answer,
    qualificationRequest_test,
    qualificationRequest_qualificationRequestId,

    -- ** QualificationRequirement
    qualificationRequirement_integerValues,
    qualificationRequirement_localeValues,
    qualificationRequirement_actionsGuarded,
    qualificationRequirement_requiredToPreview,
    qualificationRequirement_qualificationTypeId,
    qualificationRequirement_comparator,

    -- ** QualificationType
    qualificationType_qualificationTypeId,
    qualificationType_autoGrantedValue,
    qualificationType_name,
    qualificationType_testDurationInSeconds,
    qualificationType_keywords,
    qualificationType_description,
    qualificationType_test,
    qualificationType_retryDelayInSeconds,
    qualificationType_creationTime,
    qualificationType_answerKey,
    qualificationType_autoGranted,
    qualificationType_isRequestable,
    qualificationType_qualificationTypeStatus,

    -- ** ReviewActionDetail
    reviewActionDetail_actionName,
    reviewActionDetail_targetId,
    reviewActionDetail_status,
    reviewActionDetail_completeTime,
    reviewActionDetail_targetType,
    reviewActionDetail_actionId,
    reviewActionDetail_errorCode,
    reviewActionDetail_result,

    -- ** ReviewPolicy
    reviewPolicy_parameters,
    reviewPolicy_policyName,

    -- ** ReviewReport
    reviewReport_reviewResults,
    reviewReport_reviewActions,

    -- ** ReviewResultDetail
    reviewResultDetail_subjectId,
    reviewResultDetail_key,
    reviewResultDetail_questionId,
    reviewResultDetail_actionId,
    reviewResultDetail_subjectType,
    reviewResultDetail_value,

    -- ** WorkerBlock
    workerBlock_workerId,
    workerBlock_reason,
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
import Amazonka.MechanicalTurk.Types.Assignment
import Amazonka.MechanicalTurk.Types.BonusPayment
import Amazonka.MechanicalTurk.Types.HIT
import Amazonka.MechanicalTurk.Types.HITLayoutParameter
import Amazonka.MechanicalTurk.Types.Locale
import Amazonka.MechanicalTurk.Types.NotificationSpecification
import Amazonka.MechanicalTurk.Types.NotifyWorkersFailureStatus
import Amazonka.MechanicalTurk.Types.ParameterMapEntry
import Amazonka.MechanicalTurk.Types.PolicyParameter
import Amazonka.MechanicalTurk.Types.Qualification
import Amazonka.MechanicalTurk.Types.QualificationRequest
import Amazonka.MechanicalTurk.Types.QualificationRequirement
import Amazonka.MechanicalTurk.Types.QualificationType
import Amazonka.MechanicalTurk.Types.ReviewActionDetail
import Amazonka.MechanicalTurk.Types.ReviewPolicy
import Amazonka.MechanicalTurk.Types.ReviewReport
import Amazonka.MechanicalTurk.Types.ReviewResultDetail
import Amazonka.MechanicalTurk.Types.WorkerBlock
import Amazonka.MechanicalTurk.UpdateExpirationForHIT
import Amazonka.MechanicalTurk.UpdateHITReviewStatus
import Amazonka.MechanicalTurk.UpdateHITTypeOfHIT
import Amazonka.MechanicalTurk.UpdateNotificationSettings
import Amazonka.MechanicalTurk.UpdateQualificationType
