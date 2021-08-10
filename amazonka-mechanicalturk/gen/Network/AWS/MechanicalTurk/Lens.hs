{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Lens
  ( -- * Operations

    -- ** GetQualificationScore
    getQualificationScore_qualificationTypeId,
    getQualificationScore_workerId,
    getQualificationScoreResponse_qualification,
    getQualificationScoreResponse_httpStatus,

    -- ** ListReviewableHITs
    listReviewableHITs_status,
    listReviewableHITs_nextToken,
    listReviewableHITs_maxResults,
    listReviewableHITs_hITTypeId,
    listReviewableHITsResponse_nextToken,
    listReviewableHITsResponse_hITs,
    listReviewableHITsResponse_numResults,
    listReviewableHITsResponse_httpStatus,

    -- ** ListHITs
    listHITs_nextToken,
    listHITs_maxResults,
    listHITsResponse_nextToken,
    listHITsResponse_hITs,
    listHITsResponse_numResults,
    listHITsResponse_httpStatus,

    -- ** ListAssignmentsForHIT
    listAssignmentsForHIT_nextToken,
    listAssignmentsForHIT_assignmentStatuses,
    listAssignmentsForHIT_maxResults,
    listAssignmentsForHIT_hITId,
    listAssignmentsForHITResponse_nextToken,
    listAssignmentsForHITResponse_assignments,
    listAssignmentsForHITResponse_numResults,
    listAssignmentsForHITResponse_httpStatus,

    -- ** ListWorkersWithQualificationType
    listWorkersWithQualificationType_status,
    listWorkersWithQualificationType_nextToken,
    listWorkersWithQualificationType_maxResults,
    listWorkersWithQualificationType_qualificationTypeId,
    listWorkersWithQualificationTypeResponse_nextToken,
    listWorkersWithQualificationTypeResponse_numResults,
    listWorkersWithQualificationTypeResponse_qualifications,
    listWorkersWithQualificationTypeResponse_httpStatus,

    -- ** GetAccountBalance
    getAccountBalanceResponse_onHoldBalance,
    getAccountBalanceResponse_availableBalance,
    getAccountBalanceResponse_httpStatus,

    -- ** CreateHIT
    createHIT_uniqueRequestToken,
    createHIT_autoApprovalDelayInSeconds,
    createHIT_question,
    createHIT_hITLayoutId,
    createHIT_hITReviewPolicy,
    createHIT_maxAssignments,
    createHIT_requesterAnnotation,
    createHIT_assignmentReviewPolicy,
    createHIT_hITLayoutParameters,
    createHIT_qualificationRequirements,
    createHIT_keywords,
    createHIT_lifetimeInSeconds,
    createHIT_assignmentDurationInSeconds,
    createHIT_reward,
    createHIT_title,
    createHIT_description,
    createHITResponse_hit,
    createHITResponse_httpStatus,

    -- ** NotifyWorkers
    notifyWorkers_subject,
    notifyWorkers_messageText,
    notifyWorkers_workerIds,
    notifyWorkersResponse_notifyWorkersFailureStatuses,
    notifyWorkersResponse_httpStatus,

    -- ** ListWorkerBlocks
    listWorkerBlocks_nextToken,
    listWorkerBlocks_maxResults,
    listWorkerBlocksResponse_nextToken,
    listWorkerBlocksResponse_workerBlocks,
    listWorkerBlocksResponse_numResults,
    listWorkerBlocksResponse_httpStatus,

    -- ** ListHITsForQualificationType
    listHITsForQualificationType_nextToken,
    listHITsForQualificationType_maxResults,
    listHITsForQualificationType_qualificationTypeId,
    listHITsForQualificationTypeResponse_nextToken,
    listHITsForQualificationTypeResponse_hITs,
    listHITsForQualificationTypeResponse_numResults,
    listHITsForQualificationTypeResponse_httpStatus,

    -- ** CreateWorkerBlock
    createWorkerBlock_workerId,
    createWorkerBlock_reason,
    createWorkerBlockResponse_httpStatus,

    -- ** CreateAdditionalAssignmentsForHIT
    createAdditionalAssignmentsForHIT_uniqueRequestToken,
    createAdditionalAssignmentsForHIT_hITId,
    createAdditionalAssignmentsForHIT_numberOfAdditionalAssignments,
    createAdditionalAssignmentsForHITResponse_httpStatus,

    -- ** ListQualificationTypes
    listQualificationTypes_nextToken,
    listQualificationTypes_maxResults,
    listQualificationTypes_query,
    listQualificationTypes_mustBeOwnedByCaller,
    listQualificationTypes_mustBeRequestable,
    listQualificationTypesResponse_nextToken,
    listQualificationTypesResponse_qualificationTypes,
    listQualificationTypesResponse_numResults,
    listQualificationTypesResponse_httpStatus,

    -- ** UpdateHITTypeOfHIT
    updateHITTypeOfHIT_hITId,
    updateHITTypeOfHIT_hITTypeId,
    updateHITTypeOfHITResponse_httpStatus,

    -- ** GetAssignment
    getAssignment_assignmentId,
    getAssignmentResponse_hit,
    getAssignmentResponse_assignment,
    getAssignmentResponse_httpStatus,

    -- ** UpdateHITReviewStatus
    updateHITReviewStatus_revert,
    updateHITReviewStatus_hITId,
    updateHITReviewStatusResponse_httpStatus,

    -- ** RejectQualificationRequest
    rejectQualificationRequest_reason,
    rejectQualificationRequest_qualificationRequestId,
    rejectQualificationRequestResponse_httpStatus,

    -- ** GetQualificationType
    getQualificationType_qualificationTypeId,
    getQualificationTypeResponse_qualificationType,
    getQualificationTypeResponse_httpStatus,

    -- ** RejectAssignment
    rejectAssignment_assignmentId,
    rejectAssignment_requesterFeedback,
    rejectAssignmentResponse_httpStatus,

    -- ** UpdateExpirationForHIT
    updateExpirationForHIT_hITId,
    updateExpirationForHIT_expireAt,
    updateExpirationForHITResponse_httpStatus,

    -- ** ApproveAssignment
    approveAssignment_requesterFeedback,
    approveAssignment_overrideRejection,
    approveAssignment_assignmentId,
    approveAssignmentResponse_httpStatus,

    -- ** DeleteHIT
    deleteHIT_hITId,
    deleteHITResponse_httpStatus,

    -- ** ListReviewPolicyResultsForHIT
    listReviewPolicyResultsForHIT_nextToken,
    listReviewPolicyResultsForHIT_maxResults,
    listReviewPolicyResultsForHIT_retrieveResults,
    listReviewPolicyResultsForHIT_retrieveActions,
    listReviewPolicyResultsForHIT_policyLevels,
    listReviewPolicyResultsForHIT_hITId,
    listReviewPolicyResultsForHITResponse_nextToken,
    listReviewPolicyResultsForHITResponse_hITId,
    listReviewPolicyResultsForHITResponse_hITReviewPolicy,
    listReviewPolicyResultsForHITResponse_assignmentReviewReport,
    listReviewPolicyResultsForHITResponse_hITReviewReport,
    listReviewPolicyResultsForHITResponse_assignmentReviewPolicy,
    listReviewPolicyResultsForHITResponse_httpStatus,

    -- ** CreateHITType
    createHITType_autoApprovalDelayInSeconds,
    createHITType_qualificationRequirements,
    createHITType_keywords,
    createHITType_assignmentDurationInSeconds,
    createHITType_reward,
    createHITType_title,
    createHITType_description,
    createHITTypeResponse_hITTypeId,
    createHITTypeResponse_httpStatus,

    -- ** UpdateNotificationSettings
    updateNotificationSettings_active,
    updateNotificationSettings_notification,
    updateNotificationSettings_hITTypeId,
    updateNotificationSettingsResponse_httpStatus,

    -- ** SendBonus
    sendBonus_uniqueRequestToken,
    sendBonus_workerId,
    sendBonus_bonusAmount,
    sendBonus_assignmentId,
    sendBonus_reason,
    sendBonusResponse_httpStatus,

    -- ** ListQualificationRequests
    listQualificationRequests_qualificationTypeId,
    listQualificationRequests_nextToken,
    listQualificationRequests_maxResults,
    listQualificationRequestsResponse_nextToken,
    listQualificationRequestsResponse_numResults,
    listQualificationRequestsResponse_qualificationRequests,
    listQualificationRequestsResponse_httpStatus,

    -- ** AssociateQualificationWithWorker
    associateQualificationWithWorker_sendNotification,
    associateQualificationWithWorker_integerValue,
    associateQualificationWithWorker_qualificationTypeId,
    associateQualificationWithWorker_workerId,
    associateQualificationWithWorkerResponse_httpStatus,

    -- ** CreateHITWithHITType
    createHITWithHITType_uniqueRequestToken,
    createHITWithHITType_question,
    createHITWithHITType_hITLayoutId,
    createHITWithHITType_hITReviewPolicy,
    createHITWithHITType_maxAssignments,
    createHITWithHITType_requesterAnnotation,
    createHITWithHITType_assignmentReviewPolicy,
    createHITWithHITType_hITLayoutParameters,
    createHITWithHITType_hITTypeId,
    createHITWithHITType_lifetimeInSeconds,
    createHITWithHITTypeResponse_hit,
    createHITWithHITTypeResponse_httpStatus,

    -- ** DeleteWorkerBlock
    deleteWorkerBlock_reason,
    deleteWorkerBlock_workerId,
    deleteWorkerBlockResponse_httpStatus,

    -- ** ListBonusPayments
    listBonusPayments_nextToken,
    listBonusPayments_assignmentId,
    listBonusPayments_maxResults,
    listBonusPayments_hITId,
    listBonusPaymentsResponse_nextToken,
    listBonusPaymentsResponse_numResults,
    listBonusPaymentsResponse_bonusPayments,
    listBonusPaymentsResponse_httpStatus,

    -- ** DisassociateQualificationFromWorker
    disassociateQualificationFromWorker_reason,
    disassociateQualificationFromWorker_workerId,
    disassociateQualificationFromWorker_qualificationTypeId,
    disassociateQualificationFromWorkerResponse_httpStatus,

    -- ** SendTestEventNotification
    sendTestEventNotification_notification,
    sendTestEventNotification_testEventType,
    sendTestEventNotificationResponse_httpStatus,

    -- ** GetHIT
    getHIT_hITId,
    getHITResponse_hit,
    getHITResponse_httpStatus,

    -- ** UpdateQualificationType
    updateQualificationType_retryDelayInSeconds,
    updateQualificationType_autoGranted,
    updateQualificationType_qualificationTypeStatus,
    updateQualificationType_testDurationInSeconds,
    updateQualificationType_description,
    updateQualificationType_test,
    updateQualificationType_answerKey,
    updateQualificationType_autoGrantedValue,
    updateQualificationType_qualificationTypeId,
    updateQualificationTypeResponse_qualificationType,
    updateQualificationTypeResponse_httpStatus,

    -- ** AcceptQualificationRequest
    acceptQualificationRequest_integerValue,
    acceptQualificationRequest_qualificationRequestId,
    acceptQualificationRequestResponse_httpStatus,

    -- ** DeleteQualificationType
    deleteQualificationType_qualificationTypeId,
    deleteQualificationTypeResponse_httpStatus,

    -- ** GetFileUploadURL
    getFileUploadURL_assignmentId,
    getFileUploadURL_questionIdentifier,
    getFileUploadURLResponse_fileUploadURL,
    getFileUploadURLResponse_httpStatus,

    -- ** CreateQualificationType
    createQualificationType_retryDelayInSeconds,
    createQualificationType_autoGranted,
    createQualificationType_testDurationInSeconds,
    createQualificationType_test,
    createQualificationType_answerKey,
    createQualificationType_autoGrantedValue,
    createQualificationType_keywords,
    createQualificationType_name,
    createQualificationType_description,
    createQualificationType_qualificationTypeStatus,
    createQualificationTypeResponse_qualificationType,
    createQualificationTypeResponse_httpStatus,

    -- * Types

    -- ** Assignment
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

    -- ** BonusPayment
    bonusPayment_bonusAmount,
    bonusPayment_assignmentId,
    bonusPayment_grantTime,
    bonusPayment_reason,
    bonusPayment_workerId,

    -- ** HIT
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
    policyParameter_values,
    policyParameter_mapEntries,

    -- ** Qualification
    qualification_qualificationTypeId,
    qualification_status,
    qualification_grantTime,
    qualification_workerId,
    qualification_localeValue,
    qualification_integerValue,

    -- ** QualificationRequest
    qualificationRequest_qualificationTypeId,
    qualificationRequest_answer,
    qualificationRequest_submitTime,
    qualificationRequest_test,
    qualificationRequest_workerId,
    qualificationRequest_qualificationRequestId,

    -- ** QualificationRequirement
    qualificationRequirement_actionsGuarded,
    qualificationRequirement_localeValues,
    qualificationRequirement_requiredToPreview,
    qualificationRequirement_integerValues,
    qualificationRequirement_qualificationTypeId,
    qualificationRequirement_comparator,

    -- ** QualificationType
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

    -- ** ReviewActionDetail
    reviewActionDetail_status,
    reviewActionDetail_targetId,
    reviewActionDetail_actionName,
    reviewActionDetail_targetType,
    reviewActionDetail_result,
    reviewActionDetail_actionId,
    reviewActionDetail_completeTime,
    reviewActionDetail_errorCode,

    -- ** ReviewPolicy
    reviewPolicy_parameters,
    reviewPolicy_policyName,

    -- ** ReviewReport
    reviewReport_reviewActions,
    reviewReport_reviewResults,

    -- ** ReviewResultDetail
    reviewResultDetail_key,
    reviewResultDetail_subjectType,
    reviewResultDetail_subjectId,
    reviewResultDetail_actionId,
    reviewResultDetail_value,
    reviewResultDetail_questionId,

    -- ** WorkerBlock
    workerBlock_reason,
    workerBlock_workerId,
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
import Network.AWS.MechanicalTurk.Types.Assignment
import Network.AWS.MechanicalTurk.Types.BonusPayment
import Network.AWS.MechanicalTurk.Types.HIT
import Network.AWS.MechanicalTurk.Types.HITLayoutParameter
import Network.AWS.MechanicalTurk.Types.Locale
import Network.AWS.MechanicalTurk.Types.NotificationSpecification
import Network.AWS.MechanicalTurk.Types.NotifyWorkersFailureStatus
import Network.AWS.MechanicalTurk.Types.ParameterMapEntry
import Network.AWS.MechanicalTurk.Types.PolicyParameter
import Network.AWS.MechanicalTurk.Types.Qualification
import Network.AWS.MechanicalTurk.Types.QualificationRequest
import Network.AWS.MechanicalTurk.Types.QualificationRequirement
import Network.AWS.MechanicalTurk.Types.QualificationType
import Network.AWS.MechanicalTurk.Types.ReviewActionDetail
import Network.AWS.MechanicalTurk.Types.ReviewPolicy
import Network.AWS.MechanicalTurk.Types.ReviewReport
import Network.AWS.MechanicalTurk.Types.ReviewResultDetail
import Network.AWS.MechanicalTurk.Types.WorkerBlock
import Network.AWS.MechanicalTurk.UpdateExpirationForHIT
import Network.AWS.MechanicalTurk.UpdateHITReviewStatus
import Network.AWS.MechanicalTurk.UpdateHITTypeOfHIT
import Network.AWS.MechanicalTurk.UpdateNotificationSettings
import Network.AWS.MechanicalTurk.UpdateQualificationType
