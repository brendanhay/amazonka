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

    -- ** ApproveAssignment
    approveAssignment_overrideRejection,
    approveAssignment_requesterFeedback,
    approveAssignment_assignmentId,
    approveAssignmentResponse_httpStatus,

    -- ** ListReviewPolicyResultsForHIT
    listReviewPolicyResultsForHIT_retrieveResults,
    listReviewPolicyResultsForHIT_policyLevels,
    listReviewPolicyResultsForHIT_retrieveActions,
    listReviewPolicyResultsForHIT_nextToken,
    listReviewPolicyResultsForHIT_maxResults,
    listReviewPolicyResultsForHIT_hITId,
    listReviewPolicyResultsForHITResponse_hITReviewPolicy,
    listReviewPolicyResultsForHITResponse_hITReviewReport,
    listReviewPolicyResultsForHITResponse_nextToken,
    listReviewPolicyResultsForHITResponse_assignmentReviewReport,
    listReviewPolicyResultsForHITResponse_hITId,
    listReviewPolicyResultsForHITResponse_assignmentReviewPolicy,
    listReviewPolicyResultsForHITResponse_httpStatus,

    -- ** ListHITs
    listHITs_nextToken,
    listHITs_maxResults,
    listHITsResponse_nextToken,
    listHITsResponse_numResults,
    listHITsResponse_hITs,
    listHITsResponse_httpStatus,

    -- ** ListWorkersWithQualificationType
    listWorkersWithQualificationType_status,
    listWorkersWithQualificationType_nextToken,
    listWorkersWithQualificationType_maxResults,
    listWorkersWithQualificationType_qualificationTypeId,
    listWorkersWithQualificationTypeResponse_nextToken,
    listWorkersWithQualificationTypeResponse_numResults,
    listWorkersWithQualificationTypeResponse_qualifications,
    listWorkersWithQualificationTypeResponse_httpStatus,

    -- ** DeleteHIT
    deleteHIT_hITId,
    deleteHITResponse_httpStatus,

    -- ** ListReviewableHITs
    listReviewableHITs_status,
    listReviewableHITs_hITTypeId,
    listReviewableHITs_nextToken,
    listReviewableHITs_maxResults,
    listReviewableHITsResponse_nextToken,
    listReviewableHITsResponse_numResults,
    listReviewableHITsResponse_hITs,
    listReviewableHITsResponse_httpStatus,

    -- ** GetAssignment
    getAssignment_assignmentId,
    getAssignmentResponse_hit,
    getAssignmentResponse_assignment,
    getAssignmentResponse_httpStatus,

    -- ** DeleteQualificationType
    deleteQualificationType_qualificationTypeId,
    deleteQualificationTypeResponse_httpStatus,

    -- ** UpdateQualificationType
    updateQualificationType_testDurationInSeconds,
    updateQualificationType_qualificationTypeStatus,
    updateQualificationType_answerKey,
    updateQualificationType_test,
    updateQualificationType_autoGranted,
    updateQualificationType_autoGrantedValue,
    updateQualificationType_description,
    updateQualificationType_retryDelayInSeconds,
    updateQualificationType_qualificationTypeId,
    updateQualificationTypeResponse_qualificationType,
    updateQualificationTypeResponse_httpStatus,

    -- ** ListQualificationTypes
    listQualificationTypes_mustBeOwnedByCaller,
    listQualificationTypes_nextToken,
    listQualificationTypes_query,
    listQualificationTypes_maxResults,
    listQualificationTypes_mustBeRequestable,
    listQualificationTypesResponse_qualificationTypes,
    listQualificationTypesResponse_nextToken,
    listQualificationTypesResponse_numResults,
    listQualificationTypesResponse_httpStatus,

    -- ** UpdateHITTypeOfHIT
    updateHITTypeOfHIT_hITId,
    updateHITTypeOfHIT_hITTypeId,
    updateHITTypeOfHITResponse_httpStatus,

    -- ** DisassociateQualificationFromWorker
    disassociateQualificationFromWorker_reason,
    disassociateQualificationFromWorker_workerId,
    disassociateQualificationFromWorker_qualificationTypeId,
    disassociateQualificationFromWorkerResponse_httpStatus,

    -- ** SendTestEventNotification
    sendTestEventNotification_notification,
    sendTestEventNotification_testEventType,
    sendTestEventNotificationResponse_httpStatus,

    -- ** NotifyWorkers
    notifyWorkers_subject,
    notifyWorkers_messageText,
    notifyWorkers_workerIds,
    notifyWorkersResponse_notifyWorkersFailureStatuses,
    notifyWorkersResponse_httpStatus,

    -- ** CreateHITWithHITType
    createHITWithHITType_hITReviewPolicy,
    createHITWithHITType_uniqueRequestToken,
    createHITWithHITType_requesterAnnotation,
    createHITWithHITType_maxAssignments,
    createHITWithHITType_hITLayoutId,
    createHITWithHITType_hITLayoutParameters,
    createHITWithHITType_question,
    createHITWithHITType_assignmentReviewPolicy,
    createHITWithHITType_hITTypeId,
    createHITWithHITType_lifetimeInSeconds,
    createHITWithHITTypeResponse_hit,
    createHITWithHITTypeResponse_httpStatus,

    -- ** CreateHITType
    createHITType_autoApprovalDelayInSeconds,
    createHITType_keywords,
    createHITType_qualificationRequirements,
    createHITType_assignmentDurationInSeconds,
    createHITType_reward,
    createHITType_title,
    createHITType_description,
    createHITTypeResponse_hITTypeId,
    createHITTypeResponse_httpStatus,

    -- ** SendBonus
    sendBonus_uniqueRequestToken,
    sendBonus_workerId,
    sendBonus_bonusAmount,
    sendBonus_assignmentId,
    sendBonus_reason,
    sendBonusResponse_httpStatus,

    -- ** ListQualificationRequests
    listQualificationRequests_nextToken,
    listQualificationRequests_qualificationTypeId,
    listQualificationRequests_maxResults,
    listQualificationRequestsResponse_qualificationRequests,
    listQualificationRequestsResponse_nextToken,
    listQualificationRequestsResponse_numResults,
    listQualificationRequestsResponse_httpStatus,

    -- ** UpdateExpirationForHIT
    updateExpirationForHIT_hITId,
    updateExpirationForHIT_expireAt,
    updateExpirationForHITResponse_httpStatus,

    -- ** RejectAssignment
    rejectAssignment_assignmentId,
    rejectAssignment_requesterFeedback,
    rejectAssignmentResponse_httpStatus,

    -- ** ListAssignmentsForHIT
    listAssignmentsForHIT_assignmentStatuses,
    listAssignmentsForHIT_nextToken,
    listAssignmentsForHIT_maxResults,
    listAssignmentsForHIT_hITId,
    listAssignmentsForHITResponse_nextToken,
    listAssignmentsForHITResponse_numResults,
    listAssignmentsForHITResponse_assignments,
    listAssignmentsForHITResponse_httpStatus,

    -- ** RejectQualificationRequest
    rejectQualificationRequest_reason,
    rejectQualificationRequest_qualificationRequestId,
    rejectQualificationRequestResponse_httpStatus,

    -- ** GetQualificationScore
    getQualificationScore_qualificationTypeId,
    getQualificationScore_workerId,
    getQualificationScoreResponse_qualification,
    getQualificationScoreResponse_httpStatus,

    -- ** GetQualificationType
    getQualificationType_qualificationTypeId,
    getQualificationTypeResponse_qualificationType,
    getQualificationTypeResponse_httpStatus,

    -- ** UpdateHITReviewStatus
    updateHITReviewStatus_revert,
    updateHITReviewStatus_hITId,
    updateHITReviewStatusResponse_httpStatus,

    -- ** CreateQualificationType
    createQualificationType_testDurationInSeconds,
    createQualificationType_answerKey,
    createQualificationType_test,
    createQualificationType_keywords,
    createQualificationType_autoGranted,
    createQualificationType_autoGrantedValue,
    createQualificationType_retryDelayInSeconds,
    createQualificationType_name,
    createQualificationType_description,
    createQualificationType_qualificationTypeStatus,
    createQualificationTypeResponse_qualificationType,
    createQualificationTypeResponse_httpStatus,

    -- ** AcceptQualificationRequest
    acceptQualificationRequest_integerValue,
    acceptQualificationRequest_qualificationRequestId,
    acceptQualificationRequestResponse_httpStatus,

    -- ** GetFileUploadURL
    getFileUploadURL_assignmentId,
    getFileUploadURL_questionIdentifier,
    getFileUploadURLResponse_fileUploadURL,
    getFileUploadURLResponse_httpStatus,

    -- ** CreateAdditionalAssignmentsForHIT
    createAdditionalAssignmentsForHIT_uniqueRequestToken,
    createAdditionalAssignmentsForHIT_hITId,
    createAdditionalAssignmentsForHIT_numberOfAdditionalAssignments,
    createAdditionalAssignmentsForHITResponse_httpStatus,

    -- ** GetHIT
    getHIT_hITId,
    getHITResponse_hit,
    getHITResponse_httpStatus,

    -- ** CreateWorkerBlock
    createWorkerBlock_workerId,
    createWorkerBlock_reason,
    createWorkerBlockResponse_httpStatus,

    -- ** ListHITsForQualificationType
    listHITsForQualificationType_nextToken,
    listHITsForQualificationType_maxResults,
    listHITsForQualificationType_qualificationTypeId,
    listHITsForQualificationTypeResponse_nextToken,
    listHITsForQualificationTypeResponse_numResults,
    listHITsForQualificationTypeResponse_hITs,
    listHITsForQualificationTypeResponse_httpStatus,

    -- ** ListBonusPayments
    listBonusPayments_nextToken,
    listBonusPayments_hITId,
    listBonusPayments_assignmentId,
    listBonusPayments_maxResults,
    listBonusPaymentsResponse_bonusPayments,
    listBonusPaymentsResponse_nextToken,
    listBonusPaymentsResponse_numResults,
    listBonusPaymentsResponse_httpStatus,

    -- ** ListWorkerBlocks
    listWorkerBlocks_nextToken,
    listWorkerBlocks_maxResults,
    listWorkerBlocksResponse_workerBlocks,
    listWorkerBlocksResponse_nextToken,
    listWorkerBlocksResponse_numResults,
    listWorkerBlocksResponse_httpStatus,

    -- ** DeleteWorkerBlock
    deleteWorkerBlock_reason,
    deleteWorkerBlock_workerId,
    deleteWorkerBlockResponse_httpStatus,

    -- ** UpdateNotificationSettings
    updateNotificationSettings_notification,
    updateNotificationSettings_active,
    updateNotificationSettings_hITTypeId,
    updateNotificationSettingsResponse_httpStatus,

    -- ** AssociateQualificationWithWorker
    associateQualificationWithWorker_integerValue,
    associateQualificationWithWorker_sendNotification,
    associateQualificationWithWorker_qualificationTypeId,
    associateQualificationWithWorker_workerId,
    associateQualificationWithWorkerResponse_httpStatus,

    -- ** CreateHIT
    createHIT_hITReviewPolicy,
    createHIT_uniqueRequestToken,
    createHIT_autoApprovalDelayInSeconds,
    createHIT_requesterAnnotation,
    createHIT_maxAssignments,
    createHIT_keywords,
    createHIT_hITLayoutId,
    createHIT_hITLayoutParameters,
    createHIT_qualificationRequirements,
    createHIT_question,
    createHIT_assignmentReviewPolicy,
    createHIT_lifetimeInSeconds,
    createHIT_assignmentDurationInSeconds,
    createHIT_reward,
    createHIT_title,
    createHIT_description,
    createHITResponse_hit,
    createHITResponse_httpStatus,

    -- ** GetAccountBalance
    getAccountBalanceResponse_availableBalance,
    getAccountBalanceResponse_onHoldBalance,
    getAccountBalanceResponse_httpStatus,

    -- * Types

    -- ** Assignment
    assignment_acceptTime,
    assignment_answer,
    assignment_assignmentStatus,
    assignment_requesterFeedback,
    assignment_deadline,
    assignment_approvalTime,
    assignment_rejectionTime,
    assignment_autoApprovalTime,
    assignment_hITId,
    assignment_workerId,
    assignment_assignmentId,
    assignment_submitTime,

    -- ** BonusPayment
    bonusPayment_reason,
    bonusPayment_grantTime,
    bonusPayment_workerId,
    bonusPayment_assignmentId,
    bonusPayment_bonusAmount,

    -- ** HIT
    hit_creationTime,
    hit_hITGroupId,
    hit_numberOfAssignmentsPending,
    hit_hITTypeId,
    hit_expiration,
    hit_autoApprovalDelayInSeconds,
    hit_requesterAnnotation,
    hit_hITStatus,
    hit_maxAssignments,
    hit_numberOfAssignmentsCompleted,
    hit_reward,
    hit_keywords,
    hit_hITLayoutId,
    hit_qualificationRequirements,
    hit_title,
    hit_hITId,
    hit_hITReviewStatus,
    hit_numberOfAssignmentsAvailable,
    hit_description,
    hit_question,
    hit_assignmentDurationInSeconds,

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
    notifyWorkersFailureStatus_notifyWorkersFailureMessage,
    notifyWorkersFailureStatus_notifyWorkersFailureCode,
    notifyWorkersFailureStatus_workerId,

    -- ** ParameterMapEntry
    parameterMapEntry_values,
    parameterMapEntry_key,

    -- ** PolicyParameter
    policyParameter_values,
    policyParameter_mapEntries,
    policyParameter_key,

    -- ** Qualification
    qualification_status,
    qualification_integerValue,
    qualification_localeValue,
    qualification_qualificationTypeId,
    qualification_grantTime,
    qualification_workerId,

    -- ** QualificationRequest
    qualificationRequest_qualificationRequestId,
    qualificationRequest_test,
    qualificationRequest_qualificationTypeId,
    qualificationRequest_answer,
    qualificationRequest_workerId,
    qualificationRequest_submitTime,

    -- ** QualificationRequirement
    qualificationRequirement_localeValues,
    qualificationRequirement_actionsGuarded,
    qualificationRequirement_requiredToPreview,
    qualificationRequirement_integerValues,
    qualificationRequirement_qualificationTypeId,
    qualificationRequirement_comparator,

    -- ** QualificationType
    qualificationType_creationTime,
    qualificationType_testDurationInSeconds,
    qualificationType_qualificationTypeStatus,
    qualificationType_answerKey,
    qualificationType_test,
    qualificationType_qualificationTypeId,
    qualificationType_name,
    qualificationType_keywords,
    qualificationType_autoGranted,
    qualificationType_autoGrantedValue,
    qualificationType_description,
    qualificationType_isRequestable,
    qualificationType_retryDelayInSeconds,

    -- ** ReviewActionDetail
    reviewActionDetail_status,
    reviewActionDetail_targetId,
    reviewActionDetail_actionId,
    reviewActionDetail_targetType,
    reviewActionDetail_result,
    reviewActionDetail_actionName,
    reviewActionDetail_completeTime,
    reviewActionDetail_errorCode,

    -- ** ReviewPolicy
    reviewPolicy_parameters,
    reviewPolicy_policyName,

    -- ** ReviewReport
    reviewReport_reviewActions,
    reviewReport_reviewResults,

    -- ** ReviewResultDetail
    reviewResultDetail_value,
    reviewResultDetail_actionId,
    reviewResultDetail_subjectType,
    reviewResultDetail_key,
    reviewResultDetail_questionId,
    reviewResultDetail_subjectId,

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
