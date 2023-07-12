{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AuditManager.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Lens
  ( -- * Operations

    -- ** AssociateAssessmentReportEvidenceFolder
    associateAssessmentReportEvidenceFolder_assessmentId,
    associateAssessmentReportEvidenceFolder_evidenceFolderId,
    associateAssessmentReportEvidenceFolderResponse_httpStatus,

    -- ** BatchAssociateAssessmentReportEvidence
    batchAssociateAssessmentReportEvidence_assessmentId,
    batchAssociateAssessmentReportEvidence_evidenceFolderId,
    batchAssociateAssessmentReportEvidence_evidenceIds,
    batchAssociateAssessmentReportEvidenceResponse_errors,
    batchAssociateAssessmentReportEvidenceResponse_evidenceIds,
    batchAssociateAssessmentReportEvidenceResponse_httpStatus,

    -- ** BatchCreateDelegationByAssessment
    batchCreateDelegationByAssessment_createDelegationRequests,
    batchCreateDelegationByAssessment_assessmentId,
    batchCreateDelegationByAssessmentResponse_delegations,
    batchCreateDelegationByAssessmentResponse_errors,
    batchCreateDelegationByAssessmentResponse_httpStatus,

    -- ** BatchDeleteDelegationByAssessment
    batchDeleteDelegationByAssessment_delegationIds,
    batchDeleteDelegationByAssessment_assessmentId,
    batchDeleteDelegationByAssessmentResponse_errors,
    batchDeleteDelegationByAssessmentResponse_httpStatus,

    -- ** BatchDisassociateAssessmentReportEvidence
    batchDisassociateAssessmentReportEvidence_assessmentId,
    batchDisassociateAssessmentReportEvidence_evidenceFolderId,
    batchDisassociateAssessmentReportEvidence_evidenceIds,
    batchDisassociateAssessmentReportEvidenceResponse_errors,
    batchDisassociateAssessmentReportEvidenceResponse_evidenceIds,
    batchDisassociateAssessmentReportEvidenceResponse_httpStatus,

    -- ** BatchImportEvidenceToAssessmentControl
    batchImportEvidenceToAssessmentControl_assessmentId,
    batchImportEvidenceToAssessmentControl_controlSetId,
    batchImportEvidenceToAssessmentControl_controlId,
    batchImportEvidenceToAssessmentControl_manualEvidence,
    batchImportEvidenceToAssessmentControlResponse_errors,
    batchImportEvidenceToAssessmentControlResponse_httpStatus,

    -- ** CreateAssessment
    createAssessment_description,
    createAssessment_tags,
    createAssessment_name,
    createAssessment_assessmentReportsDestination,
    createAssessment_scope,
    createAssessment_roles,
    createAssessment_frameworkId,
    createAssessmentResponse_assessment,
    createAssessmentResponse_httpStatus,

    -- ** CreateAssessmentFramework
    createAssessmentFramework_complianceType,
    createAssessmentFramework_description,
    createAssessmentFramework_tags,
    createAssessmentFramework_name,
    createAssessmentFramework_controlSets,
    createAssessmentFrameworkResponse_framework,
    createAssessmentFrameworkResponse_httpStatus,

    -- ** CreateAssessmentReport
    createAssessmentReport_description,
    createAssessmentReport_queryStatement,
    createAssessmentReport_name,
    createAssessmentReport_assessmentId,
    createAssessmentReportResponse_assessmentReport,
    createAssessmentReportResponse_httpStatus,

    -- ** CreateControl
    createControl_actionPlanInstructions,
    createControl_actionPlanTitle,
    createControl_description,
    createControl_tags,
    createControl_testingInformation,
    createControl_name,
    createControl_controlMappingSources,
    createControlResponse_control,
    createControlResponse_httpStatus,

    -- ** DeleteAssessment
    deleteAssessment_assessmentId,
    deleteAssessmentResponse_httpStatus,

    -- ** DeleteAssessmentFramework
    deleteAssessmentFramework_frameworkId,
    deleteAssessmentFrameworkResponse_httpStatus,

    -- ** DeleteAssessmentFrameworkShare
    deleteAssessmentFrameworkShare_requestId,
    deleteAssessmentFrameworkShare_requestType,
    deleteAssessmentFrameworkShareResponse_httpStatus,

    -- ** DeleteAssessmentReport
    deleteAssessmentReport_assessmentId,
    deleteAssessmentReport_assessmentReportId,
    deleteAssessmentReportResponse_httpStatus,

    -- ** DeleteControl
    deleteControl_controlId,
    deleteControlResponse_httpStatus,

    -- ** DeregisterAccount
    deregisterAccountResponse_status,
    deregisterAccountResponse_httpStatus,

    -- ** DeregisterOrganizationAdminAccount
    deregisterOrganizationAdminAccount_adminAccountId,
    deregisterOrganizationAdminAccountResponse_httpStatus,

    -- ** DisassociateAssessmentReportEvidenceFolder
    disassociateAssessmentReportEvidenceFolder_assessmentId,
    disassociateAssessmentReportEvidenceFolder_evidenceFolderId,
    disassociateAssessmentReportEvidenceFolderResponse_httpStatus,

    -- ** GetAccountStatus
    getAccountStatusResponse_status,
    getAccountStatusResponse_httpStatus,

    -- ** GetAssessment
    getAssessment_assessmentId,
    getAssessmentResponse_assessment,
    getAssessmentResponse_userRole,
    getAssessmentResponse_httpStatus,

    -- ** GetAssessmentFramework
    getAssessmentFramework_frameworkId,
    getAssessmentFrameworkResponse_framework,
    getAssessmentFrameworkResponse_httpStatus,

    -- ** GetAssessmentReportUrl
    getAssessmentReportUrl_assessmentReportId,
    getAssessmentReportUrl_assessmentId,
    getAssessmentReportUrlResponse_preSignedUrl,
    getAssessmentReportUrlResponse_httpStatus,

    -- ** GetChangeLogs
    getChangeLogs_controlId,
    getChangeLogs_controlSetId,
    getChangeLogs_maxResults,
    getChangeLogs_nextToken,
    getChangeLogs_assessmentId,
    getChangeLogsResponse_changeLogs,
    getChangeLogsResponse_nextToken,
    getChangeLogsResponse_httpStatus,

    -- ** GetControl
    getControl_controlId,
    getControlResponse_control,
    getControlResponse_httpStatus,

    -- ** GetDelegations
    getDelegations_maxResults,
    getDelegations_nextToken,
    getDelegationsResponse_delegations,
    getDelegationsResponse_nextToken,
    getDelegationsResponse_httpStatus,

    -- ** GetEvidence
    getEvidence_assessmentId,
    getEvidence_controlSetId,
    getEvidence_evidenceFolderId,
    getEvidence_evidenceId,
    getEvidenceResponse_evidence,
    getEvidenceResponse_httpStatus,

    -- ** GetEvidenceByEvidenceFolder
    getEvidenceByEvidenceFolder_maxResults,
    getEvidenceByEvidenceFolder_nextToken,
    getEvidenceByEvidenceFolder_assessmentId,
    getEvidenceByEvidenceFolder_controlSetId,
    getEvidenceByEvidenceFolder_evidenceFolderId,
    getEvidenceByEvidenceFolderResponse_evidence,
    getEvidenceByEvidenceFolderResponse_nextToken,
    getEvidenceByEvidenceFolderResponse_httpStatus,

    -- ** GetEvidenceFolder
    getEvidenceFolder_assessmentId,
    getEvidenceFolder_controlSetId,
    getEvidenceFolder_evidenceFolderId,
    getEvidenceFolderResponse_evidenceFolder,
    getEvidenceFolderResponse_httpStatus,

    -- ** GetEvidenceFoldersByAssessment
    getEvidenceFoldersByAssessment_maxResults,
    getEvidenceFoldersByAssessment_nextToken,
    getEvidenceFoldersByAssessment_assessmentId,
    getEvidenceFoldersByAssessmentResponse_evidenceFolders,
    getEvidenceFoldersByAssessmentResponse_nextToken,
    getEvidenceFoldersByAssessmentResponse_httpStatus,

    -- ** GetEvidenceFoldersByAssessmentControl
    getEvidenceFoldersByAssessmentControl_maxResults,
    getEvidenceFoldersByAssessmentControl_nextToken,
    getEvidenceFoldersByAssessmentControl_assessmentId,
    getEvidenceFoldersByAssessmentControl_controlSetId,
    getEvidenceFoldersByAssessmentControl_controlId,
    getEvidenceFoldersByAssessmentControlResponse_evidenceFolders,
    getEvidenceFoldersByAssessmentControlResponse_nextToken,
    getEvidenceFoldersByAssessmentControlResponse_httpStatus,

    -- ** GetInsights
    getInsightsResponse_insights,
    getInsightsResponse_httpStatus,

    -- ** GetInsightsByAssessment
    getInsightsByAssessment_assessmentId,
    getInsightsByAssessmentResponse_insights,
    getInsightsByAssessmentResponse_httpStatus,

    -- ** GetOrganizationAdminAccount
    getOrganizationAdminAccountResponse_adminAccountId,
    getOrganizationAdminAccountResponse_organizationId,
    getOrganizationAdminAccountResponse_httpStatus,

    -- ** GetServicesInScope
    getServicesInScopeResponse_serviceMetadata,
    getServicesInScopeResponse_httpStatus,

    -- ** GetSettings
    getSettings_attribute,
    getSettingsResponse_settings,
    getSettingsResponse_httpStatus,

    -- ** ListAssessmentControlInsightsByControlDomain
    listAssessmentControlInsightsByControlDomain_maxResults,
    listAssessmentControlInsightsByControlDomain_nextToken,
    listAssessmentControlInsightsByControlDomain_controlDomainId,
    listAssessmentControlInsightsByControlDomain_assessmentId,
    listAssessmentControlInsightsByControlDomainResponse_controlInsightsByAssessment,
    listAssessmentControlInsightsByControlDomainResponse_nextToken,
    listAssessmentControlInsightsByControlDomainResponse_httpStatus,

    -- ** ListAssessmentFrameworkShareRequests
    listAssessmentFrameworkShareRequests_maxResults,
    listAssessmentFrameworkShareRequests_nextToken,
    listAssessmentFrameworkShareRequests_requestType,
    listAssessmentFrameworkShareRequestsResponse_assessmentFrameworkShareRequests,
    listAssessmentFrameworkShareRequestsResponse_nextToken,
    listAssessmentFrameworkShareRequestsResponse_httpStatus,

    -- ** ListAssessmentFrameworks
    listAssessmentFrameworks_maxResults,
    listAssessmentFrameworks_nextToken,
    listAssessmentFrameworks_frameworkType,
    listAssessmentFrameworksResponse_frameworkMetadataList,
    listAssessmentFrameworksResponse_nextToken,
    listAssessmentFrameworksResponse_httpStatus,

    -- ** ListAssessmentReports
    listAssessmentReports_maxResults,
    listAssessmentReports_nextToken,
    listAssessmentReportsResponse_assessmentReports,
    listAssessmentReportsResponse_nextToken,
    listAssessmentReportsResponse_httpStatus,

    -- ** ListAssessments
    listAssessments_maxResults,
    listAssessments_nextToken,
    listAssessments_status,
    listAssessmentsResponse_assessmentMetadata,
    listAssessmentsResponse_nextToken,
    listAssessmentsResponse_httpStatus,

    -- ** ListControlDomainInsights
    listControlDomainInsights_maxResults,
    listControlDomainInsights_nextToken,
    listControlDomainInsightsResponse_controlDomainInsights,
    listControlDomainInsightsResponse_nextToken,
    listControlDomainInsightsResponse_httpStatus,

    -- ** ListControlDomainInsightsByAssessment
    listControlDomainInsightsByAssessment_maxResults,
    listControlDomainInsightsByAssessment_nextToken,
    listControlDomainInsightsByAssessment_assessmentId,
    listControlDomainInsightsByAssessmentResponse_controlDomainInsights,
    listControlDomainInsightsByAssessmentResponse_nextToken,
    listControlDomainInsightsByAssessmentResponse_httpStatus,

    -- ** ListControlInsightsByControlDomain
    listControlInsightsByControlDomain_maxResults,
    listControlInsightsByControlDomain_nextToken,
    listControlInsightsByControlDomain_controlDomainId,
    listControlInsightsByControlDomainResponse_controlInsightsMetadata,
    listControlInsightsByControlDomainResponse_nextToken,
    listControlInsightsByControlDomainResponse_httpStatus,

    -- ** ListControls
    listControls_maxResults,
    listControls_nextToken,
    listControls_controlType,
    listControlsResponse_controlMetadataList,
    listControlsResponse_nextToken,
    listControlsResponse_httpStatus,

    -- ** ListKeywordsForDataSource
    listKeywordsForDataSource_maxResults,
    listKeywordsForDataSource_nextToken,
    listKeywordsForDataSource_source,
    listKeywordsForDataSourceResponse_keywords,
    listKeywordsForDataSourceResponse_nextToken,
    listKeywordsForDataSourceResponse_httpStatus,

    -- ** ListNotifications
    listNotifications_maxResults,
    listNotifications_nextToken,
    listNotificationsResponse_nextToken,
    listNotificationsResponse_notifications,
    listNotificationsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** RegisterAccount
    registerAccount_delegatedAdminAccount,
    registerAccount_kmsKey,
    registerAccountResponse_status,
    registerAccountResponse_httpStatus,

    -- ** RegisterOrganizationAdminAccount
    registerOrganizationAdminAccount_adminAccountId,
    registerOrganizationAdminAccountResponse_adminAccountId,
    registerOrganizationAdminAccountResponse_organizationId,
    registerOrganizationAdminAccountResponse_httpStatus,

    -- ** StartAssessmentFrameworkShare
    startAssessmentFrameworkShare_comment,
    startAssessmentFrameworkShare_frameworkId,
    startAssessmentFrameworkShare_destinationAccount,
    startAssessmentFrameworkShare_destinationRegion,
    startAssessmentFrameworkShareResponse_assessmentFrameworkShareRequest,
    startAssessmentFrameworkShareResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateAssessment
    updateAssessment_assessmentDescription,
    updateAssessment_assessmentName,
    updateAssessment_assessmentReportsDestination,
    updateAssessment_roles,
    updateAssessment_assessmentId,
    updateAssessment_scope,
    updateAssessmentResponse_assessment,
    updateAssessmentResponse_httpStatus,

    -- ** UpdateAssessmentControl
    updateAssessmentControl_commentBody,
    updateAssessmentControl_controlStatus,
    updateAssessmentControl_assessmentId,
    updateAssessmentControl_controlSetId,
    updateAssessmentControl_controlId,
    updateAssessmentControlResponse_control,
    updateAssessmentControlResponse_httpStatus,

    -- ** UpdateAssessmentControlSetStatus
    updateAssessmentControlSetStatus_assessmentId,
    updateAssessmentControlSetStatus_controlSetId,
    updateAssessmentControlSetStatus_status,
    updateAssessmentControlSetStatus_comment,
    updateAssessmentControlSetStatusResponse_controlSet,
    updateAssessmentControlSetStatusResponse_httpStatus,

    -- ** UpdateAssessmentFramework
    updateAssessmentFramework_complianceType,
    updateAssessmentFramework_description,
    updateAssessmentFramework_frameworkId,
    updateAssessmentFramework_name,
    updateAssessmentFramework_controlSets,
    updateAssessmentFrameworkResponse_framework,
    updateAssessmentFrameworkResponse_httpStatus,

    -- ** UpdateAssessmentFrameworkShare
    updateAssessmentFrameworkShare_requestId,
    updateAssessmentFrameworkShare_requestType,
    updateAssessmentFrameworkShare_action,
    updateAssessmentFrameworkShareResponse_assessmentFrameworkShareRequest,
    updateAssessmentFrameworkShareResponse_httpStatus,

    -- ** UpdateAssessmentStatus
    updateAssessmentStatus_assessmentId,
    updateAssessmentStatus_status,
    updateAssessmentStatusResponse_assessment,
    updateAssessmentStatusResponse_httpStatus,

    -- ** UpdateControl
    updateControl_actionPlanInstructions,
    updateControl_actionPlanTitle,
    updateControl_description,
    updateControl_testingInformation,
    updateControl_controlId,
    updateControl_name,
    updateControl_controlMappingSources,
    updateControlResponse_control,
    updateControlResponse_httpStatus,

    -- ** UpdateSettings
    updateSettings_defaultAssessmentReportsDestination,
    updateSettings_defaultProcessOwners,
    updateSettings_deregistrationPolicy,
    updateSettings_evidenceFinderEnabled,
    updateSettings_kmsKey,
    updateSettings_snsTopic,
    updateSettingsResponse_settings,
    updateSettingsResponse_httpStatus,

    -- ** ValidateAssessmentReportIntegrity
    validateAssessmentReportIntegrity_s3RelativePath,
    validateAssessmentReportIntegrityResponse_signatureAlgorithm,
    validateAssessmentReportIntegrityResponse_signatureDateTime,
    validateAssessmentReportIntegrityResponse_signatureKeyId,
    validateAssessmentReportIntegrityResponse_signatureValid,
    validateAssessmentReportIntegrityResponse_validationErrors,
    validateAssessmentReportIntegrityResponse_httpStatus,

    -- * Types

    -- ** AWSAccount
    aWSAccount_emailAddress,
    aWSAccount_id,
    aWSAccount_name,

    -- ** AWSService
    aWSService_serviceName,

    -- ** Assessment
    assessment_arn,
    assessment_awsAccount,
    assessment_framework,
    assessment_metadata,
    assessment_tags,

    -- ** AssessmentControl
    assessmentControl_assessmentReportEvidenceCount,
    assessmentControl_comments,
    assessmentControl_description,
    assessmentControl_evidenceCount,
    assessmentControl_evidenceSources,
    assessmentControl_id,
    assessmentControl_name,
    assessmentControl_response,
    assessmentControl_status,

    -- ** AssessmentControlSet
    assessmentControlSet_controls,
    assessmentControlSet_delegations,
    assessmentControlSet_description,
    assessmentControlSet_id,
    assessmentControlSet_manualEvidenceCount,
    assessmentControlSet_roles,
    assessmentControlSet_status,
    assessmentControlSet_systemEvidenceCount,

    -- ** AssessmentEvidenceFolder
    assessmentEvidenceFolder_assessmentId,
    assessmentEvidenceFolder_assessmentReportSelectionCount,
    assessmentEvidenceFolder_author,
    assessmentEvidenceFolder_controlId,
    assessmentEvidenceFolder_controlName,
    assessmentEvidenceFolder_controlSetId,
    assessmentEvidenceFolder_dataSource,
    assessmentEvidenceFolder_date,
    assessmentEvidenceFolder_evidenceAwsServiceSourceCount,
    assessmentEvidenceFolder_evidenceByTypeComplianceCheckCount,
    assessmentEvidenceFolder_evidenceByTypeComplianceCheckIssuesCount,
    assessmentEvidenceFolder_evidenceByTypeConfigurationDataCount,
    assessmentEvidenceFolder_evidenceByTypeManualCount,
    assessmentEvidenceFolder_evidenceByTypeUserActivityCount,
    assessmentEvidenceFolder_evidenceResourcesIncludedCount,
    assessmentEvidenceFolder_id,
    assessmentEvidenceFolder_name,
    assessmentEvidenceFolder_totalEvidence,

    -- ** AssessmentFramework
    assessmentFramework_arn,
    assessmentFramework_controlSets,
    assessmentFramework_id,
    assessmentFramework_metadata,

    -- ** AssessmentFrameworkMetadata
    assessmentFrameworkMetadata_arn,
    assessmentFrameworkMetadata_complianceType,
    assessmentFrameworkMetadata_controlSetsCount,
    assessmentFrameworkMetadata_controlsCount,
    assessmentFrameworkMetadata_createdAt,
    assessmentFrameworkMetadata_description,
    assessmentFrameworkMetadata_id,
    assessmentFrameworkMetadata_lastUpdatedAt,
    assessmentFrameworkMetadata_logo,
    assessmentFrameworkMetadata_name,
    assessmentFrameworkMetadata_type,

    -- ** AssessmentFrameworkShareRequest
    assessmentFrameworkShareRequest_comment,
    assessmentFrameworkShareRequest_complianceType,
    assessmentFrameworkShareRequest_creationTime,
    assessmentFrameworkShareRequest_customControlsCount,
    assessmentFrameworkShareRequest_destinationAccount,
    assessmentFrameworkShareRequest_destinationRegion,
    assessmentFrameworkShareRequest_expirationTime,
    assessmentFrameworkShareRequest_frameworkDescription,
    assessmentFrameworkShareRequest_frameworkId,
    assessmentFrameworkShareRequest_frameworkName,
    assessmentFrameworkShareRequest_id,
    assessmentFrameworkShareRequest_lastUpdated,
    assessmentFrameworkShareRequest_sourceAccount,
    assessmentFrameworkShareRequest_standardControlsCount,
    assessmentFrameworkShareRequest_status,

    -- ** AssessmentMetadata
    assessmentMetadata_assessmentReportsDestination,
    assessmentMetadata_complianceType,
    assessmentMetadata_creationTime,
    assessmentMetadata_delegations,
    assessmentMetadata_description,
    assessmentMetadata_id,
    assessmentMetadata_lastUpdated,
    assessmentMetadata_name,
    assessmentMetadata_roles,
    assessmentMetadata_scope,
    assessmentMetadata_status,

    -- ** AssessmentMetadataItem
    assessmentMetadataItem_complianceType,
    assessmentMetadataItem_creationTime,
    assessmentMetadataItem_delegations,
    assessmentMetadataItem_id,
    assessmentMetadataItem_lastUpdated,
    assessmentMetadataItem_name,
    assessmentMetadataItem_roles,
    assessmentMetadataItem_status,

    -- ** AssessmentReport
    assessmentReport_assessmentId,
    assessmentReport_assessmentName,
    assessmentReport_author,
    assessmentReport_awsAccountId,
    assessmentReport_creationTime,
    assessmentReport_description,
    assessmentReport_id,
    assessmentReport_name,
    assessmentReport_status,

    -- ** AssessmentReportEvidenceError
    assessmentReportEvidenceError_errorCode,
    assessmentReportEvidenceError_errorMessage,
    assessmentReportEvidenceError_evidenceId,

    -- ** AssessmentReportMetadata
    assessmentReportMetadata_assessmentId,
    assessmentReportMetadata_assessmentName,
    assessmentReportMetadata_author,
    assessmentReportMetadata_creationTime,
    assessmentReportMetadata_description,
    assessmentReportMetadata_id,
    assessmentReportMetadata_name,
    assessmentReportMetadata_status,

    -- ** AssessmentReportsDestination
    assessmentReportsDestination_destination,
    assessmentReportsDestination_destinationType,

    -- ** BatchCreateDelegationByAssessmentError
    batchCreateDelegationByAssessmentError_createDelegationRequest,
    batchCreateDelegationByAssessmentError_errorCode,
    batchCreateDelegationByAssessmentError_errorMessage,

    -- ** BatchDeleteDelegationByAssessmentError
    batchDeleteDelegationByAssessmentError_delegationId,
    batchDeleteDelegationByAssessmentError_errorCode,
    batchDeleteDelegationByAssessmentError_errorMessage,

    -- ** BatchImportEvidenceToAssessmentControlError
    batchImportEvidenceToAssessmentControlError_errorCode,
    batchImportEvidenceToAssessmentControlError_errorMessage,
    batchImportEvidenceToAssessmentControlError_manualEvidence,

    -- ** ChangeLog
    changeLog_action,
    changeLog_createdAt,
    changeLog_createdBy,
    changeLog_objectName,
    changeLog_objectType,

    -- ** Control
    control_actionPlanInstructions,
    control_actionPlanTitle,
    control_arn,
    control_controlMappingSources,
    control_controlSources,
    control_createdAt,
    control_createdBy,
    control_description,
    control_id,
    control_lastUpdatedAt,
    control_lastUpdatedBy,
    control_name,
    control_tags,
    control_testingInformation,
    control_type,

    -- ** ControlComment
    controlComment_authorName,
    controlComment_commentBody,
    controlComment_postedDate,

    -- ** ControlDomainInsights
    controlDomainInsights_controlsCountByNoncompliantEvidence,
    controlDomainInsights_evidenceInsights,
    controlDomainInsights_id,
    controlDomainInsights_lastUpdated,
    controlDomainInsights_name,
    controlDomainInsights_totalControlsCount,

    -- ** ControlInsightsMetadataByAssessmentItem
    controlInsightsMetadataByAssessmentItem_controlSetName,
    controlInsightsMetadataByAssessmentItem_evidenceInsights,
    controlInsightsMetadataByAssessmentItem_id,
    controlInsightsMetadataByAssessmentItem_lastUpdated,
    controlInsightsMetadataByAssessmentItem_name,

    -- ** ControlInsightsMetadataItem
    controlInsightsMetadataItem_evidenceInsights,
    controlInsightsMetadataItem_id,
    controlInsightsMetadataItem_lastUpdated,
    controlInsightsMetadataItem_name,

    -- ** ControlMappingSource
    controlMappingSource_sourceDescription,
    controlMappingSource_sourceFrequency,
    controlMappingSource_sourceId,
    controlMappingSource_sourceKeyword,
    controlMappingSource_sourceName,
    controlMappingSource_sourceSetUpOption,
    controlMappingSource_sourceType,
    controlMappingSource_troubleshootingText,

    -- ** ControlMetadata
    controlMetadata_arn,
    controlMetadata_controlSources,
    controlMetadata_createdAt,
    controlMetadata_id,
    controlMetadata_lastUpdatedAt,
    controlMetadata_name,

    -- ** ControlSet
    controlSet_controls,
    controlSet_id,
    controlSet_name,

    -- ** CreateAssessmentFrameworkControl
    createAssessmentFrameworkControl_id,

    -- ** CreateAssessmentFrameworkControlSet
    createAssessmentFrameworkControlSet_controls,
    createAssessmentFrameworkControlSet_name,

    -- ** CreateControlMappingSource
    createControlMappingSource_sourceDescription,
    createControlMappingSource_sourceFrequency,
    createControlMappingSource_sourceKeyword,
    createControlMappingSource_sourceName,
    createControlMappingSource_sourceSetUpOption,
    createControlMappingSource_sourceType,
    createControlMappingSource_troubleshootingText,

    -- ** CreateDelegationRequest
    createDelegationRequest_comment,
    createDelegationRequest_controlSetId,
    createDelegationRequest_roleArn,
    createDelegationRequest_roleType,

    -- ** Delegation
    delegation_assessmentId,
    delegation_assessmentName,
    delegation_comment,
    delegation_controlSetId,
    delegation_createdBy,
    delegation_creationTime,
    delegation_id,
    delegation_lastUpdated,
    delegation_roleArn,
    delegation_roleType,
    delegation_status,

    -- ** DelegationMetadata
    delegationMetadata_assessmentId,
    delegationMetadata_assessmentName,
    delegationMetadata_controlSetName,
    delegationMetadata_creationTime,
    delegationMetadata_id,
    delegationMetadata_roleArn,
    delegationMetadata_status,

    -- ** DeregistrationPolicy
    deregistrationPolicy_deleteResources,

    -- ** Evidence
    evidence_assessmentReportSelection,
    evidence_attributes,
    evidence_awsAccountId,
    evidence_awsOrganization,
    evidence_complianceCheck,
    evidence_dataSource,
    evidence_eventName,
    evidence_eventSource,
    evidence_evidenceAwsAccountId,
    evidence_evidenceByType,
    evidence_evidenceFolderId,
    evidence_iamId,
    evidence_id,
    evidence_resourcesIncluded,
    evidence_time,

    -- ** EvidenceFinderEnablement
    evidenceFinderEnablement_backfillStatus,
    evidenceFinderEnablement_enablementStatus,
    evidenceFinderEnablement_error,
    evidenceFinderEnablement_eventDataStoreArn,

    -- ** EvidenceInsights
    evidenceInsights_compliantEvidenceCount,
    evidenceInsights_inconclusiveEvidenceCount,
    evidenceInsights_noncompliantEvidenceCount,

    -- ** Framework
    framework_arn,
    framework_complianceType,
    framework_controlSets,
    framework_controlSources,
    framework_createdAt,
    framework_createdBy,
    framework_description,
    framework_id,
    framework_lastUpdatedAt,
    framework_lastUpdatedBy,
    framework_logo,
    framework_name,
    framework_tags,
    framework_type,

    -- ** FrameworkMetadata
    frameworkMetadata_complianceType,
    frameworkMetadata_description,
    frameworkMetadata_logo,
    frameworkMetadata_name,

    -- ** Insights
    insights_activeAssessmentsCount,
    insights_assessmentControlsCountByNoncompliantEvidence,
    insights_compliantEvidenceCount,
    insights_inconclusiveEvidenceCount,
    insights_lastUpdated,
    insights_noncompliantEvidenceCount,
    insights_totalAssessmentControlsCount,

    -- ** InsightsByAssessment
    insightsByAssessment_assessmentControlsCountByNoncompliantEvidence,
    insightsByAssessment_compliantEvidenceCount,
    insightsByAssessment_inconclusiveEvidenceCount,
    insightsByAssessment_lastUpdated,
    insightsByAssessment_noncompliantEvidenceCount,
    insightsByAssessment_totalAssessmentControlsCount,

    -- ** ManualEvidence
    manualEvidence_s3ResourcePath,

    -- ** Notification
    notification_assessmentId,
    notification_assessmentName,
    notification_controlSetId,
    notification_controlSetName,
    notification_description,
    notification_eventTime,
    notification_id,
    notification_source,

    -- ** Resource
    resource_arn,
    resource_complianceCheck,
    resource_value,

    -- ** Role
    role_roleType,
    role_roleArn,

    -- ** Scope
    scope_awsAccounts,
    scope_awsServices,

    -- ** ServiceMetadata
    serviceMetadata_category,
    serviceMetadata_description,
    serviceMetadata_displayName,
    serviceMetadata_name,

    -- ** Settings
    settings_defaultAssessmentReportsDestination,
    settings_defaultProcessOwners,
    settings_deregistrationPolicy,
    settings_evidenceFinderEnablement,
    settings_isAwsOrgEnabled,
    settings_kmsKey,
    settings_snsTopic,

    -- ** SourceKeyword
    sourceKeyword_keywordInputType,
    sourceKeyword_keywordValue,

    -- ** URL
    url_hyperlinkName,
    url_link,

    -- ** UpdateAssessmentFrameworkControlSet
    updateAssessmentFrameworkControlSet_id,
    updateAssessmentFrameworkControlSet_name,
    updateAssessmentFrameworkControlSet_controls,
  )
where

import Amazonka.AuditManager.AssociateAssessmentReportEvidenceFolder
import Amazonka.AuditManager.BatchAssociateAssessmentReportEvidence
import Amazonka.AuditManager.BatchCreateDelegationByAssessment
import Amazonka.AuditManager.BatchDeleteDelegationByAssessment
import Amazonka.AuditManager.BatchDisassociateAssessmentReportEvidence
import Amazonka.AuditManager.BatchImportEvidenceToAssessmentControl
import Amazonka.AuditManager.CreateAssessment
import Amazonka.AuditManager.CreateAssessmentFramework
import Amazonka.AuditManager.CreateAssessmentReport
import Amazonka.AuditManager.CreateControl
import Amazonka.AuditManager.DeleteAssessment
import Amazonka.AuditManager.DeleteAssessmentFramework
import Amazonka.AuditManager.DeleteAssessmentFrameworkShare
import Amazonka.AuditManager.DeleteAssessmentReport
import Amazonka.AuditManager.DeleteControl
import Amazonka.AuditManager.DeregisterAccount
import Amazonka.AuditManager.DeregisterOrganizationAdminAccount
import Amazonka.AuditManager.DisassociateAssessmentReportEvidenceFolder
import Amazonka.AuditManager.GetAccountStatus
import Amazonka.AuditManager.GetAssessment
import Amazonka.AuditManager.GetAssessmentFramework
import Amazonka.AuditManager.GetAssessmentReportUrl
import Amazonka.AuditManager.GetChangeLogs
import Amazonka.AuditManager.GetControl
import Amazonka.AuditManager.GetDelegations
import Amazonka.AuditManager.GetEvidence
import Amazonka.AuditManager.GetEvidenceByEvidenceFolder
import Amazonka.AuditManager.GetEvidenceFolder
import Amazonka.AuditManager.GetEvidenceFoldersByAssessment
import Amazonka.AuditManager.GetEvidenceFoldersByAssessmentControl
import Amazonka.AuditManager.GetInsights
import Amazonka.AuditManager.GetInsightsByAssessment
import Amazonka.AuditManager.GetOrganizationAdminAccount
import Amazonka.AuditManager.GetServicesInScope
import Amazonka.AuditManager.GetSettings
import Amazonka.AuditManager.ListAssessmentControlInsightsByControlDomain
import Amazonka.AuditManager.ListAssessmentFrameworkShareRequests
import Amazonka.AuditManager.ListAssessmentFrameworks
import Amazonka.AuditManager.ListAssessmentReports
import Amazonka.AuditManager.ListAssessments
import Amazonka.AuditManager.ListControlDomainInsights
import Amazonka.AuditManager.ListControlDomainInsightsByAssessment
import Amazonka.AuditManager.ListControlInsightsByControlDomain
import Amazonka.AuditManager.ListControls
import Amazonka.AuditManager.ListKeywordsForDataSource
import Amazonka.AuditManager.ListNotifications
import Amazonka.AuditManager.ListTagsForResource
import Amazonka.AuditManager.RegisterAccount
import Amazonka.AuditManager.RegisterOrganizationAdminAccount
import Amazonka.AuditManager.StartAssessmentFrameworkShare
import Amazonka.AuditManager.TagResource
import Amazonka.AuditManager.Types.AWSAccount
import Amazonka.AuditManager.Types.AWSService
import Amazonka.AuditManager.Types.Assessment
import Amazonka.AuditManager.Types.AssessmentControl
import Amazonka.AuditManager.Types.AssessmentControlSet
import Amazonka.AuditManager.Types.AssessmentEvidenceFolder
import Amazonka.AuditManager.Types.AssessmentFramework
import Amazonka.AuditManager.Types.AssessmentFrameworkMetadata
import Amazonka.AuditManager.Types.AssessmentFrameworkShareRequest
import Amazonka.AuditManager.Types.AssessmentMetadata
import Amazonka.AuditManager.Types.AssessmentMetadataItem
import Amazonka.AuditManager.Types.AssessmentReport
import Amazonka.AuditManager.Types.AssessmentReportEvidenceError
import Amazonka.AuditManager.Types.AssessmentReportMetadata
import Amazonka.AuditManager.Types.AssessmentReportsDestination
import Amazonka.AuditManager.Types.BatchCreateDelegationByAssessmentError
import Amazonka.AuditManager.Types.BatchDeleteDelegationByAssessmentError
import Amazonka.AuditManager.Types.BatchImportEvidenceToAssessmentControlError
import Amazonka.AuditManager.Types.ChangeLog
import Amazonka.AuditManager.Types.Control
import Amazonka.AuditManager.Types.ControlComment
import Amazonka.AuditManager.Types.ControlDomainInsights
import Amazonka.AuditManager.Types.ControlInsightsMetadataByAssessmentItem
import Amazonka.AuditManager.Types.ControlInsightsMetadataItem
import Amazonka.AuditManager.Types.ControlMappingSource
import Amazonka.AuditManager.Types.ControlMetadata
import Amazonka.AuditManager.Types.ControlSet
import Amazonka.AuditManager.Types.CreateAssessmentFrameworkControl
import Amazonka.AuditManager.Types.CreateAssessmentFrameworkControlSet
import Amazonka.AuditManager.Types.CreateControlMappingSource
import Amazonka.AuditManager.Types.CreateDelegationRequest
import Amazonka.AuditManager.Types.Delegation
import Amazonka.AuditManager.Types.DelegationMetadata
import Amazonka.AuditManager.Types.DeregistrationPolicy
import Amazonka.AuditManager.Types.Evidence
import Amazonka.AuditManager.Types.EvidenceFinderEnablement
import Amazonka.AuditManager.Types.EvidenceInsights
import Amazonka.AuditManager.Types.Framework
import Amazonka.AuditManager.Types.FrameworkMetadata
import Amazonka.AuditManager.Types.Insights
import Amazonka.AuditManager.Types.InsightsByAssessment
import Amazonka.AuditManager.Types.ManualEvidence
import Amazonka.AuditManager.Types.Notification
import Amazonka.AuditManager.Types.Resource
import Amazonka.AuditManager.Types.Role
import Amazonka.AuditManager.Types.Scope
import Amazonka.AuditManager.Types.ServiceMetadata
import Amazonka.AuditManager.Types.Settings
import Amazonka.AuditManager.Types.SourceKeyword
import Amazonka.AuditManager.Types.URL
import Amazonka.AuditManager.Types.UpdateAssessmentFrameworkControlSet
import Amazonka.AuditManager.UntagResource
import Amazonka.AuditManager.UpdateAssessment
import Amazonka.AuditManager.UpdateAssessmentControl
import Amazonka.AuditManager.UpdateAssessmentControlSetStatus
import Amazonka.AuditManager.UpdateAssessmentFramework
import Amazonka.AuditManager.UpdateAssessmentFrameworkShare
import Amazonka.AuditManager.UpdateAssessmentStatus
import Amazonka.AuditManager.UpdateControl
import Amazonka.AuditManager.UpdateSettings
import Amazonka.AuditManager.ValidateAssessmentReportIntegrity
