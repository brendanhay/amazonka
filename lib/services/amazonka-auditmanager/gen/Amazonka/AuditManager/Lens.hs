{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AuditManager.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    batchCreateDelegationByAssessmentResponse_errors,
    batchCreateDelegationByAssessmentResponse_delegations,
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
    createAssessment_tags,
    createAssessment_description,
    createAssessment_name,
    createAssessment_assessmentReportsDestination,
    createAssessment_scope,
    createAssessment_roles,
    createAssessment_frameworkId,
    createAssessmentResponse_assessment,
    createAssessmentResponse_httpStatus,

    -- ** CreateAssessmentFramework
    createAssessmentFramework_tags,
    createAssessmentFramework_description,
    createAssessmentFramework_complianceType,
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
    createControl_tags,
    createControl_actionPlanInstructions,
    createControl_description,
    createControl_actionPlanTitle,
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
    getChangeLogs_nextToken,
    getChangeLogs_maxResults,
    getChangeLogs_controlSetId,
    getChangeLogs_assessmentId,
    getChangeLogsResponse_nextToken,
    getChangeLogsResponse_changeLogs,
    getChangeLogsResponse_httpStatus,

    -- ** GetControl
    getControl_controlId,
    getControlResponse_control,
    getControlResponse_httpStatus,

    -- ** GetDelegations
    getDelegations_nextToken,
    getDelegations_maxResults,
    getDelegationsResponse_nextToken,
    getDelegationsResponse_delegations,
    getDelegationsResponse_httpStatus,

    -- ** GetEvidence
    getEvidence_assessmentId,
    getEvidence_controlSetId,
    getEvidence_evidenceFolderId,
    getEvidence_evidenceId,
    getEvidenceResponse_evidence,
    getEvidenceResponse_httpStatus,

    -- ** GetEvidenceByEvidenceFolder
    getEvidenceByEvidenceFolder_nextToken,
    getEvidenceByEvidenceFolder_maxResults,
    getEvidenceByEvidenceFolder_assessmentId,
    getEvidenceByEvidenceFolder_controlSetId,
    getEvidenceByEvidenceFolder_evidenceFolderId,
    getEvidenceByEvidenceFolderResponse_nextToken,
    getEvidenceByEvidenceFolderResponse_evidence,
    getEvidenceByEvidenceFolderResponse_httpStatus,

    -- ** GetEvidenceFolder
    getEvidenceFolder_assessmentId,
    getEvidenceFolder_controlSetId,
    getEvidenceFolder_evidenceFolderId,
    getEvidenceFolderResponse_evidenceFolder,
    getEvidenceFolderResponse_httpStatus,

    -- ** GetEvidenceFoldersByAssessment
    getEvidenceFoldersByAssessment_nextToken,
    getEvidenceFoldersByAssessment_maxResults,
    getEvidenceFoldersByAssessment_assessmentId,
    getEvidenceFoldersByAssessmentResponse_evidenceFolders,
    getEvidenceFoldersByAssessmentResponse_nextToken,
    getEvidenceFoldersByAssessmentResponse_httpStatus,

    -- ** GetEvidenceFoldersByAssessmentControl
    getEvidenceFoldersByAssessmentControl_nextToken,
    getEvidenceFoldersByAssessmentControl_maxResults,
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
    listAssessmentControlInsightsByControlDomain_nextToken,
    listAssessmentControlInsightsByControlDomain_maxResults,
    listAssessmentControlInsightsByControlDomain_controlDomainId,
    listAssessmentControlInsightsByControlDomain_assessmentId,
    listAssessmentControlInsightsByControlDomainResponse_controlInsightsByAssessment,
    listAssessmentControlInsightsByControlDomainResponse_nextToken,
    listAssessmentControlInsightsByControlDomainResponse_httpStatus,

    -- ** ListAssessmentFrameworkShareRequests
    listAssessmentFrameworkShareRequests_nextToken,
    listAssessmentFrameworkShareRequests_maxResults,
    listAssessmentFrameworkShareRequests_requestType,
    listAssessmentFrameworkShareRequestsResponse_nextToken,
    listAssessmentFrameworkShareRequestsResponse_assessmentFrameworkShareRequests,
    listAssessmentFrameworkShareRequestsResponse_httpStatus,

    -- ** ListAssessmentFrameworks
    listAssessmentFrameworks_nextToken,
    listAssessmentFrameworks_maxResults,
    listAssessmentFrameworks_frameworkType,
    listAssessmentFrameworksResponse_nextToken,
    listAssessmentFrameworksResponse_frameworkMetadataList,
    listAssessmentFrameworksResponse_httpStatus,

    -- ** ListAssessmentReports
    listAssessmentReports_nextToken,
    listAssessmentReports_maxResults,
    listAssessmentReportsResponse_nextToken,
    listAssessmentReportsResponse_assessmentReports,
    listAssessmentReportsResponse_httpStatus,

    -- ** ListAssessments
    listAssessments_nextToken,
    listAssessments_status,
    listAssessments_maxResults,
    listAssessmentsResponse_nextToken,
    listAssessmentsResponse_assessmentMetadata,
    listAssessmentsResponse_httpStatus,

    -- ** ListControlDomainInsights
    listControlDomainInsights_nextToken,
    listControlDomainInsights_maxResults,
    listControlDomainInsightsResponse_nextToken,
    listControlDomainInsightsResponse_controlDomainInsights,
    listControlDomainInsightsResponse_httpStatus,

    -- ** ListControlDomainInsightsByAssessment
    listControlDomainInsightsByAssessment_nextToken,
    listControlDomainInsightsByAssessment_maxResults,
    listControlDomainInsightsByAssessment_assessmentId,
    listControlDomainInsightsByAssessmentResponse_nextToken,
    listControlDomainInsightsByAssessmentResponse_controlDomainInsights,
    listControlDomainInsightsByAssessmentResponse_httpStatus,

    -- ** ListControlInsightsByControlDomain
    listControlInsightsByControlDomain_nextToken,
    listControlInsightsByControlDomain_maxResults,
    listControlInsightsByControlDomain_controlDomainId,
    listControlInsightsByControlDomainResponse_nextToken,
    listControlInsightsByControlDomainResponse_controlInsightsMetadata,
    listControlInsightsByControlDomainResponse_httpStatus,

    -- ** ListControls
    listControls_nextToken,
    listControls_maxResults,
    listControls_controlType,
    listControlsResponse_nextToken,
    listControlsResponse_controlMetadataList,
    listControlsResponse_httpStatus,

    -- ** ListKeywordsForDataSource
    listKeywordsForDataSource_nextToken,
    listKeywordsForDataSource_maxResults,
    listKeywordsForDataSource_source,
    listKeywordsForDataSourceResponse_nextToken,
    listKeywordsForDataSourceResponse_keywords,
    listKeywordsForDataSourceResponse_httpStatus,

    -- ** ListNotifications
    listNotifications_nextToken,
    listNotifications_maxResults,
    listNotificationsResponse_notifications,
    listNotificationsResponse_nextToken,
    listNotificationsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** RegisterAccount
    registerAccount_kmsKey,
    registerAccount_delegatedAdminAccount,
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
    updateAssessment_assessmentName,
    updateAssessment_assessmentDescription,
    updateAssessment_assessmentReportsDestination,
    updateAssessment_roles,
    updateAssessment_assessmentId,
    updateAssessment_scope,
    updateAssessmentResponse_assessment,
    updateAssessmentResponse_httpStatus,

    -- ** UpdateAssessmentControl
    updateAssessmentControl_controlStatus,
    updateAssessmentControl_commentBody,
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
    updateAssessmentFramework_description,
    updateAssessmentFramework_complianceType,
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
    updateControl_description,
    updateControl_actionPlanTitle,
    updateControl_testingInformation,
    updateControl_controlId,
    updateControl_name,
    updateControl_controlMappingSources,
    updateControlResponse_control,
    updateControlResponse_httpStatus,

    -- ** UpdateSettings
    updateSettings_defaultProcessOwners,
    updateSettings_snsTopic,
    updateSettings_kmsKey,
    updateSettings_defaultAssessmentReportsDestination,
    updateSettings_evidenceFinderEnabled,
    updateSettingsResponse_settings,
    updateSettingsResponse_httpStatus,

    -- ** ValidateAssessmentReportIntegrity
    validateAssessmentReportIntegrity_s3RelativePath,
    validateAssessmentReportIntegrityResponse_validationErrors,
    validateAssessmentReportIntegrityResponse_signatureValid,
    validateAssessmentReportIntegrityResponse_signatureAlgorithm,
    validateAssessmentReportIntegrityResponse_signatureKeyId,
    validateAssessmentReportIntegrityResponse_signatureDateTime,
    validateAssessmentReportIntegrityResponse_httpStatus,

    -- * Types

    -- ** AWSAccount
    aWSAccount_name,
    aWSAccount_id,
    aWSAccount_emailAddress,

    -- ** AWSService
    aWSService_serviceName,

    -- ** Assessment
    assessment_tags,
    assessment_metadata,
    assessment_awsAccount,
    assessment_arn,
    assessment_framework,

    -- ** AssessmentControl
    assessmentControl_name,
    assessmentControl_evidenceCount,
    assessmentControl_response,
    assessmentControl_assessmentReportEvidenceCount,
    assessmentControl_status,
    assessmentControl_description,
    assessmentControl_id,
    assessmentControl_comments,
    assessmentControl_evidenceSources,

    -- ** AssessmentControlSet
    assessmentControlSet_systemEvidenceCount,
    assessmentControlSet_status,
    assessmentControlSet_description,
    assessmentControlSet_id,
    assessmentControlSet_controls,
    assessmentControlSet_delegations,
    assessmentControlSet_manualEvidenceCount,
    assessmentControlSet_roles,

    -- ** AssessmentEvidenceFolder
    assessmentEvidenceFolder_controlId,
    assessmentEvidenceFolder_author,
    assessmentEvidenceFolder_name,
    assessmentEvidenceFolder_evidenceByTypeComplianceCheckIssuesCount,
    assessmentEvidenceFolder_totalEvidence,
    assessmentEvidenceFolder_assessmentId,
    assessmentEvidenceFolder_evidenceResourcesIncludedCount,
    assessmentEvidenceFolder_date,
    assessmentEvidenceFolder_evidenceAwsServiceSourceCount,
    assessmentEvidenceFolder_evidenceByTypeManualCount,
    assessmentEvidenceFolder_evidenceByTypeUserActivityCount,
    assessmentEvidenceFolder_id,
    assessmentEvidenceFolder_assessmentReportSelectionCount,
    assessmentEvidenceFolder_controlSetId,
    assessmentEvidenceFolder_evidenceByTypeComplianceCheckCount,
    assessmentEvidenceFolder_dataSource,
    assessmentEvidenceFolder_controlName,
    assessmentEvidenceFolder_evidenceByTypeConfigurationDataCount,

    -- ** AssessmentFramework
    assessmentFramework_metadata,
    assessmentFramework_arn,
    assessmentFramework_id,
    assessmentFramework_controlSets,

    -- ** AssessmentFrameworkMetadata
    assessmentFrameworkMetadata_controlSetsCount,
    assessmentFrameworkMetadata_name,
    assessmentFrameworkMetadata_type,
    assessmentFrameworkMetadata_lastUpdatedAt,
    assessmentFrameworkMetadata_arn,
    assessmentFrameworkMetadata_description,
    assessmentFrameworkMetadata_id,
    assessmentFrameworkMetadata_logo,
    assessmentFrameworkMetadata_controlsCount,
    assessmentFrameworkMetadata_complianceType,
    assessmentFrameworkMetadata_createdAt,

    -- ** AssessmentFrameworkShareRequest
    assessmentFrameworkShareRequest_customControlsCount,
    assessmentFrameworkShareRequest_destinationAccount,
    assessmentFrameworkShareRequest_expirationTime,
    assessmentFrameworkShareRequest_frameworkDescription,
    assessmentFrameworkShareRequest_status,
    assessmentFrameworkShareRequest_id,
    assessmentFrameworkShareRequest_frameworkName,
    assessmentFrameworkShareRequest_frameworkId,
    assessmentFrameworkShareRequest_lastUpdated,
    assessmentFrameworkShareRequest_comment,
    assessmentFrameworkShareRequest_creationTime,
    assessmentFrameworkShareRequest_destinationRegion,
    assessmentFrameworkShareRequest_complianceType,
    assessmentFrameworkShareRequest_standardControlsCount,
    assessmentFrameworkShareRequest_sourceAccount,

    -- ** AssessmentMetadata
    assessmentMetadata_name,
    assessmentMetadata_status,
    assessmentMetadata_description,
    assessmentMetadata_id,
    assessmentMetadata_lastUpdated,
    assessmentMetadata_scope,
    assessmentMetadata_creationTime,
    assessmentMetadata_delegations,
    assessmentMetadata_complianceType,
    assessmentMetadata_assessmentReportsDestination,
    assessmentMetadata_roles,

    -- ** AssessmentMetadataItem
    assessmentMetadataItem_name,
    assessmentMetadataItem_status,
    assessmentMetadataItem_id,
    assessmentMetadataItem_lastUpdated,
    assessmentMetadataItem_creationTime,
    assessmentMetadataItem_delegations,
    assessmentMetadataItem_complianceType,
    assessmentMetadataItem_roles,

    -- ** AssessmentReport
    assessmentReport_awsAccountId,
    assessmentReport_author,
    assessmentReport_name,
    assessmentReport_assessmentId,
    assessmentReport_assessmentName,
    assessmentReport_status,
    assessmentReport_description,
    assessmentReport_id,
    assessmentReport_creationTime,

    -- ** AssessmentReportEvidenceError
    assessmentReportEvidenceError_errorMessage,
    assessmentReportEvidenceError_evidenceId,
    assessmentReportEvidenceError_errorCode,

    -- ** AssessmentReportMetadata
    assessmentReportMetadata_author,
    assessmentReportMetadata_name,
    assessmentReportMetadata_assessmentId,
    assessmentReportMetadata_assessmentName,
    assessmentReportMetadata_status,
    assessmentReportMetadata_description,
    assessmentReportMetadata_id,
    assessmentReportMetadata_creationTime,

    -- ** AssessmentReportsDestination
    assessmentReportsDestination_destination,
    assessmentReportsDestination_destinationType,

    -- ** BatchCreateDelegationByAssessmentError
    batchCreateDelegationByAssessmentError_createDelegationRequest,
    batchCreateDelegationByAssessmentError_errorMessage,
    batchCreateDelegationByAssessmentError_errorCode,

    -- ** BatchDeleteDelegationByAssessmentError
    batchDeleteDelegationByAssessmentError_errorMessage,
    batchDeleteDelegationByAssessmentError_delegationId,
    batchDeleteDelegationByAssessmentError_errorCode,

    -- ** BatchImportEvidenceToAssessmentControlError
    batchImportEvidenceToAssessmentControlError_errorMessage,
    batchImportEvidenceToAssessmentControlError_errorCode,
    batchImportEvidenceToAssessmentControlError_manualEvidence,

    -- ** ChangeLog
    changeLog_action,
    changeLog_objectName,
    changeLog_createdBy,
    changeLog_createdAt,
    changeLog_objectType,

    -- ** Control
    control_tags,
    control_name,
    control_type,
    control_lastUpdatedAt,
    control_actionPlanInstructions,
    control_arn,
    control_description,
    control_id,
    control_actionPlanTitle,
    control_controlMappingSources,
    control_createdBy,
    control_createdAt,
    control_lastUpdatedBy,
    control_controlSources,
    control_testingInformation,

    -- ** ControlComment
    controlComment_postedDate,
    controlComment_authorName,
    controlComment_commentBody,

    -- ** ControlDomainInsights
    controlDomainInsights_evidenceInsights,
    controlDomainInsights_name,
    controlDomainInsights_totalControlsCount,
    controlDomainInsights_id,
    controlDomainInsights_lastUpdated,
    controlDomainInsights_controlsCountByNoncompliantEvidence,

    -- ** ControlInsightsMetadataByAssessmentItem
    controlInsightsMetadataByAssessmentItem_evidenceInsights,
    controlInsightsMetadataByAssessmentItem_name,
    controlInsightsMetadataByAssessmentItem_id,
    controlInsightsMetadataByAssessmentItem_lastUpdated,
    controlInsightsMetadataByAssessmentItem_controlSetName,

    -- ** ControlInsightsMetadataItem
    controlInsightsMetadataItem_evidenceInsights,
    controlInsightsMetadataItem_name,
    controlInsightsMetadataItem_id,
    controlInsightsMetadataItem_lastUpdated,

    -- ** ControlMappingSource
    controlMappingSource_sourceFrequency,
    controlMappingSource_sourceDescription,
    controlMappingSource_sourceKeyword,
    controlMappingSource_sourceName,
    controlMappingSource_sourceId,
    controlMappingSource_sourceSetUpOption,
    controlMappingSource_troubleshootingText,
    controlMappingSource_sourceType,

    -- ** ControlMetadata
    controlMetadata_name,
    controlMetadata_lastUpdatedAt,
    controlMetadata_arn,
    controlMetadata_id,
    controlMetadata_createdAt,
    controlMetadata_controlSources,

    -- ** ControlSet
    controlSet_name,
    controlSet_id,
    controlSet_controls,

    -- ** CreateAssessmentFrameworkControl
    createAssessmentFrameworkControl_id,

    -- ** CreateAssessmentFrameworkControlSet
    createAssessmentFrameworkControlSet_controls,
    createAssessmentFrameworkControlSet_name,

    -- ** CreateControlMappingSource
    createControlMappingSource_sourceFrequency,
    createControlMappingSource_sourceDescription,
    createControlMappingSource_sourceKeyword,
    createControlMappingSource_sourceName,
    createControlMappingSource_sourceSetUpOption,
    createControlMappingSource_troubleshootingText,
    createControlMappingSource_sourceType,

    -- ** CreateDelegationRequest
    createDelegationRequest_roleType,
    createDelegationRequest_roleArn,
    createDelegationRequest_comment,
    createDelegationRequest_controlSetId,

    -- ** Delegation
    delegation_roleType,
    delegation_roleArn,
    delegation_assessmentId,
    delegation_assessmentName,
    delegation_status,
    delegation_id,
    delegation_lastUpdated,
    delegation_comment,
    delegation_controlSetId,
    delegation_creationTime,
    delegation_createdBy,

    -- ** DelegationMetadata
    delegationMetadata_roleArn,
    delegationMetadata_assessmentId,
    delegationMetadata_assessmentName,
    delegationMetadata_status,
    delegationMetadata_id,
    delegationMetadata_controlSetName,
    delegationMetadata_creationTime,

    -- ** Evidence
    evidence_awsAccountId,
    evidence_evidenceAwsAccountId,
    evidence_evidenceFolderId,
    evidence_resourcesIncluded,
    evidence_awsOrganization,
    evidence_time,
    evidence_id,
    evidence_evidenceByType,
    evidence_complianceCheck,
    evidence_iamId,
    evidence_eventName,
    evidence_dataSource,
    evidence_assessmentReportSelection,
    evidence_attributes,
    evidence_eventSource,

    -- ** EvidenceFinderEnablement
    evidenceFinderEnablement_eventDataStoreArn,
    evidenceFinderEnablement_enablementStatus,
    evidenceFinderEnablement_backfillStatus,
    evidenceFinderEnablement_error,

    -- ** EvidenceInsights
    evidenceInsights_compliantEvidenceCount,
    evidenceInsights_inconclusiveEvidenceCount,
    evidenceInsights_noncompliantEvidenceCount,

    -- ** Framework
    framework_tags,
    framework_name,
    framework_type,
    framework_lastUpdatedAt,
    framework_arn,
    framework_description,
    framework_id,
    framework_logo,
    framework_controlSets,
    framework_complianceType,
    framework_createdBy,
    framework_createdAt,
    framework_lastUpdatedBy,
    framework_controlSources,

    -- ** FrameworkMetadata
    frameworkMetadata_name,
    frameworkMetadata_description,
    frameworkMetadata_logo,
    frameworkMetadata_complianceType,

    -- ** Insights
    insights_totalAssessmentControlsCount,
    insights_compliantEvidenceCount,
    insights_lastUpdated,
    insights_activeAssessmentsCount,
    insights_inconclusiveEvidenceCount,
    insights_noncompliantEvidenceCount,
    insights_assessmentControlsCountByNoncompliantEvidence,

    -- ** InsightsByAssessment
    insightsByAssessment_totalAssessmentControlsCount,
    insightsByAssessment_compliantEvidenceCount,
    insightsByAssessment_lastUpdated,
    insightsByAssessment_inconclusiveEvidenceCount,
    insightsByAssessment_noncompliantEvidenceCount,
    insightsByAssessment_assessmentControlsCountByNoncompliantEvidence,

    -- ** ManualEvidence
    manualEvidence_s3ResourcePath,

    -- ** Notification
    notification_assessmentId,
    notification_assessmentName,
    notification_description,
    notification_id,
    notification_source,
    notification_controlSetName,
    notification_controlSetId,
    notification_eventTime,

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
    serviceMetadata_name,
    serviceMetadata_displayName,
    serviceMetadata_description,
    serviceMetadata_category,

    -- ** Settings
    settings_defaultProcessOwners,
    settings_snsTopic,
    settings_isAwsOrgEnabled,
    settings_kmsKey,
    settings_evidenceFinderEnablement,
    settings_defaultAssessmentReportsDestination,

    -- ** SourceKeyword
    sourceKeyword_keywordValue,
    sourceKeyword_keywordInputType,

    -- ** URL
    url_link,
    url_hyperlinkName,

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
