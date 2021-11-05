{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AuditManager.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AuditManager.Lens
  ( -- * Operations

    -- ** CreateAssessmentReport
    createAssessmentReport_description,
    createAssessmentReport_name,
    createAssessmentReport_assessmentId,
    createAssessmentReportResponse_assessmentReport,
    createAssessmentReportResponse_httpStatus,

    -- ** RegisterOrganizationAdminAccount
    registerOrganizationAdminAccount_adminAccountId,
    registerOrganizationAdminAccountResponse_adminAccountId,
    registerOrganizationAdminAccountResponse_organizationId,
    registerOrganizationAdminAccountResponse_httpStatus,

    -- ** ListNotifications
    listNotifications_nextToken,
    listNotifications_maxResults,
    listNotificationsResponse_nextToken,
    listNotificationsResponse_notifications,
    listNotificationsResponse_httpStatus,

    -- ** BatchCreateDelegationByAssessment
    batchCreateDelegationByAssessment_createDelegationRequests,
    batchCreateDelegationByAssessment_assessmentId,
    batchCreateDelegationByAssessmentResponse_delegations,
    batchCreateDelegationByAssessmentResponse_errors,
    batchCreateDelegationByAssessmentResponse_httpStatus,

    -- ** GetEvidenceFoldersByAssessmentControl
    getEvidenceFoldersByAssessmentControl_nextToken,
    getEvidenceFoldersByAssessmentControl_maxResults,
    getEvidenceFoldersByAssessmentControl_assessmentId,
    getEvidenceFoldersByAssessmentControl_controlSetId,
    getEvidenceFoldersByAssessmentControl_controlId,
    getEvidenceFoldersByAssessmentControlResponse_nextToken,
    getEvidenceFoldersByAssessmentControlResponse_evidenceFolders,
    getEvidenceFoldersByAssessmentControlResponse_httpStatus,

    -- ** BatchDeleteDelegationByAssessment
    batchDeleteDelegationByAssessment_delegationIds,
    batchDeleteDelegationByAssessment_assessmentId,
    batchDeleteDelegationByAssessmentResponse_errors,
    batchDeleteDelegationByAssessmentResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** GetEvidence
    getEvidence_assessmentId,
    getEvidence_controlSetId,
    getEvidence_evidenceFolderId,
    getEvidence_evidenceId,
    getEvidenceResponse_evidence,
    getEvidenceResponse_httpStatus,

    -- ** GetServicesInScope
    getServicesInScopeResponse_serviceMetadata,
    getServicesInScopeResponse_httpStatus,

    -- ** BatchDisassociateAssessmentReportEvidence
    batchDisassociateAssessmentReportEvidence_assessmentId,
    batchDisassociateAssessmentReportEvidence_evidenceFolderId,
    batchDisassociateAssessmentReportEvidence_evidenceIds,
    batchDisassociateAssessmentReportEvidenceResponse_evidenceIds,
    batchDisassociateAssessmentReportEvidenceResponse_errors,
    batchDisassociateAssessmentReportEvidenceResponse_httpStatus,

    -- ** DeregisterOrganizationAdminAccount
    deregisterOrganizationAdminAccount_adminAccountId,
    deregisterOrganizationAdminAccountResponse_httpStatus,

    -- ** GetAssessmentReportUrl
    getAssessmentReportUrl_assessmentReportId,
    getAssessmentReportUrl_assessmentId,
    getAssessmentReportUrlResponse_preSignedUrl,
    getAssessmentReportUrlResponse_httpStatus,

    -- ** UpdateAssessmentControl
    updateAssessmentControl_controlStatus,
    updateAssessmentControl_commentBody,
    updateAssessmentControl_assessmentId,
    updateAssessmentControl_controlSetId,
    updateAssessmentControl_controlId,
    updateAssessmentControlResponse_control,
    updateAssessmentControlResponse_httpStatus,

    -- ** DeleteAssessmentFramework
    deleteAssessmentFramework_frameworkId,
    deleteAssessmentFrameworkResponse_httpStatus,

    -- ** UpdateAssessmentFramework
    updateAssessmentFramework_complianceType,
    updateAssessmentFramework_description,
    updateAssessmentFramework_frameworkId,
    updateAssessmentFramework_name,
    updateAssessmentFramework_controlSets,
    updateAssessmentFrameworkResponse_framework,
    updateAssessmentFrameworkResponse_httpStatus,

    -- ** BatchAssociateAssessmentReportEvidence
    batchAssociateAssessmentReportEvidence_assessmentId,
    batchAssociateAssessmentReportEvidence_evidenceFolderId,
    batchAssociateAssessmentReportEvidence_evidenceIds,
    batchAssociateAssessmentReportEvidenceResponse_evidenceIds,
    batchAssociateAssessmentReportEvidenceResponse_errors,
    batchAssociateAssessmentReportEvidenceResponse_httpStatus,

    -- ** GetEvidenceByEvidenceFolder
    getEvidenceByEvidenceFolder_nextToken,
    getEvidenceByEvidenceFolder_maxResults,
    getEvidenceByEvidenceFolder_assessmentId,
    getEvidenceByEvidenceFolder_controlSetId,
    getEvidenceByEvidenceFolder_evidenceFolderId,
    getEvidenceByEvidenceFolderResponse_nextToken,
    getEvidenceByEvidenceFolderResponse_evidence,
    getEvidenceByEvidenceFolderResponse_httpStatus,

    -- ** CreateAssessmentFramework
    createAssessmentFramework_complianceType,
    createAssessmentFramework_description,
    createAssessmentFramework_tags,
    createAssessmentFramework_name,
    createAssessmentFramework_controlSets,
    createAssessmentFrameworkResponse_framework,
    createAssessmentFrameworkResponse_httpStatus,

    -- ** ListKeywordsForDataSource
    listKeywordsForDataSource_nextToken,
    listKeywordsForDataSource_maxResults,
    listKeywordsForDataSource_source,
    listKeywordsForDataSourceResponse_nextToken,
    listKeywordsForDataSourceResponse_keywords,
    listKeywordsForDataSourceResponse_httpStatus,

    -- ** ListAssessmentReports
    listAssessmentReports_nextToken,
    listAssessmentReports_maxResults,
    listAssessmentReportsResponse_assessmentReports,
    listAssessmentReportsResponse_nextToken,
    listAssessmentReportsResponse_httpStatus,

    -- ** ValidateAssessmentReportIntegrity
    validateAssessmentReportIntegrity_s3RelativePath,
    validateAssessmentReportIntegrityResponse_signatureValid,
    validateAssessmentReportIntegrityResponse_validationErrors,
    validateAssessmentReportIntegrityResponse_signatureDateTime,
    validateAssessmentReportIntegrityResponse_signatureAlgorithm,
    validateAssessmentReportIntegrityResponse_signatureKeyId,
    validateAssessmentReportIntegrityResponse_httpStatus,

    -- ** DeregisterAccount
    deregisterAccountResponse_status,
    deregisterAccountResponse_httpStatus,

    -- ** DeleteAssessmentReport
    deleteAssessmentReport_assessmentId,
    deleteAssessmentReport_assessmentReportId,
    deleteAssessmentReportResponse_httpStatus,

    -- ** UpdateSettings
    updateSettings_kmsKey,
    updateSettings_defaultAssessmentReportsDestination,
    updateSettings_snsTopic,
    updateSettings_defaultProcessOwners,
    updateSettingsResponse_settings,
    updateSettingsResponse_httpStatus,

    -- ** GetAssessmentFramework
    getAssessmentFramework_frameworkId,
    getAssessmentFrameworkResponse_framework,
    getAssessmentFrameworkResponse_httpStatus,

    -- ** DeleteAssessment
    deleteAssessment_assessmentId,
    deleteAssessmentResponse_httpStatus,

    -- ** GetChangeLogs
    getChangeLogs_controlSetId,
    getChangeLogs_nextToken,
    getChangeLogs_controlId,
    getChangeLogs_maxResults,
    getChangeLogs_assessmentId,
    getChangeLogsResponse_changeLogs,
    getChangeLogsResponse_nextToken,
    getChangeLogsResponse_httpStatus,

    -- ** UpdateAssessment
    updateAssessment_roles,
    updateAssessment_assessmentDescription,
    updateAssessment_assessmentReportsDestination,
    updateAssessment_assessmentName,
    updateAssessment_assessmentId,
    updateAssessment_scope,
    updateAssessmentResponse_assessment,
    updateAssessmentResponse_httpStatus,

    -- ** GetDelegations
    getDelegations_nextToken,
    getDelegations_maxResults,
    getDelegationsResponse_delegations,
    getDelegationsResponse_nextToken,
    getDelegationsResponse_httpStatus,

    -- ** DisassociateAssessmentReportEvidenceFolder
    disassociateAssessmentReportEvidenceFolder_assessmentId,
    disassociateAssessmentReportEvidenceFolder_evidenceFolderId,
    disassociateAssessmentReportEvidenceFolderResponse_httpStatus,

    -- ** ListAssessments
    listAssessments_nextToken,
    listAssessments_maxResults,
    listAssessmentsResponse_nextToken,
    listAssessmentsResponse_assessmentMetadata,
    listAssessmentsResponse_httpStatus,

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

    -- ** GetEvidenceFoldersByAssessment
    getEvidenceFoldersByAssessment_nextToken,
    getEvidenceFoldersByAssessment_maxResults,
    getEvidenceFoldersByAssessment_assessmentId,
    getEvidenceFoldersByAssessmentResponse_nextToken,
    getEvidenceFoldersByAssessmentResponse_evidenceFolders,
    getEvidenceFoldersByAssessmentResponse_httpStatus,

    -- ** RegisterAccount
    registerAccount_kmsKey,
    registerAccount_delegatedAdminAccount,
    registerAccountResponse_status,
    registerAccountResponse_httpStatus,

    -- ** GetAssessment
    getAssessment_assessmentId,
    getAssessmentResponse_userRole,
    getAssessmentResponse_assessment,
    getAssessmentResponse_httpStatus,

    -- ** BatchImportEvidenceToAssessmentControl
    batchImportEvidenceToAssessmentControl_assessmentId,
    batchImportEvidenceToAssessmentControl_controlSetId,
    batchImportEvidenceToAssessmentControl_controlId,
    batchImportEvidenceToAssessmentControl_manualEvidence,
    batchImportEvidenceToAssessmentControlResponse_errors,
    batchImportEvidenceToAssessmentControlResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** GetEvidenceFolder
    getEvidenceFolder_assessmentId,
    getEvidenceFolder_controlSetId,
    getEvidenceFolder_evidenceFolderId,
    getEvidenceFolderResponse_evidenceFolder,
    getEvidenceFolderResponse_httpStatus,

    -- ** ListAssessmentFrameworks
    listAssessmentFrameworks_nextToken,
    listAssessmentFrameworks_maxResults,
    listAssessmentFrameworks_frameworkType,
    listAssessmentFrameworksResponse_nextToken,
    listAssessmentFrameworksResponse_frameworkMetadataList,
    listAssessmentFrameworksResponse_httpStatus,

    -- ** CreateControl
    createControl_testingInformation,
    createControl_actionPlanInstructions,
    createControl_actionPlanTitle,
    createControl_description,
    createControl_tags,
    createControl_name,
    createControl_controlMappingSources,
    createControlResponse_control,
    createControlResponse_httpStatus,

    -- ** UpdateAssessmentStatus
    updateAssessmentStatus_assessmentId,
    updateAssessmentStatus_status,
    updateAssessmentStatusResponse_assessment,
    updateAssessmentStatusResponse_httpStatus,

    -- ** GetAccountStatus
    getAccountStatusResponse_status,
    getAccountStatusResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** GetSettings
    getSettings_attribute,
    getSettingsResponse_settings,
    getSettingsResponse_httpStatus,

    -- ** GetOrganizationAdminAccount
    getOrganizationAdminAccountResponse_adminAccountId,
    getOrganizationAdminAccountResponse_organizationId,
    getOrganizationAdminAccountResponse_httpStatus,

    -- ** DeleteControl
    deleteControl_controlId,
    deleteControlResponse_httpStatus,

    -- ** UpdateControl
    updateControl_testingInformation,
    updateControl_actionPlanInstructions,
    updateControl_actionPlanTitle,
    updateControl_description,
    updateControl_controlId,
    updateControl_name,
    updateControl_controlMappingSources,
    updateControlResponse_control,
    updateControlResponse_httpStatus,

    -- ** UpdateAssessmentControlSetStatus
    updateAssessmentControlSetStatus_assessmentId,
    updateAssessmentControlSetStatus_controlSetId,
    updateAssessmentControlSetStatus_status,
    updateAssessmentControlSetStatus_comment,
    updateAssessmentControlSetStatusResponse_controlSet,
    updateAssessmentControlSetStatusResponse_httpStatus,

    -- ** ListControls
    listControls_nextToken,
    listControls_maxResults,
    listControls_controlType,
    listControlsResponse_nextToken,
    listControlsResponse_controlMetadataList,
    listControlsResponse_httpStatus,

    -- ** AssociateAssessmentReportEvidenceFolder
    associateAssessmentReportEvidenceFolder_assessmentId,
    associateAssessmentReportEvidenceFolder_evidenceFolderId,
    associateAssessmentReportEvidenceFolderResponse_httpStatus,

    -- ** GetControl
    getControl_controlId,
    getControlResponse_control,
    getControlResponse_httpStatus,

    -- * Types

    -- ** AWSAccount
    aWSAccount_name,
    aWSAccount_emailAddress,
    aWSAccount_id,

    -- ** AWSService
    aWSService_serviceName,

    -- ** Assessment
    assessment_framework,
    assessment_arn,
    assessment_awsAccount,
    assessment_metadata,
    assessment_tags,

    -- ** AssessmentControl
    assessmentControl_status,
    assessmentControl_evidenceCount,
    assessmentControl_response,
    assessmentControl_name,
    assessmentControl_id,
    assessmentControl_evidenceSources,
    assessmentControl_comments,
    assessmentControl_assessmentReportEvidenceCount,
    assessmentControl_description,

    -- ** AssessmentControlSet
    assessmentControlSet_status,
    assessmentControlSet_controls,
    assessmentControlSet_roles,
    assessmentControlSet_manualEvidenceCount,
    assessmentControlSet_delegations,
    assessmentControlSet_systemEvidenceCount,
    assessmentControlSet_id,
    assessmentControlSet_description,

    -- ** AssessmentEvidenceFolder
    assessmentEvidenceFolder_evidenceByTypeComplianceCheckIssuesCount,
    assessmentEvidenceFolder_controlSetId,
    assessmentEvidenceFolder_assessmentReportSelectionCount,
    assessmentEvidenceFolder_totalEvidence,
    assessmentEvidenceFolder_evidenceByTypeManualCount,
    assessmentEvidenceFolder_date,
    assessmentEvidenceFolder_name,
    assessmentEvidenceFolder_evidenceByTypeUserActivityCount,
    assessmentEvidenceFolder_controlId,
    assessmentEvidenceFolder_evidenceAwsServiceSourceCount,
    assessmentEvidenceFolder_author,
    assessmentEvidenceFolder_id,
    assessmentEvidenceFolder_dataSource,
    assessmentEvidenceFolder_controlName,
    assessmentEvidenceFolder_evidenceByTypeComplianceCheckCount,
    assessmentEvidenceFolder_assessmentId,
    assessmentEvidenceFolder_evidenceByTypeConfigurationDataCount,
    assessmentEvidenceFolder_evidenceResourcesIncludedCount,

    -- ** AssessmentFramework
    assessmentFramework_arn,
    assessmentFramework_controlSets,
    assessmentFramework_metadata,
    assessmentFramework_id,

    -- ** AssessmentFrameworkMetadata
    assessmentFrameworkMetadata_controlsCount,
    assessmentFrameworkMetadata_lastUpdatedAt,
    assessmentFrameworkMetadata_arn,
    assessmentFrameworkMetadata_createdAt,
    assessmentFrameworkMetadata_name,
    assessmentFrameworkMetadata_complianceType,
    assessmentFrameworkMetadata_controlSetsCount,
    assessmentFrameworkMetadata_id,
    assessmentFrameworkMetadata_type,
    assessmentFrameworkMetadata_logo,
    assessmentFrameworkMetadata_description,

    -- ** AssessmentMetadata
    assessmentMetadata_creationTime,
    assessmentMetadata_status,
    assessmentMetadata_lastUpdated,
    assessmentMetadata_roles,
    assessmentMetadata_delegations,
    assessmentMetadata_name,
    assessmentMetadata_assessmentReportsDestination,
    assessmentMetadata_scope,
    assessmentMetadata_complianceType,
    assessmentMetadata_id,
    assessmentMetadata_description,

    -- ** AssessmentMetadataItem
    assessmentMetadataItem_creationTime,
    assessmentMetadataItem_status,
    assessmentMetadataItem_lastUpdated,
    assessmentMetadataItem_roles,
    assessmentMetadataItem_delegations,
    assessmentMetadataItem_name,
    assessmentMetadataItem_complianceType,
    assessmentMetadataItem_id,

    -- ** AssessmentReport
    assessmentReport_creationTime,
    assessmentReport_status,
    assessmentReport_awsAccountId,
    assessmentReport_name,
    assessmentReport_author,
    assessmentReport_id,
    assessmentReport_assessmentId,
    assessmentReport_description,
    assessmentReport_assessmentName,

    -- ** AssessmentReportEvidenceError
    assessmentReportEvidenceError_errorCode,
    assessmentReportEvidenceError_errorMessage,
    assessmentReportEvidenceError_evidenceId,

    -- ** AssessmentReportMetadata
    assessmentReportMetadata_creationTime,
    assessmentReportMetadata_status,
    assessmentReportMetadata_name,
    assessmentReportMetadata_author,
    assessmentReportMetadata_id,
    assessmentReportMetadata_assessmentId,
    assessmentReportMetadata_description,
    assessmentReportMetadata_assessmentName,

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
    changeLog_objectName,
    changeLog_createdAt,
    changeLog_objectType,
    changeLog_createdBy,
    changeLog_action,

    -- ** Control
    control_lastUpdatedBy,
    control_testingInformation,
    control_lastUpdatedAt,
    control_arn,
    control_createdAt,
    control_controlMappingSources,
    control_createdBy,
    control_actionPlanInstructions,
    control_controlSources,
    control_name,
    control_actionPlanTitle,
    control_id,
    control_type,
    control_description,
    control_tags,

    -- ** ControlComment
    controlComment_authorName,
    controlComment_postedDate,
    controlComment_commentBody,

    -- ** ControlMappingSource
    controlMappingSource_sourceName,
    controlMappingSource_sourceType,
    controlMappingSource_troubleshootingText,
    controlMappingSource_sourceId,
    controlMappingSource_sourceDescription,
    controlMappingSource_sourceFrequency,
    controlMappingSource_sourceKeyword,
    controlMappingSource_sourceSetUpOption,

    -- ** ControlMetadata
    controlMetadata_lastUpdatedAt,
    controlMetadata_arn,
    controlMetadata_createdAt,
    controlMetadata_controlSources,
    controlMetadata_name,
    controlMetadata_id,

    -- ** ControlSet
    controlSet_controls,
    controlSet_name,
    controlSet_id,

    -- ** CreateAssessmentFrameworkControl
    createAssessmentFrameworkControl_id,

    -- ** CreateAssessmentFrameworkControlSet
    createAssessmentFrameworkControlSet_controls,
    createAssessmentFrameworkControlSet_name,

    -- ** CreateControlMappingSource
    createControlMappingSource_sourceName,
    createControlMappingSource_sourceType,
    createControlMappingSource_troubleshootingText,
    createControlMappingSource_sourceDescription,
    createControlMappingSource_sourceFrequency,
    createControlMappingSource_sourceKeyword,
    createControlMappingSource_sourceSetUpOption,

    -- ** CreateDelegationRequest
    createDelegationRequest_roleType,
    createDelegationRequest_controlSetId,
    createDelegationRequest_comment,
    createDelegationRequest_roleArn,

    -- ** Delegation
    delegation_roleType,
    delegation_creationTime,
    delegation_status,
    delegation_lastUpdated,
    delegation_controlSetId,
    delegation_createdBy,
    delegation_id,
    delegation_assessmentId,
    delegation_comment,
    delegation_roleArn,
    delegation_assessmentName,

    -- ** DelegationMetadata
    delegationMetadata_creationTime,
    delegationMetadata_status,
    delegationMetadata_controlSetName,
    delegationMetadata_id,
    delegationMetadata_assessmentId,
    delegationMetadata_roleArn,
    delegationMetadata_assessmentName,

    -- ** Evidence
    evidence_time,
    evidence_assessmentReportSelection,
    evidence_evidenceByType,
    evidence_complianceCheck,
    evidence_awsOrganization,
    evidence_awsAccountId,
    evidence_attributes,
    evidence_evidenceAwsAccountId,
    evidence_id,
    evidence_dataSource,
    evidence_evidenceFolderId,
    evidence_iamId,
    evidence_eventName,
    evidence_resourcesIncluded,
    evidence_eventSource,

    -- ** Framework
    framework_lastUpdatedBy,
    framework_lastUpdatedAt,
    framework_arn,
    framework_createdAt,
    framework_createdBy,
    framework_controlSets,
    framework_controlSources,
    framework_name,
    framework_complianceType,
    framework_id,
    framework_type,
    framework_logo,
    framework_description,
    framework_tags,

    -- ** FrameworkMetadata
    frameworkMetadata_name,
    frameworkMetadata_complianceType,
    frameworkMetadata_logo,
    frameworkMetadata_description,

    -- ** ManualEvidence
    manualEvidence_s3ResourcePath,

    -- ** Notification
    notification_controlSetName,
    notification_controlSetId,
    notification_eventTime,
    notification_source,
    notification_id,
    notification_assessmentId,
    notification_description,
    notification_assessmentName,

    -- ** Resource
    resource_arn,
    resource_value,

    -- ** Role
    role_roleType,
    role_roleArn,

    -- ** Scope
    scope_awsAccounts,
    scope_awsServices,

    -- ** ServiceMetadata
    serviceMetadata_category,
    serviceMetadata_name,
    serviceMetadata_displayName,
    serviceMetadata_description,

    -- ** Settings
    settings_kmsKey,
    settings_defaultAssessmentReportsDestination,
    settings_snsTopic,
    settings_defaultProcessOwners,
    settings_isAwsOrgEnabled,

    -- ** SourceKeyword
    sourceKeyword_keywordInputType,
    sourceKeyword_keywordValue,

    -- ** URL
    url_link,
    url_hyperlinkName,

    -- ** UpdateAssessmentFrameworkControlSet
    updateAssessmentFrameworkControlSet_controls,
    updateAssessmentFrameworkControlSet_id,
    updateAssessmentFrameworkControlSet_name,
  )
where

import Network.AWS.AuditManager.AssociateAssessmentReportEvidenceFolder
import Network.AWS.AuditManager.BatchAssociateAssessmentReportEvidence
import Network.AWS.AuditManager.BatchCreateDelegationByAssessment
import Network.AWS.AuditManager.BatchDeleteDelegationByAssessment
import Network.AWS.AuditManager.BatchDisassociateAssessmentReportEvidence
import Network.AWS.AuditManager.BatchImportEvidenceToAssessmentControl
import Network.AWS.AuditManager.CreateAssessment
import Network.AWS.AuditManager.CreateAssessmentFramework
import Network.AWS.AuditManager.CreateAssessmentReport
import Network.AWS.AuditManager.CreateControl
import Network.AWS.AuditManager.DeleteAssessment
import Network.AWS.AuditManager.DeleteAssessmentFramework
import Network.AWS.AuditManager.DeleteAssessmentReport
import Network.AWS.AuditManager.DeleteControl
import Network.AWS.AuditManager.DeregisterAccount
import Network.AWS.AuditManager.DeregisterOrganizationAdminAccount
import Network.AWS.AuditManager.DisassociateAssessmentReportEvidenceFolder
import Network.AWS.AuditManager.GetAccountStatus
import Network.AWS.AuditManager.GetAssessment
import Network.AWS.AuditManager.GetAssessmentFramework
import Network.AWS.AuditManager.GetAssessmentReportUrl
import Network.AWS.AuditManager.GetChangeLogs
import Network.AWS.AuditManager.GetControl
import Network.AWS.AuditManager.GetDelegations
import Network.AWS.AuditManager.GetEvidence
import Network.AWS.AuditManager.GetEvidenceByEvidenceFolder
import Network.AWS.AuditManager.GetEvidenceFolder
import Network.AWS.AuditManager.GetEvidenceFoldersByAssessment
import Network.AWS.AuditManager.GetEvidenceFoldersByAssessmentControl
import Network.AWS.AuditManager.GetOrganizationAdminAccount
import Network.AWS.AuditManager.GetServicesInScope
import Network.AWS.AuditManager.GetSettings
import Network.AWS.AuditManager.ListAssessmentFrameworks
import Network.AWS.AuditManager.ListAssessmentReports
import Network.AWS.AuditManager.ListAssessments
import Network.AWS.AuditManager.ListControls
import Network.AWS.AuditManager.ListKeywordsForDataSource
import Network.AWS.AuditManager.ListNotifications
import Network.AWS.AuditManager.ListTagsForResource
import Network.AWS.AuditManager.RegisterAccount
import Network.AWS.AuditManager.RegisterOrganizationAdminAccount
import Network.AWS.AuditManager.TagResource
import Network.AWS.AuditManager.Types.AWSAccount
import Network.AWS.AuditManager.Types.AWSService
import Network.AWS.AuditManager.Types.Assessment
import Network.AWS.AuditManager.Types.AssessmentControl
import Network.AWS.AuditManager.Types.AssessmentControlSet
import Network.AWS.AuditManager.Types.AssessmentEvidenceFolder
import Network.AWS.AuditManager.Types.AssessmentFramework
import Network.AWS.AuditManager.Types.AssessmentFrameworkMetadata
import Network.AWS.AuditManager.Types.AssessmentMetadata
import Network.AWS.AuditManager.Types.AssessmentMetadataItem
import Network.AWS.AuditManager.Types.AssessmentReport
import Network.AWS.AuditManager.Types.AssessmentReportEvidenceError
import Network.AWS.AuditManager.Types.AssessmentReportMetadata
import Network.AWS.AuditManager.Types.AssessmentReportsDestination
import Network.AWS.AuditManager.Types.BatchCreateDelegationByAssessmentError
import Network.AWS.AuditManager.Types.BatchDeleteDelegationByAssessmentError
import Network.AWS.AuditManager.Types.BatchImportEvidenceToAssessmentControlError
import Network.AWS.AuditManager.Types.ChangeLog
import Network.AWS.AuditManager.Types.Control
import Network.AWS.AuditManager.Types.ControlComment
import Network.AWS.AuditManager.Types.ControlMappingSource
import Network.AWS.AuditManager.Types.ControlMetadata
import Network.AWS.AuditManager.Types.ControlSet
import Network.AWS.AuditManager.Types.CreateAssessmentFrameworkControl
import Network.AWS.AuditManager.Types.CreateAssessmentFrameworkControlSet
import Network.AWS.AuditManager.Types.CreateControlMappingSource
import Network.AWS.AuditManager.Types.CreateDelegationRequest
import Network.AWS.AuditManager.Types.Delegation
import Network.AWS.AuditManager.Types.DelegationMetadata
import Network.AWS.AuditManager.Types.Evidence
import Network.AWS.AuditManager.Types.Framework
import Network.AWS.AuditManager.Types.FrameworkMetadata
import Network.AWS.AuditManager.Types.ManualEvidence
import Network.AWS.AuditManager.Types.Notification
import Network.AWS.AuditManager.Types.Resource
import Network.AWS.AuditManager.Types.Role
import Network.AWS.AuditManager.Types.Scope
import Network.AWS.AuditManager.Types.ServiceMetadata
import Network.AWS.AuditManager.Types.Settings
import Network.AWS.AuditManager.Types.SourceKeyword
import Network.AWS.AuditManager.Types.URL
import Network.AWS.AuditManager.Types.UpdateAssessmentFrameworkControlSet
import Network.AWS.AuditManager.UntagResource
import Network.AWS.AuditManager.UpdateAssessment
import Network.AWS.AuditManager.UpdateAssessmentControl
import Network.AWS.AuditManager.UpdateAssessmentControlSetStatus
import Network.AWS.AuditManager.UpdateAssessmentFramework
import Network.AWS.AuditManager.UpdateAssessmentStatus
import Network.AWS.AuditManager.UpdateControl
import Network.AWS.AuditManager.UpdateSettings
import Network.AWS.AuditManager.ValidateAssessmentReportIntegrity
