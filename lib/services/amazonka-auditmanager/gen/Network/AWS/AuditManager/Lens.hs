{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AuditManager.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Lens
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
import Amazonka.AuditManager.GetOrganizationAdminAccount
import Amazonka.AuditManager.GetServicesInScope
import Amazonka.AuditManager.GetSettings
import Amazonka.AuditManager.ListAssessmentFrameworks
import Amazonka.AuditManager.ListAssessmentReports
import Amazonka.AuditManager.ListAssessments
import Amazonka.AuditManager.ListControls
import Amazonka.AuditManager.ListKeywordsForDataSource
import Amazonka.AuditManager.ListNotifications
import Amazonka.AuditManager.ListTagsForResource
import Amazonka.AuditManager.RegisterAccount
import Amazonka.AuditManager.RegisterOrganizationAdminAccount
import Amazonka.AuditManager.TagResource
import Amazonka.AuditManager.Types.AWSAccount
import Amazonka.AuditManager.Types.AWSService
import Amazonka.AuditManager.Types.Assessment
import Amazonka.AuditManager.Types.AssessmentControl
import Amazonka.AuditManager.Types.AssessmentControlSet
import Amazonka.AuditManager.Types.AssessmentEvidenceFolder
import Amazonka.AuditManager.Types.AssessmentFramework
import Amazonka.AuditManager.Types.AssessmentFrameworkMetadata
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
import Amazonka.AuditManager.Types.Framework
import Amazonka.AuditManager.Types.FrameworkMetadata
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
import Amazonka.AuditManager.UpdateAssessmentStatus
import Amazonka.AuditManager.UpdateControl
import Amazonka.AuditManager.UpdateSettings
import Amazonka.AuditManager.ValidateAssessmentReportIntegrity
