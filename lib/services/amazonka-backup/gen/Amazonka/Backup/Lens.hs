{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Backup.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Lens
  ( -- * Operations

    -- ** CancelLegalHold
    cancelLegalHold_retainRecordInDays,
    cancelLegalHold_legalHoldId,
    cancelLegalHold_cancelDescription,
    cancelLegalHoldResponse_httpStatus,

    -- ** CreateBackupPlan
    createBackupPlan_backupPlanTags,
    createBackupPlan_creatorRequestId,
    createBackupPlan_backupPlan,
    createBackupPlanResponse_advancedBackupSettings,
    createBackupPlanResponse_backupPlanArn,
    createBackupPlanResponse_backupPlanId,
    createBackupPlanResponse_creationDate,
    createBackupPlanResponse_versionId,
    createBackupPlanResponse_httpStatus,

    -- ** CreateBackupSelection
    createBackupSelection_creatorRequestId,
    createBackupSelection_backupPlanId,
    createBackupSelection_backupSelection,
    createBackupSelectionResponse_backupPlanId,
    createBackupSelectionResponse_creationDate,
    createBackupSelectionResponse_selectionId,
    createBackupSelectionResponse_httpStatus,

    -- ** CreateBackupVault
    createBackupVault_backupVaultTags,
    createBackupVault_creatorRequestId,
    createBackupVault_encryptionKeyArn,
    createBackupVault_backupVaultName,
    createBackupVaultResponse_backupVaultArn,
    createBackupVaultResponse_backupVaultName,
    createBackupVaultResponse_creationDate,
    createBackupVaultResponse_httpStatus,

    -- ** CreateFramework
    createFramework_frameworkDescription,
    createFramework_frameworkTags,
    createFramework_idempotencyToken,
    createFramework_frameworkName,
    createFramework_frameworkControls,
    createFrameworkResponse_frameworkArn,
    createFrameworkResponse_frameworkName,
    createFrameworkResponse_httpStatus,

    -- ** CreateLegalHold
    createLegalHold_idempotencyToken,
    createLegalHold_recoveryPointSelection,
    createLegalHold_tags,
    createLegalHold_title,
    createLegalHold_description,
    createLegalHoldResponse_creationDate,
    createLegalHoldResponse_description,
    createLegalHoldResponse_legalHoldArn,
    createLegalHoldResponse_legalHoldId,
    createLegalHoldResponse_recoveryPointSelection,
    createLegalHoldResponse_status,
    createLegalHoldResponse_title,
    createLegalHoldResponse_httpStatus,

    -- ** CreateReportPlan
    createReportPlan_idempotencyToken,
    createReportPlan_reportPlanDescription,
    createReportPlan_reportPlanTags,
    createReportPlan_reportPlanName,
    createReportPlan_reportDeliveryChannel,
    createReportPlan_reportSetting,
    createReportPlanResponse_creationTime,
    createReportPlanResponse_reportPlanArn,
    createReportPlanResponse_reportPlanName,
    createReportPlanResponse_httpStatus,

    -- ** DeleteBackupPlan
    deleteBackupPlan_backupPlanId,
    deleteBackupPlanResponse_backupPlanArn,
    deleteBackupPlanResponse_backupPlanId,
    deleteBackupPlanResponse_deletionDate,
    deleteBackupPlanResponse_versionId,
    deleteBackupPlanResponse_httpStatus,

    -- ** DeleteBackupSelection
    deleteBackupSelection_backupPlanId,
    deleteBackupSelection_selectionId,

    -- ** DeleteBackupVault
    deleteBackupVault_backupVaultName,

    -- ** DeleteBackupVaultAccessPolicy
    deleteBackupVaultAccessPolicy_backupVaultName,

    -- ** DeleteBackupVaultLockConfiguration
    deleteBackupVaultLockConfiguration_backupVaultName,

    -- ** DeleteBackupVaultNotifications
    deleteBackupVaultNotifications_backupVaultName,

    -- ** DeleteFramework
    deleteFramework_frameworkName,

    -- ** DeleteRecoveryPoint
    deleteRecoveryPoint_backupVaultName,
    deleteRecoveryPoint_recoveryPointArn,

    -- ** DeleteReportPlan
    deleteReportPlan_reportPlanName,

    -- ** DescribeBackupJob
    describeBackupJob_backupJobId,
    describeBackupJobResponse_accountId,
    describeBackupJobResponse_backupJobId,
    describeBackupJobResponse_backupOptions,
    describeBackupJobResponse_backupSizeInBytes,
    describeBackupJobResponse_backupType,
    describeBackupJobResponse_backupVaultArn,
    describeBackupJobResponse_backupVaultName,
    describeBackupJobResponse_bytesTransferred,
    describeBackupJobResponse_childJobsInState,
    describeBackupJobResponse_completionDate,
    describeBackupJobResponse_createdBy,
    describeBackupJobResponse_creationDate,
    describeBackupJobResponse_expectedCompletionDate,
    describeBackupJobResponse_iamRoleArn,
    describeBackupJobResponse_isParent,
    describeBackupJobResponse_numberOfChildJobs,
    describeBackupJobResponse_parentJobId,
    describeBackupJobResponse_percentDone,
    describeBackupJobResponse_recoveryPointArn,
    describeBackupJobResponse_resourceArn,
    describeBackupJobResponse_resourceName,
    describeBackupJobResponse_resourceType,
    describeBackupJobResponse_startBy,
    describeBackupJobResponse_state,
    describeBackupJobResponse_statusMessage,
    describeBackupJobResponse_httpStatus,

    -- ** DescribeBackupVault
    describeBackupVault_backupVaultName,
    describeBackupVaultResponse_backupVaultArn,
    describeBackupVaultResponse_backupVaultName,
    describeBackupVaultResponse_creationDate,
    describeBackupVaultResponse_creatorRequestId,
    describeBackupVaultResponse_encryptionKeyArn,
    describeBackupVaultResponse_lockDate,
    describeBackupVaultResponse_locked,
    describeBackupVaultResponse_maxRetentionDays,
    describeBackupVaultResponse_minRetentionDays,
    describeBackupVaultResponse_numberOfRecoveryPoints,
    describeBackupVaultResponse_httpStatus,

    -- ** DescribeCopyJob
    describeCopyJob_copyJobId,
    describeCopyJobResponse_copyJob,
    describeCopyJobResponse_httpStatus,

    -- ** DescribeFramework
    describeFramework_frameworkName,
    describeFrameworkResponse_creationTime,
    describeFrameworkResponse_deploymentStatus,
    describeFrameworkResponse_frameworkArn,
    describeFrameworkResponse_frameworkControls,
    describeFrameworkResponse_frameworkDescription,
    describeFrameworkResponse_frameworkName,
    describeFrameworkResponse_frameworkStatus,
    describeFrameworkResponse_idempotencyToken,
    describeFrameworkResponse_httpStatus,

    -- ** DescribeGlobalSettings
    describeGlobalSettingsResponse_globalSettings,
    describeGlobalSettingsResponse_lastUpdateTime,
    describeGlobalSettingsResponse_httpStatus,

    -- ** DescribeProtectedResource
    describeProtectedResource_resourceArn,
    describeProtectedResourceResponse_lastBackupTime,
    describeProtectedResourceResponse_resourceArn,
    describeProtectedResourceResponse_resourceName,
    describeProtectedResourceResponse_resourceType,
    describeProtectedResourceResponse_httpStatus,

    -- ** DescribeRecoveryPoint
    describeRecoveryPoint_backupVaultName,
    describeRecoveryPoint_recoveryPointArn,
    describeRecoveryPointResponse_backupSizeInBytes,
    describeRecoveryPointResponse_backupVaultArn,
    describeRecoveryPointResponse_backupVaultName,
    describeRecoveryPointResponse_calculatedLifecycle,
    describeRecoveryPointResponse_completionDate,
    describeRecoveryPointResponse_compositeMemberIdentifier,
    describeRecoveryPointResponse_createdBy,
    describeRecoveryPointResponse_creationDate,
    describeRecoveryPointResponse_encryptionKeyArn,
    describeRecoveryPointResponse_iamRoleArn,
    describeRecoveryPointResponse_isEncrypted,
    describeRecoveryPointResponse_isParent,
    describeRecoveryPointResponse_lastRestoreTime,
    describeRecoveryPointResponse_lifecycle,
    describeRecoveryPointResponse_parentRecoveryPointArn,
    describeRecoveryPointResponse_recoveryPointArn,
    describeRecoveryPointResponse_resourceArn,
    describeRecoveryPointResponse_resourceName,
    describeRecoveryPointResponse_resourceType,
    describeRecoveryPointResponse_sourceBackupVaultArn,
    describeRecoveryPointResponse_status,
    describeRecoveryPointResponse_statusMessage,
    describeRecoveryPointResponse_storageClass,
    describeRecoveryPointResponse_httpStatus,

    -- ** DescribeRegionSettings
    describeRegionSettingsResponse_resourceTypeManagementPreference,
    describeRegionSettingsResponse_resourceTypeOptInPreference,
    describeRegionSettingsResponse_httpStatus,

    -- ** DescribeReportJob
    describeReportJob_reportJobId,
    describeReportJobResponse_reportJob,
    describeReportJobResponse_httpStatus,

    -- ** DescribeReportPlan
    describeReportPlan_reportPlanName,
    describeReportPlanResponse_reportPlan,
    describeReportPlanResponse_httpStatus,

    -- ** DescribeRestoreJob
    describeRestoreJob_restoreJobId,
    describeRestoreJobResponse_accountId,
    describeRestoreJobResponse_backupSizeInBytes,
    describeRestoreJobResponse_completionDate,
    describeRestoreJobResponse_createdResourceArn,
    describeRestoreJobResponse_creationDate,
    describeRestoreJobResponse_expectedCompletionTimeMinutes,
    describeRestoreJobResponse_iamRoleArn,
    describeRestoreJobResponse_percentDone,
    describeRestoreJobResponse_recoveryPointArn,
    describeRestoreJobResponse_resourceType,
    describeRestoreJobResponse_restoreJobId,
    describeRestoreJobResponse_status,
    describeRestoreJobResponse_statusMessage,
    describeRestoreJobResponse_httpStatus,

    -- ** DisassociateRecoveryPoint
    disassociateRecoveryPoint_backupVaultName,
    disassociateRecoveryPoint_recoveryPointArn,

    -- ** DisassociateRecoveryPointFromParent
    disassociateRecoveryPointFromParent_backupVaultName,
    disassociateRecoveryPointFromParent_recoveryPointArn,

    -- ** ExportBackupPlanTemplate
    exportBackupPlanTemplate_backupPlanId,
    exportBackupPlanTemplateResponse_backupPlanTemplateJson,
    exportBackupPlanTemplateResponse_httpStatus,

    -- ** GetBackupPlan
    getBackupPlan_versionId,
    getBackupPlan_backupPlanId,
    getBackupPlanResponse_advancedBackupSettings,
    getBackupPlanResponse_backupPlan,
    getBackupPlanResponse_backupPlanArn,
    getBackupPlanResponse_backupPlanId,
    getBackupPlanResponse_creationDate,
    getBackupPlanResponse_creatorRequestId,
    getBackupPlanResponse_deletionDate,
    getBackupPlanResponse_lastExecutionDate,
    getBackupPlanResponse_versionId,
    getBackupPlanResponse_httpStatus,

    -- ** GetBackupPlanFromJSON
    getBackupPlanFromJSON_backupPlanTemplateJson,
    getBackupPlanFromJSONResponse_backupPlan,
    getBackupPlanFromJSONResponse_httpStatus,

    -- ** GetBackupPlanFromTemplate
    getBackupPlanFromTemplate_backupPlanTemplateId,
    getBackupPlanFromTemplateResponse_backupPlanDocument,
    getBackupPlanFromTemplateResponse_httpStatus,

    -- ** GetBackupSelection
    getBackupSelection_backupPlanId,
    getBackupSelection_selectionId,
    getBackupSelectionResponse_backupPlanId,
    getBackupSelectionResponse_backupSelection,
    getBackupSelectionResponse_creationDate,
    getBackupSelectionResponse_creatorRequestId,
    getBackupSelectionResponse_selectionId,
    getBackupSelectionResponse_httpStatus,

    -- ** GetBackupVaultAccessPolicy
    getBackupVaultAccessPolicy_backupVaultName,
    getBackupVaultAccessPolicyResponse_backupVaultArn,
    getBackupVaultAccessPolicyResponse_backupVaultName,
    getBackupVaultAccessPolicyResponse_policy,
    getBackupVaultAccessPolicyResponse_httpStatus,

    -- ** GetBackupVaultNotifications
    getBackupVaultNotifications_backupVaultName,
    getBackupVaultNotificationsResponse_backupVaultArn,
    getBackupVaultNotificationsResponse_backupVaultEvents,
    getBackupVaultNotificationsResponse_backupVaultName,
    getBackupVaultNotificationsResponse_sNSTopicArn,
    getBackupVaultNotificationsResponse_httpStatus,

    -- ** GetLegalHold
    getLegalHold_legalHoldId,
    getLegalHoldResponse_cancelDescription,
    getLegalHoldResponse_cancellationDate,
    getLegalHoldResponse_creationDate,
    getLegalHoldResponse_description,
    getLegalHoldResponse_legalHoldArn,
    getLegalHoldResponse_legalHoldId,
    getLegalHoldResponse_recoveryPointSelection,
    getLegalHoldResponse_retainRecordUntil,
    getLegalHoldResponse_status,
    getLegalHoldResponse_title,
    getLegalHoldResponse_httpStatus,

    -- ** GetRecoveryPointRestoreMetadata
    getRecoveryPointRestoreMetadata_backupVaultName,
    getRecoveryPointRestoreMetadata_recoveryPointArn,
    getRecoveryPointRestoreMetadataResponse_backupVaultArn,
    getRecoveryPointRestoreMetadataResponse_recoveryPointArn,
    getRecoveryPointRestoreMetadataResponse_restoreMetadata,
    getRecoveryPointRestoreMetadataResponse_httpStatus,

    -- ** GetSupportedResourceTypes
    getSupportedResourceTypesResponse_resourceTypes,
    getSupportedResourceTypesResponse_httpStatus,

    -- ** ListBackupJobs
    listBackupJobs_byAccountId,
    listBackupJobs_byBackupVaultName,
    listBackupJobs_byCompleteAfter,
    listBackupJobs_byCompleteBefore,
    listBackupJobs_byCreatedAfter,
    listBackupJobs_byCreatedBefore,
    listBackupJobs_byParentJobId,
    listBackupJobs_byResourceArn,
    listBackupJobs_byResourceType,
    listBackupJobs_byState,
    listBackupJobs_maxResults,
    listBackupJobs_nextToken,
    listBackupJobsResponse_backupJobs,
    listBackupJobsResponse_nextToken,
    listBackupJobsResponse_httpStatus,

    -- ** ListBackupPlanTemplates
    listBackupPlanTemplates_maxResults,
    listBackupPlanTemplates_nextToken,
    listBackupPlanTemplatesResponse_backupPlanTemplatesList,
    listBackupPlanTemplatesResponse_nextToken,
    listBackupPlanTemplatesResponse_httpStatus,

    -- ** ListBackupPlanVersions
    listBackupPlanVersions_maxResults,
    listBackupPlanVersions_nextToken,
    listBackupPlanVersions_backupPlanId,
    listBackupPlanVersionsResponse_backupPlanVersionsList,
    listBackupPlanVersionsResponse_nextToken,
    listBackupPlanVersionsResponse_httpStatus,

    -- ** ListBackupPlans
    listBackupPlans_includeDeleted,
    listBackupPlans_maxResults,
    listBackupPlans_nextToken,
    listBackupPlansResponse_backupPlansList,
    listBackupPlansResponse_nextToken,
    listBackupPlansResponse_httpStatus,

    -- ** ListBackupSelections
    listBackupSelections_maxResults,
    listBackupSelections_nextToken,
    listBackupSelections_backupPlanId,
    listBackupSelectionsResponse_backupSelectionsList,
    listBackupSelectionsResponse_nextToken,
    listBackupSelectionsResponse_httpStatus,

    -- ** ListBackupVaults
    listBackupVaults_maxResults,
    listBackupVaults_nextToken,
    listBackupVaultsResponse_backupVaultList,
    listBackupVaultsResponse_nextToken,
    listBackupVaultsResponse_httpStatus,

    -- ** ListCopyJobs
    listCopyJobs_byAccountId,
    listCopyJobs_byCompleteAfter,
    listCopyJobs_byCompleteBefore,
    listCopyJobs_byCreatedAfter,
    listCopyJobs_byCreatedBefore,
    listCopyJobs_byDestinationVaultArn,
    listCopyJobs_byParentJobId,
    listCopyJobs_byResourceArn,
    listCopyJobs_byResourceType,
    listCopyJobs_byState,
    listCopyJobs_maxResults,
    listCopyJobs_nextToken,
    listCopyJobsResponse_copyJobs,
    listCopyJobsResponse_nextToken,
    listCopyJobsResponse_httpStatus,

    -- ** ListFrameworks
    listFrameworks_maxResults,
    listFrameworks_nextToken,
    listFrameworksResponse_frameworks,
    listFrameworksResponse_nextToken,
    listFrameworksResponse_httpStatus,

    -- ** ListLegalHolds
    listLegalHolds_maxResults,
    listLegalHolds_nextToken,
    listLegalHoldsResponse_legalHolds,
    listLegalHoldsResponse_nextToken,
    listLegalHoldsResponse_httpStatus,

    -- ** ListProtectedResources
    listProtectedResources_maxResults,
    listProtectedResources_nextToken,
    listProtectedResourcesResponse_nextToken,
    listProtectedResourcesResponse_results,
    listProtectedResourcesResponse_httpStatus,

    -- ** ListRecoveryPointsByBackupVault
    listRecoveryPointsByBackupVault_byBackupPlanId,
    listRecoveryPointsByBackupVault_byCreatedAfter,
    listRecoveryPointsByBackupVault_byCreatedBefore,
    listRecoveryPointsByBackupVault_byParentRecoveryPointArn,
    listRecoveryPointsByBackupVault_byResourceArn,
    listRecoveryPointsByBackupVault_byResourceType,
    listRecoveryPointsByBackupVault_maxResults,
    listRecoveryPointsByBackupVault_nextToken,
    listRecoveryPointsByBackupVault_backupVaultName,
    listRecoveryPointsByBackupVaultResponse_nextToken,
    listRecoveryPointsByBackupVaultResponse_recoveryPoints,
    listRecoveryPointsByBackupVaultResponse_httpStatus,

    -- ** ListRecoveryPointsByLegalHold
    listRecoveryPointsByLegalHold_maxResults,
    listRecoveryPointsByLegalHold_nextToken,
    listRecoveryPointsByLegalHold_legalHoldId,
    listRecoveryPointsByLegalHoldResponse_nextToken,
    listRecoveryPointsByLegalHoldResponse_recoveryPoints,
    listRecoveryPointsByLegalHoldResponse_httpStatus,

    -- ** ListRecoveryPointsByResource
    listRecoveryPointsByResource_maxResults,
    listRecoveryPointsByResource_nextToken,
    listRecoveryPointsByResource_resourceArn,
    listRecoveryPointsByResourceResponse_nextToken,
    listRecoveryPointsByResourceResponse_recoveryPoints,
    listRecoveryPointsByResourceResponse_httpStatus,

    -- ** ListReportJobs
    listReportJobs_byCreationAfter,
    listReportJobs_byCreationBefore,
    listReportJobs_byReportPlanName,
    listReportJobs_byStatus,
    listReportJobs_maxResults,
    listReportJobs_nextToken,
    listReportJobsResponse_nextToken,
    listReportJobsResponse_reportJobs,
    listReportJobsResponse_httpStatus,

    -- ** ListReportPlans
    listReportPlans_maxResults,
    listReportPlans_nextToken,
    listReportPlansResponse_nextToken,
    listReportPlansResponse_reportPlans,
    listReportPlansResponse_httpStatus,

    -- ** ListRestoreJobs
    listRestoreJobs_byAccountId,
    listRestoreJobs_byCompleteAfter,
    listRestoreJobs_byCompleteBefore,
    listRestoreJobs_byCreatedAfter,
    listRestoreJobs_byCreatedBefore,
    listRestoreJobs_byStatus,
    listRestoreJobs_maxResults,
    listRestoreJobs_nextToken,
    listRestoreJobsResponse_nextToken,
    listRestoreJobsResponse_restoreJobs,
    listRestoreJobsResponse_httpStatus,

    -- ** ListTags
    listTags_maxResults,
    listTags_nextToken,
    listTags_resourceArn,
    listTagsResponse_nextToken,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,

    -- ** PutBackupVaultAccessPolicy
    putBackupVaultAccessPolicy_policy,
    putBackupVaultAccessPolicy_backupVaultName,

    -- ** PutBackupVaultLockConfiguration
    putBackupVaultLockConfiguration_changeableForDays,
    putBackupVaultLockConfiguration_maxRetentionDays,
    putBackupVaultLockConfiguration_minRetentionDays,
    putBackupVaultLockConfiguration_backupVaultName,

    -- ** PutBackupVaultNotifications
    putBackupVaultNotifications_backupVaultName,
    putBackupVaultNotifications_sNSTopicArn,
    putBackupVaultNotifications_backupVaultEvents,

    -- ** StartBackupJob
    startBackupJob_backupOptions,
    startBackupJob_completeWindowMinutes,
    startBackupJob_idempotencyToken,
    startBackupJob_lifecycle,
    startBackupJob_recoveryPointTags,
    startBackupJob_startWindowMinutes,
    startBackupJob_backupVaultName,
    startBackupJob_resourceArn,
    startBackupJob_iamRoleArn,
    startBackupJobResponse_backupJobId,
    startBackupJobResponse_creationDate,
    startBackupJobResponse_isParent,
    startBackupJobResponse_recoveryPointArn,
    startBackupJobResponse_httpStatus,

    -- ** StartCopyJob
    startCopyJob_idempotencyToken,
    startCopyJob_lifecycle,
    startCopyJob_recoveryPointArn,
    startCopyJob_sourceBackupVaultName,
    startCopyJob_destinationBackupVaultArn,
    startCopyJob_iamRoleArn,
    startCopyJobResponse_copyJobId,
    startCopyJobResponse_creationDate,
    startCopyJobResponse_isParent,
    startCopyJobResponse_httpStatus,

    -- ** StartReportJob
    startReportJob_idempotencyToken,
    startReportJob_reportPlanName,
    startReportJobResponse_reportJobId,
    startReportJobResponse_httpStatus,

    -- ** StartRestoreJob
    startRestoreJob_copySourceTagsToRestoredResource,
    startRestoreJob_iamRoleArn,
    startRestoreJob_idempotencyToken,
    startRestoreJob_resourceType,
    startRestoreJob_recoveryPointArn,
    startRestoreJob_metadata,
    startRestoreJobResponse_restoreJobId,
    startRestoreJobResponse_httpStatus,

    -- ** StopBackupJob
    stopBackupJob_backupJobId,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeyList,

    -- ** UpdateBackupPlan
    updateBackupPlan_backupPlanId,
    updateBackupPlan_backupPlan,
    updateBackupPlanResponse_advancedBackupSettings,
    updateBackupPlanResponse_backupPlanArn,
    updateBackupPlanResponse_backupPlanId,
    updateBackupPlanResponse_creationDate,
    updateBackupPlanResponse_versionId,
    updateBackupPlanResponse_httpStatus,

    -- ** UpdateFramework
    updateFramework_frameworkControls,
    updateFramework_frameworkDescription,
    updateFramework_idempotencyToken,
    updateFramework_frameworkName,
    updateFrameworkResponse_creationTime,
    updateFrameworkResponse_frameworkArn,
    updateFrameworkResponse_frameworkName,
    updateFrameworkResponse_httpStatus,

    -- ** UpdateGlobalSettings
    updateGlobalSettings_globalSettings,

    -- ** UpdateRecoveryPointLifecycle
    updateRecoveryPointLifecycle_lifecycle,
    updateRecoveryPointLifecycle_backupVaultName,
    updateRecoveryPointLifecycle_recoveryPointArn,
    updateRecoveryPointLifecycleResponse_backupVaultArn,
    updateRecoveryPointLifecycleResponse_calculatedLifecycle,
    updateRecoveryPointLifecycleResponse_lifecycle,
    updateRecoveryPointLifecycleResponse_recoveryPointArn,
    updateRecoveryPointLifecycleResponse_httpStatus,

    -- ** UpdateRegionSettings
    updateRegionSettings_resourceTypeManagementPreference,
    updateRegionSettings_resourceTypeOptInPreference,

    -- ** UpdateReportPlan
    updateReportPlan_idempotencyToken,
    updateReportPlan_reportDeliveryChannel,
    updateReportPlan_reportPlanDescription,
    updateReportPlan_reportSetting,
    updateReportPlan_reportPlanName,
    updateReportPlanResponse_creationTime,
    updateReportPlanResponse_reportPlanArn,
    updateReportPlanResponse_reportPlanName,
    updateReportPlanResponse_httpStatus,

    -- * Types

    -- ** AdvancedBackupSetting
    advancedBackupSetting_backupOptions,
    advancedBackupSetting_resourceType,

    -- ** BackupJob
    backupJob_accountId,
    backupJob_backupJobId,
    backupJob_backupOptions,
    backupJob_backupSizeInBytes,
    backupJob_backupType,
    backupJob_backupVaultArn,
    backupJob_backupVaultName,
    backupJob_bytesTransferred,
    backupJob_completionDate,
    backupJob_createdBy,
    backupJob_creationDate,
    backupJob_expectedCompletionDate,
    backupJob_iamRoleArn,
    backupJob_isParent,
    backupJob_parentJobId,
    backupJob_percentDone,
    backupJob_recoveryPointArn,
    backupJob_resourceArn,
    backupJob_resourceName,
    backupJob_resourceType,
    backupJob_startBy,
    backupJob_state,
    backupJob_statusMessage,

    -- ** BackupPlan
    backupPlan_advancedBackupSettings,
    backupPlan_backupPlanName,
    backupPlan_rules,

    -- ** BackupPlanInput
    backupPlanInput_advancedBackupSettings,
    backupPlanInput_backupPlanName,
    backupPlanInput_rules,

    -- ** BackupPlanTemplatesListMember
    backupPlanTemplatesListMember_backupPlanTemplateId,
    backupPlanTemplatesListMember_backupPlanTemplateName,

    -- ** BackupPlansListMember
    backupPlansListMember_advancedBackupSettings,
    backupPlansListMember_backupPlanArn,
    backupPlansListMember_backupPlanId,
    backupPlansListMember_backupPlanName,
    backupPlansListMember_creationDate,
    backupPlansListMember_creatorRequestId,
    backupPlansListMember_deletionDate,
    backupPlansListMember_lastExecutionDate,
    backupPlansListMember_versionId,

    -- ** BackupRule
    backupRule_completionWindowMinutes,
    backupRule_copyActions,
    backupRule_enableContinuousBackup,
    backupRule_lifecycle,
    backupRule_recoveryPointTags,
    backupRule_ruleId,
    backupRule_scheduleExpression,
    backupRule_startWindowMinutes,
    backupRule_ruleName,
    backupRule_targetBackupVaultName,

    -- ** BackupRuleInput
    backupRuleInput_completionWindowMinutes,
    backupRuleInput_copyActions,
    backupRuleInput_enableContinuousBackup,
    backupRuleInput_lifecycle,
    backupRuleInput_recoveryPointTags,
    backupRuleInput_scheduleExpression,
    backupRuleInput_startWindowMinutes,
    backupRuleInput_ruleName,
    backupRuleInput_targetBackupVaultName,

    -- ** BackupSelection
    backupSelection_conditions,
    backupSelection_listOfTags,
    backupSelection_notResources,
    backupSelection_resources,
    backupSelection_selectionName,
    backupSelection_iamRoleArn,

    -- ** BackupSelectionsListMember
    backupSelectionsListMember_backupPlanId,
    backupSelectionsListMember_creationDate,
    backupSelectionsListMember_creatorRequestId,
    backupSelectionsListMember_iamRoleArn,
    backupSelectionsListMember_selectionId,
    backupSelectionsListMember_selectionName,

    -- ** BackupVaultListMember
    backupVaultListMember_backupVaultArn,
    backupVaultListMember_backupVaultName,
    backupVaultListMember_creationDate,
    backupVaultListMember_creatorRequestId,
    backupVaultListMember_encryptionKeyArn,
    backupVaultListMember_lockDate,
    backupVaultListMember_locked,
    backupVaultListMember_maxRetentionDays,
    backupVaultListMember_minRetentionDays,
    backupVaultListMember_numberOfRecoveryPoints,

    -- ** CalculatedLifecycle
    calculatedLifecycle_deleteAt,
    calculatedLifecycle_moveToColdStorageAt,

    -- ** Condition
    condition_conditionType,
    condition_conditionKey,
    condition_conditionValue,

    -- ** ConditionParameter
    conditionParameter_conditionKey,
    conditionParameter_conditionValue,

    -- ** Conditions
    conditions_stringEquals,
    conditions_stringLike,
    conditions_stringNotEquals,
    conditions_stringNotLike,

    -- ** ControlInputParameter
    controlInputParameter_parameterName,
    controlInputParameter_parameterValue,

    -- ** ControlScope
    controlScope_complianceResourceIds,
    controlScope_complianceResourceTypes,
    controlScope_tags,

    -- ** CopyAction
    copyAction_lifecycle,
    copyAction_destinationBackupVaultArn,

    -- ** CopyJob
    copyJob_accountId,
    copyJob_backupSizeInBytes,
    copyJob_childJobsInState,
    copyJob_completionDate,
    copyJob_compositeMemberIdentifier,
    copyJob_copyJobId,
    copyJob_createdBy,
    copyJob_creationDate,
    copyJob_destinationBackupVaultArn,
    copyJob_destinationRecoveryPointArn,
    copyJob_iamRoleArn,
    copyJob_isParent,
    copyJob_numberOfChildJobs,
    copyJob_parentJobId,
    copyJob_resourceArn,
    copyJob_resourceName,
    copyJob_resourceType,
    copyJob_sourceBackupVaultArn,
    copyJob_sourceRecoveryPointArn,
    copyJob_state,
    copyJob_statusMessage,

    -- ** DateRange
    dateRange_fromDate,
    dateRange_toDate,

    -- ** Framework
    framework_creationTime,
    framework_deploymentStatus,
    framework_frameworkArn,
    framework_frameworkDescription,
    framework_frameworkName,
    framework_numberOfControls,

    -- ** FrameworkControl
    frameworkControl_controlInputParameters,
    frameworkControl_controlScope,
    frameworkControl_controlName,

    -- ** LegalHold
    legalHold_cancellationDate,
    legalHold_creationDate,
    legalHold_description,
    legalHold_legalHoldArn,
    legalHold_legalHoldId,
    legalHold_status,
    legalHold_title,

    -- ** Lifecycle
    lifecycle_deleteAfterDays,
    lifecycle_moveToColdStorageAfterDays,

    -- ** ProtectedResource
    protectedResource_lastBackupTime,
    protectedResource_resourceArn,
    protectedResource_resourceName,
    protectedResource_resourceType,

    -- ** RecoveryPointByBackupVault
    recoveryPointByBackupVault_backupSizeInBytes,
    recoveryPointByBackupVault_backupVaultArn,
    recoveryPointByBackupVault_backupVaultName,
    recoveryPointByBackupVault_calculatedLifecycle,
    recoveryPointByBackupVault_completionDate,
    recoveryPointByBackupVault_compositeMemberIdentifier,
    recoveryPointByBackupVault_createdBy,
    recoveryPointByBackupVault_creationDate,
    recoveryPointByBackupVault_encryptionKeyArn,
    recoveryPointByBackupVault_iamRoleArn,
    recoveryPointByBackupVault_isEncrypted,
    recoveryPointByBackupVault_isParent,
    recoveryPointByBackupVault_lastRestoreTime,
    recoveryPointByBackupVault_lifecycle,
    recoveryPointByBackupVault_parentRecoveryPointArn,
    recoveryPointByBackupVault_recoveryPointArn,
    recoveryPointByBackupVault_resourceArn,
    recoveryPointByBackupVault_resourceName,
    recoveryPointByBackupVault_resourceType,
    recoveryPointByBackupVault_sourceBackupVaultArn,
    recoveryPointByBackupVault_status,
    recoveryPointByBackupVault_statusMessage,

    -- ** RecoveryPointByResource
    recoveryPointByResource_backupSizeBytes,
    recoveryPointByResource_backupVaultName,
    recoveryPointByResource_creationDate,
    recoveryPointByResource_encryptionKeyArn,
    recoveryPointByResource_isParent,
    recoveryPointByResource_parentRecoveryPointArn,
    recoveryPointByResource_recoveryPointArn,
    recoveryPointByResource_resourceName,
    recoveryPointByResource_status,
    recoveryPointByResource_statusMessage,

    -- ** RecoveryPointCreator
    recoveryPointCreator_backupPlanArn,
    recoveryPointCreator_backupPlanId,
    recoveryPointCreator_backupPlanVersion,
    recoveryPointCreator_backupRuleId,

    -- ** RecoveryPointMember
    recoveryPointMember_backupVaultName,
    recoveryPointMember_recoveryPointArn,
    recoveryPointMember_resourceArn,
    recoveryPointMember_resourceType,

    -- ** RecoveryPointSelection
    recoveryPointSelection_dateRange,
    recoveryPointSelection_resourceIdentifiers,
    recoveryPointSelection_vaultNames,

    -- ** ReportDeliveryChannel
    reportDeliveryChannel_formats,
    reportDeliveryChannel_s3KeyPrefix,
    reportDeliveryChannel_s3BucketName,

    -- ** ReportDestination
    reportDestination_s3BucketName,
    reportDestination_s3Keys,

    -- ** ReportJob
    reportJob_completionTime,
    reportJob_creationTime,
    reportJob_reportDestination,
    reportJob_reportJobId,
    reportJob_reportPlanArn,
    reportJob_reportTemplate,
    reportJob_status,
    reportJob_statusMessage,

    -- ** ReportPlan
    reportPlan_creationTime,
    reportPlan_deploymentStatus,
    reportPlan_lastAttemptedExecutionTime,
    reportPlan_lastSuccessfulExecutionTime,
    reportPlan_reportDeliveryChannel,
    reportPlan_reportPlanArn,
    reportPlan_reportPlanDescription,
    reportPlan_reportPlanName,
    reportPlan_reportSetting,

    -- ** ReportSetting
    reportSetting_accounts,
    reportSetting_frameworkArns,
    reportSetting_numberOfFrameworks,
    reportSetting_organizationUnits,
    reportSetting_regions,
    reportSetting_reportTemplate,

    -- ** RestoreJobsListMember
    restoreJobsListMember_accountId,
    restoreJobsListMember_backupSizeInBytes,
    restoreJobsListMember_completionDate,
    restoreJobsListMember_createdResourceArn,
    restoreJobsListMember_creationDate,
    restoreJobsListMember_expectedCompletionTimeMinutes,
    restoreJobsListMember_iamRoleArn,
    restoreJobsListMember_percentDone,
    restoreJobsListMember_recoveryPointArn,
    restoreJobsListMember_resourceType,
    restoreJobsListMember_restoreJobId,
    restoreJobsListMember_status,
    restoreJobsListMember_statusMessage,
  )
where

import Amazonka.Backup.CancelLegalHold
import Amazonka.Backup.CreateBackupPlan
import Amazonka.Backup.CreateBackupSelection
import Amazonka.Backup.CreateBackupVault
import Amazonka.Backup.CreateFramework
import Amazonka.Backup.CreateLegalHold
import Amazonka.Backup.CreateReportPlan
import Amazonka.Backup.DeleteBackupPlan
import Amazonka.Backup.DeleteBackupSelection
import Amazonka.Backup.DeleteBackupVault
import Amazonka.Backup.DeleteBackupVaultAccessPolicy
import Amazonka.Backup.DeleteBackupVaultLockConfiguration
import Amazonka.Backup.DeleteBackupVaultNotifications
import Amazonka.Backup.DeleteFramework
import Amazonka.Backup.DeleteRecoveryPoint
import Amazonka.Backup.DeleteReportPlan
import Amazonka.Backup.DescribeBackupJob
import Amazonka.Backup.DescribeBackupVault
import Amazonka.Backup.DescribeCopyJob
import Amazonka.Backup.DescribeFramework
import Amazonka.Backup.DescribeGlobalSettings
import Amazonka.Backup.DescribeProtectedResource
import Amazonka.Backup.DescribeRecoveryPoint
import Amazonka.Backup.DescribeRegionSettings
import Amazonka.Backup.DescribeReportJob
import Amazonka.Backup.DescribeReportPlan
import Amazonka.Backup.DescribeRestoreJob
import Amazonka.Backup.DisassociateRecoveryPoint
import Amazonka.Backup.DisassociateRecoveryPointFromParent
import Amazonka.Backup.ExportBackupPlanTemplate
import Amazonka.Backup.GetBackupPlan
import Amazonka.Backup.GetBackupPlanFromJSON
import Amazonka.Backup.GetBackupPlanFromTemplate
import Amazonka.Backup.GetBackupSelection
import Amazonka.Backup.GetBackupVaultAccessPolicy
import Amazonka.Backup.GetBackupVaultNotifications
import Amazonka.Backup.GetLegalHold
import Amazonka.Backup.GetRecoveryPointRestoreMetadata
import Amazonka.Backup.GetSupportedResourceTypes
import Amazonka.Backup.ListBackupJobs
import Amazonka.Backup.ListBackupPlanTemplates
import Amazonka.Backup.ListBackupPlanVersions
import Amazonka.Backup.ListBackupPlans
import Amazonka.Backup.ListBackupSelections
import Amazonka.Backup.ListBackupVaults
import Amazonka.Backup.ListCopyJobs
import Amazonka.Backup.ListFrameworks
import Amazonka.Backup.ListLegalHolds
import Amazonka.Backup.ListProtectedResources
import Amazonka.Backup.ListRecoveryPointsByBackupVault
import Amazonka.Backup.ListRecoveryPointsByLegalHold
import Amazonka.Backup.ListRecoveryPointsByResource
import Amazonka.Backup.ListReportJobs
import Amazonka.Backup.ListReportPlans
import Amazonka.Backup.ListRestoreJobs
import Amazonka.Backup.ListTags
import Amazonka.Backup.PutBackupVaultAccessPolicy
import Amazonka.Backup.PutBackupVaultLockConfiguration
import Amazonka.Backup.PutBackupVaultNotifications
import Amazonka.Backup.StartBackupJob
import Amazonka.Backup.StartCopyJob
import Amazonka.Backup.StartReportJob
import Amazonka.Backup.StartRestoreJob
import Amazonka.Backup.StopBackupJob
import Amazonka.Backup.TagResource
import Amazonka.Backup.Types.AdvancedBackupSetting
import Amazonka.Backup.Types.BackupJob
import Amazonka.Backup.Types.BackupPlan
import Amazonka.Backup.Types.BackupPlanInput
import Amazonka.Backup.Types.BackupPlanTemplatesListMember
import Amazonka.Backup.Types.BackupPlansListMember
import Amazonka.Backup.Types.BackupRule
import Amazonka.Backup.Types.BackupRuleInput
import Amazonka.Backup.Types.BackupSelection
import Amazonka.Backup.Types.BackupSelectionsListMember
import Amazonka.Backup.Types.BackupVaultListMember
import Amazonka.Backup.Types.CalculatedLifecycle
import Amazonka.Backup.Types.Condition
import Amazonka.Backup.Types.ConditionParameter
import Amazonka.Backup.Types.Conditions
import Amazonka.Backup.Types.ControlInputParameter
import Amazonka.Backup.Types.ControlScope
import Amazonka.Backup.Types.CopyAction
import Amazonka.Backup.Types.CopyJob
import Amazonka.Backup.Types.DateRange
import Amazonka.Backup.Types.Framework
import Amazonka.Backup.Types.FrameworkControl
import Amazonka.Backup.Types.LegalHold
import Amazonka.Backup.Types.Lifecycle
import Amazonka.Backup.Types.ProtectedResource
import Amazonka.Backup.Types.RecoveryPointByBackupVault
import Amazonka.Backup.Types.RecoveryPointByResource
import Amazonka.Backup.Types.RecoveryPointCreator
import Amazonka.Backup.Types.RecoveryPointMember
import Amazonka.Backup.Types.RecoveryPointSelection
import Amazonka.Backup.Types.ReportDeliveryChannel
import Amazonka.Backup.Types.ReportDestination
import Amazonka.Backup.Types.ReportJob
import Amazonka.Backup.Types.ReportPlan
import Amazonka.Backup.Types.ReportSetting
import Amazonka.Backup.Types.RestoreJobsListMember
import Amazonka.Backup.UntagResource
import Amazonka.Backup.UpdateBackupPlan
import Amazonka.Backup.UpdateFramework
import Amazonka.Backup.UpdateGlobalSettings
import Amazonka.Backup.UpdateRecoveryPointLifecycle
import Amazonka.Backup.UpdateRegionSettings
import Amazonka.Backup.UpdateReportPlan
