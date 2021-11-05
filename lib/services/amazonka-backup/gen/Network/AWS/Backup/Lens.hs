{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Backup.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Lens
  ( -- * Operations

    -- ** UpdateBackupPlan
    updateBackupPlan_backupPlanId,
    updateBackupPlan_backupPlan,
    updateBackupPlanResponse_versionId,
    updateBackupPlanResponse_advancedBackupSettings,
    updateBackupPlanResponse_backupPlanId,
    updateBackupPlanResponse_backupPlanArn,
    updateBackupPlanResponse_creationDate,
    updateBackupPlanResponse_httpStatus,

    -- ** DeleteBackupPlan
    deleteBackupPlan_backupPlanId,
    deleteBackupPlanResponse_versionId,
    deleteBackupPlanResponse_backupPlanId,
    deleteBackupPlanResponse_backupPlanArn,
    deleteBackupPlanResponse_deletionDate,
    deleteBackupPlanResponse_httpStatus,

    -- ** DescribeBackupJob
    describeBackupJob_backupJobId,
    describeBackupJobResponse_iamRoleArn,
    describeBackupJobResponse_state,
    describeBackupJobResponse_resourceType,
    describeBackupJobResponse_percentDone,
    describeBackupJobResponse_startBy,
    describeBackupJobResponse_createdBy,
    describeBackupJobResponse_expectedCompletionDate,
    describeBackupJobResponse_bytesTransferred,
    describeBackupJobResponse_backupVaultArn,
    describeBackupJobResponse_accountId,
    describeBackupJobResponse_backupJobId,
    describeBackupJobResponse_resourceArn,
    describeBackupJobResponse_statusMessage,
    describeBackupJobResponse_recoveryPointArn,
    describeBackupJobResponse_backupSizeInBytes,
    describeBackupJobResponse_creationDate,
    describeBackupJobResponse_completionDate,
    describeBackupJobResponse_backupVaultName,
    describeBackupJobResponse_backupType,
    describeBackupJobResponse_backupOptions,
    describeBackupJobResponse_httpStatus,

    -- ** ListBackupPlanTemplates
    listBackupPlanTemplates_nextToken,
    listBackupPlanTemplates_maxResults,
    listBackupPlanTemplatesResponse_backupPlanTemplatesList,
    listBackupPlanTemplatesResponse_nextToken,
    listBackupPlanTemplatesResponse_httpStatus,

    -- ** DeleteReportPlan
    deleteReportPlan_reportPlanName,

    -- ** UpdateReportPlan
    updateReportPlan_idempotencyToken,
    updateReportPlan_reportSetting,
    updateReportPlan_reportPlanDescription,
    updateReportPlan_reportDeliveryChannel,
    updateReportPlan_reportPlanName,
    updateReportPlanResponse_creationTime,
    updateReportPlanResponse_reportPlanName,
    updateReportPlanResponse_reportPlanArn,
    updateReportPlanResponse_httpStatus,

    -- ** DescribeReportJob
    describeReportJob_reportJobId,
    describeReportJobResponse_reportJob,
    describeReportJobResponse_httpStatus,

    -- ** UpdateRegionSettings
    updateRegionSettings_resourceTypeOptInPreference,

    -- ** UpdateGlobalSettings
    updateGlobalSettings_globalSettings,

    -- ** DeleteBackupSelection
    deleteBackupSelection_backupPlanId,
    deleteBackupSelection_selectionId,

    -- ** DescribeCopyJob
    describeCopyJob_copyJobId,
    describeCopyJobResponse_copyJob,
    describeCopyJobResponse_httpStatus,

    -- ** DescribeRecoveryPoint
    describeRecoveryPoint_backupVaultName,
    describeRecoveryPoint_recoveryPointArn,
    describeRecoveryPointResponse_isEncrypted,
    describeRecoveryPointResponse_status,
    describeRecoveryPointResponse_iamRoleArn,
    describeRecoveryPointResponse_resourceType,
    describeRecoveryPointResponse_createdBy,
    describeRecoveryPointResponse_calculatedLifecycle,
    describeRecoveryPointResponse_lifecycle,
    describeRecoveryPointResponse_backupVaultArn,
    describeRecoveryPointResponse_sourceBackupVaultArn,
    describeRecoveryPointResponse_lastRestoreTime,
    describeRecoveryPointResponse_resourceArn,
    describeRecoveryPointResponse_statusMessage,
    describeRecoveryPointResponse_storageClass,
    describeRecoveryPointResponse_recoveryPointArn,
    describeRecoveryPointResponse_encryptionKeyArn,
    describeRecoveryPointResponse_backupSizeInBytes,
    describeRecoveryPointResponse_creationDate,
    describeRecoveryPointResponse_completionDate,
    describeRecoveryPointResponse_backupVaultName,
    describeRecoveryPointResponse_httpStatus,

    -- ** DescribeRestoreJob
    describeRestoreJob_restoreJobId,
    describeRestoreJobResponse_status,
    describeRestoreJobResponse_iamRoleArn,
    describeRestoreJobResponse_expectedCompletionTimeMinutes,
    describeRestoreJobResponse_restoreJobId,
    describeRestoreJobResponse_resourceType,
    describeRestoreJobResponse_percentDone,
    describeRestoreJobResponse_accountId,
    describeRestoreJobResponse_createdResourceArn,
    describeRestoreJobResponse_statusMessage,
    describeRestoreJobResponse_recoveryPointArn,
    describeRestoreJobResponse_backupSizeInBytes,
    describeRestoreJobResponse_creationDate,
    describeRestoreJobResponse_completionDate,
    describeRestoreJobResponse_httpStatus,

    -- ** StartCopyJob
    startCopyJob_idempotencyToken,
    startCopyJob_lifecycle,
    startCopyJob_recoveryPointArn,
    startCopyJob_sourceBackupVaultName,
    startCopyJob_destinationBackupVaultArn,
    startCopyJob_iamRoleArn,
    startCopyJobResponse_copyJobId,
    startCopyJobResponse_creationDate,
    startCopyJobResponse_httpStatus,

    -- ** GetBackupPlanFromTemplate
    getBackupPlanFromTemplate_backupPlanTemplateId,
    getBackupPlanFromTemplateResponse_backupPlanDocument,
    getBackupPlanFromTemplateResponse_httpStatus,

    -- ** DisassociateRecoveryPoint
    disassociateRecoveryPoint_backupVaultName,
    disassociateRecoveryPoint_recoveryPointArn,

    -- ** DeleteBackupVault
    deleteBackupVault_backupVaultName,

    -- ** DeleteFramework
    deleteFramework_frameworkName,

    -- ** UpdateFramework
    updateFramework_idempotencyToken,
    updateFramework_frameworkDescription,
    updateFramework_frameworkControls,
    updateFramework_frameworkName,
    updateFrameworkResponse_creationTime,
    updateFrameworkResponse_frameworkArn,
    updateFrameworkResponse_frameworkName,
    updateFrameworkResponse_httpStatus,

    -- ** ListReportJobs
    listReportJobs_byStatus,
    listReportJobs_byReportPlanName,
    listReportJobs_nextToken,
    listReportJobs_byCreationBefore,
    listReportJobs_byCreationAfter,
    listReportJobs_maxResults,
    listReportJobsResponse_reportJobs,
    listReportJobsResponse_nextToken,
    listReportJobsResponse_httpStatus,

    -- ** ListBackupJobs
    listBackupJobs_byResourceArn,
    listBackupJobs_byCreatedAfter,
    listBackupJobs_byAccountId,
    listBackupJobs_byCreatedBefore,
    listBackupJobs_byBackupVaultName,
    listBackupJobs_byResourceType,
    listBackupJobs_nextToken,
    listBackupJobs_byState,
    listBackupJobs_maxResults,
    listBackupJobsResponse_backupJobs,
    listBackupJobsResponse_nextToken,
    listBackupJobsResponse_httpStatus,

    -- ** DescribeReportPlan
    describeReportPlan_reportPlanName,
    describeReportPlanResponse_reportPlan,
    describeReportPlanResponse_httpStatus,

    -- ** DescribeRegionSettings
    describeRegionSettingsResponse_resourceTypeOptInPreference,
    describeRegionSettingsResponse_httpStatus,

    -- ** GetBackupPlan
    getBackupPlan_versionId,
    getBackupPlan_backupPlanId,
    getBackupPlanResponse_versionId,
    getBackupPlanResponse_advancedBackupSettings,
    getBackupPlanResponse_backupPlanId,
    getBackupPlanResponse_creatorRequestId,
    getBackupPlanResponse_backupPlanArn,
    getBackupPlanResponse_lastExecutionDate,
    getBackupPlanResponse_backupPlan,
    getBackupPlanResponse_creationDate,
    getBackupPlanResponse_deletionDate,
    getBackupPlanResponse_httpStatus,

    -- ** DescribeGlobalSettings
    describeGlobalSettingsResponse_globalSettings,
    describeGlobalSettingsResponse_lastUpdateTime,
    describeGlobalSettingsResponse_httpStatus,

    -- ** ListBackupPlanVersions
    listBackupPlanVersions_nextToken,
    listBackupPlanVersions_maxResults,
    listBackupPlanVersions_backupPlanId,
    listBackupPlanVersionsResponse_backupPlanVersionsList,
    listBackupPlanVersionsResponse_nextToken,
    listBackupPlanVersionsResponse_httpStatus,

    -- ** ListRestoreJobs
    listRestoreJobs_byCreatedAfter,
    listRestoreJobs_byStatus,
    listRestoreJobs_byAccountId,
    listRestoreJobs_byCreatedBefore,
    listRestoreJobs_nextToken,
    listRestoreJobs_maxResults,
    listRestoreJobsResponse_nextToken,
    listRestoreJobsResponse_restoreJobs,
    listRestoreJobsResponse_httpStatus,

    -- ** CreateReportPlan
    createReportPlan_idempotencyToken,
    createReportPlan_reportPlanTags,
    createReportPlan_reportPlanDescription,
    createReportPlan_reportPlanName,
    createReportPlan_reportDeliveryChannel,
    createReportPlan_reportSetting,
    createReportPlanResponse_creationTime,
    createReportPlanResponse_reportPlanName,
    createReportPlanResponse_reportPlanArn,
    createReportPlanResponse_httpStatus,

    -- ** ExportBackupPlanTemplate
    exportBackupPlanTemplate_backupPlanId,
    exportBackupPlanTemplateResponse_backupPlanTemplateJson,
    exportBackupPlanTemplateResponse_httpStatus,

    -- ** StartBackupJob
    startBackupJob_idempotencyToken,
    startBackupJob_lifecycle,
    startBackupJob_recoveryPointTags,
    startBackupJob_completeWindowMinutes,
    startBackupJob_backupOptions,
    startBackupJob_startWindowMinutes,
    startBackupJob_backupVaultName,
    startBackupJob_resourceArn,
    startBackupJob_iamRoleArn,
    startBackupJobResponse_backupJobId,
    startBackupJobResponse_recoveryPointArn,
    startBackupJobResponse_creationDate,
    startBackupJobResponse_httpStatus,

    -- ** DescribeFramework
    describeFramework_frameworkName,
    describeFrameworkResponse_creationTime,
    describeFrameworkResponse_idempotencyToken,
    describeFrameworkResponse_frameworkDescription,
    describeFrameworkResponse_frameworkStatus,
    describeFrameworkResponse_frameworkControls,
    describeFrameworkResponse_frameworkArn,
    describeFrameworkResponse_deploymentStatus,
    describeFrameworkResponse_frameworkName,
    describeFrameworkResponse_httpStatus,

    -- ** CreateBackupPlan
    createBackupPlan_backupPlanTags,
    createBackupPlan_creatorRequestId,
    createBackupPlan_backupPlan,
    createBackupPlanResponse_versionId,
    createBackupPlanResponse_advancedBackupSettings,
    createBackupPlanResponse_backupPlanId,
    createBackupPlanResponse_backupPlanArn,
    createBackupPlanResponse_creationDate,
    createBackupPlanResponse_httpStatus,

    -- ** ListProtectedResources
    listProtectedResources_nextToken,
    listProtectedResources_maxResults,
    listProtectedResourcesResponse_results,
    listProtectedResourcesResponse_nextToken,
    listProtectedResourcesResponse_httpStatus,

    -- ** StartReportJob
    startReportJob_idempotencyToken,
    startReportJob_reportPlanName,
    startReportJobResponse_reportJobId,
    startReportJobResponse_httpStatus,

    -- ** DescribeBackupVault
    describeBackupVault_backupVaultName,
    describeBackupVaultResponse_lockDate,
    describeBackupVaultResponse_maxRetentionDays,
    describeBackupVaultResponse_locked,
    describeBackupVaultResponse_creatorRequestId,
    describeBackupVaultResponse_numberOfRecoveryPoints,
    describeBackupVaultResponse_backupVaultArn,
    describeBackupVaultResponse_encryptionKeyArn,
    describeBackupVaultResponse_creationDate,
    describeBackupVaultResponse_backupVaultName,
    describeBackupVaultResponse_minRetentionDays,
    describeBackupVaultResponse_httpStatus,

    -- ** GetBackupVaultNotifications
    getBackupVaultNotifications_backupVaultName,
    getBackupVaultNotificationsResponse_sNSTopicArn,
    getBackupVaultNotificationsResponse_backupVaultArn,
    getBackupVaultNotificationsResponse_backupVaultName,
    getBackupVaultNotificationsResponse_backupVaultEvents,
    getBackupVaultNotificationsResponse_httpStatus,

    -- ** ListReportPlans
    listReportPlans_nextToken,
    listReportPlans_maxResults,
    listReportPlansResponse_reportPlans,
    listReportPlansResponse_nextToken,
    listReportPlansResponse_httpStatus,

    -- ** GetRecoveryPointRestoreMetadata
    getRecoveryPointRestoreMetadata_backupVaultName,
    getRecoveryPointRestoreMetadata_recoveryPointArn,
    getRecoveryPointRestoreMetadataResponse_backupVaultArn,
    getRecoveryPointRestoreMetadataResponse_recoveryPointArn,
    getRecoveryPointRestoreMetadataResponse_restoreMetadata,
    getRecoveryPointRestoreMetadataResponse_httpStatus,

    -- ** ListBackupPlans
    listBackupPlans_nextToken,
    listBackupPlans_maxResults,
    listBackupPlans_includeDeleted,
    listBackupPlansResponse_nextToken,
    listBackupPlansResponse_backupPlansList,
    listBackupPlansResponse_httpStatus,

    -- ** StartRestoreJob
    startRestoreJob_idempotencyToken,
    startRestoreJob_resourceType,
    startRestoreJob_recoveryPointArn,
    startRestoreJob_metadata,
    startRestoreJob_iamRoleArn,
    startRestoreJobResponse_restoreJobId,
    startRestoreJobResponse_httpStatus,

    -- ** ListBackupSelections
    listBackupSelections_nextToken,
    listBackupSelections_maxResults,
    listBackupSelections_backupPlanId,
    listBackupSelectionsResponse_nextToken,
    listBackupSelectionsResponse_backupSelectionsList,
    listBackupSelectionsResponse_httpStatus,

    -- ** ListRecoveryPointsByResource
    listRecoveryPointsByResource_nextToken,
    listRecoveryPointsByResource_maxResults,
    listRecoveryPointsByResource_resourceArn,
    listRecoveryPointsByResourceResponse_recoveryPoints,
    listRecoveryPointsByResourceResponse_nextToken,
    listRecoveryPointsByResourceResponse_httpStatus,

    -- ** CreateBackupSelection
    createBackupSelection_creatorRequestId,
    createBackupSelection_backupPlanId,
    createBackupSelection_backupSelection,
    createBackupSelectionResponse_selectionId,
    createBackupSelectionResponse_backupPlanId,
    createBackupSelectionResponse_creationDate,
    createBackupSelectionResponse_httpStatus,

    -- ** ListFrameworks
    listFrameworks_nextToken,
    listFrameworks_maxResults,
    listFrameworksResponse_nextToken,
    listFrameworksResponse_frameworks,
    listFrameworksResponse_httpStatus,

    -- ** DescribeProtectedResource
    describeProtectedResource_resourceArn,
    describeProtectedResourceResponse_resourceType,
    describeProtectedResourceResponse_lastBackupTime,
    describeProtectedResourceResponse_resourceArn,
    describeProtectedResourceResponse_httpStatus,

    -- ** GetBackupPlanFromJSON
    getBackupPlanFromJSON_backupPlanTemplateJson,
    getBackupPlanFromJSONResponse_backupPlan,
    getBackupPlanFromJSONResponse_httpStatus,

    -- ** ListBackupVaults
    listBackupVaults_nextToken,
    listBackupVaults_maxResults,
    listBackupVaultsResponse_nextToken,
    listBackupVaultsResponse_backupVaultList,
    listBackupVaultsResponse_httpStatus,

    -- ** GetBackupSelection
    getBackupSelection_backupPlanId,
    getBackupSelection_selectionId,
    getBackupSelectionResponse_selectionId,
    getBackupSelectionResponse_backupPlanId,
    getBackupSelectionResponse_creatorRequestId,
    getBackupSelectionResponse_creationDate,
    getBackupSelectionResponse_backupSelection,
    getBackupSelectionResponse_httpStatus,

    -- ** CreateBackupVault
    createBackupVault_creatorRequestId,
    createBackupVault_encryptionKeyArn,
    createBackupVault_backupVaultTags,
    createBackupVault_backupVaultName,
    createBackupVaultResponse_backupVaultArn,
    createBackupVaultResponse_creationDate,
    createBackupVaultResponse_backupVaultName,
    createBackupVaultResponse_httpStatus,

    -- ** UpdateRecoveryPointLifecycle
    updateRecoveryPointLifecycle_lifecycle,
    updateRecoveryPointLifecycle_backupVaultName,
    updateRecoveryPointLifecycle_recoveryPointArn,
    updateRecoveryPointLifecycleResponse_calculatedLifecycle,
    updateRecoveryPointLifecycleResponse_lifecycle,
    updateRecoveryPointLifecycleResponse_backupVaultArn,
    updateRecoveryPointLifecycleResponse_recoveryPointArn,
    updateRecoveryPointLifecycleResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** CreateFramework
    createFramework_idempotencyToken,
    createFramework_frameworkDescription,
    createFramework_frameworkTags,
    createFramework_frameworkName,
    createFramework_frameworkControls,
    createFrameworkResponse_frameworkArn,
    createFrameworkResponse_frameworkName,
    createFrameworkResponse_httpStatus,

    -- ** PutBackupVaultNotifications
    putBackupVaultNotifications_backupVaultName,
    putBackupVaultNotifications_sNSTopicArn,
    putBackupVaultNotifications_backupVaultEvents,

    -- ** DeleteBackupVaultNotifications
    deleteBackupVaultNotifications_backupVaultName,

    -- ** ListTags
    listTags_nextToken,
    listTags_maxResults,
    listTags_resourceArn,
    listTagsResponse_nextToken,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeyList,

    -- ** ListCopyJobs
    listCopyJobs_byResourceArn,
    listCopyJobs_byCreatedAfter,
    listCopyJobs_byAccountId,
    listCopyJobs_byCreatedBefore,
    listCopyJobs_byDestinationVaultArn,
    listCopyJobs_byResourceType,
    listCopyJobs_nextToken,
    listCopyJobs_byState,
    listCopyJobs_maxResults,
    listCopyJobsResponse_nextToken,
    listCopyJobsResponse_copyJobs,
    listCopyJobsResponse_httpStatus,

    -- ** DeleteBackupVaultLockConfiguration
    deleteBackupVaultLockConfiguration_backupVaultName,

    -- ** GetBackupVaultAccessPolicy
    getBackupVaultAccessPolicy_backupVaultName,
    getBackupVaultAccessPolicyResponse_backupVaultArn,
    getBackupVaultAccessPolicyResponse_policy,
    getBackupVaultAccessPolicyResponse_backupVaultName,
    getBackupVaultAccessPolicyResponse_httpStatus,

    -- ** DeleteRecoveryPoint
    deleteRecoveryPoint_backupVaultName,
    deleteRecoveryPoint_recoveryPointArn,

    -- ** PutBackupVaultLockConfiguration
    putBackupVaultLockConfiguration_maxRetentionDays,
    putBackupVaultLockConfiguration_changeableForDays,
    putBackupVaultLockConfiguration_minRetentionDays,
    putBackupVaultLockConfiguration_backupVaultName,

    -- ** GetSupportedResourceTypes
    getSupportedResourceTypesResponse_resourceTypes,
    getSupportedResourceTypesResponse_httpStatus,

    -- ** StopBackupJob
    stopBackupJob_backupJobId,

    -- ** ListRecoveryPointsByBackupVault
    listRecoveryPointsByBackupVault_byResourceArn,
    listRecoveryPointsByBackupVault_byCreatedAfter,
    listRecoveryPointsByBackupVault_byCreatedBefore,
    listRecoveryPointsByBackupVault_byBackupPlanId,
    listRecoveryPointsByBackupVault_byResourceType,
    listRecoveryPointsByBackupVault_nextToken,
    listRecoveryPointsByBackupVault_maxResults,
    listRecoveryPointsByBackupVault_backupVaultName,
    listRecoveryPointsByBackupVaultResponse_recoveryPoints,
    listRecoveryPointsByBackupVaultResponse_nextToken,
    listRecoveryPointsByBackupVaultResponse_httpStatus,

    -- ** PutBackupVaultAccessPolicy
    putBackupVaultAccessPolicy_policy,
    putBackupVaultAccessPolicy_backupVaultName,

    -- ** DeleteBackupVaultAccessPolicy
    deleteBackupVaultAccessPolicy_backupVaultName,

    -- * Types

    -- ** AdvancedBackupSetting
    advancedBackupSetting_resourceType,
    advancedBackupSetting_backupOptions,

    -- ** BackupJob
    backupJob_iamRoleArn,
    backupJob_state,
    backupJob_resourceType,
    backupJob_percentDone,
    backupJob_startBy,
    backupJob_createdBy,
    backupJob_expectedCompletionDate,
    backupJob_bytesTransferred,
    backupJob_backupVaultArn,
    backupJob_accountId,
    backupJob_backupJobId,
    backupJob_resourceArn,
    backupJob_statusMessage,
    backupJob_recoveryPointArn,
    backupJob_backupSizeInBytes,
    backupJob_creationDate,
    backupJob_completionDate,
    backupJob_backupVaultName,
    backupJob_backupType,
    backupJob_backupOptions,

    -- ** BackupPlan
    backupPlan_advancedBackupSettings,
    backupPlan_backupPlanName,
    backupPlan_rules,

    -- ** BackupPlanInput
    backupPlanInput_advancedBackupSettings,
    backupPlanInput_backupPlanName,
    backupPlanInput_rules,

    -- ** BackupPlanTemplatesListMember
    backupPlanTemplatesListMember_backupPlanTemplateName,
    backupPlanTemplatesListMember_backupPlanTemplateId,

    -- ** BackupPlansListMember
    backupPlansListMember_versionId,
    backupPlansListMember_backupPlanName,
    backupPlansListMember_advancedBackupSettings,
    backupPlansListMember_backupPlanId,
    backupPlansListMember_creatorRequestId,
    backupPlansListMember_backupPlanArn,
    backupPlansListMember_lastExecutionDate,
    backupPlansListMember_creationDate,
    backupPlansListMember_deletionDate,

    -- ** BackupRule
    backupRule_ruleId,
    backupRule_lifecycle,
    backupRule_recoveryPointTags,
    backupRule_scheduleExpression,
    backupRule_enableContinuousBackup,
    backupRule_completionWindowMinutes,
    backupRule_copyActions,
    backupRule_startWindowMinutes,
    backupRule_ruleName,
    backupRule_targetBackupVaultName,

    -- ** BackupRuleInput
    backupRuleInput_lifecycle,
    backupRuleInput_recoveryPointTags,
    backupRuleInput_scheduleExpression,
    backupRuleInput_enableContinuousBackup,
    backupRuleInput_completionWindowMinutes,
    backupRuleInput_copyActions,
    backupRuleInput_startWindowMinutes,
    backupRuleInput_ruleName,
    backupRuleInput_targetBackupVaultName,

    -- ** BackupSelection
    backupSelection_resources,
    backupSelection_listOfTags,
    backupSelection_selectionName,
    backupSelection_iamRoleArn,

    -- ** BackupSelectionsListMember
    backupSelectionsListMember_iamRoleArn,
    backupSelectionsListMember_selectionName,
    backupSelectionsListMember_selectionId,
    backupSelectionsListMember_backupPlanId,
    backupSelectionsListMember_creatorRequestId,
    backupSelectionsListMember_creationDate,

    -- ** BackupVaultListMember
    backupVaultListMember_lockDate,
    backupVaultListMember_maxRetentionDays,
    backupVaultListMember_locked,
    backupVaultListMember_creatorRequestId,
    backupVaultListMember_numberOfRecoveryPoints,
    backupVaultListMember_backupVaultArn,
    backupVaultListMember_encryptionKeyArn,
    backupVaultListMember_creationDate,
    backupVaultListMember_backupVaultName,
    backupVaultListMember_minRetentionDays,

    -- ** CalculatedLifecycle
    calculatedLifecycle_deleteAt,
    calculatedLifecycle_moveToColdStorageAt,

    -- ** Condition
    condition_conditionType,
    condition_conditionKey,
    condition_conditionValue,

    -- ** ControlInputParameter
    controlInputParameter_parameterValue,
    controlInputParameter_parameterName,

    -- ** ControlScope
    controlScope_complianceResourceTypes,
    controlScope_tags,
    controlScope_complianceResourceIds,

    -- ** CopyAction
    copyAction_lifecycle,
    copyAction_destinationBackupVaultArn,

    -- ** CopyJob
    copyJob_iamRoleArn,
    copyJob_state,
    copyJob_sourceRecoveryPointArn,
    copyJob_resourceType,
    copyJob_destinationBackupVaultArn,
    copyJob_createdBy,
    copyJob_destinationRecoveryPointArn,
    copyJob_accountId,
    copyJob_sourceBackupVaultArn,
    copyJob_copyJobId,
    copyJob_resourceArn,
    copyJob_statusMessage,
    copyJob_backupSizeInBytes,
    copyJob_creationDate,
    copyJob_completionDate,

    -- ** Framework
    framework_creationTime,
    framework_frameworkDescription,
    framework_numberOfControls,
    framework_frameworkArn,
    framework_deploymentStatus,
    framework_frameworkName,

    -- ** FrameworkControl
    frameworkControl_controlScope,
    frameworkControl_controlInputParameters,
    frameworkControl_controlName,

    -- ** Lifecycle
    lifecycle_moveToColdStorageAfterDays,
    lifecycle_deleteAfterDays,

    -- ** ProtectedResource
    protectedResource_resourceType,
    protectedResource_lastBackupTime,
    protectedResource_resourceArn,

    -- ** RecoveryPointByBackupVault
    recoveryPointByBackupVault_isEncrypted,
    recoveryPointByBackupVault_status,
    recoveryPointByBackupVault_iamRoleArn,
    recoveryPointByBackupVault_resourceType,
    recoveryPointByBackupVault_createdBy,
    recoveryPointByBackupVault_calculatedLifecycle,
    recoveryPointByBackupVault_lifecycle,
    recoveryPointByBackupVault_backupVaultArn,
    recoveryPointByBackupVault_sourceBackupVaultArn,
    recoveryPointByBackupVault_lastRestoreTime,
    recoveryPointByBackupVault_resourceArn,
    recoveryPointByBackupVault_statusMessage,
    recoveryPointByBackupVault_recoveryPointArn,
    recoveryPointByBackupVault_encryptionKeyArn,
    recoveryPointByBackupVault_backupSizeInBytes,
    recoveryPointByBackupVault_creationDate,
    recoveryPointByBackupVault_completionDate,
    recoveryPointByBackupVault_backupVaultName,

    -- ** RecoveryPointByResource
    recoveryPointByResource_status,
    recoveryPointByResource_statusMessage,
    recoveryPointByResource_recoveryPointArn,
    recoveryPointByResource_backupSizeBytes,
    recoveryPointByResource_encryptionKeyArn,
    recoveryPointByResource_creationDate,
    recoveryPointByResource_backupVaultName,

    -- ** RecoveryPointCreator
    recoveryPointCreator_backupPlanId,
    recoveryPointCreator_backupPlanArn,
    recoveryPointCreator_backupPlanVersion,
    recoveryPointCreator_backupRuleId,

    -- ** ReportDeliveryChannel
    reportDeliveryChannel_s3KeyPrefix,
    reportDeliveryChannel_formats,
    reportDeliveryChannel_s3BucketName,

    -- ** ReportDestination
    reportDestination_s3Keys,
    reportDestination_s3BucketName,

    -- ** ReportJob
    reportJob_creationTime,
    reportJob_status,
    reportJob_reportPlanArn,
    reportJob_completionTime,
    reportJob_reportJobId,
    reportJob_statusMessage,
    reportJob_reportDestination,
    reportJob_reportTemplate,

    -- ** ReportPlan
    reportPlan_creationTime,
    reportPlan_reportPlanName,
    reportPlan_reportPlanArn,
    reportPlan_reportSetting,
    reportPlan_reportPlanDescription,
    reportPlan_lastAttemptedExecutionTime,
    reportPlan_deploymentStatus,
    reportPlan_lastSuccessfulExecutionTime,
    reportPlan_reportDeliveryChannel,

    -- ** ReportSetting
    reportSetting_frameworkArns,
    reportSetting_numberOfFrameworks,
    reportSetting_reportTemplate,

    -- ** RestoreJobsListMember
    restoreJobsListMember_status,
    restoreJobsListMember_iamRoleArn,
    restoreJobsListMember_expectedCompletionTimeMinutes,
    restoreJobsListMember_restoreJobId,
    restoreJobsListMember_resourceType,
    restoreJobsListMember_percentDone,
    restoreJobsListMember_accountId,
    restoreJobsListMember_createdResourceArn,
    restoreJobsListMember_statusMessage,
    restoreJobsListMember_recoveryPointArn,
    restoreJobsListMember_backupSizeInBytes,
    restoreJobsListMember_creationDate,
    restoreJobsListMember_completionDate,
  )
where

import Amazonka.Backup.CreateBackupPlan
import Amazonka.Backup.CreateBackupSelection
import Amazonka.Backup.CreateBackupVault
import Amazonka.Backup.CreateFramework
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
import Amazonka.Backup.ExportBackupPlanTemplate
import Amazonka.Backup.GetBackupPlan
import Amazonka.Backup.GetBackupPlanFromJSON
import Amazonka.Backup.GetBackupPlanFromTemplate
import Amazonka.Backup.GetBackupSelection
import Amazonka.Backup.GetBackupVaultAccessPolicy
import Amazonka.Backup.GetBackupVaultNotifications
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
import Amazonka.Backup.ListProtectedResources
import Amazonka.Backup.ListRecoveryPointsByBackupVault
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
import Amazonka.Backup.Types.ControlInputParameter
import Amazonka.Backup.Types.ControlScope
import Amazonka.Backup.Types.CopyAction
import Amazonka.Backup.Types.CopyJob
import Amazonka.Backup.Types.Framework
import Amazonka.Backup.Types.FrameworkControl
import Amazonka.Backup.Types.Lifecycle
import Amazonka.Backup.Types.ProtectedResource
import Amazonka.Backup.Types.RecoveryPointByBackupVault
import Amazonka.Backup.Types.RecoveryPointByResource
import Amazonka.Backup.Types.RecoveryPointCreator
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
