{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Backup.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Lens
  ( -- * Operations

    -- ** CreateBackupPlan
    createBackupPlan_backupPlanTags,
    createBackupPlan_creatorRequestId,
    createBackupPlan_backupPlan,
    createBackupPlanResponse_creationDate,
    createBackupPlanResponse_backupPlanArn,
    createBackupPlanResponse_backupPlanId,
    createBackupPlanResponse_advancedBackupSettings,
    createBackupPlanResponse_versionId,
    createBackupPlanResponse_httpStatus,

    -- ** CreateBackupSelection
    createBackupSelection_creatorRequestId,
    createBackupSelection_backupPlanId,
    createBackupSelection_backupSelection,
    createBackupSelectionResponse_creationDate,
    createBackupSelectionResponse_selectionId,
    createBackupSelectionResponse_backupPlanId,
    createBackupSelectionResponse_httpStatus,

    -- ** CreateBackupVault
    createBackupVault_encryptionKeyArn,
    createBackupVault_creatorRequestId,
    createBackupVault_backupVaultTags,
    createBackupVault_backupVaultName,
    createBackupVaultResponse_backupVaultName,
    createBackupVaultResponse_creationDate,
    createBackupVaultResponse_backupVaultArn,
    createBackupVaultResponse_httpStatus,

    -- ** CreateFramework
    createFramework_frameworkDescription,
    createFramework_idempotencyToken,
    createFramework_frameworkTags,
    createFramework_frameworkName,
    createFramework_frameworkControls,
    createFrameworkResponse_frameworkArn,
    createFrameworkResponse_frameworkName,
    createFrameworkResponse_httpStatus,

    -- ** CreateReportPlan
    createReportPlan_idempotencyToken,
    createReportPlan_reportPlanTags,
    createReportPlan_reportPlanDescription,
    createReportPlan_reportPlanName,
    createReportPlan_reportDeliveryChannel,
    createReportPlan_reportSetting,
    createReportPlanResponse_reportPlanArn,
    createReportPlanResponse_reportPlanName,
    createReportPlanResponse_creationTime,
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
    describeBackupJobResponse_resourceType,
    describeBackupJobResponse_recoveryPointArn,
    describeBackupJobResponse_completionDate,
    describeBackupJobResponse_backupVaultName,
    describeBackupJobResponse_state,
    describeBackupJobResponse_creationDate,
    describeBackupJobResponse_backupSizeInBytes,
    describeBackupJobResponse_backupVaultArn,
    describeBackupJobResponse_backupOptions,
    describeBackupJobResponse_iamRoleArn,
    describeBackupJobResponse_accountId,
    describeBackupJobResponse_backupType,
    describeBackupJobResponse_percentDone,
    describeBackupJobResponse_expectedCompletionDate,
    describeBackupJobResponse_backupJobId,
    describeBackupJobResponse_resourceArn,
    describeBackupJobResponse_startBy,
    describeBackupJobResponse_statusMessage,
    describeBackupJobResponse_createdBy,
    describeBackupJobResponse_bytesTransferred,
    describeBackupJobResponse_httpStatus,

    -- ** DescribeBackupVault
    describeBackupVault_backupVaultName,
    describeBackupVaultResponse_encryptionKeyArn,
    describeBackupVaultResponse_minRetentionDays,
    describeBackupVaultResponse_maxRetentionDays,
    describeBackupVaultResponse_backupVaultName,
    describeBackupVaultResponse_creationDate,
    describeBackupVaultResponse_backupVaultArn,
    describeBackupVaultResponse_creatorRequestId,
    describeBackupVaultResponse_numberOfRecoveryPoints,
    describeBackupVaultResponse_locked,
    describeBackupVaultResponse_lockDate,
    describeBackupVaultResponse_httpStatus,

    -- ** DescribeCopyJob
    describeCopyJob_copyJobId,
    describeCopyJobResponse_copyJob,
    describeCopyJobResponse_httpStatus,

    -- ** DescribeFramework
    describeFramework_frameworkName,
    describeFrameworkResponse_deploymentStatus,
    describeFrameworkResponse_frameworkArn,
    describeFrameworkResponse_frameworkStatus,
    describeFrameworkResponse_frameworkDescription,
    describeFrameworkResponse_idempotencyToken,
    describeFrameworkResponse_frameworkControls,
    describeFrameworkResponse_frameworkName,
    describeFrameworkResponse_creationTime,
    describeFrameworkResponse_httpStatus,

    -- ** DescribeGlobalSettings
    describeGlobalSettingsResponse_globalSettings,
    describeGlobalSettingsResponse_lastUpdateTime,
    describeGlobalSettingsResponse_httpStatus,

    -- ** DescribeProtectedResource
    describeProtectedResource_resourceArn,
    describeProtectedResourceResponse_resourceType,
    describeProtectedResourceResponse_resourceArn,
    describeProtectedResourceResponse_lastBackupTime,
    describeProtectedResourceResponse_httpStatus,

    -- ** DescribeRecoveryPoint
    describeRecoveryPoint_backupVaultName,
    describeRecoveryPoint_recoveryPointArn,
    describeRecoveryPointResponse_encryptionKeyArn,
    describeRecoveryPointResponse_resourceType,
    describeRecoveryPointResponse_lifecycle,
    describeRecoveryPointResponse_recoveryPointArn,
    describeRecoveryPointResponse_completionDate,
    describeRecoveryPointResponse_backupVaultName,
    describeRecoveryPointResponse_creationDate,
    describeRecoveryPointResponse_backupSizeInBytes,
    describeRecoveryPointResponse_status,
    describeRecoveryPointResponse_backupVaultArn,
    describeRecoveryPointResponse_isEncrypted,
    describeRecoveryPointResponse_iamRoleArn,
    describeRecoveryPointResponse_sourceBackupVaultArn,
    describeRecoveryPointResponse_resourceArn,
    describeRecoveryPointResponse_storageClass,
    describeRecoveryPointResponse_statusMessage,
    describeRecoveryPointResponse_createdBy,
    describeRecoveryPointResponse_lastRestoreTime,
    describeRecoveryPointResponse_calculatedLifecycle,
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
    describeRestoreJobResponse_createdResourceArn,
    describeRestoreJobResponse_resourceType,
    describeRestoreJobResponse_expectedCompletionTimeMinutes,
    describeRestoreJobResponse_recoveryPointArn,
    describeRestoreJobResponse_completionDate,
    describeRestoreJobResponse_creationDate,
    describeRestoreJobResponse_backupSizeInBytes,
    describeRestoreJobResponse_status,
    describeRestoreJobResponse_restoreJobId,
    describeRestoreJobResponse_iamRoleArn,
    describeRestoreJobResponse_accountId,
    describeRestoreJobResponse_percentDone,
    describeRestoreJobResponse_statusMessage,
    describeRestoreJobResponse_httpStatus,

    -- ** DisassociateRecoveryPoint
    disassociateRecoveryPoint_backupVaultName,
    disassociateRecoveryPoint_recoveryPointArn,

    -- ** ExportBackupPlanTemplate
    exportBackupPlanTemplate_backupPlanId,
    exportBackupPlanTemplateResponse_backupPlanTemplateJson,
    exportBackupPlanTemplateResponse_httpStatus,

    -- ** GetBackupPlan
    getBackupPlan_versionId,
    getBackupPlan_backupPlanId,
    getBackupPlanResponse_backupPlan,
    getBackupPlanResponse_creationDate,
    getBackupPlanResponse_creatorRequestId,
    getBackupPlanResponse_backupPlanArn,
    getBackupPlanResponse_backupPlanId,
    getBackupPlanResponse_advancedBackupSettings,
    getBackupPlanResponse_lastExecutionDate,
    getBackupPlanResponse_deletionDate,
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
    getBackupSelectionResponse_creationDate,
    getBackupSelectionResponse_creatorRequestId,
    getBackupSelectionResponse_backupSelection,
    getBackupSelectionResponse_selectionId,
    getBackupSelectionResponse_backupPlanId,
    getBackupSelectionResponse_httpStatus,

    -- ** GetBackupVaultAccessPolicy
    getBackupVaultAccessPolicy_backupVaultName,
    getBackupVaultAccessPolicyResponse_policy,
    getBackupVaultAccessPolicyResponse_backupVaultName,
    getBackupVaultAccessPolicyResponse_backupVaultArn,
    getBackupVaultAccessPolicyResponse_httpStatus,

    -- ** GetBackupVaultNotifications
    getBackupVaultNotifications_backupVaultName,
    getBackupVaultNotificationsResponse_backupVaultName,
    getBackupVaultNotificationsResponse_backupVaultEvents,
    getBackupVaultNotificationsResponse_backupVaultArn,
    getBackupVaultNotificationsResponse_sNSTopicArn,
    getBackupVaultNotificationsResponse_httpStatus,

    -- ** GetRecoveryPointRestoreMetadata
    getRecoveryPointRestoreMetadata_backupVaultName,
    getRecoveryPointRestoreMetadata_recoveryPointArn,
    getRecoveryPointRestoreMetadataResponse_recoveryPointArn,
    getRecoveryPointRestoreMetadataResponse_backupVaultArn,
    getRecoveryPointRestoreMetadataResponse_restoreMetadata,
    getRecoveryPointRestoreMetadataResponse_httpStatus,

    -- ** GetSupportedResourceTypes
    getSupportedResourceTypesResponse_resourceTypes,
    getSupportedResourceTypesResponse_httpStatus,

    -- ** ListBackupJobs
    listBackupJobs_byAccountId,
    listBackupJobs_nextToken,
    listBackupJobs_byCreatedAfter,
    listBackupJobs_byCompleteAfter,
    listBackupJobs_byCompleteBefore,
    listBackupJobs_byResourceType,
    listBackupJobs_byCreatedBefore,
    listBackupJobs_maxResults,
    listBackupJobs_byState,
    listBackupJobs_byBackupVaultName,
    listBackupJobs_byResourceArn,
    listBackupJobsResponse_nextToken,
    listBackupJobsResponse_backupJobs,
    listBackupJobsResponse_httpStatus,

    -- ** ListBackupPlanTemplates
    listBackupPlanTemplates_nextToken,
    listBackupPlanTemplates_maxResults,
    listBackupPlanTemplatesResponse_nextToken,
    listBackupPlanTemplatesResponse_backupPlanTemplatesList,
    listBackupPlanTemplatesResponse_httpStatus,

    -- ** ListBackupPlanVersions
    listBackupPlanVersions_nextToken,
    listBackupPlanVersions_maxResults,
    listBackupPlanVersions_backupPlanId,
    listBackupPlanVersionsResponse_nextToken,
    listBackupPlanVersionsResponse_backupPlanVersionsList,
    listBackupPlanVersionsResponse_httpStatus,

    -- ** ListBackupPlans
    listBackupPlans_nextToken,
    listBackupPlans_includeDeleted,
    listBackupPlans_maxResults,
    listBackupPlansResponse_nextToken,
    listBackupPlansResponse_backupPlansList,
    listBackupPlansResponse_httpStatus,

    -- ** ListBackupSelections
    listBackupSelections_nextToken,
    listBackupSelections_maxResults,
    listBackupSelections_backupPlanId,
    listBackupSelectionsResponse_nextToken,
    listBackupSelectionsResponse_backupSelectionsList,
    listBackupSelectionsResponse_httpStatus,

    -- ** ListBackupVaults
    listBackupVaults_nextToken,
    listBackupVaults_maxResults,
    listBackupVaultsResponse_nextToken,
    listBackupVaultsResponse_backupVaultList,
    listBackupVaultsResponse_httpStatus,

    -- ** ListCopyJobs
    listCopyJobs_byAccountId,
    listCopyJobs_nextToken,
    listCopyJobs_byCreatedAfter,
    listCopyJobs_byCompleteAfter,
    listCopyJobs_byDestinationVaultArn,
    listCopyJobs_byCompleteBefore,
    listCopyJobs_byResourceType,
    listCopyJobs_byCreatedBefore,
    listCopyJobs_maxResults,
    listCopyJobs_byState,
    listCopyJobs_byResourceArn,
    listCopyJobsResponse_nextToken,
    listCopyJobsResponse_copyJobs,
    listCopyJobsResponse_httpStatus,

    -- ** ListFrameworks
    listFrameworks_nextToken,
    listFrameworks_maxResults,
    listFrameworksResponse_nextToken,
    listFrameworksResponse_frameworks,
    listFrameworksResponse_httpStatus,

    -- ** ListProtectedResources
    listProtectedResources_nextToken,
    listProtectedResources_maxResults,
    listProtectedResourcesResponse_nextToken,
    listProtectedResourcesResponse_results,
    listProtectedResourcesResponse_httpStatus,

    -- ** ListRecoveryPointsByBackupVault
    listRecoveryPointsByBackupVault_byBackupPlanId,
    listRecoveryPointsByBackupVault_nextToken,
    listRecoveryPointsByBackupVault_byCreatedAfter,
    listRecoveryPointsByBackupVault_byResourceType,
    listRecoveryPointsByBackupVault_byCreatedBefore,
    listRecoveryPointsByBackupVault_maxResults,
    listRecoveryPointsByBackupVault_byResourceArn,
    listRecoveryPointsByBackupVault_backupVaultName,
    listRecoveryPointsByBackupVaultResponse_nextToken,
    listRecoveryPointsByBackupVaultResponse_recoveryPoints,
    listRecoveryPointsByBackupVaultResponse_httpStatus,

    -- ** ListRecoveryPointsByResource
    listRecoveryPointsByResource_nextToken,
    listRecoveryPointsByResource_maxResults,
    listRecoveryPointsByResource_resourceArn,
    listRecoveryPointsByResourceResponse_nextToken,
    listRecoveryPointsByResourceResponse_recoveryPoints,
    listRecoveryPointsByResourceResponse_httpStatus,

    -- ** ListReportJobs
    listReportJobs_byCreationBefore,
    listReportJobs_nextToken,
    listReportJobs_byCreationAfter,
    listReportJobs_maxResults,
    listReportJobs_byReportPlanName,
    listReportJobs_byStatus,
    listReportJobsResponse_nextToken,
    listReportJobsResponse_reportJobs,
    listReportJobsResponse_httpStatus,

    -- ** ListReportPlans
    listReportPlans_nextToken,
    listReportPlans_maxResults,
    listReportPlansResponse_nextToken,
    listReportPlansResponse_reportPlans,
    listReportPlansResponse_httpStatus,

    -- ** ListRestoreJobs
    listRestoreJobs_byAccountId,
    listRestoreJobs_nextToken,
    listRestoreJobs_byCreatedAfter,
    listRestoreJobs_byCompleteAfter,
    listRestoreJobs_byCompleteBefore,
    listRestoreJobs_byCreatedBefore,
    listRestoreJobs_maxResults,
    listRestoreJobs_byStatus,
    listRestoreJobsResponse_nextToken,
    listRestoreJobsResponse_restoreJobs,
    listRestoreJobsResponse_httpStatus,

    -- ** ListTags
    listTags_nextToken,
    listTags_maxResults,
    listTags_resourceArn,
    listTagsResponse_tags,
    listTagsResponse_nextToken,
    listTagsResponse_httpStatus,

    -- ** PutBackupVaultAccessPolicy
    putBackupVaultAccessPolicy_policy,
    putBackupVaultAccessPolicy_backupVaultName,

    -- ** PutBackupVaultLockConfiguration
    putBackupVaultLockConfiguration_minRetentionDays,
    putBackupVaultLockConfiguration_changeableForDays,
    putBackupVaultLockConfiguration_maxRetentionDays,
    putBackupVaultLockConfiguration_backupVaultName,

    -- ** PutBackupVaultNotifications
    putBackupVaultNotifications_backupVaultName,
    putBackupVaultNotifications_sNSTopicArn,
    putBackupVaultNotifications_backupVaultEvents,

    -- ** StartBackupJob
    startBackupJob_startWindowMinutes,
    startBackupJob_lifecycle,
    startBackupJob_idempotencyToken,
    startBackupJob_backupOptions,
    startBackupJob_completeWindowMinutes,
    startBackupJob_recoveryPointTags,
    startBackupJob_backupVaultName,
    startBackupJob_resourceArn,
    startBackupJob_iamRoleArn,
    startBackupJobResponse_recoveryPointArn,
    startBackupJobResponse_creationDate,
    startBackupJobResponse_backupJobId,
    startBackupJobResponse_httpStatus,

    -- ** StartCopyJob
    startCopyJob_lifecycle,
    startCopyJob_idempotencyToken,
    startCopyJob_recoveryPointArn,
    startCopyJob_sourceBackupVaultName,
    startCopyJob_destinationBackupVaultArn,
    startCopyJob_iamRoleArn,
    startCopyJobResponse_creationDate,
    startCopyJobResponse_copyJobId,
    startCopyJobResponse_httpStatus,

    -- ** StartReportJob
    startReportJob_idempotencyToken,
    startReportJob_reportPlanName,
    startReportJobResponse_reportJobId,
    startReportJobResponse_httpStatus,

    -- ** StartRestoreJob
    startRestoreJob_resourceType,
    startRestoreJob_idempotencyToken,
    startRestoreJob_iamRoleArn,
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
    updateBackupPlanResponse_creationDate,
    updateBackupPlanResponse_backupPlanArn,
    updateBackupPlanResponse_backupPlanId,
    updateBackupPlanResponse_advancedBackupSettings,
    updateBackupPlanResponse_versionId,
    updateBackupPlanResponse_httpStatus,

    -- ** UpdateFramework
    updateFramework_frameworkDescription,
    updateFramework_idempotencyToken,
    updateFramework_frameworkControls,
    updateFramework_frameworkName,
    updateFrameworkResponse_frameworkArn,
    updateFrameworkResponse_frameworkName,
    updateFrameworkResponse_creationTime,
    updateFrameworkResponse_httpStatus,

    -- ** UpdateGlobalSettings
    updateGlobalSettings_globalSettings,

    -- ** UpdateRecoveryPointLifecycle
    updateRecoveryPointLifecycle_lifecycle,
    updateRecoveryPointLifecycle_backupVaultName,
    updateRecoveryPointLifecycle_recoveryPointArn,
    updateRecoveryPointLifecycleResponse_lifecycle,
    updateRecoveryPointLifecycleResponse_recoveryPointArn,
    updateRecoveryPointLifecycleResponse_backupVaultArn,
    updateRecoveryPointLifecycleResponse_calculatedLifecycle,
    updateRecoveryPointLifecycleResponse_httpStatus,

    -- ** UpdateRegionSettings
    updateRegionSettings_resourceTypeManagementPreference,
    updateRegionSettings_resourceTypeOptInPreference,

    -- ** UpdateReportPlan
    updateReportPlan_reportDeliveryChannel,
    updateReportPlan_idempotencyToken,
    updateReportPlan_reportSetting,
    updateReportPlan_reportPlanDescription,
    updateReportPlan_reportPlanName,
    updateReportPlanResponse_reportPlanArn,
    updateReportPlanResponse_reportPlanName,
    updateReportPlanResponse_creationTime,
    updateReportPlanResponse_httpStatus,

    -- * Types

    -- ** AdvancedBackupSetting
    advancedBackupSetting_resourceType,
    advancedBackupSetting_backupOptions,

    -- ** BackupJob
    backupJob_resourceType,
    backupJob_recoveryPointArn,
    backupJob_completionDate,
    backupJob_backupVaultName,
    backupJob_state,
    backupJob_creationDate,
    backupJob_backupSizeInBytes,
    backupJob_backupVaultArn,
    backupJob_backupOptions,
    backupJob_iamRoleArn,
    backupJob_accountId,
    backupJob_backupType,
    backupJob_percentDone,
    backupJob_expectedCompletionDate,
    backupJob_backupJobId,
    backupJob_resourceArn,
    backupJob_startBy,
    backupJob_statusMessage,
    backupJob_createdBy,
    backupJob_bytesTransferred,

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
    backupPlansListMember_backupPlanName,
    backupPlansListMember_creationDate,
    backupPlansListMember_creatorRequestId,
    backupPlansListMember_backupPlanArn,
    backupPlansListMember_backupPlanId,
    backupPlansListMember_advancedBackupSettings,
    backupPlansListMember_lastExecutionDate,
    backupPlansListMember_deletionDate,
    backupPlansListMember_versionId,

    -- ** BackupRule
    backupRule_startWindowMinutes,
    backupRule_lifecycle,
    backupRule_ruleId,
    backupRule_copyActions,
    backupRule_scheduleExpression,
    backupRule_enableContinuousBackup,
    backupRule_completionWindowMinutes,
    backupRule_recoveryPointTags,
    backupRule_ruleName,
    backupRule_targetBackupVaultName,

    -- ** BackupRuleInput
    backupRuleInput_startWindowMinutes,
    backupRuleInput_lifecycle,
    backupRuleInput_copyActions,
    backupRuleInput_scheduleExpression,
    backupRuleInput_enableContinuousBackup,
    backupRuleInput_completionWindowMinutes,
    backupRuleInput_recoveryPointTags,
    backupRuleInput_ruleName,
    backupRuleInput_targetBackupVaultName,

    -- ** BackupSelection
    backupSelection_conditions,
    backupSelection_resources,
    backupSelection_listOfTags,
    backupSelection_notResources,
    backupSelection_selectionName,
    backupSelection_iamRoleArn,

    -- ** BackupSelectionsListMember
    backupSelectionsListMember_selectionName,
    backupSelectionsListMember_creationDate,
    backupSelectionsListMember_creatorRequestId,
    backupSelectionsListMember_iamRoleArn,
    backupSelectionsListMember_selectionId,
    backupSelectionsListMember_backupPlanId,

    -- ** BackupVaultListMember
    backupVaultListMember_encryptionKeyArn,
    backupVaultListMember_minRetentionDays,
    backupVaultListMember_maxRetentionDays,
    backupVaultListMember_backupVaultName,
    backupVaultListMember_creationDate,
    backupVaultListMember_backupVaultArn,
    backupVaultListMember_creatorRequestId,
    backupVaultListMember_numberOfRecoveryPoints,
    backupVaultListMember_locked,
    backupVaultListMember_lockDate,

    -- ** CalculatedLifecycle
    calculatedLifecycle_moveToColdStorageAt,
    calculatedLifecycle_deleteAt,

    -- ** Condition
    condition_conditionType,
    condition_conditionKey,
    condition_conditionValue,

    -- ** ConditionParameter
    conditionParameter_conditionValue,
    conditionParameter_conditionKey,

    -- ** Conditions
    conditions_stringNotEquals,
    conditions_stringNotLike,
    conditions_stringEquals,
    conditions_stringLike,

    -- ** ControlInputParameter
    controlInputParameter_parameterValue,
    controlInputParameter_parameterName,

    -- ** ControlScope
    controlScope_tags,
    controlScope_complianceResourceIds,
    controlScope_complianceResourceTypes,

    -- ** CopyAction
    copyAction_lifecycle,
    copyAction_destinationBackupVaultArn,

    -- ** CopyJob
    copyJob_resourceType,
    copyJob_completionDate,
    copyJob_state,
    copyJob_creationDate,
    copyJob_destinationBackupVaultArn,
    copyJob_backupSizeInBytes,
    copyJob_iamRoleArn,
    copyJob_accountId,
    copyJob_destinationRecoveryPointArn,
    copyJob_copyJobId,
    copyJob_sourceBackupVaultArn,
    copyJob_resourceArn,
    copyJob_statusMessage,
    copyJob_createdBy,
    copyJob_sourceRecoveryPointArn,

    -- ** Framework
    framework_deploymentStatus,
    framework_frameworkArn,
    framework_frameworkDescription,
    framework_frameworkName,
    framework_numberOfControls,
    framework_creationTime,

    -- ** FrameworkControl
    frameworkControl_controlInputParameters,
    frameworkControl_controlScope,
    frameworkControl_controlName,

    -- ** Lifecycle
    lifecycle_deleteAfterDays,
    lifecycle_moveToColdStorageAfterDays,

    -- ** ProtectedResource
    protectedResource_resourceType,
    protectedResource_resourceArn,
    protectedResource_lastBackupTime,

    -- ** RecoveryPointByBackupVault
    recoveryPointByBackupVault_encryptionKeyArn,
    recoveryPointByBackupVault_resourceType,
    recoveryPointByBackupVault_lifecycle,
    recoveryPointByBackupVault_recoveryPointArn,
    recoveryPointByBackupVault_completionDate,
    recoveryPointByBackupVault_backupVaultName,
    recoveryPointByBackupVault_creationDate,
    recoveryPointByBackupVault_backupSizeInBytes,
    recoveryPointByBackupVault_status,
    recoveryPointByBackupVault_backupVaultArn,
    recoveryPointByBackupVault_isEncrypted,
    recoveryPointByBackupVault_iamRoleArn,
    recoveryPointByBackupVault_sourceBackupVaultArn,
    recoveryPointByBackupVault_resourceArn,
    recoveryPointByBackupVault_statusMessage,
    recoveryPointByBackupVault_createdBy,
    recoveryPointByBackupVault_lastRestoreTime,
    recoveryPointByBackupVault_calculatedLifecycle,

    -- ** RecoveryPointByResource
    recoveryPointByResource_encryptionKeyArn,
    recoveryPointByResource_recoveryPointArn,
    recoveryPointByResource_backupSizeBytes,
    recoveryPointByResource_backupVaultName,
    recoveryPointByResource_creationDate,
    recoveryPointByResource_status,
    recoveryPointByResource_statusMessage,

    -- ** RecoveryPointCreator
    recoveryPointCreator_backupPlanVersion,
    recoveryPointCreator_backupPlanArn,
    recoveryPointCreator_backupPlanId,
    recoveryPointCreator_backupRuleId,

    -- ** ReportDeliveryChannel
    reportDeliveryChannel_s3KeyPrefix,
    reportDeliveryChannel_formats,
    reportDeliveryChannel_s3BucketName,

    -- ** ReportDestination
    reportDestination_s3BucketName,
    reportDestination_s3Keys,

    -- ** ReportJob
    reportJob_reportPlanArn,
    reportJob_reportTemplate,
    reportJob_reportJobId,
    reportJob_reportDestination,
    reportJob_status,
    reportJob_completionTime,
    reportJob_creationTime,
    reportJob_statusMessage,

    -- ** ReportPlan
    reportPlan_deploymentStatus,
    reportPlan_reportPlanArn,
    reportPlan_reportDeliveryChannel,
    reportPlan_lastSuccessfulExecutionTime,
    reportPlan_reportSetting,
    reportPlan_lastAttemptedExecutionTime,
    reportPlan_reportPlanName,
    reportPlan_creationTime,
    reportPlan_reportPlanDescription,

    -- ** ReportSetting
    reportSetting_frameworkArns,
    reportSetting_numberOfFrameworks,
    reportSetting_reportTemplate,

    -- ** RestoreJobsListMember
    restoreJobsListMember_createdResourceArn,
    restoreJobsListMember_resourceType,
    restoreJobsListMember_expectedCompletionTimeMinutes,
    restoreJobsListMember_recoveryPointArn,
    restoreJobsListMember_completionDate,
    restoreJobsListMember_creationDate,
    restoreJobsListMember_backupSizeInBytes,
    restoreJobsListMember_status,
    restoreJobsListMember_restoreJobId,
    restoreJobsListMember_iamRoleArn,
    restoreJobsListMember_accountId,
    restoreJobsListMember_percentDone,
    restoreJobsListMember_statusMessage,
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
import Amazonka.Backup.Types.ConditionParameter
import Amazonka.Backup.Types.Conditions
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
