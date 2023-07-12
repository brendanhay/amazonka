{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSM.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Lens
  ( -- * Operations

    -- ** AddTagsToResource
    addTagsToResource_resourceType,
    addTagsToResource_resourceId,
    addTagsToResource_tags,
    addTagsToResourceResponse_httpStatus,

    -- ** AssociateOpsItemRelatedItem
    associateOpsItemRelatedItem_opsItemId,
    associateOpsItemRelatedItem_associationType,
    associateOpsItemRelatedItem_resourceType,
    associateOpsItemRelatedItem_resourceUri,
    associateOpsItemRelatedItemResponse_associationId,
    associateOpsItemRelatedItemResponse_httpStatus,

    -- ** CancelCommand
    cancelCommand_instanceIds,
    cancelCommand_commandId,
    cancelCommandResponse_httpStatus,

    -- ** CancelMaintenanceWindowExecution
    cancelMaintenanceWindowExecution_windowExecutionId,
    cancelMaintenanceWindowExecutionResponse_windowExecutionId,
    cancelMaintenanceWindowExecutionResponse_httpStatus,

    -- ** CreateActivation
    createActivation_defaultInstanceName,
    createActivation_description,
    createActivation_expirationDate,
    createActivation_registrationLimit,
    createActivation_registrationMetadata,
    createActivation_tags,
    createActivation_iamRole,
    createActivationResponse_activationCode,
    createActivationResponse_activationId,
    createActivationResponse_httpStatus,

    -- ** CreateAssociation
    createAssociation_alarmConfiguration,
    createAssociation_applyOnlyAtCronInterval,
    createAssociation_associationName,
    createAssociation_automationTargetParameterName,
    createAssociation_calendarNames,
    createAssociation_complianceSeverity,
    createAssociation_documentVersion,
    createAssociation_instanceId,
    createAssociation_maxConcurrency,
    createAssociation_maxErrors,
    createAssociation_outputLocation,
    createAssociation_parameters,
    createAssociation_scheduleExpression,
    createAssociation_scheduleOffset,
    createAssociation_syncCompliance,
    createAssociation_tags,
    createAssociation_targetLocations,
    createAssociation_targetMaps,
    createAssociation_targets,
    createAssociation_name,
    createAssociationResponse_associationDescription,
    createAssociationResponse_httpStatus,

    -- ** CreateAssociationBatch
    createAssociationBatch_entries,
    createAssociationBatchResponse_failed,
    createAssociationBatchResponse_successful,
    createAssociationBatchResponse_httpStatus,

    -- ** CreateDocument
    createDocument_attachments,
    createDocument_displayName,
    createDocument_documentFormat,
    createDocument_documentType,
    createDocument_requires,
    createDocument_tags,
    createDocument_targetType,
    createDocument_versionName,
    createDocument_content,
    createDocument_name,
    createDocumentResponse_documentDescription,
    createDocumentResponse_httpStatus,

    -- ** CreateMaintenanceWindow
    createMaintenanceWindow_clientToken,
    createMaintenanceWindow_description,
    createMaintenanceWindow_endDate,
    createMaintenanceWindow_scheduleOffset,
    createMaintenanceWindow_scheduleTimezone,
    createMaintenanceWindow_startDate,
    createMaintenanceWindow_tags,
    createMaintenanceWindow_name,
    createMaintenanceWindow_schedule,
    createMaintenanceWindow_duration,
    createMaintenanceWindow_cutoff,
    createMaintenanceWindow_allowUnassociatedTargets,
    createMaintenanceWindowResponse_windowId,
    createMaintenanceWindowResponse_httpStatus,

    -- ** CreateOpsItem
    createOpsItem_accountId,
    createOpsItem_actualEndTime,
    createOpsItem_actualStartTime,
    createOpsItem_category,
    createOpsItem_notifications,
    createOpsItem_operationalData,
    createOpsItem_opsItemType,
    createOpsItem_plannedEndTime,
    createOpsItem_plannedStartTime,
    createOpsItem_priority,
    createOpsItem_relatedOpsItems,
    createOpsItem_severity,
    createOpsItem_tags,
    createOpsItem_description,
    createOpsItem_source,
    createOpsItem_title,
    createOpsItemResponse_opsItemArn,
    createOpsItemResponse_opsItemId,
    createOpsItemResponse_httpStatus,

    -- ** CreateOpsMetadata
    createOpsMetadata_metadata,
    createOpsMetadata_tags,
    createOpsMetadata_resourceId,
    createOpsMetadataResponse_opsMetadataArn,
    createOpsMetadataResponse_httpStatus,

    -- ** CreatePatchBaseline
    createPatchBaseline_approvalRules,
    createPatchBaseline_approvedPatches,
    createPatchBaseline_approvedPatchesComplianceLevel,
    createPatchBaseline_approvedPatchesEnableNonSecurity,
    createPatchBaseline_clientToken,
    createPatchBaseline_description,
    createPatchBaseline_globalFilters,
    createPatchBaseline_operatingSystem,
    createPatchBaseline_rejectedPatches,
    createPatchBaseline_rejectedPatchesAction,
    createPatchBaseline_sources,
    createPatchBaseline_tags,
    createPatchBaseline_name,
    createPatchBaselineResponse_baselineId,
    createPatchBaselineResponse_httpStatus,

    -- ** CreateResourceDataSync
    createResourceDataSync_s3Destination,
    createResourceDataSync_syncSource,
    createResourceDataSync_syncType,
    createResourceDataSync_syncName,
    createResourceDataSyncResponse_httpStatus,

    -- ** DeleteActivation
    deleteActivation_activationId,
    deleteActivationResponse_httpStatus,

    -- ** DeleteAssociation
    deleteAssociation_associationId,
    deleteAssociation_instanceId,
    deleteAssociation_name,
    deleteAssociationResponse_httpStatus,

    -- ** DeleteDocument
    deleteDocument_documentVersion,
    deleteDocument_force,
    deleteDocument_versionName,
    deleteDocument_name,
    deleteDocumentResponse_httpStatus,

    -- ** DeleteInventory
    deleteInventory_clientToken,
    deleteInventory_dryRun,
    deleteInventory_schemaDeleteOption,
    deleteInventory_typeName,
    deleteInventoryResponse_deletionId,
    deleteInventoryResponse_deletionSummary,
    deleteInventoryResponse_typeName,
    deleteInventoryResponse_httpStatus,

    -- ** DeleteMaintenanceWindow
    deleteMaintenanceWindow_windowId,
    deleteMaintenanceWindowResponse_windowId,
    deleteMaintenanceWindowResponse_httpStatus,

    -- ** DeleteOpsMetadata
    deleteOpsMetadata_opsMetadataArn,
    deleteOpsMetadataResponse_httpStatus,

    -- ** DeleteParameter
    deleteParameter_name,
    deleteParameterResponse_httpStatus,

    -- ** DeleteParameters
    deleteParameters_names,
    deleteParametersResponse_deletedParameters,
    deleteParametersResponse_invalidParameters,
    deleteParametersResponse_httpStatus,

    -- ** DeletePatchBaseline
    deletePatchBaseline_baselineId,
    deletePatchBaselineResponse_baselineId,
    deletePatchBaselineResponse_httpStatus,

    -- ** DeleteResourceDataSync
    deleteResourceDataSync_syncType,
    deleteResourceDataSync_syncName,
    deleteResourceDataSyncResponse_httpStatus,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_resourceArn,
    deleteResourcePolicy_policyId,
    deleteResourcePolicy_policyHash,
    deleteResourcePolicyResponse_httpStatus,

    -- ** DeregisterManagedInstance
    deregisterManagedInstance_instanceId,
    deregisterManagedInstanceResponse_httpStatus,

    -- ** DeregisterPatchBaselineForPatchGroup
    deregisterPatchBaselineForPatchGroup_baselineId,
    deregisterPatchBaselineForPatchGroup_patchGroup,
    deregisterPatchBaselineForPatchGroupResponse_baselineId,
    deregisterPatchBaselineForPatchGroupResponse_patchGroup,
    deregisterPatchBaselineForPatchGroupResponse_httpStatus,

    -- ** DeregisterTargetFromMaintenanceWindow
    deregisterTargetFromMaintenanceWindow_safe,
    deregisterTargetFromMaintenanceWindow_windowId,
    deregisterTargetFromMaintenanceWindow_windowTargetId,
    deregisterTargetFromMaintenanceWindowResponse_windowId,
    deregisterTargetFromMaintenanceWindowResponse_windowTargetId,
    deregisterTargetFromMaintenanceWindowResponse_httpStatus,

    -- ** DeregisterTaskFromMaintenanceWindow
    deregisterTaskFromMaintenanceWindow_windowId,
    deregisterTaskFromMaintenanceWindow_windowTaskId,
    deregisterTaskFromMaintenanceWindowResponse_windowId,
    deregisterTaskFromMaintenanceWindowResponse_windowTaskId,
    deregisterTaskFromMaintenanceWindowResponse_httpStatus,

    -- ** DescribeActivations
    describeActivations_filters,
    describeActivations_maxResults,
    describeActivations_nextToken,
    describeActivationsResponse_activationList,
    describeActivationsResponse_nextToken,
    describeActivationsResponse_httpStatus,

    -- ** DescribeAssociation
    describeAssociation_associationId,
    describeAssociation_associationVersion,
    describeAssociation_instanceId,
    describeAssociation_name,
    describeAssociationResponse_associationDescription,
    describeAssociationResponse_httpStatus,

    -- ** DescribeAssociationExecutionTargets
    describeAssociationExecutionTargets_filters,
    describeAssociationExecutionTargets_maxResults,
    describeAssociationExecutionTargets_nextToken,
    describeAssociationExecutionTargets_associationId,
    describeAssociationExecutionTargets_executionId,
    describeAssociationExecutionTargetsResponse_associationExecutionTargets,
    describeAssociationExecutionTargetsResponse_nextToken,
    describeAssociationExecutionTargetsResponse_httpStatus,

    -- ** DescribeAssociationExecutions
    describeAssociationExecutions_filters,
    describeAssociationExecutions_maxResults,
    describeAssociationExecutions_nextToken,
    describeAssociationExecutions_associationId,
    describeAssociationExecutionsResponse_associationExecutions,
    describeAssociationExecutionsResponse_nextToken,
    describeAssociationExecutionsResponse_httpStatus,

    -- ** DescribeAutomationExecutions
    describeAutomationExecutions_filters,
    describeAutomationExecutions_maxResults,
    describeAutomationExecutions_nextToken,
    describeAutomationExecutionsResponse_automationExecutionMetadataList,
    describeAutomationExecutionsResponse_nextToken,
    describeAutomationExecutionsResponse_httpStatus,

    -- ** DescribeAutomationStepExecutions
    describeAutomationStepExecutions_filters,
    describeAutomationStepExecutions_maxResults,
    describeAutomationStepExecutions_nextToken,
    describeAutomationStepExecutions_reverseOrder,
    describeAutomationStepExecutions_automationExecutionId,
    describeAutomationStepExecutionsResponse_nextToken,
    describeAutomationStepExecutionsResponse_stepExecutions,
    describeAutomationStepExecutionsResponse_httpStatus,

    -- ** DescribeAvailablePatches
    describeAvailablePatches_filters,
    describeAvailablePatches_maxResults,
    describeAvailablePatches_nextToken,
    describeAvailablePatchesResponse_nextToken,
    describeAvailablePatchesResponse_patches,
    describeAvailablePatchesResponse_httpStatus,

    -- ** DescribeDocument
    describeDocument_documentVersion,
    describeDocument_versionName,
    describeDocument_name,
    describeDocumentResponse_document,
    describeDocumentResponse_httpStatus,

    -- ** DescribeDocumentPermission
    describeDocumentPermission_maxResults,
    describeDocumentPermission_nextToken,
    describeDocumentPermission_name,
    describeDocumentPermission_permissionType,
    describeDocumentPermissionResponse_accountIds,
    describeDocumentPermissionResponse_accountSharingInfoList,
    describeDocumentPermissionResponse_nextToken,
    describeDocumentPermissionResponse_httpStatus,

    -- ** DescribeEffectiveInstanceAssociations
    describeEffectiveInstanceAssociations_maxResults,
    describeEffectiveInstanceAssociations_nextToken,
    describeEffectiveInstanceAssociations_instanceId,
    describeEffectiveInstanceAssociationsResponse_associations,
    describeEffectiveInstanceAssociationsResponse_nextToken,
    describeEffectiveInstanceAssociationsResponse_httpStatus,

    -- ** DescribeEffectivePatchesForPatchBaseline
    describeEffectivePatchesForPatchBaseline_maxResults,
    describeEffectivePatchesForPatchBaseline_nextToken,
    describeEffectivePatchesForPatchBaseline_baselineId,
    describeEffectivePatchesForPatchBaselineResponse_effectivePatches,
    describeEffectivePatchesForPatchBaselineResponse_nextToken,
    describeEffectivePatchesForPatchBaselineResponse_httpStatus,

    -- ** DescribeInstanceAssociationsStatus
    describeInstanceAssociationsStatus_maxResults,
    describeInstanceAssociationsStatus_nextToken,
    describeInstanceAssociationsStatus_instanceId,
    describeInstanceAssociationsStatusResponse_instanceAssociationStatusInfos,
    describeInstanceAssociationsStatusResponse_nextToken,
    describeInstanceAssociationsStatusResponse_httpStatus,

    -- ** DescribeInstanceInformation
    describeInstanceInformation_filters,
    describeInstanceInformation_instanceInformationFilterList,
    describeInstanceInformation_maxResults,
    describeInstanceInformation_nextToken,
    describeInstanceInformationResponse_instanceInformationList,
    describeInstanceInformationResponse_nextToken,
    describeInstanceInformationResponse_httpStatus,

    -- ** DescribeInstancePatchStates
    describeInstancePatchStates_maxResults,
    describeInstancePatchStates_nextToken,
    describeInstancePatchStates_instanceIds,
    describeInstancePatchStatesResponse_instancePatchStates,
    describeInstancePatchStatesResponse_nextToken,
    describeInstancePatchStatesResponse_httpStatus,

    -- ** DescribeInstancePatchStatesForPatchGroup
    describeInstancePatchStatesForPatchGroup_filters,
    describeInstancePatchStatesForPatchGroup_maxResults,
    describeInstancePatchStatesForPatchGroup_nextToken,
    describeInstancePatchStatesForPatchGroup_patchGroup,
    describeInstancePatchStatesForPatchGroupResponse_instancePatchStates,
    describeInstancePatchStatesForPatchGroupResponse_nextToken,
    describeInstancePatchStatesForPatchGroupResponse_httpStatus,

    -- ** DescribeInstancePatches
    describeInstancePatches_filters,
    describeInstancePatches_maxResults,
    describeInstancePatches_nextToken,
    describeInstancePatches_instanceId,
    describeInstancePatchesResponse_nextToken,
    describeInstancePatchesResponse_patches,
    describeInstancePatchesResponse_httpStatus,

    -- ** DescribeInventoryDeletions
    describeInventoryDeletions_deletionId,
    describeInventoryDeletions_maxResults,
    describeInventoryDeletions_nextToken,
    describeInventoryDeletionsResponse_inventoryDeletions,
    describeInventoryDeletionsResponse_nextToken,
    describeInventoryDeletionsResponse_httpStatus,

    -- ** DescribeMaintenanceWindowExecutionTaskInvocations
    describeMaintenanceWindowExecutionTaskInvocations_filters,
    describeMaintenanceWindowExecutionTaskInvocations_maxResults,
    describeMaintenanceWindowExecutionTaskInvocations_nextToken,
    describeMaintenanceWindowExecutionTaskInvocations_windowExecutionId,
    describeMaintenanceWindowExecutionTaskInvocations_taskId,
    describeMaintenanceWindowExecutionTaskInvocationsResponse_nextToken,
    describeMaintenanceWindowExecutionTaskInvocationsResponse_windowExecutionTaskInvocationIdentities,
    describeMaintenanceWindowExecutionTaskInvocationsResponse_httpStatus,

    -- ** DescribeMaintenanceWindowExecutionTasks
    describeMaintenanceWindowExecutionTasks_filters,
    describeMaintenanceWindowExecutionTasks_maxResults,
    describeMaintenanceWindowExecutionTasks_nextToken,
    describeMaintenanceWindowExecutionTasks_windowExecutionId,
    describeMaintenanceWindowExecutionTasksResponse_nextToken,
    describeMaintenanceWindowExecutionTasksResponse_windowExecutionTaskIdentities,
    describeMaintenanceWindowExecutionTasksResponse_httpStatus,

    -- ** DescribeMaintenanceWindowExecutions
    describeMaintenanceWindowExecutions_filters,
    describeMaintenanceWindowExecutions_maxResults,
    describeMaintenanceWindowExecutions_nextToken,
    describeMaintenanceWindowExecutions_windowId,
    describeMaintenanceWindowExecutionsResponse_nextToken,
    describeMaintenanceWindowExecutionsResponse_windowExecutions,
    describeMaintenanceWindowExecutionsResponse_httpStatus,

    -- ** DescribeMaintenanceWindowSchedule
    describeMaintenanceWindowSchedule_filters,
    describeMaintenanceWindowSchedule_maxResults,
    describeMaintenanceWindowSchedule_nextToken,
    describeMaintenanceWindowSchedule_resourceType,
    describeMaintenanceWindowSchedule_targets,
    describeMaintenanceWindowSchedule_windowId,
    describeMaintenanceWindowScheduleResponse_nextToken,
    describeMaintenanceWindowScheduleResponse_scheduledWindowExecutions,
    describeMaintenanceWindowScheduleResponse_httpStatus,

    -- ** DescribeMaintenanceWindowTargets
    describeMaintenanceWindowTargets_filters,
    describeMaintenanceWindowTargets_maxResults,
    describeMaintenanceWindowTargets_nextToken,
    describeMaintenanceWindowTargets_windowId,
    describeMaintenanceWindowTargetsResponse_nextToken,
    describeMaintenanceWindowTargetsResponse_targets,
    describeMaintenanceWindowTargetsResponse_httpStatus,

    -- ** DescribeMaintenanceWindowTasks
    describeMaintenanceWindowTasks_filters,
    describeMaintenanceWindowTasks_maxResults,
    describeMaintenanceWindowTasks_nextToken,
    describeMaintenanceWindowTasks_windowId,
    describeMaintenanceWindowTasksResponse_nextToken,
    describeMaintenanceWindowTasksResponse_tasks,
    describeMaintenanceWindowTasksResponse_httpStatus,

    -- ** DescribeMaintenanceWindows
    describeMaintenanceWindows_filters,
    describeMaintenanceWindows_maxResults,
    describeMaintenanceWindows_nextToken,
    describeMaintenanceWindowsResponse_nextToken,
    describeMaintenanceWindowsResponse_windowIdentities,
    describeMaintenanceWindowsResponse_httpStatus,

    -- ** DescribeMaintenanceWindowsForTarget
    describeMaintenanceWindowsForTarget_maxResults,
    describeMaintenanceWindowsForTarget_nextToken,
    describeMaintenanceWindowsForTarget_targets,
    describeMaintenanceWindowsForTarget_resourceType,
    describeMaintenanceWindowsForTargetResponse_nextToken,
    describeMaintenanceWindowsForTargetResponse_windowIdentities,
    describeMaintenanceWindowsForTargetResponse_httpStatus,

    -- ** DescribeOpsItems
    describeOpsItems_maxResults,
    describeOpsItems_nextToken,
    describeOpsItems_opsItemFilters,
    describeOpsItemsResponse_nextToken,
    describeOpsItemsResponse_opsItemSummaries,
    describeOpsItemsResponse_httpStatus,

    -- ** DescribeParameters
    describeParameters_filters,
    describeParameters_maxResults,
    describeParameters_nextToken,
    describeParameters_parameterFilters,
    describeParametersResponse_nextToken,
    describeParametersResponse_parameters,
    describeParametersResponse_httpStatus,

    -- ** DescribePatchBaselines
    describePatchBaselines_filters,
    describePatchBaselines_maxResults,
    describePatchBaselines_nextToken,
    describePatchBaselinesResponse_baselineIdentities,
    describePatchBaselinesResponse_nextToken,
    describePatchBaselinesResponse_httpStatus,

    -- ** DescribePatchGroupState
    describePatchGroupState_patchGroup,
    describePatchGroupStateResponse_instances,
    describePatchGroupStateResponse_instancesWithCriticalNonCompliantPatches,
    describePatchGroupStateResponse_instancesWithFailedPatches,
    describePatchGroupStateResponse_instancesWithInstalledOtherPatches,
    describePatchGroupStateResponse_instancesWithInstalledPatches,
    describePatchGroupStateResponse_instancesWithInstalledPendingRebootPatches,
    describePatchGroupStateResponse_instancesWithInstalledRejectedPatches,
    describePatchGroupStateResponse_instancesWithMissingPatches,
    describePatchGroupStateResponse_instancesWithNotApplicablePatches,
    describePatchGroupStateResponse_instancesWithOtherNonCompliantPatches,
    describePatchGroupStateResponse_instancesWithSecurityNonCompliantPatches,
    describePatchGroupStateResponse_instancesWithUnreportedNotApplicablePatches,
    describePatchGroupStateResponse_httpStatus,

    -- ** DescribePatchGroups
    describePatchGroups_filters,
    describePatchGroups_maxResults,
    describePatchGroups_nextToken,
    describePatchGroupsResponse_mappings,
    describePatchGroupsResponse_nextToken,
    describePatchGroupsResponse_httpStatus,

    -- ** DescribePatchProperties
    describePatchProperties_maxResults,
    describePatchProperties_nextToken,
    describePatchProperties_patchSet,
    describePatchProperties_operatingSystem,
    describePatchProperties_property,
    describePatchPropertiesResponse_nextToken,
    describePatchPropertiesResponse_properties,
    describePatchPropertiesResponse_httpStatus,

    -- ** DescribeSessions
    describeSessions_filters,
    describeSessions_maxResults,
    describeSessions_nextToken,
    describeSessions_state,
    describeSessionsResponse_nextToken,
    describeSessionsResponse_sessions,
    describeSessionsResponse_httpStatus,

    -- ** DisassociateOpsItemRelatedItem
    disassociateOpsItemRelatedItem_opsItemId,
    disassociateOpsItemRelatedItem_associationId,
    disassociateOpsItemRelatedItemResponse_httpStatus,

    -- ** GetAutomationExecution
    getAutomationExecution_automationExecutionId,
    getAutomationExecutionResponse_automationExecution,
    getAutomationExecutionResponse_httpStatus,

    -- ** GetCalendarState
    getCalendarState_atTime,
    getCalendarState_calendarNames,
    getCalendarStateResponse_atTime,
    getCalendarStateResponse_nextTransitionTime,
    getCalendarStateResponse_state,
    getCalendarStateResponse_httpStatus,

    -- ** GetCommandInvocation
    getCommandInvocation_pluginName,
    getCommandInvocation_commandId,
    getCommandInvocation_instanceId,
    getCommandInvocationResponse_cloudWatchOutputConfig,
    getCommandInvocationResponse_commandId,
    getCommandInvocationResponse_comment,
    getCommandInvocationResponse_documentName,
    getCommandInvocationResponse_documentVersion,
    getCommandInvocationResponse_executionElapsedTime,
    getCommandInvocationResponse_executionEndDateTime,
    getCommandInvocationResponse_executionStartDateTime,
    getCommandInvocationResponse_instanceId,
    getCommandInvocationResponse_pluginName,
    getCommandInvocationResponse_responseCode,
    getCommandInvocationResponse_standardErrorContent,
    getCommandInvocationResponse_standardErrorUrl,
    getCommandInvocationResponse_standardOutputContent,
    getCommandInvocationResponse_standardOutputUrl,
    getCommandInvocationResponse_status,
    getCommandInvocationResponse_statusDetails,
    getCommandInvocationResponse_httpStatus,

    -- ** GetConnectionStatus
    getConnectionStatus_target,
    getConnectionStatusResponse_status,
    getConnectionStatusResponse_target,
    getConnectionStatusResponse_httpStatus,

    -- ** GetDefaultPatchBaseline
    getDefaultPatchBaseline_operatingSystem,
    getDefaultPatchBaselineResponse_baselineId,
    getDefaultPatchBaselineResponse_operatingSystem,
    getDefaultPatchBaselineResponse_httpStatus,

    -- ** GetDeployablePatchSnapshotForInstance
    getDeployablePatchSnapshotForInstance_baselineOverride,
    getDeployablePatchSnapshotForInstance_instanceId,
    getDeployablePatchSnapshotForInstance_snapshotId,
    getDeployablePatchSnapshotForInstanceResponse_instanceId,
    getDeployablePatchSnapshotForInstanceResponse_product,
    getDeployablePatchSnapshotForInstanceResponse_snapshotDownloadUrl,
    getDeployablePatchSnapshotForInstanceResponse_snapshotId,
    getDeployablePatchSnapshotForInstanceResponse_httpStatus,

    -- ** GetDocument
    getDocument_documentFormat,
    getDocument_documentVersion,
    getDocument_versionName,
    getDocument_name,
    getDocumentResponse_attachmentsContent,
    getDocumentResponse_content,
    getDocumentResponse_createdDate,
    getDocumentResponse_displayName,
    getDocumentResponse_documentFormat,
    getDocumentResponse_documentType,
    getDocumentResponse_documentVersion,
    getDocumentResponse_name,
    getDocumentResponse_requires,
    getDocumentResponse_reviewStatus,
    getDocumentResponse_status,
    getDocumentResponse_statusInformation,
    getDocumentResponse_versionName,
    getDocumentResponse_httpStatus,

    -- ** GetInventory
    getInventory_aggregators,
    getInventory_filters,
    getInventory_maxResults,
    getInventory_nextToken,
    getInventory_resultAttributes,
    getInventoryResponse_entities,
    getInventoryResponse_nextToken,
    getInventoryResponse_httpStatus,

    -- ** GetInventorySchema
    getInventorySchema_aggregator,
    getInventorySchema_maxResults,
    getInventorySchema_nextToken,
    getInventorySchema_subType,
    getInventorySchema_typeName,
    getInventorySchemaResponse_nextToken,
    getInventorySchemaResponse_schemas,
    getInventorySchemaResponse_httpStatus,

    -- ** GetMaintenanceWindow
    getMaintenanceWindow_windowId,
    getMaintenanceWindowResponse_allowUnassociatedTargets,
    getMaintenanceWindowResponse_createdDate,
    getMaintenanceWindowResponse_cutoff,
    getMaintenanceWindowResponse_description,
    getMaintenanceWindowResponse_duration,
    getMaintenanceWindowResponse_enabled,
    getMaintenanceWindowResponse_endDate,
    getMaintenanceWindowResponse_modifiedDate,
    getMaintenanceWindowResponse_name,
    getMaintenanceWindowResponse_nextExecutionTime,
    getMaintenanceWindowResponse_schedule,
    getMaintenanceWindowResponse_scheduleOffset,
    getMaintenanceWindowResponse_scheduleTimezone,
    getMaintenanceWindowResponse_startDate,
    getMaintenanceWindowResponse_windowId,
    getMaintenanceWindowResponse_httpStatus,

    -- ** GetMaintenanceWindowExecution
    getMaintenanceWindowExecution_windowExecutionId,
    getMaintenanceWindowExecutionResponse_endTime,
    getMaintenanceWindowExecutionResponse_startTime,
    getMaintenanceWindowExecutionResponse_status,
    getMaintenanceWindowExecutionResponse_statusDetails,
    getMaintenanceWindowExecutionResponse_taskIds,
    getMaintenanceWindowExecutionResponse_windowExecutionId,
    getMaintenanceWindowExecutionResponse_httpStatus,

    -- ** GetMaintenanceWindowExecutionTask
    getMaintenanceWindowExecutionTask_windowExecutionId,
    getMaintenanceWindowExecutionTask_taskId,
    getMaintenanceWindowExecutionTaskResponse_alarmConfiguration,
    getMaintenanceWindowExecutionTaskResponse_endTime,
    getMaintenanceWindowExecutionTaskResponse_maxConcurrency,
    getMaintenanceWindowExecutionTaskResponse_maxErrors,
    getMaintenanceWindowExecutionTaskResponse_priority,
    getMaintenanceWindowExecutionTaskResponse_serviceRole,
    getMaintenanceWindowExecutionTaskResponse_startTime,
    getMaintenanceWindowExecutionTaskResponse_status,
    getMaintenanceWindowExecutionTaskResponse_statusDetails,
    getMaintenanceWindowExecutionTaskResponse_taskArn,
    getMaintenanceWindowExecutionTaskResponse_taskExecutionId,
    getMaintenanceWindowExecutionTaskResponse_taskParameters,
    getMaintenanceWindowExecutionTaskResponse_triggeredAlarms,
    getMaintenanceWindowExecutionTaskResponse_type,
    getMaintenanceWindowExecutionTaskResponse_windowExecutionId,
    getMaintenanceWindowExecutionTaskResponse_httpStatus,

    -- ** GetMaintenanceWindowExecutionTaskInvocation
    getMaintenanceWindowExecutionTaskInvocation_windowExecutionId,
    getMaintenanceWindowExecutionTaskInvocation_taskId,
    getMaintenanceWindowExecutionTaskInvocation_invocationId,
    getMaintenanceWindowExecutionTaskInvocationResponse_endTime,
    getMaintenanceWindowExecutionTaskInvocationResponse_executionId,
    getMaintenanceWindowExecutionTaskInvocationResponse_invocationId,
    getMaintenanceWindowExecutionTaskInvocationResponse_ownerInformation,
    getMaintenanceWindowExecutionTaskInvocationResponse_parameters,
    getMaintenanceWindowExecutionTaskInvocationResponse_startTime,
    getMaintenanceWindowExecutionTaskInvocationResponse_status,
    getMaintenanceWindowExecutionTaskInvocationResponse_statusDetails,
    getMaintenanceWindowExecutionTaskInvocationResponse_taskExecutionId,
    getMaintenanceWindowExecutionTaskInvocationResponse_taskType,
    getMaintenanceWindowExecutionTaskInvocationResponse_windowExecutionId,
    getMaintenanceWindowExecutionTaskInvocationResponse_windowTargetId,
    getMaintenanceWindowExecutionTaskInvocationResponse_httpStatus,

    -- ** GetMaintenanceWindowTask
    getMaintenanceWindowTask_windowId,
    getMaintenanceWindowTask_windowTaskId,
    getMaintenanceWindowTaskResponse_alarmConfiguration,
    getMaintenanceWindowTaskResponse_cutoffBehavior,
    getMaintenanceWindowTaskResponse_description,
    getMaintenanceWindowTaskResponse_loggingInfo,
    getMaintenanceWindowTaskResponse_maxConcurrency,
    getMaintenanceWindowTaskResponse_maxErrors,
    getMaintenanceWindowTaskResponse_name,
    getMaintenanceWindowTaskResponse_priority,
    getMaintenanceWindowTaskResponse_serviceRoleArn,
    getMaintenanceWindowTaskResponse_targets,
    getMaintenanceWindowTaskResponse_taskArn,
    getMaintenanceWindowTaskResponse_taskInvocationParameters,
    getMaintenanceWindowTaskResponse_taskParameters,
    getMaintenanceWindowTaskResponse_taskType,
    getMaintenanceWindowTaskResponse_windowId,
    getMaintenanceWindowTaskResponse_windowTaskId,
    getMaintenanceWindowTaskResponse_httpStatus,

    -- ** GetOpsItem
    getOpsItem_opsItemArn,
    getOpsItem_opsItemId,
    getOpsItemResponse_opsItem,
    getOpsItemResponse_httpStatus,

    -- ** GetOpsMetadata
    getOpsMetadata_maxResults,
    getOpsMetadata_nextToken,
    getOpsMetadata_opsMetadataArn,
    getOpsMetadataResponse_metadata,
    getOpsMetadataResponse_nextToken,
    getOpsMetadataResponse_resourceId,
    getOpsMetadataResponse_httpStatus,

    -- ** GetOpsSummary
    getOpsSummary_aggregators,
    getOpsSummary_filters,
    getOpsSummary_maxResults,
    getOpsSummary_nextToken,
    getOpsSummary_resultAttributes,
    getOpsSummary_syncName,
    getOpsSummaryResponse_entities,
    getOpsSummaryResponse_nextToken,
    getOpsSummaryResponse_httpStatus,

    -- ** GetParameter
    getParameter_withDecryption,
    getParameter_name,
    getParameterResponse_httpStatus,
    getParameterResponse_parameter,

    -- ** GetParameterHistory
    getParameterHistory_maxResults,
    getParameterHistory_nextToken,
    getParameterHistory_withDecryption,
    getParameterHistory_name,
    getParameterHistoryResponse_nextToken,
    getParameterHistoryResponse_parameters,
    getParameterHistoryResponse_httpStatus,

    -- ** GetParameters
    getParameters_withDecryption,
    getParameters_names,
    getParametersResponse_httpStatus,
    getParametersResponse_invalidParameters,
    getParametersResponse_parameters,

    -- ** GetParametersByPath
    getParametersByPath_maxResults,
    getParametersByPath_nextToken,
    getParametersByPath_parameterFilters,
    getParametersByPath_recursive,
    getParametersByPath_withDecryption,
    getParametersByPath_path,
    getParametersByPathResponse_nextToken,
    getParametersByPathResponse_parameters,
    getParametersByPathResponse_httpStatus,

    -- ** GetPatchBaseline
    getPatchBaseline_baselineId,
    getPatchBaselineResponse_approvalRules,
    getPatchBaselineResponse_approvedPatches,
    getPatchBaselineResponse_approvedPatchesComplianceLevel,
    getPatchBaselineResponse_approvedPatchesEnableNonSecurity,
    getPatchBaselineResponse_baselineId,
    getPatchBaselineResponse_createdDate,
    getPatchBaselineResponse_description,
    getPatchBaselineResponse_globalFilters,
    getPatchBaselineResponse_modifiedDate,
    getPatchBaselineResponse_name,
    getPatchBaselineResponse_operatingSystem,
    getPatchBaselineResponse_patchGroups,
    getPatchBaselineResponse_rejectedPatches,
    getPatchBaselineResponse_rejectedPatchesAction,
    getPatchBaselineResponse_sources,
    getPatchBaselineResponse_httpStatus,

    -- ** GetPatchBaselineForPatchGroup
    getPatchBaselineForPatchGroup_operatingSystem,
    getPatchBaselineForPatchGroup_patchGroup,
    getPatchBaselineForPatchGroupResponse_baselineId,
    getPatchBaselineForPatchGroupResponse_operatingSystem,
    getPatchBaselineForPatchGroupResponse_patchGroup,
    getPatchBaselineForPatchGroupResponse_httpStatus,

    -- ** GetResourcePolicies
    getResourcePolicies_maxResults,
    getResourcePolicies_nextToken,
    getResourcePolicies_resourceArn,
    getResourcePoliciesResponse_nextToken,
    getResourcePoliciesResponse_policies,
    getResourcePoliciesResponse_httpStatus,

    -- ** GetServiceSetting
    getServiceSetting_settingId,
    getServiceSettingResponse_serviceSetting,
    getServiceSettingResponse_httpStatus,

    -- ** LabelParameterVersion
    labelParameterVersion_parameterVersion,
    labelParameterVersion_name,
    labelParameterVersion_labels,
    labelParameterVersionResponse_invalidLabels,
    labelParameterVersionResponse_parameterVersion,
    labelParameterVersionResponse_httpStatus,

    -- ** ListAssociationVersions
    listAssociationVersions_maxResults,
    listAssociationVersions_nextToken,
    listAssociationVersions_associationId,
    listAssociationVersionsResponse_associationVersions,
    listAssociationVersionsResponse_nextToken,
    listAssociationVersionsResponse_httpStatus,

    -- ** ListAssociations
    listAssociations_associationFilterList,
    listAssociations_maxResults,
    listAssociations_nextToken,
    listAssociationsResponse_associations,
    listAssociationsResponse_nextToken,
    listAssociationsResponse_httpStatus,

    -- ** ListCommandInvocations
    listCommandInvocations_commandId,
    listCommandInvocations_details,
    listCommandInvocations_filters,
    listCommandInvocations_instanceId,
    listCommandInvocations_maxResults,
    listCommandInvocations_nextToken,
    listCommandInvocationsResponse_commandInvocations,
    listCommandInvocationsResponse_nextToken,
    listCommandInvocationsResponse_httpStatus,

    -- ** ListCommands
    listCommands_commandId,
    listCommands_filters,
    listCommands_instanceId,
    listCommands_maxResults,
    listCommands_nextToken,
    listCommandsResponse_commands,
    listCommandsResponse_nextToken,
    listCommandsResponse_httpStatus,

    -- ** ListComplianceItems
    listComplianceItems_filters,
    listComplianceItems_maxResults,
    listComplianceItems_nextToken,
    listComplianceItems_resourceIds,
    listComplianceItems_resourceTypes,
    listComplianceItemsResponse_complianceItems,
    listComplianceItemsResponse_nextToken,
    listComplianceItemsResponse_httpStatus,

    -- ** ListComplianceSummaries
    listComplianceSummaries_filters,
    listComplianceSummaries_maxResults,
    listComplianceSummaries_nextToken,
    listComplianceSummariesResponse_complianceSummaryItems,
    listComplianceSummariesResponse_nextToken,
    listComplianceSummariesResponse_httpStatus,

    -- ** ListDocumentMetadataHistory
    listDocumentMetadataHistory_documentVersion,
    listDocumentMetadataHistory_maxResults,
    listDocumentMetadataHistory_nextToken,
    listDocumentMetadataHistory_name,
    listDocumentMetadataHistory_metadata,
    listDocumentMetadataHistoryResponse_author,
    listDocumentMetadataHistoryResponse_documentVersion,
    listDocumentMetadataHistoryResponse_metadata,
    listDocumentMetadataHistoryResponse_name,
    listDocumentMetadataHistoryResponse_nextToken,
    listDocumentMetadataHistoryResponse_httpStatus,

    -- ** ListDocumentVersions
    listDocumentVersions_maxResults,
    listDocumentVersions_nextToken,
    listDocumentVersions_name,
    listDocumentVersionsResponse_documentVersions,
    listDocumentVersionsResponse_nextToken,
    listDocumentVersionsResponse_httpStatus,

    -- ** ListDocuments
    listDocuments_documentFilterList,
    listDocuments_filters,
    listDocuments_maxResults,
    listDocuments_nextToken,
    listDocumentsResponse_documentIdentifiers,
    listDocumentsResponse_nextToken,
    listDocumentsResponse_httpStatus,

    -- ** ListInventoryEntries
    listInventoryEntries_filters,
    listInventoryEntries_maxResults,
    listInventoryEntries_nextToken,
    listInventoryEntries_instanceId,
    listInventoryEntries_typeName,
    listInventoryEntriesResponse_captureTime,
    listInventoryEntriesResponse_entries,
    listInventoryEntriesResponse_instanceId,
    listInventoryEntriesResponse_nextToken,
    listInventoryEntriesResponse_schemaVersion,
    listInventoryEntriesResponse_typeName,
    listInventoryEntriesResponse_httpStatus,

    -- ** ListOpsItemEvents
    listOpsItemEvents_filters,
    listOpsItemEvents_maxResults,
    listOpsItemEvents_nextToken,
    listOpsItemEventsResponse_nextToken,
    listOpsItemEventsResponse_summaries,
    listOpsItemEventsResponse_httpStatus,

    -- ** ListOpsItemRelatedItems
    listOpsItemRelatedItems_filters,
    listOpsItemRelatedItems_maxResults,
    listOpsItemRelatedItems_nextToken,
    listOpsItemRelatedItems_opsItemId,
    listOpsItemRelatedItemsResponse_nextToken,
    listOpsItemRelatedItemsResponse_summaries,
    listOpsItemRelatedItemsResponse_httpStatus,

    -- ** ListOpsMetadata
    listOpsMetadata_filters,
    listOpsMetadata_maxResults,
    listOpsMetadata_nextToken,
    listOpsMetadataResponse_nextToken,
    listOpsMetadataResponse_opsMetadataList,
    listOpsMetadataResponse_httpStatus,

    -- ** ListResourceComplianceSummaries
    listResourceComplianceSummaries_filters,
    listResourceComplianceSummaries_maxResults,
    listResourceComplianceSummaries_nextToken,
    listResourceComplianceSummariesResponse_nextToken,
    listResourceComplianceSummariesResponse_resourceComplianceSummaryItems,
    listResourceComplianceSummariesResponse_httpStatus,

    -- ** ListResourceDataSync
    listResourceDataSync_maxResults,
    listResourceDataSync_nextToken,
    listResourceDataSync_syncType,
    listResourceDataSyncResponse_nextToken,
    listResourceDataSyncResponse_resourceDataSyncItems,
    listResourceDataSyncResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceType,
    listTagsForResource_resourceId,
    listTagsForResourceResponse_tagList,
    listTagsForResourceResponse_httpStatus,

    -- ** ModifyDocumentPermission
    modifyDocumentPermission_accountIdsToAdd,
    modifyDocumentPermission_accountIdsToRemove,
    modifyDocumentPermission_sharedDocumentVersion,
    modifyDocumentPermission_name,
    modifyDocumentPermission_permissionType,
    modifyDocumentPermissionResponse_httpStatus,

    -- ** PutComplianceItems
    putComplianceItems_itemContentHash,
    putComplianceItems_uploadType,
    putComplianceItems_resourceId,
    putComplianceItems_resourceType,
    putComplianceItems_complianceType,
    putComplianceItems_executionSummary,
    putComplianceItems_items,
    putComplianceItemsResponse_httpStatus,

    -- ** PutInventory
    putInventory_instanceId,
    putInventory_items,
    putInventoryResponse_message,
    putInventoryResponse_httpStatus,

    -- ** PutParameter
    putParameter_allowedPattern,
    putParameter_dataType,
    putParameter_description,
    putParameter_keyId,
    putParameter_overwrite,
    putParameter_policies,
    putParameter_tags,
    putParameter_tier,
    putParameter_type,
    putParameter_name,
    putParameter_value,
    putParameterResponse_tier,
    putParameterResponse_httpStatus,
    putParameterResponse_version,

    -- ** PutResourcePolicy
    putResourcePolicy_policyHash,
    putResourcePolicy_policyId,
    putResourcePolicy_resourceArn,
    putResourcePolicy_policy,
    putResourcePolicyResponse_policyHash,
    putResourcePolicyResponse_policyId,
    putResourcePolicyResponse_httpStatus,

    -- ** RegisterDefaultPatchBaseline
    registerDefaultPatchBaseline_baselineId,
    registerDefaultPatchBaselineResponse_baselineId,
    registerDefaultPatchBaselineResponse_httpStatus,

    -- ** RegisterPatchBaselineForPatchGroup
    registerPatchBaselineForPatchGroup_baselineId,
    registerPatchBaselineForPatchGroup_patchGroup,
    registerPatchBaselineForPatchGroupResponse_baselineId,
    registerPatchBaselineForPatchGroupResponse_patchGroup,
    registerPatchBaselineForPatchGroupResponse_httpStatus,

    -- ** RegisterTargetWithMaintenanceWindow
    registerTargetWithMaintenanceWindow_clientToken,
    registerTargetWithMaintenanceWindow_description,
    registerTargetWithMaintenanceWindow_name,
    registerTargetWithMaintenanceWindow_ownerInformation,
    registerTargetWithMaintenanceWindow_windowId,
    registerTargetWithMaintenanceWindow_resourceType,
    registerTargetWithMaintenanceWindow_targets,
    registerTargetWithMaintenanceWindowResponse_windowTargetId,
    registerTargetWithMaintenanceWindowResponse_httpStatus,

    -- ** RegisterTaskWithMaintenanceWindow
    registerTaskWithMaintenanceWindow_alarmConfiguration,
    registerTaskWithMaintenanceWindow_clientToken,
    registerTaskWithMaintenanceWindow_cutoffBehavior,
    registerTaskWithMaintenanceWindow_description,
    registerTaskWithMaintenanceWindow_loggingInfo,
    registerTaskWithMaintenanceWindow_maxConcurrency,
    registerTaskWithMaintenanceWindow_maxErrors,
    registerTaskWithMaintenanceWindow_name,
    registerTaskWithMaintenanceWindow_priority,
    registerTaskWithMaintenanceWindow_serviceRoleArn,
    registerTaskWithMaintenanceWindow_targets,
    registerTaskWithMaintenanceWindow_taskInvocationParameters,
    registerTaskWithMaintenanceWindow_taskParameters,
    registerTaskWithMaintenanceWindow_windowId,
    registerTaskWithMaintenanceWindow_taskArn,
    registerTaskWithMaintenanceWindow_taskType,
    registerTaskWithMaintenanceWindowResponse_windowTaskId,
    registerTaskWithMaintenanceWindowResponse_httpStatus,

    -- ** RemoveTagsFromResource
    removeTagsFromResource_resourceType,
    removeTagsFromResource_resourceId,
    removeTagsFromResource_tagKeys,
    removeTagsFromResourceResponse_httpStatus,

    -- ** ResetServiceSetting
    resetServiceSetting_settingId,
    resetServiceSettingResponse_serviceSetting,
    resetServiceSettingResponse_httpStatus,

    -- ** ResumeSession
    resumeSession_sessionId,
    resumeSessionResponse_sessionId,
    resumeSessionResponse_streamUrl,
    resumeSessionResponse_tokenValue,
    resumeSessionResponse_httpStatus,

    -- ** SendAutomationSignal
    sendAutomationSignal_payload,
    sendAutomationSignal_automationExecutionId,
    sendAutomationSignal_signalType,
    sendAutomationSignalResponse_httpStatus,

    -- ** SendCommand
    sendCommand_alarmConfiguration,
    sendCommand_cloudWatchOutputConfig,
    sendCommand_comment,
    sendCommand_documentHash,
    sendCommand_documentHashType,
    sendCommand_documentVersion,
    sendCommand_instanceIds,
    sendCommand_maxConcurrency,
    sendCommand_maxErrors,
    sendCommand_notificationConfig,
    sendCommand_outputS3BucketName,
    sendCommand_outputS3KeyPrefix,
    sendCommand_outputS3Region,
    sendCommand_parameters,
    sendCommand_serviceRoleArn,
    sendCommand_targets,
    sendCommand_timeoutSeconds,
    sendCommand_documentName,
    sendCommandResponse_command,
    sendCommandResponse_httpStatus,

    -- ** StartAssociationsOnce
    startAssociationsOnce_associationIds,
    startAssociationsOnceResponse_httpStatus,

    -- ** StartAutomationExecution
    startAutomationExecution_alarmConfiguration,
    startAutomationExecution_clientToken,
    startAutomationExecution_documentVersion,
    startAutomationExecution_maxConcurrency,
    startAutomationExecution_maxErrors,
    startAutomationExecution_mode,
    startAutomationExecution_parameters,
    startAutomationExecution_tags,
    startAutomationExecution_targetLocations,
    startAutomationExecution_targetMaps,
    startAutomationExecution_targetParameterName,
    startAutomationExecution_targets,
    startAutomationExecution_documentName,
    startAutomationExecutionResponse_automationExecutionId,
    startAutomationExecutionResponse_httpStatus,

    -- ** StartChangeRequestExecution
    startChangeRequestExecution_autoApprove,
    startChangeRequestExecution_changeDetails,
    startChangeRequestExecution_changeRequestName,
    startChangeRequestExecution_clientToken,
    startChangeRequestExecution_documentVersion,
    startChangeRequestExecution_parameters,
    startChangeRequestExecution_scheduledEndTime,
    startChangeRequestExecution_scheduledTime,
    startChangeRequestExecution_tags,
    startChangeRequestExecution_documentName,
    startChangeRequestExecution_runbooks,
    startChangeRequestExecutionResponse_automationExecutionId,
    startChangeRequestExecutionResponse_httpStatus,

    -- ** StartSession
    startSession_documentName,
    startSession_parameters,
    startSession_reason,
    startSession_target,
    startSessionResponse_sessionId,
    startSessionResponse_streamUrl,
    startSessionResponse_tokenValue,
    startSessionResponse_httpStatus,

    -- ** StopAutomationExecution
    stopAutomationExecution_type,
    stopAutomationExecution_automationExecutionId,
    stopAutomationExecutionResponse_httpStatus,

    -- ** TerminateSession
    terminateSession_sessionId,
    terminateSessionResponse_sessionId,
    terminateSessionResponse_httpStatus,

    -- ** UnlabelParameterVersion
    unlabelParameterVersion_name,
    unlabelParameterVersion_parameterVersion,
    unlabelParameterVersion_labels,
    unlabelParameterVersionResponse_invalidLabels,
    unlabelParameterVersionResponse_removedLabels,
    unlabelParameterVersionResponse_httpStatus,

    -- ** UpdateAssociation
    updateAssociation_alarmConfiguration,
    updateAssociation_applyOnlyAtCronInterval,
    updateAssociation_associationName,
    updateAssociation_associationVersion,
    updateAssociation_automationTargetParameterName,
    updateAssociation_calendarNames,
    updateAssociation_complianceSeverity,
    updateAssociation_documentVersion,
    updateAssociation_maxConcurrency,
    updateAssociation_maxErrors,
    updateAssociation_name,
    updateAssociation_outputLocation,
    updateAssociation_parameters,
    updateAssociation_scheduleExpression,
    updateAssociation_scheduleOffset,
    updateAssociation_syncCompliance,
    updateAssociation_targetLocations,
    updateAssociation_targetMaps,
    updateAssociation_targets,
    updateAssociation_associationId,
    updateAssociationResponse_associationDescription,
    updateAssociationResponse_httpStatus,

    -- ** UpdateAssociationStatus
    updateAssociationStatus_name,
    updateAssociationStatus_instanceId,
    updateAssociationStatus_associationStatus,
    updateAssociationStatusResponse_associationDescription,
    updateAssociationStatusResponse_httpStatus,

    -- ** UpdateDocument
    updateDocument_attachments,
    updateDocument_displayName,
    updateDocument_documentFormat,
    updateDocument_documentVersion,
    updateDocument_targetType,
    updateDocument_versionName,
    updateDocument_content,
    updateDocument_name,
    updateDocumentResponse_documentDescription,
    updateDocumentResponse_httpStatus,

    -- ** UpdateDocumentDefaultVersion
    updateDocumentDefaultVersion_name,
    updateDocumentDefaultVersion_documentVersion,
    updateDocumentDefaultVersionResponse_description,
    updateDocumentDefaultVersionResponse_httpStatus,

    -- ** UpdateDocumentMetadata
    updateDocumentMetadata_documentVersion,
    updateDocumentMetadata_name,
    updateDocumentMetadata_documentReviews,
    updateDocumentMetadataResponse_httpStatus,

    -- ** UpdateMaintenanceWindow
    updateMaintenanceWindow_allowUnassociatedTargets,
    updateMaintenanceWindow_cutoff,
    updateMaintenanceWindow_description,
    updateMaintenanceWindow_duration,
    updateMaintenanceWindow_enabled,
    updateMaintenanceWindow_endDate,
    updateMaintenanceWindow_name,
    updateMaintenanceWindow_replace,
    updateMaintenanceWindow_schedule,
    updateMaintenanceWindow_scheduleOffset,
    updateMaintenanceWindow_scheduleTimezone,
    updateMaintenanceWindow_startDate,
    updateMaintenanceWindow_windowId,
    updateMaintenanceWindowResponse_allowUnassociatedTargets,
    updateMaintenanceWindowResponse_cutoff,
    updateMaintenanceWindowResponse_description,
    updateMaintenanceWindowResponse_duration,
    updateMaintenanceWindowResponse_enabled,
    updateMaintenanceWindowResponse_endDate,
    updateMaintenanceWindowResponse_name,
    updateMaintenanceWindowResponse_schedule,
    updateMaintenanceWindowResponse_scheduleOffset,
    updateMaintenanceWindowResponse_scheduleTimezone,
    updateMaintenanceWindowResponse_startDate,
    updateMaintenanceWindowResponse_windowId,
    updateMaintenanceWindowResponse_httpStatus,

    -- ** UpdateMaintenanceWindowTarget
    updateMaintenanceWindowTarget_description,
    updateMaintenanceWindowTarget_name,
    updateMaintenanceWindowTarget_ownerInformation,
    updateMaintenanceWindowTarget_replace,
    updateMaintenanceWindowTarget_targets,
    updateMaintenanceWindowTarget_windowId,
    updateMaintenanceWindowTarget_windowTargetId,
    updateMaintenanceWindowTargetResponse_description,
    updateMaintenanceWindowTargetResponse_name,
    updateMaintenanceWindowTargetResponse_ownerInformation,
    updateMaintenanceWindowTargetResponse_targets,
    updateMaintenanceWindowTargetResponse_windowId,
    updateMaintenanceWindowTargetResponse_windowTargetId,
    updateMaintenanceWindowTargetResponse_httpStatus,

    -- ** UpdateMaintenanceWindowTask
    updateMaintenanceWindowTask_alarmConfiguration,
    updateMaintenanceWindowTask_cutoffBehavior,
    updateMaintenanceWindowTask_description,
    updateMaintenanceWindowTask_loggingInfo,
    updateMaintenanceWindowTask_maxConcurrency,
    updateMaintenanceWindowTask_maxErrors,
    updateMaintenanceWindowTask_name,
    updateMaintenanceWindowTask_priority,
    updateMaintenanceWindowTask_replace,
    updateMaintenanceWindowTask_serviceRoleArn,
    updateMaintenanceWindowTask_targets,
    updateMaintenanceWindowTask_taskArn,
    updateMaintenanceWindowTask_taskInvocationParameters,
    updateMaintenanceWindowTask_taskParameters,
    updateMaintenanceWindowTask_windowId,
    updateMaintenanceWindowTask_windowTaskId,
    updateMaintenanceWindowTaskResponse_alarmConfiguration,
    updateMaintenanceWindowTaskResponse_cutoffBehavior,
    updateMaintenanceWindowTaskResponse_description,
    updateMaintenanceWindowTaskResponse_loggingInfo,
    updateMaintenanceWindowTaskResponse_maxConcurrency,
    updateMaintenanceWindowTaskResponse_maxErrors,
    updateMaintenanceWindowTaskResponse_name,
    updateMaintenanceWindowTaskResponse_priority,
    updateMaintenanceWindowTaskResponse_serviceRoleArn,
    updateMaintenanceWindowTaskResponse_targets,
    updateMaintenanceWindowTaskResponse_taskArn,
    updateMaintenanceWindowTaskResponse_taskInvocationParameters,
    updateMaintenanceWindowTaskResponse_taskParameters,
    updateMaintenanceWindowTaskResponse_windowId,
    updateMaintenanceWindowTaskResponse_windowTaskId,
    updateMaintenanceWindowTaskResponse_httpStatus,

    -- ** UpdateManagedInstanceRole
    updateManagedInstanceRole_instanceId,
    updateManagedInstanceRole_iamRole,
    updateManagedInstanceRoleResponse_httpStatus,

    -- ** UpdateOpsItem
    updateOpsItem_actualEndTime,
    updateOpsItem_actualStartTime,
    updateOpsItem_category,
    updateOpsItem_description,
    updateOpsItem_notifications,
    updateOpsItem_operationalData,
    updateOpsItem_operationalDataToDelete,
    updateOpsItem_opsItemArn,
    updateOpsItem_plannedEndTime,
    updateOpsItem_plannedStartTime,
    updateOpsItem_priority,
    updateOpsItem_relatedOpsItems,
    updateOpsItem_severity,
    updateOpsItem_status,
    updateOpsItem_title,
    updateOpsItem_opsItemId,
    updateOpsItemResponse_httpStatus,

    -- ** UpdateOpsMetadata
    updateOpsMetadata_keysToDelete,
    updateOpsMetadata_metadataToUpdate,
    updateOpsMetadata_opsMetadataArn,
    updateOpsMetadataResponse_opsMetadataArn,
    updateOpsMetadataResponse_httpStatus,

    -- ** UpdatePatchBaseline
    updatePatchBaseline_approvalRules,
    updatePatchBaseline_approvedPatches,
    updatePatchBaseline_approvedPatchesComplianceLevel,
    updatePatchBaseline_approvedPatchesEnableNonSecurity,
    updatePatchBaseline_description,
    updatePatchBaseline_globalFilters,
    updatePatchBaseline_name,
    updatePatchBaseline_rejectedPatches,
    updatePatchBaseline_rejectedPatchesAction,
    updatePatchBaseline_replace,
    updatePatchBaseline_sources,
    updatePatchBaseline_baselineId,
    updatePatchBaselineResponse_approvalRules,
    updatePatchBaselineResponse_approvedPatches,
    updatePatchBaselineResponse_approvedPatchesComplianceLevel,
    updatePatchBaselineResponse_approvedPatchesEnableNonSecurity,
    updatePatchBaselineResponse_baselineId,
    updatePatchBaselineResponse_createdDate,
    updatePatchBaselineResponse_description,
    updatePatchBaselineResponse_globalFilters,
    updatePatchBaselineResponse_modifiedDate,
    updatePatchBaselineResponse_name,
    updatePatchBaselineResponse_operatingSystem,
    updatePatchBaselineResponse_rejectedPatches,
    updatePatchBaselineResponse_rejectedPatchesAction,
    updatePatchBaselineResponse_sources,
    updatePatchBaselineResponse_httpStatus,

    -- ** UpdateResourceDataSync
    updateResourceDataSync_syncName,
    updateResourceDataSync_syncType,
    updateResourceDataSync_syncSource,
    updateResourceDataSyncResponse_httpStatus,

    -- ** UpdateServiceSetting
    updateServiceSetting_settingId,
    updateServiceSetting_settingValue,
    updateServiceSettingResponse_httpStatus,

    -- * Types

    -- ** AccountSharingInfo
    accountSharingInfo_accountId,
    accountSharingInfo_sharedDocumentVersion,

    -- ** Activation
    activation_activationId,
    activation_createdDate,
    activation_defaultInstanceName,
    activation_description,
    activation_expirationDate,
    activation_expired,
    activation_iamRole,
    activation_registrationLimit,
    activation_registrationsCount,
    activation_tags,

    -- ** Alarm
    alarm_name,

    -- ** AlarmConfiguration
    alarmConfiguration_ignorePollAlarmFailure,
    alarmConfiguration_alarms,

    -- ** AlarmStateInformation
    alarmStateInformation_name,
    alarmStateInformation_state,

    -- ** Association
    association_associationId,
    association_associationName,
    association_associationVersion,
    association_documentVersion,
    association_instanceId,
    association_lastExecutionDate,
    association_name,
    association_overview,
    association_scheduleExpression,
    association_scheduleOffset,
    association_targetMaps,
    association_targets,

    -- ** AssociationDescription
    associationDescription_alarmConfiguration,
    associationDescription_applyOnlyAtCronInterval,
    associationDescription_associationId,
    associationDescription_associationName,
    associationDescription_associationVersion,
    associationDescription_automationTargetParameterName,
    associationDescription_calendarNames,
    associationDescription_complianceSeverity,
    associationDescription_date,
    associationDescription_documentVersion,
    associationDescription_instanceId,
    associationDescription_lastExecutionDate,
    associationDescription_lastSuccessfulExecutionDate,
    associationDescription_lastUpdateAssociationDate,
    associationDescription_maxConcurrency,
    associationDescription_maxErrors,
    associationDescription_name,
    associationDescription_outputLocation,
    associationDescription_overview,
    associationDescription_parameters,
    associationDescription_scheduleExpression,
    associationDescription_scheduleOffset,
    associationDescription_status,
    associationDescription_syncCompliance,
    associationDescription_targetLocations,
    associationDescription_targetMaps,
    associationDescription_targets,
    associationDescription_triggeredAlarms,

    -- ** AssociationExecution
    associationExecution_alarmConfiguration,
    associationExecution_associationId,
    associationExecution_associationVersion,
    associationExecution_createdTime,
    associationExecution_detailedStatus,
    associationExecution_executionId,
    associationExecution_lastExecutionDate,
    associationExecution_resourceCountByStatus,
    associationExecution_status,
    associationExecution_triggeredAlarms,

    -- ** AssociationExecutionFilter
    associationExecutionFilter_key,
    associationExecutionFilter_value,
    associationExecutionFilter_type,

    -- ** AssociationExecutionTarget
    associationExecutionTarget_associationId,
    associationExecutionTarget_associationVersion,
    associationExecutionTarget_detailedStatus,
    associationExecutionTarget_executionId,
    associationExecutionTarget_lastExecutionDate,
    associationExecutionTarget_outputSource,
    associationExecutionTarget_resourceId,
    associationExecutionTarget_resourceType,
    associationExecutionTarget_status,

    -- ** AssociationExecutionTargetsFilter
    associationExecutionTargetsFilter_key,
    associationExecutionTargetsFilter_value,

    -- ** AssociationFilter
    associationFilter_key,
    associationFilter_value,

    -- ** AssociationOverview
    associationOverview_associationStatusAggregatedCount,
    associationOverview_detailedStatus,
    associationOverview_status,

    -- ** AssociationStatus
    associationStatus_additionalInfo,
    associationStatus_date,
    associationStatus_name,
    associationStatus_message,

    -- ** AssociationVersionInfo
    associationVersionInfo_applyOnlyAtCronInterval,
    associationVersionInfo_associationId,
    associationVersionInfo_associationName,
    associationVersionInfo_associationVersion,
    associationVersionInfo_calendarNames,
    associationVersionInfo_complianceSeverity,
    associationVersionInfo_createdDate,
    associationVersionInfo_documentVersion,
    associationVersionInfo_maxConcurrency,
    associationVersionInfo_maxErrors,
    associationVersionInfo_name,
    associationVersionInfo_outputLocation,
    associationVersionInfo_parameters,
    associationVersionInfo_scheduleExpression,
    associationVersionInfo_scheduleOffset,
    associationVersionInfo_syncCompliance,
    associationVersionInfo_targetLocations,
    associationVersionInfo_targetMaps,
    associationVersionInfo_targets,

    -- ** AttachmentContent
    attachmentContent_hash,
    attachmentContent_hashType,
    attachmentContent_name,
    attachmentContent_size,
    attachmentContent_url,

    -- ** AttachmentInformation
    attachmentInformation_name,

    -- ** AttachmentsSource
    attachmentsSource_key,
    attachmentsSource_name,
    attachmentsSource_values,

    -- ** AutomationExecution
    automationExecution_alarmConfiguration,
    automationExecution_associationId,
    automationExecution_automationExecutionId,
    automationExecution_automationExecutionStatus,
    automationExecution_automationSubtype,
    automationExecution_changeRequestName,
    automationExecution_currentAction,
    automationExecution_currentStepName,
    automationExecution_documentName,
    automationExecution_documentVersion,
    automationExecution_executedBy,
    automationExecution_executionEndTime,
    automationExecution_executionStartTime,
    automationExecution_failureMessage,
    automationExecution_maxConcurrency,
    automationExecution_maxErrors,
    automationExecution_mode,
    automationExecution_opsItemId,
    automationExecution_outputs,
    automationExecution_parameters,
    automationExecution_parentAutomationExecutionId,
    automationExecution_progressCounters,
    automationExecution_resolvedTargets,
    automationExecution_runbooks,
    automationExecution_scheduledTime,
    automationExecution_stepExecutions,
    automationExecution_stepExecutionsTruncated,
    automationExecution_target,
    automationExecution_targetLocations,
    automationExecution_targetMaps,
    automationExecution_targetParameterName,
    automationExecution_targets,
    automationExecution_triggeredAlarms,

    -- ** AutomationExecutionFilter
    automationExecutionFilter_key,
    automationExecutionFilter_values,

    -- ** AutomationExecutionMetadata
    automationExecutionMetadata_alarmConfiguration,
    automationExecutionMetadata_associationId,
    automationExecutionMetadata_automationExecutionId,
    automationExecutionMetadata_automationExecutionStatus,
    automationExecutionMetadata_automationSubtype,
    automationExecutionMetadata_automationType,
    automationExecutionMetadata_changeRequestName,
    automationExecutionMetadata_currentAction,
    automationExecutionMetadata_currentStepName,
    automationExecutionMetadata_documentName,
    automationExecutionMetadata_documentVersion,
    automationExecutionMetadata_executedBy,
    automationExecutionMetadata_executionEndTime,
    automationExecutionMetadata_executionStartTime,
    automationExecutionMetadata_failureMessage,
    automationExecutionMetadata_logFile,
    automationExecutionMetadata_maxConcurrency,
    automationExecutionMetadata_maxErrors,
    automationExecutionMetadata_mode,
    automationExecutionMetadata_opsItemId,
    automationExecutionMetadata_outputs,
    automationExecutionMetadata_parentAutomationExecutionId,
    automationExecutionMetadata_resolvedTargets,
    automationExecutionMetadata_runbooks,
    automationExecutionMetadata_scheduledTime,
    automationExecutionMetadata_target,
    automationExecutionMetadata_targetMaps,
    automationExecutionMetadata_targetParameterName,
    automationExecutionMetadata_targets,
    automationExecutionMetadata_triggeredAlarms,

    -- ** BaselineOverride
    baselineOverride_approvalRules,
    baselineOverride_approvedPatches,
    baselineOverride_approvedPatchesComplianceLevel,
    baselineOverride_approvedPatchesEnableNonSecurity,
    baselineOverride_globalFilters,
    baselineOverride_operatingSystem,
    baselineOverride_rejectedPatches,
    baselineOverride_rejectedPatchesAction,
    baselineOverride_sources,

    -- ** CloudWatchOutputConfig
    cloudWatchOutputConfig_cloudWatchLogGroupName,
    cloudWatchOutputConfig_cloudWatchOutputEnabled,

    -- ** Command
    command_alarmConfiguration,
    command_cloudWatchOutputConfig,
    command_commandId,
    command_comment,
    command_completedCount,
    command_deliveryTimedOutCount,
    command_documentName,
    command_documentVersion,
    command_errorCount,
    command_expiresAfter,
    command_instanceIds,
    command_maxConcurrency,
    command_maxErrors,
    command_notificationConfig,
    command_outputS3BucketName,
    command_outputS3KeyPrefix,
    command_outputS3Region,
    command_parameters,
    command_requestedDateTime,
    command_serviceRole,
    command_status,
    command_statusDetails,
    command_targetCount,
    command_targets,
    command_timeoutSeconds,
    command_triggeredAlarms,

    -- ** CommandFilter
    commandFilter_key,
    commandFilter_value,

    -- ** CommandInvocation
    commandInvocation_cloudWatchOutputConfig,
    commandInvocation_commandId,
    commandInvocation_commandPlugins,
    commandInvocation_comment,
    commandInvocation_documentName,
    commandInvocation_documentVersion,
    commandInvocation_instanceId,
    commandInvocation_instanceName,
    commandInvocation_notificationConfig,
    commandInvocation_requestedDateTime,
    commandInvocation_serviceRole,
    commandInvocation_standardErrorUrl,
    commandInvocation_standardOutputUrl,
    commandInvocation_status,
    commandInvocation_statusDetails,
    commandInvocation_traceOutput,

    -- ** CommandPlugin
    commandPlugin_name,
    commandPlugin_output,
    commandPlugin_outputS3BucketName,
    commandPlugin_outputS3KeyPrefix,
    commandPlugin_outputS3Region,
    commandPlugin_responseCode,
    commandPlugin_responseFinishDateTime,
    commandPlugin_responseStartDateTime,
    commandPlugin_standardErrorUrl,
    commandPlugin_standardOutputUrl,
    commandPlugin_status,
    commandPlugin_statusDetails,

    -- ** ComplianceExecutionSummary
    complianceExecutionSummary_executionId,
    complianceExecutionSummary_executionType,
    complianceExecutionSummary_executionTime,

    -- ** ComplianceItem
    complianceItem_complianceType,
    complianceItem_details,
    complianceItem_executionSummary,
    complianceItem_id,
    complianceItem_resourceId,
    complianceItem_resourceType,
    complianceItem_severity,
    complianceItem_status,
    complianceItem_title,

    -- ** ComplianceItemEntry
    complianceItemEntry_details,
    complianceItemEntry_id,
    complianceItemEntry_title,
    complianceItemEntry_severity,
    complianceItemEntry_status,

    -- ** ComplianceStringFilter
    complianceStringFilter_key,
    complianceStringFilter_type,
    complianceStringFilter_values,

    -- ** ComplianceSummaryItem
    complianceSummaryItem_complianceType,
    complianceSummaryItem_compliantSummary,
    complianceSummaryItem_nonCompliantSummary,

    -- ** CompliantSummary
    compliantSummary_compliantCount,
    compliantSummary_severitySummary,

    -- ** CreateAssociationBatchRequestEntry
    createAssociationBatchRequestEntry_alarmConfiguration,
    createAssociationBatchRequestEntry_applyOnlyAtCronInterval,
    createAssociationBatchRequestEntry_associationName,
    createAssociationBatchRequestEntry_automationTargetParameterName,
    createAssociationBatchRequestEntry_calendarNames,
    createAssociationBatchRequestEntry_complianceSeverity,
    createAssociationBatchRequestEntry_documentVersion,
    createAssociationBatchRequestEntry_instanceId,
    createAssociationBatchRequestEntry_maxConcurrency,
    createAssociationBatchRequestEntry_maxErrors,
    createAssociationBatchRequestEntry_outputLocation,
    createAssociationBatchRequestEntry_parameters,
    createAssociationBatchRequestEntry_scheduleExpression,
    createAssociationBatchRequestEntry_scheduleOffset,
    createAssociationBatchRequestEntry_syncCompliance,
    createAssociationBatchRequestEntry_targetLocations,
    createAssociationBatchRequestEntry_targetMaps,
    createAssociationBatchRequestEntry_targets,
    createAssociationBatchRequestEntry_name,

    -- ** DescribeActivationsFilter
    describeActivationsFilter_filterKey,
    describeActivationsFilter_filterValues,

    -- ** DocumentDefaultVersionDescription
    documentDefaultVersionDescription_defaultVersion,
    documentDefaultVersionDescription_defaultVersionName,
    documentDefaultVersionDescription_name,

    -- ** DocumentDescription
    documentDescription_approvedVersion,
    documentDescription_attachmentsInformation,
    documentDescription_author,
    documentDescription_category,
    documentDescription_categoryEnum,
    documentDescription_createdDate,
    documentDescription_defaultVersion,
    documentDescription_description,
    documentDescription_displayName,
    documentDescription_documentFormat,
    documentDescription_documentType,
    documentDescription_documentVersion,
    documentDescription_hash,
    documentDescription_hashType,
    documentDescription_latestVersion,
    documentDescription_name,
    documentDescription_owner,
    documentDescription_parameters,
    documentDescription_pendingReviewVersion,
    documentDescription_platformTypes,
    documentDescription_requires,
    documentDescription_reviewInformation,
    documentDescription_reviewStatus,
    documentDescription_schemaVersion,
    documentDescription_sha1,
    documentDescription_status,
    documentDescription_statusInformation,
    documentDescription_tags,
    documentDescription_targetType,
    documentDescription_versionName,

    -- ** DocumentFilter
    documentFilter_key,
    documentFilter_value,

    -- ** DocumentIdentifier
    documentIdentifier_author,
    documentIdentifier_createdDate,
    documentIdentifier_displayName,
    documentIdentifier_documentFormat,
    documentIdentifier_documentType,
    documentIdentifier_documentVersion,
    documentIdentifier_name,
    documentIdentifier_owner,
    documentIdentifier_platformTypes,
    documentIdentifier_requires,
    documentIdentifier_reviewStatus,
    documentIdentifier_schemaVersion,
    documentIdentifier_tags,
    documentIdentifier_targetType,
    documentIdentifier_versionName,

    -- ** DocumentKeyValuesFilter
    documentKeyValuesFilter_key,
    documentKeyValuesFilter_values,

    -- ** DocumentMetadataResponseInfo
    documentMetadataResponseInfo_reviewerResponse,

    -- ** DocumentParameter
    documentParameter_defaultValue,
    documentParameter_description,
    documentParameter_name,
    documentParameter_type,

    -- ** DocumentRequires
    documentRequires_requireType,
    documentRequires_version,
    documentRequires_versionName,
    documentRequires_name,

    -- ** DocumentReviewCommentSource
    documentReviewCommentSource_content,
    documentReviewCommentSource_type,

    -- ** DocumentReviewerResponseSource
    documentReviewerResponseSource_comment,
    documentReviewerResponseSource_createTime,
    documentReviewerResponseSource_reviewStatus,
    documentReviewerResponseSource_reviewer,
    documentReviewerResponseSource_updatedTime,

    -- ** DocumentReviews
    documentReviews_comment,
    documentReviews_action,

    -- ** DocumentVersionInfo
    documentVersionInfo_createdDate,
    documentVersionInfo_displayName,
    documentVersionInfo_documentFormat,
    documentVersionInfo_documentVersion,
    documentVersionInfo_isDefaultVersion,
    documentVersionInfo_name,
    documentVersionInfo_reviewStatus,
    documentVersionInfo_status,
    documentVersionInfo_statusInformation,
    documentVersionInfo_versionName,

    -- ** EffectivePatch
    effectivePatch_patch,
    effectivePatch_patchStatus,

    -- ** FailedCreateAssociation
    failedCreateAssociation_entry,
    failedCreateAssociation_fault,
    failedCreateAssociation_message,

    -- ** FailureDetails
    failureDetails_details,
    failureDetails_failureStage,
    failureDetails_failureType,

    -- ** GetResourcePoliciesResponseEntry
    getResourcePoliciesResponseEntry_policy,
    getResourcePoliciesResponseEntry_policyHash,
    getResourcePoliciesResponseEntry_policyId,

    -- ** InstanceAggregatedAssociationOverview
    instanceAggregatedAssociationOverview_detailedStatus,
    instanceAggregatedAssociationOverview_instanceAssociationStatusAggregatedCount,

    -- ** InstanceAssociation
    instanceAssociation_associationId,
    instanceAssociation_associationVersion,
    instanceAssociation_content,
    instanceAssociation_instanceId,

    -- ** InstanceAssociationOutputLocation
    instanceAssociationOutputLocation_s3Location,

    -- ** InstanceAssociationOutputUrl
    instanceAssociationOutputUrl_s3OutputUrl,

    -- ** InstanceAssociationStatusInfo
    instanceAssociationStatusInfo_associationId,
    instanceAssociationStatusInfo_associationName,
    instanceAssociationStatusInfo_associationVersion,
    instanceAssociationStatusInfo_detailedStatus,
    instanceAssociationStatusInfo_documentVersion,
    instanceAssociationStatusInfo_errorCode,
    instanceAssociationStatusInfo_executionDate,
    instanceAssociationStatusInfo_executionSummary,
    instanceAssociationStatusInfo_instanceId,
    instanceAssociationStatusInfo_name,
    instanceAssociationStatusInfo_outputUrl,
    instanceAssociationStatusInfo_status,

    -- ** InstanceInformation
    instanceInformation_activationId,
    instanceInformation_agentVersion,
    instanceInformation_associationOverview,
    instanceInformation_associationStatus,
    instanceInformation_computerName,
    instanceInformation_iPAddress,
    instanceInformation_iamRole,
    instanceInformation_instanceId,
    instanceInformation_isLatestVersion,
    instanceInformation_lastAssociationExecutionDate,
    instanceInformation_lastPingDateTime,
    instanceInformation_lastSuccessfulAssociationExecutionDate,
    instanceInformation_name,
    instanceInformation_pingStatus,
    instanceInformation_platformName,
    instanceInformation_platformType,
    instanceInformation_platformVersion,
    instanceInformation_registrationDate,
    instanceInformation_resourceType,
    instanceInformation_sourceId,
    instanceInformation_sourceType,

    -- ** InstanceInformationFilter
    instanceInformationFilter_key,
    instanceInformationFilter_valueSet,

    -- ** InstanceInformationStringFilter
    instanceInformationStringFilter_key,
    instanceInformationStringFilter_values,

    -- ** InstancePatchState
    instancePatchState_criticalNonCompliantCount,
    instancePatchState_failedCount,
    instancePatchState_installOverrideList,
    instancePatchState_installedCount,
    instancePatchState_installedOtherCount,
    instancePatchState_installedPendingRebootCount,
    instancePatchState_installedRejectedCount,
    instancePatchState_lastNoRebootInstallOperationTime,
    instancePatchState_missingCount,
    instancePatchState_notApplicableCount,
    instancePatchState_otherNonCompliantCount,
    instancePatchState_ownerInformation,
    instancePatchState_rebootOption,
    instancePatchState_securityNonCompliantCount,
    instancePatchState_snapshotId,
    instancePatchState_unreportedNotApplicableCount,
    instancePatchState_instanceId,
    instancePatchState_patchGroup,
    instancePatchState_baselineId,
    instancePatchState_operationStartTime,
    instancePatchState_operationEndTime,
    instancePatchState_operation,

    -- ** InstancePatchStateFilter
    instancePatchStateFilter_key,
    instancePatchStateFilter_values,
    instancePatchStateFilter_type,

    -- ** InventoryAggregator
    inventoryAggregator_aggregators,
    inventoryAggregator_expression,
    inventoryAggregator_groups,

    -- ** InventoryDeletionStatusItem
    inventoryDeletionStatusItem_deletionId,
    inventoryDeletionStatusItem_deletionStartTime,
    inventoryDeletionStatusItem_deletionSummary,
    inventoryDeletionStatusItem_lastStatus,
    inventoryDeletionStatusItem_lastStatusMessage,
    inventoryDeletionStatusItem_lastStatusUpdateTime,
    inventoryDeletionStatusItem_typeName,

    -- ** InventoryDeletionSummary
    inventoryDeletionSummary_remainingCount,
    inventoryDeletionSummary_summaryItems,
    inventoryDeletionSummary_totalCount,

    -- ** InventoryDeletionSummaryItem
    inventoryDeletionSummaryItem_count,
    inventoryDeletionSummaryItem_remainingCount,
    inventoryDeletionSummaryItem_version,

    -- ** InventoryFilter
    inventoryFilter_type,
    inventoryFilter_key,
    inventoryFilter_values,

    -- ** InventoryGroup
    inventoryGroup_name,
    inventoryGroup_filters,

    -- ** InventoryItem
    inventoryItem_content,
    inventoryItem_contentHash,
    inventoryItem_context,
    inventoryItem_typeName,
    inventoryItem_schemaVersion,
    inventoryItem_captureTime,

    -- ** InventoryItemAttribute
    inventoryItemAttribute_name,
    inventoryItemAttribute_dataType,

    -- ** InventoryItemSchema
    inventoryItemSchema_displayName,
    inventoryItemSchema_version,
    inventoryItemSchema_typeName,
    inventoryItemSchema_attributes,

    -- ** InventoryResultEntity
    inventoryResultEntity_data,
    inventoryResultEntity_id,

    -- ** InventoryResultItem
    inventoryResultItem_captureTime,
    inventoryResultItem_contentHash,
    inventoryResultItem_typeName,
    inventoryResultItem_schemaVersion,
    inventoryResultItem_content,

    -- ** LoggingInfo
    loggingInfo_s3KeyPrefix,
    loggingInfo_s3BucketName,
    loggingInfo_s3Region,

    -- ** MaintenanceWindowAutomationParameters
    maintenanceWindowAutomationParameters_documentVersion,
    maintenanceWindowAutomationParameters_parameters,

    -- ** MaintenanceWindowExecution
    maintenanceWindowExecution_endTime,
    maintenanceWindowExecution_startTime,
    maintenanceWindowExecution_status,
    maintenanceWindowExecution_statusDetails,
    maintenanceWindowExecution_windowExecutionId,
    maintenanceWindowExecution_windowId,

    -- ** MaintenanceWindowExecutionTaskIdentity
    maintenanceWindowExecutionTaskIdentity_alarmConfiguration,
    maintenanceWindowExecutionTaskIdentity_endTime,
    maintenanceWindowExecutionTaskIdentity_startTime,
    maintenanceWindowExecutionTaskIdentity_status,
    maintenanceWindowExecutionTaskIdentity_statusDetails,
    maintenanceWindowExecutionTaskIdentity_taskArn,
    maintenanceWindowExecutionTaskIdentity_taskExecutionId,
    maintenanceWindowExecutionTaskIdentity_taskType,
    maintenanceWindowExecutionTaskIdentity_triggeredAlarms,
    maintenanceWindowExecutionTaskIdentity_windowExecutionId,

    -- ** MaintenanceWindowExecutionTaskInvocationIdentity
    maintenanceWindowExecutionTaskInvocationIdentity_endTime,
    maintenanceWindowExecutionTaskInvocationIdentity_executionId,
    maintenanceWindowExecutionTaskInvocationIdentity_invocationId,
    maintenanceWindowExecutionTaskInvocationIdentity_ownerInformation,
    maintenanceWindowExecutionTaskInvocationIdentity_parameters,
    maintenanceWindowExecutionTaskInvocationIdentity_startTime,
    maintenanceWindowExecutionTaskInvocationIdentity_status,
    maintenanceWindowExecutionTaskInvocationIdentity_statusDetails,
    maintenanceWindowExecutionTaskInvocationIdentity_taskExecutionId,
    maintenanceWindowExecutionTaskInvocationIdentity_taskType,
    maintenanceWindowExecutionTaskInvocationIdentity_windowExecutionId,
    maintenanceWindowExecutionTaskInvocationIdentity_windowTargetId,

    -- ** MaintenanceWindowFilter
    maintenanceWindowFilter_key,
    maintenanceWindowFilter_values,

    -- ** MaintenanceWindowIdentity
    maintenanceWindowIdentity_cutoff,
    maintenanceWindowIdentity_description,
    maintenanceWindowIdentity_duration,
    maintenanceWindowIdentity_enabled,
    maintenanceWindowIdentity_endDate,
    maintenanceWindowIdentity_name,
    maintenanceWindowIdentity_nextExecutionTime,
    maintenanceWindowIdentity_schedule,
    maintenanceWindowIdentity_scheduleOffset,
    maintenanceWindowIdentity_scheduleTimezone,
    maintenanceWindowIdentity_startDate,
    maintenanceWindowIdentity_windowId,

    -- ** MaintenanceWindowIdentityForTarget
    maintenanceWindowIdentityForTarget_name,
    maintenanceWindowIdentityForTarget_windowId,

    -- ** MaintenanceWindowLambdaParameters
    maintenanceWindowLambdaParameters_clientContext,
    maintenanceWindowLambdaParameters_payload,
    maintenanceWindowLambdaParameters_qualifier,

    -- ** MaintenanceWindowRunCommandParameters
    maintenanceWindowRunCommandParameters_cloudWatchOutputConfig,
    maintenanceWindowRunCommandParameters_comment,
    maintenanceWindowRunCommandParameters_documentHash,
    maintenanceWindowRunCommandParameters_documentHashType,
    maintenanceWindowRunCommandParameters_documentVersion,
    maintenanceWindowRunCommandParameters_notificationConfig,
    maintenanceWindowRunCommandParameters_outputS3BucketName,
    maintenanceWindowRunCommandParameters_outputS3KeyPrefix,
    maintenanceWindowRunCommandParameters_parameters,
    maintenanceWindowRunCommandParameters_serviceRoleArn,
    maintenanceWindowRunCommandParameters_timeoutSeconds,

    -- ** MaintenanceWindowStepFunctionsParameters
    maintenanceWindowStepFunctionsParameters_input,
    maintenanceWindowStepFunctionsParameters_name,

    -- ** MaintenanceWindowTarget
    maintenanceWindowTarget_description,
    maintenanceWindowTarget_name,
    maintenanceWindowTarget_ownerInformation,
    maintenanceWindowTarget_resourceType,
    maintenanceWindowTarget_targets,
    maintenanceWindowTarget_windowId,
    maintenanceWindowTarget_windowTargetId,

    -- ** MaintenanceWindowTask
    maintenanceWindowTask_alarmConfiguration,
    maintenanceWindowTask_cutoffBehavior,
    maintenanceWindowTask_description,
    maintenanceWindowTask_loggingInfo,
    maintenanceWindowTask_maxConcurrency,
    maintenanceWindowTask_maxErrors,
    maintenanceWindowTask_name,
    maintenanceWindowTask_priority,
    maintenanceWindowTask_serviceRoleArn,
    maintenanceWindowTask_targets,
    maintenanceWindowTask_taskArn,
    maintenanceWindowTask_taskParameters,
    maintenanceWindowTask_type,
    maintenanceWindowTask_windowId,
    maintenanceWindowTask_windowTaskId,

    -- ** MaintenanceWindowTaskInvocationParameters
    maintenanceWindowTaskInvocationParameters_automation,
    maintenanceWindowTaskInvocationParameters_lambda,
    maintenanceWindowTaskInvocationParameters_runCommand,
    maintenanceWindowTaskInvocationParameters_stepFunctions,

    -- ** MaintenanceWindowTaskParameterValueExpression
    maintenanceWindowTaskParameterValueExpression_values,

    -- ** MetadataValue
    metadataValue_value,

    -- ** NonCompliantSummary
    nonCompliantSummary_nonCompliantCount,
    nonCompliantSummary_severitySummary,

    -- ** NotificationConfig
    notificationConfig_notificationArn,
    notificationConfig_notificationEvents,
    notificationConfig_notificationType,

    -- ** OpsAggregator
    opsAggregator_aggregatorType,
    opsAggregator_aggregators,
    opsAggregator_attributeName,
    opsAggregator_filters,
    opsAggregator_typeName,
    opsAggregator_values,

    -- ** OpsEntity
    opsEntity_data,
    opsEntity_id,

    -- ** OpsEntityItem
    opsEntityItem_captureTime,
    opsEntityItem_content,

    -- ** OpsFilter
    opsFilter_type,
    opsFilter_key,
    opsFilter_values,

    -- ** OpsItem
    opsItem_actualEndTime,
    opsItem_actualStartTime,
    opsItem_category,
    opsItem_createdBy,
    opsItem_createdTime,
    opsItem_description,
    opsItem_lastModifiedBy,
    opsItem_lastModifiedTime,
    opsItem_notifications,
    opsItem_operationalData,
    opsItem_opsItemArn,
    opsItem_opsItemId,
    opsItem_opsItemType,
    opsItem_plannedEndTime,
    opsItem_plannedStartTime,
    opsItem_priority,
    opsItem_relatedOpsItems,
    opsItem_severity,
    opsItem_source,
    opsItem_status,
    opsItem_title,
    opsItem_version,

    -- ** OpsItemDataValue
    opsItemDataValue_type,
    opsItemDataValue_value,

    -- ** OpsItemEventFilter
    opsItemEventFilter_key,
    opsItemEventFilter_values,
    opsItemEventFilter_operator,

    -- ** OpsItemEventSummary
    opsItemEventSummary_createdBy,
    opsItemEventSummary_createdTime,
    opsItemEventSummary_detail,
    opsItemEventSummary_detailType,
    opsItemEventSummary_eventId,
    opsItemEventSummary_opsItemId,
    opsItemEventSummary_source,

    -- ** OpsItemFilter
    opsItemFilter_key,
    opsItemFilter_values,
    opsItemFilter_operator,

    -- ** OpsItemIdentity
    opsItemIdentity_arn,

    -- ** OpsItemNotification
    opsItemNotification_arn,

    -- ** OpsItemRelatedItemSummary
    opsItemRelatedItemSummary_associationId,
    opsItemRelatedItemSummary_associationType,
    opsItemRelatedItemSummary_createdBy,
    opsItemRelatedItemSummary_createdTime,
    opsItemRelatedItemSummary_lastModifiedBy,
    opsItemRelatedItemSummary_lastModifiedTime,
    opsItemRelatedItemSummary_opsItemId,
    opsItemRelatedItemSummary_resourceType,
    opsItemRelatedItemSummary_resourceUri,

    -- ** OpsItemRelatedItemsFilter
    opsItemRelatedItemsFilter_key,
    opsItemRelatedItemsFilter_values,
    opsItemRelatedItemsFilter_operator,

    -- ** OpsItemSummary
    opsItemSummary_actualEndTime,
    opsItemSummary_actualStartTime,
    opsItemSummary_category,
    opsItemSummary_createdBy,
    opsItemSummary_createdTime,
    opsItemSummary_lastModifiedBy,
    opsItemSummary_lastModifiedTime,
    opsItemSummary_operationalData,
    opsItemSummary_opsItemId,
    opsItemSummary_opsItemType,
    opsItemSummary_plannedEndTime,
    opsItemSummary_plannedStartTime,
    opsItemSummary_priority,
    opsItemSummary_severity,
    opsItemSummary_source,
    opsItemSummary_status,
    opsItemSummary_title,

    -- ** OpsMetadata
    opsMetadata_creationDate,
    opsMetadata_lastModifiedDate,
    opsMetadata_lastModifiedUser,
    opsMetadata_opsMetadataArn,
    opsMetadata_resourceId,

    -- ** OpsMetadataFilter
    opsMetadataFilter_key,
    opsMetadataFilter_values,

    -- ** OpsResultAttribute
    opsResultAttribute_typeName,

    -- ** OutputSource
    outputSource_outputSourceId,
    outputSource_outputSourceType,

    -- ** Parameter
    parameter_arn,
    parameter_dataType,
    parameter_lastModifiedDate,
    parameter_selector,
    parameter_sourceResult,
    parameter_name,
    parameter_type,
    parameter_value,
    parameter_version,

    -- ** ParameterHistory
    parameterHistory_allowedPattern,
    parameterHistory_dataType,
    parameterHistory_description,
    parameterHistory_keyId,
    parameterHistory_labels,
    parameterHistory_lastModifiedDate,
    parameterHistory_lastModifiedUser,
    parameterHistory_name,
    parameterHistory_policies,
    parameterHistory_tier,
    parameterHistory_type,
    parameterHistory_value,
    parameterHistory_version,

    -- ** ParameterInlinePolicy
    parameterInlinePolicy_policyStatus,
    parameterInlinePolicy_policyText,
    parameterInlinePolicy_policyType,

    -- ** ParameterMetadata
    parameterMetadata_allowedPattern,
    parameterMetadata_dataType,
    parameterMetadata_description,
    parameterMetadata_keyId,
    parameterMetadata_lastModifiedDate,
    parameterMetadata_lastModifiedUser,
    parameterMetadata_name,
    parameterMetadata_policies,
    parameterMetadata_tier,
    parameterMetadata_type,
    parameterMetadata_version,

    -- ** ParameterStringFilter
    parameterStringFilter_option,
    parameterStringFilter_values,
    parameterStringFilter_key,

    -- ** ParametersFilter
    parametersFilter_key,
    parametersFilter_values,

    -- ** Patch
    patch_advisoryIds,
    patch_arch,
    patch_bugzillaIds,
    patch_cVEIds,
    patch_classification,
    patch_contentUrl,
    patch_description,
    patch_epoch,
    patch_id,
    patch_kbNumber,
    patch_language,
    patch_msrcNumber,
    patch_msrcSeverity,
    patch_name,
    patch_product,
    patch_productFamily,
    patch_release,
    patch_releaseDate,
    patch_repository,
    patch_severity,
    patch_title,
    patch_vendor,
    patch_version,

    -- ** PatchBaselineIdentity
    patchBaselineIdentity_baselineDescription,
    patchBaselineIdentity_baselineId,
    patchBaselineIdentity_baselineName,
    patchBaselineIdentity_defaultBaseline,
    patchBaselineIdentity_operatingSystem,

    -- ** PatchComplianceData
    patchComplianceData_cVEIds,
    patchComplianceData_title,
    patchComplianceData_kBId,
    patchComplianceData_classification,
    patchComplianceData_severity,
    patchComplianceData_state,
    patchComplianceData_installedTime,

    -- ** PatchFilter
    patchFilter_key,
    patchFilter_values,

    -- ** PatchFilterGroup
    patchFilterGroup_patchFilters,

    -- ** PatchGroupPatchBaselineMapping
    patchGroupPatchBaselineMapping_baselineIdentity,
    patchGroupPatchBaselineMapping_patchGroup,

    -- ** PatchOrchestratorFilter
    patchOrchestratorFilter_key,
    patchOrchestratorFilter_values,

    -- ** PatchRule
    patchRule_approveAfterDays,
    patchRule_approveUntilDate,
    patchRule_complianceLevel,
    patchRule_enableNonSecurity,
    patchRule_patchFilterGroup,

    -- ** PatchRuleGroup
    patchRuleGroup_patchRules,

    -- ** PatchSource
    patchSource_name,
    patchSource_products,
    patchSource_configuration,

    -- ** PatchStatus
    patchStatus_approvalDate,
    patchStatus_complianceLevel,
    patchStatus_deploymentStatus,

    -- ** ProgressCounters
    progressCounters_cancelledSteps,
    progressCounters_failedSteps,
    progressCounters_successSteps,
    progressCounters_timedOutSteps,
    progressCounters_totalSteps,

    -- ** RegistrationMetadataItem
    registrationMetadataItem_key,
    registrationMetadataItem_value,

    -- ** RelatedOpsItem
    relatedOpsItem_opsItemId,

    -- ** ResolvedTargets
    resolvedTargets_parameterValues,
    resolvedTargets_truncated,

    -- ** ResourceComplianceSummaryItem
    resourceComplianceSummaryItem_complianceType,
    resourceComplianceSummaryItem_compliantSummary,
    resourceComplianceSummaryItem_executionSummary,
    resourceComplianceSummaryItem_nonCompliantSummary,
    resourceComplianceSummaryItem_overallSeverity,
    resourceComplianceSummaryItem_resourceId,
    resourceComplianceSummaryItem_resourceType,
    resourceComplianceSummaryItem_status,

    -- ** ResourceDataSyncAwsOrganizationsSource
    resourceDataSyncAwsOrganizationsSource_organizationalUnits,
    resourceDataSyncAwsOrganizationsSource_organizationSourceType,

    -- ** ResourceDataSyncDestinationDataSharing
    resourceDataSyncDestinationDataSharing_destinationDataSharingType,

    -- ** ResourceDataSyncItem
    resourceDataSyncItem_lastStatus,
    resourceDataSyncItem_lastSuccessfulSyncTime,
    resourceDataSyncItem_lastSyncStatusMessage,
    resourceDataSyncItem_lastSyncTime,
    resourceDataSyncItem_s3Destination,
    resourceDataSyncItem_syncCreatedTime,
    resourceDataSyncItem_syncLastModifiedTime,
    resourceDataSyncItem_syncName,
    resourceDataSyncItem_syncSource,
    resourceDataSyncItem_syncType,

    -- ** ResourceDataSyncOrganizationalUnit
    resourceDataSyncOrganizationalUnit_organizationalUnitId,

    -- ** ResourceDataSyncS3Destination
    resourceDataSyncS3Destination_aWSKMSKeyARN,
    resourceDataSyncS3Destination_destinationDataSharing,
    resourceDataSyncS3Destination_prefix,
    resourceDataSyncS3Destination_bucketName,
    resourceDataSyncS3Destination_syncFormat,
    resourceDataSyncS3Destination_region,

    -- ** ResourceDataSyncSource
    resourceDataSyncSource_awsOrganizationsSource,
    resourceDataSyncSource_enableAllOpsDataSources,
    resourceDataSyncSource_includeFutureRegions,
    resourceDataSyncSource_sourceType,
    resourceDataSyncSource_sourceRegions,

    -- ** ResourceDataSyncSourceWithState
    resourceDataSyncSourceWithState_awsOrganizationsSource,
    resourceDataSyncSourceWithState_enableAllOpsDataSources,
    resourceDataSyncSourceWithState_includeFutureRegions,
    resourceDataSyncSourceWithState_sourceRegions,
    resourceDataSyncSourceWithState_sourceType,
    resourceDataSyncSourceWithState_state,

    -- ** ResultAttribute
    resultAttribute_typeName,

    -- ** ReviewInformation
    reviewInformation_reviewedTime,
    reviewInformation_reviewer,
    reviewInformation_status,

    -- ** Runbook
    runbook_documentVersion,
    runbook_maxConcurrency,
    runbook_maxErrors,
    runbook_parameters,
    runbook_targetLocations,
    runbook_targetMaps,
    runbook_targetParameterName,
    runbook_targets,
    runbook_documentName,

    -- ** S3OutputLocation
    s3OutputLocation_outputS3BucketName,
    s3OutputLocation_outputS3KeyPrefix,
    s3OutputLocation_outputS3Region,

    -- ** S3OutputUrl
    s3OutputUrl_outputUrl,

    -- ** ScheduledWindowExecution
    scheduledWindowExecution_executionTime,
    scheduledWindowExecution_name,
    scheduledWindowExecution_windowId,

    -- ** ServiceSetting
    serviceSetting_arn,
    serviceSetting_lastModifiedDate,
    serviceSetting_lastModifiedUser,
    serviceSetting_settingId,
    serviceSetting_settingValue,
    serviceSetting_status,

    -- ** Session
    session_details,
    session_documentName,
    session_endDate,
    session_maxSessionDuration,
    session_outputUrl,
    session_owner,
    session_reason,
    session_sessionId,
    session_startDate,
    session_status,
    session_target,

    -- ** SessionFilter
    sessionFilter_key,
    sessionFilter_value,

    -- ** SessionManagerOutputUrl
    sessionManagerOutputUrl_cloudWatchOutputUrl,
    sessionManagerOutputUrl_s3OutputUrl,

    -- ** SeveritySummary
    severitySummary_criticalCount,
    severitySummary_highCount,
    severitySummary_informationalCount,
    severitySummary_lowCount,
    severitySummary_mediumCount,
    severitySummary_unspecifiedCount,

    -- ** StepExecution
    stepExecution_action,
    stepExecution_executionEndTime,
    stepExecution_executionStartTime,
    stepExecution_failureDetails,
    stepExecution_failureMessage,
    stepExecution_inputs,
    stepExecution_isCritical,
    stepExecution_isEnd,
    stepExecution_maxAttempts,
    stepExecution_nextStep,
    stepExecution_onFailure,
    stepExecution_outputs,
    stepExecution_overriddenParameters,
    stepExecution_response,
    stepExecution_responseCode,
    stepExecution_stepExecutionId,
    stepExecution_stepName,
    stepExecution_stepStatus,
    stepExecution_targetLocation,
    stepExecution_targets,
    stepExecution_timeoutSeconds,
    stepExecution_triggeredAlarms,
    stepExecution_validNextSteps,

    -- ** StepExecutionFilter
    stepExecutionFilter_key,
    stepExecutionFilter_values,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Target
    target_key,
    target_values,

    -- ** TargetLocation
    targetLocation_accounts,
    targetLocation_executionRoleName,
    targetLocation_regions,
    targetLocation_targetLocationAlarmConfiguration,
    targetLocation_targetLocationMaxConcurrency,
    targetLocation_targetLocationMaxErrors,
  )
where

import Amazonka.SSM.AddTagsToResource
import Amazonka.SSM.AssociateOpsItemRelatedItem
import Amazonka.SSM.CancelCommand
import Amazonka.SSM.CancelMaintenanceWindowExecution
import Amazonka.SSM.CreateActivation
import Amazonka.SSM.CreateAssociation
import Amazonka.SSM.CreateAssociationBatch
import Amazonka.SSM.CreateDocument
import Amazonka.SSM.CreateMaintenanceWindow
import Amazonka.SSM.CreateOpsItem
import Amazonka.SSM.CreateOpsMetadata
import Amazonka.SSM.CreatePatchBaseline
import Amazonka.SSM.CreateResourceDataSync
import Amazonka.SSM.DeleteActivation
import Amazonka.SSM.DeleteAssociation
import Amazonka.SSM.DeleteDocument
import Amazonka.SSM.DeleteInventory
import Amazonka.SSM.DeleteMaintenanceWindow
import Amazonka.SSM.DeleteOpsMetadata
import Amazonka.SSM.DeleteParameter
import Amazonka.SSM.DeleteParameters
import Amazonka.SSM.DeletePatchBaseline
import Amazonka.SSM.DeleteResourceDataSync
import Amazonka.SSM.DeleteResourcePolicy
import Amazonka.SSM.DeregisterManagedInstance
import Amazonka.SSM.DeregisterPatchBaselineForPatchGroup
import Amazonka.SSM.DeregisterTargetFromMaintenanceWindow
import Amazonka.SSM.DeregisterTaskFromMaintenanceWindow
import Amazonka.SSM.DescribeActivations
import Amazonka.SSM.DescribeAssociation
import Amazonka.SSM.DescribeAssociationExecutionTargets
import Amazonka.SSM.DescribeAssociationExecutions
import Amazonka.SSM.DescribeAutomationExecutions
import Amazonka.SSM.DescribeAutomationStepExecutions
import Amazonka.SSM.DescribeAvailablePatches
import Amazonka.SSM.DescribeDocument
import Amazonka.SSM.DescribeDocumentPermission
import Amazonka.SSM.DescribeEffectiveInstanceAssociations
import Amazonka.SSM.DescribeEffectivePatchesForPatchBaseline
import Amazonka.SSM.DescribeInstanceAssociationsStatus
import Amazonka.SSM.DescribeInstanceInformation
import Amazonka.SSM.DescribeInstancePatchStates
import Amazonka.SSM.DescribeInstancePatchStatesForPatchGroup
import Amazonka.SSM.DescribeInstancePatches
import Amazonka.SSM.DescribeInventoryDeletions
import Amazonka.SSM.DescribeMaintenanceWindowExecutionTaskInvocations
import Amazonka.SSM.DescribeMaintenanceWindowExecutionTasks
import Amazonka.SSM.DescribeMaintenanceWindowExecutions
import Amazonka.SSM.DescribeMaintenanceWindowSchedule
import Amazonka.SSM.DescribeMaintenanceWindowTargets
import Amazonka.SSM.DescribeMaintenanceWindowTasks
import Amazonka.SSM.DescribeMaintenanceWindows
import Amazonka.SSM.DescribeMaintenanceWindowsForTarget
import Amazonka.SSM.DescribeOpsItems
import Amazonka.SSM.DescribeParameters
import Amazonka.SSM.DescribePatchBaselines
import Amazonka.SSM.DescribePatchGroupState
import Amazonka.SSM.DescribePatchGroups
import Amazonka.SSM.DescribePatchProperties
import Amazonka.SSM.DescribeSessions
import Amazonka.SSM.DisassociateOpsItemRelatedItem
import Amazonka.SSM.GetAutomationExecution
import Amazonka.SSM.GetCalendarState
import Amazonka.SSM.GetCommandInvocation
import Amazonka.SSM.GetConnectionStatus
import Amazonka.SSM.GetDefaultPatchBaseline
import Amazonka.SSM.GetDeployablePatchSnapshotForInstance
import Amazonka.SSM.GetDocument
import Amazonka.SSM.GetInventory
import Amazonka.SSM.GetInventorySchema
import Amazonka.SSM.GetMaintenanceWindow
import Amazonka.SSM.GetMaintenanceWindowExecution
import Amazonka.SSM.GetMaintenanceWindowExecutionTask
import Amazonka.SSM.GetMaintenanceWindowExecutionTaskInvocation
import Amazonka.SSM.GetMaintenanceWindowTask
import Amazonka.SSM.GetOpsItem
import Amazonka.SSM.GetOpsMetadata
import Amazonka.SSM.GetOpsSummary
import Amazonka.SSM.GetParameter
import Amazonka.SSM.GetParameterHistory
import Amazonka.SSM.GetParameters
import Amazonka.SSM.GetParametersByPath
import Amazonka.SSM.GetPatchBaseline
import Amazonka.SSM.GetPatchBaselineForPatchGroup
import Amazonka.SSM.GetResourcePolicies
import Amazonka.SSM.GetServiceSetting
import Amazonka.SSM.LabelParameterVersion
import Amazonka.SSM.ListAssociationVersions
import Amazonka.SSM.ListAssociations
import Amazonka.SSM.ListCommandInvocations
import Amazonka.SSM.ListCommands
import Amazonka.SSM.ListComplianceItems
import Amazonka.SSM.ListComplianceSummaries
import Amazonka.SSM.ListDocumentMetadataHistory
import Amazonka.SSM.ListDocumentVersions
import Amazonka.SSM.ListDocuments
import Amazonka.SSM.ListInventoryEntries
import Amazonka.SSM.ListOpsItemEvents
import Amazonka.SSM.ListOpsItemRelatedItems
import Amazonka.SSM.ListOpsMetadata
import Amazonka.SSM.ListResourceComplianceSummaries
import Amazonka.SSM.ListResourceDataSync
import Amazonka.SSM.ListTagsForResource
import Amazonka.SSM.ModifyDocumentPermission
import Amazonka.SSM.PutComplianceItems
import Amazonka.SSM.PutInventory
import Amazonka.SSM.PutParameter
import Amazonka.SSM.PutResourcePolicy
import Amazonka.SSM.RegisterDefaultPatchBaseline
import Amazonka.SSM.RegisterPatchBaselineForPatchGroup
import Amazonka.SSM.RegisterTargetWithMaintenanceWindow
import Amazonka.SSM.RegisterTaskWithMaintenanceWindow
import Amazonka.SSM.RemoveTagsFromResource
import Amazonka.SSM.ResetServiceSetting
import Amazonka.SSM.ResumeSession
import Amazonka.SSM.SendAutomationSignal
import Amazonka.SSM.SendCommand
import Amazonka.SSM.StartAssociationsOnce
import Amazonka.SSM.StartAutomationExecution
import Amazonka.SSM.StartChangeRequestExecution
import Amazonka.SSM.StartSession
import Amazonka.SSM.StopAutomationExecution
import Amazonka.SSM.TerminateSession
import Amazonka.SSM.Types.AccountSharingInfo
import Amazonka.SSM.Types.Activation
import Amazonka.SSM.Types.Alarm
import Amazonka.SSM.Types.AlarmConfiguration
import Amazonka.SSM.Types.AlarmStateInformation
import Amazonka.SSM.Types.Association
import Amazonka.SSM.Types.AssociationDescription
import Amazonka.SSM.Types.AssociationExecution
import Amazonka.SSM.Types.AssociationExecutionFilter
import Amazonka.SSM.Types.AssociationExecutionTarget
import Amazonka.SSM.Types.AssociationExecutionTargetsFilter
import Amazonka.SSM.Types.AssociationFilter
import Amazonka.SSM.Types.AssociationOverview
import Amazonka.SSM.Types.AssociationStatus
import Amazonka.SSM.Types.AssociationVersionInfo
import Amazonka.SSM.Types.AttachmentContent
import Amazonka.SSM.Types.AttachmentInformation
import Amazonka.SSM.Types.AttachmentsSource
import Amazonka.SSM.Types.AutomationExecution
import Amazonka.SSM.Types.AutomationExecutionFilter
import Amazonka.SSM.Types.AutomationExecutionMetadata
import Amazonka.SSM.Types.BaselineOverride
import Amazonka.SSM.Types.CloudWatchOutputConfig
import Amazonka.SSM.Types.Command
import Amazonka.SSM.Types.CommandFilter
import Amazonka.SSM.Types.CommandInvocation
import Amazonka.SSM.Types.CommandPlugin
import Amazonka.SSM.Types.ComplianceExecutionSummary
import Amazonka.SSM.Types.ComplianceItem
import Amazonka.SSM.Types.ComplianceItemEntry
import Amazonka.SSM.Types.ComplianceStringFilter
import Amazonka.SSM.Types.ComplianceSummaryItem
import Amazonka.SSM.Types.CompliantSummary
import Amazonka.SSM.Types.CreateAssociationBatchRequestEntry
import Amazonka.SSM.Types.DescribeActivationsFilter
import Amazonka.SSM.Types.DocumentDefaultVersionDescription
import Amazonka.SSM.Types.DocumentDescription
import Amazonka.SSM.Types.DocumentFilter
import Amazonka.SSM.Types.DocumentIdentifier
import Amazonka.SSM.Types.DocumentKeyValuesFilter
import Amazonka.SSM.Types.DocumentMetadataResponseInfo
import Amazonka.SSM.Types.DocumentParameter
import Amazonka.SSM.Types.DocumentRequires
import Amazonka.SSM.Types.DocumentReviewCommentSource
import Amazonka.SSM.Types.DocumentReviewerResponseSource
import Amazonka.SSM.Types.DocumentReviews
import Amazonka.SSM.Types.DocumentVersionInfo
import Amazonka.SSM.Types.EffectivePatch
import Amazonka.SSM.Types.FailedCreateAssociation
import Amazonka.SSM.Types.FailureDetails
import Amazonka.SSM.Types.GetResourcePoliciesResponseEntry
import Amazonka.SSM.Types.InstanceAggregatedAssociationOverview
import Amazonka.SSM.Types.InstanceAssociation
import Amazonka.SSM.Types.InstanceAssociationOutputLocation
import Amazonka.SSM.Types.InstanceAssociationOutputUrl
import Amazonka.SSM.Types.InstanceAssociationStatusInfo
import Amazonka.SSM.Types.InstanceInformation
import Amazonka.SSM.Types.InstanceInformationFilter
import Amazonka.SSM.Types.InstanceInformationStringFilter
import Amazonka.SSM.Types.InstancePatchState
import Amazonka.SSM.Types.InstancePatchStateFilter
import Amazonka.SSM.Types.InventoryAggregator
import Amazonka.SSM.Types.InventoryDeletionStatusItem
import Amazonka.SSM.Types.InventoryDeletionSummary
import Amazonka.SSM.Types.InventoryDeletionSummaryItem
import Amazonka.SSM.Types.InventoryFilter
import Amazonka.SSM.Types.InventoryGroup
import Amazonka.SSM.Types.InventoryItem
import Amazonka.SSM.Types.InventoryItemAttribute
import Amazonka.SSM.Types.InventoryItemSchema
import Amazonka.SSM.Types.InventoryResultEntity
import Amazonka.SSM.Types.InventoryResultItem
import Amazonka.SSM.Types.LoggingInfo
import Amazonka.SSM.Types.MaintenanceWindowAutomationParameters
import Amazonka.SSM.Types.MaintenanceWindowExecution
import Amazonka.SSM.Types.MaintenanceWindowExecutionTaskIdentity
import Amazonka.SSM.Types.MaintenanceWindowExecutionTaskInvocationIdentity
import Amazonka.SSM.Types.MaintenanceWindowFilter
import Amazonka.SSM.Types.MaintenanceWindowIdentity
import Amazonka.SSM.Types.MaintenanceWindowIdentityForTarget
import Amazonka.SSM.Types.MaintenanceWindowLambdaParameters
import Amazonka.SSM.Types.MaintenanceWindowRunCommandParameters
import Amazonka.SSM.Types.MaintenanceWindowStepFunctionsParameters
import Amazonka.SSM.Types.MaintenanceWindowTarget
import Amazonka.SSM.Types.MaintenanceWindowTask
import Amazonka.SSM.Types.MaintenanceWindowTaskInvocationParameters
import Amazonka.SSM.Types.MaintenanceWindowTaskParameterValueExpression
import Amazonka.SSM.Types.MetadataValue
import Amazonka.SSM.Types.NonCompliantSummary
import Amazonka.SSM.Types.NotificationConfig
import Amazonka.SSM.Types.OpsAggregator
import Amazonka.SSM.Types.OpsEntity
import Amazonka.SSM.Types.OpsEntityItem
import Amazonka.SSM.Types.OpsFilter
import Amazonka.SSM.Types.OpsItem
import Amazonka.SSM.Types.OpsItemDataValue
import Amazonka.SSM.Types.OpsItemEventFilter
import Amazonka.SSM.Types.OpsItemEventSummary
import Amazonka.SSM.Types.OpsItemFilter
import Amazonka.SSM.Types.OpsItemIdentity
import Amazonka.SSM.Types.OpsItemNotification
import Amazonka.SSM.Types.OpsItemRelatedItemSummary
import Amazonka.SSM.Types.OpsItemRelatedItemsFilter
import Amazonka.SSM.Types.OpsItemSummary
import Amazonka.SSM.Types.OpsMetadata
import Amazonka.SSM.Types.OpsMetadataFilter
import Amazonka.SSM.Types.OpsResultAttribute
import Amazonka.SSM.Types.OutputSource
import Amazonka.SSM.Types.Parameter
import Amazonka.SSM.Types.ParameterHistory
import Amazonka.SSM.Types.ParameterInlinePolicy
import Amazonka.SSM.Types.ParameterMetadata
import Amazonka.SSM.Types.ParameterStringFilter
import Amazonka.SSM.Types.ParametersFilter
import Amazonka.SSM.Types.Patch
import Amazonka.SSM.Types.PatchBaselineIdentity
import Amazonka.SSM.Types.PatchComplianceData
import Amazonka.SSM.Types.PatchFilter
import Amazonka.SSM.Types.PatchFilterGroup
import Amazonka.SSM.Types.PatchGroupPatchBaselineMapping
import Amazonka.SSM.Types.PatchOrchestratorFilter
import Amazonka.SSM.Types.PatchRule
import Amazonka.SSM.Types.PatchRuleGroup
import Amazonka.SSM.Types.PatchSource
import Amazonka.SSM.Types.PatchStatus
import Amazonka.SSM.Types.ProgressCounters
import Amazonka.SSM.Types.RegistrationMetadataItem
import Amazonka.SSM.Types.RelatedOpsItem
import Amazonka.SSM.Types.ResolvedTargets
import Amazonka.SSM.Types.ResourceComplianceSummaryItem
import Amazonka.SSM.Types.ResourceDataSyncAwsOrganizationsSource
import Amazonka.SSM.Types.ResourceDataSyncDestinationDataSharing
import Amazonka.SSM.Types.ResourceDataSyncItem
import Amazonka.SSM.Types.ResourceDataSyncOrganizationalUnit
import Amazonka.SSM.Types.ResourceDataSyncS3Destination
import Amazonka.SSM.Types.ResourceDataSyncSource
import Amazonka.SSM.Types.ResourceDataSyncSourceWithState
import Amazonka.SSM.Types.ResultAttribute
import Amazonka.SSM.Types.ReviewInformation
import Amazonka.SSM.Types.Runbook
import Amazonka.SSM.Types.S3OutputLocation
import Amazonka.SSM.Types.S3OutputUrl
import Amazonka.SSM.Types.ScheduledWindowExecution
import Amazonka.SSM.Types.ServiceSetting
import Amazonka.SSM.Types.Session
import Amazonka.SSM.Types.SessionFilter
import Amazonka.SSM.Types.SessionManagerOutputUrl
import Amazonka.SSM.Types.SeveritySummary
import Amazonka.SSM.Types.StepExecution
import Amazonka.SSM.Types.StepExecutionFilter
import Amazonka.SSM.Types.Tag
import Amazonka.SSM.Types.Target
import Amazonka.SSM.Types.TargetLocation
import Amazonka.SSM.UnlabelParameterVersion
import Amazonka.SSM.UpdateAssociation
import Amazonka.SSM.UpdateAssociationStatus
import Amazonka.SSM.UpdateDocument
import Amazonka.SSM.UpdateDocumentDefaultVersion
import Amazonka.SSM.UpdateDocumentMetadata
import Amazonka.SSM.UpdateMaintenanceWindow
import Amazonka.SSM.UpdateMaintenanceWindowTarget
import Amazonka.SSM.UpdateMaintenanceWindowTask
import Amazonka.SSM.UpdateManagedInstanceRole
import Amazonka.SSM.UpdateOpsItem
import Amazonka.SSM.UpdateOpsMetadata
import Amazonka.SSM.UpdatePatchBaseline
import Amazonka.SSM.UpdateResourceDataSync
import Amazonka.SSM.UpdateServiceSetting
