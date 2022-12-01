{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSM.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    createActivation_tags,
    createActivation_defaultInstanceName,
    createActivation_description,
    createActivation_registrationMetadata,
    createActivation_registrationLimit,
    createActivation_expirationDate,
    createActivation_iamRole,
    createActivationResponse_activationId,
    createActivationResponse_activationCode,
    createActivationResponse_httpStatus,

    -- ** CreateAssociation
    createAssociation_tags,
    createAssociation_associationName,
    createAssociation_targetLocations,
    createAssociation_automationTargetParameterName,
    createAssociation_targetMaps,
    createAssociation_outputLocation,
    createAssociation_targets,
    createAssociation_calendarNames,
    createAssociation_scheduleExpression,
    createAssociation_scheduleOffset,
    createAssociation_instanceId,
    createAssociation_alarmConfiguration,
    createAssociation_maxConcurrency,
    createAssociation_applyOnlyAtCronInterval,
    createAssociation_maxErrors,
    createAssociation_complianceSeverity,
    createAssociation_syncCompliance,
    createAssociation_documentVersion,
    createAssociation_parameters,
    createAssociation_name,
    createAssociationResponse_associationDescription,
    createAssociationResponse_httpStatus,

    -- ** CreateAssociationBatch
    createAssociationBatch_entries,
    createAssociationBatchResponse_failed,
    createAssociationBatchResponse_successful,
    createAssociationBatchResponse_httpStatus,

    -- ** CreateDocument
    createDocument_tags,
    createDocument_requires,
    createDocument_documentType,
    createDocument_displayName,
    createDocument_targetType,
    createDocument_attachments,
    createDocument_versionName,
    createDocument_documentFormat,
    createDocument_content,
    createDocument_name,
    createDocumentResponse_documentDescription,
    createDocumentResponse_httpStatus,

    -- ** CreateMaintenanceWindow
    createMaintenanceWindow_tags,
    createMaintenanceWindow_clientToken,
    createMaintenanceWindow_endDate,
    createMaintenanceWindow_description,
    createMaintenanceWindow_scheduleTimezone,
    createMaintenanceWindow_scheduleOffset,
    createMaintenanceWindow_startDate,
    createMaintenanceWindow_name,
    createMaintenanceWindow_schedule,
    createMaintenanceWindow_duration,
    createMaintenanceWindow_cutoff,
    createMaintenanceWindow_allowUnassociatedTargets,
    createMaintenanceWindowResponse_windowId,
    createMaintenanceWindowResponse_httpStatus,

    -- ** CreateOpsItem
    createOpsItem_tags,
    createOpsItem_notifications,
    createOpsItem_severity,
    createOpsItem_plannedStartTime,
    createOpsItem_plannedEndTime,
    createOpsItem_accountId,
    createOpsItem_priority,
    createOpsItem_opsItemType,
    createOpsItem_category,
    createOpsItem_operationalData,
    createOpsItem_actualStartTime,
    createOpsItem_actualEndTime,
    createOpsItem_relatedOpsItems,
    createOpsItem_description,
    createOpsItem_source,
    createOpsItem_title,
    createOpsItemResponse_opsItemArn,
    createOpsItemResponse_opsItemId,
    createOpsItemResponse_httpStatus,

    -- ** CreateOpsMetadata
    createOpsMetadata_tags,
    createOpsMetadata_metadata,
    createOpsMetadata_resourceId,
    createOpsMetadataResponse_opsMetadataArn,
    createOpsMetadataResponse_httpStatus,

    -- ** CreatePatchBaseline
    createPatchBaseline_tags,
    createPatchBaseline_operatingSystem,
    createPatchBaseline_approvedPatches,
    createPatchBaseline_approvedPatchesComplianceLevel,
    createPatchBaseline_sources,
    createPatchBaseline_clientToken,
    createPatchBaseline_approvalRules,
    createPatchBaseline_rejectedPatchesAction,
    createPatchBaseline_description,
    createPatchBaseline_globalFilters,
    createPatchBaseline_rejectedPatches,
    createPatchBaseline_approvedPatchesEnableNonSecurity,
    createPatchBaseline_name,
    createPatchBaselineResponse_baselineId,
    createPatchBaselineResponse_httpStatus,

    -- ** CreateResourceDataSync
    createResourceDataSync_syncType,
    createResourceDataSync_s3Destination,
    createResourceDataSync_syncSource,
    createResourceDataSync_syncName,
    createResourceDataSyncResponse_httpStatus,

    -- ** DeleteActivation
    deleteActivation_activationId,
    deleteActivationResponse_httpStatus,

    -- ** DeleteAssociation
    deleteAssociation_name,
    deleteAssociation_instanceId,
    deleteAssociation_associationId,
    deleteAssociationResponse_httpStatus,

    -- ** DeleteDocument
    deleteDocument_versionName,
    deleteDocument_force,
    deleteDocument_documentVersion,
    deleteDocument_name,
    deleteDocumentResponse_httpStatus,

    -- ** DeleteInventory
    deleteInventory_clientToken,
    deleteInventory_schemaDeleteOption,
    deleteInventory_dryRun,
    deleteInventory_typeName,
    deleteInventoryResponse_typeName,
    deleteInventoryResponse_deletionSummary,
    deleteInventoryResponse_deletionId,
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
    deregisterTargetFromMaintenanceWindowResponse_windowTargetId,
    deregisterTargetFromMaintenanceWindowResponse_windowId,
    deregisterTargetFromMaintenanceWindowResponse_httpStatus,

    -- ** DeregisterTaskFromMaintenanceWindow
    deregisterTaskFromMaintenanceWindow_windowId,
    deregisterTaskFromMaintenanceWindow_windowTaskId,
    deregisterTaskFromMaintenanceWindowResponse_windowTaskId,
    deregisterTaskFromMaintenanceWindowResponse_windowId,
    deregisterTaskFromMaintenanceWindowResponse_httpStatus,

    -- ** DescribeActivations
    describeActivations_nextToken,
    describeActivations_filters,
    describeActivations_maxResults,
    describeActivationsResponse_nextToken,
    describeActivationsResponse_activationList,
    describeActivationsResponse_httpStatus,

    -- ** DescribeAssociation
    describeAssociation_name,
    describeAssociation_associationVersion,
    describeAssociation_instanceId,
    describeAssociation_associationId,
    describeAssociationResponse_associationDescription,
    describeAssociationResponse_httpStatus,

    -- ** DescribeAssociationExecutionTargets
    describeAssociationExecutionTargets_nextToken,
    describeAssociationExecutionTargets_filters,
    describeAssociationExecutionTargets_maxResults,
    describeAssociationExecutionTargets_associationId,
    describeAssociationExecutionTargets_executionId,
    describeAssociationExecutionTargetsResponse_nextToken,
    describeAssociationExecutionTargetsResponse_associationExecutionTargets,
    describeAssociationExecutionTargetsResponse_httpStatus,

    -- ** DescribeAssociationExecutions
    describeAssociationExecutions_nextToken,
    describeAssociationExecutions_filters,
    describeAssociationExecutions_maxResults,
    describeAssociationExecutions_associationId,
    describeAssociationExecutionsResponse_associationExecutions,
    describeAssociationExecutionsResponse_nextToken,
    describeAssociationExecutionsResponse_httpStatus,

    -- ** DescribeAutomationExecutions
    describeAutomationExecutions_nextToken,
    describeAutomationExecutions_filters,
    describeAutomationExecutions_maxResults,
    describeAutomationExecutionsResponse_nextToken,
    describeAutomationExecutionsResponse_automationExecutionMetadataList,
    describeAutomationExecutionsResponse_httpStatus,

    -- ** DescribeAutomationStepExecutions
    describeAutomationStepExecutions_nextToken,
    describeAutomationStepExecutions_filters,
    describeAutomationStepExecutions_maxResults,
    describeAutomationStepExecutions_reverseOrder,
    describeAutomationStepExecutions_automationExecutionId,
    describeAutomationStepExecutionsResponse_nextToken,
    describeAutomationStepExecutionsResponse_stepExecutions,
    describeAutomationStepExecutionsResponse_httpStatus,

    -- ** DescribeAvailablePatches
    describeAvailablePatches_nextToken,
    describeAvailablePatches_filters,
    describeAvailablePatches_maxResults,
    describeAvailablePatchesResponse_nextToken,
    describeAvailablePatchesResponse_patches,
    describeAvailablePatchesResponse_httpStatus,

    -- ** DescribeDocument
    describeDocument_versionName,
    describeDocument_documentVersion,
    describeDocument_name,
    describeDocumentResponse_document,
    describeDocumentResponse_httpStatus,

    -- ** DescribeDocumentPermission
    describeDocumentPermission_nextToken,
    describeDocumentPermission_maxResults,
    describeDocumentPermission_name,
    describeDocumentPermission_permissionType,
    describeDocumentPermissionResponse_accountIds,
    describeDocumentPermissionResponse_nextToken,
    describeDocumentPermissionResponse_accountSharingInfoList,
    describeDocumentPermissionResponse_httpStatus,

    -- ** DescribeEffectiveInstanceAssociations
    describeEffectiveInstanceAssociations_nextToken,
    describeEffectiveInstanceAssociations_maxResults,
    describeEffectiveInstanceAssociations_instanceId,
    describeEffectiveInstanceAssociationsResponse_nextToken,
    describeEffectiveInstanceAssociationsResponse_associations,
    describeEffectiveInstanceAssociationsResponse_httpStatus,

    -- ** DescribeEffectivePatchesForPatchBaseline
    describeEffectivePatchesForPatchBaseline_nextToken,
    describeEffectivePatchesForPatchBaseline_maxResults,
    describeEffectivePatchesForPatchBaseline_baselineId,
    describeEffectivePatchesForPatchBaselineResponse_nextToken,
    describeEffectivePatchesForPatchBaselineResponse_effectivePatches,
    describeEffectivePatchesForPatchBaselineResponse_httpStatus,

    -- ** DescribeInstanceAssociationsStatus
    describeInstanceAssociationsStatus_nextToken,
    describeInstanceAssociationsStatus_maxResults,
    describeInstanceAssociationsStatus_instanceId,
    describeInstanceAssociationsStatusResponse_nextToken,
    describeInstanceAssociationsStatusResponse_instanceAssociationStatusInfos,
    describeInstanceAssociationsStatusResponse_httpStatus,

    -- ** DescribeInstanceInformation
    describeInstanceInformation_nextToken,
    describeInstanceInformation_filters,
    describeInstanceInformation_maxResults,
    describeInstanceInformation_instanceInformationFilterList,
    describeInstanceInformationResponse_nextToken,
    describeInstanceInformationResponse_instanceInformationList,
    describeInstanceInformationResponse_httpStatus,

    -- ** DescribeInstancePatchStates
    describeInstancePatchStates_nextToken,
    describeInstancePatchStates_maxResults,
    describeInstancePatchStates_instanceIds,
    describeInstancePatchStatesResponse_nextToken,
    describeInstancePatchStatesResponse_instancePatchStates,
    describeInstancePatchStatesResponse_httpStatus,

    -- ** DescribeInstancePatchStatesForPatchGroup
    describeInstancePatchStatesForPatchGroup_nextToken,
    describeInstancePatchStatesForPatchGroup_filters,
    describeInstancePatchStatesForPatchGroup_maxResults,
    describeInstancePatchStatesForPatchGroup_patchGroup,
    describeInstancePatchStatesForPatchGroupResponse_nextToken,
    describeInstancePatchStatesForPatchGroupResponse_instancePatchStates,
    describeInstancePatchStatesForPatchGroupResponse_httpStatus,

    -- ** DescribeInstancePatches
    describeInstancePatches_nextToken,
    describeInstancePatches_filters,
    describeInstancePatches_maxResults,
    describeInstancePatches_instanceId,
    describeInstancePatchesResponse_nextToken,
    describeInstancePatchesResponse_patches,
    describeInstancePatchesResponse_httpStatus,

    -- ** DescribeInventoryDeletions
    describeInventoryDeletions_nextToken,
    describeInventoryDeletions_maxResults,
    describeInventoryDeletions_deletionId,
    describeInventoryDeletionsResponse_nextToken,
    describeInventoryDeletionsResponse_inventoryDeletions,
    describeInventoryDeletionsResponse_httpStatus,

    -- ** DescribeMaintenanceWindowExecutionTaskInvocations
    describeMaintenanceWindowExecutionTaskInvocations_nextToken,
    describeMaintenanceWindowExecutionTaskInvocations_filters,
    describeMaintenanceWindowExecutionTaskInvocations_maxResults,
    describeMaintenanceWindowExecutionTaskInvocations_windowExecutionId,
    describeMaintenanceWindowExecutionTaskInvocations_taskId,
    describeMaintenanceWindowExecutionTaskInvocationsResponse_nextToken,
    describeMaintenanceWindowExecutionTaskInvocationsResponse_windowExecutionTaskInvocationIdentities,
    describeMaintenanceWindowExecutionTaskInvocationsResponse_httpStatus,

    -- ** DescribeMaintenanceWindowExecutionTasks
    describeMaintenanceWindowExecutionTasks_nextToken,
    describeMaintenanceWindowExecutionTasks_filters,
    describeMaintenanceWindowExecutionTasks_maxResults,
    describeMaintenanceWindowExecutionTasks_windowExecutionId,
    describeMaintenanceWindowExecutionTasksResponse_nextToken,
    describeMaintenanceWindowExecutionTasksResponse_windowExecutionTaskIdentities,
    describeMaintenanceWindowExecutionTasksResponse_httpStatus,

    -- ** DescribeMaintenanceWindowExecutions
    describeMaintenanceWindowExecutions_nextToken,
    describeMaintenanceWindowExecutions_filters,
    describeMaintenanceWindowExecutions_maxResults,
    describeMaintenanceWindowExecutions_windowId,
    describeMaintenanceWindowExecutionsResponse_nextToken,
    describeMaintenanceWindowExecutionsResponse_windowExecutions,
    describeMaintenanceWindowExecutionsResponse_httpStatus,

    -- ** DescribeMaintenanceWindowSchedule
    describeMaintenanceWindowSchedule_resourceType,
    describeMaintenanceWindowSchedule_nextToken,
    describeMaintenanceWindowSchedule_filters,
    describeMaintenanceWindowSchedule_windowId,
    describeMaintenanceWindowSchedule_targets,
    describeMaintenanceWindowSchedule_maxResults,
    describeMaintenanceWindowScheduleResponse_nextToken,
    describeMaintenanceWindowScheduleResponse_scheduledWindowExecutions,
    describeMaintenanceWindowScheduleResponse_httpStatus,

    -- ** DescribeMaintenanceWindowTargets
    describeMaintenanceWindowTargets_nextToken,
    describeMaintenanceWindowTargets_filters,
    describeMaintenanceWindowTargets_maxResults,
    describeMaintenanceWindowTargets_windowId,
    describeMaintenanceWindowTargetsResponse_nextToken,
    describeMaintenanceWindowTargetsResponse_targets,
    describeMaintenanceWindowTargetsResponse_httpStatus,

    -- ** DescribeMaintenanceWindowTasks
    describeMaintenanceWindowTasks_nextToken,
    describeMaintenanceWindowTasks_filters,
    describeMaintenanceWindowTasks_maxResults,
    describeMaintenanceWindowTasks_windowId,
    describeMaintenanceWindowTasksResponse_tasks,
    describeMaintenanceWindowTasksResponse_nextToken,
    describeMaintenanceWindowTasksResponse_httpStatus,

    -- ** DescribeMaintenanceWindows
    describeMaintenanceWindows_nextToken,
    describeMaintenanceWindows_filters,
    describeMaintenanceWindows_maxResults,
    describeMaintenanceWindowsResponse_nextToken,
    describeMaintenanceWindowsResponse_windowIdentities,
    describeMaintenanceWindowsResponse_httpStatus,

    -- ** DescribeMaintenanceWindowsForTarget
    describeMaintenanceWindowsForTarget_nextToken,
    describeMaintenanceWindowsForTarget_maxResults,
    describeMaintenanceWindowsForTarget_targets,
    describeMaintenanceWindowsForTarget_resourceType,
    describeMaintenanceWindowsForTargetResponse_nextToken,
    describeMaintenanceWindowsForTargetResponse_windowIdentities,
    describeMaintenanceWindowsForTargetResponse_httpStatus,

    -- ** DescribeOpsItems
    describeOpsItems_nextToken,
    describeOpsItems_opsItemFilters,
    describeOpsItems_maxResults,
    describeOpsItemsResponse_opsItemSummaries,
    describeOpsItemsResponse_nextToken,
    describeOpsItemsResponse_httpStatus,

    -- ** DescribeParameters
    describeParameters_nextToken,
    describeParameters_filters,
    describeParameters_parameterFilters,
    describeParameters_maxResults,
    describeParametersResponse_nextToken,
    describeParametersResponse_parameters,
    describeParametersResponse_httpStatus,

    -- ** DescribePatchBaselines
    describePatchBaselines_nextToken,
    describePatchBaselines_filters,
    describePatchBaselines_maxResults,
    describePatchBaselinesResponse_nextToken,
    describePatchBaselinesResponse_baselineIdentities,
    describePatchBaselinesResponse_httpStatus,

    -- ** DescribePatchGroupState
    describePatchGroupState_patchGroup,
    describePatchGroupStateResponse_instances,
    describePatchGroupStateResponse_instancesWithInstalledPatches,
    describePatchGroupStateResponse_instancesWithUnreportedNotApplicablePatches,
    describePatchGroupStateResponse_instancesWithInstalledOtherPatches,
    describePatchGroupStateResponse_instancesWithSecurityNonCompliantPatches,
    describePatchGroupStateResponse_instancesWithFailedPatches,
    describePatchGroupStateResponse_instancesWithOtherNonCompliantPatches,
    describePatchGroupStateResponse_instancesWithNotApplicablePatches,
    describePatchGroupStateResponse_instancesWithMissingPatches,
    describePatchGroupStateResponse_instancesWithInstalledRejectedPatches,
    describePatchGroupStateResponse_instancesWithCriticalNonCompliantPatches,
    describePatchGroupStateResponse_instancesWithInstalledPendingRebootPatches,
    describePatchGroupStateResponse_httpStatus,

    -- ** DescribePatchGroups
    describePatchGroups_nextToken,
    describePatchGroups_filters,
    describePatchGroups_maxResults,
    describePatchGroupsResponse_nextToken,
    describePatchGroupsResponse_mappings,
    describePatchGroupsResponse_httpStatus,

    -- ** DescribePatchProperties
    describePatchProperties_nextToken,
    describePatchProperties_maxResults,
    describePatchProperties_patchSet,
    describePatchProperties_operatingSystem,
    describePatchProperties_property,
    describePatchPropertiesResponse_nextToken,
    describePatchPropertiesResponse_properties,
    describePatchPropertiesResponse_httpStatus,

    -- ** DescribeSessions
    describeSessions_nextToken,
    describeSessions_filters,
    describeSessions_maxResults,
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
    getCalendarStateResponse_state,
    getCalendarStateResponse_nextTransitionTime,
    getCalendarStateResponse_atTime,
    getCalendarStateResponse_httpStatus,

    -- ** GetCommandInvocation
    getCommandInvocation_pluginName,
    getCommandInvocation_commandId,
    getCommandInvocation_instanceId,
    getCommandInvocationResponse_statusDetails,
    getCommandInvocationResponse_standardErrorContent,
    getCommandInvocationResponse_cloudWatchOutputConfig,
    getCommandInvocationResponse_status,
    getCommandInvocationResponse_standardErrorUrl,
    getCommandInvocationResponse_commandId,
    getCommandInvocationResponse_standardOutputContent,
    getCommandInvocationResponse_comment,
    getCommandInvocationResponse_instanceId,
    getCommandInvocationResponse_pluginName,
    getCommandInvocationResponse_documentName,
    getCommandInvocationResponse_executionEndDateTime,
    getCommandInvocationResponse_executionStartDateTime,
    getCommandInvocationResponse_executionElapsedTime,
    getCommandInvocationResponse_responseCode,
    getCommandInvocationResponse_documentVersion,
    getCommandInvocationResponse_standardOutputUrl,
    getCommandInvocationResponse_httpStatus,

    -- ** GetConnectionStatus
    getConnectionStatus_target,
    getConnectionStatusResponse_status,
    getConnectionStatusResponse_target,
    getConnectionStatusResponse_httpStatus,

    -- ** GetDefaultPatchBaseline
    getDefaultPatchBaseline_operatingSystem,
    getDefaultPatchBaselineResponse_operatingSystem,
    getDefaultPatchBaselineResponse_baselineId,
    getDefaultPatchBaselineResponse_httpStatus,

    -- ** GetDeployablePatchSnapshotForInstance
    getDeployablePatchSnapshotForInstance_baselineOverride,
    getDeployablePatchSnapshotForInstance_instanceId,
    getDeployablePatchSnapshotForInstance_snapshotId,
    getDeployablePatchSnapshotForInstanceResponse_product,
    getDeployablePatchSnapshotForInstanceResponse_snapshotId,
    getDeployablePatchSnapshotForInstanceResponse_instanceId,
    getDeployablePatchSnapshotForInstanceResponse_snapshotDownloadUrl,
    getDeployablePatchSnapshotForInstanceResponse_httpStatus,

    -- ** GetDocument
    getDocument_versionName,
    getDocument_documentFormat,
    getDocument_documentVersion,
    getDocument_name,
    getDocumentResponse_requires,
    getDocumentResponse_documentType,
    getDocumentResponse_name,
    getDocumentResponse_displayName,
    getDocumentResponse_status,
    getDocumentResponse_versionName,
    getDocumentResponse_statusInformation,
    getDocumentResponse_createdDate,
    getDocumentResponse_content,
    getDocumentResponse_attachmentsContent,
    getDocumentResponse_reviewStatus,
    getDocumentResponse_documentFormat,
    getDocumentResponse_documentVersion,
    getDocumentResponse_httpStatus,

    -- ** GetInventory
    getInventory_resultAttributes,
    getInventory_nextToken,
    getInventory_filters,
    getInventory_aggregators,
    getInventory_maxResults,
    getInventoryResponse_entities,
    getInventoryResponse_nextToken,
    getInventoryResponse_httpStatus,

    -- ** GetInventorySchema
    getInventorySchema_nextToken,
    getInventorySchema_typeName,
    getInventorySchema_aggregator,
    getInventorySchema_maxResults,
    getInventorySchema_subType,
    getInventorySchemaResponse_nextToken,
    getInventorySchemaResponse_schemas,
    getInventorySchemaResponse_httpStatus,

    -- ** GetMaintenanceWindow
    getMaintenanceWindow_windowId,
    getMaintenanceWindowResponse_schedule,
    getMaintenanceWindowResponse_cutoff,
    getMaintenanceWindowResponse_name,
    getMaintenanceWindowResponse_endDate,
    getMaintenanceWindowResponse_windowId,
    getMaintenanceWindowResponse_description,
    getMaintenanceWindowResponse_enabled,
    getMaintenanceWindowResponse_duration,
    getMaintenanceWindowResponse_scheduleTimezone,
    getMaintenanceWindowResponse_scheduleOffset,
    getMaintenanceWindowResponse_startDate,
    getMaintenanceWindowResponse_createdDate,
    getMaintenanceWindowResponse_modifiedDate,
    getMaintenanceWindowResponse_nextExecutionTime,
    getMaintenanceWindowResponse_allowUnassociatedTargets,
    getMaintenanceWindowResponse_httpStatus,

    -- ** GetMaintenanceWindowExecution
    getMaintenanceWindowExecution_windowExecutionId,
    getMaintenanceWindowExecutionResponse_windowExecutionId,
    getMaintenanceWindowExecutionResponse_statusDetails,
    getMaintenanceWindowExecutionResponse_status,
    getMaintenanceWindowExecutionResponse_endTime,
    getMaintenanceWindowExecutionResponse_taskIds,
    getMaintenanceWindowExecutionResponse_startTime,
    getMaintenanceWindowExecutionResponse_httpStatus,

    -- ** GetMaintenanceWindowExecutionTask
    getMaintenanceWindowExecutionTask_windowExecutionId,
    getMaintenanceWindowExecutionTask_taskId,
    getMaintenanceWindowExecutionTaskResponse_type,
    getMaintenanceWindowExecutionTaskResponse_windowExecutionId,
    getMaintenanceWindowExecutionTaskResponse_taskParameters,
    getMaintenanceWindowExecutionTaskResponse_taskArn,
    getMaintenanceWindowExecutionTaskResponse_statusDetails,
    getMaintenanceWindowExecutionTaskResponse_status,
    getMaintenanceWindowExecutionTaskResponse_endTime,
    getMaintenanceWindowExecutionTaskResponse_serviceRole,
    getMaintenanceWindowExecutionTaskResponse_alarmConfiguration,
    getMaintenanceWindowExecutionTaskResponse_priority,
    getMaintenanceWindowExecutionTaskResponse_maxConcurrency,
    getMaintenanceWindowExecutionTaskResponse_maxErrors,
    getMaintenanceWindowExecutionTaskResponse_triggeredAlarms,
    getMaintenanceWindowExecutionTaskResponse_startTime,
    getMaintenanceWindowExecutionTaskResponse_taskExecutionId,
    getMaintenanceWindowExecutionTaskResponse_httpStatus,

    -- ** GetMaintenanceWindowExecutionTaskInvocation
    getMaintenanceWindowExecutionTaskInvocation_windowExecutionId,
    getMaintenanceWindowExecutionTaskInvocation_taskId,
    getMaintenanceWindowExecutionTaskInvocation_invocationId,
    getMaintenanceWindowExecutionTaskInvocationResponse_windowTargetId,
    getMaintenanceWindowExecutionTaskInvocationResponse_windowExecutionId,
    getMaintenanceWindowExecutionTaskInvocationResponse_invocationId,
    getMaintenanceWindowExecutionTaskInvocationResponse_statusDetails,
    getMaintenanceWindowExecutionTaskInvocationResponse_status,
    getMaintenanceWindowExecutionTaskInvocationResponse_endTime,
    getMaintenanceWindowExecutionTaskInvocationResponse_taskType,
    getMaintenanceWindowExecutionTaskInvocationResponse_executionId,
    getMaintenanceWindowExecutionTaskInvocationResponse_ownerInformation,
    getMaintenanceWindowExecutionTaskInvocationResponse_startTime,
    getMaintenanceWindowExecutionTaskInvocationResponse_taskExecutionId,
    getMaintenanceWindowExecutionTaskInvocationResponse_parameters,
    getMaintenanceWindowExecutionTaskInvocationResponse_httpStatus,

    -- ** GetMaintenanceWindowTask
    getMaintenanceWindowTask_windowId,
    getMaintenanceWindowTask_windowTaskId,
    getMaintenanceWindowTaskResponse_name,
    getMaintenanceWindowTaskResponse_taskParameters,
    getMaintenanceWindowTaskResponse_serviceRoleArn,
    getMaintenanceWindowTaskResponse_taskInvocationParameters,
    getMaintenanceWindowTaskResponse_windowTaskId,
    getMaintenanceWindowTaskResponse_taskArn,
    getMaintenanceWindowTaskResponse_windowId,
    getMaintenanceWindowTaskResponse_targets,
    getMaintenanceWindowTaskResponse_description,
    getMaintenanceWindowTaskResponse_taskType,
    getMaintenanceWindowTaskResponse_alarmConfiguration,
    getMaintenanceWindowTaskResponse_priority,
    getMaintenanceWindowTaskResponse_maxConcurrency,
    getMaintenanceWindowTaskResponse_maxErrors,
    getMaintenanceWindowTaskResponse_loggingInfo,
    getMaintenanceWindowTaskResponse_cutoffBehavior,
    getMaintenanceWindowTaskResponse_httpStatus,

    -- ** GetOpsItem
    getOpsItem_opsItemArn,
    getOpsItem_opsItemId,
    getOpsItemResponse_opsItem,
    getOpsItemResponse_httpStatus,

    -- ** GetOpsMetadata
    getOpsMetadata_nextToken,
    getOpsMetadata_maxResults,
    getOpsMetadata_opsMetadataArn,
    getOpsMetadataResponse_resourceId,
    getOpsMetadataResponse_nextToken,
    getOpsMetadataResponse_metadata,
    getOpsMetadataResponse_httpStatus,

    -- ** GetOpsSummary
    getOpsSummary_resultAttributes,
    getOpsSummary_nextToken,
    getOpsSummary_syncName,
    getOpsSummary_filters,
    getOpsSummary_aggregators,
    getOpsSummary_maxResults,
    getOpsSummaryResponse_entities,
    getOpsSummaryResponse_nextToken,
    getOpsSummaryResponse_httpStatus,

    -- ** GetParameter
    getParameter_withDecryption,
    getParameter_name,
    getParameterResponse_httpStatus,
    getParameterResponse_parameter,

    -- ** GetParameterHistory
    getParameterHistory_nextToken,
    getParameterHistory_maxResults,
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
    getParametersByPath_nextToken,
    getParametersByPath_parameterFilters,
    getParametersByPath_recursive,
    getParametersByPath_maxResults,
    getParametersByPath_withDecryption,
    getParametersByPath_path,
    getParametersByPathResponse_nextToken,
    getParametersByPathResponse_parameters,
    getParametersByPathResponse_httpStatus,

    -- ** GetPatchBaseline
    getPatchBaseline_baselineId,
    getPatchBaselineResponse_operatingSystem,
    getPatchBaselineResponse_approvedPatches,
    getPatchBaselineResponse_approvedPatchesComplianceLevel,
    getPatchBaselineResponse_sources,
    getPatchBaselineResponse_name,
    getPatchBaselineResponse_baselineId,
    getPatchBaselineResponse_approvalRules,
    getPatchBaselineResponse_rejectedPatchesAction,
    getPatchBaselineResponse_description,
    getPatchBaselineResponse_globalFilters,
    getPatchBaselineResponse_createdDate,
    getPatchBaselineResponse_rejectedPatches,
    getPatchBaselineResponse_approvedPatchesEnableNonSecurity,
    getPatchBaselineResponse_patchGroups,
    getPatchBaselineResponse_modifiedDate,
    getPatchBaselineResponse_httpStatus,

    -- ** GetPatchBaselineForPatchGroup
    getPatchBaselineForPatchGroup_operatingSystem,
    getPatchBaselineForPatchGroup_patchGroup,
    getPatchBaselineForPatchGroupResponse_operatingSystem,
    getPatchBaselineForPatchGroupResponse_baselineId,
    getPatchBaselineForPatchGroupResponse_patchGroup,
    getPatchBaselineForPatchGroupResponse_httpStatus,

    -- ** GetResourcePolicies
    getResourcePolicies_nextToken,
    getResourcePolicies_maxResults,
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
    listAssociationVersions_nextToken,
    listAssociationVersions_maxResults,
    listAssociationVersions_associationId,
    listAssociationVersionsResponse_nextToken,
    listAssociationVersionsResponse_associationVersions,
    listAssociationVersionsResponse_httpStatus,

    -- ** ListAssociations
    listAssociations_nextToken,
    listAssociations_associationFilterList,
    listAssociations_maxResults,
    listAssociationsResponse_nextToken,
    listAssociationsResponse_associations,
    listAssociationsResponse_httpStatus,

    -- ** ListCommandInvocations
    listCommandInvocations_nextToken,
    listCommandInvocations_filters,
    listCommandInvocations_details,
    listCommandInvocations_commandId,
    listCommandInvocations_instanceId,
    listCommandInvocations_maxResults,
    listCommandInvocationsResponse_nextToken,
    listCommandInvocationsResponse_commandInvocations,
    listCommandInvocationsResponse_httpStatus,

    -- ** ListCommands
    listCommands_nextToken,
    listCommands_filters,
    listCommands_commandId,
    listCommands_instanceId,
    listCommands_maxResults,
    listCommandsResponse_nextToken,
    listCommandsResponse_commands,
    listCommandsResponse_httpStatus,

    -- ** ListComplianceItems
    listComplianceItems_nextToken,
    listComplianceItems_filters,
    listComplianceItems_resourceTypes,
    listComplianceItems_resourceIds,
    listComplianceItems_maxResults,
    listComplianceItemsResponse_complianceItems,
    listComplianceItemsResponse_nextToken,
    listComplianceItemsResponse_httpStatus,

    -- ** ListComplianceSummaries
    listComplianceSummaries_nextToken,
    listComplianceSummaries_filters,
    listComplianceSummaries_maxResults,
    listComplianceSummariesResponse_nextToken,
    listComplianceSummariesResponse_complianceSummaryItems,
    listComplianceSummariesResponse_httpStatus,

    -- ** ListDocumentMetadataHistory
    listDocumentMetadataHistory_nextToken,
    listDocumentMetadataHistory_maxResults,
    listDocumentMetadataHistory_documentVersion,
    listDocumentMetadataHistory_name,
    listDocumentMetadataHistory_metadata,
    listDocumentMetadataHistoryResponse_author,
    listDocumentMetadataHistoryResponse_name,
    listDocumentMetadataHistoryResponse_nextToken,
    listDocumentMetadataHistoryResponse_metadata,
    listDocumentMetadataHistoryResponse_documentVersion,
    listDocumentMetadataHistoryResponse_httpStatus,

    -- ** ListDocumentVersions
    listDocumentVersions_nextToken,
    listDocumentVersions_maxResults,
    listDocumentVersions_name,
    listDocumentVersionsResponse_nextToken,
    listDocumentVersionsResponse_documentVersions,
    listDocumentVersionsResponse_httpStatus,

    -- ** ListDocuments
    listDocuments_nextToken,
    listDocuments_filters,
    listDocuments_maxResults,
    listDocuments_documentFilterList,
    listDocumentsResponse_nextToken,
    listDocumentsResponse_documentIdentifiers,
    listDocumentsResponse_httpStatus,

    -- ** ListInventoryEntries
    listInventoryEntries_nextToken,
    listInventoryEntries_filters,
    listInventoryEntries_maxResults,
    listInventoryEntries_instanceId,
    listInventoryEntries_typeName,
    listInventoryEntriesResponse_nextToken,
    listInventoryEntriesResponse_typeName,
    listInventoryEntriesResponse_instanceId,
    listInventoryEntriesResponse_entries,
    listInventoryEntriesResponse_captureTime,
    listInventoryEntriesResponse_schemaVersion,
    listInventoryEntriesResponse_httpStatus,

    -- ** ListOpsItemEvents
    listOpsItemEvents_nextToken,
    listOpsItemEvents_filters,
    listOpsItemEvents_maxResults,
    listOpsItemEventsResponse_nextToken,
    listOpsItemEventsResponse_summaries,
    listOpsItemEventsResponse_httpStatus,

    -- ** ListOpsItemRelatedItems
    listOpsItemRelatedItems_nextToken,
    listOpsItemRelatedItems_filters,
    listOpsItemRelatedItems_opsItemId,
    listOpsItemRelatedItems_maxResults,
    listOpsItemRelatedItemsResponse_nextToken,
    listOpsItemRelatedItemsResponse_summaries,
    listOpsItemRelatedItemsResponse_httpStatus,

    -- ** ListOpsMetadata
    listOpsMetadata_nextToken,
    listOpsMetadata_filters,
    listOpsMetadata_maxResults,
    listOpsMetadataResponse_nextToken,
    listOpsMetadataResponse_opsMetadataList,
    listOpsMetadataResponse_httpStatus,

    -- ** ListResourceComplianceSummaries
    listResourceComplianceSummaries_nextToken,
    listResourceComplianceSummaries_filters,
    listResourceComplianceSummaries_maxResults,
    listResourceComplianceSummariesResponse_nextToken,
    listResourceComplianceSummariesResponse_resourceComplianceSummaryItems,
    listResourceComplianceSummariesResponse_httpStatus,

    -- ** ListResourceDataSync
    listResourceDataSync_nextToken,
    listResourceDataSync_syncType,
    listResourceDataSync_maxResults,
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
    modifyDocumentPermission_sharedDocumentVersion,
    modifyDocumentPermission_accountIdsToRemove,
    modifyDocumentPermission_name,
    modifyDocumentPermission_permissionType,
    modifyDocumentPermissionResponse_httpStatus,

    -- ** PutComplianceItems
    putComplianceItems_uploadType,
    putComplianceItems_itemContentHash,
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
    putParameter_tags,
    putParameter_type,
    putParameter_allowedPattern,
    putParameter_description,
    putParameter_tier,
    putParameter_policies,
    putParameter_overwrite,
    putParameter_keyId,
    putParameter_dataType,
    putParameter_name,
    putParameter_value,
    putParameterResponse_tier,
    putParameterResponse_httpStatus,
    putParameterResponse_version,

    -- ** PutResourcePolicy
    putResourcePolicy_policyId,
    putResourcePolicy_policyHash,
    putResourcePolicy_resourceArn,
    putResourcePolicy_policy,
    putResourcePolicyResponse_policyId,
    putResourcePolicyResponse_policyHash,
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
    registerTargetWithMaintenanceWindow_name,
    registerTargetWithMaintenanceWindow_description,
    registerTargetWithMaintenanceWindow_ownerInformation,
    registerTargetWithMaintenanceWindow_windowId,
    registerTargetWithMaintenanceWindow_resourceType,
    registerTargetWithMaintenanceWindow_targets,
    registerTargetWithMaintenanceWindowResponse_windowTargetId,
    registerTargetWithMaintenanceWindowResponse_httpStatus,

    -- ** RegisterTaskWithMaintenanceWindow
    registerTaskWithMaintenanceWindow_clientToken,
    registerTaskWithMaintenanceWindow_name,
    registerTaskWithMaintenanceWindow_taskParameters,
    registerTaskWithMaintenanceWindow_serviceRoleArn,
    registerTaskWithMaintenanceWindow_taskInvocationParameters,
    registerTaskWithMaintenanceWindow_targets,
    registerTaskWithMaintenanceWindow_description,
    registerTaskWithMaintenanceWindow_alarmConfiguration,
    registerTaskWithMaintenanceWindow_priority,
    registerTaskWithMaintenanceWindow_maxConcurrency,
    registerTaskWithMaintenanceWindow_maxErrors,
    registerTaskWithMaintenanceWindow_loggingInfo,
    registerTaskWithMaintenanceWindow_cutoffBehavior,
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
    resumeSessionResponse_tokenValue,
    resumeSessionResponse_streamUrl,
    resumeSessionResponse_sessionId,
    resumeSessionResponse_httpStatus,

    -- ** SendAutomationSignal
    sendAutomationSignal_payload,
    sendAutomationSignal_automationExecutionId,
    sendAutomationSignal_signalType,
    sendAutomationSignalResponse_httpStatus,

    -- ** SendCommand
    sendCommand_serviceRoleArn,
    sendCommand_timeoutSeconds,
    sendCommand_cloudWatchOutputConfig,
    sendCommand_outputS3Region,
    sendCommand_targets,
    sendCommand_comment,
    sendCommand_alarmConfiguration,
    sendCommand_maxConcurrency,
    sendCommand_maxErrors,
    sendCommand_notificationConfig,
    sendCommand_documentHashType,
    sendCommand_instanceIds,
    sendCommand_outputS3BucketName,
    sendCommand_documentHash,
    sendCommand_outputS3KeyPrefix,
    sendCommand_documentVersion,
    sendCommand_parameters,
    sendCommand_documentName,
    sendCommandResponse_command,
    sendCommandResponse_httpStatus,

    -- ** StartAssociationsOnce
    startAssociationsOnce_associationIds,
    startAssociationsOnceResponse_httpStatus,

    -- ** StartAutomationExecution
    startAutomationExecution_tags,
    startAutomationExecution_clientToken,
    startAutomationExecution_targetLocations,
    startAutomationExecution_targetParameterName,
    startAutomationExecution_targetMaps,
    startAutomationExecution_targets,
    startAutomationExecution_alarmConfiguration,
    startAutomationExecution_maxConcurrency,
    startAutomationExecution_mode,
    startAutomationExecution_maxErrors,
    startAutomationExecution_documentVersion,
    startAutomationExecution_parameters,
    startAutomationExecution_documentName,
    startAutomationExecutionResponse_automationExecutionId,
    startAutomationExecutionResponse_httpStatus,

    -- ** StartChangeRequestExecution
    startChangeRequestExecution_tags,
    startChangeRequestExecution_clientToken,
    startChangeRequestExecution_changeDetails,
    startChangeRequestExecution_autoApprove,
    startChangeRequestExecution_changeRequestName,
    startChangeRequestExecution_scheduledEndTime,
    startChangeRequestExecution_scheduledTime,
    startChangeRequestExecution_documentVersion,
    startChangeRequestExecution_parameters,
    startChangeRequestExecution_documentName,
    startChangeRequestExecution_runbooks,
    startChangeRequestExecutionResponse_automationExecutionId,
    startChangeRequestExecutionResponse_httpStatus,

    -- ** StartSession
    startSession_documentName,
    startSession_reason,
    startSession_parameters,
    startSession_target,
    startSessionResponse_tokenValue,
    startSessionResponse_streamUrl,
    startSessionResponse_sessionId,
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
    updateAssociation_associationName,
    updateAssociation_name,
    updateAssociation_associationVersion,
    updateAssociation_targetLocations,
    updateAssociation_automationTargetParameterName,
    updateAssociation_targetMaps,
    updateAssociation_outputLocation,
    updateAssociation_targets,
    updateAssociation_calendarNames,
    updateAssociation_scheduleExpression,
    updateAssociation_scheduleOffset,
    updateAssociation_alarmConfiguration,
    updateAssociation_maxConcurrency,
    updateAssociation_applyOnlyAtCronInterval,
    updateAssociation_maxErrors,
    updateAssociation_complianceSeverity,
    updateAssociation_syncCompliance,
    updateAssociation_documentVersion,
    updateAssociation_parameters,
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
    updateDocument_displayName,
    updateDocument_targetType,
    updateDocument_attachments,
    updateDocument_versionName,
    updateDocument_documentFormat,
    updateDocument_documentVersion,
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
    updateMaintenanceWindow_schedule,
    updateMaintenanceWindow_cutoff,
    updateMaintenanceWindow_name,
    updateMaintenanceWindow_endDate,
    updateMaintenanceWindow_description,
    updateMaintenanceWindow_enabled,
    updateMaintenanceWindow_duration,
    updateMaintenanceWindow_scheduleTimezone,
    updateMaintenanceWindow_scheduleOffset,
    updateMaintenanceWindow_startDate,
    updateMaintenanceWindow_replace,
    updateMaintenanceWindow_allowUnassociatedTargets,
    updateMaintenanceWindow_windowId,
    updateMaintenanceWindowResponse_schedule,
    updateMaintenanceWindowResponse_cutoff,
    updateMaintenanceWindowResponse_name,
    updateMaintenanceWindowResponse_endDate,
    updateMaintenanceWindowResponse_windowId,
    updateMaintenanceWindowResponse_description,
    updateMaintenanceWindowResponse_enabled,
    updateMaintenanceWindowResponse_duration,
    updateMaintenanceWindowResponse_scheduleTimezone,
    updateMaintenanceWindowResponse_scheduleOffset,
    updateMaintenanceWindowResponse_startDate,
    updateMaintenanceWindowResponse_allowUnassociatedTargets,
    updateMaintenanceWindowResponse_httpStatus,

    -- ** UpdateMaintenanceWindowTarget
    updateMaintenanceWindowTarget_name,
    updateMaintenanceWindowTarget_targets,
    updateMaintenanceWindowTarget_description,
    updateMaintenanceWindowTarget_ownerInformation,
    updateMaintenanceWindowTarget_replace,
    updateMaintenanceWindowTarget_windowId,
    updateMaintenanceWindowTarget_windowTargetId,
    updateMaintenanceWindowTargetResponse_windowTargetId,
    updateMaintenanceWindowTargetResponse_name,
    updateMaintenanceWindowTargetResponse_windowId,
    updateMaintenanceWindowTargetResponse_targets,
    updateMaintenanceWindowTargetResponse_description,
    updateMaintenanceWindowTargetResponse_ownerInformation,
    updateMaintenanceWindowTargetResponse_httpStatus,

    -- ** UpdateMaintenanceWindowTask
    updateMaintenanceWindowTask_name,
    updateMaintenanceWindowTask_taskParameters,
    updateMaintenanceWindowTask_serviceRoleArn,
    updateMaintenanceWindowTask_taskInvocationParameters,
    updateMaintenanceWindowTask_taskArn,
    updateMaintenanceWindowTask_targets,
    updateMaintenanceWindowTask_description,
    updateMaintenanceWindowTask_alarmConfiguration,
    updateMaintenanceWindowTask_priority,
    updateMaintenanceWindowTask_maxConcurrency,
    updateMaintenanceWindowTask_maxErrors,
    updateMaintenanceWindowTask_loggingInfo,
    updateMaintenanceWindowTask_replace,
    updateMaintenanceWindowTask_cutoffBehavior,
    updateMaintenanceWindowTask_windowId,
    updateMaintenanceWindowTask_windowTaskId,
    updateMaintenanceWindowTaskResponse_name,
    updateMaintenanceWindowTaskResponse_taskParameters,
    updateMaintenanceWindowTaskResponse_serviceRoleArn,
    updateMaintenanceWindowTaskResponse_taskInvocationParameters,
    updateMaintenanceWindowTaskResponse_windowTaskId,
    updateMaintenanceWindowTaskResponse_taskArn,
    updateMaintenanceWindowTaskResponse_windowId,
    updateMaintenanceWindowTaskResponse_targets,
    updateMaintenanceWindowTaskResponse_description,
    updateMaintenanceWindowTaskResponse_alarmConfiguration,
    updateMaintenanceWindowTaskResponse_priority,
    updateMaintenanceWindowTaskResponse_maxConcurrency,
    updateMaintenanceWindowTaskResponse_maxErrors,
    updateMaintenanceWindowTaskResponse_loggingInfo,
    updateMaintenanceWindowTaskResponse_cutoffBehavior,
    updateMaintenanceWindowTaskResponse_httpStatus,

    -- ** UpdateManagedInstanceRole
    updateManagedInstanceRole_instanceId,
    updateManagedInstanceRole_iamRole,
    updateManagedInstanceRoleResponse_httpStatus,

    -- ** UpdateOpsItem
    updateOpsItem_notifications,
    updateOpsItem_opsItemArn,
    updateOpsItem_severity,
    updateOpsItem_plannedStartTime,
    updateOpsItem_operationalDataToDelete,
    updateOpsItem_plannedEndTime,
    updateOpsItem_status,
    updateOpsItem_description,
    updateOpsItem_title,
    updateOpsItem_priority,
    updateOpsItem_category,
    updateOpsItem_operationalData,
    updateOpsItem_actualStartTime,
    updateOpsItem_actualEndTime,
    updateOpsItem_relatedOpsItems,
    updateOpsItem_opsItemId,
    updateOpsItemResponse_httpStatus,

    -- ** UpdateOpsMetadata
    updateOpsMetadata_keysToDelete,
    updateOpsMetadata_metadataToUpdate,
    updateOpsMetadata_opsMetadataArn,
    updateOpsMetadataResponse_opsMetadataArn,
    updateOpsMetadataResponse_httpStatus,

    -- ** UpdatePatchBaseline
    updatePatchBaseline_approvedPatches,
    updatePatchBaseline_approvedPatchesComplianceLevel,
    updatePatchBaseline_sources,
    updatePatchBaseline_name,
    updatePatchBaseline_approvalRules,
    updatePatchBaseline_rejectedPatchesAction,
    updatePatchBaseline_description,
    updatePatchBaseline_globalFilters,
    updatePatchBaseline_rejectedPatches,
    updatePatchBaseline_approvedPatchesEnableNonSecurity,
    updatePatchBaseline_replace,
    updatePatchBaseline_baselineId,
    updatePatchBaselineResponse_operatingSystem,
    updatePatchBaselineResponse_approvedPatches,
    updatePatchBaselineResponse_approvedPatchesComplianceLevel,
    updatePatchBaselineResponse_sources,
    updatePatchBaselineResponse_name,
    updatePatchBaselineResponse_baselineId,
    updatePatchBaselineResponse_approvalRules,
    updatePatchBaselineResponse_rejectedPatchesAction,
    updatePatchBaselineResponse_description,
    updatePatchBaselineResponse_globalFilters,
    updatePatchBaselineResponse_createdDate,
    updatePatchBaselineResponse_rejectedPatches,
    updatePatchBaselineResponse_approvedPatchesEnableNonSecurity,
    updatePatchBaselineResponse_modifiedDate,
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
    activation_tags,
    activation_iamRole,
    activation_defaultInstanceName,
    activation_expired,
    activation_description,
    activation_activationId,
    activation_registrationsCount,
    activation_createdDate,
    activation_registrationLimit,
    activation_expirationDate,

    -- ** Alarm
    alarm_name,

    -- ** AlarmConfiguration
    alarmConfiguration_ignorePollAlarmFailure,
    alarmConfiguration_alarms,

    -- ** AlarmStateInformation
    alarmStateInformation_name,
    alarmStateInformation_state,

    -- ** Association
    association_associationName,
    association_name,
    association_associationVersion,
    association_targetMaps,
    association_targets,
    association_scheduleExpression,
    association_scheduleOffset,
    association_instanceId,
    association_overview,
    association_lastExecutionDate,
    association_associationId,
    association_documentVersion,

    -- ** AssociationDescription
    associationDescription_associationName,
    associationDescription_name,
    associationDescription_associationVersion,
    associationDescription_lastSuccessfulExecutionDate,
    associationDescription_targetLocations,
    associationDescription_date,
    associationDescription_automationTargetParameterName,
    associationDescription_targetMaps,
    associationDescription_outputLocation,
    associationDescription_status,
    associationDescription_targets,
    associationDescription_calendarNames,
    associationDescription_scheduleExpression,
    associationDescription_scheduleOffset,
    associationDescription_instanceId,
    associationDescription_overview,
    associationDescription_alarmConfiguration,
    associationDescription_lastUpdateAssociationDate,
    associationDescription_maxConcurrency,
    associationDescription_applyOnlyAtCronInterval,
    associationDescription_maxErrors,
    associationDescription_lastExecutionDate,
    associationDescription_triggeredAlarms,
    associationDescription_complianceSeverity,
    associationDescription_syncCompliance,
    associationDescription_associationId,
    associationDescription_documentVersion,
    associationDescription_parameters,

    -- ** AssociationExecution
    associationExecution_createdTime,
    associationExecution_associationVersion,
    associationExecution_status,
    associationExecution_resourceCountByStatus,
    associationExecution_executionId,
    associationExecution_alarmConfiguration,
    associationExecution_detailedStatus,
    associationExecution_lastExecutionDate,
    associationExecution_triggeredAlarms,
    associationExecution_associationId,

    -- ** AssociationExecutionFilter
    associationExecutionFilter_key,
    associationExecutionFilter_value,
    associationExecutionFilter_type,

    -- ** AssociationExecutionTarget
    associationExecutionTarget_resourceId,
    associationExecutionTarget_resourceType,
    associationExecutionTarget_associationVersion,
    associationExecutionTarget_status,
    associationExecutionTarget_executionId,
    associationExecutionTarget_outputSource,
    associationExecutionTarget_detailedStatus,
    associationExecutionTarget_lastExecutionDate,
    associationExecutionTarget_associationId,

    -- ** AssociationExecutionTargetsFilter
    associationExecutionTargetsFilter_key,
    associationExecutionTargetsFilter_value,

    -- ** AssociationFilter
    associationFilter_key,
    associationFilter_value,

    -- ** AssociationOverview
    associationOverview_associationStatusAggregatedCount,
    associationOverview_status,
    associationOverview_detailedStatus,

    -- ** AssociationStatus
    associationStatus_additionalInfo,
    associationStatus_date,
    associationStatus_name,
    associationStatus_message,

    -- ** AssociationVersionInfo
    associationVersionInfo_associationName,
    associationVersionInfo_name,
    associationVersionInfo_associationVersion,
    associationVersionInfo_targetLocations,
    associationVersionInfo_targetMaps,
    associationVersionInfo_outputLocation,
    associationVersionInfo_targets,
    associationVersionInfo_calendarNames,
    associationVersionInfo_scheduleExpression,
    associationVersionInfo_scheduleOffset,
    associationVersionInfo_maxConcurrency,
    associationVersionInfo_applyOnlyAtCronInterval,
    associationVersionInfo_maxErrors,
    associationVersionInfo_createdDate,
    associationVersionInfo_complianceSeverity,
    associationVersionInfo_syncCompliance,
    associationVersionInfo_associationId,
    associationVersionInfo_documentVersion,
    associationVersionInfo_parameters,

    -- ** AttachmentContent
    attachmentContent_name,
    attachmentContent_hash,
    attachmentContent_size,
    attachmentContent_url,
    attachmentContent_hashType,

    -- ** AttachmentInformation
    attachmentInformation_name,

    -- ** AttachmentsSource
    attachmentsSource_key,
    attachmentsSource_name,
    attachmentsSource_values,

    -- ** AutomationExecution
    automationExecution_targetLocations,
    automationExecution_resolvedTargets,
    automationExecution_stepExecutions,
    automationExecution_targetParameterName,
    automationExecution_opsItemId,
    automationExecution_targetMaps,
    automationExecution_target,
    automationExecution_targets,
    automationExecution_executionStartTime,
    automationExecution_failureMessage,
    automationExecution_automationExecutionId,
    automationExecution_documentName,
    automationExecution_automationSubtype,
    automationExecution_outputs,
    automationExecution_alarmConfiguration,
    automationExecution_currentStepName,
    automationExecution_executedBy,
    automationExecution_maxConcurrency,
    automationExecution_mode,
    automationExecution_changeRequestName,
    automationExecution_maxErrors,
    automationExecution_parentAutomationExecutionId,
    automationExecution_automationExecutionStatus,
    automationExecution_runbooks,
    automationExecution_triggeredAlarms,
    automationExecution_currentAction,
    automationExecution_stepExecutionsTruncated,
    automationExecution_progressCounters,
    automationExecution_associationId,
    automationExecution_scheduledTime,
    automationExecution_executionEndTime,
    automationExecution_documentVersion,
    automationExecution_parameters,

    -- ** AutomationExecutionFilter
    automationExecutionFilter_key,
    automationExecutionFilter_values,

    -- ** AutomationExecutionMetadata
    automationExecutionMetadata_resolvedTargets,
    automationExecutionMetadata_targetParameterName,
    automationExecutionMetadata_opsItemId,
    automationExecutionMetadata_targetMaps,
    automationExecutionMetadata_automationType,
    automationExecutionMetadata_target,
    automationExecutionMetadata_targets,
    automationExecutionMetadata_executionStartTime,
    automationExecutionMetadata_failureMessage,
    automationExecutionMetadata_automationExecutionId,
    automationExecutionMetadata_documentName,
    automationExecutionMetadata_automationSubtype,
    automationExecutionMetadata_outputs,
    automationExecutionMetadata_alarmConfiguration,
    automationExecutionMetadata_currentStepName,
    automationExecutionMetadata_executedBy,
    automationExecutionMetadata_maxConcurrency,
    automationExecutionMetadata_mode,
    automationExecutionMetadata_logFile,
    automationExecutionMetadata_changeRequestName,
    automationExecutionMetadata_maxErrors,
    automationExecutionMetadata_parentAutomationExecutionId,
    automationExecutionMetadata_automationExecutionStatus,
    automationExecutionMetadata_runbooks,
    automationExecutionMetadata_triggeredAlarms,
    automationExecutionMetadata_currentAction,
    automationExecutionMetadata_associationId,
    automationExecutionMetadata_scheduledTime,
    automationExecutionMetadata_executionEndTime,
    automationExecutionMetadata_documentVersion,

    -- ** BaselineOverride
    baselineOverride_operatingSystem,
    baselineOverride_approvedPatches,
    baselineOverride_approvedPatchesComplianceLevel,
    baselineOverride_sources,
    baselineOverride_approvalRules,
    baselineOverride_rejectedPatchesAction,
    baselineOverride_globalFilters,
    baselineOverride_rejectedPatches,
    baselineOverride_approvedPatchesEnableNonSecurity,

    -- ** CloudWatchOutputConfig
    cloudWatchOutputConfig_cloudWatchOutputEnabled,
    cloudWatchOutputConfig_cloudWatchLogGroupName,

    -- ** Command
    command_targetCount,
    command_errorCount,
    command_expiresAfter,
    command_statusDetails,
    command_timeoutSeconds,
    command_requestedDateTime,
    command_cloudWatchOutputConfig,
    command_outputS3Region,
    command_status,
    command_targets,
    command_serviceRole,
    command_commandId,
    command_comment,
    command_documentName,
    command_alarmConfiguration,
    command_maxConcurrency,
    command_completedCount,
    command_maxErrors,
    command_notificationConfig,
    command_instanceIds,
    command_deliveryTimedOutCount,
    command_outputS3BucketName,
    command_triggeredAlarms,
    command_outputS3KeyPrefix,
    command_documentVersion,
    command_parameters,

    -- ** CommandFilter
    commandFilter_key,
    commandFilter_value,

    -- ** CommandInvocation
    commandInvocation_commandPlugins,
    commandInvocation_instanceName,
    commandInvocation_statusDetails,
    commandInvocation_requestedDateTime,
    commandInvocation_cloudWatchOutputConfig,
    commandInvocation_status,
    commandInvocation_traceOutput,
    commandInvocation_standardErrorUrl,
    commandInvocation_serviceRole,
    commandInvocation_commandId,
    commandInvocation_comment,
    commandInvocation_instanceId,
    commandInvocation_documentName,
    commandInvocation_notificationConfig,
    commandInvocation_documentVersion,
    commandInvocation_standardOutputUrl,

    -- ** CommandPlugin
    commandPlugin_name,
    commandPlugin_statusDetails,
    commandPlugin_responseStartDateTime,
    commandPlugin_outputS3Region,
    commandPlugin_status,
    commandPlugin_responseFinishDateTime,
    commandPlugin_standardErrorUrl,
    commandPlugin_output,
    commandPlugin_outputS3BucketName,
    commandPlugin_responseCode,
    commandPlugin_outputS3KeyPrefix,
    commandPlugin_standardOutputUrl,

    -- ** ComplianceExecutionSummary
    complianceExecutionSummary_executionType,
    complianceExecutionSummary_executionId,
    complianceExecutionSummary_executionTime,

    -- ** ComplianceItem
    complianceItem_resourceId,
    complianceItem_resourceType,
    complianceItem_severity,
    complianceItem_status,
    complianceItem_id,
    complianceItem_details,
    complianceItem_executionSummary,
    complianceItem_title,
    complianceItem_complianceType,

    -- ** ComplianceItemEntry
    complianceItemEntry_id,
    complianceItemEntry_details,
    complianceItemEntry_title,
    complianceItemEntry_severity,
    complianceItemEntry_status,

    -- ** ComplianceStringFilter
    complianceStringFilter_key,
    complianceStringFilter_type,
    complianceStringFilter_values,

    -- ** ComplianceSummaryItem
    complianceSummaryItem_compliantSummary,
    complianceSummaryItem_nonCompliantSummary,
    complianceSummaryItem_complianceType,

    -- ** CompliantSummary
    compliantSummary_compliantCount,
    compliantSummary_severitySummary,

    -- ** CreateAssociationBatchRequestEntry
    createAssociationBatchRequestEntry_associationName,
    createAssociationBatchRequestEntry_targetLocations,
    createAssociationBatchRequestEntry_automationTargetParameterName,
    createAssociationBatchRequestEntry_targetMaps,
    createAssociationBatchRequestEntry_outputLocation,
    createAssociationBatchRequestEntry_targets,
    createAssociationBatchRequestEntry_calendarNames,
    createAssociationBatchRequestEntry_scheduleExpression,
    createAssociationBatchRequestEntry_scheduleOffset,
    createAssociationBatchRequestEntry_instanceId,
    createAssociationBatchRequestEntry_alarmConfiguration,
    createAssociationBatchRequestEntry_maxConcurrency,
    createAssociationBatchRequestEntry_applyOnlyAtCronInterval,
    createAssociationBatchRequestEntry_maxErrors,
    createAssociationBatchRequestEntry_complianceSeverity,
    createAssociationBatchRequestEntry_syncCompliance,
    createAssociationBatchRequestEntry_documentVersion,
    createAssociationBatchRequestEntry_parameters,
    createAssociationBatchRequestEntry_name,

    -- ** DescribeActivationsFilter
    describeActivationsFilter_filterValues,
    describeActivationsFilter_filterKey,

    -- ** DocumentDefaultVersionDescription
    documentDefaultVersionDescription_name,
    documentDefaultVersionDescription_defaultVersionName,
    documentDefaultVersionDescription_defaultVersion,

    -- ** DocumentDescription
    documentDescription_tags,
    documentDescription_requires,
    documentDescription_author,
    documentDescription_pendingReviewVersion,
    documentDescription_documentType,
    documentDescription_name,
    documentDescription_sha1,
    documentDescription_attachmentsInformation,
    documentDescription_hash,
    documentDescription_displayName,
    documentDescription_latestVersion,
    documentDescription_owner,
    documentDescription_status,
    documentDescription_defaultVersion,
    documentDescription_description,
    documentDescription_targetType,
    documentDescription_versionName,
    documentDescription_reviewInformation,
    documentDescription_platformTypes,
    documentDescription_categoryEnum,
    documentDescription_statusInformation,
    documentDescription_category,
    documentDescription_schemaVersion,
    documentDescription_createdDate,
    documentDescription_approvedVersion,
    documentDescription_reviewStatus,
    documentDescription_hashType,
    documentDescription_documentFormat,
    documentDescription_documentVersion,
    documentDescription_parameters,

    -- ** DocumentFilter
    documentFilter_key,
    documentFilter_value,

    -- ** DocumentIdentifier
    documentIdentifier_tags,
    documentIdentifier_requires,
    documentIdentifier_author,
    documentIdentifier_documentType,
    documentIdentifier_name,
    documentIdentifier_displayName,
    documentIdentifier_owner,
    documentIdentifier_targetType,
    documentIdentifier_versionName,
    documentIdentifier_platformTypes,
    documentIdentifier_schemaVersion,
    documentIdentifier_createdDate,
    documentIdentifier_reviewStatus,
    documentIdentifier_documentFormat,
    documentIdentifier_documentVersion,

    -- ** DocumentKeyValuesFilter
    documentKeyValuesFilter_key,
    documentKeyValuesFilter_values,

    -- ** DocumentMetadataResponseInfo
    documentMetadataResponseInfo_reviewerResponse,

    -- ** DocumentParameter
    documentParameter_name,
    documentParameter_type,
    documentParameter_defaultValue,
    documentParameter_description,

    -- ** DocumentRequires
    documentRequires_version,
    documentRequires_name,

    -- ** DocumentReviewCommentSource
    documentReviewCommentSource_type,
    documentReviewCommentSource_content,

    -- ** DocumentReviewerResponseSource
    documentReviewerResponseSource_reviewer,
    documentReviewerResponseSource_comment,
    documentReviewerResponseSource_createTime,
    documentReviewerResponseSource_updatedTime,
    documentReviewerResponseSource_reviewStatus,

    -- ** DocumentReviews
    documentReviews_comment,
    documentReviews_action,

    -- ** DocumentVersionInfo
    documentVersionInfo_name,
    documentVersionInfo_isDefaultVersion,
    documentVersionInfo_displayName,
    documentVersionInfo_status,
    documentVersionInfo_versionName,
    documentVersionInfo_statusInformation,
    documentVersionInfo_createdDate,
    documentVersionInfo_reviewStatus,
    documentVersionInfo_documentFormat,
    documentVersionInfo_documentVersion,

    -- ** EffectivePatch
    effectivePatch_patch,
    effectivePatch_patchStatus,

    -- ** FailedCreateAssociation
    failedCreateAssociation_message,
    failedCreateAssociation_fault,
    failedCreateAssociation_entry,

    -- ** FailureDetails
    failureDetails_failureType,
    failureDetails_details,
    failureDetails_failureStage,

    -- ** GetResourcePoliciesResponseEntry
    getResourcePoliciesResponseEntry_policyId,
    getResourcePoliciesResponseEntry_policy,
    getResourcePoliciesResponseEntry_policyHash,

    -- ** InstanceAggregatedAssociationOverview
    instanceAggregatedAssociationOverview_detailedStatus,
    instanceAggregatedAssociationOverview_instanceAssociationStatusAggregatedCount,

    -- ** InstanceAssociation
    instanceAssociation_associationVersion,
    instanceAssociation_instanceId,
    instanceAssociation_content,
    instanceAssociation_associationId,

    -- ** InstanceAssociationOutputLocation
    instanceAssociationOutputLocation_s3Location,

    -- ** InstanceAssociationOutputUrl
    instanceAssociationOutputUrl_s3OutputUrl,

    -- ** InstanceAssociationStatusInfo
    instanceAssociationStatusInfo_executionDate,
    instanceAssociationStatusInfo_associationName,
    instanceAssociationStatusInfo_name,
    instanceAssociationStatusInfo_associationVersion,
    instanceAssociationStatusInfo_outputUrl,
    instanceAssociationStatusInfo_status,
    instanceAssociationStatusInfo_executionSummary,
    instanceAssociationStatusInfo_instanceId,
    instanceAssociationStatusInfo_detailedStatus,
    instanceAssociationStatusInfo_errorCode,
    instanceAssociationStatusInfo_associationId,
    instanceAssociationStatusInfo_documentVersion,

    -- ** InstanceInformation
    instanceInformation_resourceType,
    instanceInformation_pingStatus,
    instanceInformation_name,
    instanceInformation_iamRole,
    instanceInformation_sourceId,
    instanceInformation_registrationDate,
    instanceInformation_lastPingDateTime,
    instanceInformation_platformType,
    instanceInformation_platformName,
    instanceInformation_computerName,
    instanceInformation_lastAssociationExecutionDate,
    instanceInformation_associationStatus,
    instanceInformation_sourceType,
    instanceInformation_activationId,
    instanceInformation_isLatestVersion,
    instanceInformation_instanceId,
    instanceInformation_associationOverview,
    instanceInformation_lastSuccessfulAssociationExecutionDate,
    instanceInformation_platformVersion,
    instanceInformation_agentVersion,
    instanceInformation_iPAddress,

    -- ** InstanceInformationFilter
    instanceInformationFilter_key,
    instanceInformationFilter_valueSet,

    -- ** InstanceInformationStringFilter
    instanceInformationStringFilter_key,
    instanceInformationStringFilter_values,

    -- ** InstancePatchState
    instancePatchState_installedOtherCount,
    instancePatchState_rebootOption,
    instancePatchState_lastNoRebootInstallOperationTime,
    instancePatchState_unreportedNotApplicableCount,
    instancePatchState_failedCount,
    instancePatchState_installedCount,
    instancePatchState_snapshotId,
    instancePatchState_installedRejectedCount,
    instancePatchState_installOverrideList,
    instancePatchState_installedPendingRebootCount,
    instancePatchState_notApplicableCount,
    instancePatchState_ownerInformation,
    instancePatchState_missingCount,
    instancePatchState_otherNonCompliantCount,
    instancePatchState_securityNonCompliantCount,
    instancePatchState_criticalNonCompliantCount,
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
    inventoryAggregator_expression,
    inventoryAggregator_aggregators,
    inventoryAggregator_groups,

    -- ** InventoryDeletionStatusItem
    inventoryDeletionStatusItem_deletionStartTime,
    inventoryDeletionStatusItem_lastStatusMessage,
    inventoryDeletionStatusItem_lastStatus,
    inventoryDeletionStatusItem_typeName,
    inventoryDeletionStatusItem_deletionSummary,
    inventoryDeletionStatusItem_lastStatusUpdateTime,
    inventoryDeletionStatusItem_deletionId,

    -- ** InventoryDeletionSummary
    inventoryDeletionSummary_remainingCount,
    inventoryDeletionSummary_summaryItems,
    inventoryDeletionSummary_totalCount,

    -- ** InventoryDeletionSummaryItem
    inventoryDeletionSummaryItem_remainingCount,
    inventoryDeletionSummaryItem_count,
    inventoryDeletionSummaryItem_version,

    -- ** InventoryFilter
    inventoryFilter_type,
    inventoryFilter_key,
    inventoryFilter_values,

    -- ** InventoryGroup
    inventoryGroup_name,
    inventoryGroup_filters,

    -- ** InventoryItem
    inventoryItem_contentHash,
    inventoryItem_context,
    inventoryItem_content,
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
    inventoryResultEntity_id,
    inventoryResultEntity_data,

    -- ** InventoryResultItem
    inventoryResultItem_contentHash,
    inventoryResultItem_captureTime,
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
    maintenanceWindowExecution_windowExecutionId,
    maintenanceWindowExecution_statusDetails,
    maintenanceWindowExecution_windowId,
    maintenanceWindowExecution_status,
    maintenanceWindowExecution_endTime,
    maintenanceWindowExecution_startTime,

    -- ** MaintenanceWindowExecutionTaskIdentity
    maintenanceWindowExecutionTaskIdentity_windowExecutionId,
    maintenanceWindowExecutionTaskIdentity_taskArn,
    maintenanceWindowExecutionTaskIdentity_statusDetails,
    maintenanceWindowExecutionTaskIdentity_status,
    maintenanceWindowExecutionTaskIdentity_endTime,
    maintenanceWindowExecutionTaskIdentity_taskType,
    maintenanceWindowExecutionTaskIdentity_alarmConfiguration,
    maintenanceWindowExecutionTaskIdentity_triggeredAlarms,
    maintenanceWindowExecutionTaskIdentity_startTime,
    maintenanceWindowExecutionTaskIdentity_taskExecutionId,

    -- ** MaintenanceWindowExecutionTaskInvocationIdentity
    maintenanceWindowExecutionTaskInvocationIdentity_windowTargetId,
    maintenanceWindowExecutionTaskInvocationIdentity_windowExecutionId,
    maintenanceWindowExecutionTaskInvocationIdentity_invocationId,
    maintenanceWindowExecutionTaskInvocationIdentity_statusDetails,
    maintenanceWindowExecutionTaskInvocationIdentity_status,
    maintenanceWindowExecutionTaskInvocationIdentity_endTime,
    maintenanceWindowExecutionTaskInvocationIdentity_taskType,
    maintenanceWindowExecutionTaskInvocationIdentity_executionId,
    maintenanceWindowExecutionTaskInvocationIdentity_ownerInformation,
    maintenanceWindowExecutionTaskInvocationIdentity_startTime,
    maintenanceWindowExecutionTaskInvocationIdentity_taskExecutionId,
    maintenanceWindowExecutionTaskInvocationIdentity_parameters,

    -- ** MaintenanceWindowFilter
    maintenanceWindowFilter_key,
    maintenanceWindowFilter_values,

    -- ** MaintenanceWindowIdentity
    maintenanceWindowIdentity_schedule,
    maintenanceWindowIdentity_cutoff,
    maintenanceWindowIdentity_name,
    maintenanceWindowIdentity_endDate,
    maintenanceWindowIdentity_windowId,
    maintenanceWindowIdentity_description,
    maintenanceWindowIdentity_enabled,
    maintenanceWindowIdentity_duration,
    maintenanceWindowIdentity_scheduleTimezone,
    maintenanceWindowIdentity_scheduleOffset,
    maintenanceWindowIdentity_startDate,
    maintenanceWindowIdentity_nextExecutionTime,

    -- ** MaintenanceWindowIdentityForTarget
    maintenanceWindowIdentityForTarget_name,
    maintenanceWindowIdentityForTarget_windowId,

    -- ** MaintenanceWindowLambdaParameters
    maintenanceWindowLambdaParameters_clientContext,
    maintenanceWindowLambdaParameters_payload,
    maintenanceWindowLambdaParameters_qualifier,

    -- ** MaintenanceWindowRunCommandParameters
    maintenanceWindowRunCommandParameters_serviceRoleArn,
    maintenanceWindowRunCommandParameters_timeoutSeconds,
    maintenanceWindowRunCommandParameters_cloudWatchOutputConfig,
    maintenanceWindowRunCommandParameters_comment,
    maintenanceWindowRunCommandParameters_notificationConfig,
    maintenanceWindowRunCommandParameters_documentHashType,
    maintenanceWindowRunCommandParameters_outputS3BucketName,
    maintenanceWindowRunCommandParameters_documentHash,
    maintenanceWindowRunCommandParameters_outputS3KeyPrefix,
    maintenanceWindowRunCommandParameters_documentVersion,
    maintenanceWindowRunCommandParameters_parameters,

    -- ** MaintenanceWindowStepFunctionsParameters
    maintenanceWindowStepFunctionsParameters_name,
    maintenanceWindowStepFunctionsParameters_input,

    -- ** MaintenanceWindowTarget
    maintenanceWindowTarget_windowTargetId,
    maintenanceWindowTarget_resourceType,
    maintenanceWindowTarget_name,
    maintenanceWindowTarget_windowId,
    maintenanceWindowTarget_targets,
    maintenanceWindowTarget_description,
    maintenanceWindowTarget_ownerInformation,

    -- ** MaintenanceWindowTask
    maintenanceWindowTask_name,
    maintenanceWindowTask_type,
    maintenanceWindowTask_taskParameters,
    maintenanceWindowTask_serviceRoleArn,
    maintenanceWindowTask_windowTaskId,
    maintenanceWindowTask_taskArn,
    maintenanceWindowTask_windowId,
    maintenanceWindowTask_targets,
    maintenanceWindowTask_description,
    maintenanceWindowTask_alarmConfiguration,
    maintenanceWindowTask_priority,
    maintenanceWindowTask_maxConcurrency,
    maintenanceWindowTask_maxErrors,
    maintenanceWindowTask_loggingInfo,
    maintenanceWindowTask_cutoffBehavior,

    -- ** MaintenanceWindowTaskInvocationParameters
    maintenanceWindowTaskInvocationParameters_automation,
    maintenanceWindowTaskInvocationParameters_lambda,
    maintenanceWindowTaskInvocationParameters_stepFunctions,
    maintenanceWindowTaskInvocationParameters_runCommand,

    -- ** MaintenanceWindowTaskParameterValueExpression
    maintenanceWindowTaskParameterValueExpression_values,

    -- ** MetadataValue
    metadataValue_value,

    -- ** NonCompliantSummary
    nonCompliantSummary_nonCompliantCount,
    nonCompliantSummary_severitySummary,

    -- ** NotificationConfig
    notificationConfig_notificationType,
    notificationConfig_notificationArn,
    notificationConfig_notificationEvents,

    -- ** OpsAggregator
    opsAggregator_aggregatorType,
    opsAggregator_filters,
    opsAggregator_typeName,
    opsAggregator_aggregators,
    opsAggregator_values,
    opsAggregator_attributeName,

    -- ** OpsEntity
    opsEntity_id,
    opsEntity_data,

    -- ** OpsEntityItem
    opsEntityItem_captureTime,
    opsEntityItem_content,

    -- ** OpsFilter
    opsFilter_type,
    opsFilter_key,
    opsFilter_values,

    -- ** OpsItem
    opsItem_notifications,
    opsItem_opsItemArn,
    opsItem_severity,
    opsItem_createdTime,
    opsItem_plannedStartTime,
    opsItem_plannedEndTime,
    opsItem_opsItemId,
    opsItem_status,
    opsItem_description,
    opsItem_lastModifiedTime,
    opsItem_title,
    opsItem_source,
    opsItem_priority,
    opsItem_opsItemType,
    opsItem_category,
    opsItem_operationalData,
    opsItem_actualStartTime,
    opsItem_lastModifiedBy,
    opsItem_createdBy,
    opsItem_version,
    opsItem_actualEndTime,
    opsItem_relatedOpsItems,

    -- ** OpsItemDataValue
    opsItemDataValue_type,
    opsItemDataValue_value,

    -- ** OpsItemEventFilter
    opsItemEventFilter_key,
    opsItemEventFilter_values,
    opsItemEventFilter_operator,

    -- ** OpsItemEventSummary
    opsItemEventSummary_detailType,
    opsItemEventSummary_createdTime,
    opsItemEventSummary_opsItemId,
    opsItemEventSummary_source,
    opsItemEventSummary_eventId,
    opsItemEventSummary_createdBy,
    opsItemEventSummary_detail,

    -- ** OpsItemFilter
    opsItemFilter_key,
    opsItemFilter_values,
    opsItemFilter_operator,

    -- ** OpsItemIdentity
    opsItemIdentity_arn,

    -- ** OpsItemNotification
    opsItemNotification_arn,

    -- ** OpsItemRelatedItemSummary
    opsItemRelatedItemSummary_resourceType,
    opsItemRelatedItemSummary_createdTime,
    opsItemRelatedItemSummary_associationType,
    opsItemRelatedItemSummary_opsItemId,
    opsItemRelatedItemSummary_resourceUri,
    opsItemRelatedItemSummary_lastModifiedTime,
    opsItemRelatedItemSummary_lastModifiedBy,
    opsItemRelatedItemSummary_createdBy,
    opsItemRelatedItemSummary_associationId,

    -- ** OpsItemRelatedItemsFilter
    opsItemRelatedItemsFilter_key,
    opsItemRelatedItemsFilter_values,
    opsItemRelatedItemsFilter_operator,

    -- ** OpsItemSummary
    opsItemSummary_severity,
    opsItemSummary_createdTime,
    opsItemSummary_plannedStartTime,
    opsItemSummary_plannedEndTime,
    opsItemSummary_opsItemId,
    opsItemSummary_status,
    opsItemSummary_lastModifiedTime,
    opsItemSummary_title,
    opsItemSummary_source,
    opsItemSummary_priority,
    opsItemSummary_opsItemType,
    opsItemSummary_category,
    opsItemSummary_operationalData,
    opsItemSummary_actualStartTime,
    opsItemSummary_lastModifiedBy,
    opsItemSummary_createdBy,
    opsItemSummary_actualEndTime,

    -- ** OpsMetadata
    opsMetadata_resourceId,
    opsMetadata_lastModifiedUser,
    opsMetadata_lastModifiedDate,
    opsMetadata_creationDate,
    opsMetadata_opsMetadataArn,

    -- ** OpsMetadataFilter
    opsMetadataFilter_key,
    opsMetadataFilter_values,

    -- ** OpsResultAttribute
    opsResultAttribute_typeName,

    -- ** OutputSource
    outputSource_outputSourceType,
    outputSource_outputSourceId,

    -- ** Parameter
    parameter_selector,
    parameter_lastModifiedDate,
    parameter_arn,
    parameter_dataType,
    parameter_sourceResult,
    parameter_name,
    parameter_type,
    parameter_value,
    parameter_version,

    -- ** ParameterHistory
    parameterHistory_name,
    parameterHistory_type,
    parameterHistory_lastModifiedUser,
    parameterHistory_lastModifiedDate,
    parameterHistory_allowedPattern,
    parameterHistory_description,
    parameterHistory_tier,
    parameterHistory_policies,
    parameterHistory_labels,
    parameterHistory_keyId,
    parameterHistory_version,
    parameterHistory_dataType,
    parameterHistory_value,

    -- ** ParameterInlinePolicy
    parameterInlinePolicy_policyType,
    parameterInlinePolicy_policyText,
    parameterInlinePolicy_policyStatus,

    -- ** ParameterMetadata
    parameterMetadata_name,
    parameterMetadata_type,
    parameterMetadata_lastModifiedUser,
    parameterMetadata_lastModifiedDate,
    parameterMetadata_allowedPattern,
    parameterMetadata_description,
    parameterMetadata_tier,
    parameterMetadata_policies,
    parameterMetadata_keyId,
    parameterMetadata_version,
    parameterMetadata_dataType,

    -- ** ParameterStringFilter
    parameterStringFilter_option,
    parameterStringFilter_values,
    parameterStringFilter_key,

    -- ** ParametersFilter
    parametersFilter_key,
    parametersFilter_values,

    -- ** Patch
    patch_productFamily,
    patch_product,
    patch_severity,
    patch_name,
    patch_advisoryIds,
    patch_msrcSeverity,
    patch_repository,
    patch_releaseDate,
    patch_kbNumber,
    patch_id,
    patch_description,
    patch_bugzillaIds,
    patch_title,
    patch_arch,
    patch_epoch,
    patch_cVEIds,
    patch_msrcNumber,
    patch_release,
    patch_language,
    patch_vendor,
    patch_version,
    patch_contentUrl,
    patch_classification,

    -- ** PatchBaselineIdentity
    patchBaselineIdentity_operatingSystem,
    patchBaselineIdentity_baselineId,
    patchBaselineIdentity_baselineName,
    patchBaselineIdentity_baselineDescription,
    patchBaselineIdentity_defaultBaseline,

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
    patchGroupPatchBaselineMapping_patchGroup,
    patchGroupPatchBaselineMapping_baselineIdentity,

    -- ** PatchOrchestratorFilter
    patchOrchestratorFilter_key,
    patchOrchestratorFilter_values,

    -- ** PatchRule
    patchRule_approveAfterDays,
    patchRule_enableNonSecurity,
    patchRule_complianceLevel,
    patchRule_approveUntilDate,
    patchRule_patchFilterGroup,

    -- ** PatchRuleGroup
    patchRuleGroup_patchRules,

    -- ** PatchSource
    patchSource_name,
    patchSource_products,
    patchSource_configuration,

    -- ** PatchStatus
    patchStatus_deploymentStatus,
    patchStatus_complianceLevel,
    patchStatus_approvalDate,

    -- ** ProgressCounters
    progressCounters_cancelledSteps,
    progressCounters_timedOutSteps,
    progressCounters_failedSteps,
    progressCounters_successSteps,
    progressCounters_totalSteps,

    -- ** RegistrationMetadataItem
    registrationMetadataItem_key,
    registrationMetadataItem_value,

    -- ** RelatedOpsItem
    relatedOpsItem_opsItemId,

    -- ** ResolvedTargets
    resolvedTargets_truncated,
    resolvedTargets_parameterValues,

    -- ** ResourceComplianceSummaryItem
    resourceComplianceSummaryItem_resourceId,
    resourceComplianceSummaryItem_resourceType,
    resourceComplianceSummaryItem_compliantSummary,
    resourceComplianceSummaryItem_status,
    resourceComplianceSummaryItem_executionSummary,
    resourceComplianceSummaryItem_nonCompliantSummary,
    resourceComplianceSummaryItem_complianceType,
    resourceComplianceSummaryItem_overallSeverity,

    -- ** ResourceDataSyncAwsOrganizationsSource
    resourceDataSyncAwsOrganizationsSource_organizationalUnits,
    resourceDataSyncAwsOrganizationsSource_organizationSourceType,

    -- ** ResourceDataSyncDestinationDataSharing
    resourceDataSyncDestinationDataSharing_destinationDataSharingType,

    -- ** ResourceDataSyncItem
    resourceDataSyncItem_lastSyncTime,
    resourceDataSyncItem_lastSuccessfulSyncTime,
    resourceDataSyncItem_syncName,
    resourceDataSyncItem_syncLastModifiedTime,
    resourceDataSyncItem_lastStatus,
    resourceDataSyncItem_syncType,
    resourceDataSyncItem_syncCreatedTime,
    resourceDataSyncItem_s3Destination,
    resourceDataSyncItem_syncSource,
    resourceDataSyncItem_lastSyncStatusMessage,

    -- ** ResourceDataSyncOrganizationalUnit
    resourceDataSyncOrganizationalUnit_organizationalUnitId,

    -- ** ResourceDataSyncS3Destination
    resourceDataSyncS3Destination_destinationDataSharing,
    resourceDataSyncS3Destination_aWSKMSKeyARN,
    resourceDataSyncS3Destination_prefix,
    resourceDataSyncS3Destination_bucketName,
    resourceDataSyncS3Destination_syncFormat,
    resourceDataSyncS3Destination_region,

    -- ** ResourceDataSyncSource
    resourceDataSyncSource_enableAllOpsDataSources,
    resourceDataSyncSource_awsOrganizationsSource,
    resourceDataSyncSource_includeFutureRegions,
    resourceDataSyncSource_sourceType,
    resourceDataSyncSource_sourceRegions,

    -- ** ResourceDataSyncSourceWithState
    resourceDataSyncSourceWithState_enableAllOpsDataSources,
    resourceDataSyncSourceWithState_state,
    resourceDataSyncSourceWithState_sourceType,
    resourceDataSyncSourceWithState_sourceRegions,
    resourceDataSyncSourceWithState_awsOrganizationsSource,
    resourceDataSyncSourceWithState_includeFutureRegions,

    -- ** ResultAttribute
    resultAttribute_typeName,

    -- ** ReviewInformation
    reviewInformation_reviewer,
    reviewInformation_reviewedTime,
    reviewInformation_status,

    -- ** Runbook
    runbook_targetLocations,
    runbook_targetParameterName,
    runbook_targetMaps,
    runbook_targets,
    runbook_maxConcurrency,
    runbook_maxErrors,
    runbook_documentVersion,
    runbook_parameters,
    runbook_documentName,

    -- ** S3OutputLocation
    s3OutputLocation_outputS3Region,
    s3OutputLocation_outputS3BucketName,
    s3OutputLocation_outputS3KeyPrefix,

    -- ** S3OutputUrl
    s3OutputUrl_outputUrl,

    -- ** ScheduledWindowExecution
    scheduledWindowExecution_name,
    scheduledWindowExecution_executionTime,
    scheduledWindowExecution_windowId,

    -- ** ServiceSetting
    serviceSetting_lastModifiedUser,
    serviceSetting_lastModifiedDate,
    serviceSetting_arn,
    serviceSetting_settingId,
    serviceSetting_status,
    serviceSetting_settingValue,

    -- ** Session
    session_endDate,
    session_outputUrl,
    session_owner,
    session_status,
    session_target,
    session_details,
    session_startDate,
    session_documentName,
    session_sessionId,
    session_reason,
    session_maxSessionDuration,

    -- ** SessionFilter
    sessionFilter_key,
    sessionFilter_value,

    -- ** SessionManagerOutputUrl
    sessionManagerOutputUrl_s3OutputUrl,
    sessionManagerOutputUrl_cloudWatchOutputUrl,

    -- ** SeveritySummary
    severitySummary_mediumCount,
    severitySummary_informationalCount,
    severitySummary_unspecifiedCount,
    severitySummary_criticalCount,
    severitySummary_highCount,
    severitySummary_lowCount,

    -- ** StepExecution
    stepExecution_response,
    stepExecution_overriddenParameters,
    stepExecution_isEnd,
    stepExecution_timeoutSeconds,
    stepExecution_validNextSteps,
    stepExecution_targetLocation,
    stepExecution_targets,
    stepExecution_stepExecutionId,
    stepExecution_executionStartTime,
    stepExecution_stepName,
    stepExecution_failureMessage,
    stepExecution_onFailure,
    stepExecution_failureDetails,
    stepExecution_outputs,
    stepExecution_action,
    stepExecution_nextStep,
    stepExecution_maxAttempts,
    stepExecution_inputs,
    stepExecution_isCritical,
    stepExecution_triggeredAlarms,
    stepExecution_stepStatus,
    stepExecution_responseCode,
    stepExecution_executionEndTime,

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
    targetLocation_targetLocationAlarmConfiguration,
    targetLocation_regions,
    targetLocation_targetLocationMaxConcurrency,
    targetLocation_accounts,
    targetLocation_executionRoleName,
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
