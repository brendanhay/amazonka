{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Lens
  ( -- * Operations

    -- ** ListResourceComplianceSummaries
    listResourceComplianceSummaries_nextToken,
    listResourceComplianceSummaries_maxResults,
    listResourceComplianceSummaries_filters,
    listResourceComplianceSummariesResponse_nextToken,
    listResourceComplianceSummariesResponse_resourceComplianceSummaryItems,
    listResourceComplianceSummariesResponse_httpStatus,

    -- ** DescribePatchGroups
    describePatchGroups_nextToken,
    describePatchGroups_maxResults,
    describePatchGroups_filters,
    describePatchGroupsResponse_mappings,
    describePatchGroupsResponse_nextToken,
    describePatchGroupsResponse_httpStatus,

    -- ** DescribeOpsItems
    describeOpsItems_nextToken,
    describeOpsItems_maxResults,
    describeOpsItems_opsItemFilters,
    describeOpsItemsResponse_nextToken,
    describeOpsItemsResponse_opsItemSummaries,
    describeOpsItemsResponse_httpStatus,

    -- ** TerminateSession
    terminateSession_sessionId,
    terminateSessionResponse_sessionId,
    terminateSessionResponse_httpStatus,

    -- ** GetParameter
    getParameter_withDecryption,
    getParameter_name,
    getParameterResponse_parameter,
    getParameterResponse_httpStatus,

    -- ** GetOpsMetadata
    getOpsMetadata_nextToken,
    getOpsMetadata_maxResults,
    getOpsMetadata_opsMetadataArn,
    getOpsMetadataResponse_resourceId,
    getOpsMetadataResponse_nextToken,
    getOpsMetadataResponse_metadata,
    getOpsMetadataResponse_httpStatus,

    -- ** UpdateDocumentDefaultVersion
    updateDocumentDefaultVersion_name,
    updateDocumentDefaultVersion_documentVersion,
    updateDocumentDefaultVersionResponse_description,
    updateDocumentDefaultVersionResponse_httpStatus,

    -- ** ListResourceDataSync
    listResourceDataSync_syncType,
    listResourceDataSync_nextToken,
    listResourceDataSync_maxResults,
    listResourceDataSyncResponse_nextToken,
    listResourceDataSyncResponse_resourceDataSyncItems,
    listResourceDataSyncResponse_httpStatus,

    -- ** DescribeParameters
    describeParameters_nextToken,
    describeParameters_maxResults,
    describeParameters_parameterFilters,
    describeParameters_filters,
    describeParametersResponse_nextToken,
    describeParametersResponse_parameters,
    describeParametersResponse_httpStatus,

    -- ** RegisterTaskWithMaintenanceWindow
    registerTaskWithMaintenanceWindow_maxErrors,
    registerTaskWithMaintenanceWindow_taskParameters,
    registerTaskWithMaintenanceWindow_serviceRoleArn,
    registerTaskWithMaintenanceWindow_priority,
    registerTaskWithMaintenanceWindow_targets,
    registerTaskWithMaintenanceWindow_taskInvocationParameters,
    registerTaskWithMaintenanceWindow_name,
    registerTaskWithMaintenanceWindow_maxConcurrency,
    registerTaskWithMaintenanceWindow_description,
    registerTaskWithMaintenanceWindow_loggingInfo,
    registerTaskWithMaintenanceWindow_clientToken,
    registerTaskWithMaintenanceWindow_windowId,
    registerTaskWithMaintenanceWindow_taskArn,
    registerTaskWithMaintenanceWindow_taskType,
    registerTaskWithMaintenanceWindowResponse_windowTaskId,
    registerTaskWithMaintenanceWindowResponse_httpStatus,

    -- ** GetOpsItem
    getOpsItem_opsItemId,
    getOpsItemResponse_opsItem,
    getOpsItemResponse_httpStatus,

    -- ** GetInventory
    getInventory_nextToken,
    getInventory_maxResults,
    getInventory_resultAttributes,
    getInventory_filters,
    getInventory_aggregators,
    getInventoryResponse_nextToken,
    getInventoryResponse_entities,
    getInventoryResponse_httpStatus,

    -- ** DescribeAssociation
    describeAssociation_instanceId,
    describeAssociation_name,
    describeAssociation_associationId,
    describeAssociation_associationVersion,
    describeAssociationResponse_associationDescription,
    describeAssociationResponse_httpStatus,

    -- ** UpdateAssociationStatus
    updateAssociationStatus_name,
    updateAssociationStatus_instanceId,
    updateAssociationStatus_associationStatus,
    updateAssociationStatusResponse_associationDescription,
    updateAssociationStatusResponse_httpStatus,

    -- ** DeregisterManagedInstance
    deregisterManagedInstance_instanceId,
    deregisterManagedInstanceResponse_httpStatus,

    -- ** DescribeAssociationExecutionTargets
    describeAssociationExecutionTargets_nextToken,
    describeAssociationExecutionTargets_maxResults,
    describeAssociationExecutionTargets_filters,
    describeAssociationExecutionTargets_associationId,
    describeAssociationExecutionTargets_executionId,
    describeAssociationExecutionTargetsResponse_nextToken,
    describeAssociationExecutionTargetsResponse_associationExecutionTargets,
    describeAssociationExecutionTargetsResponse_httpStatus,

    -- ** UpdateMaintenanceWindowTarget
    updateMaintenanceWindowTarget_targets,
    updateMaintenanceWindowTarget_name,
    updateMaintenanceWindowTarget_replace,
    updateMaintenanceWindowTarget_description,
    updateMaintenanceWindowTarget_ownerInformation,
    updateMaintenanceWindowTarget_windowId,
    updateMaintenanceWindowTarget_windowTargetId,
    updateMaintenanceWindowTargetResponse_windowTargetId,
    updateMaintenanceWindowTargetResponse_targets,
    updateMaintenanceWindowTargetResponse_name,
    updateMaintenanceWindowTargetResponse_windowId,
    updateMaintenanceWindowTargetResponse_description,
    updateMaintenanceWindowTargetResponse_ownerInformation,
    updateMaintenanceWindowTargetResponse_httpStatus,

    -- ** DescribeAutomationStepExecutions
    describeAutomationStepExecutions_nextToken,
    describeAutomationStepExecutions_maxResults,
    describeAutomationStepExecutions_reverseOrder,
    describeAutomationStepExecutions_filters,
    describeAutomationStepExecutions_automationExecutionId,
    describeAutomationStepExecutionsResponse_nextToken,
    describeAutomationStepExecutionsResponse_stepExecutions,
    describeAutomationStepExecutionsResponse_httpStatus,

    -- ** CreatePatchBaseline
    createPatchBaseline_sources,
    createPatchBaseline_rejectedPatches,
    createPatchBaseline_approvedPatchesEnableNonSecurity,
    createPatchBaseline_approvedPatchesComplianceLevel,
    createPatchBaseline_tags,
    createPatchBaseline_description,
    createPatchBaseline_approvedPatches,
    createPatchBaseline_rejectedPatchesAction,
    createPatchBaseline_operatingSystem,
    createPatchBaseline_globalFilters,
    createPatchBaseline_clientToken,
    createPatchBaseline_approvalRules,
    createPatchBaseline_name,
    createPatchBaselineResponse_baselineId,
    createPatchBaselineResponse_httpStatus,

    -- ** GetCommandInvocation
    getCommandInvocation_pluginName,
    getCommandInvocation_commandId,
    getCommandInvocation_instanceId,
    getCommandInvocationResponse_standardOutputUrl,
    getCommandInvocationResponse_status,
    getCommandInvocationResponse_instanceId,
    getCommandInvocationResponse_statusDetails,
    getCommandInvocationResponse_pluginName,
    getCommandInvocationResponse_comment,
    getCommandInvocationResponse_executionStartDateTime,
    getCommandInvocationResponse_standardErrorUrl,
    getCommandInvocationResponse_documentName,
    getCommandInvocationResponse_commandId,
    getCommandInvocationResponse_standardErrorContent,
    getCommandInvocationResponse_executionEndDateTime,
    getCommandInvocationResponse_responseCode,
    getCommandInvocationResponse_cloudWatchOutputConfig,
    getCommandInvocationResponse_executionElapsedTime,
    getCommandInvocationResponse_documentVersion,
    getCommandInvocationResponse_standardOutputContent,
    getCommandInvocationResponse_httpStatus,

    -- ** ListCommands
    listCommands_nextToken,
    listCommands_instanceId,
    listCommands_maxResults,
    listCommands_commandId,
    listCommands_filters,
    listCommandsResponse_nextToken,
    listCommandsResponse_commands,
    listCommandsResponse_httpStatus,

    -- ** DeregisterTargetFromMaintenanceWindow
    deregisterTargetFromMaintenanceWindow_safe,
    deregisterTargetFromMaintenanceWindow_windowId,
    deregisterTargetFromMaintenanceWindow_windowTargetId,
    deregisterTargetFromMaintenanceWindowResponse_windowTargetId,
    deregisterTargetFromMaintenanceWindowResponse_windowId,
    deregisterTargetFromMaintenanceWindowResponse_httpStatus,

    -- ** DescribeInstancePatchStates
    describeInstancePatchStates_nextToken,
    describeInstancePatchStates_maxResults,
    describeInstancePatchStates_instanceIds,
    describeInstancePatchStatesResponse_nextToken,
    describeInstancePatchStatesResponse_instancePatchStates,
    describeInstancePatchStatesResponse_httpStatus,

    -- ** UpdateDocument
    updateDocument_targetType,
    updateDocument_versionName,
    updateDocument_documentFormat,
    updateDocument_documentVersion,
    updateDocument_attachments,
    updateDocument_content,
    updateDocument_name,
    updateDocumentResponse_documentDescription,
    updateDocumentResponse_httpStatus,

    -- ** ListDocuments
    listDocuments_nextToken,
    listDocuments_maxResults,
    listDocuments_filters,
    listDocuments_documentFilterList,
    listDocumentsResponse_nextToken,
    listDocumentsResponse_documentIdentifiers,
    listDocumentsResponse_httpStatus,

    -- ** ResetServiceSetting
    resetServiceSetting_settingId,
    resetServiceSettingResponse_serviceSetting,
    resetServiceSettingResponse_httpStatus,

    -- ** StartSession
    startSession_documentName,
    startSession_parameters,
    startSession_target,
    startSessionResponse_sessionId,
    startSessionResponse_streamUrl,
    startSessionResponse_tokenValue,
    startSessionResponse_httpStatus,

    -- ** GetOpsSummary
    getOpsSummary_nextToken,
    getOpsSummary_maxResults,
    getOpsSummary_syncName,
    getOpsSummary_resultAttributes,
    getOpsSummary_filters,
    getOpsSummary_aggregators,
    getOpsSummaryResponse_nextToken,
    getOpsSummaryResponse_entities,
    getOpsSummaryResponse_httpStatus,

    -- ** DeleteDocument
    deleteDocument_force,
    deleteDocument_versionName,
    deleteDocument_documentVersion,
    deleteDocument_name,
    deleteDocumentResponse_httpStatus,

    -- ** DeleteInventory
    deleteInventory_dryRun,
    deleteInventory_schemaDeleteOption,
    deleteInventory_clientToken,
    deleteInventory_typeName,
    deleteInventoryResponse_typeName,
    deleteInventoryResponse_deletionId,
    deleteInventoryResponse_deletionSummary,
    deleteInventoryResponse_httpStatus,

    -- ** DeleteParameters
    deleteParameters_names,
    deleteParametersResponse_invalidParameters,
    deleteParametersResponse_deletedParameters,
    deleteParametersResponse_httpStatus,

    -- ** ListOpsMetadata
    listOpsMetadata_nextToken,
    listOpsMetadata_maxResults,
    listOpsMetadata_filters,
    listOpsMetadataResponse_nextToken,
    listOpsMetadataResponse_opsMetadataList,
    listOpsMetadataResponse_httpStatus,

    -- ** SendCommand
    sendCommand_notificationConfig,
    sendCommand_instanceIds,
    sendCommand_maxErrors,
    sendCommand_serviceRoleArn,
    sendCommand_outputS3BucketName,
    sendCommand_comment,
    sendCommand_documentHash,
    sendCommand_targets,
    sendCommand_outputS3Region,
    sendCommand_maxConcurrency,
    sendCommand_outputS3KeyPrefix,
    sendCommand_timeoutSeconds,
    sendCommand_cloudWatchOutputConfig,
    sendCommand_documentHashType,
    sendCommand_documentVersion,
    sendCommand_parameters,
    sendCommand_documentName,
    sendCommandResponse_command,
    sendCommandResponse_httpStatus,

    -- ** RemoveTagsFromResource
    removeTagsFromResource_resourceType,
    removeTagsFromResource_resourceId,
    removeTagsFromResource_tagKeys,
    removeTagsFromResourceResponse_httpStatus,

    -- ** DescribeEffectiveInstanceAssociations
    describeEffectiveInstanceAssociations_nextToken,
    describeEffectiveInstanceAssociations_maxResults,
    describeEffectiveInstanceAssociations_instanceId,
    describeEffectiveInstanceAssociationsResponse_nextToken,
    describeEffectiveInstanceAssociationsResponse_associations,
    describeEffectiveInstanceAssociationsResponse_httpStatus,

    -- ** CreateDocument
    createDocument_documentType,
    createDocument_targetType,
    createDocument_requires,
    createDocument_versionName,
    createDocument_documentFormat,
    createDocument_tags,
    createDocument_attachments,
    createDocument_content,
    createDocument_name,
    createDocumentResponse_documentDescription,
    createDocumentResponse_httpStatus,

    -- ** GetCalendarState
    getCalendarState_atTime,
    getCalendarState_calendarNames,
    getCalendarStateResponse_atTime,
    getCalendarStateResponse_state,
    getCalendarStateResponse_nextTransitionTime,
    getCalendarStateResponse_httpStatus,

    -- ** RegisterTargetWithMaintenanceWindow
    registerTargetWithMaintenanceWindow_name,
    registerTargetWithMaintenanceWindow_description,
    registerTargetWithMaintenanceWindow_ownerInformation,
    registerTargetWithMaintenanceWindow_clientToken,
    registerTargetWithMaintenanceWindow_windowId,
    registerTargetWithMaintenanceWindow_resourceType,
    registerTargetWithMaintenanceWindow_targets,
    registerTargetWithMaintenanceWindowResponse_windowTargetId,
    registerTargetWithMaintenanceWindowResponse_httpStatus,

    -- ** DescribeSessions
    describeSessions_nextToken,
    describeSessions_maxResults,
    describeSessions_filters,
    describeSessions_state,
    describeSessionsResponse_nextToken,
    describeSessionsResponse_sessions,
    describeSessionsResponse_httpStatus,

    -- ** DeleteOpsMetadata
    deleteOpsMetadata_opsMetadataArn,
    deleteOpsMetadataResponse_httpStatus,

    -- ** UpdateOpsItem
    updateOpsItem_status,
    updateOpsItem_plannedEndTime,
    updateOpsItem_severity,
    updateOpsItem_actualStartTime,
    updateOpsItem_category,
    updateOpsItem_operationalData,
    updateOpsItem_title,
    updateOpsItem_priority,
    updateOpsItem_actualEndTime,
    updateOpsItem_plannedStartTime,
    updateOpsItem_notifications,
    updateOpsItem_description,
    updateOpsItem_relatedOpsItems,
    updateOpsItem_operationalDataToDelete,
    updateOpsItem_opsItemId,
    updateOpsItemResponse_httpStatus,

    -- ** DeleteAssociation
    deleteAssociation_instanceId,
    deleteAssociation_name,
    deleteAssociation_associationId,
    deleteAssociationResponse_httpStatus,

    -- ** UpdateAssociation
    updateAssociation_maxErrors,
    updateAssociation_complianceSeverity,
    updateAssociation_automationTargetParameterName,
    updateAssociation_targets,
    updateAssociation_targetLocations,
    updateAssociation_scheduleExpression,
    updateAssociation_name,
    updateAssociation_maxConcurrency,
    updateAssociation_associationName,
    updateAssociation_associationVersion,
    updateAssociation_documentVersion,
    updateAssociation_parameters,
    updateAssociation_outputLocation,
    updateAssociation_applyOnlyAtCronInterval,
    updateAssociation_syncCompliance,
    updateAssociation_associationId,
    updateAssociationResponse_associationDescription,
    updateAssociationResponse_httpStatus,

    -- ** UpdateOpsMetadata
    updateOpsMetadata_metadataToUpdate,
    updateOpsMetadata_keysToDelete,
    updateOpsMetadata_opsMetadataArn,
    updateOpsMetadataResponse_opsMetadataArn,
    updateOpsMetadataResponse_httpStatus,

    -- ** DescribeInstanceInformation
    describeInstanceInformation_nextToken,
    describeInstanceInformation_maxResults,
    describeInstanceInformation_instanceInformationFilterList,
    describeInstanceInformation_filters,
    describeInstanceInformationResponse_instanceInformationList,
    describeInstanceInformationResponse_nextToken,
    describeInstanceInformationResponse_httpStatus,

    -- ** DescribeMaintenanceWindowExecutionTaskInvocations
    describeMaintenanceWindowExecutionTaskInvocations_nextToken,
    describeMaintenanceWindowExecutionTaskInvocations_maxResults,
    describeMaintenanceWindowExecutionTaskInvocations_filters,
    describeMaintenanceWindowExecutionTaskInvocations_windowExecutionId,
    describeMaintenanceWindowExecutionTaskInvocations_taskId,
    describeMaintenanceWindowExecutionTaskInvocationsResponse_nextToken,
    describeMaintenanceWindowExecutionTaskInvocationsResponse_windowExecutionTaskInvocationIdentities,
    describeMaintenanceWindowExecutionTaskInvocationsResponse_httpStatus,

    -- ** DeleteParameter
    deleteParameter_name,
    deleteParameterResponse_httpStatus,

    -- ** GetMaintenanceWindowExecutionTaskInvocation
    getMaintenanceWindowExecutionTaskInvocation_windowExecutionId,
    getMaintenanceWindowExecutionTaskInvocation_taskId,
    getMaintenanceWindowExecutionTaskInvocation_invocationId,
    getMaintenanceWindowExecutionTaskInvocationResponse_status,
    getMaintenanceWindowExecutionTaskInvocationResponse_statusDetails,
    getMaintenanceWindowExecutionTaskInvocationResponse_windowTargetId,
    getMaintenanceWindowExecutionTaskInvocationResponse_startTime,
    getMaintenanceWindowExecutionTaskInvocationResponse_endTime,
    getMaintenanceWindowExecutionTaskInvocationResponse_executionId,
    getMaintenanceWindowExecutionTaskInvocationResponse_windowExecutionId,
    getMaintenanceWindowExecutionTaskInvocationResponse_ownerInformation,
    getMaintenanceWindowExecutionTaskInvocationResponse_taskType,
    getMaintenanceWindowExecutionTaskInvocationResponse_invocationId,
    getMaintenanceWindowExecutionTaskInvocationResponse_parameters,
    getMaintenanceWindowExecutionTaskInvocationResponse_taskExecutionId,
    getMaintenanceWindowExecutionTaskInvocationResponse_httpStatus,

    -- ** CreateActivation
    createActivation_registrationLimit,
    createActivation_defaultInstanceName,
    createActivation_expirationDate,
    createActivation_tags,
    createActivation_description,
    createActivation_iamRole,
    createActivationResponse_activationCode,
    createActivationResponse_activationId,
    createActivationResponse_httpStatus,

    -- ** CreateOpsMetadata
    createOpsMetadata_metadata,
    createOpsMetadata_resourceId,
    createOpsMetadataResponse_opsMetadataArn,
    createOpsMetadataResponse_httpStatus,

    -- ** SendAutomationSignal
    sendAutomationSignal_payload,
    sendAutomationSignal_automationExecutionId,
    sendAutomationSignal_signalType,
    sendAutomationSignalResponse_httpStatus,

    -- ** ListComplianceSummaries
    listComplianceSummaries_nextToken,
    listComplianceSummaries_maxResults,
    listComplianceSummaries_filters,
    listComplianceSummariesResponse_nextToken,
    listComplianceSummariesResponse_complianceSummaryItems,
    listComplianceSummariesResponse_httpStatus,

    -- ** DescribeMaintenanceWindowExecutions
    describeMaintenanceWindowExecutions_nextToken,
    describeMaintenanceWindowExecutions_maxResults,
    describeMaintenanceWindowExecutions_filters,
    describeMaintenanceWindowExecutions_windowId,
    describeMaintenanceWindowExecutionsResponse_nextToken,
    describeMaintenanceWindowExecutionsResponse_windowExecutions,
    describeMaintenanceWindowExecutionsResponse_httpStatus,

    -- ** DescribeMaintenanceWindowsForTarget
    describeMaintenanceWindowsForTarget_nextToken,
    describeMaintenanceWindowsForTarget_maxResults,
    describeMaintenanceWindowsForTarget_targets,
    describeMaintenanceWindowsForTarget_resourceType,
    describeMaintenanceWindowsForTargetResponse_nextToken,
    describeMaintenanceWindowsForTargetResponse_windowIdentities,
    describeMaintenanceWindowsForTargetResponse_httpStatus,

    -- ** CreateAssociation
    createAssociation_maxErrors,
    createAssociation_instanceId,
    createAssociation_complianceSeverity,
    createAssociation_automationTargetParameterName,
    createAssociation_targets,
    createAssociation_targetLocations,
    createAssociation_scheduleExpression,
    createAssociation_maxConcurrency,
    createAssociation_associationName,
    createAssociation_documentVersion,
    createAssociation_parameters,
    createAssociation_outputLocation,
    createAssociation_applyOnlyAtCronInterval,
    createAssociation_syncCompliance,
    createAssociation_name,
    createAssociationResponse_associationDescription,
    createAssociationResponse_httpStatus,

    -- ** CreateOpsItem
    createOpsItem_plannedEndTime,
    createOpsItem_severity,
    createOpsItem_actualStartTime,
    createOpsItem_category,
    createOpsItem_operationalData,
    createOpsItem_priority,
    createOpsItem_actualEndTime,
    createOpsItem_tags,
    createOpsItem_opsItemType,
    createOpsItem_plannedStartTime,
    createOpsItem_notifications,
    createOpsItem_relatedOpsItems,
    createOpsItem_description,
    createOpsItem_source,
    createOpsItem_title,
    createOpsItemResponse_opsItemId,
    createOpsItemResponse_httpStatus,

    -- ** StartChangeRequestExecution
    startChangeRequestExecution_changeRequestName,
    startChangeRequestExecution_scheduledTime,
    startChangeRequestExecution_tags,
    startChangeRequestExecution_documentVersion,
    startChangeRequestExecution_parameters,
    startChangeRequestExecution_clientToken,
    startChangeRequestExecution_documentName,
    startChangeRequestExecution_runbooks,
    startChangeRequestExecutionResponse_automationExecutionId,
    startChangeRequestExecutionResponse_httpStatus,

    -- ** ListAssociationVersions
    listAssociationVersions_nextToken,
    listAssociationVersions_maxResults,
    listAssociationVersions_associationId,
    listAssociationVersionsResponse_nextToken,
    listAssociationVersionsResponse_associationVersions,
    listAssociationVersionsResponse_httpStatus,

    -- ** DescribeDocument
    describeDocument_versionName,
    describeDocument_documentVersion,
    describeDocument_name,
    describeDocumentResponse_document,
    describeDocumentResponse_httpStatus,

    -- ** DeregisterTaskFromMaintenanceWindow
    deregisterTaskFromMaintenanceWindow_windowId,
    deregisterTaskFromMaintenanceWindow_windowTaskId,
    deregisterTaskFromMaintenanceWindowResponse_windowTaskId,
    deregisterTaskFromMaintenanceWindowResponse_windowId,
    deregisterTaskFromMaintenanceWindowResponse_httpStatus,

    -- ** PutComplianceItems
    putComplianceItems_uploadType,
    putComplianceItems_itemContentHash,
    putComplianceItems_resourceId,
    putComplianceItems_resourceType,
    putComplianceItems_complianceType,
    putComplianceItems_executionSummary,
    putComplianceItems_items,
    putComplianceItemsResponse_httpStatus,

    -- ** GetMaintenanceWindowTask
    getMaintenanceWindowTask_windowId,
    getMaintenanceWindowTask_windowTaskId,
    getMaintenanceWindowTaskResponse_maxErrors,
    getMaintenanceWindowTaskResponse_taskParameters,
    getMaintenanceWindowTaskResponse_windowTaskId,
    getMaintenanceWindowTaskResponse_serviceRoleArn,
    getMaintenanceWindowTaskResponse_priority,
    getMaintenanceWindowTaskResponse_targets,
    getMaintenanceWindowTaskResponse_taskInvocationParameters,
    getMaintenanceWindowTaskResponse_name,
    getMaintenanceWindowTaskResponse_maxConcurrency,
    getMaintenanceWindowTaskResponse_windowId,
    getMaintenanceWindowTaskResponse_description,
    getMaintenanceWindowTaskResponse_taskArn,
    getMaintenanceWindowTaskResponse_taskType,
    getMaintenanceWindowTaskResponse_loggingInfo,
    getMaintenanceWindowTaskResponse_httpStatus,

    -- ** GetDeployablePatchSnapshotForInstance
    getDeployablePatchSnapshotForInstance_baselineOverride,
    getDeployablePatchSnapshotForInstance_instanceId,
    getDeployablePatchSnapshotForInstance_snapshotId,
    getDeployablePatchSnapshotForInstanceResponse_instanceId,
    getDeployablePatchSnapshotForInstanceResponse_product,
    getDeployablePatchSnapshotForInstanceResponse_snapshotDownloadUrl,
    getDeployablePatchSnapshotForInstanceResponse_snapshotId,
    getDeployablePatchSnapshotForInstanceResponse_httpStatus,

    -- ** GetMaintenanceWindow
    getMaintenanceWindow_windowId,
    getMaintenanceWindowResponse_createdDate,
    getMaintenanceWindowResponse_startDate,
    getMaintenanceWindowResponse_duration,
    getMaintenanceWindowResponse_scheduleOffset,
    getMaintenanceWindowResponse_enabled,
    getMaintenanceWindowResponse_modifiedDate,
    getMaintenanceWindowResponse_cutoff,
    getMaintenanceWindowResponse_name,
    getMaintenanceWindowResponse_windowId,
    getMaintenanceWindowResponse_description,
    getMaintenanceWindowResponse_scheduleTimezone,
    getMaintenanceWindowResponse_endDate,
    getMaintenanceWindowResponse_nextExecutionTime,
    getMaintenanceWindowResponse_allowUnassociatedTargets,
    getMaintenanceWindowResponse_schedule,
    getMaintenanceWindowResponse_httpStatus,

    -- ** DeregisterPatchBaselineForPatchGroup
    deregisterPatchBaselineForPatchGroup_baselineId,
    deregisterPatchBaselineForPatchGroup_patchGroup,
    deregisterPatchBaselineForPatchGroupResponse_baselineId,
    deregisterPatchBaselineForPatchGroupResponse_patchGroup,
    deregisterPatchBaselineForPatchGroupResponse_httpStatus,

    -- ** ResumeSession
    resumeSession_sessionId,
    resumeSessionResponse_sessionId,
    resumeSessionResponse_streamUrl,
    resumeSessionResponse_tokenValue,
    resumeSessionResponse_httpStatus,

    -- ** RegisterDefaultPatchBaseline
    registerDefaultPatchBaseline_baselineId,
    registerDefaultPatchBaselineResponse_baselineId,
    registerDefaultPatchBaselineResponse_httpStatus,

    -- ** DescribeMaintenanceWindows
    describeMaintenanceWindows_nextToken,
    describeMaintenanceWindows_maxResults,
    describeMaintenanceWindows_filters,
    describeMaintenanceWindowsResponse_nextToken,
    describeMaintenanceWindowsResponse_windowIdentities,
    describeMaintenanceWindowsResponse_httpStatus,

    -- ** ModifyDocumentPermission
    modifyDocumentPermission_accountIdsToAdd,
    modifyDocumentPermission_sharedDocumentVersion,
    modifyDocumentPermission_accountIdsToRemove,
    modifyDocumentPermission_name,
    modifyDocumentPermission_permissionType,
    modifyDocumentPermissionResponse_httpStatus,

    -- ** DescribeInstancePatches
    describeInstancePatches_nextToken,
    describeInstancePatches_maxResults,
    describeInstancePatches_filters,
    describeInstancePatches_instanceId,
    describeInstancePatchesResponse_nextToken,
    describeInstancePatchesResponse_patches,
    describeInstancePatchesResponse_httpStatus,

    -- ** GetParameters
    getParameters_withDecryption,
    getParameters_names,
    getParametersResponse_invalidParameters,
    getParametersResponse_parameters,
    getParametersResponse_httpStatus,

    -- ** ListDocumentVersions
    listDocumentVersions_nextToken,
    listDocumentVersions_maxResults,
    listDocumentVersions_name,
    listDocumentVersionsResponse_nextToken,
    listDocumentVersionsResponse_documentVersions,
    listDocumentVersionsResponse_httpStatus,

    -- ** UpdateResourceDataSync
    updateResourceDataSync_syncName,
    updateResourceDataSync_syncType,
    updateResourceDataSync_syncSource,
    updateResourceDataSyncResponse_httpStatus,

    -- ** DeletePatchBaseline
    deletePatchBaseline_baselineId,
    deletePatchBaselineResponse_baselineId,
    deletePatchBaselineResponse_httpStatus,

    -- ** ListOpsItemEvents
    listOpsItemEvents_nextToken,
    listOpsItemEvents_maxResults,
    listOpsItemEvents_filters,
    listOpsItemEventsResponse_nextToken,
    listOpsItemEventsResponse_summaries,
    listOpsItemEventsResponse_httpStatus,

    -- ** DeleteResourceDataSync
    deleteResourceDataSync_syncType,
    deleteResourceDataSync_syncName,
    deleteResourceDataSyncResponse_httpStatus,

    -- ** DescribeInstancePatchStatesForPatchGroup
    describeInstancePatchStatesForPatchGroup_nextToken,
    describeInstancePatchStatesForPatchGroup_maxResults,
    describeInstancePatchStatesForPatchGroup_filters,
    describeInstancePatchStatesForPatchGroup_patchGroup,
    describeInstancePatchStatesForPatchGroupResponse_nextToken,
    describeInstancePatchStatesForPatchGroupResponse_instancePatchStates,
    describeInstancePatchStatesForPatchGroupResponse_httpStatus,

    -- ** GetConnectionStatus
    getConnectionStatus_target,
    getConnectionStatusResponse_status,
    getConnectionStatusResponse_target,
    getConnectionStatusResponse_httpStatus,

    -- ** UpdatePatchBaseline
    updatePatchBaseline_sources,
    updatePatchBaseline_rejectedPatches,
    updatePatchBaseline_approvedPatchesEnableNonSecurity,
    updatePatchBaseline_approvedPatchesComplianceLevel,
    updatePatchBaseline_name,
    updatePatchBaseline_replace,
    updatePatchBaseline_description,
    updatePatchBaseline_approvedPatches,
    updatePatchBaseline_rejectedPatchesAction,
    updatePatchBaseline_globalFilters,
    updatePatchBaseline_approvalRules,
    updatePatchBaseline_baselineId,
    updatePatchBaselineResponse_createdDate,
    updatePatchBaselineResponse_baselineId,
    updatePatchBaselineResponse_sources,
    updatePatchBaselineResponse_rejectedPatches,
    updatePatchBaselineResponse_approvedPatchesEnableNonSecurity,
    updatePatchBaselineResponse_approvedPatchesComplianceLevel,
    updatePatchBaselineResponse_modifiedDate,
    updatePatchBaselineResponse_name,
    updatePatchBaselineResponse_description,
    updatePatchBaselineResponse_approvedPatches,
    updatePatchBaselineResponse_rejectedPatchesAction,
    updatePatchBaselineResponse_operatingSystem,
    updatePatchBaselineResponse_globalFilters,
    updatePatchBaselineResponse_approvalRules,
    updatePatchBaselineResponse_httpStatus,

    -- ** DescribeAvailablePatches
    describeAvailablePatches_nextToken,
    describeAvailablePatches_maxResults,
    describeAvailablePatches_filters,
    describeAvailablePatchesResponse_nextToken,
    describeAvailablePatchesResponse_patches,
    describeAvailablePatchesResponse_httpStatus,

    -- ** ListComplianceItems
    listComplianceItems_nextToken,
    listComplianceItems_resourceTypes,
    listComplianceItems_maxResults,
    listComplianceItems_resourceIds,
    listComplianceItems_filters,
    listComplianceItemsResponse_nextToken,
    listComplianceItemsResponse_complianceItems,
    listComplianceItemsResponse_httpStatus,

    -- ** GetDocument
    getDocument_versionName,
    getDocument_documentFormat,
    getDocument_documentVersion,
    getDocument_name,
    getDocumentResponse_documentType,
    getDocumentResponse_status,
    getDocumentResponse_requires,
    getDocumentResponse_attachmentsContent,
    getDocumentResponse_statusInformation,
    getDocumentResponse_versionName,
    getDocumentResponse_name,
    getDocumentResponse_documentFormat,
    getDocumentResponse_content,
    getDocumentResponse_reviewStatus,
    getDocumentResponse_documentVersion,
    getDocumentResponse_httpStatus,

    -- ** AddTagsToResource
    addTagsToResource_resourceType,
    addTagsToResource_resourceId,
    addTagsToResource_tags,
    addTagsToResourceResponse_httpStatus,

    -- ** GetPatchBaselineForPatchGroup
    getPatchBaselineForPatchGroup_operatingSystem,
    getPatchBaselineForPatchGroup_patchGroup,
    getPatchBaselineForPatchGroupResponse_baselineId,
    getPatchBaselineForPatchGroupResponse_patchGroup,
    getPatchBaselineForPatchGroupResponse_operatingSystem,
    getPatchBaselineForPatchGroupResponse_httpStatus,

    -- ** UpdateManagedInstanceRole
    updateManagedInstanceRole_instanceId,
    updateManagedInstanceRole_iamRole,
    updateManagedInstanceRoleResponse_httpStatus,

    -- ** DescribeMaintenanceWindowSchedule
    describeMaintenanceWindowSchedule_nextToken,
    describeMaintenanceWindowSchedule_maxResults,
    describeMaintenanceWindowSchedule_resourceType,
    describeMaintenanceWindowSchedule_targets,
    describeMaintenanceWindowSchedule_windowId,
    describeMaintenanceWindowSchedule_filters,
    describeMaintenanceWindowScheduleResponse_nextToken,
    describeMaintenanceWindowScheduleResponse_scheduledWindowExecutions,
    describeMaintenanceWindowScheduleResponse_httpStatus,

    -- ** CreateResourceDataSync
    createResourceDataSync_syncType,
    createResourceDataSync_s3Destination,
    createResourceDataSync_syncSource,
    createResourceDataSync_syncName,
    createResourceDataSyncResponse_httpStatus,

    -- ** CreateAssociationBatch
    createAssociationBatch_entries,
    createAssociationBatchResponse_successful,
    createAssociationBatchResponse_failed,
    createAssociationBatchResponse_httpStatus,

    -- ** CancelCommand
    cancelCommand_instanceIds,
    cancelCommand_commandId,
    cancelCommandResponse_httpStatus,

    -- ** DescribeDocumentPermission
    describeDocumentPermission_nextToken,
    describeDocumentPermission_maxResults,
    describeDocumentPermission_name,
    describeDocumentPermission_permissionType,
    describeDocumentPermissionResponse_accountIds,
    describeDocumentPermissionResponse_nextToken,
    describeDocumentPermissionResponse_accountSharingInfoList,
    describeDocumentPermissionResponse_httpStatus,

    -- ** RegisterPatchBaselineForPatchGroup
    registerPatchBaselineForPatchGroup_baselineId,
    registerPatchBaselineForPatchGroup_patchGroup,
    registerPatchBaselineForPatchGroupResponse_baselineId,
    registerPatchBaselineForPatchGroupResponse_patchGroup,
    registerPatchBaselineForPatchGroupResponse_httpStatus,

    -- ** DescribeEffectivePatchesForPatchBaseline
    describeEffectivePatchesForPatchBaseline_nextToken,
    describeEffectivePatchesForPatchBaseline_maxResults,
    describeEffectivePatchesForPatchBaseline_baselineId,
    describeEffectivePatchesForPatchBaselineResponse_nextToken,
    describeEffectivePatchesForPatchBaselineResponse_effectivePatches,
    describeEffectivePatchesForPatchBaselineResponse_httpStatus,

    -- ** ListCommandInvocations
    listCommandInvocations_nextToken,
    listCommandInvocations_instanceId,
    listCommandInvocations_maxResults,
    listCommandInvocations_commandId,
    listCommandInvocations_details,
    listCommandInvocations_filters,
    listCommandInvocationsResponse_nextToken,
    listCommandInvocationsResponse_commandInvocations,
    listCommandInvocationsResponse_httpStatus,

    -- ** DescribeMaintenanceWindowTargets
    describeMaintenanceWindowTargets_nextToken,
    describeMaintenanceWindowTargets_maxResults,
    describeMaintenanceWindowTargets_filters,
    describeMaintenanceWindowTargets_windowId,
    describeMaintenanceWindowTargetsResponse_nextToken,
    describeMaintenanceWindowTargetsResponse_targets,
    describeMaintenanceWindowTargetsResponse_httpStatus,

    -- ** GetAutomationExecution
    getAutomationExecution_automationExecutionId,
    getAutomationExecutionResponse_automationExecution,
    getAutomationExecutionResponse_httpStatus,

    -- ** GetPatchBaseline
    getPatchBaseline_baselineId,
    getPatchBaselineResponse_createdDate,
    getPatchBaselineResponse_baselineId,
    getPatchBaselineResponse_sources,
    getPatchBaselineResponse_rejectedPatches,
    getPatchBaselineResponse_approvedPatchesEnableNonSecurity,
    getPatchBaselineResponse_approvedPatchesComplianceLevel,
    getPatchBaselineResponse_modifiedDate,
    getPatchBaselineResponse_patchGroups,
    getPatchBaselineResponse_name,
    getPatchBaselineResponse_description,
    getPatchBaselineResponse_approvedPatches,
    getPatchBaselineResponse_rejectedPatchesAction,
    getPatchBaselineResponse_operatingSystem,
    getPatchBaselineResponse_globalFilters,
    getPatchBaselineResponse_approvalRules,
    getPatchBaselineResponse_httpStatus,

    -- ** DescribePatchGroupState
    describePatchGroupState_patchGroup,
    describePatchGroupStateResponse_instancesWithInstalledOtherPatches,
    describePatchGroupStateResponse_instancesWithUnreportedNotApplicablePatches,
    describePatchGroupStateResponse_instancesWithInstalledRejectedPatches,
    describePatchGroupStateResponse_instances,
    describePatchGroupStateResponse_instancesWithMissingPatches,
    describePatchGroupStateResponse_instancesWithInstalledPendingRebootPatches,
    describePatchGroupStateResponse_instancesWithFailedPatches,
    describePatchGroupStateResponse_instancesWithInstalledPatches,
    describePatchGroupStateResponse_instancesWithNotApplicablePatches,
    describePatchGroupStateResponse_httpStatus,

    -- ** DescribePatchBaselines
    describePatchBaselines_nextToken,
    describePatchBaselines_maxResults,
    describePatchBaselines_filters,
    describePatchBaselinesResponse_nextToken,
    describePatchBaselinesResponse_baselineIdentities,
    describePatchBaselinesResponse_httpStatus,

    -- ** UpdateDocumentMetadata
    updateDocumentMetadata_documentVersion,
    updateDocumentMetadata_name,
    updateDocumentMetadata_documentReviews,
    updateDocumentMetadataResponse_httpStatus,

    -- ** DescribeAutomationExecutions
    describeAutomationExecutions_nextToken,
    describeAutomationExecutions_maxResults,
    describeAutomationExecutions_filters,
    describeAutomationExecutionsResponse_nextToken,
    describeAutomationExecutionsResponse_automationExecutionMetadataList,
    describeAutomationExecutionsResponse_httpStatus,

    -- ** PutInventory
    putInventory_instanceId,
    putInventory_items,
    putInventoryResponse_message,
    putInventoryResponse_httpStatus,

    -- ** DescribeInventoryDeletions
    describeInventoryDeletions_nextToken,
    describeInventoryDeletions_maxResults,
    describeInventoryDeletions_deletionId,
    describeInventoryDeletionsResponse_nextToken,
    describeInventoryDeletionsResponse_inventoryDeletions,
    describeInventoryDeletionsResponse_httpStatus,

    -- ** DescribeMaintenanceWindowExecutionTasks
    describeMaintenanceWindowExecutionTasks_nextToken,
    describeMaintenanceWindowExecutionTasks_maxResults,
    describeMaintenanceWindowExecutionTasks_filters,
    describeMaintenanceWindowExecutionTasks_windowExecutionId,
    describeMaintenanceWindowExecutionTasksResponse_nextToken,
    describeMaintenanceWindowExecutionTasksResponse_windowExecutionTaskIdentities,
    describeMaintenanceWindowExecutionTasksResponse_httpStatus,

    -- ** DeleteMaintenanceWindow
    deleteMaintenanceWindow_windowId,
    deleteMaintenanceWindowResponse_windowId,
    deleteMaintenanceWindowResponse_httpStatus,

    -- ** GetDefaultPatchBaseline
    getDefaultPatchBaseline_operatingSystem,
    getDefaultPatchBaselineResponse_baselineId,
    getDefaultPatchBaselineResponse_operatingSystem,
    getDefaultPatchBaselineResponse_httpStatus,

    -- ** PutParameter
    putParameter_policies,
    putParameter_overwrite,
    putParameter_tags,
    putParameter_description,
    putParameter_type,
    putParameter_dataType,
    putParameter_allowedPattern,
    putParameter_tier,
    putParameter_keyId,
    putParameter_name,
    putParameter_value,
    putParameterResponse_version,
    putParameterResponse_tier,
    putParameterResponse_httpStatus,

    -- ** UpdateMaintenanceWindow
    updateMaintenanceWindow_startDate,
    updateMaintenanceWindow_duration,
    updateMaintenanceWindow_scheduleOffset,
    updateMaintenanceWindow_enabled,
    updateMaintenanceWindow_cutoff,
    updateMaintenanceWindow_name,
    updateMaintenanceWindow_replace,
    updateMaintenanceWindow_description,
    updateMaintenanceWindow_scheduleTimezone,
    updateMaintenanceWindow_endDate,
    updateMaintenanceWindow_allowUnassociatedTargets,
    updateMaintenanceWindow_schedule,
    updateMaintenanceWindow_windowId,
    updateMaintenanceWindowResponse_startDate,
    updateMaintenanceWindowResponse_duration,
    updateMaintenanceWindowResponse_scheduleOffset,
    updateMaintenanceWindowResponse_enabled,
    updateMaintenanceWindowResponse_cutoff,
    updateMaintenanceWindowResponse_name,
    updateMaintenanceWindowResponse_windowId,
    updateMaintenanceWindowResponse_description,
    updateMaintenanceWindowResponse_scheduleTimezone,
    updateMaintenanceWindowResponse_endDate,
    updateMaintenanceWindowResponse_allowUnassociatedTargets,
    updateMaintenanceWindowResponse_schedule,
    updateMaintenanceWindowResponse_httpStatus,

    -- ** ListAssociations
    listAssociations_nextToken,
    listAssociations_maxResults,
    listAssociations_associationFilterList,
    listAssociationsResponse_nextToken,
    listAssociationsResponse_associations,
    listAssociationsResponse_httpStatus,

    -- ** GetMaintenanceWindowExecutionTask
    getMaintenanceWindowExecutionTask_windowExecutionId,
    getMaintenanceWindowExecutionTask_taskId,
    getMaintenanceWindowExecutionTaskResponse_maxErrors,
    getMaintenanceWindowExecutionTaskResponse_taskParameters,
    getMaintenanceWindowExecutionTaskResponse_status,
    getMaintenanceWindowExecutionTaskResponse_serviceRole,
    getMaintenanceWindowExecutionTaskResponse_statusDetails,
    getMaintenanceWindowExecutionTaskResponse_startTime,
    getMaintenanceWindowExecutionTaskResponse_priority,
    getMaintenanceWindowExecutionTaskResponse_endTime,
    getMaintenanceWindowExecutionTaskResponse_maxConcurrency,
    getMaintenanceWindowExecutionTaskResponse_windowExecutionId,
    getMaintenanceWindowExecutionTaskResponse_type,
    getMaintenanceWindowExecutionTaskResponse_taskArn,
    getMaintenanceWindowExecutionTaskResponse_taskExecutionId,
    getMaintenanceWindowExecutionTaskResponse_httpStatus,

    -- ** StartAutomationExecution
    startAutomationExecution_maxErrors,
    startAutomationExecution_mode,
    startAutomationExecution_targets,
    startAutomationExecution_targetLocations,
    startAutomationExecution_targetParameterName,
    startAutomationExecution_maxConcurrency,
    startAutomationExecution_tags,
    startAutomationExecution_targetMaps,
    startAutomationExecution_documentVersion,
    startAutomationExecution_parameters,
    startAutomationExecution_clientToken,
    startAutomationExecution_documentName,
    startAutomationExecutionResponse_automationExecutionId,
    startAutomationExecutionResponse_httpStatus,

    -- ** DeleteActivation
    deleteActivation_activationId,
    deleteActivationResponse_httpStatus,

    -- ** CreateMaintenanceWindow
    createMaintenanceWindow_startDate,
    createMaintenanceWindow_scheduleOffset,
    createMaintenanceWindow_tags,
    createMaintenanceWindow_description,
    createMaintenanceWindow_scheduleTimezone,
    createMaintenanceWindow_endDate,
    createMaintenanceWindow_clientToken,
    createMaintenanceWindow_name,
    createMaintenanceWindow_schedule,
    createMaintenanceWindow_duration,
    createMaintenanceWindow_cutoff,
    createMaintenanceWindow_allowUnassociatedTargets,
    createMaintenanceWindowResponse_windowId,
    createMaintenanceWindowResponse_httpStatus,

    -- ** DescribeAssociationExecutions
    describeAssociationExecutions_nextToken,
    describeAssociationExecutions_maxResults,
    describeAssociationExecutions_filters,
    describeAssociationExecutions_associationId,
    describeAssociationExecutionsResponse_nextToken,
    describeAssociationExecutionsResponse_associationExecutions,
    describeAssociationExecutionsResponse_httpStatus,

    -- ** GetMaintenanceWindowExecution
    getMaintenanceWindowExecution_windowExecutionId,
    getMaintenanceWindowExecutionResponse_status,
    getMaintenanceWindowExecutionResponse_statusDetails,
    getMaintenanceWindowExecutionResponse_taskIds,
    getMaintenanceWindowExecutionResponse_startTime,
    getMaintenanceWindowExecutionResponse_endTime,
    getMaintenanceWindowExecutionResponse_windowExecutionId,
    getMaintenanceWindowExecutionResponse_httpStatus,

    -- ** StopAutomationExecution
    stopAutomationExecution_type,
    stopAutomationExecution_automationExecutionId,
    stopAutomationExecutionResponse_httpStatus,

    -- ** GetInventorySchema
    getInventorySchema_typeName,
    getInventorySchema_nextToken,
    getInventorySchema_subType,
    getInventorySchema_aggregator,
    getInventorySchema_maxResults,
    getInventorySchemaResponse_nextToken,
    getInventorySchemaResponse_schemas,
    getInventorySchemaResponse_httpStatus,

    -- ** StartAssociationsOnce
    startAssociationsOnce_associationIds,
    startAssociationsOnceResponse_httpStatus,

    -- ** CancelMaintenanceWindowExecution
    cancelMaintenanceWindowExecution_windowExecutionId,
    cancelMaintenanceWindowExecutionResponse_windowExecutionId,
    cancelMaintenanceWindowExecutionResponse_httpStatus,

    -- ** LabelParameterVersion
    labelParameterVersion_parameterVersion,
    labelParameterVersion_name,
    labelParameterVersion_labels,
    labelParameterVersionResponse_invalidLabels,
    labelParameterVersionResponse_parameterVersion,
    labelParameterVersionResponse_httpStatus,

    -- ** GetParameterHistory
    getParameterHistory_withDecryption,
    getParameterHistory_nextToken,
    getParameterHistory_maxResults,
    getParameterHistory_name,
    getParameterHistoryResponse_nextToken,
    getParameterHistoryResponse_parameters,
    getParameterHistoryResponse_httpStatus,

    -- ** GetServiceSetting
    getServiceSetting_settingId,
    getServiceSettingResponse_serviceSetting,
    getServiceSettingResponse_httpStatus,

    -- ** UpdateMaintenanceWindowTask
    updateMaintenanceWindowTask_maxErrors,
    updateMaintenanceWindowTask_taskParameters,
    updateMaintenanceWindowTask_serviceRoleArn,
    updateMaintenanceWindowTask_priority,
    updateMaintenanceWindowTask_targets,
    updateMaintenanceWindowTask_taskInvocationParameters,
    updateMaintenanceWindowTask_name,
    updateMaintenanceWindowTask_replace,
    updateMaintenanceWindowTask_maxConcurrency,
    updateMaintenanceWindowTask_description,
    updateMaintenanceWindowTask_taskArn,
    updateMaintenanceWindowTask_loggingInfo,
    updateMaintenanceWindowTask_windowId,
    updateMaintenanceWindowTask_windowTaskId,
    updateMaintenanceWindowTaskResponse_maxErrors,
    updateMaintenanceWindowTaskResponse_taskParameters,
    updateMaintenanceWindowTaskResponse_windowTaskId,
    updateMaintenanceWindowTaskResponse_serviceRoleArn,
    updateMaintenanceWindowTaskResponse_priority,
    updateMaintenanceWindowTaskResponse_targets,
    updateMaintenanceWindowTaskResponse_taskInvocationParameters,
    updateMaintenanceWindowTaskResponse_name,
    updateMaintenanceWindowTaskResponse_maxConcurrency,
    updateMaintenanceWindowTaskResponse_windowId,
    updateMaintenanceWindowTaskResponse_description,
    updateMaintenanceWindowTaskResponse_taskArn,
    updateMaintenanceWindowTaskResponse_loggingInfo,
    updateMaintenanceWindowTaskResponse_httpStatus,

    -- ** ListDocumentMetadataHistory
    listDocumentMetadataHistory_nextToken,
    listDocumentMetadataHistory_maxResults,
    listDocumentMetadataHistory_documentVersion,
    listDocumentMetadataHistory_name,
    listDocumentMetadataHistory_metadata,
    listDocumentMetadataHistoryResponse_nextToken,
    listDocumentMetadataHistoryResponse_author,
    listDocumentMetadataHistoryResponse_metadata,
    listDocumentMetadataHistoryResponse_name,
    listDocumentMetadataHistoryResponse_documentVersion,
    listDocumentMetadataHistoryResponse_httpStatus,

    -- ** ListInventoryEntries
    listInventoryEntries_nextToken,
    listInventoryEntries_maxResults,
    listInventoryEntries_filters,
    listInventoryEntries_instanceId,
    listInventoryEntries_typeName,
    listInventoryEntriesResponse_typeName,
    listInventoryEntriesResponse_nextToken,
    listInventoryEntriesResponse_instanceId,
    listInventoryEntriesResponse_captureTime,
    listInventoryEntriesResponse_schemaVersion,
    listInventoryEntriesResponse_entries,
    listInventoryEntriesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceType,
    listTagsForResource_resourceId,
    listTagsForResourceResponse_tagList,
    listTagsForResourceResponse_httpStatus,

    -- ** GetParametersByPath
    getParametersByPath_withDecryption,
    getParametersByPath_nextToken,
    getParametersByPath_maxResults,
    getParametersByPath_recursive,
    getParametersByPath_parameterFilters,
    getParametersByPath_path,
    getParametersByPathResponse_nextToken,
    getParametersByPathResponse_parameters,
    getParametersByPathResponse_httpStatus,

    -- ** DescribeActivations
    describeActivations_nextToken,
    describeActivations_maxResults,
    describeActivations_filters,
    describeActivationsResponse_nextToken,
    describeActivationsResponse_activationList,
    describeActivationsResponse_httpStatus,

    -- ** DescribeInstanceAssociationsStatus
    describeInstanceAssociationsStatus_nextToken,
    describeInstanceAssociationsStatus_maxResults,
    describeInstanceAssociationsStatus_instanceId,
    describeInstanceAssociationsStatusResponse_nextToken,
    describeInstanceAssociationsStatusResponse_instanceAssociationStatusInfos,
    describeInstanceAssociationsStatusResponse_httpStatus,

    -- ** DescribePatchProperties
    describePatchProperties_nextToken,
    describePatchProperties_maxResults,
    describePatchProperties_patchSet,
    describePatchProperties_operatingSystem,
    describePatchProperties_property,
    describePatchPropertiesResponse_nextToken,
    describePatchPropertiesResponse_properties,
    describePatchPropertiesResponse_httpStatus,

    -- ** UpdateServiceSetting
    updateServiceSetting_settingId,
    updateServiceSetting_settingValue,
    updateServiceSettingResponse_httpStatus,

    -- ** DescribeMaintenanceWindowTasks
    describeMaintenanceWindowTasks_nextToken,
    describeMaintenanceWindowTasks_maxResults,
    describeMaintenanceWindowTasks_filters,
    describeMaintenanceWindowTasks_windowId,
    describeMaintenanceWindowTasksResponse_nextToken,
    describeMaintenanceWindowTasksResponse_tasks,
    describeMaintenanceWindowTasksResponse_httpStatus,

    -- * Types

    -- ** AccountSharingInfo
    accountSharingInfo_accountId,
    accountSharingInfo_sharedDocumentVersion,

    -- ** Activation
    activation_registrationLimit,
    activation_createdDate,
    activation_activationId,
    activation_iamRole,
    activation_defaultInstanceName,
    activation_expirationDate,
    activation_expired,
    activation_registrationsCount,
    activation_tags,
    activation_description,

    -- ** Association
    association_lastExecutionDate,
    association_instanceId,
    association_overview,
    association_targets,
    association_scheduleExpression,
    association_name,
    association_associationId,
    association_associationName,
    association_associationVersion,
    association_documentVersion,

    -- ** AssociationDescription
    associationDescription_maxErrors,
    associationDescription_status,
    associationDescription_lastExecutionDate,
    associationDescription_instanceId,
    associationDescription_lastUpdateAssociationDate,
    associationDescription_complianceSeverity,
    associationDescription_overview,
    associationDescription_automationTargetParameterName,
    associationDescription_targets,
    associationDescription_targetLocations,
    associationDescription_scheduleExpression,
    associationDescription_name,
    associationDescription_associationId,
    associationDescription_date,
    associationDescription_maxConcurrency,
    associationDescription_associationName,
    associationDescription_associationVersion,
    associationDescription_lastSuccessfulExecutionDate,
    associationDescription_documentVersion,
    associationDescription_parameters,
    associationDescription_outputLocation,
    associationDescription_applyOnlyAtCronInterval,
    associationDescription_syncCompliance,

    -- ** AssociationExecution
    associationExecution_status,
    associationExecution_lastExecutionDate,
    associationExecution_detailedStatus,
    associationExecution_resourceCountByStatus,
    associationExecution_createdTime,
    associationExecution_executionId,
    associationExecution_associationId,
    associationExecution_associationVersion,

    -- ** AssociationExecutionFilter
    associationExecutionFilter_key,
    associationExecutionFilter_value,
    associationExecutionFilter_type,

    -- ** AssociationExecutionTarget
    associationExecutionTarget_resourceId,
    associationExecutionTarget_status,
    associationExecutionTarget_lastExecutionDate,
    associationExecutionTarget_detailedStatus,
    associationExecutionTarget_resourceType,
    associationExecutionTarget_outputSource,
    associationExecutionTarget_executionId,
    associationExecutionTarget_associationId,
    associationExecutionTarget_associationVersion,

    -- ** AssociationExecutionTargetsFilter
    associationExecutionTargetsFilter_key,
    associationExecutionTargetsFilter_value,

    -- ** AssociationFilter
    associationFilter_key,
    associationFilter_value,

    -- ** AssociationOverview
    associationOverview_status,
    associationOverview_detailedStatus,
    associationOverview_associationStatusAggregatedCount,

    -- ** AssociationStatus
    associationStatus_additionalInfo,
    associationStatus_date,
    associationStatus_name,
    associationStatus_message,

    -- ** AssociationVersionInfo
    associationVersionInfo_maxErrors,
    associationVersionInfo_createdDate,
    associationVersionInfo_complianceSeverity,
    associationVersionInfo_targets,
    associationVersionInfo_targetLocations,
    associationVersionInfo_scheduleExpression,
    associationVersionInfo_name,
    associationVersionInfo_associationId,
    associationVersionInfo_maxConcurrency,
    associationVersionInfo_associationName,
    associationVersionInfo_associationVersion,
    associationVersionInfo_documentVersion,
    associationVersionInfo_parameters,
    associationVersionInfo_outputLocation,
    associationVersionInfo_applyOnlyAtCronInterval,
    associationVersionInfo_syncCompliance,

    -- ** AttachmentContent
    attachmentContent_hash,
    attachmentContent_name,
    attachmentContent_url,
    attachmentContent_size,
    attachmentContent_hashType,

    -- ** AttachmentInformation
    attachmentInformation_name,

    -- ** AttachmentsSource
    attachmentsSource_key,
    attachmentsSource_values,
    attachmentsSource_name,

    -- ** AutomationExecution
    automationExecution_maxErrors,
    automationExecution_currentAction,
    automationExecution_parentAutomationExecutionId,
    automationExecution_outputs,
    automationExecution_mode,
    automationExecution_failureMessage,
    automationExecution_executionEndTime,
    automationExecution_documentName,
    automationExecution_automationExecutionId,
    automationExecution_changeRequestName,
    automationExecution_executedBy,
    automationExecution_progressCounters,
    automationExecution_resolvedTargets,
    automationExecution_targets,
    automationExecution_targetLocations,
    automationExecution_targetParameterName,
    automationExecution_executionStartTime,
    automationExecution_currentStepName,
    automationExecution_associationId,
    automationExecution_opsItemId,
    automationExecution_scheduledTime,
    automationExecution_maxConcurrency,
    automationExecution_stepExecutionsTruncated,
    automationExecution_target,
    automationExecution_automationExecutionStatus,
    automationExecution_targetMaps,
    automationExecution_runbooks,
    automationExecution_stepExecutions,
    automationExecution_automationSubtype,
    automationExecution_documentVersion,
    automationExecution_parameters,

    -- ** AutomationExecutionFilter
    automationExecutionFilter_key,
    automationExecutionFilter_values,

    -- ** AutomationExecutionMetadata
    automationExecutionMetadata_maxErrors,
    automationExecutionMetadata_currentAction,
    automationExecutionMetadata_parentAutomationExecutionId,
    automationExecutionMetadata_outputs,
    automationExecutionMetadata_mode,
    automationExecutionMetadata_failureMessage,
    automationExecutionMetadata_executionEndTime,
    automationExecutionMetadata_documentName,
    automationExecutionMetadata_automationExecutionId,
    automationExecutionMetadata_changeRequestName,
    automationExecutionMetadata_executedBy,
    automationExecutionMetadata_resolvedTargets,
    automationExecutionMetadata_targets,
    automationExecutionMetadata_automationType,
    automationExecutionMetadata_targetParameterName,
    automationExecutionMetadata_executionStartTime,
    automationExecutionMetadata_currentStepName,
    automationExecutionMetadata_associationId,
    automationExecutionMetadata_opsItemId,
    automationExecutionMetadata_scheduledTime,
    automationExecutionMetadata_maxConcurrency,
    automationExecutionMetadata_target,
    automationExecutionMetadata_automationExecutionStatus,
    automationExecutionMetadata_targetMaps,
    automationExecutionMetadata_runbooks,
    automationExecutionMetadata_automationSubtype,
    automationExecutionMetadata_documentVersion,
    automationExecutionMetadata_logFile,

    -- ** BaselineOverride
    baselineOverride_sources,
    baselineOverride_rejectedPatches,
    baselineOverride_approvedPatchesEnableNonSecurity,
    baselineOverride_approvedPatchesComplianceLevel,
    baselineOverride_approvedPatches,
    baselineOverride_rejectedPatchesAction,
    baselineOverride_operatingSystem,
    baselineOverride_globalFilters,
    baselineOverride_approvalRules,

    -- ** CloudWatchOutputConfig
    cloudWatchOutputConfig_cloudWatchLogGroupName,
    cloudWatchOutputConfig_cloudWatchOutputEnabled,

    -- ** Command
    command_notificationConfig,
    command_instanceIds,
    command_maxErrors,
    command_expiresAfter,
    command_status,
    command_serviceRole,
    command_requestedDateTime,
    command_statusDetails,
    command_completedCount,
    command_outputS3BucketName,
    command_comment,
    command_errorCount,
    command_documentName,
    command_commandId,
    command_targets,
    command_outputS3Region,
    command_maxConcurrency,
    command_outputS3KeyPrefix,
    command_timeoutSeconds,
    command_deliveryTimedOutCount,
    command_cloudWatchOutputConfig,
    command_documentVersion,
    command_parameters,
    command_targetCount,

    -- ** CommandFilter
    commandFilter_key,
    commandFilter_value,

    -- ** CommandInvocation
    commandInvocation_notificationConfig,
    commandInvocation_standardOutputUrl,
    commandInvocation_status,
    commandInvocation_instanceId,
    commandInvocation_serviceRole,
    commandInvocation_requestedDateTime,
    commandInvocation_statusDetails,
    commandInvocation_instanceName,
    commandInvocation_comment,
    commandInvocation_standardErrorUrl,
    commandInvocation_documentName,
    commandInvocation_commandId,
    commandInvocation_traceOutput,
    commandInvocation_cloudWatchOutputConfig,
    commandInvocation_commandPlugins,
    commandInvocation_documentVersion,

    -- ** CommandPlugin
    commandPlugin_standardOutputUrl,
    commandPlugin_status,
    commandPlugin_statusDetails,
    commandPlugin_outputS3BucketName,
    commandPlugin_standardErrorUrl,
    commandPlugin_output,
    commandPlugin_outputS3Region,
    commandPlugin_name,
    commandPlugin_responseFinishDateTime,
    commandPlugin_responseCode,
    commandPlugin_outputS3KeyPrefix,
    commandPlugin_responseStartDateTime,

    -- ** ComplianceExecutionSummary
    complianceExecutionSummary_executionId,
    complianceExecutionSummary_executionType,
    complianceExecutionSummary_executionTime,

    -- ** ComplianceItem
    complianceItem_resourceId,
    complianceItem_status,
    complianceItem_severity,
    complianceItem_title,
    complianceItem_id,
    complianceItem_complianceType,
    complianceItem_resourceType,
    complianceItem_details,
    complianceItem_executionSummary,

    -- ** ComplianceItemEntry
    complianceItemEntry_title,
    complianceItemEntry_id,
    complianceItemEntry_details,
    complianceItemEntry_severity,
    complianceItemEntry_status,

    -- ** ComplianceStringFilter
    complianceStringFilter_key,
    complianceStringFilter_values,
    complianceStringFilter_type,

    -- ** ComplianceSummaryItem
    complianceSummaryItem_compliantSummary,
    complianceSummaryItem_complianceType,
    complianceSummaryItem_nonCompliantSummary,

    -- ** CompliantSummary
    compliantSummary_severitySummary,
    compliantSummary_compliantCount,

    -- ** CreateAssociationBatchRequestEntry
    createAssociationBatchRequestEntry_maxErrors,
    createAssociationBatchRequestEntry_instanceId,
    createAssociationBatchRequestEntry_complianceSeverity,
    createAssociationBatchRequestEntry_automationTargetParameterName,
    createAssociationBatchRequestEntry_targets,
    createAssociationBatchRequestEntry_targetLocations,
    createAssociationBatchRequestEntry_scheduleExpression,
    createAssociationBatchRequestEntry_maxConcurrency,
    createAssociationBatchRequestEntry_associationName,
    createAssociationBatchRequestEntry_documentVersion,
    createAssociationBatchRequestEntry_parameters,
    createAssociationBatchRequestEntry_outputLocation,
    createAssociationBatchRequestEntry_applyOnlyAtCronInterval,
    createAssociationBatchRequestEntry_syncCompliance,
    createAssociationBatchRequestEntry_name,

    -- ** DescribeActivationsFilter
    describeActivationsFilter_filterKey,
    describeActivationsFilter_filterValues,

    -- ** DocumentDefaultVersionDescription
    documentDefaultVersionDescription_defaultVersion,
    documentDefaultVersionDescription_name,
    documentDefaultVersionDescription_defaultVersionName,

    -- ** DocumentDescription
    documentDescription_documentType,
    documentDescription_status,
    documentDescription_createdDate,
    documentDescription_platformTypes,
    documentDescription_defaultVersion,
    documentDescription_latestVersion,
    documentDescription_targetType,
    documentDescription_approvedVersion,
    documentDescription_requires,
    documentDescription_sha1,
    documentDescription_statusInformation,
    documentDescription_versionName,
    documentDescription_author,
    documentDescription_hash,
    documentDescription_pendingReviewVersion,
    documentDescription_name,
    documentDescription_documentFormat,
    documentDescription_tags,
    documentDescription_owner,
    documentDescription_reviewStatus,
    documentDescription_reviewInformation,
    documentDescription_attachmentsInformation,
    documentDescription_description,
    documentDescription_schemaVersion,
    documentDescription_documentVersion,
    documentDescription_parameters,
    documentDescription_hashType,

    -- ** DocumentFilter
    documentFilter_key,
    documentFilter_value,

    -- ** DocumentIdentifier
    documentIdentifier_documentType,
    documentIdentifier_platformTypes,
    documentIdentifier_targetType,
    documentIdentifier_requires,
    documentIdentifier_versionName,
    documentIdentifier_author,
    documentIdentifier_name,
    documentIdentifier_documentFormat,
    documentIdentifier_tags,
    documentIdentifier_owner,
    documentIdentifier_reviewStatus,
    documentIdentifier_schemaVersion,
    documentIdentifier_documentVersion,

    -- ** DocumentKeyValuesFilter
    documentKeyValuesFilter_key,
    documentKeyValuesFilter_values,

    -- ** DocumentMetadataResponseInfo
    documentMetadataResponseInfo_reviewerResponse,

    -- ** DocumentParameter
    documentParameter_name,
    documentParameter_description,
    documentParameter_type,
    documentParameter_defaultValue,

    -- ** DocumentRequires
    documentRequires_version,
    documentRequires_name,

    -- ** DocumentReviewCommentSource
    documentReviewCommentSource_content,
    documentReviewCommentSource_type,

    -- ** DocumentReviewerResponseSource
    documentReviewerResponseSource_comment,
    documentReviewerResponseSource_updatedTime,
    documentReviewerResponseSource_createTime,
    documentReviewerResponseSource_reviewStatus,
    documentReviewerResponseSource_reviewer,

    -- ** DocumentReviews
    documentReviews_comment,
    documentReviews_action,

    -- ** DocumentVersionInfo
    documentVersionInfo_status,
    documentVersionInfo_createdDate,
    documentVersionInfo_statusInformation,
    documentVersionInfo_versionName,
    documentVersionInfo_name,
    documentVersionInfo_documentFormat,
    documentVersionInfo_reviewStatus,
    documentVersionInfo_isDefaultVersion,
    documentVersionInfo_documentVersion,

    -- ** EffectivePatch
    effectivePatch_patch,
    effectivePatch_patchStatus,

    -- ** FailedCreateAssociation
    failedCreateAssociation_entry,
    failedCreateAssociation_message,
    failedCreateAssociation_fault,

    -- ** FailureDetails
    failureDetails_details,
    failureDetails_failureStage,
    failureDetails_failureType,

    -- ** InstanceAggregatedAssociationOverview
    instanceAggregatedAssociationOverview_detailedStatus,
    instanceAggregatedAssociationOverview_instanceAssociationStatusAggregatedCount,

    -- ** InstanceAssociation
    instanceAssociation_instanceId,
    instanceAssociation_associationId,
    instanceAssociation_content,
    instanceAssociation_associationVersion,

    -- ** InstanceAssociationOutputLocation
    instanceAssociationOutputLocation_s3Location,

    -- ** InstanceAssociationOutputUrl
    instanceAssociationOutputUrl_s3OutputUrl,

    -- ** InstanceAssociationStatusInfo
    instanceAssociationStatusInfo_status,
    instanceAssociationStatusInfo_instanceId,
    instanceAssociationStatusInfo_detailedStatus,
    instanceAssociationStatusInfo_name,
    instanceAssociationStatusInfo_outputUrl,
    instanceAssociationStatusInfo_associationId,
    instanceAssociationStatusInfo_associationName,
    instanceAssociationStatusInfo_executionDate,
    instanceAssociationStatusInfo_executionSummary,
    instanceAssociationStatusInfo_associationVersion,
    instanceAssociationStatusInfo_documentVersion,
    instanceAssociationStatusInfo_errorCode,

    -- ** InstanceInformation
    instanceInformation_instanceId,
    instanceInformation_pingStatus,
    instanceInformation_activationId,
    instanceInformation_iamRole,
    instanceInformation_lastSuccessfulAssociationExecutionDate,
    instanceInformation_lastPingDateTime,
    instanceInformation_agentVersion,
    instanceInformation_platformVersion,
    instanceInformation_lastAssociationExecutionDate,
    instanceInformation_resourceType,
    instanceInformation_associationOverview,
    instanceInformation_iPAddress,
    instanceInformation_name,
    instanceInformation_platformType,
    instanceInformation_isLatestVersion,
    instanceInformation_computerName,
    instanceInformation_platformName,
    instanceInformation_registrationDate,
    instanceInformation_associationStatus,

    -- ** InstanceInformationFilter
    instanceInformationFilter_key,
    instanceInformationFilter_valueSet,

    -- ** InstanceInformationStringFilter
    instanceInformationStringFilter_key,
    instanceInformationStringFilter_values,

    -- ** InstancePatchState
    instancePatchState_installOverrideList,
    instancePatchState_unreportedNotApplicableCount,
    instancePatchState_installedOtherCount,
    instancePatchState_installedPendingRebootCount,
    instancePatchState_rebootOption,
    instancePatchState_missingCount,
    instancePatchState_snapshotId,
    instancePatchState_installedCount,
    instancePatchState_lastNoRebootInstallOperationTime,
    instancePatchState_notApplicableCount,
    instancePatchState_failedCount,
    instancePatchState_ownerInformation,
    instancePatchState_installedRejectedCount,
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
    inventoryAggregator_groups,
    inventoryAggregator_aggregators,
    inventoryAggregator_expression,

    -- ** InventoryDeletionStatusItem
    inventoryDeletionStatusItem_typeName,
    inventoryDeletionStatusItem_lastStatusMessage,
    inventoryDeletionStatusItem_lastStatusUpdateTime,
    inventoryDeletionStatusItem_deletionId,
    inventoryDeletionStatusItem_deletionStartTime,
    inventoryDeletionStatusItem_lastStatus,
    inventoryDeletionStatusItem_deletionSummary,

    -- ** InventoryDeletionSummary
    inventoryDeletionSummary_remainingCount,
    inventoryDeletionSummary_totalCount,
    inventoryDeletionSummary_summaryItems,

    -- ** InventoryDeletionSummaryItem
    inventoryDeletionSummaryItem_remainingCount,
    inventoryDeletionSummaryItem_version,
    inventoryDeletionSummaryItem_count,

    -- ** InventoryFilter
    inventoryFilter_type,
    inventoryFilter_key,
    inventoryFilter_values,

    -- ** InventoryGroup
    inventoryGroup_name,
    inventoryGroup_filters,

    -- ** InventoryItem
    inventoryItem_context,
    inventoryItem_content,
    inventoryItem_contentHash,
    inventoryItem_typeName,
    inventoryItem_schemaVersion,
    inventoryItem_captureTime,

    -- ** InventoryItemAttribute
    inventoryItemAttribute_name,
    inventoryItemAttribute_dataType,

    -- ** InventoryItemSchema
    inventoryItemSchema_version,
    inventoryItemSchema_displayName,
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
    maintenanceWindowExecution_status,
    maintenanceWindowExecution_statusDetails,
    maintenanceWindowExecution_startTime,
    maintenanceWindowExecution_endTime,
    maintenanceWindowExecution_windowId,
    maintenanceWindowExecution_windowExecutionId,

    -- ** MaintenanceWindowExecutionTaskIdentity
    maintenanceWindowExecutionTaskIdentity_status,
    maintenanceWindowExecutionTaskIdentity_statusDetails,
    maintenanceWindowExecutionTaskIdentity_startTime,
    maintenanceWindowExecutionTaskIdentity_endTime,
    maintenanceWindowExecutionTaskIdentity_windowExecutionId,
    maintenanceWindowExecutionTaskIdentity_taskArn,
    maintenanceWindowExecutionTaskIdentity_taskType,
    maintenanceWindowExecutionTaskIdentity_taskExecutionId,

    -- ** MaintenanceWindowExecutionTaskInvocationIdentity
    maintenanceWindowExecutionTaskInvocationIdentity_status,
    maintenanceWindowExecutionTaskInvocationIdentity_statusDetails,
    maintenanceWindowExecutionTaskInvocationIdentity_windowTargetId,
    maintenanceWindowExecutionTaskInvocationIdentity_startTime,
    maintenanceWindowExecutionTaskInvocationIdentity_endTime,
    maintenanceWindowExecutionTaskInvocationIdentity_executionId,
    maintenanceWindowExecutionTaskInvocationIdentity_windowExecutionId,
    maintenanceWindowExecutionTaskInvocationIdentity_ownerInformation,
    maintenanceWindowExecutionTaskInvocationIdentity_taskType,
    maintenanceWindowExecutionTaskInvocationIdentity_invocationId,
    maintenanceWindowExecutionTaskInvocationIdentity_parameters,
    maintenanceWindowExecutionTaskInvocationIdentity_taskExecutionId,

    -- ** MaintenanceWindowFilter
    maintenanceWindowFilter_key,
    maintenanceWindowFilter_values,

    -- ** MaintenanceWindowIdentity
    maintenanceWindowIdentity_startDate,
    maintenanceWindowIdentity_duration,
    maintenanceWindowIdentity_scheduleOffset,
    maintenanceWindowIdentity_enabled,
    maintenanceWindowIdentity_cutoff,
    maintenanceWindowIdentity_name,
    maintenanceWindowIdentity_windowId,
    maintenanceWindowIdentity_description,
    maintenanceWindowIdentity_scheduleTimezone,
    maintenanceWindowIdentity_endDate,
    maintenanceWindowIdentity_nextExecutionTime,
    maintenanceWindowIdentity_schedule,

    -- ** MaintenanceWindowIdentityForTarget
    maintenanceWindowIdentityForTarget_name,
    maintenanceWindowIdentityForTarget_windowId,

    -- ** MaintenanceWindowLambdaParameters
    maintenanceWindowLambdaParameters_payload,
    maintenanceWindowLambdaParameters_qualifier,
    maintenanceWindowLambdaParameters_clientContext,

    -- ** MaintenanceWindowRunCommandParameters
    maintenanceWindowRunCommandParameters_notificationConfig,
    maintenanceWindowRunCommandParameters_serviceRoleArn,
    maintenanceWindowRunCommandParameters_outputS3BucketName,
    maintenanceWindowRunCommandParameters_comment,
    maintenanceWindowRunCommandParameters_documentHash,
    maintenanceWindowRunCommandParameters_outputS3KeyPrefix,
    maintenanceWindowRunCommandParameters_timeoutSeconds,
    maintenanceWindowRunCommandParameters_cloudWatchOutputConfig,
    maintenanceWindowRunCommandParameters_documentHashType,
    maintenanceWindowRunCommandParameters_documentVersion,
    maintenanceWindowRunCommandParameters_parameters,

    -- ** MaintenanceWindowStepFunctionsParameters
    maintenanceWindowStepFunctionsParameters_input,
    maintenanceWindowStepFunctionsParameters_name,

    -- ** MaintenanceWindowTarget
    maintenanceWindowTarget_windowTargetId,
    maintenanceWindowTarget_resourceType,
    maintenanceWindowTarget_targets,
    maintenanceWindowTarget_name,
    maintenanceWindowTarget_windowId,
    maintenanceWindowTarget_description,
    maintenanceWindowTarget_ownerInformation,

    -- ** MaintenanceWindowTask
    maintenanceWindowTask_maxErrors,
    maintenanceWindowTask_taskParameters,
    maintenanceWindowTask_windowTaskId,
    maintenanceWindowTask_serviceRoleArn,
    maintenanceWindowTask_priority,
    maintenanceWindowTask_targets,
    maintenanceWindowTask_name,
    maintenanceWindowTask_maxConcurrency,
    maintenanceWindowTask_windowId,
    maintenanceWindowTask_description,
    maintenanceWindowTask_type,
    maintenanceWindowTask_taskArn,
    maintenanceWindowTask_loggingInfo,

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
    nonCompliantSummary_severitySummary,
    nonCompliantSummary_nonCompliantCount,

    -- ** NotificationConfig
    notificationConfig_notificationArn,
    notificationConfig_notificationType,
    notificationConfig_notificationEvents,

    -- ** OpsAggregator
    opsAggregator_typeName,
    opsAggregator_attributeName,
    opsAggregator_values,
    opsAggregator_aggregatorType,
    opsAggregator_filters,
    opsAggregator_aggregators,

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
    opsItem_status,
    opsItem_plannedEndTime,
    opsItem_severity,
    opsItem_actualStartTime,
    opsItem_category,
    opsItem_operationalData,
    opsItem_title,
    opsItem_source,
    opsItem_createdTime,
    opsItem_version,
    opsItem_priority,
    opsItem_actualEndTime,
    opsItem_opsItemId,
    opsItem_opsItemType,
    opsItem_plannedStartTime,
    opsItem_lastModifiedTime,
    opsItem_notifications,
    opsItem_description,
    opsItem_createdBy,
    opsItem_lastModifiedBy,
    opsItem_relatedOpsItems,

    -- ** OpsItemDataValue
    opsItemDataValue_value,
    opsItemDataValue_type,

    -- ** OpsItemEventFilter
    opsItemEventFilter_key,
    opsItemEventFilter_values,
    opsItemEventFilter_operator,

    -- ** OpsItemEventSummary
    opsItemEventSummary_eventId,
    opsItemEventSummary_detailType,
    opsItemEventSummary_source,
    opsItemEventSummary_createdTime,
    opsItemEventSummary_opsItemId,
    opsItemEventSummary_detail,
    opsItemEventSummary_createdBy,

    -- ** OpsItemFilter
    opsItemFilter_key,
    opsItemFilter_values,
    opsItemFilter_operator,

    -- ** OpsItemIdentity
    opsItemIdentity_arn,

    -- ** OpsItemNotification
    opsItemNotification_arn,

    -- ** OpsItemSummary
    opsItemSummary_status,
    opsItemSummary_plannedEndTime,
    opsItemSummary_severity,
    opsItemSummary_actualStartTime,
    opsItemSummary_category,
    opsItemSummary_operationalData,
    opsItemSummary_title,
    opsItemSummary_source,
    opsItemSummary_createdTime,
    opsItemSummary_priority,
    opsItemSummary_actualEndTime,
    opsItemSummary_opsItemId,
    opsItemSummary_opsItemType,
    opsItemSummary_plannedStartTime,
    opsItemSummary_lastModifiedTime,
    opsItemSummary_createdBy,
    opsItemSummary_lastModifiedBy,

    -- ** OpsMetadata
    opsMetadata_resourceId,
    opsMetadata_lastModifiedDate,
    opsMetadata_opsMetadataArn,
    opsMetadata_creationDate,
    opsMetadata_lastModifiedUser,

    -- ** OpsMetadataFilter
    opsMetadataFilter_key,
    opsMetadataFilter_values,

    -- ** OpsResultAttribute
    opsResultAttribute_typeName,

    -- ** OutputSource
    outputSource_outputSourceId,
    outputSource_outputSourceType,

    -- ** Parameter
    parameter_lastModifiedDate,
    parameter_arn,
    parameter_version,
    parameter_name,
    parameter_sourceResult,
    parameter_value,
    parameter_type,
    parameter_dataType,
    parameter_selector,

    -- ** ParameterHistory
    parameterHistory_lastModifiedDate,
    parameterHistory_policies,
    parameterHistory_labels,
    parameterHistory_version,
    parameterHistory_name,
    parameterHistory_description,
    parameterHistory_value,
    parameterHistory_type,
    parameterHistory_dataType,
    parameterHistory_allowedPattern,
    parameterHistory_lastModifiedUser,
    parameterHistory_tier,
    parameterHistory_keyId,

    -- ** ParameterInlinePolicy
    parameterInlinePolicy_policyType,
    parameterInlinePolicy_policyText,
    parameterInlinePolicy_policyStatus,

    -- ** ParameterMetadata
    parameterMetadata_lastModifiedDate,
    parameterMetadata_policies,
    parameterMetadata_version,
    parameterMetadata_name,
    parameterMetadata_description,
    parameterMetadata_type,
    parameterMetadata_dataType,
    parameterMetadata_allowedPattern,
    parameterMetadata_lastModifiedUser,
    parameterMetadata_tier,
    parameterMetadata_keyId,

    -- ** ParameterStringFilter
    parameterStringFilter_values,
    parameterStringFilter_option,
    parameterStringFilter_key,

    -- ** ParametersFilter
    parametersFilter_key,
    parametersFilter_values,

    -- ** Patch
    patch_msrcSeverity,
    patch_vendor,
    patch_epoch,
    patch_product,
    patch_severity,
    patch_title,
    patch_id,
    patch_productFamily,
    patch_version,
    patch_repository,
    patch_name,
    patch_bugzillaIds,
    patch_msrcNumber,
    patch_release,
    patch_cVEIds,
    patch_classification,
    patch_description,
    patch_advisoryIds,
    patch_arch,
    patch_releaseDate,
    patch_language,
    patch_kbNumber,
    patch_contentUrl,

    -- ** PatchBaselineIdentity
    patchBaselineIdentity_baselineName,
    patchBaselineIdentity_baselineId,
    patchBaselineIdentity_defaultBaseline,
    patchBaselineIdentity_baselineDescription,
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
    progressCounters_timedOutSteps,
    progressCounters_totalSteps,
    progressCounters_successSteps,
    progressCounters_failedSteps,

    -- ** RelatedOpsItem
    relatedOpsItem_opsItemId,

    -- ** ResolvedTargets
    resolvedTargets_parameterValues,
    resolvedTargets_truncated,

    -- ** ResourceComplianceSummaryItem
    resourceComplianceSummaryItem_resourceId,
    resourceComplianceSummaryItem_status,
    resourceComplianceSummaryItem_overallSeverity,
    resourceComplianceSummaryItem_compliantSummary,
    resourceComplianceSummaryItem_complianceType,
    resourceComplianceSummaryItem_resourceType,
    resourceComplianceSummaryItem_nonCompliantSummary,
    resourceComplianceSummaryItem_executionSummary,

    -- ** ResourceDataSyncAwsOrganizationsSource
    resourceDataSyncAwsOrganizationsSource_organizationalUnits,
    resourceDataSyncAwsOrganizationsSource_organizationSourceType,

    -- ** ResourceDataSyncDestinationDataSharing
    resourceDataSyncDestinationDataSharing_destinationDataSharingType,

    -- ** ResourceDataSyncItem
    resourceDataSyncItem_syncType,
    resourceDataSyncItem_s3Destination,
    resourceDataSyncItem_syncLastModifiedTime,
    resourceDataSyncItem_lastSyncTime,
    resourceDataSyncItem_syncName,
    resourceDataSyncItem_syncSource,
    resourceDataSyncItem_lastSuccessfulSyncTime,
    resourceDataSyncItem_lastStatus,
    resourceDataSyncItem_lastSyncStatusMessage,
    resourceDataSyncItem_syncCreatedTime,

    -- ** ResourceDataSyncOrganizationalUnit
    resourceDataSyncOrganizationalUnit_organizationalUnitId,

    -- ** ResourceDataSyncS3Destination
    resourceDataSyncS3Destination_prefix,
    resourceDataSyncS3Destination_destinationDataSharing,
    resourceDataSyncS3Destination_aWSKMSKeyARN,
    resourceDataSyncS3Destination_bucketName,
    resourceDataSyncS3Destination_syncFormat,
    resourceDataSyncS3Destination_region,

    -- ** ResourceDataSyncSource
    resourceDataSyncSource_includeFutureRegions,
    resourceDataSyncSource_awsOrganizationsSource,
    resourceDataSyncSource_sourceType,
    resourceDataSyncSource_sourceRegions,

    -- ** ResourceDataSyncSourceWithState
    resourceDataSyncSourceWithState_includeFutureRegions,
    resourceDataSyncSourceWithState_state,
    resourceDataSyncSourceWithState_sourceRegions,
    resourceDataSyncSourceWithState_awsOrganizationsSource,
    resourceDataSyncSourceWithState_sourceType,

    -- ** ResultAttribute
    resultAttribute_typeName,

    -- ** ReviewInformation
    reviewInformation_status,
    reviewInformation_reviewedTime,
    reviewInformation_reviewer,

    -- ** Runbook
    runbook_maxErrors,
    runbook_targets,
    runbook_targetLocations,
    runbook_targetParameterName,
    runbook_maxConcurrency,
    runbook_documentVersion,
    runbook_parameters,
    runbook_documentName,

    -- ** S3OutputLocation
    s3OutputLocation_outputS3BucketName,
    s3OutputLocation_outputS3Region,
    s3OutputLocation_outputS3KeyPrefix,

    -- ** S3OutputUrl
    s3OutputUrl_outputUrl,

    -- ** ScheduledWindowExecution
    scheduledWindowExecution_executionTime,
    scheduledWindowExecution_name,
    scheduledWindowExecution_windowId,

    -- ** ServiceSetting
    serviceSetting_lastModifiedDate,
    serviceSetting_status,
    serviceSetting_arn,
    serviceSetting_settingValue,
    serviceSetting_settingId,
    serviceSetting_lastModifiedUser,

    -- ** Session
    session_status,
    session_startDate,
    session_sessionId,
    session_documentName,
    session_details,
    session_outputUrl,
    session_target,
    session_owner,
    session_endDate,

    -- ** SessionFilter
    sessionFilter_key,
    sessionFilter_value,

    -- ** SessionManagerOutputUrl
    sessionManagerOutputUrl_s3OutputUrl,
    sessionManagerOutputUrl_cloudWatchOutputUrl,

    -- ** SeveritySummary
    severitySummary_lowCount,
    severitySummary_mediumCount,
    severitySummary_criticalCount,
    severitySummary_highCount,
    severitySummary_unspecifiedCount,
    severitySummary_informationalCount,

    -- ** StepExecution
    stepExecution_outputs,
    stepExecution_onFailure,
    stepExecution_response,
    stepExecution_failureMessage,
    stepExecution_executionEndTime,
    stepExecution_nextStep,
    stepExecution_isEnd,
    stepExecution_maxAttempts,
    stepExecution_failureDetails,
    stepExecution_targets,
    stepExecution_executionStartTime,
    stepExecution_targetLocation,
    stepExecution_overriddenParameters,
    stepExecution_isCritical,
    stepExecution_responseCode,
    stepExecution_stepStatus,
    stepExecution_action,
    stepExecution_validNextSteps,
    stepExecution_timeoutSeconds,
    stepExecution_inputs,
    stepExecution_stepExecutionId,
    stepExecution_stepName,

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
    targetLocation_executionRoleName,
    targetLocation_accounts,
    targetLocation_regions,
    targetLocation_targetLocationMaxErrors,
    targetLocation_targetLocationMaxConcurrency,
  )
where

import Network.AWS.SSM.AddTagsToResource
import Network.AWS.SSM.CancelCommand
import Network.AWS.SSM.CancelMaintenanceWindowExecution
import Network.AWS.SSM.CreateActivation
import Network.AWS.SSM.CreateAssociation
import Network.AWS.SSM.CreateAssociationBatch
import Network.AWS.SSM.CreateDocument
import Network.AWS.SSM.CreateMaintenanceWindow
import Network.AWS.SSM.CreateOpsItem
import Network.AWS.SSM.CreateOpsMetadata
import Network.AWS.SSM.CreatePatchBaseline
import Network.AWS.SSM.CreateResourceDataSync
import Network.AWS.SSM.DeleteActivation
import Network.AWS.SSM.DeleteAssociation
import Network.AWS.SSM.DeleteDocument
import Network.AWS.SSM.DeleteInventory
import Network.AWS.SSM.DeleteMaintenanceWindow
import Network.AWS.SSM.DeleteOpsMetadata
import Network.AWS.SSM.DeleteParameter
import Network.AWS.SSM.DeleteParameters
import Network.AWS.SSM.DeletePatchBaseline
import Network.AWS.SSM.DeleteResourceDataSync
import Network.AWS.SSM.DeregisterManagedInstance
import Network.AWS.SSM.DeregisterPatchBaselineForPatchGroup
import Network.AWS.SSM.DeregisterTargetFromMaintenanceWindow
import Network.AWS.SSM.DeregisterTaskFromMaintenanceWindow
import Network.AWS.SSM.DescribeActivations
import Network.AWS.SSM.DescribeAssociation
import Network.AWS.SSM.DescribeAssociationExecutionTargets
import Network.AWS.SSM.DescribeAssociationExecutions
import Network.AWS.SSM.DescribeAutomationExecutions
import Network.AWS.SSM.DescribeAutomationStepExecutions
import Network.AWS.SSM.DescribeAvailablePatches
import Network.AWS.SSM.DescribeDocument
import Network.AWS.SSM.DescribeDocumentPermission
import Network.AWS.SSM.DescribeEffectiveInstanceAssociations
import Network.AWS.SSM.DescribeEffectivePatchesForPatchBaseline
import Network.AWS.SSM.DescribeInstanceAssociationsStatus
import Network.AWS.SSM.DescribeInstanceInformation
import Network.AWS.SSM.DescribeInstancePatchStates
import Network.AWS.SSM.DescribeInstancePatchStatesForPatchGroup
import Network.AWS.SSM.DescribeInstancePatches
import Network.AWS.SSM.DescribeInventoryDeletions
import Network.AWS.SSM.DescribeMaintenanceWindowExecutionTaskInvocations
import Network.AWS.SSM.DescribeMaintenanceWindowExecutionTasks
import Network.AWS.SSM.DescribeMaintenanceWindowExecutions
import Network.AWS.SSM.DescribeMaintenanceWindowSchedule
import Network.AWS.SSM.DescribeMaintenanceWindowTargets
import Network.AWS.SSM.DescribeMaintenanceWindowTasks
import Network.AWS.SSM.DescribeMaintenanceWindows
import Network.AWS.SSM.DescribeMaintenanceWindowsForTarget
import Network.AWS.SSM.DescribeOpsItems
import Network.AWS.SSM.DescribeParameters
import Network.AWS.SSM.DescribePatchBaselines
import Network.AWS.SSM.DescribePatchGroupState
import Network.AWS.SSM.DescribePatchGroups
import Network.AWS.SSM.DescribePatchProperties
import Network.AWS.SSM.DescribeSessions
import Network.AWS.SSM.GetAutomationExecution
import Network.AWS.SSM.GetCalendarState
import Network.AWS.SSM.GetCommandInvocation
import Network.AWS.SSM.GetConnectionStatus
import Network.AWS.SSM.GetDefaultPatchBaseline
import Network.AWS.SSM.GetDeployablePatchSnapshotForInstance
import Network.AWS.SSM.GetDocument
import Network.AWS.SSM.GetInventory
import Network.AWS.SSM.GetInventorySchema
import Network.AWS.SSM.GetMaintenanceWindow
import Network.AWS.SSM.GetMaintenanceWindowExecution
import Network.AWS.SSM.GetMaintenanceWindowExecutionTask
import Network.AWS.SSM.GetMaintenanceWindowExecutionTaskInvocation
import Network.AWS.SSM.GetMaintenanceWindowTask
import Network.AWS.SSM.GetOpsItem
import Network.AWS.SSM.GetOpsMetadata
import Network.AWS.SSM.GetOpsSummary
import Network.AWS.SSM.GetParameter
import Network.AWS.SSM.GetParameterHistory
import Network.AWS.SSM.GetParameters
import Network.AWS.SSM.GetParametersByPath
import Network.AWS.SSM.GetPatchBaseline
import Network.AWS.SSM.GetPatchBaselineForPatchGroup
import Network.AWS.SSM.GetServiceSetting
import Network.AWS.SSM.LabelParameterVersion
import Network.AWS.SSM.ListAssociationVersions
import Network.AWS.SSM.ListAssociations
import Network.AWS.SSM.ListCommandInvocations
import Network.AWS.SSM.ListCommands
import Network.AWS.SSM.ListComplianceItems
import Network.AWS.SSM.ListComplianceSummaries
import Network.AWS.SSM.ListDocumentMetadataHistory
import Network.AWS.SSM.ListDocumentVersions
import Network.AWS.SSM.ListDocuments
import Network.AWS.SSM.ListInventoryEntries
import Network.AWS.SSM.ListOpsItemEvents
import Network.AWS.SSM.ListOpsMetadata
import Network.AWS.SSM.ListResourceComplianceSummaries
import Network.AWS.SSM.ListResourceDataSync
import Network.AWS.SSM.ListTagsForResource
import Network.AWS.SSM.ModifyDocumentPermission
import Network.AWS.SSM.PutComplianceItems
import Network.AWS.SSM.PutInventory
import Network.AWS.SSM.PutParameter
import Network.AWS.SSM.RegisterDefaultPatchBaseline
import Network.AWS.SSM.RegisterPatchBaselineForPatchGroup
import Network.AWS.SSM.RegisterTargetWithMaintenanceWindow
import Network.AWS.SSM.RegisterTaskWithMaintenanceWindow
import Network.AWS.SSM.RemoveTagsFromResource
import Network.AWS.SSM.ResetServiceSetting
import Network.AWS.SSM.ResumeSession
import Network.AWS.SSM.SendAutomationSignal
import Network.AWS.SSM.SendCommand
import Network.AWS.SSM.StartAssociationsOnce
import Network.AWS.SSM.StartAutomationExecution
import Network.AWS.SSM.StartChangeRequestExecution
import Network.AWS.SSM.StartSession
import Network.AWS.SSM.StopAutomationExecution
import Network.AWS.SSM.TerminateSession
import Network.AWS.SSM.Types.AccountSharingInfo
import Network.AWS.SSM.Types.Activation
import Network.AWS.SSM.Types.Association
import Network.AWS.SSM.Types.AssociationDescription
import Network.AWS.SSM.Types.AssociationExecution
import Network.AWS.SSM.Types.AssociationExecutionFilter
import Network.AWS.SSM.Types.AssociationExecutionTarget
import Network.AWS.SSM.Types.AssociationExecutionTargetsFilter
import Network.AWS.SSM.Types.AssociationFilter
import Network.AWS.SSM.Types.AssociationOverview
import Network.AWS.SSM.Types.AssociationStatus
import Network.AWS.SSM.Types.AssociationVersionInfo
import Network.AWS.SSM.Types.AttachmentContent
import Network.AWS.SSM.Types.AttachmentInformation
import Network.AWS.SSM.Types.AttachmentsSource
import Network.AWS.SSM.Types.AutomationExecution
import Network.AWS.SSM.Types.AutomationExecutionFilter
import Network.AWS.SSM.Types.AutomationExecutionMetadata
import Network.AWS.SSM.Types.BaselineOverride
import Network.AWS.SSM.Types.CloudWatchOutputConfig
import Network.AWS.SSM.Types.Command
import Network.AWS.SSM.Types.CommandFilter
import Network.AWS.SSM.Types.CommandInvocation
import Network.AWS.SSM.Types.CommandPlugin
import Network.AWS.SSM.Types.ComplianceExecutionSummary
import Network.AWS.SSM.Types.ComplianceItem
import Network.AWS.SSM.Types.ComplianceItemEntry
import Network.AWS.SSM.Types.ComplianceStringFilter
import Network.AWS.SSM.Types.ComplianceSummaryItem
import Network.AWS.SSM.Types.CompliantSummary
import Network.AWS.SSM.Types.CreateAssociationBatchRequestEntry
import Network.AWS.SSM.Types.DescribeActivationsFilter
import Network.AWS.SSM.Types.DocumentDefaultVersionDescription
import Network.AWS.SSM.Types.DocumentDescription
import Network.AWS.SSM.Types.DocumentFilter
import Network.AWS.SSM.Types.DocumentIdentifier
import Network.AWS.SSM.Types.DocumentKeyValuesFilter
import Network.AWS.SSM.Types.DocumentMetadataResponseInfo
import Network.AWS.SSM.Types.DocumentParameter
import Network.AWS.SSM.Types.DocumentRequires
import Network.AWS.SSM.Types.DocumentReviewCommentSource
import Network.AWS.SSM.Types.DocumentReviewerResponseSource
import Network.AWS.SSM.Types.DocumentReviews
import Network.AWS.SSM.Types.DocumentVersionInfo
import Network.AWS.SSM.Types.EffectivePatch
import Network.AWS.SSM.Types.FailedCreateAssociation
import Network.AWS.SSM.Types.FailureDetails
import Network.AWS.SSM.Types.InstanceAggregatedAssociationOverview
import Network.AWS.SSM.Types.InstanceAssociation
import Network.AWS.SSM.Types.InstanceAssociationOutputLocation
import Network.AWS.SSM.Types.InstanceAssociationOutputUrl
import Network.AWS.SSM.Types.InstanceAssociationStatusInfo
import Network.AWS.SSM.Types.InstanceInformation
import Network.AWS.SSM.Types.InstanceInformationFilter
import Network.AWS.SSM.Types.InstanceInformationStringFilter
import Network.AWS.SSM.Types.InstancePatchState
import Network.AWS.SSM.Types.InstancePatchStateFilter
import Network.AWS.SSM.Types.InventoryAggregator
import Network.AWS.SSM.Types.InventoryDeletionStatusItem
import Network.AWS.SSM.Types.InventoryDeletionSummary
import Network.AWS.SSM.Types.InventoryDeletionSummaryItem
import Network.AWS.SSM.Types.InventoryFilter
import Network.AWS.SSM.Types.InventoryGroup
import Network.AWS.SSM.Types.InventoryItem
import Network.AWS.SSM.Types.InventoryItemAttribute
import Network.AWS.SSM.Types.InventoryItemSchema
import Network.AWS.SSM.Types.InventoryResultEntity
import Network.AWS.SSM.Types.InventoryResultItem
import Network.AWS.SSM.Types.LoggingInfo
import Network.AWS.SSM.Types.MaintenanceWindowAutomationParameters
import Network.AWS.SSM.Types.MaintenanceWindowExecution
import Network.AWS.SSM.Types.MaintenanceWindowExecutionTaskIdentity
import Network.AWS.SSM.Types.MaintenanceWindowExecutionTaskInvocationIdentity
import Network.AWS.SSM.Types.MaintenanceWindowFilter
import Network.AWS.SSM.Types.MaintenanceWindowIdentity
import Network.AWS.SSM.Types.MaintenanceWindowIdentityForTarget
import Network.AWS.SSM.Types.MaintenanceWindowLambdaParameters
import Network.AWS.SSM.Types.MaintenanceWindowRunCommandParameters
import Network.AWS.SSM.Types.MaintenanceWindowStepFunctionsParameters
import Network.AWS.SSM.Types.MaintenanceWindowTarget
import Network.AWS.SSM.Types.MaintenanceWindowTask
import Network.AWS.SSM.Types.MaintenanceWindowTaskInvocationParameters
import Network.AWS.SSM.Types.MaintenanceWindowTaskParameterValueExpression
import Network.AWS.SSM.Types.MetadataValue
import Network.AWS.SSM.Types.NonCompliantSummary
import Network.AWS.SSM.Types.NotificationConfig
import Network.AWS.SSM.Types.OpsAggregator
import Network.AWS.SSM.Types.OpsEntity
import Network.AWS.SSM.Types.OpsEntityItem
import Network.AWS.SSM.Types.OpsFilter
import Network.AWS.SSM.Types.OpsItem
import Network.AWS.SSM.Types.OpsItemDataValue
import Network.AWS.SSM.Types.OpsItemEventFilter
import Network.AWS.SSM.Types.OpsItemEventSummary
import Network.AWS.SSM.Types.OpsItemFilter
import Network.AWS.SSM.Types.OpsItemIdentity
import Network.AWS.SSM.Types.OpsItemNotification
import Network.AWS.SSM.Types.OpsItemSummary
import Network.AWS.SSM.Types.OpsMetadata
import Network.AWS.SSM.Types.OpsMetadataFilter
import Network.AWS.SSM.Types.OpsResultAttribute
import Network.AWS.SSM.Types.OutputSource
import Network.AWS.SSM.Types.Parameter
import Network.AWS.SSM.Types.ParameterHistory
import Network.AWS.SSM.Types.ParameterInlinePolicy
import Network.AWS.SSM.Types.ParameterMetadata
import Network.AWS.SSM.Types.ParameterStringFilter
import Network.AWS.SSM.Types.ParametersFilter
import Network.AWS.SSM.Types.Patch
import Network.AWS.SSM.Types.PatchBaselineIdentity
import Network.AWS.SSM.Types.PatchComplianceData
import Network.AWS.SSM.Types.PatchFilter
import Network.AWS.SSM.Types.PatchFilterGroup
import Network.AWS.SSM.Types.PatchGroupPatchBaselineMapping
import Network.AWS.SSM.Types.PatchOrchestratorFilter
import Network.AWS.SSM.Types.PatchRule
import Network.AWS.SSM.Types.PatchRuleGroup
import Network.AWS.SSM.Types.PatchSource
import Network.AWS.SSM.Types.PatchStatus
import Network.AWS.SSM.Types.ProgressCounters
import Network.AWS.SSM.Types.RelatedOpsItem
import Network.AWS.SSM.Types.ResolvedTargets
import Network.AWS.SSM.Types.ResourceComplianceSummaryItem
import Network.AWS.SSM.Types.ResourceDataSyncAwsOrganizationsSource
import Network.AWS.SSM.Types.ResourceDataSyncDestinationDataSharing
import Network.AWS.SSM.Types.ResourceDataSyncItem
import Network.AWS.SSM.Types.ResourceDataSyncOrganizationalUnit
import Network.AWS.SSM.Types.ResourceDataSyncS3Destination
import Network.AWS.SSM.Types.ResourceDataSyncSource
import Network.AWS.SSM.Types.ResourceDataSyncSourceWithState
import Network.AWS.SSM.Types.ResultAttribute
import Network.AWS.SSM.Types.ReviewInformation
import Network.AWS.SSM.Types.Runbook
import Network.AWS.SSM.Types.S3OutputLocation
import Network.AWS.SSM.Types.S3OutputUrl
import Network.AWS.SSM.Types.ScheduledWindowExecution
import Network.AWS.SSM.Types.ServiceSetting
import Network.AWS.SSM.Types.Session
import Network.AWS.SSM.Types.SessionFilter
import Network.AWS.SSM.Types.SessionManagerOutputUrl
import Network.AWS.SSM.Types.SeveritySummary
import Network.AWS.SSM.Types.StepExecution
import Network.AWS.SSM.Types.StepExecutionFilter
import Network.AWS.SSM.Types.Tag
import Network.AWS.SSM.Types.Target
import Network.AWS.SSM.Types.TargetLocation
import Network.AWS.SSM.UpdateAssociation
import Network.AWS.SSM.UpdateAssociationStatus
import Network.AWS.SSM.UpdateDocument
import Network.AWS.SSM.UpdateDocumentDefaultVersion
import Network.AWS.SSM.UpdateDocumentMetadata
import Network.AWS.SSM.UpdateMaintenanceWindow
import Network.AWS.SSM.UpdateMaintenanceWindowTarget
import Network.AWS.SSM.UpdateMaintenanceWindowTask
import Network.AWS.SSM.UpdateManagedInstanceRole
import Network.AWS.SSM.UpdateOpsItem
import Network.AWS.SSM.UpdateOpsMetadata
import Network.AWS.SSM.UpdatePatchBaseline
import Network.AWS.SSM.UpdateResourceDataSync
import Network.AWS.SSM.UpdateServiceSetting
