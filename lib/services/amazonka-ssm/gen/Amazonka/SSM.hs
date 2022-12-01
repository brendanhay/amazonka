{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SSM
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2014-11-06@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Web Services Systems Manager is a collection of capabilities to
-- help you manage your applications and infrastructure running in the
-- Amazon Web Services Cloud;. Systems Manager simplifies application and
-- resource management, shortens the time to detect and resolve operational
-- problems, and helps you manage your Amazon Web Services resources
-- securely at scale.
--
-- This reference is intended to be used with the
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/ Amazon Web Services Systems Manager User Guide>.
--
-- To get started, verify prerequisites. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-setting-up.html Setting up Amazon Web Services Systems Manager>.
--
-- __Related resources__
--
-- -   For information about how to use a Query API, see
--     <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/making-api-requests.html Making API requests>.
--
-- -   For information about other API operations you can perform on EC2
--     instances, see the
--     <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ Amazon EC2 API Reference>.
--
-- -   For information about AppConfig, a capability of Systems Manager,
--     see the
--     <https://docs.aws.amazon.com/appconfig/latest/userguide/ AppConfig User Guide>
--     and the
--     <https://docs.aws.amazon.com/appconfig/2019-10-09/APIReference/ AppConfig API Reference>.
--
-- -   For information about Incident Manager, a capability of Systems
--     Manager, see the
--     <https://docs.aws.amazon.com/incident-manager/latest/userguide/ Incident Manager User Guide>
--     and the
--     <https://docs.aws.amazon.com/incident-manager/latest/APIReference/ Incident Manager API Reference>.
module Amazonka.SSM
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** CustomSchemaCountLimitExceededException
    _CustomSchemaCountLimitExceededException,

    -- ** InvalidAggregatorException
    _InvalidAggregatorException,

    -- ** InvocationDoesNotExist
    _InvocationDoesNotExist,

    -- ** OpsMetadataInvalidArgumentException
    _OpsMetadataInvalidArgumentException,

    -- ** ResourcePolicyConflictException
    _ResourcePolicyConflictException,

    -- ** InvalidFilterKey
    _InvalidFilterKey,

    -- ** OpsItemLimitExceededException
    _OpsItemLimitExceededException,

    -- ** AutomationExecutionLimitExceededException
    _AutomationExecutionLimitExceededException,

    -- ** FeatureNotAvailableException
    _FeatureNotAvailableException,

    -- ** InvalidOptionException
    _InvalidOptionException,

    -- ** DocumentPermissionLimit
    _DocumentPermissionLimit,

    -- ** ParameterLimitExceeded
    _ParameterLimitExceeded,

    -- ** InvalidDocumentContent
    _InvalidDocumentContent,

    -- ** ParameterAlreadyExists
    _ParameterAlreadyExists,

    -- ** InvalidTypeNameException
    _InvalidTypeNameException,

    -- ** InvalidResourceId
    _InvalidResourceId,

    -- ** OpsItemAccessDeniedException
    _OpsItemAccessDeniedException,

    -- ** ResourceDataSyncAlreadyExistsException
    _ResourceDataSyncAlreadyExistsException,

    -- ** DocumentLimitExceeded
    _DocumentLimitExceeded,

    -- ** OpsItemRelatedItemAlreadyExistsException
    _OpsItemRelatedItemAlreadyExistsException,

    -- ** InvalidAutomationExecutionParametersException
    _InvalidAutomationExecutionParametersException,

    -- ** DocumentAlreadyExists
    _DocumentAlreadyExists,

    -- ** DocumentVersionLimitExceeded
    _DocumentVersionLimitExceeded,

    -- ** InvalidPolicyAttributeException
    _InvalidPolicyAttributeException,

    -- ** UnsupportedPlatformType
    _UnsupportedPlatformType,

    -- ** InvalidDocumentType
    _InvalidDocumentType,

    -- ** IncompatiblePolicyException
    _IncompatiblePolicyException,

    -- ** AssociationAlreadyExists
    _AssociationAlreadyExists,

    -- ** MaxDocumentSizeExceeded
    _MaxDocumentSizeExceeded,

    -- ** InvalidDeleteInventoryParametersException
    _InvalidDeleteInventoryParametersException,

    -- ** InvalidPluginName
    _InvalidPluginName,

    -- ** StatusUnchanged
    _StatusUnchanged,

    -- ** ResourceDataSyncInvalidConfigurationException
    _ResourceDataSyncInvalidConfigurationException,

    -- ** InvalidAllowedPatternException
    _InvalidAllowedPatternException,

    -- ** AssociationExecutionDoesNotExist
    _AssociationExecutionDoesNotExist,

    -- ** AutomationDefinitionVersionNotFoundException
    _AutomationDefinitionVersionNotFoundException,

    -- ** AutomationExecutionNotFoundException
    _AutomationExecutionNotFoundException,

    -- ** OpsItemNotFoundException
    _OpsItemNotFoundException,

    -- ** InvalidNextToken
    _InvalidNextToken,

    -- ** TooManyTagsError
    _TooManyTagsError,

    -- ** InvalidUpdate
    _InvalidUpdate,

    -- ** OpsMetadataKeyLimitExceededException
    _OpsMetadataKeyLimitExceededException,

    -- ** InvalidFilterOption
    _InvalidFilterOption,

    -- ** ItemContentMismatchException
    _ItemContentMismatchException,

    -- ** UnsupportedCalendarException
    _UnsupportedCalendarException,

    -- ** InvalidPermissionType
    _InvalidPermissionType,

    -- ** OpsMetadataTooManyUpdatesException
    _OpsMetadataTooManyUpdatesException,

    -- ** ParameterVersionLabelLimitExceeded
    _ParameterVersionLabelLimitExceeded,

    -- ** TargetNotConnected
    _TargetNotConnected,

    -- ** ParameterMaxVersionLimitExceeded
    _ParameterMaxVersionLimitExceeded,

    -- ** DuplicateDocumentContent
    _DuplicateDocumentContent,

    -- ** InvalidActivationId
    _InvalidActivationId,

    -- ** InvalidAutomationStatusUpdateException
    _InvalidAutomationStatusUpdateException,

    -- ** InvalidInventoryItemContextException
    _InvalidInventoryItemContextException,

    -- ** ItemSizeLimitExceededException
    _ItemSizeLimitExceededException,

    -- ** AssociationDoesNotExist
    _AssociationDoesNotExist,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** AlreadyExistsException
    _AlreadyExistsException,

    -- ** ResourcePolicyLimitExceededException
    _ResourcePolicyLimitExceededException,

    -- ** AssociationVersionLimitExceeded
    _AssociationVersionLimitExceeded,

    -- ** InvalidFilterValue
    _InvalidFilterValue,

    -- ** ParameterVersionNotFound
    _ParameterVersionNotFound,

    -- ** AutomationDefinitionNotFoundException
    _AutomationDefinitionNotFoundException,

    -- ** ResourceLimitExceededException
    _ResourceLimitExceededException,

    -- ** ParameterPatternMismatchException
    _ParameterPatternMismatchException,

    -- ** InvalidAssociationVersion
    _InvalidAssociationVersion,

    -- ** TargetInUseException
    _TargetInUseException,

    -- ** InvalidResultAttributeException
    _InvalidResultAttributeException,

    -- ** ResourceDataSyncNotFoundException
    _ResourceDataSyncNotFoundException,

    -- ** TotalSizeLimitExceededException
    _TotalSizeLimitExceededException,

    -- ** OpsItemAlreadyExistsException
    _OpsItemAlreadyExistsException,

    -- ** InvalidItemContentException
    _InvalidItemContentException,

    -- ** InternalServerError
    _InternalServerError,

    -- ** HierarchyLevelLimitExceededException
    _HierarchyLevelLimitExceededException,

    -- ** UnsupportedParameterType
    _UnsupportedParameterType,

    -- ** InvalidInstanceInformationFilterValue
    _InvalidInstanceInformationFilterValue,

    -- ** UnsupportedInventorySchemaVersionException
    _UnsupportedInventorySchemaVersionException,

    -- ** InvalidSchedule
    _InvalidSchedule,

    -- ** InvalidInstanceId
    _InvalidInstanceId,

    -- ** HierarchyTypeMismatchException
    _HierarchyTypeMismatchException,

    -- ** InvalidKeyId
    _InvalidKeyId,

    -- ** AssociationLimitExceeded
    _AssociationLimitExceeded,

    -- ** InvalidAutomationSignalException
    _InvalidAutomationSignalException,

    -- ** OpsItemRelatedItemAssociationNotFoundException
    _OpsItemRelatedItemAssociationNotFoundException,

    -- ** InvalidOutputLocation
    _InvalidOutputLocation,

    -- ** ComplianceTypeCountLimitExceededException
    _ComplianceTypeCountLimitExceededException,

    -- ** InvalidInventoryGroupException
    _InvalidInventoryGroupException,

    -- ** ParameterNotFound
    _ParameterNotFound,

    -- ** InvalidDeletionIdException
    _InvalidDeletionIdException,

    -- ** IdempotentParameterMismatch
    _IdempotentParameterMismatch,

    -- ** InvalidInventoryRequestException
    _InvalidInventoryRequestException,

    -- ** UnsupportedFeatureRequiredException
    _UnsupportedFeatureRequiredException,

    -- ** ServiceSettingNotFound
    _ServiceSettingNotFound,

    -- ** OpsItemInvalidParameterException
    _OpsItemInvalidParameterException,

    -- ** InvalidCommandId
    _InvalidCommandId,

    -- ** ResourceDataSyncConflictException
    _ResourceDataSyncConflictException,

    -- ** InvalidDocumentOperation
    _InvalidDocumentOperation,

    -- ** AssociatedInstances
    _AssociatedInstances,

    -- ** InvalidDocument
    _InvalidDocument,

    -- ** InvalidTarget
    _InvalidTarget,

    -- ** TooManyUpdates
    _TooManyUpdates,

    -- ** AutomationDefinitionNotApprovedException
    _AutomationDefinitionNotApprovedException,

    -- ** SubTypeCountLimitExceededException
    _SubTypeCountLimitExceededException,

    -- ** InvalidRole
    _InvalidRole,

    -- ** AutomationStepNotFoundException
    _AutomationStepNotFoundException,

    -- ** InvalidPolicyTypeException
    _InvalidPolicyTypeException,

    -- ** InvalidParameters
    _InvalidParameters,

    -- ** ResourceDataSyncCountExceededException
    _ResourceDataSyncCountExceededException,

    -- ** InvalidNotificationConfig
    _InvalidNotificationConfig,

    -- ** InvalidTag
    _InvalidTag,

    -- ** InvalidDocumentSchemaVersion
    _InvalidDocumentSchemaVersion,

    -- ** OpsMetadataNotFoundException
    _OpsMetadataNotFoundException,

    -- ** InvalidOutputFolder
    _InvalidOutputFolder,

    -- ** PoliciesLimitExceededException
    _PoliciesLimitExceededException,

    -- ** DuplicateDocumentVersionName
    _DuplicateDocumentVersionName,

    -- ** UnsupportedInventoryItemContextException
    _UnsupportedInventoryItemContextException,

    -- ** InvalidActivation
    _InvalidActivation,

    -- ** DuplicateInstanceId
    _DuplicateInstanceId,

    -- ** InvalidResourceType
    _InvalidResourceType,

    -- ** InvalidDocumentVersion
    _InvalidDocumentVersion,

    -- ** InvalidFilter
    _InvalidFilter,

    -- ** InvalidAssociation
    _InvalidAssociation,

    -- ** DoesNotExistException
    _DoesNotExistException,

    -- ** OpsMetadataLimitExceededException
    _OpsMetadataLimitExceededException,

    -- ** OpsMetadataAlreadyExistsException
    _OpsMetadataAlreadyExistsException,

    -- ** UnsupportedOperatingSystem
    _UnsupportedOperatingSystem,

    -- ** InvalidTargetMaps
    _InvalidTargetMaps,

    -- ** ResourcePolicyInvalidParameterException
    _ResourcePolicyInvalidParameterException,

    -- * Waiters
    -- $waiters

    -- ** CommandExecuted
    newCommandExecuted,

    -- * Operations
    -- $operations

    -- ** AddTagsToResource
    AddTagsToResource (AddTagsToResource'),
    newAddTagsToResource,
    AddTagsToResourceResponse (AddTagsToResourceResponse'),
    newAddTagsToResourceResponse,

    -- ** AssociateOpsItemRelatedItem
    AssociateOpsItemRelatedItem (AssociateOpsItemRelatedItem'),
    newAssociateOpsItemRelatedItem,
    AssociateOpsItemRelatedItemResponse (AssociateOpsItemRelatedItemResponse'),
    newAssociateOpsItemRelatedItemResponse,

    -- ** CancelCommand
    CancelCommand (CancelCommand'),
    newCancelCommand,
    CancelCommandResponse (CancelCommandResponse'),
    newCancelCommandResponse,

    -- ** CancelMaintenanceWindowExecution
    CancelMaintenanceWindowExecution (CancelMaintenanceWindowExecution'),
    newCancelMaintenanceWindowExecution,
    CancelMaintenanceWindowExecutionResponse (CancelMaintenanceWindowExecutionResponse'),
    newCancelMaintenanceWindowExecutionResponse,

    -- ** CreateActivation
    CreateActivation (CreateActivation'),
    newCreateActivation,
    CreateActivationResponse (CreateActivationResponse'),
    newCreateActivationResponse,

    -- ** CreateAssociation
    CreateAssociation (CreateAssociation'),
    newCreateAssociation,
    CreateAssociationResponse (CreateAssociationResponse'),
    newCreateAssociationResponse,

    -- ** CreateAssociationBatch
    CreateAssociationBatch (CreateAssociationBatch'),
    newCreateAssociationBatch,
    CreateAssociationBatchResponse (CreateAssociationBatchResponse'),
    newCreateAssociationBatchResponse,

    -- ** CreateDocument
    CreateDocument (CreateDocument'),
    newCreateDocument,
    CreateDocumentResponse (CreateDocumentResponse'),
    newCreateDocumentResponse,

    -- ** CreateMaintenanceWindow
    CreateMaintenanceWindow (CreateMaintenanceWindow'),
    newCreateMaintenanceWindow,
    CreateMaintenanceWindowResponse (CreateMaintenanceWindowResponse'),
    newCreateMaintenanceWindowResponse,

    -- ** CreateOpsItem
    CreateOpsItem (CreateOpsItem'),
    newCreateOpsItem,
    CreateOpsItemResponse (CreateOpsItemResponse'),
    newCreateOpsItemResponse,

    -- ** CreateOpsMetadata
    CreateOpsMetadata (CreateOpsMetadata'),
    newCreateOpsMetadata,
    CreateOpsMetadataResponse (CreateOpsMetadataResponse'),
    newCreateOpsMetadataResponse,

    -- ** CreatePatchBaseline
    CreatePatchBaseline (CreatePatchBaseline'),
    newCreatePatchBaseline,
    CreatePatchBaselineResponse (CreatePatchBaselineResponse'),
    newCreatePatchBaselineResponse,

    -- ** CreateResourceDataSync
    CreateResourceDataSync (CreateResourceDataSync'),
    newCreateResourceDataSync,
    CreateResourceDataSyncResponse (CreateResourceDataSyncResponse'),
    newCreateResourceDataSyncResponse,

    -- ** DeleteActivation
    DeleteActivation (DeleteActivation'),
    newDeleteActivation,
    DeleteActivationResponse (DeleteActivationResponse'),
    newDeleteActivationResponse,

    -- ** DeleteAssociation
    DeleteAssociation (DeleteAssociation'),
    newDeleteAssociation,
    DeleteAssociationResponse (DeleteAssociationResponse'),
    newDeleteAssociationResponse,

    -- ** DeleteDocument
    DeleteDocument (DeleteDocument'),
    newDeleteDocument,
    DeleteDocumentResponse (DeleteDocumentResponse'),
    newDeleteDocumentResponse,

    -- ** DeleteInventory
    DeleteInventory (DeleteInventory'),
    newDeleteInventory,
    DeleteInventoryResponse (DeleteInventoryResponse'),
    newDeleteInventoryResponse,

    -- ** DeleteMaintenanceWindow
    DeleteMaintenanceWindow (DeleteMaintenanceWindow'),
    newDeleteMaintenanceWindow,
    DeleteMaintenanceWindowResponse (DeleteMaintenanceWindowResponse'),
    newDeleteMaintenanceWindowResponse,

    -- ** DeleteOpsMetadata
    DeleteOpsMetadata (DeleteOpsMetadata'),
    newDeleteOpsMetadata,
    DeleteOpsMetadataResponse (DeleteOpsMetadataResponse'),
    newDeleteOpsMetadataResponse,

    -- ** DeleteParameter
    DeleteParameter (DeleteParameter'),
    newDeleteParameter,
    DeleteParameterResponse (DeleteParameterResponse'),
    newDeleteParameterResponse,

    -- ** DeleteParameters
    DeleteParameters (DeleteParameters'),
    newDeleteParameters,
    DeleteParametersResponse (DeleteParametersResponse'),
    newDeleteParametersResponse,

    -- ** DeletePatchBaseline
    DeletePatchBaseline (DeletePatchBaseline'),
    newDeletePatchBaseline,
    DeletePatchBaselineResponse (DeletePatchBaselineResponse'),
    newDeletePatchBaselineResponse,

    -- ** DeleteResourceDataSync
    DeleteResourceDataSync (DeleteResourceDataSync'),
    newDeleteResourceDataSync,
    DeleteResourceDataSyncResponse (DeleteResourceDataSyncResponse'),
    newDeleteResourceDataSyncResponse,

    -- ** DeleteResourcePolicy
    DeleteResourcePolicy (DeleteResourcePolicy'),
    newDeleteResourcePolicy,
    DeleteResourcePolicyResponse (DeleteResourcePolicyResponse'),
    newDeleteResourcePolicyResponse,

    -- ** DeregisterManagedInstance
    DeregisterManagedInstance (DeregisterManagedInstance'),
    newDeregisterManagedInstance,
    DeregisterManagedInstanceResponse (DeregisterManagedInstanceResponse'),
    newDeregisterManagedInstanceResponse,

    -- ** DeregisterPatchBaselineForPatchGroup
    DeregisterPatchBaselineForPatchGroup (DeregisterPatchBaselineForPatchGroup'),
    newDeregisterPatchBaselineForPatchGroup,
    DeregisterPatchBaselineForPatchGroupResponse (DeregisterPatchBaselineForPatchGroupResponse'),
    newDeregisterPatchBaselineForPatchGroupResponse,

    -- ** DeregisterTargetFromMaintenanceWindow
    DeregisterTargetFromMaintenanceWindow (DeregisterTargetFromMaintenanceWindow'),
    newDeregisterTargetFromMaintenanceWindow,
    DeregisterTargetFromMaintenanceWindowResponse (DeregisterTargetFromMaintenanceWindowResponse'),
    newDeregisterTargetFromMaintenanceWindowResponse,

    -- ** DeregisterTaskFromMaintenanceWindow
    DeregisterTaskFromMaintenanceWindow (DeregisterTaskFromMaintenanceWindow'),
    newDeregisterTaskFromMaintenanceWindow,
    DeregisterTaskFromMaintenanceWindowResponse (DeregisterTaskFromMaintenanceWindowResponse'),
    newDeregisterTaskFromMaintenanceWindowResponse,

    -- ** DescribeActivations (Paginated)
    DescribeActivations (DescribeActivations'),
    newDescribeActivations,
    DescribeActivationsResponse (DescribeActivationsResponse'),
    newDescribeActivationsResponse,

    -- ** DescribeAssociation
    DescribeAssociation (DescribeAssociation'),
    newDescribeAssociation,
    DescribeAssociationResponse (DescribeAssociationResponse'),
    newDescribeAssociationResponse,

    -- ** DescribeAssociationExecutionTargets (Paginated)
    DescribeAssociationExecutionTargets (DescribeAssociationExecutionTargets'),
    newDescribeAssociationExecutionTargets,
    DescribeAssociationExecutionTargetsResponse (DescribeAssociationExecutionTargetsResponse'),
    newDescribeAssociationExecutionTargetsResponse,

    -- ** DescribeAssociationExecutions (Paginated)
    DescribeAssociationExecutions (DescribeAssociationExecutions'),
    newDescribeAssociationExecutions,
    DescribeAssociationExecutionsResponse (DescribeAssociationExecutionsResponse'),
    newDescribeAssociationExecutionsResponse,

    -- ** DescribeAutomationExecutions (Paginated)
    DescribeAutomationExecutions (DescribeAutomationExecutions'),
    newDescribeAutomationExecutions,
    DescribeAutomationExecutionsResponse (DescribeAutomationExecutionsResponse'),
    newDescribeAutomationExecutionsResponse,

    -- ** DescribeAutomationStepExecutions (Paginated)
    DescribeAutomationStepExecutions (DescribeAutomationStepExecutions'),
    newDescribeAutomationStepExecutions,
    DescribeAutomationStepExecutionsResponse (DescribeAutomationStepExecutionsResponse'),
    newDescribeAutomationStepExecutionsResponse,

    -- ** DescribeAvailablePatches (Paginated)
    DescribeAvailablePatches (DescribeAvailablePatches'),
    newDescribeAvailablePatches,
    DescribeAvailablePatchesResponse (DescribeAvailablePatchesResponse'),
    newDescribeAvailablePatchesResponse,

    -- ** DescribeDocument
    DescribeDocument (DescribeDocument'),
    newDescribeDocument,
    DescribeDocumentResponse (DescribeDocumentResponse'),
    newDescribeDocumentResponse,

    -- ** DescribeDocumentPermission
    DescribeDocumentPermission (DescribeDocumentPermission'),
    newDescribeDocumentPermission,
    DescribeDocumentPermissionResponse (DescribeDocumentPermissionResponse'),
    newDescribeDocumentPermissionResponse,

    -- ** DescribeEffectiveInstanceAssociations (Paginated)
    DescribeEffectiveInstanceAssociations (DescribeEffectiveInstanceAssociations'),
    newDescribeEffectiveInstanceAssociations,
    DescribeEffectiveInstanceAssociationsResponse (DescribeEffectiveInstanceAssociationsResponse'),
    newDescribeEffectiveInstanceAssociationsResponse,

    -- ** DescribeEffectivePatchesForPatchBaseline (Paginated)
    DescribeEffectivePatchesForPatchBaseline (DescribeEffectivePatchesForPatchBaseline'),
    newDescribeEffectivePatchesForPatchBaseline,
    DescribeEffectivePatchesForPatchBaselineResponse (DescribeEffectivePatchesForPatchBaselineResponse'),
    newDescribeEffectivePatchesForPatchBaselineResponse,

    -- ** DescribeInstanceAssociationsStatus (Paginated)
    DescribeInstanceAssociationsStatus (DescribeInstanceAssociationsStatus'),
    newDescribeInstanceAssociationsStatus,
    DescribeInstanceAssociationsStatusResponse (DescribeInstanceAssociationsStatusResponse'),
    newDescribeInstanceAssociationsStatusResponse,

    -- ** DescribeInstanceInformation (Paginated)
    DescribeInstanceInformation (DescribeInstanceInformation'),
    newDescribeInstanceInformation,
    DescribeInstanceInformationResponse (DescribeInstanceInformationResponse'),
    newDescribeInstanceInformationResponse,

    -- ** DescribeInstancePatchStates (Paginated)
    DescribeInstancePatchStates (DescribeInstancePatchStates'),
    newDescribeInstancePatchStates,
    DescribeInstancePatchStatesResponse (DescribeInstancePatchStatesResponse'),
    newDescribeInstancePatchStatesResponse,

    -- ** DescribeInstancePatchStatesForPatchGroup (Paginated)
    DescribeInstancePatchStatesForPatchGroup (DescribeInstancePatchStatesForPatchGroup'),
    newDescribeInstancePatchStatesForPatchGroup,
    DescribeInstancePatchStatesForPatchGroupResponse (DescribeInstancePatchStatesForPatchGroupResponse'),
    newDescribeInstancePatchStatesForPatchGroupResponse,

    -- ** DescribeInstancePatches (Paginated)
    DescribeInstancePatches (DescribeInstancePatches'),
    newDescribeInstancePatches,
    DescribeInstancePatchesResponse (DescribeInstancePatchesResponse'),
    newDescribeInstancePatchesResponse,

    -- ** DescribeInventoryDeletions (Paginated)
    DescribeInventoryDeletions (DescribeInventoryDeletions'),
    newDescribeInventoryDeletions,
    DescribeInventoryDeletionsResponse (DescribeInventoryDeletionsResponse'),
    newDescribeInventoryDeletionsResponse,

    -- ** DescribeMaintenanceWindowExecutionTaskInvocations (Paginated)
    DescribeMaintenanceWindowExecutionTaskInvocations (DescribeMaintenanceWindowExecutionTaskInvocations'),
    newDescribeMaintenanceWindowExecutionTaskInvocations,
    DescribeMaintenanceWindowExecutionTaskInvocationsResponse (DescribeMaintenanceWindowExecutionTaskInvocationsResponse'),
    newDescribeMaintenanceWindowExecutionTaskInvocationsResponse,

    -- ** DescribeMaintenanceWindowExecutionTasks (Paginated)
    DescribeMaintenanceWindowExecutionTasks (DescribeMaintenanceWindowExecutionTasks'),
    newDescribeMaintenanceWindowExecutionTasks,
    DescribeMaintenanceWindowExecutionTasksResponse (DescribeMaintenanceWindowExecutionTasksResponse'),
    newDescribeMaintenanceWindowExecutionTasksResponse,

    -- ** DescribeMaintenanceWindowExecutions (Paginated)
    DescribeMaintenanceWindowExecutions (DescribeMaintenanceWindowExecutions'),
    newDescribeMaintenanceWindowExecutions,
    DescribeMaintenanceWindowExecutionsResponse (DescribeMaintenanceWindowExecutionsResponse'),
    newDescribeMaintenanceWindowExecutionsResponse,

    -- ** DescribeMaintenanceWindowSchedule (Paginated)
    DescribeMaintenanceWindowSchedule (DescribeMaintenanceWindowSchedule'),
    newDescribeMaintenanceWindowSchedule,
    DescribeMaintenanceWindowScheduleResponse (DescribeMaintenanceWindowScheduleResponse'),
    newDescribeMaintenanceWindowScheduleResponse,

    -- ** DescribeMaintenanceWindowTargets (Paginated)
    DescribeMaintenanceWindowTargets (DescribeMaintenanceWindowTargets'),
    newDescribeMaintenanceWindowTargets,
    DescribeMaintenanceWindowTargetsResponse (DescribeMaintenanceWindowTargetsResponse'),
    newDescribeMaintenanceWindowTargetsResponse,

    -- ** DescribeMaintenanceWindowTasks (Paginated)
    DescribeMaintenanceWindowTasks (DescribeMaintenanceWindowTasks'),
    newDescribeMaintenanceWindowTasks,
    DescribeMaintenanceWindowTasksResponse (DescribeMaintenanceWindowTasksResponse'),
    newDescribeMaintenanceWindowTasksResponse,

    -- ** DescribeMaintenanceWindows (Paginated)
    DescribeMaintenanceWindows (DescribeMaintenanceWindows'),
    newDescribeMaintenanceWindows,
    DescribeMaintenanceWindowsResponse (DescribeMaintenanceWindowsResponse'),
    newDescribeMaintenanceWindowsResponse,

    -- ** DescribeMaintenanceWindowsForTarget (Paginated)
    DescribeMaintenanceWindowsForTarget (DescribeMaintenanceWindowsForTarget'),
    newDescribeMaintenanceWindowsForTarget,
    DescribeMaintenanceWindowsForTargetResponse (DescribeMaintenanceWindowsForTargetResponse'),
    newDescribeMaintenanceWindowsForTargetResponse,

    -- ** DescribeOpsItems (Paginated)
    DescribeOpsItems (DescribeOpsItems'),
    newDescribeOpsItems,
    DescribeOpsItemsResponse (DescribeOpsItemsResponse'),
    newDescribeOpsItemsResponse,

    -- ** DescribeParameters (Paginated)
    DescribeParameters (DescribeParameters'),
    newDescribeParameters,
    DescribeParametersResponse (DescribeParametersResponse'),
    newDescribeParametersResponse,

    -- ** DescribePatchBaselines (Paginated)
    DescribePatchBaselines (DescribePatchBaselines'),
    newDescribePatchBaselines,
    DescribePatchBaselinesResponse (DescribePatchBaselinesResponse'),
    newDescribePatchBaselinesResponse,

    -- ** DescribePatchGroupState
    DescribePatchGroupState (DescribePatchGroupState'),
    newDescribePatchGroupState,
    DescribePatchGroupStateResponse (DescribePatchGroupStateResponse'),
    newDescribePatchGroupStateResponse,

    -- ** DescribePatchGroups (Paginated)
    DescribePatchGroups (DescribePatchGroups'),
    newDescribePatchGroups,
    DescribePatchGroupsResponse (DescribePatchGroupsResponse'),
    newDescribePatchGroupsResponse,

    -- ** DescribePatchProperties (Paginated)
    DescribePatchProperties (DescribePatchProperties'),
    newDescribePatchProperties,
    DescribePatchPropertiesResponse (DescribePatchPropertiesResponse'),
    newDescribePatchPropertiesResponse,

    -- ** DescribeSessions (Paginated)
    DescribeSessions (DescribeSessions'),
    newDescribeSessions,
    DescribeSessionsResponse (DescribeSessionsResponse'),
    newDescribeSessionsResponse,

    -- ** DisassociateOpsItemRelatedItem
    DisassociateOpsItemRelatedItem (DisassociateOpsItemRelatedItem'),
    newDisassociateOpsItemRelatedItem,
    DisassociateOpsItemRelatedItemResponse (DisassociateOpsItemRelatedItemResponse'),
    newDisassociateOpsItemRelatedItemResponse,

    -- ** GetAutomationExecution
    GetAutomationExecution (GetAutomationExecution'),
    newGetAutomationExecution,
    GetAutomationExecutionResponse (GetAutomationExecutionResponse'),
    newGetAutomationExecutionResponse,

    -- ** GetCalendarState
    GetCalendarState (GetCalendarState'),
    newGetCalendarState,
    GetCalendarStateResponse (GetCalendarStateResponse'),
    newGetCalendarStateResponse,

    -- ** GetCommandInvocation
    GetCommandInvocation (GetCommandInvocation'),
    newGetCommandInvocation,
    GetCommandInvocationResponse (GetCommandInvocationResponse'),
    newGetCommandInvocationResponse,

    -- ** GetConnectionStatus
    GetConnectionStatus (GetConnectionStatus'),
    newGetConnectionStatus,
    GetConnectionStatusResponse (GetConnectionStatusResponse'),
    newGetConnectionStatusResponse,

    -- ** GetDefaultPatchBaseline
    GetDefaultPatchBaseline (GetDefaultPatchBaseline'),
    newGetDefaultPatchBaseline,
    GetDefaultPatchBaselineResponse (GetDefaultPatchBaselineResponse'),
    newGetDefaultPatchBaselineResponse,

    -- ** GetDeployablePatchSnapshotForInstance
    GetDeployablePatchSnapshotForInstance (GetDeployablePatchSnapshotForInstance'),
    newGetDeployablePatchSnapshotForInstance,
    GetDeployablePatchSnapshotForInstanceResponse (GetDeployablePatchSnapshotForInstanceResponse'),
    newGetDeployablePatchSnapshotForInstanceResponse,

    -- ** GetDocument
    GetDocument (GetDocument'),
    newGetDocument,
    GetDocumentResponse (GetDocumentResponse'),
    newGetDocumentResponse,

    -- ** GetInventory (Paginated)
    GetInventory (GetInventory'),
    newGetInventory,
    GetInventoryResponse (GetInventoryResponse'),
    newGetInventoryResponse,

    -- ** GetInventorySchema (Paginated)
    GetInventorySchema (GetInventorySchema'),
    newGetInventorySchema,
    GetInventorySchemaResponse (GetInventorySchemaResponse'),
    newGetInventorySchemaResponse,

    -- ** GetMaintenanceWindow
    GetMaintenanceWindow (GetMaintenanceWindow'),
    newGetMaintenanceWindow,
    GetMaintenanceWindowResponse (GetMaintenanceWindowResponse'),
    newGetMaintenanceWindowResponse,

    -- ** GetMaintenanceWindowExecution
    GetMaintenanceWindowExecution (GetMaintenanceWindowExecution'),
    newGetMaintenanceWindowExecution,
    GetMaintenanceWindowExecutionResponse (GetMaintenanceWindowExecutionResponse'),
    newGetMaintenanceWindowExecutionResponse,

    -- ** GetMaintenanceWindowExecutionTask
    GetMaintenanceWindowExecutionTask (GetMaintenanceWindowExecutionTask'),
    newGetMaintenanceWindowExecutionTask,
    GetMaintenanceWindowExecutionTaskResponse (GetMaintenanceWindowExecutionTaskResponse'),
    newGetMaintenanceWindowExecutionTaskResponse,

    -- ** GetMaintenanceWindowExecutionTaskInvocation
    GetMaintenanceWindowExecutionTaskInvocation (GetMaintenanceWindowExecutionTaskInvocation'),
    newGetMaintenanceWindowExecutionTaskInvocation,
    GetMaintenanceWindowExecutionTaskInvocationResponse (GetMaintenanceWindowExecutionTaskInvocationResponse'),
    newGetMaintenanceWindowExecutionTaskInvocationResponse,

    -- ** GetMaintenanceWindowTask
    GetMaintenanceWindowTask (GetMaintenanceWindowTask'),
    newGetMaintenanceWindowTask,
    GetMaintenanceWindowTaskResponse (GetMaintenanceWindowTaskResponse'),
    newGetMaintenanceWindowTaskResponse,

    -- ** GetOpsItem
    GetOpsItem (GetOpsItem'),
    newGetOpsItem,
    GetOpsItemResponse (GetOpsItemResponse'),
    newGetOpsItemResponse,

    -- ** GetOpsMetadata
    GetOpsMetadata (GetOpsMetadata'),
    newGetOpsMetadata,
    GetOpsMetadataResponse (GetOpsMetadataResponse'),
    newGetOpsMetadataResponse,

    -- ** GetOpsSummary (Paginated)
    GetOpsSummary (GetOpsSummary'),
    newGetOpsSummary,
    GetOpsSummaryResponse (GetOpsSummaryResponse'),
    newGetOpsSummaryResponse,

    -- ** GetParameter
    GetParameter (GetParameter'),
    newGetParameter,
    GetParameterResponse (GetParameterResponse'),
    newGetParameterResponse,

    -- ** GetParameterHistory (Paginated)
    GetParameterHistory (GetParameterHistory'),
    newGetParameterHistory,
    GetParameterHistoryResponse (GetParameterHistoryResponse'),
    newGetParameterHistoryResponse,

    -- ** GetParameters
    GetParameters (GetParameters'),
    newGetParameters,
    GetParametersResponse (GetParametersResponse'),
    newGetParametersResponse,

    -- ** GetParametersByPath (Paginated)
    GetParametersByPath (GetParametersByPath'),
    newGetParametersByPath,
    GetParametersByPathResponse (GetParametersByPathResponse'),
    newGetParametersByPathResponse,

    -- ** GetPatchBaseline
    GetPatchBaseline (GetPatchBaseline'),
    newGetPatchBaseline,
    GetPatchBaselineResponse (GetPatchBaselineResponse'),
    newGetPatchBaselineResponse,

    -- ** GetPatchBaselineForPatchGroup
    GetPatchBaselineForPatchGroup (GetPatchBaselineForPatchGroup'),
    newGetPatchBaselineForPatchGroup,
    GetPatchBaselineForPatchGroupResponse (GetPatchBaselineForPatchGroupResponse'),
    newGetPatchBaselineForPatchGroupResponse,

    -- ** GetResourcePolicies (Paginated)
    GetResourcePolicies (GetResourcePolicies'),
    newGetResourcePolicies,
    GetResourcePoliciesResponse (GetResourcePoliciesResponse'),
    newGetResourcePoliciesResponse,

    -- ** GetServiceSetting
    GetServiceSetting (GetServiceSetting'),
    newGetServiceSetting,
    GetServiceSettingResponse (GetServiceSettingResponse'),
    newGetServiceSettingResponse,

    -- ** LabelParameterVersion
    LabelParameterVersion (LabelParameterVersion'),
    newLabelParameterVersion,
    LabelParameterVersionResponse (LabelParameterVersionResponse'),
    newLabelParameterVersionResponse,

    -- ** ListAssociationVersions (Paginated)
    ListAssociationVersions (ListAssociationVersions'),
    newListAssociationVersions,
    ListAssociationVersionsResponse (ListAssociationVersionsResponse'),
    newListAssociationVersionsResponse,

    -- ** ListAssociations (Paginated)
    ListAssociations (ListAssociations'),
    newListAssociations,
    ListAssociationsResponse (ListAssociationsResponse'),
    newListAssociationsResponse,

    -- ** ListCommandInvocations (Paginated)
    ListCommandInvocations (ListCommandInvocations'),
    newListCommandInvocations,
    ListCommandInvocationsResponse (ListCommandInvocationsResponse'),
    newListCommandInvocationsResponse,

    -- ** ListCommands (Paginated)
    ListCommands (ListCommands'),
    newListCommands,
    ListCommandsResponse (ListCommandsResponse'),
    newListCommandsResponse,

    -- ** ListComplianceItems (Paginated)
    ListComplianceItems (ListComplianceItems'),
    newListComplianceItems,
    ListComplianceItemsResponse (ListComplianceItemsResponse'),
    newListComplianceItemsResponse,

    -- ** ListComplianceSummaries (Paginated)
    ListComplianceSummaries (ListComplianceSummaries'),
    newListComplianceSummaries,
    ListComplianceSummariesResponse (ListComplianceSummariesResponse'),
    newListComplianceSummariesResponse,

    -- ** ListDocumentMetadataHistory
    ListDocumentMetadataHistory (ListDocumentMetadataHistory'),
    newListDocumentMetadataHistory,
    ListDocumentMetadataHistoryResponse (ListDocumentMetadataHistoryResponse'),
    newListDocumentMetadataHistoryResponse,

    -- ** ListDocumentVersions (Paginated)
    ListDocumentVersions (ListDocumentVersions'),
    newListDocumentVersions,
    ListDocumentVersionsResponse (ListDocumentVersionsResponse'),
    newListDocumentVersionsResponse,

    -- ** ListDocuments (Paginated)
    ListDocuments (ListDocuments'),
    newListDocuments,
    ListDocumentsResponse (ListDocumentsResponse'),
    newListDocumentsResponse,

    -- ** ListInventoryEntries
    ListInventoryEntries (ListInventoryEntries'),
    newListInventoryEntries,
    ListInventoryEntriesResponse (ListInventoryEntriesResponse'),
    newListInventoryEntriesResponse,

    -- ** ListOpsItemEvents (Paginated)
    ListOpsItemEvents (ListOpsItemEvents'),
    newListOpsItemEvents,
    ListOpsItemEventsResponse (ListOpsItemEventsResponse'),
    newListOpsItemEventsResponse,

    -- ** ListOpsItemRelatedItems (Paginated)
    ListOpsItemRelatedItems (ListOpsItemRelatedItems'),
    newListOpsItemRelatedItems,
    ListOpsItemRelatedItemsResponse (ListOpsItemRelatedItemsResponse'),
    newListOpsItemRelatedItemsResponse,

    -- ** ListOpsMetadata (Paginated)
    ListOpsMetadata (ListOpsMetadata'),
    newListOpsMetadata,
    ListOpsMetadataResponse (ListOpsMetadataResponse'),
    newListOpsMetadataResponse,

    -- ** ListResourceComplianceSummaries (Paginated)
    ListResourceComplianceSummaries (ListResourceComplianceSummaries'),
    newListResourceComplianceSummaries,
    ListResourceComplianceSummariesResponse (ListResourceComplianceSummariesResponse'),
    newListResourceComplianceSummariesResponse,

    -- ** ListResourceDataSync (Paginated)
    ListResourceDataSync (ListResourceDataSync'),
    newListResourceDataSync,
    ListResourceDataSyncResponse (ListResourceDataSyncResponse'),
    newListResourceDataSyncResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ModifyDocumentPermission
    ModifyDocumentPermission (ModifyDocumentPermission'),
    newModifyDocumentPermission,
    ModifyDocumentPermissionResponse (ModifyDocumentPermissionResponse'),
    newModifyDocumentPermissionResponse,

    -- ** PutComplianceItems
    PutComplianceItems (PutComplianceItems'),
    newPutComplianceItems,
    PutComplianceItemsResponse (PutComplianceItemsResponse'),
    newPutComplianceItemsResponse,

    -- ** PutInventory
    PutInventory (PutInventory'),
    newPutInventory,
    PutInventoryResponse (PutInventoryResponse'),
    newPutInventoryResponse,

    -- ** PutParameter
    PutParameter (PutParameter'),
    newPutParameter,
    PutParameterResponse (PutParameterResponse'),
    newPutParameterResponse,

    -- ** PutResourcePolicy
    PutResourcePolicy (PutResourcePolicy'),
    newPutResourcePolicy,
    PutResourcePolicyResponse (PutResourcePolicyResponse'),
    newPutResourcePolicyResponse,

    -- ** RegisterDefaultPatchBaseline
    RegisterDefaultPatchBaseline (RegisterDefaultPatchBaseline'),
    newRegisterDefaultPatchBaseline,
    RegisterDefaultPatchBaselineResponse (RegisterDefaultPatchBaselineResponse'),
    newRegisterDefaultPatchBaselineResponse,

    -- ** RegisterPatchBaselineForPatchGroup
    RegisterPatchBaselineForPatchGroup (RegisterPatchBaselineForPatchGroup'),
    newRegisterPatchBaselineForPatchGroup,
    RegisterPatchBaselineForPatchGroupResponse (RegisterPatchBaselineForPatchGroupResponse'),
    newRegisterPatchBaselineForPatchGroupResponse,

    -- ** RegisterTargetWithMaintenanceWindow
    RegisterTargetWithMaintenanceWindow (RegisterTargetWithMaintenanceWindow'),
    newRegisterTargetWithMaintenanceWindow,
    RegisterTargetWithMaintenanceWindowResponse (RegisterTargetWithMaintenanceWindowResponse'),
    newRegisterTargetWithMaintenanceWindowResponse,

    -- ** RegisterTaskWithMaintenanceWindow
    RegisterTaskWithMaintenanceWindow (RegisterTaskWithMaintenanceWindow'),
    newRegisterTaskWithMaintenanceWindow,
    RegisterTaskWithMaintenanceWindowResponse (RegisterTaskWithMaintenanceWindowResponse'),
    newRegisterTaskWithMaintenanceWindowResponse,

    -- ** RemoveTagsFromResource
    RemoveTagsFromResource (RemoveTagsFromResource'),
    newRemoveTagsFromResource,
    RemoveTagsFromResourceResponse (RemoveTagsFromResourceResponse'),
    newRemoveTagsFromResourceResponse,

    -- ** ResetServiceSetting
    ResetServiceSetting (ResetServiceSetting'),
    newResetServiceSetting,
    ResetServiceSettingResponse (ResetServiceSettingResponse'),
    newResetServiceSettingResponse,

    -- ** ResumeSession
    ResumeSession (ResumeSession'),
    newResumeSession,
    ResumeSessionResponse (ResumeSessionResponse'),
    newResumeSessionResponse,

    -- ** SendAutomationSignal
    SendAutomationSignal (SendAutomationSignal'),
    newSendAutomationSignal,
    SendAutomationSignalResponse (SendAutomationSignalResponse'),
    newSendAutomationSignalResponse,

    -- ** SendCommand
    SendCommand (SendCommand'),
    newSendCommand,
    SendCommandResponse (SendCommandResponse'),
    newSendCommandResponse,

    -- ** StartAssociationsOnce
    StartAssociationsOnce (StartAssociationsOnce'),
    newStartAssociationsOnce,
    StartAssociationsOnceResponse (StartAssociationsOnceResponse'),
    newStartAssociationsOnceResponse,

    -- ** StartAutomationExecution
    StartAutomationExecution (StartAutomationExecution'),
    newStartAutomationExecution,
    StartAutomationExecutionResponse (StartAutomationExecutionResponse'),
    newStartAutomationExecutionResponse,

    -- ** StartChangeRequestExecution
    StartChangeRequestExecution (StartChangeRequestExecution'),
    newStartChangeRequestExecution,
    StartChangeRequestExecutionResponse (StartChangeRequestExecutionResponse'),
    newStartChangeRequestExecutionResponse,

    -- ** StartSession
    StartSession (StartSession'),
    newStartSession,
    StartSessionResponse (StartSessionResponse'),
    newStartSessionResponse,

    -- ** StopAutomationExecution
    StopAutomationExecution (StopAutomationExecution'),
    newStopAutomationExecution,
    StopAutomationExecutionResponse (StopAutomationExecutionResponse'),
    newStopAutomationExecutionResponse,

    -- ** TerminateSession
    TerminateSession (TerminateSession'),
    newTerminateSession,
    TerminateSessionResponse (TerminateSessionResponse'),
    newTerminateSessionResponse,

    -- ** UnlabelParameterVersion
    UnlabelParameterVersion (UnlabelParameterVersion'),
    newUnlabelParameterVersion,
    UnlabelParameterVersionResponse (UnlabelParameterVersionResponse'),
    newUnlabelParameterVersionResponse,

    -- ** UpdateAssociation
    UpdateAssociation (UpdateAssociation'),
    newUpdateAssociation,
    UpdateAssociationResponse (UpdateAssociationResponse'),
    newUpdateAssociationResponse,

    -- ** UpdateAssociationStatus
    UpdateAssociationStatus (UpdateAssociationStatus'),
    newUpdateAssociationStatus,
    UpdateAssociationStatusResponse (UpdateAssociationStatusResponse'),
    newUpdateAssociationStatusResponse,

    -- ** UpdateDocument
    UpdateDocument (UpdateDocument'),
    newUpdateDocument,
    UpdateDocumentResponse (UpdateDocumentResponse'),
    newUpdateDocumentResponse,

    -- ** UpdateDocumentDefaultVersion
    UpdateDocumentDefaultVersion (UpdateDocumentDefaultVersion'),
    newUpdateDocumentDefaultVersion,
    UpdateDocumentDefaultVersionResponse (UpdateDocumentDefaultVersionResponse'),
    newUpdateDocumentDefaultVersionResponse,

    -- ** UpdateDocumentMetadata
    UpdateDocumentMetadata (UpdateDocumentMetadata'),
    newUpdateDocumentMetadata,
    UpdateDocumentMetadataResponse (UpdateDocumentMetadataResponse'),
    newUpdateDocumentMetadataResponse,

    -- ** UpdateMaintenanceWindow
    UpdateMaintenanceWindow (UpdateMaintenanceWindow'),
    newUpdateMaintenanceWindow,
    UpdateMaintenanceWindowResponse (UpdateMaintenanceWindowResponse'),
    newUpdateMaintenanceWindowResponse,

    -- ** UpdateMaintenanceWindowTarget
    UpdateMaintenanceWindowTarget (UpdateMaintenanceWindowTarget'),
    newUpdateMaintenanceWindowTarget,
    UpdateMaintenanceWindowTargetResponse (UpdateMaintenanceWindowTargetResponse'),
    newUpdateMaintenanceWindowTargetResponse,

    -- ** UpdateMaintenanceWindowTask
    UpdateMaintenanceWindowTask (UpdateMaintenanceWindowTask'),
    newUpdateMaintenanceWindowTask,
    UpdateMaintenanceWindowTaskResponse (UpdateMaintenanceWindowTaskResponse'),
    newUpdateMaintenanceWindowTaskResponse,

    -- ** UpdateManagedInstanceRole
    UpdateManagedInstanceRole (UpdateManagedInstanceRole'),
    newUpdateManagedInstanceRole,
    UpdateManagedInstanceRoleResponse (UpdateManagedInstanceRoleResponse'),
    newUpdateManagedInstanceRoleResponse,

    -- ** UpdateOpsItem
    UpdateOpsItem (UpdateOpsItem'),
    newUpdateOpsItem,
    UpdateOpsItemResponse (UpdateOpsItemResponse'),
    newUpdateOpsItemResponse,

    -- ** UpdateOpsMetadata
    UpdateOpsMetadata (UpdateOpsMetadata'),
    newUpdateOpsMetadata,
    UpdateOpsMetadataResponse (UpdateOpsMetadataResponse'),
    newUpdateOpsMetadataResponse,

    -- ** UpdatePatchBaseline
    UpdatePatchBaseline (UpdatePatchBaseline'),
    newUpdatePatchBaseline,
    UpdatePatchBaselineResponse (UpdatePatchBaselineResponse'),
    newUpdatePatchBaselineResponse,

    -- ** UpdateResourceDataSync
    UpdateResourceDataSync (UpdateResourceDataSync'),
    newUpdateResourceDataSync,
    UpdateResourceDataSyncResponse (UpdateResourceDataSyncResponse'),
    newUpdateResourceDataSyncResponse,

    -- ** UpdateServiceSetting
    UpdateServiceSetting (UpdateServiceSetting'),
    newUpdateServiceSetting,
    UpdateServiceSettingResponse (UpdateServiceSettingResponse'),
    newUpdateServiceSettingResponse,

    -- * Types

    -- ** AssociationComplianceSeverity
    AssociationComplianceSeverity (..),

    -- ** AssociationExecutionFilterKey
    AssociationExecutionFilterKey (..),

    -- ** AssociationExecutionTargetsFilterKey
    AssociationExecutionTargetsFilterKey (..),

    -- ** AssociationFilterKey
    AssociationFilterKey (..),

    -- ** AssociationFilterOperatorType
    AssociationFilterOperatorType (..),

    -- ** AssociationStatusName
    AssociationStatusName (..),

    -- ** AssociationSyncCompliance
    AssociationSyncCompliance (..),

    -- ** AttachmentHashType
    AttachmentHashType (..),

    -- ** AttachmentsSourceKey
    AttachmentsSourceKey (..),

    -- ** AutomationExecutionFilterKey
    AutomationExecutionFilterKey (..),

    -- ** AutomationExecutionStatus
    AutomationExecutionStatus (..),

    -- ** AutomationSubtype
    AutomationSubtype (..),

    -- ** AutomationType
    AutomationType (..),

    -- ** CalendarState
    CalendarState (..),

    -- ** CommandFilterKey
    CommandFilterKey (..),

    -- ** CommandInvocationStatus
    CommandInvocationStatus (..),

    -- ** CommandPluginStatus
    CommandPluginStatus (..),

    -- ** CommandStatus
    CommandStatus (..),

    -- ** ComplianceQueryOperatorType
    ComplianceQueryOperatorType (..),

    -- ** ComplianceSeverity
    ComplianceSeverity (..),

    -- ** ComplianceStatus
    ComplianceStatus (..),

    -- ** ComplianceUploadType
    ComplianceUploadType (..),

    -- ** ConnectionStatus
    ConnectionStatus (..),

    -- ** DescribeActivationsFilterKeys
    DescribeActivationsFilterKeys (..),

    -- ** DocumentFilterKey
    DocumentFilterKey (..),

    -- ** DocumentFormat
    DocumentFormat (..),

    -- ** DocumentHashType
    DocumentHashType (..),

    -- ** DocumentMetadataEnum
    DocumentMetadataEnum (..),

    -- ** DocumentParameterType
    DocumentParameterType (..),

    -- ** DocumentPermissionType
    DocumentPermissionType (..),

    -- ** DocumentReviewAction
    DocumentReviewAction (..),

    -- ** DocumentReviewCommentType
    DocumentReviewCommentType (..),

    -- ** DocumentStatus
    DocumentStatus (..),

    -- ** DocumentType
    DocumentType (..),

    -- ** ExecutionMode
    ExecutionMode (..),

    -- ** ExternalAlarmState
    ExternalAlarmState (..),

    -- ** Fault
    Fault (..),

    -- ** InstanceInformationFilterKey
    InstanceInformationFilterKey (..),

    -- ** InstancePatchStateOperatorType
    InstancePatchStateOperatorType (..),

    -- ** InventoryAttributeDataType
    InventoryAttributeDataType (..),

    -- ** InventoryDeletionStatus
    InventoryDeletionStatus (..),

    -- ** InventoryQueryOperatorType
    InventoryQueryOperatorType (..),

    -- ** InventorySchemaDeleteOption
    InventorySchemaDeleteOption (..),

    -- ** LastResourceDataSyncStatus
    LastResourceDataSyncStatus (..),

    -- ** MaintenanceWindowExecutionStatus
    MaintenanceWindowExecutionStatus (..),

    -- ** MaintenanceWindowResourceType
    MaintenanceWindowResourceType (..),

    -- ** MaintenanceWindowTaskCutoffBehavior
    MaintenanceWindowTaskCutoffBehavior (..),

    -- ** MaintenanceWindowTaskType
    MaintenanceWindowTaskType (..),

    -- ** NotificationEvent
    NotificationEvent (..),

    -- ** NotificationType
    NotificationType (..),

    -- ** OperatingSystem
    OperatingSystem (..),

    -- ** OpsFilterOperatorType
    OpsFilterOperatorType (..),

    -- ** OpsItemDataType
    OpsItemDataType (..),

    -- ** OpsItemEventFilterKey
    OpsItemEventFilterKey (..),

    -- ** OpsItemEventFilterOperator
    OpsItemEventFilterOperator (..),

    -- ** OpsItemFilterKey
    OpsItemFilterKey (..),

    -- ** OpsItemFilterOperator
    OpsItemFilterOperator (..),

    -- ** OpsItemRelatedItemsFilterKey
    OpsItemRelatedItemsFilterKey (..),

    -- ** OpsItemRelatedItemsFilterOperator
    OpsItemRelatedItemsFilterOperator (..),

    -- ** OpsItemStatus
    OpsItemStatus (..),

    -- ** ParameterTier
    ParameterTier (..),

    -- ** ParameterType
    ParameterType (..),

    -- ** ParametersFilterKey
    ParametersFilterKey (..),

    -- ** PatchAction
    PatchAction (..),

    -- ** PatchComplianceDataState
    PatchComplianceDataState (..),

    -- ** PatchComplianceLevel
    PatchComplianceLevel (..),

    -- ** PatchDeploymentStatus
    PatchDeploymentStatus (..),

    -- ** PatchFilterKey
    PatchFilterKey (..),

    -- ** PatchOperationType
    PatchOperationType (..),

    -- ** PatchProperty
    PatchProperty (..),

    -- ** PatchSet
    PatchSet (..),

    -- ** PingStatus
    PingStatus (..),

    -- ** PlatformType
    PlatformType (..),

    -- ** RebootOption
    RebootOption (..),

    -- ** ResourceDataSyncS3Format
    ResourceDataSyncS3Format (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** ResourceTypeForTagging
    ResourceTypeForTagging (..),

    -- ** ReviewStatus
    ReviewStatus (..),

    -- ** SessionFilterKey
    SessionFilterKey (..),

    -- ** SessionState
    SessionState (..),

    -- ** SessionStatus
    SessionStatus (..),

    -- ** SignalType
    SignalType (..),

    -- ** SourceType
    SourceType (..),

    -- ** StepExecutionFilterKey
    StepExecutionFilterKey (..),

    -- ** StopType
    StopType (..),

    -- ** AccountSharingInfo
    AccountSharingInfo (AccountSharingInfo'),
    newAccountSharingInfo,

    -- ** Activation
    Activation (Activation'),
    newActivation,

    -- ** Alarm
    Alarm (Alarm'),
    newAlarm,

    -- ** AlarmConfiguration
    AlarmConfiguration (AlarmConfiguration'),
    newAlarmConfiguration,

    -- ** AlarmStateInformation
    AlarmStateInformation (AlarmStateInformation'),
    newAlarmStateInformation,

    -- ** Association
    Association (Association'),
    newAssociation,

    -- ** AssociationDescription
    AssociationDescription (AssociationDescription'),
    newAssociationDescription,

    -- ** AssociationExecution
    AssociationExecution (AssociationExecution'),
    newAssociationExecution,

    -- ** AssociationExecutionFilter
    AssociationExecutionFilter (AssociationExecutionFilter'),
    newAssociationExecutionFilter,

    -- ** AssociationExecutionTarget
    AssociationExecutionTarget (AssociationExecutionTarget'),
    newAssociationExecutionTarget,

    -- ** AssociationExecutionTargetsFilter
    AssociationExecutionTargetsFilter (AssociationExecutionTargetsFilter'),
    newAssociationExecutionTargetsFilter,

    -- ** AssociationFilter
    AssociationFilter (AssociationFilter'),
    newAssociationFilter,

    -- ** AssociationOverview
    AssociationOverview (AssociationOverview'),
    newAssociationOverview,

    -- ** AssociationStatus
    AssociationStatus (AssociationStatus'),
    newAssociationStatus,

    -- ** AssociationVersionInfo
    AssociationVersionInfo (AssociationVersionInfo'),
    newAssociationVersionInfo,

    -- ** AttachmentContent
    AttachmentContent (AttachmentContent'),
    newAttachmentContent,

    -- ** AttachmentInformation
    AttachmentInformation (AttachmentInformation'),
    newAttachmentInformation,

    -- ** AttachmentsSource
    AttachmentsSource (AttachmentsSource'),
    newAttachmentsSource,

    -- ** AutomationExecution
    AutomationExecution (AutomationExecution'),
    newAutomationExecution,

    -- ** AutomationExecutionFilter
    AutomationExecutionFilter (AutomationExecutionFilter'),
    newAutomationExecutionFilter,

    -- ** AutomationExecutionMetadata
    AutomationExecutionMetadata (AutomationExecutionMetadata'),
    newAutomationExecutionMetadata,

    -- ** BaselineOverride
    BaselineOverride (BaselineOverride'),
    newBaselineOverride,

    -- ** CloudWatchOutputConfig
    CloudWatchOutputConfig (CloudWatchOutputConfig'),
    newCloudWatchOutputConfig,

    -- ** Command
    Command (Command'),
    newCommand,

    -- ** CommandFilter
    CommandFilter (CommandFilter'),
    newCommandFilter,

    -- ** CommandInvocation
    CommandInvocation (CommandInvocation'),
    newCommandInvocation,

    -- ** CommandPlugin
    CommandPlugin (CommandPlugin'),
    newCommandPlugin,

    -- ** ComplianceExecutionSummary
    ComplianceExecutionSummary (ComplianceExecutionSummary'),
    newComplianceExecutionSummary,

    -- ** ComplianceItem
    ComplianceItem (ComplianceItem'),
    newComplianceItem,

    -- ** ComplianceItemEntry
    ComplianceItemEntry (ComplianceItemEntry'),
    newComplianceItemEntry,

    -- ** ComplianceStringFilter
    ComplianceStringFilter (ComplianceStringFilter'),
    newComplianceStringFilter,

    -- ** ComplianceSummaryItem
    ComplianceSummaryItem (ComplianceSummaryItem'),
    newComplianceSummaryItem,

    -- ** CompliantSummary
    CompliantSummary (CompliantSummary'),
    newCompliantSummary,

    -- ** CreateAssociationBatchRequestEntry
    CreateAssociationBatchRequestEntry (CreateAssociationBatchRequestEntry'),
    newCreateAssociationBatchRequestEntry,

    -- ** DescribeActivationsFilter
    DescribeActivationsFilter (DescribeActivationsFilter'),
    newDescribeActivationsFilter,

    -- ** DocumentDefaultVersionDescription
    DocumentDefaultVersionDescription (DocumentDefaultVersionDescription'),
    newDocumentDefaultVersionDescription,

    -- ** DocumentDescription
    DocumentDescription (DocumentDescription'),
    newDocumentDescription,

    -- ** DocumentFilter
    DocumentFilter (DocumentFilter'),
    newDocumentFilter,

    -- ** DocumentIdentifier
    DocumentIdentifier (DocumentIdentifier'),
    newDocumentIdentifier,

    -- ** DocumentKeyValuesFilter
    DocumentKeyValuesFilter (DocumentKeyValuesFilter'),
    newDocumentKeyValuesFilter,

    -- ** DocumentMetadataResponseInfo
    DocumentMetadataResponseInfo (DocumentMetadataResponseInfo'),
    newDocumentMetadataResponseInfo,

    -- ** DocumentParameter
    DocumentParameter (DocumentParameter'),
    newDocumentParameter,

    -- ** DocumentRequires
    DocumentRequires (DocumentRequires'),
    newDocumentRequires,

    -- ** DocumentReviewCommentSource
    DocumentReviewCommentSource (DocumentReviewCommentSource'),
    newDocumentReviewCommentSource,

    -- ** DocumentReviewerResponseSource
    DocumentReviewerResponseSource (DocumentReviewerResponseSource'),
    newDocumentReviewerResponseSource,

    -- ** DocumentReviews
    DocumentReviews (DocumentReviews'),
    newDocumentReviews,

    -- ** DocumentVersionInfo
    DocumentVersionInfo (DocumentVersionInfo'),
    newDocumentVersionInfo,

    -- ** EffectivePatch
    EffectivePatch (EffectivePatch'),
    newEffectivePatch,

    -- ** FailedCreateAssociation
    FailedCreateAssociation (FailedCreateAssociation'),
    newFailedCreateAssociation,

    -- ** FailureDetails
    FailureDetails (FailureDetails'),
    newFailureDetails,

    -- ** GetResourcePoliciesResponseEntry
    GetResourcePoliciesResponseEntry (GetResourcePoliciesResponseEntry'),
    newGetResourcePoliciesResponseEntry,

    -- ** InstanceAggregatedAssociationOverview
    InstanceAggregatedAssociationOverview (InstanceAggregatedAssociationOverview'),
    newInstanceAggregatedAssociationOverview,

    -- ** InstanceAssociation
    InstanceAssociation (InstanceAssociation'),
    newInstanceAssociation,

    -- ** InstanceAssociationOutputLocation
    InstanceAssociationOutputLocation (InstanceAssociationOutputLocation'),
    newInstanceAssociationOutputLocation,

    -- ** InstanceAssociationOutputUrl
    InstanceAssociationOutputUrl (InstanceAssociationOutputUrl'),
    newInstanceAssociationOutputUrl,

    -- ** InstanceAssociationStatusInfo
    InstanceAssociationStatusInfo (InstanceAssociationStatusInfo'),
    newInstanceAssociationStatusInfo,

    -- ** InstanceInformation
    InstanceInformation (InstanceInformation'),
    newInstanceInformation,

    -- ** InstanceInformationFilter
    InstanceInformationFilter (InstanceInformationFilter'),
    newInstanceInformationFilter,

    -- ** InstanceInformationStringFilter
    InstanceInformationStringFilter (InstanceInformationStringFilter'),
    newInstanceInformationStringFilter,

    -- ** InstancePatchState
    InstancePatchState (InstancePatchState'),
    newInstancePatchState,

    -- ** InstancePatchStateFilter
    InstancePatchStateFilter (InstancePatchStateFilter'),
    newInstancePatchStateFilter,

    -- ** InventoryAggregator
    InventoryAggregator (InventoryAggregator'),
    newInventoryAggregator,

    -- ** InventoryDeletionStatusItem
    InventoryDeletionStatusItem (InventoryDeletionStatusItem'),
    newInventoryDeletionStatusItem,

    -- ** InventoryDeletionSummary
    InventoryDeletionSummary (InventoryDeletionSummary'),
    newInventoryDeletionSummary,

    -- ** InventoryDeletionSummaryItem
    InventoryDeletionSummaryItem (InventoryDeletionSummaryItem'),
    newInventoryDeletionSummaryItem,

    -- ** InventoryFilter
    InventoryFilter (InventoryFilter'),
    newInventoryFilter,

    -- ** InventoryGroup
    InventoryGroup (InventoryGroup'),
    newInventoryGroup,

    -- ** InventoryItem
    InventoryItem (InventoryItem'),
    newInventoryItem,

    -- ** InventoryItemAttribute
    InventoryItemAttribute (InventoryItemAttribute'),
    newInventoryItemAttribute,

    -- ** InventoryItemSchema
    InventoryItemSchema (InventoryItemSchema'),
    newInventoryItemSchema,

    -- ** InventoryResultEntity
    InventoryResultEntity (InventoryResultEntity'),
    newInventoryResultEntity,

    -- ** InventoryResultItem
    InventoryResultItem (InventoryResultItem'),
    newInventoryResultItem,

    -- ** LoggingInfo
    LoggingInfo (LoggingInfo'),
    newLoggingInfo,

    -- ** MaintenanceWindowAutomationParameters
    MaintenanceWindowAutomationParameters (MaintenanceWindowAutomationParameters'),
    newMaintenanceWindowAutomationParameters,

    -- ** MaintenanceWindowExecution
    MaintenanceWindowExecution (MaintenanceWindowExecution'),
    newMaintenanceWindowExecution,

    -- ** MaintenanceWindowExecutionTaskIdentity
    MaintenanceWindowExecutionTaskIdentity (MaintenanceWindowExecutionTaskIdentity'),
    newMaintenanceWindowExecutionTaskIdentity,

    -- ** MaintenanceWindowExecutionTaskInvocationIdentity
    MaintenanceWindowExecutionTaskInvocationIdentity (MaintenanceWindowExecutionTaskInvocationIdentity'),
    newMaintenanceWindowExecutionTaskInvocationIdentity,

    -- ** MaintenanceWindowFilter
    MaintenanceWindowFilter (MaintenanceWindowFilter'),
    newMaintenanceWindowFilter,

    -- ** MaintenanceWindowIdentity
    MaintenanceWindowIdentity (MaintenanceWindowIdentity'),
    newMaintenanceWindowIdentity,

    -- ** MaintenanceWindowIdentityForTarget
    MaintenanceWindowIdentityForTarget (MaintenanceWindowIdentityForTarget'),
    newMaintenanceWindowIdentityForTarget,

    -- ** MaintenanceWindowLambdaParameters
    MaintenanceWindowLambdaParameters (MaintenanceWindowLambdaParameters'),
    newMaintenanceWindowLambdaParameters,

    -- ** MaintenanceWindowRunCommandParameters
    MaintenanceWindowRunCommandParameters (MaintenanceWindowRunCommandParameters'),
    newMaintenanceWindowRunCommandParameters,

    -- ** MaintenanceWindowStepFunctionsParameters
    MaintenanceWindowStepFunctionsParameters (MaintenanceWindowStepFunctionsParameters'),
    newMaintenanceWindowStepFunctionsParameters,

    -- ** MaintenanceWindowTarget
    MaintenanceWindowTarget (MaintenanceWindowTarget'),
    newMaintenanceWindowTarget,

    -- ** MaintenanceWindowTask
    MaintenanceWindowTask (MaintenanceWindowTask'),
    newMaintenanceWindowTask,

    -- ** MaintenanceWindowTaskInvocationParameters
    MaintenanceWindowTaskInvocationParameters (MaintenanceWindowTaskInvocationParameters'),
    newMaintenanceWindowTaskInvocationParameters,

    -- ** MaintenanceWindowTaskParameterValueExpression
    MaintenanceWindowTaskParameterValueExpression (MaintenanceWindowTaskParameterValueExpression'),
    newMaintenanceWindowTaskParameterValueExpression,

    -- ** MetadataValue
    MetadataValue (MetadataValue'),
    newMetadataValue,

    -- ** NonCompliantSummary
    NonCompliantSummary (NonCompliantSummary'),
    newNonCompliantSummary,

    -- ** NotificationConfig
    NotificationConfig (NotificationConfig'),
    newNotificationConfig,

    -- ** OpsAggregator
    OpsAggregator (OpsAggregator'),
    newOpsAggregator,

    -- ** OpsEntity
    OpsEntity (OpsEntity'),
    newOpsEntity,

    -- ** OpsEntityItem
    OpsEntityItem (OpsEntityItem'),
    newOpsEntityItem,

    -- ** OpsFilter
    OpsFilter (OpsFilter'),
    newOpsFilter,

    -- ** OpsItem
    OpsItem (OpsItem'),
    newOpsItem,

    -- ** OpsItemDataValue
    OpsItemDataValue (OpsItemDataValue'),
    newOpsItemDataValue,

    -- ** OpsItemEventFilter
    OpsItemEventFilter (OpsItemEventFilter'),
    newOpsItemEventFilter,

    -- ** OpsItemEventSummary
    OpsItemEventSummary (OpsItemEventSummary'),
    newOpsItemEventSummary,

    -- ** OpsItemFilter
    OpsItemFilter (OpsItemFilter'),
    newOpsItemFilter,

    -- ** OpsItemIdentity
    OpsItemIdentity (OpsItemIdentity'),
    newOpsItemIdentity,

    -- ** OpsItemNotification
    OpsItemNotification (OpsItemNotification'),
    newOpsItemNotification,

    -- ** OpsItemRelatedItemSummary
    OpsItemRelatedItemSummary (OpsItemRelatedItemSummary'),
    newOpsItemRelatedItemSummary,

    -- ** OpsItemRelatedItemsFilter
    OpsItemRelatedItemsFilter (OpsItemRelatedItemsFilter'),
    newOpsItemRelatedItemsFilter,

    -- ** OpsItemSummary
    OpsItemSummary (OpsItemSummary'),
    newOpsItemSummary,

    -- ** OpsMetadata
    OpsMetadata (OpsMetadata'),
    newOpsMetadata,

    -- ** OpsMetadataFilter
    OpsMetadataFilter (OpsMetadataFilter'),
    newOpsMetadataFilter,

    -- ** OpsResultAttribute
    OpsResultAttribute (OpsResultAttribute'),
    newOpsResultAttribute,

    -- ** OutputSource
    OutputSource (OutputSource'),
    newOutputSource,

    -- ** Parameter
    Parameter (Parameter'),
    newParameter,

    -- ** ParameterHistory
    ParameterHistory (ParameterHistory'),
    newParameterHistory,

    -- ** ParameterInlinePolicy
    ParameterInlinePolicy (ParameterInlinePolicy'),
    newParameterInlinePolicy,

    -- ** ParameterMetadata
    ParameterMetadata (ParameterMetadata'),
    newParameterMetadata,

    -- ** ParameterStringFilter
    ParameterStringFilter (ParameterStringFilter'),
    newParameterStringFilter,

    -- ** ParametersFilter
    ParametersFilter (ParametersFilter'),
    newParametersFilter,

    -- ** Patch
    Patch (Patch'),
    newPatch,

    -- ** PatchBaselineIdentity
    PatchBaselineIdentity (PatchBaselineIdentity'),
    newPatchBaselineIdentity,

    -- ** PatchComplianceData
    PatchComplianceData (PatchComplianceData'),
    newPatchComplianceData,

    -- ** PatchFilter
    PatchFilter (PatchFilter'),
    newPatchFilter,

    -- ** PatchFilterGroup
    PatchFilterGroup (PatchFilterGroup'),
    newPatchFilterGroup,

    -- ** PatchGroupPatchBaselineMapping
    PatchGroupPatchBaselineMapping (PatchGroupPatchBaselineMapping'),
    newPatchGroupPatchBaselineMapping,

    -- ** PatchOrchestratorFilter
    PatchOrchestratorFilter (PatchOrchestratorFilter'),
    newPatchOrchestratorFilter,

    -- ** PatchRule
    PatchRule (PatchRule'),
    newPatchRule,

    -- ** PatchRuleGroup
    PatchRuleGroup (PatchRuleGroup'),
    newPatchRuleGroup,

    -- ** PatchSource
    PatchSource (PatchSource'),
    newPatchSource,

    -- ** PatchStatus
    PatchStatus (PatchStatus'),
    newPatchStatus,

    -- ** ProgressCounters
    ProgressCounters (ProgressCounters'),
    newProgressCounters,

    -- ** RegistrationMetadataItem
    RegistrationMetadataItem (RegistrationMetadataItem'),
    newRegistrationMetadataItem,

    -- ** RelatedOpsItem
    RelatedOpsItem (RelatedOpsItem'),
    newRelatedOpsItem,

    -- ** ResolvedTargets
    ResolvedTargets (ResolvedTargets'),
    newResolvedTargets,

    -- ** ResourceComplianceSummaryItem
    ResourceComplianceSummaryItem (ResourceComplianceSummaryItem'),
    newResourceComplianceSummaryItem,

    -- ** ResourceDataSyncAwsOrganizationsSource
    ResourceDataSyncAwsOrganizationsSource (ResourceDataSyncAwsOrganizationsSource'),
    newResourceDataSyncAwsOrganizationsSource,

    -- ** ResourceDataSyncDestinationDataSharing
    ResourceDataSyncDestinationDataSharing (ResourceDataSyncDestinationDataSharing'),
    newResourceDataSyncDestinationDataSharing,

    -- ** ResourceDataSyncItem
    ResourceDataSyncItem (ResourceDataSyncItem'),
    newResourceDataSyncItem,

    -- ** ResourceDataSyncOrganizationalUnit
    ResourceDataSyncOrganizationalUnit (ResourceDataSyncOrganizationalUnit'),
    newResourceDataSyncOrganizationalUnit,

    -- ** ResourceDataSyncS3Destination
    ResourceDataSyncS3Destination (ResourceDataSyncS3Destination'),
    newResourceDataSyncS3Destination,

    -- ** ResourceDataSyncSource
    ResourceDataSyncSource (ResourceDataSyncSource'),
    newResourceDataSyncSource,

    -- ** ResourceDataSyncSourceWithState
    ResourceDataSyncSourceWithState (ResourceDataSyncSourceWithState'),
    newResourceDataSyncSourceWithState,

    -- ** ResultAttribute
    ResultAttribute (ResultAttribute'),
    newResultAttribute,

    -- ** ReviewInformation
    ReviewInformation (ReviewInformation'),
    newReviewInformation,

    -- ** Runbook
    Runbook (Runbook'),
    newRunbook,

    -- ** S3OutputLocation
    S3OutputLocation (S3OutputLocation'),
    newS3OutputLocation,

    -- ** S3OutputUrl
    S3OutputUrl (S3OutputUrl'),
    newS3OutputUrl,

    -- ** ScheduledWindowExecution
    ScheduledWindowExecution (ScheduledWindowExecution'),
    newScheduledWindowExecution,

    -- ** ServiceSetting
    ServiceSetting (ServiceSetting'),
    newServiceSetting,

    -- ** Session
    Session (Session'),
    newSession,

    -- ** SessionFilter
    SessionFilter (SessionFilter'),
    newSessionFilter,

    -- ** SessionManagerOutputUrl
    SessionManagerOutputUrl (SessionManagerOutputUrl'),
    newSessionManagerOutputUrl,

    -- ** SeveritySummary
    SeveritySummary (SeveritySummary'),
    newSeveritySummary,

    -- ** StepExecution
    StepExecution (StepExecution'),
    newStepExecution,

    -- ** StepExecutionFilter
    StepExecutionFilter (StepExecutionFilter'),
    newStepExecutionFilter,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** Target
    Target (Target'),
    newTarget,

    -- ** TargetLocation
    TargetLocation (TargetLocation'),
    newTargetLocation,
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
import Amazonka.SSM.Lens
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
import Amazonka.SSM.Types
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
import Amazonka.SSM.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'SSM'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
