{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SSM
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2014-11-06@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Web Services Systems Manager is a collection of capabilities that
-- helps you automate management tasks such as collecting system inventory,
-- applying operating system (OS) patches, automating the creation of
-- Amazon Machine Images (AMIs), and configuring operating systems (OSs)
-- and applications at scale. Systems Manager lets you remotely and
-- securely manage the configuration of your managed instances. A /managed
-- instance/ is any Amazon Elastic Compute Cloud instance (EC2 instance),
-- or any on-premises server or virtual machine (VM) in your hybrid
-- environment that has been configured for Systems Manager.
--
-- This reference is intended to be used with the
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/ Amazon Web Services Systems Manager User Guide>.
--
-- To get started, verify prerequisites and configure managed instances.
-- For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-setting-up.html Setting up Amazon Web Services Systems Manager>
-- in the /Amazon Web Services Systems Manager User Guide/.
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

    -- ** AutomationDefinitionVersionNotFoundException
    _AutomationDefinitionVersionNotFoundException,

    -- ** InvalidDocumentVersion
    _InvalidDocumentVersion,

    -- ** HierarchyTypeMismatchException
    _HierarchyTypeMismatchException,

    -- ** InvalidSchedule
    _InvalidSchedule,

    -- ** UnsupportedParameterType
    _UnsupportedParameterType,

    -- ** InvalidAutomationStatusUpdateException
    _InvalidAutomationStatusUpdateException,

    -- ** OpsMetadataKeyLimitExceededException
    _OpsMetadataKeyLimitExceededException,

    -- ** InvalidPluginName
    _InvalidPluginName,

    -- ** UnsupportedFeatureRequiredException
    _UnsupportedFeatureRequiredException,

    -- ** InvalidAggregatorException
    _InvalidAggregatorException,

    -- ** FeatureNotAvailableException
    _FeatureNotAvailableException,

    -- ** InvalidAutomationSignalException
    _InvalidAutomationSignalException,

    -- ** ResourceDataSyncCountExceededException
    _ResourceDataSyncCountExceededException,

    -- ** OpsItemRelatedItemAlreadyExistsException
    _OpsItemRelatedItemAlreadyExistsException,

    -- ** UnsupportedPlatformType
    _UnsupportedPlatformType,

    -- ** InvalidFilterValue
    _InvalidFilterValue,

    -- ** InvalidItemContentException
    _InvalidItemContentException,

    -- ** InvalidFilterOption
    _InvalidFilterOption,

    -- ** ParameterPatternMismatchException
    _ParameterPatternMismatchException,

    -- ** InvalidPermissionType
    _InvalidPermissionType,

    -- ** AssociatedInstances
    _AssociatedInstances,

    -- ** UnsupportedOperatingSystem
    _UnsupportedOperatingSystem,

    -- ** InvalidInstanceId
    _InvalidInstanceId,

    -- ** StatusUnchanged
    _StatusUnchanged,

    -- ** InvalidNextToken
    _InvalidNextToken,

    -- ** InvalidInventoryRequestException
    _InvalidInventoryRequestException,

    -- ** InvalidAssociation
    _InvalidAssociation,

    -- ** OpsItemAlreadyExistsException
    _OpsItemAlreadyExistsException,

    -- ** InvalidOutputFolder
    _InvalidOutputFolder,

    -- ** OpsMetadataAlreadyExistsException
    _OpsMetadataAlreadyExistsException,

    -- ** OpsItemLimitExceededException
    _OpsItemLimitExceededException,

    -- ** InvalidActivationId
    _InvalidActivationId,

    -- ** OpsMetadataLimitExceededException
    _OpsMetadataLimitExceededException,

    -- ** ServiceSettingNotFound
    _ServiceSettingNotFound,

    -- ** InvalidResultAttributeException
    _InvalidResultAttributeException,

    -- ** TargetNotConnected
    _TargetNotConnected,

    -- ** ResourceLimitExceededException
    _ResourceLimitExceededException,

    -- ** OpsMetadataTooManyUpdatesException
    _OpsMetadataTooManyUpdatesException,

    -- ** ParameterVersionLabelLimitExceeded
    _ParameterVersionLabelLimitExceeded,

    -- ** ResourceDataSyncInvalidConfigurationException
    _ResourceDataSyncInvalidConfigurationException,

    -- ** InvalidCommandId
    _InvalidCommandId,

    -- ** DuplicateInstanceId
    _DuplicateInstanceId,

    -- ** InvalidResourceType
    _InvalidResourceType,

    -- ** UnsupportedInventorySchemaVersionException
    _UnsupportedInventorySchemaVersionException,

    -- ** InvalidDocument
    _InvalidDocument,

    -- ** IncompatiblePolicyException
    _IncompatiblePolicyException,

    -- ** AutomationDefinitionNotFoundException
    _AutomationDefinitionNotFoundException,

    -- ** InvalidPolicyTypeException
    _InvalidPolicyTypeException,

    -- ** InvalidFilterKey
    _InvalidFilterKey,

    -- ** InvalidAutomationExecutionParametersException
    _InvalidAutomationExecutionParametersException,

    -- ** AutomationExecutionNotFoundException
    _AutomationExecutionNotFoundException,

    -- ** InvalidTypeNameException
    _InvalidTypeNameException,

    -- ** ResourceDataSyncNotFoundException
    _ResourceDataSyncNotFoundException,

    -- ** ParameterMaxVersionLimitExceeded
    _ParameterMaxVersionLimitExceeded,

    -- ** ItemSizeLimitExceededException
    _ItemSizeLimitExceededException,

    -- ** ResourceDataSyncAlreadyExistsException
    _ResourceDataSyncAlreadyExistsException,

    -- ** DoesNotExistException
    _DoesNotExistException,

    -- ** ResourceDataSyncConflictException
    _ResourceDataSyncConflictException,

    -- ** OpsMetadataInvalidArgumentException
    _OpsMetadataInvalidArgumentException,

    -- ** AutomationExecutionLimitExceededException
    _AutomationExecutionLimitExceededException,

    -- ** IdempotentParameterMismatch
    _IdempotentParameterMismatch,

    -- ** InvalidInstanceInformationFilterValue
    _InvalidInstanceInformationFilterValue,

    -- ** ItemContentMismatchException
    _ItemContentMismatchException,

    -- ** ParameterAlreadyExists
    _ParameterAlreadyExists,

    -- ** AssociationAlreadyExists
    _AssociationAlreadyExists,

    -- ** ComplianceTypeCountLimitExceededException
    _ComplianceTypeCountLimitExceededException,

    -- ** InvalidDeleteInventoryParametersException
    _InvalidDeleteInventoryParametersException,

    -- ** InvalidDeletionIdException
    _InvalidDeletionIdException,

    -- ** PoliciesLimitExceededException
    _PoliciesLimitExceededException,

    -- ** InvalidDocumentContent
    _InvalidDocumentContent,

    -- ** ParameterLimitExceeded
    _ParameterLimitExceeded,

    -- ** AssociationLimitExceeded
    _AssociationLimitExceeded,

    -- ** InvalidAssociationVersion
    _InvalidAssociationVersion,

    -- ** AssociationDoesNotExist
    _AssociationDoesNotExist,

    -- ** InvalidPolicyAttributeException
    _InvalidPolicyAttributeException,

    -- ** ParameterNotFound
    _ParameterNotFound,

    -- ** TargetInUseException
    _TargetInUseException,

    -- ** InternalServerError
    _InternalServerError,

    -- ** UnsupportedInventoryItemContextException
    _UnsupportedInventoryItemContextException,

    -- ** OpsMetadataNotFoundException
    _OpsMetadataNotFoundException,

    -- ** AssociationVersionLimitExceeded
    _AssociationVersionLimitExceeded,

    -- ** InvalidRole
    _InvalidRole,

    -- ** TooManyUpdates
    _TooManyUpdates,

    -- ** DuplicateDocumentVersionName
    _DuplicateDocumentVersionName,

    -- ** OpsItemNotFoundException
    _OpsItemNotFoundException,

    -- ** InvalidActivation
    _InvalidActivation,

    -- ** InvalidOptionException
    _InvalidOptionException,

    -- ** InvalidDocumentSchemaVersion
    _InvalidDocumentSchemaVersion,

    -- ** MaxDocumentSizeExceeded
    _MaxDocumentSizeExceeded,

    -- ** ParameterVersionNotFound
    _ParameterVersionNotFound,

    -- ** UnsupportedCalendarException
    _UnsupportedCalendarException,

    -- ** InvalidUpdate
    _InvalidUpdate,

    -- ** CustomSchemaCountLimitExceededException
    _CustomSchemaCountLimitExceededException,

    -- ** AssociationExecutionDoesNotExist
    _AssociationExecutionDoesNotExist,

    -- ** InvalidTarget
    _InvalidTarget,

    -- ** HierarchyLevelLimitExceededException
    _HierarchyLevelLimitExceededException,

    -- ** InvalidInventoryGroupException
    _InvalidInventoryGroupException,

    -- ** InvalidDocumentOperation
    _InvalidDocumentOperation,

    -- ** InvocationDoesNotExist
    _InvocationDoesNotExist,

    -- ** DocumentVersionLimitExceeded
    _DocumentVersionLimitExceeded,

    -- ** InvalidOutputLocation
    _InvalidOutputLocation,

    -- ** InvalidKeyId
    _InvalidKeyId,

    -- ** InvalidParameters
    _InvalidParameters,

    -- ** AutomationDefinitionNotApprovedException
    _AutomationDefinitionNotApprovedException,

    -- ** OpsItemInvalidParameterException
    _OpsItemInvalidParameterException,

    -- ** InvalidResourceId
    _InvalidResourceId,

    -- ** InvalidAllowedPatternException
    _InvalidAllowedPatternException,

    -- ** InvalidNotificationConfig
    _InvalidNotificationConfig,

    -- ** OpsItemRelatedItemAssociationNotFoundException
    _OpsItemRelatedItemAssociationNotFoundException,

    -- ** InvalidInventoryItemContextException
    _InvalidInventoryItemContextException,

    -- ** TotalSizeLimitExceededException
    _TotalSizeLimitExceededException,

    -- ** SubTypeCountLimitExceededException
    _SubTypeCountLimitExceededException,

    -- ** InvalidDocumentType
    _InvalidDocumentType,

    -- ** TooManyTagsError
    _TooManyTagsError,

    -- ** DocumentPermissionLimit
    _DocumentPermissionLimit,

    -- ** AutomationStepNotFoundException
    _AutomationStepNotFoundException,

    -- ** DuplicateDocumentContent
    _DuplicateDocumentContent,

    -- ** DocumentAlreadyExists
    _DocumentAlreadyExists,

    -- ** DocumentLimitExceeded
    _DocumentLimitExceeded,

    -- ** AlreadyExistsException
    _AlreadyExistsException,

    -- ** InvalidFilter
    _InvalidFilter,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- * Waiters
    -- $waiters

    -- ** CommandExecuted
    newCommandExecuted,

    -- * Operations
    -- $operations

    -- ** GetConnectionStatus
    GetConnectionStatus (GetConnectionStatus'),
    newGetConnectionStatus,
    GetConnectionStatusResponse (GetConnectionStatusResponse'),
    newGetConnectionStatusResponse,

    -- ** DescribeInstancePatches (Paginated)
    DescribeInstancePatches (DescribeInstancePatches'),
    newDescribeInstancePatches,
    DescribeInstancePatchesResponse (DescribeInstancePatchesResponse'),
    newDescribeInstancePatchesResponse,

    -- ** GetInventory (Paginated)
    GetInventory (GetInventory'),
    newGetInventory,
    GetInventoryResponse (GetInventoryResponse'),
    newGetInventoryResponse,

    -- ** GetParameters
    GetParameters (GetParameters'),
    newGetParameters,
    GetParametersResponse (GetParametersResponse'),
    newGetParametersResponse,

    -- ** DeletePatchBaseline
    DeletePatchBaseline (DeletePatchBaseline'),
    newDeletePatchBaseline,
    DeletePatchBaselineResponse (DeletePatchBaselineResponse'),
    newDeletePatchBaselineResponse,

    -- ** UpdatePatchBaseline
    UpdatePatchBaseline (UpdatePatchBaseline'),
    newUpdatePatchBaseline,
    UpdatePatchBaselineResponse (UpdatePatchBaselineResponse'),
    newUpdatePatchBaselineResponse,

    -- ** ListOpsItemEvents (Paginated)
    ListOpsItemEvents (ListOpsItemEvents'),
    newListOpsItemEvents,
    ListOpsItemEventsResponse (ListOpsItemEventsResponse'),
    newListOpsItemEventsResponse,

    -- ** TerminateSession
    TerminateSession (TerminateSession'),
    newTerminateSession,
    TerminateSessionResponse (TerminateSessionResponse'),
    newTerminateSessionResponse,

    -- ** GetParameter
    GetParameter (GetParameter'),
    newGetParameter,
    GetParameterResponse (GetParameterResponse'),
    newGetParameterResponse,

    -- ** GetOpsMetadata
    GetOpsMetadata (GetOpsMetadata'),
    newGetOpsMetadata,
    GetOpsMetadataResponse (GetOpsMetadataResponse'),
    newGetOpsMetadataResponse,

    -- ** UpdateDocumentDefaultVersion
    UpdateDocumentDefaultVersion (UpdateDocumentDefaultVersion'),
    newUpdateDocumentDefaultVersion,
    UpdateDocumentDefaultVersionResponse (UpdateDocumentDefaultVersionResponse'),
    newUpdateDocumentDefaultVersionResponse,

    -- ** ListResourceDataSync (Paginated)
    ListResourceDataSync (ListResourceDataSync'),
    newListResourceDataSync,
    ListResourceDataSyncResponse (ListResourceDataSyncResponse'),
    newListResourceDataSyncResponse,

    -- ** GetOpsItem
    GetOpsItem (GetOpsItem'),
    newGetOpsItem,
    GetOpsItemResponse (GetOpsItemResponse'),
    newGetOpsItemResponse,

    -- ** ResumeSession
    ResumeSession (ResumeSession'),
    newResumeSession,
    ResumeSessionResponse (ResumeSessionResponse'),
    newResumeSessionResponse,

    -- ** GetDeployablePatchSnapshotForInstance
    GetDeployablePatchSnapshotForInstance (GetDeployablePatchSnapshotForInstance'),
    newGetDeployablePatchSnapshotForInstance,
    GetDeployablePatchSnapshotForInstanceResponse (GetDeployablePatchSnapshotForInstanceResponse'),
    newGetDeployablePatchSnapshotForInstanceResponse,

    -- ** DescribeParameters (Paginated)
    DescribeParameters (DescribeParameters'),
    newDescribeParameters,
    DescribeParametersResponse (DescribeParametersResponse'),
    newDescribeParametersResponse,

    -- ** DescribeOpsItems (Paginated)
    DescribeOpsItems (DescribeOpsItems'),
    newDescribeOpsItems,
    DescribeOpsItemsResponse (DescribeOpsItemsResponse'),
    newDescribeOpsItemsResponse,

    -- ** GetParametersByPath (Paginated)
    GetParametersByPath (GetParametersByPath'),
    newGetParametersByPath,
    GetParametersByPathResponse (GetParametersByPathResponse'),
    newGetParametersByPathResponse,

    -- ** PutComplianceItems
    PutComplianceItems (PutComplianceItems'),
    newPutComplianceItems,
    PutComplianceItemsResponse (PutComplianceItemsResponse'),
    newPutComplianceItemsResponse,

    -- ** ListDocumentMetadataHistory
    ListDocumentMetadataHistory (ListDocumentMetadataHistory'),
    newListDocumentMetadataHistory,
    ListDocumentMetadataHistoryResponse (ListDocumentMetadataHistoryResponse'),
    newListDocumentMetadataHistoryResponse,

    -- ** DescribeActivations (Paginated)
    DescribeActivations (DescribeActivations'),
    newDescribeActivations,
    DescribeActivationsResponse (DescribeActivationsResponse'),
    newDescribeActivationsResponse,

    -- ** GetMaintenanceWindowTask
    GetMaintenanceWindowTask (GetMaintenanceWindowTask'),
    newGetMaintenanceWindowTask,
    GetMaintenanceWindowTaskResponse (GetMaintenanceWindowTaskResponse'),
    newGetMaintenanceWindowTaskResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DescribeDocument
    DescribeDocument (DescribeDocument'),
    newDescribeDocument,
    DescribeDocumentResponse (DescribeDocumentResponse'),
    newDescribeDocumentResponse,

    -- ** DescribePatchProperties (Paginated)
    DescribePatchProperties (DescribePatchProperties'),
    newDescribePatchProperties,
    DescribePatchPropertiesResponse (DescribePatchPropertiesResponse'),
    newDescribePatchPropertiesResponse,

    -- ** CreateAssociation
    CreateAssociation (CreateAssociation'),
    newCreateAssociation,
    CreateAssociationResponse (CreateAssociationResponse'),
    newCreateAssociationResponse,

    -- ** DeleteActivation
    DeleteActivation (DeleteActivation'),
    newDeleteActivation,
    DeleteActivationResponse (DeleteActivationResponse'),
    newDeleteActivationResponse,

    -- ** DescribeMaintenanceWindowExecutions (Paginated)
    DescribeMaintenanceWindowExecutions (DescribeMaintenanceWindowExecutions'),
    newDescribeMaintenanceWindowExecutions,
    DescribeMaintenanceWindowExecutionsResponse (DescribeMaintenanceWindowExecutionsResponse'),
    newDescribeMaintenanceWindowExecutionsResponse,

    -- ** DescribeMaintenanceWindowsForTarget (Paginated)
    DescribeMaintenanceWindowsForTarget (DescribeMaintenanceWindowsForTarget'),
    newDescribeMaintenanceWindowsForTarget,
    DescribeMaintenanceWindowsForTargetResponse (DescribeMaintenanceWindowsForTargetResponse'),
    newDescribeMaintenanceWindowsForTargetResponse,

    -- ** CreateOpsMetadata
    CreateOpsMetadata (CreateOpsMetadata'),
    newCreateOpsMetadata,
    CreateOpsMetadataResponse (CreateOpsMetadataResponse'),
    newCreateOpsMetadataResponse,

    -- ** StartChangeRequestExecution
    StartChangeRequestExecution (StartChangeRequestExecution'),
    newStartChangeRequestExecution,
    StartChangeRequestExecutionResponse (StartChangeRequestExecutionResponse'),
    newStartChangeRequestExecutionResponse,

    -- ** CancelMaintenanceWindowExecution
    CancelMaintenanceWindowExecution (CancelMaintenanceWindowExecution'),
    newCancelMaintenanceWindowExecution,
    CancelMaintenanceWindowExecutionResponse (CancelMaintenanceWindowExecutionResponse'),
    newCancelMaintenanceWindowExecutionResponse,

    -- ** GetInventorySchema (Paginated)
    GetInventorySchema (GetInventorySchema'),
    newGetInventorySchema,
    GetInventorySchemaResponse (GetInventorySchemaResponse'),
    newGetInventorySchemaResponse,

    -- ** ListComplianceSummaries (Paginated)
    ListComplianceSummaries (ListComplianceSummaries'),
    newListComplianceSummaries,
    ListComplianceSummariesResponse (ListComplianceSummariesResponse'),
    newListComplianceSummariesResponse,

    -- ** StartAutomationExecution
    StartAutomationExecution (StartAutomationExecution'),
    newStartAutomationExecution,
    StartAutomationExecutionResponse (StartAutomationExecutionResponse'),
    newStartAutomationExecutionResponse,

    -- ** CreateOpsItem
    CreateOpsItem (CreateOpsItem'),
    newCreateOpsItem,
    CreateOpsItemResponse (CreateOpsItemResponse'),
    newCreateOpsItemResponse,

    -- ** CreateActivation
    CreateActivation (CreateActivation'),
    newCreateActivation,
    CreateActivationResponse (CreateActivationResponse'),
    newCreateActivationResponse,

    -- ** DeleteMaintenanceWindow
    DeleteMaintenanceWindow (DeleteMaintenanceWindow'),
    newDeleteMaintenanceWindow,
    DeleteMaintenanceWindowResponse (DeleteMaintenanceWindowResponse'),
    newDeleteMaintenanceWindowResponse,

    -- ** UpdateMaintenanceWindow
    UpdateMaintenanceWindow (UpdateMaintenanceWindow'),
    newUpdateMaintenanceWindow,
    UpdateMaintenanceWindowResponse (UpdateMaintenanceWindowResponse'),
    newUpdateMaintenanceWindowResponse,

    -- ** DescribeSessions (Paginated)
    DescribeSessions (DescribeSessions'),
    newDescribeSessions,
    DescribeSessionsResponse (DescribeSessionsResponse'),
    newDescribeSessionsResponse,

    -- ** DescribeMaintenanceWindowExecutionTasks (Paginated)
    DescribeMaintenanceWindowExecutionTasks (DescribeMaintenanceWindowExecutionTasks'),
    newDescribeMaintenanceWindowExecutionTasks,
    DescribeMaintenanceWindowExecutionTasksResponse (DescribeMaintenanceWindowExecutionTasksResponse'),
    newDescribeMaintenanceWindowExecutionTasksResponse,

    -- ** GetDefaultPatchBaseline
    GetDefaultPatchBaseline (GetDefaultPatchBaseline'),
    newGetDefaultPatchBaseline,
    GetDefaultPatchBaselineResponse (GetDefaultPatchBaselineResponse'),
    newGetDefaultPatchBaselineResponse,

    -- ** GetMaintenanceWindowExecutionTask
    GetMaintenanceWindowExecutionTask (GetMaintenanceWindowExecutionTask'),
    newGetMaintenanceWindowExecutionTask,
    GetMaintenanceWindowExecutionTaskResponse (GetMaintenanceWindowExecutionTaskResponse'),
    newGetMaintenanceWindowExecutionTaskResponse,

    -- ** CreateDocument
    CreateDocument (CreateDocument'),
    newCreateDocument,
    CreateDocumentResponse (CreateDocumentResponse'),
    newCreateDocumentResponse,

    -- ** RemoveTagsFromResource
    RemoveTagsFromResource (RemoveTagsFromResource'),
    newRemoveTagsFromResource,
    RemoveTagsFromResourceResponse (RemoveTagsFromResourceResponse'),
    newRemoveTagsFromResourceResponse,

    -- ** GetCalendarState
    GetCalendarState (GetCalendarState'),
    newGetCalendarState,
    GetCalendarStateResponse (GetCalendarStateResponse'),
    newGetCalendarStateResponse,

    -- ** DeleteParameters
    DeleteParameters (DeleteParameters'),
    newDeleteParameters,
    DeleteParametersResponse (DeleteParametersResponse'),
    newDeleteParametersResponse,

    -- ** DescribePatchGroupState
    DescribePatchGroupState (DescribePatchGroupState'),
    newDescribePatchGroupState,
    DescribePatchGroupStateResponse (DescribePatchGroupStateResponse'),
    newDescribePatchGroupStateResponse,

    -- ** ListCommandInvocations (Paginated)
    ListCommandInvocations (ListCommandInvocations'),
    newListCommandInvocations,
    ListCommandInvocationsResponse (ListCommandInvocationsResponse'),
    newListCommandInvocationsResponse,

    -- ** DeregisterTargetFromMaintenanceWindow
    DeregisterTargetFromMaintenanceWindow (DeregisterTargetFromMaintenanceWindow'),
    newDeregisterTargetFromMaintenanceWindow,
    DeregisterTargetFromMaintenanceWindowResponse (DeregisterTargetFromMaintenanceWindowResponse'),
    newDeregisterTargetFromMaintenanceWindowResponse,

    -- ** DescribeEffectivePatchesForPatchBaseline (Paginated)
    DescribeEffectivePatchesForPatchBaseline (DescribeEffectivePatchesForPatchBaseline'),
    newDescribeEffectivePatchesForPatchBaseline,
    DescribeEffectivePatchesForPatchBaselineResponse (DescribeEffectivePatchesForPatchBaselineResponse'),
    newDescribeEffectivePatchesForPatchBaselineResponse,

    -- ** UnlabelParameterVersion
    UnlabelParameterVersion (UnlabelParameterVersion'),
    newUnlabelParameterVersion,
    UnlabelParameterVersionResponse (UnlabelParameterVersionResponse'),
    newUnlabelParameterVersionResponse,

    -- ** DescribeMaintenanceWindowTargets (Paginated)
    DescribeMaintenanceWindowTargets (DescribeMaintenanceWindowTargets'),
    newDescribeMaintenanceWindowTargets,
    DescribeMaintenanceWindowTargetsResponse (DescribeMaintenanceWindowTargetsResponse'),
    newDescribeMaintenanceWindowTargetsResponse,

    -- ** ResetServiceSetting
    ResetServiceSetting (ResetServiceSetting'),
    newResetServiceSetting,
    ResetServiceSettingResponse (ResetServiceSettingResponse'),
    newResetServiceSettingResponse,

    -- ** RegisterPatchBaselineForPatchGroup
    RegisterPatchBaselineForPatchGroup (RegisterPatchBaselineForPatchGroup'),
    newRegisterPatchBaselineForPatchGroup,
    RegisterPatchBaselineForPatchGroupResponse (RegisterPatchBaselineForPatchGroupResponse'),
    newRegisterPatchBaselineForPatchGroupResponse,

    -- ** ListDocuments (Paginated)
    ListDocuments (ListDocuments'),
    newListDocuments,
    ListDocumentsResponse (ListDocumentsResponse'),
    newListDocumentsResponse,

    -- ** DescribeInstancePatchStates (Paginated)
    DescribeInstancePatchStates (DescribeInstancePatchStates'),
    newDescribeInstancePatchStates,
    DescribeInstancePatchStatesResponse (DescribeInstancePatchStatesResponse'),
    newDescribeInstancePatchStatesResponse,

    -- ** GetOpsSummary (Paginated)
    GetOpsSummary (GetOpsSummary'),
    newGetOpsSummary,
    GetOpsSummaryResponse (GetOpsSummaryResponse'),
    newGetOpsSummaryResponse,

    -- ** GetPatchBaselineForPatchGroup
    GetPatchBaselineForPatchGroup (GetPatchBaselineForPatchGroup'),
    newGetPatchBaselineForPatchGroup,
    GetPatchBaselineForPatchGroupResponse (GetPatchBaselineForPatchGroupResponse'),
    newGetPatchBaselineForPatchGroupResponse,

    -- ** UpdateManagedInstanceRole
    UpdateManagedInstanceRole (UpdateManagedInstanceRole'),
    newUpdateManagedInstanceRole,
    UpdateManagedInstanceRoleResponse (UpdateManagedInstanceRoleResponse'),
    newUpdateManagedInstanceRoleResponse,

    -- ** ListComplianceItems (Paginated)
    ListComplianceItems (ListComplianceItems'),
    newListComplianceItems,
    ListComplianceItemsResponse (ListComplianceItemsResponse'),
    newListComplianceItemsResponse,

    -- ** GetDocument
    GetDocument (GetDocument'),
    newGetDocument,
    GetDocumentResponse (GetDocumentResponse'),
    newGetDocumentResponse,

    -- ** DescribeMaintenanceWindowSchedule (Paginated)
    DescribeMaintenanceWindowSchedule (DescribeMaintenanceWindowSchedule'),
    newDescribeMaintenanceWindowSchedule,
    DescribeMaintenanceWindowScheduleResponse (DescribeMaintenanceWindowScheduleResponse'),
    newDescribeMaintenanceWindowScheduleResponse,

    -- ** AddTagsToResource
    AddTagsToResource (AddTagsToResource'),
    newAddTagsToResource,
    AddTagsToResourceResponse (AddTagsToResourceResponse'),
    newAddTagsToResourceResponse,

    -- ** CancelCommand
    CancelCommand (CancelCommand'),
    newCancelCommand,
    CancelCommandResponse (CancelCommandResponse'),
    newCancelCommandResponse,

    -- ** DescribeAutomationStepExecutions (Paginated)
    DescribeAutomationStepExecutions (DescribeAutomationStepExecutions'),
    newDescribeAutomationStepExecutions,
    DescribeAutomationStepExecutionsResponse (DescribeAutomationStepExecutionsResponse'),
    newDescribeAutomationStepExecutionsResponse,

    -- ** GetCommandInvocation
    GetCommandInvocation (GetCommandInvocation'),
    newGetCommandInvocation,
    GetCommandInvocationResponse (GetCommandInvocationResponse'),
    newGetCommandInvocationResponse,

    -- ** DescribeInstancePatchStatesForPatchGroup (Paginated)
    DescribeInstancePatchStatesForPatchGroup (DescribeInstancePatchStatesForPatchGroup'),
    newDescribeInstancePatchStatesForPatchGroup,
    DescribeInstancePatchStatesForPatchGroupResponse (DescribeInstancePatchStatesForPatchGroupResponse'),
    newDescribeInstancePatchStatesForPatchGroupResponse,

    -- ** DeregisterManagedInstance
    DeregisterManagedInstance (DeregisterManagedInstance'),
    newDeregisterManagedInstance,
    DeregisterManagedInstanceResponse (DeregisterManagedInstanceResponse'),
    newDeregisterManagedInstanceResponse,

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

    -- ** ModifyDocumentPermission
    ModifyDocumentPermission (ModifyDocumentPermission'),
    newModifyDocumentPermission,
    ModifyDocumentPermissionResponse (ModifyDocumentPermissionResponse'),
    newModifyDocumentPermissionResponse,

    -- ** UpdateResourceDataSync
    UpdateResourceDataSync (UpdateResourceDataSync'),
    newUpdateResourceDataSync,
    UpdateResourceDataSyncResponse (UpdateResourceDataSyncResponse'),
    newUpdateResourceDataSyncResponse,

    -- ** DeleteResourceDataSync
    DeleteResourceDataSync (DeleteResourceDataSync'),
    newDeleteResourceDataSync,
    DeleteResourceDataSyncResponse (DeleteResourceDataSyncResponse'),
    newDeleteResourceDataSyncResponse,

    -- ** UpdateAssociationStatus
    UpdateAssociationStatus (UpdateAssociationStatus'),
    newUpdateAssociationStatus,
    UpdateAssociationStatusResponse (UpdateAssociationStatusResponse'),
    newUpdateAssociationStatusResponse,

    -- ** DescribeAvailablePatches (Paginated)
    DescribeAvailablePatches (DescribeAvailablePatches'),
    newDescribeAvailablePatches,
    DescribeAvailablePatchesResponse (DescribeAvailablePatchesResponse'),
    newDescribeAvailablePatchesResponse,

    -- ** ListDocumentVersions (Paginated)
    ListDocumentVersions (ListDocumentVersions'),
    newListDocumentVersions,
    ListDocumentVersionsResponse (ListDocumentVersionsResponse'),
    newListDocumentVersionsResponse,

    -- ** DeregisterPatchBaselineForPatchGroup
    DeregisterPatchBaselineForPatchGroup (DeregisterPatchBaselineForPatchGroup'),
    newDeregisterPatchBaselineForPatchGroup,
    DeregisterPatchBaselineForPatchGroupResponse (DeregisterPatchBaselineForPatchGroupResponse'),
    newDeregisterPatchBaselineForPatchGroupResponse,

    -- ** DescribePatchGroups (Paginated)
    DescribePatchGroups (DescribePatchGroups'),
    newDescribePatchGroups,
    DescribePatchGroupsResponse (DescribePatchGroupsResponse'),
    newDescribePatchGroupsResponse,

    -- ** GetMaintenanceWindow
    GetMaintenanceWindow (GetMaintenanceWindow'),
    newGetMaintenanceWindow,
    GetMaintenanceWindowResponse (GetMaintenanceWindowResponse'),
    newGetMaintenanceWindowResponse,

    -- ** DescribeMaintenanceWindows (Paginated)
    DescribeMaintenanceWindows (DescribeMaintenanceWindows'),
    newDescribeMaintenanceWindows,
    DescribeMaintenanceWindowsResponse (DescribeMaintenanceWindowsResponse'),
    newDescribeMaintenanceWindowsResponse,

    -- ** RegisterTaskWithMaintenanceWindow
    RegisterTaskWithMaintenanceWindow (RegisterTaskWithMaintenanceWindow'),
    newRegisterTaskWithMaintenanceWindow,
    RegisterTaskWithMaintenanceWindowResponse (RegisterTaskWithMaintenanceWindowResponse'),
    newRegisterTaskWithMaintenanceWindowResponse,

    -- ** RegisterDefaultPatchBaseline
    RegisterDefaultPatchBaseline (RegisterDefaultPatchBaseline'),
    newRegisterDefaultPatchBaseline,
    RegisterDefaultPatchBaselineResponse (RegisterDefaultPatchBaselineResponse'),
    newRegisterDefaultPatchBaselineResponse,

    -- ** ListResourceComplianceSummaries (Paginated)
    ListResourceComplianceSummaries (ListResourceComplianceSummaries'),
    newListResourceComplianceSummaries,
    ListResourceComplianceSummariesResponse (ListResourceComplianceSummariesResponse'),
    newListResourceComplianceSummariesResponse,

    -- ** AssociateOpsItemRelatedItem
    AssociateOpsItemRelatedItem (AssociateOpsItemRelatedItem'),
    newAssociateOpsItemRelatedItem,
    AssociateOpsItemRelatedItemResponse (AssociateOpsItemRelatedItemResponse'),
    newAssociateOpsItemRelatedItemResponse,

    -- ** ListAssociationVersions (Paginated)
    ListAssociationVersions (ListAssociationVersions'),
    newListAssociationVersions,
    ListAssociationVersionsResponse (ListAssociationVersionsResponse'),
    newListAssociationVersionsResponse,

    -- ** UpdateServiceSetting
    UpdateServiceSetting (UpdateServiceSetting'),
    newUpdateServiceSetting,
    UpdateServiceSettingResponse (UpdateServiceSettingResponse'),
    newUpdateServiceSettingResponse,

    -- ** DescribeMaintenanceWindowTasks (Paginated)
    DescribeMaintenanceWindowTasks (DescribeMaintenanceWindowTasks'),
    newDescribeMaintenanceWindowTasks,
    DescribeMaintenanceWindowTasksResponse (DescribeMaintenanceWindowTasksResponse'),
    newDescribeMaintenanceWindowTasksResponse,

    -- ** DescribeInstanceAssociationsStatus (Paginated)
    DescribeInstanceAssociationsStatus (DescribeInstanceAssociationsStatus'),
    newDescribeInstanceAssociationsStatus,
    DescribeInstanceAssociationsStatusResponse (DescribeInstanceAssociationsStatusResponse'),
    newDescribeInstanceAssociationsStatusResponse,

    -- ** ListOpsItemRelatedItems (Paginated)
    ListOpsItemRelatedItems (ListOpsItemRelatedItems'),
    newListOpsItemRelatedItems,
    ListOpsItemRelatedItemsResponse (ListOpsItemRelatedItemsResponse'),
    newListOpsItemRelatedItemsResponse,

    -- ** DeregisterTaskFromMaintenanceWindow
    DeregisterTaskFromMaintenanceWindow (DeregisterTaskFromMaintenanceWindow'),
    newDeregisterTaskFromMaintenanceWindow,
    DeregisterTaskFromMaintenanceWindowResponse (DeregisterTaskFromMaintenanceWindowResponse'),
    newDeregisterTaskFromMaintenanceWindowResponse,

    -- ** ListInventoryEntries
    ListInventoryEntries (ListInventoryEntries'),
    newListInventoryEntries,
    ListInventoryEntriesResponse (ListInventoryEntriesResponse'),
    newListInventoryEntriesResponse,

    -- ** LabelParameterVersion
    LabelParameterVersion (LabelParameterVersion'),
    newLabelParameterVersion,
    LabelParameterVersionResponse (LabelParameterVersionResponse'),
    newLabelParameterVersionResponse,

    -- ** UpdateMaintenanceWindowTask
    UpdateMaintenanceWindowTask (UpdateMaintenanceWindowTask'),
    newUpdateMaintenanceWindowTask,
    UpdateMaintenanceWindowTaskResponse (UpdateMaintenanceWindowTaskResponse'),
    newUpdateMaintenanceWindowTaskResponse,

    -- ** GetParameterHistory (Paginated)
    GetParameterHistory (GetParameterHistory'),
    newGetParameterHistory,
    GetParameterHistoryResponse (GetParameterHistoryResponse'),
    newGetParameterHistoryResponse,

    -- ** DescribeAssociationExecutions (Paginated)
    DescribeAssociationExecutions (DescribeAssociationExecutions'),
    newDescribeAssociationExecutions,
    DescribeAssociationExecutionsResponse (DescribeAssociationExecutionsResponse'),
    newDescribeAssociationExecutionsResponse,

    -- ** GetServiceSetting
    GetServiceSetting (GetServiceSetting'),
    newGetServiceSetting,
    GetServiceSettingResponse (GetServiceSettingResponse'),
    newGetServiceSettingResponse,

    -- ** StartAssociationsOnce
    StartAssociationsOnce (StartAssociationsOnce'),
    newStartAssociationsOnce,
    StartAssociationsOnceResponse (StartAssociationsOnceResponse'),
    newStartAssociationsOnceResponse,

    -- ** CreateMaintenanceWindow
    CreateMaintenanceWindow (CreateMaintenanceWindow'),
    newCreateMaintenanceWindow,
    CreateMaintenanceWindowResponse (CreateMaintenanceWindowResponse'),
    newCreateMaintenanceWindowResponse,

    -- ** StopAutomationExecution
    StopAutomationExecution (StopAutomationExecution'),
    newStopAutomationExecution,
    StopAutomationExecutionResponse (StopAutomationExecutionResponse'),
    newStopAutomationExecutionResponse,

    -- ** GetMaintenanceWindowExecution
    GetMaintenanceWindowExecution (GetMaintenanceWindowExecution'),
    newGetMaintenanceWindowExecution,
    GetMaintenanceWindowExecutionResponse (GetMaintenanceWindowExecutionResponse'),
    newGetMaintenanceWindowExecutionResponse,

    -- ** SendAutomationSignal
    SendAutomationSignal (SendAutomationSignal'),
    newSendAutomationSignal,
    SendAutomationSignalResponse (SendAutomationSignalResponse'),
    newSendAutomationSignalResponse,

    -- ** DeleteOpsMetadata
    DeleteOpsMetadata (DeleteOpsMetadata'),
    newDeleteOpsMetadata,
    DeleteOpsMetadataResponse (DeleteOpsMetadataResponse'),
    newDeleteOpsMetadataResponse,

    -- ** UpdateOpsMetadata
    UpdateOpsMetadata (UpdateOpsMetadata'),
    newUpdateOpsMetadata,
    UpdateOpsMetadataResponse (UpdateOpsMetadataResponse'),
    newUpdateOpsMetadataResponse,

    -- ** PutParameter
    PutParameter (PutParameter'),
    newPutParameter,
    PutParameterResponse (PutParameterResponse'),
    newPutParameterResponse,

    -- ** DescribeMaintenanceWindowExecutionTaskInvocations (Paginated)
    DescribeMaintenanceWindowExecutionTaskInvocations (DescribeMaintenanceWindowExecutionTaskInvocations'),
    newDescribeMaintenanceWindowExecutionTaskInvocations,
    DescribeMaintenanceWindowExecutionTaskInvocationsResponse (DescribeMaintenanceWindowExecutionTaskInvocationsResponse'),
    newDescribeMaintenanceWindowExecutionTaskInvocationsResponse,

    -- ** GetMaintenanceWindowExecutionTaskInvocation
    GetMaintenanceWindowExecutionTaskInvocation (GetMaintenanceWindowExecutionTaskInvocation'),
    newGetMaintenanceWindowExecutionTaskInvocation,
    GetMaintenanceWindowExecutionTaskInvocationResponse (GetMaintenanceWindowExecutionTaskInvocationResponse'),
    newGetMaintenanceWindowExecutionTaskInvocationResponse,

    -- ** DeleteParameter
    DeleteParameter (DeleteParameter'),
    newDeleteParameter,
    DeleteParameterResponse (DeleteParameterResponse'),
    newDeleteParameterResponse,

    -- ** DescribeInstanceInformation (Paginated)
    DescribeInstanceInformation (DescribeInstanceInformation'),
    newDescribeInstanceInformation,
    DescribeInstanceInformationResponse (DescribeInstanceInformationResponse'),
    newDescribeInstanceInformationResponse,

    -- ** ListAssociations (Paginated)
    ListAssociations (ListAssociations'),
    newListAssociations,
    ListAssociationsResponse (ListAssociationsResponse'),
    newListAssociationsResponse,

    -- ** UpdateOpsItem
    UpdateOpsItem (UpdateOpsItem'),
    newUpdateOpsItem,
    UpdateOpsItemResponse (UpdateOpsItemResponse'),
    newUpdateOpsItemResponse,

    -- ** DeleteAssociation
    DeleteAssociation (DeleteAssociation'),
    newDeleteAssociation,
    DeleteAssociationResponse (DeleteAssociationResponse'),
    newDeleteAssociationResponse,

    -- ** UpdateAssociation
    UpdateAssociation (UpdateAssociation'),
    newUpdateAssociation,
    UpdateAssociationResponse (UpdateAssociationResponse'),
    newUpdateAssociationResponse,

    -- ** DescribeInventoryDeletions (Paginated)
    DescribeInventoryDeletions (DescribeInventoryDeletions'),
    newDescribeInventoryDeletions,
    DescribeInventoryDeletionsResponse (DescribeInventoryDeletionsResponse'),
    newDescribeInventoryDeletionsResponse,

    -- ** DeleteInventory
    DeleteInventory (DeleteInventory'),
    newDeleteInventory,
    DeleteInventoryResponse (DeleteInventoryResponse'),
    newDeleteInventoryResponse,

    -- ** PutInventory
    PutInventory (PutInventory'),
    newPutInventory,
    PutInventoryResponse (PutInventoryResponse'),
    newPutInventoryResponse,

    -- ** UpdateDocumentMetadata
    UpdateDocumentMetadata (UpdateDocumentMetadata'),
    newUpdateDocumentMetadata,
    UpdateDocumentMetadataResponse (UpdateDocumentMetadataResponse'),
    newUpdateDocumentMetadataResponse,

    -- ** ListOpsMetadata (Paginated)
    ListOpsMetadata (ListOpsMetadata'),
    newListOpsMetadata,
    ListOpsMetadataResponse (ListOpsMetadataResponse'),
    newListOpsMetadataResponse,

    -- ** DescribeEffectiveInstanceAssociations (Paginated)
    DescribeEffectiveInstanceAssociations (DescribeEffectiveInstanceAssociations'),
    newDescribeEffectiveInstanceAssociations,
    DescribeEffectiveInstanceAssociationsResponse (DescribeEffectiveInstanceAssociationsResponse'),
    newDescribeEffectiveInstanceAssociationsResponse,

    -- ** DescribeAutomationExecutions (Paginated)
    DescribeAutomationExecutions (DescribeAutomationExecutions'),
    newDescribeAutomationExecutions,
    DescribeAutomationExecutionsResponse (DescribeAutomationExecutionsResponse'),
    newDescribeAutomationExecutionsResponse,

    -- ** GetAutomationExecution
    GetAutomationExecution (GetAutomationExecution'),
    newGetAutomationExecution,
    GetAutomationExecutionResponse (GetAutomationExecutionResponse'),
    newGetAutomationExecutionResponse,

    -- ** SendCommand
    SendCommand (SendCommand'),
    newSendCommand,
    SendCommandResponse (SendCommandResponse'),
    newSendCommandResponse,

    -- ** DescribePatchBaselines (Paginated)
    DescribePatchBaselines (DescribePatchBaselines'),
    newDescribePatchBaselines,
    DescribePatchBaselinesResponse (DescribePatchBaselinesResponse'),
    newDescribePatchBaselinesResponse,

    -- ** GetPatchBaseline
    GetPatchBaseline (GetPatchBaseline'),
    newGetPatchBaseline,
    GetPatchBaselineResponse (GetPatchBaselineResponse'),
    newGetPatchBaselineResponse,

    -- ** RegisterTargetWithMaintenanceWindow
    RegisterTargetWithMaintenanceWindow (RegisterTargetWithMaintenanceWindow'),
    newRegisterTargetWithMaintenanceWindow,
    RegisterTargetWithMaintenanceWindowResponse (RegisterTargetWithMaintenanceWindowResponse'),
    newRegisterTargetWithMaintenanceWindowResponse,

    -- ** StartSession
    StartSession (StartSession'),
    newStartSession,
    StartSessionResponse (StartSessionResponse'),
    newStartSessionResponse,

    -- ** ListCommands (Paginated)
    ListCommands (ListCommands'),
    newListCommands,
    ListCommandsResponse (ListCommandsResponse'),
    newListCommandsResponse,

    -- ** UpdateDocument
    UpdateDocument (UpdateDocument'),
    newUpdateDocument,
    UpdateDocumentResponse (UpdateDocumentResponse'),
    newUpdateDocumentResponse,

    -- ** DeleteDocument
    DeleteDocument (DeleteDocument'),
    newDeleteDocument,
    DeleteDocumentResponse (DeleteDocumentResponse'),
    newDeleteDocumentResponse,

    -- ** DescribeDocumentPermission
    DescribeDocumentPermission (DescribeDocumentPermission'),
    newDescribeDocumentPermission,
    DescribeDocumentPermissionResponse (DescribeDocumentPermissionResponse'),
    newDescribeDocumentPermissionResponse,

    -- ** CreateAssociationBatch
    CreateAssociationBatch (CreateAssociationBatch'),
    newCreateAssociationBatch,
    CreateAssociationBatchResponse (CreateAssociationBatchResponse'),
    newCreateAssociationBatchResponse,

    -- ** UpdateMaintenanceWindowTarget
    UpdateMaintenanceWindowTarget (UpdateMaintenanceWindowTarget'),
    newUpdateMaintenanceWindowTarget,
    UpdateMaintenanceWindowTargetResponse (UpdateMaintenanceWindowTargetResponse'),
    newUpdateMaintenanceWindowTargetResponse,

    -- ** CreateResourceDataSync
    CreateResourceDataSync (CreateResourceDataSync'),
    newCreateResourceDataSync,
    CreateResourceDataSyncResponse (CreateResourceDataSyncResponse'),
    newCreateResourceDataSyncResponse,

    -- ** CreatePatchBaseline
    CreatePatchBaseline (CreatePatchBaseline'),
    newCreatePatchBaseline,
    CreatePatchBaselineResponse (CreatePatchBaselineResponse'),
    newCreatePatchBaselineResponse,

    -- ** DisassociateOpsItemRelatedItem
    DisassociateOpsItemRelatedItem (DisassociateOpsItemRelatedItem'),
    newDisassociateOpsItemRelatedItem,
    DisassociateOpsItemRelatedItemResponse (DisassociateOpsItemRelatedItemResponse'),
    newDisassociateOpsItemRelatedItemResponse,

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
