{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Systems Manager__
--
-- AWS Systems Manager is a collection of capabilities that helps you automate management tasks such as collecting system inventory, applying operating system (OS) patches, automating the creation of Amazon Machine Images (AMIs), and configuring operating systems (OSs) and applications at scale. Systems Manager lets you remotely and securely manage the configuration of your managed instances. A /managed instance/ is any Amazon Elastic Compute Cloud instance (EC2 instance), or any on-premises server or virtual machine (VM) in your hybrid environment that has been configured for Systems Manager.
-- This reference is intended to be used with the <https://docs.aws.amazon.com/systems-manager/latest/userguide/ AWS Systems Manager User Guide> .
-- To get started, verify prerequisites and configure managed instances. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-setting-up.html Setting up AWS Systems Manager> in the /AWS Systems Manager User Guide/ .
-- For information about other API actions you can perform on EC2 instances, see the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ Amazon EC2 API Reference> . For information about how to use a Query API, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/making-api-requests.html Making API requests> .
module Network.AWS.SSM
  ( -- * Service configuration
    mkServiceConfig,

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

    -- ** OpsItemLimitExceededException
    _OpsItemLimitExceededException,

    -- ** InvalidActivationId
    _InvalidActivationId,

    -- ** ServiceSettingNotFound
    _ServiceSettingNotFound,

    -- ** InvalidResultAttributeException
    _InvalidResultAttributeException,

    -- ** TargetNotConnected
    _TargetNotConnected,

    -- ** ResourceLimitExceededException
    _ResourceLimitExceededException,

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

    -- ** OpsItemInvalidParameterException
    _OpsItemInvalidParameterException,

    -- ** InvalidResourceId
    _InvalidResourceId,

    -- ** InvalidAllowedPatternException
    _InvalidAllowedPatternException,

    -- ** InvalidNotificationConfig
    _InvalidNotificationConfig,

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
    mkCommandExecuted,

    -- * Operations
    -- $operations

    -- ** GetConnectionStatus
    module Network.AWS.SSM.GetConnectionStatus,

    -- ** DescribeInstancePatches (Paginated)
    module Network.AWS.SSM.DescribeInstancePatches,

    -- ** GetInventory (Paginated)
    module Network.AWS.SSM.GetInventory,

    -- ** GetParameters
    module Network.AWS.SSM.GetParameters,

    -- ** DeletePatchBaseline
    module Network.AWS.SSM.DeletePatchBaseline,

    -- ** UpdatePatchBaseline
    module Network.AWS.SSM.UpdatePatchBaseline,

    -- ** TerminateSession
    module Network.AWS.SSM.TerminateSession,

    -- ** GetParameter
    module Network.AWS.SSM.GetParameter,

    -- ** UpdateDocumentDefaultVersion
    module Network.AWS.SSM.UpdateDocumentDefaultVersion,

    -- ** ListResourceDataSync (Paginated)
    module Network.AWS.SSM.ListResourceDataSync,

    -- ** GetOpsItem
    module Network.AWS.SSM.GetOpsItem,

    -- ** ResumeSession
    module Network.AWS.SSM.ResumeSession,

    -- ** GetDeployablePatchSnapshotForInstance
    module Network.AWS.SSM.GetDeployablePatchSnapshotForInstance,

    -- ** DescribeParameters (Paginated)
    module Network.AWS.SSM.DescribeParameters,

    -- ** DescribeOpsItems (Paginated)
    module Network.AWS.SSM.DescribeOpsItems,

    -- ** GetParametersByPath (Paginated)
    module Network.AWS.SSM.GetParametersByPath,

    -- ** PutComplianceItems
    module Network.AWS.SSM.PutComplianceItems,

    -- ** DescribeActivations (Paginated)
    module Network.AWS.SSM.DescribeActivations,

    -- ** GetMaintenanceWindowTask
    module Network.AWS.SSM.GetMaintenanceWindowTask,

    -- ** ListTagsForResource
    module Network.AWS.SSM.ListTagsForResource,

    -- ** DescribeDocument
    module Network.AWS.SSM.DescribeDocument,

    -- ** DescribePatchProperties (Paginated)
    module Network.AWS.SSM.DescribePatchProperties,

    -- ** CreateAssociation
    module Network.AWS.SSM.CreateAssociation,

    -- ** DeleteActivation
    module Network.AWS.SSM.DeleteActivation,

    -- ** DescribeMaintenanceWindowExecutions (Paginated)
    module Network.AWS.SSM.DescribeMaintenanceWindowExecutions,

    -- ** DescribeMaintenanceWindowsForTarget (Paginated)
    module Network.AWS.SSM.DescribeMaintenanceWindowsForTarget,

    -- ** CancelMaintenanceWindowExecution
    module Network.AWS.SSM.CancelMaintenanceWindowExecution,

    -- ** GetInventorySchema (Paginated)
    module Network.AWS.SSM.GetInventorySchema,

    -- ** ListComplianceSummaries (Paginated)
    module Network.AWS.SSM.ListComplianceSummaries,

    -- ** StartAutomationExecution
    module Network.AWS.SSM.StartAutomationExecution,

    -- ** CreateOpsItem
    module Network.AWS.SSM.CreateOpsItem,

    -- ** CreateActivation
    module Network.AWS.SSM.CreateActivation,

    -- ** DeleteMaintenanceWindow
    module Network.AWS.SSM.DeleteMaintenanceWindow,

    -- ** UpdateMaintenanceWindow
    module Network.AWS.SSM.UpdateMaintenanceWindow,

    -- ** DescribeSessions (Paginated)
    module Network.AWS.SSM.DescribeSessions,

    -- ** DescribeMaintenanceWindowExecutionTasks (Paginated)
    module Network.AWS.SSM.DescribeMaintenanceWindowExecutionTasks,

    -- ** GetDefaultPatchBaseline
    module Network.AWS.SSM.GetDefaultPatchBaseline,

    -- ** GetMaintenanceWindowExecutionTask
    module Network.AWS.SSM.GetMaintenanceWindowExecutionTask,

    -- ** CreateDocument
    module Network.AWS.SSM.CreateDocument,

    -- ** RemoveTagsFromResource
    module Network.AWS.SSM.RemoveTagsFromResource,

    -- ** GetCalendarState
    module Network.AWS.SSM.GetCalendarState,

    -- ** DeleteParameters
    module Network.AWS.SSM.DeleteParameters,

    -- ** DescribePatchGroupState
    module Network.AWS.SSM.DescribePatchGroupState,

    -- ** ListCommandInvocations (Paginated)
    module Network.AWS.SSM.ListCommandInvocations,

    -- ** DeregisterTargetFromMaintenanceWindow
    module Network.AWS.SSM.DeregisterTargetFromMaintenanceWindow,

    -- ** DescribeEffectivePatchesForPatchBaseline (Paginated)
    module Network.AWS.SSM.DescribeEffectivePatchesForPatchBaseline,

    -- ** DescribeMaintenanceWindowTargets (Paginated)
    module Network.AWS.SSM.DescribeMaintenanceWindowTargets,

    -- ** ResetServiceSetting
    module Network.AWS.SSM.ResetServiceSetting,

    -- ** RegisterPatchBaselineForPatchGroup
    module Network.AWS.SSM.RegisterPatchBaselineForPatchGroup,

    -- ** ListDocuments (Paginated)
    module Network.AWS.SSM.ListDocuments,

    -- ** DescribeInstancePatchStates (Paginated)
    module Network.AWS.SSM.DescribeInstancePatchStates,

    -- ** GetOpsSummary (Paginated)
    module Network.AWS.SSM.GetOpsSummary,

    -- ** GetPatchBaselineForPatchGroup
    module Network.AWS.SSM.GetPatchBaselineForPatchGroup,

    -- ** UpdateManagedInstanceRole
    module Network.AWS.SSM.UpdateManagedInstanceRole,

    -- ** ListComplianceItems (Paginated)
    module Network.AWS.SSM.ListComplianceItems,

    -- ** GetDocument
    module Network.AWS.SSM.GetDocument,

    -- ** DescribeMaintenanceWindowSchedule (Paginated)
    module Network.AWS.SSM.DescribeMaintenanceWindowSchedule,

    -- ** AddTagsToResource
    module Network.AWS.SSM.AddTagsToResource,

    -- ** CancelCommand
    module Network.AWS.SSM.CancelCommand,

    -- ** DescribeAutomationStepExecutions (Paginated)
    module Network.AWS.SSM.DescribeAutomationStepExecutions,

    -- ** GetCommandInvocation
    module Network.AWS.SSM.GetCommandInvocation,

    -- ** DescribeInstancePatchStatesForPatchGroup (Paginated)
    module Network.AWS.SSM.DescribeInstancePatchStatesForPatchGroup,

    -- ** DeregisterManagedInstance
    module Network.AWS.SSM.DeregisterManagedInstance,

    -- ** DescribeAssociation
    module Network.AWS.SSM.DescribeAssociation,

    -- ** DescribeAssociationExecutionTargets (Paginated)
    module Network.AWS.SSM.DescribeAssociationExecutionTargets,

    -- ** ModifyDocumentPermission
    module Network.AWS.SSM.ModifyDocumentPermission,

    -- ** UpdateResourceDataSync
    module Network.AWS.SSM.UpdateResourceDataSync,

    -- ** DeleteResourceDataSync
    module Network.AWS.SSM.DeleteResourceDataSync,

    -- ** UpdateAssociationStatus
    module Network.AWS.SSM.UpdateAssociationStatus,

    -- ** DescribeAvailablePatches (Paginated)
    module Network.AWS.SSM.DescribeAvailablePatches,

    -- ** ListDocumentVersions (Paginated)
    module Network.AWS.SSM.ListDocumentVersions,

    -- ** DeregisterPatchBaselineForPatchGroup
    module Network.AWS.SSM.DeregisterPatchBaselineForPatchGroup,

    -- ** DescribePatchGroups (Paginated)
    module Network.AWS.SSM.DescribePatchGroups,

    -- ** GetMaintenanceWindow
    module Network.AWS.SSM.GetMaintenanceWindow,

    -- ** DescribeMaintenanceWindows (Paginated)
    module Network.AWS.SSM.DescribeMaintenanceWindows,

    -- ** RegisterTaskWithMaintenanceWindow
    module Network.AWS.SSM.RegisterTaskWithMaintenanceWindow,

    -- ** RegisterDefaultPatchBaseline
    module Network.AWS.SSM.RegisterDefaultPatchBaseline,

    -- ** ListResourceComplianceSummaries (Paginated)
    module Network.AWS.SSM.ListResourceComplianceSummaries,

    -- ** ListAssociationVersions (Paginated)
    module Network.AWS.SSM.ListAssociationVersions,

    -- ** UpdateServiceSetting
    module Network.AWS.SSM.UpdateServiceSetting,

    -- ** DescribeMaintenanceWindowTasks (Paginated)
    module Network.AWS.SSM.DescribeMaintenanceWindowTasks,

    -- ** DescribeInstanceAssociationsStatus (Paginated)
    module Network.AWS.SSM.DescribeInstanceAssociationsStatus,

    -- ** DeregisterTaskFromMaintenanceWindow
    module Network.AWS.SSM.DeregisterTaskFromMaintenanceWindow,

    -- ** ListInventoryEntries
    module Network.AWS.SSM.ListInventoryEntries,

    -- ** LabelParameterVersion
    module Network.AWS.SSM.LabelParameterVersion,

    -- ** UpdateMaintenanceWindowTask
    module Network.AWS.SSM.UpdateMaintenanceWindowTask,

    -- ** GetParameterHistory (Paginated)
    module Network.AWS.SSM.GetParameterHistory,

    -- ** DescribeAssociationExecutions (Paginated)
    module Network.AWS.SSM.DescribeAssociationExecutions,

    -- ** GetServiceSetting
    module Network.AWS.SSM.GetServiceSetting,

    -- ** StartAssociationsOnce
    module Network.AWS.SSM.StartAssociationsOnce,

    -- ** CreateMaintenanceWindow
    module Network.AWS.SSM.CreateMaintenanceWindow,

    -- ** StopAutomationExecution
    module Network.AWS.SSM.StopAutomationExecution,

    -- ** GetMaintenanceWindowExecution
    module Network.AWS.SSM.GetMaintenanceWindowExecution,

    -- ** SendAutomationSignal
    module Network.AWS.SSM.SendAutomationSignal,

    -- ** PutParameter
    module Network.AWS.SSM.PutParameter,

    -- ** DescribeMaintenanceWindowExecutionTaskInvocations (Paginated)
    module Network.AWS.SSM.DescribeMaintenanceWindowExecutionTaskInvocations,

    -- ** GetMaintenanceWindowExecutionTaskInvocation
    module Network.AWS.SSM.GetMaintenanceWindowExecutionTaskInvocation,

    -- ** DeleteParameter
    module Network.AWS.SSM.DeleteParameter,

    -- ** DescribeInstanceInformation (Paginated)
    module Network.AWS.SSM.DescribeInstanceInformation,

    -- ** ListAssociations (Paginated)
    module Network.AWS.SSM.ListAssociations,

    -- ** UpdateOpsItem
    module Network.AWS.SSM.UpdateOpsItem,

    -- ** DeleteAssociation
    module Network.AWS.SSM.DeleteAssociation,

    -- ** UpdateAssociation
    module Network.AWS.SSM.UpdateAssociation,

    -- ** DescribeInventoryDeletions (Paginated)
    module Network.AWS.SSM.DescribeInventoryDeletions,

    -- ** DeleteInventory
    module Network.AWS.SSM.DeleteInventory,

    -- ** PutInventory
    module Network.AWS.SSM.PutInventory,

    -- ** DescribeEffectiveInstanceAssociations (Paginated)
    module Network.AWS.SSM.DescribeEffectiveInstanceAssociations,

    -- ** DescribeAutomationExecutions (Paginated)
    module Network.AWS.SSM.DescribeAutomationExecutions,

    -- ** GetAutomationExecution
    module Network.AWS.SSM.GetAutomationExecution,

    -- ** SendCommand
    module Network.AWS.SSM.SendCommand,

    -- ** DescribePatchBaselines (Paginated)
    module Network.AWS.SSM.DescribePatchBaselines,

    -- ** GetPatchBaseline
    module Network.AWS.SSM.GetPatchBaseline,

    -- ** RegisterTargetWithMaintenanceWindow
    module Network.AWS.SSM.RegisterTargetWithMaintenanceWindow,

    -- ** StartSession
    module Network.AWS.SSM.StartSession,

    -- ** ListCommands (Paginated)
    module Network.AWS.SSM.ListCommands,

    -- ** UpdateDocument
    module Network.AWS.SSM.UpdateDocument,

    -- ** DeleteDocument
    module Network.AWS.SSM.DeleteDocument,

    -- ** DescribeDocumentPermission
    module Network.AWS.SSM.DescribeDocumentPermission,

    -- ** CreateAssociationBatch
    module Network.AWS.SSM.CreateAssociationBatch,

    -- ** UpdateMaintenanceWindowTarget
    module Network.AWS.SSM.UpdateMaintenanceWindowTarget,

    -- ** CreateResourceDataSync
    module Network.AWS.SSM.CreateResourceDataSync,

    -- ** CreatePatchBaseline
    module Network.AWS.SSM.CreatePatchBaseline,

    -- * Types

    -- ** AssociationExecutionFilterKey
    AssociationExecutionFilterKey (..),

    -- ** NonCompliantSummary
    NonCompliantSummary (..),
    mkNonCompliantSummary,
    ncsNonCompliantCount,
    ncsSeveritySummary,

    -- ** BaselineName
    BaselineName (..),

    -- ** MaintenanceWindowLambdaParameters
    MaintenanceWindowLambdaParameters (..),
    mkMaintenanceWindowLambdaParameters,
    mwlpClientContext,
    mwlpPayload,
    mwlpQualifier,

    -- ** PatchComplianceDataState
    PatchComplianceDataState (..),

    -- ** AssociationId
    AssociationId (..),

    -- ** PatchLanguage
    PatchLanguage (..),

    -- ** SharedDocumentVersion
    SharedDocumentVersion (..),

    -- ** OpsFilterOperatorType
    OpsFilterOperatorType (..),

    -- ** OpsItemId
    OpsItemId (..),

    -- ** IdempotencyToken
    IdempotencyToken (..),

    -- ** InstanceId
    InstanceId (..),

    -- ** MaintenanceWindowSchedule
    MaintenanceWindowSchedule (..),

    -- ** ComplianceSummaryItem
    ComplianceSummaryItem (..),
    mkComplianceSummaryItem,
    csiComplianceType,
    csiCompliantSummary,
    csiNonCompliantSummary,

    -- ** InventoryItemAttributeName
    InventoryItemAttributeName (..),

    -- ** CloudWatchLogGroupName
    CloudWatchLogGroupName (..),

    -- ** PingStatus
    PingStatus (..),

    -- ** AttachmentsSourceValue
    AttachmentsSourceValue (..),

    -- ** MaintenanceWindowStepFunctionsInput
    MaintenanceWindowStepFunctionsInput (..),

    -- ** ComplianceItemEntry
    ComplianceItemEntry (..),
    mkComplianceItemEntry,
    cieSeverity,
    cieStatus,
    cieDetails,
    cieId,
    cieTitle,

    -- ** ParameterMetadata
    ParameterMetadata (..),
    mkParameterMetadata,
    pmAllowedPattern,
    pmDataType,
    pmDescription,
    pmKeyId,
    pmLastModifiedDate,
    pmLastModifiedUser,
    pmName,
    pmPolicies,
    pmTier,
    pmType,
    pmVersion,

    -- ** MaintenanceWindowTaskType
    MaintenanceWindowTaskType (..),

    -- ** ParameterStringFilter
    ParameterStringFilter (..),
    mkParameterStringFilter,
    psfKey,
    psfOption,
    psfValues,

    -- ** DocumentSchemaVersion
    DocumentSchemaVersion (..),

    -- ** PatchVersion
    PatchVersion (..),

    -- ** PatchContentUrl
    PatchContentUrl (..),

    -- ** CommandFilterValue
    CommandFilterValue (..),

    -- ** OpsItemSource
    OpsItemSource (..),

    -- ** MaintenanceWindowTaskParameterValueExpression
    MaintenanceWindowTaskParameterValueExpression (..),
    mkMaintenanceWindowTaskParameterValueExpression,
    mwtpveValues,

    -- ** CreateAssociationBatchRequestEntry
    CreateAssociationBatchRequestEntry (..),
    mkCreateAssociationBatchRequestEntry,
    cabreName,
    cabreApplyOnlyAtCronInterval,
    cabreAssociationName,
    cabreAutomationTargetParameterName,
    cabreComplianceSeverity,
    cabreDocumentVersion,
    cabreInstanceId,
    cabreMaxConcurrency,
    cabreMaxErrors,
    cabreOutputLocation,
    cabreParameters,
    cabreScheduleExpression,
    cabreSyncCompliance,
    cabreTargets,

    -- ** PatchKbNumber
    PatchKbNumber (..),

    -- ** AgentErrorCode
    AgentErrorCode (..),

    -- ** ComplianceStringFilterKey
    ComplianceStringFilterKey (..),

    -- ** OpsItemDataValueString
    OpsItemDataValueString (..),

    -- ** AccountSharingInfo
    AccountSharingInfo (..),
    mkAccountSharingInfo,
    asiAccountId,
    asiSharedDocumentVersion,

    -- ** Command
    Command (..),
    mkCommand,
    cCloudWatchOutputConfig,
    cCommandId,
    cComment,
    cCompletedCount,
    cDeliveryTimedOutCount,
    cDocumentName,
    cDocumentVersion,
    cErrorCount,
    cExpiresAfter,
    cInstanceIds,
    cMaxConcurrency,
    cMaxErrors,
    cNotificationConfig,
    cOutputS3BucketName,
    cOutputS3KeyPrefix,
    cOutputS3Region,
    cParameters,
    cRequestedDateTime,
    cServiceRole,
    cStatus,
    cStatusDetails,
    cTargetCount,
    cTargets,
    cTimeoutSeconds,

    -- ** ParameterLabel
    ParameterLabel (..),

    -- ** SessionManagerParameterValue
    SessionManagerParameterValue (..),

    -- ** ComplianceResourceType
    ComplianceResourceType (..),

    -- ** SessionOwner
    SessionOwner (..),

    -- ** MaintenanceWindowIdentity
    MaintenanceWindowIdentity (..),
    mkMaintenanceWindowIdentity,
    mwiCutoff,
    mwiDescription,
    mwiDuration,
    mwiEnabled,
    mwiEndDate,
    mwiName,
    mwiNextExecutionTime,
    mwiSchedule,
    mwiScheduleOffset,
    mwiScheduleTimezone,
    mwiStartDate,
    mwiWindowId,

    -- ** InventoryDeletionSummaryItem
    InventoryDeletionSummaryItem (..),
    mkInventoryDeletionSummaryItem,
    idsiCount,
    idsiRemainingCount,
    idsiVersion,

    -- ** MaintenanceWindowExecutionStatus
    MaintenanceWindowExecutionStatus (..),

    -- ** DocumentFilter
    DocumentFilter (..),
    mkDocumentFilter,
    dfKey,
    dfValue,

    -- ** ScheduledWindowExecution
    ScheduledWindowExecution (..),
    mkScheduledWindowExecution,
    sweExecutionTime,
    sweName,
    sweWindowId,

    -- ** StreamUrl
    StreamUrl (..),

    -- ** InstancePatchStateFilterKey
    InstancePatchStateFilterKey (..),

    -- ** StepExecutionFilterKey
    StepExecutionFilterKey (..),

    -- ** PatchRuleGroup
    PatchRuleGroup (..),
    mkPatchRuleGroup,
    prgPatchRules,

    -- ** OpsItemDescription
    OpsItemDescription (..),

    -- ** ParameterStringFilterValue
    ParameterStringFilterValue (..),

    -- ** IPAddress
    IPAddress (..),

    -- ** InventoryItem
    InventoryItem (..),
    mkInventoryItem,
    iiTypeName,
    iiSchemaVersion,
    iiCaptureTime,
    iiContent,
    iiContentHash,
    iiContext,

    -- ** DocumentType
    DocumentType (..),

    -- ** NotificationConfig
    NotificationConfig (..),
    mkNotificationConfig,
    ncNotificationArn,
    ncNotificationEvents,
    ncNotificationType,

    -- ** AttributeValue
    AttributeValue (..),

    -- ** ResourceId
    ResourceId (..),

    -- ** ParameterDescription
    ParameterDescription (..),

    -- ** AssociationDescription
    AssociationDescription (..),
    mkAssociationDescription,
    adApplyOnlyAtCronInterval,
    adAssociationId,
    adAssociationName,
    adAssociationVersion,
    adAutomationTargetParameterName,
    adComplianceSeverity,
    adDate,
    adDocumentVersion,
    adInstanceId,
    adLastExecutionDate,
    adLastSuccessfulExecutionDate,
    adLastUpdateAssociationDate,
    adMaxConcurrency,
    adMaxErrors,
    adName,
    adOutputLocation,
    adOverview,
    adParameters,
    adScheduleExpression,
    adStatus,
    adSyncCompliance,
    adTargets,

    -- ** InventoryDeletionStatusItem
    InventoryDeletionStatusItem (..),
    mkInventoryDeletionStatusItem,
    idsiDeletionId,
    idsiDeletionStartTime,
    idsiDeletionSummary,
    idsiLastStatus,
    idsiLastStatusMessage,
    idsiLastStatusUpdateTime,
    idsiTypeName,

    -- ** S3KeyPrefix
    S3KeyPrefix (..),

    -- ** ComplianceTypeName
    ComplianceTypeName (..),

    -- ** Tag
    Tag (..),
    mkTag,
    tfKey,
    tfValue,

    -- ** ProgressCounters
    ProgressCounters (..),
    mkProgressCounters,
    pcCancelledSteps,
    pcFailedSteps,
    pcSuccessSteps,
    pcTimedOutSteps,
    pcTotalSteps,

    -- ** AssociationExecutionTarget
    AssociationExecutionTarget (..),
    mkAssociationExecutionTarget,
    aetAssociationId,
    aetAssociationVersion,
    aetDetailedStatus,
    aetExecutionId,
    aetLastExecutionDate,
    aetOutputSource,
    aetResourceId,
    aetResourceType,
    aetStatus,

    -- ** InstanceAssociationOutputLocation
    InstanceAssociationOutputLocation (..),
    mkInstanceAssociationOutputLocation,
    iaolS3Location,

    -- ** PatchStringDateTime
    PatchStringDateTime (..),

    -- ** OpsItemDataValue
    OpsItemDataValue (..),
    mkOpsItemDataValue,
    oidvType,
    oidvValue,

    -- ** DocumentVersionName
    DocumentVersionName (..),

    -- ** InventoryFilter
    InventoryFilter (..),
    mkInventoryFilter,
    ifKey,
    ifValues,
    ifType,

    -- ** TargetValue
    TargetValue (..),

    -- ** OpsEntityItem
    OpsEntityItem (..),
    mkOpsEntityItem,
    oeiCaptureTime,
    oeiContent,

    -- ** AttachmentIdentifier
    AttachmentIdentifier (..),

    -- ** InstanceInformationFilterKey
    InstanceInformationFilterKey (..),

    -- ** AssociationStatusName
    AssociationStatusName (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** InventoryGroupName
    InventoryGroupName (..),

    -- ** DocumentARN
    DocumentARN (..),

    -- ** StandardErrorContent
    StandardErrorContent (..),

    -- ** ParametersFilter
    ParametersFilter (..),
    mkParametersFilter,
    pKey,
    pValues,

    -- ** MaintenanceWindowFilterValue
    MaintenanceWindowFilterValue (..),

    -- ** PatchMsrcNumber
    PatchMsrcNumber (..),

    -- ** FailureDetails
    FailureDetails (..),
    mkFailureDetails,
    fdDetails,
    fdFailureStage,
    fdFailureType,

    -- ** ClientToken
    ClientToken (..),

    -- ** ParameterValue
    ParameterValue (..),

    -- ** ComplianceResourceId
    ComplianceResourceId (..),

    -- ** OpsEntityId
    OpsEntityId (..),

    -- ** OutputSource
    OutputSource (..),
    mkOutputSource,
    osOutputSourceId,
    osOutputSourceType,

    -- ** S3OutputLocation
    S3OutputLocation (..),
    mkS3OutputLocation,
    solOutputS3BucketName,
    solOutputS3KeyPrefix,
    solOutputS3Region,

    -- ** SessionStatus
    SessionStatus (..),

    -- ** RebootOption
    RebootOption (..),

    -- ** PatchFilterValue
    PatchFilterValue (..),

    -- ** ComplianceItemId
    ComplianceItemId (..),

    -- ** PatchProduct
    PatchProduct (..),

    -- ** AttachmentInformation
    AttachmentInformation (..),
    mkAttachmentInformation,
    aiName,

    -- ** ResourceDataSyncOrganizationalUnit
    ResourceDataSyncOrganizationalUnit (..),
    mkResourceDataSyncOrganizationalUnit,
    rdsouOrganizationalUnitId,

    -- ** MaintenanceWindowStepFunctionsName
    MaintenanceWindowStepFunctionsName (..),

    -- ** DocumentHashType
    DocumentHashType (..),

    -- ** MaintenanceWindowExecutionStatusDetails
    MaintenanceWindowExecutionStatusDetails (..),

    -- ** OpsAggregator
    OpsAggregator (..),
    mkOpsAggregator,
    oaAggregatorType,
    oaAggregators,
    oaAttributeName,
    oaFilters,
    oaTypeName,
    oaValues,

    -- ** ResourceDataSyncS3Region
    ResourceDataSyncS3Region (..),

    -- ** MaintenanceWindowTargetId
    MaintenanceWindowTargetId (..),

    -- ** OwnerInformation
    OwnerInformation (..),

    -- ** AutomationExecution
    AutomationExecution (..),
    mkAutomationExecution,
    aeAutomationExecutionId,
    aeAutomationExecutionStatus,
    aeCurrentAction,
    aeCurrentStepName,
    aeDocumentName,
    aeDocumentVersion,
    aeExecutedBy,
    aeExecutionEndTime,
    aeExecutionStartTime,
    aeFailureMessage,
    aeMaxConcurrency,
    aeMaxErrors,
    aeMode,
    aeOutputs,
    aeParameters,
    aeParentAutomationExecutionId,
    aeProgressCounters,
    aeResolvedTargets,
    aeStepExecutions,
    aeStepExecutionsTruncated,
    aeTarget,
    aeTargetLocations,
    aeTargetMaps,
    aeTargetParameterName,
    aeTargets,

    -- ** CloudWatchOutputConfig
    CloudWatchOutputConfig (..),
    mkCloudWatchOutputConfig,
    cwocCloudWatchLogGroupName,
    cwocCloudWatchOutputEnabled,

    -- ** String
    String (..),

    -- ** OpsItemSummary
    OpsItemSummary (..),
    mkOpsItemSummary,
    oisCategory,
    oisCreatedBy,
    oisCreatedTime,
    oisLastModifiedBy,
    oisLastModifiedTime,
    oisOperationalData,
    oisOpsItemId,
    oisPriority,
    oisSeverity,
    oisSource,
    oisStatus,
    oisTitle,

    -- ** InstanceInformationFilter
    InstanceInformationFilter (..),
    mkInstanceInformationFilter,
    iifKey,
    iifValueSet,

    -- ** AutomationParameterValue
    AutomationParameterValue (..),

    -- ** PSParameterName
    PSParameterName (..),

    -- ** PatchComplianceLevel
    PatchComplianceLevel (..),

    -- ** UUID
    UUID (..),

    -- ** MaintenanceWindowRunCommandParameters
    MaintenanceWindowRunCommandParameters (..),
    mkMaintenanceWindowRunCommandParameters,
    mwrcpCloudWatchOutputConfig,
    mwrcpComment,
    mwrcpDocumentHash,
    mwrcpDocumentHashType,
    mwrcpDocumentVersion,
    mwrcpNotificationConfig,
    mwrcpOutputS3BucketName,
    mwrcpOutputS3KeyPrefix,
    mwrcpParameters,
    mwrcpServiceRoleArn,
    mwrcpTimeoutSeconds,

    -- ** DefaultInstanceName
    DefaultInstanceName (..),

    -- ** BaselineDescription
    BaselineDescription (..),

    -- ** OperatingSystem
    OperatingSystem (..),

    -- ** InventoryResultEntity
    InventoryResultEntity (..),
    mkInventoryResultEntity,
    ireData,
    ireId,

    -- ** PatchOrchestratorFilter
    PatchOrchestratorFilter (..),
    mkPatchOrchestratorFilter,
    pofKey,
    pofValues,

    -- ** S3Region
    S3Region (..),

    -- ** PatchClassification
    PatchClassification (..),

    -- ** StepExecution
    StepExecution (..),
    mkStepExecution,
    seAction,
    seExecutionEndTime,
    seExecutionStartTime,
    seFailureDetails,
    seFailureMessage,
    seInputs,
    seIsCritical,
    seIsEnd,
    seMaxAttempts,
    seNextStep,
    seOnFailure,
    seOutputs,
    seOverriddenParameters,
    seResponse,
    seResponseCode,
    seStepExecutionId,
    seStepName,
    seStepStatus,
    seTargetLocation,
    seTargets,
    seTimeoutSeconds,
    seValidNextSteps,

    -- ** PatchRule
    PatchRule (..),
    mkPatchRule,
    prPatchFilterGroup,
    prApproveAfterDays,
    prApproveUntilDate,
    prComplianceLevel,
    prEnableNonSecurity,

    -- ** InventoryItemTypeName
    InventoryItemTypeName (..),

    -- ** InstancePatchState
    InstancePatchState (..),
    mkInstancePatchState,
    ipsInstanceId,
    ipsPatchGroup,
    ipsBaselineId,
    ipsOperationStartTime,
    ipsOperationEndTime,
    ipsOperation,
    ipsFailedCount,
    ipsInstallOverrideList,
    ipsInstalledCount,
    ipsInstalledOtherCount,
    ipsInstalledPendingRebootCount,
    ipsInstalledRejectedCount,
    ipsLastNoRebootInstallOperationTime,
    ipsMissingCount,
    ipsNotApplicableCount,
    ipsOwnerInformation,
    ipsRebootOption,
    ipsSnapshotId,
    ipsUnreportedNotApplicableCount,

    -- ** MaintenanceWindowExecutionTaskInvocationIdentity
    MaintenanceWindowExecutionTaskInvocationIdentity (..),
    mkMaintenanceWindowExecutionTaskInvocationIdentity,
    mwetiiEndTime,
    mwetiiExecutionId,
    mwetiiInvocationId,
    mwetiiOwnerInformation,
    mwetiiParameters,
    mwetiiStartTime,
    mwetiiStatus,
    mwetiiStatusDetails,
    mwetiiTaskExecutionId,
    mwetiiTaskType,
    mwetiiWindowExecutionId,
    mwetiiWindowTargetId,

    -- ** PatchCVEIds
    PatchCVEIds (..),

    -- ** ResourceDataSyncSourceType
    ResourceDataSyncSourceType (..),

    -- ** DocumentName
    DocumentName (..),

    -- ** StopType
    StopType (..),

    -- ** InstanceInformationStringFilter
    InstanceInformationStringFilter (..),
    mkInstanceInformationStringFilter,
    iisfKey,
    iisfValues,

    -- ** PatchSourceProduct
    PatchSourceProduct (..),

    -- ** DocumentKeyValuesFilter
    DocumentKeyValuesFilter (..),
    mkDocumentKeyValuesFilter,
    dkvfKey,
    dkvfValues,

    -- ** DocumentFilterKey
    DocumentFilterKey (..),

    -- ** MaintenanceWindowStepFunctionsParameters
    MaintenanceWindowStepFunctionsParameters (..),
    mkMaintenanceWindowStepFunctionsParameters,
    mwsfpInput,
    mwsfpName,

    -- ** ResourceDataSyncSourceRegion
    ResourceDataSyncSourceRegion (..),

    -- ** ComplianceItemTitle
    ComplianceItemTitle (..),

    -- ** PatchAction
    PatchAction (..),

    -- ** ParameterInlinePolicy
    ParameterInlinePolicy (..),
    mkParameterInlinePolicy,
    pipPolicyStatus,
    pipPolicyText,
    pipPolicyType,

    -- ** AttachmentHash
    AttachmentHash (..),

    -- ** PatchOperationType
    PatchOperationType (..),

    -- ** ParameterKeyId
    ParameterKeyId (..),

    -- ** PatchOrchestratorFilterValue
    PatchOrchestratorFilterValue (..),

    -- ** AssociationExecutionFilter
    AssociationExecutionFilter (..),
    mkAssociationExecutionFilter,
    aefKey,
    aefValue,
    aefType,

    -- ** ParameterTier
    ParameterTier (..),

    -- ** OpsDataTypeName
    OpsDataTypeName (..),

    -- ** CommandStatus
    CommandStatus (..),

    -- ** PatchId
    PatchId (..),

    -- ** MaintenanceWindowExecution
    MaintenanceWindowExecution (..),
    mkMaintenanceWindowExecution,
    mweEndTime,
    mweStartTime,
    mweStatus,
    mweStatusDetails,
    mweWindowExecutionId,
    mweWindowId,

    -- ** MaintenanceWindowExecutionTaskId
    MaintenanceWindowExecutionTaskId (..),

    -- ** PatchSource
    PatchSource (..),
    mkPatchSource,
    psName,
    psProducts,
    psConfiguration,

    -- ** MaintenanceWindowResourceType
    MaintenanceWindowResourceType (..),

    -- ** Url
    Url (..),

    -- ** PatchBaselineIdentity
    PatchBaselineIdentity (..),
    mkPatchBaselineIdentity,
    pbiBaselineDescription,
    pbiBaselineId,
    pbiBaselineName,
    pbiDefaultBaseline,
    pbiOperatingSystem,

    -- ** CompliantSummary
    CompliantSummary (..),
    mkCompliantSummary,
    csCompliantCount,
    csSeveritySummary,

    -- ** ParameterHistory
    ParameterHistory (..),
    mkParameterHistory,
    phAllowedPattern,
    phDataType,
    phDescription,
    phKeyId,
    phLabels,
    phLastModifiedDate,
    phLastModifiedUser,
    phName,
    phPolicies,
    phTier,
    phType,
    phValue,
    phVersion,

    -- ** StatusAdditionalInfo
    StatusAdditionalInfo (..),

    -- ** OpsFilterKey
    OpsFilterKey (..),

    -- ** AssociationSyncCompliance
    AssociationSyncCompliance (..),

    -- ** DocumentKeyValuesFilterValue
    DocumentKeyValuesFilterValue (..),

    -- ** ServiceSetting
    ServiceSetting (..),
    mkServiceSetting,
    ssARN,
    ssLastModifiedDate,
    ssLastModifiedUser,
    ssSettingId,
    ssSettingValue,
    ssStatus,

    -- ** PatchCVEId
    PatchCVEId (..),

    -- ** PatchDeploymentStatus
    PatchDeploymentStatus (..),

    -- ** ResourceCountByStatus
    ResourceCountByStatus (..),

    -- ** MaintenanceWindowName
    MaintenanceWindowName (..),

    -- ** CommandPlugin
    CommandPlugin (..),
    mkCommandPlugin,
    cpName,
    cpOutput,
    cpOutputS3BucketName,
    cpOutputS3KeyPrefix,
    cpOutputS3Region,
    cpResponseCode,
    cpResponseFinishDateTime,
    cpResponseStartDateTime,
    cpStandardErrorUrl,
    cpStandardOutputUrl,
    cpStatus,
    cpStatusDetails,

    -- ** InvocationTraceOutput
    InvocationTraceOutput (..),

    -- ** MaintenanceWindowExecutionId
    MaintenanceWindowExecutionId (..),

    -- ** ResourceDataSyncS3Format
    ResourceDataSyncS3Format (..),

    -- ** ServiceSettingId
    ServiceSettingId (..),

    -- ** OpsItemDataKey
    OpsItemDataKey (..),

    -- ** Patch
    Patch (..),
    mkPatch,
    pAdvisoryIds,
    pArch,
    pBugzillaIds,
    pCVEIds,
    pClassification,
    pContentUrl,
    pDescription,
    pEpoch,
    pId,
    pKbNumber,
    pLanguage,
    pMsrcNumber,
    pMsrcSeverity,
    pName,
    pProduct,
    pProductFamily,
    pRelease,
    pReleaseDate,
    pRepository,
    pSeverity,
    pTitle,
    pVendor,
    pVersion,

    -- ** MaintenanceWindowTaskInvocationParameters
    MaintenanceWindowTaskInvocationParameters (..),
    mkMaintenanceWindowTaskInvocationParameters,
    mwtipAutomation,
    mwtipLambda,
    mwtipRunCommand,
    mwtipStepFunctions,

    -- ** SessionFilter
    SessionFilter (..),
    mkSessionFilter,
    sfKey,
    sfValue,

    -- ** SignalType
    SignalType (..),

    -- ** AttachmentName
    AttachmentName (..),

    -- ** MaintenanceWindowTaskParameterValue
    MaintenanceWindowTaskParameterValue (..),

    -- ** InventoryItemCaptureTime
    InventoryItemCaptureTime (..),

    -- ** InstanceInformationFilterValue
    InstanceInformationFilterValue (..),

    -- ** DocumentParameter
    DocumentParameter (..),
    mkDocumentParameter,
    dpDefaultValue,
    dpDescription,
    dpName,
    dpType,

    -- ** MaintenanceWindowFilterKey
    MaintenanceWindowFilterKey (..),

    -- ** ResourceComplianceSummaryItem
    ResourceComplianceSummaryItem (..),
    mkResourceComplianceSummaryItem,
    rcsiComplianceType,
    rcsiCompliantSummary,
    rcsiExecutionSummary,
    rcsiNonCompliantSummary,
    rcsiOverallSeverity,
    rcsiResourceId,
    rcsiResourceType,
    rcsiStatus,

    -- ** StatusDetails
    StatusDetails (..),

    -- ** TargetKey
    TargetKey (..),

    -- ** OpsAggregatorValue
    OpsAggregatorValue (..),

    -- ** InventoryFilterValue
    InventoryFilterValue (..),

    -- ** OpsItemFilter
    OpsItemFilter (..),
    mkOpsItemFilter,
    oifKey,
    oifValues,
    oifOperator,

    -- ** ResourceDataSyncDestinationDataSharingType
    ResourceDataSyncDestinationDataSharingType (..),

    -- ** ActivationId
    ActivationId (..),

    -- ** DocumentDescription
    DocumentDescription (..),
    mkDocumentDescription,
    ddAttachmentsInformation,
    ddCreatedDate,
    ddDefaultVersion,
    ddDescription,
    ddDocumentFormat,
    ddDocumentType,
    ddDocumentVersion,
    ddHash,
    ddHashType,
    ddLatestVersion,
    ddName,
    ddOwner,
    ddParameters,
    ddPlatformTypes,
    ddRequires,
    ddSchemaVersion,
    ddSha1,
    ddStatus,
    ddStatusInformation,
    ddTags,
    ddTargetType,
    ddVersionName,

    -- ** ParameterType
    ParameterType (..),

    -- ** AssociationExecutionTargetsFilter
    AssociationExecutionTargetsFilter (..),
    mkAssociationExecutionTargetsFilter,
    aetfKey,
    aetfValue,

    -- ** ComplianceStatus
    ComplianceStatus (..),

    -- ** ParameterDataType
    ParameterDataType (..),

    -- ** MaintenanceWindowTaskId
    MaintenanceWindowTaskId (..),

    -- ** ParametersFilterValue
    ParametersFilterValue (..),

    -- ** SessionManagerS3OutputUrl
    SessionManagerS3OutputUrl (..),

    -- ** SessionManagerCloudWatchOutputUrl
    SessionManagerCloudWatchOutputUrl (..),

    -- ** InventoryAggregatorExpression
    InventoryAggregatorExpression (..),

    -- ** SessionState
    SessionState (..),

    -- ** SessionFilterKey
    SessionFilterKey (..),

    -- ** AutomationExecutionStatus
    AutomationExecutionStatus (..),

    -- ** TargetType
    TargetType (..),

    -- ** OpsItemFilterOperator
    OpsItemFilterOperator (..),

    -- ** ActivationCode
    ActivationCode (..),

    -- ** PatchFilterGroup
    PatchFilterGroup (..),
    mkPatchFilterGroup,
    pfgPatchFilters,

    -- ** Account
    Account (..),

    -- ** OpsItemDataType
    OpsItemDataType (..),

    -- ** AssociationFilter
    AssociationFilter (..),
    mkAssociationFilter,
    afKey,
    afValue,

    -- ** ResourceDataSyncS3Destination
    ResourceDataSyncS3Destination (..),
    mkResourceDataSyncS3Destination,
    rdssdBucketName,
    rdssdSyncFormat,
    rdssdRegion,
    rdssdAWSKMSKeyARN,
    rdssdDestinationDataSharing,
    rdssdPrefix,

    -- ** OpsAggregatorValueKey
    OpsAggregatorValueKey (..),

    -- ** TargetLocation
    TargetLocation (..),
    mkTargetLocation,
    tlAccounts,
    tlExecutionRoleName,
    tlRegions,
    tlTargetLocationMaxConcurrency,
    tlTargetLocationMaxErrors,

    -- ** ResourceTypeForTagging
    ResourceTypeForTagging (..),

    -- ** AccountId
    AccountId (..),

    -- ** LastResourceDataSyncMessage
    LastResourceDataSyncMessage (..),

    -- ** PatchSet
    PatchSet (..),

    -- ** MaintenanceWindowExecutionTaskInvocationParameters
    MaintenanceWindowExecutionTaskInvocationParameters (..),

    -- ** Activation
    Activation (..),
    mkActivation,
    aActivationId,
    aCreatedDate,
    aDefaultInstanceName,
    aDescription,
    aExpirationDate,
    aExpired,
    aIamRole,
    aRegistrationLimit,
    aRegistrationsCount,
    aTags,

    -- ** InventoryItemContentHash
    InventoryItemContentHash (..),

    -- ** NextToken
    NextToken (..),

    -- ** InstanceAssociationOutputUrl
    InstanceAssociationOutputUrl (..),
    mkInstanceAssociationOutputUrl,
    iaouS3OutputUrl,

    -- ** InstanceTagName
    InstanceTagName (..),

    -- ** OpsFilter
    OpsFilter (..),
    mkOpsFilter,
    ofKey,
    ofValues,
    ofType,

    -- ** DocumentPermissionType
    DocumentPermissionType (..),

    -- ** SessionDetails
    SessionDetails (..),

    -- ** MaintenanceWindowAutomationParameters
    MaintenanceWindowAutomationParameters (..),
    mkMaintenanceWindowAutomationParameters,
    mwapDocumentVersion,
    mwapParameters,

    -- ** MaintenanceWindowTask
    MaintenanceWindowTask (..),
    mkMaintenanceWindowTask,
    mwtDescription,
    mwtLoggingInfo,
    mwtMaxConcurrency,
    mwtMaxErrors,
    mwtName,
    mwtPriority,
    mwtServiceRoleArn,
    mwtTargets,
    mwtTaskArn,
    mwtTaskParameters,
    mwtType,
    mwtWindowId,
    mwtWindowTaskId,

    -- ** S3OutputUrl
    S3OutputUrl (..),
    mkS3OutputUrl,
    souOutputUrl,

    -- ** OpsItemNotification
    OpsItemNotification (..),
    mkOpsItemNotification,
    oinArn,

    -- ** MaintenanceWindowId
    MaintenanceWindowId (..),

    -- ** DocumentIdentifier
    DocumentIdentifier (..),
    mkDocumentIdentifier,
    diDocumentFormat,
    diDocumentType,
    diDocumentVersion,
    diName,
    diOwner,
    diPlatformTypes,
    diRequires,
    diSchemaVersion,
    diTags,
    diTargetType,
    diVersionName,

    -- ** DocumentFormat
    DocumentFormat (..),

    -- ** InventoryItemSchemaVersion
    InventoryItemSchemaVersion (..),

    -- ** CommandInvocation
    CommandInvocation (..),
    mkCommandInvocation,
    ciCloudWatchOutputConfig,
    ciCommandId,
    ciCommandPlugins,
    ciComment,
    ciDocumentName,
    ciDocumentVersion,
    ciInstanceId,
    ciInstanceName,
    ciNotificationConfig,
    ciRequestedDateTime,
    ciServiceRole,
    ciStandardErrorUrl,
    ciStandardOutputUrl,
    ciStatus,
    ciStatusDetails,
    ciTraceOutput,

    -- ** MaxErrors
    MaxErrors (..),

    -- ** ComplianceQueryOperatorType
    ComplianceQueryOperatorType (..),

    -- ** SeveritySummary
    SeveritySummary (..),
    mkSeveritySummary,
    ssCriticalCount,
    ssHighCount,
    ssInformationalCount,
    ssLowCount,
    ssMediumCount,
    ssUnspecifiedCount,

    -- ** InventoryTypeDisplayName
    InventoryTypeDisplayName (..),

    -- ** MaintenanceWindowDescription
    MaintenanceWindowDescription (..),

    -- ** CommandFilter
    CommandFilter (..),
    mkCommandFilter,
    cfKey,
    cfValue,

    -- ** SessionTarget
    SessionTarget (..),

    -- ** DocumentStatusInformation
    DocumentStatusInformation (..),

    -- ** OpsFilterValue
    OpsFilterValue (..),

    -- ** InstallOverrideList
    InstallOverrideList (..),

    -- ** AttachmentsSource
    AttachmentsSource (..),
    mkAttachmentsSource,
    asfKey,
    asfName,
    asfValues,

    -- ** PSParameterSelector
    PSParameterSelector (..),

    -- ** OpsEntity
    OpsEntity (..),
    mkOpsEntity,
    oeData,
    oeId,

    -- ** AutomationType
    AutomationType (..),

    -- ** StatusMessage
    StatusMessage (..),

    -- ** ScheduleExpression
    ScheduleExpression (..),

    -- ** AssociationComplianceSeverity
    AssociationComplianceSeverity (..),

    -- ** ComplianceExecutionType
    ComplianceExecutionType (..),

    -- ** OpsDataAttributeName
    OpsDataAttributeName (..),

    -- ** PlatformType
    PlatformType (..),

    -- ** BatchErrorMessage
    BatchErrorMessage (..),

    -- ** OutputSourceId
    OutputSourceId (..),

    -- ** MaintenanceWindowTimezone
    MaintenanceWindowTimezone (..),

    -- ** AttachmentUrl
    AttachmentUrl (..),

    -- ** InstanceAssociationStatusInfo
    InstanceAssociationStatusInfo (..),
    mkInstanceAssociationStatusInfo,
    iasiAssociationId,
    iasiAssociationName,
    iasiAssociationVersion,
    iasiDetailedStatus,
    iasiDocumentVersion,
    iasiErrorCode,
    iasiExecutionDate,
    iasiExecutionSummary,
    iasiInstanceId,
    iasiName,
    iasiOutputUrl,
    iasiStatus,

    -- ** ComplianceExecutionId
    ComplianceExecutionId (..),

    -- ** TargetMapKey
    TargetMapKey (..),

    -- ** InventoryDeletionStatus
    InventoryDeletionStatus (..),

    -- ** OutputSourceType
    OutputSourceType (..),

    -- ** Version
    Version (..),

    -- ** CommandId
    CommandId (..),

    -- ** ConnectionStatus
    ConnectionStatus (..),

    -- ** AssociationOverview
    AssociationOverview (..),
    mkAssociationOverview,
    aoAssociationStatusAggregatedCount,
    aoDetailedStatus,
    aoStatus,

    -- ** InventoryDeletionSummary
    InventoryDeletionSummary (..),
    mkInventoryDeletionSummary,
    idsRemainingCount,
    idsSummaryItems,
    idsTotalCount,

    -- ** MaintenanceWindowTaskParameterName
    MaintenanceWindowTaskParameterName (..),

    -- ** ActivationDescription
    ActivationDescription (..),

    -- ** PatchStatus
    PatchStatus (..),
    mkPatchStatus,
    psApprovalDate,
    psComplianceLevel,
    psDeploymentStatus,

    -- ** Fault
    Fault (..),

    -- ** OpsItemSeverity
    OpsItemSeverity (..),

    -- ** CommandPluginStatus
    CommandPluginStatus (..),

    -- ** AutomationExecutionFilterKey
    AutomationExecutionFilterKey (..),

    -- ** ResourceDataSyncState
    ResourceDataSyncState (..),

    -- ** OpsItemFilterValue
    OpsItemFilterValue (..),

    -- ** DescribeActivationsFilter
    DescribeActivationsFilter (..),
    mkDescribeActivationsFilter,
    dafFilterKey,
    dafFilterValues,

    -- ** ResourceDataSyncName
    ResourceDataSyncName (..),

    -- ** ResolvedTargets
    ResolvedTargets (..),
    mkResolvedTargets,
    rtParameterValues,
    rtTruncated,

    -- ** OpsItemStatus
    OpsItemStatus (..),

    -- ** DocumentHash
    DocumentHash (..),

    -- ** DocumentVersion
    DocumentVersion (..),

    -- ** AssociationVersionInfo
    AssociationVersionInfo (..),
    mkAssociationVersionInfo,
    aviApplyOnlyAtCronInterval,
    aviAssociationId,
    aviAssociationName,
    aviAssociationVersion,
    aviComplianceSeverity,
    aviCreatedDate,
    aviDocumentVersion,
    aviMaxConcurrency,
    aviMaxErrors,
    aviName,
    aviOutputLocation,
    aviParameters,
    aviScheduleExpression,
    aviSyncCompliance,
    aviTargets,

    -- ** InstanceInformation
    InstanceInformation (..),
    mkInstanceInformation,
    iiActivationId,
    iiAgentVersion,
    iiAssociationOverview,
    iiAssociationStatus,
    iiComputerName,
    iiIPAddress,
    iiIamRole,
    iiInstanceId,
    iiIsLatestVersion,
    iiLastAssociationExecutionDate,
    iiLastPingDateTime,
    iiLastSuccessfulAssociationExecutionDate,
    iiName,
    iiPingStatus,
    iiPlatformName,
    iiPlatformType,
    iiPlatformVersion,
    iiRegistrationDate,
    iiResourceType,

    -- ** MaintenanceWindowStringDateTime
    MaintenanceWindowStringDateTime (..),

    -- ** AssociationStatus
    AssociationStatus (..),
    mkAssociationStatus,
    asDate,
    asName,
    asMessage,
    asAdditionalInfo,

    -- ** CalendarNameOrARN
    CalendarNameOrARN (..),

    -- ** ResourceDataSyncSourceWithState
    ResourceDataSyncSourceWithState (..),
    mkResourceDataSyncSourceWithState,
    rdsswsAwsOrganizationsSource,
    rdsswsIncludeFutureRegions,
    rdsswsSourceRegions,
    rdsswsSourceType,
    rdsswsState,

    -- ** ResultAttribute
    ResultAttribute (..),
    mkResultAttribute,
    raTypeName,

    -- ** PatchSeverity
    PatchSeverity (..),

    -- ** CalendarState
    CalendarState (..),

    -- ** ResourceDataSyncAwsOrganizationsSource
    ResourceDataSyncAwsOrganizationsSource (..),
    mkResourceDataSyncAwsOrganizationsSource,
    rdsaosOrganizationSourceType,
    rdsaosOrganizationalUnits,

    -- ** OpsEntityItemKey
    OpsEntityItemKey (..),

    -- ** AutomationExecutionFilter
    AutomationExecutionFilter (..),
    mkAutomationExecutionFilter,
    aKey,
    aValues,

    -- ** LoggingInfo
    LoggingInfo (..),
    mkLoggingInfo,
    liS3BucketName,
    liS3Region,
    liS3KeyPrefix,

    -- ** InventoryGroup
    InventoryGroup (..),
    mkInventoryGroup,
    igName,
    igFilters,

    -- ** MaintenanceWindowIdentityForTarget
    MaintenanceWindowIdentityForTarget (..),
    mkMaintenanceWindowIdentityForTarget,
    mwiftName,
    mwiftWindowId,

    -- ** ResourceDataSyncType
    ResourceDataSyncType (..),

    -- ** ParametersFilterKey
    ParametersFilterKey (..),

    -- ** StandardOutputContent
    StandardOutputContent (..),

    -- ** NotificationType
    NotificationType (..),

    -- ** TagKey
    TagKey (..),

    -- ** CommandFilterKey
    CommandFilterKey (..),

    -- ** ResourceDataSyncSource
    ResourceDataSyncSource (..),
    mkResourceDataSyncSource,
    rdssSourceType,
    rdssSourceRegions,
    rdssAwsOrganizationsSource,
    rdssIncludeFutureRegions,

    -- ** Region
    Region (..),

    -- ** AttachmentsSourceKey
    AttachmentsSourceKey (..),

    -- ** NotificationArn
    NotificationArn (..),

    -- ** AllowedPattern
    AllowedPattern (..),

    -- ** MaintenanceWindowExecutionTaskIdentity
    MaintenanceWindowExecutionTaskIdentity (..),
    mkMaintenanceWindowExecutionTaskIdentity,
    mwetiEndTime,
    mwetiStartTime,
    mwetiStatus,
    mwetiStatusDetails,
    mwetiTaskArn,
    mwetiTaskExecutionId,
    mwetiTaskType,
    mwetiWindowExecutionId,

    -- ** ComplianceStringFilter
    ComplianceStringFilter (..),
    mkComplianceStringFilter,
    csfKey,
    csfType,
    csfValues,

    -- ** AutomationExecutionMetadata
    AutomationExecutionMetadata (..),
    mkAutomationExecutionMetadata,
    aemAutomationExecutionId,
    aemAutomationExecutionStatus,
    aemAutomationType,
    aemCurrentAction,
    aemCurrentStepName,
    aemDocumentName,
    aemDocumentVersion,
    aemExecutedBy,
    aemExecutionEndTime,
    aemExecutionStartTime,
    aemFailureMessage,
    aemLogFile,
    aemMaxConcurrency,
    aemMaxErrors,
    aemMode,
    aemOutputs,
    aemParentAutomationExecutionId,
    aemResolvedTargets,
    aemTarget,
    aemTargetMaps,
    aemTargetParameterName,
    aemTargets,

    -- ** InstancePatchStateOperatorType
    InstancePatchStateOperatorType (..),

    -- ** MaintenanceWindowTarget
    MaintenanceWindowTarget (..),
    mkMaintenanceWindowTarget,
    mDescription,
    mName,
    mOwnerInformation,
    mResourceType,
    mTargets,
    mWindowId,
    mWindowTargetId,

    -- ** InstancePatchStateFilter
    InstancePatchStateFilter (..),
    mkInstancePatchStateFilter,
    ipsfKey,
    ipsfValues,
    ipsfType,

    -- ** StepExecutionFilter
    StepExecutionFilter (..),
    mkStepExecutionFilter,
    sefKey,
    sefValues,

    -- ** ComputerName
    ComputerName (..),

    -- ** InventoryQueryOperatorType
    InventoryQueryOperatorType (..),

    -- ** TokenValue
    TokenValue (..),

    -- ** AutomationExecutionId
    AutomationExecutionId (..),

    -- ** DocumentDefaultVersionDescription
    DocumentDefaultVersionDescription (..),
    mkDocumentDefaultVersionDescription,
    ddvdDefaultVersion,
    ddvdDefaultVersionName,
    ddvdName,

    -- ** AttachmentContent
    AttachmentContent (..),
    mkAttachmentContent,
    acHash,
    acHashType,
    acName,
    acSize,
    acUrl,

    -- ** MaintenanceWindowTaskTargetId
    MaintenanceWindowTaskTargetId (..),

    -- ** ComplianceItem
    ComplianceItem (..),
    mkComplianceItem,
    cifComplianceType,
    cifDetails,
    cifExecutionSummary,
    cifId,
    cifResourceId,
    cifResourceType,
    cifSeverity,
    cifStatus,
    cifTitle,

    -- ** ResourceDataSyncItem
    ResourceDataSyncItem (..),
    mkResourceDataSyncItem,
    rdsiLastStatus,
    rdsiLastSuccessfulSyncTime,
    rdsiLastSyncStatusMessage,
    rdsiLastSyncTime,
    rdsiS3Destination,
    rdsiSyncCreatedTime,
    rdsiSyncLastModifiedTime,
    rdsiSyncName,
    rdsiSyncSource,
    rdsiSyncType,

    -- ** InstanceAssociation
    InstanceAssociation (..),
    mkInstanceAssociation,
    iaAssociationId,
    iaAssociationVersion,
    iaContent,
    iaInstanceId,

    -- ** PatchGroupPatchBaselineMapping
    PatchGroupPatchBaselineMapping (..),
    mkPatchGroupPatchBaselineMapping,
    pgpbmBaselineIdentity,
    pgpbmPatchGroup,

    -- ** AutomationTargetParameterName
    AutomationTargetParameterName (..),

    -- ** InventoryItemAttribute
    InventoryItemAttribute (..),
    mkInventoryItemAttribute,
    iiaName,
    iiaDataType,

    -- ** ComplianceItemContentHash
    ComplianceItemContentHash (..),

    -- ** AssociationVersion
    AssociationVersion (..),

    -- ** StepExecutionFilterValue
    StepExecutionFilterValue (..),

    -- ** AssociationResourceType
    AssociationResourceType (..),

    -- ** AttachmentHashType
    AttachmentHashType (..),

    -- ** MaintenanceWindowExecutionTaskInvocationId
    MaintenanceWindowExecutionTaskInvocationId (..),

    -- ** ComplianceExecutionSummary
    ComplianceExecutionSummary (..),
    mkComplianceExecutionSummary,
    cesExecutionTime,
    cesExecutionId,
    cesExecutionType,

    -- ** InstancePatchStateFilterValue
    InstancePatchStateFilterValue (..),

    -- ** CommandInvocationStatus
    CommandInvocationStatus (..),

    -- ** ValidNextStep
    ValidNextStep (..),

    -- ** LastResourceDataSyncStatus
    LastResourceDataSyncStatus (..),

    -- ** DocumentStatus
    DocumentStatus (..),

    -- ** Product
    Product (..),

    -- ** SessionManagerParameterName
    SessionManagerParameterName (..),

    -- ** DocumentVersionInfo
    DocumentVersionInfo (..),
    mkDocumentVersionInfo,
    dviCreatedDate,
    dviDocumentFormat,
    dviDocumentVersion,
    dviIsDefaultVersion,
    dviName,
    dviStatus,
    dviStatusInformation,
    dviVersionName,

    -- ** AttributeName
    AttributeName (..),

    -- ** SnapshotDownloadUrl
    SnapshotDownloadUrl (..),

    -- ** StatusName
    StatusName (..),

    -- ** PatchComplianceData
    PatchComplianceData (..),
    mkPatchComplianceData,
    pcdTitle,
    pcdKBId,
    pcdClassification,
    pcdSeverity,
    pcdState,
    pcdInstalledTime,
    pcdCVEIds,

    -- ** AssociationExecution
    AssociationExecution (..),
    mkAssociationExecution,
    aeAssociationId,
    aeAssociationVersion,
    aeCreatedTime,
    aeDetailedStatus,
    aeExecutionId,
    aeLastExecutionDate,
    aeResourceCountByStatus,
    aeStatus,

    -- ** InventoryAggregator
    InventoryAggregator (..),
    mkInventoryAggregator,
    iaAggregators,
    iaExpression,
    iaGroups,

    -- ** SessionId
    SessionId (..),

    -- ** EffectivePatch
    EffectivePatch (..),
    mkEffectivePatch,
    epPatch,
    epPatchStatus,

    -- ** Comment
    Comment (..),

    -- ** TargetMapValue
    TargetMapValue (..),

    -- ** AssociationExecutionTargetsFilterKey
    AssociationExecutionTargetsFilterKey (..),

    -- ** ParameterName
    ParameterName (..),

    -- ** InventoryItemSchema
    InventoryItemSchema (..),
    mkInventoryItemSchema,
    iisTypeName,
    iisAttributes,
    iisDisplayName,
    iisVersion,

    -- ** AssociationName
    AssociationName (..),

    -- ** RelatedOpsItem
    RelatedOpsItem (..),
    mkRelatedOpsItem,
    roiOpsItemId,

    -- ** DocumentParameterType
    DocumentParameterType (..),

    -- ** InstanceAggregatedAssociationOverview
    InstanceAggregatedAssociationOverview (..),
    mkInstanceAggregatedAssociationOverview,
    iaaoDetailedStatus,
    iaaoInstanceAssociationStatusAggregatedCount,

    -- ** BaselineId
    BaselineId (..),

    -- ** OpsItemFilterKey
    OpsItemFilterKey (..),

    -- ** DocumentVersionNumber
    DocumentVersionNumber (..),

    -- ** ComplianceUploadType
    ComplianceUploadType (..),

    -- ** InventoryResultItem
    InventoryResultItem (..),
    mkInventoryResultItem,
    iriTypeName,
    iriSchemaVersion,
    iriContent,
    iriCaptureTime,
    iriContentHash,

    -- ** AssociationFilterKey
    AssociationFilterKey (..),

    -- ** Session
    Session (..),
    mkSession,
    sDetails,
    sDocumentName,
    sEndDate,
    sOutputUrl,
    sOwner,
    sSessionId,
    sStartDate,
    sStatus,
    sTarget,

    -- ** AutomationExecutionFilterValue
    AutomationExecutionFilterValue (..),

    -- ** ComplianceSeverity
    ComplianceSeverity (..),

    -- ** ComplianceFilterValue
    ComplianceFilterValue (..),

    -- ** PatchFilter
    PatchFilter (..),
    mkPatchFilter,
    pfKey,
    pfValues,

    -- ** InventoryAttributeDataType
    InventoryAttributeDataType (..),

    -- ** DescribeActivationsFilterKeys
    DescribeActivationsFilterKeys (..),

    -- ** FailedCreateAssociation
    FailedCreateAssociation (..),
    mkFailedCreateAssociation,
    fcaEntry,
    fcaFault,
    fcaMessage,

    -- ** PatchAdvisoryId
    PatchAdvisoryId (..),

    -- ** MaintenanceWindowExecutionTaskExecutionId
    MaintenanceWindowExecutionTaskExecutionId (..),

    -- ** ResourceDataSyncDestinationDataSharing
    ResourceDataSyncDestinationDataSharing (..),
    mkResourceDataSyncDestinationDataSharing,
    rdsddsDestinationDataSharingType,

    -- ** PSParameterValue
    PSParameterValue (..),

    -- ** S3BucketName
    S3BucketName (..),

    -- ** PatchGroup
    PatchGroup (..),

    -- ** PatchProperty
    PatchProperty (..),

    -- ** Parameter
    Parameter (..),
    mkParameter,
    pfARN,
    pfDataType,
    pfLastModifiedDate,
    pfName,
    pfSelector,
    pfSourceResult,
    pfType,
    pfValue,
    pfVersion,

    -- ** Association
    Association (..),
    mkAssociation,
    aAssociationId,
    aAssociationName,
    aAssociationVersion,
    aDocumentVersion,
    aInstanceId,
    aLastExecutionDate,
    aName,
    aOverview,
    aScheduleExpression,
    aTargets,

    -- ** OpsItemCategory
    OpsItemCategory (..),

    -- ** MaxConcurrency
    MaxConcurrency (..),

    -- ** InventoryResultItemKey
    InventoryResultItemKey (..),

    -- ** DocumentRequires
    DocumentRequires (..),
    mkDocumentRequires,
    drName,
    drVersion,

    -- ** OpsItem
    OpsItem (..),
    mkOpsItem,
    oiCategory,
    oiCreatedBy,
    oiCreatedTime,
    oiDescription,
    oiLastModifiedBy,
    oiLastModifiedTime,
    oiNotifications,
    oiOperationalData,
    oiOpsItemId,
    oiPriority,
    oiRelatedOpsItems,
    oiSeverity,
    oiSource,
    oiStatus,
    oiTitle,
    oiVersion,

    -- ** PatchFilterKey
    PatchFilterKey (..),

    -- ** AssociationFilterOperatorType
    AssociationFilterOperatorType (..),

    -- ** SnapshotId
    SnapshotId (..),

    -- ** NotificationEvent
    NotificationEvent (..),

    -- ** InventorySchemaDeleteOption
    InventorySchemaDeleteOption (..),

    -- ** ExecutionRoleName
    ExecutionRoleName (..),

    -- ** Target
    Target (..),
    mkTarget,
    tKey,
    tValues,

    -- ** PatchBugzillaId
    PatchBugzillaId (..),

    -- ** OpsResultAttribute
    OpsResultAttribute (..),
    mkOpsResultAttribute,
    oraTypeName,

    -- ** MaintenanceWindowFilter
    MaintenanceWindowFilter (..),
    mkMaintenanceWindowFilter,
    mwfKey,
    mwfValues,

    -- ** ExecutionMode
    ExecutionMode (..),

    -- ** IamRole
    IamRole (..),

    -- ** SessionManagerOutputUrl
    SessionManagerOutputUrl (..),
    mkSessionManagerOutputUrl,
    smouCloudWatchOutputUrl,
    smouS3OutputUrl,

    -- ** ServiceRole
    ServiceRole (..),

    -- ** AutomationParameterKey
    AutomationParameterKey (..),

    -- ** WindowId
    WindowId (..),

    -- ** Message
    Message (..),

    -- ** ClientContext
    ClientContext (..),

    -- ** Qualifier
    Qualifier (..),

    -- ** WindowExecutionId
    WindowExecutionId (..),

    -- ** Name
    Name (..),

    -- ** ComplianceType
    ComplianceType (..),

    -- ** Description
    Description (..),

    -- ** Source
    Source (..),

    -- ** Title
    Title (..),

    -- ** Category
    Category (..),

    -- ** Severity
    Severity (..),

    -- ** Id
    Id (..),

    -- ** DataType
    DataType (..),

    -- ** KeyId
    KeyId (..),

    -- ** LastModifiedUser
    LastModifiedUser (..),

    -- ** TypeName
    TypeName (..),

    -- ** Key
    Key (..),

    -- ** Option
    Option (..),

    -- ** TargetParameterName
    TargetParameterName (..),

    -- ** SyncName
    SyncName (..),

    -- ** OutputS3BucketName
    OutputS3BucketName (..),

    -- ** OutputS3KeyPrefix
    OutputS3KeyPrefix (..),

    -- ** OutputS3Region
    OutputS3Region (..),

    -- ** EndDate
    EndDate (..),

    -- ** NextExecutionTime
    NextExecutionTime (..),

    -- ** ScheduleTimezone
    ScheduleTimezone (..),

    -- ** StartDate
    StartDate (..),

    -- ** Value
    Value (..),

    -- ** ExecutionTime
    ExecutionTime (..),

    -- ** WindowTargetId
    WindowTargetId (..),

    -- ** SettingId
    SettingId (..),

    -- ** SchemaVersion
    SchemaVersion (..),

    -- ** CaptureTime
    CaptureTime (..),

    -- ** ContentHash
    ContentHash (..),

    -- ** DeletionId
    DeletionId (..),

    -- ** LastStatusMessage
    LastStatusMessage (..),

    -- ** Content
    Content (..),

    -- ** VersionName
    VersionName (..),

    -- ** AtTime
    AtTime (..),

    -- ** DetailedStatus
    DetailedStatus (..),

    -- ** ExecutionId
    ExecutionId (..),

    -- ** Status
    Status (..),

    -- ** FailureStage
    FailureStage (..),

    -- ** FailureType
    FailureType (..),

    -- ** OrganizationalUnitId
    OrganizationalUnitId (..),

    -- ** AggregatorType
    AggregatorType (..),

    -- ** CurrentAction
    CurrentAction (..),

    -- ** CurrentStepName
    CurrentStepName (..),

    -- ** ExecutedBy
    ExecutedBy (..),

    -- ** FailureMessage
    FailureMessage (..),

    -- ** ParentAutomationExecutionId
    ParentAutomationExecutionId (..),

    -- ** ServiceRoleArn
    ServiceRoleArn (..),

    -- ** SyncType
    SyncType (..),

    -- ** Action
    Action (..),

    -- ** StatusInformation
    StatusInformation (..),

    -- ** InvocationId
    InvocationId (..),

    -- ** TaskExecutionId
    TaskExecutionId (..),

    -- ** ExecutionElapsedTime
    ExecutionElapsedTime (..),

    -- ** ExecutionEndDateTime
    ExecutionEndDateTime (..),

    -- ** ExecutionStartDateTime
    ExecutionStartDateTime (..),

    -- ** PluginName
    PluginName (..),

    -- ** StandardErrorUrl
    StandardErrorUrl (..),

    -- ** StandardOutputUrl
    StandardOutputUrl (..),

    -- ** NextTransitionTime
    NextTransitionTime (..),

    -- ** TaskArn
    TaskArn (..),

    -- ** WindowTaskId
    WindowTaskId (..),

    -- ** SettingValue
    SettingValue (..),

    -- ** Configuration
    Configuration (..),

    -- ** Output
    Output (..),

    -- ** Arch
    Arch (..),

    -- ** MsrcSeverity
    MsrcSeverity (..),

    -- ** ProductFamily
    ProductFamily (..),

    -- ** Release
    Release (..),

    -- ** Repository
    Repository (..),

    -- ** Vendor
    Vendor (..),

    -- ** DefaultValue
    DefaultValue (..),

    -- ** Policies
    Policies (..),

    -- ** DefaultVersion
    DefaultVersion (..),

    -- ** Hash
    Hash (..),

    -- ** LatestVersion
    LatestVersion (..),

    -- ** Owner
    Owner (..),

    -- ** Sha1
    Sha1 (..),

    -- ** BucketName
    BucketName (..),

    -- ** AWSKMSKeyARN
    AWSKMSKeyARN (..),

    -- ** Prefix
    Prefix (..),

    -- ** TargetLocationMaxConcurrency
    TargetLocationMaxConcurrency (..),

    -- ** TargetLocationMaxErrors
    TargetLocationMaxErrors (..),

    -- ** ExecutionSummary
    ExecutionSummary (..),

    -- ** OrganizationSourceType
    OrganizationSourceType (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.AddTagsToResource
import Network.AWS.SSM.CancelCommand
import Network.AWS.SSM.CancelMaintenanceWindowExecution
import Network.AWS.SSM.CreateActivation
import Network.AWS.SSM.CreateAssociation
import Network.AWS.SSM.CreateAssociationBatch
import Network.AWS.SSM.CreateDocument
import Network.AWS.SSM.CreateMaintenanceWindow
import Network.AWS.SSM.CreateOpsItem
import Network.AWS.SSM.CreatePatchBaseline
import Network.AWS.SSM.CreateResourceDataSync
import Network.AWS.SSM.DeleteActivation
import Network.AWS.SSM.DeleteAssociation
import Network.AWS.SSM.DeleteDocument
import Network.AWS.SSM.DeleteInventory
import Network.AWS.SSM.DeleteMaintenanceWindow
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
import Network.AWS.SSM.ListDocumentVersions
import Network.AWS.SSM.ListDocuments
import Network.AWS.SSM.ListInventoryEntries
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
import Network.AWS.SSM.StartSession
import Network.AWS.SSM.StopAutomationExecution
import Network.AWS.SSM.TerminateSession
import Network.AWS.SSM.Types
import Network.AWS.SSM.UpdateAssociation
import Network.AWS.SSM.UpdateAssociationStatus
import Network.AWS.SSM.UpdateDocument
import Network.AWS.SSM.UpdateDocumentDefaultVersion
import Network.AWS.SSM.UpdateMaintenanceWindow
import Network.AWS.SSM.UpdateMaintenanceWindowTarget
import Network.AWS.SSM.UpdateMaintenanceWindowTask
import Network.AWS.SSM.UpdateManagedInstanceRole
import Network.AWS.SSM.UpdateOpsItem
import Network.AWS.SSM.UpdatePatchBaseline
import Network.AWS.SSM.UpdateResourceDataSync
import Network.AWS.SSM.UpdateServiceSetting
import Network.AWS.SSM.Waiters

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
