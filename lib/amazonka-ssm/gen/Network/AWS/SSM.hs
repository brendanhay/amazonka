{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
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
--
-- This reference is intended to be used with the <https://docs.aws.amazon.com/systems-manager/latest/userguide/ AWS Systems Manager User Guide> .
--
-- To get started, verify prerequisites and configure managed instances. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-setting-up.html Setting up AWS Systems Manager> in the /AWS Systems Manager User Guide/ .
--
-- For information about other API actions you can perform on EC2 instances, see the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ Amazon EC2 API Reference> . For information about how to use a Query API, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/making-api-requests.html Making API requests> .
module Network.AWS.SSM
  ( -- * Service Configuration
    ssm,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- ** CommandExecuted
    commandExecuted,

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

    -- ** DocumentParameterType
    DocumentParameterType (..),

    -- ** DocumentPermissionType
    DocumentPermissionType (..),

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

    -- ** OpsItemFilterKey
    OpsItemFilterKey (..),

    -- ** OpsItemFilterOperator
    OpsItemFilterOperator (..),

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
    AccountSharingInfo,
    accountSharingInfo,
    asiSharedDocumentVersion,
    asiAccountId,

    -- ** Activation
    Activation,
    activation,
    aExpired,
    aDefaultInstanceName,
    aActivationId,
    aCreatedDate,
    aRegistrationLimit,
    aExpirationDate,
    aDescription,
    aTags,
    aRegistrationsCount,
    aIAMRole,

    -- ** Association
    Association,
    association,
    assAssociationId,
    assInstanceId,
    assOverview,
    assLastExecutionDate,
    assScheduleExpression,
    assName,
    assTargets,
    assDocumentVersion,
    assAssociationVersion,
    assAssociationName,

    -- ** AssociationDescription
    AssociationDescription,
    associationDescription,
    adAssociationId,
    adInstanceId,
    adStatus,
    adApplyOnlyAtCronInterval,
    adLastSuccessfulExecutionDate,
    adOverview,
    adLastUpdateAssociationDate,
    adDate,
    adLastExecutionDate,
    adMaxErrors,
    adScheduleExpression,
    adName,
    adOutputLocation,
    adSyncCompliance,
    adTargets,
    adParameters,
    adDocumentVersion,
    adAutomationTargetParameterName,
    adAssociationVersion,
    adAssociationName,
    adComplianceSeverity,
    adMaxConcurrency,

    -- ** AssociationExecution
    AssociationExecution,
    associationExecution,
    aeAssociationId,
    aeDetailedStatus,
    aeStatus,
    aeExecutionId,
    aeCreatedTime,
    aeResourceCountByStatus,
    aeLastExecutionDate,
    aeAssociationVersion,

    -- ** AssociationExecutionFilter
    AssociationExecutionFilter,
    associationExecutionFilter,
    aefKey,
    aefValue,
    aefType,

    -- ** AssociationExecutionTarget
    AssociationExecutionTarget,
    associationExecutionTarget,
    aetAssociationId,
    aetDetailedStatus,
    aetStatus,
    aetExecutionId,
    aetResourceId,
    aetResourceType,
    aetOutputSource,
    aetLastExecutionDate,
    aetAssociationVersion,

    -- ** AssociationExecutionTargetsFilter
    AssociationExecutionTargetsFilter,
    associationExecutionTargetsFilter,
    aetfKey,
    aetfValue,

    -- ** AssociationFilter
    AssociationFilter,
    associationFilter,
    afKey,
    afValue,

    -- ** AssociationOverview
    AssociationOverview,
    associationOverview,
    aoDetailedStatus,
    aoStatus,
    aoAssociationStatusAggregatedCount,

    -- ** AssociationStatus
    AssociationStatus,
    associationStatus,
    asAdditionalInfo,
    asDate,
    asName,
    asMessage,

    -- ** AssociationVersionInfo
    AssociationVersionInfo,
    associationVersionInfo,
    aviAssociationId,
    aviApplyOnlyAtCronInterval,
    aviCreatedDate,
    aviMaxErrors,
    aviScheduleExpression,
    aviName,
    aviOutputLocation,
    aviSyncCompliance,
    aviTargets,
    aviParameters,
    aviDocumentVersion,
    aviAssociationVersion,
    aviAssociationName,
    aviComplianceSeverity,
    aviMaxConcurrency,

    -- ** AttachmentContent
    AttachmentContent,
    attachmentContent,
    acHash,
    acSize,
    acURL,
    acName,
    acHashType,

    -- ** AttachmentInformation
    AttachmentInformation,
    attachmentInformation,
    aiName,

    -- ** AttachmentsSource
    AttachmentsSource,
    attachmentsSource,
    aValues,
    aKey,
    aName,

    -- ** AutomationExecution
    AutomationExecution,
    automationExecution,
    aeCurrentStepName,
    aeTargetParameterName,
    aeTargetLocations,
    aeProgressCounters,
    aeExecutedBy,
    aeDocumentName,
    aeExecutionEndTime,
    aeFailureMessage,
    aeMode,
    aeTargetMaps,
    aeStepExecutionsTruncated,
    aeAutomationExecutionStatus,
    aeParentAutomationExecutionId,
    aeOutputs,
    aeMaxErrors,
    aeExecutionStartTime,
    aeCurrentAction,
    aeTargets,
    aeResolvedTargets,
    aeParameters,
    aeDocumentVersion,
    aeAutomationExecutionId,
    aeStepExecutions,
    aeMaxConcurrency,
    aeTarget,

    -- ** AutomationExecutionFilter
    AutomationExecutionFilter,
    automationExecutionFilter,
    autKey,
    autValues,

    -- ** AutomationExecutionMetadata
    AutomationExecutionMetadata,
    automationExecutionMetadata,
    aemCurrentStepName,
    aemTargetParameterName,
    aemLogFile,
    aemExecutedBy,
    aemDocumentName,
    aemExecutionEndTime,
    aemFailureMessage,
    aemMode,
    aemTargetMaps,
    aemAutomationExecutionStatus,
    aemParentAutomationExecutionId,
    aemOutputs,
    aemMaxErrors,
    aemExecutionStartTime,
    aemAutomationType,
    aemCurrentAction,
    aemTargets,
    aemResolvedTargets,
    aemDocumentVersion,
    aemAutomationExecutionId,
    aemMaxConcurrency,
    aemTarget,

    -- ** CloudWatchOutputConfig
    CloudWatchOutputConfig,
    cloudWatchOutputConfig,
    cwocCloudWatchLogGroupName,
    cwocCloudWatchOutputEnabled,

    -- ** Command
    Command,
    command,
    cStatus,
    cExpiresAfter,
    cNotificationConfig,
    cTargetCount,
    cCloudWatchOutputConfig,
    cDeliveryTimedOutCount,
    cOutputS3KeyPrefix,
    cDocumentName,
    cErrorCount,
    cStatusDetails,
    cMaxErrors,
    cInstanceIds,
    cOutputS3Region,
    cTargets,
    cCommandId,
    cParameters,
    cDocumentVersion,
    cTimeoutSeconds,
    cComment,
    cCompletedCount,
    cOutputS3BucketName,
    cMaxConcurrency,
    cRequestedDateTime,
    cServiceRole,

    -- ** CommandFilter
    CommandFilter,
    commandFilter,
    cfKey,
    cfValue,

    -- ** CommandInvocation
    CommandInvocation,
    commandInvocation,
    comInstanceId,
    comStatus,
    comNotificationConfig,
    comCommandPlugins,
    comCloudWatchOutputConfig,
    comDocumentName,
    comStandardErrorURL,
    comStatusDetails,
    comStandardOutputURL,
    comCommandId,
    comDocumentVersion,
    comComment,
    comTraceOutput,
    comInstanceName,
    comRequestedDateTime,
    comServiceRole,

    -- ** CommandPlugin
    CommandPlugin,
    commandPlugin,
    cpStatus,
    cpResponseStartDateTime,
    cpOutputS3KeyPrefix,
    cpStandardErrorURL,
    cpResponseCode,
    cpStatusDetails,
    cpOutput,
    cpStandardOutputURL,
    cpName,
    cpOutputS3Region,
    cpOutputS3BucketName,
    cpResponseFinishDateTime,

    -- ** ComplianceExecutionSummary
    ComplianceExecutionSummary,
    complianceExecutionSummary,
    cesExecutionId,
    cesExecutionType,
    cesExecutionTime,

    -- ** ComplianceItem
    ComplianceItem,
    complianceItem,
    ciStatus,
    ciResourceId,
    ciResourceType,
    ciSeverity,
    ciExecutionSummary,
    ciDetails,
    ciId,
    ciComplianceType,
    ciTitle,

    -- ** ComplianceItemEntry
    ComplianceItemEntry,
    complianceItemEntry,
    cieDetails,
    cieId,
    cieTitle,
    cieSeverity,
    cieStatus,

    -- ** ComplianceStringFilter
    ComplianceStringFilter,
    complianceStringFilter,
    csfValues,
    csfKey,
    csfType,

    -- ** ComplianceSummaryItem
    ComplianceSummaryItem,
    complianceSummaryItem,
    csiNonCompliantSummary,
    csiCompliantSummary,
    csiComplianceType,

    -- ** CompliantSummary
    CompliantSummary,
    compliantSummary,
    csCompliantCount,
    csSeveritySummary,

    -- ** CreateAssociationBatchRequestEntry
    CreateAssociationBatchRequestEntry,
    createAssociationBatchRequestEntry,
    cabreInstanceId,
    cabreApplyOnlyAtCronInterval,
    cabreMaxErrors,
    cabreScheduleExpression,
    cabreOutputLocation,
    cabreSyncCompliance,
    cabreTargets,
    cabreParameters,
    cabreDocumentVersion,
    cabreAutomationTargetParameterName,
    cabreAssociationName,
    cabreComplianceSeverity,
    cabreMaxConcurrency,
    cabreName,

    -- ** DescribeActivationsFilter
    DescribeActivationsFilter,
    describeActivationsFilter,
    dafFilterKey,
    dafFilterValues,

    -- ** DocumentDefaultVersionDescription
    DocumentDefaultVersionDescription,
    documentDefaultVersionDescription,
    ddvdDefaultVersionName,
    ddvdDefaultVersion,
    ddvdName,

    -- ** DocumentDescription
    DocumentDescription,
    documentDescription,
    dStatus,
    dDocumentType,
    dHash,
    dVersionName,
    dSchemaVersion,
    dSha1,
    dAttachmentsInformation,
    dDefaultVersion,
    dTargetType,
    dOwner,
    dPlatformTypes,
    dCreatedDate,
    dDocumentFormat,
    dName,
    dHashType,
    dParameters,
    dDocumentVersion,
    dStatusInformation,
    dDescription,
    dRequires,
    dTags,
    dLatestVersion,

    -- ** DocumentFilter
    DocumentFilter,
    documentFilter,
    dfKey,
    dfValue,

    -- ** DocumentIdentifier
    DocumentIdentifier,
    documentIdentifier,
    diDocumentType,
    diVersionName,
    diSchemaVersion,
    diTargetType,
    diOwner,
    diPlatformTypes,
    diDocumentFormat,
    diName,
    diDocumentVersion,
    diRequires,
    diTags,

    -- ** DocumentKeyValuesFilter
    DocumentKeyValuesFilter,
    documentKeyValuesFilter,
    dkvfValues,
    dkvfKey,

    -- ** DocumentParameter
    DocumentParameter,
    documentParameter,
    dpName,
    dpDefaultValue,
    dpType,
    dpDescription,

    -- ** DocumentRequires
    DocumentRequires,
    documentRequires,
    drVersion,
    drName,

    -- ** DocumentVersionInfo
    DocumentVersionInfo,
    documentVersionInfo,
    dviStatus,
    dviVersionName,
    dviCreatedDate,
    dviDocumentFormat,
    dviName,
    dviDocumentVersion,
    dviStatusInformation,
    dviIsDefaultVersion,

    -- ** EffectivePatch
    EffectivePatch,
    effectivePatch,
    epPatch,
    epPatchStatus,

    -- ** FailedCreateAssociation
    FailedCreateAssociation,
    failedCreateAssociation,
    fcaEntry,
    fcaFault,
    fcaMessage,

    -- ** FailureDetails
    FailureDetails,
    failureDetails,
    fdFailureType,
    fdFailureStage,
    fdDetails,

    -- ** InstanceAggregatedAssociationOverview
    InstanceAggregatedAssociationOverview,
    instanceAggregatedAssociationOverview,
    iaaoDetailedStatus,
    iaaoInstanceAssociationStatusAggregatedCount,

    -- ** InstanceAssociation
    InstanceAssociation,
    instanceAssociation,
    iaAssociationId,
    iaInstanceId,
    iaContent,
    iaAssociationVersion,

    -- ** InstanceAssociationOutputLocation
    InstanceAssociationOutputLocation,
    instanceAssociationOutputLocation,
    iaolS3Location,

    -- ** InstanceAssociationOutputURL
    InstanceAssociationOutputURL,
    instanceAssociationOutputURL,
    iaouS3OutputURL,

    -- ** InstanceAssociationStatusInfo
    InstanceAssociationStatusInfo,
    instanceAssociationStatusInfo,
    iasiAssociationId,
    iasiInstanceId,
    iasiDetailedStatus,
    iasiStatus,
    iasiOutputURL,
    iasiExecutionSummary,
    iasiName,
    iasiErrorCode,
    iasiDocumentVersion,
    iasiAssociationVersion,
    iasiExecutionDate,
    iasiAssociationName,

    -- ** InstanceInformation
    InstanceInformation,
    instanceInformation,
    iiInstanceId,
    iiPingStatus,
    iiIPAddress,
    iiResourceType,
    iiRegistrationDate,
    iiPlatformVersion,
    iiIsLatestVersion,
    iiAgentVersion,
    iiLastPingDateTime,
    iiLastSuccessfulAssociationExecutionDate,
    iiActivationId,
    iiName,
    iiPlatformType,
    iiAssociationOverview,
    iiAssociationStatus,
    iiLastAssociationExecutionDate,
    iiPlatformName,
    iiComputerName,
    iiIAMRole,

    -- ** InstanceInformationFilter
    InstanceInformationFilter,
    instanceInformationFilter,
    iifKey,
    iifValueSet,

    -- ** InstanceInformationStringFilter
    InstanceInformationStringFilter,
    instanceInformationStringFilter,
    iisfKey,
    iisfValues,

    -- ** InstancePatchState
    InstancePatchState,
    instancePatchState,
    ipsUnreportedNotApplicableCount,
    ipsRebootOption,
    ipsInstalledPendingRebootCount,
    ipsOwnerInformation,
    ipsInstalledRejectedCount,
    ipsFailedCount,
    ipsInstalledOtherCount,
    ipsMissingCount,
    ipsInstallOverrideList,
    ipsNotApplicableCount,
    ipsInstalledCount,
    ipsLastNoRebootInstallOperationTime,
    ipsSnapshotId,
    ipsInstanceId,
    ipsPatchGroup,
    ipsBaselineId,
    ipsOperationStartTime,
    ipsOperationEndTime,
    ipsOperation,

    -- ** InstancePatchStateFilter
    InstancePatchStateFilter,
    instancePatchStateFilter,
    ipsfKey,
    ipsfValues,
    ipsfType,

    -- ** InventoryAggregator
    InventoryAggregator,
    inventoryAggregator,
    iaGroups,
    iaAggregators,
    iaExpression,

    -- ** InventoryDeletionStatusItem
    InventoryDeletionStatusItem,
    inventoryDeletionStatusItem,
    idsiTypeName,
    idsiLastStatusUpdateTime,
    idsiLastStatusMessage,
    idsiDeletionSummary,
    idsiLastStatus,
    idsiDeletionStartTime,
    idsiDeletionId,

    -- ** InventoryDeletionSummary
    InventoryDeletionSummary,
    inventoryDeletionSummary,
    idsRemainingCount,
    idsSummaryItems,
    idsTotalCount,

    -- ** InventoryDeletionSummaryItem
    InventoryDeletionSummaryItem,
    inventoryDeletionSummaryItem,
    idsiRemainingCount,
    idsiCount,
    idsiVersion,

    -- ** InventoryFilter
    InventoryFilter,
    inventoryFilter,
    ifType,
    ifKey,
    ifValues,

    -- ** InventoryGroup
    InventoryGroup,
    inventoryGroup,
    igName,
    igFilters,

    -- ** InventoryItem
    InventoryItem,
    inventoryItem,
    iiContext,
    iiContentHash,
    iiContent,
    iiTypeName,
    iiSchemaVersion,
    iiCaptureTime,

    -- ** InventoryItemAttribute
    InventoryItemAttribute,
    inventoryItemAttribute,
    iiaName,
    iiaDataType,

    -- ** InventoryItemSchema
    InventoryItemSchema,
    inventoryItemSchema,
    iisVersion,
    iisDisplayName,
    iisTypeName,
    iisAttributes,

    -- ** InventoryResultEntity
    InventoryResultEntity,
    inventoryResultEntity,
    ireData,
    ireId,

    -- ** InventoryResultItem
    InventoryResultItem,
    inventoryResultItem,
    iriContentHash,
    iriCaptureTime,
    iriTypeName,
    iriSchemaVersion,
    iriContent,

    -- ** LoggingInfo
    LoggingInfo,
    loggingInfo,
    liS3KeyPrefix,
    liS3BucketName,
    liS3Region,

    -- ** MaintenanceWindowAutomationParameters
    MaintenanceWindowAutomationParameters,
    maintenanceWindowAutomationParameters,
    mwapParameters,
    mwapDocumentVersion,

    -- ** MaintenanceWindowExecution
    MaintenanceWindowExecution,
    maintenanceWindowExecution,
    mweStatus,
    mweStartTime,
    mweWindowExecutionId,
    mweStatusDetails,
    mweEndTime,
    mweWindowId,

    -- ** MaintenanceWindowExecutionTaskIdentity
    MaintenanceWindowExecutionTaskIdentity,
    maintenanceWindowExecutionTaskIdentity,
    mwetiStatus,
    mwetiTaskExecutionId,
    mwetiStartTime,
    mwetiTaskType,
    mwetiTaskARN,
    mwetiWindowExecutionId,
    mwetiStatusDetails,
    mwetiEndTime,

    -- ** MaintenanceWindowExecutionTaskInvocationIdentity
    MaintenanceWindowExecutionTaskInvocationIdentity,
    maintenanceWindowExecutionTaskInvocationIdentity,
    mwetiiStatus,
    mwetiiExecutionId,
    mwetiiTaskExecutionId,
    mwetiiStartTime,
    mwetiiInvocationId,
    mwetiiOwnerInformation,
    mwetiiTaskType,
    mwetiiWindowTargetId,
    mwetiiWindowExecutionId,
    mwetiiStatusDetails,
    mwetiiEndTime,
    mwetiiParameters,

    -- ** MaintenanceWindowFilter
    MaintenanceWindowFilter,
    maintenanceWindowFilter,
    mwfValues,
    mwfKey,

    -- ** MaintenanceWindowIdentity
    MaintenanceWindowIdentity,
    maintenanceWindowIdentity,
    mwiEnabled,
    mwiSchedule,
    mwiNextExecutionTime,
    mwiScheduleOffset,
    mwiEndDate,
    mwiScheduleTimezone,
    mwiStartDate,
    mwiName,
    mwiCutoff,
    mwiDescription,
    mwiDuration,
    mwiWindowId,

    -- ** MaintenanceWindowIdentityForTarget
    MaintenanceWindowIdentityForTarget,
    maintenanceWindowIdentityForTarget,
    mwiftName,
    mwiftWindowId,

    -- ** MaintenanceWindowLambdaParameters
    MaintenanceWindowLambdaParameters,
    maintenanceWindowLambdaParameters,
    mwlpPayload,
    mwlpQualifier,
    mwlpClientContext,

    -- ** MaintenanceWindowRunCommandParameters
    MaintenanceWindowRunCommandParameters,
    maintenanceWindowRunCommandParameters,
    mwrcpServiceRoleARN,
    mwrcpNotificationConfig,
    mwrcpDocumentHashType,
    mwrcpCloudWatchOutputConfig,
    mwrcpOutputS3KeyPrefix,
    mwrcpParameters,
    mwrcpDocumentHash,
    mwrcpDocumentVersion,
    mwrcpTimeoutSeconds,
    mwrcpComment,
    mwrcpOutputS3BucketName,

    -- ** MaintenanceWindowStepFunctionsParameters
    MaintenanceWindowStepFunctionsParameters,
    maintenanceWindowStepFunctionsParameters,
    mwsfpInput,
    mwsfpName,

    -- ** MaintenanceWindowTarget
    MaintenanceWindowTarget,
    maintenanceWindowTarget,
    mResourceType,
    mOwnerInformation,
    mWindowTargetId,
    mName,
    mTargets,
    mDescription,
    mWindowId,

    -- ** MaintenanceWindowTask
    MaintenanceWindowTask,
    maintenanceWindowTask,
    mwtServiceRoleARN,
    mwtWindowTaskId,
    mwtTaskParameters,
    mwtPriority,
    mwtTaskARN,
    mwtMaxErrors,
    mwtName,
    mwtTargets,
    mwtLoggingInfo,
    mwtType,
    mwtDescription,
    mwtMaxConcurrency,
    mwtWindowId,

    -- ** MaintenanceWindowTaskInvocationParameters
    MaintenanceWindowTaskInvocationParameters,
    maintenanceWindowTaskInvocationParameters,
    mwtipAutomation,
    mwtipStepFunctions,
    mwtipRunCommand,
    mwtipLambda,

    -- ** MaintenanceWindowTaskParameterValueExpression
    MaintenanceWindowTaskParameterValueExpression,
    maintenanceWindowTaskParameterValueExpression,
    mwtpveValues,

    -- ** NonCompliantSummary
    NonCompliantSummary,
    nonCompliantSummary,
    ncsNonCompliantCount,
    ncsSeveritySummary,

    -- ** NotificationConfig
    NotificationConfig,
    notificationConfig,
    ncNotificationEvents,
    ncNotificationType,
    ncNotificationARN,

    -- ** OpsAggregator
    OpsAggregator,
    opsAggregator,
    oaTypeName,
    oaAggregators,
    oaValues,
    oaFilters,
    oaAttributeName,
    oaAggregatorType,

    -- ** OpsEntity
    OpsEntity,
    opsEntity,
    oeData,
    oeId,

    -- ** OpsEntityItem
    OpsEntityItem,
    opsEntityItem,
    oeiContent,
    oeiCaptureTime,

    -- ** OpsFilter
    OpsFilter,
    opsFilter,
    ofType,
    ofKey,
    ofValues,

    -- ** OpsItem
    OpsItem,
    opsItem,
    oiOpsItemId,
    oiStatus,
    oiPriority,
    oiCreatedTime,
    oiCategory,
    oiSeverity,
    oiCreatedBy,
    oiLastModifiedTime,
    oiVersion,
    oiSource,
    oiRelatedOpsItems,
    oiTitle,
    oiLastModifiedBy,
    oiOperationalData,
    oiDescription,
    oiNotifications,

    -- ** OpsItemDataValue
    OpsItemDataValue,
    opsItemDataValue,
    oidvValue,
    oidvType,

    -- ** OpsItemFilter
    OpsItemFilter,
    opsItemFilter,
    oifKey,
    oifValues,
    oifOperator,

    -- ** OpsItemNotification
    OpsItemNotification,
    opsItemNotification,
    oinARN,

    -- ** OpsItemSummary
    OpsItemSummary,
    opsItemSummary,
    oisOpsItemId,
    oisStatus,
    oisPriority,
    oisCreatedTime,
    oisCategory,
    oisSeverity,
    oisCreatedBy,
    oisLastModifiedTime,
    oisSource,
    oisTitle,
    oisLastModifiedBy,
    oisOperationalData,

    -- ** OpsResultAttribute
    OpsResultAttribute,
    opsResultAttribute,
    oraTypeName,

    -- ** OutputSource
    OutputSource,
    outputSource,
    osOutputSourceId,
    osOutputSourceType,

    -- ** Parameter
    Parameter,
    parameter,
    pLastModifiedDate,
    pSelector,
    pARN,
    pValue,
    pSourceResult,
    pName,
    pVersion,
    pType,
    pDataType,

    -- ** ParameterHistory
    ParameterHistory,
    parameterHistory,
    phLastModifiedDate,
    phKeyId,
    phValue,
    phName,
    phTier,
    phVersion,
    phLastModifiedUser,
    phLabels,
    phAllowedPattern,
    phType,
    phDataType,
    phDescription,
    phPolicies,

    -- ** ParameterInlinePolicy
    ParameterInlinePolicy,
    parameterInlinePolicy,
    pipPolicyType,
    pipPolicyStatus,
    pipPolicyText,

    -- ** ParameterMetadata
    ParameterMetadata,
    parameterMetadata,
    pmLastModifiedDate,
    pmKeyId,
    pmName,
    pmTier,
    pmVersion,
    pmLastModifiedUser,
    pmAllowedPattern,
    pmType,
    pmDataType,
    pmDescription,
    pmPolicies,

    -- ** ParameterStringFilter
    ParameterStringFilter,
    parameterStringFilter,
    psfValues,
    psfOption,
    psfKey,

    -- ** ParametersFilter
    ParametersFilter,
    parametersFilter,
    pKey,
    pValues,

    -- ** Patch
    Patch,
    patch,
    patBugzillaIds,
    patVendor,
    patMsrcSeverity,
    patRepository,
    patProductFamily,
    patSeverity,
    patAdvisoryIds,
    patCVEIds,
    patClassification,
    patRelease,
    patMsrcNumber,
    patName,
    patVersion,
    patLanguage,
    patKbNumber,
    patContentURL,
    patId,
    patReleaseDate,
    patTitle,
    patArch,
    patProduct,
    patDescription,
    patEpoch,

    -- ** PatchBaselineIdentity
    PatchBaselineIdentity,
    patchBaselineIdentity,
    pbiBaselineName,
    pbiBaselineDescription,
    pbiOperatingSystem,
    pbiDefaultBaseline,
    pbiBaselineId,

    -- ** PatchComplianceData
    PatchComplianceData,
    patchComplianceData,
    pcdCVEIds,
    pcdTitle,
    pcdKBId,
    pcdClassification,
    pcdSeverity,
    pcdState,
    pcdInstalledTime,

    -- ** PatchFilter
    PatchFilter,
    patchFilter,
    pfKey,
    pfValues,

    -- ** PatchFilterGroup
    PatchFilterGroup,
    patchFilterGroup,
    pfgPatchFilters,

    -- ** PatchGroupPatchBaselineMapping
    PatchGroupPatchBaselineMapping,
    patchGroupPatchBaselineMapping,
    pgpbmBaselineIdentity,
    pgpbmPatchGroup,

    -- ** PatchOrchestratorFilter
    PatchOrchestratorFilter,
    patchOrchestratorFilter,
    pofValues,
    pofKey,

    -- ** PatchRule
    PatchRule,
    patchRule,
    prApproveAfterDays,
    prApproveUntilDate,
    prEnableNonSecurity,
    prComplianceLevel,
    prPatchFilterGroup,

    -- ** PatchRuleGroup
    PatchRuleGroup,
    patchRuleGroup,
    prgPatchRules,

    -- ** PatchSource
    PatchSource,
    patchSource,
    psName,
    psProducts,
    psConfiguration,

    -- ** PatchStatus
    PatchStatus,
    patchStatus,
    psApprovalDate,
    psDeploymentStatus,
    psComplianceLevel,

    -- ** ProgressCounters
    ProgressCounters,
    progressCounters,
    pcFailedSteps,
    pcCancelledSteps,
    pcSuccessSteps,
    pcTotalSteps,
    pcTimedOutSteps,

    -- ** RelatedOpsItem
    RelatedOpsItem,
    relatedOpsItem,
    roiOpsItemId,

    -- ** ResolvedTargets
    ResolvedTargets,
    resolvedTargets,
    rtTruncated,
    rtParameterValues,

    -- ** ResourceComplianceSummaryItem
    ResourceComplianceSummaryItem,
    resourceComplianceSummaryItem,
    rcsiNonCompliantSummary,
    rcsiStatus,
    rcsiResourceId,
    rcsiResourceType,
    rcsiCompliantSummary,
    rcsiExecutionSummary,
    rcsiOverallSeverity,
    rcsiComplianceType,

    -- ** ResourceDataSyncAWSOrganizationsSource
    ResourceDataSyncAWSOrganizationsSource,
    resourceDataSyncAWSOrganizationsSource,
    rdsaosOrganizationalUnits,
    rdsaosOrganizationSourceType,

    -- ** ResourceDataSyncDestinationDataSharing
    ResourceDataSyncDestinationDataSharing,
    resourceDataSyncDestinationDataSharing,
    rdsddsDestinationDataSharingType,

    -- ** ResourceDataSyncItem
    ResourceDataSyncItem,
    resourceDataSyncItem,
    rdsiSyncType,
    rdsiSyncSource,
    rdsiLastSyncStatusMessage,
    rdsiSyncCreatedTime,
    rdsiLastSyncTime,
    rdsiSyncName,
    rdsiLastStatus,
    rdsiSyncLastModifiedTime,
    rdsiS3Destination,
    rdsiLastSuccessfulSyncTime,

    -- ** ResourceDataSyncOrganizationalUnit
    ResourceDataSyncOrganizationalUnit,
    resourceDataSyncOrganizationalUnit,
    rdsouOrganizationalUnitId,

    -- ** ResourceDataSyncS3Destination
    ResourceDataSyncS3Destination,
    resourceDataSyncS3Destination,
    rdssdPrefix,
    rdssdDestinationDataSharing,
    rdssdAWSKMSKeyARN,
    rdssdBucketName,
    rdssdSyncFormat,
    rdssdRegion,

    -- ** ResourceDataSyncSource
    ResourceDataSyncSource,
    resourceDataSyncSource,
    rdssIncludeFutureRegions,
    rdssAWSOrganizationsSource,
    rdssSourceType,
    rdssSourceRegions,

    -- ** ResourceDataSyncSourceWithState
    ResourceDataSyncSourceWithState,
    resourceDataSyncSourceWithState,
    rdsswsState,
    rdsswsIncludeFutureRegions,
    rdsswsSourceType,
    rdsswsAWSOrganizationsSource,
    rdsswsSourceRegions,

    -- ** ResultAttribute
    ResultAttribute,
    resultAttribute,
    raTypeName,

    -- ** S3OutputLocation
    S3OutputLocation,
    s3OutputLocation,
    solOutputS3KeyPrefix,
    solOutputS3Region,
    solOutputS3BucketName,

    -- ** S3OutputURL
    S3OutputURL,
    s3OutputURL,
    souOutputURL,

    -- ** ScheduledWindowExecution
    ScheduledWindowExecution,
    scheduledWindowExecution,
    sweExecutionTime,
    sweName,
    sweWindowId,

    -- ** ServiceSetting
    ServiceSetting,
    serviceSetting,
    ssStatus,
    ssLastModifiedDate,
    ssARN,
    ssSettingId,
    ssLastModifiedUser,
    ssSettingValue,

    -- ** Session
    Session,
    session,
    sesStatus,
    sesOutputURL,
    sesDocumentName,
    sesEndDate,
    sesOwner,
    sesStartDate,
    sesDetails,
    sesSessionId,
    sesTarget,

    -- ** SessionFilter
    SessionFilter,
    sessionFilter,
    sfKey,
    sfValue,

    -- ** SessionManagerOutputURL
    SessionManagerOutputURL,
    sessionManagerOutputURL,
    smouS3OutputURL,
    smouCloudWatchOutputURL,

    -- ** SeveritySummary
    SeveritySummary,
    severitySummary,
    ssLowCount,
    ssUnspecifiedCount,
    ssHighCount,
    ssMediumCount,
    ssInformationalCount,
    ssCriticalCount,

    -- ** StepExecution
    StepExecution,
    stepExecution,
    seFailureDetails,
    seIsEnd,
    seInputs,
    seStepName,
    seExecutionEndTime,
    seFailureMessage,
    seResponse,
    seAction,
    seResponseCode,
    seStepStatus,
    seTargetLocation,
    seOverriddenParameters,
    seOutputs,
    seExecutionStartTime,
    seMaxAttempts,
    seTargets,
    seNextStep,
    seStepExecutionId,
    seValidNextSteps,
    seTimeoutSeconds,
    seOnFailure,
    seIsCritical,

    -- ** StepExecutionFilter
    StepExecutionFilter,
    stepExecutionFilter,
    sefKey,
    sefValues,

    -- ** Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- ** Target
    Target,
    target,
    tValues,
    tKey,

    -- ** TargetLocation
    TargetLocation,
    targetLocation,
    tlAccounts,
    tlTargetLocationMaxConcurrency,
    tlTargetLocationMaxErrors,
    tlRegions,
    tlExecutionRoleName,
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
