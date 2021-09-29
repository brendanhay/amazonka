{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidAutomationSignalException,
    _InvalidDeleteInventoryParametersException,
    _OpsMetadataKeyLimitExceededException,
    _FeatureNotAvailableException,
    _InvalidInstanceInformationFilterValue,
    _InvalidAggregatorException,
    _ComplianceTypeCountLimitExceededException,
    _AutomationDefinitionVersionNotFoundException,
    _UnsupportedParameterType,
    _InvalidDocumentVersion,
    _AutomationExecutionLimitExceededException,
    _IdempotentParameterMismatch,
    _OpsMetadataInvalidArgumentException,
    _ResourceDataSyncConflictException,
    _InvalidAutomationStatusUpdateException,
    _HierarchyTypeMismatchException,
    _DoesNotExistException,
    _ItemSizeLimitExceededException,
    _DocumentAlreadyExists,
    _ParameterMaxVersionLimitExceeded,
    _DocumentLimitExceeded,
    _AutomationStepNotFoundException,
    _AutomationExecutionNotFoundException,
    _DocumentPermissionLimit,
    _InvalidFilter,
    _InvalidTypeNameException,
    _SubTypeCountLimitExceededException,
    _AutomationDefinitionNotFoundException,
    _OpsItemRelatedItemAssociationNotFoundException,
    _InvalidInventoryItemContextException,
    _InvalidDocument,
    _TooManyTagsError,
    _OpsItemInvalidParameterException,
    _IncompatiblePolicyException,
    _InvalidAllowedPatternException,
    _OpsMetadataTooManyUpdatesException,
    _InvalidParameters,
    _TargetNotConnected,
    _ParameterVersionLabelLimitExceeded,
    _UnsupportedInventorySchemaVersionException,
    _InvalidCommandId,
    _InvalidOutputLocation,
    _ResourceLimitExceededException,
    _InvalidTarget,
    _ServiceSettingNotFound,
    _CustomSchemaCountLimitExceededException,
    _InvalidUpdate,
    _InvalidAssociation,
    _InvalidDocumentSchemaVersion,
    _InvalidOptionException,
    _OpsItemNotFoundException,
    _StatusUnchanged,
    _OpsMetadataNotFoundException,
    _InvalidInstanceId,
    _TooManyUpdates,
    _DuplicateDocumentVersionName,
    _OpsItemRelatedItemAlreadyExistsException,
    _InvalidPolicyAttributeException,
    _AssociationDoesNotExist,
    _InvalidAssociationVersion,
    _InvalidFilterValue,
    _ResourceDataSyncCountExceededException,
    _InternalServerError,
    _ParameterNotFound,
    _AssociationLimitExceeded,
    _AssociationAlreadyExists,
    _ParameterLimitExceeded,
    _InvalidDocumentContent,
    _ItemContentMismatchException,
    _UnsupportedFeatureRequiredException,
    _PoliciesLimitExceededException,
    _InvalidPluginName,
    _ParameterAlreadyExists,
    _InvalidDeletionIdException,
    _ResourceDataSyncAlreadyExistsException,
    _InvalidSchedule,
    _ResourceDataSyncNotFoundException,
    _AlreadyExistsException,
    _InvalidAutomationExecutionParametersException,
    _DuplicateDocumentContent,
    _ResourceInUseException,
    _InvalidPolicyTypeException,
    _InvalidNotificationConfig,
    _InvalidFilterKey,
    _InvalidDocumentType,
    _InvalidResourceId,
    _TotalSizeLimitExceededException,
    _DuplicateInstanceId,
    _AutomationDefinitionNotApprovedException,
    _InvalidDocumentOperation,
    _InvalidKeyId,
    _InvalidResultAttributeException,
    _InvocationDoesNotExist,
    _InvalidResourceType,
    _DocumentVersionLimitExceeded,
    _ResourceDataSyncInvalidConfigurationException,
    _InvalidInventoryGroupException,
    _AssociationExecutionDoesNotExist,
    _OpsMetadataLimitExceededException,
    _OpsMetadataAlreadyExistsException,
    _InvalidOutputFolder,
    _HierarchyLevelLimitExceededException,
    _OpsItemLimitExceededException,
    _UnsupportedCalendarException,
    _ParameterVersionNotFound,
    _OpsItemAlreadyExistsException,
    _InvalidActivationId,
    _InvalidRole,
    _InvalidActivation,
    _InvalidInventoryRequestException,
    _InvalidNextToken,
    _AssociationVersionLimitExceeded,
    _MaxDocumentSizeExceeded,
    _UnsupportedOperatingSystem,
    _UnsupportedPlatformType,
    _AssociatedInstances,
    _InvalidItemContentException,
    _InvalidPermissionType,
    _UnsupportedInventoryItemContextException,
    _InvalidFilterOption,
    _ParameterPatternMismatchException,
    _TargetInUseException,

    -- * AssociationComplianceSeverity
    AssociationComplianceSeverity (..),

    -- * AssociationExecutionFilterKey
    AssociationExecutionFilterKey (..),

    -- * AssociationExecutionTargetsFilterKey
    AssociationExecutionTargetsFilterKey (..),

    -- * AssociationFilterKey
    AssociationFilterKey (..),

    -- * AssociationFilterOperatorType
    AssociationFilterOperatorType (..),

    -- * AssociationStatusName
    AssociationStatusName (..),

    -- * AssociationSyncCompliance
    AssociationSyncCompliance (..),

    -- * AttachmentHashType
    AttachmentHashType (..),

    -- * AttachmentsSourceKey
    AttachmentsSourceKey (..),

    -- * AutomationExecutionFilterKey
    AutomationExecutionFilterKey (..),

    -- * AutomationExecutionStatus
    AutomationExecutionStatus (..),

    -- * AutomationSubtype
    AutomationSubtype (..),

    -- * AutomationType
    AutomationType (..),

    -- * CalendarState
    CalendarState (..),

    -- * CommandFilterKey
    CommandFilterKey (..),

    -- * CommandInvocationStatus
    CommandInvocationStatus (..),

    -- * CommandPluginStatus
    CommandPluginStatus (..),

    -- * CommandStatus
    CommandStatus (..),

    -- * ComplianceQueryOperatorType
    ComplianceQueryOperatorType (..),

    -- * ComplianceSeverity
    ComplianceSeverity (..),

    -- * ComplianceStatus
    ComplianceStatus (..),

    -- * ComplianceUploadType
    ComplianceUploadType (..),

    -- * ConnectionStatus
    ConnectionStatus (..),

    -- * DescribeActivationsFilterKeys
    DescribeActivationsFilterKeys (..),

    -- * DocumentFilterKey
    DocumentFilterKey (..),

    -- * DocumentFormat
    DocumentFormat (..),

    -- * DocumentHashType
    DocumentHashType (..),

    -- * DocumentMetadataEnum
    DocumentMetadataEnum (..),

    -- * DocumentParameterType
    DocumentParameterType (..),

    -- * DocumentPermissionType
    DocumentPermissionType (..),

    -- * DocumentReviewAction
    DocumentReviewAction (..),

    -- * DocumentReviewCommentType
    DocumentReviewCommentType (..),

    -- * DocumentStatus
    DocumentStatus (..),

    -- * DocumentType
    DocumentType (..),

    -- * ExecutionMode
    ExecutionMode (..),

    -- * Fault
    Fault (..),

    -- * InstanceInformationFilterKey
    InstanceInformationFilterKey (..),

    -- * InstancePatchStateOperatorType
    InstancePatchStateOperatorType (..),

    -- * InventoryAttributeDataType
    InventoryAttributeDataType (..),

    -- * InventoryDeletionStatus
    InventoryDeletionStatus (..),

    -- * InventoryQueryOperatorType
    InventoryQueryOperatorType (..),

    -- * InventorySchemaDeleteOption
    InventorySchemaDeleteOption (..),

    -- * LastResourceDataSyncStatus
    LastResourceDataSyncStatus (..),

    -- * MaintenanceWindowExecutionStatus
    MaintenanceWindowExecutionStatus (..),

    -- * MaintenanceWindowResourceType
    MaintenanceWindowResourceType (..),

    -- * MaintenanceWindowTaskCutoffBehavior
    MaintenanceWindowTaskCutoffBehavior (..),

    -- * MaintenanceWindowTaskType
    MaintenanceWindowTaskType (..),

    -- * NotificationEvent
    NotificationEvent (..),

    -- * NotificationType
    NotificationType (..),

    -- * OperatingSystem
    OperatingSystem (..),

    -- * OpsFilterOperatorType
    OpsFilterOperatorType (..),

    -- * OpsItemDataType
    OpsItemDataType (..),

    -- * OpsItemEventFilterKey
    OpsItemEventFilterKey (..),

    -- * OpsItemEventFilterOperator
    OpsItemEventFilterOperator (..),

    -- * OpsItemFilterKey
    OpsItemFilterKey (..),

    -- * OpsItemFilterOperator
    OpsItemFilterOperator (..),

    -- * OpsItemRelatedItemsFilterKey
    OpsItemRelatedItemsFilterKey (..),

    -- * OpsItemRelatedItemsFilterOperator
    OpsItemRelatedItemsFilterOperator (..),

    -- * OpsItemStatus
    OpsItemStatus (..),

    -- * ParameterTier
    ParameterTier (..),

    -- * ParameterType
    ParameterType (..),

    -- * ParametersFilterKey
    ParametersFilterKey (..),

    -- * PatchAction
    PatchAction (..),

    -- * PatchComplianceDataState
    PatchComplianceDataState (..),

    -- * PatchComplianceLevel
    PatchComplianceLevel (..),

    -- * PatchDeploymentStatus
    PatchDeploymentStatus (..),

    -- * PatchFilterKey
    PatchFilterKey (..),

    -- * PatchOperationType
    PatchOperationType (..),

    -- * PatchProperty
    PatchProperty (..),

    -- * PatchSet
    PatchSet (..),

    -- * PingStatus
    PingStatus (..),

    -- * PlatformType
    PlatformType (..),

    -- * RebootOption
    RebootOption (..),

    -- * ResourceDataSyncS3Format
    ResourceDataSyncS3Format (..),

    -- * ResourceType
    ResourceType (..),

    -- * ResourceTypeForTagging
    ResourceTypeForTagging (..),

    -- * ReviewStatus
    ReviewStatus (..),

    -- * SessionFilterKey
    SessionFilterKey (..),

    -- * SessionState
    SessionState (..),

    -- * SessionStatus
    SessionStatus (..),

    -- * SignalType
    SignalType (..),

    -- * StepExecutionFilterKey
    StepExecutionFilterKey (..),

    -- * StopType
    StopType (..),

    -- * AccountSharingInfo
    AccountSharingInfo (..),
    newAccountSharingInfo,
    accountSharingInfo_accountId,
    accountSharingInfo_sharedDocumentVersion,

    -- * Activation
    Activation (..),
    newActivation,
    activation_createdDate,
    activation_registrationLimit,
    activation_iamRole,
    activation_activationId,
    activation_defaultInstanceName,
    activation_expired,
    activation_expirationDate,
    activation_tags,
    activation_registrationsCount,
    activation_description,

    -- * Association
    Association (..),
    newAssociation,
    association_instanceId,
    association_lastExecutionDate,
    association_overview,
    association_targets,
    association_name,
    association_scheduleExpression,
    association_associationId,
    association_associationName,
    association_associationVersion,
    association_documentVersion,

    -- * AssociationDescription
    AssociationDescription (..),
    newAssociationDescription,
    associationDescription_maxErrors,
    associationDescription_status,
    associationDescription_instanceId,
    associationDescription_lastExecutionDate,
    associationDescription_lastUpdateAssociationDate,
    associationDescription_complianceSeverity,
    associationDescription_overview,
    associationDescription_automationTargetParameterName,
    associationDescription_targets,
    associationDescription_name,
    associationDescription_scheduleExpression,
    associationDescription_targetLocations,
    associationDescription_date,
    associationDescription_associationId,
    associationDescription_maxConcurrency,
    associationDescription_calendarNames,
    associationDescription_associationName,
    associationDescription_associationVersion,
    associationDescription_lastSuccessfulExecutionDate,
    associationDescription_documentVersion,
    associationDescription_parameters,
    associationDescription_syncCompliance,
    associationDescription_outputLocation,
    associationDescription_applyOnlyAtCronInterval,

    -- * AssociationExecution
    AssociationExecution (..),
    newAssociationExecution,
    associationExecution_status,
    associationExecution_detailedStatus,
    associationExecution_lastExecutionDate,
    associationExecution_resourceCountByStatus,
    associationExecution_createdTime,
    associationExecution_executionId,
    associationExecution_associationId,
    associationExecution_associationVersion,

    -- * AssociationExecutionFilter
    AssociationExecutionFilter (..),
    newAssociationExecutionFilter,
    associationExecutionFilter_key,
    associationExecutionFilter_value,
    associationExecutionFilter_type,

    -- * AssociationExecutionTarget
    AssociationExecutionTarget (..),
    newAssociationExecutionTarget,
    associationExecutionTarget_resourceId,
    associationExecutionTarget_status,
    associationExecutionTarget_detailedStatus,
    associationExecutionTarget_lastExecutionDate,
    associationExecutionTarget_resourceType,
    associationExecutionTarget_outputSource,
    associationExecutionTarget_executionId,
    associationExecutionTarget_associationId,
    associationExecutionTarget_associationVersion,

    -- * AssociationExecutionTargetsFilter
    AssociationExecutionTargetsFilter (..),
    newAssociationExecutionTargetsFilter,
    associationExecutionTargetsFilter_key,
    associationExecutionTargetsFilter_value,

    -- * AssociationFilter
    AssociationFilter (..),
    newAssociationFilter,
    associationFilter_key,
    associationFilter_value,

    -- * AssociationOverview
    AssociationOverview (..),
    newAssociationOverview,
    associationOverview_status,
    associationOverview_detailedStatus,
    associationOverview_associationStatusAggregatedCount,

    -- * AssociationStatus
    AssociationStatus (..),
    newAssociationStatus,
    associationStatus_additionalInfo,
    associationStatus_date,
    associationStatus_name,
    associationStatus_message,

    -- * AssociationVersionInfo
    AssociationVersionInfo (..),
    newAssociationVersionInfo,
    associationVersionInfo_maxErrors,
    associationVersionInfo_createdDate,
    associationVersionInfo_complianceSeverity,
    associationVersionInfo_targets,
    associationVersionInfo_name,
    associationVersionInfo_scheduleExpression,
    associationVersionInfo_targetLocations,
    associationVersionInfo_associationId,
    associationVersionInfo_maxConcurrency,
    associationVersionInfo_calendarNames,
    associationVersionInfo_associationName,
    associationVersionInfo_associationVersion,
    associationVersionInfo_documentVersion,
    associationVersionInfo_parameters,
    associationVersionInfo_syncCompliance,
    associationVersionInfo_outputLocation,
    associationVersionInfo_applyOnlyAtCronInterval,

    -- * AttachmentContent
    AttachmentContent (..),
    newAttachmentContent,
    attachmentContent_hash,
    attachmentContent_name,
    attachmentContent_url,
    attachmentContent_size,
    attachmentContent_hashType,

    -- * AttachmentInformation
    AttachmentInformation (..),
    newAttachmentInformation,
    attachmentInformation_name,

    -- * AttachmentsSource
    AttachmentsSource (..),
    newAttachmentsSource,
    attachmentsSource_key,
    attachmentsSource_values,
    attachmentsSource_name,

    -- * AutomationExecution
    AutomationExecution (..),
    newAutomationExecution,
    automationExecution_maxErrors,
    automationExecution_currentAction,
    automationExecution_parentAutomationExecutionId,
    automationExecution_outputs,
    automationExecution_failureMessage,
    automationExecution_mode,
    automationExecution_executionEndTime,
    automationExecution_changeRequestName,
    automationExecution_automationExecutionId,
    automationExecution_executedBy,
    automationExecution_documentName,
    automationExecution_progressCounters,
    automationExecution_targets,
    automationExecution_resolvedTargets,
    automationExecution_executionStartTime,
    automationExecution_targetLocations,
    automationExecution_targetParameterName,
    automationExecution_currentStepName,
    automationExecution_opsItemId,
    automationExecution_associationId,
    automationExecution_scheduledTime,
    automationExecution_stepExecutionsTruncated,
    automationExecution_maxConcurrency,
    automationExecution_target,
    automationExecution_automationExecutionStatus,
    automationExecution_targetMaps,
    automationExecution_runbooks,
    automationExecution_automationSubtype,
    automationExecution_stepExecutions,
    automationExecution_documentVersion,
    automationExecution_parameters,

    -- * AutomationExecutionFilter
    AutomationExecutionFilter (..),
    newAutomationExecutionFilter,
    automationExecutionFilter_key,
    automationExecutionFilter_values,

    -- * AutomationExecutionMetadata
    AutomationExecutionMetadata (..),
    newAutomationExecutionMetadata,
    automationExecutionMetadata_maxErrors,
    automationExecutionMetadata_currentAction,
    automationExecutionMetadata_parentAutomationExecutionId,
    automationExecutionMetadata_outputs,
    automationExecutionMetadata_failureMessage,
    automationExecutionMetadata_mode,
    automationExecutionMetadata_executionEndTime,
    automationExecutionMetadata_changeRequestName,
    automationExecutionMetadata_automationExecutionId,
    automationExecutionMetadata_executedBy,
    automationExecutionMetadata_documentName,
    automationExecutionMetadata_targets,
    automationExecutionMetadata_resolvedTargets,
    automationExecutionMetadata_executionStartTime,
    automationExecutionMetadata_automationType,
    automationExecutionMetadata_targetParameterName,
    automationExecutionMetadata_currentStepName,
    automationExecutionMetadata_opsItemId,
    automationExecutionMetadata_associationId,
    automationExecutionMetadata_scheduledTime,
    automationExecutionMetadata_maxConcurrency,
    automationExecutionMetadata_target,
    automationExecutionMetadata_automationExecutionStatus,
    automationExecutionMetadata_targetMaps,
    automationExecutionMetadata_runbooks,
    automationExecutionMetadata_automationSubtype,
    automationExecutionMetadata_documentVersion,
    automationExecutionMetadata_logFile,

    -- * BaselineOverride
    BaselineOverride (..),
    newBaselineOverride,
    baselineOverride_rejectedPatches,
    baselineOverride_sources,
    baselineOverride_approvedPatchesEnableNonSecurity,
    baselineOverride_approvedPatchesComplianceLevel,
    baselineOverride_approvedPatches,
    baselineOverride_rejectedPatchesAction,
    baselineOverride_operatingSystem,
    baselineOverride_globalFilters,
    baselineOverride_approvalRules,

    -- * CloudWatchOutputConfig
    CloudWatchOutputConfig (..),
    newCloudWatchOutputConfig,
    cloudWatchOutputConfig_cloudWatchLogGroupName,
    cloudWatchOutputConfig_cloudWatchOutputEnabled,

    -- * Command
    Command (..),
    newCommand,
    command_instanceIds,
    command_maxErrors,
    command_notificationConfig,
    command_status,
    command_expiresAfter,
    command_requestedDateTime,
    command_serviceRole,
    command_statusDetails,
    command_completedCount,
    command_outputS3BucketName,
    command_errorCount,
    command_comment,
    command_documentName,
    command_commandId,
    command_targets,
    command_outputS3Region,
    command_maxConcurrency,
    command_deliveryTimedOutCount,
    command_timeoutSeconds,
    command_outputS3KeyPrefix,
    command_cloudWatchOutputConfig,
    command_documentVersion,
    command_targetCount,
    command_parameters,

    -- * CommandFilter
    CommandFilter (..),
    newCommandFilter,
    commandFilter_key,
    commandFilter_value,

    -- * CommandInvocation
    CommandInvocation (..),
    newCommandInvocation,
    commandInvocation_notificationConfig,
    commandInvocation_standardOutputUrl,
    commandInvocation_status,
    commandInvocation_instanceId,
    commandInvocation_requestedDateTime,
    commandInvocation_serviceRole,
    commandInvocation_statusDetails,
    commandInvocation_instanceName,
    commandInvocation_comment,
    commandInvocation_standardErrorUrl,
    commandInvocation_documentName,
    commandInvocation_commandId,
    commandInvocation_traceOutput,
    commandInvocation_commandPlugins,
    commandInvocation_cloudWatchOutputConfig,
    commandInvocation_documentVersion,

    -- * CommandPlugin
    CommandPlugin (..),
    newCommandPlugin,
    commandPlugin_standardOutputUrl,
    commandPlugin_status,
    commandPlugin_statusDetails,
    commandPlugin_outputS3BucketName,
    commandPlugin_standardErrorUrl,
    commandPlugin_name,
    commandPlugin_outputS3Region,
    commandPlugin_output,
    commandPlugin_responseFinishDateTime,
    commandPlugin_responseCode,
    commandPlugin_outputS3KeyPrefix,
    commandPlugin_responseStartDateTime,

    -- * ComplianceExecutionSummary
    ComplianceExecutionSummary (..),
    newComplianceExecutionSummary,
    complianceExecutionSummary_executionId,
    complianceExecutionSummary_executionType,
    complianceExecutionSummary_executionTime,

    -- * ComplianceItem
    ComplianceItem (..),
    newComplianceItem,
    complianceItem_resourceId,
    complianceItem_status,
    complianceItem_severity,
    complianceItem_title,
    complianceItem_complianceType,
    complianceItem_id,
    complianceItem_resourceType,
    complianceItem_details,
    complianceItem_executionSummary,

    -- * ComplianceItemEntry
    ComplianceItemEntry (..),
    newComplianceItemEntry,
    complianceItemEntry_title,
    complianceItemEntry_id,
    complianceItemEntry_details,
    complianceItemEntry_severity,
    complianceItemEntry_status,

    -- * ComplianceStringFilter
    ComplianceStringFilter (..),
    newComplianceStringFilter,
    complianceStringFilter_key,
    complianceStringFilter_values,
    complianceStringFilter_type,

    -- * ComplianceSummaryItem
    ComplianceSummaryItem (..),
    newComplianceSummaryItem,
    complianceSummaryItem_compliantSummary,
    complianceSummaryItem_complianceType,
    complianceSummaryItem_nonCompliantSummary,

    -- * CompliantSummary
    CompliantSummary (..),
    newCompliantSummary,
    compliantSummary_severitySummary,
    compliantSummary_compliantCount,

    -- * CreateAssociationBatchRequestEntry
    CreateAssociationBatchRequestEntry (..),
    newCreateAssociationBatchRequestEntry,
    createAssociationBatchRequestEntry_maxErrors,
    createAssociationBatchRequestEntry_instanceId,
    createAssociationBatchRequestEntry_complianceSeverity,
    createAssociationBatchRequestEntry_automationTargetParameterName,
    createAssociationBatchRequestEntry_targets,
    createAssociationBatchRequestEntry_scheduleExpression,
    createAssociationBatchRequestEntry_targetLocations,
    createAssociationBatchRequestEntry_maxConcurrency,
    createAssociationBatchRequestEntry_calendarNames,
    createAssociationBatchRequestEntry_associationName,
    createAssociationBatchRequestEntry_documentVersion,
    createAssociationBatchRequestEntry_parameters,
    createAssociationBatchRequestEntry_syncCompliance,
    createAssociationBatchRequestEntry_outputLocation,
    createAssociationBatchRequestEntry_applyOnlyAtCronInterval,
    createAssociationBatchRequestEntry_name,

    -- * DescribeActivationsFilter
    DescribeActivationsFilter (..),
    newDescribeActivationsFilter,
    describeActivationsFilter_filterKey,
    describeActivationsFilter_filterValues,

    -- * DocumentDefaultVersionDescription
    DocumentDefaultVersionDescription (..),
    newDocumentDefaultVersionDescription,
    documentDefaultVersionDescription_defaultVersion,
    documentDefaultVersionDescription_name,
    documentDefaultVersionDescription_defaultVersionName,

    -- * DocumentDescription
    DocumentDescription (..),
    newDocumentDescription,
    documentDescription_documentType,
    documentDescription_platformTypes,
    documentDescription_createdDate,
    documentDescription_status,
    documentDescription_approvedVersion,
    documentDescription_defaultVersion,
    documentDescription_latestVersion,
    documentDescription_targetType,
    documentDescription_requires,
    documentDescription_sha1,
    documentDescription_statusInformation,
    documentDescription_author,
    documentDescription_versionName,
    documentDescription_hash,
    documentDescription_name,
    documentDescription_pendingReviewVersion,
    documentDescription_documentFormat,
    documentDescription_tags,
    documentDescription_owner,
    documentDescription_attachmentsInformation,
    documentDescription_reviewInformation,
    documentDescription_description,
    documentDescription_reviewStatus,
    documentDescription_schemaVersion,
    documentDescription_documentVersion,
    documentDescription_displayName,
    documentDescription_hashType,
    documentDescription_parameters,

    -- * DocumentFilter
    DocumentFilter (..),
    newDocumentFilter,
    documentFilter_key,
    documentFilter_value,

    -- * DocumentIdentifier
    DocumentIdentifier (..),
    newDocumentIdentifier,
    documentIdentifier_documentType,
    documentIdentifier_platformTypes,
    documentIdentifier_createdDate,
    documentIdentifier_targetType,
    documentIdentifier_requires,
    documentIdentifier_author,
    documentIdentifier_versionName,
    documentIdentifier_name,
    documentIdentifier_documentFormat,
    documentIdentifier_tags,
    documentIdentifier_owner,
    documentIdentifier_reviewStatus,
    documentIdentifier_schemaVersion,
    documentIdentifier_documentVersion,
    documentIdentifier_displayName,

    -- * DocumentKeyValuesFilter
    DocumentKeyValuesFilter (..),
    newDocumentKeyValuesFilter,
    documentKeyValuesFilter_key,
    documentKeyValuesFilter_values,

    -- * DocumentMetadataResponseInfo
    DocumentMetadataResponseInfo (..),
    newDocumentMetadataResponseInfo,
    documentMetadataResponseInfo_reviewerResponse,

    -- * DocumentParameter
    DocumentParameter (..),
    newDocumentParameter,
    documentParameter_name,
    documentParameter_description,
    documentParameter_defaultValue,
    documentParameter_type,

    -- * DocumentRequires
    DocumentRequires (..),
    newDocumentRequires,
    documentRequires_version,
    documentRequires_name,

    -- * DocumentReviewCommentSource
    DocumentReviewCommentSource (..),
    newDocumentReviewCommentSource,
    documentReviewCommentSource_content,
    documentReviewCommentSource_type,

    -- * DocumentReviewerResponseSource
    DocumentReviewerResponseSource (..),
    newDocumentReviewerResponseSource,
    documentReviewerResponseSource_comment,
    documentReviewerResponseSource_updatedTime,
    documentReviewerResponseSource_createTime,
    documentReviewerResponseSource_reviewStatus,
    documentReviewerResponseSource_reviewer,

    -- * DocumentReviews
    DocumentReviews (..),
    newDocumentReviews,
    documentReviews_comment,
    documentReviews_action,

    -- * DocumentVersionInfo
    DocumentVersionInfo (..),
    newDocumentVersionInfo,
    documentVersionInfo_createdDate,
    documentVersionInfo_status,
    documentVersionInfo_statusInformation,
    documentVersionInfo_versionName,
    documentVersionInfo_name,
    documentVersionInfo_documentFormat,
    documentVersionInfo_reviewStatus,
    documentVersionInfo_isDefaultVersion,
    documentVersionInfo_documentVersion,
    documentVersionInfo_displayName,

    -- * EffectivePatch
    EffectivePatch (..),
    newEffectivePatch,
    effectivePatch_patch,
    effectivePatch_patchStatus,

    -- * FailedCreateAssociation
    FailedCreateAssociation (..),
    newFailedCreateAssociation,
    failedCreateAssociation_entry,
    failedCreateAssociation_message,
    failedCreateAssociation_fault,

    -- * FailureDetails
    FailureDetails (..),
    newFailureDetails,
    failureDetails_details,
    failureDetails_failureStage,
    failureDetails_failureType,

    -- * InstanceAggregatedAssociationOverview
    InstanceAggregatedAssociationOverview (..),
    newInstanceAggregatedAssociationOverview,
    instanceAggregatedAssociationOverview_detailedStatus,
    instanceAggregatedAssociationOverview_instanceAssociationStatusAggregatedCount,

    -- * InstanceAssociation
    InstanceAssociation (..),
    newInstanceAssociation,
    instanceAssociation_instanceId,
    instanceAssociation_associationId,
    instanceAssociation_content,
    instanceAssociation_associationVersion,

    -- * InstanceAssociationOutputLocation
    InstanceAssociationOutputLocation (..),
    newInstanceAssociationOutputLocation,
    instanceAssociationOutputLocation_s3Location,

    -- * InstanceAssociationOutputUrl
    InstanceAssociationOutputUrl (..),
    newInstanceAssociationOutputUrl,
    instanceAssociationOutputUrl_s3OutputUrl,

    -- * InstanceAssociationStatusInfo
    InstanceAssociationStatusInfo (..),
    newInstanceAssociationStatusInfo,
    instanceAssociationStatusInfo_status,
    instanceAssociationStatusInfo_instanceId,
    instanceAssociationStatusInfo_detailedStatus,
    instanceAssociationStatusInfo_name,
    instanceAssociationStatusInfo_associationId,
    instanceAssociationStatusInfo_outputUrl,
    instanceAssociationStatusInfo_executionDate,
    instanceAssociationStatusInfo_executionSummary,
    instanceAssociationStatusInfo_associationName,
    instanceAssociationStatusInfo_associationVersion,
    instanceAssociationStatusInfo_errorCode,
    instanceAssociationStatusInfo_documentVersion,

    -- * InstanceInformation
    InstanceInformation (..),
    newInstanceInformation,
    instanceInformation_instanceId,
    instanceInformation_pingStatus,
    instanceInformation_iamRole,
    instanceInformation_activationId,
    instanceInformation_lastSuccessfulAssociationExecutionDate,
    instanceInformation_agentVersion,
    instanceInformation_lastPingDateTime,
    instanceInformation_platformVersion,
    instanceInformation_lastAssociationExecutionDate,
    instanceInformation_resourceType,
    instanceInformation_associationOverview,
    instanceInformation_name,
    instanceInformation_iPAddress,
    instanceInformation_platformType,
    instanceInformation_isLatestVersion,
    instanceInformation_platformName,
    instanceInformation_computerName,
    instanceInformation_associationStatus,
    instanceInformation_registrationDate,

    -- * InstanceInformationFilter
    InstanceInformationFilter (..),
    newInstanceInformationFilter,
    instanceInformationFilter_key,
    instanceInformationFilter_valueSet,

    -- * InstanceInformationStringFilter
    InstanceInformationStringFilter (..),
    newInstanceInformationStringFilter,
    instanceInformationStringFilter_key,
    instanceInformationStringFilter_values,

    -- * InstancePatchState
    InstancePatchState (..),
    newInstancePatchState,
    instancePatchState_otherNonCompliantCount,
    instancePatchState_unreportedNotApplicableCount,
    instancePatchState_installedOtherCount,
    instancePatchState_installOverrideList,
    instancePatchState_criticalNonCompliantCount,
    instancePatchState_installedPendingRebootCount,
    instancePatchState_securityNonCompliantCount,
    instancePatchState_rebootOption,
    instancePatchState_missingCount,
    instancePatchState_snapshotId,
    instancePatchState_lastNoRebootInstallOperationTime,
    instancePatchState_installedCount,
    instancePatchState_notApplicableCount,
    instancePatchState_failedCount,
    instancePatchState_installedRejectedCount,
    instancePatchState_ownerInformation,
    instancePatchState_instanceId,
    instancePatchState_patchGroup,
    instancePatchState_baselineId,
    instancePatchState_operationStartTime,
    instancePatchState_operationEndTime,
    instancePatchState_operation,

    -- * InstancePatchStateFilter
    InstancePatchStateFilter (..),
    newInstancePatchStateFilter,
    instancePatchStateFilter_key,
    instancePatchStateFilter_values,
    instancePatchStateFilter_type,

    -- * InventoryAggregator
    InventoryAggregator (..),
    newInventoryAggregator,
    inventoryAggregator_groups,
    inventoryAggregator_aggregators,
    inventoryAggregator_expression,

    -- * InventoryDeletionStatusItem
    InventoryDeletionStatusItem (..),
    newInventoryDeletionStatusItem,
    inventoryDeletionStatusItem_typeName,
    inventoryDeletionStatusItem_lastStatusMessage,
    inventoryDeletionStatusItem_lastStatusUpdateTime,
    inventoryDeletionStatusItem_deletionStartTime,
    inventoryDeletionStatusItem_deletionId,
    inventoryDeletionStatusItem_lastStatus,
    inventoryDeletionStatusItem_deletionSummary,

    -- * InventoryDeletionSummary
    InventoryDeletionSummary (..),
    newInventoryDeletionSummary,
    inventoryDeletionSummary_remainingCount,
    inventoryDeletionSummary_totalCount,
    inventoryDeletionSummary_summaryItems,

    -- * InventoryDeletionSummaryItem
    InventoryDeletionSummaryItem (..),
    newInventoryDeletionSummaryItem,
    inventoryDeletionSummaryItem_remainingCount,
    inventoryDeletionSummaryItem_version,
    inventoryDeletionSummaryItem_count,

    -- * InventoryFilter
    InventoryFilter (..),
    newInventoryFilter,
    inventoryFilter_type,
    inventoryFilter_key,
    inventoryFilter_values,

    -- * InventoryGroup
    InventoryGroup (..),
    newInventoryGroup,
    inventoryGroup_name,
    inventoryGroup_filters,

    -- * InventoryItem
    InventoryItem (..),
    newInventoryItem,
    inventoryItem_context,
    inventoryItem_content,
    inventoryItem_contentHash,
    inventoryItem_typeName,
    inventoryItem_schemaVersion,
    inventoryItem_captureTime,

    -- * InventoryItemAttribute
    InventoryItemAttribute (..),
    newInventoryItemAttribute,
    inventoryItemAttribute_name,
    inventoryItemAttribute_dataType,

    -- * InventoryItemSchema
    InventoryItemSchema (..),
    newInventoryItemSchema,
    inventoryItemSchema_version,
    inventoryItemSchema_displayName,
    inventoryItemSchema_typeName,
    inventoryItemSchema_attributes,

    -- * InventoryResultEntity
    InventoryResultEntity (..),
    newInventoryResultEntity,
    inventoryResultEntity_data,
    inventoryResultEntity_id,

    -- * InventoryResultItem
    InventoryResultItem (..),
    newInventoryResultItem,
    inventoryResultItem_captureTime,
    inventoryResultItem_contentHash,
    inventoryResultItem_typeName,
    inventoryResultItem_schemaVersion,
    inventoryResultItem_content,

    -- * LoggingInfo
    LoggingInfo (..),
    newLoggingInfo,
    loggingInfo_s3KeyPrefix,
    loggingInfo_s3BucketName,
    loggingInfo_s3Region,

    -- * MaintenanceWindowAutomationParameters
    MaintenanceWindowAutomationParameters (..),
    newMaintenanceWindowAutomationParameters,
    maintenanceWindowAutomationParameters_documentVersion,
    maintenanceWindowAutomationParameters_parameters,

    -- * MaintenanceWindowExecution
    MaintenanceWindowExecution (..),
    newMaintenanceWindowExecution,
    maintenanceWindowExecution_status,
    maintenanceWindowExecution_statusDetails,
    maintenanceWindowExecution_startTime,
    maintenanceWindowExecution_endTime,
    maintenanceWindowExecution_windowId,
    maintenanceWindowExecution_windowExecutionId,

    -- * MaintenanceWindowExecutionTaskIdentity
    MaintenanceWindowExecutionTaskIdentity (..),
    newMaintenanceWindowExecutionTaskIdentity,
    maintenanceWindowExecutionTaskIdentity_status,
    maintenanceWindowExecutionTaskIdentity_statusDetails,
    maintenanceWindowExecutionTaskIdentity_startTime,
    maintenanceWindowExecutionTaskIdentity_endTime,
    maintenanceWindowExecutionTaskIdentity_windowExecutionId,
    maintenanceWindowExecutionTaskIdentity_taskArn,
    maintenanceWindowExecutionTaskIdentity_taskType,
    maintenanceWindowExecutionTaskIdentity_taskExecutionId,

    -- * MaintenanceWindowExecutionTaskInvocationIdentity
    MaintenanceWindowExecutionTaskInvocationIdentity (..),
    newMaintenanceWindowExecutionTaskInvocationIdentity,
    maintenanceWindowExecutionTaskInvocationIdentity_status,
    maintenanceWindowExecutionTaskInvocationIdentity_statusDetails,
    maintenanceWindowExecutionTaskInvocationIdentity_windowTargetId,
    maintenanceWindowExecutionTaskInvocationIdentity_startTime,
    maintenanceWindowExecutionTaskInvocationIdentity_endTime,
    maintenanceWindowExecutionTaskInvocationIdentity_executionId,
    maintenanceWindowExecutionTaskInvocationIdentity_windowExecutionId,
    maintenanceWindowExecutionTaskInvocationIdentity_invocationId,
    maintenanceWindowExecutionTaskInvocationIdentity_ownerInformation,
    maintenanceWindowExecutionTaskInvocationIdentity_taskType,
    maintenanceWindowExecutionTaskInvocationIdentity_parameters,
    maintenanceWindowExecutionTaskInvocationIdentity_taskExecutionId,

    -- * MaintenanceWindowFilter
    MaintenanceWindowFilter (..),
    newMaintenanceWindowFilter,
    maintenanceWindowFilter_key,
    maintenanceWindowFilter_values,

    -- * MaintenanceWindowIdentity
    MaintenanceWindowIdentity (..),
    newMaintenanceWindowIdentity,
    maintenanceWindowIdentity_startDate,
    maintenanceWindowIdentity_duration,
    maintenanceWindowIdentity_scheduleOffset,
    maintenanceWindowIdentity_enabled,
    maintenanceWindowIdentity_cutoff,
    maintenanceWindowIdentity_name,
    maintenanceWindowIdentity_windowId,
    maintenanceWindowIdentity_scheduleTimezone,
    maintenanceWindowIdentity_description,
    maintenanceWindowIdentity_endDate,
    maintenanceWindowIdentity_nextExecutionTime,
    maintenanceWindowIdentity_schedule,

    -- * MaintenanceWindowIdentityForTarget
    MaintenanceWindowIdentityForTarget (..),
    newMaintenanceWindowIdentityForTarget,
    maintenanceWindowIdentityForTarget_name,
    maintenanceWindowIdentityForTarget_windowId,

    -- * MaintenanceWindowLambdaParameters
    MaintenanceWindowLambdaParameters (..),
    newMaintenanceWindowLambdaParameters,
    maintenanceWindowLambdaParameters_payload,
    maintenanceWindowLambdaParameters_qualifier,
    maintenanceWindowLambdaParameters_clientContext,

    -- * MaintenanceWindowRunCommandParameters
    MaintenanceWindowRunCommandParameters (..),
    newMaintenanceWindowRunCommandParameters,
    maintenanceWindowRunCommandParameters_notificationConfig,
    maintenanceWindowRunCommandParameters_serviceRoleArn,
    maintenanceWindowRunCommandParameters_outputS3BucketName,
    maintenanceWindowRunCommandParameters_comment,
    maintenanceWindowRunCommandParameters_documentHash,
    maintenanceWindowRunCommandParameters_timeoutSeconds,
    maintenanceWindowRunCommandParameters_outputS3KeyPrefix,
    maintenanceWindowRunCommandParameters_cloudWatchOutputConfig,
    maintenanceWindowRunCommandParameters_documentVersion,
    maintenanceWindowRunCommandParameters_documentHashType,
    maintenanceWindowRunCommandParameters_parameters,

    -- * MaintenanceWindowStepFunctionsParameters
    MaintenanceWindowStepFunctionsParameters (..),
    newMaintenanceWindowStepFunctionsParameters,
    maintenanceWindowStepFunctionsParameters_input,
    maintenanceWindowStepFunctionsParameters_name,

    -- * MaintenanceWindowTarget
    MaintenanceWindowTarget (..),
    newMaintenanceWindowTarget,
    maintenanceWindowTarget_windowTargetId,
    maintenanceWindowTarget_resourceType,
    maintenanceWindowTarget_targets,
    maintenanceWindowTarget_name,
    maintenanceWindowTarget_windowId,
    maintenanceWindowTarget_description,
    maintenanceWindowTarget_ownerInformation,

    -- * MaintenanceWindowTask
    MaintenanceWindowTask (..),
    newMaintenanceWindowTask,
    maintenanceWindowTask_maxErrors,
    maintenanceWindowTask_taskParameters,
    maintenanceWindowTask_serviceRoleArn,
    maintenanceWindowTask_windowTaskId,
    maintenanceWindowTask_cutoffBehavior,
    maintenanceWindowTask_targets,
    maintenanceWindowTask_priority,
    maintenanceWindowTask_name,
    maintenanceWindowTask_windowId,
    maintenanceWindowTask_maxConcurrency,
    maintenanceWindowTask_description,
    maintenanceWindowTask_taskArn,
    maintenanceWindowTask_type,
    maintenanceWindowTask_loggingInfo,

    -- * MaintenanceWindowTaskInvocationParameters
    MaintenanceWindowTaskInvocationParameters (..),
    newMaintenanceWindowTaskInvocationParameters,
    maintenanceWindowTaskInvocationParameters_automation,
    maintenanceWindowTaskInvocationParameters_lambda,
    maintenanceWindowTaskInvocationParameters_runCommand,
    maintenanceWindowTaskInvocationParameters_stepFunctions,

    -- * MaintenanceWindowTaskParameterValueExpression
    MaintenanceWindowTaskParameterValueExpression (..),
    newMaintenanceWindowTaskParameterValueExpression,
    maintenanceWindowTaskParameterValueExpression_values,

    -- * MetadataValue
    MetadataValue (..),
    newMetadataValue,
    metadataValue_value,

    -- * NonCompliantSummary
    NonCompliantSummary (..),
    newNonCompliantSummary,
    nonCompliantSummary_severitySummary,
    nonCompliantSummary_nonCompliantCount,

    -- * NotificationConfig
    NotificationConfig (..),
    newNotificationConfig,
    notificationConfig_notificationArn,
    notificationConfig_notificationType,
    notificationConfig_notificationEvents,

    -- * OpsAggregator
    OpsAggregator (..),
    newOpsAggregator,
    opsAggregator_typeName,
    opsAggregator_attributeName,
    opsAggregator_values,
    opsAggregator_aggregatorType,
    opsAggregator_filters,
    opsAggregator_aggregators,

    -- * OpsEntity
    OpsEntity (..),
    newOpsEntity,
    opsEntity_data,
    opsEntity_id,

    -- * OpsEntityItem
    OpsEntityItem (..),
    newOpsEntityItem,
    opsEntityItem_content,
    opsEntityItem_captureTime,

    -- * OpsFilter
    OpsFilter (..),
    newOpsFilter,
    opsFilter_type,
    opsFilter_key,
    opsFilter_values,

    -- * OpsItem
    OpsItem (..),
    newOpsItem,
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
    opsItem_plannedStartTime,
    opsItem_opsItemType,
    opsItem_notifications,
    opsItem_lastModifiedTime,
    opsItem_description,
    opsItem_createdBy,
    opsItem_lastModifiedBy,
    opsItem_relatedOpsItems,

    -- * OpsItemDataValue
    OpsItemDataValue (..),
    newOpsItemDataValue,
    opsItemDataValue_value,
    opsItemDataValue_type,

    -- * OpsItemEventFilter
    OpsItemEventFilter (..),
    newOpsItemEventFilter,
    opsItemEventFilter_key,
    opsItemEventFilter_values,
    opsItemEventFilter_operator,

    -- * OpsItemEventSummary
    OpsItemEventSummary (..),
    newOpsItemEventSummary,
    opsItemEventSummary_detailType,
    opsItemEventSummary_eventId,
    opsItemEventSummary_source,
    opsItemEventSummary_createdTime,
    opsItemEventSummary_opsItemId,
    opsItemEventSummary_detail,
    opsItemEventSummary_createdBy,

    -- * OpsItemFilter
    OpsItemFilter (..),
    newOpsItemFilter,
    opsItemFilter_key,
    opsItemFilter_values,
    opsItemFilter_operator,

    -- * OpsItemIdentity
    OpsItemIdentity (..),
    newOpsItemIdentity,
    opsItemIdentity_arn,

    -- * OpsItemNotification
    OpsItemNotification (..),
    newOpsItemNotification,
    opsItemNotification_arn,

    -- * OpsItemRelatedItemSummary
    OpsItemRelatedItemSummary (..),
    newOpsItemRelatedItemSummary,
    opsItemRelatedItemSummary_createdTime,
    opsItemRelatedItemSummary_resourceType,
    opsItemRelatedItemSummary_opsItemId,
    opsItemRelatedItemSummary_associationId,
    opsItemRelatedItemSummary_resourceUri,
    opsItemRelatedItemSummary_associationType,
    opsItemRelatedItemSummary_lastModifiedTime,
    opsItemRelatedItemSummary_createdBy,
    opsItemRelatedItemSummary_lastModifiedBy,

    -- * OpsItemRelatedItemsFilter
    OpsItemRelatedItemsFilter (..),
    newOpsItemRelatedItemsFilter,
    opsItemRelatedItemsFilter_key,
    opsItemRelatedItemsFilter_values,
    opsItemRelatedItemsFilter_operator,

    -- * OpsItemSummary
    OpsItemSummary (..),
    newOpsItemSummary,
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
    opsItemSummary_plannedStartTime,
    opsItemSummary_opsItemType,
    opsItemSummary_lastModifiedTime,
    opsItemSummary_createdBy,
    opsItemSummary_lastModifiedBy,

    -- * OpsMetadata
    OpsMetadata (..),
    newOpsMetadata,
    opsMetadata_lastModifiedDate,
    opsMetadata_resourceId,
    opsMetadata_opsMetadataArn,
    opsMetadata_creationDate,
    opsMetadata_lastModifiedUser,

    -- * OpsMetadataFilter
    OpsMetadataFilter (..),
    newOpsMetadataFilter,
    opsMetadataFilter_key,
    opsMetadataFilter_values,

    -- * OpsResultAttribute
    OpsResultAttribute (..),
    newOpsResultAttribute,
    opsResultAttribute_typeName,

    -- * OutputSource
    OutputSource (..),
    newOutputSource,
    outputSource_outputSourceId,
    outputSource_outputSourceType,

    -- * Parameter
    Parameter (..),
    newParameter,
    parameter_lastModifiedDate,
    parameter_arn,
    parameter_version,
    parameter_name,
    parameter_sourceResult,
    parameter_value,
    parameter_dataType,
    parameter_type,
    parameter_selector,

    -- * ParameterHistory
    ParameterHistory (..),
    newParameterHistory,
    parameterHistory_lastModifiedDate,
    parameterHistory_policies,
    parameterHistory_labels,
    parameterHistory_version,
    parameterHistory_name,
    parameterHistory_description,
    parameterHistory_value,
    parameterHistory_dataType,
    parameterHistory_allowedPattern,
    parameterHistory_type,
    parameterHistory_lastModifiedUser,
    parameterHistory_tier,
    parameterHistory_keyId,

    -- * ParameterInlinePolicy
    ParameterInlinePolicy (..),
    newParameterInlinePolicy,
    parameterInlinePolicy_policyType,
    parameterInlinePolicy_policyText,
    parameterInlinePolicy_policyStatus,

    -- * ParameterMetadata
    ParameterMetadata (..),
    newParameterMetadata,
    parameterMetadata_lastModifiedDate,
    parameterMetadata_policies,
    parameterMetadata_version,
    parameterMetadata_name,
    parameterMetadata_description,
    parameterMetadata_dataType,
    parameterMetadata_allowedPattern,
    parameterMetadata_type,
    parameterMetadata_lastModifiedUser,
    parameterMetadata_tier,
    parameterMetadata_keyId,

    -- * ParameterStringFilter
    ParameterStringFilter (..),
    newParameterStringFilter,
    parameterStringFilter_values,
    parameterStringFilter_option,
    parameterStringFilter_key,

    -- * ParametersFilter
    ParametersFilter (..),
    newParametersFilter,
    parametersFilter_key,
    parametersFilter_values,

    -- * Patch
    Patch (..),
    newPatch,
    patch_msrcSeverity,
    patch_vendor,
    patch_epoch,
    patch_product,
    patch_severity,
    patch_title,
    patch_id,
    patch_productFamily,
    patch_repository,
    patch_version,
    patch_name,
    patch_bugzillaIds,
    patch_msrcNumber,
    patch_release,
    patch_cVEIds,
    patch_description,
    patch_classification,
    patch_advisoryIds,
    patch_arch,
    patch_releaseDate,
    patch_language,
    patch_kbNumber,
    patch_contentUrl,

    -- * PatchBaselineIdentity
    PatchBaselineIdentity (..),
    newPatchBaselineIdentity,
    patchBaselineIdentity_baselineName,
    patchBaselineIdentity_baselineId,
    patchBaselineIdentity_defaultBaseline,
    patchBaselineIdentity_baselineDescription,
    patchBaselineIdentity_operatingSystem,

    -- * PatchComplianceData
    PatchComplianceData (..),
    newPatchComplianceData,
    patchComplianceData_cVEIds,
    patchComplianceData_title,
    patchComplianceData_kBId,
    patchComplianceData_classification,
    patchComplianceData_severity,
    patchComplianceData_state,
    patchComplianceData_installedTime,

    -- * PatchFilter
    PatchFilter (..),
    newPatchFilter,
    patchFilter_key,
    patchFilter_values,

    -- * PatchFilterGroup
    PatchFilterGroup (..),
    newPatchFilterGroup,
    patchFilterGroup_patchFilters,

    -- * PatchGroupPatchBaselineMapping
    PatchGroupPatchBaselineMapping (..),
    newPatchGroupPatchBaselineMapping,
    patchGroupPatchBaselineMapping_baselineIdentity,
    patchGroupPatchBaselineMapping_patchGroup,

    -- * PatchOrchestratorFilter
    PatchOrchestratorFilter (..),
    newPatchOrchestratorFilter,
    patchOrchestratorFilter_key,
    patchOrchestratorFilter_values,

    -- * PatchRule
    PatchRule (..),
    newPatchRule,
    patchRule_approveAfterDays,
    patchRule_approveUntilDate,
    patchRule_complianceLevel,
    patchRule_enableNonSecurity,
    patchRule_patchFilterGroup,

    -- * PatchRuleGroup
    PatchRuleGroup (..),
    newPatchRuleGroup,
    patchRuleGroup_patchRules,

    -- * PatchSource
    PatchSource (..),
    newPatchSource,
    patchSource_name,
    patchSource_products,
    patchSource_configuration,

    -- * PatchStatus
    PatchStatus (..),
    newPatchStatus,
    patchStatus_approvalDate,
    patchStatus_complianceLevel,
    patchStatus_deploymentStatus,

    -- * ProgressCounters
    ProgressCounters (..),
    newProgressCounters,
    progressCounters_cancelledSteps,
    progressCounters_timedOutSteps,
    progressCounters_totalSteps,
    progressCounters_failedSteps,
    progressCounters_successSteps,

    -- * RelatedOpsItem
    RelatedOpsItem (..),
    newRelatedOpsItem,
    relatedOpsItem_opsItemId,

    -- * ResolvedTargets
    ResolvedTargets (..),
    newResolvedTargets,
    resolvedTargets_parameterValues,
    resolvedTargets_truncated,

    -- * ResourceComplianceSummaryItem
    ResourceComplianceSummaryItem (..),
    newResourceComplianceSummaryItem,
    resourceComplianceSummaryItem_resourceId,
    resourceComplianceSummaryItem_status,
    resourceComplianceSummaryItem_overallSeverity,
    resourceComplianceSummaryItem_compliantSummary,
    resourceComplianceSummaryItem_complianceType,
    resourceComplianceSummaryItem_resourceType,
    resourceComplianceSummaryItem_nonCompliantSummary,
    resourceComplianceSummaryItem_executionSummary,

    -- * ResourceDataSyncAwsOrganizationsSource
    ResourceDataSyncAwsOrganizationsSource (..),
    newResourceDataSyncAwsOrganizationsSource,
    resourceDataSyncAwsOrganizationsSource_organizationalUnits,
    resourceDataSyncAwsOrganizationsSource_organizationSourceType,

    -- * ResourceDataSyncDestinationDataSharing
    ResourceDataSyncDestinationDataSharing (..),
    newResourceDataSyncDestinationDataSharing,
    resourceDataSyncDestinationDataSharing_destinationDataSharingType,

    -- * ResourceDataSyncItem
    ResourceDataSyncItem (..),
    newResourceDataSyncItem,
    resourceDataSyncItem_syncType,
    resourceDataSyncItem_s3Destination,
    resourceDataSyncItem_syncLastModifiedTime,
    resourceDataSyncItem_syncName,
    resourceDataSyncItem_lastSyncTime,
    resourceDataSyncItem_syncSource,
    resourceDataSyncItem_lastSuccessfulSyncTime,
    resourceDataSyncItem_lastStatus,
    resourceDataSyncItem_lastSyncStatusMessage,
    resourceDataSyncItem_syncCreatedTime,

    -- * ResourceDataSyncOrganizationalUnit
    ResourceDataSyncOrganizationalUnit (..),
    newResourceDataSyncOrganizationalUnit,
    resourceDataSyncOrganizationalUnit_organizationalUnitId,

    -- * ResourceDataSyncS3Destination
    ResourceDataSyncS3Destination (..),
    newResourceDataSyncS3Destination,
    resourceDataSyncS3Destination_prefix,
    resourceDataSyncS3Destination_destinationDataSharing,
    resourceDataSyncS3Destination_aWSKMSKeyARN,
    resourceDataSyncS3Destination_bucketName,
    resourceDataSyncS3Destination_syncFormat,
    resourceDataSyncS3Destination_region,

    -- * ResourceDataSyncSource
    ResourceDataSyncSource (..),
    newResourceDataSyncSource,
    resourceDataSyncSource_includeFutureRegions,
    resourceDataSyncSource_awsOrganizationsSource,
    resourceDataSyncSource_enableAllOpsDataSources,
    resourceDataSyncSource_sourceType,
    resourceDataSyncSource_sourceRegions,

    -- * ResourceDataSyncSourceWithState
    ResourceDataSyncSourceWithState (..),
    newResourceDataSyncSourceWithState,
    resourceDataSyncSourceWithState_includeFutureRegions,
    resourceDataSyncSourceWithState_state,
    resourceDataSyncSourceWithState_sourceRegions,
    resourceDataSyncSourceWithState_awsOrganizationsSource,
    resourceDataSyncSourceWithState_sourceType,
    resourceDataSyncSourceWithState_enableAllOpsDataSources,

    -- * ResultAttribute
    ResultAttribute (..),
    newResultAttribute,
    resultAttribute_typeName,

    -- * ReviewInformation
    ReviewInformation (..),
    newReviewInformation,
    reviewInformation_status,
    reviewInformation_reviewedTime,
    reviewInformation_reviewer,

    -- * Runbook
    Runbook (..),
    newRunbook,
    runbook_maxErrors,
    runbook_targets,
    runbook_targetLocations,
    runbook_targetParameterName,
    runbook_maxConcurrency,
    runbook_documentVersion,
    runbook_parameters,
    runbook_documentName,

    -- * S3OutputLocation
    S3OutputLocation (..),
    newS3OutputLocation,
    s3OutputLocation_outputS3BucketName,
    s3OutputLocation_outputS3Region,
    s3OutputLocation_outputS3KeyPrefix,

    -- * S3OutputUrl
    S3OutputUrl (..),
    newS3OutputUrl,
    s3OutputUrl_outputUrl,

    -- * ScheduledWindowExecution
    ScheduledWindowExecution (..),
    newScheduledWindowExecution,
    scheduledWindowExecution_executionTime,
    scheduledWindowExecution_name,
    scheduledWindowExecution_windowId,

    -- * ServiceSetting
    ServiceSetting (..),
    newServiceSetting,
    serviceSetting_lastModifiedDate,
    serviceSetting_status,
    serviceSetting_settingValue,
    serviceSetting_arn,
    serviceSetting_settingId,
    serviceSetting_lastModifiedUser,

    -- * Session
    Session (..),
    newSession,
    session_status,
    session_startDate,
    session_sessionId,
    session_documentName,
    session_details,
    session_outputUrl,
    session_owner,
    session_target,
    session_endDate,

    -- * SessionFilter
    SessionFilter (..),
    newSessionFilter,
    sessionFilter_key,
    sessionFilter_value,

    -- * SessionManagerOutputUrl
    SessionManagerOutputUrl (..),
    newSessionManagerOutputUrl,
    sessionManagerOutputUrl_s3OutputUrl,
    sessionManagerOutputUrl_cloudWatchOutputUrl,

    -- * SeveritySummary
    SeveritySummary (..),
    newSeveritySummary,
    severitySummary_lowCount,
    severitySummary_mediumCount,
    severitySummary_criticalCount,
    severitySummary_highCount,
    severitySummary_unspecifiedCount,
    severitySummary_informationalCount,

    -- * StepExecution
    StepExecution (..),
    newStepExecution,
    stepExecution_outputs,
    stepExecution_onFailure,
    stepExecution_failureMessage,
    stepExecution_response,
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
    stepExecution_action,
    stepExecution_responseCode,
    stepExecution_stepStatus,
    stepExecution_timeoutSeconds,
    stepExecution_validNextSteps,
    stepExecution_stepExecutionId,
    stepExecution_stepName,
    stepExecution_inputs,

    -- * StepExecutionFilter
    StepExecutionFilter (..),
    newStepExecutionFilter,
    stepExecutionFilter_key,
    stepExecutionFilter_values,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * Target
    Target (..),
    newTarget,
    target_key,
    target_values,

    -- * TargetLocation
    TargetLocation (..),
    newTargetLocation,
    targetLocation_executionRoleName,
    targetLocation_accounts,
    targetLocation_regions,
    targetLocation_targetLocationMaxErrors,
    targetLocation_targetLocationMaxConcurrency,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.AccountSharingInfo
import Network.AWS.SSM.Types.Activation
import Network.AWS.SSM.Types.Association
import Network.AWS.SSM.Types.AssociationComplianceSeverity
import Network.AWS.SSM.Types.AssociationDescription
import Network.AWS.SSM.Types.AssociationExecution
import Network.AWS.SSM.Types.AssociationExecutionFilter
import Network.AWS.SSM.Types.AssociationExecutionFilterKey
import Network.AWS.SSM.Types.AssociationExecutionTarget
import Network.AWS.SSM.Types.AssociationExecutionTargetsFilter
import Network.AWS.SSM.Types.AssociationExecutionTargetsFilterKey
import Network.AWS.SSM.Types.AssociationFilter
import Network.AWS.SSM.Types.AssociationFilterKey
import Network.AWS.SSM.Types.AssociationFilterOperatorType
import Network.AWS.SSM.Types.AssociationOverview
import Network.AWS.SSM.Types.AssociationStatus
import Network.AWS.SSM.Types.AssociationStatusName
import Network.AWS.SSM.Types.AssociationSyncCompliance
import Network.AWS.SSM.Types.AssociationVersionInfo
import Network.AWS.SSM.Types.AttachmentContent
import Network.AWS.SSM.Types.AttachmentHashType
import Network.AWS.SSM.Types.AttachmentInformation
import Network.AWS.SSM.Types.AttachmentsSource
import Network.AWS.SSM.Types.AttachmentsSourceKey
import Network.AWS.SSM.Types.AutomationExecution
import Network.AWS.SSM.Types.AutomationExecutionFilter
import Network.AWS.SSM.Types.AutomationExecutionFilterKey
import Network.AWS.SSM.Types.AutomationExecutionMetadata
import Network.AWS.SSM.Types.AutomationExecutionStatus
import Network.AWS.SSM.Types.AutomationSubtype
import Network.AWS.SSM.Types.AutomationType
import Network.AWS.SSM.Types.BaselineOverride
import Network.AWS.SSM.Types.CalendarState
import Network.AWS.SSM.Types.CloudWatchOutputConfig
import Network.AWS.SSM.Types.Command
import Network.AWS.SSM.Types.CommandFilter
import Network.AWS.SSM.Types.CommandFilterKey
import Network.AWS.SSM.Types.CommandInvocation
import Network.AWS.SSM.Types.CommandInvocationStatus
import Network.AWS.SSM.Types.CommandPlugin
import Network.AWS.SSM.Types.CommandPluginStatus
import Network.AWS.SSM.Types.CommandStatus
import Network.AWS.SSM.Types.ComplianceExecutionSummary
import Network.AWS.SSM.Types.ComplianceItem
import Network.AWS.SSM.Types.ComplianceItemEntry
import Network.AWS.SSM.Types.ComplianceQueryOperatorType
import Network.AWS.SSM.Types.ComplianceSeverity
import Network.AWS.SSM.Types.ComplianceStatus
import Network.AWS.SSM.Types.ComplianceStringFilter
import Network.AWS.SSM.Types.ComplianceSummaryItem
import Network.AWS.SSM.Types.ComplianceUploadType
import Network.AWS.SSM.Types.CompliantSummary
import Network.AWS.SSM.Types.ConnectionStatus
import Network.AWS.SSM.Types.CreateAssociationBatchRequestEntry
import Network.AWS.SSM.Types.DescribeActivationsFilter
import Network.AWS.SSM.Types.DescribeActivationsFilterKeys
import Network.AWS.SSM.Types.DocumentDefaultVersionDescription
import Network.AWS.SSM.Types.DocumentDescription
import Network.AWS.SSM.Types.DocumentFilter
import Network.AWS.SSM.Types.DocumentFilterKey
import Network.AWS.SSM.Types.DocumentFormat
import Network.AWS.SSM.Types.DocumentHashType
import Network.AWS.SSM.Types.DocumentIdentifier
import Network.AWS.SSM.Types.DocumentKeyValuesFilter
import Network.AWS.SSM.Types.DocumentMetadataEnum
import Network.AWS.SSM.Types.DocumentMetadataResponseInfo
import Network.AWS.SSM.Types.DocumentParameter
import Network.AWS.SSM.Types.DocumentParameterType
import Network.AWS.SSM.Types.DocumentPermissionType
import Network.AWS.SSM.Types.DocumentRequires
import Network.AWS.SSM.Types.DocumentReviewAction
import Network.AWS.SSM.Types.DocumentReviewCommentSource
import Network.AWS.SSM.Types.DocumentReviewCommentType
import Network.AWS.SSM.Types.DocumentReviewerResponseSource
import Network.AWS.SSM.Types.DocumentReviews
import Network.AWS.SSM.Types.DocumentStatus
import Network.AWS.SSM.Types.DocumentType
import Network.AWS.SSM.Types.DocumentVersionInfo
import Network.AWS.SSM.Types.EffectivePatch
import Network.AWS.SSM.Types.ExecutionMode
import Network.AWS.SSM.Types.FailedCreateAssociation
import Network.AWS.SSM.Types.FailureDetails
import Network.AWS.SSM.Types.Fault
import Network.AWS.SSM.Types.InstanceAggregatedAssociationOverview
import Network.AWS.SSM.Types.InstanceAssociation
import Network.AWS.SSM.Types.InstanceAssociationOutputLocation
import Network.AWS.SSM.Types.InstanceAssociationOutputUrl
import Network.AWS.SSM.Types.InstanceAssociationStatusInfo
import Network.AWS.SSM.Types.InstanceInformation
import Network.AWS.SSM.Types.InstanceInformationFilter
import Network.AWS.SSM.Types.InstanceInformationFilterKey
import Network.AWS.SSM.Types.InstanceInformationStringFilter
import Network.AWS.SSM.Types.InstancePatchState
import Network.AWS.SSM.Types.InstancePatchStateFilter
import Network.AWS.SSM.Types.InstancePatchStateOperatorType
import Network.AWS.SSM.Types.InventoryAggregator
import Network.AWS.SSM.Types.InventoryAttributeDataType
import Network.AWS.SSM.Types.InventoryDeletionStatus
import Network.AWS.SSM.Types.InventoryDeletionStatusItem
import Network.AWS.SSM.Types.InventoryDeletionSummary
import Network.AWS.SSM.Types.InventoryDeletionSummaryItem
import Network.AWS.SSM.Types.InventoryFilter
import Network.AWS.SSM.Types.InventoryGroup
import Network.AWS.SSM.Types.InventoryItem
import Network.AWS.SSM.Types.InventoryItemAttribute
import Network.AWS.SSM.Types.InventoryItemSchema
import Network.AWS.SSM.Types.InventoryQueryOperatorType
import Network.AWS.SSM.Types.InventoryResultEntity
import Network.AWS.SSM.Types.InventoryResultItem
import Network.AWS.SSM.Types.InventorySchemaDeleteOption
import Network.AWS.SSM.Types.LastResourceDataSyncStatus
import Network.AWS.SSM.Types.LoggingInfo
import Network.AWS.SSM.Types.MaintenanceWindowAutomationParameters
import Network.AWS.SSM.Types.MaintenanceWindowExecution
import Network.AWS.SSM.Types.MaintenanceWindowExecutionStatus
import Network.AWS.SSM.Types.MaintenanceWindowExecutionTaskIdentity
import Network.AWS.SSM.Types.MaintenanceWindowExecutionTaskInvocationIdentity
import Network.AWS.SSM.Types.MaintenanceWindowFilter
import Network.AWS.SSM.Types.MaintenanceWindowIdentity
import Network.AWS.SSM.Types.MaintenanceWindowIdentityForTarget
import Network.AWS.SSM.Types.MaintenanceWindowLambdaParameters
import Network.AWS.SSM.Types.MaintenanceWindowResourceType
import Network.AWS.SSM.Types.MaintenanceWindowRunCommandParameters
import Network.AWS.SSM.Types.MaintenanceWindowStepFunctionsParameters
import Network.AWS.SSM.Types.MaintenanceWindowTarget
import Network.AWS.SSM.Types.MaintenanceWindowTask
import Network.AWS.SSM.Types.MaintenanceWindowTaskCutoffBehavior
import Network.AWS.SSM.Types.MaintenanceWindowTaskInvocationParameters
import Network.AWS.SSM.Types.MaintenanceWindowTaskParameterValueExpression
import Network.AWS.SSM.Types.MaintenanceWindowTaskType
import Network.AWS.SSM.Types.MetadataValue
import Network.AWS.SSM.Types.NonCompliantSummary
import Network.AWS.SSM.Types.NotificationConfig
import Network.AWS.SSM.Types.NotificationEvent
import Network.AWS.SSM.Types.NotificationType
import Network.AWS.SSM.Types.OperatingSystem
import Network.AWS.SSM.Types.OpsAggregator
import Network.AWS.SSM.Types.OpsEntity
import Network.AWS.SSM.Types.OpsEntityItem
import Network.AWS.SSM.Types.OpsFilter
import Network.AWS.SSM.Types.OpsFilterOperatorType
import Network.AWS.SSM.Types.OpsItem
import Network.AWS.SSM.Types.OpsItemDataType
import Network.AWS.SSM.Types.OpsItemDataValue
import Network.AWS.SSM.Types.OpsItemEventFilter
import Network.AWS.SSM.Types.OpsItemEventFilterKey
import Network.AWS.SSM.Types.OpsItemEventFilterOperator
import Network.AWS.SSM.Types.OpsItemEventSummary
import Network.AWS.SSM.Types.OpsItemFilter
import Network.AWS.SSM.Types.OpsItemFilterKey
import Network.AWS.SSM.Types.OpsItemFilterOperator
import Network.AWS.SSM.Types.OpsItemIdentity
import Network.AWS.SSM.Types.OpsItemNotification
import Network.AWS.SSM.Types.OpsItemRelatedItemSummary
import Network.AWS.SSM.Types.OpsItemRelatedItemsFilter
import Network.AWS.SSM.Types.OpsItemRelatedItemsFilterKey
import Network.AWS.SSM.Types.OpsItemRelatedItemsFilterOperator
import Network.AWS.SSM.Types.OpsItemStatus
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
import Network.AWS.SSM.Types.ParameterTier
import Network.AWS.SSM.Types.ParameterType
import Network.AWS.SSM.Types.ParametersFilter
import Network.AWS.SSM.Types.ParametersFilterKey
import Network.AWS.SSM.Types.Patch
import Network.AWS.SSM.Types.PatchAction
import Network.AWS.SSM.Types.PatchBaselineIdentity
import Network.AWS.SSM.Types.PatchComplianceData
import Network.AWS.SSM.Types.PatchComplianceDataState
import Network.AWS.SSM.Types.PatchComplianceLevel
import Network.AWS.SSM.Types.PatchDeploymentStatus
import Network.AWS.SSM.Types.PatchFilter
import Network.AWS.SSM.Types.PatchFilterGroup
import Network.AWS.SSM.Types.PatchFilterKey
import Network.AWS.SSM.Types.PatchGroupPatchBaselineMapping
import Network.AWS.SSM.Types.PatchOperationType
import Network.AWS.SSM.Types.PatchOrchestratorFilter
import Network.AWS.SSM.Types.PatchProperty
import Network.AWS.SSM.Types.PatchRule
import Network.AWS.SSM.Types.PatchRuleGroup
import Network.AWS.SSM.Types.PatchSet
import Network.AWS.SSM.Types.PatchSource
import Network.AWS.SSM.Types.PatchStatus
import Network.AWS.SSM.Types.PingStatus
import Network.AWS.SSM.Types.PlatformType
import Network.AWS.SSM.Types.ProgressCounters
import Network.AWS.SSM.Types.RebootOption
import Network.AWS.SSM.Types.RelatedOpsItem
import Network.AWS.SSM.Types.ResolvedTargets
import Network.AWS.SSM.Types.ResourceComplianceSummaryItem
import Network.AWS.SSM.Types.ResourceDataSyncAwsOrganizationsSource
import Network.AWS.SSM.Types.ResourceDataSyncDestinationDataSharing
import Network.AWS.SSM.Types.ResourceDataSyncItem
import Network.AWS.SSM.Types.ResourceDataSyncOrganizationalUnit
import Network.AWS.SSM.Types.ResourceDataSyncS3Destination
import Network.AWS.SSM.Types.ResourceDataSyncS3Format
import Network.AWS.SSM.Types.ResourceDataSyncSource
import Network.AWS.SSM.Types.ResourceDataSyncSourceWithState
import Network.AWS.SSM.Types.ResourceType
import Network.AWS.SSM.Types.ResourceTypeForTagging
import Network.AWS.SSM.Types.ResultAttribute
import Network.AWS.SSM.Types.ReviewInformation
import Network.AWS.SSM.Types.ReviewStatus
import Network.AWS.SSM.Types.Runbook
import Network.AWS.SSM.Types.S3OutputLocation
import Network.AWS.SSM.Types.S3OutputUrl
import Network.AWS.SSM.Types.ScheduledWindowExecution
import Network.AWS.SSM.Types.ServiceSetting
import Network.AWS.SSM.Types.Session
import Network.AWS.SSM.Types.SessionFilter
import Network.AWS.SSM.Types.SessionFilterKey
import Network.AWS.SSM.Types.SessionManagerOutputUrl
import Network.AWS.SSM.Types.SessionState
import Network.AWS.SSM.Types.SessionStatus
import Network.AWS.SSM.Types.SeveritySummary
import Network.AWS.SSM.Types.SignalType
import Network.AWS.SSM.Types.StepExecution
import Network.AWS.SSM.Types.StepExecutionFilter
import Network.AWS.SSM.Types.StepExecutionFilterKey
import Network.AWS.SSM.Types.StopType
import Network.AWS.SSM.Types.Tag
import Network.AWS.SSM.Types.Target
import Network.AWS.SSM.Types.TargetLocation
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-11-06@ of the Amazon Simple Systems Manager (SSM) SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "SSM",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "ssm",
      Core._serviceSigningName = "ssm",
      Core._serviceVersion = "2014-11-06",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "SSM",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The signal isn\'t valid for the current Automation execution.
_InvalidAutomationSignalException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAutomationSignalException =
  Core._MatchServiceError
    defaultService
    "InvalidAutomationSignalException"

-- | One or more of the parameters specified for the delete operation isn\'t
-- valid. Verify all parameters and try again.
_InvalidDeleteInventoryParametersException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeleteInventoryParametersException =
  Core._MatchServiceError
    defaultService
    "InvalidDeleteInventoryParametersException"

-- | The OpsMetadata object exceeds the maximum number of OpsMetadata keys
-- that you can assign to an application in Application Manager.
_OpsMetadataKeyLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsMetadataKeyLimitExceededException =
  Core._MatchServiceError
    defaultService
    "OpsMetadataKeyLimitExceededException"

-- | You attempted to register a @LAMBDA@ or @STEP_FUNCTIONS@ task in a
-- region where the corresponding service isn\'t available.
_FeatureNotAvailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FeatureNotAvailableException =
  Core._MatchServiceError
    defaultService
    "FeatureNotAvailableException"

-- | The specified filter value isn\'t valid.
_InvalidInstanceInformationFilterValue :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInstanceInformationFilterValue =
  Core._MatchServiceError
    defaultService
    "InvalidInstanceInformationFilterValue"

-- | The specified aggregator isn\'t valid for inventory groups. Verify that
-- the aggregator uses a valid inventory type such as @AWS:Application@ or
-- @AWS:InstanceInformation@.
_InvalidAggregatorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAggregatorException =
  Core._MatchServiceError
    defaultService
    "InvalidAggregatorException"

-- | You specified too many custom compliance types. You can specify a
-- maximum of 10 different types.
_ComplianceTypeCountLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ComplianceTypeCountLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ComplianceTypeCountLimitExceededException"

-- | An Automation runbook with the specified name and version couldn\'t be
-- found.
_AutomationDefinitionVersionNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AutomationDefinitionVersionNotFoundException =
  Core._MatchServiceError
    defaultService
    "AutomationDefinitionVersionNotFoundException"

-- | The parameter type isn\'t supported.
_UnsupportedParameterType :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedParameterType =
  Core._MatchServiceError
    defaultService
    "UnsupportedParameterType"

-- | The document version isn\'t valid or doesn\'t exist.
_InvalidDocumentVersion :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDocumentVersion =
  Core._MatchServiceError
    defaultService
    "InvalidDocumentVersion"

-- | The number of simultaneously running Automation executions exceeded the
-- allowable limit.
_AutomationExecutionLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AutomationExecutionLimitExceededException =
  Core._MatchServiceError
    defaultService
    "AutomationExecutionLimitExceededException"

-- | Error returned when an idempotent operation is retried and the
-- parameters don\'t match the original call to the API with the same
-- idempotency token.
_IdempotentParameterMismatch :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IdempotentParameterMismatch =
  Core._MatchServiceError
    defaultService
    "IdempotentParameterMismatch"

-- | One of the arguments passed is invalid.
_OpsMetadataInvalidArgumentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsMetadataInvalidArgumentException =
  Core._MatchServiceError
    defaultService
    "OpsMetadataInvalidArgumentException"

-- | Another @UpdateResourceDataSync@ request is being processed. Wait a few
-- minutes and try again.
_ResourceDataSyncConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceDataSyncConflictException =
  Core._MatchServiceError
    defaultService
    "ResourceDataSyncConflictException"

-- | The specified update status operation isn\'t valid.
_InvalidAutomationStatusUpdateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAutomationStatusUpdateException =
  Core._MatchServiceError
    defaultService
    "InvalidAutomationStatusUpdateException"

-- | Parameter Store doesn\'t support changing a parameter type in a
-- hierarchy. For example, you can\'t change a parameter from a @String@
-- type to a @SecureString@ type. You must create a new, unique parameter.
_HierarchyTypeMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_HierarchyTypeMismatchException =
  Core._MatchServiceError
    defaultService
    "HierarchyTypeMismatchException"

-- | Error returned when the ID specified for a resource, such as a
-- maintenance window or patch baseline, doesn\'t exist.
--
-- For information about resource quotas in Amazon Web Services Systems
-- Manager, see
-- <https://docs.aws.amazon.com/general/latest/gr/ssm.html#limits_ssm Systems Manager service quotas>
-- in the /Amazon Web Services General Reference/.
_DoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DoesNotExistException =
  Core._MatchServiceError
    defaultService
    "DoesNotExistException"

-- | The inventory item size has exceeded the size limit.
_ItemSizeLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ItemSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ItemSizeLimitExceededException"

-- | The specified document already exists.
_DocumentAlreadyExists :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DocumentAlreadyExists =
  Core._MatchServiceError
    defaultService
    "DocumentAlreadyExists"

-- | Parameter Store retains the 100 most recently created versions of a
-- parameter. After this number of versions has been created, Parameter
-- Store deletes the oldest version when a new one is created. However, if
-- the oldest version has a /label/ attached to it, Parameter Store won\'t
-- delete the version and instead presents this error message:
--
-- @An error occurred (ParameterMaxVersionLimitExceeded) when calling the PutParameter operation: You attempted to create a new version of parameter-name by calling the PutParameter API with the overwrite flag. Version version-number, the oldest version, can\'t be deleted because it has a label associated with it. Move the label to another version of the parameter, and try again.@
--
-- This safeguard is to prevent parameter versions with mission critical
-- labels assigned to them from being deleted. To continue creating new
-- parameters, first move the label from the oldest version of the
-- parameter to a newer one for use in your operations. For information
-- about moving parameter labels, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-labels.html#sysman-paramstore-labels-console-move Move a parameter label (console)>
-- or
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-labels.html#sysman-paramstore-labels-cli-move Move a parameter label (CLI)>
-- in the /Amazon Web Services Systems Manager User Guide/.
_ParameterMaxVersionLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterMaxVersionLimitExceeded =
  Core._MatchServiceError
    defaultService
    "ParameterMaxVersionLimitExceeded"

-- | You can have at most 500 active SSM documents.
_DocumentLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DocumentLimitExceeded =
  Core._MatchServiceError
    defaultService
    "DocumentLimitExceeded"

-- | The specified step name and execution ID don\'t exist. Verify the
-- information and try again.
_AutomationStepNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AutomationStepNotFoundException =
  Core._MatchServiceError
    defaultService
    "AutomationStepNotFoundException"

-- | There is no automation execution information for the requested
-- automation execution ID.
_AutomationExecutionNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AutomationExecutionNotFoundException =
  Core._MatchServiceError
    defaultService
    "AutomationExecutionNotFoundException"

-- | The document can\'t be shared with more Amazon Web Services user
-- accounts. You can share a document with a maximum of 20 accounts. You
-- can publicly share up to five documents. If you need to increase this
-- limit, contact Amazon Web Services Support.
_DocumentPermissionLimit :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DocumentPermissionLimit =
  Core._MatchServiceError
    defaultService
    "DocumentPermissionLimit"

-- | The filter name isn\'t valid. Verify the you entered the correct name
-- and try again.
_InvalidFilter :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFilter =
  Core._MatchServiceError
    defaultService
    "InvalidFilter"

-- | The parameter type name isn\'t valid.
_InvalidTypeNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTypeNameException =
  Core._MatchServiceError
    defaultService
    "InvalidTypeNameException"

-- | The sub-type count exceeded the limit for the inventory type.
_SubTypeCountLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubTypeCountLimitExceededException =
  Core._MatchServiceError
    defaultService
    "SubTypeCountLimitExceededException"

-- | An Automation runbook with the specified name couldn\'t be found.
_AutomationDefinitionNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AutomationDefinitionNotFoundException =
  Core._MatchServiceError
    defaultService
    "AutomationDefinitionNotFoundException"

-- | The association wasn\'t found using the parameters you specified in the
-- call. Verify the information and try again.
_OpsItemRelatedItemAssociationNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsItemRelatedItemAssociationNotFoundException =
  Core._MatchServiceError
    defaultService
    "OpsItemRelatedItemAssociationNotFoundException"

-- | You specified invalid keys or values in the @Context@ attribute for
-- @InventoryItem@. Verify the keys and values, and try again.
_InvalidInventoryItemContextException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInventoryItemContextException =
  Core._MatchServiceError
    defaultService
    "InvalidInventoryItemContextException"

-- | The specified SSM document doesn\'t exist.
_InvalidDocument :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDocument =
  Core._MatchServiceError
    defaultService
    "InvalidDocument"

-- | The @Targets@ parameter includes too many tags. Remove one or more tags
-- and try the command again.
_TooManyTagsError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsError =
  Core._MatchServiceError
    defaultService
    "TooManyTagsError"

-- | A specified parameter argument isn\'t valid. Verify the available
-- arguments and try again.
_OpsItemInvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsItemInvalidParameterException =
  Core._MatchServiceError
    defaultService
    "OpsItemInvalidParameterException"

-- | There is a conflict in the policies specified for this parameter. You
-- can\'t, for example, specify two Expiration policies for a parameter.
-- Review your policies, and try again.
_IncompatiblePolicyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IncompatiblePolicyException =
  Core._MatchServiceError
    defaultService
    "IncompatiblePolicyException"

-- | The request doesn\'t meet the regular expression requirement.
_InvalidAllowedPatternException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAllowedPatternException =
  Core._MatchServiceError
    defaultService
    "InvalidAllowedPatternException"

-- | The system is processing too many concurrent updates. Wait a few moments
-- and try again.
_OpsMetadataTooManyUpdatesException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsMetadataTooManyUpdatesException =
  Core._MatchServiceError
    defaultService
    "OpsMetadataTooManyUpdatesException"

-- | You must specify values for all required parameters in the Amazon Web
-- Services Systems Manager document (SSM document). You can only supply
-- values to parameters defined in the SSM document.
_InvalidParameters :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameters =
  Core._MatchServiceError
    defaultService
    "InvalidParameters"

-- | The specified target instance for the session isn\'t fully configured
-- for use with Session Manager. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/session-manager-getting-started.html Getting started with Session Manager>
-- in the /Amazon Web Services Systems Manager User Guide/. This error is
-- also returned if you attempt to start a session on an instance that is
-- located in a different account or Region
_TargetNotConnected :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TargetNotConnected =
  Core._MatchServiceError
    defaultService
    "TargetNotConnected"

-- | A parameter version can have a maximum of ten labels.
_ParameterVersionLabelLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterVersionLabelLimitExceeded =
  Core._MatchServiceError
    defaultService
    "ParameterVersionLabelLimitExceeded"

-- | Inventory item type schema version has to match supported versions in
-- the service. Check output of GetInventorySchema to see the available
-- schema version for each type.
_UnsupportedInventorySchemaVersionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedInventorySchemaVersionException =
  Core._MatchServiceError
    defaultService
    "UnsupportedInventorySchemaVersionException"

-- | The specified command ID isn\'t valid. Verify the ID and try again.
_InvalidCommandId :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCommandId =
  Core._MatchServiceError
    defaultService
    "InvalidCommandId"

-- | The output location isn\'t valid or doesn\'t exist.
_InvalidOutputLocation :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOutputLocation =
  Core._MatchServiceError
    defaultService
    "InvalidOutputLocation"

-- | Error returned when the caller has exceeded the default resource quotas.
-- For example, too many maintenance windows or patch baselines have been
-- created.
--
-- For information about resource quotas in Systems Manager, see
-- <https://docs.aws.amazon.com/general/latest/gr/ssm.html#limits_ssm Systems Manager service quotas>
-- in the /Amazon Web Services General Reference/.
_ResourceLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceLimitExceededException"

-- | The target isn\'t valid or doesn\'t exist. It might not be configured
-- for Systems Manager or you might not have permission to perform the
-- operation.
_InvalidTarget :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTarget =
  Core._MatchServiceError
    defaultService
    "InvalidTarget"

-- | The specified service setting wasn\'t found. Either the service name or
-- the setting hasn\'t been provisioned by the Amazon Web Services service
-- team.
_ServiceSettingNotFound :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceSettingNotFound =
  Core._MatchServiceError
    defaultService
    "ServiceSettingNotFound"

-- | You have exceeded the limit for custom schemas. Delete one or more
-- custom schemas and try again.
_CustomSchemaCountLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CustomSchemaCountLimitExceededException =
  Core._MatchServiceError
    defaultService
    "CustomSchemaCountLimitExceededException"

-- | The update isn\'t valid.
_InvalidUpdate :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidUpdate =
  Core._MatchServiceError
    defaultService
    "InvalidUpdate"

-- | The association isn\'t valid or doesn\'t exist.
_InvalidAssociation :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAssociation =
  Core._MatchServiceError
    defaultService
    "InvalidAssociation"

-- | The version of the document schema isn\'t supported.
_InvalidDocumentSchemaVersion :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDocumentSchemaVersion =
  Core._MatchServiceError
    defaultService
    "InvalidDocumentSchemaVersion"

-- | The delete inventory option specified isn\'t valid. Verify the option
-- and try again.
_InvalidOptionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOptionException =
  Core._MatchServiceError
    defaultService
    "InvalidOptionException"

-- | The specified OpsItem ID doesn\'t exist. Verify the ID and try again.
_OpsItemNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsItemNotFoundException =
  Core._MatchServiceError
    defaultService
    "OpsItemNotFoundException"

-- | The updated status is the same as the current status.
_StatusUnchanged :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StatusUnchanged =
  Core._MatchServiceError
    defaultService
    "StatusUnchanged"

-- | The OpsMetadata object doesn\'t exist.
_OpsMetadataNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsMetadataNotFoundException =
  Core._MatchServiceError
    defaultService
    "OpsMetadataNotFoundException"

-- | The following problems can cause this exception:
--
-- -   You don\'t have permission to access the instance.
--
-- -   Amazon Web Services Systems Manager Agent(SSM Agent) isn\'t running.
--     Verify that SSM Agent is running.
--
-- -   SSM Agent isn\'t registered with the SSM endpoint. Try reinstalling
--     SSM Agent.
--
-- -   The instance isn\'t in valid state. Valid states are: @Running@,
--     @Pending@, @Stopped@, and @Stopping@. Invalid states are:
--     @Shutting-down@ and @Terminated@.
_InvalidInstanceId :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInstanceId =
  Core._MatchServiceError
    defaultService
    "InvalidInstanceId"

-- | There are concurrent updates for a resource that supports one update at
-- a time.
_TooManyUpdates :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyUpdates =
  Core._MatchServiceError
    defaultService
    "TooManyUpdates"

-- | The version name has already been used in this document. Specify a
-- different version name, and then try again.
_DuplicateDocumentVersionName :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateDocumentVersionName =
  Core._MatchServiceError
    defaultService
    "DuplicateDocumentVersionName"

-- | The Amazon Resource Name (ARN) is already associated with the OpsItem.
_OpsItemRelatedItemAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsItemRelatedItemAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "OpsItemRelatedItemAlreadyExistsException"

-- | A policy attribute or its value is invalid.
_InvalidPolicyAttributeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPolicyAttributeException =
  Core._MatchServiceError
    defaultService
    "InvalidPolicyAttributeException"

-- | The specified association doesn\'t exist.
_AssociationDoesNotExist :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AssociationDoesNotExist =
  Core._MatchServiceError
    defaultService
    "AssociationDoesNotExist"

-- | The version you specified isn\'t valid. Use ListAssociationVersions to
-- view all versions of an association according to the association ID. Or,
-- use the @$LATEST@ parameter to view the latest version of the
-- association.
_InvalidAssociationVersion :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAssociationVersion =
  Core._MatchServiceError
    defaultService
    "InvalidAssociationVersion"

-- | The filter value isn\'t valid. Verify the value and try again.
_InvalidFilterValue :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFilterValue =
  Core._MatchServiceError
    defaultService
    "InvalidFilterValue"

-- | You have exceeded the allowed maximum sync configurations.
_ResourceDataSyncCountExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceDataSyncCountExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceDataSyncCountExceededException"

-- | An error occurred on the server side.
_InternalServerError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError
    defaultService
    "InternalServerError"

-- | The parameter couldn\'t be found. Verify the name and try again.
_ParameterNotFound :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterNotFound =
  Core._MatchServiceError
    defaultService
    "ParameterNotFound"

-- | You can have at most 2,000 active associations.
_AssociationLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AssociationLimitExceeded =
  Core._MatchServiceError
    defaultService
    "AssociationLimitExceeded"

-- | The specified association already exists.
_AssociationAlreadyExists :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AssociationAlreadyExists =
  Core._MatchServiceError
    defaultService
    "AssociationAlreadyExists"

-- | You have exceeded the number of parameters for this Amazon Web Services
-- account. Delete one or more parameters and try again.
_ParameterLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterLimitExceeded =
  Core._MatchServiceError
    defaultService
    "ParameterLimitExceeded"

-- | The content for the document isn\'t valid.
_InvalidDocumentContent :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDocumentContent =
  Core._MatchServiceError
    defaultService
    "InvalidDocumentContent"

-- | The inventory item has invalid content.
_ItemContentMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ItemContentMismatchException =
  Core._MatchServiceError
    defaultService
    "ItemContentMismatchException"

-- | Patching for applications released by Microsoft is only available on EC2
-- instances and advanced instances. To patch applications released by
-- Microsoft on on-premises servers and VMs, you must enable advanced
-- instances. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-managedinstances-advanced.html Enabling the advanced-instances tier>
-- in the /Amazon Web Services Systems Manager User Guide/.
_UnsupportedFeatureRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedFeatureRequiredException =
  Core._MatchServiceError
    defaultService
    "UnsupportedFeatureRequiredException"

-- | You specified more than the maximum number of allowed policies for the
-- parameter. The maximum is 10.
_PoliciesLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PoliciesLimitExceededException =
  Core._MatchServiceError
    defaultService
    "PoliciesLimitExceededException"

-- | The plugin name isn\'t valid.
_InvalidPluginName :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPluginName =
  Core._MatchServiceError
    defaultService
    "InvalidPluginName"

-- | The parameter already exists. You can\'t create duplicate parameters.
_ParameterAlreadyExists :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterAlreadyExists =
  Core._MatchServiceError
    defaultService
    "ParameterAlreadyExists"

-- | The ID specified for the delete operation doesn\'t exist or isn\'t
-- valid. Verify the ID and try again.
_InvalidDeletionIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeletionIdException =
  Core._MatchServiceError
    defaultService
    "InvalidDeletionIdException"

-- | A sync configuration with the same name already exists.
_ResourceDataSyncAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceDataSyncAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceDataSyncAlreadyExistsException"

-- | The schedule is invalid. Verify your cron or rate expression and try
-- again.
_InvalidSchedule :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSchedule =
  Core._MatchServiceError
    defaultService
    "InvalidSchedule"

-- | The specified sync name wasn\'t found.
_ResourceDataSyncNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceDataSyncNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceDataSyncNotFoundException"

-- | Error returned if an attempt is made to register a patch group with a
-- patch baseline that is already registered with a different patch
-- baseline.
_AlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "AlreadyExistsException"

-- | The supplied parameters for invoking the specified Automation runbook
-- are incorrect. For example, they may not match the set of parameters
-- permitted for the specified Automation document.
_InvalidAutomationExecutionParametersException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAutomationExecutionParametersException =
  Core._MatchServiceError
    defaultService
    "InvalidAutomationExecutionParametersException"

-- | The content of the association document matches another document. Change
-- the content of the document and try again.
_DuplicateDocumentContent :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateDocumentContent =
  Core._MatchServiceError
    defaultService
    "DuplicateDocumentContent"

-- | Error returned if an attempt is made to delete a patch baseline that is
-- registered for a patch group.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | The policy type isn\'t supported. Parameter Store supports the following
-- policy types: Expiration, ExpirationNotification, and
-- NoChangeNotification.
_InvalidPolicyTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPolicyTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidPolicyTypeException"

-- | One or more configuration items isn\'t valid. Verify that a valid Amazon
-- Resource Name (ARN) was provided for an Amazon Simple Notification
-- Service topic.
_InvalidNotificationConfig :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNotificationConfig =
  Core._MatchServiceError
    defaultService
    "InvalidNotificationConfig"

-- | The specified key isn\'t valid.
_InvalidFilterKey :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFilterKey =
  Core._MatchServiceError
    defaultService
    "InvalidFilterKey"

-- | The SSM document type isn\'t valid. Valid document types are described
-- in the @DocumentType@ property.
_InvalidDocumentType :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDocumentType =
  Core._MatchServiceError
    defaultService
    "InvalidDocumentType"

-- | The resource ID isn\'t valid. Verify that you entered the correct ID and
-- try again.
_InvalidResourceId :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResourceId =
  Core._MatchServiceError
    defaultService
    "InvalidResourceId"

-- | The size of inventory data has exceeded the total size limit for the
-- resource.
_TotalSizeLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TotalSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TotalSizeLimitExceededException"

-- | You can\'t specify an instance ID in more than one association.
_DuplicateInstanceId :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateInstanceId =
  Core._MatchServiceError
    defaultService
    "DuplicateInstanceId"

-- | Indicates that the Change Manager change template used in the change
-- request was rejected or is still in a pending state.
_AutomationDefinitionNotApprovedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AutomationDefinitionNotApprovedException =
  Core._MatchServiceError
    defaultService
    "AutomationDefinitionNotApprovedException"

-- | You attempted to delete a document while it is still shared. You must
-- stop sharing the document before you can delete it.
_InvalidDocumentOperation :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDocumentOperation =
  Core._MatchServiceError
    defaultService
    "InvalidDocumentOperation"

-- | The query key ID isn\'t valid.
_InvalidKeyId :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidKeyId =
  Core._MatchServiceError
    defaultService
    "InvalidKeyId"

-- | The specified inventory item result attribute isn\'t valid.
_InvalidResultAttributeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResultAttributeException =
  Core._MatchServiceError
    defaultService
    "InvalidResultAttributeException"

-- | The command ID and instance ID you specified didn\'t match any
-- invocations. Verify the command ID and the instance ID and try again.
_InvocationDoesNotExist :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvocationDoesNotExist =
  Core._MatchServiceError
    defaultService
    "InvocationDoesNotExist"

-- | The resource type isn\'t valid. For example, if you are attempting to
-- tag an instance, the instance must be a registered, managed instance.
_InvalidResourceType :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResourceType =
  Core._MatchServiceError
    defaultService
    "InvalidResourceType"

-- | The document has too many versions. Delete one or more document versions
-- and try again.
_DocumentVersionLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DocumentVersionLimitExceeded =
  Core._MatchServiceError
    defaultService
    "DocumentVersionLimitExceeded"

-- | The specified sync configuration is invalid.
_ResourceDataSyncInvalidConfigurationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceDataSyncInvalidConfigurationException =
  Core._MatchServiceError
    defaultService
    "ResourceDataSyncInvalidConfigurationException"

-- | The specified inventory group isn\'t valid.
_InvalidInventoryGroupException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInventoryGroupException =
  Core._MatchServiceError
    defaultService
    "InvalidInventoryGroupException"

-- | The specified execution ID doesn\'t exist. Verify the ID number and try
-- again.
_AssociationExecutionDoesNotExist :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AssociationExecutionDoesNotExist =
  Core._MatchServiceError
    defaultService
    "AssociationExecutionDoesNotExist"

-- | Your account reached the maximum number of OpsMetadata objects allowed
-- by Application Manager. The maximum is 200 OpsMetadata objects. Delete
-- one or more OpsMetadata object and try again.
_OpsMetadataLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsMetadataLimitExceededException =
  Core._MatchServiceError
    defaultService
    "OpsMetadataLimitExceededException"

-- | An OpsMetadata object already exists for the selected resource.
_OpsMetadataAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsMetadataAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "OpsMetadataAlreadyExistsException"

-- | The S3 bucket doesn\'t exist.
_InvalidOutputFolder :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOutputFolder =
  Core._MatchServiceError
    defaultService
    "InvalidOutputFolder"

-- | A hierarchy can have a maximum of 15 levels. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-parameter-name-constraints.html Requirements and constraints for parameter names>
-- in the /Amazon Web Services Systems Manager User Guide/.
_HierarchyLevelLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_HierarchyLevelLimitExceededException =
  Core._MatchServiceError
    defaultService
    "HierarchyLevelLimitExceededException"

-- | The request caused OpsItems to exceed one or more quotas. For
-- information about OpsItem quotas, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-learn-more.html#OpsCenter-learn-more-limits What are the resource limits for OpsCenter?>.
_OpsItemLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsItemLimitExceededException =
  Core._MatchServiceError
    defaultService
    "OpsItemLimitExceededException"

-- | The calendar entry contained in the specified SSM document isn\'t
-- supported.
_UnsupportedCalendarException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedCalendarException =
  Core._MatchServiceError
    defaultService
    "UnsupportedCalendarException"

-- | The specified parameter version wasn\'t found. Verify the parameter name
-- and version, and try again.
_ParameterVersionNotFound :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterVersionNotFound =
  Core._MatchServiceError
    defaultService
    "ParameterVersionNotFound"

-- | The OpsItem already exists.
_OpsItemAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsItemAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "OpsItemAlreadyExistsException"

-- | The activation ID isn\'t valid. Verify the you entered the correct
-- ActivationId or ActivationCode and try again.
_InvalidActivationId :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidActivationId =
  Core._MatchServiceError
    defaultService
    "InvalidActivationId"

-- | The role name can\'t contain invalid characters. Also verify that you
-- specified an IAM role for notifications that includes the required trust
-- policy. For information about configuring the IAM role for Run Command
-- notifications, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/rc-sns-notifications.html Configuring Amazon SNS Notifications for Run Command>
-- in the /Amazon Web Services Systems Manager User Guide/.
_InvalidRole :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRole =
  Core._MatchServiceError
    defaultService
    "InvalidRole"

-- | The activation isn\'t valid. The activation might have been deleted, or
-- the ActivationId and the ActivationCode don\'t match.
_InvalidActivation :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidActivation =
  Core._MatchServiceError
    defaultService
    "InvalidActivation"

-- | The request isn\'t valid.
_InvalidInventoryRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInventoryRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidInventoryRequestException"

-- | The specified token isn\'t valid.
_InvalidNextToken :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextToken =
  Core._MatchServiceError
    defaultService
    "InvalidNextToken"

-- | You have reached the maximum number versions allowed for an association.
-- Each association has a limit of 1,000 versions.
_AssociationVersionLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AssociationVersionLimitExceeded =
  Core._MatchServiceError
    defaultService
    "AssociationVersionLimitExceeded"

-- | The size limit of a document is 64 KB.
_MaxDocumentSizeExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MaxDocumentSizeExceeded =
  Core._MatchServiceError
    defaultService
    "MaxDocumentSizeExceeded"

-- | The operating systems you specified isn\'t supported, or the operation
-- isn\'t supported for the operating system.
_UnsupportedOperatingSystem :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperatingSystem =
  Core._MatchServiceError
    defaultService
    "UnsupportedOperatingSystem"

-- | The document doesn\'t support the platform type of the given instance
-- ID(s). For example, you sent an document for a Windows instance to a
-- Linux instance.
_UnsupportedPlatformType :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedPlatformType =
  Core._MatchServiceError
    defaultService
    "UnsupportedPlatformType"

-- | You must disassociate a document from all instances before you can
-- delete it.
_AssociatedInstances :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AssociatedInstances =
  Core._MatchServiceError
    defaultService
    "AssociatedInstances"

-- | One or more content items isn\'t valid.
_InvalidItemContentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidItemContentException =
  Core._MatchServiceError
    defaultService
    "InvalidItemContentException"

-- | The permission type isn\'t supported. /Share/ is the only supported
-- permission type.
_InvalidPermissionType :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPermissionType =
  Core._MatchServiceError
    defaultService
    "InvalidPermissionType"

-- | The @Context@ attribute that you specified for the @InventoryItem@
-- isn\'t allowed for this inventory type. You can only use the @Context@
-- attribute with inventory types like @AWS:ComplianceItem@.
_UnsupportedInventoryItemContextException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedInventoryItemContextException =
  Core._MatchServiceError
    defaultService
    "UnsupportedInventoryItemContextException"

-- | The specified filter option isn\'t valid. Valid options are Equals and
-- BeginsWith. For Path filter, valid options are Recursive and OneLevel.
_InvalidFilterOption :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFilterOption =
  Core._MatchServiceError
    defaultService
    "InvalidFilterOption"

-- | The parameter name isn\'t valid.
_ParameterPatternMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterPatternMismatchException =
  Core._MatchServiceError
    defaultService
    "ParameterPatternMismatchException"

-- | You specified the @Safe@ option for the
-- DeregisterTargetFromMaintenanceWindow operation, but the target is still
-- referenced in a task.
_TargetInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TargetInUseException =
  Core._MatchServiceError
    defaultService
    "TargetInUseException"
