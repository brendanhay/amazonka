{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSM.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AutomationDefinitionVersionNotFoundException,
    _InvalidDocumentVersion,
    _HierarchyTypeMismatchException,
    _InvalidSchedule,
    _UnsupportedParameterType,
    _InvalidAutomationStatusUpdateException,
    _OpsMetadataKeyLimitExceededException,
    _InvalidPluginName,
    _UnsupportedFeatureRequiredException,
    _InvalidAggregatorException,
    _FeatureNotAvailableException,
    _InvalidAutomationSignalException,
    _ResourceDataSyncCountExceededException,
    _OpsItemRelatedItemAlreadyExistsException,
    _UnsupportedPlatformType,
    _InvalidFilterValue,
    _InvalidItemContentException,
    _InvalidFilterOption,
    _ParameterPatternMismatchException,
    _InvalidPermissionType,
    _AssociatedInstances,
    _UnsupportedOperatingSystem,
    _InvalidInstanceId,
    _StatusUnchanged,
    _InvalidNextToken,
    _InvalidInventoryRequestException,
    _InvalidAssociation,
    _OpsItemAlreadyExistsException,
    _InvalidOutputFolder,
    _OpsMetadataAlreadyExistsException,
    _OpsItemLimitExceededException,
    _InvalidActivationId,
    _OpsMetadataLimitExceededException,
    _ServiceSettingNotFound,
    _InvalidResultAttributeException,
    _TargetNotConnected,
    _ResourceLimitExceededException,
    _OpsMetadataTooManyUpdatesException,
    _ParameterVersionLabelLimitExceeded,
    _ResourceDataSyncInvalidConfigurationException,
    _InvalidCommandId,
    _DuplicateInstanceId,
    _InvalidResourceType,
    _UnsupportedInventorySchemaVersionException,
    _InvalidDocument,
    _IncompatiblePolicyException,
    _AutomationDefinitionNotFoundException,
    _InvalidPolicyTypeException,
    _InvalidFilterKey,
    _InvalidAutomationExecutionParametersException,
    _AutomationExecutionNotFoundException,
    _InvalidTypeNameException,
    _ResourceDataSyncNotFoundException,
    _ParameterMaxVersionLimitExceeded,
    _ItemSizeLimitExceededException,
    _ResourceDataSyncAlreadyExistsException,
    _DoesNotExistException,
    _ResourceDataSyncConflictException,
    _OpsMetadataInvalidArgumentException,
    _AutomationExecutionLimitExceededException,
    _IdempotentParameterMismatch,
    _InvalidInstanceInformationFilterValue,
    _ItemContentMismatchException,
    _ParameterAlreadyExists,
    _AssociationAlreadyExists,
    _ComplianceTypeCountLimitExceededException,
    _InvalidDeleteInventoryParametersException,
    _InvalidDeletionIdException,
    _PoliciesLimitExceededException,
    _InvalidDocumentContent,
    _ParameterLimitExceeded,
    _AssociationLimitExceeded,
    _InvalidAssociationVersion,
    _AssociationDoesNotExist,
    _InvalidPolicyAttributeException,
    _ParameterNotFound,
    _TargetInUseException,
    _InternalServerError,
    _UnsupportedInventoryItemContextException,
    _OpsMetadataNotFoundException,
    _AssociationVersionLimitExceeded,
    _InvalidRole,
    _TooManyUpdates,
    _DuplicateDocumentVersionName,
    _OpsItemNotFoundException,
    _InvalidActivation,
    _InvalidOptionException,
    _InvalidDocumentSchemaVersion,
    _MaxDocumentSizeExceeded,
    _ParameterVersionNotFound,
    _UnsupportedCalendarException,
    _InvalidUpdate,
    _CustomSchemaCountLimitExceededException,
    _AssociationExecutionDoesNotExist,
    _InvalidTarget,
    _HierarchyLevelLimitExceededException,
    _InvalidInventoryGroupException,
    _InvalidDocumentOperation,
    _InvocationDoesNotExist,
    _DocumentVersionLimitExceeded,
    _InvalidOutputLocation,
    _InvalidKeyId,
    _InvalidParameters,
    _AutomationDefinitionNotApprovedException,
    _OpsItemInvalidParameterException,
    _InvalidResourceId,
    _InvalidAllowedPatternException,
    _InvalidNotificationConfig,
    _OpsItemRelatedItemAssociationNotFoundException,
    _InvalidInventoryItemContextException,
    _TotalSizeLimitExceededException,
    _SubTypeCountLimitExceededException,
    _InvalidDocumentType,
    _TooManyTagsError,
    _DocumentPermissionLimit,
    _AutomationStepNotFoundException,
    _DuplicateDocumentContent,
    _DocumentAlreadyExists,
    _DocumentLimitExceeded,
    _AlreadyExistsException,
    _InvalidFilter,
    _ResourceInUseException,

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
    accountSharingInfo_sharedDocumentVersion,
    accountSharingInfo_accountId,

    -- * Activation
    Activation (..),
    newActivation,
    activation_expired,
    activation_defaultInstanceName,
    activation_activationId,
    activation_createdDate,
    activation_registrationLimit,
    activation_expirationDate,
    activation_description,
    activation_tags,
    activation_registrationsCount,
    activation_iamRole,

    -- * Association
    Association (..),
    newAssociation,
    association_associationId,
    association_instanceId,
    association_overview,
    association_lastExecutionDate,
    association_scheduleExpression,
    association_name,
    association_targets,
    association_documentVersion,
    association_associationVersion,
    association_associationName,

    -- * AssociationDescription
    AssociationDescription (..),
    newAssociationDescription,
    associationDescription_associationId,
    associationDescription_instanceId,
    associationDescription_status,
    associationDescription_targetLocations,
    associationDescription_applyOnlyAtCronInterval,
    associationDescription_lastSuccessfulExecutionDate,
    associationDescription_overview,
    associationDescription_lastUpdateAssociationDate,
    associationDescription_date,
    associationDescription_lastExecutionDate,
    associationDescription_maxErrors,
    associationDescription_scheduleExpression,
    associationDescription_name,
    associationDescription_outputLocation,
    associationDescription_syncCompliance,
    associationDescription_targets,
    associationDescription_parameters,
    associationDescription_documentVersion,
    associationDescription_automationTargetParameterName,
    associationDescription_associationVersion,
    associationDescription_associationName,
    associationDescription_calendarNames,
    associationDescription_complianceSeverity,
    associationDescription_maxConcurrency,

    -- * AssociationExecution
    AssociationExecution (..),
    newAssociationExecution,
    associationExecution_associationId,
    associationExecution_detailedStatus,
    associationExecution_status,
    associationExecution_executionId,
    associationExecution_createdTime,
    associationExecution_resourceCountByStatus,
    associationExecution_lastExecutionDate,
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
    associationExecutionTarget_associationId,
    associationExecutionTarget_detailedStatus,
    associationExecutionTarget_status,
    associationExecutionTarget_executionId,
    associationExecutionTarget_resourceId,
    associationExecutionTarget_resourceType,
    associationExecutionTarget_outputSource,
    associationExecutionTarget_lastExecutionDate,
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
    associationOverview_detailedStatus,
    associationOverview_status,
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
    associationVersionInfo_associationId,
    associationVersionInfo_targetLocations,
    associationVersionInfo_applyOnlyAtCronInterval,
    associationVersionInfo_createdDate,
    associationVersionInfo_maxErrors,
    associationVersionInfo_scheduleExpression,
    associationVersionInfo_name,
    associationVersionInfo_outputLocation,
    associationVersionInfo_syncCompliance,
    associationVersionInfo_targets,
    associationVersionInfo_parameters,
    associationVersionInfo_documentVersion,
    associationVersionInfo_associationVersion,
    associationVersionInfo_associationName,
    associationVersionInfo_calendarNames,
    associationVersionInfo_complianceSeverity,
    associationVersionInfo_maxConcurrency,

    -- * AttachmentContent
    AttachmentContent (..),
    newAttachmentContent,
    attachmentContent_hash,
    attachmentContent_size,
    attachmentContent_url,
    attachmentContent_name,
    attachmentContent_hashType,

    -- * AttachmentInformation
    AttachmentInformation (..),
    newAttachmentInformation,
    attachmentInformation_name,

    -- * AttachmentsSource
    AttachmentsSource (..),
    newAttachmentsSource,
    attachmentsSource_values,
    attachmentsSource_key,
    attachmentsSource_name,

    -- * AutomationExecution
    AutomationExecution (..),
    newAutomationExecution,
    automationExecution_scheduledTime,
    automationExecution_associationId,
    automationExecution_opsItemId,
    automationExecution_currentStepName,
    automationExecution_targetParameterName,
    automationExecution_targetLocations,
    automationExecution_progressCounters,
    automationExecution_executedBy,
    automationExecution_documentName,
    automationExecution_executionEndTime,
    automationExecution_failureMessage,
    automationExecution_automationSubtype,
    automationExecution_mode,
    automationExecution_targetMaps,
    automationExecution_stepExecutionsTruncated,
    automationExecution_automationExecutionStatus,
    automationExecution_parentAutomationExecutionId,
    automationExecution_outputs,
    automationExecution_maxErrors,
    automationExecution_executionStartTime,
    automationExecution_currentAction,
    automationExecution_targets,
    automationExecution_resolvedTargets,
    automationExecution_parameters,
    automationExecution_documentVersion,
    automationExecution_automationExecutionId,
    automationExecution_changeRequestName,
    automationExecution_stepExecutions,
    automationExecution_runbooks,
    automationExecution_maxConcurrency,
    automationExecution_target,

    -- * AutomationExecutionFilter
    AutomationExecutionFilter (..),
    newAutomationExecutionFilter,
    automationExecutionFilter_key,
    automationExecutionFilter_values,

    -- * AutomationExecutionMetadata
    AutomationExecutionMetadata (..),
    newAutomationExecutionMetadata,
    automationExecutionMetadata_scheduledTime,
    automationExecutionMetadata_associationId,
    automationExecutionMetadata_opsItemId,
    automationExecutionMetadata_currentStepName,
    automationExecutionMetadata_targetParameterName,
    automationExecutionMetadata_logFile,
    automationExecutionMetadata_executedBy,
    automationExecutionMetadata_documentName,
    automationExecutionMetadata_executionEndTime,
    automationExecutionMetadata_failureMessage,
    automationExecutionMetadata_automationSubtype,
    automationExecutionMetadata_mode,
    automationExecutionMetadata_targetMaps,
    automationExecutionMetadata_automationExecutionStatus,
    automationExecutionMetadata_parentAutomationExecutionId,
    automationExecutionMetadata_outputs,
    automationExecutionMetadata_maxErrors,
    automationExecutionMetadata_executionStartTime,
    automationExecutionMetadata_automationType,
    automationExecutionMetadata_currentAction,
    automationExecutionMetadata_targets,
    automationExecutionMetadata_resolvedTargets,
    automationExecutionMetadata_documentVersion,
    automationExecutionMetadata_automationExecutionId,
    automationExecutionMetadata_changeRequestName,
    automationExecutionMetadata_runbooks,
    automationExecutionMetadata_maxConcurrency,
    automationExecutionMetadata_target,

    -- * BaselineOverride
    BaselineOverride (..),
    newBaselineOverride,
    baselineOverride_approvalRules,
    baselineOverride_operatingSystem,
    baselineOverride_globalFilters,
    baselineOverride_approvedPatchesComplianceLevel,
    baselineOverride_rejectedPatchesAction,
    baselineOverride_approvedPatches,
    baselineOverride_approvedPatchesEnableNonSecurity,
    baselineOverride_rejectedPatches,
    baselineOverride_sources,

    -- * CloudWatchOutputConfig
    CloudWatchOutputConfig (..),
    newCloudWatchOutputConfig,
    cloudWatchOutputConfig_cloudWatchLogGroupName,
    cloudWatchOutputConfig_cloudWatchOutputEnabled,

    -- * Command
    Command (..),
    newCommand,
    command_status,
    command_expiresAfter,
    command_notificationConfig,
    command_targetCount,
    command_cloudWatchOutputConfig,
    command_deliveryTimedOutCount,
    command_outputS3KeyPrefix,
    command_documentName,
    command_errorCount,
    command_statusDetails,
    command_maxErrors,
    command_instanceIds,
    command_outputS3Region,
    command_targets,
    command_commandId,
    command_parameters,
    command_documentVersion,
    command_timeoutSeconds,
    command_comment,
    command_completedCount,
    command_outputS3BucketName,
    command_maxConcurrency,
    command_requestedDateTime,
    command_serviceRole,

    -- * CommandFilter
    CommandFilter (..),
    newCommandFilter,
    commandFilter_key,
    commandFilter_value,

    -- * CommandInvocation
    CommandInvocation (..),
    newCommandInvocation,
    commandInvocation_instanceId,
    commandInvocation_status,
    commandInvocation_notificationConfig,
    commandInvocation_commandPlugins,
    commandInvocation_cloudWatchOutputConfig,
    commandInvocation_documentName,
    commandInvocation_standardErrorUrl,
    commandInvocation_statusDetails,
    commandInvocation_standardOutputUrl,
    commandInvocation_commandId,
    commandInvocation_documentVersion,
    commandInvocation_comment,
    commandInvocation_traceOutput,
    commandInvocation_instanceName,
    commandInvocation_requestedDateTime,
    commandInvocation_serviceRole,

    -- * CommandPlugin
    CommandPlugin (..),
    newCommandPlugin,
    commandPlugin_status,
    commandPlugin_responseStartDateTime,
    commandPlugin_outputS3KeyPrefix,
    commandPlugin_standardErrorUrl,
    commandPlugin_responseCode,
    commandPlugin_statusDetails,
    commandPlugin_output,
    commandPlugin_standardOutputUrl,
    commandPlugin_name,
    commandPlugin_outputS3Region,
    commandPlugin_outputS3BucketName,
    commandPlugin_responseFinishDateTime,

    -- * ComplianceExecutionSummary
    ComplianceExecutionSummary (..),
    newComplianceExecutionSummary,
    complianceExecutionSummary_executionId,
    complianceExecutionSummary_executionType,
    complianceExecutionSummary_executionTime,

    -- * ComplianceItem
    ComplianceItem (..),
    newComplianceItem,
    complianceItem_status,
    complianceItem_resourceId,
    complianceItem_resourceType,
    complianceItem_severity,
    complianceItem_executionSummary,
    complianceItem_details,
    complianceItem_id,
    complianceItem_complianceType,
    complianceItem_title,

    -- * ComplianceItemEntry
    ComplianceItemEntry (..),
    newComplianceItemEntry,
    complianceItemEntry_details,
    complianceItemEntry_id,
    complianceItemEntry_title,
    complianceItemEntry_severity,
    complianceItemEntry_status,

    -- * ComplianceStringFilter
    ComplianceStringFilter (..),
    newComplianceStringFilter,
    complianceStringFilter_values,
    complianceStringFilter_key,
    complianceStringFilter_type,

    -- * ComplianceSummaryItem
    ComplianceSummaryItem (..),
    newComplianceSummaryItem,
    complianceSummaryItem_nonCompliantSummary,
    complianceSummaryItem_compliantSummary,
    complianceSummaryItem_complianceType,

    -- * CompliantSummary
    CompliantSummary (..),
    newCompliantSummary,
    compliantSummary_compliantCount,
    compliantSummary_severitySummary,

    -- * CreateAssociationBatchRequestEntry
    CreateAssociationBatchRequestEntry (..),
    newCreateAssociationBatchRequestEntry,
    createAssociationBatchRequestEntry_instanceId,
    createAssociationBatchRequestEntry_targetLocations,
    createAssociationBatchRequestEntry_applyOnlyAtCronInterval,
    createAssociationBatchRequestEntry_maxErrors,
    createAssociationBatchRequestEntry_scheduleExpression,
    createAssociationBatchRequestEntry_outputLocation,
    createAssociationBatchRequestEntry_syncCompliance,
    createAssociationBatchRequestEntry_targets,
    createAssociationBatchRequestEntry_parameters,
    createAssociationBatchRequestEntry_documentVersion,
    createAssociationBatchRequestEntry_automationTargetParameterName,
    createAssociationBatchRequestEntry_associationName,
    createAssociationBatchRequestEntry_calendarNames,
    createAssociationBatchRequestEntry_complianceSeverity,
    createAssociationBatchRequestEntry_maxConcurrency,
    createAssociationBatchRequestEntry_name,

    -- * DescribeActivationsFilter
    DescribeActivationsFilter (..),
    newDescribeActivationsFilter,
    describeActivationsFilter_filterKey,
    describeActivationsFilter_filterValues,

    -- * DocumentDefaultVersionDescription
    DocumentDefaultVersionDescription (..),
    newDocumentDefaultVersionDescription,
    documentDefaultVersionDescription_defaultVersionName,
    documentDefaultVersionDescription_defaultVersion,
    documentDefaultVersionDescription_name,

    -- * DocumentDescription
    DocumentDescription (..),
    newDocumentDescription,
    documentDescription_status,
    documentDescription_documentType,
    documentDescription_hash,
    documentDescription_versionName,
    documentDescription_schemaVersion,
    documentDescription_sha1,
    documentDescription_reviewStatus,
    documentDescription_attachmentsInformation,
    documentDescription_defaultVersion,
    documentDescription_targetType,
    documentDescription_owner,
    documentDescription_platformTypes,
    documentDescription_createdDate,
    documentDescription_documentFormat,
    documentDescription_pendingReviewVersion,
    documentDescription_name,
    documentDescription_hashType,
    documentDescription_parameters,
    documentDescription_documentVersion,
    documentDescription_author,
    documentDescription_displayName,
    documentDescription_statusInformation,
    documentDescription_description,
    documentDescription_requires,
    documentDescription_reviewInformation,
    documentDescription_tags,
    documentDescription_latestVersion,
    documentDescription_approvedVersion,

    -- * DocumentFilter
    DocumentFilter (..),
    newDocumentFilter,
    documentFilter_key,
    documentFilter_value,

    -- * DocumentIdentifier
    DocumentIdentifier (..),
    newDocumentIdentifier,
    documentIdentifier_documentType,
    documentIdentifier_versionName,
    documentIdentifier_schemaVersion,
    documentIdentifier_reviewStatus,
    documentIdentifier_targetType,
    documentIdentifier_owner,
    documentIdentifier_platformTypes,
    documentIdentifier_createdDate,
    documentIdentifier_documentFormat,
    documentIdentifier_name,
    documentIdentifier_documentVersion,
    documentIdentifier_author,
    documentIdentifier_displayName,
    documentIdentifier_requires,
    documentIdentifier_tags,

    -- * DocumentKeyValuesFilter
    DocumentKeyValuesFilter (..),
    newDocumentKeyValuesFilter,
    documentKeyValuesFilter_values,
    documentKeyValuesFilter_key,

    -- * DocumentMetadataResponseInfo
    DocumentMetadataResponseInfo (..),
    newDocumentMetadataResponseInfo,
    documentMetadataResponseInfo_reviewerResponse,

    -- * DocumentParameter
    DocumentParameter (..),
    newDocumentParameter,
    documentParameter_name,
    documentParameter_defaultValue,
    documentParameter_type,
    documentParameter_description,

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
    documentReviewerResponseSource_reviewer,
    documentReviewerResponseSource_reviewStatus,
    documentReviewerResponseSource_updatedTime,
    documentReviewerResponseSource_comment,
    documentReviewerResponseSource_createTime,

    -- * DocumentReviews
    DocumentReviews (..),
    newDocumentReviews,
    documentReviews_comment,
    documentReviews_action,

    -- * DocumentVersionInfo
    DocumentVersionInfo (..),
    newDocumentVersionInfo,
    documentVersionInfo_status,
    documentVersionInfo_versionName,
    documentVersionInfo_reviewStatus,
    documentVersionInfo_createdDate,
    documentVersionInfo_documentFormat,
    documentVersionInfo_name,
    documentVersionInfo_documentVersion,
    documentVersionInfo_displayName,
    documentVersionInfo_statusInformation,
    documentVersionInfo_isDefaultVersion,

    -- * EffectivePatch
    EffectivePatch (..),
    newEffectivePatch,
    effectivePatch_patch,
    effectivePatch_patchStatus,

    -- * FailedCreateAssociation
    FailedCreateAssociation (..),
    newFailedCreateAssociation,
    failedCreateAssociation_entry,
    failedCreateAssociation_fault,
    failedCreateAssociation_message,

    -- * FailureDetails
    FailureDetails (..),
    newFailureDetails,
    failureDetails_failureType,
    failureDetails_failureStage,
    failureDetails_details,

    -- * InstanceAggregatedAssociationOverview
    InstanceAggregatedAssociationOverview (..),
    newInstanceAggregatedAssociationOverview,
    instanceAggregatedAssociationOverview_detailedStatus,
    instanceAggregatedAssociationOverview_instanceAssociationStatusAggregatedCount,

    -- * InstanceAssociation
    InstanceAssociation (..),
    newInstanceAssociation,
    instanceAssociation_associationId,
    instanceAssociation_instanceId,
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
    instanceAssociationStatusInfo_associationId,
    instanceAssociationStatusInfo_instanceId,
    instanceAssociationStatusInfo_detailedStatus,
    instanceAssociationStatusInfo_status,
    instanceAssociationStatusInfo_outputUrl,
    instanceAssociationStatusInfo_executionSummary,
    instanceAssociationStatusInfo_name,
    instanceAssociationStatusInfo_errorCode,
    instanceAssociationStatusInfo_documentVersion,
    instanceAssociationStatusInfo_associationVersion,
    instanceAssociationStatusInfo_executionDate,
    instanceAssociationStatusInfo_associationName,

    -- * InstanceInformation
    InstanceInformation (..),
    newInstanceInformation,
    instanceInformation_instanceId,
    instanceInformation_pingStatus,
    instanceInformation_iPAddress,
    instanceInformation_resourceType,
    instanceInformation_registrationDate,
    instanceInformation_platformVersion,
    instanceInformation_isLatestVersion,
    instanceInformation_agentVersion,
    instanceInformation_lastPingDateTime,
    instanceInformation_lastSuccessfulAssociationExecutionDate,
    instanceInformation_activationId,
    instanceInformation_name,
    instanceInformation_platformType,
    instanceInformation_associationOverview,
    instanceInformation_associationStatus,
    instanceInformation_lastAssociationExecutionDate,
    instanceInformation_platformName,
    instanceInformation_computerName,
    instanceInformation_iamRole,

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
    instancePatchState_unreportedNotApplicableCount,
    instancePatchState_otherNonCompliantCount,
    instancePatchState_rebootOption,
    instancePatchState_installedPendingRebootCount,
    instancePatchState_ownerInformation,
    instancePatchState_securityNonCompliantCount,
    instancePatchState_installedRejectedCount,
    instancePatchState_failedCount,
    instancePatchState_installedOtherCount,
    instancePatchState_missingCount,
    instancePatchState_installOverrideList,
    instancePatchState_criticalNonCompliantCount,
    instancePatchState_notApplicableCount,
    instancePatchState_installedCount,
    instancePatchState_lastNoRebootInstallOperationTime,
    instancePatchState_snapshotId,
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
    inventoryDeletionStatusItem_lastStatusUpdateTime,
    inventoryDeletionStatusItem_lastStatusMessage,
    inventoryDeletionStatusItem_deletionSummary,
    inventoryDeletionStatusItem_lastStatus,
    inventoryDeletionStatusItem_deletionStartTime,
    inventoryDeletionStatusItem_deletionId,

    -- * InventoryDeletionSummary
    InventoryDeletionSummary (..),
    newInventoryDeletionSummary,
    inventoryDeletionSummary_remainingCount,
    inventoryDeletionSummary_summaryItems,
    inventoryDeletionSummary_totalCount,

    -- * InventoryDeletionSummaryItem
    InventoryDeletionSummaryItem (..),
    newInventoryDeletionSummaryItem,
    inventoryDeletionSummaryItem_remainingCount,
    inventoryDeletionSummaryItem_count,
    inventoryDeletionSummaryItem_version,

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
    inventoryItem_contentHash,
    inventoryItem_content,
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
    inventoryResultItem_contentHash,
    inventoryResultItem_captureTime,
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
    maintenanceWindowAutomationParameters_parameters,
    maintenanceWindowAutomationParameters_documentVersion,

    -- * MaintenanceWindowExecution
    MaintenanceWindowExecution (..),
    newMaintenanceWindowExecution,
    maintenanceWindowExecution_status,
    maintenanceWindowExecution_startTime,
    maintenanceWindowExecution_windowExecutionId,
    maintenanceWindowExecution_statusDetails,
    maintenanceWindowExecution_endTime,
    maintenanceWindowExecution_windowId,

    -- * MaintenanceWindowExecutionTaskIdentity
    MaintenanceWindowExecutionTaskIdentity (..),
    newMaintenanceWindowExecutionTaskIdentity,
    maintenanceWindowExecutionTaskIdentity_status,
    maintenanceWindowExecutionTaskIdentity_taskExecutionId,
    maintenanceWindowExecutionTaskIdentity_startTime,
    maintenanceWindowExecutionTaskIdentity_taskType,
    maintenanceWindowExecutionTaskIdentity_taskArn,
    maintenanceWindowExecutionTaskIdentity_windowExecutionId,
    maintenanceWindowExecutionTaskIdentity_statusDetails,
    maintenanceWindowExecutionTaskIdentity_endTime,

    -- * MaintenanceWindowExecutionTaskInvocationIdentity
    MaintenanceWindowExecutionTaskInvocationIdentity (..),
    newMaintenanceWindowExecutionTaskInvocationIdentity,
    maintenanceWindowExecutionTaskInvocationIdentity_status,
    maintenanceWindowExecutionTaskInvocationIdentity_executionId,
    maintenanceWindowExecutionTaskInvocationIdentity_taskExecutionId,
    maintenanceWindowExecutionTaskInvocationIdentity_startTime,
    maintenanceWindowExecutionTaskInvocationIdentity_invocationId,
    maintenanceWindowExecutionTaskInvocationIdentity_ownerInformation,
    maintenanceWindowExecutionTaskInvocationIdentity_taskType,
    maintenanceWindowExecutionTaskInvocationIdentity_windowTargetId,
    maintenanceWindowExecutionTaskInvocationIdentity_windowExecutionId,
    maintenanceWindowExecutionTaskInvocationIdentity_statusDetails,
    maintenanceWindowExecutionTaskInvocationIdentity_endTime,
    maintenanceWindowExecutionTaskInvocationIdentity_parameters,

    -- * MaintenanceWindowFilter
    MaintenanceWindowFilter (..),
    newMaintenanceWindowFilter,
    maintenanceWindowFilter_values,
    maintenanceWindowFilter_key,

    -- * MaintenanceWindowIdentity
    MaintenanceWindowIdentity (..),
    newMaintenanceWindowIdentity,
    maintenanceWindowIdentity_enabled,
    maintenanceWindowIdentity_schedule,
    maintenanceWindowIdentity_nextExecutionTime,
    maintenanceWindowIdentity_scheduleOffset,
    maintenanceWindowIdentity_endDate,
    maintenanceWindowIdentity_scheduleTimezone,
    maintenanceWindowIdentity_startDate,
    maintenanceWindowIdentity_name,
    maintenanceWindowIdentity_cutoff,
    maintenanceWindowIdentity_description,
    maintenanceWindowIdentity_duration,
    maintenanceWindowIdentity_windowId,

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
    maintenanceWindowRunCommandParameters_serviceRoleArn,
    maintenanceWindowRunCommandParameters_notificationConfig,
    maintenanceWindowRunCommandParameters_documentHashType,
    maintenanceWindowRunCommandParameters_cloudWatchOutputConfig,
    maintenanceWindowRunCommandParameters_outputS3KeyPrefix,
    maintenanceWindowRunCommandParameters_parameters,
    maintenanceWindowRunCommandParameters_documentHash,
    maintenanceWindowRunCommandParameters_documentVersion,
    maintenanceWindowRunCommandParameters_timeoutSeconds,
    maintenanceWindowRunCommandParameters_comment,
    maintenanceWindowRunCommandParameters_outputS3BucketName,

    -- * MaintenanceWindowStepFunctionsParameters
    MaintenanceWindowStepFunctionsParameters (..),
    newMaintenanceWindowStepFunctionsParameters,
    maintenanceWindowStepFunctionsParameters_input,
    maintenanceWindowStepFunctionsParameters_name,

    -- * MaintenanceWindowTarget
    MaintenanceWindowTarget (..),
    newMaintenanceWindowTarget,
    maintenanceWindowTarget_resourceType,
    maintenanceWindowTarget_ownerInformation,
    maintenanceWindowTarget_windowTargetId,
    maintenanceWindowTarget_name,
    maintenanceWindowTarget_targets,
    maintenanceWindowTarget_description,
    maintenanceWindowTarget_windowId,

    -- * MaintenanceWindowTask
    MaintenanceWindowTask (..),
    newMaintenanceWindowTask,
    maintenanceWindowTask_serviceRoleArn,
    maintenanceWindowTask_windowTaskId,
    maintenanceWindowTask_taskParameters,
    maintenanceWindowTask_priority,
    maintenanceWindowTask_taskArn,
    maintenanceWindowTask_cutoffBehavior,
    maintenanceWindowTask_maxErrors,
    maintenanceWindowTask_name,
    maintenanceWindowTask_targets,
    maintenanceWindowTask_loggingInfo,
    maintenanceWindowTask_type,
    maintenanceWindowTask_description,
    maintenanceWindowTask_maxConcurrency,
    maintenanceWindowTask_windowId,

    -- * MaintenanceWindowTaskInvocationParameters
    MaintenanceWindowTaskInvocationParameters (..),
    newMaintenanceWindowTaskInvocationParameters,
    maintenanceWindowTaskInvocationParameters_automation,
    maintenanceWindowTaskInvocationParameters_stepFunctions,
    maintenanceWindowTaskInvocationParameters_runCommand,
    maintenanceWindowTaskInvocationParameters_lambda,

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
    nonCompliantSummary_nonCompliantCount,
    nonCompliantSummary_severitySummary,

    -- * NotificationConfig
    NotificationConfig (..),
    newNotificationConfig,
    notificationConfig_notificationEvents,
    notificationConfig_notificationType,
    notificationConfig_notificationArn,

    -- * OpsAggregator
    OpsAggregator (..),
    newOpsAggregator,
    opsAggregator_typeName,
    opsAggregator_aggregators,
    opsAggregator_values,
    opsAggregator_filters,
    opsAggregator_attributeName,
    opsAggregator_aggregatorType,

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
    opsItem_actualEndTime,
    opsItem_opsItemId,
    opsItem_status,
    opsItem_priority,
    opsItem_createdTime,
    opsItem_category,
    opsItem_severity,
    opsItem_createdBy,
    opsItem_lastModifiedTime,
    opsItem_opsItemType,
    opsItem_version,
    opsItem_source,
    opsItem_relatedOpsItems,
    opsItem_title,
    opsItem_lastModifiedBy,
    opsItem_operationalData,
    opsItem_actualStartTime,
    opsItem_description,
    opsItem_plannedEndTime,
    opsItem_notifications,
    opsItem_plannedStartTime,

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
    opsItemEventSummary_opsItemId,
    opsItemEventSummary_createdTime,
    opsItemEventSummary_createdBy,
    opsItemEventSummary_detailType,
    opsItemEventSummary_source,
    opsItemEventSummary_detail,
    opsItemEventSummary_eventId,

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
    opsItemRelatedItemSummary_associationId,
    opsItemRelatedItemSummary_opsItemId,
    opsItemRelatedItemSummary_resourceUri,
    opsItemRelatedItemSummary_resourceType,
    opsItemRelatedItemSummary_createdTime,
    opsItemRelatedItemSummary_createdBy,
    opsItemRelatedItemSummary_lastModifiedTime,
    opsItemRelatedItemSummary_associationType,
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
    opsItemSummary_actualEndTime,
    opsItemSummary_opsItemId,
    opsItemSummary_status,
    opsItemSummary_priority,
    opsItemSummary_createdTime,
    opsItemSummary_category,
    opsItemSummary_severity,
    opsItemSummary_createdBy,
    opsItemSummary_lastModifiedTime,
    opsItemSummary_opsItemType,
    opsItemSummary_source,
    opsItemSummary_title,
    opsItemSummary_lastModifiedBy,
    opsItemSummary_operationalData,
    opsItemSummary_actualStartTime,
    opsItemSummary_plannedEndTime,
    opsItemSummary_plannedStartTime,

    -- * OpsMetadata
    OpsMetadata (..),
    newOpsMetadata,
    opsMetadata_opsMetadataArn,
    opsMetadata_resourceId,
    opsMetadata_lastModifiedDate,
    opsMetadata_lastModifiedUser,
    opsMetadata_creationDate,

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
    parameter_selector,
    parameter_arn,
    parameter_sourceResult,
    parameter_dataType,
    parameter_name,
    parameter_type,
    parameter_value,
    parameter_version,

    -- * ParameterHistory
    ParameterHistory (..),
    newParameterHistory,
    parameterHistory_lastModifiedDate,
    parameterHistory_keyId,
    parameterHistory_value,
    parameterHistory_name,
    parameterHistory_tier,
    parameterHistory_version,
    parameterHistory_lastModifiedUser,
    parameterHistory_labels,
    parameterHistory_allowedPattern,
    parameterHistory_type,
    parameterHistory_dataType,
    parameterHistory_description,
    parameterHistory_policies,

    -- * ParameterInlinePolicy
    ParameterInlinePolicy (..),
    newParameterInlinePolicy,
    parameterInlinePolicy_policyType,
    parameterInlinePolicy_policyStatus,
    parameterInlinePolicy_policyText,

    -- * ParameterMetadata
    ParameterMetadata (..),
    newParameterMetadata,
    parameterMetadata_lastModifiedDate,
    parameterMetadata_keyId,
    parameterMetadata_name,
    parameterMetadata_tier,
    parameterMetadata_version,
    parameterMetadata_lastModifiedUser,
    parameterMetadata_allowedPattern,
    parameterMetadata_type,
    parameterMetadata_dataType,
    parameterMetadata_description,
    parameterMetadata_policies,

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
    patch_bugzillaIds,
    patch_vendor,
    patch_msrcSeverity,
    patch_repository,
    patch_productFamily,
    patch_severity,
    patch_advisoryIds,
    patch_cVEIds,
    patch_classification,
    patch_release,
    patch_msrcNumber,
    patch_name,
    patch_version,
    patch_language,
    patch_kbNumber,
    patch_contentUrl,
    patch_id,
    patch_releaseDate,
    patch_title,
    patch_arch,
    patch_product,
    patch_description,
    patch_epoch,

    -- * PatchBaselineIdentity
    PatchBaselineIdentity (..),
    newPatchBaselineIdentity,
    patchBaselineIdentity_baselineName,
    patchBaselineIdentity_baselineDescription,
    patchBaselineIdentity_operatingSystem,
    patchBaselineIdentity_defaultBaseline,
    patchBaselineIdentity_baselineId,

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
    patchOrchestratorFilter_values,
    patchOrchestratorFilter_key,

    -- * PatchRule
    PatchRule (..),
    newPatchRule,
    patchRule_approveAfterDays,
    patchRule_approveUntilDate,
    patchRule_enableNonSecurity,
    patchRule_complianceLevel,
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
    patchStatus_deploymentStatus,
    patchStatus_complianceLevel,

    -- * ProgressCounters
    ProgressCounters (..),
    newProgressCounters,
    progressCounters_failedSteps,
    progressCounters_cancelledSteps,
    progressCounters_successSteps,
    progressCounters_totalSteps,
    progressCounters_timedOutSteps,

    -- * RelatedOpsItem
    RelatedOpsItem (..),
    newRelatedOpsItem,
    relatedOpsItem_opsItemId,

    -- * ResolvedTargets
    ResolvedTargets (..),
    newResolvedTargets,
    resolvedTargets_truncated,
    resolvedTargets_parameterValues,

    -- * ResourceComplianceSummaryItem
    ResourceComplianceSummaryItem (..),
    newResourceComplianceSummaryItem,
    resourceComplianceSummaryItem_nonCompliantSummary,
    resourceComplianceSummaryItem_status,
    resourceComplianceSummaryItem_resourceId,
    resourceComplianceSummaryItem_resourceType,
    resourceComplianceSummaryItem_compliantSummary,
    resourceComplianceSummaryItem_executionSummary,
    resourceComplianceSummaryItem_overallSeverity,
    resourceComplianceSummaryItem_complianceType,

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
    resourceDataSyncItem_syncSource,
    resourceDataSyncItem_lastSyncStatusMessage,
    resourceDataSyncItem_syncCreatedTime,
    resourceDataSyncItem_lastSyncTime,
    resourceDataSyncItem_syncName,
    resourceDataSyncItem_lastStatus,
    resourceDataSyncItem_syncLastModifiedTime,
    resourceDataSyncItem_s3Destination,
    resourceDataSyncItem_lastSuccessfulSyncTime,

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
    resourceDataSyncSource_enableAllOpsDataSources,
    resourceDataSyncSource_includeFutureRegions,
    resourceDataSyncSource_awsOrganizationsSource,
    resourceDataSyncSource_sourceType,
    resourceDataSyncSource_sourceRegions,

    -- * ResourceDataSyncSourceWithState
    ResourceDataSyncSourceWithState (..),
    newResourceDataSyncSourceWithState,
    resourceDataSyncSourceWithState_state,
    resourceDataSyncSourceWithState_enableAllOpsDataSources,
    resourceDataSyncSourceWithState_includeFutureRegions,
    resourceDataSyncSourceWithState_sourceType,
    resourceDataSyncSourceWithState_awsOrganizationsSource,
    resourceDataSyncSourceWithState_sourceRegions,

    -- * ResultAttribute
    ResultAttribute (..),
    newResultAttribute,
    resultAttribute_typeName,

    -- * ReviewInformation
    ReviewInformation (..),
    newReviewInformation,
    reviewInformation_status,
    reviewInformation_reviewer,
    reviewInformation_reviewedTime,

    -- * Runbook
    Runbook (..),
    newRunbook,
    runbook_targetParameterName,
    runbook_targetLocations,
    runbook_maxErrors,
    runbook_targets,
    runbook_parameters,
    runbook_documentVersion,
    runbook_maxConcurrency,
    runbook_documentName,

    -- * S3OutputLocation
    S3OutputLocation (..),
    newS3OutputLocation,
    s3OutputLocation_outputS3KeyPrefix,
    s3OutputLocation_outputS3Region,
    s3OutputLocation_outputS3BucketName,

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
    serviceSetting_status,
    serviceSetting_lastModifiedDate,
    serviceSetting_arn,
    serviceSetting_settingId,
    serviceSetting_lastModifiedUser,
    serviceSetting_settingValue,

    -- * Session
    Session (..),
    newSession,
    session_status,
    session_outputUrl,
    session_documentName,
    session_endDate,
    session_owner,
    session_startDate,
    session_details,
    session_sessionId,
    session_target,

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
    severitySummary_unspecifiedCount,
    severitySummary_highCount,
    severitySummary_mediumCount,
    severitySummary_informationalCount,
    severitySummary_criticalCount,

    -- * StepExecution
    StepExecution (..),
    newStepExecution,
    stepExecution_failureDetails,
    stepExecution_isEnd,
    stepExecution_inputs,
    stepExecution_stepName,
    stepExecution_executionEndTime,
    stepExecution_failureMessage,
    stepExecution_response,
    stepExecution_action,
    stepExecution_responseCode,
    stepExecution_stepStatus,
    stepExecution_targetLocation,
    stepExecution_overriddenParameters,
    stepExecution_outputs,
    stepExecution_executionStartTime,
    stepExecution_maxAttempts,
    stepExecution_targets,
    stepExecution_nextStep,
    stepExecution_stepExecutionId,
    stepExecution_validNextSteps,
    stepExecution_timeoutSeconds,
    stepExecution_onFailure,
    stepExecution_isCritical,

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
    target_values,
    target_key,

    -- * TargetLocation
    TargetLocation (..),
    newTargetLocation,
    targetLocation_accounts,
    targetLocation_targetLocationMaxConcurrency,
    targetLocation_targetLocationMaxErrors,
    targetLocation_regions,
    targetLocation_executionRoleName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.AccountSharingInfo
import Amazonka.SSM.Types.Activation
import Amazonka.SSM.Types.Association
import Amazonka.SSM.Types.AssociationComplianceSeverity
import Amazonka.SSM.Types.AssociationDescription
import Amazonka.SSM.Types.AssociationExecution
import Amazonka.SSM.Types.AssociationExecutionFilter
import Amazonka.SSM.Types.AssociationExecutionFilterKey
import Amazonka.SSM.Types.AssociationExecutionTarget
import Amazonka.SSM.Types.AssociationExecutionTargetsFilter
import Amazonka.SSM.Types.AssociationExecutionTargetsFilterKey
import Amazonka.SSM.Types.AssociationFilter
import Amazonka.SSM.Types.AssociationFilterKey
import Amazonka.SSM.Types.AssociationFilterOperatorType
import Amazonka.SSM.Types.AssociationOverview
import Amazonka.SSM.Types.AssociationStatus
import Amazonka.SSM.Types.AssociationStatusName
import Amazonka.SSM.Types.AssociationSyncCompliance
import Amazonka.SSM.Types.AssociationVersionInfo
import Amazonka.SSM.Types.AttachmentContent
import Amazonka.SSM.Types.AttachmentHashType
import Amazonka.SSM.Types.AttachmentInformation
import Amazonka.SSM.Types.AttachmentsSource
import Amazonka.SSM.Types.AttachmentsSourceKey
import Amazonka.SSM.Types.AutomationExecution
import Amazonka.SSM.Types.AutomationExecutionFilter
import Amazonka.SSM.Types.AutomationExecutionFilterKey
import Amazonka.SSM.Types.AutomationExecutionMetadata
import Amazonka.SSM.Types.AutomationExecutionStatus
import Amazonka.SSM.Types.AutomationSubtype
import Amazonka.SSM.Types.AutomationType
import Amazonka.SSM.Types.BaselineOverride
import Amazonka.SSM.Types.CalendarState
import Amazonka.SSM.Types.CloudWatchOutputConfig
import Amazonka.SSM.Types.Command
import Amazonka.SSM.Types.CommandFilter
import Amazonka.SSM.Types.CommandFilterKey
import Amazonka.SSM.Types.CommandInvocation
import Amazonka.SSM.Types.CommandInvocationStatus
import Amazonka.SSM.Types.CommandPlugin
import Amazonka.SSM.Types.CommandPluginStatus
import Amazonka.SSM.Types.CommandStatus
import Amazonka.SSM.Types.ComplianceExecutionSummary
import Amazonka.SSM.Types.ComplianceItem
import Amazonka.SSM.Types.ComplianceItemEntry
import Amazonka.SSM.Types.ComplianceQueryOperatorType
import Amazonka.SSM.Types.ComplianceSeverity
import Amazonka.SSM.Types.ComplianceStatus
import Amazonka.SSM.Types.ComplianceStringFilter
import Amazonka.SSM.Types.ComplianceSummaryItem
import Amazonka.SSM.Types.ComplianceUploadType
import Amazonka.SSM.Types.CompliantSummary
import Amazonka.SSM.Types.ConnectionStatus
import Amazonka.SSM.Types.CreateAssociationBatchRequestEntry
import Amazonka.SSM.Types.DescribeActivationsFilter
import Amazonka.SSM.Types.DescribeActivationsFilterKeys
import Amazonka.SSM.Types.DocumentDefaultVersionDescription
import Amazonka.SSM.Types.DocumentDescription
import Amazonka.SSM.Types.DocumentFilter
import Amazonka.SSM.Types.DocumentFilterKey
import Amazonka.SSM.Types.DocumentFormat
import Amazonka.SSM.Types.DocumentHashType
import Amazonka.SSM.Types.DocumentIdentifier
import Amazonka.SSM.Types.DocumentKeyValuesFilter
import Amazonka.SSM.Types.DocumentMetadataEnum
import Amazonka.SSM.Types.DocumentMetadataResponseInfo
import Amazonka.SSM.Types.DocumentParameter
import Amazonka.SSM.Types.DocumentParameterType
import Amazonka.SSM.Types.DocumentPermissionType
import Amazonka.SSM.Types.DocumentRequires
import Amazonka.SSM.Types.DocumentReviewAction
import Amazonka.SSM.Types.DocumentReviewCommentSource
import Amazonka.SSM.Types.DocumentReviewCommentType
import Amazonka.SSM.Types.DocumentReviewerResponseSource
import Amazonka.SSM.Types.DocumentReviews
import Amazonka.SSM.Types.DocumentStatus
import Amazonka.SSM.Types.DocumentType
import Amazonka.SSM.Types.DocumentVersionInfo
import Amazonka.SSM.Types.EffectivePatch
import Amazonka.SSM.Types.ExecutionMode
import Amazonka.SSM.Types.FailedCreateAssociation
import Amazonka.SSM.Types.FailureDetails
import Amazonka.SSM.Types.Fault
import Amazonka.SSM.Types.InstanceAggregatedAssociationOverview
import Amazonka.SSM.Types.InstanceAssociation
import Amazonka.SSM.Types.InstanceAssociationOutputLocation
import Amazonka.SSM.Types.InstanceAssociationOutputUrl
import Amazonka.SSM.Types.InstanceAssociationStatusInfo
import Amazonka.SSM.Types.InstanceInformation
import Amazonka.SSM.Types.InstanceInformationFilter
import Amazonka.SSM.Types.InstanceInformationFilterKey
import Amazonka.SSM.Types.InstanceInformationStringFilter
import Amazonka.SSM.Types.InstancePatchState
import Amazonka.SSM.Types.InstancePatchStateFilter
import Amazonka.SSM.Types.InstancePatchStateOperatorType
import Amazonka.SSM.Types.InventoryAggregator
import Amazonka.SSM.Types.InventoryAttributeDataType
import Amazonka.SSM.Types.InventoryDeletionStatus
import Amazonka.SSM.Types.InventoryDeletionStatusItem
import Amazonka.SSM.Types.InventoryDeletionSummary
import Amazonka.SSM.Types.InventoryDeletionSummaryItem
import Amazonka.SSM.Types.InventoryFilter
import Amazonka.SSM.Types.InventoryGroup
import Amazonka.SSM.Types.InventoryItem
import Amazonka.SSM.Types.InventoryItemAttribute
import Amazonka.SSM.Types.InventoryItemSchema
import Amazonka.SSM.Types.InventoryQueryOperatorType
import Amazonka.SSM.Types.InventoryResultEntity
import Amazonka.SSM.Types.InventoryResultItem
import Amazonka.SSM.Types.InventorySchemaDeleteOption
import Amazonka.SSM.Types.LastResourceDataSyncStatus
import Amazonka.SSM.Types.LoggingInfo
import Amazonka.SSM.Types.MaintenanceWindowAutomationParameters
import Amazonka.SSM.Types.MaintenanceWindowExecution
import Amazonka.SSM.Types.MaintenanceWindowExecutionStatus
import Amazonka.SSM.Types.MaintenanceWindowExecutionTaskIdentity
import Amazonka.SSM.Types.MaintenanceWindowExecutionTaskInvocationIdentity
import Amazonka.SSM.Types.MaintenanceWindowFilter
import Amazonka.SSM.Types.MaintenanceWindowIdentity
import Amazonka.SSM.Types.MaintenanceWindowIdentityForTarget
import Amazonka.SSM.Types.MaintenanceWindowLambdaParameters
import Amazonka.SSM.Types.MaintenanceWindowResourceType
import Amazonka.SSM.Types.MaintenanceWindowRunCommandParameters
import Amazonka.SSM.Types.MaintenanceWindowStepFunctionsParameters
import Amazonka.SSM.Types.MaintenanceWindowTarget
import Amazonka.SSM.Types.MaintenanceWindowTask
import Amazonka.SSM.Types.MaintenanceWindowTaskCutoffBehavior
import Amazonka.SSM.Types.MaintenanceWindowTaskInvocationParameters
import Amazonka.SSM.Types.MaintenanceWindowTaskParameterValueExpression
import Amazonka.SSM.Types.MaintenanceWindowTaskType
import Amazonka.SSM.Types.MetadataValue
import Amazonka.SSM.Types.NonCompliantSummary
import Amazonka.SSM.Types.NotificationConfig
import Amazonka.SSM.Types.NotificationEvent
import Amazonka.SSM.Types.NotificationType
import Amazonka.SSM.Types.OperatingSystem
import Amazonka.SSM.Types.OpsAggregator
import Amazonka.SSM.Types.OpsEntity
import Amazonka.SSM.Types.OpsEntityItem
import Amazonka.SSM.Types.OpsFilter
import Amazonka.SSM.Types.OpsFilterOperatorType
import Amazonka.SSM.Types.OpsItem
import Amazonka.SSM.Types.OpsItemDataType
import Amazonka.SSM.Types.OpsItemDataValue
import Amazonka.SSM.Types.OpsItemEventFilter
import Amazonka.SSM.Types.OpsItemEventFilterKey
import Amazonka.SSM.Types.OpsItemEventFilterOperator
import Amazonka.SSM.Types.OpsItemEventSummary
import Amazonka.SSM.Types.OpsItemFilter
import Amazonka.SSM.Types.OpsItemFilterKey
import Amazonka.SSM.Types.OpsItemFilterOperator
import Amazonka.SSM.Types.OpsItemIdentity
import Amazonka.SSM.Types.OpsItemNotification
import Amazonka.SSM.Types.OpsItemRelatedItemSummary
import Amazonka.SSM.Types.OpsItemRelatedItemsFilter
import Amazonka.SSM.Types.OpsItemRelatedItemsFilterKey
import Amazonka.SSM.Types.OpsItemRelatedItemsFilterOperator
import Amazonka.SSM.Types.OpsItemStatus
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
import Amazonka.SSM.Types.ParameterTier
import Amazonka.SSM.Types.ParameterType
import Amazonka.SSM.Types.ParametersFilter
import Amazonka.SSM.Types.ParametersFilterKey
import Amazonka.SSM.Types.Patch
import Amazonka.SSM.Types.PatchAction
import Amazonka.SSM.Types.PatchBaselineIdentity
import Amazonka.SSM.Types.PatchComplianceData
import Amazonka.SSM.Types.PatchComplianceDataState
import Amazonka.SSM.Types.PatchComplianceLevel
import Amazonka.SSM.Types.PatchDeploymentStatus
import Amazonka.SSM.Types.PatchFilter
import Amazonka.SSM.Types.PatchFilterGroup
import Amazonka.SSM.Types.PatchFilterKey
import Amazonka.SSM.Types.PatchGroupPatchBaselineMapping
import Amazonka.SSM.Types.PatchOperationType
import Amazonka.SSM.Types.PatchOrchestratorFilter
import Amazonka.SSM.Types.PatchProperty
import Amazonka.SSM.Types.PatchRule
import Amazonka.SSM.Types.PatchRuleGroup
import Amazonka.SSM.Types.PatchSet
import Amazonka.SSM.Types.PatchSource
import Amazonka.SSM.Types.PatchStatus
import Amazonka.SSM.Types.PingStatus
import Amazonka.SSM.Types.PlatformType
import Amazonka.SSM.Types.ProgressCounters
import Amazonka.SSM.Types.RebootOption
import Amazonka.SSM.Types.RelatedOpsItem
import Amazonka.SSM.Types.ResolvedTargets
import Amazonka.SSM.Types.ResourceComplianceSummaryItem
import Amazonka.SSM.Types.ResourceDataSyncAwsOrganizationsSource
import Amazonka.SSM.Types.ResourceDataSyncDestinationDataSharing
import Amazonka.SSM.Types.ResourceDataSyncItem
import Amazonka.SSM.Types.ResourceDataSyncOrganizationalUnit
import Amazonka.SSM.Types.ResourceDataSyncS3Destination
import Amazonka.SSM.Types.ResourceDataSyncS3Format
import Amazonka.SSM.Types.ResourceDataSyncSource
import Amazonka.SSM.Types.ResourceDataSyncSourceWithState
import Amazonka.SSM.Types.ResourceType
import Amazonka.SSM.Types.ResourceTypeForTagging
import Amazonka.SSM.Types.ResultAttribute
import Amazonka.SSM.Types.ReviewInformation
import Amazonka.SSM.Types.ReviewStatus
import Amazonka.SSM.Types.Runbook
import Amazonka.SSM.Types.S3OutputLocation
import Amazonka.SSM.Types.S3OutputUrl
import Amazonka.SSM.Types.ScheduledWindowExecution
import Amazonka.SSM.Types.ServiceSetting
import Amazonka.SSM.Types.Session
import Amazonka.SSM.Types.SessionFilter
import Amazonka.SSM.Types.SessionFilterKey
import Amazonka.SSM.Types.SessionManagerOutputUrl
import Amazonka.SSM.Types.SessionState
import Amazonka.SSM.Types.SessionStatus
import Amazonka.SSM.Types.SeveritySummary
import Amazonka.SSM.Types.SignalType
import Amazonka.SSM.Types.StepExecution
import Amazonka.SSM.Types.StepExecutionFilter
import Amazonka.SSM.Types.StepExecutionFilterKey
import Amazonka.SSM.Types.StopType
import Amazonka.SSM.Types.Tag
import Amazonka.SSM.Types.Target
import Amazonka.SSM.Types.TargetLocation
import qualified Amazonka.Sign.V4 as Sign

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
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
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
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | An Automation runbook with the specified name and version couldn\'t be
-- found.
_AutomationDefinitionVersionNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AutomationDefinitionVersionNotFoundException =
  Core._MatchServiceError
    defaultService
    "AutomationDefinitionVersionNotFoundException"

-- | The document version isn\'t valid or doesn\'t exist.
_InvalidDocumentVersion :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDocumentVersion =
  Core._MatchServiceError
    defaultService
    "InvalidDocumentVersion"

-- | Parameter Store doesn\'t support changing a parameter type in a
-- hierarchy. For example, you can\'t change a parameter from a @String@
-- type to a @SecureString@ type. You must create a new, unique parameter.
_HierarchyTypeMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_HierarchyTypeMismatchException =
  Core._MatchServiceError
    defaultService
    "HierarchyTypeMismatchException"

-- | The schedule is invalid. Verify your cron or rate expression and try
-- again.
_InvalidSchedule :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSchedule =
  Core._MatchServiceError
    defaultService
    "InvalidSchedule"

-- | The parameter type isn\'t supported.
_UnsupportedParameterType :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedParameterType =
  Core._MatchServiceError
    defaultService
    "UnsupportedParameterType"

-- | The specified update status operation isn\'t valid.
_InvalidAutomationStatusUpdateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAutomationStatusUpdateException =
  Core._MatchServiceError
    defaultService
    "InvalidAutomationStatusUpdateException"

-- | The OpsMetadata object exceeds the maximum number of OpsMetadata keys
-- that you can assign to an application in Application Manager.
_OpsMetadataKeyLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsMetadataKeyLimitExceededException =
  Core._MatchServiceError
    defaultService
    "OpsMetadataKeyLimitExceededException"

-- | The plugin name isn\'t valid.
_InvalidPluginName :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPluginName =
  Core._MatchServiceError
    defaultService
    "InvalidPluginName"

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

-- | The specified aggregator isn\'t valid for inventory groups. Verify that
-- the aggregator uses a valid inventory type such as @AWS:Application@ or
-- @AWS:InstanceInformation@.
_InvalidAggregatorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAggregatorException =
  Core._MatchServiceError
    defaultService
    "InvalidAggregatorException"

-- | You attempted to register a @LAMBDA@ or @STEP_FUNCTIONS@ task in a
-- region where the corresponding service isn\'t available.
_FeatureNotAvailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FeatureNotAvailableException =
  Core._MatchServiceError
    defaultService
    "FeatureNotAvailableException"

-- | The signal isn\'t valid for the current Automation execution.
_InvalidAutomationSignalException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAutomationSignalException =
  Core._MatchServiceError
    defaultService
    "InvalidAutomationSignalException"

-- | You have exceeded the allowed maximum sync configurations.
_ResourceDataSyncCountExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceDataSyncCountExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceDataSyncCountExceededException"

-- | The Amazon Resource Name (ARN) is already associated with the OpsItem.
_OpsItemRelatedItemAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsItemRelatedItemAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "OpsItemRelatedItemAlreadyExistsException"

-- | The document doesn\'t support the platform type of the given instance
-- ID(s). For example, you sent an document for a Windows instance to a
-- Linux instance.
_UnsupportedPlatformType :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedPlatformType =
  Core._MatchServiceError
    defaultService
    "UnsupportedPlatformType"

-- | The filter value isn\'t valid. Verify the value and try again.
_InvalidFilterValue :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFilterValue =
  Core._MatchServiceError
    defaultService
    "InvalidFilterValue"

-- | One or more content items isn\'t valid.
_InvalidItemContentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidItemContentException =
  Core._MatchServiceError
    defaultService
    "InvalidItemContentException"

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

-- | The permission type isn\'t supported. /Share/ is the only supported
-- permission type.
_InvalidPermissionType :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPermissionType =
  Core._MatchServiceError
    defaultService
    "InvalidPermissionType"

-- | You must disassociate a document from all instances before you can
-- delete it.
_AssociatedInstances :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AssociatedInstances =
  Core._MatchServiceError
    defaultService
    "AssociatedInstances"

-- | The operating systems you specified isn\'t supported, or the operation
-- isn\'t supported for the operating system.
_UnsupportedOperatingSystem :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperatingSystem =
  Core._MatchServiceError
    defaultService
    "UnsupportedOperatingSystem"

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

-- | The updated status is the same as the current status.
_StatusUnchanged :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StatusUnchanged =
  Core._MatchServiceError
    defaultService
    "StatusUnchanged"

-- | The specified token isn\'t valid.
_InvalidNextToken :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextToken =
  Core._MatchServiceError
    defaultService
    "InvalidNextToken"

-- | The request isn\'t valid.
_InvalidInventoryRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInventoryRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidInventoryRequestException"

-- | The association isn\'t valid or doesn\'t exist.
_InvalidAssociation :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAssociation =
  Core._MatchServiceError
    defaultService
    "InvalidAssociation"

-- | The OpsItem already exists.
_OpsItemAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsItemAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "OpsItemAlreadyExistsException"

-- | The S3 bucket doesn\'t exist.
_InvalidOutputFolder :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOutputFolder =
  Core._MatchServiceError
    defaultService
    "InvalidOutputFolder"

-- | An OpsMetadata object already exists for the selected resource.
_OpsMetadataAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsMetadataAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "OpsMetadataAlreadyExistsException"

-- | The request caused OpsItems to exceed one or more quotas. For
-- information about OpsItem quotas, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-learn-more.html#OpsCenter-learn-more-limits What are the resource limits for OpsCenter?>.
_OpsItemLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsItemLimitExceededException =
  Core._MatchServiceError
    defaultService
    "OpsItemLimitExceededException"

-- | The activation ID isn\'t valid. Verify the you entered the correct
-- ActivationId or ActivationCode and try again.
_InvalidActivationId :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidActivationId =
  Core._MatchServiceError
    defaultService
    "InvalidActivationId"

-- | Your account reached the maximum number of OpsMetadata objects allowed
-- by Application Manager. The maximum is 200 OpsMetadata objects. Delete
-- one or more OpsMetadata object and try again.
_OpsMetadataLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsMetadataLimitExceededException =
  Core._MatchServiceError
    defaultService
    "OpsMetadataLimitExceededException"

-- | The specified service setting wasn\'t found. Either the service name or
-- the setting hasn\'t been provisioned by the Amazon Web Services service
-- team.
_ServiceSettingNotFound :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceSettingNotFound =
  Core._MatchServiceError
    defaultService
    "ServiceSettingNotFound"

-- | The specified inventory item result attribute isn\'t valid.
_InvalidResultAttributeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResultAttributeException =
  Core._MatchServiceError
    defaultService
    "InvalidResultAttributeException"

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

-- | The system is processing too many concurrent updates. Wait a few moments
-- and try again.
_OpsMetadataTooManyUpdatesException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsMetadataTooManyUpdatesException =
  Core._MatchServiceError
    defaultService
    "OpsMetadataTooManyUpdatesException"

-- | A parameter version can have a maximum of ten labels.
_ParameterVersionLabelLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterVersionLabelLimitExceeded =
  Core._MatchServiceError
    defaultService
    "ParameterVersionLabelLimitExceeded"

-- | The specified sync configuration is invalid.
_ResourceDataSyncInvalidConfigurationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceDataSyncInvalidConfigurationException =
  Core._MatchServiceError
    defaultService
    "ResourceDataSyncInvalidConfigurationException"

-- | The specified command ID isn\'t valid. Verify the ID and try again.
_InvalidCommandId :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCommandId =
  Core._MatchServiceError
    defaultService
    "InvalidCommandId"

-- | You can\'t specify an instance ID in more than one association.
_DuplicateInstanceId :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateInstanceId =
  Core._MatchServiceError
    defaultService
    "DuplicateInstanceId"

-- | The resource type isn\'t valid. For example, if you are attempting to
-- tag an instance, the instance must be a registered, managed instance.
_InvalidResourceType :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResourceType =
  Core._MatchServiceError
    defaultService
    "InvalidResourceType"

-- | Inventory item type schema version has to match supported versions in
-- the service. Check output of GetInventorySchema to see the available
-- schema version for each type.
_UnsupportedInventorySchemaVersionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedInventorySchemaVersionException =
  Core._MatchServiceError
    defaultService
    "UnsupportedInventorySchemaVersionException"

-- | The specified SSM document doesn\'t exist.
_InvalidDocument :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDocument =
  Core._MatchServiceError
    defaultService
    "InvalidDocument"

-- | There is a conflict in the policies specified for this parameter. You
-- can\'t, for example, specify two Expiration policies for a parameter.
-- Review your policies, and try again.
_IncompatiblePolicyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IncompatiblePolicyException =
  Core._MatchServiceError
    defaultService
    "IncompatiblePolicyException"

-- | An Automation runbook with the specified name couldn\'t be found.
_AutomationDefinitionNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AutomationDefinitionNotFoundException =
  Core._MatchServiceError
    defaultService
    "AutomationDefinitionNotFoundException"

-- | The policy type isn\'t supported. Parameter Store supports the following
-- policy types: Expiration, ExpirationNotification, and
-- NoChangeNotification.
_InvalidPolicyTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPolicyTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidPolicyTypeException"

-- | The specified key isn\'t valid.
_InvalidFilterKey :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFilterKey =
  Core._MatchServiceError
    defaultService
    "InvalidFilterKey"

-- | The supplied parameters for invoking the specified Automation runbook
-- are incorrect. For example, they may not match the set of parameters
-- permitted for the specified Automation document.
_InvalidAutomationExecutionParametersException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAutomationExecutionParametersException =
  Core._MatchServiceError
    defaultService
    "InvalidAutomationExecutionParametersException"

-- | There is no automation execution information for the requested
-- automation execution ID.
_AutomationExecutionNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AutomationExecutionNotFoundException =
  Core._MatchServiceError
    defaultService
    "AutomationExecutionNotFoundException"

-- | The parameter type name isn\'t valid.
_InvalidTypeNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTypeNameException =
  Core._MatchServiceError
    defaultService
    "InvalidTypeNameException"

-- | The specified sync name wasn\'t found.
_ResourceDataSyncNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceDataSyncNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceDataSyncNotFoundException"

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

-- | The inventory item size has exceeded the size limit.
_ItemSizeLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ItemSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ItemSizeLimitExceededException"

-- | A sync configuration with the same name already exists.
_ResourceDataSyncAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceDataSyncAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceDataSyncAlreadyExistsException"

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

-- | Another @UpdateResourceDataSync@ request is being processed. Wait a few
-- minutes and try again.
_ResourceDataSyncConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceDataSyncConflictException =
  Core._MatchServiceError
    defaultService
    "ResourceDataSyncConflictException"

-- | One of the arguments passed is invalid.
_OpsMetadataInvalidArgumentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsMetadataInvalidArgumentException =
  Core._MatchServiceError
    defaultService
    "OpsMetadataInvalidArgumentException"

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

-- | The specified filter value isn\'t valid.
_InvalidInstanceInformationFilterValue :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInstanceInformationFilterValue =
  Core._MatchServiceError
    defaultService
    "InvalidInstanceInformationFilterValue"

-- | The inventory item has invalid content.
_ItemContentMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ItemContentMismatchException =
  Core._MatchServiceError
    defaultService
    "ItemContentMismatchException"

-- | The parameter already exists. You can\'t create duplicate parameters.
_ParameterAlreadyExists :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterAlreadyExists =
  Core._MatchServiceError
    defaultService
    "ParameterAlreadyExists"

-- | The specified association already exists.
_AssociationAlreadyExists :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AssociationAlreadyExists =
  Core._MatchServiceError
    defaultService
    "AssociationAlreadyExists"

-- | You specified too many custom compliance types. You can specify a
-- maximum of 10 different types.
_ComplianceTypeCountLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ComplianceTypeCountLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ComplianceTypeCountLimitExceededException"

-- | One or more of the parameters specified for the delete operation isn\'t
-- valid. Verify all parameters and try again.
_InvalidDeleteInventoryParametersException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeleteInventoryParametersException =
  Core._MatchServiceError
    defaultService
    "InvalidDeleteInventoryParametersException"

-- | The ID specified for the delete operation doesn\'t exist or isn\'t
-- valid. Verify the ID and try again.
_InvalidDeletionIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeletionIdException =
  Core._MatchServiceError
    defaultService
    "InvalidDeletionIdException"

-- | You specified more than the maximum number of allowed policies for the
-- parameter. The maximum is 10.
_PoliciesLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PoliciesLimitExceededException =
  Core._MatchServiceError
    defaultService
    "PoliciesLimitExceededException"

-- | The content for the document isn\'t valid.
_InvalidDocumentContent :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDocumentContent =
  Core._MatchServiceError
    defaultService
    "InvalidDocumentContent"

-- | You have exceeded the number of parameters for this Amazon Web Services
-- account. Delete one or more parameters and try again.
_ParameterLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterLimitExceeded =
  Core._MatchServiceError
    defaultService
    "ParameterLimitExceeded"

-- | You can have at most 2,000 active associations.
_AssociationLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AssociationLimitExceeded =
  Core._MatchServiceError
    defaultService
    "AssociationLimitExceeded"

-- | The version you specified isn\'t valid. Use ListAssociationVersions to
-- view all versions of an association according to the association ID. Or,
-- use the @$LATEST@ parameter to view the latest version of the
-- association.
_InvalidAssociationVersion :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAssociationVersion =
  Core._MatchServiceError
    defaultService
    "InvalidAssociationVersion"

-- | The specified association doesn\'t exist.
_AssociationDoesNotExist :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AssociationDoesNotExist =
  Core._MatchServiceError
    defaultService
    "AssociationDoesNotExist"

-- | A policy attribute or its value is invalid.
_InvalidPolicyAttributeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPolicyAttributeException =
  Core._MatchServiceError
    defaultService
    "InvalidPolicyAttributeException"

-- | The parameter couldn\'t be found. Verify the name and try again.
_ParameterNotFound :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterNotFound =
  Core._MatchServiceError
    defaultService
    "ParameterNotFound"

-- | You specified the @Safe@ option for the
-- DeregisterTargetFromMaintenanceWindow operation, but the target is still
-- referenced in a task.
_TargetInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TargetInUseException =
  Core._MatchServiceError
    defaultService
    "TargetInUseException"

-- | An error occurred on the server side.
_InternalServerError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError
    defaultService
    "InternalServerError"

-- | The @Context@ attribute that you specified for the @InventoryItem@
-- isn\'t allowed for this inventory type. You can only use the @Context@
-- attribute with inventory types like @AWS:ComplianceItem@.
_UnsupportedInventoryItemContextException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedInventoryItemContextException =
  Core._MatchServiceError
    defaultService
    "UnsupportedInventoryItemContextException"

-- | The OpsMetadata object doesn\'t exist.
_OpsMetadataNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsMetadataNotFoundException =
  Core._MatchServiceError
    defaultService
    "OpsMetadataNotFoundException"

-- | You have reached the maximum number versions allowed for an association.
-- Each association has a limit of 1,000 versions.
_AssociationVersionLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AssociationVersionLimitExceeded =
  Core._MatchServiceError
    defaultService
    "AssociationVersionLimitExceeded"

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

-- | The specified OpsItem ID doesn\'t exist. Verify the ID and try again.
_OpsItemNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsItemNotFoundException =
  Core._MatchServiceError
    defaultService
    "OpsItemNotFoundException"

-- | The activation isn\'t valid. The activation might have been deleted, or
-- the ActivationId and the ActivationCode don\'t match.
_InvalidActivation :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidActivation =
  Core._MatchServiceError
    defaultService
    "InvalidActivation"

-- | The delete inventory option specified isn\'t valid. Verify the option
-- and try again.
_InvalidOptionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOptionException =
  Core._MatchServiceError
    defaultService
    "InvalidOptionException"

-- | The version of the document schema isn\'t supported.
_InvalidDocumentSchemaVersion :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDocumentSchemaVersion =
  Core._MatchServiceError
    defaultService
    "InvalidDocumentSchemaVersion"

-- | The size limit of a document is 64 KB.
_MaxDocumentSizeExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MaxDocumentSizeExceeded =
  Core._MatchServiceError
    defaultService
    "MaxDocumentSizeExceeded"

-- | The specified parameter version wasn\'t found. Verify the parameter name
-- and version, and try again.
_ParameterVersionNotFound :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterVersionNotFound =
  Core._MatchServiceError
    defaultService
    "ParameterVersionNotFound"

-- | The calendar entry contained in the specified SSM document isn\'t
-- supported.
_UnsupportedCalendarException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedCalendarException =
  Core._MatchServiceError
    defaultService
    "UnsupportedCalendarException"

-- | The update isn\'t valid.
_InvalidUpdate :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidUpdate =
  Core._MatchServiceError
    defaultService
    "InvalidUpdate"

-- | You have exceeded the limit for custom schemas. Delete one or more
-- custom schemas and try again.
_CustomSchemaCountLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CustomSchemaCountLimitExceededException =
  Core._MatchServiceError
    defaultService
    "CustomSchemaCountLimitExceededException"

-- | The specified execution ID doesn\'t exist. Verify the ID number and try
-- again.
_AssociationExecutionDoesNotExist :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AssociationExecutionDoesNotExist =
  Core._MatchServiceError
    defaultService
    "AssociationExecutionDoesNotExist"

-- | The target isn\'t valid or doesn\'t exist. It might not be configured
-- for Systems Manager or you might not have permission to perform the
-- operation.
_InvalidTarget :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTarget =
  Core._MatchServiceError
    defaultService
    "InvalidTarget"

-- | A hierarchy can have a maximum of 15 levels. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-parameter-name-constraints.html Requirements and constraints for parameter names>
-- in the /Amazon Web Services Systems Manager User Guide/.
_HierarchyLevelLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_HierarchyLevelLimitExceededException =
  Core._MatchServiceError
    defaultService
    "HierarchyLevelLimitExceededException"

-- | The specified inventory group isn\'t valid.
_InvalidInventoryGroupException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInventoryGroupException =
  Core._MatchServiceError
    defaultService
    "InvalidInventoryGroupException"

-- | You attempted to delete a document while it is still shared. You must
-- stop sharing the document before you can delete it.
_InvalidDocumentOperation :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDocumentOperation =
  Core._MatchServiceError
    defaultService
    "InvalidDocumentOperation"

-- | The command ID and instance ID you specified didn\'t match any
-- invocations. Verify the command ID and the instance ID and try again.
_InvocationDoesNotExist :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvocationDoesNotExist =
  Core._MatchServiceError
    defaultService
    "InvocationDoesNotExist"

-- | The document has too many versions. Delete one or more document versions
-- and try again.
_DocumentVersionLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DocumentVersionLimitExceeded =
  Core._MatchServiceError
    defaultService
    "DocumentVersionLimitExceeded"

-- | The output location isn\'t valid or doesn\'t exist.
_InvalidOutputLocation :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOutputLocation =
  Core._MatchServiceError
    defaultService
    "InvalidOutputLocation"

-- | The query key ID isn\'t valid.
_InvalidKeyId :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidKeyId =
  Core._MatchServiceError
    defaultService
    "InvalidKeyId"

-- | You must specify values for all required parameters in the Amazon Web
-- Services Systems Manager document (SSM document). You can only supply
-- values to parameters defined in the SSM document.
_InvalidParameters :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameters =
  Core._MatchServiceError
    defaultService
    "InvalidParameters"

-- | Indicates that the Change Manager change template used in the change
-- request was rejected or is still in a pending state.
_AutomationDefinitionNotApprovedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AutomationDefinitionNotApprovedException =
  Core._MatchServiceError
    defaultService
    "AutomationDefinitionNotApprovedException"

-- | A specified parameter argument isn\'t valid. Verify the available
-- arguments and try again.
_OpsItemInvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsItemInvalidParameterException =
  Core._MatchServiceError
    defaultService
    "OpsItemInvalidParameterException"

-- | The resource ID isn\'t valid. Verify that you entered the correct ID and
-- try again.
_InvalidResourceId :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResourceId =
  Core._MatchServiceError
    defaultService
    "InvalidResourceId"

-- | The request doesn\'t meet the regular expression requirement.
_InvalidAllowedPatternException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAllowedPatternException =
  Core._MatchServiceError
    defaultService
    "InvalidAllowedPatternException"

-- | One or more configuration items isn\'t valid. Verify that a valid Amazon
-- Resource Name (ARN) was provided for an Amazon Simple Notification
-- Service topic.
_InvalidNotificationConfig :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNotificationConfig =
  Core._MatchServiceError
    defaultService
    "InvalidNotificationConfig"

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

-- | The size of inventory data has exceeded the total size limit for the
-- resource.
_TotalSizeLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TotalSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TotalSizeLimitExceededException"

-- | The sub-type count exceeded the limit for the inventory type.
_SubTypeCountLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubTypeCountLimitExceededException =
  Core._MatchServiceError
    defaultService
    "SubTypeCountLimitExceededException"

-- | The SSM document type isn\'t valid. Valid document types are described
-- in the @DocumentType@ property.
_InvalidDocumentType :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDocumentType =
  Core._MatchServiceError
    defaultService
    "InvalidDocumentType"

-- | The @Targets@ parameter includes too many tags. Remove one or more tags
-- and try the command again.
_TooManyTagsError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsError =
  Core._MatchServiceError
    defaultService
    "TooManyTagsError"

-- | The document can\'t be shared with more Amazon Web Services user
-- accounts. You can share a document with a maximum of 20 accounts. You
-- can publicly share up to five documents. If you need to increase this
-- limit, contact Amazon Web Services Support.
_DocumentPermissionLimit :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DocumentPermissionLimit =
  Core._MatchServiceError
    defaultService
    "DocumentPermissionLimit"

-- | The specified step name and execution ID don\'t exist. Verify the
-- information and try again.
_AutomationStepNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AutomationStepNotFoundException =
  Core._MatchServiceError
    defaultService
    "AutomationStepNotFoundException"

-- | The content of the association document matches another document. Change
-- the content of the document and try again.
_DuplicateDocumentContent :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateDocumentContent =
  Core._MatchServiceError
    defaultService
    "DuplicateDocumentContent"

-- | The specified document already exists.
_DocumentAlreadyExists :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DocumentAlreadyExists =
  Core._MatchServiceError
    defaultService
    "DocumentAlreadyExists"

-- | You can have at most 500 active SSM documents.
_DocumentLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DocumentLimitExceeded =
  Core._MatchServiceError
    defaultService
    "DocumentLimitExceeded"

-- | Error returned if an attempt is made to register a patch group with a
-- patch baseline that is already registered with a different patch
-- baseline.
_AlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "AlreadyExistsException"

-- | The filter name isn\'t valid. Verify the you entered the correct name
-- and try again.
_InvalidFilter :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFilter =
  Core._MatchServiceError
    defaultService
    "InvalidFilter"

-- | Error returned if an attempt is made to delete a patch baseline that is
-- registered for a patch group.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
