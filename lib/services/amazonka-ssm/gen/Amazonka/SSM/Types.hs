{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSM.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AlreadyExistsException,
    _AssociatedInstances,
    _AssociationAlreadyExists,
    _AssociationDoesNotExist,
    _AssociationExecutionDoesNotExist,
    _AssociationLimitExceeded,
    _AssociationVersionLimitExceeded,
    _AutomationDefinitionNotApprovedException,
    _AutomationDefinitionNotFoundException,
    _AutomationDefinitionVersionNotFoundException,
    _AutomationExecutionLimitExceededException,
    _AutomationExecutionNotFoundException,
    _AutomationStepNotFoundException,
    _ComplianceTypeCountLimitExceededException,
    _CustomSchemaCountLimitExceededException,
    _DocumentAlreadyExists,
    _DocumentLimitExceeded,
    _DocumentPermissionLimit,
    _DocumentVersionLimitExceeded,
    _DoesNotExistException,
    _DuplicateDocumentContent,
    _DuplicateDocumentVersionName,
    _DuplicateInstanceId,
    _FeatureNotAvailableException,
    _HierarchyLevelLimitExceededException,
    _HierarchyTypeMismatchException,
    _IdempotentParameterMismatch,
    _IncompatiblePolicyException,
    _InternalServerError,
    _InvalidActivation,
    _InvalidActivationId,
    _InvalidAggregatorException,
    _InvalidAllowedPatternException,
    _InvalidAssociation,
    _InvalidAssociationVersion,
    _InvalidAutomationExecutionParametersException,
    _InvalidAutomationSignalException,
    _InvalidAutomationStatusUpdateException,
    _InvalidCommandId,
    _InvalidDeleteInventoryParametersException,
    _InvalidDeletionIdException,
    _InvalidDocument,
    _InvalidDocumentContent,
    _InvalidDocumentOperation,
    _InvalidDocumentSchemaVersion,
    _InvalidDocumentType,
    _InvalidDocumentVersion,
    _InvalidFilter,
    _InvalidFilterKey,
    _InvalidFilterOption,
    _InvalidFilterValue,
    _InvalidInstanceId,
    _InvalidInstanceInformationFilterValue,
    _InvalidInventoryGroupException,
    _InvalidInventoryItemContextException,
    _InvalidInventoryRequestException,
    _InvalidItemContentException,
    _InvalidKeyId,
    _InvalidNextToken,
    _InvalidNotificationConfig,
    _InvalidOptionException,
    _InvalidOutputFolder,
    _InvalidOutputLocation,
    _InvalidParameters,
    _InvalidPermissionType,
    _InvalidPluginName,
    _InvalidPolicyAttributeException,
    _InvalidPolicyTypeException,
    _InvalidResourceId,
    _InvalidResourceType,
    _InvalidResultAttributeException,
    _InvalidRole,
    _InvalidSchedule,
    _InvalidTag,
    _InvalidTarget,
    _InvalidTargetMaps,
    _InvalidTypeNameException,
    _InvalidUpdate,
    _InvocationDoesNotExist,
    _ItemContentMismatchException,
    _ItemSizeLimitExceededException,
    _MaxDocumentSizeExceeded,
    _OpsItemAccessDeniedException,
    _OpsItemAlreadyExistsException,
    _OpsItemInvalidParameterException,
    _OpsItemLimitExceededException,
    _OpsItemNotFoundException,
    _OpsItemRelatedItemAlreadyExistsException,
    _OpsItemRelatedItemAssociationNotFoundException,
    _OpsMetadataAlreadyExistsException,
    _OpsMetadataInvalidArgumentException,
    _OpsMetadataKeyLimitExceededException,
    _OpsMetadataLimitExceededException,
    _OpsMetadataNotFoundException,
    _OpsMetadataTooManyUpdatesException,
    _ParameterAlreadyExists,
    _ParameterLimitExceeded,
    _ParameterMaxVersionLimitExceeded,
    _ParameterNotFound,
    _ParameterPatternMismatchException,
    _ParameterVersionLabelLimitExceeded,
    _ParameterVersionNotFound,
    _PoliciesLimitExceededException,
    _ResourceDataSyncAlreadyExistsException,
    _ResourceDataSyncConflictException,
    _ResourceDataSyncCountExceededException,
    _ResourceDataSyncInvalidConfigurationException,
    _ResourceDataSyncNotFoundException,
    _ResourceInUseException,
    _ResourceLimitExceededException,
    _ResourcePolicyConflictException,
    _ResourcePolicyInvalidParameterException,
    _ResourcePolicyLimitExceededException,
    _ServiceSettingNotFound,
    _StatusUnchanged,
    _SubTypeCountLimitExceededException,
    _TargetInUseException,
    _TargetNotConnected,
    _TooManyTagsError,
    _TooManyUpdates,
    _TotalSizeLimitExceededException,
    _UnsupportedCalendarException,
    _UnsupportedFeatureRequiredException,
    _UnsupportedInventoryItemContextException,
    _UnsupportedInventorySchemaVersionException,
    _UnsupportedOperatingSystem,
    _UnsupportedParameterType,
    _UnsupportedPlatformType,

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

    -- * ExternalAlarmState
    ExternalAlarmState (..),

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

    -- * SourceType
    SourceType (..),

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

    -- * Alarm
    Alarm (..),
    newAlarm,
    alarm_name,

    -- * AlarmConfiguration
    AlarmConfiguration (..),
    newAlarmConfiguration,
    alarmConfiguration_ignorePollAlarmFailure,
    alarmConfiguration_alarms,

    -- * AlarmStateInformation
    AlarmStateInformation (..),
    newAlarmStateInformation,
    alarmStateInformation_name,
    alarmStateInformation_state,

    -- * Association
    Association (..),
    newAssociation,
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

    -- * AssociationDescription
    AssociationDescription (..),
    newAssociationDescription,
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

    -- * AssociationExecution
    AssociationExecution (..),
    newAssociationExecution,
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
    associationExecutionTarget_associationVersion,
    associationExecutionTarget_detailedStatus,
    associationExecutionTarget_executionId,
    associationExecutionTarget_lastExecutionDate,
    associationExecutionTarget_outputSource,
    associationExecutionTarget_resourceId,
    associationExecutionTarget_resourceType,
    associationExecutionTarget_status,

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
    associationOverview_associationStatusAggregatedCount,
    associationOverview_detailedStatus,
    associationOverview_status,

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

    -- * AttachmentContent
    AttachmentContent (..),
    newAttachmentContent,
    attachmentContent_hash,
    attachmentContent_hashType,
    attachmentContent_name,
    attachmentContent_size,
    attachmentContent_url,

    -- * AttachmentInformation
    AttachmentInformation (..),
    newAttachmentInformation,
    attachmentInformation_name,

    -- * AttachmentsSource
    AttachmentsSource (..),
    newAttachmentsSource,
    attachmentsSource_key,
    attachmentsSource_name,
    attachmentsSource_values,

    -- * AutomationExecution
    AutomationExecution (..),
    newAutomationExecution,
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

    -- * AutomationExecutionFilter
    AutomationExecutionFilter (..),
    newAutomationExecutionFilter,
    automationExecutionFilter_key,
    automationExecutionFilter_values,

    -- * AutomationExecutionMetadata
    AutomationExecutionMetadata (..),
    newAutomationExecutionMetadata,
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

    -- * BaselineOverride
    BaselineOverride (..),
    newBaselineOverride,
    baselineOverride_approvalRules,
    baselineOverride_approvedPatches,
    baselineOverride_approvedPatchesComplianceLevel,
    baselineOverride_approvedPatchesEnableNonSecurity,
    baselineOverride_globalFilters,
    baselineOverride_operatingSystem,
    baselineOverride_rejectedPatches,
    baselineOverride_rejectedPatchesAction,
    baselineOverride_sources,

    -- * CloudWatchOutputConfig
    CloudWatchOutputConfig (..),
    newCloudWatchOutputConfig,
    cloudWatchOutputConfig_cloudWatchLogGroupName,
    cloudWatchOutputConfig_cloudWatchOutputEnabled,

    -- * Command
    Command (..),
    newCommand,
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

    -- * CommandFilter
    CommandFilter (..),
    newCommandFilter,
    commandFilter_key,
    commandFilter_value,

    -- * CommandInvocation
    CommandInvocation (..),
    newCommandInvocation,
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

    -- * CommandPlugin
    CommandPlugin (..),
    newCommandPlugin,
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

    -- * ComplianceExecutionSummary
    ComplianceExecutionSummary (..),
    newComplianceExecutionSummary,
    complianceExecutionSummary_executionId,
    complianceExecutionSummary_executionType,
    complianceExecutionSummary_executionTime,

    -- * ComplianceItem
    ComplianceItem (..),
    newComplianceItem,
    complianceItem_complianceType,
    complianceItem_details,
    complianceItem_executionSummary,
    complianceItem_id,
    complianceItem_resourceId,
    complianceItem_resourceType,
    complianceItem_severity,
    complianceItem_status,
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
    complianceStringFilter_key,
    complianceStringFilter_type,
    complianceStringFilter_values,

    -- * ComplianceSummaryItem
    ComplianceSummaryItem (..),
    newComplianceSummaryItem,
    complianceSummaryItem_complianceType,
    complianceSummaryItem_compliantSummary,
    complianceSummaryItem_nonCompliantSummary,

    -- * CompliantSummary
    CompliantSummary (..),
    newCompliantSummary,
    compliantSummary_compliantCount,
    compliantSummary_severitySummary,

    -- * CreateAssociationBatchRequestEntry
    CreateAssociationBatchRequestEntry (..),
    newCreateAssociationBatchRequestEntry,
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

    -- * DescribeActivationsFilter
    DescribeActivationsFilter (..),
    newDescribeActivationsFilter,
    describeActivationsFilter_filterKey,
    describeActivationsFilter_filterValues,

    -- * DocumentDefaultVersionDescription
    DocumentDefaultVersionDescription (..),
    newDocumentDefaultVersionDescription,
    documentDefaultVersionDescription_defaultVersion,
    documentDefaultVersionDescription_defaultVersionName,
    documentDefaultVersionDescription_name,

    -- * DocumentDescription
    DocumentDescription (..),
    newDocumentDescription,
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

    -- * DocumentFilter
    DocumentFilter (..),
    newDocumentFilter,
    documentFilter_key,
    documentFilter_value,

    -- * DocumentIdentifier
    DocumentIdentifier (..),
    newDocumentIdentifier,
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
    documentParameter_defaultValue,
    documentParameter_description,
    documentParameter_name,
    documentParameter_type,

    -- * DocumentRequires
    DocumentRequires (..),
    newDocumentRequires,
    documentRequires_requireType,
    documentRequires_version,
    documentRequires_versionName,
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
    documentReviewerResponseSource_createTime,
    documentReviewerResponseSource_reviewStatus,
    documentReviewerResponseSource_reviewer,
    documentReviewerResponseSource_updatedTime,

    -- * DocumentReviews
    DocumentReviews (..),
    newDocumentReviews,
    documentReviews_comment,
    documentReviews_action,

    -- * DocumentVersionInfo
    DocumentVersionInfo (..),
    newDocumentVersionInfo,
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
    failureDetails_details,
    failureDetails_failureStage,
    failureDetails_failureType,

    -- * GetResourcePoliciesResponseEntry
    GetResourcePoliciesResponseEntry (..),
    newGetResourcePoliciesResponseEntry,
    getResourcePoliciesResponseEntry_policy,
    getResourcePoliciesResponseEntry_policyHash,
    getResourcePoliciesResponseEntry_policyId,

    -- * InstanceAggregatedAssociationOverview
    InstanceAggregatedAssociationOverview (..),
    newInstanceAggregatedAssociationOverview,
    instanceAggregatedAssociationOverview_detailedStatus,
    instanceAggregatedAssociationOverview_instanceAssociationStatusAggregatedCount,

    -- * InstanceAssociation
    InstanceAssociation (..),
    newInstanceAssociation,
    instanceAssociation_associationId,
    instanceAssociation_associationVersion,
    instanceAssociation_content,
    instanceAssociation_instanceId,

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

    -- * InstanceInformation
    InstanceInformation (..),
    newInstanceInformation,
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

    -- * InstancePatchStateFilter
    InstancePatchStateFilter (..),
    newInstancePatchStateFilter,
    instancePatchStateFilter_key,
    instancePatchStateFilter_values,
    instancePatchStateFilter_type,

    -- * InventoryAggregator
    InventoryAggregator (..),
    newInventoryAggregator,
    inventoryAggregator_aggregators,
    inventoryAggregator_expression,
    inventoryAggregator_groups,

    -- * InventoryDeletionStatusItem
    InventoryDeletionStatusItem (..),
    newInventoryDeletionStatusItem,
    inventoryDeletionStatusItem_deletionId,
    inventoryDeletionStatusItem_deletionStartTime,
    inventoryDeletionStatusItem_deletionSummary,
    inventoryDeletionStatusItem_lastStatus,
    inventoryDeletionStatusItem_lastStatusMessage,
    inventoryDeletionStatusItem_lastStatusUpdateTime,
    inventoryDeletionStatusItem_typeName,

    -- * InventoryDeletionSummary
    InventoryDeletionSummary (..),
    newInventoryDeletionSummary,
    inventoryDeletionSummary_remainingCount,
    inventoryDeletionSummary_summaryItems,
    inventoryDeletionSummary_totalCount,

    -- * InventoryDeletionSummaryItem
    InventoryDeletionSummaryItem (..),
    newInventoryDeletionSummaryItem,
    inventoryDeletionSummaryItem_count,
    inventoryDeletionSummaryItem_remainingCount,
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
    inventoryItem_content,
    inventoryItem_contentHash,
    inventoryItem_context,
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
    inventoryItemSchema_displayName,
    inventoryItemSchema_version,
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
    maintenanceWindowExecution_endTime,
    maintenanceWindowExecution_startTime,
    maintenanceWindowExecution_status,
    maintenanceWindowExecution_statusDetails,
    maintenanceWindowExecution_windowExecutionId,
    maintenanceWindowExecution_windowId,

    -- * MaintenanceWindowExecutionTaskIdentity
    MaintenanceWindowExecutionTaskIdentity (..),
    newMaintenanceWindowExecutionTaskIdentity,
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

    -- * MaintenanceWindowExecutionTaskInvocationIdentity
    MaintenanceWindowExecutionTaskInvocationIdentity (..),
    newMaintenanceWindowExecutionTaskInvocationIdentity,
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

    -- * MaintenanceWindowFilter
    MaintenanceWindowFilter (..),
    newMaintenanceWindowFilter,
    maintenanceWindowFilter_key,
    maintenanceWindowFilter_values,

    -- * MaintenanceWindowIdentity
    MaintenanceWindowIdentity (..),
    newMaintenanceWindowIdentity,
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

    -- * MaintenanceWindowIdentityForTarget
    MaintenanceWindowIdentityForTarget (..),
    newMaintenanceWindowIdentityForTarget,
    maintenanceWindowIdentityForTarget_name,
    maintenanceWindowIdentityForTarget_windowId,

    -- * MaintenanceWindowLambdaParameters
    MaintenanceWindowLambdaParameters (..),
    newMaintenanceWindowLambdaParameters,
    maintenanceWindowLambdaParameters_clientContext,
    maintenanceWindowLambdaParameters_payload,
    maintenanceWindowLambdaParameters_qualifier,

    -- * MaintenanceWindowRunCommandParameters
    MaintenanceWindowRunCommandParameters (..),
    newMaintenanceWindowRunCommandParameters,
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

    -- * MaintenanceWindowStepFunctionsParameters
    MaintenanceWindowStepFunctionsParameters (..),
    newMaintenanceWindowStepFunctionsParameters,
    maintenanceWindowStepFunctionsParameters_input,
    maintenanceWindowStepFunctionsParameters_name,

    -- * MaintenanceWindowTarget
    MaintenanceWindowTarget (..),
    newMaintenanceWindowTarget,
    maintenanceWindowTarget_description,
    maintenanceWindowTarget_name,
    maintenanceWindowTarget_ownerInformation,
    maintenanceWindowTarget_resourceType,
    maintenanceWindowTarget_targets,
    maintenanceWindowTarget_windowId,
    maintenanceWindowTarget_windowTargetId,

    -- * MaintenanceWindowTask
    MaintenanceWindowTask (..),
    newMaintenanceWindowTask,
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
    nonCompliantSummary_nonCompliantCount,
    nonCompliantSummary_severitySummary,

    -- * NotificationConfig
    NotificationConfig (..),
    newNotificationConfig,
    notificationConfig_notificationArn,
    notificationConfig_notificationEvents,
    notificationConfig_notificationType,

    -- * OpsAggregator
    OpsAggregator (..),
    newOpsAggregator,
    opsAggregator_aggregatorType,
    opsAggregator_aggregators,
    opsAggregator_attributeName,
    opsAggregator_filters,
    opsAggregator_typeName,
    opsAggregator_values,

    -- * OpsEntity
    OpsEntity (..),
    newOpsEntity,
    opsEntity_data,
    opsEntity_id,

    -- * OpsEntityItem
    OpsEntityItem (..),
    newOpsEntityItem,
    opsEntityItem_captureTime,
    opsEntityItem_content,

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

    -- * OpsItemDataValue
    OpsItemDataValue (..),
    newOpsItemDataValue,
    opsItemDataValue_type,
    opsItemDataValue_value,

    -- * OpsItemEventFilter
    OpsItemEventFilter (..),
    newOpsItemEventFilter,
    opsItemEventFilter_key,
    opsItemEventFilter_values,
    opsItemEventFilter_operator,

    -- * OpsItemEventSummary
    OpsItemEventSummary (..),
    newOpsItemEventSummary,
    opsItemEventSummary_createdBy,
    opsItemEventSummary_createdTime,
    opsItemEventSummary_detail,
    opsItemEventSummary_detailType,
    opsItemEventSummary_eventId,
    opsItemEventSummary_opsItemId,
    opsItemEventSummary_source,

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
    opsItemRelatedItemSummary_associationType,
    opsItemRelatedItemSummary_createdBy,
    opsItemRelatedItemSummary_createdTime,
    opsItemRelatedItemSummary_lastModifiedBy,
    opsItemRelatedItemSummary_lastModifiedTime,
    opsItemRelatedItemSummary_opsItemId,
    opsItemRelatedItemSummary_resourceType,
    opsItemRelatedItemSummary_resourceUri,

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

    -- * OpsMetadata
    OpsMetadata (..),
    newOpsMetadata,
    opsMetadata_creationDate,
    opsMetadata_lastModifiedDate,
    opsMetadata_lastModifiedUser,
    opsMetadata_opsMetadataArn,
    opsMetadata_resourceId,

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
    parameter_arn,
    parameter_dataType,
    parameter_lastModifiedDate,
    parameter_selector,
    parameter_sourceResult,
    parameter_name,
    parameter_type,
    parameter_value,
    parameter_version,

    -- * ParameterHistory
    ParameterHistory (..),
    newParameterHistory,
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

    -- * ParameterInlinePolicy
    ParameterInlinePolicy (..),
    newParameterInlinePolicy,
    parameterInlinePolicy_policyStatus,
    parameterInlinePolicy_policyText,
    parameterInlinePolicy_policyType,

    -- * ParameterMetadata
    ParameterMetadata (..),
    newParameterMetadata,
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

    -- * ParameterStringFilter
    ParameterStringFilter (..),
    newParameterStringFilter,
    parameterStringFilter_option,
    parameterStringFilter_values,
    parameterStringFilter_key,

    -- * ParametersFilter
    ParametersFilter (..),
    newParametersFilter,
    parametersFilter_key,
    parametersFilter_values,

    -- * Patch
    Patch (..),
    newPatch,
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

    -- * PatchBaselineIdentity
    PatchBaselineIdentity (..),
    newPatchBaselineIdentity,
    patchBaselineIdentity_baselineDescription,
    patchBaselineIdentity_baselineId,
    patchBaselineIdentity_baselineName,
    patchBaselineIdentity_defaultBaseline,
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
    progressCounters_failedSteps,
    progressCounters_successSteps,
    progressCounters_timedOutSteps,
    progressCounters_totalSteps,

    -- * RegistrationMetadataItem
    RegistrationMetadataItem (..),
    newRegistrationMetadataItem,
    registrationMetadataItem_key,
    registrationMetadataItem_value,

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
    resourceComplianceSummaryItem_complianceType,
    resourceComplianceSummaryItem_compliantSummary,
    resourceComplianceSummaryItem_executionSummary,
    resourceComplianceSummaryItem_nonCompliantSummary,
    resourceComplianceSummaryItem_overallSeverity,
    resourceComplianceSummaryItem_resourceId,
    resourceComplianceSummaryItem_resourceType,
    resourceComplianceSummaryItem_status,

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

    -- * ResourceDataSyncOrganizationalUnit
    ResourceDataSyncOrganizationalUnit (..),
    newResourceDataSyncOrganizationalUnit,
    resourceDataSyncOrganizationalUnit_organizationalUnitId,

    -- * ResourceDataSyncS3Destination
    ResourceDataSyncS3Destination (..),
    newResourceDataSyncS3Destination,
    resourceDataSyncS3Destination_aWSKMSKeyARN,
    resourceDataSyncS3Destination_destinationDataSharing,
    resourceDataSyncS3Destination_prefix,
    resourceDataSyncS3Destination_bucketName,
    resourceDataSyncS3Destination_syncFormat,
    resourceDataSyncS3Destination_region,

    -- * ResourceDataSyncSource
    ResourceDataSyncSource (..),
    newResourceDataSyncSource,
    resourceDataSyncSource_awsOrganizationsSource,
    resourceDataSyncSource_enableAllOpsDataSources,
    resourceDataSyncSource_includeFutureRegions,
    resourceDataSyncSource_sourceType,
    resourceDataSyncSource_sourceRegions,

    -- * ResourceDataSyncSourceWithState
    ResourceDataSyncSourceWithState (..),
    newResourceDataSyncSourceWithState,
    resourceDataSyncSourceWithState_awsOrganizationsSource,
    resourceDataSyncSourceWithState_enableAllOpsDataSources,
    resourceDataSyncSourceWithState_includeFutureRegions,
    resourceDataSyncSourceWithState_sourceRegions,
    resourceDataSyncSourceWithState_sourceType,
    resourceDataSyncSourceWithState_state,

    -- * ResultAttribute
    ResultAttribute (..),
    newResultAttribute,
    resultAttribute_typeName,

    -- * ReviewInformation
    ReviewInformation (..),
    newReviewInformation,
    reviewInformation_reviewedTime,
    reviewInformation_reviewer,
    reviewInformation_status,

    -- * Runbook
    Runbook (..),
    newRunbook,
    runbook_documentVersion,
    runbook_maxConcurrency,
    runbook_maxErrors,
    runbook_parameters,
    runbook_targetLocations,
    runbook_targetMaps,
    runbook_targetParameterName,
    runbook_targets,
    runbook_documentName,

    -- * S3OutputLocation
    S3OutputLocation (..),
    newS3OutputLocation,
    s3OutputLocation_outputS3BucketName,
    s3OutputLocation_outputS3KeyPrefix,
    s3OutputLocation_outputS3Region,

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
    serviceSetting_arn,
    serviceSetting_lastModifiedDate,
    serviceSetting_lastModifiedUser,
    serviceSetting_settingId,
    serviceSetting_settingValue,
    serviceSetting_status,

    -- * Session
    Session (..),
    newSession,
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

    -- * SessionFilter
    SessionFilter (..),
    newSessionFilter,
    sessionFilter_key,
    sessionFilter_value,

    -- * SessionManagerOutputUrl
    SessionManagerOutputUrl (..),
    newSessionManagerOutputUrl,
    sessionManagerOutputUrl_cloudWatchOutputUrl,
    sessionManagerOutputUrl_s3OutputUrl,

    -- * SeveritySummary
    SeveritySummary (..),
    newSeveritySummary,
    severitySummary_criticalCount,
    severitySummary_highCount,
    severitySummary_informationalCount,
    severitySummary_lowCount,
    severitySummary_mediumCount,
    severitySummary_unspecifiedCount,

    -- * StepExecution
    StepExecution (..),
    newStepExecution,
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
    targetLocation_accounts,
    targetLocation_executionRoleName,
    targetLocation_regions,
    targetLocation_targetLocationAlarmConfiguration,
    targetLocation_targetLocationMaxConcurrency,
    targetLocation_targetLocationMaxErrors,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.AccountSharingInfo
import Amazonka.SSM.Types.Activation
import Amazonka.SSM.Types.Alarm
import Amazonka.SSM.Types.AlarmConfiguration
import Amazonka.SSM.Types.AlarmStateInformation
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
import Amazonka.SSM.Types.ExternalAlarmState
import Amazonka.SSM.Types.FailedCreateAssociation
import Amazonka.SSM.Types.FailureDetails
import Amazonka.SSM.Types.Fault
import Amazonka.SSM.Types.GetResourcePoliciesResponseEntry
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
import Amazonka.SSM.Types.RegistrationMetadataItem
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
import Amazonka.SSM.Types.SourceType
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
    { Core.abbrev = "SSM",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "ssm",
      Core.signingName = "ssm",
      Core.version = "2014-11-06",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "SSM",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | Error returned if an attempt is made to register a patch group with a
-- patch baseline that is already registered with a different patch
-- baseline.
_AlreadyExistsException :: Core.AsError a => Lens.Fold a Core.ServiceError
_AlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "AlreadyExistsException"

-- | You must disassociate a document from all managed nodes before you can
-- delete it.
_AssociatedInstances :: Core.AsError a => Lens.Fold a Core.ServiceError
_AssociatedInstances =
  Core._MatchServiceError
    defaultService
    "AssociatedInstances"

-- | The specified association already exists.
_AssociationAlreadyExists :: Core.AsError a => Lens.Fold a Core.ServiceError
_AssociationAlreadyExists =
  Core._MatchServiceError
    defaultService
    "AssociationAlreadyExists"

-- | The specified association doesn\'t exist.
_AssociationDoesNotExist :: Core.AsError a => Lens.Fold a Core.ServiceError
_AssociationDoesNotExist =
  Core._MatchServiceError
    defaultService
    "AssociationDoesNotExist"

-- | The specified execution ID doesn\'t exist. Verify the ID number and try
-- again.
_AssociationExecutionDoesNotExist :: Core.AsError a => Lens.Fold a Core.ServiceError
_AssociationExecutionDoesNotExist =
  Core._MatchServiceError
    defaultService
    "AssociationExecutionDoesNotExist"

-- | You can have at most 2,000 active associations.
_AssociationLimitExceeded :: Core.AsError a => Lens.Fold a Core.ServiceError
_AssociationLimitExceeded =
  Core._MatchServiceError
    defaultService
    "AssociationLimitExceeded"

-- | You have reached the maximum number versions allowed for an association.
-- Each association has a limit of 1,000 versions.
_AssociationVersionLimitExceeded :: Core.AsError a => Lens.Fold a Core.ServiceError
_AssociationVersionLimitExceeded =
  Core._MatchServiceError
    defaultService
    "AssociationVersionLimitExceeded"

-- | Indicates that the Change Manager change template used in the change
-- request was rejected or is still in a pending state.
_AutomationDefinitionNotApprovedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_AutomationDefinitionNotApprovedException =
  Core._MatchServiceError
    defaultService
    "AutomationDefinitionNotApprovedException"

-- | An Automation runbook with the specified name couldn\'t be found.
_AutomationDefinitionNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_AutomationDefinitionNotFoundException =
  Core._MatchServiceError
    defaultService
    "AutomationDefinitionNotFoundException"

-- | An Automation runbook with the specified name and version couldn\'t be
-- found.
_AutomationDefinitionVersionNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_AutomationDefinitionVersionNotFoundException =
  Core._MatchServiceError
    defaultService
    "AutomationDefinitionVersionNotFoundException"

-- | The number of simultaneously running Automation executions exceeded the
-- allowable limit.
_AutomationExecutionLimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_AutomationExecutionLimitExceededException =
  Core._MatchServiceError
    defaultService
    "AutomationExecutionLimitExceededException"

-- | There is no automation execution information for the requested
-- automation execution ID.
_AutomationExecutionNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_AutomationExecutionNotFoundException =
  Core._MatchServiceError
    defaultService
    "AutomationExecutionNotFoundException"

-- | The specified step name and execution ID don\'t exist. Verify the
-- information and try again.
_AutomationStepNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_AutomationStepNotFoundException =
  Core._MatchServiceError
    defaultService
    "AutomationStepNotFoundException"

-- | You specified too many custom compliance types. You can specify a
-- maximum of 10 different types.
_ComplianceTypeCountLimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ComplianceTypeCountLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ComplianceTypeCountLimitExceededException"

-- | You have exceeded the limit for custom schemas. Delete one or more
-- custom schemas and try again.
_CustomSchemaCountLimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_CustomSchemaCountLimitExceededException =
  Core._MatchServiceError
    defaultService
    "CustomSchemaCountLimitExceededException"

-- | The specified document already exists.
_DocumentAlreadyExists :: Core.AsError a => Lens.Fold a Core.ServiceError
_DocumentAlreadyExists =
  Core._MatchServiceError
    defaultService
    "DocumentAlreadyExists"

-- | You can have at most 500 active SSM documents.
_DocumentLimitExceeded :: Core.AsError a => Lens.Fold a Core.ServiceError
_DocumentLimitExceeded =
  Core._MatchServiceError
    defaultService
    "DocumentLimitExceeded"

-- | The document can\'t be shared with more Amazon Web Services user
-- accounts. You can specify a maximum of 20 accounts per API operation to
-- share a private document.
--
-- By default, you can share a private document with a maximum of 1,000
-- accounts and publicly share up to five documents.
--
-- If you need to increase the quota for privately or publicly shared
-- Systems Manager documents, contact Amazon Web Services Support.
_DocumentPermissionLimit :: Core.AsError a => Lens.Fold a Core.ServiceError
_DocumentPermissionLimit =
  Core._MatchServiceError
    defaultService
    "DocumentPermissionLimit"

-- | The document has too many versions. Delete one or more document versions
-- and try again.
_DocumentVersionLimitExceeded :: Core.AsError a => Lens.Fold a Core.ServiceError
_DocumentVersionLimitExceeded =
  Core._MatchServiceError
    defaultService
    "DocumentVersionLimitExceeded"

-- | Error returned when the ID specified for a resource, such as a
-- maintenance window or patch baseline, doesn\'t exist.
--
-- For information about resource quotas in Amazon Web Services Systems
-- Manager, see
-- <https://docs.aws.amazon.com/general/latest/gr/ssm.html#limits_ssm Systems Manager service quotas>
-- in the /Amazon Web Services General Reference/.
_DoesNotExistException :: Core.AsError a => Lens.Fold a Core.ServiceError
_DoesNotExistException =
  Core._MatchServiceError
    defaultService
    "DoesNotExistException"

-- | The content of the association document matches another document. Change
-- the content of the document and try again.
_DuplicateDocumentContent :: Core.AsError a => Lens.Fold a Core.ServiceError
_DuplicateDocumentContent =
  Core._MatchServiceError
    defaultService
    "DuplicateDocumentContent"

-- | The version name has already been used in this document. Specify a
-- different version name, and then try again.
_DuplicateDocumentVersionName :: Core.AsError a => Lens.Fold a Core.ServiceError
_DuplicateDocumentVersionName =
  Core._MatchServiceError
    defaultService
    "DuplicateDocumentVersionName"

-- | You can\'t specify a managed node ID in more than one association.
_DuplicateInstanceId :: Core.AsError a => Lens.Fold a Core.ServiceError
_DuplicateInstanceId =
  Core._MatchServiceError
    defaultService
    "DuplicateInstanceId"

-- | You attempted to register a @LAMBDA@ or @STEP_FUNCTIONS@ task in a
-- region where the corresponding service isn\'t available.
_FeatureNotAvailableException :: Core.AsError a => Lens.Fold a Core.ServiceError
_FeatureNotAvailableException =
  Core._MatchServiceError
    defaultService
    "FeatureNotAvailableException"

-- | A hierarchy can have a maximum of 15 levels. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-parameter-name-constraints.html Requirements and constraints for parameter names>
-- in the /Amazon Web Services Systems Manager User Guide/.
_HierarchyLevelLimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_HierarchyLevelLimitExceededException =
  Core._MatchServiceError
    defaultService
    "HierarchyLevelLimitExceededException"

-- | Parameter Store doesn\'t support changing a parameter type in a
-- hierarchy. For example, you can\'t change a parameter from a @String@
-- type to a @SecureString@ type. You must create a new, unique parameter.
_HierarchyTypeMismatchException :: Core.AsError a => Lens.Fold a Core.ServiceError
_HierarchyTypeMismatchException =
  Core._MatchServiceError
    defaultService
    "HierarchyTypeMismatchException"

-- | Error returned when an idempotent operation is retried and the
-- parameters don\'t match the original call to the API with the same
-- idempotency token.
_IdempotentParameterMismatch :: Core.AsError a => Lens.Fold a Core.ServiceError
_IdempotentParameterMismatch =
  Core._MatchServiceError
    defaultService
    "IdempotentParameterMismatch"

-- | There is a conflict in the policies specified for this parameter. You
-- can\'t, for example, specify two Expiration policies for a parameter.
-- Review your policies, and try again.
_IncompatiblePolicyException :: Core.AsError a => Lens.Fold a Core.ServiceError
_IncompatiblePolicyException =
  Core._MatchServiceError
    defaultService
    "IncompatiblePolicyException"

-- | An error occurred on the server side.
_InternalServerError :: Core.AsError a => Lens.Fold a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError
    defaultService
    "InternalServerError"

-- | The activation isn\'t valid. The activation might have been deleted, or
-- the ActivationId and the ActivationCode don\'t match.
_InvalidActivation :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidActivation =
  Core._MatchServiceError
    defaultService
    "InvalidActivation"

-- | The activation ID isn\'t valid. Verify the you entered the correct
-- ActivationId or ActivationCode and try again.
_InvalidActivationId :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidActivationId =
  Core._MatchServiceError
    defaultService
    "InvalidActivationId"

-- | The specified aggregator isn\'t valid for inventory groups. Verify that
-- the aggregator uses a valid inventory type such as @AWS:Application@ or
-- @AWS:InstanceInformation@.
_InvalidAggregatorException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidAggregatorException =
  Core._MatchServiceError
    defaultService
    "InvalidAggregatorException"

-- | The request doesn\'t meet the regular expression requirement.
_InvalidAllowedPatternException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidAllowedPatternException =
  Core._MatchServiceError
    defaultService
    "InvalidAllowedPatternException"

-- | The association isn\'t valid or doesn\'t exist.
_InvalidAssociation :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidAssociation =
  Core._MatchServiceError
    defaultService
    "InvalidAssociation"

-- | The version you specified isn\'t valid. Use ListAssociationVersions to
-- view all versions of an association according to the association ID. Or,
-- use the @$LATEST@ parameter to view the latest version of the
-- association.
_InvalidAssociationVersion :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidAssociationVersion =
  Core._MatchServiceError
    defaultService
    "InvalidAssociationVersion"

-- | The supplied parameters for invoking the specified Automation runbook
-- are incorrect. For example, they may not match the set of parameters
-- permitted for the specified Automation document.
_InvalidAutomationExecutionParametersException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidAutomationExecutionParametersException =
  Core._MatchServiceError
    defaultService
    "InvalidAutomationExecutionParametersException"

-- | The signal isn\'t valid for the current Automation execution.
_InvalidAutomationSignalException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidAutomationSignalException =
  Core._MatchServiceError
    defaultService
    "InvalidAutomationSignalException"

-- | The specified update status operation isn\'t valid.
_InvalidAutomationStatusUpdateException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidAutomationStatusUpdateException =
  Core._MatchServiceError
    defaultService
    "InvalidAutomationStatusUpdateException"

-- | The specified command ID isn\'t valid. Verify the ID and try again.
_InvalidCommandId :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidCommandId =
  Core._MatchServiceError
    defaultService
    "InvalidCommandId"

-- | One or more of the parameters specified for the delete operation isn\'t
-- valid. Verify all parameters and try again.
_InvalidDeleteInventoryParametersException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidDeleteInventoryParametersException =
  Core._MatchServiceError
    defaultService
    "InvalidDeleteInventoryParametersException"

-- | The ID specified for the delete operation doesn\'t exist or isn\'t
-- valid. Verify the ID and try again.
_InvalidDeletionIdException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidDeletionIdException =
  Core._MatchServiceError
    defaultService
    "InvalidDeletionIdException"

-- | The specified SSM document doesn\'t exist.
_InvalidDocument :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidDocument =
  Core._MatchServiceError
    defaultService
    "InvalidDocument"

-- | The content for the document isn\'t valid.
_InvalidDocumentContent :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidDocumentContent =
  Core._MatchServiceError
    defaultService
    "InvalidDocumentContent"

-- | You attempted to delete a document while it is still shared. You must
-- stop sharing the document before you can delete it.
_InvalidDocumentOperation :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidDocumentOperation =
  Core._MatchServiceError
    defaultService
    "InvalidDocumentOperation"

-- | The version of the document schema isn\'t supported.
_InvalidDocumentSchemaVersion :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidDocumentSchemaVersion =
  Core._MatchServiceError
    defaultService
    "InvalidDocumentSchemaVersion"

-- | The SSM document type isn\'t valid. Valid document types are described
-- in the @DocumentType@ property.
_InvalidDocumentType :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidDocumentType =
  Core._MatchServiceError
    defaultService
    "InvalidDocumentType"

-- | The document version isn\'t valid or doesn\'t exist.
_InvalidDocumentVersion :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidDocumentVersion =
  Core._MatchServiceError
    defaultService
    "InvalidDocumentVersion"

-- | The filter name isn\'t valid. Verify the you entered the correct name
-- and try again.
_InvalidFilter :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidFilter =
  Core._MatchServiceError
    defaultService
    "InvalidFilter"

-- | The specified key isn\'t valid.
_InvalidFilterKey :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidFilterKey =
  Core._MatchServiceError
    defaultService
    "InvalidFilterKey"

-- | The specified filter option isn\'t valid. Valid options are Equals and
-- BeginsWith. For Path filter, valid options are Recursive and OneLevel.
_InvalidFilterOption :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidFilterOption =
  Core._MatchServiceError
    defaultService
    "InvalidFilterOption"

-- | The filter value isn\'t valid. Verify the value and try again.
_InvalidFilterValue :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidFilterValue =
  Core._MatchServiceError
    defaultService
    "InvalidFilterValue"

-- | The following problems can cause this exception:
--
-- -   You don\'t have permission to access the managed node.
--
-- -   Amazon Web Services Systems Manager Agent(SSM Agent) isn\'t running.
--     Verify that SSM Agent is running.
--
-- -   SSM Agent isn\'t registered with the SSM endpoint. Try reinstalling
--     SSM Agent.
--
-- -   The managed node isn\'t in valid state. Valid states are: @Running@,
--     @Pending@, @Stopped@, and @Stopping@. Invalid states are:
--     @Shutting-down@ and @Terminated@.
_InvalidInstanceId :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidInstanceId =
  Core._MatchServiceError
    defaultService
    "InvalidInstanceId"

-- | The specified filter value isn\'t valid.
_InvalidInstanceInformationFilterValue :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidInstanceInformationFilterValue =
  Core._MatchServiceError
    defaultService
    "InvalidInstanceInformationFilterValue"

-- | The specified inventory group isn\'t valid.
_InvalidInventoryGroupException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidInventoryGroupException =
  Core._MatchServiceError
    defaultService
    "InvalidInventoryGroupException"

-- | You specified invalid keys or values in the @Context@ attribute for
-- @InventoryItem@. Verify the keys and values, and try again.
_InvalidInventoryItemContextException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidInventoryItemContextException =
  Core._MatchServiceError
    defaultService
    "InvalidInventoryItemContextException"

-- | The request isn\'t valid.
_InvalidInventoryRequestException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidInventoryRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidInventoryRequestException"

-- | One or more content items isn\'t valid.
_InvalidItemContentException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidItemContentException =
  Core._MatchServiceError
    defaultService
    "InvalidItemContentException"

-- | The query key ID isn\'t valid.
_InvalidKeyId :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidKeyId =
  Core._MatchServiceError
    defaultService
    "InvalidKeyId"

-- | The specified token isn\'t valid.
_InvalidNextToken :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidNextToken =
  Core._MatchServiceError
    defaultService
    "InvalidNextToken"

-- | One or more configuration items isn\'t valid. Verify that a valid Amazon
-- Resource Name (ARN) was provided for an Amazon Simple Notification
-- Service topic.
_InvalidNotificationConfig :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidNotificationConfig =
  Core._MatchServiceError
    defaultService
    "InvalidNotificationConfig"

-- | The delete inventory option specified isn\'t valid. Verify the option
-- and try again.
_InvalidOptionException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidOptionException =
  Core._MatchServiceError
    defaultService
    "InvalidOptionException"

-- | The S3 bucket doesn\'t exist.
_InvalidOutputFolder :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidOutputFolder =
  Core._MatchServiceError
    defaultService
    "InvalidOutputFolder"

-- | The output location isn\'t valid or doesn\'t exist.
_InvalidOutputLocation :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidOutputLocation =
  Core._MatchServiceError
    defaultService
    "InvalidOutputLocation"

-- | You must specify values for all required parameters in the Amazon Web
-- Services Systems Manager document (SSM document). You can only supply
-- values to parameters defined in the SSM document.
_InvalidParameters :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidParameters =
  Core._MatchServiceError
    defaultService
    "InvalidParameters"

-- | The permission type isn\'t supported. /Share/ is the only supported
-- permission type.
_InvalidPermissionType :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidPermissionType =
  Core._MatchServiceError
    defaultService
    "InvalidPermissionType"

-- | The plugin name isn\'t valid.
_InvalidPluginName :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidPluginName =
  Core._MatchServiceError
    defaultService
    "InvalidPluginName"

-- | A policy attribute or its value is invalid.
_InvalidPolicyAttributeException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidPolicyAttributeException =
  Core._MatchServiceError
    defaultService
    "InvalidPolicyAttributeException"

-- | The policy type isn\'t supported. Parameter Store supports the following
-- policy types: Expiration, ExpirationNotification, and
-- NoChangeNotification.
_InvalidPolicyTypeException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidPolicyTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidPolicyTypeException"

-- | The resource ID isn\'t valid. Verify that you entered the correct ID and
-- try again.
_InvalidResourceId :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidResourceId =
  Core._MatchServiceError
    defaultService
    "InvalidResourceId"

-- | The resource type isn\'t valid. For example, if you are attempting to
-- tag an EC2 instance, the instance must be a registered managed node.
_InvalidResourceType :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidResourceType =
  Core._MatchServiceError
    defaultService
    "InvalidResourceType"

-- | The specified inventory item result attribute isn\'t valid.
_InvalidResultAttributeException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidResultAttributeException =
  Core._MatchServiceError
    defaultService
    "InvalidResultAttributeException"

-- | The role name can\'t contain invalid characters. Also verify that you
-- specified an IAM role for notifications that includes the required trust
-- policy. For information about configuring the IAM role for Run Command
-- notifications, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/rc-sns-notifications.html Configuring Amazon SNS Notifications for Run Command>
-- in the /Amazon Web Services Systems Manager User Guide/.
_InvalidRole :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidRole =
  Core._MatchServiceError
    defaultService
    "InvalidRole"

-- | The schedule is invalid. Verify your cron or rate expression and try
-- again.
_InvalidSchedule :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidSchedule =
  Core._MatchServiceError
    defaultService
    "InvalidSchedule"

-- | The specified tag key or value isn\'t valid.
_InvalidTag :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidTag =
  Core._MatchServiceError defaultService "InvalidTag"

-- | The target isn\'t valid or doesn\'t exist. It might not be configured
-- for Systems Manager or you might not have permission to perform the
-- operation.
_InvalidTarget :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidTarget =
  Core._MatchServiceError
    defaultService
    "InvalidTarget"

-- | TargetMap parameter isn\'t valid.
_InvalidTargetMaps :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidTargetMaps =
  Core._MatchServiceError
    defaultService
    "InvalidTargetMaps"

-- | The parameter type name isn\'t valid.
_InvalidTypeNameException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidTypeNameException =
  Core._MatchServiceError
    defaultService
    "InvalidTypeNameException"

-- | The update isn\'t valid.
_InvalidUpdate :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidUpdate =
  Core._MatchServiceError
    defaultService
    "InvalidUpdate"

-- | The command ID and managed node ID you specified didn\'t match any
-- invocations. Verify the command ID and the managed node ID and try
-- again.
_InvocationDoesNotExist :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvocationDoesNotExist =
  Core._MatchServiceError
    defaultService
    "InvocationDoesNotExist"

-- | The inventory item has invalid content.
_ItemContentMismatchException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ItemContentMismatchException =
  Core._MatchServiceError
    defaultService
    "ItemContentMismatchException"

-- | The inventory item size has exceeded the size limit.
_ItemSizeLimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ItemSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ItemSizeLimitExceededException"

-- | The size limit of a document is 64 KB.
_MaxDocumentSizeExceeded :: Core.AsError a => Lens.Fold a Core.ServiceError
_MaxDocumentSizeExceeded =
  Core._MatchServiceError
    defaultService
    "MaxDocumentSizeExceeded"

-- | You don\'t have permission to view OpsItems in the specified account.
-- Verify that your account is configured either as a Systems Manager
-- delegated administrator or that you are logged into the Organizations
-- management account.
_OpsItemAccessDeniedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_OpsItemAccessDeniedException =
  Core._MatchServiceError
    defaultService
    "OpsItemAccessDeniedException"

-- | The OpsItem already exists.
_OpsItemAlreadyExistsException :: Core.AsError a => Lens.Fold a Core.ServiceError
_OpsItemAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "OpsItemAlreadyExistsException"

-- | A specified parameter argument isn\'t valid. Verify the available
-- arguments and try again.
_OpsItemInvalidParameterException :: Core.AsError a => Lens.Fold a Core.ServiceError
_OpsItemInvalidParameterException =
  Core._MatchServiceError
    defaultService
    "OpsItemInvalidParameterException"

-- | The request caused OpsItems to exceed one or more quotas. For
-- information about OpsItem quotas, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-learn-more.html#OpsCenter-learn-more-limits What are the resource limits for OpsCenter?>.
_OpsItemLimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_OpsItemLimitExceededException =
  Core._MatchServiceError
    defaultService
    "OpsItemLimitExceededException"

-- | The specified OpsItem ID doesn\'t exist. Verify the ID and try again.
_OpsItemNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_OpsItemNotFoundException =
  Core._MatchServiceError
    defaultService
    "OpsItemNotFoundException"

-- | The Amazon Resource Name (ARN) is already associated with the OpsItem.
_OpsItemRelatedItemAlreadyExistsException :: Core.AsError a => Lens.Fold a Core.ServiceError
_OpsItemRelatedItemAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "OpsItemRelatedItemAlreadyExistsException"

-- | The association wasn\'t found using the parameters you specified in the
-- call. Verify the information and try again.
_OpsItemRelatedItemAssociationNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_OpsItemRelatedItemAssociationNotFoundException =
  Core._MatchServiceError
    defaultService
    "OpsItemRelatedItemAssociationNotFoundException"

-- | An OpsMetadata object already exists for the selected resource.
_OpsMetadataAlreadyExistsException :: Core.AsError a => Lens.Fold a Core.ServiceError
_OpsMetadataAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "OpsMetadataAlreadyExistsException"

-- | One of the arguments passed is invalid.
_OpsMetadataInvalidArgumentException :: Core.AsError a => Lens.Fold a Core.ServiceError
_OpsMetadataInvalidArgumentException =
  Core._MatchServiceError
    defaultService
    "OpsMetadataInvalidArgumentException"

-- | The OpsMetadata object exceeds the maximum number of OpsMetadata keys
-- that you can assign to an application in Application Manager.
_OpsMetadataKeyLimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_OpsMetadataKeyLimitExceededException =
  Core._MatchServiceError
    defaultService
    "OpsMetadataKeyLimitExceededException"

-- | Your account reached the maximum number of OpsMetadata objects allowed
-- by Application Manager. The maximum is 200 OpsMetadata objects. Delete
-- one or more OpsMetadata object and try again.
_OpsMetadataLimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_OpsMetadataLimitExceededException =
  Core._MatchServiceError
    defaultService
    "OpsMetadataLimitExceededException"

-- | The OpsMetadata object doesn\'t exist.
_OpsMetadataNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_OpsMetadataNotFoundException =
  Core._MatchServiceError
    defaultService
    "OpsMetadataNotFoundException"

-- | The system is processing too many concurrent updates. Wait a few moments
-- and try again.
_OpsMetadataTooManyUpdatesException :: Core.AsError a => Lens.Fold a Core.ServiceError
_OpsMetadataTooManyUpdatesException =
  Core._MatchServiceError
    defaultService
    "OpsMetadataTooManyUpdatesException"

-- | The parameter already exists. You can\'t create duplicate parameters.
_ParameterAlreadyExists :: Core.AsError a => Lens.Fold a Core.ServiceError
_ParameterAlreadyExists =
  Core._MatchServiceError
    defaultService
    "ParameterAlreadyExists"

-- | You have exceeded the number of parameters for this Amazon Web Services
-- account. Delete one or more parameters and try again.
_ParameterLimitExceeded :: Core.AsError a => Lens.Fold a Core.ServiceError
_ParameterLimitExceeded =
  Core._MatchServiceError
    defaultService
    "ParameterLimitExceeded"

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
_ParameterMaxVersionLimitExceeded :: Core.AsError a => Lens.Fold a Core.ServiceError
_ParameterMaxVersionLimitExceeded =
  Core._MatchServiceError
    defaultService
    "ParameterMaxVersionLimitExceeded"

-- | The parameter couldn\'t be found. Verify the name and try again.
_ParameterNotFound :: Core.AsError a => Lens.Fold a Core.ServiceError
_ParameterNotFound =
  Core._MatchServiceError
    defaultService
    "ParameterNotFound"

-- | The parameter name isn\'t valid.
_ParameterPatternMismatchException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ParameterPatternMismatchException =
  Core._MatchServiceError
    defaultService
    "ParameterPatternMismatchException"

-- | A parameter version can have a maximum of ten labels.
_ParameterVersionLabelLimitExceeded :: Core.AsError a => Lens.Fold a Core.ServiceError
_ParameterVersionLabelLimitExceeded =
  Core._MatchServiceError
    defaultService
    "ParameterVersionLabelLimitExceeded"

-- | The specified parameter version wasn\'t found. Verify the parameter name
-- and version, and try again.
_ParameterVersionNotFound :: Core.AsError a => Lens.Fold a Core.ServiceError
_ParameterVersionNotFound =
  Core._MatchServiceError
    defaultService
    "ParameterVersionNotFound"

-- | You specified more than the maximum number of allowed policies for the
-- parameter. The maximum is 10.
_PoliciesLimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_PoliciesLimitExceededException =
  Core._MatchServiceError
    defaultService
    "PoliciesLimitExceededException"

-- | A sync configuration with the same name already exists.
_ResourceDataSyncAlreadyExistsException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceDataSyncAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceDataSyncAlreadyExistsException"

-- | Another @UpdateResourceDataSync@ request is being processed. Wait a few
-- minutes and try again.
_ResourceDataSyncConflictException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceDataSyncConflictException =
  Core._MatchServiceError
    defaultService
    "ResourceDataSyncConflictException"

-- | You have exceeded the allowed maximum sync configurations.
_ResourceDataSyncCountExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceDataSyncCountExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceDataSyncCountExceededException"

-- | The specified sync configuration is invalid.
_ResourceDataSyncInvalidConfigurationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceDataSyncInvalidConfigurationException =
  Core._MatchServiceError
    defaultService
    "ResourceDataSyncInvalidConfigurationException"

-- | The specified sync name wasn\'t found.
_ResourceDataSyncNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceDataSyncNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceDataSyncNotFoundException"

-- | Error returned if an attempt is made to delete a patch baseline that is
-- registered for a patch group.
_ResourceInUseException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | Error returned when the caller has exceeded the default resource quotas.
-- For example, too many maintenance windows or patch baselines have been
-- created.
--
-- For information about resource quotas in Systems Manager, see
-- <https://docs.aws.amazon.com/general/latest/gr/ssm.html#limits_ssm Systems Manager service quotas>
-- in the /Amazon Web Services General Reference/.
_ResourceLimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceLimitExceededException"

-- | The hash provided in the call doesn\'t match the stored hash. This
-- exception is thrown when trying to update an obsolete policy version or
-- when multiple requests to update a policy are sent.
_ResourcePolicyConflictException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourcePolicyConflictException =
  Core._MatchServiceError
    defaultService
    "ResourcePolicyConflictException"

-- | One or more parameters specified for the call aren\'t valid. Verify the
-- parameters and their values and try again.
_ResourcePolicyInvalidParameterException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourcePolicyInvalidParameterException =
  Core._MatchServiceError
    defaultService
    "ResourcePolicyInvalidParameterException"

-- | The PutResourcePolicy API action enforces two limits. A policy can\'t be
-- greater than 1024 bytes in size. And only one policy can be attached to
-- @OpsItemGroup@. Verify these limits and try again.
_ResourcePolicyLimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourcePolicyLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ResourcePolicyLimitExceededException"

-- | The specified service setting wasn\'t found. Either the service name or
-- the setting hasn\'t been provisioned by the Amazon Web Services service
-- team.
_ServiceSettingNotFound :: Core.AsError a => Lens.Fold a Core.ServiceError
_ServiceSettingNotFound =
  Core._MatchServiceError
    defaultService
    "ServiceSettingNotFound"

-- | The updated status is the same as the current status.
_StatusUnchanged :: Core.AsError a => Lens.Fold a Core.ServiceError
_StatusUnchanged =
  Core._MatchServiceError
    defaultService
    "StatusUnchanged"

-- | The sub-type count exceeded the limit for the inventory type.
_SubTypeCountLimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_SubTypeCountLimitExceededException =
  Core._MatchServiceError
    defaultService
    "SubTypeCountLimitExceededException"

-- | You specified the @Safe@ option for the
-- DeregisterTargetFromMaintenanceWindow operation, but the target is still
-- referenced in a task.
_TargetInUseException :: Core.AsError a => Lens.Fold a Core.ServiceError
_TargetInUseException =
  Core._MatchServiceError
    defaultService
    "TargetInUseException"

-- | The specified target managed node for the session isn\'t fully
-- configured for use with Session Manager. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/session-manager-getting-started.html Getting started with Session Manager>
-- in the /Amazon Web Services Systems Manager User Guide/. This error is
-- also returned if you attempt to start a session on a managed node that
-- is located in a different account or Region
_TargetNotConnected :: Core.AsError a => Lens.Fold a Core.ServiceError
_TargetNotConnected =
  Core._MatchServiceError
    defaultService
    "TargetNotConnected"

-- | The @Targets@ parameter includes too many tags. Remove one or more tags
-- and try the command again.
_TooManyTagsError :: Core.AsError a => Lens.Fold a Core.ServiceError
_TooManyTagsError =
  Core._MatchServiceError
    defaultService
    "TooManyTagsError"

-- | There are concurrent updates for a resource that supports one update at
-- a time.
_TooManyUpdates :: Core.AsError a => Lens.Fold a Core.ServiceError
_TooManyUpdates =
  Core._MatchServiceError
    defaultService
    "TooManyUpdates"

-- | The size of inventory data has exceeded the total size limit for the
-- resource.
_TotalSizeLimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_TotalSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TotalSizeLimitExceededException"

-- | The calendar entry contained in the specified SSM document isn\'t
-- supported.
_UnsupportedCalendarException :: Core.AsError a => Lens.Fold a Core.ServiceError
_UnsupportedCalendarException =
  Core._MatchServiceError
    defaultService
    "UnsupportedCalendarException"

-- | Patching for applications released by Microsoft is only available on EC2
-- instances and advanced instances. To patch applications released by
-- Microsoft on on-premises servers and VMs, you must enable advanced
-- instances. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-managedinstances-advanced.html Enabling the advanced-instances tier>
-- in the /Amazon Web Services Systems Manager User Guide/.
_UnsupportedFeatureRequiredException :: Core.AsError a => Lens.Fold a Core.ServiceError
_UnsupportedFeatureRequiredException =
  Core._MatchServiceError
    defaultService
    "UnsupportedFeatureRequiredException"

-- | The @Context@ attribute that you specified for the @InventoryItem@
-- isn\'t allowed for this inventory type. You can only use the @Context@
-- attribute with inventory types like @AWS:ComplianceItem@.
_UnsupportedInventoryItemContextException :: Core.AsError a => Lens.Fold a Core.ServiceError
_UnsupportedInventoryItemContextException =
  Core._MatchServiceError
    defaultService
    "UnsupportedInventoryItemContextException"

-- | Inventory item type schema version has to match supported versions in
-- the service. Check output of GetInventorySchema to see the available
-- schema version for each type.
_UnsupportedInventorySchemaVersionException :: Core.AsError a => Lens.Fold a Core.ServiceError
_UnsupportedInventorySchemaVersionException =
  Core._MatchServiceError
    defaultService
    "UnsupportedInventorySchemaVersionException"

-- | The operating systems you specified isn\'t supported, or the operation
-- isn\'t supported for the operating system.
_UnsupportedOperatingSystem :: Core.AsError a => Lens.Fold a Core.ServiceError
_UnsupportedOperatingSystem =
  Core._MatchServiceError
    defaultService
    "UnsupportedOperatingSystem"

-- | The parameter type isn\'t supported.
_UnsupportedParameterType :: Core.AsError a => Lens.Fold a Core.ServiceError
_UnsupportedParameterType =
  Core._MatchServiceError
    defaultService
    "UnsupportedParameterType"

-- | The document doesn\'t support the platform type of the given managed
-- node ID(s). For example, you sent an document for a Windows managed node
-- to a Linux node.
_UnsupportedPlatformType :: Core.AsError a => Lens.Fold a Core.ServiceError
_UnsupportedPlatformType =
  Core._MatchServiceError
    defaultService
    "UnsupportedPlatformType"
