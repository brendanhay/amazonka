{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSM.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _CustomSchemaCountLimitExceededException,
    _InvalidAggregatorException,
    _InvocationDoesNotExist,
    _OpsMetadataInvalidArgumentException,
    _ResourcePolicyConflictException,
    _InvalidFilterKey,
    _OpsItemLimitExceededException,
    _AutomationExecutionLimitExceededException,
    _FeatureNotAvailableException,
    _InvalidOptionException,
    _DocumentPermissionLimit,
    _ParameterLimitExceeded,
    _InvalidDocumentContent,
    _ParameterAlreadyExists,
    _InvalidTypeNameException,
    _InvalidResourceId,
    _OpsItemAccessDeniedException,
    _ResourceDataSyncAlreadyExistsException,
    _DocumentLimitExceeded,
    _OpsItemRelatedItemAlreadyExistsException,
    _InvalidAutomationExecutionParametersException,
    _DocumentAlreadyExists,
    _DocumentVersionLimitExceeded,
    _InvalidPolicyAttributeException,
    _UnsupportedPlatformType,
    _InvalidDocumentType,
    _IncompatiblePolicyException,
    _AssociationAlreadyExists,
    _MaxDocumentSizeExceeded,
    _InvalidDeleteInventoryParametersException,
    _InvalidPluginName,
    _StatusUnchanged,
    _ResourceDataSyncInvalidConfigurationException,
    _InvalidAllowedPatternException,
    _AssociationExecutionDoesNotExist,
    _AutomationDefinitionVersionNotFoundException,
    _AutomationExecutionNotFoundException,
    _OpsItemNotFoundException,
    _InvalidNextToken,
    _TooManyTagsError,
    _InvalidUpdate,
    _OpsMetadataKeyLimitExceededException,
    _InvalidFilterOption,
    _ItemContentMismatchException,
    _UnsupportedCalendarException,
    _InvalidPermissionType,
    _OpsMetadataTooManyUpdatesException,
    _ParameterVersionLabelLimitExceeded,
    _TargetNotConnected,
    _ParameterMaxVersionLimitExceeded,
    _DuplicateDocumentContent,
    _InvalidActivationId,
    _InvalidAutomationStatusUpdateException,
    _InvalidInventoryItemContextException,
    _ItemSizeLimitExceededException,
    _AssociationDoesNotExist,
    _ResourceInUseException,
    _AlreadyExistsException,
    _ResourcePolicyLimitExceededException,
    _AssociationVersionLimitExceeded,
    _InvalidFilterValue,
    _ParameterVersionNotFound,
    _AutomationDefinitionNotFoundException,
    _ResourceLimitExceededException,
    _ParameterPatternMismatchException,
    _InvalidAssociationVersion,
    _TargetInUseException,
    _InvalidResultAttributeException,
    _ResourceDataSyncNotFoundException,
    _TotalSizeLimitExceededException,
    _OpsItemAlreadyExistsException,
    _InvalidItemContentException,
    _InternalServerError,
    _HierarchyLevelLimitExceededException,
    _UnsupportedParameterType,
    _InvalidInstanceInformationFilterValue,
    _UnsupportedInventorySchemaVersionException,
    _InvalidSchedule,
    _InvalidInstanceId,
    _HierarchyTypeMismatchException,
    _InvalidKeyId,
    _AssociationLimitExceeded,
    _InvalidAutomationSignalException,
    _OpsItemRelatedItemAssociationNotFoundException,
    _InvalidOutputLocation,
    _ComplianceTypeCountLimitExceededException,
    _InvalidInventoryGroupException,
    _ParameterNotFound,
    _InvalidDeletionIdException,
    _IdempotentParameterMismatch,
    _InvalidInventoryRequestException,
    _UnsupportedFeatureRequiredException,
    _ServiceSettingNotFound,
    _OpsItemInvalidParameterException,
    _InvalidCommandId,
    _ResourceDataSyncConflictException,
    _InvalidDocumentOperation,
    _AssociatedInstances,
    _InvalidDocument,
    _InvalidTarget,
    _TooManyUpdates,
    _AutomationDefinitionNotApprovedException,
    _SubTypeCountLimitExceededException,
    _InvalidRole,
    _AutomationStepNotFoundException,
    _InvalidPolicyTypeException,
    _InvalidParameters,
    _ResourceDataSyncCountExceededException,
    _InvalidNotificationConfig,
    _InvalidTag,
    _InvalidDocumentSchemaVersion,
    _OpsMetadataNotFoundException,
    _InvalidOutputFolder,
    _PoliciesLimitExceededException,
    _DuplicateDocumentVersionName,
    _UnsupportedInventoryItemContextException,
    _InvalidActivation,
    _DuplicateInstanceId,
    _InvalidResourceType,
    _InvalidDocumentVersion,
    _InvalidFilter,
    _InvalidAssociation,
    _DoesNotExistException,
    _OpsMetadataLimitExceededException,
    _OpsMetadataAlreadyExistsException,
    _UnsupportedOperatingSystem,
    _InvalidTargetMaps,
    _ResourcePolicyInvalidParameterException,

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

    -- * AssociationDescription
    AssociationDescription (..),
    newAssociationDescription,
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

    -- * AssociationExecution
    AssociationExecution (..),
    newAssociationExecution,
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
    associationExecutionTarget_resourceType,
    associationExecutionTarget_associationVersion,
    associationExecutionTarget_status,
    associationExecutionTarget_executionId,
    associationExecutionTarget_outputSource,
    associationExecutionTarget_detailedStatus,
    associationExecutionTarget_lastExecutionDate,
    associationExecutionTarget_associationId,

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
    associationOverview_status,
    associationOverview_detailedStatus,

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

    -- * AttachmentContent
    AttachmentContent (..),
    newAttachmentContent,
    attachmentContent_name,
    attachmentContent_hash,
    attachmentContent_size,
    attachmentContent_url,
    attachmentContent_hashType,

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

    -- * AutomationExecutionFilter
    AutomationExecutionFilter (..),
    newAutomationExecutionFilter,
    automationExecutionFilter_key,
    automationExecutionFilter_values,

    -- * AutomationExecutionMetadata
    AutomationExecutionMetadata (..),
    newAutomationExecutionMetadata,
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

    -- * BaselineOverride
    BaselineOverride (..),
    newBaselineOverride,
    baselineOverride_operatingSystem,
    baselineOverride_approvedPatches,
    baselineOverride_approvedPatchesComplianceLevel,
    baselineOverride_sources,
    baselineOverride_approvalRules,
    baselineOverride_rejectedPatchesAction,
    baselineOverride_globalFilters,
    baselineOverride_rejectedPatches,
    baselineOverride_approvedPatchesEnableNonSecurity,

    -- * CloudWatchOutputConfig
    CloudWatchOutputConfig (..),
    newCloudWatchOutputConfig,
    cloudWatchOutputConfig_cloudWatchOutputEnabled,
    cloudWatchOutputConfig_cloudWatchLogGroupName,

    -- * Command
    Command (..),
    newCommand,
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

    -- * CommandFilter
    CommandFilter (..),
    newCommandFilter,
    commandFilter_key,
    commandFilter_value,

    -- * CommandInvocation
    CommandInvocation (..),
    newCommandInvocation,
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

    -- * CommandPlugin
    CommandPlugin (..),
    newCommandPlugin,
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

    -- * ComplianceExecutionSummary
    ComplianceExecutionSummary (..),
    newComplianceExecutionSummary,
    complianceExecutionSummary_executionType,
    complianceExecutionSummary_executionId,
    complianceExecutionSummary_executionTime,

    -- * ComplianceItem
    ComplianceItem (..),
    newComplianceItem,
    complianceItem_resourceId,
    complianceItem_resourceType,
    complianceItem_severity,
    complianceItem_status,
    complianceItem_id,
    complianceItem_details,
    complianceItem_executionSummary,
    complianceItem_title,
    complianceItem_complianceType,

    -- * ComplianceItemEntry
    ComplianceItemEntry (..),
    newComplianceItemEntry,
    complianceItemEntry_id,
    complianceItemEntry_details,
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
    complianceSummaryItem_compliantSummary,
    complianceSummaryItem_nonCompliantSummary,
    complianceSummaryItem_complianceType,

    -- * CompliantSummary
    CompliantSummary (..),
    newCompliantSummary,
    compliantSummary_compliantCount,
    compliantSummary_severitySummary,

    -- * CreateAssociationBatchRequestEntry
    CreateAssociationBatchRequestEntry (..),
    newCreateAssociationBatchRequestEntry,
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

    -- * DescribeActivationsFilter
    DescribeActivationsFilter (..),
    newDescribeActivationsFilter,
    describeActivationsFilter_filterValues,
    describeActivationsFilter_filterKey,

    -- * DocumentDefaultVersionDescription
    DocumentDefaultVersionDescription (..),
    newDocumentDefaultVersionDescription,
    documentDefaultVersionDescription_name,
    documentDefaultVersionDescription_defaultVersionName,
    documentDefaultVersionDescription_defaultVersion,

    -- * DocumentDescription
    DocumentDescription (..),
    newDocumentDescription,
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

    -- * DocumentFilter
    DocumentFilter (..),
    newDocumentFilter,
    documentFilter_key,
    documentFilter_value,

    -- * DocumentIdentifier
    DocumentIdentifier (..),
    newDocumentIdentifier,
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
    documentParameter_type,
    documentParameter_defaultValue,
    documentParameter_description,

    -- * DocumentRequires
    DocumentRequires (..),
    newDocumentRequires,
    documentRequires_version,
    documentRequires_name,

    -- * DocumentReviewCommentSource
    DocumentReviewCommentSource (..),
    newDocumentReviewCommentSource,
    documentReviewCommentSource_type,
    documentReviewCommentSource_content,

    -- * DocumentReviewerResponseSource
    DocumentReviewerResponseSource (..),
    newDocumentReviewerResponseSource,
    documentReviewerResponseSource_reviewer,
    documentReviewerResponseSource_comment,
    documentReviewerResponseSource_createTime,
    documentReviewerResponseSource_updatedTime,
    documentReviewerResponseSource_reviewStatus,

    -- * DocumentReviews
    DocumentReviews (..),
    newDocumentReviews,
    documentReviews_comment,
    documentReviews_action,

    -- * DocumentVersionInfo
    DocumentVersionInfo (..),
    newDocumentVersionInfo,
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

    -- * EffectivePatch
    EffectivePatch (..),
    newEffectivePatch,
    effectivePatch_patch,
    effectivePatch_patchStatus,

    -- * FailedCreateAssociation
    FailedCreateAssociation (..),
    newFailedCreateAssociation,
    failedCreateAssociation_message,
    failedCreateAssociation_fault,
    failedCreateAssociation_entry,

    -- * FailureDetails
    FailureDetails (..),
    newFailureDetails,
    failureDetails_failureType,
    failureDetails_details,
    failureDetails_failureStage,

    -- * GetResourcePoliciesResponseEntry
    GetResourcePoliciesResponseEntry (..),
    newGetResourcePoliciesResponseEntry,
    getResourcePoliciesResponseEntry_policyId,
    getResourcePoliciesResponseEntry_policy,
    getResourcePoliciesResponseEntry_policyHash,

    -- * InstanceAggregatedAssociationOverview
    InstanceAggregatedAssociationOverview (..),
    newInstanceAggregatedAssociationOverview,
    instanceAggregatedAssociationOverview_detailedStatus,
    instanceAggregatedAssociationOverview_instanceAssociationStatusAggregatedCount,

    -- * InstanceAssociation
    InstanceAssociation (..),
    newInstanceAssociation,
    instanceAssociation_associationVersion,
    instanceAssociation_instanceId,
    instanceAssociation_content,
    instanceAssociation_associationId,

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

    -- * InstanceInformation
    InstanceInformation (..),
    newInstanceInformation,
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

    -- * InstancePatchStateFilter
    InstancePatchStateFilter (..),
    newInstancePatchStateFilter,
    instancePatchStateFilter_key,
    instancePatchStateFilter_values,
    instancePatchStateFilter_type,

    -- * InventoryAggregator
    InventoryAggregator (..),
    newInventoryAggregator,
    inventoryAggregator_expression,
    inventoryAggregator_aggregators,
    inventoryAggregator_groups,

    -- * InventoryDeletionStatusItem
    InventoryDeletionStatusItem (..),
    newInventoryDeletionStatusItem,
    inventoryDeletionStatusItem_deletionStartTime,
    inventoryDeletionStatusItem_lastStatusMessage,
    inventoryDeletionStatusItem_lastStatus,
    inventoryDeletionStatusItem_typeName,
    inventoryDeletionStatusItem_deletionSummary,
    inventoryDeletionStatusItem_lastStatusUpdateTime,
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
    inventoryItem_contentHash,
    inventoryItem_context,
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
    inventoryItemSchema_displayName,
    inventoryItemSchema_version,
    inventoryItemSchema_typeName,
    inventoryItemSchema_attributes,

    -- * InventoryResultEntity
    InventoryResultEntity (..),
    newInventoryResultEntity,
    inventoryResultEntity_id,
    inventoryResultEntity_data,

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
    maintenanceWindowAutomationParameters_documentVersion,
    maintenanceWindowAutomationParameters_parameters,

    -- * MaintenanceWindowExecution
    MaintenanceWindowExecution (..),
    newMaintenanceWindowExecution,
    maintenanceWindowExecution_windowExecutionId,
    maintenanceWindowExecution_statusDetails,
    maintenanceWindowExecution_windowId,
    maintenanceWindowExecution_status,
    maintenanceWindowExecution_endTime,
    maintenanceWindowExecution_startTime,

    -- * MaintenanceWindowExecutionTaskIdentity
    MaintenanceWindowExecutionTaskIdentity (..),
    newMaintenanceWindowExecutionTaskIdentity,
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

    -- * MaintenanceWindowExecutionTaskInvocationIdentity
    MaintenanceWindowExecutionTaskInvocationIdentity (..),
    newMaintenanceWindowExecutionTaskInvocationIdentity,
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

    -- * MaintenanceWindowFilter
    MaintenanceWindowFilter (..),
    newMaintenanceWindowFilter,
    maintenanceWindowFilter_key,
    maintenanceWindowFilter_values,

    -- * MaintenanceWindowIdentity
    MaintenanceWindowIdentity (..),
    newMaintenanceWindowIdentity,
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

    -- * MaintenanceWindowStepFunctionsParameters
    MaintenanceWindowStepFunctionsParameters (..),
    newMaintenanceWindowStepFunctionsParameters,
    maintenanceWindowStepFunctionsParameters_name,
    maintenanceWindowStepFunctionsParameters_input,

    -- * MaintenanceWindowTarget
    MaintenanceWindowTarget (..),
    newMaintenanceWindowTarget,
    maintenanceWindowTarget_windowTargetId,
    maintenanceWindowTarget_resourceType,
    maintenanceWindowTarget_name,
    maintenanceWindowTarget_windowId,
    maintenanceWindowTarget_targets,
    maintenanceWindowTarget_description,
    maintenanceWindowTarget_ownerInformation,

    -- * MaintenanceWindowTask
    MaintenanceWindowTask (..),
    newMaintenanceWindowTask,
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

    -- * MaintenanceWindowTaskInvocationParameters
    MaintenanceWindowTaskInvocationParameters (..),
    newMaintenanceWindowTaskInvocationParameters,
    maintenanceWindowTaskInvocationParameters_automation,
    maintenanceWindowTaskInvocationParameters_lambda,
    maintenanceWindowTaskInvocationParameters_stepFunctions,
    maintenanceWindowTaskInvocationParameters_runCommand,

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
    notificationConfig_notificationType,
    notificationConfig_notificationArn,
    notificationConfig_notificationEvents,

    -- * OpsAggregator
    OpsAggregator (..),
    newOpsAggregator,
    opsAggregator_aggregatorType,
    opsAggregator_filters,
    opsAggregator_typeName,
    opsAggregator_aggregators,
    opsAggregator_values,
    opsAggregator_attributeName,

    -- * OpsEntity
    OpsEntity (..),
    newOpsEntity,
    opsEntity_id,
    opsEntity_data,

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
    opsItemEventSummary_detailType,
    opsItemEventSummary_createdTime,
    opsItemEventSummary_opsItemId,
    opsItemEventSummary_source,
    opsItemEventSummary_eventId,
    opsItemEventSummary_createdBy,
    opsItemEventSummary_detail,

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
    opsItemRelatedItemSummary_resourceType,
    opsItemRelatedItemSummary_createdTime,
    opsItemRelatedItemSummary_associationType,
    opsItemRelatedItemSummary_opsItemId,
    opsItemRelatedItemSummary_resourceUri,
    opsItemRelatedItemSummary_lastModifiedTime,
    opsItemRelatedItemSummary_lastModifiedBy,
    opsItemRelatedItemSummary_createdBy,
    opsItemRelatedItemSummary_associationId,

    -- * OpsItemRelatedItemsFilter
    OpsItemRelatedItemsFilter (..),
    newOpsItemRelatedItemsFilter,
    opsItemRelatedItemsFilter_key,
    opsItemRelatedItemsFilter_values,
    opsItemRelatedItemsFilter_operator,

    -- * OpsItemSummary
    OpsItemSummary (..),
    newOpsItemSummary,
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

    -- * OpsMetadata
    OpsMetadata (..),
    newOpsMetadata,
    opsMetadata_resourceId,
    opsMetadata_lastModifiedUser,
    opsMetadata_lastModifiedDate,
    opsMetadata_creationDate,
    opsMetadata_opsMetadataArn,

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
    outputSource_outputSourceType,
    outputSource_outputSourceId,

    -- * Parameter
    Parameter (..),
    newParameter,
    parameter_selector,
    parameter_lastModifiedDate,
    parameter_arn,
    parameter_dataType,
    parameter_sourceResult,
    parameter_name,
    parameter_type,
    parameter_value,
    parameter_version,

    -- * ParameterHistory
    ParameterHistory (..),
    newParameterHistory,
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

    -- * ParameterInlinePolicy
    ParameterInlinePolicy (..),
    newParameterInlinePolicy,
    parameterInlinePolicy_policyType,
    parameterInlinePolicy_policyText,
    parameterInlinePolicy_policyStatus,

    -- * ParameterMetadata
    ParameterMetadata (..),
    newParameterMetadata,
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

    -- * PatchBaselineIdentity
    PatchBaselineIdentity (..),
    newPatchBaselineIdentity,
    patchBaselineIdentity_operatingSystem,
    patchBaselineIdentity_baselineId,
    patchBaselineIdentity_baselineName,
    patchBaselineIdentity_baselineDescription,
    patchBaselineIdentity_defaultBaseline,

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
    patchGroupPatchBaselineMapping_patchGroup,
    patchGroupPatchBaselineMapping_baselineIdentity,

    -- * PatchOrchestratorFilter
    PatchOrchestratorFilter (..),
    newPatchOrchestratorFilter,
    patchOrchestratorFilter_key,
    patchOrchestratorFilter_values,

    -- * PatchRule
    PatchRule (..),
    newPatchRule,
    patchRule_approveAfterDays,
    patchRule_enableNonSecurity,
    patchRule_complianceLevel,
    patchRule_approveUntilDate,
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
    patchStatus_deploymentStatus,
    patchStatus_complianceLevel,
    patchStatus_approvalDate,

    -- * ProgressCounters
    ProgressCounters (..),
    newProgressCounters,
    progressCounters_cancelledSteps,
    progressCounters_timedOutSteps,
    progressCounters_failedSteps,
    progressCounters_successSteps,
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
    resolvedTargets_truncated,
    resolvedTargets_parameterValues,

    -- * ResourceComplianceSummaryItem
    ResourceComplianceSummaryItem (..),
    newResourceComplianceSummaryItem,
    resourceComplianceSummaryItem_resourceId,
    resourceComplianceSummaryItem_resourceType,
    resourceComplianceSummaryItem_compliantSummary,
    resourceComplianceSummaryItem_status,
    resourceComplianceSummaryItem_executionSummary,
    resourceComplianceSummaryItem_nonCompliantSummary,
    resourceComplianceSummaryItem_complianceType,
    resourceComplianceSummaryItem_overallSeverity,

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

    -- * ResourceDataSyncOrganizationalUnit
    ResourceDataSyncOrganizationalUnit (..),
    newResourceDataSyncOrganizationalUnit,
    resourceDataSyncOrganizationalUnit_organizationalUnitId,

    -- * ResourceDataSyncS3Destination
    ResourceDataSyncS3Destination (..),
    newResourceDataSyncS3Destination,
    resourceDataSyncS3Destination_destinationDataSharing,
    resourceDataSyncS3Destination_aWSKMSKeyARN,
    resourceDataSyncS3Destination_prefix,
    resourceDataSyncS3Destination_bucketName,
    resourceDataSyncS3Destination_syncFormat,
    resourceDataSyncS3Destination_region,

    -- * ResourceDataSyncSource
    ResourceDataSyncSource (..),
    newResourceDataSyncSource,
    resourceDataSyncSource_enableAllOpsDataSources,
    resourceDataSyncSource_awsOrganizationsSource,
    resourceDataSyncSource_includeFutureRegions,
    resourceDataSyncSource_sourceType,
    resourceDataSyncSource_sourceRegions,

    -- * ResourceDataSyncSourceWithState
    ResourceDataSyncSourceWithState (..),
    newResourceDataSyncSourceWithState,
    resourceDataSyncSourceWithState_enableAllOpsDataSources,
    resourceDataSyncSourceWithState_state,
    resourceDataSyncSourceWithState_sourceType,
    resourceDataSyncSourceWithState_sourceRegions,
    resourceDataSyncSourceWithState_awsOrganizationsSource,
    resourceDataSyncSourceWithState_includeFutureRegions,

    -- * ResultAttribute
    ResultAttribute (..),
    newResultAttribute,
    resultAttribute_typeName,

    -- * ReviewInformation
    ReviewInformation (..),
    newReviewInformation,
    reviewInformation_reviewer,
    reviewInformation_reviewedTime,
    reviewInformation_status,

    -- * Runbook
    Runbook (..),
    newRunbook,
    runbook_targetLocations,
    runbook_targetParameterName,
    runbook_targetMaps,
    runbook_targets,
    runbook_maxConcurrency,
    runbook_maxErrors,
    runbook_documentVersion,
    runbook_parameters,
    runbook_documentName,

    -- * S3OutputLocation
    S3OutputLocation (..),
    newS3OutputLocation,
    s3OutputLocation_outputS3Region,
    s3OutputLocation_outputS3BucketName,
    s3OutputLocation_outputS3KeyPrefix,

    -- * S3OutputUrl
    S3OutputUrl (..),
    newS3OutputUrl,
    s3OutputUrl_outputUrl,

    -- * ScheduledWindowExecution
    ScheduledWindowExecution (..),
    newScheduledWindowExecution,
    scheduledWindowExecution_name,
    scheduledWindowExecution_executionTime,
    scheduledWindowExecution_windowId,

    -- * ServiceSetting
    ServiceSetting (..),
    newServiceSetting,
    serviceSetting_lastModifiedUser,
    serviceSetting_lastModifiedDate,
    serviceSetting_arn,
    serviceSetting_settingId,
    serviceSetting_status,
    serviceSetting_settingValue,

    -- * Session
    Session (..),
    newSession,
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
    severitySummary_mediumCount,
    severitySummary_informationalCount,
    severitySummary_unspecifiedCount,
    severitySummary_criticalCount,
    severitySummary_highCount,
    severitySummary_lowCount,

    -- * StepExecution
    StepExecution (..),
    newStepExecution,
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
    targetLocation_targetLocationAlarmConfiguration,
    targetLocation_regions,
    targetLocation_targetLocationMaxConcurrency,
    targetLocation_accounts,
    targetLocation_executionRoleName,
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | You have exceeded the limit for custom schemas. Delete one or more
-- custom schemas and try again.
_CustomSchemaCountLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CustomSchemaCountLimitExceededException =
  Core._MatchServiceError
    defaultService
    "CustomSchemaCountLimitExceededException"

-- | The specified aggregator isn\'t valid for inventory groups. Verify that
-- the aggregator uses a valid inventory type such as @AWS:Application@ or
-- @AWS:InstanceInformation@.
_InvalidAggregatorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAggregatorException =
  Core._MatchServiceError
    defaultService
    "InvalidAggregatorException"

-- | The command ID and managed node ID you specified didn\'t match any
-- invocations. Verify the command ID and the managed node ID and try
-- again.
_InvocationDoesNotExist :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvocationDoesNotExist =
  Core._MatchServiceError
    defaultService
    "InvocationDoesNotExist"

-- | One of the arguments passed is invalid.
_OpsMetadataInvalidArgumentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsMetadataInvalidArgumentException =
  Core._MatchServiceError
    defaultService
    "OpsMetadataInvalidArgumentException"

-- | The hash provided in the call doesn\'t match the stored hash. This
-- exception is thrown when trying to update an obsolete policy version or
-- when multiple requests to update a policy are sent.
_ResourcePolicyConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourcePolicyConflictException =
  Core._MatchServiceError
    defaultService
    "ResourcePolicyConflictException"

-- | The specified key isn\'t valid.
_InvalidFilterKey :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFilterKey =
  Core._MatchServiceError
    defaultService
    "InvalidFilterKey"

-- | The request caused OpsItems to exceed one or more quotas. For
-- information about OpsItem quotas, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-learn-more.html#OpsCenter-learn-more-limits What are the resource limits for OpsCenter?>.
_OpsItemLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsItemLimitExceededException =
  Core._MatchServiceError
    defaultService
    "OpsItemLimitExceededException"

-- | The number of simultaneously running Automation executions exceeded the
-- allowable limit.
_AutomationExecutionLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AutomationExecutionLimitExceededException =
  Core._MatchServiceError
    defaultService
    "AutomationExecutionLimitExceededException"

-- | You attempted to register a @LAMBDA@ or @STEP_FUNCTIONS@ task in a
-- region where the corresponding service isn\'t available.
_FeatureNotAvailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FeatureNotAvailableException =
  Core._MatchServiceError
    defaultService
    "FeatureNotAvailableException"

-- | The delete inventory option specified isn\'t valid. Verify the option
-- and try again.
_InvalidOptionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOptionException =
  Core._MatchServiceError
    defaultService
    "InvalidOptionException"

-- | The document can\'t be shared with more Amazon Web Services user
-- accounts. You can specify a maximum of 20 accounts per API operation to
-- share a private document.
--
-- By default, you can share a private document with a maximum of 1,000
-- accounts and publicly share up to five documents.
--
-- If you need to increase the quota for privately or publicly shared
-- Systems Manager documents, contact Amazon Web Services Support.
_DocumentPermissionLimit :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DocumentPermissionLimit =
  Core._MatchServiceError
    defaultService
    "DocumentPermissionLimit"

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

-- | The parameter already exists. You can\'t create duplicate parameters.
_ParameterAlreadyExists :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterAlreadyExists =
  Core._MatchServiceError
    defaultService
    "ParameterAlreadyExists"

-- | The parameter type name isn\'t valid.
_InvalidTypeNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTypeNameException =
  Core._MatchServiceError
    defaultService
    "InvalidTypeNameException"

-- | The resource ID isn\'t valid. Verify that you entered the correct ID and
-- try again.
_InvalidResourceId :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResourceId =
  Core._MatchServiceError
    defaultService
    "InvalidResourceId"

-- | You don\'t have permission to view OpsItems in the specified account.
-- Verify that your account is configured either as a Systems Manager
-- delegated administrator or that you are logged into the Organizations
-- management account.
_OpsItemAccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsItemAccessDeniedException =
  Core._MatchServiceError
    defaultService
    "OpsItemAccessDeniedException"

-- | A sync configuration with the same name already exists.
_ResourceDataSyncAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceDataSyncAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceDataSyncAlreadyExistsException"

-- | You can have at most 500 active SSM documents.
_DocumentLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DocumentLimitExceeded =
  Core._MatchServiceError
    defaultService
    "DocumentLimitExceeded"

-- | The Amazon Resource Name (ARN) is already associated with the OpsItem.
_OpsItemRelatedItemAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsItemRelatedItemAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "OpsItemRelatedItemAlreadyExistsException"

-- | The supplied parameters for invoking the specified Automation runbook
-- are incorrect. For example, they may not match the set of parameters
-- permitted for the specified Automation document.
_InvalidAutomationExecutionParametersException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAutomationExecutionParametersException =
  Core._MatchServiceError
    defaultService
    "InvalidAutomationExecutionParametersException"

-- | The specified document already exists.
_DocumentAlreadyExists :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DocumentAlreadyExists =
  Core._MatchServiceError
    defaultService
    "DocumentAlreadyExists"

-- | The document has too many versions. Delete one or more document versions
-- and try again.
_DocumentVersionLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DocumentVersionLimitExceeded =
  Core._MatchServiceError
    defaultService
    "DocumentVersionLimitExceeded"

-- | A policy attribute or its value is invalid.
_InvalidPolicyAttributeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPolicyAttributeException =
  Core._MatchServiceError
    defaultService
    "InvalidPolicyAttributeException"

-- | The document doesn\'t support the platform type of the given managed
-- node ID(s). For example, you sent an document for a Windows managed node
-- to a Linux node.
_UnsupportedPlatformType :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedPlatformType =
  Core._MatchServiceError
    defaultService
    "UnsupportedPlatformType"

-- | The SSM document type isn\'t valid. Valid document types are described
-- in the @DocumentType@ property.
_InvalidDocumentType :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDocumentType =
  Core._MatchServiceError
    defaultService
    "InvalidDocumentType"

-- | There is a conflict in the policies specified for this parameter. You
-- can\'t, for example, specify two Expiration policies for a parameter.
-- Review your policies, and try again.
_IncompatiblePolicyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IncompatiblePolicyException =
  Core._MatchServiceError
    defaultService
    "IncompatiblePolicyException"

-- | The specified association already exists.
_AssociationAlreadyExists :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AssociationAlreadyExists =
  Core._MatchServiceError
    defaultService
    "AssociationAlreadyExists"

-- | The size limit of a document is 64 KB.
_MaxDocumentSizeExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MaxDocumentSizeExceeded =
  Core._MatchServiceError
    defaultService
    "MaxDocumentSizeExceeded"

-- | One or more of the parameters specified for the delete operation isn\'t
-- valid. Verify all parameters and try again.
_InvalidDeleteInventoryParametersException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeleteInventoryParametersException =
  Core._MatchServiceError
    defaultService
    "InvalidDeleteInventoryParametersException"

-- | The plugin name isn\'t valid.
_InvalidPluginName :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPluginName =
  Core._MatchServiceError
    defaultService
    "InvalidPluginName"

-- | The updated status is the same as the current status.
_StatusUnchanged :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StatusUnchanged =
  Core._MatchServiceError
    defaultService
    "StatusUnchanged"

-- | The specified sync configuration is invalid.
_ResourceDataSyncInvalidConfigurationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceDataSyncInvalidConfigurationException =
  Core._MatchServiceError
    defaultService
    "ResourceDataSyncInvalidConfigurationException"

-- | The request doesn\'t meet the regular expression requirement.
_InvalidAllowedPatternException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAllowedPatternException =
  Core._MatchServiceError
    defaultService
    "InvalidAllowedPatternException"

-- | The specified execution ID doesn\'t exist. Verify the ID number and try
-- again.
_AssociationExecutionDoesNotExist :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AssociationExecutionDoesNotExist =
  Core._MatchServiceError
    defaultService
    "AssociationExecutionDoesNotExist"

-- | An Automation runbook with the specified name and version couldn\'t be
-- found.
_AutomationDefinitionVersionNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AutomationDefinitionVersionNotFoundException =
  Core._MatchServiceError
    defaultService
    "AutomationDefinitionVersionNotFoundException"

-- | There is no automation execution information for the requested
-- automation execution ID.
_AutomationExecutionNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AutomationExecutionNotFoundException =
  Core._MatchServiceError
    defaultService
    "AutomationExecutionNotFoundException"

-- | The specified OpsItem ID doesn\'t exist. Verify the ID and try again.
_OpsItemNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsItemNotFoundException =
  Core._MatchServiceError
    defaultService
    "OpsItemNotFoundException"

-- | The specified token isn\'t valid.
_InvalidNextToken :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextToken =
  Core._MatchServiceError
    defaultService
    "InvalidNextToken"

-- | The @Targets@ parameter includes too many tags. Remove one or more tags
-- and try the command again.
_TooManyTagsError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsError =
  Core._MatchServiceError
    defaultService
    "TooManyTagsError"

-- | The update isn\'t valid.
_InvalidUpdate :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidUpdate =
  Core._MatchServiceError
    defaultService
    "InvalidUpdate"

-- | The OpsMetadata object exceeds the maximum number of OpsMetadata keys
-- that you can assign to an application in Application Manager.
_OpsMetadataKeyLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsMetadataKeyLimitExceededException =
  Core._MatchServiceError
    defaultService
    "OpsMetadataKeyLimitExceededException"

-- | The specified filter option isn\'t valid. Valid options are Equals and
-- BeginsWith. For Path filter, valid options are Recursive and OneLevel.
_InvalidFilterOption :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFilterOption =
  Core._MatchServiceError
    defaultService
    "InvalidFilterOption"

-- | The inventory item has invalid content.
_ItemContentMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ItemContentMismatchException =
  Core._MatchServiceError
    defaultService
    "ItemContentMismatchException"

-- | The calendar entry contained in the specified SSM document isn\'t
-- supported.
_UnsupportedCalendarException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedCalendarException =
  Core._MatchServiceError
    defaultService
    "UnsupportedCalendarException"

-- | The permission type isn\'t supported. /Share/ is the only supported
-- permission type.
_InvalidPermissionType :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPermissionType =
  Core._MatchServiceError
    defaultService
    "InvalidPermissionType"

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

-- | The specified target managed node for the session isn\'t fully
-- configured for use with Session Manager. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/session-manager-getting-started.html Getting started with Session Manager>
-- in the /Amazon Web Services Systems Manager User Guide/. This error is
-- also returned if you attempt to start a session on a managed node that
-- is located in a different account or Region
_TargetNotConnected :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TargetNotConnected =
  Core._MatchServiceError
    defaultService
    "TargetNotConnected"

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

-- | The content of the association document matches another document. Change
-- the content of the document and try again.
_DuplicateDocumentContent :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateDocumentContent =
  Core._MatchServiceError
    defaultService
    "DuplicateDocumentContent"

-- | The activation ID isn\'t valid. Verify the you entered the correct
-- ActivationId or ActivationCode and try again.
_InvalidActivationId :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidActivationId =
  Core._MatchServiceError
    defaultService
    "InvalidActivationId"

-- | The specified update status operation isn\'t valid.
_InvalidAutomationStatusUpdateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAutomationStatusUpdateException =
  Core._MatchServiceError
    defaultService
    "InvalidAutomationStatusUpdateException"

-- | You specified invalid keys or values in the @Context@ attribute for
-- @InventoryItem@. Verify the keys and values, and try again.
_InvalidInventoryItemContextException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInventoryItemContextException =
  Core._MatchServiceError
    defaultService
    "InvalidInventoryItemContextException"

-- | The inventory item size has exceeded the size limit.
_ItemSizeLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ItemSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ItemSizeLimitExceededException"

-- | The specified association doesn\'t exist.
_AssociationDoesNotExist :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AssociationDoesNotExist =
  Core._MatchServiceError
    defaultService
    "AssociationDoesNotExist"

-- | Error returned if an attempt is made to delete a patch baseline that is
-- registered for a patch group.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | Error returned if an attempt is made to register a patch group with a
-- patch baseline that is already registered with a different patch
-- baseline.
_AlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "AlreadyExistsException"

-- | The PutResourcePolicy API action enforces two limits. A policy can\'t be
-- greater than 1024 bytes in size. And only one policy can be attached to
-- @OpsItemGroup@. Verify these limits and try again.
_ResourcePolicyLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourcePolicyLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ResourcePolicyLimitExceededException"

-- | You have reached the maximum number versions allowed for an association.
-- Each association has a limit of 1,000 versions.
_AssociationVersionLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AssociationVersionLimitExceeded =
  Core._MatchServiceError
    defaultService
    "AssociationVersionLimitExceeded"

-- | The filter value isn\'t valid. Verify the value and try again.
_InvalidFilterValue :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFilterValue =
  Core._MatchServiceError
    defaultService
    "InvalidFilterValue"

-- | The specified parameter version wasn\'t found. Verify the parameter name
-- and version, and try again.
_ParameterVersionNotFound :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterVersionNotFound =
  Core._MatchServiceError
    defaultService
    "ParameterVersionNotFound"

-- | An Automation runbook with the specified name couldn\'t be found.
_AutomationDefinitionNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AutomationDefinitionNotFoundException =
  Core._MatchServiceError
    defaultService
    "AutomationDefinitionNotFoundException"

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

-- | The parameter name isn\'t valid.
_ParameterPatternMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterPatternMismatchException =
  Core._MatchServiceError
    defaultService
    "ParameterPatternMismatchException"

-- | The version you specified isn\'t valid. Use ListAssociationVersions to
-- view all versions of an association according to the association ID. Or,
-- use the @$LATEST@ parameter to view the latest version of the
-- association.
_InvalidAssociationVersion :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAssociationVersion =
  Core._MatchServiceError
    defaultService
    "InvalidAssociationVersion"

-- | You specified the @Safe@ option for the
-- DeregisterTargetFromMaintenanceWindow operation, but the target is still
-- referenced in a task.
_TargetInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TargetInUseException =
  Core._MatchServiceError
    defaultService
    "TargetInUseException"

-- | The specified inventory item result attribute isn\'t valid.
_InvalidResultAttributeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResultAttributeException =
  Core._MatchServiceError
    defaultService
    "InvalidResultAttributeException"

-- | The specified sync name wasn\'t found.
_ResourceDataSyncNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceDataSyncNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceDataSyncNotFoundException"

-- | The size of inventory data has exceeded the total size limit for the
-- resource.
_TotalSizeLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TotalSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TotalSizeLimitExceededException"

-- | The OpsItem already exists.
_OpsItemAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsItemAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "OpsItemAlreadyExistsException"

-- | One or more content items isn\'t valid.
_InvalidItemContentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidItemContentException =
  Core._MatchServiceError
    defaultService
    "InvalidItemContentException"

-- | An error occurred on the server side.
_InternalServerError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError
    defaultService
    "InternalServerError"

-- | A hierarchy can have a maximum of 15 levels. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-parameter-name-constraints.html Requirements and constraints for parameter names>
-- in the /Amazon Web Services Systems Manager User Guide/.
_HierarchyLevelLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_HierarchyLevelLimitExceededException =
  Core._MatchServiceError
    defaultService
    "HierarchyLevelLimitExceededException"

-- | The parameter type isn\'t supported.
_UnsupportedParameterType :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedParameterType =
  Core._MatchServiceError
    defaultService
    "UnsupportedParameterType"

-- | The specified filter value isn\'t valid.
_InvalidInstanceInformationFilterValue :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInstanceInformationFilterValue =
  Core._MatchServiceError
    defaultService
    "InvalidInstanceInformationFilterValue"

-- | Inventory item type schema version has to match supported versions in
-- the service. Check output of GetInventorySchema to see the available
-- schema version for each type.
_UnsupportedInventorySchemaVersionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedInventorySchemaVersionException =
  Core._MatchServiceError
    defaultService
    "UnsupportedInventorySchemaVersionException"

-- | The schedule is invalid. Verify your cron or rate expression and try
-- again.
_InvalidSchedule :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSchedule =
  Core._MatchServiceError
    defaultService
    "InvalidSchedule"

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
_InvalidInstanceId :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInstanceId =
  Core._MatchServiceError
    defaultService
    "InvalidInstanceId"

-- | Parameter Store doesn\'t support changing a parameter type in a
-- hierarchy. For example, you can\'t change a parameter from a @String@
-- type to a @SecureString@ type. You must create a new, unique parameter.
_HierarchyTypeMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_HierarchyTypeMismatchException =
  Core._MatchServiceError
    defaultService
    "HierarchyTypeMismatchException"

-- | The query key ID isn\'t valid.
_InvalidKeyId :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidKeyId =
  Core._MatchServiceError
    defaultService
    "InvalidKeyId"

-- | You can have at most 2,000 active associations.
_AssociationLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AssociationLimitExceeded =
  Core._MatchServiceError
    defaultService
    "AssociationLimitExceeded"

-- | The signal isn\'t valid for the current Automation execution.
_InvalidAutomationSignalException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAutomationSignalException =
  Core._MatchServiceError
    defaultService
    "InvalidAutomationSignalException"

-- | The association wasn\'t found using the parameters you specified in the
-- call. Verify the information and try again.
_OpsItemRelatedItemAssociationNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsItemRelatedItemAssociationNotFoundException =
  Core._MatchServiceError
    defaultService
    "OpsItemRelatedItemAssociationNotFoundException"

-- | The output location isn\'t valid or doesn\'t exist.
_InvalidOutputLocation :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOutputLocation =
  Core._MatchServiceError
    defaultService
    "InvalidOutputLocation"

-- | You specified too many custom compliance types. You can specify a
-- maximum of 10 different types.
_ComplianceTypeCountLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ComplianceTypeCountLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ComplianceTypeCountLimitExceededException"

-- | The specified inventory group isn\'t valid.
_InvalidInventoryGroupException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInventoryGroupException =
  Core._MatchServiceError
    defaultService
    "InvalidInventoryGroupException"

-- | The parameter couldn\'t be found. Verify the name and try again.
_ParameterNotFound :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterNotFound =
  Core._MatchServiceError
    defaultService
    "ParameterNotFound"

-- | The ID specified for the delete operation doesn\'t exist or isn\'t
-- valid. Verify the ID and try again.
_InvalidDeletionIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeletionIdException =
  Core._MatchServiceError
    defaultService
    "InvalidDeletionIdException"

-- | Error returned when an idempotent operation is retried and the
-- parameters don\'t match the original call to the API with the same
-- idempotency token.
_IdempotentParameterMismatch :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IdempotentParameterMismatch =
  Core._MatchServiceError
    defaultService
    "IdempotentParameterMismatch"

-- | The request isn\'t valid.
_InvalidInventoryRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInventoryRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidInventoryRequestException"

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

-- | The specified service setting wasn\'t found. Either the service name or
-- the setting hasn\'t been provisioned by the Amazon Web Services service
-- team.
_ServiceSettingNotFound :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceSettingNotFound =
  Core._MatchServiceError
    defaultService
    "ServiceSettingNotFound"

-- | A specified parameter argument isn\'t valid. Verify the available
-- arguments and try again.
_OpsItemInvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsItemInvalidParameterException =
  Core._MatchServiceError
    defaultService
    "OpsItemInvalidParameterException"

-- | The specified command ID isn\'t valid. Verify the ID and try again.
_InvalidCommandId :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCommandId =
  Core._MatchServiceError
    defaultService
    "InvalidCommandId"

-- | Another @UpdateResourceDataSync@ request is being processed. Wait a few
-- minutes and try again.
_ResourceDataSyncConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceDataSyncConflictException =
  Core._MatchServiceError
    defaultService
    "ResourceDataSyncConflictException"

-- | You attempted to delete a document while it is still shared. You must
-- stop sharing the document before you can delete it.
_InvalidDocumentOperation :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDocumentOperation =
  Core._MatchServiceError
    defaultService
    "InvalidDocumentOperation"

-- | You must disassociate a document from all managed nodes before you can
-- delete it.
_AssociatedInstances :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AssociatedInstances =
  Core._MatchServiceError
    defaultService
    "AssociatedInstances"

-- | The specified SSM document doesn\'t exist.
_InvalidDocument :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDocument =
  Core._MatchServiceError
    defaultService
    "InvalidDocument"

-- | The target isn\'t valid or doesn\'t exist. It might not be configured
-- for Systems Manager or you might not have permission to perform the
-- operation.
_InvalidTarget :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTarget =
  Core._MatchServiceError
    defaultService
    "InvalidTarget"

-- | There are concurrent updates for a resource that supports one update at
-- a time.
_TooManyUpdates :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyUpdates =
  Core._MatchServiceError
    defaultService
    "TooManyUpdates"

-- | Indicates that the Change Manager change template used in the change
-- request was rejected or is still in a pending state.
_AutomationDefinitionNotApprovedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AutomationDefinitionNotApprovedException =
  Core._MatchServiceError
    defaultService
    "AutomationDefinitionNotApprovedException"

-- | The sub-type count exceeded the limit for the inventory type.
_SubTypeCountLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubTypeCountLimitExceededException =
  Core._MatchServiceError
    defaultService
    "SubTypeCountLimitExceededException"

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

-- | The specified step name and execution ID don\'t exist. Verify the
-- information and try again.
_AutomationStepNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AutomationStepNotFoundException =
  Core._MatchServiceError
    defaultService
    "AutomationStepNotFoundException"

-- | The policy type isn\'t supported. Parameter Store supports the following
-- policy types: Expiration, ExpirationNotification, and
-- NoChangeNotification.
_InvalidPolicyTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPolicyTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidPolicyTypeException"

-- | You must specify values for all required parameters in the Amazon Web
-- Services Systems Manager document (SSM document). You can only supply
-- values to parameters defined in the SSM document.
_InvalidParameters :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameters =
  Core._MatchServiceError
    defaultService
    "InvalidParameters"

-- | You have exceeded the allowed maximum sync configurations.
_ResourceDataSyncCountExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceDataSyncCountExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceDataSyncCountExceededException"

-- | One or more configuration items isn\'t valid. Verify that a valid Amazon
-- Resource Name (ARN) was provided for an Amazon Simple Notification
-- Service topic.
_InvalidNotificationConfig :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNotificationConfig =
  Core._MatchServiceError
    defaultService
    "InvalidNotificationConfig"

-- | The specified tag key or value isn\'t valid.
_InvalidTag :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTag =
  Core._MatchServiceError defaultService "InvalidTag"

-- | The version of the document schema isn\'t supported.
_InvalidDocumentSchemaVersion :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDocumentSchemaVersion =
  Core._MatchServiceError
    defaultService
    "InvalidDocumentSchemaVersion"

-- | The OpsMetadata object doesn\'t exist.
_OpsMetadataNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsMetadataNotFoundException =
  Core._MatchServiceError
    defaultService
    "OpsMetadataNotFoundException"

-- | The S3 bucket doesn\'t exist.
_InvalidOutputFolder :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOutputFolder =
  Core._MatchServiceError
    defaultService
    "InvalidOutputFolder"

-- | You specified more than the maximum number of allowed policies for the
-- parameter. The maximum is 10.
_PoliciesLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PoliciesLimitExceededException =
  Core._MatchServiceError
    defaultService
    "PoliciesLimitExceededException"

-- | The version name has already been used in this document. Specify a
-- different version name, and then try again.
_DuplicateDocumentVersionName :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateDocumentVersionName =
  Core._MatchServiceError
    defaultService
    "DuplicateDocumentVersionName"

-- | The @Context@ attribute that you specified for the @InventoryItem@
-- isn\'t allowed for this inventory type. You can only use the @Context@
-- attribute with inventory types like @AWS:ComplianceItem@.
_UnsupportedInventoryItemContextException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedInventoryItemContextException =
  Core._MatchServiceError
    defaultService
    "UnsupportedInventoryItemContextException"

-- | The activation isn\'t valid. The activation might have been deleted, or
-- the ActivationId and the ActivationCode don\'t match.
_InvalidActivation :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidActivation =
  Core._MatchServiceError
    defaultService
    "InvalidActivation"

-- | You can\'t specify a managed node ID in more than one association.
_DuplicateInstanceId :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateInstanceId =
  Core._MatchServiceError
    defaultService
    "DuplicateInstanceId"

-- | The resource type isn\'t valid. For example, if you are attempting to
-- tag an EC2 instance, the instance must be a registered managed node.
_InvalidResourceType :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResourceType =
  Core._MatchServiceError
    defaultService
    "InvalidResourceType"

-- | The document version isn\'t valid or doesn\'t exist.
_InvalidDocumentVersion :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDocumentVersion =
  Core._MatchServiceError
    defaultService
    "InvalidDocumentVersion"

-- | The filter name isn\'t valid. Verify the you entered the correct name
-- and try again.
_InvalidFilter :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFilter =
  Core._MatchServiceError
    defaultService
    "InvalidFilter"

-- | The association isn\'t valid or doesn\'t exist.
_InvalidAssociation :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAssociation =
  Core._MatchServiceError
    defaultService
    "InvalidAssociation"

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

-- | The operating systems you specified isn\'t supported, or the operation
-- isn\'t supported for the operating system.
_UnsupportedOperatingSystem :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperatingSystem =
  Core._MatchServiceError
    defaultService
    "UnsupportedOperatingSystem"

-- | TargetMap parameter isn\'t valid.
_InvalidTargetMaps :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTargetMaps =
  Core._MatchServiceError
    defaultService
    "InvalidTargetMaps"

-- | One or more parameters specified for the call aren\'t valid. Verify the
-- parameters and their values and try again.
_ResourcePolicyInvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourcePolicyInvalidParameterException =
  Core._MatchServiceError
    defaultService
    "ResourcePolicyInvalidParameterException"
