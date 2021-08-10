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
    _InvalidAggregatorException,
    _InvalidInstanceInformationFilterValue,
    _InvalidDeleteInventoryParametersException,
    _ComplianceTypeCountLimitExceededException,
    _OpsMetadataKeyLimitExceededException,
    _FeatureNotAvailableException,
    _InvalidAutomationSignalException,
    _IdempotentParameterMismatch,
    _ResourceDataSyncConflictException,
    _DoesNotExistException,
    _AutomationDefinitionVersionNotFoundException,
    _InvalidDocumentVersion,
    _UnsupportedParameterType,
    _HierarchyTypeMismatchException,
    _OpsMetadataInvalidArgumentException,
    _AutomationExecutionLimitExceededException,
    _InvalidAutomationStatusUpdateException,
    _AutomationExecutionNotFoundException,
    _InvalidFilter,
    _InvalidTypeNameException,
    _DocumentAlreadyExists,
    _DocumentPermissionLimit,
    _ItemSizeLimitExceededException,
    _ParameterMaxVersionLimitExceeded,
    _AutomationStepNotFoundException,
    _DocumentLimitExceeded,
    _TooManyTagsError,
    _InvalidInventoryItemContextException,
    _IncompatiblePolicyException,
    _OpsItemInvalidParameterException,
    _InvalidDocument,
    _AutomationDefinitionNotFoundException,
    _SubTypeCountLimitExceededException,
    _InvalidAllowedPatternException,
    _ParameterVersionLabelLimitExceeded,
    _InvalidCommandId,
    _OpsMetadataTooManyUpdatesException,
    _ResourceLimitExceededException,
    _InvalidOutputLocation,
    _InvalidParameters,
    _TargetNotConnected,
    _UnsupportedInventorySchemaVersionException,
    _InvalidAssociation,
    _InvalidUpdate,
    _InvalidTarget,
    _CustomSchemaCountLimitExceededException,
    _ServiceSettingNotFound,
    _DuplicateDocumentVersionName,
    _InvalidInstanceId,
    _OpsMetadataNotFoundException,
    _InvalidDocumentSchemaVersion,
    _InvalidOptionException,
    _TooManyUpdates,
    _OpsItemNotFoundException,
    _StatusUnchanged,
    _ParameterNotFound,
    _AssociationDoesNotExist,
    _InvalidAssociationVersion,
    _ResourceDataSyncCountExceededException,
    _InvalidPolicyAttributeException,
    _InternalServerError,
    _InvalidFilterValue,
    _PoliciesLimitExceededException,
    _InvalidPluginName,
    _AssociationLimitExceeded,
    _ItemContentMismatchException,
    _ParameterAlreadyExists,
    _InvalidDocumentContent,
    _ParameterLimitExceeded,
    _UnsupportedFeatureRequiredException,
    _InvalidDeletionIdException,
    _AssociationAlreadyExists,
    _InvalidSchedule,
    _ResourceDataSyncAlreadyExistsException,
    _ResourceInUseException,
    _DuplicateDocumentContent,
    _ResourceDataSyncNotFoundException,
    _AlreadyExistsException,
    _InvalidAutomationExecutionParametersException,
    _InvalidNotificationConfig,
    _InvalidFilterKey,
    _InvalidPolicyTypeException,
    _InvalidDocumentType,
    _TotalSizeLimitExceededException,
    _InvalidResourceId,
    _InvalidResultAttributeException,
    _InvalidResourceType,
    _ResourceDataSyncInvalidConfigurationException,
    _InvalidKeyId,
    _InvalidInventoryGroupException,
    _DocumentVersionLimitExceeded,
    _DuplicateInstanceId,
    _InvocationDoesNotExist,
    _InvalidDocumentOperation,
    _AutomationDefinitionNotApprovedException,
    _OpsMetadataAlreadyExistsException,
    _ParameterVersionNotFound,
    _OpsItemLimitExceededException,
    _UnsupportedCalendarException,
    _OpsMetadataLimitExceededException,
    _AssociationExecutionDoesNotExist,
    _HierarchyLevelLimitExceededException,
    _InvalidOutputFolder,
    _OpsItemAlreadyExistsException,
    _InvalidActivationId,
    _InvalidRole,
    _MaxDocumentSizeExceeded,
    _InvalidNextToken,
    _AssociationVersionLimitExceeded,
    _UnsupportedOperatingSystem,
    _InvalidActivation,
    _InvalidInventoryRequestException,
    _InvalidPermissionType,
    _InvalidFilterOption,
    _InvalidItemContentException,
    _TargetInUseException,
    _UnsupportedInventoryItemContextException,
    _UnsupportedPlatformType,
    _AssociatedInstances,
    _ParameterPatternMismatchException,

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

    -- * Association
    Association (..),
    newAssociation,
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

    -- * AssociationDescription
    AssociationDescription (..),
    newAssociationDescription,
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

    -- * AssociationExecution
    AssociationExecution (..),
    newAssociationExecution,
    associationExecution_status,
    associationExecution_lastExecutionDate,
    associationExecution_detailedStatus,
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
    associationExecutionTarget_lastExecutionDate,
    associationExecutionTarget_detailedStatus,
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

    -- * BaselineOverride
    BaselineOverride (..),
    newBaselineOverride,
    baselineOverride_sources,
    baselineOverride_rejectedPatches,
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

    -- * CommandPlugin
    CommandPlugin (..),
    newCommandPlugin,
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
    complianceItem_id,
    complianceItem_complianceType,
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
    documentParameter_type,
    documentParameter_defaultValue,

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
    documentVersionInfo_status,
    documentVersionInfo_createdDate,
    documentVersionInfo_statusInformation,
    documentVersionInfo_versionName,
    documentVersionInfo_name,
    documentVersionInfo_documentFormat,
    documentVersionInfo_reviewStatus,
    documentVersionInfo_isDefaultVersion,
    documentVersionInfo_documentVersion,

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
    instanceAssociationStatusInfo_outputUrl,
    instanceAssociationStatusInfo_associationId,
    instanceAssociationStatusInfo_associationName,
    instanceAssociationStatusInfo_executionDate,
    instanceAssociationStatusInfo_executionSummary,
    instanceAssociationStatusInfo_associationVersion,
    instanceAssociationStatusInfo_documentVersion,
    instanceAssociationStatusInfo_errorCode,

    -- * InstanceInformation
    InstanceInformation (..),
    newInstanceInformation,
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
    inventoryDeletionStatusItem_deletionId,
    inventoryDeletionStatusItem_deletionStartTime,
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
    maintenanceWindowExecutionTaskInvocationIdentity_ownerInformation,
    maintenanceWindowExecutionTaskInvocationIdentity_taskType,
    maintenanceWindowExecutionTaskInvocationIdentity_invocationId,
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
    maintenanceWindowIdentity_description,
    maintenanceWindowIdentity_scheduleTimezone,
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
    maintenanceWindowRunCommandParameters_outputS3KeyPrefix,
    maintenanceWindowRunCommandParameters_timeoutSeconds,
    maintenanceWindowRunCommandParameters_cloudWatchOutputConfig,
    maintenanceWindowRunCommandParameters_documentHashType,
    maintenanceWindowRunCommandParameters_documentVersion,
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
    opsItemEventSummary_eventId,
    opsItemEventSummary_detailType,
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
    opsItemSummary_opsItemType,
    opsItemSummary_plannedStartTime,
    opsItemSummary_lastModifiedTime,
    opsItemSummary_createdBy,
    opsItemSummary_lastModifiedBy,

    -- * OpsMetadata
    OpsMetadata (..),
    newOpsMetadata,
    opsMetadata_resourceId,
    opsMetadata_lastModifiedDate,
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
    parameter_type,
    parameter_dataType,
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
    parameterHistory_type,
    parameterHistory_dataType,
    parameterHistory_allowedPattern,
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
    parameterMetadata_type,
    parameterMetadata_dataType,
    parameterMetadata_allowedPattern,
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
    progressCounters_successSteps,
    progressCounters_failedSteps,

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
    resourceDataSyncItem_lastSyncTime,
    resourceDataSyncItem_syncName,
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
    serviceSetting_arn,
    serviceSetting_settingValue,
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
    session_target,
    session_owner,
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

-- | The specified aggregator is not valid for inventory groups. Verify that
-- the aggregator uses a valid inventory type such as @AWS:Application@ or
-- @AWS:InstanceInformation@.
_InvalidAggregatorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAggregatorException =
  Core._MatchServiceError
    defaultService
    "InvalidAggregatorException"

-- | The specified filter value is not valid.
_InvalidInstanceInformationFilterValue :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInstanceInformationFilterValue =
  Core._MatchServiceError
    defaultService
    "InvalidInstanceInformationFilterValue"

-- | One or more of the parameters specified for the delete operation is not
-- valid. Verify all parameters and try again.
_InvalidDeleteInventoryParametersException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeleteInventoryParametersException =
  Core._MatchServiceError
    defaultService
    "InvalidDeleteInventoryParametersException"

-- | You specified too many custom compliance types. You can specify a
-- maximum of 10 different types.
_ComplianceTypeCountLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ComplianceTypeCountLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ComplianceTypeCountLimitExceededException"

-- | The OpsMetadata object exceeds the maximum number of OpsMetadata keys
-- that you can assign to an application in Application Manager.
_OpsMetadataKeyLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsMetadataKeyLimitExceededException =
  Core._MatchServiceError
    defaultService
    "OpsMetadataKeyLimitExceededException"

-- | You attempted to register a LAMBDA or STEP_FUNCTIONS task in a region
-- where the corresponding service is not available.
_FeatureNotAvailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FeatureNotAvailableException =
  Core._MatchServiceError
    defaultService
    "FeatureNotAvailableException"

-- | The signal is not valid for the current Automation execution.
_InvalidAutomationSignalException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAutomationSignalException =
  Core._MatchServiceError
    defaultService
    "InvalidAutomationSignalException"

-- | Error returned when an idempotent operation is retried and the
-- parameters don\'t match the original call to the API with the same
-- idempotency token.
_IdempotentParameterMismatch :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IdempotentParameterMismatch =
  Core._MatchServiceError
    defaultService
    "IdempotentParameterMismatch"

-- | Another @UpdateResourceDataSync@ request is being processed. Wait a few
-- minutes and try again.
_ResourceDataSyncConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceDataSyncConflictException =
  Core._MatchServiceError
    defaultService
    "ResourceDataSyncConflictException"

-- | Error returned when the ID specified for a resource, such as a
-- maintenance window or Patch baseline, doesn\'t exist.
--
-- For information about resource quotas in Systems Manager, see
-- <http://docs.aws.amazon.com/general/latest/gr/ssm.html#limits_ssm Systems Manager service quotas>
-- in the /AWS General Reference/.
_DoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DoesNotExistException =
  Core._MatchServiceError
    defaultService
    "DoesNotExistException"

-- | An Automation document with the specified name and version could not be
-- found.
_AutomationDefinitionVersionNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AutomationDefinitionVersionNotFoundException =
  Core._MatchServiceError
    defaultService
    "AutomationDefinitionVersionNotFoundException"

-- | The document version is not valid or does not exist.
_InvalidDocumentVersion :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDocumentVersion =
  Core._MatchServiceError
    defaultService
    "InvalidDocumentVersion"

-- | The parameter type is not supported.
_UnsupportedParameterType :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedParameterType =
  Core._MatchServiceError
    defaultService
    "UnsupportedParameterType"

-- | Parameter Store does not support changing a parameter type in a
-- hierarchy. For example, you can\'t change a parameter from a @String@
-- type to a @SecureString@ type. You must create a new, unique parameter.
_HierarchyTypeMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_HierarchyTypeMismatchException =
  Core._MatchServiceError
    defaultService
    "HierarchyTypeMismatchException"

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

-- | The specified update status operation is not valid.
_InvalidAutomationStatusUpdateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAutomationStatusUpdateException =
  Core._MatchServiceError
    defaultService
    "InvalidAutomationStatusUpdateException"

-- | There is no automation execution information for the requested
-- automation execution ID.
_AutomationExecutionNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AutomationExecutionNotFoundException =
  Core._MatchServiceError
    defaultService
    "AutomationExecutionNotFoundException"

-- | The filter name is not valid. Verify the you entered the correct name
-- and try again.
_InvalidFilter :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFilter =
  Core._MatchServiceError
    defaultService
    "InvalidFilter"

-- | The parameter type name is not valid.
_InvalidTypeNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTypeNameException =
  Core._MatchServiceError
    defaultService
    "InvalidTypeNameException"

-- | The specified document already exists.
_DocumentAlreadyExists :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DocumentAlreadyExists =
  Core._MatchServiceError
    defaultService
    "DocumentAlreadyExists"

-- | The document cannot be shared with more AWS user accounts. You can share
-- a document with a maximum of 20 accounts. You can publicly share up to
-- five documents. If you need to increase this limit, contact AWS Support.
_DocumentPermissionLimit :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DocumentPermissionLimit =
  Core._MatchServiceError
    defaultService
    "DocumentPermissionLimit"

-- | The inventory item size has exceeded the size limit.
_ItemSizeLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ItemSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ItemSizeLimitExceededException"

-- | Parameter Store retains the 100 most recently created versions of a
-- parameter. After this number of versions has been created, Parameter
-- Store deletes the oldest version when a new one is created. However, if
-- the oldest version has a /label/ attached to it, Parameter Store will
-- not delete the version and instead presents this error message:
--
-- @An error occurred (ParameterMaxVersionLimitExceeded) when calling the PutParameter operation: You attempted to create a new version of parameter-name by calling the PutParameter API with the overwrite flag. Version version-number, the oldest version, can\'t be deleted because it has a label associated with it. Move the label to another version of the parameter, and try again.@
--
-- This safeguard is to prevent parameter versions with mission critical
-- labels assigned to them from being deleted. To continue creating new
-- parameters, first move the label from the oldest version of the
-- parameter to a newer one for use in your operations. For information
-- about moving parameter labels, see
-- <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-labels.html#sysman-paramstore-labels-console-move Move a parameter label (console)>
-- or
-- <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-labels.html#sysman-paramstore-labels-cli-move Move a parameter label (CLI)>
-- in the /AWS Systems Manager User Guide/.
_ParameterMaxVersionLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterMaxVersionLimitExceeded =
  Core._MatchServiceError
    defaultService
    "ParameterMaxVersionLimitExceeded"

-- | The specified step name and execution ID don\'t exist. Verify the
-- information and try again.
_AutomationStepNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AutomationStepNotFoundException =
  Core._MatchServiceError
    defaultService
    "AutomationStepNotFoundException"

-- | You can have at most 500 active Systems Manager documents.
_DocumentLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DocumentLimitExceeded =
  Core._MatchServiceError
    defaultService
    "DocumentLimitExceeded"

-- | The @Targets@ parameter includes too many tags. Remove one or more tags
-- and try the command again.
_TooManyTagsError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsError =
  Core._MatchServiceError
    defaultService
    "TooManyTagsError"

-- | You specified invalid keys or values in the @Context@ attribute for
-- @InventoryItem@. Verify the keys and values, and try again.
_InvalidInventoryItemContextException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInventoryItemContextException =
  Core._MatchServiceError
    defaultService
    "InvalidInventoryItemContextException"

-- | There is a conflict in the policies specified for this parameter. You
-- can\'t, for example, specify two Expiration policies for a parameter.
-- Review your policies, and try again.
_IncompatiblePolicyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IncompatiblePolicyException =
  Core._MatchServiceError
    defaultService
    "IncompatiblePolicyException"

-- | A specified parameter argument isn\'t valid. Verify the available
-- arguments and try again.
_OpsItemInvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsItemInvalidParameterException =
  Core._MatchServiceError
    defaultService
    "OpsItemInvalidParameterException"

-- | The specified document does not exist.
_InvalidDocument :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDocument =
  Core._MatchServiceError
    defaultService
    "InvalidDocument"

-- | An Automation document with the specified name could not be found.
_AutomationDefinitionNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AutomationDefinitionNotFoundException =
  Core._MatchServiceError
    defaultService
    "AutomationDefinitionNotFoundException"

-- | The sub-type count exceeded the limit for the inventory type.
_SubTypeCountLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubTypeCountLimitExceededException =
  Core._MatchServiceError
    defaultService
    "SubTypeCountLimitExceededException"

-- | The request does not meet the regular expression requirement.
_InvalidAllowedPatternException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAllowedPatternException =
  Core._MatchServiceError
    defaultService
    "InvalidAllowedPatternException"

-- | A parameter version can have a maximum of ten labels.
_ParameterVersionLabelLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterVersionLabelLimitExceeded =
  Core._MatchServiceError
    defaultService
    "ParameterVersionLabelLimitExceeded"

-- | Prism for InvalidCommandId' errors.
_InvalidCommandId :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCommandId =
  Core._MatchServiceError
    defaultService
    "InvalidCommandId"

-- | The system is processing too many concurrent updates. Wait a few moments
-- and try again.
_OpsMetadataTooManyUpdatesException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsMetadataTooManyUpdatesException =
  Core._MatchServiceError
    defaultService
    "OpsMetadataTooManyUpdatesException"

-- | Error returned when the caller has exceeded the default resource quotas.
-- For example, too many maintenance windows or patch baselines have been
-- created.
--
-- For information about resource quotas in Systems Manager, see
-- <http://docs.aws.amazon.com/general/latest/gr/ssm.html#limits_ssm Systems Manager service quotas>
-- in the /AWS General Reference/.
_ResourceLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceLimitExceededException"

-- | The output location is not valid or does not exist.
_InvalidOutputLocation :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOutputLocation =
  Core._MatchServiceError
    defaultService
    "InvalidOutputLocation"

-- | You must specify values for all required parameters in the Systems
-- Manager document. You can only supply values to parameters defined in
-- the Systems Manager document.
_InvalidParameters :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameters =
  Core._MatchServiceError
    defaultService
    "InvalidParameters"

-- | The specified target instance for the session is not fully configured
-- for use with Session Manager. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/session-manager-getting-started.html Getting started with Session Manager>
-- in the /AWS Systems Manager User Guide/. This error is also returned if
-- you attempt to start a session on an instance that is located in a
-- different account or Region
_TargetNotConnected :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TargetNotConnected =
  Core._MatchServiceError
    defaultService
    "TargetNotConnected"

-- | Inventory item type schema version has to match supported versions in
-- the service. Check output of GetInventorySchema to see the available
-- schema version for each type.
_UnsupportedInventorySchemaVersionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedInventorySchemaVersionException =
  Core._MatchServiceError
    defaultService
    "UnsupportedInventorySchemaVersionException"

-- | The association is not valid or does not exist.
_InvalidAssociation :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAssociation =
  Core._MatchServiceError
    defaultService
    "InvalidAssociation"

-- | The update is not valid.
_InvalidUpdate :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidUpdate =
  Core._MatchServiceError
    defaultService
    "InvalidUpdate"

-- | The target is not valid or does not exist. It might not be configured
-- for Systems Manager or you might not have permission to perform the
-- operation.
_InvalidTarget :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTarget =
  Core._MatchServiceError
    defaultService
    "InvalidTarget"

-- | You have exceeded the limit for custom schemas. Delete one or more
-- custom schemas and try again.
_CustomSchemaCountLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CustomSchemaCountLimitExceededException =
  Core._MatchServiceError
    defaultService
    "CustomSchemaCountLimitExceededException"

-- | The specified service setting was not found. Either the service name or
-- the setting has not been provisioned by the AWS service team.
_ServiceSettingNotFound :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceSettingNotFound =
  Core._MatchServiceError
    defaultService
    "ServiceSettingNotFound"

-- | The version name has already been used in this document. Specify a
-- different version name, and then try again.
_DuplicateDocumentVersionName :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateDocumentVersionName =
  Core._MatchServiceError
    defaultService
    "DuplicateDocumentVersionName"

-- | The following problems can cause this exception:
--
-- You do not have permission to access the instance.
--
-- SSM Agent is not running. Verify that SSM Agent is running.
--
-- SSM Agent is not registered with the SSM endpoint. Try reinstalling SSM
-- Agent.
--
-- The instance is not in valid state. Valid states are: Running, Pending,
-- Stopped, Stopping. Invalid states are: Shutting-down and Terminated.
_InvalidInstanceId :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInstanceId =
  Core._MatchServiceError
    defaultService
    "InvalidInstanceId"

-- | The OpsMetadata object does not exist.
_OpsMetadataNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsMetadataNotFoundException =
  Core._MatchServiceError
    defaultService
    "OpsMetadataNotFoundException"

-- | The version of the document schema is not supported.
_InvalidDocumentSchemaVersion :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDocumentSchemaVersion =
  Core._MatchServiceError
    defaultService
    "InvalidDocumentSchemaVersion"

-- | The delete inventory option specified is not valid. Verify the option
-- and try again.
_InvalidOptionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOptionException =
  Core._MatchServiceError
    defaultService
    "InvalidOptionException"

-- | There are concurrent updates for a resource that supports one update at
-- a time.
_TooManyUpdates :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyUpdates =
  Core._MatchServiceError
    defaultService
    "TooManyUpdates"

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

-- | The parameter could not be found. Verify the name and try again.
_ParameterNotFound :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterNotFound =
  Core._MatchServiceError
    defaultService
    "ParameterNotFound"

-- | The specified association does not exist.
_AssociationDoesNotExist :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AssociationDoesNotExist =
  Core._MatchServiceError
    defaultService
    "AssociationDoesNotExist"

-- | The version you specified is not valid. Use ListAssociationVersions to
-- view all versions of an association according to the association ID. Or,
-- use the @$LATEST@ parameter to view the latest version of the
-- association.
_InvalidAssociationVersion :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAssociationVersion =
  Core._MatchServiceError
    defaultService
    "InvalidAssociationVersion"

-- | You have exceeded the allowed maximum sync configurations.
_ResourceDataSyncCountExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceDataSyncCountExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceDataSyncCountExceededException"

-- | A policy attribute or its value is invalid.
_InvalidPolicyAttributeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPolicyAttributeException =
  Core._MatchServiceError
    defaultService
    "InvalidPolicyAttributeException"

-- | An error occurred on the server side.
_InternalServerError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError
    defaultService
    "InternalServerError"

-- | The filter value is not valid. Verify the value and try again.
_InvalidFilterValue :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFilterValue =
  Core._MatchServiceError
    defaultService
    "InvalidFilterValue"

-- | You specified more than the maximum number of allowed policies for the
-- parameter. The maximum is 10.
_PoliciesLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PoliciesLimitExceededException =
  Core._MatchServiceError
    defaultService
    "PoliciesLimitExceededException"

-- | The plugin name is not valid.
_InvalidPluginName :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPluginName =
  Core._MatchServiceError
    defaultService
    "InvalidPluginName"

-- | You can have at most 2,000 active associations.
_AssociationLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AssociationLimitExceeded =
  Core._MatchServiceError
    defaultService
    "AssociationLimitExceeded"

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

-- | The content for the document is not valid.
_InvalidDocumentContent :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDocumentContent =
  Core._MatchServiceError
    defaultService
    "InvalidDocumentContent"

-- | You have exceeded the number of parameters for this AWS account. Delete
-- one or more parameters and try again.
_ParameterLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterLimitExceeded =
  Core._MatchServiceError
    defaultService
    "ParameterLimitExceeded"

-- | Microsoft application patching is only available on EC2 instances and
-- advanced instances. To patch Microsoft applications on on-premises
-- servers and VMs, you must enable advanced instances. For more
-- information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-managedinstances-advanced.html Using the advanced-instances tier>
-- in the /AWS Systems Manager User Guide/.
_UnsupportedFeatureRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedFeatureRequiredException =
  Core._MatchServiceError
    defaultService
    "UnsupportedFeatureRequiredException"

-- | The ID specified for the delete operation does not exist or is not
-- valid. Verify the ID and try again.
_InvalidDeletionIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeletionIdException =
  Core._MatchServiceError
    defaultService
    "InvalidDeletionIdException"

-- | The specified association already exists.
_AssociationAlreadyExists :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AssociationAlreadyExists =
  Core._MatchServiceError
    defaultService
    "AssociationAlreadyExists"

-- | The schedule is invalid. Verify your cron or rate expression and try
-- again.
_InvalidSchedule :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSchedule =
  Core._MatchServiceError
    defaultService
    "InvalidSchedule"

-- | A sync configuration with the same name already exists.
_ResourceDataSyncAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceDataSyncAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceDataSyncAlreadyExistsException"

-- | Error returned if an attempt is made to delete a patch baseline that is
-- registered for a patch group.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | The content of the association document matches another document. Change
-- the content of the document and try again.
_DuplicateDocumentContent :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateDocumentContent =
  Core._MatchServiceError
    defaultService
    "DuplicateDocumentContent"

-- | The specified sync name was not found.
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

-- | The supplied parameters for invoking the specified Automation document
-- are incorrect. For example, they may not match the set of parameters
-- permitted for the specified Automation document.
_InvalidAutomationExecutionParametersException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAutomationExecutionParametersException =
  Core._MatchServiceError
    defaultService
    "InvalidAutomationExecutionParametersException"

-- | One or more configuration items is not valid. Verify that a valid Amazon
-- Resource Name (ARN) was provided for an Amazon SNS topic.
_InvalidNotificationConfig :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNotificationConfig =
  Core._MatchServiceError
    defaultService
    "InvalidNotificationConfig"

-- | The specified key is not valid.
_InvalidFilterKey :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFilterKey =
  Core._MatchServiceError
    defaultService
    "InvalidFilterKey"

-- | The policy type is not supported. Parameter Store supports the following
-- policy types: Expiration, ExpirationNotification, and
-- NoChangeNotification.
_InvalidPolicyTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPolicyTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidPolicyTypeException"

-- | The document type is not valid. Valid document types are described in
-- the @DocumentType@ property.
_InvalidDocumentType :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDocumentType =
  Core._MatchServiceError
    defaultService
    "InvalidDocumentType"

-- | The size of inventory data has exceeded the total size limit for the
-- resource.
_TotalSizeLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TotalSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TotalSizeLimitExceededException"

-- | The resource ID is not valid. Verify that you entered the correct ID and
-- try again.
_InvalidResourceId :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResourceId =
  Core._MatchServiceError
    defaultService
    "InvalidResourceId"

-- | The specified inventory item result attribute is not valid.
_InvalidResultAttributeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResultAttributeException =
  Core._MatchServiceError
    defaultService
    "InvalidResultAttributeException"

-- | The resource type is not valid. For example, if you are attempting to
-- tag an instance, the instance must be a registered, managed instance.
_InvalidResourceType :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResourceType =
  Core._MatchServiceError
    defaultService
    "InvalidResourceType"

-- | The specified sync configuration is invalid.
_ResourceDataSyncInvalidConfigurationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceDataSyncInvalidConfigurationException =
  Core._MatchServiceError
    defaultService
    "ResourceDataSyncInvalidConfigurationException"

-- | The query key ID is not valid.
_InvalidKeyId :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidKeyId =
  Core._MatchServiceError
    defaultService
    "InvalidKeyId"

-- | The specified inventory group is not valid.
_InvalidInventoryGroupException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInventoryGroupException =
  Core._MatchServiceError
    defaultService
    "InvalidInventoryGroupException"

-- | The document has too many versions. Delete one or more document versions
-- and try again.
_DocumentVersionLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DocumentVersionLimitExceeded =
  Core._MatchServiceError
    defaultService
    "DocumentVersionLimitExceeded"

-- | You cannot specify an instance ID in more than one association.
_DuplicateInstanceId :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateInstanceId =
  Core._MatchServiceError
    defaultService
    "DuplicateInstanceId"

-- | The command ID and instance ID you specified did not match any
-- invocations. Verify the command ID and the instance ID and try again.
_InvocationDoesNotExist :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvocationDoesNotExist =
  Core._MatchServiceError
    defaultService
    "InvocationDoesNotExist"

-- | You attempted to delete a document while it is still shared. You must
-- stop sharing the document before you can delete it.
_InvalidDocumentOperation :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDocumentOperation =
  Core._MatchServiceError
    defaultService
    "InvalidDocumentOperation"

-- | Indicates that the Change Manager change template used in the change
-- request was rejected or is still in a pending state.
_AutomationDefinitionNotApprovedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AutomationDefinitionNotApprovedException =
  Core._MatchServiceError
    defaultService
    "AutomationDefinitionNotApprovedException"

-- | An OpsMetadata object already exists for the selected resource.
_OpsMetadataAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsMetadataAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "OpsMetadataAlreadyExistsException"

-- | The specified parameter version was not found. Verify the parameter name
-- and version, and try again.
_ParameterVersionNotFound :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterVersionNotFound =
  Core._MatchServiceError
    defaultService
    "ParameterVersionNotFound"

-- | The request caused OpsItems to exceed one or more quotas. For
-- information about OpsItem quotas, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-learn-more.html#OpsCenter-learn-more-limits What are the resource limits for OpsCenter?>.
_OpsItemLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsItemLimitExceededException =
  Core._MatchServiceError
    defaultService
    "OpsItemLimitExceededException"

-- | The calendar entry contained in the specified Systems Manager document
-- is not supported.
_UnsupportedCalendarException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedCalendarException =
  Core._MatchServiceError
    defaultService
    "UnsupportedCalendarException"

-- | Your account reached the maximum number of OpsMetadata objects allowed
-- by Application Manager. The maximum is 200 OpsMetadata objects. Delete
-- one or more OpsMetadata object and try again.
_OpsMetadataLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsMetadataLimitExceededException =
  Core._MatchServiceError
    defaultService
    "OpsMetadataLimitExceededException"

-- | The specified execution ID does not exist. Verify the ID number and try
-- again.
_AssociationExecutionDoesNotExist :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AssociationExecutionDoesNotExist =
  Core._MatchServiceError
    defaultService
    "AssociationExecutionDoesNotExist"

-- | A hierarchy can have a maximum of 15 levels. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-parameter-name-constraints.html Requirements and constraints for parameter names>
-- in the /AWS Systems Manager User Guide/.
_HierarchyLevelLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_HierarchyLevelLimitExceededException =
  Core._MatchServiceError
    defaultService
    "HierarchyLevelLimitExceededException"

-- | The S3 bucket does not exist.
_InvalidOutputFolder :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOutputFolder =
  Core._MatchServiceError
    defaultService
    "InvalidOutputFolder"

-- | The OpsItem already exists.
_OpsItemAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OpsItemAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "OpsItemAlreadyExistsException"

-- | The activation ID is not valid. Verify the you entered the correct
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
-- in the /AWS Systems Manager User Guide/.
_InvalidRole :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRole =
  Core._MatchServiceError
    defaultService
    "InvalidRole"

-- | The size limit of a document is 64 KB.
_MaxDocumentSizeExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MaxDocumentSizeExceeded =
  Core._MatchServiceError
    defaultService
    "MaxDocumentSizeExceeded"

-- | The specified token is not valid.
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

-- | The operating systems you specified is not supported, or the operation
-- is not supported for the operating system.
_UnsupportedOperatingSystem :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperatingSystem =
  Core._MatchServiceError
    defaultService
    "UnsupportedOperatingSystem"

-- | The activation is not valid. The activation might have been deleted, or
-- the ActivationId and the ActivationCode do not match.
_InvalidActivation :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidActivation =
  Core._MatchServiceError
    defaultService
    "InvalidActivation"

-- | The request is not valid.
_InvalidInventoryRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInventoryRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidInventoryRequestException"

-- | The permission type is not supported. /Share/ is the only supported
-- permission type.
_InvalidPermissionType :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPermissionType =
  Core._MatchServiceError
    defaultService
    "InvalidPermissionType"

-- | The specified filter option is not valid. Valid options are Equals and
-- BeginsWith. For Path filter, valid options are Recursive and OneLevel.
_InvalidFilterOption :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFilterOption =
  Core._MatchServiceError
    defaultService
    "InvalidFilterOption"

-- | One or more content items is not valid.
_InvalidItemContentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidItemContentException =
  Core._MatchServiceError
    defaultService
    "InvalidItemContentException"

-- | You specified the @Safe@ option for the
-- DeregisterTargetFromMaintenanceWindow operation, but the target is still
-- referenced in a task.
_TargetInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TargetInUseException =
  Core._MatchServiceError
    defaultService
    "TargetInUseException"

-- | The @Context@ attribute that you specified for the @InventoryItem@ is
-- not allowed for this inventory type. You can only use the @Context@
-- attribute with inventory types like @AWS:ComplianceItem@.
_UnsupportedInventoryItemContextException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedInventoryItemContextException =
  Core._MatchServiceError
    defaultService
    "UnsupportedInventoryItemContextException"

-- | The document does not support the platform type of the given instance
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

-- | The parameter name is not valid.
_ParameterPatternMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterPatternMismatchException =
  Core._MatchServiceError
    defaultService
    "ParameterPatternMismatchException"
