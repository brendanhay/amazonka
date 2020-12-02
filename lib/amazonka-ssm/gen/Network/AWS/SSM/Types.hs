{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types
  ( -- * Service Configuration
    ssm,

    -- * Errors

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

    -- * DocumentParameterType
    DocumentParameterType (..),

    -- * DocumentPermissionType
    DocumentPermissionType (..),

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
    AccountSharingInfo,
    accountSharingInfo,
    asiSharedDocumentVersion,
    asiAccountId,

    -- * Activation
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

    -- * Association
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

    -- * AssociationDescription
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

    -- * AssociationExecution
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

    -- * AssociationExecutionFilter
    AssociationExecutionFilter,
    associationExecutionFilter,
    aefKey,
    aefValue,
    aefType,

    -- * AssociationExecutionTarget
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

    -- * AssociationExecutionTargetsFilter
    AssociationExecutionTargetsFilter,
    associationExecutionTargetsFilter,
    aetfKey,
    aetfValue,

    -- * AssociationFilter
    AssociationFilter,
    associationFilter,
    afKey,
    afValue,

    -- * AssociationOverview
    AssociationOverview,
    associationOverview,
    aoDetailedStatus,
    aoStatus,
    aoAssociationStatusAggregatedCount,

    -- * AssociationStatus
    AssociationStatus,
    associationStatus,
    asAdditionalInfo,
    asDate,
    asName,
    asMessage,

    -- * AssociationVersionInfo
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

    -- * AttachmentContent
    AttachmentContent,
    attachmentContent,
    acHash,
    acSize,
    acURL,
    acName,
    acHashType,

    -- * AttachmentInformation
    AttachmentInformation,
    attachmentInformation,
    aiName,

    -- * AttachmentsSource
    AttachmentsSource,
    attachmentsSource,
    aValues,
    aKey,
    aName,

    -- * AutomationExecution
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

    -- * AutomationExecutionFilter
    AutomationExecutionFilter,
    automationExecutionFilter,
    autKey,
    autValues,

    -- * AutomationExecutionMetadata
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

    -- * CloudWatchOutputConfig
    CloudWatchOutputConfig,
    cloudWatchOutputConfig,
    cwocCloudWatchLogGroupName,
    cwocCloudWatchOutputEnabled,

    -- * Command
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

    -- * CommandFilter
    CommandFilter,
    commandFilter,
    cfKey,
    cfValue,

    -- * CommandInvocation
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

    -- * CommandPlugin
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

    -- * ComplianceExecutionSummary
    ComplianceExecutionSummary,
    complianceExecutionSummary,
    cesExecutionId,
    cesExecutionType,
    cesExecutionTime,

    -- * ComplianceItem
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

    -- * ComplianceItemEntry
    ComplianceItemEntry,
    complianceItemEntry,
    cieDetails,
    cieId,
    cieTitle,
    cieSeverity,
    cieStatus,

    -- * ComplianceStringFilter
    ComplianceStringFilter,
    complianceStringFilter,
    csfValues,
    csfKey,
    csfType,

    -- * ComplianceSummaryItem
    ComplianceSummaryItem,
    complianceSummaryItem,
    csiNonCompliantSummary,
    csiCompliantSummary,
    csiComplianceType,

    -- * CompliantSummary
    CompliantSummary,
    compliantSummary,
    csCompliantCount,
    csSeveritySummary,

    -- * CreateAssociationBatchRequestEntry
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

    -- * DescribeActivationsFilter
    DescribeActivationsFilter,
    describeActivationsFilter,
    dafFilterKey,
    dafFilterValues,

    -- * DocumentDefaultVersionDescription
    DocumentDefaultVersionDescription,
    documentDefaultVersionDescription,
    ddvdDefaultVersionName,
    ddvdDefaultVersion,
    ddvdName,

    -- * DocumentDescription
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

    -- * DocumentFilter
    DocumentFilter,
    documentFilter,
    dfKey,
    dfValue,

    -- * DocumentIdentifier
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

    -- * DocumentKeyValuesFilter
    DocumentKeyValuesFilter,
    documentKeyValuesFilter,
    dkvfValues,
    dkvfKey,

    -- * DocumentParameter
    DocumentParameter,
    documentParameter,
    dpName,
    dpDefaultValue,
    dpType,
    dpDescription,

    -- * DocumentRequires
    DocumentRequires,
    documentRequires,
    drVersion,
    drName,

    -- * DocumentVersionInfo
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

    -- * EffectivePatch
    EffectivePatch,
    effectivePatch,
    epPatch,
    epPatchStatus,

    -- * FailedCreateAssociation
    FailedCreateAssociation,
    failedCreateAssociation,
    fcaEntry,
    fcaFault,
    fcaMessage,

    -- * FailureDetails
    FailureDetails,
    failureDetails,
    fdFailureType,
    fdFailureStage,
    fdDetails,

    -- * InstanceAggregatedAssociationOverview
    InstanceAggregatedAssociationOverview,
    instanceAggregatedAssociationOverview,
    iaaoDetailedStatus,
    iaaoInstanceAssociationStatusAggregatedCount,

    -- * InstanceAssociation
    InstanceAssociation,
    instanceAssociation,
    iaAssociationId,
    iaInstanceId,
    iaContent,
    iaAssociationVersion,

    -- * InstanceAssociationOutputLocation
    InstanceAssociationOutputLocation,
    instanceAssociationOutputLocation,
    iaolS3Location,

    -- * InstanceAssociationOutputURL
    InstanceAssociationOutputURL,
    instanceAssociationOutputURL,
    iaouS3OutputURL,

    -- * InstanceAssociationStatusInfo
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

    -- * InstanceInformation
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

    -- * InstanceInformationFilter
    InstanceInformationFilter,
    instanceInformationFilter,
    iifKey,
    iifValueSet,

    -- * InstanceInformationStringFilter
    InstanceInformationStringFilter,
    instanceInformationStringFilter,
    iisfKey,
    iisfValues,

    -- * InstancePatchState
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

    -- * InstancePatchStateFilter
    InstancePatchStateFilter,
    instancePatchStateFilter,
    ipsfKey,
    ipsfValues,
    ipsfType,

    -- * InventoryAggregator
    InventoryAggregator,
    inventoryAggregator,
    iaGroups,
    iaAggregators,
    iaExpression,

    -- * InventoryDeletionStatusItem
    InventoryDeletionStatusItem,
    inventoryDeletionStatusItem,
    idsiTypeName,
    idsiLastStatusUpdateTime,
    idsiLastStatusMessage,
    idsiDeletionSummary,
    idsiLastStatus,
    idsiDeletionStartTime,
    idsiDeletionId,

    -- * InventoryDeletionSummary
    InventoryDeletionSummary,
    inventoryDeletionSummary,
    idsRemainingCount,
    idsSummaryItems,
    idsTotalCount,

    -- * InventoryDeletionSummaryItem
    InventoryDeletionSummaryItem,
    inventoryDeletionSummaryItem,
    idsiRemainingCount,
    idsiCount,
    idsiVersion,

    -- * InventoryFilter
    InventoryFilter,
    inventoryFilter,
    ifType,
    ifKey,
    ifValues,

    -- * InventoryGroup
    InventoryGroup,
    inventoryGroup,
    igName,
    igFilters,

    -- * InventoryItem
    InventoryItem,
    inventoryItem,
    iiContext,
    iiContentHash,
    iiContent,
    iiTypeName,
    iiSchemaVersion,
    iiCaptureTime,

    -- * InventoryItemAttribute
    InventoryItemAttribute,
    inventoryItemAttribute,
    iiaName,
    iiaDataType,

    -- * InventoryItemSchema
    InventoryItemSchema,
    inventoryItemSchema,
    iisVersion,
    iisDisplayName,
    iisTypeName,
    iisAttributes,

    -- * InventoryResultEntity
    InventoryResultEntity,
    inventoryResultEntity,
    ireData,
    ireId,

    -- * InventoryResultItem
    InventoryResultItem,
    inventoryResultItem,
    iriContentHash,
    iriCaptureTime,
    iriTypeName,
    iriSchemaVersion,
    iriContent,

    -- * LoggingInfo
    LoggingInfo,
    loggingInfo,
    liS3KeyPrefix,
    liS3BucketName,
    liS3Region,

    -- * MaintenanceWindowAutomationParameters
    MaintenanceWindowAutomationParameters,
    maintenanceWindowAutomationParameters,
    mwapParameters,
    mwapDocumentVersion,

    -- * MaintenanceWindowExecution
    MaintenanceWindowExecution,
    maintenanceWindowExecution,
    mweStatus,
    mweStartTime,
    mweWindowExecutionId,
    mweStatusDetails,
    mweEndTime,
    mweWindowId,

    -- * MaintenanceWindowExecutionTaskIdentity
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

    -- * MaintenanceWindowExecutionTaskInvocationIdentity
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

    -- * MaintenanceWindowFilter
    MaintenanceWindowFilter,
    maintenanceWindowFilter,
    mwfValues,
    mwfKey,

    -- * MaintenanceWindowIdentity
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

    -- * MaintenanceWindowIdentityForTarget
    MaintenanceWindowIdentityForTarget,
    maintenanceWindowIdentityForTarget,
    mwiftName,
    mwiftWindowId,

    -- * MaintenanceWindowLambdaParameters
    MaintenanceWindowLambdaParameters,
    maintenanceWindowLambdaParameters,
    mwlpPayload,
    mwlpQualifier,
    mwlpClientContext,

    -- * MaintenanceWindowRunCommandParameters
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

    -- * MaintenanceWindowStepFunctionsParameters
    MaintenanceWindowStepFunctionsParameters,
    maintenanceWindowStepFunctionsParameters,
    mwsfpInput,
    mwsfpName,

    -- * MaintenanceWindowTarget
    MaintenanceWindowTarget,
    maintenanceWindowTarget,
    mResourceType,
    mOwnerInformation,
    mWindowTargetId,
    mName,
    mTargets,
    mDescription,
    mWindowId,

    -- * MaintenanceWindowTask
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

    -- * MaintenanceWindowTaskInvocationParameters
    MaintenanceWindowTaskInvocationParameters,
    maintenanceWindowTaskInvocationParameters,
    mwtipAutomation,
    mwtipStepFunctions,
    mwtipRunCommand,
    mwtipLambda,

    -- * MaintenanceWindowTaskParameterValueExpression
    MaintenanceWindowTaskParameterValueExpression,
    maintenanceWindowTaskParameterValueExpression,
    mwtpveValues,

    -- * NonCompliantSummary
    NonCompliantSummary,
    nonCompliantSummary,
    ncsNonCompliantCount,
    ncsSeveritySummary,

    -- * NotificationConfig
    NotificationConfig,
    notificationConfig,
    ncNotificationEvents,
    ncNotificationType,
    ncNotificationARN,

    -- * OpsAggregator
    OpsAggregator,
    opsAggregator,
    oaTypeName,
    oaAggregators,
    oaValues,
    oaFilters,
    oaAttributeName,
    oaAggregatorType,

    -- * OpsEntity
    OpsEntity,
    opsEntity,
    oeData,
    oeId,

    -- * OpsEntityItem
    OpsEntityItem,
    opsEntityItem,
    oeiContent,
    oeiCaptureTime,

    -- * OpsFilter
    OpsFilter,
    opsFilter,
    ofType,
    ofKey,
    ofValues,

    -- * OpsItem
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

    -- * OpsItemDataValue
    OpsItemDataValue,
    opsItemDataValue,
    oidvValue,
    oidvType,

    -- * OpsItemFilter
    OpsItemFilter,
    opsItemFilter,
    oifKey,
    oifValues,
    oifOperator,

    -- * OpsItemNotification
    OpsItemNotification,
    opsItemNotification,
    oinARN,

    -- * OpsItemSummary
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

    -- * OpsResultAttribute
    OpsResultAttribute,
    opsResultAttribute,
    oraTypeName,

    -- * OutputSource
    OutputSource,
    outputSource,
    osOutputSourceId,
    osOutputSourceType,

    -- * Parameter
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

    -- * ParameterHistory
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

    -- * ParameterInlinePolicy
    ParameterInlinePolicy,
    parameterInlinePolicy,
    pipPolicyType,
    pipPolicyStatus,
    pipPolicyText,

    -- * ParameterMetadata
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

    -- * ParameterStringFilter
    ParameterStringFilter,
    parameterStringFilter,
    psfValues,
    psfOption,
    psfKey,

    -- * ParametersFilter
    ParametersFilter,
    parametersFilter,
    pKey,
    pValues,

    -- * Patch
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

    -- * PatchBaselineIdentity
    PatchBaselineIdentity,
    patchBaselineIdentity,
    pbiBaselineName,
    pbiBaselineDescription,
    pbiOperatingSystem,
    pbiDefaultBaseline,
    pbiBaselineId,

    -- * PatchComplianceData
    PatchComplianceData,
    patchComplianceData,
    pcdCVEIds,
    pcdTitle,
    pcdKBId,
    pcdClassification,
    pcdSeverity,
    pcdState,
    pcdInstalledTime,

    -- * PatchFilter
    PatchFilter,
    patchFilter,
    pfKey,
    pfValues,

    -- * PatchFilterGroup
    PatchFilterGroup,
    patchFilterGroup,
    pfgPatchFilters,

    -- * PatchGroupPatchBaselineMapping
    PatchGroupPatchBaselineMapping,
    patchGroupPatchBaselineMapping,
    pgpbmBaselineIdentity,
    pgpbmPatchGroup,

    -- * PatchOrchestratorFilter
    PatchOrchestratorFilter,
    patchOrchestratorFilter,
    pofValues,
    pofKey,

    -- * PatchRule
    PatchRule,
    patchRule,
    prApproveAfterDays,
    prApproveUntilDate,
    prEnableNonSecurity,
    prComplianceLevel,
    prPatchFilterGroup,

    -- * PatchRuleGroup
    PatchRuleGroup,
    patchRuleGroup,
    prgPatchRules,

    -- * PatchSource
    PatchSource,
    patchSource,
    psName,
    psProducts,
    psConfiguration,

    -- * PatchStatus
    PatchStatus,
    patchStatus,
    psApprovalDate,
    psDeploymentStatus,
    psComplianceLevel,

    -- * ProgressCounters
    ProgressCounters,
    progressCounters,
    pcFailedSteps,
    pcCancelledSteps,
    pcSuccessSteps,
    pcTotalSteps,
    pcTimedOutSteps,

    -- * RelatedOpsItem
    RelatedOpsItem,
    relatedOpsItem,
    roiOpsItemId,

    -- * ResolvedTargets
    ResolvedTargets,
    resolvedTargets,
    rtTruncated,
    rtParameterValues,

    -- * ResourceComplianceSummaryItem
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

    -- * ResourceDataSyncAWSOrganizationsSource
    ResourceDataSyncAWSOrganizationsSource,
    resourceDataSyncAWSOrganizationsSource,
    rdsaosOrganizationalUnits,
    rdsaosOrganizationSourceType,

    -- * ResourceDataSyncDestinationDataSharing
    ResourceDataSyncDestinationDataSharing,
    resourceDataSyncDestinationDataSharing,
    rdsddsDestinationDataSharingType,

    -- * ResourceDataSyncItem
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

    -- * ResourceDataSyncOrganizationalUnit
    ResourceDataSyncOrganizationalUnit,
    resourceDataSyncOrganizationalUnit,
    rdsouOrganizationalUnitId,

    -- * ResourceDataSyncS3Destination
    ResourceDataSyncS3Destination,
    resourceDataSyncS3Destination,
    rdssdPrefix,
    rdssdDestinationDataSharing,
    rdssdAWSKMSKeyARN,
    rdssdBucketName,
    rdssdSyncFormat,
    rdssdRegion,

    -- * ResourceDataSyncSource
    ResourceDataSyncSource,
    resourceDataSyncSource,
    rdssIncludeFutureRegions,
    rdssAWSOrganizationsSource,
    rdssSourceType,
    rdssSourceRegions,

    -- * ResourceDataSyncSourceWithState
    ResourceDataSyncSourceWithState,
    resourceDataSyncSourceWithState,
    rdsswsState,
    rdsswsIncludeFutureRegions,
    rdsswsSourceType,
    rdsswsAWSOrganizationsSource,
    rdsswsSourceRegions,

    -- * ResultAttribute
    ResultAttribute,
    resultAttribute,
    raTypeName,

    -- * S3OutputLocation
    S3OutputLocation,
    s3OutputLocation,
    solOutputS3KeyPrefix,
    solOutputS3Region,
    solOutputS3BucketName,

    -- * S3OutputURL
    S3OutputURL,
    s3OutputURL,
    souOutputURL,

    -- * ScheduledWindowExecution
    ScheduledWindowExecution,
    scheduledWindowExecution,
    sweExecutionTime,
    sweName,
    sweWindowId,

    -- * ServiceSetting
    ServiceSetting,
    serviceSetting,
    ssStatus,
    ssLastModifiedDate,
    ssARN,
    ssSettingId,
    ssLastModifiedUser,
    ssSettingValue,

    -- * Session
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

    -- * SessionFilter
    SessionFilter,
    sessionFilter,
    sfKey,
    sfValue,

    -- * SessionManagerOutputURL
    SessionManagerOutputURL,
    sessionManagerOutputURL,
    smouS3OutputURL,
    smouCloudWatchOutputURL,

    -- * SeveritySummary
    SeveritySummary,
    severitySummary,
    ssLowCount,
    ssUnspecifiedCount,
    ssHighCount,
    ssMediumCount,
    ssInformationalCount,
    ssCriticalCount,

    -- * StepExecution
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

    -- * StepExecutionFilter
    StepExecutionFilter,
    stepExecutionFilter,
    sefKey,
    sefValues,

    -- * Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- * Target
    Target,
    target,
    tValues,
    tKey,

    -- * TargetLocation
    TargetLocation,
    targetLocation,
    tlAccounts,
    tlTargetLocationMaxConcurrency,
    tlTargetLocationMaxErrors,
    tlRegions,
    tlExecutionRoleName,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
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
import Network.AWS.SSM.Types.AutomationType
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
import Network.AWS.SSM.Types.DocumentParameter
import Network.AWS.SSM.Types.DocumentParameterType
import Network.AWS.SSM.Types.DocumentPermissionType
import Network.AWS.SSM.Types.DocumentRequires
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
import Network.AWS.SSM.Types.InstanceAssociationOutputURL
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
import Network.AWS.SSM.Types.OpsItemFilter
import Network.AWS.SSM.Types.OpsItemFilterKey
import Network.AWS.SSM.Types.OpsItemFilterOperator
import Network.AWS.SSM.Types.OpsItemNotification
import Network.AWS.SSM.Types.OpsItemStatus
import Network.AWS.SSM.Types.OpsItemSummary
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
import Network.AWS.SSM.Types.ResourceDataSyncAWSOrganizationsSource
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
import Network.AWS.SSM.Types.S3OutputLocation
import Network.AWS.SSM.Types.S3OutputURL
import Network.AWS.SSM.Types.ScheduledWindowExecution
import Network.AWS.SSM.Types.ServiceSetting
import Network.AWS.SSM.Types.Session
import Network.AWS.SSM.Types.SessionFilter
import Network.AWS.SSM.Types.SessionFilterKey
import Network.AWS.SSM.Types.SessionManagerOutputURL
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
import Network.AWS.Sign.V4

-- | API version @2014-11-06@ of the Amazon Simple Systems Manager (SSM) SDK configuration.
ssm :: Service
ssm =
  Service
    { _svcAbbrev = "SSM",
      _svcSigner = v4,
      _svcPrefix = "ssm",
      _svcVersion = "2014-11-06",
      _svcEndpoint = defaultEndpoint ssm,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "SSM",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
