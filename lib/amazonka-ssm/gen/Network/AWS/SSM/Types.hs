-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _AutomationDefinitionVersionNotFoundException,
    _InvalidDocumentVersion,
    _HierarchyTypeMismatchException,
    _InvalidSchedule,
    _UnsupportedParameterType,
    _InvalidAutomationStatusUpdateException,
    _InvalidPluginName,
    _UnsupportedFeatureRequiredException,
    _InvalidAggregatorException,
    _FeatureNotAvailableException,
    _InvalidAutomationSignalException,
    _ResourceDataSyncCountExceededException,
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
    _OpsItemLimitExceededException,
    _InvalidActivationId,
    _ServiceSettingNotFound,
    _InvalidResultAttributeException,
    _TargetNotConnected,
    _ResourceLimitExceededException,
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
    _OpsItemInvalidParameterException,
    _InvalidResourceId,
    _InvalidAllowedPatternException,
    _InvalidNotificationConfig,
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

    -- * AssociationExecutionFilterKey
    AssociationExecutionFilterKey (..),

    -- * NonCompliantSummary
    NonCompliantSummary (..),
    mkNonCompliantSummary,
    ncsNonCompliantCount,
    ncsSeveritySummary,

    -- * BaselineName
    BaselineName (..),

    -- * MaintenanceWindowLambdaParameters
    MaintenanceWindowLambdaParameters (..),
    mkMaintenanceWindowLambdaParameters,
    mwlpClientContext,
    mwlpPayload,
    mwlpQualifier,

    -- * PatchComplianceDataState
    PatchComplianceDataState (..),

    -- * AssociationId
    AssociationId (..),

    -- * PatchLanguage
    PatchLanguage (..),

    -- * SharedDocumentVersion
    SharedDocumentVersion (..),

    -- * OpsFilterOperatorType
    OpsFilterOperatorType (..),

    -- * OpsItemId
    OpsItemId (..),

    -- * IdempotencyToken
    IdempotencyToken (..),

    -- * InstanceId
    InstanceId (..),

    -- * MaintenanceWindowSchedule
    MaintenanceWindowSchedule (..),

    -- * ComplianceSummaryItem
    ComplianceSummaryItem (..),
    mkComplianceSummaryItem,
    csiComplianceType,
    csiCompliantSummary,
    csiNonCompliantSummary,

    -- * InventoryItemAttributeName
    InventoryItemAttributeName (..),

    -- * CloudWatchLogGroupName
    CloudWatchLogGroupName (..),

    -- * PingStatus
    PingStatus (..),

    -- * AttachmentsSourceValue
    AttachmentsSourceValue (..),

    -- * MaintenanceWindowStepFunctionsInput
    MaintenanceWindowStepFunctionsInput (..),

    -- * ComplianceItemEntry
    ComplianceItemEntry (..),
    mkComplianceItemEntry,
    cieSeverity,
    cieStatus,
    cieDetails,
    cieId,
    cieTitle,

    -- * ParameterMetadata
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

    -- * MaintenanceWindowTaskType
    MaintenanceWindowTaskType (..),

    -- * ParameterStringFilter
    ParameterStringFilter (..),
    mkParameterStringFilter,
    psfKey,
    psfOption,
    psfValues,

    -- * DocumentSchemaVersion
    DocumentSchemaVersion (..),

    -- * PatchVersion
    PatchVersion (..),

    -- * PatchContentUrl
    PatchContentUrl (..),

    -- * CommandFilterValue
    CommandFilterValue (..),

    -- * OpsItemSource
    OpsItemSource (..),

    -- * MaintenanceWindowTaskParameterValueExpression
    MaintenanceWindowTaskParameterValueExpression (..),
    mkMaintenanceWindowTaskParameterValueExpression,
    mwtpveValues,

    -- * CreateAssociationBatchRequestEntry
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

    -- * PatchKbNumber
    PatchKbNumber (..),

    -- * AgentErrorCode
    AgentErrorCode (..),

    -- * ComplianceStringFilterKey
    ComplianceStringFilterKey (..),

    -- * OpsItemDataValueString
    OpsItemDataValueString (..),

    -- * AccountSharingInfo
    AccountSharingInfo (..),
    mkAccountSharingInfo,
    asiAccountId,
    asiSharedDocumentVersion,

    -- * Command
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

    -- * ParameterLabel
    ParameterLabel (..),

    -- * SessionManagerParameterValue
    SessionManagerParameterValue (..),

    -- * ComplianceResourceType
    ComplianceResourceType (..),

    -- * SessionOwner
    SessionOwner (..),

    -- * MaintenanceWindowIdentity
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

    -- * InventoryDeletionSummaryItem
    InventoryDeletionSummaryItem (..),
    mkInventoryDeletionSummaryItem,
    idsiCount,
    idsiRemainingCount,
    idsiVersion,

    -- * MaintenanceWindowExecutionStatus
    MaintenanceWindowExecutionStatus (..),

    -- * DocumentFilter
    DocumentFilter (..),
    mkDocumentFilter,
    dfKey,
    dfValue,

    -- * ScheduledWindowExecution
    ScheduledWindowExecution (..),
    mkScheduledWindowExecution,
    sweExecutionTime,
    sweName,
    sweWindowId,

    -- * StreamUrl
    StreamUrl (..),

    -- * InstancePatchStateFilterKey
    InstancePatchStateFilterKey (..),

    -- * StepExecutionFilterKey
    StepExecutionFilterKey (..),

    -- * PatchRuleGroup
    PatchRuleGroup (..),
    mkPatchRuleGroup,
    prgPatchRules,

    -- * OpsItemDescription
    OpsItemDescription (..),

    -- * ParameterStringFilterValue
    ParameterStringFilterValue (..),

    -- * IPAddress
    IPAddress (..),

    -- * InventoryItem
    InventoryItem (..),
    mkInventoryItem,
    iiTypeName,
    iiSchemaVersion,
    iiCaptureTime,
    iiContent,
    iiContentHash,
    iiContext,

    -- * DocumentType
    DocumentType (..),

    -- * NotificationConfig
    NotificationConfig (..),
    mkNotificationConfig,
    ncNotificationArn,
    ncNotificationEvents,
    ncNotificationType,

    -- * AttributeValue
    AttributeValue (..),

    -- * ResourceId
    ResourceId (..),

    -- * ParameterDescription
    ParameterDescription (..),

    -- * AssociationDescription
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

    -- * InventoryDeletionStatusItem
    InventoryDeletionStatusItem (..),
    mkInventoryDeletionStatusItem,
    idsiDeletionId,
    idsiDeletionStartTime,
    idsiDeletionSummary,
    idsiLastStatus,
    idsiLastStatusMessage,
    idsiLastStatusUpdateTime,
    idsiTypeName,

    -- * S3KeyPrefix
    S3KeyPrefix (..),

    -- * ComplianceTypeName
    ComplianceTypeName (..),

    -- * Tag
    Tag (..),
    mkTag,
    tfKey,
    tfValue,

    -- * ProgressCounters
    ProgressCounters (..),
    mkProgressCounters,
    pcCancelledSteps,
    pcFailedSteps,
    pcSuccessSteps,
    pcTimedOutSteps,
    pcTotalSteps,

    -- * AssociationExecutionTarget
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

    -- * InstanceAssociationOutputLocation
    InstanceAssociationOutputLocation (..),
    mkInstanceAssociationOutputLocation,
    iaolS3Location,

    -- * PatchStringDateTime
    PatchStringDateTime (..),

    -- * OpsItemDataValue
    OpsItemDataValue (..),
    mkOpsItemDataValue,
    oidvType,
    oidvValue,

    -- * DocumentVersionName
    DocumentVersionName (..),

    -- * InventoryFilter
    InventoryFilter (..),
    mkInventoryFilter,
    ifKey,
    ifValues,
    ifType,

    -- * TargetValue
    TargetValue (..),

    -- * OpsEntityItem
    OpsEntityItem (..),
    mkOpsEntityItem,
    oeiCaptureTime,
    oeiContent,

    -- * AttachmentIdentifier
    AttachmentIdentifier (..),

    -- * InstanceInformationFilterKey
    InstanceInformationFilterKey (..),

    -- * AssociationStatusName
    AssociationStatusName (..),

    -- * ResourceType
    ResourceType (..),

    -- * InventoryGroupName
    InventoryGroupName (..),

    -- * DocumentARN
    DocumentARN (..),

    -- * StandardErrorContent
    StandardErrorContent (..),

    -- * ParametersFilter
    ParametersFilter (..),
    mkParametersFilter,
    pKey,
    pValues,

    -- * MaintenanceWindowFilterValue
    MaintenanceWindowFilterValue (..),

    -- * PatchMsrcNumber
    PatchMsrcNumber (..),

    -- * FailureDetails
    FailureDetails (..),
    mkFailureDetails,
    fdDetails,
    fdFailureStage,
    fdFailureType,

    -- * ClientToken
    ClientToken (..),

    -- * ParameterValue
    ParameterValue (..),

    -- * ComplianceResourceId
    ComplianceResourceId (..),

    -- * OpsEntityId
    OpsEntityId (..),

    -- * OutputSource
    OutputSource (..),
    mkOutputSource,
    osOutputSourceId,
    osOutputSourceType,

    -- * S3OutputLocation
    S3OutputLocation (..),
    mkS3OutputLocation,
    solOutputS3BucketName,
    solOutputS3KeyPrefix,
    solOutputS3Region,

    -- * SessionStatus
    SessionStatus (..),

    -- * RebootOption
    RebootOption (..),

    -- * PatchFilterValue
    PatchFilterValue (..),

    -- * ComplianceItemId
    ComplianceItemId (..),

    -- * PatchProduct
    PatchProduct (..),

    -- * AttachmentInformation
    AttachmentInformation (..),
    mkAttachmentInformation,
    aiName,

    -- * ResourceDataSyncOrganizationalUnit
    ResourceDataSyncOrganizationalUnit (..),
    mkResourceDataSyncOrganizationalUnit,
    rdsouOrganizationalUnitId,

    -- * MaintenanceWindowStepFunctionsName
    MaintenanceWindowStepFunctionsName (..),

    -- * DocumentHashType
    DocumentHashType (..),

    -- * MaintenanceWindowExecutionStatusDetails
    MaintenanceWindowExecutionStatusDetails (..),

    -- * OpsAggregator
    OpsAggregator (..),
    mkOpsAggregator,
    oaAggregatorType,
    oaAggregators,
    oaAttributeName,
    oaFilters,
    oaTypeName,
    oaValues,

    -- * ResourceDataSyncS3Region
    ResourceDataSyncS3Region (..),

    -- * MaintenanceWindowTargetId
    MaintenanceWindowTargetId (..),

    -- * OwnerInformation
    OwnerInformation (..),

    -- * AutomationExecution
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

    -- * CloudWatchOutputConfig
    CloudWatchOutputConfig (..),
    mkCloudWatchOutputConfig,
    cwocCloudWatchLogGroupName,
    cwocCloudWatchOutputEnabled,

    -- * String
    String (..),

    -- * OpsItemSummary
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

    -- * InstanceInformationFilter
    InstanceInformationFilter (..),
    mkInstanceInformationFilter,
    iifKey,
    iifValueSet,

    -- * AutomationParameterValue
    AutomationParameterValue (..),

    -- * PSParameterName
    PSParameterName (..),

    -- * PatchComplianceLevel
    PatchComplianceLevel (..),

    -- * UUID
    UUID (..),

    -- * MaintenanceWindowRunCommandParameters
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

    -- * DefaultInstanceName
    DefaultInstanceName (..),

    -- * BaselineDescription
    BaselineDescription (..),

    -- * OperatingSystem
    OperatingSystem (..),

    -- * InventoryResultEntity
    InventoryResultEntity (..),
    mkInventoryResultEntity,
    ireData,
    ireId,

    -- * PatchOrchestratorFilter
    PatchOrchestratorFilter (..),
    mkPatchOrchestratorFilter,
    pofKey,
    pofValues,

    -- * S3Region
    S3Region (..),

    -- * PatchClassification
    PatchClassification (..),

    -- * StepExecution
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

    -- * PatchRule
    PatchRule (..),
    mkPatchRule,
    prPatchFilterGroup,
    prApproveAfterDays,
    prApproveUntilDate,
    prComplianceLevel,
    prEnableNonSecurity,

    -- * InventoryItemTypeName
    InventoryItemTypeName (..),

    -- * InstancePatchState
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

    -- * MaintenanceWindowExecutionTaskInvocationIdentity
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

    -- * PatchCVEIds
    PatchCVEIds (..),

    -- * ResourceDataSyncSourceType
    ResourceDataSyncSourceType (..),

    -- * DocumentName
    DocumentName (..),

    -- * StopType
    StopType (..),

    -- * InstanceInformationStringFilter
    InstanceInformationStringFilter (..),
    mkInstanceInformationStringFilter,
    iisfKey,
    iisfValues,

    -- * PatchSourceProduct
    PatchSourceProduct (..),

    -- * DocumentKeyValuesFilter
    DocumentKeyValuesFilter (..),
    mkDocumentKeyValuesFilter,
    dkvfKey,
    dkvfValues,

    -- * DocumentFilterKey
    DocumentFilterKey (..),

    -- * MaintenanceWindowStepFunctionsParameters
    MaintenanceWindowStepFunctionsParameters (..),
    mkMaintenanceWindowStepFunctionsParameters,
    mwsfpInput,
    mwsfpName,

    -- * ResourceDataSyncSourceRegion
    ResourceDataSyncSourceRegion (..),

    -- * ComplianceItemTitle
    ComplianceItemTitle (..),

    -- * PatchAction
    PatchAction (..),

    -- * ParameterInlinePolicy
    ParameterInlinePolicy (..),
    mkParameterInlinePolicy,
    pipPolicyStatus,
    pipPolicyText,
    pipPolicyType,

    -- * AttachmentHash
    AttachmentHash (..),

    -- * PatchOperationType
    PatchOperationType (..),

    -- * ParameterKeyId
    ParameterKeyId (..),

    -- * PatchOrchestratorFilterValue
    PatchOrchestratorFilterValue (..),

    -- * AssociationExecutionFilter
    AssociationExecutionFilter (..),
    mkAssociationExecutionFilter,
    aefKey,
    aefValue,
    aefType,

    -- * ParameterTier
    ParameterTier (..),

    -- * OpsDataTypeName
    OpsDataTypeName (..),

    -- * CommandStatus
    CommandStatus (..),

    -- * PatchId
    PatchId (..),

    -- * MaintenanceWindowExecution
    MaintenanceWindowExecution (..),
    mkMaintenanceWindowExecution,
    mweEndTime,
    mweStartTime,
    mweStatus,
    mweStatusDetails,
    mweWindowExecutionId,
    mweWindowId,

    -- * MaintenanceWindowExecutionTaskId
    MaintenanceWindowExecutionTaskId (..),

    -- * PatchSource
    PatchSource (..),
    mkPatchSource,
    psName,
    psProducts,
    psConfiguration,

    -- * MaintenanceWindowResourceType
    MaintenanceWindowResourceType (..),

    -- * Url
    Url (..),

    -- * PatchBaselineIdentity
    PatchBaselineIdentity (..),
    mkPatchBaselineIdentity,
    pbiBaselineDescription,
    pbiBaselineId,
    pbiBaselineName,
    pbiDefaultBaseline,
    pbiOperatingSystem,

    -- * CompliantSummary
    CompliantSummary (..),
    mkCompliantSummary,
    csCompliantCount,
    csSeveritySummary,

    -- * ParameterHistory
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

    -- * StatusAdditionalInfo
    StatusAdditionalInfo (..),

    -- * OpsFilterKey
    OpsFilterKey (..),

    -- * AssociationSyncCompliance
    AssociationSyncCompliance (..),

    -- * DocumentKeyValuesFilterValue
    DocumentKeyValuesFilterValue (..),

    -- * ServiceSetting
    ServiceSetting (..),
    mkServiceSetting,
    ssARN,
    ssLastModifiedDate,
    ssLastModifiedUser,
    ssSettingId,
    ssSettingValue,
    ssStatus,

    -- * PatchCVEId
    PatchCVEId (..),

    -- * PatchDeploymentStatus
    PatchDeploymentStatus (..),

    -- * ResourceCountByStatus
    ResourceCountByStatus (..),

    -- * MaintenanceWindowName
    MaintenanceWindowName (..),

    -- * CommandPlugin
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

    -- * InvocationTraceOutput
    InvocationTraceOutput (..),

    -- * MaintenanceWindowExecutionId
    MaintenanceWindowExecutionId (..),

    -- * ResourceDataSyncS3Format
    ResourceDataSyncS3Format (..),

    -- * ServiceSettingId
    ServiceSettingId (..),

    -- * OpsItemDataKey
    OpsItemDataKey (..),

    -- * Patch
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

    -- * MaintenanceWindowTaskInvocationParameters
    MaintenanceWindowTaskInvocationParameters (..),
    mkMaintenanceWindowTaskInvocationParameters,
    mwtipAutomation,
    mwtipLambda,
    mwtipRunCommand,
    mwtipStepFunctions,

    -- * SessionFilter
    SessionFilter (..),
    mkSessionFilter,
    sfKey,
    sfValue,

    -- * SignalType
    SignalType (..),

    -- * AttachmentName
    AttachmentName (..),

    -- * MaintenanceWindowTaskParameterValue
    MaintenanceWindowTaskParameterValue (..),

    -- * InventoryItemCaptureTime
    InventoryItemCaptureTime (..),

    -- * InstanceInformationFilterValue
    InstanceInformationFilterValue (..),

    -- * DocumentParameter
    DocumentParameter (..),
    mkDocumentParameter,
    dpDefaultValue,
    dpDescription,
    dpName,
    dpType,

    -- * MaintenanceWindowFilterKey
    MaintenanceWindowFilterKey (..),

    -- * ResourceComplianceSummaryItem
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

    -- * StatusDetails
    StatusDetails (..),

    -- * TargetKey
    TargetKey (..),

    -- * OpsAggregatorValue
    OpsAggregatorValue (..),

    -- * InventoryFilterValue
    InventoryFilterValue (..),

    -- * OpsItemFilter
    OpsItemFilter (..),
    mkOpsItemFilter,
    oifKey,
    oifValues,
    oifOperator,

    -- * ResourceDataSyncDestinationDataSharingType
    ResourceDataSyncDestinationDataSharingType (..),

    -- * ActivationId
    ActivationId (..),

    -- * DocumentDescription
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

    -- * ParameterType
    ParameterType (..),

    -- * AssociationExecutionTargetsFilter
    AssociationExecutionTargetsFilter (..),
    mkAssociationExecutionTargetsFilter,
    aetfKey,
    aetfValue,

    -- * ComplianceStatus
    ComplianceStatus (..),

    -- * ParameterDataType
    ParameterDataType (..),

    -- * MaintenanceWindowTaskId
    MaintenanceWindowTaskId (..),

    -- * ParametersFilterValue
    ParametersFilterValue (..),

    -- * SessionManagerS3OutputUrl
    SessionManagerS3OutputUrl (..),

    -- * SessionManagerCloudWatchOutputUrl
    SessionManagerCloudWatchOutputUrl (..),

    -- * InventoryAggregatorExpression
    InventoryAggregatorExpression (..),

    -- * SessionState
    SessionState (..),

    -- * SessionFilterKey
    SessionFilterKey (..),

    -- * AutomationExecutionStatus
    AutomationExecutionStatus (..),

    -- * TargetType
    TargetType (..),

    -- * OpsItemFilterOperator
    OpsItemFilterOperator (..),

    -- * ActivationCode
    ActivationCode (..),

    -- * PatchFilterGroup
    PatchFilterGroup (..),
    mkPatchFilterGroup,
    pfgPatchFilters,

    -- * Account
    Account (..),

    -- * OpsItemDataType
    OpsItemDataType (..),

    -- * AssociationFilter
    AssociationFilter (..),
    mkAssociationFilter,
    afKey,
    afValue,

    -- * ResourceDataSyncS3Destination
    ResourceDataSyncS3Destination (..),
    mkResourceDataSyncS3Destination,
    rdssdBucketName,
    rdssdSyncFormat,
    rdssdRegion,
    rdssdAWSKMSKeyARN,
    rdssdDestinationDataSharing,
    rdssdPrefix,

    -- * OpsAggregatorValueKey
    OpsAggregatorValueKey (..),

    -- * TargetLocation
    TargetLocation (..),
    mkTargetLocation,
    tlAccounts,
    tlExecutionRoleName,
    tlRegions,
    tlTargetLocationMaxConcurrency,
    tlTargetLocationMaxErrors,

    -- * ResourceTypeForTagging
    ResourceTypeForTagging (..),

    -- * AccountId
    AccountId (..),

    -- * LastResourceDataSyncMessage
    LastResourceDataSyncMessage (..),

    -- * PatchSet
    PatchSet (..),

    -- * MaintenanceWindowExecutionTaskInvocationParameters
    MaintenanceWindowExecutionTaskInvocationParameters (..),

    -- * Activation
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

    -- * InventoryItemContentHash
    InventoryItemContentHash (..),

    -- * NextToken
    NextToken (..),

    -- * InstanceAssociationOutputUrl
    InstanceAssociationOutputUrl (..),
    mkInstanceAssociationOutputUrl,
    iaouS3OutputUrl,

    -- * InstanceTagName
    InstanceTagName (..),

    -- * OpsFilter
    OpsFilter (..),
    mkOpsFilter,
    ofKey,
    ofValues,
    ofType,

    -- * DocumentPermissionType
    DocumentPermissionType (..),

    -- * SessionDetails
    SessionDetails (..),

    -- * MaintenanceWindowAutomationParameters
    MaintenanceWindowAutomationParameters (..),
    mkMaintenanceWindowAutomationParameters,
    mwapDocumentVersion,
    mwapParameters,

    -- * MaintenanceWindowTask
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

    -- * S3OutputUrl
    S3OutputUrl (..),
    mkS3OutputUrl,
    souOutputUrl,

    -- * OpsItemNotification
    OpsItemNotification (..),
    mkOpsItemNotification,
    oinArn,

    -- * MaintenanceWindowId
    MaintenanceWindowId (..),

    -- * DocumentIdentifier
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

    -- * DocumentFormat
    DocumentFormat (..),

    -- * InventoryItemSchemaVersion
    InventoryItemSchemaVersion (..),

    -- * CommandInvocation
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

    -- * MaxErrors
    MaxErrors (..),

    -- * ComplianceQueryOperatorType
    ComplianceQueryOperatorType (..),

    -- * SeveritySummary
    SeveritySummary (..),
    mkSeveritySummary,
    ssCriticalCount,
    ssHighCount,
    ssInformationalCount,
    ssLowCount,
    ssMediumCount,
    ssUnspecifiedCount,

    -- * InventoryTypeDisplayName
    InventoryTypeDisplayName (..),

    -- * MaintenanceWindowDescription
    MaintenanceWindowDescription (..),

    -- * CommandFilter
    CommandFilter (..),
    mkCommandFilter,
    cfKey,
    cfValue,

    -- * SessionTarget
    SessionTarget (..),

    -- * DocumentStatusInformation
    DocumentStatusInformation (..),

    -- * OpsFilterValue
    OpsFilterValue (..),

    -- * InstallOverrideList
    InstallOverrideList (..),

    -- * AttachmentsSource
    AttachmentsSource (..),
    mkAttachmentsSource,
    asfKey,
    asfName,
    asfValues,

    -- * PSParameterSelector
    PSParameterSelector (..),

    -- * OpsEntity
    OpsEntity (..),
    mkOpsEntity,
    oeData,
    oeId,

    -- * AutomationType
    AutomationType (..),

    -- * StatusMessage
    StatusMessage (..),

    -- * ScheduleExpression
    ScheduleExpression (..),

    -- * AssociationComplianceSeverity
    AssociationComplianceSeverity (..),

    -- * ComplianceExecutionType
    ComplianceExecutionType (..),

    -- * OpsDataAttributeName
    OpsDataAttributeName (..),

    -- * PlatformType
    PlatformType (..),

    -- * BatchErrorMessage
    BatchErrorMessage (..),

    -- * OutputSourceId
    OutputSourceId (..),

    -- * MaintenanceWindowTimezone
    MaintenanceWindowTimezone (..),

    -- * AttachmentUrl
    AttachmentUrl (..),

    -- * InstanceAssociationStatusInfo
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

    -- * ComplianceExecutionId
    ComplianceExecutionId (..),

    -- * TargetMapKey
    TargetMapKey (..),

    -- * InventoryDeletionStatus
    InventoryDeletionStatus (..),

    -- * OutputSourceType
    OutputSourceType (..),

    -- * Version
    Version (..),

    -- * CommandId
    CommandId (..),

    -- * ConnectionStatus
    ConnectionStatus (..),

    -- * AssociationOverview
    AssociationOverview (..),
    mkAssociationOverview,
    aoAssociationStatusAggregatedCount,
    aoDetailedStatus,
    aoStatus,

    -- * InventoryDeletionSummary
    InventoryDeletionSummary (..),
    mkInventoryDeletionSummary,
    idsRemainingCount,
    idsSummaryItems,
    idsTotalCount,

    -- * MaintenanceWindowTaskParameterName
    MaintenanceWindowTaskParameterName (..),

    -- * ActivationDescription
    ActivationDescription (..),

    -- * PatchStatus
    PatchStatus (..),
    mkPatchStatus,
    psApprovalDate,
    psComplianceLevel,
    psDeploymentStatus,

    -- * Fault
    Fault (..),

    -- * OpsItemSeverity
    OpsItemSeverity (..),

    -- * CommandPluginStatus
    CommandPluginStatus (..),

    -- * AutomationExecutionFilterKey
    AutomationExecutionFilterKey (..),

    -- * ResourceDataSyncState
    ResourceDataSyncState (..),

    -- * OpsItemFilterValue
    OpsItemFilterValue (..),

    -- * DescribeActivationsFilter
    DescribeActivationsFilter (..),
    mkDescribeActivationsFilter,
    dafFilterKey,
    dafFilterValues,

    -- * ResourceDataSyncName
    ResourceDataSyncName (..),

    -- * ResolvedTargets
    ResolvedTargets (..),
    mkResolvedTargets,
    rtParameterValues,
    rtTruncated,

    -- * OpsItemStatus
    OpsItemStatus (..),

    -- * DocumentHash
    DocumentHash (..),

    -- * DocumentVersion
    DocumentVersion (..),

    -- * AssociationVersionInfo
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

    -- * InstanceInformation
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

    -- * MaintenanceWindowStringDateTime
    MaintenanceWindowStringDateTime (..),

    -- * AssociationStatus
    AssociationStatus (..),
    mkAssociationStatus,
    asDate,
    asName,
    asMessage,
    asAdditionalInfo,

    -- * CalendarNameOrARN
    CalendarNameOrARN (..),

    -- * ResourceDataSyncSourceWithState
    ResourceDataSyncSourceWithState (..),
    mkResourceDataSyncSourceWithState,
    rdsswsAwsOrganizationsSource,
    rdsswsIncludeFutureRegions,
    rdsswsSourceRegions,
    rdsswsSourceType,
    rdsswsState,

    -- * ResultAttribute
    ResultAttribute (..),
    mkResultAttribute,
    raTypeName,

    -- * PatchSeverity
    PatchSeverity (..),

    -- * CalendarState
    CalendarState (..),

    -- * ResourceDataSyncAwsOrganizationsSource
    ResourceDataSyncAwsOrganizationsSource (..),
    mkResourceDataSyncAwsOrganizationsSource,
    rdsaosOrganizationSourceType,
    rdsaosOrganizationalUnits,

    -- * OpsEntityItemKey
    OpsEntityItemKey (..),

    -- * AutomationExecutionFilter
    AutomationExecutionFilter (..),
    mkAutomationExecutionFilter,
    aKey,
    aValues,

    -- * LoggingInfo
    LoggingInfo (..),
    mkLoggingInfo,
    liS3BucketName,
    liS3Region,
    liS3KeyPrefix,

    -- * InventoryGroup
    InventoryGroup (..),
    mkInventoryGroup,
    igName,
    igFilters,

    -- * MaintenanceWindowIdentityForTarget
    MaintenanceWindowIdentityForTarget (..),
    mkMaintenanceWindowIdentityForTarget,
    mwiftName,
    mwiftWindowId,

    -- * ResourceDataSyncType
    ResourceDataSyncType (..),

    -- * ParametersFilterKey
    ParametersFilterKey (..),

    -- * StandardOutputContent
    StandardOutputContent (..),

    -- * NotificationType
    NotificationType (..),

    -- * TagKey
    TagKey (..),

    -- * CommandFilterKey
    CommandFilterKey (..),

    -- * ResourceDataSyncSource
    ResourceDataSyncSource (..),
    mkResourceDataSyncSource,
    rdssSourceType,
    rdssSourceRegions,
    rdssAwsOrganizationsSource,
    rdssIncludeFutureRegions,

    -- * Region
    Region (..),

    -- * AttachmentsSourceKey
    AttachmentsSourceKey (..),

    -- * NotificationArn
    NotificationArn (..),

    -- * AllowedPattern
    AllowedPattern (..),

    -- * MaintenanceWindowExecutionTaskIdentity
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

    -- * ComplianceStringFilter
    ComplianceStringFilter (..),
    mkComplianceStringFilter,
    csfKey,
    csfType,
    csfValues,

    -- * AutomationExecutionMetadata
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

    -- * InstancePatchStateOperatorType
    InstancePatchStateOperatorType (..),

    -- * MaintenanceWindowTarget
    MaintenanceWindowTarget (..),
    mkMaintenanceWindowTarget,
    mDescription,
    mName,
    mOwnerInformation,
    mResourceType,
    mTargets,
    mWindowId,
    mWindowTargetId,

    -- * InstancePatchStateFilter
    InstancePatchStateFilter (..),
    mkInstancePatchStateFilter,
    ipsfKey,
    ipsfValues,
    ipsfType,

    -- * StepExecutionFilter
    StepExecutionFilter (..),
    mkStepExecutionFilter,
    sefKey,
    sefValues,

    -- * ComputerName
    ComputerName (..),

    -- * InventoryQueryOperatorType
    InventoryQueryOperatorType (..),

    -- * TokenValue
    TokenValue (..),

    -- * AutomationExecutionId
    AutomationExecutionId (..),

    -- * DocumentDefaultVersionDescription
    DocumentDefaultVersionDescription (..),
    mkDocumentDefaultVersionDescription,
    ddvdDefaultVersion,
    ddvdDefaultVersionName,
    ddvdName,

    -- * AttachmentContent
    AttachmentContent (..),
    mkAttachmentContent,
    acHash,
    acHashType,
    acName,
    acSize,
    acUrl,

    -- * MaintenanceWindowTaskTargetId
    MaintenanceWindowTaskTargetId (..),

    -- * ComplianceItem
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

    -- * ResourceDataSyncItem
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

    -- * InstanceAssociation
    InstanceAssociation (..),
    mkInstanceAssociation,
    iaAssociationId,
    iaAssociationVersion,
    iaContent,
    iaInstanceId,

    -- * PatchGroupPatchBaselineMapping
    PatchGroupPatchBaselineMapping (..),
    mkPatchGroupPatchBaselineMapping,
    pgpbmBaselineIdentity,
    pgpbmPatchGroup,

    -- * AutomationTargetParameterName
    AutomationTargetParameterName (..),

    -- * InventoryItemAttribute
    InventoryItemAttribute (..),
    mkInventoryItemAttribute,
    iiaName,
    iiaDataType,

    -- * ComplianceItemContentHash
    ComplianceItemContentHash (..),

    -- * AssociationVersion
    AssociationVersion (..),

    -- * StepExecutionFilterValue
    StepExecutionFilterValue (..),

    -- * AssociationResourceType
    AssociationResourceType (..),

    -- * AttachmentHashType
    AttachmentHashType (..),

    -- * MaintenanceWindowExecutionTaskInvocationId
    MaintenanceWindowExecutionTaskInvocationId (..),

    -- * ComplianceExecutionSummary
    ComplianceExecutionSummary (..),
    mkComplianceExecutionSummary,
    cesExecutionTime,
    cesExecutionId,
    cesExecutionType,

    -- * InstancePatchStateFilterValue
    InstancePatchStateFilterValue (..),

    -- * CommandInvocationStatus
    CommandInvocationStatus (..),

    -- * ValidNextStep
    ValidNextStep (..),

    -- * LastResourceDataSyncStatus
    LastResourceDataSyncStatus (..),

    -- * DocumentStatus
    DocumentStatus (..),

    -- * Product
    Product (..),

    -- * SessionManagerParameterName
    SessionManagerParameterName (..),

    -- * DocumentVersionInfo
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

    -- * AttributeName
    AttributeName (..),

    -- * SnapshotDownloadUrl
    SnapshotDownloadUrl (..),

    -- * StatusName
    StatusName (..),

    -- * PatchComplianceData
    PatchComplianceData (..),
    mkPatchComplianceData,
    pcdTitle,
    pcdKBId,
    pcdClassification,
    pcdSeverity,
    pcdState,
    pcdInstalledTime,
    pcdCVEIds,

    -- * AssociationExecution
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

    -- * InventoryAggregator
    InventoryAggregator (..),
    mkInventoryAggregator,
    iaAggregators,
    iaExpression,
    iaGroups,

    -- * SessionId
    SessionId (..),

    -- * EffectivePatch
    EffectivePatch (..),
    mkEffectivePatch,
    epPatch,
    epPatchStatus,

    -- * Comment
    Comment (..),

    -- * TargetMapValue
    TargetMapValue (..),

    -- * AssociationExecutionTargetsFilterKey
    AssociationExecutionTargetsFilterKey (..),

    -- * ParameterName
    ParameterName (..),

    -- * InventoryItemSchema
    InventoryItemSchema (..),
    mkInventoryItemSchema,
    iisTypeName,
    iisAttributes,
    iisDisplayName,
    iisVersion,

    -- * AssociationName
    AssociationName (..),

    -- * RelatedOpsItem
    RelatedOpsItem (..),
    mkRelatedOpsItem,
    roiOpsItemId,

    -- * DocumentParameterType
    DocumentParameterType (..),

    -- * InstanceAggregatedAssociationOverview
    InstanceAggregatedAssociationOverview (..),
    mkInstanceAggregatedAssociationOverview,
    iaaoDetailedStatus,
    iaaoInstanceAssociationStatusAggregatedCount,

    -- * BaselineId
    BaselineId (..),

    -- * OpsItemFilterKey
    OpsItemFilterKey (..),

    -- * DocumentVersionNumber
    DocumentVersionNumber (..),

    -- * ComplianceUploadType
    ComplianceUploadType (..),

    -- * InventoryResultItem
    InventoryResultItem (..),
    mkInventoryResultItem,
    iriTypeName,
    iriSchemaVersion,
    iriContent,
    iriCaptureTime,
    iriContentHash,

    -- * AssociationFilterKey
    AssociationFilterKey (..),

    -- * Session
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

    -- * AutomationExecutionFilterValue
    AutomationExecutionFilterValue (..),

    -- * ComplianceSeverity
    ComplianceSeverity (..),

    -- * ComplianceFilterValue
    ComplianceFilterValue (..),

    -- * PatchFilter
    PatchFilter (..),
    mkPatchFilter,
    pfKey,
    pfValues,

    -- * InventoryAttributeDataType
    InventoryAttributeDataType (..),

    -- * DescribeActivationsFilterKeys
    DescribeActivationsFilterKeys (..),

    -- * FailedCreateAssociation
    FailedCreateAssociation (..),
    mkFailedCreateAssociation,
    fcaEntry,
    fcaFault,
    fcaMessage,

    -- * PatchAdvisoryId
    PatchAdvisoryId (..),

    -- * MaintenanceWindowExecutionTaskExecutionId
    MaintenanceWindowExecutionTaskExecutionId (..),

    -- * ResourceDataSyncDestinationDataSharing
    ResourceDataSyncDestinationDataSharing (..),
    mkResourceDataSyncDestinationDataSharing,
    rdsddsDestinationDataSharingType,

    -- * PSParameterValue
    PSParameterValue (..),

    -- * S3BucketName
    S3BucketName (..),

    -- * PatchGroup
    PatchGroup (..),

    -- * PatchProperty
    PatchProperty (..),

    -- * Parameter
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

    -- * Association
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

    -- * OpsItemCategory
    OpsItemCategory (..),

    -- * MaxConcurrency
    MaxConcurrency (..),

    -- * InventoryResultItemKey
    InventoryResultItemKey (..),

    -- * DocumentRequires
    DocumentRequires (..),
    mkDocumentRequires,
    drName,
    drVersion,

    -- * OpsItem
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

    -- * PatchFilterKey
    PatchFilterKey (..),

    -- * AssociationFilterOperatorType
    AssociationFilterOperatorType (..),

    -- * SnapshotId
    SnapshotId (..),

    -- * NotificationEvent
    NotificationEvent (..),

    -- * InventorySchemaDeleteOption
    InventorySchemaDeleteOption (..),

    -- * ExecutionRoleName
    ExecutionRoleName (..),

    -- * Target
    Target (..),
    mkTarget,
    tKey,
    tValues,

    -- * PatchBugzillaId
    PatchBugzillaId (..),

    -- * OpsResultAttribute
    OpsResultAttribute (..),
    mkOpsResultAttribute,
    oraTypeName,

    -- * MaintenanceWindowFilter
    MaintenanceWindowFilter (..),
    mkMaintenanceWindowFilter,
    mwfKey,
    mwfValues,

    -- * ExecutionMode
    ExecutionMode (..),

    -- * IamRole
    IamRole (..),

    -- * SessionManagerOutputUrl
    SessionManagerOutputUrl (..),
    mkSessionManagerOutputUrl,
    smouCloudWatchOutputUrl,
    smouS3OutputUrl,

    -- * ServiceRole
    ServiceRole (..),

    -- * AutomationParameterKey
    AutomationParameterKey (..),

    -- * WindowId
    WindowId (..),

    -- * Message
    Message (..),

    -- * ClientContext
    ClientContext (..),

    -- * Qualifier
    Qualifier (..),

    -- * WindowExecutionId
    WindowExecutionId (..),

    -- * Name
    Name (..),

    -- * ComplianceType
    ComplianceType (..),

    -- * Description
    Description (..),

    -- * Source
    Source (..),

    -- * Title
    Title (..),

    -- * Category
    Category (..),

    -- * Severity
    Severity (..),

    -- * Id
    Id (..),

    -- * DataType
    DataType (..),

    -- * KeyId
    KeyId (..),

    -- * LastModifiedUser
    LastModifiedUser (..),

    -- * TypeName
    TypeName (..),

    -- * Key
    Key (..),

    -- * Option
    Option (..),

    -- * TargetParameterName
    TargetParameterName (..),

    -- * SyncName
    SyncName (..),

    -- * OutputS3BucketName
    OutputS3BucketName (..),

    -- * OutputS3KeyPrefix
    OutputS3KeyPrefix (..),

    -- * OutputS3Region
    OutputS3Region (..),

    -- * EndDate
    EndDate (..),

    -- * NextExecutionTime
    NextExecutionTime (..),

    -- * ScheduleTimezone
    ScheduleTimezone (..),

    -- * StartDate
    StartDate (..),

    -- * Value
    Value (..),

    -- * ExecutionTime
    ExecutionTime (..),

    -- * WindowTargetId
    WindowTargetId (..),

    -- * SettingId
    SettingId (..),

    -- * SchemaVersion
    SchemaVersion (..),

    -- * CaptureTime
    CaptureTime (..),

    -- * ContentHash
    ContentHash (..),

    -- * DeletionId
    DeletionId (..),

    -- * LastStatusMessage
    LastStatusMessage (..),

    -- * Content
    Content (..),

    -- * VersionName
    VersionName (..),

    -- * AtTime
    AtTime (..),

    -- * DetailedStatus
    DetailedStatus (..),

    -- * ExecutionId
    ExecutionId (..),

    -- * Status
    Status (..),

    -- * FailureStage
    FailureStage (..),

    -- * FailureType
    FailureType (..),

    -- * OrganizationalUnitId
    OrganizationalUnitId (..),

    -- * AggregatorType
    AggregatorType (..),

    -- * CurrentAction
    CurrentAction (..),

    -- * CurrentStepName
    CurrentStepName (..),

    -- * ExecutedBy
    ExecutedBy (..),

    -- * FailureMessage
    FailureMessage (..),

    -- * ParentAutomationExecutionId
    ParentAutomationExecutionId (..),

    -- * ServiceRoleArn
    ServiceRoleArn (..),

    -- * SyncType
    SyncType (..),

    -- * Action
    Action (..),

    -- * StatusInformation
    StatusInformation (..),

    -- * InvocationId
    InvocationId (..),

    -- * TaskExecutionId
    TaskExecutionId (..),

    -- * ExecutionElapsedTime
    ExecutionElapsedTime (..),

    -- * ExecutionEndDateTime
    ExecutionEndDateTime (..),

    -- * ExecutionStartDateTime
    ExecutionStartDateTime (..),

    -- * PluginName
    PluginName (..),

    -- * StandardErrorUrl
    StandardErrorUrl (..),

    -- * StandardOutputUrl
    StandardOutputUrl (..),

    -- * NextTransitionTime
    NextTransitionTime (..),

    -- * TaskArn
    TaskArn (..),

    -- * WindowTaskId
    WindowTaskId (..),

    -- * SettingValue
    SettingValue (..),

    -- * Configuration
    Configuration (..),

    -- * Output
    Output (..),

    -- * Arch
    Arch (..),

    -- * MsrcSeverity
    MsrcSeverity (..),

    -- * ProductFamily
    ProductFamily (..),

    -- * Release
    Release (..),

    -- * Repository
    Repository (..),

    -- * Vendor
    Vendor (..),

    -- * DefaultValue
    DefaultValue (..),

    -- * Policies
    Policies (..),

    -- * DefaultVersion
    DefaultVersion (..),

    -- * Hash
    Hash (..),

    -- * LatestVersion
    LatestVersion (..),

    -- * Owner
    Owner (..),

    -- * Sha1
    Sha1 (..),

    -- * BucketName
    BucketName (..),

    -- * AWSKMSKeyARN
    AWSKMSKeyARN (..),

    -- * Prefix
    Prefix (..),

    -- * TargetLocationMaxConcurrency
    TargetLocationMaxConcurrency (..),

    -- * TargetLocationMaxErrors
    TargetLocationMaxErrors (..),

    -- * ExecutionSummary
    ExecutionSummary (..),

    -- * OrganizationSourceType
    OrganizationSourceType (..),
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import Network.AWS.SSM.Types.AWSKMSKeyARN
import Network.AWS.SSM.Types.Account
import Network.AWS.SSM.Types.AccountId
import Network.AWS.SSM.Types.AccountSharingInfo
import Network.AWS.SSM.Types.Action
import Network.AWS.SSM.Types.Activation
import Network.AWS.SSM.Types.ActivationCode
import Network.AWS.SSM.Types.ActivationDescription
import Network.AWS.SSM.Types.ActivationId
import Network.AWS.SSM.Types.AgentErrorCode
import Network.AWS.SSM.Types.AggregatorType
import Network.AWS.SSM.Types.AllowedPattern
import Network.AWS.SSM.Types.Arch
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
import Network.AWS.SSM.Types.AssociationId
import Network.AWS.SSM.Types.AssociationName
import Network.AWS.SSM.Types.AssociationOverview
import Network.AWS.SSM.Types.AssociationResourceType
import Network.AWS.SSM.Types.AssociationStatus
import Network.AWS.SSM.Types.AssociationStatusName
import Network.AWS.SSM.Types.AssociationSyncCompliance
import Network.AWS.SSM.Types.AssociationVersion
import Network.AWS.SSM.Types.AssociationVersionInfo
import Network.AWS.SSM.Types.AtTime
import Network.AWS.SSM.Types.AttachmentContent
import Network.AWS.SSM.Types.AttachmentHash
import Network.AWS.SSM.Types.AttachmentHashType
import Network.AWS.SSM.Types.AttachmentIdentifier
import Network.AWS.SSM.Types.AttachmentInformation
import Network.AWS.SSM.Types.AttachmentName
import Network.AWS.SSM.Types.AttachmentUrl
import Network.AWS.SSM.Types.AttachmentsSource
import Network.AWS.SSM.Types.AttachmentsSourceKey
import Network.AWS.SSM.Types.AttachmentsSourceValue
import Network.AWS.SSM.Types.AttributeName
import Network.AWS.SSM.Types.AttributeValue
import Network.AWS.SSM.Types.AutomationExecution
import Network.AWS.SSM.Types.AutomationExecutionFilter
import Network.AWS.SSM.Types.AutomationExecutionFilterKey
import Network.AWS.SSM.Types.AutomationExecutionFilterValue
import Network.AWS.SSM.Types.AutomationExecutionId
import Network.AWS.SSM.Types.AutomationExecutionMetadata
import Network.AWS.SSM.Types.AutomationExecutionStatus
import Network.AWS.SSM.Types.AutomationParameterKey
import Network.AWS.SSM.Types.AutomationParameterValue
import Network.AWS.SSM.Types.AutomationTargetParameterName
import Network.AWS.SSM.Types.AutomationType
import Network.AWS.SSM.Types.BaselineDescription
import Network.AWS.SSM.Types.BaselineId
import Network.AWS.SSM.Types.BaselineName
import Network.AWS.SSM.Types.BatchErrorMessage
import Network.AWS.SSM.Types.BucketName
import Network.AWS.SSM.Types.CalendarNameOrARN
import Network.AWS.SSM.Types.CalendarState
import Network.AWS.SSM.Types.CaptureTime
import Network.AWS.SSM.Types.Category
import Network.AWS.SSM.Types.ClientContext
import Network.AWS.SSM.Types.ClientToken
import Network.AWS.SSM.Types.CloudWatchLogGroupName
import Network.AWS.SSM.Types.CloudWatchOutputConfig
import Network.AWS.SSM.Types.Command
import Network.AWS.SSM.Types.CommandFilter
import Network.AWS.SSM.Types.CommandFilterKey
import Network.AWS.SSM.Types.CommandFilterValue
import Network.AWS.SSM.Types.CommandId
import Network.AWS.SSM.Types.CommandInvocation
import Network.AWS.SSM.Types.CommandInvocationStatus
import Network.AWS.SSM.Types.CommandPlugin
import Network.AWS.SSM.Types.CommandPluginStatus
import Network.AWS.SSM.Types.CommandStatus
import Network.AWS.SSM.Types.Comment
import Network.AWS.SSM.Types.ComplianceExecutionId
import Network.AWS.SSM.Types.ComplianceExecutionSummary
import Network.AWS.SSM.Types.ComplianceExecutionType
import Network.AWS.SSM.Types.ComplianceFilterValue
import Network.AWS.SSM.Types.ComplianceItem
import Network.AWS.SSM.Types.ComplianceItemContentHash
import Network.AWS.SSM.Types.ComplianceItemEntry
import Network.AWS.SSM.Types.ComplianceItemId
import Network.AWS.SSM.Types.ComplianceItemTitle
import Network.AWS.SSM.Types.ComplianceQueryOperatorType
import Network.AWS.SSM.Types.ComplianceResourceId
import Network.AWS.SSM.Types.ComplianceResourceType
import Network.AWS.SSM.Types.ComplianceSeverity
import Network.AWS.SSM.Types.ComplianceStatus
import Network.AWS.SSM.Types.ComplianceStringFilter
import Network.AWS.SSM.Types.ComplianceStringFilterKey
import Network.AWS.SSM.Types.ComplianceSummaryItem
import Network.AWS.SSM.Types.ComplianceType
import Network.AWS.SSM.Types.ComplianceTypeName
import Network.AWS.SSM.Types.ComplianceUploadType
import Network.AWS.SSM.Types.CompliantSummary
import Network.AWS.SSM.Types.ComputerName
import Network.AWS.SSM.Types.Configuration
import Network.AWS.SSM.Types.ConnectionStatus
import Network.AWS.SSM.Types.Content
import Network.AWS.SSM.Types.ContentHash
import Network.AWS.SSM.Types.CreateAssociationBatchRequestEntry
import Network.AWS.SSM.Types.CurrentAction
import Network.AWS.SSM.Types.CurrentStepName
import Network.AWS.SSM.Types.DataType
import Network.AWS.SSM.Types.DefaultInstanceName
import Network.AWS.SSM.Types.DefaultValue
import Network.AWS.SSM.Types.DefaultVersion
import Network.AWS.SSM.Types.DeletionId
import Network.AWS.SSM.Types.DescribeActivationsFilter
import Network.AWS.SSM.Types.DescribeActivationsFilterKeys
import Network.AWS.SSM.Types.Description
import Network.AWS.SSM.Types.DetailedStatus
import Network.AWS.SSM.Types.DocumentARN
import Network.AWS.SSM.Types.DocumentDefaultVersionDescription
import Network.AWS.SSM.Types.DocumentDescription
import Network.AWS.SSM.Types.DocumentFilter
import Network.AWS.SSM.Types.DocumentFilterKey
import Network.AWS.SSM.Types.DocumentFormat
import Network.AWS.SSM.Types.DocumentHash
import Network.AWS.SSM.Types.DocumentHashType
import Network.AWS.SSM.Types.DocumentIdentifier
import Network.AWS.SSM.Types.DocumentKeyValuesFilter
import Network.AWS.SSM.Types.DocumentKeyValuesFilterValue
import Network.AWS.SSM.Types.DocumentName
import Network.AWS.SSM.Types.DocumentParameter
import Network.AWS.SSM.Types.DocumentParameterType
import Network.AWS.SSM.Types.DocumentPermissionType
import Network.AWS.SSM.Types.DocumentRequires
import Network.AWS.SSM.Types.DocumentSchemaVersion
import Network.AWS.SSM.Types.DocumentStatus
import Network.AWS.SSM.Types.DocumentStatusInformation
import Network.AWS.SSM.Types.DocumentType
import Network.AWS.SSM.Types.DocumentVersion
import Network.AWS.SSM.Types.DocumentVersionInfo
import Network.AWS.SSM.Types.DocumentVersionName
import Network.AWS.SSM.Types.DocumentVersionNumber
import Network.AWS.SSM.Types.EffectivePatch
import Network.AWS.SSM.Types.EndDate
import Network.AWS.SSM.Types.ExecutedBy
import Network.AWS.SSM.Types.ExecutionElapsedTime
import Network.AWS.SSM.Types.ExecutionEndDateTime
import Network.AWS.SSM.Types.ExecutionId
import Network.AWS.SSM.Types.ExecutionMode
import Network.AWS.SSM.Types.ExecutionRoleName
import Network.AWS.SSM.Types.ExecutionStartDateTime
import Network.AWS.SSM.Types.ExecutionSummary
import Network.AWS.SSM.Types.ExecutionTime
import Network.AWS.SSM.Types.FailedCreateAssociation
import Network.AWS.SSM.Types.FailureDetails
import Network.AWS.SSM.Types.FailureMessage
import Network.AWS.SSM.Types.FailureStage
import Network.AWS.SSM.Types.FailureType
import Network.AWS.SSM.Types.Fault
import Network.AWS.SSM.Types.Hash
import Network.AWS.SSM.Types.IPAddress
import Network.AWS.SSM.Types.IamRole
import Network.AWS.SSM.Types.Id
import Network.AWS.SSM.Types.IdempotencyToken
import Network.AWS.SSM.Types.InstallOverrideList
import Network.AWS.SSM.Types.InstanceAggregatedAssociationOverview
import Network.AWS.SSM.Types.InstanceAssociation
import Network.AWS.SSM.Types.InstanceAssociationOutputLocation
import Network.AWS.SSM.Types.InstanceAssociationOutputUrl
import Network.AWS.SSM.Types.InstanceAssociationStatusInfo
import Network.AWS.SSM.Types.InstanceId
import Network.AWS.SSM.Types.InstanceInformation
import Network.AWS.SSM.Types.InstanceInformationFilter
import Network.AWS.SSM.Types.InstanceInformationFilterKey
import Network.AWS.SSM.Types.InstanceInformationFilterValue
import Network.AWS.SSM.Types.InstanceInformationStringFilter
import Network.AWS.SSM.Types.InstancePatchState
import Network.AWS.SSM.Types.InstancePatchStateFilter
import Network.AWS.SSM.Types.InstancePatchStateFilterKey
import Network.AWS.SSM.Types.InstancePatchStateFilterValue
import Network.AWS.SSM.Types.InstancePatchStateOperatorType
import Network.AWS.SSM.Types.InstanceTagName
import Network.AWS.SSM.Types.InventoryAggregator
import Network.AWS.SSM.Types.InventoryAggregatorExpression
import Network.AWS.SSM.Types.InventoryAttributeDataType
import Network.AWS.SSM.Types.InventoryDeletionStatus
import Network.AWS.SSM.Types.InventoryDeletionStatusItem
import Network.AWS.SSM.Types.InventoryDeletionSummary
import Network.AWS.SSM.Types.InventoryDeletionSummaryItem
import Network.AWS.SSM.Types.InventoryFilter
import Network.AWS.SSM.Types.InventoryFilterValue
import Network.AWS.SSM.Types.InventoryGroup
import Network.AWS.SSM.Types.InventoryGroupName
import Network.AWS.SSM.Types.InventoryItem
import Network.AWS.SSM.Types.InventoryItemAttribute
import Network.AWS.SSM.Types.InventoryItemAttributeName
import Network.AWS.SSM.Types.InventoryItemCaptureTime
import Network.AWS.SSM.Types.InventoryItemContentHash
import Network.AWS.SSM.Types.InventoryItemSchema
import Network.AWS.SSM.Types.InventoryItemSchemaVersion
import Network.AWS.SSM.Types.InventoryItemTypeName
import Network.AWS.SSM.Types.InventoryQueryOperatorType
import Network.AWS.SSM.Types.InventoryResultEntity
import Network.AWS.SSM.Types.InventoryResultItem
import Network.AWS.SSM.Types.InventoryResultItemKey
import Network.AWS.SSM.Types.InventorySchemaDeleteOption
import Network.AWS.SSM.Types.InventoryTypeDisplayName
import Network.AWS.SSM.Types.InvocationId
import Network.AWS.SSM.Types.InvocationTraceOutput
import Network.AWS.SSM.Types.Key
import Network.AWS.SSM.Types.KeyId
import Network.AWS.SSM.Types.LastModifiedUser
import Network.AWS.SSM.Types.LastResourceDataSyncMessage
import Network.AWS.SSM.Types.LastResourceDataSyncStatus
import Network.AWS.SSM.Types.LastStatusMessage
import Network.AWS.SSM.Types.LatestVersion
import Network.AWS.SSM.Types.LoggingInfo
import Network.AWS.SSM.Types.MaintenanceWindowAutomationParameters
import Network.AWS.SSM.Types.MaintenanceWindowDescription
import Network.AWS.SSM.Types.MaintenanceWindowExecution
import Network.AWS.SSM.Types.MaintenanceWindowExecutionId
import Network.AWS.SSM.Types.MaintenanceWindowExecutionStatus
import Network.AWS.SSM.Types.MaintenanceWindowExecutionStatusDetails
import Network.AWS.SSM.Types.MaintenanceWindowExecutionTaskExecutionId
import Network.AWS.SSM.Types.MaintenanceWindowExecutionTaskId
import Network.AWS.SSM.Types.MaintenanceWindowExecutionTaskIdentity
import Network.AWS.SSM.Types.MaintenanceWindowExecutionTaskInvocationId
import Network.AWS.SSM.Types.MaintenanceWindowExecutionTaskInvocationIdentity
import Network.AWS.SSM.Types.MaintenanceWindowExecutionTaskInvocationParameters
import Network.AWS.SSM.Types.MaintenanceWindowFilter
import Network.AWS.SSM.Types.MaintenanceWindowFilterKey
import Network.AWS.SSM.Types.MaintenanceWindowFilterValue
import Network.AWS.SSM.Types.MaintenanceWindowId
import Network.AWS.SSM.Types.MaintenanceWindowIdentity
import Network.AWS.SSM.Types.MaintenanceWindowIdentityForTarget
import Network.AWS.SSM.Types.MaintenanceWindowLambdaParameters
import Network.AWS.SSM.Types.MaintenanceWindowName
import Network.AWS.SSM.Types.MaintenanceWindowResourceType
import Network.AWS.SSM.Types.MaintenanceWindowRunCommandParameters
import Network.AWS.SSM.Types.MaintenanceWindowSchedule
import Network.AWS.SSM.Types.MaintenanceWindowStepFunctionsInput
import Network.AWS.SSM.Types.MaintenanceWindowStepFunctionsName
import Network.AWS.SSM.Types.MaintenanceWindowStepFunctionsParameters
import Network.AWS.SSM.Types.MaintenanceWindowStringDateTime
import Network.AWS.SSM.Types.MaintenanceWindowTarget
import Network.AWS.SSM.Types.MaintenanceWindowTargetId
import Network.AWS.SSM.Types.MaintenanceWindowTask
import Network.AWS.SSM.Types.MaintenanceWindowTaskId
import Network.AWS.SSM.Types.MaintenanceWindowTaskInvocationParameters
import Network.AWS.SSM.Types.MaintenanceWindowTaskParameterName
import Network.AWS.SSM.Types.MaintenanceWindowTaskParameterValue
import Network.AWS.SSM.Types.MaintenanceWindowTaskParameterValueExpression
import Network.AWS.SSM.Types.MaintenanceWindowTaskTargetId
import Network.AWS.SSM.Types.MaintenanceWindowTaskType
import Network.AWS.SSM.Types.MaintenanceWindowTimezone
import Network.AWS.SSM.Types.MaxConcurrency
import Network.AWS.SSM.Types.MaxErrors
import Network.AWS.SSM.Types.Message
import Network.AWS.SSM.Types.MsrcSeverity
import Network.AWS.SSM.Types.Name
import Network.AWS.SSM.Types.NextExecutionTime
import Network.AWS.SSM.Types.NextToken
import Network.AWS.SSM.Types.NextTransitionTime
import Network.AWS.SSM.Types.NonCompliantSummary
import Network.AWS.SSM.Types.NotificationArn
import Network.AWS.SSM.Types.NotificationConfig
import Network.AWS.SSM.Types.NotificationEvent
import Network.AWS.SSM.Types.NotificationType
import Network.AWS.SSM.Types.OperatingSystem
import Network.AWS.SSM.Types.OpsAggregator
import Network.AWS.SSM.Types.OpsAggregatorValue
import Network.AWS.SSM.Types.OpsAggregatorValueKey
import Network.AWS.SSM.Types.OpsDataAttributeName
import Network.AWS.SSM.Types.OpsDataTypeName
import Network.AWS.SSM.Types.OpsEntity
import Network.AWS.SSM.Types.OpsEntityId
import Network.AWS.SSM.Types.OpsEntityItem
import Network.AWS.SSM.Types.OpsEntityItemKey
import Network.AWS.SSM.Types.OpsFilter
import Network.AWS.SSM.Types.OpsFilterKey
import Network.AWS.SSM.Types.OpsFilterOperatorType
import Network.AWS.SSM.Types.OpsFilterValue
import Network.AWS.SSM.Types.OpsItem
import Network.AWS.SSM.Types.OpsItemCategory
import Network.AWS.SSM.Types.OpsItemDataKey
import Network.AWS.SSM.Types.OpsItemDataType
import Network.AWS.SSM.Types.OpsItemDataValue
import Network.AWS.SSM.Types.OpsItemDataValueString
import Network.AWS.SSM.Types.OpsItemDescription
import Network.AWS.SSM.Types.OpsItemFilter
import Network.AWS.SSM.Types.OpsItemFilterKey
import Network.AWS.SSM.Types.OpsItemFilterOperator
import Network.AWS.SSM.Types.OpsItemFilterValue
import Network.AWS.SSM.Types.OpsItemId
import Network.AWS.SSM.Types.OpsItemNotification
import Network.AWS.SSM.Types.OpsItemSeverity
import Network.AWS.SSM.Types.OpsItemSource
import Network.AWS.SSM.Types.OpsItemStatus
import Network.AWS.SSM.Types.OpsItemSummary
import Network.AWS.SSM.Types.OpsResultAttribute
import Network.AWS.SSM.Types.Option
import Network.AWS.SSM.Types.OrganizationSourceType
import Network.AWS.SSM.Types.OrganizationalUnitId
import Network.AWS.SSM.Types.Output
import Network.AWS.SSM.Types.OutputS3BucketName
import Network.AWS.SSM.Types.OutputS3KeyPrefix
import Network.AWS.SSM.Types.OutputS3Region
import Network.AWS.SSM.Types.OutputSource
import Network.AWS.SSM.Types.OutputSourceId
import Network.AWS.SSM.Types.OutputSourceType
import Network.AWS.SSM.Types.Owner
import Network.AWS.SSM.Types.OwnerInformation
import Network.AWS.SSM.Types.PSParameterName
import Network.AWS.SSM.Types.PSParameterSelector
import Network.AWS.SSM.Types.PSParameterValue
import Network.AWS.SSM.Types.Parameter
import Network.AWS.SSM.Types.ParameterDataType
import Network.AWS.SSM.Types.ParameterDescription
import Network.AWS.SSM.Types.ParameterHistory
import Network.AWS.SSM.Types.ParameterInlinePolicy
import Network.AWS.SSM.Types.ParameterKeyId
import Network.AWS.SSM.Types.ParameterLabel
import Network.AWS.SSM.Types.ParameterMetadata
import Network.AWS.SSM.Types.ParameterName
import Network.AWS.SSM.Types.ParameterStringFilter
import Network.AWS.SSM.Types.ParameterStringFilterValue
import Network.AWS.SSM.Types.ParameterTier
import Network.AWS.SSM.Types.ParameterType
import Network.AWS.SSM.Types.ParameterValue
import Network.AWS.SSM.Types.ParametersFilter
import Network.AWS.SSM.Types.ParametersFilterKey
import Network.AWS.SSM.Types.ParametersFilterValue
import Network.AWS.SSM.Types.ParentAutomationExecutionId
import Network.AWS.SSM.Types.Patch
import Network.AWS.SSM.Types.PatchAction
import Network.AWS.SSM.Types.PatchAdvisoryId
import Network.AWS.SSM.Types.PatchBaselineIdentity
import Network.AWS.SSM.Types.PatchBugzillaId
import Network.AWS.SSM.Types.PatchCVEId
import Network.AWS.SSM.Types.PatchCVEIds
import Network.AWS.SSM.Types.PatchClassification
import Network.AWS.SSM.Types.PatchComplianceData
import Network.AWS.SSM.Types.PatchComplianceDataState
import Network.AWS.SSM.Types.PatchComplianceLevel
import Network.AWS.SSM.Types.PatchContentUrl
import Network.AWS.SSM.Types.PatchDeploymentStatus
import Network.AWS.SSM.Types.PatchFilter
import Network.AWS.SSM.Types.PatchFilterGroup
import Network.AWS.SSM.Types.PatchFilterKey
import Network.AWS.SSM.Types.PatchFilterValue
import Network.AWS.SSM.Types.PatchGroup
import Network.AWS.SSM.Types.PatchGroupPatchBaselineMapping
import Network.AWS.SSM.Types.PatchId
import Network.AWS.SSM.Types.PatchKbNumber
import Network.AWS.SSM.Types.PatchLanguage
import Network.AWS.SSM.Types.PatchMsrcNumber
import Network.AWS.SSM.Types.PatchOperationType
import Network.AWS.SSM.Types.PatchOrchestratorFilter
import Network.AWS.SSM.Types.PatchOrchestratorFilterValue
import Network.AWS.SSM.Types.PatchProduct
import Network.AWS.SSM.Types.PatchProperty
import Network.AWS.SSM.Types.PatchRule
import Network.AWS.SSM.Types.PatchRuleGroup
import Network.AWS.SSM.Types.PatchSet
import Network.AWS.SSM.Types.PatchSeverity
import Network.AWS.SSM.Types.PatchSource
import Network.AWS.SSM.Types.PatchSourceProduct
import Network.AWS.SSM.Types.PatchStatus
import Network.AWS.SSM.Types.PatchStringDateTime
import Network.AWS.SSM.Types.PatchVersion
import Network.AWS.SSM.Types.PingStatus
import Network.AWS.SSM.Types.PlatformType
import Network.AWS.SSM.Types.PluginName
import Network.AWS.SSM.Types.Policies
import Network.AWS.SSM.Types.Prefix
import Network.AWS.SSM.Types.Product
import Network.AWS.SSM.Types.ProductFamily
import Network.AWS.SSM.Types.ProgressCounters
import Network.AWS.SSM.Types.Qualifier
import Network.AWS.SSM.Types.RebootOption
import Network.AWS.SSM.Types.Region
import Network.AWS.SSM.Types.RelatedOpsItem
import Network.AWS.SSM.Types.Release
import Network.AWS.SSM.Types.Repository
import Network.AWS.SSM.Types.ResolvedTargets
import Network.AWS.SSM.Types.ResourceComplianceSummaryItem
import Network.AWS.SSM.Types.ResourceCountByStatus
import Network.AWS.SSM.Types.ResourceDataSyncAwsOrganizationsSource
import Network.AWS.SSM.Types.ResourceDataSyncDestinationDataSharing
import Network.AWS.SSM.Types.ResourceDataSyncDestinationDataSharingType
import Network.AWS.SSM.Types.ResourceDataSyncItem
import Network.AWS.SSM.Types.ResourceDataSyncName
import Network.AWS.SSM.Types.ResourceDataSyncOrganizationalUnit
import Network.AWS.SSM.Types.ResourceDataSyncS3Destination
import Network.AWS.SSM.Types.ResourceDataSyncS3Format
import Network.AWS.SSM.Types.ResourceDataSyncS3Region
import Network.AWS.SSM.Types.ResourceDataSyncSource
import Network.AWS.SSM.Types.ResourceDataSyncSourceRegion
import Network.AWS.SSM.Types.ResourceDataSyncSourceType
import Network.AWS.SSM.Types.ResourceDataSyncSourceWithState
import Network.AWS.SSM.Types.ResourceDataSyncState
import Network.AWS.SSM.Types.ResourceDataSyncType
import Network.AWS.SSM.Types.ResourceId
import Network.AWS.SSM.Types.ResourceType
import Network.AWS.SSM.Types.ResourceTypeForTagging
import Network.AWS.SSM.Types.ResultAttribute
import Network.AWS.SSM.Types.S3BucketName
import Network.AWS.SSM.Types.S3KeyPrefix
import Network.AWS.SSM.Types.S3OutputLocation
import Network.AWS.SSM.Types.S3OutputUrl
import Network.AWS.SSM.Types.S3Region
import Network.AWS.SSM.Types.ScheduleExpression
import Network.AWS.SSM.Types.ScheduleTimezone
import Network.AWS.SSM.Types.ScheduledWindowExecution
import Network.AWS.SSM.Types.SchemaVersion
import Network.AWS.SSM.Types.ServiceRole
import Network.AWS.SSM.Types.ServiceRoleArn
import Network.AWS.SSM.Types.ServiceSetting
import Network.AWS.SSM.Types.ServiceSettingId
import Network.AWS.SSM.Types.Session
import Network.AWS.SSM.Types.SessionDetails
import Network.AWS.SSM.Types.SessionFilter
import Network.AWS.SSM.Types.SessionFilterKey
import Network.AWS.SSM.Types.SessionId
import Network.AWS.SSM.Types.SessionManagerCloudWatchOutputUrl
import Network.AWS.SSM.Types.SessionManagerOutputUrl
import Network.AWS.SSM.Types.SessionManagerParameterName
import Network.AWS.SSM.Types.SessionManagerParameterValue
import Network.AWS.SSM.Types.SessionManagerS3OutputUrl
import Network.AWS.SSM.Types.SessionOwner
import Network.AWS.SSM.Types.SessionState
import Network.AWS.SSM.Types.SessionStatus
import Network.AWS.SSM.Types.SessionTarget
import Network.AWS.SSM.Types.SettingId
import Network.AWS.SSM.Types.SettingValue
import Network.AWS.SSM.Types.Severity
import Network.AWS.SSM.Types.SeveritySummary
import Network.AWS.SSM.Types.Sha1
import Network.AWS.SSM.Types.SharedDocumentVersion
import Network.AWS.SSM.Types.SignalType
import Network.AWS.SSM.Types.SnapshotDownloadUrl
import Network.AWS.SSM.Types.SnapshotId
import Network.AWS.SSM.Types.Source
import Network.AWS.SSM.Types.StandardErrorContent
import Network.AWS.SSM.Types.StandardErrorUrl
import Network.AWS.SSM.Types.StandardOutputContent
import Network.AWS.SSM.Types.StandardOutputUrl
import Network.AWS.SSM.Types.StartDate
import Network.AWS.SSM.Types.Status
import Network.AWS.SSM.Types.StatusAdditionalInfo
import Network.AWS.SSM.Types.StatusDetails
import Network.AWS.SSM.Types.StatusInformation
import Network.AWS.SSM.Types.StatusMessage
import Network.AWS.SSM.Types.StatusName
import Network.AWS.SSM.Types.StepExecution
import Network.AWS.SSM.Types.StepExecutionFilter
import Network.AWS.SSM.Types.StepExecutionFilterKey
import Network.AWS.SSM.Types.StepExecutionFilterValue
import Network.AWS.SSM.Types.StopType
import Network.AWS.SSM.Types.StreamUrl
import Network.AWS.SSM.Types.String
import Network.AWS.SSM.Types.SyncName
import Network.AWS.SSM.Types.SyncType
import Network.AWS.SSM.Types.Tag
import Network.AWS.SSM.Types.TagKey
import Network.AWS.SSM.Types.Target
import Network.AWS.SSM.Types.TargetKey
import Network.AWS.SSM.Types.TargetLocation
import Network.AWS.SSM.Types.TargetLocationMaxConcurrency
import Network.AWS.SSM.Types.TargetLocationMaxErrors
import Network.AWS.SSM.Types.TargetMapKey
import Network.AWS.SSM.Types.TargetMapValue
import Network.AWS.SSM.Types.TargetParameterName
import Network.AWS.SSM.Types.TargetType
import Network.AWS.SSM.Types.TargetValue
import Network.AWS.SSM.Types.TaskArn
import Network.AWS.SSM.Types.TaskExecutionId
import Network.AWS.SSM.Types.Title
import Network.AWS.SSM.Types.TokenValue
import Network.AWS.SSM.Types.TypeName
import Network.AWS.SSM.Types.UUID
import Network.AWS.SSM.Types.Url
import Network.AWS.SSM.Types.ValidNextStep
import Network.AWS.SSM.Types.Value
import Network.AWS.SSM.Types.Vendor
import Network.AWS.SSM.Types.Version
import Network.AWS.SSM.Types.VersionName
import Network.AWS.SSM.Types.WindowExecutionId
import Network.AWS.SSM.Types.WindowId
import Network.AWS.SSM.Types.WindowTargetId
import Network.AWS.SSM.Types.WindowTaskId
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-11-06@ of the Amazon Simple Systems Manager (SSM) SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "SSM",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "ssm",
      Core._svcVersion = "2014-11-06",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "SSM",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | An Automation document with the specified name and version could not be found.
_AutomationDefinitionVersionNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AutomationDefinitionVersionNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "AutomationDefinitionVersionNotFoundException"
{-# DEPRECATED _AutomationDefinitionVersionNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The document version is not valid or does not exist.
_InvalidDocumentVersion :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDocumentVersion =
  Core._MatchServiceError mkServiceConfig "InvalidDocumentVersion"
{-# DEPRECATED _InvalidDocumentVersion "Use generic-lens or generic-optics instead." #-}

-- | Parameter Store does not support changing a parameter type in a hierarchy. For example, you can't change a parameter from a @String@ type to a @SecureString@ type. You must create a new, unique parameter.
_HierarchyTypeMismatchException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_HierarchyTypeMismatchException =
  Core._MatchServiceError
    mkServiceConfig
    "HierarchyTypeMismatchException"
{-# DEPRECATED _HierarchyTypeMismatchException "Use generic-lens or generic-optics instead." #-}

-- | The schedule is invalid. Verify your cron or rate expression and try again.
_InvalidSchedule :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSchedule =
  Core._MatchServiceError mkServiceConfig "InvalidSchedule"
{-# DEPRECATED _InvalidSchedule "Use generic-lens or generic-optics instead." #-}

-- | The parameter type is not supported.
_UnsupportedParameterType :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedParameterType =
  Core._MatchServiceError
    mkServiceConfig
    "UnsupportedParameterType"
{-# DEPRECATED _UnsupportedParameterType "Use generic-lens or generic-optics instead." #-}

-- | The specified update status operation is not valid.
_InvalidAutomationStatusUpdateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidAutomationStatusUpdateException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidAutomationStatusUpdateException"
{-# DEPRECATED _InvalidAutomationStatusUpdateException "Use generic-lens or generic-optics instead." #-}

-- | The plugin name is not valid.
_InvalidPluginName :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidPluginName =
  Core._MatchServiceError mkServiceConfig "InvalidPluginName"
{-# DEPRECATED _InvalidPluginName "Use generic-lens or generic-optics instead." #-}

-- | Microsoft application patching is only available on EC2 instances and advanced instances. To patch Microsoft applications on on-premises servers and VMs, you must enable advanced instances. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-managedinstances-advanced.html Using the advanced-instances tier> in the /AWS Systems Manager User Guide/ .
_UnsupportedFeatureRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedFeatureRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "UnsupportedFeatureRequiredException"
{-# DEPRECATED _UnsupportedFeatureRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The specified aggregator is not valid for inventory groups. Verify that the aggregator uses a valid inventory type such as @AWS:Application@ or @AWS:InstanceInformation@ .
_InvalidAggregatorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidAggregatorException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidAggregatorException"
{-# DEPRECATED _InvalidAggregatorException "Use generic-lens or generic-optics instead." #-}

-- | You attempted to register a LAMBDA or STEP_FUNCTIONS task in a region where the corresponding service is not available.
_FeatureNotAvailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FeatureNotAvailableException =
  Core._MatchServiceError
    mkServiceConfig
    "FeatureNotAvailableException"
{-# DEPRECATED _FeatureNotAvailableException "Use generic-lens or generic-optics instead." #-}

-- | The signal is not valid for the current Automation execution.
_InvalidAutomationSignalException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidAutomationSignalException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidAutomationSignalException"
{-# DEPRECATED _InvalidAutomationSignalException "Use generic-lens or generic-optics instead." #-}

-- | You have exceeded the allowed maximum sync configurations.
_ResourceDataSyncCountExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceDataSyncCountExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceDataSyncCountExceededException"
{-# DEPRECATED _ResourceDataSyncCountExceededException "Use generic-lens or generic-optics instead." #-}

-- | The document does not support the platform type of the given instance ID(s). For example, you sent an document for a Windows instance to a Linux instance.
_UnsupportedPlatformType :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedPlatformType =
  Core._MatchServiceError mkServiceConfig "UnsupportedPlatformType"
{-# DEPRECATED _UnsupportedPlatformType "Use generic-lens or generic-optics instead." #-}

-- | The filter value is not valid. Verify the value and try again.
_InvalidFilterValue :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidFilterValue =
  Core._MatchServiceError mkServiceConfig "InvalidFilterValue"
{-# DEPRECATED _InvalidFilterValue "Use generic-lens or generic-optics instead." #-}

-- | One or more content items is not valid.
_InvalidItemContentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidItemContentException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidItemContentException"
{-# DEPRECATED _InvalidItemContentException "Use generic-lens or generic-optics instead." #-}

-- | The specified filter option is not valid. Valid options are Equals and BeginsWith. For Path filter, valid options are Recursive and OneLevel.
_InvalidFilterOption :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidFilterOption =
  Core._MatchServiceError mkServiceConfig "InvalidFilterOption"
{-# DEPRECATED _InvalidFilterOption "Use generic-lens or generic-optics instead." #-}

-- | The parameter name is not valid.
_ParameterPatternMismatchException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ParameterPatternMismatchException =
  Core._MatchServiceError
    mkServiceConfig
    "ParameterPatternMismatchException"
{-# DEPRECATED _ParameterPatternMismatchException "Use generic-lens or generic-optics instead." #-}

-- | The permission type is not supported. /Share/ is the only supported permission type.
_InvalidPermissionType :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidPermissionType =
  Core._MatchServiceError mkServiceConfig "InvalidPermissionType"
{-# DEPRECATED _InvalidPermissionType "Use generic-lens or generic-optics instead." #-}

-- | You must disassociate a document from all instances before you can delete it.
_AssociatedInstances :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AssociatedInstances =
  Core._MatchServiceError mkServiceConfig "AssociatedInstances"
{-# DEPRECATED _AssociatedInstances "Use generic-lens or generic-optics instead." #-}

-- | The operating systems you specified is not supported, or the operation is not supported for the operating system.
_UnsupportedOperatingSystem :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperatingSystem =
  Core._MatchServiceError
    mkServiceConfig
    "UnsupportedOperatingSystem"
{-# DEPRECATED _UnsupportedOperatingSystem "Use generic-lens or generic-optics instead." #-}

-- | The following problems can cause this exception:
--
-- You do not have permission to access the instance.
-- SSM Agent is not running. Verify that SSM Agent is running.
-- SSM Agent is not registered with the SSM endpoint. Try reinstalling SSM Agent.
-- The instance is not in valid state. Valid states are: Running, Pending, Stopped, Stopping. Invalid states are: Shutting-down and Terminated.
_InvalidInstanceId :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidInstanceId =
  Core._MatchServiceError mkServiceConfig "InvalidInstanceId"
{-# DEPRECATED _InvalidInstanceId "Use generic-lens or generic-optics instead." #-}

-- | The updated status is the same as the current status.
_StatusUnchanged :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_StatusUnchanged =
  Core._MatchServiceError mkServiceConfig "StatusUnchanged"
{-# DEPRECATED _StatusUnchanged "Use generic-lens or generic-optics instead." #-}

-- | The specified token is not valid.
_InvalidNextToken :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNextToken =
  Core._MatchServiceError mkServiceConfig "InvalidNextToken"
{-# DEPRECATED _InvalidNextToken "Use generic-lens or generic-optics instead." #-}

-- | The request is not valid.
_InvalidInventoryRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidInventoryRequestException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidInventoryRequestException"
{-# DEPRECATED _InvalidInventoryRequestException "Use generic-lens or generic-optics instead." #-}

-- | The association is not valid or does not exist.
_InvalidAssociation :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidAssociation =
  Core._MatchServiceError mkServiceConfig "InvalidAssociation"
{-# DEPRECATED _InvalidAssociation "Use generic-lens or generic-optics instead." #-}

-- | The OpsItem already exists.
_OpsItemAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OpsItemAlreadyExistsException =
  Core._MatchServiceError
    mkServiceConfig
    "OpsItemAlreadyExistsException"
{-# DEPRECATED _OpsItemAlreadyExistsException "Use generic-lens or generic-optics instead." #-}

-- | The S3 bucket does not exist.
_InvalidOutputFolder :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidOutputFolder =
  Core._MatchServiceError mkServiceConfig "InvalidOutputFolder"
{-# DEPRECATED _InvalidOutputFolder "Use generic-lens or generic-optics instead." #-}

-- | The request caused OpsItems to exceed one or more quotas. For information about OpsItem quotas, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-learn-more.html#OpsCenter-learn-more-limits What are the resource limits for OpsCenter?> .
_OpsItemLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OpsItemLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "OpsItemLimitExceededException"
{-# DEPRECATED _OpsItemLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | The activation ID is not valid. Verify the you entered the correct ActivationId or ActivationCode and try again.
_InvalidActivationId :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidActivationId =
  Core._MatchServiceError mkServiceConfig "InvalidActivationId"
{-# DEPRECATED _InvalidActivationId "Use generic-lens or generic-optics instead." #-}

-- | The specified service setting was not found. Either the service name or the setting has not been provisioned by the AWS service team.
_ServiceSettingNotFound :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceSettingNotFound =
  Core._MatchServiceError mkServiceConfig "ServiceSettingNotFound"
{-# DEPRECATED _ServiceSettingNotFound "Use generic-lens or generic-optics instead." #-}

-- | The specified inventory item result attribute is not valid.
_InvalidResultAttributeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidResultAttributeException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidResultAttributeException"
{-# DEPRECATED _InvalidResultAttributeException "Use generic-lens or generic-optics instead." #-}

-- | The specified target instance for the session is not fully configured for use with Session Manager. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/session-manager-getting-started.html Getting started with Session Manager> in the /AWS Systems Manager User Guide/ . This error is also returned if you attempt to start a session on an instance that is located in a different account or Region
_TargetNotConnected :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TargetNotConnected =
  Core._MatchServiceError mkServiceConfig "TargetNotConnected"
{-# DEPRECATED _TargetNotConnected "Use generic-lens or generic-optics instead." #-}

-- | Error returned when the caller has exceeded the default resource quotas. For example, too many maintenance windows or patch baselines have been created.
--
-- For information about resource quotas in Systems Manager, see <http://docs.aws.amazon.com/general/latest/gr/ssm.html#limits_ssm Systems Manager service quotas> in the /AWS General Reference/ .
_ResourceLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceLimitExceededException"
{-# DEPRECATED _ResourceLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | A parameter version can have a maximum of ten labels.
_ParameterVersionLabelLimitExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ParameterVersionLabelLimitExceeded =
  Core._MatchServiceError
    mkServiceConfig
    "ParameterVersionLabelLimitExceeded"
{-# DEPRECATED _ParameterVersionLabelLimitExceeded "Use generic-lens or generic-optics instead." #-}

-- | The specified sync configuration is invalid.
_ResourceDataSyncInvalidConfigurationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceDataSyncInvalidConfigurationException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceDataSyncInvalidConfigurationException"
{-# DEPRECATED _ResourceDataSyncInvalidConfigurationException "Use generic-lens or generic-optics instead." #-}

-- | Prism for 'InvalidCommandId' errors.
_InvalidCommandId :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidCommandId =
  Core._MatchServiceError mkServiceConfig "InvalidCommandId"
{-# DEPRECATED _InvalidCommandId "Use generic-lens or generic-optics instead." #-}

-- | You cannot specify an instance ID in more than one association.
_DuplicateInstanceId :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateInstanceId =
  Core._MatchServiceError mkServiceConfig "DuplicateInstanceId"
{-# DEPRECATED _DuplicateInstanceId "Use generic-lens or generic-optics instead." #-}

-- | The resource type is not valid. For example, if you are attempting to tag an instance, the instance must be a registered, managed instance.
_InvalidResourceType :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidResourceType =
  Core._MatchServiceError mkServiceConfig "InvalidResourceType"
{-# DEPRECATED _InvalidResourceType "Use generic-lens or generic-optics instead." #-}

-- | Inventory item type schema version has to match supported versions in the service. Check output of GetInventorySchema to see the available schema version for each type.
_UnsupportedInventorySchemaVersionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedInventorySchemaVersionException =
  Core._MatchServiceError
    mkServiceConfig
    "UnsupportedInventorySchemaVersionException"
{-# DEPRECATED _UnsupportedInventorySchemaVersionException "Use generic-lens or generic-optics instead." #-}

-- | The specified document does not exist.
_InvalidDocument :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDocument =
  Core._MatchServiceError mkServiceConfig "InvalidDocument"
{-# DEPRECATED _InvalidDocument "Use generic-lens or generic-optics instead." #-}

-- | There is a conflict in the policies specified for this parameter. You can't, for example, specify two Expiration policies for a parameter. Review your policies, and try again.
_IncompatiblePolicyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IncompatiblePolicyException =
  Core._MatchServiceError
    mkServiceConfig
    "IncompatiblePolicyException"
{-# DEPRECATED _IncompatiblePolicyException "Use generic-lens or generic-optics instead." #-}

-- | An Automation document with the specified name could not be found.
_AutomationDefinitionNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AutomationDefinitionNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "AutomationDefinitionNotFoundException"
{-# DEPRECATED _AutomationDefinitionNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The policy type is not supported. Parameter Store supports the following policy types: Expiration, ExpirationNotification, and NoChangeNotification.
_InvalidPolicyTypeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidPolicyTypeException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidPolicyTypeException"
{-# DEPRECATED _InvalidPolicyTypeException "Use generic-lens or generic-optics instead." #-}

-- | The specified key is not valid.
_InvalidFilterKey :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidFilterKey =
  Core._MatchServiceError mkServiceConfig "InvalidFilterKey"
{-# DEPRECATED _InvalidFilterKey "Use generic-lens or generic-optics instead." #-}

-- | The supplied parameters for invoking the specified Automation document are incorrect. For example, they may not match the set of parameters permitted for the specified Automation document.
_InvalidAutomationExecutionParametersException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidAutomationExecutionParametersException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidAutomationExecutionParametersException"
{-# DEPRECATED _InvalidAutomationExecutionParametersException "Use generic-lens or generic-optics instead." #-}

-- | There is no automation execution information for the requested automation execution ID.
_AutomationExecutionNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AutomationExecutionNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "AutomationExecutionNotFoundException"
{-# DEPRECATED _AutomationExecutionNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The parameter type name is not valid.
_InvalidTypeNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTypeNameException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidTypeNameException"
{-# DEPRECATED _InvalidTypeNameException "Use generic-lens or generic-optics instead." #-}

-- | The specified sync name was not found.
_ResourceDataSyncNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceDataSyncNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceDataSyncNotFoundException"
{-# DEPRECATED _ResourceDataSyncNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | Parameter Store retains the 100 most recently created versions of a parameter. After this number of versions has been created, Parameter Store deletes the oldest version when a new one is created. However, if the oldest version has a /label/ attached to it, Parameter Store will not delete the version and instead presents this error message:
--
-- @An error occurred (ParameterMaxVersionLimitExceeded) when calling the PutParameter operation: You attempted to create a new version of /parameter-name/ by calling the PutParameter API with the overwrite flag. Version /version-number/ , the oldest version, can't be deleted because it has a label associated with it. Move the label to another version of the parameter, and try again.@
-- This safeguard is to prevent parameter versions with mission critical labels assigned to them from being deleted. To continue creating new parameters, first move the label from the oldest version of the parameter to a newer one for use in your operations. For information about moving parameter labels, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-labels.html#sysman-paramstore-labels-console-move Move a parameter label (console)> or <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-labels.html#sysman-paramstore-labels-cli-move Move a parameter label (CLI) > in the /AWS Systems Manager User Guide/ .
_ParameterMaxVersionLimitExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ParameterMaxVersionLimitExceeded =
  Core._MatchServiceError
    mkServiceConfig
    "ParameterMaxVersionLimitExceeded"
{-# DEPRECATED _ParameterMaxVersionLimitExceeded "Use generic-lens or generic-optics instead." #-}

-- | The inventory item size has exceeded the size limit.
_ItemSizeLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ItemSizeLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "ItemSizeLimitExceededException"
{-# DEPRECATED _ItemSizeLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | A sync configuration with the same name already exists.
_ResourceDataSyncAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceDataSyncAlreadyExistsException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceDataSyncAlreadyExistsException"
{-# DEPRECATED _ResourceDataSyncAlreadyExistsException "Use generic-lens or generic-optics instead." #-}

-- | Error returned when the ID specified for a resource, such as a maintenance window or Patch baseline, doesn't exist.
--
-- For information about resource quotas in Systems Manager, see <http://docs.aws.amazon.com/general/latest/gr/ssm.html#limits_ssm Systems Manager service quotas> in the /AWS General Reference/ .
_DoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DoesNotExistException =
  Core._MatchServiceError mkServiceConfig "DoesNotExistException"
{-# DEPRECATED _DoesNotExistException "Use generic-lens or generic-optics instead." #-}

-- | Another @UpdateResourceDataSync@ request is being processed. Wait a few minutes and try again.
_ResourceDataSyncConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceDataSyncConflictException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceDataSyncConflictException"
{-# DEPRECATED _ResourceDataSyncConflictException "Use generic-lens or generic-optics instead." #-}

-- | The number of simultaneously running Automation executions exceeded the allowable limit.
_AutomationExecutionLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AutomationExecutionLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "AutomationExecutionLimitExceededException"
{-# DEPRECATED _AutomationExecutionLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | Error returned when an idempotent operation is retried and the parameters don't match the original call to the API with the same idempotency token.
_IdempotentParameterMismatch :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IdempotentParameterMismatch =
  Core._MatchServiceError
    mkServiceConfig
    "IdempotentParameterMismatch"
{-# DEPRECATED _IdempotentParameterMismatch "Use generic-lens or generic-optics instead." #-}

-- | The specified filter value is not valid.
_InvalidInstanceInformationFilterValue :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidInstanceInformationFilterValue =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidInstanceInformationFilterValue"
{-# DEPRECATED _InvalidInstanceInformationFilterValue "Use generic-lens or generic-optics instead." #-}

-- | The inventory item has invalid content.
_ItemContentMismatchException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ItemContentMismatchException =
  Core._MatchServiceError
    mkServiceConfig
    "ItemContentMismatchException"
{-# DEPRECATED _ItemContentMismatchException "Use generic-lens or generic-optics instead." #-}

-- | The parameter already exists. You can't create duplicate parameters.
_ParameterAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ParameterAlreadyExists =
  Core._MatchServiceError mkServiceConfig "ParameterAlreadyExists"
{-# DEPRECATED _ParameterAlreadyExists "Use generic-lens or generic-optics instead." #-}

-- | The specified association already exists.
_AssociationAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AssociationAlreadyExists =
  Core._MatchServiceError
    mkServiceConfig
    "AssociationAlreadyExists"
{-# DEPRECATED _AssociationAlreadyExists "Use generic-lens or generic-optics instead." #-}

-- | You specified too many custom compliance types. You can specify a maximum of 10 different types.
_ComplianceTypeCountLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ComplianceTypeCountLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "ComplianceTypeCountLimitExceededException"
{-# DEPRECATED _ComplianceTypeCountLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | One or more of the parameters specified for the delete operation is not valid. Verify all parameters and try again.
_InvalidDeleteInventoryParametersException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDeleteInventoryParametersException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidDeleteInventoryParametersException"
{-# DEPRECATED _InvalidDeleteInventoryParametersException "Use generic-lens or generic-optics instead." #-}

-- | The ID specified for the delete operation does not exist or is not valid. Verify the ID and try again.
_InvalidDeletionIdException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDeletionIdException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidDeletionIdException"
{-# DEPRECATED _InvalidDeletionIdException "Use generic-lens or generic-optics instead." #-}

-- | You specified more than the maximum number of allowed policies for the parameter. The maximum is 10.
_PoliciesLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PoliciesLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "PoliciesLimitExceededException"
{-# DEPRECATED _PoliciesLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | The content for the document is not valid.
_InvalidDocumentContent :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDocumentContent =
  Core._MatchServiceError mkServiceConfig "InvalidDocumentContent"
{-# DEPRECATED _InvalidDocumentContent "Use generic-lens or generic-optics instead." #-}

-- | You have exceeded the number of parameters for this AWS account. Delete one or more parameters and try again.
_ParameterLimitExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ParameterLimitExceeded =
  Core._MatchServiceError mkServiceConfig "ParameterLimitExceeded"
{-# DEPRECATED _ParameterLimitExceeded "Use generic-lens or generic-optics instead." #-}

-- | You can have at most 2,000 active associations.
_AssociationLimitExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AssociationLimitExceeded =
  Core._MatchServiceError
    mkServiceConfig
    "AssociationLimitExceeded"
{-# DEPRECATED _AssociationLimitExceeded "Use generic-lens or generic-optics instead." #-}

-- | The version you specified is not valid. Use ListAssociationVersions to view all versions of an association according to the association ID. Or, use the @> LATEST@ parameter to view the latest version of the association.
_InvalidAssociationVersion :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidAssociationVersion =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidAssociationVersion"
{-# DEPRECATED _InvalidAssociationVersion "Use generic-lens or generic-optics instead." #-}

-- | The specified association does not exist.
_AssociationDoesNotExist :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AssociationDoesNotExist =
  Core._MatchServiceError mkServiceConfig "AssociationDoesNotExist"
{-# DEPRECATED _AssociationDoesNotExist "Use generic-lens or generic-optics instead." #-}

-- | A policy attribute or its value is invalid.
_InvalidPolicyAttributeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidPolicyAttributeException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidPolicyAttributeException"
{-# DEPRECATED _InvalidPolicyAttributeException "Use generic-lens or generic-optics instead." #-}

-- | The parameter could not be found. Verify the name and try again.
_ParameterNotFound :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ParameterNotFound =
  Core._MatchServiceError mkServiceConfig "ParameterNotFound"
{-# DEPRECATED _ParameterNotFound "Use generic-lens or generic-optics instead." #-}

-- | You specified the @Safe@ option for the DeregisterTargetFromMaintenanceWindow operation, but the target is still referenced in a task.
_TargetInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TargetInUseException =
  Core._MatchServiceError mkServiceConfig "TargetInUseException"
{-# DEPRECATED _TargetInUseException "Use generic-lens or generic-optics instead." #-}

-- | An error occurred on the server side.
_InternalServerError :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError mkServiceConfig "InternalServerError"
{-# DEPRECATED _InternalServerError "Use generic-lens or generic-optics instead." #-}

-- | The @Context@ attribute that you specified for the @InventoryItem@ is not allowed for this inventory type. You can only use the @Context@ attribute with inventory types like @AWS:ComplianceItem@ .
_UnsupportedInventoryItemContextException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedInventoryItemContextException =
  Core._MatchServiceError
    mkServiceConfig
    "UnsupportedInventoryItemContextException"
{-# DEPRECATED _UnsupportedInventoryItemContextException "Use generic-lens or generic-optics instead." #-}

-- | You have reached the maximum number versions allowed for an association. Each association has a limit of 1,000 versions.
_AssociationVersionLimitExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AssociationVersionLimitExceeded =
  Core._MatchServiceError
    mkServiceConfig
    "AssociationVersionLimitExceeded"
{-# DEPRECATED _AssociationVersionLimitExceeded "Use generic-lens or generic-optics instead." #-}

-- | The role name can't contain invalid characters. Also verify that you specified an IAM role for notifications that includes the required trust policy. For information about configuring the IAM role for Run Command notifications, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/rc-sns-notifications.html Configuring Amazon SNS Notifications for Run Command> in the /AWS Systems Manager User Guide/ .
_InvalidRole :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRole =
  Core._MatchServiceError mkServiceConfig "InvalidRole"
{-# DEPRECATED _InvalidRole "Use generic-lens or generic-optics instead." #-}

-- | There are concurrent updates for a resource that supports one update at a time.
_TooManyUpdates :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyUpdates =
  Core._MatchServiceError mkServiceConfig "TooManyUpdates"
{-# DEPRECATED _TooManyUpdates "Use generic-lens or generic-optics instead." #-}

-- | The version name has already been used in this document. Specify a different version name, and then try again.
_DuplicateDocumentVersionName :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateDocumentVersionName =
  Core._MatchServiceError
    mkServiceConfig
    "DuplicateDocumentVersionName"
{-# DEPRECATED _DuplicateDocumentVersionName "Use generic-lens or generic-optics instead." #-}

-- | The specified OpsItem ID doesn't exist. Verify the ID and try again.
_OpsItemNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OpsItemNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "OpsItemNotFoundException"
{-# DEPRECATED _OpsItemNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The activation is not valid. The activation might have been deleted, or the ActivationId and the ActivationCode do not match.
_InvalidActivation :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidActivation =
  Core._MatchServiceError mkServiceConfig "InvalidActivation"
{-# DEPRECATED _InvalidActivation "Use generic-lens or generic-optics instead." #-}

-- | The delete inventory option specified is not valid. Verify the option and try again.
_InvalidOptionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidOptionException =
  Core._MatchServiceError mkServiceConfig "InvalidOptionException"
{-# DEPRECATED _InvalidOptionException "Use generic-lens or generic-optics instead." #-}

-- | The version of the document schema is not supported.
_InvalidDocumentSchemaVersion :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDocumentSchemaVersion =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidDocumentSchemaVersion"
{-# DEPRECATED _InvalidDocumentSchemaVersion "Use generic-lens or generic-optics instead." #-}

-- | The size limit of a document is 64 KB.
_MaxDocumentSizeExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaxDocumentSizeExceeded =
  Core._MatchServiceError mkServiceConfig "MaxDocumentSizeExceeded"
{-# DEPRECATED _MaxDocumentSizeExceeded "Use generic-lens or generic-optics instead." #-}

-- | The specified parameter version was not found. Verify the parameter name and version, and try again.
_ParameterVersionNotFound :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ParameterVersionNotFound =
  Core._MatchServiceError
    mkServiceConfig
    "ParameterVersionNotFound"
{-# DEPRECATED _ParameterVersionNotFound "Use generic-lens or generic-optics instead." #-}

-- | The calendar entry contained in the specified Systems Manager document is not supported.
_UnsupportedCalendarException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedCalendarException =
  Core._MatchServiceError
    mkServiceConfig
    "UnsupportedCalendarException"
{-# DEPRECATED _UnsupportedCalendarException "Use generic-lens or generic-optics instead." #-}

-- | The update is not valid.
_InvalidUpdate :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidUpdate =
  Core._MatchServiceError mkServiceConfig "InvalidUpdate"
{-# DEPRECATED _InvalidUpdate "Use generic-lens or generic-optics instead." #-}

-- | You have exceeded the limit for custom schemas. Delete one or more custom schemas and try again.
_CustomSchemaCountLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CustomSchemaCountLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "CustomSchemaCountLimitExceededException"
{-# DEPRECATED _CustomSchemaCountLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | The specified execution ID does not exist. Verify the ID number and try again.
_AssociationExecutionDoesNotExist :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AssociationExecutionDoesNotExist =
  Core._MatchServiceError
    mkServiceConfig
    "AssociationExecutionDoesNotExist"
{-# DEPRECATED _AssociationExecutionDoesNotExist "Use generic-lens or generic-optics instead." #-}

-- | The target is not valid or does not exist. It might not be configured for Systems Manager or you might not have permission to perform the operation.
_InvalidTarget :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTarget =
  Core._MatchServiceError mkServiceConfig "InvalidTarget"
{-# DEPRECATED _InvalidTarget "Use generic-lens or generic-optics instead." #-}

-- | A hierarchy can have a maximum of 15 levels. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-parameter-name-constraints.html Requirements and constraints for parameter names> in the /AWS Systems Manager User Guide/ .
_HierarchyLevelLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_HierarchyLevelLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "HierarchyLevelLimitExceededException"
{-# DEPRECATED _HierarchyLevelLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | The specified inventory group is not valid.
_InvalidInventoryGroupException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidInventoryGroupException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidInventoryGroupException"
{-# DEPRECATED _InvalidInventoryGroupException "Use generic-lens or generic-optics instead." #-}

-- | You attempted to delete a document while it is still shared. You must stop sharing the document before you can delete it.
_InvalidDocumentOperation :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDocumentOperation =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidDocumentOperation"
{-# DEPRECATED _InvalidDocumentOperation "Use generic-lens or generic-optics instead." #-}

-- | The command ID and instance ID you specified did not match any invocations. Verify the command ID and the instance ID and try again.
_InvocationDoesNotExist :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvocationDoesNotExist =
  Core._MatchServiceError mkServiceConfig "InvocationDoesNotExist"
{-# DEPRECATED _InvocationDoesNotExist "Use generic-lens or generic-optics instead." #-}

-- | The document has too many versions. Delete one or more document versions and try again.
_DocumentVersionLimitExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DocumentVersionLimitExceeded =
  Core._MatchServiceError
    mkServiceConfig
    "DocumentVersionLimitExceeded"
{-# DEPRECATED _DocumentVersionLimitExceeded "Use generic-lens or generic-optics instead." #-}

-- | The output location is not valid or does not exist.
_InvalidOutputLocation :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidOutputLocation =
  Core._MatchServiceError mkServiceConfig "InvalidOutputLocation"
{-# DEPRECATED _InvalidOutputLocation "Use generic-lens or generic-optics instead." #-}

-- | The query key ID is not valid.
_InvalidKeyId :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidKeyId =
  Core._MatchServiceError mkServiceConfig "InvalidKeyId"
{-# DEPRECATED _InvalidKeyId "Use generic-lens or generic-optics instead." #-}

-- | You must specify values for all required parameters in the Systems Manager document. You can only supply values to parameters defined in the Systems Manager document.
_InvalidParameters :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameters =
  Core._MatchServiceError mkServiceConfig "InvalidParameters"
{-# DEPRECATED _InvalidParameters "Use generic-lens or generic-optics instead." #-}

-- | A specified parameter argument isn't valid. Verify the available arguments and try again.
_OpsItemInvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OpsItemInvalidParameterException =
  Core._MatchServiceError
    mkServiceConfig
    "OpsItemInvalidParameterException"
{-# DEPRECATED _OpsItemInvalidParameterException "Use generic-lens or generic-optics instead." #-}

-- | The resource ID is not valid. Verify that you entered the correct ID and try again.
_InvalidResourceId :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidResourceId =
  Core._MatchServiceError mkServiceConfig "InvalidResourceId"
{-# DEPRECATED _InvalidResourceId "Use generic-lens or generic-optics instead." #-}

-- | The request does not meet the regular expression requirement.
_InvalidAllowedPatternException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidAllowedPatternException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidAllowedPatternException"
{-# DEPRECATED _InvalidAllowedPatternException "Use generic-lens or generic-optics instead." #-}

-- | One or more configuration items is not valid. Verify that a valid Amazon Resource Name (ARN) was provided for an Amazon SNS topic.
_InvalidNotificationConfig :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNotificationConfig =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidNotificationConfig"
{-# DEPRECATED _InvalidNotificationConfig "Use generic-lens or generic-optics instead." #-}

-- | You specified invalid keys or values in the @Context@ attribute for @InventoryItem@ . Verify the keys and values, and try again.
_InvalidInventoryItemContextException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidInventoryItemContextException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidInventoryItemContextException"
{-# DEPRECATED _InvalidInventoryItemContextException "Use generic-lens or generic-optics instead." #-}

-- | The size of inventory data has exceeded the total size limit for the resource.
_TotalSizeLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TotalSizeLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "TotalSizeLimitExceededException"
{-# DEPRECATED _TotalSizeLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | The sub-type count exceeded the limit for the inventory type.
_SubTypeCountLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubTypeCountLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "SubTypeCountLimitExceededException"
{-# DEPRECATED _SubTypeCountLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | The document type is not valid. Valid document types are described in the @DocumentType@ property.
_InvalidDocumentType :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDocumentType =
  Core._MatchServiceError mkServiceConfig "InvalidDocumentType"
{-# DEPRECATED _InvalidDocumentType "Use generic-lens or generic-optics instead." #-}

-- | The @Targets@ parameter includes too many tags. Remove one or more tags and try the command again.
_TooManyTagsError :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTagsError =
  Core._MatchServiceError mkServiceConfig "TooManyTagsError"
{-# DEPRECATED _TooManyTagsError "Use generic-lens or generic-optics instead." #-}

-- | The document cannot be shared with more AWS user accounts. You can share a document with a maximum of 20 accounts. You can publicly share up to five documents. If you need to increase this limit, contact AWS Support.
_DocumentPermissionLimit :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DocumentPermissionLimit =
  Core._MatchServiceError mkServiceConfig "DocumentPermissionLimit"
{-# DEPRECATED _DocumentPermissionLimit "Use generic-lens or generic-optics instead." #-}

-- | The specified step name and execution ID don't exist. Verify the information and try again.
_AutomationStepNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AutomationStepNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "AutomationStepNotFoundException"
{-# DEPRECATED _AutomationStepNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The content of the association document matches another document. Change the content of the document and try again.
_DuplicateDocumentContent :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateDocumentContent =
  Core._MatchServiceError
    mkServiceConfig
    "DuplicateDocumentContent"
{-# DEPRECATED _DuplicateDocumentContent "Use generic-lens or generic-optics instead." #-}

-- | The specified document already exists.
_DocumentAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DocumentAlreadyExists =
  Core._MatchServiceError mkServiceConfig "DocumentAlreadyExists"
{-# DEPRECATED _DocumentAlreadyExists "Use generic-lens or generic-optics instead." #-}

-- | You can have at most 500 active Systems Manager documents.
_DocumentLimitExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DocumentLimitExceeded =
  Core._MatchServiceError mkServiceConfig "DocumentLimitExceeded"
{-# DEPRECATED _DocumentLimitExceeded "Use generic-lens or generic-optics instead." #-}

-- | Error returned if an attempt is made to register a patch group with a patch baseline that is already registered with a different patch baseline.
_AlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AlreadyExistsException =
  Core._MatchServiceError mkServiceConfig "AlreadyExistsException"
{-# DEPRECATED _AlreadyExistsException "Use generic-lens or generic-optics instead." #-}

-- | The filter name is not valid. Verify the you entered the correct name and try again.
_InvalidFilter :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidFilter =
  Core._MatchServiceError mkServiceConfig "InvalidFilter"
{-# DEPRECATED _InvalidFilter "Use generic-lens or generic-optics instead." #-}

-- | Error returned if an attempt is made to delete a patch baseline that is registered for a patch group.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError mkServiceConfig "ResourceInUseException"
{-# DEPRECATED _ResourceInUseException "Use generic-lens or generic-optics instead." #-}
