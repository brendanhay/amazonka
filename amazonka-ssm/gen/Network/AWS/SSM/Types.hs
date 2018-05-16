{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types
    (
    -- * Service Configuration
      ssm

    -- * Errors
    , _AutomationDefinitionVersionNotFoundException
    , _InvalidDocumentVersion
    , _HierarchyTypeMismatchException
    , _InvalidSchedule
    , _UnsupportedParameterType
    , _InvalidAutomationStatusUpdateException
    , _InvalidPluginName
    , _FeatureNotAvailableException
    , _InvalidAutomationSignalException
    , _ResourceDataSyncCountExceededException
    , _UnsupportedPlatformType
    , _InvalidFilterValue
    , _InvalidItemContentException
    , _InvalidFilterOption
    , _ParameterPatternMismatchException
    , _InvalidPermissionType
    , _AssociatedInstances
    , _UnsupportedOperatingSystem
    , _InvalidInstanceId
    , _StatusUnchanged
    , _InvalidNextToken
    , _InvalidInventoryRequestException
    , _InvalidOutputFolder
    , _InvalidActivationId
    , _InvalidResultAttributeException
    , _ResourceLimitExceededException
    , _ResourceDataSyncInvalidConfigurationException
    , _InvalidCommandId
    , _DuplicateInstanceId
    , _InvalidResourceType
    , _UnsupportedInventorySchemaVersionException
    , _InvalidDocument
    , _AutomationDefinitionNotFoundException
    , _InvalidFilterKey
    , _InvalidAutomationExecutionParametersException
    , _AutomationExecutionNotFoundException
    , _InvalidTypeNameException
    , _ResourceDataSyncNotFoundException
    , _ParameterMaxVersionLimitExceeded
    , _ItemSizeLimitExceededException
    , _ResourceDataSyncAlreadyExistsException
    , _DoesNotExistException
    , _AutomationExecutionLimitExceededException
    , _IdempotentParameterMismatch
    , _InvalidInstanceInformationFilterValue
    , _ItemContentMismatchException
    , _ParameterAlreadyExists
    , _AssociationAlreadyExists
    , _ComplianceTypeCountLimitExceededException
    , _InvalidDeleteInventoryParametersException
    , _InvalidDeletionIdException
    , _InvalidDocumentContent
    , _ParameterLimitExceeded
    , _AssociationLimitExceeded
    , _InvalidAssociationVersion
    , _AssociationDoesNotExist
    , _ParameterNotFound
    , _TargetInUseException
    , _InternalServerError
    , _UnsupportedInventoryItemContextException
    , _AssociationVersionLimitExceeded
    , _InvalidRole
    , _TooManyUpdates
    , _InvalidActivation
    , _InvalidOptionException
    , _InvalidDocumentSchemaVersion
    , _MaxDocumentSizeExceeded
    , _ParameterVersionNotFound
    , _InvalidUpdate
    , _CustomSchemaCountLimitExceededException
    , _InvalidTarget
    , _HierarchyLevelLimitExceededException
    , _InvalidDocumentOperation
    , _InvocationDoesNotExist
    , _DocumentVersionLimitExceeded
    , _InvalidOutputLocation
    , _InvalidKeyId
    , _InvalidParameters
    , _InvalidResourceId
    , _InvalidAllowedPatternException
    , _InvalidNotificationConfig
    , _InvalidInventoryItemContextException
    , _TotalSizeLimitExceededException
    , _SubTypeCountLimitExceededException
    , _TooManyTagsError
    , _DocumentPermissionLimit
    , _AutomationStepNotFoundException
    , _DuplicateDocumentContent
    , _DocumentAlreadyExists
    , _DocumentLimitExceeded
    , _AlreadyExistsException
    , _InvalidFilter
    , _ResourceInUseException

    -- * AssociationFilterKey
    , AssociationFilterKey (..)

    -- * AssociationStatusName
    , AssociationStatusName (..)

    -- * AutomationExecutionFilterKey
    , AutomationExecutionFilterKey (..)

    -- * AutomationExecutionStatus
    , AutomationExecutionStatus (..)

    -- * CommandFilterKey
    , CommandFilterKey (..)

    -- * CommandInvocationStatus
    , CommandInvocationStatus (..)

    -- * CommandPluginStatus
    , CommandPluginStatus (..)

    -- * CommandStatus
    , CommandStatus (..)

    -- * ComplianceQueryOperatorType
    , ComplianceQueryOperatorType (..)

    -- * ComplianceSeverity
    , ComplianceSeverity (..)

    -- * ComplianceStatus
    , ComplianceStatus (..)

    -- * DescribeActivationsFilterKeys
    , DescribeActivationsFilterKeys (..)

    -- * DocumentFilterKey
    , DocumentFilterKey (..)

    -- * DocumentFormat
    , DocumentFormat (..)

    -- * DocumentHashType
    , DocumentHashType (..)

    -- * DocumentParameterType
    , DocumentParameterType (..)

    -- * DocumentPermissionType
    , DocumentPermissionType (..)

    -- * DocumentStatus
    , DocumentStatus (..)

    -- * DocumentType
    , DocumentType (..)

    -- * ExecutionMode
    , ExecutionMode (..)

    -- * Fault
    , Fault (..)

    -- * InstanceInformationFilterKey
    , InstanceInformationFilterKey (..)

    -- * InstancePatchStateOperatorType
    , InstancePatchStateOperatorType (..)

    -- * InventoryAttributeDataType
    , InventoryAttributeDataType (..)

    -- * InventoryDeletionStatus
    , InventoryDeletionStatus (..)

    -- * InventoryQueryOperatorType
    , InventoryQueryOperatorType (..)

    -- * InventorySchemaDeleteOption
    , InventorySchemaDeleteOption (..)

    -- * LastResourceDataSyncStatus
    , LastResourceDataSyncStatus (..)

    -- * MaintenanceWindowExecutionStatus
    , MaintenanceWindowExecutionStatus (..)

    -- * MaintenanceWindowResourceType
    , MaintenanceWindowResourceType (..)

    -- * MaintenanceWindowTaskType
    , MaintenanceWindowTaskType (..)

    -- * NotificationEvent
    , NotificationEvent (..)

    -- * NotificationType
    , NotificationType (..)

    -- * OperatingSystem
    , OperatingSystem (..)

    -- * ParameterType
    , ParameterType (..)

    -- * ParametersFilterKey
    , ParametersFilterKey (..)

    -- * PatchComplianceDataState
    , PatchComplianceDataState (..)

    -- * PatchComplianceLevel
    , PatchComplianceLevel (..)

    -- * PatchDeploymentStatus
    , PatchDeploymentStatus (..)

    -- * PatchFilterKey
    , PatchFilterKey (..)

    -- * PatchOperationType
    , PatchOperationType (..)

    -- * PingStatus
    , PingStatus (..)

    -- * PlatformType
    , PlatformType (..)

    -- * ResourceDataSyncS3Format
    , ResourceDataSyncS3Format (..)

    -- * ResourceType
    , ResourceType (..)

    -- * ResourceTypeForTagging
    , ResourceTypeForTagging (..)

    -- * SignalType
    , SignalType (..)

    -- * StepExecutionFilterKey
    , StepExecutionFilterKey (..)

    -- * StopType
    , StopType (..)

    -- * Activation
    , Activation
    , activation
    , aExpired
    , aDefaultInstanceName
    , aActivationId
    , aCreatedDate
    , aRegistrationLimit
    , aExpirationDate
    , aDescription
    , aRegistrationsCount
    , aIAMRole

    -- * Association
    , Association
    , association
    , aAssociationId
    , aInstanceId
    , aOverview
    , aLastExecutionDate
    , aScheduleExpression
    , aName
    , aTargets
    , aDocumentVersion
    , aAssociationVersion
    , aAssociationName

    -- * AssociationDescription
    , AssociationDescription
    , associationDescription
    , adAssociationId
    , adInstanceId
    , adStatus
    , adLastSuccessfulExecutionDate
    , adOverview
    , adLastUpdateAssociationDate
    , adDate
    , adLastExecutionDate
    , adScheduleExpression
    , adName
    , adOutputLocation
    , adTargets
    , adParameters
    , adDocumentVersion
    , adAssociationVersion
    , adAssociationName

    -- * AssociationFilter
    , AssociationFilter
    , associationFilter
    , afKey
    , afValue

    -- * AssociationOverview
    , AssociationOverview
    , associationOverview
    , aoDetailedStatus
    , aoStatus
    , aoAssociationStatusAggregatedCount

    -- * AssociationStatus
    , AssociationStatus
    , associationStatus
    , asAdditionalInfo
    , asDate
    , asName
    , asMessage

    -- * AssociationVersionInfo
    , AssociationVersionInfo
    , associationVersionInfo
    , aviAssociationId
    , aviCreatedDate
    , aviScheduleExpression
    , aviName
    , aviOutputLocation
    , aviTargets
    , aviParameters
    , aviDocumentVersion
    , aviAssociationVersion
    , aviAssociationName

    -- * AutomationExecution
    , AutomationExecution
    , automationExecution
    , aeCurrentStepName
    , aeTargetParameterName
    , aeExecutedBy
    , aeDocumentName
    , aeExecutionEndTime
    , aeFailureMessage
    , aeMode
    , aeStepExecutionsTruncated
    , aeAutomationExecutionStatus
    , aeParentAutomationExecutionId
    , aeOutputs
    , aeMaxErrors
    , aeExecutionStartTime
    , aeCurrentAction
    , aeTargets
    , aeResolvedTargets
    , aeParameters
    , aeDocumentVersion
    , aeAutomationExecutionId
    , aeStepExecutions
    , aeMaxConcurrency
    , aeTarget

    -- * AutomationExecutionFilter
    , AutomationExecutionFilter
    , automationExecutionFilter
    , aefKey
    , aefValues

    -- * AutomationExecutionMetadata
    , AutomationExecutionMetadata
    , automationExecutionMetadata
    , aemCurrentStepName
    , aemTargetParameterName
    , aemLogFile
    , aemExecutedBy
    , aemDocumentName
    , aemExecutionEndTime
    , aemFailureMessage
    , aemMode
    , aemAutomationExecutionStatus
    , aemParentAutomationExecutionId
    , aemOutputs
    , aemMaxErrors
    , aemExecutionStartTime
    , aemCurrentAction
    , aemTargets
    , aemResolvedTargets
    , aemDocumentVersion
    , aemAutomationExecutionId
    , aemMaxConcurrency
    , aemTarget

    -- * Command
    , Command
    , command
    , cStatus
    , cExpiresAfter
    , cNotificationConfig
    , cTargetCount
    , cOutputS3KeyPrefix
    , cDocumentName
    , cErrorCount
    , cStatusDetails
    , cMaxErrors
    , cInstanceIds
    , cOutputS3Region
    , cTargets
    , cCommandId
    , cParameters
    , cDocumentVersion
    , cComment
    , cCompletedCount
    , cOutputS3BucketName
    , cMaxConcurrency
    , cRequestedDateTime
    , cServiceRole

    -- * CommandFilter
    , CommandFilter
    , commandFilter
    , cfKey
    , cfValue

    -- * CommandInvocation
    , CommandInvocation
    , commandInvocation
    , comInstanceId
    , comStatus
    , comNotificationConfig
    , comCommandPlugins
    , comDocumentName
    , comStandardErrorURL
    , comStatusDetails
    , comStandardOutputURL
    , comCommandId
    , comDocumentVersion
    , comComment
    , comTraceOutput
    , comInstanceName
    , comRequestedDateTime
    , comServiceRole

    -- * CommandPlugin
    , CommandPlugin
    , commandPlugin
    , cpStatus
    , cpResponseStartDateTime
    , cpOutputS3KeyPrefix
    , cpStandardErrorURL
    , cpResponseCode
    , cpStatusDetails
    , cpOutput
    , cpStandardOutputURL
    , cpName
    , cpOutputS3Region
    , cpOutputS3BucketName
    , cpResponseFinishDateTime

    -- * ComplianceExecutionSummary
    , ComplianceExecutionSummary
    , complianceExecutionSummary
    , cesExecutionId
    , cesExecutionType
    , cesExecutionTime

    -- * ComplianceItem
    , ComplianceItem
    , complianceItem
    , ciStatus
    , ciResourceId
    , ciResourceType
    , ciSeverity
    , ciExecutionSummary
    , ciDetails
    , ciId
    , ciComplianceType
    , ciTitle

    -- * ComplianceItemEntry
    , ComplianceItemEntry
    , complianceItemEntry
    , cieDetails
    , cieId
    , cieTitle
    , cieSeverity
    , cieStatus

    -- * ComplianceStringFilter
    , ComplianceStringFilter
    , complianceStringFilter
    , csfValues
    , csfKey
    , csfType

    -- * ComplianceSummaryItem
    , ComplianceSummaryItem
    , complianceSummaryItem
    , csiNonCompliantSummary
    , csiCompliantSummary
    , csiComplianceType

    -- * CompliantSummary
    , CompliantSummary
    , compliantSummary
    , csCompliantCount
    , csSeveritySummary

    -- * CreateAssociationBatchRequestEntry
    , CreateAssociationBatchRequestEntry
    , createAssociationBatchRequestEntry
    , cabreInstanceId
    , cabreScheduleExpression
    , cabreOutputLocation
    , cabreTargets
    , cabreParameters
    , cabreDocumentVersion
    , cabreAssociationName
    , cabreName

    -- * DescribeActivationsFilter
    , DescribeActivationsFilter
    , describeActivationsFilter
    , dafFilterKey
    , dafFilterValues

    -- * DocumentDefaultVersionDescription
    , DocumentDefaultVersionDescription
    , documentDefaultVersionDescription
    , ddvdDefaultVersion
    , ddvdName

    -- * DocumentDescription
    , DocumentDescription
    , documentDescription
    , dStatus
    , dDocumentType
    , dHash
    , dSchemaVersion
    , dSha1
    , dDefaultVersion
    , dTargetType
    , dOwner
    , dPlatformTypes
    , dCreatedDate
    , dDocumentFormat
    , dName
    , dHashType
    , dParameters
    , dDocumentVersion
    , dDescription
    , dTags
    , dLatestVersion

    -- * DocumentFilter
    , DocumentFilter
    , documentFilter
    , dfKey
    , dfValue

    -- * DocumentIdentifier
    , DocumentIdentifier
    , documentIdentifier
    , diDocumentType
    , diSchemaVersion
    , diTargetType
    , diOwner
    , diPlatformTypes
    , diDocumentFormat
    , diName
    , diDocumentVersion
    , diTags

    -- * DocumentKeyValuesFilter
    , DocumentKeyValuesFilter
    , documentKeyValuesFilter
    , dkvfValues
    , dkvfKey

    -- * DocumentParameter
    , DocumentParameter
    , documentParameter
    , dpName
    , dpDefaultValue
    , dpType
    , dpDescription

    -- * DocumentVersionInfo
    , DocumentVersionInfo
    , documentVersionInfo
    , dviCreatedDate
    , dviDocumentFormat
    , dviName
    , dviDocumentVersion
    , dviIsDefaultVersion

    -- * EffectivePatch
    , EffectivePatch
    , effectivePatch
    , epPatch
    , epPatchStatus

    -- * FailedCreateAssociation
    , FailedCreateAssociation
    , failedCreateAssociation
    , fcaEntry
    , fcaFault
    , fcaMessage

    -- * FailureDetails
    , FailureDetails
    , failureDetails
    , fdFailureType
    , fdFailureStage
    , fdDetails

    -- * InstanceAggregatedAssociationOverview
    , InstanceAggregatedAssociationOverview
    , instanceAggregatedAssociationOverview
    , iaaoDetailedStatus
    , iaaoInstanceAssociationStatusAggregatedCount

    -- * InstanceAssociation
    , InstanceAssociation
    , instanceAssociation
    , iaAssociationId
    , iaInstanceId
    , iaContent
    , iaAssociationVersion

    -- * InstanceAssociationOutputLocation
    , InstanceAssociationOutputLocation
    , instanceAssociationOutputLocation
    , iaolS3Location

    -- * InstanceAssociationOutputURL
    , InstanceAssociationOutputURL
    , instanceAssociationOutputURL
    , iaouS3OutputURL

    -- * InstanceAssociationStatusInfo
    , InstanceAssociationStatusInfo
    , instanceAssociationStatusInfo
    , iasiAssociationId
    , iasiInstanceId
    , iasiDetailedStatus
    , iasiStatus
    , iasiOutputURL
    , iasiExecutionSummary
    , iasiName
    , iasiErrorCode
    , iasiDocumentVersion
    , iasiAssociationVersion
    , iasiExecutionDate
    , iasiAssociationName

    -- * InstanceInformation
    , InstanceInformation
    , instanceInformation
    , iiInstanceId
    , iiPingStatus
    , iiIPAddress
    , iiResourceType
    , iiRegistrationDate
    , iiPlatformVersion
    , iiIsLatestVersion
    , iiAgentVersion
    , iiLastPingDateTime
    , iiLastSuccessfulAssociationExecutionDate
    , iiActivationId
    , iiName
    , iiPlatformType
    , iiAssociationOverview
    , iiAssociationStatus
    , iiLastAssociationExecutionDate
    , iiPlatformName
    , iiComputerName
    , iiIAMRole

    -- * InstanceInformationFilter
    , InstanceInformationFilter
    , instanceInformationFilter
    , iifKey
    , iifValueSet

    -- * InstanceInformationStringFilter
    , InstanceInformationStringFilter
    , instanceInformationStringFilter
    , iisfKey
    , iisfValues

    -- * InstancePatchState
    , InstancePatchState
    , instancePatchState
    , ipsOwnerInformation
    , ipsFailedCount
    , ipsInstalledOtherCount
    , ipsMissingCount
    , ipsNotApplicableCount
    , ipsInstalledCount
    , ipsSnapshotId
    , ipsInstanceId
    , ipsPatchGroup
    , ipsBaselineId
    , ipsOperationStartTime
    , ipsOperationEndTime
    , ipsOperation

    -- * InstancePatchStateFilter
    , InstancePatchStateFilter
    , instancePatchStateFilter
    , ipsfKey
    , ipsfValues
    , ipsfType

    -- * InventoryAggregator
    , InventoryAggregator
    , inventoryAggregator
    , iaAggregators
    , iaExpression

    -- * InventoryDeletionStatusItem
    , InventoryDeletionStatusItem
    , inventoryDeletionStatusItem
    , idsiTypeName
    , idsiLastStatusUpdateTime
    , idsiLastStatusMessage
    , idsiDeletionSummary
    , idsiLastStatus
    , idsiDeletionStartTime
    , idsiDeletionId

    -- * InventoryDeletionSummary
    , InventoryDeletionSummary
    , inventoryDeletionSummary
    , idsRemainingCount
    , idsSummaryItems
    , idsTotalCount

    -- * InventoryDeletionSummaryItem
    , InventoryDeletionSummaryItem
    , inventoryDeletionSummaryItem
    , idsiRemainingCount
    , idsiCount
    , idsiVersion

    -- * InventoryFilter
    , InventoryFilter
    , inventoryFilter
    , ifType
    , ifKey
    , ifValues

    -- * InventoryItem
    , InventoryItem
    , inventoryItem
    , iiContext
    , iiContentHash
    , iiContent
    , iiTypeName
    , iiSchemaVersion
    , iiCaptureTime

    -- * InventoryItemAttribute
    , InventoryItemAttribute
    , inventoryItemAttribute
    , iiaName
    , iiaDataType

    -- * InventoryItemSchema
    , InventoryItemSchema
    , inventoryItemSchema
    , iisVersion
    , iisDisplayName
    , iisTypeName
    , iisAttributes

    -- * InventoryResultEntity
    , InventoryResultEntity
    , inventoryResultEntity
    , ireData
    , ireId

    -- * InventoryResultItem
    , InventoryResultItem
    , inventoryResultItem
    , iriContentHash
    , iriCaptureTime
    , iriTypeName
    , iriSchemaVersion
    , iriContent

    -- * LoggingInfo
    , LoggingInfo
    , loggingInfo
    , liS3KeyPrefix
    , liS3BucketName
    , liS3Region

    -- * MaintenanceWindowAutomationParameters
    , MaintenanceWindowAutomationParameters
    , maintenanceWindowAutomationParameters
    , mwapParameters
    , mwapDocumentVersion

    -- * MaintenanceWindowExecution
    , MaintenanceWindowExecution
    , maintenanceWindowExecution
    , mweStatus
    , mweStartTime
    , mweWindowExecutionId
    , mweStatusDetails
    , mweEndTime
    , mweWindowId

    -- * MaintenanceWindowExecutionTaskIdentity
    , MaintenanceWindowExecutionTaskIdentity
    , maintenanceWindowExecutionTaskIdentity
    , mwetiStatus
    , mwetiTaskExecutionId
    , mwetiStartTime
    , mwetiTaskType
    , mwetiTaskARN
    , mwetiWindowExecutionId
    , mwetiStatusDetails
    , mwetiEndTime

    -- * MaintenanceWindowExecutionTaskInvocationIdentity
    , MaintenanceWindowExecutionTaskInvocationIdentity
    , maintenanceWindowExecutionTaskInvocationIdentity
    , mwetiiStatus
    , mwetiiExecutionId
    , mwetiiTaskExecutionId
    , mwetiiStartTime
    , mwetiiInvocationId
    , mwetiiOwnerInformation
    , mwetiiTaskType
    , mwetiiWindowTargetId
    , mwetiiWindowExecutionId
    , mwetiiStatusDetails
    , mwetiiEndTime
    , mwetiiParameters

    -- * MaintenanceWindowFilter
    , MaintenanceWindowFilter
    , maintenanceWindowFilter
    , mwfValues
    , mwfKey

    -- * MaintenanceWindowIdentity
    , MaintenanceWindowIdentity
    , maintenanceWindowIdentity
    , mwiEnabled
    , mwiName
    , mwiCutoff
    , mwiDescription
    , mwiDuration
    , mwiWindowId

    -- * MaintenanceWindowLambdaParameters
    , MaintenanceWindowLambdaParameters
    , maintenanceWindowLambdaParameters
    , mwlpPayload
    , mwlpQualifier
    , mwlpClientContext

    -- * MaintenanceWindowRunCommandParameters
    , MaintenanceWindowRunCommandParameters
    , maintenanceWindowRunCommandParameters
    , mwrcpServiceRoleARN
    , mwrcpNotificationConfig
    , mwrcpDocumentHashType
    , mwrcpOutputS3KeyPrefix
    , mwrcpParameters
    , mwrcpDocumentHash
    , mwrcpTimeoutSeconds
    , mwrcpComment
    , mwrcpOutputS3BucketName

    -- * MaintenanceWindowStepFunctionsParameters
    , MaintenanceWindowStepFunctionsParameters
    , maintenanceWindowStepFunctionsParameters
    , mwsfpInput
    , mwsfpName

    -- * MaintenanceWindowTarget
    , MaintenanceWindowTarget
    , maintenanceWindowTarget
    , mResourceType
    , mOwnerInformation
    , mWindowTargetId
    , mName
    , mTargets
    , mDescription
    , mWindowId

    -- * MaintenanceWindowTask
    , MaintenanceWindowTask
    , maintenanceWindowTask
    , mwtServiceRoleARN
    , mwtWindowTaskId
    , mwtTaskParameters
    , mwtPriority
    , mwtTaskARN
    , mwtMaxErrors
    , mwtName
    , mwtTargets
    , mwtLoggingInfo
    , mwtType
    , mwtDescription
    , mwtMaxConcurrency
    , mwtWindowId

    -- * MaintenanceWindowTaskInvocationParameters
    , MaintenanceWindowTaskInvocationParameters
    , maintenanceWindowTaskInvocationParameters
    , mwtipAutomation
    , mwtipStepFunctions
    , mwtipRunCommand
    , mwtipLambda

    -- * MaintenanceWindowTaskParameterValueExpression
    , MaintenanceWindowTaskParameterValueExpression
    , maintenanceWindowTaskParameterValueExpression
    , mwtpveValues

    -- * NonCompliantSummary
    , NonCompliantSummary
    , nonCompliantSummary
    , ncsNonCompliantCount
    , ncsSeveritySummary

    -- * NotificationConfig
    , NotificationConfig
    , notificationConfig
    , ncNotificationEvents
    , ncNotificationType
    , ncNotificationARN

    -- * Parameter
    , Parameter
    , parameter
    , pValue
    , pName
    , pVersion
    , pType

    -- * ParameterHistory
    , ParameterHistory
    , parameterHistory
    , phLastModifiedDate
    , phKeyId
    , phValue
    , phName
    , phVersion
    , phLastModifiedUser
    , phAllowedPattern
    , phType
    , phDescription

    -- * ParameterMetadata
    , ParameterMetadata
    , parameterMetadata
    , pmLastModifiedDate
    , pmKeyId
    , pmName
    , pmVersion
    , pmLastModifiedUser
    , pmAllowedPattern
    , pmType
    , pmDescription

    -- * ParameterStringFilter
    , ParameterStringFilter
    , parameterStringFilter
    , psfValues
    , psfOption
    , psfKey

    -- * ParametersFilter
    , ParametersFilter
    , parametersFilter
    , pKey
    , pValues

    -- * Patch
    , Patch
    , patch
    , pVendor
    , pMsrcSeverity
    , pProductFamily
    , pClassification
    , pMsrcNumber
    , pLanguage
    , pKbNumber
    , pContentURL
    , pId
    , pReleaseDate
    , pTitle
    , pProduct
    , pDescription

    -- * PatchBaselineIdentity
    , PatchBaselineIdentity
    , patchBaselineIdentity
    , pbiBaselineName
    , pbiBaselineDescription
    , pbiOperatingSystem
    , pbiDefaultBaseline
    , pbiBaselineId

    -- * PatchComplianceData
    , PatchComplianceData
    , patchComplianceData
    , pcdTitle
    , pcdKBId
    , pcdClassification
    , pcdSeverity
    , pcdState
    , pcdInstalledTime

    -- * PatchFilter
    , PatchFilter
    , patchFilter
    , pfKey
    , pfValues

    -- * PatchFilterGroup
    , PatchFilterGroup
    , patchFilterGroup
    , pfgPatchFilters

    -- * PatchGroupPatchBaselineMapping
    , PatchGroupPatchBaselineMapping
    , patchGroupPatchBaselineMapping
    , pgpbmBaselineIdentity
    , pgpbmPatchGroup

    -- * PatchOrchestratorFilter
    , PatchOrchestratorFilter
    , patchOrchestratorFilter
    , pofValues
    , pofKey

    -- * PatchRule
    , PatchRule
    , patchRule
    , prEnableNonSecurity
    , prComplianceLevel
    , prPatchFilterGroup
    , prApproveAfterDays

    -- * PatchRuleGroup
    , PatchRuleGroup
    , patchRuleGroup
    , prgPatchRules

    -- * PatchSource
    , PatchSource
    , patchSource
    , psName
    , psProducts
    , psConfiguration

    -- * PatchStatus
    , PatchStatus
    , patchStatus
    , psApprovalDate
    , psDeploymentStatus
    , psComplianceLevel

    -- * ResolvedTargets
    , ResolvedTargets
    , resolvedTargets
    , rtTruncated
    , rtParameterValues

    -- * ResourceComplianceSummaryItem
    , ResourceComplianceSummaryItem
    , resourceComplianceSummaryItem
    , rcsiNonCompliantSummary
    , rcsiStatus
    , rcsiResourceId
    , rcsiResourceType
    , rcsiCompliantSummary
    , rcsiExecutionSummary
    , rcsiOverallSeverity
    , rcsiComplianceType

    -- * ResourceDataSyncItem
    , ResourceDataSyncItem
    , resourceDataSyncItem
    , rdsiLastSyncStatusMessage
    , rdsiSyncCreatedTime
    , rdsiLastSyncTime
    , rdsiSyncName
    , rdsiLastStatus
    , rdsiS3Destination
    , rdsiLastSuccessfulSyncTime

    -- * ResourceDataSyncS3Destination
    , ResourceDataSyncS3Destination
    , resourceDataSyncS3Destination
    , rdssdPrefix
    , rdssdAWSKMSKeyARN
    , rdssdBucketName
    , rdssdSyncFormat
    , rdssdRegion

    -- * ResultAttribute
    , ResultAttribute
    , resultAttribute
    , raTypeName

    -- * S3OutputLocation
    , S3OutputLocation
    , s3OutputLocation
    , solOutputS3KeyPrefix
    , solOutputS3Region
    , solOutputS3BucketName

    -- * S3OutputURL
    , S3OutputURL
    , s3OutputURL
    , souOutputURL

    -- * SeveritySummary
    , SeveritySummary
    , severitySummary
    , ssLowCount
    , ssUnspecifiedCount
    , ssHighCount
    , ssMediumCount
    , ssInformationalCount
    , ssCriticalCount

    -- * StepExecution
    , StepExecution
    , stepExecution
    , seFailureDetails
    , seInputs
    , seStepName
    , seExecutionEndTime
    , seFailureMessage
    , seResponse
    , seAction
    , seResponseCode
    , seStepStatus
    , seOverriddenParameters
    , seOutputs
    , seExecutionStartTime
    , seMaxAttempts
    , seStepExecutionId
    , seTimeoutSeconds
    , seOnFailure

    -- * StepExecutionFilter
    , StepExecutionFilter
    , stepExecutionFilter
    , sefKey
    , sefValues

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- * Target
    , Target
    , target
    , tValues
    , tKey
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.SSM.Types.Product
import Network.AWS.SSM.Types.Sum

-- | API version @2014-11-06@ of the Amazon Simple Systems Manager (SSM) SDK configuration.
ssm :: Service
ssm =
  Service
    { _svcAbbrev = "SSM"
    , _svcSigner = v4
    , _svcPrefix = "ssm"
    , _svcVersion = "2014-11-06"
    , _svcEndpoint = defaultEndpoint ssm
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "SSM"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | An Automation document with the specified name and version could not be found.
--
--
_AutomationDefinitionVersionNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_AutomationDefinitionVersionNotFoundException =
  _MatchServiceError ssm "AutomationDefinitionVersionNotFoundException"


-- | The document version is not valid or does not exist.
--
--
_InvalidDocumentVersion :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDocumentVersion = _MatchServiceError ssm "InvalidDocumentVersion"


-- | Parameter Store does not support changing a parameter type in a hierarchy. For example, you can't change a parameter from a String type to a SecureString type. You must create a new, unique parameter.
--
--
_HierarchyTypeMismatchException :: AsError a => Getting (First ServiceError) a ServiceError
_HierarchyTypeMismatchException =
  _MatchServiceError ssm "HierarchyTypeMismatchException"


-- | The schedule is invalid. Verify your cron or rate expression and try again.
--
--
_InvalidSchedule :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSchedule = _MatchServiceError ssm "InvalidSchedule"


-- | The parameter type is not supported.
--
--
_UnsupportedParameterType :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedParameterType = _MatchServiceError ssm "UnsupportedParameterType"


-- | The specified update status operation is not valid.
--
--
_InvalidAutomationStatusUpdateException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidAutomationStatusUpdateException =
  _MatchServiceError ssm "InvalidAutomationStatusUpdateException"


-- | The plugin name is not valid.
--
--
_InvalidPluginName :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidPluginName = _MatchServiceError ssm "InvalidPluginName"


-- | You attempted to register a LAMBDA or STEP_FUNCTION task in a region where the corresponding service is not available.
--
--
_FeatureNotAvailableException :: AsError a => Getting (First ServiceError) a ServiceError
_FeatureNotAvailableException =
  _MatchServiceError ssm "FeatureNotAvailableException"


-- | The signal is not valid for the current Automation execution.
--
--
_InvalidAutomationSignalException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidAutomationSignalException =
  _MatchServiceError ssm "InvalidAutomationSignalException"


-- | You have exceeded the allowed maximum sync configurations.
--
--
_ResourceDataSyncCountExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceDataSyncCountExceededException =
  _MatchServiceError ssm "ResourceDataSyncCountExceededException"


-- | The document does not support the platform type of the given instance ID(s). For example, you sent an document for a Windows instance to a Linux instance.
--
--
_UnsupportedPlatformType :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedPlatformType = _MatchServiceError ssm "UnsupportedPlatformType"


-- | The filter value is not valid. Verify the value and try again.
--
--
_InvalidFilterValue :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidFilterValue = _MatchServiceError ssm "InvalidFilterValue"


-- | One or more content items is not valid.
--
--
_InvalidItemContentException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidItemContentException =
  _MatchServiceError ssm "InvalidItemContentException"


-- | The specified filter option is not valid. Valid options are Equals and BeginsWith. For Path filter, valid options are Recursive and OneLevel.
--
--
_InvalidFilterOption :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidFilterOption = _MatchServiceError ssm "InvalidFilterOption"


-- | The parameter name is not valid.
--
--
_ParameterPatternMismatchException :: AsError a => Getting (First ServiceError) a ServiceError
_ParameterPatternMismatchException =
  _MatchServiceError ssm "ParameterPatternMismatchException"


-- | The permission type is not supported. /Share/ is the only supported permission type.
--
--
_InvalidPermissionType :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidPermissionType = _MatchServiceError ssm "InvalidPermissionType"


-- | You must disassociate a document from all instances before you can delete it.
--
--
_AssociatedInstances :: AsError a => Getting (First ServiceError) a ServiceError
_AssociatedInstances = _MatchServiceError ssm "AssociatedInstances"


-- | The operating systems you specified is not supported, or the operation is not supported for the operating system. Valid operating systems include: Windows, AmazonLinux, RedhatEnterpriseLinux, and Ubuntu.
--
--
_UnsupportedOperatingSystem :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedOperatingSystem =
  _MatchServiceError ssm "UnsupportedOperatingSystem"


-- | The following problems can cause this exception:
--
--
-- You do not have permission to access the instance.
--
-- The SSM Agent is not running. On managed instances and Linux instances, verify that the SSM Agent is running. On EC2 Windows instances, verify that the EC2Config service is running.
--
-- The SSM Agent or EC2Config service is not registered to the SSM endpoint. Try reinstalling the SSM Agent or EC2Config service.
--
-- The instance is not in valid state. Valid states are: Running, Pending, Stopped, Stopping. Invalid states are: Shutting-down and Terminated.
--
_InvalidInstanceId :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInstanceId = _MatchServiceError ssm "InvalidInstanceId"


-- | The updated status is the same as the current status.
--
--
_StatusUnchanged :: AsError a => Getting (First ServiceError) a ServiceError
_StatusUnchanged = _MatchServiceError ssm "StatusUnchanged"


-- | The specified token is not valid.
--
--
_InvalidNextToken :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextToken = _MatchServiceError ssm "InvalidNextToken"


-- | The request is not valid.
--
--
_InvalidInventoryRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInventoryRequestException =
  _MatchServiceError ssm "InvalidInventoryRequestException"


-- | The S3 bucket does not exist.
--
--
_InvalidOutputFolder :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidOutputFolder = _MatchServiceError ssm "InvalidOutputFolder"


-- | The activation ID is not valid. Verify the you entered the correct ActivationId or ActivationCode and try again.
--
--
_InvalidActivationId :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidActivationId = _MatchServiceError ssm "InvalidActivationId"


-- | The specified inventory item result attribute is not valid.
--
--
_InvalidResultAttributeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidResultAttributeException =
  _MatchServiceError ssm "InvalidResultAttributeException"


-- | Error returned when the caller has exceeded the default resource limits. For example, too many Maintenance Windows or Patch baselines have been created.
--
--
-- For information about resource limits in Systems Manager, see <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_ssm AWS Systems Manager Limits> .
--
_ResourceLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceLimitExceededException =
  _MatchServiceError ssm "ResourceLimitExceededException"


-- | The specified sync configuration is invalid.
--
--
_ResourceDataSyncInvalidConfigurationException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceDataSyncInvalidConfigurationException =
  _MatchServiceError ssm "ResourceDataSyncInvalidConfigurationException"


-- | Prism for InvalidCommandId' errors.
_InvalidCommandId :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCommandId = _MatchServiceError ssm "InvalidCommandId"


-- | You cannot specify an instance ID in more than one association.
--
--
_DuplicateInstanceId :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateInstanceId = _MatchServiceError ssm "DuplicateInstanceId"


-- | The resource type is not valid. For example, if you are attempting to tag an instance, the instance must be a registered, managed instance.
--
--
_InvalidResourceType :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidResourceType = _MatchServiceError ssm "InvalidResourceType"


-- | Inventory item type schema version has to match supported versions in the service. Check output of GetInventorySchema to see the available schema version for each type.
--
--
_UnsupportedInventorySchemaVersionException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedInventorySchemaVersionException =
  _MatchServiceError ssm "UnsupportedInventorySchemaVersionException"


-- | The specified document does not exist.
--
--
_InvalidDocument :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDocument = _MatchServiceError ssm "InvalidDocument"


-- | An Automation document with the specified name could not be found.
--
--
_AutomationDefinitionNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_AutomationDefinitionNotFoundException =
  _MatchServiceError ssm "AutomationDefinitionNotFoundException"


-- | The specified key is not valid.
--
--
_InvalidFilterKey :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidFilterKey = _MatchServiceError ssm "InvalidFilterKey"


-- | The supplied parameters for invoking the specified Automation document are incorrect. For example, they may not match the set of parameters permitted for the specified Automation document.
--
--
_InvalidAutomationExecutionParametersException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidAutomationExecutionParametersException =
  _MatchServiceError ssm "InvalidAutomationExecutionParametersException"


-- | There is no automation execution information for the requested automation execution ID.
--
--
_AutomationExecutionNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_AutomationExecutionNotFoundException =
  _MatchServiceError ssm "AutomationExecutionNotFoundException"


-- | The parameter type name is not valid.
--
--
_InvalidTypeNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTypeNameException = _MatchServiceError ssm "InvalidTypeNameException"


-- | The specified sync name was not found.
--
--
_ResourceDataSyncNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceDataSyncNotFoundException =
  _MatchServiceError ssm "ResourceDataSyncNotFoundException"


-- | The parameter exceeded the maximum number of allowed versions.
--
--
_ParameterMaxVersionLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_ParameterMaxVersionLimitExceeded =
  _MatchServiceError ssm "ParameterMaxVersionLimitExceeded"


-- | The inventory item size has exceeded the size limit.
--
--
_ItemSizeLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ItemSizeLimitExceededException =
  _MatchServiceError ssm "ItemSizeLimitExceededException"


-- | A sync configuration with the same name already exists.
--
--
_ResourceDataSyncAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceDataSyncAlreadyExistsException =
  _MatchServiceError ssm "ResourceDataSyncAlreadyExistsException"


-- | Error returned when the ID specified for a resource, such as a Maintenance Window or Patch baseline, doesn't exist.
--
--
-- For information about resource limits in Systems Manager, see <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_ssm AWS Systems Manager Limits> .
--
_DoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_DoesNotExistException = _MatchServiceError ssm "DoesNotExistException"


-- | The number of simultaneously running Automation executions exceeded the allowable limit.
--
--
_AutomationExecutionLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_AutomationExecutionLimitExceededException =
  _MatchServiceError ssm "AutomationExecutionLimitExceededException"


-- | Error returned when an idempotent operation is retried and the parameters don't match the original call to the API with the same idempotency token.
--
--
_IdempotentParameterMismatch :: AsError a => Getting (First ServiceError) a ServiceError
_IdempotentParameterMismatch =
  _MatchServiceError ssm "IdempotentParameterMismatch"


-- | The specified filter value is not valid.
--
--
_InvalidInstanceInformationFilterValue :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInstanceInformationFilterValue =
  _MatchServiceError ssm "InvalidInstanceInformationFilterValue"


-- | The inventory item has invalid content.
--
--
_ItemContentMismatchException :: AsError a => Getting (First ServiceError) a ServiceError
_ItemContentMismatchException =
  _MatchServiceError ssm "ItemContentMismatchException"


-- | The parameter already exists. You can't create duplicate parameters.
--
--
_ParameterAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_ParameterAlreadyExists = _MatchServiceError ssm "ParameterAlreadyExists"


-- | The specified association already exists.
--
--
_AssociationAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_AssociationAlreadyExists = _MatchServiceError ssm "AssociationAlreadyExists"


-- | You specified too many custom compliance types. You can specify a maximum of 10 different types.
--
--
_ComplianceTypeCountLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ComplianceTypeCountLimitExceededException =
  _MatchServiceError ssm "ComplianceTypeCountLimitExceededException"


-- | One or more of the parameters specified for the delete operation is not valid. Verify all parameters and try again.
--
--
_InvalidDeleteInventoryParametersException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDeleteInventoryParametersException =
  _MatchServiceError ssm "InvalidDeleteInventoryParametersException"


-- | The ID specified for the delete operation does not exist or is not valide. Verify the ID and try again.
--
--
_InvalidDeletionIdException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDeletionIdException =
  _MatchServiceError ssm "InvalidDeletionIdException"


-- | The content for the document is not valid.
--
--
_InvalidDocumentContent :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDocumentContent = _MatchServiceError ssm "InvalidDocumentContent"


-- | You have exceeded the number of parameters for this AWS account. Delete one or more parameters and try again.
--
--
_ParameterLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_ParameterLimitExceeded = _MatchServiceError ssm "ParameterLimitExceeded"


-- | You can have at most 2,000 active associations.
--
--
_AssociationLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_AssociationLimitExceeded = _MatchServiceError ssm "AssociationLimitExceeded"


-- | The version you specified is not valid. Use ListAssociationVersions to view all versions of an association according to the association ID. Or, use the @> LATEST@ parameter to view the latest version of the association.
--
--
_InvalidAssociationVersion :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidAssociationVersion = _MatchServiceError ssm "InvalidAssociationVersion"


-- | The specified association does not exist.
--
--
_AssociationDoesNotExist :: AsError a => Getting (First ServiceError) a ServiceError
_AssociationDoesNotExist = _MatchServiceError ssm "AssociationDoesNotExist"


-- | The parameter could not be found. Verify the name and try again.
--
--
_ParameterNotFound :: AsError a => Getting (First ServiceError) a ServiceError
_ParameterNotFound = _MatchServiceError ssm "ParameterNotFound"


-- | You specified the @Safe@ option for the DeregisterTargetFromMaintenanceWindow operation, but the target is still referenced in a task.
--
--
_TargetInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_TargetInUseException = _MatchServiceError ssm "TargetInUseException"


-- | An error occurred on the server side.
--
--
_InternalServerError :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerError = _MatchServiceError ssm "InternalServerError"


-- | The @Context@ attribute that you specified for the @InventoryItem@ is not allowed for this inventory type. You can only use the @Context@ attribute with inventory types like @AWS:ComplianceItem@ .
--
--
_UnsupportedInventoryItemContextException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedInventoryItemContextException =
  _MatchServiceError ssm "UnsupportedInventoryItemContextException"


-- | You have reached the maximum number versions allowed for an association. Each association has a limit of 1,000 versions.
--
--
_AssociationVersionLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_AssociationVersionLimitExceeded =
  _MatchServiceError ssm "AssociationVersionLimitExceeded"


-- | The role name can't contain invalid characters. Also verify that you specified an IAM role for notifications that includes the required trust policy. For information about configuring the IAM role for Run Command notifications, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/rc-sns-notifications.html Configuring Amazon SNS Notifications for Run Command> in the /AWS Systems Manager User Guide/ .
--
--
_InvalidRole :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRole = _MatchServiceError ssm "InvalidRole"


-- | There are concurrent updates for a resource that supports one update at a time.
--
--
_TooManyUpdates :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyUpdates = _MatchServiceError ssm "TooManyUpdates"


-- | The activation is not valid. The activation might have been deleted, or the ActivationId and the ActivationCode do not match.
--
--
_InvalidActivation :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidActivation = _MatchServiceError ssm "InvalidActivation"


-- | The delete inventory option specified is not valid. Verify the option and try again.
--
--
_InvalidOptionException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidOptionException = _MatchServiceError ssm "InvalidOptionException"


-- | The version of the document schema is not supported.
--
--
_InvalidDocumentSchemaVersion :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDocumentSchemaVersion =
  _MatchServiceError ssm "InvalidDocumentSchemaVersion"


-- | The size limit of a document is 64 KB.
--
--
_MaxDocumentSizeExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_MaxDocumentSizeExceeded = _MatchServiceError ssm "MaxDocumentSizeExceeded"


-- | The specified parameter version was not found. Verify the parameter name and version, and try again.
--
--
_ParameterVersionNotFound :: AsError a => Getting (First ServiceError) a ServiceError
_ParameterVersionNotFound = _MatchServiceError ssm "ParameterVersionNotFound"


-- | The update is not valid.
--
--
_InvalidUpdate :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidUpdate = _MatchServiceError ssm "InvalidUpdate"


-- | You have exceeded the limit for custom schemas. Delete one or more custom schemas and try again.
--
--
_CustomSchemaCountLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_CustomSchemaCountLimitExceededException =
  _MatchServiceError ssm "CustomSchemaCountLimitExceededException"


-- | The target is not valid or does not exist. It might not be configured for EC2 Systems Manager or you might not have permission to perform the operation.
--
--
_InvalidTarget :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTarget = _MatchServiceError ssm "InvalidTarget"


-- | A hierarchy can have a maximum of 15 levels. For more information, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-working.html Working with Systems Manager Parameters> .
--
--
_HierarchyLevelLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_HierarchyLevelLimitExceededException =
  _MatchServiceError ssm "HierarchyLevelLimitExceededException"


-- | You attempted to delete a document while it is still shared. You must stop sharing the document before you can delete it.
--
--
_InvalidDocumentOperation :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDocumentOperation = _MatchServiceError ssm "InvalidDocumentOperation"


-- | The command ID and instance ID you specified did not match any invocations. Verify the command ID adn the instance ID and try again.
--
--
_InvocationDoesNotExist :: AsError a => Getting (First ServiceError) a ServiceError
_InvocationDoesNotExist = _MatchServiceError ssm "InvocationDoesNotExist"


-- | The document has too many versions. Delete one or more document versions and try again.
--
--
_DocumentVersionLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_DocumentVersionLimitExceeded =
  _MatchServiceError ssm "DocumentVersionLimitExceeded"


-- | The output location is not valid or does not exist.
--
--
_InvalidOutputLocation :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidOutputLocation = _MatchServiceError ssm "InvalidOutputLocation"


-- | The query key ID is not valid.
--
--
_InvalidKeyId :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidKeyId = _MatchServiceError ssm "InvalidKeyId"


-- | You must specify values for all required parameters in the Systems Manager document. You can only supply values to parameters defined in the Systems Manager document.
--
--
_InvalidParameters :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameters = _MatchServiceError ssm "InvalidParameters"


-- | The resource ID is not valid. Verify that you entered the correct ID and try again.
--
--
_InvalidResourceId :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidResourceId = _MatchServiceError ssm "InvalidResourceId"


-- | The request does not meet the regular expression requirement.
--
--
_InvalidAllowedPatternException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidAllowedPatternException =
  _MatchServiceError ssm "InvalidAllowedPatternException"


-- | One or more configuration items is not valid. Verify that a valid Amazon Resource Name (ARN) was provided for an Amazon SNS topic.
--
--
_InvalidNotificationConfig :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNotificationConfig = _MatchServiceError ssm "InvalidNotificationConfig"


-- | You specified invalid keys or values in the @Context@ attribute for @InventoryItem@ . Verify the keys and values, and try again.
--
--
_InvalidInventoryItemContextException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInventoryItemContextException =
  _MatchServiceError ssm "InvalidInventoryItemContextException"


-- | The size of inventory data has exceeded the total size limit for the resource.
--
--
_TotalSizeLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_TotalSizeLimitExceededException =
  _MatchServiceError ssm "TotalSizeLimitExceededException"


-- | The sub-type count exceeded the limit for the inventory type.
--
--
_SubTypeCountLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_SubTypeCountLimitExceededException =
  _MatchServiceError ssm "SubTypeCountLimitExceededException"


-- | The Targets parameter includes too many tags. Remove one or more tags and try the command again.
--
--
_TooManyTagsError :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyTagsError = _MatchServiceError ssm "TooManyTagsError"


-- | The document cannot be shared with more AWS user accounts. You can share a document with a maximum of 20 accounts. You can publicly share up to five documents. If you need to increase this limit, contact AWS Support.
--
--
_DocumentPermissionLimit :: AsError a => Getting (First ServiceError) a ServiceError
_DocumentPermissionLimit = _MatchServiceError ssm "DocumentPermissionLimit"


-- | The specified step name and execution ID don't exist. Verify the information and try again.
--
--
_AutomationStepNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_AutomationStepNotFoundException =
  _MatchServiceError ssm "AutomationStepNotFoundException"


-- | The content of the association document matches another document. Change the content of the document and try again.
--
--
_DuplicateDocumentContent :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateDocumentContent = _MatchServiceError ssm "DuplicateDocumentContent"


-- | The specified document already exists.
--
--
_DocumentAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_DocumentAlreadyExists = _MatchServiceError ssm "DocumentAlreadyExists"


-- | You can have at most 200 active Systems Manager documents.
--
--
_DocumentLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_DocumentLimitExceeded = _MatchServiceError ssm "DocumentLimitExceeded"


-- | Error returned if an attempt is made to register a patch group with a patch baseline that is already registered with a different patch baseline.
--
--
_AlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_AlreadyExistsException = _MatchServiceError ssm "AlreadyExistsException"


-- | The filter name is not valid. Verify the you entered the correct name and try again.
--
--
_InvalidFilter :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidFilter = _MatchServiceError ssm "InvalidFilter"


-- | Error returned if an attempt is made to delete a patch baseline that is registered for a patch group.
--
--
_ResourceInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException = _MatchServiceError ssm "ResourceInUseException"

