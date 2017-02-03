{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon EC2 Systems Manager is a collection of capabilities that helps you automate management tasks such as collecting system inventory, applying operating system (OS) patches, automating the creation of Amazon Machine Images (AMIs), and configuring operating systems (OSs) and applications at scale. Systems Manager works with managed instances: Amazon EC2 instances and servers or virtual machines (VMs) in your on-premises environment that are configured for Systems Manager.
--
--
-- This references is intended to be used with the EC2 Systems Manager User Guide (<http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/systems-manager.html Linux> ) (<http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/systems-manager.html Windows> ).
--
-- To get started, verify prerequisites and configure managed instances (<http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/systems-manager-prereqs.html Linux> ) (<http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/systems-manager-prereqs.html Windows> ).
--
module Network.AWS.SSM
    (
    -- * Service Configuration
      ssm

    -- * Errors
    -- $errors

    -- ** AutomationDefinitionVersionNotFoundException
    , _AutomationDefinitionVersionNotFoundException

    -- ** InvalidDocumentVersion
    , _InvalidDocumentVersion

    -- ** InvalidSchedule
    , _InvalidSchedule

    -- ** UnsupportedParameterType
    , _UnsupportedParameterType

    -- ** InvalidPluginName
    , _InvalidPluginName

    -- ** UnsupportedPlatformType
    , _UnsupportedPlatformType

    -- ** InvalidFilterValue
    , _InvalidFilterValue

    -- ** InvalidItemContentException
    , _InvalidItemContentException

    -- ** InvalidPermissionType
    , _InvalidPermissionType

    -- ** AssociatedInstances
    , _AssociatedInstances

    -- ** InvalidInstanceId
    , _InvalidInstanceId

    -- ** StatusUnchanged
    , _StatusUnchanged

    -- ** InvalidNextToken
    , _InvalidNextToken

    -- ** InvalidOutputFolder
    , _InvalidOutputFolder

    -- ** InvalidActivationId
    , _InvalidActivationId

    -- ** InvalidResultAttributeException
    , _InvalidResultAttributeException

    -- ** ResourceLimitExceededException
    , _ResourceLimitExceededException

    -- ** InvalidCommandId
    , _InvalidCommandId

    -- ** DuplicateInstanceId
    , _DuplicateInstanceId

    -- ** InvalidResourceType
    , _InvalidResourceType

    -- ** UnsupportedInventorySchemaVersionException
    , _UnsupportedInventorySchemaVersionException

    -- ** InvalidDocument
    , _InvalidDocument

    -- ** AutomationDefinitionNotFoundException
    , _AutomationDefinitionNotFoundException

    -- ** InvalidFilterKey
    , _InvalidFilterKey

    -- ** InvalidAutomationExecutionParametersException
    , _InvalidAutomationExecutionParametersException

    -- ** AutomationExecutionNotFoundException
    , _AutomationExecutionNotFoundException

    -- ** InvalidTypeNameException
    , _InvalidTypeNameException

    -- ** ItemSizeLimitExceededException
    , _ItemSizeLimitExceededException

    -- ** DoesNotExistException
    , _DoesNotExistException

    -- ** AutomationExecutionLimitExceededException
    , _AutomationExecutionLimitExceededException

    -- ** IdempotentParameterMismatch
    , _IdempotentParameterMismatch

    -- ** InvalidInstanceInformationFilterValue
    , _InvalidInstanceInformationFilterValue

    -- ** ItemContentMismatchException
    , _ItemContentMismatchException

    -- ** ParameterAlreadyExists
    , _ParameterAlreadyExists

    -- ** AssociationAlreadyExists
    , _AssociationAlreadyExists

    -- ** InvalidDocumentContent
    , _InvalidDocumentContent

    -- ** ParameterLimitExceeded
    , _ParameterLimitExceeded

    -- ** AssociationLimitExceeded
    , _AssociationLimitExceeded

    -- ** AssociationDoesNotExist
    , _AssociationDoesNotExist

    -- ** ParameterNotFound
    , _ParameterNotFound

    -- ** InternalServerError
    , _InternalServerError

    -- ** InvalidRole
    , _InvalidRole

    -- ** TooManyUpdates
    , _TooManyUpdates

    -- ** InvalidActivation
    , _InvalidActivation

    -- ** InvalidDocumentSchemaVersion
    , _InvalidDocumentSchemaVersion

    -- ** MaxDocumentSizeExceeded
    , _MaxDocumentSizeExceeded

    -- ** InvalidUpdate
    , _InvalidUpdate

    -- ** CustomSchemaCountLimitExceededException
    , _CustomSchemaCountLimitExceededException

    -- ** InvalidTarget
    , _InvalidTarget

    -- ** InvalidDocumentOperation
    , _InvalidDocumentOperation

    -- ** InvocationDoesNotExist
    , _InvocationDoesNotExist

    -- ** DocumentVersionLimitExceeded
    , _DocumentVersionLimitExceeded

    -- ** InvalidOutputLocation
    , _InvalidOutputLocation

    -- ** InvalidKeyId
    , _InvalidKeyId

    -- ** InvalidParameters
    , _InvalidParameters

    -- ** InvalidResourceId
    , _InvalidResourceId

    -- ** InvalidNotificationConfig
    , _InvalidNotificationConfig

    -- ** TotalSizeLimitExceededException
    , _TotalSizeLimitExceededException

    -- ** TooManyTagsError
    , _TooManyTagsError

    -- ** DocumentPermissionLimit
    , _DocumentPermissionLimit

    -- ** DuplicateDocumentContent
    , _DuplicateDocumentContent

    -- ** DocumentAlreadyExists
    , _DocumentAlreadyExists

    -- ** DocumentLimitExceeded
    , _DocumentLimitExceeded

    -- ** AlreadyExistsException
    , _AlreadyExistsException

    -- ** InvalidFilter
    , _InvalidFilter

    -- ** ResourceInUseException
    , _ResourceInUseException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeInstancePatches
    , module Network.AWS.SSM.DescribeInstancePatches

    -- ** GetInventory
    , module Network.AWS.SSM.GetInventory

    -- ** GetParameters
    , module Network.AWS.SSM.GetParameters

    -- ** DeletePatchBaseline
    , module Network.AWS.SSM.DeletePatchBaseline

    -- ** UpdatePatchBaseline
    , module Network.AWS.SSM.UpdatePatchBaseline

    -- ** UpdateDocumentDefaultVersion
    , module Network.AWS.SSM.UpdateDocumentDefaultVersion

    -- ** GetDeployablePatchSnapshotForInstance
    , module Network.AWS.SSM.GetDeployablePatchSnapshotForInstance

    -- ** DescribeParameters
    , module Network.AWS.SSM.DescribeParameters

    -- ** DescribeActivations (Paginated)
    , module Network.AWS.SSM.DescribeActivations

    -- ** ListTagsForResource
    , module Network.AWS.SSM.ListTagsForResource

    -- ** DescribeDocument
    , module Network.AWS.SSM.DescribeDocument

    -- ** CreateAssociation
    , module Network.AWS.SSM.CreateAssociation

    -- ** DeleteActivation
    , module Network.AWS.SSM.DeleteActivation

    -- ** DescribeMaintenanceWindowExecutions
    , module Network.AWS.SSM.DescribeMaintenanceWindowExecutions

    -- ** GetInventorySchema
    , module Network.AWS.SSM.GetInventorySchema

    -- ** StartAutomationExecution
    , module Network.AWS.SSM.StartAutomationExecution

    -- ** CreateActivation
    , module Network.AWS.SSM.CreateActivation

    -- ** DeleteMaintenanceWindow
    , module Network.AWS.SSM.DeleteMaintenanceWindow

    -- ** UpdateMaintenanceWindow
    , module Network.AWS.SSM.UpdateMaintenanceWindow

    -- ** DescribeMaintenanceWindowExecutionTasks
    , module Network.AWS.SSM.DescribeMaintenanceWindowExecutionTasks

    -- ** GetDefaultPatchBaseline
    , module Network.AWS.SSM.GetDefaultPatchBaseline

    -- ** GetMaintenanceWindowExecutionTask
    , module Network.AWS.SSM.GetMaintenanceWindowExecutionTask

    -- ** CreateDocument
    , module Network.AWS.SSM.CreateDocument

    -- ** RemoveTagsFromResource
    , module Network.AWS.SSM.RemoveTagsFromResource

    -- ** DescribePatchGroupState
    , module Network.AWS.SSM.DescribePatchGroupState

    -- ** ListCommandInvocations (Paginated)
    , module Network.AWS.SSM.ListCommandInvocations

    -- ** DeregisterTargetFromMaintenanceWindow
    , module Network.AWS.SSM.DeregisterTargetFromMaintenanceWindow

    -- ** DescribeEffectivePatchesForPatchBaseline
    , module Network.AWS.SSM.DescribeEffectivePatchesForPatchBaseline

    -- ** DescribeMaintenanceWindowTargets
    , module Network.AWS.SSM.DescribeMaintenanceWindowTargets

    -- ** RegisterPatchBaselineForPatchGroup
    , module Network.AWS.SSM.RegisterPatchBaselineForPatchGroup

    -- ** ListDocuments (Paginated)
    , module Network.AWS.SSM.ListDocuments

    -- ** DescribeInstancePatchStates
    , module Network.AWS.SSM.DescribeInstancePatchStates

    -- ** GetPatchBaselineForPatchGroup
    , module Network.AWS.SSM.GetPatchBaselineForPatchGroup

    -- ** UpdateManagedInstanceRole
    , module Network.AWS.SSM.UpdateManagedInstanceRole

    -- ** GetDocument
    , module Network.AWS.SSM.GetDocument

    -- ** AddTagsToResource
    , module Network.AWS.SSM.AddTagsToResource

    -- ** CancelCommand
    , module Network.AWS.SSM.CancelCommand

    -- ** GetCommandInvocation
    , module Network.AWS.SSM.GetCommandInvocation

    -- ** DescribeInstancePatchStatesForPatchGroup
    , module Network.AWS.SSM.DescribeInstancePatchStatesForPatchGroup

    -- ** DeregisterManagedInstance
    , module Network.AWS.SSM.DeregisterManagedInstance

    -- ** DescribeAssociation
    , module Network.AWS.SSM.DescribeAssociation

    -- ** ModifyDocumentPermission
    , module Network.AWS.SSM.ModifyDocumentPermission

    -- ** UpdateAssociationStatus
    , module Network.AWS.SSM.UpdateAssociationStatus

    -- ** DescribeAvailablePatches
    , module Network.AWS.SSM.DescribeAvailablePatches

    -- ** ListDocumentVersions
    , module Network.AWS.SSM.ListDocumentVersions

    -- ** DeregisterPatchBaselineForPatchGroup
    , module Network.AWS.SSM.DeregisterPatchBaselineForPatchGroup

    -- ** DescribePatchGroups
    , module Network.AWS.SSM.DescribePatchGroups

    -- ** GetMaintenanceWindow
    , module Network.AWS.SSM.GetMaintenanceWindow

    -- ** DescribeMaintenanceWindows
    , module Network.AWS.SSM.DescribeMaintenanceWindows

    -- ** RegisterTaskWithMaintenanceWindow
    , module Network.AWS.SSM.RegisterTaskWithMaintenanceWindow

    -- ** RegisterDefaultPatchBaseline
    , module Network.AWS.SSM.RegisterDefaultPatchBaseline

    -- ** DescribeMaintenanceWindowTasks
    , module Network.AWS.SSM.DescribeMaintenanceWindowTasks

    -- ** DescribeInstanceAssociationsStatus
    , module Network.AWS.SSM.DescribeInstanceAssociationsStatus

    -- ** DeregisterTaskFromMaintenanceWindow
    , module Network.AWS.SSM.DeregisterTaskFromMaintenanceWindow

    -- ** ListInventoryEntries
    , module Network.AWS.SSM.ListInventoryEntries

    -- ** GetParameterHistory
    , module Network.AWS.SSM.GetParameterHistory

    -- ** CreateMaintenanceWindow
    , module Network.AWS.SSM.CreateMaintenanceWindow

    -- ** StopAutomationExecution
    , module Network.AWS.SSM.StopAutomationExecution

    -- ** GetMaintenanceWindowExecution
    , module Network.AWS.SSM.GetMaintenanceWindowExecution

    -- ** PutParameter
    , module Network.AWS.SSM.PutParameter

    -- ** DescribeMaintenanceWindowExecutionTaskInvocations
    , module Network.AWS.SSM.DescribeMaintenanceWindowExecutionTaskInvocations

    -- ** DeleteParameter
    , module Network.AWS.SSM.DeleteParameter

    -- ** DescribeInstanceInformation (Paginated)
    , module Network.AWS.SSM.DescribeInstanceInformation

    -- ** ListAssociations (Paginated)
    , module Network.AWS.SSM.ListAssociations

    -- ** DeleteAssociation
    , module Network.AWS.SSM.DeleteAssociation

    -- ** UpdateAssociation
    , module Network.AWS.SSM.UpdateAssociation

    -- ** PutInventory
    , module Network.AWS.SSM.PutInventory

    -- ** DescribeEffectiveInstanceAssociations
    , module Network.AWS.SSM.DescribeEffectiveInstanceAssociations

    -- ** DescribeAutomationExecutions
    , module Network.AWS.SSM.DescribeAutomationExecutions

    -- ** GetAutomationExecution
    , module Network.AWS.SSM.GetAutomationExecution

    -- ** SendCommand
    , module Network.AWS.SSM.SendCommand

    -- ** DescribePatchBaselines
    , module Network.AWS.SSM.DescribePatchBaselines

    -- ** GetPatchBaseline
    , module Network.AWS.SSM.GetPatchBaseline

    -- ** RegisterTargetWithMaintenanceWindow
    , module Network.AWS.SSM.RegisterTargetWithMaintenanceWindow

    -- ** ListCommands (Paginated)
    , module Network.AWS.SSM.ListCommands

    -- ** UpdateDocument
    , module Network.AWS.SSM.UpdateDocument

    -- ** DeleteDocument
    , module Network.AWS.SSM.DeleteDocument

    -- ** DescribeDocumentPermission
    , module Network.AWS.SSM.DescribeDocumentPermission

    -- ** CreateAssociationBatch
    , module Network.AWS.SSM.CreateAssociationBatch

    -- ** CreatePatchBaseline
    , module Network.AWS.SSM.CreatePatchBaseline

    -- * Types

    -- ** AssociationFilterKey
    , AssociationFilterKey (..)

    -- ** AssociationStatusName
    , AssociationStatusName (..)

    -- ** AutomationExecutionFilterKey
    , AutomationExecutionFilterKey (..)

    -- ** AutomationExecutionStatus
    , AutomationExecutionStatus (..)

    -- ** CommandFilterKey
    , CommandFilterKey (..)

    -- ** CommandInvocationStatus
    , CommandInvocationStatus (..)

    -- ** CommandPluginStatus
    , CommandPluginStatus (..)

    -- ** CommandStatus
    , CommandStatus (..)

    -- ** DescribeActivationsFilterKeys
    , DescribeActivationsFilterKeys (..)

    -- ** DocumentFilterKey
    , DocumentFilterKey (..)

    -- ** DocumentHashType
    , DocumentHashType (..)

    -- ** DocumentParameterType
    , DocumentParameterType (..)

    -- ** DocumentPermissionType
    , DocumentPermissionType (..)

    -- ** DocumentStatus
    , DocumentStatus (..)

    -- ** DocumentType
    , DocumentType (..)

    -- ** Fault
    , Fault (..)

    -- ** InstanceInformationFilterKey
    , InstanceInformationFilterKey (..)

    -- ** InstancePatchStateOperatorType
    , InstancePatchStateOperatorType (..)

    -- ** InventoryAttributeDataType
    , InventoryAttributeDataType (..)

    -- ** InventoryQueryOperatorType
    , InventoryQueryOperatorType (..)

    -- ** MaintenanceWindowExecutionStatus
    , MaintenanceWindowExecutionStatus (..)

    -- ** MaintenanceWindowResourceType
    , MaintenanceWindowResourceType (..)

    -- ** MaintenanceWindowTaskType
    , MaintenanceWindowTaskType (..)

    -- ** NotificationEvent
    , NotificationEvent (..)

    -- ** NotificationType
    , NotificationType (..)

    -- ** ParameterType
    , ParameterType (..)

    -- ** ParametersFilterKey
    , ParametersFilterKey (..)

    -- ** PatchComplianceDataState
    , PatchComplianceDataState (..)

    -- ** PatchDeploymentStatus
    , PatchDeploymentStatus (..)

    -- ** PatchFilterKey
    , PatchFilterKey (..)

    -- ** PatchOperationType
    , PatchOperationType (..)

    -- ** PingStatus
    , PingStatus (..)

    -- ** PlatformType
    , PlatformType (..)

    -- ** ResourceType
    , ResourceType (..)

    -- ** ResourceTypeForTagging
    , ResourceTypeForTagging (..)

    -- ** Activation
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

    -- ** Association
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

    -- ** AssociationDescription
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

    -- ** AssociationFilter
    , AssociationFilter
    , associationFilter
    , afKey
    , afValue

    -- ** AssociationOverview
    , AssociationOverview
    , associationOverview
    , aoDetailedStatus
    , aoStatus
    , aoAssociationStatusAggregatedCount

    -- ** AssociationStatus
    , AssociationStatus
    , associationStatus
    , asAdditionalInfo
    , asDate
    , asName
    , asMessage

    -- ** AutomationExecution
    , AutomationExecution
    , automationExecution
    , aeDocumentName
    , aeExecutionEndTime
    , aeFailureMessage
    , aeAutomationExecutionStatus
    , aeOutputs
    , aeExecutionStartTime
    , aeParameters
    , aeDocumentVersion
    , aeAutomationExecutionId
    , aeStepExecutions

    -- ** AutomationExecutionFilter
    , AutomationExecutionFilter
    , automationExecutionFilter
    , aefKey
    , aefValues

    -- ** AutomationExecutionMetadata
    , AutomationExecutionMetadata
    , automationExecutionMetadata
    , aemLogFile
    , aemExecutedBy
    , aemDocumentName
    , aemExecutionEndTime
    , aemAutomationExecutionStatus
    , aemOutputs
    , aemExecutionStartTime
    , aemDocumentVersion
    , aemAutomationExecutionId

    -- ** Command
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
    , cComment
    , cCompletedCount
    , cOutputS3BucketName
    , cMaxConcurrency
    , cRequestedDateTime
    , cServiceRole

    -- ** CommandFilter
    , CommandFilter
    , commandFilter
    , cfKey
    , cfValue

    -- ** CommandInvocation
    , CommandInvocation
    , commandInvocation
    , ciInstanceId
    , ciStatus
    , ciNotificationConfig
    , ciCommandPlugins
    , ciDocumentName
    , ciStandardErrorURL
    , ciStatusDetails
    , ciStandardOutputURL
    , ciCommandId
    , ciComment
    , ciTraceOutput
    , ciInstanceName
    , ciRequestedDateTime
    , ciServiceRole

    -- ** CommandPlugin
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

    -- ** CreateAssociationBatchRequestEntry
    , CreateAssociationBatchRequestEntry
    , createAssociationBatchRequestEntry
    , cabreInstanceId
    , cabreScheduleExpression
    , cabreOutputLocation
    , cabreTargets
    , cabreParameters
    , cabreDocumentVersion
    , cabreName

    -- ** DescribeActivationsFilter
    , DescribeActivationsFilter
    , describeActivationsFilter
    , dafFilterKey
    , dafFilterValues

    -- ** DocumentDefaultVersionDescription
    , DocumentDefaultVersionDescription
    , documentDefaultVersionDescription
    , ddvdDefaultVersion
    , ddvdName

    -- ** DocumentDescription
    , DocumentDescription
    , documentDescription
    , dStatus
    , dDocumentType
    , dHash
    , dSchemaVersion
    , dSha1
    , dDefaultVersion
    , dOwner
    , dPlatformTypes
    , dCreatedDate
    , dName
    , dHashType
    , dParameters
    , dDocumentVersion
    , dDescription
    , dLatestVersion

    -- ** DocumentFilter
    , DocumentFilter
    , documentFilter
    , dfKey
    , dfValue

    -- ** DocumentIdentifier
    , DocumentIdentifier
    , documentIdentifier
    , diDocumentType
    , diSchemaVersion
    , diOwner
    , diPlatformTypes
    , diName
    , diDocumentVersion

    -- ** DocumentParameter
    , DocumentParameter
    , documentParameter
    , dpName
    , dpDefaultValue
    , dpType
    , dpDescription

    -- ** DocumentVersionInfo
    , DocumentVersionInfo
    , documentVersionInfo
    , dviCreatedDate
    , dviName
    , dviDocumentVersion
    , dviIsDefaultVersion

    -- ** EffectivePatch
    , EffectivePatch
    , effectivePatch
    , epPatch
    , epPatchStatus

    -- ** FailedCreateAssociation
    , FailedCreateAssociation
    , failedCreateAssociation
    , fcaEntry
    , fcaFault
    , fcaMessage

    -- ** InstanceAggregatedAssociationOverview
    , InstanceAggregatedAssociationOverview
    , instanceAggregatedAssociationOverview
    , iaaoDetailedStatus
    , iaaoInstanceAssociationStatusAggregatedCount

    -- ** InstanceAssociation
    , InstanceAssociation
    , instanceAssociation
    , iaAssociationId
    , iaInstanceId
    , iaContent

    -- ** InstanceAssociationOutputLocation
    , InstanceAssociationOutputLocation
    , instanceAssociationOutputLocation
    , iaolS3Location

    -- ** InstanceAssociationOutputURL
    , InstanceAssociationOutputURL
    , instanceAssociationOutputURL
    , iaouS3OutputURL

    -- ** InstanceAssociationStatusInfo
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
    , iasiExecutionDate

    -- ** InstanceInformation
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

    -- ** InstanceInformationFilter
    , InstanceInformationFilter
    , instanceInformationFilter
    , iifKey
    , iifValueSet

    -- ** InstanceInformationStringFilter
    , InstanceInformationStringFilter
    , instanceInformationStringFilter
    , iisfKey
    , iisfValues

    -- ** InstancePatchState
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

    -- ** InstancePatchStateFilter
    , InstancePatchStateFilter
    , instancePatchStateFilter
    , ipsfKey
    , ipsfValues
    , ipsfType

    -- ** InventoryFilter
    , InventoryFilter
    , inventoryFilter
    , ifType
    , ifKey
    , ifValues

    -- ** InventoryItem
    , InventoryItem
    , inventoryItem
    , iiContentHash
    , iiContent
    , iiTypeName
    , iiSchemaVersion
    , iiCaptureTime

    -- ** InventoryItemAttribute
    , InventoryItemAttribute
    , inventoryItemAttribute
    , iiaName
    , iiaDataType

    -- ** InventoryItemSchema
    , InventoryItemSchema
    , inventoryItemSchema
    , iisVersion
    , iisTypeName
    , iisAttributes

    -- ** InventoryResultEntity
    , InventoryResultEntity
    , inventoryResultEntity
    , ireData
    , ireId

    -- ** InventoryResultItem
    , InventoryResultItem
    , inventoryResultItem
    , iriContentHash
    , iriCaptureTime
    , iriTypeName
    , iriSchemaVersion
    , iriContent

    -- ** LoggingInfo
    , LoggingInfo
    , loggingInfo
    , liS3KeyPrefix
    , liS3BucketName
    , liS3Region

    -- ** MaintenanceWindowExecution
    , MaintenanceWindowExecution
    , maintenanceWindowExecution
    , mweStatus
    , mweStartTime
    , mweWindowExecutionId
    , mweStatusDetails
    , mweEndTime
    , mweWindowId

    -- ** MaintenanceWindowExecutionTaskIdentity
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

    -- ** MaintenanceWindowExecutionTaskInvocationIdentity
    , MaintenanceWindowExecutionTaskInvocationIdentity
    , maintenanceWindowExecutionTaskInvocationIdentity
    , mwetiiStatus
    , mwetiiExecutionId
    , mwetiiTaskExecutionId
    , mwetiiStartTime
    , mwetiiInvocationId
    , mwetiiOwnerInformation
    , mwetiiWindowTargetId
    , mwetiiWindowExecutionId
    , mwetiiStatusDetails
    , mwetiiEndTime
    , mwetiiParameters

    -- ** MaintenanceWindowFilter
    , MaintenanceWindowFilter
    , maintenanceWindowFilter
    , mwfValues
    , mwfKey

    -- ** MaintenanceWindowIdentity
    , MaintenanceWindowIdentity
    , maintenanceWindowIdentity
    , mwiEnabled
    , mwiName
    , mwiCutoff
    , mwiDuration
    , mwiWindowId

    -- ** MaintenanceWindowTarget
    , MaintenanceWindowTarget
    , maintenanceWindowTarget
    , mResourceType
    , mOwnerInformation
    , mWindowTargetId
    , mTargets
    , mWindowId

    -- ** MaintenanceWindowTask
    , MaintenanceWindowTask
    , maintenanceWindowTask
    , mwtServiceRoleARN
    , mwtWindowTaskId
    , mwtTaskParameters
    , mwtPriority
    , mwtTaskARN
    , mwtMaxErrors
    , mwtTargets
    , mwtLoggingInfo
    , mwtType
    , mwtMaxConcurrency
    , mwtWindowId

    -- ** MaintenanceWindowTaskParameterValueExpression
    , MaintenanceWindowTaskParameterValueExpression
    , maintenanceWindowTaskParameterValueExpression
    , mwtpveValues

    -- ** NotificationConfig
    , NotificationConfig
    , notificationConfig
    , ncNotificationEvents
    , ncNotificationType
    , ncNotificationARN

    -- ** Parameter
    , Parameter
    , parameter
    , pValue
    , pName
    , pType

    -- ** ParameterHistory
    , ParameterHistory
    , parameterHistory
    , phLastModifiedDate
    , phKeyId
    , phValue
    , phName
    , phLastModifiedUser
    , phType
    , phDescription

    -- ** ParameterMetadata
    , ParameterMetadata
    , parameterMetadata
    , pmLastModifiedDate
    , pmKeyId
    , pmName
    , pmLastModifiedUser
    , pmType
    , pmDescription

    -- ** ParametersFilter
    , ParametersFilter
    , parametersFilter
    , pKey
    , pValues

    -- ** Patch
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

    -- ** PatchBaselineIdentity
    , PatchBaselineIdentity
    , patchBaselineIdentity
    , pbiBaselineName
    , pbiBaselineDescription
    , pbiDefaultBaseline
    , pbiBaselineId

    -- ** PatchComplianceData
    , PatchComplianceData
    , patchComplianceData
    , pcdTitle
    , pcdKBId
    , pcdClassification
    , pcdSeverity
    , pcdState
    , pcdInstalledTime

    -- ** PatchFilter
    , PatchFilter
    , patchFilter
    , pfKey
    , pfValues

    -- ** PatchFilterGroup
    , PatchFilterGroup
    , patchFilterGroup
    , pfgPatchFilters

    -- ** PatchGroupPatchBaselineMapping
    , PatchGroupPatchBaselineMapping
    , patchGroupPatchBaselineMapping
    , pgpbmBaselineIdentity
    , pgpbmPatchGroup

    -- ** PatchOrchestratorFilter
    , PatchOrchestratorFilter
    , patchOrchestratorFilter
    , pofValues
    , pofKey

    -- ** PatchRule
    , PatchRule
    , patchRule
    , prPatchFilterGroup
    , prApproveAfterDays

    -- ** PatchRuleGroup
    , PatchRuleGroup
    , patchRuleGroup
    , prgPatchRules

    -- ** PatchStatus
    , PatchStatus
    , patchStatus
    , psApprovalDate
    , psDeploymentStatus

    -- ** ResultAttribute
    , ResultAttribute
    , resultAttribute
    , raTypeName

    -- ** S3OutputLocation
    , S3OutputLocation
    , s3OutputLocation
    , solOutputS3KeyPrefix
    , solOutputS3Region
    , solOutputS3BucketName

    -- ** S3OutputURL
    , S3OutputURL
    , s3OutputURL
    , souOutputURL

    -- ** StepExecution
    , StepExecution
    , stepExecution
    , seInputs
    , seStepName
    , seExecutionEndTime
    , seFailureMessage
    , seResponse
    , seAction
    , seResponseCode
    , seStepStatus
    , seOutputs
    , seExecutionStartTime

    -- ** Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- ** Target
    , Target
    , target
    , tValues
    , tKey
    ) where

import           Network.AWS.SSM.AddTagsToResource
import           Network.AWS.SSM.CancelCommand
import           Network.AWS.SSM.CreateActivation
import           Network.AWS.SSM.CreateAssociation
import           Network.AWS.SSM.CreateAssociationBatch
import           Network.AWS.SSM.CreateDocument
import           Network.AWS.SSM.CreateMaintenanceWindow
import           Network.AWS.SSM.CreatePatchBaseline
import           Network.AWS.SSM.DeleteActivation
import           Network.AWS.SSM.DeleteAssociation
import           Network.AWS.SSM.DeleteDocument
import           Network.AWS.SSM.DeleteMaintenanceWindow
import           Network.AWS.SSM.DeleteParameter
import           Network.AWS.SSM.DeletePatchBaseline
import           Network.AWS.SSM.DeregisterManagedInstance
import           Network.AWS.SSM.DeregisterPatchBaselineForPatchGroup
import           Network.AWS.SSM.DeregisterTargetFromMaintenanceWindow
import           Network.AWS.SSM.DeregisterTaskFromMaintenanceWindow
import           Network.AWS.SSM.DescribeActivations
import           Network.AWS.SSM.DescribeAssociation
import           Network.AWS.SSM.DescribeAutomationExecutions
import           Network.AWS.SSM.DescribeAvailablePatches
import           Network.AWS.SSM.DescribeDocument
import           Network.AWS.SSM.DescribeDocumentPermission
import           Network.AWS.SSM.DescribeEffectiveInstanceAssociations
import           Network.AWS.SSM.DescribeEffectivePatchesForPatchBaseline
import           Network.AWS.SSM.DescribeInstanceAssociationsStatus
import           Network.AWS.SSM.DescribeInstanceInformation
import           Network.AWS.SSM.DescribeInstancePatches
import           Network.AWS.SSM.DescribeInstancePatchStates
import           Network.AWS.SSM.DescribeInstancePatchStatesForPatchGroup
import           Network.AWS.SSM.DescribeMaintenanceWindowExecutions
import           Network.AWS.SSM.DescribeMaintenanceWindowExecutionTaskInvocations
import           Network.AWS.SSM.DescribeMaintenanceWindowExecutionTasks
import           Network.AWS.SSM.DescribeMaintenanceWindows
import           Network.AWS.SSM.DescribeMaintenanceWindowTargets
import           Network.AWS.SSM.DescribeMaintenanceWindowTasks
import           Network.AWS.SSM.DescribeParameters
import           Network.AWS.SSM.DescribePatchBaselines
import           Network.AWS.SSM.DescribePatchGroups
import           Network.AWS.SSM.DescribePatchGroupState
import           Network.AWS.SSM.GetAutomationExecution
import           Network.AWS.SSM.GetCommandInvocation
import           Network.AWS.SSM.GetDefaultPatchBaseline
import           Network.AWS.SSM.GetDeployablePatchSnapshotForInstance
import           Network.AWS.SSM.GetDocument
import           Network.AWS.SSM.GetInventory
import           Network.AWS.SSM.GetInventorySchema
import           Network.AWS.SSM.GetMaintenanceWindow
import           Network.AWS.SSM.GetMaintenanceWindowExecution
import           Network.AWS.SSM.GetMaintenanceWindowExecutionTask
import           Network.AWS.SSM.GetParameterHistory
import           Network.AWS.SSM.GetParameters
import           Network.AWS.SSM.GetPatchBaseline
import           Network.AWS.SSM.GetPatchBaselineForPatchGroup
import           Network.AWS.SSM.ListAssociations
import           Network.AWS.SSM.ListCommandInvocations
import           Network.AWS.SSM.ListCommands
import           Network.AWS.SSM.ListDocuments
import           Network.AWS.SSM.ListDocumentVersions
import           Network.AWS.SSM.ListInventoryEntries
import           Network.AWS.SSM.ListTagsForResource
import           Network.AWS.SSM.ModifyDocumentPermission
import           Network.AWS.SSM.PutInventory
import           Network.AWS.SSM.PutParameter
import           Network.AWS.SSM.RegisterDefaultPatchBaseline
import           Network.AWS.SSM.RegisterPatchBaselineForPatchGroup
import           Network.AWS.SSM.RegisterTargetWithMaintenanceWindow
import           Network.AWS.SSM.RegisterTaskWithMaintenanceWindow
import           Network.AWS.SSM.RemoveTagsFromResource
import           Network.AWS.SSM.SendCommand
import           Network.AWS.SSM.StartAutomationExecution
import           Network.AWS.SSM.StopAutomationExecution
import           Network.AWS.SSM.Types
import           Network.AWS.SSM.UpdateAssociation
import           Network.AWS.SSM.UpdateAssociationStatus
import           Network.AWS.SSM.UpdateDocument
import           Network.AWS.SSM.UpdateDocumentDefaultVersion
import           Network.AWS.SSM.UpdateMaintenanceWindow
import           Network.AWS.SSM.UpdateManagedInstanceRole
import           Network.AWS.SSM.UpdatePatchBaseline
import           Network.AWS.SSM.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'SSM'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
