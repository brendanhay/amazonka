{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SSM
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.SSM where

import Data.Proxy
import Network.AWS.SSM
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.SSM.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListResourceComplianceSummaries $
--             newListResourceComplianceSummaries
--
--         , requestDescribePatchGroups $
--             newDescribePatchGroups
--
--         , requestDescribeOpsItems $
--             newDescribeOpsItems
--
--         , requestTerminateSession $
--             newTerminateSession
--
--         , requestGetParameter $
--             newGetParameter
--
--         , requestGetOpsMetadata $
--             newGetOpsMetadata
--
--         , requestUpdateDocumentDefaultVersion $
--             newUpdateDocumentDefaultVersion
--
--         , requestListResourceDataSync $
--             newListResourceDataSync
--
--         , requestDescribeParameters $
--             newDescribeParameters
--
--         , requestRegisterTaskWithMaintenanceWindow $
--             newRegisterTaskWithMaintenanceWindow
--
--         , requestGetOpsItem $
--             newGetOpsItem
--
--         , requestGetInventory $
--             newGetInventory
--
--         , requestDescribeAssociation $
--             newDescribeAssociation
--
--         , requestUpdateAssociationStatus $
--             newUpdateAssociationStatus
--
--         , requestDeregisterManagedInstance $
--             newDeregisterManagedInstance
--
--         , requestDescribeAssociationExecutionTargets $
--             newDescribeAssociationExecutionTargets
--
--         , requestUpdateMaintenanceWindowTarget $
--             newUpdateMaintenanceWindowTarget
--
--         , requestDescribeAutomationStepExecutions $
--             newDescribeAutomationStepExecutions
--
--         , requestCreatePatchBaseline $
--             newCreatePatchBaseline
--
--         , requestGetCommandInvocation $
--             newGetCommandInvocation
--
--         , requestListCommands $
--             newListCommands
--
--         , requestDeregisterTargetFromMaintenanceWindow $
--             newDeregisterTargetFromMaintenanceWindow
--
--         , requestDescribeInstancePatchStates $
--             newDescribeInstancePatchStates
--
--         , requestUpdateDocument $
--             newUpdateDocument
--
--         , requestListDocuments $
--             newListDocuments
--
--         , requestResetServiceSetting $
--             newResetServiceSetting
--
--         , requestStartSession $
--             newStartSession
--
--         , requestGetOpsSummary $
--             newGetOpsSummary
--
--         , requestDeleteDocument $
--             newDeleteDocument
--
--         , requestDeleteInventory $
--             newDeleteInventory
--
--         , requestDeleteParameters $
--             newDeleteParameters
--
--         , requestListOpsMetadata $
--             newListOpsMetadata
--
--         , requestSendCommand $
--             newSendCommand
--
--         , requestRemoveTagsFromResource $
--             newRemoveTagsFromResource
--
--         , requestDescribeEffectiveInstanceAssociations $
--             newDescribeEffectiveInstanceAssociations
--
--         , requestCreateDocument $
--             newCreateDocument
--
--         , requestGetCalendarState $
--             newGetCalendarState
--
--         , requestRegisterTargetWithMaintenanceWindow $
--             newRegisterTargetWithMaintenanceWindow
--
--         , requestDescribeSessions $
--             newDescribeSessions
--
--         , requestDeleteOpsMetadata $
--             newDeleteOpsMetadata
--
--         , requestUpdateOpsItem $
--             newUpdateOpsItem
--
--         , requestDeleteAssociation $
--             newDeleteAssociation
--
--         , requestUpdateAssociation $
--             newUpdateAssociation
--
--         , requestUpdateOpsMetadata $
--             newUpdateOpsMetadata
--
--         , requestDescribeInstanceInformation $
--             newDescribeInstanceInformation
--
--         , requestDescribeMaintenanceWindowExecutionTaskInvocations $
--             newDescribeMaintenanceWindowExecutionTaskInvocations
--
--         , requestDeleteParameter $
--             newDeleteParameter
--
--         , requestGetMaintenanceWindowExecutionTaskInvocation $
--             newGetMaintenanceWindowExecutionTaskInvocation
--
--         , requestCreateActivation $
--             newCreateActivation
--
--         , requestCreateOpsMetadata $
--             newCreateOpsMetadata
--
--         , requestSendAutomationSignal $
--             newSendAutomationSignal
--
--         , requestListComplianceSummaries $
--             newListComplianceSummaries
--
--         , requestDescribeMaintenanceWindowExecutions $
--             newDescribeMaintenanceWindowExecutions
--
--         , requestDescribeMaintenanceWindowsForTarget $
--             newDescribeMaintenanceWindowsForTarget
--
--         , requestCreateAssociation $
--             newCreateAssociation
--
--         , requestCreateOpsItem $
--             newCreateOpsItem
--
--         , requestStartChangeRequestExecution $
--             newStartChangeRequestExecution
--
--         , requestListAssociationVersions $
--             newListAssociationVersions
--
--         , requestDescribeDocument $
--             newDescribeDocument
--
--         , requestDeregisterTaskFromMaintenanceWindow $
--             newDeregisterTaskFromMaintenanceWindow
--
--         , requestPutComplianceItems $
--             newPutComplianceItems
--
--         , requestGetMaintenanceWindowTask $
--             newGetMaintenanceWindowTask
--
--         , requestGetDeployablePatchSnapshotForInstance $
--             newGetDeployablePatchSnapshotForInstance
--
--         , requestGetMaintenanceWindow $
--             newGetMaintenanceWindow
--
--         , requestDeregisterPatchBaselineForPatchGroup $
--             newDeregisterPatchBaselineForPatchGroup
--
--         , requestResumeSession $
--             newResumeSession
--
--         , requestRegisterDefaultPatchBaseline $
--             newRegisterDefaultPatchBaseline
--
--         , requestDescribeMaintenanceWindows $
--             newDescribeMaintenanceWindows
--
--         , requestModifyDocumentPermission $
--             newModifyDocumentPermission
--
--         , requestDescribeInstancePatches $
--             newDescribeInstancePatches
--
--         , requestGetParameters $
--             newGetParameters
--
--         , requestListDocumentVersions $
--             newListDocumentVersions
--
--         , requestUpdateResourceDataSync $
--             newUpdateResourceDataSync
--
--         , requestDeletePatchBaseline $
--             newDeletePatchBaseline
--
--         , requestListOpsItemEvents $
--             newListOpsItemEvents
--
--         , requestDeleteResourceDataSync $
--             newDeleteResourceDataSync
--
--         , requestDescribeInstancePatchStatesForPatchGroup $
--             newDescribeInstancePatchStatesForPatchGroup
--
--         , requestGetConnectionStatus $
--             newGetConnectionStatus
--
--         , requestUpdatePatchBaseline $
--             newUpdatePatchBaseline
--
--         , requestDescribeAvailablePatches $
--             newDescribeAvailablePatches
--
--         , requestListComplianceItems $
--             newListComplianceItems
--
--         , requestGetDocument $
--             newGetDocument
--
--         , requestAddTagsToResource $
--             newAddTagsToResource
--
--         , requestGetPatchBaselineForPatchGroup $
--             newGetPatchBaselineForPatchGroup
--
--         , requestUpdateManagedInstanceRole $
--             newUpdateManagedInstanceRole
--
--         , requestDescribeMaintenanceWindowSchedule $
--             newDescribeMaintenanceWindowSchedule
--
--         , requestCreateResourceDataSync $
--             newCreateResourceDataSync
--
--         , requestCreateAssociationBatch $
--             newCreateAssociationBatch
--
--         , requestCancelCommand $
--             newCancelCommand
--
--         , requestDescribeDocumentPermission $
--             newDescribeDocumentPermission
--
--         , requestRegisterPatchBaselineForPatchGroup $
--             newRegisterPatchBaselineForPatchGroup
--
--         , requestDescribeEffectivePatchesForPatchBaseline $
--             newDescribeEffectivePatchesForPatchBaseline
--
--         , requestListCommandInvocations $
--             newListCommandInvocations
--
--         , requestDescribeMaintenanceWindowTargets $
--             newDescribeMaintenanceWindowTargets
--
--         , requestGetAutomationExecution $
--             newGetAutomationExecution
--
--         , requestGetPatchBaseline $
--             newGetPatchBaseline
--
--         , requestDescribePatchGroupState $
--             newDescribePatchGroupState
--
--         , requestDescribePatchBaselines $
--             newDescribePatchBaselines
--
--         , requestUpdateDocumentMetadata $
--             newUpdateDocumentMetadata
--
--         , requestDescribeAutomationExecutions $
--             newDescribeAutomationExecutions
--
--         , requestPutInventory $
--             newPutInventory
--
--         , requestDescribeInventoryDeletions $
--             newDescribeInventoryDeletions
--
--         , requestDescribeMaintenanceWindowExecutionTasks $
--             newDescribeMaintenanceWindowExecutionTasks
--
--         , requestDeleteMaintenanceWindow $
--             newDeleteMaintenanceWindow
--
--         , requestGetDefaultPatchBaseline $
--             newGetDefaultPatchBaseline
--
--         , requestPutParameter $
--             newPutParameter
--
--         , requestUpdateMaintenanceWindow $
--             newUpdateMaintenanceWindow
--
--         , requestListAssociations $
--             newListAssociations
--
--         , requestGetMaintenanceWindowExecutionTask $
--             newGetMaintenanceWindowExecutionTask
--
--         , requestStartAutomationExecution $
--             newStartAutomationExecution
--
--         , requestDeleteActivation $
--             newDeleteActivation
--
--         , requestCreateMaintenanceWindow $
--             newCreateMaintenanceWindow
--
--         , requestDescribeAssociationExecutions $
--             newDescribeAssociationExecutions
--
--         , requestGetMaintenanceWindowExecution $
--             newGetMaintenanceWindowExecution
--
--         , requestStopAutomationExecution $
--             newStopAutomationExecution
--
--         , requestGetInventorySchema $
--             newGetInventorySchema
--
--         , requestStartAssociationsOnce $
--             newStartAssociationsOnce
--
--         , requestCancelMaintenanceWindowExecution $
--             newCancelMaintenanceWindowExecution
--
--         , requestLabelParameterVersion $
--             newLabelParameterVersion
--
--         , requestGetParameterHistory $
--             newGetParameterHistory
--
--         , requestGetServiceSetting $
--             newGetServiceSetting
--
--         , requestUpdateMaintenanceWindowTask $
--             newUpdateMaintenanceWindowTask
--
--         , requestListDocumentMetadataHistory $
--             newListDocumentMetadataHistory
--
--         , requestListInventoryEntries $
--             newListInventoryEntries
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetParametersByPath $
--             newGetParametersByPath
--
--         , requestDescribeActivations $
--             newDescribeActivations
--
--         , requestDescribeInstanceAssociationsStatus $
--             newDescribeInstanceAssociationsStatus
--
--         , requestDescribePatchProperties $
--             newDescribePatchProperties
--
--         , requestUpdateServiceSetting $
--             newUpdateServiceSetting
--
--         , requestDescribeMaintenanceWindowTasks $
--             newDescribeMaintenanceWindowTasks
--
--           ]

--     , testGroup "response"
--         [ responseListResourceComplianceSummaries $
--             newListResourceComplianceSummariesResponse
--
--         , responseDescribePatchGroups $
--             newDescribePatchGroupsResponse
--
--         , responseDescribeOpsItems $
--             newDescribeOpsItemsResponse
--
--         , responseTerminateSession $
--             newTerminateSessionResponse
--
--         , responseGetParameter $
--             newGetParameterResponse
--
--         , responseGetOpsMetadata $
--             newGetOpsMetadataResponse
--
--         , responseUpdateDocumentDefaultVersion $
--             newUpdateDocumentDefaultVersionResponse
--
--         , responseListResourceDataSync $
--             newListResourceDataSyncResponse
--
--         , responseDescribeParameters $
--             newDescribeParametersResponse
--
--         , responseRegisterTaskWithMaintenanceWindow $
--             newRegisterTaskWithMaintenanceWindowResponse
--
--         , responseGetOpsItem $
--             newGetOpsItemResponse
--
--         , responseGetInventory $
--             newGetInventoryResponse
--
--         , responseDescribeAssociation $
--             newDescribeAssociationResponse
--
--         , responseUpdateAssociationStatus $
--             newUpdateAssociationStatusResponse
--
--         , responseDeregisterManagedInstance $
--             newDeregisterManagedInstanceResponse
--
--         , responseDescribeAssociationExecutionTargets $
--             newDescribeAssociationExecutionTargetsResponse
--
--         , responseUpdateMaintenanceWindowTarget $
--             newUpdateMaintenanceWindowTargetResponse
--
--         , responseDescribeAutomationStepExecutions $
--             newDescribeAutomationStepExecutionsResponse
--
--         , responseCreatePatchBaseline $
--             newCreatePatchBaselineResponse
--
--         , responseGetCommandInvocation $
--             newGetCommandInvocationResponse
--
--         , responseListCommands $
--             newListCommandsResponse
--
--         , responseDeregisterTargetFromMaintenanceWindow $
--             newDeregisterTargetFromMaintenanceWindowResponse
--
--         , responseDescribeInstancePatchStates $
--             newDescribeInstancePatchStatesResponse
--
--         , responseUpdateDocument $
--             newUpdateDocumentResponse
--
--         , responseListDocuments $
--             newListDocumentsResponse
--
--         , responseResetServiceSetting $
--             newResetServiceSettingResponse
--
--         , responseStartSession $
--             newStartSessionResponse
--
--         , responseGetOpsSummary $
--             newGetOpsSummaryResponse
--
--         , responseDeleteDocument $
--             newDeleteDocumentResponse
--
--         , responseDeleteInventory $
--             newDeleteInventoryResponse
--
--         , responseDeleteParameters $
--             newDeleteParametersResponse
--
--         , responseListOpsMetadata $
--             newListOpsMetadataResponse
--
--         , responseSendCommand $
--             newSendCommandResponse
--
--         , responseRemoveTagsFromResource $
--             newRemoveTagsFromResourceResponse
--
--         , responseDescribeEffectiveInstanceAssociations $
--             newDescribeEffectiveInstanceAssociationsResponse
--
--         , responseCreateDocument $
--             newCreateDocumentResponse
--
--         , responseGetCalendarState $
--             newGetCalendarStateResponse
--
--         , responseRegisterTargetWithMaintenanceWindow $
--             newRegisterTargetWithMaintenanceWindowResponse
--
--         , responseDescribeSessions $
--             newDescribeSessionsResponse
--
--         , responseDeleteOpsMetadata $
--             newDeleteOpsMetadataResponse
--
--         , responseUpdateOpsItem $
--             newUpdateOpsItemResponse
--
--         , responseDeleteAssociation $
--             newDeleteAssociationResponse
--
--         , responseUpdateAssociation $
--             newUpdateAssociationResponse
--
--         , responseUpdateOpsMetadata $
--             newUpdateOpsMetadataResponse
--
--         , responseDescribeInstanceInformation $
--             newDescribeInstanceInformationResponse
--
--         , responseDescribeMaintenanceWindowExecutionTaskInvocations $
--             newDescribeMaintenanceWindowExecutionTaskInvocationsResponse
--
--         , responseDeleteParameter $
--             newDeleteParameterResponse
--
--         , responseGetMaintenanceWindowExecutionTaskInvocation $
--             newGetMaintenanceWindowExecutionTaskInvocationResponse
--
--         , responseCreateActivation $
--             newCreateActivationResponse
--
--         , responseCreateOpsMetadata $
--             newCreateOpsMetadataResponse
--
--         , responseSendAutomationSignal $
--             newSendAutomationSignalResponse
--
--         , responseListComplianceSummaries $
--             newListComplianceSummariesResponse
--
--         , responseDescribeMaintenanceWindowExecutions $
--             newDescribeMaintenanceWindowExecutionsResponse
--
--         , responseDescribeMaintenanceWindowsForTarget $
--             newDescribeMaintenanceWindowsForTargetResponse
--
--         , responseCreateAssociation $
--             newCreateAssociationResponse
--
--         , responseCreateOpsItem $
--             newCreateOpsItemResponse
--
--         , responseStartChangeRequestExecution $
--             newStartChangeRequestExecutionResponse
--
--         , responseListAssociationVersions $
--             newListAssociationVersionsResponse
--
--         , responseDescribeDocument $
--             newDescribeDocumentResponse
--
--         , responseDeregisterTaskFromMaintenanceWindow $
--             newDeregisterTaskFromMaintenanceWindowResponse
--
--         , responsePutComplianceItems $
--             newPutComplianceItemsResponse
--
--         , responseGetMaintenanceWindowTask $
--             newGetMaintenanceWindowTaskResponse
--
--         , responseGetDeployablePatchSnapshotForInstance $
--             newGetDeployablePatchSnapshotForInstanceResponse
--
--         , responseGetMaintenanceWindow $
--             newGetMaintenanceWindowResponse
--
--         , responseDeregisterPatchBaselineForPatchGroup $
--             newDeregisterPatchBaselineForPatchGroupResponse
--
--         , responseResumeSession $
--             newResumeSessionResponse
--
--         , responseRegisterDefaultPatchBaseline $
--             newRegisterDefaultPatchBaselineResponse
--
--         , responseDescribeMaintenanceWindows $
--             newDescribeMaintenanceWindowsResponse
--
--         , responseModifyDocumentPermission $
--             newModifyDocumentPermissionResponse
--
--         , responseDescribeInstancePatches $
--             newDescribeInstancePatchesResponse
--
--         , responseGetParameters $
--             newGetParametersResponse
--
--         , responseListDocumentVersions $
--             newListDocumentVersionsResponse
--
--         , responseUpdateResourceDataSync $
--             newUpdateResourceDataSyncResponse
--
--         , responseDeletePatchBaseline $
--             newDeletePatchBaselineResponse
--
--         , responseListOpsItemEvents $
--             newListOpsItemEventsResponse
--
--         , responseDeleteResourceDataSync $
--             newDeleteResourceDataSyncResponse
--
--         , responseDescribeInstancePatchStatesForPatchGroup $
--             newDescribeInstancePatchStatesForPatchGroupResponse
--
--         , responseGetConnectionStatus $
--             newGetConnectionStatusResponse
--
--         , responseUpdatePatchBaseline $
--             newUpdatePatchBaselineResponse
--
--         , responseDescribeAvailablePatches $
--             newDescribeAvailablePatchesResponse
--
--         , responseListComplianceItems $
--             newListComplianceItemsResponse
--
--         , responseGetDocument $
--             newGetDocumentResponse
--
--         , responseAddTagsToResource $
--             newAddTagsToResourceResponse
--
--         , responseGetPatchBaselineForPatchGroup $
--             newGetPatchBaselineForPatchGroupResponse
--
--         , responseUpdateManagedInstanceRole $
--             newUpdateManagedInstanceRoleResponse
--
--         , responseDescribeMaintenanceWindowSchedule $
--             newDescribeMaintenanceWindowScheduleResponse
--
--         , responseCreateResourceDataSync $
--             newCreateResourceDataSyncResponse
--
--         , responseCreateAssociationBatch $
--             newCreateAssociationBatchResponse
--
--         , responseCancelCommand $
--             newCancelCommandResponse
--
--         , responseDescribeDocumentPermission $
--             newDescribeDocumentPermissionResponse
--
--         , responseRegisterPatchBaselineForPatchGroup $
--             newRegisterPatchBaselineForPatchGroupResponse
--
--         , responseDescribeEffectivePatchesForPatchBaseline $
--             newDescribeEffectivePatchesForPatchBaselineResponse
--
--         , responseListCommandInvocations $
--             newListCommandInvocationsResponse
--
--         , responseDescribeMaintenanceWindowTargets $
--             newDescribeMaintenanceWindowTargetsResponse
--
--         , responseGetAutomationExecution $
--             newGetAutomationExecutionResponse
--
--         , responseGetPatchBaseline $
--             newGetPatchBaselineResponse
--
--         , responseDescribePatchGroupState $
--             newDescribePatchGroupStateResponse
--
--         , responseDescribePatchBaselines $
--             newDescribePatchBaselinesResponse
--
--         , responseUpdateDocumentMetadata $
--             newUpdateDocumentMetadataResponse
--
--         , responseDescribeAutomationExecutions $
--             newDescribeAutomationExecutionsResponse
--
--         , responsePutInventory $
--             newPutInventoryResponse
--
--         , responseDescribeInventoryDeletions $
--             newDescribeInventoryDeletionsResponse
--
--         , responseDescribeMaintenanceWindowExecutionTasks $
--             newDescribeMaintenanceWindowExecutionTasksResponse
--
--         , responseDeleteMaintenanceWindow $
--             newDeleteMaintenanceWindowResponse
--
--         , responseGetDefaultPatchBaseline $
--             newGetDefaultPatchBaselineResponse
--
--         , responsePutParameter $
--             newPutParameterResponse
--
--         , responseUpdateMaintenanceWindow $
--             newUpdateMaintenanceWindowResponse
--
--         , responseListAssociations $
--             newListAssociationsResponse
--
--         , responseGetMaintenanceWindowExecutionTask $
--             newGetMaintenanceWindowExecutionTaskResponse
--
--         , responseStartAutomationExecution $
--             newStartAutomationExecutionResponse
--
--         , responseDeleteActivation $
--             newDeleteActivationResponse
--
--         , responseCreateMaintenanceWindow $
--             newCreateMaintenanceWindowResponse
--
--         , responseDescribeAssociationExecutions $
--             newDescribeAssociationExecutionsResponse
--
--         , responseGetMaintenanceWindowExecution $
--             newGetMaintenanceWindowExecutionResponse
--
--         , responseStopAutomationExecution $
--             newStopAutomationExecutionResponse
--
--         , responseGetInventorySchema $
--             newGetInventorySchemaResponse
--
--         , responseStartAssociationsOnce $
--             newStartAssociationsOnceResponse
--
--         , responseCancelMaintenanceWindowExecution $
--             newCancelMaintenanceWindowExecutionResponse
--
--         , responseLabelParameterVersion $
--             newLabelParameterVersionResponse
--
--         , responseGetParameterHistory $
--             newGetParameterHistoryResponse
--
--         , responseGetServiceSetting $
--             newGetServiceSettingResponse
--
--         , responseUpdateMaintenanceWindowTask $
--             newUpdateMaintenanceWindowTaskResponse
--
--         , responseListDocumentMetadataHistory $
--             newListDocumentMetadataHistoryResponse
--
--         , responseListInventoryEntries $
--             newListInventoryEntriesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetParametersByPath $
--             newGetParametersByPathResponse
--
--         , responseDescribeActivations $
--             newDescribeActivationsResponse
--
--         , responseDescribeInstanceAssociationsStatus $
--             newDescribeInstanceAssociationsStatusResponse
--
--         , responseDescribePatchProperties $
--             newDescribePatchPropertiesResponse
--
--         , responseUpdateServiceSetting $
--             newUpdateServiceSettingResponse
--
--         , responseDescribeMaintenanceWindowTasks $
--             newDescribeMaintenanceWindowTasksResponse
--
--           ]
--     ]

-- Requests

requestListResourceComplianceSummaries :: ListResourceComplianceSummaries -> TestTree
requestListResourceComplianceSummaries =
  req
    "ListResourceComplianceSummaries"
    "fixture/ListResourceComplianceSummaries.yaml"

requestDescribePatchGroups :: DescribePatchGroups -> TestTree
requestDescribePatchGroups =
  req
    "DescribePatchGroups"
    "fixture/DescribePatchGroups.yaml"

requestDescribeOpsItems :: DescribeOpsItems -> TestTree
requestDescribeOpsItems =
  req
    "DescribeOpsItems"
    "fixture/DescribeOpsItems.yaml"

requestTerminateSession :: TerminateSession -> TestTree
requestTerminateSession =
  req
    "TerminateSession"
    "fixture/TerminateSession.yaml"

requestGetParameter :: GetParameter -> TestTree
requestGetParameter =
  req
    "GetParameter"
    "fixture/GetParameter.yaml"

requestGetOpsMetadata :: GetOpsMetadata -> TestTree
requestGetOpsMetadata =
  req
    "GetOpsMetadata"
    "fixture/GetOpsMetadata.yaml"

requestUpdateDocumentDefaultVersion :: UpdateDocumentDefaultVersion -> TestTree
requestUpdateDocumentDefaultVersion =
  req
    "UpdateDocumentDefaultVersion"
    "fixture/UpdateDocumentDefaultVersion.yaml"

requestListResourceDataSync :: ListResourceDataSync -> TestTree
requestListResourceDataSync =
  req
    "ListResourceDataSync"
    "fixture/ListResourceDataSync.yaml"

requestDescribeParameters :: DescribeParameters -> TestTree
requestDescribeParameters =
  req
    "DescribeParameters"
    "fixture/DescribeParameters.yaml"

requestRegisterTaskWithMaintenanceWindow :: RegisterTaskWithMaintenanceWindow -> TestTree
requestRegisterTaskWithMaintenanceWindow =
  req
    "RegisterTaskWithMaintenanceWindow"
    "fixture/RegisterTaskWithMaintenanceWindow.yaml"

requestGetOpsItem :: GetOpsItem -> TestTree
requestGetOpsItem =
  req
    "GetOpsItem"
    "fixture/GetOpsItem.yaml"

requestGetInventory :: GetInventory -> TestTree
requestGetInventory =
  req
    "GetInventory"
    "fixture/GetInventory.yaml"

requestDescribeAssociation :: DescribeAssociation -> TestTree
requestDescribeAssociation =
  req
    "DescribeAssociation"
    "fixture/DescribeAssociation.yaml"

requestUpdateAssociationStatus :: UpdateAssociationStatus -> TestTree
requestUpdateAssociationStatus =
  req
    "UpdateAssociationStatus"
    "fixture/UpdateAssociationStatus.yaml"

requestDeregisterManagedInstance :: DeregisterManagedInstance -> TestTree
requestDeregisterManagedInstance =
  req
    "DeregisterManagedInstance"
    "fixture/DeregisterManagedInstance.yaml"

requestDescribeAssociationExecutionTargets :: DescribeAssociationExecutionTargets -> TestTree
requestDescribeAssociationExecutionTargets =
  req
    "DescribeAssociationExecutionTargets"
    "fixture/DescribeAssociationExecutionTargets.yaml"

requestUpdateMaintenanceWindowTarget :: UpdateMaintenanceWindowTarget -> TestTree
requestUpdateMaintenanceWindowTarget =
  req
    "UpdateMaintenanceWindowTarget"
    "fixture/UpdateMaintenanceWindowTarget.yaml"

requestDescribeAutomationStepExecutions :: DescribeAutomationStepExecutions -> TestTree
requestDescribeAutomationStepExecutions =
  req
    "DescribeAutomationStepExecutions"
    "fixture/DescribeAutomationStepExecutions.yaml"

requestCreatePatchBaseline :: CreatePatchBaseline -> TestTree
requestCreatePatchBaseline =
  req
    "CreatePatchBaseline"
    "fixture/CreatePatchBaseline.yaml"

requestGetCommandInvocation :: GetCommandInvocation -> TestTree
requestGetCommandInvocation =
  req
    "GetCommandInvocation"
    "fixture/GetCommandInvocation.yaml"

requestListCommands :: ListCommands -> TestTree
requestListCommands =
  req
    "ListCommands"
    "fixture/ListCommands.yaml"

requestDeregisterTargetFromMaintenanceWindow :: DeregisterTargetFromMaintenanceWindow -> TestTree
requestDeregisterTargetFromMaintenanceWindow =
  req
    "DeregisterTargetFromMaintenanceWindow"
    "fixture/DeregisterTargetFromMaintenanceWindow.yaml"

requestDescribeInstancePatchStates :: DescribeInstancePatchStates -> TestTree
requestDescribeInstancePatchStates =
  req
    "DescribeInstancePatchStates"
    "fixture/DescribeInstancePatchStates.yaml"

requestUpdateDocument :: UpdateDocument -> TestTree
requestUpdateDocument =
  req
    "UpdateDocument"
    "fixture/UpdateDocument.yaml"

requestListDocuments :: ListDocuments -> TestTree
requestListDocuments =
  req
    "ListDocuments"
    "fixture/ListDocuments.yaml"

requestResetServiceSetting :: ResetServiceSetting -> TestTree
requestResetServiceSetting =
  req
    "ResetServiceSetting"
    "fixture/ResetServiceSetting.yaml"

requestStartSession :: StartSession -> TestTree
requestStartSession =
  req
    "StartSession"
    "fixture/StartSession.yaml"

requestGetOpsSummary :: GetOpsSummary -> TestTree
requestGetOpsSummary =
  req
    "GetOpsSummary"
    "fixture/GetOpsSummary.yaml"

requestDeleteDocument :: DeleteDocument -> TestTree
requestDeleteDocument =
  req
    "DeleteDocument"
    "fixture/DeleteDocument.yaml"

requestDeleteInventory :: DeleteInventory -> TestTree
requestDeleteInventory =
  req
    "DeleteInventory"
    "fixture/DeleteInventory.yaml"

requestDeleteParameters :: DeleteParameters -> TestTree
requestDeleteParameters =
  req
    "DeleteParameters"
    "fixture/DeleteParameters.yaml"

requestListOpsMetadata :: ListOpsMetadata -> TestTree
requestListOpsMetadata =
  req
    "ListOpsMetadata"
    "fixture/ListOpsMetadata.yaml"

requestSendCommand :: SendCommand -> TestTree
requestSendCommand =
  req
    "SendCommand"
    "fixture/SendCommand.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource =
  req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestDescribeEffectiveInstanceAssociations :: DescribeEffectiveInstanceAssociations -> TestTree
requestDescribeEffectiveInstanceAssociations =
  req
    "DescribeEffectiveInstanceAssociations"
    "fixture/DescribeEffectiveInstanceAssociations.yaml"

requestCreateDocument :: CreateDocument -> TestTree
requestCreateDocument =
  req
    "CreateDocument"
    "fixture/CreateDocument.yaml"

requestGetCalendarState :: GetCalendarState -> TestTree
requestGetCalendarState =
  req
    "GetCalendarState"
    "fixture/GetCalendarState.yaml"

requestRegisterTargetWithMaintenanceWindow :: RegisterTargetWithMaintenanceWindow -> TestTree
requestRegisterTargetWithMaintenanceWindow =
  req
    "RegisterTargetWithMaintenanceWindow"
    "fixture/RegisterTargetWithMaintenanceWindow.yaml"

requestDescribeSessions :: DescribeSessions -> TestTree
requestDescribeSessions =
  req
    "DescribeSessions"
    "fixture/DescribeSessions.yaml"

requestDeleteOpsMetadata :: DeleteOpsMetadata -> TestTree
requestDeleteOpsMetadata =
  req
    "DeleteOpsMetadata"
    "fixture/DeleteOpsMetadata.yaml"

requestUpdateOpsItem :: UpdateOpsItem -> TestTree
requestUpdateOpsItem =
  req
    "UpdateOpsItem"
    "fixture/UpdateOpsItem.yaml"

requestDeleteAssociation :: DeleteAssociation -> TestTree
requestDeleteAssociation =
  req
    "DeleteAssociation"
    "fixture/DeleteAssociation.yaml"

requestUpdateAssociation :: UpdateAssociation -> TestTree
requestUpdateAssociation =
  req
    "UpdateAssociation"
    "fixture/UpdateAssociation.yaml"

requestUpdateOpsMetadata :: UpdateOpsMetadata -> TestTree
requestUpdateOpsMetadata =
  req
    "UpdateOpsMetadata"
    "fixture/UpdateOpsMetadata.yaml"

requestDescribeInstanceInformation :: DescribeInstanceInformation -> TestTree
requestDescribeInstanceInformation =
  req
    "DescribeInstanceInformation"
    "fixture/DescribeInstanceInformation.yaml"

requestDescribeMaintenanceWindowExecutionTaskInvocations :: DescribeMaintenanceWindowExecutionTaskInvocations -> TestTree
requestDescribeMaintenanceWindowExecutionTaskInvocations =
  req
    "DescribeMaintenanceWindowExecutionTaskInvocations"
    "fixture/DescribeMaintenanceWindowExecutionTaskInvocations.yaml"

requestDeleteParameter :: DeleteParameter -> TestTree
requestDeleteParameter =
  req
    "DeleteParameter"
    "fixture/DeleteParameter.yaml"

requestGetMaintenanceWindowExecutionTaskInvocation :: GetMaintenanceWindowExecutionTaskInvocation -> TestTree
requestGetMaintenanceWindowExecutionTaskInvocation =
  req
    "GetMaintenanceWindowExecutionTaskInvocation"
    "fixture/GetMaintenanceWindowExecutionTaskInvocation.yaml"

requestCreateActivation :: CreateActivation -> TestTree
requestCreateActivation =
  req
    "CreateActivation"
    "fixture/CreateActivation.yaml"

requestCreateOpsMetadata :: CreateOpsMetadata -> TestTree
requestCreateOpsMetadata =
  req
    "CreateOpsMetadata"
    "fixture/CreateOpsMetadata.yaml"

requestSendAutomationSignal :: SendAutomationSignal -> TestTree
requestSendAutomationSignal =
  req
    "SendAutomationSignal"
    "fixture/SendAutomationSignal.yaml"

requestListComplianceSummaries :: ListComplianceSummaries -> TestTree
requestListComplianceSummaries =
  req
    "ListComplianceSummaries"
    "fixture/ListComplianceSummaries.yaml"

requestDescribeMaintenanceWindowExecutions :: DescribeMaintenanceWindowExecutions -> TestTree
requestDescribeMaintenanceWindowExecutions =
  req
    "DescribeMaintenanceWindowExecutions"
    "fixture/DescribeMaintenanceWindowExecutions.yaml"

requestDescribeMaintenanceWindowsForTarget :: DescribeMaintenanceWindowsForTarget -> TestTree
requestDescribeMaintenanceWindowsForTarget =
  req
    "DescribeMaintenanceWindowsForTarget"
    "fixture/DescribeMaintenanceWindowsForTarget.yaml"

requestCreateAssociation :: CreateAssociation -> TestTree
requestCreateAssociation =
  req
    "CreateAssociation"
    "fixture/CreateAssociation.yaml"

requestCreateOpsItem :: CreateOpsItem -> TestTree
requestCreateOpsItem =
  req
    "CreateOpsItem"
    "fixture/CreateOpsItem.yaml"

requestStartChangeRequestExecution :: StartChangeRequestExecution -> TestTree
requestStartChangeRequestExecution =
  req
    "StartChangeRequestExecution"
    "fixture/StartChangeRequestExecution.yaml"

requestListAssociationVersions :: ListAssociationVersions -> TestTree
requestListAssociationVersions =
  req
    "ListAssociationVersions"
    "fixture/ListAssociationVersions.yaml"

requestDescribeDocument :: DescribeDocument -> TestTree
requestDescribeDocument =
  req
    "DescribeDocument"
    "fixture/DescribeDocument.yaml"

requestDeregisterTaskFromMaintenanceWindow :: DeregisterTaskFromMaintenanceWindow -> TestTree
requestDeregisterTaskFromMaintenanceWindow =
  req
    "DeregisterTaskFromMaintenanceWindow"
    "fixture/DeregisterTaskFromMaintenanceWindow.yaml"

requestPutComplianceItems :: PutComplianceItems -> TestTree
requestPutComplianceItems =
  req
    "PutComplianceItems"
    "fixture/PutComplianceItems.yaml"

requestGetMaintenanceWindowTask :: GetMaintenanceWindowTask -> TestTree
requestGetMaintenanceWindowTask =
  req
    "GetMaintenanceWindowTask"
    "fixture/GetMaintenanceWindowTask.yaml"

requestGetDeployablePatchSnapshotForInstance :: GetDeployablePatchSnapshotForInstance -> TestTree
requestGetDeployablePatchSnapshotForInstance =
  req
    "GetDeployablePatchSnapshotForInstance"
    "fixture/GetDeployablePatchSnapshotForInstance.yaml"

requestGetMaintenanceWindow :: GetMaintenanceWindow -> TestTree
requestGetMaintenanceWindow =
  req
    "GetMaintenanceWindow"
    "fixture/GetMaintenanceWindow.yaml"

requestDeregisterPatchBaselineForPatchGroup :: DeregisterPatchBaselineForPatchGroup -> TestTree
requestDeregisterPatchBaselineForPatchGroup =
  req
    "DeregisterPatchBaselineForPatchGroup"
    "fixture/DeregisterPatchBaselineForPatchGroup.yaml"

requestResumeSession :: ResumeSession -> TestTree
requestResumeSession =
  req
    "ResumeSession"
    "fixture/ResumeSession.yaml"

requestRegisterDefaultPatchBaseline :: RegisterDefaultPatchBaseline -> TestTree
requestRegisterDefaultPatchBaseline =
  req
    "RegisterDefaultPatchBaseline"
    "fixture/RegisterDefaultPatchBaseline.yaml"

requestDescribeMaintenanceWindows :: DescribeMaintenanceWindows -> TestTree
requestDescribeMaintenanceWindows =
  req
    "DescribeMaintenanceWindows"
    "fixture/DescribeMaintenanceWindows.yaml"

requestModifyDocumentPermission :: ModifyDocumentPermission -> TestTree
requestModifyDocumentPermission =
  req
    "ModifyDocumentPermission"
    "fixture/ModifyDocumentPermission.yaml"

requestDescribeInstancePatches :: DescribeInstancePatches -> TestTree
requestDescribeInstancePatches =
  req
    "DescribeInstancePatches"
    "fixture/DescribeInstancePatches.yaml"

requestGetParameters :: GetParameters -> TestTree
requestGetParameters =
  req
    "GetParameters"
    "fixture/GetParameters.yaml"

requestListDocumentVersions :: ListDocumentVersions -> TestTree
requestListDocumentVersions =
  req
    "ListDocumentVersions"
    "fixture/ListDocumentVersions.yaml"

requestUpdateResourceDataSync :: UpdateResourceDataSync -> TestTree
requestUpdateResourceDataSync =
  req
    "UpdateResourceDataSync"
    "fixture/UpdateResourceDataSync.yaml"

requestDeletePatchBaseline :: DeletePatchBaseline -> TestTree
requestDeletePatchBaseline =
  req
    "DeletePatchBaseline"
    "fixture/DeletePatchBaseline.yaml"

requestListOpsItemEvents :: ListOpsItemEvents -> TestTree
requestListOpsItemEvents =
  req
    "ListOpsItemEvents"
    "fixture/ListOpsItemEvents.yaml"

requestDeleteResourceDataSync :: DeleteResourceDataSync -> TestTree
requestDeleteResourceDataSync =
  req
    "DeleteResourceDataSync"
    "fixture/DeleteResourceDataSync.yaml"

requestDescribeInstancePatchStatesForPatchGroup :: DescribeInstancePatchStatesForPatchGroup -> TestTree
requestDescribeInstancePatchStatesForPatchGroup =
  req
    "DescribeInstancePatchStatesForPatchGroup"
    "fixture/DescribeInstancePatchStatesForPatchGroup.yaml"

requestGetConnectionStatus :: GetConnectionStatus -> TestTree
requestGetConnectionStatus =
  req
    "GetConnectionStatus"
    "fixture/GetConnectionStatus.yaml"

requestUpdatePatchBaseline :: UpdatePatchBaseline -> TestTree
requestUpdatePatchBaseline =
  req
    "UpdatePatchBaseline"
    "fixture/UpdatePatchBaseline.yaml"

requestDescribeAvailablePatches :: DescribeAvailablePatches -> TestTree
requestDescribeAvailablePatches =
  req
    "DescribeAvailablePatches"
    "fixture/DescribeAvailablePatches.yaml"

requestListComplianceItems :: ListComplianceItems -> TestTree
requestListComplianceItems =
  req
    "ListComplianceItems"
    "fixture/ListComplianceItems.yaml"

requestGetDocument :: GetDocument -> TestTree
requestGetDocument =
  req
    "GetDocument"
    "fixture/GetDocument.yaml"

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource =
  req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestGetPatchBaselineForPatchGroup :: GetPatchBaselineForPatchGroup -> TestTree
requestGetPatchBaselineForPatchGroup =
  req
    "GetPatchBaselineForPatchGroup"
    "fixture/GetPatchBaselineForPatchGroup.yaml"

requestUpdateManagedInstanceRole :: UpdateManagedInstanceRole -> TestTree
requestUpdateManagedInstanceRole =
  req
    "UpdateManagedInstanceRole"
    "fixture/UpdateManagedInstanceRole.yaml"

requestDescribeMaintenanceWindowSchedule :: DescribeMaintenanceWindowSchedule -> TestTree
requestDescribeMaintenanceWindowSchedule =
  req
    "DescribeMaintenanceWindowSchedule"
    "fixture/DescribeMaintenanceWindowSchedule.yaml"

requestCreateResourceDataSync :: CreateResourceDataSync -> TestTree
requestCreateResourceDataSync =
  req
    "CreateResourceDataSync"
    "fixture/CreateResourceDataSync.yaml"

requestCreateAssociationBatch :: CreateAssociationBatch -> TestTree
requestCreateAssociationBatch =
  req
    "CreateAssociationBatch"
    "fixture/CreateAssociationBatch.yaml"

requestCancelCommand :: CancelCommand -> TestTree
requestCancelCommand =
  req
    "CancelCommand"
    "fixture/CancelCommand.yaml"

requestDescribeDocumentPermission :: DescribeDocumentPermission -> TestTree
requestDescribeDocumentPermission =
  req
    "DescribeDocumentPermission"
    "fixture/DescribeDocumentPermission.yaml"

requestRegisterPatchBaselineForPatchGroup :: RegisterPatchBaselineForPatchGroup -> TestTree
requestRegisterPatchBaselineForPatchGroup =
  req
    "RegisterPatchBaselineForPatchGroup"
    "fixture/RegisterPatchBaselineForPatchGroup.yaml"

requestDescribeEffectivePatchesForPatchBaseline :: DescribeEffectivePatchesForPatchBaseline -> TestTree
requestDescribeEffectivePatchesForPatchBaseline =
  req
    "DescribeEffectivePatchesForPatchBaseline"
    "fixture/DescribeEffectivePatchesForPatchBaseline.yaml"

requestListCommandInvocations :: ListCommandInvocations -> TestTree
requestListCommandInvocations =
  req
    "ListCommandInvocations"
    "fixture/ListCommandInvocations.yaml"

requestDescribeMaintenanceWindowTargets :: DescribeMaintenanceWindowTargets -> TestTree
requestDescribeMaintenanceWindowTargets =
  req
    "DescribeMaintenanceWindowTargets"
    "fixture/DescribeMaintenanceWindowTargets.yaml"

requestGetAutomationExecution :: GetAutomationExecution -> TestTree
requestGetAutomationExecution =
  req
    "GetAutomationExecution"
    "fixture/GetAutomationExecution.yaml"

requestGetPatchBaseline :: GetPatchBaseline -> TestTree
requestGetPatchBaseline =
  req
    "GetPatchBaseline"
    "fixture/GetPatchBaseline.yaml"

requestDescribePatchGroupState :: DescribePatchGroupState -> TestTree
requestDescribePatchGroupState =
  req
    "DescribePatchGroupState"
    "fixture/DescribePatchGroupState.yaml"

requestDescribePatchBaselines :: DescribePatchBaselines -> TestTree
requestDescribePatchBaselines =
  req
    "DescribePatchBaselines"
    "fixture/DescribePatchBaselines.yaml"

requestUpdateDocumentMetadata :: UpdateDocumentMetadata -> TestTree
requestUpdateDocumentMetadata =
  req
    "UpdateDocumentMetadata"
    "fixture/UpdateDocumentMetadata.yaml"

requestDescribeAutomationExecutions :: DescribeAutomationExecutions -> TestTree
requestDescribeAutomationExecutions =
  req
    "DescribeAutomationExecutions"
    "fixture/DescribeAutomationExecutions.yaml"

requestPutInventory :: PutInventory -> TestTree
requestPutInventory =
  req
    "PutInventory"
    "fixture/PutInventory.yaml"

requestDescribeInventoryDeletions :: DescribeInventoryDeletions -> TestTree
requestDescribeInventoryDeletions =
  req
    "DescribeInventoryDeletions"
    "fixture/DescribeInventoryDeletions.yaml"

requestDescribeMaintenanceWindowExecutionTasks :: DescribeMaintenanceWindowExecutionTasks -> TestTree
requestDescribeMaintenanceWindowExecutionTasks =
  req
    "DescribeMaintenanceWindowExecutionTasks"
    "fixture/DescribeMaintenanceWindowExecutionTasks.yaml"

requestDeleteMaintenanceWindow :: DeleteMaintenanceWindow -> TestTree
requestDeleteMaintenanceWindow =
  req
    "DeleteMaintenanceWindow"
    "fixture/DeleteMaintenanceWindow.yaml"

requestGetDefaultPatchBaseline :: GetDefaultPatchBaseline -> TestTree
requestGetDefaultPatchBaseline =
  req
    "GetDefaultPatchBaseline"
    "fixture/GetDefaultPatchBaseline.yaml"

requestPutParameter :: PutParameter -> TestTree
requestPutParameter =
  req
    "PutParameter"
    "fixture/PutParameter.yaml"

requestUpdateMaintenanceWindow :: UpdateMaintenanceWindow -> TestTree
requestUpdateMaintenanceWindow =
  req
    "UpdateMaintenanceWindow"
    "fixture/UpdateMaintenanceWindow.yaml"

requestListAssociations :: ListAssociations -> TestTree
requestListAssociations =
  req
    "ListAssociations"
    "fixture/ListAssociations.yaml"

requestGetMaintenanceWindowExecutionTask :: GetMaintenanceWindowExecutionTask -> TestTree
requestGetMaintenanceWindowExecutionTask =
  req
    "GetMaintenanceWindowExecutionTask"
    "fixture/GetMaintenanceWindowExecutionTask.yaml"

requestStartAutomationExecution :: StartAutomationExecution -> TestTree
requestStartAutomationExecution =
  req
    "StartAutomationExecution"
    "fixture/StartAutomationExecution.yaml"

requestDeleteActivation :: DeleteActivation -> TestTree
requestDeleteActivation =
  req
    "DeleteActivation"
    "fixture/DeleteActivation.yaml"

requestCreateMaintenanceWindow :: CreateMaintenanceWindow -> TestTree
requestCreateMaintenanceWindow =
  req
    "CreateMaintenanceWindow"
    "fixture/CreateMaintenanceWindow.yaml"

requestDescribeAssociationExecutions :: DescribeAssociationExecutions -> TestTree
requestDescribeAssociationExecutions =
  req
    "DescribeAssociationExecutions"
    "fixture/DescribeAssociationExecutions.yaml"

requestGetMaintenanceWindowExecution :: GetMaintenanceWindowExecution -> TestTree
requestGetMaintenanceWindowExecution =
  req
    "GetMaintenanceWindowExecution"
    "fixture/GetMaintenanceWindowExecution.yaml"

requestStopAutomationExecution :: StopAutomationExecution -> TestTree
requestStopAutomationExecution =
  req
    "StopAutomationExecution"
    "fixture/StopAutomationExecution.yaml"

requestGetInventorySchema :: GetInventorySchema -> TestTree
requestGetInventorySchema =
  req
    "GetInventorySchema"
    "fixture/GetInventorySchema.yaml"

requestStartAssociationsOnce :: StartAssociationsOnce -> TestTree
requestStartAssociationsOnce =
  req
    "StartAssociationsOnce"
    "fixture/StartAssociationsOnce.yaml"

requestCancelMaintenanceWindowExecution :: CancelMaintenanceWindowExecution -> TestTree
requestCancelMaintenanceWindowExecution =
  req
    "CancelMaintenanceWindowExecution"
    "fixture/CancelMaintenanceWindowExecution.yaml"

requestLabelParameterVersion :: LabelParameterVersion -> TestTree
requestLabelParameterVersion =
  req
    "LabelParameterVersion"
    "fixture/LabelParameterVersion.yaml"

requestGetParameterHistory :: GetParameterHistory -> TestTree
requestGetParameterHistory =
  req
    "GetParameterHistory"
    "fixture/GetParameterHistory.yaml"

requestGetServiceSetting :: GetServiceSetting -> TestTree
requestGetServiceSetting =
  req
    "GetServiceSetting"
    "fixture/GetServiceSetting.yaml"

requestUpdateMaintenanceWindowTask :: UpdateMaintenanceWindowTask -> TestTree
requestUpdateMaintenanceWindowTask =
  req
    "UpdateMaintenanceWindowTask"
    "fixture/UpdateMaintenanceWindowTask.yaml"

requestListDocumentMetadataHistory :: ListDocumentMetadataHistory -> TestTree
requestListDocumentMetadataHistory =
  req
    "ListDocumentMetadataHistory"
    "fixture/ListDocumentMetadataHistory.yaml"

requestListInventoryEntries :: ListInventoryEntries -> TestTree
requestListInventoryEntries =
  req
    "ListInventoryEntries"
    "fixture/ListInventoryEntries.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetParametersByPath :: GetParametersByPath -> TestTree
requestGetParametersByPath =
  req
    "GetParametersByPath"
    "fixture/GetParametersByPath.yaml"

requestDescribeActivations :: DescribeActivations -> TestTree
requestDescribeActivations =
  req
    "DescribeActivations"
    "fixture/DescribeActivations.yaml"

requestDescribeInstanceAssociationsStatus :: DescribeInstanceAssociationsStatus -> TestTree
requestDescribeInstanceAssociationsStatus =
  req
    "DescribeInstanceAssociationsStatus"
    "fixture/DescribeInstanceAssociationsStatus.yaml"

requestDescribePatchProperties :: DescribePatchProperties -> TestTree
requestDescribePatchProperties =
  req
    "DescribePatchProperties"
    "fixture/DescribePatchProperties.yaml"

requestUpdateServiceSetting :: UpdateServiceSetting -> TestTree
requestUpdateServiceSetting =
  req
    "UpdateServiceSetting"
    "fixture/UpdateServiceSetting.yaml"

requestDescribeMaintenanceWindowTasks :: DescribeMaintenanceWindowTasks -> TestTree
requestDescribeMaintenanceWindowTasks =
  req
    "DescribeMaintenanceWindowTasks"
    "fixture/DescribeMaintenanceWindowTasks.yaml"

-- Responses

responseListResourceComplianceSummaries :: ListResourceComplianceSummariesResponse -> TestTree
responseListResourceComplianceSummaries =
  res
    "ListResourceComplianceSummariesResponse"
    "fixture/ListResourceComplianceSummariesResponse.proto"
    defaultService
    (Proxy :: Proxy ListResourceComplianceSummaries)

responseDescribePatchGroups :: DescribePatchGroupsResponse -> TestTree
responseDescribePatchGroups =
  res
    "DescribePatchGroupsResponse"
    "fixture/DescribePatchGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePatchGroups)

responseDescribeOpsItems :: DescribeOpsItemsResponse -> TestTree
responseDescribeOpsItems =
  res
    "DescribeOpsItemsResponse"
    "fixture/DescribeOpsItemsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeOpsItems)

responseTerminateSession :: TerminateSessionResponse -> TestTree
responseTerminateSession =
  res
    "TerminateSessionResponse"
    "fixture/TerminateSessionResponse.proto"
    defaultService
    (Proxy :: Proxy TerminateSession)

responseGetParameter :: GetParameterResponse -> TestTree
responseGetParameter =
  res
    "GetParameterResponse"
    "fixture/GetParameterResponse.proto"
    defaultService
    (Proxy :: Proxy GetParameter)

responseGetOpsMetadata :: GetOpsMetadataResponse -> TestTree
responseGetOpsMetadata =
  res
    "GetOpsMetadataResponse"
    "fixture/GetOpsMetadataResponse.proto"
    defaultService
    (Proxy :: Proxy GetOpsMetadata)

responseUpdateDocumentDefaultVersion :: UpdateDocumentDefaultVersionResponse -> TestTree
responseUpdateDocumentDefaultVersion =
  res
    "UpdateDocumentDefaultVersionResponse"
    "fixture/UpdateDocumentDefaultVersionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDocumentDefaultVersion)

responseListResourceDataSync :: ListResourceDataSyncResponse -> TestTree
responseListResourceDataSync =
  res
    "ListResourceDataSyncResponse"
    "fixture/ListResourceDataSyncResponse.proto"
    defaultService
    (Proxy :: Proxy ListResourceDataSync)

responseDescribeParameters :: DescribeParametersResponse -> TestTree
responseDescribeParameters =
  res
    "DescribeParametersResponse"
    "fixture/DescribeParametersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeParameters)

responseRegisterTaskWithMaintenanceWindow :: RegisterTaskWithMaintenanceWindowResponse -> TestTree
responseRegisterTaskWithMaintenanceWindow =
  res
    "RegisterTaskWithMaintenanceWindowResponse"
    "fixture/RegisterTaskWithMaintenanceWindowResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterTaskWithMaintenanceWindow)

responseGetOpsItem :: GetOpsItemResponse -> TestTree
responseGetOpsItem =
  res
    "GetOpsItemResponse"
    "fixture/GetOpsItemResponse.proto"
    defaultService
    (Proxy :: Proxy GetOpsItem)

responseGetInventory :: GetInventoryResponse -> TestTree
responseGetInventory =
  res
    "GetInventoryResponse"
    "fixture/GetInventoryResponse.proto"
    defaultService
    (Proxy :: Proxy GetInventory)

responseDescribeAssociation :: DescribeAssociationResponse -> TestTree
responseDescribeAssociation =
  res
    "DescribeAssociationResponse"
    "fixture/DescribeAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAssociation)

responseUpdateAssociationStatus :: UpdateAssociationStatusResponse -> TestTree
responseUpdateAssociationStatus =
  res
    "UpdateAssociationStatusResponse"
    "fixture/UpdateAssociationStatusResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAssociationStatus)

responseDeregisterManagedInstance :: DeregisterManagedInstanceResponse -> TestTree
responseDeregisterManagedInstance =
  res
    "DeregisterManagedInstanceResponse"
    "fixture/DeregisterManagedInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterManagedInstance)

responseDescribeAssociationExecutionTargets :: DescribeAssociationExecutionTargetsResponse -> TestTree
responseDescribeAssociationExecutionTargets =
  res
    "DescribeAssociationExecutionTargetsResponse"
    "fixture/DescribeAssociationExecutionTargetsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAssociationExecutionTargets)

responseUpdateMaintenanceWindowTarget :: UpdateMaintenanceWindowTargetResponse -> TestTree
responseUpdateMaintenanceWindowTarget =
  res
    "UpdateMaintenanceWindowTargetResponse"
    "fixture/UpdateMaintenanceWindowTargetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMaintenanceWindowTarget)

responseDescribeAutomationStepExecutions :: DescribeAutomationStepExecutionsResponse -> TestTree
responseDescribeAutomationStepExecutions =
  res
    "DescribeAutomationStepExecutionsResponse"
    "fixture/DescribeAutomationStepExecutionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAutomationStepExecutions)

responseCreatePatchBaseline :: CreatePatchBaselineResponse -> TestTree
responseCreatePatchBaseline =
  res
    "CreatePatchBaselineResponse"
    "fixture/CreatePatchBaselineResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePatchBaseline)

responseGetCommandInvocation :: GetCommandInvocationResponse -> TestTree
responseGetCommandInvocation =
  res
    "GetCommandInvocationResponse"
    "fixture/GetCommandInvocationResponse.proto"
    defaultService
    (Proxy :: Proxy GetCommandInvocation)

responseListCommands :: ListCommandsResponse -> TestTree
responseListCommands =
  res
    "ListCommandsResponse"
    "fixture/ListCommandsResponse.proto"
    defaultService
    (Proxy :: Proxy ListCommands)

responseDeregisterTargetFromMaintenanceWindow :: DeregisterTargetFromMaintenanceWindowResponse -> TestTree
responseDeregisterTargetFromMaintenanceWindow =
  res
    "DeregisterTargetFromMaintenanceWindowResponse"
    "fixture/DeregisterTargetFromMaintenanceWindowResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterTargetFromMaintenanceWindow)

responseDescribeInstancePatchStates :: DescribeInstancePatchStatesResponse -> TestTree
responseDescribeInstancePatchStates =
  res
    "DescribeInstancePatchStatesResponse"
    "fixture/DescribeInstancePatchStatesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstancePatchStates)

responseUpdateDocument :: UpdateDocumentResponse -> TestTree
responseUpdateDocument =
  res
    "UpdateDocumentResponse"
    "fixture/UpdateDocumentResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDocument)

responseListDocuments :: ListDocumentsResponse -> TestTree
responseListDocuments =
  res
    "ListDocumentsResponse"
    "fixture/ListDocumentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDocuments)

responseResetServiceSetting :: ResetServiceSettingResponse -> TestTree
responseResetServiceSetting =
  res
    "ResetServiceSettingResponse"
    "fixture/ResetServiceSettingResponse.proto"
    defaultService
    (Proxy :: Proxy ResetServiceSetting)

responseStartSession :: StartSessionResponse -> TestTree
responseStartSession =
  res
    "StartSessionResponse"
    "fixture/StartSessionResponse.proto"
    defaultService
    (Proxy :: Proxy StartSession)

responseGetOpsSummary :: GetOpsSummaryResponse -> TestTree
responseGetOpsSummary =
  res
    "GetOpsSummaryResponse"
    "fixture/GetOpsSummaryResponse.proto"
    defaultService
    (Proxy :: Proxy GetOpsSummary)

responseDeleteDocument :: DeleteDocumentResponse -> TestTree
responseDeleteDocument =
  res
    "DeleteDocumentResponse"
    "fixture/DeleteDocumentResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDocument)

responseDeleteInventory :: DeleteInventoryResponse -> TestTree
responseDeleteInventory =
  res
    "DeleteInventoryResponse"
    "fixture/DeleteInventoryResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteInventory)

responseDeleteParameters :: DeleteParametersResponse -> TestTree
responseDeleteParameters =
  res
    "DeleteParametersResponse"
    "fixture/DeleteParametersResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteParameters)

responseListOpsMetadata :: ListOpsMetadataResponse -> TestTree
responseListOpsMetadata =
  res
    "ListOpsMetadataResponse"
    "fixture/ListOpsMetadataResponse.proto"
    defaultService
    (Proxy :: Proxy ListOpsMetadata)

responseSendCommand :: SendCommandResponse -> TestTree
responseSendCommand =
  res
    "SendCommandResponse"
    "fixture/SendCommandResponse.proto"
    defaultService
    (Proxy :: Proxy SendCommand)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveTagsFromResource)

responseDescribeEffectiveInstanceAssociations :: DescribeEffectiveInstanceAssociationsResponse -> TestTree
responseDescribeEffectiveInstanceAssociations =
  res
    "DescribeEffectiveInstanceAssociationsResponse"
    "fixture/DescribeEffectiveInstanceAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEffectiveInstanceAssociations)

responseCreateDocument :: CreateDocumentResponse -> TestTree
responseCreateDocument =
  res
    "CreateDocumentResponse"
    "fixture/CreateDocumentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDocument)

responseGetCalendarState :: GetCalendarStateResponse -> TestTree
responseGetCalendarState =
  res
    "GetCalendarStateResponse"
    "fixture/GetCalendarStateResponse.proto"
    defaultService
    (Proxy :: Proxy GetCalendarState)

responseRegisterTargetWithMaintenanceWindow :: RegisterTargetWithMaintenanceWindowResponse -> TestTree
responseRegisterTargetWithMaintenanceWindow =
  res
    "RegisterTargetWithMaintenanceWindowResponse"
    "fixture/RegisterTargetWithMaintenanceWindowResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterTargetWithMaintenanceWindow)

responseDescribeSessions :: DescribeSessionsResponse -> TestTree
responseDescribeSessions =
  res
    "DescribeSessionsResponse"
    "fixture/DescribeSessionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSessions)

responseDeleteOpsMetadata :: DeleteOpsMetadataResponse -> TestTree
responseDeleteOpsMetadata =
  res
    "DeleteOpsMetadataResponse"
    "fixture/DeleteOpsMetadataResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteOpsMetadata)

responseUpdateOpsItem :: UpdateOpsItemResponse -> TestTree
responseUpdateOpsItem =
  res
    "UpdateOpsItemResponse"
    "fixture/UpdateOpsItemResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateOpsItem)

responseDeleteAssociation :: DeleteAssociationResponse -> TestTree
responseDeleteAssociation =
  res
    "DeleteAssociationResponse"
    "fixture/DeleteAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAssociation)

responseUpdateAssociation :: UpdateAssociationResponse -> TestTree
responseUpdateAssociation =
  res
    "UpdateAssociationResponse"
    "fixture/UpdateAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAssociation)

responseUpdateOpsMetadata :: UpdateOpsMetadataResponse -> TestTree
responseUpdateOpsMetadata =
  res
    "UpdateOpsMetadataResponse"
    "fixture/UpdateOpsMetadataResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateOpsMetadata)

responseDescribeInstanceInformation :: DescribeInstanceInformationResponse -> TestTree
responseDescribeInstanceInformation =
  res
    "DescribeInstanceInformationResponse"
    "fixture/DescribeInstanceInformationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstanceInformation)

responseDescribeMaintenanceWindowExecutionTaskInvocations :: DescribeMaintenanceWindowExecutionTaskInvocationsResponse -> TestTree
responseDescribeMaintenanceWindowExecutionTaskInvocations =
  res
    "DescribeMaintenanceWindowExecutionTaskInvocationsResponse"
    "fixture/DescribeMaintenanceWindowExecutionTaskInvocationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMaintenanceWindowExecutionTaskInvocations)

responseDeleteParameter :: DeleteParameterResponse -> TestTree
responseDeleteParameter =
  res
    "DeleteParameterResponse"
    "fixture/DeleteParameterResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteParameter)

responseGetMaintenanceWindowExecutionTaskInvocation :: GetMaintenanceWindowExecutionTaskInvocationResponse -> TestTree
responseGetMaintenanceWindowExecutionTaskInvocation =
  res
    "GetMaintenanceWindowExecutionTaskInvocationResponse"
    "fixture/GetMaintenanceWindowExecutionTaskInvocationResponse.proto"
    defaultService
    (Proxy :: Proxy GetMaintenanceWindowExecutionTaskInvocation)

responseCreateActivation :: CreateActivationResponse -> TestTree
responseCreateActivation =
  res
    "CreateActivationResponse"
    "fixture/CreateActivationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateActivation)

responseCreateOpsMetadata :: CreateOpsMetadataResponse -> TestTree
responseCreateOpsMetadata =
  res
    "CreateOpsMetadataResponse"
    "fixture/CreateOpsMetadataResponse.proto"
    defaultService
    (Proxy :: Proxy CreateOpsMetadata)

responseSendAutomationSignal :: SendAutomationSignalResponse -> TestTree
responseSendAutomationSignal =
  res
    "SendAutomationSignalResponse"
    "fixture/SendAutomationSignalResponse.proto"
    defaultService
    (Proxy :: Proxy SendAutomationSignal)

responseListComplianceSummaries :: ListComplianceSummariesResponse -> TestTree
responseListComplianceSummaries =
  res
    "ListComplianceSummariesResponse"
    "fixture/ListComplianceSummariesResponse.proto"
    defaultService
    (Proxy :: Proxy ListComplianceSummaries)

responseDescribeMaintenanceWindowExecutions :: DescribeMaintenanceWindowExecutionsResponse -> TestTree
responseDescribeMaintenanceWindowExecutions =
  res
    "DescribeMaintenanceWindowExecutionsResponse"
    "fixture/DescribeMaintenanceWindowExecutionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMaintenanceWindowExecutions)

responseDescribeMaintenanceWindowsForTarget :: DescribeMaintenanceWindowsForTargetResponse -> TestTree
responseDescribeMaintenanceWindowsForTarget =
  res
    "DescribeMaintenanceWindowsForTargetResponse"
    "fixture/DescribeMaintenanceWindowsForTargetResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMaintenanceWindowsForTarget)

responseCreateAssociation :: CreateAssociationResponse -> TestTree
responseCreateAssociation =
  res
    "CreateAssociationResponse"
    "fixture/CreateAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAssociation)

responseCreateOpsItem :: CreateOpsItemResponse -> TestTree
responseCreateOpsItem =
  res
    "CreateOpsItemResponse"
    "fixture/CreateOpsItemResponse.proto"
    defaultService
    (Proxy :: Proxy CreateOpsItem)

responseStartChangeRequestExecution :: StartChangeRequestExecutionResponse -> TestTree
responseStartChangeRequestExecution =
  res
    "StartChangeRequestExecutionResponse"
    "fixture/StartChangeRequestExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy StartChangeRequestExecution)

responseListAssociationVersions :: ListAssociationVersionsResponse -> TestTree
responseListAssociationVersions =
  res
    "ListAssociationVersionsResponse"
    "fixture/ListAssociationVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAssociationVersions)

responseDescribeDocument :: DescribeDocumentResponse -> TestTree
responseDescribeDocument =
  res
    "DescribeDocumentResponse"
    "fixture/DescribeDocumentResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDocument)

responseDeregisterTaskFromMaintenanceWindow :: DeregisterTaskFromMaintenanceWindowResponse -> TestTree
responseDeregisterTaskFromMaintenanceWindow =
  res
    "DeregisterTaskFromMaintenanceWindowResponse"
    "fixture/DeregisterTaskFromMaintenanceWindowResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterTaskFromMaintenanceWindow)

responsePutComplianceItems :: PutComplianceItemsResponse -> TestTree
responsePutComplianceItems =
  res
    "PutComplianceItemsResponse"
    "fixture/PutComplianceItemsResponse.proto"
    defaultService
    (Proxy :: Proxy PutComplianceItems)

responseGetMaintenanceWindowTask :: GetMaintenanceWindowTaskResponse -> TestTree
responseGetMaintenanceWindowTask =
  res
    "GetMaintenanceWindowTaskResponse"
    "fixture/GetMaintenanceWindowTaskResponse.proto"
    defaultService
    (Proxy :: Proxy GetMaintenanceWindowTask)

responseGetDeployablePatchSnapshotForInstance :: GetDeployablePatchSnapshotForInstanceResponse -> TestTree
responseGetDeployablePatchSnapshotForInstance =
  res
    "GetDeployablePatchSnapshotForInstanceResponse"
    "fixture/GetDeployablePatchSnapshotForInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy GetDeployablePatchSnapshotForInstance)

responseGetMaintenanceWindow :: GetMaintenanceWindowResponse -> TestTree
responseGetMaintenanceWindow =
  res
    "GetMaintenanceWindowResponse"
    "fixture/GetMaintenanceWindowResponse.proto"
    defaultService
    (Proxy :: Proxy GetMaintenanceWindow)

responseDeregisterPatchBaselineForPatchGroup :: DeregisterPatchBaselineForPatchGroupResponse -> TestTree
responseDeregisterPatchBaselineForPatchGroup =
  res
    "DeregisterPatchBaselineForPatchGroupResponse"
    "fixture/DeregisterPatchBaselineForPatchGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterPatchBaselineForPatchGroup)

responseResumeSession :: ResumeSessionResponse -> TestTree
responseResumeSession =
  res
    "ResumeSessionResponse"
    "fixture/ResumeSessionResponse.proto"
    defaultService
    (Proxy :: Proxy ResumeSession)

responseRegisterDefaultPatchBaseline :: RegisterDefaultPatchBaselineResponse -> TestTree
responseRegisterDefaultPatchBaseline =
  res
    "RegisterDefaultPatchBaselineResponse"
    "fixture/RegisterDefaultPatchBaselineResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterDefaultPatchBaseline)

responseDescribeMaintenanceWindows :: DescribeMaintenanceWindowsResponse -> TestTree
responseDescribeMaintenanceWindows =
  res
    "DescribeMaintenanceWindowsResponse"
    "fixture/DescribeMaintenanceWindowsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMaintenanceWindows)

responseModifyDocumentPermission :: ModifyDocumentPermissionResponse -> TestTree
responseModifyDocumentPermission =
  res
    "ModifyDocumentPermissionResponse"
    "fixture/ModifyDocumentPermissionResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyDocumentPermission)

responseDescribeInstancePatches :: DescribeInstancePatchesResponse -> TestTree
responseDescribeInstancePatches =
  res
    "DescribeInstancePatchesResponse"
    "fixture/DescribeInstancePatchesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstancePatches)

responseGetParameters :: GetParametersResponse -> TestTree
responseGetParameters =
  res
    "GetParametersResponse"
    "fixture/GetParametersResponse.proto"
    defaultService
    (Proxy :: Proxy GetParameters)

responseListDocumentVersions :: ListDocumentVersionsResponse -> TestTree
responseListDocumentVersions =
  res
    "ListDocumentVersionsResponse"
    "fixture/ListDocumentVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDocumentVersions)

responseUpdateResourceDataSync :: UpdateResourceDataSyncResponse -> TestTree
responseUpdateResourceDataSync =
  res
    "UpdateResourceDataSyncResponse"
    "fixture/UpdateResourceDataSyncResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateResourceDataSync)

responseDeletePatchBaseline :: DeletePatchBaselineResponse -> TestTree
responseDeletePatchBaseline =
  res
    "DeletePatchBaselineResponse"
    "fixture/DeletePatchBaselineResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePatchBaseline)

responseListOpsItemEvents :: ListOpsItemEventsResponse -> TestTree
responseListOpsItemEvents =
  res
    "ListOpsItemEventsResponse"
    "fixture/ListOpsItemEventsResponse.proto"
    defaultService
    (Proxy :: Proxy ListOpsItemEvents)

responseDeleteResourceDataSync :: DeleteResourceDataSyncResponse -> TestTree
responseDeleteResourceDataSync =
  res
    "DeleteResourceDataSyncResponse"
    "fixture/DeleteResourceDataSyncResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteResourceDataSync)

responseDescribeInstancePatchStatesForPatchGroup :: DescribeInstancePatchStatesForPatchGroupResponse -> TestTree
responseDescribeInstancePatchStatesForPatchGroup =
  res
    "DescribeInstancePatchStatesForPatchGroupResponse"
    "fixture/DescribeInstancePatchStatesForPatchGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstancePatchStatesForPatchGroup)

responseGetConnectionStatus :: GetConnectionStatusResponse -> TestTree
responseGetConnectionStatus =
  res
    "GetConnectionStatusResponse"
    "fixture/GetConnectionStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetConnectionStatus)

responseUpdatePatchBaseline :: UpdatePatchBaselineResponse -> TestTree
responseUpdatePatchBaseline =
  res
    "UpdatePatchBaselineResponse"
    "fixture/UpdatePatchBaselineResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePatchBaseline)

responseDescribeAvailablePatches :: DescribeAvailablePatchesResponse -> TestTree
responseDescribeAvailablePatches =
  res
    "DescribeAvailablePatchesResponse"
    "fixture/DescribeAvailablePatchesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAvailablePatches)

responseListComplianceItems :: ListComplianceItemsResponse -> TestTree
responseListComplianceItems =
  res
    "ListComplianceItemsResponse"
    "fixture/ListComplianceItemsResponse.proto"
    defaultService
    (Proxy :: Proxy ListComplianceItems)

responseGetDocument :: GetDocumentResponse -> TestTree
responseGetDocument =
  res
    "GetDocumentResponse"
    "fixture/GetDocumentResponse.proto"
    defaultService
    (Proxy :: Proxy GetDocument)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    defaultService
    (Proxy :: Proxy AddTagsToResource)

responseGetPatchBaselineForPatchGroup :: GetPatchBaselineForPatchGroupResponse -> TestTree
responseGetPatchBaselineForPatchGroup =
  res
    "GetPatchBaselineForPatchGroupResponse"
    "fixture/GetPatchBaselineForPatchGroupResponse.proto"
    defaultService
    (Proxy :: Proxy GetPatchBaselineForPatchGroup)

responseUpdateManagedInstanceRole :: UpdateManagedInstanceRoleResponse -> TestTree
responseUpdateManagedInstanceRole =
  res
    "UpdateManagedInstanceRoleResponse"
    "fixture/UpdateManagedInstanceRoleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateManagedInstanceRole)

responseDescribeMaintenanceWindowSchedule :: DescribeMaintenanceWindowScheduleResponse -> TestTree
responseDescribeMaintenanceWindowSchedule =
  res
    "DescribeMaintenanceWindowScheduleResponse"
    "fixture/DescribeMaintenanceWindowScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMaintenanceWindowSchedule)

responseCreateResourceDataSync :: CreateResourceDataSyncResponse -> TestTree
responseCreateResourceDataSync =
  res
    "CreateResourceDataSyncResponse"
    "fixture/CreateResourceDataSyncResponse.proto"
    defaultService
    (Proxy :: Proxy CreateResourceDataSync)

responseCreateAssociationBatch :: CreateAssociationBatchResponse -> TestTree
responseCreateAssociationBatch =
  res
    "CreateAssociationBatchResponse"
    "fixture/CreateAssociationBatchResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAssociationBatch)

responseCancelCommand :: CancelCommandResponse -> TestTree
responseCancelCommand =
  res
    "CancelCommandResponse"
    "fixture/CancelCommandResponse.proto"
    defaultService
    (Proxy :: Proxy CancelCommand)

responseDescribeDocumentPermission :: DescribeDocumentPermissionResponse -> TestTree
responseDescribeDocumentPermission =
  res
    "DescribeDocumentPermissionResponse"
    "fixture/DescribeDocumentPermissionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDocumentPermission)

responseRegisterPatchBaselineForPatchGroup :: RegisterPatchBaselineForPatchGroupResponse -> TestTree
responseRegisterPatchBaselineForPatchGroup =
  res
    "RegisterPatchBaselineForPatchGroupResponse"
    "fixture/RegisterPatchBaselineForPatchGroupResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterPatchBaselineForPatchGroup)

responseDescribeEffectivePatchesForPatchBaseline :: DescribeEffectivePatchesForPatchBaselineResponse -> TestTree
responseDescribeEffectivePatchesForPatchBaseline =
  res
    "DescribeEffectivePatchesForPatchBaselineResponse"
    "fixture/DescribeEffectivePatchesForPatchBaselineResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEffectivePatchesForPatchBaseline)

responseListCommandInvocations :: ListCommandInvocationsResponse -> TestTree
responseListCommandInvocations =
  res
    "ListCommandInvocationsResponse"
    "fixture/ListCommandInvocationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListCommandInvocations)

responseDescribeMaintenanceWindowTargets :: DescribeMaintenanceWindowTargetsResponse -> TestTree
responseDescribeMaintenanceWindowTargets =
  res
    "DescribeMaintenanceWindowTargetsResponse"
    "fixture/DescribeMaintenanceWindowTargetsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMaintenanceWindowTargets)

responseGetAutomationExecution :: GetAutomationExecutionResponse -> TestTree
responseGetAutomationExecution =
  res
    "GetAutomationExecutionResponse"
    "fixture/GetAutomationExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy GetAutomationExecution)

responseGetPatchBaseline :: GetPatchBaselineResponse -> TestTree
responseGetPatchBaseline =
  res
    "GetPatchBaselineResponse"
    "fixture/GetPatchBaselineResponse.proto"
    defaultService
    (Proxy :: Proxy GetPatchBaseline)

responseDescribePatchGroupState :: DescribePatchGroupStateResponse -> TestTree
responseDescribePatchGroupState =
  res
    "DescribePatchGroupStateResponse"
    "fixture/DescribePatchGroupStateResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePatchGroupState)

responseDescribePatchBaselines :: DescribePatchBaselinesResponse -> TestTree
responseDescribePatchBaselines =
  res
    "DescribePatchBaselinesResponse"
    "fixture/DescribePatchBaselinesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePatchBaselines)

responseUpdateDocumentMetadata :: UpdateDocumentMetadataResponse -> TestTree
responseUpdateDocumentMetadata =
  res
    "UpdateDocumentMetadataResponse"
    "fixture/UpdateDocumentMetadataResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDocumentMetadata)

responseDescribeAutomationExecutions :: DescribeAutomationExecutionsResponse -> TestTree
responseDescribeAutomationExecutions =
  res
    "DescribeAutomationExecutionsResponse"
    "fixture/DescribeAutomationExecutionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAutomationExecutions)

responsePutInventory :: PutInventoryResponse -> TestTree
responsePutInventory =
  res
    "PutInventoryResponse"
    "fixture/PutInventoryResponse.proto"
    defaultService
    (Proxy :: Proxy PutInventory)

responseDescribeInventoryDeletions :: DescribeInventoryDeletionsResponse -> TestTree
responseDescribeInventoryDeletions =
  res
    "DescribeInventoryDeletionsResponse"
    "fixture/DescribeInventoryDeletionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInventoryDeletions)

responseDescribeMaintenanceWindowExecutionTasks :: DescribeMaintenanceWindowExecutionTasksResponse -> TestTree
responseDescribeMaintenanceWindowExecutionTasks =
  res
    "DescribeMaintenanceWindowExecutionTasksResponse"
    "fixture/DescribeMaintenanceWindowExecutionTasksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMaintenanceWindowExecutionTasks)

responseDeleteMaintenanceWindow :: DeleteMaintenanceWindowResponse -> TestTree
responseDeleteMaintenanceWindow =
  res
    "DeleteMaintenanceWindowResponse"
    "fixture/DeleteMaintenanceWindowResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMaintenanceWindow)

responseGetDefaultPatchBaseline :: GetDefaultPatchBaselineResponse -> TestTree
responseGetDefaultPatchBaseline =
  res
    "GetDefaultPatchBaselineResponse"
    "fixture/GetDefaultPatchBaselineResponse.proto"
    defaultService
    (Proxy :: Proxy GetDefaultPatchBaseline)

responsePutParameter :: PutParameterResponse -> TestTree
responsePutParameter =
  res
    "PutParameterResponse"
    "fixture/PutParameterResponse.proto"
    defaultService
    (Proxy :: Proxy PutParameter)

responseUpdateMaintenanceWindow :: UpdateMaintenanceWindowResponse -> TestTree
responseUpdateMaintenanceWindow =
  res
    "UpdateMaintenanceWindowResponse"
    "fixture/UpdateMaintenanceWindowResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMaintenanceWindow)

responseListAssociations :: ListAssociationsResponse -> TestTree
responseListAssociations =
  res
    "ListAssociationsResponse"
    "fixture/ListAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAssociations)

responseGetMaintenanceWindowExecutionTask :: GetMaintenanceWindowExecutionTaskResponse -> TestTree
responseGetMaintenanceWindowExecutionTask =
  res
    "GetMaintenanceWindowExecutionTaskResponse"
    "fixture/GetMaintenanceWindowExecutionTaskResponse.proto"
    defaultService
    (Proxy :: Proxy GetMaintenanceWindowExecutionTask)

responseStartAutomationExecution :: StartAutomationExecutionResponse -> TestTree
responseStartAutomationExecution =
  res
    "StartAutomationExecutionResponse"
    "fixture/StartAutomationExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy StartAutomationExecution)

responseDeleteActivation :: DeleteActivationResponse -> TestTree
responseDeleteActivation =
  res
    "DeleteActivationResponse"
    "fixture/DeleteActivationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteActivation)

responseCreateMaintenanceWindow :: CreateMaintenanceWindowResponse -> TestTree
responseCreateMaintenanceWindow =
  res
    "CreateMaintenanceWindowResponse"
    "fixture/CreateMaintenanceWindowResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMaintenanceWindow)

responseDescribeAssociationExecutions :: DescribeAssociationExecutionsResponse -> TestTree
responseDescribeAssociationExecutions =
  res
    "DescribeAssociationExecutionsResponse"
    "fixture/DescribeAssociationExecutionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAssociationExecutions)

responseGetMaintenanceWindowExecution :: GetMaintenanceWindowExecutionResponse -> TestTree
responseGetMaintenanceWindowExecution =
  res
    "GetMaintenanceWindowExecutionResponse"
    "fixture/GetMaintenanceWindowExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy GetMaintenanceWindowExecution)

responseStopAutomationExecution :: StopAutomationExecutionResponse -> TestTree
responseStopAutomationExecution =
  res
    "StopAutomationExecutionResponse"
    "fixture/StopAutomationExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy StopAutomationExecution)

responseGetInventorySchema :: GetInventorySchemaResponse -> TestTree
responseGetInventorySchema =
  res
    "GetInventorySchemaResponse"
    "fixture/GetInventorySchemaResponse.proto"
    defaultService
    (Proxy :: Proxy GetInventorySchema)

responseStartAssociationsOnce :: StartAssociationsOnceResponse -> TestTree
responseStartAssociationsOnce =
  res
    "StartAssociationsOnceResponse"
    "fixture/StartAssociationsOnceResponse.proto"
    defaultService
    (Proxy :: Proxy StartAssociationsOnce)

responseCancelMaintenanceWindowExecution :: CancelMaintenanceWindowExecutionResponse -> TestTree
responseCancelMaintenanceWindowExecution =
  res
    "CancelMaintenanceWindowExecutionResponse"
    "fixture/CancelMaintenanceWindowExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy CancelMaintenanceWindowExecution)

responseLabelParameterVersion :: LabelParameterVersionResponse -> TestTree
responseLabelParameterVersion =
  res
    "LabelParameterVersionResponse"
    "fixture/LabelParameterVersionResponse.proto"
    defaultService
    (Proxy :: Proxy LabelParameterVersion)

responseGetParameterHistory :: GetParameterHistoryResponse -> TestTree
responseGetParameterHistory =
  res
    "GetParameterHistoryResponse"
    "fixture/GetParameterHistoryResponse.proto"
    defaultService
    (Proxy :: Proxy GetParameterHistory)

responseGetServiceSetting :: GetServiceSettingResponse -> TestTree
responseGetServiceSetting =
  res
    "GetServiceSettingResponse"
    "fixture/GetServiceSettingResponse.proto"
    defaultService
    (Proxy :: Proxy GetServiceSetting)

responseUpdateMaintenanceWindowTask :: UpdateMaintenanceWindowTaskResponse -> TestTree
responseUpdateMaintenanceWindowTask =
  res
    "UpdateMaintenanceWindowTaskResponse"
    "fixture/UpdateMaintenanceWindowTaskResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMaintenanceWindowTask)

responseListDocumentMetadataHistory :: ListDocumentMetadataHistoryResponse -> TestTree
responseListDocumentMetadataHistory =
  res
    "ListDocumentMetadataHistoryResponse"
    "fixture/ListDocumentMetadataHistoryResponse.proto"
    defaultService
    (Proxy :: Proxy ListDocumentMetadataHistory)

responseListInventoryEntries :: ListInventoryEntriesResponse -> TestTree
responseListInventoryEntries =
  res
    "ListInventoryEntriesResponse"
    "fixture/ListInventoryEntriesResponse.proto"
    defaultService
    (Proxy :: Proxy ListInventoryEntries)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseGetParametersByPath :: GetParametersByPathResponse -> TestTree
responseGetParametersByPath =
  res
    "GetParametersByPathResponse"
    "fixture/GetParametersByPathResponse.proto"
    defaultService
    (Proxy :: Proxy GetParametersByPath)

responseDescribeActivations :: DescribeActivationsResponse -> TestTree
responseDescribeActivations =
  res
    "DescribeActivationsResponse"
    "fixture/DescribeActivationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeActivations)

responseDescribeInstanceAssociationsStatus :: DescribeInstanceAssociationsStatusResponse -> TestTree
responseDescribeInstanceAssociationsStatus =
  res
    "DescribeInstanceAssociationsStatusResponse"
    "fixture/DescribeInstanceAssociationsStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstanceAssociationsStatus)

responseDescribePatchProperties :: DescribePatchPropertiesResponse -> TestTree
responseDescribePatchProperties =
  res
    "DescribePatchPropertiesResponse"
    "fixture/DescribePatchPropertiesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePatchProperties)

responseUpdateServiceSetting :: UpdateServiceSettingResponse -> TestTree
responseUpdateServiceSetting =
  res
    "UpdateServiceSettingResponse"
    "fixture/UpdateServiceSettingResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateServiceSetting)

responseDescribeMaintenanceWindowTasks :: DescribeMaintenanceWindowTasksResponse -> TestTree
responseDescribeMaintenanceWindowTasks =
  res
    "DescribeMaintenanceWindowTasksResponse"
    "fixture/DescribeMaintenanceWindowTasksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMaintenanceWindowTasks)
