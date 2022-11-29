{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SSM
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.SSM where

import Amazonka.SSM
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.SSM.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAddTagsToResource $
--             newAddTagsToResource
--
--         , requestAssociateOpsItemRelatedItem $
--             newAssociateOpsItemRelatedItem
--
--         , requestCancelCommand $
--             newCancelCommand
--
--         , requestCancelMaintenanceWindowExecution $
--             newCancelMaintenanceWindowExecution
--
--         , requestCreateActivation $
--             newCreateActivation
--
--         , requestCreateAssociation $
--             newCreateAssociation
--
--         , requestCreateAssociationBatch $
--             newCreateAssociationBatch
--
--         , requestCreateDocument $
--             newCreateDocument
--
--         , requestCreateMaintenanceWindow $
--             newCreateMaintenanceWindow
--
--         , requestCreateOpsItem $
--             newCreateOpsItem
--
--         , requestCreateOpsMetadata $
--             newCreateOpsMetadata
--
--         , requestCreatePatchBaseline $
--             newCreatePatchBaseline
--
--         , requestCreateResourceDataSync $
--             newCreateResourceDataSync
--
--         , requestDeleteActivation $
--             newDeleteActivation
--
--         , requestDeleteAssociation $
--             newDeleteAssociation
--
--         , requestDeleteDocument $
--             newDeleteDocument
--
--         , requestDeleteInventory $
--             newDeleteInventory
--
--         , requestDeleteMaintenanceWindow $
--             newDeleteMaintenanceWindow
--
--         , requestDeleteOpsMetadata $
--             newDeleteOpsMetadata
--
--         , requestDeleteParameter $
--             newDeleteParameter
--
--         , requestDeleteParameters $
--             newDeleteParameters
--
--         , requestDeletePatchBaseline $
--             newDeletePatchBaseline
--
--         , requestDeleteResourceDataSync $
--             newDeleteResourceDataSync
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestDeregisterManagedInstance $
--             newDeregisterManagedInstance
--
--         , requestDeregisterPatchBaselineForPatchGroup $
--             newDeregisterPatchBaselineForPatchGroup
--
--         , requestDeregisterTargetFromMaintenanceWindow $
--             newDeregisterTargetFromMaintenanceWindow
--
--         , requestDeregisterTaskFromMaintenanceWindow $
--             newDeregisterTaskFromMaintenanceWindow
--
--         , requestDescribeActivations $
--             newDescribeActivations
--
--         , requestDescribeAssociation $
--             newDescribeAssociation
--
--         , requestDescribeAssociationExecutionTargets $
--             newDescribeAssociationExecutionTargets
--
--         , requestDescribeAssociationExecutions $
--             newDescribeAssociationExecutions
--
--         , requestDescribeAutomationExecutions $
--             newDescribeAutomationExecutions
--
--         , requestDescribeAutomationStepExecutions $
--             newDescribeAutomationStepExecutions
--
--         , requestDescribeAvailablePatches $
--             newDescribeAvailablePatches
--
--         , requestDescribeDocument $
--             newDescribeDocument
--
--         , requestDescribeDocumentPermission $
--             newDescribeDocumentPermission
--
--         , requestDescribeEffectiveInstanceAssociations $
--             newDescribeEffectiveInstanceAssociations
--
--         , requestDescribeEffectivePatchesForPatchBaseline $
--             newDescribeEffectivePatchesForPatchBaseline
--
--         , requestDescribeInstanceAssociationsStatus $
--             newDescribeInstanceAssociationsStatus
--
--         , requestDescribeInstanceInformation $
--             newDescribeInstanceInformation
--
--         , requestDescribeInstancePatchStates $
--             newDescribeInstancePatchStates
--
--         , requestDescribeInstancePatchStatesForPatchGroup $
--             newDescribeInstancePatchStatesForPatchGroup
--
--         , requestDescribeInstancePatches $
--             newDescribeInstancePatches
--
--         , requestDescribeInventoryDeletions $
--             newDescribeInventoryDeletions
--
--         , requestDescribeMaintenanceWindowExecutionTaskInvocations $
--             newDescribeMaintenanceWindowExecutionTaskInvocations
--
--         , requestDescribeMaintenanceWindowExecutionTasks $
--             newDescribeMaintenanceWindowExecutionTasks
--
--         , requestDescribeMaintenanceWindowExecutions $
--             newDescribeMaintenanceWindowExecutions
--
--         , requestDescribeMaintenanceWindowSchedule $
--             newDescribeMaintenanceWindowSchedule
--
--         , requestDescribeMaintenanceWindowTargets $
--             newDescribeMaintenanceWindowTargets
--
--         , requestDescribeMaintenanceWindowTasks $
--             newDescribeMaintenanceWindowTasks
--
--         , requestDescribeMaintenanceWindows $
--             newDescribeMaintenanceWindows
--
--         , requestDescribeMaintenanceWindowsForTarget $
--             newDescribeMaintenanceWindowsForTarget
--
--         , requestDescribeOpsItems $
--             newDescribeOpsItems
--
--         , requestDescribeParameters $
--             newDescribeParameters
--
--         , requestDescribePatchBaselines $
--             newDescribePatchBaselines
--
--         , requestDescribePatchGroupState $
--             newDescribePatchGroupState
--
--         , requestDescribePatchGroups $
--             newDescribePatchGroups
--
--         , requestDescribePatchProperties $
--             newDescribePatchProperties
--
--         , requestDescribeSessions $
--             newDescribeSessions
--
--         , requestDisassociateOpsItemRelatedItem $
--             newDisassociateOpsItemRelatedItem
--
--         , requestGetAutomationExecution $
--             newGetAutomationExecution
--
--         , requestGetCalendarState $
--             newGetCalendarState
--
--         , requestGetCommandInvocation $
--             newGetCommandInvocation
--
--         , requestGetConnectionStatus $
--             newGetConnectionStatus
--
--         , requestGetDefaultPatchBaseline $
--             newGetDefaultPatchBaseline
--
--         , requestGetDeployablePatchSnapshotForInstance $
--             newGetDeployablePatchSnapshotForInstance
--
--         , requestGetDocument $
--             newGetDocument
--
--         , requestGetInventory $
--             newGetInventory
--
--         , requestGetInventorySchema $
--             newGetInventorySchema
--
--         , requestGetMaintenanceWindow $
--             newGetMaintenanceWindow
--
--         , requestGetMaintenanceWindowExecution $
--             newGetMaintenanceWindowExecution
--
--         , requestGetMaintenanceWindowExecutionTask $
--             newGetMaintenanceWindowExecutionTask
--
--         , requestGetMaintenanceWindowExecutionTaskInvocation $
--             newGetMaintenanceWindowExecutionTaskInvocation
--
--         , requestGetMaintenanceWindowTask $
--             newGetMaintenanceWindowTask
--
--         , requestGetOpsItem $
--             newGetOpsItem
--
--         , requestGetOpsMetadata $
--             newGetOpsMetadata
--
--         , requestGetOpsSummary $
--             newGetOpsSummary
--
--         , requestGetParameter $
--             newGetParameter
--
--         , requestGetParameterHistory $
--             newGetParameterHistory
--
--         , requestGetParameters $
--             newGetParameters
--
--         , requestGetParametersByPath $
--             newGetParametersByPath
--
--         , requestGetPatchBaseline $
--             newGetPatchBaseline
--
--         , requestGetPatchBaselineForPatchGroup $
--             newGetPatchBaselineForPatchGroup
--
--         , requestGetResourcePolicies $
--             newGetResourcePolicies
--
--         , requestGetServiceSetting $
--             newGetServiceSetting
--
--         , requestLabelParameterVersion $
--             newLabelParameterVersion
--
--         , requestListAssociationVersions $
--             newListAssociationVersions
--
--         , requestListAssociations $
--             newListAssociations
--
--         , requestListCommandInvocations $
--             newListCommandInvocations
--
--         , requestListCommands $
--             newListCommands
--
--         , requestListComplianceItems $
--             newListComplianceItems
--
--         , requestListComplianceSummaries $
--             newListComplianceSummaries
--
--         , requestListDocumentMetadataHistory $
--             newListDocumentMetadataHistory
--
--         , requestListDocumentVersions $
--             newListDocumentVersions
--
--         , requestListDocuments $
--             newListDocuments
--
--         , requestListInventoryEntries $
--             newListInventoryEntries
--
--         , requestListOpsItemEvents $
--             newListOpsItemEvents
--
--         , requestListOpsItemRelatedItems $
--             newListOpsItemRelatedItems
--
--         , requestListOpsMetadata $
--             newListOpsMetadata
--
--         , requestListResourceComplianceSummaries $
--             newListResourceComplianceSummaries
--
--         , requestListResourceDataSync $
--             newListResourceDataSync
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestModifyDocumentPermission $
--             newModifyDocumentPermission
--
--         , requestPutComplianceItems $
--             newPutComplianceItems
--
--         , requestPutInventory $
--             newPutInventory
--
--         , requestPutParameter $
--             newPutParameter
--
--         , requestPutResourcePolicy $
--             newPutResourcePolicy
--
--         , requestRegisterDefaultPatchBaseline $
--             newRegisterDefaultPatchBaseline
--
--         , requestRegisterPatchBaselineForPatchGroup $
--             newRegisterPatchBaselineForPatchGroup
--
--         , requestRegisterTargetWithMaintenanceWindow $
--             newRegisterTargetWithMaintenanceWindow
--
--         , requestRegisterTaskWithMaintenanceWindow $
--             newRegisterTaskWithMaintenanceWindow
--
--         , requestRemoveTagsFromResource $
--             newRemoveTagsFromResource
--
--         , requestResetServiceSetting $
--             newResetServiceSetting
--
--         , requestResumeSession $
--             newResumeSession
--
--         , requestSendAutomationSignal $
--             newSendAutomationSignal
--
--         , requestSendCommand $
--             newSendCommand
--
--         , requestStartAssociationsOnce $
--             newStartAssociationsOnce
--
--         , requestStartAutomationExecution $
--             newStartAutomationExecution
--
--         , requestStartChangeRequestExecution $
--             newStartChangeRequestExecution
--
--         , requestStartSession $
--             newStartSession
--
--         , requestStopAutomationExecution $
--             newStopAutomationExecution
--
--         , requestTerminateSession $
--             newTerminateSession
--
--         , requestUnlabelParameterVersion $
--             newUnlabelParameterVersion
--
--         , requestUpdateAssociation $
--             newUpdateAssociation
--
--         , requestUpdateAssociationStatus $
--             newUpdateAssociationStatus
--
--         , requestUpdateDocument $
--             newUpdateDocument
--
--         , requestUpdateDocumentDefaultVersion $
--             newUpdateDocumentDefaultVersion
--
--         , requestUpdateDocumentMetadata $
--             newUpdateDocumentMetadata
--
--         , requestUpdateMaintenanceWindow $
--             newUpdateMaintenanceWindow
--
--         , requestUpdateMaintenanceWindowTarget $
--             newUpdateMaintenanceWindowTarget
--
--         , requestUpdateMaintenanceWindowTask $
--             newUpdateMaintenanceWindowTask
--
--         , requestUpdateManagedInstanceRole $
--             newUpdateManagedInstanceRole
--
--         , requestUpdateOpsItem $
--             newUpdateOpsItem
--
--         , requestUpdateOpsMetadata $
--             newUpdateOpsMetadata
--
--         , requestUpdatePatchBaseline $
--             newUpdatePatchBaseline
--
--         , requestUpdateResourceDataSync $
--             newUpdateResourceDataSync
--
--         , requestUpdateServiceSetting $
--             newUpdateServiceSetting
--
--           ]

--     , testGroup "response"
--         [ responseAddTagsToResource $
--             newAddTagsToResourceResponse
--
--         , responseAssociateOpsItemRelatedItem $
--             newAssociateOpsItemRelatedItemResponse
--
--         , responseCancelCommand $
--             newCancelCommandResponse
--
--         , responseCancelMaintenanceWindowExecution $
--             newCancelMaintenanceWindowExecutionResponse
--
--         , responseCreateActivation $
--             newCreateActivationResponse
--
--         , responseCreateAssociation $
--             newCreateAssociationResponse
--
--         , responseCreateAssociationBatch $
--             newCreateAssociationBatchResponse
--
--         , responseCreateDocument $
--             newCreateDocumentResponse
--
--         , responseCreateMaintenanceWindow $
--             newCreateMaintenanceWindowResponse
--
--         , responseCreateOpsItem $
--             newCreateOpsItemResponse
--
--         , responseCreateOpsMetadata $
--             newCreateOpsMetadataResponse
--
--         , responseCreatePatchBaseline $
--             newCreatePatchBaselineResponse
--
--         , responseCreateResourceDataSync $
--             newCreateResourceDataSyncResponse
--
--         , responseDeleteActivation $
--             newDeleteActivationResponse
--
--         , responseDeleteAssociation $
--             newDeleteAssociationResponse
--
--         , responseDeleteDocument $
--             newDeleteDocumentResponse
--
--         , responseDeleteInventory $
--             newDeleteInventoryResponse
--
--         , responseDeleteMaintenanceWindow $
--             newDeleteMaintenanceWindowResponse
--
--         , responseDeleteOpsMetadata $
--             newDeleteOpsMetadataResponse
--
--         , responseDeleteParameter $
--             newDeleteParameterResponse
--
--         , responseDeleteParameters $
--             newDeleteParametersResponse
--
--         , responseDeletePatchBaseline $
--             newDeletePatchBaselineResponse
--
--         , responseDeleteResourceDataSync $
--             newDeleteResourceDataSyncResponse
--
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseDeregisterManagedInstance $
--             newDeregisterManagedInstanceResponse
--
--         , responseDeregisterPatchBaselineForPatchGroup $
--             newDeregisterPatchBaselineForPatchGroupResponse
--
--         , responseDeregisterTargetFromMaintenanceWindow $
--             newDeregisterTargetFromMaintenanceWindowResponse
--
--         , responseDeregisterTaskFromMaintenanceWindow $
--             newDeregisterTaskFromMaintenanceWindowResponse
--
--         , responseDescribeActivations $
--             newDescribeActivationsResponse
--
--         , responseDescribeAssociation $
--             newDescribeAssociationResponse
--
--         , responseDescribeAssociationExecutionTargets $
--             newDescribeAssociationExecutionTargetsResponse
--
--         , responseDescribeAssociationExecutions $
--             newDescribeAssociationExecutionsResponse
--
--         , responseDescribeAutomationExecutions $
--             newDescribeAutomationExecutionsResponse
--
--         , responseDescribeAutomationStepExecutions $
--             newDescribeAutomationStepExecutionsResponse
--
--         , responseDescribeAvailablePatches $
--             newDescribeAvailablePatchesResponse
--
--         , responseDescribeDocument $
--             newDescribeDocumentResponse
--
--         , responseDescribeDocumentPermission $
--             newDescribeDocumentPermissionResponse
--
--         , responseDescribeEffectiveInstanceAssociations $
--             newDescribeEffectiveInstanceAssociationsResponse
--
--         , responseDescribeEffectivePatchesForPatchBaseline $
--             newDescribeEffectivePatchesForPatchBaselineResponse
--
--         , responseDescribeInstanceAssociationsStatus $
--             newDescribeInstanceAssociationsStatusResponse
--
--         , responseDescribeInstanceInformation $
--             newDescribeInstanceInformationResponse
--
--         , responseDescribeInstancePatchStates $
--             newDescribeInstancePatchStatesResponse
--
--         , responseDescribeInstancePatchStatesForPatchGroup $
--             newDescribeInstancePatchStatesForPatchGroupResponse
--
--         , responseDescribeInstancePatches $
--             newDescribeInstancePatchesResponse
--
--         , responseDescribeInventoryDeletions $
--             newDescribeInventoryDeletionsResponse
--
--         , responseDescribeMaintenanceWindowExecutionTaskInvocations $
--             newDescribeMaintenanceWindowExecutionTaskInvocationsResponse
--
--         , responseDescribeMaintenanceWindowExecutionTasks $
--             newDescribeMaintenanceWindowExecutionTasksResponse
--
--         , responseDescribeMaintenanceWindowExecutions $
--             newDescribeMaintenanceWindowExecutionsResponse
--
--         , responseDescribeMaintenanceWindowSchedule $
--             newDescribeMaintenanceWindowScheduleResponse
--
--         , responseDescribeMaintenanceWindowTargets $
--             newDescribeMaintenanceWindowTargetsResponse
--
--         , responseDescribeMaintenanceWindowTasks $
--             newDescribeMaintenanceWindowTasksResponse
--
--         , responseDescribeMaintenanceWindows $
--             newDescribeMaintenanceWindowsResponse
--
--         , responseDescribeMaintenanceWindowsForTarget $
--             newDescribeMaintenanceWindowsForTargetResponse
--
--         , responseDescribeOpsItems $
--             newDescribeOpsItemsResponse
--
--         , responseDescribeParameters $
--             newDescribeParametersResponse
--
--         , responseDescribePatchBaselines $
--             newDescribePatchBaselinesResponse
--
--         , responseDescribePatchGroupState $
--             newDescribePatchGroupStateResponse
--
--         , responseDescribePatchGroups $
--             newDescribePatchGroupsResponse
--
--         , responseDescribePatchProperties $
--             newDescribePatchPropertiesResponse
--
--         , responseDescribeSessions $
--             newDescribeSessionsResponse
--
--         , responseDisassociateOpsItemRelatedItem $
--             newDisassociateOpsItemRelatedItemResponse
--
--         , responseGetAutomationExecution $
--             newGetAutomationExecutionResponse
--
--         , responseGetCalendarState $
--             newGetCalendarStateResponse
--
--         , responseGetCommandInvocation $
--             newGetCommandInvocationResponse
--
--         , responseGetConnectionStatus $
--             newGetConnectionStatusResponse
--
--         , responseGetDefaultPatchBaseline $
--             newGetDefaultPatchBaselineResponse
--
--         , responseGetDeployablePatchSnapshotForInstance $
--             newGetDeployablePatchSnapshotForInstanceResponse
--
--         , responseGetDocument $
--             newGetDocumentResponse
--
--         , responseGetInventory $
--             newGetInventoryResponse
--
--         , responseGetInventorySchema $
--             newGetInventorySchemaResponse
--
--         , responseGetMaintenanceWindow $
--             newGetMaintenanceWindowResponse
--
--         , responseGetMaintenanceWindowExecution $
--             newGetMaintenanceWindowExecutionResponse
--
--         , responseGetMaintenanceWindowExecutionTask $
--             newGetMaintenanceWindowExecutionTaskResponse
--
--         , responseGetMaintenanceWindowExecutionTaskInvocation $
--             newGetMaintenanceWindowExecutionTaskInvocationResponse
--
--         , responseGetMaintenanceWindowTask $
--             newGetMaintenanceWindowTaskResponse
--
--         , responseGetOpsItem $
--             newGetOpsItemResponse
--
--         , responseGetOpsMetadata $
--             newGetOpsMetadataResponse
--
--         , responseGetOpsSummary $
--             newGetOpsSummaryResponse
--
--         , responseGetParameter $
--             newGetParameterResponse
--
--         , responseGetParameterHistory $
--             newGetParameterHistoryResponse
--
--         , responseGetParameters $
--             newGetParametersResponse
--
--         , responseGetParametersByPath $
--             newGetParametersByPathResponse
--
--         , responseGetPatchBaseline $
--             newGetPatchBaselineResponse
--
--         , responseGetPatchBaselineForPatchGroup $
--             newGetPatchBaselineForPatchGroupResponse
--
--         , responseGetResourcePolicies $
--             newGetResourcePoliciesResponse
--
--         , responseGetServiceSetting $
--             newGetServiceSettingResponse
--
--         , responseLabelParameterVersion $
--             newLabelParameterVersionResponse
--
--         , responseListAssociationVersions $
--             newListAssociationVersionsResponse
--
--         , responseListAssociations $
--             newListAssociationsResponse
--
--         , responseListCommandInvocations $
--             newListCommandInvocationsResponse
--
--         , responseListCommands $
--             newListCommandsResponse
--
--         , responseListComplianceItems $
--             newListComplianceItemsResponse
--
--         , responseListComplianceSummaries $
--             newListComplianceSummariesResponse
--
--         , responseListDocumentMetadataHistory $
--             newListDocumentMetadataHistoryResponse
--
--         , responseListDocumentVersions $
--             newListDocumentVersionsResponse
--
--         , responseListDocuments $
--             newListDocumentsResponse
--
--         , responseListInventoryEntries $
--             newListInventoryEntriesResponse
--
--         , responseListOpsItemEvents $
--             newListOpsItemEventsResponse
--
--         , responseListOpsItemRelatedItems $
--             newListOpsItemRelatedItemsResponse
--
--         , responseListOpsMetadata $
--             newListOpsMetadataResponse
--
--         , responseListResourceComplianceSummaries $
--             newListResourceComplianceSummariesResponse
--
--         , responseListResourceDataSync $
--             newListResourceDataSyncResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseModifyDocumentPermission $
--             newModifyDocumentPermissionResponse
--
--         , responsePutComplianceItems $
--             newPutComplianceItemsResponse
--
--         , responsePutInventory $
--             newPutInventoryResponse
--
--         , responsePutParameter $
--             newPutParameterResponse
--
--         , responsePutResourcePolicy $
--             newPutResourcePolicyResponse
--
--         , responseRegisterDefaultPatchBaseline $
--             newRegisterDefaultPatchBaselineResponse
--
--         , responseRegisterPatchBaselineForPatchGroup $
--             newRegisterPatchBaselineForPatchGroupResponse
--
--         , responseRegisterTargetWithMaintenanceWindow $
--             newRegisterTargetWithMaintenanceWindowResponse
--
--         , responseRegisterTaskWithMaintenanceWindow $
--             newRegisterTaskWithMaintenanceWindowResponse
--
--         , responseRemoveTagsFromResource $
--             newRemoveTagsFromResourceResponse
--
--         , responseResetServiceSetting $
--             newResetServiceSettingResponse
--
--         , responseResumeSession $
--             newResumeSessionResponse
--
--         , responseSendAutomationSignal $
--             newSendAutomationSignalResponse
--
--         , responseSendCommand $
--             newSendCommandResponse
--
--         , responseStartAssociationsOnce $
--             newStartAssociationsOnceResponse
--
--         , responseStartAutomationExecution $
--             newStartAutomationExecutionResponse
--
--         , responseStartChangeRequestExecution $
--             newStartChangeRequestExecutionResponse
--
--         , responseStartSession $
--             newStartSessionResponse
--
--         , responseStopAutomationExecution $
--             newStopAutomationExecutionResponse
--
--         , responseTerminateSession $
--             newTerminateSessionResponse
--
--         , responseUnlabelParameterVersion $
--             newUnlabelParameterVersionResponse
--
--         , responseUpdateAssociation $
--             newUpdateAssociationResponse
--
--         , responseUpdateAssociationStatus $
--             newUpdateAssociationStatusResponse
--
--         , responseUpdateDocument $
--             newUpdateDocumentResponse
--
--         , responseUpdateDocumentDefaultVersion $
--             newUpdateDocumentDefaultVersionResponse
--
--         , responseUpdateDocumentMetadata $
--             newUpdateDocumentMetadataResponse
--
--         , responseUpdateMaintenanceWindow $
--             newUpdateMaintenanceWindowResponse
--
--         , responseUpdateMaintenanceWindowTarget $
--             newUpdateMaintenanceWindowTargetResponse
--
--         , responseUpdateMaintenanceWindowTask $
--             newUpdateMaintenanceWindowTaskResponse
--
--         , responseUpdateManagedInstanceRole $
--             newUpdateManagedInstanceRoleResponse
--
--         , responseUpdateOpsItem $
--             newUpdateOpsItemResponse
--
--         , responseUpdateOpsMetadata $
--             newUpdateOpsMetadataResponse
--
--         , responseUpdatePatchBaseline $
--             newUpdatePatchBaselineResponse
--
--         , responseUpdateResourceDataSync $
--             newUpdateResourceDataSyncResponse
--
--         , responseUpdateServiceSetting $
--             newUpdateServiceSettingResponse
--
--           ]
--     ]

-- Requests

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource =
  req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestAssociateOpsItemRelatedItem :: AssociateOpsItemRelatedItem -> TestTree
requestAssociateOpsItemRelatedItem =
  req
    "AssociateOpsItemRelatedItem"
    "fixture/AssociateOpsItemRelatedItem.yaml"

requestCancelCommand :: CancelCommand -> TestTree
requestCancelCommand =
  req
    "CancelCommand"
    "fixture/CancelCommand.yaml"

requestCancelMaintenanceWindowExecution :: CancelMaintenanceWindowExecution -> TestTree
requestCancelMaintenanceWindowExecution =
  req
    "CancelMaintenanceWindowExecution"
    "fixture/CancelMaintenanceWindowExecution.yaml"

requestCreateActivation :: CreateActivation -> TestTree
requestCreateActivation =
  req
    "CreateActivation"
    "fixture/CreateActivation.yaml"

requestCreateAssociation :: CreateAssociation -> TestTree
requestCreateAssociation =
  req
    "CreateAssociation"
    "fixture/CreateAssociation.yaml"

requestCreateAssociationBatch :: CreateAssociationBatch -> TestTree
requestCreateAssociationBatch =
  req
    "CreateAssociationBatch"
    "fixture/CreateAssociationBatch.yaml"

requestCreateDocument :: CreateDocument -> TestTree
requestCreateDocument =
  req
    "CreateDocument"
    "fixture/CreateDocument.yaml"

requestCreateMaintenanceWindow :: CreateMaintenanceWindow -> TestTree
requestCreateMaintenanceWindow =
  req
    "CreateMaintenanceWindow"
    "fixture/CreateMaintenanceWindow.yaml"

requestCreateOpsItem :: CreateOpsItem -> TestTree
requestCreateOpsItem =
  req
    "CreateOpsItem"
    "fixture/CreateOpsItem.yaml"

requestCreateOpsMetadata :: CreateOpsMetadata -> TestTree
requestCreateOpsMetadata =
  req
    "CreateOpsMetadata"
    "fixture/CreateOpsMetadata.yaml"

requestCreatePatchBaseline :: CreatePatchBaseline -> TestTree
requestCreatePatchBaseline =
  req
    "CreatePatchBaseline"
    "fixture/CreatePatchBaseline.yaml"

requestCreateResourceDataSync :: CreateResourceDataSync -> TestTree
requestCreateResourceDataSync =
  req
    "CreateResourceDataSync"
    "fixture/CreateResourceDataSync.yaml"

requestDeleteActivation :: DeleteActivation -> TestTree
requestDeleteActivation =
  req
    "DeleteActivation"
    "fixture/DeleteActivation.yaml"

requestDeleteAssociation :: DeleteAssociation -> TestTree
requestDeleteAssociation =
  req
    "DeleteAssociation"
    "fixture/DeleteAssociation.yaml"

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

requestDeleteMaintenanceWindow :: DeleteMaintenanceWindow -> TestTree
requestDeleteMaintenanceWindow =
  req
    "DeleteMaintenanceWindow"
    "fixture/DeleteMaintenanceWindow.yaml"

requestDeleteOpsMetadata :: DeleteOpsMetadata -> TestTree
requestDeleteOpsMetadata =
  req
    "DeleteOpsMetadata"
    "fixture/DeleteOpsMetadata.yaml"

requestDeleteParameter :: DeleteParameter -> TestTree
requestDeleteParameter =
  req
    "DeleteParameter"
    "fixture/DeleteParameter.yaml"

requestDeleteParameters :: DeleteParameters -> TestTree
requestDeleteParameters =
  req
    "DeleteParameters"
    "fixture/DeleteParameters.yaml"

requestDeletePatchBaseline :: DeletePatchBaseline -> TestTree
requestDeletePatchBaseline =
  req
    "DeletePatchBaseline"
    "fixture/DeletePatchBaseline.yaml"

requestDeleteResourceDataSync :: DeleteResourceDataSync -> TestTree
requestDeleteResourceDataSync =
  req
    "DeleteResourceDataSync"
    "fixture/DeleteResourceDataSync.yaml"

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy =
  req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

requestDeregisterManagedInstance :: DeregisterManagedInstance -> TestTree
requestDeregisterManagedInstance =
  req
    "DeregisterManagedInstance"
    "fixture/DeregisterManagedInstance.yaml"

requestDeregisterPatchBaselineForPatchGroup :: DeregisterPatchBaselineForPatchGroup -> TestTree
requestDeregisterPatchBaselineForPatchGroup =
  req
    "DeregisterPatchBaselineForPatchGroup"
    "fixture/DeregisterPatchBaselineForPatchGroup.yaml"

requestDeregisterTargetFromMaintenanceWindow :: DeregisterTargetFromMaintenanceWindow -> TestTree
requestDeregisterTargetFromMaintenanceWindow =
  req
    "DeregisterTargetFromMaintenanceWindow"
    "fixture/DeregisterTargetFromMaintenanceWindow.yaml"

requestDeregisterTaskFromMaintenanceWindow :: DeregisterTaskFromMaintenanceWindow -> TestTree
requestDeregisterTaskFromMaintenanceWindow =
  req
    "DeregisterTaskFromMaintenanceWindow"
    "fixture/DeregisterTaskFromMaintenanceWindow.yaml"

requestDescribeActivations :: DescribeActivations -> TestTree
requestDescribeActivations =
  req
    "DescribeActivations"
    "fixture/DescribeActivations.yaml"

requestDescribeAssociation :: DescribeAssociation -> TestTree
requestDescribeAssociation =
  req
    "DescribeAssociation"
    "fixture/DescribeAssociation.yaml"

requestDescribeAssociationExecutionTargets :: DescribeAssociationExecutionTargets -> TestTree
requestDescribeAssociationExecutionTargets =
  req
    "DescribeAssociationExecutionTargets"
    "fixture/DescribeAssociationExecutionTargets.yaml"

requestDescribeAssociationExecutions :: DescribeAssociationExecutions -> TestTree
requestDescribeAssociationExecutions =
  req
    "DescribeAssociationExecutions"
    "fixture/DescribeAssociationExecutions.yaml"

requestDescribeAutomationExecutions :: DescribeAutomationExecutions -> TestTree
requestDescribeAutomationExecutions =
  req
    "DescribeAutomationExecutions"
    "fixture/DescribeAutomationExecutions.yaml"

requestDescribeAutomationStepExecutions :: DescribeAutomationStepExecutions -> TestTree
requestDescribeAutomationStepExecutions =
  req
    "DescribeAutomationStepExecutions"
    "fixture/DescribeAutomationStepExecutions.yaml"

requestDescribeAvailablePatches :: DescribeAvailablePatches -> TestTree
requestDescribeAvailablePatches =
  req
    "DescribeAvailablePatches"
    "fixture/DescribeAvailablePatches.yaml"

requestDescribeDocument :: DescribeDocument -> TestTree
requestDescribeDocument =
  req
    "DescribeDocument"
    "fixture/DescribeDocument.yaml"

requestDescribeDocumentPermission :: DescribeDocumentPermission -> TestTree
requestDescribeDocumentPermission =
  req
    "DescribeDocumentPermission"
    "fixture/DescribeDocumentPermission.yaml"

requestDescribeEffectiveInstanceAssociations :: DescribeEffectiveInstanceAssociations -> TestTree
requestDescribeEffectiveInstanceAssociations =
  req
    "DescribeEffectiveInstanceAssociations"
    "fixture/DescribeEffectiveInstanceAssociations.yaml"

requestDescribeEffectivePatchesForPatchBaseline :: DescribeEffectivePatchesForPatchBaseline -> TestTree
requestDescribeEffectivePatchesForPatchBaseline =
  req
    "DescribeEffectivePatchesForPatchBaseline"
    "fixture/DescribeEffectivePatchesForPatchBaseline.yaml"

requestDescribeInstanceAssociationsStatus :: DescribeInstanceAssociationsStatus -> TestTree
requestDescribeInstanceAssociationsStatus =
  req
    "DescribeInstanceAssociationsStatus"
    "fixture/DescribeInstanceAssociationsStatus.yaml"

requestDescribeInstanceInformation :: DescribeInstanceInformation -> TestTree
requestDescribeInstanceInformation =
  req
    "DescribeInstanceInformation"
    "fixture/DescribeInstanceInformation.yaml"

requestDescribeInstancePatchStates :: DescribeInstancePatchStates -> TestTree
requestDescribeInstancePatchStates =
  req
    "DescribeInstancePatchStates"
    "fixture/DescribeInstancePatchStates.yaml"

requestDescribeInstancePatchStatesForPatchGroup :: DescribeInstancePatchStatesForPatchGroup -> TestTree
requestDescribeInstancePatchStatesForPatchGroup =
  req
    "DescribeInstancePatchStatesForPatchGroup"
    "fixture/DescribeInstancePatchStatesForPatchGroup.yaml"

requestDescribeInstancePatches :: DescribeInstancePatches -> TestTree
requestDescribeInstancePatches =
  req
    "DescribeInstancePatches"
    "fixture/DescribeInstancePatches.yaml"

requestDescribeInventoryDeletions :: DescribeInventoryDeletions -> TestTree
requestDescribeInventoryDeletions =
  req
    "DescribeInventoryDeletions"
    "fixture/DescribeInventoryDeletions.yaml"

requestDescribeMaintenanceWindowExecutionTaskInvocations :: DescribeMaintenanceWindowExecutionTaskInvocations -> TestTree
requestDescribeMaintenanceWindowExecutionTaskInvocations =
  req
    "DescribeMaintenanceWindowExecutionTaskInvocations"
    "fixture/DescribeMaintenanceWindowExecutionTaskInvocations.yaml"

requestDescribeMaintenanceWindowExecutionTasks :: DescribeMaintenanceWindowExecutionTasks -> TestTree
requestDescribeMaintenanceWindowExecutionTasks =
  req
    "DescribeMaintenanceWindowExecutionTasks"
    "fixture/DescribeMaintenanceWindowExecutionTasks.yaml"

requestDescribeMaintenanceWindowExecutions :: DescribeMaintenanceWindowExecutions -> TestTree
requestDescribeMaintenanceWindowExecutions =
  req
    "DescribeMaintenanceWindowExecutions"
    "fixture/DescribeMaintenanceWindowExecutions.yaml"

requestDescribeMaintenanceWindowSchedule :: DescribeMaintenanceWindowSchedule -> TestTree
requestDescribeMaintenanceWindowSchedule =
  req
    "DescribeMaintenanceWindowSchedule"
    "fixture/DescribeMaintenanceWindowSchedule.yaml"

requestDescribeMaintenanceWindowTargets :: DescribeMaintenanceWindowTargets -> TestTree
requestDescribeMaintenanceWindowTargets =
  req
    "DescribeMaintenanceWindowTargets"
    "fixture/DescribeMaintenanceWindowTargets.yaml"

requestDescribeMaintenanceWindowTasks :: DescribeMaintenanceWindowTasks -> TestTree
requestDescribeMaintenanceWindowTasks =
  req
    "DescribeMaintenanceWindowTasks"
    "fixture/DescribeMaintenanceWindowTasks.yaml"

requestDescribeMaintenanceWindows :: DescribeMaintenanceWindows -> TestTree
requestDescribeMaintenanceWindows =
  req
    "DescribeMaintenanceWindows"
    "fixture/DescribeMaintenanceWindows.yaml"

requestDescribeMaintenanceWindowsForTarget :: DescribeMaintenanceWindowsForTarget -> TestTree
requestDescribeMaintenanceWindowsForTarget =
  req
    "DescribeMaintenanceWindowsForTarget"
    "fixture/DescribeMaintenanceWindowsForTarget.yaml"

requestDescribeOpsItems :: DescribeOpsItems -> TestTree
requestDescribeOpsItems =
  req
    "DescribeOpsItems"
    "fixture/DescribeOpsItems.yaml"

requestDescribeParameters :: DescribeParameters -> TestTree
requestDescribeParameters =
  req
    "DescribeParameters"
    "fixture/DescribeParameters.yaml"

requestDescribePatchBaselines :: DescribePatchBaselines -> TestTree
requestDescribePatchBaselines =
  req
    "DescribePatchBaselines"
    "fixture/DescribePatchBaselines.yaml"

requestDescribePatchGroupState :: DescribePatchGroupState -> TestTree
requestDescribePatchGroupState =
  req
    "DescribePatchGroupState"
    "fixture/DescribePatchGroupState.yaml"

requestDescribePatchGroups :: DescribePatchGroups -> TestTree
requestDescribePatchGroups =
  req
    "DescribePatchGroups"
    "fixture/DescribePatchGroups.yaml"

requestDescribePatchProperties :: DescribePatchProperties -> TestTree
requestDescribePatchProperties =
  req
    "DescribePatchProperties"
    "fixture/DescribePatchProperties.yaml"

requestDescribeSessions :: DescribeSessions -> TestTree
requestDescribeSessions =
  req
    "DescribeSessions"
    "fixture/DescribeSessions.yaml"

requestDisassociateOpsItemRelatedItem :: DisassociateOpsItemRelatedItem -> TestTree
requestDisassociateOpsItemRelatedItem =
  req
    "DisassociateOpsItemRelatedItem"
    "fixture/DisassociateOpsItemRelatedItem.yaml"

requestGetAutomationExecution :: GetAutomationExecution -> TestTree
requestGetAutomationExecution =
  req
    "GetAutomationExecution"
    "fixture/GetAutomationExecution.yaml"

requestGetCalendarState :: GetCalendarState -> TestTree
requestGetCalendarState =
  req
    "GetCalendarState"
    "fixture/GetCalendarState.yaml"

requestGetCommandInvocation :: GetCommandInvocation -> TestTree
requestGetCommandInvocation =
  req
    "GetCommandInvocation"
    "fixture/GetCommandInvocation.yaml"

requestGetConnectionStatus :: GetConnectionStatus -> TestTree
requestGetConnectionStatus =
  req
    "GetConnectionStatus"
    "fixture/GetConnectionStatus.yaml"

requestGetDefaultPatchBaseline :: GetDefaultPatchBaseline -> TestTree
requestGetDefaultPatchBaseline =
  req
    "GetDefaultPatchBaseline"
    "fixture/GetDefaultPatchBaseline.yaml"

requestGetDeployablePatchSnapshotForInstance :: GetDeployablePatchSnapshotForInstance -> TestTree
requestGetDeployablePatchSnapshotForInstance =
  req
    "GetDeployablePatchSnapshotForInstance"
    "fixture/GetDeployablePatchSnapshotForInstance.yaml"

requestGetDocument :: GetDocument -> TestTree
requestGetDocument =
  req
    "GetDocument"
    "fixture/GetDocument.yaml"

requestGetInventory :: GetInventory -> TestTree
requestGetInventory =
  req
    "GetInventory"
    "fixture/GetInventory.yaml"

requestGetInventorySchema :: GetInventorySchema -> TestTree
requestGetInventorySchema =
  req
    "GetInventorySchema"
    "fixture/GetInventorySchema.yaml"

requestGetMaintenanceWindow :: GetMaintenanceWindow -> TestTree
requestGetMaintenanceWindow =
  req
    "GetMaintenanceWindow"
    "fixture/GetMaintenanceWindow.yaml"

requestGetMaintenanceWindowExecution :: GetMaintenanceWindowExecution -> TestTree
requestGetMaintenanceWindowExecution =
  req
    "GetMaintenanceWindowExecution"
    "fixture/GetMaintenanceWindowExecution.yaml"

requestGetMaintenanceWindowExecutionTask :: GetMaintenanceWindowExecutionTask -> TestTree
requestGetMaintenanceWindowExecutionTask =
  req
    "GetMaintenanceWindowExecutionTask"
    "fixture/GetMaintenanceWindowExecutionTask.yaml"

requestGetMaintenanceWindowExecutionTaskInvocation :: GetMaintenanceWindowExecutionTaskInvocation -> TestTree
requestGetMaintenanceWindowExecutionTaskInvocation =
  req
    "GetMaintenanceWindowExecutionTaskInvocation"
    "fixture/GetMaintenanceWindowExecutionTaskInvocation.yaml"

requestGetMaintenanceWindowTask :: GetMaintenanceWindowTask -> TestTree
requestGetMaintenanceWindowTask =
  req
    "GetMaintenanceWindowTask"
    "fixture/GetMaintenanceWindowTask.yaml"

requestGetOpsItem :: GetOpsItem -> TestTree
requestGetOpsItem =
  req
    "GetOpsItem"
    "fixture/GetOpsItem.yaml"

requestGetOpsMetadata :: GetOpsMetadata -> TestTree
requestGetOpsMetadata =
  req
    "GetOpsMetadata"
    "fixture/GetOpsMetadata.yaml"

requestGetOpsSummary :: GetOpsSummary -> TestTree
requestGetOpsSummary =
  req
    "GetOpsSummary"
    "fixture/GetOpsSummary.yaml"

requestGetParameter :: GetParameter -> TestTree
requestGetParameter =
  req
    "GetParameter"
    "fixture/GetParameter.yaml"

requestGetParameterHistory :: GetParameterHistory -> TestTree
requestGetParameterHistory =
  req
    "GetParameterHistory"
    "fixture/GetParameterHistory.yaml"

requestGetParameters :: GetParameters -> TestTree
requestGetParameters =
  req
    "GetParameters"
    "fixture/GetParameters.yaml"

requestGetParametersByPath :: GetParametersByPath -> TestTree
requestGetParametersByPath =
  req
    "GetParametersByPath"
    "fixture/GetParametersByPath.yaml"

requestGetPatchBaseline :: GetPatchBaseline -> TestTree
requestGetPatchBaseline =
  req
    "GetPatchBaseline"
    "fixture/GetPatchBaseline.yaml"

requestGetPatchBaselineForPatchGroup :: GetPatchBaselineForPatchGroup -> TestTree
requestGetPatchBaselineForPatchGroup =
  req
    "GetPatchBaselineForPatchGroup"
    "fixture/GetPatchBaselineForPatchGroup.yaml"

requestGetResourcePolicies :: GetResourcePolicies -> TestTree
requestGetResourcePolicies =
  req
    "GetResourcePolicies"
    "fixture/GetResourcePolicies.yaml"

requestGetServiceSetting :: GetServiceSetting -> TestTree
requestGetServiceSetting =
  req
    "GetServiceSetting"
    "fixture/GetServiceSetting.yaml"

requestLabelParameterVersion :: LabelParameterVersion -> TestTree
requestLabelParameterVersion =
  req
    "LabelParameterVersion"
    "fixture/LabelParameterVersion.yaml"

requestListAssociationVersions :: ListAssociationVersions -> TestTree
requestListAssociationVersions =
  req
    "ListAssociationVersions"
    "fixture/ListAssociationVersions.yaml"

requestListAssociations :: ListAssociations -> TestTree
requestListAssociations =
  req
    "ListAssociations"
    "fixture/ListAssociations.yaml"

requestListCommandInvocations :: ListCommandInvocations -> TestTree
requestListCommandInvocations =
  req
    "ListCommandInvocations"
    "fixture/ListCommandInvocations.yaml"

requestListCommands :: ListCommands -> TestTree
requestListCommands =
  req
    "ListCommands"
    "fixture/ListCommands.yaml"

requestListComplianceItems :: ListComplianceItems -> TestTree
requestListComplianceItems =
  req
    "ListComplianceItems"
    "fixture/ListComplianceItems.yaml"

requestListComplianceSummaries :: ListComplianceSummaries -> TestTree
requestListComplianceSummaries =
  req
    "ListComplianceSummaries"
    "fixture/ListComplianceSummaries.yaml"

requestListDocumentMetadataHistory :: ListDocumentMetadataHistory -> TestTree
requestListDocumentMetadataHistory =
  req
    "ListDocumentMetadataHistory"
    "fixture/ListDocumentMetadataHistory.yaml"

requestListDocumentVersions :: ListDocumentVersions -> TestTree
requestListDocumentVersions =
  req
    "ListDocumentVersions"
    "fixture/ListDocumentVersions.yaml"

requestListDocuments :: ListDocuments -> TestTree
requestListDocuments =
  req
    "ListDocuments"
    "fixture/ListDocuments.yaml"

requestListInventoryEntries :: ListInventoryEntries -> TestTree
requestListInventoryEntries =
  req
    "ListInventoryEntries"
    "fixture/ListInventoryEntries.yaml"

requestListOpsItemEvents :: ListOpsItemEvents -> TestTree
requestListOpsItemEvents =
  req
    "ListOpsItemEvents"
    "fixture/ListOpsItemEvents.yaml"

requestListOpsItemRelatedItems :: ListOpsItemRelatedItems -> TestTree
requestListOpsItemRelatedItems =
  req
    "ListOpsItemRelatedItems"
    "fixture/ListOpsItemRelatedItems.yaml"

requestListOpsMetadata :: ListOpsMetadata -> TestTree
requestListOpsMetadata =
  req
    "ListOpsMetadata"
    "fixture/ListOpsMetadata.yaml"

requestListResourceComplianceSummaries :: ListResourceComplianceSummaries -> TestTree
requestListResourceComplianceSummaries =
  req
    "ListResourceComplianceSummaries"
    "fixture/ListResourceComplianceSummaries.yaml"

requestListResourceDataSync :: ListResourceDataSync -> TestTree
requestListResourceDataSync =
  req
    "ListResourceDataSync"
    "fixture/ListResourceDataSync.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestModifyDocumentPermission :: ModifyDocumentPermission -> TestTree
requestModifyDocumentPermission =
  req
    "ModifyDocumentPermission"
    "fixture/ModifyDocumentPermission.yaml"

requestPutComplianceItems :: PutComplianceItems -> TestTree
requestPutComplianceItems =
  req
    "PutComplianceItems"
    "fixture/PutComplianceItems.yaml"

requestPutInventory :: PutInventory -> TestTree
requestPutInventory =
  req
    "PutInventory"
    "fixture/PutInventory.yaml"

requestPutParameter :: PutParameter -> TestTree
requestPutParameter =
  req
    "PutParameter"
    "fixture/PutParameter.yaml"

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy =
  req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

requestRegisterDefaultPatchBaseline :: RegisterDefaultPatchBaseline -> TestTree
requestRegisterDefaultPatchBaseline =
  req
    "RegisterDefaultPatchBaseline"
    "fixture/RegisterDefaultPatchBaseline.yaml"

requestRegisterPatchBaselineForPatchGroup :: RegisterPatchBaselineForPatchGroup -> TestTree
requestRegisterPatchBaselineForPatchGroup =
  req
    "RegisterPatchBaselineForPatchGroup"
    "fixture/RegisterPatchBaselineForPatchGroup.yaml"

requestRegisterTargetWithMaintenanceWindow :: RegisterTargetWithMaintenanceWindow -> TestTree
requestRegisterTargetWithMaintenanceWindow =
  req
    "RegisterTargetWithMaintenanceWindow"
    "fixture/RegisterTargetWithMaintenanceWindow.yaml"

requestRegisterTaskWithMaintenanceWindow :: RegisterTaskWithMaintenanceWindow -> TestTree
requestRegisterTaskWithMaintenanceWindow =
  req
    "RegisterTaskWithMaintenanceWindow"
    "fixture/RegisterTaskWithMaintenanceWindow.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource =
  req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestResetServiceSetting :: ResetServiceSetting -> TestTree
requestResetServiceSetting =
  req
    "ResetServiceSetting"
    "fixture/ResetServiceSetting.yaml"

requestResumeSession :: ResumeSession -> TestTree
requestResumeSession =
  req
    "ResumeSession"
    "fixture/ResumeSession.yaml"

requestSendAutomationSignal :: SendAutomationSignal -> TestTree
requestSendAutomationSignal =
  req
    "SendAutomationSignal"
    "fixture/SendAutomationSignal.yaml"

requestSendCommand :: SendCommand -> TestTree
requestSendCommand =
  req
    "SendCommand"
    "fixture/SendCommand.yaml"

requestStartAssociationsOnce :: StartAssociationsOnce -> TestTree
requestStartAssociationsOnce =
  req
    "StartAssociationsOnce"
    "fixture/StartAssociationsOnce.yaml"

requestStartAutomationExecution :: StartAutomationExecution -> TestTree
requestStartAutomationExecution =
  req
    "StartAutomationExecution"
    "fixture/StartAutomationExecution.yaml"

requestStartChangeRequestExecution :: StartChangeRequestExecution -> TestTree
requestStartChangeRequestExecution =
  req
    "StartChangeRequestExecution"
    "fixture/StartChangeRequestExecution.yaml"

requestStartSession :: StartSession -> TestTree
requestStartSession =
  req
    "StartSession"
    "fixture/StartSession.yaml"

requestStopAutomationExecution :: StopAutomationExecution -> TestTree
requestStopAutomationExecution =
  req
    "StopAutomationExecution"
    "fixture/StopAutomationExecution.yaml"

requestTerminateSession :: TerminateSession -> TestTree
requestTerminateSession =
  req
    "TerminateSession"
    "fixture/TerminateSession.yaml"

requestUnlabelParameterVersion :: UnlabelParameterVersion -> TestTree
requestUnlabelParameterVersion =
  req
    "UnlabelParameterVersion"
    "fixture/UnlabelParameterVersion.yaml"

requestUpdateAssociation :: UpdateAssociation -> TestTree
requestUpdateAssociation =
  req
    "UpdateAssociation"
    "fixture/UpdateAssociation.yaml"

requestUpdateAssociationStatus :: UpdateAssociationStatus -> TestTree
requestUpdateAssociationStatus =
  req
    "UpdateAssociationStatus"
    "fixture/UpdateAssociationStatus.yaml"

requestUpdateDocument :: UpdateDocument -> TestTree
requestUpdateDocument =
  req
    "UpdateDocument"
    "fixture/UpdateDocument.yaml"

requestUpdateDocumentDefaultVersion :: UpdateDocumentDefaultVersion -> TestTree
requestUpdateDocumentDefaultVersion =
  req
    "UpdateDocumentDefaultVersion"
    "fixture/UpdateDocumentDefaultVersion.yaml"

requestUpdateDocumentMetadata :: UpdateDocumentMetadata -> TestTree
requestUpdateDocumentMetadata =
  req
    "UpdateDocumentMetadata"
    "fixture/UpdateDocumentMetadata.yaml"

requestUpdateMaintenanceWindow :: UpdateMaintenanceWindow -> TestTree
requestUpdateMaintenanceWindow =
  req
    "UpdateMaintenanceWindow"
    "fixture/UpdateMaintenanceWindow.yaml"

requestUpdateMaintenanceWindowTarget :: UpdateMaintenanceWindowTarget -> TestTree
requestUpdateMaintenanceWindowTarget =
  req
    "UpdateMaintenanceWindowTarget"
    "fixture/UpdateMaintenanceWindowTarget.yaml"

requestUpdateMaintenanceWindowTask :: UpdateMaintenanceWindowTask -> TestTree
requestUpdateMaintenanceWindowTask =
  req
    "UpdateMaintenanceWindowTask"
    "fixture/UpdateMaintenanceWindowTask.yaml"

requestUpdateManagedInstanceRole :: UpdateManagedInstanceRole -> TestTree
requestUpdateManagedInstanceRole =
  req
    "UpdateManagedInstanceRole"
    "fixture/UpdateManagedInstanceRole.yaml"

requestUpdateOpsItem :: UpdateOpsItem -> TestTree
requestUpdateOpsItem =
  req
    "UpdateOpsItem"
    "fixture/UpdateOpsItem.yaml"

requestUpdateOpsMetadata :: UpdateOpsMetadata -> TestTree
requestUpdateOpsMetadata =
  req
    "UpdateOpsMetadata"
    "fixture/UpdateOpsMetadata.yaml"

requestUpdatePatchBaseline :: UpdatePatchBaseline -> TestTree
requestUpdatePatchBaseline =
  req
    "UpdatePatchBaseline"
    "fixture/UpdatePatchBaseline.yaml"

requestUpdateResourceDataSync :: UpdateResourceDataSync -> TestTree
requestUpdateResourceDataSync =
  req
    "UpdateResourceDataSync"
    "fixture/UpdateResourceDataSync.yaml"

requestUpdateServiceSetting :: UpdateServiceSetting -> TestTree
requestUpdateServiceSetting =
  req
    "UpdateServiceSetting"
    "fixture/UpdateServiceSetting.yaml"

-- Responses

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTagsToResource)

responseAssociateOpsItemRelatedItem :: AssociateOpsItemRelatedItemResponse -> TestTree
responseAssociateOpsItemRelatedItem =
  res
    "AssociateOpsItemRelatedItemResponse"
    "fixture/AssociateOpsItemRelatedItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateOpsItemRelatedItem)

responseCancelCommand :: CancelCommandResponse -> TestTree
responseCancelCommand =
  res
    "CancelCommandResponse"
    "fixture/CancelCommandResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelCommand)

responseCancelMaintenanceWindowExecution :: CancelMaintenanceWindowExecutionResponse -> TestTree
responseCancelMaintenanceWindowExecution =
  res
    "CancelMaintenanceWindowExecutionResponse"
    "fixture/CancelMaintenanceWindowExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelMaintenanceWindowExecution)

responseCreateActivation :: CreateActivationResponse -> TestTree
responseCreateActivation =
  res
    "CreateActivationResponse"
    "fixture/CreateActivationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateActivation)

responseCreateAssociation :: CreateAssociationResponse -> TestTree
responseCreateAssociation =
  res
    "CreateAssociationResponse"
    "fixture/CreateAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAssociation)

responseCreateAssociationBatch :: CreateAssociationBatchResponse -> TestTree
responseCreateAssociationBatch =
  res
    "CreateAssociationBatchResponse"
    "fixture/CreateAssociationBatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAssociationBatch)

responseCreateDocument :: CreateDocumentResponse -> TestTree
responseCreateDocument =
  res
    "CreateDocumentResponse"
    "fixture/CreateDocumentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDocument)

responseCreateMaintenanceWindow :: CreateMaintenanceWindowResponse -> TestTree
responseCreateMaintenanceWindow =
  res
    "CreateMaintenanceWindowResponse"
    "fixture/CreateMaintenanceWindowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMaintenanceWindow)

responseCreateOpsItem :: CreateOpsItemResponse -> TestTree
responseCreateOpsItem =
  res
    "CreateOpsItemResponse"
    "fixture/CreateOpsItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateOpsItem)

responseCreateOpsMetadata :: CreateOpsMetadataResponse -> TestTree
responseCreateOpsMetadata =
  res
    "CreateOpsMetadataResponse"
    "fixture/CreateOpsMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateOpsMetadata)

responseCreatePatchBaseline :: CreatePatchBaselineResponse -> TestTree
responseCreatePatchBaseline =
  res
    "CreatePatchBaselineResponse"
    "fixture/CreatePatchBaselineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePatchBaseline)

responseCreateResourceDataSync :: CreateResourceDataSyncResponse -> TestTree
responseCreateResourceDataSync =
  res
    "CreateResourceDataSyncResponse"
    "fixture/CreateResourceDataSyncResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResourceDataSync)

responseDeleteActivation :: DeleteActivationResponse -> TestTree
responseDeleteActivation =
  res
    "DeleteActivationResponse"
    "fixture/DeleteActivationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteActivation)

responseDeleteAssociation :: DeleteAssociationResponse -> TestTree
responseDeleteAssociation =
  res
    "DeleteAssociationResponse"
    "fixture/DeleteAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAssociation)

responseDeleteDocument :: DeleteDocumentResponse -> TestTree
responseDeleteDocument =
  res
    "DeleteDocumentResponse"
    "fixture/DeleteDocumentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDocument)

responseDeleteInventory :: DeleteInventoryResponse -> TestTree
responseDeleteInventory =
  res
    "DeleteInventoryResponse"
    "fixture/DeleteInventoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInventory)

responseDeleteMaintenanceWindow :: DeleteMaintenanceWindowResponse -> TestTree
responseDeleteMaintenanceWindow =
  res
    "DeleteMaintenanceWindowResponse"
    "fixture/DeleteMaintenanceWindowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMaintenanceWindow)

responseDeleteOpsMetadata :: DeleteOpsMetadataResponse -> TestTree
responseDeleteOpsMetadata =
  res
    "DeleteOpsMetadataResponse"
    "fixture/DeleteOpsMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOpsMetadata)

responseDeleteParameter :: DeleteParameterResponse -> TestTree
responseDeleteParameter =
  res
    "DeleteParameterResponse"
    "fixture/DeleteParameterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteParameter)

responseDeleteParameters :: DeleteParametersResponse -> TestTree
responseDeleteParameters =
  res
    "DeleteParametersResponse"
    "fixture/DeleteParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteParameters)

responseDeletePatchBaseline :: DeletePatchBaselineResponse -> TestTree
responseDeletePatchBaseline =
  res
    "DeletePatchBaselineResponse"
    "fixture/DeletePatchBaselineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePatchBaseline)

responseDeleteResourceDataSync :: DeleteResourceDataSyncResponse -> TestTree
responseDeleteResourceDataSync =
  res
    "DeleteResourceDataSyncResponse"
    "fixture/DeleteResourceDataSyncResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourceDataSync)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourcePolicy)

responseDeregisterManagedInstance :: DeregisterManagedInstanceResponse -> TestTree
responseDeregisterManagedInstance =
  res
    "DeregisterManagedInstanceResponse"
    "fixture/DeregisterManagedInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterManagedInstance)

responseDeregisterPatchBaselineForPatchGroup :: DeregisterPatchBaselineForPatchGroupResponse -> TestTree
responseDeregisterPatchBaselineForPatchGroup =
  res
    "DeregisterPatchBaselineForPatchGroupResponse"
    "fixture/DeregisterPatchBaselineForPatchGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterPatchBaselineForPatchGroup)

responseDeregisterTargetFromMaintenanceWindow :: DeregisterTargetFromMaintenanceWindowResponse -> TestTree
responseDeregisterTargetFromMaintenanceWindow =
  res
    "DeregisterTargetFromMaintenanceWindowResponse"
    "fixture/DeregisterTargetFromMaintenanceWindowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterTargetFromMaintenanceWindow)

responseDeregisterTaskFromMaintenanceWindow :: DeregisterTaskFromMaintenanceWindowResponse -> TestTree
responseDeregisterTaskFromMaintenanceWindow =
  res
    "DeregisterTaskFromMaintenanceWindowResponse"
    "fixture/DeregisterTaskFromMaintenanceWindowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterTaskFromMaintenanceWindow)

responseDescribeActivations :: DescribeActivationsResponse -> TestTree
responseDescribeActivations =
  res
    "DescribeActivationsResponse"
    "fixture/DescribeActivationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeActivations)

responseDescribeAssociation :: DescribeAssociationResponse -> TestTree
responseDescribeAssociation =
  res
    "DescribeAssociationResponse"
    "fixture/DescribeAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAssociation)

responseDescribeAssociationExecutionTargets :: DescribeAssociationExecutionTargetsResponse -> TestTree
responseDescribeAssociationExecutionTargets =
  res
    "DescribeAssociationExecutionTargetsResponse"
    "fixture/DescribeAssociationExecutionTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAssociationExecutionTargets)

responseDescribeAssociationExecutions :: DescribeAssociationExecutionsResponse -> TestTree
responseDescribeAssociationExecutions =
  res
    "DescribeAssociationExecutionsResponse"
    "fixture/DescribeAssociationExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAssociationExecutions)

responseDescribeAutomationExecutions :: DescribeAutomationExecutionsResponse -> TestTree
responseDescribeAutomationExecutions =
  res
    "DescribeAutomationExecutionsResponse"
    "fixture/DescribeAutomationExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAutomationExecutions)

responseDescribeAutomationStepExecutions :: DescribeAutomationStepExecutionsResponse -> TestTree
responseDescribeAutomationStepExecutions =
  res
    "DescribeAutomationStepExecutionsResponse"
    "fixture/DescribeAutomationStepExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAutomationStepExecutions)

responseDescribeAvailablePatches :: DescribeAvailablePatchesResponse -> TestTree
responseDescribeAvailablePatches =
  res
    "DescribeAvailablePatchesResponse"
    "fixture/DescribeAvailablePatchesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAvailablePatches)

responseDescribeDocument :: DescribeDocumentResponse -> TestTree
responseDescribeDocument =
  res
    "DescribeDocumentResponse"
    "fixture/DescribeDocumentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDocument)

responseDescribeDocumentPermission :: DescribeDocumentPermissionResponse -> TestTree
responseDescribeDocumentPermission =
  res
    "DescribeDocumentPermissionResponse"
    "fixture/DescribeDocumentPermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDocumentPermission)

responseDescribeEffectiveInstanceAssociations :: DescribeEffectiveInstanceAssociationsResponse -> TestTree
responseDescribeEffectiveInstanceAssociations =
  res
    "DescribeEffectiveInstanceAssociationsResponse"
    "fixture/DescribeEffectiveInstanceAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEffectiveInstanceAssociations)

responseDescribeEffectivePatchesForPatchBaseline :: DescribeEffectivePatchesForPatchBaselineResponse -> TestTree
responseDescribeEffectivePatchesForPatchBaseline =
  res
    "DescribeEffectivePatchesForPatchBaselineResponse"
    "fixture/DescribeEffectivePatchesForPatchBaselineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEffectivePatchesForPatchBaseline)

responseDescribeInstanceAssociationsStatus :: DescribeInstanceAssociationsStatusResponse -> TestTree
responseDescribeInstanceAssociationsStatus =
  res
    "DescribeInstanceAssociationsStatusResponse"
    "fixture/DescribeInstanceAssociationsStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceAssociationsStatus)

responseDescribeInstanceInformation :: DescribeInstanceInformationResponse -> TestTree
responseDescribeInstanceInformation =
  res
    "DescribeInstanceInformationResponse"
    "fixture/DescribeInstanceInformationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceInformation)

responseDescribeInstancePatchStates :: DescribeInstancePatchStatesResponse -> TestTree
responseDescribeInstancePatchStates =
  res
    "DescribeInstancePatchStatesResponse"
    "fixture/DescribeInstancePatchStatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstancePatchStates)

responseDescribeInstancePatchStatesForPatchGroup :: DescribeInstancePatchStatesForPatchGroupResponse -> TestTree
responseDescribeInstancePatchStatesForPatchGroup =
  res
    "DescribeInstancePatchStatesForPatchGroupResponse"
    "fixture/DescribeInstancePatchStatesForPatchGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstancePatchStatesForPatchGroup)

responseDescribeInstancePatches :: DescribeInstancePatchesResponse -> TestTree
responseDescribeInstancePatches =
  res
    "DescribeInstancePatchesResponse"
    "fixture/DescribeInstancePatchesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstancePatches)

responseDescribeInventoryDeletions :: DescribeInventoryDeletionsResponse -> TestTree
responseDescribeInventoryDeletions =
  res
    "DescribeInventoryDeletionsResponse"
    "fixture/DescribeInventoryDeletionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInventoryDeletions)

responseDescribeMaintenanceWindowExecutionTaskInvocations :: DescribeMaintenanceWindowExecutionTaskInvocationsResponse -> TestTree
responseDescribeMaintenanceWindowExecutionTaskInvocations =
  res
    "DescribeMaintenanceWindowExecutionTaskInvocationsResponse"
    "fixture/DescribeMaintenanceWindowExecutionTaskInvocationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMaintenanceWindowExecutionTaskInvocations)

responseDescribeMaintenanceWindowExecutionTasks :: DescribeMaintenanceWindowExecutionTasksResponse -> TestTree
responseDescribeMaintenanceWindowExecutionTasks =
  res
    "DescribeMaintenanceWindowExecutionTasksResponse"
    "fixture/DescribeMaintenanceWindowExecutionTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMaintenanceWindowExecutionTasks)

responseDescribeMaintenanceWindowExecutions :: DescribeMaintenanceWindowExecutionsResponse -> TestTree
responseDescribeMaintenanceWindowExecutions =
  res
    "DescribeMaintenanceWindowExecutionsResponse"
    "fixture/DescribeMaintenanceWindowExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMaintenanceWindowExecutions)

responseDescribeMaintenanceWindowSchedule :: DescribeMaintenanceWindowScheduleResponse -> TestTree
responseDescribeMaintenanceWindowSchedule =
  res
    "DescribeMaintenanceWindowScheduleResponse"
    "fixture/DescribeMaintenanceWindowScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMaintenanceWindowSchedule)

responseDescribeMaintenanceWindowTargets :: DescribeMaintenanceWindowTargetsResponse -> TestTree
responseDescribeMaintenanceWindowTargets =
  res
    "DescribeMaintenanceWindowTargetsResponse"
    "fixture/DescribeMaintenanceWindowTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMaintenanceWindowTargets)

responseDescribeMaintenanceWindowTasks :: DescribeMaintenanceWindowTasksResponse -> TestTree
responseDescribeMaintenanceWindowTasks =
  res
    "DescribeMaintenanceWindowTasksResponse"
    "fixture/DescribeMaintenanceWindowTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMaintenanceWindowTasks)

responseDescribeMaintenanceWindows :: DescribeMaintenanceWindowsResponse -> TestTree
responseDescribeMaintenanceWindows =
  res
    "DescribeMaintenanceWindowsResponse"
    "fixture/DescribeMaintenanceWindowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMaintenanceWindows)

responseDescribeMaintenanceWindowsForTarget :: DescribeMaintenanceWindowsForTargetResponse -> TestTree
responseDescribeMaintenanceWindowsForTarget =
  res
    "DescribeMaintenanceWindowsForTargetResponse"
    "fixture/DescribeMaintenanceWindowsForTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMaintenanceWindowsForTarget)

responseDescribeOpsItems :: DescribeOpsItemsResponse -> TestTree
responseDescribeOpsItems =
  res
    "DescribeOpsItemsResponse"
    "fixture/DescribeOpsItemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOpsItems)

responseDescribeParameters :: DescribeParametersResponse -> TestTree
responseDescribeParameters =
  res
    "DescribeParametersResponse"
    "fixture/DescribeParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeParameters)

responseDescribePatchBaselines :: DescribePatchBaselinesResponse -> TestTree
responseDescribePatchBaselines =
  res
    "DescribePatchBaselinesResponse"
    "fixture/DescribePatchBaselinesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePatchBaselines)

responseDescribePatchGroupState :: DescribePatchGroupStateResponse -> TestTree
responseDescribePatchGroupState =
  res
    "DescribePatchGroupStateResponse"
    "fixture/DescribePatchGroupStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePatchGroupState)

responseDescribePatchGroups :: DescribePatchGroupsResponse -> TestTree
responseDescribePatchGroups =
  res
    "DescribePatchGroupsResponse"
    "fixture/DescribePatchGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePatchGroups)

responseDescribePatchProperties :: DescribePatchPropertiesResponse -> TestTree
responseDescribePatchProperties =
  res
    "DescribePatchPropertiesResponse"
    "fixture/DescribePatchPropertiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePatchProperties)

responseDescribeSessions :: DescribeSessionsResponse -> TestTree
responseDescribeSessions =
  res
    "DescribeSessionsResponse"
    "fixture/DescribeSessionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSessions)

responseDisassociateOpsItemRelatedItem :: DisassociateOpsItemRelatedItemResponse -> TestTree
responseDisassociateOpsItemRelatedItem =
  res
    "DisassociateOpsItemRelatedItemResponse"
    "fixture/DisassociateOpsItemRelatedItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateOpsItemRelatedItem)

responseGetAutomationExecution :: GetAutomationExecutionResponse -> TestTree
responseGetAutomationExecution =
  res
    "GetAutomationExecutionResponse"
    "fixture/GetAutomationExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAutomationExecution)

responseGetCalendarState :: GetCalendarStateResponse -> TestTree
responseGetCalendarState =
  res
    "GetCalendarStateResponse"
    "fixture/GetCalendarStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCalendarState)

responseGetCommandInvocation :: GetCommandInvocationResponse -> TestTree
responseGetCommandInvocation =
  res
    "GetCommandInvocationResponse"
    "fixture/GetCommandInvocationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCommandInvocation)

responseGetConnectionStatus :: GetConnectionStatusResponse -> TestTree
responseGetConnectionStatus =
  res
    "GetConnectionStatusResponse"
    "fixture/GetConnectionStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConnectionStatus)

responseGetDefaultPatchBaseline :: GetDefaultPatchBaselineResponse -> TestTree
responseGetDefaultPatchBaseline =
  res
    "GetDefaultPatchBaselineResponse"
    "fixture/GetDefaultPatchBaselineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDefaultPatchBaseline)

responseGetDeployablePatchSnapshotForInstance :: GetDeployablePatchSnapshotForInstanceResponse -> TestTree
responseGetDeployablePatchSnapshotForInstance =
  res
    "GetDeployablePatchSnapshotForInstanceResponse"
    "fixture/GetDeployablePatchSnapshotForInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeployablePatchSnapshotForInstance)

responseGetDocument :: GetDocumentResponse -> TestTree
responseGetDocument =
  res
    "GetDocumentResponse"
    "fixture/GetDocumentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDocument)

responseGetInventory :: GetInventoryResponse -> TestTree
responseGetInventory =
  res
    "GetInventoryResponse"
    "fixture/GetInventoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInventory)

responseGetInventorySchema :: GetInventorySchemaResponse -> TestTree
responseGetInventorySchema =
  res
    "GetInventorySchemaResponse"
    "fixture/GetInventorySchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInventorySchema)

responseGetMaintenanceWindow :: GetMaintenanceWindowResponse -> TestTree
responseGetMaintenanceWindow =
  res
    "GetMaintenanceWindowResponse"
    "fixture/GetMaintenanceWindowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMaintenanceWindow)

responseGetMaintenanceWindowExecution :: GetMaintenanceWindowExecutionResponse -> TestTree
responseGetMaintenanceWindowExecution =
  res
    "GetMaintenanceWindowExecutionResponse"
    "fixture/GetMaintenanceWindowExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMaintenanceWindowExecution)

responseGetMaintenanceWindowExecutionTask :: GetMaintenanceWindowExecutionTaskResponse -> TestTree
responseGetMaintenanceWindowExecutionTask =
  res
    "GetMaintenanceWindowExecutionTaskResponse"
    "fixture/GetMaintenanceWindowExecutionTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMaintenanceWindowExecutionTask)

responseGetMaintenanceWindowExecutionTaskInvocation :: GetMaintenanceWindowExecutionTaskInvocationResponse -> TestTree
responseGetMaintenanceWindowExecutionTaskInvocation =
  res
    "GetMaintenanceWindowExecutionTaskInvocationResponse"
    "fixture/GetMaintenanceWindowExecutionTaskInvocationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMaintenanceWindowExecutionTaskInvocation)

responseGetMaintenanceWindowTask :: GetMaintenanceWindowTaskResponse -> TestTree
responseGetMaintenanceWindowTask =
  res
    "GetMaintenanceWindowTaskResponse"
    "fixture/GetMaintenanceWindowTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMaintenanceWindowTask)

responseGetOpsItem :: GetOpsItemResponse -> TestTree
responseGetOpsItem =
  res
    "GetOpsItemResponse"
    "fixture/GetOpsItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOpsItem)

responseGetOpsMetadata :: GetOpsMetadataResponse -> TestTree
responseGetOpsMetadata =
  res
    "GetOpsMetadataResponse"
    "fixture/GetOpsMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOpsMetadata)

responseGetOpsSummary :: GetOpsSummaryResponse -> TestTree
responseGetOpsSummary =
  res
    "GetOpsSummaryResponse"
    "fixture/GetOpsSummaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOpsSummary)

responseGetParameter :: GetParameterResponse -> TestTree
responseGetParameter =
  res
    "GetParameterResponse"
    "fixture/GetParameterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetParameter)

responseGetParameterHistory :: GetParameterHistoryResponse -> TestTree
responseGetParameterHistory =
  res
    "GetParameterHistoryResponse"
    "fixture/GetParameterHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetParameterHistory)

responseGetParameters :: GetParametersResponse -> TestTree
responseGetParameters =
  res
    "GetParametersResponse"
    "fixture/GetParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetParameters)

responseGetParametersByPath :: GetParametersByPathResponse -> TestTree
responseGetParametersByPath =
  res
    "GetParametersByPathResponse"
    "fixture/GetParametersByPathResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetParametersByPath)

responseGetPatchBaseline :: GetPatchBaselineResponse -> TestTree
responseGetPatchBaseline =
  res
    "GetPatchBaselineResponse"
    "fixture/GetPatchBaselineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPatchBaseline)

responseGetPatchBaselineForPatchGroup :: GetPatchBaselineForPatchGroupResponse -> TestTree
responseGetPatchBaselineForPatchGroup =
  res
    "GetPatchBaselineForPatchGroupResponse"
    "fixture/GetPatchBaselineForPatchGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPatchBaselineForPatchGroup)

responseGetResourcePolicies :: GetResourcePoliciesResponse -> TestTree
responseGetResourcePolicies =
  res
    "GetResourcePoliciesResponse"
    "fixture/GetResourcePoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourcePolicies)

responseGetServiceSetting :: GetServiceSettingResponse -> TestTree
responseGetServiceSetting =
  res
    "GetServiceSettingResponse"
    "fixture/GetServiceSettingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServiceSetting)

responseLabelParameterVersion :: LabelParameterVersionResponse -> TestTree
responseLabelParameterVersion =
  res
    "LabelParameterVersionResponse"
    "fixture/LabelParameterVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy LabelParameterVersion)

responseListAssociationVersions :: ListAssociationVersionsResponse -> TestTree
responseListAssociationVersions =
  res
    "ListAssociationVersionsResponse"
    "fixture/ListAssociationVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssociationVersions)

responseListAssociations :: ListAssociationsResponse -> TestTree
responseListAssociations =
  res
    "ListAssociationsResponse"
    "fixture/ListAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssociations)

responseListCommandInvocations :: ListCommandInvocationsResponse -> TestTree
responseListCommandInvocations =
  res
    "ListCommandInvocationsResponse"
    "fixture/ListCommandInvocationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCommandInvocations)

responseListCommands :: ListCommandsResponse -> TestTree
responseListCommands =
  res
    "ListCommandsResponse"
    "fixture/ListCommandsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCommands)

responseListComplianceItems :: ListComplianceItemsResponse -> TestTree
responseListComplianceItems =
  res
    "ListComplianceItemsResponse"
    "fixture/ListComplianceItemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListComplianceItems)

responseListComplianceSummaries :: ListComplianceSummariesResponse -> TestTree
responseListComplianceSummaries =
  res
    "ListComplianceSummariesResponse"
    "fixture/ListComplianceSummariesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListComplianceSummaries)

responseListDocumentMetadataHistory :: ListDocumentMetadataHistoryResponse -> TestTree
responseListDocumentMetadataHistory =
  res
    "ListDocumentMetadataHistoryResponse"
    "fixture/ListDocumentMetadataHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDocumentMetadataHistory)

responseListDocumentVersions :: ListDocumentVersionsResponse -> TestTree
responseListDocumentVersions =
  res
    "ListDocumentVersionsResponse"
    "fixture/ListDocumentVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDocumentVersions)

responseListDocuments :: ListDocumentsResponse -> TestTree
responseListDocuments =
  res
    "ListDocumentsResponse"
    "fixture/ListDocumentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDocuments)

responseListInventoryEntries :: ListInventoryEntriesResponse -> TestTree
responseListInventoryEntries =
  res
    "ListInventoryEntriesResponse"
    "fixture/ListInventoryEntriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInventoryEntries)

responseListOpsItemEvents :: ListOpsItemEventsResponse -> TestTree
responseListOpsItemEvents =
  res
    "ListOpsItemEventsResponse"
    "fixture/ListOpsItemEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOpsItemEvents)

responseListOpsItemRelatedItems :: ListOpsItemRelatedItemsResponse -> TestTree
responseListOpsItemRelatedItems =
  res
    "ListOpsItemRelatedItemsResponse"
    "fixture/ListOpsItemRelatedItemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOpsItemRelatedItems)

responseListOpsMetadata :: ListOpsMetadataResponse -> TestTree
responseListOpsMetadata =
  res
    "ListOpsMetadataResponse"
    "fixture/ListOpsMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOpsMetadata)

responseListResourceComplianceSummaries :: ListResourceComplianceSummariesResponse -> TestTree
responseListResourceComplianceSummaries =
  res
    "ListResourceComplianceSummariesResponse"
    "fixture/ListResourceComplianceSummariesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceComplianceSummaries)

responseListResourceDataSync :: ListResourceDataSyncResponse -> TestTree
responseListResourceDataSync =
  res
    "ListResourceDataSyncResponse"
    "fixture/ListResourceDataSyncResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceDataSync)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseModifyDocumentPermission :: ModifyDocumentPermissionResponse -> TestTree
responseModifyDocumentPermission =
  res
    "ModifyDocumentPermissionResponse"
    "fixture/ModifyDocumentPermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyDocumentPermission)

responsePutComplianceItems :: PutComplianceItemsResponse -> TestTree
responsePutComplianceItems =
  res
    "PutComplianceItemsResponse"
    "fixture/PutComplianceItemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutComplianceItems)

responsePutInventory :: PutInventoryResponse -> TestTree
responsePutInventory =
  res
    "PutInventoryResponse"
    "fixture/PutInventoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutInventory)

responsePutParameter :: PutParameterResponse -> TestTree
responsePutParameter =
  res
    "PutParameterResponse"
    "fixture/PutParameterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutParameter)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResourcePolicy)

responseRegisterDefaultPatchBaseline :: RegisterDefaultPatchBaselineResponse -> TestTree
responseRegisterDefaultPatchBaseline =
  res
    "RegisterDefaultPatchBaselineResponse"
    "fixture/RegisterDefaultPatchBaselineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterDefaultPatchBaseline)

responseRegisterPatchBaselineForPatchGroup :: RegisterPatchBaselineForPatchGroupResponse -> TestTree
responseRegisterPatchBaselineForPatchGroup =
  res
    "RegisterPatchBaselineForPatchGroupResponse"
    "fixture/RegisterPatchBaselineForPatchGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterPatchBaselineForPatchGroup)

responseRegisterTargetWithMaintenanceWindow :: RegisterTargetWithMaintenanceWindowResponse -> TestTree
responseRegisterTargetWithMaintenanceWindow =
  res
    "RegisterTargetWithMaintenanceWindowResponse"
    "fixture/RegisterTargetWithMaintenanceWindowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterTargetWithMaintenanceWindow)

responseRegisterTaskWithMaintenanceWindow :: RegisterTaskWithMaintenanceWindowResponse -> TestTree
responseRegisterTaskWithMaintenanceWindow =
  res
    "RegisterTaskWithMaintenanceWindowResponse"
    "fixture/RegisterTaskWithMaintenanceWindowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterTaskWithMaintenanceWindow)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTagsFromResource)

responseResetServiceSetting :: ResetServiceSettingResponse -> TestTree
responseResetServiceSetting =
  res
    "ResetServiceSettingResponse"
    "fixture/ResetServiceSettingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetServiceSetting)

responseResumeSession :: ResumeSessionResponse -> TestTree
responseResumeSession =
  res
    "ResumeSessionResponse"
    "fixture/ResumeSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResumeSession)

responseSendAutomationSignal :: SendAutomationSignalResponse -> TestTree
responseSendAutomationSignal =
  res
    "SendAutomationSignalResponse"
    "fixture/SendAutomationSignalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendAutomationSignal)

responseSendCommand :: SendCommandResponse -> TestTree
responseSendCommand =
  res
    "SendCommandResponse"
    "fixture/SendCommandResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendCommand)

responseStartAssociationsOnce :: StartAssociationsOnceResponse -> TestTree
responseStartAssociationsOnce =
  res
    "StartAssociationsOnceResponse"
    "fixture/StartAssociationsOnceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartAssociationsOnce)

responseStartAutomationExecution :: StartAutomationExecutionResponse -> TestTree
responseStartAutomationExecution =
  res
    "StartAutomationExecutionResponse"
    "fixture/StartAutomationExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartAutomationExecution)

responseStartChangeRequestExecution :: StartChangeRequestExecutionResponse -> TestTree
responseStartChangeRequestExecution =
  res
    "StartChangeRequestExecutionResponse"
    "fixture/StartChangeRequestExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartChangeRequestExecution)

responseStartSession :: StartSessionResponse -> TestTree
responseStartSession =
  res
    "StartSessionResponse"
    "fixture/StartSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartSession)

responseStopAutomationExecution :: StopAutomationExecutionResponse -> TestTree
responseStopAutomationExecution =
  res
    "StopAutomationExecutionResponse"
    "fixture/StopAutomationExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopAutomationExecution)

responseTerminateSession :: TerminateSessionResponse -> TestTree
responseTerminateSession =
  res
    "TerminateSessionResponse"
    "fixture/TerminateSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TerminateSession)

responseUnlabelParameterVersion :: UnlabelParameterVersionResponse -> TestTree
responseUnlabelParameterVersion =
  res
    "UnlabelParameterVersionResponse"
    "fixture/UnlabelParameterVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UnlabelParameterVersion)

responseUpdateAssociation :: UpdateAssociationResponse -> TestTree
responseUpdateAssociation =
  res
    "UpdateAssociationResponse"
    "fixture/UpdateAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAssociation)

responseUpdateAssociationStatus :: UpdateAssociationStatusResponse -> TestTree
responseUpdateAssociationStatus =
  res
    "UpdateAssociationStatusResponse"
    "fixture/UpdateAssociationStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAssociationStatus)

responseUpdateDocument :: UpdateDocumentResponse -> TestTree
responseUpdateDocument =
  res
    "UpdateDocumentResponse"
    "fixture/UpdateDocumentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDocument)

responseUpdateDocumentDefaultVersion :: UpdateDocumentDefaultVersionResponse -> TestTree
responseUpdateDocumentDefaultVersion =
  res
    "UpdateDocumentDefaultVersionResponse"
    "fixture/UpdateDocumentDefaultVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDocumentDefaultVersion)

responseUpdateDocumentMetadata :: UpdateDocumentMetadataResponse -> TestTree
responseUpdateDocumentMetadata =
  res
    "UpdateDocumentMetadataResponse"
    "fixture/UpdateDocumentMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDocumentMetadata)

responseUpdateMaintenanceWindow :: UpdateMaintenanceWindowResponse -> TestTree
responseUpdateMaintenanceWindow =
  res
    "UpdateMaintenanceWindowResponse"
    "fixture/UpdateMaintenanceWindowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMaintenanceWindow)

responseUpdateMaintenanceWindowTarget :: UpdateMaintenanceWindowTargetResponse -> TestTree
responseUpdateMaintenanceWindowTarget =
  res
    "UpdateMaintenanceWindowTargetResponse"
    "fixture/UpdateMaintenanceWindowTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMaintenanceWindowTarget)

responseUpdateMaintenanceWindowTask :: UpdateMaintenanceWindowTaskResponse -> TestTree
responseUpdateMaintenanceWindowTask =
  res
    "UpdateMaintenanceWindowTaskResponse"
    "fixture/UpdateMaintenanceWindowTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMaintenanceWindowTask)

responseUpdateManagedInstanceRole :: UpdateManagedInstanceRoleResponse -> TestTree
responseUpdateManagedInstanceRole =
  res
    "UpdateManagedInstanceRoleResponse"
    "fixture/UpdateManagedInstanceRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateManagedInstanceRole)

responseUpdateOpsItem :: UpdateOpsItemResponse -> TestTree
responseUpdateOpsItem =
  res
    "UpdateOpsItemResponse"
    "fixture/UpdateOpsItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateOpsItem)

responseUpdateOpsMetadata :: UpdateOpsMetadataResponse -> TestTree
responseUpdateOpsMetadata =
  res
    "UpdateOpsMetadataResponse"
    "fixture/UpdateOpsMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateOpsMetadata)

responseUpdatePatchBaseline :: UpdatePatchBaselineResponse -> TestTree
responseUpdatePatchBaseline =
  res
    "UpdatePatchBaselineResponse"
    "fixture/UpdatePatchBaselineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePatchBaseline)

responseUpdateResourceDataSync :: UpdateResourceDataSyncResponse -> TestTree
responseUpdateResourceDataSync =
  res
    "UpdateResourceDataSyncResponse"
    "fixture/UpdateResourceDataSyncResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResourceDataSync)

responseUpdateServiceSetting :: UpdateServiceSettingResponse -> TestTree
responseUpdateServiceSetting =
  res
    "UpdateServiceSettingResponse"
    "fixture/UpdateServiceSettingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServiceSetting)
