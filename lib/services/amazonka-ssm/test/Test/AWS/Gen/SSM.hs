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
--         [ requestDescribeOpsItems $
--             newDescribeOpsItems
--
--         , requestListResourceComplianceSummaries $
--             newListResourceComplianceSummaries
--
--         , requestGetParameter $
--             newGetParameter
--
--         , requestGetOpsMetadata $
--             newGetOpsMetadata
--
--         , requestAssociateOpsItemRelatedItem $
--             newAssociateOpsItemRelatedItem
--
--         , requestDescribeParameters $
--             newDescribeParameters
--
--         , requestRegisterTaskWithMaintenanceWindow $
--             newRegisterTaskWithMaintenanceWindow
--
--         , requestListResourceDataSync $
--             newListResourceDataSync
--
--         , requestGetOpsItem $
--             newGetOpsItem
--
--         , requestDescribePatchGroups $
--             newDescribePatchGroups
--
--         , requestTerminateSession $
--             newTerminateSession
--
--         , requestUpdateDocumentDefaultVersion $
--             newUpdateDocumentDefaultVersion
--
--         , requestGetInventory $
--             newGetInventory
--
--         , requestDescribeAssociation $
--             newDescribeAssociation
--
--         , requestDescribeAssociationExecutionTargets $
--             newDescribeAssociationExecutionTargets
--
--         , requestDeregisterManagedInstance $
--             newDeregisterManagedInstance
--
--         , requestUpdateAssociationStatus $
--             newUpdateAssociationStatus
--
--         , requestCreatePatchBaseline $
--             newCreatePatchBaseline
--
--         , requestUpdateMaintenanceWindowTarget $
--             newUpdateMaintenanceWindowTarget
--
--         , requestDescribeAutomationStepExecutions $
--             newDescribeAutomationStepExecutions
--
--         , requestGetCommandInvocation $
--             newGetCommandInvocation
--
--         , requestDeregisterTargetFromMaintenanceWindow $
--             newDeregisterTargetFromMaintenanceWindow
--
--         , requestGetOpsSummary $
--             newGetOpsSummary
--
--         , requestResetServiceSetting $
--             newResetServiceSetting
--
--         , requestListDocuments $
--             newListDocuments
--
--         , requestDescribeInstancePatchStates $
--             newDescribeInstancePatchStates
--
--         , requestUnlabelParameterVersion $
--             newUnlabelParameterVersion
--
--         , requestUpdateDocument $
--             newUpdateDocument
--
--         , requestDeleteDocument $
--             newDeleteDocument
--
--         , requestListCommands $
--             newListCommands
--
--         , requestStartSession $
--             newStartSession
--
--         , requestCreateDocument $
--             newCreateDocument
--
--         , requestDeleteInventory $
--             newDeleteInventory
--
--         , requestRemoveTagsFromResource $
--             newRemoveTagsFromResource
--
--         , requestDeleteParameters $
--             newDeleteParameters
--
--         , requestSendCommand $
--             newSendCommand
--
--         , requestGetCalendarState $
--             newGetCalendarState
--
--         , requestDescribeEffectiveInstanceAssociations $
--             newDescribeEffectiveInstanceAssociations
--
--         , requestRegisterTargetWithMaintenanceWindow $
--             newRegisterTargetWithMaintenanceWindow
--
--         , requestListOpsMetadata $
--             newListOpsMetadata
--
--         , requestDeleteParameter $
--             newDeleteParameter
--
--         , requestCreateActivation $
--             newCreateActivation
--
--         , requestUpdateAssociation $
--             newUpdateAssociation
--
--         , requestDeleteOpsMetadata $
--             newDeleteOpsMetadata
--
--         , requestUpdateOpsMetadata $
--             newUpdateOpsMetadata
--
--         , requestDeleteAssociation $
--             newDeleteAssociation
--
--         , requestDescribeMaintenanceWindowExecutionTaskInvocations $
--             newDescribeMaintenanceWindowExecutionTaskInvocations
--
--         , requestUpdateOpsItem $
--             newUpdateOpsItem
--
--         , requestDescribeSessions $
--             newDescribeSessions
--
--         , requestDescribeInstanceInformation $
--             newDescribeInstanceInformation
--
--         , requestGetMaintenanceWindowExecutionTaskInvocation $
--             newGetMaintenanceWindowExecutionTaskInvocation
--
--         , requestStartChangeRequestExecution $
--             newStartChangeRequestExecution
--
--         , requestListComplianceSummaries $
--             newListComplianceSummaries
--
--         , requestDescribeMaintenanceWindowExecutions $
--             newDescribeMaintenanceWindowExecutions
--
--         , requestCreateOpsItem $
--             newCreateOpsItem
--
--         , requestDescribeMaintenanceWindowsForTarget $
--             newDescribeMaintenanceWindowsForTarget
--
--         , requestCreateAssociation $
--             newCreateAssociation
--
--         , requestCreateOpsMetadata $
--             newCreateOpsMetadata
--
--         , requestSendAutomationSignal $
--             newSendAutomationSignal
--
--         , requestDescribeDocument $
--             newDescribeDocument
--
--         , requestListAssociationVersions $
--             newListAssociationVersions
--
--         , requestListOpsItemRelatedItems $
--             newListOpsItemRelatedItems
--
--         , requestPutComplianceItems $
--             newPutComplianceItems
--
--         , requestDeregisterTaskFromMaintenanceWindow $
--             newDeregisterTaskFromMaintenanceWindow
--
--         , requestGetMaintenanceWindowTask $
--             newGetMaintenanceWindowTask
--
--         , requestDeregisterPatchBaselineForPatchGroup $
--             newDeregisterPatchBaselineForPatchGroup
--
--         , requestDescribeMaintenanceWindows $
--             newDescribeMaintenanceWindows
--
--         , requestGetDeployablePatchSnapshotForInstance $
--             newGetDeployablePatchSnapshotForInstance
--
--         , requestResumeSession $
--             newResumeSession
--
--         , requestGetMaintenanceWindow $
--             newGetMaintenanceWindow
--
--         , requestRegisterDefaultPatchBaseline $
--             newRegisterDefaultPatchBaseline
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
--         , requestUpdatePatchBaseline $
--             newUpdatePatchBaseline
--
--         , requestDescribeInstancePatches $
--             newDescribeInstancePatches
--
--         , requestDescribeAvailablePatches $
--             newDescribeAvailablePatches
--
--         , requestDescribeInstancePatchStatesForPatchGroup $
--             newDescribeInstancePatchStatesForPatchGroup
--
--         , requestDeleteResourceDataSync $
--             newDeleteResourceDataSync
--
--         , requestGetParameters $
--             newGetParameters
--
--         , requestGetConnectionStatus $
--             newGetConnectionStatus
--
--         , requestModifyDocumentPermission $
--             newModifyDocumentPermission
--
--         , requestListOpsItemEvents $
--             newListOpsItemEvents
--
--         , requestAddTagsToResource $
--             newAddTagsToResource
--
--         , requestDescribeDocumentPermission $
--             newDescribeDocumentPermission
--
--         , requestCreateResourceDataSync $
--             newCreateResourceDataSync
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
--         , requestCancelCommand $
--             newCancelCommand
--
--         , requestGetDocument $
--             newGetDocument
--
--         , requestListComplianceItems $
--             newListComplianceItems
--
--         , requestCreateAssociationBatch $
--             newCreateAssociationBatch
--
--         , requestDisassociateOpsItemRelatedItem $
--             newDisassociateOpsItemRelatedItem
--
--         , requestListCommandInvocations $
--             newListCommandInvocations
--
--         , requestRegisterPatchBaselineForPatchGroup $
--             newRegisterPatchBaselineForPatchGroup
--
--         , requestDescribeEffectivePatchesForPatchBaseline $
--             newDescribeEffectivePatchesForPatchBaseline
--
--         , requestDescribeMaintenanceWindowTargets $
--             newDescribeMaintenanceWindowTargets
--
--         , requestDescribeAutomationExecutions $
--             newDescribeAutomationExecutions
--
--         , requestDescribePatchBaselines $
--             newDescribePatchBaselines
--
--         , requestDescribePatchGroupState $
--             newDescribePatchGroupState
--
--         , requestUpdateDocumentMetadata $
--             newUpdateDocumentMetadata
--
--         , requestGetPatchBaseline $
--             newGetPatchBaseline
--
--         , requestPutInventory $
--             newPutInventory
--
--         , requestGetAutomationExecution $
--             newGetAutomationExecution
--
--         , requestUpdateMaintenanceWindow $
--             newUpdateMaintenanceWindow
--
--         , requestDescribeInventoryDeletions $
--             newDescribeInventoryDeletions
--
--         , requestDeleteMaintenanceWindow $
--             newDeleteMaintenanceWindow
--
--         , requestListAssociations $
--             newListAssociations
--
--         , requestPutParameter $
--             newPutParameter
--
--         , requestGetMaintenanceWindowExecutionTask $
--             newGetMaintenanceWindowExecutionTask
--
--         , requestDescribeMaintenanceWindowExecutionTasks $
--             newDescribeMaintenanceWindowExecutionTasks
--
--         , requestGetDefaultPatchBaseline $
--             newGetDefaultPatchBaseline
--
--         , requestDescribeAssociationExecutions $
--             newDescribeAssociationExecutions
--
--         , requestCancelMaintenanceWindowExecution $
--             newCancelMaintenanceWindowExecution
--
--         , requestStopAutomationExecution $
--             newStopAutomationExecution
--
--         , requestGetInventorySchema $
--             newGetInventorySchema
--
--         , requestCreateMaintenanceWindow $
--             newCreateMaintenanceWindow
--
--         , requestGetParameterHistory $
--             newGetParameterHistory
--
--         , requestDeleteActivation $
--             newDeleteActivation
--
--         , requestUpdateMaintenanceWindowTask $
--             newUpdateMaintenanceWindowTask
--
--         , requestStartAutomationExecution $
--             newStartAutomationExecution
--
--         , requestGetServiceSetting $
--             newGetServiceSetting
--
--         , requestLabelParameterVersion $
--             newLabelParameterVersion
--
--         , requestGetMaintenanceWindowExecution $
--             newGetMaintenanceWindowExecution
--
--         , requestStartAssociationsOnce $
--             newStartAssociationsOnce
--
--         , requestDescribePatchProperties $
--             newDescribePatchProperties
--
--         , requestListInventoryEntries $
--             newListInventoryEntries
--
--         , requestDescribeMaintenanceWindowTasks $
--             newDescribeMaintenanceWindowTasks
--
--         , requestListDocumentMetadataHistory $
--             newListDocumentMetadataHistory
--
--         , requestDescribeActivations $
--             newDescribeActivations
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetParametersByPath $
--             newGetParametersByPath
--
--         , requestUpdateServiceSetting $
--             newUpdateServiceSetting
--
--         , requestDescribeInstanceAssociationsStatus $
--             newDescribeInstanceAssociationsStatus
--
--           ]

--     , testGroup "response"
--         [ responseDescribeOpsItems $
--             newDescribeOpsItemsResponse
--
--         , responseListResourceComplianceSummaries $
--             newListResourceComplianceSummariesResponse
--
--         , responseGetParameter $
--             newGetParameterResponse
--
--         , responseGetOpsMetadata $
--             newGetOpsMetadataResponse
--
--         , responseAssociateOpsItemRelatedItem $
--             newAssociateOpsItemRelatedItemResponse
--
--         , responseDescribeParameters $
--             newDescribeParametersResponse
--
--         , responseRegisterTaskWithMaintenanceWindow $
--             newRegisterTaskWithMaintenanceWindowResponse
--
--         , responseListResourceDataSync $
--             newListResourceDataSyncResponse
--
--         , responseGetOpsItem $
--             newGetOpsItemResponse
--
--         , responseDescribePatchGroups $
--             newDescribePatchGroupsResponse
--
--         , responseTerminateSession $
--             newTerminateSessionResponse
--
--         , responseUpdateDocumentDefaultVersion $
--             newUpdateDocumentDefaultVersionResponse
--
--         , responseGetInventory $
--             newGetInventoryResponse
--
--         , responseDescribeAssociation $
--             newDescribeAssociationResponse
--
--         , responseDescribeAssociationExecutionTargets $
--             newDescribeAssociationExecutionTargetsResponse
--
--         , responseDeregisterManagedInstance $
--             newDeregisterManagedInstanceResponse
--
--         , responseUpdateAssociationStatus $
--             newUpdateAssociationStatusResponse
--
--         , responseCreatePatchBaseline $
--             newCreatePatchBaselineResponse
--
--         , responseUpdateMaintenanceWindowTarget $
--             newUpdateMaintenanceWindowTargetResponse
--
--         , responseDescribeAutomationStepExecutions $
--             newDescribeAutomationStepExecutionsResponse
--
--         , responseGetCommandInvocation $
--             newGetCommandInvocationResponse
--
--         , responseDeregisterTargetFromMaintenanceWindow $
--             newDeregisterTargetFromMaintenanceWindowResponse
--
--         , responseGetOpsSummary $
--             newGetOpsSummaryResponse
--
--         , responseResetServiceSetting $
--             newResetServiceSettingResponse
--
--         , responseListDocuments $
--             newListDocumentsResponse
--
--         , responseDescribeInstancePatchStates $
--             newDescribeInstancePatchStatesResponse
--
--         , responseUnlabelParameterVersion $
--             newUnlabelParameterVersionResponse
--
--         , responseUpdateDocument $
--             newUpdateDocumentResponse
--
--         , responseDeleteDocument $
--             newDeleteDocumentResponse
--
--         , responseListCommands $
--             newListCommandsResponse
--
--         , responseStartSession $
--             newStartSessionResponse
--
--         , responseCreateDocument $
--             newCreateDocumentResponse
--
--         , responseDeleteInventory $
--             newDeleteInventoryResponse
--
--         , responseRemoveTagsFromResource $
--             newRemoveTagsFromResourceResponse
--
--         , responseDeleteParameters $
--             newDeleteParametersResponse
--
--         , responseSendCommand $
--             newSendCommandResponse
--
--         , responseGetCalendarState $
--             newGetCalendarStateResponse
--
--         , responseDescribeEffectiveInstanceAssociations $
--             newDescribeEffectiveInstanceAssociationsResponse
--
--         , responseRegisterTargetWithMaintenanceWindow $
--             newRegisterTargetWithMaintenanceWindowResponse
--
--         , responseListOpsMetadata $
--             newListOpsMetadataResponse
--
--         , responseDeleteParameter $
--             newDeleteParameterResponse
--
--         , responseCreateActivation $
--             newCreateActivationResponse
--
--         , responseUpdateAssociation $
--             newUpdateAssociationResponse
--
--         , responseDeleteOpsMetadata $
--             newDeleteOpsMetadataResponse
--
--         , responseUpdateOpsMetadata $
--             newUpdateOpsMetadataResponse
--
--         , responseDeleteAssociation $
--             newDeleteAssociationResponse
--
--         , responseDescribeMaintenanceWindowExecutionTaskInvocations $
--             newDescribeMaintenanceWindowExecutionTaskInvocationsResponse
--
--         , responseUpdateOpsItem $
--             newUpdateOpsItemResponse
--
--         , responseDescribeSessions $
--             newDescribeSessionsResponse
--
--         , responseDescribeInstanceInformation $
--             newDescribeInstanceInformationResponse
--
--         , responseGetMaintenanceWindowExecutionTaskInvocation $
--             newGetMaintenanceWindowExecutionTaskInvocationResponse
--
--         , responseStartChangeRequestExecution $
--             newStartChangeRequestExecutionResponse
--
--         , responseListComplianceSummaries $
--             newListComplianceSummariesResponse
--
--         , responseDescribeMaintenanceWindowExecutions $
--             newDescribeMaintenanceWindowExecutionsResponse
--
--         , responseCreateOpsItem $
--             newCreateOpsItemResponse
--
--         , responseDescribeMaintenanceWindowsForTarget $
--             newDescribeMaintenanceWindowsForTargetResponse
--
--         , responseCreateAssociation $
--             newCreateAssociationResponse
--
--         , responseCreateOpsMetadata $
--             newCreateOpsMetadataResponse
--
--         , responseSendAutomationSignal $
--             newSendAutomationSignalResponse
--
--         , responseDescribeDocument $
--             newDescribeDocumentResponse
--
--         , responseListAssociationVersions $
--             newListAssociationVersionsResponse
--
--         , responseListOpsItemRelatedItems $
--             newListOpsItemRelatedItemsResponse
--
--         , responsePutComplianceItems $
--             newPutComplianceItemsResponse
--
--         , responseDeregisterTaskFromMaintenanceWindow $
--             newDeregisterTaskFromMaintenanceWindowResponse
--
--         , responseGetMaintenanceWindowTask $
--             newGetMaintenanceWindowTaskResponse
--
--         , responseDeregisterPatchBaselineForPatchGroup $
--             newDeregisterPatchBaselineForPatchGroupResponse
--
--         , responseDescribeMaintenanceWindows $
--             newDescribeMaintenanceWindowsResponse
--
--         , responseGetDeployablePatchSnapshotForInstance $
--             newGetDeployablePatchSnapshotForInstanceResponse
--
--         , responseResumeSession $
--             newResumeSessionResponse
--
--         , responseGetMaintenanceWindow $
--             newGetMaintenanceWindowResponse
--
--         , responseRegisterDefaultPatchBaseline $
--             newRegisterDefaultPatchBaselineResponse
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
--         , responseUpdatePatchBaseline $
--             newUpdatePatchBaselineResponse
--
--         , responseDescribeInstancePatches $
--             newDescribeInstancePatchesResponse
--
--         , responseDescribeAvailablePatches $
--             newDescribeAvailablePatchesResponse
--
--         , responseDescribeInstancePatchStatesForPatchGroup $
--             newDescribeInstancePatchStatesForPatchGroupResponse
--
--         , responseDeleteResourceDataSync $
--             newDeleteResourceDataSyncResponse
--
--         , responseGetParameters $
--             newGetParametersResponse
--
--         , responseGetConnectionStatus $
--             newGetConnectionStatusResponse
--
--         , responseModifyDocumentPermission $
--             newModifyDocumentPermissionResponse
--
--         , responseListOpsItemEvents $
--             newListOpsItemEventsResponse
--
--         , responseAddTagsToResource $
--             newAddTagsToResourceResponse
--
--         , responseDescribeDocumentPermission $
--             newDescribeDocumentPermissionResponse
--
--         , responseCreateResourceDataSync $
--             newCreateResourceDataSyncResponse
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
--         , responseCancelCommand $
--             newCancelCommandResponse
--
--         , responseGetDocument $
--             newGetDocumentResponse
--
--         , responseListComplianceItems $
--             newListComplianceItemsResponse
--
--         , responseCreateAssociationBatch $
--             newCreateAssociationBatchResponse
--
--         , responseDisassociateOpsItemRelatedItem $
--             newDisassociateOpsItemRelatedItemResponse
--
--         , responseListCommandInvocations $
--             newListCommandInvocationsResponse
--
--         , responseRegisterPatchBaselineForPatchGroup $
--             newRegisterPatchBaselineForPatchGroupResponse
--
--         , responseDescribeEffectivePatchesForPatchBaseline $
--             newDescribeEffectivePatchesForPatchBaselineResponse
--
--         , responseDescribeMaintenanceWindowTargets $
--             newDescribeMaintenanceWindowTargetsResponse
--
--         , responseDescribeAutomationExecutions $
--             newDescribeAutomationExecutionsResponse
--
--         , responseDescribePatchBaselines $
--             newDescribePatchBaselinesResponse
--
--         , responseDescribePatchGroupState $
--             newDescribePatchGroupStateResponse
--
--         , responseUpdateDocumentMetadata $
--             newUpdateDocumentMetadataResponse
--
--         , responseGetPatchBaseline $
--             newGetPatchBaselineResponse
--
--         , responsePutInventory $
--             newPutInventoryResponse
--
--         , responseGetAutomationExecution $
--             newGetAutomationExecutionResponse
--
--         , responseUpdateMaintenanceWindow $
--             newUpdateMaintenanceWindowResponse
--
--         , responseDescribeInventoryDeletions $
--             newDescribeInventoryDeletionsResponse
--
--         , responseDeleteMaintenanceWindow $
--             newDeleteMaintenanceWindowResponse
--
--         , responseListAssociations $
--             newListAssociationsResponse
--
--         , responsePutParameter $
--             newPutParameterResponse
--
--         , responseGetMaintenanceWindowExecutionTask $
--             newGetMaintenanceWindowExecutionTaskResponse
--
--         , responseDescribeMaintenanceWindowExecutionTasks $
--             newDescribeMaintenanceWindowExecutionTasksResponse
--
--         , responseGetDefaultPatchBaseline $
--             newGetDefaultPatchBaselineResponse
--
--         , responseDescribeAssociationExecutions $
--             newDescribeAssociationExecutionsResponse
--
--         , responseCancelMaintenanceWindowExecution $
--             newCancelMaintenanceWindowExecutionResponse
--
--         , responseStopAutomationExecution $
--             newStopAutomationExecutionResponse
--
--         , responseGetInventorySchema $
--             newGetInventorySchemaResponse
--
--         , responseCreateMaintenanceWindow $
--             newCreateMaintenanceWindowResponse
--
--         , responseGetParameterHistory $
--             newGetParameterHistoryResponse
--
--         , responseDeleteActivation $
--             newDeleteActivationResponse
--
--         , responseUpdateMaintenanceWindowTask $
--             newUpdateMaintenanceWindowTaskResponse
--
--         , responseStartAutomationExecution $
--             newStartAutomationExecutionResponse
--
--         , responseGetServiceSetting $
--             newGetServiceSettingResponse
--
--         , responseLabelParameterVersion $
--             newLabelParameterVersionResponse
--
--         , responseGetMaintenanceWindowExecution $
--             newGetMaintenanceWindowExecutionResponse
--
--         , responseStartAssociationsOnce $
--             newStartAssociationsOnceResponse
--
--         , responseDescribePatchProperties $
--             newDescribePatchPropertiesResponse
--
--         , responseListInventoryEntries $
--             newListInventoryEntriesResponse
--
--         , responseDescribeMaintenanceWindowTasks $
--             newDescribeMaintenanceWindowTasksResponse
--
--         , responseListDocumentMetadataHistory $
--             newListDocumentMetadataHistoryResponse
--
--         , responseDescribeActivations $
--             newDescribeActivationsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetParametersByPath $
--             newGetParametersByPathResponse
--
--         , responseUpdateServiceSetting $
--             newUpdateServiceSettingResponse
--
--         , responseDescribeInstanceAssociationsStatus $
--             newDescribeInstanceAssociationsStatusResponse
--
--           ]
--     ]

-- Requests

requestDescribeOpsItems :: DescribeOpsItems -> TestTree
requestDescribeOpsItems =
  req
    "DescribeOpsItems"
    "fixture/DescribeOpsItems.yaml"

requestListResourceComplianceSummaries :: ListResourceComplianceSummaries -> TestTree
requestListResourceComplianceSummaries =
  req
    "ListResourceComplianceSummaries"
    "fixture/ListResourceComplianceSummaries.yaml"

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

requestAssociateOpsItemRelatedItem :: AssociateOpsItemRelatedItem -> TestTree
requestAssociateOpsItemRelatedItem =
  req
    "AssociateOpsItemRelatedItem"
    "fixture/AssociateOpsItemRelatedItem.yaml"

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

requestListResourceDataSync :: ListResourceDataSync -> TestTree
requestListResourceDataSync =
  req
    "ListResourceDataSync"
    "fixture/ListResourceDataSync.yaml"

requestGetOpsItem :: GetOpsItem -> TestTree
requestGetOpsItem =
  req
    "GetOpsItem"
    "fixture/GetOpsItem.yaml"

requestDescribePatchGroups :: DescribePatchGroups -> TestTree
requestDescribePatchGroups =
  req
    "DescribePatchGroups"
    "fixture/DescribePatchGroups.yaml"

requestTerminateSession :: TerminateSession -> TestTree
requestTerminateSession =
  req
    "TerminateSession"
    "fixture/TerminateSession.yaml"

requestUpdateDocumentDefaultVersion :: UpdateDocumentDefaultVersion -> TestTree
requestUpdateDocumentDefaultVersion =
  req
    "UpdateDocumentDefaultVersion"
    "fixture/UpdateDocumentDefaultVersion.yaml"

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

requestDescribeAssociationExecutionTargets :: DescribeAssociationExecutionTargets -> TestTree
requestDescribeAssociationExecutionTargets =
  req
    "DescribeAssociationExecutionTargets"
    "fixture/DescribeAssociationExecutionTargets.yaml"

requestDeregisterManagedInstance :: DeregisterManagedInstance -> TestTree
requestDeregisterManagedInstance =
  req
    "DeregisterManagedInstance"
    "fixture/DeregisterManagedInstance.yaml"

requestUpdateAssociationStatus :: UpdateAssociationStatus -> TestTree
requestUpdateAssociationStatus =
  req
    "UpdateAssociationStatus"
    "fixture/UpdateAssociationStatus.yaml"

requestCreatePatchBaseline :: CreatePatchBaseline -> TestTree
requestCreatePatchBaseline =
  req
    "CreatePatchBaseline"
    "fixture/CreatePatchBaseline.yaml"

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

requestGetCommandInvocation :: GetCommandInvocation -> TestTree
requestGetCommandInvocation =
  req
    "GetCommandInvocation"
    "fixture/GetCommandInvocation.yaml"

requestDeregisterTargetFromMaintenanceWindow :: DeregisterTargetFromMaintenanceWindow -> TestTree
requestDeregisterTargetFromMaintenanceWindow =
  req
    "DeregisterTargetFromMaintenanceWindow"
    "fixture/DeregisterTargetFromMaintenanceWindow.yaml"

requestGetOpsSummary :: GetOpsSummary -> TestTree
requestGetOpsSummary =
  req
    "GetOpsSummary"
    "fixture/GetOpsSummary.yaml"

requestResetServiceSetting :: ResetServiceSetting -> TestTree
requestResetServiceSetting =
  req
    "ResetServiceSetting"
    "fixture/ResetServiceSetting.yaml"

requestListDocuments :: ListDocuments -> TestTree
requestListDocuments =
  req
    "ListDocuments"
    "fixture/ListDocuments.yaml"

requestDescribeInstancePatchStates :: DescribeInstancePatchStates -> TestTree
requestDescribeInstancePatchStates =
  req
    "DescribeInstancePatchStates"
    "fixture/DescribeInstancePatchStates.yaml"

requestUnlabelParameterVersion :: UnlabelParameterVersion -> TestTree
requestUnlabelParameterVersion =
  req
    "UnlabelParameterVersion"
    "fixture/UnlabelParameterVersion.yaml"

requestUpdateDocument :: UpdateDocument -> TestTree
requestUpdateDocument =
  req
    "UpdateDocument"
    "fixture/UpdateDocument.yaml"

requestDeleteDocument :: DeleteDocument -> TestTree
requestDeleteDocument =
  req
    "DeleteDocument"
    "fixture/DeleteDocument.yaml"

requestListCommands :: ListCommands -> TestTree
requestListCommands =
  req
    "ListCommands"
    "fixture/ListCommands.yaml"

requestStartSession :: StartSession -> TestTree
requestStartSession =
  req
    "StartSession"
    "fixture/StartSession.yaml"

requestCreateDocument :: CreateDocument -> TestTree
requestCreateDocument =
  req
    "CreateDocument"
    "fixture/CreateDocument.yaml"

requestDeleteInventory :: DeleteInventory -> TestTree
requestDeleteInventory =
  req
    "DeleteInventory"
    "fixture/DeleteInventory.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource =
  req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestDeleteParameters :: DeleteParameters -> TestTree
requestDeleteParameters =
  req
    "DeleteParameters"
    "fixture/DeleteParameters.yaml"

requestSendCommand :: SendCommand -> TestTree
requestSendCommand =
  req
    "SendCommand"
    "fixture/SendCommand.yaml"

requestGetCalendarState :: GetCalendarState -> TestTree
requestGetCalendarState =
  req
    "GetCalendarState"
    "fixture/GetCalendarState.yaml"

requestDescribeEffectiveInstanceAssociations :: DescribeEffectiveInstanceAssociations -> TestTree
requestDescribeEffectiveInstanceAssociations =
  req
    "DescribeEffectiveInstanceAssociations"
    "fixture/DescribeEffectiveInstanceAssociations.yaml"

requestRegisterTargetWithMaintenanceWindow :: RegisterTargetWithMaintenanceWindow -> TestTree
requestRegisterTargetWithMaintenanceWindow =
  req
    "RegisterTargetWithMaintenanceWindow"
    "fixture/RegisterTargetWithMaintenanceWindow.yaml"

requestListOpsMetadata :: ListOpsMetadata -> TestTree
requestListOpsMetadata =
  req
    "ListOpsMetadata"
    "fixture/ListOpsMetadata.yaml"

requestDeleteParameter :: DeleteParameter -> TestTree
requestDeleteParameter =
  req
    "DeleteParameter"
    "fixture/DeleteParameter.yaml"

requestCreateActivation :: CreateActivation -> TestTree
requestCreateActivation =
  req
    "CreateActivation"
    "fixture/CreateActivation.yaml"

requestUpdateAssociation :: UpdateAssociation -> TestTree
requestUpdateAssociation =
  req
    "UpdateAssociation"
    "fixture/UpdateAssociation.yaml"

requestDeleteOpsMetadata :: DeleteOpsMetadata -> TestTree
requestDeleteOpsMetadata =
  req
    "DeleteOpsMetadata"
    "fixture/DeleteOpsMetadata.yaml"

requestUpdateOpsMetadata :: UpdateOpsMetadata -> TestTree
requestUpdateOpsMetadata =
  req
    "UpdateOpsMetadata"
    "fixture/UpdateOpsMetadata.yaml"

requestDeleteAssociation :: DeleteAssociation -> TestTree
requestDeleteAssociation =
  req
    "DeleteAssociation"
    "fixture/DeleteAssociation.yaml"

requestDescribeMaintenanceWindowExecutionTaskInvocations :: DescribeMaintenanceWindowExecutionTaskInvocations -> TestTree
requestDescribeMaintenanceWindowExecutionTaskInvocations =
  req
    "DescribeMaintenanceWindowExecutionTaskInvocations"
    "fixture/DescribeMaintenanceWindowExecutionTaskInvocations.yaml"

requestUpdateOpsItem :: UpdateOpsItem -> TestTree
requestUpdateOpsItem =
  req
    "UpdateOpsItem"
    "fixture/UpdateOpsItem.yaml"

requestDescribeSessions :: DescribeSessions -> TestTree
requestDescribeSessions =
  req
    "DescribeSessions"
    "fixture/DescribeSessions.yaml"

requestDescribeInstanceInformation :: DescribeInstanceInformation -> TestTree
requestDescribeInstanceInformation =
  req
    "DescribeInstanceInformation"
    "fixture/DescribeInstanceInformation.yaml"

requestGetMaintenanceWindowExecutionTaskInvocation :: GetMaintenanceWindowExecutionTaskInvocation -> TestTree
requestGetMaintenanceWindowExecutionTaskInvocation =
  req
    "GetMaintenanceWindowExecutionTaskInvocation"
    "fixture/GetMaintenanceWindowExecutionTaskInvocation.yaml"

requestStartChangeRequestExecution :: StartChangeRequestExecution -> TestTree
requestStartChangeRequestExecution =
  req
    "StartChangeRequestExecution"
    "fixture/StartChangeRequestExecution.yaml"

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

requestCreateOpsItem :: CreateOpsItem -> TestTree
requestCreateOpsItem =
  req
    "CreateOpsItem"
    "fixture/CreateOpsItem.yaml"

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

requestDescribeDocument :: DescribeDocument -> TestTree
requestDescribeDocument =
  req
    "DescribeDocument"
    "fixture/DescribeDocument.yaml"

requestListAssociationVersions :: ListAssociationVersions -> TestTree
requestListAssociationVersions =
  req
    "ListAssociationVersions"
    "fixture/ListAssociationVersions.yaml"

requestListOpsItemRelatedItems :: ListOpsItemRelatedItems -> TestTree
requestListOpsItemRelatedItems =
  req
    "ListOpsItemRelatedItems"
    "fixture/ListOpsItemRelatedItems.yaml"

requestPutComplianceItems :: PutComplianceItems -> TestTree
requestPutComplianceItems =
  req
    "PutComplianceItems"
    "fixture/PutComplianceItems.yaml"

requestDeregisterTaskFromMaintenanceWindow :: DeregisterTaskFromMaintenanceWindow -> TestTree
requestDeregisterTaskFromMaintenanceWindow =
  req
    "DeregisterTaskFromMaintenanceWindow"
    "fixture/DeregisterTaskFromMaintenanceWindow.yaml"

requestGetMaintenanceWindowTask :: GetMaintenanceWindowTask -> TestTree
requestGetMaintenanceWindowTask =
  req
    "GetMaintenanceWindowTask"
    "fixture/GetMaintenanceWindowTask.yaml"

requestDeregisterPatchBaselineForPatchGroup :: DeregisterPatchBaselineForPatchGroup -> TestTree
requestDeregisterPatchBaselineForPatchGroup =
  req
    "DeregisterPatchBaselineForPatchGroup"
    "fixture/DeregisterPatchBaselineForPatchGroup.yaml"

requestDescribeMaintenanceWindows :: DescribeMaintenanceWindows -> TestTree
requestDescribeMaintenanceWindows =
  req
    "DescribeMaintenanceWindows"
    "fixture/DescribeMaintenanceWindows.yaml"

requestGetDeployablePatchSnapshotForInstance :: GetDeployablePatchSnapshotForInstance -> TestTree
requestGetDeployablePatchSnapshotForInstance =
  req
    "GetDeployablePatchSnapshotForInstance"
    "fixture/GetDeployablePatchSnapshotForInstance.yaml"

requestResumeSession :: ResumeSession -> TestTree
requestResumeSession =
  req
    "ResumeSession"
    "fixture/ResumeSession.yaml"

requestGetMaintenanceWindow :: GetMaintenanceWindow -> TestTree
requestGetMaintenanceWindow =
  req
    "GetMaintenanceWindow"
    "fixture/GetMaintenanceWindow.yaml"

requestRegisterDefaultPatchBaseline :: RegisterDefaultPatchBaseline -> TestTree
requestRegisterDefaultPatchBaseline =
  req
    "RegisterDefaultPatchBaseline"
    "fixture/RegisterDefaultPatchBaseline.yaml"

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

requestUpdatePatchBaseline :: UpdatePatchBaseline -> TestTree
requestUpdatePatchBaseline =
  req
    "UpdatePatchBaseline"
    "fixture/UpdatePatchBaseline.yaml"

requestDescribeInstancePatches :: DescribeInstancePatches -> TestTree
requestDescribeInstancePatches =
  req
    "DescribeInstancePatches"
    "fixture/DescribeInstancePatches.yaml"

requestDescribeAvailablePatches :: DescribeAvailablePatches -> TestTree
requestDescribeAvailablePatches =
  req
    "DescribeAvailablePatches"
    "fixture/DescribeAvailablePatches.yaml"

requestDescribeInstancePatchStatesForPatchGroup :: DescribeInstancePatchStatesForPatchGroup -> TestTree
requestDescribeInstancePatchStatesForPatchGroup =
  req
    "DescribeInstancePatchStatesForPatchGroup"
    "fixture/DescribeInstancePatchStatesForPatchGroup.yaml"

requestDeleteResourceDataSync :: DeleteResourceDataSync -> TestTree
requestDeleteResourceDataSync =
  req
    "DeleteResourceDataSync"
    "fixture/DeleteResourceDataSync.yaml"

requestGetParameters :: GetParameters -> TestTree
requestGetParameters =
  req
    "GetParameters"
    "fixture/GetParameters.yaml"

requestGetConnectionStatus :: GetConnectionStatus -> TestTree
requestGetConnectionStatus =
  req
    "GetConnectionStatus"
    "fixture/GetConnectionStatus.yaml"

requestModifyDocumentPermission :: ModifyDocumentPermission -> TestTree
requestModifyDocumentPermission =
  req
    "ModifyDocumentPermission"
    "fixture/ModifyDocumentPermission.yaml"

requestListOpsItemEvents :: ListOpsItemEvents -> TestTree
requestListOpsItemEvents =
  req
    "ListOpsItemEvents"
    "fixture/ListOpsItemEvents.yaml"

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource =
  req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestDescribeDocumentPermission :: DescribeDocumentPermission -> TestTree
requestDescribeDocumentPermission =
  req
    "DescribeDocumentPermission"
    "fixture/DescribeDocumentPermission.yaml"

requestCreateResourceDataSync :: CreateResourceDataSync -> TestTree
requestCreateResourceDataSync =
  req
    "CreateResourceDataSync"
    "fixture/CreateResourceDataSync.yaml"

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

requestCancelCommand :: CancelCommand -> TestTree
requestCancelCommand =
  req
    "CancelCommand"
    "fixture/CancelCommand.yaml"

requestGetDocument :: GetDocument -> TestTree
requestGetDocument =
  req
    "GetDocument"
    "fixture/GetDocument.yaml"

requestListComplianceItems :: ListComplianceItems -> TestTree
requestListComplianceItems =
  req
    "ListComplianceItems"
    "fixture/ListComplianceItems.yaml"

requestCreateAssociationBatch :: CreateAssociationBatch -> TestTree
requestCreateAssociationBatch =
  req
    "CreateAssociationBatch"
    "fixture/CreateAssociationBatch.yaml"

requestDisassociateOpsItemRelatedItem :: DisassociateOpsItemRelatedItem -> TestTree
requestDisassociateOpsItemRelatedItem =
  req
    "DisassociateOpsItemRelatedItem"
    "fixture/DisassociateOpsItemRelatedItem.yaml"

requestListCommandInvocations :: ListCommandInvocations -> TestTree
requestListCommandInvocations =
  req
    "ListCommandInvocations"
    "fixture/ListCommandInvocations.yaml"

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

requestDescribeMaintenanceWindowTargets :: DescribeMaintenanceWindowTargets -> TestTree
requestDescribeMaintenanceWindowTargets =
  req
    "DescribeMaintenanceWindowTargets"
    "fixture/DescribeMaintenanceWindowTargets.yaml"

requestDescribeAutomationExecutions :: DescribeAutomationExecutions -> TestTree
requestDescribeAutomationExecutions =
  req
    "DescribeAutomationExecutions"
    "fixture/DescribeAutomationExecutions.yaml"

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

requestUpdateDocumentMetadata :: UpdateDocumentMetadata -> TestTree
requestUpdateDocumentMetadata =
  req
    "UpdateDocumentMetadata"
    "fixture/UpdateDocumentMetadata.yaml"

requestGetPatchBaseline :: GetPatchBaseline -> TestTree
requestGetPatchBaseline =
  req
    "GetPatchBaseline"
    "fixture/GetPatchBaseline.yaml"

requestPutInventory :: PutInventory -> TestTree
requestPutInventory =
  req
    "PutInventory"
    "fixture/PutInventory.yaml"

requestGetAutomationExecution :: GetAutomationExecution -> TestTree
requestGetAutomationExecution =
  req
    "GetAutomationExecution"
    "fixture/GetAutomationExecution.yaml"

requestUpdateMaintenanceWindow :: UpdateMaintenanceWindow -> TestTree
requestUpdateMaintenanceWindow =
  req
    "UpdateMaintenanceWindow"
    "fixture/UpdateMaintenanceWindow.yaml"

requestDescribeInventoryDeletions :: DescribeInventoryDeletions -> TestTree
requestDescribeInventoryDeletions =
  req
    "DescribeInventoryDeletions"
    "fixture/DescribeInventoryDeletions.yaml"

requestDeleteMaintenanceWindow :: DeleteMaintenanceWindow -> TestTree
requestDeleteMaintenanceWindow =
  req
    "DeleteMaintenanceWindow"
    "fixture/DeleteMaintenanceWindow.yaml"

requestListAssociations :: ListAssociations -> TestTree
requestListAssociations =
  req
    "ListAssociations"
    "fixture/ListAssociations.yaml"

requestPutParameter :: PutParameter -> TestTree
requestPutParameter =
  req
    "PutParameter"
    "fixture/PutParameter.yaml"

requestGetMaintenanceWindowExecutionTask :: GetMaintenanceWindowExecutionTask -> TestTree
requestGetMaintenanceWindowExecutionTask =
  req
    "GetMaintenanceWindowExecutionTask"
    "fixture/GetMaintenanceWindowExecutionTask.yaml"

requestDescribeMaintenanceWindowExecutionTasks :: DescribeMaintenanceWindowExecutionTasks -> TestTree
requestDescribeMaintenanceWindowExecutionTasks =
  req
    "DescribeMaintenanceWindowExecutionTasks"
    "fixture/DescribeMaintenanceWindowExecutionTasks.yaml"

requestGetDefaultPatchBaseline :: GetDefaultPatchBaseline -> TestTree
requestGetDefaultPatchBaseline =
  req
    "GetDefaultPatchBaseline"
    "fixture/GetDefaultPatchBaseline.yaml"

requestDescribeAssociationExecutions :: DescribeAssociationExecutions -> TestTree
requestDescribeAssociationExecutions =
  req
    "DescribeAssociationExecutions"
    "fixture/DescribeAssociationExecutions.yaml"

requestCancelMaintenanceWindowExecution :: CancelMaintenanceWindowExecution -> TestTree
requestCancelMaintenanceWindowExecution =
  req
    "CancelMaintenanceWindowExecution"
    "fixture/CancelMaintenanceWindowExecution.yaml"

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

requestCreateMaintenanceWindow :: CreateMaintenanceWindow -> TestTree
requestCreateMaintenanceWindow =
  req
    "CreateMaintenanceWindow"
    "fixture/CreateMaintenanceWindow.yaml"

requestGetParameterHistory :: GetParameterHistory -> TestTree
requestGetParameterHistory =
  req
    "GetParameterHistory"
    "fixture/GetParameterHistory.yaml"

requestDeleteActivation :: DeleteActivation -> TestTree
requestDeleteActivation =
  req
    "DeleteActivation"
    "fixture/DeleteActivation.yaml"

requestUpdateMaintenanceWindowTask :: UpdateMaintenanceWindowTask -> TestTree
requestUpdateMaintenanceWindowTask =
  req
    "UpdateMaintenanceWindowTask"
    "fixture/UpdateMaintenanceWindowTask.yaml"

requestStartAutomationExecution :: StartAutomationExecution -> TestTree
requestStartAutomationExecution =
  req
    "StartAutomationExecution"
    "fixture/StartAutomationExecution.yaml"

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

requestGetMaintenanceWindowExecution :: GetMaintenanceWindowExecution -> TestTree
requestGetMaintenanceWindowExecution =
  req
    "GetMaintenanceWindowExecution"
    "fixture/GetMaintenanceWindowExecution.yaml"

requestStartAssociationsOnce :: StartAssociationsOnce -> TestTree
requestStartAssociationsOnce =
  req
    "StartAssociationsOnce"
    "fixture/StartAssociationsOnce.yaml"

requestDescribePatchProperties :: DescribePatchProperties -> TestTree
requestDescribePatchProperties =
  req
    "DescribePatchProperties"
    "fixture/DescribePatchProperties.yaml"

requestListInventoryEntries :: ListInventoryEntries -> TestTree
requestListInventoryEntries =
  req
    "ListInventoryEntries"
    "fixture/ListInventoryEntries.yaml"

requestDescribeMaintenanceWindowTasks :: DescribeMaintenanceWindowTasks -> TestTree
requestDescribeMaintenanceWindowTasks =
  req
    "DescribeMaintenanceWindowTasks"
    "fixture/DescribeMaintenanceWindowTasks.yaml"

requestListDocumentMetadataHistory :: ListDocumentMetadataHistory -> TestTree
requestListDocumentMetadataHistory =
  req
    "ListDocumentMetadataHistory"
    "fixture/ListDocumentMetadataHistory.yaml"

requestDescribeActivations :: DescribeActivations -> TestTree
requestDescribeActivations =
  req
    "DescribeActivations"
    "fixture/DescribeActivations.yaml"

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

requestUpdateServiceSetting :: UpdateServiceSetting -> TestTree
requestUpdateServiceSetting =
  req
    "UpdateServiceSetting"
    "fixture/UpdateServiceSetting.yaml"

requestDescribeInstanceAssociationsStatus :: DescribeInstanceAssociationsStatus -> TestTree
requestDescribeInstanceAssociationsStatus =
  req
    "DescribeInstanceAssociationsStatus"
    "fixture/DescribeInstanceAssociationsStatus.yaml"

-- Responses

responseDescribeOpsItems :: DescribeOpsItemsResponse -> TestTree
responseDescribeOpsItems =
  res
    "DescribeOpsItemsResponse"
    "fixture/DescribeOpsItemsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeOpsItems)

responseListResourceComplianceSummaries :: ListResourceComplianceSummariesResponse -> TestTree
responseListResourceComplianceSummaries =
  res
    "ListResourceComplianceSummariesResponse"
    "fixture/ListResourceComplianceSummariesResponse.proto"
    defaultService
    (Proxy :: Proxy ListResourceComplianceSummaries)

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

responseAssociateOpsItemRelatedItem :: AssociateOpsItemRelatedItemResponse -> TestTree
responseAssociateOpsItemRelatedItem =
  res
    "AssociateOpsItemRelatedItemResponse"
    "fixture/AssociateOpsItemRelatedItemResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateOpsItemRelatedItem)

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

responseListResourceDataSync :: ListResourceDataSyncResponse -> TestTree
responseListResourceDataSync =
  res
    "ListResourceDataSyncResponse"
    "fixture/ListResourceDataSyncResponse.proto"
    defaultService
    (Proxy :: Proxy ListResourceDataSync)

responseGetOpsItem :: GetOpsItemResponse -> TestTree
responseGetOpsItem =
  res
    "GetOpsItemResponse"
    "fixture/GetOpsItemResponse.proto"
    defaultService
    (Proxy :: Proxy GetOpsItem)

responseDescribePatchGroups :: DescribePatchGroupsResponse -> TestTree
responseDescribePatchGroups =
  res
    "DescribePatchGroupsResponse"
    "fixture/DescribePatchGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePatchGroups)

responseTerminateSession :: TerminateSessionResponse -> TestTree
responseTerminateSession =
  res
    "TerminateSessionResponse"
    "fixture/TerminateSessionResponse.proto"
    defaultService
    (Proxy :: Proxy TerminateSession)

responseUpdateDocumentDefaultVersion :: UpdateDocumentDefaultVersionResponse -> TestTree
responseUpdateDocumentDefaultVersion =
  res
    "UpdateDocumentDefaultVersionResponse"
    "fixture/UpdateDocumentDefaultVersionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDocumentDefaultVersion)

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

responseDescribeAssociationExecutionTargets :: DescribeAssociationExecutionTargetsResponse -> TestTree
responseDescribeAssociationExecutionTargets =
  res
    "DescribeAssociationExecutionTargetsResponse"
    "fixture/DescribeAssociationExecutionTargetsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAssociationExecutionTargets)

responseDeregisterManagedInstance :: DeregisterManagedInstanceResponse -> TestTree
responseDeregisterManagedInstance =
  res
    "DeregisterManagedInstanceResponse"
    "fixture/DeregisterManagedInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterManagedInstance)

responseUpdateAssociationStatus :: UpdateAssociationStatusResponse -> TestTree
responseUpdateAssociationStatus =
  res
    "UpdateAssociationStatusResponse"
    "fixture/UpdateAssociationStatusResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAssociationStatus)

responseCreatePatchBaseline :: CreatePatchBaselineResponse -> TestTree
responseCreatePatchBaseline =
  res
    "CreatePatchBaselineResponse"
    "fixture/CreatePatchBaselineResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePatchBaseline)

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

responseGetCommandInvocation :: GetCommandInvocationResponse -> TestTree
responseGetCommandInvocation =
  res
    "GetCommandInvocationResponse"
    "fixture/GetCommandInvocationResponse.proto"
    defaultService
    (Proxy :: Proxy GetCommandInvocation)

responseDeregisterTargetFromMaintenanceWindow :: DeregisterTargetFromMaintenanceWindowResponse -> TestTree
responseDeregisterTargetFromMaintenanceWindow =
  res
    "DeregisterTargetFromMaintenanceWindowResponse"
    "fixture/DeregisterTargetFromMaintenanceWindowResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterTargetFromMaintenanceWindow)

responseGetOpsSummary :: GetOpsSummaryResponse -> TestTree
responseGetOpsSummary =
  res
    "GetOpsSummaryResponse"
    "fixture/GetOpsSummaryResponse.proto"
    defaultService
    (Proxy :: Proxy GetOpsSummary)

responseResetServiceSetting :: ResetServiceSettingResponse -> TestTree
responseResetServiceSetting =
  res
    "ResetServiceSettingResponse"
    "fixture/ResetServiceSettingResponse.proto"
    defaultService
    (Proxy :: Proxy ResetServiceSetting)

responseListDocuments :: ListDocumentsResponse -> TestTree
responseListDocuments =
  res
    "ListDocumentsResponse"
    "fixture/ListDocumentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDocuments)

responseDescribeInstancePatchStates :: DescribeInstancePatchStatesResponse -> TestTree
responseDescribeInstancePatchStates =
  res
    "DescribeInstancePatchStatesResponse"
    "fixture/DescribeInstancePatchStatesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstancePatchStates)

responseUnlabelParameterVersion :: UnlabelParameterVersionResponse -> TestTree
responseUnlabelParameterVersion =
  res
    "UnlabelParameterVersionResponse"
    "fixture/UnlabelParameterVersionResponse.proto"
    defaultService
    (Proxy :: Proxy UnlabelParameterVersion)

responseUpdateDocument :: UpdateDocumentResponse -> TestTree
responseUpdateDocument =
  res
    "UpdateDocumentResponse"
    "fixture/UpdateDocumentResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDocument)

responseDeleteDocument :: DeleteDocumentResponse -> TestTree
responseDeleteDocument =
  res
    "DeleteDocumentResponse"
    "fixture/DeleteDocumentResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDocument)

responseListCommands :: ListCommandsResponse -> TestTree
responseListCommands =
  res
    "ListCommandsResponse"
    "fixture/ListCommandsResponse.proto"
    defaultService
    (Proxy :: Proxy ListCommands)

responseStartSession :: StartSessionResponse -> TestTree
responseStartSession =
  res
    "StartSessionResponse"
    "fixture/StartSessionResponse.proto"
    defaultService
    (Proxy :: Proxy StartSession)

responseCreateDocument :: CreateDocumentResponse -> TestTree
responseCreateDocument =
  res
    "CreateDocumentResponse"
    "fixture/CreateDocumentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDocument)

responseDeleteInventory :: DeleteInventoryResponse -> TestTree
responseDeleteInventory =
  res
    "DeleteInventoryResponse"
    "fixture/DeleteInventoryResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteInventory)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveTagsFromResource)

responseDeleteParameters :: DeleteParametersResponse -> TestTree
responseDeleteParameters =
  res
    "DeleteParametersResponse"
    "fixture/DeleteParametersResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteParameters)

responseSendCommand :: SendCommandResponse -> TestTree
responseSendCommand =
  res
    "SendCommandResponse"
    "fixture/SendCommandResponse.proto"
    defaultService
    (Proxy :: Proxy SendCommand)

responseGetCalendarState :: GetCalendarStateResponse -> TestTree
responseGetCalendarState =
  res
    "GetCalendarStateResponse"
    "fixture/GetCalendarStateResponse.proto"
    defaultService
    (Proxy :: Proxy GetCalendarState)

responseDescribeEffectiveInstanceAssociations :: DescribeEffectiveInstanceAssociationsResponse -> TestTree
responseDescribeEffectiveInstanceAssociations =
  res
    "DescribeEffectiveInstanceAssociationsResponse"
    "fixture/DescribeEffectiveInstanceAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEffectiveInstanceAssociations)

responseRegisterTargetWithMaintenanceWindow :: RegisterTargetWithMaintenanceWindowResponse -> TestTree
responseRegisterTargetWithMaintenanceWindow =
  res
    "RegisterTargetWithMaintenanceWindowResponse"
    "fixture/RegisterTargetWithMaintenanceWindowResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterTargetWithMaintenanceWindow)

responseListOpsMetadata :: ListOpsMetadataResponse -> TestTree
responseListOpsMetadata =
  res
    "ListOpsMetadataResponse"
    "fixture/ListOpsMetadataResponse.proto"
    defaultService
    (Proxy :: Proxy ListOpsMetadata)

responseDeleteParameter :: DeleteParameterResponse -> TestTree
responseDeleteParameter =
  res
    "DeleteParameterResponse"
    "fixture/DeleteParameterResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteParameter)

responseCreateActivation :: CreateActivationResponse -> TestTree
responseCreateActivation =
  res
    "CreateActivationResponse"
    "fixture/CreateActivationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateActivation)

responseUpdateAssociation :: UpdateAssociationResponse -> TestTree
responseUpdateAssociation =
  res
    "UpdateAssociationResponse"
    "fixture/UpdateAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAssociation)

responseDeleteOpsMetadata :: DeleteOpsMetadataResponse -> TestTree
responseDeleteOpsMetadata =
  res
    "DeleteOpsMetadataResponse"
    "fixture/DeleteOpsMetadataResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteOpsMetadata)

responseUpdateOpsMetadata :: UpdateOpsMetadataResponse -> TestTree
responseUpdateOpsMetadata =
  res
    "UpdateOpsMetadataResponse"
    "fixture/UpdateOpsMetadataResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateOpsMetadata)

responseDeleteAssociation :: DeleteAssociationResponse -> TestTree
responseDeleteAssociation =
  res
    "DeleteAssociationResponse"
    "fixture/DeleteAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAssociation)

responseDescribeMaintenanceWindowExecutionTaskInvocations :: DescribeMaintenanceWindowExecutionTaskInvocationsResponse -> TestTree
responseDescribeMaintenanceWindowExecutionTaskInvocations =
  res
    "DescribeMaintenanceWindowExecutionTaskInvocationsResponse"
    "fixture/DescribeMaintenanceWindowExecutionTaskInvocationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMaintenanceWindowExecutionTaskInvocations)

responseUpdateOpsItem :: UpdateOpsItemResponse -> TestTree
responseUpdateOpsItem =
  res
    "UpdateOpsItemResponse"
    "fixture/UpdateOpsItemResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateOpsItem)

responseDescribeSessions :: DescribeSessionsResponse -> TestTree
responseDescribeSessions =
  res
    "DescribeSessionsResponse"
    "fixture/DescribeSessionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSessions)

responseDescribeInstanceInformation :: DescribeInstanceInformationResponse -> TestTree
responseDescribeInstanceInformation =
  res
    "DescribeInstanceInformationResponse"
    "fixture/DescribeInstanceInformationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstanceInformation)

responseGetMaintenanceWindowExecutionTaskInvocation :: GetMaintenanceWindowExecutionTaskInvocationResponse -> TestTree
responseGetMaintenanceWindowExecutionTaskInvocation =
  res
    "GetMaintenanceWindowExecutionTaskInvocationResponse"
    "fixture/GetMaintenanceWindowExecutionTaskInvocationResponse.proto"
    defaultService
    (Proxy :: Proxy GetMaintenanceWindowExecutionTaskInvocation)

responseStartChangeRequestExecution :: StartChangeRequestExecutionResponse -> TestTree
responseStartChangeRequestExecution =
  res
    "StartChangeRequestExecutionResponse"
    "fixture/StartChangeRequestExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy StartChangeRequestExecution)

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

responseCreateOpsItem :: CreateOpsItemResponse -> TestTree
responseCreateOpsItem =
  res
    "CreateOpsItemResponse"
    "fixture/CreateOpsItemResponse.proto"
    defaultService
    (Proxy :: Proxy CreateOpsItem)

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

responseDescribeDocument :: DescribeDocumentResponse -> TestTree
responseDescribeDocument =
  res
    "DescribeDocumentResponse"
    "fixture/DescribeDocumentResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDocument)

responseListAssociationVersions :: ListAssociationVersionsResponse -> TestTree
responseListAssociationVersions =
  res
    "ListAssociationVersionsResponse"
    "fixture/ListAssociationVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAssociationVersions)

responseListOpsItemRelatedItems :: ListOpsItemRelatedItemsResponse -> TestTree
responseListOpsItemRelatedItems =
  res
    "ListOpsItemRelatedItemsResponse"
    "fixture/ListOpsItemRelatedItemsResponse.proto"
    defaultService
    (Proxy :: Proxy ListOpsItemRelatedItems)

responsePutComplianceItems :: PutComplianceItemsResponse -> TestTree
responsePutComplianceItems =
  res
    "PutComplianceItemsResponse"
    "fixture/PutComplianceItemsResponse.proto"
    defaultService
    (Proxy :: Proxy PutComplianceItems)

responseDeregisterTaskFromMaintenanceWindow :: DeregisterTaskFromMaintenanceWindowResponse -> TestTree
responseDeregisterTaskFromMaintenanceWindow =
  res
    "DeregisterTaskFromMaintenanceWindowResponse"
    "fixture/DeregisterTaskFromMaintenanceWindowResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterTaskFromMaintenanceWindow)

responseGetMaintenanceWindowTask :: GetMaintenanceWindowTaskResponse -> TestTree
responseGetMaintenanceWindowTask =
  res
    "GetMaintenanceWindowTaskResponse"
    "fixture/GetMaintenanceWindowTaskResponse.proto"
    defaultService
    (Proxy :: Proxy GetMaintenanceWindowTask)

responseDeregisterPatchBaselineForPatchGroup :: DeregisterPatchBaselineForPatchGroupResponse -> TestTree
responseDeregisterPatchBaselineForPatchGroup =
  res
    "DeregisterPatchBaselineForPatchGroupResponse"
    "fixture/DeregisterPatchBaselineForPatchGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterPatchBaselineForPatchGroup)

responseDescribeMaintenanceWindows :: DescribeMaintenanceWindowsResponse -> TestTree
responseDescribeMaintenanceWindows =
  res
    "DescribeMaintenanceWindowsResponse"
    "fixture/DescribeMaintenanceWindowsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMaintenanceWindows)

responseGetDeployablePatchSnapshotForInstance :: GetDeployablePatchSnapshotForInstanceResponse -> TestTree
responseGetDeployablePatchSnapshotForInstance =
  res
    "GetDeployablePatchSnapshotForInstanceResponse"
    "fixture/GetDeployablePatchSnapshotForInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy GetDeployablePatchSnapshotForInstance)

responseResumeSession :: ResumeSessionResponse -> TestTree
responseResumeSession =
  res
    "ResumeSessionResponse"
    "fixture/ResumeSessionResponse.proto"
    defaultService
    (Proxy :: Proxy ResumeSession)

responseGetMaintenanceWindow :: GetMaintenanceWindowResponse -> TestTree
responseGetMaintenanceWindow =
  res
    "GetMaintenanceWindowResponse"
    "fixture/GetMaintenanceWindowResponse.proto"
    defaultService
    (Proxy :: Proxy GetMaintenanceWindow)

responseRegisterDefaultPatchBaseline :: RegisterDefaultPatchBaselineResponse -> TestTree
responseRegisterDefaultPatchBaseline =
  res
    "RegisterDefaultPatchBaselineResponse"
    "fixture/RegisterDefaultPatchBaselineResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterDefaultPatchBaseline)

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

responseUpdatePatchBaseline :: UpdatePatchBaselineResponse -> TestTree
responseUpdatePatchBaseline =
  res
    "UpdatePatchBaselineResponse"
    "fixture/UpdatePatchBaselineResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePatchBaseline)

responseDescribeInstancePatches :: DescribeInstancePatchesResponse -> TestTree
responseDescribeInstancePatches =
  res
    "DescribeInstancePatchesResponse"
    "fixture/DescribeInstancePatchesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstancePatches)

responseDescribeAvailablePatches :: DescribeAvailablePatchesResponse -> TestTree
responseDescribeAvailablePatches =
  res
    "DescribeAvailablePatchesResponse"
    "fixture/DescribeAvailablePatchesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAvailablePatches)

responseDescribeInstancePatchStatesForPatchGroup :: DescribeInstancePatchStatesForPatchGroupResponse -> TestTree
responseDescribeInstancePatchStatesForPatchGroup =
  res
    "DescribeInstancePatchStatesForPatchGroupResponse"
    "fixture/DescribeInstancePatchStatesForPatchGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstancePatchStatesForPatchGroup)

responseDeleteResourceDataSync :: DeleteResourceDataSyncResponse -> TestTree
responseDeleteResourceDataSync =
  res
    "DeleteResourceDataSyncResponse"
    "fixture/DeleteResourceDataSyncResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteResourceDataSync)

responseGetParameters :: GetParametersResponse -> TestTree
responseGetParameters =
  res
    "GetParametersResponse"
    "fixture/GetParametersResponse.proto"
    defaultService
    (Proxy :: Proxy GetParameters)

responseGetConnectionStatus :: GetConnectionStatusResponse -> TestTree
responseGetConnectionStatus =
  res
    "GetConnectionStatusResponse"
    "fixture/GetConnectionStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetConnectionStatus)

responseModifyDocumentPermission :: ModifyDocumentPermissionResponse -> TestTree
responseModifyDocumentPermission =
  res
    "ModifyDocumentPermissionResponse"
    "fixture/ModifyDocumentPermissionResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyDocumentPermission)

responseListOpsItemEvents :: ListOpsItemEventsResponse -> TestTree
responseListOpsItemEvents =
  res
    "ListOpsItemEventsResponse"
    "fixture/ListOpsItemEventsResponse.proto"
    defaultService
    (Proxy :: Proxy ListOpsItemEvents)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    defaultService
    (Proxy :: Proxy AddTagsToResource)

responseDescribeDocumentPermission :: DescribeDocumentPermissionResponse -> TestTree
responseDescribeDocumentPermission =
  res
    "DescribeDocumentPermissionResponse"
    "fixture/DescribeDocumentPermissionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDocumentPermission)

responseCreateResourceDataSync :: CreateResourceDataSyncResponse -> TestTree
responseCreateResourceDataSync =
  res
    "CreateResourceDataSyncResponse"
    "fixture/CreateResourceDataSyncResponse.proto"
    defaultService
    (Proxy :: Proxy CreateResourceDataSync)

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

responseCancelCommand :: CancelCommandResponse -> TestTree
responseCancelCommand =
  res
    "CancelCommandResponse"
    "fixture/CancelCommandResponse.proto"
    defaultService
    (Proxy :: Proxy CancelCommand)

responseGetDocument :: GetDocumentResponse -> TestTree
responseGetDocument =
  res
    "GetDocumentResponse"
    "fixture/GetDocumentResponse.proto"
    defaultService
    (Proxy :: Proxy GetDocument)

responseListComplianceItems :: ListComplianceItemsResponse -> TestTree
responseListComplianceItems =
  res
    "ListComplianceItemsResponse"
    "fixture/ListComplianceItemsResponse.proto"
    defaultService
    (Proxy :: Proxy ListComplianceItems)

responseCreateAssociationBatch :: CreateAssociationBatchResponse -> TestTree
responseCreateAssociationBatch =
  res
    "CreateAssociationBatchResponse"
    "fixture/CreateAssociationBatchResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAssociationBatch)

responseDisassociateOpsItemRelatedItem :: DisassociateOpsItemRelatedItemResponse -> TestTree
responseDisassociateOpsItemRelatedItem =
  res
    "DisassociateOpsItemRelatedItemResponse"
    "fixture/DisassociateOpsItemRelatedItemResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateOpsItemRelatedItem)

responseListCommandInvocations :: ListCommandInvocationsResponse -> TestTree
responseListCommandInvocations =
  res
    "ListCommandInvocationsResponse"
    "fixture/ListCommandInvocationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListCommandInvocations)

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

responseDescribeMaintenanceWindowTargets :: DescribeMaintenanceWindowTargetsResponse -> TestTree
responseDescribeMaintenanceWindowTargets =
  res
    "DescribeMaintenanceWindowTargetsResponse"
    "fixture/DescribeMaintenanceWindowTargetsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMaintenanceWindowTargets)

responseDescribeAutomationExecutions :: DescribeAutomationExecutionsResponse -> TestTree
responseDescribeAutomationExecutions =
  res
    "DescribeAutomationExecutionsResponse"
    "fixture/DescribeAutomationExecutionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAutomationExecutions)

responseDescribePatchBaselines :: DescribePatchBaselinesResponse -> TestTree
responseDescribePatchBaselines =
  res
    "DescribePatchBaselinesResponse"
    "fixture/DescribePatchBaselinesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePatchBaselines)

responseDescribePatchGroupState :: DescribePatchGroupStateResponse -> TestTree
responseDescribePatchGroupState =
  res
    "DescribePatchGroupStateResponse"
    "fixture/DescribePatchGroupStateResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePatchGroupState)

responseUpdateDocumentMetadata :: UpdateDocumentMetadataResponse -> TestTree
responseUpdateDocumentMetadata =
  res
    "UpdateDocumentMetadataResponse"
    "fixture/UpdateDocumentMetadataResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDocumentMetadata)

responseGetPatchBaseline :: GetPatchBaselineResponse -> TestTree
responseGetPatchBaseline =
  res
    "GetPatchBaselineResponse"
    "fixture/GetPatchBaselineResponse.proto"
    defaultService
    (Proxy :: Proxy GetPatchBaseline)

responsePutInventory :: PutInventoryResponse -> TestTree
responsePutInventory =
  res
    "PutInventoryResponse"
    "fixture/PutInventoryResponse.proto"
    defaultService
    (Proxy :: Proxy PutInventory)

responseGetAutomationExecution :: GetAutomationExecutionResponse -> TestTree
responseGetAutomationExecution =
  res
    "GetAutomationExecutionResponse"
    "fixture/GetAutomationExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy GetAutomationExecution)

responseUpdateMaintenanceWindow :: UpdateMaintenanceWindowResponse -> TestTree
responseUpdateMaintenanceWindow =
  res
    "UpdateMaintenanceWindowResponse"
    "fixture/UpdateMaintenanceWindowResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMaintenanceWindow)

responseDescribeInventoryDeletions :: DescribeInventoryDeletionsResponse -> TestTree
responseDescribeInventoryDeletions =
  res
    "DescribeInventoryDeletionsResponse"
    "fixture/DescribeInventoryDeletionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInventoryDeletions)

responseDeleteMaintenanceWindow :: DeleteMaintenanceWindowResponse -> TestTree
responseDeleteMaintenanceWindow =
  res
    "DeleteMaintenanceWindowResponse"
    "fixture/DeleteMaintenanceWindowResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMaintenanceWindow)

responseListAssociations :: ListAssociationsResponse -> TestTree
responseListAssociations =
  res
    "ListAssociationsResponse"
    "fixture/ListAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAssociations)

responsePutParameter :: PutParameterResponse -> TestTree
responsePutParameter =
  res
    "PutParameterResponse"
    "fixture/PutParameterResponse.proto"
    defaultService
    (Proxy :: Proxy PutParameter)

responseGetMaintenanceWindowExecutionTask :: GetMaintenanceWindowExecutionTaskResponse -> TestTree
responseGetMaintenanceWindowExecutionTask =
  res
    "GetMaintenanceWindowExecutionTaskResponse"
    "fixture/GetMaintenanceWindowExecutionTaskResponse.proto"
    defaultService
    (Proxy :: Proxy GetMaintenanceWindowExecutionTask)

responseDescribeMaintenanceWindowExecutionTasks :: DescribeMaintenanceWindowExecutionTasksResponse -> TestTree
responseDescribeMaintenanceWindowExecutionTasks =
  res
    "DescribeMaintenanceWindowExecutionTasksResponse"
    "fixture/DescribeMaintenanceWindowExecutionTasksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMaintenanceWindowExecutionTasks)

responseGetDefaultPatchBaseline :: GetDefaultPatchBaselineResponse -> TestTree
responseGetDefaultPatchBaseline =
  res
    "GetDefaultPatchBaselineResponse"
    "fixture/GetDefaultPatchBaselineResponse.proto"
    defaultService
    (Proxy :: Proxy GetDefaultPatchBaseline)

responseDescribeAssociationExecutions :: DescribeAssociationExecutionsResponse -> TestTree
responseDescribeAssociationExecutions =
  res
    "DescribeAssociationExecutionsResponse"
    "fixture/DescribeAssociationExecutionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAssociationExecutions)

responseCancelMaintenanceWindowExecution :: CancelMaintenanceWindowExecutionResponse -> TestTree
responseCancelMaintenanceWindowExecution =
  res
    "CancelMaintenanceWindowExecutionResponse"
    "fixture/CancelMaintenanceWindowExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy CancelMaintenanceWindowExecution)

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

responseCreateMaintenanceWindow :: CreateMaintenanceWindowResponse -> TestTree
responseCreateMaintenanceWindow =
  res
    "CreateMaintenanceWindowResponse"
    "fixture/CreateMaintenanceWindowResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMaintenanceWindow)

responseGetParameterHistory :: GetParameterHistoryResponse -> TestTree
responseGetParameterHistory =
  res
    "GetParameterHistoryResponse"
    "fixture/GetParameterHistoryResponse.proto"
    defaultService
    (Proxy :: Proxy GetParameterHistory)

responseDeleteActivation :: DeleteActivationResponse -> TestTree
responseDeleteActivation =
  res
    "DeleteActivationResponse"
    "fixture/DeleteActivationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteActivation)

responseUpdateMaintenanceWindowTask :: UpdateMaintenanceWindowTaskResponse -> TestTree
responseUpdateMaintenanceWindowTask =
  res
    "UpdateMaintenanceWindowTaskResponse"
    "fixture/UpdateMaintenanceWindowTaskResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMaintenanceWindowTask)

responseStartAutomationExecution :: StartAutomationExecutionResponse -> TestTree
responseStartAutomationExecution =
  res
    "StartAutomationExecutionResponse"
    "fixture/StartAutomationExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy StartAutomationExecution)

responseGetServiceSetting :: GetServiceSettingResponse -> TestTree
responseGetServiceSetting =
  res
    "GetServiceSettingResponse"
    "fixture/GetServiceSettingResponse.proto"
    defaultService
    (Proxy :: Proxy GetServiceSetting)

responseLabelParameterVersion :: LabelParameterVersionResponse -> TestTree
responseLabelParameterVersion =
  res
    "LabelParameterVersionResponse"
    "fixture/LabelParameterVersionResponse.proto"
    defaultService
    (Proxy :: Proxy LabelParameterVersion)

responseGetMaintenanceWindowExecution :: GetMaintenanceWindowExecutionResponse -> TestTree
responseGetMaintenanceWindowExecution =
  res
    "GetMaintenanceWindowExecutionResponse"
    "fixture/GetMaintenanceWindowExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy GetMaintenanceWindowExecution)

responseStartAssociationsOnce :: StartAssociationsOnceResponse -> TestTree
responseStartAssociationsOnce =
  res
    "StartAssociationsOnceResponse"
    "fixture/StartAssociationsOnceResponse.proto"
    defaultService
    (Proxy :: Proxy StartAssociationsOnce)

responseDescribePatchProperties :: DescribePatchPropertiesResponse -> TestTree
responseDescribePatchProperties =
  res
    "DescribePatchPropertiesResponse"
    "fixture/DescribePatchPropertiesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePatchProperties)

responseListInventoryEntries :: ListInventoryEntriesResponse -> TestTree
responseListInventoryEntries =
  res
    "ListInventoryEntriesResponse"
    "fixture/ListInventoryEntriesResponse.proto"
    defaultService
    (Proxy :: Proxy ListInventoryEntries)

responseDescribeMaintenanceWindowTasks :: DescribeMaintenanceWindowTasksResponse -> TestTree
responseDescribeMaintenanceWindowTasks =
  res
    "DescribeMaintenanceWindowTasksResponse"
    "fixture/DescribeMaintenanceWindowTasksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMaintenanceWindowTasks)

responseListDocumentMetadataHistory :: ListDocumentMetadataHistoryResponse -> TestTree
responseListDocumentMetadataHistory =
  res
    "ListDocumentMetadataHistoryResponse"
    "fixture/ListDocumentMetadataHistoryResponse.proto"
    defaultService
    (Proxy :: Proxy ListDocumentMetadataHistory)

responseDescribeActivations :: DescribeActivationsResponse -> TestTree
responseDescribeActivations =
  res
    "DescribeActivationsResponse"
    "fixture/DescribeActivationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeActivations)

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

responseUpdateServiceSetting :: UpdateServiceSettingResponse -> TestTree
responseUpdateServiceSetting =
  res
    "UpdateServiceSettingResponse"
    "fixture/UpdateServiceSettingResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateServiceSetting)

responseDescribeInstanceAssociationsStatus :: DescribeInstanceAssociationsStatusResponse -> TestTree
responseDescribeInstanceAssociationsStatus =
  res
    "DescribeInstanceAssociationsStatusResponse"
    "fixture/DescribeInstanceAssociationsStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstanceAssociationsStatus)
