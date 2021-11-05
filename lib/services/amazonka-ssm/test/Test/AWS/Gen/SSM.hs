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

import Amazonka.SSM
import qualified Data.Proxy as Proxy
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
--         [ requestGetConnectionStatus $
--             newGetConnectionStatus
--
--         , requestDescribeInstancePatches $
--             newDescribeInstancePatches
--
--         , requestGetInventory $
--             newGetInventory
--
--         , requestGetParameters $
--             newGetParameters
--
--         , requestDeletePatchBaseline $
--             newDeletePatchBaseline
--
--         , requestUpdatePatchBaseline $
--             newUpdatePatchBaseline
--
--         , requestListOpsItemEvents $
--             newListOpsItemEvents
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
--         , requestGetOpsItem $
--             newGetOpsItem
--
--         , requestResumeSession $
--             newResumeSession
--
--         , requestGetDeployablePatchSnapshotForInstance $
--             newGetDeployablePatchSnapshotForInstance
--
--         , requestDescribeParameters $
--             newDescribeParameters
--
--         , requestDescribeOpsItems $
--             newDescribeOpsItems
--
--         , requestGetParametersByPath $
--             newGetParametersByPath
--
--         , requestPutComplianceItems $
--             newPutComplianceItems
--
--         , requestListDocumentMetadataHistory $
--             newListDocumentMetadataHistory
--
--         , requestDescribeActivations $
--             newDescribeActivations
--
--         , requestGetMaintenanceWindowTask $
--             newGetMaintenanceWindowTask
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDescribeDocument $
--             newDescribeDocument
--
--         , requestDescribePatchProperties $
--             newDescribePatchProperties
--
--         , requestCreateAssociation $
--             newCreateAssociation
--
--         , requestDeleteActivation $
--             newDeleteActivation
--
--         , requestDescribeMaintenanceWindowExecutions $
--             newDescribeMaintenanceWindowExecutions
--
--         , requestDescribeMaintenanceWindowsForTarget $
--             newDescribeMaintenanceWindowsForTarget
--
--         , requestCreateOpsMetadata $
--             newCreateOpsMetadata
--
--         , requestStartChangeRequestExecution $
--             newStartChangeRequestExecution
--
--         , requestCancelMaintenanceWindowExecution $
--             newCancelMaintenanceWindowExecution
--
--         , requestGetInventorySchema $
--             newGetInventorySchema
--
--         , requestListComplianceSummaries $
--             newListComplianceSummaries
--
--         , requestStartAutomationExecution $
--             newStartAutomationExecution
--
--         , requestCreateOpsItem $
--             newCreateOpsItem
--
--         , requestCreateActivation $
--             newCreateActivation
--
--         , requestDeleteMaintenanceWindow $
--             newDeleteMaintenanceWindow
--
--         , requestUpdateMaintenanceWindow $
--             newUpdateMaintenanceWindow
--
--         , requestDescribeSessions $
--             newDescribeSessions
--
--         , requestDescribeMaintenanceWindowExecutionTasks $
--             newDescribeMaintenanceWindowExecutionTasks
--
--         , requestGetDefaultPatchBaseline $
--             newGetDefaultPatchBaseline
--
--         , requestGetMaintenanceWindowExecutionTask $
--             newGetMaintenanceWindowExecutionTask
--
--         , requestCreateDocument $
--             newCreateDocument
--
--         , requestRemoveTagsFromResource $
--             newRemoveTagsFromResource
--
--         , requestGetCalendarState $
--             newGetCalendarState
--
--         , requestDeleteParameters $
--             newDeleteParameters
--
--         , requestDescribePatchGroupState $
--             newDescribePatchGroupState
--
--         , requestListCommandInvocations $
--             newListCommandInvocations
--
--         , requestDeregisterTargetFromMaintenanceWindow $
--             newDeregisterTargetFromMaintenanceWindow
--
--         , requestDescribeEffectivePatchesForPatchBaseline $
--             newDescribeEffectivePatchesForPatchBaseline
--
--         , requestUnlabelParameterVersion $
--             newUnlabelParameterVersion
--
--         , requestDescribeMaintenanceWindowTargets $
--             newDescribeMaintenanceWindowTargets
--
--         , requestResetServiceSetting $
--             newResetServiceSetting
--
--         , requestRegisterPatchBaselineForPatchGroup $
--             newRegisterPatchBaselineForPatchGroup
--
--         , requestListDocuments $
--             newListDocuments
--
--         , requestDescribeInstancePatchStates $
--             newDescribeInstancePatchStates
--
--         , requestGetOpsSummary $
--             newGetOpsSummary
--
--         , requestGetPatchBaselineForPatchGroup $
--             newGetPatchBaselineForPatchGroup
--
--         , requestUpdateManagedInstanceRole $
--             newUpdateManagedInstanceRole
--
--         , requestListComplianceItems $
--             newListComplianceItems
--
--         , requestGetDocument $
--             newGetDocument
--
--         , requestDescribeMaintenanceWindowSchedule $
--             newDescribeMaintenanceWindowSchedule
--
--         , requestAddTagsToResource $
--             newAddTagsToResource
--
--         , requestCancelCommand $
--             newCancelCommand
--
--         , requestDescribeAutomationStepExecutions $
--             newDescribeAutomationStepExecutions
--
--         , requestGetCommandInvocation $
--             newGetCommandInvocation
--
--         , requestDescribeInstancePatchStatesForPatchGroup $
--             newDescribeInstancePatchStatesForPatchGroup
--
--         , requestDeregisterManagedInstance $
--             newDeregisterManagedInstance
--
--         , requestDescribeAssociation $
--             newDescribeAssociation
--
--         , requestDescribeAssociationExecutionTargets $
--             newDescribeAssociationExecutionTargets
--
--         , requestModifyDocumentPermission $
--             newModifyDocumentPermission
--
--         , requestUpdateResourceDataSync $
--             newUpdateResourceDataSync
--
--         , requestDeleteResourceDataSync $
--             newDeleteResourceDataSync
--
--         , requestUpdateAssociationStatus $
--             newUpdateAssociationStatus
--
--         , requestDescribeAvailablePatches $
--             newDescribeAvailablePatches
--
--         , requestListDocumentVersions $
--             newListDocumentVersions
--
--         , requestDeregisterPatchBaselineForPatchGroup $
--             newDeregisterPatchBaselineForPatchGroup
--
--         , requestDescribePatchGroups $
--             newDescribePatchGroups
--
--         , requestGetMaintenanceWindow $
--             newGetMaintenanceWindow
--
--         , requestDescribeMaintenanceWindows $
--             newDescribeMaintenanceWindows
--
--         , requestRegisterTaskWithMaintenanceWindow $
--             newRegisterTaskWithMaintenanceWindow
--
--         , requestRegisterDefaultPatchBaseline $
--             newRegisterDefaultPatchBaseline
--
--         , requestListResourceComplianceSummaries $
--             newListResourceComplianceSummaries
--
--         , requestAssociateOpsItemRelatedItem $
--             newAssociateOpsItemRelatedItem
--
--         , requestListAssociationVersions $
--             newListAssociationVersions
--
--         , requestUpdateServiceSetting $
--             newUpdateServiceSetting
--
--         , requestDescribeMaintenanceWindowTasks $
--             newDescribeMaintenanceWindowTasks
--
--         , requestDescribeInstanceAssociationsStatus $
--             newDescribeInstanceAssociationsStatus
--
--         , requestListOpsItemRelatedItems $
--             newListOpsItemRelatedItems
--
--         , requestDeregisterTaskFromMaintenanceWindow $
--             newDeregisterTaskFromMaintenanceWindow
--
--         , requestListInventoryEntries $
--             newListInventoryEntries
--
--         , requestLabelParameterVersion $
--             newLabelParameterVersion
--
--         , requestUpdateMaintenanceWindowTask $
--             newUpdateMaintenanceWindowTask
--
--         , requestGetParameterHistory $
--             newGetParameterHistory
--
--         , requestDescribeAssociationExecutions $
--             newDescribeAssociationExecutions
--
--         , requestGetServiceSetting $
--             newGetServiceSetting
--
--         , requestStartAssociationsOnce $
--             newStartAssociationsOnce
--
--         , requestCreateMaintenanceWindow $
--             newCreateMaintenanceWindow
--
--         , requestStopAutomationExecution $
--             newStopAutomationExecution
--
--         , requestGetMaintenanceWindowExecution $
--             newGetMaintenanceWindowExecution
--
--         , requestSendAutomationSignal $
--             newSendAutomationSignal
--
--         , requestDeleteOpsMetadata $
--             newDeleteOpsMetadata
--
--         , requestUpdateOpsMetadata $
--             newUpdateOpsMetadata
--
--         , requestPutParameter $
--             newPutParameter
--
--         , requestDescribeMaintenanceWindowExecutionTaskInvocations $
--             newDescribeMaintenanceWindowExecutionTaskInvocations
--
--         , requestGetMaintenanceWindowExecutionTaskInvocation $
--             newGetMaintenanceWindowExecutionTaskInvocation
--
--         , requestDeleteParameter $
--             newDeleteParameter
--
--         , requestDescribeInstanceInformation $
--             newDescribeInstanceInformation
--
--         , requestListAssociations $
--             newListAssociations
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
--         , requestDescribeInventoryDeletions $
--             newDescribeInventoryDeletions
--
--         , requestDeleteInventory $
--             newDeleteInventory
--
--         , requestPutInventory $
--             newPutInventory
--
--         , requestUpdateDocumentMetadata $
--             newUpdateDocumentMetadata
--
--         , requestListOpsMetadata $
--             newListOpsMetadata
--
--         , requestDescribeEffectiveInstanceAssociations $
--             newDescribeEffectiveInstanceAssociations
--
--         , requestDescribeAutomationExecutions $
--             newDescribeAutomationExecutions
--
--         , requestGetAutomationExecution $
--             newGetAutomationExecution
--
--         , requestSendCommand $
--             newSendCommand
--
--         , requestDescribePatchBaselines $
--             newDescribePatchBaselines
--
--         , requestGetPatchBaseline $
--             newGetPatchBaseline
--
--         , requestRegisterTargetWithMaintenanceWindow $
--             newRegisterTargetWithMaintenanceWindow
--
--         , requestStartSession $
--             newStartSession
--
--         , requestListCommands $
--             newListCommands
--
--         , requestUpdateDocument $
--             newUpdateDocument
--
--         , requestDeleteDocument $
--             newDeleteDocument
--
--         , requestDescribeDocumentPermission $
--             newDescribeDocumentPermission
--
--         , requestCreateAssociationBatch $
--             newCreateAssociationBatch
--
--         , requestUpdateMaintenanceWindowTarget $
--             newUpdateMaintenanceWindowTarget
--
--         , requestCreateResourceDataSync $
--             newCreateResourceDataSync
--
--         , requestCreatePatchBaseline $
--             newCreatePatchBaseline
--
--         , requestDisassociateOpsItemRelatedItem $
--             newDisassociateOpsItemRelatedItem
--
--           ]

--     , testGroup "response"
--         [ responseGetConnectionStatus $
--             newGetConnectionStatusResponse
--
--         , responseDescribeInstancePatches $
--             newDescribeInstancePatchesResponse
--
--         , responseGetInventory $
--             newGetInventoryResponse
--
--         , responseGetParameters $
--             newGetParametersResponse
--
--         , responseDeletePatchBaseline $
--             newDeletePatchBaselineResponse
--
--         , responseUpdatePatchBaseline $
--             newUpdatePatchBaselineResponse
--
--         , responseListOpsItemEvents $
--             newListOpsItemEventsResponse
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
--         , responseGetOpsItem $
--             newGetOpsItemResponse
--
--         , responseResumeSession $
--             newResumeSessionResponse
--
--         , responseGetDeployablePatchSnapshotForInstance $
--             newGetDeployablePatchSnapshotForInstanceResponse
--
--         , responseDescribeParameters $
--             newDescribeParametersResponse
--
--         , responseDescribeOpsItems $
--             newDescribeOpsItemsResponse
--
--         , responseGetParametersByPath $
--             newGetParametersByPathResponse
--
--         , responsePutComplianceItems $
--             newPutComplianceItemsResponse
--
--         , responseListDocumentMetadataHistory $
--             newListDocumentMetadataHistoryResponse
--
--         , responseDescribeActivations $
--             newDescribeActivationsResponse
--
--         , responseGetMaintenanceWindowTask $
--             newGetMaintenanceWindowTaskResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDescribeDocument $
--             newDescribeDocumentResponse
--
--         , responseDescribePatchProperties $
--             newDescribePatchPropertiesResponse
--
--         , responseCreateAssociation $
--             newCreateAssociationResponse
--
--         , responseDeleteActivation $
--             newDeleteActivationResponse
--
--         , responseDescribeMaintenanceWindowExecutions $
--             newDescribeMaintenanceWindowExecutionsResponse
--
--         , responseDescribeMaintenanceWindowsForTarget $
--             newDescribeMaintenanceWindowsForTargetResponse
--
--         , responseCreateOpsMetadata $
--             newCreateOpsMetadataResponse
--
--         , responseStartChangeRequestExecution $
--             newStartChangeRequestExecutionResponse
--
--         , responseCancelMaintenanceWindowExecution $
--             newCancelMaintenanceWindowExecutionResponse
--
--         , responseGetInventorySchema $
--             newGetInventorySchemaResponse
--
--         , responseListComplianceSummaries $
--             newListComplianceSummariesResponse
--
--         , responseStartAutomationExecution $
--             newStartAutomationExecutionResponse
--
--         , responseCreateOpsItem $
--             newCreateOpsItemResponse
--
--         , responseCreateActivation $
--             newCreateActivationResponse
--
--         , responseDeleteMaintenanceWindow $
--             newDeleteMaintenanceWindowResponse
--
--         , responseUpdateMaintenanceWindow $
--             newUpdateMaintenanceWindowResponse
--
--         , responseDescribeSessions $
--             newDescribeSessionsResponse
--
--         , responseDescribeMaintenanceWindowExecutionTasks $
--             newDescribeMaintenanceWindowExecutionTasksResponse
--
--         , responseGetDefaultPatchBaseline $
--             newGetDefaultPatchBaselineResponse
--
--         , responseGetMaintenanceWindowExecutionTask $
--             newGetMaintenanceWindowExecutionTaskResponse
--
--         , responseCreateDocument $
--             newCreateDocumentResponse
--
--         , responseRemoveTagsFromResource $
--             newRemoveTagsFromResourceResponse
--
--         , responseGetCalendarState $
--             newGetCalendarStateResponse
--
--         , responseDeleteParameters $
--             newDeleteParametersResponse
--
--         , responseDescribePatchGroupState $
--             newDescribePatchGroupStateResponse
--
--         , responseListCommandInvocations $
--             newListCommandInvocationsResponse
--
--         , responseDeregisterTargetFromMaintenanceWindow $
--             newDeregisterTargetFromMaintenanceWindowResponse
--
--         , responseDescribeEffectivePatchesForPatchBaseline $
--             newDescribeEffectivePatchesForPatchBaselineResponse
--
--         , responseUnlabelParameterVersion $
--             newUnlabelParameterVersionResponse
--
--         , responseDescribeMaintenanceWindowTargets $
--             newDescribeMaintenanceWindowTargetsResponse
--
--         , responseResetServiceSetting $
--             newResetServiceSettingResponse
--
--         , responseRegisterPatchBaselineForPatchGroup $
--             newRegisterPatchBaselineForPatchGroupResponse
--
--         , responseListDocuments $
--             newListDocumentsResponse
--
--         , responseDescribeInstancePatchStates $
--             newDescribeInstancePatchStatesResponse
--
--         , responseGetOpsSummary $
--             newGetOpsSummaryResponse
--
--         , responseGetPatchBaselineForPatchGroup $
--             newGetPatchBaselineForPatchGroupResponse
--
--         , responseUpdateManagedInstanceRole $
--             newUpdateManagedInstanceRoleResponse
--
--         , responseListComplianceItems $
--             newListComplianceItemsResponse
--
--         , responseGetDocument $
--             newGetDocumentResponse
--
--         , responseDescribeMaintenanceWindowSchedule $
--             newDescribeMaintenanceWindowScheduleResponse
--
--         , responseAddTagsToResource $
--             newAddTagsToResourceResponse
--
--         , responseCancelCommand $
--             newCancelCommandResponse
--
--         , responseDescribeAutomationStepExecutions $
--             newDescribeAutomationStepExecutionsResponse
--
--         , responseGetCommandInvocation $
--             newGetCommandInvocationResponse
--
--         , responseDescribeInstancePatchStatesForPatchGroup $
--             newDescribeInstancePatchStatesForPatchGroupResponse
--
--         , responseDeregisterManagedInstance $
--             newDeregisterManagedInstanceResponse
--
--         , responseDescribeAssociation $
--             newDescribeAssociationResponse
--
--         , responseDescribeAssociationExecutionTargets $
--             newDescribeAssociationExecutionTargetsResponse
--
--         , responseModifyDocumentPermission $
--             newModifyDocumentPermissionResponse
--
--         , responseUpdateResourceDataSync $
--             newUpdateResourceDataSyncResponse
--
--         , responseDeleteResourceDataSync $
--             newDeleteResourceDataSyncResponse
--
--         , responseUpdateAssociationStatus $
--             newUpdateAssociationStatusResponse
--
--         , responseDescribeAvailablePatches $
--             newDescribeAvailablePatchesResponse
--
--         , responseListDocumentVersions $
--             newListDocumentVersionsResponse
--
--         , responseDeregisterPatchBaselineForPatchGroup $
--             newDeregisterPatchBaselineForPatchGroupResponse
--
--         , responseDescribePatchGroups $
--             newDescribePatchGroupsResponse
--
--         , responseGetMaintenanceWindow $
--             newGetMaintenanceWindowResponse
--
--         , responseDescribeMaintenanceWindows $
--             newDescribeMaintenanceWindowsResponse
--
--         , responseRegisterTaskWithMaintenanceWindow $
--             newRegisterTaskWithMaintenanceWindowResponse
--
--         , responseRegisterDefaultPatchBaseline $
--             newRegisterDefaultPatchBaselineResponse
--
--         , responseListResourceComplianceSummaries $
--             newListResourceComplianceSummariesResponse
--
--         , responseAssociateOpsItemRelatedItem $
--             newAssociateOpsItemRelatedItemResponse
--
--         , responseListAssociationVersions $
--             newListAssociationVersionsResponse
--
--         , responseUpdateServiceSetting $
--             newUpdateServiceSettingResponse
--
--         , responseDescribeMaintenanceWindowTasks $
--             newDescribeMaintenanceWindowTasksResponse
--
--         , responseDescribeInstanceAssociationsStatus $
--             newDescribeInstanceAssociationsStatusResponse
--
--         , responseListOpsItemRelatedItems $
--             newListOpsItemRelatedItemsResponse
--
--         , responseDeregisterTaskFromMaintenanceWindow $
--             newDeregisterTaskFromMaintenanceWindowResponse
--
--         , responseListInventoryEntries $
--             newListInventoryEntriesResponse
--
--         , responseLabelParameterVersion $
--             newLabelParameterVersionResponse
--
--         , responseUpdateMaintenanceWindowTask $
--             newUpdateMaintenanceWindowTaskResponse
--
--         , responseGetParameterHistory $
--             newGetParameterHistoryResponse
--
--         , responseDescribeAssociationExecutions $
--             newDescribeAssociationExecutionsResponse
--
--         , responseGetServiceSetting $
--             newGetServiceSettingResponse
--
--         , responseStartAssociationsOnce $
--             newStartAssociationsOnceResponse
--
--         , responseCreateMaintenanceWindow $
--             newCreateMaintenanceWindowResponse
--
--         , responseStopAutomationExecution $
--             newStopAutomationExecutionResponse
--
--         , responseGetMaintenanceWindowExecution $
--             newGetMaintenanceWindowExecutionResponse
--
--         , responseSendAutomationSignal $
--             newSendAutomationSignalResponse
--
--         , responseDeleteOpsMetadata $
--             newDeleteOpsMetadataResponse
--
--         , responseUpdateOpsMetadata $
--             newUpdateOpsMetadataResponse
--
--         , responsePutParameter $
--             newPutParameterResponse
--
--         , responseDescribeMaintenanceWindowExecutionTaskInvocations $
--             newDescribeMaintenanceWindowExecutionTaskInvocationsResponse
--
--         , responseGetMaintenanceWindowExecutionTaskInvocation $
--             newGetMaintenanceWindowExecutionTaskInvocationResponse
--
--         , responseDeleteParameter $
--             newDeleteParameterResponse
--
--         , responseDescribeInstanceInformation $
--             newDescribeInstanceInformationResponse
--
--         , responseListAssociations $
--             newListAssociationsResponse
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
--         , responseDescribeInventoryDeletions $
--             newDescribeInventoryDeletionsResponse
--
--         , responseDeleteInventory $
--             newDeleteInventoryResponse
--
--         , responsePutInventory $
--             newPutInventoryResponse
--
--         , responseUpdateDocumentMetadata $
--             newUpdateDocumentMetadataResponse
--
--         , responseListOpsMetadata $
--             newListOpsMetadataResponse
--
--         , responseDescribeEffectiveInstanceAssociations $
--             newDescribeEffectiveInstanceAssociationsResponse
--
--         , responseDescribeAutomationExecutions $
--             newDescribeAutomationExecutionsResponse
--
--         , responseGetAutomationExecution $
--             newGetAutomationExecutionResponse
--
--         , responseSendCommand $
--             newSendCommandResponse
--
--         , responseDescribePatchBaselines $
--             newDescribePatchBaselinesResponse
--
--         , responseGetPatchBaseline $
--             newGetPatchBaselineResponse
--
--         , responseRegisterTargetWithMaintenanceWindow $
--             newRegisterTargetWithMaintenanceWindowResponse
--
--         , responseStartSession $
--             newStartSessionResponse
--
--         , responseListCommands $
--             newListCommandsResponse
--
--         , responseUpdateDocument $
--             newUpdateDocumentResponse
--
--         , responseDeleteDocument $
--             newDeleteDocumentResponse
--
--         , responseDescribeDocumentPermission $
--             newDescribeDocumentPermissionResponse
--
--         , responseCreateAssociationBatch $
--             newCreateAssociationBatchResponse
--
--         , responseUpdateMaintenanceWindowTarget $
--             newUpdateMaintenanceWindowTargetResponse
--
--         , responseCreateResourceDataSync $
--             newCreateResourceDataSyncResponse
--
--         , responseCreatePatchBaseline $
--             newCreatePatchBaselineResponse
--
--         , responseDisassociateOpsItemRelatedItem $
--             newDisassociateOpsItemRelatedItemResponse
--
--           ]
--     ]

-- Requests

requestGetConnectionStatus :: GetConnectionStatus -> TestTree
requestGetConnectionStatus =
  req
    "GetConnectionStatus"
    "fixture/GetConnectionStatus.yaml"

requestDescribeInstancePatches :: DescribeInstancePatches -> TestTree
requestDescribeInstancePatches =
  req
    "DescribeInstancePatches"
    "fixture/DescribeInstancePatches.yaml"

requestGetInventory :: GetInventory -> TestTree
requestGetInventory =
  req
    "GetInventory"
    "fixture/GetInventory.yaml"

requestGetParameters :: GetParameters -> TestTree
requestGetParameters =
  req
    "GetParameters"
    "fixture/GetParameters.yaml"

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

requestListOpsItemEvents :: ListOpsItemEvents -> TestTree
requestListOpsItemEvents =
  req
    "ListOpsItemEvents"
    "fixture/ListOpsItemEvents.yaml"

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

requestGetOpsItem :: GetOpsItem -> TestTree
requestGetOpsItem =
  req
    "GetOpsItem"
    "fixture/GetOpsItem.yaml"

requestResumeSession :: ResumeSession -> TestTree
requestResumeSession =
  req
    "ResumeSession"
    "fixture/ResumeSession.yaml"

requestGetDeployablePatchSnapshotForInstance :: GetDeployablePatchSnapshotForInstance -> TestTree
requestGetDeployablePatchSnapshotForInstance =
  req
    "GetDeployablePatchSnapshotForInstance"
    "fixture/GetDeployablePatchSnapshotForInstance.yaml"

requestDescribeParameters :: DescribeParameters -> TestTree
requestDescribeParameters =
  req
    "DescribeParameters"
    "fixture/DescribeParameters.yaml"

requestDescribeOpsItems :: DescribeOpsItems -> TestTree
requestDescribeOpsItems =
  req
    "DescribeOpsItems"
    "fixture/DescribeOpsItems.yaml"

requestGetParametersByPath :: GetParametersByPath -> TestTree
requestGetParametersByPath =
  req
    "GetParametersByPath"
    "fixture/GetParametersByPath.yaml"

requestPutComplianceItems :: PutComplianceItems -> TestTree
requestPutComplianceItems =
  req
    "PutComplianceItems"
    "fixture/PutComplianceItems.yaml"

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

requestGetMaintenanceWindowTask :: GetMaintenanceWindowTask -> TestTree
requestGetMaintenanceWindowTask =
  req
    "GetMaintenanceWindowTask"
    "fixture/GetMaintenanceWindowTask.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDescribeDocument :: DescribeDocument -> TestTree
requestDescribeDocument =
  req
    "DescribeDocument"
    "fixture/DescribeDocument.yaml"

requestDescribePatchProperties :: DescribePatchProperties -> TestTree
requestDescribePatchProperties =
  req
    "DescribePatchProperties"
    "fixture/DescribePatchProperties.yaml"

requestCreateAssociation :: CreateAssociation -> TestTree
requestCreateAssociation =
  req
    "CreateAssociation"
    "fixture/CreateAssociation.yaml"

requestDeleteActivation :: DeleteActivation -> TestTree
requestDeleteActivation =
  req
    "DeleteActivation"
    "fixture/DeleteActivation.yaml"

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

requestCreateOpsMetadata :: CreateOpsMetadata -> TestTree
requestCreateOpsMetadata =
  req
    "CreateOpsMetadata"
    "fixture/CreateOpsMetadata.yaml"

requestStartChangeRequestExecution :: StartChangeRequestExecution -> TestTree
requestStartChangeRequestExecution =
  req
    "StartChangeRequestExecution"
    "fixture/StartChangeRequestExecution.yaml"

requestCancelMaintenanceWindowExecution :: CancelMaintenanceWindowExecution -> TestTree
requestCancelMaintenanceWindowExecution =
  req
    "CancelMaintenanceWindowExecution"
    "fixture/CancelMaintenanceWindowExecution.yaml"

requestGetInventorySchema :: GetInventorySchema -> TestTree
requestGetInventorySchema =
  req
    "GetInventorySchema"
    "fixture/GetInventorySchema.yaml"

requestListComplianceSummaries :: ListComplianceSummaries -> TestTree
requestListComplianceSummaries =
  req
    "ListComplianceSummaries"
    "fixture/ListComplianceSummaries.yaml"

requestStartAutomationExecution :: StartAutomationExecution -> TestTree
requestStartAutomationExecution =
  req
    "StartAutomationExecution"
    "fixture/StartAutomationExecution.yaml"

requestCreateOpsItem :: CreateOpsItem -> TestTree
requestCreateOpsItem =
  req
    "CreateOpsItem"
    "fixture/CreateOpsItem.yaml"

requestCreateActivation :: CreateActivation -> TestTree
requestCreateActivation =
  req
    "CreateActivation"
    "fixture/CreateActivation.yaml"

requestDeleteMaintenanceWindow :: DeleteMaintenanceWindow -> TestTree
requestDeleteMaintenanceWindow =
  req
    "DeleteMaintenanceWindow"
    "fixture/DeleteMaintenanceWindow.yaml"

requestUpdateMaintenanceWindow :: UpdateMaintenanceWindow -> TestTree
requestUpdateMaintenanceWindow =
  req
    "UpdateMaintenanceWindow"
    "fixture/UpdateMaintenanceWindow.yaml"

requestDescribeSessions :: DescribeSessions -> TestTree
requestDescribeSessions =
  req
    "DescribeSessions"
    "fixture/DescribeSessions.yaml"

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

requestGetMaintenanceWindowExecutionTask :: GetMaintenanceWindowExecutionTask -> TestTree
requestGetMaintenanceWindowExecutionTask =
  req
    "GetMaintenanceWindowExecutionTask"
    "fixture/GetMaintenanceWindowExecutionTask.yaml"

requestCreateDocument :: CreateDocument -> TestTree
requestCreateDocument =
  req
    "CreateDocument"
    "fixture/CreateDocument.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource =
  req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestGetCalendarState :: GetCalendarState -> TestTree
requestGetCalendarState =
  req
    "GetCalendarState"
    "fixture/GetCalendarState.yaml"

requestDeleteParameters :: DeleteParameters -> TestTree
requestDeleteParameters =
  req
    "DeleteParameters"
    "fixture/DeleteParameters.yaml"

requestDescribePatchGroupState :: DescribePatchGroupState -> TestTree
requestDescribePatchGroupState =
  req
    "DescribePatchGroupState"
    "fixture/DescribePatchGroupState.yaml"

requestListCommandInvocations :: ListCommandInvocations -> TestTree
requestListCommandInvocations =
  req
    "ListCommandInvocations"
    "fixture/ListCommandInvocations.yaml"

requestDeregisterTargetFromMaintenanceWindow :: DeregisterTargetFromMaintenanceWindow -> TestTree
requestDeregisterTargetFromMaintenanceWindow =
  req
    "DeregisterTargetFromMaintenanceWindow"
    "fixture/DeregisterTargetFromMaintenanceWindow.yaml"

requestDescribeEffectivePatchesForPatchBaseline :: DescribeEffectivePatchesForPatchBaseline -> TestTree
requestDescribeEffectivePatchesForPatchBaseline =
  req
    "DescribeEffectivePatchesForPatchBaseline"
    "fixture/DescribeEffectivePatchesForPatchBaseline.yaml"

requestUnlabelParameterVersion :: UnlabelParameterVersion -> TestTree
requestUnlabelParameterVersion =
  req
    "UnlabelParameterVersion"
    "fixture/UnlabelParameterVersion.yaml"

requestDescribeMaintenanceWindowTargets :: DescribeMaintenanceWindowTargets -> TestTree
requestDescribeMaintenanceWindowTargets =
  req
    "DescribeMaintenanceWindowTargets"
    "fixture/DescribeMaintenanceWindowTargets.yaml"

requestResetServiceSetting :: ResetServiceSetting -> TestTree
requestResetServiceSetting =
  req
    "ResetServiceSetting"
    "fixture/ResetServiceSetting.yaml"

requestRegisterPatchBaselineForPatchGroup :: RegisterPatchBaselineForPatchGroup -> TestTree
requestRegisterPatchBaselineForPatchGroup =
  req
    "RegisterPatchBaselineForPatchGroup"
    "fixture/RegisterPatchBaselineForPatchGroup.yaml"

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

requestGetOpsSummary :: GetOpsSummary -> TestTree
requestGetOpsSummary =
  req
    "GetOpsSummary"
    "fixture/GetOpsSummary.yaml"

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

requestDescribeMaintenanceWindowSchedule :: DescribeMaintenanceWindowSchedule -> TestTree
requestDescribeMaintenanceWindowSchedule =
  req
    "DescribeMaintenanceWindowSchedule"
    "fixture/DescribeMaintenanceWindowSchedule.yaml"

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource =
  req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestCancelCommand :: CancelCommand -> TestTree
requestCancelCommand =
  req
    "CancelCommand"
    "fixture/CancelCommand.yaml"

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

requestDescribeInstancePatchStatesForPatchGroup :: DescribeInstancePatchStatesForPatchGroup -> TestTree
requestDescribeInstancePatchStatesForPatchGroup =
  req
    "DescribeInstancePatchStatesForPatchGroup"
    "fixture/DescribeInstancePatchStatesForPatchGroup.yaml"

requestDeregisterManagedInstance :: DeregisterManagedInstance -> TestTree
requestDeregisterManagedInstance =
  req
    "DeregisterManagedInstance"
    "fixture/DeregisterManagedInstance.yaml"

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

requestModifyDocumentPermission :: ModifyDocumentPermission -> TestTree
requestModifyDocumentPermission =
  req
    "ModifyDocumentPermission"
    "fixture/ModifyDocumentPermission.yaml"

requestUpdateResourceDataSync :: UpdateResourceDataSync -> TestTree
requestUpdateResourceDataSync =
  req
    "UpdateResourceDataSync"
    "fixture/UpdateResourceDataSync.yaml"

requestDeleteResourceDataSync :: DeleteResourceDataSync -> TestTree
requestDeleteResourceDataSync =
  req
    "DeleteResourceDataSync"
    "fixture/DeleteResourceDataSync.yaml"

requestUpdateAssociationStatus :: UpdateAssociationStatus -> TestTree
requestUpdateAssociationStatus =
  req
    "UpdateAssociationStatus"
    "fixture/UpdateAssociationStatus.yaml"

requestDescribeAvailablePatches :: DescribeAvailablePatches -> TestTree
requestDescribeAvailablePatches =
  req
    "DescribeAvailablePatches"
    "fixture/DescribeAvailablePatches.yaml"

requestListDocumentVersions :: ListDocumentVersions -> TestTree
requestListDocumentVersions =
  req
    "ListDocumentVersions"
    "fixture/ListDocumentVersions.yaml"

requestDeregisterPatchBaselineForPatchGroup :: DeregisterPatchBaselineForPatchGroup -> TestTree
requestDeregisterPatchBaselineForPatchGroup =
  req
    "DeregisterPatchBaselineForPatchGroup"
    "fixture/DeregisterPatchBaselineForPatchGroup.yaml"

requestDescribePatchGroups :: DescribePatchGroups -> TestTree
requestDescribePatchGroups =
  req
    "DescribePatchGroups"
    "fixture/DescribePatchGroups.yaml"

requestGetMaintenanceWindow :: GetMaintenanceWindow -> TestTree
requestGetMaintenanceWindow =
  req
    "GetMaintenanceWindow"
    "fixture/GetMaintenanceWindow.yaml"

requestDescribeMaintenanceWindows :: DescribeMaintenanceWindows -> TestTree
requestDescribeMaintenanceWindows =
  req
    "DescribeMaintenanceWindows"
    "fixture/DescribeMaintenanceWindows.yaml"

requestRegisterTaskWithMaintenanceWindow :: RegisterTaskWithMaintenanceWindow -> TestTree
requestRegisterTaskWithMaintenanceWindow =
  req
    "RegisterTaskWithMaintenanceWindow"
    "fixture/RegisterTaskWithMaintenanceWindow.yaml"

requestRegisterDefaultPatchBaseline :: RegisterDefaultPatchBaseline -> TestTree
requestRegisterDefaultPatchBaseline =
  req
    "RegisterDefaultPatchBaseline"
    "fixture/RegisterDefaultPatchBaseline.yaml"

requestListResourceComplianceSummaries :: ListResourceComplianceSummaries -> TestTree
requestListResourceComplianceSummaries =
  req
    "ListResourceComplianceSummaries"
    "fixture/ListResourceComplianceSummaries.yaml"

requestAssociateOpsItemRelatedItem :: AssociateOpsItemRelatedItem -> TestTree
requestAssociateOpsItemRelatedItem =
  req
    "AssociateOpsItemRelatedItem"
    "fixture/AssociateOpsItemRelatedItem.yaml"

requestListAssociationVersions :: ListAssociationVersions -> TestTree
requestListAssociationVersions =
  req
    "ListAssociationVersions"
    "fixture/ListAssociationVersions.yaml"

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

requestDescribeInstanceAssociationsStatus :: DescribeInstanceAssociationsStatus -> TestTree
requestDescribeInstanceAssociationsStatus =
  req
    "DescribeInstanceAssociationsStatus"
    "fixture/DescribeInstanceAssociationsStatus.yaml"

requestListOpsItemRelatedItems :: ListOpsItemRelatedItems -> TestTree
requestListOpsItemRelatedItems =
  req
    "ListOpsItemRelatedItems"
    "fixture/ListOpsItemRelatedItems.yaml"

requestDeregisterTaskFromMaintenanceWindow :: DeregisterTaskFromMaintenanceWindow -> TestTree
requestDeregisterTaskFromMaintenanceWindow =
  req
    "DeregisterTaskFromMaintenanceWindow"
    "fixture/DeregisterTaskFromMaintenanceWindow.yaml"

requestListInventoryEntries :: ListInventoryEntries -> TestTree
requestListInventoryEntries =
  req
    "ListInventoryEntries"
    "fixture/ListInventoryEntries.yaml"

requestLabelParameterVersion :: LabelParameterVersion -> TestTree
requestLabelParameterVersion =
  req
    "LabelParameterVersion"
    "fixture/LabelParameterVersion.yaml"

requestUpdateMaintenanceWindowTask :: UpdateMaintenanceWindowTask -> TestTree
requestUpdateMaintenanceWindowTask =
  req
    "UpdateMaintenanceWindowTask"
    "fixture/UpdateMaintenanceWindowTask.yaml"

requestGetParameterHistory :: GetParameterHistory -> TestTree
requestGetParameterHistory =
  req
    "GetParameterHistory"
    "fixture/GetParameterHistory.yaml"

requestDescribeAssociationExecutions :: DescribeAssociationExecutions -> TestTree
requestDescribeAssociationExecutions =
  req
    "DescribeAssociationExecutions"
    "fixture/DescribeAssociationExecutions.yaml"

requestGetServiceSetting :: GetServiceSetting -> TestTree
requestGetServiceSetting =
  req
    "GetServiceSetting"
    "fixture/GetServiceSetting.yaml"

requestStartAssociationsOnce :: StartAssociationsOnce -> TestTree
requestStartAssociationsOnce =
  req
    "StartAssociationsOnce"
    "fixture/StartAssociationsOnce.yaml"

requestCreateMaintenanceWindow :: CreateMaintenanceWindow -> TestTree
requestCreateMaintenanceWindow =
  req
    "CreateMaintenanceWindow"
    "fixture/CreateMaintenanceWindow.yaml"

requestStopAutomationExecution :: StopAutomationExecution -> TestTree
requestStopAutomationExecution =
  req
    "StopAutomationExecution"
    "fixture/StopAutomationExecution.yaml"

requestGetMaintenanceWindowExecution :: GetMaintenanceWindowExecution -> TestTree
requestGetMaintenanceWindowExecution =
  req
    "GetMaintenanceWindowExecution"
    "fixture/GetMaintenanceWindowExecution.yaml"

requestSendAutomationSignal :: SendAutomationSignal -> TestTree
requestSendAutomationSignal =
  req
    "SendAutomationSignal"
    "fixture/SendAutomationSignal.yaml"

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

requestPutParameter :: PutParameter -> TestTree
requestPutParameter =
  req
    "PutParameter"
    "fixture/PutParameter.yaml"

requestDescribeMaintenanceWindowExecutionTaskInvocations :: DescribeMaintenanceWindowExecutionTaskInvocations -> TestTree
requestDescribeMaintenanceWindowExecutionTaskInvocations =
  req
    "DescribeMaintenanceWindowExecutionTaskInvocations"
    "fixture/DescribeMaintenanceWindowExecutionTaskInvocations.yaml"

requestGetMaintenanceWindowExecutionTaskInvocation :: GetMaintenanceWindowExecutionTaskInvocation -> TestTree
requestGetMaintenanceWindowExecutionTaskInvocation =
  req
    "GetMaintenanceWindowExecutionTaskInvocation"
    "fixture/GetMaintenanceWindowExecutionTaskInvocation.yaml"

requestDeleteParameter :: DeleteParameter -> TestTree
requestDeleteParameter =
  req
    "DeleteParameter"
    "fixture/DeleteParameter.yaml"

requestDescribeInstanceInformation :: DescribeInstanceInformation -> TestTree
requestDescribeInstanceInformation =
  req
    "DescribeInstanceInformation"
    "fixture/DescribeInstanceInformation.yaml"

requestListAssociations :: ListAssociations -> TestTree
requestListAssociations =
  req
    "ListAssociations"
    "fixture/ListAssociations.yaml"

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

requestDescribeInventoryDeletions :: DescribeInventoryDeletions -> TestTree
requestDescribeInventoryDeletions =
  req
    "DescribeInventoryDeletions"
    "fixture/DescribeInventoryDeletions.yaml"

requestDeleteInventory :: DeleteInventory -> TestTree
requestDeleteInventory =
  req
    "DeleteInventory"
    "fixture/DeleteInventory.yaml"

requestPutInventory :: PutInventory -> TestTree
requestPutInventory =
  req
    "PutInventory"
    "fixture/PutInventory.yaml"

requestUpdateDocumentMetadata :: UpdateDocumentMetadata -> TestTree
requestUpdateDocumentMetadata =
  req
    "UpdateDocumentMetadata"
    "fixture/UpdateDocumentMetadata.yaml"

requestListOpsMetadata :: ListOpsMetadata -> TestTree
requestListOpsMetadata =
  req
    "ListOpsMetadata"
    "fixture/ListOpsMetadata.yaml"

requestDescribeEffectiveInstanceAssociations :: DescribeEffectiveInstanceAssociations -> TestTree
requestDescribeEffectiveInstanceAssociations =
  req
    "DescribeEffectiveInstanceAssociations"
    "fixture/DescribeEffectiveInstanceAssociations.yaml"

requestDescribeAutomationExecutions :: DescribeAutomationExecutions -> TestTree
requestDescribeAutomationExecutions =
  req
    "DescribeAutomationExecutions"
    "fixture/DescribeAutomationExecutions.yaml"

requestGetAutomationExecution :: GetAutomationExecution -> TestTree
requestGetAutomationExecution =
  req
    "GetAutomationExecution"
    "fixture/GetAutomationExecution.yaml"

requestSendCommand :: SendCommand -> TestTree
requestSendCommand =
  req
    "SendCommand"
    "fixture/SendCommand.yaml"

requestDescribePatchBaselines :: DescribePatchBaselines -> TestTree
requestDescribePatchBaselines =
  req
    "DescribePatchBaselines"
    "fixture/DescribePatchBaselines.yaml"

requestGetPatchBaseline :: GetPatchBaseline -> TestTree
requestGetPatchBaseline =
  req
    "GetPatchBaseline"
    "fixture/GetPatchBaseline.yaml"

requestRegisterTargetWithMaintenanceWindow :: RegisterTargetWithMaintenanceWindow -> TestTree
requestRegisterTargetWithMaintenanceWindow =
  req
    "RegisterTargetWithMaintenanceWindow"
    "fixture/RegisterTargetWithMaintenanceWindow.yaml"

requestStartSession :: StartSession -> TestTree
requestStartSession =
  req
    "StartSession"
    "fixture/StartSession.yaml"

requestListCommands :: ListCommands -> TestTree
requestListCommands =
  req
    "ListCommands"
    "fixture/ListCommands.yaml"

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

requestDescribeDocumentPermission :: DescribeDocumentPermission -> TestTree
requestDescribeDocumentPermission =
  req
    "DescribeDocumentPermission"
    "fixture/DescribeDocumentPermission.yaml"

requestCreateAssociationBatch :: CreateAssociationBatch -> TestTree
requestCreateAssociationBatch =
  req
    "CreateAssociationBatch"
    "fixture/CreateAssociationBatch.yaml"

requestUpdateMaintenanceWindowTarget :: UpdateMaintenanceWindowTarget -> TestTree
requestUpdateMaintenanceWindowTarget =
  req
    "UpdateMaintenanceWindowTarget"
    "fixture/UpdateMaintenanceWindowTarget.yaml"

requestCreateResourceDataSync :: CreateResourceDataSync -> TestTree
requestCreateResourceDataSync =
  req
    "CreateResourceDataSync"
    "fixture/CreateResourceDataSync.yaml"

requestCreatePatchBaseline :: CreatePatchBaseline -> TestTree
requestCreatePatchBaseline =
  req
    "CreatePatchBaseline"
    "fixture/CreatePatchBaseline.yaml"

requestDisassociateOpsItemRelatedItem :: DisassociateOpsItemRelatedItem -> TestTree
requestDisassociateOpsItemRelatedItem =
  req
    "DisassociateOpsItemRelatedItem"
    "fixture/DisassociateOpsItemRelatedItem.yaml"

-- Responses

responseGetConnectionStatus :: GetConnectionStatusResponse -> TestTree
responseGetConnectionStatus =
  res
    "GetConnectionStatusResponse"
    "fixture/GetConnectionStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConnectionStatus)

responseDescribeInstancePatches :: DescribeInstancePatchesResponse -> TestTree
responseDescribeInstancePatches =
  res
    "DescribeInstancePatchesResponse"
    "fixture/DescribeInstancePatchesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstancePatches)

responseGetInventory :: GetInventoryResponse -> TestTree
responseGetInventory =
  res
    "GetInventoryResponse"
    "fixture/GetInventoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInventory)

responseGetParameters :: GetParametersResponse -> TestTree
responseGetParameters =
  res
    "GetParametersResponse"
    "fixture/GetParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetParameters)

responseDeletePatchBaseline :: DeletePatchBaselineResponse -> TestTree
responseDeletePatchBaseline =
  res
    "DeletePatchBaselineResponse"
    "fixture/DeletePatchBaselineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePatchBaseline)

responseUpdatePatchBaseline :: UpdatePatchBaselineResponse -> TestTree
responseUpdatePatchBaseline =
  res
    "UpdatePatchBaselineResponse"
    "fixture/UpdatePatchBaselineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePatchBaseline)

responseListOpsItemEvents :: ListOpsItemEventsResponse -> TestTree
responseListOpsItemEvents =
  res
    "ListOpsItemEventsResponse"
    "fixture/ListOpsItemEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOpsItemEvents)

responseTerminateSession :: TerminateSessionResponse -> TestTree
responseTerminateSession =
  res
    "TerminateSessionResponse"
    "fixture/TerminateSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TerminateSession)

responseGetParameter :: GetParameterResponse -> TestTree
responseGetParameter =
  res
    "GetParameterResponse"
    "fixture/GetParameterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetParameter)

responseGetOpsMetadata :: GetOpsMetadataResponse -> TestTree
responseGetOpsMetadata =
  res
    "GetOpsMetadataResponse"
    "fixture/GetOpsMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOpsMetadata)

responseUpdateDocumentDefaultVersion :: UpdateDocumentDefaultVersionResponse -> TestTree
responseUpdateDocumentDefaultVersion =
  res
    "UpdateDocumentDefaultVersionResponse"
    "fixture/UpdateDocumentDefaultVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDocumentDefaultVersion)

responseListResourceDataSync :: ListResourceDataSyncResponse -> TestTree
responseListResourceDataSync =
  res
    "ListResourceDataSyncResponse"
    "fixture/ListResourceDataSyncResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceDataSync)

responseGetOpsItem :: GetOpsItemResponse -> TestTree
responseGetOpsItem =
  res
    "GetOpsItemResponse"
    "fixture/GetOpsItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOpsItem)

responseResumeSession :: ResumeSessionResponse -> TestTree
responseResumeSession =
  res
    "ResumeSessionResponse"
    "fixture/ResumeSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResumeSession)

responseGetDeployablePatchSnapshotForInstance :: GetDeployablePatchSnapshotForInstanceResponse -> TestTree
responseGetDeployablePatchSnapshotForInstance =
  res
    "GetDeployablePatchSnapshotForInstanceResponse"
    "fixture/GetDeployablePatchSnapshotForInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeployablePatchSnapshotForInstance)

responseDescribeParameters :: DescribeParametersResponse -> TestTree
responseDescribeParameters =
  res
    "DescribeParametersResponse"
    "fixture/DescribeParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeParameters)

responseDescribeOpsItems :: DescribeOpsItemsResponse -> TestTree
responseDescribeOpsItems =
  res
    "DescribeOpsItemsResponse"
    "fixture/DescribeOpsItemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOpsItems)

responseGetParametersByPath :: GetParametersByPathResponse -> TestTree
responseGetParametersByPath =
  res
    "GetParametersByPathResponse"
    "fixture/GetParametersByPathResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetParametersByPath)

responsePutComplianceItems :: PutComplianceItemsResponse -> TestTree
responsePutComplianceItems =
  res
    "PutComplianceItemsResponse"
    "fixture/PutComplianceItemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutComplianceItems)

responseListDocumentMetadataHistory :: ListDocumentMetadataHistoryResponse -> TestTree
responseListDocumentMetadataHistory =
  res
    "ListDocumentMetadataHistoryResponse"
    "fixture/ListDocumentMetadataHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDocumentMetadataHistory)

responseDescribeActivations :: DescribeActivationsResponse -> TestTree
responseDescribeActivations =
  res
    "DescribeActivationsResponse"
    "fixture/DescribeActivationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeActivations)

responseGetMaintenanceWindowTask :: GetMaintenanceWindowTaskResponse -> TestTree
responseGetMaintenanceWindowTask =
  res
    "GetMaintenanceWindowTaskResponse"
    "fixture/GetMaintenanceWindowTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMaintenanceWindowTask)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseDescribeDocument :: DescribeDocumentResponse -> TestTree
responseDescribeDocument =
  res
    "DescribeDocumentResponse"
    "fixture/DescribeDocumentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDocument)

responseDescribePatchProperties :: DescribePatchPropertiesResponse -> TestTree
responseDescribePatchProperties =
  res
    "DescribePatchPropertiesResponse"
    "fixture/DescribePatchPropertiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePatchProperties)

responseCreateAssociation :: CreateAssociationResponse -> TestTree
responseCreateAssociation =
  res
    "CreateAssociationResponse"
    "fixture/CreateAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAssociation)

responseDeleteActivation :: DeleteActivationResponse -> TestTree
responseDeleteActivation =
  res
    "DeleteActivationResponse"
    "fixture/DeleteActivationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteActivation)

responseDescribeMaintenanceWindowExecutions :: DescribeMaintenanceWindowExecutionsResponse -> TestTree
responseDescribeMaintenanceWindowExecutions =
  res
    "DescribeMaintenanceWindowExecutionsResponse"
    "fixture/DescribeMaintenanceWindowExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMaintenanceWindowExecutions)

responseDescribeMaintenanceWindowsForTarget :: DescribeMaintenanceWindowsForTargetResponse -> TestTree
responseDescribeMaintenanceWindowsForTarget =
  res
    "DescribeMaintenanceWindowsForTargetResponse"
    "fixture/DescribeMaintenanceWindowsForTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMaintenanceWindowsForTarget)

responseCreateOpsMetadata :: CreateOpsMetadataResponse -> TestTree
responseCreateOpsMetadata =
  res
    "CreateOpsMetadataResponse"
    "fixture/CreateOpsMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateOpsMetadata)

responseStartChangeRequestExecution :: StartChangeRequestExecutionResponse -> TestTree
responseStartChangeRequestExecution =
  res
    "StartChangeRequestExecutionResponse"
    "fixture/StartChangeRequestExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartChangeRequestExecution)

responseCancelMaintenanceWindowExecution :: CancelMaintenanceWindowExecutionResponse -> TestTree
responseCancelMaintenanceWindowExecution =
  res
    "CancelMaintenanceWindowExecutionResponse"
    "fixture/CancelMaintenanceWindowExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelMaintenanceWindowExecution)

responseGetInventorySchema :: GetInventorySchemaResponse -> TestTree
responseGetInventorySchema =
  res
    "GetInventorySchemaResponse"
    "fixture/GetInventorySchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInventorySchema)

responseListComplianceSummaries :: ListComplianceSummariesResponse -> TestTree
responseListComplianceSummaries =
  res
    "ListComplianceSummariesResponse"
    "fixture/ListComplianceSummariesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListComplianceSummaries)

responseStartAutomationExecution :: StartAutomationExecutionResponse -> TestTree
responseStartAutomationExecution =
  res
    "StartAutomationExecutionResponse"
    "fixture/StartAutomationExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartAutomationExecution)

responseCreateOpsItem :: CreateOpsItemResponse -> TestTree
responseCreateOpsItem =
  res
    "CreateOpsItemResponse"
    "fixture/CreateOpsItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateOpsItem)

responseCreateActivation :: CreateActivationResponse -> TestTree
responseCreateActivation =
  res
    "CreateActivationResponse"
    "fixture/CreateActivationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateActivation)

responseDeleteMaintenanceWindow :: DeleteMaintenanceWindowResponse -> TestTree
responseDeleteMaintenanceWindow =
  res
    "DeleteMaintenanceWindowResponse"
    "fixture/DeleteMaintenanceWindowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMaintenanceWindow)

responseUpdateMaintenanceWindow :: UpdateMaintenanceWindowResponse -> TestTree
responseUpdateMaintenanceWindow =
  res
    "UpdateMaintenanceWindowResponse"
    "fixture/UpdateMaintenanceWindowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMaintenanceWindow)

responseDescribeSessions :: DescribeSessionsResponse -> TestTree
responseDescribeSessions =
  res
    "DescribeSessionsResponse"
    "fixture/DescribeSessionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSessions)

responseDescribeMaintenanceWindowExecutionTasks :: DescribeMaintenanceWindowExecutionTasksResponse -> TestTree
responseDescribeMaintenanceWindowExecutionTasks =
  res
    "DescribeMaintenanceWindowExecutionTasksResponse"
    "fixture/DescribeMaintenanceWindowExecutionTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMaintenanceWindowExecutionTasks)

responseGetDefaultPatchBaseline :: GetDefaultPatchBaselineResponse -> TestTree
responseGetDefaultPatchBaseline =
  res
    "GetDefaultPatchBaselineResponse"
    "fixture/GetDefaultPatchBaselineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDefaultPatchBaseline)

responseGetMaintenanceWindowExecutionTask :: GetMaintenanceWindowExecutionTaskResponse -> TestTree
responseGetMaintenanceWindowExecutionTask =
  res
    "GetMaintenanceWindowExecutionTaskResponse"
    "fixture/GetMaintenanceWindowExecutionTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMaintenanceWindowExecutionTask)

responseCreateDocument :: CreateDocumentResponse -> TestTree
responseCreateDocument =
  res
    "CreateDocumentResponse"
    "fixture/CreateDocumentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDocument)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTagsFromResource)

responseGetCalendarState :: GetCalendarStateResponse -> TestTree
responseGetCalendarState =
  res
    "GetCalendarStateResponse"
    "fixture/GetCalendarStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCalendarState)

responseDeleteParameters :: DeleteParametersResponse -> TestTree
responseDeleteParameters =
  res
    "DeleteParametersResponse"
    "fixture/DeleteParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteParameters)

responseDescribePatchGroupState :: DescribePatchGroupStateResponse -> TestTree
responseDescribePatchGroupState =
  res
    "DescribePatchGroupStateResponse"
    "fixture/DescribePatchGroupStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePatchGroupState)

responseListCommandInvocations :: ListCommandInvocationsResponse -> TestTree
responseListCommandInvocations =
  res
    "ListCommandInvocationsResponse"
    "fixture/ListCommandInvocationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCommandInvocations)

responseDeregisterTargetFromMaintenanceWindow :: DeregisterTargetFromMaintenanceWindowResponse -> TestTree
responseDeregisterTargetFromMaintenanceWindow =
  res
    "DeregisterTargetFromMaintenanceWindowResponse"
    "fixture/DeregisterTargetFromMaintenanceWindowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterTargetFromMaintenanceWindow)

responseDescribeEffectivePatchesForPatchBaseline :: DescribeEffectivePatchesForPatchBaselineResponse -> TestTree
responseDescribeEffectivePatchesForPatchBaseline =
  res
    "DescribeEffectivePatchesForPatchBaselineResponse"
    "fixture/DescribeEffectivePatchesForPatchBaselineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEffectivePatchesForPatchBaseline)

responseUnlabelParameterVersion :: UnlabelParameterVersionResponse -> TestTree
responseUnlabelParameterVersion =
  res
    "UnlabelParameterVersionResponse"
    "fixture/UnlabelParameterVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UnlabelParameterVersion)

responseDescribeMaintenanceWindowTargets :: DescribeMaintenanceWindowTargetsResponse -> TestTree
responseDescribeMaintenanceWindowTargets =
  res
    "DescribeMaintenanceWindowTargetsResponse"
    "fixture/DescribeMaintenanceWindowTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMaintenanceWindowTargets)

responseResetServiceSetting :: ResetServiceSettingResponse -> TestTree
responseResetServiceSetting =
  res
    "ResetServiceSettingResponse"
    "fixture/ResetServiceSettingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetServiceSetting)

responseRegisterPatchBaselineForPatchGroup :: RegisterPatchBaselineForPatchGroupResponse -> TestTree
responseRegisterPatchBaselineForPatchGroup =
  res
    "RegisterPatchBaselineForPatchGroupResponse"
    "fixture/RegisterPatchBaselineForPatchGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterPatchBaselineForPatchGroup)

responseListDocuments :: ListDocumentsResponse -> TestTree
responseListDocuments =
  res
    "ListDocumentsResponse"
    "fixture/ListDocumentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDocuments)

responseDescribeInstancePatchStates :: DescribeInstancePatchStatesResponse -> TestTree
responseDescribeInstancePatchStates =
  res
    "DescribeInstancePatchStatesResponse"
    "fixture/DescribeInstancePatchStatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstancePatchStates)

responseGetOpsSummary :: GetOpsSummaryResponse -> TestTree
responseGetOpsSummary =
  res
    "GetOpsSummaryResponse"
    "fixture/GetOpsSummaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOpsSummary)

responseGetPatchBaselineForPatchGroup :: GetPatchBaselineForPatchGroupResponse -> TestTree
responseGetPatchBaselineForPatchGroup =
  res
    "GetPatchBaselineForPatchGroupResponse"
    "fixture/GetPatchBaselineForPatchGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPatchBaselineForPatchGroup)

responseUpdateManagedInstanceRole :: UpdateManagedInstanceRoleResponse -> TestTree
responseUpdateManagedInstanceRole =
  res
    "UpdateManagedInstanceRoleResponse"
    "fixture/UpdateManagedInstanceRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateManagedInstanceRole)

responseListComplianceItems :: ListComplianceItemsResponse -> TestTree
responseListComplianceItems =
  res
    "ListComplianceItemsResponse"
    "fixture/ListComplianceItemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListComplianceItems)

responseGetDocument :: GetDocumentResponse -> TestTree
responseGetDocument =
  res
    "GetDocumentResponse"
    "fixture/GetDocumentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDocument)

responseDescribeMaintenanceWindowSchedule :: DescribeMaintenanceWindowScheduleResponse -> TestTree
responseDescribeMaintenanceWindowSchedule =
  res
    "DescribeMaintenanceWindowScheduleResponse"
    "fixture/DescribeMaintenanceWindowScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMaintenanceWindowSchedule)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTagsToResource)

responseCancelCommand :: CancelCommandResponse -> TestTree
responseCancelCommand =
  res
    "CancelCommandResponse"
    "fixture/CancelCommandResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelCommand)

responseDescribeAutomationStepExecutions :: DescribeAutomationStepExecutionsResponse -> TestTree
responseDescribeAutomationStepExecutions =
  res
    "DescribeAutomationStepExecutionsResponse"
    "fixture/DescribeAutomationStepExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAutomationStepExecutions)

responseGetCommandInvocation :: GetCommandInvocationResponse -> TestTree
responseGetCommandInvocation =
  res
    "GetCommandInvocationResponse"
    "fixture/GetCommandInvocationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCommandInvocation)

responseDescribeInstancePatchStatesForPatchGroup :: DescribeInstancePatchStatesForPatchGroupResponse -> TestTree
responseDescribeInstancePatchStatesForPatchGroup =
  res
    "DescribeInstancePatchStatesForPatchGroupResponse"
    "fixture/DescribeInstancePatchStatesForPatchGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstancePatchStatesForPatchGroup)

responseDeregisterManagedInstance :: DeregisterManagedInstanceResponse -> TestTree
responseDeregisterManagedInstance =
  res
    "DeregisterManagedInstanceResponse"
    "fixture/DeregisterManagedInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterManagedInstance)

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

responseModifyDocumentPermission :: ModifyDocumentPermissionResponse -> TestTree
responseModifyDocumentPermission =
  res
    "ModifyDocumentPermissionResponse"
    "fixture/ModifyDocumentPermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyDocumentPermission)

responseUpdateResourceDataSync :: UpdateResourceDataSyncResponse -> TestTree
responseUpdateResourceDataSync =
  res
    "UpdateResourceDataSyncResponse"
    "fixture/UpdateResourceDataSyncResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResourceDataSync)

responseDeleteResourceDataSync :: DeleteResourceDataSyncResponse -> TestTree
responseDeleteResourceDataSync =
  res
    "DeleteResourceDataSyncResponse"
    "fixture/DeleteResourceDataSyncResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourceDataSync)

responseUpdateAssociationStatus :: UpdateAssociationStatusResponse -> TestTree
responseUpdateAssociationStatus =
  res
    "UpdateAssociationStatusResponse"
    "fixture/UpdateAssociationStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAssociationStatus)

responseDescribeAvailablePatches :: DescribeAvailablePatchesResponse -> TestTree
responseDescribeAvailablePatches =
  res
    "DescribeAvailablePatchesResponse"
    "fixture/DescribeAvailablePatchesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAvailablePatches)

responseListDocumentVersions :: ListDocumentVersionsResponse -> TestTree
responseListDocumentVersions =
  res
    "ListDocumentVersionsResponse"
    "fixture/ListDocumentVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDocumentVersions)

responseDeregisterPatchBaselineForPatchGroup :: DeregisterPatchBaselineForPatchGroupResponse -> TestTree
responseDeregisterPatchBaselineForPatchGroup =
  res
    "DeregisterPatchBaselineForPatchGroupResponse"
    "fixture/DeregisterPatchBaselineForPatchGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterPatchBaselineForPatchGroup)

responseDescribePatchGroups :: DescribePatchGroupsResponse -> TestTree
responseDescribePatchGroups =
  res
    "DescribePatchGroupsResponse"
    "fixture/DescribePatchGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePatchGroups)

responseGetMaintenanceWindow :: GetMaintenanceWindowResponse -> TestTree
responseGetMaintenanceWindow =
  res
    "GetMaintenanceWindowResponse"
    "fixture/GetMaintenanceWindowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMaintenanceWindow)

responseDescribeMaintenanceWindows :: DescribeMaintenanceWindowsResponse -> TestTree
responseDescribeMaintenanceWindows =
  res
    "DescribeMaintenanceWindowsResponse"
    "fixture/DescribeMaintenanceWindowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMaintenanceWindows)

responseRegisterTaskWithMaintenanceWindow :: RegisterTaskWithMaintenanceWindowResponse -> TestTree
responseRegisterTaskWithMaintenanceWindow =
  res
    "RegisterTaskWithMaintenanceWindowResponse"
    "fixture/RegisterTaskWithMaintenanceWindowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterTaskWithMaintenanceWindow)

responseRegisterDefaultPatchBaseline :: RegisterDefaultPatchBaselineResponse -> TestTree
responseRegisterDefaultPatchBaseline =
  res
    "RegisterDefaultPatchBaselineResponse"
    "fixture/RegisterDefaultPatchBaselineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterDefaultPatchBaseline)

responseListResourceComplianceSummaries :: ListResourceComplianceSummariesResponse -> TestTree
responseListResourceComplianceSummaries =
  res
    "ListResourceComplianceSummariesResponse"
    "fixture/ListResourceComplianceSummariesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceComplianceSummaries)

responseAssociateOpsItemRelatedItem :: AssociateOpsItemRelatedItemResponse -> TestTree
responseAssociateOpsItemRelatedItem =
  res
    "AssociateOpsItemRelatedItemResponse"
    "fixture/AssociateOpsItemRelatedItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateOpsItemRelatedItem)

responseListAssociationVersions :: ListAssociationVersionsResponse -> TestTree
responseListAssociationVersions =
  res
    "ListAssociationVersionsResponse"
    "fixture/ListAssociationVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssociationVersions)

responseUpdateServiceSetting :: UpdateServiceSettingResponse -> TestTree
responseUpdateServiceSetting =
  res
    "UpdateServiceSettingResponse"
    "fixture/UpdateServiceSettingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServiceSetting)

responseDescribeMaintenanceWindowTasks :: DescribeMaintenanceWindowTasksResponse -> TestTree
responseDescribeMaintenanceWindowTasks =
  res
    "DescribeMaintenanceWindowTasksResponse"
    "fixture/DescribeMaintenanceWindowTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMaintenanceWindowTasks)

responseDescribeInstanceAssociationsStatus :: DescribeInstanceAssociationsStatusResponse -> TestTree
responseDescribeInstanceAssociationsStatus =
  res
    "DescribeInstanceAssociationsStatusResponse"
    "fixture/DescribeInstanceAssociationsStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceAssociationsStatus)

responseListOpsItemRelatedItems :: ListOpsItemRelatedItemsResponse -> TestTree
responseListOpsItemRelatedItems =
  res
    "ListOpsItemRelatedItemsResponse"
    "fixture/ListOpsItemRelatedItemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOpsItemRelatedItems)

responseDeregisterTaskFromMaintenanceWindow :: DeregisterTaskFromMaintenanceWindowResponse -> TestTree
responseDeregisterTaskFromMaintenanceWindow =
  res
    "DeregisterTaskFromMaintenanceWindowResponse"
    "fixture/DeregisterTaskFromMaintenanceWindowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterTaskFromMaintenanceWindow)

responseListInventoryEntries :: ListInventoryEntriesResponse -> TestTree
responseListInventoryEntries =
  res
    "ListInventoryEntriesResponse"
    "fixture/ListInventoryEntriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInventoryEntries)

responseLabelParameterVersion :: LabelParameterVersionResponse -> TestTree
responseLabelParameterVersion =
  res
    "LabelParameterVersionResponse"
    "fixture/LabelParameterVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy LabelParameterVersion)

responseUpdateMaintenanceWindowTask :: UpdateMaintenanceWindowTaskResponse -> TestTree
responseUpdateMaintenanceWindowTask =
  res
    "UpdateMaintenanceWindowTaskResponse"
    "fixture/UpdateMaintenanceWindowTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMaintenanceWindowTask)

responseGetParameterHistory :: GetParameterHistoryResponse -> TestTree
responseGetParameterHistory =
  res
    "GetParameterHistoryResponse"
    "fixture/GetParameterHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetParameterHistory)

responseDescribeAssociationExecutions :: DescribeAssociationExecutionsResponse -> TestTree
responseDescribeAssociationExecutions =
  res
    "DescribeAssociationExecutionsResponse"
    "fixture/DescribeAssociationExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAssociationExecutions)

responseGetServiceSetting :: GetServiceSettingResponse -> TestTree
responseGetServiceSetting =
  res
    "GetServiceSettingResponse"
    "fixture/GetServiceSettingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServiceSetting)

responseStartAssociationsOnce :: StartAssociationsOnceResponse -> TestTree
responseStartAssociationsOnce =
  res
    "StartAssociationsOnceResponse"
    "fixture/StartAssociationsOnceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartAssociationsOnce)

responseCreateMaintenanceWindow :: CreateMaintenanceWindowResponse -> TestTree
responseCreateMaintenanceWindow =
  res
    "CreateMaintenanceWindowResponse"
    "fixture/CreateMaintenanceWindowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMaintenanceWindow)

responseStopAutomationExecution :: StopAutomationExecutionResponse -> TestTree
responseStopAutomationExecution =
  res
    "StopAutomationExecutionResponse"
    "fixture/StopAutomationExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopAutomationExecution)

responseGetMaintenanceWindowExecution :: GetMaintenanceWindowExecutionResponse -> TestTree
responseGetMaintenanceWindowExecution =
  res
    "GetMaintenanceWindowExecutionResponse"
    "fixture/GetMaintenanceWindowExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMaintenanceWindowExecution)

responseSendAutomationSignal :: SendAutomationSignalResponse -> TestTree
responseSendAutomationSignal =
  res
    "SendAutomationSignalResponse"
    "fixture/SendAutomationSignalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendAutomationSignal)

responseDeleteOpsMetadata :: DeleteOpsMetadataResponse -> TestTree
responseDeleteOpsMetadata =
  res
    "DeleteOpsMetadataResponse"
    "fixture/DeleteOpsMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOpsMetadata)

responseUpdateOpsMetadata :: UpdateOpsMetadataResponse -> TestTree
responseUpdateOpsMetadata =
  res
    "UpdateOpsMetadataResponse"
    "fixture/UpdateOpsMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateOpsMetadata)

responsePutParameter :: PutParameterResponse -> TestTree
responsePutParameter =
  res
    "PutParameterResponse"
    "fixture/PutParameterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutParameter)

responseDescribeMaintenanceWindowExecutionTaskInvocations :: DescribeMaintenanceWindowExecutionTaskInvocationsResponse -> TestTree
responseDescribeMaintenanceWindowExecutionTaskInvocations =
  res
    "DescribeMaintenanceWindowExecutionTaskInvocationsResponse"
    "fixture/DescribeMaintenanceWindowExecutionTaskInvocationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMaintenanceWindowExecutionTaskInvocations)

responseGetMaintenanceWindowExecutionTaskInvocation :: GetMaintenanceWindowExecutionTaskInvocationResponse -> TestTree
responseGetMaintenanceWindowExecutionTaskInvocation =
  res
    "GetMaintenanceWindowExecutionTaskInvocationResponse"
    "fixture/GetMaintenanceWindowExecutionTaskInvocationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMaintenanceWindowExecutionTaskInvocation)

responseDeleteParameter :: DeleteParameterResponse -> TestTree
responseDeleteParameter =
  res
    "DeleteParameterResponse"
    "fixture/DeleteParameterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteParameter)

responseDescribeInstanceInformation :: DescribeInstanceInformationResponse -> TestTree
responseDescribeInstanceInformation =
  res
    "DescribeInstanceInformationResponse"
    "fixture/DescribeInstanceInformationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceInformation)

responseListAssociations :: ListAssociationsResponse -> TestTree
responseListAssociations =
  res
    "ListAssociationsResponse"
    "fixture/ListAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssociations)

responseUpdateOpsItem :: UpdateOpsItemResponse -> TestTree
responseUpdateOpsItem =
  res
    "UpdateOpsItemResponse"
    "fixture/UpdateOpsItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateOpsItem)

responseDeleteAssociation :: DeleteAssociationResponse -> TestTree
responseDeleteAssociation =
  res
    "DeleteAssociationResponse"
    "fixture/DeleteAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAssociation)

responseUpdateAssociation :: UpdateAssociationResponse -> TestTree
responseUpdateAssociation =
  res
    "UpdateAssociationResponse"
    "fixture/UpdateAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAssociation)

responseDescribeInventoryDeletions :: DescribeInventoryDeletionsResponse -> TestTree
responseDescribeInventoryDeletions =
  res
    "DescribeInventoryDeletionsResponse"
    "fixture/DescribeInventoryDeletionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInventoryDeletions)

responseDeleteInventory :: DeleteInventoryResponse -> TestTree
responseDeleteInventory =
  res
    "DeleteInventoryResponse"
    "fixture/DeleteInventoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInventory)

responsePutInventory :: PutInventoryResponse -> TestTree
responsePutInventory =
  res
    "PutInventoryResponse"
    "fixture/PutInventoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutInventory)

responseUpdateDocumentMetadata :: UpdateDocumentMetadataResponse -> TestTree
responseUpdateDocumentMetadata =
  res
    "UpdateDocumentMetadataResponse"
    "fixture/UpdateDocumentMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDocumentMetadata)

responseListOpsMetadata :: ListOpsMetadataResponse -> TestTree
responseListOpsMetadata =
  res
    "ListOpsMetadataResponse"
    "fixture/ListOpsMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOpsMetadata)

responseDescribeEffectiveInstanceAssociations :: DescribeEffectiveInstanceAssociationsResponse -> TestTree
responseDescribeEffectiveInstanceAssociations =
  res
    "DescribeEffectiveInstanceAssociationsResponse"
    "fixture/DescribeEffectiveInstanceAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEffectiveInstanceAssociations)

responseDescribeAutomationExecutions :: DescribeAutomationExecutionsResponse -> TestTree
responseDescribeAutomationExecutions =
  res
    "DescribeAutomationExecutionsResponse"
    "fixture/DescribeAutomationExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAutomationExecutions)

responseGetAutomationExecution :: GetAutomationExecutionResponse -> TestTree
responseGetAutomationExecution =
  res
    "GetAutomationExecutionResponse"
    "fixture/GetAutomationExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAutomationExecution)

responseSendCommand :: SendCommandResponse -> TestTree
responseSendCommand =
  res
    "SendCommandResponse"
    "fixture/SendCommandResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendCommand)

responseDescribePatchBaselines :: DescribePatchBaselinesResponse -> TestTree
responseDescribePatchBaselines =
  res
    "DescribePatchBaselinesResponse"
    "fixture/DescribePatchBaselinesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePatchBaselines)

responseGetPatchBaseline :: GetPatchBaselineResponse -> TestTree
responseGetPatchBaseline =
  res
    "GetPatchBaselineResponse"
    "fixture/GetPatchBaselineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPatchBaseline)

responseRegisterTargetWithMaintenanceWindow :: RegisterTargetWithMaintenanceWindowResponse -> TestTree
responseRegisterTargetWithMaintenanceWindow =
  res
    "RegisterTargetWithMaintenanceWindowResponse"
    "fixture/RegisterTargetWithMaintenanceWindowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterTargetWithMaintenanceWindow)

responseStartSession :: StartSessionResponse -> TestTree
responseStartSession =
  res
    "StartSessionResponse"
    "fixture/StartSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartSession)

responseListCommands :: ListCommandsResponse -> TestTree
responseListCommands =
  res
    "ListCommandsResponse"
    "fixture/ListCommandsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCommands)

responseUpdateDocument :: UpdateDocumentResponse -> TestTree
responseUpdateDocument =
  res
    "UpdateDocumentResponse"
    "fixture/UpdateDocumentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDocument)

responseDeleteDocument :: DeleteDocumentResponse -> TestTree
responseDeleteDocument =
  res
    "DeleteDocumentResponse"
    "fixture/DeleteDocumentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDocument)

responseDescribeDocumentPermission :: DescribeDocumentPermissionResponse -> TestTree
responseDescribeDocumentPermission =
  res
    "DescribeDocumentPermissionResponse"
    "fixture/DescribeDocumentPermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDocumentPermission)

responseCreateAssociationBatch :: CreateAssociationBatchResponse -> TestTree
responseCreateAssociationBatch =
  res
    "CreateAssociationBatchResponse"
    "fixture/CreateAssociationBatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAssociationBatch)

responseUpdateMaintenanceWindowTarget :: UpdateMaintenanceWindowTargetResponse -> TestTree
responseUpdateMaintenanceWindowTarget =
  res
    "UpdateMaintenanceWindowTargetResponse"
    "fixture/UpdateMaintenanceWindowTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMaintenanceWindowTarget)

responseCreateResourceDataSync :: CreateResourceDataSyncResponse -> TestTree
responseCreateResourceDataSync =
  res
    "CreateResourceDataSyncResponse"
    "fixture/CreateResourceDataSyncResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResourceDataSync)

responseCreatePatchBaseline :: CreatePatchBaselineResponse -> TestTree
responseCreatePatchBaseline =
  res
    "CreatePatchBaselineResponse"
    "fixture/CreatePatchBaselineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePatchBaseline)

responseDisassociateOpsItemRelatedItem :: DisassociateOpsItemRelatedItemResponse -> TestTree
responseDisassociateOpsItemRelatedItem =
  res
    "DisassociateOpsItemRelatedItemResponse"
    "fixture/DisassociateOpsItemRelatedItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateOpsItemRelatedItem)
