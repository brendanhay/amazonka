{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SSM
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestGetConnectionStatus $
--             mkGetConnectionStatus
--
--         , requestDescribeInstancePatches $
--             mkDescribeInstancePatches
--
--         , requestGetInventory $
--             mkGetInventory
--
--         , requestGetParameters $
--             mkGetParameters
--
--         , requestDeletePatchBaseline $
--             mkDeletePatchBaseline
--
--         , requestUpdatePatchBaseline $
--             mkUpdatePatchBaseline
--
--         , requestTerminateSession $
--             mkTerminateSession
--
--         , requestGetParameter $
--             mkGetParameter
--
--         , requestUpdateDocumentDefaultVersion $
--             mkUpdateDocumentDefaultVersion
--
--         , requestListResourceDataSync $
--             mkListResourceDataSync
--
--         , requestGetOpsItem $
--             mkGetOpsItem
--
--         , requestResumeSession $
--             mkResumeSession
--
--         , requestGetDeployablePatchSnapshotForInstance $
--             mkGetDeployablePatchSnapshotForInstance
--
--         , requestDescribeParameters $
--             mkDescribeParameters
--
--         , requestDescribeOpsItems $
--             mkDescribeOpsItems
--
--         , requestGetParametersByPath $
--             mkGetParametersByPath
--
--         , requestPutComplianceItems $
--             mkPutComplianceItems
--
--         , requestDescribeActivations $
--             mkDescribeActivations
--
--         , requestGetMaintenanceWindowTask $
--             mkGetMaintenanceWindowTask
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestDescribeDocument $
--             mkDescribeDocument
--
--         , requestDescribePatchProperties $
--             mkDescribePatchProperties
--
--         , requestCreateAssociation $
--             mkCreateAssociation
--
--         , requestDeleteActivation $
--             mkDeleteActivation
--
--         , requestDescribeMaintenanceWindowExecutions $
--             mkDescribeMaintenanceWindowExecutions
--
--         , requestDescribeMaintenanceWindowsForTarget $
--             mkDescribeMaintenanceWindowsForTarget
--
--         , requestCancelMaintenanceWindowExecution $
--             mkCancelMaintenanceWindowExecution
--
--         , requestGetInventorySchema $
--             mkGetInventorySchema
--
--         , requestListComplianceSummaries $
--             mkListComplianceSummaries
--
--         , requestStartAutomationExecution $
--             mkStartAutomationExecution
--
--         , requestCreateOpsItem $
--             mkCreateOpsItem
--
--         , requestCreateActivation $
--             mkCreateActivation
--
--         , requestDeleteMaintenanceWindow $
--             mkDeleteMaintenanceWindow
--
--         , requestUpdateMaintenanceWindow $
--             mkUpdateMaintenanceWindow
--
--         , requestDescribeSessions $
--             mkDescribeSessions
--
--         , requestDescribeMaintenanceWindowExecutionTasks $
--             mkDescribeMaintenanceWindowExecutionTasks
--
--         , requestGetDefaultPatchBaseline $
--             mkGetDefaultPatchBaseline
--
--         , requestGetMaintenanceWindowExecutionTask $
--             mkGetMaintenanceWindowExecutionTask
--
--         , requestCreateDocument $
--             mkCreateDocument
--
--         , requestRemoveTagsFromResource $
--             mkRemoveTagsFromResource
--
--         , requestGetCalendarState $
--             mkGetCalendarState
--
--         , requestDeleteParameters $
--             mkDeleteParameters
--
--         , requestDescribePatchGroupState $
--             mkDescribePatchGroupState
--
--         , requestListCommandInvocations $
--             mkListCommandInvocations
--
--         , requestDeregisterTargetFromMaintenanceWindow $
--             mkDeregisterTargetFromMaintenanceWindow
--
--         , requestDescribeEffectivePatchesForPatchBaseline $
--             mkDescribeEffectivePatchesForPatchBaseline
--
--         , requestDescribeMaintenanceWindowTargets $
--             mkDescribeMaintenanceWindowTargets
--
--         , requestResetServiceSetting $
--             mkResetServiceSetting
--
--         , requestRegisterPatchBaselineForPatchGroup $
--             mkRegisterPatchBaselineForPatchGroup
--
--         , requestListDocuments $
--             mkListDocuments
--
--         , requestDescribeInstancePatchStates $
--             mkDescribeInstancePatchStates
--
--         , requestGetOpsSummary $
--             mkGetOpsSummary
--
--         , requestGetPatchBaselineForPatchGroup $
--             mkGetPatchBaselineForPatchGroup
--
--         , requestUpdateManagedInstanceRole $
--             mkUpdateManagedInstanceRole
--
--         , requestListComplianceItems $
--             mkListComplianceItems
--
--         , requestGetDocument $
--             mkGetDocument
--
--         , requestDescribeMaintenanceWindowSchedule $
--             mkDescribeMaintenanceWindowSchedule
--
--         , requestAddTagsToResource $
--             mkAddTagsToResource
--
--         , requestCancelCommand $
--             mkCancelCommand
--
--         , requestDescribeAutomationStepExecutions $
--             mkDescribeAutomationStepExecutions
--
--         , requestGetCommandInvocation $
--             mkGetCommandInvocation
--
--         , requestDescribeInstancePatchStatesForPatchGroup $
--             mkDescribeInstancePatchStatesForPatchGroup
--
--         , requestDeregisterManagedInstance $
--             mkDeregisterManagedInstance
--
--         , requestDescribeAssociation $
--             mkDescribeAssociation
--
--         , requestDescribeAssociationExecutionTargets $
--             mkDescribeAssociationExecutionTargets
--
--         , requestModifyDocumentPermission $
--             mkModifyDocumentPermission
--
--         , requestUpdateResourceDataSync $
--             mkUpdateResourceDataSync
--
--         , requestDeleteResourceDataSync $
--             mkDeleteResourceDataSync
--
--         , requestUpdateAssociationStatus $
--             mkUpdateAssociationStatus
--
--         , requestDescribeAvailablePatches $
--             mkDescribeAvailablePatches
--
--         , requestListDocumentVersions $
--             mkListDocumentVersions
--
--         , requestDeregisterPatchBaselineForPatchGroup $
--             mkDeregisterPatchBaselineForPatchGroup
--
--         , requestDescribePatchGroups $
--             mkDescribePatchGroups
--
--         , requestGetMaintenanceWindow $
--             mkGetMaintenanceWindow
--
--         , requestDescribeMaintenanceWindows $
--             mkDescribeMaintenanceWindows
--
--         , requestRegisterTaskWithMaintenanceWindow $
--             mkRegisterTaskWithMaintenanceWindow
--
--         , requestRegisterDefaultPatchBaseline $
--             mkRegisterDefaultPatchBaseline
--
--         , requestListResourceComplianceSummaries $
--             mkListResourceComplianceSummaries
--
--         , requestListAssociationVersions $
--             mkListAssociationVersions
--
--         , requestUpdateServiceSetting $
--             mkUpdateServiceSetting
--
--         , requestDescribeMaintenanceWindowTasks $
--             mkDescribeMaintenanceWindowTasks
--
--         , requestDescribeInstanceAssociationsStatus $
--             mkDescribeInstanceAssociationsStatus
--
--         , requestDeregisterTaskFromMaintenanceWindow $
--             mkDeregisterTaskFromMaintenanceWindow
--
--         , requestListInventoryEntries $
--             mkListInventoryEntries
--
--         , requestLabelParameterVersion $
--             mkLabelParameterVersion
--
--         , requestUpdateMaintenanceWindowTask $
--             mkUpdateMaintenanceWindowTask
--
--         , requestGetParameterHistory $
--             mkGetParameterHistory
--
--         , requestDescribeAssociationExecutions $
--             mkDescribeAssociationExecutions
--
--         , requestGetServiceSetting $
--             mkGetServiceSetting
--
--         , requestStartAssociationsOnce $
--             mkStartAssociationsOnce
--
--         , requestCreateMaintenanceWindow $
--             mkCreateMaintenanceWindow
--
--         , requestStopAutomationExecution $
--             mkStopAutomationExecution
--
--         , requestGetMaintenanceWindowExecution $
--             mkGetMaintenanceWindowExecution
--
--         , requestSendAutomationSignal $
--             mkSendAutomationSignal
--
--         , requestPutParameter $
--             mkPutParameter
--
--         , requestDescribeMaintenanceWindowExecutionTaskInvocations $
--             mkDescribeMaintenanceWindowExecutionTaskInvocations
--
--         , requestGetMaintenanceWindowExecutionTaskInvocation $
--             mkGetMaintenanceWindowExecutionTaskInvocation
--
--         , requestDeleteParameter $
--             mkDeleteParameter
--
--         , requestDescribeInstanceInformation $
--             mkDescribeInstanceInformation
--
--         , requestListAssociations $
--             mkListAssociations
--
--         , requestUpdateOpsItem $
--             mkUpdateOpsItem
--
--         , requestDeleteAssociation $
--             mkDeleteAssociation
--
--         , requestUpdateAssociation $
--             mkUpdateAssociation
--
--         , requestDescribeInventoryDeletions $
--             mkDescribeInventoryDeletions
--
--         , requestDeleteInventory $
--             mkDeleteInventory
--
--         , requestPutInventory $
--             mkPutInventory
--
--         , requestDescribeEffectiveInstanceAssociations $
--             mkDescribeEffectiveInstanceAssociations
--
--         , requestDescribeAutomationExecutions $
--             mkDescribeAutomationExecutions
--
--         , requestGetAutomationExecution $
--             mkGetAutomationExecution
--
--         , requestSendCommand $
--             mkSendCommand
--
--         , requestDescribePatchBaselines $
--             mkDescribePatchBaselines
--
--         , requestGetPatchBaseline $
--             mkGetPatchBaseline
--
--         , requestRegisterTargetWithMaintenanceWindow $
--             mkRegisterTargetWithMaintenanceWindow
--
--         , requestStartSession $
--             mkStartSession
--
--         , requestListCommands $
--             mkListCommands
--
--         , requestUpdateDocument $
--             mkUpdateDocument
--
--         , requestDeleteDocument $
--             mkDeleteDocument
--
--         , requestDescribeDocumentPermission $
--             mkDescribeDocumentPermission
--
--         , requestCreateAssociationBatch $
--             mkCreateAssociationBatch
--
--         , requestUpdateMaintenanceWindowTarget $
--             mkUpdateMaintenanceWindowTarget
--
--         , requestCreateResourceDataSync $
--             mkCreateResourceDataSync
--
--         , requestCreatePatchBaseline $
--             mkCreatePatchBaseline
--
--           ]

--     , testGroup "response"
--         [ responseGetConnectionStatus $
--             mkGetConnectionStatusResponse
--
--         , responseDescribeInstancePatches $
--             mkDescribeInstancePatchesResponse
--
--         , responseGetInventory $
--             mkGetInventoryResponse
--
--         , responseGetParameters $
--             mkGetParametersResponse
--
--         , responseDeletePatchBaseline $
--             mkDeletePatchBaselineResponse
--
--         , responseUpdatePatchBaseline $
--             mkUpdatePatchBaselineResponse
--
--         , responseTerminateSession $
--             mkTerminateSessionResponse
--
--         , responseGetParameter $
--             mkGetParameterResponse
--
--         , responseUpdateDocumentDefaultVersion $
--             mkUpdateDocumentDefaultVersionResponse
--
--         , responseListResourceDataSync $
--             mkListResourceDataSyncResponse
--
--         , responseGetOpsItem $
--             mkGetOpsItemResponse
--
--         , responseResumeSession $
--             mkResumeSessionResponse
--
--         , responseGetDeployablePatchSnapshotForInstance $
--             mkGetDeployablePatchSnapshotForInstanceResponse
--
--         , responseDescribeParameters $
--             mkDescribeParametersResponse
--
--         , responseDescribeOpsItems $
--             mkDescribeOpsItemsResponse
--
--         , responseGetParametersByPath $
--             mkGetParametersByPathResponse
--
--         , responsePutComplianceItems $
--             mkPutComplianceItemsResponse
--
--         , responseDescribeActivations $
--             mkDescribeActivationsResponse
--
--         , responseGetMaintenanceWindowTask $
--             mkGetMaintenanceWindowTaskResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseDescribeDocument $
--             mkDescribeDocumentResponse
--
--         , responseDescribePatchProperties $
--             mkDescribePatchPropertiesResponse
--
--         , responseCreateAssociation $
--             mkCreateAssociationResponse
--
--         , responseDeleteActivation $
--             mkDeleteActivationResponse
--
--         , responseDescribeMaintenanceWindowExecutions $
--             mkDescribeMaintenanceWindowExecutionsResponse
--
--         , responseDescribeMaintenanceWindowsForTarget $
--             mkDescribeMaintenanceWindowsForTargetResponse
--
--         , responseCancelMaintenanceWindowExecution $
--             mkCancelMaintenanceWindowExecutionResponse
--
--         , responseGetInventorySchema $
--             mkGetInventorySchemaResponse
--
--         , responseListComplianceSummaries $
--             mkListComplianceSummariesResponse
--
--         , responseStartAutomationExecution $
--             mkStartAutomationExecutionResponse
--
--         , responseCreateOpsItem $
--             mkCreateOpsItemResponse
--
--         , responseCreateActivation $
--             mkCreateActivationResponse
--
--         , responseDeleteMaintenanceWindow $
--             mkDeleteMaintenanceWindowResponse
--
--         , responseUpdateMaintenanceWindow $
--             mkUpdateMaintenanceWindowResponse
--
--         , responseDescribeSessions $
--             mkDescribeSessionsResponse
--
--         , responseDescribeMaintenanceWindowExecutionTasks $
--             mkDescribeMaintenanceWindowExecutionTasksResponse
--
--         , responseGetDefaultPatchBaseline $
--             mkGetDefaultPatchBaselineResponse
--
--         , responseGetMaintenanceWindowExecutionTask $
--             mkGetMaintenanceWindowExecutionTaskResponse
--
--         , responseCreateDocument $
--             mkCreateDocumentResponse
--
--         , responseRemoveTagsFromResource $
--             mkRemoveTagsFromResourceResponse
--
--         , responseGetCalendarState $
--             mkGetCalendarStateResponse
--
--         , responseDeleteParameters $
--             mkDeleteParametersResponse
--
--         , responseDescribePatchGroupState $
--             mkDescribePatchGroupStateResponse
--
--         , responseListCommandInvocations $
--             mkListCommandInvocationsResponse
--
--         , responseDeregisterTargetFromMaintenanceWindow $
--             mkDeregisterTargetFromMaintenanceWindowResponse
--
--         , responseDescribeEffectivePatchesForPatchBaseline $
--             mkDescribeEffectivePatchesForPatchBaselineResponse
--
--         , responseDescribeMaintenanceWindowTargets $
--             mkDescribeMaintenanceWindowTargetsResponse
--
--         , responseResetServiceSetting $
--             mkResetServiceSettingResponse
--
--         , responseRegisterPatchBaselineForPatchGroup $
--             mkRegisterPatchBaselineForPatchGroupResponse
--
--         , responseListDocuments $
--             mkListDocumentsResponse
--
--         , responseDescribeInstancePatchStates $
--             mkDescribeInstancePatchStatesResponse
--
--         , responseGetOpsSummary $
--             mkGetOpsSummaryResponse
--
--         , responseGetPatchBaselineForPatchGroup $
--             mkGetPatchBaselineForPatchGroupResponse
--
--         , responseUpdateManagedInstanceRole $
--             mkUpdateManagedInstanceRoleResponse
--
--         , responseListComplianceItems $
--             mkListComplianceItemsResponse
--
--         , responseGetDocument $
--             mkGetDocumentResponse
--
--         , responseDescribeMaintenanceWindowSchedule $
--             mkDescribeMaintenanceWindowScheduleResponse
--
--         , responseAddTagsToResource $
--             mkAddTagsToResourceResponse
--
--         , responseCancelCommand $
--             mkCancelCommandResponse
--
--         , responseDescribeAutomationStepExecutions $
--             mkDescribeAutomationStepExecutionsResponse
--
--         , responseGetCommandInvocation $
--             mkGetCommandInvocationResponse
--
--         , responseDescribeInstancePatchStatesForPatchGroup $
--             mkDescribeInstancePatchStatesForPatchGroupResponse
--
--         , responseDeregisterManagedInstance $
--             mkDeregisterManagedInstanceResponse
--
--         , responseDescribeAssociation $
--             mkDescribeAssociationResponse
--
--         , responseDescribeAssociationExecutionTargets $
--             mkDescribeAssociationExecutionTargetsResponse
--
--         , responseModifyDocumentPermission $
--             mkModifyDocumentPermissionResponse
--
--         , responseUpdateResourceDataSync $
--             mkUpdateResourceDataSyncResponse
--
--         , responseDeleteResourceDataSync $
--             mkDeleteResourceDataSyncResponse
--
--         , responseUpdateAssociationStatus $
--             mkUpdateAssociationStatusResponse
--
--         , responseDescribeAvailablePatches $
--             mkDescribeAvailablePatchesResponse
--
--         , responseListDocumentVersions $
--             mkListDocumentVersionsResponse
--
--         , responseDeregisterPatchBaselineForPatchGroup $
--             mkDeregisterPatchBaselineForPatchGroupResponse
--
--         , responseDescribePatchGroups $
--             mkDescribePatchGroupsResponse
--
--         , responseGetMaintenanceWindow $
--             mkGetMaintenanceWindowResponse
--
--         , responseDescribeMaintenanceWindows $
--             mkDescribeMaintenanceWindowsResponse
--
--         , responseRegisterTaskWithMaintenanceWindow $
--             mkRegisterTaskWithMaintenanceWindowResponse
--
--         , responseRegisterDefaultPatchBaseline $
--             mkRegisterDefaultPatchBaselineResponse
--
--         , responseListResourceComplianceSummaries $
--             mkListResourceComplianceSummariesResponse
--
--         , responseListAssociationVersions $
--             mkListAssociationVersionsResponse
--
--         , responseUpdateServiceSetting $
--             mkUpdateServiceSettingResponse
--
--         , responseDescribeMaintenanceWindowTasks $
--             mkDescribeMaintenanceWindowTasksResponse
--
--         , responseDescribeInstanceAssociationsStatus $
--             mkDescribeInstanceAssociationsStatusResponse
--
--         , responseDeregisterTaskFromMaintenanceWindow $
--             mkDeregisterTaskFromMaintenanceWindowResponse
--
--         , responseListInventoryEntries $
--             mkListInventoryEntriesResponse
--
--         , responseLabelParameterVersion $
--             mkLabelParameterVersionResponse
--
--         , responseUpdateMaintenanceWindowTask $
--             mkUpdateMaintenanceWindowTaskResponse
--
--         , responseGetParameterHistory $
--             mkGetParameterHistoryResponse
--
--         , responseDescribeAssociationExecutions $
--             mkDescribeAssociationExecutionsResponse
--
--         , responseGetServiceSetting $
--             mkGetServiceSettingResponse
--
--         , responseStartAssociationsOnce $
--             mkStartAssociationsOnceResponse
--
--         , responseCreateMaintenanceWindow $
--             mkCreateMaintenanceWindowResponse
--
--         , responseStopAutomationExecution $
--             mkStopAutomationExecutionResponse
--
--         , responseGetMaintenanceWindowExecution $
--             mkGetMaintenanceWindowExecutionResponse
--
--         , responseSendAutomationSignal $
--             mkSendAutomationSignalResponse
--
--         , responsePutParameter $
--             mkPutParameterResponse
--
--         , responseDescribeMaintenanceWindowExecutionTaskInvocations $
--             mkDescribeMaintenanceWindowExecutionTaskInvocationsResponse
--
--         , responseGetMaintenanceWindowExecutionTaskInvocation $
--             mkGetMaintenanceWindowExecutionTaskInvocationResponse
--
--         , responseDeleteParameter $
--             mkDeleteParameterResponse
--
--         , responseDescribeInstanceInformation $
--             mkDescribeInstanceInformationResponse
--
--         , responseListAssociations $
--             mkListAssociationsResponse
--
--         , responseUpdateOpsItem $
--             mkUpdateOpsItemResponse
--
--         , responseDeleteAssociation $
--             mkDeleteAssociationResponse
--
--         , responseUpdateAssociation $
--             mkUpdateAssociationResponse
--
--         , responseDescribeInventoryDeletions $
--             mkDescribeInventoryDeletionsResponse
--
--         , responseDeleteInventory $
--             mkDeleteInventoryResponse
--
--         , responsePutInventory $
--             mkPutInventoryResponse
--
--         , responseDescribeEffectiveInstanceAssociations $
--             mkDescribeEffectiveInstanceAssociationsResponse
--
--         , responseDescribeAutomationExecutions $
--             mkDescribeAutomationExecutionsResponse
--
--         , responseGetAutomationExecution $
--             mkGetAutomationExecutionResponse
--
--         , responseSendCommand $
--             mkSendCommandResponse
--
--         , responseDescribePatchBaselines $
--             mkDescribePatchBaselinesResponse
--
--         , responseGetPatchBaseline $
--             mkGetPatchBaselineResponse
--
--         , responseRegisterTargetWithMaintenanceWindow $
--             mkRegisterTargetWithMaintenanceWindowResponse
--
--         , responseStartSession $
--             mkStartSessionResponse
--
--         , responseListCommands $
--             mkListCommandsResponse
--
--         , responseUpdateDocument $
--             mkUpdateDocumentResponse
--
--         , responseDeleteDocument $
--             mkDeleteDocumentResponse
--
--         , responseDescribeDocumentPermission $
--             mkDescribeDocumentPermissionResponse
--
--         , responseCreateAssociationBatch $
--             mkCreateAssociationBatchResponse
--
--         , responseUpdateMaintenanceWindowTarget $
--             mkUpdateMaintenanceWindowTargetResponse
--
--         , responseCreateResourceDataSync $
--             mkCreateResourceDataSyncResponse
--
--         , responseCreatePatchBaseline $
--             mkCreatePatchBaselineResponse
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

-- Responses

responseGetConnectionStatus :: GetConnectionStatusResponse -> TestTree
responseGetConnectionStatus =
  res
    "GetConnectionStatusResponse"
    "fixture/GetConnectionStatusResponse.proto"
    ssmService
    (Proxy :: Proxy GetConnectionStatus)

responseDescribeInstancePatches :: DescribeInstancePatchesResponse -> TestTree
responseDescribeInstancePatches =
  res
    "DescribeInstancePatchesResponse"
    "fixture/DescribeInstancePatchesResponse.proto"
    ssmService
    (Proxy :: Proxy DescribeInstancePatches)

responseGetInventory :: GetInventoryResponse -> TestTree
responseGetInventory =
  res
    "GetInventoryResponse"
    "fixture/GetInventoryResponse.proto"
    ssmService
    (Proxy :: Proxy GetInventory)

responseGetParameters :: GetParametersResponse -> TestTree
responseGetParameters =
  res
    "GetParametersResponse"
    "fixture/GetParametersResponse.proto"
    ssmService
    (Proxy :: Proxy GetParameters)

responseDeletePatchBaseline :: DeletePatchBaselineResponse -> TestTree
responseDeletePatchBaseline =
  res
    "DeletePatchBaselineResponse"
    "fixture/DeletePatchBaselineResponse.proto"
    ssmService
    (Proxy :: Proxy DeletePatchBaseline)

responseUpdatePatchBaseline :: UpdatePatchBaselineResponse -> TestTree
responseUpdatePatchBaseline =
  res
    "UpdatePatchBaselineResponse"
    "fixture/UpdatePatchBaselineResponse.proto"
    ssmService
    (Proxy :: Proxy UpdatePatchBaseline)

responseTerminateSession :: TerminateSessionResponse -> TestTree
responseTerminateSession =
  res
    "TerminateSessionResponse"
    "fixture/TerminateSessionResponse.proto"
    ssmService
    (Proxy :: Proxy TerminateSession)

responseGetParameter :: GetParameterResponse -> TestTree
responseGetParameter =
  res
    "GetParameterResponse"
    "fixture/GetParameterResponse.proto"
    ssmService
    (Proxy :: Proxy GetParameter)

responseUpdateDocumentDefaultVersion :: UpdateDocumentDefaultVersionResponse -> TestTree
responseUpdateDocumentDefaultVersion =
  res
    "UpdateDocumentDefaultVersionResponse"
    "fixture/UpdateDocumentDefaultVersionResponse.proto"
    ssmService
    (Proxy :: Proxy UpdateDocumentDefaultVersion)

responseListResourceDataSync :: ListResourceDataSyncResponse -> TestTree
responseListResourceDataSync =
  res
    "ListResourceDataSyncResponse"
    "fixture/ListResourceDataSyncResponse.proto"
    ssmService
    (Proxy :: Proxy ListResourceDataSync)

responseGetOpsItem :: GetOpsItemResponse -> TestTree
responseGetOpsItem =
  res
    "GetOpsItemResponse"
    "fixture/GetOpsItemResponse.proto"
    ssmService
    (Proxy :: Proxy GetOpsItem)

responseResumeSession :: ResumeSessionResponse -> TestTree
responseResumeSession =
  res
    "ResumeSessionResponse"
    "fixture/ResumeSessionResponse.proto"
    ssmService
    (Proxy :: Proxy ResumeSession)

responseGetDeployablePatchSnapshotForInstance :: GetDeployablePatchSnapshotForInstanceResponse -> TestTree
responseGetDeployablePatchSnapshotForInstance =
  res
    "GetDeployablePatchSnapshotForInstanceResponse"
    "fixture/GetDeployablePatchSnapshotForInstanceResponse.proto"
    ssmService
    (Proxy :: Proxy GetDeployablePatchSnapshotForInstance)

responseDescribeParameters :: DescribeParametersResponse -> TestTree
responseDescribeParameters =
  res
    "DescribeParametersResponse"
    "fixture/DescribeParametersResponse.proto"
    ssmService
    (Proxy :: Proxy DescribeParameters)

responseDescribeOpsItems :: DescribeOpsItemsResponse -> TestTree
responseDescribeOpsItems =
  res
    "DescribeOpsItemsResponse"
    "fixture/DescribeOpsItemsResponse.proto"
    ssmService
    (Proxy :: Proxy DescribeOpsItems)

responseGetParametersByPath :: GetParametersByPathResponse -> TestTree
responseGetParametersByPath =
  res
    "GetParametersByPathResponse"
    "fixture/GetParametersByPathResponse.proto"
    ssmService
    (Proxy :: Proxy GetParametersByPath)

responsePutComplianceItems :: PutComplianceItemsResponse -> TestTree
responsePutComplianceItems =
  res
    "PutComplianceItemsResponse"
    "fixture/PutComplianceItemsResponse.proto"
    ssmService
    (Proxy :: Proxy PutComplianceItems)

responseDescribeActivations :: DescribeActivationsResponse -> TestTree
responseDescribeActivations =
  res
    "DescribeActivationsResponse"
    "fixture/DescribeActivationsResponse.proto"
    ssmService
    (Proxy :: Proxy DescribeActivations)

responseGetMaintenanceWindowTask :: GetMaintenanceWindowTaskResponse -> TestTree
responseGetMaintenanceWindowTask =
  res
    "GetMaintenanceWindowTaskResponse"
    "fixture/GetMaintenanceWindowTaskResponse.proto"
    ssmService
    (Proxy :: Proxy GetMaintenanceWindowTask)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    ssmService
    (Proxy :: Proxy ListTagsForResource)

responseDescribeDocument :: DescribeDocumentResponse -> TestTree
responseDescribeDocument =
  res
    "DescribeDocumentResponse"
    "fixture/DescribeDocumentResponse.proto"
    ssmService
    (Proxy :: Proxy DescribeDocument)

responseDescribePatchProperties :: DescribePatchPropertiesResponse -> TestTree
responseDescribePatchProperties =
  res
    "DescribePatchPropertiesResponse"
    "fixture/DescribePatchPropertiesResponse.proto"
    ssmService
    (Proxy :: Proxy DescribePatchProperties)

responseCreateAssociation :: CreateAssociationResponse -> TestTree
responseCreateAssociation =
  res
    "CreateAssociationResponse"
    "fixture/CreateAssociationResponse.proto"
    ssmService
    (Proxy :: Proxy CreateAssociation)

responseDeleteActivation :: DeleteActivationResponse -> TestTree
responseDeleteActivation =
  res
    "DeleteActivationResponse"
    "fixture/DeleteActivationResponse.proto"
    ssmService
    (Proxy :: Proxy DeleteActivation)

responseDescribeMaintenanceWindowExecutions :: DescribeMaintenanceWindowExecutionsResponse -> TestTree
responseDescribeMaintenanceWindowExecutions =
  res
    "DescribeMaintenanceWindowExecutionsResponse"
    "fixture/DescribeMaintenanceWindowExecutionsResponse.proto"
    ssmService
    (Proxy :: Proxy DescribeMaintenanceWindowExecutions)

responseDescribeMaintenanceWindowsForTarget :: DescribeMaintenanceWindowsForTargetResponse -> TestTree
responseDescribeMaintenanceWindowsForTarget =
  res
    "DescribeMaintenanceWindowsForTargetResponse"
    "fixture/DescribeMaintenanceWindowsForTargetResponse.proto"
    ssmService
    (Proxy :: Proxy DescribeMaintenanceWindowsForTarget)

responseCancelMaintenanceWindowExecution :: CancelMaintenanceWindowExecutionResponse -> TestTree
responseCancelMaintenanceWindowExecution =
  res
    "CancelMaintenanceWindowExecutionResponse"
    "fixture/CancelMaintenanceWindowExecutionResponse.proto"
    ssmService
    (Proxy :: Proxy CancelMaintenanceWindowExecution)

responseGetInventorySchema :: GetInventorySchemaResponse -> TestTree
responseGetInventorySchema =
  res
    "GetInventorySchemaResponse"
    "fixture/GetInventorySchemaResponse.proto"
    ssmService
    (Proxy :: Proxy GetInventorySchema)

responseListComplianceSummaries :: ListComplianceSummariesResponse -> TestTree
responseListComplianceSummaries =
  res
    "ListComplianceSummariesResponse"
    "fixture/ListComplianceSummariesResponse.proto"
    ssmService
    (Proxy :: Proxy ListComplianceSummaries)

responseStartAutomationExecution :: StartAutomationExecutionResponse -> TestTree
responseStartAutomationExecution =
  res
    "StartAutomationExecutionResponse"
    "fixture/StartAutomationExecutionResponse.proto"
    ssmService
    (Proxy :: Proxy StartAutomationExecution)

responseCreateOpsItem :: CreateOpsItemResponse -> TestTree
responseCreateOpsItem =
  res
    "CreateOpsItemResponse"
    "fixture/CreateOpsItemResponse.proto"
    ssmService
    (Proxy :: Proxy CreateOpsItem)

responseCreateActivation :: CreateActivationResponse -> TestTree
responseCreateActivation =
  res
    "CreateActivationResponse"
    "fixture/CreateActivationResponse.proto"
    ssmService
    (Proxy :: Proxy CreateActivation)

responseDeleteMaintenanceWindow :: DeleteMaintenanceWindowResponse -> TestTree
responseDeleteMaintenanceWindow =
  res
    "DeleteMaintenanceWindowResponse"
    "fixture/DeleteMaintenanceWindowResponse.proto"
    ssmService
    (Proxy :: Proxy DeleteMaintenanceWindow)

responseUpdateMaintenanceWindow :: UpdateMaintenanceWindowResponse -> TestTree
responseUpdateMaintenanceWindow =
  res
    "UpdateMaintenanceWindowResponse"
    "fixture/UpdateMaintenanceWindowResponse.proto"
    ssmService
    (Proxy :: Proxy UpdateMaintenanceWindow)

responseDescribeSessions :: DescribeSessionsResponse -> TestTree
responseDescribeSessions =
  res
    "DescribeSessionsResponse"
    "fixture/DescribeSessionsResponse.proto"
    ssmService
    (Proxy :: Proxy DescribeSessions)

responseDescribeMaintenanceWindowExecutionTasks :: DescribeMaintenanceWindowExecutionTasksResponse -> TestTree
responseDescribeMaintenanceWindowExecutionTasks =
  res
    "DescribeMaintenanceWindowExecutionTasksResponse"
    "fixture/DescribeMaintenanceWindowExecutionTasksResponse.proto"
    ssmService
    (Proxy :: Proxy DescribeMaintenanceWindowExecutionTasks)

responseGetDefaultPatchBaseline :: GetDefaultPatchBaselineResponse -> TestTree
responseGetDefaultPatchBaseline =
  res
    "GetDefaultPatchBaselineResponse"
    "fixture/GetDefaultPatchBaselineResponse.proto"
    ssmService
    (Proxy :: Proxy GetDefaultPatchBaseline)

responseGetMaintenanceWindowExecutionTask :: GetMaintenanceWindowExecutionTaskResponse -> TestTree
responseGetMaintenanceWindowExecutionTask =
  res
    "GetMaintenanceWindowExecutionTaskResponse"
    "fixture/GetMaintenanceWindowExecutionTaskResponse.proto"
    ssmService
    (Proxy :: Proxy GetMaintenanceWindowExecutionTask)

responseCreateDocument :: CreateDocumentResponse -> TestTree
responseCreateDocument =
  res
    "CreateDocumentResponse"
    "fixture/CreateDocumentResponse.proto"
    ssmService
    (Proxy :: Proxy CreateDocument)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    ssmService
    (Proxy :: Proxy RemoveTagsFromResource)

responseGetCalendarState :: GetCalendarStateResponse -> TestTree
responseGetCalendarState =
  res
    "GetCalendarStateResponse"
    "fixture/GetCalendarStateResponse.proto"
    ssmService
    (Proxy :: Proxy GetCalendarState)

responseDeleteParameters :: DeleteParametersResponse -> TestTree
responseDeleteParameters =
  res
    "DeleteParametersResponse"
    "fixture/DeleteParametersResponse.proto"
    ssmService
    (Proxy :: Proxy DeleteParameters)

responseDescribePatchGroupState :: DescribePatchGroupStateResponse -> TestTree
responseDescribePatchGroupState =
  res
    "DescribePatchGroupStateResponse"
    "fixture/DescribePatchGroupStateResponse.proto"
    ssmService
    (Proxy :: Proxy DescribePatchGroupState)

responseListCommandInvocations :: ListCommandInvocationsResponse -> TestTree
responseListCommandInvocations =
  res
    "ListCommandInvocationsResponse"
    "fixture/ListCommandInvocationsResponse.proto"
    ssmService
    (Proxy :: Proxy ListCommandInvocations)

responseDeregisterTargetFromMaintenanceWindow :: DeregisterTargetFromMaintenanceWindowResponse -> TestTree
responseDeregisterTargetFromMaintenanceWindow =
  res
    "DeregisterTargetFromMaintenanceWindowResponse"
    "fixture/DeregisterTargetFromMaintenanceWindowResponse.proto"
    ssmService
    (Proxy :: Proxy DeregisterTargetFromMaintenanceWindow)

responseDescribeEffectivePatchesForPatchBaseline :: DescribeEffectivePatchesForPatchBaselineResponse -> TestTree
responseDescribeEffectivePatchesForPatchBaseline =
  res
    "DescribeEffectivePatchesForPatchBaselineResponse"
    "fixture/DescribeEffectivePatchesForPatchBaselineResponse.proto"
    ssmService
    (Proxy :: Proxy DescribeEffectivePatchesForPatchBaseline)

responseDescribeMaintenanceWindowTargets :: DescribeMaintenanceWindowTargetsResponse -> TestTree
responseDescribeMaintenanceWindowTargets =
  res
    "DescribeMaintenanceWindowTargetsResponse"
    "fixture/DescribeMaintenanceWindowTargetsResponse.proto"
    ssmService
    (Proxy :: Proxy DescribeMaintenanceWindowTargets)

responseResetServiceSetting :: ResetServiceSettingResponse -> TestTree
responseResetServiceSetting =
  res
    "ResetServiceSettingResponse"
    "fixture/ResetServiceSettingResponse.proto"
    ssmService
    (Proxy :: Proxy ResetServiceSetting)

responseRegisterPatchBaselineForPatchGroup :: RegisterPatchBaselineForPatchGroupResponse -> TestTree
responseRegisterPatchBaselineForPatchGroup =
  res
    "RegisterPatchBaselineForPatchGroupResponse"
    "fixture/RegisterPatchBaselineForPatchGroupResponse.proto"
    ssmService
    (Proxy :: Proxy RegisterPatchBaselineForPatchGroup)

responseListDocuments :: ListDocumentsResponse -> TestTree
responseListDocuments =
  res
    "ListDocumentsResponse"
    "fixture/ListDocumentsResponse.proto"
    ssmService
    (Proxy :: Proxy ListDocuments)

responseDescribeInstancePatchStates :: DescribeInstancePatchStatesResponse -> TestTree
responseDescribeInstancePatchStates =
  res
    "DescribeInstancePatchStatesResponse"
    "fixture/DescribeInstancePatchStatesResponse.proto"
    ssmService
    (Proxy :: Proxy DescribeInstancePatchStates)

responseGetOpsSummary :: GetOpsSummaryResponse -> TestTree
responseGetOpsSummary =
  res
    "GetOpsSummaryResponse"
    "fixture/GetOpsSummaryResponse.proto"
    ssmService
    (Proxy :: Proxy GetOpsSummary)

responseGetPatchBaselineForPatchGroup :: GetPatchBaselineForPatchGroupResponse -> TestTree
responseGetPatchBaselineForPatchGroup =
  res
    "GetPatchBaselineForPatchGroupResponse"
    "fixture/GetPatchBaselineForPatchGroupResponse.proto"
    ssmService
    (Proxy :: Proxy GetPatchBaselineForPatchGroup)

responseUpdateManagedInstanceRole :: UpdateManagedInstanceRoleResponse -> TestTree
responseUpdateManagedInstanceRole =
  res
    "UpdateManagedInstanceRoleResponse"
    "fixture/UpdateManagedInstanceRoleResponse.proto"
    ssmService
    (Proxy :: Proxy UpdateManagedInstanceRole)

responseListComplianceItems :: ListComplianceItemsResponse -> TestTree
responseListComplianceItems =
  res
    "ListComplianceItemsResponse"
    "fixture/ListComplianceItemsResponse.proto"
    ssmService
    (Proxy :: Proxy ListComplianceItems)

responseGetDocument :: GetDocumentResponse -> TestTree
responseGetDocument =
  res
    "GetDocumentResponse"
    "fixture/GetDocumentResponse.proto"
    ssmService
    (Proxy :: Proxy GetDocument)

responseDescribeMaintenanceWindowSchedule :: DescribeMaintenanceWindowScheduleResponse -> TestTree
responseDescribeMaintenanceWindowSchedule =
  res
    "DescribeMaintenanceWindowScheduleResponse"
    "fixture/DescribeMaintenanceWindowScheduleResponse.proto"
    ssmService
    (Proxy :: Proxy DescribeMaintenanceWindowSchedule)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    ssmService
    (Proxy :: Proxy AddTagsToResource)

responseCancelCommand :: CancelCommandResponse -> TestTree
responseCancelCommand =
  res
    "CancelCommandResponse"
    "fixture/CancelCommandResponse.proto"
    ssmService
    (Proxy :: Proxy CancelCommand)

responseDescribeAutomationStepExecutions :: DescribeAutomationStepExecutionsResponse -> TestTree
responseDescribeAutomationStepExecutions =
  res
    "DescribeAutomationStepExecutionsResponse"
    "fixture/DescribeAutomationStepExecutionsResponse.proto"
    ssmService
    (Proxy :: Proxy DescribeAutomationStepExecutions)

responseGetCommandInvocation :: GetCommandInvocationResponse -> TestTree
responseGetCommandInvocation =
  res
    "GetCommandInvocationResponse"
    "fixture/GetCommandInvocationResponse.proto"
    ssmService
    (Proxy :: Proxy GetCommandInvocation)

responseDescribeInstancePatchStatesForPatchGroup :: DescribeInstancePatchStatesForPatchGroupResponse -> TestTree
responseDescribeInstancePatchStatesForPatchGroup =
  res
    "DescribeInstancePatchStatesForPatchGroupResponse"
    "fixture/DescribeInstancePatchStatesForPatchGroupResponse.proto"
    ssmService
    (Proxy :: Proxy DescribeInstancePatchStatesForPatchGroup)

responseDeregisterManagedInstance :: DeregisterManagedInstanceResponse -> TestTree
responseDeregisterManagedInstance =
  res
    "DeregisterManagedInstanceResponse"
    "fixture/DeregisterManagedInstanceResponse.proto"
    ssmService
    (Proxy :: Proxy DeregisterManagedInstance)

responseDescribeAssociation :: DescribeAssociationResponse -> TestTree
responseDescribeAssociation =
  res
    "DescribeAssociationResponse"
    "fixture/DescribeAssociationResponse.proto"
    ssmService
    (Proxy :: Proxy DescribeAssociation)

responseDescribeAssociationExecutionTargets :: DescribeAssociationExecutionTargetsResponse -> TestTree
responseDescribeAssociationExecutionTargets =
  res
    "DescribeAssociationExecutionTargetsResponse"
    "fixture/DescribeAssociationExecutionTargetsResponse.proto"
    ssmService
    (Proxy :: Proxy DescribeAssociationExecutionTargets)

responseModifyDocumentPermission :: ModifyDocumentPermissionResponse -> TestTree
responseModifyDocumentPermission =
  res
    "ModifyDocumentPermissionResponse"
    "fixture/ModifyDocumentPermissionResponse.proto"
    ssmService
    (Proxy :: Proxy ModifyDocumentPermission)

responseUpdateResourceDataSync :: UpdateResourceDataSyncResponse -> TestTree
responseUpdateResourceDataSync =
  res
    "UpdateResourceDataSyncResponse"
    "fixture/UpdateResourceDataSyncResponse.proto"
    ssmService
    (Proxy :: Proxy UpdateResourceDataSync)

responseDeleteResourceDataSync :: DeleteResourceDataSyncResponse -> TestTree
responseDeleteResourceDataSync =
  res
    "DeleteResourceDataSyncResponse"
    "fixture/DeleteResourceDataSyncResponse.proto"
    ssmService
    (Proxy :: Proxy DeleteResourceDataSync)

responseUpdateAssociationStatus :: UpdateAssociationStatusResponse -> TestTree
responseUpdateAssociationStatus =
  res
    "UpdateAssociationStatusResponse"
    "fixture/UpdateAssociationStatusResponse.proto"
    ssmService
    (Proxy :: Proxy UpdateAssociationStatus)

responseDescribeAvailablePatches :: DescribeAvailablePatchesResponse -> TestTree
responseDescribeAvailablePatches =
  res
    "DescribeAvailablePatchesResponse"
    "fixture/DescribeAvailablePatchesResponse.proto"
    ssmService
    (Proxy :: Proxy DescribeAvailablePatches)

responseListDocumentVersions :: ListDocumentVersionsResponse -> TestTree
responseListDocumentVersions =
  res
    "ListDocumentVersionsResponse"
    "fixture/ListDocumentVersionsResponse.proto"
    ssmService
    (Proxy :: Proxy ListDocumentVersions)

responseDeregisterPatchBaselineForPatchGroup :: DeregisterPatchBaselineForPatchGroupResponse -> TestTree
responseDeregisterPatchBaselineForPatchGroup =
  res
    "DeregisterPatchBaselineForPatchGroupResponse"
    "fixture/DeregisterPatchBaselineForPatchGroupResponse.proto"
    ssmService
    (Proxy :: Proxy DeregisterPatchBaselineForPatchGroup)

responseDescribePatchGroups :: DescribePatchGroupsResponse -> TestTree
responseDescribePatchGroups =
  res
    "DescribePatchGroupsResponse"
    "fixture/DescribePatchGroupsResponse.proto"
    ssmService
    (Proxy :: Proxy DescribePatchGroups)

responseGetMaintenanceWindow :: GetMaintenanceWindowResponse -> TestTree
responseGetMaintenanceWindow =
  res
    "GetMaintenanceWindowResponse"
    "fixture/GetMaintenanceWindowResponse.proto"
    ssmService
    (Proxy :: Proxy GetMaintenanceWindow)

responseDescribeMaintenanceWindows :: DescribeMaintenanceWindowsResponse -> TestTree
responseDescribeMaintenanceWindows =
  res
    "DescribeMaintenanceWindowsResponse"
    "fixture/DescribeMaintenanceWindowsResponse.proto"
    ssmService
    (Proxy :: Proxy DescribeMaintenanceWindows)

responseRegisterTaskWithMaintenanceWindow :: RegisterTaskWithMaintenanceWindowResponse -> TestTree
responseRegisterTaskWithMaintenanceWindow =
  res
    "RegisterTaskWithMaintenanceWindowResponse"
    "fixture/RegisterTaskWithMaintenanceWindowResponse.proto"
    ssmService
    (Proxy :: Proxy RegisterTaskWithMaintenanceWindow)

responseRegisterDefaultPatchBaseline :: RegisterDefaultPatchBaselineResponse -> TestTree
responseRegisterDefaultPatchBaseline =
  res
    "RegisterDefaultPatchBaselineResponse"
    "fixture/RegisterDefaultPatchBaselineResponse.proto"
    ssmService
    (Proxy :: Proxy RegisterDefaultPatchBaseline)

responseListResourceComplianceSummaries :: ListResourceComplianceSummariesResponse -> TestTree
responseListResourceComplianceSummaries =
  res
    "ListResourceComplianceSummariesResponse"
    "fixture/ListResourceComplianceSummariesResponse.proto"
    ssmService
    (Proxy :: Proxy ListResourceComplianceSummaries)

responseListAssociationVersions :: ListAssociationVersionsResponse -> TestTree
responseListAssociationVersions =
  res
    "ListAssociationVersionsResponse"
    "fixture/ListAssociationVersionsResponse.proto"
    ssmService
    (Proxy :: Proxy ListAssociationVersions)

responseUpdateServiceSetting :: UpdateServiceSettingResponse -> TestTree
responseUpdateServiceSetting =
  res
    "UpdateServiceSettingResponse"
    "fixture/UpdateServiceSettingResponse.proto"
    ssmService
    (Proxy :: Proxy UpdateServiceSetting)

responseDescribeMaintenanceWindowTasks :: DescribeMaintenanceWindowTasksResponse -> TestTree
responseDescribeMaintenanceWindowTasks =
  res
    "DescribeMaintenanceWindowTasksResponse"
    "fixture/DescribeMaintenanceWindowTasksResponse.proto"
    ssmService
    (Proxy :: Proxy DescribeMaintenanceWindowTasks)

responseDescribeInstanceAssociationsStatus :: DescribeInstanceAssociationsStatusResponse -> TestTree
responseDescribeInstanceAssociationsStatus =
  res
    "DescribeInstanceAssociationsStatusResponse"
    "fixture/DescribeInstanceAssociationsStatusResponse.proto"
    ssmService
    (Proxy :: Proxy DescribeInstanceAssociationsStatus)

responseDeregisterTaskFromMaintenanceWindow :: DeregisterTaskFromMaintenanceWindowResponse -> TestTree
responseDeregisterTaskFromMaintenanceWindow =
  res
    "DeregisterTaskFromMaintenanceWindowResponse"
    "fixture/DeregisterTaskFromMaintenanceWindowResponse.proto"
    ssmService
    (Proxy :: Proxy DeregisterTaskFromMaintenanceWindow)

responseListInventoryEntries :: ListInventoryEntriesResponse -> TestTree
responseListInventoryEntries =
  res
    "ListInventoryEntriesResponse"
    "fixture/ListInventoryEntriesResponse.proto"
    ssmService
    (Proxy :: Proxy ListInventoryEntries)

responseLabelParameterVersion :: LabelParameterVersionResponse -> TestTree
responseLabelParameterVersion =
  res
    "LabelParameterVersionResponse"
    "fixture/LabelParameterVersionResponse.proto"
    ssmService
    (Proxy :: Proxy LabelParameterVersion)

responseUpdateMaintenanceWindowTask :: UpdateMaintenanceWindowTaskResponse -> TestTree
responseUpdateMaintenanceWindowTask =
  res
    "UpdateMaintenanceWindowTaskResponse"
    "fixture/UpdateMaintenanceWindowTaskResponse.proto"
    ssmService
    (Proxy :: Proxy UpdateMaintenanceWindowTask)

responseGetParameterHistory :: GetParameterHistoryResponse -> TestTree
responseGetParameterHistory =
  res
    "GetParameterHistoryResponse"
    "fixture/GetParameterHistoryResponse.proto"
    ssmService
    (Proxy :: Proxy GetParameterHistory)

responseDescribeAssociationExecutions :: DescribeAssociationExecutionsResponse -> TestTree
responseDescribeAssociationExecutions =
  res
    "DescribeAssociationExecutionsResponse"
    "fixture/DescribeAssociationExecutionsResponse.proto"
    ssmService
    (Proxy :: Proxy DescribeAssociationExecutions)

responseGetServiceSetting :: GetServiceSettingResponse -> TestTree
responseGetServiceSetting =
  res
    "GetServiceSettingResponse"
    "fixture/GetServiceSettingResponse.proto"
    ssmService
    (Proxy :: Proxy GetServiceSetting)

responseStartAssociationsOnce :: StartAssociationsOnceResponse -> TestTree
responseStartAssociationsOnce =
  res
    "StartAssociationsOnceResponse"
    "fixture/StartAssociationsOnceResponse.proto"
    ssmService
    (Proxy :: Proxy StartAssociationsOnce)

responseCreateMaintenanceWindow :: CreateMaintenanceWindowResponse -> TestTree
responseCreateMaintenanceWindow =
  res
    "CreateMaintenanceWindowResponse"
    "fixture/CreateMaintenanceWindowResponse.proto"
    ssmService
    (Proxy :: Proxy CreateMaintenanceWindow)

responseStopAutomationExecution :: StopAutomationExecutionResponse -> TestTree
responseStopAutomationExecution =
  res
    "StopAutomationExecutionResponse"
    "fixture/StopAutomationExecutionResponse.proto"
    ssmService
    (Proxy :: Proxy StopAutomationExecution)

responseGetMaintenanceWindowExecution :: GetMaintenanceWindowExecutionResponse -> TestTree
responseGetMaintenanceWindowExecution =
  res
    "GetMaintenanceWindowExecutionResponse"
    "fixture/GetMaintenanceWindowExecutionResponse.proto"
    ssmService
    (Proxy :: Proxy GetMaintenanceWindowExecution)

responseSendAutomationSignal :: SendAutomationSignalResponse -> TestTree
responseSendAutomationSignal =
  res
    "SendAutomationSignalResponse"
    "fixture/SendAutomationSignalResponse.proto"
    ssmService
    (Proxy :: Proxy SendAutomationSignal)

responsePutParameter :: PutParameterResponse -> TestTree
responsePutParameter =
  res
    "PutParameterResponse"
    "fixture/PutParameterResponse.proto"
    ssmService
    (Proxy :: Proxy PutParameter)

responseDescribeMaintenanceWindowExecutionTaskInvocations :: DescribeMaintenanceWindowExecutionTaskInvocationsResponse -> TestTree
responseDescribeMaintenanceWindowExecutionTaskInvocations =
  res
    "DescribeMaintenanceWindowExecutionTaskInvocationsResponse"
    "fixture/DescribeMaintenanceWindowExecutionTaskInvocationsResponse.proto"
    ssmService
    (Proxy :: Proxy DescribeMaintenanceWindowExecutionTaskInvocations)

responseGetMaintenanceWindowExecutionTaskInvocation :: GetMaintenanceWindowExecutionTaskInvocationResponse -> TestTree
responseGetMaintenanceWindowExecutionTaskInvocation =
  res
    "GetMaintenanceWindowExecutionTaskInvocationResponse"
    "fixture/GetMaintenanceWindowExecutionTaskInvocationResponse.proto"
    ssmService
    (Proxy :: Proxy GetMaintenanceWindowExecutionTaskInvocation)

responseDeleteParameter :: DeleteParameterResponse -> TestTree
responseDeleteParameter =
  res
    "DeleteParameterResponse"
    "fixture/DeleteParameterResponse.proto"
    ssmService
    (Proxy :: Proxy DeleteParameter)

responseDescribeInstanceInformation :: DescribeInstanceInformationResponse -> TestTree
responseDescribeInstanceInformation =
  res
    "DescribeInstanceInformationResponse"
    "fixture/DescribeInstanceInformationResponse.proto"
    ssmService
    (Proxy :: Proxy DescribeInstanceInformation)

responseListAssociations :: ListAssociationsResponse -> TestTree
responseListAssociations =
  res
    "ListAssociationsResponse"
    "fixture/ListAssociationsResponse.proto"
    ssmService
    (Proxy :: Proxy ListAssociations)

responseUpdateOpsItem :: UpdateOpsItemResponse -> TestTree
responseUpdateOpsItem =
  res
    "UpdateOpsItemResponse"
    "fixture/UpdateOpsItemResponse.proto"
    ssmService
    (Proxy :: Proxy UpdateOpsItem)

responseDeleteAssociation :: DeleteAssociationResponse -> TestTree
responseDeleteAssociation =
  res
    "DeleteAssociationResponse"
    "fixture/DeleteAssociationResponse.proto"
    ssmService
    (Proxy :: Proxy DeleteAssociation)

responseUpdateAssociation :: UpdateAssociationResponse -> TestTree
responseUpdateAssociation =
  res
    "UpdateAssociationResponse"
    "fixture/UpdateAssociationResponse.proto"
    ssmService
    (Proxy :: Proxy UpdateAssociation)

responseDescribeInventoryDeletions :: DescribeInventoryDeletionsResponse -> TestTree
responseDescribeInventoryDeletions =
  res
    "DescribeInventoryDeletionsResponse"
    "fixture/DescribeInventoryDeletionsResponse.proto"
    ssmService
    (Proxy :: Proxy DescribeInventoryDeletions)

responseDeleteInventory :: DeleteInventoryResponse -> TestTree
responseDeleteInventory =
  res
    "DeleteInventoryResponse"
    "fixture/DeleteInventoryResponse.proto"
    ssmService
    (Proxy :: Proxy DeleteInventory)

responsePutInventory :: PutInventoryResponse -> TestTree
responsePutInventory =
  res
    "PutInventoryResponse"
    "fixture/PutInventoryResponse.proto"
    ssmService
    (Proxy :: Proxy PutInventory)

responseDescribeEffectiveInstanceAssociations :: DescribeEffectiveInstanceAssociationsResponse -> TestTree
responseDescribeEffectiveInstanceAssociations =
  res
    "DescribeEffectiveInstanceAssociationsResponse"
    "fixture/DescribeEffectiveInstanceAssociationsResponse.proto"
    ssmService
    (Proxy :: Proxy DescribeEffectiveInstanceAssociations)

responseDescribeAutomationExecutions :: DescribeAutomationExecutionsResponse -> TestTree
responseDescribeAutomationExecutions =
  res
    "DescribeAutomationExecutionsResponse"
    "fixture/DescribeAutomationExecutionsResponse.proto"
    ssmService
    (Proxy :: Proxy DescribeAutomationExecutions)

responseGetAutomationExecution :: GetAutomationExecutionResponse -> TestTree
responseGetAutomationExecution =
  res
    "GetAutomationExecutionResponse"
    "fixture/GetAutomationExecutionResponse.proto"
    ssmService
    (Proxy :: Proxy GetAutomationExecution)

responseSendCommand :: SendCommandResponse -> TestTree
responseSendCommand =
  res
    "SendCommandResponse"
    "fixture/SendCommandResponse.proto"
    ssmService
    (Proxy :: Proxy SendCommand)

responseDescribePatchBaselines :: DescribePatchBaselinesResponse -> TestTree
responseDescribePatchBaselines =
  res
    "DescribePatchBaselinesResponse"
    "fixture/DescribePatchBaselinesResponse.proto"
    ssmService
    (Proxy :: Proxy DescribePatchBaselines)

responseGetPatchBaseline :: GetPatchBaselineResponse -> TestTree
responseGetPatchBaseline =
  res
    "GetPatchBaselineResponse"
    "fixture/GetPatchBaselineResponse.proto"
    ssmService
    (Proxy :: Proxy GetPatchBaseline)

responseRegisterTargetWithMaintenanceWindow :: RegisterTargetWithMaintenanceWindowResponse -> TestTree
responseRegisterTargetWithMaintenanceWindow =
  res
    "RegisterTargetWithMaintenanceWindowResponse"
    "fixture/RegisterTargetWithMaintenanceWindowResponse.proto"
    ssmService
    (Proxy :: Proxy RegisterTargetWithMaintenanceWindow)

responseStartSession :: StartSessionResponse -> TestTree
responseStartSession =
  res
    "StartSessionResponse"
    "fixture/StartSessionResponse.proto"
    ssmService
    (Proxy :: Proxy StartSession)

responseListCommands :: ListCommandsResponse -> TestTree
responseListCommands =
  res
    "ListCommandsResponse"
    "fixture/ListCommandsResponse.proto"
    ssmService
    (Proxy :: Proxy ListCommands)

responseUpdateDocument :: UpdateDocumentResponse -> TestTree
responseUpdateDocument =
  res
    "UpdateDocumentResponse"
    "fixture/UpdateDocumentResponse.proto"
    ssmService
    (Proxy :: Proxy UpdateDocument)

responseDeleteDocument :: DeleteDocumentResponse -> TestTree
responseDeleteDocument =
  res
    "DeleteDocumentResponse"
    "fixture/DeleteDocumentResponse.proto"
    ssmService
    (Proxy :: Proxy DeleteDocument)

responseDescribeDocumentPermission :: DescribeDocumentPermissionResponse -> TestTree
responseDescribeDocumentPermission =
  res
    "DescribeDocumentPermissionResponse"
    "fixture/DescribeDocumentPermissionResponse.proto"
    ssmService
    (Proxy :: Proxy DescribeDocumentPermission)

responseCreateAssociationBatch :: CreateAssociationBatchResponse -> TestTree
responseCreateAssociationBatch =
  res
    "CreateAssociationBatchResponse"
    "fixture/CreateAssociationBatchResponse.proto"
    ssmService
    (Proxy :: Proxy CreateAssociationBatch)

responseUpdateMaintenanceWindowTarget :: UpdateMaintenanceWindowTargetResponse -> TestTree
responseUpdateMaintenanceWindowTarget =
  res
    "UpdateMaintenanceWindowTargetResponse"
    "fixture/UpdateMaintenanceWindowTargetResponse.proto"
    ssmService
    (Proxy :: Proxy UpdateMaintenanceWindowTarget)

responseCreateResourceDataSync :: CreateResourceDataSyncResponse -> TestTree
responseCreateResourceDataSync =
  res
    "CreateResourceDataSyncResponse"
    "fixture/CreateResourceDataSyncResponse.proto"
    ssmService
    (Proxy :: Proxy CreateResourceDataSync)

responseCreatePatchBaseline :: CreatePatchBaselineResponse -> TestTree
responseCreatePatchBaseline =
  res
    "CreatePatchBaselineResponse"
    "fixture/CreatePatchBaselineResponse.proto"
    ssmService
    (Proxy :: Proxy CreatePatchBaseline)
