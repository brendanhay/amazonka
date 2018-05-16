{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SSM
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--         [ requestDescribeInstancePatches $
--             describeInstancePatches
--
--         , requestGetInventory $
--             getInventory
--
--         , requestGetParameters $
--             getParameters
--
--         , requestDeletePatchBaseline $
--             deletePatchBaseline
--
--         , requestUpdatePatchBaseline $
--             updatePatchBaseline
--
--         , requestGetParameter $
--             getParameter
--
--         , requestUpdateDocumentDefaultVersion $
--             updateDocumentDefaultVersion
--
--         , requestListResourceDataSync $
--             listResourceDataSync
--
--         , requestGetDeployablePatchSnapshotForInstance $
--             getDeployablePatchSnapshotForInstance
--
--         , requestDescribeParameters $
--             describeParameters
--
--         , requestGetParametersByPath $
--             getParametersByPath
--
--         , requestPutComplianceItems $
--             putComplianceItems
--
--         , requestDescribeActivations $
--             describeActivations
--
--         , requestGetMaintenanceWindowTask $
--             getMaintenanceWindowTask
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestDescribeDocument $
--             describeDocument
--
--         , requestCreateAssociation $
--             createAssociation
--
--         , requestDeleteActivation $
--             deleteActivation
--
--         , requestDescribeMaintenanceWindowExecutions $
--             describeMaintenanceWindowExecutions
--
--         , requestGetInventorySchema $
--             getInventorySchema
--
--         , requestListComplianceSummaries $
--             listComplianceSummaries
--
--         , requestStartAutomationExecution $
--             startAutomationExecution
--
--         , requestCreateActivation $
--             createActivation
--
--         , requestDeleteMaintenanceWindow $
--             deleteMaintenanceWindow
--
--         , requestUpdateMaintenanceWindow $
--             updateMaintenanceWindow
--
--         , requestDescribeMaintenanceWindowExecutionTasks $
--             describeMaintenanceWindowExecutionTasks
--
--         , requestGetDefaultPatchBaseline $
--             getDefaultPatchBaseline
--
--         , requestGetMaintenanceWindowExecutionTask $
--             getMaintenanceWindowExecutionTask
--
--         , requestCreateDocument $
--             createDocument
--
--         , requestRemoveTagsFromResource $
--             removeTagsFromResource
--
--         , requestDeleteParameters $
--             deleteParameters
--
--         , requestDescribePatchGroupState $
--             describePatchGroupState
--
--         , requestListCommandInvocations $
--             listCommandInvocations
--
--         , requestDeregisterTargetFromMaintenanceWindow $
--             deregisterTargetFromMaintenanceWindow
--
--         , requestDescribeEffectivePatchesForPatchBaseline $
--             describeEffectivePatchesForPatchBaseline
--
--         , requestDescribeMaintenanceWindowTargets $
--             describeMaintenanceWindowTargets
--
--         , requestRegisterPatchBaselineForPatchGroup $
--             registerPatchBaselineForPatchGroup
--
--         , requestListDocuments $
--             listDocuments
--
--         , requestDescribeInstancePatchStates $
--             describeInstancePatchStates
--
--         , requestGetPatchBaselineForPatchGroup $
--             getPatchBaselineForPatchGroup
--
--         , requestUpdateManagedInstanceRole $
--             updateManagedInstanceRole
--
--         , requestListComplianceItems $
--             listComplianceItems
--
--         , requestGetDocument $
--             getDocument
--
--         , requestAddTagsToResource $
--             addTagsToResource
--
--         , requestCancelCommand $
--             cancelCommand
--
--         , requestDescribeAutomationStepExecutions $
--             describeAutomationStepExecutions
--
--         , requestGetCommandInvocation $
--             getCommandInvocation
--
--         , requestDescribeInstancePatchStatesForPatchGroup $
--             describeInstancePatchStatesForPatchGroup
--
--         , requestDeregisterManagedInstance $
--             deregisterManagedInstance
--
--         , requestDescribeAssociation $
--             describeAssociation
--
--         , requestModifyDocumentPermission $
--             modifyDocumentPermission
--
--         , requestDeleteResourceDataSync $
--             deleteResourceDataSync
--
--         , requestUpdateAssociationStatus $
--             updateAssociationStatus
--
--         , requestDescribeAvailablePatches $
--             describeAvailablePatches
--
--         , requestListDocumentVersions $
--             listDocumentVersions
--
--         , requestDeregisterPatchBaselineForPatchGroup $
--             deregisterPatchBaselineForPatchGroup
--
--         , requestDescribePatchGroups $
--             describePatchGroups
--
--         , requestGetMaintenanceWindow $
--             getMaintenanceWindow
--
--         , requestDescribeMaintenanceWindows $
--             describeMaintenanceWindows
--
--         , requestRegisterTaskWithMaintenanceWindow $
--             registerTaskWithMaintenanceWindow
--
--         , requestRegisterDefaultPatchBaseline $
--             registerDefaultPatchBaseline
--
--         , requestListResourceComplianceSummaries $
--             listResourceComplianceSummaries
--
--         , requestListAssociationVersions $
--             listAssociationVersions
--
--         , requestDescribeMaintenanceWindowTasks $
--             describeMaintenanceWindowTasks
--
--         , requestDescribeInstanceAssociationsStatus $
--             describeInstanceAssociationsStatus
--
--         , requestDeregisterTaskFromMaintenanceWindow $
--             deregisterTaskFromMaintenanceWindow
--
--         , requestListInventoryEntries $
--             listInventoryEntries
--
--         , requestUpdateMaintenanceWindowTask $
--             updateMaintenanceWindowTask
--
--         , requestGetParameterHistory $
--             getParameterHistory
--
--         , requestCreateMaintenanceWindow $
--             createMaintenanceWindow
--
--         , requestStopAutomationExecution $
--             stopAutomationExecution
--
--         , requestGetMaintenanceWindowExecution $
--             getMaintenanceWindowExecution
--
--         , requestSendAutomationSignal $
--             sendAutomationSignal
--
--         , requestPutParameter $
--             putParameter
--
--         , requestDescribeMaintenanceWindowExecutionTaskInvocations $
--             describeMaintenanceWindowExecutionTaskInvocations
--
--         , requestGetMaintenanceWindowExecutionTaskInvocation $
--             getMaintenanceWindowExecutionTaskInvocation
--
--         , requestDeleteParameter $
--             deleteParameter
--
--         , requestDescribeInstanceInformation $
--             describeInstanceInformation
--
--         , requestListAssociations $
--             listAssociations
--
--         , requestDeleteAssociation $
--             deleteAssociation
--
--         , requestUpdateAssociation $
--             updateAssociation
--
--         , requestDescribeInventoryDeletions $
--             describeInventoryDeletions
--
--         , requestDeleteInventory $
--             deleteInventory
--
--         , requestPutInventory $
--             putInventory
--
--         , requestDescribeEffectiveInstanceAssociations $
--             describeEffectiveInstanceAssociations
--
--         , requestDescribeAutomationExecutions $
--             describeAutomationExecutions
--
--         , requestGetAutomationExecution $
--             getAutomationExecution
--
--         , requestSendCommand $
--             sendCommand
--
--         , requestDescribePatchBaselines $
--             describePatchBaselines
--
--         , requestGetPatchBaseline $
--             getPatchBaseline
--
--         , requestRegisterTargetWithMaintenanceWindow $
--             registerTargetWithMaintenanceWindow
--
--         , requestListCommands $
--             listCommands
--
--         , requestUpdateDocument $
--             updateDocument
--
--         , requestDeleteDocument $
--             deleteDocument
--
--         , requestDescribeDocumentPermission $
--             describeDocumentPermission
--
--         , requestCreateAssociationBatch $
--             createAssociationBatch
--
--         , requestUpdateMaintenanceWindowTarget $
--             updateMaintenanceWindowTarget
--
--         , requestCreateResourceDataSync $
--             createResourceDataSync
--
--         , requestCreatePatchBaseline $
--             createPatchBaseline
--
--           ]

--     , testGroup "response"
--         [ responseDescribeInstancePatches $
--             describeInstancePatchesResponse
--
--         , responseGetInventory $
--             getInventoryResponse
--
--         , responseGetParameters $
--             getParametersResponse
--
--         , responseDeletePatchBaseline $
--             deletePatchBaselineResponse
--
--         , responseUpdatePatchBaseline $
--             updatePatchBaselineResponse
--
--         , responseGetParameter $
--             getParameterResponse
--
--         , responseUpdateDocumentDefaultVersion $
--             updateDocumentDefaultVersionResponse
--
--         , responseListResourceDataSync $
--             listResourceDataSyncResponse
--
--         , responseGetDeployablePatchSnapshotForInstance $
--             getDeployablePatchSnapshotForInstanceResponse
--
--         , responseDescribeParameters $
--             describeParametersResponse
--
--         , responseGetParametersByPath $
--             getParametersByPathResponse
--
--         , responsePutComplianceItems $
--             putComplianceItemsResponse
--
--         , responseDescribeActivations $
--             describeActivationsResponse
--
--         , responseGetMaintenanceWindowTask $
--             getMaintenanceWindowTaskResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseDescribeDocument $
--             describeDocumentResponse
--
--         , responseCreateAssociation $
--             createAssociationResponse
--
--         , responseDeleteActivation $
--             deleteActivationResponse
--
--         , responseDescribeMaintenanceWindowExecutions $
--             describeMaintenanceWindowExecutionsResponse
--
--         , responseGetInventorySchema $
--             getInventorySchemaResponse
--
--         , responseListComplianceSummaries $
--             listComplianceSummariesResponse
--
--         , responseStartAutomationExecution $
--             startAutomationExecutionResponse
--
--         , responseCreateActivation $
--             createActivationResponse
--
--         , responseDeleteMaintenanceWindow $
--             deleteMaintenanceWindowResponse
--
--         , responseUpdateMaintenanceWindow $
--             updateMaintenanceWindowResponse
--
--         , responseDescribeMaintenanceWindowExecutionTasks $
--             describeMaintenanceWindowExecutionTasksResponse
--
--         , responseGetDefaultPatchBaseline $
--             getDefaultPatchBaselineResponse
--
--         , responseGetMaintenanceWindowExecutionTask $
--             getMaintenanceWindowExecutionTaskResponse
--
--         , responseCreateDocument $
--             createDocumentResponse
--
--         , responseRemoveTagsFromResource $
--             removeTagsFromResourceResponse
--
--         , responseDeleteParameters $
--             deleteParametersResponse
--
--         , responseDescribePatchGroupState $
--             describePatchGroupStateResponse
--
--         , responseListCommandInvocations $
--             listCommandInvocationsResponse
--
--         , responseDeregisterTargetFromMaintenanceWindow $
--             deregisterTargetFromMaintenanceWindowResponse
--
--         , responseDescribeEffectivePatchesForPatchBaseline $
--             describeEffectivePatchesForPatchBaselineResponse
--
--         , responseDescribeMaintenanceWindowTargets $
--             describeMaintenanceWindowTargetsResponse
--
--         , responseRegisterPatchBaselineForPatchGroup $
--             registerPatchBaselineForPatchGroupResponse
--
--         , responseListDocuments $
--             listDocumentsResponse
--
--         , responseDescribeInstancePatchStates $
--             describeInstancePatchStatesResponse
--
--         , responseGetPatchBaselineForPatchGroup $
--             getPatchBaselineForPatchGroupResponse
--
--         , responseUpdateManagedInstanceRole $
--             updateManagedInstanceRoleResponse
--
--         , responseListComplianceItems $
--             listComplianceItemsResponse
--
--         , responseGetDocument $
--             getDocumentResponse
--
--         , responseAddTagsToResource $
--             addTagsToResourceResponse
--
--         , responseCancelCommand $
--             cancelCommandResponse
--
--         , responseDescribeAutomationStepExecutions $
--             describeAutomationStepExecutionsResponse
--
--         , responseGetCommandInvocation $
--             getCommandInvocationResponse
--
--         , responseDescribeInstancePatchStatesForPatchGroup $
--             describeInstancePatchStatesForPatchGroupResponse
--
--         , responseDeregisterManagedInstance $
--             deregisterManagedInstanceResponse
--
--         , responseDescribeAssociation $
--             describeAssociationResponse
--
--         , responseModifyDocumentPermission $
--             modifyDocumentPermissionResponse
--
--         , responseDeleteResourceDataSync $
--             deleteResourceDataSyncResponse
--
--         , responseUpdateAssociationStatus $
--             updateAssociationStatusResponse
--
--         , responseDescribeAvailablePatches $
--             describeAvailablePatchesResponse
--
--         , responseListDocumentVersions $
--             listDocumentVersionsResponse
--
--         , responseDeregisterPatchBaselineForPatchGroup $
--             deregisterPatchBaselineForPatchGroupResponse
--
--         , responseDescribePatchGroups $
--             describePatchGroupsResponse
--
--         , responseGetMaintenanceWindow $
--             getMaintenanceWindowResponse
--
--         , responseDescribeMaintenanceWindows $
--             describeMaintenanceWindowsResponse
--
--         , responseRegisterTaskWithMaintenanceWindow $
--             registerTaskWithMaintenanceWindowResponse
--
--         , responseRegisterDefaultPatchBaseline $
--             registerDefaultPatchBaselineResponse
--
--         , responseListResourceComplianceSummaries $
--             listResourceComplianceSummariesResponse
--
--         , responseListAssociationVersions $
--             listAssociationVersionsResponse
--
--         , responseDescribeMaintenanceWindowTasks $
--             describeMaintenanceWindowTasksResponse
--
--         , responseDescribeInstanceAssociationsStatus $
--             describeInstanceAssociationsStatusResponse
--
--         , responseDeregisterTaskFromMaintenanceWindow $
--             deregisterTaskFromMaintenanceWindowResponse
--
--         , responseListInventoryEntries $
--             listInventoryEntriesResponse
--
--         , responseUpdateMaintenanceWindowTask $
--             updateMaintenanceWindowTaskResponse
--
--         , responseGetParameterHistory $
--             getParameterHistoryResponse
--
--         , responseCreateMaintenanceWindow $
--             createMaintenanceWindowResponse
--
--         , responseStopAutomationExecution $
--             stopAutomationExecutionResponse
--
--         , responseGetMaintenanceWindowExecution $
--             getMaintenanceWindowExecutionResponse
--
--         , responseSendAutomationSignal $
--             sendAutomationSignalResponse
--
--         , responsePutParameter $
--             putParameterResponse
--
--         , responseDescribeMaintenanceWindowExecutionTaskInvocations $
--             describeMaintenanceWindowExecutionTaskInvocationsResponse
--
--         , responseGetMaintenanceWindowExecutionTaskInvocation $
--             getMaintenanceWindowExecutionTaskInvocationResponse
--
--         , responseDeleteParameter $
--             deleteParameterResponse
--
--         , responseDescribeInstanceInformation $
--             describeInstanceInformationResponse
--
--         , responseListAssociations $
--             listAssociationsResponse
--
--         , responseDeleteAssociation $
--             deleteAssociationResponse
--
--         , responseUpdateAssociation $
--             updateAssociationResponse
--
--         , responseDescribeInventoryDeletions $
--             describeInventoryDeletionsResponse
--
--         , responseDeleteInventory $
--             deleteInventoryResponse
--
--         , responsePutInventory $
--             putInventoryResponse
--
--         , responseDescribeEffectiveInstanceAssociations $
--             describeEffectiveInstanceAssociationsResponse
--
--         , responseDescribeAutomationExecutions $
--             describeAutomationExecutionsResponse
--
--         , responseGetAutomationExecution $
--             getAutomationExecutionResponse
--
--         , responseSendCommand $
--             sendCommandResponse
--
--         , responseDescribePatchBaselines $
--             describePatchBaselinesResponse
--
--         , responseGetPatchBaseline $
--             getPatchBaselineResponse
--
--         , responseRegisterTargetWithMaintenanceWindow $
--             registerTargetWithMaintenanceWindowResponse
--
--         , responseListCommands $
--             listCommandsResponse
--
--         , responseUpdateDocument $
--             updateDocumentResponse
--
--         , responseDeleteDocument $
--             deleteDocumentResponse
--
--         , responseDescribeDocumentPermission $
--             describeDocumentPermissionResponse
--
--         , responseCreateAssociationBatch $
--             createAssociationBatchResponse
--
--         , responseUpdateMaintenanceWindowTarget $
--             updateMaintenanceWindowTargetResponse
--
--         , responseCreateResourceDataSync $
--             createResourceDataSyncResponse
--
--         , responseCreatePatchBaseline $
--             createPatchBaselineResponse
--
--           ]
--     ]

-- Requests

requestDescribeInstancePatches :: DescribeInstancePatches -> TestTree
requestDescribeInstancePatches = req
    "DescribeInstancePatches"
    "fixture/DescribeInstancePatches.yaml"

requestGetInventory :: GetInventory -> TestTree
requestGetInventory = req
    "GetInventory"
    "fixture/GetInventory.yaml"

requestGetParameters :: GetParameters -> TestTree
requestGetParameters = req
    "GetParameters"
    "fixture/GetParameters.yaml"

requestDeletePatchBaseline :: DeletePatchBaseline -> TestTree
requestDeletePatchBaseline = req
    "DeletePatchBaseline"
    "fixture/DeletePatchBaseline.yaml"

requestUpdatePatchBaseline :: UpdatePatchBaseline -> TestTree
requestUpdatePatchBaseline = req
    "UpdatePatchBaseline"
    "fixture/UpdatePatchBaseline.yaml"

requestGetParameter :: GetParameter -> TestTree
requestGetParameter = req
    "GetParameter"
    "fixture/GetParameter.yaml"

requestUpdateDocumentDefaultVersion :: UpdateDocumentDefaultVersion -> TestTree
requestUpdateDocumentDefaultVersion = req
    "UpdateDocumentDefaultVersion"
    "fixture/UpdateDocumentDefaultVersion.yaml"

requestListResourceDataSync :: ListResourceDataSync -> TestTree
requestListResourceDataSync = req
    "ListResourceDataSync"
    "fixture/ListResourceDataSync.yaml"

requestGetDeployablePatchSnapshotForInstance :: GetDeployablePatchSnapshotForInstance -> TestTree
requestGetDeployablePatchSnapshotForInstance = req
    "GetDeployablePatchSnapshotForInstance"
    "fixture/GetDeployablePatchSnapshotForInstance.yaml"

requestDescribeParameters :: DescribeParameters -> TestTree
requestDescribeParameters = req
    "DescribeParameters"
    "fixture/DescribeParameters.yaml"

requestGetParametersByPath :: GetParametersByPath -> TestTree
requestGetParametersByPath = req
    "GetParametersByPath"
    "fixture/GetParametersByPath.yaml"

requestPutComplianceItems :: PutComplianceItems -> TestTree
requestPutComplianceItems = req
    "PutComplianceItems"
    "fixture/PutComplianceItems.yaml"

requestDescribeActivations :: DescribeActivations -> TestTree
requestDescribeActivations = req
    "DescribeActivations"
    "fixture/DescribeActivations.yaml"

requestGetMaintenanceWindowTask :: GetMaintenanceWindowTask -> TestTree
requestGetMaintenanceWindowTask = req
    "GetMaintenanceWindowTask"
    "fixture/GetMaintenanceWindowTask.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDescribeDocument :: DescribeDocument -> TestTree
requestDescribeDocument = req
    "DescribeDocument"
    "fixture/DescribeDocument.yaml"

requestCreateAssociation :: CreateAssociation -> TestTree
requestCreateAssociation = req
    "CreateAssociation"
    "fixture/CreateAssociation.yaml"

requestDeleteActivation :: DeleteActivation -> TestTree
requestDeleteActivation = req
    "DeleteActivation"
    "fixture/DeleteActivation.yaml"

requestDescribeMaintenanceWindowExecutions :: DescribeMaintenanceWindowExecutions -> TestTree
requestDescribeMaintenanceWindowExecutions = req
    "DescribeMaintenanceWindowExecutions"
    "fixture/DescribeMaintenanceWindowExecutions.yaml"

requestGetInventorySchema :: GetInventorySchema -> TestTree
requestGetInventorySchema = req
    "GetInventorySchema"
    "fixture/GetInventorySchema.yaml"

requestListComplianceSummaries :: ListComplianceSummaries -> TestTree
requestListComplianceSummaries = req
    "ListComplianceSummaries"
    "fixture/ListComplianceSummaries.yaml"

requestStartAutomationExecution :: StartAutomationExecution -> TestTree
requestStartAutomationExecution = req
    "StartAutomationExecution"
    "fixture/StartAutomationExecution.yaml"

requestCreateActivation :: CreateActivation -> TestTree
requestCreateActivation = req
    "CreateActivation"
    "fixture/CreateActivation.yaml"

requestDeleteMaintenanceWindow :: DeleteMaintenanceWindow -> TestTree
requestDeleteMaintenanceWindow = req
    "DeleteMaintenanceWindow"
    "fixture/DeleteMaintenanceWindow.yaml"

requestUpdateMaintenanceWindow :: UpdateMaintenanceWindow -> TestTree
requestUpdateMaintenanceWindow = req
    "UpdateMaintenanceWindow"
    "fixture/UpdateMaintenanceWindow.yaml"

requestDescribeMaintenanceWindowExecutionTasks :: DescribeMaintenanceWindowExecutionTasks -> TestTree
requestDescribeMaintenanceWindowExecutionTasks = req
    "DescribeMaintenanceWindowExecutionTasks"
    "fixture/DescribeMaintenanceWindowExecutionTasks.yaml"

requestGetDefaultPatchBaseline :: GetDefaultPatchBaseline -> TestTree
requestGetDefaultPatchBaseline = req
    "GetDefaultPatchBaseline"
    "fixture/GetDefaultPatchBaseline.yaml"

requestGetMaintenanceWindowExecutionTask :: GetMaintenanceWindowExecutionTask -> TestTree
requestGetMaintenanceWindowExecutionTask = req
    "GetMaintenanceWindowExecutionTask"
    "fixture/GetMaintenanceWindowExecutionTask.yaml"

requestCreateDocument :: CreateDocument -> TestTree
requestCreateDocument = req
    "CreateDocument"
    "fixture/CreateDocument.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource = req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestDeleteParameters :: DeleteParameters -> TestTree
requestDeleteParameters = req
    "DeleteParameters"
    "fixture/DeleteParameters.yaml"

requestDescribePatchGroupState :: DescribePatchGroupState -> TestTree
requestDescribePatchGroupState = req
    "DescribePatchGroupState"
    "fixture/DescribePatchGroupState.yaml"

requestListCommandInvocations :: ListCommandInvocations -> TestTree
requestListCommandInvocations = req
    "ListCommandInvocations"
    "fixture/ListCommandInvocations.yaml"

requestDeregisterTargetFromMaintenanceWindow :: DeregisterTargetFromMaintenanceWindow -> TestTree
requestDeregisterTargetFromMaintenanceWindow = req
    "DeregisterTargetFromMaintenanceWindow"
    "fixture/DeregisterTargetFromMaintenanceWindow.yaml"

requestDescribeEffectivePatchesForPatchBaseline :: DescribeEffectivePatchesForPatchBaseline -> TestTree
requestDescribeEffectivePatchesForPatchBaseline = req
    "DescribeEffectivePatchesForPatchBaseline"
    "fixture/DescribeEffectivePatchesForPatchBaseline.yaml"

requestDescribeMaintenanceWindowTargets :: DescribeMaintenanceWindowTargets -> TestTree
requestDescribeMaintenanceWindowTargets = req
    "DescribeMaintenanceWindowTargets"
    "fixture/DescribeMaintenanceWindowTargets.yaml"

requestRegisterPatchBaselineForPatchGroup :: RegisterPatchBaselineForPatchGroup -> TestTree
requestRegisterPatchBaselineForPatchGroup = req
    "RegisterPatchBaselineForPatchGroup"
    "fixture/RegisterPatchBaselineForPatchGroup.yaml"

requestListDocuments :: ListDocuments -> TestTree
requestListDocuments = req
    "ListDocuments"
    "fixture/ListDocuments.yaml"

requestDescribeInstancePatchStates :: DescribeInstancePatchStates -> TestTree
requestDescribeInstancePatchStates = req
    "DescribeInstancePatchStates"
    "fixture/DescribeInstancePatchStates.yaml"

requestGetPatchBaselineForPatchGroup :: GetPatchBaselineForPatchGroup -> TestTree
requestGetPatchBaselineForPatchGroup = req
    "GetPatchBaselineForPatchGroup"
    "fixture/GetPatchBaselineForPatchGroup.yaml"

requestUpdateManagedInstanceRole :: UpdateManagedInstanceRole -> TestTree
requestUpdateManagedInstanceRole = req
    "UpdateManagedInstanceRole"
    "fixture/UpdateManagedInstanceRole.yaml"

requestListComplianceItems :: ListComplianceItems -> TestTree
requestListComplianceItems = req
    "ListComplianceItems"
    "fixture/ListComplianceItems.yaml"

requestGetDocument :: GetDocument -> TestTree
requestGetDocument = req
    "GetDocument"
    "fixture/GetDocument.yaml"

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource = req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestCancelCommand :: CancelCommand -> TestTree
requestCancelCommand = req
    "CancelCommand"
    "fixture/CancelCommand.yaml"

requestDescribeAutomationStepExecutions :: DescribeAutomationStepExecutions -> TestTree
requestDescribeAutomationStepExecutions = req
    "DescribeAutomationStepExecutions"
    "fixture/DescribeAutomationStepExecutions.yaml"

requestGetCommandInvocation :: GetCommandInvocation -> TestTree
requestGetCommandInvocation = req
    "GetCommandInvocation"
    "fixture/GetCommandInvocation.yaml"

requestDescribeInstancePatchStatesForPatchGroup :: DescribeInstancePatchStatesForPatchGroup -> TestTree
requestDescribeInstancePatchStatesForPatchGroup = req
    "DescribeInstancePatchStatesForPatchGroup"
    "fixture/DescribeInstancePatchStatesForPatchGroup.yaml"

requestDeregisterManagedInstance :: DeregisterManagedInstance -> TestTree
requestDeregisterManagedInstance = req
    "DeregisterManagedInstance"
    "fixture/DeregisterManagedInstance.yaml"

requestDescribeAssociation :: DescribeAssociation -> TestTree
requestDescribeAssociation = req
    "DescribeAssociation"
    "fixture/DescribeAssociation.yaml"

requestModifyDocumentPermission :: ModifyDocumentPermission -> TestTree
requestModifyDocumentPermission = req
    "ModifyDocumentPermission"
    "fixture/ModifyDocumentPermission.yaml"

requestDeleteResourceDataSync :: DeleteResourceDataSync -> TestTree
requestDeleteResourceDataSync = req
    "DeleteResourceDataSync"
    "fixture/DeleteResourceDataSync.yaml"

requestUpdateAssociationStatus :: UpdateAssociationStatus -> TestTree
requestUpdateAssociationStatus = req
    "UpdateAssociationStatus"
    "fixture/UpdateAssociationStatus.yaml"

requestDescribeAvailablePatches :: DescribeAvailablePatches -> TestTree
requestDescribeAvailablePatches = req
    "DescribeAvailablePatches"
    "fixture/DescribeAvailablePatches.yaml"

requestListDocumentVersions :: ListDocumentVersions -> TestTree
requestListDocumentVersions = req
    "ListDocumentVersions"
    "fixture/ListDocumentVersions.yaml"

requestDeregisterPatchBaselineForPatchGroup :: DeregisterPatchBaselineForPatchGroup -> TestTree
requestDeregisterPatchBaselineForPatchGroup = req
    "DeregisterPatchBaselineForPatchGroup"
    "fixture/DeregisterPatchBaselineForPatchGroup.yaml"

requestDescribePatchGroups :: DescribePatchGroups -> TestTree
requestDescribePatchGroups = req
    "DescribePatchGroups"
    "fixture/DescribePatchGroups.yaml"

requestGetMaintenanceWindow :: GetMaintenanceWindow -> TestTree
requestGetMaintenanceWindow = req
    "GetMaintenanceWindow"
    "fixture/GetMaintenanceWindow.yaml"

requestDescribeMaintenanceWindows :: DescribeMaintenanceWindows -> TestTree
requestDescribeMaintenanceWindows = req
    "DescribeMaintenanceWindows"
    "fixture/DescribeMaintenanceWindows.yaml"

requestRegisterTaskWithMaintenanceWindow :: RegisterTaskWithMaintenanceWindow -> TestTree
requestRegisterTaskWithMaintenanceWindow = req
    "RegisterTaskWithMaintenanceWindow"
    "fixture/RegisterTaskWithMaintenanceWindow.yaml"

requestRegisterDefaultPatchBaseline :: RegisterDefaultPatchBaseline -> TestTree
requestRegisterDefaultPatchBaseline = req
    "RegisterDefaultPatchBaseline"
    "fixture/RegisterDefaultPatchBaseline.yaml"

requestListResourceComplianceSummaries :: ListResourceComplianceSummaries -> TestTree
requestListResourceComplianceSummaries = req
    "ListResourceComplianceSummaries"
    "fixture/ListResourceComplianceSummaries.yaml"

requestListAssociationVersions :: ListAssociationVersions -> TestTree
requestListAssociationVersions = req
    "ListAssociationVersions"
    "fixture/ListAssociationVersions.yaml"

requestDescribeMaintenanceWindowTasks :: DescribeMaintenanceWindowTasks -> TestTree
requestDescribeMaintenanceWindowTasks = req
    "DescribeMaintenanceWindowTasks"
    "fixture/DescribeMaintenanceWindowTasks.yaml"

requestDescribeInstanceAssociationsStatus :: DescribeInstanceAssociationsStatus -> TestTree
requestDescribeInstanceAssociationsStatus = req
    "DescribeInstanceAssociationsStatus"
    "fixture/DescribeInstanceAssociationsStatus.yaml"

requestDeregisterTaskFromMaintenanceWindow :: DeregisterTaskFromMaintenanceWindow -> TestTree
requestDeregisterTaskFromMaintenanceWindow = req
    "DeregisterTaskFromMaintenanceWindow"
    "fixture/DeregisterTaskFromMaintenanceWindow.yaml"

requestListInventoryEntries :: ListInventoryEntries -> TestTree
requestListInventoryEntries = req
    "ListInventoryEntries"
    "fixture/ListInventoryEntries.yaml"

requestUpdateMaintenanceWindowTask :: UpdateMaintenanceWindowTask -> TestTree
requestUpdateMaintenanceWindowTask = req
    "UpdateMaintenanceWindowTask"
    "fixture/UpdateMaintenanceWindowTask.yaml"

requestGetParameterHistory :: GetParameterHistory -> TestTree
requestGetParameterHistory = req
    "GetParameterHistory"
    "fixture/GetParameterHistory.yaml"

requestCreateMaintenanceWindow :: CreateMaintenanceWindow -> TestTree
requestCreateMaintenanceWindow = req
    "CreateMaintenanceWindow"
    "fixture/CreateMaintenanceWindow.yaml"

requestStopAutomationExecution :: StopAutomationExecution -> TestTree
requestStopAutomationExecution = req
    "StopAutomationExecution"
    "fixture/StopAutomationExecution.yaml"

requestGetMaintenanceWindowExecution :: GetMaintenanceWindowExecution -> TestTree
requestGetMaintenanceWindowExecution = req
    "GetMaintenanceWindowExecution"
    "fixture/GetMaintenanceWindowExecution.yaml"

requestSendAutomationSignal :: SendAutomationSignal -> TestTree
requestSendAutomationSignal = req
    "SendAutomationSignal"
    "fixture/SendAutomationSignal.yaml"

requestPutParameter :: PutParameter -> TestTree
requestPutParameter = req
    "PutParameter"
    "fixture/PutParameter.yaml"

requestDescribeMaintenanceWindowExecutionTaskInvocations :: DescribeMaintenanceWindowExecutionTaskInvocations -> TestTree
requestDescribeMaintenanceWindowExecutionTaskInvocations = req
    "DescribeMaintenanceWindowExecutionTaskInvocations"
    "fixture/DescribeMaintenanceWindowExecutionTaskInvocations.yaml"

requestGetMaintenanceWindowExecutionTaskInvocation :: GetMaintenanceWindowExecutionTaskInvocation -> TestTree
requestGetMaintenanceWindowExecutionTaskInvocation = req
    "GetMaintenanceWindowExecutionTaskInvocation"
    "fixture/GetMaintenanceWindowExecutionTaskInvocation.yaml"

requestDeleteParameter :: DeleteParameter -> TestTree
requestDeleteParameter = req
    "DeleteParameter"
    "fixture/DeleteParameter.yaml"

requestDescribeInstanceInformation :: DescribeInstanceInformation -> TestTree
requestDescribeInstanceInformation = req
    "DescribeInstanceInformation"
    "fixture/DescribeInstanceInformation.yaml"

requestListAssociations :: ListAssociations -> TestTree
requestListAssociations = req
    "ListAssociations"
    "fixture/ListAssociations.yaml"

requestDeleteAssociation :: DeleteAssociation -> TestTree
requestDeleteAssociation = req
    "DeleteAssociation"
    "fixture/DeleteAssociation.yaml"

requestUpdateAssociation :: UpdateAssociation -> TestTree
requestUpdateAssociation = req
    "UpdateAssociation"
    "fixture/UpdateAssociation.yaml"

requestDescribeInventoryDeletions :: DescribeInventoryDeletions -> TestTree
requestDescribeInventoryDeletions = req
    "DescribeInventoryDeletions"
    "fixture/DescribeInventoryDeletions.yaml"

requestDeleteInventory :: DeleteInventory -> TestTree
requestDeleteInventory = req
    "DeleteInventory"
    "fixture/DeleteInventory.yaml"

requestPutInventory :: PutInventory -> TestTree
requestPutInventory = req
    "PutInventory"
    "fixture/PutInventory.yaml"

requestDescribeEffectiveInstanceAssociations :: DescribeEffectiveInstanceAssociations -> TestTree
requestDescribeEffectiveInstanceAssociations = req
    "DescribeEffectiveInstanceAssociations"
    "fixture/DescribeEffectiveInstanceAssociations.yaml"

requestDescribeAutomationExecutions :: DescribeAutomationExecutions -> TestTree
requestDescribeAutomationExecutions = req
    "DescribeAutomationExecutions"
    "fixture/DescribeAutomationExecutions.yaml"

requestGetAutomationExecution :: GetAutomationExecution -> TestTree
requestGetAutomationExecution = req
    "GetAutomationExecution"
    "fixture/GetAutomationExecution.yaml"

requestSendCommand :: SendCommand -> TestTree
requestSendCommand = req
    "SendCommand"
    "fixture/SendCommand.yaml"

requestDescribePatchBaselines :: DescribePatchBaselines -> TestTree
requestDescribePatchBaselines = req
    "DescribePatchBaselines"
    "fixture/DescribePatchBaselines.yaml"

requestGetPatchBaseline :: GetPatchBaseline -> TestTree
requestGetPatchBaseline = req
    "GetPatchBaseline"
    "fixture/GetPatchBaseline.yaml"

requestRegisterTargetWithMaintenanceWindow :: RegisterTargetWithMaintenanceWindow -> TestTree
requestRegisterTargetWithMaintenanceWindow = req
    "RegisterTargetWithMaintenanceWindow"
    "fixture/RegisterTargetWithMaintenanceWindow.yaml"

requestListCommands :: ListCommands -> TestTree
requestListCommands = req
    "ListCommands"
    "fixture/ListCommands.yaml"

requestUpdateDocument :: UpdateDocument -> TestTree
requestUpdateDocument = req
    "UpdateDocument"
    "fixture/UpdateDocument.yaml"

requestDeleteDocument :: DeleteDocument -> TestTree
requestDeleteDocument = req
    "DeleteDocument"
    "fixture/DeleteDocument.yaml"

requestDescribeDocumentPermission :: DescribeDocumentPermission -> TestTree
requestDescribeDocumentPermission = req
    "DescribeDocumentPermission"
    "fixture/DescribeDocumentPermission.yaml"

requestCreateAssociationBatch :: CreateAssociationBatch -> TestTree
requestCreateAssociationBatch = req
    "CreateAssociationBatch"
    "fixture/CreateAssociationBatch.yaml"

requestUpdateMaintenanceWindowTarget :: UpdateMaintenanceWindowTarget -> TestTree
requestUpdateMaintenanceWindowTarget = req
    "UpdateMaintenanceWindowTarget"
    "fixture/UpdateMaintenanceWindowTarget.yaml"

requestCreateResourceDataSync :: CreateResourceDataSync -> TestTree
requestCreateResourceDataSync = req
    "CreateResourceDataSync"
    "fixture/CreateResourceDataSync.yaml"

requestCreatePatchBaseline :: CreatePatchBaseline -> TestTree
requestCreatePatchBaseline = req
    "CreatePatchBaseline"
    "fixture/CreatePatchBaseline.yaml"

-- Responses

responseDescribeInstancePatches :: DescribeInstancePatchesResponse -> TestTree
responseDescribeInstancePatches = res
    "DescribeInstancePatchesResponse"
    "fixture/DescribeInstancePatchesResponse.proto"
    ssm
    (Proxy :: Proxy DescribeInstancePatches)

responseGetInventory :: GetInventoryResponse -> TestTree
responseGetInventory = res
    "GetInventoryResponse"
    "fixture/GetInventoryResponse.proto"
    ssm
    (Proxy :: Proxy GetInventory)

responseGetParameters :: GetParametersResponse -> TestTree
responseGetParameters = res
    "GetParametersResponse"
    "fixture/GetParametersResponse.proto"
    ssm
    (Proxy :: Proxy GetParameters)

responseDeletePatchBaseline :: DeletePatchBaselineResponse -> TestTree
responseDeletePatchBaseline = res
    "DeletePatchBaselineResponse"
    "fixture/DeletePatchBaselineResponse.proto"
    ssm
    (Proxy :: Proxy DeletePatchBaseline)

responseUpdatePatchBaseline :: UpdatePatchBaselineResponse -> TestTree
responseUpdatePatchBaseline = res
    "UpdatePatchBaselineResponse"
    "fixture/UpdatePatchBaselineResponse.proto"
    ssm
    (Proxy :: Proxy UpdatePatchBaseline)

responseGetParameter :: GetParameterResponse -> TestTree
responseGetParameter = res
    "GetParameterResponse"
    "fixture/GetParameterResponse.proto"
    ssm
    (Proxy :: Proxy GetParameter)

responseUpdateDocumentDefaultVersion :: UpdateDocumentDefaultVersionResponse -> TestTree
responseUpdateDocumentDefaultVersion = res
    "UpdateDocumentDefaultVersionResponse"
    "fixture/UpdateDocumentDefaultVersionResponse.proto"
    ssm
    (Proxy :: Proxy UpdateDocumentDefaultVersion)

responseListResourceDataSync :: ListResourceDataSyncResponse -> TestTree
responseListResourceDataSync = res
    "ListResourceDataSyncResponse"
    "fixture/ListResourceDataSyncResponse.proto"
    ssm
    (Proxy :: Proxy ListResourceDataSync)

responseGetDeployablePatchSnapshotForInstance :: GetDeployablePatchSnapshotForInstanceResponse -> TestTree
responseGetDeployablePatchSnapshotForInstance = res
    "GetDeployablePatchSnapshotForInstanceResponse"
    "fixture/GetDeployablePatchSnapshotForInstanceResponse.proto"
    ssm
    (Proxy :: Proxy GetDeployablePatchSnapshotForInstance)

responseDescribeParameters :: DescribeParametersResponse -> TestTree
responseDescribeParameters = res
    "DescribeParametersResponse"
    "fixture/DescribeParametersResponse.proto"
    ssm
    (Proxy :: Proxy DescribeParameters)

responseGetParametersByPath :: GetParametersByPathResponse -> TestTree
responseGetParametersByPath = res
    "GetParametersByPathResponse"
    "fixture/GetParametersByPathResponse.proto"
    ssm
    (Proxy :: Proxy GetParametersByPath)

responsePutComplianceItems :: PutComplianceItemsResponse -> TestTree
responsePutComplianceItems = res
    "PutComplianceItemsResponse"
    "fixture/PutComplianceItemsResponse.proto"
    ssm
    (Proxy :: Proxy PutComplianceItems)

responseDescribeActivations :: DescribeActivationsResponse -> TestTree
responseDescribeActivations = res
    "DescribeActivationsResponse"
    "fixture/DescribeActivationsResponse.proto"
    ssm
    (Proxy :: Proxy DescribeActivations)

responseGetMaintenanceWindowTask :: GetMaintenanceWindowTaskResponse -> TestTree
responseGetMaintenanceWindowTask = res
    "GetMaintenanceWindowTaskResponse"
    "fixture/GetMaintenanceWindowTaskResponse.proto"
    ssm
    (Proxy :: Proxy GetMaintenanceWindowTask)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    ssm
    (Proxy :: Proxy ListTagsForResource)

responseDescribeDocument :: DescribeDocumentResponse -> TestTree
responseDescribeDocument = res
    "DescribeDocumentResponse"
    "fixture/DescribeDocumentResponse.proto"
    ssm
    (Proxy :: Proxy DescribeDocument)

responseCreateAssociation :: CreateAssociationResponse -> TestTree
responseCreateAssociation = res
    "CreateAssociationResponse"
    "fixture/CreateAssociationResponse.proto"
    ssm
    (Proxy :: Proxy CreateAssociation)

responseDeleteActivation :: DeleteActivationResponse -> TestTree
responseDeleteActivation = res
    "DeleteActivationResponse"
    "fixture/DeleteActivationResponse.proto"
    ssm
    (Proxy :: Proxy DeleteActivation)

responseDescribeMaintenanceWindowExecutions :: DescribeMaintenanceWindowExecutionsResponse -> TestTree
responseDescribeMaintenanceWindowExecutions = res
    "DescribeMaintenanceWindowExecutionsResponse"
    "fixture/DescribeMaintenanceWindowExecutionsResponse.proto"
    ssm
    (Proxy :: Proxy DescribeMaintenanceWindowExecutions)

responseGetInventorySchema :: GetInventorySchemaResponse -> TestTree
responseGetInventorySchema = res
    "GetInventorySchemaResponse"
    "fixture/GetInventorySchemaResponse.proto"
    ssm
    (Proxy :: Proxy GetInventorySchema)

responseListComplianceSummaries :: ListComplianceSummariesResponse -> TestTree
responseListComplianceSummaries = res
    "ListComplianceSummariesResponse"
    "fixture/ListComplianceSummariesResponse.proto"
    ssm
    (Proxy :: Proxy ListComplianceSummaries)

responseStartAutomationExecution :: StartAutomationExecutionResponse -> TestTree
responseStartAutomationExecution = res
    "StartAutomationExecutionResponse"
    "fixture/StartAutomationExecutionResponse.proto"
    ssm
    (Proxy :: Proxy StartAutomationExecution)

responseCreateActivation :: CreateActivationResponse -> TestTree
responseCreateActivation = res
    "CreateActivationResponse"
    "fixture/CreateActivationResponse.proto"
    ssm
    (Proxy :: Proxy CreateActivation)

responseDeleteMaintenanceWindow :: DeleteMaintenanceWindowResponse -> TestTree
responseDeleteMaintenanceWindow = res
    "DeleteMaintenanceWindowResponse"
    "fixture/DeleteMaintenanceWindowResponse.proto"
    ssm
    (Proxy :: Proxy DeleteMaintenanceWindow)

responseUpdateMaintenanceWindow :: UpdateMaintenanceWindowResponse -> TestTree
responseUpdateMaintenanceWindow = res
    "UpdateMaintenanceWindowResponse"
    "fixture/UpdateMaintenanceWindowResponse.proto"
    ssm
    (Proxy :: Proxy UpdateMaintenanceWindow)

responseDescribeMaintenanceWindowExecutionTasks :: DescribeMaintenanceWindowExecutionTasksResponse -> TestTree
responseDescribeMaintenanceWindowExecutionTasks = res
    "DescribeMaintenanceWindowExecutionTasksResponse"
    "fixture/DescribeMaintenanceWindowExecutionTasksResponse.proto"
    ssm
    (Proxy :: Proxy DescribeMaintenanceWindowExecutionTasks)

responseGetDefaultPatchBaseline :: GetDefaultPatchBaselineResponse -> TestTree
responseGetDefaultPatchBaseline = res
    "GetDefaultPatchBaselineResponse"
    "fixture/GetDefaultPatchBaselineResponse.proto"
    ssm
    (Proxy :: Proxy GetDefaultPatchBaseline)

responseGetMaintenanceWindowExecutionTask :: GetMaintenanceWindowExecutionTaskResponse -> TestTree
responseGetMaintenanceWindowExecutionTask = res
    "GetMaintenanceWindowExecutionTaskResponse"
    "fixture/GetMaintenanceWindowExecutionTaskResponse.proto"
    ssm
    (Proxy :: Proxy GetMaintenanceWindowExecutionTask)

responseCreateDocument :: CreateDocumentResponse -> TestTree
responseCreateDocument = res
    "CreateDocumentResponse"
    "fixture/CreateDocumentResponse.proto"
    ssm
    (Proxy :: Proxy CreateDocument)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource = res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    ssm
    (Proxy :: Proxy RemoveTagsFromResource)

responseDeleteParameters :: DeleteParametersResponse -> TestTree
responseDeleteParameters = res
    "DeleteParametersResponse"
    "fixture/DeleteParametersResponse.proto"
    ssm
    (Proxy :: Proxy DeleteParameters)

responseDescribePatchGroupState :: DescribePatchGroupStateResponse -> TestTree
responseDescribePatchGroupState = res
    "DescribePatchGroupStateResponse"
    "fixture/DescribePatchGroupStateResponse.proto"
    ssm
    (Proxy :: Proxy DescribePatchGroupState)

responseListCommandInvocations :: ListCommandInvocationsResponse -> TestTree
responseListCommandInvocations = res
    "ListCommandInvocationsResponse"
    "fixture/ListCommandInvocationsResponse.proto"
    ssm
    (Proxy :: Proxy ListCommandInvocations)

responseDeregisterTargetFromMaintenanceWindow :: DeregisterTargetFromMaintenanceWindowResponse -> TestTree
responseDeregisterTargetFromMaintenanceWindow = res
    "DeregisterTargetFromMaintenanceWindowResponse"
    "fixture/DeregisterTargetFromMaintenanceWindowResponse.proto"
    ssm
    (Proxy :: Proxy DeregisterTargetFromMaintenanceWindow)

responseDescribeEffectivePatchesForPatchBaseline :: DescribeEffectivePatchesForPatchBaselineResponse -> TestTree
responseDescribeEffectivePatchesForPatchBaseline = res
    "DescribeEffectivePatchesForPatchBaselineResponse"
    "fixture/DescribeEffectivePatchesForPatchBaselineResponse.proto"
    ssm
    (Proxy :: Proxy DescribeEffectivePatchesForPatchBaseline)

responseDescribeMaintenanceWindowTargets :: DescribeMaintenanceWindowTargetsResponse -> TestTree
responseDescribeMaintenanceWindowTargets = res
    "DescribeMaintenanceWindowTargetsResponse"
    "fixture/DescribeMaintenanceWindowTargetsResponse.proto"
    ssm
    (Proxy :: Proxy DescribeMaintenanceWindowTargets)

responseRegisterPatchBaselineForPatchGroup :: RegisterPatchBaselineForPatchGroupResponse -> TestTree
responseRegisterPatchBaselineForPatchGroup = res
    "RegisterPatchBaselineForPatchGroupResponse"
    "fixture/RegisterPatchBaselineForPatchGroupResponse.proto"
    ssm
    (Proxy :: Proxy RegisterPatchBaselineForPatchGroup)

responseListDocuments :: ListDocumentsResponse -> TestTree
responseListDocuments = res
    "ListDocumentsResponse"
    "fixture/ListDocumentsResponse.proto"
    ssm
    (Proxy :: Proxy ListDocuments)

responseDescribeInstancePatchStates :: DescribeInstancePatchStatesResponse -> TestTree
responseDescribeInstancePatchStates = res
    "DescribeInstancePatchStatesResponse"
    "fixture/DescribeInstancePatchStatesResponse.proto"
    ssm
    (Proxy :: Proxy DescribeInstancePatchStates)

responseGetPatchBaselineForPatchGroup :: GetPatchBaselineForPatchGroupResponse -> TestTree
responseGetPatchBaselineForPatchGroup = res
    "GetPatchBaselineForPatchGroupResponse"
    "fixture/GetPatchBaselineForPatchGroupResponse.proto"
    ssm
    (Proxy :: Proxy GetPatchBaselineForPatchGroup)

responseUpdateManagedInstanceRole :: UpdateManagedInstanceRoleResponse -> TestTree
responseUpdateManagedInstanceRole = res
    "UpdateManagedInstanceRoleResponse"
    "fixture/UpdateManagedInstanceRoleResponse.proto"
    ssm
    (Proxy :: Proxy UpdateManagedInstanceRole)

responseListComplianceItems :: ListComplianceItemsResponse -> TestTree
responseListComplianceItems = res
    "ListComplianceItemsResponse"
    "fixture/ListComplianceItemsResponse.proto"
    ssm
    (Proxy :: Proxy ListComplianceItems)

responseGetDocument :: GetDocumentResponse -> TestTree
responseGetDocument = res
    "GetDocumentResponse"
    "fixture/GetDocumentResponse.proto"
    ssm
    (Proxy :: Proxy GetDocument)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource = res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    ssm
    (Proxy :: Proxy AddTagsToResource)

responseCancelCommand :: CancelCommandResponse -> TestTree
responseCancelCommand = res
    "CancelCommandResponse"
    "fixture/CancelCommandResponse.proto"
    ssm
    (Proxy :: Proxy CancelCommand)

responseDescribeAutomationStepExecutions :: DescribeAutomationStepExecutionsResponse -> TestTree
responseDescribeAutomationStepExecutions = res
    "DescribeAutomationStepExecutionsResponse"
    "fixture/DescribeAutomationStepExecutionsResponse.proto"
    ssm
    (Proxy :: Proxy DescribeAutomationStepExecutions)

responseGetCommandInvocation :: GetCommandInvocationResponse -> TestTree
responseGetCommandInvocation = res
    "GetCommandInvocationResponse"
    "fixture/GetCommandInvocationResponse.proto"
    ssm
    (Proxy :: Proxy GetCommandInvocation)

responseDescribeInstancePatchStatesForPatchGroup :: DescribeInstancePatchStatesForPatchGroupResponse -> TestTree
responseDescribeInstancePatchStatesForPatchGroup = res
    "DescribeInstancePatchStatesForPatchGroupResponse"
    "fixture/DescribeInstancePatchStatesForPatchGroupResponse.proto"
    ssm
    (Proxy :: Proxy DescribeInstancePatchStatesForPatchGroup)

responseDeregisterManagedInstance :: DeregisterManagedInstanceResponse -> TestTree
responseDeregisterManagedInstance = res
    "DeregisterManagedInstanceResponse"
    "fixture/DeregisterManagedInstanceResponse.proto"
    ssm
    (Proxy :: Proxy DeregisterManagedInstance)

responseDescribeAssociation :: DescribeAssociationResponse -> TestTree
responseDescribeAssociation = res
    "DescribeAssociationResponse"
    "fixture/DescribeAssociationResponse.proto"
    ssm
    (Proxy :: Proxy DescribeAssociation)

responseModifyDocumentPermission :: ModifyDocumentPermissionResponse -> TestTree
responseModifyDocumentPermission = res
    "ModifyDocumentPermissionResponse"
    "fixture/ModifyDocumentPermissionResponse.proto"
    ssm
    (Proxy :: Proxy ModifyDocumentPermission)

responseDeleteResourceDataSync :: DeleteResourceDataSyncResponse -> TestTree
responseDeleteResourceDataSync = res
    "DeleteResourceDataSyncResponse"
    "fixture/DeleteResourceDataSyncResponse.proto"
    ssm
    (Proxy :: Proxy DeleteResourceDataSync)

responseUpdateAssociationStatus :: UpdateAssociationStatusResponse -> TestTree
responseUpdateAssociationStatus = res
    "UpdateAssociationStatusResponse"
    "fixture/UpdateAssociationStatusResponse.proto"
    ssm
    (Proxy :: Proxy UpdateAssociationStatus)

responseDescribeAvailablePatches :: DescribeAvailablePatchesResponse -> TestTree
responseDescribeAvailablePatches = res
    "DescribeAvailablePatchesResponse"
    "fixture/DescribeAvailablePatchesResponse.proto"
    ssm
    (Proxy :: Proxy DescribeAvailablePatches)

responseListDocumentVersions :: ListDocumentVersionsResponse -> TestTree
responseListDocumentVersions = res
    "ListDocumentVersionsResponse"
    "fixture/ListDocumentVersionsResponse.proto"
    ssm
    (Proxy :: Proxy ListDocumentVersions)

responseDeregisterPatchBaselineForPatchGroup :: DeregisterPatchBaselineForPatchGroupResponse -> TestTree
responseDeregisterPatchBaselineForPatchGroup = res
    "DeregisterPatchBaselineForPatchGroupResponse"
    "fixture/DeregisterPatchBaselineForPatchGroupResponse.proto"
    ssm
    (Proxy :: Proxy DeregisterPatchBaselineForPatchGroup)

responseDescribePatchGroups :: DescribePatchGroupsResponse -> TestTree
responseDescribePatchGroups = res
    "DescribePatchGroupsResponse"
    "fixture/DescribePatchGroupsResponse.proto"
    ssm
    (Proxy :: Proxy DescribePatchGroups)

responseGetMaintenanceWindow :: GetMaintenanceWindowResponse -> TestTree
responseGetMaintenanceWindow = res
    "GetMaintenanceWindowResponse"
    "fixture/GetMaintenanceWindowResponse.proto"
    ssm
    (Proxy :: Proxy GetMaintenanceWindow)

responseDescribeMaintenanceWindows :: DescribeMaintenanceWindowsResponse -> TestTree
responseDescribeMaintenanceWindows = res
    "DescribeMaintenanceWindowsResponse"
    "fixture/DescribeMaintenanceWindowsResponse.proto"
    ssm
    (Proxy :: Proxy DescribeMaintenanceWindows)

responseRegisterTaskWithMaintenanceWindow :: RegisterTaskWithMaintenanceWindowResponse -> TestTree
responseRegisterTaskWithMaintenanceWindow = res
    "RegisterTaskWithMaintenanceWindowResponse"
    "fixture/RegisterTaskWithMaintenanceWindowResponse.proto"
    ssm
    (Proxy :: Proxy RegisterTaskWithMaintenanceWindow)

responseRegisterDefaultPatchBaseline :: RegisterDefaultPatchBaselineResponse -> TestTree
responseRegisterDefaultPatchBaseline = res
    "RegisterDefaultPatchBaselineResponse"
    "fixture/RegisterDefaultPatchBaselineResponse.proto"
    ssm
    (Proxy :: Proxy RegisterDefaultPatchBaseline)

responseListResourceComplianceSummaries :: ListResourceComplianceSummariesResponse -> TestTree
responseListResourceComplianceSummaries = res
    "ListResourceComplianceSummariesResponse"
    "fixture/ListResourceComplianceSummariesResponse.proto"
    ssm
    (Proxy :: Proxy ListResourceComplianceSummaries)

responseListAssociationVersions :: ListAssociationVersionsResponse -> TestTree
responseListAssociationVersions = res
    "ListAssociationVersionsResponse"
    "fixture/ListAssociationVersionsResponse.proto"
    ssm
    (Proxy :: Proxy ListAssociationVersions)

responseDescribeMaintenanceWindowTasks :: DescribeMaintenanceWindowTasksResponse -> TestTree
responseDescribeMaintenanceWindowTasks = res
    "DescribeMaintenanceWindowTasksResponse"
    "fixture/DescribeMaintenanceWindowTasksResponse.proto"
    ssm
    (Proxy :: Proxy DescribeMaintenanceWindowTasks)

responseDescribeInstanceAssociationsStatus :: DescribeInstanceAssociationsStatusResponse -> TestTree
responseDescribeInstanceAssociationsStatus = res
    "DescribeInstanceAssociationsStatusResponse"
    "fixture/DescribeInstanceAssociationsStatusResponse.proto"
    ssm
    (Proxy :: Proxy DescribeInstanceAssociationsStatus)

responseDeregisterTaskFromMaintenanceWindow :: DeregisterTaskFromMaintenanceWindowResponse -> TestTree
responseDeregisterTaskFromMaintenanceWindow = res
    "DeregisterTaskFromMaintenanceWindowResponse"
    "fixture/DeregisterTaskFromMaintenanceWindowResponse.proto"
    ssm
    (Proxy :: Proxy DeregisterTaskFromMaintenanceWindow)

responseListInventoryEntries :: ListInventoryEntriesResponse -> TestTree
responseListInventoryEntries = res
    "ListInventoryEntriesResponse"
    "fixture/ListInventoryEntriesResponse.proto"
    ssm
    (Proxy :: Proxy ListInventoryEntries)

responseUpdateMaintenanceWindowTask :: UpdateMaintenanceWindowTaskResponse -> TestTree
responseUpdateMaintenanceWindowTask = res
    "UpdateMaintenanceWindowTaskResponse"
    "fixture/UpdateMaintenanceWindowTaskResponse.proto"
    ssm
    (Proxy :: Proxy UpdateMaintenanceWindowTask)

responseGetParameterHistory :: GetParameterHistoryResponse -> TestTree
responseGetParameterHistory = res
    "GetParameterHistoryResponse"
    "fixture/GetParameterHistoryResponse.proto"
    ssm
    (Proxy :: Proxy GetParameterHistory)

responseCreateMaintenanceWindow :: CreateMaintenanceWindowResponse -> TestTree
responseCreateMaintenanceWindow = res
    "CreateMaintenanceWindowResponse"
    "fixture/CreateMaintenanceWindowResponse.proto"
    ssm
    (Proxy :: Proxy CreateMaintenanceWindow)

responseStopAutomationExecution :: StopAutomationExecutionResponse -> TestTree
responseStopAutomationExecution = res
    "StopAutomationExecutionResponse"
    "fixture/StopAutomationExecutionResponse.proto"
    ssm
    (Proxy :: Proxy StopAutomationExecution)

responseGetMaintenanceWindowExecution :: GetMaintenanceWindowExecutionResponse -> TestTree
responseGetMaintenanceWindowExecution = res
    "GetMaintenanceWindowExecutionResponse"
    "fixture/GetMaintenanceWindowExecutionResponse.proto"
    ssm
    (Proxy :: Proxy GetMaintenanceWindowExecution)

responseSendAutomationSignal :: SendAutomationSignalResponse -> TestTree
responseSendAutomationSignal = res
    "SendAutomationSignalResponse"
    "fixture/SendAutomationSignalResponse.proto"
    ssm
    (Proxy :: Proxy SendAutomationSignal)

responsePutParameter :: PutParameterResponse -> TestTree
responsePutParameter = res
    "PutParameterResponse"
    "fixture/PutParameterResponse.proto"
    ssm
    (Proxy :: Proxy PutParameter)

responseDescribeMaintenanceWindowExecutionTaskInvocations :: DescribeMaintenanceWindowExecutionTaskInvocationsResponse -> TestTree
responseDescribeMaintenanceWindowExecutionTaskInvocations = res
    "DescribeMaintenanceWindowExecutionTaskInvocationsResponse"
    "fixture/DescribeMaintenanceWindowExecutionTaskInvocationsResponse.proto"
    ssm
    (Proxy :: Proxy DescribeMaintenanceWindowExecutionTaskInvocations)

responseGetMaintenanceWindowExecutionTaskInvocation :: GetMaintenanceWindowExecutionTaskInvocationResponse -> TestTree
responseGetMaintenanceWindowExecutionTaskInvocation = res
    "GetMaintenanceWindowExecutionTaskInvocationResponse"
    "fixture/GetMaintenanceWindowExecutionTaskInvocationResponse.proto"
    ssm
    (Proxy :: Proxy GetMaintenanceWindowExecutionTaskInvocation)

responseDeleteParameter :: DeleteParameterResponse -> TestTree
responseDeleteParameter = res
    "DeleteParameterResponse"
    "fixture/DeleteParameterResponse.proto"
    ssm
    (Proxy :: Proxy DeleteParameter)

responseDescribeInstanceInformation :: DescribeInstanceInformationResponse -> TestTree
responseDescribeInstanceInformation = res
    "DescribeInstanceInformationResponse"
    "fixture/DescribeInstanceInformationResponse.proto"
    ssm
    (Proxy :: Proxy DescribeInstanceInformation)

responseListAssociations :: ListAssociationsResponse -> TestTree
responseListAssociations = res
    "ListAssociationsResponse"
    "fixture/ListAssociationsResponse.proto"
    ssm
    (Proxy :: Proxy ListAssociations)

responseDeleteAssociation :: DeleteAssociationResponse -> TestTree
responseDeleteAssociation = res
    "DeleteAssociationResponse"
    "fixture/DeleteAssociationResponse.proto"
    ssm
    (Proxy :: Proxy DeleteAssociation)

responseUpdateAssociation :: UpdateAssociationResponse -> TestTree
responseUpdateAssociation = res
    "UpdateAssociationResponse"
    "fixture/UpdateAssociationResponse.proto"
    ssm
    (Proxy :: Proxy UpdateAssociation)

responseDescribeInventoryDeletions :: DescribeInventoryDeletionsResponse -> TestTree
responseDescribeInventoryDeletions = res
    "DescribeInventoryDeletionsResponse"
    "fixture/DescribeInventoryDeletionsResponse.proto"
    ssm
    (Proxy :: Proxy DescribeInventoryDeletions)

responseDeleteInventory :: DeleteInventoryResponse -> TestTree
responseDeleteInventory = res
    "DeleteInventoryResponse"
    "fixture/DeleteInventoryResponse.proto"
    ssm
    (Proxy :: Proxy DeleteInventory)

responsePutInventory :: PutInventoryResponse -> TestTree
responsePutInventory = res
    "PutInventoryResponse"
    "fixture/PutInventoryResponse.proto"
    ssm
    (Proxy :: Proxy PutInventory)

responseDescribeEffectiveInstanceAssociations :: DescribeEffectiveInstanceAssociationsResponse -> TestTree
responseDescribeEffectiveInstanceAssociations = res
    "DescribeEffectiveInstanceAssociationsResponse"
    "fixture/DescribeEffectiveInstanceAssociationsResponse.proto"
    ssm
    (Proxy :: Proxy DescribeEffectiveInstanceAssociations)

responseDescribeAutomationExecutions :: DescribeAutomationExecutionsResponse -> TestTree
responseDescribeAutomationExecutions = res
    "DescribeAutomationExecutionsResponse"
    "fixture/DescribeAutomationExecutionsResponse.proto"
    ssm
    (Proxy :: Proxy DescribeAutomationExecutions)

responseGetAutomationExecution :: GetAutomationExecutionResponse -> TestTree
responseGetAutomationExecution = res
    "GetAutomationExecutionResponse"
    "fixture/GetAutomationExecutionResponse.proto"
    ssm
    (Proxy :: Proxy GetAutomationExecution)

responseSendCommand :: SendCommandResponse -> TestTree
responseSendCommand = res
    "SendCommandResponse"
    "fixture/SendCommandResponse.proto"
    ssm
    (Proxy :: Proxy SendCommand)

responseDescribePatchBaselines :: DescribePatchBaselinesResponse -> TestTree
responseDescribePatchBaselines = res
    "DescribePatchBaselinesResponse"
    "fixture/DescribePatchBaselinesResponse.proto"
    ssm
    (Proxy :: Proxy DescribePatchBaselines)

responseGetPatchBaseline :: GetPatchBaselineResponse -> TestTree
responseGetPatchBaseline = res
    "GetPatchBaselineResponse"
    "fixture/GetPatchBaselineResponse.proto"
    ssm
    (Proxy :: Proxy GetPatchBaseline)

responseRegisterTargetWithMaintenanceWindow :: RegisterTargetWithMaintenanceWindowResponse -> TestTree
responseRegisterTargetWithMaintenanceWindow = res
    "RegisterTargetWithMaintenanceWindowResponse"
    "fixture/RegisterTargetWithMaintenanceWindowResponse.proto"
    ssm
    (Proxy :: Proxy RegisterTargetWithMaintenanceWindow)

responseListCommands :: ListCommandsResponse -> TestTree
responseListCommands = res
    "ListCommandsResponse"
    "fixture/ListCommandsResponse.proto"
    ssm
    (Proxy :: Proxy ListCommands)

responseUpdateDocument :: UpdateDocumentResponse -> TestTree
responseUpdateDocument = res
    "UpdateDocumentResponse"
    "fixture/UpdateDocumentResponse.proto"
    ssm
    (Proxy :: Proxy UpdateDocument)

responseDeleteDocument :: DeleteDocumentResponse -> TestTree
responseDeleteDocument = res
    "DeleteDocumentResponse"
    "fixture/DeleteDocumentResponse.proto"
    ssm
    (Proxy :: Proxy DeleteDocument)

responseDescribeDocumentPermission :: DescribeDocumentPermissionResponse -> TestTree
responseDescribeDocumentPermission = res
    "DescribeDocumentPermissionResponse"
    "fixture/DescribeDocumentPermissionResponse.proto"
    ssm
    (Proxy :: Proxy DescribeDocumentPermission)

responseCreateAssociationBatch :: CreateAssociationBatchResponse -> TestTree
responseCreateAssociationBatch = res
    "CreateAssociationBatchResponse"
    "fixture/CreateAssociationBatchResponse.proto"
    ssm
    (Proxy :: Proxy CreateAssociationBatch)

responseUpdateMaintenanceWindowTarget :: UpdateMaintenanceWindowTargetResponse -> TestTree
responseUpdateMaintenanceWindowTarget = res
    "UpdateMaintenanceWindowTargetResponse"
    "fixture/UpdateMaintenanceWindowTargetResponse.proto"
    ssm
    (Proxy :: Proxy UpdateMaintenanceWindowTarget)

responseCreateResourceDataSync :: CreateResourceDataSyncResponse -> TestTree
responseCreateResourceDataSync = res
    "CreateResourceDataSyncResponse"
    "fixture/CreateResourceDataSyncResponse.proto"
    ssm
    (Proxy :: Proxy CreateResourceDataSync)

responseCreatePatchBaseline :: CreatePatchBaselineResponse -> TestTree
responseCreatePatchBaseline = res
    "CreatePatchBaselineResponse"
    "fixture/CreatePatchBaselineResponse.proto"
    ssm
    (Proxy :: Proxy CreatePatchBaseline)
