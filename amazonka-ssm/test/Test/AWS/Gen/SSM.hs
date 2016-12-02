{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SSM
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.SSM where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.SSM
import Test.AWS.SSM.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetInventory $
--             getInventory
--
--         , requestGetParameters $
--             getParameters
--
--         , requestUpdateDocumentDefaultVersion $
--             updateDocumentDefaultVersion
--
--         , requestDescribeParameters $
--             describeParameters
--
--         , requestDescribeActivations $
--             describeActivations
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
--         , requestGetMaintenanceWindowExecutionTask $
--             getMaintenanceWindowExecutionTask
--
--         , requestCreateDocument $
--             createDocument
--
--         , requestRemoveTagsFromResource $
--             removeTagsFromResource
--
--         , requestListCommandInvocations $
--             listCommandInvocations
--
--         , requestDeregisterTargetFromMaintenanceWindow $
--             deregisterTargetFromMaintenanceWindow
--
--         , requestDescribeMaintenanceWindowTargets $
--             describeMaintenanceWindowTargets
--
--         , requestListDocuments $
--             listDocuments
--
--         , requestUpdateManagedInstanceRole $
--             updateManagedInstanceRole
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
--         , requestGetCommandInvocation $
--             getCommandInvocation
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
--         , requestUpdateAssociationStatus $
--             updateAssociationStatus
--
--         , requestListDocumentVersions $
--             listDocumentVersions
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
--         , requestPutParameter $
--             putParameter
--
--         , requestDescribeMaintenanceWindowExecutionTaskInvocations $
--             describeMaintenanceWindowExecutionTaskInvocations
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
--           ]

--     , testGroup "response"
--         [ responseGetInventory $
--             getInventoryResponse
--
--         , responseGetParameters $
--             getParametersResponse
--
--         , responseUpdateDocumentDefaultVersion $
--             updateDocumentDefaultVersionResponse
--
--         , responseDescribeParameters $
--             describeParametersResponse
--
--         , responseDescribeActivations $
--             describeActivationsResponse
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
--         , responseGetMaintenanceWindowExecutionTask $
--             getMaintenanceWindowExecutionTaskResponse
--
--         , responseCreateDocument $
--             createDocumentResponse
--
--         , responseRemoveTagsFromResource $
--             removeTagsFromResourceResponse
--
--         , responseListCommandInvocations $
--             listCommandInvocationsResponse
--
--         , responseDeregisterTargetFromMaintenanceWindow $
--             deregisterTargetFromMaintenanceWindowResponse
--
--         , responseDescribeMaintenanceWindowTargets $
--             describeMaintenanceWindowTargetsResponse
--
--         , responseListDocuments $
--             listDocumentsResponse
--
--         , responseUpdateManagedInstanceRole $
--             updateManagedInstanceRoleResponse
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
--         , responseGetCommandInvocation $
--             getCommandInvocationResponse
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
--         , responseUpdateAssociationStatus $
--             updateAssociationStatusResponse
--
--         , responseListDocumentVersions $
--             listDocumentVersionsResponse
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
--         , responsePutParameter $
--             putParameterResponse
--
--         , responseDescribeMaintenanceWindowExecutionTaskInvocations $
--             describeMaintenanceWindowExecutionTaskInvocationsResponse
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
--           ]
--     ]

-- Requests

requestGetInventory :: GetInventory -> TestTree
requestGetInventory = req
    "GetInventory"
    "fixture/GetInventory.yaml"

requestGetParameters :: GetParameters -> TestTree
requestGetParameters = req
    "GetParameters"
    "fixture/GetParameters.yaml"

requestUpdateDocumentDefaultVersion :: UpdateDocumentDefaultVersion -> TestTree
requestUpdateDocumentDefaultVersion = req
    "UpdateDocumentDefaultVersion"
    "fixture/UpdateDocumentDefaultVersion.yaml"

requestDescribeParameters :: DescribeParameters -> TestTree
requestDescribeParameters = req
    "DescribeParameters"
    "fixture/DescribeParameters.yaml"

requestDescribeActivations :: DescribeActivations -> TestTree
requestDescribeActivations = req
    "DescribeActivations"
    "fixture/DescribeActivations.yaml"

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

requestListCommandInvocations :: ListCommandInvocations -> TestTree
requestListCommandInvocations = req
    "ListCommandInvocations"
    "fixture/ListCommandInvocations.yaml"

requestDeregisterTargetFromMaintenanceWindow :: DeregisterTargetFromMaintenanceWindow -> TestTree
requestDeregisterTargetFromMaintenanceWindow = req
    "DeregisterTargetFromMaintenanceWindow"
    "fixture/DeregisterTargetFromMaintenanceWindow.yaml"

requestDescribeMaintenanceWindowTargets :: DescribeMaintenanceWindowTargets -> TestTree
requestDescribeMaintenanceWindowTargets = req
    "DescribeMaintenanceWindowTargets"
    "fixture/DescribeMaintenanceWindowTargets.yaml"

requestListDocuments :: ListDocuments -> TestTree
requestListDocuments = req
    "ListDocuments"
    "fixture/ListDocuments.yaml"

requestUpdateManagedInstanceRole :: UpdateManagedInstanceRole -> TestTree
requestUpdateManagedInstanceRole = req
    "UpdateManagedInstanceRole"
    "fixture/UpdateManagedInstanceRole.yaml"

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

requestGetCommandInvocation :: GetCommandInvocation -> TestTree
requestGetCommandInvocation = req
    "GetCommandInvocation"
    "fixture/GetCommandInvocation.yaml"

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

requestUpdateAssociationStatus :: UpdateAssociationStatus -> TestTree
requestUpdateAssociationStatus = req
    "UpdateAssociationStatus"
    "fixture/UpdateAssociationStatus.yaml"

requestListDocumentVersions :: ListDocumentVersions -> TestTree
requestListDocumentVersions = req
    "ListDocumentVersions"
    "fixture/ListDocumentVersions.yaml"

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

requestPutParameter :: PutParameter -> TestTree
requestPutParameter = req
    "PutParameter"
    "fixture/PutParameter.yaml"

requestDescribeMaintenanceWindowExecutionTaskInvocations :: DescribeMaintenanceWindowExecutionTaskInvocations -> TestTree
requestDescribeMaintenanceWindowExecutionTaskInvocations = req
    "DescribeMaintenanceWindowExecutionTaskInvocations"
    "fixture/DescribeMaintenanceWindowExecutionTaskInvocations.yaml"

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

-- Responses

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

responseUpdateDocumentDefaultVersion :: UpdateDocumentDefaultVersionResponse -> TestTree
responseUpdateDocumentDefaultVersion = res
    "UpdateDocumentDefaultVersionResponse"
    "fixture/UpdateDocumentDefaultVersionResponse.proto"
    ssm
    (Proxy :: Proxy UpdateDocumentDefaultVersion)

responseDescribeParameters :: DescribeParametersResponse -> TestTree
responseDescribeParameters = res
    "DescribeParametersResponse"
    "fixture/DescribeParametersResponse.proto"
    ssm
    (Proxy :: Proxy DescribeParameters)

responseDescribeActivations :: DescribeActivationsResponse -> TestTree
responseDescribeActivations = res
    "DescribeActivationsResponse"
    "fixture/DescribeActivationsResponse.proto"
    ssm
    (Proxy :: Proxy DescribeActivations)

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

responseDescribeMaintenanceWindowTargets :: DescribeMaintenanceWindowTargetsResponse -> TestTree
responseDescribeMaintenanceWindowTargets = res
    "DescribeMaintenanceWindowTargetsResponse"
    "fixture/DescribeMaintenanceWindowTargetsResponse.proto"
    ssm
    (Proxy :: Proxy DescribeMaintenanceWindowTargets)

responseListDocuments :: ListDocumentsResponse -> TestTree
responseListDocuments = res
    "ListDocumentsResponse"
    "fixture/ListDocumentsResponse.proto"
    ssm
    (Proxy :: Proxy ListDocuments)

responseUpdateManagedInstanceRole :: UpdateManagedInstanceRoleResponse -> TestTree
responseUpdateManagedInstanceRole = res
    "UpdateManagedInstanceRoleResponse"
    "fixture/UpdateManagedInstanceRoleResponse.proto"
    ssm
    (Proxy :: Proxy UpdateManagedInstanceRole)

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

responseGetCommandInvocation :: GetCommandInvocationResponse -> TestTree
responseGetCommandInvocation = res
    "GetCommandInvocationResponse"
    "fixture/GetCommandInvocationResponse.proto"
    ssm
    (Proxy :: Proxy GetCommandInvocation)

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

responseUpdateAssociationStatus :: UpdateAssociationStatusResponse -> TestTree
responseUpdateAssociationStatus = res
    "UpdateAssociationStatusResponse"
    "fixture/UpdateAssociationStatusResponse.proto"
    ssm
    (Proxy :: Proxy UpdateAssociationStatus)

responseListDocumentVersions :: ListDocumentVersionsResponse -> TestTree
responseListDocumentVersions = res
    "ListDocumentVersionsResponse"
    "fixture/ListDocumentVersionsResponse.proto"
    ssm
    (Proxy :: Proxy ListDocumentVersions)

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
