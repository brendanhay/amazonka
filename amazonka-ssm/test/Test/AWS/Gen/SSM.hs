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
--         [ requestDescribeActivations $
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
--         , requestCreateActivation $
--             createActivation
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
--         , requestDescribeInstanceInformation $
--             describeInstanceInformation
--
--         , requestListAssociations $
--             listAssociations
--
--         , requestDeleteAssociation $
--             deleteAssociation
--
--         , requestSendCommand $
--             sendCommand
--
--         , requestListCommands $
--             listCommands
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
--         [ responseDescribeActivations $
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
--         , responseCreateActivation $
--             createActivationResponse
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
--         , responseDescribeInstanceInformation $
--             describeInstanceInformationResponse
--
--         , responseListAssociations $
--             listAssociationsResponse
--
--         , responseDeleteAssociation $
--             deleteAssociationResponse
--
--         , responseSendCommand $
--             sendCommandResponse
--
--         , responseListCommands $
--             listCommandsResponse
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

requestCreateActivation :: CreateActivation -> TestTree
requestCreateActivation = req
    "CreateActivation"
    "fixture/CreateActivation.yaml"

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

requestSendCommand :: SendCommand -> TestTree
requestSendCommand = req
    "SendCommand"
    "fixture/SendCommand.yaml"

requestListCommands :: ListCommands -> TestTree
requestListCommands = req
    "ListCommands"
    "fixture/ListCommands.yaml"

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

responseCreateActivation :: CreateActivationResponse -> TestTree
responseCreateActivation = res
    "CreateActivationResponse"
    "fixture/CreateActivationResponse.proto"
    ssm
    (Proxy :: Proxy CreateActivation)

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

responseSendCommand :: SendCommandResponse -> TestTree
responseSendCommand = res
    "SendCommandResponse"
    "fixture/SendCommandResponse.proto"
    ssm
    (Proxy :: Proxy SendCommand)

responseListCommands :: ListCommandsResponse -> TestTree
responseListCommands = res
    "ListCommandsResponse"
    "fixture/ListCommandsResponse.proto"
    ssm
    (Proxy :: Proxy ListCommands)

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
