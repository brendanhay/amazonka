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
--         [ requestDescribeDocument $
--             describeDocument
--
--         , requestCreateAssociation $
--             createAssociation
--
--         , requestCreateDocument $
--             createDocument
--
--         , requestListCommandInvocations $
--             listCommandInvocations
--
--         , requestListDocuments $
--             listDocuments
--
--         , requestGetDocument $
--             getDocument
--
--         , requestCancelCommand $
--             cancelCommand
--
--         , requestDescribeAssociation $
--             describeAssociation
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
--         , requestCreateAssociationBatch $
--             createAssociationBatch
--
--           ]

--     , testGroup "response"
--         [ responseDescribeDocument $
--             describeDocumentResponse
--
--         , responseCreateAssociation $
--             createAssociationResponse
--
--         , responseCreateDocument $
--             createDocumentResponse
--
--         , responseListCommandInvocations $
--             listCommandInvocationsResponse
--
--         , responseListDocuments $
--             listDocumentsResponse
--
--         , responseGetDocument $
--             getDocumentResponse
--
--         , responseCancelCommand $
--             cancelCommandResponse
--
--         , responseDescribeAssociation $
--             describeAssociationResponse
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
--         , responseCreateAssociationBatch $
--             createAssociationBatchResponse
--
--           ]
--     ]

-- Requests

requestDescribeDocument :: DescribeDocument -> TestTree
requestDescribeDocument = req
    "DescribeDocument"
    "fixture/DescribeDocument.yaml"

requestCreateAssociation :: CreateAssociation -> TestTree
requestCreateAssociation = req
    "CreateAssociation"
    "fixture/CreateAssociation.yaml"

requestCreateDocument :: CreateDocument -> TestTree
requestCreateDocument = req
    "CreateDocument"
    "fixture/CreateDocument.yaml"

requestListCommandInvocations :: ListCommandInvocations -> TestTree
requestListCommandInvocations = req
    "ListCommandInvocations"
    "fixture/ListCommandInvocations.yaml"

requestListDocuments :: ListDocuments -> TestTree
requestListDocuments = req
    "ListDocuments"
    "fixture/ListDocuments.yaml"

requestGetDocument :: GetDocument -> TestTree
requestGetDocument = req
    "GetDocument"
    "fixture/GetDocument.yaml"

requestCancelCommand :: CancelCommand -> TestTree
requestCancelCommand = req
    "CancelCommand"
    "fixture/CancelCommand.yaml"

requestDescribeAssociation :: DescribeAssociation -> TestTree
requestDescribeAssociation = req
    "DescribeAssociation"
    "fixture/DescribeAssociation.yaml"

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

requestCreateAssociationBatch :: CreateAssociationBatch -> TestTree
requestCreateAssociationBatch = req
    "CreateAssociationBatch"
    "fixture/CreateAssociationBatch.yaml"

-- Responses

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

responseCreateDocument :: CreateDocumentResponse -> TestTree
responseCreateDocument = res
    "CreateDocumentResponse"
    "fixture/CreateDocumentResponse.proto"
    ssm
    (Proxy :: Proxy CreateDocument)

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

responseGetDocument :: GetDocumentResponse -> TestTree
responseGetDocument = res
    "GetDocumentResponse"
    "fixture/GetDocumentResponse.proto"
    ssm
    (Proxy :: Proxy GetDocument)

responseCancelCommand :: CancelCommandResponse -> TestTree
responseCancelCommand = res
    "CancelCommandResponse"
    "fixture/CancelCommandResponse.proto"
    ssm
    (Proxy :: Proxy CancelCommand)

responseDescribeAssociation :: DescribeAssociationResponse -> TestTree
responseDescribeAssociation = res
    "DescribeAssociationResponse"
    "fixture/DescribeAssociationResponse.proto"
    ssm
    (Proxy :: Proxy DescribeAssociation)

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

responseCreateAssociationBatch :: CreateAssociationBatchResponse -> TestTree
responseCreateAssociationBatch = res
    "CreateAssociationBatchResponse"
    "fixture/CreateAssociationBatchResponse.proto"
    ssm
    (Proxy :: Proxy CreateAssociationBatch)
