{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SSM
-- Copyright   : (c) 2013-2015 Brendan Hay
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
--         [ testDescribeDocument $
--             describeDocument
--
--         , testCreateAssociation $
--             createAssociation
--
--         , testCreateDocument $
--             createDocument
--
--         , testListDocuments $
--             listDocuments
--
--         , testGetDocument $
--             getDocument
--
--         , testDescribeAssociation $
--             describeAssociation
--
--         , testUpdateAssociationStatus $
--             updateAssociationStatus
--
--         , testDeleteAssociation $
--             deleteAssociation
--
--         , testListAssociations $
--             listAssociations
--
--         , testDeleteDocument $
--             deleteDocument
--
--         , testCreateAssociationBatch $
--             createAssociationBatch
--
--           ]

--     , testGroup "response"
--         [ testDescribeDocumentResponse $
--             describeDocumentResponse
--
--         , testCreateAssociationResponse $
--             createAssociationResponse
--
--         , testCreateDocumentResponse $
--             createDocumentResponse
--
--         , testListDocumentsResponse $
--             listDocumentsResponse
--
--         , testGetDocumentResponse $
--             getDocumentResponse
--
--         , testDescribeAssociationResponse $
--             describeAssociationResponse
--
--         , testUpdateAssociationStatusResponse $
--             updateAssociationStatusResponse
--
--         , testDeleteAssociationResponse $
--             deleteAssociationResponse
--
--         , testListAssociationsResponse $
--             listAssociationsResponse
--
--         , testDeleteDocumentResponse $
--             deleteDocumentResponse
--
--         , testCreateAssociationBatchResponse $
--             createAssociationBatchResponse
--
--           ]
--     ]

-- Requests

testDescribeDocument :: DescribeDocument -> TestTree
testDescribeDocument = req
    "DescribeDocument"
    "fixture/DescribeDocument"

testCreateAssociation :: CreateAssociation -> TestTree
testCreateAssociation = req
    "CreateAssociation"
    "fixture/CreateAssociation"

testCreateDocument :: CreateDocument -> TestTree
testCreateDocument = req
    "CreateDocument"
    "fixture/CreateDocument"

testListDocuments :: ListDocuments -> TestTree
testListDocuments = req
    "ListDocuments"
    "fixture/ListDocuments"

testGetDocument :: GetDocument -> TestTree
testGetDocument = req
    "GetDocument"
    "fixture/GetDocument"

testDescribeAssociation :: DescribeAssociation -> TestTree
testDescribeAssociation = req
    "DescribeAssociation"
    "fixture/DescribeAssociation"

testUpdateAssociationStatus :: UpdateAssociationStatus -> TestTree
testUpdateAssociationStatus = req
    "UpdateAssociationStatus"
    "fixture/UpdateAssociationStatus"

testDeleteAssociation :: DeleteAssociation -> TestTree
testDeleteAssociation = req
    "DeleteAssociation"
    "fixture/DeleteAssociation"

testListAssociations :: ListAssociations -> TestTree
testListAssociations = req
    "ListAssociations"
    "fixture/ListAssociations"

testDeleteDocument :: DeleteDocument -> TestTree
testDeleteDocument = req
    "DeleteDocument"
    "fixture/DeleteDocument"

testCreateAssociationBatch :: CreateAssociationBatch -> TestTree
testCreateAssociationBatch = req
    "CreateAssociationBatch"
    "fixture/CreateAssociationBatch"

-- Responses

testDescribeDocumentResponse :: DescribeDocumentResponse -> TestTree
testDescribeDocumentResponse = res
    "DescribeDocumentResponse"
    "fixture/DescribeDocumentResponse"
    (Proxy :: Proxy DescribeDocument)

testCreateAssociationResponse :: CreateAssociationResponse -> TestTree
testCreateAssociationResponse = res
    "CreateAssociationResponse"
    "fixture/CreateAssociationResponse"
    (Proxy :: Proxy CreateAssociation)

testCreateDocumentResponse :: CreateDocumentResponse -> TestTree
testCreateDocumentResponse = res
    "CreateDocumentResponse"
    "fixture/CreateDocumentResponse"
    (Proxy :: Proxy CreateDocument)

testListDocumentsResponse :: ListDocumentsResponse -> TestTree
testListDocumentsResponse = res
    "ListDocumentsResponse"
    "fixture/ListDocumentsResponse"
    (Proxy :: Proxy ListDocuments)

testGetDocumentResponse :: GetDocumentResponse -> TestTree
testGetDocumentResponse = res
    "GetDocumentResponse"
    "fixture/GetDocumentResponse"
    (Proxy :: Proxy GetDocument)

testDescribeAssociationResponse :: DescribeAssociationResponse -> TestTree
testDescribeAssociationResponse = res
    "DescribeAssociationResponse"
    "fixture/DescribeAssociationResponse"
    (Proxy :: Proxy DescribeAssociation)

testUpdateAssociationStatusResponse :: UpdateAssociationStatusResponse -> TestTree
testUpdateAssociationStatusResponse = res
    "UpdateAssociationStatusResponse"
    "fixture/UpdateAssociationStatusResponse"
    (Proxy :: Proxy UpdateAssociationStatus)

testDeleteAssociationResponse :: DeleteAssociationResponse -> TestTree
testDeleteAssociationResponse = res
    "DeleteAssociationResponse"
    "fixture/DeleteAssociationResponse"
    (Proxy :: Proxy DeleteAssociation)

testListAssociationsResponse :: ListAssociationsResponse -> TestTree
testListAssociationsResponse = res
    "ListAssociationsResponse"
    "fixture/ListAssociationsResponse"
    (Proxy :: Proxy ListAssociations)

testDeleteDocumentResponse :: DeleteDocumentResponse -> TestTree
testDeleteDocumentResponse = res
    "DeleteDocumentResponse"
    "fixture/DeleteDocumentResponse"
    (Proxy :: Proxy DeleteDocument)

testCreateAssociationBatchResponse :: CreateAssociationBatchResponse -> TestTree
testCreateAssociationBatchResponse = res
    "CreateAssociationBatchResponse"
    "fixture/CreateAssociationBatchResponse"
    (Proxy :: Proxy CreateAssociationBatch)
