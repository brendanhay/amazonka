-- Module      : Test.AWS.Gen.SSM
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.SSM where

import           Data.Proxy
import           Network.AWS.SSM
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ describeDocumentTest $
--             describeDocument
--
--         , createAssociationTest $
--             createAssociation
--
--         , createDocumentTest $
--             createDocument
--
--         , listDocumentsTest $
--             listDocuments
--
--         , getDocumentTest $
--             getDocument
--
--         , describeAssociationTest $
--             describeAssociation
--
--         , updateAssociationStatusTest $
--             updateAssociationStatus
--
--         , deleteAssociationTest $
--             deleteAssociation
--
--         , listAssociationsTest $
--             listAssociations
--
--         , deleteDocumentTest $
--             deleteDocument
--
--         , createAssociationBatchTest $
--             createAssociationBatch
--
--           ]

--     , testGroup "response"
--         [ describeDocumentResponseTest $
--             describeDocumentResponse
--
--         , createAssociationResponseTest $
--             createAssociationResponse
--
--         , createDocumentResponseTest $
--             createDocumentResponse
--
--         , listDocumentsResponseTest $
--             listDocumentsResponse
--
--         , getDocumentResponseTest $
--             getDocumentResponse
--
--         , describeAssociationResponseTest $
--             describeAssociationResponse
--
--         , updateAssociationStatusResponseTest $
--             updateAssociationStatusResponse
--
--         , deleteAssociationResponseTest $
--             deleteAssociationResponse
--
--         , listAssociationsResponseTest $
--             listAssociationsResponse
--
--         , deleteDocumentResponseTest $
--             deleteDocumentResponse
--
--         , createAssociationBatchResponseTest $
--             createAssociationBatchResponse
--
--           ]
--     ]

-- Requests

describeDocumentTest :: DescribeDocument -> TestTree
describeDocumentTest = undefined

createAssociationTest :: CreateAssociation -> TestTree
createAssociationTest = undefined

createDocumentTest :: CreateDocument -> TestTree
createDocumentTest = undefined

listDocumentsTest :: ListDocuments -> TestTree
listDocumentsTest = undefined

getDocumentTest :: GetDocument -> TestTree
getDocumentTest = undefined

describeAssociationTest :: DescribeAssociation -> TestTree
describeAssociationTest = undefined

updateAssociationStatusTest :: UpdateAssociationStatus -> TestTree
updateAssociationStatusTest = undefined

deleteAssociationTest :: DeleteAssociation -> TestTree
deleteAssociationTest = undefined

listAssociationsTest :: ListAssociations -> TestTree
listAssociationsTest = undefined

deleteDocumentTest :: DeleteDocument -> TestTree
deleteDocumentTest = undefined

createAssociationBatchTest :: CreateAssociationBatch -> TestTree
createAssociationBatchTest = undefined

-- Responses

describeDocumentResponseTest :: DescribeDocumentResponse -> TestTree
describeDocumentResponseTest = resp
    "DescribeDocumentResponse"
    "fixture/SSM/DescribeDocumentResponse"
    (Proxy :: Proxy DescribeDocument)

createAssociationResponseTest :: CreateAssociationResponse -> TestTree
createAssociationResponseTest = resp
    "CreateAssociationResponse"
    "fixture/SSM/CreateAssociationResponse"
    (Proxy :: Proxy CreateAssociation)

createDocumentResponseTest :: CreateDocumentResponse -> TestTree
createDocumentResponseTest = resp
    "CreateDocumentResponse"
    "fixture/SSM/CreateDocumentResponse"
    (Proxy :: Proxy CreateDocument)

listDocumentsResponseTest :: ListDocumentsResponse -> TestTree
listDocumentsResponseTest = resp
    "ListDocumentsResponse"
    "fixture/SSM/ListDocumentsResponse"
    (Proxy :: Proxy ListDocuments)

getDocumentResponseTest :: GetDocumentResponse -> TestTree
getDocumentResponseTest = resp
    "GetDocumentResponse"
    "fixture/SSM/GetDocumentResponse"
    (Proxy :: Proxy GetDocument)

describeAssociationResponseTest :: DescribeAssociationResponse -> TestTree
describeAssociationResponseTest = resp
    "DescribeAssociationResponse"
    "fixture/SSM/DescribeAssociationResponse"
    (Proxy :: Proxy DescribeAssociation)

updateAssociationStatusResponseTest :: UpdateAssociationStatusResponse -> TestTree
updateAssociationStatusResponseTest = resp
    "UpdateAssociationStatusResponse"
    "fixture/SSM/UpdateAssociationStatusResponse"
    (Proxy :: Proxy UpdateAssociationStatus)

deleteAssociationResponseTest :: DeleteAssociationResponse -> TestTree
deleteAssociationResponseTest = resp
    "DeleteAssociationResponse"
    "fixture/SSM/DeleteAssociationResponse"
    (Proxy :: Proxy DeleteAssociation)

listAssociationsResponseTest :: ListAssociationsResponse -> TestTree
listAssociationsResponseTest = resp
    "ListAssociationsResponse"
    "fixture/SSM/ListAssociationsResponse"
    (Proxy :: Proxy ListAssociations)

deleteDocumentResponseTest :: DeleteDocumentResponse -> TestTree
deleteDocumentResponseTest = resp
    "DeleteDocumentResponse"
    "fixture/SSM/DeleteDocumentResponse"
    (Proxy :: Proxy DeleteDocument)

createAssociationBatchResponseTest :: CreateAssociationBatchResponse -> TestTree
createAssociationBatchResponseTest = resp
    "CreateAssociationBatchResponse"
    "fixture/SSM/CreateAssociationBatchResponse"
    (Proxy :: Proxy CreateAssociationBatch)
