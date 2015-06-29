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

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.SSM

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ createAssociationTest $
--             createAssociation
--
--         , createAssociationBatchTest $
--             createAssociationBatch
--
--         , createDocumentTest $
--             createDocument
--
--         , deleteAssociationTest $
--             deleteAssociation
--
--         , deleteDocumentTest $
--             deleteDocument
--
--         , describeAssociationTest $
--             describeAssociation
--
--         , describeDocumentTest $
--             describeDocument
--
--         , getDocumentTest $
--             getDocument
--
--         , listAssociationsTest $
--             listAssociations
--
--         , listDocumentsTest $
--             listDocuments
--
--         , updateAssociationStatusTest $
--             updateAssociationStatus
--
--           ]

--     , testGroup "response"
--         [ createAssociationResponseTest $
--             createAssociationResponse
--
--         , createAssociationBatchResponseTest $
--             createAssociationBatchResponse
--
--         , createDocumentResponseTest $
--             createDocumentResponse
--
--         , deleteAssociationResponseTest $
--             deleteAssociationResponse
--
--         , deleteDocumentResponseTest $
--             deleteDocumentResponse
--
--         , describeAssociationResponseTest $
--             describeAssociationResponse
--
--         , describeDocumentResponseTest $
--             describeDocumentResponse
--
--         , getDocumentResponseTest $
--             getDocumentResponse
--
--         , listAssociationsResponseTest $
--             listAssociationsResponse
--
--         , listDocumentsResponseTest $
--             listDocumentsResponse
--
--         , updateAssociationStatusResponseTest $
--             updateAssociationStatusResponse
--
--           ]
--     ]

-- Requests

createAssociationTest :: CreateAssociation -> TestTree
createAssociationTest = undefined

createAssociationBatchTest :: CreateAssociationBatch -> TestTree
createAssociationBatchTest = undefined

createDocumentTest :: CreateDocument -> TestTree
createDocumentTest = undefined

deleteAssociationTest :: DeleteAssociation -> TestTree
deleteAssociationTest = undefined

deleteDocumentTest :: DeleteDocument -> TestTree
deleteDocumentTest = undefined

describeAssociationTest :: DescribeAssociation -> TestTree
describeAssociationTest = undefined

describeDocumentTest :: DescribeDocument -> TestTree
describeDocumentTest = undefined

getDocumentTest :: GetDocument -> TestTree
getDocumentTest = undefined

listAssociationsTest :: ListAssociations -> TestTree
listAssociationsTest = undefined

listDocumentsTest :: ListDocuments -> TestTree
listDocumentsTest = undefined

updateAssociationStatusTest :: UpdateAssociationStatus -> TestTree
updateAssociationStatusTest = undefined

-- Responses

createAssociationResponseTest :: CreateAssociationResponse -> TestTree
createAssociationResponseTest = resp
    "createAssociationResponse"
    "fixture/CreateAssociationResponse"
    (Proxy :: Proxy CreateAssociation)

createAssociationBatchResponseTest :: CreateAssociationBatchResponse -> TestTree
createAssociationBatchResponseTest = resp
    "createAssociationBatchResponse"
    "fixture/CreateAssociationBatchResponse"
    (Proxy :: Proxy CreateAssociationBatch)

createDocumentResponseTest :: CreateDocumentResponse -> TestTree
createDocumentResponseTest = resp
    "createDocumentResponse"
    "fixture/CreateDocumentResponse"
    (Proxy :: Proxy CreateDocument)

deleteAssociationResponseTest :: DeleteAssociationResponse -> TestTree
deleteAssociationResponseTest = resp
    "deleteAssociationResponse"
    "fixture/DeleteAssociationResponse"
    (Proxy :: Proxy DeleteAssociation)

deleteDocumentResponseTest :: DeleteDocumentResponse -> TestTree
deleteDocumentResponseTest = resp
    "deleteDocumentResponse"
    "fixture/DeleteDocumentResponse"
    (Proxy :: Proxy DeleteDocument)

describeAssociationResponseTest :: DescribeAssociationResponse -> TestTree
describeAssociationResponseTest = resp
    "describeAssociationResponse"
    "fixture/DescribeAssociationResponse"
    (Proxy :: Proxy DescribeAssociation)

describeDocumentResponseTest :: DescribeDocumentResponse -> TestTree
describeDocumentResponseTest = resp
    "describeDocumentResponse"
    "fixture/DescribeDocumentResponse"
    (Proxy :: Proxy DescribeDocument)

getDocumentResponseTest :: GetDocumentResponse -> TestTree
getDocumentResponseTest = resp
    "getDocumentResponse"
    "fixture/GetDocumentResponse"
    (Proxy :: Proxy GetDocument)

listAssociationsResponseTest :: ListAssociationsResponse -> TestTree
listAssociationsResponseTest = resp
    "listAssociationsResponse"
    "fixture/ListAssociationsResponse"
    (Proxy :: Proxy ListAssociations)

listDocumentsResponseTest :: ListDocumentsResponse -> TestTree
listDocumentsResponseTest = resp
    "listDocumentsResponse"
    "fixture/ListDocumentsResponse"
    (Proxy :: Proxy ListDocuments)

updateAssociationStatusResponseTest :: UpdateAssociationStatusResponse -> TestTree
updateAssociationStatusResponseTest = resp
    "updateAssociationStatusResponse"
    "fixture/UpdateAssociationStatusResponse"
    (Proxy :: Proxy UpdateAssociationStatus)
