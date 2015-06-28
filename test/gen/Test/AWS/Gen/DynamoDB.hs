-- Module      : Test.AWS.Gen.DynamoDB
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

module Test.AWS.Gen.DynamoDB where

import           Data.Proxy
import           Network.AWS.DynamoDB
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ updateItemTest $
--             updateItem
--
--         , deleteItemTest $
--             deleteItem
--
--         , putItemTest $
--             putItem
--
--         , deleteTableTest $
--             deleteTable
--
--         , updateTableTest $
--             updateTable
--
--         , batchGetItemTest $
--             batchGetItem
--
--         , describeTableTest $
--             describeTable
--
--         , getItemTest $
--             getItem
--
--         , batchWriteItemTest $
--             batchWriteItem
--
--         , scanTest $
--             scan
--
--         , listTablesTest $
--             listTables
--
--         , queryTest $
--             query
--
--         , createTableTest $
--             createTable
--
--           ]

--     , testGroup "response"
--         [ updateItemResponseTest $
--             updateItemResponse
--
--         , deleteItemResponseTest $
--             deleteItemResponse
--
--         , putItemResponseTest $
--             putItemResponse
--
--         , deleteTableResponseTest $
--             deleteTableResponse
--
--         , updateTableResponseTest $
--             updateTableResponse
--
--         , batchGetItemResponseTest $
--             batchGetItemResponse
--
--         , describeTableResponseTest $
--             describeTableResponse
--
--         , getItemResponseTest $
--             getItemResponse
--
--         , batchWriteItemResponseTest $
--             batchWriteItemResponse
--
--         , scanResponseTest $
--             scanResponse
--
--         , listTablesResponseTest $
--             listTablesResponse
--
--         , queryResponseTest $
--             queryResponse
--
--         , createTableResponseTest $
--             createTableResponse
--
--           ]
--     ]

-- Requests

updateItemTest :: UpdateItem -> TestTree
updateItemTest = undefined

deleteItemTest :: DeleteItem -> TestTree
deleteItemTest = undefined

putItemTest :: PutItem -> TestTree
putItemTest = undefined

deleteTableTest :: DeleteTable -> TestTree
deleteTableTest = undefined

updateTableTest :: UpdateTable -> TestTree
updateTableTest = undefined

batchGetItemTest :: BatchGetItem -> TestTree
batchGetItemTest = undefined

describeTableTest :: DescribeTable -> TestTree
describeTableTest = undefined

getItemTest :: GetItem -> TestTree
getItemTest = undefined

batchWriteItemTest :: BatchWriteItem -> TestTree
batchWriteItemTest = undefined

scanTest :: Scan -> TestTree
scanTest = undefined

listTablesTest :: ListTables -> TestTree
listTablesTest = undefined

queryTest :: Query -> TestTree
queryTest = undefined

createTableTest :: CreateTable -> TestTree
createTableTest = undefined

-- Responses

updateItemResponseTest :: UpdateItemResponse -> TestTree
updateItemResponseTest = resp
    "UpdateItemResponse"
    "fixture/DynamoDB/UpdateItemResponse"
    (Proxy :: Proxy UpdateItem)

deleteItemResponseTest :: DeleteItemResponse -> TestTree
deleteItemResponseTest = resp
    "DeleteItemResponse"
    "fixture/DynamoDB/DeleteItemResponse"
    (Proxy :: Proxy DeleteItem)

putItemResponseTest :: PutItemResponse -> TestTree
putItemResponseTest = resp
    "PutItemResponse"
    "fixture/DynamoDB/PutItemResponse"
    (Proxy :: Proxy PutItem)

deleteTableResponseTest :: DeleteTableResponse -> TestTree
deleteTableResponseTest = resp
    "DeleteTableResponse"
    "fixture/DynamoDB/DeleteTableResponse"
    (Proxy :: Proxy DeleteTable)

updateTableResponseTest :: UpdateTableResponse -> TestTree
updateTableResponseTest = resp
    "UpdateTableResponse"
    "fixture/DynamoDB/UpdateTableResponse"
    (Proxy :: Proxy UpdateTable)

batchGetItemResponseTest :: BatchGetItemResponse -> TestTree
batchGetItemResponseTest = resp
    "BatchGetItemResponse"
    "fixture/DynamoDB/BatchGetItemResponse"
    (Proxy :: Proxy BatchGetItem)

describeTableResponseTest :: DescribeTableResponse -> TestTree
describeTableResponseTest = resp
    "DescribeTableResponse"
    "fixture/DynamoDB/DescribeTableResponse"
    (Proxy :: Proxy DescribeTable)

getItemResponseTest :: GetItemResponse -> TestTree
getItemResponseTest = resp
    "GetItemResponse"
    "fixture/DynamoDB/GetItemResponse"
    (Proxy :: Proxy GetItem)

batchWriteItemResponseTest :: BatchWriteItemResponse -> TestTree
batchWriteItemResponseTest = resp
    "BatchWriteItemResponse"
    "fixture/DynamoDB/BatchWriteItemResponse"
    (Proxy :: Proxy BatchWriteItem)

scanResponseTest :: ScanResponse -> TestTree
scanResponseTest = resp
    "ScanResponse"
    "fixture/DynamoDB/ScanResponse"
    (Proxy :: Proxy Scan)

listTablesResponseTest :: ListTablesResponse -> TestTree
listTablesResponseTest = resp
    "ListTablesResponse"
    "fixture/DynamoDB/ListTablesResponse"
    (Proxy :: Proxy ListTables)

queryResponseTest :: QueryResponse -> TestTree
queryResponseTest = resp
    "QueryResponse"
    "fixture/DynamoDB/QueryResponse"
    (Proxy :: Proxy Query)

createTableResponseTest :: CreateTableResponse -> TestTree
createTableResponseTest = resp
    "CreateTableResponse"
    "fixture/DynamoDB/CreateTableResponse"
    (Proxy :: Proxy CreateTable)
