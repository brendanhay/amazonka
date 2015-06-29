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

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.DynamoDB

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ batchGetItemTest $
--             batchGetItem
--
--         , batchWriteItemTest $
--             batchWriteItem
--
--         , createTableTest $
--             createTable
--
--         , deleteItemTest $
--             deleteItem
--
--         , deleteTableTest $
--             deleteTable
--
--         , describeTableTest $
--             describeTable
--
--         , getItemTest $
--             getItem
--
--         , listTablesTest $
--             listTables
--
--         , putItemTest $
--             putItem
--
--         , queryTest $
--             query
--
--         , scanTest $
--             scan
--
--         , updateItemTest $
--             updateItem
--
--         , updateTableTest $
--             updateTable
--
--           ]

--     , testGroup "response"
--         [ batchGetItemResponseTest $
--             batchGetItemResponse
--
--         , batchWriteItemResponseTest $
--             batchWriteItemResponse
--
--         , createTableResponseTest $
--             createTableResponse
--
--         , deleteItemResponseTest $
--             deleteItemResponse
--
--         , deleteTableResponseTest $
--             deleteTableResponse
--
--         , describeTableResponseTest $
--             describeTableResponse
--
--         , getItemResponseTest $
--             getItemResponse
--
--         , listTablesResponseTest $
--             listTablesResponse
--
--         , putItemResponseTest $
--             putItemResponse
--
--         , queryResponseTest $
--             queryResponse
--
--         , scanResponseTest $
--             scanResponse
--
--         , updateItemResponseTest $
--             updateItemResponse
--
--         , updateTableResponseTest $
--             updateTableResponse
--
--           ]
--     ]

-- Requests

batchGetItemTest :: BatchGetItem -> TestTree
batchGetItemTest = undefined

batchWriteItemTest :: BatchWriteItem -> TestTree
batchWriteItemTest = undefined

createTableTest :: CreateTable -> TestTree
createTableTest = undefined

deleteItemTest :: DeleteItem -> TestTree
deleteItemTest = undefined

deleteTableTest :: DeleteTable -> TestTree
deleteTableTest = undefined

describeTableTest :: DescribeTable -> TestTree
describeTableTest = undefined

getItemTest :: GetItem -> TestTree
getItemTest = undefined

listTablesTest :: ListTables -> TestTree
listTablesTest = undefined

putItemTest :: PutItem -> TestTree
putItemTest = undefined

queryTest :: Query -> TestTree
queryTest = undefined

scanTest :: Scan -> TestTree
scanTest = undefined

updateItemTest :: UpdateItem -> TestTree
updateItemTest = undefined

updateTableTest :: UpdateTable -> TestTree
updateTableTest = undefined

-- Responses

batchGetItemResponseTest :: BatchGetItemResponse -> TestTree
batchGetItemResponseTest = resp
    "batchGetItemResponse"
    "fixture/BatchGetItemResponse"
    (Proxy :: Proxy BatchGetItem)

batchWriteItemResponseTest :: BatchWriteItemResponse -> TestTree
batchWriteItemResponseTest = resp
    "batchWriteItemResponse"
    "fixture/BatchWriteItemResponse"
    (Proxy :: Proxy BatchWriteItem)

createTableResponseTest :: CreateTableResponse -> TestTree
createTableResponseTest = resp
    "createTableResponse"
    "fixture/CreateTableResponse"
    (Proxy :: Proxy CreateTable)

deleteItemResponseTest :: DeleteItemResponse -> TestTree
deleteItemResponseTest = resp
    "deleteItemResponse"
    "fixture/DeleteItemResponse"
    (Proxy :: Proxy DeleteItem)

deleteTableResponseTest :: DeleteTableResponse -> TestTree
deleteTableResponseTest = resp
    "deleteTableResponse"
    "fixture/DeleteTableResponse"
    (Proxy :: Proxy DeleteTable)

describeTableResponseTest :: DescribeTableResponse -> TestTree
describeTableResponseTest = resp
    "describeTableResponse"
    "fixture/DescribeTableResponse"
    (Proxy :: Proxy DescribeTable)

getItemResponseTest :: GetItemResponse -> TestTree
getItemResponseTest = resp
    "getItemResponse"
    "fixture/GetItemResponse"
    (Proxy :: Proxy GetItem)

listTablesResponseTest :: ListTablesResponse -> TestTree
listTablesResponseTest = resp
    "listTablesResponse"
    "fixture/ListTablesResponse"
    (Proxy :: Proxy ListTables)

putItemResponseTest :: PutItemResponse -> TestTree
putItemResponseTest = resp
    "putItemResponse"
    "fixture/PutItemResponse"
    (Proxy :: Proxy PutItem)

queryResponseTest :: QueryResponse -> TestTree
queryResponseTest = resp
    "queryResponse"
    "fixture/QueryResponse"
    (Proxy :: Proxy Query)

scanResponseTest :: ScanResponse -> TestTree
scanResponseTest = resp
    "scanResponse"
    "fixture/ScanResponse"
    (Proxy :: Proxy Scan)

updateItemResponseTest :: UpdateItemResponse -> TestTree
updateItemResponseTest = resp
    "updateItemResponse"
    "fixture/UpdateItemResponse"
    (Proxy :: Proxy UpdateItem)

updateTableResponseTest :: UpdateTableResponse -> TestTree
updateTableResponseTest = resp
    "updateTableResponse"
    "fixture/UpdateTableResponse"
    (Proxy :: Proxy UpdateTable)
