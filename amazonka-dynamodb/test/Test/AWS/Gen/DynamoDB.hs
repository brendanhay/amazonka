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
-- fixtures =
--     [ testGroup "request"
--         [ testUpdateItem $
--             updateItem
--
--         , testDeleteItem $
--             deleteItem
--
--         , testPutItem $
--             putItem
--
--         , testDeleteTable $
--             deleteTable
--
--         , testUpdateTable $
--             updateTable
--
--         , testBatchGetItem $
--             batchGetItem
--
--         , testDescribeTable $
--             describeTable
--
--         , testGetItem $
--             getItem
--
--         , testBatchWriteItem $
--             batchWriteItem
--
--         , testScan $
--             scan
--
--         , testListTables $
--             listTables
--
--         , testQuery $
--             query
--
--         , testCreateTable $
--             createTable
--
--           ]

--     , testGroup "response"
--         [ testUpdateItemResponse $
--             updateItemResponse
--
--         , testDeleteItemResponse $
--             deleteItemResponse
--
--         , testPutItemResponse $
--             putItemResponse
--
--         , testDeleteTableResponse $
--             deleteTableResponse
--
--         , testUpdateTableResponse $
--             updateTableResponse
--
--         , testBatchGetItemResponse $
--             batchGetItemResponse
--
--         , testDescribeTableResponse $
--             describeTableResponse
--
--         , testGetItemResponse $
--             getItemResponse
--
--         , testBatchWriteItemResponse $
--             batchWriteItemResponse
--
--         , testScanResponse $
--             scanResponse
--
--         , testListTablesResponse $
--             listTablesResponse
--
--         , testQueryResponse $
--             queryResponse
--
--         , testCreateTableResponse $
--             createTableResponse
--
--           ]
--     ]

-- Requests

testUpdateItem :: UpdateItem -> TestTree
testUpdateItem = undefined

testDeleteItem :: DeleteItem -> TestTree
testDeleteItem = undefined

testPutItem :: PutItem -> TestTree
testPutItem = undefined

testDeleteTable :: DeleteTable -> TestTree
testDeleteTable = undefined

testUpdateTable :: UpdateTable -> TestTree
testUpdateTable = undefined

testBatchGetItem :: BatchGetItem -> TestTree
testBatchGetItem = undefined

testDescribeTable :: DescribeTable -> TestTree
testDescribeTable = undefined

testGetItem :: GetItem -> TestTree
testGetItem = undefined

testBatchWriteItem :: BatchWriteItem -> TestTree
testBatchWriteItem = undefined

testScan :: Scan -> TestTree
testScan = undefined

testListTables :: ListTables -> TestTree
testListTables = undefined

testQuery :: Query -> TestTree
testQuery = undefined

testCreateTable :: CreateTable -> TestTree
testCreateTable = undefined

-- Responses

testUpdateItemResponse :: UpdateItemResponse -> TestTree
testUpdateItemResponse = resp
    "UpdateItemResponse"
    "fixture/UpdateItemResponse"
    (Proxy :: Proxy UpdateItem)

testDeleteItemResponse :: DeleteItemResponse -> TestTree
testDeleteItemResponse = resp
    "DeleteItemResponse"
    "fixture/DeleteItemResponse"
    (Proxy :: Proxy DeleteItem)

testPutItemResponse :: PutItemResponse -> TestTree
testPutItemResponse = resp
    "PutItemResponse"
    "fixture/PutItemResponse"
    (Proxy :: Proxy PutItem)

testDeleteTableResponse :: DeleteTableResponse -> TestTree
testDeleteTableResponse = resp
    "DeleteTableResponse"
    "fixture/DeleteTableResponse"
    (Proxy :: Proxy DeleteTable)

testUpdateTableResponse :: UpdateTableResponse -> TestTree
testUpdateTableResponse = resp
    "UpdateTableResponse"
    "fixture/UpdateTableResponse"
    (Proxy :: Proxy UpdateTable)

testBatchGetItemResponse :: BatchGetItemResponse -> TestTree
testBatchGetItemResponse = resp
    "BatchGetItemResponse"
    "fixture/BatchGetItemResponse"
    (Proxy :: Proxy BatchGetItem)

testDescribeTableResponse :: DescribeTableResponse -> TestTree
testDescribeTableResponse = resp
    "DescribeTableResponse"
    "fixture/DescribeTableResponse"
    (Proxy :: Proxy DescribeTable)

testGetItemResponse :: GetItemResponse -> TestTree
testGetItemResponse = resp
    "GetItemResponse"
    "fixture/GetItemResponse"
    (Proxy :: Proxy GetItem)

testBatchWriteItemResponse :: BatchWriteItemResponse -> TestTree
testBatchWriteItemResponse = resp
    "BatchWriteItemResponse"
    "fixture/BatchWriteItemResponse"
    (Proxy :: Proxy BatchWriteItem)

testScanResponse :: ScanResponse -> TestTree
testScanResponse = resp
    "ScanResponse"
    "fixture/ScanResponse"
    (Proxy :: Proxy Scan)

testListTablesResponse :: ListTablesResponse -> TestTree
testListTablesResponse = resp
    "ListTablesResponse"
    "fixture/ListTablesResponse"
    (Proxy :: Proxy ListTables)

testQueryResponse :: QueryResponse -> TestTree
testQueryResponse = resp
    "QueryResponse"
    "fixture/QueryResponse"
    (Proxy :: Proxy Query)

testCreateTableResponse :: CreateTableResponse -> TestTree
testCreateTableResponse = resp
    "CreateTableResponse"
    "fixture/CreateTableResponse"
    (Proxy :: Proxy CreateTable)
