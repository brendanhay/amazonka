{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DynamoDB
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.DynamoDB where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.DynamoDB
import Test.AWS.DynamoDB.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestPutItem $
--             putItem
--
--         , requestDeleteItem $
--             deleteItem
--
--         , requestUpdateItem $
--             updateItem
--
--         , requestDeleteTable $
--             deleteTable
--
--         , requestUpdateTable $
--             updateTable
--
--         , requestBatchGetItem $
--             batchGetItem
--
--         , requestDescribeTable $
--             describeTable
--
--         , requestDescribeLimits $
--             describeLimits
--
--         , requestGetItem $
--             getItem
--
--         , requestBatchWriteItem $
--             batchWriteItem
--
--         , requestListTables $
--             listTables
--
--         , requestScan $
--             scan
--
--         , requestQuery $
--             query
--
--         , requestCreateTable $
--             createTable
--
--           ]

--     , testGroup "response"
--         [ responsePutItem $
--             putItemResponse
--
--         , responseDeleteItem $
--             deleteItemResponse
--
--         , responseUpdateItem $
--             updateItemResponse
--
--         , responseDeleteTable $
--             deleteTableResponse
--
--         , responseUpdateTable $
--             updateTableResponse
--
--         , responseBatchGetItem $
--             batchGetItemResponse
--
--         , responseDescribeTable $
--             describeTableResponse
--
--         , responseDescribeLimits $
--             describeLimitsResponse
--
--         , responseGetItem $
--             getItemResponse
--
--         , responseBatchWriteItem $
--             batchWriteItemResponse
--
--         , responseListTables $
--             listTablesResponse
--
--         , responseScan $
--             scanResponse
--
--         , responseQuery $
--             queryResponse
--
--         , responseCreateTable $
--             createTableResponse
--
--           ]
--     ]

-- Requests

requestPutItem :: PutItem -> TestTree
requestPutItem = req
    "PutItem"
    "fixture/PutItem.yaml"

requestDeleteItem :: DeleteItem -> TestTree
requestDeleteItem = req
    "DeleteItem"
    "fixture/DeleteItem.yaml"

requestUpdateItem :: UpdateItem -> TestTree
requestUpdateItem = req
    "UpdateItem"
    "fixture/UpdateItem.yaml"

requestDeleteTable :: DeleteTable -> TestTree
requestDeleteTable = req
    "DeleteTable"
    "fixture/DeleteTable.yaml"

requestUpdateTable :: UpdateTable -> TestTree
requestUpdateTable = req
    "UpdateTable"
    "fixture/UpdateTable.yaml"

requestBatchGetItem :: BatchGetItem -> TestTree
requestBatchGetItem = req
    "BatchGetItem"
    "fixture/BatchGetItem.yaml"

requestDescribeTable :: DescribeTable -> TestTree
requestDescribeTable = req
    "DescribeTable"
    "fixture/DescribeTable.yaml"

requestDescribeLimits :: DescribeLimits -> TestTree
requestDescribeLimits = req
    "DescribeLimits"
    "fixture/DescribeLimits.yaml"

requestGetItem :: GetItem -> TestTree
requestGetItem = req
    "GetItem"
    "fixture/GetItem.yaml"

requestBatchWriteItem :: BatchWriteItem -> TestTree
requestBatchWriteItem = req
    "BatchWriteItem"
    "fixture/BatchWriteItem.yaml"

requestListTables :: ListTables -> TestTree
requestListTables = req
    "ListTables"
    "fixture/ListTables.yaml"

requestScan :: Scan -> TestTree
requestScan = req
    "Scan"
    "fixture/Scan.yaml"

requestQuery :: Query -> TestTree
requestQuery = req
    "Query"
    "fixture/Query.yaml"

requestCreateTable :: CreateTable -> TestTree
requestCreateTable = req
    "CreateTable"
    "fixture/CreateTable.yaml"

-- Responses

responsePutItem :: PutItemResponse -> TestTree
responsePutItem = res
    "PutItemResponse"
    "fixture/PutItemResponse.proto"
    dynamoDB
    (Proxy :: Proxy PutItem)

responseDeleteItem :: DeleteItemResponse -> TestTree
responseDeleteItem = res
    "DeleteItemResponse"
    "fixture/DeleteItemResponse.proto"
    dynamoDB
    (Proxy :: Proxy DeleteItem)

responseUpdateItem :: UpdateItemResponse -> TestTree
responseUpdateItem = res
    "UpdateItemResponse"
    "fixture/UpdateItemResponse.proto"
    dynamoDB
    (Proxy :: Proxy UpdateItem)

responseDeleteTable :: DeleteTableResponse -> TestTree
responseDeleteTable = res
    "DeleteTableResponse"
    "fixture/DeleteTableResponse.proto"
    dynamoDB
    (Proxy :: Proxy DeleteTable)

responseUpdateTable :: UpdateTableResponse -> TestTree
responseUpdateTable = res
    "UpdateTableResponse"
    "fixture/UpdateTableResponse.proto"
    dynamoDB
    (Proxy :: Proxy UpdateTable)

responseBatchGetItem :: BatchGetItemResponse -> TestTree
responseBatchGetItem = res
    "BatchGetItemResponse"
    "fixture/BatchGetItemResponse.proto"
    dynamoDB
    (Proxy :: Proxy BatchGetItem)

responseDescribeTable :: DescribeTableResponse -> TestTree
responseDescribeTable = res
    "DescribeTableResponse"
    "fixture/DescribeTableResponse.proto"
    dynamoDB
    (Proxy :: Proxy DescribeTable)

responseDescribeLimits :: DescribeLimitsResponse -> TestTree
responseDescribeLimits = res
    "DescribeLimitsResponse"
    "fixture/DescribeLimitsResponse.proto"
    dynamoDB
    (Proxy :: Proxy DescribeLimits)

responseGetItem :: GetItemResponse -> TestTree
responseGetItem = res
    "GetItemResponse"
    "fixture/GetItemResponse.proto"
    dynamoDB
    (Proxy :: Proxy GetItem)

responseBatchWriteItem :: BatchWriteItemResponse -> TestTree
responseBatchWriteItem = res
    "BatchWriteItemResponse"
    "fixture/BatchWriteItemResponse.proto"
    dynamoDB
    (Proxy :: Proxy BatchWriteItem)

responseListTables :: ListTablesResponse -> TestTree
responseListTables = res
    "ListTablesResponse"
    "fixture/ListTablesResponse.proto"
    dynamoDB
    (Proxy :: Proxy ListTables)

responseScan :: ScanResponse -> TestTree
responseScan = res
    "ScanResponse"
    "fixture/ScanResponse.proto"
    dynamoDB
    (Proxy :: Proxy Scan)

responseQuery :: QueryResponse -> TestTree
responseQuery = res
    "QueryResponse"
    "fixture/QueryResponse.proto"
    dynamoDB
    (Proxy :: Proxy Query)

responseCreateTable :: CreateTableResponse -> TestTree
responseCreateTable = res
    "CreateTableResponse"
    "fixture/CreateTableResponse.proto"
    dynamoDB
    (Proxy :: Proxy CreateTable)
