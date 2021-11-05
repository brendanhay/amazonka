{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.RedshiftData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.RedshiftData where

import qualified Data.Proxy as Proxy
import Network.AWS.RedshiftData
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.RedshiftData.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListStatements $
--             newListStatements
--
--         , requestListDatabases $
--             newListDatabases
--
--         , requestListSchemas $
--             newListSchemas
--
--         , requestDescribeStatement $
--             newDescribeStatement
--
--         , requestCancelStatement $
--             newCancelStatement
--
--         , requestDescribeTable $
--             newDescribeTable
--
--         , requestBatchExecuteStatement $
--             newBatchExecuteStatement
--
--         , requestListTables $
--             newListTables
--
--         , requestExecuteStatement $
--             newExecuteStatement
--
--         , requestGetStatementResult $
--             newGetStatementResult
--
--           ]

--     , testGroup "response"
--         [ responseListStatements $
--             newListStatementsResponse
--
--         , responseListDatabases $
--             newListDatabasesResponse
--
--         , responseListSchemas $
--             newListSchemasResponse
--
--         , responseDescribeStatement $
--             newDescribeStatementResponse
--
--         , responseCancelStatement $
--             newCancelStatementResponse
--
--         , responseDescribeTable $
--             newDescribeTableResponse
--
--         , responseBatchExecuteStatement $
--             newBatchExecuteStatementResponse
--
--         , responseListTables $
--             newListTablesResponse
--
--         , responseExecuteStatement $
--             newExecuteStatementResponse
--
--         , responseGetStatementResult $
--             newGetStatementResultResponse
--
--           ]
--     ]

-- Requests

requestListStatements :: ListStatements -> TestTree
requestListStatements =
  req
    "ListStatements"
    "fixture/ListStatements.yaml"

requestListDatabases :: ListDatabases -> TestTree
requestListDatabases =
  req
    "ListDatabases"
    "fixture/ListDatabases.yaml"

requestListSchemas :: ListSchemas -> TestTree
requestListSchemas =
  req
    "ListSchemas"
    "fixture/ListSchemas.yaml"

requestDescribeStatement :: DescribeStatement -> TestTree
requestDescribeStatement =
  req
    "DescribeStatement"
    "fixture/DescribeStatement.yaml"

requestCancelStatement :: CancelStatement -> TestTree
requestCancelStatement =
  req
    "CancelStatement"
    "fixture/CancelStatement.yaml"

requestDescribeTable :: DescribeTable -> TestTree
requestDescribeTable =
  req
    "DescribeTable"
    "fixture/DescribeTable.yaml"

requestBatchExecuteStatement :: BatchExecuteStatement -> TestTree
requestBatchExecuteStatement =
  req
    "BatchExecuteStatement"
    "fixture/BatchExecuteStatement.yaml"

requestListTables :: ListTables -> TestTree
requestListTables =
  req
    "ListTables"
    "fixture/ListTables.yaml"

requestExecuteStatement :: ExecuteStatement -> TestTree
requestExecuteStatement =
  req
    "ExecuteStatement"
    "fixture/ExecuteStatement.yaml"

requestGetStatementResult :: GetStatementResult -> TestTree
requestGetStatementResult =
  req
    "GetStatementResult"
    "fixture/GetStatementResult.yaml"

-- Responses

responseListStatements :: ListStatementsResponse -> TestTree
responseListStatements =
  res
    "ListStatementsResponse"
    "fixture/ListStatementsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStatements)

responseListDatabases :: ListDatabasesResponse -> TestTree
responseListDatabases =
  res
    "ListDatabasesResponse"
    "fixture/ListDatabasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatabases)

responseListSchemas :: ListSchemasResponse -> TestTree
responseListSchemas =
  res
    "ListSchemasResponse"
    "fixture/ListSchemasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSchemas)

responseDescribeStatement :: DescribeStatementResponse -> TestTree
responseDescribeStatement =
  res
    "DescribeStatementResponse"
    "fixture/DescribeStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStatement)

responseCancelStatement :: CancelStatementResponse -> TestTree
responseCancelStatement =
  res
    "CancelStatementResponse"
    "fixture/CancelStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelStatement)

responseDescribeTable :: DescribeTableResponse -> TestTree
responseDescribeTable =
  res
    "DescribeTableResponse"
    "fixture/DescribeTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTable)

responseBatchExecuteStatement :: BatchExecuteStatementResponse -> TestTree
responseBatchExecuteStatement =
  res
    "BatchExecuteStatementResponse"
    "fixture/BatchExecuteStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchExecuteStatement)

responseListTables :: ListTablesResponse -> TestTree
responseListTables =
  res
    "ListTablesResponse"
    "fixture/ListTablesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTables)

responseExecuteStatement :: ExecuteStatementResponse -> TestTree
responseExecuteStatement =
  res
    "ExecuteStatementResponse"
    "fixture/ExecuteStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExecuteStatement)

responseGetStatementResult :: GetStatementResultResponse -> TestTree
responseGetStatementResult =
  res
    "GetStatementResultResponse"
    "fixture/GetStatementResultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStatementResult)
