{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.RedshiftData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.RedshiftData where

import Amazonka.RedshiftData
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.RedshiftData.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchExecuteStatement $
--             newBatchExecuteStatement
--
--         , requestCancelStatement $
--             newCancelStatement
--
--         , requestDescribeStatement $
--             newDescribeStatement
--
--         , requestDescribeTable $
--             newDescribeTable
--
--         , requestExecuteStatement $
--             newExecuteStatement
--
--         , requestGetStatementResult $
--             newGetStatementResult
--
--         , requestListDatabases $
--             newListDatabases
--
--         , requestListSchemas $
--             newListSchemas
--
--         , requestListStatements $
--             newListStatements
--
--         , requestListTables $
--             newListTables
--
--           ]

--     , testGroup "response"
--         [ responseBatchExecuteStatement $
--             newBatchExecuteStatementResponse
--
--         , responseCancelStatement $
--             newCancelStatementResponse
--
--         , responseDescribeStatement $
--             newDescribeStatementResponse
--
--         , responseDescribeTable $
--             newDescribeTableResponse
--
--         , responseExecuteStatement $
--             newExecuteStatementResponse
--
--         , responseGetStatementResult $
--             newGetStatementResultResponse
--
--         , responseListDatabases $
--             newListDatabasesResponse
--
--         , responseListSchemas $
--             newListSchemasResponse
--
--         , responseListStatements $
--             newListStatementsResponse
--
--         , responseListTables $
--             newListTablesResponse
--
--           ]
--     ]

-- Requests

requestBatchExecuteStatement :: BatchExecuteStatement -> TestTree
requestBatchExecuteStatement =
  req
    "BatchExecuteStatement"
    "fixture/BatchExecuteStatement.yaml"

requestCancelStatement :: CancelStatement -> TestTree
requestCancelStatement =
  req
    "CancelStatement"
    "fixture/CancelStatement.yaml"

requestDescribeStatement :: DescribeStatement -> TestTree
requestDescribeStatement =
  req
    "DescribeStatement"
    "fixture/DescribeStatement.yaml"

requestDescribeTable :: DescribeTable -> TestTree
requestDescribeTable =
  req
    "DescribeTable"
    "fixture/DescribeTable.yaml"

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

requestListStatements :: ListStatements -> TestTree
requestListStatements =
  req
    "ListStatements"
    "fixture/ListStatements.yaml"

requestListTables :: ListTables -> TestTree
requestListTables =
  req
    "ListTables"
    "fixture/ListTables.yaml"

-- Responses

responseBatchExecuteStatement :: BatchExecuteStatementResponse -> TestTree
responseBatchExecuteStatement =
  res
    "BatchExecuteStatementResponse"
    "fixture/BatchExecuteStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchExecuteStatement)

responseCancelStatement :: CancelStatementResponse -> TestTree
responseCancelStatement =
  res
    "CancelStatementResponse"
    "fixture/CancelStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelStatement)

responseDescribeStatement :: DescribeStatementResponse -> TestTree
responseDescribeStatement =
  res
    "DescribeStatementResponse"
    "fixture/DescribeStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStatement)

responseDescribeTable :: DescribeTableResponse -> TestTree
responseDescribeTable =
  res
    "DescribeTableResponse"
    "fixture/DescribeTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTable)

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

responseListStatements :: ListStatementsResponse -> TestTree
responseListStatements =
  res
    "ListStatementsResponse"
    "fixture/ListStatementsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStatements)

responseListTables :: ListTablesResponse -> TestTree
responseListTables =
  res
    "ListTablesResponse"
    "fixture/ListTablesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTables)
