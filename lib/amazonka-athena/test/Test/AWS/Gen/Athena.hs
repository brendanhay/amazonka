{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Athena
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Athena where

import Data.Proxy
import Network.AWS.Athena
import Test.AWS.Athena.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetNamedQuery $
--             getNamedQuery
--
--         , requestCreateNamedQuery $
--             createNamedQuery
--
--         , requestListNamedQueries $
--             listNamedQueries
--
--         , requestDeleteNamedQuery $
--             deleteNamedQuery
--
--         , requestStartQueryExecution $
--             startQueryExecution
--
--         , requestBatchGetNamedQuery $
--             batchGetNamedQuery
--
--         , requestGetQueryExecution $
--             getQueryExecution
--
--         , requestBatchGetQueryExecution $
--             batchGetQueryExecution
--
--         , requestStopQueryExecution $
--             stopQueryExecution
--
--         , requestGetQueryResults $
--             getQueryResults
--
--         , requestListQueryExecutions $
--             listQueryExecutions
--
--           ]

--     , testGroup "response"
--         [ responseGetNamedQuery $
--             getNamedQueryResponse
--
--         , responseCreateNamedQuery $
--             createNamedQueryResponse
--
--         , responseListNamedQueries $
--             listNamedQueriesResponse
--
--         , responseDeleteNamedQuery $
--             deleteNamedQueryResponse
--
--         , responseStartQueryExecution $
--             startQueryExecutionResponse
--
--         , responseBatchGetNamedQuery $
--             batchGetNamedQueryResponse
--
--         , responseGetQueryExecution $
--             getQueryExecutionResponse
--
--         , responseBatchGetQueryExecution $
--             batchGetQueryExecutionResponse
--
--         , responseStopQueryExecution $
--             stopQueryExecutionResponse
--
--         , responseGetQueryResults $
--             getQueryResultsResponse
--
--         , responseListQueryExecutions $
--             listQueryExecutionsResponse
--
--           ]
--     ]

-- Requests

requestGetNamedQuery :: GetNamedQuery -> TestTree
requestGetNamedQuery = req
    "GetNamedQuery"
    "fixture/GetNamedQuery.yaml"

requestCreateNamedQuery :: CreateNamedQuery -> TestTree
requestCreateNamedQuery = req
    "CreateNamedQuery"
    "fixture/CreateNamedQuery.yaml"

requestListNamedQueries :: ListNamedQueries -> TestTree
requestListNamedQueries = req
    "ListNamedQueries"
    "fixture/ListNamedQueries.yaml"

requestDeleteNamedQuery :: DeleteNamedQuery -> TestTree
requestDeleteNamedQuery = req
    "DeleteNamedQuery"
    "fixture/DeleteNamedQuery.yaml"

requestStartQueryExecution :: StartQueryExecution -> TestTree
requestStartQueryExecution = req
    "StartQueryExecution"
    "fixture/StartQueryExecution.yaml"

requestBatchGetNamedQuery :: BatchGetNamedQuery -> TestTree
requestBatchGetNamedQuery = req
    "BatchGetNamedQuery"
    "fixture/BatchGetNamedQuery.yaml"

requestGetQueryExecution :: GetQueryExecution -> TestTree
requestGetQueryExecution = req
    "GetQueryExecution"
    "fixture/GetQueryExecution.yaml"

requestBatchGetQueryExecution :: BatchGetQueryExecution -> TestTree
requestBatchGetQueryExecution = req
    "BatchGetQueryExecution"
    "fixture/BatchGetQueryExecution.yaml"

requestStopQueryExecution :: StopQueryExecution -> TestTree
requestStopQueryExecution = req
    "StopQueryExecution"
    "fixture/StopQueryExecution.yaml"

requestGetQueryResults :: GetQueryResults -> TestTree
requestGetQueryResults = req
    "GetQueryResults"
    "fixture/GetQueryResults.yaml"

requestListQueryExecutions :: ListQueryExecutions -> TestTree
requestListQueryExecutions = req
    "ListQueryExecutions"
    "fixture/ListQueryExecutions.yaml"

-- Responses

responseGetNamedQuery :: GetNamedQueryResponse -> TestTree
responseGetNamedQuery = res
    "GetNamedQueryResponse"
    "fixture/GetNamedQueryResponse.proto"
    athena
    (Proxy :: Proxy GetNamedQuery)

responseCreateNamedQuery :: CreateNamedQueryResponse -> TestTree
responseCreateNamedQuery = res
    "CreateNamedQueryResponse"
    "fixture/CreateNamedQueryResponse.proto"
    athena
    (Proxy :: Proxy CreateNamedQuery)

responseListNamedQueries :: ListNamedQueriesResponse -> TestTree
responseListNamedQueries = res
    "ListNamedQueriesResponse"
    "fixture/ListNamedQueriesResponse.proto"
    athena
    (Proxy :: Proxy ListNamedQueries)

responseDeleteNamedQuery :: DeleteNamedQueryResponse -> TestTree
responseDeleteNamedQuery = res
    "DeleteNamedQueryResponse"
    "fixture/DeleteNamedQueryResponse.proto"
    athena
    (Proxy :: Proxy DeleteNamedQuery)

responseStartQueryExecution :: StartQueryExecutionResponse -> TestTree
responseStartQueryExecution = res
    "StartQueryExecutionResponse"
    "fixture/StartQueryExecutionResponse.proto"
    athena
    (Proxy :: Proxy StartQueryExecution)

responseBatchGetNamedQuery :: BatchGetNamedQueryResponse -> TestTree
responseBatchGetNamedQuery = res
    "BatchGetNamedQueryResponse"
    "fixture/BatchGetNamedQueryResponse.proto"
    athena
    (Proxy :: Proxy BatchGetNamedQuery)

responseGetQueryExecution :: GetQueryExecutionResponse -> TestTree
responseGetQueryExecution = res
    "GetQueryExecutionResponse"
    "fixture/GetQueryExecutionResponse.proto"
    athena
    (Proxy :: Proxy GetQueryExecution)

responseBatchGetQueryExecution :: BatchGetQueryExecutionResponse -> TestTree
responseBatchGetQueryExecution = res
    "BatchGetQueryExecutionResponse"
    "fixture/BatchGetQueryExecutionResponse.proto"
    athena
    (Proxy :: Proxy BatchGetQueryExecution)

responseStopQueryExecution :: StopQueryExecutionResponse -> TestTree
responseStopQueryExecution = res
    "StopQueryExecutionResponse"
    "fixture/StopQueryExecutionResponse.proto"
    athena
    (Proxy :: Proxy StopQueryExecution)

responseGetQueryResults :: GetQueryResultsResponse -> TestTree
responseGetQueryResults = res
    "GetQueryResultsResponse"
    "fixture/GetQueryResultsResponse.proto"
    athena
    (Proxy :: Proxy GetQueryResults)

responseListQueryExecutions :: ListQueryExecutionsResponse -> TestTree
responseListQueryExecutions = res
    "ListQueryExecutionsResponse"
    "fixture/ListQueryExecutionsResponse.proto"
    athena
    (Proxy :: Proxy ListQueryExecutions)
