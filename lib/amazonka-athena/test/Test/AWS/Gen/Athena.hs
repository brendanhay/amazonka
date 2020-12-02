{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Athena
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--         [ requestListDatabases $
--             listDatabases
--
--         , requestDeleteWorkGroup $
--             deleteWorkGroup
--
--         , requestUpdateWorkGroup $
--             updateWorkGroup
--
--         , requestGetNamedQuery $
--             getNamedQuery
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestDeleteDataCatalog $
--             deleteDataCatalog
--
--         , requestUpdateDataCatalog $
--             updateDataCatalog
--
--         , requestListDataCatalogs $
--             listDataCatalogs
--
--         , requestCreateNamedQuery $
--             createNamedQuery
--
--         , requestGetTableMetadata $
--             getTableMetadata
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
--         , requestCreateDataCatalog $
--             createDataCatalog
--
--         , requestListWorkGroups $
--             listWorkGroups
--
--         , requestCreateWorkGroup $
--             createWorkGroup
--
--         , requestBatchGetQueryExecution $
--             batchGetQueryExecution
--
--         , requestGetDataCatalog $
--             getDataCatalog
--
--         , requestStopQueryExecution $
--             stopQueryExecution
--
--         , requestTagResource $
--             tagResource
--
--         , requestGetWorkGroup $
--             getWorkGroup
--
--         , requestGetDatabase $
--             getDatabase
--
--         , requestUntagResource $
--             untagResource
--
--         , requestGetQueryResults $
--             getQueryResults
--
--         , requestListTableMetadata $
--             listTableMetadata
--
--         , requestListQueryExecutions $
--             listQueryExecutions
--
--           ]

--     , testGroup "response"
--         [ responseListDatabases $
--             listDatabasesResponse
--
--         , responseDeleteWorkGroup $
--             deleteWorkGroupResponse
--
--         , responseUpdateWorkGroup $
--             updateWorkGroupResponse
--
--         , responseGetNamedQuery $
--             getNamedQueryResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseDeleteDataCatalog $
--             deleteDataCatalogResponse
--
--         , responseUpdateDataCatalog $
--             updateDataCatalogResponse
--
--         , responseListDataCatalogs $
--             listDataCatalogsResponse
--
--         , responseCreateNamedQuery $
--             createNamedQueryResponse
--
--         , responseGetTableMetadata $
--             getTableMetadataResponse
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
--         , responseCreateDataCatalog $
--             createDataCatalogResponse
--
--         , responseListWorkGroups $
--             listWorkGroupsResponse
--
--         , responseCreateWorkGroup $
--             createWorkGroupResponse
--
--         , responseBatchGetQueryExecution $
--             batchGetQueryExecutionResponse
--
--         , responseGetDataCatalog $
--             getDataCatalogResponse
--
--         , responseStopQueryExecution $
--             stopQueryExecutionResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseGetWorkGroup $
--             getWorkGroupResponse
--
--         , responseGetDatabase $
--             getDatabaseResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseGetQueryResults $
--             getQueryResultsResponse
--
--         , responseListTableMetadata $
--             listTableMetadataResponse
--
--         , responseListQueryExecutions $
--             listQueryExecutionsResponse
--
--           ]
--     ]

-- Requests

requestListDatabases :: ListDatabases -> TestTree
requestListDatabases =
  req
    "ListDatabases"
    "fixture/ListDatabases.yaml"

requestDeleteWorkGroup :: DeleteWorkGroup -> TestTree
requestDeleteWorkGroup =
  req
    "DeleteWorkGroup"
    "fixture/DeleteWorkGroup.yaml"

requestUpdateWorkGroup :: UpdateWorkGroup -> TestTree
requestUpdateWorkGroup =
  req
    "UpdateWorkGroup"
    "fixture/UpdateWorkGroup.yaml"

requestGetNamedQuery :: GetNamedQuery -> TestTree
requestGetNamedQuery =
  req
    "GetNamedQuery"
    "fixture/GetNamedQuery.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDeleteDataCatalog :: DeleteDataCatalog -> TestTree
requestDeleteDataCatalog =
  req
    "DeleteDataCatalog"
    "fixture/DeleteDataCatalog.yaml"

requestUpdateDataCatalog :: UpdateDataCatalog -> TestTree
requestUpdateDataCatalog =
  req
    "UpdateDataCatalog"
    "fixture/UpdateDataCatalog.yaml"

requestListDataCatalogs :: ListDataCatalogs -> TestTree
requestListDataCatalogs =
  req
    "ListDataCatalogs"
    "fixture/ListDataCatalogs.yaml"

requestCreateNamedQuery :: CreateNamedQuery -> TestTree
requestCreateNamedQuery =
  req
    "CreateNamedQuery"
    "fixture/CreateNamedQuery.yaml"

requestGetTableMetadata :: GetTableMetadata -> TestTree
requestGetTableMetadata =
  req
    "GetTableMetadata"
    "fixture/GetTableMetadata.yaml"

requestListNamedQueries :: ListNamedQueries -> TestTree
requestListNamedQueries =
  req
    "ListNamedQueries"
    "fixture/ListNamedQueries.yaml"

requestDeleteNamedQuery :: DeleteNamedQuery -> TestTree
requestDeleteNamedQuery =
  req
    "DeleteNamedQuery"
    "fixture/DeleteNamedQuery.yaml"

requestStartQueryExecution :: StartQueryExecution -> TestTree
requestStartQueryExecution =
  req
    "StartQueryExecution"
    "fixture/StartQueryExecution.yaml"

requestBatchGetNamedQuery :: BatchGetNamedQuery -> TestTree
requestBatchGetNamedQuery =
  req
    "BatchGetNamedQuery"
    "fixture/BatchGetNamedQuery.yaml"

requestGetQueryExecution :: GetQueryExecution -> TestTree
requestGetQueryExecution =
  req
    "GetQueryExecution"
    "fixture/GetQueryExecution.yaml"

requestCreateDataCatalog :: CreateDataCatalog -> TestTree
requestCreateDataCatalog =
  req
    "CreateDataCatalog"
    "fixture/CreateDataCatalog.yaml"

requestListWorkGroups :: ListWorkGroups -> TestTree
requestListWorkGroups =
  req
    "ListWorkGroups"
    "fixture/ListWorkGroups.yaml"

requestCreateWorkGroup :: CreateWorkGroup -> TestTree
requestCreateWorkGroup =
  req
    "CreateWorkGroup"
    "fixture/CreateWorkGroup.yaml"

requestBatchGetQueryExecution :: BatchGetQueryExecution -> TestTree
requestBatchGetQueryExecution =
  req
    "BatchGetQueryExecution"
    "fixture/BatchGetQueryExecution.yaml"

requestGetDataCatalog :: GetDataCatalog -> TestTree
requestGetDataCatalog =
  req
    "GetDataCatalog"
    "fixture/GetDataCatalog.yaml"

requestStopQueryExecution :: StopQueryExecution -> TestTree
requestStopQueryExecution =
  req
    "StopQueryExecution"
    "fixture/StopQueryExecution.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetWorkGroup :: GetWorkGroup -> TestTree
requestGetWorkGroup =
  req
    "GetWorkGroup"
    "fixture/GetWorkGroup.yaml"

requestGetDatabase :: GetDatabase -> TestTree
requestGetDatabase =
  req
    "GetDatabase"
    "fixture/GetDatabase.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestGetQueryResults :: GetQueryResults -> TestTree
requestGetQueryResults =
  req
    "GetQueryResults"
    "fixture/GetQueryResults.yaml"

requestListTableMetadata :: ListTableMetadata -> TestTree
requestListTableMetadata =
  req
    "ListTableMetadata"
    "fixture/ListTableMetadata.yaml"

requestListQueryExecutions :: ListQueryExecutions -> TestTree
requestListQueryExecutions =
  req
    "ListQueryExecutions"
    "fixture/ListQueryExecutions.yaml"

-- Responses

responseListDatabases :: ListDatabasesResponse -> TestTree
responseListDatabases =
  res
    "ListDatabasesResponse"
    "fixture/ListDatabasesResponse.proto"
    athena
    (Proxy :: Proxy ListDatabases)

responseDeleteWorkGroup :: DeleteWorkGroupResponse -> TestTree
responseDeleteWorkGroup =
  res
    "DeleteWorkGroupResponse"
    "fixture/DeleteWorkGroupResponse.proto"
    athena
    (Proxy :: Proxy DeleteWorkGroup)

responseUpdateWorkGroup :: UpdateWorkGroupResponse -> TestTree
responseUpdateWorkGroup =
  res
    "UpdateWorkGroupResponse"
    "fixture/UpdateWorkGroupResponse.proto"
    athena
    (Proxy :: Proxy UpdateWorkGroup)

responseGetNamedQuery :: GetNamedQueryResponse -> TestTree
responseGetNamedQuery =
  res
    "GetNamedQueryResponse"
    "fixture/GetNamedQueryResponse.proto"
    athena
    (Proxy :: Proxy GetNamedQuery)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    athena
    (Proxy :: Proxy ListTagsForResource)

responseDeleteDataCatalog :: DeleteDataCatalogResponse -> TestTree
responseDeleteDataCatalog =
  res
    "DeleteDataCatalogResponse"
    "fixture/DeleteDataCatalogResponse.proto"
    athena
    (Proxy :: Proxy DeleteDataCatalog)

responseUpdateDataCatalog :: UpdateDataCatalogResponse -> TestTree
responseUpdateDataCatalog =
  res
    "UpdateDataCatalogResponse"
    "fixture/UpdateDataCatalogResponse.proto"
    athena
    (Proxy :: Proxy UpdateDataCatalog)

responseListDataCatalogs :: ListDataCatalogsResponse -> TestTree
responseListDataCatalogs =
  res
    "ListDataCatalogsResponse"
    "fixture/ListDataCatalogsResponse.proto"
    athena
    (Proxy :: Proxy ListDataCatalogs)

responseCreateNamedQuery :: CreateNamedQueryResponse -> TestTree
responseCreateNamedQuery =
  res
    "CreateNamedQueryResponse"
    "fixture/CreateNamedQueryResponse.proto"
    athena
    (Proxy :: Proxy CreateNamedQuery)

responseGetTableMetadata :: GetTableMetadataResponse -> TestTree
responseGetTableMetadata =
  res
    "GetTableMetadataResponse"
    "fixture/GetTableMetadataResponse.proto"
    athena
    (Proxy :: Proxy GetTableMetadata)

responseListNamedQueries :: ListNamedQueriesResponse -> TestTree
responseListNamedQueries =
  res
    "ListNamedQueriesResponse"
    "fixture/ListNamedQueriesResponse.proto"
    athena
    (Proxy :: Proxy ListNamedQueries)

responseDeleteNamedQuery :: DeleteNamedQueryResponse -> TestTree
responseDeleteNamedQuery =
  res
    "DeleteNamedQueryResponse"
    "fixture/DeleteNamedQueryResponse.proto"
    athena
    (Proxy :: Proxy DeleteNamedQuery)

responseStartQueryExecution :: StartQueryExecutionResponse -> TestTree
responseStartQueryExecution =
  res
    "StartQueryExecutionResponse"
    "fixture/StartQueryExecutionResponse.proto"
    athena
    (Proxy :: Proxy StartQueryExecution)

responseBatchGetNamedQuery :: BatchGetNamedQueryResponse -> TestTree
responseBatchGetNamedQuery =
  res
    "BatchGetNamedQueryResponse"
    "fixture/BatchGetNamedQueryResponse.proto"
    athena
    (Proxy :: Proxy BatchGetNamedQuery)

responseGetQueryExecution :: GetQueryExecutionResponse -> TestTree
responseGetQueryExecution =
  res
    "GetQueryExecutionResponse"
    "fixture/GetQueryExecutionResponse.proto"
    athena
    (Proxy :: Proxy GetQueryExecution)

responseCreateDataCatalog :: CreateDataCatalogResponse -> TestTree
responseCreateDataCatalog =
  res
    "CreateDataCatalogResponse"
    "fixture/CreateDataCatalogResponse.proto"
    athena
    (Proxy :: Proxy CreateDataCatalog)

responseListWorkGroups :: ListWorkGroupsResponse -> TestTree
responseListWorkGroups =
  res
    "ListWorkGroupsResponse"
    "fixture/ListWorkGroupsResponse.proto"
    athena
    (Proxy :: Proxy ListWorkGroups)

responseCreateWorkGroup :: CreateWorkGroupResponse -> TestTree
responseCreateWorkGroup =
  res
    "CreateWorkGroupResponse"
    "fixture/CreateWorkGroupResponse.proto"
    athena
    (Proxy :: Proxy CreateWorkGroup)

responseBatchGetQueryExecution :: BatchGetQueryExecutionResponse -> TestTree
responseBatchGetQueryExecution =
  res
    "BatchGetQueryExecutionResponse"
    "fixture/BatchGetQueryExecutionResponse.proto"
    athena
    (Proxy :: Proxy BatchGetQueryExecution)

responseGetDataCatalog :: GetDataCatalogResponse -> TestTree
responseGetDataCatalog =
  res
    "GetDataCatalogResponse"
    "fixture/GetDataCatalogResponse.proto"
    athena
    (Proxy :: Proxy GetDataCatalog)

responseStopQueryExecution :: StopQueryExecutionResponse -> TestTree
responseStopQueryExecution =
  res
    "StopQueryExecutionResponse"
    "fixture/StopQueryExecutionResponse.proto"
    athena
    (Proxy :: Proxy StopQueryExecution)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    athena
    (Proxy :: Proxy TagResource)

responseGetWorkGroup :: GetWorkGroupResponse -> TestTree
responseGetWorkGroup =
  res
    "GetWorkGroupResponse"
    "fixture/GetWorkGroupResponse.proto"
    athena
    (Proxy :: Proxy GetWorkGroup)

responseGetDatabase :: GetDatabaseResponse -> TestTree
responseGetDatabase =
  res
    "GetDatabaseResponse"
    "fixture/GetDatabaseResponse.proto"
    athena
    (Proxy :: Proxy GetDatabase)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    athena
    (Proxy :: Proxy UntagResource)

responseGetQueryResults :: GetQueryResultsResponse -> TestTree
responseGetQueryResults =
  res
    "GetQueryResultsResponse"
    "fixture/GetQueryResultsResponse.proto"
    athena
    (Proxy :: Proxy GetQueryResults)

responseListTableMetadata :: ListTableMetadataResponse -> TestTree
responseListTableMetadata =
  res
    "ListTableMetadataResponse"
    "fixture/ListTableMetadataResponse.proto"
    athena
    (Proxy :: Proxy ListTableMetadata)

responseListQueryExecutions :: ListQueryExecutionsResponse -> TestTree
responseListQueryExecutions =
  res
    "ListQueryExecutionsResponse"
    "fixture/ListQueryExecutionsResponse.proto"
    athena
    (Proxy :: Proxy ListQueryExecutions)
