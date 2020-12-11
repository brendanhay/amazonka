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
--             mkListDatabases
--
--         , requestDeleteWorkGroup $
--             mkDeleteWorkGroup
--
--         , requestUpdateWorkGroup $
--             mkUpdateWorkGroup
--
--         , requestGetNamedQuery $
--             mkGetNamedQuery
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestDeleteDataCatalog $
--             mkDeleteDataCatalog
--
--         , requestUpdateDataCatalog $
--             mkUpdateDataCatalog
--
--         , requestListDataCatalogs $
--             mkListDataCatalogs
--
--         , requestCreateNamedQuery $
--             mkCreateNamedQuery
--
--         , requestGetTableMetadata $
--             mkGetTableMetadata
--
--         , requestListNamedQueries $
--             mkListNamedQueries
--
--         , requestDeleteNamedQuery $
--             mkDeleteNamedQuery
--
--         , requestStartQueryExecution $
--             mkStartQueryExecution
--
--         , requestBatchGetNamedQuery $
--             mkBatchGetNamedQuery
--
--         , requestGetQueryExecution $
--             mkGetQueryExecution
--
--         , requestCreateDataCatalog $
--             mkCreateDataCatalog
--
--         , requestListWorkGroups $
--             mkListWorkGroups
--
--         , requestCreateWorkGroup $
--             mkCreateWorkGroup
--
--         , requestBatchGetQueryExecution $
--             mkBatchGetQueryExecution
--
--         , requestGetDataCatalog $
--             mkGetDataCatalog
--
--         , requestStopQueryExecution $
--             mkStopQueryExecution
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestGetWorkGroup $
--             mkGetWorkGroup
--
--         , requestGetDatabase $
--             mkGetDatabase
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestGetQueryResults $
--             mkGetQueryResults
--
--         , requestListTableMetadata $
--             mkListTableMetadata
--
--         , requestListQueryExecutions $
--             mkListQueryExecutions
--
--           ]

--     , testGroup "response"
--         [ responseListDatabases $
--             mkListDatabasesResponse
--
--         , responseDeleteWorkGroup $
--             mkDeleteWorkGroupResponse
--
--         , responseUpdateWorkGroup $
--             mkUpdateWorkGroupResponse
--
--         , responseGetNamedQuery $
--             mkGetNamedQueryResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseDeleteDataCatalog $
--             mkDeleteDataCatalogResponse
--
--         , responseUpdateDataCatalog $
--             mkUpdateDataCatalogResponse
--
--         , responseListDataCatalogs $
--             mkListDataCatalogsResponse
--
--         , responseCreateNamedQuery $
--             mkCreateNamedQueryResponse
--
--         , responseGetTableMetadata $
--             mkGetTableMetadataResponse
--
--         , responseListNamedQueries $
--             mkListNamedQueriesResponse
--
--         , responseDeleteNamedQuery $
--             mkDeleteNamedQueryResponse
--
--         , responseStartQueryExecution $
--             mkStartQueryExecutionResponse
--
--         , responseBatchGetNamedQuery $
--             mkBatchGetNamedQueryResponse
--
--         , responseGetQueryExecution $
--             mkGetQueryExecutionResponse
--
--         , responseCreateDataCatalog $
--             mkCreateDataCatalogResponse
--
--         , responseListWorkGroups $
--             mkListWorkGroupsResponse
--
--         , responseCreateWorkGroup $
--             mkCreateWorkGroupResponse
--
--         , responseBatchGetQueryExecution $
--             mkBatchGetQueryExecutionResponse
--
--         , responseGetDataCatalog $
--             mkGetDataCatalogResponse
--
--         , responseStopQueryExecution $
--             mkStopQueryExecutionResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseGetWorkGroup $
--             mkGetWorkGroupResponse
--
--         , responseGetDatabase $
--             mkGetDatabaseResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseGetQueryResults $
--             mkGetQueryResultsResponse
--
--         , responseListTableMetadata $
--             mkListTableMetadataResponse
--
--         , responseListQueryExecutions $
--             mkListQueryExecutionsResponse
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
    athenaService
    (Proxy :: Proxy ListDatabases)

responseDeleteWorkGroup :: DeleteWorkGroupResponse -> TestTree
responseDeleteWorkGroup =
  res
    "DeleteWorkGroupResponse"
    "fixture/DeleteWorkGroupResponse.proto"
    athenaService
    (Proxy :: Proxy DeleteWorkGroup)

responseUpdateWorkGroup :: UpdateWorkGroupResponse -> TestTree
responseUpdateWorkGroup =
  res
    "UpdateWorkGroupResponse"
    "fixture/UpdateWorkGroupResponse.proto"
    athenaService
    (Proxy :: Proxy UpdateWorkGroup)

responseGetNamedQuery :: GetNamedQueryResponse -> TestTree
responseGetNamedQuery =
  res
    "GetNamedQueryResponse"
    "fixture/GetNamedQueryResponse.proto"
    athenaService
    (Proxy :: Proxy GetNamedQuery)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    athenaService
    (Proxy :: Proxy ListTagsForResource)

responseDeleteDataCatalog :: DeleteDataCatalogResponse -> TestTree
responseDeleteDataCatalog =
  res
    "DeleteDataCatalogResponse"
    "fixture/DeleteDataCatalogResponse.proto"
    athenaService
    (Proxy :: Proxy DeleteDataCatalog)

responseUpdateDataCatalog :: UpdateDataCatalogResponse -> TestTree
responseUpdateDataCatalog =
  res
    "UpdateDataCatalogResponse"
    "fixture/UpdateDataCatalogResponse.proto"
    athenaService
    (Proxy :: Proxy UpdateDataCatalog)

responseListDataCatalogs :: ListDataCatalogsResponse -> TestTree
responseListDataCatalogs =
  res
    "ListDataCatalogsResponse"
    "fixture/ListDataCatalogsResponse.proto"
    athenaService
    (Proxy :: Proxy ListDataCatalogs)

responseCreateNamedQuery :: CreateNamedQueryResponse -> TestTree
responseCreateNamedQuery =
  res
    "CreateNamedQueryResponse"
    "fixture/CreateNamedQueryResponse.proto"
    athenaService
    (Proxy :: Proxy CreateNamedQuery)

responseGetTableMetadata :: GetTableMetadataResponse -> TestTree
responseGetTableMetadata =
  res
    "GetTableMetadataResponse"
    "fixture/GetTableMetadataResponse.proto"
    athenaService
    (Proxy :: Proxy GetTableMetadata)

responseListNamedQueries :: ListNamedQueriesResponse -> TestTree
responseListNamedQueries =
  res
    "ListNamedQueriesResponse"
    "fixture/ListNamedQueriesResponse.proto"
    athenaService
    (Proxy :: Proxy ListNamedQueries)

responseDeleteNamedQuery :: DeleteNamedQueryResponse -> TestTree
responseDeleteNamedQuery =
  res
    "DeleteNamedQueryResponse"
    "fixture/DeleteNamedQueryResponse.proto"
    athenaService
    (Proxy :: Proxy DeleteNamedQuery)

responseStartQueryExecution :: StartQueryExecutionResponse -> TestTree
responseStartQueryExecution =
  res
    "StartQueryExecutionResponse"
    "fixture/StartQueryExecutionResponse.proto"
    athenaService
    (Proxy :: Proxy StartQueryExecution)

responseBatchGetNamedQuery :: BatchGetNamedQueryResponse -> TestTree
responseBatchGetNamedQuery =
  res
    "BatchGetNamedQueryResponse"
    "fixture/BatchGetNamedQueryResponse.proto"
    athenaService
    (Proxy :: Proxy BatchGetNamedQuery)

responseGetQueryExecution :: GetQueryExecutionResponse -> TestTree
responseGetQueryExecution =
  res
    "GetQueryExecutionResponse"
    "fixture/GetQueryExecutionResponse.proto"
    athenaService
    (Proxy :: Proxy GetQueryExecution)

responseCreateDataCatalog :: CreateDataCatalogResponse -> TestTree
responseCreateDataCatalog =
  res
    "CreateDataCatalogResponse"
    "fixture/CreateDataCatalogResponse.proto"
    athenaService
    (Proxy :: Proxy CreateDataCatalog)

responseListWorkGroups :: ListWorkGroupsResponse -> TestTree
responseListWorkGroups =
  res
    "ListWorkGroupsResponse"
    "fixture/ListWorkGroupsResponse.proto"
    athenaService
    (Proxy :: Proxy ListWorkGroups)

responseCreateWorkGroup :: CreateWorkGroupResponse -> TestTree
responseCreateWorkGroup =
  res
    "CreateWorkGroupResponse"
    "fixture/CreateWorkGroupResponse.proto"
    athenaService
    (Proxy :: Proxy CreateWorkGroup)

responseBatchGetQueryExecution :: BatchGetQueryExecutionResponse -> TestTree
responseBatchGetQueryExecution =
  res
    "BatchGetQueryExecutionResponse"
    "fixture/BatchGetQueryExecutionResponse.proto"
    athenaService
    (Proxy :: Proxy BatchGetQueryExecution)

responseGetDataCatalog :: GetDataCatalogResponse -> TestTree
responseGetDataCatalog =
  res
    "GetDataCatalogResponse"
    "fixture/GetDataCatalogResponse.proto"
    athenaService
    (Proxy :: Proxy GetDataCatalog)

responseStopQueryExecution :: StopQueryExecutionResponse -> TestTree
responseStopQueryExecution =
  res
    "StopQueryExecutionResponse"
    "fixture/StopQueryExecutionResponse.proto"
    athenaService
    (Proxy :: Proxy StopQueryExecution)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    athenaService
    (Proxy :: Proxy TagResource)

responseGetWorkGroup :: GetWorkGroupResponse -> TestTree
responseGetWorkGroup =
  res
    "GetWorkGroupResponse"
    "fixture/GetWorkGroupResponse.proto"
    athenaService
    (Proxy :: Proxy GetWorkGroup)

responseGetDatabase :: GetDatabaseResponse -> TestTree
responseGetDatabase =
  res
    "GetDatabaseResponse"
    "fixture/GetDatabaseResponse.proto"
    athenaService
    (Proxy :: Proxy GetDatabase)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    athenaService
    (Proxy :: Proxy UntagResource)

responseGetQueryResults :: GetQueryResultsResponse -> TestTree
responseGetQueryResults =
  res
    "GetQueryResultsResponse"
    "fixture/GetQueryResultsResponse.proto"
    athenaService
    (Proxy :: Proxy GetQueryResults)

responseListTableMetadata :: ListTableMetadataResponse -> TestTree
responseListTableMetadata =
  res
    "ListTableMetadataResponse"
    "fixture/ListTableMetadataResponse.proto"
    athenaService
    (Proxy :: Proxy ListTableMetadata)

responseListQueryExecutions :: ListQueryExecutionsResponse -> TestTree
responseListQueryExecutions =
  res
    "ListQueryExecutionsResponse"
    "fixture/ListQueryExecutionsResponse.proto"
    athenaService
    (Proxy :: Proxy ListQueryExecutions)
