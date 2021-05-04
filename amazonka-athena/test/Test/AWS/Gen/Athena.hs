{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Athena
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestCreateDataCatalog $
--             newCreateDataCatalog
--
--         , requestListQueryExecutions $
--             newListQueryExecutions
--
--         , requestListTableMetadata $
--             newListTableMetadata
--
--         , requestGetQueryExecution $
--             newGetQueryExecution
--
--         , requestBatchGetNamedQuery $
--             newBatchGetNamedQuery
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestGetDatabase $
--             newGetDatabase
--
--         , requestDeleteNamedQuery $
--             newDeleteNamedQuery
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListEngineVersions $
--             newListEngineVersions
--
--         , requestGetDataCatalog $
--             newGetDataCatalog
--
--         , requestListDataCatalogs $
--             newListDataCatalogs
--
--         , requestCreateWorkGroup $
--             newCreateWorkGroup
--
--         , requestGetNamedQuery $
--             newGetNamedQuery
--
--         , requestUpdateWorkGroup $
--             newUpdateWorkGroup
--
--         , requestDeleteWorkGroup $
--             newDeleteWorkGroup
--
--         , requestListWorkGroups $
--             newListWorkGroups
--
--         , requestListDatabases $
--             newListDatabases
--
--         , requestGetQueryResults $
--             newGetQueryResults
--
--         , requestGetWorkGroup $
--             newGetWorkGroup
--
--         , requestStartQueryExecution $
--             newStartQueryExecution
--
--         , requestStopQueryExecution $
--             newStopQueryExecution
--
--         , requestGetTableMetadata $
--             newGetTableMetadata
--
--         , requestCreateNamedQuery $
--             newCreateNamedQuery
--
--         , requestListNamedQueries $
--             newListNamedQueries
--
--         , requestBatchGetQueryExecution $
--             newBatchGetQueryExecution
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDeleteDataCatalog $
--             newDeleteDataCatalog
--
--         , requestUpdateDataCatalog $
--             newUpdateDataCatalog
--
--           ]

--     , testGroup "response"
--         [ responseCreateDataCatalog $
--             newCreateDataCatalogResponse
--
--         , responseListQueryExecutions $
--             newListQueryExecutionsResponse
--
--         , responseListTableMetadata $
--             newListTableMetadataResponse
--
--         , responseGetQueryExecution $
--             newGetQueryExecutionResponse
--
--         , responseBatchGetNamedQuery $
--             newBatchGetNamedQueryResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseGetDatabase $
--             newGetDatabaseResponse
--
--         , responseDeleteNamedQuery $
--             newDeleteNamedQueryResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListEngineVersions $
--             newListEngineVersionsResponse
--
--         , responseGetDataCatalog $
--             newGetDataCatalogResponse
--
--         , responseListDataCatalogs $
--             newListDataCatalogsResponse
--
--         , responseCreateWorkGroup $
--             newCreateWorkGroupResponse
--
--         , responseGetNamedQuery $
--             newGetNamedQueryResponse
--
--         , responseUpdateWorkGroup $
--             newUpdateWorkGroupResponse
--
--         , responseDeleteWorkGroup $
--             newDeleteWorkGroupResponse
--
--         , responseListWorkGroups $
--             newListWorkGroupsResponse
--
--         , responseListDatabases $
--             newListDatabasesResponse
--
--         , responseGetQueryResults $
--             newGetQueryResultsResponse
--
--         , responseGetWorkGroup $
--             newGetWorkGroupResponse
--
--         , responseStartQueryExecution $
--             newStartQueryExecutionResponse
--
--         , responseStopQueryExecution $
--             newStopQueryExecutionResponse
--
--         , responseGetTableMetadata $
--             newGetTableMetadataResponse
--
--         , responseCreateNamedQuery $
--             newCreateNamedQueryResponse
--
--         , responseListNamedQueries $
--             newListNamedQueriesResponse
--
--         , responseBatchGetQueryExecution $
--             newBatchGetQueryExecutionResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDeleteDataCatalog $
--             newDeleteDataCatalogResponse
--
--         , responseUpdateDataCatalog $
--             newUpdateDataCatalogResponse
--
--           ]
--     ]

-- Requests

requestCreateDataCatalog :: CreateDataCatalog -> TestTree
requestCreateDataCatalog =
  req
    "CreateDataCatalog"
    "fixture/CreateDataCatalog.yaml"

requestListQueryExecutions :: ListQueryExecutions -> TestTree
requestListQueryExecutions =
  req
    "ListQueryExecutions"
    "fixture/ListQueryExecutions.yaml"

requestListTableMetadata :: ListTableMetadata -> TestTree
requestListTableMetadata =
  req
    "ListTableMetadata"
    "fixture/ListTableMetadata.yaml"

requestGetQueryExecution :: GetQueryExecution -> TestTree
requestGetQueryExecution =
  req
    "GetQueryExecution"
    "fixture/GetQueryExecution.yaml"

requestBatchGetNamedQuery :: BatchGetNamedQuery -> TestTree
requestBatchGetNamedQuery =
  req
    "BatchGetNamedQuery"
    "fixture/BatchGetNamedQuery.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestGetDatabase :: GetDatabase -> TestTree
requestGetDatabase =
  req
    "GetDatabase"
    "fixture/GetDatabase.yaml"

requestDeleteNamedQuery :: DeleteNamedQuery -> TestTree
requestDeleteNamedQuery =
  req
    "DeleteNamedQuery"
    "fixture/DeleteNamedQuery.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListEngineVersions :: ListEngineVersions -> TestTree
requestListEngineVersions =
  req
    "ListEngineVersions"
    "fixture/ListEngineVersions.yaml"

requestGetDataCatalog :: GetDataCatalog -> TestTree
requestGetDataCatalog =
  req
    "GetDataCatalog"
    "fixture/GetDataCatalog.yaml"

requestListDataCatalogs :: ListDataCatalogs -> TestTree
requestListDataCatalogs =
  req
    "ListDataCatalogs"
    "fixture/ListDataCatalogs.yaml"

requestCreateWorkGroup :: CreateWorkGroup -> TestTree
requestCreateWorkGroup =
  req
    "CreateWorkGroup"
    "fixture/CreateWorkGroup.yaml"

requestGetNamedQuery :: GetNamedQuery -> TestTree
requestGetNamedQuery =
  req
    "GetNamedQuery"
    "fixture/GetNamedQuery.yaml"

requestUpdateWorkGroup :: UpdateWorkGroup -> TestTree
requestUpdateWorkGroup =
  req
    "UpdateWorkGroup"
    "fixture/UpdateWorkGroup.yaml"

requestDeleteWorkGroup :: DeleteWorkGroup -> TestTree
requestDeleteWorkGroup =
  req
    "DeleteWorkGroup"
    "fixture/DeleteWorkGroup.yaml"

requestListWorkGroups :: ListWorkGroups -> TestTree
requestListWorkGroups =
  req
    "ListWorkGroups"
    "fixture/ListWorkGroups.yaml"

requestListDatabases :: ListDatabases -> TestTree
requestListDatabases =
  req
    "ListDatabases"
    "fixture/ListDatabases.yaml"

requestGetQueryResults :: GetQueryResults -> TestTree
requestGetQueryResults =
  req
    "GetQueryResults"
    "fixture/GetQueryResults.yaml"

requestGetWorkGroup :: GetWorkGroup -> TestTree
requestGetWorkGroup =
  req
    "GetWorkGroup"
    "fixture/GetWorkGroup.yaml"

requestStartQueryExecution :: StartQueryExecution -> TestTree
requestStartQueryExecution =
  req
    "StartQueryExecution"
    "fixture/StartQueryExecution.yaml"

requestStopQueryExecution :: StopQueryExecution -> TestTree
requestStopQueryExecution =
  req
    "StopQueryExecution"
    "fixture/StopQueryExecution.yaml"

requestGetTableMetadata :: GetTableMetadata -> TestTree
requestGetTableMetadata =
  req
    "GetTableMetadata"
    "fixture/GetTableMetadata.yaml"

requestCreateNamedQuery :: CreateNamedQuery -> TestTree
requestCreateNamedQuery =
  req
    "CreateNamedQuery"
    "fixture/CreateNamedQuery.yaml"

requestListNamedQueries :: ListNamedQueries -> TestTree
requestListNamedQueries =
  req
    "ListNamedQueries"
    "fixture/ListNamedQueries.yaml"

requestBatchGetQueryExecution :: BatchGetQueryExecution -> TestTree
requestBatchGetQueryExecution =
  req
    "BatchGetQueryExecution"
    "fixture/BatchGetQueryExecution.yaml"

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

-- Responses

responseCreateDataCatalog :: CreateDataCatalogResponse -> TestTree
responseCreateDataCatalog =
  res
    "CreateDataCatalogResponse"
    "fixture/CreateDataCatalogResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDataCatalog)

responseListQueryExecutions :: ListQueryExecutionsResponse -> TestTree
responseListQueryExecutions =
  res
    "ListQueryExecutionsResponse"
    "fixture/ListQueryExecutionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListQueryExecutions)

responseListTableMetadata :: ListTableMetadataResponse -> TestTree
responseListTableMetadata =
  res
    "ListTableMetadataResponse"
    "fixture/ListTableMetadataResponse.proto"
    defaultService
    (Proxy :: Proxy ListTableMetadata)

responseGetQueryExecution :: GetQueryExecutionResponse -> TestTree
responseGetQueryExecution =
  res
    "GetQueryExecutionResponse"
    "fixture/GetQueryExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy GetQueryExecution)

responseBatchGetNamedQuery :: BatchGetNamedQueryResponse -> TestTree
responseBatchGetNamedQuery =
  res
    "BatchGetNamedQueryResponse"
    "fixture/BatchGetNamedQueryResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetNamedQuery)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseGetDatabase :: GetDatabaseResponse -> TestTree
responseGetDatabase =
  res
    "GetDatabaseResponse"
    "fixture/GetDatabaseResponse.proto"
    defaultService
    (Proxy :: Proxy GetDatabase)

responseDeleteNamedQuery :: DeleteNamedQueryResponse -> TestTree
responseDeleteNamedQuery =
  res
    "DeleteNamedQueryResponse"
    "fixture/DeleteNamedQueryResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNamedQuery)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseListEngineVersions :: ListEngineVersionsResponse -> TestTree
responseListEngineVersions =
  res
    "ListEngineVersionsResponse"
    "fixture/ListEngineVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListEngineVersions)

responseGetDataCatalog :: GetDataCatalogResponse -> TestTree
responseGetDataCatalog =
  res
    "GetDataCatalogResponse"
    "fixture/GetDataCatalogResponse.proto"
    defaultService
    (Proxy :: Proxy GetDataCatalog)

responseListDataCatalogs :: ListDataCatalogsResponse -> TestTree
responseListDataCatalogs =
  res
    "ListDataCatalogsResponse"
    "fixture/ListDataCatalogsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDataCatalogs)

responseCreateWorkGroup :: CreateWorkGroupResponse -> TestTree
responseCreateWorkGroup =
  res
    "CreateWorkGroupResponse"
    "fixture/CreateWorkGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateWorkGroup)

responseGetNamedQuery :: GetNamedQueryResponse -> TestTree
responseGetNamedQuery =
  res
    "GetNamedQueryResponse"
    "fixture/GetNamedQueryResponse.proto"
    defaultService
    (Proxy :: Proxy GetNamedQuery)

responseUpdateWorkGroup :: UpdateWorkGroupResponse -> TestTree
responseUpdateWorkGroup =
  res
    "UpdateWorkGroupResponse"
    "fixture/UpdateWorkGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateWorkGroup)

responseDeleteWorkGroup :: DeleteWorkGroupResponse -> TestTree
responseDeleteWorkGroup =
  res
    "DeleteWorkGroupResponse"
    "fixture/DeleteWorkGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteWorkGroup)

responseListWorkGroups :: ListWorkGroupsResponse -> TestTree
responseListWorkGroups =
  res
    "ListWorkGroupsResponse"
    "fixture/ListWorkGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListWorkGroups)

responseListDatabases :: ListDatabasesResponse -> TestTree
responseListDatabases =
  res
    "ListDatabasesResponse"
    "fixture/ListDatabasesResponse.proto"
    defaultService
    (Proxy :: Proxy ListDatabases)

responseGetQueryResults :: GetQueryResultsResponse -> TestTree
responseGetQueryResults =
  res
    "GetQueryResultsResponse"
    "fixture/GetQueryResultsResponse.proto"
    defaultService
    (Proxy :: Proxy GetQueryResults)

responseGetWorkGroup :: GetWorkGroupResponse -> TestTree
responseGetWorkGroup =
  res
    "GetWorkGroupResponse"
    "fixture/GetWorkGroupResponse.proto"
    defaultService
    (Proxy :: Proxy GetWorkGroup)

responseStartQueryExecution :: StartQueryExecutionResponse -> TestTree
responseStartQueryExecution =
  res
    "StartQueryExecutionResponse"
    "fixture/StartQueryExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy StartQueryExecution)

responseStopQueryExecution :: StopQueryExecutionResponse -> TestTree
responseStopQueryExecution =
  res
    "StopQueryExecutionResponse"
    "fixture/StopQueryExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy StopQueryExecution)

responseGetTableMetadata :: GetTableMetadataResponse -> TestTree
responseGetTableMetadata =
  res
    "GetTableMetadataResponse"
    "fixture/GetTableMetadataResponse.proto"
    defaultService
    (Proxy :: Proxy GetTableMetadata)

responseCreateNamedQuery :: CreateNamedQueryResponse -> TestTree
responseCreateNamedQuery =
  res
    "CreateNamedQueryResponse"
    "fixture/CreateNamedQueryResponse.proto"
    defaultService
    (Proxy :: Proxy CreateNamedQuery)

responseListNamedQueries :: ListNamedQueriesResponse -> TestTree
responseListNamedQueries =
  res
    "ListNamedQueriesResponse"
    "fixture/ListNamedQueriesResponse.proto"
    defaultService
    (Proxy :: Proxy ListNamedQueries)

responseBatchGetQueryExecution :: BatchGetQueryExecutionResponse -> TestTree
responseBatchGetQueryExecution =
  res
    "BatchGetQueryExecutionResponse"
    "fixture/BatchGetQueryExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetQueryExecution)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseDeleteDataCatalog :: DeleteDataCatalogResponse -> TestTree
responseDeleteDataCatalog =
  res
    "DeleteDataCatalogResponse"
    "fixture/DeleteDataCatalogResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDataCatalog)

responseUpdateDataCatalog :: UpdateDataCatalogResponse -> TestTree
responseUpdateDataCatalog =
  res
    "UpdateDataCatalogResponse"
    "fixture/UpdateDataCatalogResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDataCatalog)
