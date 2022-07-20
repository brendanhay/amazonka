{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Athena
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Athena where

import Amazonka.Athena
import qualified Data.Proxy as Proxy
import Test.Amazonka.Athena.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchGetNamedQuery $
--             newBatchGetNamedQuery
--
--         , requestBatchGetQueryExecution $
--             newBatchGetQueryExecution
--
--         , requestCreateDataCatalog $
--             newCreateDataCatalog
--
--         , requestCreateNamedQuery $
--             newCreateNamedQuery
--
--         , requestCreatePreparedStatement $
--             newCreatePreparedStatement
--
--         , requestCreateWorkGroup $
--             newCreateWorkGroup
--
--         , requestDeleteDataCatalog $
--             newDeleteDataCatalog
--
--         , requestDeleteNamedQuery $
--             newDeleteNamedQuery
--
--         , requestDeletePreparedStatement $
--             newDeletePreparedStatement
--
--         , requestDeleteWorkGroup $
--             newDeleteWorkGroup
--
--         , requestGetDataCatalog $
--             newGetDataCatalog
--
--         , requestGetDatabase $
--             newGetDatabase
--
--         , requestGetNamedQuery $
--             newGetNamedQuery
--
--         , requestGetPreparedStatement $
--             newGetPreparedStatement
--
--         , requestGetQueryExecution $
--             newGetQueryExecution
--
--         , requestGetQueryResults $
--             newGetQueryResults
--
--         , requestGetTableMetadata $
--             newGetTableMetadata
--
--         , requestGetWorkGroup $
--             newGetWorkGroup
--
--         , requestListDataCatalogs $
--             newListDataCatalogs
--
--         , requestListDatabases $
--             newListDatabases
--
--         , requestListEngineVersions $
--             newListEngineVersions
--
--         , requestListNamedQueries $
--             newListNamedQueries
--
--         , requestListPreparedStatements $
--             newListPreparedStatements
--
--         , requestListQueryExecutions $
--             newListQueryExecutions
--
--         , requestListTableMetadata $
--             newListTableMetadata
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListWorkGroups $
--             newListWorkGroups
--
--         , requestStartQueryExecution $
--             newStartQueryExecution
--
--         , requestStopQueryExecution $
--             newStopQueryExecution
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateDataCatalog $
--             newUpdateDataCatalog
--
--         , requestUpdatePreparedStatement $
--             newUpdatePreparedStatement
--
--         , requestUpdateWorkGroup $
--             newUpdateWorkGroup
--
--           ]

--     , testGroup "response"
--         [ responseBatchGetNamedQuery $
--             newBatchGetNamedQueryResponse
--
--         , responseBatchGetQueryExecution $
--             newBatchGetQueryExecutionResponse
--
--         , responseCreateDataCatalog $
--             newCreateDataCatalogResponse
--
--         , responseCreateNamedQuery $
--             newCreateNamedQueryResponse
--
--         , responseCreatePreparedStatement $
--             newCreatePreparedStatementResponse
--
--         , responseCreateWorkGroup $
--             newCreateWorkGroupResponse
--
--         , responseDeleteDataCatalog $
--             newDeleteDataCatalogResponse
--
--         , responseDeleteNamedQuery $
--             newDeleteNamedQueryResponse
--
--         , responseDeletePreparedStatement $
--             newDeletePreparedStatementResponse
--
--         , responseDeleteWorkGroup $
--             newDeleteWorkGroupResponse
--
--         , responseGetDataCatalog $
--             newGetDataCatalogResponse
--
--         , responseGetDatabase $
--             newGetDatabaseResponse
--
--         , responseGetNamedQuery $
--             newGetNamedQueryResponse
--
--         , responseGetPreparedStatement $
--             newGetPreparedStatementResponse
--
--         , responseGetQueryExecution $
--             newGetQueryExecutionResponse
--
--         , responseGetQueryResults $
--             newGetQueryResultsResponse
--
--         , responseGetTableMetadata $
--             newGetTableMetadataResponse
--
--         , responseGetWorkGroup $
--             newGetWorkGroupResponse
--
--         , responseListDataCatalogs $
--             newListDataCatalogsResponse
--
--         , responseListDatabases $
--             newListDatabasesResponse
--
--         , responseListEngineVersions $
--             newListEngineVersionsResponse
--
--         , responseListNamedQueries $
--             newListNamedQueriesResponse
--
--         , responseListPreparedStatements $
--             newListPreparedStatementsResponse
--
--         , responseListQueryExecutions $
--             newListQueryExecutionsResponse
--
--         , responseListTableMetadata $
--             newListTableMetadataResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListWorkGroups $
--             newListWorkGroupsResponse
--
--         , responseStartQueryExecution $
--             newStartQueryExecutionResponse
--
--         , responseStopQueryExecution $
--             newStopQueryExecutionResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateDataCatalog $
--             newUpdateDataCatalogResponse
--
--         , responseUpdatePreparedStatement $
--             newUpdatePreparedStatementResponse
--
--         , responseUpdateWorkGroup $
--             newUpdateWorkGroupResponse
--
--           ]
--     ]

-- Requests

requestBatchGetNamedQuery :: BatchGetNamedQuery -> TestTree
requestBatchGetNamedQuery =
  req
    "BatchGetNamedQuery"
    "fixture/BatchGetNamedQuery.yaml"

requestBatchGetQueryExecution :: BatchGetQueryExecution -> TestTree
requestBatchGetQueryExecution =
  req
    "BatchGetQueryExecution"
    "fixture/BatchGetQueryExecution.yaml"

requestCreateDataCatalog :: CreateDataCatalog -> TestTree
requestCreateDataCatalog =
  req
    "CreateDataCatalog"
    "fixture/CreateDataCatalog.yaml"

requestCreateNamedQuery :: CreateNamedQuery -> TestTree
requestCreateNamedQuery =
  req
    "CreateNamedQuery"
    "fixture/CreateNamedQuery.yaml"

requestCreatePreparedStatement :: CreatePreparedStatement -> TestTree
requestCreatePreparedStatement =
  req
    "CreatePreparedStatement"
    "fixture/CreatePreparedStatement.yaml"

requestCreateWorkGroup :: CreateWorkGroup -> TestTree
requestCreateWorkGroup =
  req
    "CreateWorkGroup"
    "fixture/CreateWorkGroup.yaml"

requestDeleteDataCatalog :: DeleteDataCatalog -> TestTree
requestDeleteDataCatalog =
  req
    "DeleteDataCatalog"
    "fixture/DeleteDataCatalog.yaml"

requestDeleteNamedQuery :: DeleteNamedQuery -> TestTree
requestDeleteNamedQuery =
  req
    "DeleteNamedQuery"
    "fixture/DeleteNamedQuery.yaml"

requestDeletePreparedStatement :: DeletePreparedStatement -> TestTree
requestDeletePreparedStatement =
  req
    "DeletePreparedStatement"
    "fixture/DeletePreparedStatement.yaml"

requestDeleteWorkGroup :: DeleteWorkGroup -> TestTree
requestDeleteWorkGroup =
  req
    "DeleteWorkGroup"
    "fixture/DeleteWorkGroup.yaml"

requestGetDataCatalog :: GetDataCatalog -> TestTree
requestGetDataCatalog =
  req
    "GetDataCatalog"
    "fixture/GetDataCatalog.yaml"

requestGetDatabase :: GetDatabase -> TestTree
requestGetDatabase =
  req
    "GetDatabase"
    "fixture/GetDatabase.yaml"

requestGetNamedQuery :: GetNamedQuery -> TestTree
requestGetNamedQuery =
  req
    "GetNamedQuery"
    "fixture/GetNamedQuery.yaml"

requestGetPreparedStatement :: GetPreparedStatement -> TestTree
requestGetPreparedStatement =
  req
    "GetPreparedStatement"
    "fixture/GetPreparedStatement.yaml"

requestGetQueryExecution :: GetQueryExecution -> TestTree
requestGetQueryExecution =
  req
    "GetQueryExecution"
    "fixture/GetQueryExecution.yaml"

requestGetQueryResults :: GetQueryResults -> TestTree
requestGetQueryResults =
  req
    "GetQueryResults"
    "fixture/GetQueryResults.yaml"

requestGetTableMetadata :: GetTableMetadata -> TestTree
requestGetTableMetadata =
  req
    "GetTableMetadata"
    "fixture/GetTableMetadata.yaml"

requestGetWorkGroup :: GetWorkGroup -> TestTree
requestGetWorkGroup =
  req
    "GetWorkGroup"
    "fixture/GetWorkGroup.yaml"

requestListDataCatalogs :: ListDataCatalogs -> TestTree
requestListDataCatalogs =
  req
    "ListDataCatalogs"
    "fixture/ListDataCatalogs.yaml"

requestListDatabases :: ListDatabases -> TestTree
requestListDatabases =
  req
    "ListDatabases"
    "fixture/ListDatabases.yaml"

requestListEngineVersions :: ListEngineVersions -> TestTree
requestListEngineVersions =
  req
    "ListEngineVersions"
    "fixture/ListEngineVersions.yaml"

requestListNamedQueries :: ListNamedQueries -> TestTree
requestListNamedQueries =
  req
    "ListNamedQueries"
    "fixture/ListNamedQueries.yaml"

requestListPreparedStatements :: ListPreparedStatements -> TestTree
requestListPreparedStatements =
  req
    "ListPreparedStatements"
    "fixture/ListPreparedStatements.yaml"

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

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListWorkGroups :: ListWorkGroups -> TestTree
requestListWorkGroups =
  req
    "ListWorkGroups"
    "fixture/ListWorkGroups.yaml"

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

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateDataCatalog :: UpdateDataCatalog -> TestTree
requestUpdateDataCatalog =
  req
    "UpdateDataCatalog"
    "fixture/UpdateDataCatalog.yaml"

requestUpdatePreparedStatement :: UpdatePreparedStatement -> TestTree
requestUpdatePreparedStatement =
  req
    "UpdatePreparedStatement"
    "fixture/UpdatePreparedStatement.yaml"

requestUpdateWorkGroup :: UpdateWorkGroup -> TestTree
requestUpdateWorkGroup =
  req
    "UpdateWorkGroup"
    "fixture/UpdateWorkGroup.yaml"

-- Responses

responseBatchGetNamedQuery :: BatchGetNamedQueryResponse -> TestTree
responseBatchGetNamedQuery =
  res
    "BatchGetNamedQueryResponse"
    "fixture/BatchGetNamedQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetNamedQuery)

responseBatchGetQueryExecution :: BatchGetQueryExecutionResponse -> TestTree
responseBatchGetQueryExecution =
  res
    "BatchGetQueryExecutionResponse"
    "fixture/BatchGetQueryExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetQueryExecution)

responseCreateDataCatalog :: CreateDataCatalogResponse -> TestTree
responseCreateDataCatalog =
  res
    "CreateDataCatalogResponse"
    "fixture/CreateDataCatalogResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataCatalog)

responseCreateNamedQuery :: CreateNamedQueryResponse -> TestTree
responseCreateNamedQuery =
  res
    "CreateNamedQueryResponse"
    "fixture/CreateNamedQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNamedQuery)

responseCreatePreparedStatement :: CreatePreparedStatementResponse -> TestTree
responseCreatePreparedStatement =
  res
    "CreatePreparedStatementResponse"
    "fixture/CreatePreparedStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePreparedStatement)

responseCreateWorkGroup :: CreateWorkGroupResponse -> TestTree
responseCreateWorkGroup =
  res
    "CreateWorkGroupResponse"
    "fixture/CreateWorkGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkGroup)

responseDeleteDataCatalog :: DeleteDataCatalogResponse -> TestTree
responseDeleteDataCatalog =
  res
    "DeleteDataCatalogResponse"
    "fixture/DeleteDataCatalogResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataCatalog)

responseDeleteNamedQuery :: DeleteNamedQueryResponse -> TestTree
responseDeleteNamedQuery =
  res
    "DeleteNamedQueryResponse"
    "fixture/DeleteNamedQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNamedQuery)

responseDeletePreparedStatement :: DeletePreparedStatementResponse -> TestTree
responseDeletePreparedStatement =
  res
    "DeletePreparedStatementResponse"
    "fixture/DeletePreparedStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePreparedStatement)

responseDeleteWorkGroup :: DeleteWorkGroupResponse -> TestTree
responseDeleteWorkGroup =
  res
    "DeleteWorkGroupResponse"
    "fixture/DeleteWorkGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkGroup)

responseGetDataCatalog :: GetDataCatalogResponse -> TestTree
responseGetDataCatalog =
  res
    "GetDataCatalogResponse"
    "fixture/GetDataCatalogResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataCatalog)

responseGetDatabase :: GetDatabaseResponse -> TestTree
responseGetDatabase =
  res
    "GetDatabaseResponse"
    "fixture/GetDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDatabase)

responseGetNamedQuery :: GetNamedQueryResponse -> TestTree
responseGetNamedQuery =
  res
    "GetNamedQueryResponse"
    "fixture/GetNamedQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetNamedQuery)

responseGetPreparedStatement :: GetPreparedStatementResponse -> TestTree
responseGetPreparedStatement =
  res
    "GetPreparedStatementResponse"
    "fixture/GetPreparedStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPreparedStatement)

responseGetQueryExecution :: GetQueryExecutionResponse -> TestTree
responseGetQueryExecution =
  res
    "GetQueryExecutionResponse"
    "fixture/GetQueryExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetQueryExecution)

responseGetQueryResults :: GetQueryResultsResponse -> TestTree
responseGetQueryResults =
  res
    "GetQueryResultsResponse"
    "fixture/GetQueryResultsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetQueryResults)

responseGetTableMetadata :: GetTableMetadataResponse -> TestTree
responseGetTableMetadata =
  res
    "GetTableMetadataResponse"
    "fixture/GetTableMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTableMetadata)

responseGetWorkGroup :: GetWorkGroupResponse -> TestTree
responseGetWorkGroup =
  res
    "GetWorkGroupResponse"
    "fixture/GetWorkGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWorkGroup)

responseListDataCatalogs :: ListDataCatalogsResponse -> TestTree
responseListDataCatalogs =
  res
    "ListDataCatalogsResponse"
    "fixture/ListDataCatalogsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataCatalogs)

responseListDatabases :: ListDatabasesResponse -> TestTree
responseListDatabases =
  res
    "ListDatabasesResponse"
    "fixture/ListDatabasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatabases)

responseListEngineVersions :: ListEngineVersionsResponse -> TestTree
responseListEngineVersions =
  res
    "ListEngineVersionsResponse"
    "fixture/ListEngineVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEngineVersions)

responseListNamedQueries :: ListNamedQueriesResponse -> TestTree
responseListNamedQueries =
  res
    "ListNamedQueriesResponse"
    "fixture/ListNamedQueriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNamedQueries)

responseListPreparedStatements :: ListPreparedStatementsResponse -> TestTree
responseListPreparedStatements =
  res
    "ListPreparedStatementsResponse"
    "fixture/ListPreparedStatementsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPreparedStatements)

responseListQueryExecutions :: ListQueryExecutionsResponse -> TestTree
responseListQueryExecutions =
  res
    "ListQueryExecutionsResponse"
    "fixture/ListQueryExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListQueryExecutions)

responseListTableMetadata :: ListTableMetadataResponse -> TestTree
responseListTableMetadata =
  res
    "ListTableMetadataResponse"
    "fixture/ListTableMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTableMetadata)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListWorkGroups :: ListWorkGroupsResponse -> TestTree
responseListWorkGroups =
  res
    "ListWorkGroupsResponse"
    "fixture/ListWorkGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkGroups)

responseStartQueryExecution :: StartQueryExecutionResponse -> TestTree
responseStartQueryExecution =
  res
    "StartQueryExecutionResponse"
    "fixture/StartQueryExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartQueryExecution)

responseStopQueryExecution :: StopQueryExecutionResponse -> TestTree
responseStopQueryExecution =
  res
    "StopQueryExecutionResponse"
    "fixture/StopQueryExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopQueryExecution)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateDataCatalog :: UpdateDataCatalogResponse -> TestTree
responseUpdateDataCatalog =
  res
    "UpdateDataCatalogResponse"
    "fixture/UpdateDataCatalogResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataCatalog)

responseUpdatePreparedStatement :: UpdatePreparedStatementResponse -> TestTree
responseUpdatePreparedStatement =
  res
    "UpdatePreparedStatementResponse"
    "fixture/UpdatePreparedStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePreparedStatement)

responseUpdateWorkGroup :: UpdateWorkGroupResponse -> TestTree
responseUpdateWorkGroup =
  res
    "UpdateWorkGroupResponse"
    "fixture/UpdateWorkGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWorkGroup)
