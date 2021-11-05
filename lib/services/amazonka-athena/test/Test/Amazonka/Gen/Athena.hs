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
--         [ requestListDatabases $
--             newListDatabases
--
--         , requestCreatePreparedStatement $
--             newCreatePreparedStatement
--
--         , requestDeleteWorkGroup $
--             newDeleteWorkGroup
--
--         , requestUpdateWorkGroup $
--             newUpdateWorkGroup
--
--         , requestGetNamedQuery $
--             newGetNamedQuery
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
--         , requestListDataCatalogs $
--             newListDataCatalogs
--
--         , requestCreateNamedQuery $
--             newCreateNamedQuery
--
--         , requestGetTableMetadata $
--             newGetTableMetadata
--
--         , requestListNamedQueries $
--             newListNamedQueries
--
--         , requestDeleteNamedQuery $
--             newDeleteNamedQuery
--
--         , requestStartQueryExecution $
--             newStartQueryExecution
--
--         , requestBatchGetNamedQuery $
--             newBatchGetNamedQuery
--
--         , requestGetQueryExecution $
--             newGetQueryExecution
--
--         , requestListPreparedStatements $
--             newListPreparedStatements
--
--         , requestCreateDataCatalog $
--             newCreateDataCatalog
--
--         , requestListWorkGroups $
--             newListWorkGroups
--
--         , requestCreateWorkGroup $
--             newCreateWorkGroup
--
--         , requestBatchGetQueryExecution $
--             newBatchGetQueryExecution
--
--         , requestListEngineVersions $
--             newListEngineVersions
--
--         , requestGetDataCatalog $
--             newGetDataCatalog
--
--         , requestStopQueryExecution $
--             newStopQueryExecution
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetWorkGroup $
--             newGetWorkGroup
--
--         , requestGetDatabase $
--             newGetDatabase
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestGetPreparedStatement $
--             newGetPreparedStatement
--
--         , requestGetQueryResults $
--             newGetQueryResults
--
--         , requestListTableMetadata $
--             newListTableMetadata
--
--         , requestListQueryExecutions $
--             newListQueryExecutions
--
--         , requestDeletePreparedStatement $
--             newDeletePreparedStatement
--
--         , requestUpdatePreparedStatement $
--             newUpdatePreparedStatement
--
--           ]

--     , testGroup "response"
--         [ responseListDatabases $
--             newListDatabasesResponse
--
--         , responseCreatePreparedStatement $
--             newCreatePreparedStatementResponse
--
--         , responseDeleteWorkGroup $
--             newDeleteWorkGroupResponse
--
--         , responseUpdateWorkGroup $
--             newUpdateWorkGroupResponse
--
--         , responseGetNamedQuery $
--             newGetNamedQueryResponse
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
--         , responseListDataCatalogs $
--             newListDataCatalogsResponse
--
--         , responseCreateNamedQuery $
--             newCreateNamedQueryResponse
--
--         , responseGetTableMetadata $
--             newGetTableMetadataResponse
--
--         , responseListNamedQueries $
--             newListNamedQueriesResponse
--
--         , responseDeleteNamedQuery $
--             newDeleteNamedQueryResponse
--
--         , responseStartQueryExecution $
--             newStartQueryExecutionResponse
--
--         , responseBatchGetNamedQuery $
--             newBatchGetNamedQueryResponse
--
--         , responseGetQueryExecution $
--             newGetQueryExecutionResponse
--
--         , responseListPreparedStatements $
--             newListPreparedStatementsResponse
--
--         , responseCreateDataCatalog $
--             newCreateDataCatalogResponse
--
--         , responseListWorkGroups $
--             newListWorkGroupsResponse
--
--         , responseCreateWorkGroup $
--             newCreateWorkGroupResponse
--
--         , responseBatchGetQueryExecution $
--             newBatchGetQueryExecutionResponse
--
--         , responseListEngineVersions $
--             newListEngineVersionsResponse
--
--         , responseGetDataCatalog $
--             newGetDataCatalogResponse
--
--         , responseStopQueryExecution $
--             newStopQueryExecutionResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetWorkGroup $
--             newGetWorkGroupResponse
--
--         , responseGetDatabase $
--             newGetDatabaseResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseGetPreparedStatement $
--             newGetPreparedStatementResponse
--
--         , responseGetQueryResults $
--             newGetQueryResultsResponse
--
--         , responseListTableMetadata $
--             newListTableMetadataResponse
--
--         , responseListQueryExecutions $
--             newListQueryExecutionsResponse
--
--         , responseDeletePreparedStatement $
--             newDeletePreparedStatementResponse
--
--         , responseUpdatePreparedStatement $
--             newUpdatePreparedStatementResponse
--
--           ]
--     ]

-- Requests

requestListDatabases :: ListDatabases -> TestTree
requestListDatabases =
  req
    "ListDatabases"
    "fixture/ListDatabases.yaml"

requestCreatePreparedStatement :: CreatePreparedStatement -> TestTree
requestCreatePreparedStatement =
  req
    "CreatePreparedStatement"
    "fixture/CreatePreparedStatement.yaml"

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

requestListPreparedStatements :: ListPreparedStatements -> TestTree
requestListPreparedStatements =
  req
    "ListPreparedStatements"
    "fixture/ListPreparedStatements.yaml"

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

requestGetPreparedStatement :: GetPreparedStatement -> TestTree
requestGetPreparedStatement =
  req
    "GetPreparedStatement"
    "fixture/GetPreparedStatement.yaml"

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

requestDeletePreparedStatement :: DeletePreparedStatement -> TestTree
requestDeletePreparedStatement =
  req
    "DeletePreparedStatement"
    "fixture/DeletePreparedStatement.yaml"

requestUpdatePreparedStatement :: UpdatePreparedStatement -> TestTree
requestUpdatePreparedStatement =
  req
    "UpdatePreparedStatement"
    "fixture/UpdatePreparedStatement.yaml"

-- Responses

responseListDatabases :: ListDatabasesResponse -> TestTree
responseListDatabases =
  res
    "ListDatabasesResponse"
    "fixture/ListDatabasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatabases)

responseCreatePreparedStatement :: CreatePreparedStatementResponse -> TestTree
responseCreatePreparedStatement =
  res
    "CreatePreparedStatementResponse"
    "fixture/CreatePreparedStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePreparedStatement)

responseDeleteWorkGroup :: DeleteWorkGroupResponse -> TestTree
responseDeleteWorkGroup =
  res
    "DeleteWorkGroupResponse"
    "fixture/DeleteWorkGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkGroup)

responseUpdateWorkGroup :: UpdateWorkGroupResponse -> TestTree
responseUpdateWorkGroup =
  res
    "UpdateWorkGroupResponse"
    "fixture/UpdateWorkGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWorkGroup)

responseGetNamedQuery :: GetNamedQueryResponse -> TestTree
responseGetNamedQuery =
  res
    "GetNamedQueryResponse"
    "fixture/GetNamedQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetNamedQuery)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseDeleteDataCatalog :: DeleteDataCatalogResponse -> TestTree
responseDeleteDataCatalog =
  res
    "DeleteDataCatalogResponse"
    "fixture/DeleteDataCatalogResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataCatalog)

responseUpdateDataCatalog :: UpdateDataCatalogResponse -> TestTree
responseUpdateDataCatalog =
  res
    "UpdateDataCatalogResponse"
    "fixture/UpdateDataCatalogResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataCatalog)

responseListDataCatalogs :: ListDataCatalogsResponse -> TestTree
responseListDataCatalogs =
  res
    "ListDataCatalogsResponse"
    "fixture/ListDataCatalogsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataCatalogs)

responseCreateNamedQuery :: CreateNamedQueryResponse -> TestTree
responseCreateNamedQuery =
  res
    "CreateNamedQueryResponse"
    "fixture/CreateNamedQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNamedQuery)

responseGetTableMetadata :: GetTableMetadataResponse -> TestTree
responseGetTableMetadata =
  res
    "GetTableMetadataResponse"
    "fixture/GetTableMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTableMetadata)

responseListNamedQueries :: ListNamedQueriesResponse -> TestTree
responseListNamedQueries =
  res
    "ListNamedQueriesResponse"
    "fixture/ListNamedQueriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNamedQueries)

responseDeleteNamedQuery :: DeleteNamedQueryResponse -> TestTree
responseDeleteNamedQuery =
  res
    "DeleteNamedQueryResponse"
    "fixture/DeleteNamedQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNamedQuery)

responseStartQueryExecution :: StartQueryExecutionResponse -> TestTree
responseStartQueryExecution =
  res
    "StartQueryExecutionResponse"
    "fixture/StartQueryExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartQueryExecution)

responseBatchGetNamedQuery :: BatchGetNamedQueryResponse -> TestTree
responseBatchGetNamedQuery =
  res
    "BatchGetNamedQueryResponse"
    "fixture/BatchGetNamedQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetNamedQuery)

responseGetQueryExecution :: GetQueryExecutionResponse -> TestTree
responseGetQueryExecution =
  res
    "GetQueryExecutionResponse"
    "fixture/GetQueryExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetQueryExecution)

responseListPreparedStatements :: ListPreparedStatementsResponse -> TestTree
responseListPreparedStatements =
  res
    "ListPreparedStatementsResponse"
    "fixture/ListPreparedStatementsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPreparedStatements)

responseCreateDataCatalog :: CreateDataCatalogResponse -> TestTree
responseCreateDataCatalog =
  res
    "CreateDataCatalogResponse"
    "fixture/CreateDataCatalogResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataCatalog)

responseListWorkGroups :: ListWorkGroupsResponse -> TestTree
responseListWorkGroups =
  res
    "ListWorkGroupsResponse"
    "fixture/ListWorkGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkGroups)

responseCreateWorkGroup :: CreateWorkGroupResponse -> TestTree
responseCreateWorkGroup =
  res
    "CreateWorkGroupResponse"
    "fixture/CreateWorkGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkGroup)

responseBatchGetQueryExecution :: BatchGetQueryExecutionResponse -> TestTree
responseBatchGetQueryExecution =
  res
    "BatchGetQueryExecutionResponse"
    "fixture/BatchGetQueryExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetQueryExecution)

responseListEngineVersions :: ListEngineVersionsResponse -> TestTree
responseListEngineVersions =
  res
    "ListEngineVersionsResponse"
    "fixture/ListEngineVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEngineVersions)

responseGetDataCatalog :: GetDataCatalogResponse -> TestTree
responseGetDataCatalog =
  res
    "GetDataCatalogResponse"
    "fixture/GetDataCatalogResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataCatalog)

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

responseGetWorkGroup :: GetWorkGroupResponse -> TestTree
responseGetWorkGroup =
  res
    "GetWorkGroupResponse"
    "fixture/GetWorkGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWorkGroup)

responseGetDatabase :: GetDatabaseResponse -> TestTree
responseGetDatabase =
  res
    "GetDatabaseResponse"
    "fixture/GetDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDatabase)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseGetPreparedStatement :: GetPreparedStatementResponse -> TestTree
responseGetPreparedStatement =
  res
    "GetPreparedStatementResponse"
    "fixture/GetPreparedStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPreparedStatement)

responseGetQueryResults :: GetQueryResultsResponse -> TestTree
responseGetQueryResults =
  res
    "GetQueryResultsResponse"
    "fixture/GetQueryResultsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetQueryResults)

responseListTableMetadata :: ListTableMetadataResponse -> TestTree
responseListTableMetadata =
  res
    "ListTableMetadataResponse"
    "fixture/ListTableMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTableMetadata)

responseListQueryExecutions :: ListQueryExecutionsResponse -> TestTree
responseListQueryExecutions =
  res
    "ListQueryExecutionsResponse"
    "fixture/ListQueryExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListQueryExecutions)

responseDeletePreparedStatement :: DeletePreparedStatementResponse -> TestTree
responseDeletePreparedStatement =
  res
    "DeletePreparedStatementResponse"
    "fixture/DeletePreparedStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePreparedStatement)

responseUpdatePreparedStatement :: UpdatePreparedStatementResponse -> TestTree
responseUpdatePreparedStatement =
  res
    "UpdatePreparedStatementResponse"
    "fixture/UpdatePreparedStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePreparedStatement)
