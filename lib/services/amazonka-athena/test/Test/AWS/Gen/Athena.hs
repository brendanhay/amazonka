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
--         , requestDeletePreparedStatement $
--             newDeletePreparedStatement
--
--         , requestListTableMetadata $
--             newListTableMetadata
--
--         , requestUpdatePreparedStatement $
--             newUpdatePreparedStatement
--
--         , requestListPreparedStatements $
--             newListPreparedStatements
--
--         , requestGetPreparedStatement $
--             newGetPreparedStatement
--
--         , requestGetQueryExecution $
--             newGetQueryExecution
--
--         , requestGetDatabase $
--             newGetDatabase
--
--         , requestBatchGetNamedQuery $
--             newBatchGetNamedQuery
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDeleteNamedQuery $
--             newDeleteNamedQuery
--
--         , requestGetDataCatalog $
--             newGetDataCatalog
--
--         , requestListEngineVersions $
--             newListEngineVersions
--
--         , requestCreateWorkGroup $
--             newCreateWorkGroup
--
--         , requestListDataCatalogs $
--             newListDataCatalogs
--
--         , requestUpdateWorkGroup $
--             newUpdateWorkGroup
--
--         , requestGetNamedQuery $
--             newGetNamedQuery
--
--         , requestListWorkGroups $
--             newListWorkGroups
--
--         , requestDeleteWorkGroup $
--             newDeleteWorkGroup
--
--         , requestListDatabases $
--             newListDatabases
--
--         , requestCreatePreparedStatement $
--             newCreatePreparedStatement
--
--         , requestGetQueryResults $
--             newGetQueryResults
--
--         , requestGetWorkGroup $
--             newGetWorkGroup
--
--         , requestStopQueryExecution $
--             newStopQueryExecution
--
--         , requestStartQueryExecution $
--             newStartQueryExecution
--
--         , requestListNamedQueries $
--             newListNamedQueries
--
--         , requestGetTableMetadata $
--             newGetTableMetadata
--
--         , requestCreateNamedQuery $
--             newCreateNamedQuery
--
--         , requestBatchGetQueryExecution $
--             newBatchGetQueryExecution
--
--         , requestDeleteDataCatalog $
--             newDeleteDataCatalog
--
--         , requestListTagsForResource $
--             newListTagsForResource
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
--         , responseDeletePreparedStatement $
--             newDeletePreparedStatementResponse
--
--         , responseListTableMetadata $
--             newListTableMetadataResponse
--
--         , responseUpdatePreparedStatement $
--             newUpdatePreparedStatementResponse
--
--         , responseListPreparedStatements $
--             newListPreparedStatementsResponse
--
--         , responseGetPreparedStatement $
--             newGetPreparedStatementResponse
--
--         , responseGetQueryExecution $
--             newGetQueryExecutionResponse
--
--         , responseGetDatabase $
--             newGetDatabaseResponse
--
--         , responseBatchGetNamedQuery $
--             newBatchGetNamedQueryResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDeleteNamedQuery $
--             newDeleteNamedQueryResponse
--
--         , responseGetDataCatalog $
--             newGetDataCatalogResponse
--
--         , responseListEngineVersions $
--             newListEngineVersionsResponse
--
--         , responseCreateWorkGroup $
--             newCreateWorkGroupResponse
--
--         , responseListDataCatalogs $
--             newListDataCatalogsResponse
--
--         , responseUpdateWorkGroup $
--             newUpdateWorkGroupResponse
--
--         , responseGetNamedQuery $
--             newGetNamedQueryResponse
--
--         , responseListWorkGroups $
--             newListWorkGroupsResponse
--
--         , responseDeleteWorkGroup $
--             newDeleteWorkGroupResponse
--
--         , responseListDatabases $
--             newListDatabasesResponse
--
--         , responseCreatePreparedStatement $
--             newCreatePreparedStatementResponse
--
--         , responseGetQueryResults $
--             newGetQueryResultsResponse
--
--         , responseGetWorkGroup $
--             newGetWorkGroupResponse
--
--         , responseStopQueryExecution $
--             newStopQueryExecutionResponse
--
--         , responseStartQueryExecution $
--             newStartQueryExecutionResponse
--
--         , responseListNamedQueries $
--             newListNamedQueriesResponse
--
--         , responseGetTableMetadata $
--             newGetTableMetadataResponse
--
--         , responseCreateNamedQuery $
--             newCreateNamedQueryResponse
--
--         , responseBatchGetQueryExecution $
--             newBatchGetQueryExecutionResponse
--
--         , responseDeleteDataCatalog $
--             newDeleteDataCatalogResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
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

requestDeletePreparedStatement :: DeletePreparedStatement -> TestTree
requestDeletePreparedStatement =
  req
    "DeletePreparedStatement"
    "fixture/DeletePreparedStatement.yaml"

requestListTableMetadata :: ListTableMetadata -> TestTree
requestListTableMetadata =
  req
    "ListTableMetadata"
    "fixture/ListTableMetadata.yaml"

requestUpdatePreparedStatement :: UpdatePreparedStatement -> TestTree
requestUpdatePreparedStatement =
  req
    "UpdatePreparedStatement"
    "fixture/UpdatePreparedStatement.yaml"

requestListPreparedStatements :: ListPreparedStatements -> TestTree
requestListPreparedStatements =
  req
    "ListPreparedStatements"
    "fixture/ListPreparedStatements.yaml"

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

requestGetDatabase :: GetDatabase -> TestTree
requestGetDatabase =
  req
    "GetDatabase"
    "fixture/GetDatabase.yaml"

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

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDeleteNamedQuery :: DeleteNamedQuery -> TestTree
requestDeleteNamedQuery =
  req
    "DeleteNamedQuery"
    "fixture/DeleteNamedQuery.yaml"

requestGetDataCatalog :: GetDataCatalog -> TestTree
requestGetDataCatalog =
  req
    "GetDataCatalog"
    "fixture/GetDataCatalog.yaml"

requestListEngineVersions :: ListEngineVersions -> TestTree
requestListEngineVersions =
  req
    "ListEngineVersions"
    "fixture/ListEngineVersions.yaml"

requestCreateWorkGroup :: CreateWorkGroup -> TestTree
requestCreateWorkGroup =
  req
    "CreateWorkGroup"
    "fixture/CreateWorkGroup.yaml"

requestListDataCatalogs :: ListDataCatalogs -> TestTree
requestListDataCatalogs =
  req
    "ListDataCatalogs"
    "fixture/ListDataCatalogs.yaml"

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

requestListWorkGroups :: ListWorkGroups -> TestTree
requestListWorkGroups =
  req
    "ListWorkGroups"
    "fixture/ListWorkGroups.yaml"

requestDeleteWorkGroup :: DeleteWorkGroup -> TestTree
requestDeleteWorkGroup =
  req
    "DeleteWorkGroup"
    "fixture/DeleteWorkGroup.yaml"

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

requestStopQueryExecution :: StopQueryExecution -> TestTree
requestStopQueryExecution =
  req
    "StopQueryExecution"
    "fixture/StopQueryExecution.yaml"

requestStartQueryExecution :: StartQueryExecution -> TestTree
requestStartQueryExecution =
  req
    "StartQueryExecution"
    "fixture/StartQueryExecution.yaml"

requestListNamedQueries :: ListNamedQueries -> TestTree
requestListNamedQueries =
  req
    "ListNamedQueries"
    "fixture/ListNamedQueries.yaml"

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

requestBatchGetQueryExecution :: BatchGetQueryExecution -> TestTree
requestBatchGetQueryExecution =
  req
    "BatchGetQueryExecution"
    "fixture/BatchGetQueryExecution.yaml"

requestDeleteDataCatalog :: DeleteDataCatalog -> TestTree
requestDeleteDataCatalog =
  req
    "DeleteDataCatalog"
    "fixture/DeleteDataCatalog.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

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

responseDeletePreparedStatement :: DeletePreparedStatementResponse -> TestTree
responseDeletePreparedStatement =
  res
    "DeletePreparedStatementResponse"
    "fixture/DeletePreparedStatementResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePreparedStatement)

responseListTableMetadata :: ListTableMetadataResponse -> TestTree
responseListTableMetadata =
  res
    "ListTableMetadataResponse"
    "fixture/ListTableMetadataResponse.proto"
    defaultService
    (Proxy :: Proxy ListTableMetadata)

responseUpdatePreparedStatement :: UpdatePreparedStatementResponse -> TestTree
responseUpdatePreparedStatement =
  res
    "UpdatePreparedStatementResponse"
    "fixture/UpdatePreparedStatementResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePreparedStatement)

responseListPreparedStatements :: ListPreparedStatementsResponse -> TestTree
responseListPreparedStatements =
  res
    "ListPreparedStatementsResponse"
    "fixture/ListPreparedStatementsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPreparedStatements)

responseGetPreparedStatement :: GetPreparedStatementResponse -> TestTree
responseGetPreparedStatement =
  res
    "GetPreparedStatementResponse"
    "fixture/GetPreparedStatementResponse.proto"
    defaultService
    (Proxy :: Proxy GetPreparedStatement)

responseGetQueryExecution :: GetQueryExecutionResponse -> TestTree
responseGetQueryExecution =
  res
    "GetQueryExecutionResponse"
    "fixture/GetQueryExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy GetQueryExecution)

responseGetDatabase :: GetDatabaseResponse -> TestTree
responseGetDatabase =
  res
    "GetDatabaseResponse"
    "fixture/GetDatabaseResponse.proto"
    defaultService
    (Proxy :: Proxy GetDatabase)

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

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseDeleteNamedQuery :: DeleteNamedQueryResponse -> TestTree
responseDeleteNamedQuery =
  res
    "DeleteNamedQueryResponse"
    "fixture/DeleteNamedQueryResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNamedQuery)

responseGetDataCatalog :: GetDataCatalogResponse -> TestTree
responseGetDataCatalog =
  res
    "GetDataCatalogResponse"
    "fixture/GetDataCatalogResponse.proto"
    defaultService
    (Proxy :: Proxy GetDataCatalog)

responseListEngineVersions :: ListEngineVersionsResponse -> TestTree
responseListEngineVersions =
  res
    "ListEngineVersionsResponse"
    "fixture/ListEngineVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListEngineVersions)

responseCreateWorkGroup :: CreateWorkGroupResponse -> TestTree
responseCreateWorkGroup =
  res
    "CreateWorkGroupResponse"
    "fixture/CreateWorkGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateWorkGroup)

responseListDataCatalogs :: ListDataCatalogsResponse -> TestTree
responseListDataCatalogs =
  res
    "ListDataCatalogsResponse"
    "fixture/ListDataCatalogsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDataCatalogs)

responseUpdateWorkGroup :: UpdateWorkGroupResponse -> TestTree
responseUpdateWorkGroup =
  res
    "UpdateWorkGroupResponse"
    "fixture/UpdateWorkGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateWorkGroup)

responseGetNamedQuery :: GetNamedQueryResponse -> TestTree
responseGetNamedQuery =
  res
    "GetNamedQueryResponse"
    "fixture/GetNamedQueryResponse.proto"
    defaultService
    (Proxy :: Proxy GetNamedQuery)

responseListWorkGroups :: ListWorkGroupsResponse -> TestTree
responseListWorkGroups =
  res
    "ListWorkGroupsResponse"
    "fixture/ListWorkGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListWorkGroups)

responseDeleteWorkGroup :: DeleteWorkGroupResponse -> TestTree
responseDeleteWorkGroup =
  res
    "DeleteWorkGroupResponse"
    "fixture/DeleteWorkGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteWorkGroup)

responseListDatabases :: ListDatabasesResponse -> TestTree
responseListDatabases =
  res
    "ListDatabasesResponse"
    "fixture/ListDatabasesResponse.proto"
    defaultService
    (Proxy :: Proxy ListDatabases)

responseCreatePreparedStatement :: CreatePreparedStatementResponse -> TestTree
responseCreatePreparedStatement =
  res
    "CreatePreparedStatementResponse"
    "fixture/CreatePreparedStatementResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePreparedStatement)

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

responseStopQueryExecution :: StopQueryExecutionResponse -> TestTree
responseStopQueryExecution =
  res
    "StopQueryExecutionResponse"
    "fixture/StopQueryExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy StopQueryExecution)

responseStartQueryExecution :: StartQueryExecutionResponse -> TestTree
responseStartQueryExecution =
  res
    "StartQueryExecutionResponse"
    "fixture/StartQueryExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy StartQueryExecution)

responseListNamedQueries :: ListNamedQueriesResponse -> TestTree
responseListNamedQueries =
  res
    "ListNamedQueriesResponse"
    "fixture/ListNamedQueriesResponse.proto"
    defaultService
    (Proxy :: Proxy ListNamedQueries)

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

responseBatchGetQueryExecution :: BatchGetQueryExecutionResponse -> TestTree
responseBatchGetQueryExecution =
  res
    "BatchGetQueryExecutionResponse"
    "fixture/BatchGetQueryExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetQueryExecution)

responseDeleteDataCatalog :: DeleteDataCatalogResponse -> TestTree
responseDeleteDataCatalog =
  res
    "DeleteDataCatalogResponse"
    "fixture/DeleteDataCatalogResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDataCatalog)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseUpdateDataCatalog :: UpdateDataCatalogResponse -> TestTree
responseUpdateDataCatalog =
  res
    "UpdateDataCatalogResponse"
    "fixture/UpdateDataCatalogResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDataCatalog)
