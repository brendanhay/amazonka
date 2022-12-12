{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Athena
-- Copyright   : (c) 2013-2022 Brendan Hay
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
--         , requestBatchGetPreparedStatement $
--             newBatchGetPreparedStatement
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
--         , requestCreateNotebook $
--             newCreateNotebook
--
--         , requestCreatePreparedStatement $
--             newCreatePreparedStatement
--
--         , requestCreatePresignedNotebookUrl $
--             newCreatePresignedNotebookUrl
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
--         , requestDeleteNotebook $
--             newDeleteNotebook
--
--         , requestDeletePreparedStatement $
--             newDeletePreparedStatement
--
--         , requestDeleteWorkGroup $
--             newDeleteWorkGroup
--
--         , requestExportNotebook $
--             newExportNotebook
--
--         , requestGetCalculationExecution $
--             newGetCalculationExecution
--
--         , requestGetCalculationExecutionCode $
--             newGetCalculationExecutionCode
--
--         , requestGetCalculationExecutionStatus $
--             newGetCalculationExecutionStatus
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
--         , requestGetNotebookMetadata $
--             newGetNotebookMetadata
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
--         , requestGetQueryRuntimeStatistics $
--             newGetQueryRuntimeStatistics
--
--         , requestGetSession $
--             newGetSession
--
--         , requestGetSessionStatus $
--             newGetSessionStatus
--
--         , requestGetTableMetadata $
--             newGetTableMetadata
--
--         , requestGetWorkGroup $
--             newGetWorkGroup
--
--         , requestImportNotebook $
--             newImportNotebook
--
--         , requestListApplicationDPUSizes $
--             newListApplicationDPUSizes
--
--         , requestListCalculationExecutions $
--             newListCalculationExecutions
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
--         , requestListExecutors $
--             newListExecutors
--
--         , requestListNamedQueries $
--             newListNamedQueries
--
--         , requestListNotebookMetadata $
--             newListNotebookMetadata
--
--         , requestListNotebookSessions $
--             newListNotebookSessions
--
--         , requestListPreparedStatements $
--             newListPreparedStatements
--
--         , requestListQueryExecutions $
--             newListQueryExecutions
--
--         , requestListSessions $
--             newListSessions
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
--         , requestStartCalculationExecution $
--             newStartCalculationExecution
--
--         , requestStartQueryExecution $
--             newStartQueryExecution
--
--         , requestStartSession $
--             newStartSession
--
--         , requestStopCalculationExecution $
--             newStopCalculationExecution
--
--         , requestStopQueryExecution $
--             newStopQueryExecution
--
--         , requestTagResource $
--             newTagResource
--
--         , requestTerminateSession $
--             newTerminateSession
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateDataCatalog $
--             newUpdateDataCatalog
--
--         , requestUpdateNamedQuery $
--             newUpdateNamedQuery
--
--         , requestUpdateNotebook $
--             newUpdateNotebook
--
--         , requestUpdateNotebookMetadata $
--             newUpdateNotebookMetadata
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
--         , responseBatchGetPreparedStatement $
--             newBatchGetPreparedStatementResponse
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
--         , responseCreateNotebook $
--             newCreateNotebookResponse
--
--         , responseCreatePreparedStatement $
--             newCreatePreparedStatementResponse
--
--         , responseCreatePresignedNotebookUrl $
--             newCreatePresignedNotebookUrlResponse
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
--         , responseDeleteNotebook $
--             newDeleteNotebookResponse
--
--         , responseDeletePreparedStatement $
--             newDeletePreparedStatementResponse
--
--         , responseDeleteWorkGroup $
--             newDeleteWorkGroupResponse
--
--         , responseExportNotebook $
--             newExportNotebookResponse
--
--         , responseGetCalculationExecution $
--             newGetCalculationExecutionResponse
--
--         , responseGetCalculationExecutionCode $
--             newGetCalculationExecutionCodeResponse
--
--         , responseGetCalculationExecutionStatus $
--             newGetCalculationExecutionStatusResponse
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
--         , responseGetNotebookMetadata $
--             newGetNotebookMetadataResponse
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
--         , responseGetQueryRuntimeStatistics $
--             newGetQueryRuntimeStatisticsResponse
--
--         , responseGetSession $
--             newGetSessionResponse
--
--         , responseGetSessionStatus $
--             newGetSessionStatusResponse
--
--         , responseGetTableMetadata $
--             newGetTableMetadataResponse
--
--         , responseGetWorkGroup $
--             newGetWorkGroupResponse
--
--         , responseImportNotebook $
--             newImportNotebookResponse
--
--         , responseListApplicationDPUSizes $
--             newListApplicationDPUSizesResponse
--
--         , responseListCalculationExecutions $
--             newListCalculationExecutionsResponse
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
--         , responseListExecutors $
--             newListExecutorsResponse
--
--         , responseListNamedQueries $
--             newListNamedQueriesResponse
--
--         , responseListNotebookMetadata $
--             newListNotebookMetadataResponse
--
--         , responseListNotebookSessions $
--             newListNotebookSessionsResponse
--
--         , responseListPreparedStatements $
--             newListPreparedStatementsResponse
--
--         , responseListQueryExecutions $
--             newListQueryExecutionsResponse
--
--         , responseListSessions $
--             newListSessionsResponse
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
--         , responseStartCalculationExecution $
--             newStartCalculationExecutionResponse
--
--         , responseStartQueryExecution $
--             newStartQueryExecutionResponse
--
--         , responseStartSession $
--             newStartSessionResponse
--
--         , responseStopCalculationExecution $
--             newStopCalculationExecutionResponse
--
--         , responseStopQueryExecution $
--             newStopQueryExecutionResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseTerminateSession $
--             newTerminateSessionResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateDataCatalog $
--             newUpdateDataCatalogResponse
--
--         , responseUpdateNamedQuery $
--             newUpdateNamedQueryResponse
--
--         , responseUpdateNotebook $
--             newUpdateNotebookResponse
--
--         , responseUpdateNotebookMetadata $
--             newUpdateNotebookMetadataResponse
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

requestBatchGetPreparedStatement :: BatchGetPreparedStatement -> TestTree
requestBatchGetPreparedStatement =
  req
    "BatchGetPreparedStatement"
    "fixture/BatchGetPreparedStatement.yaml"

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

requestCreateNotebook :: CreateNotebook -> TestTree
requestCreateNotebook =
  req
    "CreateNotebook"
    "fixture/CreateNotebook.yaml"

requestCreatePreparedStatement :: CreatePreparedStatement -> TestTree
requestCreatePreparedStatement =
  req
    "CreatePreparedStatement"
    "fixture/CreatePreparedStatement.yaml"

requestCreatePresignedNotebookUrl :: CreatePresignedNotebookUrl -> TestTree
requestCreatePresignedNotebookUrl =
  req
    "CreatePresignedNotebookUrl"
    "fixture/CreatePresignedNotebookUrl.yaml"

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

requestDeleteNotebook :: DeleteNotebook -> TestTree
requestDeleteNotebook =
  req
    "DeleteNotebook"
    "fixture/DeleteNotebook.yaml"

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

requestExportNotebook :: ExportNotebook -> TestTree
requestExportNotebook =
  req
    "ExportNotebook"
    "fixture/ExportNotebook.yaml"

requestGetCalculationExecution :: GetCalculationExecution -> TestTree
requestGetCalculationExecution =
  req
    "GetCalculationExecution"
    "fixture/GetCalculationExecution.yaml"

requestGetCalculationExecutionCode :: GetCalculationExecutionCode -> TestTree
requestGetCalculationExecutionCode =
  req
    "GetCalculationExecutionCode"
    "fixture/GetCalculationExecutionCode.yaml"

requestGetCalculationExecutionStatus :: GetCalculationExecutionStatus -> TestTree
requestGetCalculationExecutionStatus =
  req
    "GetCalculationExecutionStatus"
    "fixture/GetCalculationExecutionStatus.yaml"

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

requestGetNotebookMetadata :: GetNotebookMetadata -> TestTree
requestGetNotebookMetadata =
  req
    "GetNotebookMetadata"
    "fixture/GetNotebookMetadata.yaml"

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

requestGetQueryRuntimeStatistics :: GetQueryRuntimeStatistics -> TestTree
requestGetQueryRuntimeStatistics =
  req
    "GetQueryRuntimeStatistics"
    "fixture/GetQueryRuntimeStatistics.yaml"

requestGetSession :: GetSession -> TestTree
requestGetSession =
  req
    "GetSession"
    "fixture/GetSession.yaml"

requestGetSessionStatus :: GetSessionStatus -> TestTree
requestGetSessionStatus =
  req
    "GetSessionStatus"
    "fixture/GetSessionStatus.yaml"

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

requestImportNotebook :: ImportNotebook -> TestTree
requestImportNotebook =
  req
    "ImportNotebook"
    "fixture/ImportNotebook.yaml"

requestListApplicationDPUSizes :: ListApplicationDPUSizes -> TestTree
requestListApplicationDPUSizes =
  req
    "ListApplicationDPUSizes"
    "fixture/ListApplicationDPUSizes.yaml"

requestListCalculationExecutions :: ListCalculationExecutions -> TestTree
requestListCalculationExecutions =
  req
    "ListCalculationExecutions"
    "fixture/ListCalculationExecutions.yaml"

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

requestListExecutors :: ListExecutors -> TestTree
requestListExecutors =
  req
    "ListExecutors"
    "fixture/ListExecutors.yaml"

requestListNamedQueries :: ListNamedQueries -> TestTree
requestListNamedQueries =
  req
    "ListNamedQueries"
    "fixture/ListNamedQueries.yaml"

requestListNotebookMetadata :: ListNotebookMetadata -> TestTree
requestListNotebookMetadata =
  req
    "ListNotebookMetadata"
    "fixture/ListNotebookMetadata.yaml"

requestListNotebookSessions :: ListNotebookSessions -> TestTree
requestListNotebookSessions =
  req
    "ListNotebookSessions"
    "fixture/ListNotebookSessions.yaml"

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

requestListSessions :: ListSessions -> TestTree
requestListSessions =
  req
    "ListSessions"
    "fixture/ListSessions.yaml"

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

requestStartCalculationExecution :: StartCalculationExecution -> TestTree
requestStartCalculationExecution =
  req
    "StartCalculationExecution"
    "fixture/StartCalculationExecution.yaml"

requestStartQueryExecution :: StartQueryExecution -> TestTree
requestStartQueryExecution =
  req
    "StartQueryExecution"
    "fixture/StartQueryExecution.yaml"

requestStartSession :: StartSession -> TestTree
requestStartSession =
  req
    "StartSession"
    "fixture/StartSession.yaml"

requestStopCalculationExecution :: StopCalculationExecution -> TestTree
requestStopCalculationExecution =
  req
    "StopCalculationExecution"
    "fixture/StopCalculationExecution.yaml"

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

requestTerminateSession :: TerminateSession -> TestTree
requestTerminateSession =
  req
    "TerminateSession"
    "fixture/TerminateSession.yaml"

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

requestUpdateNamedQuery :: UpdateNamedQuery -> TestTree
requestUpdateNamedQuery =
  req
    "UpdateNamedQuery"
    "fixture/UpdateNamedQuery.yaml"

requestUpdateNotebook :: UpdateNotebook -> TestTree
requestUpdateNotebook =
  req
    "UpdateNotebook"
    "fixture/UpdateNotebook.yaml"

requestUpdateNotebookMetadata :: UpdateNotebookMetadata -> TestTree
requestUpdateNotebookMetadata =
  req
    "UpdateNotebookMetadata"
    "fixture/UpdateNotebookMetadata.yaml"

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

responseBatchGetPreparedStatement :: BatchGetPreparedStatementResponse -> TestTree
responseBatchGetPreparedStatement =
  res
    "BatchGetPreparedStatementResponse"
    "fixture/BatchGetPreparedStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetPreparedStatement)

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

responseCreateNotebook :: CreateNotebookResponse -> TestTree
responseCreateNotebook =
  res
    "CreateNotebookResponse"
    "fixture/CreateNotebookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNotebook)

responseCreatePreparedStatement :: CreatePreparedStatementResponse -> TestTree
responseCreatePreparedStatement =
  res
    "CreatePreparedStatementResponse"
    "fixture/CreatePreparedStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePreparedStatement)

responseCreatePresignedNotebookUrl :: CreatePresignedNotebookUrlResponse -> TestTree
responseCreatePresignedNotebookUrl =
  res
    "CreatePresignedNotebookUrlResponse"
    "fixture/CreatePresignedNotebookUrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePresignedNotebookUrl)

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

responseDeleteNotebook :: DeleteNotebookResponse -> TestTree
responseDeleteNotebook =
  res
    "DeleteNotebookResponse"
    "fixture/DeleteNotebookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNotebook)

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

responseExportNotebook :: ExportNotebookResponse -> TestTree
responseExportNotebook =
  res
    "ExportNotebookResponse"
    "fixture/ExportNotebookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportNotebook)

responseGetCalculationExecution :: GetCalculationExecutionResponse -> TestTree
responseGetCalculationExecution =
  res
    "GetCalculationExecutionResponse"
    "fixture/GetCalculationExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCalculationExecution)

responseGetCalculationExecutionCode :: GetCalculationExecutionCodeResponse -> TestTree
responseGetCalculationExecutionCode =
  res
    "GetCalculationExecutionCodeResponse"
    "fixture/GetCalculationExecutionCodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCalculationExecutionCode)

responseGetCalculationExecutionStatus :: GetCalculationExecutionStatusResponse -> TestTree
responseGetCalculationExecutionStatus =
  res
    "GetCalculationExecutionStatusResponse"
    "fixture/GetCalculationExecutionStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCalculationExecutionStatus)

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

responseGetNotebookMetadata :: GetNotebookMetadataResponse -> TestTree
responseGetNotebookMetadata =
  res
    "GetNotebookMetadataResponse"
    "fixture/GetNotebookMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetNotebookMetadata)

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

responseGetQueryRuntimeStatistics :: GetQueryRuntimeStatisticsResponse -> TestTree
responseGetQueryRuntimeStatistics =
  res
    "GetQueryRuntimeStatisticsResponse"
    "fixture/GetQueryRuntimeStatisticsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetQueryRuntimeStatistics)

responseGetSession :: GetSessionResponse -> TestTree
responseGetSession =
  res
    "GetSessionResponse"
    "fixture/GetSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSession)

responseGetSessionStatus :: GetSessionStatusResponse -> TestTree
responseGetSessionStatus =
  res
    "GetSessionStatusResponse"
    "fixture/GetSessionStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSessionStatus)

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

responseImportNotebook :: ImportNotebookResponse -> TestTree
responseImportNotebook =
  res
    "ImportNotebookResponse"
    "fixture/ImportNotebookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportNotebook)

responseListApplicationDPUSizes :: ListApplicationDPUSizesResponse -> TestTree
responseListApplicationDPUSizes =
  res
    "ListApplicationDPUSizesResponse"
    "fixture/ListApplicationDPUSizesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApplicationDPUSizes)

responseListCalculationExecutions :: ListCalculationExecutionsResponse -> TestTree
responseListCalculationExecutions =
  res
    "ListCalculationExecutionsResponse"
    "fixture/ListCalculationExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCalculationExecutions)

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

responseListExecutors :: ListExecutorsResponse -> TestTree
responseListExecutors =
  res
    "ListExecutorsResponse"
    "fixture/ListExecutorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListExecutors)

responseListNamedQueries :: ListNamedQueriesResponse -> TestTree
responseListNamedQueries =
  res
    "ListNamedQueriesResponse"
    "fixture/ListNamedQueriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNamedQueries)

responseListNotebookMetadata :: ListNotebookMetadataResponse -> TestTree
responseListNotebookMetadata =
  res
    "ListNotebookMetadataResponse"
    "fixture/ListNotebookMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNotebookMetadata)

responseListNotebookSessions :: ListNotebookSessionsResponse -> TestTree
responseListNotebookSessions =
  res
    "ListNotebookSessionsResponse"
    "fixture/ListNotebookSessionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNotebookSessions)

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

responseListSessions :: ListSessionsResponse -> TestTree
responseListSessions =
  res
    "ListSessionsResponse"
    "fixture/ListSessionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSessions)

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

responseStartCalculationExecution :: StartCalculationExecutionResponse -> TestTree
responseStartCalculationExecution =
  res
    "StartCalculationExecutionResponse"
    "fixture/StartCalculationExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartCalculationExecution)

responseStartQueryExecution :: StartQueryExecutionResponse -> TestTree
responseStartQueryExecution =
  res
    "StartQueryExecutionResponse"
    "fixture/StartQueryExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartQueryExecution)

responseStartSession :: StartSessionResponse -> TestTree
responseStartSession =
  res
    "StartSessionResponse"
    "fixture/StartSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartSession)

responseStopCalculationExecution :: StopCalculationExecutionResponse -> TestTree
responseStopCalculationExecution =
  res
    "StopCalculationExecutionResponse"
    "fixture/StopCalculationExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopCalculationExecution)

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

responseTerminateSession :: TerminateSessionResponse -> TestTree
responseTerminateSession =
  res
    "TerminateSessionResponse"
    "fixture/TerminateSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TerminateSession)

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

responseUpdateNamedQuery :: UpdateNamedQueryResponse -> TestTree
responseUpdateNamedQuery =
  res
    "UpdateNamedQueryResponse"
    "fixture/UpdateNamedQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNamedQuery)

responseUpdateNotebook :: UpdateNotebookResponse -> TestTree
responseUpdateNotebook =
  res
    "UpdateNotebookResponse"
    "fixture/UpdateNotebookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNotebook)

responseUpdateNotebookMetadata :: UpdateNotebookMetadataResponse -> TestTree
responseUpdateNotebookMetadata =
  res
    "UpdateNotebookMetadataResponse"
    "fixture/UpdateNotebookMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNotebookMetadata)

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
