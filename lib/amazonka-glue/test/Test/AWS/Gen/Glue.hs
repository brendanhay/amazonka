{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Glue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Glue where

import Data.Proxy
import Network.AWS.Glue
import Test.AWS.Fixture
import Test.AWS.Glue.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestStartCrawler $
--             startCrawler
--
--         , requestGetCatalogImportStatus $
--             getCatalogImportStatus
--
--         , requestGetPartition $
--             getPartition
--
--         , requestCreateTrigger $
--             createTrigger
--
--         , requestDeleteTable $
--             deleteTable
--
--         , requestUpdateTable $
--             updateTable
--
--         , requestDeleteConnection $
--             deleteConnection
--
--         , requestUpdateConnection $
--             updateConnection
--
--         , requestGetUserDefinedFunctions $
--             getUserDefinedFunctions
--
--         , requestBatchCreatePartition $
--             batchCreatePartition
--
--         , requestGetMapping $
--             getMapping
--
--         , requestGetTableVersion $
--             getTableVersion
--
--         , requestGetJobs $
--             getJobs
--
--         , requestGetClassifiers $
--             getClassifiers
--
--         , requestCreateConnection $
--             createConnection
--
--         , requestDeleteTableVersion $
--             deleteTableVersion
--
--         , requestDeleteDevEndpoint $
--             deleteDevEndpoint
--
--         , requestUpdateDevEndpoint $
--             updateDevEndpoint
--
--         , requestDeleteCrawler $
--             deleteCrawler
--
--         , requestUpdateCrawler $
--             updateCrawler
--
--         , requestGetConnection $
--             getConnection
--
--         , requestBatchGetPartition $
--             batchGetPartition
--
--         , requestStopTrigger $
--             stopTrigger
--
--         , requestUpdateCrawlerSchedule $
--             updateCrawlerSchedule
--
--         , requestDeleteUserDefinedFunction $
--             deleteUserDefinedFunction
--
--         , requestUpdateUserDefinedFunction $
--             updateUserDefinedFunction
--
--         , requestBatchDeleteTable $
--             batchDeleteTable
--
--         , requestGetTables $
--             getTables
--
--         , requestCreateClassifier $
--             createClassifier
--
--         , requestBatchDeleteConnection $
--             batchDeleteConnection
--
--         , requestCreateJob $
--             createJob
--
--         , requestGetJobRuns $
--             getJobRuns
--
--         , requestCreateUserDefinedFunction $
--             createUserDefinedFunction
--
--         , requestResetJobBookmark $
--             resetJobBookmark
--
--         , requestDeleteJob $
--             deleteJob
--
--         , requestUpdateJob $
--             updateJob
--
--         , requestGetCrawlers $
--             getCrawlers
--
--         , requestGetClassifier $
--             getClassifier
--
--         , requestGetJob $
--             getJob
--
--         , requestBatchDeleteTableVersion $
--             batchDeleteTableVersion
--
--         , requestGetDevEndpoints $
--             getDevEndpoints
--
--         , requestStartCrawlerSchedule $
--             startCrawlerSchedule
--
--         , requestGetUserDefinedFunction $
--             getUserDefinedFunction
--
--         , requestDeleteDatabase $
--             deleteDatabase
--
--         , requestUpdateDatabase $
--             updateDatabase
--
--         , requestStopCrawler $
--             stopCrawler
--
--         , requestGetPartitions $
--             getPartitions
--
--         , requestBatchDeletePartition $
--             batchDeletePartition
--
--         , requestGetCrawler $
--             getCrawler
--
--         , requestBatchStopJobRun $
--             batchStopJobRun
--
--         , requestGetDevEndpoint $
--             getDevEndpoint
--
--         , requestCreateTable $
--             createTable
--
--         , requestGetCrawlerMetrics $
--             getCrawlerMetrics
--
--         , requestGetPlan $
--             getPlan
--
--         , requestGetTriggers $
--             getTriggers
--
--         , requestStartTrigger $
--             startTrigger
--
--         , requestGetDataflowGraph $
--             getDataflowGraph
--
--         , requestGetDatabases $
--             getDatabases
--
--         , requestGetTable $
--             getTable
--
--         , requestCreateCrawler $
--             createCrawler
--
--         , requestGetJobRun $
--             getJobRun
--
--         , requestCreateDevEndpoint $
--             createDevEndpoint
--
--         , requestGetDatabase $
--             getDatabase
--
--         , requestDeletePartition $
--             deletePartition
--
--         , requestUpdatePartition $
--             updatePartition
--
--         , requestCreateScript $
--             createScript
--
--         , requestGetConnections $
--             getConnections
--
--         , requestGetTrigger $
--             getTrigger
--
--         , requestImportCatalogToGlue $
--             importCatalogToGlue
--
--         , requestDeleteClassifier $
--             deleteClassifier
--
--         , requestUpdateClassifier $
--             updateClassifier
--
--         , requestStartJobRun $
--             startJobRun
--
--         , requestCreatePartition $
--             createPartition
--
--         , requestStopCrawlerSchedule $
--             stopCrawlerSchedule
--
--         , requestCreateDatabase $
--             createDatabase
--
--         , requestGetTableVersions $
--             getTableVersions
--
--         , requestDeleteTrigger $
--             deleteTrigger
--
--         , requestUpdateTrigger $
--             updateTrigger
--
--           ]

--     , testGroup "response"
--         [ responseStartCrawler $
--             startCrawlerResponse
--
--         , responseGetCatalogImportStatus $
--             getCatalogImportStatusResponse
--
--         , responseGetPartition $
--             getPartitionResponse
--
--         , responseCreateTrigger $
--             createTriggerResponse
--
--         , responseDeleteTable $
--             deleteTableResponse
--
--         , responseUpdateTable $
--             updateTableResponse
--
--         , responseDeleteConnection $
--             deleteConnectionResponse
--
--         , responseUpdateConnection $
--             updateConnectionResponse
--
--         , responseGetUserDefinedFunctions $
--             getUserDefinedFunctionsResponse
--
--         , responseBatchCreatePartition $
--             batchCreatePartitionResponse
--
--         , responseGetMapping $
--             getMappingResponse
--
--         , responseGetTableVersion $
--             getTableVersionResponse
--
--         , responseGetJobs $
--             getJobsResponse
--
--         , responseGetClassifiers $
--             getClassifiersResponse
--
--         , responseCreateConnection $
--             createConnectionResponse
--
--         , responseDeleteTableVersion $
--             deleteTableVersionResponse
--
--         , responseDeleteDevEndpoint $
--             deleteDevEndpointResponse
--
--         , responseUpdateDevEndpoint $
--             updateDevEndpointResponse
--
--         , responseDeleteCrawler $
--             deleteCrawlerResponse
--
--         , responseUpdateCrawler $
--             updateCrawlerResponse
--
--         , responseGetConnection $
--             getConnectionResponse
--
--         , responseBatchGetPartition $
--             batchGetPartitionResponse
--
--         , responseStopTrigger $
--             stopTriggerResponse
--
--         , responseUpdateCrawlerSchedule $
--             updateCrawlerScheduleResponse
--
--         , responseDeleteUserDefinedFunction $
--             deleteUserDefinedFunctionResponse
--
--         , responseUpdateUserDefinedFunction $
--             updateUserDefinedFunctionResponse
--
--         , responseBatchDeleteTable $
--             batchDeleteTableResponse
--
--         , responseGetTables $
--             getTablesResponse
--
--         , responseCreateClassifier $
--             createClassifierResponse
--
--         , responseBatchDeleteConnection $
--             batchDeleteConnectionResponse
--
--         , responseCreateJob $
--             createJobResponse
--
--         , responseGetJobRuns $
--             getJobRunsResponse
--
--         , responseCreateUserDefinedFunction $
--             createUserDefinedFunctionResponse
--
--         , responseResetJobBookmark $
--             resetJobBookmarkResponse
--
--         , responseDeleteJob $
--             deleteJobResponse
--
--         , responseUpdateJob $
--             updateJobResponse
--
--         , responseGetCrawlers $
--             getCrawlersResponse
--
--         , responseGetClassifier $
--             getClassifierResponse
--
--         , responseGetJob $
--             getJobResponse
--
--         , responseBatchDeleteTableVersion $
--             batchDeleteTableVersionResponse
--
--         , responseGetDevEndpoints $
--             getDevEndpointsResponse
--
--         , responseStartCrawlerSchedule $
--             startCrawlerScheduleResponse
--
--         , responseGetUserDefinedFunction $
--             getUserDefinedFunctionResponse
--
--         , responseDeleteDatabase $
--             deleteDatabaseResponse
--
--         , responseUpdateDatabase $
--             updateDatabaseResponse
--
--         , responseStopCrawler $
--             stopCrawlerResponse
--
--         , responseGetPartitions $
--             getPartitionsResponse
--
--         , responseBatchDeletePartition $
--             batchDeletePartitionResponse
--
--         , responseGetCrawler $
--             getCrawlerResponse
--
--         , responseBatchStopJobRun $
--             batchStopJobRunResponse
--
--         , responseGetDevEndpoint $
--             getDevEndpointResponse
--
--         , responseCreateTable $
--             createTableResponse
--
--         , responseGetCrawlerMetrics $
--             getCrawlerMetricsResponse
--
--         , responseGetPlan $
--             getPlanResponse
--
--         , responseGetTriggers $
--             getTriggersResponse
--
--         , responseStartTrigger $
--             startTriggerResponse
--
--         , responseGetDataflowGraph $
--             getDataflowGraphResponse
--
--         , responseGetDatabases $
--             getDatabasesResponse
--
--         , responseGetTable $
--             getTableResponse
--
--         , responseCreateCrawler $
--             createCrawlerResponse
--
--         , responseGetJobRun $
--             getJobRunResponse
--
--         , responseCreateDevEndpoint $
--             createDevEndpointResponse
--
--         , responseGetDatabase $
--             getDatabaseResponse
--
--         , responseDeletePartition $
--             deletePartitionResponse
--
--         , responseUpdatePartition $
--             updatePartitionResponse
--
--         , responseCreateScript $
--             createScriptResponse
--
--         , responseGetConnections $
--             getConnectionsResponse
--
--         , responseGetTrigger $
--             getTriggerResponse
--
--         , responseImportCatalogToGlue $
--             importCatalogToGlueResponse
--
--         , responseDeleteClassifier $
--             deleteClassifierResponse
--
--         , responseUpdateClassifier $
--             updateClassifierResponse
--
--         , responseStartJobRun $
--             startJobRunResponse
--
--         , responseCreatePartition $
--             createPartitionResponse
--
--         , responseStopCrawlerSchedule $
--             stopCrawlerScheduleResponse
--
--         , responseCreateDatabase $
--             createDatabaseResponse
--
--         , responseGetTableVersions $
--             getTableVersionsResponse
--
--         , responseDeleteTrigger $
--             deleteTriggerResponse
--
--         , responseUpdateTrigger $
--             updateTriggerResponse
--
--           ]
--     ]

-- Requests

requestStartCrawler :: StartCrawler -> TestTree
requestStartCrawler = req
    "StartCrawler"
    "fixture/StartCrawler.yaml"

requestGetCatalogImportStatus :: GetCatalogImportStatus -> TestTree
requestGetCatalogImportStatus = req
    "GetCatalogImportStatus"
    "fixture/GetCatalogImportStatus.yaml"

requestGetPartition :: GetPartition -> TestTree
requestGetPartition = req
    "GetPartition"
    "fixture/GetPartition.yaml"

requestCreateTrigger :: CreateTrigger -> TestTree
requestCreateTrigger = req
    "CreateTrigger"
    "fixture/CreateTrigger.yaml"

requestDeleteTable :: DeleteTable -> TestTree
requestDeleteTable = req
    "DeleteTable"
    "fixture/DeleteTable.yaml"

requestUpdateTable :: UpdateTable -> TestTree
requestUpdateTable = req
    "UpdateTable"
    "fixture/UpdateTable.yaml"

requestDeleteConnection :: DeleteConnection -> TestTree
requestDeleteConnection = req
    "DeleteConnection"
    "fixture/DeleteConnection.yaml"

requestUpdateConnection :: UpdateConnection -> TestTree
requestUpdateConnection = req
    "UpdateConnection"
    "fixture/UpdateConnection.yaml"

requestGetUserDefinedFunctions :: GetUserDefinedFunctions -> TestTree
requestGetUserDefinedFunctions = req
    "GetUserDefinedFunctions"
    "fixture/GetUserDefinedFunctions.yaml"

requestBatchCreatePartition :: BatchCreatePartition -> TestTree
requestBatchCreatePartition = req
    "BatchCreatePartition"
    "fixture/BatchCreatePartition.yaml"

requestGetMapping :: GetMapping -> TestTree
requestGetMapping = req
    "GetMapping"
    "fixture/GetMapping.yaml"

requestGetTableVersion :: GetTableVersion -> TestTree
requestGetTableVersion = req
    "GetTableVersion"
    "fixture/GetTableVersion.yaml"

requestGetJobs :: GetJobs -> TestTree
requestGetJobs = req
    "GetJobs"
    "fixture/GetJobs.yaml"

requestGetClassifiers :: GetClassifiers -> TestTree
requestGetClassifiers = req
    "GetClassifiers"
    "fixture/GetClassifiers.yaml"

requestCreateConnection :: CreateConnection -> TestTree
requestCreateConnection = req
    "CreateConnection"
    "fixture/CreateConnection.yaml"

requestDeleteTableVersion :: DeleteTableVersion -> TestTree
requestDeleteTableVersion = req
    "DeleteTableVersion"
    "fixture/DeleteTableVersion.yaml"

requestDeleteDevEndpoint :: DeleteDevEndpoint -> TestTree
requestDeleteDevEndpoint = req
    "DeleteDevEndpoint"
    "fixture/DeleteDevEndpoint.yaml"

requestUpdateDevEndpoint :: UpdateDevEndpoint -> TestTree
requestUpdateDevEndpoint = req
    "UpdateDevEndpoint"
    "fixture/UpdateDevEndpoint.yaml"

requestDeleteCrawler :: DeleteCrawler -> TestTree
requestDeleteCrawler = req
    "DeleteCrawler"
    "fixture/DeleteCrawler.yaml"

requestUpdateCrawler :: UpdateCrawler -> TestTree
requestUpdateCrawler = req
    "UpdateCrawler"
    "fixture/UpdateCrawler.yaml"

requestGetConnection :: GetConnection -> TestTree
requestGetConnection = req
    "GetConnection"
    "fixture/GetConnection.yaml"

requestBatchGetPartition :: BatchGetPartition -> TestTree
requestBatchGetPartition = req
    "BatchGetPartition"
    "fixture/BatchGetPartition.yaml"

requestStopTrigger :: StopTrigger -> TestTree
requestStopTrigger = req
    "StopTrigger"
    "fixture/StopTrigger.yaml"

requestUpdateCrawlerSchedule :: UpdateCrawlerSchedule -> TestTree
requestUpdateCrawlerSchedule = req
    "UpdateCrawlerSchedule"
    "fixture/UpdateCrawlerSchedule.yaml"

requestDeleteUserDefinedFunction :: DeleteUserDefinedFunction -> TestTree
requestDeleteUserDefinedFunction = req
    "DeleteUserDefinedFunction"
    "fixture/DeleteUserDefinedFunction.yaml"

requestUpdateUserDefinedFunction :: UpdateUserDefinedFunction -> TestTree
requestUpdateUserDefinedFunction = req
    "UpdateUserDefinedFunction"
    "fixture/UpdateUserDefinedFunction.yaml"

requestBatchDeleteTable :: BatchDeleteTable -> TestTree
requestBatchDeleteTable = req
    "BatchDeleteTable"
    "fixture/BatchDeleteTable.yaml"

requestGetTables :: GetTables -> TestTree
requestGetTables = req
    "GetTables"
    "fixture/GetTables.yaml"

requestCreateClassifier :: CreateClassifier -> TestTree
requestCreateClassifier = req
    "CreateClassifier"
    "fixture/CreateClassifier.yaml"

requestBatchDeleteConnection :: BatchDeleteConnection -> TestTree
requestBatchDeleteConnection = req
    "BatchDeleteConnection"
    "fixture/BatchDeleteConnection.yaml"

requestCreateJob :: CreateJob -> TestTree
requestCreateJob = req
    "CreateJob"
    "fixture/CreateJob.yaml"

requestGetJobRuns :: GetJobRuns -> TestTree
requestGetJobRuns = req
    "GetJobRuns"
    "fixture/GetJobRuns.yaml"

requestCreateUserDefinedFunction :: CreateUserDefinedFunction -> TestTree
requestCreateUserDefinedFunction = req
    "CreateUserDefinedFunction"
    "fixture/CreateUserDefinedFunction.yaml"

requestResetJobBookmark :: ResetJobBookmark -> TestTree
requestResetJobBookmark = req
    "ResetJobBookmark"
    "fixture/ResetJobBookmark.yaml"

requestDeleteJob :: DeleteJob -> TestTree
requestDeleteJob = req
    "DeleteJob"
    "fixture/DeleteJob.yaml"

requestUpdateJob :: UpdateJob -> TestTree
requestUpdateJob = req
    "UpdateJob"
    "fixture/UpdateJob.yaml"

requestGetCrawlers :: GetCrawlers -> TestTree
requestGetCrawlers = req
    "GetCrawlers"
    "fixture/GetCrawlers.yaml"

requestGetClassifier :: GetClassifier -> TestTree
requestGetClassifier = req
    "GetClassifier"
    "fixture/GetClassifier.yaml"

requestGetJob :: GetJob -> TestTree
requestGetJob = req
    "GetJob"
    "fixture/GetJob.yaml"

requestBatchDeleteTableVersion :: BatchDeleteTableVersion -> TestTree
requestBatchDeleteTableVersion = req
    "BatchDeleteTableVersion"
    "fixture/BatchDeleteTableVersion.yaml"

requestGetDevEndpoints :: GetDevEndpoints -> TestTree
requestGetDevEndpoints = req
    "GetDevEndpoints"
    "fixture/GetDevEndpoints.yaml"

requestStartCrawlerSchedule :: StartCrawlerSchedule -> TestTree
requestStartCrawlerSchedule = req
    "StartCrawlerSchedule"
    "fixture/StartCrawlerSchedule.yaml"

requestGetUserDefinedFunction :: GetUserDefinedFunction -> TestTree
requestGetUserDefinedFunction = req
    "GetUserDefinedFunction"
    "fixture/GetUserDefinedFunction.yaml"

requestDeleteDatabase :: DeleteDatabase -> TestTree
requestDeleteDatabase = req
    "DeleteDatabase"
    "fixture/DeleteDatabase.yaml"

requestUpdateDatabase :: UpdateDatabase -> TestTree
requestUpdateDatabase = req
    "UpdateDatabase"
    "fixture/UpdateDatabase.yaml"

requestStopCrawler :: StopCrawler -> TestTree
requestStopCrawler = req
    "StopCrawler"
    "fixture/StopCrawler.yaml"

requestGetPartitions :: GetPartitions -> TestTree
requestGetPartitions = req
    "GetPartitions"
    "fixture/GetPartitions.yaml"

requestBatchDeletePartition :: BatchDeletePartition -> TestTree
requestBatchDeletePartition = req
    "BatchDeletePartition"
    "fixture/BatchDeletePartition.yaml"

requestGetCrawler :: GetCrawler -> TestTree
requestGetCrawler = req
    "GetCrawler"
    "fixture/GetCrawler.yaml"

requestBatchStopJobRun :: BatchStopJobRun -> TestTree
requestBatchStopJobRun = req
    "BatchStopJobRun"
    "fixture/BatchStopJobRun.yaml"

requestGetDevEndpoint :: GetDevEndpoint -> TestTree
requestGetDevEndpoint = req
    "GetDevEndpoint"
    "fixture/GetDevEndpoint.yaml"

requestCreateTable :: CreateTable -> TestTree
requestCreateTable = req
    "CreateTable"
    "fixture/CreateTable.yaml"

requestGetCrawlerMetrics :: GetCrawlerMetrics -> TestTree
requestGetCrawlerMetrics = req
    "GetCrawlerMetrics"
    "fixture/GetCrawlerMetrics.yaml"

requestGetPlan :: GetPlan -> TestTree
requestGetPlan = req
    "GetPlan"
    "fixture/GetPlan.yaml"

requestGetTriggers :: GetTriggers -> TestTree
requestGetTriggers = req
    "GetTriggers"
    "fixture/GetTriggers.yaml"

requestStartTrigger :: StartTrigger -> TestTree
requestStartTrigger = req
    "StartTrigger"
    "fixture/StartTrigger.yaml"

requestGetDataflowGraph :: GetDataflowGraph -> TestTree
requestGetDataflowGraph = req
    "GetDataflowGraph"
    "fixture/GetDataflowGraph.yaml"

requestGetDatabases :: GetDatabases -> TestTree
requestGetDatabases = req
    "GetDatabases"
    "fixture/GetDatabases.yaml"

requestGetTable :: GetTable -> TestTree
requestGetTable = req
    "GetTable"
    "fixture/GetTable.yaml"

requestCreateCrawler :: CreateCrawler -> TestTree
requestCreateCrawler = req
    "CreateCrawler"
    "fixture/CreateCrawler.yaml"

requestGetJobRun :: GetJobRun -> TestTree
requestGetJobRun = req
    "GetJobRun"
    "fixture/GetJobRun.yaml"

requestCreateDevEndpoint :: CreateDevEndpoint -> TestTree
requestCreateDevEndpoint = req
    "CreateDevEndpoint"
    "fixture/CreateDevEndpoint.yaml"

requestGetDatabase :: GetDatabase -> TestTree
requestGetDatabase = req
    "GetDatabase"
    "fixture/GetDatabase.yaml"

requestDeletePartition :: DeletePartition -> TestTree
requestDeletePartition = req
    "DeletePartition"
    "fixture/DeletePartition.yaml"

requestUpdatePartition :: UpdatePartition -> TestTree
requestUpdatePartition = req
    "UpdatePartition"
    "fixture/UpdatePartition.yaml"

requestCreateScript :: CreateScript -> TestTree
requestCreateScript = req
    "CreateScript"
    "fixture/CreateScript.yaml"

requestGetConnections :: GetConnections -> TestTree
requestGetConnections = req
    "GetConnections"
    "fixture/GetConnections.yaml"

requestGetTrigger :: GetTrigger -> TestTree
requestGetTrigger = req
    "GetTrigger"
    "fixture/GetTrigger.yaml"

requestImportCatalogToGlue :: ImportCatalogToGlue -> TestTree
requestImportCatalogToGlue = req
    "ImportCatalogToGlue"
    "fixture/ImportCatalogToGlue.yaml"

requestDeleteClassifier :: DeleteClassifier -> TestTree
requestDeleteClassifier = req
    "DeleteClassifier"
    "fixture/DeleteClassifier.yaml"

requestUpdateClassifier :: UpdateClassifier -> TestTree
requestUpdateClassifier = req
    "UpdateClassifier"
    "fixture/UpdateClassifier.yaml"

requestStartJobRun :: StartJobRun -> TestTree
requestStartJobRun = req
    "StartJobRun"
    "fixture/StartJobRun.yaml"

requestCreatePartition :: CreatePartition -> TestTree
requestCreatePartition = req
    "CreatePartition"
    "fixture/CreatePartition.yaml"

requestStopCrawlerSchedule :: StopCrawlerSchedule -> TestTree
requestStopCrawlerSchedule = req
    "StopCrawlerSchedule"
    "fixture/StopCrawlerSchedule.yaml"

requestCreateDatabase :: CreateDatabase -> TestTree
requestCreateDatabase = req
    "CreateDatabase"
    "fixture/CreateDatabase.yaml"

requestGetTableVersions :: GetTableVersions -> TestTree
requestGetTableVersions = req
    "GetTableVersions"
    "fixture/GetTableVersions.yaml"

requestDeleteTrigger :: DeleteTrigger -> TestTree
requestDeleteTrigger = req
    "DeleteTrigger"
    "fixture/DeleteTrigger.yaml"

requestUpdateTrigger :: UpdateTrigger -> TestTree
requestUpdateTrigger = req
    "UpdateTrigger"
    "fixture/UpdateTrigger.yaml"

-- Responses

responseStartCrawler :: StartCrawlerResponse -> TestTree
responseStartCrawler = res
    "StartCrawlerResponse"
    "fixture/StartCrawlerResponse.proto"
    glue
    (Proxy :: Proxy StartCrawler)

responseGetCatalogImportStatus :: GetCatalogImportStatusResponse -> TestTree
responseGetCatalogImportStatus = res
    "GetCatalogImportStatusResponse"
    "fixture/GetCatalogImportStatusResponse.proto"
    glue
    (Proxy :: Proxy GetCatalogImportStatus)

responseGetPartition :: GetPartitionResponse -> TestTree
responseGetPartition = res
    "GetPartitionResponse"
    "fixture/GetPartitionResponse.proto"
    glue
    (Proxy :: Proxy GetPartition)

responseCreateTrigger :: CreateTriggerResponse -> TestTree
responseCreateTrigger = res
    "CreateTriggerResponse"
    "fixture/CreateTriggerResponse.proto"
    glue
    (Proxy :: Proxy CreateTrigger)

responseDeleteTable :: DeleteTableResponse -> TestTree
responseDeleteTable = res
    "DeleteTableResponse"
    "fixture/DeleteTableResponse.proto"
    glue
    (Proxy :: Proxy DeleteTable)

responseUpdateTable :: UpdateTableResponse -> TestTree
responseUpdateTable = res
    "UpdateTableResponse"
    "fixture/UpdateTableResponse.proto"
    glue
    (Proxy :: Proxy UpdateTable)

responseDeleteConnection :: DeleteConnectionResponse -> TestTree
responseDeleteConnection = res
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse.proto"
    glue
    (Proxy :: Proxy DeleteConnection)

responseUpdateConnection :: UpdateConnectionResponse -> TestTree
responseUpdateConnection = res
    "UpdateConnectionResponse"
    "fixture/UpdateConnectionResponse.proto"
    glue
    (Proxy :: Proxy UpdateConnection)

responseGetUserDefinedFunctions :: GetUserDefinedFunctionsResponse -> TestTree
responseGetUserDefinedFunctions = res
    "GetUserDefinedFunctionsResponse"
    "fixture/GetUserDefinedFunctionsResponse.proto"
    glue
    (Proxy :: Proxy GetUserDefinedFunctions)

responseBatchCreatePartition :: BatchCreatePartitionResponse -> TestTree
responseBatchCreatePartition = res
    "BatchCreatePartitionResponse"
    "fixture/BatchCreatePartitionResponse.proto"
    glue
    (Proxy :: Proxy BatchCreatePartition)

responseGetMapping :: GetMappingResponse -> TestTree
responseGetMapping = res
    "GetMappingResponse"
    "fixture/GetMappingResponse.proto"
    glue
    (Proxy :: Proxy GetMapping)

responseGetTableVersion :: GetTableVersionResponse -> TestTree
responseGetTableVersion = res
    "GetTableVersionResponse"
    "fixture/GetTableVersionResponse.proto"
    glue
    (Proxy :: Proxy GetTableVersion)

responseGetJobs :: GetJobsResponse -> TestTree
responseGetJobs = res
    "GetJobsResponse"
    "fixture/GetJobsResponse.proto"
    glue
    (Proxy :: Proxy GetJobs)

responseGetClassifiers :: GetClassifiersResponse -> TestTree
responseGetClassifiers = res
    "GetClassifiersResponse"
    "fixture/GetClassifiersResponse.proto"
    glue
    (Proxy :: Proxy GetClassifiers)

responseCreateConnection :: CreateConnectionResponse -> TestTree
responseCreateConnection = res
    "CreateConnectionResponse"
    "fixture/CreateConnectionResponse.proto"
    glue
    (Proxy :: Proxy CreateConnection)

responseDeleteTableVersion :: DeleteTableVersionResponse -> TestTree
responseDeleteTableVersion = res
    "DeleteTableVersionResponse"
    "fixture/DeleteTableVersionResponse.proto"
    glue
    (Proxy :: Proxy DeleteTableVersion)

responseDeleteDevEndpoint :: DeleteDevEndpointResponse -> TestTree
responseDeleteDevEndpoint = res
    "DeleteDevEndpointResponse"
    "fixture/DeleteDevEndpointResponse.proto"
    glue
    (Proxy :: Proxy DeleteDevEndpoint)

responseUpdateDevEndpoint :: UpdateDevEndpointResponse -> TestTree
responseUpdateDevEndpoint = res
    "UpdateDevEndpointResponse"
    "fixture/UpdateDevEndpointResponse.proto"
    glue
    (Proxy :: Proxy UpdateDevEndpoint)

responseDeleteCrawler :: DeleteCrawlerResponse -> TestTree
responseDeleteCrawler = res
    "DeleteCrawlerResponse"
    "fixture/DeleteCrawlerResponse.proto"
    glue
    (Proxy :: Proxy DeleteCrawler)

responseUpdateCrawler :: UpdateCrawlerResponse -> TestTree
responseUpdateCrawler = res
    "UpdateCrawlerResponse"
    "fixture/UpdateCrawlerResponse.proto"
    glue
    (Proxy :: Proxy UpdateCrawler)

responseGetConnection :: GetConnectionResponse -> TestTree
responseGetConnection = res
    "GetConnectionResponse"
    "fixture/GetConnectionResponse.proto"
    glue
    (Proxy :: Proxy GetConnection)

responseBatchGetPartition :: BatchGetPartitionResponse -> TestTree
responseBatchGetPartition = res
    "BatchGetPartitionResponse"
    "fixture/BatchGetPartitionResponse.proto"
    glue
    (Proxy :: Proxy BatchGetPartition)

responseStopTrigger :: StopTriggerResponse -> TestTree
responseStopTrigger = res
    "StopTriggerResponse"
    "fixture/StopTriggerResponse.proto"
    glue
    (Proxy :: Proxy StopTrigger)

responseUpdateCrawlerSchedule :: UpdateCrawlerScheduleResponse -> TestTree
responseUpdateCrawlerSchedule = res
    "UpdateCrawlerScheduleResponse"
    "fixture/UpdateCrawlerScheduleResponse.proto"
    glue
    (Proxy :: Proxy UpdateCrawlerSchedule)

responseDeleteUserDefinedFunction :: DeleteUserDefinedFunctionResponse -> TestTree
responseDeleteUserDefinedFunction = res
    "DeleteUserDefinedFunctionResponse"
    "fixture/DeleteUserDefinedFunctionResponse.proto"
    glue
    (Proxy :: Proxy DeleteUserDefinedFunction)

responseUpdateUserDefinedFunction :: UpdateUserDefinedFunctionResponse -> TestTree
responseUpdateUserDefinedFunction = res
    "UpdateUserDefinedFunctionResponse"
    "fixture/UpdateUserDefinedFunctionResponse.proto"
    glue
    (Proxy :: Proxy UpdateUserDefinedFunction)

responseBatchDeleteTable :: BatchDeleteTableResponse -> TestTree
responseBatchDeleteTable = res
    "BatchDeleteTableResponse"
    "fixture/BatchDeleteTableResponse.proto"
    glue
    (Proxy :: Proxy BatchDeleteTable)

responseGetTables :: GetTablesResponse -> TestTree
responseGetTables = res
    "GetTablesResponse"
    "fixture/GetTablesResponse.proto"
    glue
    (Proxy :: Proxy GetTables)

responseCreateClassifier :: CreateClassifierResponse -> TestTree
responseCreateClassifier = res
    "CreateClassifierResponse"
    "fixture/CreateClassifierResponse.proto"
    glue
    (Proxy :: Proxy CreateClassifier)

responseBatchDeleteConnection :: BatchDeleteConnectionResponse -> TestTree
responseBatchDeleteConnection = res
    "BatchDeleteConnectionResponse"
    "fixture/BatchDeleteConnectionResponse.proto"
    glue
    (Proxy :: Proxy BatchDeleteConnection)

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob = res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    glue
    (Proxy :: Proxy CreateJob)

responseGetJobRuns :: GetJobRunsResponse -> TestTree
responseGetJobRuns = res
    "GetJobRunsResponse"
    "fixture/GetJobRunsResponse.proto"
    glue
    (Proxy :: Proxy GetJobRuns)

responseCreateUserDefinedFunction :: CreateUserDefinedFunctionResponse -> TestTree
responseCreateUserDefinedFunction = res
    "CreateUserDefinedFunctionResponse"
    "fixture/CreateUserDefinedFunctionResponse.proto"
    glue
    (Proxy :: Proxy CreateUserDefinedFunction)

responseResetJobBookmark :: ResetJobBookmarkResponse -> TestTree
responseResetJobBookmark = res
    "ResetJobBookmarkResponse"
    "fixture/ResetJobBookmarkResponse.proto"
    glue
    (Proxy :: Proxy ResetJobBookmark)

responseDeleteJob :: DeleteJobResponse -> TestTree
responseDeleteJob = res
    "DeleteJobResponse"
    "fixture/DeleteJobResponse.proto"
    glue
    (Proxy :: Proxy DeleteJob)

responseUpdateJob :: UpdateJobResponse -> TestTree
responseUpdateJob = res
    "UpdateJobResponse"
    "fixture/UpdateJobResponse.proto"
    glue
    (Proxy :: Proxy UpdateJob)

responseGetCrawlers :: GetCrawlersResponse -> TestTree
responseGetCrawlers = res
    "GetCrawlersResponse"
    "fixture/GetCrawlersResponse.proto"
    glue
    (Proxy :: Proxy GetCrawlers)

responseGetClassifier :: GetClassifierResponse -> TestTree
responseGetClassifier = res
    "GetClassifierResponse"
    "fixture/GetClassifierResponse.proto"
    glue
    (Proxy :: Proxy GetClassifier)

responseGetJob :: GetJobResponse -> TestTree
responseGetJob = res
    "GetJobResponse"
    "fixture/GetJobResponse.proto"
    glue
    (Proxy :: Proxy GetJob)

responseBatchDeleteTableVersion :: BatchDeleteTableVersionResponse -> TestTree
responseBatchDeleteTableVersion = res
    "BatchDeleteTableVersionResponse"
    "fixture/BatchDeleteTableVersionResponse.proto"
    glue
    (Proxy :: Proxy BatchDeleteTableVersion)

responseGetDevEndpoints :: GetDevEndpointsResponse -> TestTree
responseGetDevEndpoints = res
    "GetDevEndpointsResponse"
    "fixture/GetDevEndpointsResponse.proto"
    glue
    (Proxy :: Proxy GetDevEndpoints)

responseStartCrawlerSchedule :: StartCrawlerScheduleResponse -> TestTree
responseStartCrawlerSchedule = res
    "StartCrawlerScheduleResponse"
    "fixture/StartCrawlerScheduleResponse.proto"
    glue
    (Proxy :: Proxy StartCrawlerSchedule)

responseGetUserDefinedFunction :: GetUserDefinedFunctionResponse -> TestTree
responseGetUserDefinedFunction = res
    "GetUserDefinedFunctionResponse"
    "fixture/GetUserDefinedFunctionResponse.proto"
    glue
    (Proxy :: Proxy GetUserDefinedFunction)

responseDeleteDatabase :: DeleteDatabaseResponse -> TestTree
responseDeleteDatabase = res
    "DeleteDatabaseResponse"
    "fixture/DeleteDatabaseResponse.proto"
    glue
    (Proxy :: Proxy DeleteDatabase)

responseUpdateDatabase :: UpdateDatabaseResponse -> TestTree
responseUpdateDatabase = res
    "UpdateDatabaseResponse"
    "fixture/UpdateDatabaseResponse.proto"
    glue
    (Proxy :: Proxy UpdateDatabase)

responseStopCrawler :: StopCrawlerResponse -> TestTree
responseStopCrawler = res
    "StopCrawlerResponse"
    "fixture/StopCrawlerResponse.proto"
    glue
    (Proxy :: Proxy StopCrawler)

responseGetPartitions :: GetPartitionsResponse -> TestTree
responseGetPartitions = res
    "GetPartitionsResponse"
    "fixture/GetPartitionsResponse.proto"
    glue
    (Proxy :: Proxy GetPartitions)

responseBatchDeletePartition :: BatchDeletePartitionResponse -> TestTree
responseBatchDeletePartition = res
    "BatchDeletePartitionResponse"
    "fixture/BatchDeletePartitionResponse.proto"
    glue
    (Proxy :: Proxy BatchDeletePartition)

responseGetCrawler :: GetCrawlerResponse -> TestTree
responseGetCrawler = res
    "GetCrawlerResponse"
    "fixture/GetCrawlerResponse.proto"
    glue
    (Proxy :: Proxy GetCrawler)

responseBatchStopJobRun :: BatchStopJobRunResponse -> TestTree
responseBatchStopJobRun = res
    "BatchStopJobRunResponse"
    "fixture/BatchStopJobRunResponse.proto"
    glue
    (Proxy :: Proxy BatchStopJobRun)

responseGetDevEndpoint :: GetDevEndpointResponse -> TestTree
responseGetDevEndpoint = res
    "GetDevEndpointResponse"
    "fixture/GetDevEndpointResponse.proto"
    glue
    (Proxy :: Proxy GetDevEndpoint)

responseCreateTable :: CreateTableResponse -> TestTree
responseCreateTable = res
    "CreateTableResponse"
    "fixture/CreateTableResponse.proto"
    glue
    (Proxy :: Proxy CreateTable)

responseGetCrawlerMetrics :: GetCrawlerMetricsResponse -> TestTree
responseGetCrawlerMetrics = res
    "GetCrawlerMetricsResponse"
    "fixture/GetCrawlerMetricsResponse.proto"
    glue
    (Proxy :: Proxy GetCrawlerMetrics)

responseGetPlan :: GetPlanResponse -> TestTree
responseGetPlan = res
    "GetPlanResponse"
    "fixture/GetPlanResponse.proto"
    glue
    (Proxy :: Proxy GetPlan)

responseGetTriggers :: GetTriggersResponse -> TestTree
responseGetTriggers = res
    "GetTriggersResponse"
    "fixture/GetTriggersResponse.proto"
    glue
    (Proxy :: Proxy GetTriggers)

responseStartTrigger :: StartTriggerResponse -> TestTree
responseStartTrigger = res
    "StartTriggerResponse"
    "fixture/StartTriggerResponse.proto"
    glue
    (Proxy :: Proxy StartTrigger)

responseGetDataflowGraph :: GetDataflowGraphResponse -> TestTree
responseGetDataflowGraph = res
    "GetDataflowGraphResponse"
    "fixture/GetDataflowGraphResponse.proto"
    glue
    (Proxy :: Proxy GetDataflowGraph)

responseGetDatabases :: GetDatabasesResponse -> TestTree
responseGetDatabases = res
    "GetDatabasesResponse"
    "fixture/GetDatabasesResponse.proto"
    glue
    (Proxy :: Proxy GetDatabases)

responseGetTable :: GetTableResponse -> TestTree
responseGetTable = res
    "GetTableResponse"
    "fixture/GetTableResponse.proto"
    glue
    (Proxy :: Proxy GetTable)

responseCreateCrawler :: CreateCrawlerResponse -> TestTree
responseCreateCrawler = res
    "CreateCrawlerResponse"
    "fixture/CreateCrawlerResponse.proto"
    glue
    (Proxy :: Proxy CreateCrawler)

responseGetJobRun :: GetJobRunResponse -> TestTree
responseGetJobRun = res
    "GetJobRunResponse"
    "fixture/GetJobRunResponse.proto"
    glue
    (Proxy :: Proxy GetJobRun)

responseCreateDevEndpoint :: CreateDevEndpointResponse -> TestTree
responseCreateDevEndpoint = res
    "CreateDevEndpointResponse"
    "fixture/CreateDevEndpointResponse.proto"
    glue
    (Proxy :: Proxy CreateDevEndpoint)

responseGetDatabase :: GetDatabaseResponse -> TestTree
responseGetDatabase = res
    "GetDatabaseResponse"
    "fixture/GetDatabaseResponse.proto"
    glue
    (Proxy :: Proxy GetDatabase)

responseDeletePartition :: DeletePartitionResponse -> TestTree
responseDeletePartition = res
    "DeletePartitionResponse"
    "fixture/DeletePartitionResponse.proto"
    glue
    (Proxy :: Proxy DeletePartition)

responseUpdatePartition :: UpdatePartitionResponse -> TestTree
responseUpdatePartition = res
    "UpdatePartitionResponse"
    "fixture/UpdatePartitionResponse.proto"
    glue
    (Proxy :: Proxy UpdatePartition)

responseCreateScript :: CreateScriptResponse -> TestTree
responseCreateScript = res
    "CreateScriptResponse"
    "fixture/CreateScriptResponse.proto"
    glue
    (Proxy :: Proxy CreateScript)

responseGetConnections :: GetConnectionsResponse -> TestTree
responseGetConnections = res
    "GetConnectionsResponse"
    "fixture/GetConnectionsResponse.proto"
    glue
    (Proxy :: Proxy GetConnections)

responseGetTrigger :: GetTriggerResponse -> TestTree
responseGetTrigger = res
    "GetTriggerResponse"
    "fixture/GetTriggerResponse.proto"
    glue
    (Proxy :: Proxy GetTrigger)

responseImportCatalogToGlue :: ImportCatalogToGlueResponse -> TestTree
responseImportCatalogToGlue = res
    "ImportCatalogToGlueResponse"
    "fixture/ImportCatalogToGlueResponse.proto"
    glue
    (Proxy :: Proxy ImportCatalogToGlue)

responseDeleteClassifier :: DeleteClassifierResponse -> TestTree
responseDeleteClassifier = res
    "DeleteClassifierResponse"
    "fixture/DeleteClassifierResponse.proto"
    glue
    (Proxy :: Proxy DeleteClassifier)

responseUpdateClassifier :: UpdateClassifierResponse -> TestTree
responseUpdateClassifier = res
    "UpdateClassifierResponse"
    "fixture/UpdateClassifierResponse.proto"
    glue
    (Proxy :: Proxy UpdateClassifier)

responseStartJobRun :: StartJobRunResponse -> TestTree
responseStartJobRun = res
    "StartJobRunResponse"
    "fixture/StartJobRunResponse.proto"
    glue
    (Proxy :: Proxy StartJobRun)

responseCreatePartition :: CreatePartitionResponse -> TestTree
responseCreatePartition = res
    "CreatePartitionResponse"
    "fixture/CreatePartitionResponse.proto"
    glue
    (Proxy :: Proxy CreatePartition)

responseStopCrawlerSchedule :: StopCrawlerScheduleResponse -> TestTree
responseStopCrawlerSchedule = res
    "StopCrawlerScheduleResponse"
    "fixture/StopCrawlerScheduleResponse.proto"
    glue
    (Proxy :: Proxy StopCrawlerSchedule)

responseCreateDatabase :: CreateDatabaseResponse -> TestTree
responseCreateDatabase = res
    "CreateDatabaseResponse"
    "fixture/CreateDatabaseResponse.proto"
    glue
    (Proxy :: Proxy CreateDatabase)

responseGetTableVersions :: GetTableVersionsResponse -> TestTree
responseGetTableVersions = res
    "GetTableVersionsResponse"
    "fixture/GetTableVersionsResponse.proto"
    glue
    (Proxy :: Proxy GetTableVersions)

responseDeleteTrigger :: DeleteTriggerResponse -> TestTree
responseDeleteTrigger = res
    "DeleteTriggerResponse"
    "fixture/DeleteTriggerResponse.proto"
    glue
    (Proxy :: Proxy DeleteTrigger)

responseUpdateTrigger :: UpdateTriggerResponse -> TestTree
responseUpdateTrigger = res
    "UpdateTriggerResponse"
    "fixture/UpdateTriggerResponse.proto"
    glue
    (Proxy :: Proxy UpdateTrigger)
