{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.DynamoDB
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.DynamoDB where

import Amazonka.DynamoDB
import qualified Data.Proxy as Proxy
import Test.Amazonka.DynamoDB.Internal
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
--         [ requestBatchExecuteStatement $
--             newBatchExecuteStatement
--
--         , requestBatchGetItem $
--             newBatchGetItem
--
--         , requestBatchWriteItem $
--             newBatchWriteItem
--
--         , requestCreateBackup $
--             newCreateBackup
--
--         , requestCreateGlobalTable $
--             newCreateGlobalTable
--
--         , requestCreateTable $
--             newCreateTable
--
--         , requestDeleteBackup $
--             newDeleteBackup
--
--         , requestDeleteItem $
--             newDeleteItem
--
--         , requestDeleteTable $
--             newDeleteTable
--
--         , requestDescribeBackup $
--             newDescribeBackup
--
--         , requestDescribeContinuousBackups $
--             newDescribeContinuousBackups
--
--         , requestDescribeContributorInsights $
--             newDescribeContributorInsights
--
--         , requestDescribeEndpoints $
--             newDescribeEndpoints
--
--         , requestDescribeExport $
--             newDescribeExport
--
--         , requestDescribeGlobalTable $
--             newDescribeGlobalTable
--
--         , requestDescribeGlobalTableSettings $
--             newDescribeGlobalTableSettings
--
--         , requestDescribeImport $
--             newDescribeImport
--
--         , requestDescribeKinesisStreamingDestination $
--             newDescribeKinesisStreamingDestination
--
--         , requestDescribeLimits $
--             newDescribeLimits
--
--         , requestDescribeTable $
--             newDescribeTable
--
--         , requestDescribeTableReplicaAutoScaling $
--             newDescribeTableReplicaAutoScaling
--
--         , requestDescribeTimeToLive $
--             newDescribeTimeToLive
--
--         , requestDisableKinesisStreamingDestination $
--             newDisableKinesisStreamingDestination
--
--         , requestEnableKinesisStreamingDestination $
--             newEnableKinesisStreamingDestination
--
--         , requestExecuteStatement $
--             newExecuteStatement
--
--         , requestExecuteTransaction $
--             newExecuteTransaction
--
--         , requestExportTableToPointInTime $
--             newExportTableToPointInTime
--
--         , requestGetItem $
--             newGetItem
--
--         , requestImportTable $
--             newImportTable
--
--         , requestListBackups $
--             newListBackups
--
--         , requestListContributorInsights $
--             newListContributorInsights
--
--         , requestListExports $
--             newListExports
--
--         , requestListGlobalTables $
--             newListGlobalTables
--
--         , requestListImports $
--             newListImports
--
--         , requestListTables $
--             newListTables
--
--         , requestListTagsOfResource $
--             newListTagsOfResource
--
--         , requestPutItem $
--             newPutItem
--
--         , requestQuery $
--             newQuery
--
--         , requestRestoreTableFromBackup $
--             newRestoreTableFromBackup
--
--         , requestRestoreTableToPointInTime $
--             newRestoreTableToPointInTime
--
--         , requestScan $
--             newScan
--
--         , requestTagResource $
--             newTagResource
--
--         , requestTransactGetItems $
--             newTransactGetItems
--
--         , requestTransactWriteItems $
--             newTransactWriteItems
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateContinuousBackups $
--             newUpdateContinuousBackups
--
--         , requestUpdateContributorInsights $
--             newUpdateContributorInsights
--
--         , requestUpdateGlobalTable $
--             newUpdateGlobalTable
--
--         , requestUpdateGlobalTableSettings $
--             newUpdateGlobalTableSettings
--
--         , requestUpdateItem $
--             newUpdateItem
--
--         , requestUpdateTable $
--             newUpdateTable
--
--         , requestUpdateTableReplicaAutoScaling $
--             newUpdateTableReplicaAutoScaling
--
--         , requestUpdateTimeToLive $
--             newUpdateTimeToLive
--
--           ]

--     , testGroup "response"
--         [ responseBatchExecuteStatement $
--             newBatchExecuteStatementResponse
--
--         , responseBatchGetItem $
--             newBatchGetItemResponse
--
--         , responseBatchWriteItem $
--             newBatchWriteItemResponse
--
--         , responseCreateBackup $
--             newCreateBackupResponse
--
--         , responseCreateGlobalTable $
--             newCreateGlobalTableResponse
--
--         , responseCreateTable $
--             newCreateTableResponse
--
--         , responseDeleteBackup $
--             newDeleteBackupResponse
--
--         , responseDeleteItem $
--             newDeleteItemResponse
--
--         , responseDeleteTable $
--             newDeleteTableResponse
--
--         , responseDescribeBackup $
--             newDescribeBackupResponse
--
--         , responseDescribeContinuousBackups $
--             newDescribeContinuousBackupsResponse
--
--         , responseDescribeContributorInsights $
--             newDescribeContributorInsightsResponse
--
--         , responseDescribeEndpoints $
--             newDescribeEndpointsResponse
--
--         , responseDescribeExport $
--             newDescribeExportResponse
--
--         , responseDescribeGlobalTable $
--             newDescribeGlobalTableResponse
--
--         , responseDescribeGlobalTableSettings $
--             newDescribeGlobalTableSettingsResponse
--
--         , responseDescribeImport $
--             newDescribeImportResponse
--
--         , responseDescribeKinesisStreamingDestination $
--             newDescribeKinesisStreamingDestinationResponse
--
--         , responseDescribeLimits $
--             newDescribeLimitsResponse
--
--         , responseDescribeTable $
--             newDescribeTableResponse
--
--         , responseDescribeTableReplicaAutoScaling $
--             newDescribeTableReplicaAutoScalingResponse
--
--         , responseDescribeTimeToLive $
--             newDescribeTimeToLiveResponse
--
--         , responseDisableKinesisStreamingDestination $
--             newKinesisStreamingDestinationOutput
--
--         , responseEnableKinesisStreamingDestination $
--             newKinesisStreamingDestinationOutput
--
--         , responseExecuteStatement $
--             newExecuteStatementResponse
--
--         , responseExecuteTransaction $
--             newExecuteTransactionResponse
--
--         , responseExportTableToPointInTime $
--             newExportTableToPointInTimeResponse
--
--         , responseGetItem $
--             newGetItemResponse
--
--         , responseImportTable $
--             newImportTableResponse
--
--         , responseListBackups $
--             newListBackupsResponse
--
--         , responseListContributorInsights $
--             newListContributorInsightsResponse
--
--         , responseListExports $
--             newListExportsResponse
--
--         , responseListGlobalTables $
--             newListGlobalTablesResponse
--
--         , responseListImports $
--             newListImportsResponse
--
--         , responseListTables $
--             newListTablesResponse
--
--         , responseListTagsOfResource $
--             newListTagsOfResourceResponse
--
--         , responsePutItem $
--             newPutItemResponse
--
--         , responseQuery $
--             newQueryResponse
--
--         , responseRestoreTableFromBackup $
--             newRestoreTableFromBackupResponse
--
--         , responseRestoreTableToPointInTime $
--             newRestoreTableToPointInTimeResponse
--
--         , responseScan $
--             newScanResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseTransactGetItems $
--             newTransactGetItemsResponse
--
--         , responseTransactWriteItems $
--             newTransactWriteItemsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateContinuousBackups $
--             newUpdateContinuousBackupsResponse
--
--         , responseUpdateContributorInsights $
--             newUpdateContributorInsightsResponse
--
--         , responseUpdateGlobalTable $
--             newUpdateGlobalTableResponse
--
--         , responseUpdateGlobalTableSettings $
--             newUpdateGlobalTableSettingsResponse
--
--         , responseUpdateItem $
--             newUpdateItemResponse
--
--         , responseUpdateTable $
--             newUpdateTableResponse
--
--         , responseUpdateTableReplicaAutoScaling $
--             newUpdateTableReplicaAutoScalingResponse
--
--         , responseUpdateTimeToLive $
--             newUpdateTimeToLiveResponse
--
--           ]
--     ]

-- Requests

requestBatchExecuteStatement :: BatchExecuteStatement -> TestTree
requestBatchExecuteStatement =
  req
    "BatchExecuteStatement"
    "fixture/BatchExecuteStatement.yaml"

requestBatchGetItem :: BatchGetItem -> TestTree
requestBatchGetItem =
  req
    "BatchGetItem"
    "fixture/BatchGetItem.yaml"

requestBatchWriteItem :: BatchWriteItem -> TestTree
requestBatchWriteItem =
  req
    "BatchWriteItem"
    "fixture/BatchWriteItem.yaml"

requestCreateBackup :: CreateBackup -> TestTree
requestCreateBackup =
  req
    "CreateBackup"
    "fixture/CreateBackup.yaml"

requestCreateGlobalTable :: CreateGlobalTable -> TestTree
requestCreateGlobalTable =
  req
    "CreateGlobalTable"
    "fixture/CreateGlobalTable.yaml"

requestCreateTable :: CreateTable -> TestTree
requestCreateTable =
  req
    "CreateTable"
    "fixture/CreateTable.yaml"

requestDeleteBackup :: DeleteBackup -> TestTree
requestDeleteBackup =
  req
    "DeleteBackup"
    "fixture/DeleteBackup.yaml"

requestDeleteItem :: DeleteItem -> TestTree
requestDeleteItem =
  req
    "DeleteItem"
    "fixture/DeleteItem.yaml"

requestDeleteTable :: DeleteTable -> TestTree
requestDeleteTable =
  req
    "DeleteTable"
    "fixture/DeleteTable.yaml"

requestDescribeBackup :: DescribeBackup -> TestTree
requestDescribeBackup =
  req
    "DescribeBackup"
    "fixture/DescribeBackup.yaml"

requestDescribeContinuousBackups :: DescribeContinuousBackups -> TestTree
requestDescribeContinuousBackups =
  req
    "DescribeContinuousBackups"
    "fixture/DescribeContinuousBackups.yaml"

requestDescribeContributorInsights :: DescribeContributorInsights -> TestTree
requestDescribeContributorInsights =
  req
    "DescribeContributorInsights"
    "fixture/DescribeContributorInsights.yaml"

requestDescribeEndpoints :: DescribeEndpoints -> TestTree
requestDescribeEndpoints =
  req
    "DescribeEndpoints"
    "fixture/DescribeEndpoints.yaml"

requestDescribeExport :: DescribeExport -> TestTree
requestDescribeExport =
  req
    "DescribeExport"
    "fixture/DescribeExport.yaml"

requestDescribeGlobalTable :: DescribeGlobalTable -> TestTree
requestDescribeGlobalTable =
  req
    "DescribeGlobalTable"
    "fixture/DescribeGlobalTable.yaml"

requestDescribeGlobalTableSettings :: DescribeGlobalTableSettings -> TestTree
requestDescribeGlobalTableSettings =
  req
    "DescribeGlobalTableSettings"
    "fixture/DescribeGlobalTableSettings.yaml"

requestDescribeImport :: DescribeImport -> TestTree
requestDescribeImport =
  req
    "DescribeImport"
    "fixture/DescribeImport.yaml"

requestDescribeKinesisStreamingDestination :: DescribeKinesisStreamingDestination -> TestTree
requestDescribeKinesisStreamingDestination =
  req
    "DescribeKinesisStreamingDestination"
    "fixture/DescribeKinesisStreamingDestination.yaml"

requestDescribeLimits :: DescribeLimits -> TestTree
requestDescribeLimits =
  req
    "DescribeLimits"
    "fixture/DescribeLimits.yaml"

requestDescribeTable :: DescribeTable -> TestTree
requestDescribeTable =
  req
    "DescribeTable"
    "fixture/DescribeTable.yaml"

requestDescribeTableReplicaAutoScaling :: DescribeTableReplicaAutoScaling -> TestTree
requestDescribeTableReplicaAutoScaling =
  req
    "DescribeTableReplicaAutoScaling"
    "fixture/DescribeTableReplicaAutoScaling.yaml"

requestDescribeTimeToLive :: DescribeTimeToLive -> TestTree
requestDescribeTimeToLive =
  req
    "DescribeTimeToLive"
    "fixture/DescribeTimeToLive.yaml"

requestDisableKinesisStreamingDestination :: DisableKinesisStreamingDestination -> TestTree
requestDisableKinesisStreamingDestination =
  req
    "DisableKinesisStreamingDestination"
    "fixture/DisableKinesisStreamingDestination.yaml"

requestEnableKinesisStreamingDestination :: EnableKinesisStreamingDestination -> TestTree
requestEnableKinesisStreamingDestination =
  req
    "EnableKinesisStreamingDestination"
    "fixture/EnableKinesisStreamingDestination.yaml"

requestExecuteStatement :: ExecuteStatement -> TestTree
requestExecuteStatement =
  req
    "ExecuteStatement"
    "fixture/ExecuteStatement.yaml"

requestExecuteTransaction :: ExecuteTransaction -> TestTree
requestExecuteTransaction =
  req
    "ExecuteTransaction"
    "fixture/ExecuteTransaction.yaml"

requestExportTableToPointInTime :: ExportTableToPointInTime -> TestTree
requestExportTableToPointInTime =
  req
    "ExportTableToPointInTime"
    "fixture/ExportTableToPointInTime.yaml"

requestGetItem :: GetItem -> TestTree
requestGetItem =
  req
    "GetItem"
    "fixture/GetItem.yaml"

requestImportTable :: ImportTable -> TestTree
requestImportTable =
  req
    "ImportTable"
    "fixture/ImportTable.yaml"

requestListBackups :: ListBackups -> TestTree
requestListBackups =
  req
    "ListBackups"
    "fixture/ListBackups.yaml"

requestListContributorInsights :: ListContributorInsights -> TestTree
requestListContributorInsights =
  req
    "ListContributorInsights"
    "fixture/ListContributorInsights.yaml"

requestListExports :: ListExports -> TestTree
requestListExports =
  req
    "ListExports"
    "fixture/ListExports.yaml"

requestListGlobalTables :: ListGlobalTables -> TestTree
requestListGlobalTables =
  req
    "ListGlobalTables"
    "fixture/ListGlobalTables.yaml"

requestListImports :: ListImports -> TestTree
requestListImports =
  req
    "ListImports"
    "fixture/ListImports.yaml"

requestListTables :: ListTables -> TestTree
requestListTables =
  req
    "ListTables"
    "fixture/ListTables.yaml"

requestListTagsOfResource :: ListTagsOfResource -> TestTree
requestListTagsOfResource =
  req
    "ListTagsOfResource"
    "fixture/ListTagsOfResource.yaml"

requestPutItem :: PutItem -> TestTree
requestPutItem =
  req
    "PutItem"
    "fixture/PutItem.yaml"

requestQuery :: Query -> TestTree
requestQuery =
  req
    "Query"
    "fixture/Query.yaml"

requestRestoreTableFromBackup :: RestoreTableFromBackup -> TestTree
requestRestoreTableFromBackup =
  req
    "RestoreTableFromBackup"
    "fixture/RestoreTableFromBackup.yaml"

requestRestoreTableToPointInTime :: RestoreTableToPointInTime -> TestTree
requestRestoreTableToPointInTime =
  req
    "RestoreTableToPointInTime"
    "fixture/RestoreTableToPointInTime.yaml"

requestScan :: Scan -> TestTree
requestScan =
  req
    "Scan"
    "fixture/Scan.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestTransactGetItems :: TransactGetItems -> TestTree
requestTransactGetItems =
  req
    "TransactGetItems"
    "fixture/TransactGetItems.yaml"

requestTransactWriteItems :: TransactWriteItems -> TestTree
requestTransactWriteItems =
  req
    "TransactWriteItems"
    "fixture/TransactWriteItems.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateContinuousBackups :: UpdateContinuousBackups -> TestTree
requestUpdateContinuousBackups =
  req
    "UpdateContinuousBackups"
    "fixture/UpdateContinuousBackups.yaml"

requestUpdateContributorInsights :: UpdateContributorInsights -> TestTree
requestUpdateContributorInsights =
  req
    "UpdateContributorInsights"
    "fixture/UpdateContributorInsights.yaml"

requestUpdateGlobalTable :: UpdateGlobalTable -> TestTree
requestUpdateGlobalTable =
  req
    "UpdateGlobalTable"
    "fixture/UpdateGlobalTable.yaml"

requestUpdateGlobalTableSettings :: UpdateGlobalTableSettings -> TestTree
requestUpdateGlobalTableSettings =
  req
    "UpdateGlobalTableSettings"
    "fixture/UpdateGlobalTableSettings.yaml"

requestUpdateItem :: UpdateItem -> TestTree
requestUpdateItem =
  req
    "UpdateItem"
    "fixture/UpdateItem.yaml"

requestUpdateTable :: UpdateTable -> TestTree
requestUpdateTable =
  req
    "UpdateTable"
    "fixture/UpdateTable.yaml"

requestUpdateTableReplicaAutoScaling :: UpdateTableReplicaAutoScaling -> TestTree
requestUpdateTableReplicaAutoScaling =
  req
    "UpdateTableReplicaAutoScaling"
    "fixture/UpdateTableReplicaAutoScaling.yaml"

requestUpdateTimeToLive :: UpdateTimeToLive -> TestTree
requestUpdateTimeToLive =
  req
    "UpdateTimeToLive"
    "fixture/UpdateTimeToLive.yaml"

-- Responses

responseBatchExecuteStatement :: BatchExecuteStatementResponse -> TestTree
responseBatchExecuteStatement =
  res
    "BatchExecuteStatementResponse"
    "fixture/BatchExecuteStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchExecuteStatement)

responseBatchGetItem :: BatchGetItemResponse -> TestTree
responseBatchGetItem =
  res
    "BatchGetItemResponse"
    "fixture/BatchGetItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetItem)

responseBatchWriteItem :: BatchWriteItemResponse -> TestTree
responseBatchWriteItem =
  res
    "BatchWriteItemResponse"
    "fixture/BatchWriteItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchWriteItem)

responseCreateBackup :: CreateBackupResponse -> TestTree
responseCreateBackup =
  res
    "CreateBackupResponse"
    "fixture/CreateBackupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBackup)

responseCreateGlobalTable :: CreateGlobalTableResponse -> TestTree
responseCreateGlobalTable =
  res
    "CreateGlobalTableResponse"
    "fixture/CreateGlobalTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGlobalTable)

responseCreateTable :: CreateTableResponse -> TestTree
responseCreateTable =
  res
    "CreateTableResponse"
    "fixture/CreateTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTable)

responseDeleteBackup :: DeleteBackupResponse -> TestTree
responseDeleteBackup =
  res
    "DeleteBackupResponse"
    "fixture/DeleteBackupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBackup)

responseDeleteItem :: DeleteItemResponse -> TestTree
responseDeleteItem =
  res
    "DeleteItemResponse"
    "fixture/DeleteItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteItem)

responseDeleteTable :: DeleteTableResponse -> TestTree
responseDeleteTable =
  res
    "DeleteTableResponse"
    "fixture/DeleteTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTable)

responseDescribeBackup :: DescribeBackupResponse -> TestTree
responseDescribeBackup =
  res
    "DescribeBackupResponse"
    "fixture/DescribeBackupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBackup)

responseDescribeContinuousBackups :: DescribeContinuousBackupsResponse -> TestTree
responseDescribeContinuousBackups =
  res
    "DescribeContinuousBackupsResponse"
    "fixture/DescribeContinuousBackupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeContinuousBackups)

responseDescribeContributorInsights :: DescribeContributorInsightsResponse -> TestTree
responseDescribeContributorInsights =
  res
    "DescribeContributorInsightsResponse"
    "fixture/DescribeContributorInsightsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeContributorInsights)

responseDescribeEndpoints :: DescribeEndpointsResponse -> TestTree
responseDescribeEndpoints =
  res
    "DescribeEndpointsResponse"
    "fixture/DescribeEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEndpoints)

responseDescribeExport :: DescribeExportResponse -> TestTree
responseDescribeExport =
  res
    "DescribeExportResponse"
    "fixture/DescribeExportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeExport)

responseDescribeGlobalTable :: DescribeGlobalTableResponse -> TestTree
responseDescribeGlobalTable =
  res
    "DescribeGlobalTableResponse"
    "fixture/DescribeGlobalTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGlobalTable)

responseDescribeGlobalTableSettings :: DescribeGlobalTableSettingsResponse -> TestTree
responseDescribeGlobalTableSettings =
  res
    "DescribeGlobalTableSettingsResponse"
    "fixture/DescribeGlobalTableSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGlobalTableSettings)

responseDescribeImport :: DescribeImportResponse -> TestTree
responseDescribeImport =
  res
    "DescribeImportResponse"
    "fixture/DescribeImportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImport)

responseDescribeKinesisStreamingDestination :: DescribeKinesisStreamingDestinationResponse -> TestTree
responseDescribeKinesisStreamingDestination =
  res
    "DescribeKinesisStreamingDestinationResponse"
    "fixture/DescribeKinesisStreamingDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeKinesisStreamingDestination)

responseDescribeLimits :: DescribeLimitsResponse -> TestTree
responseDescribeLimits =
  res
    "DescribeLimitsResponse"
    "fixture/DescribeLimitsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLimits)

responseDescribeTable :: DescribeTableResponse -> TestTree
responseDescribeTable =
  res
    "DescribeTableResponse"
    "fixture/DescribeTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTable)

responseDescribeTableReplicaAutoScaling :: DescribeTableReplicaAutoScalingResponse -> TestTree
responseDescribeTableReplicaAutoScaling =
  res
    "DescribeTableReplicaAutoScalingResponse"
    "fixture/DescribeTableReplicaAutoScalingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTableReplicaAutoScaling)

responseDescribeTimeToLive :: DescribeTimeToLiveResponse -> TestTree
responseDescribeTimeToLive =
  res
    "DescribeTimeToLiveResponse"
    "fixture/DescribeTimeToLiveResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTimeToLive)

responseDisableKinesisStreamingDestination :: KinesisStreamingDestinationOutput -> TestTree
responseDisableKinesisStreamingDestination =
  res
    "DisableKinesisStreamingDestinationResponse"
    "fixture/DisableKinesisStreamingDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableKinesisStreamingDestination)

responseEnableKinesisStreamingDestination :: KinesisStreamingDestinationOutput -> TestTree
responseEnableKinesisStreamingDestination =
  res
    "EnableKinesisStreamingDestinationResponse"
    "fixture/EnableKinesisStreamingDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableKinesisStreamingDestination)

responseExecuteStatement :: ExecuteStatementResponse -> TestTree
responseExecuteStatement =
  res
    "ExecuteStatementResponse"
    "fixture/ExecuteStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExecuteStatement)

responseExecuteTransaction :: ExecuteTransactionResponse -> TestTree
responseExecuteTransaction =
  res
    "ExecuteTransactionResponse"
    "fixture/ExecuteTransactionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExecuteTransaction)

responseExportTableToPointInTime :: ExportTableToPointInTimeResponse -> TestTree
responseExportTableToPointInTime =
  res
    "ExportTableToPointInTimeResponse"
    "fixture/ExportTableToPointInTimeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportTableToPointInTime)

responseGetItem :: GetItemResponse -> TestTree
responseGetItem =
  res
    "GetItemResponse"
    "fixture/GetItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetItem)

responseImportTable :: ImportTableResponse -> TestTree
responseImportTable =
  res
    "ImportTableResponse"
    "fixture/ImportTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportTable)

responseListBackups :: ListBackupsResponse -> TestTree
responseListBackups =
  res
    "ListBackupsResponse"
    "fixture/ListBackupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBackups)

responseListContributorInsights :: ListContributorInsightsResponse -> TestTree
responseListContributorInsights =
  res
    "ListContributorInsightsResponse"
    "fixture/ListContributorInsightsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListContributorInsights)

responseListExports :: ListExportsResponse -> TestTree
responseListExports =
  res
    "ListExportsResponse"
    "fixture/ListExportsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListExports)

responseListGlobalTables :: ListGlobalTablesResponse -> TestTree
responseListGlobalTables =
  res
    "ListGlobalTablesResponse"
    "fixture/ListGlobalTablesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGlobalTables)

responseListImports :: ListImportsResponse -> TestTree
responseListImports =
  res
    "ListImportsResponse"
    "fixture/ListImportsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListImports)

responseListTables :: ListTablesResponse -> TestTree
responseListTables =
  res
    "ListTablesResponse"
    "fixture/ListTablesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTables)

responseListTagsOfResource :: ListTagsOfResourceResponse -> TestTree
responseListTagsOfResource =
  res
    "ListTagsOfResourceResponse"
    "fixture/ListTagsOfResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsOfResource)

responsePutItem :: PutItemResponse -> TestTree
responsePutItem =
  res
    "PutItemResponse"
    "fixture/PutItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutItem)

responseQuery :: QueryResponse -> TestTree
responseQuery =
  res
    "QueryResponse"
    "fixture/QueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Query)

responseRestoreTableFromBackup :: RestoreTableFromBackupResponse -> TestTree
responseRestoreTableFromBackup =
  res
    "RestoreTableFromBackupResponse"
    "fixture/RestoreTableFromBackupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreTableFromBackup)

responseRestoreTableToPointInTime :: RestoreTableToPointInTimeResponse -> TestTree
responseRestoreTableToPointInTime =
  res
    "RestoreTableToPointInTimeResponse"
    "fixture/RestoreTableToPointInTimeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreTableToPointInTime)

responseScan :: ScanResponse -> TestTree
responseScan =
  res
    "ScanResponse"
    "fixture/ScanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Scan)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseTransactGetItems :: TransactGetItemsResponse -> TestTree
responseTransactGetItems =
  res
    "TransactGetItemsResponse"
    "fixture/TransactGetItemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TransactGetItems)

responseTransactWriteItems :: TransactWriteItemsResponse -> TestTree
responseTransactWriteItems =
  res
    "TransactWriteItemsResponse"
    "fixture/TransactWriteItemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TransactWriteItems)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateContinuousBackups :: UpdateContinuousBackupsResponse -> TestTree
responseUpdateContinuousBackups =
  res
    "UpdateContinuousBackupsResponse"
    "fixture/UpdateContinuousBackupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContinuousBackups)

responseUpdateContributorInsights :: UpdateContributorInsightsResponse -> TestTree
responseUpdateContributorInsights =
  res
    "UpdateContributorInsightsResponse"
    "fixture/UpdateContributorInsightsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContributorInsights)

responseUpdateGlobalTable :: UpdateGlobalTableResponse -> TestTree
responseUpdateGlobalTable =
  res
    "UpdateGlobalTableResponse"
    "fixture/UpdateGlobalTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGlobalTable)

responseUpdateGlobalTableSettings :: UpdateGlobalTableSettingsResponse -> TestTree
responseUpdateGlobalTableSettings =
  res
    "UpdateGlobalTableSettingsResponse"
    "fixture/UpdateGlobalTableSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGlobalTableSettings)

responseUpdateItem :: UpdateItemResponse -> TestTree
responseUpdateItem =
  res
    "UpdateItemResponse"
    "fixture/UpdateItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateItem)

responseUpdateTable :: UpdateTableResponse -> TestTree
responseUpdateTable =
  res
    "UpdateTableResponse"
    "fixture/UpdateTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTable)

responseUpdateTableReplicaAutoScaling :: UpdateTableReplicaAutoScalingResponse -> TestTree
responseUpdateTableReplicaAutoScaling =
  res
    "UpdateTableReplicaAutoScalingResponse"
    "fixture/UpdateTableReplicaAutoScalingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTableReplicaAutoScaling)

responseUpdateTimeToLive :: UpdateTimeToLiveResponse -> TestTree
responseUpdateTimeToLive =
  res
    "UpdateTimeToLiveResponse"
    "fixture/UpdateTimeToLiveResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTimeToLive)
