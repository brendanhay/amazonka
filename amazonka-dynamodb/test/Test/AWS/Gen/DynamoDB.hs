{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DynamoDB
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.DynamoDB where

import Data.Proxy
import Network.AWS.DynamoDB
import Test.AWS.DynamoDB.Internal
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
--         [ requestBatchGetItem $
--             newBatchGetItem
--
--         , requestUpdateContributorInsights $
--             newUpdateContributorInsights
--
--         , requestDeleteBackup $
--             newDeleteBackup
--
--         , requestDeleteItem $
--             newDeleteItem
--
--         , requestUpdateItem $
--             newUpdateItem
--
--         , requestListContributorInsights $
--             newListContributorInsights
--
--         , requestListGlobalTables $
--             newListGlobalTables
--
--         , requestDisableKinesisStreamingDestination $
--             newDisableKinesisStreamingDestination
--
--         , requestUpdateContinuousBackups $
--             newUpdateContinuousBackups
--
--         , requestCreateGlobalTable $
--             newCreateGlobalTable
--
--         , requestBatchExecuteStatement $
--             newBatchExecuteStatement
--
--         , requestRestoreTableFromBackup $
--             newRestoreTableFromBackup
--
--         , requestDescribeLimits $
--             newDescribeLimits
--
--         , requestExecuteTransaction $
--             newExecuteTransaction
--
--         , requestRestoreTableToPointInTime $
--             newRestoreTableToPointInTime
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeContributorInsights $
--             newDescribeContributorInsights
--
--         , requestDescribeBackup $
--             newDescribeBackup
--
--         , requestListTagsOfResource $
--             newListTagsOfResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDescribeGlobalTableSettings $
--             newDescribeGlobalTableSettings
--
--         , requestUpdateTableReplicaAutoScaling $
--             newUpdateTableReplicaAutoScaling
--
--         , requestDescribeTimeToLive $
--             newDescribeTimeToLive
--
--         , requestQuery $
--             newQuery
--
--         , requestCreateTable $
--             newCreateTable
--
--         , requestCreateBackup $
--             newCreateBackup
--
--         , requestListTables $
--             newListTables
--
--         , requestScan $
--             newScan
--
--         , requestUpdateTable $
--             newUpdateTable
--
--         , requestDeleteTable $
--             newDeleteTable
--
--         , requestTransactWriteItems $
--             newTransactWriteItems
--
--         , requestExportTableToPointInTime $
--             newExportTableToPointInTime
--
--         , requestListBackups $
--             newListBackups
--
--         , requestTransactGetItems $
--             newTransactGetItems
--
--         , requestUpdateGlobalTable $
--             newUpdateGlobalTable
--
--         , requestBatchWriteItem $
--             newBatchWriteItem
--
--         , requestPutItem $
--             newPutItem
--
--         , requestUpdateTimeToLive $
--             newUpdateTimeToLive
--
--         , requestUpdateGlobalTableSettings $
--             newUpdateGlobalTableSettings
--
--         , requestEnableKinesisStreamingDestination $
--             newEnableKinesisStreamingDestination
--
--         , requestDescribeExport $
--             newDescribeExport
--
--         , requestDescribeTableReplicaAutoScaling $
--             newDescribeTableReplicaAutoScaling
--
--         , requestGetItem $
--             newGetItem
--
--         , requestDescribeTable $
--             newDescribeTable
--
--         , requestDescribeGlobalTable $
--             newDescribeGlobalTable
--
--         , requestListExports $
--             newListExports
--
--         , requestDescribeContinuousBackups $
--             newDescribeContinuousBackups
--
--         , requestDescribeKinesisStreamingDestination $
--             newDescribeKinesisStreamingDestination
--
--         , requestDescribeEndpoints $
--             newDescribeEndpoints
--
--         , requestExecuteStatement $
--             newExecuteStatement
--
--           ]

--     , testGroup "response"
--         [ responseBatchGetItem $
--             newBatchGetItemResponse
--
--         , responseUpdateContributorInsights $
--             newUpdateContributorInsightsResponse
--
--         , responseDeleteBackup $
--             newDeleteBackupResponse
--
--         , responseDeleteItem $
--             newDeleteItemResponse
--
--         , responseUpdateItem $
--             newUpdateItemResponse
--
--         , responseListContributorInsights $
--             newListContributorInsightsResponse
--
--         , responseListGlobalTables $
--             newListGlobalTablesResponse
--
--         , responseDisableKinesisStreamingDestination $
--             newKinesisStreamingDestinationOutput
--
--         , responseUpdateContinuousBackups $
--             newUpdateContinuousBackupsResponse
--
--         , responseCreateGlobalTable $
--             newCreateGlobalTableResponse
--
--         , responseBatchExecuteStatement $
--             newBatchExecuteStatementResponse
--
--         , responseRestoreTableFromBackup $
--             newRestoreTableFromBackupResponse
--
--         , responseDescribeLimits $
--             newDescribeLimitsResponse
--
--         , responseExecuteTransaction $
--             newExecuteTransactionResponse
--
--         , responseRestoreTableToPointInTime $
--             newRestoreTableToPointInTimeResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeContributorInsights $
--             newDescribeContributorInsightsResponse
--
--         , responseDescribeBackup $
--             newDescribeBackupResponse
--
--         , responseListTagsOfResource $
--             newListTagsOfResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDescribeGlobalTableSettings $
--             newDescribeGlobalTableSettingsResponse
--
--         , responseUpdateTableReplicaAutoScaling $
--             newUpdateTableReplicaAutoScalingResponse
--
--         , responseDescribeTimeToLive $
--             newDescribeTimeToLiveResponse
--
--         , responseQuery $
--             newQueryResponse
--
--         , responseCreateTable $
--             newCreateTableResponse
--
--         , responseCreateBackup $
--             newCreateBackupResponse
--
--         , responseListTables $
--             newListTablesResponse
--
--         , responseScan $
--             newScanResponse
--
--         , responseUpdateTable $
--             newUpdateTableResponse
--
--         , responseDeleteTable $
--             newDeleteTableResponse
--
--         , responseTransactWriteItems $
--             newTransactWriteItemsResponse
--
--         , responseExportTableToPointInTime $
--             newExportTableToPointInTimeResponse
--
--         , responseListBackups $
--             newListBackupsResponse
--
--         , responseTransactGetItems $
--             newTransactGetItemsResponse
--
--         , responseUpdateGlobalTable $
--             newUpdateGlobalTableResponse
--
--         , responseBatchWriteItem $
--             newBatchWriteItemResponse
--
--         , responsePutItem $
--             newPutItemResponse
--
--         , responseUpdateTimeToLive $
--             newUpdateTimeToLiveResponse
--
--         , responseUpdateGlobalTableSettings $
--             newUpdateGlobalTableSettingsResponse
--
--         , responseEnableKinesisStreamingDestination $
--             newKinesisStreamingDestinationOutput
--
--         , responseDescribeExport $
--             newDescribeExportResponse
--
--         , responseDescribeTableReplicaAutoScaling $
--             newDescribeTableReplicaAutoScalingResponse
--
--         , responseGetItem $
--             newGetItemResponse
--
--         , responseDescribeTable $
--             newDescribeTableResponse
--
--         , responseDescribeGlobalTable $
--             newDescribeGlobalTableResponse
--
--         , responseListExports $
--             newListExportsResponse
--
--         , responseDescribeContinuousBackups $
--             newDescribeContinuousBackupsResponse
--
--         , responseDescribeKinesisStreamingDestination $
--             newDescribeKinesisStreamingDestinationResponse
--
--         , responseDescribeEndpoints $
--             newDescribeEndpointsResponse
--
--         , responseExecuteStatement $
--             newExecuteStatementResponse
--
--           ]
--     ]

-- Requests

requestBatchGetItem :: BatchGetItem -> TestTree
requestBatchGetItem =
  req
    "BatchGetItem"
    "fixture/BatchGetItem.yaml"

requestUpdateContributorInsights :: UpdateContributorInsights -> TestTree
requestUpdateContributorInsights =
  req
    "UpdateContributorInsights"
    "fixture/UpdateContributorInsights.yaml"

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

requestUpdateItem :: UpdateItem -> TestTree
requestUpdateItem =
  req
    "UpdateItem"
    "fixture/UpdateItem.yaml"

requestListContributorInsights :: ListContributorInsights -> TestTree
requestListContributorInsights =
  req
    "ListContributorInsights"
    "fixture/ListContributorInsights.yaml"

requestListGlobalTables :: ListGlobalTables -> TestTree
requestListGlobalTables =
  req
    "ListGlobalTables"
    "fixture/ListGlobalTables.yaml"

requestDisableKinesisStreamingDestination :: DisableKinesisStreamingDestination -> TestTree
requestDisableKinesisStreamingDestination =
  req
    "DisableKinesisStreamingDestination"
    "fixture/DisableKinesisStreamingDestination.yaml"

requestUpdateContinuousBackups :: UpdateContinuousBackups -> TestTree
requestUpdateContinuousBackups =
  req
    "UpdateContinuousBackups"
    "fixture/UpdateContinuousBackups.yaml"

requestCreateGlobalTable :: CreateGlobalTable -> TestTree
requestCreateGlobalTable =
  req
    "CreateGlobalTable"
    "fixture/CreateGlobalTable.yaml"

requestBatchExecuteStatement :: BatchExecuteStatement -> TestTree
requestBatchExecuteStatement =
  req
    "BatchExecuteStatement"
    "fixture/BatchExecuteStatement.yaml"

requestRestoreTableFromBackup :: RestoreTableFromBackup -> TestTree
requestRestoreTableFromBackup =
  req
    "RestoreTableFromBackup"
    "fixture/RestoreTableFromBackup.yaml"

requestDescribeLimits :: DescribeLimits -> TestTree
requestDescribeLimits =
  req
    "DescribeLimits"
    "fixture/DescribeLimits.yaml"

requestExecuteTransaction :: ExecuteTransaction -> TestTree
requestExecuteTransaction =
  req
    "ExecuteTransaction"
    "fixture/ExecuteTransaction.yaml"

requestRestoreTableToPointInTime :: RestoreTableToPointInTime -> TestTree
requestRestoreTableToPointInTime =
  req
    "RestoreTableToPointInTime"
    "fixture/RestoreTableToPointInTime.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDescribeContributorInsights :: DescribeContributorInsights -> TestTree
requestDescribeContributorInsights =
  req
    "DescribeContributorInsights"
    "fixture/DescribeContributorInsights.yaml"

requestDescribeBackup :: DescribeBackup -> TestTree
requestDescribeBackup =
  req
    "DescribeBackup"
    "fixture/DescribeBackup.yaml"

requestListTagsOfResource :: ListTagsOfResource -> TestTree
requestListTagsOfResource =
  req
    "ListTagsOfResource"
    "fixture/ListTagsOfResource.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDescribeGlobalTableSettings :: DescribeGlobalTableSettings -> TestTree
requestDescribeGlobalTableSettings =
  req
    "DescribeGlobalTableSettings"
    "fixture/DescribeGlobalTableSettings.yaml"

requestUpdateTableReplicaAutoScaling :: UpdateTableReplicaAutoScaling -> TestTree
requestUpdateTableReplicaAutoScaling =
  req
    "UpdateTableReplicaAutoScaling"
    "fixture/UpdateTableReplicaAutoScaling.yaml"

requestDescribeTimeToLive :: DescribeTimeToLive -> TestTree
requestDescribeTimeToLive =
  req
    "DescribeTimeToLive"
    "fixture/DescribeTimeToLive.yaml"

requestQuery :: Query -> TestTree
requestQuery =
  req
    "Query"
    "fixture/Query.yaml"

requestCreateTable :: CreateTable -> TestTree
requestCreateTable =
  req
    "CreateTable"
    "fixture/CreateTable.yaml"

requestCreateBackup :: CreateBackup -> TestTree
requestCreateBackup =
  req
    "CreateBackup"
    "fixture/CreateBackup.yaml"

requestListTables :: ListTables -> TestTree
requestListTables =
  req
    "ListTables"
    "fixture/ListTables.yaml"

requestScan :: Scan -> TestTree
requestScan =
  req
    "Scan"
    "fixture/Scan.yaml"

requestUpdateTable :: UpdateTable -> TestTree
requestUpdateTable =
  req
    "UpdateTable"
    "fixture/UpdateTable.yaml"

requestDeleteTable :: DeleteTable -> TestTree
requestDeleteTable =
  req
    "DeleteTable"
    "fixture/DeleteTable.yaml"

requestTransactWriteItems :: TransactWriteItems -> TestTree
requestTransactWriteItems =
  req
    "TransactWriteItems"
    "fixture/TransactWriteItems.yaml"

requestExportTableToPointInTime :: ExportTableToPointInTime -> TestTree
requestExportTableToPointInTime =
  req
    "ExportTableToPointInTime"
    "fixture/ExportTableToPointInTime.yaml"

requestListBackups :: ListBackups -> TestTree
requestListBackups =
  req
    "ListBackups"
    "fixture/ListBackups.yaml"

requestTransactGetItems :: TransactGetItems -> TestTree
requestTransactGetItems =
  req
    "TransactGetItems"
    "fixture/TransactGetItems.yaml"

requestUpdateGlobalTable :: UpdateGlobalTable -> TestTree
requestUpdateGlobalTable =
  req
    "UpdateGlobalTable"
    "fixture/UpdateGlobalTable.yaml"

requestBatchWriteItem :: BatchWriteItem -> TestTree
requestBatchWriteItem =
  req
    "BatchWriteItem"
    "fixture/BatchWriteItem.yaml"

requestPutItem :: PutItem -> TestTree
requestPutItem =
  req
    "PutItem"
    "fixture/PutItem.yaml"

requestUpdateTimeToLive :: UpdateTimeToLive -> TestTree
requestUpdateTimeToLive =
  req
    "UpdateTimeToLive"
    "fixture/UpdateTimeToLive.yaml"

requestUpdateGlobalTableSettings :: UpdateGlobalTableSettings -> TestTree
requestUpdateGlobalTableSettings =
  req
    "UpdateGlobalTableSettings"
    "fixture/UpdateGlobalTableSettings.yaml"

requestEnableKinesisStreamingDestination :: EnableKinesisStreamingDestination -> TestTree
requestEnableKinesisStreamingDestination =
  req
    "EnableKinesisStreamingDestination"
    "fixture/EnableKinesisStreamingDestination.yaml"

requestDescribeExport :: DescribeExport -> TestTree
requestDescribeExport =
  req
    "DescribeExport"
    "fixture/DescribeExport.yaml"

requestDescribeTableReplicaAutoScaling :: DescribeTableReplicaAutoScaling -> TestTree
requestDescribeTableReplicaAutoScaling =
  req
    "DescribeTableReplicaAutoScaling"
    "fixture/DescribeTableReplicaAutoScaling.yaml"

requestGetItem :: GetItem -> TestTree
requestGetItem =
  req
    "GetItem"
    "fixture/GetItem.yaml"

requestDescribeTable :: DescribeTable -> TestTree
requestDescribeTable =
  req
    "DescribeTable"
    "fixture/DescribeTable.yaml"

requestDescribeGlobalTable :: DescribeGlobalTable -> TestTree
requestDescribeGlobalTable =
  req
    "DescribeGlobalTable"
    "fixture/DescribeGlobalTable.yaml"

requestListExports :: ListExports -> TestTree
requestListExports =
  req
    "ListExports"
    "fixture/ListExports.yaml"

requestDescribeContinuousBackups :: DescribeContinuousBackups -> TestTree
requestDescribeContinuousBackups =
  req
    "DescribeContinuousBackups"
    "fixture/DescribeContinuousBackups.yaml"

requestDescribeKinesisStreamingDestination :: DescribeKinesisStreamingDestination -> TestTree
requestDescribeKinesisStreamingDestination =
  req
    "DescribeKinesisStreamingDestination"
    "fixture/DescribeKinesisStreamingDestination.yaml"

requestDescribeEndpoints :: DescribeEndpoints -> TestTree
requestDescribeEndpoints =
  req
    "DescribeEndpoints"
    "fixture/DescribeEndpoints.yaml"

requestExecuteStatement :: ExecuteStatement -> TestTree
requestExecuteStatement =
  req
    "ExecuteStatement"
    "fixture/ExecuteStatement.yaml"

-- Responses

responseBatchGetItem :: BatchGetItemResponse -> TestTree
responseBatchGetItem =
  res
    "BatchGetItemResponse"
    "fixture/BatchGetItemResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetItem)

responseUpdateContributorInsights :: UpdateContributorInsightsResponse -> TestTree
responseUpdateContributorInsights =
  res
    "UpdateContributorInsightsResponse"
    "fixture/UpdateContributorInsightsResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateContributorInsights)

responseDeleteBackup :: DeleteBackupResponse -> TestTree
responseDeleteBackup =
  res
    "DeleteBackupResponse"
    "fixture/DeleteBackupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBackup)

responseDeleteItem :: DeleteItemResponse -> TestTree
responseDeleteItem =
  res
    "DeleteItemResponse"
    "fixture/DeleteItemResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteItem)

responseUpdateItem :: UpdateItemResponse -> TestTree
responseUpdateItem =
  res
    "UpdateItemResponse"
    "fixture/UpdateItemResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateItem)

responseListContributorInsights :: ListContributorInsightsResponse -> TestTree
responseListContributorInsights =
  res
    "ListContributorInsightsResponse"
    "fixture/ListContributorInsightsResponse.proto"
    defaultService
    (Proxy :: Proxy ListContributorInsights)

responseListGlobalTables :: ListGlobalTablesResponse -> TestTree
responseListGlobalTables =
  res
    "ListGlobalTablesResponse"
    "fixture/ListGlobalTablesResponse.proto"
    defaultService
    (Proxy :: Proxy ListGlobalTables)

responseDisableKinesisStreamingDestination :: KinesisStreamingDestinationOutput -> TestTree
responseDisableKinesisStreamingDestination =
  res
    "DisableKinesisStreamingDestinationResponse"
    "fixture/DisableKinesisStreamingDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy DisableKinesisStreamingDestination)

responseUpdateContinuousBackups :: UpdateContinuousBackupsResponse -> TestTree
responseUpdateContinuousBackups =
  res
    "UpdateContinuousBackupsResponse"
    "fixture/UpdateContinuousBackupsResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateContinuousBackups)

responseCreateGlobalTable :: CreateGlobalTableResponse -> TestTree
responseCreateGlobalTable =
  res
    "CreateGlobalTableResponse"
    "fixture/CreateGlobalTableResponse.proto"
    defaultService
    (Proxy :: Proxy CreateGlobalTable)

responseBatchExecuteStatement :: BatchExecuteStatementResponse -> TestTree
responseBatchExecuteStatement =
  res
    "BatchExecuteStatementResponse"
    "fixture/BatchExecuteStatementResponse.proto"
    defaultService
    (Proxy :: Proxy BatchExecuteStatement)

responseRestoreTableFromBackup :: RestoreTableFromBackupResponse -> TestTree
responseRestoreTableFromBackup =
  res
    "RestoreTableFromBackupResponse"
    "fixture/RestoreTableFromBackupResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreTableFromBackup)

responseDescribeLimits :: DescribeLimitsResponse -> TestTree
responseDescribeLimits =
  res
    "DescribeLimitsResponse"
    "fixture/DescribeLimitsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLimits)

responseExecuteTransaction :: ExecuteTransactionResponse -> TestTree
responseExecuteTransaction =
  res
    "ExecuteTransactionResponse"
    "fixture/ExecuteTransactionResponse.proto"
    defaultService
    (Proxy :: Proxy ExecuteTransaction)

responseRestoreTableToPointInTime :: RestoreTableToPointInTimeResponse -> TestTree
responseRestoreTableToPointInTime =
  res
    "RestoreTableToPointInTimeResponse"
    "fixture/RestoreTableToPointInTimeResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreTableToPointInTime)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDescribeContributorInsights :: DescribeContributorInsightsResponse -> TestTree
responseDescribeContributorInsights =
  res
    "DescribeContributorInsightsResponse"
    "fixture/DescribeContributorInsightsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeContributorInsights)

responseDescribeBackup :: DescribeBackupResponse -> TestTree
responseDescribeBackup =
  res
    "DescribeBackupResponse"
    "fixture/DescribeBackupResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBackup)

responseListTagsOfResource :: ListTagsOfResourceResponse -> TestTree
responseListTagsOfResource =
  res
    "ListTagsOfResourceResponse"
    "fixture/ListTagsOfResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsOfResource)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseDescribeGlobalTableSettings :: DescribeGlobalTableSettingsResponse -> TestTree
responseDescribeGlobalTableSettings =
  res
    "DescribeGlobalTableSettingsResponse"
    "fixture/DescribeGlobalTableSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeGlobalTableSettings)

responseUpdateTableReplicaAutoScaling :: UpdateTableReplicaAutoScalingResponse -> TestTree
responseUpdateTableReplicaAutoScaling =
  res
    "UpdateTableReplicaAutoScalingResponse"
    "fixture/UpdateTableReplicaAutoScalingResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTableReplicaAutoScaling)

responseDescribeTimeToLive :: DescribeTimeToLiveResponse -> TestTree
responseDescribeTimeToLive =
  res
    "DescribeTimeToLiveResponse"
    "fixture/DescribeTimeToLiveResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTimeToLive)

responseQuery :: QueryResponse -> TestTree
responseQuery =
  res
    "QueryResponse"
    "fixture/QueryResponse.proto"
    defaultService
    (Proxy :: Proxy Query)

responseCreateTable :: CreateTableResponse -> TestTree
responseCreateTable =
  res
    "CreateTableResponse"
    "fixture/CreateTableResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTable)

responseCreateBackup :: CreateBackupResponse -> TestTree
responseCreateBackup =
  res
    "CreateBackupResponse"
    "fixture/CreateBackupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateBackup)

responseListTables :: ListTablesResponse -> TestTree
responseListTables =
  res
    "ListTablesResponse"
    "fixture/ListTablesResponse.proto"
    defaultService
    (Proxy :: Proxy ListTables)

responseScan :: ScanResponse -> TestTree
responseScan =
  res
    "ScanResponse"
    "fixture/ScanResponse.proto"
    defaultService
    (Proxy :: Proxy Scan)

responseUpdateTable :: UpdateTableResponse -> TestTree
responseUpdateTable =
  res
    "UpdateTableResponse"
    "fixture/UpdateTableResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTable)

responseDeleteTable :: DeleteTableResponse -> TestTree
responseDeleteTable =
  res
    "DeleteTableResponse"
    "fixture/DeleteTableResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTable)

responseTransactWriteItems :: TransactWriteItemsResponse -> TestTree
responseTransactWriteItems =
  res
    "TransactWriteItemsResponse"
    "fixture/TransactWriteItemsResponse.proto"
    defaultService
    (Proxy :: Proxy TransactWriteItems)

responseExportTableToPointInTime :: ExportTableToPointInTimeResponse -> TestTree
responseExportTableToPointInTime =
  res
    "ExportTableToPointInTimeResponse"
    "fixture/ExportTableToPointInTimeResponse.proto"
    defaultService
    (Proxy :: Proxy ExportTableToPointInTime)

responseListBackups :: ListBackupsResponse -> TestTree
responseListBackups =
  res
    "ListBackupsResponse"
    "fixture/ListBackupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListBackups)

responseTransactGetItems :: TransactGetItemsResponse -> TestTree
responseTransactGetItems =
  res
    "TransactGetItemsResponse"
    "fixture/TransactGetItemsResponse.proto"
    defaultService
    (Proxy :: Proxy TransactGetItems)

responseUpdateGlobalTable :: UpdateGlobalTableResponse -> TestTree
responseUpdateGlobalTable =
  res
    "UpdateGlobalTableResponse"
    "fixture/UpdateGlobalTableResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGlobalTable)

responseBatchWriteItem :: BatchWriteItemResponse -> TestTree
responseBatchWriteItem =
  res
    "BatchWriteItemResponse"
    "fixture/BatchWriteItemResponse.proto"
    defaultService
    (Proxy :: Proxy BatchWriteItem)

responsePutItem :: PutItemResponse -> TestTree
responsePutItem =
  res
    "PutItemResponse"
    "fixture/PutItemResponse.proto"
    defaultService
    (Proxy :: Proxy PutItem)

responseUpdateTimeToLive :: UpdateTimeToLiveResponse -> TestTree
responseUpdateTimeToLive =
  res
    "UpdateTimeToLiveResponse"
    "fixture/UpdateTimeToLiveResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTimeToLive)

responseUpdateGlobalTableSettings :: UpdateGlobalTableSettingsResponse -> TestTree
responseUpdateGlobalTableSettings =
  res
    "UpdateGlobalTableSettingsResponse"
    "fixture/UpdateGlobalTableSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGlobalTableSettings)

responseEnableKinesisStreamingDestination :: KinesisStreamingDestinationOutput -> TestTree
responseEnableKinesisStreamingDestination =
  res
    "EnableKinesisStreamingDestinationResponse"
    "fixture/EnableKinesisStreamingDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy EnableKinesisStreamingDestination)

responseDescribeExport :: DescribeExportResponse -> TestTree
responseDescribeExport =
  res
    "DescribeExportResponse"
    "fixture/DescribeExportResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeExport)

responseDescribeTableReplicaAutoScaling :: DescribeTableReplicaAutoScalingResponse -> TestTree
responseDescribeTableReplicaAutoScaling =
  res
    "DescribeTableReplicaAutoScalingResponse"
    "fixture/DescribeTableReplicaAutoScalingResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTableReplicaAutoScaling)

responseGetItem :: GetItemResponse -> TestTree
responseGetItem =
  res
    "GetItemResponse"
    "fixture/GetItemResponse.proto"
    defaultService
    (Proxy :: Proxy GetItem)

responseDescribeTable :: DescribeTableResponse -> TestTree
responseDescribeTable =
  res
    "DescribeTableResponse"
    "fixture/DescribeTableResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTable)

responseDescribeGlobalTable :: DescribeGlobalTableResponse -> TestTree
responseDescribeGlobalTable =
  res
    "DescribeGlobalTableResponse"
    "fixture/DescribeGlobalTableResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeGlobalTable)

responseListExports :: ListExportsResponse -> TestTree
responseListExports =
  res
    "ListExportsResponse"
    "fixture/ListExportsResponse.proto"
    defaultService
    (Proxy :: Proxy ListExports)

responseDescribeContinuousBackups :: DescribeContinuousBackupsResponse -> TestTree
responseDescribeContinuousBackups =
  res
    "DescribeContinuousBackupsResponse"
    "fixture/DescribeContinuousBackupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeContinuousBackups)

responseDescribeKinesisStreamingDestination :: DescribeKinesisStreamingDestinationResponse -> TestTree
responseDescribeKinesisStreamingDestination =
  res
    "DescribeKinesisStreamingDestinationResponse"
    "fixture/DescribeKinesisStreamingDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeKinesisStreamingDestination)

responseDescribeEndpoints :: DescribeEndpointsResponse -> TestTree
responseDescribeEndpoints =
  res
    "DescribeEndpointsResponse"
    "fixture/DescribeEndpointsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEndpoints)

responseExecuteStatement :: ExecuteStatementResponse -> TestTree
responseExecuteStatement =
  res
    "ExecuteStatementResponse"
    "fixture/ExecuteStatementResponse.proto"
    defaultService
    (Proxy :: Proxy ExecuteStatement)
