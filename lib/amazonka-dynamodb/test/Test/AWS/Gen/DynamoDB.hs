{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DynamoDB
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestPutItem $
--             putItem
--
--         , requestDeleteItem $
--             deleteItem
--
--         , requestUpdateItem $
--             updateItem
--
--         , requestDisableKinesisStreamingDestination $
--             disableKinesisStreamingDestination
--
--         , requestListGlobalTables $
--             listGlobalTables
--
--         , requestUpdateGlobalTable $
--             updateGlobalTable
--
--         , requestDeleteTable $
--             deleteTable
--
--         , requestUpdateTable $
--             updateTable
--
--         , requestBatchGetItem $
--             batchGetItem
--
--         , requestListBackups $
--             listBackups
--
--         , requestDeleteBackup $
--             deleteBackup
--
--         , requestCreateBackup $
--             createBackup
--
--         , requestUpdateTableReplicaAutoScaling $
--             updateTableReplicaAutoScaling
--
--         , requestDescribeGlobalTableSettings $
--             describeGlobalTableSettings
--
--         , requestListTagsOfResource $
--             listTagsOfResource
--
--         , requestDescribeGlobalTable $
--             describeGlobalTable
--
--         , requestDescribeTable $
--             describeTable
--
--         , requestDescribeLimits $
--             describeLimits
--
--         , requestExecuteTransaction $
--             executeTransaction
--
--         , requestGetItem $
--             getItem
--
--         , requestDescribeBackup $
--             describeBackup
--
--         , requestBatchExecuteStatement $
--             batchExecuteStatement
--
--         , requestDescribeTableReplicaAutoScaling $
--             describeTableReplicaAutoScaling
--
--         , requestUpdateGlobalTableSettings $
--             updateGlobalTableSettings
--
--         , requestEnableKinesisStreamingDestination $
--             enableKinesisStreamingDestination
--
--         , requestTransactGetItems $
--             transactGetItems
--
--         , requestListContributorInsights $
--             listContributorInsights
--
--         , requestBatchWriteItem $
--             batchWriteItem
--
--         , requestExportTableToPointInTime $
--             exportTableToPointInTime
--
--         , requestTransactWriteItems $
--             transactWriteItems
--
--         , requestListTables $
--             listTables
--
--         , requestScan $
--             scan
--
--         , requestUpdateContributorInsights $
--             updateContributorInsights
--
--         , requestExecuteStatement $
--             executeStatement
--
--         , requestQuery $
--             query
--
--         , requestCreateTable $
--             createTable
--
--         , requestDescribeKinesisStreamingDestination $
--             describeKinesisStreamingDestination
--
--         , requestDescribeEndpoints $
--             describeEndpoints
--
--         , requestDescribeTimeToLive $
--             describeTimeToLive
--
--         , requestDescribeContinuousBackups $
--             describeContinuousBackups
--
--         , requestListExports $
--             listExports
--
--         , requestTagResource $
--             tagResource
--
--         , requestDescribeContributorInsights $
--             describeContributorInsights
--
--         , requestUntagResource $
--             untagResource
--
--         , requestRestoreTableToPointInTime $
--             restoreTableToPointInTime
--
--         , requestRestoreTableFromBackup $
--             restoreTableFromBackup
--
--         , requestUpdateTimeToLive $
--             updateTimeToLive
--
--         , requestCreateGlobalTable $
--             createGlobalTable
--
--         , requestUpdateContinuousBackups $
--             updateContinuousBackups
--
--         , requestDescribeExport $
--             describeExport
--
--           ]

--     , testGroup "response"
--         [ responsePutItem $
--             putItemResponse
--
--         , responseDeleteItem $
--             deleteItemResponse
--
--         , responseUpdateItem $
--             updateItemResponse
--
--         , responseDisableKinesisStreamingDestination $
--             kinesisStreamingDestinationOutput
--
--         , responseListGlobalTables $
--             listGlobalTablesResponse
--
--         , responseUpdateGlobalTable $
--             updateGlobalTableResponse
--
--         , responseDeleteTable $
--             deleteTableResponse
--
--         , responseUpdateTable $
--             updateTableResponse
--
--         , responseBatchGetItem $
--             batchGetItemResponse
--
--         , responseListBackups $
--             listBackupsResponse
--
--         , responseDeleteBackup $
--             deleteBackupResponse
--
--         , responseCreateBackup $
--             createBackupResponse
--
--         , responseUpdateTableReplicaAutoScaling $
--             updateTableReplicaAutoScalingResponse
--
--         , responseDescribeGlobalTableSettings $
--             describeGlobalTableSettingsResponse
--
--         , responseListTagsOfResource $
--             listTagsOfResourceResponse
--
--         , responseDescribeGlobalTable $
--             describeGlobalTableResponse
--
--         , responseDescribeTable $
--             describeTableResponse
--
--         , responseDescribeLimits $
--             describeLimitsResponse
--
--         , responseExecuteTransaction $
--             executeTransactionResponse
--
--         , responseGetItem $
--             getItemResponse
--
--         , responseDescribeBackup $
--             describeBackupResponse
--
--         , responseBatchExecuteStatement $
--             batchExecuteStatementResponse
--
--         , responseDescribeTableReplicaAutoScaling $
--             describeTableReplicaAutoScalingResponse
--
--         , responseUpdateGlobalTableSettings $
--             updateGlobalTableSettingsResponse
--
--         , responseEnableKinesisStreamingDestination $
--             kinesisStreamingDestinationOutput
--
--         , responseTransactGetItems $
--             transactGetItemsResponse
--
--         , responseListContributorInsights $
--             listContributorInsightsResponse
--
--         , responseBatchWriteItem $
--             batchWriteItemResponse
--
--         , responseExportTableToPointInTime $
--             exportTableToPointInTimeResponse
--
--         , responseTransactWriteItems $
--             transactWriteItemsResponse
--
--         , responseListTables $
--             listTablesResponse
--
--         , responseScan $
--             scanResponse
--
--         , responseUpdateContributorInsights $
--             updateContributorInsightsResponse
--
--         , responseExecuteStatement $
--             executeStatementResponse
--
--         , responseQuery $
--             queryResponse
--
--         , responseCreateTable $
--             createTableResponse
--
--         , responseDescribeKinesisStreamingDestination $
--             describeKinesisStreamingDestinationResponse
--
--         , responseDescribeEndpoints $
--             describeEndpointsResponse
--
--         , responseDescribeTimeToLive $
--             describeTimeToLiveResponse
--
--         , responseDescribeContinuousBackups $
--             describeContinuousBackupsResponse
--
--         , responseListExports $
--             listExportsResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseDescribeContributorInsights $
--             describeContributorInsightsResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseRestoreTableToPointInTime $
--             restoreTableToPointInTimeResponse
--
--         , responseRestoreTableFromBackup $
--             restoreTableFromBackupResponse
--
--         , responseUpdateTimeToLive $
--             updateTimeToLiveResponse
--
--         , responseCreateGlobalTable $
--             createGlobalTableResponse
--
--         , responseUpdateContinuousBackups $
--             updateContinuousBackupsResponse
--
--         , responseDescribeExport $
--             describeExportResponse
--
--           ]
--     ]

-- Requests

requestPutItem :: PutItem -> TestTree
requestPutItem =
  req
    "PutItem"
    "fixture/PutItem.yaml"

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

requestDisableKinesisStreamingDestination :: DisableKinesisStreamingDestination -> TestTree
requestDisableKinesisStreamingDestination =
  req
    "DisableKinesisStreamingDestination"
    "fixture/DisableKinesisStreamingDestination.yaml"

requestListGlobalTables :: ListGlobalTables -> TestTree
requestListGlobalTables =
  req
    "ListGlobalTables"
    "fixture/ListGlobalTables.yaml"

requestUpdateGlobalTable :: UpdateGlobalTable -> TestTree
requestUpdateGlobalTable =
  req
    "UpdateGlobalTable"
    "fixture/UpdateGlobalTable.yaml"

requestDeleteTable :: DeleteTable -> TestTree
requestDeleteTable =
  req
    "DeleteTable"
    "fixture/DeleteTable.yaml"

requestUpdateTable :: UpdateTable -> TestTree
requestUpdateTable =
  req
    "UpdateTable"
    "fixture/UpdateTable.yaml"

requestBatchGetItem :: BatchGetItem -> TestTree
requestBatchGetItem =
  req
    "BatchGetItem"
    "fixture/BatchGetItem.yaml"

requestListBackups :: ListBackups -> TestTree
requestListBackups =
  req
    "ListBackups"
    "fixture/ListBackups.yaml"

requestDeleteBackup :: DeleteBackup -> TestTree
requestDeleteBackup =
  req
    "DeleteBackup"
    "fixture/DeleteBackup.yaml"

requestCreateBackup :: CreateBackup -> TestTree
requestCreateBackup =
  req
    "CreateBackup"
    "fixture/CreateBackup.yaml"

requestUpdateTableReplicaAutoScaling :: UpdateTableReplicaAutoScaling -> TestTree
requestUpdateTableReplicaAutoScaling =
  req
    "UpdateTableReplicaAutoScaling"
    "fixture/UpdateTableReplicaAutoScaling.yaml"

requestDescribeGlobalTableSettings :: DescribeGlobalTableSettings -> TestTree
requestDescribeGlobalTableSettings =
  req
    "DescribeGlobalTableSettings"
    "fixture/DescribeGlobalTableSettings.yaml"

requestListTagsOfResource :: ListTagsOfResource -> TestTree
requestListTagsOfResource =
  req
    "ListTagsOfResource"
    "fixture/ListTagsOfResource.yaml"

requestDescribeGlobalTable :: DescribeGlobalTable -> TestTree
requestDescribeGlobalTable =
  req
    "DescribeGlobalTable"
    "fixture/DescribeGlobalTable.yaml"

requestDescribeTable :: DescribeTable -> TestTree
requestDescribeTable =
  req
    "DescribeTable"
    "fixture/DescribeTable.yaml"

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

requestGetItem :: GetItem -> TestTree
requestGetItem =
  req
    "GetItem"
    "fixture/GetItem.yaml"

requestDescribeBackup :: DescribeBackup -> TestTree
requestDescribeBackup =
  req
    "DescribeBackup"
    "fixture/DescribeBackup.yaml"

requestBatchExecuteStatement :: BatchExecuteStatement -> TestTree
requestBatchExecuteStatement =
  req
    "BatchExecuteStatement"
    "fixture/BatchExecuteStatement.yaml"

requestDescribeTableReplicaAutoScaling :: DescribeTableReplicaAutoScaling -> TestTree
requestDescribeTableReplicaAutoScaling =
  req
    "DescribeTableReplicaAutoScaling"
    "fixture/DescribeTableReplicaAutoScaling.yaml"

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

requestTransactGetItems :: TransactGetItems -> TestTree
requestTransactGetItems =
  req
    "TransactGetItems"
    "fixture/TransactGetItems.yaml"

requestListContributorInsights :: ListContributorInsights -> TestTree
requestListContributorInsights =
  req
    "ListContributorInsights"
    "fixture/ListContributorInsights.yaml"

requestBatchWriteItem :: BatchWriteItem -> TestTree
requestBatchWriteItem =
  req
    "BatchWriteItem"
    "fixture/BatchWriteItem.yaml"

requestExportTableToPointInTime :: ExportTableToPointInTime -> TestTree
requestExportTableToPointInTime =
  req
    "ExportTableToPointInTime"
    "fixture/ExportTableToPointInTime.yaml"

requestTransactWriteItems :: TransactWriteItems -> TestTree
requestTransactWriteItems =
  req
    "TransactWriteItems"
    "fixture/TransactWriteItems.yaml"

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

requestUpdateContributorInsights :: UpdateContributorInsights -> TestTree
requestUpdateContributorInsights =
  req
    "UpdateContributorInsights"
    "fixture/UpdateContributorInsights.yaml"

requestExecuteStatement :: ExecuteStatement -> TestTree
requestExecuteStatement =
  req
    "ExecuteStatement"
    "fixture/ExecuteStatement.yaml"

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

requestDescribeTimeToLive :: DescribeTimeToLive -> TestTree
requestDescribeTimeToLive =
  req
    "DescribeTimeToLive"
    "fixture/DescribeTimeToLive.yaml"

requestDescribeContinuousBackups :: DescribeContinuousBackups -> TestTree
requestDescribeContinuousBackups =
  req
    "DescribeContinuousBackups"
    "fixture/DescribeContinuousBackups.yaml"

requestListExports :: ListExports -> TestTree
requestListExports =
  req
    "ListExports"
    "fixture/ListExports.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDescribeContributorInsights :: DescribeContributorInsights -> TestTree
requestDescribeContributorInsights =
  req
    "DescribeContributorInsights"
    "fixture/DescribeContributorInsights.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestRestoreTableToPointInTime :: RestoreTableToPointInTime -> TestTree
requestRestoreTableToPointInTime =
  req
    "RestoreTableToPointInTime"
    "fixture/RestoreTableToPointInTime.yaml"

requestRestoreTableFromBackup :: RestoreTableFromBackup -> TestTree
requestRestoreTableFromBackup =
  req
    "RestoreTableFromBackup"
    "fixture/RestoreTableFromBackup.yaml"

requestUpdateTimeToLive :: UpdateTimeToLive -> TestTree
requestUpdateTimeToLive =
  req
    "UpdateTimeToLive"
    "fixture/UpdateTimeToLive.yaml"

requestCreateGlobalTable :: CreateGlobalTable -> TestTree
requestCreateGlobalTable =
  req
    "CreateGlobalTable"
    "fixture/CreateGlobalTable.yaml"

requestUpdateContinuousBackups :: UpdateContinuousBackups -> TestTree
requestUpdateContinuousBackups =
  req
    "UpdateContinuousBackups"
    "fixture/UpdateContinuousBackups.yaml"

requestDescribeExport :: DescribeExport -> TestTree
requestDescribeExport =
  req
    "DescribeExport"
    "fixture/DescribeExport.yaml"

-- Responses

responsePutItem :: PutItemResponse -> TestTree
responsePutItem =
  res
    "PutItemResponse"
    "fixture/PutItemResponse.proto"
    dynamoDB
    (Proxy :: Proxy PutItem)

responseDeleteItem :: DeleteItemResponse -> TestTree
responseDeleteItem =
  res
    "DeleteItemResponse"
    "fixture/DeleteItemResponse.proto"
    dynamoDB
    (Proxy :: Proxy DeleteItem)

responseUpdateItem :: UpdateItemResponse -> TestTree
responseUpdateItem =
  res
    "UpdateItemResponse"
    "fixture/UpdateItemResponse.proto"
    dynamoDB
    (Proxy :: Proxy UpdateItem)

responseDisableKinesisStreamingDestination :: KinesisStreamingDestinationOutput -> TestTree
responseDisableKinesisStreamingDestination =
  res
    "DisableKinesisStreamingDestinationResponse"
    "fixture/DisableKinesisStreamingDestinationResponse.proto"
    dynamoDB
    (Proxy :: Proxy DisableKinesisStreamingDestination)

responseListGlobalTables :: ListGlobalTablesResponse -> TestTree
responseListGlobalTables =
  res
    "ListGlobalTablesResponse"
    "fixture/ListGlobalTablesResponse.proto"
    dynamoDB
    (Proxy :: Proxy ListGlobalTables)

responseUpdateGlobalTable :: UpdateGlobalTableResponse -> TestTree
responseUpdateGlobalTable =
  res
    "UpdateGlobalTableResponse"
    "fixture/UpdateGlobalTableResponse.proto"
    dynamoDB
    (Proxy :: Proxy UpdateGlobalTable)

responseDeleteTable :: DeleteTableResponse -> TestTree
responseDeleteTable =
  res
    "DeleteTableResponse"
    "fixture/DeleteTableResponse.proto"
    dynamoDB
    (Proxy :: Proxy DeleteTable)

responseUpdateTable :: UpdateTableResponse -> TestTree
responseUpdateTable =
  res
    "UpdateTableResponse"
    "fixture/UpdateTableResponse.proto"
    dynamoDB
    (Proxy :: Proxy UpdateTable)

responseBatchGetItem :: BatchGetItemResponse -> TestTree
responseBatchGetItem =
  res
    "BatchGetItemResponse"
    "fixture/BatchGetItemResponse.proto"
    dynamoDB
    (Proxy :: Proxy BatchGetItem)

responseListBackups :: ListBackupsResponse -> TestTree
responseListBackups =
  res
    "ListBackupsResponse"
    "fixture/ListBackupsResponse.proto"
    dynamoDB
    (Proxy :: Proxy ListBackups)

responseDeleteBackup :: DeleteBackupResponse -> TestTree
responseDeleteBackup =
  res
    "DeleteBackupResponse"
    "fixture/DeleteBackupResponse.proto"
    dynamoDB
    (Proxy :: Proxy DeleteBackup)

responseCreateBackup :: CreateBackupResponse -> TestTree
responseCreateBackup =
  res
    "CreateBackupResponse"
    "fixture/CreateBackupResponse.proto"
    dynamoDB
    (Proxy :: Proxy CreateBackup)

responseUpdateTableReplicaAutoScaling :: UpdateTableReplicaAutoScalingResponse -> TestTree
responseUpdateTableReplicaAutoScaling =
  res
    "UpdateTableReplicaAutoScalingResponse"
    "fixture/UpdateTableReplicaAutoScalingResponse.proto"
    dynamoDB
    (Proxy :: Proxy UpdateTableReplicaAutoScaling)

responseDescribeGlobalTableSettings :: DescribeGlobalTableSettingsResponse -> TestTree
responseDescribeGlobalTableSettings =
  res
    "DescribeGlobalTableSettingsResponse"
    "fixture/DescribeGlobalTableSettingsResponse.proto"
    dynamoDB
    (Proxy :: Proxy DescribeGlobalTableSettings)

responseListTagsOfResource :: ListTagsOfResourceResponse -> TestTree
responseListTagsOfResource =
  res
    "ListTagsOfResourceResponse"
    "fixture/ListTagsOfResourceResponse.proto"
    dynamoDB
    (Proxy :: Proxy ListTagsOfResource)

responseDescribeGlobalTable :: DescribeGlobalTableResponse -> TestTree
responseDescribeGlobalTable =
  res
    "DescribeGlobalTableResponse"
    "fixture/DescribeGlobalTableResponse.proto"
    dynamoDB
    (Proxy :: Proxy DescribeGlobalTable)

responseDescribeTable :: DescribeTableResponse -> TestTree
responseDescribeTable =
  res
    "DescribeTableResponse"
    "fixture/DescribeTableResponse.proto"
    dynamoDB
    (Proxy :: Proxy DescribeTable)

responseDescribeLimits :: DescribeLimitsResponse -> TestTree
responseDescribeLimits =
  res
    "DescribeLimitsResponse"
    "fixture/DescribeLimitsResponse.proto"
    dynamoDB
    (Proxy :: Proxy DescribeLimits)

responseExecuteTransaction :: ExecuteTransactionResponse -> TestTree
responseExecuteTransaction =
  res
    "ExecuteTransactionResponse"
    "fixture/ExecuteTransactionResponse.proto"
    dynamoDB
    (Proxy :: Proxy ExecuteTransaction)

responseGetItem :: GetItemResponse -> TestTree
responseGetItem =
  res
    "GetItemResponse"
    "fixture/GetItemResponse.proto"
    dynamoDB
    (Proxy :: Proxy GetItem)

responseDescribeBackup :: DescribeBackupResponse -> TestTree
responseDescribeBackup =
  res
    "DescribeBackupResponse"
    "fixture/DescribeBackupResponse.proto"
    dynamoDB
    (Proxy :: Proxy DescribeBackup)

responseBatchExecuteStatement :: BatchExecuteStatementResponse -> TestTree
responseBatchExecuteStatement =
  res
    "BatchExecuteStatementResponse"
    "fixture/BatchExecuteStatementResponse.proto"
    dynamoDB
    (Proxy :: Proxy BatchExecuteStatement)

responseDescribeTableReplicaAutoScaling :: DescribeTableReplicaAutoScalingResponse -> TestTree
responseDescribeTableReplicaAutoScaling =
  res
    "DescribeTableReplicaAutoScalingResponse"
    "fixture/DescribeTableReplicaAutoScalingResponse.proto"
    dynamoDB
    (Proxy :: Proxy DescribeTableReplicaAutoScaling)

responseUpdateGlobalTableSettings :: UpdateGlobalTableSettingsResponse -> TestTree
responseUpdateGlobalTableSettings =
  res
    "UpdateGlobalTableSettingsResponse"
    "fixture/UpdateGlobalTableSettingsResponse.proto"
    dynamoDB
    (Proxy :: Proxy UpdateGlobalTableSettings)

responseEnableKinesisStreamingDestination :: KinesisStreamingDestinationOutput -> TestTree
responseEnableKinesisStreamingDestination =
  res
    "EnableKinesisStreamingDestinationResponse"
    "fixture/EnableKinesisStreamingDestinationResponse.proto"
    dynamoDB
    (Proxy :: Proxy EnableKinesisStreamingDestination)

responseTransactGetItems :: TransactGetItemsResponse -> TestTree
responseTransactGetItems =
  res
    "TransactGetItemsResponse"
    "fixture/TransactGetItemsResponse.proto"
    dynamoDB
    (Proxy :: Proxy TransactGetItems)

responseListContributorInsights :: ListContributorInsightsResponse -> TestTree
responseListContributorInsights =
  res
    "ListContributorInsightsResponse"
    "fixture/ListContributorInsightsResponse.proto"
    dynamoDB
    (Proxy :: Proxy ListContributorInsights)

responseBatchWriteItem :: BatchWriteItemResponse -> TestTree
responseBatchWriteItem =
  res
    "BatchWriteItemResponse"
    "fixture/BatchWriteItemResponse.proto"
    dynamoDB
    (Proxy :: Proxy BatchWriteItem)

responseExportTableToPointInTime :: ExportTableToPointInTimeResponse -> TestTree
responseExportTableToPointInTime =
  res
    "ExportTableToPointInTimeResponse"
    "fixture/ExportTableToPointInTimeResponse.proto"
    dynamoDB
    (Proxy :: Proxy ExportTableToPointInTime)

responseTransactWriteItems :: TransactWriteItemsResponse -> TestTree
responseTransactWriteItems =
  res
    "TransactWriteItemsResponse"
    "fixture/TransactWriteItemsResponse.proto"
    dynamoDB
    (Proxy :: Proxy TransactWriteItems)

responseListTables :: ListTablesResponse -> TestTree
responseListTables =
  res
    "ListTablesResponse"
    "fixture/ListTablesResponse.proto"
    dynamoDB
    (Proxy :: Proxy ListTables)

responseScan :: ScanResponse -> TestTree
responseScan =
  res
    "ScanResponse"
    "fixture/ScanResponse.proto"
    dynamoDB
    (Proxy :: Proxy Scan)

responseUpdateContributorInsights :: UpdateContributorInsightsResponse -> TestTree
responseUpdateContributorInsights =
  res
    "UpdateContributorInsightsResponse"
    "fixture/UpdateContributorInsightsResponse.proto"
    dynamoDB
    (Proxy :: Proxy UpdateContributorInsights)

responseExecuteStatement :: ExecuteStatementResponse -> TestTree
responseExecuteStatement =
  res
    "ExecuteStatementResponse"
    "fixture/ExecuteStatementResponse.proto"
    dynamoDB
    (Proxy :: Proxy ExecuteStatement)

responseQuery :: QueryResponse -> TestTree
responseQuery =
  res
    "QueryResponse"
    "fixture/QueryResponse.proto"
    dynamoDB
    (Proxy :: Proxy Query)

responseCreateTable :: CreateTableResponse -> TestTree
responseCreateTable =
  res
    "CreateTableResponse"
    "fixture/CreateTableResponse.proto"
    dynamoDB
    (Proxy :: Proxy CreateTable)

responseDescribeKinesisStreamingDestination :: DescribeKinesisStreamingDestinationResponse -> TestTree
responseDescribeKinesisStreamingDestination =
  res
    "DescribeKinesisStreamingDestinationResponse"
    "fixture/DescribeKinesisStreamingDestinationResponse.proto"
    dynamoDB
    (Proxy :: Proxy DescribeKinesisStreamingDestination)

responseDescribeEndpoints :: DescribeEndpointsResponse -> TestTree
responseDescribeEndpoints =
  res
    "DescribeEndpointsResponse"
    "fixture/DescribeEndpointsResponse.proto"
    dynamoDB
    (Proxy :: Proxy DescribeEndpoints)

responseDescribeTimeToLive :: DescribeTimeToLiveResponse -> TestTree
responseDescribeTimeToLive =
  res
    "DescribeTimeToLiveResponse"
    "fixture/DescribeTimeToLiveResponse.proto"
    dynamoDB
    (Proxy :: Proxy DescribeTimeToLive)

responseDescribeContinuousBackups :: DescribeContinuousBackupsResponse -> TestTree
responseDescribeContinuousBackups =
  res
    "DescribeContinuousBackupsResponse"
    "fixture/DescribeContinuousBackupsResponse.proto"
    dynamoDB
    (Proxy :: Proxy DescribeContinuousBackups)

responseListExports :: ListExportsResponse -> TestTree
responseListExports =
  res
    "ListExportsResponse"
    "fixture/ListExportsResponse.proto"
    dynamoDB
    (Proxy :: Proxy ListExports)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    dynamoDB
    (Proxy :: Proxy TagResource)

responseDescribeContributorInsights :: DescribeContributorInsightsResponse -> TestTree
responseDescribeContributorInsights =
  res
    "DescribeContributorInsightsResponse"
    "fixture/DescribeContributorInsightsResponse.proto"
    dynamoDB
    (Proxy :: Proxy DescribeContributorInsights)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    dynamoDB
    (Proxy :: Proxy UntagResource)

responseRestoreTableToPointInTime :: RestoreTableToPointInTimeResponse -> TestTree
responseRestoreTableToPointInTime =
  res
    "RestoreTableToPointInTimeResponse"
    "fixture/RestoreTableToPointInTimeResponse.proto"
    dynamoDB
    (Proxy :: Proxy RestoreTableToPointInTime)

responseRestoreTableFromBackup :: RestoreTableFromBackupResponse -> TestTree
responseRestoreTableFromBackup =
  res
    "RestoreTableFromBackupResponse"
    "fixture/RestoreTableFromBackupResponse.proto"
    dynamoDB
    (Proxy :: Proxy RestoreTableFromBackup)

responseUpdateTimeToLive :: UpdateTimeToLiveResponse -> TestTree
responseUpdateTimeToLive =
  res
    "UpdateTimeToLiveResponse"
    "fixture/UpdateTimeToLiveResponse.proto"
    dynamoDB
    (Proxy :: Proxy UpdateTimeToLive)

responseCreateGlobalTable :: CreateGlobalTableResponse -> TestTree
responseCreateGlobalTable =
  res
    "CreateGlobalTableResponse"
    "fixture/CreateGlobalTableResponse.proto"
    dynamoDB
    (Proxy :: Proxy CreateGlobalTable)

responseUpdateContinuousBackups :: UpdateContinuousBackupsResponse -> TestTree
responseUpdateContinuousBackups =
  res
    "UpdateContinuousBackupsResponse"
    "fixture/UpdateContinuousBackupsResponse.proto"
    dynamoDB
    (Proxy :: Proxy UpdateContinuousBackups)

responseDescribeExport :: DescribeExportResponse -> TestTree
responseDescribeExport =
  res
    "DescribeExportResponse"
    "fixture/DescribeExportResponse.proto"
    dynamoDB
    (Proxy :: Proxy DescribeExport)
