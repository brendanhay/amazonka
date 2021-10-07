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
--         , requestDisableKinesisStreamingDestination $
--             newDisableKinesisStreamingDestination
--
--         , requestListContributorInsights $
--             newListContributorInsights
--
--         , requestUpdateItem $
--             newUpdateItem
--
--         , requestDeleteItem $
--             newDeleteItem
--
--         , requestListGlobalTables $
--             newListGlobalTables
--
--         , requestUpdateContinuousBackups $
--             newUpdateContinuousBackups
--
--         , requestCreateGlobalTable $
--             newCreateGlobalTable
--
--         , requestRestoreTableFromBackup $
--             newRestoreTableFromBackup
--
--         , requestBatchExecuteStatement $
--             newBatchExecuteStatement
--
--         , requestDescribeContributorInsights $
--             newDescribeContributorInsights
--
--         , requestDescribeBackup $
--             newDescribeBackup
--
--         , requestExecuteTransaction $
--             newExecuteTransaction
--
--         , requestRestoreTableToPointInTime $
--             newRestoreTableToPointInTime
--
--         , requestDescribeLimits $
--             newDescribeLimits
--
--         , requestUntagResource $
--             newUntagResource
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
--         , requestCreateTable $
--             newCreateTable
--
--         , requestCreateBackup $
--             newCreateBackup
--
--         , requestQuery $
--             newQuery
--
--         , requestScan $
--             newScan
--
--         , requestUpdateTable $
--             newUpdateTable
--
--         , requestListBackups $
--             newListBackups
--
--         , requestDeleteTable $
--             newDeleteTable
--
--         , requestTransactWriteItems $
--             newTransactWriteItems
--
--         , requestListTables $
--             newListTables
--
--         , requestExportTableToPointInTime $
--             newExportTableToPointInTime
--
--         , requestBatchWriteItem $
--             newBatchWriteItem
--
--         , requestUpdateGlobalTable $
--             newUpdateGlobalTable
--
--         , requestTransactGetItems $
--             newTransactGetItems
--
--         , requestPutItem $
--             newPutItem
--
--         , requestDescribeExport $
--             newDescribeExport
--
--         , requestDescribeTableReplicaAutoScaling $
--             newDescribeTableReplicaAutoScaling
--
--         , requestUpdateTimeToLive $
--             newUpdateTimeToLive
--
--         , requestEnableKinesisStreamingDestination $
--             newEnableKinesisStreamingDestination
--
--         , requestUpdateGlobalTableSettings $
--             newUpdateGlobalTableSettings
--
--         , requestDescribeTable $
--             newDescribeTable
--
--         , requestGetItem $
--             newGetItem
--
--         , requestDescribeGlobalTable $
--             newDescribeGlobalTable
--
--         , requestDescribeContinuousBackups $
--             newDescribeContinuousBackups
--
--         , requestDescribeEndpoints $
--             newDescribeEndpoints
--
--         , requestListExports $
--             newListExports
--
--         , requestDescribeKinesisStreamingDestination $
--             newDescribeKinesisStreamingDestination
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
--         , responseDisableKinesisStreamingDestination $
--             newKinesisStreamingDestinationOutput
--
--         , responseListContributorInsights $
--             newListContributorInsightsResponse
--
--         , responseUpdateItem $
--             newUpdateItemResponse
--
--         , responseDeleteItem $
--             newDeleteItemResponse
--
--         , responseListGlobalTables $
--             newListGlobalTablesResponse
--
--         , responseUpdateContinuousBackups $
--             newUpdateContinuousBackupsResponse
--
--         , responseCreateGlobalTable $
--             newCreateGlobalTableResponse
--
--         , responseRestoreTableFromBackup $
--             newRestoreTableFromBackupResponse
--
--         , responseBatchExecuteStatement $
--             newBatchExecuteStatementResponse
--
--         , responseDescribeContributorInsights $
--             newDescribeContributorInsightsResponse
--
--         , responseDescribeBackup $
--             newDescribeBackupResponse
--
--         , responseExecuteTransaction $
--             newExecuteTransactionResponse
--
--         , responseRestoreTableToPointInTime $
--             newRestoreTableToPointInTimeResponse
--
--         , responseDescribeLimits $
--             newDescribeLimitsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
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
--         , responseCreateTable $
--             newCreateTableResponse
--
--         , responseCreateBackup $
--             newCreateBackupResponse
--
--         , responseQuery $
--             newQueryResponse
--
--         , responseScan $
--             newScanResponse
--
--         , responseUpdateTable $
--             newUpdateTableResponse
--
--         , responseListBackups $
--             newListBackupsResponse
--
--         , responseDeleteTable $
--             newDeleteTableResponse
--
--         , responseTransactWriteItems $
--             newTransactWriteItemsResponse
--
--         , responseListTables $
--             newListTablesResponse
--
--         , responseExportTableToPointInTime $
--             newExportTableToPointInTimeResponse
--
--         , responseBatchWriteItem $
--             newBatchWriteItemResponse
--
--         , responseUpdateGlobalTable $
--             newUpdateGlobalTableResponse
--
--         , responseTransactGetItems $
--             newTransactGetItemsResponse
--
--         , responsePutItem $
--             newPutItemResponse
--
--         , responseDescribeExport $
--             newDescribeExportResponse
--
--         , responseDescribeTableReplicaAutoScaling $
--             newDescribeTableReplicaAutoScalingResponse
--
--         , responseUpdateTimeToLive $
--             newUpdateTimeToLiveResponse
--
--         , responseEnableKinesisStreamingDestination $
--             newKinesisStreamingDestinationOutput
--
--         , responseUpdateGlobalTableSettings $
--             newUpdateGlobalTableSettingsResponse
--
--         , responseDescribeTable $
--             newDescribeTableResponse
--
--         , responseGetItem $
--             newGetItemResponse
--
--         , responseDescribeGlobalTable $
--             newDescribeGlobalTableResponse
--
--         , responseDescribeContinuousBackups $
--             newDescribeContinuousBackupsResponse
--
--         , responseDescribeEndpoints $
--             newDescribeEndpointsResponse
--
--         , responseListExports $
--             newListExportsResponse
--
--         , responseDescribeKinesisStreamingDestination $
--             newDescribeKinesisStreamingDestinationResponse
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

requestDisableKinesisStreamingDestination :: DisableKinesisStreamingDestination -> TestTree
requestDisableKinesisStreamingDestination =
  req
    "DisableKinesisStreamingDestination"
    "fixture/DisableKinesisStreamingDestination.yaml"

requestListContributorInsights :: ListContributorInsights -> TestTree
requestListContributorInsights =
  req
    "ListContributorInsights"
    "fixture/ListContributorInsights.yaml"

requestUpdateItem :: UpdateItem -> TestTree
requestUpdateItem =
  req
    "UpdateItem"
    "fixture/UpdateItem.yaml"

requestDeleteItem :: DeleteItem -> TestTree
requestDeleteItem =
  req
    "DeleteItem"
    "fixture/DeleteItem.yaml"

requestListGlobalTables :: ListGlobalTables -> TestTree
requestListGlobalTables =
  req
    "ListGlobalTables"
    "fixture/ListGlobalTables.yaml"

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

requestRestoreTableFromBackup :: RestoreTableFromBackup -> TestTree
requestRestoreTableFromBackup =
  req
    "RestoreTableFromBackup"
    "fixture/RestoreTableFromBackup.yaml"

requestBatchExecuteStatement :: BatchExecuteStatement -> TestTree
requestBatchExecuteStatement =
  req
    "BatchExecuteStatement"
    "fixture/BatchExecuteStatement.yaml"

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

requestDescribeLimits :: DescribeLimits -> TestTree
requestDescribeLimits =
  req
    "DescribeLimits"
    "fixture/DescribeLimits.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

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

requestQuery :: Query -> TestTree
requestQuery =
  req
    "Query"
    "fixture/Query.yaml"

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

requestListBackups :: ListBackups -> TestTree
requestListBackups =
  req
    "ListBackups"
    "fixture/ListBackups.yaml"

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

requestListTables :: ListTables -> TestTree
requestListTables =
  req
    "ListTables"
    "fixture/ListTables.yaml"

requestExportTableToPointInTime :: ExportTableToPointInTime -> TestTree
requestExportTableToPointInTime =
  req
    "ExportTableToPointInTime"
    "fixture/ExportTableToPointInTime.yaml"

requestBatchWriteItem :: BatchWriteItem -> TestTree
requestBatchWriteItem =
  req
    "BatchWriteItem"
    "fixture/BatchWriteItem.yaml"

requestUpdateGlobalTable :: UpdateGlobalTable -> TestTree
requestUpdateGlobalTable =
  req
    "UpdateGlobalTable"
    "fixture/UpdateGlobalTable.yaml"

requestTransactGetItems :: TransactGetItems -> TestTree
requestTransactGetItems =
  req
    "TransactGetItems"
    "fixture/TransactGetItems.yaml"

requestPutItem :: PutItem -> TestTree
requestPutItem =
  req
    "PutItem"
    "fixture/PutItem.yaml"

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

requestUpdateTimeToLive :: UpdateTimeToLive -> TestTree
requestUpdateTimeToLive =
  req
    "UpdateTimeToLive"
    "fixture/UpdateTimeToLive.yaml"

requestEnableKinesisStreamingDestination :: EnableKinesisStreamingDestination -> TestTree
requestEnableKinesisStreamingDestination =
  req
    "EnableKinesisStreamingDestination"
    "fixture/EnableKinesisStreamingDestination.yaml"

requestUpdateGlobalTableSettings :: UpdateGlobalTableSettings -> TestTree
requestUpdateGlobalTableSettings =
  req
    "UpdateGlobalTableSettings"
    "fixture/UpdateGlobalTableSettings.yaml"

requestDescribeTable :: DescribeTable -> TestTree
requestDescribeTable =
  req
    "DescribeTable"
    "fixture/DescribeTable.yaml"

requestGetItem :: GetItem -> TestTree
requestGetItem =
  req
    "GetItem"
    "fixture/GetItem.yaml"

requestDescribeGlobalTable :: DescribeGlobalTable -> TestTree
requestDescribeGlobalTable =
  req
    "DescribeGlobalTable"
    "fixture/DescribeGlobalTable.yaml"

requestDescribeContinuousBackups :: DescribeContinuousBackups -> TestTree
requestDescribeContinuousBackups =
  req
    "DescribeContinuousBackups"
    "fixture/DescribeContinuousBackups.yaml"

requestDescribeEndpoints :: DescribeEndpoints -> TestTree
requestDescribeEndpoints =
  req
    "DescribeEndpoints"
    "fixture/DescribeEndpoints.yaml"

requestListExports :: ListExports -> TestTree
requestListExports =
  req
    "ListExports"
    "fixture/ListExports.yaml"

requestDescribeKinesisStreamingDestination :: DescribeKinesisStreamingDestination -> TestTree
requestDescribeKinesisStreamingDestination =
  req
    "DescribeKinesisStreamingDestination"
    "fixture/DescribeKinesisStreamingDestination.yaml"

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

responseDisableKinesisStreamingDestination :: KinesisStreamingDestinationOutput -> TestTree
responseDisableKinesisStreamingDestination =
  res
    "DisableKinesisStreamingDestinationResponse"
    "fixture/DisableKinesisStreamingDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy DisableKinesisStreamingDestination)

responseListContributorInsights :: ListContributorInsightsResponse -> TestTree
responseListContributorInsights =
  res
    "ListContributorInsightsResponse"
    "fixture/ListContributorInsightsResponse.proto"
    defaultService
    (Proxy :: Proxy ListContributorInsights)

responseUpdateItem :: UpdateItemResponse -> TestTree
responseUpdateItem =
  res
    "UpdateItemResponse"
    "fixture/UpdateItemResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateItem)

responseDeleteItem :: DeleteItemResponse -> TestTree
responseDeleteItem =
  res
    "DeleteItemResponse"
    "fixture/DeleteItemResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteItem)

responseListGlobalTables :: ListGlobalTablesResponse -> TestTree
responseListGlobalTables =
  res
    "ListGlobalTablesResponse"
    "fixture/ListGlobalTablesResponse.proto"
    defaultService
    (Proxy :: Proxy ListGlobalTables)

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

responseRestoreTableFromBackup :: RestoreTableFromBackupResponse -> TestTree
responseRestoreTableFromBackup =
  res
    "RestoreTableFromBackupResponse"
    "fixture/RestoreTableFromBackupResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreTableFromBackup)

responseBatchExecuteStatement :: BatchExecuteStatementResponse -> TestTree
responseBatchExecuteStatement =
  res
    "BatchExecuteStatementResponse"
    "fixture/BatchExecuteStatementResponse.proto"
    defaultService
    (Proxy :: Proxy BatchExecuteStatement)

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

responseDescribeLimits :: DescribeLimitsResponse -> TestTree
responseDescribeLimits =
  res
    "DescribeLimitsResponse"
    "fixture/DescribeLimitsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLimits)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

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

responseQuery :: QueryResponse -> TestTree
responseQuery =
  res
    "QueryResponse"
    "fixture/QueryResponse.proto"
    defaultService
    (Proxy :: Proxy Query)

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

responseListBackups :: ListBackupsResponse -> TestTree
responseListBackups =
  res
    "ListBackupsResponse"
    "fixture/ListBackupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListBackups)

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

responseListTables :: ListTablesResponse -> TestTree
responseListTables =
  res
    "ListTablesResponse"
    "fixture/ListTablesResponse.proto"
    defaultService
    (Proxy :: Proxy ListTables)

responseExportTableToPointInTime :: ExportTableToPointInTimeResponse -> TestTree
responseExportTableToPointInTime =
  res
    "ExportTableToPointInTimeResponse"
    "fixture/ExportTableToPointInTimeResponse.proto"
    defaultService
    (Proxy :: Proxy ExportTableToPointInTime)

responseBatchWriteItem :: BatchWriteItemResponse -> TestTree
responseBatchWriteItem =
  res
    "BatchWriteItemResponse"
    "fixture/BatchWriteItemResponse.proto"
    defaultService
    (Proxy :: Proxy BatchWriteItem)

responseUpdateGlobalTable :: UpdateGlobalTableResponse -> TestTree
responseUpdateGlobalTable =
  res
    "UpdateGlobalTableResponse"
    "fixture/UpdateGlobalTableResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGlobalTable)

responseTransactGetItems :: TransactGetItemsResponse -> TestTree
responseTransactGetItems =
  res
    "TransactGetItemsResponse"
    "fixture/TransactGetItemsResponse.proto"
    defaultService
    (Proxy :: Proxy TransactGetItems)

responsePutItem :: PutItemResponse -> TestTree
responsePutItem =
  res
    "PutItemResponse"
    "fixture/PutItemResponse.proto"
    defaultService
    (Proxy :: Proxy PutItem)

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

responseUpdateTimeToLive :: UpdateTimeToLiveResponse -> TestTree
responseUpdateTimeToLive =
  res
    "UpdateTimeToLiveResponse"
    "fixture/UpdateTimeToLiveResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTimeToLive)

responseEnableKinesisStreamingDestination :: KinesisStreamingDestinationOutput -> TestTree
responseEnableKinesisStreamingDestination =
  res
    "EnableKinesisStreamingDestinationResponse"
    "fixture/EnableKinesisStreamingDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy EnableKinesisStreamingDestination)

responseUpdateGlobalTableSettings :: UpdateGlobalTableSettingsResponse -> TestTree
responseUpdateGlobalTableSettings =
  res
    "UpdateGlobalTableSettingsResponse"
    "fixture/UpdateGlobalTableSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGlobalTableSettings)

responseDescribeTable :: DescribeTableResponse -> TestTree
responseDescribeTable =
  res
    "DescribeTableResponse"
    "fixture/DescribeTableResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTable)

responseGetItem :: GetItemResponse -> TestTree
responseGetItem =
  res
    "GetItemResponse"
    "fixture/GetItemResponse.proto"
    defaultService
    (Proxy :: Proxy GetItem)

responseDescribeGlobalTable :: DescribeGlobalTableResponse -> TestTree
responseDescribeGlobalTable =
  res
    "DescribeGlobalTableResponse"
    "fixture/DescribeGlobalTableResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeGlobalTable)

responseDescribeContinuousBackups :: DescribeContinuousBackupsResponse -> TestTree
responseDescribeContinuousBackups =
  res
    "DescribeContinuousBackupsResponse"
    "fixture/DescribeContinuousBackupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeContinuousBackups)

responseDescribeEndpoints :: DescribeEndpointsResponse -> TestTree
responseDescribeEndpoints =
  res
    "DescribeEndpointsResponse"
    "fixture/DescribeEndpointsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEndpoints)

responseListExports :: ListExportsResponse -> TestTree
responseListExports =
  res
    "ListExportsResponse"
    "fixture/ListExportsResponse.proto"
    defaultService
    (Proxy :: Proxy ListExports)

responseDescribeKinesisStreamingDestination :: DescribeKinesisStreamingDestinationResponse -> TestTree
responseDescribeKinesisStreamingDestination =
  res
    "DescribeKinesisStreamingDestinationResponse"
    "fixture/DescribeKinesisStreamingDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeKinesisStreamingDestination)

responseExecuteStatement :: ExecuteStatementResponse -> TestTree
responseExecuteStatement =
  res
    "ExecuteStatementResponse"
    "fixture/ExecuteStatementResponse.proto"
    defaultService
    (Proxy :: Proxy ExecuteStatement)
