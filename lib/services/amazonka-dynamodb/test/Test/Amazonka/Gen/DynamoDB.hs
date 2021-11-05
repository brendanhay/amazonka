{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.DynamoDB
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--         [ requestPutItem $
--             newPutItem
--
--         , requestDeleteItem $
--             newDeleteItem
--
--         , requestUpdateItem $
--             newUpdateItem
--
--         , requestDisableKinesisStreamingDestination $
--             newDisableKinesisStreamingDestination
--
--         , requestListGlobalTables $
--             newListGlobalTables
--
--         , requestUpdateGlobalTable $
--             newUpdateGlobalTable
--
--         , requestDeleteTable $
--             newDeleteTable
--
--         , requestUpdateTable $
--             newUpdateTable
--
--         , requestBatchGetItem $
--             newBatchGetItem
--
--         , requestListBackups $
--             newListBackups
--
--         , requestDeleteBackup $
--             newDeleteBackup
--
--         , requestCreateBackup $
--             newCreateBackup
--
--         , requestUpdateTableReplicaAutoScaling $
--             newUpdateTableReplicaAutoScaling
--
--         , requestDescribeGlobalTableSettings $
--             newDescribeGlobalTableSettings
--
--         , requestListTagsOfResource $
--             newListTagsOfResource
--
--         , requestDescribeGlobalTable $
--             newDescribeGlobalTable
--
--         , requestDescribeTable $
--             newDescribeTable
--
--         , requestDescribeLimits $
--             newDescribeLimits
--
--         , requestExecuteTransaction $
--             newExecuteTransaction
--
--         , requestGetItem $
--             newGetItem
--
--         , requestDescribeBackup $
--             newDescribeBackup
--
--         , requestBatchExecuteStatement $
--             newBatchExecuteStatement
--
--         , requestDescribeTableReplicaAutoScaling $
--             newDescribeTableReplicaAutoScaling
--
--         , requestUpdateGlobalTableSettings $
--             newUpdateGlobalTableSettings
--
--         , requestEnableKinesisStreamingDestination $
--             newEnableKinesisStreamingDestination
--
--         , requestTransactGetItems $
--             newTransactGetItems
--
--         , requestListContributorInsights $
--             newListContributorInsights
--
--         , requestBatchWriteItem $
--             newBatchWriteItem
--
--         , requestExportTableToPointInTime $
--             newExportTableToPointInTime
--
--         , requestTransactWriteItems $
--             newTransactWriteItems
--
--         , requestListTables $
--             newListTables
--
--         , requestScan $
--             newScan
--
--         , requestUpdateContributorInsights $
--             newUpdateContributorInsights
--
--         , requestExecuteStatement $
--             newExecuteStatement
--
--         , requestQuery $
--             newQuery
--
--         , requestCreateTable $
--             newCreateTable
--
--         , requestDescribeKinesisStreamingDestination $
--             newDescribeKinesisStreamingDestination
--
--         , requestDescribeEndpoints $
--             newDescribeEndpoints
--
--         , requestDescribeTimeToLive $
--             newDescribeTimeToLive
--
--         , requestDescribeContinuousBackups $
--             newDescribeContinuousBackups
--
--         , requestListExports $
--             newListExports
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDescribeContributorInsights $
--             newDescribeContributorInsights
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestRestoreTableToPointInTime $
--             newRestoreTableToPointInTime
--
--         , requestRestoreTableFromBackup $
--             newRestoreTableFromBackup
--
--         , requestUpdateTimeToLive $
--             newUpdateTimeToLive
--
--         , requestCreateGlobalTable $
--             newCreateGlobalTable
--
--         , requestUpdateContinuousBackups $
--             newUpdateContinuousBackups
--
--         , requestDescribeExport $
--             newDescribeExport
--
--           ]

--     , testGroup "response"
--         [ responsePutItem $
--             newPutItemResponse
--
--         , responseDeleteItem $
--             newDeleteItemResponse
--
--         , responseUpdateItem $
--             newUpdateItemResponse
--
--         , responseDisableKinesisStreamingDestination $
--             newKinesisStreamingDestinationOutput
--
--         , responseListGlobalTables $
--             newListGlobalTablesResponse
--
--         , responseUpdateGlobalTable $
--             newUpdateGlobalTableResponse
--
--         , responseDeleteTable $
--             newDeleteTableResponse
--
--         , responseUpdateTable $
--             newUpdateTableResponse
--
--         , responseBatchGetItem $
--             newBatchGetItemResponse
--
--         , responseListBackups $
--             newListBackupsResponse
--
--         , responseDeleteBackup $
--             newDeleteBackupResponse
--
--         , responseCreateBackup $
--             newCreateBackupResponse
--
--         , responseUpdateTableReplicaAutoScaling $
--             newUpdateTableReplicaAutoScalingResponse
--
--         , responseDescribeGlobalTableSettings $
--             newDescribeGlobalTableSettingsResponse
--
--         , responseListTagsOfResource $
--             newListTagsOfResourceResponse
--
--         , responseDescribeGlobalTable $
--             newDescribeGlobalTableResponse
--
--         , responseDescribeTable $
--             newDescribeTableResponse
--
--         , responseDescribeLimits $
--             newDescribeLimitsResponse
--
--         , responseExecuteTransaction $
--             newExecuteTransactionResponse
--
--         , responseGetItem $
--             newGetItemResponse
--
--         , responseDescribeBackup $
--             newDescribeBackupResponse
--
--         , responseBatchExecuteStatement $
--             newBatchExecuteStatementResponse
--
--         , responseDescribeTableReplicaAutoScaling $
--             newDescribeTableReplicaAutoScalingResponse
--
--         , responseUpdateGlobalTableSettings $
--             newUpdateGlobalTableSettingsResponse
--
--         , responseEnableKinesisStreamingDestination $
--             newKinesisStreamingDestinationOutput
--
--         , responseTransactGetItems $
--             newTransactGetItemsResponse
--
--         , responseListContributorInsights $
--             newListContributorInsightsResponse
--
--         , responseBatchWriteItem $
--             newBatchWriteItemResponse
--
--         , responseExportTableToPointInTime $
--             newExportTableToPointInTimeResponse
--
--         , responseTransactWriteItems $
--             newTransactWriteItemsResponse
--
--         , responseListTables $
--             newListTablesResponse
--
--         , responseScan $
--             newScanResponse
--
--         , responseUpdateContributorInsights $
--             newUpdateContributorInsightsResponse
--
--         , responseExecuteStatement $
--             newExecuteStatementResponse
--
--         , responseQuery $
--             newQueryResponse
--
--         , responseCreateTable $
--             newCreateTableResponse
--
--         , responseDescribeKinesisStreamingDestination $
--             newDescribeKinesisStreamingDestinationResponse
--
--         , responseDescribeEndpoints $
--             newDescribeEndpointsResponse
--
--         , responseDescribeTimeToLive $
--             newDescribeTimeToLiveResponse
--
--         , responseDescribeContinuousBackups $
--             newDescribeContinuousBackupsResponse
--
--         , responseListExports $
--             newListExportsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDescribeContributorInsights $
--             newDescribeContributorInsightsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseRestoreTableToPointInTime $
--             newRestoreTableToPointInTimeResponse
--
--         , responseRestoreTableFromBackup $
--             newRestoreTableFromBackupResponse
--
--         , responseUpdateTimeToLive $
--             newUpdateTimeToLiveResponse
--
--         , responseCreateGlobalTable $
--             newCreateGlobalTableResponse
--
--         , responseUpdateContinuousBackups $
--             newUpdateContinuousBackupsResponse
--
--         , responseDescribeExport $
--             newDescribeExportResponse
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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutItem)

responseDeleteItem :: DeleteItemResponse -> TestTree
responseDeleteItem =
  res
    "DeleteItemResponse"
    "fixture/DeleteItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteItem)

responseUpdateItem :: UpdateItemResponse -> TestTree
responseUpdateItem =
  res
    "UpdateItemResponse"
    "fixture/UpdateItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateItem)

responseDisableKinesisStreamingDestination :: KinesisStreamingDestinationOutput -> TestTree
responseDisableKinesisStreamingDestination =
  res
    "DisableKinesisStreamingDestinationResponse"
    "fixture/DisableKinesisStreamingDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableKinesisStreamingDestination)

responseListGlobalTables :: ListGlobalTablesResponse -> TestTree
responseListGlobalTables =
  res
    "ListGlobalTablesResponse"
    "fixture/ListGlobalTablesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGlobalTables)

responseUpdateGlobalTable :: UpdateGlobalTableResponse -> TestTree
responseUpdateGlobalTable =
  res
    "UpdateGlobalTableResponse"
    "fixture/UpdateGlobalTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGlobalTable)

responseDeleteTable :: DeleteTableResponse -> TestTree
responseDeleteTable =
  res
    "DeleteTableResponse"
    "fixture/DeleteTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTable)

responseUpdateTable :: UpdateTableResponse -> TestTree
responseUpdateTable =
  res
    "UpdateTableResponse"
    "fixture/UpdateTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTable)

responseBatchGetItem :: BatchGetItemResponse -> TestTree
responseBatchGetItem =
  res
    "BatchGetItemResponse"
    "fixture/BatchGetItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetItem)

responseListBackups :: ListBackupsResponse -> TestTree
responseListBackups =
  res
    "ListBackupsResponse"
    "fixture/ListBackupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBackups)

responseDeleteBackup :: DeleteBackupResponse -> TestTree
responseDeleteBackup =
  res
    "DeleteBackupResponse"
    "fixture/DeleteBackupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBackup)

responseCreateBackup :: CreateBackupResponse -> TestTree
responseCreateBackup =
  res
    "CreateBackupResponse"
    "fixture/CreateBackupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBackup)

responseUpdateTableReplicaAutoScaling :: UpdateTableReplicaAutoScalingResponse -> TestTree
responseUpdateTableReplicaAutoScaling =
  res
    "UpdateTableReplicaAutoScalingResponse"
    "fixture/UpdateTableReplicaAutoScalingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTableReplicaAutoScaling)

responseDescribeGlobalTableSettings :: DescribeGlobalTableSettingsResponse -> TestTree
responseDescribeGlobalTableSettings =
  res
    "DescribeGlobalTableSettingsResponse"
    "fixture/DescribeGlobalTableSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGlobalTableSettings)

responseListTagsOfResource :: ListTagsOfResourceResponse -> TestTree
responseListTagsOfResource =
  res
    "ListTagsOfResourceResponse"
    "fixture/ListTagsOfResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsOfResource)

responseDescribeGlobalTable :: DescribeGlobalTableResponse -> TestTree
responseDescribeGlobalTable =
  res
    "DescribeGlobalTableResponse"
    "fixture/DescribeGlobalTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGlobalTable)

responseDescribeTable :: DescribeTableResponse -> TestTree
responseDescribeTable =
  res
    "DescribeTableResponse"
    "fixture/DescribeTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTable)

responseDescribeLimits :: DescribeLimitsResponse -> TestTree
responseDescribeLimits =
  res
    "DescribeLimitsResponse"
    "fixture/DescribeLimitsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLimits)

responseExecuteTransaction :: ExecuteTransactionResponse -> TestTree
responseExecuteTransaction =
  res
    "ExecuteTransactionResponse"
    "fixture/ExecuteTransactionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExecuteTransaction)

responseGetItem :: GetItemResponse -> TestTree
responseGetItem =
  res
    "GetItemResponse"
    "fixture/GetItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetItem)

responseDescribeBackup :: DescribeBackupResponse -> TestTree
responseDescribeBackup =
  res
    "DescribeBackupResponse"
    "fixture/DescribeBackupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBackup)

responseBatchExecuteStatement :: BatchExecuteStatementResponse -> TestTree
responseBatchExecuteStatement =
  res
    "BatchExecuteStatementResponse"
    "fixture/BatchExecuteStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchExecuteStatement)

responseDescribeTableReplicaAutoScaling :: DescribeTableReplicaAutoScalingResponse -> TestTree
responseDescribeTableReplicaAutoScaling =
  res
    "DescribeTableReplicaAutoScalingResponse"
    "fixture/DescribeTableReplicaAutoScalingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTableReplicaAutoScaling)

responseUpdateGlobalTableSettings :: UpdateGlobalTableSettingsResponse -> TestTree
responseUpdateGlobalTableSettings =
  res
    "UpdateGlobalTableSettingsResponse"
    "fixture/UpdateGlobalTableSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGlobalTableSettings)

responseEnableKinesisStreamingDestination :: KinesisStreamingDestinationOutput -> TestTree
responseEnableKinesisStreamingDestination =
  res
    "EnableKinesisStreamingDestinationResponse"
    "fixture/EnableKinesisStreamingDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableKinesisStreamingDestination)

responseTransactGetItems :: TransactGetItemsResponse -> TestTree
responseTransactGetItems =
  res
    "TransactGetItemsResponse"
    "fixture/TransactGetItemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TransactGetItems)

responseListContributorInsights :: ListContributorInsightsResponse -> TestTree
responseListContributorInsights =
  res
    "ListContributorInsightsResponse"
    "fixture/ListContributorInsightsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListContributorInsights)

responseBatchWriteItem :: BatchWriteItemResponse -> TestTree
responseBatchWriteItem =
  res
    "BatchWriteItemResponse"
    "fixture/BatchWriteItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchWriteItem)

responseExportTableToPointInTime :: ExportTableToPointInTimeResponse -> TestTree
responseExportTableToPointInTime =
  res
    "ExportTableToPointInTimeResponse"
    "fixture/ExportTableToPointInTimeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportTableToPointInTime)

responseTransactWriteItems :: TransactWriteItemsResponse -> TestTree
responseTransactWriteItems =
  res
    "TransactWriteItemsResponse"
    "fixture/TransactWriteItemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TransactWriteItems)

responseListTables :: ListTablesResponse -> TestTree
responseListTables =
  res
    "ListTablesResponse"
    "fixture/ListTablesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTables)

responseScan :: ScanResponse -> TestTree
responseScan =
  res
    "ScanResponse"
    "fixture/ScanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Scan)

responseUpdateContributorInsights :: UpdateContributorInsightsResponse -> TestTree
responseUpdateContributorInsights =
  res
    "UpdateContributorInsightsResponse"
    "fixture/UpdateContributorInsightsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContributorInsights)

responseExecuteStatement :: ExecuteStatementResponse -> TestTree
responseExecuteStatement =
  res
    "ExecuteStatementResponse"
    "fixture/ExecuteStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExecuteStatement)

responseQuery :: QueryResponse -> TestTree
responseQuery =
  res
    "QueryResponse"
    "fixture/QueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Query)

responseCreateTable :: CreateTableResponse -> TestTree
responseCreateTable =
  res
    "CreateTableResponse"
    "fixture/CreateTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTable)

responseDescribeKinesisStreamingDestination :: DescribeKinesisStreamingDestinationResponse -> TestTree
responseDescribeKinesisStreamingDestination =
  res
    "DescribeKinesisStreamingDestinationResponse"
    "fixture/DescribeKinesisStreamingDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeKinesisStreamingDestination)

responseDescribeEndpoints :: DescribeEndpointsResponse -> TestTree
responseDescribeEndpoints =
  res
    "DescribeEndpointsResponse"
    "fixture/DescribeEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEndpoints)

responseDescribeTimeToLive :: DescribeTimeToLiveResponse -> TestTree
responseDescribeTimeToLive =
  res
    "DescribeTimeToLiveResponse"
    "fixture/DescribeTimeToLiveResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTimeToLive)

responseDescribeContinuousBackups :: DescribeContinuousBackupsResponse -> TestTree
responseDescribeContinuousBackups =
  res
    "DescribeContinuousBackupsResponse"
    "fixture/DescribeContinuousBackupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeContinuousBackups)

responseListExports :: ListExportsResponse -> TestTree
responseListExports =
  res
    "ListExportsResponse"
    "fixture/ListExportsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListExports)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseDescribeContributorInsights :: DescribeContributorInsightsResponse -> TestTree
responseDescribeContributorInsights =
  res
    "DescribeContributorInsightsResponse"
    "fixture/DescribeContributorInsightsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeContributorInsights)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseRestoreTableToPointInTime :: RestoreTableToPointInTimeResponse -> TestTree
responseRestoreTableToPointInTime =
  res
    "RestoreTableToPointInTimeResponse"
    "fixture/RestoreTableToPointInTimeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreTableToPointInTime)

responseRestoreTableFromBackup :: RestoreTableFromBackupResponse -> TestTree
responseRestoreTableFromBackup =
  res
    "RestoreTableFromBackupResponse"
    "fixture/RestoreTableFromBackupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreTableFromBackup)

responseUpdateTimeToLive :: UpdateTimeToLiveResponse -> TestTree
responseUpdateTimeToLive =
  res
    "UpdateTimeToLiveResponse"
    "fixture/UpdateTimeToLiveResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTimeToLive)

responseCreateGlobalTable :: CreateGlobalTableResponse -> TestTree
responseCreateGlobalTable =
  res
    "CreateGlobalTableResponse"
    "fixture/CreateGlobalTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGlobalTable)

responseUpdateContinuousBackups :: UpdateContinuousBackupsResponse -> TestTree
responseUpdateContinuousBackups =
  res
    "UpdateContinuousBackupsResponse"
    "fixture/UpdateContinuousBackupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContinuousBackups)

responseDescribeExport :: DescribeExportResponse -> TestTree
responseDescribeExport =
  res
    "DescribeExportResponse"
    "fixture/DescribeExportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeExport)
