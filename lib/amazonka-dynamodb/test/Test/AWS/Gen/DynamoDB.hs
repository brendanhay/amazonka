{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DynamoDB
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.DynamoDB where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.DynamoDB
import Test.AWS.DynamoDB.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestPutItem $
--             mkPutItem
--
--         , requestDeleteItem $
--             mkDeleteItem
--
--         , requestUpdateItem $
--             mkUpdateItem
--
--         , requestDisableKinesisStreamingDestination $
--             mkDisableKinesisStreamingDestination
--
--         , requestListGlobalTables $
--             mkListGlobalTables
--
--         , requestUpdateGlobalTable $
--             mkUpdateGlobalTable
--
--         , requestDeleteTable $
--             mkDeleteTable
--
--         , requestUpdateTable $
--             mkUpdateTable
--
--         , requestBatchGetItem $
--             mkBatchGetItem
--
--         , requestListBackups $
--             mkListBackups
--
--         , requestDeleteBackup $
--             mkDeleteBackup
--
--         , requestCreateBackup $
--             mkCreateBackup
--
--         , requestUpdateTableReplicaAutoScaling $
--             mkUpdateTableReplicaAutoScaling
--
--         , requestDescribeGlobalTableSettings $
--             mkDescribeGlobalTableSettings
--
--         , requestListTagsOfResource $
--             mkListTagsOfResource
--
--         , requestDescribeGlobalTable $
--             mkDescribeGlobalTable
--
--         , requestDescribeTable $
--             mkDescribeTable
--
--         , requestDescribeLimits $
--             mkDescribeLimits
--
--         , requestExecuteTransaction $
--             mkExecuteTransaction
--
--         , requestGetItem $
--             mkGetItem
--
--         , requestDescribeBackup $
--             mkDescribeBackup
--
--         , requestBatchExecuteStatement $
--             mkBatchExecuteStatement
--
--         , requestDescribeTableReplicaAutoScaling $
--             mkDescribeTableReplicaAutoScaling
--
--         , requestUpdateGlobalTableSettings $
--             mkUpdateGlobalTableSettings
--
--         , requestEnableKinesisStreamingDestination $
--             mkEnableKinesisStreamingDestination
--
--         , requestTransactGetItems $
--             mkTransactGetItems
--
--         , requestListContributorInsights $
--             mkListContributorInsights
--
--         , requestBatchWriteItem $
--             mkBatchWriteItem
--
--         , requestExportTableToPointInTime $
--             mkExportTableToPointInTime
--
--         , requestTransactWriteItems $
--             mkTransactWriteItems
--
--         , requestListTables $
--             mkListTables
--
--         , requestScan $
--             mkScan
--
--         , requestUpdateContributorInsights $
--             mkUpdateContributorInsights
--
--         , requestExecuteStatement $
--             mkExecuteStatement
--
--         , requestQuery $
--             mkQuery
--
--         , requestCreateTable $
--             mkCreateTable
--
--         , requestDescribeKinesisStreamingDestination $
--             mkDescribeKinesisStreamingDestination
--
--         , requestDescribeEndpoints $
--             mkDescribeEndpoints
--
--         , requestDescribeTimeToLive $
--             mkDescribeTimeToLive
--
--         , requestDescribeContinuousBackups $
--             mkDescribeContinuousBackups
--
--         , requestListExports $
--             mkListExports
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestDescribeContributorInsights $
--             mkDescribeContributorInsights
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestRestoreTableToPointInTime $
--             mkRestoreTableToPointInTime
--
--         , requestRestoreTableFromBackup $
--             mkRestoreTableFromBackup
--
--         , requestUpdateTimeToLive $
--             mkUpdateTimeToLive
--
--         , requestCreateGlobalTable $
--             mkCreateGlobalTable
--
--         , requestUpdateContinuousBackups $
--             mkUpdateContinuousBackups
--
--         , requestDescribeExport $
--             mkDescribeExport
--
--           ]

--     , testGroup "response"
--         [ responsePutItem $
--             mkPutItemResponse
--
--         , responseDeleteItem $
--             mkDeleteItemResponse
--
--         , responseUpdateItem $
--             mkUpdateItemResponse
--
--         , responseDisableKinesisStreamingDestination $
--             mkKinesisStreamingDestinationOutput
--
--         , responseListGlobalTables $
--             mkListGlobalTablesResponse
--
--         , responseUpdateGlobalTable $
--             mkUpdateGlobalTableResponse
--
--         , responseDeleteTable $
--             mkDeleteTableResponse
--
--         , responseUpdateTable $
--             mkUpdateTableResponse
--
--         , responseBatchGetItem $
--             mkBatchGetItemResponse
--
--         , responseListBackups $
--             mkListBackupsResponse
--
--         , responseDeleteBackup $
--             mkDeleteBackupResponse
--
--         , responseCreateBackup $
--             mkCreateBackupResponse
--
--         , responseUpdateTableReplicaAutoScaling $
--             mkUpdateTableReplicaAutoScalingResponse
--
--         , responseDescribeGlobalTableSettings $
--             mkDescribeGlobalTableSettingsResponse
--
--         , responseListTagsOfResource $
--             mkListTagsOfResourceResponse
--
--         , responseDescribeGlobalTable $
--             mkDescribeGlobalTableResponse
--
--         , responseDescribeTable $
--             mkDescribeTableResponse
--
--         , responseDescribeLimits $
--             mkDescribeLimitsResponse
--
--         , responseExecuteTransaction $
--             mkExecuteTransactionResponse
--
--         , responseGetItem $
--             mkGetItemResponse
--
--         , responseDescribeBackup $
--             mkDescribeBackupResponse
--
--         , responseBatchExecuteStatement $
--             mkBatchExecuteStatementResponse
--
--         , responseDescribeTableReplicaAutoScaling $
--             mkDescribeTableReplicaAutoScalingResponse
--
--         , responseUpdateGlobalTableSettings $
--             mkUpdateGlobalTableSettingsResponse
--
--         , responseEnableKinesisStreamingDestination $
--             mkKinesisStreamingDestinationOutput
--
--         , responseTransactGetItems $
--             mkTransactGetItemsResponse
--
--         , responseListContributorInsights $
--             mkListContributorInsightsResponse
--
--         , responseBatchWriteItem $
--             mkBatchWriteItemResponse
--
--         , responseExportTableToPointInTime $
--             mkExportTableToPointInTimeResponse
--
--         , responseTransactWriteItems $
--             mkTransactWriteItemsResponse
--
--         , responseListTables $
--             mkListTablesResponse
--
--         , responseScan $
--             mkScanResponse
--
--         , responseUpdateContributorInsights $
--             mkUpdateContributorInsightsResponse
--
--         , responseExecuteStatement $
--             mkExecuteStatementResponse
--
--         , responseQuery $
--             mkQueryResponse
--
--         , responseCreateTable $
--             mkCreateTableResponse
--
--         , responseDescribeKinesisStreamingDestination $
--             mkDescribeKinesisStreamingDestinationResponse
--
--         , responseDescribeEndpoints $
--             mkDescribeEndpointsResponse
--
--         , responseDescribeTimeToLive $
--             mkDescribeTimeToLiveResponse
--
--         , responseDescribeContinuousBackups $
--             mkDescribeContinuousBackupsResponse
--
--         , responseListExports $
--             mkListExportsResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseDescribeContributorInsights $
--             mkDescribeContributorInsightsResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseRestoreTableToPointInTime $
--             mkRestoreTableToPointInTimeResponse
--
--         , responseRestoreTableFromBackup $
--             mkRestoreTableFromBackupResponse
--
--         , responseUpdateTimeToLive $
--             mkUpdateTimeToLiveResponse
--
--         , responseCreateGlobalTable $
--             mkCreateGlobalTableResponse
--
--         , responseUpdateContinuousBackups $
--             mkUpdateContinuousBackupsResponse
--
--         , responseDescribeExport $
--             mkDescribeExportResponse
--
--           ]
--     ]

-- Requests

requestPutItem :: PutItem -> TestTree
requestPutItem = req
    "PutItem"
    "fixture/PutItem.yaml"

requestDeleteItem :: DeleteItem -> TestTree
requestDeleteItem = req
    "DeleteItem"
    "fixture/DeleteItem.yaml"

requestUpdateItem :: UpdateItem -> TestTree
requestUpdateItem = req
    "UpdateItem"
    "fixture/UpdateItem.yaml"

requestDisableKinesisStreamingDestination :: DisableKinesisStreamingDestination -> TestTree
requestDisableKinesisStreamingDestination = req
    "DisableKinesisStreamingDestination"
    "fixture/DisableKinesisStreamingDestination.yaml"

requestListGlobalTables :: ListGlobalTables -> TestTree
requestListGlobalTables = req
    "ListGlobalTables"
    "fixture/ListGlobalTables.yaml"

requestUpdateGlobalTable :: UpdateGlobalTable -> TestTree
requestUpdateGlobalTable = req
    "UpdateGlobalTable"
    "fixture/UpdateGlobalTable.yaml"

requestDeleteTable :: DeleteTable -> TestTree
requestDeleteTable = req
    "DeleteTable"
    "fixture/DeleteTable.yaml"

requestUpdateTable :: UpdateTable -> TestTree
requestUpdateTable = req
    "UpdateTable"
    "fixture/UpdateTable.yaml"

requestBatchGetItem :: BatchGetItem -> TestTree
requestBatchGetItem = req
    "BatchGetItem"
    "fixture/BatchGetItem.yaml"

requestListBackups :: ListBackups -> TestTree
requestListBackups = req
    "ListBackups"
    "fixture/ListBackups.yaml"

requestDeleteBackup :: DeleteBackup -> TestTree
requestDeleteBackup = req
    "DeleteBackup"
    "fixture/DeleteBackup.yaml"

requestCreateBackup :: CreateBackup -> TestTree
requestCreateBackup = req
    "CreateBackup"
    "fixture/CreateBackup.yaml"

requestUpdateTableReplicaAutoScaling :: UpdateTableReplicaAutoScaling -> TestTree
requestUpdateTableReplicaAutoScaling = req
    "UpdateTableReplicaAutoScaling"
    "fixture/UpdateTableReplicaAutoScaling.yaml"

requestDescribeGlobalTableSettings :: DescribeGlobalTableSettings -> TestTree
requestDescribeGlobalTableSettings = req
    "DescribeGlobalTableSettings"
    "fixture/DescribeGlobalTableSettings.yaml"

requestListTagsOfResource :: ListTagsOfResource -> TestTree
requestListTagsOfResource = req
    "ListTagsOfResource"
    "fixture/ListTagsOfResource.yaml"

requestDescribeGlobalTable :: DescribeGlobalTable -> TestTree
requestDescribeGlobalTable = req
    "DescribeGlobalTable"
    "fixture/DescribeGlobalTable.yaml"

requestDescribeTable :: DescribeTable -> TestTree
requestDescribeTable = req
    "DescribeTable"
    "fixture/DescribeTable.yaml"

requestDescribeLimits :: DescribeLimits -> TestTree
requestDescribeLimits = req
    "DescribeLimits"
    "fixture/DescribeLimits.yaml"

requestExecuteTransaction :: ExecuteTransaction -> TestTree
requestExecuteTransaction = req
    "ExecuteTransaction"
    "fixture/ExecuteTransaction.yaml"

requestGetItem :: GetItem -> TestTree
requestGetItem = req
    "GetItem"
    "fixture/GetItem.yaml"

requestDescribeBackup :: DescribeBackup -> TestTree
requestDescribeBackup = req
    "DescribeBackup"
    "fixture/DescribeBackup.yaml"

requestBatchExecuteStatement :: BatchExecuteStatement -> TestTree
requestBatchExecuteStatement = req
    "BatchExecuteStatement"
    "fixture/BatchExecuteStatement.yaml"

requestDescribeTableReplicaAutoScaling :: DescribeTableReplicaAutoScaling -> TestTree
requestDescribeTableReplicaAutoScaling = req
    "DescribeTableReplicaAutoScaling"
    "fixture/DescribeTableReplicaAutoScaling.yaml"

requestUpdateGlobalTableSettings :: UpdateGlobalTableSettings -> TestTree
requestUpdateGlobalTableSettings = req
    "UpdateGlobalTableSettings"
    "fixture/UpdateGlobalTableSettings.yaml"

requestEnableKinesisStreamingDestination :: EnableKinesisStreamingDestination -> TestTree
requestEnableKinesisStreamingDestination = req
    "EnableKinesisStreamingDestination"
    "fixture/EnableKinesisStreamingDestination.yaml"

requestTransactGetItems :: TransactGetItems -> TestTree
requestTransactGetItems = req
    "TransactGetItems"
    "fixture/TransactGetItems.yaml"

requestListContributorInsights :: ListContributorInsights -> TestTree
requestListContributorInsights = req
    "ListContributorInsights"
    "fixture/ListContributorInsights.yaml"

requestBatchWriteItem :: BatchWriteItem -> TestTree
requestBatchWriteItem = req
    "BatchWriteItem"
    "fixture/BatchWriteItem.yaml"

requestExportTableToPointInTime :: ExportTableToPointInTime -> TestTree
requestExportTableToPointInTime = req
    "ExportTableToPointInTime"
    "fixture/ExportTableToPointInTime.yaml"

requestTransactWriteItems :: TransactWriteItems -> TestTree
requestTransactWriteItems = req
    "TransactWriteItems"
    "fixture/TransactWriteItems.yaml"

requestListTables :: ListTables -> TestTree
requestListTables = req
    "ListTables"
    "fixture/ListTables.yaml"

requestScan :: Scan -> TestTree
requestScan = req
    "Scan"
    "fixture/Scan.yaml"

requestUpdateContributorInsights :: UpdateContributorInsights -> TestTree
requestUpdateContributorInsights = req
    "UpdateContributorInsights"
    "fixture/UpdateContributorInsights.yaml"

requestExecuteStatement :: ExecuteStatement -> TestTree
requestExecuteStatement = req
    "ExecuteStatement"
    "fixture/ExecuteStatement.yaml"

requestQuery :: Query -> TestTree
requestQuery = req
    "Query"
    "fixture/Query.yaml"

requestCreateTable :: CreateTable -> TestTree
requestCreateTable = req
    "CreateTable"
    "fixture/CreateTable.yaml"

requestDescribeKinesisStreamingDestination :: DescribeKinesisStreamingDestination -> TestTree
requestDescribeKinesisStreamingDestination = req
    "DescribeKinesisStreamingDestination"
    "fixture/DescribeKinesisStreamingDestination.yaml"

requestDescribeEndpoints :: DescribeEndpoints -> TestTree
requestDescribeEndpoints = req
    "DescribeEndpoints"
    "fixture/DescribeEndpoints.yaml"

requestDescribeTimeToLive :: DescribeTimeToLive -> TestTree
requestDescribeTimeToLive = req
    "DescribeTimeToLive"
    "fixture/DescribeTimeToLive.yaml"

requestDescribeContinuousBackups :: DescribeContinuousBackups -> TestTree
requestDescribeContinuousBackups = req
    "DescribeContinuousBackups"
    "fixture/DescribeContinuousBackups.yaml"

requestListExports :: ListExports -> TestTree
requestListExports = req
    "ListExports"
    "fixture/ListExports.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestDescribeContributorInsights :: DescribeContributorInsights -> TestTree
requestDescribeContributorInsights = req
    "DescribeContributorInsights"
    "fixture/DescribeContributorInsights.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestRestoreTableToPointInTime :: RestoreTableToPointInTime -> TestTree
requestRestoreTableToPointInTime = req
    "RestoreTableToPointInTime"
    "fixture/RestoreTableToPointInTime.yaml"

requestRestoreTableFromBackup :: RestoreTableFromBackup -> TestTree
requestRestoreTableFromBackup = req
    "RestoreTableFromBackup"
    "fixture/RestoreTableFromBackup.yaml"

requestUpdateTimeToLive :: UpdateTimeToLive -> TestTree
requestUpdateTimeToLive = req
    "UpdateTimeToLive"
    "fixture/UpdateTimeToLive.yaml"

requestCreateGlobalTable :: CreateGlobalTable -> TestTree
requestCreateGlobalTable = req
    "CreateGlobalTable"
    "fixture/CreateGlobalTable.yaml"

requestUpdateContinuousBackups :: UpdateContinuousBackups -> TestTree
requestUpdateContinuousBackups = req
    "UpdateContinuousBackups"
    "fixture/UpdateContinuousBackups.yaml"

requestDescribeExport :: DescribeExport -> TestTree
requestDescribeExport = req
    "DescribeExport"
    "fixture/DescribeExport.yaml"

-- Responses

responsePutItem :: PutItemResponse -> TestTree
responsePutItem = res
    "PutItemResponse"
    "fixture/PutItemResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutItem)

responseDeleteItem :: DeleteItemResponse -> TestTree
responseDeleteItem = res
    "DeleteItemResponse"
    "fixture/DeleteItemResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteItem)

responseUpdateItem :: UpdateItemResponse -> TestTree
responseUpdateItem = res
    "UpdateItemResponse"
    "fixture/UpdateItemResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateItem)

responseDisableKinesisStreamingDestination :: KinesisStreamingDestinationOutput -> TestTree
responseDisableKinesisStreamingDestination = res
    "DisableKinesisStreamingDestinationResponse"
    "fixture/DisableKinesisStreamingDestinationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisableKinesisStreamingDestination)

responseListGlobalTables :: ListGlobalTablesResponse -> TestTree
responseListGlobalTables = res
    "ListGlobalTablesResponse"
    "fixture/ListGlobalTablesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListGlobalTables)

responseUpdateGlobalTable :: UpdateGlobalTableResponse -> TestTree
responseUpdateGlobalTable = res
    "UpdateGlobalTableResponse"
    "fixture/UpdateGlobalTableResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateGlobalTable)

responseDeleteTable :: DeleteTableResponse -> TestTree
responseDeleteTable = res
    "DeleteTableResponse"
    "fixture/DeleteTableResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTable)

responseUpdateTable :: UpdateTableResponse -> TestTree
responseUpdateTable = res
    "UpdateTableResponse"
    "fixture/UpdateTableResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateTable)

responseBatchGetItem :: BatchGetItemResponse -> TestTree
responseBatchGetItem = res
    "BatchGetItemResponse"
    "fixture/BatchGetItemResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BatchGetItem)

responseListBackups :: ListBackupsResponse -> TestTree
responseListBackups = res
    "ListBackupsResponse"
    "fixture/ListBackupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListBackups)

responseDeleteBackup :: DeleteBackupResponse -> TestTree
responseDeleteBackup = res
    "DeleteBackupResponse"
    "fixture/DeleteBackupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBackup)

responseCreateBackup :: CreateBackupResponse -> TestTree
responseCreateBackup = res
    "CreateBackupResponse"
    "fixture/CreateBackupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateBackup)

responseUpdateTableReplicaAutoScaling :: UpdateTableReplicaAutoScalingResponse -> TestTree
responseUpdateTableReplicaAutoScaling = res
    "UpdateTableReplicaAutoScalingResponse"
    "fixture/UpdateTableReplicaAutoScalingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateTableReplicaAutoScaling)

responseDescribeGlobalTableSettings :: DescribeGlobalTableSettingsResponse -> TestTree
responseDescribeGlobalTableSettings = res
    "DescribeGlobalTableSettingsResponse"
    "fixture/DescribeGlobalTableSettingsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeGlobalTableSettings)

responseListTagsOfResource :: ListTagsOfResourceResponse -> TestTree
responseListTagsOfResource = res
    "ListTagsOfResourceResponse"
    "fixture/ListTagsOfResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsOfResource)

responseDescribeGlobalTable :: DescribeGlobalTableResponse -> TestTree
responseDescribeGlobalTable = res
    "DescribeGlobalTableResponse"
    "fixture/DescribeGlobalTableResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeGlobalTable)

responseDescribeTable :: DescribeTableResponse -> TestTree
responseDescribeTable = res
    "DescribeTableResponse"
    "fixture/DescribeTableResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTable)

responseDescribeLimits :: DescribeLimitsResponse -> TestTree
responseDescribeLimits = res
    "DescribeLimitsResponse"
    "fixture/DescribeLimitsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeLimits)

responseExecuteTransaction :: ExecuteTransactionResponse -> TestTree
responseExecuteTransaction = res
    "ExecuteTransactionResponse"
    "fixture/ExecuteTransactionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ExecuteTransaction)

responseGetItem :: GetItemResponse -> TestTree
responseGetItem = res
    "GetItemResponse"
    "fixture/GetItemResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetItem)

responseDescribeBackup :: DescribeBackupResponse -> TestTree
responseDescribeBackup = res
    "DescribeBackupResponse"
    "fixture/DescribeBackupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeBackup)

responseBatchExecuteStatement :: BatchExecuteStatementResponse -> TestTree
responseBatchExecuteStatement = res
    "BatchExecuteStatementResponse"
    "fixture/BatchExecuteStatementResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BatchExecuteStatement)

responseDescribeTableReplicaAutoScaling :: DescribeTableReplicaAutoScalingResponse -> TestTree
responseDescribeTableReplicaAutoScaling = res
    "DescribeTableReplicaAutoScalingResponse"
    "fixture/DescribeTableReplicaAutoScalingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTableReplicaAutoScaling)

responseUpdateGlobalTableSettings :: UpdateGlobalTableSettingsResponse -> TestTree
responseUpdateGlobalTableSettings = res
    "UpdateGlobalTableSettingsResponse"
    "fixture/UpdateGlobalTableSettingsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateGlobalTableSettings)

responseEnableKinesisStreamingDestination :: KinesisStreamingDestinationOutput -> TestTree
responseEnableKinesisStreamingDestination = res
    "EnableKinesisStreamingDestinationResponse"
    "fixture/EnableKinesisStreamingDestinationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy EnableKinesisStreamingDestination)

responseTransactGetItems :: TransactGetItemsResponse -> TestTree
responseTransactGetItems = res
    "TransactGetItemsResponse"
    "fixture/TransactGetItemsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TransactGetItems)

responseListContributorInsights :: ListContributorInsightsResponse -> TestTree
responseListContributorInsights = res
    "ListContributorInsightsResponse"
    "fixture/ListContributorInsightsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListContributorInsights)

responseBatchWriteItem :: BatchWriteItemResponse -> TestTree
responseBatchWriteItem = res
    "BatchWriteItemResponse"
    "fixture/BatchWriteItemResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BatchWriteItem)

responseExportTableToPointInTime :: ExportTableToPointInTimeResponse -> TestTree
responseExportTableToPointInTime = res
    "ExportTableToPointInTimeResponse"
    "fixture/ExportTableToPointInTimeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ExportTableToPointInTime)

responseTransactWriteItems :: TransactWriteItemsResponse -> TestTree
responseTransactWriteItems = res
    "TransactWriteItemsResponse"
    "fixture/TransactWriteItemsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TransactWriteItems)

responseListTables :: ListTablesResponse -> TestTree
responseListTables = res
    "ListTablesResponse"
    "fixture/ListTablesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTables)

responseScan :: ScanResponse -> TestTree
responseScan = res
    "ScanResponse"
    "fixture/ScanResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy Scan)

responseUpdateContributorInsights :: UpdateContributorInsightsResponse -> TestTree
responseUpdateContributorInsights = res
    "UpdateContributorInsightsResponse"
    "fixture/UpdateContributorInsightsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateContributorInsights)

responseExecuteStatement :: ExecuteStatementResponse -> TestTree
responseExecuteStatement = res
    "ExecuteStatementResponse"
    "fixture/ExecuteStatementResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ExecuteStatement)

responseQuery :: QueryResponse -> TestTree
responseQuery = res
    "QueryResponse"
    "fixture/QueryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy Query)

responseCreateTable :: CreateTableResponse -> TestTree
responseCreateTable = res
    "CreateTableResponse"
    "fixture/CreateTableResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTable)

responseDescribeKinesisStreamingDestination :: DescribeKinesisStreamingDestinationResponse -> TestTree
responseDescribeKinesisStreamingDestination = res
    "DescribeKinesisStreamingDestinationResponse"
    "fixture/DescribeKinesisStreamingDestinationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeKinesisStreamingDestination)

responseDescribeEndpoints :: DescribeEndpointsResponse -> TestTree
responseDescribeEndpoints = res
    "DescribeEndpointsResponse"
    "fixture/DescribeEndpointsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeEndpoints)

responseDescribeTimeToLive :: DescribeTimeToLiveResponse -> TestTree
responseDescribeTimeToLive = res
    "DescribeTimeToLiveResponse"
    "fixture/DescribeTimeToLiveResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTimeToLive)

responseDescribeContinuousBackups :: DescribeContinuousBackupsResponse -> TestTree
responseDescribeContinuousBackups = res
    "DescribeContinuousBackupsResponse"
    "fixture/DescribeContinuousBackupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeContinuousBackups)

responseListExports :: ListExportsResponse -> TestTree
responseListExports = res
    "ListExportsResponse"
    "fixture/ListExportsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListExports)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responseDescribeContributorInsights :: DescribeContributorInsightsResponse -> TestTree
responseDescribeContributorInsights = res
    "DescribeContributorInsightsResponse"
    "fixture/DescribeContributorInsightsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeContributorInsights)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseRestoreTableToPointInTime :: RestoreTableToPointInTimeResponse -> TestTree
responseRestoreTableToPointInTime = res
    "RestoreTableToPointInTimeResponse"
    "fixture/RestoreTableToPointInTimeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RestoreTableToPointInTime)

responseRestoreTableFromBackup :: RestoreTableFromBackupResponse -> TestTree
responseRestoreTableFromBackup = res
    "RestoreTableFromBackupResponse"
    "fixture/RestoreTableFromBackupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RestoreTableFromBackup)

responseUpdateTimeToLive :: UpdateTimeToLiveResponse -> TestTree
responseUpdateTimeToLive = res
    "UpdateTimeToLiveResponse"
    "fixture/UpdateTimeToLiveResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateTimeToLive)

responseCreateGlobalTable :: CreateGlobalTableResponse -> TestTree
responseCreateGlobalTable = res
    "CreateGlobalTableResponse"
    "fixture/CreateGlobalTableResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateGlobalTable)

responseUpdateContinuousBackups :: UpdateContinuousBackupsResponse -> TestTree
responseUpdateContinuousBackups = res
    "UpdateContinuousBackupsResponse"
    "fixture/UpdateContinuousBackupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateContinuousBackups)

responseDescribeExport :: DescribeExportResponse -> TestTree
responseDescribeExport = res
    "DescribeExportResponse"
    "fixture/DescribeExportResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeExport)
