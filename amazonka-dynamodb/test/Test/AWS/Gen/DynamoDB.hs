{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DynamoDB
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--         , requestGetItem $
--             getItem
--
--         , requestDescribeBackup $
--             describeBackup
--
--         , requestUpdateGlobalTableSettings $
--             updateGlobalTableSettings
--
--         , requestBatchWriteItem $
--             batchWriteItem
--
--         , requestListTables $
--             listTables
--
--         , requestScan $
--             scan
--
--         , requestQuery $
--             query
--
--         , requestCreateTable $
--             createTable
--
--         , requestDescribeTimeToLive $
--             describeTimeToLive
--
--         , requestDescribeContinuousBackups $
--             describeContinuousBackups
--
--         , requestTagResource $
--             tagResource
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
--         , responseGetItem $
--             getItemResponse
--
--         , responseDescribeBackup $
--             describeBackupResponse
--
--         , responseUpdateGlobalTableSettings $
--             updateGlobalTableSettingsResponse
--
--         , responseBatchWriteItem $
--             batchWriteItemResponse
--
--         , responseListTables $
--             listTablesResponse
--
--         , responseScan $
--             scanResponse
--
--         , responseQuery $
--             queryResponse
--
--         , responseCreateTable $
--             createTableResponse
--
--         , responseDescribeTimeToLive $
--             describeTimeToLiveResponse
--
--         , responseDescribeContinuousBackups $
--             describeContinuousBackupsResponse
--
--         , responseTagResource $
--             tagResourceResponse
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

requestGetItem :: GetItem -> TestTree
requestGetItem = req
    "GetItem"
    "fixture/GetItem.yaml"

requestDescribeBackup :: DescribeBackup -> TestTree
requestDescribeBackup = req
    "DescribeBackup"
    "fixture/DescribeBackup.yaml"

requestUpdateGlobalTableSettings :: UpdateGlobalTableSettings -> TestTree
requestUpdateGlobalTableSettings = req
    "UpdateGlobalTableSettings"
    "fixture/UpdateGlobalTableSettings.yaml"

requestBatchWriteItem :: BatchWriteItem -> TestTree
requestBatchWriteItem = req
    "BatchWriteItem"
    "fixture/BatchWriteItem.yaml"

requestListTables :: ListTables -> TestTree
requestListTables = req
    "ListTables"
    "fixture/ListTables.yaml"

requestScan :: Scan -> TestTree
requestScan = req
    "Scan"
    "fixture/Scan.yaml"

requestQuery :: Query -> TestTree
requestQuery = req
    "Query"
    "fixture/Query.yaml"

requestCreateTable :: CreateTable -> TestTree
requestCreateTable = req
    "CreateTable"
    "fixture/CreateTable.yaml"

requestDescribeTimeToLive :: DescribeTimeToLive -> TestTree
requestDescribeTimeToLive = req
    "DescribeTimeToLive"
    "fixture/DescribeTimeToLive.yaml"

requestDescribeContinuousBackups :: DescribeContinuousBackups -> TestTree
requestDescribeContinuousBackups = req
    "DescribeContinuousBackups"
    "fixture/DescribeContinuousBackups.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

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

-- Responses

responsePutItem :: PutItemResponse -> TestTree
responsePutItem = res
    "PutItemResponse"
    "fixture/PutItemResponse.proto"
    dynamoDB
    (Proxy :: Proxy PutItem)

responseDeleteItem :: DeleteItemResponse -> TestTree
responseDeleteItem = res
    "DeleteItemResponse"
    "fixture/DeleteItemResponse.proto"
    dynamoDB
    (Proxy :: Proxy DeleteItem)

responseUpdateItem :: UpdateItemResponse -> TestTree
responseUpdateItem = res
    "UpdateItemResponse"
    "fixture/UpdateItemResponse.proto"
    dynamoDB
    (Proxy :: Proxy UpdateItem)

responseListGlobalTables :: ListGlobalTablesResponse -> TestTree
responseListGlobalTables = res
    "ListGlobalTablesResponse"
    "fixture/ListGlobalTablesResponse.proto"
    dynamoDB
    (Proxy :: Proxy ListGlobalTables)

responseUpdateGlobalTable :: UpdateGlobalTableResponse -> TestTree
responseUpdateGlobalTable = res
    "UpdateGlobalTableResponse"
    "fixture/UpdateGlobalTableResponse.proto"
    dynamoDB
    (Proxy :: Proxy UpdateGlobalTable)

responseDeleteTable :: DeleteTableResponse -> TestTree
responseDeleteTable = res
    "DeleteTableResponse"
    "fixture/DeleteTableResponse.proto"
    dynamoDB
    (Proxy :: Proxy DeleteTable)

responseUpdateTable :: UpdateTableResponse -> TestTree
responseUpdateTable = res
    "UpdateTableResponse"
    "fixture/UpdateTableResponse.proto"
    dynamoDB
    (Proxy :: Proxy UpdateTable)

responseBatchGetItem :: BatchGetItemResponse -> TestTree
responseBatchGetItem = res
    "BatchGetItemResponse"
    "fixture/BatchGetItemResponse.proto"
    dynamoDB
    (Proxy :: Proxy BatchGetItem)

responseListBackups :: ListBackupsResponse -> TestTree
responseListBackups = res
    "ListBackupsResponse"
    "fixture/ListBackupsResponse.proto"
    dynamoDB
    (Proxy :: Proxy ListBackups)

responseDeleteBackup :: DeleteBackupResponse -> TestTree
responseDeleteBackup = res
    "DeleteBackupResponse"
    "fixture/DeleteBackupResponse.proto"
    dynamoDB
    (Proxy :: Proxy DeleteBackup)

responseCreateBackup :: CreateBackupResponse -> TestTree
responseCreateBackup = res
    "CreateBackupResponse"
    "fixture/CreateBackupResponse.proto"
    dynamoDB
    (Proxy :: Proxy CreateBackup)

responseDescribeGlobalTableSettings :: DescribeGlobalTableSettingsResponse -> TestTree
responseDescribeGlobalTableSettings = res
    "DescribeGlobalTableSettingsResponse"
    "fixture/DescribeGlobalTableSettingsResponse.proto"
    dynamoDB
    (Proxy :: Proxy DescribeGlobalTableSettings)

responseListTagsOfResource :: ListTagsOfResourceResponse -> TestTree
responseListTagsOfResource = res
    "ListTagsOfResourceResponse"
    "fixture/ListTagsOfResourceResponse.proto"
    dynamoDB
    (Proxy :: Proxy ListTagsOfResource)

responseDescribeGlobalTable :: DescribeGlobalTableResponse -> TestTree
responseDescribeGlobalTable = res
    "DescribeGlobalTableResponse"
    "fixture/DescribeGlobalTableResponse.proto"
    dynamoDB
    (Proxy :: Proxy DescribeGlobalTable)

responseDescribeTable :: DescribeTableResponse -> TestTree
responseDescribeTable = res
    "DescribeTableResponse"
    "fixture/DescribeTableResponse.proto"
    dynamoDB
    (Proxy :: Proxy DescribeTable)

responseDescribeLimits :: DescribeLimitsResponse -> TestTree
responseDescribeLimits = res
    "DescribeLimitsResponse"
    "fixture/DescribeLimitsResponse.proto"
    dynamoDB
    (Proxy :: Proxy DescribeLimits)

responseGetItem :: GetItemResponse -> TestTree
responseGetItem = res
    "GetItemResponse"
    "fixture/GetItemResponse.proto"
    dynamoDB
    (Proxy :: Proxy GetItem)

responseDescribeBackup :: DescribeBackupResponse -> TestTree
responseDescribeBackup = res
    "DescribeBackupResponse"
    "fixture/DescribeBackupResponse.proto"
    dynamoDB
    (Proxy :: Proxy DescribeBackup)

responseUpdateGlobalTableSettings :: UpdateGlobalTableSettingsResponse -> TestTree
responseUpdateGlobalTableSettings = res
    "UpdateGlobalTableSettingsResponse"
    "fixture/UpdateGlobalTableSettingsResponse.proto"
    dynamoDB
    (Proxy :: Proxy UpdateGlobalTableSettings)

responseBatchWriteItem :: BatchWriteItemResponse -> TestTree
responseBatchWriteItem = res
    "BatchWriteItemResponse"
    "fixture/BatchWriteItemResponse.proto"
    dynamoDB
    (Proxy :: Proxy BatchWriteItem)

responseListTables :: ListTablesResponse -> TestTree
responseListTables = res
    "ListTablesResponse"
    "fixture/ListTablesResponse.proto"
    dynamoDB
    (Proxy :: Proxy ListTables)

responseScan :: ScanResponse -> TestTree
responseScan = res
    "ScanResponse"
    "fixture/ScanResponse.proto"
    dynamoDB
    (Proxy :: Proxy Scan)

responseQuery :: QueryResponse -> TestTree
responseQuery = res
    "QueryResponse"
    "fixture/QueryResponse.proto"
    dynamoDB
    (Proxy :: Proxy Query)

responseCreateTable :: CreateTableResponse -> TestTree
responseCreateTable = res
    "CreateTableResponse"
    "fixture/CreateTableResponse.proto"
    dynamoDB
    (Proxy :: Proxy CreateTable)

responseDescribeTimeToLive :: DescribeTimeToLiveResponse -> TestTree
responseDescribeTimeToLive = res
    "DescribeTimeToLiveResponse"
    "fixture/DescribeTimeToLiveResponse.proto"
    dynamoDB
    (Proxy :: Proxy DescribeTimeToLive)

responseDescribeContinuousBackups :: DescribeContinuousBackupsResponse -> TestTree
responseDescribeContinuousBackups = res
    "DescribeContinuousBackupsResponse"
    "fixture/DescribeContinuousBackupsResponse.proto"
    dynamoDB
    (Proxy :: Proxy DescribeContinuousBackups)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    dynamoDB
    (Proxy :: Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    dynamoDB
    (Proxy :: Proxy UntagResource)

responseRestoreTableToPointInTime :: RestoreTableToPointInTimeResponse -> TestTree
responseRestoreTableToPointInTime = res
    "RestoreTableToPointInTimeResponse"
    "fixture/RestoreTableToPointInTimeResponse.proto"
    dynamoDB
    (Proxy :: Proxy RestoreTableToPointInTime)

responseRestoreTableFromBackup :: RestoreTableFromBackupResponse -> TestTree
responseRestoreTableFromBackup = res
    "RestoreTableFromBackupResponse"
    "fixture/RestoreTableFromBackupResponse.proto"
    dynamoDB
    (Proxy :: Proxy RestoreTableFromBackup)

responseUpdateTimeToLive :: UpdateTimeToLiveResponse -> TestTree
responseUpdateTimeToLive = res
    "UpdateTimeToLiveResponse"
    "fixture/UpdateTimeToLiveResponse.proto"
    dynamoDB
    (Proxy :: Proxy UpdateTimeToLive)

responseCreateGlobalTable :: CreateGlobalTableResponse -> TestTree
responseCreateGlobalTable = res
    "CreateGlobalTableResponse"
    "fixture/CreateGlobalTableResponse.proto"
    dynamoDB
    (Proxy :: Proxy CreateGlobalTable)

responseUpdateContinuousBackups :: UpdateContinuousBackupsResponse -> TestTree
responseUpdateContinuousBackups = res
    "UpdateContinuousBackupsResponse"
    "fixture/UpdateContinuousBackupsResponse.proto"
    dynamoDB
    (Proxy :: Proxy UpdateContinuousBackups)
