{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.HoneyCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.HoneyCode where

import Amazonka.HoneyCode
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.HoneyCode.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchDeleteTableRows $
--             newBatchDeleteTableRows
--
--         , requestBatchUpdateTableRows $
--             newBatchUpdateTableRows
--
--         , requestListTableRows $
--             newListTableRows
--
--         , requestInvokeScreenAutomation $
--             newInvokeScreenAutomation
--
--         , requestDescribeTableDataImportJob $
--             newDescribeTableDataImportJob
--
--         , requestStartTableDataImportJob $
--             newStartTableDataImportJob
--
--         , requestBatchCreateTableRows $
--             newBatchCreateTableRows
--
--         , requestListTables $
--             newListTables
--
--         , requestGetScreenData $
--             newGetScreenData
--
--         , requestQueryTableRows $
--             newQueryTableRows
--
--         , requestBatchUpsertTableRows $
--             newBatchUpsertTableRows
--
--         , requestListTableColumns $
--             newListTableColumns
--
--           ]

--     , testGroup "response"
--         [ responseBatchDeleteTableRows $
--             newBatchDeleteTableRowsResponse
--
--         , responseBatchUpdateTableRows $
--             newBatchUpdateTableRowsResponse
--
--         , responseListTableRows $
--             newListTableRowsResponse
--
--         , responseInvokeScreenAutomation $
--             newInvokeScreenAutomationResponse
--
--         , responseDescribeTableDataImportJob $
--             newDescribeTableDataImportJobResponse
--
--         , responseStartTableDataImportJob $
--             newStartTableDataImportJobResponse
--
--         , responseBatchCreateTableRows $
--             newBatchCreateTableRowsResponse
--
--         , responseListTables $
--             newListTablesResponse
--
--         , responseGetScreenData $
--             newGetScreenDataResponse
--
--         , responseQueryTableRows $
--             newQueryTableRowsResponse
--
--         , responseBatchUpsertTableRows $
--             newBatchUpsertTableRowsResponse
--
--         , responseListTableColumns $
--             newListTableColumnsResponse
--
--           ]
--     ]

-- Requests

requestBatchDeleteTableRows :: BatchDeleteTableRows -> TestTree
requestBatchDeleteTableRows =
  req
    "BatchDeleteTableRows"
    "fixture/BatchDeleteTableRows.yaml"

requestBatchUpdateTableRows :: BatchUpdateTableRows -> TestTree
requestBatchUpdateTableRows =
  req
    "BatchUpdateTableRows"
    "fixture/BatchUpdateTableRows.yaml"

requestListTableRows :: ListTableRows -> TestTree
requestListTableRows =
  req
    "ListTableRows"
    "fixture/ListTableRows.yaml"

requestInvokeScreenAutomation :: InvokeScreenAutomation -> TestTree
requestInvokeScreenAutomation =
  req
    "InvokeScreenAutomation"
    "fixture/InvokeScreenAutomation.yaml"

requestDescribeTableDataImportJob :: DescribeTableDataImportJob -> TestTree
requestDescribeTableDataImportJob =
  req
    "DescribeTableDataImportJob"
    "fixture/DescribeTableDataImportJob.yaml"

requestStartTableDataImportJob :: StartTableDataImportJob -> TestTree
requestStartTableDataImportJob =
  req
    "StartTableDataImportJob"
    "fixture/StartTableDataImportJob.yaml"

requestBatchCreateTableRows :: BatchCreateTableRows -> TestTree
requestBatchCreateTableRows =
  req
    "BatchCreateTableRows"
    "fixture/BatchCreateTableRows.yaml"

requestListTables :: ListTables -> TestTree
requestListTables =
  req
    "ListTables"
    "fixture/ListTables.yaml"

requestGetScreenData :: GetScreenData -> TestTree
requestGetScreenData =
  req
    "GetScreenData"
    "fixture/GetScreenData.yaml"

requestQueryTableRows :: QueryTableRows -> TestTree
requestQueryTableRows =
  req
    "QueryTableRows"
    "fixture/QueryTableRows.yaml"

requestBatchUpsertTableRows :: BatchUpsertTableRows -> TestTree
requestBatchUpsertTableRows =
  req
    "BatchUpsertTableRows"
    "fixture/BatchUpsertTableRows.yaml"

requestListTableColumns :: ListTableColumns -> TestTree
requestListTableColumns =
  req
    "ListTableColumns"
    "fixture/ListTableColumns.yaml"

-- Responses

responseBatchDeleteTableRows :: BatchDeleteTableRowsResponse -> TestTree
responseBatchDeleteTableRows =
  res
    "BatchDeleteTableRowsResponse"
    "fixture/BatchDeleteTableRowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteTableRows)

responseBatchUpdateTableRows :: BatchUpdateTableRowsResponse -> TestTree
responseBatchUpdateTableRows =
  res
    "BatchUpdateTableRowsResponse"
    "fixture/BatchUpdateTableRowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchUpdateTableRows)

responseListTableRows :: ListTableRowsResponse -> TestTree
responseListTableRows =
  res
    "ListTableRowsResponse"
    "fixture/ListTableRowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTableRows)

responseInvokeScreenAutomation :: InvokeScreenAutomationResponse -> TestTree
responseInvokeScreenAutomation =
  res
    "InvokeScreenAutomationResponse"
    "fixture/InvokeScreenAutomationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InvokeScreenAutomation)

responseDescribeTableDataImportJob :: DescribeTableDataImportJobResponse -> TestTree
responseDescribeTableDataImportJob =
  res
    "DescribeTableDataImportJobResponse"
    "fixture/DescribeTableDataImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTableDataImportJob)

responseStartTableDataImportJob :: StartTableDataImportJobResponse -> TestTree
responseStartTableDataImportJob =
  res
    "StartTableDataImportJobResponse"
    "fixture/StartTableDataImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartTableDataImportJob)

responseBatchCreateTableRows :: BatchCreateTableRowsResponse -> TestTree
responseBatchCreateTableRows =
  res
    "BatchCreateTableRowsResponse"
    "fixture/BatchCreateTableRowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchCreateTableRows)

responseListTables :: ListTablesResponse -> TestTree
responseListTables =
  res
    "ListTablesResponse"
    "fixture/ListTablesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTables)

responseGetScreenData :: GetScreenDataResponse -> TestTree
responseGetScreenData =
  res
    "GetScreenDataResponse"
    "fixture/GetScreenDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetScreenData)

responseQueryTableRows :: QueryTableRowsResponse -> TestTree
responseQueryTableRows =
  res
    "QueryTableRowsResponse"
    "fixture/QueryTableRowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy QueryTableRows)

responseBatchUpsertTableRows :: BatchUpsertTableRowsResponse -> TestTree
responseBatchUpsertTableRows =
  res
    "BatchUpsertTableRowsResponse"
    "fixture/BatchUpsertTableRowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchUpsertTableRows)

responseListTableColumns :: ListTableColumnsResponse -> TestTree
responseListTableColumns =
  res
    "ListTableColumnsResponse"
    "fixture/ListTableColumnsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTableColumns)
