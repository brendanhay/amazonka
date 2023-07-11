{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.HoneyCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
--         [ requestBatchCreateTableRows $
--             newBatchCreateTableRows
--
--         , requestBatchDeleteTableRows $
--             newBatchDeleteTableRows
--
--         , requestBatchUpdateTableRows $
--             newBatchUpdateTableRows
--
--         , requestBatchUpsertTableRows $
--             newBatchUpsertTableRows
--
--         , requestDescribeTableDataImportJob $
--             newDescribeTableDataImportJob
--
--         , requestGetScreenData $
--             newGetScreenData
--
--         , requestInvokeScreenAutomation $
--             newInvokeScreenAutomation
--
--         , requestListTableColumns $
--             newListTableColumns
--
--         , requestListTableRows $
--             newListTableRows
--
--         , requestListTables $
--             newListTables
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestQueryTableRows $
--             newQueryTableRows
--
--         , requestStartTableDataImportJob $
--             newStartTableDataImportJob
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseBatchCreateTableRows $
--             newBatchCreateTableRowsResponse
--
--         , responseBatchDeleteTableRows $
--             newBatchDeleteTableRowsResponse
--
--         , responseBatchUpdateTableRows $
--             newBatchUpdateTableRowsResponse
--
--         , responseBatchUpsertTableRows $
--             newBatchUpsertTableRowsResponse
--
--         , responseDescribeTableDataImportJob $
--             newDescribeTableDataImportJobResponse
--
--         , responseGetScreenData $
--             newGetScreenDataResponse
--
--         , responseInvokeScreenAutomation $
--             newInvokeScreenAutomationResponse
--
--         , responseListTableColumns $
--             newListTableColumnsResponse
--
--         , responseListTableRows $
--             newListTableRowsResponse
--
--         , responseListTables $
--             newListTablesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseQueryTableRows $
--             newQueryTableRowsResponse
--
--         , responseStartTableDataImportJob $
--             newStartTableDataImportJobResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--           ]
--     ]

-- Requests

requestBatchCreateTableRows :: BatchCreateTableRows -> TestTree
requestBatchCreateTableRows =
  req
    "BatchCreateTableRows"
    "fixture/BatchCreateTableRows.yaml"

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

requestBatchUpsertTableRows :: BatchUpsertTableRows -> TestTree
requestBatchUpsertTableRows =
  req
    "BatchUpsertTableRows"
    "fixture/BatchUpsertTableRows.yaml"

requestDescribeTableDataImportJob :: DescribeTableDataImportJob -> TestTree
requestDescribeTableDataImportJob =
  req
    "DescribeTableDataImportJob"
    "fixture/DescribeTableDataImportJob.yaml"

requestGetScreenData :: GetScreenData -> TestTree
requestGetScreenData =
  req
    "GetScreenData"
    "fixture/GetScreenData.yaml"

requestInvokeScreenAutomation :: InvokeScreenAutomation -> TestTree
requestInvokeScreenAutomation =
  req
    "InvokeScreenAutomation"
    "fixture/InvokeScreenAutomation.yaml"

requestListTableColumns :: ListTableColumns -> TestTree
requestListTableColumns =
  req
    "ListTableColumns"
    "fixture/ListTableColumns.yaml"

requestListTableRows :: ListTableRows -> TestTree
requestListTableRows =
  req
    "ListTableRows"
    "fixture/ListTableRows.yaml"

requestListTables :: ListTables -> TestTree
requestListTables =
  req
    "ListTables"
    "fixture/ListTables.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestQueryTableRows :: QueryTableRows -> TestTree
requestQueryTableRows =
  req
    "QueryTableRows"
    "fixture/QueryTableRows.yaml"

requestStartTableDataImportJob :: StartTableDataImportJob -> TestTree
requestStartTableDataImportJob =
  req
    "StartTableDataImportJob"
    "fixture/StartTableDataImportJob.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

-- Responses

responseBatchCreateTableRows :: BatchCreateTableRowsResponse -> TestTree
responseBatchCreateTableRows =
  res
    "BatchCreateTableRowsResponse"
    "fixture/BatchCreateTableRowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchCreateTableRows)

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

responseBatchUpsertTableRows :: BatchUpsertTableRowsResponse -> TestTree
responseBatchUpsertTableRows =
  res
    "BatchUpsertTableRowsResponse"
    "fixture/BatchUpsertTableRowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchUpsertTableRows)

responseDescribeTableDataImportJob :: DescribeTableDataImportJobResponse -> TestTree
responseDescribeTableDataImportJob =
  res
    "DescribeTableDataImportJobResponse"
    "fixture/DescribeTableDataImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTableDataImportJob)

responseGetScreenData :: GetScreenDataResponse -> TestTree
responseGetScreenData =
  res
    "GetScreenDataResponse"
    "fixture/GetScreenDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetScreenData)

responseInvokeScreenAutomation :: InvokeScreenAutomationResponse -> TestTree
responseInvokeScreenAutomation =
  res
    "InvokeScreenAutomationResponse"
    "fixture/InvokeScreenAutomationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InvokeScreenAutomation)

responseListTableColumns :: ListTableColumnsResponse -> TestTree
responseListTableColumns =
  res
    "ListTableColumnsResponse"
    "fixture/ListTableColumnsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTableColumns)

responseListTableRows :: ListTableRowsResponse -> TestTree
responseListTableRows =
  res
    "ListTableRowsResponse"
    "fixture/ListTableRowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTableRows)

responseListTables :: ListTablesResponse -> TestTree
responseListTables =
  res
    "ListTablesResponse"
    "fixture/ListTablesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTables)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseQueryTableRows :: QueryTableRowsResponse -> TestTree
responseQueryTableRows =
  res
    "QueryTableRowsResponse"
    "fixture/QueryTableRowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy QueryTableRows)

responseStartTableDataImportJob :: StartTableDataImportJobResponse -> TestTree
responseStartTableDataImportJob =
  res
    "StartTableDataImportJobResponse"
    "fixture/StartTableDataImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartTableDataImportJob)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)
