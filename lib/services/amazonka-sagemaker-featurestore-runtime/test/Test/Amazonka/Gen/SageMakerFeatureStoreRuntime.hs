{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SageMakerFeatureStoreRuntime
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.SageMakerFeatureStoreRuntime where

import Amazonka.SageMakerFeatureStoreRuntime
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.SageMakerFeatureStoreRuntime.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchGetRecord $
--             newBatchGetRecord
--
--         , requestDeleteRecord $
--             newDeleteRecord
--
--         , requestGetRecord $
--             newGetRecord
--
--         , requestPutRecord $
--             newPutRecord
--
--           ]

--     , testGroup "response"
--         [ responseBatchGetRecord $
--             newBatchGetRecordResponse
--
--         , responseDeleteRecord $
--             newDeleteRecordResponse
--
--         , responseGetRecord $
--             newGetRecordResponse
--
--         , responsePutRecord $
--             newPutRecordResponse
--
--           ]
--     ]

-- Requests

requestBatchGetRecord :: BatchGetRecord -> TestTree
requestBatchGetRecord =
  req
    "BatchGetRecord"
    "fixture/BatchGetRecord.yaml"

requestDeleteRecord :: DeleteRecord -> TestTree
requestDeleteRecord =
  req
    "DeleteRecord"
    "fixture/DeleteRecord.yaml"

requestGetRecord :: GetRecord -> TestTree
requestGetRecord =
  req
    "GetRecord"
    "fixture/GetRecord.yaml"

requestPutRecord :: PutRecord -> TestTree
requestPutRecord =
  req
    "PutRecord"
    "fixture/PutRecord.yaml"

-- Responses

responseBatchGetRecord :: BatchGetRecordResponse -> TestTree
responseBatchGetRecord =
  res
    "BatchGetRecordResponse"
    "fixture/BatchGetRecordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetRecord)

responseDeleteRecord :: DeleteRecordResponse -> TestTree
responseDeleteRecord =
  res
    "DeleteRecordResponse"
    "fixture/DeleteRecordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRecord)

responseGetRecord :: GetRecordResponse -> TestTree
responseGetRecord =
  res
    "GetRecordResponse"
    "fixture/GetRecordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRecord)

responsePutRecord :: PutRecordResponse -> TestTree
responsePutRecord =
  res
    "PutRecordResponse"
    "fixture/PutRecordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRecord)
