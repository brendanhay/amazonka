{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.EBS
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.EBS where

import Amazonka.EBS
import qualified Data.Proxy as Proxy
import Test.Amazonka.EBS.Internal
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
--         [ requestCompleteSnapshot $
--             newCompleteSnapshot
--
--         , requestGetSnapshotBlock $
--             newGetSnapshotBlock
--
--         , requestListChangedBlocks $
--             newListChangedBlocks
--
--         , requestListSnapshotBlocks $
--             newListSnapshotBlocks
--
--         , requestPutSnapshotBlock $
--             newPutSnapshotBlock
--
--         , requestStartSnapshot $
--             newStartSnapshot
--
--           ]

--     , testGroup "response"
--         [ responseCompleteSnapshot $
--             newCompleteSnapshotResponse
--
--         , responseGetSnapshotBlock $
--             newGetSnapshotBlockResponse
--
--         , responseListChangedBlocks $
--             newListChangedBlocksResponse
--
--         , responseListSnapshotBlocks $
--             newListSnapshotBlocksResponse
--
--         , responsePutSnapshotBlock $
--             newPutSnapshotBlockResponse
--
--         , responseStartSnapshot $
--             newStartSnapshotResponse
--
--           ]
--     ]

-- Requests

requestCompleteSnapshot :: CompleteSnapshot -> TestTree
requestCompleteSnapshot =
  req
    "CompleteSnapshot"
    "fixture/CompleteSnapshot.yaml"

requestGetSnapshotBlock :: GetSnapshotBlock -> TestTree
requestGetSnapshotBlock =
  req
    "GetSnapshotBlock"
    "fixture/GetSnapshotBlock.yaml"

requestListChangedBlocks :: ListChangedBlocks -> TestTree
requestListChangedBlocks =
  req
    "ListChangedBlocks"
    "fixture/ListChangedBlocks.yaml"

requestListSnapshotBlocks :: ListSnapshotBlocks -> TestTree
requestListSnapshotBlocks =
  req
    "ListSnapshotBlocks"
    "fixture/ListSnapshotBlocks.yaml"

requestStartSnapshot :: StartSnapshot -> TestTree
requestStartSnapshot =
  req
    "StartSnapshot"
    "fixture/StartSnapshot.yaml"

-- Responses

responseCompleteSnapshot :: CompleteSnapshotResponse -> TestTree
responseCompleteSnapshot =
  res
    "CompleteSnapshotResponse"
    "fixture/CompleteSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CompleteSnapshot)

responseListChangedBlocks :: ListChangedBlocksResponse -> TestTree
responseListChangedBlocks =
  res
    "ListChangedBlocksResponse"
    "fixture/ListChangedBlocksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChangedBlocks)

responseListSnapshotBlocks :: ListSnapshotBlocksResponse -> TestTree
responseListSnapshotBlocks =
  res
    "ListSnapshotBlocksResponse"
    "fixture/ListSnapshotBlocksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSnapshotBlocks)

responsePutSnapshotBlock :: PutSnapshotBlockResponse -> TestTree
responsePutSnapshotBlock =
  res
    "PutSnapshotBlockResponse"
    "fixture/PutSnapshotBlockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutSnapshotBlock)

responseStartSnapshot :: StartSnapshotResponse -> TestTree
responseStartSnapshot =
  res
    "StartSnapshotResponse"
    "fixture/StartSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartSnapshot)
