{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.EBS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.EBS where

import Data.Proxy
import Network.AWS.EBS
import Test.AWS.EBS.Internal
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
--         [ requestStartSnapshot $
--             newStartSnapshot
--
--         , requestListSnapshotBlocks $
--             newListSnapshotBlocks
--
--         , requestPutSnapshotBlock $
--             newPutSnapshotBlock
--
--         , requestListChangedBlocks $
--             newListChangedBlocks
--
--         , requestCompleteSnapshot $
--             newCompleteSnapshot
--
--         , requestGetSnapshotBlock $
--             newGetSnapshotBlock
--
--           ]

--     , testGroup "response"
--         [ responseStartSnapshot $
--             newStartSnapshotResponse
--
--         , responseListSnapshotBlocks $
--             newListSnapshotBlocksResponse
--
--         , responsePutSnapshotBlock $
--             newPutSnapshotBlockResponse
--
--         , responseListChangedBlocks $
--             newListChangedBlocksResponse
--
--         , responseCompleteSnapshot $
--             newCompleteSnapshotResponse
--
--         , responseGetSnapshotBlock $
--             newGetSnapshotBlockResponse
--
--           ]
--     ]

-- Requests

requestStartSnapshot :: StartSnapshot -> TestTree
requestStartSnapshot =
  req
    "StartSnapshot"
    "fixture/StartSnapshot.yaml"

requestListSnapshotBlocks :: ListSnapshotBlocks -> TestTree
requestListSnapshotBlocks =
  req
    "ListSnapshotBlocks"
    "fixture/ListSnapshotBlocks.yaml"

requestListChangedBlocks :: ListChangedBlocks -> TestTree
requestListChangedBlocks =
  req
    "ListChangedBlocks"
    "fixture/ListChangedBlocks.yaml"

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

-- Responses

responseStartSnapshot :: StartSnapshotResponse -> TestTree
responseStartSnapshot =
  res
    "StartSnapshotResponse"
    "fixture/StartSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy StartSnapshot)

responseListSnapshotBlocks :: ListSnapshotBlocksResponse -> TestTree
responseListSnapshotBlocks =
  res
    "ListSnapshotBlocksResponse"
    "fixture/ListSnapshotBlocksResponse.proto"
    defaultService
    (Proxy :: Proxy ListSnapshotBlocks)

responsePutSnapshotBlock :: PutSnapshotBlockResponse -> TestTree
responsePutSnapshotBlock =
  res
    "PutSnapshotBlockResponse"
    "fixture/PutSnapshotBlockResponse.proto"
    defaultService
    (Proxy :: Proxy PutSnapshotBlock)

responseListChangedBlocks :: ListChangedBlocksResponse -> TestTree
responseListChangedBlocks =
  res
    "ListChangedBlocksResponse"
    "fixture/ListChangedBlocksResponse.proto"
    defaultService
    (Proxy :: Proxy ListChangedBlocks)

responseCompleteSnapshot :: CompleteSnapshotResponse -> TestTree
responseCompleteSnapshot =
  res
    "CompleteSnapshotResponse"
    "fixture/CompleteSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CompleteSnapshot)
