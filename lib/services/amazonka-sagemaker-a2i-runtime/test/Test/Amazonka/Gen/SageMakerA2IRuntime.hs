{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SageMakerA2IRuntime
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.SageMakerA2IRuntime where

import Amazonka.SageMakerA2IRuntime
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.SageMakerA2IRuntime.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDeleteHumanLoop $
--             newDeleteHumanLoop
--
--         , requestDescribeHumanLoop $
--             newDescribeHumanLoop
--
--         , requestListHumanLoops $
--             newListHumanLoops
--
--         , requestStartHumanLoop $
--             newStartHumanLoop
--
--         , requestStopHumanLoop $
--             newStopHumanLoop
--
--           ]

--     , testGroup "response"
--         [ responseDeleteHumanLoop $
--             newDeleteHumanLoopResponse
--
--         , responseDescribeHumanLoop $
--             newDescribeHumanLoopResponse
--
--         , responseListHumanLoops $
--             newListHumanLoopsResponse
--
--         , responseStartHumanLoop $
--             newStartHumanLoopResponse
--
--         , responseStopHumanLoop $
--             newStopHumanLoopResponse
--
--           ]
--     ]

-- Requests

requestDeleteHumanLoop :: DeleteHumanLoop -> TestTree
requestDeleteHumanLoop =
  req
    "DeleteHumanLoop"
    "fixture/DeleteHumanLoop.yaml"

requestDescribeHumanLoop :: DescribeHumanLoop -> TestTree
requestDescribeHumanLoop =
  req
    "DescribeHumanLoop"
    "fixture/DescribeHumanLoop.yaml"

requestListHumanLoops :: ListHumanLoops -> TestTree
requestListHumanLoops =
  req
    "ListHumanLoops"
    "fixture/ListHumanLoops.yaml"

requestStartHumanLoop :: StartHumanLoop -> TestTree
requestStartHumanLoop =
  req
    "StartHumanLoop"
    "fixture/StartHumanLoop.yaml"

requestStopHumanLoop :: StopHumanLoop -> TestTree
requestStopHumanLoop =
  req
    "StopHumanLoop"
    "fixture/StopHumanLoop.yaml"

-- Responses

responseDeleteHumanLoop :: DeleteHumanLoopResponse -> TestTree
responseDeleteHumanLoop =
  res
    "DeleteHumanLoopResponse"
    "fixture/DeleteHumanLoopResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHumanLoop)

responseDescribeHumanLoop :: DescribeHumanLoopResponse -> TestTree
responseDescribeHumanLoop =
  res
    "DescribeHumanLoopResponse"
    "fixture/DescribeHumanLoopResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHumanLoop)

responseListHumanLoops :: ListHumanLoopsResponse -> TestTree
responseListHumanLoops =
  res
    "ListHumanLoopsResponse"
    "fixture/ListHumanLoopsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHumanLoops)

responseStartHumanLoop :: StartHumanLoopResponse -> TestTree
responseStartHumanLoop =
  res
    "StartHumanLoopResponse"
    "fixture/StartHumanLoopResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartHumanLoop)

responseStopHumanLoop :: StopHumanLoopResponse -> TestTree
responseStopHumanLoop =
  res
    "StopHumanLoopResponse"
    "fixture/StopHumanLoopResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopHumanLoop)
