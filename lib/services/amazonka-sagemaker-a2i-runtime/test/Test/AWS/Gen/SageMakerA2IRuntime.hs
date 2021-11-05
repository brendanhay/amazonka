{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SageMakerA2IRuntime
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.SageMakerA2IRuntime where

import Amazonka.SageMakerA2IRuntime
import qualified Data.Proxy as Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.SageMakerA2IRuntime.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListHumanLoops $
--             newListHumanLoops
--
--         , requestDeleteHumanLoop $
--             newDeleteHumanLoop
--
--         , requestStartHumanLoop $
--             newStartHumanLoop
--
--         , requestStopHumanLoop $
--             newStopHumanLoop
--
--         , requestDescribeHumanLoop $
--             newDescribeHumanLoop
--
--           ]

--     , testGroup "response"
--         [ responseListHumanLoops $
--             newListHumanLoopsResponse
--
--         , responseDeleteHumanLoop $
--             newDeleteHumanLoopResponse
--
--         , responseStartHumanLoop $
--             newStartHumanLoopResponse
--
--         , responseStopHumanLoop $
--             newStopHumanLoopResponse
--
--         , responseDescribeHumanLoop $
--             newDescribeHumanLoopResponse
--
--           ]
--     ]

-- Requests

requestListHumanLoops :: ListHumanLoops -> TestTree
requestListHumanLoops =
  req
    "ListHumanLoops"
    "fixture/ListHumanLoops.yaml"

requestDeleteHumanLoop :: DeleteHumanLoop -> TestTree
requestDeleteHumanLoop =
  req
    "DeleteHumanLoop"
    "fixture/DeleteHumanLoop.yaml"

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

requestDescribeHumanLoop :: DescribeHumanLoop -> TestTree
requestDescribeHumanLoop =
  req
    "DescribeHumanLoop"
    "fixture/DescribeHumanLoop.yaml"

-- Responses

responseListHumanLoops :: ListHumanLoopsResponse -> TestTree
responseListHumanLoops =
  res
    "ListHumanLoopsResponse"
    "fixture/ListHumanLoopsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHumanLoops)

responseDeleteHumanLoop :: DeleteHumanLoopResponse -> TestTree
responseDeleteHumanLoop =
  res
    "DeleteHumanLoopResponse"
    "fixture/DeleteHumanLoopResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHumanLoop)

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

responseDescribeHumanLoop :: DescribeHumanLoopResponse -> TestTree
responseDescribeHumanLoop =
  res
    "DescribeHumanLoopResponse"
    "fixture/DescribeHumanLoopResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHumanLoop)
