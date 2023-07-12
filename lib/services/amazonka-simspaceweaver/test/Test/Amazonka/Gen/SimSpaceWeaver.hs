{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SimSpaceWeaver
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.SimSpaceWeaver where

import Amazonka.SimSpaceWeaver
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.SimSpaceWeaver.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDeleteApp $
--             newDeleteApp
--
--         , requestDeleteSimulation $
--             newDeleteSimulation
--
--         , requestDescribeApp $
--             newDescribeApp
--
--         , requestDescribeSimulation $
--             newDescribeSimulation
--
--         , requestListApps $
--             newListApps
--
--         , requestListSimulations $
--             newListSimulations
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStartApp $
--             newStartApp
--
--         , requestStartClock $
--             newStartClock
--
--         , requestStartSimulation $
--             newStartSimulation
--
--         , requestStopApp $
--             newStopApp
--
--         , requestStopClock $
--             newStopClock
--
--         , requestStopSimulation $
--             newStopSimulation
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseDeleteApp $
--             newDeleteAppResponse
--
--         , responseDeleteSimulation $
--             newDeleteSimulationResponse
--
--         , responseDescribeApp $
--             newDescribeAppResponse
--
--         , responseDescribeSimulation $
--             newDescribeSimulationResponse
--
--         , responseListApps $
--             newListAppsResponse
--
--         , responseListSimulations $
--             newListSimulationsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStartApp $
--             newStartAppResponse
--
--         , responseStartClock $
--             newStartClockResponse
--
--         , responseStartSimulation $
--             newStartSimulationResponse
--
--         , responseStopApp $
--             newStopAppResponse
--
--         , responseStopClock $
--             newStopClockResponse
--
--         , responseStopSimulation $
--             newStopSimulationResponse
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

requestDeleteApp :: DeleteApp -> TestTree
requestDeleteApp =
  req
    "DeleteApp"
    "fixture/DeleteApp.yaml"

requestDeleteSimulation :: DeleteSimulation -> TestTree
requestDeleteSimulation =
  req
    "DeleteSimulation"
    "fixture/DeleteSimulation.yaml"

requestDescribeApp :: DescribeApp -> TestTree
requestDescribeApp =
  req
    "DescribeApp"
    "fixture/DescribeApp.yaml"

requestDescribeSimulation :: DescribeSimulation -> TestTree
requestDescribeSimulation =
  req
    "DescribeSimulation"
    "fixture/DescribeSimulation.yaml"

requestListApps :: ListApps -> TestTree
requestListApps =
  req
    "ListApps"
    "fixture/ListApps.yaml"

requestListSimulations :: ListSimulations -> TestTree
requestListSimulations =
  req
    "ListSimulations"
    "fixture/ListSimulations.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestStartApp :: StartApp -> TestTree
requestStartApp =
  req
    "StartApp"
    "fixture/StartApp.yaml"

requestStartClock :: StartClock -> TestTree
requestStartClock =
  req
    "StartClock"
    "fixture/StartClock.yaml"

requestStartSimulation :: StartSimulation -> TestTree
requestStartSimulation =
  req
    "StartSimulation"
    "fixture/StartSimulation.yaml"

requestStopApp :: StopApp -> TestTree
requestStopApp =
  req
    "StopApp"
    "fixture/StopApp.yaml"

requestStopClock :: StopClock -> TestTree
requestStopClock =
  req
    "StopClock"
    "fixture/StopClock.yaml"

requestStopSimulation :: StopSimulation -> TestTree
requestStopSimulation =
  req
    "StopSimulation"
    "fixture/StopSimulation.yaml"

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

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp =
  res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApp)

responseDeleteSimulation :: DeleteSimulationResponse -> TestTree
responseDeleteSimulation =
  res
    "DeleteSimulationResponse"
    "fixture/DeleteSimulationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSimulation)

responseDescribeApp :: DescribeAppResponse -> TestTree
responseDescribeApp =
  res
    "DescribeAppResponse"
    "fixture/DescribeAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeApp)

responseDescribeSimulation :: DescribeSimulationResponse -> TestTree
responseDescribeSimulation =
  res
    "DescribeSimulationResponse"
    "fixture/DescribeSimulationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSimulation)

responseListApps :: ListAppsResponse -> TestTree
responseListApps =
  res
    "ListAppsResponse"
    "fixture/ListAppsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApps)

responseListSimulations :: ListSimulationsResponse -> TestTree
responseListSimulations =
  res
    "ListSimulationsResponse"
    "fixture/ListSimulationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSimulations)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseStartApp :: StartAppResponse -> TestTree
responseStartApp =
  res
    "StartAppResponse"
    "fixture/StartAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartApp)

responseStartClock :: StartClockResponse -> TestTree
responseStartClock =
  res
    "StartClockResponse"
    "fixture/StartClockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartClock)

responseStartSimulation :: StartSimulationResponse -> TestTree
responseStartSimulation =
  res
    "StartSimulationResponse"
    "fixture/StartSimulationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartSimulation)

responseStopApp :: StopAppResponse -> TestTree
responseStopApp =
  res
    "StopAppResponse"
    "fixture/StopAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopApp)

responseStopClock :: StopClockResponse -> TestTree
responseStopClock =
  res
    "StopClockResponse"
    "fixture/StopClockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopClock)

responseStopSimulation :: StopSimulationResponse -> TestTree
responseStopSimulation =
  res
    "StopSimulationResponse"
    "fixture/StopSimulationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopSimulation)

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
