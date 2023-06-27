{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.InternetMonitor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.InternetMonitor where

import Amazonka.InternetMonitor
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.InternetMonitor.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateMonitor $
--             newCreateMonitor
--
--         , requestDeleteMonitor $
--             newDeleteMonitor
--
--         , requestGetHealthEvent $
--             newGetHealthEvent
--
--         , requestGetMonitor $
--             newGetMonitor
--
--         , requestListHealthEvents $
--             newListHealthEvents
--
--         , requestListMonitors $
--             newListMonitors
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateMonitor $
--             newUpdateMonitor
--
--           ]

--     , testGroup "response"
--         [ responseCreateMonitor $
--             newCreateMonitorResponse
--
--         , responseDeleteMonitor $
--             newDeleteMonitorResponse
--
--         , responseGetHealthEvent $
--             newGetHealthEventResponse
--
--         , responseGetMonitor $
--             newGetMonitorResponse
--
--         , responseListHealthEvents $
--             newListHealthEventsResponse
--
--         , responseListMonitors $
--             newListMonitorsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateMonitor $
--             newUpdateMonitorResponse
--
--           ]
--     ]

-- Requests

requestCreateMonitor :: CreateMonitor -> TestTree
requestCreateMonitor =
  req
    "CreateMonitor"
    "fixture/CreateMonitor.yaml"

requestDeleteMonitor :: DeleteMonitor -> TestTree
requestDeleteMonitor =
  req
    "DeleteMonitor"
    "fixture/DeleteMonitor.yaml"

requestGetHealthEvent :: GetHealthEvent -> TestTree
requestGetHealthEvent =
  req
    "GetHealthEvent"
    "fixture/GetHealthEvent.yaml"

requestGetMonitor :: GetMonitor -> TestTree
requestGetMonitor =
  req
    "GetMonitor"
    "fixture/GetMonitor.yaml"

requestListHealthEvents :: ListHealthEvents -> TestTree
requestListHealthEvents =
  req
    "ListHealthEvents"
    "fixture/ListHealthEvents.yaml"

requestListMonitors :: ListMonitors -> TestTree
requestListMonitors =
  req
    "ListMonitors"
    "fixture/ListMonitors.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

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

requestUpdateMonitor :: UpdateMonitor -> TestTree
requestUpdateMonitor =
  req
    "UpdateMonitor"
    "fixture/UpdateMonitor.yaml"

-- Responses

responseCreateMonitor :: CreateMonitorResponse -> TestTree
responseCreateMonitor =
  res
    "CreateMonitorResponse"
    "fixture/CreateMonitorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMonitor)

responseDeleteMonitor :: DeleteMonitorResponse -> TestTree
responseDeleteMonitor =
  res
    "DeleteMonitorResponse"
    "fixture/DeleteMonitorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMonitor)

responseGetHealthEvent :: GetHealthEventResponse -> TestTree
responseGetHealthEvent =
  res
    "GetHealthEventResponse"
    "fixture/GetHealthEventResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHealthEvent)

responseGetMonitor :: GetMonitorResponse -> TestTree
responseGetMonitor =
  res
    "GetMonitorResponse"
    "fixture/GetMonitorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMonitor)

responseListHealthEvents :: ListHealthEventsResponse -> TestTree
responseListHealthEvents =
  res
    "ListHealthEventsResponse"
    "fixture/ListHealthEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHealthEvents)

responseListMonitors :: ListMonitorsResponse -> TestTree
responseListMonitors =
  res
    "ListMonitorsResponse"
    "fixture/ListMonitorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMonitors)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

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

responseUpdateMonitor :: UpdateMonitorResponse -> TestTree
responseUpdateMonitor =
  res
    "UpdateMonitorResponse"
    "fixture/UpdateMonitorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMonitor)
