{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Rum
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Rum where

import Amazonka.Rum
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.Rum.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateAppMonitor $
--             newCreateAppMonitor
--
--         , requestDeleteAppMonitor $
--             newDeleteAppMonitor
--
--         , requestGetAppMonitor $
--             newGetAppMonitor
--
--         , requestGetAppMonitorData $
--             newGetAppMonitorData
--
--         , requestListAppMonitors $
--             newListAppMonitors
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutRumEvents $
--             newPutRumEvents
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAppMonitor $
--             newUpdateAppMonitor
--
--           ]

--     , testGroup "response"
--         [ responseCreateAppMonitor $
--             newCreateAppMonitorResponse
--
--         , responseDeleteAppMonitor $
--             newDeleteAppMonitorResponse
--
--         , responseGetAppMonitor $
--             newGetAppMonitorResponse
--
--         , responseGetAppMonitorData $
--             newGetAppMonitorDataResponse
--
--         , responseListAppMonitors $
--             newListAppMonitorsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutRumEvents $
--             newPutRumEventsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAppMonitor $
--             newUpdateAppMonitorResponse
--
--           ]
--     ]

-- Requests

requestCreateAppMonitor :: CreateAppMonitor -> TestTree
requestCreateAppMonitor =
  req
    "CreateAppMonitor"
    "fixture/CreateAppMonitor.yaml"

requestDeleteAppMonitor :: DeleteAppMonitor -> TestTree
requestDeleteAppMonitor =
  req
    "DeleteAppMonitor"
    "fixture/DeleteAppMonitor.yaml"

requestGetAppMonitor :: GetAppMonitor -> TestTree
requestGetAppMonitor =
  req
    "GetAppMonitor"
    "fixture/GetAppMonitor.yaml"

requestGetAppMonitorData :: GetAppMonitorData -> TestTree
requestGetAppMonitorData =
  req
    "GetAppMonitorData"
    "fixture/GetAppMonitorData.yaml"

requestListAppMonitors :: ListAppMonitors -> TestTree
requestListAppMonitors =
  req
    "ListAppMonitors"
    "fixture/ListAppMonitors.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutRumEvents :: PutRumEvents -> TestTree
requestPutRumEvents =
  req
    "PutRumEvents"
    "fixture/PutRumEvents.yaml"

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

requestUpdateAppMonitor :: UpdateAppMonitor -> TestTree
requestUpdateAppMonitor =
  req
    "UpdateAppMonitor"
    "fixture/UpdateAppMonitor.yaml"

-- Responses

responseCreateAppMonitor :: CreateAppMonitorResponse -> TestTree
responseCreateAppMonitor =
  res
    "CreateAppMonitorResponse"
    "fixture/CreateAppMonitorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAppMonitor)

responseDeleteAppMonitor :: DeleteAppMonitorResponse -> TestTree
responseDeleteAppMonitor =
  res
    "DeleteAppMonitorResponse"
    "fixture/DeleteAppMonitorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAppMonitor)

responseGetAppMonitor :: GetAppMonitorResponse -> TestTree
responseGetAppMonitor =
  res
    "GetAppMonitorResponse"
    "fixture/GetAppMonitorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAppMonitor)

responseGetAppMonitorData :: GetAppMonitorDataResponse -> TestTree
responseGetAppMonitorData =
  res
    "GetAppMonitorDataResponse"
    "fixture/GetAppMonitorDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAppMonitorData)

responseListAppMonitors :: ListAppMonitorsResponse -> TestTree
responseListAppMonitors =
  res
    "ListAppMonitorsResponse"
    "fixture/ListAppMonitorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAppMonitors)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutRumEvents :: PutRumEventsResponse -> TestTree
responsePutRumEvents =
  res
    "PutRumEventsResponse"
    "fixture/PutRumEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRumEvents)

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

responseUpdateAppMonitor :: UpdateAppMonitorResponse -> TestTree
responseUpdateAppMonitor =
  res
    "UpdateAppMonitorResponse"
    "fixture/UpdateAppMonitorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAppMonitor)
