{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.IoTData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.IoTData where

import Amazonka.IoTData
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.IoTData.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDeleteThingShadow $
--             newDeleteThingShadow
--
--         , requestGetRetainedMessage $
--             newGetRetainedMessage
--
--         , requestGetThingShadow $
--             newGetThingShadow
--
--         , requestListNamedShadowsForThing $
--             newListNamedShadowsForThing
--
--         , requestListRetainedMessages $
--             newListRetainedMessages
--
--         , requestPublish $
--             newPublish
--
--         , requestUpdateThingShadow $
--             newUpdateThingShadow
--
--           ]

--     , testGroup "response"
--         [ responseDeleteThingShadow $
--             newDeleteThingShadowResponse
--
--         , responseGetRetainedMessage $
--             newGetRetainedMessageResponse
--
--         , responseGetThingShadow $
--             newGetThingShadowResponse
--
--         , responseListNamedShadowsForThing $
--             newListNamedShadowsForThingResponse
--
--         , responseListRetainedMessages $
--             newListRetainedMessagesResponse
--
--         , responsePublish $
--             newPublishResponse
--
--         , responseUpdateThingShadow $
--             newUpdateThingShadowResponse
--
--           ]
--     ]

-- Requests

requestDeleteThingShadow :: DeleteThingShadow -> TestTree
requestDeleteThingShadow =
  req
    "DeleteThingShadow"
    "fixture/DeleteThingShadow.yaml"

requestGetRetainedMessage :: GetRetainedMessage -> TestTree
requestGetRetainedMessage =
  req
    "GetRetainedMessage"
    "fixture/GetRetainedMessage.yaml"

requestGetThingShadow :: GetThingShadow -> TestTree
requestGetThingShadow =
  req
    "GetThingShadow"
    "fixture/GetThingShadow.yaml"

requestListNamedShadowsForThing :: ListNamedShadowsForThing -> TestTree
requestListNamedShadowsForThing =
  req
    "ListNamedShadowsForThing"
    "fixture/ListNamedShadowsForThing.yaml"

requestListRetainedMessages :: ListRetainedMessages -> TestTree
requestListRetainedMessages =
  req
    "ListRetainedMessages"
    "fixture/ListRetainedMessages.yaml"

requestPublish :: Publish -> TestTree
requestPublish =
  req
    "Publish"
    "fixture/Publish.yaml"

requestUpdateThingShadow :: UpdateThingShadow -> TestTree
requestUpdateThingShadow =
  req
    "UpdateThingShadow"
    "fixture/UpdateThingShadow.yaml"

-- Responses

responseDeleteThingShadow :: DeleteThingShadowResponse -> TestTree
responseDeleteThingShadow =
  res
    "DeleteThingShadowResponse"
    "fixture/DeleteThingShadowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteThingShadow)

responseGetRetainedMessage :: GetRetainedMessageResponse -> TestTree
responseGetRetainedMessage =
  res
    "GetRetainedMessageResponse"
    "fixture/GetRetainedMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRetainedMessage)

responseGetThingShadow :: GetThingShadowResponse -> TestTree
responseGetThingShadow =
  res
    "GetThingShadowResponse"
    "fixture/GetThingShadowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetThingShadow)

responseListNamedShadowsForThing :: ListNamedShadowsForThingResponse -> TestTree
responseListNamedShadowsForThing =
  res
    "ListNamedShadowsForThingResponse"
    "fixture/ListNamedShadowsForThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNamedShadowsForThing)

responseListRetainedMessages :: ListRetainedMessagesResponse -> TestTree
responseListRetainedMessages =
  res
    "ListRetainedMessagesResponse"
    "fixture/ListRetainedMessagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRetainedMessages)

responsePublish :: PublishResponse -> TestTree
responsePublish =
  res
    "PublishResponse"
    "fixture/PublishResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Publish)

responseUpdateThingShadow :: UpdateThingShadowResponse -> TestTree
responseUpdateThingShadow =
  res
    "UpdateThingShadowResponse"
    "fixture/UpdateThingShadowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateThingShadow)
