{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.IoTData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.IoTData where

import qualified Data.Proxy as Proxy
import Network.AWS.IoTData
import Test.AWS.Fixture
import Test.AWS.IoTData.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetThingShadow $
--             newGetThingShadow
--
--         , requestListNamedShadowsForThing $
--             newListNamedShadowsForThing
--
--         , requestDeleteThingShadow $
--             newDeleteThingShadow
--
--         , requestUpdateThingShadow $
--             newUpdateThingShadow
--
--         , requestListRetainedMessages $
--             newListRetainedMessages
--
--         , requestGetRetainedMessage $
--             newGetRetainedMessage
--
--         , requestPublish $
--             newPublish
--
--           ]

--     , testGroup "response"
--         [ responseGetThingShadow $
--             newGetThingShadowResponse
--
--         , responseListNamedShadowsForThing $
--             newListNamedShadowsForThingResponse
--
--         , responseDeleteThingShadow $
--             newDeleteThingShadowResponse
--
--         , responseUpdateThingShadow $
--             newUpdateThingShadowResponse
--
--         , responseListRetainedMessages $
--             newListRetainedMessagesResponse
--
--         , responseGetRetainedMessage $
--             newGetRetainedMessageResponse
--
--         , responsePublish $
--             newPublishResponse
--
--           ]
--     ]

-- Requests

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

requestDeleteThingShadow :: DeleteThingShadow -> TestTree
requestDeleteThingShadow =
  req
    "DeleteThingShadow"
    "fixture/DeleteThingShadow.yaml"

requestUpdateThingShadow :: UpdateThingShadow -> TestTree
requestUpdateThingShadow =
  req
    "UpdateThingShadow"
    "fixture/UpdateThingShadow.yaml"

requestListRetainedMessages :: ListRetainedMessages -> TestTree
requestListRetainedMessages =
  req
    "ListRetainedMessages"
    "fixture/ListRetainedMessages.yaml"

requestGetRetainedMessage :: GetRetainedMessage -> TestTree
requestGetRetainedMessage =
  req
    "GetRetainedMessage"
    "fixture/GetRetainedMessage.yaml"

requestPublish :: Publish -> TestTree
requestPublish =
  req
    "Publish"
    "fixture/Publish.yaml"

-- Responses

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

responseDeleteThingShadow :: DeleteThingShadowResponse -> TestTree
responseDeleteThingShadow =
  res
    "DeleteThingShadowResponse"
    "fixture/DeleteThingShadowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteThingShadow)

responseUpdateThingShadow :: UpdateThingShadowResponse -> TestTree
responseUpdateThingShadow =
  res
    "UpdateThingShadowResponse"
    "fixture/UpdateThingShadowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateThingShadow)

responseListRetainedMessages :: ListRetainedMessagesResponse -> TestTree
responseListRetainedMessages =
  res
    "ListRetainedMessagesResponse"
    "fixture/ListRetainedMessagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRetainedMessages)

responseGetRetainedMessage :: GetRetainedMessageResponse -> TestTree
responseGetRetainedMessage =
  res
    "GetRetainedMessageResponse"
    "fixture/GetRetainedMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRetainedMessage)

responsePublish :: PublishResponse -> TestTree
responsePublish =
  res
    "PublishResponse"
    "fixture/PublishResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Publish)
