{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.IoTData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.IoTData where

import Data.Proxy
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
--             mkGetThingShadow
--
--         , requestListNamedShadowsForThing $
--             mkListNamedShadowsForThing
--
--         , requestDeleteThingShadow $
--             mkDeleteThingShadow
--
--         , requestUpdateThingShadow $
--             mkUpdateThingShadow
--
--         , requestPublish $
--             mkPublish
--
--           ]

--     , testGroup "response"
--         [ responseGetThingShadow $
--             mkGetThingShadowResponse
--
--         , responseListNamedShadowsForThing $
--             mkListNamedShadowsForThingResponse
--
--         , responseDeleteThingShadow $
--             mkDeleteThingShadowResponse
--
--         , responseUpdateThingShadow $
--             mkUpdateThingShadowResponse
--
--         , responsePublish $
--             mkPublishResponse
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
    mkServiceConfig
    (Proxy :: Proxy GetThingShadow)

responseListNamedShadowsForThing :: ListNamedShadowsForThingResponse -> TestTree
responseListNamedShadowsForThing =
  res
    "ListNamedShadowsForThingResponse"
    "fixture/ListNamedShadowsForThingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListNamedShadowsForThing)

responseDeleteThingShadow :: DeleteThingShadowResponse -> TestTree
responseDeleteThingShadow =
  res
    "DeleteThingShadowResponse"
    "fixture/DeleteThingShadowResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteThingShadow)

responseUpdateThingShadow :: UpdateThingShadowResponse -> TestTree
responseUpdateThingShadow =
  res
    "UpdateThingShadowResponse"
    "fixture/UpdateThingShadowResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateThingShadow)

responsePublish :: PublishResponse -> TestTree
responsePublish =
  res
    "PublishResponse"
    "fixture/PublishResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy Publish)
