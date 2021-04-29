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
--             newGetThingShadow
--
--         , requestPublish $
--             newPublish
--
--         , requestListNamedShadowsForThing $
--             newListNamedShadowsForThing
--
--         , requestUpdateThingShadow $
--             newUpdateThingShadow
--
--         , requestDeleteThingShadow $
--             newDeleteThingShadow
--
--           ]

--     , testGroup "response"
--         [ responseGetThingShadow $
--             newGetThingShadowResponse
--
--         , responsePublish $
--             newPublishResponse
--
--         , responseListNamedShadowsForThing $
--             newListNamedShadowsForThingResponse
--
--         , responseUpdateThingShadow $
--             newUpdateThingShadowResponse
--
--         , responseDeleteThingShadow $
--             newDeleteThingShadowResponse
--
--           ]
--     ]

-- Requests

requestGetThingShadow :: GetThingShadow -> TestTree
requestGetThingShadow =
  req
    "GetThingShadow"
    "fixture/GetThingShadow.yaml"

requestPublish :: Publish -> TestTree
requestPublish =
  req
    "Publish"
    "fixture/Publish.yaml"

requestListNamedShadowsForThing :: ListNamedShadowsForThing -> TestTree
requestListNamedShadowsForThing =
  req
    "ListNamedShadowsForThing"
    "fixture/ListNamedShadowsForThing.yaml"

requestUpdateThingShadow :: UpdateThingShadow -> TestTree
requestUpdateThingShadow =
  req
    "UpdateThingShadow"
    "fixture/UpdateThingShadow.yaml"

requestDeleteThingShadow :: DeleteThingShadow -> TestTree
requestDeleteThingShadow =
  req
    "DeleteThingShadow"
    "fixture/DeleteThingShadow.yaml"

-- Responses

responseGetThingShadow :: GetThingShadowResponse -> TestTree
responseGetThingShadow =
  res
    "GetThingShadowResponse"
    "fixture/GetThingShadowResponse.proto"
    defaultService
    (Proxy :: Proxy GetThingShadow)

responsePublish :: PublishResponse -> TestTree
responsePublish =
  res
    "PublishResponse"
    "fixture/PublishResponse.proto"
    defaultService
    (Proxy :: Proxy Publish)

responseListNamedShadowsForThing :: ListNamedShadowsForThingResponse -> TestTree
responseListNamedShadowsForThing =
  res
    "ListNamedShadowsForThingResponse"
    "fixture/ListNamedShadowsForThingResponse.proto"
    defaultService
    (Proxy :: Proxy ListNamedShadowsForThing)

responseUpdateThingShadow :: UpdateThingShadowResponse -> TestTree
responseUpdateThingShadow =
  res
    "UpdateThingShadowResponse"
    "fixture/UpdateThingShadowResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateThingShadow)

responseDeleteThingShadow :: DeleteThingShadowResponse -> TestTree
responseDeleteThingShadow =
  res
    "DeleteThingShadowResponse"
    "fixture/DeleteThingShadowResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteThingShadow)
