{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.IoTDataPlane
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.IoTDataPlane where

import Data.Proxy
import Network.AWS.IoTDataPlane
import Test.AWS.Fixture
import Test.AWS.IoTDataPlane.Internal
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
--             getThingShadow
--
--         , requestDeleteThingShadow $
--             deleteThingShadow
--
--         , requestUpdateThingShadow $
--             updateThingShadow
--
--         , requestPublish $
--             publish
--
--           ]

--     , testGroup "response"
--         [ responseGetThingShadow $
--             getThingShadowResponse
--
--         , responseDeleteThingShadow $
--             deleteThingShadowResponse
--
--         , responseUpdateThingShadow $
--             updateThingShadowResponse
--
--         , responsePublish $
--             publishResponse
--
--           ]
--     ]

-- Requests

requestGetThingShadow :: GetThingShadow -> TestTree
requestGetThingShadow = req
    "GetThingShadow"
    "fixture/GetThingShadow.yaml"

requestDeleteThingShadow :: DeleteThingShadow -> TestTree
requestDeleteThingShadow = req
    "DeleteThingShadow"
    "fixture/DeleteThingShadow.yaml"

requestUpdateThingShadow :: UpdateThingShadow -> TestTree
requestUpdateThingShadow = req
    "UpdateThingShadow"
    "fixture/UpdateThingShadow.yaml"

requestPublish :: Publish -> TestTree
requestPublish = req
    "Publish"
    "fixture/Publish.yaml"

-- Responses

responseGetThingShadow :: GetThingShadowResponse -> TestTree
responseGetThingShadow = res
    "GetThingShadowResponse"
    "fixture/GetThingShadowResponse.proto"
    ioTDataPlane
    (Proxy :: Proxy GetThingShadow)

responseDeleteThingShadow :: DeleteThingShadowResponse -> TestTree
responseDeleteThingShadow = res
    "DeleteThingShadowResponse"
    "fixture/DeleteThingShadowResponse.proto"
    ioTDataPlane
    (Proxy :: Proxy DeleteThingShadow)

responseUpdateThingShadow :: UpdateThingShadowResponse -> TestTree
responseUpdateThingShadow = res
    "UpdateThingShadowResponse"
    "fixture/UpdateThingShadowResponse.proto"
    ioTDataPlane
    (Proxy :: Proxy UpdateThingShadow)

responsePublish :: PublishResponse -> TestTree
responsePublish = res
    "PublishResponse"
    "fixture/PublishResponse.proto"
    ioTDataPlane
    (Proxy :: Proxy Publish)
