{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.IoTDataPlane
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.IoTDataPlane where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.IoTDataPlane
import Test.AWS.IoTDataPlane.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testGetThingShadow $
--             getThingShadow
--
--         , testDeleteThingShadow $
--             deleteThingShadow
--
--         , testUpdateThingShadow $
--             updateThingShadow
--
--         , testPublish $
--             publish
--
--           ]

--     , testGroup "response"
--         [ testGetThingShadowResponse $
--             getThingShadowResponse
--
--         , testDeleteThingShadowResponse $
--             deleteThingShadowResponse
--
--         , testUpdateThingShadowResponse $
--             updateThingShadowResponse
--
--         , testPublishResponse $
--             publishResponse
--
--           ]
--     ]

-- Requests

testGetThingShadow :: GetThingShadow -> TestTree
testGetThingShadow = req
    "GetThingShadow"
    "fixture/GetThingShadow.yaml"

testDeleteThingShadow :: DeleteThingShadow -> TestTree
testDeleteThingShadow = req
    "DeleteThingShadow"
    "fixture/DeleteThingShadow.yaml"

testUpdateThingShadow :: UpdateThingShadow -> TestTree
testUpdateThingShadow = req
    "UpdateThingShadow"
    "fixture/UpdateThingShadow.yaml"

testPublish :: Publish -> TestTree
testPublish = req
    "Publish"
    "fixture/Publish.yaml"

-- Responses

testGetThingShadowResponse :: GetThingShadowResponse -> TestTree
testGetThingShadowResponse = res
    "GetThingShadowResponse"
    "fixture/GetThingShadowResponse.proto"
    ioTDataPlane
    (Proxy :: Proxy GetThingShadow)

testDeleteThingShadowResponse :: DeleteThingShadowResponse -> TestTree
testDeleteThingShadowResponse = res
    "DeleteThingShadowResponse"
    "fixture/DeleteThingShadowResponse.proto"
    ioTDataPlane
    (Proxy :: Proxy DeleteThingShadow)

testUpdateThingShadowResponse :: UpdateThingShadowResponse -> TestTree
testUpdateThingShadowResponse = res
    "UpdateThingShadowResponse"
    "fixture/UpdateThingShadowResponse.proto"
    ioTDataPlane
    (Proxy :: Proxy UpdateThingShadow)

testPublishResponse :: PublishResponse -> TestTree
testPublishResponse = res
    "PublishResponse"
    "fixture/PublishResponse.proto"
    ioTDataPlane
    (Proxy :: Proxy Publish)
