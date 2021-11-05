{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.IoT1ClickDevices
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.IoT1ClickDevices where

import Amazonka.IoT1ClickDevices
import qualified Data.Proxy as Proxy
import Test.AWS.Fixture
import Test.AWS.IoT1ClickDevices.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetDeviceMethods $
--             newGetDeviceMethods
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestClaimDevicesByClaimCode $
--             newClaimDevicesByClaimCode
--
--         , requestInitiateDeviceClaim $
--             newInitiateDeviceClaim
--
--         , requestInvokeDeviceMethod $
--             newInvokeDeviceMethod
--
--         , requestDescribeDevice $
--             newDescribeDevice
--
--         , requestListDeviceEvents $
--             newListDeviceEvents
--
--         , requestFinalizeDeviceClaim $
--             newFinalizeDeviceClaim
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateDeviceState $
--             newUpdateDeviceState
--
--         , requestUnclaimDevice $
--             newUnclaimDevice
--
--         , requestListDevices $
--             newListDevices
--
--           ]

--     , testGroup "response"
--         [ responseGetDeviceMethods $
--             newGetDeviceMethodsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseClaimDevicesByClaimCode $
--             newClaimDevicesByClaimCodeResponse
--
--         , responseInitiateDeviceClaim $
--             newInitiateDeviceClaimResponse
--
--         , responseInvokeDeviceMethod $
--             newInvokeDeviceMethodResponse
--
--         , responseDescribeDevice $
--             newDescribeDeviceResponse
--
--         , responseListDeviceEvents $
--             newListDeviceEventsResponse
--
--         , responseFinalizeDeviceClaim $
--             newFinalizeDeviceClaimResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateDeviceState $
--             newUpdateDeviceStateResponse
--
--         , responseUnclaimDevice $
--             newUnclaimDeviceResponse
--
--         , responseListDevices $
--             newListDevicesResponse
--
--           ]
--     ]

-- Requests

requestGetDeviceMethods :: GetDeviceMethods -> TestTree
requestGetDeviceMethods =
  req
    "GetDeviceMethods"
    "fixture/GetDeviceMethods.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestClaimDevicesByClaimCode :: ClaimDevicesByClaimCode -> TestTree
requestClaimDevicesByClaimCode =
  req
    "ClaimDevicesByClaimCode"
    "fixture/ClaimDevicesByClaimCode.yaml"

requestInitiateDeviceClaim :: InitiateDeviceClaim -> TestTree
requestInitiateDeviceClaim =
  req
    "InitiateDeviceClaim"
    "fixture/InitiateDeviceClaim.yaml"

requestInvokeDeviceMethod :: InvokeDeviceMethod -> TestTree
requestInvokeDeviceMethod =
  req
    "InvokeDeviceMethod"
    "fixture/InvokeDeviceMethod.yaml"

requestDescribeDevice :: DescribeDevice -> TestTree
requestDescribeDevice =
  req
    "DescribeDevice"
    "fixture/DescribeDevice.yaml"

requestListDeviceEvents :: ListDeviceEvents -> TestTree
requestListDeviceEvents =
  req
    "ListDeviceEvents"
    "fixture/ListDeviceEvents.yaml"

requestFinalizeDeviceClaim :: FinalizeDeviceClaim -> TestTree
requestFinalizeDeviceClaim =
  req
    "FinalizeDeviceClaim"
    "fixture/FinalizeDeviceClaim.yaml"

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

requestUpdateDeviceState :: UpdateDeviceState -> TestTree
requestUpdateDeviceState =
  req
    "UpdateDeviceState"
    "fixture/UpdateDeviceState.yaml"

requestUnclaimDevice :: UnclaimDevice -> TestTree
requestUnclaimDevice =
  req
    "UnclaimDevice"
    "fixture/UnclaimDevice.yaml"

requestListDevices :: ListDevices -> TestTree
requestListDevices =
  req
    "ListDevices"
    "fixture/ListDevices.yaml"

-- Responses

responseGetDeviceMethods :: GetDeviceMethodsResponse -> TestTree
responseGetDeviceMethods =
  res
    "GetDeviceMethodsResponse"
    "fixture/GetDeviceMethodsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeviceMethods)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseClaimDevicesByClaimCode :: ClaimDevicesByClaimCodeResponse -> TestTree
responseClaimDevicesByClaimCode =
  res
    "ClaimDevicesByClaimCodeResponse"
    "fixture/ClaimDevicesByClaimCodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ClaimDevicesByClaimCode)

responseInitiateDeviceClaim :: InitiateDeviceClaimResponse -> TestTree
responseInitiateDeviceClaim =
  res
    "InitiateDeviceClaimResponse"
    "fixture/InitiateDeviceClaimResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InitiateDeviceClaim)

responseInvokeDeviceMethod :: InvokeDeviceMethodResponse -> TestTree
responseInvokeDeviceMethod =
  res
    "InvokeDeviceMethodResponse"
    "fixture/InvokeDeviceMethodResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InvokeDeviceMethod)

responseDescribeDevice :: DescribeDeviceResponse -> TestTree
responseDescribeDevice =
  res
    "DescribeDeviceResponse"
    "fixture/DescribeDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDevice)

responseListDeviceEvents :: ListDeviceEventsResponse -> TestTree
responseListDeviceEvents =
  res
    "ListDeviceEventsResponse"
    "fixture/ListDeviceEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeviceEvents)

responseFinalizeDeviceClaim :: FinalizeDeviceClaimResponse -> TestTree
responseFinalizeDeviceClaim =
  res
    "FinalizeDeviceClaimResponse"
    "fixture/FinalizeDeviceClaimResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy FinalizeDeviceClaim)

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

responseUpdateDeviceState :: UpdateDeviceStateResponse -> TestTree
responseUpdateDeviceState =
  res
    "UpdateDeviceStateResponse"
    "fixture/UpdateDeviceStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDeviceState)

responseUnclaimDevice :: UnclaimDeviceResponse -> TestTree
responseUnclaimDevice =
  res
    "UnclaimDeviceResponse"
    "fixture/UnclaimDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UnclaimDevice)

responseListDevices :: ListDevicesResponse -> TestTree
responseListDevices =
  res
    "ListDevicesResponse"
    "fixture/ListDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDevices)
