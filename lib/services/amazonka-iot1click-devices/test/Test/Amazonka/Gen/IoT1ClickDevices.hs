{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.IoT1ClickDevices
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.IoT1ClickDevices where

import Amazonka.IoT1ClickDevices
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.IoT1ClickDevices.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestClaimDevicesByClaimCode $
--             newClaimDevicesByClaimCode
--
--         , requestDescribeDevice $
--             newDescribeDevice
--
--         , requestFinalizeDeviceClaim $
--             newFinalizeDeviceClaim
--
--         , requestGetDeviceMethods $
--             newGetDeviceMethods
--
--         , requestInitiateDeviceClaim $
--             newInitiateDeviceClaim
--
--         , requestInvokeDeviceMethod $
--             newInvokeDeviceMethod
--
--         , requestListDeviceEvents $
--             newListDeviceEvents
--
--         , requestListDevices $
--             newListDevices
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUnclaimDevice $
--             newUnclaimDevice
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateDeviceState $
--             newUpdateDeviceState
--
--           ]

--     , testGroup "response"
--         [ responseClaimDevicesByClaimCode $
--             newClaimDevicesByClaimCodeResponse
--
--         , responseDescribeDevice $
--             newDescribeDeviceResponse
--
--         , responseFinalizeDeviceClaim $
--             newFinalizeDeviceClaimResponse
--
--         , responseGetDeviceMethods $
--             newGetDeviceMethodsResponse
--
--         , responseInitiateDeviceClaim $
--             newInitiateDeviceClaimResponse
--
--         , responseInvokeDeviceMethod $
--             newInvokeDeviceMethodResponse
--
--         , responseListDeviceEvents $
--             newListDeviceEventsResponse
--
--         , responseListDevices $
--             newListDevicesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUnclaimDevice $
--             newUnclaimDeviceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateDeviceState $
--             newUpdateDeviceStateResponse
--
--           ]
--     ]

-- Requests

requestClaimDevicesByClaimCode :: ClaimDevicesByClaimCode -> TestTree
requestClaimDevicesByClaimCode =
  req
    "ClaimDevicesByClaimCode"
    "fixture/ClaimDevicesByClaimCode.yaml"

requestDescribeDevice :: DescribeDevice -> TestTree
requestDescribeDevice =
  req
    "DescribeDevice"
    "fixture/DescribeDevice.yaml"

requestFinalizeDeviceClaim :: FinalizeDeviceClaim -> TestTree
requestFinalizeDeviceClaim =
  req
    "FinalizeDeviceClaim"
    "fixture/FinalizeDeviceClaim.yaml"

requestGetDeviceMethods :: GetDeviceMethods -> TestTree
requestGetDeviceMethods =
  req
    "GetDeviceMethods"
    "fixture/GetDeviceMethods.yaml"

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

requestListDeviceEvents :: ListDeviceEvents -> TestTree
requestListDeviceEvents =
  req
    "ListDeviceEvents"
    "fixture/ListDeviceEvents.yaml"

requestListDevices :: ListDevices -> TestTree
requestListDevices =
  req
    "ListDevices"
    "fixture/ListDevices.yaml"

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

requestUnclaimDevice :: UnclaimDevice -> TestTree
requestUnclaimDevice =
  req
    "UnclaimDevice"
    "fixture/UnclaimDevice.yaml"

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

-- Responses

responseClaimDevicesByClaimCode :: ClaimDevicesByClaimCodeResponse -> TestTree
responseClaimDevicesByClaimCode =
  res
    "ClaimDevicesByClaimCodeResponse"
    "fixture/ClaimDevicesByClaimCodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ClaimDevicesByClaimCode)

responseDescribeDevice :: DescribeDeviceResponse -> TestTree
responseDescribeDevice =
  res
    "DescribeDeviceResponse"
    "fixture/DescribeDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDevice)

responseFinalizeDeviceClaim :: FinalizeDeviceClaimResponse -> TestTree
responseFinalizeDeviceClaim =
  res
    "FinalizeDeviceClaimResponse"
    "fixture/FinalizeDeviceClaimResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy FinalizeDeviceClaim)

responseGetDeviceMethods :: GetDeviceMethodsResponse -> TestTree
responseGetDeviceMethods =
  res
    "GetDeviceMethodsResponse"
    "fixture/GetDeviceMethodsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeviceMethods)

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

responseListDeviceEvents :: ListDeviceEventsResponse -> TestTree
responseListDeviceEvents =
  res
    "ListDeviceEventsResponse"
    "fixture/ListDeviceEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeviceEvents)

responseListDevices :: ListDevicesResponse -> TestTree
responseListDevices =
  res
    "ListDevicesResponse"
    "fixture/ListDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDevices)

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

responseUnclaimDevice :: UnclaimDeviceResponse -> TestTree
responseUnclaimDevice =
  res
    "UnclaimDeviceResponse"
    "fixture/UnclaimDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UnclaimDevice)

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
