{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SageMakerEdge
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.SageMakerEdge where

import Data.Proxy
import Network.AWS.SageMakerEdge
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.SageMakerEdge.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestSendHeartbeat $
--             newSendHeartbeat
--
--         , requestGetDeviceRegistration $
--             newGetDeviceRegistration
--
--           ]

--     , testGroup "response"
--         [ responseSendHeartbeat $
--             newSendHeartbeatResponse
--
--         , responseGetDeviceRegistration $
--             newGetDeviceRegistrationResponse
--
--           ]
--     ]

-- Requests

requestSendHeartbeat :: SendHeartbeat -> TestTree
requestSendHeartbeat =
  req
    "SendHeartbeat"
    "fixture/SendHeartbeat.yaml"

requestGetDeviceRegistration :: GetDeviceRegistration -> TestTree
requestGetDeviceRegistration =
  req
    "GetDeviceRegistration"
    "fixture/GetDeviceRegistration.yaml"

-- Responses

responseSendHeartbeat :: SendHeartbeatResponse -> TestTree
responseSendHeartbeat =
  res
    "SendHeartbeatResponse"
    "fixture/SendHeartbeatResponse.proto"
    defaultService
    (Proxy :: Proxy SendHeartbeat)

responseGetDeviceRegistration :: GetDeviceRegistrationResponse -> TestTree
responseGetDeviceRegistration =
  res
    "GetDeviceRegistrationResponse"
    "fixture/GetDeviceRegistrationResponse.proto"
    defaultService
    (Proxy :: Proxy GetDeviceRegistration)
