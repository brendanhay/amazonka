{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SageMakerEdge
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.SageMakerEdge where

import Amazonka.SageMakerEdge
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.SageMakerEdge.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetDeployments $
--             newGetDeployments
--
--         , requestGetDeviceRegistration $
--             newGetDeviceRegistration
--
--         , requestSendHeartbeat $
--             newSendHeartbeat
--
--           ]

--     , testGroup "response"
--         [ responseGetDeployments $
--             newGetDeploymentsResponse
--
--         , responseGetDeviceRegistration $
--             newGetDeviceRegistrationResponse
--
--         , responseSendHeartbeat $
--             newSendHeartbeatResponse
--
--           ]
--     ]

-- Requests

requestGetDeployments :: GetDeployments -> TestTree
requestGetDeployments =
  req
    "GetDeployments"
    "fixture/GetDeployments.yaml"

requestGetDeviceRegistration :: GetDeviceRegistration -> TestTree
requestGetDeviceRegistration =
  req
    "GetDeviceRegistration"
    "fixture/GetDeviceRegistration.yaml"

requestSendHeartbeat :: SendHeartbeat -> TestTree
requestSendHeartbeat =
  req
    "SendHeartbeat"
    "fixture/SendHeartbeat.yaml"

-- Responses

responseGetDeployments :: GetDeploymentsResponse -> TestTree
responseGetDeployments =
  res
    "GetDeploymentsResponse"
    "fixture/GetDeploymentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeployments)

responseGetDeviceRegistration :: GetDeviceRegistrationResponse -> TestTree
responseGetDeviceRegistration =
  res
    "GetDeviceRegistrationResponse"
    "fixture/GetDeviceRegistrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeviceRegistration)

responseSendHeartbeat :: SendHeartbeatResponse -> TestTree
responseSendHeartbeat =
  res
    "SendHeartbeatResponse"
    "fixture/SendHeartbeatResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendHeartbeat)
