{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Route53RecoveryCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Route53RecoveryCluster where

import Amazonka.Route53RecoveryCluster
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.Route53RecoveryCluster.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestUpdateRoutingControlState $
--             newUpdateRoutingControlState
--
--         , requestGetRoutingControlState $
--             newGetRoutingControlState
--
--         , requestUpdateRoutingControlStates $
--             newUpdateRoutingControlStates
--
--           ]

--     , testGroup "response"
--         [ responseUpdateRoutingControlState $
--             newUpdateRoutingControlStateResponse
--
--         , responseGetRoutingControlState $
--             newGetRoutingControlStateResponse
--
--         , responseUpdateRoutingControlStates $
--             newUpdateRoutingControlStatesResponse
--
--           ]
--     ]

-- Requests

requestUpdateRoutingControlState :: UpdateRoutingControlState -> TestTree
requestUpdateRoutingControlState =
  req
    "UpdateRoutingControlState"
    "fixture/UpdateRoutingControlState.yaml"

requestGetRoutingControlState :: GetRoutingControlState -> TestTree
requestGetRoutingControlState =
  req
    "GetRoutingControlState"
    "fixture/GetRoutingControlState.yaml"

requestUpdateRoutingControlStates :: UpdateRoutingControlStates -> TestTree
requestUpdateRoutingControlStates =
  req
    "UpdateRoutingControlStates"
    "fixture/UpdateRoutingControlStates.yaml"

-- Responses

responseUpdateRoutingControlState :: UpdateRoutingControlStateResponse -> TestTree
responseUpdateRoutingControlState =
  res
    "UpdateRoutingControlStateResponse"
    "fixture/UpdateRoutingControlStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRoutingControlState)

responseGetRoutingControlState :: GetRoutingControlStateResponse -> TestTree
responseGetRoutingControlState =
  res
    "GetRoutingControlStateResponse"
    "fixture/GetRoutingControlStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRoutingControlState)

responseUpdateRoutingControlStates :: UpdateRoutingControlStatesResponse -> TestTree
responseUpdateRoutingControlStates =
  res
    "UpdateRoutingControlStatesResponse"
    "fixture/UpdateRoutingControlStatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRoutingControlStates)
