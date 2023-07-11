{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Route53RecoveryCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
--         [ requestGetRoutingControlState $
--             newGetRoutingControlState
--
--         , requestListRoutingControls $
--             newListRoutingControls
--
--         , requestUpdateRoutingControlState $
--             newUpdateRoutingControlState
--
--         , requestUpdateRoutingControlStates $
--             newUpdateRoutingControlStates
--
--           ]

--     , testGroup "response"
--         [ responseGetRoutingControlState $
--             newGetRoutingControlStateResponse
--
--         , responseListRoutingControls $
--             newListRoutingControlsResponse
--
--         , responseUpdateRoutingControlState $
--             newUpdateRoutingControlStateResponse
--
--         , responseUpdateRoutingControlStates $
--             newUpdateRoutingControlStatesResponse
--
--           ]
--     ]

-- Requests

requestGetRoutingControlState :: GetRoutingControlState -> TestTree
requestGetRoutingControlState =
  req
    "GetRoutingControlState"
    "fixture/GetRoutingControlState.yaml"

requestListRoutingControls :: ListRoutingControls -> TestTree
requestListRoutingControls =
  req
    "ListRoutingControls"
    "fixture/ListRoutingControls.yaml"

requestUpdateRoutingControlState :: UpdateRoutingControlState -> TestTree
requestUpdateRoutingControlState =
  req
    "UpdateRoutingControlState"
    "fixture/UpdateRoutingControlState.yaml"

requestUpdateRoutingControlStates :: UpdateRoutingControlStates -> TestTree
requestUpdateRoutingControlStates =
  req
    "UpdateRoutingControlStates"
    "fixture/UpdateRoutingControlStates.yaml"

-- Responses

responseGetRoutingControlState :: GetRoutingControlStateResponse -> TestTree
responseGetRoutingControlState =
  res
    "GetRoutingControlStateResponse"
    "fixture/GetRoutingControlStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRoutingControlState)

responseListRoutingControls :: ListRoutingControlsResponse -> TestTree
responseListRoutingControls =
  res
    "ListRoutingControlsResponse"
    "fixture/ListRoutingControlsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRoutingControls)

responseUpdateRoutingControlState :: UpdateRoutingControlStateResponse -> TestTree
responseUpdateRoutingControlState =
  res
    "UpdateRoutingControlStateResponse"
    "fixture/UpdateRoutingControlStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRoutingControlState)

responseUpdateRoutingControlStates :: UpdateRoutingControlStatesResponse -> TestTree
responseUpdateRoutingControlStates =
  res
    "UpdateRoutingControlStatesResponse"
    "fixture/UpdateRoutingControlStatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRoutingControlStates)
