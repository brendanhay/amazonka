{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ControlTower
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ControlTower where

import Amazonka.ControlTower
import qualified Data.Proxy as Proxy
import Test.Amazonka.ControlTower.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDisableControl $
--             newDisableControl
--
--         , requestEnableControl $
--             newEnableControl
--
--         , requestGetControlOperation $
--             newGetControlOperation
--
--         , requestListEnabledControls $
--             newListEnabledControls
--
--           ]

--     , testGroup "response"
--         [ responseDisableControl $
--             newDisableControlResponse
--
--         , responseEnableControl $
--             newEnableControlResponse
--
--         , responseGetControlOperation $
--             newGetControlOperationResponse
--
--         , responseListEnabledControls $
--             newListEnabledControlsResponse
--
--           ]
--     ]

-- Requests

requestDisableControl :: DisableControl -> TestTree
requestDisableControl =
  req
    "DisableControl"
    "fixture/DisableControl.yaml"

requestEnableControl :: EnableControl -> TestTree
requestEnableControl =
  req
    "EnableControl"
    "fixture/EnableControl.yaml"

requestGetControlOperation :: GetControlOperation -> TestTree
requestGetControlOperation =
  req
    "GetControlOperation"
    "fixture/GetControlOperation.yaml"

requestListEnabledControls :: ListEnabledControls -> TestTree
requestListEnabledControls =
  req
    "ListEnabledControls"
    "fixture/ListEnabledControls.yaml"

-- Responses

responseDisableControl :: DisableControlResponse -> TestTree
responseDisableControl =
  res
    "DisableControlResponse"
    "fixture/DisableControlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableControl)

responseEnableControl :: EnableControlResponse -> TestTree
responseEnableControl =
  res
    "EnableControlResponse"
    "fixture/EnableControlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableControl)

responseGetControlOperation :: GetControlOperationResponse -> TestTree
responseGetControlOperation =
  res
    "GetControlOperationResponse"
    "fixture/GetControlOperationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetControlOperation)

responseListEnabledControls :: ListEnabledControlsResponse -> TestTree
responseListEnabledControls =
  res
    "ListEnabledControlsResponse"
    "fixture/ListEnabledControlsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEnabledControls)
