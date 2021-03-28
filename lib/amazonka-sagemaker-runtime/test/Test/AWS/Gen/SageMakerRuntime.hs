{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SageMakerRuntime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.SageMakerRuntime where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.SageMakerRuntime
import Test.AWS.SageMakerRuntime.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestInvokeEndpoint $
--             mkInvokeEndpoint
--
--           ]

--     , testGroup "response"
--         [ responseInvokeEndpoint $
--             mkInvokeEndpointResponse
--
--           ]
--     ]

-- Requests

requestInvokeEndpoint :: InvokeEndpoint -> TestTree
requestInvokeEndpoint = req
    "InvokeEndpoint"
    "fixture/InvokeEndpoint.yaml"

-- Responses

responseInvokeEndpoint :: InvokeEndpointResponse -> TestTree
responseInvokeEndpoint = res
    "InvokeEndpointResponse"
    "fixture/InvokeEndpointResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy InvokeEndpoint)
