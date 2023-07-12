{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MarketplaceEntitlement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.MarketplaceEntitlement where

import Amazonka.MarketplaceEntitlement
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.MarketplaceEntitlement.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetEntitlements $
--             newGetEntitlements
--
--           ]

--     , testGroup "response"
--         [ responseGetEntitlements $
--             newGetEntitlementsResponse
--
--           ]
--     ]

-- Requests

requestGetEntitlements :: GetEntitlements -> TestTree
requestGetEntitlements =
  req
    "GetEntitlements"
    "fixture/GetEntitlements.yaml"

-- Responses

responseGetEntitlements :: GetEntitlementsResponse -> TestTree
responseGetEntitlements =
  res
    "GetEntitlementsResponse"
    "fixture/GetEntitlementsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEntitlements)
