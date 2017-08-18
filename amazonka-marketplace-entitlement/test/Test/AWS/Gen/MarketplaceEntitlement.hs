{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MarketplaceEntitlement
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.MarketplaceEntitlement where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.MarketplaceEntitlement
import Test.AWS.MarketplaceEntitlement.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetEntitlements $
--             getEntitlements
--
--           ]

--     , testGroup "response"
--         [ responseGetEntitlements $
--             getEntitlementsResponse
--
--           ]
--     ]

-- Requests

requestGetEntitlements :: GetEntitlements -> TestTree
requestGetEntitlements = req
    "GetEntitlements"
    "fixture/GetEntitlements.yaml"

-- Responses

responseGetEntitlements :: GetEntitlementsResponse -> TestTree
responseGetEntitlements = res
    "GetEntitlementsResponse"
    "fixture/GetEntitlementsResponse.proto"
    marketplaceEntitlement
    (Proxy :: Proxy GetEntitlements)
