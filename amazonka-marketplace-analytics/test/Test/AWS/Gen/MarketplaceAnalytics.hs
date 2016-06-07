{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MarketplaceAnalytics
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.MarketplaceAnalytics where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.MarketplaceAnalytics
import Test.AWS.MarketplaceAnalytics.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGenerateDataSet $
--             generateDataSet
--
--           ]

--     , testGroup "response"
--         [ responseGenerateDataSet $
--             generateDataSetResponse
--
--           ]
--     ]

-- Requests

requestGenerateDataSet :: GenerateDataSet -> TestTree
requestGenerateDataSet = req
    "GenerateDataSet"
    "fixture/GenerateDataSet.yaml"

-- Responses

responseGenerateDataSet :: GenerateDataSetResponse -> TestTree
responseGenerateDataSet = res
    "GenerateDataSetResponse"
    "fixture/GenerateDataSetResponse.proto"
    marketplaceAnalytics
    (Proxy :: Proxy GenerateDataSet)
