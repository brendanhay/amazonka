{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MarketplaceMetering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.MarketplaceMetering where

import Data.Proxy
import Network.AWS.MarketplaceMetering
import Test.AWS.Fixture
import Test.AWS.MarketplaceMetering.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestRegisterUsage $
--             mkRegisterUsage
--
--         , requestBatchMeterUsage $
--             mkBatchMeterUsage
--
--         , requestResolveCustomer $
--             mkResolveCustomer
--
--         , requestMeterUsage $
--             mkMeterUsage
--
--           ]

--     , testGroup "response"
--         [ responseRegisterUsage $
--             mkRegisterUsageResponse
--
--         , responseBatchMeterUsage $
--             mkBatchMeterUsageResponse
--
--         , responseResolveCustomer $
--             mkResolveCustomerResponse
--
--         , responseMeterUsage $
--             mkMeterUsageResponse
--
--           ]
--     ]

-- Requests

requestRegisterUsage :: RegisterUsage -> TestTree
requestRegisterUsage =
  req
    "RegisterUsage"
    "fixture/RegisterUsage.yaml"

requestBatchMeterUsage :: BatchMeterUsage -> TestTree
requestBatchMeterUsage =
  req
    "BatchMeterUsage"
    "fixture/BatchMeterUsage.yaml"

requestResolveCustomer :: ResolveCustomer -> TestTree
requestResolveCustomer =
  req
    "ResolveCustomer"
    "fixture/ResolveCustomer.yaml"

requestMeterUsage :: MeterUsage -> TestTree
requestMeterUsage =
  req
    "MeterUsage"
    "fixture/MeterUsage.yaml"

-- Responses

responseRegisterUsage :: RegisterUsageResponse -> TestTree
responseRegisterUsage =
  res
    "RegisterUsageResponse"
    "fixture/RegisterUsageResponse.proto"
    marketplaceMeteringService
    (Proxy :: Proxy RegisterUsage)

responseBatchMeterUsage :: BatchMeterUsageResponse -> TestTree
responseBatchMeterUsage =
  res
    "BatchMeterUsageResponse"
    "fixture/BatchMeterUsageResponse.proto"
    marketplaceMeteringService
    (Proxy :: Proxy BatchMeterUsage)

responseResolveCustomer :: ResolveCustomerResponse -> TestTree
responseResolveCustomer =
  res
    "ResolveCustomerResponse"
    "fixture/ResolveCustomerResponse.proto"
    marketplaceMeteringService
    (Proxy :: Proxy ResolveCustomer)

responseMeterUsage :: MeterUsageResponse -> TestTree
responseMeterUsage =
  res
    "MeterUsageResponse"
    "fixture/MeterUsageResponse.proto"
    marketplaceMeteringService
    (Proxy :: Proxy MeterUsage)
