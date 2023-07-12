{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MarketplaceMetering
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.MarketplaceMetering where

import Amazonka.MarketplaceMetering
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.MarketplaceMetering.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchMeterUsage $
--             newBatchMeterUsage
--
--         , requestMeterUsage $
--             newMeterUsage
--
--         , requestRegisterUsage $
--             newRegisterUsage
--
--         , requestResolveCustomer $
--             newResolveCustomer
--
--           ]

--     , testGroup "response"
--         [ responseBatchMeterUsage $
--             newBatchMeterUsageResponse
--
--         , responseMeterUsage $
--             newMeterUsageResponse
--
--         , responseRegisterUsage $
--             newRegisterUsageResponse
--
--         , responseResolveCustomer $
--             newResolveCustomerResponse
--
--           ]
--     ]

-- Requests

requestBatchMeterUsage :: BatchMeterUsage -> TestTree
requestBatchMeterUsage =
  req
    "BatchMeterUsage"
    "fixture/BatchMeterUsage.yaml"

requestMeterUsage :: MeterUsage -> TestTree
requestMeterUsage =
  req
    "MeterUsage"
    "fixture/MeterUsage.yaml"

requestRegisterUsage :: RegisterUsage -> TestTree
requestRegisterUsage =
  req
    "RegisterUsage"
    "fixture/RegisterUsage.yaml"

requestResolveCustomer :: ResolveCustomer -> TestTree
requestResolveCustomer =
  req
    "ResolveCustomer"
    "fixture/ResolveCustomer.yaml"

-- Responses

responseBatchMeterUsage :: BatchMeterUsageResponse -> TestTree
responseBatchMeterUsage =
  res
    "BatchMeterUsageResponse"
    "fixture/BatchMeterUsageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchMeterUsage)

responseMeterUsage :: MeterUsageResponse -> TestTree
responseMeterUsage =
  res
    "MeterUsageResponse"
    "fixture/MeterUsageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MeterUsage)

responseRegisterUsage :: RegisterUsageResponse -> TestTree
responseRegisterUsage =
  res
    "RegisterUsageResponse"
    "fixture/RegisterUsageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterUsage)

responseResolveCustomer :: ResolveCustomerResponse -> TestTree
responseResolveCustomer =
  res
    "ResolveCustomerResponse"
    "fixture/ResolveCustomerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResolveCustomer)
