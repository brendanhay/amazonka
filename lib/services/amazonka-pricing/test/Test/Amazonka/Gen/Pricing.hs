{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Pricing
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Pricing where

import Amazonka.Pricing
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.Pricing.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribeServices $
--             newDescribeServices
--
--         , requestGetAttributeValues $
--             newGetAttributeValues
--
--         , requestGetProducts $
--             newGetProducts
--
--           ]

--     , testGroup "response"
--         [ responseDescribeServices $
--             newDescribeServicesResponse
--
--         , responseGetAttributeValues $
--             newGetAttributeValuesResponse
--
--         , responseGetProducts $
--             newGetProductsResponse
--
--           ]
--     ]

-- Requests

requestDescribeServices :: DescribeServices -> TestTree
requestDescribeServices =
  req
    "DescribeServices"
    "fixture/DescribeServices.yaml"

requestGetAttributeValues :: GetAttributeValues -> TestTree
requestGetAttributeValues =
  req
    "GetAttributeValues"
    "fixture/GetAttributeValues.yaml"

requestGetProducts :: GetProducts -> TestTree
requestGetProducts =
  req
    "GetProducts"
    "fixture/GetProducts.yaml"

-- Responses

responseDescribeServices :: DescribeServicesResponse -> TestTree
responseDescribeServices =
  res
    "DescribeServicesResponse"
    "fixture/DescribeServicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeServices)

responseGetAttributeValues :: GetAttributeValuesResponse -> TestTree
responseGetAttributeValues =
  res
    "GetAttributeValuesResponse"
    "fixture/GetAttributeValuesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAttributeValues)

responseGetProducts :: GetProductsResponse -> TestTree
responseGetProducts =
  res
    "GetProductsResponse"
    "fixture/GetProductsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetProducts)
