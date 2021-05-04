{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Pricing
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Pricing where

import Data.Proxy
import Network.AWS.Pricing
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.Pricing.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetProducts $
--             newGetProducts
--
--         , requestDescribeServices $
--             newDescribeServices
--
--         , requestGetAttributeValues $
--             newGetAttributeValues
--
--           ]

--     , testGroup "response"
--         [ responseGetProducts $
--             newGetProductsResponse
--
--         , responseDescribeServices $
--             newDescribeServicesResponse
--
--         , responseGetAttributeValues $
--             newGetAttributeValuesResponse
--
--           ]
--     ]

-- Requests

requestGetProducts :: GetProducts -> TestTree
requestGetProducts =
  req
    "GetProducts"
    "fixture/GetProducts.yaml"

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

-- Responses

responseGetProducts :: GetProductsResponse -> TestTree
responseGetProducts =
  res
    "GetProductsResponse"
    "fixture/GetProductsResponse.proto"
    defaultService
    (Proxy :: Proxy GetProducts)

responseDescribeServices :: DescribeServicesResponse -> TestTree
responseDescribeServices =
  res
    "DescribeServicesResponse"
    "fixture/DescribeServicesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeServices)

responseGetAttributeValues :: GetAttributeValuesResponse -> TestTree
responseGetAttributeValues =
  res
    "GetAttributeValuesResponse"
    "fixture/GetAttributeValuesResponse.proto"
    defaultService
    (Proxy :: Proxy GetAttributeValues)
