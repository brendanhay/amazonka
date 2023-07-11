{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ElasticInference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ElasticInference where

import Amazonka.ElasticInference
import qualified Data.Proxy as Proxy
import Test.Amazonka.ElasticInference.Internal
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
--         [ requestDescribeAcceleratorOfferings $
--             newDescribeAcceleratorOfferings
--
--         , requestDescribeAcceleratorTypes $
--             newDescribeAcceleratorTypes
--
--         , requestDescribeAccelerators $
--             newDescribeAccelerators
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseDescribeAcceleratorOfferings $
--             newDescribeAcceleratorOfferingsResponse
--
--         , responseDescribeAcceleratorTypes $
--             newDescribeAcceleratorTypesResponse
--
--         , responseDescribeAccelerators $
--             newDescribeAcceleratorsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--           ]
--     ]

-- Requests

requestDescribeAcceleratorOfferings :: DescribeAcceleratorOfferings -> TestTree
requestDescribeAcceleratorOfferings =
  req
    "DescribeAcceleratorOfferings"
    "fixture/DescribeAcceleratorOfferings.yaml"

requestDescribeAcceleratorTypes :: DescribeAcceleratorTypes -> TestTree
requestDescribeAcceleratorTypes =
  req
    "DescribeAcceleratorTypes"
    "fixture/DescribeAcceleratorTypes.yaml"

requestDescribeAccelerators :: DescribeAccelerators -> TestTree
requestDescribeAccelerators =
  req
    "DescribeAccelerators"
    "fixture/DescribeAccelerators.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

-- Responses

responseDescribeAcceleratorOfferings :: DescribeAcceleratorOfferingsResponse -> TestTree
responseDescribeAcceleratorOfferings =
  res
    "DescribeAcceleratorOfferingsResponse"
    "fixture/DescribeAcceleratorOfferingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAcceleratorOfferings)

responseDescribeAcceleratorTypes :: DescribeAcceleratorTypesResponse -> TestTree
responseDescribeAcceleratorTypes =
  res
    "DescribeAcceleratorTypesResponse"
    "fixture/DescribeAcceleratorTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAcceleratorTypes)

responseDescribeAccelerators :: DescribeAcceleratorsResponse -> TestTree
responseDescribeAccelerators =
  res
    "DescribeAcceleratorsResponse"
    "fixture/DescribeAcceleratorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccelerators)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)
