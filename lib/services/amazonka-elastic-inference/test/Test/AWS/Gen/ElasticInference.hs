{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ElasticInference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.ElasticInference where

import qualified Data.Proxy as Proxy
import Network.AWS.ElasticInference
import Test.AWS.ElasticInference.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDescribeAcceleratorOfferings $
--             newDescribeAcceleratorOfferings
--
--         , requestDescribeAccelerators $
--             newDescribeAccelerators
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeAcceleratorTypes $
--             newDescribeAcceleratorTypes
--
--           ]

--     , testGroup "response"
--         [ responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDescribeAcceleratorOfferings $
--             newDescribeAcceleratorOfferingsResponse
--
--         , responseDescribeAccelerators $
--             newDescribeAcceleratorsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeAcceleratorTypes $
--             newDescribeAcceleratorTypesResponse
--
--           ]
--     ]

-- Requests

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDescribeAcceleratorOfferings :: DescribeAcceleratorOfferings -> TestTree
requestDescribeAcceleratorOfferings =
  req
    "DescribeAcceleratorOfferings"
    "fixture/DescribeAcceleratorOfferings.yaml"

requestDescribeAccelerators :: DescribeAccelerators -> TestTree
requestDescribeAccelerators =
  req
    "DescribeAccelerators"
    "fixture/DescribeAccelerators.yaml"

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

requestDescribeAcceleratorTypes :: DescribeAcceleratorTypes -> TestTree
requestDescribeAcceleratorTypes =
  req
    "DescribeAcceleratorTypes"
    "fixture/DescribeAcceleratorTypes.yaml"

-- Responses

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseDescribeAcceleratorOfferings :: DescribeAcceleratorOfferingsResponse -> TestTree
responseDescribeAcceleratorOfferings =
  res
    "DescribeAcceleratorOfferingsResponse"
    "fixture/DescribeAcceleratorOfferingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAcceleratorOfferings)

responseDescribeAccelerators :: DescribeAcceleratorsResponse -> TestTree
responseDescribeAccelerators =
  res
    "DescribeAcceleratorsResponse"
    "fixture/DescribeAcceleratorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccelerators)

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

responseDescribeAcceleratorTypes :: DescribeAcceleratorTypesResponse -> TestTree
responseDescribeAcceleratorTypes =
  res
    "DescribeAcceleratorTypesResponse"
    "fixture/DescribeAcceleratorTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAcceleratorTypes)
