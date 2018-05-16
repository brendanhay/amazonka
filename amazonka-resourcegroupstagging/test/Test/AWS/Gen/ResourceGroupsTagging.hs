{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ResourceGroupsTagging
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.ResourceGroupsTagging where

import Data.Proxy
import Network.AWS.ResourceGroupsTagging
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.ResourceGroupsTagging.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetTagKeys $
--             getTagKeys
--
--         , requestTagResources $
--             tagResources
--
--         , requestGetTagValues $
--             getTagValues
--
--         , requestGetResources $
--             getResources
--
--         , requestUntagResources $
--             untagResources
--
--           ]

--     , testGroup "response"
--         [ responseGetTagKeys $
--             getTagKeysResponse
--
--         , responseTagResources $
--             tagResourcesResponse
--
--         , responseGetTagValues $
--             getTagValuesResponse
--
--         , responseGetResources $
--             getResourcesResponse
--
--         , responseUntagResources $
--             untagResourcesResponse
--
--           ]
--     ]

-- Requests

requestGetTagKeys :: GetTagKeys -> TestTree
requestGetTagKeys = req
    "GetTagKeys"
    "fixture/GetTagKeys.yaml"

requestTagResources :: TagResources -> TestTree
requestTagResources = req
    "TagResources"
    "fixture/TagResources.yaml"

requestGetTagValues :: GetTagValues -> TestTree
requestGetTagValues = req
    "GetTagValues"
    "fixture/GetTagValues.yaml"

requestGetResources :: GetResources -> TestTree
requestGetResources = req
    "GetResources"
    "fixture/GetResources.yaml"

requestUntagResources :: UntagResources -> TestTree
requestUntagResources = req
    "UntagResources"
    "fixture/UntagResources.yaml"

-- Responses

responseGetTagKeys :: GetTagKeysResponse -> TestTree
responseGetTagKeys = res
    "GetTagKeysResponse"
    "fixture/GetTagKeysResponse.proto"
    resourceGroupsTagging
    (Proxy :: Proxy GetTagKeys)

responseTagResources :: TagResourcesResponse -> TestTree
responseTagResources = res
    "TagResourcesResponse"
    "fixture/TagResourcesResponse.proto"
    resourceGroupsTagging
    (Proxy :: Proxy TagResources)

responseGetTagValues :: GetTagValuesResponse -> TestTree
responseGetTagValues = res
    "GetTagValuesResponse"
    "fixture/GetTagValuesResponse.proto"
    resourceGroupsTagging
    (Proxy :: Proxy GetTagValues)

responseGetResources :: GetResourcesResponse -> TestTree
responseGetResources = res
    "GetResourcesResponse"
    "fixture/GetResourcesResponse.proto"
    resourceGroupsTagging
    (Proxy :: Proxy GetResources)

responseUntagResources :: UntagResourcesResponse -> TestTree
responseUntagResources = res
    "UntagResourcesResponse"
    "fixture/UntagResourcesResponse.proto"
    resourceGroupsTagging
    (Proxy :: Proxy UntagResources)
