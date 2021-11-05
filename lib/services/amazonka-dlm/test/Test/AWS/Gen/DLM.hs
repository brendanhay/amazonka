{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DLM
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.DLM where

import Amazonka.DLM
import qualified Data.Proxy as Proxy
import Test.AWS.DLM.Internal
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
--         [ requestDeleteLifecyclePolicy $
--             newDeleteLifecyclePolicy
--
--         , requestUpdateLifecyclePolicy $
--             newUpdateLifecyclePolicy
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreateLifecyclePolicy $
--             newCreateLifecyclePolicy
--
--         , requestGetLifecyclePolicy $
--             newGetLifecyclePolicy
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestGetLifecyclePolicies $
--             newGetLifecyclePolicies
--
--           ]

--     , testGroup "response"
--         [ responseDeleteLifecyclePolicy $
--             newDeleteLifecyclePolicyResponse
--
--         , responseUpdateLifecyclePolicy $
--             newUpdateLifecyclePolicyResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreateLifecyclePolicy $
--             newCreateLifecyclePolicyResponse
--
--         , responseGetLifecyclePolicy $
--             newGetLifecyclePolicyResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseGetLifecyclePolicies $
--             newGetLifecyclePoliciesResponse
--
--           ]
--     ]

-- Requests

requestDeleteLifecyclePolicy :: DeleteLifecyclePolicy -> TestTree
requestDeleteLifecyclePolicy =
  req
    "DeleteLifecyclePolicy"
    "fixture/DeleteLifecyclePolicy.yaml"

requestUpdateLifecyclePolicy :: UpdateLifecyclePolicy -> TestTree
requestUpdateLifecyclePolicy =
  req
    "UpdateLifecyclePolicy"
    "fixture/UpdateLifecyclePolicy.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreateLifecyclePolicy :: CreateLifecyclePolicy -> TestTree
requestCreateLifecyclePolicy =
  req
    "CreateLifecyclePolicy"
    "fixture/CreateLifecyclePolicy.yaml"

requestGetLifecyclePolicy :: GetLifecyclePolicy -> TestTree
requestGetLifecyclePolicy =
  req
    "GetLifecyclePolicy"
    "fixture/GetLifecyclePolicy.yaml"

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

requestGetLifecyclePolicies :: GetLifecyclePolicies -> TestTree
requestGetLifecyclePolicies =
  req
    "GetLifecyclePolicies"
    "fixture/GetLifecyclePolicies.yaml"

-- Responses

responseDeleteLifecyclePolicy :: DeleteLifecyclePolicyResponse -> TestTree
responseDeleteLifecyclePolicy =
  res
    "DeleteLifecyclePolicyResponse"
    "fixture/DeleteLifecyclePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLifecyclePolicy)

responseUpdateLifecyclePolicy :: UpdateLifecyclePolicyResponse -> TestTree
responseUpdateLifecyclePolicy =
  res
    "UpdateLifecyclePolicyResponse"
    "fixture/UpdateLifecyclePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLifecyclePolicy)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseCreateLifecyclePolicy :: CreateLifecyclePolicyResponse -> TestTree
responseCreateLifecyclePolicy =
  res
    "CreateLifecyclePolicyResponse"
    "fixture/CreateLifecyclePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLifecyclePolicy)

responseGetLifecyclePolicy :: GetLifecyclePolicyResponse -> TestTree
responseGetLifecyclePolicy =
  res
    "GetLifecyclePolicyResponse"
    "fixture/GetLifecyclePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLifecyclePolicy)

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

responseGetLifecyclePolicies :: GetLifecyclePoliciesResponse -> TestTree
responseGetLifecyclePolicies =
  res
    "GetLifecyclePoliciesResponse"
    "fixture/GetLifecyclePoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLifecyclePolicies)
