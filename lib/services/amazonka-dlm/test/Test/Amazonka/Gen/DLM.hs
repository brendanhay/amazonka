{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.DLM
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.DLM where

import Amazonka.DLM
import qualified Data.Proxy as Proxy
import Test.Amazonka.DLM.Internal
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
--         [ requestCreateLifecyclePolicy $
--             newCreateLifecyclePolicy
--
--         , requestDeleteLifecyclePolicy $
--             newDeleteLifecyclePolicy
--
--         , requestGetLifecyclePolicies $
--             newGetLifecyclePolicies
--
--         , requestGetLifecyclePolicy $
--             newGetLifecyclePolicy
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
--         , requestUpdateLifecyclePolicy $
--             newUpdateLifecyclePolicy
--
--           ]

--     , testGroup "response"
--         [ responseCreateLifecyclePolicy $
--             newCreateLifecyclePolicyResponse
--
--         , responseDeleteLifecyclePolicy $
--             newDeleteLifecyclePolicyResponse
--
--         , responseGetLifecyclePolicies $
--             newGetLifecyclePoliciesResponse
--
--         , responseGetLifecyclePolicy $
--             newGetLifecyclePolicyResponse
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
--         , responseUpdateLifecyclePolicy $
--             newUpdateLifecyclePolicyResponse
--
--           ]
--     ]

-- Requests

requestCreateLifecyclePolicy :: CreateLifecyclePolicy -> TestTree
requestCreateLifecyclePolicy =
  req
    "CreateLifecyclePolicy"
    "fixture/CreateLifecyclePolicy.yaml"

requestDeleteLifecyclePolicy :: DeleteLifecyclePolicy -> TestTree
requestDeleteLifecyclePolicy =
  req
    "DeleteLifecyclePolicy"
    "fixture/DeleteLifecyclePolicy.yaml"

requestGetLifecyclePolicies :: GetLifecyclePolicies -> TestTree
requestGetLifecyclePolicies =
  req
    "GetLifecyclePolicies"
    "fixture/GetLifecyclePolicies.yaml"

requestGetLifecyclePolicy :: GetLifecyclePolicy -> TestTree
requestGetLifecyclePolicy =
  req
    "GetLifecyclePolicy"
    "fixture/GetLifecyclePolicy.yaml"

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

requestUpdateLifecyclePolicy :: UpdateLifecyclePolicy -> TestTree
requestUpdateLifecyclePolicy =
  req
    "UpdateLifecyclePolicy"
    "fixture/UpdateLifecyclePolicy.yaml"

-- Responses

responseCreateLifecyclePolicy :: CreateLifecyclePolicyResponse -> TestTree
responseCreateLifecyclePolicy =
  res
    "CreateLifecyclePolicyResponse"
    "fixture/CreateLifecyclePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLifecyclePolicy)

responseDeleteLifecyclePolicy :: DeleteLifecyclePolicyResponse -> TestTree
responseDeleteLifecyclePolicy =
  res
    "DeleteLifecyclePolicyResponse"
    "fixture/DeleteLifecyclePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLifecyclePolicy)

responseGetLifecyclePolicies :: GetLifecyclePoliciesResponse -> TestTree
responseGetLifecyclePolicies =
  res
    "GetLifecyclePoliciesResponse"
    "fixture/GetLifecyclePoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLifecyclePolicies)

responseGetLifecyclePolicy :: GetLifecyclePolicyResponse -> TestTree
responseGetLifecyclePolicy =
  res
    "GetLifecyclePolicyResponse"
    "fixture/GetLifecyclePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLifecyclePolicy)

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

responseUpdateLifecyclePolicy :: UpdateLifecyclePolicyResponse -> TestTree
responseUpdateLifecyclePolicy =
  res
    "UpdateLifecyclePolicyResponse"
    "fixture/UpdateLifecyclePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLifecyclePolicy)
