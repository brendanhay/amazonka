{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.RBin
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.RBin where

import Amazonka.RBin
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.RBin.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateRule $
--             newCreateRule
--
--         , requestDeleteRule $
--             newDeleteRule
--
--         , requestGetRule $
--             newGetRule
--
--         , requestListRules $
--             newListRules
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestLockRule $
--             newLockRule
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUnlockRule $
--             newUnlockRule
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateRule $
--             newUpdateRule
--
--           ]

--     , testGroup "response"
--         [ responseCreateRule $
--             newCreateRuleResponse
--
--         , responseDeleteRule $
--             newDeleteRuleResponse
--
--         , responseGetRule $
--             newGetRuleResponse
--
--         , responseListRules $
--             newListRulesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseLockRule $
--             newLockRuleResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUnlockRule $
--             newUnlockRuleResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateRule $
--             newUpdateRuleResponse
--
--           ]
--     ]

-- Requests

requestCreateRule :: CreateRule -> TestTree
requestCreateRule =
  req
    "CreateRule"
    "fixture/CreateRule.yaml"

requestDeleteRule :: DeleteRule -> TestTree
requestDeleteRule =
  req
    "DeleteRule"
    "fixture/DeleteRule.yaml"

requestGetRule :: GetRule -> TestTree
requestGetRule =
  req
    "GetRule"
    "fixture/GetRule.yaml"

requestListRules :: ListRules -> TestTree
requestListRules =
  req
    "ListRules"
    "fixture/ListRules.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestLockRule :: LockRule -> TestTree
requestLockRule =
  req
    "LockRule"
    "fixture/LockRule.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUnlockRule :: UnlockRule -> TestTree
requestUnlockRule =
  req
    "UnlockRule"
    "fixture/UnlockRule.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateRule :: UpdateRule -> TestTree
requestUpdateRule =
  req
    "UpdateRule"
    "fixture/UpdateRule.yaml"

-- Responses

responseCreateRule :: CreateRuleResponse -> TestTree
responseCreateRule =
  res
    "CreateRuleResponse"
    "fixture/CreateRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRule)

responseDeleteRule :: DeleteRuleResponse -> TestTree
responseDeleteRule =
  res
    "DeleteRuleResponse"
    "fixture/DeleteRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRule)

responseGetRule :: GetRuleResponse -> TestTree
responseGetRule =
  res
    "GetRuleResponse"
    "fixture/GetRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRule)

responseListRules :: ListRulesResponse -> TestTree
responseListRules =
  res
    "ListRulesResponse"
    "fixture/ListRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRules)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseLockRule :: LockRuleResponse -> TestTree
responseLockRule =
  res
    "LockRuleResponse"
    "fixture/LockRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy LockRule)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUnlockRule :: UnlockRuleResponse -> TestTree
responseUnlockRule =
  res
    "UnlockRuleResponse"
    "fixture/UnlockRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UnlockRule)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateRule :: UpdateRuleResponse -> TestTree
responseUpdateRule =
  res
    "UpdateRuleResponse"
    "fixture/UpdateRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRule)
