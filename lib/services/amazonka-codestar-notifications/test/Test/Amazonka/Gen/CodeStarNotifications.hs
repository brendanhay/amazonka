{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CodeStarNotifications
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CodeStarNotifications where

import Amazonka.CodeStarNotifications
import qualified Data.Proxy as Proxy
import Test.Amazonka.CodeStarNotifications.Internal
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
--         [ requestCreateNotificationRule $
--             newCreateNotificationRule
--
--         , requestDeleteNotificationRule $
--             newDeleteNotificationRule
--
--         , requestDeleteTarget $
--             newDeleteTarget
--
--         , requestDescribeNotificationRule $
--             newDescribeNotificationRule
--
--         , requestListEventTypes $
--             newListEventTypes
--
--         , requestListNotificationRules $
--             newListNotificationRules
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTargets $
--             newListTargets
--
--         , requestSubscribe $
--             newSubscribe
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUnsubscribe $
--             newUnsubscribe
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateNotificationRule $
--             newUpdateNotificationRule
--
--           ]

--     , testGroup "response"
--         [ responseCreateNotificationRule $
--             newCreateNotificationRuleResponse
--
--         , responseDeleteNotificationRule $
--             newDeleteNotificationRuleResponse
--
--         , responseDeleteTarget $
--             newDeleteTargetResponse
--
--         , responseDescribeNotificationRule $
--             newDescribeNotificationRuleResponse
--
--         , responseListEventTypes $
--             newListEventTypesResponse
--
--         , responseListNotificationRules $
--             newListNotificationRulesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTargets $
--             newListTargetsResponse
--
--         , responseSubscribe $
--             newSubscribeResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUnsubscribe $
--             newUnsubscribeResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateNotificationRule $
--             newUpdateNotificationRuleResponse
--
--           ]
--     ]

-- Requests

requestCreateNotificationRule :: CreateNotificationRule -> TestTree
requestCreateNotificationRule =
  req
    "CreateNotificationRule"
    "fixture/CreateNotificationRule.yaml"

requestDeleteNotificationRule :: DeleteNotificationRule -> TestTree
requestDeleteNotificationRule =
  req
    "DeleteNotificationRule"
    "fixture/DeleteNotificationRule.yaml"

requestDeleteTarget :: DeleteTarget -> TestTree
requestDeleteTarget =
  req
    "DeleteTarget"
    "fixture/DeleteTarget.yaml"

requestDescribeNotificationRule :: DescribeNotificationRule -> TestTree
requestDescribeNotificationRule =
  req
    "DescribeNotificationRule"
    "fixture/DescribeNotificationRule.yaml"

requestListEventTypes :: ListEventTypes -> TestTree
requestListEventTypes =
  req
    "ListEventTypes"
    "fixture/ListEventTypes.yaml"

requestListNotificationRules :: ListNotificationRules -> TestTree
requestListNotificationRules =
  req
    "ListNotificationRules"
    "fixture/ListNotificationRules.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTargets :: ListTargets -> TestTree
requestListTargets =
  req
    "ListTargets"
    "fixture/ListTargets.yaml"

requestSubscribe :: Subscribe -> TestTree
requestSubscribe =
  req
    "Subscribe"
    "fixture/Subscribe.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUnsubscribe :: Unsubscribe -> TestTree
requestUnsubscribe =
  req
    "Unsubscribe"
    "fixture/Unsubscribe.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateNotificationRule :: UpdateNotificationRule -> TestTree
requestUpdateNotificationRule =
  req
    "UpdateNotificationRule"
    "fixture/UpdateNotificationRule.yaml"

-- Responses

responseCreateNotificationRule :: CreateNotificationRuleResponse -> TestTree
responseCreateNotificationRule =
  res
    "CreateNotificationRuleResponse"
    "fixture/CreateNotificationRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNotificationRule)

responseDeleteNotificationRule :: DeleteNotificationRuleResponse -> TestTree
responseDeleteNotificationRule =
  res
    "DeleteNotificationRuleResponse"
    "fixture/DeleteNotificationRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNotificationRule)

responseDeleteTarget :: DeleteTargetResponse -> TestTree
responseDeleteTarget =
  res
    "DeleteTargetResponse"
    "fixture/DeleteTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTarget)

responseDescribeNotificationRule :: DescribeNotificationRuleResponse -> TestTree
responseDescribeNotificationRule =
  res
    "DescribeNotificationRuleResponse"
    "fixture/DescribeNotificationRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNotificationRule)

responseListEventTypes :: ListEventTypesResponse -> TestTree
responseListEventTypes =
  res
    "ListEventTypesResponse"
    "fixture/ListEventTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEventTypes)

responseListNotificationRules :: ListNotificationRulesResponse -> TestTree
responseListNotificationRules =
  res
    "ListNotificationRulesResponse"
    "fixture/ListNotificationRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNotificationRules)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTargets :: ListTargetsResponse -> TestTree
responseListTargets =
  res
    "ListTargetsResponse"
    "fixture/ListTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTargets)

responseSubscribe :: SubscribeResponse -> TestTree
responseSubscribe =
  res
    "SubscribeResponse"
    "fixture/SubscribeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Subscribe)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUnsubscribe :: UnsubscribeResponse -> TestTree
responseUnsubscribe =
  res
    "UnsubscribeResponse"
    "fixture/UnsubscribeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Unsubscribe)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateNotificationRule :: UpdateNotificationRuleResponse -> TestTree
responseUpdateNotificationRule =
  res
    "UpdateNotificationRuleResponse"
    "fixture/UpdateNotificationRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNotificationRule)
