{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CodeStarNotifications
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         , requestUpdateNotificationRule $
--             newUpdateNotificationRule
--
--         , requestDeleteNotificationRule $
--             newDeleteNotificationRule
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListEventTypes $
--             newListEventTypes
--
--         , requestDeleteTarget $
--             newDeleteTarget
--
--         , requestListNotificationRules $
--             newListNotificationRules
--
--         , requestListTargets $
--             newListTargets
--
--         , requestTagResource $
--             newTagResource
--
--         , requestSubscribe $
--             newSubscribe
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUnsubscribe $
--             newUnsubscribe
--
--         , requestDescribeNotificationRule $
--             newDescribeNotificationRule
--
--           ]

--     , testGroup "response"
--         [ responseCreateNotificationRule $
--             newCreateNotificationRuleResponse
--
--         , responseUpdateNotificationRule $
--             newUpdateNotificationRuleResponse
--
--         , responseDeleteNotificationRule $
--             newDeleteNotificationRuleResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListEventTypes $
--             newListEventTypesResponse
--
--         , responseDeleteTarget $
--             newDeleteTargetResponse
--
--         , responseListNotificationRules $
--             newListNotificationRulesResponse
--
--         , responseListTargets $
--             newListTargetsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseSubscribe $
--             newSubscribeResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUnsubscribe $
--             newUnsubscribeResponse
--
--         , responseDescribeNotificationRule $
--             newDescribeNotificationRuleResponse
--
--           ]
--     ]

-- Requests

requestCreateNotificationRule :: CreateNotificationRule -> TestTree
requestCreateNotificationRule =
  req
    "CreateNotificationRule"
    "fixture/CreateNotificationRule.yaml"

requestUpdateNotificationRule :: UpdateNotificationRule -> TestTree
requestUpdateNotificationRule =
  req
    "UpdateNotificationRule"
    "fixture/UpdateNotificationRule.yaml"

requestDeleteNotificationRule :: DeleteNotificationRule -> TestTree
requestDeleteNotificationRule =
  req
    "DeleteNotificationRule"
    "fixture/DeleteNotificationRule.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListEventTypes :: ListEventTypes -> TestTree
requestListEventTypes =
  req
    "ListEventTypes"
    "fixture/ListEventTypes.yaml"

requestDeleteTarget :: DeleteTarget -> TestTree
requestDeleteTarget =
  req
    "DeleteTarget"
    "fixture/DeleteTarget.yaml"

requestListNotificationRules :: ListNotificationRules -> TestTree
requestListNotificationRules =
  req
    "ListNotificationRules"
    "fixture/ListNotificationRules.yaml"

requestListTargets :: ListTargets -> TestTree
requestListTargets =
  req
    "ListTargets"
    "fixture/ListTargets.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestSubscribe :: Subscribe -> TestTree
requestSubscribe =
  req
    "Subscribe"
    "fixture/Subscribe.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUnsubscribe :: Unsubscribe -> TestTree
requestUnsubscribe =
  req
    "Unsubscribe"
    "fixture/Unsubscribe.yaml"

requestDescribeNotificationRule :: DescribeNotificationRule -> TestTree
requestDescribeNotificationRule =
  req
    "DescribeNotificationRule"
    "fixture/DescribeNotificationRule.yaml"

-- Responses

responseCreateNotificationRule :: CreateNotificationRuleResponse -> TestTree
responseCreateNotificationRule =
  res
    "CreateNotificationRuleResponse"
    "fixture/CreateNotificationRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNotificationRule)

responseUpdateNotificationRule :: UpdateNotificationRuleResponse -> TestTree
responseUpdateNotificationRule =
  res
    "UpdateNotificationRuleResponse"
    "fixture/UpdateNotificationRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNotificationRule)

responseDeleteNotificationRule :: DeleteNotificationRuleResponse -> TestTree
responseDeleteNotificationRule =
  res
    "DeleteNotificationRuleResponse"
    "fixture/DeleteNotificationRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNotificationRule)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListEventTypes :: ListEventTypesResponse -> TestTree
responseListEventTypes =
  res
    "ListEventTypesResponse"
    "fixture/ListEventTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEventTypes)

responseDeleteTarget :: DeleteTargetResponse -> TestTree
responseDeleteTarget =
  res
    "DeleteTargetResponse"
    "fixture/DeleteTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTarget)

responseListNotificationRules :: ListNotificationRulesResponse -> TestTree
responseListNotificationRules =
  res
    "ListNotificationRulesResponse"
    "fixture/ListNotificationRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNotificationRules)

responseListTargets :: ListTargetsResponse -> TestTree
responseListTargets =
  res
    "ListTargetsResponse"
    "fixture/ListTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTargets)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseSubscribe :: SubscribeResponse -> TestTree
responseSubscribe =
  res
    "SubscribeResponse"
    "fixture/SubscribeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Subscribe)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUnsubscribe :: UnsubscribeResponse -> TestTree
responseUnsubscribe =
  res
    "UnsubscribeResponse"
    "fixture/UnsubscribeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Unsubscribe)

responseDescribeNotificationRule :: DescribeNotificationRuleResponse -> TestTree
responseDescribeNotificationRule =
  res
    "DescribeNotificationRuleResponse"
    "fixture/DescribeNotificationRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNotificationRule)
