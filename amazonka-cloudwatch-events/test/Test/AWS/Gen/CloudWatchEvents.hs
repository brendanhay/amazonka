{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudWatchEvents
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CloudWatchEvents where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.CloudWatchEvents
import Test.AWS.CloudWatchEvents.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestRemoveTargets $
--             removeTargets
--
--         , requestDeleteRule $
--             deleteRule
--
--         , requestListRules $
--             listRules
--
--         , requestPutRule $
--             putRule
--
--         , requestDisableRule $
--             disableRule
--
--         , requestListTargetsByRule $
--             listTargetsByRule
--
--         , requestDescribeRule $
--             describeRule
--
--         , requestEnableRule $
--             enableRule
--
--         , requestListRuleNamesByTarget $
--             listRuleNamesByTarget
--
--         , requestTestEventPattern $
--             testEventPattern
--
--         , requestPutTargets $
--             putTargets
--
--         , requestPutEvents $
--             putEvents
--
--           ]

--     , testGroup "response"
--         [ responseRemoveTargets $
--             removeTargetsResponse
--
--         , responseDeleteRule $
--             deleteRuleResponse
--
--         , responseListRules $
--             listRulesResponse
--
--         , responsePutRule $
--             putRuleResponse
--
--         , responseDisableRule $
--             disableRuleResponse
--
--         , responseListTargetsByRule $
--             listTargetsByRuleResponse
--
--         , responseDescribeRule $
--             describeRuleResponse
--
--         , responseEnableRule $
--             enableRuleResponse
--
--         , responseListRuleNamesByTarget $
--             listRuleNamesByTargetResponse
--
--         , responseTestEventPattern $
--             testEventPatternResponse
--
--         , responsePutTargets $
--             putTargetsResponse
--
--         , responsePutEvents $
--             putEventsResponse
--
--           ]
--     ]

-- Requests

requestRemoveTargets :: RemoveTargets -> TestTree
requestRemoveTargets = req
    "RemoveTargets"
    "fixture/RemoveTargets.yaml"

requestDeleteRule :: DeleteRule -> TestTree
requestDeleteRule = req
    "DeleteRule"
    "fixture/DeleteRule.yaml"

requestListRules :: ListRules -> TestTree
requestListRules = req
    "ListRules"
    "fixture/ListRules.yaml"

requestPutRule :: PutRule -> TestTree
requestPutRule = req
    "PutRule"
    "fixture/PutRule.yaml"

requestDisableRule :: DisableRule -> TestTree
requestDisableRule = req
    "DisableRule"
    "fixture/DisableRule.yaml"

requestListTargetsByRule :: ListTargetsByRule -> TestTree
requestListTargetsByRule = req
    "ListTargetsByRule"
    "fixture/ListTargetsByRule.yaml"

requestDescribeRule :: DescribeRule -> TestTree
requestDescribeRule = req
    "DescribeRule"
    "fixture/DescribeRule.yaml"

requestEnableRule :: EnableRule -> TestTree
requestEnableRule = req
    "EnableRule"
    "fixture/EnableRule.yaml"

requestListRuleNamesByTarget :: ListRuleNamesByTarget -> TestTree
requestListRuleNamesByTarget = req
    "ListRuleNamesByTarget"
    "fixture/ListRuleNamesByTarget.yaml"

requestTestEventPattern :: TestEventPattern -> TestTree
requestTestEventPattern = req
    "TestEventPattern"
    "fixture/TestEventPattern.yaml"

requestPutTargets :: PutTargets -> TestTree
requestPutTargets = req
    "PutTargets"
    "fixture/PutTargets.yaml"

requestPutEvents :: PutEvents -> TestTree
requestPutEvents = req
    "PutEvents"
    "fixture/PutEvents.yaml"

-- Responses

responseRemoveTargets :: RemoveTargetsResponse -> TestTree
responseRemoveTargets = res
    "RemoveTargetsResponse"
    "fixture/RemoveTargetsResponse.proto"
    cloudWatchEvents
    (Proxy :: Proxy RemoveTargets)

responseDeleteRule :: DeleteRuleResponse -> TestTree
responseDeleteRule = res
    "DeleteRuleResponse"
    "fixture/DeleteRuleResponse.proto"
    cloudWatchEvents
    (Proxy :: Proxy DeleteRule)

responseListRules :: ListRulesResponse -> TestTree
responseListRules = res
    "ListRulesResponse"
    "fixture/ListRulesResponse.proto"
    cloudWatchEvents
    (Proxy :: Proxy ListRules)

responsePutRule :: PutRuleResponse -> TestTree
responsePutRule = res
    "PutRuleResponse"
    "fixture/PutRuleResponse.proto"
    cloudWatchEvents
    (Proxy :: Proxy PutRule)

responseDisableRule :: DisableRuleResponse -> TestTree
responseDisableRule = res
    "DisableRuleResponse"
    "fixture/DisableRuleResponse.proto"
    cloudWatchEvents
    (Proxy :: Proxy DisableRule)

responseListTargetsByRule :: ListTargetsByRuleResponse -> TestTree
responseListTargetsByRule = res
    "ListTargetsByRuleResponse"
    "fixture/ListTargetsByRuleResponse.proto"
    cloudWatchEvents
    (Proxy :: Proxy ListTargetsByRule)

responseDescribeRule :: DescribeRuleResponse -> TestTree
responseDescribeRule = res
    "DescribeRuleResponse"
    "fixture/DescribeRuleResponse.proto"
    cloudWatchEvents
    (Proxy :: Proxy DescribeRule)

responseEnableRule :: EnableRuleResponse -> TestTree
responseEnableRule = res
    "EnableRuleResponse"
    "fixture/EnableRuleResponse.proto"
    cloudWatchEvents
    (Proxy :: Proxy EnableRule)

responseListRuleNamesByTarget :: ListRuleNamesByTargetResponse -> TestTree
responseListRuleNamesByTarget = res
    "ListRuleNamesByTargetResponse"
    "fixture/ListRuleNamesByTargetResponse.proto"
    cloudWatchEvents
    (Proxy :: Proxy ListRuleNamesByTarget)

responseTestEventPattern :: TestEventPatternResponse -> TestTree
responseTestEventPattern = res
    "TestEventPatternResponse"
    "fixture/TestEventPatternResponse.proto"
    cloudWatchEvents
    (Proxy :: Proxy TestEventPattern)

responsePutTargets :: PutTargetsResponse -> TestTree
responsePutTargets = res
    "PutTargetsResponse"
    "fixture/PutTargetsResponse.proto"
    cloudWatchEvents
    (Proxy :: Proxy PutTargets)

responsePutEvents :: PutEventsResponse -> TestTree
responsePutEvents = res
    "PutEventsResponse"
    "fixture/PutEventsResponse.proto"
    cloudWatchEvents
    (Proxy :: Proxy PutEvents)
