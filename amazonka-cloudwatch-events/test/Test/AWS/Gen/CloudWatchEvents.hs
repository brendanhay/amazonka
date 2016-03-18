{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudWatchEvents
-- Copyright   : (c) 2013-2015 Brendan Hay
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
--         [ testRemoveTargets $
--             removeTargets
--
--         , testDeleteRule $
--             deleteRule
--
--         , testListRules $
--             listRules
--
--         , testPutRule $
--             putRule
--
--         , testDisableRule $
--             disableRule
--
--         , testListTargetsByRule $
--             listTargetsByRule
--
--         , testDescribeRule $
--             describeRule
--
--         , testEnableRule $
--             enableRule
--
--         , testListRuleNamesByTarget $
--             listRuleNamesByTarget
--
--         , testTestEventPattern $
--             testEventPattern
--
--         , testPutTargets $
--             putTargets
--
--         , testPutEvents $
--             putEvents
--
--           ]

--     , testGroup "response"
--         [ testRemoveTargetsResponse $
--             removeTargetsResponse
--
--         , testDeleteRuleResponse $
--             deleteRuleResponse
--
--         , testListRulesResponse $
--             listRulesResponse
--
--         , testPutRuleResponse $
--             putRuleResponse
--
--         , testDisableRuleResponse $
--             disableRuleResponse
--
--         , testListTargetsByRuleResponse $
--             listTargetsByRuleResponse
--
--         , testDescribeRuleResponse $
--             describeRuleResponse
--
--         , testEnableRuleResponse $
--             enableRuleResponse
--
--         , testListRuleNamesByTargetResponse $
--             listRuleNamesByTargetResponse
--
--         , testTestEventPatternResponse $
--             testEventPatternResponse
--
--         , testPutTargetsResponse $
--             putTargetsResponse
--
--         , testPutEventsResponse $
--             putEventsResponse
--
--           ]
--     ]

-- Requests

testRemoveTargets :: RemoveTargets -> TestTree
testRemoveTargets = req
    "RemoveTargets"
    "fixture/RemoveTargets.yaml"

testDeleteRule :: DeleteRule -> TestTree
testDeleteRule = req
    "DeleteRule"
    "fixture/DeleteRule.yaml"

testListRules :: ListRules -> TestTree
testListRules = req
    "ListRules"
    "fixture/ListRules.yaml"

testPutRule :: PutRule -> TestTree
testPutRule = req
    "PutRule"
    "fixture/PutRule.yaml"

testDisableRule :: DisableRule -> TestTree
testDisableRule = req
    "DisableRule"
    "fixture/DisableRule.yaml"

testListTargetsByRule :: ListTargetsByRule -> TestTree
testListTargetsByRule = req
    "ListTargetsByRule"
    "fixture/ListTargetsByRule.yaml"

testDescribeRule :: DescribeRule -> TestTree
testDescribeRule = req
    "DescribeRule"
    "fixture/DescribeRule.yaml"

testEnableRule :: EnableRule -> TestTree
testEnableRule = req
    "EnableRule"
    "fixture/EnableRule.yaml"

testListRuleNamesByTarget :: ListRuleNamesByTarget -> TestTree
testListRuleNamesByTarget = req
    "ListRuleNamesByTarget"
    "fixture/ListRuleNamesByTarget.yaml"

testTestEventPattern :: TestEventPattern -> TestTree
testTestEventPattern = req
    "TestEventPattern"
    "fixture/TestEventPattern.yaml"

testPutTargets :: PutTargets -> TestTree
testPutTargets = req
    "PutTargets"
    "fixture/PutTargets.yaml"

testPutEvents :: PutEvents -> TestTree
testPutEvents = req
    "PutEvents"
    "fixture/PutEvents.yaml"

-- Responses

testRemoveTargetsResponse :: RemoveTargetsResponse -> TestTree
testRemoveTargetsResponse = res
    "RemoveTargetsResponse"
    "fixture/RemoveTargetsResponse.proto"
    cloudWatchEvents
    (Proxy :: Proxy RemoveTargets)

testDeleteRuleResponse :: DeleteRuleResponse -> TestTree
testDeleteRuleResponse = res
    "DeleteRuleResponse"
    "fixture/DeleteRuleResponse.proto"
    cloudWatchEvents
    (Proxy :: Proxy DeleteRule)

testListRulesResponse :: ListRulesResponse -> TestTree
testListRulesResponse = res
    "ListRulesResponse"
    "fixture/ListRulesResponse.proto"
    cloudWatchEvents
    (Proxy :: Proxy ListRules)

testPutRuleResponse :: PutRuleResponse -> TestTree
testPutRuleResponse = res
    "PutRuleResponse"
    "fixture/PutRuleResponse.proto"
    cloudWatchEvents
    (Proxy :: Proxy PutRule)

testDisableRuleResponse :: DisableRuleResponse -> TestTree
testDisableRuleResponse = res
    "DisableRuleResponse"
    "fixture/DisableRuleResponse.proto"
    cloudWatchEvents
    (Proxy :: Proxy DisableRule)

testListTargetsByRuleResponse :: ListTargetsByRuleResponse -> TestTree
testListTargetsByRuleResponse = res
    "ListTargetsByRuleResponse"
    "fixture/ListTargetsByRuleResponse.proto"
    cloudWatchEvents
    (Proxy :: Proxy ListTargetsByRule)

testDescribeRuleResponse :: DescribeRuleResponse -> TestTree
testDescribeRuleResponse = res
    "DescribeRuleResponse"
    "fixture/DescribeRuleResponse.proto"
    cloudWatchEvents
    (Proxy :: Proxy DescribeRule)

testEnableRuleResponse :: EnableRuleResponse -> TestTree
testEnableRuleResponse = res
    "EnableRuleResponse"
    "fixture/EnableRuleResponse.proto"
    cloudWatchEvents
    (Proxy :: Proxy EnableRule)

testListRuleNamesByTargetResponse :: ListRuleNamesByTargetResponse -> TestTree
testListRuleNamesByTargetResponse = res
    "ListRuleNamesByTargetResponse"
    "fixture/ListRuleNamesByTargetResponse.proto"
    cloudWatchEvents
    (Proxy :: Proxy ListRuleNamesByTarget)

testTestEventPatternResponse :: TestEventPatternResponse -> TestTree
testTestEventPatternResponse = res
    "TestEventPatternResponse"
    "fixture/TestEventPatternResponse.proto"
    cloudWatchEvents
    (Proxy :: Proxy TestEventPattern)

testPutTargetsResponse :: PutTargetsResponse -> TestTree
testPutTargetsResponse = res
    "PutTargetsResponse"
    "fixture/PutTargetsResponse.proto"
    cloudWatchEvents
    (Proxy :: Proxy PutTargets)

testPutEventsResponse :: PutEventsResponse -> TestTree
testPutEventsResponse = res
    "PutEventsResponse"
    "fixture/PutEventsResponse.proto"
    cloudWatchEvents
    (Proxy :: Proxy PutEvents)
