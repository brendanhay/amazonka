{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ELBv2
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.ELBv2 where

import Data.Proxy
import Network.AWS.ELBv2
import Test.AWS.ELBv2.Internal
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
--         [ requestDescribeLoadBalancers $
--             describeLoadBalancers
--
--         , requestDescribeTags $
--             describeTags
--
--         , requestDeleteRule $
--             deleteRule
--
--         , requestRemoveTags $
--             removeTags
--
--         , requestDeleteTargetGroup $
--             deleteTargetGroup
--
--         , requestSetSubnets $
--             setSubnets
--
--         , requestCreateRule $
--             createRule
--
--         , requestDescribeListenerCertificates $
--             describeListenerCertificates
--
--         , requestSetSecurityGroups $
--             setSecurityGroups
--
--         , requestSetRulePriorities $
--             setRulePriorities
--
--         , requestDescribeTargetGroups $
--             describeTargetGroups
--
--         , requestDescribeRules $
--             describeRules
--
--         , requestDeleteLoadBalancer $
--             deleteLoadBalancer
--
--         , requestRegisterTargets $
--             registerTargets
--
--         , requestModifyListener $
--             modifyListener
--
--         , requestModifyTargetGroup $
--             modifyTargetGroup
--
--         , requestModifyTargetGroupAttributes $
--             modifyTargetGroupAttributes
--
--         , requestDescribeTargetGroupAttributes $
--             describeTargetGroupAttributes
--
--         , requestDeleteListener $
--             deleteListener
--
--         , requestDescribeSSLPolicies $
--             describeSSLPolicies
--
--         , requestDescribeAccountLimits $
--             describeAccountLimits
--
--         , requestDeregisterTargets $
--             deregisterTargets
--
--         , requestCreateListener $
--             createListener
--
--         , requestCreateTargetGroup $
--             createTargetGroup
--
--         , requestModifyLoadBalancerAttributes $
--             modifyLoadBalancerAttributes
--
--         , requestSetIPAddressType $
--             setIPAddressType
--
--         , requestAddTags $
--             addTags
--
--         , requestDescribeLoadBalancerAttributes $
--             describeLoadBalancerAttributes
--
--         , requestDescribeListeners $
--             describeListeners
--
--         , requestDescribeTargetHealth $
--             describeTargetHealth
--
--         , requestCreateLoadBalancer $
--             createLoadBalancer
--
--         , requestRemoveListenerCertificates $
--             removeListenerCertificates
--
--         , requestModifyRule $
--             modifyRule
--
--         , requestAddListenerCertificates $
--             addListenerCertificates
--
--           ]

--     , testGroup "response"
--         [ responseDescribeLoadBalancers $
--             describeLoadBalancersResponse
--
--         , responseDescribeTags $
--             describeTagsResponse
--
--         , responseDeleteRule $
--             deleteRuleResponse
--
--         , responseRemoveTags $
--             removeTagsResponse
--
--         , responseDeleteTargetGroup $
--             deleteTargetGroupResponse
--
--         , responseSetSubnets $
--             setSubnetsResponse
--
--         , responseCreateRule $
--             createRuleResponse
--
--         , responseDescribeListenerCertificates $
--             describeListenerCertificatesResponse
--
--         , responseSetSecurityGroups $
--             setSecurityGroupsResponse
--
--         , responseSetRulePriorities $
--             setRulePrioritiesResponse
--
--         , responseDescribeTargetGroups $
--             describeTargetGroupsResponse
--
--         , responseDescribeRules $
--             describeRulesResponse
--
--         , responseDeleteLoadBalancer $
--             deleteLoadBalancerResponse
--
--         , responseRegisterTargets $
--             registerTargetsResponse
--
--         , responseModifyListener $
--             modifyListenerResponse
--
--         , responseModifyTargetGroup $
--             modifyTargetGroupResponse
--
--         , responseModifyTargetGroupAttributes $
--             modifyTargetGroupAttributesResponse
--
--         , responseDescribeTargetGroupAttributes $
--             describeTargetGroupAttributesResponse
--
--         , responseDeleteListener $
--             deleteListenerResponse
--
--         , responseDescribeSSLPolicies $
--             describeSSLPoliciesResponse
--
--         , responseDescribeAccountLimits $
--             describeAccountLimitsResponse
--
--         , responseDeregisterTargets $
--             deregisterTargetsResponse
--
--         , responseCreateListener $
--             createListenerResponse
--
--         , responseCreateTargetGroup $
--             createTargetGroupResponse
--
--         , responseModifyLoadBalancerAttributes $
--             modifyLoadBalancerAttributesResponse
--
--         , responseSetIPAddressType $
--             setIPAddressTypeResponse
--
--         , responseAddTags $
--             addTagsResponse
--
--         , responseDescribeLoadBalancerAttributes $
--             describeLoadBalancerAttributesResponse
--
--         , responseDescribeListeners $
--             describeListenersResponse
--
--         , responseDescribeTargetHealth $
--             describeTargetHealthResponse
--
--         , responseCreateLoadBalancer $
--             createLoadBalancerResponse
--
--         , responseRemoveListenerCertificates $
--             removeListenerCertificatesResponse
--
--         , responseModifyRule $
--             modifyRuleResponse
--
--         , responseAddListenerCertificates $
--             addListenerCertificatesResponse
--
--           ]
--     ]

-- Requests

requestDescribeLoadBalancers :: DescribeLoadBalancers -> TestTree
requestDescribeLoadBalancers = req
    "DescribeLoadBalancers"
    "fixture/DescribeLoadBalancers.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags = req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestDeleteRule :: DeleteRule -> TestTree
requestDeleteRule = req
    "DeleteRule"
    "fixture/DeleteRule.yaml"

requestRemoveTags :: RemoveTags -> TestTree
requestRemoveTags = req
    "RemoveTags"
    "fixture/RemoveTags.yaml"

requestDeleteTargetGroup :: DeleteTargetGroup -> TestTree
requestDeleteTargetGroup = req
    "DeleteTargetGroup"
    "fixture/DeleteTargetGroup.yaml"

requestSetSubnets :: SetSubnets -> TestTree
requestSetSubnets = req
    "SetSubnets"
    "fixture/SetSubnets.yaml"

requestCreateRule :: CreateRule -> TestTree
requestCreateRule = req
    "CreateRule"
    "fixture/CreateRule.yaml"

requestDescribeListenerCertificates :: DescribeListenerCertificates -> TestTree
requestDescribeListenerCertificates = req
    "DescribeListenerCertificates"
    "fixture/DescribeListenerCertificates.yaml"

requestSetSecurityGroups :: SetSecurityGroups -> TestTree
requestSetSecurityGroups = req
    "SetSecurityGroups"
    "fixture/SetSecurityGroups.yaml"

requestSetRulePriorities :: SetRulePriorities -> TestTree
requestSetRulePriorities = req
    "SetRulePriorities"
    "fixture/SetRulePriorities.yaml"

requestDescribeTargetGroups :: DescribeTargetGroups -> TestTree
requestDescribeTargetGroups = req
    "DescribeTargetGroups"
    "fixture/DescribeTargetGroups.yaml"

requestDescribeRules :: DescribeRules -> TestTree
requestDescribeRules = req
    "DescribeRules"
    "fixture/DescribeRules.yaml"

requestDeleteLoadBalancer :: DeleteLoadBalancer -> TestTree
requestDeleteLoadBalancer = req
    "DeleteLoadBalancer"
    "fixture/DeleteLoadBalancer.yaml"

requestRegisterTargets :: RegisterTargets -> TestTree
requestRegisterTargets = req
    "RegisterTargets"
    "fixture/RegisterTargets.yaml"

requestModifyListener :: ModifyListener -> TestTree
requestModifyListener = req
    "ModifyListener"
    "fixture/ModifyListener.yaml"

requestModifyTargetGroup :: ModifyTargetGroup -> TestTree
requestModifyTargetGroup = req
    "ModifyTargetGroup"
    "fixture/ModifyTargetGroup.yaml"

requestModifyTargetGroupAttributes :: ModifyTargetGroupAttributes -> TestTree
requestModifyTargetGroupAttributes = req
    "ModifyTargetGroupAttributes"
    "fixture/ModifyTargetGroupAttributes.yaml"

requestDescribeTargetGroupAttributes :: DescribeTargetGroupAttributes -> TestTree
requestDescribeTargetGroupAttributes = req
    "DescribeTargetGroupAttributes"
    "fixture/DescribeTargetGroupAttributes.yaml"

requestDeleteListener :: DeleteListener -> TestTree
requestDeleteListener = req
    "DeleteListener"
    "fixture/DeleteListener.yaml"

requestDescribeSSLPolicies :: DescribeSSLPolicies -> TestTree
requestDescribeSSLPolicies = req
    "DescribeSSLPolicies"
    "fixture/DescribeSSLPolicies.yaml"

requestDescribeAccountLimits :: DescribeAccountLimits -> TestTree
requestDescribeAccountLimits = req
    "DescribeAccountLimits"
    "fixture/DescribeAccountLimits.yaml"

requestDeregisterTargets :: DeregisterTargets -> TestTree
requestDeregisterTargets = req
    "DeregisterTargets"
    "fixture/DeregisterTargets.yaml"

requestCreateListener :: CreateListener -> TestTree
requestCreateListener = req
    "CreateListener"
    "fixture/CreateListener.yaml"

requestCreateTargetGroup :: CreateTargetGroup -> TestTree
requestCreateTargetGroup = req
    "CreateTargetGroup"
    "fixture/CreateTargetGroup.yaml"

requestModifyLoadBalancerAttributes :: ModifyLoadBalancerAttributes -> TestTree
requestModifyLoadBalancerAttributes = req
    "ModifyLoadBalancerAttributes"
    "fixture/ModifyLoadBalancerAttributes.yaml"

requestSetIPAddressType :: SetIPAddressType -> TestTree
requestSetIPAddressType = req
    "SetIPAddressType"
    "fixture/SetIPAddressType.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags = req
    "AddTags"
    "fixture/AddTags.yaml"

requestDescribeLoadBalancerAttributes :: DescribeLoadBalancerAttributes -> TestTree
requestDescribeLoadBalancerAttributes = req
    "DescribeLoadBalancerAttributes"
    "fixture/DescribeLoadBalancerAttributes.yaml"

requestDescribeListeners :: DescribeListeners -> TestTree
requestDescribeListeners = req
    "DescribeListeners"
    "fixture/DescribeListeners.yaml"

requestDescribeTargetHealth :: DescribeTargetHealth -> TestTree
requestDescribeTargetHealth = req
    "DescribeTargetHealth"
    "fixture/DescribeTargetHealth.yaml"

requestCreateLoadBalancer :: CreateLoadBalancer -> TestTree
requestCreateLoadBalancer = req
    "CreateLoadBalancer"
    "fixture/CreateLoadBalancer.yaml"

requestRemoveListenerCertificates :: RemoveListenerCertificates -> TestTree
requestRemoveListenerCertificates = req
    "RemoveListenerCertificates"
    "fixture/RemoveListenerCertificates.yaml"

requestModifyRule :: ModifyRule -> TestTree
requestModifyRule = req
    "ModifyRule"
    "fixture/ModifyRule.yaml"

requestAddListenerCertificates :: AddListenerCertificates -> TestTree
requestAddListenerCertificates = req
    "AddListenerCertificates"
    "fixture/AddListenerCertificates.yaml"

-- Responses

responseDescribeLoadBalancers :: DescribeLoadBalancersResponse -> TestTree
responseDescribeLoadBalancers = res
    "DescribeLoadBalancersResponse"
    "fixture/DescribeLoadBalancersResponse.proto"
    eLBv2
    (Proxy :: Proxy DescribeLoadBalancers)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    eLBv2
    (Proxy :: Proxy DescribeTags)

responseDeleteRule :: DeleteRuleResponse -> TestTree
responseDeleteRule = res
    "DeleteRuleResponse"
    "fixture/DeleteRuleResponse.proto"
    eLBv2
    (Proxy :: Proxy DeleteRule)

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags = res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    eLBv2
    (Proxy :: Proxy RemoveTags)

responseDeleteTargetGroup :: DeleteTargetGroupResponse -> TestTree
responseDeleteTargetGroup = res
    "DeleteTargetGroupResponse"
    "fixture/DeleteTargetGroupResponse.proto"
    eLBv2
    (Proxy :: Proxy DeleteTargetGroup)

responseSetSubnets :: SetSubnetsResponse -> TestTree
responseSetSubnets = res
    "SetSubnetsResponse"
    "fixture/SetSubnetsResponse.proto"
    eLBv2
    (Proxy :: Proxy SetSubnets)

responseCreateRule :: CreateRuleResponse -> TestTree
responseCreateRule = res
    "CreateRuleResponse"
    "fixture/CreateRuleResponse.proto"
    eLBv2
    (Proxy :: Proxy CreateRule)

responseDescribeListenerCertificates :: DescribeListenerCertificatesResponse -> TestTree
responseDescribeListenerCertificates = res
    "DescribeListenerCertificatesResponse"
    "fixture/DescribeListenerCertificatesResponse.proto"
    eLBv2
    (Proxy :: Proxy DescribeListenerCertificates)

responseSetSecurityGroups :: SetSecurityGroupsResponse -> TestTree
responseSetSecurityGroups = res
    "SetSecurityGroupsResponse"
    "fixture/SetSecurityGroupsResponse.proto"
    eLBv2
    (Proxy :: Proxy SetSecurityGroups)

responseSetRulePriorities :: SetRulePrioritiesResponse -> TestTree
responseSetRulePriorities = res
    "SetRulePrioritiesResponse"
    "fixture/SetRulePrioritiesResponse.proto"
    eLBv2
    (Proxy :: Proxy SetRulePriorities)

responseDescribeTargetGroups :: DescribeTargetGroupsResponse -> TestTree
responseDescribeTargetGroups = res
    "DescribeTargetGroupsResponse"
    "fixture/DescribeTargetGroupsResponse.proto"
    eLBv2
    (Proxy :: Proxy DescribeTargetGroups)

responseDescribeRules :: DescribeRulesResponse -> TestTree
responseDescribeRules = res
    "DescribeRulesResponse"
    "fixture/DescribeRulesResponse.proto"
    eLBv2
    (Proxy :: Proxy DescribeRules)

responseDeleteLoadBalancer :: DeleteLoadBalancerResponse -> TestTree
responseDeleteLoadBalancer = res
    "DeleteLoadBalancerResponse"
    "fixture/DeleteLoadBalancerResponse.proto"
    eLBv2
    (Proxy :: Proxy DeleteLoadBalancer)

responseRegisterTargets :: RegisterTargetsResponse -> TestTree
responseRegisterTargets = res
    "RegisterTargetsResponse"
    "fixture/RegisterTargetsResponse.proto"
    eLBv2
    (Proxy :: Proxy RegisterTargets)

responseModifyListener :: ModifyListenerResponse -> TestTree
responseModifyListener = res
    "ModifyListenerResponse"
    "fixture/ModifyListenerResponse.proto"
    eLBv2
    (Proxy :: Proxy ModifyListener)

responseModifyTargetGroup :: ModifyTargetGroupResponse -> TestTree
responseModifyTargetGroup = res
    "ModifyTargetGroupResponse"
    "fixture/ModifyTargetGroupResponse.proto"
    eLBv2
    (Proxy :: Proxy ModifyTargetGroup)

responseModifyTargetGroupAttributes :: ModifyTargetGroupAttributesResponse -> TestTree
responseModifyTargetGroupAttributes = res
    "ModifyTargetGroupAttributesResponse"
    "fixture/ModifyTargetGroupAttributesResponse.proto"
    eLBv2
    (Proxy :: Proxy ModifyTargetGroupAttributes)

responseDescribeTargetGroupAttributes :: DescribeTargetGroupAttributesResponse -> TestTree
responseDescribeTargetGroupAttributes = res
    "DescribeTargetGroupAttributesResponse"
    "fixture/DescribeTargetGroupAttributesResponse.proto"
    eLBv2
    (Proxy :: Proxy DescribeTargetGroupAttributes)

responseDeleteListener :: DeleteListenerResponse -> TestTree
responseDeleteListener = res
    "DeleteListenerResponse"
    "fixture/DeleteListenerResponse.proto"
    eLBv2
    (Proxy :: Proxy DeleteListener)

responseDescribeSSLPolicies :: DescribeSSLPoliciesResponse -> TestTree
responseDescribeSSLPolicies = res
    "DescribeSSLPoliciesResponse"
    "fixture/DescribeSSLPoliciesResponse.proto"
    eLBv2
    (Proxy :: Proxy DescribeSSLPolicies)

responseDescribeAccountLimits :: DescribeAccountLimitsResponse -> TestTree
responseDescribeAccountLimits = res
    "DescribeAccountLimitsResponse"
    "fixture/DescribeAccountLimitsResponse.proto"
    eLBv2
    (Proxy :: Proxy DescribeAccountLimits)

responseDeregisterTargets :: DeregisterTargetsResponse -> TestTree
responseDeregisterTargets = res
    "DeregisterTargetsResponse"
    "fixture/DeregisterTargetsResponse.proto"
    eLBv2
    (Proxy :: Proxy DeregisterTargets)

responseCreateListener :: CreateListenerResponse -> TestTree
responseCreateListener = res
    "CreateListenerResponse"
    "fixture/CreateListenerResponse.proto"
    eLBv2
    (Proxy :: Proxy CreateListener)

responseCreateTargetGroup :: CreateTargetGroupResponse -> TestTree
responseCreateTargetGroup = res
    "CreateTargetGroupResponse"
    "fixture/CreateTargetGroupResponse.proto"
    eLBv2
    (Proxy :: Proxy CreateTargetGroup)

responseModifyLoadBalancerAttributes :: ModifyLoadBalancerAttributesResponse -> TestTree
responseModifyLoadBalancerAttributes = res
    "ModifyLoadBalancerAttributesResponse"
    "fixture/ModifyLoadBalancerAttributesResponse.proto"
    eLBv2
    (Proxy :: Proxy ModifyLoadBalancerAttributes)

responseSetIPAddressType :: SetIPAddressTypeResponse -> TestTree
responseSetIPAddressType = res
    "SetIPAddressTypeResponse"
    "fixture/SetIPAddressTypeResponse.proto"
    eLBv2
    (Proxy :: Proxy SetIPAddressType)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags = res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    eLBv2
    (Proxy :: Proxy AddTags)

responseDescribeLoadBalancerAttributes :: DescribeLoadBalancerAttributesResponse -> TestTree
responseDescribeLoadBalancerAttributes = res
    "DescribeLoadBalancerAttributesResponse"
    "fixture/DescribeLoadBalancerAttributesResponse.proto"
    eLBv2
    (Proxy :: Proxy DescribeLoadBalancerAttributes)

responseDescribeListeners :: DescribeListenersResponse -> TestTree
responseDescribeListeners = res
    "DescribeListenersResponse"
    "fixture/DescribeListenersResponse.proto"
    eLBv2
    (Proxy :: Proxy DescribeListeners)

responseDescribeTargetHealth :: DescribeTargetHealthResponse -> TestTree
responseDescribeTargetHealth = res
    "DescribeTargetHealthResponse"
    "fixture/DescribeTargetHealthResponse.proto"
    eLBv2
    (Proxy :: Proxy DescribeTargetHealth)

responseCreateLoadBalancer :: CreateLoadBalancerResponse -> TestTree
responseCreateLoadBalancer = res
    "CreateLoadBalancerResponse"
    "fixture/CreateLoadBalancerResponse.proto"
    eLBv2
    (Proxy :: Proxy CreateLoadBalancer)

responseRemoveListenerCertificates :: RemoveListenerCertificatesResponse -> TestTree
responseRemoveListenerCertificates = res
    "RemoveListenerCertificatesResponse"
    "fixture/RemoveListenerCertificatesResponse.proto"
    eLBv2
    (Proxy :: Proxy RemoveListenerCertificates)

responseModifyRule :: ModifyRuleResponse -> TestTree
responseModifyRule = res
    "ModifyRuleResponse"
    "fixture/ModifyRuleResponse.proto"
    eLBv2
    (Proxy :: Proxy ModifyRule)

responseAddListenerCertificates :: AddListenerCertificatesResponse -> TestTree
responseAddListenerCertificates = res
    "AddListenerCertificatesResponse"
    "fixture/AddListenerCertificatesResponse.proto"
    eLBv2
    (Proxy :: Proxy AddListenerCertificates)
