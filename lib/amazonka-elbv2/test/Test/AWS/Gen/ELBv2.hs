{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ELBv2
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--             mkDescribeLoadBalancers
--
--         , requestDescribeTags $
--             mkDescribeTags
--
--         , requestDeleteRule $
--             mkDeleteRule
--
--         , requestRemoveTags $
--             mkRemoveTags
--
--         , requestDeleteTargetGroup $
--             mkDeleteTargetGroup
--
--         , requestSetSubnets $
--             mkSetSubnets
--
--         , requestCreateRule $
--             mkCreateRule
--
--         , requestDescribeListenerCertificates $
--             mkDescribeListenerCertificates
--
--         , requestSetSecurityGroups $
--             mkSetSecurityGroups
--
--         , requestSetRulePriorities $
--             mkSetRulePriorities
--
--         , requestDescribeTargetGroups $
--             mkDescribeTargetGroups
--
--         , requestDescribeRules $
--             mkDescribeRules
--
--         , requestDeleteLoadBalancer $
--             mkDeleteLoadBalancer
--
--         , requestRegisterTargets $
--             mkRegisterTargets
--
--         , requestModifyListener $
--             mkModifyListener
--
--         , requestModifyTargetGroup $
--             mkModifyTargetGroup
--
--         , requestModifyTargetGroupAttributes $
--             mkModifyTargetGroupAttributes
--
--         , requestDescribeTargetGroupAttributes $
--             mkDescribeTargetGroupAttributes
--
--         , requestDeleteListener $
--             mkDeleteListener
--
--         , requestDescribeSSLPolicies $
--             mkDescribeSSLPolicies
--
--         , requestDescribeAccountLimits $
--             mkDescribeAccountLimits
--
--         , requestDeregisterTargets $
--             mkDeregisterTargets
--
--         , requestCreateListener $
--             mkCreateListener
--
--         , requestCreateTargetGroup $
--             mkCreateTargetGroup
--
--         , requestModifyLoadBalancerAttributes $
--             mkModifyLoadBalancerAttributes
--
--         , requestSetIpAddressType $
--             mkSetIpAddressType
--
--         , requestAddTags $
--             mkAddTags
--
--         , requestDescribeLoadBalancerAttributes $
--             mkDescribeLoadBalancerAttributes
--
--         , requestDescribeListeners $
--             mkDescribeListeners
--
--         , requestDescribeTargetHealth $
--             mkDescribeTargetHealth
--
--         , requestCreateLoadBalancer $
--             mkCreateLoadBalancer
--
--         , requestRemoveListenerCertificates $
--             mkRemoveListenerCertificates
--
--         , requestModifyRule $
--             mkModifyRule
--
--         , requestAddListenerCertificates $
--             mkAddListenerCertificates
--
--           ]

--     , testGroup "response"
--         [ responseDescribeLoadBalancers $
--             mkDescribeLoadBalancersResponse
--
--         , responseDescribeTags $
--             mkDescribeTagsResponse
--
--         , responseDeleteRule $
--             mkDeleteRuleResponse
--
--         , responseRemoveTags $
--             mkRemoveTagsResponse
--
--         , responseDeleteTargetGroup $
--             mkDeleteTargetGroupResponse
--
--         , responseSetSubnets $
--             mkSetSubnetsResponse
--
--         , responseCreateRule $
--             mkCreateRuleResponse
--
--         , responseDescribeListenerCertificates $
--             mkDescribeListenerCertificatesResponse
--
--         , responseSetSecurityGroups $
--             mkSetSecurityGroupsResponse
--
--         , responseSetRulePriorities $
--             mkSetRulePrioritiesResponse
--
--         , responseDescribeTargetGroups $
--             mkDescribeTargetGroupsResponse
--
--         , responseDescribeRules $
--             mkDescribeRulesResponse
--
--         , responseDeleteLoadBalancer $
--             mkDeleteLoadBalancerResponse
--
--         , responseRegisterTargets $
--             mkRegisterTargetsResponse
--
--         , responseModifyListener $
--             mkModifyListenerResponse
--
--         , responseModifyTargetGroup $
--             mkModifyTargetGroupResponse
--
--         , responseModifyTargetGroupAttributes $
--             mkModifyTargetGroupAttributesResponse
--
--         , responseDescribeTargetGroupAttributes $
--             mkDescribeTargetGroupAttributesResponse
--
--         , responseDeleteListener $
--             mkDeleteListenerResponse
--
--         , responseDescribeSSLPolicies $
--             mkDescribeSSLPoliciesResponse
--
--         , responseDescribeAccountLimits $
--             mkDescribeAccountLimitsResponse
--
--         , responseDeregisterTargets $
--             mkDeregisterTargetsResponse
--
--         , responseCreateListener $
--             mkCreateListenerResponse
--
--         , responseCreateTargetGroup $
--             mkCreateTargetGroupResponse
--
--         , responseModifyLoadBalancerAttributes $
--             mkModifyLoadBalancerAttributesResponse
--
--         , responseSetIpAddressType $
--             mkSetIpAddressTypeResponse
--
--         , responseAddTags $
--             mkAddTagsResponse
--
--         , responseDescribeLoadBalancerAttributes $
--             mkDescribeLoadBalancerAttributesResponse
--
--         , responseDescribeListeners $
--             mkDescribeListenersResponse
--
--         , responseDescribeTargetHealth $
--             mkDescribeTargetHealthResponse
--
--         , responseCreateLoadBalancer $
--             mkCreateLoadBalancerResponse
--
--         , responseRemoveListenerCertificates $
--             mkRemoveListenerCertificatesResponse
--
--         , responseModifyRule $
--             mkModifyRuleResponse
--
--         , responseAddListenerCertificates $
--             mkAddListenerCertificatesResponse
--
--           ]
--     ]

-- Requests

requestDescribeLoadBalancers :: DescribeLoadBalancers -> TestTree
requestDescribeLoadBalancers =
  req
    "DescribeLoadBalancers"
    "fixture/DescribeLoadBalancers.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags =
  req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestDeleteRule :: DeleteRule -> TestTree
requestDeleteRule =
  req
    "DeleteRule"
    "fixture/DeleteRule.yaml"

requestRemoveTags :: RemoveTags -> TestTree
requestRemoveTags =
  req
    "RemoveTags"
    "fixture/RemoveTags.yaml"

requestDeleteTargetGroup :: DeleteTargetGroup -> TestTree
requestDeleteTargetGroup =
  req
    "DeleteTargetGroup"
    "fixture/DeleteTargetGroup.yaml"

requestSetSubnets :: SetSubnets -> TestTree
requestSetSubnets =
  req
    "SetSubnets"
    "fixture/SetSubnets.yaml"

requestCreateRule :: CreateRule -> TestTree
requestCreateRule =
  req
    "CreateRule"
    "fixture/CreateRule.yaml"

requestDescribeListenerCertificates :: DescribeListenerCertificates -> TestTree
requestDescribeListenerCertificates =
  req
    "DescribeListenerCertificates"
    "fixture/DescribeListenerCertificates.yaml"

requestSetSecurityGroups :: SetSecurityGroups -> TestTree
requestSetSecurityGroups =
  req
    "SetSecurityGroups"
    "fixture/SetSecurityGroups.yaml"

requestSetRulePriorities :: SetRulePriorities -> TestTree
requestSetRulePriorities =
  req
    "SetRulePriorities"
    "fixture/SetRulePriorities.yaml"

requestDescribeTargetGroups :: DescribeTargetGroups -> TestTree
requestDescribeTargetGroups =
  req
    "DescribeTargetGroups"
    "fixture/DescribeTargetGroups.yaml"

requestDescribeRules :: DescribeRules -> TestTree
requestDescribeRules =
  req
    "DescribeRules"
    "fixture/DescribeRules.yaml"

requestDeleteLoadBalancer :: DeleteLoadBalancer -> TestTree
requestDeleteLoadBalancer =
  req
    "DeleteLoadBalancer"
    "fixture/DeleteLoadBalancer.yaml"

requestRegisterTargets :: RegisterTargets -> TestTree
requestRegisterTargets =
  req
    "RegisterTargets"
    "fixture/RegisterTargets.yaml"

requestModifyListener :: ModifyListener -> TestTree
requestModifyListener =
  req
    "ModifyListener"
    "fixture/ModifyListener.yaml"

requestModifyTargetGroup :: ModifyTargetGroup -> TestTree
requestModifyTargetGroup =
  req
    "ModifyTargetGroup"
    "fixture/ModifyTargetGroup.yaml"

requestModifyTargetGroupAttributes :: ModifyTargetGroupAttributes -> TestTree
requestModifyTargetGroupAttributes =
  req
    "ModifyTargetGroupAttributes"
    "fixture/ModifyTargetGroupAttributes.yaml"

requestDescribeTargetGroupAttributes :: DescribeTargetGroupAttributes -> TestTree
requestDescribeTargetGroupAttributes =
  req
    "DescribeTargetGroupAttributes"
    "fixture/DescribeTargetGroupAttributes.yaml"

requestDeleteListener :: DeleteListener -> TestTree
requestDeleteListener =
  req
    "DeleteListener"
    "fixture/DeleteListener.yaml"

requestDescribeSSLPolicies :: DescribeSSLPolicies -> TestTree
requestDescribeSSLPolicies =
  req
    "DescribeSSLPolicies"
    "fixture/DescribeSSLPolicies.yaml"

requestDescribeAccountLimits :: DescribeAccountLimits -> TestTree
requestDescribeAccountLimits =
  req
    "DescribeAccountLimits"
    "fixture/DescribeAccountLimits.yaml"

requestDeregisterTargets :: DeregisterTargets -> TestTree
requestDeregisterTargets =
  req
    "DeregisterTargets"
    "fixture/DeregisterTargets.yaml"

requestCreateListener :: CreateListener -> TestTree
requestCreateListener =
  req
    "CreateListener"
    "fixture/CreateListener.yaml"

requestCreateTargetGroup :: CreateTargetGroup -> TestTree
requestCreateTargetGroup =
  req
    "CreateTargetGroup"
    "fixture/CreateTargetGroup.yaml"

requestModifyLoadBalancerAttributes :: ModifyLoadBalancerAttributes -> TestTree
requestModifyLoadBalancerAttributes =
  req
    "ModifyLoadBalancerAttributes"
    "fixture/ModifyLoadBalancerAttributes.yaml"

requestSetIpAddressType :: SetIpAddressType -> TestTree
requestSetIpAddressType =
  req
    "SetIpAddressType"
    "fixture/SetIpAddressType.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags =
  req
    "AddTags"
    "fixture/AddTags.yaml"

requestDescribeLoadBalancerAttributes :: DescribeLoadBalancerAttributes -> TestTree
requestDescribeLoadBalancerAttributes =
  req
    "DescribeLoadBalancerAttributes"
    "fixture/DescribeLoadBalancerAttributes.yaml"

requestDescribeListeners :: DescribeListeners -> TestTree
requestDescribeListeners =
  req
    "DescribeListeners"
    "fixture/DescribeListeners.yaml"

requestDescribeTargetHealth :: DescribeTargetHealth -> TestTree
requestDescribeTargetHealth =
  req
    "DescribeTargetHealth"
    "fixture/DescribeTargetHealth.yaml"

requestCreateLoadBalancer :: CreateLoadBalancer -> TestTree
requestCreateLoadBalancer =
  req
    "CreateLoadBalancer"
    "fixture/CreateLoadBalancer.yaml"

requestRemoveListenerCertificates :: RemoveListenerCertificates -> TestTree
requestRemoveListenerCertificates =
  req
    "RemoveListenerCertificates"
    "fixture/RemoveListenerCertificates.yaml"

requestModifyRule :: ModifyRule -> TestTree
requestModifyRule =
  req
    "ModifyRule"
    "fixture/ModifyRule.yaml"

requestAddListenerCertificates :: AddListenerCertificates -> TestTree
requestAddListenerCertificates =
  req
    "AddListenerCertificates"
    "fixture/AddListenerCertificates.yaml"

-- Responses

responseDescribeLoadBalancers :: DescribeLoadBalancersResponse -> TestTree
responseDescribeLoadBalancers =
  res
    "DescribeLoadBalancersResponse"
    "fixture/DescribeLoadBalancersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeLoadBalancers)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTags)

responseDeleteRule :: DeleteRuleResponse -> TestTree
responseDeleteRule =
  res
    "DeleteRuleResponse"
    "fixture/DeleteRuleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteRule)

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags =
  res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RemoveTags)

responseDeleteTargetGroup :: DeleteTargetGroupResponse -> TestTree
responseDeleteTargetGroup =
  res
    "DeleteTargetGroupResponse"
    "fixture/DeleteTargetGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTargetGroup)

responseSetSubnets :: SetSubnetsResponse -> TestTree
responseSetSubnets =
  res
    "SetSubnetsResponse"
    "fixture/SetSubnetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetSubnets)

responseCreateRule :: CreateRuleResponse -> TestTree
responseCreateRule =
  res
    "CreateRuleResponse"
    "fixture/CreateRuleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateRule)

responseDescribeListenerCertificates :: DescribeListenerCertificatesResponse -> TestTree
responseDescribeListenerCertificates =
  res
    "DescribeListenerCertificatesResponse"
    "fixture/DescribeListenerCertificatesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeListenerCertificates)

responseSetSecurityGroups :: SetSecurityGroupsResponse -> TestTree
responseSetSecurityGroups =
  res
    "SetSecurityGroupsResponse"
    "fixture/SetSecurityGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetSecurityGroups)

responseSetRulePriorities :: SetRulePrioritiesResponse -> TestTree
responseSetRulePriorities =
  res
    "SetRulePrioritiesResponse"
    "fixture/SetRulePrioritiesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetRulePriorities)

responseDescribeTargetGroups :: DescribeTargetGroupsResponse -> TestTree
responseDescribeTargetGroups =
  res
    "DescribeTargetGroupsResponse"
    "fixture/DescribeTargetGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTargetGroups)

responseDescribeRules :: DescribeRulesResponse -> TestTree
responseDescribeRules =
  res
    "DescribeRulesResponse"
    "fixture/DescribeRulesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeRules)

responseDeleteLoadBalancer :: DeleteLoadBalancerResponse -> TestTree
responseDeleteLoadBalancer =
  res
    "DeleteLoadBalancerResponse"
    "fixture/DeleteLoadBalancerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteLoadBalancer)

responseRegisterTargets :: RegisterTargetsResponse -> TestTree
responseRegisterTargets =
  res
    "RegisterTargetsResponse"
    "fixture/RegisterTargetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RegisterTargets)

responseModifyListener :: ModifyListenerResponse -> TestTree
responseModifyListener =
  res
    "ModifyListenerResponse"
    "fixture/ModifyListenerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyListener)

responseModifyTargetGroup :: ModifyTargetGroupResponse -> TestTree
responseModifyTargetGroup =
  res
    "ModifyTargetGroupResponse"
    "fixture/ModifyTargetGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyTargetGroup)

responseModifyTargetGroupAttributes :: ModifyTargetGroupAttributesResponse -> TestTree
responseModifyTargetGroupAttributes =
  res
    "ModifyTargetGroupAttributesResponse"
    "fixture/ModifyTargetGroupAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyTargetGroupAttributes)

responseDescribeTargetGroupAttributes :: DescribeTargetGroupAttributesResponse -> TestTree
responseDescribeTargetGroupAttributes =
  res
    "DescribeTargetGroupAttributesResponse"
    "fixture/DescribeTargetGroupAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTargetGroupAttributes)

responseDeleteListener :: DeleteListenerResponse -> TestTree
responseDeleteListener =
  res
    "DeleteListenerResponse"
    "fixture/DeleteListenerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteListener)

responseDescribeSSLPolicies :: DescribeSSLPoliciesResponse -> TestTree
responseDescribeSSLPolicies =
  res
    "DescribeSSLPoliciesResponse"
    "fixture/DescribeSSLPoliciesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeSSLPolicies)

responseDescribeAccountLimits :: DescribeAccountLimitsResponse -> TestTree
responseDescribeAccountLimits =
  res
    "DescribeAccountLimitsResponse"
    "fixture/DescribeAccountLimitsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeAccountLimits)

responseDeregisterTargets :: DeregisterTargetsResponse -> TestTree
responseDeregisterTargets =
  res
    "DeregisterTargetsResponse"
    "fixture/DeregisterTargetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeregisterTargets)

responseCreateListener :: CreateListenerResponse -> TestTree
responseCreateListener =
  res
    "CreateListenerResponse"
    "fixture/CreateListenerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateListener)

responseCreateTargetGroup :: CreateTargetGroupResponse -> TestTree
responseCreateTargetGroup =
  res
    "CreateTargetGroupResponse"
    "fixture/CreateTargetGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTargetGroup)

responseModifyLoadBalancerAttributes :: ModifyLoadBalancerAttributesResponse -> TestTree
responseModifyLoadBalancerAttributes =
  res
    "ModifyLoadBalancerAttributesResponse"
    "fixture/ModifyLoadBalancerAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyLoadBalancerAttributes)

responseSetIpAddressType :: SetIpAddressTypeResponse -> TestTree
responseSetIpAddressType =
  res
    "SetIpAddressTypeResponse"
    "fixture/SetIpAddressTypeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetIpAddressType)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddTags)

responseDescribeLoadBalancerAttributes :: DescribeLoadBalancerAttributesResponse -> TestTree
responseDescribeLoadBalancerAttributes =
  res
    "DescribeLoadBalancerAttributesResponse"
    "fixture/DescribeLoadBalancerAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeLoadBalancerAttributes)

responseDescribeListeners :: DescribeListenersResponse -> TestTree
responseDescribeListeners =
  res
    "DescribeListenersResponse"
    "fixture/DescribeListenersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeListeners)

responseDescribeTargetHealth :: DescribeTargetHealthResponse -> TestTree
responseDescribeTargetHealth =
  res
    "DescribeTargetHealthResponse"
    "fixture/DescribeTargetHealthResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTargetHealth)

responseCreateLoadBalancer :: CreateLoadBalancerResponse -> TestTree
responseCreateLoadBalancer =
  res
    "CreateLoadBalancerResponse"
    "fixture/CreateLoadBalancerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateLoadBalancer)

responseRemoveListenerCertificates :: RemoveListenerCertificatesResponse -> TestTree
responseRemoveListenerCertificates =
  res
    "RemoveListenerCertificatesResponse"
    "fixture/RemoveListenerCertificatesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RemoveListenerCertificates)

responseModifyRule :: ModifyRuleResponse -> TestTree
responseModifyRule =
  res
    "ModifyRuleResponse"
    "fixture/ModifyRuleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyRule)

responseAddListenerCertificates :: AddListenerCertificatesResponse -> TestTree
responseAddListenerCertificates =
  res
    "AddListenerCertificatesResponse"
    "fixture/AddListenerCertificatesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddListenerCertificates)
