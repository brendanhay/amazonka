{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ELBv2
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestDescribeSSLPolicies $
--             newDescribeSSLPolicies
--
--         , requestRemoveTags $
--             newRemoveTags
--
--         , requestDeleteRule $
--             newDeleteRule
--
--         , requestDescribeTags $
--             newDescribeTags
--
--         , requestDescribeTargetGroupAttributes $
--             newDescribeTargetGroupAttributes
--
--         , requestAddListenerCertificates $
--             newAddListenerCertificates
--
--         , requestCreateLoadBalancer $
--             newCreateLoadBalancer
--
--         , requestRemoveListenerCertificates $
--             newRemoveListenerCertificates
--
--         , requestModifyRule $
--             newModifyRule
--
--         , requestModifyTargetGroup $
--             newModifyTargetGroup
--
--         , requestDeleteLoadBalancer $
--             newDeleteLoadBalancer
--
--         , requestDescribeListeners $
--             newDescribeListeners
--
--         , requestAddTags $
--             newAddTags
--
--         , requestDescribeTargetGroups $
--             newDescribeTargetGroups
--
--         , requestSetIpAddressType $
--             newSetIpAddressType
--
--         , requestModifyLoadBalancerAttributes $
--             newModifyLoadBalancerAttributes
--
--         , requestDescribeAccountLimits $
--             newDescribeAccountLimits
--
--         , requestCreateRule $
--             newCreateRule
--
--         , requestCreateTargetGroup $
--             newCreateTargetGroup
--
--         , requestSetSubnets $
--             newSetSubnets
--
--         , requestDeregisterTargets $
--             newDeregisterTargets
--
--         , requestDeleteListener $
--             newDeleteListener
--
--         , requestDeleteTargetGroup $
--             newDeleteTargetGroup
--
--         , requestDescribeLoadBalancers $
--             newDescribeLoadBalancers
--
--         , requestModifyTargetGroupAttributes $
--             newModifyTargetGroupAttributes
--
--         , requestModifyListener $
--             newModifyListener
--
--         , requestRegisterTargets $
--             newRegisterTargets
--
--         , requestDescribeTargetHealth $
--             newDescribeTargetHealth
--
--         , requestSetRulePriorities $
--             newSetRulePriorities
--
--         , requestDescribeRules $
--             newDescribeRules
--
--         , requestSetSecurityGroups $
--             newSetSecurityGroups
--
--         , requestDescribeLoadBalancerAttributes $
--             newDescribeLoadBalancerAttributes
--
--         , requestDescribeListenerCertificates $
--             newDescribeListenerCertificates
--
--         , requestCreateListener $
--             newCreateListener
--
--           ]

--     , testGroup "response"
--         [ responseDescribeSSLPolicies $
--             newDescribeSSLPoliciesResponse
--
--         , responseRemoveTags $
--             newRemoveTagsResponse
--
--         , responseDeleteRule $
--             newDeleteRuleResponse
--
--         , responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseDescribeTargetGroupAttributes $
--             newDescribeTargetGroupAttributesResponse
--
--         , responseAddListenerCertificates $
--             newAddListenerCertificatesResponse
--
--         , responseCreateLoadBalancer $
--             newCreateLoadBalancerResponse
--
--         , responseRemoveListenerCertificates $
--             newRemoveListenerCertificatesResponse
--
--         , responseModifyRule $
--             newModifyRuleResponse
--
--         , responseModifyTargetGroup $
--             newModifyTargetGroupResponse
--
--         , responseDeleteLoadBalancer $
--             newDeleteLoadBalancerResponse
--
--         , responseDescribeListeners $
--             newDescribeListenersResponse
--
--         , responseAddTags $
--             newAddTagsResponse
--
--         , responseDescribeTargetGroups $
--             newDescribeTargetGroupsResponse
--
--         , responseSetIpAddressType $
--             newSetIpAddressTypeResponse
--
--         , responseModifyLoadBalancerAttributes $
--             newModifyLoadBalancerAttributesResponse
--
--         , responseDescribeAccountLimits $
--             newDescribeAccountLimitsResponse
--
--         , responseCreateRule $
--             newCreateRuleResponse
--
--         , responseCreateTargetGroup $
--             newCreateTargetGroupResponse
--
--         , responseSetSubnets $
--             newSetSubnetsResponse
--
--         , responseDeregisterTargets $
--             newDeregisterTargetsResponse
--
--         , responseDeleteListener $
--             newDeleteListenerResponse
--
--         , responseDeleteTargetGroup $
--             newDeleteTargetGroupResponse
--
--         , responseDescribeLoadBalancers $
--             newDescribeLoadBalancersResponse
--
--         , responseModifyTargetGroupAttributes $
--             newModifyTargetGroupAttributesResponse
--
--         , responseModifyListener $
--             newModifyListenerResponse
--
--         , responseRegisterTargets $
--             newRegisterTargetsResponse
--
--         , responseDescribeTargetHealth $
--             newDescribeTargetHealthResponse
--
--         , responseSetRulePriorities $
--             newSetRulePrioritiesResponse
--
--         , responseDescribeRules $
--             newDescribeRulesResponse
--
--         , responseSetSecurityGroups $
--             newSetSecurityGroupsResponse
--
--         , responseDescribeLoadBalancerAttributes $
--             newDescribeLoadBalancerAttributesResponse
--
--         , responseDescribeListenerCertificates $
--             newDescribeListenerCertificatesResponse
--
--         , responseCreateListener $
--             newCreateListenerResponse
--
--           ]
--     ]

-- Requests

requestDescribeSSLPolicies :: DescribeSSLPolicies -> TestTree
requestDescribeSSLPolicies =
  req
    "DescribeSSLPolicies"
    "fixture/DescribeSSLPolicies.yaml"

requestRemoveTags :: RemoveTags -> TestTree
requestRemoveTags =
  req
    "RemoveTags"
    "fixture/RemoveTags.yaml"

requestDeleteRule :: DeleteRule -> TestTree
requestDeleteRule =
  req
    "DeleteRule"
    "fixture/DeleteRule.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags =
  req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestDescribeTargetGroupAttributes :: DescribeTargetGroupAttributes -> TestTree
requestDescribeTargetGroupAttributes =
  req
    "DescribeTargetGroupAttributes"
    "fixture/DescribeTargetGroupAttributes.yaml"

requestAddListenerCertificates :: AddListenerCertificates -> TestTree
requestAddListenerCertificates =
  req
    "AddListenerCertificates"
    "fixture/AddListenerCertificates.yaml"

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

requestModifyTargetGroup :: ModifyTargetGroup -> TestTree
requestModifyTargetGroup =
  req
    "ModifyTargetGroup"
    "fixture/ModifyTargetGroup.yaml"

requestDeleteLoadBalancer :: DeleteLoadBalancer -> TestTree
requestDeleteLoadBalancer =
  req
    "DeleteLoadBalancer"
    "fixture/DeleteLoadBalancer.yaml"

requestDescribeListeners :: DescribeListeners -> TestTree
requestDescribeListeners =
  req
    "DescribeListeners"
    "fixture/DescribeListeners.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags =
  req
    "AddTags"
    "fixture/AddTags.yaml"

requestDescribeTargetGroups :: DescribeTargetGroups -> TestTree
requestDescribeTargetGroups =
  req
    "DescribeTargetGroups"
    "fixture/DescribeTargetGroups.yaml"

requestSetIpAddressType :: SetIpAddressType -> TestTree
requestSetIpAddressType =
  req
    "SetIpAddressType"
    "fixture/SetIpAddressType.yaml"

requestModifyLoadBalancerAttributes :: ModifyLoadBalancerAttributes -> TestTree
requestModifyLoadBalancerAttributes =
  req
    "ModifyLoadBalancerAttributes"
    "fixture/ModifyLoadBalancerAttributes.yaml"

requestDescribeAccountLimits :: DescribeAccountLimits -> TestTree
requestDescribeAccountLimits =
  req
    "DescribeAccountLimits"
    "fixture/DescribeAccountLimits.yaml"

requestCreateRule :: CreateRule -> TestTree
requestCreateRule =
  req
    "CreateRule"
    "fixture/CreateRule.yaml"

requestCreateTargetGroup :: CreateTargetGroup -> TestTree
requestCreateTargetGroup =
  req
    "CreateTargetGroup"
    "fixture/CreateTargetGroup.yaml"

requestSetSubnets :: SetSubnets -> TestTree
requestSetSubnets =
  req
    "SetSubnets"
    "fixture/SetSubnets.yaml"

requestDeregisterTargets :: DeregisterTargets -> TestTree
requestDeregisterTargets =
  req
    "DeregisterTargets"
    "fixture/DeregisterTargets.yaml"

requestDeleteListener :: DeleteListener -> TestTree
requestDeleteListener =
  req
    "DeleteListener"
    "fixture/DeleteListener.yaml"

requestDeleteTargetGroup :: DeleteTargetGroup -> TestTree
requestDeleteTargetGroup =
  req
    "DeleteTargetGroup"
    "fixture/DeleteTargetGroup.yaml"

requestDescribeLoadBalancers :: DescribeLoadBalancers -> TestTree
requestDescribeLoadBalancers =
  req
    "DescribeLoadBalancers"
    "fixture/DescribeLoadBalancers.yaml"

requestModifyTargetGroupAttributes :: ModifyTargetGroupAttributes -> TestTree
requestModifyTargetGroupAttributes =
  req
    "ModifyTargetGroupAttributes"
    "fixture/ModifyTargetGroupAttributes.yaml"

requestModifyListener :: ModifyListener -> TestTree
requestModifyListener =
  req
    "ModifyListener"
    "fixture/ModifyListener.yaml"

requestRegisterTargets :: RegisterTargets -> TestTree
requestRegisterTargets =
  req
    "RegisterTargets"
    "fixture/RegisterTargets.yaml"

requestDescribeTargetHealth :: DescribeTargetHealth -> TestTree
requestDescribeTargetHealth =
  req
    "DescribeTargetHealth"
    "fixture/DescribeTargetHealth.yaml"

requestSetRulePriorities :: SetRulePriorities -> TestTree
requestSetRulePriorities =
  req
    "SetRulePriorities"
    "fixture/SetRulePriorities.yaml"

requestDescribeRules :: DescribeRules -> TestTree
requestDescribeRules =
  req
    "DescribeRules"
    "fixture/DescribeRules.yaml"

requestSetSecurityGroups :: SetSecurityGroups -> TestTree
requestSetSecurityGroups =
  req
    "SetSecurityGroups"
    "fixture/SetSecurityGroups.yaml"

requestDescribeLoadBalancerAttributes :: DescribeLoadBalancerAttributes -> TestTree
requestDescribeLoadBalancerAttributes =
  req
    "DescribeLoadBalancerAttributes"
    "fixture/DescribeLoadBalancerAttributes.yaml"

requestDescribeListenerCertificates :: DescribeListenerCertificates -> TestTree
requestDescribeListenerCertificates =
  req
    "DescribeListenerCertificates"
    "fixture/DescribeListenerCertificates.yaml"

requestCreateListener :: CreateListener -> TestTree
requestCreateListener =
  req
    "CreateListener"
    "fixture/CreateListener.yaml"

-- Responses

responseDescribeSSLPolicies :: DescribeSSLPoliciesResponse -> TestTree
responseDescribeSSLPolicies =
  res
    "DescribeSSLPoliciesResponse"
    "fixture/DescribeSSLPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSSLPolicies)

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags =
  res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveTags)

responseDeleteRule :: DeleteRuleResponse -> TestTree
responseDeleteRule =
  res
    "DeleteRuleResponse"
    "fixture/DeleteRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRule)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTags)

responseDescribeTargetGroupAttributes :: DescribeTargetGroupAttributesResponse -> TestTree
responseDescribeTargetGroupAttributes =
  res
    "DescribeTargetGroupAttributesResponse"
    "fixture/DescribeTargetGroupAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTargetGroupAttributes)

responseAddListenerCertificates :: AddListenerCertificatesResponse -> TestTree
responseAddListenerCertificates =
  res
    "AddListenerCertificatesResponse"
    "fixture/AddListenerCertificatesResponse.proto"
    defaultService
    (Proxy :: Proxy AddListenerCertificates)

responseCreateLoadBalancer :: CreateLoadBalancerResponse -> TestTree
responseCreateLoadBalancer =
  res
    "CreateLoadBalancerResponse"
    "fixture/CreateLoadBalancerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLoadBalancer)

responseRemoveListenerCertificates :: RemoveListenerCertificatesResponse -> TestTree
responseRemoveListenerCertificates =
  res
    "RemoveListenerCertificatesResponse"
    "fixture/RemoveListenerCertificatesResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveListenerCertificates)

responseModifyRule :: ModifyRuleResponse -> TestTree
responseModifyRule =
  res
    "ModifyRuleResponse"
    "fixture/ModifyRuleResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyRule)

responseModifyTargetGroup :: ModifyTargetGroupResponse -> TestTree
responseModifyTargetGroup =
  res
    "ModifyTargetGroupResponse"
    "fixture/ModifyTargetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyTargetGroup)

responseDeleteLoadBalancer :: DeleteLoadBalancerResponse -> TestTree
responseDeleteLoadBalancer =
  res
    "DeleteLoadBalancerResponse"
    "fixture/DeleteLoadBalancerResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLoadBalancer)

responseDescribeListeners :: DescribeListenersResponse -> TestTree
responseDescribeListeners =
  res
    "DescribeListenersResponse"
    "fixture/DescribeListenersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeListeners)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    defaultService
    (Proxy :: Proxy AddTags)

responseDescribeTargetGroups :: DescribeTargetGroupsResponse -> TestTree
responseDescribeTargetGroups =
  res
    "DescribeTargetGroupsResponse"
    "fixture/DescribeTargetGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTargetGroups)

responseSetIpAddressType :: SetIpAddressTypeResponse -> TestTree
responseSetIpAddressType =
  res
    "SetIpAddressTypeResponse"
    "fixture/SetIpAddressTypeResponse.proto"
    defaultService
    (Proxy :: Proxy SetIpAddressType)

responseModifyLoadBalancerAttributes :: ModifyLoadBalancerAttributesResponse -> TestTree
responseModifyLoadBalancerAttributes =
  res
    "ModifyLoadBalancerAttributesResponse"
    "fixture/ModifyLoadBalancerAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyLoadBalancerAttributes)

responseDescribeAccountLimits :: DescribeAccountLimitsResponse -> TestTree
responseDescribeAccountLimits =
  res
    "DescribeAccountLimitsResponse"
    "fixture/DescribeAccountLimitsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAccountLimits)

responseCreateRule :: CreateRuleResponse -> TestTree
responseCreateRule =
  res
    "CreateRuleResponse"
    "fixture/CreateRuleResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRule)

responseCreateTargetGroup :: CreateTargetGroupResponse -> TestTree
responseCreateTargetGroup =
  res
    "CreateTargetGroupResponse"
    "fixture/CreateTargetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTargetGroup)

responseSetSubnets :: SetSubnetsResponse -> TestTree
responseSetSubnets =
  res
    "SetSubnetsResponse"
    "fixture/SetSubnetsResponse.proto"
    defaultService
    (Proxy :: Proxy SetSubnets)

responseDeregisterTargets :: DeregisterTargetsResponse -> TestTree
responseDeregisterTargets =
  res
    "DeregisterTargetsResponse"
    "fixture/DeregisterTargetsResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterTargets)

responseDeleteListener :: DeleteListenerResponse -> TestTree
responseDeleteListener =
  res
    "DeleteListenerResponse"
    "fixture/DeleteListenerResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteListener)

responseDeleteTargetGroup :: DeleteTargetGroupResponse -> TestTree
responseDeleteTargetGroup =
  res
    "DeleteTargetGroupResponse"
    "fixture/DeleteTargetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTargetGroup)

responseDescribeLoadBalancers :: DescribeLoadBalancersResponse -> TestTree
responseDescribeLoadBalancers =
  res
    "DescribeLoadBalancersResponse"
    "fixture/DescribeLoadBalancersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLoadBalancers)

responseModifyTargetGroupAttributes :: ModifyTargetGroupAttributesResponse -> TestTree
responseModifyTargetGroupAttributes =
  res
    "ModifyTargetGroupAttributesResponse"
    "fixture/ModifyTargetGroupAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyTargetGroupAttributes)

responseModifyListener :: ModifyListenerResponse -> TestTree
responseModifyListener =
  res
    "ModifyListenerResponse"
    "fixture/ModifyListenerResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyListener)

responseRegisterTargets :: RegisterTargetsResponse -> TestTree
responseRegisterTargets =
  res
    "RegisterTargetsResponse"
    "fixture/RegisterTargetsResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterTargets)

responseDescribeTargetHealth :: DescribeTargetHealthResponse -> TestTree
responseDescribeTargetHealth =
  res
    "DescribeTargetHealthResponse"
    "fixture/DescribeTargetHealthResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTargetHealth)

responseSetRulePriorities :: SetRulePrioritiesResponse -> TestTree
responseSetRulePriorities =
  res
    "SetRulePrioritiesResponse"
    "fixture/SetRulePrioritiesResponse.proto"
    defaultService
    (Proxy :: Proxy SetRulePriorities)

responseDescribeRules :: DescribeRulesResponse -> TestTree
responseDescribeRules =
  res
    "DescribeRulesResponse"
    "fixture/DescribeRulesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRules)

responseSetSecurityGroups :: SetSecurityGroupsResponse -> TestTree
responseSetSecurityGroups =
  res
    "SetSecurityGroupsResponse"
    "fixture/SetSecurityGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy SetSecurityGroups)

responseDescribeLoadBalancerAttributes :: DescribeLoadBalancerAttributesResponse -> TestTree
responseDescribeLoadBalancerAttributes =
  res
    "DescribeLoadBalancerAttributesResponse"
    "fixture/DescribeLoadBalancerAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLoadBalancerAttributes)

responseDescribeListenerCertificates :: DescribeListenerCertificatesResponse -> TestTree
responseDescribeListenerCertificates =
  res
    "DescribeListenerCertificatesResponse"
    "fixture/DescribeListenerCertificatesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeListenerCertificates)

responseCreateListener :: CreateListenerResponse -> TestTree
responseCreateListener =
  res
    "CreateListenerResponse"
    "fixture/CreateListenerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateListener)
