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
--         [ requestDeleteRule $
--             newDeleteRule
--
--         , requestDescribeSSLPolicies $
--             newDescribeSSLPolicies
--
--         , requestRemoveTags $
--             newRemoveTags
--
--         , requestDescribeTargetGroupAttributes $
--             newDescribeTargetGroupAttributes
--
--         , requestDescribeTags $
--             newDescribeTags
--
--         , requestAddListenerCertificates $
--             newAddListenerCertificates
--
--         , requestRemoveListenerCertificates $
--             newRemoveListenerCertificates
--
--         , requestModifyTargetGroup $
--             newModifyTargetGroup
--
--         , requestCreateLoadBalancer $
--             newCreateLoadBalancer
--
--         , requestModifyRule $
--             newModifyRule
--
--         , requestDeleteLoadBalancer $
--             newDeleteLoadBalancer
--
--         , requestDescribeListeners $
--             newDescribeListeners
--
--         , requestDescribeTargetGroups $
--             newDescribeTargetGroups
--
--         , requestAddTags $
--             newAddTags
--
--         , requestSetIpAddressType $
--             newSetIpAddressType
--
--         , requestModifyLoadBalancerAttributes $
--             newModifyLoadBalancerAttributes
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
--         , requestDescribeAccountLimits $
--             newDescribeAccountLimits
--
--         , requestDeregisterTargets $
--             newDeregisterTargets
--
--         , requestDeleteTargetGroup $
--             newDeleteTargetGroup
--
--         , requestDeleteListener $
--             newDeleteListener
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
--         , requestSetRulePriorities $
--             newSetRulePriorities
--
--         , requestDescribeLoadBalancerAttributes $
--             newDescribeLoadBalancerAttributes
--
--         , requestDescribeTargetHealth $
--             newDescribeTargetHealth
--
--         , requestDescribeRules $
--             newDescribeRules
--
--         , requestSetSecurityGroups $
--             newSetSecurityGroups
--
--         , requestDescribeListenerCertificates $
--             newDescribeListenerCertificates
--
--         , requestCreateListener $
--             newCreateListener
--
--           ]

--     , testGroup "response"
--         [ responseDeleteRule $
--             newDeleteRuleResponse
--
--         , responseDescribeSSLPolicies $
--             newDescribeSSLPoliciesResponse
--
--         , responseRemoveTags $
--             newRemoveTagsResponse
--
--         , responseDescribeTargetGroupAttributes $
--             newDescribeTargetGroupAttributesResponse
--
--         , responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseAddListenerCertificates $
--             newAddListenerCertificatesResponse
--
--         , responseRemoveListenerCertificates $
--             newRemoveListenerCertificatesResponse
--
--         , responseModifyTargetGroup $
--             newModifyTargetGroupResponse
--
--         , responseCreateLoadBalancer $
--             newCreateLoadBalancerResponse
--
--         , responseModifyRule $
--             newModifyRuleResponse
--
--         , responseDeleteLoadBalancer $
--             newDeleteLoadBalancerResponse
--
--         , responseDescribeListeners $
--             newDescribeListenersResponse
--
--         , responseDescribeTargetGroups $
--             newDescribeTargetGroupsResponse
--
--         , responseAddTags $
--             newAddTagsResponse
--
--         , responseSetIpAddressType $
--             newSetIpAddressTypeResponse
--
--         , responseModifyLoadBalancerAttributes $
--             newModifyLoadBalancerAttributesResponse
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
--         , responseDescribeAccountLimits $
--             newDescribeAccountLimitsResponse
--
--         , responseDeregisterTargets $
--             newDeregisterTargetsResponse
--
--         , responseDeleteTargetGroup $
--             newDeleteTargetGroupResponse
--
--         , responseDeleteListener $
--             newDeleteListenerResponse
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
--         , responseSetRulePriorities $
--             newSetRulePrioritiesResponse
--
--         , responseDescribeLoadBalancerAttributes $
--             newDescribeLoadBalancerAttributesResponse
--
--         , responseDescribeTargetHealth $
--             newDescribeTargetHealthResponse
--
--         , responseDescribeRules $
--             newDescribeRulesResponse
--
--         , responseSetSecurityGroups $
--             newSetSecurityGroupsResponse
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

requestDeleteRule :: DeleteRule -> TestTree
requestDeleteRule =
  req
    "DeleteRule"
    "fixture/DeleteRule.yaml"

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

requestDescribeTargetGroupAttributes :: DescribeTargetGroupAttributes -> TestTree
requestDescribeTargetGroupAttributes =
  req
    "DescribeTargetGroupAttributes"
    "fixture/DescribeTargetGroupAttributes.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags =
  req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestAddListenerCertificates :: AddListenerCertificates -> TestTree
requestAddListenerCertificates =
  req
    "AddListenerCertificates"
    "fixture/AddListenerCertificates.yaml"

requestRemoveListenerCertificates :: RemoveListenerCertificates -> TestTree
requestRemoveListenerCertificates =
  req
    "RemoveListenerCertificates"
    "fixture/RemoveListenerCertificates.yaml"

requestModifyTargetGroup :: ModifyTargetGroup -> TestTree
requestModifyTargetGroup =
  req
    "ModifyTargetGroup"
    "fixture/ModifyTargetGroup.yaml"

requestCreateLoadBalancer :: CreateLoadBalancer -> TestTree
requestCreateLoadBalancer =
  req
    "CreateLoadBalancer"
    "fixture/CreateLoadBalancer.yaml"

requestModifyRule :: ModifyRule -> TestTree
requestModifyRule =
  req
    "ModifyRule"
    "fixture/ModifyRule.yaml"

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

requestDescribeTargetGroups :: DescribeTargetGroups -> TestTree
requestDescribeTargetGroups =
  req
    "DescribeTargetGroups"
    "fixture/DescribeTargetGroups.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags =
  req
    "AddTags"
    "fixture/AddTags.yaml"

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

requestDeleteTargetGroup :: DeleteTargetGroup -> TestTree
requestDeleteTargetGroup =
  req
    "DeleteTargetGroup"
    "fixture/DeleteTargetGroup.yaml"

requestDeleteListener :: DeleteListener -> TestTree
requestDeleteListener =
  req
    "DeleteListener"
    "fixture/DeleteListener.yaml"

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

requestSetRulePriorities :: SetRulePriorities -> TestTree
requestSetRulePriorities =
  req
    "SetRulePriorities"
    "fixture/SetRulePriorities.yaml"

requestDescribeLoadBalancerAttributes :: DescribeLoadBalancerAttributes -> TestTree
requestDescribeLoadBalancerAttributes =
  req
    "DescribeLoadBalancerAttributes"
    "fixture/DescribeLoadBalancerAttributes.yaml"

requestDescribeTargetHealth :: DescribeTargetHealth -> TestTree
requestDescribeTargetHealth =
  req
    "DescribeTargetHealth"
    "fixture/DescribeTargetHealth.yaml"

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

responseDeleteRule :: DeleteRuleResponse -> TestTree
responseDeleteRule =
  res
    "DeleteRuleResponse"
    "fixture/DeleteRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRule)

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

responseDescribeTargetGroupAttributes :: DescribeTargetGroupAttributesResponse -> TestTree
responseDescribeTargetGroupAttributes =
  res
    "DescribeTargetGroupAttributesResponse"
    "fixture/DescribeTargetGroupAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTargetGroupAttributes)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTags)

responseAddListenerCertificates :: AddListenerCertificatesResponse -> TestTree
responseAddListenerCertificates =
  res
    "AddListenerCertificatesResponse"
    "fixture/AddListenerCertificatesResponse.proto"
    defaultService
    (Proxy :: Proxy AddListenerCertificates)

responseRemoveListenerCertificates :: RemoveListenerCertificatesResponse -> TestTree
responseRemoveListenerCertificates =
  res
    "RemoveListenerCertificatesResponse"
    "fixture/RemoveListenerCertificatesResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveListenerCertificates)

responseModifyTargetGroup :: ModifyTargetGroupResponse -> TestTree
responseModifyTargetGroup =
  res
    "ModifyTargetGroupResponse"
    "fixture/ModifyTargetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyTargetGroup)

responseCreateLoadBalancer :: CreateLoadBalancerResponse -> TestTree
responseCreateLoadBalancer =
  res
    "CreateLoadBalancerResponse"
    "fixture/CreateLoadBalancerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLoadBalancer)

responseModifyRule :: ModifyRuleResponse -> TestTree
responseModifyRule =
  res
    "ModifyRuleResponse"
    "fixture/ModifyRuleResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyRule)

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

responseDescribeTargetGroups :: DescribeTargetGroupsResponse -> TestTree
responseDescribeTargetGroups =
  res
    "DescribeTargetGroupsResponse"
    "fixture/DescribeTargetGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTargetGroups)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    defaultService
    (Proxy :: Proxy AddTags)

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

responseDescribeAccountLimits :: DescribeAccountLimitsResponse -> TestTree
responseDescribeAccountLimits =
  res
    "DescribeAccountLimitsResponse"
    "fixture/DescribeAccountLimitsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAccountLimits)

responseDeregisterTargets :: DeregisterTargetsResponse -> TestTree
responseDeregisterTargets =
  res
    "DeregisterTargetsResponse"
    "fixture/DeregisterTargetsResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterTargets)

responseDeleteTargetGroup :: DeleteTargetGroupResponse -> TestTree
responseDeleteTargetGroup =
  res
    "DeleteTargetGroupResponse"
    "fixture/DeleteTargetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTargetGroup)

responseDeleteListener :: DeleteListenerResponse -> TestTree
responseDeleteListener =
  res
    "DeleteListenerResponse"
    "fixture/DeleteListenerResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteListener)

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

responseSetRulePriorities :: SetRulePrioritiesResponse -> TestTree
responseSetRulePriorities =
  res
    "SetRulePrioritiesResponse"
    "fixture/SetRulePrioritiesResponse.proto"
    defaultService
    (Proxy :: Proxy SetRulePriorities)

responseDescribeLoadBalancerAttributes :: DescribeLoadBalancerAttributesResponse -> TestTree
responseDescribeLoadBalancerAttributes =
  res
    "DescribeLoadBalancerAttributesResponse"
    "fixture/DescribeLoadBalancerAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLoadBalancerAttributes)

responseDescribeTargetHealth :: DescribeTargetHealthResponse -> TestTree
responseDescribeTargetHealth =
  res
    "DescribeTargetHealthResponse"
    "fixture/DescribeTargetHealthResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTargetHealth)

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
