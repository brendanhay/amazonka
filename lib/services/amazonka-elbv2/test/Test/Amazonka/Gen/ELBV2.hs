{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ELBV2
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ELBV2 where

import Amazonka.ELBV2
import qualified Data.Proxy as Proxy
import Test.Amazonka.ELBV2.Internal
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
--         [ requestDescribeLoadBalancers $
--             newDescribeLoadBalancers
--
--         , requestDescribeTags $
--             newDescribeTags
--
--         , requestDeleteRule $
--             newDeleteRule
--
--         , requestRemoveTags $
--             newRemoveTags
--
--         , requestDeleteTargetGroup $
--             newDeleteTargetGroup
--
--         , requestSetSubnets $
--             newSetSubnets
--
--         , requestCreateRule $
--             newCreateRule
--
--         , requestDescribeListenerCertificates $
--             newDescribeListenerCertificates
--
--         , requestSetSecurityGroups $
--             newSetSecurityGroups
--
--         , requestSetRulePriorities $
--             newSetRulePriorities
--
--         , requestDescribeTargetGroups $
--             newDescribeTargetGroups
--
--         , requestDescribeRules $
--             newDescribeRules
--
--         , requestDeleteLoadBalancer $
--             newDeleteLoadBalancer
--
--         , requestRegisterTargets $
--             newRegisterTargets
--
--         , requestModifyListener $
--             newModifyListener
--
--         , requestModifyTargetGroup $
--             newModifyTargetGroup
--
--         , requestModifyTargetGroupAttributes $
--             newModifyTargetGroupAttributes
--
--         , requestDescribeTargetGroupAttributes $
--             newDescribeTargetGroupAttributes
--
--         , requestDeleteListener $
--             newDeleteListener
--
--         , requestDescribeSSLPolicies $
--             newDescribeSSLPolicies
--
--         , requestDescribeAccountLimits $
--             newDescribeAccountLimits
--
--         , requestDeregisterTargets $
--             newDeregisterTargets
--
--         , requestCreateListener $
--             newCreateListener
--
--         , requestCreateTargetGroup $
--             newCreateTargetGroup
--
--         , requestModifyLoadBalancerAttributes $
--             newModifyLoadBalancerAttributes
--
--         , requestSetIpAddressType $
--             newSetIpAddressType
--
--         , requestAddTags $
--             newAddTags
--
--         , requestDescribeLoadBalancerAttributes $
--             newDescribeLoadBalancerAttributes
--
--         , requestDescribeListeners $
--             newDescribeListeners
--
--         , requestDescribeTargetHealth $
--             newDescribeTargetHealth
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
--         , requestAddListenerCertificates $
--             newAddListenerCertificates
--
--           ]

--     , testGroup "response"
--         [ responseDescribeLoadBalancers $
--             newDescribeLoadBalancersResponse
--
--         , responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseDeleteRule $
--             newDeleteRuleResponse
--
--         , responseRemoveTags $
--             newRemoveTagsResponse
--
--         , responseDeleteTargetGroup $
--             newDeleteTargetGroupResponse
--
--         , responseSetSubnets $
--             newSetSubnetsResponse
--
--         , responseCreateRule $
--             newCreateRuleResponse
--
--         , responseDescribeListenerCertificates $
--             newDescribeListenerCertificatesResponse
--
--         , responseSetSecurityGroups $
--             newSetSecurityGroupsResponse
--
--         , responseSetRulePriorities $
--             newSetRulePrioritiesResponse
--
--         , responseDescribeTargetGroups $
--             newDescribeTargetGroupsResponse
--
--         , responseDescribeRules $
--             newDescribeRulesResponse
--
--         , responseDeleteLoadBalancer $
--             newDeleteLoadBalancerResponse
--
--         , responseRegisterTargets $
--             newRegisterTargetsResponse
--
--         , responseModifyListener $
--             newModifyListenerResponse
--
--         , responseModifyTargetGroup $
--             newModifyTargetGroupResponse
--
--         , responseModifyTargetGroupAttributes $
--             newModifyTargetGroupAttributesResponse
--
--         , responseDescribeTargetGroupAttributes $
--             newDescribeTargetGroupAttributesResponse
--
--         , responseDeleteListener $
--             newDeleteListenerResponse
--
--         , responseDescribeSSLPolicies $
--             newDescribeSSLPoliciesResponse
--
--         , responseDescribeAccountLimits $
--             newDescribeAccountLimitsResponse
--
--         , responseDeregisterTargets $
--             newDeregisterTargetsResponse
--
--         , responseCreateListener $
--             newCreateListenerResponse
--
--         , responseCreateTargetGroup $
--             newCreateTargetGroupResponse
--
--         , responseModifyLoadBalancerAttributes $
--             newModifyLoadBalancerAttributesResponse
--
--         , responseSetIpAddressType $
--             newSetIpAddressTypeResponse
--
--         , responseAddTags $
--             newAddTagsResponse
--
--         , responseDescribeLoadBalancerAttributes $
--             newDescribeLoadBalancerAttributesResponse
--
--         , responseDescribeListeners $
--             newDescribeListenersResponse
--
--         , responseDescribeTargetHealth $
--             newDescribeTargetHealthResponse
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
--         , responseAddListenerCertificates $
--             newAddListenerCertificatesResponse
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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLoadBalancers)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTags)

responseDeleteRule :: DeleteRuleResponse -> TestTree
responseDeleteRule =
  res
    "DeleteRuleResponse"
    "fixture/DeleteRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRule)

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags =
  res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTags)

responseDeleteTargetGroup :: DeleteTargetGroupResponse -> TestTree
responseDeleteTargetGroup =
  res
    "DeleteTargetGroupResponse"
    "fixture/DeleteTargetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTargetGroup)

responseSetSubnets :: SetSubnetsResponse -> TestTree
responseSetSubnets =
  res
    "SetSubnetsResponse"
    "fixture/SetSubnetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetSubnets)

responseCreateRule :: CreateRuleResponse -> TestTree
responseCreateRule =
  res
    "CreateRuleResponse"
    "fixture/CreateRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRule)

responseDescribeListenerCertificates :: DescribeListenerCertificatesResponse -> TestTree
responseDescribeListenerCertificates =
  res
    "DescribeListenerCertificatesResponse"
    "fixture/DescribeListenerCertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeListenerCertificates)

responseSetSecurityGroups :: SetSecurityGroupsResponse -> TestTree
responseSetSecurityGroups =
  res
    "SetSecurityGroupsResponse"
    "fixture/SetSecurityGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetSecurityGroups)

responseSetRulePriorities :: SetRulePrioritiesResponse -> TestTree
responseSetRulePriorities =
  res
    "SetRulePrioritiesResponse"
    "fixture/SetRulePrioritiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetRulePriorities)

responseDescribeTargetGroups :: DescribeTargetGroupsResponse -> TestTree
responseDescribeTargetGroups =
  res
    "DescribeTargetGroupsResponse"
    "fixture/DescribeTargetGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTargetGroups)

responseDescribeRules :: DescribeRulesResponse -> TestTree
responseDescribeRules =
  res
    "DescribeRulesResponse"
    "fixture/DescribeRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRules)

responseDeleteLoadBalancer :: DeleteLoadBalancerResponse -> TestTree
responseDeleteLoadBalancer =
  res
    "DeleteLoadBalancerResponse"
    "fixture/DeleteLoadBalancerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLoadBalancer)

responseRegisterTargets :: RegisterTargetsResponse -> TestTree
responseRegisterTargets =
  res
    "RegisterTargetsResponse"
    "fixture/RegisterTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterTargets)

responseModifyListener :: ModifyListenerResponse -> TestTree
responseModifyListener =
  res
    "ModifyListenerResponse"
    "fixture/ModifyListenerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyListener)

responseModifyTargetGroup :: ModifyTargetGroupResponse -> TestTree
responseModifyTargetGroup =
  res
    "ModifyTargetGroupResponse"
    "fixture/ModifyTargetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyTargetGroup)

responseModifyTargetGroupAttributes :: ModifyTargetGroupAttributesResponse -> TestTree
responseModifyTargetGroupAttributes =
  res
    "ModifyTargetGroupAttributesResponse"
    "fixture/ModifyTargetGroupAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyTargetGroupAttributes)

responseDescribeTargetGroupAttributes :: DescribeTargetGroupAttributesResponse -> TestTree
responseDescribeTargetGroupAttributes =
  res
    "DescribeTargetGroupAttributesResponse"
    "fixture/DescribeTargetGroupAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTargetGroupAttributes)

responseDeleteListener :: DeleteListenerResponse -> TestTree
responseDeleteListener =
  res
    "DeleteListenerResponse"
    "fixture/DeleteListenerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteListener)

responseDescribeSSLPolicies :: DescribeSSLPoliciesResponse -> TestTree
responseDescribeSSLPolicies =
  res
    "DescribeSSLPoliciesResponse"
    "fixture/DescribeSSLPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSSLPolicies)

responseDescribeAccountLimits :: DescribeAccountLimitsResponse -> TestTree
responseDescribeAccountLimits =
  res
    "DescribeAccountLimitsResponse"
    "fixture/DescribeAccountLimitsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountLimits)

responseDeregisterTargets :: DeregisterTargetsResponse -> TestTree
responseDeregisterTargets =
  res
    "DeregisterTargetsResponse"
    "fixture/DeregisterTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterTargets)

responseCreateListener :: CreateListenerResponse -> TestTree
responseCreateListener =
  res
    "CreateListenerResponse"
    "fixture/CreateListenerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateListener)

responseCreateTargetGroup :: CreateTargetGroupResponse -> TestTree
responseCreateTargetGroup =
  res
    "CreateTargetGroupResponse"
    "fixture/CreateTargetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTargetGroup)

responseModifyLoadBalancerAttributes :: ModifyLoadBalancerAttributesResponse -> TestTree
responseModifyLoadBalancerAttributes =
  res
    "ModifyLoadBalancerAttributesResponse"
    "fixture/ModifyLoadBalancerAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyLoadBalancerAttributes)

responseSetIpAddressType :: SetIpAddressTypeResponse -> TestTree
responseSetIpAddressType =
  res
    "SetIpAddressTypeResponse"
    "fixture/SetIpAddressTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetIpAddressType)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTags)

responseDescribeLoadBalancerAttributes :: DescribeLoadBalancerAttributesResponse -> TestTree
responseDescribeLoadBalancerAttributes =
  res
    "DescribeLoadBalancerAttributesResponse"
    "fixture/DescribeLoadBalancerAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLoadBalancerAttributes)

responseDescribeListeners :: DescribeListenersResponse -> TestTree
responseDescribeListeners =
  res
    "DescribeListenersResponse"
    "fixture/DescribeListenersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeListeners)

responseDescribeTargetHealth :: DescribeTargetHealthResponse -> TestTree
responseDescribeTargetHealth =
  res
    "DescribeTargetHealthResponse"
    "fixture/DescribeTargetHealthResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTargetHealth)

responseCreateLoadBalancer :: CreateLoadBalancerResponse -> TestTree
responseCreateLoadBalancer =
  res
    "CreateLoadBalancerResponse"
    "fixture/CreateLoadBalancerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLoadBalancer)

responseRemoveListenerCertificates :: RemoveListenerCertificatesResponse -> TestTree
responseRemoveListenerCertificates =
  res
    "RemoveListenerCertificatesResponse"
    "fixture/RemoveListenerCertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveListenerCertificates)

responseModifyRule :: ModifyRuleResponse -> TestTree
responseModifyRule =
  res
    "ModifyRuleResponse"
    "fixture/ModifyRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyRule)

responseAddListenerCertificates :: AddListenerCertificatesResponse -> TestTree
responseAddListenerCertificates =
  res
    "AddListenerCertificatesResponse"
    "fixture/AddListenerCertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddListenerCertificates)
