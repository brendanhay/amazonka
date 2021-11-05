{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.NetworkFirewall
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.NetworkFirewall where

import Amazonka.NetworkFirewall
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.NetworkFirewall.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateSubnets $
--             newAssociateSubnets
--
--         , requestUpdateSubnetChangeProtection $
--             newUpdateSubnetChangeProtection
--
--         , requestUpdateFirewallPolicy $
--             newUpdateFirewallPolicy
--
--         , requestDeleteFirewallPolicy $
--             newDeleteFirewallPolicy
--
--         , requestCreateFirewallPolicy $
--             newCreateFirewallPolicy
--
--         , requestUpdateLoggingConfiguration $
--             newUpdateLoggingConfiguration
--
--         , requestDisassociateSubnets $
--             newDisassociateSubnets
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListFirewallPolicies $
--             newListFirewallPolicies
--
--         , requestUpdateFirewallDeleteProtection $
--             newUpdateFirewallDeleteProtection
--
--         , requestCreateRuleGroup $
--             newCreateRuleGroup
--
--         , requestDescribeFirewallPolicy $
--             newDescribeFirewallPolicy
--
--         , requestUpdateFirewallDescription $
--             newUpdateFirewallDescription
--
--         , requestDescribeRuleGroup $
--             newDescribeRuleGroup
--
--         , requestDeleteFirewall $
--             newDeleteFirewall
--
--         , requestListFirewalls $
--             newListFirewalls
--
--         , requestDescribeResourcePolicy $
--             newDescribeResourcePolicy
--
--         , requestAssociateFirewallPolicy $
--             newAssociateFirewallPolicy
--
--         , requestUpdateFirewallPolicyChangeProtection $
--             newUpdateFirewallPolicyChangeProtection
--
--         , requestCreateFirewall $
--             newCreateFirewall
--
--         , requestListRuleGroups $
--             newListRuleGroups
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDeleteRuleGroup $
--             newDeleteRuleGroup
--
--         , requestUpdateRuleGroup $
--             newUpdateRuleGroup
--
--         , requestPutResourcePolicy $
--             newPutResourcePolicy
--
--         , requestDescribeFirewall $
--             newDescribeFirewall
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeLoggingConfiguration $
--             newDescribeLoggingConfiguration
--
--           ]

--     , testGroup "response"
--         [ responseAssociateSubnets $
--             newAssociateSubnetsResponse
--
--         , responseUpdateSubnetChangeProtection $
--             newUpdateSubnetChangeProtectionResponse
--
--         , responseUpdateFirewallPolicy $
--             newUpdateFirewallPolicyResponse
--
--         , responseDeleteFirewallPolicy $
--             newDeleteFirewallPolicyResponse
--
--         , responseCreateFirewallPolicy $
--             newCreateFirewallPolicyResponse
--
--         , responseUpdateLoggingConfiguration $
--             newUpdateLoggingConfigurationResponse
--
--         , responseDisassociateSubnets $
--             newDisassociateSubnetsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListFirewallPolicies $
--             newListFirewallPoliciesResponse
--
--         , responseUpdateFirewallDeleteProtection $
--             newUpdateFirewallDeleteProtectionResponse
--
--         , responseCreateRuleGroup $
--             newCreateRuleGroupResponse
--
--         , responseDescribeFirewallPolicy $
--             newDescribeFirewallPolicyResponse
--
--         , responseUpdateFirewallDescription $
--             newUpdateFirewallDescriptionResponse
--
--         , responseDescribeRuleGroup $
--             newDescribeRuleGroupResponse
--
--         , responseDeleteFirewall $
--             newDeleteFirewallResponse
--
--         , responseListFirewalls $
--             newListFirewallsResponse
--
--         , responseDescribeResourcePolicy $
--             newDescribeResourcePolicyResponse
--
--         , responseAssociateFirewallPolicy $
--             newAssociateFirewallPolicyResponse
--
--         , responseUpdateFirewallPolicyChangeProtection $
--             newUpdateFirewallPolicyChangeProtectionResponse
--
--         , responseCreateFirewall $
--             newCreateFirewallResponse
--
--         , responseListRuleGroups $
--             newListRuleGroupsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDeleteRuleGroup $
--             newDeleteRuleGroupResponse
--
--         , responseUpdateRuleGroup $
--             newUpdateRuleGroupResponse
--
--         , responsePutResourcePolicy $
--             newPutResourcePolicyResponse
--
--         , responseDescribeFirewall $
--             newDescribeFirewallResponse
--
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeLoggingConfiguration $
--             newDescribeLoggingConfigurationResponse
--
--           ]
--     ]

-- Requests

requestAssociateSubnets :: AssociateSubnets -> TestTree
requestAssociateSubnets =
  req
    "AssociateSubnets"
    "fixture/AssociateSubnets.yaml"

requestUpdateSubnetChangeProtection :: UpdateSubnetChangeProtection -> TestTree
requestUpdateSubnetChangeProtection =
  req
    "UpdateSubnetChangeProtection"
    "fixture/UpdateSubnetChangeProtection.yaml"

requestUpdateFirewallPolicy :: UpdateFirewallPolicy -> TestTree
requestUpdateFirewallPolicy =
  req
    "UpdateFirewallPolicy"
    "fixture/UpdateFirewallPolicy.yaml"

requestDeleteFirewallPolicy :: DeleteFirewallPolicy -> TestTree
requestDeleteFirewallPolicy =
  req
    "DeleteFirewallPolicy"
    "fixture/DeleteFirewallPolicy.yaml"

requestCreateFirewallPolicy :: CreateFirewallPolicy -> TestTree
requestCreateFirewallPolicy =
  req
    "CreateFirewallPolicy"
    "fixture/CreateFirewallPolicy.yaml"

requestUpdateLoggingConfiguration :: UpdateLoggingConfiguration -> TestTree
requestUpdateLoggingConfiguration =
  req
    "UpdateLoggingConfiguration"
    "fixture/UpdateLoggingConfiguration.yaml"

requestDisassociateSubnets :: DisassociateSubnets -> TestTree
requestDisassociateSubnets =
  req
    "DisassociateSubnets"
    "fixture/DisassociateSubnets.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListFirewallPolicies :: ListFirewallPolicies -> TestTree
requestListFirewallPolicies =
  req
    "ListFirewallPolicies"
    "fixture/ListFirewallPolicies.yaml"

requestUpdateFirewallDeleteProtection :: UpdateFirewallDeleteProtection -> TestTree
requestUpdateFirewallDeleteProtection =
  req
    "UpdateFirewallDeleteProtection"
    "fixture/UpdateFirewallDeleteProtection.yaml"

requestCreateRuleGroup :: CreateRuleGroup -> TestTree
requestCreateRuleGroup =
  req
    "CreateRuleGroup"
    "fixture/CreateRuleGroup.yaml"

requestDescribeFirewallPolicy :: DescribeFirewallPolicy -> TestTree
requestDescribeFirewallPolicy =
  req
    "DescribeFirewallPolicy"
    "fixture/DescribeFirewallPolicy.yaml"

requestUpdateFirewallDescription :: UpdateFirewallDescription -> TestTree
requestUpdateFirewallDescription =
  req
    "UpdateFirewallDescription"
    "fixture/UpdateFirewallDescription.yaml"

requestDescribeRuleGroup :: DescribeRuleGroup -> TestTree
requestDescribeRuleGroup =
  req
    "DescribeRuleGroup"
    "fixture/DescribeRuleGroup.yaml"

requestDeleteFirewall :: DeleteFirewall -> TestTree
requestDeleteFirewall =
  req
    "DeleteFirewall"
    "fixture/DeleteFirewall.yaml"

requestListFirewalls :: ListFirewalls -> TestTree
requestListFirewalls =
  req
    "ListFirewalls"
    "fixture/ListFirewalls.yaml"

requestDescribeResourcePolicy :: DescribeResourcePolicy -> TestTree
requestDescribeResourcePolicy =
  req
    "DescribeResourcePolicy"
    "fixture/DescribeResourcePolicy.yaml"

requestAssociateFirewallPolicy :: AssociateFirewallPolicy -> TestTree
requestAssociateFirewallPolicy =
  req
    "AssociateFirewallPolicy"
    "fixture/AssociateFirewallPolicy.yaml"

requestUpdateFirewallPolicyChangeProtection :: UpdateFirewallPolicyChangeProtection -> TestTree
requestUpdateFirewallPolicyChangeProtection =
  req
    "UpdateFirewallPolicyChangeProtection"
    "fixture/UpdateFirewallPolicyChangeProtection.yaml"

requestCreateFirewall :: CreateFirewall -> TestTree
requestCreateFirewall =
  req
    "CreateFirewall"
    "fixture/CreateFirewall.yaml"

requestListRuleGroups :: ListRuleGroups -> TestTree
requestListRuleGroups =
  req
    "ListRuleGroups"
    "fixture/ListRuleGroups.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDeleteRuleGroup :: DeleteRuleGroup -> TestTree
requestDeleteRuleGroup =
  req
    "DeleteRuleGroup"
    "fixture/DeleteRuleGroup.yaml"

requestUpdateRuleGroup :: UpdateRuleGroup -> TestTree
requestUpdateRuleGroup =
  req
    "UpdateRuleGroup"
    "fixture/UpdateRuleGroup.yaml"

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy =
  req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

requestDescribeFirewall :: DescribeFirewall -> TestTree
requestDescribeFirewall =
  req
    "DescribeFirewall"
    "fixture/DescribeFirewall.yaml"

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy =
  req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDescribeLoggingConfiguration :: DescribeLoggingConfiguration -> TestTree
requestDescribeLoggingConfiguration =
  req
    "DescribeLoggingConfiguration"
    "fixture/DescribeLoggingConfiguration.yaml"

-- Responses

responseAssociateSubnets :: AssociateSubnetsResponse -> TestTree
responseAssociateSubnets =
  res
    "AssociateSubnetsResponse"
    "fixture/AssociateSubnetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateSubnets)

responseUpdateSubnetChangeProtection :: UpdateSubnetChangeProtectionResponse -> TestTree
responseUpdateSubnetChangeProtection =
  res
    "UpdateSubnetChangeProtectionResponse"
    "fixture/UpdateSubnetChangeProtectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSubnetChangeProtection)

responseUpdateFirewallPolicy :: UpdateFirewallPolicyResponse -> TestTree
responseUpdateFirewallPolicy =
  res
    "UpdateFirewallPolicyResponse"
    "fixture/UpdateFirewallPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFirewallPolicy)

responseDeleteFirewallPolicy :: DeleteFirewallPolicyResponse -> TestTree
responseDeleteFirewallPolicy =
  res
    "DeleteFirewallPolicyResponse"
    "fixture/DeleteFirewallPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFirewallPolicy)

responseCreateFirewallPolicy :: CreateFirewallPolicyResponse -> TestTree
responseCreateFirewallPolicy =
  res
    "CreateFirewallPolicyResponse"
    "fixture/CreateFirewallPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFirewallPolicy)

responseUpdateLoggingConfiguration :: UpdateLoggingConfigurationResponse -> TestTree
responseUpdateLoggingConfiguration =
  res
    "UpdateLoggingConfigurationResponse"
    "fixture/UpdateLoggingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLoggingConfiguration)

responseDisassociateSubnets :: DisassociateSubnetsResponse -> TestTree
responseDisassociateSubnets =
  res
    "DisassociateSubnetsResponse"
    "fixture/DisassociateSubnetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateSubnets)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListFirewallPolicies :: ListFirewallPoliciesResponse -> TestTree
responseListFirewallPolicies =
  res
    "ListFirewallPoliciesResponse"
    "fixture/ListFirewallPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFirewallPolicies)

responseUpdateFirewallDeleteProtection :: UpdateFirewallDeleteProtectionResponse -> TestTree
responseUpdateFirewallDeleteProtection =
  res
    "UpdateFirewallDeleteProtectionResponse"
    "fixture/UpdateFirewallDeleteProtectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFirewallDeleteProtection)

responseCreateRuleGroup :: CreateRuleGroupResponse -> TestTree
responseCreateRuleGroup =
  res
    "CreateRuleGroupResponse"
    "fixture/CreateRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRuleGroup)

responseDescribeFirewallPolicy :: DescribeFirewallPolicyResponse -> TestTree
responseDescribeFirewallPolicy =
  res
    "DescribeFirewallPolicyResponse"
    "fixture/DescribeFirewallPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFirewallPolicy)

responseUpdateFirewallDescription :: UpdateFirewallDescriptionResponse -> TestTree
responseUpdateFirewallDescription =
  res
    "UpdateFirewallDescriptionResponse"
    "fixture/UpdateFirewallDescriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFirewallDescription)

responseDescribeRuleGroup :: DescribeRuleGroupResponse -> TestTree
responseDescribeRuleGroup =
  res
    "DescribeRuleGroupResponse"
    "fixture/DescribeRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRuleGroup)

responseDeleteFirewall :: DeleteFirewallResponse -> TestTree
responseDeleteFirewall =
  res
    "DeleteFirewallResponse"
    "fixture/DeleteFirewallResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFirewall)

responseListFirewalls :: ListFirewallsResponse -> TestTree
responseListFirewalls =
  res
    "ListFirewallsResponse"
    "fixture/ListFirewallsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFirewalls)

responseDescribeResourcePolicy :: DescribeResourcePolicyResponse -> TestTree
responseDescribeResourcePolicy =
  res
    "DescribeResourcePolicyResponse"
    "fixture/DescribeResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeResourcePolicy)

responseAssociateFirewallPolicy :: AssociateFirewallPolicyResponse -> TestTree
responseAssociateFirewallPolicy =
  res
    "AssociateFirewallPolicyResponse"
    "fixture/AssociateFirewallPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateFirewallPolicy)

responseUpdateFirewallPolicyChangeProtection :: UpdateFirewallPolicyChangeProtectionResponse -> TestTree
responseUpdateFirewallPolicyChangeProtection =
  res
    "UpdateFirewallPolicyChangeProtectionResponse"
    "fixture/UpdateFirewallPolicyChangeProtectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFirewallPolicyChangeProtection)

responseCreateFirewall :: CreateFirewallResponse -> TestTree
responseCreateFirewall =
  res
    "CreateFirewallResponse"
    "fixture/CreateFirewallResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFirewall)

responseListRuleGroups :: ListRuleGroupsResponse -> TestTree
responseListRuleGroups =
  res
    "ListRuleGroupsResponse"
    "fixture/ListRuleGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRuleGroups)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseDeleteRuleGroup :: DeleteRuleGroupResponse -> TestTree
responseDeleteRuleGroup =
  res
    "DeleteRuleGroupResponse"
    "fixture/DeleteRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRuleGroup)

responseUpdateRuleGroup :: UpdateRuleGroupResponse -> TestTree
responseUpdateRuleGroup =
  res
    "UpdateRuleGroupResponse"
    "fixture/UpdateRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRuleGroup)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResourcePolicy)

responseDescribeFirewall :: DescribeFirewallResponse -> TestTree
responseDescribeFirewall =
  res
    "DescribeFirewallResponse"
    "fixture/DescribeFirewallResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFirewall)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourcePolicy)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseDescribeLoggingConfiguration :: DescribeLoggingConfigurationResponse -> TestTree
responseDescribeLoggingConfiguration =
  res
    "DescribeLoggingConfigurationResponse"
    "fixture/DescribeLoggingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLoggingConfiguration)
