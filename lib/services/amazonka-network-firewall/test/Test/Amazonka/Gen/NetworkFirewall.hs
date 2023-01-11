{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.NetworkFirewall
-- Copyright   : (c) 2013-2023 Brendan Hay
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
--         [ requestAssociateFirewallPolicy $
--             newAssociateFirewallPolicy
--
--         , requestAssociateSubnets $
--             newAssociateSubnets
--
--         , requestCreateFirewall $
--             newCreateFirewall
--
--         , requestCreateFirewallPolicy $
--             newCreateFirewallPolicy
--
--         , requestCreateRuleGroup $
--             newCreateRuleGroup
--
--         , requestDeleteFirewall $
--             newDeleteFirewall
--
--         , requestDeleteFirewallPolicy $
--             newDeleteFirewallPolicy
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestDeleteRuleGroup $
--             newDeleteRuleGroup
--
--         , requestDescribeFirewall $
--             newDescribeFirewall
--
--         , requestDescribeFirewallPolicy $
--             newDescribeFirewallPolicy
--
--         , requestDescribeLoggingConfiguration $
--             newDescribeLoggingConfiguration
--
--         , requestDescribeResourcePolicy $
--             newDescribeResourcePolicy
--
--         , requestDescribeRuleGroup $
--             newDescribeRuleGroup
--
--         , requestDescribeRuleGroupMetadata $
--             newDescribeRuleGroupMetadata
--
--         , requestDisassociateSubnets $
--             newDisassociateSubnets
--
--         , requestListFirewallPolicies $
--             newListFirewallPolicies
--
--         , requestListFirewalls $
--             newListFirewalls
--
--         , requestListRuleGroups $
--             newListRuleGroups
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutResourcePolicy $
--             newPutResourcePolicy
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateFirewallDeleteProtection $
--             newUpdateFirewallDeleteProtection
--
--         , requestUpdateFirewallDescription $
--             newUpdateFirewallDescription
--
--         , requestUpdateFirewallEncryptionConfiguration $
--             newUpdateFirewallEncryptionConfiguration
--
--         , requestUpdateFirewallPolicy $
--             newUpdateFirewallPolicy
--
--         , requestUpdateFirewallPolicyChangeProtection $
--             newUpdateFirewallPolicyChangeProtection
--
--         , requestUpdateLoggingConfiguration $
--             newUpdateLoggingConfiguration
--
--         , requestUpdateRuleGroup $
--             newUpdateRuleGroup
--
--         , requestUpdateSubnetChangeProtection $
--             newUpdateSubnetChangeProtection
--
--           ]

--     , testGroup "response"
--         [ responseAssociateFirewallPolicy $
--             newAssociateFirewallPolicyResponse
--
--         , responseAssociateSubnets $
--             newAssociateSubnetsResponse
--
--         , responseCreateFirewall $
--             newCreateFirewallResponse
--
--         , responseCreateFirewallPolicy $
--             newCreateFirewallPolicyResponse
--
--         , responseCreateRuleGroup $
--             newCreateRuleGroupResponse
--
--         , responseDeleteFirewall $
--             newDeleteFirewallResponse
--
--         , responseDeleteFirewallPolicy $
--             newDeleteFirewallPolicyResponse
--
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseDeleteRuleGroup $
--             newDeleteRuleGroupResponse
--
--         , responseDescribeFirewall $
--             newDescribeFirewallResponse
--
--         , responseDescribeFirewallPolicy $
--             newDescribeFirewallPolicyResponse
--
--         , responseDescribeLoggingConfiguration $
--             newDescribeLoggingConfigurationResponse
--
--         , responseDescribeResourcePolicy $
--             newDescribeResourcePolicyResponse
--
--         , responseDescribeRuleGroup $
--             newDescribeRuleGroupResponse
--
--         , responseDescribeRuleGroupMetadata $
--             newDescribeRuleGroupMetadataResponse
--
--         , responseDisassociateSubnets $
--             newDisassociateSubnetsResponse
--
--         , responseListFirewallPolicies $
--             newListFirewallPoliciesResponse
--
--         , responseListFirewalls $
--             newListFirewallsResponse
--
--         , responseListRuleGroups $
--             newListRuleGroupsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutResourcePolicy $
--             newPutResourcePolicyResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateFirewallDeleteProtection $
--             newUpdateFirewallDeleteProtectionResponse
--
--         , responseUpdateFirewallDescription $
--             newUpdateFirewallDescriptionResponse
--
--         , responseUpdateFirewallEncryptionConfiguration $
--             newUpdateFirewallEncryptionConfigurationResponse
--
--         , responseUpdateFirewallPolicy $
--             newUpdateFirewallPolicyResponse
--
--         , responseUpdateFirewallPolicyChangeProtection $
--             newUpdateFirewallPolicyChangeProtectionResponse
--
--         , responseUpdateLoggingConfiguration $
--             newUpdateLoggingConfigurationResponse
--
--         , responseUpdateRuleGroup $
--             newUpdateRuleGroupResponse
--
--         , responseUpdateSubnetChangeProtection $
--             newUpdateSubnetChangeProtectionResponse
--
--           ]
--     ]

-- Requests

requestAssociateFirewallPolicy :: AssociateFirewallPolicy -> TestTree
requestAssociateFirewallPolicy =
  req
    "AssociateFirewallPolicy"
    "fixture/AssociateFirewallPolicy.yaml"

requestAssociateSubnets :: AssociateSubnets -> TestTree
requestAssociateSubnets =
  req
    "AssociateSubnets"
    "fixture/AssociateSubnets.yaml"

requestCreateFirewall :: CreateFirewall -> TestTree
requestCreateFirewall =
  req
    "CreateFirewall"
    "fixture/CreateFirewall.yaml"

requestCreateFirewallPolicy :: CreateFirewallPolicy -> TestTree
requestCreateFirewallPolicy =
  req
    "CreateFirewallPolicy"
    "fixture/CreateFirewallPolicy.yaml"

requestCreateRuleGroup :: CreateRuleGroup -> TestTree
requestCreateRuleGroup =
  req
    "CreateRuleGroup"
    "fixture/CreateRuleGroup.yaml"

requestDeleteFirewall :: DeleteFirewall -> TestTree
requestDeleteFirewall =
  req
    "DeleteFirewall"
    "fixture/DeleteFirewall.yaml"

requestDeleteFirewallPolicy :: DeleteFirewallPolicy -> TestTree
requestDeleteFirewallPolicy =
  req
    "DeleteFirewallPolicy"
    "fixture/DeleteFirewallPolicy.yaml"

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy =
  req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

requestDeleteRuleGroup :: DeleteRuleGroup -> TestTree
requestDeleteRuleGroup =
  req
    "DeleteRuleGroup"
    "fixture/DeleteRuleGroup.yaml"

requestDescribeFirewall :: DescribeFirewall -> TestTree
requestDescribeFirewall =
  req
    "DescribeFirewall"
    "fixture/DescribeFirewall.yaml"

requestDescribeFirewallPolicy :: DescribeFirewallPolicy -> TestTree
requestDescribeFirewallPolicy =
  req
    "DescribeFirewallPolicy"
    "fixture/DescribeFirewallPolicy.yaml"

requestDescribeLoggingConfiguration :: DescribeLoggingConfiguration -> TestTree
requestDescribeLoggingConfiguration =
  req
    "DescribeLoggingConfiguration"
    "fixture/DescribeLoggingConfiguration.yaml"

requestDescribeResourcePolicy :: DescribeResourcePolicy -> TestTree
requestDescribeResourcePolicy =
  req
    "DescribeResourcePolicy"
    "fixture/DescribeResourcePolicy.yaml"

requestDescribeRuleGroup :: DescribeRuleGroup -> TestTree
requestDescribeRuleGroup =
  req
    "DescribeRuleGroup"
    "fixture/DescribeRuleGroup.yaml"

requestDescribeRuleGroupMetadata :: DescribeRuleGroupMetadata -> TestTree
requestDescribeRuleGroupMetadata =
  req
    "DescribeRuleGroupMetadata"
    "fixture/DescribeRuleGroupMetadata.yaml"

requestDisassociateSubnets :: DisassociateSubnets -> TestTree
requestDisassociateSubnets =
  req
    "DisassociateSubnets"
    "fixture/DisassociateSubnets.yaml"

requestListFirewallPolicies :: ListFirewallPolicies -> TestTree
requestListFirewallPolicies =
  req
    "ListFirewallPolicies"
    "fixture/ListFirewallPolicies.yaml"

requestListFirewalls :: ListFirewalls -> TestTree
requestListFirewalls =
  req
    "ListFirewalls"
    "fixture/ListFirewalls.yaml"

requestListRuleGroups :: ListRuleGroups -> TestTree
requestListRuleGroups =
  req
    "ListRuleGroups"
    "fixture/ListRuleGroups.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy =
  req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateFirewallDeleteProtection :: UpdateFirewallDeleteProtection -> TestTree
requestUpdateFirewallDeleteProtection =
  req
    "UpdateFirewallDeleteProtection"
    "fixture/UpdateFirewallDeleteProtection.yaml"

requestUpdateFirewallDescription :: UpdateFirewallDescription -> TestTree
requestUpdateFirewallDescription =
  req
    "UpdateFirewallDescription"
    "fixture/UpdateFirewallDescription.yaml"

requestUpdateFirewallEncryptionConfiguration :: UpdateFirewallEncryptionConfiguration -> TestTree
requestUpdateFirewallEncryptionConfiguration =
  req
    "UpdateFirewallEncryptionConfiguration"
    "fixture/UpdateFirewallEncryptionConfiguration.yaml"

requestUpdateFirewallPolicy :: UpdateFirewallPolicy -> TestTree
requestUpdateFirewallPolicy =
  req
    "UpdateFirewallPolicy"
    "fixture/UpdateFirewallPolicy.yaml"

requestUpdateFirewallPolicyChangeProtection :: UpdateFirewallPolicyChangeProtection -> TestTree
requestUpdateFirewallPolicyChangeProtection =
  req
    "UpdateFirewallPolicyChangeProtection"
    "fixture/UpdateFirewallPolicyChangeProtection.yaml"

requestUpdateLoggingConfiguration :: UpdateLoggingConfiguration -> TestTree
requestUpdateLoggingConfiguration =
  req
    "UpdateLoggingConfiguration"
    "fixture/UpdateLoggingConfiguration.yaml"

requestUpdateRuleGroup :: UpdateRuleGroup -> TestTree
requestUpdateRuleGroup =
  req
    "UpdateRuleGroup"
    "fixture/UpdateRuleGroup.yaml"

requestUpdateSubnetChangeProtection :: UpdateSubnetChangeProtection -> TestTree
requestUpdateSubnetChangeProtection =
  req
    "UpdateSubnetChangeProtection"
    "fixture/UpdateSubnetChangeProtection.yaml"

-- Responses

responseAssociateFirewallPolicy :: AssociateFirewallPolicyResponse -> TestTree
responseAssociateFirewallPolicy =
  res
    "AssociateFirewallPolicyResponse"
    "fixture/AssociateFirewallPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateFirewallPolicy)

responseAssociateSubnets :: AssociateSubnetsResponse -> TestTree
responseAssociateSubnets =
  res
    "AssociateSubnetsResponse"
    "fixture/AssociateSubnetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateSubnets)

responseCreateFirewall :: CreateFirewallResponse -> TestTree
responseCreateFirewall =
  res
    "CreateFirewallResponse"
    "fixture/CreateFirewallResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFirewall)

responseCreateFirewallPolicy :: CreateFirewallPolicyResponse -> TestTree
responseCreateFirewallPolicy =
  res
    "CreateFirewallPolicyResponse"
    "fixture/CreateFirewallPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFirewallPolicy)

responseCreateRuleGroup :: CreateRuleGroupResponse -> TestTree
responseCreateRuleGroup =
  res
    "CreateRuleGroupResponse"
    "fixture/CreateRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRuleGroup)

responseDeleteFirewall :: DeleteFirewallResponse -> TestTree
responseDeleteFirewall =
  res
    "DeleteFirewallResponse"
    "fixture/DeleteFirewallResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFirewall)

responseDeleteFirewallPolicy :: DeleteFirewallPolicyResponse -> TestTree
responseDeleteFirewallPolicy =
  res
    "DeleteFirewallPolicyResponse"
    "fixture/DeleteFirewallPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFirewallPolicy)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourcePolicy)

responseDeleteRuleGroup :: DeleteRuleGroupResponse -> TestTree
responseDeleteRuleGroup =
  res
    "DeleteRuleGroupResponse"
    "fixture/DeleteRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRuleGroup)

responseDescribeFirewall :: DescribeFirewallResponse -> TestTree
responseDescribeFirewall =
  res
    "DescribeFirewallResponse"
    "fixture/DescribeFirewallResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFirewall)

responseDescribeFirewallPolicy :: DescribeFirewallPolicyResponse -> TestTree
responseDescribeFirewallPolicy =
  res
    "DescribeFirewallPolicyResponse"
    "fixture/DescribeFirewallPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFirewallPolicy)

responseDescribeLoggingConfiguration :: DescribeLoggingConfigurationResponse -> TestTree
responseDescribeLoggingConfiguration =
  res
    "DescribeLoggingConfigurationResponse"
    "fixture/DescribeLoggingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLoggingConfiguration)

responseDescribeResourcePolicy :: DescribeResourcePolicyResponse -> TestTree
responseDescribeResourcePolicy =
  res
    "DescribeResourcePolicyResponse"
    "fixture/DescribeResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeResourcePolicy)

responseDescribeRuleGroup :: DescribeRuleGroupResponse -> TestTree
responseDescribeRuleGroup =
  res
    "DescribeRuleGroupResponse"
    "fixture/DescribeRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRuleGroup)

responseDescribeRuleGroupMetadata :: DescribeRuleGroupMetadataResponse -> TestTree
responseDescribeRuleGroupMetadata =
  res
    "DescribeRuleGroupMetadataResponse"
    "fixture/DescribeRuleGroupMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRuleGroupMetadata)

responseDisassociateSubnets :: DisassociateSubnetsResponse -> TestTree
responseDisassociateSubnets =
  res
    "DisassociateSubnetsResponse"
    "fixture/DisassociateSubnetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateSubnets)

responseListFirewallPolicies :: ListFirewallPoliciesResponse -> TestTree
responseListFirewallPolicies =
  res
    "ListFirewallPoliciesResponse"
    "fixture/ListFirewallPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFirewallPolicies)

responseListFirewalls :: ListFirewallsResponse -> TestTree
responseListFirewalls =
  res
    "ListFirewallsResponse"
    "fixture/ListFirewallsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFirewalls)

responseListRuleGroups :: ListRuleGroupsResponse -> TestTree
responseListRuleGroups =
  res
    "ListRuleGroupsResponse"
    "fixture/ListRuleGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRuleGroups)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResourcePolicy)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateFirewallDeleteProtection :: UpdateFirewallDeleteProtectionResponse -> TestTree
responseUpdateFirewallDeleteProtection =
  res
    "UpdateFirewallDeleteProtectionResponse"
    "fixture/UpdateFirewallDeleteProtectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFirewallDeleteProtection)

responseUpdateFirewallDescription :: UpdateFirewallDescriptionResponse -> TestTree
responseUpdateFirewallDescription =
  res
    "UpdateFirewallDescriptionResponse"
    "fixture/UpdateFirewallDescriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFirewallDescription)

responseUpdateFirewallEncryptionConfiguration :: UpdateFirewallEncryptionConfigurationResponse -> TestTree
responseUpdateFirewallEncryptionConfiguration =
  res
    "UpdateFirewallEncryptionConfigurationResponse"
    "fixture/UpdateFirewallEncryptionConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFirewallEncryptionConfiguration)

responseUpdateFirewallPolicy :: UpdateFirewallPolicyResponse -> TestTree
responseUpdateFirewallPolicy =
  res
    "UpdateFirewallPolicyResponse"
    "fixture/UpdateFirewallPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFirewallPolicy)

responseUpdateFirewallPolicyChangeProtection :: UpdateFirewallPolicyChangeProtectionResponse -> TestTree
responseUpdateFirewallPolicyChangeProtection =
  res
    "UpdateFirewallPolicyChangeProtectionResponse"
    "fixture/UpdateFirewallPolicyChangeProtectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFirewallPolicyChangeProtection)

responseUpdateLoggingConfiguration :: UpdateLoggingConfigurationResponse -> TestTree
responseUpdateLoggingConfiguration =
  res
    "UpdateLoggingConfigurationResponse"
    "fixture/UpdateLoggingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLoggingConfiguration)

responseUpdateRuleGroup :: UpdateRuleGroupResponse -> TestTree
responseUpdateRuleGroup =
  res
    "UpdateRuleGroupResponse"
    "fixture/UpdateRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRuleGroup)

responseUpdateSubnetChangeProtection :: UpdateSubnetChangeProtectionResponse -> TestTree
responseUpdateSubnetChangeProtection =
  res
    "UpdateSubnetChangeProtectionResponse"
    "fixture/UpdateSubnetChangeProtectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSubnetChangeProtection)
