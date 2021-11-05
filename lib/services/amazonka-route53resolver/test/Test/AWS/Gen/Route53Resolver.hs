{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Route53Resolver
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Route53Resolver where

import Amazonka.Route53Resolver
import qualified Data.Proxy as Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.Route53Resolver.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestUpdateResolverEndpoint $
--             newUpdateResolverEndpoint
--
--         , requestDeleteResolverEndpoint $
--             newDeleteResolverEndpoint
--
--         , requestCreateResolverRule $
--             newCreateResolverRule
--
--         , requestGetResolverQueryLogConfig $
--             newGetResolverQueryLogConfig
--
--         , requestCreateFirewallRule $
--             newCreateFirewallRule
--
--         , requestUpdateFirewallRuleGroupAssociation $
--             newUpdateFirewallRuleGroupAssociation
--
--         , requestListFirewallRuleGroupAssociations $
--             newListFirewallRuleGroupAssociations
--
--         , requestListResolverQueryLogConfigAssociations $
--             newListResolverQueryLogConfigAssociations
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetFirewallRuleGroupAssociation $
--             newGetFirewallRuleGroupAssociation
--
--         , requestDisassociateResolverEndpointIpAddress $
--             newDisassociateResolverEndpointIpAddress
--
--         , requestListResolverRuleAssociations $
--             newListResolverRuleAssociations
--
--         , requestDeleteResolverQueryLogConfig $
--             newDeleteResolverQueryLogConfig
--
--         , requestCreateFirewallRuleGroup $
--             newCreateFirewallRuleGroup
--
--         , requestGetResolverEndpoint $
--             newGetResolverEndpoint
--
--         , requestListResolverQueryLogConfigs $
--             newListResolverQueryLogConfigs
--
--         , requestDeleteFirewallRuleGroup $
--             newDeleteFirewallRuleGroup
--
--         , requestListResolverEndpointIpAddresses $
--             newListResolverEndpointIpAddresses
--
--         , requestAssociateResolverQueryLogConfig $
--             newAssociateResolverQueryLogConfig
--
--         , requestGetResolverRulePolicy $
--             newGetResolverRulePolicy
--
--         , requestGetResolverDnssecConfig $
--             newGetResolverDnssecConfig
--
--         , requestListFirewallRuleGroups $
--             newListFirewallRuleGroups
--
--         , requestUpdateResolverRule $
--             newUpdateResolverRule
--
--         , requestDeleteResolverRule $
--             newDeleteResolverRule
--
--         , requestDeleteFirewallRule $
--             newDeleteFirewallRule
--
--         , requestUpdateFirewallRule $
--             newUpdateFirewallRule
--
--         , requestListFirewallRules $
--             newListFirewallRules
--
--         , requestGetFirewallRuleGroup $
--             newGetFirewallRuleGroup
--
--         , requestListResolverRules $
--             newListResolverRules
--
--         , requestCreateResolverEndpoint $
--             newCreateResolverEndpoint
--
--         , requestAssociateResolverRule $
--             newAssociateResolverRule
--
--         , requestGetResolverQueryLogConfigPolicy $
--             newGetResolverQueryLogConfigPolicy
--
--         , requestUpdateFirewallDomains $
--             newUpdateFirewallDomains
--
--         , requestListResolverEndpoints $
--             newListResolverEndpoints
--
--         , requestListFirewallDomains $
--             newListFirewallDomains
--
--         , requestGetResolverRuleAssociation $
--             newGetResolverRuleAssociation
--
--         , requestGetFirewallConfig $
--             newGetFirewallConfig
--
--         , requestGetFirewallDomainList $
--             newGetFirewallDomainList
--
--         , requestDisassociateResolverRule $
--             newDisassociateResolverRule
--
--         , requestGetResolverQueryLogConfigAssociation $
--             newGetResolverQueryLogConfigAssociation
--
--         , requestListFirewallDomainLists $
--             newListFirewallDomainLists
--
--         , requestDisassociateFirewallRuleGroup $
--             newDisassociateFirewallRuleGroup
--
--         , requestUpdateFirewallConfig $
--             newUpdateFirewallConfig
--
--         , requestDeleteFirewallDomainList $
--             newDeleteFirewallDomainList
--
--         , requestListFirewallConfigs $
--             newListFirewallConfigs
--
--         , requestCreateFirewallDomainList $
--             newCreateFirewallDomainList
--
--         , requestImportFirewallDomains $
--             newImportFirewallDomains
--
--         , requestDisassociateResolverQueryLogConfig $
--             newDisassociateResolverQueryLogConfig
--
--         , requestTagResource $
--             newTagResource
--
--         , requestAssociateFirewallRuleGroup $
--             newAssociateFirewallRuleGroup
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestPutResolverQueryLogConfigPolicy $
--             newPutResolverQueryLogConfigPolicy
--
--         , requestAssociateResolverEndpointIpAddress $
--             newAssociateResolverEndpointIpAddress
--
--         , requestCreateResolverQueryLogConfig $
--             newCreateResolverQueryLogConfig
--
--         , requestGetResolverRule $
--             newGetResolverRule
--
--         , requestPutFirewallRuleGroupPolicy $
--             newPutFirewallRuleGroupPolicy
--
--         , requestPutResolverRulePolicy $
--             newPutResolverRulePolicy
--
--         , requestListResolverDnssecConfigs $
--             newListResolverDnssecConfigs
--
--         , requestUpdateResolverDnssecConfig $
--             newUpdateResolverDnssecConfig
--
--         , requestGetFirewallRuleGroupPolicy $
--             newGetFirewallRuleGroupPolicy
--
--           ]

--     , testGroup "response"
--         [ responseUpdateResolverEndpoint $
--             newUpdateResolverEndpointResponse
--
--         , responseDeleteResolverEndpoint $
--             newDeleteResolverEndpointResponse
--
--         , responseCreateResolverRule $
--             newCreateResolverRuleResponse
--
--         , responseGetResolverQueryLogConfig $
--             newGetResolverQueryLogConfigResponse
--
--         , responseCreateFirewallRule $
--             newCreateFirewallRuleResponse
--
--         , responseUpdateFirewallRuleGroupAssociation $
--             newUpdateFirewallRuleGroupAssociationResponse
--
--         , responseListFirewallRuleGroupAssociations $
--             newListFirewallRuleGroupAssociationsResponse
--
--         , responseListResolverQueryLogConfigAssociations $
--             newListResolverQueryLogConfigAssociationsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetFirewallRuleGroupAssociation $
--             newGetFirewallRuleGroupAssociationResponse
--
--         , responseDisassociateResolverEndpointIpAddress $
--             newDisassociateResolverEndpointIpAddressResponse
--
--         , responseListResolverRuleAssociations $
--             newListResolverRuleAssociationsResponse
--
--         , responseDeleteResolverQueryLogConfig $
--             newDeleteResolverQueryLogConfigResponse
--
--         , responseCreateFirewallRuleGroup $
--             newCreateFirewallRuleGroupResponse
--
--         , responseGetResolverEndpoint $
--             newGetResolverEndpointResponse
--
--         , responseListResolverQueryLogConfigs $
--             newListResolverQueryLogConfigsResponse
--
--         , responseDeleteFirewallRuleGroup $
--             newDeleteFirewallRuleGroupResponse
--
--         , responseListResolverEndpointIpAddresses $
--             newListResolverEndpointIpAddressesResponse
--
--         , responseAssociateResolverQueryLogConfig $
--             newAssociateResolverQueryLogConfigResponse
--
--         , responseGetResolverRulePolicy $
--             newGetResolverRulePolicyResponse
--
--         , responseGetResolverDnssecConfig $
--             newGetResolverDnssecConfigResponse
--
--         , responseListFirewallRuleGroups $
--             newListFirewallRuleGroupsResponse
--
--         , responseUpdateResolverRule $
--             newUpdateResolverRuleResponse
--
--         , responseDeleteResolverRule $
--             newDeleteResolverRuleResponse
--
--         , responseDeleteFirewallRule $
--             newDeleteFirewallRuleResponse
--
--         , responseUpdateFirewallRule $
--             newUpdateFirewallRuleResponse
--
--         , responseListFirewallRules $
--             newListFirewallRulesResponse
--
--         , responseGetFirewallRuleGroup $
--             newGetFirewallRuleGroupResponse
--
--         , responseListResolverRules $
--             newListResolverRulesResponse
--
--         , responseCreateResolverEndpoint $
--             newCreateResolverEndpointResponse
--
--         , responseAssociateResolverRule $
--             newAssociateResolverRuleResponse
--
--         , responseGetResolverQueryLogConfigPolicy $
--             newGetResolverQueryLogConfigPolicyResponse
--
--         , responseUpdateFirewallDomains $
--             newUpdateFirewallDomainsResponse
--
--         , responseListResolverEndpoints $
--             newListResolverEndpointsResponse
--
--         , responseListFirewallDomains $
--             newListFirewallDomainsResponse
--
--         , responseGetResolverRuleAssociation $
--             newGetResolverRuleAssociationResponse
--
--         , responseGetFirewallConfig $
--             newGetFirewallConfigResponse
--
--         , responseGetFirewallDomainList $
--             newGetFirewallDomainListResponse
--
--         , responseDisassociateResolverRule $
--             newDisassociateResolverRuleResponse
--
--         , responseGetResolverQueryLogConfigAssociation $
--             newGetResolverQueryLogConfigAssociationResponse
--
--         , responseListFirewallDomainLists $
--             newListFirewallDomainListsResponse
--
--         , responseDisassociateFirewallRuleGroup $
--             newDisassociateFirewallRuleGroupResponse
--
--         , responseUpdateFirewallConfig $
--             newUpdateFirewallConfigResponse
--
--         , responseDeleteFirewallDomainList $
--             newDeleteFirewallDomainListResponse
--
--         , responseListFirewallConfigs $
--             newListFirewallConfigsResponse
--
--         , responseCreateFirewallDomainList $
--             newCreateFirewallDomainListResponse
--
--         , responseImportFirewallDomains $
--             newImportFirewallDomainsResponse
--
--         , responseDisassociateResolverQueryLogConfig $
--             newDisassociateResolverQueryLogConfigResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseAssociateFirewallRuleGroup $
--             newAssociateFirewallRuleGroupResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responsePutResolverQueryLogConfigPolicy $
--             newPutResolverQueryLogConfigPolicyResponse
--
--         , responseAssociateResolverEndpointIpAddress $
--             newAssociateResolverEndpointIpAddressResponse
--
--         , responseCreateResolverQueryLogConfig $
--             newCreateResolverQueryLogConfigResponse
--
--         , responseGetResolverRule $
--             newGetResolverRuleResponse
--
--         , responsePutFirewallRuleGroupPolicy $
--             newPutFirewallRuleGroupPolicyResponse
--
--         , responsePutResolverRulePolicy $
--             newPutResolverRulePolicyResponse
--
--         , responseListResolverDnssecConfigs $
--             newListResolverDnssecConfigsResponse
--
--         , responseUpdateResolverDnssecConfig $
--             newUpdateResolverDnssecConfigResponse
--
--         , responseGetFirewallRuleGroupPolicy $
--             newGetFirewallRuleGroupPolicyResponse
--
--           ]
--     ]

-- Requests

requestUpdateResolverEndpoint :: UpdateResolverEndpoint -> TestTree
requestUpdateResolverEndpoint =
  req
    "UpdateResolverEndpoint"
    "fixture/UpdateResolverEndpoint.yaml"

requestDeleteResolverEndpoint :: DeleteResolverEndpoint -> TestTree
requestDeleteResolverEndpoint =
  req
    "DeleteResolverEndpoint"
    "fixture/DeleteResolverEndpoint.yaml"

requestCreateResolverRule :: CreateResolverRule -> TestTree
requestCreateResolverRule =
  req
    "CreateResolverRule"
    "fixture/CreateResolverRule.yaml"

requestGetResolverQueryLogConfig :: GetResolverQueryLogConfig -> TestTree
requestGetResolverQueryLogConfig =
  req
    "GetResolverQueryLogConfig"
    "fixture/GetResolverQueryLogConfig.yaml"

requestCreateFirewallRule :: CreateFirewallRule -> TestTree
requestCreateFirewallRule =
  req
    "CreateFirewallRule"
    "fixture/CreateFirewallRule.yaml"

requestUpdateFirewallRuleGroupAssociation :: UpdateFirewallRuleGroupAssociation -> TestTree
requestUpdateFirewallRuleGroupAssociation =
  req
    "UpdateFirewallRuleGroupAssociation"
    "fixture/UpdateFirewallRuleGroupAssociation.yaml"

requestListFirewallRuleGroupAssociations :: ListFirewallRuleGroupAssociations -> TestTree
requestListFirewallRuleGroupAssociations =
  req
    "ListFirewallRuleGroupAssociations"
    "fixture/ListFirewallRuleGroupAssociations.yaml"

requestListResolverQueryLogConfigAssociations :: ListResolverQueryLogConfigAssociations -> TestTree
requestListResolverQueryLogConfigAssociations =
  req
    "ListResolverQueryLogConfigAssociations"
    "fixture/ListResolverQueryLogConfigAssociations.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetFirewallRuleGroupAssociation :: GetFirewallRuleGroupAssociation -> TestTree
requestGetFirewallRuleGroupAssociation =
  req
    "GetFirewallRuleGroupAssociation"
    "fixture/GetFirewallRuleGroupAssociation.yaml"

requestDisassociateResolverEndpointIpAddress :: DisassociateResolverEndpointIpAddress -> TestTree
requestDisassociateResolverEndpointIpAddress =
  req
    "DisassociateResolverEndpointIpAddress"
    "fixture/DisassociateResolverEndpointIpAddress.yaml"

requestListResolverRuleAssociations :: ListResolverRuleAssociations -> TestTree
requestListResolverRuleAssociations =
  req
    "ListResolverRuleAssociations"
    "fixture/ListResolverRuleAssociations.yaml"

requestDeleteResolverQueryLogConfig :: DeleteResolverQueryLogConfig -> TestTree
requestDeleteResolverQueryLogConfig =
  req
    "DeleteResolverQueryLogConfig"
    "fixture/DeleteResolverQueryLogConfig.yaml"

requestCreateFirewallRuleGroup :: CreateFirewallRuleGroup -> TestTree
requestCreateFirewallRuleGroup =
  req
    "CreateFirewallRuleGroup"
    "fixture/CreateFirewallRuleGroup.yaml"

requestGetResolverEndpoint :: GetResolverEndpoint -> TestTree
requestGetResolverEndpoint =
  req
    "GetResolverEndpoint"
    "fixture/GetResolverEndpoint.yaml"

requestListResolverQueryLogConfigs :: ListResolverQueryLogConfigs -> TestTree
requestListResolverQueryLogConfigs =
  req
    "ListResolverQueryLogConfigs"
    "fixture/ListResolverQueryLogConfigs.yaml"

requestDeleteFirewallRuleGroup :: DeleteFirewallRuleGroup -> TestTree
requestDeleteFirewallRuleGroup =
  req
    "DeleteFirewallRuleGroup"
    "fixture/DeleteFirewallRuleGroup.yaml"

requestListResolverEndpointIpAddresses :: ListResolverEndpointIpAddresses -> TestTree
requestListResolverEndpointIpAddresses =
  req
    "ListResolverEndpointIpAddresses"
    "fixture/ListResolverEndpointIpAddresses.yaml"

requestAssociateResolverQueryLogConfig :: AssociateResolverQueryLogConfig -> TestTree
requestAssociateResolverQueryLogConfig =
  req
    "AssociateResolverQueryLogConfig"
    "fixture/AssociateResolverQueryLogConfig.yaml"

requestGetResolverRulePolicy :: GetResolverRulePolicy -> TestTree
requestGetResolverRulePolicy =
  req
    "GetResolverRulePolicy"
    "fixture/GetResolverRulePolicy.yaml"

requestGetResolverDnssecConfig :: GetResolverDnssecConfig -> TestTree
requestGetResolverDnssecConfig =
  req
    "GetResolverDnssecConfig"
    "fixture/GetResolverDnssecConfig.yaml"

requestListFirewallRuleGroups :: ListFirewallRuleGroups -> TestTree
requestListFirewallRuleGroups =
  req
    "ListFirewallRuleGroups"
    "fixture/ListFirewallRuleGroups.yaml"

requestUpdateResolverRule :: UpdateResolverRule -> TestTree
requestUpdateResolverRule =
  req
    "UpdateResolverRule"
    "fixture/UpdateResolverRule.yaml"

requestDeleteResolverRule :: DeleteResolverRule -> TestTree
requestDeleteResolverRule =
  req
    "DeleteResolverRule"
    "fixture/DeleteResolverRule.yaml"

requestDeleteFirewallRule :: DeleteFirewallRule -> TestTree
requestDeleteFirewallRule =
  req
    "DeleteFirewallRule"
    "fixture/DeleteFirewallRule.yaml"

requestUpdateFirewallRule :: UpdateFirewallRule -> TestTree
requestUpdateFirewallRule =
  req
    "UpdateFirewallRule"
    "fixture/UpdateFirewallRule.yaml"

requestListFirewallRules :: ListFirewallRules -> TestTree
requestListFirewallRules =
  req
    "ListFirewallRules"
    "fixture/ListFirewallRules.yaml"

requestGetFirewallRuleGroup :: GetFirewallRuleGroup -> TestTree
requestGetFirewallRuleGroup =
  req
    "GetFirewallRuleGroup"
    "fixture/GetFirewallRuleGroup.yaml"

requestListResolverRules :: ListResolverRules -> TestTree
requestListResolverRules =
  req
    "ListResolverRules"
    "fixture/ListResolverRules.yaml"

requestCreateResolverEndpoint :: CreateResolverEndpoint -> TestTree
requestCreateResolverEndpoint =
  req
    "CreateResolverEndpoint"
    "fixture/CreateResolverEndpoint.yaml"

requestAssociateResolverRule :: AssociateResolverRule -> TestTree
requestAssociateResolverRule =
  req
    "AssociateResolverRule"
    "fixture/AssociateResolverRule.yaml"

requestGetResolverQueryLogConfigPolicy :: GetResolverQueryLogConfigPolicy -> TestTree
requestGetResolverQueryLogConfigPolicy =
  req
    "GetResolverQueryLogConfigPolicy"
    "fixture/GetResolverQueryLogConfigPolicy.yaml"

requestUpdateFirewallDomains :: UpdateFirewallDomains -> TestTree
requestUpdateFirewallDomains =
  req
    "UpdateFirewallDomains"
    "fixture/UpdateFirewallDomains.yaml"

requestListResolverEndpoints :: ListResolverEndpoints -> TestTree
requestListResolverEndpoints =
  req
    "ListResolverEndpoints"
    "fixture/ListResolverEndpoints.yaml"

requestListFirewallDomains :: ListFirewallDomains -> TestTree
requestListFirewallDomains =
  req
    "ListFirewallDomains"
    "fixture/ListFirewallDomains.yaml"

requestGetResolverRuleAssociation :: GetResolverRuleAssociation -> TestTree
requestGetResolverRuleAssociation =
  req
    "GetResolverRuleAssociation"
    "fixture/GetResolverRuleAssociation.yaml"

requestGetFirewallConfig :: GetFirewallConfig -> TestTree
requestGetFirewallConfig =
  req
    "GetFirewallConfig"
    "fixture/GetFirewallConfig.yaml"

requestGetFirewallDomainList :: GetFirewallDomainList -> TestTree
requestGetFirewallDomainList =
  req
    "GetFirewallDomainList"
    "fixture/GetFirewallDomainList.yaml"

requestDisassociateResolverRule :: DisassociateResolverRule -> TestTree
requestDisassociateResolverRule =
  req
    "DisassociateResolverRule"
    "fixture/DisassociateResolverRule.yaml"

requestGetResolverQueryLogConfigAssociation :: GetResolverQueryLogConfigAssociation -> TestTree
requestGetResolverQueryLogConfigAssociation =
  req
    "GetResolverQueryLogConfigAssociation"
    "fixture/GetResolverQueryLogConfigAssociation.yaml"

requestListFirewallDomainLists :: ListFirewallDomainLists -> TestTree
requestListFirewallDomainLists =
  req
    "ListFirewallDomainLists"
    "fixture/ListFirewallDomainLists.yaml"

requestDisassociateFirewallRuleGroup :: DisassociateFirewallRuleGroup -> TestTree
requestDisassociateFirewallRuleGroup =
  req
    "DisassociateFirewallRuleGroup"
    "fixture/DisassociateFirewallRuleGroup.yaml"

requestUpdateFirewallConfig :: UpdateFirewallConfig -> TestTree
requestUpdateFirewallConfig =
  req
    "UpdateFirewallConfig"
    "fixture/UpdateFirewallConfig.yaml"

requestDeleteFirewallDomainList :: DeleteFirewallDomainList -> TestTree
requestDeleteFirewallDomainList =
  req
    "DeleteFirewallDomainList"
    "fixture/DeleteFirewallDomainList.yaml"

requestListFirewallConfigs :: ListFirewallConfigs -> TestTree
requestListFirewallConfigs =
  req
    "ListFirewallConfigs"
    "fixture/ListFirewallConfigs.yaml"

requestCreateFirewallDomainList :: CreateFirewallDomainList -> TestTree
requestCreateFirewallDomainList =
  req
    "CreateFirewallDomainList"
    "fixture/CreateFirewallDomainList.yaml"

requestImportFirewallDomains :: ImportFirewallDomains -> TestTree
requestImportFirewallDomains =
  req
    "ImportFirewallDomains"
    "fixture/ImportFirewallDomains.yaml"

requestDisassociateResolverQueryLogConfig :: DisassociateResolverQueryLogConfig -> TestTree
requestDisassociateResolverQueryLogConfig =
  req
    "DisassociateResolverQueryLogConfig"
    "fixture/DisassociateResolverQueryLogConfig.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestAssociateFirewallRuleGroup :: AssociateFirewallRuleGroup -> TestTree
requestAssociateFirewallRuleGroup =
  req
    "AssociateFirewallRuleGroup"
    "fixture/AssociateFirewallRuleGroup.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestPutResolverQueryLogConfigPolicy :: PutResolverQueryLogConfigPolicy -> TestTree
requestPutResolverQueryLogConfigPolicy =
  req
    "PutResolverQueryLogConfigPolicy"
    "fixture/PutResolverQueryLogConfigPolicy.yaml"

requestAssociateResolverEndpointIpAddress :: AssociateResolverEndpointIpAddress -> TestTree
requestAssociateResolverEndpointIpAddress =
  req
    "AssociateResolverEndpointIpAddress"
    "fixture/AssociateResolverEndpointIpAddress.yaml"

requestCreateResolverQueryLogConfig :: CreateResolverQueryLogConfig -> TestTree
requestCreateResolverQueryLogConfig =
  req
    "CreateResolverQueryLogConfig"
    "fixture/CreateResolverQueryLogConfig.yaml"

requestGetResolverRule :: GetResolverRule -> TestTree
requestGetResolverRule =
  req
    "GetResolverRule"
    "fixture/GetResolverRule.yaml"

requestPutFirewallRuleGroupPolicy :: PutFirewallRuleGroupPolicy -> TestTree
requestPutFirewallRuleGroupPolicy =
  req
    "PutFirewallRuleGroupPolicy"
    "fixture/PutFirewallRuleGroupPolicy.yaml"

requestPutResolverRulePolicy :: PutResolverRulePolicy -> TestTree
requestPutResolverRulePolicy =
  req
    "PutResolverRulePolicy"
    "fixture/PutResolverRulePolicy.yaml"

requestListResolverDnssecConfigs :: ListResolverDnssecConfigs -> TestTree
requestListResolverDnssecConfigs =
  req
    "ListResolverDnssecConfigs"
    "fixture/ListResolverDnssecConfigs.yaml"

requestUpdateResolverDnssecConfig :: UpdateResolverDnssecConfig -> TestTree
requestUpdateResolverDnssecConfig =
  req
    "UpdateResolverDnssecConfig"
    "fixture/UpdateResolverDnssecConfig.yaml"

requestGetFirewallRuleGroupPolicy :: GetFirewallRuleGroupPolicy -> TestTree
requestGetFirewallRuleGroupPolicy =
  req
    "GetFirewallRuleGroupPolicy"
    "fixture/GetFirewallRuleGroupPolicy.yaml"

-- Responses

responseUpdateResolverEndpoint :: UpdateResolverEndpointResponse -> TestTree
responseUpdateResolverEndpoint =
  res
    "UpdateResolverEndpointResponse"
    "fixture/UpdateResolverEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResolverEndpoint)

responseDeleteResolverEndpoint :: DeleteResolverEndpointResponse -> TestTree
responseDeleteResolverEndpoint =
  res
    "DeleteResolverEndpointResponse"
    "fixture/DeleteResolverEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResolverEndpoint)

responseCreateResolverRule :: CreateResolverRuleResponse -> TestTree
responseCreateResolverRule =
  res
    "CreateResolverRuleResponse"
    "fixture/CreateResolverRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResolverRule)

responseGetResolverQueryLogConfig :: GetResolverQueryLogConfigResponse -> TestTree
responseGetResolverQueryLogConfig =
  res
    "GetResolverQueryLogConfigResponse"
    "fixture/GetResolverQueryLogConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResolverQueryLogConfig)

responseCreateFirewallRule :: CreateFirewallRuleResponse -> TestTree
responseCreateFirewallRule =
  res
    "CreateFirewallRuleResponse"
    "fixture/CreateFirewallRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFirewallRule)

responseUpdateFirewallRuleGroupAssociation :: UpdateFirewallRuleGroupAssociationResponse -> TestTree
responseUpdateFirewallRuleGroupAssociation =
  res
    "UpdateFirewallRuleGroupAssociationResponse"
    "fixture/UpdateFirewallRuleGroupAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFirewallRuleGroupAssociation)

responseListFirewallRuleGroupAssociations :: ListFirewallRuleGroupAssociationsResponse -> TestTree
responseListFirewallRuleGroupAssociations =
  res
    "ListFirewallRuleGroupAssociationsResponse"
    "fixture/ListFirewallRuleGroupAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFirewallRuleGroupAssociations)

responseListResolverQueryLogConfigAssociations :: ListResolverQueryLogConfigAssociationsResponse -> TestTree
responseListResolverQueryLogConfigAssociations =
  res
    "ListResolverQueryLogConfigAssociationsResponse"
    "fixture/ListResolverQueryLogConfigAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResolverQueryLogConfigAssociations)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseGetFirewallRuleGroupAssociation :: GetFirewallRuleGroupAssociationResponse -> TestTree
responseGetFirewallRuleGroupAssociation =
  res
    "GetFirewallRuleGroupAssociationResponse"
    "fixture/GetFirewallRuleGroupAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFirewallRuleGroupAssociation)

responseDisassociateResolverEndpointIpAddress :: DisassociateResolverEndpointIpAddressResponse -> TestTree
responseDisassociateResolverEndpointIpAddress =
  res
    "DisassociateResolverEndpointIpAddressResponse"
    "fixture/DisassociateResolverEndpointIpAddressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateResolverEndpointIpAddress)

responseListResolverRuleAssociations :: ListResolverRuleAssociationsResponse -> TestTree
responseListResolverRuleAssociations =
  res
    "ListResolverRuleAssociationsResponse"
    "fixture/ListResolverRuleAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResolverRuleAssociations)

responseDeleteResolverQueryLogConfig :: DeleteResolverQueryLogConfigResponse -> TestTree
responseDeleteResolverQueryLogConfig =
  res
    "DeleteResolverQueryLogConfigResponse"
    "fixture/DeleteResolverQueryLogConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResolverQueryLogConfig)

responseCreateFirewallRuleGroup :: CreateFirewallRuleGroupResponse -> TestTree
responseCreateFirewallRuleGroup =
  res
    "CreateFirewallRuleGroupResponse"
    "fixture/CreateFirewallRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFirewallRuleGroup)

responseGetResolverEndpoint :: GetResolverEndpointResponse -> TestTree
responseGetResolverEndpoint =
  res
    "GetResolverEndpointResponse"
    "fixture/GetResolverEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResolverEndpoint)

responseListResolverQueryLogConfigs :: ListResolverQueryLogConfigsResponse -> TestTree
responseListResolverQueryLogConfigs =
  res
    "ListResolverQueryLogConfigsResponse"
    "fixture/ListResolverQueryLogConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResolverQueryLogConfigs)

responseDeleteFirewallRuleGroup :: DeleteFirewallRuleGroupResponse -> TestTree
responseDeleteFirewallRuleGroup =
  res
    "DeleteFirewallRuleGroupResponse"
    "fixture/DeleteFirewallRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFirewallRuleGroup)

responseListResolverEndpointIpAddresses :: ListResolverEndpointIpAddressesResponse -> TestTree
responseListResolverEndpointIpAddresses =
  res
    "ListResolverEndpointIpAddressesResponse"
    "fixture/ListResolverEndpointIpAddressesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResolverEndpointIpAddresses)

responseAssociateResolverQueryLogConfig :: AssociateResolverQueryLogConfigResponse -> TestTree
responseAssociateResolverQueryLogConfig =
  res
    "AssociateResolverQueryLogConfigResponse"
    "fixture/AssociateResolverQueryLogConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateResolverQueryLogConfig)

responseGetResolverRulePolicy :: GetResolverRulePolicyResponse -> TestTree
responseGetResolverRulePolicy =
  res
    "GetResolverRulePolicyResponse"
    "fixture/GetResolverRulePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResolverRulePolicy)

responseGetResolverDnssecConfig :: GetResolverDnssecConfigResponse -> TestTree
responseGetResolverDnssecConfig =
  res
    "GetResolverDnssecConfigResponse"
    "fixture/GetResolverDnssecConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResolverDnssecConfig)

responseListFirewallRuleGroups :: ListFirewallRuleGroupsResponse -> TestTree
responseListFirewallRuleGroups =
  res
    "ListFirewallRuleGroupsResponse"
    "fixture/ListFirewallRuleGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFirewallRuleGroups)

responseUpdateResolverRule :: UpdateResolverRuleResponse -> TestTree
responseUpdateResolverRule =
  res
    "UpdateResolverRuleResponse"
    "fixture/UpdateResolverRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResolverRule)

responseDeleteResolverRule :: DeleteResolverRuleResponse -> TestTree
responseDeleteResolverRule =
  res
    "DeleteResolverRuleResponse"
    "fixture/DeleteResolverRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResolverRule)

responseDeleteFirewallRule :: DeleteFirewallRuleResponse -> TestTree
responseDeleteFirewallRule =
  res
    "DeleteFirewallRuleResponse"
    "fixture/DeleteFirewallRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFirewallRule)

responseUpdateFirewallRule :: UpdateFirewallRuleResponse -> TestTree
responseUpdateFirewallRule =
  res
    "UpdateFirewallRuleResponse"
    "fixture/UpdateFirewallRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFirewallRule)

responseListFirewallRules :: ListFirewallRulesResponse -> TestTree
responseListFirewallRules =
  res
    "ListFirewallRulesResponse"
    "fixture/ListFirewallRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFirewallRules)

responseGetFirewallRuleGroup :: GetFirewallRuleGroupResponse -> TestTree
responseGetFirewallRuleGroup =
  res
    "GetFirewallRuleGroupResponse"
    "fixture/GetFirewallRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFirewallRuleGroup)

responseListResolverRules :: ListResolverRulesResponse -> TestTree
responseListResolverRules =
  res
    "ListResolverRulesResponse"
    "fixture/ListResolverRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResolverRules)

responseCreateResolverEndpoint :: CreateResolverEndpointResponse -> TestTree
responseCreateResolverEndpoint =
  res
    "CreateResolverEndpointResponse"
    "fixture/CreateResolverEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResolverEndpoint)

responseAssociateResolverRule :: AssociateResolverRuleResponse -> TestTree
responseAssociateResolverRule =
  res
    "AssociateResolverRuleResponse"
    "fixture/AssociateResolverRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateResolverRule)

responseGetResolverQueryLogConfigPolicy :: GetResolverQueryLogConfigPolicyResponse -> TestTree
responseGetResolverQueryLogConfigPolicy =
  res
    "GetResolverQueryLogConfigPolicyResponse"
    "fixture/GetResolverQueryLogConfigPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResolverQueryLogConfigPolicy)

responseUpdateFirewallDomains :: UpdateFirewallDomainsResponse -> TestTree
responseUpdateFirewallDomains =
  res
    "UpdateFirewallDomainsResponse"
    "fixture/UpdateFirewallDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFirewallDomains)

responseListResolverEndpoints :: ListResolverEndpointsResponse -> TestTree
responseListResolverEndpoints =
  res
    "ListResolverEndpointsResponse"
    "fixture/ListResolverEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResolverEndpoints)

responseListFirewallDomains :: ListFirewallDomainsResponse -> TestTree
responseListFirewallDomains =
  res
    "ListFirewallDomainsResponse"
    "fixture/ListFirewallDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFirewallDomains)

responseGetResolverRuleAssociation :: GetResolverRuleAssociationResponse -> TestTree
responseGetResolverRuleAssociation =
  res
    "GetResolverRuleAssociationResponse"
    "fixture/GetResolverRuleAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResolverRuleAssociation)

responseGetFirewallConfig :: GetFirewallConfigResponse -> TestTree
responseGetFirewallConfig =
  res
    "GetFirewallConfigResponse"
    "fixture/GetFirewallConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFirewallConfig)

responseGetFirewallDomainList :: GetFirewallDomainListResponse -> TestTree
responseGetFirewallDomainList =
  res
    "GetFirewallDomainListResponse"
    "fixture/GetFirewallDomainListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFirewallDomainList)

responseDisassociateResolverRule :: DisassociateResolverRuleResponse -> TestTree
responseDisassociateResolverRule =
  res
    "DisassociateResolverRuleResponse"
    "fixture/DisassociateResolverRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateResolverRule)

responseGetResolverQueryLogConfigAssociation :: GetResolverQueryLogConfigAssociationResponse -> TestTree
responseGetResolverQueryLogConfigAssociation =
  res
    "GetResolverQueryLogConfigAssociationResponse"
    "fixture/GetResolverQueryLogConfigAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResolverQueryLogConfigAssociation)

responseListFirewallDomainLists :: ListFirewallDomainListsResponse -> TestTree
responseListFirewallDomainLists =
  res
    "ListFirewallDomainListsResponse"
    "fixture/ListFirewallDomainListsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFirewallDomainLists)

responseDisassociateFirewallRuleGroup :: DisassociateFirewallRuleGroupResponse -> TestTree
responseDisassociateFirewallRuleGroup =
  res
    "DisassociateFirewallRuleGroupResponse"
    "fixture/DisassociateFirewallRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateFirewallRuleGroup)

responseUpdateFirewallConfig :: UpdateFirewallConfigResponse -> TestTree
responseUpdateFirewallConfig =
  res
    "UpdateFirewallConfigResponse"
    "fixture/UpdateFirewallConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFirewallConfig)

responseDeleteFirewallDomainList :: DeleteFirewallDomainListResponse -> TestTree
responseDeleteFirewallDomainList =
  res
    "DeleteFirewallDomainListResponse"
    "fixture/DeleteFirewallDomainListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFirewallDomainList)

responseListFirewallConfigs :: ListFirewallConfigsResponse -> TestTree
responseListFirewallConfigs =
  res
    "ListFirewallConfigsResponse"
    "fixture/ListFirewallConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFirewallConfigs)

responseCreateFirewallDomainList :: CreateFirewallDomainListResponse -> TestTree
responseCreateFirewallDomainList =
  res
    "CreateFirewallDomainListResponse"
    "fixture/CreateFirewallDomainListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFirewallDomainList)

responseImportFirewallDomains :: ImportFirewallDomainsResponse -> TestTree
responseImportFirewallDomains =
  res
    "ImportFirewallDomainsResponse"
    "fixture/ImportFirewallDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportFirewallDomains)

responseDisassociateResolverQueryLogConfig :: DisassociateResolverQueryLogConfigResponse -> TestTree
responseDisassociateResolverQueryLogConfig =
  res
    "DisassociateResolverQueryLogConfigResponse"
    "fixture/DisassociateResolverQueryLogConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateResolverQueryLogConfig)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseAssociateFirewallRuleGroup :: AssociateFirewallRuleGroupResponse -> TestTree
responseAssociateFirewallRuleGroup =
  res
    "AssociateFirewallRuleGroupResponse"
    "fixture/AssociateFirewallRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateFirewallRuleGroup)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responsePutResolverQueryLogConfigPolicy :: PutResolverQueryLogConfigPolicyResponse -> TestTree
responsePutResolverQueryLogConfigPolicy =
  res
    "PutResolverQueryLogConfigPolicyResponse"
    "fixture/PutResolverQueryLogConfigPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResolverQueryLogConfigPolicy)

responseAssociateResolverEndpointIpAddress :: AssociateResolverEndpointIpAddressResponse -> TestTree
responseAssociateResolverEndpointIpAddress =
  res
    "AssociateResolverEndpointIpAddressResponse"
    "fixture/AssociateResolverEndpointIpAddressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateResolverEndpointIpAddress)

responseCreateResolverQueryLogConfig :: CreateResolverQueryLogConfigResponse -> TestTree
responseCreateResolverQueryLogConfig =
  res
    "CreateResolverQueryLogConfigResponse"
    "fixture/CreateResolverQueryLogConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResolverQueryLogConfig)

responseGetResolverRule :: GetResolverRuleResponse -> TestTree
responseGetResolverRule =
  res
    "GetResolverRuleResponse"
    "fixture/GetResolverRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResolverRule)

responsePutFirewallRuleGroupPolicy :: PutFirewallRuleGroupPolicyResponse -> TestTree
responsePutFirewallRuleGroupPolicy =
  res
    "PutFirewallRuleGroupPolicyResponse"
    "fixture/PutFirewallRuleGroupPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutFirewallRuleGroupPolicy)

responsePutResolverRulePolicy :: PutResolverRulePolicyResponse -> TestTree
responsePutResolverRulePolicy =
  res
    "PutResolverRulePolicyResponse"
    "fixture/PutResolverRulePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResolverRulePolicy)

responseListResolverDnssecConfigs :: ListResolverDnssecConfigsResponse -> TestTree
responseListResolverDnssecConfigs =
  res
    "ListResolverDnssecConfigsResponse"
    "fixture/ListResolverDnssecConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResolverDnssecConfigs)

responseUpdateResolverDnssecConfig :: UpdateResolverDnssecConfigResponse -> TestTree
responseUpdateResolverDnssecConfig =
  res
    "UpdateResolverDnssecConfigResponse"
    "fixture/UpdateResolverDnssecConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResolverDnssecConfig)

responseGetFirewallRuleGroupPolicy :: GetFirewallRuleGroupPolicyResponse -> TestTree
responseGetFirewallRuleGroupPolicy =
  res
    "GetFirewallRuleGroupPolicyResponse"
    "fixture/GetFirewallRuleGroupPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFirewallRuleGroupPolicy)
