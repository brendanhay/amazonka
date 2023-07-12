{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Route53Resolver
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Route53Resolver where

import Amazonka.Route53Resolver
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.Route53Resolver.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateFirewallRuleGroup $
--             newAssociateFirewallRuleGroup
--
--         , requestAssociateResolverEndpointIpAddress $
--             newAssociateResolverEndpointIpAddress
--
--         , requestAssociateResolverQueryLogConfig $
--             newAssociateResolverQueryLogConfig
--
--         , requestAssociateResolverRule $
--             newAssociateResolverRule
--
--         , requestCreateFirewallDomainList $
--             newCreateFirewallDomainList
--
--         , requestCreateFirewallRule $
--             newCreateFirewallRule
--
--         , requestCreateFirewallRuleGroup $
--             newCreateFirewallRuleGroup
--
--         , requestCreateResolverEndpoint $
--             newCreateResolverEndpoint
--
--         , requestCreateResolverQueryLogConfig $
--             newCreateResolverQueryLogConfig
--
--         , requestCreateResolverRule $
--             newCreateResolverRule
--
--         , requestDeleteFirewallDomainList $
--             newDeleteFirewallDomainList
--
--         , requestDeleteFirewallRule $
--             newDeleteFirewallRule
--
--         , requestDeleteFirewallRuleGroup $
--             newDeleteFirewallRuleGroup
--
--         , requestDeleteResolverEndpoint $
--             newDeleteResolverEndpoint
--
--         , requestDeleteResolverQueryLogConfig $
--             newDeleteResolverQueryLogConfig
--
--         , requestDeleteResolverRule $
--             newDeleteResolverRule
--
--         , requestDisassociateFirewallRuleGroup $
--             newDisassociateFirewallRuleGroup
--
--         , requestDisassociateResolverEndpointIpAddress $
--             newDisassociateResolverEndpointIpAddress
--
--         , requestDisassociateResolverQueryLogConfig $
--             newDisassociateResolverQueryLogConfig
--
--         , requestDisassociateResolverRule $
--             newDisassociateResolverRule
--
--         , requestGetFirewallConfig $
--             newGetFirewallConfig
--
--         , requestGetFirewallDomainList $
--             newGetFirewallDomainList
--
--         , requestGetFirewallRuleGroup $
--             newGetFirewallRuleGroup
--
--         , requestGetFirewallRuleGroupAssociation $
--             newGetFirewallRuleGroupAssociation
--
--         , requestGetFirewallRuleGroupPolicy $
--             newGetFirewallRuleGroupPolicy
--
--         , requestGetResolverConfig $
--             newGetResolverConfig
--
--         , requestGetResolverDnssecConfig $
--             newGetResolverDnssecConfig
--
--         , requestGetResolverEndpoint $
--             newGetResolverEndpoint
--
--         , requestGetResolverQueryLogConfig $
--             newGetResolverQueryLogConfig
--
--         , requestGetResolverQueryLogConfigAssociation $
--             newGetResolverQueryLogConfigAssociation
--
--         , requestGetResolverQueryLogConfigPolicy $
--             newGetResolverQueryLogConfigPolicy
--
--         , requestGetResolverRule $
--             newGetResolverRule
--
--         , requestGetResolverRuleAssociation $
--             newGetResolverRuleAssociation
--
--         , requestGetResolverRulePolicy $
--             newGetResolverRulePolicy
--
--         , requestImportFirewallDomains $
--             newImportFirewallDomains
--
--         , requestListFirewallConfigs $
--             newListFirewallConfigs
--
--         , requestListFirewallDomainLists $
--             newListFirewallDomainLists
--
--         , requestListFirewallDomains $
--             newListFirewallDomains
--
--         , requestListFirewallRuleGroupAssociations $
--             newListFirewallRuleGroupAssociations
--
--         , requestListFirewallRuleGroups $
--             newListFirewallRuleGroups
--
--         , requestListFirewallRules $
--             newListFirewallRules
--
--         , requestListResolverConfigs $
--             newListResolverConfigs
--
--         , requestListResolverDnssecConfigs $
--             newListResolverDnssecConfigs
--
--         , requestListResolverEndpointIpAddresses $
--             newListResolverEndpointIpAddresses
--
--         , requestListResolverEndpoints $
--             newListResolverEndpoints
--
--         , requestListResolverQueryLogConfigAssociations $
--             newListResolverQueryLogConfigAssociations
--
--         , requestListResolverQueryLogConfigs $
--             newListResolverQueryLogConfigs
--
--         , requestListResolverRuleAssociations $
--             newListResolverRuleAssociations
--
--         , requestListResolverRules $
--             newListResolverRules
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutFirewallRuleGroupPolicy $
--             newPutFirewallRuleGroupPolicy
--
--         , requestPutResolverQueryLogConfigPolicy $
--             newPutResolverQueryLogConfigPolicy
--
--         , requestPutResolverRulePolicy $
--             newPutResolverRulePolicy
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateFirewallConfig $
--             newUpdateFirewallConfig
--
--         , requestUpdateFirewallDomains $
--             newUpdateFirewallDomains
--
--         , requestUpdateFirewallRule $
--             newUpdateFirewallRule
--
--         , requestUpdateFirewallRuleGroupAssociation $
--             newUpdateFirewallRuleGroupAssociation
--
--         , requestUpdateResolverConfig $
--             newUpdateResolverConfig
--
--         , requestUpdateResolverDnssecConfig $
--             newUpdateResolverDnssecConfig
--
--         , requestUpdateResolverEndpoint $
--             newUpdateResolverEndpoint
--
--         , requestUpdateResolverRule $
--             newUpdateResolverRule
--
--           ]

--     , testGroup "response"
--         [ responseAssociateFirewallRuleGroup $
--             newAssociateFirewallRuleGroupResponse
--
--         , responseAssociateResolverEndpointIpAddress $
--             newAssociateResolverEndpointIpAddressResponse
--
--         , responseAssociateResolverQueryLogConfig $
--             newAssociateResolverQueryLogConfigResponse
--
--         , responseAssociateResolverRule $
--             newAssociateResolverRuleResponse
--
--         , responseCreateFirewallDomainList $
--             newCreateFirewallDomainListResponse
--
--         , responseCreateFirewallRule $
--             newCreateFirewallRuleResponse
--
--         , responseCreateFirewallRuleGroup $
--             newCreateFirewallRuleGroupResponse
--
--         , responseCreateResolverEndpoint $
--             newCreateResolverEndpointResponse
--
--         , responseCreateResolverQueryLogConfig $
--             newCreateResolverQueryLogConfigResponse
--
--         , responseCreateResolverRule $
--             newCreateResolverRuleResponse
--
--         , responseDeleteFirewallDomainList $
--             newDeleteFirewallDomainListResponse
--
--         , responseDeleteFirewallRule $
--             newDeleteFirewallRuleResponse
--
--         , responseDeleteFirewallRuleGroup $
--             newDeleteFirewallRuleGroupResponse
--
--         , responseDeleteResolverEndpoint $
--             newDeleteResolverEndpointResponse
--
--         , responseDeleteResolverQueryLogConfig $
--             newDeleteResolverQueryLogConfigResponse
--
--         , responseDeleteResolverRule $
--             newDeleteResolverRuleResponse
--
--         , responseDisassociateFirewallRuleGroup $
--             newDisassociateFirewallRuleGroupResponse
--
--         , responseDisassociateResolverEndpointIpAddress $
--             newDisassociateResolverEndpointIpAddressResponse
--
--         , responseDisassociateResolverQueryLogConfig $
--             newDisassociateResolverQueryLogConfigResponse
--
--         , responseDisassociateResolverRule $
--             newDisassociateResolverRuleResponse
--
--         , responseGetFirewallConfig $
--             newGetFirewallConfigResponse
--
--         , responseGetFirewallDomainList $
--             newGetFirewallDomainListResponse
--
--         , responseGetFirewallRuleGroup $
--             newGetFirewallRuleGroupResponse
--
--         , responseGetFirewallRuleGroupAssociation $
--             newGetFirewallRuleGroupAssociationResponse
--
--         , responseGetFirewallRuleGroupPolicy $
--             newGetFirewallRuleGroupPolicyResponse
--
--         , responseGetResolverConfig $
--             newGetResolverConfigResponse
--
--         , responseGetResolverDnssecConfig $
--             newGetResolverDnssecConfigResponse
--
--         , responseGetResolverEndpoint $
--             newGetResolverEndpointResponse
--
--         , responseGetResolverQueryLogConfig $
--             newGetResolverQueryLogConfigResponse
--
--         , responseGetResolverQueryLogConfigAssociation $
--             newGetResolverQueryLogConfigAssociationResponse
--
--         , responseGetResolverQueryLogConfigPolicy $
--             newGetResolverQueryLogConfigPolicyResponse
--
--         , responseGetResolverRule $
--             newGetResolverRuleResponse
--
--         , responseGetResolverRuleAssociation $
--             newGetResolverRuleAssociationResponse
--
--         , responseGetResolverRulePolicy $
--             newGetResolverRulePolicyResponse
--
--         , responseImportFirewallDomains $
--             newImportFirewallDomainsResponse
--
--         , responseListFirewallConfigs $
--             newListFirewallConfigsResponse
--
--         , responseListFirewallDomainLists $
--             newListFirewallDomainListsResponse
--
--         , responseListFirewallDomains $
--             newListFirewallDomainsResponse
--
--         , responseListFirewallRuleGroupAssociations $
--             newListFirewallRuleGroupAssociationsResponse
--
--         , responseListFirewallRuleGroups $
--             newListFirewallRuleGroupsResponse
--
--         , responseListFirewallRules $
--             newListFirewallRulesResponse
--
--         , responseListResolverConfigs $
--             newListResolverConfigsResponse
--
--         , responseListResolverDnssecConfigs $
--             newListResolverDnssecConfigsResponse
--
--         , responseListResolverEndpointIpAddresses $
--             newListResolverEndpointIpAddressesResponse
--
--         , responseListResolverEndpoints $
--             newListResolverEndpointsResponse
--
--         , responseListResolverQueryLogConfigAssociations $
--             newListResolverQueryLogConfigAssociationsResponse
--
--         , responseListResolverQueryLogConfigs $
--             newListResolverQueryLogConfigsResponse
--
--         , responseListResolverRuleAssociations $
--             newListResolverRuleAssociationsResponse
--
--         , responseListResolverRules $
--             newListResolverRulesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutFirewallRuleGroupPolicy $
--             newPutFirewallRuleGroupPolicyResponse
--
--         , responsePutResolverQueryLogConfigPolicy $
--             newPutResolverQueryLogConfigPolicyResponse
--
--         , responsePutResolverRulePolicy $
--             newPutResolverRulePolicyResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateFirewallConfig $
--             newUpdateFirewallConfigResponse
--
--         , responseUpdateFirewallDomains $
--             newUpdateFirewallDomainsResponse
--
--         , responseUpdateFirewallRule $
--             newUpdateFirewallRuleResponse
--
--         , responseUpdateFirewallRuleGroupAssociation $
--             newUpdateFirewallRuleGroupAssociationResponse
--
--         , responseUpdateResolverConfig $
--             newUpdateResolverConfigResponse
--
--         , responseUpdateResolverDnssecConfig $
--             newUpdateResolverDnssecConfigResponse
--
--         , responseUpdateResolverEndpoint $
--             newUpdateResolverEndpointResponse
--
--         , responseUpdateResolverRule $
--             newUpdateResolverRuleResponse
--
--           ]
--     ]

-- Requests

requestAssociateFirewallRuleGroup :: AssociateFirewallRuleGroup -> TestTree
requestAssociateFirewallRuleGroup =
  req
    "AssociateFirewallRuleGroup"
    "fixture/AssociateFirewallRuleGroup.yaml"

requestAssociateResolverEndpointIpAddress :: AssociateResolverEndpointIpAddress -> TestTree
requestAssociateResolverEndpointIpAddress =
  req
    "AssociateResolverEndpointIpAddress"
    "fixture/AssociateResolverEndpointIpAddress.yaml"

requestAssociateResolverQueryLogConfig :: AssociateResolverQueryLogConfig -> TestTree
requestAssociateResolverQueryLogConfig =
  req
    "AssociateResolverQueryLogConfig"
    "fixture/AssociateResolverQueryLogConfig.yaml"

requestAssociateResolverRule :: AssociateResolverRule -> TestTree
requestAssociateResolverRule =
  req
    "AssociateResolverRule"
    "fixture/AssociateResolverRule.yaml"

requestCreateFirewallDomainList :: CreateFirewallDomainList -> TestTree
requestCreateFirewallDomainList =
  req
    "CreateFirewallDomainList"
    "fixture/CreateFirewallDomainList.yaml"

requestCreateFirewallRule :: CreateFirewallRule -> TestTree
requestCreateFirewallRule =
  req
    "CreateFirewallRule"
    "fixture/CreateFirewallRule.yaml"

requestCreateFirewallRuleGroup :: CreateFirewallRuleGroup -> TestTree
requestCreateFirewallRuleGroup =
  req
    "CreateFirewallRuleGroup"
    "fixture/CreateFirewallRuleGroup.yaml"

requestCreateResolverEndpoint :: CreateResolverEndpoint -> TestTree
requestCreateResolverEndpoint =
  req
    "CreateResolverEndpoint"
    "fixture/CreateResolverEndpoint.yaml"

requestCreateResolverQueryLogConfig :: CreateResolverQueryLogConfig -> TestTree
requestCreateResolverQueryLogConfig =
  req
    "CreateResolverQueryLogConfig"
    "fixture/CreateResolverQueryLogConfig.yaml"

requestCreateResolverRule :: CreateResolverRule -> TestTree
requestCreateResolverRule =
  req
    "CreateResolverRule"
    "fixture/CreateResolverRule.yaml"

requestDeleteFirewallDomainList :: DeleteFirewallDomainList -> TestTree
requestDeleteFirewallDomainList =
  req
    "DeleteFirewallDomainList"
    "fixture/DeleteFirewallDomainList.yaml"

requestDeleteFirewallRule :: DeleteFirewallRule -> TestTree
requestDeleteFirewallRule =
  req
    "DeleteFirewallRule"
    "fixture/DeleteFirewallRule.yaml"

requestDeleteFirewallRuleGroup :: DeleteFirewallRuleGroup -> TestTree
requestDeleteFirewallRuleGroup =
  req
    "DeleteFirewallRuleGroup"
    "fixture/DeleteFirewallRuleGroup.yaml"

requestDeleteResolverEndpoint :: DeleteResolverEndpoint -> TestTree
requestDeleteResolverEndpoint =
  req
    "DeleteResolverEndpoint"
    "fixture/DeleteResolverEndpoint.yaml"

requestDeleteResolverQueryLogConfig :: DeleteResolverQueryLogConfig -> TestTree
requestDeleteResolverQueryLogConfig =
  req
    "DeleteResolverQueryLogConfig"
    "fixture/DeleteResolverQueryLogConfig.yaml"

requestDeleteResolverRule :: DeleteResolverRule -> TestTree
requestDeleteResolverRule =
  req
    "DeleteResolverRule"
    "fixture/DeleteResolverRule.yaml"

requestDisassociateFirewallRuleGroup :: DisassociateFirewallRuleGroup -> TestTree
requestDisassociateFirewallRuleGroup =
  req
    "DisassociateFirewallRuleGroup"
    "fixture/DisassociateFirewallRuleGroup.yaml"

requestDisassociateResolverEndpointIpAddress :: DisassociateResolverEndpointIpAddress -> TestTree
requestDisassociateResolverEndpointIpAddress =
  req
    "DisassociateResolverEndpointIpAddress"
    "fixture/DisassociateResolverEndpointIpAddress.yaml"

requestDisassociateResolverQueryLogConfig :: DisassociateResolverQueryLogConfig -> TestTree
requestDisassociateResolverQueryLogConfig =
  req
    "DisassociateResolverQueryLogConfig"
    "fixture/DisassociateResolverQueryLogConfig.yaml"

requestDisassociateResolverRule :: DisassociateResolverRule -> TestTree
requestDisassociateResolverRule =
  req
    "DisassociateResolverRule"
    "fixture/DisassociateResolverRule.yaml"

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

requestGetFirewallRuleGroup :: GetFirewallRuleGroup -> TestTree
requestGetFirewallRuleGroup =
  req
    "GetFirewallRuleGroup"
    "fixture/GetFirewallRuleGroup.yaml"

requestGetFirewallRuleGroupAssociation :: GetFirewallRuleGroupAssociation -> TestTree
requestGetFirewallRuleGroupAssociation =
  req
    "GetFirewallRuleGroupAssociation"
    "fixture/GetFirewallRuleGroupAssociation.yaml"

requestGetFirewallRuleGroupPolicy :: GetFirewallRuleGroupPolicy -> TestTree
requestGetFirewallRuleGroupPolicy =
  req
    "GetFirewallRuleGroupPolicy"
    "fixture/GetFirewallRuleGroupPolicy.yaml"

requestGetResolverConfig :: GetResolverConfig -> TestTree
requestGetResolverConfig =
  req
    "GetResolverConfig"
    "fixture/GetResolverConfig.yaml"

requestGetResolverDnssecConfig :: GetResolverDnssecConfig -> TestTree
requestGetResolverDnssecConfig =
  req
    "GetResolverDnssecConfig"
    "fixture/GetResolverDnssecConfig.yaml"

requestGetResolverEndpoint :: GetResolverEndpoint -> TestTree
requestGetResolverEndpoint =
  req
    "GetResolverEndpoint"
    "fixture/GetResolverEndpoint.yaml"

requestGetResolverQueryLogConfig :: GetResolverQueryLogConfig -> TestTree
requestGetResolverQueryLogConfig =
  req
    "GetResolverQueryLogConfig"
    "fixture/GetResolverQueryLogConfig.yaml"

requestGetResolverQueryLogConfigAssociation :: GetResolverQueryLogConfigAssociation -> TestTree
requestGetResolverQueryLogConfigAssociation =
  req
    "GetResolverQueryLogConfigAssociation"
    "fixture/GetResolverQueryLogConfigAssociation.yaml"

requestGetResolverQueryLogConfigPolicy :: GetResolverQueryLogConfigPolicy -> TestTree
requestGetResolverQueryLogConfigPolicy =
  req
    "GetResolverQueryLogConfigPolicy"
    "fixture/GetResolverQueryLogConfigPolicy.yaml"

requestGetResolverRule :: GetResolverRule -> TestTree
requestGetResolverRule =
  req
    "GetResolverRule"
    "fixture/GetResolverRule.yaml"

requestGetResolverRuleAssociation :: GetResolverRuleAssociation -> TestTree
requestGetResolverRuleAssociation =
  req
    "GetResolverRuleAssociation"
    "fixture/GetResolverRuleAssociation.yaml"

requestGetResolverRulePolicy :: GetResolverRulePolicy -> TestTree
requestGetResolverRulePolicy =
  req
    "GetResolverRulePolicy"
    "fixture/GetResolverRulePolicy.yaml"

requestImportFirewallDomains :: ImportFirewallDomains -> TestTree
requestImportFirewallDomains =
  req
    "ImportFirewallDomains"
    "fixture/ImportFirewallDomains.yaml"

requestListFirewallConfigs :: ListFirewallConfigs -> TestTree
requestListFirewallConfigs =
  req
    "ListFirewallConfigs"
    "fixture/ListFirewallConfigs.yaml"

requestListFirewallDomainLists :: ListFirewallDomainLists -> TestTree
requestListFirewallDomainLists =
  req
    "ListFirewallDomainLists"
    "fixture/ListFirewallDomainLists.yaml"

requestListFirewallDomains :: ListFirewallDomains -> TestTree
requestListFirewallDomains =
  req
    "ListFirewallDomains"
    "fixture/ListFirewallDomains.yaml"

requestListFirewallRuleGroupAssociations :: ListFirewallRuleGroupAssociations -> TestTree
requestListFirewallRuleGroupAssociations =
  req
    "ListFirewallRuleGroupAssociations"
    "fixture/ListFirewallRuleGroupAssociations.yaml"

requestListFirewallRuleGroups :: ListFirewallRuleGroups -> TestTree
requestListFirewallRuleGroups =
  req
    "ListFirewallRuleGroups"
    "fixture/ListFirewallRuleGroups.yaml"

requestListFirewallRules :: ListFirewallRules -> TestTree
requestListFirewallRules =
  req
    "ListFirewallRules"
    "fixture/ListFirewallRules.yaml"

requestListResolverConfigs :: ListResolverConfigs -> TestTree
requestListResolverConfigs =
  req
    "ListResolverConfigs"
    "fixture/ListResolverConfigs.yaml"

requestListResolverDnssecConfigs :: ListResolverDnssecConfigs -> TestTree
requestListResolverDnssecConfigs =
  req
    "ListResolverDnssecConfigs"
    "fixture/ListResolverDnssecConfigs.yaml"

requestListResolverEndpointIpAddresses :: ListResolverEndpointIpAddresses -> TestTree
requestListResolverEndpointIpAddresses =
  req
    "ListResolverEndpointIpAddresses"
    "fixture/ListResolverEndpointIpAddresses.yaml"

requestListResolverEndpoints :: ListResolverEndpoints -> TestTree
requestListResolverEndpoints =
  req
    "ListResolverEndpoints"
    "fixture/ListResolverEndpoints.yaml"

requestListResolverQueryLogConfigAssociations :: ListResolverQueryLogConfigAssociations -> TestTree
requestListResolverQueryLogConfigAssociations =
  req
    "ListResolverQueryLogConfigAssociations"
    "fixture/ListResolverQueryLogConfigAssociations.yaml"

requestListResolverQueryLogConfigs :: ListResolverQueryLogConfigs -> TestTree
requestListResolverQueryLogConfigs =
  req
    "ListResolverQueryLogConfigs"
    "fixture/ListResolverQueryLogConfigs.yaml"

requestListResolverRuleAssociations :: ListResolverRuleAssociations -> TestTree
requestListResolverRuleAssociations =
  req
    "ListResolverRuleAssociations"
    "fixture/ListResolverRuleAssociations.yaml"

requestListResolverRules :: ListResolverRules -> TestTree
requestListResolverRules =
  req
    "ListResolverRules"
    "fixture/ListResolverRules.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutFirewallRuleGroupPolicy :: PutFirewallRuleGroupPolicy -> TestTree
requestPutFirewallRuleGroupPolicy =
  req
    "PutFirewallRuleGroupPolicy"
    "fixture/PutFirewallRuleGroupPolicy.yaml"

requestPutResolverQueryLogConfigPolicy :: PutResolverQueryLogConfigPolicy -> TestTree
requestPutResolverQueryLogConfigPolicy =
  req
    "PutResolverQueryLogConfigPolicy"
    "fixture/PutResolverQueryLogConfigPolicy.yaml"

requestPutResolverRulePolicy :: PutResolverRulePolicy -> TestTree
requestPutResolverRulePolicy =
  req
    "PutResolverRulePolicy"
    "fixture/PutResolverRulePolicy.yaml"

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

requestUpdateFirewallConfig :: UpdateFirewallConfig -> TestTree
requestUpdateFirewallConfig =
  req
    "UpdateFirewallConfig"
    "fixture/UpdateFirewallConfig.yaml"

requestUpdateFirewallDomains :: UpdateFirewallDomains -> TestTree
requestUpdateFirewallDomains =
  req
    "UpdateFirewallDomains"
    "fixture/UpdateFirewallDomains.yaml"

requestUpdateFirewallRule :: UpdateFirewallRule -> TestTree
requestUpdateFirewallRule =
  req
    "UpdateFirewallRule"
    "fixture/UpdateFirewallRule.yaml"

requestUpdateFirewallRuleGroupAssociation :: UpdateFirewallRuleGroupAssociation -> TestTree
requestUpdateFirewallRuleGroupAssociation =
  req
    "UpdateFirewallRuleGroupAssociation"
    "fixture/UpdateFirewallRuleGroupAssociation.yaml"

requestUpdateResolverConfig :: UpdateResolverConfig -> TestTree
requestUpdateResolverConfig =
  req
    "UpdateResolverConfig"
    "fixture/UpdateResolverConfig.yaml"

requestUpdateResolverDnssecConfig :: UpdateResolverDnssecConfig -> TestTree
requestUpdateResolverDnssecConfig =
  req
    "UpdateResolverDnssecConfig"
    "fixture/UpdateResolverDnssecConfig.yaml"

requestUpdateResolverEndpoint :: UpdateResolverEndpoint -> TestTree
requestUpdateResolverEndpoint =
  req
    "UpdateResolverEndpoint"
    "fixture/UpdateResolverEndpoint.yaml"

requestUpdateResolverRule :: UpdateResolverRule -> TestTree
requestUpdateResolverRule =
  req
    "UpdateResolverRule"
    "fixture/UpdateResolverRule.yaml"

-- Responses

responseAssociateFirewallRuleGroup :: AssociateFirewallRuleGroupResponse -> TestTree
responseAssociateFirewallRuleGroup =
  res
    "AssociateFirewallRuleGroupResponse"
    "fixture/AssociateFirewallRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateFirewallRuleGroup)

responseAssociateResolverEndpointIpAddress :: AssociateResolverEndpointIpAddressResponse -> TestTree
responseAssociateResolverEndpointIpAddress =
  res
    "AssociateResolverEndpointIpAddressResponse"
    "fixture/AssociateResolverEndpointIpAddressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateResolverEndpointIpAddress)

responseAssociateResolverQueryLogConfig :: AssociateResolverQueryLogConfigResponse -> TestTree
responseAssociateResolverQueryLogConfig =
  res
    "AssociateResolverQueryLogConfigResponse"
    "fixture/AssociateResolverQueryLogConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateResolverQueryLogConfig)

responseAssociateResolverRule :: AssociateResolverRuleResponse -> TestTree
responseAssociateResolverRule =
  res
    "AssociateResolverRuleResponse"
    "fixture/AssociateResolverRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateResolverRule)

responseCreateFirewallDomainList :: CreateFirewallDomainListResponse -> TestTree
responseCreateFirewallDomainList =
  res
    "CreateFirewallDomainListResponse"
    "fixture/CreateFirewallDomainListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFirewallDomainList)

responseCreateFirewallRule :: CreateFirewallRuleResponse -> TestTree
responseCreateFirewallRule =
  res
    "CreateFirewallRuleResponse"
    "fixture/CreateFirewallRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFirewallRule)

responseCreateFirewallRuleGroup :: CreateFirewallRuleGroupResponse -> TestTree
responseCreateFirewallRuleGroup =
  res
    "CreateFirewallRuleGroupResponse"
    "fixture/CreateFirewallRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFirewallRuleGroup)

responseCreateResolverEndpoint :: CreateResolverEndpointResponse -> TestTree
responseCreateResolverEndpoint =
  res
    "CreateResolverEndpointResponse"
    "fixture/CreateResolverEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResolverEndpoint)

responseCreateResolverQueryLogConfig :: CreateResolverQueryLogConfigResponse -> TestTree
responseCreateResolverQueryLogConfig =
  res
    "CreateResolverQueryLogConfigResponse"
    "fixture/CreateResolverQueryLogConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResolverQueryLogConfig)

responseCreateResolverRule :: CreateResolverRuleResponse -> TestTree
responseCreateResolverRule =
  res
    "CreateResolverRuleResponse"
    "fixture/CreateResolverRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResolverRule)

responseDeleteFirewallDomainList :: DeleteFirewallDomainListResponse -> TestTree
responseDeleteFirewallDomainList =
  res
    "DeleteFirewallDomainListResponse"
    "fixture/DeleteFirewallDomainListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFirewallDomainList)

responseDeleteFirewallRule :: DeleteFirewallRuleResponse -> TestTree
responseDeleteFirewallRule =
  res
    "DeleteFirewallRuleResponse"
    "fixture/DeleteFirewallRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFirewallRule)

responseDeleteFirewallRuleGroup :: DeleteFirewallRuleGroupResponse -> TestTree
responseDeleteFirewallRuleGroup =
  res
    "DeleteFirewallRuleGroupResponse"
    "fixture/DeleteFirewallRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFirewallRuleGroup)

responseDeleteResolverEndpoint :: DeleteResolverEndpointResponse -> TestTree
responseDeleteResolverEndpoint =
  res
    "DeleteResolverEndpointResponse"
    "fixture/DeleteResolverEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResolverEndpoint)

responseDeleteResolverQueryLogConfig :: DeleteResolverQueryLogConfigResponse -> TestTree
responseDeleteResolverQueryLogConfig =
  res
    "DeleteResolverQueryLogConfigResponse"
    "fixture/DeleteResolverQueryLogConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResolverQueryLogConfig)

responseDeleteResolverRule :: DeleteResolverRuleResponse -> TestTree
responseDeleteResolverRule =
  res
    "DeleteResolverRuleResponse"
    "fixture/DeleteResolverRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResolverRule)

responseDisassociateFirewallRuleGroup :: DisassociateFirewallRuleGroupResponse -> TestTree
responseDisassociateFirewallRuleGroup =
  res
    "DisassociateFirewallRuleGroupResponse"
    "fixture/DisassociateFirewallRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateFirewallRuleGroup)

responseDisassociateResolverEndpointIpAddress :: DisassociateResolverEndpointIpAddressResponse -> TestTree
responseDisassociateResolverEndpointIpAddress =
  res
    "DisassociateResolverEndpointIpAddressResponse"
    "fixture/DisassociateResolverEndpointIpAddressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateResolverEndpointIpAddress)

responseDisassociateResolverQueryLogConfig :: DisassociateResolverQueryLogConfigResponse -> TestTree
responseDisassociateResolverQueryLogConfig =
  res
    "DisassociateResolverQueryLogConfigResponse"
    "fixture/DisassociateResolverQueryLogConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateResolverQueryLogConfig)

responseDisassociateResolverRule :: DisassociateResolverRuleResponse -> TestTree
responseDisassociateResolverRule =
  res
    "DisassociateResolverRuleResponse"
    "fixture/DisassociateResolverRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateResolverRule)

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

responseGetFirewallRuleGroup :: GetFirewallRuleGroupResponse -> TestTree
responseGetFirewallRuleGroup =
  res
    "GetFirewallRuleGroupResponse"
    "fixture/GetFirewallRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFirewallRuleGroup)

responseGetFirewallRuleGroupAssociation :: GetFirewallRuleGroupAssociationResponse -> TestTree
responseGetFirewallRuleGroupAssociation =
  res
    "GetFirewallRuleGroupAssociationResponse"
    "fixture/GetFirewallRuleGroupAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFirewallRuleGroupAssociation)

responseGetFirewallRuleGroupPolicy :: GetFirewallRuleGroupPolicyResponse -> TestTree
responseGetFirewallRuleGroupPolicy =
  res
    "GetFirewallRuleGroupPolicyResponse"
    "fixture/GetFirewallRuleGroupPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFirewallRuleGroupPolicy)

responseGetResolverConfig :: GetResolverConfigResponse -> TestTree
responseGetResolverConfig =
  res
    "GetResolverConfigResponse"
    "fixture/GetResolverConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResolverConfig)

responseGetResolverDnssecConfig :: GetResolverDnssecConfigResponse -> TestTree
responseGetResolverDnssecConfig =
  res
    "GetResolverDnssecConfigResponse"
    "fixture/GetResolverDnssecConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResolverDnssecConfig)

responseGetResolverEndpoint :: GetResolverEndpointResponse -> TestTree
responseGetResolverEndpoint =
  res
    "GetResolverEndpointResponse"
    "fixture/GetResolverEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResolverEndpoint)

responseGetResolverQueryLogConfig :: GetResolverQueryLogConfigResponse -> TestTree
responseGetResolverQueryLogConfig =
  res
    "GetResolverQueryLogConfigResponse"
    "fixture/GetResolverQueryLogConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResolverQueryLogConfig)

responseGetResolverQueryLogConfigAssociation :: GetResolverQueryLogConfigAssociationResponse -> TestTree
responseGetResolverQueryLogConfigAssociation =
  res
    "GetResolverQueryLogConfigAssociationResponse"
    "fixture/GetResolverQueryLogConfigAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResolverQueryLogConfigAssociation)

responseGetResolverQueryLogConfigPolicy :: GetResolverQueryLogConfigPolicyResponse -> TestTree
responseGetResolverQueryLogConfigPolicy =
  res
    "GetResolverQueryLogConfigPolicyResponse"
    "fixture/GetResolverQueryLogConfigPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResolverQueryLogConfigPolicy)

responseGetResolverRule :: GetResolverRuleResponse -> TestTree
responseGetResolverRule =
  res
    "GetResolverRuleResponse"
    "fixture/GetResolverRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResolverRule)

responseGetResolverRuleAssociation :: GetResolverRuleAssociationResponse -> TestTree
responseGetResolverRuleAssociation =
  res
    "GetResolverRuleAssociationResponse"
    "fixture/GetResolverRuleAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResolverRuleAssociation)

responseGetResolverRulePolicy :: GetResolverRulePolicyResponse -> TestTree
responseGetResolverRulePolicy =
  res
    "GetResolverRulePolicyResponse"
    "fixture/GetResolverRulePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResolverRulePolicy)

responseImportFirewallDomains :: ImportFirewallDomainsResponse -> TestTree
responseImportFirewallDomains =
  res
    "ImportFirewallDomainsResponse"
    "fixture/ImportFirewallDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportFirewallDomains)

responseListFirewallConfigs :: ListFirewallConfigsResponse -> TestTree
responseListFirewallConfigs =
  res
    "ListFirewallConfigsResponse"
    "fixture/ListFirewallConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFirewallConfigs)

responseListFirewallDomainLists :: ListFirewallDomainListsResponse -> TestTree
responseListFirewallDomainLists =
  res
    "ListFirewallDomainListsResponse"
    "fixture/ListFirewallDomainListsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFirewallDomainLists)

responseListFirewallDomains :: ListFirewallDomainsResponse -> TestTree
responseListFirewallDomains =
  res
    "ListFirewallDomainsResponse"
    "fixture/ListFirewallDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFirewallDomains)

responseListFirewallRuleGroupAssociations :: ListFirewallRuleGroupAssociationsResponse -> TestTree
responseListFirewallRuleGroupAssociations =
  res
    "ListFirewallRuleGroupAssociationsResponse"
    "fixture/ListFirewallRuleGroupAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFirewallRuleGroupAssociations)

responseListFirewallRuleGroups :: ListFirewallRuleGroupsResponse -> TestTree
responseListFirewallRuleGroups =
  res
    "ListFirewallRuleGroupsResponse"
    "fixture/ListFirewallRuleGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFirewallRuleGroups)

responseListFirewallRules :: ListFirewallRulesResponse -> TestTree
responseListFirewallRules =
  res
    "ListFirewallRulesResponse"
    "fixture/ListFirewallRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFirewallRules)

responseListResolverConfigs :: ListResolverConfigsResponse -> TestTree
responseListResolverConfigs =
  res
    "ListResolverConfigsResponse"
    "fixture/ListResolverConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResolverConfigs)

responseListResolverDnssecConfigs :: ListResolverDnssecConfigsResponse -> TestTree
responseListResolverDnssecConfigs =
  res
    "ListResolverDnssecConfigsResponse"
    "fixture/ListResolverDnssecConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResolverDnssecConfigs)

responseListResolverEndpointIpAddresses :: ListResolverEndpointIpAddressesResponse -> TestTree
responseListResolverEndpointIpAddresses =
  res
    "ListResolverEndpointIpAddressesResponse"
    "fixture/ListResolverEndpointIpAddressesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResolverEndpointIpAddresses)

responseListResolverEndpoints :: ListResolverEndpointsResponse -> TestTree
responseListResolverEndpoints =
  res
    "ListResolverEndpointsResponse"
    "fixture/ListResolverEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResolverEndpoints)

responseListResolverQueryLogConfigAssociations :: ListResolverQueryLogConfigAssociationsResponse -> TestTree
responseListResolverQueryLogConfigAssociations =
  res
    "ListResolverQueryLogConfigAssociationsResponse"
    "fixture/ListResolverQueryLogConfigAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResolverQueryLogConfigAssociations)

responseListResolverQueryLogConfigs :: ListResolverQueryLogConfigsResponse -> TestTree
responseListResolverQueryLogConfigs =
  res
    "ListResolverQueryLogConfigsResponse"
    "fixture/ListResolverQueryLogConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResolverQueryLogConfigs)

responseListResolverRuleAssociations :: ListResolverRuleAssociationsResponse -> TestTree
responseListResolverRuleAssociations =
  res
    "ListResolverRuleAssociationsResponse"
    "fixture/ListResolverRuleAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResolverRuleAssociations)

responseListResolverRules :: ListResolverRulesResponse -> TestTree
responseListResolverRules =
  res
    "ListResolverRulesResponse"
    "fixture/ListResolverRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResolverRules)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutFirewallRuleGroupPolicy :: PutFirewallRuleGroupPolicyResponse -> TestTree
responsePutFirewallRuleGroupPolicy =
  res
    "PutFirewallRuleGroupPolicyResponse"
    "fixture/PutFirewallRuleGroupPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutFirewallRuleGroupPolicy)

responsePutResolverQueryLogConfigPolicy :: PutResolverQueryLogConfigPolicyResponse -> TestTree
responsePutResolverQueryLogConfigPolicy =
  res
    "PutResolverQueryLogConfigPolicyResponse"
    "fixture/PutResolverQueryLogConfigPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResolverQueryLogConfigPolicy)

responsePutResolverRulePolicy :: PutResolverRulePolicyResponse -> TestTree
responsePutResolverRulePolicy =
  res
    "PutResolverRulePolicyResponse"
    "fixture/PutResolverRulePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResolverRulePolicy)

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

responseUpdateFirewallConfig :: UpdateFirewallConfigResponse -> TestTree
responseUpdateFirewallConfig =
  res
    "UpdateFirewallConfigResponse"
    "fixture/UpdateFirewallConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFirewallConfig)

responseUpdateFirewallDomains :: UpdateFirewallDomainsResponse -> TestTree
responseUpdateFirewallDomains =
  res
    "UpdateFirewallDomainsResponse"
    "fixture/UpdateFirewallDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFirewallDomains)

responseUpdateFirewallRule :: UpdateFirewallRuleResponse -> TestTree
responseUpdateFirewallRule =
  res
    "UpdateFirewallRuleResponse"
    "fixture/UpdateFirewallRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFirewallRule)

responseUpdateFirewallRuleGroupAssociation :: UpdateFirewallRuleGroupAssociationResponse -> TestTree
responseUpdateFirewallRuleGroupAssociation =
  res
    "UpdateFirewallRuleGroupAssociationResponse"
    "fixture/UpdateFirewallRuleGroupAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFirewallRuleGroupAssociation)

responseUpdateResolverConfig :: UpdateResolverConfigResponse -> TestTree
responseUpdateResolverConfig =
  res
    "UpdateResolverConfigResponse"
    "fixture/UpdateResolverConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResolverConfig)

responseUpdateResolverDnssecConfig :: UpdateResolverDnssecConfigResponse -> TestTree
responseUpdateResolverDnssecConfig =
  res
    "UpdateResolverDnssecConfigResponse"
    "fixture/UpdateResolverDnssecConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResolverDnssecConfig)

responseUpdateResolverEndpoint :: UpdateResolverEndpointResponse -> TestTree
responseUpdateResolverEndpoint =
  res
    "UpdateResolverEndpointResponse"
    "fixture/UpdateResolverEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResolverEndpoint)

responseUpdateResolverRule :: UpdateResolverRuleResponse -> TestTree
responseUpdateResolverRule =
  res
    "UpdateResolverRuleResponse"
    "fixture/UpdateResolverRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResolverRule)
