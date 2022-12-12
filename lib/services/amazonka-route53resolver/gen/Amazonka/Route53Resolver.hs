{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Route53Resolver
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-04-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- When you create a VPC using Amazon VPC, you automatically get DNS
-- resolution within the VPC from Route 53 Resolver. By default, Resolver
-- answers DNS queries for VPC domain names such as domain names for EC2
-- instances or Elastic Load Balancing load balancers. Resolver performs
-- recursive lookups against public name servers for all other domain
-- names.
--
-- You can also configure DNS resolution between your VPC and your network
-- over a Direct Connect or VPN connection:
--
-- __Forward DNS queries from resolvers on your network to Route 53
-- Resolver__
--
-- DNS resolvers on your network can forward DNS queries to Resolver in a
-- specified VPC. This allows your DNS resolvers to easily resolve domain
-- names for Amazon Web Services resources such as EC2 instances or records
-- in a Route 53 private hosted zone. For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/resolver.html#resolver-overview-forward-network-to-vpc How DNS Resolvers on Your Network Forward DNS Queries to Route 53 Resolver>
-- in the /Amazon Route 53 Developer Guide/.
--
-- __Conditionally forward queries from a VPC to resolvers on your
-- network__
--
-- You can configure Resolver to forward queries that it receives from EC2
-- instances in your VPCs to DNS resolvers on your network. To forward
-- selected queries, you create Resolver rules that specify the domain
-- names for the DNS queries that you want to forward (such as
-- example.com), and the IP addresses of the DNS resolvers on your network
-- that you want to forward the queries to. If a query matches multiple
-- rules (example.com, acme.example.com), Resolver chooses the rule with
-- the most specific match (acme.example.com) and forwards the query to the
-- IP addresses that you specified in that rule. For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/resolver.html#resolver-overview-forward-vpc-to-network How Route 53 Resolver Forwards DNS Queries from Your VPCs to Your Network>
-- in the /Amazon Route 53 Developer Guide/.
--
-- Like Amazon VPC, Resolver is Regional. In each Region where you have
-- VPCs, you can choose whether to forward queries from your VPCs to your
-- network (outbound queries), from your network to your VPCs (inbound
-- queries), or both.
module Amazonka.Route53Resolver
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServiceErrorException
    _InternalServiceErrorException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** InvalidPolicyDocument
    _InvalidPolicyDocument,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** InvalidTagException
    _InvalidTagException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceExistsException
    _ResourceExistsException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ResourceUnavailableException
    _ResourceUnavailableException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** UnknownResourceException
    _UnknownResourceException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateFirewallRuleGroup
    AssociateFirewallRuleGroup (AssociateFirewallRuleGroup'),
    newAssociateFirewallRuleGroup,
    AssociateFirewallRuleGroupResponse (AssociateFirewallRuleGroupResponse'),
    newAssociateFirewallRuleGroupResponse,

    -- ** AssociateResolverEndpointIpAddress
    AssociateResolverEndpointIpAddress (AssociateResolverEndpointIpAddress'),
    newAssociateResolverEndpointIpAddress,
    AssociateResolverEndpointIpAddressResponse (AssociateResolverEndpointIpAddressResponse'),
    newAssociateResolverEndpointIpAddressResponse,

    -- ** AssociateResolverQueryLogConfig
    AssociateResolverQueryLogConfig (AssociateResolverQueryLogConfig'),
    newAssociateResolverQueryLogConfig,
    AssociateResolverQueryLogConfigResponse (AssociateResolverQueryLogConfigResponse'),
    newAssociateResolverQueryLogConfigResponse,

    -- ** AssociateResolverRule
    AssociateResolverRule (AssociateResolverRule'),
    newAssociateResolverRule,
    AssociateResolverRuleResponse (AssociateResolverRuleResponse'),
    newAssociateResolverRuleResponse,

    -- ** CreateFirewallDomainList
    CreateFirewallDomainList (CreateFirewallDomainList'),
    newCreateFirewallDomainList,
    CreateFirewallDomainListResponse (CreateFirewallDomainListResponse'),
    newCreateFirewallDomainListResponse,

    -- ** CreateFirewallRule
    CreateFirewallRule (CreateFirewallRule'),
    newCreateFirewallRule,
    CreateFirewallRuleResponse (CreateFirewallRuleResponse'),
    newCreateFirewallRuleResponse,

    -- ** CreateFirewallRuleGroup
    CreateFirewallRuleGroup (CreateFirewallRuleGroup'),
    newCreateFirewallRuleGroup,
    CreateFirewallRuleGroupResponse (CreateFirewallRuleGroupResponse'),
    newCreateFirewallRuleGroupResponse,

    -- ** CreateResolverEndpoint
    CreateResolverEndpoint (CreateResolverEndpoint'),
    newCreateResolverEndpoint,
    CreateResolverEndpointResponse (CreateResolverEndpointResponse'),
    newCreateResolverEndpointResponse,

    -- ** CreateResolverQueryLogConfig
    CreateResolverQueryLogConfig (CreateResolverQueryLogConfig'),
    newCreateResolverQueryLogConfig,
    CreateResolverQueryLogConfigResponse (CreateResolverQueryLogConfigResponse'),
    newCreateResolverQueryLogConfigResponse,

    -- ** CreateResolverRule
    CreateResolverRule (CreateResolverRule'),
    newCreateResolverRule,
    CreateResolverRuleResponse (CreateResolverRuleResponse'),
    newCreateResolverRuleResponse,

    -- ** DeleteFirewallDomainList
    DeleteFirewallDomainList (DeleteFirewallDomainList'),
    newDeleteFirewallDomainList,
    DeleteFirewallDomainListResponse (DeleteFirewallDomainListResponse'),
    newDeleteFirewallDomainListResponse,

    -- ** DeleteFirewallRule
    DeleteFirewallRule (DeleteFirewallRule'),
    newDeleteFirewallRule,
    DeleteFirewallRuleResponse (DeleteFirewallRuleResponse'),
    newDeleteFirewallRuleResponse,

    -- ** DeleteFirewallRuleGroup
    DeleteFirewallRuleGroup (DeleteFirewallRuleGroup'),
    newDeleteFirewallRuleGroup,
    DeleteFirewallRuleGroupResponse (DeleteFirewallRuleGroupResponse'),
    newDeleteFirewallRuleGroupResponse,

    -- ** DeleteResolverEndpoint
    DeleteResolverEndpoint (DeleteResolverEndpoint'),
    newDeleteResolverEndpoint,
    DeleteResolverEndpointResponse (DeleteResolverEndpointResponse'),
    newDeleteResolverEndpointResponse,

    -- ** DeleteResolverQueryLogConfig
    DeleteResolverQueryLogConfig (DeleteResolverQueryLogConfig'),
    newDeleteResolverQueryLogConfig,
    DeleteResolverQueryLogConfigResponse (DeleteResolverQueryLogConfigResponse'),
    newDeleteResolverQueryLogConfigResponse,

    -- ** DeleteResolverRule
    DeleteResolverRule (DeleteResolverRule'),
    newDeleteResolverRule,
    DeleteResolverRuleResponse (DeleteResolverRuleResponse'),
    newDeleteResolverRuleResponse,

    -- ** DisassociateFirewallRuleGroup
    DisassociateFirewallRuleGroup (DisassociateFirewallRuleGroup'),
    newDisassociateFirewallRuleGroup,
    DisassociateFirewallRuleGroupResponse (DisassociateFirewallRuleGroupResponse'),
    newDisassociateFirewallRuleGroupResponse,

    -- ** DisassociateResolverEndpointIpAddress
    DisassociateResolverEndpointIpAddress (DisassociateResolverEndpointIpAddress'),
    newDisassociateResolverEndpointIpAddress,
    DisassociateResolverEndpointIpAddressResponse (DisassociateResolverEndpointIpAddressResponse'),
    newDisassociateResolverEndpointIpAddressResponse,

    -- ** DisassociateResolverQueryLogConfig
    DisassociateResolverQueryLogConfig (DisassociateResolverQueryLogConfig'),
    newDisassociateResolverQueryLogConfig,
    DisassociateResolverQueryLogConfigResponse (DisassociateResolverQueryLogConfigResponse'),
    newDisassociateResolverQueryLogConfigResponse,

    -- ** DisassociateResolverRule
    DisassociateResolverRule (DisassociateResolverRule'),
    newDisassociateResolverRule,
    DisassociateResolverRuleResponse (DisassociateResolverRuleResponse'),
    newDisassociateResolverRuleResponse,

    -- ** GetFirewallConfig
    GetFirewallConfig (GetFirewallConfig'),
    newGetFirewallConfig,
    GetFirewallConfigResponse (GetFirewallConfigResponse'),
    newGetFirewallConfigResponse,

    -- ** GetFirewallDomainList
    GetFirewallDomainList (GetFirewallDomainList'),
    newGetFirewallDomainList,
    GetFirewallDomainListResponse (GetFirewallDomainListResponse'),
    newGetFirewallDomainListResponse,

    -- ** GetFirewallRuleGroup
    GetFirewallRuleGroup (GetFirewallRuleGroup'),
    newGetFirewallRuleGroup,
    GetFirewallRuleGroupResponse (GetFirewallRuleGroupResponse'),
    newGetFirewallRuleGroupResponse,

    -- ** GetFirewallRuleGroupAssociation
    GetFirewallRuleGroupAssociation (GetFirewallRuleGroupAssociation'),
    newGetFirewallRuleGroupAssociation,
    GetFirewallRuleGroupAssociationResponse (GetFirewallRuleGroupAssociationResponse'),
    newGetFirewallRuleGroupAssociationResponse,

    -- ** GetFirewallRuleGroupPolicy
    GetFirewallRuleGroupPolicy (GetFirewallRuleGroupPolicy'),
    newGetFirewallRuleGroupPolicy,
    GetFirewallRuleGroupPolicyResponse (GetFirewallRuleGroupPolicyResponse'),
    newGetFirewallRuleGroupPolicyResponse,

    -- ** GetResolverConfig
    GetResolverConfig (GetResolverConfig'),
    newGetResolverConfig,
    GetResolverConfigResponse (GetResolverConfigResponse'),
    newGetResolverConfigResponse,

    -- ** GetResolverDnssecConfig
    GetResolverDnssecConfig (GetResolverDnssecConfig'),
    newGetResolverDnssecConfig,
    GetResolverDnssecConfigResponse (GetResolverDnssecConfigResponse'),
    newGetResolverDnssecConfigResponse,

    -- ** GetResolverEndpoint
    GetResolverEndpoint (GetResolverEndpoint'),
    newGetResolverEndpoint,
    GetResolverEndpointResponse (GetResolverEndpointResponse'),
    newGetResolverEndpointResponse,

    -- ** GetResolverQueryLogConfig
    GetResolverQueryLogConfig (GetResolverQueryLogConfig'),
    newGetResolverQueryLogConfig,
    GetResolverQueryLogConfigResponse (GetResolverQueryLogConfigResponse'),
    newGetResolverQueryLogConfigResponse,

    -- ** GetResolverQueryLogConfigAssociation
    GetResolverQueryLogConfigAssociation (GetResolverQueryLogConfigAssociation'),
    newGetResolverQueryLogConfigAssociation,
    GetResolverQueryLogConfigAssociationResponse (GetResolverQueryLogConfigAssociationResponse'),
    newGetResolverQueryLogConfigAssociationResponse,

    -- ** GetResolverQueryLogConfigPolicy
    GetResolverQueryLogConfigPolicy (GetResolverQueryLogConfigPolicy'),
    newGetResolverQueryLogConfigPolicy,
    GetResolverQueryLogConfigPolicyResponse (GetResolverQueryLogConfigPolicyResponse'),
    newGetResolverQueryLogConfigPolicyResponse,

    -- ** GetResolverRule
    GetResolverRule (GetResolverRule'),
    newGetResolverRule,
    GetResolverRuleResponse (GetResolverRuleResponse'),
    newGetResolverRuleResponse,

    -- ** GetResolverRuleAssociation
    GetResolverRuleAssociation (GetResolverRuleAssociation'),
    newGetResolverRuleAssociation,
    GetResolverRuleAssociationResponse (GetResolverRuleAssociationResponse'),
    newGetResolverRuleAssociationResponse,

    -- ** GetResolverRulePolicy
    GetResolverRulePolicy (GetResolverRulePolicy'),
    newGetResolverRulePolicy,
    GetResolverRulePolicyResponse (GetResolverRulePolicyResponse'),
    newGetResolverRulePolicyResponse,

    -- ** ImportFirewallDomains
    ImportFirewallDomains (ImportFirewallDomains'),
    newImportFirewallDomains,
    ImportFirewallDomainsResponse (ImportFirewallDomainsResponse'),
    newImportFirewallDomainsResponse,

    -- ** ListFirewallConfigs (Paginated)
    ListFirewallConfigs (ListFirewallConfigs'),
    newListFirewallConfigs,
    ListFirewallConfigsResponse (ListFirewallConfigsResponse'),
    newListFirewallConfigsResponse,

    -- ** ListFirewallDomainLists (Paginated)
    ListFirewallDomainLists (ListFirewallDomainLists'),
    newListFirewallDomainLists,
    ListFirewallDomainListsResponse (ListFirewallDomainListsResponse'),
    newListFirewallDomainListsResponse,

    -- ** ListFirewallDomains (Paginated)
    ListFirewallDomains (ListFirewallDomains'),
    newListFirewallDomains,
    ListFirewallDomainsResponse (ListFirewallDomainsResponse'),
    newListFirewallDomainsResponse,

    -- ** ListFirewallRuleGroupAssociations (Paginated)
    ListFirewallRuleGroupAssociations (ListFirewallRuleGroupAssociations'),
    newListFirewallRuleGroupAssociations,
    ListFirewallRuleGroupAssociationsResponse (ListFirewallRuleGroupAssociationsResponse'),
    newListFirewallRuleGroupAssociationsResponse,

    -- ** ListFirewallRuleGroups (Paginated)
    ListFirewallRuleGroups (ListFirewallRuleGroups'),
    newListFirewallRuleGroups,
    ListFirewallRuleGroupsResponse (ListFirewallRuleGroupsResponse'),
    newListFirewallRuleGroupsResponse,

    -- ** ListFirewallRules (Paginated)
    ListFirewallRules (ListFirewallRules'),
    newListFirewallRules,
    ListFirewallRulesResponse (ListFirewallRulesResponse'),
    newListFirewallRulesResponse,

    -- ** ListResolverConfigs (Paginated)
    ListResolverConfigs (ListResolverConfigs'),
    newListResolverConfigs,
    ListResolverConfigsResponse (ListResolverConfigsResponse'),
    newListResolverConfigsResponse,

    -- ** ListResolverDnssecConfigs (Paginated)
    ListResolverDnssecConfigs (ListResolverDnssecConfigs'),
    newListResolverDnssecConfigs,
    ListResolverDnssecConfigsResponse (ListResolverDnssecConfigsResponse'),
    newListResolverDnssecConfigsResponse,

    -- ** ListResolverEndpointIpAddresses (Paginated)
    ListResolverEndpointIpAddresses (ListResolverEndpointIpAddresses'),
    newListResolverEndpointIpAddresses,
    ListResolverEndpointIpAddressesResponse (ListResolverEndpointIpAddressesResponse'),
    newListResolverEndpointIpAddressesResponse,

    -- ** ListResolverEndpoints (Paginated)
    ListResolverEndpoints (ListResolverEndpoints'),
    newListResolverEndpoints,
    ListResolverEndpointsResponse (ListResolverEndpointsResponse'),
    newListResolverEndpointsResponse,

    -- ** ListResolverQueryLogConfigAssociations (Paginated)
    ListResolverQueryLogConfigAssociations (ListResolverQueryLogConfigAssociations'),
    newListResolverQueryLogConfigAssociations,
    ListResolverQueryLogConfigAssociationsResponse (ListResolverQueryLogConfigAssociationsResponse'),
    newListResolverQueryLogConfigAssociationsResponse,

    -- ** ListResolverQueryLogConfigs (Paginated)
    ListResolverQueryLogConfigs (ListResolverQueryLogConfigs'),
    newListResolverQueryLogConfigs,
    ListResolverQueryLogConfigsResponse (ListResolverQueryLogConfigsResponse'),
    newListResolverQueryLogConfigsResponse,

    -- ** ListResolverRuleAssociations (Paginated)
    ListResolverRuleAssociations (ListResolverRuleAssociations'),
    newListResolverRuleAssociations,
    ListResolverRuleAssociationsResponse (ListResolverRuleAssociationsResponse'),
    newListResolverRuleAssociationsResponse,

    -- ** ListResolverRules (Paginated)
    ListResolverRules (ListResolverRules'),
    newListResolverRules,
    ListResolverRulesResponse (ListResolverRulesResponse'),
    newListResolverRulesResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutFirewallRuleGroupPolicy
    PutFirewallRuleGroupPolicy (PutFirewallRuleGroupPolicy'),
    newPutFirewallRuleGroupPolicy,
    PutFirewallRuleGroupPolicyResponse (PutFirewallRuleGroupPolicyResponse'),
    newPutFirewallRuleGroupPolicyResponse,

    -- ** PutResolverQueryLogConfigPolicy
    PutResolverQueryLogConfigPolicy (PutResolverQueryLogConfigPolicy'),
    newPutResolverQueryLogConfigPolicy,
    PutResolverQueryLogConfigPolicyResponse (PutResolverQueryLogConfigPolicyResponse'),
    newPutResolverQueryLogConfigPolicyResponse,

    -- ** PutResolverRulePolicy
    PutResolverRulePolicy (PutResolverRulePolicy'),
    newPutResolverRulePolicy,
    PutResolverRulePolicyResponse (PutResolverRulePolicyResponse'),
    newPutResolverRulePolicyResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateFirewallConfig
    UpdateFirewallConfig (UpdateFirewallConfig'),
    newUpdateFirewallConfig,
    UpdateFirewallConfigResponse (UpdateFirewallConfigResponse'),
    newUpdateFirewallConfigResponse,

    -- ** UpdateFirewallDomains
    UpdateFirewallDomains (UpdateFirewallDomains'),
    newUpdateFirewallDomains,
    UpdateFirewallDomainsResponse (UpdateFirewallDomainsResponse'),
    newUpdateFirewallDomainsResponse,

    -- ** UpdateFirewallRule
    UpdateFirewallRule (UpdateFirewallRule'),
    newUpdateFirewallRule,
    UpdateFirewallRuleResponse (UpdateFirewallRuleResponse'),
    newUpdateFirewallRuleResponse,

    -- ** UpdateFirewallRuleGroupAssociation
    UpdateFirewallRuleGroupAssociation (UpdateFirewallRuleGroupAssociation'),
    newUpdateFirewallRuleGroupAssociation,
    UpdateFirewallRuleGroupAssociationResponse (UpdateFirewallRuleGroupAssociationResponse'),
    newUpdateFirewallRuleGroupAssociationResponse,

    -- ** UpdateResolverConfig
    UpdateResolverConfig (UpdateResolverConfig'),
    newUpdateResolverConfig,
    UpdateResolverConfigResponse (UpdateResolverConfigResponse'),
    newUpdateResolverConfigResponse,

    -- ** UpdateResolverDnssecConfig
    UpdateResolverDnssecConfig (UpdateResolverDnssecConfig'),
    newUpdateResolverDnssecConfig,
    UpdateResolverDnssecConfigResponse (UpdateResolverDnssecConfigResponse'),
    newUpdateResolverDnssecConfigResponse,

    -- ** UpdateResolverEndpoint
    UpdateResolverEndpoint (UpdateResolverEndpoint'),
    newUpdateResolverEndpoint,
    UpdateResolverEndpointResponse (UpdateResolverEndpointResponse'),
    newUpdateResolverEndpointResponse,

    -- ** UpdateResolverRule
    UpdateResolverRule (UpdateResolverRule'),
    newUpdateResolverRule,
    UpdateResolverRuleResponse (UpdateResolverRuleResponse'),
    newUpdateResolverRuleResponse,

    -- * Types

    -- ** Action
    Action (..),

    -- ** AutodefinedReverseFlag
    AutodefinedReverseFlag (..),

    -- ** BlockOverrideDnsType
    BlockOverrideDnsType (..),

    -- ** BlockResponse
    BlockResponse (..),

    -- ** FirewallDomainImportOperation
    FirewallDomainImportOperation (..),

    -- ** FirewallDomainListStatus
    FirewallDomainListStatus (..),

    -- ** FirewallDomainUpdateOperation
    FirewallDomainUpdateOperation (..),

    -- ** FirewallFailOpenStatus
    FirewallFailOpenStatus (..),

    -- ** FirewallRuleGroupAssociationStatus
    FirewallRuleGroupAssociationStatus (..),

    -- ** FirewallRuleGroupStatus
    FirewallRuleGroupStatus (..),

    -- ** IpAddressStatus
    IpAddressStatus (..),

    -- ** MutationProtectionStatus
    MutationProtectionStatus (..),

    -- ** ResolverAutodefinedReverseStatus
    ResolverAutodefinedReverseStatus (..),

    -- ** ResolverDNSSECValidationStatus
    ResolverDNSSECValidationStatus (..),

    -- ** ResolverEndpointDirection
    ResolverEndpointDirection (..),

    -- ** ResolverEndpointStatus
    ResolverEndpointStatus (..),

    -- ** ResolverQueryLogConfigAssociationError
    ResolverQueryLogConfigAssociationError (..),

    -- ** ResolverQueryLogConfigAssociationStatus
    ResolverQueryLogConfigAssociationStatus (..),

    -- ** ResolverQueryLogConfigStatus
    ResolverQueryLogConfigStatus (..),

    -- ** ResolverRuleAssociationStatus
    ResolverRuleAssociationStatus (..),

    -- ** ResolverRuleStatus
    ResolverRuleStatus (..),

    -- ** RuleTypeOption
    RuleTypeOption (..),

    -- ** ShareStatus
    ShareStatus (..),

    -- ** SortOrder
    SortOrder (..),

    -- ** Validation
    Validation (..),

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** FirewallConfig
    FirewallConfig (FirewallConfig'),
    newFirewallConfig,

    -- ** FirewallDomainList
    FirewallDomainList (FirewallDomainList'),
    newFirewallDomainList,

    -- ** FirewallDomainListMetadata
    FirewallDomainListMetadata (FirewallDomainListMetadata'),
    newFirewallDomainListMetadata,

    -- ** FirewallRule
    FirewallRule (FirewallRule'),
    newFirewallRule,

    -- ** FirewallRuleGroup
    FirewallRuleGroup (FirewallRuleGroup'),
    newFirewallRuleGroup,

    -- ** FirewallRuleGroupAssociation
    FirewallRuleGroupAssociation (FirewallRuleGroupAssociation'),
    newFirewallRuleGroupAssociation,

    -- ** FirewallRuleGroupMetadata
    FirewallRuleGroupMetadata (FirewallRuleGroupMetadata'),
    newFirewallRuleGroupMetadata,

    -- ** IpAddressRequest
    IpAddressRequest (IpAddressRequest'),
    newIpAddressRequest,

    -- ** IpAddressResponse
    IpAddressResponse (IpAddressResponse'),
    newIpAddressResponse,

    -- ** IpAddressUpdate
    IpAddressUpdate (IpAddressUpdate'),
    newIpAddressUpdate,

    -- ** ResolverConfig
    ResolverConfig (ResolverConfig'),
    newResolverConfig,

    -- ** ResolverDnssecConfig
    ResolverDnssecConfig (ResolverDnssecConfig'),
    newResolverDnssecConfig,

    -- ** ResolverEndpoint
    ResolverEndpoint (ResolverEndpoint'),
    newResolverEndpoint,

    -- ** ResolverQueryLogConfig
    ResolverQueryLogConfig (ResolverQueryLogConfig'),
    newResolverQueryLogConfig,

    -- ** ResolverQueryLogConfigAssociation
    ResolverQueryLogConfigAssociation (ResolverQueryLogConfigAssociation'),
    newResolverQueryLogConfigAssociation,

    -- ** ResolverRule
    ResolverRule (ResolverRule'),
    newResolverRule,

    -- ** ResolverRuleAssociation
    ResolverRuleAssociation (ResolverRuleAssociation'),
    newResolverRuleAssociation,

    -- ** ResolverRuleConfig
    ResolverRuleConfig (ResolverRuleConfig'),
    newResolverRuleConfig,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TargetAddress
    TargetAddress (TargetAddress'),
    newTargetAddress,
  )
where

import Amazonka.Route53Resolver.AssociateFirewallRuleGroup
import Amazonka.Route53Resolver.AssociateResolverEndpointIpAddress
import Amazonka.Route53Resolver.AssociateResolverQueryLogConfig
import Amazonka.Route53Resolver.AssociateResolverRule
import Amazonka.Route53Resolver.CreateFirewallDomainList
import Amazonka.Route53Resolver.CreateFirewallRule
import Amazonka.Route53Resolver.CreateFirewallRuleGroup
import Amazonka.Route53Resolver.CreateResolverEndpoint
import Amazonka.Route53Resolver.CreateResolverQueryLogConfig
import Amazonka.Route53Resolver.CreateResolverRule
import Amazonka.Route53Resolver.DeleteFirewallDomainList
import Amazonka.Route53Resolver.DeleteFirewallRule
import Amazonka.Route53Resolver.DeleteFirewallRuleGroup
import Amazonka.Route53Resolver.DeleteResolverEndpoint
import Amazonka.Route53Resolver.DeleteResolverQueryLogConfig
import Amazonka.Route53Resolver.DeleteResolverRule
import Amazonka.Route53Resolver.DisassociateFirewallRuleGroup
import Amazonka.Route53Resolver.DisassociateResolverEndpointIpAddress
import Amazonka.Route53Resolver.DisassociateResolverQueryLogConfig
import Amazonka.Route53Resolver.DisassociateResolverRule
import Amazonka.Route53Resolver.GetFirewallConfig
import Amazonka.Route53Resolver.GetFirewallDomainList
import Amazonka.Route53Resolver.GetFirewallRuleGroup
import Amazonka.Route53Resolver.GetFirewallRuleGroupAssociation
import Amazonka.Route53Resolver.GetFirewallRuleGroupPolicy
import Amazonka.Route53Resolver.GetResolverConfig
import Amazonka.Route53Resolver.GetResolverDnssecConfig
import Amazonka.Route53Resolver.GetResolverEndpoint
import Amazonka.Route53Resolver.GetResolverQueryLogConfig
import Amazonka.Route53Resolver.GetResolverQueryLogConfigAssociation
import Amazonka.Route53Resolver.GetResolverQueryLogConfigPolicy
import Amazonka.Route53Resolver.GetResolverRule
import Amazonka.Route53Resolver.GetResolverRuleAssociation
import Amazonka.Route53Resolver.GetResolverRulePolicy
import Amazonka.Route53Resolver.ImportFirewallDomains
import Amazonka.Route53Resolver.Lens
import Amazonka.Route53Resolver.ListFirewallConfigs
import Amazonka.Route53Resolver.ListFirewallDomainLists
import Amazonka.Route53Resolver.ListFirewallDomains
import Amazonka.Route53Resolver.ListFirewallRuleGroupAssociations
import Amazonka.Route53Resolver.ListFirewallRuleGroups
import Amazonka.Route53Resolver.ListFirewallRules
import Amazonka.Route53Resolver.ListResolverConfigs
import Amazonka.Route53Resolver.ListResolverDnssecConfigs
import Amazonka.Route53Resolver.ListResolverEndpointIpAddresses
import Amazonka.Route53Resolver.ListResolverEndpoints
import Amazonka.Route53Resolver.ListResolverQueryLogConfigAssociations
import Amazonka.Route53Resolver.ListResolverQueryLogConfigs
import Amazonka.Route53Resolver.ListResolverRuleAssociations
import Amazonka.Route53Resolver.ListResolverRules
import Amazonka.Route53Resolver.ListTagsForResource
import Amazonka.Route53Resolver.PutFirewallRuleGroupPolicy
import Amazonka.Route53Resolver.PutResolverQueryLogConfigPolicy
import Amazonka.Route53Resolver.PutResolverRulePolicy
import Amazonka.Route53Resolver.TagResource
import Amazonka.Route53Resolver.Types
import Amazonka.Route53Resolver.UntagResource
import Amazonka.Route53Resolver.UpdateFirewallConfig
import Amazonka.Route53Resolver.UpdateFirewallDomains
import Amazonka.Route53Resolver.UpdateFirewallRule
import Amazonka.Route53Resolver.UpdateFirewallRuleGroupAssociation
import Amazonka.Route53Resolver.UpdateResolverConfig
import Amazonka.Route53Resolver.UpdateResolverDnssecConfig
import Amazonka.Route53Resolver.UpdateResolverEndpoint
import Amazonka.Route53Resolver.UpdateResolverRule
import Amazonka.Route53Resolver.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Route53Resolver'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
