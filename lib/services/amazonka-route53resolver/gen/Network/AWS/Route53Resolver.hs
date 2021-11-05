{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.Route53Resolver
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Route53Resolver
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidTagException
    _InvalidTagException,

    -- ** ValidationException
    _ValidationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ResourceUnavailableException
    _ResourceUnavailableException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** ConflictException
    _ConflictException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** InternalServiceErrorException
    _InternalServiceErrorException,

    -- ** ResourceExistsException
    _ResourceExistsException,

    -- ** UnknownResourceException
    _UnknownResourceException,

    -- ** InvalidPolicyDocument
    _InvalidPolicyDocument,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** UpdateResolverEndpoint
    UpdateResolverEndpoint (UpdateResolverEndpoint'),
    newUpdateResolverEndpoint,
    UpdateResolverEndpointResponse (UpdateResolverEndpointResponse'),
    newUpdateResolverEndpointResponse,

    -- ** DeleteResolverEndpoint
    DeleteResolverEndpoint (DeleteResolverEndpoint'),
    newDeleteResolverEndpoint,
    DeleteResolverEndpointResponse (DeleteResolverEndpointResponse'),
    newDeleteResolverEndpointResponse,

    -- ** CreateResolverRule
    CreateResolverRule (CreateResolverRule'),
    newCreateResolverRule,
    CreateResolverRuleResponse (CreateResolverRuleResponse'),
    newCreateResolverRuleResponse,

    -- ** GetResolverQueryLogConfig
    GetResolverQueryLogConfig (GetResolverQueryLogConfig'),
    newGetResolverQueryLogConfig,
    GetResolverQueryLogConfigResponse (GetResolverQueryLogConfigResponse'),
    newGetResolverQueryLogConfigResponse,

    -- ** CreateFirewallRule
    CreateFirewallRule (CreateFirewallRule'),
    newCreateFirewallRule,
    CreateFirewallRuleResponse (CreateFirewallRuleResponse'),
    newCreateFirewallRuleResponse,

    -- ** UpdateFirewallRuleGroupAssociation
    UpdateFirewallRuleGroupAssociation (UpdateFirewallRuleGroupAssociation'),
    newUpdateFirewallRuleGroupAssociation,
    UpdateFirewallRuleGroupAssociationResponse (UpdateFirewallRuleGroupAssociationResponse'),
    newUpdateFirewallRuleGroupAssociationResponse,

    -- ** ListFirewallRuleGroupAssociations (Paginated)
    ListFirewallRuleGroupAssociations (ListFirewallRuleGroupAssociations'),
    newListFirewallRuleGroupAssociations,
    ListFirewallRuleGroupAssociationsResponse (ListFirewallRuleGroupAssociationsResponse'),
    newListFirewallRuleGroupAssociationsResponse,

    -- ** ListResolverQueryLogConfigAssociations (Paginated)
    ListResolverQueryLogConfigAssociations (ListResolverQueryLogConfigAssociations'),
    newListResolverQueryLogConfigAssociations,
    ListResolverQueryLogConfigAssociationsResponse (ListResolverQueryLogConfigAssociationsResponse'),
    newListResolverQueryLogConfigAssociationsResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** GetFirewallRuleGroupAssociation
    GetFirewallRuleGroupAssociation (GetFirewallRuleGroupAssociation'),
    newGetFirewallRuleGroupAssociation,
    GetFirewallRuleGroupAssociationResponse (GetFirewallRuleGroupAssociationResponse'),
    newGetFirewallRuleGroupAssociationResponse,

    -- ** DisassociateResolverEndpointIpAddress
    DisassociateResolverEndpointIpAddress (DisassociateResolverEndpointIpAddress'),
    newDisassociateResolverEndpointIpAddress,
    DisassociateResolverEndpointIpAddressResponse (DisassociateResolverEndpointIpAddressResponse'),
    newDisassociateResolverEndpointIpAddressResponse,

    -- ** ListResolverRuleAssociations (Paginated)
    ListResolverRuleAssociations (ListResolverRuleAssociations'),
    newListResolverRuleAssociations,
    ListResolverRuleAssociationsResponse (ListResolverRuleAssociationsResponse'),
    newListResolverRuleAssociationsResponse,

    -- ** DeleteResolverQueryLogConfig
    DeleteResolverQueryLogConfig (DeleteResolverQueryLogConfig'),
    newDeleteResolverQueryLogConfig,
    DeleteResolverQueryLogConfigResponse (DeleteResolverQueryLogConfigResponse'),
    newDeleteResolverQueryLogConfigResponse,

    -- ** CreateFirewallRuleGroup
    CreateFirewallRuleGroup (CreateFirewallRuleGroup'),
    newCreateFirewallRuleGroup,
    CreateFirewallRuleGroupResponse (CreateFirewallRuleGroupResponse'),
    newCreateFirewallRuleGroupResponse,

    -- ** GetResolverEndpoint
    GetResolverEndpoint (GetResolverEndpoint'),
    newGetResolverEndpoint,
    GetResolverEndpointResponse (GetResolverEndpointResponse'),
    newGetResolverEndpointResponse,

    -- ** ListResolverQueryLogConfigs (Paginated)
    ListResolverQueryLogConfigs (ListResolverQueryLogConfigs'),
    newListResolverQueryLogConfigs,
    ListResolverQueryLogConfigsResponse (ListResolverQueryLogConfigsResponse'),
    newListResolverQueryLogConfigsResponse,

    -- ** DeleteFirewallRuleGroup
    DeleteFirewallRuleGroup (DeleteFirewallRuleGroup'),
    newDeleteFirewallRuleGroup,
    DeleteFirewallRuleGroupResponse (DeleteFirewallRuleGroupResponse'),
    newDeleteFirewallRuleGroupResponse,

    -- ** ListResolverEndpointIpAddresses (Paginated)
    ListResolverEndpointIpAddresses (ListResolverEndpointIpAddresses'),
    newListResolverEndpointIpAddresses,
    ListResolverEndpointIpAddressesResponse (ListResolverEndpointIpAddressesResponse'),
    newListResolverEndpointIpAddressesResponse,

    -- ** AssociateResolverQueryLogConfig
    AssociateResolverQueryLogConfig (AssociateResolverQueryLogConfig'),
    newAssociateResolverQueryLogConfig,
    AssociateResolverQueryLogConfigResponse (AssociateResolverQueryLogConfigResponse'),
    newAssociateResolverQueryLogConfigResponse,

    -- ** GetResolverRulePolicy
    GetResolverRulePolicy (GetResolverRulePolicy'),
    newGetResolverRulePolicy,
    GetResolverRulePolicyResponse (GetResolverRulePolicyResponse'),
    newGetResolverRulePolicyResponse,

    -- ** GetResolverDnssecConfig
    GetResolverDnssecConfig (GetResolverDnssecConfig'),
    newGetResolverDnssecConfig,
    GetResolverDnssecConfigResponse (GetResolverDnssecConfigResponse'),
    newGetResolverDnssecConfigResponse,

    -- ** ListFirewallRuleGroups (Paginated)
    ListFirewallRuleGroups (ListFirewallRuleGroups'),
    newListFirewallRuleGroups,
    ListFirewallRuleGroupsResponse (ListFirewallRuleGroupsResponse'),
    newListFirewallRuleGroupsResponse,

    -- ** UpdateResolverRule
    UpdateResolverRule (UpdateResolverRule'),
    newUpdateResolverRule,
    UpdateResolverRuleResponse (UpdateResolverRuleResponse'),
    newUpdateResolverRuleResponse,

    -- ** DeleteResolverRule
    DeleteResolverRule (DeleteResolverRule'),
    newDeleteResolverRule,
    DeleteResolverRuleResponse (DeleteResolverRuleResponse'),
    newDeleteResolverRuleResponse,

    -- ** DeleteFirewallRule
    DeleteFirewallRule (DeleteFirewallRule'),
    newDeleteFirewallRule,
    DeleteFirewallRuleResponse (DeleteFirewallRuleResponse'),
    newDeleteFirewallRuleResponse,

    -- ** UpdateFirewallRule
    UpdateFirewallRule (UpdateFirewallRule'),
    newUpdateFirewallRule,
    UpdateFirewallRuleResponse (UpdateFirewallRuleResponse'),
    newUpdateFirewallRuleResponse,

    -- ** ListFirewallRules (Paginated)
    ListFirewallRules (ListFirewallRules'),
    newListFirewallRules,
    ListFirewallRulesResponse (ListFirewallRulesResponse'),
    newListFirewallRulesResponse,

    -- ** GetFirewallRuleGroup
    GetFirewallRuleGroup (GetFirewallRuleGroup'),
    newGetFirewallRuleGroup,
    GetFirewallRuleGroupResponse (GetFirewallRuleGroupResponse'),
    newGetFirewallRuleGroupResponse,

    -- ** ListResolverRules (Paginated)
    ListResolverRules (ListResolverRules'),
    newListResolverRules,
    ListResolverRulesResponse (ListResolverRulesResponse'),
    newListResolverRulesResponse,

    -- ** CreateResolverEndpoint
    CreateResolverEndpoint (CreateResolverEndpoint'),
    newCreateResolverEndpoint,
    CreateResolverEndpointResponse (CreateResolverEndpointResponse'),
    newCreateResolverEndpointResponse,

    -- ** AssociateResolverRule
    AssociateResolverRule (AssociateResolverRule'),
    newAssociateResolverRule,
    AssociateResolverRuleResponse (AssociateResolverRuleResponse'),
    newAssociateResolverRuleResponse,

    -- ** GetResolverQueryLogConfigPolicy
    GetResolverQueryLogConfigPolicy (GetResolverQueryLogConfigPolicy'),
    newGetResolverQueryLogConfigPolicy,
    GetResolverQueryLogConfigPolicyResponse (GetResolverQueryLogConfigPolicyResponse'),
    newGetResolverQueryLogConfigPolicyResponse,

    -- ** UpdateFirewallDomains
    UpdateFirewallDomains (UpdateFirewallDomains'),
    newUpdateFirewallDomains,
    UpdateFirewallDomainsResponse (UpdateFirewallDomainsResponse'),
    newUpdateFirewallDomainsResponse,

    -- ** ListResolverEndpoints (Paginated)
    ListResolverEndpoints (ListResolverEndpoints'),
    newListResolverEndpoints,
    ListResolverEndpointsResponse (ListResolverEndpointsResponse'),
    newListResolverEndpointsResponse,

    -- ** ListFirewallDomains (Paginated)
    ListFirewallDomains (ListFirewallDomains'),
    newListFirewallDomains,
    ListFirewallDomainsResponse (ListFirewallDomainsResponse'),
    newListFirewallDomainsResponse,

    -- ** GetResolverRuleAssociation
    GetResolverRuleAssociation (GetResolverRuleAssociation'),
    newGetResolverRuleAssociation,
    GetResolverRuleAssociationResponse (GetResolverRuleAssociationResponse'),
    newGetResolverRuleAssociationResponse,

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

    -- ** DisassociateResolverRule
    DisassociateResolverRule (DisassociateResolverRule'),
    newDisassociateResolverRule,
    DisassociateResolverRuleResponse (DisassociateResolverRuleResponse'),
    newDisassociateResolverRuleResponse,

    -- ** GetResolverQueryLogConfigAssociation
    GetResolverQueryLogConfigAssociation (GetResolverQueryLogConfigAssociation'),
    newGetResolverQueryLogConfigAssociation,
    GetResolverQueryLogConfigAssociationResponse (GetResolverQueryLogConfigAssociationResponse'),
    newGetResolverQueryLogConfigAssociationResponse,

    -- ** ListFirewallDomainLists (Paginated)
    ListFirewallDomainLists (ListFirewallDomainLists'),
    newListFirewallDomainLists,
    ListFirewallDomainListsResponse (ListFirewallDomainListsResponse'),
    newListFirewallDomainListsResponse,

    -- ** DisassociateFirewallRuleGroup
    DisassociateFirewallRuleGroup (DisassociateFirewallRuleGroup'),
    newDisassociateFirewallRuleGroup,
    DisassociateFirewallRuleGroupResponse (DisassociateFirewallRuleGroupResponse'),
    newDisassociateFirewallRuleGroupResponse,

    -- ** UpdateFirewallConfig
    UpdateFirewallConfig (UpdateFirewallConfig'),
    newUpdateFirewallConfig,
    UpdateFirewallConfigResponse (UpdateFirewallConfigResponse'),
    newUpdateFirewallConfigResponse,

    -- ** DeleteFirewallDomainList
    DeleteFirewallDomainList (DeleteFirewallDomainList'),
    newDeleteFirewallDomainList,
    DeleteFirewallDomainListResponse (DeleteFirewallDomainListResponse'),
    newDeleteFirewallDomainListResponse,

    -- ** ListFirewallConfigs (Paginated)
    ListFirewallConfigs (ListFirewallConfigs'),
    newListFirewallConfigs,
    ListFirewallConfigsResponse (ListFirewallConfigsResponse'),
    newListFirewallConfigsResponse,

    -- ** CreateFirewallDomainList
    CreateFirewallDomainList (CreateFirewallDomainList'),
    newCreateFirewallDomainList,
    CreateFirewallDomainListResponse (CreateFirewallDomainListResponse'),
    newCreateFirewallDomainListResponse,

    -- ** ImportFirewallDomains
    ImportFirewallDomains (ImportFirewallDomains'),
    newImportFirewallDomains,
    ImportFirewallDomainsResponse (ImportFirewallDomainsResponse'),
    newImportFirewallDomainsResponse,

    -- ** DisassociateResolverQueryLogConfig
    DisassociateResolverQueryLogConfig (DisassociateResolverQueryLogConfig'),
    newDisassociateResolverQueryLogConfig,
    DisassociateResolverQueryLogConfigResponse (DisassociateResolverQueryLogConfigResponse'),
    newDisassociateResolverQueryLogConfigResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** AssociateFirewallRuleGroup
    AssociateFirewallRuleGroup (AssociateFirewallRuleGroup'),
    newAssociateFirewallRuleGroup,
    AssociateFirewallRuleGroupResponse (AssociateFirewallRuleGroupResponse'),
    newAssociateFirewallRuleGroupResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** PutResolverQueryLogConfigPolicy
    PutResolverQueryLogConfigPolicy (PutResolverQueryLogConfigPolicy'),
    newPutResolverQueryLogConfigPolicy,
    PutResolverQueryLogConfigPolicyResponse (PutResolverQueryLogConfigPolicyResponse'),
    newPutResolverQueryLogConfigPolicyResponse,

    -- ** AssociateResolverEndpointIpAddress
    AssociateResolverEndpointIpAddress (AssociateResolverEndpointIpAddress'),
    newAssociateResolverEndpointIpAddress,
    AssociateResolverEndpointIpAddressResponse (AssociateResolverEndpointIpAddressResponse'),
    newAssociateResolverEndpointIpAddressResponse,

    -- ** CreateResolverQueryLogConfig
    CreateResolverQueryLogConfig (CreateResolverQueryLogConfig'),
    newCreateResolverQueryLogConfig,
    CreateResolverQueryLogConfigResponse (CreateResolverQueryLogConfigResponse'),
    newCreateResolverQueryLogConfigResponse,

    -- ** GetResolverRule
    GetResolverRule (GetResolverRule'),
    newGetResolverRule,
    GetResolverRuleResponse (GetResolverRuleResponse'),
    newGetResolverRuleResponse,

    -- ** PutFirewallRuleGroupPolicy
    PutFirewallRuleGroupPolicy (PutFirewallRuleGroupPolicy'),
    newPutFirewallRuleGroupPolicy,
    PutFirewallRuleGroupPolicyResponse (PutFirewallRuleGroupPolicyResponse'),
    newPutFirewallRuleGroupPolicyResponse,

    -- ** PutResolverRulePolicy
    PutResolverRulePolicy (PutResolverRulePolicy'),
    newPutResolverRulePolicy,
    PutResolverRulePolicyResponse (PutResolverRulePolicyResponse'),
    newPutResolverRulePolicyResponse,

    -- ** ListResolverDnssecConfigs (Paginated)
    ListResolverDnssecConfigs (ListResolverDnssecConfigs'),
    newListResolverDnssecConfigs,
    ListResolverDnssecConfigsResponse (ListResolverDnssecConfigsResponse'),
    newListResolverDnssecConfigsResponse,

    -- ** UpdateResolverDnssecConfig
    UpdateResolverDnssecConfig (UpdateResolverDnssecConfig'),
    newUpdateResolverDnssecConfig,
    UpdateResolverDnssecConfigResponse (UpdateResolverDnssecConfigResponse'),
    newUpdateResolverDnssecConfigResponse,

    -- ** GetFirewallRuleGroupPolicy
    GetFirewallRuleGroupPolicy (GetFirewallRuleGroupPolicy'),
    newGetFirewallRuleGroupPolicy,
    GetFirewallRuleGroupPolicyResponse (GetFirewallRuleGroupPolicyResponse'),
    newGetFirewallRuleGroupPolicyResponse,

    -- * Types

    -- ** Action
    Action (..),

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

import Network.AWS.Route53Resolver.AssociateFirewallRuleGroup
import Network.AWS.Route53Resolver.AssociateResolverEndpointIpAddress
import Network.AWS.Route53Resolver.AssociateResolverQueryLogConfig
import Network.AWS.Route53Resolver.AssociateResolverRule
import Network.AWS.Route53Resolver.CreateFirewallDomainList
import Network.AWS.Route53Resolver.CreateFirewallRule
import Network.AWS.Route53Resolver.CreateFirewallRuleGroup
import Network.AWS.Route53Resolver.CreateResolverEndpoint
import Network.AWS.Route53Resolver.CreateResolverQueryLogConfig
import Network.AWS.Route53Resolver.CreateResolverRule
import Network.AWS.Route53Resolver.DeleteFirewallDomainList
import Network.AWS.Route53Resolver.DeleteFirewallRule
import Network.AWS.Route53Resolver.DeleteFirewallRuleGroup
import Network.AWS.Route53Resolver.DeleteResolverEndpoint
import Network.AWS.Route53Resolver.DeleteResolverQueryLogConfig
import Network.AWS.Route53Resolver.DeleteResolverRule
import Network.AWS.Route53Resolver.DisassociateFirewallRuleGroup
import Network.AWS.Route53Resolver.DisassociateResolverEndpointIpAddress
import Network.AWS.Route53Resolver.DisassociateResolverQueryLogConfig
import Network.AWS.Route53Resolver.DisassociateResolverRule
import Network.AWS.Route53Resolver.GetFirewallConfig
import Network.AWS.Route53Resolver.GetFirewallDomainList
import Network.AWS.Route53Resolver.GetFirewallRuleGroup
import Network.AWS.Route53Resolver.GetFirewallRuleGroupAssociation
import Network.AWS.Route53Resolver.GetFirewallRuleGroupPolicy
import Network.AWS.Route53Resolver.GetResolverDnssecConfig
import Network.AWS.Route53Resolver.GetResolverEndpoint
import Network.AWS.Route53Resolver.GetResolverQueryLogConfig
import Network.AWS.Route53Resolver.GetResolverQueryLogConfigAssociation
import Network.AWS.Route53Resolver.GetResolverQueryLogConfigPolicy
import Network.AWS.Route53Resolver.GetResolverRule
import Network.AWS.Route53Resolver.GetResolverRuleAssociation
import Network.AWS.Route53Resolver.GetResolverRulePolicy
import Network.AWS.Route53Resolver.ImportFirewallDomains
import Network.AWS.Route53Resolver.Lens
import Network.AWS.Route53Resolver.ListFirewallConfigs
import Network.AWS.Route53Resolver.ListFirewallDomainLists
import Network.AWS.Route53Resolver.ListFirewallDomains
import Network.AWS.Route53Resolver.ListFirewallRuleGroupAssociations
import Network.AWS.Route53Resolver.ListFirewallRuleGroups
import Network.AWS.Route53Resolver.ListFirewallRules
import Network.AWS.Route53Resolver.ListResolverDnssecConfigs
import Network.AWS.Route53Resolver.ListResolverEndpointIpAddresses
import Network.AWS.Route53Resolver.ListResolverEndpoints
import Network.AWS.Route53Resolver.ListResolverQueryLogConfigAssociations
import Network.AWS.Route53Resolver.ListResolverQueryLogConfigs
import Network.AWS.Route53Resolver.ListResolverRuleAssociations
import Network.AWS.Route53Resolver.ListResolverRules
import Network.AWS.Route53Resolver.ListTagsForResource
import Network.AWS.Route53Resolver.PutFirewallRuleGroupPolicy
import Network.AWS.Route53Resolver.PutResolverQueryLogConfigPolicy
import Network.AWS.Route53Resolver.PutResolverRulePolicy
import Network.AWS.Route53Resolver.TagResource
import Network.AWS.Route53Resolver.Types
import Network.AWS.Route53Resolver.UntagResource
import Network.AWS.Route53Resolver.UpdateFirewallConfig
import Network.AWS.Route53Resolver.UpdateFirewallDomains
import Network.AWS.Route53Resolver.UpdateFirewallRule
import Network.AWS.Route53Resolver.UpdateFirewallRuleGroupAssociation
import Network.AWS.Route53Resolver.UpdateResolverDnssecConfig
import Network.AWS.Route53Resolver.UpdateResolverEndpoint
import Network.AWS.Route53Resolver.UpdateResolverRule
import Network.AWS.Route53Resolver.Waiters

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
