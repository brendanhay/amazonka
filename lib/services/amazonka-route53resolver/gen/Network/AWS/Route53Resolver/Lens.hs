{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Resolver.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Resolver.Lens
  ( -- * Operations

    -- ** UpdateResolverEndpoint
    updateResolverEndpoint_name,
    updateResolverEndpoint_resolverEndpointId,
    updateResolverEndpointResponse_resolverEndpoint,
    updateResolverEndpointResponse_httpStatus,

    -- ** DeleteResolverEndpoint
    deleteResolverEndpoint_resolverEndpointId,
    deleteResolverEndpointResponse_resolverEndpoint,
    deleteResolverEndpointResponse_httpStatus,

    -- ** CreateResolverRule
    createResolverRule_resolverEndpointId,
    createResolverRule_targetIps,
    createResolverRule_name,
    createResolverRule_tags,
    createResolverRule_creatorRequestId,
    createResolverRule_ruleType,
    createResolverRule_domainName,
    createResolverRuleResponse_resolverRule,
    createResolverRuleResponse_httpStatus,

    -- ** GetResolverQueryLogConfig
    getResolverQueryLogConfig_resolverQueryLogConfigId,
    getResolverQueryLogConfigResponse_resolverQueryLogConfig,
    getResolverQueryLogConfigResponse_httpStatus,

    -- ** CreateFirewallRule
    createFirewallRule_blockOverrideDnsType,
    createFirewallRule_blockResponse,
    createFirewallRule_blockOverrideTtl,
    createFirewallRule_blockOverrideDomain,
    createFirewallRule_creatorRequestId,
    createFirewallRule_firewallRuleGroupId,
    createFirewallRule_firewallDomainListId,
    createFirewallRule_priority,
    createFirewallRule_action,
    createFirewallRule_name,
    createFirewallRuleResponse_firewallRule,
    createFirewallRuleResponse_httpStatus,

    -- ** UpdateFirewallRuleGroupAssociation
    updateFirewallRuleGroupAssociation_mutationProtection,
    updateFirewallRuleGroupAssociation_priority,
    updateFirewallRuleGroupAssociation_name,
    updateFirewallRuleGroupAssociation_firewallRuleGroupAssociationId,
    updateFirewallRuleGroupAssociationResponse_firewallRuleGroupAssociation,
    updateFirewallRuleGroupAssociationResponse_httpStatus,

    -- ** ListFirewallRuleGroupAssociations
    listFirewallRuleGroupAssociations_status,
    listFirewallRuleGroupAssociations_firewallRuleGroupId,
    listFirewallRuleGroupAssociations_priority,
    listFirewallRuleGroupAssociations_vpcId,
    listFirewallRuleGroupAssociations_nextToken,
    listFirewallRuleGroupAssociations_maxResults,
    listFirewallRuleGroupAssociationsResponse_firewallRuleGroupAssociations,
    listFirewallRuleGroupAssociationsResponse_nextToken,
    listFirewallRuleGroupAssociationsResponse_httpStatus,

    -- ** ListResolverQueryLogConfigAssociations
    listResolverQueryLogConfigAssociations_filters,
    listResolverQueryLogConfigAssociations_nextToken,
    listResolverQueryLogConfigAssociations_sortOrder,
    listResolverQueryLogConfigAssociations_maxResults,
    listResolverQueryLogConfigAssociations_sortBy,
    listResolverQueryLogConfigAssociationsResponse_totalFilteredCount,
    listResolverQueryLogConfigAssociationsResponse_resolverQueryLogConfigAssociations,
    listResolverQueryLogConfigAssociationsResponse_nextToken,
    listResolverQueryLogConfigAssociationsResponse_totalCount,
    listResolverQueryLogConfigAssociationsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_maxResults,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** GetFirewallRuleGroupAssociation
    getFirewallRuleGroupAssociation_firewallRuleGroupAssociationId,
    getFirewallRuleGroupAssociationResponse_firewallRuleGroupAssociation,
    getFirewallRuleGroupAssociationResponse_httpStatus,

    -- ** DisassociateResolverEndpointIpAddress
    disassociateResolverEndpointIpAddress_resolverEndpointId,
    disassociateResolverEndpointIpAddress_ipAddress,
    disassociateResolverEndpointIpAddressResponse_resolverEndpoint,
    disassociateResolverEndpointIpAddressResponse_httpStatus,

    -- ** ListResolverRuleAssociations
    listResolverRuleAssociations_filters,
    listResolverRuleAssociations_nextToken,
    listResolverRuleAssociations_maxResults,
    listResolverRuleAssociationsResponse_resolverRuleAssociations,
    listResolverRuleAssociationsResponse_nextToken,
    listResolverRuleAssociationsResponse_maxResults,
    listResolverRuleAssociationsResponse_httpStatus,

    -- ** DeleteResolverQueryLogConfig
    deleteResolverQueryLogConfig_resolverQueryLogConfigId,
    deleteResolverQueryLogConfigResponse_resolverQueryLogConfig,
    deleteResolverQueryLogConfigResponse_httpStatus,

    -- ** CreateFirewallRuleGroup
    createFirewallRuleGroup_tags,
    createFirewallRuleGroup_creatorRequestId,
    createFirewallRuleGroup_name,
    createFirewallRuleGroupResponse_firewallRuleGroup,
    createFirewallRuleGroupResponse_httpStatus,

    -- ** GetResolverEndpoint
    getResolverEndpoint_resolverEndpointId,
    getResolverEndpointResponse_resolverEndpoint,
    getResolverEndpointResponse_httpStatus,

    -- ** ListResolverQueryLogConfigs
    listResolverQueryLogConfigs_filters,
    listResolverQueryLogConfigs_nextToken,
    listResolverQueryLogConfigs_sortOrder,
    listResolverQueryLogConfigs_maxResults,
    listResolverQueryLogConfigs_sortBy,
    listResolverQueryLogConfigsResponse_totalFilteredCount,
    listResolverQueryLogConfigsResponse_resolverQueryLogConfigs,
    listResolverQueryLogConfigsResponse_nextToken,
    listResolverQueryLogConfigsResponse_totalCount,
    listResolverQueryLogConfigsResponse_httpStatus,

    -- ** DeleteFirewallRuleGroup
    deleteFirewallRuleGroup_firewallRuleGroupId,
    deleteFirewallRuleGroupResponse_firewallRuleGroup,
    deleteFirewallRuleGroupResponse_httpStatus,

    -- ** ListResolverEndpointIpAddresses
    listResolverEndpointIpAddresses_nextToken,
    listResolverEndpointIpAddresses_maxResults,
    listResolverEndpointIpAddresses_resolverEndpointId,
    listResolverEndpointIpAddressesResponse_nextToken,
    listResolverEndpointIpAddressesResponse_maxResults,
    listResolverEndpointIpAddressesResponse_ipAddresses,
    listResolverEndpointIpAddressesResponse_httpStatus,

    -- ** AssociateResolverQueryLogConfig
    associateResolverQueryLogConfig_resolverQueryLogConfigId,
    associateResolverQueryLogConfig_resourceId,
    associateResolverQueryLogConfigResponse_resolverQueryLogConfigAssociation,
    associateResolverQueryLogConfigResponse_httpStatus,

    -- ** GetResolverRulePolicy
    getResolverRulePolicy_arn,
    getResolverRulePolicyResponse_resolverRulePolicy,
    getResolverRulePolicyResponse_httpStatus,

    -- ** GetResolverDnssecConfig
    getResolverDnssecConfig_resourceId,
    getResolverDnssecConfigResponse_resolverDNSSECConfig,
    getResolverDnssecConfigResponse_httpStatus,

    -- ** ListFirewallRuleGroups
    listFirewallRuleGroups_nextToken,
    listFirewallRuleGroups_maxResults,
    listFirewallRuleGroupsResponse_nextToken,
    listFirewallRuleGroupsResponse_firewallRuleGroups,
    listFirewallRuleGroupsResponse_httpStatus,

    -- ** UpdateResolverRule
    updateResolverRule_resolverRuleId,
    updateResolverRule_config,
    updateResolverRuleResponse_resolverRule,
    updateResolverRuleResponse_httpStatus,

    -- ** DeleteResolverRule
    deleteResolverRule_resolverRuleId,
    deleteResolverRuleResponse_resolverRule,
    deleteResolverRuleResponse_httpStatus,

    -- ** DeleteFirewallRule
    deleteFirewallRule_firewallRuleGroupId,
    deleteFirewallRule_firewallDomainListId,
    deleteFirewallRuleResponse_firewallRule,
    deleteFirewallRuleResponse_httpStatus,

    -- ** UpdateFirewallRule
    updateFirewallRule_blockOverrideDnsType,
    updateFirewallRule_priority,
    updateFirewallRule_blockResponse,
    updateFirewallRule_action,
    updateFirewallRule_blockOverrideTtl,
    updateFirewallRule_name,
    updateFirewallRule_blockOverrideDomain,
    updateFirewallRule_firewallRuleGroupId,
    updateFirewallRule_firewallDomainListId,
    updateFirewallRuleResponse_firewallRule,
    updateFirewallRuleResponse_httpStatus,

    -- ** ListFirewallRules
    listFirewallRules_priority,
    listFirewallRules_action,
    listFirewallRules_nextToken,
    listFirewallRules_maxResults,
    listFirewallRules_firewallRuleGroupId,
    listFirewallRulesResponse_firewallRules,
    listFirewallRulesResponse_nextToken,
    listFirewallRulesResponse_httpStatus,

    -- ** GetFirewallRuleGroup
    getFirewallRuleGroup_firewallRuleGroupId,
    getFirewallRuleGroupResponse_firewallRuleGroup,
    getFirewallRuleGroupResponse_httpStatus,

    -- ** ListResolverRules
    listResolverRules_filters,
    listResolverRules_nextToken,
    listResolverRules_maxResults,
    listResolverRulesResponse_resolverRules,
    listResolverRulesResponse_nextToken,
    listResolverRulesResponse_maxResults,
    listResolverRulesResponse_httpStatus,

    -- ** CreateResolverEndpoint
    createResolverEndpoint_name,
    createResolverEndpoint_tags,
    createResolverEndpoint_creatorRequestId,
    createResolverEndpoint_securityGroupIds,
    createResolverEndpoint_direction,
    createResolverEndpoint_ipAddresses,
    createResolverEndpointResponse_resolverEndpoint,
    createResolverEndpointResponse_httpStatus,

    -- ** AssociateResolverRule
    associateResolverRule_name,
    associateResolverRule_resolverRuleId,
    associateResolverRule_vPCId,
    associateResolverRuleResponse_resolverRuleAssociation,
    associateResolverRuleResponse_httpStatus,

    -- ** GetResolverQueryLogConfigPolicy
    getResolverQueryLogConfigPolicy_arn,
    getResolverQueryLogConfigPolicyResponse_resolverQueryLogConfigPolicy,
    getResolverQueryLogConfigPolicyResponse_httpStatus,

    -- ** UpdateFirewallDomains
    updateFirewallDomains_firewallDomainListId,
    updateFirewallDomains_operation,
    updateFirewallDomains_domains,
    updateFirewallDomainsResponse_status,
    updateFirewallDomainsResponse_statusMessage,
    updateFirewallDomainsResponse_name,
    updateFirewallDomainsResponse_id,
    updateFirewallDomainsResponse_httpStatus,

    -- ** ListResolverEndpoints
    listResolverEndpoints_filters,
    listResolverEndpoints_nextToken,
    listResolverEndpoints_maxResults,
    listResolverEndpointsResponse_resolverEndpoints,
    listResolverEndpointsResponse_nextToken,
    listResolverEndpointsResponse_maxResults,
    listResolverEndpointsResponse_httpStatus,

    -- ** ListFirewallDomains
    listFirewallDomains_nextToken,
    listFirewallDomains_maxResults,
    listFirewallDomains_firewallDomainListId,
    listFirewallDomainsResponse_nextToken,
    listFirewallDomainsResponse_domains,
    listFirewallDomainsResponse_httpStatus,

    -- ** GetResolverRuleAssociation
    getResolverRuleAssociation_resolverRuleAssociationId,
    getResolverRuleAssociationResponse_resolverRuleAssociation,
    getResolverRuleAssociationResponse_httpStatus,

    -- ** GetFirewallConfig
    getFirewallConfig_resourceId,
    getFirewallConfigResponse_firewallConfig,
    getFirewallConfigResponse_httpStatus,

    -- ** GetFirewallDomainList
    getFirewallDomainList_firewallDomainListId,
    getFirewallDomainListResponse_firewallDomainList,
    getFirewallDomainListResponse_httpStatus,

    -- ** DisassociateResolverRule
    disassociateResolverRule_vPCId,
    disassociateResolverRule_resolverRuleId,
    disassociateResolverRuleResponse_resolverRuleAssociation,
    disassociateResolverRuleResponse_httpStatus,

    -- ** GetResolverQueryLogConfigAssociation
    getResolverQueryLogConfigAssociation_resolverQueryLogConfigAssociationId,
    getResolverQueryLogConfigAssociationResponse_resolverQueryLogConfigAssociation,
    getResolverQueryLogConfigAssociationResponse_httpStatus,

    -- ** ListFirewallDomainLists
    listFirewallDomainLists_nextToken,
    listFirewallDomainLists_maxResults,
    listFirewallDomainListsResponse_nextToken,
    listFirewallDomainListsResponse_firewallDomainLists,
    listFirewallDomainListsResponse_httpStatus,

    -- ** DisassociateFirewallRuleGroup
    disassociateFirewallRuleGroup_firewallRuleGroupAssociationId,
    disassociateFirewallRuleGroupResponse_firewallRuleGroupAssociation,
    disassociateFirewallRuleGroupResponse_httpStatus,

    -- ** UpdateFirewallConfig
    updateFirewallConfig_resourceId,
    updateFirewallConfig_firewallFailOpen,
    updateFirewallConfigResponse_firewallConfig,
    updateFirewallConfigResponse_httpStatus,

    -- ** DeleteFirewallDomainList
    deleteFirewallDomainList_firewallDomainListId,
    deleteFirewallDomainListResponse_firewallDomainList,
    deleteFirewallDomainListResponse_httpStatus,

    -- ** ListFirewallConfigs
    listFirewallConfigs_nextToken,
    listFirewallConfigs_maxResults,
    listFirewallConfigsResponse_nextToken,
    listFirewallConfigsResponse_firewallConfigs,
    listFirewallConfigsResponse_httpStatus,

    -- ** CreateFirewallDomainList
    createFirewallDomainList_tags,
    createFirewallDomainList_creatorRequestId,
    createFirewallDomainList_name,
    createFirewallDomainListResponse_firewallDomainList,
    createFirewallDomainListResponse_httpStatus,

    -- ** ImportFirewallDomains
    importFirewallDomains_firewallDomainListId,
    importFirewallDomains_operation,
    importFirewallDomains_domainFileUrl,
    importFirewallDomainsResponse_status,
    importFirewallDomainsResponse_statusMessage,
    importFirewallDomainsResponse_name,
    importFirewallDomainsResponse_id,
    importFirewallDomainsResponse_httpStatus,

    -- ** DisassociateResolverQueryLogConfig
    disassociateResolverQueryLogConfig_resolverQueryLogConfigId,
    disassociateResolverQueryLogConfig_resourceId,
    disassociateResolverQueryLogConfigResponse_resolverQueryLogConfigAssociation,
    disassociateResolverQueryLogConfigResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** AssociateFirewallRuleGroup
    associateFirewallRuleGroup_mutationProtection,
    associateFirewallRuleGroup_tags,
    associateFirewallRuleGroup_creatorRequestId,
    associateFirewallRuleGroup_firewallRuleGroupId,
    associateFirewallRuleGroup_vpcId,
    associateFirewallRuleGroup_priority,
    associateFirewallRuleGroup_name,
    associateFirewallRuleGroupResponse_firewallRuleGroupAssociation,
    associateFirewallRuleGroupResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** PutResolverQueryLogConfigPolicy
    putResolverQueryLogConfigPolicy_arn,
    putResolverQueryLogConfigPolicy_resolverQueryLogConfigPolicy,
    putResolverQueryLogConfigPolicyResponse_returnValue,
    putResolverQueryLogConfigPolicyResponse_httpStatus,

    -- ** AssociateResolverEndpointIpAddress
    associateResolverEndpointIpAddress_resolverEndpointId,
    associateResolverEndpointIpAddress_ipAddress,
    associateResolverEndpointIpAddressResponse_resolverEndpoint,
    associateResolverEndpointIpAddressResponse_httpStatus,

    -- ** CreateResolverQueryLogConfig
    createResolverQueryLogConfig_tags,
    createResolverQueryLogConfig_name,
    createResolverQueryLogConfig_destinationArn,
    createResolverQueryLogConfig_creatorRequestId,
    createResolverQueryLogConfigResponse_resolverQueryLogConfig,
    createResolverQueryLogConfigResponse_httpStatus,

    -- ** GetResolverRule
    getResolverRule_resolverRuleId,
    getResolverRuleResponse_resolverRule,
    getResolverRuleResponse_httpStatus,

    -- ** PutFirewallRuleGroupPolicy
    putFirewallRuleGroupPolicy_arn,
    putFirewallRuleGroupPolicy_firewallRuleGroupPolicy,
    putFirewallRuleGroupPolicyResponse_returnValue,
    putFirewallRuleGroupPolicyResponse_httpStatus,

    -- ** PutResolverRulePolicy
    putResolverRulePolicy_arn,
    putResolverRulePolicy_resolverRulePolicy,
    putResolverRulePolicyResponse_returnValue,
    putResolverRulePolicyResponse_httpStatus,

    -- ** ListResolverDnssecConfigs
    listResolverDnssecConfigs_filters,
    listResolverDnssecConfigs_nextToken,
    listResolverDnssecConfigs_maxResults,
    listResolverDnssecConfigsResponse_nextToken,
    listResolverDnssecConfigsResponse_resolverDnssecConfigs,
    listResolverDnssecConfigsResponse_httpStatus,

    -- ** UpdateResolverDnssecConfig
    updateResolverDnssecConfig_resourceId,
    updateResolverDnssecConfig_validation,
    updateResolverDnssecConfigResponse_resolverDNSSECConfig,
    updateResolverDnssecConfigResponse_httpStatus,

    -- ** GetFirewallRuleGroupPolicy
    getFirewallRuleGroupPolicy_arn,
    getFirewallRuleGroupPolicyResponse_firewallRuleGroupPolicy,
    getFirewallRuleGroupPolicyResponse_httpStatus,

    -- * Types

    -- ** Filter
    filter_values,
    filter_name,

    -- ** FirewallConfig
    firewallConfig_resourceId,
    firewallConfig_ownerId,
    firewallConfig_id,
    firewallConfig_firewallFailOpen,

    -- ** FirewallDomainList
    firewallDomainList_creationTime,
    firewallDomainList_status,
    firewallDomainList_arn,
    firewallDomainList_creatorRequestId,
    firewallDomainList_managedOwnerName,
    firewallDomainList_domainCount,
    firewallDomainList_modificationTime,
    firewallDomainList_statusMessage,
    firewallDomainList_name,
    firewallDomainList_id,

    -- ** FirewallDomainListMetadata
    firewallDomainListMetadata_arn,
    firewallDomainListMetadata_creatorRequestId,
    firewallDomainListMetadata_managedOwnerName,
    firewallDomainListMetadata_name,
    firewallDomainListMetadata_id,

    -- ** FirewallRule
    firewallRule_creationTime,
    firewallRule_blockOverrideDnsType,
    firewallRule_firewallRuleGroupId,
    firewallRule_priority,
    firewallRule_blockResponse,
    firewallRule_creatorRequestId,
    firewallRule_modificationTime,
    firewallRule_action,
    firewallRule_blockOverrideTtl,
    firewallRule_name,
    firewallRule_blockOverrideDomain,
    firewallRule_firewallDomainListId,

    -- ** FirewallRuleGroup
    firewallRuleGroup_creationTime,
    firewallRuleGroup_status,
    firewallRuleGroup_arn,
    firewallRuleGroup_creatorRequestId,
    firewallRuleGroup_modificationTime,
    firewallRuleGroup_shareStatus,
    firewallRuleGroup_ownerId,
    firewallRuleGroup_statusMessage,
    firewallRuleGroup_name,
    firewallRuleGroup_id,
    firewallRuleGroup_ruleCount,

    -- ** FirewallRuleGroupAssociation
    firewallRuleGroupAssociation_creationTime,
    firewallRuleGroupAssociation_status,
    firewallRuleGroupAssociation_mutationProtection,
    firewallRuleGroupAssociation_firewallRuleGroupId,
    firewallRuleGroupAssociation_priority,
    firewallRuleGroupAssociation_arn,
    firewallRuleGroupAssociation_vpcId,
    firewallRuleGroupAssociation_creatorRequestId,
    firewallRuleGroupAssociation_managedOwnerName,
    firewallRuleGroupAssociation_modificationTime,
    firewallRuleGroupAssociation_statusMessage,
    firewallRuleGroupAssociation_name,
    firewallRuleGroupAssociation_id,

    -- ** FirewallRuleGroupMetadata
    firewallRuleGroupMetadata_arn,
    firewallRuleGroupMetadata_creatorRequestId,
    firewallRuleGroupMetadata_shareStatus,
    firewallRuleGroupMetadata_ownerId,
    firewallRuleGroupMetadata_name,
    firewallRuleGroupMetadata_id,

    -- ** IpAddressRequest
    ipAddressRequest_ip,
    ipAddressRequest_subnetId,

    -- ** IpAddressResponse
    ipAddressResponse_creationTime,
    ipAddressResponse_status,
    ipAddressResponse_modificationTime,
    ipAddressResponse_subnetId,
    ipAddressResponse_ip,
    ipAddressResponse_ipId,
    ipAddressResponse_statusMessage,

    -- ** IpAddressUpdate
    ipAddressUpdate_subnetId,
    ipAddressUpdate_ip,
    ipAddressUpdate_ipId,

    -- ** ResolverDnssecConfig
    resolverDnssecConfig_resourceId,
    resolverDnssecConfig_ownerId,
    resolverDnssecConfig_validationStatus,
    resolverDnssecConfig_id,

    -- ** ResolverEndpoint
    resolverEndpoint_creationTime,
    resolverEndpoint_status,
    resolverEndpoint_securityGroupIds,
    resolverEndpoint_direction,
    resolverEndpoint_arn,
    resolverEndpoint_creatorRequestId,
    resolverEndpoint_modificationTime,
    resolverEndpoint_ipAddressCount,
    resolverEndpoint_statusMessage,
    resolverEndpoint_name,
    resolverEndpoint_id,
    resolverEndpoint_hostVPCId,

    -- ** ResolverQueryLogConfig
    resolverQueryLogConfig_creationTime,
    resolverQueryLogConfig_status,
    resolverQueryLogConfig_associationCount,
    resolverQueryLogConfig_arn,
    resolverQueryLogConfig_creatorRequestId,
    resolverQueryLogConfig_destinationArn,
    resolverQueryLogConfig_shareStatus,
    resolverQueryLogConfig_ownerId,
    resolverQueryLogConfig_name,
    resolverQueryLogConfig_id,

    -- ** ResolverQueryLogConfigAssociation
    resolverQueryLogConfigAssociation_creationTime,
    resolverQueryLogConfigAssociation_status,
    resolverQueryLogConfigAssociation_resolverQueryLogConfigId,
    resolverQueryLogConfigAssociation_resourceId,
    resolverQueryLogConfigAssociation_error,
    resolverQueryLogConfigAssociation_id,
    resolverQueryLogConfigAssociation_errorMessage,

    -- ** ResolverRule
    resolverRule_creationTime,
    resolverRule_status,
    resolverRule_arn,
    resolverRule_resolverEndpointId,
    resolverRule_creatorRequestId,
    resolverRule_targetIps,
    resolverRule_modificationTime,
    resolverRule_shareStatus,
    resolverRule_ownerId,
    resolverRule_domainName,
    resolverRule_statusMessage,
    resolverRule_name,
    resolverRule_id,
    resolverRule_ruleType,

    -- ** ResolverRuleAssociation
    resolverRuleAssociation_status,
    resolverRuleAssociation_resolverRuleId,
    resolverRuleAssociation_vPCId,
    resolverRuleAssociation_statusMessage,
    resolverRuleAssociation_name,
    resolverRuleAssociation_id,

    -- ** ResolverRuleConfig
    resolverRuleConfig_resolverEndpointId,
    resolverRuleConfig_targetIps,
    resolverRuleConfig_name,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TargetAddress
    targetAddress_port,
    targetAddress_ip,
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
import Network.AWS.Route53Resolver.Types.Filter
import Network.AWS.Route53Resolver.Types.FirewallConfig
import Network.AWS.Route53Resolver.Types.FirewallDomainList
import Network.AWS.Route53Resolver.Types.FirewallDomainListMetadata
import Network.AWS.Route53Resolver.Types.FirewallRule
import Network.AWS.Route53Resolver.Types.FirewallRuleGroup
import Network.AWS.Route53Resolver.Types.FirewallRuleGroupAssociation
import Network.AWS.Route53Resolver.Types.FirewallRuleGroupMetadata
import Network.AWS.Route53Resolver.Types.IpAddressRequest
import Network.AWS.Route53Resolver.Types.IpAddressResponse
import Network.AWS.Route53Resolver.Types.IpAddressUpdate
import Network.AWS.Route53Resolver.Types.ResolverDnssecConfig
import Network.AWS.Route53Resolver.Types.ResolverEndpoint
import Network.AWS.Route53Resolver.Types.ResolverQueryLogConfig
import Network.AWS.Route53Resolver.Types.ResolverQueryLogConfigAssociation
import Network.AWS.Route53Resolver.Types.ResolverRule
import Network.AWS.Route53Resolver.Types.ResolverRuleAssociation
import Network.AWS.Route53Resolver.Types.ResolverRuleConfig
import Network.AWS.Route53Resolver.Types.Tag
import Network.AWS.Route53Resolver.Types.TargetAddress
import Network.AWS.Route53Resolver.UntagResource
import Network.AWS.Route53Resolver.UpdateFirewallConfig
import Network.AWS.Route53Resolver.UpdateFirewallDomains
import Network.AWS.Route53Resolver.UpdateFirewallRule
import Network.AWS.Route53Resolver.UpdateFirewallRuleGroupAssociation
import Network.AWS.Route53Resolver.UpdateResolverDnssecConfig
import Network.AWS.Route53Resolver.UpdateResolverEndpoint
import Network.AWS.Route53Resolver.UpdateResolverRule
