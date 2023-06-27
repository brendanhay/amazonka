{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.VPCLattice.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Lens
  ( -- * Operations

    -- ** BatchUpdateRule
    batchUpdateRule_listenerIdentifier,
    batchUpdateRule_rules,
    batchUpdateRule_serviceIdentifier,
    batchUpdateRuleResponse_successful,
    batchUpdateRuleResponse_unsuccessful,
    batchUpdateRuleResponse_httpStatus,

    -- ** CreateAccessLogSubscription
    createAccessLogSubscription_clientToken,
    createAccessLogSubscription_tags,
    createAccessLogSubscription_destinationArn,
    createAccessLogSubscription_resourceIdentifier,
    createAccessLogSubscriptionResponse_httpStatus,
    createAccessLogSubscriptionResponse_arn,
    createAccessLogSubscriptionResponse_destinationArn,
    createAccessLogSubscriptionResponse_id,
    createAccessLogSubscriptionResponse_resourceArn,
    createAccessLogSubscriptionResponse_resourceId,

    -- ** CreateListener
    createListener_clientToken,
    createListener_port,
    createListener_tags,
    createListener_defaultAction,
    createListener_name,
    createListener_protocol,
    createListener_serviceIdentifier,
    createListenerResponse_arn,
    createListenerResponse_defaultAction,
    createListenerResponse_id,
    createListenerResponse_name,
    createListenerResponse_port,
    createListenerResponse_protocol,
    createListenerResponse_serviceArn,
    createListenerResponse_serviceId,
    createListenerResponse_httpStatus,

    -- ** CreateRule
    createRule_clientToken,
    createRule_tags,
    createRule_action,
    createRule_listenerIdentifier,
    createRule_match,
    createRule_name,
    createRule_priority,
    createRule_serviceIdentifier,
    createRuleResponse_action,
    createRuleResponse_arn,
    createRuleResponse_id,
    createRuleResponse_match,
    createRuleResponse_name,
    createRuleResponse_priority,
    createRuleResponse_httpStatus,

    -- ** CreateService
    createService_authType,
    createService_certificateArn,
    createService_clientToken,
    createService_customDomainName,
    createService_tags,
    createService_name,
    createServiceResponse_arn,
    createServiceResponse_authType,
    createServiceResponse_certificateArn,
    createServiceResponse_customDomainName,
    createServiceResponse_dnsEntry,
    createServiceResponse_id,
    createServiceResponse_name,
    createServiceResponse_status,
    createServiceResponse_httpStatus,

    -- ** CreateServiceNetwork
    createServiceNetwork_authType,
    createServiceNetwork_clientToken,
    createServiceNetwork_tags,
    createServiceNetwork_name,
    createServiceNetworkResponse_arn,
    createServiceNetworkResponse_authType,
    createServiceNetworkResponse_id,
    createServiceNetworkResponse_name,
    createServiceNetworkResponse_httpStatus,

    -- ** CreateServiceNetworkServiceAssociation
    createServiceNetworkServiceAssociation_clientToken,
    createServiceNetworkServiceAssociation_tags,
    createServiceNetworkServiceAssociation_serviceIdentifier,
    createServiceNetworkServiceAssociation_serviceNetworkIdentifier,
    createServiceNetworkServiceAssociationResponse_arn,
    createServiceNetworkServiceAssociationResponse_createdBy,
    createServiceNetworkServiceAssociationResponse_customDomainName,
    createServiceNetworkServiceAssociationResponse_dnsEntry,
    createServiceNetworkServiceAssociationResponse_id,
    createServiceNetworkServiceAssociationResponse_status,
    createServiceNetworkServiceAssociationResponse_httpStatus,

    -- ** CreateServiceNetworkVpcAssociation
    createServiceNetworkVpcAssociation_clientToken,
    createServiceNetworkVpcAssociation_securityGroupIds,
    createServiceNetworkVpcAssociation_tags,
    createServiceNetworkVpcAssociation_serviceNetworkIdentifier,
    createServiceNetworkVpcAssociation_vpcIdentifier,
    createServiceNetworkVpcAssociationResponse_arn,
    createServiceNetworkVpcAssociationResponse_createdBy,
    createServiceNetworkVpcAssociationResponse_id,
    createServiceNetworkVpcAssociationResponse_securityGroupIds,
    createServiceNetworkVpcAssociationResponse_status,
    createServiceNetworkVpcAssociationResponse_httpStatus,

    -- ** CreateTargetGroup
    createTargetGroup_clientToken,
    createTargetGroup_config,
    createTargetGroup_tags,
    createTargetGroup_name,
    createTargetGroup_type,
    createTargetGroupResponse_arn,
    createTargetGroupResponse_config,
    createTargetGroupResponse_id,
    createTargetGroupResponse_name,
    createTargetGroupResponse_status,
    createTargetGroupResponse_type,
    createTargetGroupResponse_httpStatus,

    -- ** DeleteAccessLogSubscription
    deleteAccessLogSubscription_accessLogSubscriptionIdentifier,
    deleteAccessLogSubscriptionResponse_httpStatus,

    -- ** DeleteAuthPolicy
    deleteAuthPolicy_resourceIdentifier,
    deleteAuthPolicyResponse_httpStatus,

    -- ** DeleteListener
    deleteListener_listenerIdentifier,
    deleteListener_serviceIdentifier,
    deleteListenerResponse_httpStatus,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_resourceArn,
    deleteResourcePolicyResponse_httpStatus,

    -- ** DeleteRule
    deleteRule_listenerIdentifier,
    deleteRule_ruleIdentifier,
    deleteRule_serviceIdentifier,
    deleteRuleResponse_httpStatus,

    -- ** DeleteService
    deleteService_serviceIdentifier,
    deleteServiceResponse_arn,
    deleteServiceResponse_id,
    deleteServiceResponse_name,
    deleteServiceResponse_status,
    deleteServiceResponse_httpStatus,

    -- ** DeleteServiceNetwork
    deleteServiceNetwork_serviceNetworkIdentifier,
    deleteServiceNetworkResponse_httpStatus,

    -- ** DeleteServiceNetworkServiceAssociation
    deleteServiceNetworkServiceAssociation_serviceNetworkServiceAssociationIdentifier,
    deleteServiceNetworkServiceAssociationResponse_arn,
    deleteServiceNetworkServiceAssociationResponse_id,
    deleteServiceNetworkServiceAssociationResponse_status,
    deleteServiceNetworkServiceAssociationResponse_httpStatus,

    -- ** DeleteServiceNetworkVpcAssociation
    deleteServiceNetworkVpcAssociation_serviceNetworkVpcAssociationIdentifier,
    deleteServiceNetworkVpcAssociationResponse_arn,
    deleteServiceNetworkVpcAssociationResponse_id,
    deleteServiceNetworkVpcAssociationResponse_status,
    deleteServiceNetworkVpcAssociationResponse_httpStatus,

    -- ** DeleteTargetGroup
    deleteTargetGroup_targetGroupIdentifier,
    deleteTargetGroupResponse_arn,
    deleteTargetGroupResponse_id,
    deleteTargetGroupResponse_status,
    deleteTargetGroupResponse_httpStatus,

    -- ** DeregisterTargets
    deregisterTargets_targetGroupIdentifier,
    deregisterTargets_targets,
    deregisterTargetsResponse_successful,
    deregisterTargetsResponse_unsuccessful,
    deregisterTargetsResponse_httpStatus,

    -- ** GetAccessLogSubscription
    getAccessLogSubscription_accessLogSubscriptionIdentifier,
    getAccessLogSubscriptionResponse_httpStatus,
    getAccessLogSubscriptionResponse_arn,
    getAccessLogSubscriptionResponse_createdAt,
    getAccessLogSubscriptionResponse_destinationArn,
    getAccessLogSubscriptionResponse_id,
    getAccessLogSubscriptionResponse_lastUpdatedAt,
    getAccessLogSubscriptionResponse_resourceArn,
    getAccessLogSubscriptionResponse_resourceId,

    -- ** GetAuthPolicy
    getAuthPolicy_resourceIdentifier,
    getAuthPolicyResponse_createdAt,
    getAuthPolicyResponse_lastUpdatedAt,
    getAuthPolicyResponse_policy,
    getAuthPolicyResponse_state,
    getAuthPolicyResponse_httpStatus,

    -- ** GetListener
    getListener_listenerIdentifier,
    getListener_serviceIdentifier,
    getListenerResponse_arn,
    getListenerResponse_createdAt,
    getListenerResponse_defaultAction,
    getListenerResponse_id,
    getListenerResponse_lastUpdatedAt,
    getListenerResponse_name,
    getListenerResponse_port,
    getListenerResponse_protocol,
    getListenerResponse_serviceArn,
    getListenerResponse_serviceId,
    getListenerResponse_httpStatus,

    -- ** GetResourcePolicy
    getResourcePolicy_resourceArn,
    getResourcePolicyResponse_policy,
    getResourcePolicyResponse_httpStatus,

    -- ** GetRule
    getRule_listenerIdentifier,
    getRule_ruleIdentifier,
    getRule_serviceIdentifier,
    getRuleResponse_action,
    getRuleResponse_arn,
    getRuleResponse_createdAt,
    getRuleResponse_id,
    getRuleResponse_isDefault,
    getRuleResponse_lastUpdatedAt,
    getRuleResponse_match,
    getRuleResponse_name,
    getRuleResponse_priority,
    getRuleResponse_httpStatus,

    -- ** GetService
    getService_serviceIdentifier,
    getServiceResponse_arn,
    getServiceResponse_authType,
    getServiceResponse_certificateArn,
    getServiceResponse_createdAt,
    getServiceResponse_customDomainName,
    getServiceResponse_dnsEntry,
    getServiceResponse_failureCode,
    getServiceResponse_failureMessage,
    getServiceResponse_id,
    getServiceResponse_lastUpdatedAt,
    getServiceResponse_name,
    getServiceResponse_status,
    getServiceResponse_httpStatus,

    -- ** GetServiceNetwork
    getServiceNetwork_serviceNetworkIdentifier,
    getServiceNetworkResponse_arn,
    getServiceNetworkResponse_authType,
    getServiceNetworkResponse_createdAt,
    getServiceNetworkResponse_id,
    getServiceNetworkResponse_lastUpdatedAt,
    getServiceNetworkResponse_name,
    getServiceNetworkResponse_numberOfAssociatedServices,
    getServiceNetworkResponse_numberOfAssociatedVPCs,
    getServiceNetworkResponse_httpStatus,

    -- ** GetServiceNetworkServiceAssociation
    getServiceNetworkServiceAssociation_serviceNetworkServiceAssociationIdentifier,
    getServiceNetworkServiceAssociationResponse_arn,
    getServiceNetworkServiceAssociationResponse_createdAt,
    getServiceNetworkServiceAssociationResponse_createdBy,
    getServiceNetworkServiceAssociationResponse_customDomainName,
    getServiceNetworkServiceAssociationResponse_dnsEntry,
    getServiceNetworkServiceAssociationResponse_failureCode,
    getServiceNetworkServiceAssociationResponse_failureMessage,
    getServiceNetworkServiceAssociationResponse_id,
    getServiceNetworkServiceAssociationResponse_serviceArn,
    getServiceNetworkServiceAssociationResponse_serviceId,
    getServiceNetworkServiceAssociationResponse_serviceName,
    getServiceNetworkServiceAssociationResponse_serviceNetworkArn,
    getServiceNetworkServiceAssociationResponse_serviceNetworkId,
    getServiceNetworkServiceAssociationResponse_serviceNetworkName,
    getServiceNetworkServiceAssociationResponse_status,
    getServiceNetworkServiceAssociationResponse_httpStatus,

    -- ** GetServiceNetworkVpcAssociation
    getServiceNetworkVpcAssociation_serviceNetworkVpcAssociationIdentifier,
    getServiceNetworkVpcAssociationResponse_arn,
    getServiceNetworkVpcAssociationResponse_createdAt,
    getServiceNetworkVpcAssociationResponse_createdBy,
    getServiceNetworkVpcAssociationResponse_failureCode,
    getServiceNetworkVpcAssociationResponse_failureMessage,
    getServiceNetworkVpcAssociationResponse_id,
    getServiceNetworkVpcAssociationResponse_lastUpdatedAt,
    getServiceNetworkVpcAssociationResponse_securityGroupIds,
    getServiceNetworkVpcAssociationResponse_serviceNetworkArn,
    getServiceNetworkVpcAssociationResponse_serviceNetworkId,
    getServiceNetworkVpcAssociationResponse_serviceNetworkName,
    getServiceNetworkVpcAssociationResponse_status,
    getServiceNetworkVpcAssociationResponse_vpcId,
    getServiceNetworkVpcAssociationResponse_httpStatus,

    -- ** GetTargetGroup
    getTargetGroup_targetGroupIdentifier,
    getTargetGroupResponse_arn,
    getTargetGroupResponse_config,
    getTargetGroupResponse_createdAt,
    getTargetGroupResponse_failureCode,
    getTargetGroupResponse_failureMessage,
    getTargetGroupResponse_id,
    getTargetGroupResponse_lastUpdatedAt,
    getTargetGroupResponse_name,
    getTargetGroupResponse_serviceArns,
    getTargetGroupResponse_status,
    getTargetGroupResponse_type,
    getTargetGroupResponse_httpStatus,

    -- ** ListAccessLogSubscriptions
    listAccessLogSubscriptions_maxResults,
    listAccessLogSubscriptions_nextToken,
    listAccessLogSubscriptions_resourceIdentifier,
    listAccessLogSubscriptionsResponse_nextToken,
    listAccessLogSubscriptionsResponse_httpStatus,
    listAccessLogSubscriptionsResponse_items,

    -- ** ListListeners
    listListeners_maxResults,
    listListeners_nextToken,
    listListeners_serviceIdentifier,
    listListenersResponse_nextToken,
    listListenersResponse_httpStatus,
    listListenersResponse_items,

    -- ** ListRules
    listRules_maxResults,
    listRules_nextToken,
    listRules_listenerIdentifier,
    listRules_serviceIdentifier,
    listRulesResponse_nextToken,
    listRulesResponse_httpStatus,
    listRulesResponse_items,

    -- ** ListServiceNetworkServiceAssociations
    listServiceNetworkServiceAssociations_maxResults,
    listServiceNetworkServiceAssociations_nextToken,
    listServiceNetworkServiceAssociations_serviceIdentifier,
    listServiceNetworkServiceAssociations_serviceNetworkIdentifier,
    listServiceNetworkServiceAssociationsResponse_nextToken,
    listServiceNetworkServiceAssociationsResponse_httpStatus,
    listServiceNetworkServiceAssociationsResponse_items,

    -- ** ListServiceNetworkVpcAssociations
    listServiceNetworkVpcAssociations_maxResults,
    listServiceNetworkVpcAssociations_nextToken,
    listServiceNetworkVpcAssociations_serviceNetworkIdentifier,
    listServiceNetworkVpcAssociations_vpcIdentifier,
    listServiceNetworkVpcAssociationsResponse_nextToken,
    listServiceNetworkVpcAssociationsResponse_httpStatus,
    listServiceNetworkVpcAssociationsResponse_items,

    -- ** ListServiceNetworks
    listServiceNetworks_maxResults,
    listServiceNetworks_nextToken,
    listServiceNetworksResponse_nextToken,
    listServiceNetworksResponse_httpStatus,
    listServiceNetworksResponse_items,

    -- ** ListServices
    listServices_maxResults,
    listServices_nextToken,
    listServicesResponse_items,
    listServicesResponse_nextToken,
    listServicesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTargetGroups
    listTargetGroups_maxResults,
    listTargetGroups_nextToken,
    listTargetGroups_targetGroupType,
    listTargetGroups_vpcIdentifier,
    listTargetGroupsResponse_items,
    listTargetGroupsResponse_nextToken,
    listTargetGroupsResponse_httpStatus,

    -- ** ListTargets
    listTargets_maxResults,
    listTargets_nextToken,
    listTargets_targets,
    listTargets_targetGroupIdentifier,
    listTargetsResponse_nextToken,
    listTargetsResponse_httpStatus,
    listTargetsResponse_items,

    -- ** PutAuthPolicy
    putAuthPolicy_policy,
    putAuthPolicy_resourceIdentifier,
    putAuthPolicyResponse_policy,
    putAuthPolicyResponse_state,
    putAuthPolicyResponse_httpStatus,

    -- ** PutResourcePolicy
    putResourcePolicy_policy,
    putResourcePolicy_resourceArn,
    putResourcePolicyResponse_httpStatus,

    -- ** RegisterTargets
    registerTargets_targetGroupIdentifier,
    registerTargets_targets,
    registerTargetsResponse_successful,
    registerTargetsResponse_unsuccessful,
    registerTargetsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateAccessLogSubscription
    updateAccessLogSubscription_accessLogSubscriptionIdentifier,
    updateAccessLogSubscription_destinationArn,
    updateAccessLogSubscriptionResponse_httpStatus,
    updateAccessLogSubscriptionResponse_arn,
    updateAccessLogSubscriptionResponse_destinationArn,
    updateAccessLogSubscriptionResponse_id,
    updateAccessLogSubscriptionResponse_resourceArn,
    updateAccessLogSubscriptionResponse_resourceId,

    -- ** UpdateListener
    updateListener_defaultAction,
    updateListener_listenerIdentifier,
    updateListener_serviceIdentifier,
    updateListenerResponse_arn,
    updateListenerResponse_defaultAction,
    updateListenerResponse_id,
    updateListenerResponse_name,
    updateListenerResponse_port,
    updateListenerResponse_protocol,
    updateListenerResponse_serviceArn,
    updateListenerResponse_serviceId,
    updateListenerResponse_httpStatus,

    -- ** UpdateRule
    updateRule_action,
    updateRule_match,
    updateRule_priority,
    updateRule_listenerIdentifier,
    updateRule_ruleIdentifier,
    updateRule_serviceIdentifier,
    updateRuleResponse_action,
    updateRuleResponse_arn,
    updateRuleResponse_id,
    updateRuleResponse_isDefault,
    updateRuleResponse_match,
    updateRuleResponse_name,
    updateRuleResponse_priority,
    updateRuleResponse_httpStatus,

    -- ** UpdateService
    updateService_authType,
    updateService_certificateArn,
    updateService_serviceIdentifier,
    updateServiceResponse_arn,
    updateServiceResponse_authType,
    updateServiceResponse_certificateArn,
    updateServiceResponse_customDomainName,
    updateServiceResponse_id,
    updateServiceResponse_name,
    updateServiceResponse_httpStatus,

    -- ** UpdateServiceNetwork
    updateServiceNetwork_authType,
    updateServiceNetwork_serviceNetworkIdentifier,
    updateServiceNetworkResponse_arn,
    updateServiceNetworkResponse_authType,
    updateServiceNetworkResponse_id,
    updateServiceNetworkResponse_name,
    updateServiceNetworkResponse_httpStatus,

    -- ** UpdateServiceNetworkVpcAssociation
    updateServiceNetworkVpcAssociation_securityGroupIds,
    updateServiceNetworkVpcAssociation_serviceNetworkVpcAssociationIdentifier,
    updateServiceNetworkVpcAssociationResponse_arn,
    updateServiceNetworkVpcAssociationResponse_createdBy,
    updateServiceNetworkVpcAssociationResponse_id,
    updateServiceNetworkVpcAssociationResponse_securityGroupIds,
    updateServiceNetworkVpcAssociationResponse_status,
    updateServiceNetworkVpcAssociationResponse_httpStatus,

    -- ** UpdateTargetGroup
    updateTargetGroup_healthCheck,
    updateTargetGroup_targetGroupIdentifier,
    updateTargetGroupResponse_arn,
    updateTargetGroupResponse_config,
    updateTargetGroupResponse_id,
    updateTargetGroupResponse_name,
    updateTargetGroupResponse_status,
    updateTargetGroupResponse_type,
    updateTargetGroupResponse_httpStatus,

    -- * Types

    -- ** AccessLogSubscriptionSummary
    accessLogSubscriptionSummary_arn,
    accessLogSubscriptionSummary_createdAt,
    accessLogSubscriptionSummary_destinationArn,
    accessLogSubscriptionSummary_id,
    accessLogSubscriptionSummary_lastUpdatedAt,
    accessLogSubscriptionSummary_resourceArn,
    accessLogSubscriptionSummary_resourceId,

    -- ** DnsEntry
    dnsEntry_domainName,
    dnsEntry_hostedZoneId,

    -- ** FixedResponseAction
    fixedResponseAction_statusCode,

    -- ** ForwardAction
    forwardAction_targetGroups,

    -- ** HeaderMatch
    headerMatch_caseSensitive,
    headerMatch_match,
    headerMatch_name,

    -- ** HeaderMatchType
    headerMatchType_contains,
    headerMatchType_exact,
    headerMatchType_prefix,

    -- ** HealthCheckConfig
    healthCheckConfig_enabled,
    healthCheckConfig_healthCheckIntervalSeconds,
    healthCheckConfig_healthCheckTimeoutSeconds,
    healthCheckConfig_healthyThresholdCount,
    healthCheckConfig_matcher,
    healthCheckConfig_path,
    healthCheckConfig_port,
    healthCheckConfig_protocol,
    healthCheckConfig_protocolVersion,
    healthCheckConfig_unhealthyThresholdCount,

    -- ** HttpMatch
    httpMatch_headerMatches,
    httpMatch_method,
    httpMatch_pathMatch,

    -- ** ListenerSummary
    listenerSummary_arn,
    listenerSummary_createdAt,
    listenerSummary_id,
    listenerSummary_lastUpdatedAt,
    listenerSummary_name,
    listenerSummary_port,
    listenerSummary_protocol,

    -- ** Matcher
    matcher_httpCode,

    -- ** PathMatch
    pathMatch_caseSensitive,
    pathMatch_match,

    -- ** PathMatchType
    pathMatchType_exact,
    pathMatchType_prefix,

    -- ** RuleAction
    ruleAction_fixedResponse,
    ruleAction_forward,

    -- ** RuleMatch
    ruleMatch_httpMatch,

    -- ** RuleSummary
    ruleSummary_arn,
    ruleSummary_createdAt,
    ruleSummary_id,
    ruleSummary_isDefault,
    ruleSummary_lastUpdatedAt,
    ruleSummary_name,
    ruleSummary_priority,

    -- ** RuleUpdate
    ruleUpdate_action,
    ruleUpdate_match,
    ruleUpdate_priority,
    ruleUpdate_ruleIdentifier,

    -- ** RuleUpdateFailure
    ruleUpdateFailure_failureCode,
    ruleUpdateFailure_failureMessage,
    ruleUpdateFailure_ruleIdentifier,

    -- ** RuleUpdateSuccess
    ruleUpdateSuccess_action,
    ruleUpdateSuccess_arn,
    ruleUpdateSuccess_id,
    ruleUpdateSuccess_isDefault,
    ruleUpdateSuccess_match,
    ruleUpdateSuccess_name,
    ruleUpdateSuccess_priority,

    -- ** ServiceNetworkServiceAssociationSummary
    serviceNetworkServiceAssociationSummary_arn,
    serviceNetworkServiceAssociationSummary_createdAt,
    serviceNetworkServiceAssociationSummary_createdBy,
    serviceNetworkServiceAssociationSummary_customDomainName,
    serviceNetworkServiceAssociationSummary_dnsEntry,
    serviceNetworkServiceAssociationSummary_id,
    serviceNetworkServiceAssociationSummary_serviceArn,
    serviceNetworkServiceAssociationSummary_serviceId,
    serviceNetworkServiceAssociationSummary_serviceName,
    serviceNetworkServiceAssociationSummary_serviceNetworkArn,
    serviceNetworkServiceAssociationSummary_serviceNetworkId,
    serviceNetworkServiceAssociationSummary_serviceNetworkName,
    serviceNetworkServiceAssociationSummary_status,

    -- ** ServiceNetworkSummary
    serviceNetworkSummary_arn,
    serviceNetworkSummary_createdAt,
    serviceNetworkSummary_id,
    serviceNetworkSummary_lastUpdatedAt,
    serviceNetworkSummary_name,
    serviceNetworkSummary_numberOfAssociatedServices,
    serviceNetworkSummary_numberOfAssociatedVPCs,

    -- ** ServiceNetworkVpcAssociationSummary
    serviceNetworkVpcAssociationSummary_arn,
    serviceNetworkVpcAssociationSummary_createdAt,
    serviceNetworkVpcAssociationSummary_createdBy,
    serviceNetworkVpcAssociationSummary_id,
    serviceNetworkVpcAssociationSummary_lastUpdatedAt,
    serviceNetworkVpcAssociationSummary_serviceNetworkArn,
    serviceNetworkVpcAssociationSummary_serviceNetworkId,
    serviceNetworkVpcAssociationSummary_serviceNetworkName,
    serviceNetworkVpcAssociationSummary_status,
    serviceNetworkVpcAssociationSummary_vpcId,

    -- ** ServiceSummary
    serviceSummary_arn,
    serviceSummary_createdAt,
    serviceSummary_customDomainName,
    serviceSummary_dnsEntry,
    serviceSummary_id,
    serviceSummary_lastUpdatedAt,
    serviceSummary_name,
    serviceSummary_status,

    -- ** Target
    target_port,
    target_id,

    -- ** TargetFailure
    targetFailure_failureCode,
    targetFailure_failureMessage,
    targetFailure_id,
    targetFailure_port,

    -- ** TargetGroupConfig
    targetGroupConfig_healthCheck,
    targetGroupConfig_ipAddressType,
    targetGroupConfig_protocolVersion,
    targetGroupConfig_port,
    targetGroupConfig_protocol,
    targetGroupConfig_vpcIdentifier,

    -- ** TargetGroupSummary
    targetGroupSummary_arn,
    targetGroupSummary_createdAt,
    targetGroupSummary_id,
    targetGroupSummary_ipAddressType,
    targetGroupSummary_lastUpdatedAt,
    targetGroupSummary_name,
    targetGroupSummary_port,
    targetGroupSummary_protocol,
    targetGroupSummary_serviceArns,
    targetGroupSummary_status,
    targetGroupSummary_type,
    targetGroupSummary_vpcIdentifier,

    -- ** TargetSummary
    targetSummary_id,
    targetSummary_port,
    targetSummary_reasonCode,
    targetSummary_status,

    -- ** WeightedTargetGroup
    weightedTargetGroup_weight,
    weightedTargetGroup_targetGroupIdentifier,
  )
where

import Amazonka.VPCLattice.BatchUpdateRule
import Amazonka.VPCLattice.CreateAccessLogSubscription
import Amazonka.VPCLattice.CreateListener
import Amazonka.VPCLattice.CreateRule
import Amazonka.VPCLattice.CreateService
import Amazonka.VPCLattice.CreateServiceNetwork
import Amazonka.VPCLattice.CreateServiceNetworkServiceAssociation
import Amazonka.VPCLattice.CreateServiceNetworkVpcAssociation
import Amazonka.VPCLattice.CreateTargetGroup
import Amazonka.VPCLattice.DeleteAccessLogSubscription
import Amazonka.VPCLattice.DeleteAuthPolicy
import Amazonka.VPCLattice.DeleteListener
import Amazonka.VPCLattice.DeleteResourcePolicy
import Amazonka.VPCLattice.DeleteRule
import Amazonka.VPCLattice.DeleteService
import Amazonka.VPCLattice.DeleteServiceNetwork
import Amazonka.VPCLattice.DeleteServiceNetworkServiceAssociation
import Amazonka.VPCLattice.DeleteServiceNetworkVpcAssociation
import Amazonka.VPCLattice.DeleteTargetGroup
import Amazonka.VPCLattice.DeregisterTargets
import Amazonka.VPCLattice.GetAccessLogSubscription
import Amazonka.VPCLattice.GetAuthPolicy
import Amazonka.VPCLattice.GetListener
import Amazonka.VPCLattice.GetResourcePolicy
import Amazonka.VPCLattice.GetRule
import Amazonka.VPCLattice.GetService
import Amazonka.VPCLattice.GetServiceNetwork
import Amazonka.VPCLattice.GetServiceNetworkServiceAssociation
import Amazonka.VPCLattice.GetServiceNetworkVpcAssociation
import Amazonka.VPCLattice.GetTargetGroup
import Amazonka.VPCLattice.ListAccessLogSubscriptions
import Amazonka.VPCLattice.ListListeners
import Amazonka.VPCLattice.ListRules
import Amazonka.VPCLattice.ListServiceNetworkServiceAssociations
import Amazonka.VPCLattice.ListServiceNetworkVpcAssociations
import Amazonka.VPCLattice.ListServiceNetworks
import Amazonka.VPCLattice.ListServices
import Amazonka.VPCLattice.ListTagsForResource
import Amazonka.VPCLattice.ListTargetGroups
import Amazonka.VPCLattice.ListTargets
import Amazonka.VPCLattice.PutAuthPolicy
import Amazonka.VPCLattice.PutResourcePolicy
import Amazonka.VPCLattice.RegisterTargets
import Amazonka.VPCLattice.TagResource
import Amazonka.VPCLattice.Types.AccessLogSubscriptionSummary
import Amazonka.VPCLattice.Types.DnsEntry
import Amazonka.VPCLattice.Types.FixedResponseAction
import Amazonka.VPCLattice.Types.ForwardAction
import Amazonka.VPCLattice.Types.HeaderMatch
import Amazonka.VPCLattice.Types.HeaderMatchType
import Amazonka.VPCLattice.Types.HealthCheckConfig
import Amazonka.VPCLattice.Types.HttpMatch
import Amazonka.VPCLattice.Types.ListenerSummary
import Amazonka.VPCLattice.Types.Matcher
import Amazonka.VPCLattice.Types.PathMatch
import Amazonka.VPCLattice.Types.PathMatchType
import Amazonka.VPCLattice.Types.RuleAction
import Amazonka.VPCLattice.Types.RuleMatch
import Amazonka.VPCLattice.Types.RuleSummary
import Amazonka.VPCLattice.Types.RuleUpdate
import Amazonka.VPCLattice.Types.RuleUpdateFailure
import Amazonka.VPCLattice.Types.RuleUpdateSuccess
import Amazonka.VPCLattice.Types.ServiceNetworkServiceAssociationSummary
import Amazonka.VPCLattice.Types.ServiceNetworkSummary
import Amazonka.VPCLattice.Types.ServiceNetworkVpcAssociationSummary
import Amazonka.VPCLattice.Types.ServiceSummary
import Amazonka.VPCLattice.Types.Target
import Amazonka.VPCLattice.Types.TargetFailure
import Amazonka.VPCLattice.Types.TargetGroupConfig
import Amazonka.VPCLattice.Types.TargetGroupSummary
import Amazonka.VPCLattice.Types.TargetSummary
import Amazonka.VPCLattice.Types.WeightedTargetGroup
import Amazonka.VPCLattice.UntagResource
import Amazonka.VPCLattice.UpdateAccessLogSubscription
import Amazonka.VPCLattice.UpdateListener
import Amazonka.VPCLattice.UpdateRule
import Amazonka.VPCLattice.UpdateService
import Amazonka.VPCLattice.UpdateServiceNetwork
import Amazonka.VPCLattice.UpdateServiceNetworkVpcAssociation
import Amazonka.VPCLattice.UpdateTargetGroup
