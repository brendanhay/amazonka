{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Lens
  ( -- * Operations

    -- ** DescribeSSLPolicies
    describeSSLPolicies_names,
    describeSSLPolicies_pageSize,
    describeSSLPolicies_marker,
    describeSSLPoliciesResponse_nextMarker,
    describeSSLPoliciesResponse_sslPolicies,
    describeSSLPoliciesResponse_httpStatus,

    -- ** RemoveTags
    removeTags_resourceArns,
    removeTags_tagKeys,
    removeTagsResponse_httpStatus,

    -- ** DeleteRule
    deleteRule_ruleArn,
    deleteRuleResponse_httpStatus,

    -- ** DescribeTags
    describeTags_resourceArns,
    describeTagsResponse_tagDescriptions,
    describeTagsResponse_httpStatus,

    -- ** DescribeTargetGroupAttributes
    describeTargetGroupAttributes_targetGroupArn,
    describeTargetGroupAttributesResponse_attributes,
    describeTargetGroupAttributesResponse_httpStatus,

    -- ** AddListenerCertificates
    addListenerCertificates_listenerArn,
    addListenerCertificates_certificates,
    addListenerCertificatesResponse_certificates,
    addListenerCertificatesResponse_httpStatus,

    -- ** CreateLoadBalancer
    createLoadBalancer_ipAddressType,
    createLoadBalancer_customerOwnedIpv4Pool,
    createLoadBalancer_subnetMappings,
    createLoadBalancer_scheme,
    createLoadBalancer_securityGroups,
    createLoadBalancer_tags,
    createLoadBalancer_type,
    createLoadBalancer_subnets,
    createLoadBalancer_name,
    createLoadBalancerResponse_loadBalancers,
    createLoadBalancerResponse_httpStatus,

    -- ** RemoveListenerCertificates
    removeListenerCertificates_listenerArn,
    removeListenerCertificates_certificates,
    removeListenerCertificatesResponse_httpStatus,

    -- ** ModifyRule
    modifyRule_actions,
    modifyRule_conditions,
    modifyRule_ruleArn,
    modifyRuleResponse_rules,
    modifyRuleResponse_httpStatus,

    -- ** ModifyTargetGroup
    modifyTargetGroup_healthCheckEnabled,
    modifyTargetGroup_healthCheckProtocol,
    modifyTargetGroup_healthCheckPort,
    modifyTargetGroup_healthCheckTimeoutSeconds,
    modifyTargetGroup_healthCheckPath,
    modifyTargetGroup_matcher,
    modifyTargetGroup_healthyThresholdCount,
    modifyTargetGroup_healthCheckIntervalSeconds,
    modifyTargetGroup_unhealthyThresholdCount,
    modifyTargetGroup_targetGroupArn,
    modifyTargetGroupResponse_targetGroups,
    modifyTargetGroupResponse_httpStatus,

    -- ** DeleteLoadBalancer
    deleteLoadBalancer_loadBalancerArn,
    deleteLoadBalancerResponse_httpStatus,

    -- ** DescribeListeners
    describeListeners_loadBalancerArn,
    describeListeners_pageSize,
    describeListeners_listenerArns,
    describeListeners_marker,
    describeListenersResponse_nextMarker,
    describeListenersResponse_listeners,
    describeListenersResponse_httpStatus,

    -- ** AddTags
    addTags_resourceArns,
    addTags_tags,
    addTagsResponse_httpStatus,

    -- ** DescribeTargetGroups
    describeTargetGroups_loadBalancerArn,
    describeTargetGroups_names,
    describeTargetGroups_pageSize,
    describeTargetGroups_targetGroupArns,
    describeTargetGroups_marker,
    describeTargetGroupsResponse_targetGroups,
    describeTargetGroupsResponse_nextMarker,
    describeTargetGroupsResponse_httpStatus,

    -- ** SetIpAddressType
    setIpAddressType_loadBalancerArn,
    setIpAddressType_ipAddressType,
    setIpAddressTypeResponse_ipAddressType,
    setIpAddressTypeResponse_httpStatus,

    -- ** ModifyLoadBalancerAttributes
    modifyLoadBalancerAttributes_loadBalancerArn,
    modifyLoadBalancerAttributes_attributes,
    modifyLoadBalancerAttributesResponse_attributes,
    modifyLoadBalancerAttributesResponse_httpStatus,

    -- ** DescribeAccountLimits
    describeAccountLimits_pageSize,
    describeAccountLimits_marker,
    describeAccountLimitsResponse_nextMarker,
    describeAccountLimitsResponse_limits,
    describeAccountLimitsResponse_httpStatus,

    -- ** CreateRule
    createRule_tags,
    createRule_listenerArn,
    createRule_conditions,
    createRule_priority,
    createRule_actions,
    createRuleResponse_rules,
    createRuleResponse_httpStatus,

    -- ** CreateTargetGroup
    createTargetGroup_healthCheckEnabled,
    createTargetGroup_healthCheckProtocol,
    createTargetGroup_targetType,
    createTargetGroup_healthCheckPort,
    createTargetGroup_healthCheckTimeoutSeconds,
    createTargetGroup_healthCheckPath,
    createTargetGroup_matcher,
    createTargetGroup_protocolVersion,
    createTargetGroup_healthyThresholdCount,
    createTargetGroup_tags,
    createTargetGroup_port,
    createTargetGroup_healthCheckIntervalSeconds,
    createTargetGroup_protocol,
    createTargetGroup_vpcId,
    createTargetGroup_unhealthyThresholdCount,
    createTargetGroup_name,
    createTargetGroupResponse_targetGroups,
    createTargetGroupResponse_httpStatus,

    -- ** SetSubnets
    setSubnets_ipAddressType,
    setSubnets_subnetMappings,
    setSubnets_subnets,
    setSubnets_loadBalancerArn,
    setSubnetsResponse_ipAddressType,
    setSubnetsResponse_availabilityZones,
    setSubnetsResponse_httpStatus,

    -- ** DeregisterTargets
    deregisterTargets_targetGroupArn,
    deregisterTargets_targets,
    deregisterTargetsResponse_httpStatus,

    -- ** DeleteListener
    deleteListener_listenerArn,
    deleteListenerResponse_httpStatus,

    -- ** DeleteTargetGroup
    deleteTargetGroup_targetGroupArn,
    deleteTargetGroupResponse_httpStatus,

    -- ** DescribeLoadBalancers
    describeLoadBalancers_names,
    describeLoadBalancers_pageSize,
    describeLoadBalancers_loadBalancerArns,
    describeLoadBalancers_marker,
    describeLoadBalancersResponse_nextMarker,
    describeLoadBalancersResponse_loadBalancers,
    describeLoadBalancersResponse_httpStatus,

    -- ** ModifyTargetGroupAttributes
    modifyTargetGroupAttributes_targetGroupArn,
    modifyTargetGroupAttributes_attributes,
    modifyTargetGroupAttributesResponse_attributes,
    modifyTargetGroupAttributesResponse_httpStatus,

    -- ** ModifyListener
    modifyListener_sslPolicy,
    modifyListener_port,
    modifyListener_defaultActions,
    modifyListener_protocol,
    modifyListener_certificates,
    modifyListener_alpnPolicy,
    modifyListener_listenerArn,
    modifyListenerResponse_listeners,
    modifyListenerResponse_httpStatus,

    -- ** RegisterTargets
    registerTargets_targetGroupArn,
    registerTargets_targets,
    registerTargetsResponse_httpStatus,

    -- ** DescribeTargetHealth
    describeTargetHealth_targets,
    describeTargetHealth_targetGroupArn,
    describeTargetHealthResponse_targetHealthDescriptions,
    describeTargetHealthResponse_httpStatus,

    -- ** SetRulePriorities
    setRulePriorities_rulePriorities,
    setRulePrioritiesResponse_rules,
    setRulePrioritiesResponse_httpStatus,

    -- ** DescribeRules
    describeRules_pageSize,
    describeRules_listenerArn,
    describeRules_ruleArns,
    describeRules_marker,
    describeRulesResponse_nextMarker,
    describeRulesResponse_rules,
    describeRulesResponse_httpStatus,

    -- ** SetSecurityGroups
    setSecurityGroups_loadBalancerArn,
    setSecurityGroups_securityGroups,
    setSecurityGroupsResponse_securityGroupIds,
    setSecurityGroupsResponse_httpStatus,

    -- ** DescribeLoadBalancerAttributes
    describeLoadBalancerAttributes_loadBalancerArn,
    describeLoadBalancerAttributesResponse_attributes,
    describeLoadBalancerAttributesResponse_httpStatus,

    -- ** DescribeListenerCertificates
    describeListenerCertificates_pageSize,
    describeListenerCertificates_marker,
    describeListenerCertificates_listenerArn,
    describeListenerCertificatesResponse_nextMarker,
    describeListenerCertificatesResponse_certificates,
    describeListenerCertificatesResponse_httpStatus,

    -- ** CreateListener
    createListener_sslPolicy,
    createListener_tags,
    createListener_port,
    createListener_protocol,
    createListener_certificates,
    createListener_alpnPolicy,
    createListener_loadBalancerArn,
    createListener_defaultActions,
    createListenerResponse_listeners,
    createListenerResponse_httpStatus,

    -- * Types

    -- ** Action
    action_authenticateOidcConfig,
    action_targetGroupArn,
    action_authenticateCognitoConfig,
    action_fixedResponseConfig,
    action_forwardConfig,
    action_order,
    action_redirectConfig,
    action_type,

    -- ** AuthenticateCognitoActionConfig
    authenticateCognitoActionConfig_sessionTimeout,
    authenticateCognitoActionConfig_scope,
    authenticateCognitoActionConfig_authenticationRequestExtraParams,
    authenticateCognitoActionConfig_sessionCookieName,
    authenticateCognitoActionConfig_onUnauthenticatedRequest,
    authenticateCognitoActionConfig_userPoolArn,
    authenticateCognitoActionConfig_userPoolClientId,
    authenticateCognitoActionConfig_userPoolDomain,

    -- ** AuthenticateOidcActionConfig
    authenticateOidcActionConfig_useExistingClientSecret,
    authenticateOidcActionConfig_clientSecret,
    authenticateOidcActionConfig_sessionTimeout,
    authenticateOidcActionConfig_scope,
    authenticateOidcActionConfig_authenticationRequestExtraParams,
    authenticateOidcActionConfig_sessionCookieName,
    authenticateOidcActionConfig_onUnauthenticatedRequest,
    authenticateOidcActionConfig_issuer,
    authenticateOidcActionConfig_authorizationEndpoint,
    authenticateOidcActionConfig_tokenEndpoint,
    authenticateOidcActionConfig_userInfoEndpoint,
    authenticateOidcActionConfig_clientId,

    -- ** AvailabilityZone
    availabilityZone_zoneName,
    availabilityZone_outpostId,
    availabilityZone_loadBalancerAddresses,
    availabilityZone_subnetId,

    -- ** Certificate
    certificate_isDefault,
    certificate_certificateArn,

    -- ** Cipher
    cipher_priority,
    cipher_name,

    -- ** FixedResponseActionConfig
    fixedResponseActionConfig_contentType,
    fixedResponseActionConfig_messageBody,
    fixedResponseActionConfig_statusCode,

    -- ** ForwardActionConfig
    forwardActionConfig_targetGroupStickinessConfig,
    forwardActionConfig_targetGroups,

    -- ** HostHeaderConditionConfig
    hostHeaderConditionConfig_values,

    -- ** HttpHeaderConditionConfig
    httpHeaderConditionConfig_values,
    httpHeaderConditionConfig_httpHeaderName,

    -- ** HttpRequestMethodConditionConfig
    httpRequestMethodConditionConfig_values,

    -- ** Limit
    limit_name,
    limit_max,

    -- ** Listener
    listener_loadBalancerArn,
    listener_sslPolicy,
    listener_port,
    listener_defaultActions,
    listener_protocol,
    listener_certificates,
    listener_listenerArn,
    listener_alpnPolicy,

    -- ** LoadBalancer
    loadBalancer_ipAddressType,
    loadBalancer_loadBalancerArn,
    loadBalancer_customerOwnedIpv4Pool,
    loadBalancer_availabilityZones,
    loadBalancer_scheme,
    loadBalancer_createdTime,
    loadBalancer_securityGroups,
    loadBalancer_state,
    loadBalancer_dNSName,
    loadBalancer_type,
    loadBalancer_canonicalHostedZoneId,
    loadBalancer_vpcId,
    loadBalancer_loadBalancerName,

    -- ** LoadBalancerAddress
    loadBalancerAddress_privateIPv4Address,
    loadBalancerAddress_ipAddress,
    loadBalancerAddress_iPv6Address,
    loadBalancerAddress_allocationId,

    -- ** LoadBalancerAttribute
    loadBalancerAttribute_key,
    loadBalancerAttribute_value,

    -- ** LoadBalancerState
    loadBalancerState_code,
    loadBalancerState_reason,

    -- ** Matcher
    matcher_grpcCode,
    matcher_httpCode,

    -- ** PathPatternConditionConfig
    pathPatternConditionConfig_values,

    -- ** QueryStringConditionConfig
    queryStringConditionConfig_values,

    -- ** QueryStringKeyValuePair
    queryStringKeyValuePair_key,
    queryStringKeyValuePair_value,

    -- ** RedirectActionConfig
    redirectActionConfig_query,
    redirectActionConfig_port,
    redirectActionConfig_protocol,
    redirectActionConfig_host,
    redirectActionConfig_path,
    redirectActionConfig_statusCode,

    -- ** Rule
    rule_isDefault,
    rule_ruleArn,
    rule_actions,
    rule_priority,
    rule_conditions,

    -- ** RuleCondition
    ruleCondition_pathPatternConfig,
    ruleCondition_httpRequestMethodConfig,
    ruleCondition_values,
    ruleCondition_sourceIpConfig,
    ruleCondition_httpHeaderConfig,
    ruleCondition_hostHeaderConfig,
    ruleCondition_queryStringConfig,
    ruleCondition_field,

    -- ** RulePriorityPair
    rulePriorityPair_ruleArn,
    rulePriorityPair_priority,

    -- ** SourceIpConditionConfig
    sourceIpConditionConfig_values,

    -- ** SslPolicy
    sslPolicy_ciphers,
    sslPolicy_name,
    sslPolicy_sslProtocols,

    -- ** SubnetMapping
    subnetMapping_privateIPv4Address,
    subnetMapping_iPv6Address,
    subnetMapping_subnetId,
    subnetMapping_allocationId,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** TagDescription
    tagDescription_resourceArn,
    tagDescription_tags,

    -- ** TargetDescription
    targetDescription_availabilityZone,
    targetDescription_port,
    targetDescription_id,

    -- ** TargetGroup
    targetGroup_healthCheckEnabled,
    targetGroup_healthCheckProtocol,
    targetGroup_targetGroupName,
    targetGroup_targetType,
    targetGroup_targetGroupArn,
    targetGroup_healthCheckPort,
    targetGroup_healthCheckTimeoutSeconds,
    targetGroup_healthCheckPath,
    targetGroup_loadBalancerArns,
    targetGroup_matcher,
    targetGroup_protocolVersion,
    targetGroup_healthyThresholdCount,
    targetGroup_port,
    targetGroup_healthCheckIntervalSeconds,
    targetGroup_protocol,
    targetGroup_vpcId,
    targetGroup_unhealthyThresholdCount,

    -- ** TargetGroupAttribute
    targetGroupAttribute_key,
    targetGroupAttribute_value,

    -- ** TargetGroupStickinessConfig
    targetGroupStickinessConfig_enabled,
    targetGroupStickinessConfig_durationSeconds,

    -- ** TargetGroupTuple
    targetGroupTuple_targetGroupArn,
    targetGroupTuple_weight,

    -- ** TargetHealth
    targetHealth_state,
    targetHealth_reason,
    targetHealth_description,

    -- ** TargetHealthDescription
    targetHealthDescription_healthCheckPort,
    targetHealthDescription_target,
    targetHealthDescription_targetHealth,
  )
where

import Network.AWS.ELBv2.AddListenerCertificates
import Network.AWS.ELBv2.AddTags
import Network.AWS.ELBv2.CreateListener
import Network.AWS.ELBv2.CreateLoadBalancer
import Network.AWS.ELBv2.CreateRule
import Network.AWS.ELBv2.CreateTargetGroup
import Network.AWS.ELBv2.DeleteListener
import Network.AWS.ELBv2.DeleteLoadBalancer
import Network.AWS.ELBv2.DeleteRule
import Network.AWS.ELBv2.DeleteTargetGroup
import Network.AWS.ELBv2.DeregisterTargets
import Network.AWS.ELBv2.DescribeAccountLimits
import Network.AWS.ELBv2.DescribeListenerCertificates
import Network.AWS.ELBv2.DescribeListeners
import Network.AWS.ELBv2.DescribeLoadBalancerAttributes
import Network.AWS.ELBv2.DescribeLoadBalancers
import Network.AWS.ELBv2.DescribeRules
import Network.AWS.ELBv2.DescribeSSLPolicies
import Network.AWS.ELBv2.DescribeTags
import Network.AWS.ELBv2.DescribeTargetGroupAttributes
import Network.AWS.ELBv2.DescribeTargetGroups
import Network.AWS.ELBv2.DescribeTargetHealth
import Network.AWS.ELBv2.ModifyListener
import Network.AWS.ELBv2.ModifyLoadBalancerAttributes
import Network.AWS.ELBv2.ModifyRule
import Network.AWS.ELBv2.ModifyTargetGroup
import Network.AWS.ELBv2.ModifyTargetGroupAttributes
import Network.AWS.ELBv2.RegisterTargets
import Network.AWS.ELBv2.RemoveListenerCertificates
import Network.AWS.ELBv2.RemoveTags
import Network.AWS.ELBv2.SetIpAddressType
import Network.AWS.ELBv2.SetRulePriorities
import Network.AWS.ELBv2.SetSecurityGroups
import Network.AWS.ELBv2.SetSubnets
import Network.AWS.ELBv2.Types.Action
import Network.AWS.ELBv2.Types.AuthenticateCognitoActionConfig
import Network.AWS.ELBv2.Types.AuthenticateOidcActionConfig
import Network.AWS.ELBv2.Types.AvailabilityZone
import Network.AWS.ELBv2.Types.Certificate
import Network.AWS.ELBv2.Types.Cipher
import Network.AWS.ELBv2.Types.FixedResponseActionConfig
import Network.AWS.ELBv2.Types.ForwardActionConfig
import Network.AWS.ELBv2.Types.HostHeaderConditionConfig
import Network.AWS.ELBv2.Types.HttpHeaderConditionConfig
import Network.AWS.ELBv2.Types.HttpRequestMethodConditionConfig
import Network.AWS.ELBv2.Types.Limit
import Network.AWS.ELBv2.Types.Listener
import Network.AWS.ELBv2.Types.LoadBalancer
import Network.AWS.ELBv2.Types.LoadBalancerAddress
import Network.AWS.ELBv2.Types.LoadBalancerAttribute
import Network.AWS.ELBv2.Types.LoadBalancerState
import Network.AWS.ELBv2.Types.Matcher
import Network.AWS.ELBv2.Types.PathPatternConditionConfig
import Network.AWS.ELBv2.Types.QueryStringConditionConfig
import Network.AWS.ELBv2.Types.QueryStringKeyValuePair
import Network.AWS.ELBv2.Types.RedirectActionConfig
import Network.AWS.ELBv2.Types.Rule
import Network.AWS.ELBv2.Types.RuleCondition
import Network.AWS.ELBv2.Types.RulePriorityPair
import Network.AWS.ELBv2.Types.SourceIpConditionConfig
import Network.AWS.ELBv2.Types.SslPolicy
import Network.AWS.ELBv2.Types.SubnetMapping
import Network.AWS.ELBv2.Types.Tag
import Network.AWS.ELBv2.Types.TagDescription
import Network.AWS.ELBv2.Types.TargetDescription
import Network.AWS.ELBv2.Types.TargetGroup
import Network.AWS.ELBv2.Types.TargetGroupAttribute
import Network.AWS.ELBv2.Types.TargetGroupStickinessConfig
import Network.AWS.ELBv2.Types.TargetGroupTuple
import Network.AWS.ELBv2.Types.TargetHealth
import Network.AWS.ELBv2.Types.TargetHealthDescription
