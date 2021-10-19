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

    -- ** DescribeLoadBalancers
    describeLoadBalancers_names,
    describeLoadBalancers_loadBalancerArns,
    describeLoadBalancers_marker,
    describeLoadBalancers_pageSize,
    describeLoadBalancersResponse_loadBalancers,
    describeLoadBalancersResponse_nextMarker,
    describeLoadBalancersResponse_httpStatus,

    -- ** DescribeTags
    describeTags_resourceArns,
    describeTagsResponse_tagDescriptions,
    describeTagsResponse_httpStatus,

    -- ** DeleteRule
    deleteRule_ruleArn,
    deleteRuleResponse_httpStatus,

    -- ** RemoveTags
    removeTags_resourceArns,
    removeTags_tagKeys,
    removeTagsResponse_httpStatus,

    -- ** DeleteTargetGroup
    deleteTargetGroup_targetGroupArn,
    deleteTargetGroupResponse_httpStatus,

    -- ** SetSubnets
    setSubnets_subnetMappings,
    setSubnets_subnets,
    setSubnets_ipAddressType,
    setSubnets_loadBalancerArn,
    setSubnetsResponse_availabilityZones,
    setSubnetsResponse_ipAddressType,
    setSubnetsResponse_httpStatus,

    -- ** CreateRule
    createRule_tags,
    createRule_listenerArn,
    createRule_conditions,
    createRule_priority,
    createRule_actions,
    createRuleResponse_rules,
    createRuleResponse_httpStatus,

    -- ** DescribeListenerCertificates
    describeListenerCertificates_marker,
    describeListenerCertificates_pageSize,
    describeListenerCertificates_listenerArn,
    describeListenerCertificatesResponse_certificates,
    describeListenerCertificatesResponse_nextMarker,
    describeListenerCertificatesResponse_httpStatus,

    -- ** SetSecurityGroups
    setSecurityGroups_loadBalancerArn,
    setSecurityGroups_securityGroups,
    setSecurityGroupsResponse_securityGroupIds,
    setSecurityGroupsResponse_httpStatus,

    -- ** SetRulePriorities
    setRulePriorities_rulePriorities,
    setRulePrioritiesResponse_rules,
    setRulePrioritiesResponse_httpStatus,

    -- ** DescribeTargetGroups
    describeTargetGroups_targetGroupArns,
    describeTargetGroups_names,
    describeTargetGroups_loadBalancerArn,
    describeTargetGroups_marker,
    describeTargetGroups_pageSize,
    describeTargetGroupsResponse_nextMarker,
    describeTargetGroupsResponse_targetGroups,
    describeTargetGroupsResponse_httpStatus,

    -- ** DescribeRules
    describeRules_listenerArn,
    describeRules_marker,
    describeRules_ruleArns,
    describeRules_pageSize,
    describeRulesResponse_rules,
    describeRulesResponse_nextMarker,
    describeRulesResponse_httpStatus,

    -- ** DeleteLoadBalancer
    deleteLoadBalancer_loadBalancerArn,
    deleteLoadBalancerResponse_httpStatus,

    -- ** RegisterTargets
    registerTargets_targetGroupArn,
    registerTargets_targets,
    registerTargetsResponse_httpStatus,

    -- ** ModifyListener
    modifyListener_sslPolicy,
    modifyListener_protocol,
    modifyListener_defaultActions,
    modifyListener_certificates,
    modifyListener_alpnPolicy,
    modifyListener_port,
    modifyListener_listenerArn,
    modifyListenerResponse_listeners,
    modifyListenerResponse_httpStatus,

    -- ** ModifyTargetGroup
    modifyTargetGroup_matcher,
    modifyTargetGroup_healthCheckPath,
    modifyTargetGroup_healthCheckEnabled,
    modifyTargetGroup_unhealthyThresholdCount,
    modifyTargetGroup_healthCheckIntervalSeconds,
    modifyTargetGroup_healthyThresholdCount,
    modifyTargetGroup_healthCheckProtocol,
    modifyTargetGroup_healthCheckTimeoutSeconds,
    modifyTargetGroup_healthCheckPort,
    modifyTargetGroup_targetGroupArn,
    modifyTargetGroupResponse_targetGroups,
    modifyTargetGroupResponse_httpStatus,

    -- ** ModifyTargetGroupAttributes
    modifyTargetGroupAttributes_targetGroupArn,
    modifyTargetGroupAttributes_attributes,
    modifyTargetGroupAttributesResponse_attributes,
    modifyTargetGroupAttributesResponse_httpStatus,

    -- ** DescribeTargetGroupAttributes
    describeTargetGroupAttributes_targetGroupArn,
    describeTargetGroupAttributesResponse_attributes,
    describeTargetGroupAttributesResponse_httpStatus,

    -- ** DeleteListener
    deleteListener_listenerArn,
    deleteListenerResponse_httpStatus,

    -- ** DescribeSSLPolicies
    describeSSLPolicies_loadBalancerType,
    describeSSLPolicies_names,
    describeSSLPolicies_marker,
    describeSSLPolicies_pageSize,
    describeSSLPoliciesResponse_sslPolicies,
    describeSSLPoliciesResponse_nextMarker,
    describeSSLPoliciesResponse_httpStatus,

    -- ** DescribeAccountLimits
    describeAccountLimits_marker,
    describeAccountLimits_pageSize,
    describeAccountLimitsResponse_limits,
    describeAccountLimitsResponse_nextMarker,
    describeAccountLimitsResponse_httpStatus,

    -- ** DeregisterTargets
    deregisterTargets_targetGroupArn,
    deregisterTargets_targets,
    deregisterTargetsResponse_httpStatus,

    -- ** CreateListener
    createListener_sslPolicy,
    createListener_protocol,
    createListener_certificates,
    createListener_alpnPolicy,
    createListener_tags,
    createListener_port,
    createListener_loadBalancerArn,
    createListener_defaultActions,
    createListenerResponse_listeners,
    createListenerResponse_httpStatus,

    -- ** CreateTargetGroup
    createTargetGroup_protocolVersion,
    createTargetGroup_matcher,
    createTargetGroup_healthCheckPath,
    createTargetGroup_healthCheckEnabled,
    createTargetGroup_unhealthyThresholdCount,
    createTargetGroup_vpcId,
    createTargetGroup_protocol,
    createTargetGroup_healthCheckIntervalSeconds,
    createTargetGroup_targetType,
    createTargetGroup_healthyThresholdCount,
    createTargetGroup_healthCheckProtocol,
    createTargetGroup_ipAddressType,
    createTargetGroup_healthCheckTimeoutSeconds,
    createTargetGroup_healthCheckPort,
    createTargetGroup_tags,
    createTargetGroup_port,
    createTargetGroup_name,
    createTargetGroupResponse_targetGroups,
    createTargetGroupResponse_httpStatus,

    -- ** ModifyLoadBalancerAttributes
    modifyLoadBalancerAttributes_loadBalancerArn,
    modifyLoadBalancerAttributes_attributes,
    modifyLoadBalancerAttributesResponse_attributes,
    modifyLoadBalancerAttributesResponse_httpStatus,

    -- ** SetIpAddressType
    setIpAddressType_loadBalancerArn,
    setIpAddressType_ipAddressType,
    setIpAddressTypeResponse_ipAddressType,
    setIpAddressTypeResponse_httpStatus,

    -- ** AddTags
    addTags_resourceArns,
    addTags_tags,
    addTagsResponse_httpStatus,

    -- ** DescribeLoadBalancerAttributes
    describeLoadBalancerAttributes_loadBalancerArn,
    describeLoadBalancerAttributesResponse_attributes,
    describeLoadBalancerAttributesResponse_httpStatus,

    -- ** DescribeListeners
    describeListeners_listenerArns,
    describeListeners_loadBalancerArn,
    describeListeners_marker,
    describeListeners_pageSize,
    describeListenersResponse_nextMarker,
    describeListenersResponse_listeners,
    describeListenersResponse_httpStatus,

    -- ** DescribeTargetHealth
    describeTargetHealth_targets,
    describeTargetHealth_targetGroupArn,
    describeTargetHealthResponse_targetHealthDescriptions,
    describeTargetHealthResponse_httpStatus,

    -- ** CreateLoadBalancer
    createLoadBalancer_subnetMappings,
    createLoadBalancer_securityGroups,
    createLoadBalancer_subnets,
    createLoadBalancer_customerOwnedIpv4Pool,
    createLoadBalancer_ipAddressType,
    createLoadBalancer_scheme,
    createLoadBalancer_type,
    createLoadBalancer_tags,
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

    -- ** AddListenerCertificates
    addListenerCertificates_listenerArn,
    addListenerCertificates_certificates,
    addListenerCertificatesResponse_certificates,
    addListenerCertificatesResponse_httpStatus,

    -- * Types

    -- ** Action
    action_fixedResponseConfig,
    action_targetGroupArn,
    action_forwardConfig,
    action_redirectConfig,
    action_authenticateCognitoConfig,
    action_order,
    action_authenticateOidcConfig,
    action_type,

    -- ** AuthenticateCognitoActionConfig
    authenticateCognitoActionConfig_authenticationRequestExtraParams,
    authenticateCognitoActionConfig_scope,
    authenticateCognitoActionConfig_onUnauthenticatedRequest,
    authenticateCognitoActionConfig_sessionCookieName,
    authenticateCognitoActionConfig_sessionTimeout,
    authenticateCognitoActionConfig_userPoolArn,
    authenticateCognitoActionConfig_userPoolClientId,
    authenticateCognitoActionConfig_userPoolDomain,

    -- ** AuthenticateOidcActionConfig
    authenticateOidcActionConfig_clientSecret,
    authenticateOidcActionConfig_useExistingClientSecret,
    authenticateOidcActionConfig_authenticationRequestExtraParams,
    authenticateOidcActionConfig_scope,
    authenticateOidcActionConfig_onUnauthenticatedRequest,
    authenticateOidcActionConfig_sessionCookieName,
    authenticateOidcActionConfig_sessionTimeout,
    authenticateOidcActionConfig_issuer,
    authenticateOidcActionConfig_authorizationEndpoint,
    authenticateOidcActionConfig_tokenEndpoint,
    authenticateOidcActionConfig_userInfoEndpoint,
    authenticateOidcActionConfig_clientId,

    -- ** AvailabilityZone
    availabilityZone_subnetId,
    availabilityZone_zoneName,
    availabilityZone_loadBalancerAddresses,
    availabilityZone_outpostId,

    -- ** Certificate
    certificate_certificateArn,
    certificate_isDefault,

    -- ** Cipher
    cipher_priority,
    cipher_name,

    -- ** FixedResponseActionConfig
    fixedResponseActionConfig_messageBody,
    fixedResponseActionConfig_contentType,
    fixedResponseActionConfig_statusCode,

    -- ** ForwardActionConfig
    forwardActionConfig_targetGroups,
    forwardActionConfig_targetGroupStickinessConfig,

    -- ** HostHeaderConditionConfig
    hostHeaderConditionConfig_values,

    -- ** HttpHeaderConditionConfig
    httpHeaderConditionConfig_values,
    httpHeaderConditionConfig_httpHeaderName,

    -- ** HttpRequestMethodConditionConfig
    httpRequestMethodConditionConfig_values,

    -- ** Limit
    limit_max,
    limit_name,

    -- ** Listener
    listener_sslPolicy,
    listener_listenerArn,
    listener_protocol,
    listener_defaultActions,
    listener_certificates,
    listener_loadBalancerArn,
    listener_alpnPolicy,
    listener_port,

    -- ** LoadBalancer
    loadBalancer_state,
    loadBalancer_securityGroups,
    loadBalancer_loadBalancerName,
    loadBalancer_createdTime,
    loadBalancer_vpcId,
    loadBalancer_canonicalHostedZoneId,
    loadBalancer_availabilityZones,
    loadBalancer_customerOwnedIpv4Pool,
    loadBalancer_loadBalancerArn,
    loadBalancer_ipAddressType,
    loadBalancer_scheme,
    loadBalancer_type,
    loadBalancer_dNSName,

    -- ** LoadBalancerAddress
    loadBalancerAddress_iPv6Address,
    loadBalancerAddress_ipAddress,
    loadBalancerAddress_allocationId,
    loadBalancerAddress_privateIPv4Address,

    -- ** LoadBalancerAttribute
    loadBalancerAttribute_value,
    loadBalancerAttribute_key,

    -- ** LoadBalancerState
    loadBalancerState_reason,
    loadBalancerState_code,

    -- ** Matcher
    matcher_httpCode,
    matcher_grpcCode,

    -- ** PathPatternConditionConfig
    pathPatternConditionConfig_values,

    -- ** QueryStringConditionConfig
    queryStringConditionConfig_values,

    -- ** QueryStringKeyValuePair
    queryStringKeyValuePair_value,
    queryStringKeyValuePair_key,

    -- ** RedirectActionConfig
    redirectActionConfig_path,
    redirectActionConfig_protocol,
    redirectActionConfig_query,
    redirectActionConfig_host,
    redirectActionConfig_port,
    redirectActionConfig_statusCode,

    -- ** Rule
    rule_priority,
    rule_actions,
    rule_conditions,
    rule_ruleArn,
    rule_isDefault,

    -- ** RuleCondition
    ruleCondition_field,
    ruleCondition_httpHeaderConfig,
    ruleCondition_hostHeaderConfig,
    ruleCondition_values,
    ruleCondition_sourceIpConfig,
    ruleCondition_httpRequestMethodConfig,
    ruleCondition_pathPatternConfig,
    ruleCondition_queryStringConfig,

    -- ** RulePriorityPair
    rulePriorityPair_priority,
    rulePriorityPair_ruleArn,

    -- ** SourceIpConditionConfig
    sourceIpConditionConfig_values,

    -- ** SslPolicy
    sslPolicy_supportedLoadBalancerTypes,
    sslPolicy_ciphers,
    sslPolicy_name,
    sslPolicy_sslProtocols,

    -- ** SubnetMapping
    subnetMapping_iPv6Address,
    subnetMapping_allocationId,
    subnetMapping_privateIPv4Address,
    subnetMapping_subnetId,

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
    targetGroup_protocolVersion,
    targetGroup_matcher,
    targetGroup_healthCheckPath,
    targetGroup_healthCheckEnabled,
    targetGroup_unhealthyThresholdCount,
    targetGroup_vpcId,
    targetGroup_targetGroupArn,
    targetGroup_protocol,
    targetGroup_healthCheckIntervalSeconds,
    targetGroup_targetType,
    targetGroup_healthyThresholdCount,
    targetGroup_healthCheckProtocol,
    targetGroup_loadBalancerArns,
    targetGroup_ipAddressType,
    targetGroup_healthCheckTimeoutSeconds,
    targetGroup_healthCheckPort,
    targetGroup_targetGroupName,
    targetGroup_port,

    -- ** TargetGroupAttribute
    targetGroupAttribute_value,
    targetGroupAttribute_key,

    -- ** TargetGroupStickinessConfig
    targetGroupStickinessConfig_enabled,
    targetGroupStickinessConfig_durationSeconds,

    -- ** TargetGroupTuple
    targetGroupTuple_weight,
    targetGroupTuple_targetGroupArn,

    -- ** TargetHealth
    targetHealth_state,
    targetHealth_reason,
    targetHealth_description,

    -- ** TargetHealthDescription
    targetHealthDescription_targetHealth,
    targetHealthDescription_healthCheckPort,
    targetHealthDescription_target,
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
