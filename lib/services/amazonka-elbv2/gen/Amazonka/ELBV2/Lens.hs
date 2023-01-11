{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ELBV2.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Lens
  ( -- * Operations

    -- ** AddListenerCertificates
    addListenerCertificates_listenerArn,
    addListenerCertificates_certificates,
    addListenerCertificatesResponse_certificates,
    addListenerCertificatesResponse_httpStatus,

    -- ** AddTags
    addTags_resourceArns,
    addTags_tags,
    addTagsResponse_httpStatus,

    -- ** CreateListener
    createListener_alpnPolicy,
    createListener_certificates,
    createListener_port,
    createListener_protocol,
    createListener_sslPolicy,
    createListener_tags,
    createListener_loadBalancerArn,
    createListener_defaultActions,
    createListenerResponse_listeners,
    createListenerResponse_httpStatus,

    -- ** CreateLoadBalancer
    createLoadBalancer_customerOwnedIpv4Pool,
    createLoadBalancer_ipAddressType,
    createLoadBalancer_scheme,
    createLoadBalancer_securityGroups,
    createLoadBalancer_subnetMappings,
    createLoadBalancer_subnets,
    createLoadBalancer_tags,
    createLoadBalancer_type,
    createLoadBalancer_name,
    createLoadBalancerResponse_loadBalancers,
    createLoadBalancerResponse_httpStatus,

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
    createTargetGroup_healthCheckIntervalSeconds,
    createTargetGroup_healthCheckPath,
    createTargetGroup_healthCheckPort,
    createTargetGroup_healthCheckProtocol,
    createTargetGroup_healthCheckTimeoutSeconds,
    createTargetGroup_healthyThresholdCount,
    createTargetGroup_ipAddressType,
    createTargetGroup_matcher,
    createTargetGroup_port,
    createTargetGroup_protocol,
    createTargetGroup_protocolVersion,
    createTargetGroup_tags,
    createTargetGroup_targetType,
    createTargetGroup_unhealthyThresholdCount,
    createTargetGroup_vpcId,
    createTargetGroup_name,
    createTargetGroupResponse_targetGroups,
    createTargetGroupResponse_httpStatus,

    -- ** DeleteListener
    deleteListener_listenerArn,
    deleteListenerResponse_httpStatus,

    -- ** DeleteLoadBalancer
    deleteLoadBalancer_loadBalancerArn,
    deleteLoadBalancerResponse_httpStatus,

    -- ** DeleteRule
    deleteRule_ruleArn,
    deleteRuleResponse_httpStatus,

    -- ** DeleteTargetGroup
    deleteTargetGroup_targetGroupArn,
    deleteTargetGroupResponse_httpStatus,

    -- ** DeregisterTargets
    deregisterTargets_targetGroupArn,
    deregisterTargets_targets,
    deregisterTargetsResponse_httpStatus,

    -- ** DescribeAccountLimits
    describeAccountLimits_marker,
    describeAccountLimits_pageSize,
    describeAccountLimitsResponse_limits,
    describeAccountLimitsResponse_nextMarker,
    describeAccountLimitsResponse_httpStatus,

    -- ** DescribeListenerCertificates
    describeListenerCertificates_marker,
    describeListenerCertificates_pageSize,
    describeListenerCertificates_listenerArn,
    describeListenerCertificatesResponse_certificates,
    describeListenerCertificatesResponse_nextMarker,
    describeListenerCertificatesResponse_httpStatus,

    -- ** DescribeListeners
    describeListeners_listenerArns,
    describeListeners_loadBalancerArn,
    describeListeners_marker,
    describeListeners_pageSize,
    describeListenersResponse_listeners,
    describeListenersResponse_nextMarker,
    describeListenersResponse_httpStatus,

    -- ** DescribeLoadBalancerAttributes
    describeLoadBalancerAttributes_loadBalancerArn,
    describeLoadBalancerAttributesResponse_attributes,
    describeLoadBalancerAttributesResponse_httpStatus,

    -- ** DescribeLoadBalancers
    describeLoadBalancers_loadBalancerArns,
    describeLoadBalancers_marker,
    describeLoadBalancers_names,
    describeLoadBalancers_pageSize,
    describeLoadBalancersResponse_loadBalancers,
    describeLoadBalancersResponse_nextMarker,
    describeLoadBalancersResponse_httpStatus,

    -- ** DescribeRules
    describeRules_listenerArn,
    describeRules_marker,
    describeRules_pageSize,
    describeRules_ruleArns,
    describeRulesResponse_nextMarker,
    describeRulesResponse_rules,
    describeRulesResponse_httpStatus,

    -- ** DescribeSSLPolicies
    describeSSLPolicies_loadBalancerType,
    describeSSLPolicies_marker,
    describeSSLPolicies_names,
    describeSSLPolicies_pageSize,
    describeSSLPoliciesResponse_nextMarker,
    describeSSLPoliciesResponse_sslPolicies,
    describeSSLPoliciesResponse_httpStatus,

    -- ** DescribeTags
    describeTags_resourceArns,
    describeTagsResponse_tagDescriptions,
    describeTagsResponse_httpStatus,

    -- ** DescribeTargetGroupAttributes
    describeTargetGroupAttributes_targetGroupArn,
    describeTargetGroupAttributesResponse_attributes,
    describeTargetGroupAttributesResponse_httpStatus,

    -- ** DescribeTargetGroups
    describeTargetGroups_loadBalancerArn,
    describeTargetGroups_marker,
    describeTargetGroups_names,
    describeTargetGroups_pageSize,
    describeTargetGroups_targetGroupArns,
    describeTargetGroupsResponse_nextMarker,
    describeTargetGroupsResponse_targetGroups,
    describeTargetGroupsResponse_httpStatus,

    -- ** DescribeTargetHealth
    describeTargetHealth_targets,
    describeTargetHealth_targetGroupArn,
    describeTargetHealthResponse_targetHealthDescriptions,
    describeTargetHealthResponse_httpStatus,

    -- ** ModifyListener
    modifyListener_alpnPolicy,
    modifyListener_certificates,
    modifyListener_defaultActions,
    modifyListener_port,
    modifyListener_protocol,
    modifyListener_sslPolicy,
    modifyListener_listenerArn,
    modifyListenerResponse_listeners,
    modifyListenerResponse_httpStatus,

    -- ** ModifyLoadBalancerAttributes
    modifyLoadBalancerAttributes_loadBalancerArn,
    modifyLoadBalancerAttributes_attributes,
    modifyLoadBalancerAttributesResponse_attributes,
    modifyLoadBalancerAttributesResponse_httpStatus,

    -- ** ModifyRule
    modifyRule_actions,
    modifyRule_conditions,
    modifyRule_ruleArn,
    modifyRuleResponse_rules,
    modifyRuleResponse_httpStatus,

    -- ** ModifyTargetGroup
    modifyTargetGroup_healthCheckEnabled,
    modifyTargetGroup_healthCheckIntervalSeconds,
    modifyTargetGroup_healthCheckPath,
    modifyTargetGroup_healthCheckPort,
    modifyTargetGroup_healthCheckProtocol,
    modifyTargetGroup_healthCheckTimeoutSeconds,
    modifyTargetGroup_healthyThresholdCount,
    modifyTargetGroup_matcher,
    modifyTargetGroup_unhealthyThresholdCount,
    modifyTargetGroup_targetGroupArn,
    modifyTargetGroupResponse_targetGroups,
    modifyTargetGroupResponse_httpStatus,

    -- ** ModifyTargetGroupAttributes
    modifyTargetGroupAttributes_targetGroupArn,
    modifyTargetGroupAttributes_attributes,
    modifyTargetGroupAttributesResponse_attributes,
    modifyTargetGroupAttributesResponse_httpStatus,

    -- ** RegisterTargets
    registerTargets_targetGroupArn,
    registerTargets_targets,
    registerTargetsResponse_httpStatus,

    -- ** RemoveListenerCertificates
    removeListenerCertificates_listenerArn,
    removeListenerCertificates_certificates,
    removeListenerCertificatesResponse_httpStatus,

    -- ** RemoveTags
    removeTags_resourceArns,
    removeTags_tagKeys,
    removeTagsResponse_httpStatus,

    -- ** SetIpAddressType
    setIpAddressType_loadBalancerArn,
    setIpAddressType_ipAddressType,
    setIpAddressTypeResponse_ipAddressType,
    setIpAddressTypeResponse_httpStatus,

    -- ** SetRulePriorities
    setRulePriorities_rulePriorities,
    setRulePrioritiesResponse_rules,
    setRulePrioritiesResponse_httpStatus,

    -- ** SetSecurityGroups
    setSecurityGroups_loadBalancerArn,
    setSecurityGroups_securityGroups,
    setSecurityGroupsResponse_securityGroupIds,
    setSecurityGroupsResponse_httpStatus,

    -- ** SetSubnets
    setSubnets_ipAddressType,
    setSubnets_subnetMappings,
    setSubnets_subnets,
    setSubnets_loadBalancerArn,
    setSubnetsResponse_availabilityZones,
    setSubnetsResponse_ipAddressType,
    setSubnetsResponse_httpStatus,

    -- * Types

    -- ** Action
    action_authenticateCognitoConfig,
    action_authenticateOidcConfig,
    action_fixedResponseConfig,
    action_forwardConfig,
    action_order,
    action_redirectConfig,
    action_targetGroupArn,
    action_type,

    -- ** AuthenticateCognitoActionConfig
    authenticateCognitoActionConfig_authenticationRequestExtraParams,
    authenticateCognitoActionConfig_onUnauthenticatedRequest,
    authenticateCognitoActionConfig_scope,
    authenticateCognitoActionConfig_sessionCookieName,
    authenticateCognitoActionConfig_sessionTimeout,
    authenticateCognitoActionConfig_userPoolArn,
    authenticateCognitoActionConfig_userPoolClientId,
    authenticateCognitoActionConfig_userPoolDomain,

    -- ** AuthenticateOidcActionConfig
    authenticateOidcActionConfig_authenticationRequestExtraParams,
    authenticateOidcActionConfig_clientSecret,
    authenticateOidcActionConfig_onUnauthenticatedRequest,
    authenticateOidcActionConfig_scope,
    authenticateOidcActionConfig_sessionCookieName,
    authenticateOidcActionConfig_sessionTimeout,
    authenticateOidcActionConfig_useExistingClientSecret,
    authenticateOidcActionConfig_issuer,
    authenticateOidcActionConfig_authorizationEndpoint,
    authenticateOidcActionConfig_tokenEndpoint,
    authenticateOidcActionConfig_userInfoEndpoint,
    authenticateOidcActionConfig_clientId,

    -- ** AvailabilityZone
    availabilityZone_loadBalancerAddresses,
    availabilityZone_outpostId,
    availabilityZone_subnetId,
    availabilityZone_zoneName,

    -- ** Certificate
    certificate_certificateArn,
    certificate_isDefault,

    -- ** Cipher
    cipher_name,
    cipher_priority,

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
    httpHeaderConditionConfig_httpHeaderName,
    httpHeaderConditionConfig_values,

    -- ** HttpRequestMethodConditionConfig
    httpRequestMethodConditionConfig_values,

    -- ** Limit
    limit_max,
    limit_name,

    -- ** Listener
    listener_alpnPolicy,
    listener_certificates,
    listener_defaultActions,
    listener_listenerArn,
    listener_loadBalancerArn,
    listener_port,
    listener_protocol,
    listener_sslPolicy,

    -- ** LoadBalancer
    loadBalancer_availabilityZones,
    loadBalancer_canonicalHostedZoneId,
    loadBalancer_createdTime,
    loadBalancer_customerOwnedIpv4Pool,
    loadBalancer_dNSName,
    loadBalancer_ipAddressType,
    loadBalancer_loadBalancerArn,
    loadBalancer_loadBalancerName,
    loadBalancer_scheme,
    loadBalancer_securityGroups,
    loadBalancer_state,
    loadBalancer_type,
    loadBalancer_vpcId,

    -- ** LoadBalancerAddress
    loadBalancerAddress_allocationId,
    loadBalancerAddress_iPv6Address,
    loadBalancerAddress_ipAddress,
    loadBalancerAddress_privateIPv4Address,

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
    redirectActionConfig_host,
    redirectActionConfig_path,
    redirectActionConfig_port,
    redirectActionConfig_protocol,
    redirectActionConfig_query,
    redirectActionConfig_statusCode,

    -- ** Rule
    rule_actions,
    rule_conditions,
    rule_isDefault,
    rule_priority,
    rule_ruleArn,

    -- ** RuleCondition
    ruleCondition_field,
    ruleCondition_hostHeaderConfig,
    ruleCondition_httpHeaderConfig,
    ruleCondition_httpRequestMethodConfig,
    ruleCondition_pathPatternConfig,
    ruleCondition_queryStringConfig,
    ruleCondition_sourceIpConfig,
    ruleCondition_values,

    -- ** RulePriorityPair
    rulePriorityPair_priority,
    rulePriorityPair_ruleArn,

    -- ** SourceIpConditionConfig
    sourceIpConditionConfig_values,

    -- ** SslPolicy
    sslPolicy_ciphers,
    sslPolicy_name,
    sslPolicy_sslProtocols,
    sslPolicy_supportedLoadBalancerTypes,

    -- ** SubnetMapping
    subnetMapping_allocationId,
    subnetMapping_iPv6Address,
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
    targetGroup_healthCheckEnabled,
    targetGroup_healthCheckIntervalSeconds,
    targetGroup_healthCheckPath,
    targetGroup_healthCheckPort,
    targetGroup_healthCheckProtocol,
    targetGroup_healthCheckTimeoutSeconds,
    targetGroup_healthyThresholdCount,
    targetGroup_ipAddressType,
    targetGroup_loadBalancerArns,
    targetGroup_matcher,
    targetGroup_port,
    targetGroup_protocol,
    targetGroup_protocolVersion,
    targetGroup_targetGroupArn,
    targetGroup_targetGroupName,
    targetGroup_targetType,
    targetGroup_unhealthyThresholdCount,
    targetGroup_vpcId,

    -- ** TargetGroupAttribute
    targetGroupAttribute_key,
    targetGroupAttribute_value,

    -- ** TargetGroupStickinessConfig
    targetGroupStickinessConfig_durationSeconds,
    targetGroupStickinessConfig_enabled,

    -- ** TargetGroupTuple
    targetGroupTuple_targetGroupArn,
    targetGroupTuple_weight,

    -- ** TargetHealth
    targetHealth_description,
    targetHealth_reason,
    targetHealth_state,

    -- ** TargetHealthDescription
    targetHealthDescription_healthCheckPort,
    targetHealthDescription_target,
    targetHealthDescription_targetHealth,
  )
where

import Amazonka.ELBV2.AddListenerCertificates
import Amazonka.ELBV2.AddTags
import Amazonka.ELBV2.CreateListener
import Amazonka.ELBV2.CreateLoadBalancer
import Amazonka.ELBV2.CreateRule
import Amazonka.ELBV2.CreateTargetGroup
import Amazonka.ELBV2.DeleteListener
import Amazonka.ELBV2.DeleteLoadBalancer
import Amazonka.ELBV2.DeleteRule
import Amazonka.ELBV2.DeleteTargetGroup
import Amazonka.ELBV2.DeregisterTargets
import Amazonka.ELBV2.DescribeAccountLimits
import Amazonka.ELBV2.DescribeListenerCertificates
import Amazonka.ELBV2.DescribeListeners
import Amazonka.ELBV2.DescribeLoadBalancerAttributes
import Amazonka.ELBV2.DescribeLoadBalancers
import Amazonka.ELBV2.DescribeRules
import Amazonka.ELBV2.DescribeSSLPolicies
import Amazonka.ELBV2.DescribeTags
import Amazonka.ELBV2.DescribeTargetGroupAttributes
import Amazonka.ELBV2.DescribeTargetGroups
import Amazonka.ELBV2.DescribeTargetHealth
import Amazonka.ELBV2.ModifyListener
import Amazonka.ELBV2.ModifyLoadBalancerAttributes
import Amazonka.ELBV2.ModifyRule
import Amazonka.ELBV2.ModifyTargetGroup
import Amazonka.ELBV2.ModifyTargetGroupAttributes
import Amazonka.ELBV2.RegisterTargets
import Amazonka.ELBV2.RemoveListenerCertificates
import Amazonka.ELBV2.RemoveTags
import Amazonka.ELBV2.SetIpAddressType
import Amazonka.ELBV2.SetRulePriorities
import Amazonka.ELBV2.SetSecurityGroups
import Amazonka.ELBV2.SetSubnets
import Amazonka.ELBV2.Types.Action
import Amazonka.ELBV2.Types.AuthenticateCognitoActionConfig
import Amazonka.ELBV2.Types.AuthenticateOidcActionConfig
import Amazonka.ELBV2.Types.AvailabilityZone
import Amazonka.ELBV2.Types.Certificate
import Amazonka.ELBV2.Types.Cipher
import Amazonka.ELBV2.Types.FixedResponseActionConfig
import Amazonka.ELBV2.Types.ForwardActionConfig
import Amazonka.ELBV2.Types.HostHeaderConditionConfig
import Amazonka.ELBV2.Types.HttpHeaderConditionConfig
import Amazonka.ELBV2.Types.HttpRequestMethodConditionConfig
import Amazonka.ELBV2.Types.Limit
import Amazonka.ELBV2.Types.Listener
import Amazonka.ELBV2.Types.LoadBalancer
import Amazonka.ELBV2.Types.LoadBalancerAddress
import Amazonka.ELBV2.Types.LoadBalancerAttribute
import Amazonka.ELBV2.Types.LoadBalancerState
import Amazonka.ELBV2.Types.Matcher
import Amazonka.ELBV2.Types.PathPatternConditionConfig
import Amazonka.ELBV2.Types.QueryStringConditionConfig
import Amazonka.ELBV2.Types.QueryStringKeyValuePair
import Amazonka.ELBV2.Types.RedirectActionConfig
import Amazonka.ELBV2.Types.Rule
import Amazonka.ELBV2.Types.RuleCondition
import Amazonka.ELBV2.Types.RulePriorityPair
import Amazonka.ELBV2.Types.SourceIpConditionConfig
import Amazonka.ELBV2.Types.SslPolicy
import Amazonka.ELBV2.Types.SubnetMapping
import Amazonka.ELBV2.Types.Tag
import Amazonka.ELBV2.Types.TagDescription
import Amazonka.ELBV2.Types.TargetDescription
import Amazonka.ELBV2.Types.TargetGroup
import Amazonka.ELBV2.Types.TargetGroupAttribute
import Amazonka.ELBV2.Types.TargetGroupStickinessConfig
import Amazonka.ELBV2.Types.TargetGroupTuple
import Amazonka.ELBV2.Types.TargetHealth
import Amazonka.ELBV2.Types.TargetHealthDescription
