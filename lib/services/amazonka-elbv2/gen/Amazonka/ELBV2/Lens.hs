{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ELBV2.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    createListener_tags,
    createListener_port,
    createListener_certificates,
    createListener_protocol,
    createListener_sslPolicy,
    createListener_alpnPolicy,
    createListener_loadBalancerArn,
    createListener_defaultActions,
    createListenerResponse_listeners,
    createListenerResponse_httpStatus,

    -- ** CreateLoadBalancer
    createLoadBalancer_tags,
    createLoadBalancer_scheme,
    createLoadBalancer_type,
    createLoadBalancer_subnets,
    createLoadBalancer_customerOwnedIpv4Pool,
    createLoadBalancer_securityGroups,
    createLoadBalancer_subnetMappings,
    createLoadBalancer_ipAddressType,
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
    createTargetGroup_healthCheckProtocol,
    createTargetGroup_tags,
    createTargetGroup_port,
    createTargetGroup_healthCheckTimeoutSeconds,
    createTargetGroup_healthCheckPath,
    createTargetGroup_unhealthyThresholdCount,
    createTargetGroup_healthCheckEnabled,
    createTargetGroup_healthCheckIntervalSeconds,
    createTargetGroup_healthyThresholdCount,
    createTargetGroup_targetType,
    createTargetGroup_protocolVersion,
    createTargetGroup_healthCheckPort,
    createTargetGroup_protocol,
    createTargetGroup_vpcId,
    createTargetGroup_ipAddressType,
    createTargetGroup_matcher,
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
    describeListeners_marker,
    describeListeners_loadBalancerArn,
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
    describeRules_marker,
    describeRules_listenerArn,
    describeRules_ruleArns,
    describeRules_pageSize,
    describeRulesResponse_rules,
    describeRulesResponse_nextMarker,
    describeRulesResponse_httpStatus,

    -- ** DescribeSSLPolicies
    describeSSLPolicies_marker,
    describeSSLPolicies_names,
    describeSSLPolicies_pageSize,
    describeSSLPolicies_loadBalancerType,
    describeSSLPoliciesResponse_sslPolicies,
    describeSSLPoliciesResponse_nextMarker,
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
    describeTargetGroups_marker,
    describeTargetGroups_loadBalancerArn,
    describeTargetGroups_names,
    describeTargetGroups_targetGroupArns,
    describeTargetGroups_pageSize,
    describeTargetGroupsResponse_nextMarker,
    describeTargetGroupsResponse_targetGroups,
    describeTargetGroupsResponse_httpStatus,

    -- ** DescribeTargetHealth
    describeTargetHealth_targets,
    describeTargetHealth_targetGroupArn,
    describeTargetHealthResponse_targetHealthDescriptions,
    describeTargetHealthResponse_httpStatus,

    -- ** ModifyListener
    modifyListener_port,
    modifyListener_certificates,
    modifyListener_defaultActions,
    modifyListener_protocol,
    modifyListener_sslPolicy,
    modifyListener_alpnPolicy,
    modifyListener_listenerArn,
    modifyListenerResponse_listeners,
    modifyListenerResponse_httpStatus,

    -- ** ModifyLoadBalancerAttributes
    modifyLoadBalancerAttributes_loadBalancerArn,
    modifyLoadBalancerAttributes_attributes,
    modifyLoadBalancerAttributesResponse_attributes,
    modifyLoadBalancerAttributesResponse_httpStatus,

    -- ** ModifyRule
    modifyRule_conditions,
    modifyRule_actions,
    modifyRule_ruleArn,
    modifyRuleResponse_rules,
    modifyRuleResponse_httpStatus,

    -- ** ModifyTargetGroup
    modifyTargetGroup_healthCheckProtocol,
    modifyTargetGroup_healthCheckTimeoutSeconds,
    modifyTargetGroup_healthCheckPath,
    modifyTargetGroup_unhealthyThresholdCount,
    modifyTargetGroup_healthCheckEnabled,
    modifyTargetGroup_healthCheckIntervalSeconds,
    modifyTargetGroup_healthyThresholdCount,
    modifyTargetGroup_healthCheckPort,
    modifyTargetGroup_matcher,
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
    setSubnets_subnets,
    setSubnets_subnetMappings,
    setSubnets_ipAddressType,
    setSubnets_loadBalancerArn,
    setSubnetsResponse_availabilityZones,
    setSubnetsResponse_ipAddressType,
    setSubnetsResponse_httpStatus,

    -- * Types

    -- ** Action
    action_forwardConfig,
    action_redirectConfig,
    action_targetGroupArn,
    action_order,
    action_fixedResponseConfig,
    action_authenticateCognitoConfig,
    action_authenticateOidcConfig,
    action_type,

    -- ** AuthenticateCognitoActionConfig
    authenticateCognitoActionConfig_sessionTimeout,
    authenticateCognitoActionConfig_onUnauthenticatedRequest,
    authenticateCognitoActionConfig_authenticationRequestExtraParams,
    authenticateCognitoActionConfig_scope,
    authenticateCognitoActionConfig_sessionCookieName,
    authenticateCognitoActionConfig_userPoolArn,
    authenticateCognitoActionConfig_userPoolClientId,
    authenticateCognitoActionConfig_userPoolDomain,

    -- ** AuthenticateOidcActionConfig
    authenticateOidcActionConfig_clientSecret,
    authenticateOidcActionConfig_useExistingClientSecret,
    authenticateOidcActionConfig_sessionTimeout,
    authenticateOidcActionConfig_onUnauthenticatedRequest,
    authenticateOidcActionConfig_authenticationRequestExtraParams,
    authenticateOidcActionConfig_scope,
    authenticateOidcActionConfig_sessionCookieName,
    authenticateOidcActionConfig_issuer,
    authenticateOidcActionConfig_authorizationEndpoint,
    authenticateOidcActionConfig_tokenEndpoint,
    authenticateOidcActionConfig_userInfoEndpoint,
    authenticateOidcActionConfig_clientId,

    -- ** AvailabilityZone
    availabilityZone_outpostId,
    availabilityZone_zoneName,
    availabilityZone_subnetId,
    availabilityZone_loadBalancerAddresses,

    -- ** Certificate
    certificate_certificateArn,
    certificate_isDefault,

    -- ** Cipher
    cipher_name,
    cipher_priority,

    -- ** FixedResponseActionConfig
    fixedResponseActionConfig_messageBody,
    fixedResponseActionConfig_contentType,
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
    limit_name,
    limit_max,

    -- ** Listener
    listener_port,
    listener_listenerArn,
    listener_certificates,
    listener_loadBalancerArn,
    listener_defaultActions,
    listener_protocol,
    listener_sslPolicy,
    listener_alpnPolicy,

    -- ** LoadBalancer
    loadBalancer_scheme,
    loadBalancer_type,
    loadBalancer_createdTime,
    loadBalancer_loadBalancerName,
    loadBalancer_availabilityZones,
    loadBalancer_state,
    loadBalancer_loadBalancerArn,
    loadBalancer_customerOwnedIpv4Pool,
    loadBalancer_securityGroups,
    loadBalancer_dNSName,
    loadBalancer_vpcId,
    loadBalancer_canonicalHostedZoneId,
    loadBalancer_ipAddressType,

    -- ** LoadBalancerAddress
    loadBalancerAddress_allocationId,
    loadBalancerAddress_iPv6Address,
    loadBalancerAddress_privateIPv4Address,
    loadBalancerAddress_ipAddress,

    -- ** LoadBalancerAttribute
    loadBalancerAttribute_key,
    loadBalancerAttribute_value,

    -- ** LoadBalancerState
    loadBalancerState_code,
    loadBalancerState_reason,

    -- ** Matcher
    matcher_httpCode,
    matcher_grpcCode,

    -- ** PathPatternConditionConfig
    pathPatternConditionConfig_values,

    -- ** QueryStringConditionConfig
    queryStringConditionConfig_values,

    -- ** QueryStringKeyValuePair
    queryStringKeyValuePair_key,
    queryStringKeyValuePair_value,

    -- ** RedirectActionConfig
    redirectActionConfig_port,
    redirectActionConfig_host,
    redirectActionConfig_path,
    redirectActionConfig_query,
    redirectActionConfig_protocol,
    redirectActionConfig_statusCode,

    -- ** Rule
    rule_ruleArn,
    rule_conditions,
    rule_isDefault,
    rule_priority,
    rule_actions,

    -- ** RuleCondition
    ruleCondition_httpHeaderConfig,
    ruleCondition_field,
    ruleCondition_sourceIpConfig,
    ruleCondition_pathPatternConfig,
    ruleCondition_httpRequestMethodConfig,
    ruleCondition_hostHeaderConfig,
    ruleCondition_values,
    ruleCondition_queryStringConfig,

    -- ** RulePriorityPair
    rulePriorityPair_ruleArn,
    rulePriorityPair_priority,

    -- ** SourceIpConditionConfig
    sourceIpConditionConfig_values,

    -- ** SslPolicy
    sslPolicy_name,
    sslPolicy_supportedLoadBalancerTypes,
    sslPolicy_ciphers,
    sslPolicy_sslProtocols,

    -- ** SubnetMapping
    subnetMapping_allocationId,
    subnetMapping_subnetId,
    subnetMapping_iPv6Address,
    subnetMapping_privateIPv4Address,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** TagDescription
    tagDescription_tags,
    tagDescription_resourceArn,

    -- ** TargetDescription
    targetDescription_port,
    targetDescription_availabilityZone,
    targetDescription_id,

    -- ** TargetGroup
    targetGroup_healthCheckProtocol,
    targetGroup_port,
    targetGroup_healthCheckTimeoutSeconds,
    targetGroup_loadBalancerArns,
    targetGroup_healthCheckPath,
    targetGroup_unhealthyThresholdCount,
    targetGroup_healthCheckEnabled,
    targetGroup_targetGroupName,
    targetGroup_healthCheckIntervalSeconds,
    targetGroup_healthyThresholdCount,
    targetGroup_targetGroupArn,
    targetGroup_targetType,
    targetGroup_protocolVersion,
    targetGroup_healthCheckPort,
    targetGroup_protocol,
    targetGroup_vpcId,
    targetGroup_ipAddressType,
    targetGroup_matcher,

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
    targetHealth_state,
    targetHealth_description,
    targetHealth_reason,

    -- ** TargetHealthDescription
    targetHealthDescription_targetHealth,
    targetHealthDescription_target,
    targetHealthDescription_healthCheckPort,
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
