{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Lens
  ( -- * Operations

    -- ** DescribeLoadBalancers
    describeLoadBalancers_marker,
    describeLoadBalancers_pageSize,
    describeLoadBalancers_loadBalancerNames,
    describeLoadBalancersResponse_loadBalancerDescriptions,
    describeLoadBalancersResponse_nextMarker,
    describeLoadBalancersResponse_httpStatus,

    -- ** DescribeTags
    describeTags_loadBalancerNames,
    describeTagsResponse_tagDescriptions,
    describeTagsResponse_httpStatus,

    -- ** DescribeLoadBalancerPolicyTypes
    describeLoadBalancerPolicyTypes_policyTypeNames,
    describeLoadBalancerPolicyTypesResponse_policyTypeDescriptions,
    describeLoadBalancerPolicyTypesResponse_httpStatus,

    -- ** ApplySecurityGroupsToLoadBalancer
    applySecurityGroupsToLoadBalancer_loadBalancerName,
    applySecurityGroupsToLoadBalancer_securityGroups,
    applySecurityGroupsToLoadBalancerResponse_securityGroups,
    applySecurityGroupsToLoadBalancerResponse_httpStatus,

    -- ** RemoveTags
    removeTags_loadBalancerNames,
    removeTags_tags,
    removeTagsResponse_httpStatus,

    -- ** CreateLBCookieStickinessPolicy
    createLBCookieStickinessPolicy_cookieExpirationPeriod,
    createLBCookieStickinessPolicy_loadBalancerName,
    createLBCookieStickinessPolicy_policyName,
    createLBCookieStickinessPolicyResponse_httpStatus,

    -- ** DeleteLoadBalancer
    deleteLoadBalancer_loadBalancerName,
    deleteLoadBalancerResponse_httpStatus,

    -- ** DeregisterInstancesFromLoadBalancer
    deregisterInstancesFromLoadBalancer_loadBalancerName,
    deregisterInstancesFromLoadBalancer_instances,
    deregisterInstancesFromLoadBalancerResponse_instances,
    deregisterInstancesFromLoadBalancerResponse_httpStatus,

    -- ** CreateLoadBalancerPolicy
    createLoadBalancerPolicy_policyAttributes,
    createLoadBalancerPolicy_loadBalancerName,
    createLoadBalancerPolicy_policyName,
    createLoadBalancerPolicy_policyTypeName,
    createLoadBalancerPolicyResponse_httpStatus,

    -- ** DescribeLoadBalancerPolicies
    describeLoadBalancerPolicies_policyNames,
    describeLoadBalancerPolicies_loadBalancerName,
    describeLoadBalancerPoliciesResponse_policyDescriptions,
    describeLoadBalancerPoliciesResponse_httpStatus,

    -- ** DisableAvailabilityZonesForLoadBalancer
    disableAvailabilityZonesForLoadBalancer_loadBalancerName,
    disableAvailabilityZonesForLoadBalancer_availabilityZones,
    disableAvailabilityZonesForLoadBalancerResponse_availabilityZones,
    disableAvailabilityZonesForLoadBalancerResponse_httpStatus,

    -- ** EnableAvailabilityZonesForLoadBalancer
    enableAvailabilityZonesForLoadBalancer_loadBalancerName,
    enableAvailabilityZonesForLoadBalancer_availabilityZones,
    enableAvailabilityZonesForLoadBalancerResponse_availabilityZones,
    enableAvailabilityZonesForLoadBalancerResponse_httpStatus,

    -- ** SetLoadBalancerPoliciesForBackendServer
    setLoadBalancerPoliciesForBackendServer_loadBalancerName,
    setLoadBalancerPoliciesForBackendServer_instancePort,
    setLoadBalancerPoliciesForBackendServer_policyNames,
    setLoadBalancerPoliciesForBackendServerResponse_httpStatus,

    -- ** SetLoadBalancerListenerSSLCertificate
    setLoadBalancerListenerSSLCertificate_loadBalancerName,
    setLoadBalancerListenerSSLCertificate_loadBalancerPort,
    setLoadBalancerListenerSSLCertificate_sSLCertificateId,
    setLoadBalancerListenerSSLCertificateResponse_httpStatus,

    -- ** DescribeAccountLimits
    describeAccountLimits_marker,
    describeAccountLimits_pageSize,
    describeAccountLimitsResponse_limits,
    describeAccountLimitsResponse_nextMarker,
    describeAccountLimitsResponse_httpStatus,

    -- ** AttachLoadBalancerToSubnets
    attachLoadBalancerToSubnets_loadBalancerName,
    attachLoadBalancerToSubnets_subnets,
    attachLoadBalancerToSubnetsResponse_subnets,
    attachLoadBalancerToSubnetsResponse_httpStatus,

    -- ** ConfigureHealthCheck
    configureHealthCheck_loadBalancerName,
    configureHealthCheck_healthCheck,
    configureHealthCheckResponse_healthCheck,
    configureHealthCheckResponse_httpStatus,

    -- ** ModifyLoadBalancerAttributes
    modifyLoadBalancerAttributes_loadBalancerName,
    modifyLoadBalancerAttributes_loadBalancerAttributes,
    modifyLoadBalancerAttributesResponse_loadBalancerName,
    modifyLoadBalancerAttributesResponse_loadBalancerAttributes,
    modifyLoadBalancerAttributesResponse_httpStatus,

    -- ** CreateAppCookieStickinessPolicy
    createAppCookieStickinessPolicy_loadBalancerName,
    createAppCookieStickinessPolicy_policyName,
    createAppCookieStickinessPolicy_cookieName,
    createAppCookieStickinessPolicyResponse_httpStatus,

    -- ** DescribeInstanceHealth
    describeInstanceHealth_instances,
    describeInstanceHealth_loadBalancerName,
    describeInstanceHealthResponse_instanceStates,
    describeInstanceHealthResponse_httpStatus,

    -- ** AddTags
    addTags_loadBalancerNames,
    addTags_tags,
    addTagsResponse_httpStatus,

    -- ** DescribeLoadBalancerAttributes
    describeLoadBalancerAttributes_loadBalancerName,
    describeLoadBalancerAttributesResponse_loadBalancerAttributes,
    describeLoadBalancerAttributesResponse_httpStatus,

    -- ** CreateLoadBalancerListeners
    createLoadBalancerListeners_loadBalancerName,
    createLoadBalancerListeners_listeners,
    createLoadBalancerListenersResponse_httpStatus,

    -- ** DeleteLoadBalancerPolicy
    deleteLoadBalancerPolicy_loadBalancerName,
    deleteLoadBalancerPolicy_policyName,
    deleteLoadBalancerPolicyResponse_httpStatus,

    -- ** DetachLoadBalancerFromSubnets
    detachLoadBalancerFromSubnets_loadBalancerName,
    detachLoadBalancerFromSubnets_subnets,
    detachLoadBalancerFromSubnetsResponse_subnets,
    detachLoadBalancerFromSubnetsResponse_httpStatus,

    -- ** RegisterInstancesWithLoadBalancer
    registerInstancesWithLoadBalancer_loadBalancerName,
    registerInstancesWithLoadBalancer_instances,
    registerInstancesWithLoadBalancerResponse_instances,
    registerInstancesWithLoadBalancerResponse_httpStatus,

    -- ** CreateLoadBalancer
    createLoadBalancer_securityGroups,
    createLoadBalancer_subnets,
    createLoadBalancer_availabilityZones,
    createLoadBalancer_scheme,
    createLoadBalancer_tags,
    createLoadBalancer_loadBalancerName,
    createLoadBalancer_listeners,
    createLoadBalancerResponse_dNSName,
    createLoadBalancerResponse_httpStatus,

    -- ** DeleteLoadBalancerListeners
    deleteLoadBalancerListeners_loadBalancerName,
    deleteLoadBalancerListeners_loadBalancerPorts,
    deleteLoadBalancerListenersResponse_httpStatus,

    -- ** SetLoadBalancerPoliciesOfListener
    setLoadBalancerPoliciesOfListener_loadBalancerName,
    setLoadBalancerPoliciesOfListener_loadBalancerPort,
    setLoadBalancerPoliciesOfListener_policyNames,
    setLoadBalancerPoliciesOfListenerResponse_httpStatus,

    -- * Types

    -- ** AccessLog
    accessLog_emitInterval,
    accessLog_s3BucketPrefix,
    accessLog_s3BucketName,
    accessLog_enabled,

    -- ** AdditionalAttribute
    additionalAttribute_value,
    additionalAttribute_key,

    -- ** AppCookieStickinessPolicy
    appCookieStickinessPolicy_policyName,
    appCookieStickinessPolicy_cookieName,

    -- ** BackendServerDescription
    backendServerDescription_policyNames,
    backendServerDescription_instancePort,

    -- ** ConnectionDraining
    connectionDraining_timeout,
    connectionDraining_enabled,

    -- ** ConnectionSettings
    connectionSettings_idleTimeout,

    -- ** CrossZoneLoadBalancing
    crossZoneLoadBalancing_enabled,

    -- ** HealthCheck
    healthCheck_target,
    healthCheck_interval,
    healthCheck_timeout,
    healthCheck_unhealthyThreshold,
    healthCheck_healthyThreshold,

    -- ** Instance
    instance_instanceId,

    -- ** InstanceState
    instanceState_instanceId,
    instanceState_state,
    instanceState_reasonCode,
    instanceState_description,

    -- ** LBCookieStickinessPolicy
    lBCookieStickinessPolicy_policyName,
    lBCookieStickinessPolicy_cookieExpirationPeriod,

    -- ** Limit
    limit_max,
    limit_name,

    -- ** Listener
    listener_instanceProtocol,
    listener_sSLCertificateId,
    listener_protocol,
    listener_loadBalancerPort,
    listener_instancePort,

    -- ** ListenerDescription
    listenerDescription_policyNames,
    listenerDescription_listener,

    -- ** LoadBalancerAttributes
    loadBalancerAttributes_crossZoneLoadBalancing,
    loadBalancerAttributes_accessLog,
    loadBalancerAttributes_additionalAttributes,
    loadBalancerAttributes_connectionSettings,
    loadBalancerAttributes_connectionDraining,

    -- ** LoadBalancerDescription
    loadBalancerDescription_sourceSecurityGroup,
    loadBalancerDescription_canonicalHostedZoneName,
    loadBalancerDescription_securityGroups,
    loadBalancerDescription_healthCheck,
    loadBalancerDescription_loadBalancerName,
    loadBalancerDescription_createdTime,
    loadBalancerDescription_vPCId,
    loadBalancerDescription_subnets,
    loadBalancerDescription_availabilityZones,
    loadBalancerDescription_backendServerDescriptions,
    loadBalancerDescription_canonicalHostedZoneNameID,
    loadBalancerDescription_instances,
    loadBalancerDescription_scheme,
    loadBalancerDescription_listenerDescriptions,
    loadBalancerDescription_dNSName,
    loadBalancerDescription_policies,

    -- ** Policies
    policies_otherPolicies,
    policies_lBCookieStickinessPolicies,
    policies_appCookieStickinessPolicies,

    -- ** PolicyAttribute
    policyAttribute_attributeValue,
    policyAttribute_attributeName,

    -- ** PolicyAttributeDescription
    policyAttributeDescription_attributeValue,
    policyAttributeDescription_attributeName,

    -- ** PolicyAttributeTypeDescription
    policyAttributeTypeDescription_attributeType,
    policyAttributeTypeDescription_cardinality,
    policyAttributeTypeDescription_defaultValue,
    policyAttributeTypeDescription_attributeName,
    policyAttributeTypeDescription_description,

    -- ** PolicyDescription
    policyDescription_policyName,
    policyDescription_policyAttributeDescriptions,
    policyDescription_policyTypeName,

    -- ** PolicyTypeDescription
    policyTypeDescription_policyTypeName,
    policyTypeDescription_description,
    policyTypeDescription_policyAttributeTypeDescriptions,

    -- ** SourceSecurityGroup
    sourceSecurityGroup_ownerAlias,
    sourceSecurityGroup_groupName,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** TagDescription
    tagDescription_loadBalancerName,
    tagDescription_tags,

    -- ** TagKeyOnly
    tagKeyOnly_key,
  )
where

import Network.AWS.ELB.AddTags
import Network.AWS.ELB.ApplySecurityGroupsToLoadBalancer
import Network.AWS.ELB.AttachLoadBalancerToSubnets
import Network.AWS.ELB.ConfigureHealthCheck
import Network.AWS.ELB.CreateAppCookieStickinessPolicy
import Network.AWS.ELB.CreateLBCookieStickinessPolicy
import Network.AWS.ELB.CreateLoadBalancer
import Network.AWS.ELB.CreateLoadBalancerListeners
import Network.AWS.ELB.CreateLoadBalancerPolicy
import Network.AWS.ELB.DeleteLoadBalancer
import Network.AWS.ELB.DeleteLoadBalancerListeners
import Network.AWS.ELB.DeleteLoadBalancerPolicy
import Network.AWS.ELB.DeregisterInstancesFromLoadBalancer
import Network.AWS.ELB.DescribeAccountLimits
import Network.AWS.ELB.DescribeInstanceHealth
import Network.AWS.ELB.DescribeLoadBalancerAttributes
import Network.AWS.ELB.DescribeLoadBalancerPolicies
import Network.AWS.ELB.DescribeLoadBalancerPolicyTypes
import Network.AWS.ELB.DescribeLoadBalancers
import Network.AWS.ELB.DescribeTags
import Network.AWS.ELB.DetachLoadBalancerFromSubnets
import Network.AWS.ELB.DisableAvailabilityZonesForLoadBalancer
import Network.AWS.ELB.EnableAvailabilityZonesForLoadBalancer
import Network.AWS.ELB.ModifyLoadBalancerAttributes
import Network.AWS.ELB.RegisterInstancesWithLoadBalancer
import Network.AWS.ELB.RemoveTags
import Network.AWS.ELB.SetLoadBalancerListenerSSLCertificate
import Network.AWS.ELB.SetLoadBalancerPoliciesForBackendServer
import Network.AWS.ELB.SetLoadBalancerPoliciesOfListener
import Network.AWS.ELB.Types.AccessLog
import Network.AWS.ELB.Types.AdditionalAttribute
import Network.AWS.ELB.Types.AppCookieStickinessPolicy
import Network.AWS.ELB.Types.BackendServerDescription
import Network.AWS.ELB.Types.ConnectionDraining
import Network.AWS.ELB.Types.ConnectionSettings
import Network.AWS.ELB.Types.CrossZoneLoadBalancing
import Network.AWS.ELB.Types.HealthCheck
import Network.AWS.ELB.Types.Instance
import Network.AWS.ELB.Types.InstanceState
import Network.AWS.ELB.Types.LBCookieStickinessPolicy
import Network.AWS.ELB.Types.Limit
import Network.AWS.ELB.Types.Listener
import Network.AWS.ELB.Types.ListenerDescription
import Network.AWS.ELB.Types.LoadBalancerAttributes
import Network.AWS.ELB.Types.LoadBalancerDescription
import Network.AWS.ELB.Types.Policies
import Network.AWS.ELB.Types.PolicyAttribute
import Network.AWS.ELB.Types.PolicyAttributeDescription
import Network.AWS.ELB.Types.PolicyAttributeTypeDescription
import Network.AWS.ELB.Types.PolicyDescription
import Network.AWS.ELB.Types.PolicyTypeDescription
import Network.AWS.ELB.Types.SourceSecurityGroup
import Network.AWS.ELB.Types.Tag
import Network.AWS.ELB.Types.TagDescription
import Network.AWS.ELB.Types.TagKeyOnly
