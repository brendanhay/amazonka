{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types
  ( -- * Service Configuration
    elb,

    -- * Errors

    -- * Re-exported Types
    module Network.AWS.ELB.Internal,

    -- * AccessLog
    AccessLog,
    accessLog,
    alEmitInterval,
    alS3BucketPrefix,
    alS3BucketName,
    alEnabled,

    -- * AdditionalAttribute
    AdditionalAttribute,
    additionalAttribute,
    aaValue,
    aaKey,

    -- * AppCookieStickinessPolicy
    AppCookieStickinessPolicy,
    appCookieStickinessPolicy,
    acspPolicyName,
    acspCookieName,

    -- * BackendServerDescription
    BackendServerDescription,
    backendServerDescription,
    bsdPolicyNames,
    bsdInstancePort,

    -- * ConnectionDraining
    ConnectionDraining,
    connectionDraining,
    cdTimeout,
    cdEnabled,

    -- * ConnectionSettings
    ConnectionSettings,
    connectionSettings,
    csIdleTimeout,

    -- * CrossZoneLoadBalancing
    CrossZoneLoadBalancing,
    crossZoneLoadBalancing,
    czlbEnabled,

    -- * HealthCheck
    HealthCheck,
    healthCheck,
    hcTarget,
    hcInterval,
    hcTimeout,
    hcUnhealthyThreshold,
    hcHealthyThreshold,

    -- * Instance
    Instance,
    instance',
    iInstanceId,

    -- * InstanceState
    InstanceState,
    instanceState,
    isInstanceId,
    isState,
    isReasonCode,
    isDescription,

    -- * LBCookieStickinessPolicy
    LBCookieStickinessPolicy,
    lBCookieStickinessPolicy,
    lbcspPolicyName,
    lbcspCookieExpirationPeriod,

    -- * Limit
    Limit,
    limit,
    lMax,
    lName,

    -- * Listener
    Listener,
    listener,
    lInstanceProtocol,
    lSSLCertificateId,
    lProtocol,
    lLoadBalancerPort,
    lInstancePort,

    -- * ListenerDescription
    ListenerDescription,
    listenerDescription,
    ldPolicyNames,
    ldListener,

    -- * LoadBalancerAttributes
    LoadBalancerAttributes,
    loadBalancerAttributes,
    lbaCrossZoneLoadBalancing,
    lbaAccessLog,
    lbaAdditionalAttributes,
    lbaConnectionSettings,
    lbaConnectionDraining,

    -- * LoadBalancerDescription
    LoadBalancerDescription,
    loadBalancerDescription,
    lbdSourceSecurityGroup,
    lbdCanonicalHostedZoneName,
    lbdSecurityGroups,
    lbdHealthCheck,
    lbdLoadBalancerName,
    lbdCreatedTime,
    lbdVPCId,
    lbdSubnets,
    lbdAvailabilityZones,
    lbdBackendServerDescriptions,
    lbdCanonicalHostedZoneNameId,
    lbdInstances,
    lbdScheme,
    lbdListenerDescriptions,
    lbdDNSName,
    lbdPolicies,

    -- * Policies
    Policies,
    policies,
    pOtherPolicies,
    pLBCookieStickinessPolicies,
    pAppCookieStickinessPolicies,

    -- * PolicyAttribute
    PolicyAttribute,
    policyAttribute,
    paAttributeValue,
    paAttributeName,

    -- * PolicyAttributeDescription
    PolicyAttributeDescription,
    policyAttributeDescription,
    padAttributeValue,
    padAttributeName,

    -- * PolicyAttributeTypeDescription
    PolicyAttributeTypeDescription,
    policyAttributeTypeDescription,
    patdAttributeType,
    patdCardinality,
    patdDefaultValue,
    patdAttributeName,
    patdDescription,

    -- * PolicyDescription
    PolicyDescription,
    policyDescription,
    pdPolicyName,
    pdPolicyAttributeDescriptions,
    pdPolicyTypeName,

    -- * PolicyTypeDescription
    PolicyTypeDescription,
    policyTypeDescription,
    ptdPolicyTypeName,
    ptdDescription,
    ptdPolicyAttributeTypeDescriptions,

    -- * SourceSecurityGroup
    SourceSecurityGroup,
    sourceSecurityGroup,
    ssgOwnerAlias,
    ssgGroupName,

    -- * Tag
    Tag,
    tag,
    tagValue,
    tagKey,

    -- * TagDescription
    TagDescription,
    tagDescription,
    tdLoadBalancerName,
    tdTags,

    -- * TagKeyOnly
    TagKeyOnly,
    tagKeyOnly,
    tkoKey,
  )
where

import Network.AWS.ELB.Internal
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2012-06-01@ of the Amazon Elastic Load Balancing SDK configuration.
elb :: Service
elb =
  Service
    { _svcAbbrev = "ELB",
      _svcSigner = v4,
      _svcPrefix = "elasticloadbalancing",
      _svcVersion = "2012-06-01",
      _svcEndpoint = defaultEndpoint elb,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseXMLError "ELB",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
