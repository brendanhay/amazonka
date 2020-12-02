{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types
  ( -- * Service Configuration
    eLBv2,

    -- * Errors

    -- * ActionTypeEnum
    ActionTypeEnum (..),

    -- * AuthenticateCognitoActionConditionalBehaviorEnum
    AuthenticateCognitoActionConditionalBehaviorEnum (..),

    -- * AuthenticateOidcActionConditionalBehaviorEnum
    AuthenticateOidcActionConditionalBehaviorEnum (..),

    -- * IPAddressType
    IPAddressType (..),

    -- * LoadBalancerSchemeEnum
    LoadBalancerSchemeEnum (..),

    -- * LoadBalancerStateEnum
    LoadBalancerStateEnum (..),

    -- * LoadBalancerTypeEnum
    LoadBalancerTypeEnum (..),

    -- * ProtocolEnum
    ProtocolEnum (..),

    -- * RedirectActionStatusCodeEnum
    RedirectActionStatusCodeEnum (..),

    -- * TargetHealthReasonEnum
    TargetHealthReasonEnum (..),

    -- * TargetHealthStateEnum
    TargetHealthStateEnum (..),

    -- * TargetTypeEnum
    TargetTypeEnum (..),

    -- * Action
    Action,
    action,
    aFixedResponseConfig,
    aTargetGroupARN,
    aForwardConfig,
    aRedirectConfig,
    aAuthenticateCognitoConfig,
    aOrder,
    aAuthenticateOidcConfig,
    aType,

    -- * AuthenticateCognitoActionConfig
    AuthenticateCognitoActionConfig,
    authenticateCognitoActionConfig,
    acacAuthenticationRequestExtraParams,
    acacScope,
    acacOnUnauthenticatedRequest,
    acacSessionCookieName,
    acacSessionTimeout,
    acacUserPoolARN,
    acacUserPoolClientId,
    acacUserPoolDomain,

    -- * AuthenticateOidcActionConfig
    AuthenticateOidcActionConfig,
    authenticateOidcActionConfig,
    aoacClientSecret,
    aoacUseExistingClientSecret,
    aoacAuthenticationRequestExtraParams,
    aoacScope,
    aoacOnUnauthenticatedRequest,
    aoacSessionCookieName,
    aoacSessionTimeout,
    aoacIssuer,
    aoacAuthorizationEndpoint,
    aoacTokenEndpoint,
    aoacUserInfoEndpoint,
    aoacClientId,

    -- * AvailabilityZone
    AvailabilityZone,
    availabilityZone,
    azSubnetId,
    azZoneName,
    azLoadBalancerAddresses,
    azOutpostId,

    -- * Certificate
    Certificate,
    certificate,
    cCertificateARN,
    cIsDefault,

    -- * Cipher
    Cipher,
    cipher,
    cPriority,
    cName,

    -- * FixedResponseActionConfig
    FixedResponseActionConfig,
    fixedResponseActionConfig,
    fracMessageBody,
    fracContentType,
    fracStatusCode,

    -- * ForwardActionConfig
    ForwardActionConfig,
    forwardActionConfig,
    facTargetGroups,
    facTargetGroupStickinessConfig,

    -- * HTTPHeaderConditionConfig
    HTTPHeaderConditionConfig,
    hTTPHeaderConditionConfig,
    httphccValues,
    httphccHTTPHeaderName,

    -- * HTTPRequestMethodConditionConfig
    HTTPRequestMethodConditionConfig,
    hTTPRequestMethodConditionConfig,
    httprmccValues,

    -- * HostHeaderConditionConfig
    HostHeaderConditionConfig,
    hostHeaderConditionConfig,
    hhccValues,

    -- * Limit
    Limit,
    limit,
    lMax,
    lName,

    -- * Listener
    Listener,
    listener,
    lSSLPolicy,
    lListenerARN,
    lProtocol,
    lDefaultActions,
    lCertificates,
    lLoadBalancerARN,
    lAlpnPolicy,
    lPort,

    -- * LoadBalancer
    LoadBalancer,
    loadBalancer,
    lbState,
    lbSecurityGroups,
    lbLoadBalancerName,
    lbCreatedTime,
    lbVPCId,
    lbCanonicalHostedZoneId,
    lbAvailabilityZones,
    lbCustomerOwnedIPv4Pool,
    lbLoadBalancerARN,
    lbIPAddressType,
    lbScheme,
    lbType,
    lbDNSName,

    -- * LoadBalancerAddress
    LoadBalancerAddress,
    loadBalancerAddress,
    lbaIPv6Address,
    lbaIPAddress,
    lbaAllocationId,
    lbaPrivateIPv4Address,

    -- * LoadBalancerAttribute
    LoadBalancerAttribute,
    loadBalancerAttribute,
    lbaValue,
    lbaKey,

    -- * LoadBalancerState
    LoadBalancerState,
    loadBalancerState,
    lbsReason,
    lbsCode,

    -- * Matcher
    Matcher,
    matcher,
    mHTTPCode,
    mGrpcCode,

    -- * PathPatternConditionConfig
    PathPatternConditionConfig,
    pathPatternConditionConfig,
    ppccValues,

    -- * QueryStringConditionConfig
    QueryStringConditionConfig,
    queryStringConditionConfig,
    qsccValues,

    -- * QueryStringKeyValuePair
    QueryStringKeyValuePair,
    queryStringKeyValuePair,
    qskvpValue,
    qskvpKey,

    -- * RedirectActionConfig
    RedirectActionConfig,
    redirectActionConfig,
    racPath,
    racProtocol,
    racQuery,
    racHost,
    racPort,
    racStatusCode,

    -- * Rule
    Rule,
    rule,
    rPriority,
    rActions,
    rConditions,
    rRuleARN,
    rIsDefault,

    -- * RuleCondition
    RuleCondition,
    ruleCondition,
    rcField,
    rcHTTPHeaderConfig,
    rcHostHeaderConfig,
    rcValues,
    rcSourceIPConfig,
    rcHTTPRequestMethodConfig,
    rcPathPatternConfig,
    rcQueryStringConfig,

    -- * RulePriorityPair
    RulePriorityPair,
    rulePriorityPair,
    rppPriority,
    rppRuleARN,

    -- * SSLPolicy
    SSLPolicy,
    sslPolicy,
    spCiphers,
    spName,
    spSSLProtocols,

    -- * SourceIPConditionConfig
    SourceIPConditionConfig,
    sourceIPConditionConfig,
    siccValues,

    -- * SubnetMapping
    SubnetMapping,
    subnetMapping,
    smIPv6Address,
    smAllocationId,
    smPrivateIPv4Address,
    smSubnetId,

    -- * Tag
    Tag,
    tag,
    tagValue,
    tagKey,

    -- * TagDescription
    TagDescription,
    tagDescription,
    tdResourceARN,
    tdTags,

    -- * TargetDescription
    TargetDescription,
    targetDescription,
    tdAvailabilityZone,
    tdPort,
    tdId,

    -- * TargetGroup
    TargetGroup,
    targetGroup,
    tgProtocolVersion,
    tgMatcher,
    tgHealthCheckPath,
    tgHealthCheckEnabled,
    tgUnhealthyThresholdCount,
    tgVPCId,
    tgTargetGroupARN,
    tgProtocol,
    tgHealthCheckIntervalSeconds,
    tgTargetType,
    tgHealthyThresholdCount,
    tgHealthCheckProtocol,
    tgLoadBalancerARNs,
    tgHealthCheckTimeoutSeconds,
    tgHealthCheckPort,
    tgTargetGroupName,
    tgPort,

    -- * TargetGroupAttribute
    TargetGroupAttribute,
    targetGroupAttribute,
    tgaValue,
    tgaKey,

    -- * TargetGroupStickinessConfig
    TargetGroupStickinessConfig,
    targetGroupStickinessConfig,
    tgscEnabled,
    tgscDurationSeconds,

    -- * TargetGroupTuple
    TargetGroupTuple,
    targetGroupTuple,
    tgtWeight,
    tgtTargetGroupARN,

    -- * TargetHealth
    TargetHealth,
    targetHealth,
    thState,
    thReason,
    thDescription,

    -- * TargetHealthDescription
    TargetHealthDescription,
    targetHealthDescription,
    thdTargetHealth,
    thdHealthCheckPort,
    thdTarget,
  )
where

import Network.AWS.ELBv2.Types.Action
import Network.AWS.ELBv2.Types.ActionTypeEnum
import Network.AWS.ELBv2.Types.AuthenticateCognitoActionConditionalBehaviorEnum
import Network.AWS.ELBv2.Types.AuthenticateCognitoActionConfig
import Network.AWS.ELBv2.Types.AuthenticateOidcActionConditionalBehaviorEnum
import Network.AWS.ELBv2.Types.AuthenticateOidcActionConfig
import Network.AWS.ELBv2.Types.AvailabilityZone
import Network.AWS.ELBv2.Types.Certificate
import Network.AWS.ELBv2.Types.Cipher
import Network.AWS.ELBv2.Types.FixedResponseActionConfig
import Network.AWS.ELBv2.Types.ForwardActionConfig
import Network.AWS.ELBv2.Types.HTTPHeaderConditionConfig
import Network.AWS.ELBv2.Types.HTTPRequestMethodConditionConfig
import Network.AWS.ELBv2.Types.HostHeaderConditionConfig
import Network.AWS.ELBv2.Types.IPAddressType
import Network.AWS.ELBv2.Types.Limit
import Network.AWS.ELBv2.Types.Listener
import Network.AWS.ELBv2.Types.LoadBalancer
import Network.AWS.ELBv2.Types.LoadBalancerAddress
import Network.AWS.ELBv2.Types.LoadBalancerAttribute
import Network.AWS.ELBv2.Types.LoadBalancerSchemeEnum
import Network.AWS.ELBv2.Types.LoadBalancerState
import Network.AWS.ELBv2.Types.LoadBalancerStateEnum
import Network.AWS.ELBv2.Types.LoadBalancerTypeEnum
import Network.AWS.ELBv2.Types.Matcher
import Network.AWS.ELBv2.Types.PathPatternConditionConfig
import Network.AWS.ELBv2.Types.ProtocolEnum
import Network.AWS.ELBv2.Types.QueryStringConditionConfig
import Network.AWS.ELBv2.Types.QueryStringKeyValuePair
import Network.AWS.ELBv2.Types.RedirectActionConfig
import Network.AWS.ELBv2.Types.RedirectActionStatusCodeEnum
import Network.AWS.ELBv2.Types.Rule
import Network.AWS.ELBv2.Types.RuleCondition
import Network.AWS.ELBv2.Types.RulePriorityPair
import Network.AWS.ELBv2.Types.SSLPolicy
import Network.AWS.ELBv2.Types.SourceIPConditionConfig
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
import Network.AWS.ELBv2.Types.TargetHealthReasonEnum
import Network.AWS.ELBv2.Types.TargetHealthStateEnum
import Network.AWS.ELBv2.Types.TargetTypeEnum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2015-12-01@ of the Amazon Elastic Load Balancing SDK configuration.
eLBv2 :: Service
eLBv2 =
  Service
    { _svcAbbrev = "ELBv2",
      _svcSigner = v4,
      _svcPrefix = "elasticloadbalancing",
      _svcVersion = "2015-12-01",
      _svcEndpoint = defaultEndpoint eLBv2,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseXMLError "ELBv2",
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
