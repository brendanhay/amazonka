-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types
  ( -- * Service configuration
    eLBv2Service,

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
    Action (..),
    mkAction,
    aFixedResponseConfig,
    aTargetGroupARN,
    aForwardConfig,
    aRedirectConfig,
    aType,
    aAuthenticateCognitoConfig,
    aOrder,
    aAuthenticateOidcConfig,

    -- * AuthenticateCognitoActionConfig
    AuthenticateCognitoActionConfig (..),
    mkAuthenticateCognitoActionConfig,
    acacUserPoolARN,
    acacAuthenticationRequestExtraParams,
    acacScope,
    acacOnUnauthenticatedRequest,
    acacSessionCookieName,
    acacSessionTimeout,
    acacUserPoolDomain,
    acacUserPoolClientId,

    -- * AuthenticateOidcActionConfig
    AuthenticateOidcActionConfig (..),
    mkAuthenticateOidcActionConfig,
    aoacClientId,
    aoacClientSecret,
    aoacUserInfoEndpoint,
    aoacUseExistingClientSecret,
    aoacAuthenticationRequestExtraParams,
    aoacScope,
    aoacOnUnauthenticatedRequest,
    aoacSessionCookieName,
    aoacSessionTimeout,
    aoacAuthorizationEndpoint,
    aoacTokenEndpoint,
    aoacIssuer,

    -- * AvailabilityZone
    AvailabilityZone (..),
    mkAvailabilityZone,
    azSubnetId,
    azZoneName,
    azLoadBalancerAddresses,
    azOutpostId,

    -- * Certificate
    Certificate (..),
    mkCertificate,
    cCertificateARN,
    cIsDefault,

    -- * Cipher
    Cipher (..),
    mkCipher,
    cPriority,
    cName,

    -- * FixedResponseActionConfig
    FixedResponseActionConfig (..),
    mkFixedResponseActionConfig,
    fracMessageBody,
    fracContentType,
    fracStatusCode,

    -- * ForwardActionConfig
    ForwardActionConfig (..),
    mkForwardActionConfig,
    facTargetGroups,
    facTargetGroupStickinessConfig,

    -- * HTTPHeaderConditionConfig
    HTTPHeaderConditionConfig (..),
    mkHTTPHeaderConditionConfig,
    httphccValues,
    httphccHTTPHeaderName,

    -- * HTTPRequestMethodConditionConfig
    HTTPRequestMethodConditionConfig (..),
    mkHTTPRequestMethodConditionConfig,
    httprmccValues,

    -- * HostHeaderConditionConfig
    HostHeaderConditionConfig (..),
    mkHostHeaderConditionConfig,
    hhccValues,

    -- * Limit
    Limit (..),
    mkLimit,
    lMax,
    lName,

    -- * Listener
    Listener (..),
    mkListener,
    lSSLPolicy,
    lListenerARN,
    lProtocol,
    lDefaultActions,
    lCertificates,
    lLoadBalancerARN,
    lAlpnPolicy,
    lPort,

    -- * LoadBalancer
    LoadBalancer (..),
    mkLoadBalancer,
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
    LoadBalancerAddress (..),
    mkLoadBalancerAddress,
    lbaIPv6Address,
    lbaIPAddress,
    lbaAllocationId,
    lbaPrivateIPv4Address,

    -- * LoadBalancerAttribute
    LoadBalancerAttribute (..),
    mkLoadBalancerAttribute,
    lbaValue,
    lbaKey,

    -- * LoadBalancerState
    LoadBalancerState (..),
    mkLoadBalancerState,
    lbsReason,
    lbsCode,

    -- * Matcher
    Matcher (..),
    mkMatcher,
    mHTTPCode,
    mGrpcCode,

    -- * PathPatternConditionConfig
    PathPatternConditionConfig (..),
    mkPathPatternConditionConfig,
    ppccValues,

    -- * QueryStringConditionConfig
    QueryStringConditionConfig (..),
    mkQueryStringConditionConfig,
    qsccValues,

    -- * QueryStringKeyValuePair
    QueryStringKeyValuePair (..),
    mkQueryStringKeyValuePair,
    qskvpValue,
    qskvpKey,

    -- * RedirectActionConfig
    RedirectActionConfig (..),
    mkRedirectActionConfig,
    racPath,
    racProtocol,
    racQuery,
    racHost,
    racPort,
    racStatusCode,

    -- * Rule
    Rule (..),
    mkRule,
    rPriority,
    rActions,
    rConditions,
    rRuleARN,
    rIsDefault,

    -- * RuleCondition
    RuleCondition (..),
    mkRuleCondition,
    rcField,
    rcHTTPHeaderConfig,
    rcHostHeaderConfig,
    rcValues,
    rcSourceIPConfig,
    rcHTTPRequestMethodConfig,
    rcPathPatternConfig,
    rcQueryStringConfig,

    -- * RulePriorityPair
    RulePriorityPair (..),
    mkRulePriorityPair,
    rppPriority,
    rppRuleARN,

    -- * SSLPolicy
    SSLPolicy (..),
    mkSSLPolicy,
    spCiphers,
    spName,
    spSSLProtocols,

    -- * SourceIPConditionConfig
    SourceIPConditionConfig (..),
    mkSourceIPConditionConfig,
    siccValues,

    -- * SubnetMapping
    SubnetMapping (..),
    mkSubnetMapping,
    smIPv6Address,
    smAllocationId,
    smPrivateIPv4Address,
    smSubnetId,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * TagDescription
    TagDescription (..),
    mkTagDescription,
    tdResourceARN,
    tdTags,

    -- * TargetDescription
    TargetDescription (..),
    mkTargetDescription,
    tdAvailabilityZone,
    tdId,
    tdPort,

    -- * TargetGroup
    TargetGroup (..),
    mkTargetGroup,
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
    TargetGroupAttribute (..),
    mkTargetGroupAttribute,
    tgaValue,
    tgaKey,

    -- * TargetGroupStickinessConfig
    TargetGroupStickinessConfig (..),
    mkTargetGroupStickinessConfig,
    tgscEnabled,
    tgscDurationSeconds,

    -- * TargetGroupTuple
    TargetGroupTuple (..),
    mkTargetGroupTuple,
    tgtWeight,
    tgtTargetGroupARN,

    -- * TargetHealth
    TargetHealth (..),
    mkTargetHealth,
    thState,
    thReason,
    thDescription,

    -- * TargetHealthDescription
    TargetHealthDescription (..),
    mkTargetHealthDescription,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-12-01@ of the Amazon Elastic Load Balancing SDK configuration.
eLBv2Service :: Lude.Service
eLBv2Service =
  Lude.Service
    { Lude._svcAbbrev = "ELBv2",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "elasticloadbalancing",
      Lude._svcVersion = "2015-12-01",
      Lude._svcEndpoint = Lude.defaultEndpoint eLBv2Service,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseXMLError "ELBv2",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
