{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Elastic Load Balancing__
--
-- A load balancer distributes incoming traffic across targets, such as your EC2 instances. This enables you to increase the availability of your application. The load balancer also monitors the health of its registered targets and ensures that it routes traffic only to healthy targets. You configure your load balancer to accept incoming traffic by specifying one or more listeners, which are configured with a protocol and port number for connections from clients to the load balancer. You configure a target group with a protocol and port number for connections from the load balancer to the targets, and with health check settings to be used when checking the health status of the targets.
-- Elastic Load Balancing supports the following types of load balancers: Application Load Balancers, Network Load Balancers, Gateway Load Balancers, and Classic Load Balancers. This reference covers the following load balancer types:
--
--     * Application Load Balancer - Operates at the application layer (layer 7) and supports HTTP and HTTPS.
--
--
--     * Network Load Balancer - Operates at the transport layer (layer 4) and supports TCP, TLS, and UDP.
--
--
--     * Gateway Load Balancer - Operates at the network layer (layer 3).
--
--
-- For more information, see the <https://docs.aws.amazon.com/elasticloadbalancing/latest/userguide/ Elastic Load Balancing User Guide> .
-- All Elastic Load Balancing operations are idempotent, which means that they complete at most one time. If you repeat an operation, it succeeds.
module Network.AWS.ELBv2
  ( -- * Service configuration
    eLBv2Service,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- ** LoadBalancersDeleted
    mkLoadBalancersDeleted,

    -- ** TargetDeregistered
    mkTargetDeregistered,

    -- ** LoadBalancerAvailable
    mkLoadBalancerAvailable,

    -- ** TargetInService
    mkTargetInService,

    -- ** LoadBalancerExists
    mkLoadBalancerExists,

    -- * Operations
    -- $operations

    -- ** DescribeLoadBalancers (Paginated)
    module Network.AWS.ELBv2.DescribeLoadBalancers,

    -- ** DescribeTags
    module Network.AWS.ELBv2.DescribeTags,

    -- ** DeleteRule
    module Network.AWS.ELBv2.DeleteRule,

    -- ** RemoveTags
    module Network.AWS.ELBv2.RemoveTags,

    -- ** DeleteTargetGroup
    module Network.AWS.ELBv2.DeleteTargetGroup,

    -- ** SetSubnets
    module Network.AWS.ELBv2.SetSubnets,

    -- ** CreateRule
    module Network.AWS.ELBv2.CreateRule,

    -- ** DescribeListenerCertificates (Paginated)
    module Network.AWS.ELBv2.DescribeListenerCertificates,

    -- ** SetSecurityGroups
    module Network.AWS.ELBv2.SetSecurityGroups,

    -- ** SetRulePriorities
    module Network.AWS.ELBv2.SetRulePriorities,

    -- ** DescribeTargetGroups (Paginated)
    module Network.AWS.ELBv2.DescribeTargetGroups,

    -- ** DescribeRules (Paginated)
    module Network.AWS.ELBv2.DescribeRules,

    -- ** DeleteLoadBalancer
    module Network.AWS.ELBv2.DeleteLoadBalancer,

    -- ** RegisterTargets
    module Network.AWS.ELBv2.RegisterTargets,

    -- ** ModifyListener
    module Network.AWS.ELBv2.ModifyListener,

    -- ** ModifyTargetGroup
    module Network.AWS.ELBv2.ModifyTargetGroup,

    -- ** ModifyTargetGroupAttributes
    module Network.AWS.ELBv2.ModifyTargetGroupAttributes,

    -- ** DescribeTargetGroupAttributes
    module Network.AWS.ELBv2.DescribeTargetGroupAttributes,

    -- ** DeleteListener
    module Network.AWS.ELBv2.DeleteListener,

    -- ** DescribeSSLPolicies (Paginated)
    module Network.AWS.ELBv2.DescribeSSLPolicies,

    -- ** DescribeAccountLimits (Paginated)
    module Network.AWS.ELBv2.DescribeAccountLimits,

    -- ** DeregisterTargets
    module Network.AWS.ELBv2.DeregisterTargets,

    -- ** CreateListener
    module Network.AWS.ELBv2.CreateListener,

    -- ** CreateTargetGroup
    module Network.AWS.ELBv2.CreateTargetGroup,

    -- ** ModifyLoadBalancerAttributes
    module Network.AWS.ELBv2.ModifyLoadBalancerAttributes,

    -- ** SetIPAddressType
    module Network.AWS.ELBv2.SetIPAddressType,

    -- ** AddTags
    module Network.AWS.ELBv2.AddTags,

    -- ** DescribeLoadBalancerAttributes
    module Network.AWS.ELBv2.DescribeLoadBalancerAttributes,

    -- ** DescribeListeners (Paginated)
    module Network.AWS.ELBv2.DescribeListeners,

    -- ** DescribeTargetHealth
    module Network.AWS.ELBv2.DescribeTargetHealth,

    -- ** CreateLoadBalancer
    module Network.AWS.ELBv2.CreateLoadBalancer,

    -- ** RemoveListenerCertificates
    module Network.AWS.ELBv2.RemoveListenerCertificates,

    -- ** ModifyRule
    module Network.AWS.ELBv2.ModifyRule,

    -- ** AddListenerCertificates
    module Network.AWS.ELBv2.AddListenerCertificates,

    -- * Types

    -- ** ActionTypeEnum
    ActionTypeEnum (..),

    -- ** AuthenticateCognitoActionConditionalBehaviorEnum
    AuthenticateCognitoActionConditionalBehaviorEnum (..),

    -- ** AuthenticateOidcActionConditionalBehaviorEnum
    AuthenticateOidcActionConditionalBehaviorEnum (..),

    -- ** IPAddressType
    IPAddressType (..),

    -- ** LoadBalancerSchemeEnum
    LoadBalancerSchemeEnum (..),

    -- ** LoadBalancerStateEnum
    LoadBalancerStateEnum (..),

    -- ** LoadBalancerTypeEnum
    LoadBalancerTypeEnum (..),

    -- ** ProtocolEnum
    ProtocolEnum (..),

    -- ** RedirectActionStatusCodeEnum
    RedirectActionStatusCodeEnum (..),

    -- ** TargetHealthReasonEnum
    TargetHealthReasonEnum (..),

    -- ** TargetHealthStateEnum
    TargetHealthStateEnum (..),

    -- ** TargetTypeEnum
    TargetTypeEnum (..),

    -- ** Action
    Action (..),
    mkAction,
    aFixedResponseConfig,
    aTargetGroupARN,
    aForwardConfig,
    aRedirectConfig,
    aAuthenticateCognitoConfig,
    aOrder,
    aAuthenticateOidcConfig,
    aType,

    -- ** AuthenticateCognitoActionConfig
    AuthenticateCognitoActionConfig (..),
    mkAuthenticateCognitoActionConfig,
    acacAuthenticationRequestExtraParams,
    acacScope,
    acacOnUnauthenticatedRequest,
    acacSessionCookieName,
    acacSessionTimeout,
    acacUserPoolARN,
    acacUserPoolClientId,
    acacUserPoolDomain,

    -- ** AuthenticateOidcActionConfig
    AuthenticateOidcActionConfig (..),
    mkAuthenticateOidcActionConfig,
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

    -- ** AvailabilityZone
    AvailabilityZone (..),
    mkAvailabilityZone,
    azSubnetId,
    azZoneName,
    azLoadBalancerAddresses,
    azOutpostId,

    -- ** Certificate
    Certificate (..),
    mkCertificate,
    cCertificateARN,
    cIsDefault,

    -- ** Cipher
    Cipher (..),
    mkCipher,
    cPriority,
    cName,

    -- ** FixedResponseActionConfig
    FixedResponseActionConfig (..),
    mkFixedResponseActionConfig,
    fracMessageBody,
    fracContentType,
    fracStatusCode,

    -- ** ForwardActionConfig
    ForwardActionConfig (..),
    mkForwardActionConfig,
    facTargetGroups,
    facTargetGroupStickinessConfig,

    -- ** HTTPHeaderConditionConfig
    HTTPHeaderConditionConfig (..),
    mkHTTPHeaderConditionConfig,
    httphccValues,
    httphccHTTPHeaderName,

    -- ** HTTPRequestMethodConditionConfig
    HTTPRequestMethodConditionConfig (..),
    mkHTTPRequestMethodConditionConfig,
    httprmccValues,

    -- ** HostHeaderConditionConfig
    HostHeaderConditionConfig (..),
    mkHostHeaderConditionConfig,
    hhccValues,

    -- ** Limit
    Limit (..),
    mkLimit,
    lMax,
    lName,

    -- ** Listener
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

    -- ** LoadBalancer
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

    -- ** LoadBalancerAddress
    LoadBalancerAddress (..),
    mkLoadBalancerAddress,
    lbaIPv6Address,
    lbaIPAddress,
    lbaAllocationId,
    lbaPrivateIPv4Address,

    -- ** LoadBalancerAttribute
    LoadBalancerAttribute (..),
    mkLoadBalancerAttribute,
    lbaValue,
    lbaKey,

    -- ** LoadBalancerState
    LoadBalancerState (..),
    mkLoadBalancerState,
    lbsReason,
    lbsCode,

    -- ** Matcher
    Matcher (..),
    mkMatcher,
    mHTTPCode,
    mGrpcCode,

    -- ** PathPatternConditionConfig
    PathPatternConditionConfig (..),
    mkPathPatternConditionConfig,
    ppccValues,

    -- ** QueryStringConditionConfig
    QueryStringConditionConfig (..),
    mkQueryStringConditionConfig,
    qsccValues,

    -- ** QueryStringKeyValuePair
    QueryStringKeyValuePair (..),
    mkQueryStringKeyValuePair,
    qskvpValue,
    qskvpKey,

    -- ** RedirectActionConfig
    RedirectActionConfig (..),
    mkRedirectActionConfig,
    racPath,
    racProtocol,
    racQuery,
    racHost,
    racPort,
    racStatusCode,

    -- ** Rule
    Rule (..),
    mkRule,
    rPriority,
    rActions,
    rConditions,
    rRuleARN,
    rIsDefault,

    -- ** RuleCondition
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

    -- ** RulePriorityPair
    RulePriorityPair (..),
    mkRulePriorityPair,
    rppPriority,
    rppRuleARN,

    -- ** SSLPolicy
    SSLPolicy (..),
    mkSSLPolicy,
    spCiphers,
    spName,
    spSSLProtocols,

    -- ** SourceIPConditionConfig
    SourceIPConditionConfig (..),
    mkSourceIPConditionConfig,
    siccValues,

    -- ** SubnetMapping
    SubnetMapping (..),
    mkSubnetMapping,
    smIPv6Address,
    smAllocationId,
    smPrivateIPv4Address,
    smSubnetId,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- ** TagDescription
    TagDescription (..),
    mkTagDescription,
    tdResourceARN,
    tdTags,

    -- ** TargetDescription
    TargetDescription (..),
    mkTargetDescription,
    tdAvailabilityZone,
    tdPort,
    tdId,

    -- ** TargetGroup
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

    -- ** TargetGroupAttribute
    TargetGroupAttribute (..),
    mkTargetGroupAttribute,
    tgaValue,
    tgaKey,

    -- ** TargetGroupStickinessConfig
    TargetGroupStickinessConfig (..),
    mkTargetGroupStickinessConfig,
    tgscEnabled,
    tgscDurationSeconds,

    -- ** TargetGroupTuple
    TargetGroupTuple (..),
    mkTargetGroupTuple,
    tgtWeight,
    tgtTargetGroupARN,

    -- ** TargetHealth
    TargetHealth (..),
    mkTargetHealth,
    thState,
    thReason,
    thDescription,

    -- ** TargetHealthDescription
    TargetHealthDescription (..),
    mkTargetHealthDescription,
    thdTargetHealth,
    thdHealthCheckPort,
    thdTarget,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.ISO8601,
    Lude.Timestamp,
    Lude.UTCTime,
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
import Network.AWS.ELBv2.SetIPAddressType
import Network.AWS.ELBv2.SetRulePriorities
import Network.AWS.ELBv2.SetSecurityGroups
import Network.AWS.ELBv2.SetSubnets
import Network.AWS.ELBv2.Types
import Network.AWS.ELBv2.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ELBv2'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
