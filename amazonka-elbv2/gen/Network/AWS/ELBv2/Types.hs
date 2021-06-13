{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidSecurityGroupException,
    _ALPNPolicyNotSupportedException,
    _InvalidConfigurationRequestException,
    _TooManyTagsException,
    _HealthUnavailableException,
    _PriorityInUseException,
    _DuplicateTagKeysException,
    _AvailabilityZoneNotSupportedException,
    _TooManyUniqueTargetGroupsPerLoadBalancerException,
    _IncompatibleProtocolsException,
    _TooManyRulesException,
    _TooManyActionsException,
    _OperationNotPermittedException,
    _TargetGroupAssociationLimitException,
    _TooManyRegistrationsForTargetIdException,
    _ListenerNotFoundException,
    _SubnetNotFoundException,
    _InvalidTargetException,
    _UnsupportedProtocolException,
    _ResourceInUseException,
    _AllocationIdNotFoundException,
    _TooManyLoadBalancersException,
    _CertificateNotFoundException,
    _DuplicateTargetGroupNameException,
    _DuplicateListenerException,
    _LoadBalancerNotFoundException,
    _TooManyCertificatesException,
    _InvalidSchemeException,
    _SSLPolicyNotFoundException,
    _InvalidSubnetException,
    _TooManyTargetGroupsException,
    _DuplicateLoadBalancerNameException,
    _TooManyListenersException,
    _InvalidLoadBalancerActionException,
    _TooManyTargetsException,
    _RuleNotFoundException,
    _TargetGroupNotFoundException,

    -- * ActionTypeEnum
    ActionTypeEnum (..),

    -- * AuthenticateCognitoActionConditionalBehaviorEnum
    AuthenticateCognitoActionConditionalBehaviorEnum (..),

    -- * AuthenticateOidcActionConditionalBehaviorEnum
    AuthenticateOidcActionConditionalBehaviorEnum (..),

    -- * IpAddressType
    IpAddressType (..),

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
    newAction,
    action_authenticateOidcConfig,
    action_targetGroupArn,
    action_authenticateCognitoConfig,
    action_fixedResponseConfig,
    action_forwardConfig,
    action_order,
    action_redirectConfig,
    action_type,

    -- * AuthenticateCognitoActionConfig
    AuthenticateCognitoActionConfig (..),
    newAuthenticateCognitoActionConfig,
    authenticateCognitoActionConfig_sessionTimeout,
    authenticateCognitoActionConfig_scope,
    authenticateCognitoActionConfig_authenticationRequestExtraParams,
    authenticateCognitoActionConfig_sessionCookieName,
    authenticateCognitoActionConfig_onUnauthenticatedRequest,
    authenticateCognitoActionConfig_userPoolArn,
    authenticateCognitoActionConfig_userPoolClientId,
    authenticateCognitoActionConfig_userPoolDomain,

    -- * AuthenticateOidcActionConfig
    AuthenticateOidcActionConfig (..),
    newAuthenticateOidcActionConfig,
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

    -- * AvailabilityZone
    AvailabilityZone (..),
    newAvailabilityZone,
    availabilityZone_zoneName,
    availabilityZone_outpostId,
    availabilityZone_loadBalancerAddresses,
    availabilityZone_subnetId,

    -- * Certificate
    Certificate (..),
    newCertificate,
    certificate_isDefault,
    certificate_certificateArn,

    -- * Cipher
    Cipher (..),
    newCipher,
    cipher_priority,
    cipher_name,

    -- * FixedResponseActionConfig
    FixedResponseActionConfig (..),
    newFixedResponseActionConfig,
    fixedResponseActionConfig_contentType,
    fixedResponseActionConfig_messageBody,
    fixedResponseActionConfig_statusCode,

    -- * ForwardActionConfig
    ForwardActionConfig (..),
    newForwardActionConfig,
    forwardActionConfig_targetGroupStickinessConfig,
    forwardActionConfig_targetGroups,

    -- * HostHeaderConditionConfig
    HostHeaderConditionConfig (..),
    newHostHeaderConditionConfig,
    hostHeaderConditionConfig_values,

    -- * HttpHeaderConditionConfig
    HttpHeaderConditionConfig (..),
    newHttpHeaderConditionConfig,
    httpHeaderConditionConfig_values,
    httpHeaderConditionConfig_httpHeaderName,

    -- * HttpRequestMethodConditionConfig
    HttpRequestMethodConditionConfig (..),
    newHttpRequestMethodConditionConfig,
    httpRequestMethodConditionConfig_values,

    -- * Limit
    Limit (..),
    newLimit,
    limit_name,
    limit_max,

    -- * Listener
    Listener (..),
    newListener,
    listener_loadBalancerArn,
    listener_sslPolicy,
    listener_port,
    listener_defaultActions,
    listener_protocol,
    listener_certificates,
    listener_listenerArn,
    listener_alpnPolicy,

    -- * LoadBalancer
    LoadBalancer (..),
    newLoadBalancer,
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

    -- * LoadBalancerAddress
    LoadBalancerAddress (..),
    newLoadBalancerAddress,
    loadBalancerAddress_privateIPv4Address,
    loadBalancerAddress_ipAddress,
    loadBalancerAddress_iPv6Address,
    loadBalancerAddress_allocationId,

    -- * LoadBalancerAttribute
    LoadBalancerAttribute (..),
    newLoadBalancerAttribute,
    loadBalancerAttribute_key,
    loadBalancerAttribute_value,

    -- * LoadBalancerState
    LoadBalancerState (..),
    newLoadBalancerState,
    loadBalancerState_code,
    loadBalancerState_reason,

    -- * Matcher
    Matcher (..),
    newMatcher,
    matcher_grpcCode,
    matcher_httpCode,

    -- * PathPatternConditionConfig
    PathPatternConditionConfig (..),
    newPathPatternConditionConfig,
    pathPatternConditionConfig_values,

    -- * QueryStringConditionConfig
    QueryStringConditionConfig (..),
    newQueryStringConditionConfig,
    queryStringConditionConfig_values,

    -- * QueryStringKeyValuePair
    QueryStringKeyValuePair (..),
    newQueryStringKeyValuePair,
    queryStringKeyValuePair_key,
    queryStringKeyValuePair_value,

    -- * RedirectActionConfig
    RedirectActionConfig (..),
    newRedirectActionConfig,
    redirectActionConfig_query,
    redirectActionConfig_port,
    redirectActionConfig_protocol,
    redirectActionConfig_host,
    redirectActionConfig_path,
    redirectActionConfig_statusCode,

    -- * Rule
    Rule (..),
    newRule,
    rule_isDefault,
    rule_ruleArn,
    rule_actions,
    rule_priority,
    rule_conditions,

    -- * RuleCondition
    RuleCondition (..),
    newRuleCondition,
    ruleCondition_pathPatternConfig,
    ruleCondition_httpRequestMethodConfig,
    ruleCondition_values,
    ruleCondition_sourceIpConfig,
    ruleCondition_httpHeaderConfig,
    ruleCondition_hostHeaderConfig,
    ruleCondition_queryStringConfig,
    ruleCondition_field,

    -- * RulePriorityPair
    RulePriorityPair (..),
    newRulePriorityPair,
    rulePriorityPair_ruleArn,
    rulePriorityPair_priority,

    -- * SourceIpConditionConfig
    SourceIpConditionConfig (..),
    newSourceIpConditionConfig,
    sourceIpConditionConfig_values,

    -- * SslPolicy
    SslPolicy (..),
    newSslPolicy,
    sslPolicy_ciphers,
    sslPolicy_name,
    sslPolicy_sslProtocols,

    -- * SubnetMapping
    SubnetMapping (..),
    newSubnetMapping,
    subnetMapping_privateIPv4Address,
    subnetMapping_iPv6Address,
    subnetMapping_subnetId,
    subnetMapping_allocationId,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * TagDescription
    TagDescription (..),
    newTagDescription,
    tagDescription_resourceArn,
    tagDescription_tags,

    -- * TargetDescription
    TargetDescription (..),
    newTargetDescription,
    targetDescription_availabilityZone,
    targetDescription_port,
    targetDescription_id,

    -- * TargetGroup
    TargetGroup (..),
    newTargetGroup,
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

    -- * TargetGroupAttribute
    TargetGroupAttribute (..),
    newTargetGroupAttribute,
    targetGroupAttribute_key,
    targetGroupAttribute_value,

    -- * TargetGroupStickinessConfig
    TargetGroupStickinessConfig (..),
    newTargetGroupStickinessConfig,
    targetGroupStickinessConfig_enabled,
    targetGroupStickinessConfig_durationSeconds,

    -- * TargetGroupTuple
    TargetGroupTuple (..),
    newTargetGroupTuple,
    targetGroupTuple_targetGroupArn,
    targetGroupTuple_weight,

    -- * TargetHealth
    TargetHealth (..),
    newTargetHealth,
    targetHealth_state,
    targetHealth_reason,
    targetHealth_description,

    -- * TargetHealthDescription
    TargetHealthDescription (..),
    newTargetHealthDescription,
    targetHealthDescription_healthCheckPort,
    targetHealthDescription_target,
    targetHealthDescription_targetHealth,
  )
where

import qualified Network.AWS.Core as Core
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
import Network.AWS.ELBv2.Types.HostHeaderConditionConfig
import Network.AWS.ELBv2.Types.HttpHeaderConditionConfig
import Network.AWS.ELBv2.Types.HttpRequestMethodConditionConfig
import Network.AWS.ELBv2.Types.IpAddressType
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
import Network.AWS.ELBv2.Types.TargetHealthReasonEnum
import Network.AWS.ELBv2.Types.TargetHealthStateEnum
import Network.AWS.ELBv2.Types.TargetTypeEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-12-01@ of the Amazon Elastic Load Balancing SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "ELBv2",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "elasticloadbalancing",
      Core._serviceSigningName = "elasticloadbalancing",
      Core._serviceVersion = "2015-12-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseXMLError "ELBv2",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified security group does not exist.
_InvalidSecurityGroupException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSecurityGroupException =
  Core._MatchServiceError
    defaultService
    "InvalidSecurityGroup"
    Prelude.. Core.hasStatus 400

-- | The specified ALPN policy is not supported.
_ALPNPolicyNotSupportedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ALPNPolicyNotSupportedException =
  Core._MatchServiceError
    defaultService
    "ALPNPolicyNotFound"
    Prelude.. Core.hasStatus 400

-- | The requested configuration is not valid.
_InvalidConfigurationRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidConfigurationRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidConfigurationRequest"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of tags per load balancer.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTags"
    Prelude.. Core.hasStatus 400

-- | The health of the specified targets could not be retrieved due to an
-- internal error.
_HealthUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_HealthUnavailableException =
  Core._MatchServiceError
    defaultService
    "HealthUnavailable"
    Prelude.. Core.hasStatus 500

-- | The specified priority is in use.
_PriorityInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PriorityInUseException =
  Core._MatchServiceError
    defaultService
    "PriorityInUse"
    Prelude.. Core.hasStatus 400

-- | A tag key was specified more than once.
_DuplicateTagKeysException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateTagKeysException =
  Core._MatchServiceError
    defaultService
    "DuplicateTagKeys"
    Prelude.. Core.hasStatus 400

-- | The specified Availability Zone is not supported.
_AvailabilityZoneNotSupportedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AvailabilityZoneNotSupportedException =
  Core._MatchServiceError
    defaultService
    "AvailabilityZoneNotSupported"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of unique target groups per load
-- balancer across all listeners. If a target group is used by multiple
-- actions for a load balancer, it is counted as only one use.
_TooManyUniqueTargetGroupsPerLoadBalancerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyUniqueTargetGroupsPerLoadBalancerException =
  Core._MatchServiceError
    defaultService
    "TooManyUniqueTargetGroupsPerLoadBalancer"
    Prelude.. Core.hasStatus 400

-- | The specified configuration is not valid with this protocol.
_IncompatibleProtocolsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IncompatibleProtocolsException =
  Core._MatchServiceError
    defaultService
    "IncompatibleProtocols"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of rules per load balancer.
_TooManyRulesException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRulesException =
  Core._MatchServiceError
    defaultService
    "TooManyRules"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of actions per rule.
_TooManyActionsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyActionsException =
  Core._MatchServiceError
    defaultService
    "TooManyActions"
    Prelude.. Core.hasStatus 400

-- | This operation is not allowed.
_OperationNotPermittedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationNotPermittedException =
  Core._MatchServiceError
    defaultService
    "OperationNotPermitted"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of load balancers per target
-- group.
_TargetGroupAssociationLimitException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TargetGroupAssociationLimitException =
  Core._MatchServiceError
    defaultService
    "TargetGroupAssociationLimit"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of times a target can be
-- registered with a load balancer.
_TooManyRegistrationsForTargetIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRegistrationsForTargetIdException =
  Core._MatchServiceError
    defaultService
    "TooManyRegistrationsForTargetId"
    Prelude.. Core.hasStatus 400

-- | The specified listener does not exist.
_ListenerNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ListenerNotFoundException =
  Core._MatchServiceError
    defaultService
    "ListenerNotFound"
    Prelude.. Core.hasStatus 400

-- | The specified subnet does not exist.
_SubnetNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetNotFoundException =
  Core._MatchServiceError
    defaultService
    "SubnetNotFound"
    Prelude.. Core.hasStatus 400

-- | The specified target does not exist, is not in the same VPC as the
-- target group, or has an unsupported instance type.
_InvalidTargetException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTargetException =
  Core._MatchServiceError
    defaultService
    "InvalidTarget"
    Prelude.. Core.hasStatus 400

-- | The specified protocol is not supported.
_UnsupportedProtocolException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedProtocolException =
  Core._MatchServiceError
    defaultService
    "UnsupportedProtocol"
    Prelude.. Core.hasStatus 400

-- | A specified resource is in use.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUse"
    Prelude.. Core.hasStatus 400

-- | The specified allocation ID does not exist.
_AllocationIdNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AllocationIdNotFoundException =
  Core._MatchServiceError
    defaultService
    "AllocationIdNotFound"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of load balancers for your AWS
-- account.
_TooManyLoadBalancersException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyLoadBalancersException =
  Core._MatchServiceError
    defaultService
    "TooManyLoadBalancers"
    Prelude.. Core.hasStatus 400

-- | The specified certificate does not exist.
_CertificateNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CertificateNotFoundException =
  Core._MatchServiceError
    defaultService
    "CertificateNotFound"
    Prelude.. Core.hasStatus 400

-- | A target group with the specified name already exists.
_DuplicateTargetGroupNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateTargetGroupNameException =
  Core._MatchServiceError
    defaultService
    "DuplicateTargetGroupName"
    Prelude.. Core.hasStatus 400

-- | A listener with the specified port already exists.
_DuplicateListenerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateListenerException =
  Core._MatchServiceError
    defaultService
    "DuplicateListener"
    Prelude.. Core.hasStatus 400

-- | The specified load balancer does not exist.
_LoadBalancerNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LoadBalancerNotFoundException =
  Core._MatchServiceError
    defaultService
    "LoadBalancerNotFound"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of certificates per load
-- balancer.
_TooManyCertificatesException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyCertificatesException =
  Core._MatchServiceError
    defaultService
    "TooManyCertificates"
    Prelude.. Core.hasStatus 400

-- | The requested scheme is not valid.
_InvalidSchemeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSchemeException =
  Core._MatchServiceError
    defaultService
    "InvalidScheme"
    Prelude.. Core.hasStatus 400

-- | The specified SSL policy does not exist.
_SSLPolicyNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SSLPolicyNotFoundException =
  Core._MatchServiceError
    defaultService
    "SSLPolicyNotFound"
    Prelude.. Core.hasStatus 400

-- | The specified subnet is out of available addresses.
_InvalidSubnetException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSubnetException =
  Core._MatchServiceError
    defaultService
    "InvalidSubnet"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of target groups for your AWS
-- account.
_TooManyTargetGroupsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTargetGroupsException =
  Core._MatchServiceError
    defaultService
    "TooManyTargetGroups"
    Prelude.. Core.hasStatus 400

-- | A load balancer with the specified name already exists.
_DuplicateLoadBalancerNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateLoadBalancerNameException =
  Core._MatchServiceError
    defaultService
    "DuplicateLoadBalancerName"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of listeners per load balancer.
_TooManyListenersException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyListenersException =
  Core._MatchServiceError
    defaultService
    "TooManyListeners"
    Prelude.. Core.hasStatus 400

-- | The requested action is not valid.
_InvalidLoadBalancerActionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidLoadBalancerActionException =
  Core._MatchServiceError
    defaultService
    "InvalidLoadBalancerAction"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of targets.
_TooManyTargetsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTargetsException =
  Core._MatchServiceError
    defaultService
    "TooManyTargets"
    Prelude.. Core.hasStatus 400

-- | The specified rule does not exist.
_RuleNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RuleNotFoundException =
  Core._MatchServiceError
    defaultService
    "RuleNotFound"
    Prelude.. Core.hasStatus 400

-- | The specified target group does not exist.
_TargetGroupNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TargetGroupNotFoundException =
  Core._MatchServiceError
    defaultService
    "TargetGroupNotFound"
    Prelude.. Core.hasStatus 400
