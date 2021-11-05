{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBV2.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBV2.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidConfigurationRequestException,
    _SubnetNotFoundException,
    _TooManyTargetsException,
    _RuleNotFoundException,
    _InvalidSubnetException,
    _TooManyRulesException,
    _TooManyTargetGroupsException,
    _TooManyActionsException,
    _DuplicateLoadBalancerNameException,
    _IncompatibleProtocolsException,
    _TooManyCertificatesException,
    _DuplicateTagKeysException,
    _DuplicateListenerException,
    _TooManyTagsException,
    _DuplicateTargetGroupNameException,
    _HealthUnavailableException,
    _AllocationIdNotFoundException,
    _PriorityInUseException,
    _TooManyLoadBalancersException,
    _UnsupportedProtocolException,
    _ALPNPolicyNotSupportedException,
    _InvalidTargetException,
    _InvalidSecurityGroupException,
    _TargetGroupNotFoundException,
    _ListenerNotFoundException,
    _InvalidLoadBalancerActionException,
    _TooManyRegistrationsForTargetIdException,
    _TooManyListenersException,
    _TargetGroupAssociationLimitException,
    _OperationNotPermittedException,
    _SSLPolicyNotFoundException,
    _InvalidSchemeException,
    _AvailabilityZoneNotSupportedException,
    _TooManyUniqueTargetGroupsPerLoadBalancerException,
    _LoadBalancerNotFoundException,
    _ResourceInUseException,
    _CertificateNotFoundException,

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

    -- * TargetGroupIpAddressTypeEnum
    TargetGroupIpAddressTypeEnum (..),

    -- * TargetHealthReasonEnum
    TargetHealthReasonEnum (..),

    -- * TargetHealthStateEnum
    TargetHealthStateEnum (..),

    -- * TargetTypeEnum
    TargetTypeEnum (..),

    -- * Action
    Action (..),
    newAction,
    action_fixedResponseConfig,
    action_targetGroupArn,
    action_forwardConfig,
    action_redirectConfig,
    action_authenticateCognitoConfig,
    action_order,
    action_authenticateOidcConfig,
    action_type,

    -- * AuthenticateCognitoActionConfig
    AuthenticateCognitoActionConfig (..),
    newAuthenticateCognitoActionConfig,
    authenticateCognitoActionConfig_authenticationRequestExtraParams,
    authenticateCognitoActionConfig_scope,
    authenticateCognitoActionConfig_onUnauthenticatedRequest,
    authenticateCognitoActionConfig_sessionCookieName,
    authenticateCognitoActionConfig_sessionTimeout,
    authenticateCognitoActionConfig_userPoolArn,
    authenticateCognitoActionConfig_userPoolClientId,
    authenticateCognitoActionConfig_userPoolDomain,

    -- * AuthenticateOidcActionConfig
    AuthenticateOidcActionConfig (..),
    newAuthenticateOidcActionConfig,
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

    -- * AvailabilityZone
    AvailabilityZone (..),
    newAvailabilityZone,
    availabilityZone_subnetId,
    availabilityZone_zoneName,
    availabilityZone_loadBalancerAddresses,
    availabilityZone_outpostId,

    -- * Certificate
    Certificate (..),
    newCertificate,
    certificate_certificateArn,
    certificate_isDefault,

    -- * Cipher
    Cipher (..),
    newCipher,
    cipher_priority,
    cipher_name,

    -- * FixedResponseActionConfig
    FixedResponseActionConfig (..),
    newFixedResponseActionConfig,
    fixedResponseActionConfig_messageBody,
    fixedResponseActionConfig_contentType,
    fixedResponseActionConfig_statusCode,

    -- * ForwardActionConfig
    ForwardActionConfig (..),
    newForwardActionConfig,
    forwardActionConfig_targetGroups,
    forwardActionConfig_targetGroupStickinessConfig,

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
    limit_max,
    limit_name,

    -- * Listener
    Listener (..),
    newListener,
    listener_sslPolicy,
    listener_listenerArn,
    listener_protocol,
    listener_defaultActions,
    listener_certificates,
    listener_loadBalancerArn,
    listener_alpnPolicy,
    listener_port,

    -- * LoadBalancer
    LoadBalancer (..),
    newLoadBalancer,
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

    -- * LoadBalancerAddress
    LoadBalancerAddress (..),
    newLoadBalancerAddress,
    loadBalancerAddress_iPv6Address,
    loadBalancerAddress_ipAddress,
    loadBalancerAddress_allocationId,
    loadBalancerAddress_privateIPv4Address,

    -- * LoadBalancerAttribute
    LoadBalancerAttribute (..),
    newLoadBalancerAttribute,
    loadBalancerAttribute_value,
    loadBalancerAttribute_key,

    -- * LoadBalancerState
    LoadBalancerState (..),
    newLoadBalancerState,
    loadBalancerState_reason,
    loadBalancerState_code,

    -- * Matcher
    Matcher (..),
    newMatcher,
    matcher_httpCode,
    matcher_grpcCode,

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
    queryStringKeyValuePair_value,
    queryStringKeyValuePair_key,

    -- * RedirectActionConfig
    RedirectActionConfig (..),
    newRedirectActionConfig,
    redirectActionConfig_path,
    redirectActionConfig_protocol,
    redirectActionConfig_query,
    redirectActionConfig_host,
    redirectActionConfig_port,
    redirectActionConfig_statusCode,

    -- * Rule
    Rule (..),
    newRule,
    rule_priority,
    rule_actions,
    rule_conditions,
    rule_ruleArn,
    rule_isDefault,

    -- * RuleCondition
    RuleCondition (..),
    newRuleCondition,
    ruleCondition_field,
    ruleCondition_httpHeaderConfig,
    ruleCondition_hostHeaderConfig,
    ruleCondition_values,
    ruleCondition_sourceIpConfig,
    ruleCondition_httpRequestMethodConfig,
    ruleCondition_pathPatternConfig,
    ruleCondition_queryStringConfig,

    -- * RulePriorityPair
    RulePriorityPair (..),
    newRulePriorityPair,
    rulePriorityPair_priority,
    rulePriorityPair_ruleArn,

    -- * SourceIpConditionConfig
    SourceIpConditionConfig (..),
    newSourceIpConditionConfig,
    sourceIpConditionConfig_values,

    -- * SslPolicy
    SslPolicy (..),
    newSslPolicy,
    sslPolicy_supportedLoadBalancerTypes,
    sslPolicy_ciphers,
    sslPolicy_name,
    sslPolicy_sslProtocols,

    -- * SubnetMapping
    SubnetMapping (..),
    newSubnetMapping,
    subnetMapping_iPv6Address,
    subnetMapping_allocationId,
    subnetMapping_privateIPv4Address,
    subnetMapping_subnetId,

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

    -- * TargetGroupAttribute
    TargetGroupAttribute (..),
    newTargetGroupAttribute,
    targetGroupAttribute_value,
    targetGroupAttribute_key,

    -- * TargetGroupStickinessConfig
    TargetGroupStickinessConfig (..),
    newTargetGroupStickinessConfig,
    targetGroupStickinessConfig_enabled,
    targetGroupStickinessConfig_durationSeconds,

    -- * TargetGroupTuple
    TargetGroupTuple (..),
    newTargetGroupTuple,
    targetGroupTuple_weight,
    targetGroupTuple_targetGroupArn,

    -- * TargetHealth
    TargetHealth (..),
    newTargetHealth,
    targetHealth_state,
    targetHealth_reason,
    targetHealth_description,

    -- * TargetHealthDescription
    TargetHealthDescription (..),
    newTargetHealthDescription,
    targetHealthDescription_targetHealth,
    targetHealthDescription_healthCheckPort,
    targetHealthDescription_target,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBV2.Types.Action
import Network.AWS.ELBV2.Types.ActionTypeEnum
import Network.AWS.ELBV2.Types.AuthenticateCognitoActionConditionalBehaviorEnum
import Network.AWS.ELBV2.Types.AuthenticateCognitoActionConfig
import Network.AWS.ELBV2.Types.AuthenticateOidcActionConditionalBehaviorEnum
import Network.AWS.ELBV2.Types.AuthenticateOidcActionConfig
import Network.AWS.ELBV2.Types.AvailabilityZone
import Network.AWS.ELBV2.Types.Certificate
import Network.AWS.ELBV2.Types.Cipher
import Network.AWS.ELBV2.Types.FixedResponseActionConfig
import Network.AWS.ELBV2.Types.ForwardActionConfig
import Network.AWS.ELBV2.Types.HostHeaderConditionConfig
import Network.AWS.ELBV2.Types.HttpHeaderConditionConfig
import Network.AWS.ELBV2.Types.HttpRequestMethodConditionConfig
import Network.AWS.ELBV2.Types.IpAddressType
import Network.AWS.ELBV2.Types.Limit
import Network.AWS.ELBV2.Types.Listener
import Network.AWS.ELBV2.Types.LoadBalancer
import Network.AWS.ELBV2.Types.LoadBalancerAddress
import Network.AWS.ELBV2.Types.LoadBalancerAttribute
import Network.AWS.ELBV2.Types.LoadBalancerSchemeEnum
import Network.AWS.ELBV2.Types.LoadBalancerState
import Network.AWS.ELBV2.Types.LoadBalancerStateEnum
import Network.AWS.ELBV2.Types.LoadBalancerTypeEnum
import Network.AWS.ELBV2.Types.Matcher
import Network.AWS.ELBV2.Types.PathPatternConditionConfig
import Network.AWS.ELBV2.Types.ProtocolEnum
import Network.AWS.ELBV2.Types.QueryStringConditionConfig
import Network.AWS.ELBV2.Types.QueryStringKeyValuePair
import Network.AWS.ELBV2.Types.RedirectActionConfig
import Network.AWS.ELBV2.Types.RedirectActionStatusCodeEnum
import Network.AWS.ELBV2.Types.Rule
import Network.AWS.ELBV2.Types.RuleCondition
import Network.AWS.ELBV2.Types.RulePriorityPair
import Network.AWS.ELBV2.Types.SourceIpConditionConfig
import Network.AWS.ELBV2.Types.SslPolicy
import Network.AWS.ELBV2.Types.SubnetMapping
import Network.AWS.ELBV2.Types.Tag
import Network.AWS.ELBV2.Types.TagDescription
import Network.AWS.ELBV2.Types.TargetDescription
import Network.AWS.ELBV2.Types.TargetGroup
import Network.AWS.ELBV2.Types.TargetGroupAttribute
import Network.AWS.ELBV2.Types.TargetGroupIpAddressTypeEnum
import Network.AWS.ELBV2.Types.TargetGroupStickinessConfig
import Network.AWS.ELBV2.Types.TargetGroupTuple
import Network.AWS.ELBV2.Types.TargetHealth
import Network.AWS.ELBV2.Types.TargetHealthDescription
import Network.AWS.ELBV2.Types.TargetHealthReasonEnum
import Network.AWS.ELBV2.Types.TargetHealthStateEnum
import Network.AWS.ELBV2.Types.TargetTypeEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-12-01@ of the Amazon Elastic Load Balancing SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "ELBV2",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "elasticloadbalancing",
      Core._serviceSigningName = "elasticloadbalancing",
      Core._serviceVersion = "2015-12-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseXMLError "ELBV2",
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
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
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
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The requested configuration is not valid.
_InvalidConfigurationRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidConfigurationRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidConfigurationRequest"
    Prelude.. Core.hasStatus 400

-- | The specified subnet does not exist.
_SubnetNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetNotFoundException =
  Core._MatchServiceError
    defaultService
    "SubnetNotFound"
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

-- | The specified subnet is out of available addresses.
_InvalidSubnetException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSubnetException =
  Core._MatchServiceError
    defaultService
    "InvalidSubnet"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of rules per load balancer.
_TooManyRulesException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRulesException =
  Core._MatchServiceError
    defaultService
    "TooManyRules"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of target groups for your Amazon
-- Web Services account.
_TooManyTargetGroupsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTargetGroupsException =
  Core._MatchServiceError
    defaultService
    "TooManyTargetGroups"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of actions per rule.
_TooManyActionsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyActionsException =
  Core._MatchServiceError
    defaultService
    "TooManyActions"
    Prelude.. Core.hasStatus 400

-- | A load balancer with the specified name already exists.
_DuplicateLoadBalancerNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateLoadBalancerNameException =
  Core._MatchServiceError
    defaultService
    "DuplicateLoadBalancerName"
    Prelude.. Core.hasStatus 400

-- | The specified configuration is not valid with this protocol.
_IncompatibleProtocolsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IncompatibleProtocolsException =
  Core._MatchServiceError
    defaultService
    "IncompatibleProtocols"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of certificates per load
-- balancer.
_TooManyCertificatesException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyCertificatesException =
  Core._MatchServiceError
    defaultService
    "TooManyCertificates"
    Prelude.. Core.hasStatus 400

-- | A tag key was specified more than once.
_DuplicateTagKeysException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateTagKeysException =
  Core._MatchServiceError
    defaultService
    "DuplicateTagKeys"
    Prelude.. Core.hasStatus 400

-- | A listener with the specified port already exists.
_DuplicateListenerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateListenerException =
  Core._MatchServiceError
    defaultService
    "DuplicateListener"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of tags per load balancer.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTags"
    Prelude.. Core.hasStatus 400

-- | A target group with the specified name already exists.
_DuplicateTargetGroupNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateTargetGroupNameException =
  Core._MatchServiceError
    defaultService
    "DuplicateTargetGroupName"
    Prelude.. Core.hasStatus 400

-- | The health of the specified targets could not be retrieved due to an
-- internal error.
_HealthUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_HealthUnavailableException =
  Core._MatchServiceError
    defaultService
    "HealthUnavailable"
    Prelude.. Core.hasStatus 500

-- | The specified allocation ID does not exist.
_AllocationIdNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AllocationIdNotFoundException =
  Core._MatchServiceError
    defaultService
    "AllocationIdNotFound"
    Prelude.. Core.hasStatus 400

-- | The specified priority is in use.
_PriorityInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PriorityInUseException =
  Core._MatchServiceError
    defaultService
    "PriorityInUse"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of load balancers for your
-- Amazon Web Services account.
_TooManyLoadBalancersException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyLoadBalancersException =
  Core._MatchServiceError
    defaultService
    "TooManyLoadBalancers"
    Prelude.. Core.hasStatus 400

-- | The specified protocol is not supported.
_UnsupportedProtocolException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedProtocolException =
  Core._MatchServiceError
    defaultService
    "UnsupportedProtocol"
    Prelude.. Core.hasStatus 400

-- | The specified ALPN policy is not supported.
_ALPNPolicyNotSupportedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ALPNPolicyNotSupportedException =
  Core._MatchServiceError
    defaultService
    "ALPNPolicyNotFound"
    Prelude.. Core.hasStatus 400

-- | The specified target does not exist, is not in the same VPC as the
-- target group, or has an unsupported instance type.
_InvalidTargetException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTargetException =
  Core._MatchServiceError
    defaultService
    "InvalidTarget"
    Prelude.. Core.hasStatus 400

-- | The specified security group does not exist.
_InvalidSecurityGroupException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSecurityGroupException =
  Core._MatchServiceError
    defaultService
    "InvalidSecurityGroup"
    Prelude.. Core.hasStatus 400

-- | The specified target group does not exist.
_TargetGroupNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TargetGroupNotFoundException =
  Core._MatchServiceError
    defaultService
    "TargetGroupNotFound"
    Prelude.. Core.hasStatus 400

-- | The specified listener does not exist.
_ListenerNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ListenerNotFoundException =
  Core._MatchServiceError
    defaultService
    "ListenerNotFound"
    Prelude.. Core.hasStatus 400

-- | The requested action is not valid.
_InvalidLoadBalancerActionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidLoadBalancerActionException =
  Core._MatchServiceError
    defaultService
    "InvalidLoadBalancerAction"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of times a target can be
-- registered with a load balancer.
_TooManyRegistrationsForTargetIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRegistrationsForTargetIdException =
  Core._MatchServiceError
    defaultService
    "TooManyRegistrationsForTargetId"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of listeners per load balancer.
_TooManyListenersException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyListenersException =
  Core._MatchServiceError
    defaultService
    "TooManyListeners"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of load balancers per target
-- group.
_TargetGroupAssociationLimitException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TargetGroupAssociationLimitException =
  Core._MatchServiceError
    defaultService
    "TargetGroupAssociationLimit"
    Prelude.. Core.hasStatus 400

-- | This operation is not allowed.
_OperationNotPermittedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationNotPermittedException =
  Core._MatchServiceError
    defaultService
    "OperationNotPermitted"
    Prelude.. Core.hasStatus 400

-- | The specified SSL policy does not exist.
_SSLPolicyNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SSLPolicyNotFoundException =
  Core._MatchServiceError
    defaultService
    "SSLPolicyNotFound"
    Prelude.. Core.hasStatus 400

-- | The requested scheme is not valid.
_InvalidSchemeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSchemeException =
  Core._MatchServiceError
    defaultService
    "InvalidScheme"
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

-- | The specified load balancer does not exist.
_LoadBalancerNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LoadBalancerNotFoundException =
  Core._MatchServiceError
    defaultService
    "LoadBalancerNotFound"
    Prelude.. Core.hasStatus 400

-- | A specified resource is in use.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUse"
    Prelude.. Core.hasStatus 400

-- | The specified certificate does not exist.
_CertificateNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CertificateNotFoundException =
  Core._MatchServiceError
    defaultService
    "CertificateNotFound"
    Prelude.. Core.hasStatus 400
