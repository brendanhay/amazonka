{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ELBV2.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ALPNPolicyNotSupportedException,
    _AllocationIdNotFoundException,
    _AvailabilityZoneNotSupportedException,
    _CertificateNotFoundException,
    _DuplicateListenerException,
    _DuplicateLoadBalancerNameException,
    _DuplicateTagKeysException,
    _DuplicateTargetGroupNameException,
    _HealthUnavailableException,
    _IncompatibleProtocolsException,
    _InvalidConfigurationRequestException,
    _InvalidLoadBalancerActionException,
    _InvalidSchemeException,
    _InvalidSecurityGroupException,
    _InvalidSubnetException,
    _InvalidTargetException,
    _ListenerNotFoundException,
    _LoadBalancerNotFoundException,
    _OperationNotPermittedException,
    _PriorityInUseException,
    _ResourceInUseException,
    _RuleNotFoundException,
    _SSLPolicyNotFoundException,
    _SubnetNotFoundException,
    _TargetGroupAssociationLimitException,
    _TargetGroupNotFoundException,
    _TooManyActionsException,
    _TooManyCertificatesException,
    _TooManyListenersException,
    _TooManyLoadBalancersException,
    _TooManyRegistrationsForTargetIdException,
    _TooManyRulesException,
    _TooManyTagsException,
    _TooManyTargetGroupsException,
    _TooManyTargetsException,
    _TooManyUniqueTargetGroupsPerLoadBalancerException,
    _UnsupportedProtocolException,

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
    action_authenticateCognitoConfig,
    action_authenticateOidcConfig,
    action_fixedResponseConfig,
    action_forwardConfig,
    action_order,
    action_redirectConfig,
    action_targetGroupArn,
    action_type,

    -- * AuthenticateCognitoActionConfig
    AuthenticateCognitoActionConfig (..),
    newAuthenticateCognitoActionConfig,
    authenticateCognitoActionConfig_authenticationRequestExtraParams,
    authenticateCognitoActionConfig_onUnauthenticatedRequest,
    authenticateCognitoActionConfig_scope,
    authenticateCognitoActionConfig_sessionCookieName,
    authenticateCognitoActionConfig_sessionTimeout,
    authenticateCognitoActionConfig_userPoolArn,
    authenticateCognitoActionConfig_userPoolClientId,
    authenticateCognitoActionConfig_userPoolDomain,

    -- * AuthenticateOidcActionConfig
    AuthenticateOidcActionConfig (..),
    newAuthenticateOidcActionConfig,
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

    -- * AvailabilityZone
    AvailabilityZone (..),
    newAvailabilityZone,
    availabilityZone_loadBalancerAddresses,
    availabilityZone_outpostId,
    availabilityZone_subnetId,
    availabilityZone_zoneName,

    -- * Certificate
    Certificate (..),
    newCertificate,
    certificate_certificateArn,
    certificate_isDefault,

    -- * Cipher
    Cipher (..),
    newCipher,
    cipher_name,
    cipher_priority,

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
    httpHeaderConditionConfig_httpHeaderName,
    httpHeaderConditionConfig_values,

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
    listener_alpnPolicy,
    listener_certificates,
    listener_defaultActions,
    listener_listenerArn,
    listener_loadBalancerArn,
    listener_port,
    listener_protocol,
    listener_sslPolicy,

    -- * LoadBalancer
    LoadBalancer (..),
    newLoadBalancer,
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

    -- * LoadBalancerAddress
    LoadBalancerAddress (..),
    newLoadBalancerAddress,
    loadBalancerAddress_allocationId,
    loadBalancerAddress_iPv6Address,
    loadBalancerAddress_ipAddress,
    loadBalancerAddress_privateIPv4Address,

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
    redirectActionConfig_host,
    redirectActionConfig_path,
    redirectActionConfig_port,
    redirectActionConfig_protocol,
    redirectActionConfig_query,
    redirectActionConfig_statusCode,

    -- * Rule
    Rule (..),
    newRule,
    rule_actions,
    rule_conditions,
    rule_isDefault,
    rule_priority,
    rule_ruleArn,

    -- * RuleCondition
    RuleCondition (..),
    newRuleCondition,
    ruleCondition_field,
    ruleCondition_hostHeaderConfig,
    ruleCondition_httpHeaderConfig,
    ruleCondition_httpRequestMethodConfig,
    ruleCondition_pathPatternConfig,
    ruleCondition_queryStringConfig,
    ruleCondition_sourceIpConfig,
    ruleCondition_values,

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
    sslPolicy_ciphers,
    sslPolicy_name,
    sslPolicy_sslProtocols,
    sslPolicy_supportedLoadBalancerTypes,

    -- * SubnetMapping
    SubnetMapping (..),
    newSubnetMapping,
    subnetMapping_allocationId,
    subnetMapping_iPv6Address,
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

    -- * TargetGroupAttribute
    TargetGroupAttribute (..),
    newTargetGroupAttribute,
    targetGroupAttribute_key,
    targetGroupAttribute_value,

    -- * TargetGroupStickinessConfig
    TargetGroupStickinessConfig (..),
    newTargetGroupStickinessConfig,
    targetGroupStickinessConfig_durationSeconds,
    targetGroupStickinessConfig_enabled,

    -- * TargetGroupTuple
    TargetGroupTuple (..),
    newTargetGroupTuple,
    targetGroupTuple_targetGroupArn,
    targetGroupTuple_weight,

    -- * TargetHealth
    TargetHealth (..),
    newTargetHealth,
    targetHealth_description,
    targetHealth_reason,
    targetHealth_state,

    -- * TargetHealthDescription
    TargetHealthDescription (..),
    newTargetHealthDescription,
    targetHealthDescription_healthCheckPort,
    targetHealthDescription_target,
    targetHealthDescription_targetHealth,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ELBV2.Types.Action
import Amazonka.ELBV2.Types.ActionTypeEnum
import Amazonka.ELBV2.Types.AuthenticateCognitoActionConditionalBehaviorEnum
import Amazonka.ELBV2.Types.AuthenticateCognitoActionConfig
import Amazonka.ELBV2.Types.AuthenticateOidcActionConditionalBehaviorEnum
import Amazonka.ELBV2.Types.AuthenticateOidcActionConfig
import Amazonka.ELBV2.Types.AvailabilityZone
import Amazonka.ELBV2.Types.Certificate
import Amazonka.ELBV2.Types.Cipher
import Amazonka.ELBV2.Types.FixedResponseActionConfig
import Amazonka.ELBV2.Types.ForwardActionConfig
import Amazonka.ELBV2.Types.HostHeaderConditionConfig
import Amazonka.ELBV2.Types.HttpHeaderConditionConfig
import Amazonka.ELBV2.Types.HttpRequestMethodConditionConfig
import Amazonka.ELBV2.Types.IpAddressType
import Amazonka.ELBV2.Types.Limit
import Amazonka.ELBV2.Types.Listener
import Amazonka.ELBV2.Types.LoadBalancer
import Amazonka.ELBV2.Types.LoadBalancerAddress
import Amazonka.ELBV2.Types.LoadBalancerAttribute
import Amazonka.ELBV2.Types.LoadBalancerSchemeEnum
import Amazonka.ELBV2.Types.LoadBalancerState
import Amazonka.ELBV2.Types.LoadBalancerStateEnum
import Amazonka.ELBV2.Types.LoadBalancerTypeEnum
import Amazonka.ELBV2.Types.Matcher
import Amazonka.ELBV2.Types.PathPatternConditionConfig
import Amazonka.ELBV2.Types.ProtocolEnum
import Amazonka.ELBV2.Types.QueryStringConditionConfig
import Amazonka.ELBV2.Types.QueryStringKeyValuePair
import Amazonka.ELBV2.Types.RedirectActionConfig
import Amazonka.ELBV2.Types.RedirectActionStatusCodeEnum
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
import Amazonka.ELBV2.Types.TargetGroupIpAddressTypeEnum
import Amazonka.ELBV2.Types.TargetGroupStickinessConfig
import Amazonka.ELBV2.Types.TargetGroupTuple
import Amazonka.ELBV2.Types.TargetHealth
import Amazonka.ELBV2.Types.TargetHealthDescription
import Amazonka.ELBV2.Types.TargetHealthReasonEnum
import Amazonka.ELBV2.Types.TargetHealthStateEnum
import Amazonka.ELBV2.Types.TargetTypeEnum
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2015-12-01@ of the Amazon Elastic Load Balancing SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "ELBV2",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "elasticloadbalancing",
      Core.signingName = "elasticloadbalancing",
      Core.version = "2015-12-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseXMLError "ELBV2",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified ALPN policy is not supported.
_ALPNPolicyNotSupportedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ALPNPolicyNotSupportedException =
  Core._MatchServiceError
    defaultService
    "ALPNPolicyNotFound"
    Prelude.. Core.hasStatus 400

-- | The specified allocation ID does not exist.
_AllocationIdNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AllocationIdNotFoundException =
  Core._MatchServiceError
    defaultService
    "AllocationIdNotFound"
    Prelude.. Core.hasStatus 400

-- | The specified Availability Zone is not supported.
_AvailabilityZoneNotSupportedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AvailabilityZoneNotSupportedException =
  Core._MatchServiceError
    defaultService
    "AvailabilityZoneNotSupported"
    Prelude.. Core.hasStatus 400

-- | The specified certificate does not exist.
_CertificateNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CertificateNotFoundException =
  Core._MatchServiceError
    defaultService
    "CertificateNotFound"
    Prelude.. Core.hasStatus 400

-- | A listener with the specified port already exists.
_DuplicateListenerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DuplicateListenerException =
  Core._MatchServiceError
    defaultService
    "DuplicateListener"
    Prelude.. Core.hasStatus 400

-- | A load balancer with the specified name already exists.
_DuplicateLoadBalancerNameException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DuplicateLoadBalancerNameException =
  Core._MatchServiceError
    defaultService
    "DuplicateLoadBalancerName"
    Prelude.. Core.hasStatus 400

-- | A tag key was specified more than once.
_DuplicateTagKeysException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DuplicateTagKeysException =
  Core._MatchServiceError
    defaultService
    "DuplicateTagKeys"
    Prelude.. Core.hasStatus 400

-- | A target group with the specified name already exists.
_DuplicateTargetGroupNameException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DuplicateTargetGroupNameException =
  Core._MatchServiceError
    defaultService
    "DuplicateTargetGroupName"
    Prelude.. Core.hasStatus 400

-- | The health of the specified targets could not be retrieved due to an
-- internal error.
_HealthUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_HealthUnavailableException =
  Core._MatchServiceError
    defaultService
    "HealthUnavailable"
    Prelude.. Core.hasStatus 500

-- | The specified configuration is not valid with this protocol.
_IncompatibleProtocolsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_IncompatibleProtocolsException =
  Core._MatchServiceError
    defaultService
    "IncompatibleProtocols"
    Prelude.. Core.hasStatus 400

-- | The requested configuration is not valid.
_InvalidConfigurationRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidConfigurationRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidConfigurationRequest"
    Prelude.. Core.hasStatus 400

-- | The requested action is not valid.
_InvalidLoadBalancerActionException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidLoadBalancerActionException =
  Core._MatchServiceError
    defaultService
    "InvalidLoadBalancerAction"
    Prelude.. Core.hasStatus 400

-- | The requested scheme is not valid.
_InvalidSchemeException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidSchemeException =
  Core._MatchServiceError
    defaultService
    "InvalidScheme"
    Prelude.. Core.hasStatus 400

-- | The specified security group does not exist.
_InvalidSecurityGroupException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidSecurityGroupException =
  Core._MatchServiceError
    defaultService
    "InvalidSecurityGroup"
    Prelude.. Core.hasStatus 400

-- | The specified subnet is out of available addresses.
_InvalidSubnetException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidSubnetException =
  Core._MatchServiceError
    defaultService
    "InvalidSubnet"
    Prelude.. Core.hasStatus 400

-- | The specified target does not exist, is not in the same VPC as the
-- target group, or has an unsupported instance type.
_InvalidTargetException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidTargetException =
  Core._MatchServiceError
    defaultService
    "InvalidTarget"
    Prelude.. Core.hasStatus 400

-- | The specified listener does not exist.
_ListenerNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ListenerNotFoundException =
  Core._MatchServiceError
    defaultService
    "ListenerNotFound"
    Prelude.. Core.hasStatus 400

-- | The specified load balancer does not exist.
_LoadBalancerNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LoadBalancerNotFoundException =
  Core._MatchServiceError
    defaultService
    "LoadBalancerNotFound"
    Prelude.. Core.hasStatus 400

-- | This operation is not allowed.
_OperationNotPermittedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OperationNotPermittedException =
  Core._MatchServiceError
    defaultService
    "OperationNotPermitted"
    Prelude.. Core.hasStatus 400

-- | The specified priority is in use.
_PriorityInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PriorityInUseException =
  Core._MatchServiceError
    defaultService
    "PriorityInUse"
    Prelude.. Core.hasStatus 400

-- | A specified resource is in use.
_ResourceInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUse"
    Prelude.. Core.hasStatus 400

-- | The specified rule does not exist.
_RuleNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_RuleNotFoundException =
  Core._MatchServiceError
    defaultService
    "RuleNotFound"
    Prelude.. Core.hasStatus 400

-- | The specified SSL policy does not exist.
_SSLPolicyNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SSLPolicyNotFoundException =
  Core._MatchServiceError
    defaultService
    "SSLPolicyNotFound"
    Prelude.. Core.hasStatus 400

-- | The specified subnet does not exist.
_SubnetNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubnetNotFoundException =
  Core._MatchServiceError
    defaultService
    "SubnetNotFound"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of load balancers per target
-- group.
_TargetGroupAssociationLimitException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TargetGroupAssociationLimitException =
  Core._MatchServiceError
    defaultService
    "TargetGroupAssociationLimit"
    Prelude.. Core.hasStatus 400

-- | The specified target group does not exist.
_TargetGroupNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TargetGroupNotFoundException =
  Core._MatchServiceError
    defaultService
    "TargetGroupNotFound"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of actions per rule.
_TooManyActionsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyActionsException =
  Core._MatchServiceError
    defaultService
    "TooManyActions"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of certificates per load
-- balancer.
_TooManyCertificatesException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyCertificatesException =
  Core._MatchServiceError
    defaultService
    "TooManyCertificates"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of listeners per load balancer.
_TooManyListenersException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyListenersException =
  Core._MatchServiceError
    defaultService
    "TooManyListeners"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of load balancers for your
-- Amazon Web Services account.
_TooManyLoadBalancersException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyLoadBalancersException =
  Core._MatchServiceError
    defaultService
    "TooManyLoadBalancers"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of times a target can be
-- registered with a load balancer.
_TooManyRegistrationsForTargetIdException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyRegistrationsForTargetIdException =
  Core._MatchServiceError
    defaultService
    "TooManyRegistrationsForTargetId"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of rules per load balancer.
_TooManyRulesException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyRulesException =
  Core._MatchServiceError
    defaultService
    "TooManyRules"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of tags per load balancer.
_TooManyTagsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTags"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of target groups for your Amazon
-- Web Services account.
_TooManyTargetGroupsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyTargetGroupsException =
  Core._MatchServiceError
    defaultService
    "TooManyTargetGroups"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of targets.
_TooManyTargetsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyTargetsException =
  Core._MatchServiceError
    defaultService
    "TooManyTargets"
    Prelude.. Core.hasStatus 400

-- | You\'ve reached the limit on the number of unique target groups per load
-- balancer across all listeners. If a target group is used by multiple
-- actions for a load balancer, it is counted as only one use.
_TooManyUniqueTargetGroupsPerLoadBalancerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyUniqueTargetGroupsPerLoadBalancerException =
  Core._MatchServiceError
    defaultService
    "TooManyUniqueTargetGroupsPerLoadBalancer"
    Prelude.. Core.hasStatus 400

-- | The specified protocol is not supported.
_UnsupportedProtocolException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnsupportedProtocolException =
  Core._MatchServiceError
    defaultService
    "UnsupportedProtocol"
    Prelude.. Core.hasStatus 400
