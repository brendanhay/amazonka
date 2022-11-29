{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ELB.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELB.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidSchemeException,
    _ListenerNotFoundException,
    _DuplicatePolicyNameException,
    _DuplicateTagKeysException,
    _PolicyTypeNotFoundException,
    _CertificateNotFoundException,
    _TooManyTagsException,
    _InvalidSecurityGroupException,
    _LoadBalancerAttributeNotFoundException,
    _DependencyThrottleException,
    _PolicyNotFoundException,
    _TooManyPoliciesException,
    _TooManyAccessPointsException,
    _AccessPointNotFoundException,
    _InvalidEndPointException,
    _InvalidSubnetException,
    _OperationNotPermittedException,
    _InvalidConfigurationRequestException,
    _DuplicateListenerException,
    _SubnetNotFoundException,
    _UnsupportedProtocolException,
    _DuplicateAccessPointNameException,

    -- * Re-exported Types
    module Amazonka.ELB.Internal,

    -- * AccessLog
    AccessLog (..),
    newAccessLog,
    accessLog_s3BucketPrefix,
    accessLog_s3BucketName,
    accessLog_emitInterval,
    accessLog_enabled,

    -- * AdditionalAttribute
    AdditionalAttribute (..),
    newAdditionalAttribute,
    additionalAttribute_key,
    additionalAttribute_value,

    -- * AppCookieStickinessPolicy
    AppCookieStickinessPolicy (..),
    newAppCookieStickinessPolicy,
    appCookieStickinessPolicy_policyName,
    appCookieStickinessPolicy_cookieName,

    -- * BackendServerDescription
    BackendServerDescription (..),
    newBackendServerDescription,
    backendServerDescription_policyNames,
    backendServerDescription_instancePort,

    -- * ConnectionDraining
    ConnectionDraining (..),
    newConnectionDraining,
    connectionDraining_timeout,
    connectionDraining_enabled,

    -- * ConnectionSettings
    ConnectionSettings (..),
    newConnectionSettings,
    connectionSettings_idleTimeout,

    -- * CrossZoneLoadBalancing
    CrossZoneLoadBalancing (..),
    newCrossZoneLoadBalancing,
    crossZoneLoadBalancing_enabled,

    -- * HealthCheck
    HealthCheck (..),
    newHealthCheck,
    healthCheck_target,
    healthCheck_interval,
    healthCheck_timeout,
    healthCheck_unhealthyThreshold,
    healthCheck_healthyThreshold,

    -- * Instance
    Instance (..),
    newInstance,
    instance_instanceId,

    -- * InstanceState
    InstanceState (..),
    newInstanceState,
    instanceState_state,
    instanceState_description,
    instanceState_instanceId,
    instanceState_reasonCode,

    -- * LBCookieStickinessPolicy
    LBCookieStickinessPolicy (..),
    newLBCookieStickinessPolicy,
    lBCookieStickinessPolicy_policyName,
    lBCookieStickinessPolicy_cookieExpirationPeriod,

    -- * Limit
    Limit (..),
    newLimit,
    limit_name,
    limit_max,

    -- * Listener
    Listener (..),
    newListener,
    listener_sSLCertificateId,
    listener_instanceProtocol,
    listener_protocol,
    listener_loadBalancerPort,
    listener_instancePort,

    -- * ListenerDescription
    ListenerDescription (..),
    newListenerDescription,
    listenerDescription_listener,
    listenerDescription_policyNames,

    -- * LoadBalancerAttributes
    LoadBalancerAttributes (..),
    newLoadBalancerAttributes,
    loadBalancerAttributes_connectionSettings,
    loadBalancerAttributes_connectionDraining,
    loadBalancerAttributes_additionalAttributes,
    loadBalancerAttributes_accessLog,
    loadBalancerAttributes_crossZoneLoadBalancing,

    -- * LoadBalancerDescription
    LoadBalancerDescription (..),
    newLoadBalancerDescription,
    loadBalancerDescription_instances,
    loadBalancerDescription_healthCheck,
    loadBalancerDescription_scheme,
    loadBalancerDescription_createdTime,
    loadBalancerDescription_canonicalHostedZoneName,
    loadBalancerDescription_canonicalHostedZoneNameID,
    loadBalancerDescription_loadBalancerName,
    loadBalancerDescription_subnets,
    loadBalancerDescription_sourceSecurityGroup,
    loadBalancerDescription_availabilityZones,
    loadBalancerDescription_listenerDescriptions,
    loadBalancerDescription_policies,
    loadBalancerDescription_backendServerDescriptions,
    loadBalancerDescription_securityGroups,
    loadBalancerDescription_vPCId,
    loadBalancerDescription_dNSName,

    -- * Policies
    Policies (..),
    newPolicies,
    policies_otherPolicies,
    policies_lBCookieStickinessPolicies,
    policies_appCookieStickinessPolicies,

    -- * PolicyAttribute
    PolicyAttribute (..),
    newPolicyAttribute,
    policyAttribute_attributeValue,
    policyAttribute_attributeName,

    -- * PolicyAttributeDescription
    PolicyAttributeDescription (..),
    newPolicyAttributeDescription,
    policyAttributeDescription_attributeValue,
    policyAttributeDescription_attributeName,

    -- * PolicyAttributeTypeDescription
    PolicyAttributeTypeDescription (..),
    newPolicyAttributeTypeDescription,
    policyAttributeTypeDescription_defaultValue,
    policyAttributeTypeDescription_description,
    policyAttributeTypeDescription_attributeType,
    policyAttributeTypeDescription_cardinality,
    policyAttributeTypeDescription_attributeName,

    -- * PolicyDescription
    PolicyDescription (..),
    newPolicyDescription,
    policyDescription_policyName,
    policyDescription_policyAttributeDescriptions,
    policyDescription_policyTypeName,

    -- * PolicyTypeDescription
    PolicyTypeDescription (..),
    newPolicyTypeDescription,
    policyTypeDescription_policyAttributeTypeDescriptions,
    policyTypeDescription_policyTypeName,
    policyTypeDescription_description,

    -- * SourceSecurityGroup
    SourceSecurityGroup (..),
    newSourceSecurityGroup,
    sourceSecurityGroup_ownerAlias,
    sourceSecurityGroup_groupName,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * TagDescription
    TagDescription (..),
    newTagDescription,
    tagDescription_tags,
    tagDescription_loadBalancerName,

    -- * TagKeyOnly
    TagKeyOnly (..),
    newTagKeyOnly,
    tagKeyOnly_key,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ELB.Internal
import Amazonka.ELB.Types.AccessLog
import Amazonka.ELB.Types.AdditionalAttribute
import Amazonka.ELB.Types.AppCookieStickinessPolicy
import Amazonka.ELB.Types.BackendServerDescription
import Amazonka.ELB.Types.ConnectionDraining
import Amazonka.ELB.Types.ConnectionSettings
import Amazonka.ELB.Types.CrossZoneLoadBalancing
import Amazonka.ELB.Types.HealthCheck
import Amazonka.ELB.Types.Instance
import Amazonka.ELB.Types.InstanceState
import Amazonka.ELB.Types.LBCookieStickinessPolicy
import Amazonka.ELB.Types.Limit
import Amazonka.ELB.Types.Listener
import Amazonka.ELB.Types.ListenerDescription
import Amazonka.ELB.Types.LoadBalancerAttributes
import Amazonka.ELB.Types.LoadBalancerDescription
import Amazonka.ELB.Types.Policies
import Amazonka.ELB.Types.PolicyAttribute
import Amazonka.ELB.Types.PolicyAttributeDescription
import Amazonka.ELB.Types.PolicyAttributeTypeDescription
import Amazonka.ELB.Types.PolicyDescription
import Amazonka.ELB.Types.PolicyTypeDescription
import Amazonka.ELB.Types.SourceSecurityGroup
import Amazonka.ELB.Types.Tag
import Amazonka.ELB.Types.TagDescription
import Amazonka.ELB.Types.TagKeyOnly
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2012-06-01@ of the Amazon Elastic Load Balancing SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "ELB",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "elasticloadbalancing",
      Core.signingName = "elasticloadbalancing",
      Core.version = "2012-06-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseXMLError "ELB",
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified value for the schema is not valid. You can only specify a
-- scheme for load balancers in a VPC.
_InvalidSchemeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSchemeException =
  Core._MatchServiceError
    defaultService
    "InvalidScheme"
    Prelude.. Core.hasStatus 400

-- | The load balancer does not have a listener configured at the specified
-- port.
_ListenerNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ListenerNotFoundException =
  Core._MatchServiceError
    defaultService
    "ListenerNotFound"
    Prelude.. Core.hasStatus 400

-- | A policy with the specified name already exists for this load balancer.
_DuplicatePolicyNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicatePolicyNameException =
  Core._MatchServiceError
    defaultService
    "DuplicatePolicyName"
    Prelude.. Core.hasStatus 400

-- | A tag key was specified more than once.
_DuplicateTagKeysException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateTagKeysException =
  Core._MatchServiceError
    defaultService
    "DuplicateTagKeys"
    Prelude.. Core.hasStatus 400

-- | One or more of the specified policy types do not exist.
_PolicyTypeNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PolicyTypeNotFoundException =
  Core._MatchServiceError
    defaultService
    "PolicyTypeNotFound"
    Prelude.. Core.hasStatus 400

-- | The specified ARN does not refer to a valid SSL certificate in AWS
-- Identity and Access Management (IAM) or AWS Certificate Manager (ACM).
-- Note that if you recently uploaded the certificate to IAM, this error
-- might indicate that the certificate is not fully available yet.
_CertificateNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CertificateNotFoundException =
  Core._MatchServiceError
    defaultService
    "CertificateNotFound"
    Prelude.. Core.hasStatus 400

-- | The quota for the number of tags that can be assigned to a load balancer
-- has been reached.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTags"
    Prelude.. Core.hasStatus 400

-- | One or more of the specified security groups do not exist.
_InvalidSecurityGroupException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSecurityGroupException =
  Core._MatchServiceError
    defaultService
    "InvalidSecurityGroup"
    Prelude.. Core.hasStatus 400

-- | The specified load balancer attribute does not exist.
_LoadBalancerAttributeNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LoadBalancerAttributeNotFoundException =
  Core._MatchServiceError
    defaultService
    "LoadBalancerAttributeNotFound"
    Prelude.. Core.hasStatus 400

-- | A request made by Elastic Load Balancing to another service exceeds the
-- maximum request rate permitted for your account.
_DependencyThrottleException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DependencyThrottleException =
  Core._MatchServiceError
    defaultService
    "DependencyThrottle"
    Prelude.. Core.hasStatus 400

-- | One or more of the specified policies do not exist.
_PolicyNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PolicyNotFoundException =
  Core._MatchServiceError
    defaultService
    "PolicyNotFound"
    Prelude.. Core.hasStatus 400

-- | The quota for the number of policies for this load balancer has been
-- reached.
_TooManyPoliciesException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyPoliciesException =
  Core._MatchServiceError
    defaultService
    "TooManyPolicies"
    Prelude.. Core.hasStatus 400

-- | The quota for the number of load balancers has been reached.
_TooManyAccessPointsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyAccessPointsException =
  Core._MatchServiceError
    defaultService
    "TooManyLoadBalancers"
    Prelude.. Core.hasStatus 400

-- | The specified load balancer does not exist.
_AccessPointNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessPointNotFoundException =
  Core._MatchServiceError
    defaultService
    "LoadBalancerNotFound"
    Prelude.. Core.hasStatus 400

-- | The specified endpoint is not valid.
_InvalidEndPointException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidEndPointException =
  Core._MatchServiceError
    defaultService
    "InvalidInstance"
    Prelude.. Core.hasStatus 400

-- | The specified VPC has no associated Internet gateway.
_InvalidSubnetException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSubnetException =
  Core._MatchServiceError
    defaultService
    "InvalidSubnet"
    Prelude.. Core.hasStatus 400

-- | This operation is not allowed.
_OperationNotPermittedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationNotPermittedException =
  Core._MatchServiceError
    defaultService
    "OperationNotPermitted"
    Prelude.. Core.hasStatus 400

-- | The requested configuration change is not valid.
_InvalidConfigurationRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidConfigurationRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidConfigurationRequest"
    Prelude.. Core.hasStatus 409

-- | A listener already exists for the specified load balancer name and port,
-- but with a different instance port, protocol, or SSL certificate.
_DuplicateListenerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateListenerException =
  Core._MatchServiceError
    defaultService
    "DuplicateListener"
    Prelude.. Core.hasStatus 400

-- | One or more of the specified subnets do not exist.
_SubnetNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetNotFoundException =
  Core._MatchServiceError
    defaultService
    "SubnetNotFound"
    Prelude.. Core.hasStatus 400

-- | The specified protocol or signature version is not supported.
_UnsupportedProtocolException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedProtocolException =
  Core._MatchServiceError
    defaultService
    "UnsupportedProtocol"
    Prelude.. Core.hasStatus 400

-- | The specified load balancer name already exists for this account.
_DuplicateAccessPointNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateAccessPointNameException =
  Core._MatchServiceError
    defaultService
    "DuplicateLoadBalancerName"
    Prelude.. Core.hasStatus 400
