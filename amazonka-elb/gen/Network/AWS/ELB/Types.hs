{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _DuplicateAccessPointNameException,
    _InvalidSecurityGroupException,
    _InvalidConfigurationRequestException,
    _DuplicatePolicyNameException,
    _AccessPointNotFoundException,
    _PolicyTypeNotFoundException,
    _TooManyTagsException,
    _TooManyPoliciesException,
    _DuplicateTagKeysException,
    _DependencyThrottleException,
    _OperationNotPermittedException,
    _InvalidEndPointException,
    _ListenerNotFoundException,
    _LoadBalancerAttributeNotFoundException,
    _SubnetNotFoundException,
    _UnsupportedProtocolException,
    _PolicyNotFoundException,
    _CertificateNotFoundException,
    _DuplicateListenerException,
    _TooManyAccessPointsException,
    _InvalidSchemeException,
    _InvalidSubnetException,

    -- * Re-exported Types
    module Network.AWS.ELB.Internal,

    -- * AccessLog
    AccessLog (..),
    newAccessLog,
    accessLog_s3BucketPrefix,
    accessLog_emitInterval,
    accessLog_s3BucketName,
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
    backendServerDescription_instancePort,
    backendServerDescription_policyNames,

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
    instanceState_instanceId,
    instanceState_reasonCode,
    instanceState_state,
    instanceState_description,

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
    listener_instanceProtocol,
    listener_sSLCertificateId,
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
    loadBalancerAttributes_connectionDraining,
    loadBalancerAttributes_additionalAttributes,
    loadBalancerAttributes_connectionSettings,
    loadBalancerAttributes_accessLog,
    loadBalancerAttributes_crossZoneLoadBalancing,

    -- * LoadBalancerDescription
    LoadBalancerDescription (..),
    newLoadBalancerDescription,
    loadBalancerDescription_canonicalHostedZoneNameID,
    loadBalancerDescription_backendServerDescriptions,
    loadBalancerDescription_availabilityZones,
    loadBalancerDescription_policies,
    loadBalancerDescription_scheme,
    loadBalancerDescription_createdTime,
    loadBalancerDescription_instances,
    loadBalancerDescription_securityGroups,
    loadBalancerDescription_sourceSecurityGroup,
    loadBalancerDescription_dNSName,
    loadBalancerDescription_listenerDescriptions,
    loadBalancerDescription_subnets,
    loadBalancerDescription_vPCId,
    loadBalancerDescription_loadBalancerName,
    loadBalancerDescription_healthCheck,
    loadBalancerDescription_canonicalHostedZoneName,

    -- * Policies
    Policies (..),
    newPolicies,
    policies_appCookieStickinessPolicies,
    policies_lBCookieStickinessPolicies,
    policies_otherPolicies,

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
    policyAttributeTypeDescription_attributeType,
    policyAttributeTypeDescription_attributeName,
    policyAttributeTypeDescription_cardinality,
    policyAttributeTypeDescription_description,
    policyAttributeTypeDescription_defaultValue,

    -- * PolicyDescription
    PolicyDescription (..),
    newPolicyDescription,
    policyDescription_policyName,
    policyDescription_policyTypeName,
    policyDescription_policyAttributeDescriptions,

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

import qualified Network.AWS.Core as Core
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2012-06-01@ of the Amazon Elastic Load Balancing SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "ELB",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "elasticloadbalancing",
      Core._serviceSigningName = "elasticloadbalancing",
      Core._serviceVersion = "2012-06-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseXMLError "ELB",
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
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | The specified load balancer name already exists for this account.
_DuplicateAccessPointNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateAccessPointNameException =
  Core._MatchServiceError
    defaultService
    "DuplicateLoadBalancerName"
    Core.. Core.hasStatus 400

-- | One or more of the specified security groups do not exist.
_InvalidSecurityGroupException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSecurityGroupException =
  Core._MatchServiceError
    defaultService
    "InvalidSecurityGroup"
    Core.. Core.hasStatus 400

-- | The requested configuration change is not valid.
_InvalidConfigurationRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidConfigurationRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidConfigurationRequest"
    Core.. Core.hasStatus 409

-- | A policy with the specified name already exists for this load balancer.
_DuplicatePolicyNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicatePolicyNameException =
  Core._MatchServiceError
    defaultService
    "DuplicatePolicyName"
    Core.. Core.hasStatus 400

-- | The specified load balancer does not exist.
_AccessPointNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessPointNotFoundException =
  Core._MatchServiceError
    defaultService
    "LoadBalancerNotFound"
    Core.. Core.hasStatus 400

-- | One or more of the specified policy types do not exist.
_PolicyTypeNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PolicyTypeNotFoundException =
  Core._MatchServiceError
    defaultService
    "PolicyTypeNotFound"
    Core.. Core.hasStatus 400

-- | The quota for the number of tags that can be assigned to a load balancer
-- has been reached.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTags"
    Core.. Core.hasStatus 400

-- | The quota for the number of policies for this load balancer has been
-- reached.
_TooManyPoliciesException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyPoliciesException =
  Core._MatchServiceError
    defaultService
    "TooManyPolicies"
    Core.. Core.hasStatus 400

-- | A tag key was specified more than once.
_DuplicateTagKeysException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateTagKeysException =
  Core._MatchServiceError
    defaultService
    "DuplicateTagKeys"
    Core.. Core.hasStatus 400

-- | A request made by Elastic Load Balancing to another service exceeds the
-- maximum request rate permitted for your account.
_DependencyThrottleException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DependencyThrottleException =
  Core._MatchServiceError
    defaultService
    "DependencyThrottle"
    Core.. Core.hasStatus 400

-- | This operation is not allowed.
_OperationNotPermittedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OperationNotPermittedException =
  Core._MatchServiceError
    defaultService
    "OperationNotPermitted"
    Core.. Core.hasStatus 400

-- | The specified endpoint is not valid.
_InvalidEndPointException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidEndPointException =
  Core._MatchServiceError
    defaultService
    "InvalidInstance"
    Core.. Core.hasStatus 400

-- | The load balancer does not have a listener configured at the specified
-- port.
_ListenerNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ListenerNotFoundException =
  Core._MatchServiceError
    defaultService
    "ListenerNotFound"
    Core.. Core.hasStatus 400

-- | The specified load balancer attribute does not exist.
_LoadBalancerAttributeNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LoadBalancerAttributeNotFoundException =
  Core._MatchServiceError
    defaultService
    "LoadBalancerAttributeNotFound"
    Core.. Core.hasStatus 400

-- | One or more of the specified subnets do not exist.
_SubnetNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubnetNotFoundException =
  Core._MatchServiceError
    defaultService
    "SubnetNotFound"
    Core.. Core.hasStatus 400

-- | The specified protocol or signature version is not supported.
_UnsupportedProtocolException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedProtocolException =
  Core._MatchServiceError
    defaultService
    "UnsupportedProtocol"
    Core.. Core.hasStatus 400

-- | One or more of the specified policies do not exist.
_PolicyNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PolicyNotFoundException =
  Core._MatchServiceError
    defaultService
    "PolicyNotFound"
    Core.. Core.hasStatus 400

-- | The specified ARN does not refer to a valid SSL certificate in AWS
-- Identity and Access Management (IAM) or AWS Certificate Manager (ACM).
-- Note that if you recently uploaded the certificate to IAM, this error
-- might indicate that the certificate is not fully available yet.
_CertificateNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CertificateNotFoundException =
  Core._MatchServiceError
    defaultService
    "CertificateNotFound"
    Core.. Core.hasStatus 400

-- | A listener already exists for the specified load balancer name and port,
-- but with a different instance port, protocol, or SSL certificate.
_DuplicateListenerException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateListenerException =
  Core._MatchServiceError
    defaultService
    "DuplicateListener"
    Core.. Core.hasStatus 400

-- | The quota for the number of load balancers has been reached.
_TooManyAccessPointsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyAccessPointsException =
  Core._MatchServiceError
    defaultService
    "TooManyLoadBalancers"
    Core.. Core.hasStatus 400

-- | The specified value for the schema is not valid. You can only specify a
-- scheme for load balancers in a VPC.
_InvalidSchemeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSchemeException =
  Core._MatchServiceError
    defaultService
    "InvalidScheme"
    Core.. Core.hasStatus 400

-- | The specified VPC has no associated Internet gateway.
_InvalidSubnetException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSubnetException =
  Core._MatchServiceError
    defaultService
    "InvalidSubnet"
    Core.. Core.hasStatus 400
