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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2012-06-01@ of the Amazon Elastic Load Balancing SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "ELB",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "elasticloadbalancing",
      Prelude._svcVersion = "2012-06-01",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError = Prelude.parseXMLError "ELB",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified load balancer name already exists for this account.
_DuplicateAccessPointNameException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DuplicateAccessPointNameException =
  Prelude._MatchServiceError
    defaultService
    "DuplicateLoadBalancerName"
    Prelude.. Prelude.hasStatus 400

-- | One or more of the specified security groups do not exist.
_InvalidSecurityGroupException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidSecurityGroupException =
  Prelude._MatchServiceError
    defaultService
    "InvalidSecurityGroup"
    Prelude.. Prelude.hasStatus 400

-- | The requested configuration change is not valid.
_InvalidConfigurationRequestException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidConfigurationRequestException =
  Prelude._MatchServiceError
    defaultService
    "InvalidConfigurationRequest"
    Prelude.. Prelude.hasStatus 409

-- | A policy with the specified name already exists for this load balancer.
_DuplicatePolicyNameException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DuplicatePolicyNameException =
  Prelude._MatchServiceError
    defaultService
    "DuplicatePolicyName"
    Prelude.. Prelude.hasStatus 400

-- | The specified load balancer does not exist.
_AccessPointNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AccessPointNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "LoadBalancerNotFound"
    Prelude.. Prelude.hasStatus 400

-- | One or more of the specified policy types do not exist.
_PolicyTypeNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PolicyTypeNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "PolicyTypeNotFound"
    Prelude.. Prelude.hasStatus 400

-- | The quota for the number of tags that can be assigned to a load balancer
-- has been reached.
_TooManyTagsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyTagsException =
  Prelude._MatchServiceError
    defaultService
    "TooManyTags"
    Prelude.. Prelude.hasStatus 400

-- | The quota for the number of policies for this load balancer has been
-- reached.
_TooManyPoliciesException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyPoliciesException =
  Prelude._MatchServiceError
    defaultService
    "TooManyPolicies"
    Prelude.. Prelude.hasStatus 400

-- | A tag key was specified more than once.
_DuplicateTagKeysException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DuplicateTagKeysException =
  Prelude._MatchServiceError
    defaultService
    "DuplicateTagKeys"
    Prelude.. Prelude.hasStatus 400

-- | A request made by Elastic Load Balancing to another service exceeds the
-- maximum request rate permitted for your account.
_DependencyThrottleException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DependencyThrottleException =
  Prelude._MatchServiceError
    defaultService
    "DependencyThrottle"
    Prelude.. Prelude.hasStatus 400

-- | This operation is not allowed.
_OperationNotPermittedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_OperationNotPermittedException =
  Prelude._MatchServiceError
    defaultService
    "OperationNotPermitted"
    Prelude.. Prelude.hasStatus 400

-- | The specified endpoint is not valid.
_InvalidEndPointException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidEndPointException =
  Prelude._MatchServiceError
    defaultService
    "InvalidInstance"
    Prelude.. Prelude.hasStatus 400

-- | The load balancer does not have a listener configured at the specified
-- port.
_ListenerNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ListenerNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ListenerNotFound"
    Prelude.. Prelude.hasStatus 400

-- | The specified load balancer attribute does not exist.
_LoadBalancerAttributeNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LoadBalancerAttributeNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "LoadBalancerAttributeNotFound"
    Prelude.. Prelude.hasStatus 400

-- | One or more of the specified subnets do not exist.
_SubnetNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SubnetNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "SubnetNotFound"
    Prelude.. Prelude.hasStatus 400

-- | The specified protocol or signature version is not supported.
_UnsupportedProtocolException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnsupportedProtocolException =
  Prelude._MatchServiceError
    defaultService
    "UnsupportedProtocol"
    Prelude.. Prelude.hasStatus 400

-- | One or more of the specified policies do not exist.
_PolicyNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PolicyNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "PolicyNotFound"
    Prelude.. Prelude.hasStatus 400

-- | The specified ARN does not refer to a valid SSL certificate in AWS
-- Identity and Access Management (IAM) or AWS Certificate Manager (ACM).
-- Note that if you recently uploaded the certificate to IAM, this error
-- might indicate that the certificate is not fully available yet.
_CertificateNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CertificateNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "CertificateNotFound"
    Prelude.. Prelude.hasStatus 400

-- | A listener already exists for the specified load balancer name and port,
-- but with a different instance port, protocol, or SSL certificate.
_DuplicateListenerException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DuplicateListenerException =
  Prelude._MatchServiceError
    defaultService
    "DuplicateListener"
    Prelude.. Prelude.hasStatus 400

-- | The quota for the number of load balancers has been reached.
_TooManyAccessPointsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyAccessPointsException =
  Prelude._MatchServiceError
    defaultService
    "TooManyLoadBalancers"
    Prelude.. Prelude.hasStatus 400

-- | The specified value for the schema is not valid. You can only specify a
-- scheme for load balancers in a VPC.
_InvalidSchemeException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidSchemeException =
  Prelude._MatchServiceError
    defaultService
    "InvalidScheme"
    Prelude.. Prelude.hasStatus 400

-- | The specified VPC has no associated Internet gateway.
_InvalidSubnetException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidSubnetException =
  Prelude._MatchServiceError
    defaultService
    "InvalidSubnet"
    Prelude.. Prelude.hasStatus 400
