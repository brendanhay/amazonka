{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELB.Types
    (
    -- * Service Configuration
      elb

    -- * Errors
    , _PolicyNotFoundException
    , _AccessPointNotFoundException
    , _DuplicatePolicyNameException
    , _InvalidConfigurationRequestException
    , _SubnetNotFoundException
    , _LoadBalancerAttributeNotFoundException
    , _InvalidSubnetException
    , _DuplicateTagKeysException
    , _DuplicateListenerException
    , _TooManyTagsException
    , _PolicyTypeNotFoundException
    , _UnsupportedProtocolException
    , _DuplicateAccessPointNameException
    , _InvalidSecurityGroupException
    , _ListenerNotFoundException
    , _InvalidEndPointException
    , _OperationNotPermittedException
    , _DependencyThrottleException
    , _InvalidSchemeException
    , _TooManyAccessPointsException
    , _TooManyPoliciesException
    , _CertificateNotFoundException

    -- * Re-exported Types
    , module Network.AWS.ELB.Internal

    -- * AccessLog
    , AccessLog
    , accessLog
    , alEmitInterval
    , alS3BucketPrefix
    , alS3BucketName
    , alEnabled

    -- * AdditionalAttribute
    , AdditionalAttribute
    , additionalAttribute
    , aaValue
    , aaKey

    -- * AppCookieStickinessPolicy
    , AppCookieStickinessPolicy
    , appCookieStickinessPolicy
    , acspPolicyName
    , acspCookieName

    -- * BackendServerDescription
    , BackendServerDescription
    , backendServerDescription
    , bsdPolicyNames
    , bsdInstancePort

    -- * ConnectionDraining
    , ConnectionDraining
    , connectionDraining
    , cdTimeout
    , cdEnabled

    -- * ConnectionSettings
    , ConnectionSettings
    , connectionSettings
    , csIdleTimeout

    -- * CrossZoneLoadBalancing
    , CrossZoneLoadBalancing
    , crossZoneLoadBalancing
    , czlbEnabled

    -- * HealthCheck
    , HealthCheck
    , healthCheck
    , hcTarget
    , hcInterval
    , hcTimeout
    , hcUnhealthyThreshold
    , hcHealthyThreshold

    -- * Instance
    , Instance
    , instance'
    , iInstanceId

    -- * InstanceState
    , InstanceState
    , instanceState
    , isInstanceId
    , isState
    , isReasonCode
    , isDescription

    -- * LBCookieStickinessPolicy
    , LBCookieStickinessPolicy
    , lBCookieStickinessPolicy
    , lbcspPolicyName
    , lbcspCookieExpirationPeriod

    -- * Limit
    , Limit
    , limit
    , lMax
    , lName

    -- * Listener
    , Listener
    , listener
    , lInstanceProtocol
    , lSSLCertificateId
    , lProtocol
    , lLoadBalancerPort
    , lInstancePort

    -- * ListenerDescription
    , ListenerDescription
    , listenerDescription
    , ldPolicyNames
    , ldListener

    -- * LoadBalancerAttributes
    , LoadBalancerAttributes
    , loadBalancerAttributes
    , lbaCrossZoneLoadBalancing
    , lbaAccessLog
    , lbaAdditionalAttributes
    , lbaConnectionSettings
    , lbaConnectionDraining

    -- * LoadBalancerDescription
    , LoadBalancerDescription
    , loadBalancerDescription
    , lbdSourceSecurityGroup
    , lbdCanonicalHostedZoneName
    , lbdSecurityGroups
    , lbdHealthCheck
    , lbdLoadBalancerName
    , lbdCreatedTime
    , lbdVPCId
    , lbdSubnets
    , lbdAvailabilityZones
    , lbdBackendServerDescriptions
    , lbdCanonicalHostedZoneNameId
    , lbdInstances
    , lbdScheme
    , lbdListenerDescriptions
    , lbdDNSName
    , lbdPolicies

    -- * Policies
    , Policies
    , policies
    , pOtherPolicies
    , pLBCookieStickinessPolicies
    , pAppCookieStickinessPolicies

    -- * PolicyAttribute
    , PolicyAttribute
    , policyAttribute
    , paAttributeValue
    , paAttributeName

    -- * PolicyAttributeDescription
    , PolicyAttributeDescription
    , policyAttributeDescription
    , padAttributeValue
    , padAttributeName

    -- * PolicyAttributeTypeDescription
    , PolicyAttributeTypeDescription
    , policyAttributeTypeDescription
    , patdAttributeType
    , patdCardinality
    , patdDefaultValue
    , patdAttributeName
    , patdDescription

    -- * PolicyDescription
    , PolicyDescription
    , policyDescription
    , pdPolicyName
    , pdPolicyAttributeDescriptions
    , pdPolicyTypeName

    -- * PolicyTypeDescription
    , PolicyTypeDescription
    , policyTypeDescription
    , ptdPolicyTypeName
    , ptdDescription
    , ptdPolicyAttributeTypeDescriptions

    -- * SourceSecurityGroup
    , SourceSecurityGroup
    , sourceSecurityGroup
    , ssgOwnerAlias
    , ssgGroupName

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * TagDescription
    , TagDescription
    , tagDescription
    , tdLoadBalancerName
    , tdTags

    -- * TagKeyOnly
    , TagKeyOnly
    , tagKeyOnly
    , tkoKey
    ) where

import Network.AWS.ELB.Internal
import Network.AWS.ELB.Types.Product
import Network.AWS.ELB.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2012-06-01@ of the Amazon Elastic Load Balancing SDK configuration.
elb :: Service
elb =
  Service
    { _svcAbbrev = "ELB"
    , _svcSigner = v4
    , _svcPrefix = "elasticloadbalancing"
    , _svcVersion = "2012-06-01"
    , _svcEndpoint = defaultEndpoint elb
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseXMLError "ELB"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | One or more of the specified policies do not exist.
--
--
_PolicyNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_PolicyNotFoundException =
  _MatchServiceError elb "PolicyNotFound" . hasStatus 400


-- | The specified load balancer does not exist.
--
--
_AccessPointNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_AccessPointNotFoundException =
  _MatchServiceError elb "LoadBalancerNotFound" . hasStatus 400


-- | A policy with the specified name already exists for this load balancer.
--
--
_DuplicatePolicyNameException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicatePolicyNameException =
  _MatchServiceError elb "DuplicatePolicyName" . hasStatus 400


-- | The requested configuration change is not valid.
--
--
_InvalidConfigurationRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidConfigurationRequestException =
  _MatchServiceError elb "InvalidConfigurationRequest" . hasStatus 409


-- | One or more of the specified subnets do not exist.
--
--
_SubnetNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetNotFoundException =
  _MatchServiceError elb "SubnetNotFound" . hasStatus 400


-- | The specified load balancer attribute does not exist.
--
--
_LoadBalancerAttributeNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_LoadBalancerAttributeNotFoundException =
  _MatchServiceError elb "LoadBalancerAttributeNotFound" . hasStatus 400


-- | The specified VPC has no associated Internet gateway.
--
--
_InvalidSubnetException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSubnetException = _MatchServiceError elb "InvalidSubnet" . hasStatus 400


-- | A tag key was specified more than once.
--
--
_DuplicateTagKeysException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateTagKeysException =
  _MatchServiceError elb "DuplicateTagKeys" . hasStatus 400


-- | A listener already exists for the specified load balancer name and port, but with a different instance port, protocol, or SSL certificate.
--
--
_DuplicateListenerException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateListenerException =
  _MatchServiceError elb "DuplicateListener" . hasStatus 400


-- | The quota for the number of tags that can be assigned to a load balancer has been reached.
--
--
_TooManyTagsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyTagsException = _MatchServiceError elb "TooManyTags" . hasStatus 400


-- | One or more of the specified policy types do not exist.
--
--
_PolicyTypeNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_PolicyTypeNotFoundException =
  _MatchServiceError elb "PolicyTypeNotFound" . hasStatus 400


-- | The specified protocol or signature version is not supported.
--
--
_UnsupportedProtocolException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedProtocolException =
  _MatchServiceError elb "UnsupportedProtocol" . hasStatus 400


-- | The specified load balancer name already exists for this account.
--
--
_DuplicateAccessPointNameException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateAccessPointNameException =
  _MatchServiceError elb "DuplicateLoadBalancerName" . hasStatus 400


-- | One or more of the specified security groups do not exist.
--
--
_InvalidSecurityGroupException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSecurityGroupException =
  _MatchServiceError elb "InvalidSecurityGroup" . hasStatus 400


-- | The load balancer does not have a listener configured at the specified port.
--
--
_ListenerNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ListenerNotFoundException =
  _MatchServiceError elb "ListenerNotFound" . hasStatus 400


-- | The specified endpoint is not valid.
--
--
_InvalidEndPointException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidEndPointException =
  _MatchServiceError elb "InvalidInstance" . hasStatus 400


-- | This operation is not allowed.
--
--
_OperationNotPermittedException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationNotPermittedException =
  _MatchServiceError elb "OperationNotPermitted" . hasStatus 400


-- | Prism for DependencyThrottleException' errors.
_DependencyThrottleException :: AsError a => Getting (First ServiceError) a ServiceError
_DependencyThrottleException =
  _MatchServiceError elb "DependencyThrottle" . hasStatus 400


-- | The specified value for the schema is not valid. You can only specify a scheme for load balancers in a VPC.
--
--
_InvalidSchemeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSchemeException = _MatchServiceError elb "InvalidScheme" . hasStatus 400


-- | The quota for the number of load balancers has been reached.
--
--
_TooManyAccessPointsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyAccessPointsException =
  _MatchServiceError elb "TooManyLoadBalancers" . hasStatus 400


-- | The quota for the number of policies for this load balancer has been reached.
--
--
_TooManyPoliciesException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyPoliciesException =
  _MatchServiceError elb "TooManyPolicies" . hasStatus 400


-- | The specified ARN does not refer to a valid SSL certificate in AWS Identity and Access Management (IAM) or AWS Certificate Manager (ACM). Note that if you recently uploaded the certificate to IAM, this error might indicate that the certificate is not fully available yet.
--
--
_CertificateNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_CertificateNotFoundException =
  _MatchServiceError elb "CertificateNotFound" . hasStatus 400

