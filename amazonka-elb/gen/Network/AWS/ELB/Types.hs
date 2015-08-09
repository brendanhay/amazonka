{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELB.Types
    (
    -- * Service
      ELB

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
    , _DuplicateAccessPointNameException
    , _InvalidSecurityGroupException
    , _ListenerNotFoundException
    , _InvalidEndPointException
    , _TooManyAccessPointsException
    , _InvalidSchemeException
    , _TooManyPoliciesException
    , _CertificateNotFoundException

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
    , lbdHealthCheck
    , lbdCanonicalHostedZoneName
    , lbdSecurityGroups
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

import           Network.AWS.ELB.Types.Product
import           Network.AWS.ELB.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2012-06-01@ of the Amazon Elastic Load Balancing SDK.
data ELB

instance AWSService ELB where
    type Sg ELB = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "ELB"
            , _svcPrefix = "elasticloadbalancing"
            , _svcVersion = "2012-06-01"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70
            , _svcStatus = statusSuccess
            , _svcError = parseXMLError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | One or more of the specified policies do not exist.
_PolicyNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_PolicyNotFoundException =
    _ServiceError . hasStatus 400 . hasCode "PolicyNotFound"

-- | The specified load balancer does not exist.
_AccessPointNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_AccessPointNotFoundException =
    _ServiceError . hasStatus 400 . hasCode "LoadBalancerNotFound"

-- | A policy with the specified name already exists for this load balancer.
_DuplicatePolicyNameException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicatePolicyNameException =
    _ServiceError . hasStatus 400 . hasCode "DuplicatePolicyName"

-- | The requested configuration change is not valid.
_InvalidConfigurationRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidConfigurationRequestException =
    _ServiceError . hasStatus 409 . hasCode "InvalidConfigurationRequest"

-- | One or more of the specified subnets do not exist.
_SubnetNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetNotFoundException =
    _ServiceError . hasStatus 400 . hasCode "SubnetNotFound"

-- | The specified load balancer attribute does not exist.
_LoadBalancerAttributeNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_LoadBalancerAttributeNotFoundException =
    _ServiceError . hasStatus 400 . hasCode "LoadBalancerAttributeNotFound"

-- | The specified VPC has no associated Internet gateway.
_InvalidSubnetException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSubnetException =
    _ServiceError . hasStatus 400 . hasCode "InvalidSubnet"

-- | A tag key was specified more than once.
_DuplicateTagKeysException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateTagKeysException =
    _ServiceError . hasStatus 400 . hasCode "DuplicateTagKeys"

-- | A listener already exists for the specified 'LoadBalancerName' and
-- 'LoadBalancerPort', but with a different 'InstancePort', 'Protocol', or
-- 'SSLCertificateId'.
_DuplicateListenerException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateListenerException =
    _ServiceError . hasStatus 400 . hasCode "DuplicateListener"

-- | The quota for the number of tags that can be assigned to a load balancer
-- has been reached.
_TooManyTagsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyTagsException = _ServiceError . hasStatus 400 . hasCode "TooManyTags"

-- | One or more of the specified policy types do not exist.
_PolicyTypeNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_PolicyTypeNotFoundException =
    _ServiceError . hasStatus 400 . hasCode "PolicyTypeNotFound"

-- | The specified load balancer name already exists for this account.
_DuplicateAccessPointNameException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateAccessPointNameException =
    _ServiceError . hasStatus 400 . hasCode "DuplicateLoadBalancerName"

-- | One or more of the specified security groups do not exist.
_InvalidSecurityGroupException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSecurityGroupException =
    _ServiceError . hasStatus 400 . hasCode "InvalidSecurityGroup"

-- | The load balancer does not have a listener configured at the specified
-- port.
_ListenerNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ListenerNotFoundException =
    _ServiceError . hasStatus 400 . hasCode "ListenerNotFound"

-- | The specified endpoint is not valid.
_InvalidEndPointException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidEndPointException =
    _ServiceError . hasStatus 400 . hasCode "InvalidInstance"

-- | The quota for the number of load balancers has been reached.
_TooManyAccessPointsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyAccessPointsException =
    _ServiceError . hasStatus 400 . hasCode "TooManyLoadBalancers"

-- | The specified value for the schema is not valid. You can only specify a
-- scheme for load balancers in a VPC.
_InvalidSchemeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSchemeException =
    _ServiceError . hasStatus 400 . hasCode "InvalidScheme"

-- | The quota for the number of policies for this load balancer has been
-- reached.
_TooManyPoliciesException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyPoliciesException =
    _ServiceError . hasStatus 400 . hasCode "TooManyPolicies"

-- | The specified SSL ID does not refer to a valid SSL certificate in AWS
-- Identity and Access Management (IAM).
_CertificateNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_CertificateNotFoundException =
    _ServiceError . hasStatus 400 . hasCode "CertificateNotFound"
