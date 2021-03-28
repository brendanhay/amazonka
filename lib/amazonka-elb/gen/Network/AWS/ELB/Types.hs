-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELB.Types
    (
    -- * Service configuration
      mkServiceConfig

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

    -- * Re-exported types
    , module Network.AWS.ELB.Internal

    -- * InstanceId
    , InstanceId (..)

    -- * SourceSecurityGroup
    , SourceSecurityGroup (..)
    , mkSourceSecurityGroup
    , ssgGroupName
    , ssgOwnerAlias

    -- * TagDescription
    , TagDescription (..)
    , mkTagDescription
    , tdLoadBalancerName
    , tdTags

    -- * Max
    , Max (..)

    -- * State
    , State (..)

    -- * AccessPointName
    , AccessPointName (..)

    -- * LoadBalancerScheme
    , LoadBalancerScheme (..)

    -- * AttributeValue
    , AttributeValue (..)

    -- * PolicyName
    , PolicyName (..)

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * PolicyAttributeTypeDescription
    , PolicyAttributeTypeDescription (..)
    , mkPolicyAttributeTypeDescription
    , patdAttributeName
    , patdAttributeType
    , patdCardinality
    , patdDefaultValue
    , patdDescription

    -- * HealthCheck
    , HealthCheck (..)
    , mkHealthCheck
    , hcTarget
    , hcInterval
    , hcTimeout
    , hcUnhealthyThreshold
    , hcHealthyThreshold

    -- * CrossZoneLoadBalancing
    , CrossZoneLoadBalancing (..)
    , mkCrossZoneLoadBalancing
    , czlbEnabled

    -- * LoadBalancerAttributes
    , LoadBalancerAttributes (..)
    , mkLoadBalancerAttributes
    , lbaAccessLog
    , lbaAdditionalAttributes
    , lbaConnectionDraining
    , lbaConnectionSettings
    , lbaCrossZoneLoadBalancing

    -- * AccessLog
    , AccessLog (..)
    , mkAccessLog
    , alEnabled
    , alEmitInterval
    , alS3BucketName
    , alS3BucketPrefix

    -- * ListenerDescription
    , ListenerDescription (..)
    , mkListenerDescription
    , ldListener
    , ldPolicyNames

    -- * VPCId
    , VPCId (..)

    -- * Protocol
    , Protocol (..)

    -- * LBCookieStickinessPolicy
    , LBCookieStickinessPolicy (..)
    , mkLBCookieStickinessPolicy
    , lbcspCookieExpirationPeriod
    , lbcspPolicyName

    -- * PolicyDescription
    , PolicyDescription (..)
    , mkPolicyDescription
    , pdPolicyAttributeDescriptions
    , pdPolicyName
    , pdPolicyTypeName

    -- * SubnetId
    , SubnetId (..)

    -- * ReasonCode
    , ReasonCode (..)

    -- * AppCookieStickinessPolicy
    , AppCookieStickinessPolicy (..)
    , mkAppCookieStickinessPolicy
    , acspCookieName
    , acspPolicyName

    -- * SecurityGroupId
    , SecurityGroupId (..)

    -- * PolicyAttribute
    , PolicyAttribute (..)
    , mkPolicyAttribute
    , paAttributeName
    , paAttributeValue

    -- * LoadBalancerDescription
    , LoadBalancerDescription (..)
    , mkLoadBalancerDescription
    , lbdAvailabilityZones
    , lbdBackendServerDescriptions
    , lbdCanonicalHostedZoneName
    , lbdCanonicalHostedZoneNameID
    , lbdCreatedTime
    , lbdDNSName
    , lbdHealthCheck
    , lbdInstances
    , lbdListenerDescriptions
    , lbdLoadBalancerName
    , lbdPolicies
    , lbdScheme
    , lbdSecurityGroups
    , lbdSourceSecurityGroup
    , lbdSubnets
    , lbdVPCId

    -- * SSLCertificateId
    , SSLCertificateId (..)

    -- * AttributeType
    , AttributeType (..)

    -- * BackendServerDescription
    , BackendServerDescription (..)
    , mkBackendServerDescription
    , bsdInstancePort
    , bsdPolicyNames

    -- * PolicyTypeName
    , PolicyTypeName (..)

    -- * AvailabilityZone
    , AvailabilityZone (..)

    -- * Name
    , Name (..)

    -- * AdditionalAttributeKey
    , AdditionalAttributeKey (..)

    -- * PolicyAttributeDescription
    , PolicyAttributeDescription (..)
    , mkPolicyAttributeDescription
    , padAttributeName
    , padAttributeValue

    -- * Marker
    , Marker (..)

    -- * Cardinality
    , Cardinality (..)

    -- * Limit
    , Limit (..)
    , mkLimit
    , lMax
    , lName

    -- * AdditionalAttribute
    , AdditionalAttribute (..)
    , mkAdditionalAttribute
    , aaKey
    , aaValue

    -- * TagKey
    , TagKey (..)

    -- * DefaultValue
    , DefaultValue (..)

    -- * CookieName
    , CookieName (..)

    -- * AttributeName
    , AttributeName (..)

    -- * ConnectionSettings
    , ConnectionSettings (..)
    , mkConnectionSettings
    , csIdleTimeout

    -- * PolicyTypeDescription
    , PolicyTypeDescription (..)
    , mkPolicyTypeDescription
    , ptdDescription
    , ptdPolicyAttributeTypeDescriptions
    , ptdPolicyTypeName

    -- * Description
    , Description (..)

    -- * DNSName
    , DNSName (..)

    -- * Policies
    , Policies (..)
    , mkPolicies
    , pAppCookieStickinessPolicies
    , pLBCookieStickinessPolicies
    , pOtherPolicies

    -- * Listener
    , Listener (..)
    , mkListener
    , lProtocol
    , lLoadBalancerPort
    , lInstancePort
    , lInstanceProtocol
    , lSSLCertificateId

    -- * ConnectionDraining
    , ConnectionDraining (..)
    , mkConnectionDraining
    , cdEnabled
    , cdTimeout

    -- * InstanceState
    , InstanceState (..)
    , mkInstanceState
    , isDescription
    , isInstanceId
    , isReasonCode
    , isState

    -- * S3BucketName
    , S3BucketName (..)

    -- * TagKeyOnly
    , TagKeyOnly (..)
    , mkTagKeyOnly
    , tkoKey

    -- * Instance
    , Instance (..)
    , mkInstance
    , iInstanceId

    -- * LoadBalancerName
    , LoadBalancerName (..)

    -- * GroupName
    , GroupName (..)

    -- * OwnerAlias
    , OwnerAlias (..)

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)

    -- * Target
    , Target (..)

    -- * NextMarker
    , NextMarker (..)

    -- * S3BucketPrefix
    , S3BucketPrefix (..)

    -- * CanonicalHostedZoneName
    , CanonicalHostedZoneName (..)

    -- * CanonicalHostedZoneNameID
    , CanonicalHostedZoneNameID (..)
    ) where

import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
  
import Network.AWS.ELB.Types.InstanceId
  
import Network.AWS.ELB.Types.SourceSecurityGroup
  
import Network.AWS.ELB.Types.TagDescription
  
import Network.AWS.ELB.Types.Max
  
  
import Network.AWS.ELB.Types.State
  
  
import Network.AWS.ELB.Types.AccessPointName
  
import Network.AWS.ELB.Types.LoadBalancerScheme
  
  
  
import Network.AWS.ELB.Types.AttributeValue
  
import Network.AWS.ELB.Types.PolicyName
  
import Network.AWS.ELB.Types.Tag
  
  
import Network.AWS.ELB.Types.PolicyAttributeTypeDescription
  
import Network.AWS.ELB.Types.HealthCheck
  
import Network.AWS.ELB.Types.CrossZoneLoadBalancing
  
import Network.AWS.ELB.Types.LoadBalancerAttributes
  
  
import Network.AWS.ELB.Types.AccessLog
  
import Network.AWS.ELB.Types.ListenerDescription
  
import Network.AWS.ELB.Types.VPCId
  
import Network.AWS.ELB.Types.Protocol
  
  
  
import Network.AWS.ELB.Types.LBCookieStickinessPolicy
  
import Network.AWS.ELB.Types.PolicyDescription
  
import Network.AWS.ELB.Types.SubnetId
  
  
import Network.AWS.ELB.Types.ReasonCode
  
import Network.AWS.ELB.Types.AppCookieStickinessPolicy
  
import Network.AWS.ELB.Types.SecurityGroupId
  
import Network.AWS.ELB.Types.PolicyAttribute
  
  
import Network.AWS.ELB.Types.LoadBalancerDescription
  
  
import Network.AWS.ELB.Types.SSLCertificateId
  
import Network.AWS.ELB.Types.AttributeType
  
import Network.AWS.ELB.Types.BackendServerDescription
  
  
import Network.AWS.ELB.Types.PolicyTypeName
  
import Network.AWS.ELB.Types.AvailabilityZone
  
  
import Network.AWS.ELB.Types.Name
  
import Network.AWS.ELB.Types.AdditionalAttributeKey
  
import Network.AWS.ELB.Types.PolicyAttributeDescription
  
import Network.AWS.ELB.Types.Marker
  
import Network.AWS.ELB.Types.Cardinality
  
  
import Network.AWS.ELB.Types.Limit
  
  
import Network.AWS.ELB.Types.AdditionalAttribute
  
  
import Network.AWS.ELB.Types.TagKey
  
import Network.AWS.ELB.Types.DefaultValue
  
  
  
import Network.AWS.ELB.Types.CookieName
  
import Network.AWS.ELB.Types.AttributeName
  
  
import Network.AWS.ELB.Types.ConnectionSettings
  
import Network.AWS.ELB.Types.PolicyTypeDescription
  
import Network.AWS.ELB.Types.Description
  
  
import Network.AWS.ELB.Types.DNSName
  
import Network.AWS.ELB.Types.Policies
  
import Network.AWS.ELB.Types.Listener
  
import Network.AWS.ELB.Types.ConnectionDraining
  
import Network.AWS.ELB.Types.InstanceState
  
import Network.AWS.ELB.Types.S3BucketName
  
import Network.AWS.ELB.Types.TagKeyOnly
  
import Network.AWS.ELB.Types.Instance
  
  
import Network.AWS.ELB.Types.LoadBalancerName
  
import Network.AWS.ELB.Types.GroupName
  
import Network.AWS.ELB.Types.OwnerAlias
  
import Network.AWS.ELB.Types.Key
  
import Network.AWS.ELB.Types.Value
  
import Network.AWS.ELB.Types.Target
  
import Network.AWS.ELB.Types.NextMarker
  
import Network.AWS.ELB.Types.S3BucketPrefix
  
import Network.AWS.ELB.Types.CanonicalHostedZoneName
  
import Network.AWS.ELB.Types.CanonicalHostedZoneNameID
  

-- | API version @2012-06-01@ of the Amazon Elastic Load Balancing SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "ELB", Core._svcSigner = Sign.v4,
                 Core._svcPrefix = "elasticloadbalancing",
                 Core._svcVersion = "2012-06-01", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseXMLError "ELB",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | One or more of the specified policies do not exist.
_PolicyNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PolicyNotFoundException
  = Core._MatchServiceError mkServiceConfig "PolicyNotFound" Core..
      Core.hasStatues 400
{-# INLINEABLE _PolicyNotFoundException #-}
{-# DEPRECATED _PolicyNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The specified load balancer does not exist.
_AccessPointNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessPointNotFoundException
  = Core._MatchServiceError mkServiceConfig "LoadBalancerNotFound"
      Core.. Core.hasStatues 400
{-# INLINEABLE _AccessPointNotFoundException #-}
{-# DEPRECATED _AccessPointNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | A policy with the specified name already exists for this load balancer.
_DuplicatePolicyNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicatePolicyNameException
  = Core._MatchServiceError mkServiceConfig "DuplicatePolicyName"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DuplicatePolicyNameException #-}
{-# DEPRECATED _DuplicatePolicyNameException "Use generic-lens or generic-optics instead"  #-}

-- | The requested configuration change is not valid.
_InvalidConfigurationRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidConfigurationRequestException
  = Core._MatchServiceError mkServiceConfig
      "InvalidConfigurationRequest"
      Core.. Core.hasStatues 409
{-# INLINEABLE _InvalidConfigurationRequestException #-}
{-# DEPRECATED _InvalidConfigurationRequestException "Use generic-lens or generic-optics instead"  #-}

-- | One or more of the specified subnets do not exist.
_SubnetNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubnetNotFoundException
  = Core._MatchServiceError mkServiceConfig "SubnetNotFound" Core..
      Core.hasStatues 400
{-# INLINEABLE _SubnetNotFoundException #-}
{-# DEPRECATED _SubnetNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The specified load balancer attribute does not exist.
_LoadBalancerAttributeNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LoadBalancerAttributeNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "LoadBalancerAttributeNotFound"
      Core.. Core.hasStatues 400
{-# INLINEABLE _LoadBalancerAttributeNotFoundException #-}
{-# DEPRECATED _LoadBalancerAttributeNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The specified VPC has no associated Internet gateway.
_InvalidSubnetException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSubnetException
  = Core._MatchServiceError mkServiceConfig "InvalidSubnet" Core..
      Core.hasStatues 400
{-# INLINEABLE _InvalidSubnetException #-}
{-# DEPRECATED _InvalidSubnetException "Use generic-lens or generic-optics instead"  #-}

-- | A tag key was specified more than once.
_DuplicateTagKeysException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateTagKeysException
  = Core._MatchServiceError mkServiceConfig "DuplicateTagKeys" Core..
      Core.hasStatues 400
{-# INLINEABLE _DuplicateTagKeysException #-}
{-# DEPRECATED _DuplicateTagKeysException "Use generic-lens or generic-optics instead"  #-}

-- | A listener already exists for the specified load balancer name and port, but with a different instance port, protocol, or SSL certificate.
_DuplicateListenerException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateListenerException
  = Core._MatchServiceError mkServiceConfig "DuplicateListener"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DuplicateListenerException #-}
{-# DEPRECATED _DuplicateListenerException "Use generic-lens or generic-optics instead"  #-}

-- | The quota for the number of tags that can be assigned to a load balancer has been reached.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException
  = Core._MatchServiceError mkServiceConfig "TooManyTags" Core..
      Core.hasStatues 400
{-# INLINEABLE _TooManyTagsException #-}
{-# DEPRECATED _TooManyTagsException "Use generic-lens or generic-optics instead"  #-}

-- | One or more of the specified policy types do not exist.
_PolicyTypeNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PolicyTypeNotFoundException
  = Core._MatchServiceError mkServiceConfig "PolicyTypeNotFound"
      Core.. Core.hasStatues 400
{-# INLINEABLE _PolicyTypeNotFoundException #-}
{-# DEPRECATED _PolicyTypeNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The specified protocol or signature version is not supported.
_UnsupportedProtocolException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedProtocolException
  = Core._MatchServiceError mkServiceConfig "UnsupportedProtocol"
      Core.. Core.hasStatues 400
{-# INLINEABLE _UnsupportedProtocolException #-}
{-# DEPRECATED _UnsupportedProtocolException "Use generic-lens or generic-optics instead"  #-}

-- | The specified load balancer name already exists for this account.
_DuplicateAccessPointNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateAccessPointNameException
  = Core._MatchServiceError mkServiceConfig
      "DuplicateLoadBalancerName"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DuplicateAccessPointNameException #-}
{-# DEPRECATED _DuplicateAccessPointNameException "Use generic-lens or generic-optics instead"  #-}

-- | One or more of the specified security groups do not exist.
_InvalidSecurityGroupException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSecurityGroupException
  = Core._MatchServiceError mkServiceConfig "InvalidSecurityGroup"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidSecurityGroupException #-}
{-# DEPRECATED _InvalidSecurityGroupException "Use generic-lens or generic-optics instead"  #-}

-- | The load balancer does not have a listener configured at the specified port.
_ListenerNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ListenerNotFoundException
  = Core._MatchServiceError mkServiceConfig "ListenerNotFound" Core..
      Core.hasStatues 400
{-# INLINEABLE _ListenerNotFoundException #-}
{-# DEPRECATED _ListenerNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The specified endpoint is not valid.
_InvalidEndPointException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidEndPointException
  = Core._MatchServiceError mkServiceConfig "InvalidInstance" Core..
      Core.hasStatues 400
{-# INLINEABLE _InvalidEndPointException #-}
{-# DEPRECATED _InvalidEndPointException "Use generic-lens or generic-optics instead"  #-}

-- | This operation is not allowed.
_OperationNotPermittedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OperationNotPermittedException
  = Core._MatchServiceError mkServiceConfig "OperationNotPermitted"
      Core.. Core.hasStatues 400
{-# INLINEABLE _OperationNotPermittedException #-}
{-# DEPRECATED _OperationNotPermittedException "Use generic-lens or generic-optics instead"  #-}

-- | A request made by Elastic Load Balancing to another service exceeds the maximum request rate permitted for your account.
_DependencyThrottleException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DependencyThrottleException
  = Core._MatchServiceError mkServiceConfig "DependencyThrottle"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DependencyThrottleException #-}
{-# DEPRECATED _DependencyThrottleException "Use generic-lens or generic-optics instead"  #-}

-- | The specified value for the schema is not valid. You can only specify a scheme for load balancers in a VPC.
_InvalidSchemeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSchemeException
  = Core._MatchServiceError mkServiceConfig "InvalidScheme" Core..
      Core.hasStatues 400
{-# INLINEABLE _InvalidSchemeException #-}
{-# DEPRECATED _InvalidSchemeException "Use generic-lens or generic-optics instead"  #-}

-- | The quota for the number of load balancers has been reached.
_TooManyAccessPointsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyAccessPointsException
  = Core._MatchServiceError mkServiceConfig "TooManyLoadBalancers"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyAccessPointsException #-}
{-# DEPRECATED _TooManyAccessPointsException "Use generic-lens or generic-optics instead"  #-}

-- | The quota for the number of policies for this load balancer has been reached.
_TooManyPoliciesException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyPoliciesException
  = Core._MatchServiceError mkServiceConfig "TooManyPolicies" Core..
      Core.hasStatues 400
{-# INLINEABLE _TooManyPoliciesException #-}
{-# DEPRECATED _TooManyPoliciesException "Use generic-lens or generic-optics instead"  #-}

-- | The specified ARN does not refer to a valid SSL certificate in AWS Identity and Access Management (IAM) or AWS Certificate Manager (ACM). Note that if you recently uploaded the certificate to IAM, this error might indicate that the certificate is not fully available yet.
_CertificateNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CertificateNotFoundException
  = Core._MatchServiceError mkServiceConfig "CertificateNotFound"
      Core.. Core.hasStatues 400
{-# INLINEABLE _CertificateNotFoundException #-}
{-# DEPRECATED _CertificateNotFoundException "Use generic-lens or generic-optics instead"  #-}
