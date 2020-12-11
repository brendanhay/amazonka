-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types
  ( -- * Service configuration
    elbService,

    -- * Errors

    -- * Re-exported types
    module Network.AWS.ELB.Internal,

    -- * AccessLog
    AccessLog (..),
    mkAccessLog,
    alEmitInterval,
    alS3BucketPrefix,
    alS3BucketName,
    alEnabled,

    -- * AdditionalAttribute
    AdditionalAttribute (..),
    mkAdditionalAttribute,
    aaValue,
    aaKey,

    -- * AppCookieStickinessPolicy
    AppCookieStickinessPolicy (..),
    mkAppCookieStickinessPolicy,
    acspPolicyName,
    acspCookieName,

    -- * BackendServerDescription
    BackendServerDescription (..),
    mkBackendServerDescription,
    bsdPolicyNames,
    bsdInstancePort,

    -- * ConnectionDraining
    ConnectionDraining (..),
    mkConnectionDraining,
    cdTimeout,
    cdEnabled,

    -- * ConnectionSettings
    ConnectionSettings (..),
    mkConnectionSettings,
    csIdleTimeout,

    -- * CrossZoneLoadBalancing
    CrossZoneLoadBalancing (..),
    mkCrossZoneLoadBalancing,
    czlbEnabled,

    -- * HealthCheck
    HealthCheck (..),
    mkHealthCheck,
    hcTarget,
    hcInterval,
    hcTimeout,
    hcUnhealthyThreshold,
    hcHealthyThreshold,

    -- * Instance
    Instance (..),
    mkInstance,
    iInstanceId,

    -- * InstanceState
    InstanceState (..),
    mkInstanceState,
    isInstanceId,
    isState,
    isReasonCode,
    isDescription,

    -- * LBCookieStickinessPolicy
    LBCookieStickinessPolicy (..),
    mkLBCookieStickinessPolicy,
    lbcspPolicyName,
    lbcspCookieExpirationPeriod,

    -- * Limit
    Limit (..),
    mkLimit,
    lMax,
    lName,

    -- * Listener
    Listener (..),
    mkListener,
    lInstanceProtocol,
    lSSLCertificateId,
    lProtocol,
    lLoadBalancerPort,
    lInstancePort,

    -- * ListenerDescription
    ListenerDescription (..),
    mkListenerDescription,
    ldPolicyNames,
    ldListener,

    -- * LoadBalancerAttributes
    LoadBalancerAttributes (..),
    mkLoadBalancerAttributes,
    lbaCrossZoneLoadBalancing,
    lbaAccessLog,
    lbaAdditionalAttributes,
    lbaConnectionSettings,
    lbaConnectionDraining,

    -- * LoadBalancerDescription
    LoadBalancerDescription (..),
    mkLoadBalancerDescription,
    lbdSourceSecurityGroup,
    lbdCanonicalHostedZoneName,
    lbdSecurityGroups,
    lbdHealthCheck,
    lbdLoadBalancerName,
    lbdCreatedTime,
    lbdVPCId,
    lbdSubnets,
    lbdAvailabilityZones,
    lbdBackendServerDescriptions,
    lbdCanonicalHostedZoneNameId,
    lbdInstances,
    lbdScheme,
    lbdListenerDescriptions,
    lbdDNSName,
    lbdPolicies,

    -- * Policies
    Policies (..),
    mkPolicies,
    pOtherPolicies,
    pLBCookieStickinessPolicies,
    pAppCookieStickinessPolicies,

    -- * PolicyAttribute
    PolicyAttribute (..),
    mkPolicyAttribute,
    paAttributeValue,
    paAttributeName,

    -- * PolicyAttributeDescription
    PolicyAttributeDescription (..),
    mkPolicyAttributeDescription,
    padAttributeValue,
    padAttributeName,

    -- * PolicyAttributeTypeDescription
    PolicyAttributeTypeDescription (..),
    mkPolicyAttributeTypeDescription,
    patdAttributeType,
    patdCardinality,
    patdDefaultValue,
    patdAttributeName,
    patdDescription,

    -- * PolicyDescription
    PolicyDescription (..),
    mkPolicyDescription,
    pdPolicyName,
    pdPolicyAttributeDescriptions,
    pdPolicyTypeName,

    -- * PolicyTypeDescription
    PolicyTypeDescription (..),
    mkPolicyTypeDescription,
    ptdPolicyTypeName,
    ptdDescription,
    ptdPolicyAttributeTypeDescriptions,

    -- * SourceSecurityGroup
    SourceSecurityGroup (..),
    mkSourceSecurityGroup,
    ssgOwnerAlias,
    ssgGroupName,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * TagDescription
    TagDescription (..),
    mkTagDescription,
    tdLoadBalancerName,
    tdTags,

    -- * TagKeyOnly
    TagKeyOnly (..),
    mkTagKeyOnly,
    tkoKey,
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
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2012-06-01@ of the Amazon Elastic Load Balancing SDK configuration.
elbService :: Lude.Service
elbService =
  Lude.Service
    { Lude._svcAbbrev = "ELB",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "elasticloadbalancing",
      Lude._svcVersion = "2012-06-01",
      Lude._svcEndpoint = Lude.defaultEndpoint elbService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseXMLError "ELB",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
