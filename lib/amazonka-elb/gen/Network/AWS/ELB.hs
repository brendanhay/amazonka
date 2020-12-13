{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Elastic Load Balancing__
--
-- A load balancer can distribute incoming traffic across your EC2 instances. This enables you to increase the availability of your application. The load balancer also monitors the health of its registered instances and ensures that it routes traffic only to healthy instances. You configure your load balancer to accept incoming traffic by specifying one or more listeners, which are configured with a protocol and port number for connections from clients to the load balancer and a protocol and port number for connections from the load balancer to the instances.
-- Elastic Load Balancing supports three types of load balancers: Application Load Balancers, Network Load Balancers, and Classic Load Balancers. You can select a load balancer based on your application needs. For more information, see the <https://docs.aws.amazon.com/elasticloadbalancing/latest/userguide/ Elastic Load Balancing User Guide> .
-- This reference covers the 2012-06-01 API, which supports Classic Load Balancers. The 2015-12-01 API supports Application Load Balancers and Network Load Balancers.
-- To get started, create a load balancer with one or more listeners using 'CreateLoadBalancer' . Register your instances with the load balancer using 'RegisterInstancesWithLoadBalancer' .
-- All Elastic Load Balancing operations are /idempotent/ , which means that they complete at most one time. If you repeat an operation, it succeeds with a 200 OK response code.
module Network.AWS.ELB
  ( -- * Service configuration
    elbService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- ** AnyInstanceInService
    mkAnyInstanceInService,

    -- ** InstanceDeregistered
    mkInstanceDeregistered,

    -- ** InstanceInService
    mkInstanceInService,

    -- * Operations
    -- $operations

    -- ** DescribeLoadBalancers (Paginated)
    module Network.AWS.ELB.DescribeLoadBalancers,

    -- ** DescribeTags
    module Network.AWS.ELB.DescribeTags,

    -- ** DescribeLoadBalancerPolicyTypes
    module Network.AWS.ELB.DescribeLoadBalancerPolicyTypes,

    -- ** ApplySecurityGroupsToLoadBalancer
    module Network.AWS.ELB.ApplySecurityGroupsToLoadBalancer,

    -- ** RemoveTags
    module Network.AWS.ELB.RemoveTags,

    -- ** CreateLBCookieStickinessPolicy
    module Network.AWS.ELB.CreateLBCookieStickinessPolicy,

    -- ** DeleteLoadBalancer
    module Network.AWS.ELB.DeleteLoadBalancer,

    -- ** DeregisterInstancesFromLoadBalancer
    module Network.AWS.ELB.DeregisterInstancesFromLoadBalancer,

    -- ** CreateLoadBalancerPolicy
    module Network.AWS.ELB.CreateLoadBalancerPolicy,

    -- ** DescribeLoadBalancerPolicies
    module Network.AWS.ELB.DescribeLoadBalancerPolicies,

    -- ** DisableAvailabilityZonesForLoadBalancer
    module Network.AWS.ELB.DisableAvailabilityZonesForLoadBalancer,

    -- ** EnableAvailabilityZonesForLoadBalancer
    module Network.AWS.ELB.EnableAvailabilityZonesForLoadBalancer,

    -- ** SetLoadBalancerPoliciesForBackendServer
    module Network.AWS.ELB.SetLoadBalancerPoliciesForBackendServer,

    -- ** SetLoadBalancerListenerSSLCertificate
    module Network.AWS.ELB.SetLoadBalancerListenerSSLCertificate,

    -- ** DescribeAccountLimits (Paginated)
    module Network.AWS.ELB.DescribeAccountLimits,

    -- ** AttachLoadBalancerToSubnets
    module Network.AWS.ELB.AttachLoadBalancerToSubnets,

    -- ** ConfigureHealthCheck
    module Network.AWS.ELB.ConfigureHealthCheck,

    -- ** ModifyLoadBalancerAttributes
    module Network.AWS.ELB.ModifyLoadBalancerAttributes,

    -- ** CreateAppCookieStickinessPolicy
    module Network.AWS.ELB.CreateAppCookieStickinessPolicy,

    -- ** DescribeInstanceHealth
    module Network.AWS.ELB.DescribeInstanceHealth,

    -- ** AddTags
    module Network.AWS.ELB.AddTags,

    -- ** DescribeLoadBalancerAttributes
    module Network.AWS.ELB.DescribeLoadBalancerAttributes,

    -- ** CreateLoadBalancerListeners
    module Network.AWS.ELB.CreateLoadBalancerListeners,

    -- ** DeleteLoadBalancerPolicy
    module Network.AWS.ELB.DeleteLoadBalancerPolicy,

    -- ** DetachLoadBalancerFromSubnets
    module Network.AWS.ELB.DetachLoadBalancerFromSubnets,

    -- ** RegisterInstancesWithLoadBalancer
    module Network.AWS.ELB.RegisterInstancesWithLoadBalancer,

    -- ** CreateLoadBalancer
    module Network.AWS.ELB.CreateLoadBalancer,

    -- ** DeleteLoadBalancerListeners
    module Network.AWS.ELB.DeleteLoadBalancerListeners,

    -- ** SetLoadBalancerPoliciesOfListener
    module Network.AWS.ELB.SetLoadBalancerPoliciesOfListener,

    -- * Types

    -- ** Common
    module Network.AWS.ELB.Internal,

    -- ** AccessLog
    AccessLog (..),
    mkAccessLog,
    alEmitInterval,
    alEnabled,
    alS3BucketPrefix,
    alS3BucketName,

    -- ** AdditionalAttribute
    AdditionalAttribute (..),
    mkAdditionalAttribute,
    aaValue,
    aaKey,

    -- ** AppCookieStickinessPolicy
    AppCookieStickinessPolicy (..),
    mkAppCookieStickinessPolicy,
    acspPolicyName,
    acspCookieName,

    -- ** BackendServerDescription
    BackendServerDescription (..),
    mkBackendServerDescription,
    bsdPolicyNames,
    bsdInstancePort,

    -- ** ConnectionDraining
    ConnectionDraining (..),
    mkConnectionDraining,
    cdEnabled,
    cdTimeout,

    -- ** ConnectionSettings
    ConnectionSettings (..),
    mkConnectionSettings,
    csIdleTimeout,

    -- ** CrossZoneLoadBalancing
    CrossZoneLoadBalancing (..),
    mkCrossZoneLoadBalancing,
    czlbEnabled,

    -- ** HealthCheck
    HealthCheck (..),
    mkHealthCheck,
    hcHealthyThreshold,
    hcInterval,
    hcTimeout,
    hcUnhealthyThreshold,
    hcTarget,

    -- ** Instance
    Instance (..),
    mkInstance,
    iInstanceId,

    -- ** InstanceState
    InstanceState (..),
    mkInstanceState,
    isInstanceId,
    isState,
    isReasonCode,
    isDescription,

    -- ** LBCookieStickinessPolicy
    LBCookieStickinessPolicy (..),
    mkLBCookieStickinessPolicy,
    lbcspPolicyName,
    lbcspCookieExpirationPeriod,

    -- ** Limit
    Limit (..),
    mkLimit,
    lMax,
    lName,

    -- ** Listener
    Listener (..),
    mkListener,
    lInstanceProtocol,
    lInstancePort,
    lLoadBalancerPort,
    lProtocol,
    lSSLCertificateId,

    -- ** ListenerDescription
    ListenerDescription (..),
    mkListenerDescription,
    ldPolicyNames,
    ldListener,

    -- ** LoadBalancerAttributes
    LoadBalancerAttributes (..),
    mkLoadBalancerAttributes,
    lbaCrossZoneLoadBalancing,
    lbaAccessLog,
    lbaAdditionalAttributes,
    lbaConnectionSettings,
    lbaConnectionDraining,

    -- ** LoadBalancerDescription
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

    -- ** Policies
    Policies (..),
    mkPolicies,
    pOtherPolicies,
    pLBCookieStickinessPolicies,
    pAppCookieStickinessPolicies,

    -- ** PolicyAttribute
    PolicyAttribute (..),
    mkPolicyAttribute,
    paAttributeValue,
    paAttributeName,

    -- ** PolicyAttributeDescription
    PolicyAttributeDescription (..),
    mkPolicyAttributeDescription,
    padAttributeValue,
    padAttributeName,

    -- ** PolicyAttributeTypeDescription
    PolicyAttributeTypeDescription (..),
    mkPolicyAttributeTypeDescription,
    patdAttributeType,
    patdCardinality,
    patdDefaultValue,
    patdAttributeName,
    patdDescription,

    -- ** PolicyDescription
    PolicyDescription (..),
    mkPolicyDescription,
    pdPolicyName,
    pdPolicyAttributeDescriptions,
    pdPolicyTypeName,

    -- ** PolicyTypeDescription
    PolicyTypeDescription (..),
    mkPolicyTypeDescription,
    ptdPolicyTypeName,
    ptdDescription,
    ptdPolicyAttributeTypeDescriptions,

    -- ** SourceSecurityGroup
    SourceSecurityGroup (..),
    mkSourceSecurityGroup,
    ssgOwnerAlias,
    ssgGroupName,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- ** TagDescription
    TagDescription (..),
    mkTagDescription,
    tdLoadBalancerName,
    tdTags,

    -- ** TagKeyOnly
    TagKeyOnly (..),
    mkTagKeyOnly,
    tkoKey,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
  )
where

import Network.AWS.ELB.AddTags
import Network.AWS.ELB.ApplySecurityGroupsToLoadBalancer
import Network.AWS.ELB.AttachLoadBalancerToSubnets
import Network.AWS.ELB.ConfigureHealthCheck
import Network.AWS.ELB.CreateAppCookieStickinessPolicy
import Network.AWS.ELB.CreateLBCookieStickinessPolicy
import Network.AWS.ELB.CreateLoadBalancer
import Network.AWS.ELB.CreateLoadBalancerListeners
import Network.AWS.ELB.CreateLoadBalancerPolicy
import Network.AWS.ELB.DeleteLoadBalancer
import Network.AWS.ELB.DeleteLoadBalancerListeners
import Network.AWS.ELB.DeleteLoadBalancerPolicy
import Network.AWS.ELB.DeregisterInstancesFromLoadBalancer
import Network.AWS.ELB.DescribeAccountLimits
import Network.AWS.ELB.DescribeInstanceHealth
import Network.AWS.ELB.DescribeLoadBalancerAttributes
import Network.AWS.ELB.DescribeLoadBalancerPolicies
import Network.AWS.ELB.DescribeLoadBalancerPolicyTypes
import Network.AWS.ELB.DescribeLoadBalancers
import Network.AWS.ELB.DescribeTags
import Network.AWS.ELB.DetachLoadBalancerFromSubnets
import Network.AWS.ELB.DisableAvailabilityZonesForLoadBalancer
import Network.AWS.ELB.EnableAvailabilityZonesForLoadBalancer
import Network.AWS.ELB.Internal
import Network.AWS.ELB.ModifyLoadBalancerAttributes
import Network.AWS.ELB.RegisterInstancesWithLoadBalancer
import Network.AWS.ELB.RemoveTags
import Network.AWS.ELB.SetLoadBalancerListenerSSLCertificate
import Network.AWS.ELB.SetLoadBalancerPoliciesForBackendServer
import Network.AWS.ELB.SetLoadBalancerPoliciesOfListener
import Network.AWS.ELB.Types
import Network.AWS.ELB.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ELB'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
