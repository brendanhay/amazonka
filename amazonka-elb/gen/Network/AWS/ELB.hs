{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Elastic Load Balancing__
--
-- A load balancer can distribute incoming traffic across your EC2 instances. This enables you to increase the availability of your application. The load balancer also monitors the health of its registered instances and ensures that it routes traffic only to healthy instances. You configure your load balancer to accept incoming traffic by specifying one or more listeners, which are configured with a protocol and port number for connections from clients to the load balancer and a protocol and port number for connections from the load balancer to the instances.
--
-- Elastic Load Balancing supports three types of load balancers: Application Load Balancers, Network Load Balancers, and Classic Load Balancers. You can select a load balancer based on your application needs. For more information, see the <http://docs.aws.amazon.com/elasticloadbalancing/latest/userguide/ Elastic Load Balancing User Guide> .
--
-- This reference covers the 2012-06-01 API, which supports Classic Load Balancers. The 2015-12-01 API supports Application Load Balancers and Network Load Balancers.
--
-- To get started, create a load balancer with one or more listeners using 'CreateLoadBalancer' . Register your instances with the load balancer using 'RegisterInstancesWithLoadBalancer' .
--
-- All Elastic Load Balancing operations are /idempotent/ , which means that they complete at most one time. If you repeat an operation, it succeeds with a 200 OK response code.
--
module Network.AWS.ELB
    (
    -- * Service Configuration
      elb

    -- * Errors
    -- $errors

    -- ** PolicyNotFoundException
    , _PolicyNotFoundException

    -- ** AccessPointNotFoundException
    , _AccessPointNotFoundException

    -- ** DuplicatePolicyNameException
    , _DuplicatePolicyNameException

    -- ** InvalidConfigurationRequestException
    , _InvalidConfigurationRequestException

    -- ** SubnetNotFoundException
    , _SubnetNotFoundException

    -- ** LoadBalancerAttributeNotFoundException
    , _LoadBalancerAttributeNotFoundException

    -- ** InvalidSubnetException
    , _InvalidSubnetException

    -- ** DuplicateTagKeysException
    , _DuplicateTagKeysException

    -- ** DuplicateListenerException
    , _DuplicateListenerException

    -- ** TooManyTagsException
    , _TooManyTagsException

    -- ** PolicyTypeNotFoundException
    , _PolicyTypeNotFoundException

    -- ** UnsupportedProtocolException
    , _UnsupportedProtocolException

    -- ** DuplicateAccessPointNameException
    , _DuplicateAccessPointNameException

    -- ** InvalidSecurityGroupException
    , _InvalidSecurityGroupException

    -- ** ListenerNotFoundException
    , _ListenerNotFoundException

    -- ** InvalidEndPointException
    , _InvalidEndPointException

    -- ** OperationNotPermittedException
    , _OperationNotPermittedException

    -- ** DependencyThrottleException
    , _DependencyThrottleException

    -- ** InvalidSchemeException
    , _InvalidSchemeException

    -- ** TooManyAccessPointsException
    , _TooManyAccessPointsException

    -- ** TooManyPoliciesException
    , _TooManyPoliciesException

    -- ** CertificateNotFoundException
    , _CertificateNotFoundException

    -- * Waiters
    -- $waiters

    -- ** AnyInstanceInService
    , anyInstanceInService

    -- ** InstanceDeregistered
    , instanceDeregistered

    -- ** InstanceInService
    , instanceInService

    -- * Operations
    -- $operations

    -- ** DescribeLoadBalancers (Paginated)
    , module Network.AWS.ELB.DescribeLoadBalancers

    -- ** DescribeTags
    , module Network.AWS.ELB.DescribeTags

    -- ** DescribeLoadBalancerPolicyTypes
    , module Network.AWS.ELB.DescribeLoadBalancerPolicyTypes

    -- ** ApplySecurityGroupsToLoadBalancer
    , module Network.AWS.ELB.ApplySecurityGroupsToLoadBalancer

    -- ** RemoveTags
    , module Network.AWS.ELB.RemoveTags

    -- ** CreateLBCookieStickinessPolicy
    , module Network.AWS.ELB.CreateLBCookieStickinessPolicy

    -- ** DeleteLoadBalancer
    , module Network.AWS.ELB.DeleteLoadBalancer

    -- ** DeregisterInstancesFromLoadBalancer
    , module Network.AWS.ELB.DeregisterInstancesFromLoadBalancer

    -- ** CreateLoadBalancerPolicy
    , module Network.AWS.ELB.CreateLoadBalancerPolicy

    -- ** DescribeLoadBalancerPolicies
    , module Network.AWS.ELB.DescribeLoadBalancerPolicies

    -- ** DisableAvailabilityZonesForLoadBalancer
    , module Network.AWS.ELB.DisableAvailabilityZonesForLoadBalancer

    -- ** EnableAvailabilityZonesForLoadBalancer
    , module Network.AWS.ELB.EnableAvailabilityZonesForLoadBalancer

    -- ** SetLoadBalancerPoliciesForBackendServer
    , module Network.AWS.ELB.SetLoadBalancerPoliciesForBackendServer

    -- ** SetLoadBalancerListenerSSLCertificate
    , module Network.AWS.ELB.SetLoadBalancerListenerSSLCertificate

    -- ** DescribeAccountLimits
    , module Network.AWS.ELB.DescribeAccountLimits

    -- ** AttachLoadBalancerToSubnets
    , module Network.AWS.ELB.AttachLoadBalancerToSubnets

    -- ** ConfigureHealthCheck
    , module Network.AWS.ELB.ConfigureHealthCheck

    -- ** ModifyLoadBalancerAttributes
    , module Network.AWS.ELB.ModifyLoadBalancerAttributes

    -- ** CreateAppCookieStickinessPolicy
    , module Network.AWS.ELB.CreateAppCookieStickinessPolicy

    -- ** DescribeInstanceHealth
    , module Network.AWS.ELB.DescribeInstanceHealth

    -- ** AddTags
    , module Network.AWS.ELB.AddTags

    -- ** DescribeLoadBalancerAttributes
    , module Network.AWS.ELB.DescribeLoadBalancerAttributes

    -- ** CreateLoadBalancerListeners
    , module Network.AWS.ELB.CreateLoadBalancerListeners

    -- ** DeleteLoadBalancerPolicy
    , module Network.AWS.ELB.DeleteLoadBalancerPolicy

    -- ** DetachLoadBalancerFromSubnets
    , module Network.AWS.ELB.DetachLoadBalancerFromSubnets

    -- ** RegisterInstancesWithLoadBalancer
    , module Network.AWS.ELB.RegisterInstancesWithLoadBalancer

    -- ** CreateLoadBalancer
    , module Network.AWS.ELB.CreateLoadBalancer

    -- ** DeleteLoadBalancerListeners
    , module Network.AWS.ELB.DeleteLoadBalancerListeners

    -- ** SetLoadBalancerPoliciesOfListener
    , module Network.AWS.ELB.SetLoadBalancerPoliciesOfListener

    -- * Types

    -- ** Common
    , module Network.AWS.ELB.Internal

    -- ** AccessLog
    , AccessLog
    , accessLog
    , alEmitInterval
    , alS3BucketPrefix
    , alS3BucketName
    , alEnabled

    -- ** AdditionalAttribute
    , AdditionalAttribute
    , additionalAttribute
    , aaValue
    , aaKey

    -- ** AppCookieStickinessPolicy
    , AppCookieStickinessPolicy
    , appCookieStickinessPolicy
    , acspPolicyName
    , acspCookieName

    -- ** BackendServerDescription
    , BackendServerDescription
    , backendServerDescription
    , bsdPolicyNames
    , bsdInstancePort

    -- ** ConnectionDraining
    , ConnectionDraining
    , connectionDraining
    , cdTimeout
    , cdEnabled

    -- ** ConnectionSettings
    , ConnectionSettings
    , connectionSettings
    , csIdleTimeout

    -- ** CrossZoneLoadBalancing
    , CrossZoneLoadBalancing
    , crossZoneLoadBalancing
    , czlbEnabled

    -- ** HealthCheck
    , HealthCheck
    , healthCheck
    , hcTarget
    , hcInterval
    , hcTimeout
    , hcUnhealthyThreshold
    , hcHealthyThreshold

    -- ** Instance
    , Instance
    , instance'
    , iInstanceId

    -- ** InstanceState
    , InstanceState
    , instanceState
    , isInstanceId
    , isState
    , isReasonCode
    , isDescription

    -- ** LBCookieStickinessPolicy
    , LBCookieStickinessPolicy
    , lBCookieStickinessPolicy
    , lbcspPolicyName
    , lbcspCookieExpirationPeriod

    -- ** Limit
    , Limit
    , limit
    , lMax
    , lName

    -- ** Listener
    , Listener
    , listener
    , lInstanceProtocol
    , lSSLCertificateId
    , lProtocol
    , lLoadBalancerPort
    , lInstancePort

    -- ** ListenerDescription
    , ListenerDescription
    , listenerDescription
    , ldPolicyNames
    , ldListener

    -- ** LoadBalancerAttributes
    , LoadBalancerAttributes
    , loadBalancerAttributes
    , lbaCrossZoneLoadBalancing
    , lbaAccessLog
    , lbaAdditionalAttributes
    , lbaConnectionSettings
    , lbaConnectionDraining

    -- ** LoadBalancerDescription
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

    -- ** Policies
    , Policies
    , policies
    , pOtherPolicies
    , pLBCookieStickinessPolicies
    , pAppCookieStickinessPolicies

    -- ** PolicyAttribute
    , PolicyAttribute
    , policyAttribute
    , paAttributeValue
    , paAttributeName

    -- ** PolicyAttributeDescription
    , PolicyAttributeDescription
    , policyAttributeDescription
    , padAttributeValue
    , padAttributeName

    -- ** PolicyAttributeTypeDescription
    , PolicyAttributeTypeDescription
    , policyAttributeTypeDescription
    , patdAttributeType
    , patdCardinality
    , patdDefaultValue
    , patdAttributeName
    , patdDescription

    -- ** PolicyDescription
    , PolicyDescription
    , policyDescription
    , pdPolicyName
    , pdPolicyAttributeDescriptions
    , pdPolicyTypeName

    -- ** PolicyTypeDescription
    , PolicyTypeDescription
    , policyTypeDescription
    , ptdPolicyTypeName
    , ptdDescription
    , ptdPolicyAttributeTypeDescriptions

    -- ** SourceSecurityGroup
    , SourceSecurityGroup
    , sourceSecurityGroup
    , ssgOwnerAlias
    , ssgGroupName

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- ** TagDescription
    , TagDescription
    , tagDescription
    , tdLoadBalancerName
    , tdTags

    -- ** TagKeyOnly
    , TagKeyOnly
    , tagKeyOnly
    , tkoKey
    ) where

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

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'ELB'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
