{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Elastic Load Balancing
--
-- A load balancer can distribute incoming traffic across your EC2
-- instances. This enables you to increase the availability of your
-- application. The load balancer also monitors the health of its
-- registered instances and ensures that it routes traffic only to healthy
-- instances. You configure your load balancer to accept incoming traffic
-- by specifying one or more listeners, which are configured with a
-- protocol and port number for connections from clients to the load
-- balancer and a protocol and port number for connections from the load
-- balancer to the instances.
--
-- Elastic Load Balancing supports three types of load balancers:
-- Application Load Balancers, Network Load Balancers, and Classic Load
-- Balancers. You can select a load balancer based on your application
-- needs. For more information, see the
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/userguide/ Elastic Load Balancing User Guide>.
--
-- This reference covers the 2012-06-01 API, which supports Classic Load
-- Balancers. The 2015-12-01 API supports Application Load Balancers and
-- Network Load Balancers.
--
-- To get started, create a load balancer with one or more listeners using
-- CreateLoadBalancer. Register your instances with the load balancer using
-- RegisterInstancesWithLoadBalancer.
--
-- All Elastic Load Balancing operations are /idempotent/, which means that
-- they complete at most one time. If you repeat an operation, it succeeds
-- with a 200 OK response code.
module Network.AWS.ELB
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** DuplicateAccessPointNameException
    _DuplicateAccessPointNameException,

    -- ** InvalidSecurityGroupException
    _InvalidSecurityGroupException,

    -- ** InvalidConfigurationRequestException
    _InvalidConfigurationRequestException,

    -- ** DuplicatePolicyNameException
    _DuplicatePolicyNameException,

    -- ** AccessPointNotFoundException
    _AccessPointNotFoundException,

    -- ** PolicyTypeNotFoundException
    _PolicyTypeNotFoundException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** TooManyPoliciesException
    _TooManyPoliciesException,

    -- ** DuplicateTagKeysException
    _DuplicateTagKeysException,

    -- ** DependencyThrottleException
    _DependencyThrottleException,

    -- ** OperationNotPermittedException
    _OperationNotPermittedException,

    -- ** InvalidEndPointException
    _InvalidEndPointException,

    -- ** ListenerNotFoundException
    _ListenerNotFoundException,

    -- ** LoadBalancerAttributeNotFoundException
    _LoadBalancerAttributeNotFoundException,

    -- ** SubnetNotFoundException
    _SubnetNotFoundException,

    -- ** UnsupportedProtocolException
    _UnsupportedProtocolException,

    -- ** PolicyNotFoundException
    _PolicyNotFoundException,

    -- ** CertificateNotFoundException
    _CertificateNotFoundException,

    -- ** DuplicateListenerException
    _DuplicateListenerException,

    -- ** TooManyAccessPointsException
    _TooManyAccessPointsException,

    -- ** InvalidSchemeException
    _InvalidSchemeException,

    -- ** InvalidSubnetException
    _InvalidSubnetException,

    -- * Waiters
    -- $waiters

    -- ** InstanceDeregistered
    newInstanceDeregistered,

    -- ** InstanceInService
    newInstanceInService,

    -- ** AnyInstanceInService
    newAnyInstanceInService,

    -- * Operations
    -- $operations

    -- ** RemoveTags
    RemoveTags (RemoveTags'),
    newRemoveTags,
    RemoveTagsResponse (RemoveTagsResponse'),
    newRemoveTagsResponse,

    -- ** SetLoadBalancerListenerSSLCertificate
    SetLoadBalancerListenerSSLCertificate (SetLoadBalancerListenerSSLCertificate'),
    newSetLoadBalancerListenerSSLCertificate,
    SetLoadBalancerListenerSSLCertificateResponse (SetLoadBalancerListenerSSLCertificateResponse'),
    newSetLoadBalancerListenerSSLCertificateResponse,

    -- ** SetLoadBalancerPoliciesForBackendServer
    SetLoadBalancerPoliciesForBackendServer (SetLoadBalancerPoliciesForBackendServer'),
    newSetLoadBalancerPoliciesForBackendServer,
    SetLoadBalancerPoliciesForBackendServerResponse (SetLoadBalancerPoliciesForBackendServerResponse'),
    newSetLoadBalancerPoliciesForBackendServerResponse,

    -- ** DescribeTags
    DescribeTags (DescribeTags'),
    newDescribeTags,
    DescribeTagsResponse (DescribeTagsResponse'),
    newDescribeTagsResponse,

    -- ** DescribeLoadBalancerPolicies
    DescribeLoadBalancerPolicies (DescribeLoadBalancerPolicies'),
    newDescribeLoadBalancerPolicies,
    DescribeLoadBalancerPoliciesResponse (DescribeLoadBalancerPoliciesResponse'),
    newDescribeLoadBalancerPoliciesResponse,

    -- ** SetLoadBalancerPoliciesOfListener
    SetLoadBalancerPoliciesOfListener (SetLoadBalancerPoliciesOfListener'),
    newSetLoadBalancerPoliciesOfListener,
    SetLoadBalancerPoliciesOfListenerResponse (SetLoadBalancerPoliciesOfListenerResponse'),
    newSetLoadBalancerPoliciesOfListenerResponse,

    -- ** DeregisterInstancesFromLoadBalancer
    DeregisterInstancesFromLoadBalancer (DeregisterInstancesFromLoadBalancer'),
    newDeregisterInstancesFromLoadBalancer,
    DeregisterInstancesFromLoadBalancerResponse (DeregisterInstancesFromLoadBalancerResponse'),
    newDeregisterInstancesFromLoadBalancerResponse,

    -- ** CreateLoadBalancer
    CreateLoadBalancer (CreateLoadBalancer'),
    newCreateLoadBalancer,
    CreateLoadBalancerResponse (CreateLoadBalancerResponse'),
    newCreateLoadBalancerResponse,

    -- ** DetachLoadBalancerFromSubnets
    DetachLoadBalancerFromSubnets (DetachLoadBalancerFromSubnets'),
    newDetachLoadBalancerFromSubnets,
    DetachLoadBalancerFromSubnetsResponse (DetachLoadBalancerFromSubnetsResponse'),
    newDetachLoadBalancerFromSubnetsResponse,

    -- ** RegisterInstancesWithLoadBalancer
    RegisterInstancesWithLoadBalancer (RegisterInstancesWithLoadBalancer'),
    newRegisterInstancesWithLoadBalancer,
    RegisterInstancesWithLoadBalancerResponse (RegisterInstancesWithLoadBalancerResponse'),
    newRegisterInstancesWithLoadBalancerResponse,

    -- ** CreateLoadBalancerListeners
    CreateLoadBalancerListeners (CreateLoadBalancerListeners'),
    newCreateLoadBalancerListeners,
    CreateLoadBalancerListenersResponse (CreateLoadBalancerListenersResponse'),
    newCreateLoadBalancerListenersResponse,

    -- ** DeleteLoadBalancer
    DeleteLoadBalancer (DeleteLoadBalancer'),
    newDeleteLoadBalancer,
    DeleteLoadBalancerResponse (DeleteLoadBalancerResponse'),
    newDeleteLoadBalancerResponse,

    -- ** AddTags
    AddTags (AddTags'),
    newAddTags,
    AddTagsResponse (AddTagsResponse'),
    newAddTagsResponse,

    -- ** ModifyLoadBalancerAttributes
    ModifyLoadBalancerAttributes (ModifyLoadBalancerAttributes'),
    newModifyLoadBalancerAttributes,
    ModifyLoadBalancerAttributesResponse (ModifyLoadBalancerAttributesResponse'),
    newModifyLoadBalancerAttributesResponse,

    -- ** AttachLoadBalancerToSubnets
    AttachLoadBalancerToSubnets (AttachLoadBalancerToSubnets'),
    newAttachLoadBalancerToSubnets,
    AttachLoadBalancerToSubnetsResponse (AttachLoadBalancerToSubnetsResponse'),
    newAttachLoadBalancerToSubnetsResponse,

    -- ** DescribeAccountLimits (Paginated)
    DescribeAccountLimits (DescribeAccountLimits'),
    newDescribeAccountLimits,
    DescribeAccountLimitsResponse (DescribeAccountLimitsResponse'),
    newDescribeAccountLimitsResponse,

    -- ** CreateLBCookieStickinessPolicy
    CreateLBCookieStickinessPolicy (CreateLBCookieStickinessPolicy'),
    newCreateLBCookieStickinessPolicy,
    CreateLBCookieStickinessPolicyResponse (CreateLBCookieStickinessPolicyResponse'),
    newCreateLBCookieStickinessPolicyResponse,

    -- ** ApplySecurityGroupsToLoadBalancer
    ApplySecurityGroupsToLoadBalancer (ApplySecurityGroupsToLoadBalancer'),
    newApplySecurityGroupsToLoadBalancer,
    ApplySecurityGroupsToLoadBalancerResponse (ApplySecurityGroupsToLoadBalancerResponse'),
    newApplySecurityGroupsToLoadBalancerResponse,

    -- ** EnableAvailabilityZonesForLoadBalancer
    EnableAvailabilityZonesForLoadBalancer (EnableAvailabilityZonesForLoadBalancer'),
    newEnableAvailabilityZonesForLoadBalancer,
    EnableAvailabilityZonesForLoadBalancerResponse (EnableAvailabilityZonesForLoadBalancerResponse'),
    newEnableAvailabilityZonesForLoadBalancerResponse,

    -- ** DescribeLoadBalancers (Paginated)
    DescribeLoadBalancers (DescribeLoadBalancers'),
    newDescribeLoadBalancers,
    DescribeLoadBalancersResponse (DescribeLoadBalancersResponse'),
    newDescribeLoadBalancersResponse,

    -- ** DescribeLoadBalancerPolicyTypes
    DescribeLoadBalancerPolicyTypes (DescribeLoadBalancerPolicyTypes'),
    newDescribeLoadBalancerPolicyTypes,
    DescribeLoadBalancerPolicyTypesResponse (DescribeLoadBalancerPolicyTypesResponse'),
    newDescribeLoadBalancerPolicyTypesResponse,

    -- ** DisableAvailabilityZonesForLoadBalancer
    DisableAvailabilityZonesForLoadBalancer (DisableAvailabilityZonesForLoadBalancer'),
    newDisableAvailabilityZonesForLoadBalancer,
    DisableAvailabilityZonesForLoadBalancerResponse (DisableAvailabilityZonesForLoadBalancerResponse'),
    newDisableAvailabilityZonesForLoadBalancerResponse,

    -- ** CreateLoadBalancerPolicy
    CreateLoadBalancerPolicy (CreateLoadBalancerPolicy'),
    newCreateLoadBalancerPolicy,
    CreateLoadBalancerPolicyResponse (CreateLoadBalancerPolicyResponse'),
    newCreateLoadBalancerPolicyResponse,

    -- ** DeleteLoadBalancerListeners
    DeleteLoadBalancerListeners (DeleteLoadBalancerListeners'),
    newDeleteLoadBalancerListeners,
    DeleteLoadBalancerListenersResponse (DeleteLoadBalancerListenersResponse'),
    newDeleteLoadBalancerListenersResponse,

    -- ** DeleteLoadBalancerPolicy
    DeleteLoadBalancerPolicy (DeleteLoadBalancerPolicy'),
    newDeleteLoadBalancerPolicy,
    DeleteLoadBalancerPolicyResponse (DeleteLoadBalancerPolicyResponse'),
    newDeleteLoadBalancerPolicyResponse,

    -- ** DescribeInstanceHealth
    DescribeInstanceHealth (DescribeInstanceHealth'),
    newDescribeInstanceHealth,
    DescribeInstanceHealthResponse (DescribeInstanceHealthResponse'),
    newDescribeInstanceHealthResponse,

    -- ** DescribeLoadBalancerAttributes
    DescribeLoadBalancerAttributes (DescribeLoadBalancerAttributes'),
    newDescribeLoadBalancerAttributes,
    DescribeLoadBalancerAttributesResponse (DescribeLoadBalancerAttributesResponse'),
    newDescribeLoadBalancerAttributesResponse,

    -- ** CreateAppCookieStickinessPolicy
    CreateAppCookieStickinessPolicy (CreateAppCookieStickinessPolicy'),
    newCreateAppCookieStickinessPolicy,
    CreateAppCookieStickinessPolicyResponse (CreateAppCookieStickinessPolicyResponse'),
    newCreateAppCookieStickinessPolicyResponse,

    -- ** ConfigureHealthCheck
    ConfigureHealthCheck (ConfigureHealthCheck'),
    newConfigureHealthCheck,
    ConfigureHealthCheckResponse (ConfigureHealthCheckResponse'),
    newConfigureHealthCheckResponse,

    -- * Types

    -- ** Common
    module Network.AWS.ELB.Internal,

    -- ** AccessLog
    AccessLog (AccessLog'),
    newAccessLog,

    -- ** AdditionalAttribute
    AdditionalAttribute (AdditionalAttribute'),
    newAdditionalAttribute,

    -- ** AppCookieStickinessPolicy
    AppCookieStickinessPolicy (AppCookieStickinessPolicy'),
    newAppCookieStickinessPolicy,

    -- ** BackendServerDescription
    BackendServerDescription (BackendServerDescription'),
    newBackendServerDescription,

    -- ** ConnectionDraining
    ConnectionDraining (ConnectionDraining'),
    newConnectionDraining,

    -- ** ConnectionSettings
    ConnectionSettings (ConnectionSettings'),
    newConnectionSettings,

    -- ** CrossZoneLoadBalancing
    CrossZoneLoadBalancing (CrossZoneLoadBalancing'),
    newCrossZoneLoadBalancing,

    -- ** HealthCheck
    HealthCheck (HealthCheck'),
    newHealthCheck,

    -- ** Instance
    Instance (Instance'),
    newInstance,

    -- ** InstanceState
    InstanceState (InstanceState'),
    newInstanceState,

    -- ** LBCookieStickinessPolicy
    LBCookieStickinessPolicy (LBCookieStickinessPolicy'),
    newLBCookieStickinessPolicy,

    -- ** Limit
    Limit (Limit'),
    newLimit,

    -- ** Listener
    Listener (Listener'),
    newListener,

    -- ** ListenerDescription
    ListenerDescription (ListenerDescription'),
    newListenerDescription,

    -- ** LoadBalancerAttributes
    LoadBalancerAttributes (LoadBalancerAttributes'),
    newLoadBalancerAttributes,

    -- ** LoadBalancerDescription
    LoadBalancerDescription (LoadBalancerDescription'),
    newLoadBalancerDescription,

    -- ** Policies
    Policies (Policies'),
    newPolicies,

    -- ** PolicyAttribute
    PolicyAttribute (PolicyAttribute'),
    newPolicyAttribute,

    -- ** PolicyAttributeDescription
    PolicyAttributeDescription (PolicyAttributeDescription'),
    newPolicyAttributeDescription,

    -- ** PolicyAttributeTypeDescription
    PolicyAttributeTypeDescription (PolicyAttributeTypeDescription'),
    newPolicyAttributeTypeDescription,

    -- ** PolicyDescription
    PolicyDescription (PolicyDescription'),
    newPolicyDescription,

    -- ** PolicyTypeDescription
    PolicyTypeDescription (PolicyTypeDescription'),
    newPolicyTypeDescription,

    -- ** SourceSecurityGroup
    SourceSecurityGroup (SourceSecurityGroup'),
    newSourceSecurityGroup,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TagDescription
    TagDescription (TagDescription'),
    newTagDescription,

    -- ** TagKeyOnly
    TagKeyOnly (TagKeyOnly'),
    newTagKeyOnly,
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
import Network.AWS.ELB.Lens
import Network.AWS.ELB.ModifyLoadBalancerAttributes
import Network.AWS.ELB.RegisterInstancesWithLoadBalancer
import Network.AWS.ELB.RemoveTags
import Network.AWS.ELB.SetLoadBalancerListenerSSLCertificate
import Network.AWS.ELB.SetLoadBalancerPoliciesForBackendServer
import Network.AWS.ELB.SetLoadBalancerPoliciesOfListener
import Network.AWS.ELB.Types
import Network.AWS.ELB.Waiters

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
