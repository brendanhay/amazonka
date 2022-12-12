{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ELB
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2012-06-01@ of the AWS service descriptions, licensed under Apache 2.0.
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
module Amazonka.ELB
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessPointNotFoundException
    _AccessPointNotFoundException,

    -- ** CertificateNotFoundException
    _CertificateNotFoundException,

    -- ** DependencyThrottleException
    _DependencyThrottleException,

    -- ** DuplicateAccessPointNameException
    _DuplicateAccessPointNameException,

    -- ** DuplicateListenerException
    _DuplicateListenerException,

    -- ** DuplicatePolicyNameException
    _DuplicatePolicyNameException,

    -- ** DuplicateTagKeysException
    _DuplicateTagKeysException,

    -- ** InvalidConfigurationRequestException
    _InvalidConfigurationRequestException,

    -- ** InvalidEndPointException
    _InvalidEndPointException,

    -- ** InvalidSchemeException
    _InvalidSchemeException,

    -- ** InvalidSecurityGroupException
    _InvalidSecurityGroupException,

    -- ** InvalidSubnetException
    _InvalidSubnetException,

    -- ** ListenerNotFoundException
    _ListenerNotFoundException,

    -- ** LoadBalancerAttributeNotFoundException
    _LoadBalancerAttributeNotFoundException,

    -- ** OperationNotPermittedException
    _OperationNotPermittedException,

    -- ** PolicyNotFoundException
    _PolicyNotFoundException,

    -- ** PolicyTypeNotFoundException
    _PolicyTypeNotFoundException,

    -- ** SubnetNotFoundException
    _SubnetNotFoundException,

    -- ** TooManyAccessPointsException
    _TooManyAccessPointsException,

    -- ** TooManyPoliciesException
    _TooManyPoliciesException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** UnsupportedProtocolException
    _UnsupportedProtocolException,

    -- * Waiters
    -- $waiters

    -- ** AnyInstanceInService
    newAnyInstanceInService,

    -- ** InstanceDeregistered
    newInstanceDeregistered,

    -- ** InstanceInService
    newInstanceInService,

    -- * Operations
    -- $operations

    -- ** AddTags
    AddTags (AddTags'),
    newAddTags,
    AddTagsResponse (AddTagsResponse'),
    newAddTagsResponse,

    -- ** ApplySecurityGroupsToLoadBalancer
    ApplySecurityGroupsToLoadBalancer (ApplySecurityGroupsToLoadBalancer'),
    newApplySecurityGroupsToLoadBalancer,
    ApplySecurityGroupsToLoadBalancerResponse (ApplySecurityGroupsToLoadBalancerResponse'),
    newApplySecurityGroupsToLoadBalancerResponse,

    -- ** AttachLoadBalancerToSubnets
    AttachLoadBalancerToSubnets (AttachLoadBalancerToSubnets'),
    newAttachLoadBalancerToSubnets,
    AttachLoadBalancerToSubnetsResponse (AttachLoadBalancerToSubnetsResponse'),
    newAttachLoadBalancerToSubnetsResponse,

    -- ** ConfigureHealthCheck
    ConfigureHealthCheck (ConfigureHealthCheck'),
    newConfigureHealthCheck,
    ConfigureHealthCheckResponse (ConfigureHealthCheckResponse'),
    newConfigureHealthCheckResponse,

    -- ** CreateAppCookieStickinessPolicy
    CreateAppCookieStickinessPolicy (CreateAppCookieStickinessPolicy'),
    newCreateAppCookieStickinessPolicy,
    CreateAppCookieStickinessPolicyResponse (CreateAppCookieStickinessPolicyResponse'),
    newCreateAppCookieStickinessPolicyResponse,

    -- ** CreateLBCookieStickinessPolicy
    CreateLBCookieStickinessPolicy (CreateLBCookieStickinessPolicy'),
    newCreateLBCookieStickinessPolicy,
    CreateLBCookieStickinessPolicyResponse (CreateLBCookieStickinessPolicyResponse'),
    newCreateLBCookieStickinessPolicyResponse,

    -- ** CreateLoadBalancer
    CreateLoadBalancer (CreateLoadBalancer'),
    newCreateLoadBalancer,
    CreateLoadBalancerResponse (CreateLoadBalancerResponse'),
    newCreateLoadBalancerResponse,

    -- ** CreateLoadBalancerListeners
    CreateLoadBalancerListeners (CreateLoadBalancerListeners'),
    newCreateLoadBalancerListeners,
    CreateLoadBalancerListenersResponse (CreateLoadBalancerListenersResponse'),
    newCreateLoadBalancerListenersResponse,

    -- ** CreateLoadBalancerPolicy
    CreateLoadBalancerPolicy (CreateLoadBalancerPolicy'),
    newCreateLoadBalancerPolicy,
    CreateLoadBalancerPolicyResponse (CreateLoadBalancerPolicyResponse'),
    newCreateLoadBalancerPolicyResponse,

    -- ** DeleteLoadBalancer
    DeleteLoadBalancer (DeleteLoadBalancer'),
    newDeleteLoadBalancer,
    DeleteLoadBalancerResponse (DeleteLoadBalancerResponse'),
    newDeleteLoadBalancerResponse,

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

    -- ** DeregisterInstancesFromLoadBalancer
    DeregisterInstancesFromLoadBalancer (DeregisterInstancesFromLoadBalancer'),
    newDeregisterInstancesFromLoadBalancer,
    DeregisterInstancesFromLoadBalancerResponse (DeregisterInstancesFromLoadBalancerResponse'),
    newDeregisterInstancesFromLoadBalancerResponse,

    -- ** DescribeAccountLimits (Paginated)
    DescribeAccountLimits (DescribeAccountLimits'),
    newDescribeAccountLimits,
    DescribeAccountLimitsResponse (DescribeAccountLimitsResponse'),
    newDescribeAccountLimitsResponse,

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

    -- ** DescribeLoadBalancerPolicies
    DescribeLoadBalancerPolicies (DescribeLoadBalancerPolicies'),
    newDescribeLoadBalancerPolicies,
    DescribeLoadBalancerPoliciesResponse (DescribeLoadBalancerPoliciesResponse'),
    newDescribeLoadBalancerPoliciesResponse,

    -- ** DescribeLoadBalancerPolicyTypes
    DescribeLoadBalancerPolicyTypes (DescribeLoadBalancerPolicyTypes'),
    newDescribeLoadBalancerPolicyTypes,
    DescribeLoadBalancerPolicyTypesResponse (DescribeLoadBalancerPolicyTypesResponse'),
    newDescribeLoadBalancerPolicyTypesResponse,

    -- ** DescribeLoadBalancers (Paginated)
    DescribeLoadBalancers (DescribeLoadBalancers'),
    newDescribeLoadBalancers,
    DescribeLoadBalancersResponse (DescribeLoadBalancersResponse'),
    newDescribeLoadBalancersResponse,

    -- ** DescribeTags
    DescribeTags (DescribeTags'),
    newDescribeTags,
    DescribeTagsResponse (DescribeTagsResponse'),
    newDescribeTagsResponse,

    -- ** DetachLoadBalancerFromSubnets
    DetachLoadBalancerFromSubnets (DetachLoadBalancerFromSubnets'),
    newDetachLoadBalancerFromSubnets,
    DetachLoadBalancerFromSubnetsResponse (DetachLoadBalancerFromSubnetsResponse'),
    newDetachLoadBalancerFromSubnetsResponse,

    -- ** DisableAvailabilityZonesForLoadBalancer
    DisableAvailabilityZonesForLoadBalancer (DisableAvailabilityZonesForLoadBalancer'),
    newDisableAvailabilityZonesForLoadBalancer,
    DisableAvailabilityZonesForLoadBalancerResponse (DisableAvailabilityZonesForLoadBalancerResponse'),
    newDisableAvailabilityZonesForLoadBalancerResponse,

    -- ** EnableAvailabilityZonesForLoadBalancer
    EnableAvailabilityZonesForLoadBalancer (EnableAvailabilityZonesForLoadBalancer'),
    newEnableAvailabilityZonesForLoadBalancer,
    EnableAvailabilityZonesForLoadBalancerResponse (EnableAvailabilityZonesForLoadBalancerResponse'),
    newEnableAvailabilityZonesForLoadBalancerResponse,

    -- ** ModifyLoadBalancerAttributes
    ModifyLoadBalancerAttributes (ModifyLoadBalancerAttributes'),
    newModifyLoadBalancerAttributes,
    ModifyLoadBalancerAttributesResponse (ModifyLoadBalancerAttributesResponse'),
    newModifyLoadBalancerAttributesResponse,

    -- ** RegisterInstancesWithLoadBalancer
    RegisterInstancesWithLoadBalancer (RegisterInstancesWithLoadBalancer'),
    newRegisterInstancesWithLoadBalancer,
    RegisterInstancesWithLoadBalancerResponse (RegisterInstancesWithLoadBalancerResponse'),
    newRegisterInstancesWithLoadBalancerResponse,

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

    -- ** SetLoadBalancerPoliciesOfListener
    SetLoadBalancerPoliciesOfListener (SetLoadBalancerPoliciesOfListener'),
    newSetLoadBalancerPoliciesOfListener,
    SetLoadBalancerPoliciesOfListenerResponse (SetLoadBalancerPoliciesOfListenerResponse'),
    newSetLoadBalancerPoliciesOfListenerResponse,

    -- * Types

    -- ** Common
    module Amazonka.ELB.Internal,

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

import Amazonka.ELB.AddTags
import Amazonka.ELB.ApplySecurityGroupsToLoadBalancer
import Amazonka.ELB.AttachLoadBalancerToSubnets
import Amazonka.ELB.ConfigureHealthCheck
import Amazonka.ELB.CreateAppCookieStickinessPolicy
import Amazonka.ELB.CreateLBCookieStickinessPolicy
import Amazonka.ELB.CreateLoadBalancer
import Amazonka.ELB.CreateLoadBalancerListeners
import Amazonka.ELB.CreateLoadBalancerPolicy
import Amazonka.ELB.DeleteLoadBalancer
import Amazonka.ELB.DeleteLoadBalancerListeners
import Amazonka.ELB.DeleteLoadBalancerPolicy
import Amazonka.ELB.DeregisterInstancesFromLoadBalancer
import Amazonka.ELB.DescribeAccountLimits
import Amazonka.ELB.DescribeInstanceHealth
import Amazonka.ELB.DescribeLoadBalancerAttributes
import Amazonka.ELB.DescribeLoadBalancerPolicies
import Amazonka.ELB.DescribeLoadBalancerPolicyTypes
import Amazonka.ELB.DescribeLoadBalancers
import Amazonka.ELB.DescribeTags
import Amazonka.ELB.DetachLoadBalancerFromSubnets
import Amazonka.ELB.DisableAvailabilityZonesForLoadBalancer
import Amazonka.ELB.EnableAvailabilityZonesForLoadBalancer
import Amazonka.ELB.Internal
import Amazonka.ELB.Lens
import Amazonka.ELB.ModifyLoadBalancerAttributes
import Amazonka.ELB.RegisterInstancesWithLoadBalancer
import Amazonka.ELB.RemoveTags
import Amazonka.ELB.SetLoadBalancerListenerSSLCertificate
import Amazonka.ELB.SetLoadBalancerPoliciesForBackendServer
import Amazonka.ELB.SetLoadBalancerPoliciesOfListener
import Amazonka.ELB.Types
import Amazonka.ELB.Waiters

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
