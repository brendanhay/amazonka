{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.GlobalAccelerator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-08-08@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS Global Accelerator
--
-- This is the /AWS Global Accelerator API Reference/. This guide is for
-- developers who need detailed information about AWS Global Accelerator
-- API actions, data types, and errors. For more information about Global
-- Accelerator features, see the
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/Welcome.html AWS Global Accelerator Developer Guide>.
--
-- AWS Global Accelerator is a service in which you create /accelerators/
-- to improve the performance of your applications for local and global
-- users. Depending on the type of accelerator you choose, you can gain
-- additional benefits.
--
-- -   By using a standard accelerator, you can improve availability of
--     your internet applications that are used by a global audience. With
--     a standard accelerator, Global Accelerator directs traffic to
--     optimal endpoints over the AWS global network.
--
-- -   For other scenarios, you might choose a custom routing accelerator.
--     With a custom routing accelerator, you can use application logic to
--     directly map one or more users to a specific endpoint among many
--     endpoints.
--
-- Global Accelerator is a global service that supports endpoints in
-- multiple AWS Regions but you must specify the US West (Oregon) Region to
-- create or update accelerators.
--
-- By default, Global Accelerator provides you with two static IP addresses
-- that you associate with your accelerator. With a standard accelerator,
-- instead of using the IP addresses that Global Accelerator provides, you
-- can configure these entry points to be IPv4 addresses from your own IP
-- address ranges that you bring to Global Accelerator. The static IP
-- addresses are anycast from the AWS edge network. For a standard
-- accelerator, they distribute incoming application traffic across
-- multiple endpoint resources in multiple AWS Regions, which increases the
-- availability of your applications. Endpoints for standard accelerators
-- can be Network Load Balancers, Application Load Balancers, Amazon EC2
-- instances, or Elastic IP addresses that are located in one AWS Region or
-- multiple Regions. For custom routing accelerators, you map traffic that
-- arrives to the static IP addresses to specific Amazon EC2 servers in
-- endpoints that are virtual private cloud (VPC) subnets.
--
-- The static IP addresses remain assigned to your accelerator for as long
-- as it exists, even if you disable the accelerator and it no longer
-- accepts or routes traffic. However, when you /delete/ an accelerator,
-- you lose the static IP addresses that are assigned to it, so you can no
-- longer route traffic by using them. You can use IAM policies like
-- tag-based permissions with Global Accelerator to limit the users who
-- have permissions to delete an accelerator. For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/access-control-manage-access-tag-policies.html Tag-based policies>.
--
-- For standard accelerators, Global Accelerator uses the AWS global
-- network to route traffic to the optimal regional endpoint based on
-- health, client location, and policies that you configure. The service
-- reacts instantly to changes in health or configuration to ensure that
-- internet traffic from clients is always directed to healthy endpoints.
--
-- For a list of the AWS Regions where Global Accelerator and other
-- services are currently supported, see the
-- <https://docs.aws.amazon.com/about-aws/global-infrastructure/regional-product-services/ AWS Region Table>.
--
-- AWS Global Accelerator includes the following components:
--
-- [Static IP addresses]
--     Global Accelerator provides you with a set of two static IP
--     addresses that are anycast from the AWS edge network. If you bring
--     your own IP address range to AWS (BYOIP) to use with a standard
--     accelerator, you can instead assign IP addresses from your own pool
--     to use with your accelerator. For more information, see
--     <https://docs.aws.amazon.com/global-accelerator/latest/dg/using-byoip.html Bring your own IP addresses (BYOIP) in AWS Global Accelerator>.
--
--     The IP addresses serve as single fixed entry points for your
--     clients. If you already have Elastic Load Balancing load balancers,
--     Amazon EC2 instances, or Elastic IP address resources set up for
--     your applications, you can easily add those to a standard
--     accelerator in Global Accelerator. This allows Global Accelerator to
--     use static IP addresses to access the resources.
--
--     The static IP addresses remain assigned to your accelerator for as
--     long as it exists, even if you disable the accelerator and it no
--     longer accepts or routes traffic. However, when you /delete/ an
--     accelerator, you lose the static IP addresses that are assigned to
--     it, so you can no longer route traffic by using them. You can use
--     IAM policies like tag-based permissions with Global Accelerator to
--     delete an accelerator. For more information, see
--     <https://docs.aws.amazon.com/global-accelerator/latest/dg/access-control-manage-access-tag-policies.html Tag-based policies>.
--
-- [Accelerator]
--     An accelerator directs traffic to endpoints over the AWS global
--     network to improve the performance of your internet applications.
--     Each accelerator includes one or more listeners.
--
--     There are two types of accelerators:
--
--     -   A /standard/ accelerator directs traffic to the optimal AWS
--         endpoint based on several factors, including the user’s
--         location, the health of the endpoint, and the endpoint weights
--         that you configure. This improves the availability and
--         performance of your applications. Endpoints can be Network Load
--         Balancers, Application Load Balancers, Amazon EC2 instances, or
--         Elastic IP addresses.
--
--     -   A /custom routing/ accelerator directs traffic to one of
--         possibly thousands of Amazon EC2 instances running in a single
--         or multiple virtual private clouds (VPCs). With custom routing,
--         listener ports are mapped to statically associate port ranges
--         with VPC subnets, which allows Global Accelerator to determine
--         an EC2 instance IP address at the time of connection. By
--         default, all port mapping destinations in a VPC subnet can\'t
--         receive traffic. You can choose to configure all destinations in
--         the subnet to receive traffic, or to specify individual port
--         mappings that can receive traffic.
--
--     For more information, see
--     <https://docs.aws.amazon.com/global-accelerator/latest/dg/introduction-accelerator-types.html Types of accelerators>.
--
-- [DNS name]
--     Global Accelerator assigns each accelerator a default Domain Name
--     System (DNS) name, similar to
--     @a1234567890abcdef.awsglobalaccelerator.com@, that points to the
--     static IP addresses that Global Accelerator assigns to you or that
--     you choose from your own IP address range. Depending on the use
--     case, you can use your accelerator\'s static IP addresses or DNS
--     name to route traffic to your accelerator, or set up DNS records to
--     route traffic using your own custom domain name.
--
-- [Network zone]
--     A network zone services the static IP addresses for your accelerator
--     from a unique IP subnet. Similar to an AWS Availability Zone, a
--     network zone is an isolated unit with its own set of physical
--     infrastructure. When you configure an accelerator, by default,
--     Global Accelerator allocates two IPv4 addresses for it. If one IP
--     address from a network zone becomes unavailable due to IP address
--     blocking by certain client networks, or network disruptions, then
--     client applications can retry on the healthy static IP address from
--     the other isolated network zone.
--
-- [Listener]
--     A listener processes inbound connections from clients to Global
--     Accelerator, based on the port (or port range) and protocol (or
--     protocols) that you configure. A listener can be configured for TCP,
--     UDP, or both TCP and UDP protocols. Each listener has one or more
--     endpoint groups associated with it, and traffic is forwarded to
--     endpoints in one of the groups. You associate endpoint groups with
--     listeners by specifying the Regions that you want to distribute
--     traffic to. With a standard accelerator, traffic is distributed to
--     optimal endpoints within the endpoint groups associated with a
--     listener.
--
-- [Endpoint group]
--     Each endpoint group is associated with a specific AWS Region.
--     Endpoint groups include one or more endpoints in the Region. With a
--     standard accelerator, you can increase or reduce the percentage of
--     traffic that would be otherwise directed to an endpoint group by
--     adjusting a setting called a /traffic dial/. The traffic dial lets
--     you easily do performance testing or blue\/green deployment testing,
--     for example, for new releases across different AWS Regions.
--
-- [Endpoint]
--     An endpoint is a resource that Global Accelerator directs traffic
--     to.
--
--     Endpoints for standard accelerators can be Network Load Balancers,
--     Application Load Balancers, Amazon EC2 instances, or Elastic IP
--     addresses. An Application Load Balancer endpoint can be
--     internet-facing or internal. Traffic for standard accelerators is
--     routed to endpoints based on the health of the endpoint along with
--     configuration options that you choose, such as endpoint weights. For
--     each endpoint, you can configure weights, which are numbers that you
--     can use to specify the proportion of traffic to route to each one.
--     This can be useful, for example, to do performance testing within a
--     Region.
--
--     Endpoints for custom routing accelerators are virtual private cloud
--     (VPC) subnets with one or many EC2 instances.
module Amazonka.GlobalAccelerator
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** AssociatedListenerFoundException
    _AssociatedListenerFoundException,

    -- ** EndpointAlreadyExistsException
    _EndpointAlreadyExistsException,

    -- ** InvalidArgumentException
    _InvalidArgumentException,

    -- ** AssociatedEndpointGroupFoundException
    _AssociatedEndpointGroupFoundException,

    -- ** AcceleratorNotDisabledException
    _AcceleratorNotDisabledException,

    -- ** ConflictException
    _ConflictException,

    -- ** EndpointNotFoundException
    _EndpointNotFoundException,

    -- ** ListenerNotFoundException
    _ListenerNotFoundException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** InternalServiceErrorException
    _InternalServiceErrorException,

    -- ** EndpointGroupAlreadyExistsException
    _EndpointGroupAlreadyExistsException,

    -- ** ByoipCidrNotFoundException
    _ByoipCidrNotFoundException,

    -- ** IncorrectCidrStateException
    _IncorrectCidrStateException,

    -- ** InvalidPortRangeException
    _InvalidPortRangeException,

    -- ** EndpointGroupNotFoundException
    _EndpointGroupNotFoundException,

    -- ** AcceleratorNotFoundException
    _AcceleratorNotFoundException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DenyCustomRoutingTraffic
    DenyCustomRoutingTraffic (DenyCustomRoutingTraffic'),
    newDenyCustomRoutingTraffic,
    DenyCustomRoutingTrafficResponse (DenyCustomRoutingTrafficResponse'),
    newDenyCustomRoutingTrafficResponse,

    -- ** DescribeCustomRoutingListener
    DescribeCustomRoutingListener (DescribeCustomRoutingListener'),
    newDescribeCustomRoutingListener,
    DescribeCustomRoutingListenerResponse (DescribeCustomRoutingListenerResponse'),
    newDescribeCustomRoutingListenerResponse,

    -- ** CreateCustomRoutingEndpointGroup
    CreateCustomRoutingEndpointGroup (CreateCustomRoutingEndpointGroup'),
    newCreateCustomRoutingEndpointGroup,
    CreateCustomRoutingEndpointGroupResponse (CreateCustomRoutingEndpointGroupResponse'),
    newCreateCustomRoutingEndpointGroupResponse,

    -- ** DescribeCustomRoutingAcceleratorAttributes
    DescribeCustomRoutingAcceleratorAttributes (DescribeCustomRoutingAcceleratorAttributes'),
    newDescribeCustomRoutingAcceleratorAttributes,
    DescribeCustomRoutingAcceleratorAttributesResponse (DescribeCustomRoutingAcceleratorAttributesResponse'),
    newDescribeCustomRoutingAcceleratorAttributesResponse,

    -- ** DeleteCustomRoutingEndpointGroup
    DeleteCustomRoutingEndpointGroup (DeleteCustomRoutingEndpointGroup'),
    newDeleteCustomRoutingEndpointGroup,
    DeleteCustomRoutingEndpointGroupResponse (DeleteCustomRoutingEndpointGroupResponse'),
    newDeleteCustomRoutingEndpointGroupResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DescribeAcceleratorAttributes
    DescribeAcceleratorAttributes (DescribeAcceleratorAttributes'),
    newDescribeAcceleratorAttributes,
    DescribeAcceleratorAttributesResponse (DescribeAcceleratorAttributesResponse'),
    newDescribeAcceleratorAttributesResponse,

    -- ** DeleteEndpointGroup
    DeleteEndpointGroup (DeleteEndpointGroup'),
    newDeleteEndpointGroup,
    DeleteEndpointGroupResponse (DeleteEndpointGroupResponse'),
    newDeleteEndpointGroupResponse,

    -- ** UpdateEndpointGroup
    UpdateEndpointGroup (UpdateEndpointGroup'),
    newUpdateEndpointGroup,
    UpdateEndpointGroupResponse (UpdateEndpointGroupResponse'),
    newUpdateEndpointGroupResponse,

    -- ** ListCustomRoutingListeners (Paginated)
    ListCustomRoutingListeners (ListCustomRoutingListeners'),
    newListCustomRoutingListeners,
    ListCustomRoutingListenersResponse (ListCustomRoutingListenersResponse'),
    newListCustomRoutingListenersResponse,

    -- ** DeleteCustomRoutingListener
    DeleteCustomRoutingListener (DeleteCustomRoutingListener'),
    newDeleteCustomRoutingListener,
    DeleteCustomRoutingListenerResponse (DeleteCustomRoutingListenerResponse'),
    newDeleteCustomRoutingListenerResponse,

    -- ** UpdateCustomRoutingListener
    UpdateCustomRoutingListener (UpdateCustomRoutingListener'),
    newUpdateCustomRoutingListener,
    UpdateCustomRoutingListenerResponse (UpdateCustomRoutingListenerResponse'),
    newUpdateCustomRoutingListenerResponse,

    -- ** CreateAccelerator
    CreateAccelerator (CreateAccelerator'),
    newCreateAccelerator,
    CreateAcceleratorResponse (CreateAcceleratorResponse'),
    newCreateAcceleratorResponse,

    -- ** AllowCustomRoutingTraffic
    AllowCustomRoutingTraffic (AllowCustomRoutingTraffic'),
    newAllowCustomRoutingTraffic,
    AllowCustomRoutingTrafficResponse (AllowCustomRoutingTrafficResponse'),
    newAllowCustomRoutingTrafficResponse,

    -- ** WithdrawByoipCidr
    WithdrawByoipCidr (WithdrawByoipCidr'),
    newWithdrawByoipCidr,
    WithdrawByoipCidrResponse (WithdrawByoipCidrResponse'),
    newWithdrawByoipCidrResponse,

    -- ** AdvertiseByoipCidr
    AdvertiseByoipCidr (AdvertiseByoipCidr'),
    newAdvertiseByoipCidr,
    AdvertiseByoipCidrResponse (AdvertiseByoipCidrResponse'),
    newAdvertiseByoipCidrResponse,

    -- ** DeleteAccelerator
    DeleteAccelerator (DeleteAccelerator'),
    newDeleteAccelerator,
    DeleteAcceleratorResponse (DeleteAcceleratorResponse'),
    newDeleteAcceleratorResponse,

    -- ** UpdateAccelerator
    UpdateAccelerator (UpdateAccelerator'),
    newUpdateAccelerator,
    UpdateAcceleratorResponse (UpdateAcceleratorResponse'),
    newUpdateAcceleratorResponse,

    -- ** ListAccelerators (Paginated)
    ListAccelerators (ListAccelerators'),
    newListAccelerators,
    ListAcceleratorsResponse (ListAcceleratorsResponse'),
    newListAcceleratorsResponse,

    -- ** DescribeEndpointGroup
    DescribeEndpointGroup (DescribeEndpointGroup'),
    newDescribeEndpointGroup,
    DescribeEndpointGroupResponse (DescribeEndpointGroupResponse'),
    newDescribeEndpointGroupResponse,

    -- ** UpdateAcceleratorAttributes
    UpdateAcceleratorAttributes (UpdateAcceleratorAttributes'),
    newUpdateAcceleratorAttributes,
    UpdateAcceleratorAttributesResponse (UpdateAcceleratorAttributesResponse'),
    newUpdateAcceleratorAttributesResponse,

    -- ** CreateCustomRoutingAccelerator
    CreateCustomRoutingAccelerator (CreateCustomRoutingAccelerator'),
    newCreateCustomRoutingAccelerator,
    CreateCustomRoutingAcceleratorResponse (CreateCustomRoutingAcceleratorResponse'),
    newCreateCustomRoutingAcceleratorResponse,

    -- ** ListCustomRoutingPortMappingsByDestination (Paginated)
    ListCustomRoutingPortMappingsByDestination (ListCustomRoutingPortMappingsByDestination'),
    newListCustomRoutingPortMappingsByDestination,
    ListCustomRoutingPortMappingsByDestinationResponse (ListCustomRoutingPortMappingsByDestinationResponse'),
    newListCustomRoutingPortMappingsByDestinationResponse,

    -- ** DeleteListener
    DeleteListener (DeleteListener'),
    newDeleteListener,
    DeleteListenerResponse (DeleteListenerResponse'),
    newDeleteListenerResponse,

    -- ** UpdateListener
    UpdateListener (UpdateListener'),
    newUpdateListener,
    UpdateListenerResponse (UpdateListenerResponse'),
    newUpdateListenerResponse,

    -- ** ListListeners (Paginated)
    ListListeners (ListListeners'),
    newListListeners,
    ListListenersResponse (ListListenersResponse'),
    newListListenersResponse,

    -- ** ListCustomRoutingEndpointGroups
    ListCustomRoutingEndpointGroups (ListCustomRoutingEndpointGroups'),
    newListCustomRoutingEndpointGroups,
    ListCustomRoutingEndpointGroupsResponse (ListCustomRoutingEndpointGroupsResponse'),
    newListCustomRoutingEndpointGroupsResponse,

    -- ** CreateListener
    CreateListener (CreateListener'),
    newCreateListener,
    CreateListenerResponse (CreateListenerResponse'),
    newCreateListenerResponse,

    -- ** DescribeAccelerator
    DescribeAccelerator (DescribeAccelerator'),
    newDescribeAccelerator,
    DescribeAcceleratorResponse (DescribeAcceleratorResponse'),
    newDescribeAcceleratorResponse,

    -- ** CreateCustomRoutingListener
    CreateCustomRoutingListener (CreateCustomRoutingListener'),
    newCreateCustomRoutingListener,
    CreateCustomRoutingListenerResponse (CreateCustomRoutingListenerResponse'),
    newCreateCustomRoutingListenerResponse,

    -- ** DescribeCustomRoutingAccelerator
    DescribeCustomRoutingAccelerator (DescribeCustomRoutingAccelerator'),
    newDescribeCustomRoutingAccelerator,
    DescribeCustomRoutingAcceleratorResponse (DescribeCustomRoutingAcceleratorResponse'),
    newDescribeCustomRoutingAcceleratorResponse,

    -- ** ListEndpointGroups (Paginated)
    ListEndpointGroups (ListEndpointGroups'),
    newListEndpointGroups,
    ListEndpointGroupsResponse (ListEndpointGroupsResponse'),
    newListEndpointGroupsResponse,

    -- ** ProvisionByoipCidr
    ProvisionByoipCidr (ProvisionByoipCidr'),
    newProvisionByoipCidr,
    ProvisionByoipCidrResponse (ProvisionByoipCidrResponse'),
    newProvisionByoipCidrResponse,

    -- ** CreateEndpointGroup
    CreateEndpointGroup (CreateEndpointGroup'),
    newCreateEndpointGroup,
    CreateEndpointGroupResponse (CreateEndpointGroupResponse'),
    newCreateEndpointGroupResponse,

    -- ** ListByoipCidrs (Paginated)
    ListByoipCidrs (ListByoipCidrs'),
    newListByoipCidrs,
    ListByoipCidrsResponse (ListByoipCidrsResponse'),
    newListByoipCidrsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DescribeListener
    DescribeListener (DescribeListener'),
    newDescribeListener,
    DescribeListenerResponse (DescribeListenerResponse'),
    newDescribeListenerResponse,

    -- ** ListCustomRoutingPortMappings (Paginated)
    ListCustomRoutingPortMappings (ListCustomRoutingPortMappings'),
    newListCustomRoutingPortMappings,
    ListCustomRoutingPortMappingsResponse (ListCustomRoutingPortMappingsResponse'),
    newListCustomRoutingPortMappingsResponse,

    -- ** AddCustomRoutingEndpoints
    AddCustomRoutingEndpoints (AddCustomRoutingEndpoints'),
    newAddCustomRoutingEndpoints,
    AddCustomRoutingEndpointsResponse (AddCustomRoutingEndpointsResponse'),
    newAddCustomRoutingEndpointsResponse,

    -- ** DescribeCustomRoutingEndpointGroup
    DescribeCustomRoutingEndpointGroup (DescribeCustomRoutingEndpointGroup'),
    newDescribeCustomRoutingEndpointGroup,
    DescribeCustomRoutingEndpointGroupResponse (DescribeCustomRoutingEndpointGroupResponse'),
    newDescribeCustomRoutingEndpointGroupResponse,

    -- ** UpdateCustomRoutingAcceleratorAttributes
    UpdateCustomRoutingAcceleratorAttributes (UpdateCustomRoutingAcceleratorAttributes'),
    newUpdateCustomRoutingAcceleratorAttributes,
    UpdateCustomRoutingAcceleratorAttributesResponse (UpdateCustomRoutingAcceleratorAttributesResponse'),
    newUpdateCustomRoutingAcceleratorAttributesResponse,

    -- ** RemoveCustomRoutingEndpoints
    RemoveCustomRoutingEndpoints (RemoveCustomRoutingEndpoints'),
    newRemoveCustomRoutingEndpoints,
    RemoveCustomRoutingEndpointsResponse (RemoveCustomRoutingEndpointsResponse'),
    newRemoveCustomRoutingEndpointsResponse,

    -- ** UpdateCustomRoutingAccelerator
    UpdateCustomRoutingAccelerator (UpdateCustomRoutingAccelerator'),
    newUpdateCustomRoutingAccelerator,
    UpdateCustomRoutingAcceleratorResponse (UpdateCustomRoutingAcceleratorResponse'),
    newUpdateCustomRoutingAcceleratorResponse,

    -- ** DeleteCustomRoutingAccelerator
    DeleteCustomRoutingAccelerator (DeleteCustomRoutingAccelerator'),
    newDeleteCustomRoutingAccelerator,
    DeleteCustomRoutingAcceleratorResponse (DeleteCustomRoutingAcceleratorResponse'),
    newDeleteCustomRoutingAcceleratorResponse,

    -- ** ListCustomRoutingAccelerators (Paginated)
    ListCustomRoutingAccelerators (ListCustomRoutingAccelerators'),
    newListCustomRoutingAccelerators,
    ListCustomRoutingAcceleratorsResponse (ListCustomRoutingAcceleratorsResponse'),
    newListCustomRoutingAcceleratorsResponse,

    -- ** DeprovisionByoipCidr
    DeprovisionByoipCidr (DeprovisionByoipCidr'),
    newDeprovisionByoipCidr,
    DeprovisionByoipCidrResponse (DeprovisionByoipCidrResponse'),
    newDeprovisionByoipCidrResponse,

    -- * Types

    -- ** AcceleratorStatus
    AcceleratorStatus (..),

    -- ** ByoipCidrState
    ByoipCidrState (..),

    -- ** ClientAffinity
    ClientAffinity (..),

    -- ** CustomRoutingAcceleratorStatus
    CustomRoutingAcceleratorStatus (..),

    -- ** CustomRoutingDestinationTrafficState
    CustomRoutingDestinationTrafficState (..),

    -- ** CustomRoutingProtocol
    CustomRoutingProtocol (..),

    -- ** HealthCheckProtocol
    HealthCheckProtocol (..),

    -- ** HealthState
    HealthState (..),

    -- ** IpAddressType
    IpAddressType (..),

    -- ** Protocol
    Protocol (..),

    -- ** Accelerator
    Accelerator (Accelerator'),
    newAccelerator,

    -- ** AcceleratorAttributes
    AcceleratorAttributes (AcceleratorAttributes'),
    newAcceleratorAttributes,

    -- ** ByoipCidr
    ByoipCidr (ByoipCidr'),
    newByoipCidr,

    -- ** ByoipCidrEvent
    ByoipCidrEvent (ByoipCidrEvent'),
    newByoipCidrEvent,

    -- ** CidrAuthorizationContext
    CidrAuthorizationContext (CidrAuthorizationContext'),
    newCidrAuthorizationContext,

    -- ** CustomRoutingAccelerator
    CustomRoutingAccelerator (CustomRoutingAccelerator'),
    newCustomRoutingAccelerator,

    -- ** CustomRoutingAcceleratorAttributes
    CustomRoutingAcceleratorAttributes (CustomRoutingAcceleratorAttributes'),
    newCustomRoutingAcceleratorAttributes,

    -- ** CustomRoutingDestinationConfiguration
    CustomRoutingDestinationConfiguration (CustomRoutingDestinationConfiguration'),
    newCustomRoutingDestinationConfiguration,

    -- ** CustomRoutingDestinationDescription
    CustomRoutingDestinationDescription (CustomRoutingDestinationDescription'),
    newCustomRoutingDestinationDescription,

    -- ** CustomRoutingEndpointConfiguration
    CustomRoutingEndpointConfiguration (CustomRoutingEndpointConfiguration'),
    newCustomRoutingEndpointConfiguration,

    -- ** CustomRoutingEndpointDescription
    CustomRoutingEndpointDescription (CustomRoutingEndpointDescription'),
    newCustomRoutingEndpointDescription,

    -- ** CustomRoutingEndpointGroup
    CustomRoutingEndpointGroup (CustomRoutingEndpointGroup'),
    newCustomRoutingEndpointGroup,

    -- ** CustomRoutingListener
    CustomRoutingListener (CustomRoutingListener'),
    newCustomRoutingListener,

    -- ** DestinationPortMapping
    DestinationPortMapping (DestinationPortMapping'),
    newDestinationPortMapping,

    -- ** EndpointConfiguration
    EndpointConfiguration (EndpointConfiguration'),
    newEndpointConfiguration,

    -- ** EndpointDescription
    EndpointDescription (EndpointDescription'),
    newEndpointDescription,

    -- ** EndpointGroup
    EndpointGroup (EndpointGroup'),
    newEndpointGroup,

    -- ** IpSet
    IpSet (IpSet'),
    newIpSet,

    -- ** Listener
    Listener (Listener'),
    newListener,

    -- ** PortMapping
    PortMapping (PortMapping'),
    newPortMapping,

    -- ** PortOverride
    PortOverride (PortOverride'),
    newPortOverride,

    -- ** PortRange
    PortRange (PortRange'),
    newPortRange,

    -- ** SocketAddress
    SocketAddress (SocketAddress'),
    newSocketAddress,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Amazonka.GlobalAccelerator.AddCustomRoutingEndpoints
import Amazonka.GlobalAccelerator.AdvertiseByoipCidr
import Amazonka.GlobalAccelerator.AllowCustomRoutingTraffic
import Amazonka.GlobalAccelerator.CreateAccelerator
import Amazonka.GlobalAccelerator.CreateCustomRoutingAccelerator
import Amazonka.GlobalAccelerator.CreateCustomRoutingEndpointGroup
import Amazonka.GlobalAccelerator.CreateCustomRoutingListener
import Amazonka.GlobalAccelerator.CreateEndpointGroup
import Amazonka.GlobalAccelerator.CreateListener
import Amazonka.GlobalAccelerator.DeleteAccelerator
import Amazonka.GlobalAccelerator.DeleteCustomRoutingAccelerator
import Amazonka.GlobalAccelerator.DeleteCustomRoutingEndpointGroup
import Amazonka.GlobalAccelerator.DeleteCustomRoutingListener
import Amazonka.GlobalAccelerator.DeleteEndpointGroup
import Amazonka.GlobalAccelerator.DeleteListener
import Amazonka.GlobalAccelerator.DenyCustomRoutingTraffic
import Amazonka.GlobalAccelerator.DeprovisionByoipCidr
import Amazonka.GlobalAccelerator.DescribeAccelerator
import Amazonka.GlobalAccelerator.DescribeAcceleratorAttributes
import Amazonka.GlobalAccelerator.DescribeCustomRoutingAccelerator
import Amazonka.GlobalAccelerator.DescribeCustomRoutingAcceleratorAttributes
import Amazonka.GlobalAccelerator.DescribeCustomRoutingEndpointGroup
import Amazonka.GlobalAccelerator.DescribeCustomRoutingListener
import Amazonka.GlobalAccelerator.DescribeEndpointGroup
import Amazonka.GlobalAccelerator.DescribeListener
import Amazonka.GlobalAccelerator.Lens
import Amazonka.GlobalAccelerator.ListAccelerators
import Amazonka.GlobalAccelerator.ListByoipCidrs
import Amazonka.GlobalAccelerator.ListCustomRoutingAccelerators
import Amazonka.GlobalAccelerator.ListCustomRoutingEndpointGroups
import Amazonka.GlobalAccelerator.ListCustomRoutingListeners
import Amazonka.GlobalAccelerator.ListCustomRoutingPortMappings
import Amazonka.GlobalAccelerator.ListCustomRoutingPortMappingsByDestination
import Amazonka.GlobalAccelerator.ListEndpointGroups
import Amazonka.GlobalAccelerator.ListListeners
import Amazonka.GlobalAccelerator.ListTagsForResource
import Amazonka.GlobalAccelerator.ProvisionByoipCidr
import Amazonka.GlobalAccelerator.RemoveCustomRoutingEndpoints
import Amazonka.GlobalAccelerator.TagResource
import Amazonka.GlobalAccelerator.Types
import Amazonka.GlobalAccelerator.UntagResource
import Amazonka.GlobalAccelerator.UpdateAccelerator
import Amazonka.GlobalAccelerator.UpdateAcceleratorAttributes
import Amazonka.GlobalAccelerator.UpdateCustomRoutingAccelerator
import Amazonka.GlobalAccelerator.UpdateCustomRoutingAcceleratorAttributes
import Amazonka.GlobalAccelerator.UpdateCustomRoutingListener
import Amazonka.GlobalAccelerator.UpdateEndpointGroup
import Amazonka.GlobalAccelerator.UpdateListener
import Amazonka.GlobalAccelerator.Waiters
import Amazonka.GlobalAccelerator.WithdrawByoipCidr

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'GlobalAccelerator'.

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
