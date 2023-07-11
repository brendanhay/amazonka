{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.GlobalAccelerator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-08-08@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Global Accelerator
--
-- This is the /Global Accelerator API Reference/. This guide is for
-- developers who need detailed information about Global Accelerator API
-- actions, data types, and errors. For more information about Global
-- Accelerator features, see the
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/what-is-global-accelerator.html Global Accelerator Developer Guide>.
--
-- Global Accelerator is a service in which you create /accelerators/ to
-- improve the performance of your applications for local and global users.
-- Depending on the type of accelerator you choose, you can gain additional
-- benefits.
--
-- -   By using a standard accelerator, you can improve availability of
--     your internet applications that are used by a global audience. With
--     a standard accelerator, Global Accelerator directs traffic to
--     optimal endpoints over the Amazon Web Services global network.
--
-- -   For other scenarios, you might choose a custom routing accelerator.
--     With a custom routing accelerator, you can use application logic to
--     directly map one or more users to a specific endpoint among many
--     endpoints.
--
-- Global Accelerator is a global service that supports endpoints in
-- multiple Amazon Web Services Regions but you must specify the US West
-- (Oregon) Region to create, update, or otherwise work with accelerators.
-- That is, for example, specify @--region us-west-2@ on AWS CLI commands.
--
-- By default, Global Accelerator provides you with static IP addresses
-- that you associate with your accelerator. The static IP addresses are
-- anycast from the Amazon Web Services edge network. For IPv4, Global
-- Accelerator provides two static IPv4 addresses. For dual-stack, Global
-- Accelerator provides a total of four addresses: two static IPv4
-- addresses and two static IPv6 addresses. With a standard accelerator for
-- IPv4, instead of using the addresses that Global Accelerator provides,
-- you can configure these entry points to be IPv4 addresses from your own
-- IP address ranges that you bring toGlobal Accelerator (BYOIP).
--
-- For a standard accelerator, they distribute incoming application traffic
-- across multiple endpoint resources in multiple Amazon Web Services
-- Regions , which increases the availability of your applications.
-- Endpoints for standard accelerators can be Network Load Balancers,
-- Application Load Balancers, Amazon EC2 instances, or Elastic IP
-- addresses that are located in one Amazon Web Services Region or multiple
-- Amazon Web Services Regions. For custom routing accelerators, you map
-- traffic that arrives to the static IP addresses to specific Amazon EC2
-- servers in endpoints that are virtual private cloud (VPC) subnets.
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
-- For standard accelerators, Global Accelerator uses the Amazon Web
-- Services global network to route traffic to the optimal regional
-- endpoint based on health, client location, and policies that you
-- configure. The service reacts instantly to changes in health or
-- configuration to ensure that internet traffic from clients is always
-- directed to healthy endpoints.
--
-- For more information about understanding and using Global Accelerator,
-- see the
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/what-is-global-accelerator.html Global Accelerator Developer Guide>.
module Amazonka.GlobalAccelerator
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AcceleratorNotDisabledException
    _AcceleratorNotDisabledException,

    -- ** AcceleratorNotFoundException
    _AcceleratorNotFoundException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** AssociatedEndpointGroupFoundException
    _AssociatedEndpointGroupFoundException,

    -- ** AssociatedListenerFoundException
    _AssociatedListenerFoundException,

    -- ** ByoipCidrNotFoundException
    _ByoipCidrNotFoundException,

    -- ** ConflictException
    _ConflictException,

    -- ** EndpointAlreadyExistsException
    _EndpointAlreadyExistsException,

    -- ** EndpointGroupAlreadyExistsException
    _EndpointGroupAlreadyExistsException,

    -- ** EndpointGroupNotFoundException
    _EndpointGroupNotFoundException,

    -- ** EndpointNotFoundException
    _EndpointNotFoundException,

    -- ** IncorrectCidrStateException
    _IncorrectCidrStateException,

    -- ** InternalServiceErrorException
    _InternalServiceErrorException,

    -- ** InvalidArgumentException
    _InvalidArgumentException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** InvalidPortRangeException
    _InvalidPortRangeException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ListenerNotFoundException
    _ListenerNotFoundException,

    -- ** TransactionInProgressException
    _TransactionInProgressException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AddCustomRoutingEndpoints
    AddCustomRoutingEndpoints (AddCustomRoutingEndpoints'),
    newAddCustomRoutingEndpoints,
    AddCustomRoutingEndpointsResponse (AddCustomRoutingEndpointsResponse'),
    newAddCustomRoutingEndpointsResponse,

    -- ** AddEndpoints
    AddEndpoints (AddEndpoints'),
    newAddEndpoints,
    AddEndpointsResponse (AddEndpointsResponse'),
    newAddEndpointsResponse,

    -- ** AdvertiseByoipCidr
    AdvertiseByoipCidr (AdvertiseByoipCidr'),
    newAdvertiseByoipCidr,
    AdvertiseByoipCidrResponse (AdvertiseByoipCidrResponse'),
    newAdvertiseByoipCidrResponse,

    -- ** AllowCustomRoutingTraffic
    AllowCustomRoutingTraffic (AllowCustomRoutingTraffic'),
    newAllowCustomRoutingTraffic,
    AllowCustomRoutingTrafficResponse (AllowCustomRoutingTrafficResponse'),
    newAllowCustomRoutingTrafficResponse,

    -- ** CreateAccelerator
    CreateAccelerator (CreateAccelerator'),
    newCreateAccelerator,
    CreateAcceleratorResponse (CreateAcceleratorResponse'),
    newCreateAcceleratorResponse,

    -- ** CreateCustomRoutingAccelerator
    CreateCustomRoutingAccelerator (CreateCustomRoutingAccelerator'),
    newCreateCustomRoutingAccelerator,
    CreateCustomRoutingAcceleratorResponse (CreateCustomRoutingAcceleratorResponse'),
    newCreateCustomRoutingAcceleratorResponse,

    -- ** CreateCustomRoutingEndpointGroup
    CreateCustomRoutingEndpointGroup (CreateCustomRoutingEndpointGroup'),
    newCreateCustomRoutingEndpointGroup,
    CreateCustomRoutingEndpointGroupResponse (CreateCustomRoutingEndpointGroupResponse'),
    newCreateCustomRoutingEndpointGroupResponse,

    -- ** CreateCustomRoutingListener
    CreateCustomRoutingListener (CreateCustomRoutingListener'),
    newCreateCustomRoutingListener,
    CreateCustomRoutingListenerResponse (CreateCustomRoutingListenerResponse'),
    newCreateCustomRoutingListenerResponse,

    -- ** CreateEndpointGroup
    CreateEndpointGroup (CreateEndpointGroup'),
    newCreateEndpointGroup,
    CreateEndpointGroupResponse (CreateEndpointGroupResponse'),
    newCreateEndpointGroupResponse,

    -- ** CreateListener
    CreateListener (CreateListener'),
    newCreateListener,
    CreateListenerResponse (CreateListenerResponse'),
    newCreateListenerResponse,

    -- ** DeleteAccelerator
    DeleteAccelerator (DeleteAccelerator'),
    newDeleteAccelerator,
    DeleteAcceleratorResponse (DeleteAcceleratorResponse'),
    newDeleteAcceleratorResponse,

    -- ** DeleteCustomRoutingAccelerator
    DeleteCustomRoutingAccelerator (DeleteCustomRoutingAccelerator'),
    newDeleteCustomRoutingAccelerator,
    DeleteCustomRoutingAcceleratorResponse (DeleteCustomRoutingAcceleratorResponse'),
    newDeleteCustomRoutingAcceleratorResponse,

    -- ** DeleteCustomRoutingEndpointGroup
    DeleteCustomRoutingEndpointGroup (DeleteCustomRoutingEndpointGroup'),
    newDeleteCustomRoutingEndpointGroup,
    DeleteCustomRoutingEndpointGroupResponse (DeleteCustomRoutingEndpointGroupResponse'),
    newDeleteCustomRoutingEndpointGroupResponse,

    -- ** DeleteCustomRoutingListener
    DeleteCustomRoutingListener (DeleteCustomRoutingListener'),
    newDeleteCustomRoutingListener,
    DeleteCustomRoutingListenerResponse (DeleteCustomRoutingListenerResponse'),
    newDeleteCustomRoutingListenerResponse,

    -- ** DeleteEndpointGroup
    DeleteEndpointGroup (DeleteEndpointGroup'),
    newDeleteEndpointGroup,
    DeleteEndpointGroupResponse (DeleteEndpointGroupResponse'),
    newDeleteEndpointGroupResponse,

    -- ** DeleteListener
    DeleteListener (DeleteListener'),
    newDeleteListener,
    DeleteListenerResponse (DeleteListenerResponse'),
    newDeleteListenerResponse,

    -- ** DenyCustomRoutingTraffic
    DenyCustomRoutingTraffic (DenyCustomRoutingTraffic'),
    newDenyCustomRoutingTraffic,
    DenyCustomRoutingTrafficResponse (DenyCustomRoutingTrafficResponse'),
    newDenyCustomRoutingTrafficResponse,

    -- ** DeprovisionByoipCidr
    DeprovisionByoipCidr (DeprovisionByoipCidr'),
    newDeprovisionByoipCidr,
    DeprovisionByoipCidrResponse (DeprovisionByoipCidrResponse'),
    newDeprovisionByoipCidrResponse,

    -- ** DescribeAccelerator
    DescribeAccelerator (DescribeAccelerator'),
    newDescribeAccelerator,
    DescribeAcceleratorResponse (DescribeAcceleratorResponse'),
    newDescribeAcceleratorResponse,

    -- ** DescribeAcceleratorAttributes
    DescribeAcceleratorAttributes (DescribeAcceleratorAttributes'),
    newDescribeAcceleratorAttributes,
    DescribeAcceleratorAttributesResponse (DescribeAcceleratorAttributesResponse'),
    newDescribeAcceleratorAttributesResponse,

    -- ** DescribeCustomRoutingAccelerator
    DescribeCustomRoutingAccelerator (DescribeCustomRoutingAccelerator'),
    newDescribeCustomRoutingAccelerator,
    DescribeCustomRoutingAcceleratorResponse (DescribeCustomRoutingAcceleratorResponse'),
    newDescribeCustomRoutingAcceleratorResponse,

    -- ** DescribeCustomRoutingAcceleratorAttributes
    DescribeCustomRoutingAcceleratorAttributes (DescribeCustomRoutingAcceleratorAttributes'),
    newDescribeCustomRoutingAcceleratorAttributes,
    DescribeCustomRoutingAcceleratorAttributesResponse (DescribeCustomRoutingAcceleratorAttributesResponse'),
    newDescribeCustomRoutingAcceleratorAttributesResponse,

    -- ** DescribeCustomRoutingEndpointGroup
    DescribeCustomRoutingEndpointGroup (DescribeCustomRoutingEndpointGroup'),
    newDescribeCustomRoutingEndpointGroup,
    DescribeCustomRoutingEndpointGroupResponse (DescribeCustomRoutingEndpointGroupResponse'),
    newDescribeCustomRoutingEndpointGroupResponse,

    -- ** DescribeCustomRoutingListener
    DescribeCustomRoutingListener (DescribeCustomRoutingListener'),
    newDescribeCustomRoutingListener,
    DescribeCustomRoutingListenerResponse (DescribeCustomRoutingListenerResponse'),
    newDescribeCustomRoutingListenerResponse,

    -- ** DescribeEndpointGroup
    DescribeEndpointGroup (DescribeEndpointGroup'),
    newDescribeEndpointGroup,
    DescribeEndpointGroupResponse (DescribeEndpointGroupResponse'),
    newDescribeEndpointGroupResponse,

    -- ** DescribeListener
    DescribeListener (DescribeListener'),
    newDescribeListener,
    DescribeListenerResponse (DescribeListenerResponse'),
    newDescribeListenerResponse,

    -- ** ListAccelerators (Paginated)
    ListAccelerators (ListAccelerators'),
    newListAccelerators,
    ListAcceleratorsResponse (ListAcceleratorsResponse'),
    newListAcceleratorsResponse,

    -- ** ListByoipCidrs (Paginated)
    ListByoipCidrs (ListByoipCidrs'),
    newListByoipCidrs,
    ListByoipCidrsResponse (ListByoipCidrsResponse'),
    newListByoipCidrsResponse,

    -- ** ListCustomRoutingAccelerators (Paginated)
    ListCustomRoutingAccelerators (ListCustomRoutingAccelerators'),
    newListCustomRoutingAccelerators,
    ListCustomRoutingAcceleratorsResponse (ListCustomRoutingAcceleratorsResponse'),
    newListCustomRoutingAcceleratorsResponse,

    -- ** ListCustomRoutingEndpointGroups
    ListCustomRoutingEndpointGroups (ListCustomRoutingEndpointGroups'),
    newListCustomRoutingEndpointGroups,
    ListCustomRoutingEndpointGroupsResponse (ListCustomRoutingEndpointGroupsResponse'),
    newListCustomRoutingEndpointGroupsResponse,

    -- ** ListCustomRoutingListeners (Paginated)
    ListCustomRoutingListeners (ListCustomRoutingListeners'),
    newListCustomRoutingListeners,
    ListCustomRoutingListenersResponse (ListCustomRoutingListenersResponse'),
    newListCustomRoutingListenersResponse,

    -- ** ListCustomRoutingPortMappings (Paginated)
    ListCustomRoutingPortMappings (ListCustomRoutingPortMappings'),
    newListCustomRoutingPortMappings,
    ListCustomRoutingPortMappingsResponse (ListCustomRoutingPortMappingsResponse'),
    newListCustomRoutingPortMappingsResponse,

    -- ** ListCustomRoutingPortMappingsByDestination (Paginated)
    ListCustomRoutingPortMappingsByDestination (ListCustomRoutingPortMappingsByDestination'),
    newListCustomRoutingPortMappingsByDestination,
    ListCustomRoutingPortMappingsByDestinationResponse (ListCustomRoutingPortMappingsByDestinationResponse'),
    newListCustomRoutingPortMappingsByDestinationResponse,

    -- ** ListEndpointGroups (Paginated)
    ListEndpointGroups (ListEndpointGroups'),
    newListEndpointGroups,
    ListEndpointGroupsResponse (ListEndpointGroupsResponse'),
    newListEndpointGroupsResponse,

    -- ** ListListeners (Paginated)
    ListListeners (ListListeners'),
    newListListeners,
    ListListenersResponse (ListListenersResponse'),
    newListListenersResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ProvisionByoipCidr
    ProvisionByoipCidr (ProvisionByoipCidr'),
    newProvisionByoipCidr,
    ProvisionByoipCidrResponse (ProvisionByoipCidrResponse'),
    newProvisionByoipCidrResponse,

    -- ** RemoveCustomRoutingEndpoints
    RemoveCustomRoutingEndpoints (RemoveCustomRoutingEndpoints'),
    newRemoveCustomRoutingEndpoints,
    RemoveCustomRoutingEndpointsResponse (RemoveCustomRoutingEndpointsResponse'),
    newRemoveCustomRoutingEndpointsResponse,

    -- ** RemoveEndpoints
    RemoveEndpoints (RemoveEndpoints'),
    newRemoveEndpoints,
    RemoveEndpointsResponse (RemoveEndpointsResponse'),
    newRemoveEndpointsResponse,

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

    -- ** UpdateAccelerator
    UpdateAccelerator (UpdateAccelerator'),
    newUpdateAccelerator,
    UpdateAcceleratorResponse (UpdateAcceleratorResponse'),
    newUpdateAcceleratorResponse,

    -- ** UpdateAcceleratorAttributes
    UpdateAcceleratorAttributes (UpdateAcceleratorAttributes'),
    newUpdateAcceleratorAttributes,
    UpdateAcceleratorAttributesResponse (UpdateAcceleratorAttributesResponse'),
    newUpdateAcceleratorAttributesResponse,

    -- ** UpdateCustomRoutingAccelerator
    UpdateCustomRoutingAccelerator (UpdateCustomRoutingAccelerator'),
    newUpdateCustomRoutingAccelerator,
    UpdateCustomRoutingAcceleratorResponse (UpdateCustomRoutingAcceleratorResponse'),
    newUpdateCustomRoutingAcceleratorResponse,

    -- ** UpdateCustomRoutingAcceleratorAttributes
    UpdateCustomRoutingAcceleratorAttributes (UpdateCustomRoutingAcceleratorAttributes'),
    newUpdateCustomRoutingAcceleratorAttributes,
    UpdateCustomRoutingAcceleratorAttributesResponse (UpdateCustomRoutingAcceleratorAttributesResponse'),
    newUpdateCustomRoutingAcceleratorAttributesResponse,

    -- ** UpdateCustomRoutingListener
    UpdateCustomRoutingListener (UpdateCustomRoutingListener'),
    newUpdateCustomRoutingListener,
    UpdateCustomRoutingListenerResponse (UpdateCustomRoutingListenerResponse'),
    newUpdateCustomRoutingListenerResponse,

    -- ** UpdateEndpointGroup
    UpdateEndpointGroup (UpdateEndpointGroup'),
    newUpdateEndpointGroup,
    UpdateEndpointGroupResponse (UpdateEndpointGroupResponse'),
    newUpdateEndpointGroupResponse,

    -- ** UpdateListener
    UpdateListener (UpdateListener'),
    newUpdateListener,
    UpdateListenerResponse (UpdateListenerResponse'),
    newUpdateListenerResponse,

    -- ** WithdrawByoipCidr
    WithdrawByoipCidr (WithdrawByoipCidr'),
    newWithdrawByoipCidr,
    WithdrawByoipCidrResponse (WithdrawByoipCidrResponse'),
    newWithdrawByoipCidrResponse,

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

    -- ** IpAddressFamily
    IpAddressFamily (..),

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

    -- ** AcceleratorEvent
    AcceleratorEvent (AcceleratorEvent'),
    newAcceleratorEvent,

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

    -- ** EndpointIdentifier
    EndpointIdentifier (EndpointIdentifier'),
    newEndpointIdentifier,

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
import Amazonka.GlobalAccelerator.AddEndpoints
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
import Amazonka.GlobalAccelerator.RemoveEndpoints
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
