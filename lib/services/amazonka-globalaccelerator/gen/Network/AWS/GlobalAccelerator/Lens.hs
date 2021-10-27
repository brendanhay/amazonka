{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GlobalAccelerator.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GlobalAccelerator.Lens
  ( -- * Operations

    -- ** DenyCustomRoutingTraffic
    denyCustomRoutingTraffic_destinationAddresses,
    denyCustomRoutingTraffic_destinationPorts,
    denyCustomRoutingTraffic_denyAllTrafficToEndpoint,
    denyCustomRoutingTraffic_endpointGroupArn,
    denyCustomRoutingTraffic_endpointId,

    -- ** DescribeCustomRoutingListener
    describeCustomRoutingListener_listenerArn,
    describeCustomRoutingListenerResponse_listener,
    describeCustomRoutingListenerResponse_httpStatus,

    -- ** CreateCustomRoutingEndpointGroup
    createCustomRoutingEndpointGroup_listenerArn,
    createCustomRoutingEndpointGroup_endpointGroupRegion,
    createCustomRoutingEndpointGroup_destinationConfigurations,
    createCustomRoutingEndpointGroup_idempotencyToken,
    createCustomRoutingEndpointGroupResponse_endpointGroup,
    createCustomRoutingEndpointGroupResponse_httpStatus,

    -- ** DescribeCustomRoutingAcceleratorAttributes
    describeCustomRoutingAcceleratorAttributes_acceleratorArn,
    describeCustomRoutingAcceleratorAttributesResponse_acceleratorAttributes,
    describeCustomRoutingAcceleratorAttributesResponse_httpStatus,

    -- ** DeleteCustomRoutingEndpointGroup
    deleteCustomRoutingEndpointGroup_endpointGroupArn,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DescribeAcceleratorAttributes
    describeAcceleratorAttributes_acceleratorArn,
    describeAcceleratorAttributesResponse_acceleratorAttributes,
    describeAcceleratorAttributesResponse_httpStatus,

    -- ** DeleteEndpointGroup
    deleteEndpointGroup_endpointGroupArn,

    -- ** UpdateEndpointGroup
    updateEndpointGroup_thresholdCount,
    updateEndpointGroup_healthCheckPath,
    updateEndpointGroup_healthCheckIntervalSeconds,
    updateEndpointGroup_healthCheckProtocol,
    updateEndpointGroup_trafficDialPercentage,
    updateEndpointGroup_endpointConfigurations,
    updateEndpointGroup_healthCheckPort,
    updateEndpointGroup_portOverrides,
    updateEndpointGroup_endpointGroupArn,
    updateEndpointGroupResponse_endpointGroup,
    updateEndpointGroupResponse_httpStatus,

    -- ** ListCustomRoutingListeners
    listCustomRoutingListeners_nextToken,
    listCustomRoutingListeners_maxResults,
    listCustomRoutingListeners_acceleratorArn,
    listCustomRoutingListenersResponse_nextToken,
    listCustomRoutingListenersResponse_listeners,
    listCustomRoutingListenersResponse_httpStatus,

    -- ** DeleteCustomRoutingListener
    deleteCustomRoutingListener_listenerArn,

    -- ** UpdateCustomRoutingListener
    updateCustomRoutingListener_listenerArn,
    updateCustomRoutingListener_portRanges,
    updateCustomRoutingListenerResponse_listener,
    updateCustomRoutingListenerResponse_httpStatus,

    -- ** CreateAccelerator
    createAccelerator_enabled,
    createAccelerator_ipAddressType,
    createAccelerator_tags,
    createAccelerator_ipAddresses,
    createAccelerator_name,
    createAccelerator_idempotencyToken,
    createAcceleratorResponse_accelerator,
    createAcceleratorResponse_httpStatus,

    -- ** AllowCustomRoutingTraffic
    allowCustomRoutingTraffic_destinationAddresses,
    allowCustomRoutingTraffic_destinationPorts,
    allowCustomRoutingTraffic_allowAllTrafficToEndpoint,
    allowCustomRoutingTraffic_endpointGroupArn,
    allowCustomRoutingTraffic_endpointId,

    -- ** WithdrawByoipCidr
    withdrawByoipCidr_cidr,
    withdrawByoipCidrResponse_byoipCidr,
    withdrawByoipCidrResponse_httpStatus,

    -- ** AdvertiseByoipCidr
    advertiseByoipCidr_cidr,
    advertiseByoipCidrResponse_byoipCidr,
    advertiseByoipCidrResponse_httpStatus,

    -- ** DeleteAccelerator
    deleteAccelerator_acceleratorArn,

    -- ** UpdateAccelerator
    updateAccelerator_enabled,
    updateAccelerator_ipAddressType,
    updateAccelerator_name,
    updateAccelerator_acceleratorArn,
    updateAcceleratorResponse_accelerator,
    updateAcceleratorResponse_httpStatus,

    -- ** ListAccelerators
    listAccelerators_nextToken,
    listAccelerators_maxResults,
    listAcceleratorsResponse_nextToken,
    listAcceleratorsResponse_accelerators,
    listAcceleratorsResponse_httpStatus,

    -- ** DescribeEndpointGroup
    describeEndpointGroup_endpointGroupArn,
    describeEndpointGroupResponse_endpointGroup,
    describeEndpointGroupResponse_httpStatus,

    -- ** UpdateAcceleratorAttributes
    updateAcceleratorAttributes_flowLogsS3Prefix,
    updateAcceleratorAttributes_flowLogsEnabled,
    updateAcceleratorAttributes_flowLogsS3Bucket,
    updateAcceleratorAttributes_acceleratorArn,
    updateAcceleratorAttributesResponse_acceleratorAttributes,
    updateAcceleratorAttributesResponse_httpStatus,

    -- ** CreateCustomRoutingAccelerator
    createCustomRoutingAccelerator_enabled,
    createCustomRoutingAccelerator_ipAddressType,
    createCustomRoutingAccelerator_tags,
    createCustomRoutingAccelerator_ipAddresses,
    createCustomRoutingAccelerator_name,
    createCustomRoutingAccelerator_idempotencyToken,
    createCustomRoutingAcceleratorResponse_accelerator,
    createCustomRoutingAcceleratorResponse_httpStatus,

    -- ** ListCustomRoutingPortMappingsByDestination
    listCustomRoutingPortMappingsByDestination_nextToken,
    listCustomRoutingPortMappingsByDestination_maxResults,
    listCustomRoutingPortMappingsByDestination_endpointId,
    listCustomRoutingPortMappingsByDestination_destinationAddress,
    listCustomRoutingPortMappingsByDestinationResponse_nextToken,
    listCustomRoutingPortMappingsByDestinationResponse_destinationPortMappings,
    listCustomRoutingPortMappingsByDestinationResponse_httpStatus,

    -- ** DeleteListener
    deleteListener_listenerArn,

    -- ** UpdateListener
    updateListener_portRanges,
    updateListener_protocol,
    updateListener_clientAffinity,
    updateListener_listenerArn,
    updateListenerResponse_listener,
    updateListenerResponse_httpStatus,

    -- ** ListListeners
    listListeners_nextToken,
    listListeners_maxResults,
    listListeners_acceleratorArn,
    listListenersResponse_nextToken,
    listListenersResponse_listeners,
    listListenersResponse_httpStatus,

    -- ** ListCustomRoutingEndpointGroups
    listCustomRoutingEndpointGroups_nextToken,
    listCustomRoutingEndpointGroups_maxResults,
    listCustomRoutingEndpointGroups_listenerArn,
    listCustomRoutingEndpointGroupsResponse_nextToken,
    listCustomRoutingEndpointGroupsResponse_endpointGroups,
    listCustomRoutingEndpointGroupsResponse_httpStatus,

    -- ** CreateListener
    createListener_clientAffinity,
    createListener_acceleratorArn,
    createListener_portRanges,
    createListener_protocol,
    createListener_idempotencyToken,
    createListenerResponse_listener,
    createListenerResponse_httpStatus,

    -- ** DescribeAccelerator
    describeAccelerator_acceleratorArn,
    describeAcceleratorResponse_accelerator,
    describeAcceleratorResponse_httpStatus,

    -- ** CreateCustomRoutingListener
    createCustomRoutingListener_acceleratorArn,
    createCustomRoutingListener_portRanges,
    createCustomRoutingListener_idempotencyToken,
    createCustomRoutingListenerResponse_listener,
    createCustomRoutingListenerResponse_httpStatus,

    -- ** DescribeCustomRoutingAccelerator
    describeCustomRoutingAccelerator_acceleratorArn,
    describeCustomRoutingAcceleratorResponse_accelerator,
    describeCustomRoutingAcceleratorResponse_httpStatus,

    -- ** ListEndpointGroups
    listEndpointGroups_nextToken,
    listEndpointGroups_maxResults,
    listEndpointGroups_listenerArn,
    listEndpointGroupsResponse_nextToken,
    listEndpointGroupsResponse_endpointGroups,
    listEndpointGroupsResponse_httpStatus,

    -- ** ProvisionByoipCidr
    provisionByoipCidr_cidr,
    provisionByoipCidr_cidrAuthorizationContext,
    provisionByoipCidrResponse_byoipCidr,
    provisionByoipCidrResponse_httpStatus,

    -- ** CreateEndpointGroup
    createEndpointGroup_thresholdCount,
    createEndpointGroup_healthCheckPath,
    createEndpointGroup_healthCheckIntervalSeconds,
    createEndpointGroup_healthCheckProtocol,
    createEndpointGroup_trafficDialPercentage,
    createEndpointGroup_endpointConfigurations,
    createEndpointGroup_healthCheckPort,
    createEndpointGroup_portOverrides,
    createEndpointGroup_listenerArn,
    createEndpointGroup_endpointGroupRegion,
    createEndpointGroup_idempotencyToken,
    createEndpointGroupResponse_endpointGroup,
    createEndpointGroupResponse_httpStatus,

    -- ** ListByoipCidrs
    listByoipCidrs_nextToken,
    listByoipCidrs_maxResults,
    listByoipCidrsResponse_nextToken,
    listByoipCidrsResponse_byoipCidrs,
    listByoipCidrsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DescribeListener
    describeListener_listenerArn,
    describeListenerResponse_listener,
    describeListenerResponse_httpStatus,

    -- ** ListCustomRoutingPortMappings
    listCustomRoutingPortMappings_endpointGroupArn,
    listCustomRoutingPortMappings_nextToken,
    listCustomRoutingPortMappings_maxResults,
    listCustomRoutingPortMappings_acceleratorArn,
    listCustomRoutingPortMappingsResponse_portMappings,
    listCustomRoutingPortMappingsResponse_nextToken,
    listCustomRoutingPortMappingsResponse_httpStatus,

    -- ** AddCustomRoutingEndpoints
    addCustomRoutingEndpoints_endpointConfigurations,
    addCustomRoutingEndpoints_endpointGroupArn,
    addCustomRoutingEndpointsResponse_endpointGroupArn,
    addCustomRoutingEndpointsResponse_endpointDescriptions,
    addCustomRoutingEndpointsResponse_httpStatus,

    -- ** DescribeCustomRoutingEndpointGroup
    describeCustomRoutingEndpointGroup_endpointGroupArn,
    describeCustomRoutingEndpointGroupResponse_endpointGroup,
    describeCustomRoutingEndpointGroupResponse_httpStatus,

    -- ** UpdateCustomRoutingAcceleratorAttributes
    updateCustomRoutingAcceleratorAttributes_flowLogsS3Prefix,
    updateCustomRoutingAcceleratorAttributes_flowLogsEnabled,
    updateCustomRoutingAcceleratorAttributes_flowLogsS3Bucket,
    updateCustomRoutingAcceleratorAttributes_acceleratorArn,
    updateCustomRoutingAcceleratorAttributesResponse_acceleratorAttributes,
    updateCustomRoutingAcceleratorAttributesResponse_httpStatus,

    -- ** RemoveCustomRoutingEndpoints
    removeCustomRoutingEndpoints_endpointIds,
    removeCustomRoutingEndpoints_endpointGroupArn,

    -- ** UpdateCustomRoutingAccelerator
    updateCustomRoutingAccelerator_enabled,
    updateCustomRoutingAccelerator_ipAddressType,
    updateCustomRoutingAccelerator_name,
    updateCustomRoutingAccelerator_acceleratorArn,
    updateCustomRoutingAcceleratorResponse_accelerator,
    updateCustomRoutingAcceleratorResponse_httpStatus,

    -- ** DeleteCustomRoutingAccelerator
    deleteCustomRoutingAccelerator_acceleratorArn,

    -- ** ListCustomRoutingAccelerators
    listCustomRoutingAccelerators_nextToken,
    listCustomRoutingAccelerators_maxResults,
    listCustomRoutingAcceleratorsResponse_nextToken,
    listCustomRoutingAcceleratorsResponse_accelerators,
    listCustomRoutingAcceleratorsResponse_httpStatus,

    -- ** DeprovisionByoipCidr
    deprovisionByoipCidr_cidr,
    deprovisionByoipCidrResponse_byoipCidr,
    deprovisionByoipCidrResponse_httpStatus,

    -- * Types

    -- ** Accelerator
    accelerator_status,
    accelerator_acceleratorArn,
    accelerator_enabled,
    accelerator_createdTime,
    accelerator_lastModifiedTime,
    accelerator_ipAddressType,
    accelerator_name,
    accelerator_ipSets,
    accelerator_dnsName,

    -- ** AcceleratorAttributes
    acceleratorAttributes_flowLogsS3Prefix,
    acceleratorAttributes_flowLogsEnabled,
    acceleratorAttributes_flowLogsS3Bucket,

    -- ** ByoipCidr
    byoipCidr_state,
    byoipCidr_cidr,
    byoipCidr_events,

    -- ** ByoipCidrEvent
    byoipCidrEvent_message,
    byoipCidrEvent_timestamp,

    -- ** CidrAuthorizationContext
    cidrAuthorizationContext_message,
    cidrAuthorizationContext_signature,

    -- ** CustomRoutingAccelerator
    customRoutingAccelerator_status,
    customRoutingAccelerator_acceleratorArn,
    customRoutingAccelerator_enabled,
    customRoutingAccelerator_createdTime,
    customRoutingAccelerator_lastModifiedTime,
    customRoutingAccelerator_ipAddressType,
    customRoutingAccelerator_name,
    customRoutingAccelerator_ipSets,
    customRoutingAccelerator_dnsName,

    -- ** CustomRoutingAcceleratorAttributes
    customRoutingAcceleratorAttributes_flowLogsS3Prefix,
    customRoutingAcceleratorAttributes_flowLogsEnabled,
    customRoutingAcceleratorAttributes_flowLogsS3Bucket,

    -- ** CustomRoutingDestinationConfiguration
    customRoutingDestinationConfiguration_fromPort,
    customRoutingDestinationConfiguration_toPort,
    customRoutingDestinationConfiguration_protocols,

    -- ** CustomRoutingDestinationDescription
    customRoutingDestinationDescription_fromPort,
    customRoutingDestinationDescription_protocols,
    customRoutingDestinationDescription_toPort,

    -- ** CustomRoutingEndpointConfiguration
    customRoutingEndpointConfiguration_endpointId,

    -- ** CustomRoutingEndpointDescription
    customRoutingEndpointDescription_endpointId,

    -- ** CustomRoutingEndpointGroup
    customRoutingEndpointGroup_endpointGroupArn,
    customRoutingEndpointGroup_endpointGroupRegion,
    customRoutingEndpointGroup_endpointDescriptions,
    customRoutingEndpointGroup_destinationDescriptions,

    -- ** CustomRoutingListener
    customRoutingListener_portRanges,
    customRoutingListener_listenerArn,

    -- ** DestinationPortMapping
    destinationPortMapping_destinationSocketAddress,
    destinationPortMapping_acceleratorArn,
    destinationPortMapping_endpointGroupArn,
    destinationPortMapping_endpointGroupRegion,
    destinationPortMapping_ipAddressType,
    destinationPortMapping_acceleratorSocketAddresses,
    destinationPortMapping_endpointId,
    destinationPortMapping_destinationTrafficState,

    -- ** EndpointConfiguration
    endpointConfiguration_weight,
    endpointConfiguration_clientIPPreservationEnabled,
    endpointConfiguration_endpointId,

    -- ** EndpointDescription
    endpointDescription_healthReason,
    endpointDescription_weight,
    endpointDescription_clientIPPreservationEnabled,
    endpointDescription_healthState,
    endpointDescription_endpointId,

    -- ** EndpointGroup
    endpointGroup_thresholdCount,
    endpointGroup_healthCheckPath,
    endpointGroup_healthCheckIntervalSeconds,
    endpointGroup_endpointGroupArn,
    endpointGroup_healthCheckProtocol,
    endpointGroup_endpointGroupRegion,
    endpointGroup_trafficDialPercentage,
    endpointGroup_healthCheckPort,
    endpointGroup_portOverrides,
    endpointGroup_endpointDescriptions,

    -- ** IpSet
    ipSet_ipFamily,
    ipSet_ipAddresses,

    -- ** Listener
    listener_portRanges,
    listener_listenerArn,
    listener_protocol,
    listener_clientAffinity,

    -- ** PortMapping
    portMapping_destinationSocketAddress,
    portMapping_protocols,
    portMapping_endpointGroupArn,
    portMapping_endpointId,
    portMapping_destinationTrafficState,
    portMapping_acceleratorPort,

    -- ** PortOverride
    portOverride_endpointPort,
    portOverride_listenerPort,

    -- ** PortRange
    portRange_fromPort,
    portRange_toPort,

    -- ** SocketAddress
    socketAddress_ipAddress,
    socketAddress_port,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Network.AWS.GlobalAccelerator.AddCustomRoutingEndpoints
import Network.AWS.GlobalAccelerator.AdvertiseByoipCidr
import Network.AWS.GlobalAccelerator.AllowCustomRoutingTraffic
import Network.AWS.GlobalAccelerator.CreateAccelerator
import Network.AWS.GlobalAccelerator.CreateCustomRoutingAccelerator
import Network.AWS.GlobalAccelerator.CreateCustomRoutingEndpointGroup
import Network.AWS.GlobalAccelerator.CreateCustomRoutingListener
import Network.AWS.GlobalAccelerator.CreateEndpointGroup
import Network.AWS.GlobalAccelerator.CreateListener
import Network.AWS.GlobalAccelerator.DeleteAccelerator
import Network.AWS.GlobalAccelerator.DeleteCustomRoutingAccelerator
import Network.AWS.GlobalAccelerator.DeleteCustomRoutingEndpointGroup
import Network.AWS.GlobalAccelerator.DeleteCustomRoutingListener
import Network.AWS.GlobalAccelerator.DeleteEndpointGroup
import Network.AWS.GlobalAccelerator.DeleteListener
import Network.AWS.GlobalAccelerator.DenyCustomRoutingTraffic
import Network.AWS.GlobalAccelerator.DeprovisionByoipCidr
import Network.AWS.GlobalAccelerator.DescribeAccelerator
import Network.AWS.GlobalAccelerator.DescribeAcceleratorAttributes
import Network.AWS.GlobalAccelerator.DescribeCustomRoutingAccelerator
import Network.AWS.GlobalAccelerator.DescribeCustomRoutingAcceleratorAttributes
import Network.AWS.GlobalAccelerator.DescribeCustomRoutingEndpointGroup
import Network.AWS.GlobalAccelerator.DescribeCustomRoutingListener
import Network.AWS.GlobalAccelerator.DescribeEndpointGroup
import Network.AWS.GlobalAccelerator.DescribeListener
import Network.AWS.GlobalAccelerator.ListAccelerators
import Network.AWS.GlobalAccelerator.ListByoipCidrs
import Network.AWS.GlobalAccelerator.ListCustomRoutingAccelerators
import Network.AWS.GlobalAccelerator.ListCustomRoutingEndpointGroups
import Network.AWS.GlobalAccelerator.ListCustomRoutingListeners
import Network.AWS.GlobalAccelerator.ListCustomRoutingPortMappings
import Network.AWS.GlobalAccelerator.ListCustomRoutingPortMappingsByDestination
import Network.AWS.GlobalAccelerator.ListEndpointGroups
import Network.AWS.GlobalAccelerator.ListListeners
import Network.AWS.GlobalAccelerator.ListTagsForResource
import Network.AWS.GlobalAccelerator.ProvisionByoipCidr
import Network.AWS.GlobalAccelerator.RemoveCustomRoutingEndpoints
import Network.AWS.GlobalAccelerator.TagResource
import Network.AWS.GlobalAccelerator.Types.Accelerator
import Network.AWS.GlobalAccelerator.Types.AcceleratorAttributes
import Network.AWS.GlobalAccelerator.Types.ByoipCidr
import Network.AWS.GlobalAccelerator.Types.ByoipCidrEvent
import Network.AWS.GlobalAccelerator.Types.CidrAuthorizationContext
import Network.AWS.GlobalAccelerator.Types.CustomRoutingAccelerator
import Network.AWS.GlobalAccelerator.Types.CustomRoutingAcceleratorAttributes
import Network.AWS.GlobalAccelerator.Types.CustomRoutingDestinationConfiguration
import Network.AWS.GlobalAccelerator.Types.CustomRoutingDestinationDescription
import Network.AWS.GlobalAccelerator.Types.CustomRoutingEndpointConfiguration
import Network.AWS.GlobalAccelerator.Types.CustomRoutingEndpointDescription
import Network.AWS.GlobalAccelerator.Types.CustomRoutingEndpointGroup
import Network.AWS.GlobalAccelerator.Types.CustomRoutingListener
import Network.AWS.GlobalAccelerator.Types.DestinationPortMapping
import Network.AWS.GlobalAccelerator.Types.EndpointConfiguration
import Network.AWS.GlobalAccelerator.Types.EndpointDescription
import Network.AWS.GlobalAccelerator.Types.EndpointGroup
import Network.AWS.GlobalAccelerator.Types.IpSet
import Network.AWS.GlobalAccelerator.Types.Listener
import Network.AWS.GlobalAccelerator.Types.PortMapping
import Network.AWS.GlobalAccelerator.Types.PortOverride
import Network.AWS.GlobalAccelerator.Types.PortRange
import Network.AWS.GlobalAccelerator.Types.SocketAddress
import Network.AWS.GlobalAccelerator.Types.Tag
import Network.AWS.GlobalAccelerator.UntagResource
import Network.AWS.GlobalAccelerator.UpdateAccelerator
import Network.AWS.GlobalAccelerator.UpdateAcceleratorAttributes
import Network.AWS.GlobalAccelerator.UpdateCustomRoutingAccelerator
import Network.AWS.GlobalAccelerator.UpdateCustomRoutingAcceleratorAttributes
import Network.AWS.GlobalAccelerator.UpdateCustomRoutingListener
import Network.AWS.GlobalAccelerator.UpdateEndpointGroup
import Network.AWS.GlobalAccelerator.UpdateListener
import Network.AWS.GlobalAccelerator.WithdrawByoipCidr
