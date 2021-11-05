{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GlobalAccelerator.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Lens
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
import Amazonka.GlobalAccelerator.Types.Accelerator
import Amazonka.GlobalAccelerator.Types.AcceleratorAttributes
import Amazonka.GlobalAccelerator.Types.ByoipCidr
import Amazonka.GlobalAccelerator.Types.ByoipCidrEvent
import Amazonka.GlobalAccelerator.Types.CidrAuthorizationContext
import Amazonka.GlobalAccelerator.Types.CustomRoutingAccelerator
import Amazonka.GlobalAccelerator.Types.CustomRoutingAcceleratorAttributes
import Amazonka.GlobalAccelerator.Types.CustomRoutingDestinationConfiguration
import Amazonka.GlobalAccelerator.Types.CustomRoutingDestinationDescription
import Amazonka.GlobalAccelerator.Types.CustomRoutingEndpointConfiguration
import Amazonka.GlobalAccelerator.Types.CustomRoutingEndpointDescription
import Amazonka.GlobalAccelerator.Types.CustomRoutingEndpointGroup
import Amazonka.GlobalAccelerator.Types.CustomRoutingListener
import Amazonka.GlobalAccelerator.Types.DestinationPortMapping
import Amazonka.GlobalAccelerator.Types.EndpointConfiguration
import Amazonka.GlobalAccelerator.Types.EndpointDescription
import Amazonka.GlobalAccelerator.Types.EndpointGroup
import Amazonka.GlobalAccelerator.Types.IpSet
import Amazonka.GlobalAccelerator.Types.Listener
import Amazonka.GlobalAccelerator.Types.PortMapping
import Amazonka.GlobalAccelerator.Types.PortOverride
import Amazonka.GlobalAccelerator.Types.PortRange
import Amazonka.GlobalAccelerator.Types.SocketAddress
import Amazonka.GlobalAccelerator.Types.Tag
import Amazonka.GlobalAccelerator.UntagResource
import Amazonka.GlobalAccelerator.UpdateAccelerator
import Amazonka.GlobalAccelerator.UpdateAcceleratorAttributes
import Amazonka.GlobalAccelerator.UpdateCustomRoutingAccelerator
import Amazonka.GlobalAccelerator.UpdateCustomRoutingAcceleratorAttributes
import Amazonka.GlobalAccelerator.UpdateCustomRoutingListener
import Amazonka.GlobalAccelerator.UpdateEndpointGroup
import Amazonka.GlobalAccelerator.UpdateListener
import Amazonka.GlobalAccelerator.WithdrawByoipCidr
