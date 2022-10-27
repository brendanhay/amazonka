{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GlobalAccelerator.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Lens
  ( -- * Operations

    -- ** AddCustomRoutingEndpoints
    addCustomRoutingEndpoints_endpointConfigurations,
    addCustomRoutingEndpoints_endpointGroupArn,
    addCustomRoutingEndpointsResponse_endpointDescriptions,
    addCustomRoutingEndpointsResponse_endpointGroupArn,
    addCustomRoutingEndpointsResponse_httpStatus,

    -- ** AddEndpoints
    addEndpoints_endpointConfigurations,
    addEndpoints_endpointGroupArn,
    addEndpointsResponse_endpointDescriptions,
    addEndpointsResponse_endpointGroupArn,
    addEndpointsResponse_httpStatus,

    -- ** AdvertiseByoipCidr
    advertiseByoipCidr_cidr,
    advertiseByoipCidrResponse_byoipCidr,
    advertiseByoipCidrResponse_httpStatus,

    -- ** AllowCustomRoutingTraffic
    allowCustomRoutingTraffic_destinationPorts,
    allowCustomRoutingTraffic_allowAllTrafficToEndpoint,
    allowCustomRoutingTraffic_destinationAddresses,
    allowCustomRoutingTraffic_endpointGroupArn,
    allowCustomRoutingTraffic_endpointId,

    -- ** CreateAccelerator
    createAccelerator_tags,
    createAccelerator_enabled,
    createAccelerator_ipAddressType,
    createAccelerator_ipAddresses,
    createAccelerator_name,
    createAccelerator_idempotencyToken,
    createAcceleratorResponse_accelerator,
    createAcceleratorResponse_httpStatus,

    -- ** CreateCustomRoutingAccelerator
    createCustomRoutingAccelerator_tags,
    createCustomRoutingAccelerator_enabled,
    createCustomRoutingAccelerator_ipAddressType,
    createCustomRoutingAccelerator_ipAddresses,
    createCustomRoutingAccelerator_name,
    createCustomRoutingAccelerator_idempotencyToken,
    createCustomRoutingAcceleratorResponse_accelerator,
    createCustomRoutingAcceleratorResponse_httpStatus,

    -- ** CreateCustomRoutingEndpointGroup
    createCustomRoutingEndpointGroup_listenerArn,
    createCustomRoutingEndpointGroup_endpointGroupRegion,
    createCustomRoutingEndpointGroup_destinationConfigurations,
    createCustomRoutingEndpointGroup_idempotencyToken,
    createCustomRoutingEndpointGroupResponse_endpointGroup,
    createCustomRoutingEndpointGroupResponse_httpStatus,

    -- ** CreateCustomRoutingListener
    createCustomRoutingListener_acceleratorArn,
    createCustomRoutingListener_portRanges,
    createCustomRoutingListener_idempotencyToken,
    createCustomRoutingListenerResponse_listener,
    createCustomRoutingListenerResponse_httpStatus,

    -- ** CreateEndpointGroup
    createEndpointGroup_healthCheckProtocol,
    createEndpointGroup_portOverrides,
    createEndpointGroup_healthCheckPath,
    createEndpointGroup_healthCheckIntervalSeconds,
    createEndpointGroup_endpointConfigurations,
    createEndpointGroup_trafficDialPercentage,
    createEndpointGroup_thresholdCount,
    createEndpointGroup_healthCheckPort,
    createEndpointGroup_listenerArn,
    createEndpointGroup_endpointGroupRegion,
    createEndpointGroup_idempotencyToken,
    createEndpointGroupResponse_endpointGroup,
    createEndpointGroupResponse_httpStatus,

    -- ** CreateListener
    createListener_clientAffinity,
    createListener_acceleratorArn,
    createListener_portRanges,
    createListener_protocol,
    createListener_idempotencyToken,
    createListenerResponse_listener,
    createListenerResponse_httpStatus,

    -- ** DeleteAccelerator
    deleteAccelerator_acceleratorArn,

    -- ** DeleteCustomRoutingAccelerator
    deleteCustomRoutingAccelerator_acceleratorArn,

    -- ** DeleteCustomRoutingEndpointGroup
    deleteCustomRoutingEndpointGroup_endpointGroupArn,

    -- ** DeleteCustomRoutingListener
    deleteCustomRoutingListener_listenerArn,

    -- ** DeleteEndpointGroup
    deleteEndpointGroup_endpointGroupArn,

    -- ** DeleteListener
    deleteListener_listenerArn,

    -- ** DenyCustomRoutingTraffic
    denyCustomRoutingTraffic_destinationPorts,
    denyCustomRoutingTraffic_denyAllTrafficToEndpoint,
    denyCustomRoutingTraffic_destinationAddresses,
    denyCustomRoutingTraffic_endpointGroupArn,
    denyCustomRoutingTraffic_endpointId,

    -- ** DeprovisionByoipCidr
    deprovisionByoipCidr_cidr,
    deprovisionByoipCidrResponse_byoipCidr,
    deprovisionByoipCidrResponse_httpStatus,

    -- ** DescribeAccelerator
    describeAccelerator_acceleratorArn,
    describeAcceleratorResponse_accelerator,
    describeAcceleratorResponse_httpStatus,

    -- ** DescribeAcceleratorAttributes
    describeAcceleratorAttributes_acceleratorArn,
    describeAcceleratorAttributesResponse_acceleratorAttributes,
    describeAcceleratorAttributesResponse_httpStatus,

    -- ** DescribeCustomRoutingAccelerator
    describeCustomRoutingAccelerator_acceleratorArn,
    describeCustomRoutingAcceleratorResponse_accelerator,
    describeCustomRoutingAcceleratorResponse_httpStatus,

    -- ** DescribeCustomRoutingAcceleratorAttributes
    describeCustomRoutingAcceleratorAttributes_acceleratorArn,
    describeCustomRoutingAcceleratorAttributesResponse_acceleratorAttributes,
    describeCustomRoutingAcceleratorAttributesResponse_httpStatus,

    -- ** DescribeCustomRoutingEndpointGroup
    describeCustomRoutingEndpointGroup_endpointGroupArn,
    describeCustomRoutingEndpointGroupResponse_endpointGroup,
    describeCustomRoutingEndpointGroupResponse_httpStatus,

    -- ** DescribeCustomRoutingListener
    describeCustomRoutingListener_listenerArn,
    describeCustomRoutingListenerResponse_listener,
    describeCustomRoutingListenerResponse_httpStatus,

    -- ** DescribeEndpointGroup
    describeEndpointGroup_endpointGroupArn,
    describeEndpointGroupResponse_endpointGroup,
    describeEndpointGroupResponse_httpStatus,

    -- ** DescribeListener
    describeListener_listenerArn,
    describeListenerResponse_listener,
    describeListenerResponse_httpStatus,

    -- ** ListAccelerators
    listAccelerators_nextToken,
    listAccelerators_maxResults,
    listAcceleratorsResponse_nextToken,
    listAcceleratorsResponse_accelerators,
    listAcceleratorsResponse_httpStatus,

    -- ** ListByoipCidrs
    listByoipCidrs_nextToken,
    listByoipCidrs_maxResults,
    listByoipCidrsResponse_nextToken,
    listByoipCidrsResponse_byoipCidrs,
    listByoipCidrsResponse_httpStatus,

    -- ** ListCustomRoutingAccelerators
    listCustomRoutingAccelerators_nextToken,
    listCustomRoutingAccelerators_maxResults,
    listCustomRoutingAcceleratorsResponse_nextToken,
    listCustomRoutingAcceleratorsResponse_accelerators,
    listCustomRoutingAcceleratorsResponse_httpStatus,

    -- ** ListCustomRoutingEndpointGroups
    listCustomRoutingEndpointGroups_nextToken,
    listCustomRoutingEndpointGroups_maxResults,
    listCustomRoutingEndpointGroups_listenerArn,
    listCustomRoutingEndpointGroupsResponse_nextToken,
    listCustomRoutingEndpointGroupsResponse_endpointGroups,
    listCustomRoutingEndpointGroupsResponse_httpStatus,

    -- ** ListCustomRoutingListeners
    listCustomRoutingListeners_nextToken,
    listCustomRoutingListeners_maxResults,
    listCustomRoutingListeners_acceleratorArn,
    listCustomRoutingListenersResponse_nextToken,
    listCustomRoutingListenersResponse_listeners,
    listCustomRoutingListenersResponse_httpStatus,

    -- ** ListCustomRoutingPortMappings
    listCustomRoutingPortMappings_nextToken,
    listCustomRoutingPortMappings_maxResults,
    listCustomRoutingPortMappings_endpointGroupArn,
    listCustomRoutingPortMappings_acceleratorArn,
    listCustomRoutingPortMappingsResponse_nextToken,
    listCustomRoutingPortMappingsResponse_portMappings,
    listCustomRoutingPortMappingsResponse_httpStatus,

    -- ** ListCustomRoutingPortMappingsByDestination
    listCustomRoutingPortMappingsByDestination_nextToken,
    listCustomRoutingPortMappingsByDestination_maxResults,
    listCustomRoutingPortMappingsByDestination_endpointId,
    listCustomRoutingPortMappingsByDestination_destinationAddress,
    listCustomRoutingPortMappingsByDestinationResponse_nextToken,
    listCustomRoutingPortMappingsByDestinationResponse_destinationPortMappings,
    listCustomRoutingPortMappingsByDestinationResponse_httpStatus,

    -- ** ListEndpointGroups
    listEndpointGroups_nextToken,
    listEndpointGroups_maxResults,
    listEndpointGroups_listenerArn,
    listEndpointGroupsResponse_nextToken,
    listEndpointGroupsResponse_endpointGroups,
    listEndpointGroupsResponse_httpStatus,

    -- ** ListListeners
    listListeners_nextToken,
    listListeners_maxResults,
    listListeners_acceleratorArn,
    listListenersResponse_nextToken,
    listListenersResponse_listeners,
    listListenersResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ProvisionByoipCidr
    provisionByoipCidr_cidr,
    provisionByoipCidr_cidrAuthorizationContext,
    provisionByoipCidrResponse_byoipCidr,
    provisionByoipCidrResponse_httpStatus,

    -- ** RemoveCustomRoutingEndpoints
    removeCustomRoutingEndpoints_endpointIds,
    removeCustomRoutingEndpoints_endpointGroupArn,

    -- ** RemoveEndpoints
    removeEndpoints_endpointIdentifiers,
    removeEndpoints_endpointGroupArn,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateAccelerator
    updateAccelerator_name,
    updateAccelerator_enabled,
    updateAccelerator_ipAddressType,
    updateAccelerator_acceleratorArn,
    updateAcceleratorResponse_accelerator,
    updateAcceleratorResponse_httpStatus,

    -- ** UpdateAcceleratorAttributes
    updateAcceleratorAttributes_flowLogsEnabled,
    updateAcceleratorAttributes_flowLogsS3Bucket,
    updateAcceleratorAttributes_flowLogsS3Prefix,
    updateAcceleratorAttributes_acceleratorArn,
    updateAcceleratorAttributesResponse_acceleratorAttributes,
    updateAcceleratorAttributesResponse_httpStatus,

    -- ** UpdateCustomRoutingAccelerator
    updateCustomRoutingAccelerator_name,
    updateCustomRoutingAccelerator_enabled,
    updateCustomRoutingAccelerator_ipAddressType,
    updateCustomRoutingAccelerator_acceleratorArn,
    updateCustomRoutingAcceleratorResponse_accelerator,
    updateCustomRoutingAcceleratorResponse_httpStatus,

    -- ** UpdateCustomRoutingAcceleratorAttributes
    updateCustomRoutingAcceleratorAttributes_flowLogsEnabled,
    updateCustomRoutingAcceleratorAttributes_flowLogsS3Bucket,
    updateCustomRoutingAcceleratorAttributes_flowLogsS3Prefix,
    updateCustomRoutingAcceleratorAttributes_acceleratorArn,
    updateCustomRoutingAcceleratorAttributesResponse_acceleratorAttributes,
    updateCustomRoutingAcceleratorAttributesResponse_httpStatus,

    -- ** UpdateCustomRoutingListener
    updateCustomRoutingListener_listenerArn,
    updateCustomRoutingListener_portRanges,
    updateCustomRoutingListenerResponse_listener,
    updateCustomRoutingListenerResponse_httpStatus,

    -- ** UpdateEndpointGroup
    updateEndpointGroup_healthCheckProtocol,
    updateEndpointGroup_portOverrides,
    updateEndpointGroup_healthCheckPath,
    updateEndpointGroup_healthCheckIntervalSeconds,
    updateEndpointGroup_endpointConfigurations,
    updateEndpointGroup_trafficDialPercentage,
    updateEndpointGroup_thresholdCount,
    updateEndpointGroup_healthCheckPort,
    updateEndpointGroup_endpointGroupArn,
    updateEndpointGroupResponse_endpointGroup,
    updateEndpointGroupResponse_httpStatus,

    -- ** UpdateListener
    updateListener_clientAffinity,
    updateListener_protocol,
    updateListener_portRanges,
    updateListener_listenerArn,
    updateListenerResponse_listener,
    updateListenerResponse_httpStatus,

    -- ** WithdrawByoipCidr
    withdrawByoipCidr_cidr,
    withdrawByoipCidrResponse_byoipCidr,
    withdrawByoipCidrResponse_httpStatus,

    -- * Types

    -- ** Accelerator
    accelerator_ipSets,
    accelerator_name,
    accelerator_acceleratorArn,
    accelerator_createdTime,
    accelerator_status,
    accelerator_enabled,
    accelerator_lastModifiedTime,
    accelerator_dualStackDnsName,
    accelerator_events,
    accelerator_dnsName,
    accelerator_ipAddressType,

    -- ** AcceleratorAttributes
    acceleratorAttributes_flowLogsEnabled,
    acceleratorAttributes_flowLogsS3Bucket,
    acceleratorAttributes_flowLogsS3Prefix,

    -- ** AcceleratorEvent
    acceleratorEvent_message,
    acceleratorEvent_timestamp,

    -- ** ByoipCidr
    byoipCidr_cidr,
    byoipCidr_state,
    byoipCidr_events,

    -- ** ByoipCidrEvent
    byoipCidrEvent_message,
    byoipCidrEvent_timestamp,

    -- ** CidrAuthorizationContext
    cidrAuthorizationContext_message,
    cidrAuthorizationContext_signature,

    -- ** CustomRoutingAccelerator
    customRoutingAccelerator_ipSets,
    customRoutingAccelerator_name,
    customRoutingAccelerator_acceleratorArn,
    customRoutingAccelerator_createdTime,
    customRoutingAccelerator_status,
    customRoutingAccelerator_enabled,
    customRoutingAccelerator_lastModifiedTime,
    customRoutingAccelerator_dnsName,
    customRoutingAccelerator_ipAddressType,

    -- ** CustomRoutingAcceleratorAttributes
    customRoutingAcceleratorAttributes_flowLogsEnabled,
    customRoutingAcceleratorAttributes_flowLogsS3Bucket,
    customRoutingAcceleratorAttributes_flowLogsS3Prefix,

    -- ** CustomRoutingDestinationConfiguration
    customRoutingDestinationConfiguration_fromPort,
    customRoutingDestinationConfiguration_toPort,
    customRoutingDestinationConfiguration_protocols,

    -- ** CustomRoutingDestinationDescription
    customRoutingDestinationDescription_toPort,
    customRoutingDestinationDescription_protocols,
    customRoutingDestinationDescription_fromPort,

    -- ** CustomRoutingEndpointConfiguration
    customRoutingEndpointConfiguration_endpointId,

    -- ** CustomRoutingEndpointDescription
    customRoutingEndpointDescription_endpointId,

    -- ** CustomRoutingEndpointGroup
    customRoutingEndpointGroup_endpointGroupRegion,
    customRoutingEndpointGroup_endpointDescriptions,
    customRoutingEndpointGroup_destinationDescriptions,
    customRoutingEndpointGroup_endpointGroupArn,

    -- ** CustomRoutingListener
    customRoutingListener_listenerArn,
    customRoutingListener_portRanges,

    -- ** DestinationPortMapping
    destinationPortMapping_endpointGroupRegion,
    destinationPortMapping_acceleratorArn,
    destinationPortMapping_endpointId,
    destinationPortMapping_destinationTrafficState,
    destinationPortMapping_ipAddressType,
    destinationPortMapping_endpointGroupArn,
    destinationPortMapping_destinationSocketAddress,
    destinationPortMapping_acceleratorSocketAddresses,

    -- ** EndpointConfiguration
    endpointConfiguration_endpointId,
    endpointConfiguration_weight,
    endpointConfiguration_clientIPPreservationEnabled,

    -- ** EndpointDescription
    endpointDescription_endpointId,
    endpointDescription_healthReason,
    endpointDescription_weight,
    endpointDescription_clientIPPreservationEnabled,
    endpointDescription_healthState,

    -- ** EndpointGroup
    endpointGroup_healthCheckProtocol,
    endpointGroup_endpointGroupRegion,
    endpointGroup_portOverrides,
    endpointGroup_healthCheckPath,
    endpointGroup_healthCheckIntervalSeconds,
    endpointGroup_endpointDescriptions,
    endpointGroup_trafficDialPercentage,
    endpointGroup_thresholdCount,
    endpointGroup_healthCheckPort,
    endpointGroup_endpointGroupArn,

    -- ** EndpointIdentifier
    endpointIdentifier_clientIPPreservationEnabled,
    endpointIdentifier_endpointId,

    -- ** IpSet
    ipSet_ipFamily,
    ipSet_ipAddressFamily,
    ipSet_ipAddresses,

    -- ** Listener
    listener_listenerArn,
    listener_clientAffinity,
    listener_protocol,
    listener_portRanges,

    -- ** PortMapping
    portMapping_acceleratorPort,
    portMapping_endpointId,
    portMapping_protocols,
    portMapping_destinationTrafficState,
    portMapping_endpointGroupArn,
    portMapping_destinationSocketAddress,

    -- ** PortOverride
    portOverride_endpointPort,
    portOverride_listenerPort,

    -- ** PortRange
    portRange_toPort,
    portRange_fromPort,

    -- ** SocketAddress
    socketAddress_port,
    socketAddress_ipAddress,

    -- ** Tag
    tag_key,
    tag_value,
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
import Amazonka.GlobalAccelerator.Types.Accelerator
import Amazonka.GlobalAccelerator.Types.AcceleratorAttributes
import Amazonka.GlobalAccelerator.Types.AcceleratorEvent
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
import Amazonka.GlobalAccelerator.Types.EndpointIdentifier
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
