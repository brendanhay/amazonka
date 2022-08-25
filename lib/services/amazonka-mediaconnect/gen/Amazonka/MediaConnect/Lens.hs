{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaConnect.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Lens
  ( -- * Operations

    -- ** AddFlowMediaStreams
    addFlowMediaStreams_flowArn,
    addFlowMediaStreams_mediaStreams,
    addFlowMediaStreamsResponse_flowArn,
    addFlowMediaStreamsResponse_mediaStreams,
    addFlowMediaStreamsResponse_httpStatus,

    -- ** AddFlowOutputs
    addFlowOutputs_flowArn,
    addFlowOutputs_outputs,
    addFlowOutputsResponse_outputs,
    addFlowOutputsResponse_flowArn,
    addFlowOutputsResponse_httpStatus,

    -- ** AddFlowSources
    addFlowSources_flowArn,
    addFlowSources_sources,
    addFlowSourcesResponse_sources,
    addFlowSourcesResponse_flowArn,
    addFlowSourcesResponse_httpStatus,

    -- ** AddFlowVpcInterfaces
    addFlowVpcInterfaces_flowArn,
    addFlowVpcInterfaces_vpcInterfaces,
    addFlowVpcInterfacesResponse_vpcInterfaces,
    addFlowVpcInterfacesResponse_flowArn,
    addFlowVpcInterfacesResponse_httpStatus,

    -- ** CreateFlow
    createFlow_sources,
    createFlow_maintenance,
    createFlow_vpcInterfaces,
    createFlow_entitlements,
    createFlow_availabilityZone,
    createFlow_source,
    createFlow_outputs,
    createFlow_sourceFailoverConfig,
    createFlow_mediaStreams,
    createFlow_name,
    createFlowResponse_flow,
    createFlowResponse_httpStatus,

    -- ** DeleteFlow
    deleteFlow_flowArn,
    deleteFlowResponse_status,
    deleteFlowResponse_flowArn,
    deleteFlowResponse_httpStatus,

    -- ** DescribeFlow
    describeFlow_flowArn,
    describeFlowResponse_flow,
    describeFlowResponse_messages,
    describeFlowResponse_httpStatus,

    -- ** DescribeOffering
    describeOffering_offeringArn,
    describeOfferingResponse_offering,
    describeOfferingResponse_httpStatus,

    -- ** DescribeReservation
    describeReservation_reservationArn,
    describeReservationResponse_reservation,
    describeReservationResponse_httpStatus,

    -- ** GrantFlowEntitlements
    grantFlowEntitlements_flowArn,
    grantFlowEntitlements_entitlements,
    grantFlowEntitlementsResponse_entitlements,
    grantFlowEntitlementsResponse_flowArn,
    grantFlowEntitlementsResponse_httpStatus,

    -- ** ListEntitlements
    listEntitlements_nextToken,
    listEntitlements_maxResults,
    listEntitlementsResponse_nextToken,
    listEntitlementsResponse_entitlements,
    listEntitlementsResponse_httpStatus,

    -- ** ListFlows
    listFlows_nextToken,
    listFlows_maxResults,
    listFlowsResponse_nextToken,
    listFlowsResponse_flows,
    listFlowsResponse_httpStatus,

    -- ** ListOfferings
    listOfferings_nextToken,
    listOfferings_maxResults,
    listOfferingsResponse_nextToken,
    listOfferingsResponse_offerings,
    listOfferingsResponse_httpStatus,

    -- ** ListReservations
    listReservations_nextToken,
    listReservations_maxResults,
    listReservationsResponse_nextToken,
    listReservationsResponse_reservations,
    listReservationsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PurchaseOffering
    purchaseOffering_offeringArn,
    purchaseOffering_start,
    purchaseOffering_reservationName,
    purchaseOfferingResponse_reservation,
    purchaseOfferingResponse_httpStatus,

    -- ** RemoveFlowMediaStream
    removeFlowMediaStream_flowArn,
    removeFlowMediaStream_mediaStreamName,
    removeFlowMediaStreamResponse_mediaStreamName,
    removeFlowMediaStreamResponse_flowArn,
    removeFlowMediaStreamResponse_httpStatus,

    -- ** RemoveFlowOutput
    removeFlowOutput_flowArn,
    removeFlowOutput_outputArn,
    removeFlowOutputResponse_outputArn,
    removeFlowOutputResponse_flowArn,
    removeFlowOutputResponse_httpStatus,

    -- ** RemoveFlowSource
    removeFlowSource_flowArn,
    removeFlowSource_sourceArn,
    removeFlowSourceResponse_sourceArn,
    removeFlowSourceResponse_flowArn,
    removeFlowSourceResponse_httpStatus,

    -- ** RemoveFlowVpcInterface
    removeFlowVpcInterface_flowArn,
    removeFlowVpcInterface_vpcInterfaceName,
    removeFlowVpcInterfaceResponse_vpcInterfaceName,
    removeFlowVpcInterfaceResponse_nonDeletedNetworkInterfaceIds,
    removeFlowVpcInterfaceResponse_flowArn,
    removeFlowVpcInterfaceResponse_httpStatus,

    -- ** RevokeFlowEntitlement
    revokeFlowEntitlement_flowArn,
    revokeFlowEntitlement_entitlementArn,
    revokeFlowEntitlementResponse_entitlementArn,
    revokeFlowEntitlementResponse_flowArn,
    revokeFlowEntitlementResponse_httpStatus,

    -- ** StartFlow
    startFlow_flowArn,
    startFlowResponse_status,
    startFlowResponse_flowArn,
    startFlowResponse_httpStatus,

    -- ** StopFlow
    stopFlow_flowArn,
    stopFlowResponse_status,
    stopFlowResponse_flowArn,
    stopFlowResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** UntagResource
    untagResource_tagKeys,
    untagResource_resourceArn,

    -- ** UpdateFlow
    updateFlow_maintenance,
    updateFlow_sourceFailoverConfig,
    updateFlow_flowArn,
    updateFlowResponse_flow,
    updateFlowResponse_httpStatus,

    -- ** UpdateFlowEntitlement
    updateFlowEntitlement_entitlementStatus,
    updateFlowEntitlement_description,
    updateFlowEntitlement_subscribers,
    updateFlowEntitlement_encryption,
    updateFlowEntitlement_flowArn,
    updateFlowEntitlement_entitlementArn,
    updateFlowEntitlementResponse_entitlement,
    updateFlowEntitlementResponse_flowArn,
    updateFlowEntitlementResponse_httpStatus,

    -- ** UpdateFlowMediaStream
    updateFlowMediaStream_mediaStreamType,
    updateFlowMediaStream_videoFormat,
    updateFlowMediaStream_description,
    updateFlowMediaStream_clockRate,
    updateFlowMediaStream_attributes,
    updateFlowMediaStream_flowArn,
    updateFlowMediaStream_mediaStreamName,
    updateFlowMediaStreamResponse_mediaStream,
    updateFlowMediaStreamResponse_flowArn,
    updateFlowMediaStreamResponse_httpStatus,

    -- ** UpdateFlowOutput
    updateFlowOutput_destination,
    updateFlowOutput_port,
    updateFlowOutput_maxLatency,
    updateFlowOutput_mediaStreamOutputConfigurations,
    updateFlowOutput_smoothingLatency,
    updateFlowOutput_cidrAllowList,
    updateFlowOutput_streamId,
    updateFlowOutput_remoteId,
    updateFlowOutput_senderIpAddress,
    updateFlowOutput_vpcInterfaceAttachment,
    updateFlowOutput_description,
    updateFlowOutput_senderControlPort,
    updateFlowOutput_encryption,
    updateFlowOutput_protocol,
    updateFlowOutput_minLatency,
    updateFlowOutput_flowArn,
    updateFlowOutput_outputArn,
    updateFlowOutputResponse_output,
    updateFlowOutputResponse_flowArn,
    updateFlowOutputResponse_httpStatus,

    -- ** UpdateFlowSource
    updateFlowSource_maxLatency,
    updateFlowSource_mediaStreamSourceConfigurations,
    updateFlowSource_entitlementArn,
    updateFlowSource_vpcInterfaceName,
    updateFlowSource_maxSyncBuffer,
    updateFlowSource_maxBitrate,
    updateFlowSource_streamId,
    updateFlowSource_senderIpAddress,
    updateFlowSource_decryption,
    updateFlowSource_description,
    updateFlowSource_senderControlPort,
    updateFlowSource_protocol,
    updateFlowSource_ingestPort,
    updateFlowSource_whitelistCidr,
    updateFlowSource_minLatency,
    updateFlowSource_flowArn,
    updateFlowSource_sourceArn,
    updateFlowSourceResponse_source,
    updateFlowSourceResponse_flowArn,
    updateFlowSourceResponse_httpStatus,

    -- * Types

    -- ** AddMaintenance
    addMaintenance_maintenanceDay,
    addMaintenance_maintenanceStartHour,

    -- ** AddMediaStreamRequest
    addMediaStreamRequest_videoFormat,
    addMediaStreamRequest_description,
    addMediaStreamRequest_clockRate,
    addMediaStreamRequest_attributes,
    addMediaStreamRequest_mediaStreamType,
    addMediaStreamRequest_mediaStreamId,
    addMediaStreamRequest_mediaStreamName,

    -- ** AddOutputRequest
    addOutputRequest_destination,
    addOutputRequest_port,
    addOutputRequest_maxLatency,
    addOutputRequest_name,
    addOutputRequest_mediaStreamOutputConfigurations,
    addOutputRequest_smoothingLatency,
    addOutputRequest_cidrAllowList,
    addOutputRequest_streamId,
    addOutputRequest_remoteId,
    addOutputRequest_vpcInterfaceAttachment,
    addOutputRequest_description,
    addOutputRequest_senderControlPort,
    addOutputRequest_encryption,
    addOutputRequest_minLatency,
    addOutputRequest_protocol,

    -- ** DestinationConfiguration
    destinationConfiguration_destinationIp,
    destinationConfiguration_destinationPort,
    destinationConfiguration_interface,
    destinationConfiguration_outboundIp,

    -- ** DestinationConfigurationRequest
    destinationConfigurationRequest_destinationIp,
    destinationConfigurationRequest_destinationPort,
    destinationConfigurationRequest_interface,

    -- ** EncodingParameters
    encodingParameters_encoderProfile,
    encodingParameters_compressionFactor,

    -- ** EncodingParametersRequest
    encodingParametersRequest_encoderProfile,
    encodingParametersRequest_compressionFactor,

    -- ** Encryption
    encryption_resourceId,
    encryption_deviceId,
    encryption_constantInitializationVector,
    encryption_keyType,
    encryption_url,
    encryption_region,
    encryption_secretArn,
    encryption_algorithm,
    encryption_roleArn,

    -- ** Entitlement
    entitlement_entitlementStatus,
    entitlement_description,
    entitlement_encryption,
    entitlement_dataTransferSubscriberFeePercent,
    entitlement_entitlementArn,
    entitlement_subscribers,
    entitlement_name,

    -- ** FailoverConfig
    failoverConfig_recoveryWindow,
    failoverConfig_state,
    failoverConfig_sourcePriority,
    failoverConfig_failoverMode,

    -- ** Flow
    flow_sources,
    flow_maintenance,
    flow_vpcInterfaces,
    flow_egressIp,
    flow_description,
    flow_sourceFailoverConfig,
    flow_mediaStreams,
    flow_status,
    flow_entitlements,
    flow_outputs,
    flow_availabilityZone,
    flow_flowArn,
    flow_source,
    flow_name,

    -- ** Fmtp
    fmtp_exactFramerate,
    fmtp_scanMode,
    fmtp_colorimetry,
    fmtp_par,
    fmtp_range,
    fmtp_tcs,
    fmtp_channelOrder,

    -- ** FmtpRequest
    fmtpRequest_exactFramerate,
    fmtpRequest_scanMode,
    fmtpRequest_colorimetry,
    fmtpRequest_par,
    fmtpRequest_range,
    fmtpRequest_tcs,
    fmtpRequest_channelOrder,

    -- ** GrantEntitlementRequest
    grantEntitlementRequest_name,
    grantEntitlementRequest_entitlementStatus,
    grantEntitlementRequest_description,
    grantEntitlementRequest_encryption,
    grantEntitlementRequest_dataTransferSubscriberFeePercent,
    grantEntitlementRequest_subscribers,

    -- ** InputConfiguration
    inputConfiguration_inputPort,
    inputConfiguration_inputIp,
    inputConfiguration_interface,

    -- ** InputConfigurationRequest
    inputConfigurationRequest_inputPort,
    inputConfigurationRequest_interface,

    -- ** Interface
    interface_name,

    -- ** InterfaceRequest
    interfaceRequest_name,

    -- ** ListedEntitlement
    listedEntitlement_dataTransferSubscriberFeePercent,
    listedEntitlement_entitlementArn,
    listedEntitlement_entitlementName,

    -- ** ListedFlow
    listedFlow_maintenance,
    listedFlow_status,
    listedFlow_description,
    listedFlow_sourceType,
    listedFlow_availabilityZone,
    listedFlow_flowArn,
    listedFlow_name,

    -- ** Maintenance
    maintenance_maintenanceDeadline,
    maintenance_maintenanceScheduledDate,
    maintenance_maintenanceStartHour,
    maintenance_maintenanceDay,

    -- ** MediaStream
    mediaStream_videoFormat,
    mediaStream_description,
    mediaStream_clockRate,
    mediaStream_attributes,
    mediaStream_mediaStreamType,
    mediaStream_mediaStreamId,
    mediaStream_mediaStreamName,
    mediaStream_fmt,

    -- ** MediaStreamAttributes
    mediaStreamAttributes_lang,
    mediaStreamAttributes_fmtp,

    -- ** MediaStreamAttributesRequest
    mediaStreamAttributesRequest_fmtp,
    mediaStreamAttributesRequest_lang,

    -- ** MediaStreamOutputConfiguration
    mediaStreamOutputConfiguration_encodingParameters,
    mediaStreamOutputConfiguration_destinationConfigurations,
    mediaStreamOutputConfiguration_mediaStreamName,
    mediaStreamOutputConfiguration_encodingName,

    -- ** MediaStreamOutputConfigurationRequest
    mediaStreamOutputConfigurationRequest_encodingParameters,
    mediaStreamOutputConfigurationRequest_destinationConfigurations,
    mediaStreamOutputConfigurationRequest_mediaStreamName,
    mediaStreamOutputConfigurationRequest_encodingName,

    -- ** MediaStreamSourceConfiguration
    mediaStreamSourceConfiguration_inputConfigurations,
    mediaStreamSourceConfiguration_mediaStreamName,
    mediaStreamSourceConfiguration_encodingName,

    -- ** MediaStreamSourceConfigurationRequest
    mediaStreamSourceConfigurationRequest_inputConfigurations,
    mediaStreamSourceConfigurationRequest_mediaStreamName,
    mediaStreamSourceConfigurationRequest_encodingName,

    -- ** Messages
    messages_errors,

    -- ** Offering
    offering_currencyCode,
    offering_offeringArn,
    offering_offeringDescription,
    offering_durationUnits,
    offering_duration,
    offering_pricePerUnit,
    offering_resourceSpecification,
    offering_priceUnits,

    -- ** Output
    output_destination,
    output_port,
    output_mediaStreamOutputConfigurations,
    output_entitlementArn,
    output_listenerAddress,
    output_vpcInterfaceAttachment,
    output_description,
    output_mediaLiveInputArn,
    output_transport,
    output_encryption,
    output_dataTransferSubscriberFeePercent,
    output_outputArn,
    output_name,

    -- ** Reservation
    reservation_currencyCode,
    reservation_reservationState,
    reservation_offeringArn,
    reservation_reservationArn,
    reservation_start,
    reservation_offeringDescription,
    reservation_reservationName,
    reservation_end,
    reservation_duration,
    reservation_durationUnits,
    reservation_pricePerUnit,
    reservation_resourceSpecification,
    reservation_priceUnits,

    -- ** ResourceSpecification
    resourceSpecification_reservedBitrate,
    resourceSpecification_resourceType,

    -- ** SetSourceRequest
    setSourceRequest_maxLatency,
    setSourceRequest_name,
    setSourceRequest_mediaStreamSourceConfigurations,
    setSourceRequest_entitlementArn,
    setSourceRequest_vpcInterfaceName,
    setSourceRequest_maxSyncBuffer,
    setSourceRequest_maxBitrate,
    setSourceRequest_streamId,
    setSourceRequest_senderIpAddress,
    setSourceRequest_decryption,
    setSourceRequest_description,
    setSourceRequest_senderControlPort,
    setSourceRequest_protocol,
    setSourceRequest_ingestPort,
    setSourceRequest_whitelistCidr,
    setSourceRequest_minLatency,

    -- ** Source
    source_mediaStreamSourceConfigurations,
    source_ingestIp,
    source_entitlementArn,
    source_vpcInterfaceName,
    source_senderIpAddress,
    source_decryption,
    source_description,
    source_transport,
    source_senderControlPort,
    source_dataTransferSubscriberFeePercent,
    source_ingestPort,
    source_whitelistCidr,
    source_sourceArn,
    source_name,

    -- ** SourcePriority
    sourcePriority_primarySource,

    -- ** Transport
    transport_maxLatency,
    transport_smoothingLatency,
    transport_maxSyncBuffer,
    transport_maxBitrate,
    transport_cidrAllowList,
    transport_streamId,
    transport_remoteId,
    transport_senderIpAddress,
    transport_senderControlPort,
    transport_minLatency,
    transport_protocol,

    -- ** UpdateEncryption
    updateEncryption_resourceId,
    updateEncryption_roleArn,
    updateEncryption_deviceId,
    updateEncryption_constantInitializationVector,
    updateEncryption_keyType,
    updateEncryption_url,
    updateEncryption_region,
    updateEncryption_secretArn,
    updateEncryption_algorithm,

    -- ** UpdateFailoverConfig
    updateFailoverConfig_recoveryWindow,
    updateFailoverConfig_state,
    updateFailoverConfig_sourcePriority,
    updateFailoverConfig_failoverMode,

    -- ** UpdateMaintenance
    updateMaintenance_maintenanceScheduledDate,
    updateMaintenance_maintenanceStartHour,
    updateMaintenance_maintenanceDay,

    -- ** VpcInterface
    vpcInterface_networkInterfaceType,
    vpcInterface_networkInterfaceIds,
    vpcInterface_subnetId,
    vpcInterface_securityGroupIds,
    vpcInterface_roleArn,
    vpcInterface_name,

    -- ** VpcInterfaceAttachment
    vpcInterfaceAttachment_vpcInterfaceName,

    -- ** VpcInterfaceRequest
    vpcInterfaceRequest_networkInterfaceType,
    vpcInterfaceRequest_subnetId,
    vpcInterfaceRequest_securityGroupIds,
    vpcInterfaceRequest_roleArn,
    vpcInterfaceRequest_name,
  )
where

import Amazonka.MediaConnect.AddFlowMediaStreams
import Amazonka.MediaConnect.AddFlowOutputs
import Amazonka.MediaConnect.AddFlowSources
import Amazonka.MediaConnect.AddFlowVpcInterfaces
import Amazonka.MediaConnect.CreateFlow
import Amazonka.MediaConnect.DeleteFlow
import Amazonka.MediaConnect.DescribeFlow
import Amazonka.MediaConnect.DescribeOffering
import Amazonka.MediaConnect.DescribeReservation
import Amazonka.MediaConnect.GrantFlowEntitlements
import Amazonka.MediaConnect.ListEntitlements
import Amazonka.MediaConnect.ListFlows
import Amazonka.MediaConnect.ListOfferings
import Amazonka.MediaConnect.ListReservations
import Amazonka.MediaConnect.ListTagsForResource
import Amazonka.MediaConnect.PurchaseOffering
import Amazonka.MediaConnect.RemoveFlowMediaStream
import Amazonka.MediaConnect.RemoveFlowOutput
import Amazonka.MediaConnect.RemoveFlowSource
import Amazonka.MediaConnect.RemoveFlowVpcInterface
import Amazonka.MediaConnect.RevokeFlowEntitlement
import Amazonka.MediaConnect.StartFlow
import Amazonka.MediaConnect.StopFlow
import Amazonka.MediaConnect.TagResource
import Amazonka.MediaConnect.Types.AddMaintenance
import Amazonka.MediaConnect.Types.AddMediaStreamRequest
import Amazonka.MediaConnect.Types.AddOutputRequest
import Amazonka.MediaConnect.Types.DestinationConfiguration
import Amazonka.MediaConnect.Types.DestinationConfigurationRequest
import Amazonka.MediaConnect.Types.EncodingParameters
import Amazonka.MediaConnect.Types.EncodingParametersRequest
import Amazonka.MediaConnect.Types.Encryption
import Amazonka.MediaConnect.Types.Entitlement
import Amazonka.MediaConnect.Types.FailoverConfig
import Amazonka.MediaConnect.Types.Flow
import Amazonka.MediaConnect.Types.Fmtp
import Amazonka.MediaConnect.Types.FmtpRequest
import Amazonka.MediaConnect.Types.GrantEntitlementRequest
import Amazonka.MediaConnect.Types.InputConfiguration
import Amazonka.MediaConnect.Types.InputConfigurationRequest
import Amazonka.MediaConnect.Types.Interface
import Amazonka.MediaConnect.Types.InterfaceRequest
import Amazonka.MediaConnect.Types.ListedEntitlement
import Amazonka.MediaConnect.Types.ListedFlow
import Amazonka.MediaConnect.Types.Maintenance
import Amazonka.MediaConnect.Types.MediaStream
import Amazonka.MediaConnect.Types.MediaStreamAttributes
import Amazonka.MediaConnect.Types.MediaStreamAttributesRequest
import Amazonka.MediaConnect.Types.MediaStreamOutputConfiguration
import Amazonka.MediaConnect.Types.MediaStreamOutputConfigurationRequest
import Amazonka.MediaConnect.Types.MediaStreamSourceConfiguration
import Amazonka.MediaConnect.Types.MediaStreamSourceConfigurationRequest
import Amazonka.MediaConnect.Types.Messages
import Amazonka.MediaConnect.Types.Offering
import Amazonka.MediaConnect.Types.Output
import Amazonka.MediaConnect.Types.Reservation
import Amazonka.MediaConnect.Types.ResourceSpecification
import Amazonka.MediaConnect.Types.SetSourceRequest
import Amazonka.MediaConnect.Types.Source
import Amazonka.MediaConnect.Types.SourcePriority
import Amazonka.MediaConnect.Types.Transport
import Amazonka.MediaConnect.Types.UpdateEncryption
import Amazonka.MediaConnect.Types.UpdateFailoverConfig
import Amazonka.MediaConnect.Types.UpdateMaintenance
import Amazonka.MediaConnect.Types.VpcInterface
import Amazonka.MediaConnect.Types.VpcInterfaceAttachment
import Amazonka.MediaConnect.Types.VpcInterfaceRequest
import Amazonka.MediaConnect.UntagResource
import Amazonka.MediaConnect.UpdateFlow
import Amazonka.MediaConnect.UpdateFlowEntitlement
import Amazonka.MediaConnect.UpdateFlowMediaStream
import Amazonka.MediaConnect.UpdateFlowOutput
import Amazonka.MediaConnect.UpdateFlowSource
