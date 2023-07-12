{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaConnect.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    addFlowOutputsResponse_flowArn,
    addFlowOutputsResponse_outputs,
    addFlowOutputsResponse_httpStatus,

    -- ** AddFlowSources
    addFlowSources_flowArn,
    addFlowSources_sources,
    addFlowSourcesResponse_flowArn,
    addFlowSourcesResponse_sources,
    addFlowSourcesResponse_httpStatus,

    -- ** AddFlowVpcInterfaces
    addFlowVpcInterfaces_flowArn,
    addFlowVpcInterfaces_vpcInterfaces,
    addFlowVpcInterfacesResponse_flowArn,
    addFlowVpcInterfacesResponse_vpcInterfaces,
    addFlowVpcInterfacesResponse_httpStatus,

    -- ** CreateFlow
    createFlow_availabilityZone,
    createFlow_entitlements,
    createFlow_maintenance,
    createFlow_mediaStreams,
    createFlow_outputs,
    createFlow_source,
    createFlow_sourceFailoverConfig,
    createFlow_sources,
    createFlow_vpcInterfaces,
    createFlow_name,
    createFlowResponse_flow,
    createFlowResponse_httpStatus,

    -- ** DeleteFlow
    deleteFlow_flowArn,
    deleteFlowResponse_flowArn,
    deleteFlowResponse_status,
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
    listEntitlements_maxResults,
    listEntitlements_nextToken,
    listEntitlementsResponse_entitlements,
    listEntitlementsResponse_nextToken,
    listEntitlementsResponse_httpStatus,

    -- ** ListFlows
    listFlows_maxResults,
    listFlows_nextToken,
    listFlowsResponse_flows,
    listFlowsResponse_nextToken,
    listFlowsResponse_httpStatus,

    -- ** ListOfferings
    listOfferings_maxResults,
    listOfferings_nextToken,
    listOfferingsResponse_nextToken,
    listOfferingsResponse_offerings,
    listOfferingsResponse_httpStatus,

    -- ** ListReservations
    listReservations_maxResults,
    listReservations_nextToken,
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
    removeFlowMediaStreamResponse_flowArn,
    removeFlowMediaStreamResponse_mediaStreamName,
    removeFlowMediaStreamResponse_httpStatus,

    -- ** RemoveFlowOutput
    removeFlowOutput_flowArn,
    removeFlowOutput_outputArn,
    removeFlowOutputResponse_flowArn,
    removeFlowOutputResponse_outputArn,
    removeFlowOutputResponse_httpStatus,

    -- ** RemoveFlowSource
    removeFlowSource_flowArn,
    removeFlowSource_sourceArn,
    removeFlowSourceResponse_flowArn,
    removeFlowSourceResponse_sourceArn,
    removeFlowSourceResponse_httpStatus,

    -- ** RemoveFlowVpcInterface
    removeFlowVpcInterface_flowArn,
    removeFlowVpcInterface_vpcInterfaceName,
    removeFlowVpcInterfaceResponse_flowArn,
    removeFlowVpcInterfaceResponse_nonDeletedNetworkInterfaceIds,
    removeFlowVpcInterfaceResponse_vpcInterfaceName,
    removeFlowVpcInterfaceResponse_httpStatus,

    -- ** RevokeFlowEntitlement
    revokeFlowEntitlement_flowArn,
    revokeFlowEntitlement_entitlementArn,
    revokeFlowEntitlementResponse_entitlementArn,
    revokeFlowEntitlementResponse_flowArn,
    revokeFlowEntitlementResponse_httpStatus,

    -- ** StartFlow
    startFlow_flowArn,
    startFlowResponse_flowArn,
    startFlowResponse_status,
    startFlowResponse_httpStatus,

    -- ** StopFlow
    stopFlow_flowArn,
    stopFlowResponse_flowArn,
    stopFlowResponse_status,
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
    updateFlowEntitlement_description,
    updateFlowEntitlement_encryption,
    updateFlowEntitlement_entitlementStatus,
    updateFlowEntitlement_subscribers,
    updateFlowEntitlement_flowArn,
    updateFlowEntitlement_entitlementArn,
    updateFlowEntitlementResponse_entitlement,
    updateFlowEntitlementResponse_flowArn,
    updateFlowEntitlementResponse_httpStatus,

    -- ** UpdateFlowMediaStream
    updateFlowMediaStream_attributes,
    updateFlowMediaStream_clockRate,
    updateFlowMediaStream_description,
    updateFlowMediaStream_mediaStreamType,
    updateFlowMediaStream_videoFormat,
    updateFlowMediaStream_flowArn,
    updateFlowMediaStream_mediaStreamName,
    updateFlowMediaStreamResponse_flowArn,
    updateFlowMediaStreamResponse_mediaStream,
    updateFlowMediaStreamResponse_httpStatus,

    -- ** UpdateFlowOutput
    updateFlowOutput_cidrAllowList,
    updateFlowOutput_description,
    updateFlowOutput_destination,
    updateFlowOutput_encryption,
    updateFlowOutput_maxLatency,
    updateFlowOutput_mediaStreamOutputConfigurations,
    updateFlowOutput_minLatency,
    updateFlowOutput_port,
    updateFlowOutput_protocol,
    updateFlowOutput_remoteId,
    updateFlowOutput_senderControlPort,
    updateFlowOutput_senderIpAddress,
    updateFlowOutput_smoothingLatency,
    updateFlowOutput_streamId,
    updateFlowOutput_vpcInterfaceAttachment,
    updateFlowOutput_flowArn,
    updateFlowOutput_outputArn,
    updateFlowOutputResponse_flowArn,
    updateFlowOutputResponse_output,
    updateFlowOutputResponse_httpStatus,

    -- ** UpdateFlowSource
    updateFlowSource_decryption,
    updateFlowSource_description,
    updateFlowSource_entitlementArn,
    updateFlowSource_ingestPort,
    updateFlowSource_maxBitrate,
    updateFlowSource_maxLatency,
    updateFlowSource_maxSyncBuffer,
    updateFlowSource_mediaStreamSourceConfigurations,
    updateFlowSource_minLatency,
    updateFlowSource_protocol,
    updateFlowSource_senderControlPort,
    updateFlowSource_senderIpAddress,
    updateFlowSource_sourceListenerAddress,
    updateFlowSource_sourceListenerPort,
    updateFlowSource_streamId,
    updateFlowSource_vpcInterfaceName,
    updateFlowSource_whitelistCidr,
    updateFlowSource_flowArn,
    updateFlowSource_sourceArn,
    updateFlowSourceResponse_flowArn,
    updateFlowSourceResponse_source,
    updateFlowSourceResponse_httpStatus,

    -- * Types

    -- ** AddMaintenance
    addMaintenance_maintenanceDay,
    addMaintenance_maintenanceStartHour,

    -- ** AddMediaStreamRequest
    addMediaStreamRequest_attributes,
    addMediaStreamRequest_clockRate,
    addMediaStreamRequest_description,
    addMediaStreamRequest_videoFormat,
    addMediaStreamRequest_mediaStreamType,
    addMediaStreamRequest_mediaStreamId,
    addMediaStreamRequest_mediaStreamName,

    -- ** AddOutputRequest
    addOutputRequest_cidrAllowList,
    addOutputRequest_description,
    addOutputRequest_destination,
    addOutputRequest_encryption,
    addOutputRequest_maxLatency,
    addOutputRequest_mediaStreamOutputConfigurations,
    addOutputRequest_minLatency,
    addOutputRequest_name,
    addOutputRequest_port,
    addOutputRequest_remoteId,
    addOutputRequest_senderControlPort,
    addOutputRequest_smoothingLatency,
    addOutputRequest_streamId,
    addOutputRequest_vpcInterfaceAttachment,
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
    encryption_algorithm,
    encryption_constantInitializationVector,
    encryption_deviceId,
    encryption_keyType,
    encryption_region,
    encryption_resourceId,
    encryption_secretArn,
    encryption_url,
    encryption_roleArn,

    -- ** Entitlement
    entitlement_dataTransferSubscriberFeePercent,
    entitlement_description,
    entitlement_encryption,
    entitlement_entitlementStatus,
    entitlement_entitlementArn,
    entitlement_subscribers,
    entitlement_name,

    -- ** FailoverConfig
    failoverConfig_failoverMode,
    failoverConfig_recoveryWindow,
    failoverConfig_sourcePriority,
    failoverConfig_state,

    -- ** Flow
    flow_description,
    flow_egressIp,
    flow_maintenance,
    flow_mediaStreams,
    flow_sourceFailoverConfig,
    flow_sources,
    flow_vpcInterfaces,
    flow_status,
    flow_entitlements,
    flow_outputs,
    flow_availabilityZone,
    flow_flowArn,
    flow_source,
    flow_name,

    -- ** Fmtp
    fmtp_channelOrder,
    fmtp_colorimetry,
    fmtp_exactFramerate,
    fmtp_par,
    fmtp_range,
    fmtp_scanMode,
    fmtp_tcs,

    -- ** FmtpRequest
    fmtpRequest_channelOrder,
    fmtpRequest_colorimetry,
    fmtpRequest_exactFramerate,
    fmtpRequest_par,
    fmtpRequest_range,
    fmtpRequest_scanMode,
    fmtpRequest_tcs,

    -- ** GrantEntitlementRequest
    grantEntitlementRequest_dataTransferSubscriberFeePercent,
    grantEntitlementRequest_description,
    grantEntitlementRequest_encryption,
    grantEntitlementRequest_entitlementStatus,
    grantEntitlementRequest_name,
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
    maintenance_maintenanceDay,
    maintenance_maintenanceDeadline,
    maintenance_maintenanceScheduledDate,
    maintenance_maintenanceStartHour,

    -- ** MediaStream
    mediaStream_attributes,
    mediaStream_clockRate,
    mediaStream_description,
    mediaStream_videoFormat,
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
    mediaStreamOutputConfiguration_destinationConfigurations,
    mediaStreamOutputConfiguration_encodingParameters,
    mediaStreamOutputConfiguration_mediaStreamName,
    mediaStreamOutputConfiguration_encodingName,

    -- ** MediaStreamOutputConfigurationRequest
    mediaStreamOutputConfigurationRequest_destinationConfigurations,
    mediaStreamOutputConfigurationRequest_encodingParameters,
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
    output_dataTransferSubscriberFeePercent,
    output_description,
    output_destination,
    output_encryption,
    output_entitlementArn,
    output_listenerAddress,
    output_mediaLiveInputArn,
    output_mediaStreamOutputConfigurations,
    output_port,
    output_transport,
    output_vpcInterfaceAttachment,
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
    setSourceRequest_decryption,
    setSourceRequest_description,
    setSourceRequest_entitlementArn,
    setSourceRequest_ingestPort,
    setSourceRequest_maxBitrate,
    setSourceRequest_maxLatency,
    setSourceRequest_maxSyncBuffer,
    setSourceRequest_mediaStreamSourceConfigurations,
    setSourceRequest_minLatency,
    setSourceRequest_name,
    setSourceRequest_protocol,
    setSourceRequest_senderControlPort,
    setSourceRequest_senderIpAddress,
    setSourceRequest_sourceListenerAddress,
    setSourceRequest_sourceListenerPort,
    setSourceRequest_streamId,
    setSourceRequest_vpcInterfaceName,
    setSourceRequest_whitelistCidr,

    -- ** Source
    source_dataTransferSubscriberFeePercent,
    source_decryption,
    source_description,
    source_entitlementArn,
    source_ingestIp,
    source_ingestPort,
    source_mediaStreamSourceConfigurations,
    source_senderControlPort,
    source_senderIpAddress,
    source_transport,
    source_vpcInterfaceName,
    source_whitelistCidr,
    source_sourceArn,
    source_name,

    -- ** SourcePriority
    sourcePriority_primarySource,

    -- ** Transport
    transport_cidrAllowList,
    transport_maxBitrate,
    transport_maxLatency,
    transport_maxSyncBuffer,
    transport_minLatency,
    transport_remoteId,
    transport_senderControlPort,
    transport_senderIpAddress,
    transport_smoothingLatency,
    transport_sourceListenerAddress,
    transport_sourceListenerPort,
    transport_streamId,
    transport_protocol,

    -- ** UpdateEncryption
    updateEncryption_algorithm,
    updateEncryption_constantInitializationVector,
    updateEncryption_deviceId,
    updateEncryption_keyType,
    updateEncryption_region,
    updateEncryption_resourceId,
    updateEncryption_roleArn,
    updateEncryption_secretArn,
    updateEncryption_url,

    -- ** UpdateFailoverConfig
    updateFailoverConfig_failoverMode,
    updateFailoverConfig_recoveryWindow,
    updateFailoverConfig_sourcePriority,
    updateFailoverConfig_state,

    -- ** UpdateMaintenance
    updateMaintenance_maintenanceDay,
    updateMaintenance_maintenanceScheduledDate,
    updateMaintenance_maintenanceStartHour,

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
