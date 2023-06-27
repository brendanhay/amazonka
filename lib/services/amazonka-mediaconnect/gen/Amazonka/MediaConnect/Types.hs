{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaConnect.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AddFlowOutputs420Exception,
    _BadRequestException,
    _ConflictException,
    _CreateBridge420Exception,
    _CreateFlow420Exception,
    _CreateGateway420Exception,
    _ForbiddenException,
    _GrantFlowEntitlements420Exception,
    _InternalServerErrorException,
    _NotFoundException,
    _ServiceUnavailableException,
    _TooManyRequestsException,

    -- * Algorithm
    Algorithm (..),

    -- * BridgePlacement
    BridgePlacement (..),

    -- * BridgeState
    BridgeState (..),

    -- * Colorimetry
    Colorimetry (..),

    -- * ConnectionStatus
    ConnectionStatus (..),

    -- * DesiredState
    DesiredState (..),

    -- * DurationUnits
    DurationUnits (..),

    -- * EncoderProfile
    EncoderProfile (..),

    -- * EncodingName
    EncodingName (..),

    -- * EntitlementStatus
    EntitlementStatus (..),

    -- * FailoverMode
    FailoverMode (..),

    -- * GatewayState
    GatewayState (..),

    -- * InstanceState
    InstanceState (..),

    -- * KeyType
    KeyType (..),

    -- * MaintenanceDay
    MaintenanceDay (..),

    -- * MediaStreamType
    MediaStreamType (..),

    -- * NetworkInterfaceType
    NetworkInterfaceType (..),

    -- * PriceUnits
    PriceUnits (..),

    -- * Protocol
    Protocol (..),

    -- * Range
    Range (..),

    -- * ReservationState
    ReservationState (..),

    -- * ResourceType
    ResourceType (..),

    -- * ScanMode
    ScanMode (..),

    -- * SourceType
    SourceType (..),

    -- * State
    State (..),

    -- * Status
    Status (..),

    -- * Tcs
    Tcs (..),

    -- * AddBridgeFlowSourceRequest
    AddBridgeFlowSourceRequest (..),
    newAddBridgeFlowSourceRequest,
    addBridgeFlowSourceRequest_flowVpcInterfaceAttachment,
    addBridgeFlowSourceRequest_flowArn,
    addBridgeFlowSourceRequest_name,

    -- * AddBridgeNetworkOutputRequest
    AddBridgeNetworkOutputRequest (..),
    newAddBridgeNetworkOutputRequest,
    addBridgeNetworkOutputRequest_networkName,
    addBridgeNetworkOutputRequest_port,
    addBridgeNetworkOutputRequest_ipAddress,
    addBridgeNetworkOutputRequest_protocol,
    addBridgeNetworkOutputRequest_ttl,
    addBridgeNetworkOutputRequest_name,

    -- * AddBridgeNetworkSourceRequest
    AddBridgeNetworkSourceRequest (..),
    newAddBridgeNetworkSourceRequest,
    addBridgeNetworkSourceRequest_networkName,
    addBridgeNetworkSourceRequest_multicastIp,
    addBridgeNetworkSourceRequest_port,
    addBridgeNetworkSourceRequest_protocol,
    addBridgeNetworkSourceRequest_name,

    -- * AddBridgeOutputRequest
    AddBridgeOutputRequest (..),
    newAddBridgeOutputRequest,
    addBridgeOutputRequest_networkOutput,

    -- * AddBridgeSourceRequest
    AddBridgeSourceRequest (..),
    newAddBridgeSourceRequest,
    addBridgeSourceRequest_flowSource,
    addBridgeSourceRequest_networkSource,

    -- * AddEgressGatewayBridgeRequest
    AddEgressGatewayBridgeRequest (..),
    newAddEgressGatewayBridgeRequest,
    addEgressGatewayBridgeRequest_maxBitrate,

    -- * AddIngressGatewayBridgeRequest
    AddIngressGatewayBridgeRequest (..),
    newAddIngressGatewayBridgeRequest,
    addIngressGatewayBridgeRequest_maxOutputs,
    addIngressGatewayBridgeRequest_maxBitrate,

    -- * AddMaintenance
    AddMaintenance (..),
    newAddMaintenance,
    addMaintenance_maintenanceDay,
    addMaintenance_maintenanceStartHour,

    -- * AddMediaStreamRequest
    AddMediaStreamRequest (..),
    newAddMediaStreamRequest,
    addMediaStreamRequest_attributes,
    addMediaStreamRequest_clockRate,
    addMediaStreamRequest_description,
    addMediaStreamRequest_videoFormat,
    addMediaStreamRequest_mediaStreamType,
    addMediaStreamRequest_mediaStreamId,
    addMediaStreamRequest_mediaStreamName,

    -- * AddOutputRequest
    AddOutputRequest (..),
    newAddOutputRequest,
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

    -- * Bridge
    Bridge (..),
    newBridge,
    bridge_bridgeMessages,
    bridge_egressGatewayBridge,
    bridge_ingressGatewayBridge,
    bridge_outputs,
    bridge_sourceFailoverConfig,
    bridge_sources,
    bridge_bridgeArn,
    bridge_bridgeState,
    bridge_placementArn,
    bridge_name,

    -- * BridgeFlowOutput
    BridgeFlowOutput (..),
    newBridgeFlowOutput,
    bridgeFlowOutput_flowSourceArn,
    bridgeFlowOutput_flowArn,
    bridgeFlowOutput_name,

    -- * BridgeFlowSource
    BridgeFlowSource (..),
    newBridgeFlowSource,
    bridgeFlowSource_flowVpcInterfaceAttachment,
    bridgeFlowSource_outputArn,
    bridgeFlowSource_flowArn,
    bridgeFlowSource_name,

    -- * BridgeNetworkOutput
    BridgeNetworkOutput (..),
    newBridgeNetworkOutput,
    bridgeNetworkOutput_networkName,
    bridgeNetworkOutput_port,
    bridgeNetworkOutput_ipAddress,
    bridgeNetworkOutput_protocol,
    bridgeNetworkOutput_ttl,
    bridgeNetworkOutput_name,

    -- * BridgeNetworkSource
    BridgeNetworkSource (..),
    newBridgeNetworkSource,
    bridgeNetworkSource_networkName,
    bridgeNetworkSource_multicastIp,
    bridgeNetworkSource_port,
    bridgeNetworkSource_protocol,
    bridgeNetworkSource_name,

    -- * BridgeOutput
    BridgeOutput (..),
    newBridgeOutput,
    bridgeOutput_flowOutput,
    bridgeOutput_networkOutput,

    -- * BridgeSource
    BridgeSource (..),
    newBridgeSource,
    bridgeSource_flowSource,
    bridgeSource_networkSource,

    -- * DestinationConfiguration
    DestinationConfiguration (..),
    newDestinationConfiguration,
    destinationConfiguration_destinationIp,
    destinationConfiguration_destinationPort,
    destinationConfiguration_interface,
    destinationConfiguration_outboundIp,

    -- * DestinationConfigurationRequest
    DestinationConfigurationRequest (..),
    newDestinationConfigurationRequest,
    destinationConfigurationRequest_destinationIp,
    destinationConfigurationRequest_destinationPort,
    destinationConfigurationRequest_interface,

    -- * EgressGatewayBridge
    EgressGatewayBridge (..),
    newEgressGatewayBridge,
    egressGatewayBridge_instanceId,
    egressGatewayBridge_maxBitrate,

    -- * EncodingParameters
    EncodingParameters (..),
    newEncodingParameters,
    encodingParameters_encoderProfile,
    encodingParameters_compressionFactor,

    -- * EncodingParametersRequest
    EncodingParametersRequest (..),
    newEncodingParametersRequest,
    encodingParametersRequest_encoderProfile,
    encodingParametersRequest_compressionFactor,

    -- * Encryption
    Encryption (..),
    newEncryption,
    encryption_algorithm,
    encryption_constantInitializationVector,
    encryption_deviceId,
    encryption_keyType,
    encryption_region,
    encryption_resourceId,
    encryption_secretArn,
    encryption_url,
    encryption_roleArn,

    -- * Entitlement
    Entitlement (..),
    newEntitlement,
    entitlement_dataTransferSubscriberFeePercent,
    entitlement_description,
    entitlement_encryption,
    entitlement_entitlementStatus,
    entitlement_entitlementArn,
    entitlement_subscribers,
    entitlement_name,

    -- * FailoverConfig
    FailoverConfig (..),
    newFailoverConfig,
    failoverConfig_failoverMode,
    failoverConfig_recoveryWindow,
    failoverConfig_sourcePriority,
    failoverConfig_state,

    -- * Flow
    Flow (..),
    newFlow,
    flow_description,
    flow_egressIp,
    flow_maintenance,
    flow_mediaStreams,
    flow_sourceFailoverConfig,
    flow_sources,
    flow_vpcInterfaces,
    flow_status,
    flow_availabilityZone,
    flow_source,
    flow_name,
    flow_entitlements,
    flow_outputs,
    flow_flowArn,

    -- * Fmtp
    Fmtp (..),
    newFmtp,
    fmtp_channelOrder,
    fmtp_colorimetry,
    fmtp_exactFramerate,
    fmtp_par,
    fmtp_range,
    fmtp_scanMode,
    fmtp_tcs,

    -- * FmtpRequest
    FmtpRequest (..),
    newFmtpRequest,
    fmtpRequest_channelOrder,
    fmtpRequest_colorimetry,
    fmtpRequest_exactFramerate,
    fmtpRequest_par,
    fmtpRequest_range,
    fmtpRequest_scanMode,
    fmtpRequest_tcs,

    -- * Gateway
    Gateway (..),
    newGateway,
    gateway_gatewayMessages,
    gateway_gatewayState,
    gateway_gatewayArn,
    gateway_networks,
    gateway_egressCidrBlocks,
    gateway_name,

    -- * GatewayBridgeSource
    GatewayBridgeSource (..),
    newGatewayBridgeSource,
    gatewayBridgeSource_vpcInterfaceAttachment,
    gatewayBridgeSource_bridgeArn,

    -- * GatewayInstance
    GatewayInstance (..),
    newGatewayInstance,
    gatewayInstance_instanceMessages,
    gatewayInstance_gatewayArn,
    gatewayInstance_instanceState,
    gatewayInstance_gatewayInstanceArn,
    gatewayInstance_instanceId,
    gatewayInstance_runningBridgeCount,
    gatewayInstance_bridgePlacement,
    gatewayInstance_connectionStatus,

    -- * GatewayNetwork
    GatewayNetwork (..),
    newGatewayNetwork,
    gatewayNetwork_cidrBlock,
    gatewayNetwork_name,

    -- * GrantEntitlementRequest
    GrantEntitlementRequest (..),
    newGrantEntitlementRequest,
    grantEntitlementRequest_dataTransferSubscriberFeePercent,
    grantEntitlementRequest_description,
    grantEntitlementRequest_encryption,
    grantEntitlementRequest_entitlementStatus,
    grantEntitlementRequest_name,
    grantEntitlementRequest_subscribers,

    -- * IngressGatewayBridge
    IngressGatewayBridge (..),
    newIngressGatewayBridge,
    ingressGatewayBridge_instanceId,
    ingressGatewayBridge_maxOutputs,
    ingressGatewayBridge_maxBitrate,

    -- * InputConfiguration
    InputConfiguration (..),
    newInputConfiguration,
    inputConfiguration_inputPort,
    inputConfiguration_inputIp,
    inputConfiguration_interface,

    -- * InputConfigurationRequest
    InputConfigurationRequest (..),
    newInputConfigurationRequest,
    inputConfigurationRequest_inputPort,
    inputConfigurationRequest_interface,

    -- * Interface
    Interface (..),
    newInterface,
    interface_name,

    -- * InterfaceRequest
    InterfaceRequest (..),
    newInterfaceRequest,
    interfaceRequest_name,

    -- * ListedBridge
    ListedBridge (..),
    newListedBridge,
    listedBridge_bridgeArn,
    listedBridge_bridgeState,
    listedBridge_placementArn,
    listedBridge_bridgeType,
    listedBridge_name,

    -- * ListedEntitlement
    ListedEntitlement (..),
    newListedEntitlement,
    listedEntitlement_dataTransferSubscriberFeePercent,
    listedEntitlement_entitlementArn,
    listedEntitlement_entitlementName,

    -- * ListedFlow
    ListedFlow (..),
    newListedFlow,
    listedFlow_maintenance,
    listedFlow_status,
    listedFlow_description,
    listedFlow_sourceType,
    listedFlow_availabilityZone,
    listedFlow_flowArn,
    listedFlow_name,

    -- * ListedGateway
    ListedGateway (..),
    newListedGateway,
    listedGateway_gatewayArn,
    listedGateway_gatewayState,
    listedGateway_name,

    -- * ListedGatewayInstance
    ListedGatewayInstance (..),
    newListedGatewayInstance,
    listedGatewayInstance_instanceState,
    listedGatewayInstance_gatewayArn,
    listedGatewayInstance_gatewayInstanceArn,
    listedGatewayInstance_instanceId,

    -- * Maintenance
    Maintenance (..),
    newMaintenance,
    maintenance_maintenanceDay,
    maintenance_maintenanceDeadline,
    maintenance_maintenanceScheduledDate,
    maintenance_maintenanceStartHour,

    -- * MediaStream
    MediaStream (..),
    newMediaStream,
    mediaStream_attributes,
    mediaStream_clockRate,
    mediaStream_description,
    mediaStream_videoFormat,
    mediaStream_mediaStreamType,
    mediaStream_mediaStreamId,
    mediaStream_mediaStreamName,
    mediaStream_fmt,

    -- * MediaStreamAttributes
    MediaStreamAttributes (..),
    newMediaStreamAttributes,
    mediaStreamAttributes_lang,
    mediaStreamAttributes_fmtp,

    -- * MediaStreamAttributesRequest
    MediaStreamAttributesRequest (..),
    newMediaStreamAttributesRequest,
    mediaStreamAttributesRequest_fmtp,
    mediaStreamAttributesRequest_lang,

    -- * MediaStreamOutputConfiguration
    MediaStreamOutputConfiguration (..),
    newMediaStreamOutputConfiguration,
    mediaStreamOutputConfiguration_destinationConfigurations,
    mediaStreamOutputConfiguration_encodingParameters,
    mediaStreamOutputConfiguration_mediaStreamName,
    mediaStreamOutputConfiguration_encodingName,

    -- * MediaStreamOutputConfigurationRequest
    MediaStreamOutputConfigurationRequest (..),
    newMediaStreamOutputConfigurationRequest,
    mediaStreamOutputConfigurationRequest_destinationConfigurations,
    mediaStreamOutputConfigurationRequest_encodingParameters,
    mediaStreamOutputConfigurationRequest_mediaStreamName,
    mediaStreamOutputConfigurationRequest_encodingName,

    -- * MediaStreamSourceConfiguration
    MediaStreamSourceConfiguration (..),
    newMediaStreamSourceConfiguration,
    mediaStreamSourceConfiguration_inputConfigurations,
    mediaStreamSourceConfiguration_mediaStreamName,
    mediaStreamSourceConfiguration_encodingName,

    -- * MediaStreamSourceConfigurationRequest
    MediaStreamSourceConfigurationRequest (..),
    newMediaStreamSourceConfigurationRequest,
    mediaStreamSourceConfigurationRequest_inputConfigurations,
    mediaStreamSourceConfigurationRequest_mediaStreamName,
    mediaStreamSourceConfigurationRequest_encodingName,

    -- * MessageDetail
    MessageDetail (..),
    newMessageDetail,
    messageDetail_resourceName,
    messageDetail_message,
    messageDetail_code,

    -- * Messages
    Messages (..),
    newMessages,
    messages_errors,

    -- * Offering
    Offering (..),
    newOffering,
    offering_currencyCode,
    offering_offeringArn,
    offering_offeringDescription,
    offering_durationUnits,
    offering_duration,
    offering_pricePerUnit,
    offering_resourceSpecification,
    offering_priceUnits,

    -- * Output
    Output (..),
    newOutput,
    output_bridgeArn,
    output_bridgePorts,
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

    -- * Reservation
    Reservation (..),
    newReservation,
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

    -- * ResourceSpecification
    ResourceSpecification (..),
    newResourceSpecification,
    resourceSpecification_reservedBitrate,
    resourceSpecification_resourceType,

    -- * SetGatewayBridgeSourceRequest
    SetGatewayBridgeSourceRequest (..),
    newSetGatewayBridgeSourceRequest,
    setGatewayBridgeSourceRequest_vpcInterfaceAttachment,
    setGatewayBridgeSourceRequest_bridgeArn,

    -- * SetSourceRequest
    SetSourceRequest (..),
    newSetSourceRequest,
    setSourceRequest_decryption,
    setSourceRequest_description,
    setSourceRequest_entitlementArn,
    setSourceRequest_gatewayBridgeSource,
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

    -- * Source
    Source (..),
    newSource,
    source_dataTransferSubscriberFeePercent,
    source_decryption,
    source_description,
    source_entitlementArn,
    source_gatewayBridgeSource,
    source_ingestIp,
    source_ingestPort,
    source_mediaStreamSourceConfigurations,
    source_senderControlPort,
    source_senderIpAddress,
    source_transport,
    source_vpcInterfaceName,
    source_whitelistCidr,
    source_name,
    source_sourceArn,

    -- * SourcePriority
    SourcePriority (..),
    newSourcePriority,
    sourcePriority_primarySource,

    -- * Transport
    Transport (..),
    newTransport,
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

    -- * UpdateBridgeFlowSourceRequest
    UpdateBridgeFlowSourceRequest (..),
    newUpdateBridgeFlowSourceRequest,
    updateBridgeFlowSourceRequest_flowArn,
    updateBridgeFlowSourceRequest_flowVpcInterfaceAttachment,

    -- * UpdateBridgeNetworkOutputRequest
    UpdateBridgeNetworkOutputRequest (..),
    newUpdateBridgeNetworkOutputRequest,
    updateBridgeNetworkOutputRequest_ipAddress,
    updateBridgeNetworkOutputRequest_networkName,
    updateBridgeNetworkOutputRequest_port,
    updateBridgeNetworkOutputRequest_protocol,
    updateBridgeNetworkOutputRequest_ttl,

    -- * UpdateBridgeNetworkSourceRequest
    UpdateBridgeNetworkSourceRequest (..),
    newUpdateBridgeNetworkSourceRequest,
    updateBridgeNetworkSourceRequest_multicastIp,
    updateBridgeNetworkSourceRequest_networkName,
    updateBridgeNetworkSourceRequest_port,
    updateBridgeNetworkSourceRequest_protocol,

    -- * UpdateEgressGatewayBridgeRequest
    UpdateEgressGatewayBridgeRequest (..),
    newUpdateEgressGatewayBridgeRequest,
    updateEgressGatewayBridgeRequest_maxBitrate,

    -- * UpdateEncryption
    UpdateEncryption (..),
    newUpdateEncryption,
    updateEncryption_algorithm,
    updateEncryption_constantInitializationVector,
    updateEncryption_deviceId,
    updateEncryption_keyType,
    updateEncryption_region,
    updateEncryption_resourceId,
    updateEncryption_roleArn,
    updateEncryption_secretArn,
    updateEncryption_url,

    -- * UpdateFailoverConfig
    UpdateFailoverConfig (..),
    newUpdateFailoverConfig,
    updateFailoverConfig_failoverMode,
    updateFailoverConfig_recoveryWindow,
    updateFailoverConfig_sourcePriority,
    updateFailoverConfig_state,

    -- * UpdateGatewayBridgeSourceRequest
    UpdateGatewayBridgeSourceRequest (..),
    newUpdateGatewayBridgeSourceRequest,
    updateGatewayBridgeSourceRequest_bridgeArn,
    updateGatewayBridgeSourceRequest_vpcInterfaceAttachment,

    -- * UpdateIngressGatewayBridgeRequest
    UpdateIngressGatewayBridgeRequest (..),
    newUpdateIngressGatewayBridgeRequest,
    updateIngressGatewayBridgeRequest_maxBitrate,
    updateIngressGatewayBridgeRequest_maxOutputs,

    -- * UpdateMaintenance
    UpdateMaintenance (..),
    newUpdateMaintenance,
    updateMaintenance_maintenanceDay,
    updateMaintenance_maintenanceScheduledDate,
    updateMaintenance_maintenanceStartHour,

    -- * VpcInterface
    VpcInterface (..),
    newVpcInterface,
    vpcInterface_networkInterfaceType,
    vpcInterface_networkInterfaceIds,
    vpcInterface_subnetId,
    vpcInterface_securityGroupIds,
    vpcInterface_roleArn,
    vpcInterface_name,

    -- * VpcInterfaceAttachment
    VpcInterfaceAttachment (..),
    newVpcInterfaceAttachment,
    vpcInterfaceAttachment_vpcInterfaceName,

    -- * VpcInterfaceRequest
    VpcInterfaceRequest (..),
    newVpcInterfaceRequest,
    vpcInterfaceRequest_networkInterfaceType,
    vpcInterfaceRequest_subnetId,
    vpcInterfaceRequest_securityGroupIds,
    vpcInterfaceRequest_roleArn,
    vpcInterfaceRequest_name,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaConnect.Types.AddBridgeFlowSourceRequest
import Amazonka.MediaConnect.Types.AddBridgeNetworkOutputRequest
import Amazonka.MediaConnect.Types.AddBridgeNetworkSourceRequest
import Amazonka.MediaConnect.Types.AddBridgeOutputRequest
import Amazonka.MediaConnect.Types.AddBridgeSourceRequest
import Amazonka.MediaConnect.Types.AddEgressGatewayBridgeRequest
import Amazonka.MediaConnect.Types.AddIngressGatewayBridgeRequest
import Amazonka.MediaConnect.Types.AddMaintenance
import Amazonka.MediaConnect.Types.AddMediaStreamRequest
import Amazonka.MediaConnect.Types.AddOutputRequest
import Amazonka.MediaConnect.Types.Algorithm
import Amazonka.MediaConnect.Types.Bridge
import Amazonka.MediaConnect.Types.BridgeFlowOutput
import Amazonka.MediaConnect.Types.BridgeFlowSource
import Amazonka.MediaConnect.Types.BridgeNetworkOutput
import Amazonka.MediaConnect.Types.BridgeNetworkSource
import Amazonka.MediaConnect.Types.BridgeOutput
import Amazonka.MediaConnect.Types.BridgePlacement
import Amazonka.MediaConnect.Types.BridgeSource
import Amazonka.MediaConnect.Types.BridgeState
import Amazonka.MediaConnect.Types.Colorimetry
import Amazonka.MediaConnect.Types.ConnectionStatus
import Amazonka.MediaConnect.Types.DesiredState
import Amazonka.MediaConnect.Types.DestinationConfiguration
import Amazonka.MediaConnect.Types.DestinationConfigurationRequest
import Amazonka.MediaConnect.Types.DurationUnits
import Amazonka.MediaConnect.Types.EgressGatewayBridge
import Amazonka.MediaConnect.Types.EncoderProfile
import Amazonka.MediaConnect.Types.EncodingName
import Amazonka.MediaConnect.Types.EncodingParameters
import Amazonka.MediaConnect.Types.EncodingParametersRequest
import Amazonka.MediaConnect.Types.Encryption
import Amazonka.MediaConnect.Types.Entitlement
import Amazonka.MediaConnect.Types.EntitlementStatus
import Amazonka.MediaConnect.Types.FailoverConfig
import Amazonka.MediaConnect.Types.FailoverMode
import Amazonka.MediaConnect.Types.Flow
import Amazonka.MediaConnect.Types.Fmtp
import Amazonka.MediaConnect.Types.FmtpRequest
import Amazonka.MediaConnect.Types.Gateway
import Amazonka.MediaConnect.Types.GatewayBridgeSource
import Amazonka.MediaConnect.Types.GatewayInstance
import Amazonka.MediaConnect.Types.GatewayNetwork
import Amazonka.MediaConnect.Types.GatewayState
import Amazonka.MediaConnect.Types.GrantEntitlementRequest
import Amazonka.MediaConnect.Types.IngressGatewayBridge
import Amazonka.MediaConnect.Types.InputConfiguration
import Amazonka.MediaConnect.Types.InputConfigurationRequest
import Amazonka.MediaConnect.Types.InstanceState
import Amazonka.MediaConnect.Types.Interface
import Amazonka.MediaConnect.Types.InterfaceRequest
import Amazonka.MediaConnect.Types.KeyType
import Amazonka.MediaConnect.Types.ListedBridge
import Amazonka.MediaConnect.Types.ListedEntitlement
import Amazonka.MediaConnect.Types.ListedFlow
import Amazonka.MediaConnect.Types.ListedGateway
import Amazonka.MediaConnect.Types.ListedGatewayInstance
import Amazonka.MediaConnect.Types.Maintenance
import Amazonka.MediaConnect.Types.MaintenanceDay
import Amazonka.MediaConnect.Types.MediaStream
import Amazonka.MediaConnect.Types.MediaStreamAttributes
import Amazonka.MediaConnect.Types.MediaStreamAttributesRequest
import Amazonka.MediaConnect.Types.MediaStreamOutputConfiguration
import Amazonka.MediaConnect.Types.MediaStreamOutputConfigurationRequest
import Amazonka.MediaConnect.Types.MediaStreamSourceConfiguration
import Amazonka.MediaConnect.Types.MediaStreamSourceConfigurationRequest
import Amazonka.MediaConnect.Types.MediaStreamType
import Amazonka.MediaConnect.Types.MessageDetail
import Amazonka.MediaConnect.Types.Messages
import Amazonka.MediaConnect.Types.NetworkInterfaceType
import Amazonka.MediaConnect.Types.Offering
import Amazonka.MediaConnect.Types.Output
import Amazonka.MediaConnect.Types.PriceUnits
import Amazonka.MediaConnect.Types.Protocol
import Amazonka.MediaConnect.Types.Range
import Amazonka.MediaConnect.Types.Reservation
import Amazonka.MediaConnect.Types.ReservationState
import Amazonka.MediaConnect.Types.ResourceSpecification
import Amazonka.MediaConnect.Types.ResourceType
import Amazonka.MediaConnect.Types.ScanMode
import Amazonka.MediaConnect.Types.SetGatewayBridgeSourceRequest
import Amazonka.MediaConnect.Types.SetSourceRequest
import Amazonka.MediaConnect.Types.Source
import Amazonka.MediaConnect.Types.SourcePriority
import Amazonka.MediaConnect.Types.SourceType
import Amazonka.MediaConnect.Types.State
import Amazonka.MediaConnect.Types.Status
import Amazonka.MediaConnect.Types.Tcs
import Amazonka.MediaConnect.Types.Transport
import Amazonka.MediaConnect.Types.UpdateBridgeFlowSourceRequest
import Amazonka.MediaConnect.Types.UpdateBridgeNetworkOutputRequest
import Amazonka.MediaConnect.Types.UpdateBridgeNetworkSourceRequest
import Amazonka.MediaConnect.Types.UpdateEgressGatewayBridgeRequest
import Amazonka.MediaConnect.Types.UpdateEncryption
import Amazonka.MediaConnect.Types.UpdateFailoverConfig
import Amazonka.MediaConnect.Types.UpdateGatewayBridgeSourceRequest
import Amazonka.MediaConnect.Types.UpdateIngressGatewayBridgeRequest
import Amazonka.MediaConnect.Types.UpdateMaintenance
import Amazonka.MediaConnect.Types.VpcInterface
import Amazonka.MediaConnect.Types.VpcInterfaceAttachment
import Amazonka.MediaConnect.Types.VpcInterfaceRequest
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-11-14@ of the Amazon MediaConnect SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "MediaConnect",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "mediaconnect",
      Core.signingName = "mediaconnect",
      Core.version = "2018-11-14",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "MediaConnect",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | Exception raised by AWS Elemental MediaConnect. See the error message
-- and documentation for the operation for more information on the cause of
-- this exception.
_AddFlowOutputs420Exception :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AddFlowOutputs420Exception =
  Core._MatchServiceError
    defaultService
    "AddFlowOutputs420Exception"
    Prelude.. Core.hasStatus 420

-- | Exception raised by AWS Elemental MediaConnect. See the error message
-- and documentation for the operation for more information on the cause of
-- this exception.
_BadRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | Exception raised by AWS Elemental MediaConnect. See the error message
-- and documentation for the operation for more information on the cause of
-- this exception.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Exception raised by AWS Elemental MediaConnect. See the error message
-- and documentation for the operation for more information on the cause of
-- this exception.
_CreateBridge420Exception :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CreateBridge420Exception =
  Core._MatchServiceError
    defaultService
    "CreateBridge420Exception"
    Prelude.. Core.hasStatus 420

-- | Exception raised by AWS Elemental MediaConnect. See the error message
-- and documentation for the operation for more information on the cause of
-- this exception.
_CreateFlow420Exception :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CreateFlow420Exception =
  Core._MatchServiceError
    defaultService
    "CreateFlow420Exception"
    Prelude.. Core.hasStatus 420

-- | Exception raised by AWS Elemental MediaConnect. See the error message
-- and documentation for the operation for more information on the cause of
-- this exception.
_CreateGateway420Exception :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CreateGateway420Exception =
  Core._MatchServiceError
    defaultService
    "CreateGateway420Exception"
    Prelude.. Core.hasStatus 420

-- | Exception raised by AWS Elemental MediaConnect. See the error message
-- and documentation for the operation for more information on the cause of
-- this exception.
_ForbiddenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Core.hasStatus 403

-- | Exception raised by AWS Elemental MediaConnect. See the error message
-- and documentation for the operation for more information on the cause of
-- this exception.
_GrantFlowEntitlements420Exception :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_GrantFlowEntitlements420Exception =
  Core._MatchServiceError
    defaultService
    "GrantFlowEntitlements420Exception"
    Prelude.. Core.hasStatus 420

-- | Exception raised by AWS Elemental MediaConnect. See the error message
-- and documentation for the operation for more information on the cause of
-- this exception.
_InternalServerErrorException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Core.hasStatus 500

-- | Exception raised by AWS Elemental MediaConnect. See the error message
-- and documentation for the operation for more information on the cause of
-- this exception.
_NotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | Exception raised by AWS Elemental MediaConnect. See the error message
-- and documentation for the operation for more information on the cause of
-- this exception.
_ServiceUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | Exception raised by AWS Elemental MediaConnect. See the error message
-- and documentation for the operation for more information on the cause of
-- this exception.
_TooManyRequestsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429
