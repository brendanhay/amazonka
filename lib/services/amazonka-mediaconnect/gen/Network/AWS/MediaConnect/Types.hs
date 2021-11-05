{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConnect.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConnect.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _GrantFlowEntitlements420Exception,
    _ForbiddenException,
    _NotFoundException,
    _CreateFlow420Exception,
    _TooManyRequestsException,
    _InternalServerErrorException,
    _ServiceUnavailableException,
    _AddFlowOutputs420Exception,
    _BadRequestException,

    -- * Algorithm
    Algorithm (..),

    -- * Colorimetry
    Colorimetry (..),

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

    -- * KeyType
    KeyType (..),

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

    -- * AddMediaStreamRequest
    AddMediaStreamRequest (..),
    newAddMediaStreamRequest,
    addMediaStreamRequest_videoFormat,
    addMediaStreamRequest_attributes,
    addMediaStreamRequest_clockRate,
    addMediaStreamRequest_description,
    addMediaStreamRequest_mediaStreamType,
    addMediaStreamRequest_mediaStreamId,
    addMediaStreamRequest_mediaStreamName,

    -- * AddOutputRequest
    AddOutputRequest (..),
    newAddOutputRequest,
    addOutputRequest_destination,
    addOutputRequest_maxLatency,
    addOutputRequest_mediaStreamOutputConfigurations,
    addOutputRequest_encryption,
    addOutputRequest_name,
    addOutputRequest_cidrAllowList,
    addOutputRequest_smoothingLatency,
    addOutputRequest_minLatency,
    addOutputRequest_description,
    addOutputRequest_port,
    addOutputRequest_streamId,
    addOutputRequest_remoteId,
    addOutputRequest_vpcInterfaceAttachment,
    addOutputRequest_protocol,

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
    encryption_keyType,
    encryption_resourceId,
    encryption_url,
    encryption_algorithm,
    encryption_constantInitializationVector,
    encryption_deviceId,
    encryption_region,
    encryption_secretArn,
    encryption_roleArn,

    -- * Entitlement
    Entitlement (..),
    newEntitlement,
    entitlement_dataTransferSubscriberFeePercent,
    entitlement_encryption,
    entitlement_entitlementStatus,
    entitlement_description,
    entitlement_entitlementArn,
    entitlement_subscribers,
    entitlement_name,

    -- * FailoverConfig
    FailoverConfig (..),
    newFailoverConfig,
    failoverConfig_state,
    failoverConfig_recoveryWindow,
    failoverConfig_sourcePriority,
    failoverConfig_failoverMode,

    -- * Flow
    Flow (..),
    newFlow,
    flow_mediaStreams,
    flow_sourceFailoverConfig,
    flow_vpcInterfaces,
    flow_sources,
    flow_egressIp,
    flow_description,
    flow_status,
    flow_entitlements,
    flow_outputs,
    flow_availabilityZone,
    flow_flowArn,
    flow_source,
    flow_name,

    -- * Fmtp
    Fmtp (..),
    newFmtp,
    fmtp_tcs,
    fmtp_exactFramerate,
    fmtp_par,
    fmtp_scanMode,
    fmtp_range,
    fmtp_channelOrder,
    fmtp_colorimetry,

    -- * FmtpRequest
    FmtpRequest (..),
    newFmtpRequest,
    fmtpRequest_tcs,
    fmtpRequest_exactFramerate,
    fmtpRequest_par,
    fmtpRequest_scanMode,
    fmtpRequest_range,
    fmtpRequest_channelOrder,
    fmtpRequest_colorimetry,

    -- * GrantEntitlementRequest
    GrantEntitlementRequest (..),
    newGrantEntitlementRequest,
    grantEntitlementRequest_dataTransferSubscriberFeePercent,
    grantEntitlementRequest_encryption,
    grantEntitlementRequest_name,
    grantEntitlementRequest_entitlementStatus,
    grantEntitlementRequest_description,
    grantEntitlementRequest_subscribers,

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

    -- * ListedEntitlement
    ListedEntitlement (..),
    newListedEntitlement,
    listedEntitlement_dataTransferSubscriberFeePercent,
    listedEntitlement_entitlementArn,
    listedEntitlement_entitlementName,

    -- * ListedFlow
    ListedFlow (..),
    newListedFlow,
    listedFlow_status,
    listedFlow_description,
    listedFlow_sourceType,
    listedFlow_availabilityZone,
    listedFlow_flowArn,
    listedFlow_name,

    -- * MediaStream
    MediaStream (..),
    newMediaStream,
    mediaStream_videoFormat,
    mediaStream_attributes,
    mediaStream_clockRate,
    mediaStream_description,
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
    mediaStreamAttributesRequest_lang,
    mediaStreamAttributesRequest_fmtp,

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
    output_entitlementArn,
    output_dataTransferSubscriberFeePercent,
    output_destination,
    output_mediaStreamOutputConfigurations,
    output_mediaLiveInputArn,
    output_encryption,
    output_listenerAddress,
    output_transport,
    output_description,
    output_port,
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

    -- * SetSourceRequest
    SetSourceRequest (..),
    newSetSourceRequest,
    setSourceRequest_entitlementArn,
    setSourceRequest_maxLatency,
    setSourceRequest_vpcInterfaceName,
    setSourceRequest_decryption,
    setSourceRequest_maxSyncBuffer,
    setSourceRequest_protocol,
    setSourceRequest_name,
    setSourceRequest_minLatency,
    setSourceRequest_ingestPort,
    setSourceRequest_description,
    setSourceRequest_mediaStreamSourceConfigurations,
    setSourceRequest_whitelistCidr,
    setSourceRequest_maxBitrate,
    setSourceRequest_streamId,

    -- * Source
    Source (..),
    newSource,
    source_entitlementArn,
    source_dataTransferSubscriberFeePercent,
    source_vpcInterfaceName,
    source_decryption,
    source_ingestIp,
    source_ingestPort,
    source_transport,
    source_description,
    source_mediaStreamSourceConfigurations,
    source_whitelistCidr,
    source_sourceArn,
    source_name,

    -- * SourcePriority
    SourcePriority (..),
    newSourcePriority,
    sourcePriority_primarySource,

    -- * Transport
    Transport (..),
    newTransport,
    transport_maxLatency,
    transport_maxSyncBuffer,
    transport_cidrAllowList,
    transport_smoothingLatency,
    transport_minLatency,
    transport_maxBitrate,
    transport_streamId,
    transport_remoteId,
    transport_protocol,

    -- * UpdateEncryption
    UpdateEncryption (..),
    newUpdateEncryption,
    updateEncryption_keyType,
    updateEncryption_resourceId,
    updateEncryption_url,
    updateEncryption_algorithm,
    updateEncryption_constantInitializationVector,
    updateEncryption_deviceId,
    updateEncryption_region,
    updateEncryption_secretArn,
    updateEncryption_roleArn,

    -- * UpdateFailoverConfig
    UpdateFailoverConfig (..),
    newUpdateFailoverConfig,
    updateFailoverConfig_state,
    updateFailoverConfig_recoveryWindow,
    updateFailoverConfig_sourcePriority,
    updateFailoverConfig_failoverMode,

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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConnect.Types.AddMediaStreamRequest
import Network.AWS.MediaConnect.Types.AddOutputRequest
import Network.AWS.MediaConnect.Types.Algorithm
import Network.AWS.MediaConnect.Types.Colorimetry
import Network.AWS.MediaConnect.Types.DestinationConfiguration
import Network.AWS.MediaConnect.Types.DestinationConfigurationRequest
import Network.AWS.MediaConnect.Types.DurationUnits
import Network.AWS.MediaConnect.Types.EncoderProfile
import Network.AWS.MediaConnect.Types.EncodingName
import Network.AWS.MediaConnect.Types.EncodingParameters
import Network.AWS.MediaConnect.Types.EncodingParametersRequest
import Network.AWS.MediaConnect.Types.Encryption
import Network.AWS.MediaConnect.Types.Entitlement
import Network.AWS.MediaConnect.Types.EntitlementStatus
import Network.AWS.MediaConnect.Types.FailoverConfig
import Network.AWS.MediaConnect.Types.FailoverMode
import Network.AWS.MediaConnect.Types.Flow
import Network.AWS.MediaConnect.Types.Fmtp
import Network.AWS.MediaConnect.Types.FmtpRequest
import Network.AWS.MediaConnect.Types.GrantEntitlementRequest
import Network.AWS.MediaConnect.Types.InputConfiguration
import Network.AWS.MediaConnect.Types.InputConfigurationRequest
import Network.AWS.MediaConnect.Types.Interface
import Network.AWS.MediaConnect.Types.InterfaceRequest
import Network.AWS.MediaConnect.Types.KeyType
import Network.AWS.MediaConnect.Types.ListedEntitlement
import Network.AWS.MediaConnect.Types.ListedFlow
import Network.AWS.MediaConnect.Types.MediaStream
import Network.AWS.MediaConnect.Types.MediaStreamAttributes
import Network.AWS.MediaConnect.Types.MediaStreamAttributesRequest
import Network.AWS.MediaConnect.Types.MediaStreamOutputConfiguration
import Network.AWS.MediaConnect.Types.MediaStreamOutputConfigurationRequest
import Network.AWS.MediaConnect.Types.MediaStreamSourceConfiguration
import Network.AWS.MediaConnect.Types.MediaStreamSourceConfigurationRequest
import Network.AWS.MediaConnect.Types.MediaStreamType
import Network.AWS.MediaConnect.Types.Messages
import Network.AWS.MediaConnect.Types.NetworkInterfaceType
import Network.AWS.MediaConnect.Types.Offering
import Network.AWS.MediaConnect.Types.Output
import Network.AWS.MediaConnect.Types.PriceUnits
import Network.AWS.MediaConnect.Types.Protocol
import Network.AWS.MediaConnect.Types.Range
import Network.AWS.MediaConnect.Types.Reservation
import Network.AWS.MediaConnect.Types.ReservationState
import Network.AWS.MediaConnect.Types.ResourceSpecification
import Network.AWS.MediaConnect.Types.ResourceType
import Network.AWS.MediaConnect.Types.ScanMode
import Network.AWS.MediaConnect.Types.SetSourceRequest
import Network.AWS.MediaConnect.Types.Source
import Network.AWS.MediaConnect.Types.SourcePriority
import Network.AWS.MediaConnect.Types.SourceType
import Network.AWS.MediaConnect.Types.State
import Network.AWS.MediaConnect.Types.Status
import Network.AWS.MediaConnect.Types.Tcs
import Network.AWS.MediaConnect.Types.Transport
import Network.AWS.MediaConnect.Types.UpdateEncryption
import Network.AWS.MediaConnect.Types.UpdateFailoverConfig
import Network.AWS.MediaConnect.Types.VpcInterface
import Network.AWS.MediaConnect.Types.VpcInterfaceAttachment
import Network.AWS.MediaConnect.Types.VpcInterfaceRequest
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2018-11-14@ of the Amazon MediaConnect SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "MediaConnect",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "mediaconnect",
      Core._serviceSigningName = "mediaconnect",
      Core._serviceVersion = "2018-11-14",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "MediaConnect",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Exception raised by AWS Elemental MediaConnect. See the error message
-- and documentation for the operation for more information on the cause of
-- this exception.
_GrantFlowEntitlements420Exception :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GrantFlowEntitlements420Exception =
  Core._MatchServiceError
    defaultService
    "GrantFlowEntitlements420Exception"
    Prelude.. Core.hasStatus 420

-- | Exception raised by AWS Elemental MediaConnect. See the error message
-- and documentation for the operation for more information on the cause of
-- this exception.
_ForbiddenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Core.hasStatus 403

-- | Exception raised by AWS Elemental MediaConnect. See the error message
-- and documentation for the operation for more information on the cause of
-- this exception.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | Exception raised by AWS Elemental MediaConnect. See the error message
-- and documentation for the operation for more information on the cause of
-- this exception.
_CreateFlow420Exception :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CreateFlow420Exception =
  Core._MatchServiceError
    defaultService
    "CreateFlow420Exception"
    Prelude.. Core.hasStatus 420

-- | Exception raised by AWS Elemental MediaConnect. See the error message
-- and documentation for the operation for more information on the cause of
-- this exception.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429

-- | Exception raised by AWS Elemental MediaConnect. See the error message
-- and documentation for the operation for more information on the cause of
-- this exception.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Core.hasStatus 500

-- | Exception raised by AWS Elemental MediaConnect. See the error message
-- and documentation for the operation for more information on the cause of
-- this exception.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | Exception raised by AWS Elemental MediaConnect. See the error message
-- and documentation for the operation for more information on the cause of
-- this exception.
_AddFlowOutputs420Exception :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AddFlowOutputs420Exception =
  Core._MatchServiceError
    defaultService
    "AddFlowOutputs420Exception"
    Prelude.. Core.hasStatus 420

-- | Exception raised by AWS Elemental MediaConnect. See the error message
-- and documentation for the operation for more information on the cause of
-- this exception.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400
