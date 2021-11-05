{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GroundStation.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GroundStation.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidParameterException,
    _DependencyException,
    _ResourceLimitExceededException,
    _ResourceNotFoundException,

    -- * AngleUnits
    AngleUnits (..),

    -- * BandwidthUnits
    BandwidthUnits (..),

    -- * ConfigCapabilityType
    ConfigCapabilityType (..),

    -- * ContactStatus
    ContactStatus (..),

    -- * Criticality
    Criticality (..),

    -- * EirpUnits
    EirpUnits (..),

    -- * EndpointStatus
    EndpointStatus (..),

    -- * FrequencyUnits
    FrequencyUnits (..),

    -- * Polarization
    Polarization (..),

    -- * AntennaDemodDecodeDetails
    AntennaDemodDecodeDetails (..),
    newAntennaDemodDecodeDetails,
    antennaDemodDecodeDetails_outputNode,

    -- * AntennaDownlinkConfig
    AntennaDownlinkConfig (..),
    newAntennaDownlinkConfig,
    antennaDownlinkConfig_spectrumConfig,

    -- * AntennaDownlinkDemodDecodeConfig
    AntennaDownlinkDemodDecodeConfig (..),
    newAntennaDownlinkDemodDecodeConfig,
    antennaDownlinkDemodDecodeConfig_decodeConfig,
    antennaDownlinkDemodDecodeConfig_demodulationConfig,
    antennaDownlinkDemodDecodeConfig_spectrumConfig,

    -- * AntennaUplinkConfig
    AntennaUplinkConfig (..),
    newAntennaUplinkConfig,
    antennaUplinkConfig_transmitDisabled,
    antennaUplinkConfig_spectrumConfig,
    antennaUplinkConfig_targetEirp,

    -- * ConfigDetails
    ConfigDetails (..),
    newConfigDetails,
    configDetails_antennaDemodDecodeDetails,
    configDetails_s3RecordingDetails,
    configDetails_endpointDetails,

    -- * ConfigIdResponse
    ConfigIdResponse (..),
    newConfigIdResponse,
    configIdResponse_configArn,
    configIdResponse_configId,
    configIdResponse_configType,

    -- * ConfigListItem
    ConfigListItem (..),
    newConfigListItem,
    configListItem_configArn,
    configListItem_name,
    configListItem_configId,
    configListItem_configType,

    -- * ConfigTypeData
    ConfigTypeData (..),
    newConfigTypeData,
    configTypeData_antennaDownlinkDemodDecodeConfig,
    configTypeData_antennaDownlinkConfig,
    configTypeData_antennaUplinkConfig,
    configTypeData_uplinkEchoConfig,
    configTypeData_s3RecordingConfig,
    configTypeData_trackingConfig,
    configTypeData_dataflowEndpointConfig,

    -- * ContactData
    ContactData (..),
    newContactData,
    contactData_contactStatus,
    contactData_missionProfileArn,
    contactData_startTime,
    contactData_satelliteArn,
    contactData_maximumElevation,
    contactData_groundStation,
    contactData_endTime,
    contactData_contactId,
    contactData_region,
    contactData_postPassEndTime,
    contactData_prePassStartTime,
    contactData_errorMessage,
    contactData_tags,

    -- * ContactIdResponse
    ContactIdResponse (..),
    newContactIdResponse,
    contactIdResponse_contactId,

    -- * DataflowDetail
    DataflowDetail (..),
    newDataflowDetail,
    dataflowDetail_destination,
    dataflowDetail_source,
    dataflowDetail_errorMessage,

    -- * DataflowEndpoint
    DataflowEndpoint (..),
    newDataflowEndpoint,
    dataflowEndpoint_mtu,
    dataflowEndpoint_status,
    dataflowEndpoint_address,
    dataflowEndpoint_name,

    -- * DataflowEndpointConfig
    DataflowEndpointConfig (..),
    newDataflowEndpointConfig,
    dataflowEndpointConfig_dataflowEndpointRegion,
    dataflowEndpointConfig_dataflowEndpointName,

    -- * DataflowEndpointGroupIdResponse
    DataflowEndpointGroupIdResponse (..),
    newDataflowEndpointGroupIdResponse,
    dataflowEndpointGroupIdResponse_dataflowEndpointGroupId,

    -- * DataflowEndpointListItem
    DataflowEndpointListItem (..),
    newDataflowEndpointListItem,
    dataflowEndpointListItem_dataflowEndpointGroupArn,
    dataflowEndpointListItem_dataflowEndpointGroupId,

    -- * DecodeConfig
    DecodeConfig (..),
    newDecodeConfig,
    decodeConfig_unvalidatedJSON,

    -- * DemodulationConfig
    DemodulationConfig (..),
    newDemodulationConfig,
    demodulationConfig_unvalidatedJSON,

    -- * Destination
    Destination (..),
    newDestination,
    destination_dataflowDestinationRegion,
    destination_configId,
    destination_configType,
    destination_configDetails,

    -- * Eirp
    Eirp (..),
    newEirp,
    eirp_units,
    eirp_value,

    -- * Elevation
    Elevation (..),
    newElevation,
    elevation_unit,
    elevation_value,

    -- * EndpointDetails
    EndpointDetails (..),
    newEndpointDetails,
    endpointDetails_securityDetails,
    endpointDetails_endpoint,

    -- * Frequency
    Frequency (..),
    newFrequency,
    frequency_units,
    frequency_value,

    -- * FrequencyBandwidth
    FrequencyBandwidth (..),
    newFrequencyBandwidth,
    frequencyBandwidth_units,
    frequencyBandwidth_value,

    -- * GroundStationData
    GroundStationData (..),
    newGroundStationData,
    groundStationData_groundStationId,
    groundStationData_groundStationName,
    groundStationData_region,

    -- * MissionProfileIdResponse
    MissionProfileIdResponse (..),
    newMissionProfileIdResponse,
    missionProfileIdResponse_missionProfileId,

    -- * MissionProfileListItem
    MissionProfileListItem (..),
    newMissionProfileListItem,
    missionProfileListItem_missionProfileId,
    missionProfileListItem_missionProfileArn,
    missionProfileListItem_name,
    missionProfileListItem_region,

    -- * S3RecordingConfig
    S3RecordingConfig (..),
    newS3RecordingConfig,
    s3RecordingConfig_prefix,
    s3RecordingConfig_bucketArn,
    s3RecordingConfig_roleArn,

    -- * S3RecordingDetails
    S3RecordingDetails (..),
    newS3RecordingDetails,
    s3RecordingDetails_keyTemplate,
    s3RecordingDetails_bucketArn,

    -- * SatelliteListItem
    SatelliteListItem (..),
    newSatelliteListItem,
    satelliteListItem_satelliteId,
    satelliteListItem_satelliteArn,
    satelliteListItem_groundStations,
    satelliteListItem_noradSatelliteID,

    -- * SecurityDetails
    SecurityDetails (..),
    newSecurityDetails,
    securityDetails_roleArn,
    securityDetails_securityGroupIds,
    securityDetails_subnetIds,

    -- * SocketAddress
    SocketAddress (..),
    newSocketAddress,
    socketAddress_name,
    socketAddress_port,

    -- * Source
    Source (..),
    newSource,
    source_dataflowSourceRegion,
    source_configId,
    source_configType,
    source_configDetails,

    -- * SpectrumConfig
    SpectrumConfig (..),
    newSpectrumConfig,
    spectrumConfig_polarization,
    spectrumConfig_bandwidth,
    spectrumConfig_centerFrequency,

    -- * TrackingConfig
    TrackingConfig (..),
    newTrackingConfig,
    trackingConfig_autotrack,

    -- * UplinkEchoConfig
    UplinkEchoConfig (..),
    newUplinkEchoConfig,
    uplinkEchoConfig_antennaUplinkConfigArn,
    uplinkEchoConfig_enabled,

    -- * UplinkSpectrumConfig
    UplinkSpectrumConfig (..),
    newUplinkSpectrumConfig,
    uplinkSpectrumConfig_polarization,
    uplinkSpectrumConfig_centerFrequency,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GroundStation.Types.AngleUnits
import Network.AWS.GroundStation.Types.AntennaDemodDecodeDetails
import Network.AWS.GroundStation.Types.AntennaDownlinkConfig
import Network.AWS.GroundStation.Types.AntennaDownlinkDemodDecodeConfig
import Network.AWS.GroundStation.Types.AntennaUplinkConfig
import Network.AWS.GroundStation.Types.BandwidthUnits
import Network.AWS.GroundStation.Types.ConfigCapabilityType
import Network.AWS.GroundStation.Types.ConfigDetails
import Network.AWS.GroundStation.Types.ConfigIdResponse
import Network.AWS.GroundStation.Types.ConfigListItem
import Network.AWS.GroundStation.Types.ConfigTypeData
import Network.AWS.GroundStation.Types.ContactData
import Network.AWS.GroundStation.Types.ContactIdResponse
import Network.AWS.GroundStation.Types.ContactStatus
import Network.AWS.GroundStation.Types.Criticality
import Network.AWS.GroundStation.Types.DataflowDetail
import Network.AWS.GroundStation.Types.DataflowEndpoint
import Network.AWS.GroundStation.Types.DataflowEndpointConfig
import Network.AWS.GroundStation.Types.DataflowEndpointGroupIdResponse
import Network.AWS.GroundStation.Types.DataflowEndpointListItem
import Network.AWS.GroundStation.Types.DecodeConfig
import Network.AWS.GroundStation.Types.DemodulationConfig
import Network.AWS.GroundStation.Types.Destination
import Network.AWS.GroundStation.Types.Eirp
import Network.AWS.GroundStation.Types.EirpUnits
import Network.AWS.GroundStation.Types.Elevation
import Network.AWS.GroundStation.Types.EndpointDetails
import Network.AWS.GroundStation.Types.EndpointStatus
import Network.AWS.GroundStation.Types.Frequency
import Network.AWS.GroundStation.Types.FrequencyBandwidth
import Network.AWS.GroundStation.Types.FrequencyUnits
import Network.AWS.GroundStation.Types.GroundStationData
import Network.AWS.GroundStation.Types.MissionProfileIdResponse
import Network.AWS.GroundStation.Types.MissionProfileListItem
import Network.AWS.GroundStation.Types.Polarization
import Network.AWS.GroundStation.Types.S3RecordingConfig
import Network.AWS.GroundStation.Types.S3RecordingDetails
import Network.AWS.GroundStation.Types.SatelliteListItem
import Network.AWS.GroundStation.Types.SecurityDetails
import Network.AWS.GroundStation.Types.SocketAddress
import Network.AWS.GroundStation.Types.Source
import Network.AWS.GroundStation.Types.SpectrumConfig
import Network.AWS.GroundStation.Types.TrackingConfig
import Network.AWS.GroundStation.Types.UplinkEchoConfig
import Network.AWS.GroundStation.Types.UplinkSpectrumConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2019-05-23@ of the Amazon Ground Station SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "GroundStation",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "groundstation",
      Core._serviceSigningName = "groundstation",
      Core._serviceVersion = "2019-05-23",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "GroundStation",
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

-- | One or more parameters are not valid.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"
    Prelude.. Core.hasStatus 431

-- | Dependency encountered an error.
_DependencyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DependencyException =
  Core._MatchServiceError
    defaultService
    "DependencyException"
    Prelude.. Core.hasStatus 531

-- | Account limits for this resource have been exceeded.
_ResourceLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceLimitExceededException"
    Prelude.. Core.hasStatus 429

-- | Resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 434
