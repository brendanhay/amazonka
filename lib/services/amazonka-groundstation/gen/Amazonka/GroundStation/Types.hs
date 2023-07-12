{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GroundStation.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _DependencyException,
    _InvalidParameterException,
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

    -- * EphemerisInvalidReason
    EphemerisInvalidReason (..),

    -- * EphemerisSource
    EphemerisSource (..),

    -- * EphemerisStatus
    EphemerisStatus (..),

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
    configDetails_endpointDetails,
    configDetails_s3RecordingDetails,

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
    configListItem_configId,
    configListItem_configType,
    configListItem_name,

    -- * ConfigTypeData
    ConfigTypeData (..),
    newConfigTypeData,
    configTypeData_antennaDownlinkConfig,
    configTypeData_antennaDownlinkDemodDecodeConfig,
    configTypeData_antennaUplinkConfig,
    configTypeData_dataflowEndpointConfig,
    configTypeData_s3RecordingConfig,
    configTypeData_trackingConfig,
    configTypeData_uplinkEchoConfig,

    -- * ContactData
    ContactData (..),
    newContactData,
    contactData_contactId,
    contactData_contactStatus,
    contactData_endTime,
    contactData_errorMessage,
    contactData_groundStation,
    contactData_maximumElevation,
    contactData_missionProfileArn,
    contactData_postPassEndTime,
    contactData_prePassStartTime,
    contactData_region,
    contactData_satelliteArn,
    contactData_startTime,
    contactData_tags,

    -- * ContactIdResponse
    ContactIdResponse (..),
    newContactIdResponse,
    contactIdResponse_contactId,

    -- * DataflowDetail
    DataflowDetail (..),
    newDataflowDetail,
    dataflowDetail_destination,
    dataflowDetail_errorMessage,
    dataflowDetail_source,

    -- * DataflowEndpoint
    DataflowEndpoint (..),
    newDataflowEndpoint,
    dataflowEndpoint_address,
    dataflowEndpoint_mtu,
    dataflowEndpoint_name,
    dataflowEndpoint_status,

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
    destination_configDetails,
    destination_configId,
    destination_configType,
    destination_dataflowDestinationRegion,

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
    endpointDetails_endpoint,
    endpointDetails_securityDetails,

    -- * EphemerisData
    EphemerisData (..),
    newEphemerisData,
    ephemerisData_oem,
    ephemerisData_tle,

    -- * EphemerisDescription
    EphemerisDescription (..),
    newEphemerisDescription,
    ephemerisDescription_ephemerisData,
    ephemerisDescription_sourceS3Object,

    -- * EphemerisIdResponse
    EphemerisIdResponse (..),
    newEphemerisIdResponse,
    ephemerisIdResponse_ephemerisId,

    -- * EphemerisItem
    EphemerisItem (..),
    newEphemerisItem,
    ephemerisItem_creationTime,
    ephemerisItem_enabled,
    ephemerisItem_ephemerisId,
    ephemerisItem_name,
    ephemerisItem_priority,
    ephemerisItem_sourceS3Object,
    ephemerisItem_status,

    -- * EphemerisMetaData
    EphemerisMetaData (..),
    newEphemerisMetaData,
    ephemerisMetaData_ephemerisId,
    ephemerisMetaData_epoch,
    ephemerisMetaData_name,
    ephemerisMetaData_source,

    -- * EphemerisTypeDescription
    EphemerisTypeDescription (..),
    newEphemerisTypeDescription,
    ephemerisTypeDescription_oem,
    ephemerisTypeDescription_tle,

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
    missionProfileListItem_missionProfileArn,
    missionProfileListItem_missionProfileId,
    missionProfileListItem_name,
    missionProfileListItem_region,

    -- * OEMEphemeris
    OEMEphemeris (..),
    newOEMEphemeris,
    oEMEphemeris_oemData,
    oEMEphemeris_s3Object,

    -- * S3Object
    S3Object (..),
    newS3Object,
    s3Object_bucket,
    s3Object_key,
    s3Object_version,

    -- * S3RecordingConfig
    S3RecordingConfig (..),
    newS3RecordingConfig,
    s3RecordingConfig_prefix,
    s3RecordingConfig_bucketArn,
    s3RecordingConfig_roleArn,

    -- * S3RecordingDetails
    S3RecordingDetails (..),
    newS3RecordingDetails,
    s3RecordingDetails_bucketArn,
    s3RecordingDetails_keyTemplate,

    -- * SatelliteListItem
    SatelliteListItem (..),
    newSatelliteListItem,
    satelliteListItem_currentEphemeris,
    satelliteListItem_groundStations,
    satelliteListItem_noradSatelliteID,
    satelliteListItem_satelliteArn,
    satelliteListItem_satelliteId,

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
    source_configDetails,
    source_configId,
    source_configType,
    source_dataflowSourceRegion,

    -- * SpectrumConfig
    SpectrumConfig (..),
    newSpectrumConfig,
    spectrumConfig_polarization,
    spectrumConfig_bandwidth,
    spectrumConfig_centerFrequency,

    -- * TLEData
    TLEData (..),
    newTLEData,
    tLEData_tleLine1,
    tLEData_tleLine2,
    tLEData_validTimeRange,

    -- * TLEEphemeris
    TLEEphemeris (..),
    newTLEEphemeris,
    tLEEphemeris_s3Object,
    tLEEphemeris_tleData,

    -- * TimeRange
    TimeRange (..),
    newTimeRange,
    timeRange_endTime,
    timeRange_startTime,

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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GroundStation.Types.AngleUnits
import Amazonka.GroundStation.Types.AntennaDemodDecodeDetails
import Amazonka.GroundStation.Types.AntennaDownlinkConfig
import Amazonka.GroundStation.Types.AntennaDownlinkDemodDecodeConfig
import Amazonka.GroundStation.Types.AntennaUplinkConfig
import Amazonka.GroundStation.Types.BandwidthUnits
import Amazonka.GroundStation.Types.ConfigCapabilityType
import Amazonka.GroundStation.Types.ConfigDetails
import Amazonka.GroundStation.Types.ConfigIdResponse
import Amazonka.GroundStation.Types.ConfigListItem
import Amazonka.GroundStation.Types.ConfigTypeData
import Amazonka.GroundStation.Types.ContactData
import Amazonka.GroundStation.Types.ContactIdResponse
import Amazonka.GroundStation.Types.ContactStatus
import Amazonka.GroundStation.Types.Criticality
import Amazonka.GroundStation.Types.DataflowDetail
import Amazonka.GroundStation.Types.DataflowEndpoint
import Amazonka.GroundStation.Types.DataflowEndpointConfig
import Amazonka.GroundStation.Types.DataflowEndpointGroupIdResponse
import Amazonka.GroundStation.Types.DataflowEndpointListItem
import Amazonka.GroundStation.Types.DecodeConfig
import Amazonka.GroundStation.Types.DemodulationConfig
import Amazonka.GroundStation.Types.Destination
import Amazonka.GroundStation.Types.Eirp
import Amazonka.GroundStation.Types.EirpUnits
import Amazonka.GroundStation.Types.Elevation
import Amazonka.GroundStation.Types.EndpointDetails
import Amazonka.GroundStation.Types.EndpointStatus
import Amazonka.GroundStation.Types.EphemerisData
import Amazonka.GroundStation.Types.EphemerisDescription
import Amazonka.GroundStation.Types.EphemerisIdResponse
import Amazonka.GroundStation.Types.EphemerisInvalidReason
import Amazonka.GroundStation.Types.EphemerisItem
import Amazonka.GroundStation.Types.EphemerisMetaData
import Amazonka.GroundStation.Types.EphemerisSource
import Amazonka.GroundStation.Types.EphemerisStatus
import Amazonka.GroundStation.Types.EphemerisTypeDescription
import Amazonka.GroundStation.Types.Frequency
import Amazonka.GroundStation.Types.FrequencyBandwidth
import Amazonka.GroundStation.Types.FrequencyUnits
import Amazonka.GroundStation.Types.GroundStationData
import Amazonka.GroundStation.Types.MissionProfileIdResponse
import Amazonka.GroundStation.Types.MissionProfileListItem
import Amazonka.GroundStation.Types.OEMEphemeris
import Amazonka.GroundStation.Types.Polarization
import Amazonka.GroundStation.Types.S3Object
import Amazonka.GroundStation.Types.S3RecordingConfig
import Amazonka.GroundStation.Types.S3RecordingDetails
import Amazonka.GroundStation.Types.SatelliteListItem
import Amazonka.GroundStation.Types.SecurityDetails
import Amazonka.GroundStation.Types.SocketAddress
import Amazonka.GroundStation.Types.Source
import Amazonka.GroundStation.Types.SpectrumConfig
import Amazonka.GroundStation.Types.TLEData
import Amazonka.GroundStation.Types.TLEEphemeris
import Amazonka.GroundStation.Types.TimeRange
import Amazonka.GroundStation.Types.TrackingConfig
import Amazonka.GroundStation.Types.UplinkEchoConfig
import Amazonka.GroundStation.Types.UplinkSpectrumConfig
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2019-05-23@ of the Amazon Ground Station SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "GroundStation",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "groundstation",
      Core.signingName = "groundstation",
      Core.version = "2019-05-23",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "GroundStation",
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

-- | Dependency encountered an error.
_DependencyException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DependencyException =
  Core._MatchServiceError
    defaultService
    "DependencyException"
    Prelude.. Core.hasStatus 531

-- | One or more parameters are not valid.
_InvalidParameterException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"
    Prelude.. Core.hasStatus 431

-- | Account limits for this resource have been exceeded.
_ResourceLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceLimitExceededException"
    Prelude.. Core.hasStatus 429

-- | Resource was not found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 434
