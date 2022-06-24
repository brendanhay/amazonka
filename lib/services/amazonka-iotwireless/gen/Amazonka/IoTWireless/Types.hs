{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTWireless.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InternalServerException,
    _TooManyTagsException,
    _ResourceNotFoundException,
    _ConflictException,
    _ThrottlingException,
    _ValidationException,

    -- * BatteryLevel
    BatteryLevel (..),

    -- * ConnectionStatus
    ConnectionStatus (..),

    -- * DeviceState
    DeviceState (..),

    -- * Event
    Event (..),

    -- * ExpressionType
    ExpressionType (..),

    -- * LogLevel
    LogLevel (..),

    -- * MessageType
    MessageType (..),

    -- * PartnerType
    PartnerType (..),

    -- * SigningAlg
    SigningAlg (..),

    -- * WirelessDeviceEvent
    WirelessDeviceEvent (..),

    -- * WirelessDeviceIdType
    WirelessDeviceIdType (..),

    -- * WirelessDeviceType
    WirelessDeviceType (..),

    -- * WirelessGatewayEvent
    WirelessGatewayEvent (..),

    -- * WirelessGatewayIdType
    WirelessGatewayIdType (..),

    -- * WirelessGatewayServiceType
    WirelessGatewayServiceType (..),

    -- * WirelessGatewayTaskDefinitionType
    WirelessGatewayTaskDefinitionType (..),

    -- * WirelessGatewayTaskStatus
    WirelessGatewayTaskStatus (..),

    -- * WirelessGatewayType
    WirelessGatewayType (..),

    -- * AbpV1_0_x
    AbpV1_0_x (..),
    newAbpV1_0_x,
    abpV1_0_x_sessionKeys,
    abpV1_0_x_devAddr,

    -- * AbpV1_1
    AbpV1_1 (..),
    newAbpV1_1,
    abpV1_1_sessionKeys,
    abpV1_1_devAddr,

    -- * CertificateList
    CertificateList (..),
    newCertificateList,
    certificateList_signingAlg,
    certificateList_value,

    -- * Destinations
    Destinations (..),
    newDestinations,
    destinations_name,
    destinations_roleArn,
    destinations_arn,
    destinations_description,
    destinations_expression,
    destinations_expressionType,

    -- * DeviceProfile
    DeviceProfile (..),
    newDeviceProfile,
    deviceProfile_name,
    deviceProfile_arn,
    deviceProfile_id,

    -- * LoRaWANDevice
    LoRaWANDevice (..),
    newLoRaWANDevice,
    loRaWANDevice_otaaV1_1,
    loRaWANDevice_otaaV1_0_x,
    loRaWANDevice_abpV1_0_x,
    loRaWANDevice_deviceProfileId,
    loRaWANDevice_serviceProfileId,
    loRaWANDevice_abpV1_1,
    loRaWANDevice_devEui,

    -- * LoRaWANDeviceMetadata
    LoRaWANDeviceMetadata (..),
    newLoRaWANDeviceMetadata,
    loRaWANDeviceMetadata_fPort,
    loRaWANDeviceMetadata_frequency,
    loRaWANDeviceMetadata_gateways,
    loRaWANDeviceMetadata_timestamp,
    loRaWANDeviceMetadata_dataRate,
    loRaWANDeviceMetadata_devEui,

    -- * LoRaWANDeviceProfile
    LoRaWANDeviceProfile (..),
    newLoRaWANDeviceProfile,
    loRaWANDeviceProfile_rxDataRate2,
    loRaWANDeviceProfile_rfRegion,
    loRaWANDeviceProfile_supportsJoin,
    loRaWANDeviceProfile_supportsClassC,
    loRaWANDeviceProfile_maxEirp,
    loRaWANDeviceProfile_factoryPresetFreqsList,
    loRaWANDeviceProfile_maxDutyCycle,
    loRaWANDeviceProfile_regParamsRevision,
    loRaWANDeviceProfile_pingSlotDr,
    loRaWANDeviceProfile_rxDelay1,
    loRaWANDeviceProfile_classBTimeout,
    loRaWANDeviceProfile_pingSlotPeriod,
    loRaWANDeviceProfile_pingSlotFreq,
    loRaWANDeviceProfile_classCTimeout,
    loRaWANDeviceProfile_rxFreq2,
    loRaWANDeviceProfile_supportsClassB,
    loRaWANDeviceProfile_rxDrOffset1,
    loRaWANDeviceProfile_supports32BitFCnt,
    loRaWANDeviceProfile_macVersion,

    -- * LoRaWANGateway
    LoRaWANGateway (..),
    newLoRaWANGateway,
    loRaWANGateway_rfRegion,
    loRaWANGateway_subBands,
    loRaWANGateway_gatewayEui,
    loRaWANGateway_netIdFilters,
    loRaWANGateway_joinEuiFilters,

    -- * LoRaWANGatewayCurrentVersion
    LoRaWANGatewayCurrentVersion (..),
    newLoRaWANGatewayCurrentVersion,
    loRaWANGatewayCurrentVersion_currentVersion,

    -- * LoRaWANGatewayMetadata
    LoRaWANGatewayMetadata (..),
    newLoRaWANGatewayMetadata,
    loRaWANGatewayMetadata_snr,
    loRaWANGatewayMetadata_gatewayEui,
    loRaWANGatewayMetadata_rssi,

    -- * LoRaWANGatewayVersion
    LoRaWANGatewayVersion (..),
    newLoRaWANGatewayVersion,
    loRaWANGatewayVersion_model,
    loRaWANGatewayVersion_station,
    loRaWANGatewayVersion_packageVersion,

    -- * LoRaWANGetServiceProfileInfo
    LoRaWANGetServiceProfileInfo (..),
    newLoRaWANGetServiceProfileInfo,
    loRaWANGetServiceProfileInfo_hrAllowed,
    loRaWANGetServiceProfileInfo_targetPer,
    loRaWANGetServiceProfileInfo_nwkGeoLoc,
    loRaWANGetServiceProfileInfo_devStatusReqFreq,
    loRaWANGetServiceProfileInfo_prAllowed,
    loRaWANGetServiceProfileInfo_dlRatePolicy,
    loRaWANGetServiceProfileInfo_addGwMetadata,
    loRaWANGetServiceProfileInfo_ulRatePolicy,
    loRaWANGetServiceProfileInfo_ulRate,
    loRaWANGetServiceProfileInfo_raAllowed,
    loRaWANGetServiceProfileInfo_minGwDiversity,
    loRaWANGetServiceProfileInfo_reportDevStatusMargin,
    loRaWANGetServiceProfileInfo_channelMask,
    loRaWANGetServiceProfileInfo_reportDevStatusBattery,
    loRaWANGetServiceProfileInfo_ulBucketSize,
    loRaWANGetServiceProfileInfo_dlRate,
    loRaWANGetServiceProfileInfo_drMax,
    loRaWANGetServiceProfileInfo_dlBucketSize,
    loRaWANGetServiceProfileInfo_drMin,

    -- * LoRaWANListDevice
    LoRaWANListDevice (..),
    newLoRaWANListDevice,
    loRaWANListDevice_devEui,

    -- * LoRaWANSendDataToDevice
    LoRaWANSendDataToDevice (..),
    newLoRaWANSendDataToDevice,
    loRaWANSendDataToDevice_fPort,

    -- * LoRaWANServiceProfile
    LoRaWANServiceProfile (..),
    newLoRaWANServiceProfile,
    loRaWANServiceProfile_addGwMetadata,

    -- * LoRaWANUpdateDevice
    LoRaWANUpdateDevice (..),
    newLoRaWANUpdateDevice,
    loRaWANUpdateDevice_deviceProfileId,
    loRaWANUpdateDevice_serviceProfileId,

    -- * LoRaWANUpdateGatewayTaskCreate
    LoRaWANUpdateGatewayTaskCreate (..),
    newLoRaWANUpdateGatewayTaskCreate,
    loRaWANUpdateGatewayTaskCreate_sigKeyCrc,
    loRaWANUpdateGatewayTaskCreate_currentVersion,
    loRaWANUpdateGatewayTaskCreate_updateSignature,
    loRaWANUpdateGatewayTaskCreate_updateVersion,

    -- * LoRaWANUpdateGatewayTaskEntry
    LoRaWANUpdateGatewayTaskEntry (..),
    newLoRaWANUpdateGatewayTaskEntry,
    loRaWANUpdateGatewayTaskEntry_currentVersion,
    loRaWANUpdateGatewayTaskEntry_updateVersion,

    -- * OtaaV1_0_x
    OtaaV1_0_x (..),
    newOtaaV1_0_x,
    otaaV1_0_x_appKey,
    otaaV1_0_x_appEui,

    -- * OtaaV1_1
    OtaaV1_1 (..),
    newOtaaV1_1,
    otaaV1_1_appKey,
    otaaV1_1_nwkKey,
    otaaV1_1_joinEui,

    -- * ServiceProfile
    ServiceProfile (..),
    newServiceProfile,
    serviceProfile_name,
    serviceProfile_arn,
    serviceProfile_id,

    -- * SessionKeysAbpV1_0_x
    SessionKeysAbpV1_0_x (..),
    newSessionKeysAbpV1_0_x,
    sessionKeysAbpV1_0_x_nwkSKey,
    sessionKeysAbpV1_0_x_appSKey,

    -- * SessionKeysAbpV1_1
    SessionKeysAbpV1_1 (..),
    newSessionKeysAbpV1_1,
    sessionKeysAbpV1_1_nwkSEncKey,
    sessionKeysAbpV1_1_fNwkSIntKey,
    sessionKeysAbpV1_1_sNwkSIntKey,
    sessionKeysAbpV1_1_appSKey,

    -- * SidewalkAccountInfo
    SidewalkAccountInfo (..),
    newSidewalkAccountInfo,
    sidewalkAccountInfo_appServerPrivateKey,
    sidewalkAccountInfo_amazonId,

    -- * SidewalkAccountInfoWithFingerprint
    SidewalkAccountInfoWithFingerprint (..),
    newSidewalkAccountInfoWithFingerprint,
    sidewalkAccountInfoWithFingerprint_arn,
    sidewalkAccountInfoWithFingerprint_amazonId,
    sidewalkAccountInfoWithFingerprint_fingerprint,

    -- * SidewalkDevice
    SidewalkDevice (..),
    newSidewalkDevice,
    sidewalkDevice_deviceCertificates,
    sidewalkDevice_sidewalkId,
    sidewalkDevice_amazonId,
    sidewalkDevice_sidewalkManufacturingSn,

    -- * SidewalkDeviceMetadata
    SidewalkDeviceMetadata (..),
    newSidewalkDeviceMetadata,
    sidewalkDeviceMetadata_deviceState,
    sidewalkDeviceMetadata_batteryLevel,
    sidewalkDeviceMetadata_event,
    sidewalkDeviceMetadata_rssi,

    -- * SidewalkListDevice
    SidewalkListDevice (..),
    newSidewalkListDevice,
    sidewalkListDevice_deviceCertificates,
    sidewalkListDevice_sidewalkId,
    sidewalkListDevice_amazonId,
    sidewalkListDevice_sidewalkManufacturingSn,

    -- * SidewalkSendDataToDevice
    SidewalkSendDataToDevice (..),
    newSidewalkSendDataToDevice,
    sidewalkSendDataToDevice_seq,
    sidewalkSendDataToDevice_messageType,

    -- * SidewalkUpdateAccount
    SidewalkUpdateAccount (..),
    newSidewalkUpdateAccount,
    sidewalkUpdateAccount_appServerPrivateKey,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * UpdateWirelessGatewayTaskCreate
    UpdateWirelessGatewayTaskCreate (..),
    newUpdateWirelessGatewayTaskCreate,
    updateWirelessGatewayTaskCreate_updateDataRole,
    updateWirelessGatewayTaskCreate_loRaWAN,
    updateWirelessGatewayTaskCreate_updateDataSource,

    -- * UpdateWirelessGatewayTaskEntry
    UpdateWirelessGatewayTaskEntry (..),
    newUpdateWirelessGatewayTaskEntry,
    updateWirelessGatewayTaskEntry_loRaWAN,
    updateWirelessGatewayTaskEntry_arn,
    updateWirelessGatewayTaskEntry_id,

    -- * WirelessDeviceEventLogOption
    WirelessDeviceEventLogOption (..),
    newWirelessDeviceEventLogOption,
    wirelessDeviceEventLogOption_event,
    wirelessDeviceEventLogOption_logLevel,

    -- * WirelessDeviceLogOption
    WirelessDeviceLogOption (..),
    newWirelessDeviceLogOption,
    wirelessDeviceLogOption_events,
    wirelessDeviceLogOption_type,
    wirelessDeviceLogOption_logLevel,

    -- * WirelessDeviceStatistics
    WirelessDeviceStatistics (..),
    newWirelessDeviceStatistics,
    wirelessDeviceStatistics_name,
    wirelessDeviceStatistics_type,
    wirelessDeviceStatistics_lastUplinkReceivedAt,
    wirelessDeviceStatistics_loRaWAN,
    wirelessDeviceStatistics_destinationName,
    wirelessDeviceStatistics_arn,
    wirelessDeviceStatistics_id,
    wirelessDeviceStatistics_sidewalk,

    -- * WirelessGatewayEventLogOption
    WirelessGatewayEventLogOption (..),
    newWirelessGatewayEventLogOption,
    wirelessGatewayEventLogOption_event,
    wirelessGatewayEventLogOption_logLevel,

    -- * WirelessGatewayLogOption
    WirelessGatewayLogOption (..),
    newWirelessGatewayLogOption,
    wirelessGatewayLogOption_events,
    wirelessGatewayLogOption_type,
    wirelessGatewayLogOption_logLevel,

    -- * WirelessGatewayStatistics
    WirelessGatewayStatistics (..),
    newWirelessGatewayStatistics,
    wirelessGatewayStatistics_name,
    wirelessGatewayStatistics_lastUplinkReceivedAt,
    wirelessGatewayStatistics_loRaWAN,
    wirelessGatewayStatistics_arn,
    wirelessGatewayStatistics_id,
    wirelessGatewayStatistics_description,

    -- * WirelessMetadata
    WirelessMetadata (..),
    newWirelessMetadata,
    wirelessMetadata_loRaWAN,
    wirelessMetadata_sidewalk,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTWireless.Types.AbpV1_0_x
import Amazonka.IoTWireless.Types.AbpV1_1
import Amazonka.IoTWireless.Types.BatteryLevel
import Amazonka.IoTWireless.Types.CertificateList
import Amazonka.IoTWireless.Types.ConnectionStatus
import Amazonka.IoTWireless.Types.Destinations
import Amazonka.IoTWireless.Types.DeviceProfile
import Amazonka.IoTWireless.Types.DeviceState
import Amazonka.IoTWireless.Types.Event
import Amazonka.IoTWireless.Types.ExpressionType
import Amazonka.IoTWireless.Types.LoRaWANDevice
import Amazonka.IoTWireless.Types.LoRaWANDeviceMetadata
import Amazonka.IoTWireless.Types.LoRaWANDeviceProfile
import Amazonka.IoTWireless.Types.LoRaWANGateway
import Amazonka.IoTWireless.Types.LoRaWANGatewayCurrentVersion
import Amazonka.IoTWireless.Types.LoRaWANGatewayMetadata
import Amazonka.IoTWireless.Types.LoRaWANGatewayVersion
import Amazonka.IoTWireless.Types.LoRaWANGetServiceProfileInfo
import Amazonka.IoTWireless.Types.LoRaWANListDevice
import Amazonka.IoTWireless.Types.LoRaWANSendDataToDevice
import Amazonka.IoTWireless.Types.LoRaWANServiceProfile
import Amazonka.IoTWireless.Types.LoRaWANUpdateDevice
import Amazonka.IoTWireless.Types.LoRaWANUpdateGatewayTaskCreate
import Amazonka.IoTWireless.Types.LoRaWANUpdateGatewayTaskEntry
import Amazonka.IoTWireless.Types.LogLevel
import Amazonka.IoTWireless.Types.MessageType
import Amazonka.IoTWireless.Types.OtaaV1_0_x
import Amazonka.IoTWireless.Types.OtaaV1_1
import Amazonka.IoTWireless.Types.PartnerType
import Amazonka.IoTWireless.Types.ServiceProfile
import Amazonka.IoTWireless.Types.SessionKeysAbpV1_0_x
import Amazonka.IoTWireless.Types.SessionKeysAbpV1_1
import Amazonka.IoTWireless.Types.SidewalkAccountInfo
import Amazonka.IoTWireless.Types.SidewalkAccountInfoWithFingerprint
import Amazonka.IoTWireless.Types.SidewalkDevice
import Amazonka.IoTWireless.Types.SidewalkDeviceMetadata
import Amazonka.IoTWireless.Types.SidewalkListDevice
import Amazonka.IoTWireless.Types.SidewalkSendDataToDevice
import Amazonka.IoTWireless.Types.SidewalkUpdateAccount
import Amazonka.IoTWireless.Types.SigningAlg
import Amazonka.IoTWireless.Types.Tag
import Amazonka.IoTWireless.Types.UpdateWirelessGatewayTaskCreate
import Amazonka.IoTWireless.Types.UpdateWirelessGatewayTaskEntry
import Amazonka.IoTWireless.Types.WirelessDeviceEvent
import Amazonka.IoTWireless.Types.WirelessDeviceEventLogOption
import Amazonka.IoTWireless.Types.WirelessDeviceIdType
import Amazonka.IoTWireless.Types.WirelessDeviceLogOption
import Amazonka.IoTWireless.Types.WirelessDeviceStatistics
import Amazonka.IoTWireless.Types.WirelessDeviceType
import Amazonka.IoTWireless.Types.WirelessGatewayEvent
import Amazonka.IoTWireless.Types.WirelessGatewayEventLogOption
import Amazonka.IoTWireless.Types.WirelessGatewayIdType
import Amazonka.IoTWireless.Types.WirelessGatewayLogOption
import Amazonka.IoTWireless.Types.WirelessGatewayServiceType
import Amazonka.IoTWireless.Types.WirelessGatewayStatistics
import Amazonka.IoTWireless.Types.WirelessGatewayTaskDefinitionType
import Amazonka.IoTWireless.Types.WirelessGatewayTaskStatus
import Amazonka.IoTWireless.Types.WirelessGatewayType
import Amazonka.IoTWireless.Types.WirelessMetadata
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-11-22@ of the Amazon IoT Wireless SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "IoTWireless",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "api.iotwireless",
      Core._serviceSigningName = "iotwireless",
      Core._serviceVersion = "2020-11-22",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "IoTWireless",
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | User does not have permission to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | An unexpected error occurred while processing a request.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The request was denied because the resource can\'t have any more tags.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"
    Prelude.. Core.hasStatus 400

-- | Resource does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Adding, updating, or deleting the resource can cause an inconsistent
-- state.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The request was denied because it exceeded the allowed API request rate.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The input did not meet the specified constraints.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
