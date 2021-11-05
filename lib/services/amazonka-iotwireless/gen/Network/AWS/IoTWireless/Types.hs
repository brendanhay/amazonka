{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTWireless.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTWireless.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _TooManyTagsException,
    _ConflictException,
    _ThrottlingException,
    _InternalServerException,
    _ResourceNotFoundException,

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
    abpV1_0_x_devAddr,
    abpV1_0_x_sessionKeys,

    -- * AbpV1_1
    AbpV1_1 (..),
    newAbpV1_1,
    abpV1_1_devAddr,
    abpV1_1_sessionKeys,

    -- * CertificateList
    CertificateList (..),
    newCertificateList,
    certificateList_signingAlg,
    certificateList_value,

    -- * Destinations
    Destinations (..),
    newDestinations,
    destinations_expressionType,
    destinations_arn,
    destinations_name,
    destinations_expression,
    destinations_description,
    destinations_roleArn,

    -- * DeviceProfile
    DeviceProfile (..),
    newDeviceProfile,
    deviceProfile_arn,
    deviceProfile_name,
    deviceProfile_id,

    -- * LoRaWANDevice
    LoRaWANDevice (..),
    newLoRaWANDevice,
    loRaWANDevice_abpV1_0_x,
    loRaWANDevice_otaaV1_1,
    loRaWANDevice_serviceProfileId,
    loRaWANDevice_deviceProfileId,
    loRaWANDevice_otaaV1_0_x,
    loRaWANDevice_abpV1_1,
    loRaWANDevice_devEui,

    -- * LoRaWANDeviceMetadata
    LoRaWANDeviceMetadata (..),
    newLoRaWANDeviceMetadata,
    loRaWANDeviceMetadata_frequency,
    loRaWANDeviceMetadata_fPort,
    loRaWANDeviceMetadata_dataRate,
    loRaWANDeviceMetadata_gateways,
    loRaWANDeviceMetadata_timestamp,
    loRaWANDeviceMetadata_devEui,

    -- * LoRaWANDeviceProfile
    LoRaWANDeviceProfile (..),
    newLoRaWANDeviceProfile,
    loRaWANDeviceProfile_rfRegion,
    loRaWANDeviceProfile_pingSlotFreq,
    loRaWANDeviceProfile_classBTimeout,
    loRaWANDeviceProfile_maxEirp,
    loRaWANDeviceProfile_rxDataRate2,
    loRaWANDeviceProfile_macVersion,
    loRaWANDeviceProfile_factoryPresetFreqsList,
    loRaWANDeviceProfile_rxFreq2,
    loRaWANDeviceProfile_classCTimeout,
    loRaWANDeviceProfile_regParamsRevision,
    loRaWANDeviceProfile_pingSlotDr,
    loRaWANDeviceProfile_supports32BitFCnt,
    loRaWANDeviceProfile_maxDutyCycle,
    loRaWANDeviceProfile_rxDrOffset1,
    loRaWANDeviceProfile_supportsClassB,
    loRaWANDeviceProfile_supportsJoin,
    loRaWANDeviceProfile_rxDelay1,
    loRaWANDeviceProfile_supportsClassC,
    loRaWANDeviceProfile_pingSlotPeriod,

    -- * LoRaWANGateway
    LoRaWANGateway (..),
    newLoRaWANGateway,
    loRaWANGateway_rfRegion,
    loRaWANGateway_subBands,
    loRaWANGateway_gatewayEui,
    loRaWANGateway_joinEuiFilters,
    loRaWANGateway_netIdFilters,

    -- * LoRaWANGatewayCurrentVersion
    LoRaWANGatewayCurrentVersion (..),
    newLoRaWANGatewayCurrentVersion,
    loRaWANGatewayCurrentVersion_currentVersion,

    -- * LoRaWANGatewayMetadata
    LoRaWANGatewayMetadata (..),
    newLoRaWANGatewayMetadata,
    loRaWANGatewayMetadata_gatewayEui,
    loRaWANGatewayMetadata_snr,
    loRaWANGatewayMetadata_rssi,

    -- * LoRaWANGatewayVersion
    LoRaWANGatewayVersion (..),
    newLoRaWANGatewayVersion,
    loRaWANGatewayVersion_packageVersion,
    loRaWANGatewayVersion_model,
    loRaWANGatewayVersion_station,

    -- * LoRaWANGetServiceProfileInfo
    LoRaWANGetServiceProfileInfo (..),
    newLoRaWANGetServiceProfileInfo,
    loRaWANGetServiceProfileInfo_addGwMetadata,
    loRaWANGetServiceProfileInfo_prAllowed,
    loRaWANGetServiceProfileInfo_drMax,
    loRaWANGetServiceProfileInfo_dlRate,
    loRaWANGetServiceProfileInfo_devStatusReqFreq,
    loRaWANGetServiceProfileInfo_reportDevStatusBattery,
    loRaWANGetServiceProfileInfo_dlBucketSize,
    loRaWANGetServiceProfileInfo_ulRatePolicy,
    loRaWANGetServiceProfileInfo_targetPer,
    loRaWANGetServiceProfileInfo_drMin,
    loRaWANGetServiceProfileInfo_nwkGeoLoc,
    loRaWANGetServiceProfileInfo_channelMask,
    loRaWANGetServiceProfileInfo_dlRatePolicy,
    loRaWANGetServiceProfileInfo_hrAllowed,
    loRaWANGetServiceProfileInfo_ulBucketSize,
    loRaWANGetServiceProfileInfo_ulRate,
    loRaWANGetServiceProfileInfo_reportDevStatusMargin,
    loRaWANGetServiceProfileInfo_raAllowed,
    loRaWANGetServiceProfileInfo_minGwDiversity,

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
    loRaWANUpdateDevice_serviceProfileId,
    loRaWANUpdateDevice_deviceProfileId,

    -- * LoRaWANUpdateGatewayTaskCreate
    LoRaWANUpdateGatewayTaskCreate (..),
    newLoRaWANUpdateGatewayTaskCreate,
    loRaWANUpdateGatewayTaskCreate_updateSignature,
    loRaWANUpdateGatewayTaskCreate_sigKeyCrc,
    loRaWANUpdateGatewayTaskCreate_currentVersion,
    loRaWANUpdateGatewayTaskCreate_updateVersion,

    -- * LoRaWANUpdateGatewayTaskEntry
    LoRaWANUpdateGatewayTaskEntry (..),
    newLoRaWANUpdateGatewayTaskEntry,
    loRaWANUpdateGatewayTaskEntry_currentVersion,
    loRaWANUpdateGatewayTaskEntry_updateVersion,

    -- * OtaaV1_0_x
    OtaaV1_0_x (..),
    newOtaaV1_0_x,
    otaaV1_0_x_appEui,
    otaaV1_0_x_appKey,

    -- * OtaaV1_1
    OtaaV1_1 (..),
    newOtaaV1_1,
    otaaV1_1_joinEui,
    otaaV1_1_nwkKey,
    otaaV1_1_appKey,

    -- * ServiceProfile
    ServiceProfile (..),
    newServiceProfile,
    serviceProfile_arn,
    serviceProfile_name,
    serviceProfile_id,

    -- * SessionKeysAbpV1_0_x
    SessionKeysAbpV1_0_x (..),
    newSessionKeysAbpV1_0_x,
    sessionKeysAbpV1_0_x_nwkSKey,
    sessionKeysAbpV1_0_x_appSKey,

    -- * SessionKeysAbpV1_1
    SessionKeysAbpV1_1 (..),
    newSessionKeysAbpV1_1,
    sessionKeysAbpV1_1_fNwkSIntKey,
    sessionKeysAbpV1_1_sNwkSIntKey,
    sessionKeysAbpV1_1_nwkSEncKey,
    sessionKeysAbpV1_1_appSKey,

    -- * SidewalkAccountInfo
    SidewalkAccountInfo (..),
    newSidewalkAccountInfo,
    sidewalkAccountInfo_amazonId,
    sidewalkAccountInfo_appServerPrivateKey,

    -- * SidewalkAccountInfoWithFingerprint
    SidewalkAccountInfoWithFingerprint (..),
    newSidewalkAccountInfoWithFingerprint,
    sidewalkAccountInfoWithFingerprint_arn,
    sidewalkAccountInfoWithFingerprint_fingerprint,
    sidewalkAccountInfoWithFingerprint_amazonId,

    -- * SidewalkDevice
    SidewalkDevice (..),
    newSidewalkDevice,
    sidewalkDevice_sidewalkManufacturingSn,
    sidewalkDevice_amazonId,
    sidewalkDevice_deviceCertificates,
    sidewalkDevice_sidewalkId,

    -- * SidewalkDeviceMetadata
    SidewalkDeviceMetadata (..),
    newSidewalkDeviceMetadata,
    sidewalkDeviceMetadata_event,
    sidewalkDeviceMetadata_deviceState,
    sidewalkDeviceMetadata_batteryLevel,
    sidewalkDeviceMetadata_rssi,

    -- * SidewalkListDevice
    SidewalkListDevice (..),
    newSidewalkListDevice,
    sidewalkListDevice_sidewalkManufacturingSn,
    sidewalkListDevice_amazonId,
    sidewalkListDevice_deviceCertificates,
    sidewalkListDevice_sidewalkId,

    -- * SidewalkSendDataToDevice
    SidewalkSendDataToDevice (..),
    newSidewalkSendDataToDevice,
    sidewalkSendDataToDevice_messageType,
    sidewalkSendDataToDevice_seq,

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
    updateWirelessGatewayTaskCreate_updateDataSource,
    updateWirelessGatewayTaskCreate_updateDataRole,
    updateWirelessGatewayTaskCreate_loRaWAN,

    -- * UpdateWirelessGatewayTaskEntry
    UpdateWirelessGatewayTaskEntry (..),
    newUpdateWirelessGatewayTaskEntry,
    updateWirelessGatewayTaskEntry_arn,
    updateWirelessGatewayTaskEntry_loRaWAN,
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
    wirelessDeviceStatistics_sidewalk,
    wirelessDeviceStatistics_arn,
    wirelessDeviceStatistics_loRaWAN,
    wirelessDeviceStatistics_name,
    wirelessDeviceStatistics_id,
    wirelessDeviceStatistics_lastUplinkReceivedAt,
    wirelessDeviceStatistics_type,
    wirelessDeviceStatistics_destinationName,

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
    wirelessGatewayStatistics_arn,
    wirelessGatewayStatistics_loRaWAN,
    wirelessGatewayStatistics_name,
    wirelessGatewayStatistics_id,
    wirelessGatewayStatistics_lastUplinkReceivedAt,
    wirelessGatewayStatistics_description,

    -- * WirelessMetadata
    WirelessMetadata (..),
    newWirelessMetadata,
    wirelessMetadata_sidewalk,
    wirelessMetadata_loRaWAN,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTWireless.Types.AbpV1_0_x
import Network.AWS.IoTWireless.Types.AbpV1_1
import Network.AWS.IoTWireless.Types.BatteryLevel
import Network.AWS.IoTWireless.Types.CertificateList
import Network.AWS.IoTWireless.Types.ConnectionStatus
import Network.AWS.IoTWireless.Types.Destinations
import Network.AWS.IoTWireless.Types.DeviceProfile
import Network.AWS.IoTWireless.Types.DeviceState
import Network.AWS.IoTWireless.Types.Event
import Network.AWS.IoTWireless.Types.ExpressionType
import Network.AWS.IoTWireless.Types.LoRaWANDevice
import Network.AWS.IoTWireless.Types.LoRaWANDeviceMetadata
import Network.AWS.IoTWireless.Types.LoRaWANDeviceProfile
import Network.AWS.IoTWireless.Types.LoRaWANGateway
import Network.AWS.IoTWireless.Types.LoRaWANGatewayCurrentVersion
import Network.AWS.IoTWireless.Types.LoRaWANGatewayMetadata
import Network.AWS.IoTWireless.Types.LoRaWANGatewayVersion
import Network.AWS.IoTWireless.Types.LoRaWANGetServiceProfileInfo
import Network.AWS.IoTWireless.Types.LoRaWANListDevice
import Network.AWS.IoTWireless.Types.LoRaWANSendDataToDevice
import Network.AWS.IoTWireless.Types.LoRaWANServiceProfile
import Network.AWS.IoTWireless.Types.LoRaWANUpdateDevice
import Network.AWS.IoTWireless.Types.LoRaWANUpdateGatewayTaskCreate
import Network.AWS.IoTWireless.Types.LoRaWANUpdateGatewayTaskEntry
import Network.AWS.IoTWireless.Types.LogLevel
import Network.AWS.IoTWireless.Types.MessageType
import Network.AWS.IoTWireless.Types.OtaaV1_0_x
import Network.AWS.IoTWireless.Types.OtaaV1_1
import Network.AWS.IoTWireless.Types.PartnerType
import Network.AWS.IoTWireless.Types.ServiceProfile
import Network.AWS.IoTWireless.Types.SessionKeysAbpV1_0_x
import Network.AWS.IoTWireless.Types.SessionKeysAbpV1_1
import Network.AWS.IoTWireless.Types.SidewalkAccountInfo
import Network.AWS.IoTWireless.Types.SidewalkAccountInfoWithFingerprint
import Network.AWS.IoTWireless.Types.SidewalkDevice
import Network.AWS.IoTWireless.Types.SidewalkDeviceMetadata
import Network.AWS.IoTWireless.Types.SidewalkListDevice
import Network.AWS.IoTWireless.Types.SidewalkSendDataToDevice
import Network.AWS.IoTWireless.Types.SidewalkUpdateAccount
import Network.AWS.IoTWireless.Types.SigningAlg
import Network.AWS.IoTWireless.Types.Tag
import Network.AWS.IoTWireless.Types.UpdateWirelessGatewayTaskCreate
import Network.AWS.IoTWireless.Types.UpdateWirelessGatewayTaskEntry
import Network.AWS.IoTWireless.Types.WirelessDeviceEvent
import Network.AWS.IoTWireless.Types.WirelessDeviceEventLogOption
import Network.AWS.IoTWireless.Types.WirelessDeviceIdType
import Network.AWS.IoTWireless.Types.WirelessDeviceLogOption
import Network.AWS.IoTWireless.Types.WirelessDeviceStatistics
import Network.AWS.IoTWireless.Types.WirelessDeviceType
import Network.AWS.IoTWireless.Types.WirelessGatewayEvent
import Network.AWS.IoTWireless.Types.WirelessGatewayEventLogOption
import Network.AWS.IoTWireless.Types.WirelessGatewayIdType
import Network.AWS.IoTWireless.Types.WirelessGatewayLogOption
import Network.AWS.IoTWireless.Types.WirelessGatewayServiceType
import Network.AWS.IoTWireless.Types.WirelessGatewayStatistics
import Network.AWS.IoTWireless.Types.WirelessGatewayTaskDefinitionType
import Network.AWS.IoTWireless.Types.WirelessGatewayTaskStatus
import Network.AWS.IoTWireless.Types.WirelessGatewayType
import Network.AWS.IoTWireless.Types.WirelessMetadata
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

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

-- | The input did not meet the specified constraints.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | User does not have permission to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The request was denied because the resource can\'t have any more tags.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"
    Prelude.. Core.hasStatus 400

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

-- | An unexpected error occurred while processing a request.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | Resource does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
