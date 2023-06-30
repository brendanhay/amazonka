{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTWireless.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ThrottlingException,
    _TooManyTagsException,
    _ValidationException,

    -- * ApplicationConfigType
    ApplicationConfigType (..),

    -- * BatteryLevel
    BatteryLevel (..),

    -- * ConnectionStatus
    ConnectionStatus (..),

    -- * DeviceState
    DeviceState (..),

    -- * DlClass
    DlClass (..),

    -- * DownlinkMode
    DownlinkMode (..),

    -- * Event
    Event (..),

    -- * EventNotificationPartnerType
    EventNotificationPartnerType (..),

    -- * EventNotificationResourceType
    EventNotificationResourceType (..),

    -- * EventNotificationTopicStatus
    EventNotificationTopicStatus (..),

    -- * ExpressionType
    ExpressionType (..),

    -- * FuotaDeviceStatus
    FuotaDeviceStatus (..),

    -- * FuotaTaskStatus
    FuotaTaskStatus (..),

    -- * IdentifierType
    IdentifierType (..),

    -- * LogLevel
    LogLevel (..),

    -- * MessageType
    MessageType (..),

    -- * PartnerType
    PartnerType (..),

    -- * PositionResourceType
    PositionResourceType (..),

    -- * PositioningConfigStatus
    PositioningConfigStatus (..),

    -- * SigningAlg
    SigningAlg (..),

    -- * SupportedRfRegion
    SupportedRfRegion (..),

    -- * WirelessDeviceEvent
    WirelessDeviceEvent (..),

    -- * WirelessDeviceFrameInfo
    WirelessDeviceFrameInfo (..),

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
    abpV1_0_x_fCntStart,
    abpV1_0_x_sessionKeys,

    -- * AbpV1_1
    AbpV1_1 (..),
    newAbpV1_1,
    abpV1_1_devAddr,
    abpV1_1_fCntStart,
    abpV1_1_sessionKeys,

    -- * ApplicationConfig
    ApplicationConfig (..),
    newApplicationConfig,
    applicationConfig_destinationName,
    applicationConfig_fPort,
    applicationConfig_type,

    -- * Beaconing
    Beaconing (..),
    newBeaconing,
    beaconing_dataRate,
    beaconing_frequencies,

    -- * CdmaLocalId
    CdmaLocalId (..),
    newCdmaLocalId,
    cdmaLocalId_pnOffset,
    cdmaLocalId_cdmaChannel,

    -- * CdmaNmrObj
    CdmaNmrObj (..),
    newCdmaNmrObj,
    cdmaNmrObj_baseStationId,
    cdmaNmrObj_pilotPower,
    cdmaNmrObj_pnOffset,
    cdmaNmrObj_cdmaChannel,

    -- * CdmaObj
    CdmaObj (..),
    newCdmaObj,
    cdmaObj_baseLat,
    cdmaObj_baseLng,
    cdmaObj_cdmaLocalId,
    cdmaObj_cdmaNmr,
    cdmaObj_pilotPower,
    cdmaObj_registrationZone,
    cdmaObj_systemId,
    cdmaObj_networkId,
    cdmaObj_baseStationId,

    -- * CellTowers
    CellTowers (..),
    newCellTowers,
    cellTowers_cdma,
    cellTowers_gsm,
    cellTowers_lte,
    cellTowers_tdscdma,
    cellTowers_wcdma,

    -- * CertificateList
    CertificateList (..),
    newCertificateList,
    certificateList_signingAlg,
    certificateList_value,

    -- * ConnectionStatusEventConfiguration
    ConnectionStatusEventConfiguration (..),
    newConnectionStatusEventConfiguration,
    connectionStatusEventConfiguration_loRaWAN,
    connectionStatusEventConfiguration_wirelessGatewayIdEventTopic,

    -- * ConnectionStatusResourceTypeEventConfiguration
    ConnectionStatusResourceTypeEventConfiguration (..),
    newConnectionStatusResourceTypeEventConfiguration,
    connectionStatusResourceTypeEventConfiguration_loRaWAN,

    -- * Destinations
    Destinations (..),
    newDestinations,
    destinations_arn,
    destinations_description,
    destinations_expression,
    destinations_expressionType,
    destinations_name,
    destinations_roleArn,

    -- * DeviceProfile
    DeviceProfile (..),
    newDeviceProfile,
    deviceProfile_arn,
    deviceProfile_id,
    deviceProfile_name,

    -- * DeviceRegistrationStateEventConfiguration
    DeviceRegistrationStateEventConfiguration (..),
    newDeviceRegistrationStateEventConfiguration,
    deviceRegistrationStateEventConfiguration_sidewalk,
    deviceRegistrationStateEventConfiguration_wirelessDeviceIdEventTopic,

    -- * DeviceRegistrationStateResourceTypeEventConfiguration
    DeviceRegistrationStateResourceTypeEventConfiguration (..),
    newDeviceRegistrationStateResourceTypeEventConfiguration,
    deviceRegistrationStateResourceTypeEventConfiguration_sidewalk,

    -- * DownlinkQueueMessage
    DownlinkQueueMessage (..),
    newDownlinkQueueMessage,
    downlinkQueueMessage_loRaWAN,
    downlinkQueueMessage_messageId,
    downlinkQueueMessage_receivedAt,
    downlinkQueueMessage_transmitMode,

    -- * EventConfigurationItem
    EventConfigurationItem (..),
    newEventConfigurationItem,
    eventConfigurationItem_events,
    eventConfigurationItem_identifier,
    eventConfigurationItem_identifierType,
    eventConfigurationItem_partnerType,

    -- * EventNotificationItemConfigurations
    EventNotificationItemConfigurations (..),
    newEventNotificationItemConfigurations,
    eventNotificationItemConfigurations_connectionStatus,
    eventNotificationItemConfigurations_deviceRegistrationState,
    eventNotificationItemConfigurations_join,
    eventNotificationItemConfigurations_messageDeliveryStatus,
    eventNotificationItemConfigurations_proximity,

    -- * FPorts
    FPorts (..),
    newFPorts,
    fPorts_applications,
    fPorts_clockSync,
    fPorts_fuota,
    fPorts_multicast,
    fPorts_positioning,

    -- * FuotaTask
    FuotaTask (..),
    newFuotaTask,
    fuotaTask_arn,
    fuotaTask_id,
    fuotaTask_name,

    -- * GatewayListItem
    GatewayListItem (..),
    newGatewayListItem,
    gatewayListItem_gatewayId,
    gatewayListItem_downlinkFrequency,

    -- * GlobalIdentity
    GlobalIdentity (..),
    newGlobalIdentity,
    globalIdentity_lac,
    globalIdentity_geranCid,

    -- * Gnss
    Gnss (..),
    newGnss,
    gnss_assistAltitude,
    gnss_assistPosition,
    gnss_captureTime,
    gnss_captureTimeAccuracy,
    gnss_use2DSolver,
    gnss_payload,

    -- * GsmLocalId
    GsmLocalId (..),
    newGsmLocalId,
    gsmLocalId_bsic,
    gsmLocalId_bcch,

    -- * GsmNmrObj
    GsmNmrObj (..),
    newGsmNmrObj,
    gsmNmrObj_globalIdentity,
    gsmNmrObj_rxLevel,
    gsmNmrObj_bsic,
    gsmNmrObj_bcch,

    -- * GsmObj
    GsmObj (..),
    newGsmObj,
    gsmObj_gsmLocalId,
    gsmObj_gsmNmr,
    gsmObj_gsmTimingAdvance,
    gsmObj_rxLevel,
    gsmObj_mcc,
    gsmObj_mnc,
    gsmObj_lac,
    gsmObj_geranCid,

    -- * Ip
    Ip (..),
    newIp,
    ip_ipAddress,

    -- * JoinEventConfiguration
    JoinEventConfiguration (..),
    newJoinEventConfiguration,
    joinEventConfiguration_loRaWAN,
    joinEventConfiguration_wirelessDeviceIdEventTopic,

    -- * JoinResourceTypeEventConfiguration
    JoinResourceTypeEventConfiguration (..),
    newJoinResourceTypeEventConfiguration,
    joinResourceTypeEventConfiguration_loRaWAN,

    -- * LoRaWANConnectionStatusEventNotificationConfigurations
    LoRaWANConnectionStatusEventNotificationConfigurations (..),
    newLoRaWANConnectionStatusEventNotificationConfigurations,
    loRaWANConnectionStatusEventNotificationConfigurations_gatewayEuiEventTopic,

    -- * LoRaWANConnectionStatusResourceTypeEventConfiguration
    LoRaWANConnectionStatusResourceTypeEventConfiguration (..),
    newLoRaWANConnectionStatusResourceTypeEventConfiguration,
    loRaWANConnectionStatusResourceTypeEventConfiguration_wirelessGatewayEventTopic,

    -- * LoRaWANDevice
    LoRaWANDevice (..),
    newLoRaWANDevice,
    loRaWANDevice_abpV1_0_x,
    loRaWANDevice_abpV1_1,
    loRaWANDevice_devEui,
    loRaWANDevice_deviceProfileId,
    loRaWANDevice_fPorts,
    loRaWANDevice_otaaV1_0_x,
    loRaWANDevice_otaaV1_1,
    loRaWANDevice_serviceProfileId,

    -- * LoRaWANDeviceMetadata
    LoRaWANDeviceMetadata (..),
    newLoRaWANDeviceMetadata,
    loRaWANDeviceMetadata_dataRate,
    loRaWANDeviceMetadata_devEui,
    loRaWANDeviceMetadata_fPort,
    loRaWANDeviceMetadata_frequency,
    loRaWANDeviceMetadata_gateways,
    loRaWANDeviceMetadata_timestamp,

    -- * LoRaWANDeviceProfile
    LoRaWANDeviceProfile (..),
    newLoRaWANDeviceProfile,
    loRaWANDeviceProfile_classBTimeout,
    loRaWANDeviceProfile_classCTimeout,
    loRaWANDeviceProfile_factoryPresetFreqsList,
    loRaWANDeviceProfile_macVersion,
    loRaWANDeviceProfile_maxDutyCycle,
    loRaWANDeviceProfile_maxEirp,
    loRaWANDeviceProfile_pingSlotDr,
    loRaWANDeviceProfile_pingSlotFreq,
    loRaWANDeviceProfile_pingSlotPeriod,
    loRaWANDeviceProfile_regParamsRevision,
    loRaWANDeviceProfile_rfRegion,
    loRaWANDeviceProfile_rxDataRate2,
    loRaWANDeviceProfile_rxDelay1,
    loRaWANDeviceProfile_rxDrOffset1,
    loRaWANDeviceProfile_rxFreq2,
    loRaWANDeviceProfile_supports32BitFCnt,
    loRaWANDeviceProfile_supportsClassB,
    loRaWANDeviceProfile_supportsClassC,
    loRaWANDeviceProfile_supportsJoin,

    -- * LoRaWANFuotaTask
    LoRaWANFuotaTask (..),
    newLoRaWANFuotaTask,
    loRaWANFuotaTask_rfRegion,

    -- * LoRaWANFuotaTaskGetInfo
    LoRaWANFuotaTaskGetInfo (..),
    newLoRaWANFuotaTaskGetInfo,
    loRaWANFuotaTaskGetInfo_rfRegion,
    loRaWANFuotaTaskGetInfo_startTime,

    -- * LoRaWANGateway
    LoRaWANGateway (..),
    newLoRaWANGateway,
    loRaWANGateway_beaconing,
    loRaWANGateway_gatewayEui,
    loRaWANGateway_joinEuiFilters,
    loRaWANGateway_netIdFilters,
    loRaWANGateway_rfRegion,
    loRaWANGateway_subBands,

    -- * LoRaWANGatewayCurrentVersion
    LoRaWANGatewayCurrentVersion (..),
    newLoRaWANGatewayCurrentVersion,
    loRaWANGatewayCurrentVersion_currentVersion,

    -- * LoRaWANGatewayMetadata
    LoRaWANGatewayMetadata (..),
    newLoRaWANGatewayMetadata,
    loRaWANGatewayMetadata_gatewayEui,
    loRaWANGatewayMetadata_rssi,
    loRaWANGatewayMetadata_snr,

    -- * LoRaWANGatewayVersion
    LoRaWANGatewayVersion (..),
    newLoRaWANGatewayVersion,
    loRaWANGatewayVersion_model,
    loRaWANGatewayVersion_packageVersion,
    loRaWANGatewayVersion_station,

    -- * LoRaWANGetServiceProfileInfo
    LoRaWANGetServiceProfileInfo (..),
    newLoRaWANGetServiceProfileInfo,
    loRaWANGetServiceProfileInfo_addGwMetadata,
    loRaWANGetServiceProfileInfo_channelMask,
    loRaWANGetServiceProfileInfo_devStatusReqFreq,
    loRaWANGetServiceProfileInfo_dlBucketSize,
    loRaWANGetServiceProfileInfo_dlRate,
    loRaWANGetServiceProfileInfo_dlRatePolicy,
    loRaWANGetServiceProfileInfo_drMax,
    loRaWANGetServiceProfileInfo_drMin,
    loRaWANGetServiceProfileInfo_hrAllowed,
    loRaWANGetServiceProfileInfo_minGwDiversity,
    loRaWANGetServiceProfileInfo_nwkGeoLoc,
    loRaWANGetServiceProfileInfo_prAllowed,
    loRaWANGetServiceProfileInfo_raAllowed,
    loRaWANGetServiceProfileInfo_reportDevStatusBattery,
    loRaWANGetServiceProfileInfo_reportDevStatusMargin,
    loRaWANGetServiceProfileInfo_targetPer,
    loRaWANGetServiceProfileInfo_ulBucketSize,
    loRaWANGetServiceProfileInfo_ulRate,
    loRaWANGetServiceProfileInfo_ulRatePolicy,

    -- * LoRaWANJoinEventNotificationConfigurations
    LoRaWANJoinEventNotificationConfigurations (..),
    newLoRaWANJoinEventNotificationConfigurations,
    loRaWANJoinEventNotificationConfigurations_devEuiEventTopic,

    -- * LoRaWANJoinResourceTypeEventConfiguration
    LoRaWANJoinResourceTypeEventConfiguration (..),
    newLoRaWANJoinResourceTypeEventConfiguration,
    loRaWANJoinResourceTypeEventConfiguration_wirelessDeviceEventTopic,

    -- * LoRaWANListDevice
    LoRaWANListDevice (..),
    newLoRaWANListDevice,
    loRaWANListDevice_devEui,

    -- * LoRaWANMulticast
    LoRaWANMulticast (..),
    newLoRaWANMulticast,
    loRaWANMulticast_dlClass,
    loRaWANMulticast_rfRegion,

    -- * LoRaWANMulticastGet
    LoRaWANMulticastGet (..),
    newLoRaWANMulticastGet,
    loRaWANMulticastGet_dlClass,
    loRaWANMulticastGet_numberOfDevicesInGroup,
    loRaWANMulticastGet_numberOfDevicesRequested,
    loRaWANMulticastGet_rfRegion,

    -- * LoRaWANMulticastMetadata
    LoRaWANMulticastMetadata (..),
    newLoRaWANMulticastMetadata,
    loRaWANMulticastMetadata_fPort,

    -- * LoRaWANMulticastSession
    LoRaWANMulticastSession (..),
    newLoRaWANMulticastSession,
    loRaWANMulticastSession_dlDr,
    loRaWANMulticastSession_dlFreq,
    loRaWANMulticastSession_sessionStartTime,
    loRaWANMulticastSession_sessionTimeout,

    -- * LoRaWANSendDataToDevice
    LoRaWANSendDataToDevice (..),
    newLoRaWANSendDataToDevice,
    loRaWANSendDataToDevice_fPort,
    loRaWANSendDataToDevice_participatingGateways,

    -- * LoRaWANServiceProfile
    LoRaWANServiceProfile (..),
    newLoRaWANServiceProfile,
    loRaWANServiceProfile_addGwMetadata,
    loRaWANServiceProfile_drMax,
    loRaWANServiceProfile_drMin,

    -- * LoRaWANStartFuotaTask
    LoRaWANStartFuotaTask (..),
    newLoRaWANStartFuotaTask,
    loRaWANStartFuotaTask_startTime,

    -- * LoRaWANUpdateDevice
    LoRaWANUpdateDevice (..),
    newLoRaWANUpdateDevice,
    loRaWANUpdateDevice_abpV1_0_x,
    loRaWANUpdateDevice_abpV1_1,
    loRaWANUpdateDevice_deviceProfileId,
    loRaWANUpdateDevice_fPorts,
    loRaWANUpdateDevice_serviceProfileId,

    -- * LoRaWANUpdateGatewayTaskCreate
    LoRaWANUpdateGatewayTaskCreate (..),
    newLoRaWANUpdateGatewayTaskCreate,
    loRaWANUpdateGatewayTaskCreate_currentVersion,
    loRaWANUpdateGatewayTaskCreate_sigKeyCrc,
    loRaWANUpdateGatewayTaskCreate_updateSignature,
    loRaWANUpdateGatewayTaskCreate_updateVersion,

    -- * LoRaWANUpdateGatewayTaskEntry
    LoRaWANUpdateGatewayTaskEntry (..),
    newLoRaWANUpdateGatewayTaskEntry,
    loRaWANUpdateGatewayTaskEntry_currentVersion,
    loRaWANUpdateGatewayTaskEntry_updateVersion,

    -- * LteLocalId
    LteLocalId (..),
    newLteLocalId,
    lteLocalId_pci,
    lteLocalId_earfcn,

    -- * LteNmrObj
    LteNmrObj (..),
    newLteNmrObj,
    lteNmrObj_rsrp,
    lteNmrObj_rsrq,
    lteNmrObj_pci,
    lteNmrObj_earfcn,
    lteNmrObj_eutranCid,

    -- * LteObj
    LteObj (..),
    newLteObj,
    lteObj_lteLocalId,
    lteObj_lteNmr,
    lteObj_lteTimingAdvance,
    lteObj_nrCapable,
    lteObj_rsrp,
    lteObj_rsrq,
    lteObj_tac,
    lteObj_mcc,
    lteObj_mnc,
    lteObj_eutranCid,

    -- * MessageDeliveryStatusEventConfiguration
    MessageDeliveryStatusEventConfiguration (..),
    newMessageDeliveryStatusEventConfiguration,
    messageDeliveryStatusEventConfiguration_sidewalk,
    messageDeliveryStatusEventConfiguration_wirelessDeviceIdEventTopic,

    -- * MessageDeliveryStatusResourceTypeEventConfiguration
    MessageDeliveryStatusResourceTypeEventConfiguration (..),
    newMessageDeliveryStatusResourceTypeEventConfiguration,
    messageDeliveryStatusResourceTypeEventConfiguration_sidewalk,

    -- * MulticastGroup
    MulticastGroup (..),
    newMulticastGroup,
    multicastGroup_arn,
    multicastGroup_id,
    multicastGroup_name,

    -- * MulticastGroupByFuotaTask
    MulticastGroupByFuotaTask (..),
    newMulticastGroupByFuotaTask,
    multicastGroupByFuotaTask_id,

    -- * MulticastWirelessMetadata
    MulticastWirelessMetadata (..),
    newMulticastWirelessMetadata,
    multicastWirelessMetadata_loRaWAN,

    -- * NetworkAnalyzerConfigurations
    NetworkAnalyzerConfigurations (..),
    newNetworkAnalyzerConfigurations,
    networkAnalyzerConfigurations_arn,
    networkAnalyzerConfigurations_name,

    -- * OtaaV1_0_x
    OtaaV1_0_x (..),
    newOtaaV1_0_x,
    otaaV1_0_x_appEui,
    otaaV1_0_x_appKey,
    otaaV1_0_x_genAppKey,

    -- * OtaaV1_1
    OtaaV1_1 (..),
    newOtaaV1_1,
    otaaV1_1_appKey,
    otaaV1_1_joinEui,
    otaaV1_1_nwkKey,

    -- * ParticipatingGateways
    ParticipatingGateways (..),
    newParticipatingGateways,
    participatingGateways_downlinkMode,
    participatingGateways_gatewayList,
    participatingGateways_transmissionInterval,

    -- * Positioning
    Positioning (..),
    newPositioning,
    positioning_clockSync,
    positioning_gnss,
    positioning_stream,

    -- * ProximityEventConfiguration
    ProximityEventConfiguration (..),
    newProximityEventConfiguration,
    proximityEventConfiguration_sidewalk,
    proximityEventConfiguration_wirelessDeviceIdEventTopic,

    -- * ProximityResourceTypeEventConfiguration
    ProximityResourceTypeEventConfiguration (..),
    newProximityResourceTypeEventConfiguration,
    proximityResourceTypeEventConfiguration_sidewalk,

    -- * ServiceProfile
    ServiceProfile (..),
    newServiceProfile,
    serviceProfile_arn,
    serviceProfile_id,
    serviceProfile_name,

    -- * SessionKeysAbpV1_0_x
    SessionKeysAbpV1_0_x (..),
    newSessionKeysAbpV1_0_x,
    sessionKeysAbpV1_0_x_appSKey,
    sessionKeysAbpV1_0_x_nwkSKey,

    -- * SessionKeysAbpV1_1
    SessionKeysAbpV1_1 (..),
    newSessionKeysAbpV1_1,
    sessionKeysAbpV1_1_appSKey,
    sessionKeysAbpV1_1_fNwkSIntKey,
    sessionKeysAbpV1_1_nwkSEncKey,
    sessionKeysAbpV1_1_sNwkSIntKey,

    -- * SidewalkAccountInfo
    SidewalkAccountInfo (..),
    newSidewalkAccountInfo,
    sidewalkAccountInfo_amazonId,
    sidewalkAccountInfo_appServerPrivateKey,

    -- * SidewalkAccountInfoWithFingerprint
    SidewalkAccountInfoWithFingerprint (..),
    newSidewalkAccountInfoWithFingerprint,
    sidewalkAccountInfoWithFingerprint_amazonId,
    sidewalkAccountInfoWithFingerprint_arn,
    sidewalkAccountInfoWithFingerprint_fingerprint,

    -- * SidewalkDevice
    SidewalkDevice (..),
    newSidewalkDevice,
    sidewalkDevice_amazonId,
    sidewalkDevice_deviceCertificates,
    sidewalkDevice_sidewalkId,
    sidewalkDevice_sidewalkManufacturingSn,

    -- * SidewalkDeviceMetadata
    SidewalkDeviceMetadata (..),
    newSidewalkDeviceMetadata,
    sidewalkDeviceMetadata_batteryLevel,
    sidewalkDeviceMetadata_deviceState,
    sidewalkDeviceMetadata_event,
    sidewalkDeviceMetadata_rssi,

    -- * SidewalkEventNotificationConfigurations
    SidewalkEventNotificationConfigurations (..),
    newSidewalkEventNotificationConfigurations,
    sidewalkEventNotificationConfigurations_amazonIdEventTopic,

    -- * SidewalkListDevice
    SidewalkListDevice (..),
    newSidewalkListDevice,
    sidewalkListDevice_amazonId,
    sidewalkListDevice_deviceCertificates,
    sidewalkListDevice_sidewalkId,
    sidewalkListDevice_sidewalkManufacturingSn,

    -- * SidewalkResourceTypeEventConfiguration
    SidewalkResourceTypeEventConfiguration (..),
    newSidewalkResourceTypeEventConfiguration,
    sidewalkResourceTypeEventConfiguration_wirelessDeviceEventTopic,

    -- * SidewalkSendDataToDevice
    SidewalkSendDataToDevice (..),
    newSidewalkSendDataToDevice,
    sidewalkSendDataToDevice_ackModeRetryDurationSecs,
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

    -- * TdscdmaLocalId
    TdscdmaLocalId (..),
    newTdscdmaLocalId,
    tdscdmaLocalId_uarfcn,
    tdscdmaLocalId_cellParams,

    -- * TdscdmaNmrObj
    TdscdmaNmrObj (..),
    newTdscdmaNmrObj,
    tdscdmaNmrObj_pathLoss,
    tdscdmaNmrObj_rscp,
    tdscdmaNmrObj_utranCid,
    tdscdmaNmrObj_uarfcn,
    tdscdmaNmrObj_cellParams,

    -- * TdscdmaObj
    TdscdmaObj (..),
    newTdscdmaObj,
    tdscdmaObj_lac,
    tdscdmaObj_pathLoss,
    tdscdmaObj_rscp,
    tdscdmaObj_tdscdmaLocalId,
    tdscdmaObj_tdscdmaNmr,
    tdscdmaObj_tdscdmaTimingAdvance,
    tdscdmaObj_mcc,
    tdscdmaObj_mnc,
    tdscdmaObj_utranCid,

    -- * TraceContent
    TraceContent (..),
    newTraceContent,
    traceContent_logLevel,
    traceContent_wirelessDeviceFrameInfo,

    -- * UpdateAbpV1_0_x
    UpdateAbpV1_0_x (..),
    newUpdateAbpV1_0_x,
    updateAbpV1_0_x_fCntStart,

    -- * UpdateAbpV1_1
    UpdateAbpV1_1 (..),
    newUpdateAbpV1_1,
    updateAbpV1_1_fCntStart,

    -- * UpdateFPorts
    UpdateFPorts (..),
    newUpdateFPorts,
    updateFPorts_applications,
    updateFPorts_positioning,

    -- * UpdateWirelessGatewayTaskCreate
    UpdateWirelessGatewayTaskCreate (..),
    newUpdateWirelessGatewayTaskCreate,
    updateWirelessGatewayTaskCreate_loRaWAN,
    updateWirelessGatewayTaskCreate_updateDataRole,
    updateWirelessGatewayTaskCreate_updateDataSource,

    -- * UpdateWirelessGatewayTaskEntry
    UpdateWirelessGatewayTaskEntry (..),
    newUpdateWirelessGatewayTaskEntry,
    updateWirelessGatewayTaskEntry_arn,
    updateWirelessGatewayTaskEntry_id,
    updateWirelessGatewayTaskEntry_loRaWAN,

    -- * WcdmaLocalId
    WcdmaLocalId (..),
    newWcdmaLocalId,
    wcdmaLocalId_uarfcndl,
    wcdmaLocalId_psc,

    -- * WcdmaNmrObj
    WcdmaNmrObj (..),
    newWcdmaNmrObj,
    wcdmaNmrObj_pathLoss,
    wcdmaNmrObj_rscp,
    wcdmaNmrObj_uarfcndl,
    wcdmaNmrObj_psc,
    wcdmaNmrObj_utranCid,

    -- * WcdmaObj
    WcdmaObj (..),
    newWcdmaObj,
    wcdmaObj_lac,
    wcdmaObj_pathLoss,
    wcdmaObj_rscp,
    wcdmaObj_wcdmaLocalId,
    wcdmaObj_wcdmaNmr,
    wcdmaObj_mcc,
    wcdmaObj_mnc,
    wcdmaObj_utranCid,

    -- * WiFiAccessPoint
    WiFiAccessPoint (..),
    newWiFiAccessPoint,
    wiFiAccessPoint_macAddress,
    wiFiAccessPoint_rss,

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
    wirelessDeviceStatistics_arn,
    wirelessDeviceStatistics_destinationName,
    wirelessDeviceStatistics_fuotaDeviceStatus,
    wirelessDeviceStatistics_id,
    wirelessDeviceStatistics_lastUplinkReceivedAt,
    wirelessDeviceStatistics_loRaWAN,
    wirelessDeviceStatistics_mcGroupId,
    wirelessDeviceStatistics_multicastDeviceStatus,
    wirelessDeviceStatistics_name,
    wirelessDeviceStatistics_sidewalk,
    wirelessDeviceStatistics_type,

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
    wirelessGatewayStatistics_description,
    wirelessGatewayStatistics_id,
    wirelessGatewayStatistics_lastUplinkReceivedAt,
    wirelessGatewayStatistics_loRaWAN,
    wirelessGatewayStatistics_name,

    -- * WirelessMetadata
    WirelessMetadata (..),
    newWirelessMetadata,
    wirelessMetadata_loRaWAN,
    wirelessMetadata_sidewalk,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types.AbpV1_0_x
import Amazonka.IoTWireless.Types.AbpV1_1
import Amazonka.IoTWireless.Types.ApplicationConfig
import Amazonka.IoTWireless.Types.ApplicationConfigType
import Amazonka.IoTWireless.Types.BatteryLevel
import Amazonka.IoTWireless.Types.Beaconing
import Amazonka.IoTWireless.Types.CdmaLocalId
import Amazonka.IoTWireless.Types.CdmaNmrObj
import Amazonka.IoTWireless.Types.CdmaObj
import Amazonka.IoTWireless.Types.CellTowers
import Amazonka.IoTWireless.Types.CertificateList
import Amazonka.IoTWireless.Types.ConnectionStatus
import Amazonka.IoTWireless.Types.ConnectionStatusEventConfiguration
import Amazonka.IoTWireless.Types.ConnectionStatusResourceTypeEventConfiguration
import Amazonka.IoTWireless.Types.Destinations
import Amazonka.IoTWireless.Types.DeviceProfile
import Amazonka.IoTWireless.Types.DeviceRegistrationStateEventConfiguration
import Amazonka.IoTWireless.Types.DeviceRegistrationStateResourceTypeEventConfiguration
import Amazonka.IoTWireless.Types.DeviceState
import Amazonka.IoTWireless.Types.DlClass
import Amazonka.IoTWireless.Types.DownlinkMode
import Amazonka.IoTWireless.Types.DownlinkQueueMessage
import Amazonka.IoTWireless.Types.Event
import Amazonka.IoTWireless.Types.EventConfigurationItem
import Amazonka.IoTWireless.Types.EventNotificationItemConfigurations
import Amazonka.IoTWireless.Types.EventNotificationPartnerType
import Amazonka.IoTWireless.Types.EventNotificationResourceType
import Amazonka.IoTWireless.Types.EventNotificationTopicStatus
import Amazonka.IoTWireless.Types.ExpressionType
import Amazonka.IoTWireless.Types.FPorts
import Amazonka.IoTWireless.Types.FuotaDeviceStatus
import Amazonka.IoTWireless.Types.FuotaTask
import Amazonka.IoTWireless.Types.FuotaTaskStatus
import Amazonka.IoTWireless.Types.GatewayListItem
import Amazonka.IoTWireless.Types.GlobalIdentity
import Amazonka.IoTWireless.Types.Gnss
import Amazonka.IoTWireless.Types.GsmLocalId
import Amazonka.IoTWireless.Types.GsmNmrObj
import Amazonka.IoTWireless.Types.GsmObj
import Amazonka.IoTWireless.Types.IdentifierType
import Amazonka.IoTWireless.Types.Ip
import Amazonka.IoTWireless.Types.JoinEventConfiguration
import Amazonka.IoTWireless.Types.JoinResourceTypeEventConfiguration
import Amazonka.IoTWireless.Types.LoRaWANConnectionStatusEventNotificationConfigurations
import Amazonka.IoTWireless.Types.LoRaWANConnectionStatusResourceTypeEventConfiguration
import Amazonka.IoTWireless.Types.LoRaWANDevice
import Amazonka.IoTWireless.Types.LoRaWANDeviceMetadata
import Amazonka.IoTWireless.Types.LoRaWANDeviceProfile
import Amazonka.IoTWireless.Types.LoRaWANFuotaTask
import Amazonka.IoTWireless.Types.LoRaWANFuotaTaskGetInfo
import Amazonka.IoTWireless.Types.LoRaWANGateway
import Amazonka.IoTWireless.Types.LoRaWANGatewayCurrentVersion
import Amazonka.IoTWireless.Types.LoRaWANGatewayMetadata
import Amazonka.IoTWireless.Types.LoRaWANGatewayVersion
import Amazonka.IoTWireless.Types.LoRaWANGetServiceProfileInfo
import Amazonka.IoTWireless.Types.LoRaWANJoinEventNotificationConfigurations
import Amazonka.IoTWireless.Types.LoRaWANJoinResourceTypeEventConfiguration
import Amazonka.IoTWireless.Types.LoRaWANListDevice
import Amazonka.IoTWireless.Types.LoRaWANMulticast
import Amazonka.IoTWireless.Types.LoRaWANMulticastGet
import Amazonka.IoTWireless.Types.LoRaWANMulticastMetadata
import Amazonka.IoTWireless.Types.LoRaWANMulticastSession
import Amazonka.IoTWireless.Types.LoRaWANSendDataToDevice
import Amazonka.IoTWireless.Types.LoRaWANServiceProfile
import Amazonka.IoTWireless.Types.LoRaWANStartFuotaTask
import Amazonka.IoTWireless.Types.LoRaWANUpdateDevice
import Amazonka.IoTWireless.Types.LoRaWANUpdateGatewayTaskCreate
import Amazonka.IoTWireless.Types.LoRaWANUpdateGatewayTaskEntry
import Amazonka.IoTWireless.Types.LogLevel
import Amazonka.IoTWireless.Types.LteLocalId
import Amazonka.IoTWireless.Types.LteNmrObj
import Amazonka.IoTWireless.Types.LteObj
import Amazonka.IoTWireless.Types.MessageDeliveryStatusEventConfiguration
import Amazonka.IoTWireless.Types.MessageDeliveryStatusResourceTypeEventConfiguration
import Amazonka.IoTWireless.Types.MessageType
import Amazonka.IoTWireless.Types.MulticastGroup
import Amazonka.IoTWireless.Types.MulticastGroupByFuotaTask
import Amazonka.IoTWireless.Types.MulticastWirelessMetadata
import Amazonka.IoTWireless.Types.NetworkAnalyzerConfigurations
import Amazonka.IoTWireless.Types.OtaaV1_0_x
import Amazonka.IoTWireless.Types.OtaaV1_1
import Amazonka.IoTWireless.Types.ParticipatingGateways
import Amazonka.IoTWireless.Types.PartnerType
import Amazonka.IoTWireless.Types.PositionResourceType
import Amazonka.IoTWireless.Types.Positioning
import Amazonka.IoTWireless.Types.PositioningConfigStatus
import Amazonka.IoTWireless.Types.ProximityEventConfiguration
import Amazonka.IoTWireless.Types.ProximityResourceTypeEventConfiguration
import Amazonka.IoTWireless.Types.ServiceProfile
import Amazonka.IoTWireless.Types.SessionKeysAbpV1_0_x
import Amazonka.IoTWireless.Types.SessionKeysAbpV1_1
import Amazonka.IoTWireless.Types.SidewalkAccountInfo
import Amazonka.IoTWireless.Types.SidewalkAccountInfoWithFingerprint
import Amazonka.IoTWireless.Types.SidewalkDevice
import Amazonka.IoTWireless.Types.SidewalkDeviceMetadata
import Amazonka.IoTWireless.Types.SidewalkEventNotificationConfigurations
import Amazonka.IoTWireless.Types.SidewalkListDevice
import Amazonka.IoTWireless.Types.SidewalkResourceTypeEventConfiguration
import Amazonka.IoTWireless.Types.SidewalkSendDataToDevice
import Amazonka.IoTWireless.Types.SidewalkUpdateAccount
import Amazonka.IoTWireless.Types.SigningAlg
import Amazonka.IoTWireless.Types.SupportedRfRegion
import Amazonka.IoTWireless.Types.Tag
import Amazonka.IoTWireless.Types.TdscdmaLocalId
import Amazonka.IoTWireless.Types.TdscdmaNmrObj
import Amazonka.IoTWireless.Types.TdscdmaObj
import Amazonka.IoTWireless.Types.TraceContent
import Amazonka.IoTWireless.Types.UpdateAbpV1_0_x
import Amazonka.IoTWireless.Types.UpdateAbpV1_1
import Amazonka.IoTWireless.Types.UpdateFPorts
import Amazonka.IoTWireless.Types.UpdateWirelessGatewayTaskCreate
import Amazonka.IoTWireless.Types.UpdateWirelessGatewayTaskEntry
import Amazonka.IoTWireless.Types.WcdmaLocalId
import Amazonka.IoTWireless.Types.WcdmaNmrObj
import Amazonka.IoTWireless.Types.WcdmaObj
import Amazonka.IoTWireless.Types.WiFiAccessPoint
import Amazonka.IoTWireless.Types.WirelessDeviceEvent
import Amazonka.IoTWireless.Types.WirelessDeviceEventLogOption
import Amazonka.IoTWireless.Types.WirelessDeviceFrameInfo
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
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-11-22@ of the Amazon IoT Wireless SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "IoTWireless",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "api.iotwireless",
      Core.signingName = "iotwireless",
      Core.version = "2020-11-22",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "IoTWireless",
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

-- | User does not have permission to perform this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | Adding, updating, or deleting the resource can cause an inconsistent
-- state.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | An unexpected error occurred while processing a request.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | Resource does not exist.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request was denied because it exceeded the allowed API request rate.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The request was denied because the resource can\'t have any more tags.
_TooManyTagsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"
    Prelude.. Core.hasStatus 400

-- | The input did not meet the specified constraints.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
